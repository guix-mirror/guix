;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guix status)
  #:use-module (guix records)
  #:use-module (guix i18n)
  #:use-module ((guix ui) #:select (colorize-string))
  #:use-module (guix progress)
  #:autoload   (guix build syscalls) (terminal-columns)
  #:use-module ((guix build download)
                #:select (nar-uri-abbreviation))
  #:use-module (guix store)
  #:use-module (guix derivations)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 binary-ports)
  #:autoload   (ice-9 rdelim) (read-string)
  #:use-module (rnrs bytevectors)
  #:use-module ((system foreign)
                #:select (bytevector->pointer pointer->bytevector))
  #:export (build-event-output-port
            compute-status

            build-status
            build-status?
            build-status-building
            build-status-downloading
            build-status-builds-completed
            build-status-downloads-completed

            download?
            download
            download-item
            download-uri
            download-size
            download-start
            download-end
            download-transferred

            build-status-updater
            print-build-event
            print-build-event/quiet
            print-build-status

            with-status-report))

;;; Commentary:
;;;
;;; This module provides facilities to track the status of ongoing builds and
;;; downloads in a given session, as well as tools to report about the current
;;; status to user interfaces.  It does so by analyzing the output of
;;; 'current-build-output-port'.  The build status is maintained in a
;;; <build-status> record.
;;;
;;; Code:


;;;
;;; Build status tracking.
;;;

;; Builds and substitutions performed by the daemon.
(define-record-type* <build-status> build-status make-build-status
  build-status?
  (building     build-status-building             ;list of drv
                (default '()))
  (downloading  build-status-downloading          ;list of <download>
                (default '()))
  (builds-completed build-status-builds-completed ;list of drv
                    (default '()))
  (downloads-completed build-status-downloads-completed ;list of store items
                       (default '())))

;; On-going or completed downloads.  Downloads can be stem from substitutes
;; and from "builtin:download" fixed-output derivations.
(define-record-type <download>
  (%download item uri size start end transferred)
  download?
  (item         download-item)            ;store item
  (uri          download-uri)             ;string | #f
  (size         download-size)            ;integer | #f
  (start        download-start)           ;<time>
  (end          download-end)             ;#f | <time>
  (transferred  download-transferred))    ;integer

(define* (download item uri
                   #:key size
                   (start (current-time time-monotonic)) end
                   (transferred 0))
  "Return a new download."
  (%download item uri size start end transferred))

(define (matching-download item)
  "Return a predicate that matches downloads of ITEM."
  (lambda (download)
    (string=? item (download-item download))))

(define* (compute-status event status
                         #:key
                         (current-time current-time)
                         (derivation-path->output-path
                          derivation-path->output-path))
  "Given EVENT, a tuple like (build-started \"/gnu/store/...-foo.drv\" ...),
compute a new status based on STATUS."
  (match event
    (('build-started drv _ ...)
     (build-status
      (inherit status)
      (building (cons drv (build-status-building status)))))
    (((or 'build-succeeded 'build-failed) drv _ ...)
     (build-status
      (inherit status)
      (building (delete drv (build-status-building status)))
      (builds-completed (cons drv (build-status-builds-completed status)))))

    ;; Note: Ignore 'substituter-started' and 'substituter-succeeded' because
    ;; they're not as informative as 'download-started' and
    ;; 'download-succeeded'.

    (('download-started item uri (= string->number size))
     ;; This is presumably a fixed-output derivation so move it from
     ;; 'building' to 'downloading'.  XXX: This doesn't work in 'check' mode
     ;; because ITEM is different from DRV's output.
     (build-status
      (inherit status)
      (building (remove (lambda (drv)
                          (equal? (false-if-exception
                                   (derivation-path->output-path drv))
                                  item))
                        (build-status-building status)))
      (downloading (cons (download item uri #:size size
                                   #:start (current-time time-monotonic))
                         (build-status-downloading status)))))
    (('download-succeeded item uri (= string->number size))
     (let ((current (find (matching-download item)
                          (build-status-downloading status))))
       (build-status
        (inherit status)
        (downloading (delq current (build-status-downloading status)))
        (downloads-completed
         (cons (download item uri
                         #:size size
                         #:start (download-start current)
                         #:transferred size
                         #:end (current-time time-monotonic))
               (build-status-downloads-completed status))))))
    (('substituter-succeeded item _ ...)
     (match (find (matching-download item)
                  (build-status-downloading status))
       (#f
        ;; Presumably we already got a 'download-succeeded' event for ITEM,
        ;; everything is fine.
        status)
       (current
        ;; Maybe the build process didn't emit a 'download-succeeded' event
        ;; for ITEM, so remove CURRENT from the queue now.
        (build-status
         (inherit status)
         (downloading (delq current (build-status-downloading status)))
         (downloads-completed
          (cons (download item (download-uri current)
                          #:size (download-size current)
                          #:start (download-start current)
                          #:transferred (download-size current)
                          #:end (current-time time-monotonic))
                (build-status-downloads-completed status)))))))
    (('download-progress item uri
                         (= string->number size)
                         (= string->number transferred))
     (let ((downloads (remove (matching-download item)
                              (build-status-downloading status)))
           (current   (find (matching-download item)
                            (build-status-downloading status))))
       (build-status
        (inherit status)
        (downloading (cons (download item uri
                                     #:size size
                                     #:start
                                     (or (and current
                                              (download-start current))
                                         (current-time time-monotonic))
                                     #:transferred transferred)
                           downloads)))))
    (_
     status)))

(define (simultaneous-jobs status)
  "Return the number of on-going builds and downloads for STATUS."
  (+ (length (build-status-building status))
     (length (build-status-downloading status))))


;;;
;;; Rendering.
;;;

(define (extended-build-trace-supported?)
  "Return true if the currently used store is known to support \"extended
build traces\" such as \"@ download-progress\" traces."
  ;; Support for extended build traces was added in protocol version #x162.
  (and (current-store-protocol-version)
       (>= (current-store-protocol-version) #x162)))

(define (multiplexed-output-supported?)
  "Return true if the daemon supports \"multiplexed output\"--i.e., \"@
build-log\" traces."
  (and (current-store-protocol-version)
       (>= (current-store-protocol-version) #x163)))

(define spin!
  (let ((steps (circular-list "\\" "|" "/" "-")))
    (lambda (port)
      "Display a spinner on PORT."
      (match steps
        ((first . rest)
         (set! steps rest)
         (display "\r\x1b[K" port)
         (display first port)
         (force-output port))))))

(define (color-output? port)
  "Return true if we should write colored output to PORT."
  (and (not (getenv "INSIDE_EMACS"))
       (not (getenv "NO_COLOR"))
       (isatty? port)))

(define-syntax color-rules
  (syntax-rules ()
    "Return a procedure that colorizes the string it is passed according to
the given rules.  Each rule has the form:

  (REGEXP COLOR1 COLOR2 ...)

where COLOR1 specifies how to colorize the first submatch of REGEXP, and so
on."
    ((_ (regexp colors ...) rest ...)
     (let ((next (color-rules rest ...))
           (rx   (make-regexp regexp)))
       (lambda (str)
         (if (string-index str #\nul)
             str
             (match (regexp-exec rx str)
               (#f (next str))
               (m  (let loop ((n 1)
                              (c '(colors ...))
                              (result '()))
                     (match c
                       (()
                        (string-concatenate-reverse result))
                       ((first . tail)
                        (loop (+ n 1) tail
                              (cons (colorize-string (match:substring m n)
                                                     first)
                                    result)))))))))))
    ((_)
     (lambda (str)
       str))))

(define colorize-log-line
  ;; Take a string and return a possibly colorized string according to the
  ;; rules below.
  (color-rules
   ("^(phase)(.*)(succeeded after)(.*)(seconds)(.*)"
    GREEN    BOLD GREEN          RESET  GREEN  BLUE)
   ("^(phase)(.*)(failed after)(.*)(seconds)(.*)"
    RED BLUE RED BLUE RED BLUE)
   ("^(.*)(error|fail|failed|\\<FAIL|FAILED)([[:blank:]]*)(:)(.*)"
    RESET  RED                           BOLD         BOLD BOLD)
   ("^(.*)(warning)([[:blank:]]*)(:)(.*)"
    RESET  MAGENTA   BOLD        BOLD BOLD)))

(define* (print-build-event event old-status status
                            #:optional (port (current-error-port))
                            #:key
                            (colorize? (color-output? port))
                            (print-log? #t))
  "Print information about EVENT and STATUS to PORT.  When COLORIZE? is true,
produce colorful output.  When PRINT-LOG? is true, display the build log in
addition to build events."
  (define info
    (if colorize?
        (cut colorize-string <> 'BOLD)
        identity))

  (define success
    (if colorize?
        (cut colorize-string <> 'GREEN 'BOLD)
        identity))

  (define failure
    (if colorize?
        (cut colorize-string <> 'RED 'BOLD)
        identity))

  (define print-log-line
    (if print-log?
        (if colorize?
            (lambda (line)
              (display (colorize-log-line line) port))
            (cut display <> port))
        (lambda (line)
          (spin! port))))

  (unless print-log?
    (display "\r" port))                          ;erase the spinner
  (match event
    (('build-started drv . _)
     (format port (info (G_ "building ~a...")) drv)
     (newline port))
    (('build-succeeded drv . _)
     (when (or print-log? (not (extended-build-trace-supported?)))
       (format port (success (G_ "successfully built ~a")) drv)
       (newline port))
     (match (build-status-building status)
       (() #t)
       (ongoing                                   ;when max-jobs > 1
        (format port
                (N_ "The following build is still in progress:~%~{  ~a~%~}~%"
                    "The following builds are still in progress:~%~{  ~a~%~}~%"
                    (length ongoing))
                ongoing))))
    (('build-failed drv . _)
     (format port (failure (G_ "build of ~a failed")) drv)
     (newline port)
     (match (derivation-log-file drv)
       (#f
        (format port (failure (G_ "Could not find build log for '~a'."))
                drv))
       (log
        (format port (info (G_ "View build log at '~a'.")) log)))
     (newline port))
    (('substituter-started item _ ...)
     (when (or print-log? (not (extended-build-trace-supported?)))
       (format port (info (G_ "substituting ~a...")) item)
       (newline port)))
    (('download-started item uri _ ...)
     (format port (info (G_ "downloading from ~a...")) uri)
     (newline port))
    (('download-progress item uri
                         (= string->number size)
                         (= string->number transferred))
     ;; Print a progress bar, but only if there's only one on-going
     ;; job--otherwise the output would be intermingled.
     (when (= 1 (simultaneous-jobs status))
       (match (find (matching-download item)
                    (build-status-downloading status))
         (#f #f)                                  ;shouldn't happen!
         (download
          ;; XXX: It would be nice to memoize the abbreviation.
          (let ((uri (if (string-contains uri "/nar/")
                         (nar-uri-abbreviation uri)
                         (basename uri))))
            (display-download-progress uri size
                                       #:start-time
                                       (download-start download)
                                       #:transferred transferred))))))
    (('substituter-succeeded item _ ...)
     ;; If there are no jobs running, we already reported download completion
     ;; so there's nothing left to do.
     (unless (and (zero? (simultaneous-jobs status))
                  (extended-build-trace-supported?))
       (format port (success (G_ "substitution of ~a complete")) item)
       (newline port)))
    (('substituter-failed item _ ...)
     (format port (failure (G_ "substitution of ~a failed")) item)
     (newline port))
    (('hash-mismatch item algo expected actual _ ...)
     ;; TRANSLATORS: The final string looks like "sha256 hash mismatch for
     ;; /gnu/store/…-sth:", where "sha256" is the hash algorithm.
     (format port (failure (G_ "~a hash mismatch for ~a:")) algo item)
     (newline port)
     (format port (info (G_ "\
  expected hash: ~a
  actual hash:   ~a~%"))
             expected actual))
    (('build-log pid line)
     (if (multiplexed-output-supported?)
         (if (not pid)
             (begin
               ;; LINE comes from the daemon, not from builders.  Let it
               ;; through.
               (display line port)
               (force-output port))
             (print-log-line line))
         (cond ((string-prefix? "substitute: " line)
                ;; The daemon prefixes early messages coming with 'guix
                ;; substitute' with "substitute:".  These are useful ("updating
                ;; substitutes from URL"), so let them through.
                (display line port)
                (force-output port))
               ((string-prefix? "waiting for locks" line)
                ;; This is when a derivation is already being built and we're just
                ;; waiting for the build to complete.
                (display (info (string-trim-right line)) port)
                (newline))
               (else
                (print-log-line line)))))
    (_
     event)))

(define* (print-build-event/quiet event old-status status
                                  #:optional
                                  (port (current-error-port))
                                  #:key
                                  (colorize? (color-output? port)))
  (print-build-event event old-status status port
                     #:colorize? colorize?
                     #:print-log? #f))

(define* (build-status-updater #:optional (on-change (const #t)))
  "Return a procedure that can be passed to 'build-event-output-port'.  That
procedure computes the new build status upon each event and calls ON-CHANGE:

  (ON-CHANGE event status new-status)

ON-CHANGE can display the build status, build events, etc."
  (lambda (event status)
    (let ((new (compute-status event status)))
      (on-change event status new)
      new)))


;;;
;;; Build port.
;;;

(define (maybe-utf8->string bv)
  "Attempt to decode BV as UTF-8 string and return it.  Gracefully handle the
case where BV does not contain only valid UTF-8."
  (catch 'decoding-error
    (lambda ()
      (utf8->string bv))
    (lambda _
      ;; This is the sledgehammer but it's the only safe way we have to
      ;; properly handle this.  It's expensive but it's rarely needed.
      (let ((port (open-bytevector-input-port bv)))
        (set-port-encoding! port "UTF-8")
        (set-port-conversion-strategy! port 'substitute)
        (let ((str (read-string port)))
          (close-port port)
          str)))))

(define (bytevector-index bv number offset count)
  "Search for NUMBER in BV starting from OFFSET and reading up to COUNT bytes;
return the offset where NUMBER first occurs or #f if it could not be found."
  (let loop ((offset offset)
             (count count))
    (cond ((zero? count) #f)
          ((= (bytevector-u8-ref bv offset) number) offset)
          (else (loop (+ 1 offset) (- count 1))))))

(define (split-lines str)
  "Split STR into lines in a way that preserves newline characters."
  (let loop ((str str)
             (result '()))
    (if (string-null? str)
        (reverse result)
        (match (string-index str #\newline)
          (#f
           (loop "" (cons str result)))
          (index
           (loop (string-drop str (+ index 1))
                 (cons (string-take str (+ index 1)) result)))))))

(define* (build-event-output-port proc #:optional (seed (build-status)))
  "Return an output port for use as 'current-build-output-port' that calls
PROC with its current state value, initialized with SEED, on every build
event.  Build events passed to PROC are tuples corresponding to the \"build
traces\" produced by the daemon:

  (build-started \"/gnu/store/...-foo.drv\" ...)
  (substituter-started \"/gnu/store/...-foo\" ...)

and so on.

The second return value is a thunk to retrieve the current state."
  (define %fragments
    ;; Line fragments received so far.
    '())

  (define %state
    ;; Current state for PROC.
    seed)

  ;; When true, this represents the current state while reading a
  ;; "@ build-log" trace: the current builder PID, the previously-read
  ;; bytevectors, and the number of bytes that remain to be read.
  (define %build-output-pid #f)
  (define %build-output '())
  (define %build-output-left #f)

  (define (process-line line)
    (cond ((string-prefix? "@ " line)
           (match (string-tokenize (string-drop line 2))
             (("build-log" (= string->number pid) (= string->number len))
              (set! %build-output-pid pid)
              (set! %build-output '())
              (set! %build-output-left len))
             (((= string->symbol event-name) args ...)
              (set! %state
                (proc (cons event-name args)
                      %state)))))
          (else
           (set! %state (proc (list 'build-log #f line)
                              %state)))))

  (define (process-build-output pid output)
    ;; Transform OUTPUT in 'build-log' events or download events as generated
    ;; by extended build traces.
    (define (line->event line)
      (match (and (string-prefix? "@ " line)
                  (string-tokenize (string-drop line 2)))
        ((type . args)
         (if (or (string-prefix? "download-" type)
                 (string=? "build-remote" type))
             (cons (string->symbol type) args)
             `(build-log ,pid ,line)))
        (_
         `(build-log ,pid ,line))))

    (let* ((lines  (split-lines output))
           (events (map line->event lines)))
      (set! %state (fold proc %state events))))

  (define (bytevector-range bv offset count)
    (let ((ptr (bytevector->pointer bv offset)))
      (pointer->bytevector ptr count)))

  (define (write! bv offset count)
    (if %build-output-pid
        (let ((keep (min count %build-output-left)))
          (set! %build-output
            (let ((bv* (make-bytevector keep)))
              (bytevector-copy! bv offset bv* 0 keep)
              (cons bv* %build-output)))
          (set! %build-output-left
            (- %build-output-left keep))

          (when (zero? %build-output-left)
            (process-build-output %build-output-pid
                                  (string-concatenate-reverse
                                   (map maybe-utf8->string %build-output))) ;XXX
            (set! %build-output '())
            (set! %build-output-pid #f))
          keep)
        (match (bytevector-index bv (char->integer #\newline)
                                 offset count)
          ((? integer? cr)
           (let* ((tail (maybe-utf8->string
                         (bytevector-range bv offset (- cr -1 offset))))
                  (line (string-concatenate-reverse
                         (cons tail %fragments))))
             (process-line line)
             (set! %fragments '())
             (- cr -1 offset)))
          (#f
           (unless (zero? count)
             (let ((str (maybe-utf8->string
                         (bytevector-range bv offset count))))
               (set! %fragments (cons str %fragments))))
           count))))

  (define port
    (make-custom-binary-output-port "filtering-input-port"
                                    write!
                                    #f #f
                                    #f))

  ;; The build port actually receives Unicode strings.
  (set-port-encoding! port "UTF-8")
  (cond-expand
    ((and guile-2 (not guile-2.2)) #t)
    (else (setvbuf port 'line)))
  (values port (lambda () %state)))

(define (call-with-status-report on-event thunk)
  (parameterize ((current-terminal-columns (terminal-columns))
                 (current-build-output-port
                  (build-event-output-port (build-status-updater on-event))))
    (thunk)))

(define-syntax-rule (with-status-report on-event exp ...)
  "Set up build status reporting to the user using the ON-EVENT procedure;
evaluate EXP... in that context."
  (call-with-status-report on-event (lambda () exp ...)))
