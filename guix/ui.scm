;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Mark H Weaver <mhw@netris.org>
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

(define-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix store)
  #:use-module (guix config)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module ((guix licenses) #:select (license? license-name))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:export (_
            N_
            leave
            show-version-and-exit
            show-bug-report-information
            show-what-to-build
            call-with-error-handling
            with-error-handling
            read/eval-package-expression
            location->string
            call-with-temporary-output-file
            switch-symlinks
            config-directory
            fill-paragraph
            string->recutils
            package->recutils
            run-guix-command
            guix-main))

;;; Commentary:
;;;
;;; User interface facilities for command-line tools.
;;;
;;; Code:

(define %gettext-domain
  "guix")

(define _ (cut gettext <> %gettext-domain))
(define N_ (cut ngettext <> <> <> %gettext-domain))

(define (install-locale)
  "Install the current locale settings."
  (catch 'system-error
    (lambda _
      (setlocale LC_ALL ""))
    (lambda args
      (format (current-error-port)
              (_ "warning: failed to install locale: ~a~%")
              (strerror (system-error-errno args))))))

(define (initialize-guix)
  "Perform the usual initialization for stand-alone Guix commands."
  (install-locale)
  (textdomain "guix")
  (setvbuf (current-output-port) _IOLBF)
  (setvbuf (current-error-port) _IOLBF))

(define-syntax-rule (leave fmt args ...)
  "Format FMT and ARGS to the error port and exit."
  (begin
    (format (current-error-port) fmt args ...)
    (exit 1)))

(define* (show-version-and-exit #:optional (command (car (command-line))))
  "Display version information for COMMAND and `(exit 0)'."
  (simple-format #t "~a (~a) ~a~%"
                 command %guix-package-name %guix-version)
  (exit 0))

(define (show-bug-report-information)
  (format #t (_ "
Report bugs to: ~a.") %guix-bug-report-address)
  (format #t (_ "
~a home page: <~a>") %guix-package-name %guix-home-page-url)
  (display (_ "
General help using GNU software: <http://www.gnu.org/gethelp/>"))
  (newline))

(define (call-with-error-handling thunk)
  "Call THUNK within a user-friendly error handler."
  (guard (c ((package-input-error? c)
             (let* ((package  (package-error-package c))
                    (input    (package-error-invalid-input c))
                    (location (package-location package))
                    (file     (location-file location))
                    (line     (location-line location))
                    (column   (location-column location)))
               (leave (_ "~a:~a:~a: error: package `~a' has an invalid input: ~s~%")
                      file line column
                      (package-full-name package) input)))
            ((nix-protocol-error? c)
             ;; FIXME: Server-provided error messages aren't i18n'd.
             (leave (_ "error: build failed: ~a~%")
                    (nix-protocol-error-message c))))
    (thunk)))

(define (read/eval-package-expression str)
  "Read and evaluate STR and return the package it refers to, or exit an
error."
  (let ((exp (catch #t
               (lambda ()
                 (call-with-input-string str read))
               (lambda args
                 (leave (_ "failed to read expression ~s: ~s~%")
                        str args)))))
    (let ((p (catch #t
               (lambda ()
                 (eval exp the-scm-module))
               (lambda args
                 (leave (_ "failed to evaluate expression `~a': ~s~%")
                        exp args)))))
      (if (package? p)
          p
          (leave (_ "expression `~s' does not evaluate to a package~%")
                 exp)))))

(define* (show-what-to-build store drv #:optional dry-run?)
  "Show what will or would (depending on DRY-RUN?) be built in realizing the
derivations listed in DRV.  Return #t if there's something to build, #f
otherwise."
  (let* ((req  (append-map (lambda (drv-path)
                             (let ((d (call-with-input-file drv-path
                                        read-derivation)))
                               (derivation-prerequisites-to-build
                                store d)))
                           drv))
         (req* (delete-duplicates
                (append (remove (compose (cute valid-path? store <>)
                                         derivation-path->output-path)
                                drv)
                        (map derivation-input-path req)))))
    (if dry-run?
        (format (current-error-port)
                (N_ "~:[the following derivation would be built:~%~{   ~a~%~}~;~]"
                    "~:[the following derivations would be built:~%~{    ~a~%~}~;~]"
                    (length req*))
                (null? req*) req*)
        (format (current-error-port)
                (N_ "~:[the following derivation will be built:~%~{   ~a~%~}~;~]"
                    "~:[the following derivations will be built:~%~{    ~a~%~}~;~]"
                    (length req*))
                (null? req*) req*))
    (pair? req*)))

(define-syntax with-error-handling
  (syntax-rules ()
    "Run BODY within a user-friendly error condition handler."
    ((_ body ...)
     (call-with-error-handling
      (lambda ()
        body ...)))))

(define (location->string loc)
  "Return a human-friendly, GNU-standard representation of LOC."
  (match loc
    (#f (_ "<unknown location>"))
    (($ <location> file line column)
     (format #f "~a:~a:~a" file line column))))

(define (call-with-temporary-output-file proc)
  "Call PROC with a name of a temporary file and open output port to that
file; close the file and delete it when leaving the dynamic extent of this
call."
  (let* ((template (string-copy "guix-file.XXXXXX"))
         (out      (mkstemp! template)))
    (dynamic-wind
      (lambda ()
        #t)
      (lambda ()
        (proc template out))
      (lambda ()
        (false-if-exception (close out))
        (false-if-exception (delete-file template))))))

(define (switch-symlinks link target)
  "Atomically switch LINK, a symbolic link, to point to TARGET.  Works
both when LINK already exists and when it does not."
  (let ((pivot (string-append link ".new")))
    (symlink target pivot)
    (rename-file pivot link)))

(define (config-directory)
  "Return the name of the configuration directory, after making sure that it
exists.  Honor the XDG specs,
<http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html>."
  (let ((dir (and=> (or (getenv "XDG_CONFIG_HOME")
                        (and=> (getenv "HOME")
                               (cut string-append <> "/.config")))
                    (cut string-append <> "/guix"))))
    (catch 'system-error
      (lambda ()
        (mkdir dir)
        dir)
      (lambda args
        (match (system-error-errno args)
          ((or EEXIST 0)
           dir)
          (err
           (leave (_ "failed to create configuration directory `~a': ~a~%")
                  dir (strerror err))))))))

(define* (fill-paragraph str width #:optional (column 0))
  "Fill STR such that each line contains at most WIDTH characters, assuming
that the first character is at COLUMN.

When STR contains a single line break surrounded by other characters, it is
converted to a space; sequences of more than one line break are preserved."
  (define (maybe-break chr result)
    (match result
      ((column newlines chars)
       (case chr
         ((#\newline)
          `(,column ,(+ 1 newlines) ,chars))
         (else
          (let ((chars  (case newlines
                          ((0) chars)
                          ((1) (cons #\space chars))
                          (else
                           (append (make-list newlines #\newline) chars))))
                (column (case newlines
                          ((0) column)
                          ((1) (+ 1 column))
                          (else 0))))
            (let ((chars  (cons chr chars))
                  (column (+ 1 column)))
              (if (> column width)
                  (let*-values (((before after)
                                 (break (cut eqv? #\space <>) chars))
                                ((len)
                                 (length before)))
                    (if (<= len width)
                        `(,len
                          0
                          ,(if (null? after)
                               before
                               (append before (cons #\newline (cdr after)))))
                        `(,column 0 ,chars)))     ; unbreakable
                  `(,column 0 ,chars)))))))))

  (match (string-fold maybe-break
                      `(,column 0 ())
                      str)
    ((_ _ chars)
     (list->string (reverse chars)))))

(define (string->recutils str)
  "Return a version of STR where newlines have been replaced by newlines
followed by \"+ \", which makes for a valid multi-line field value in the
`recutils' syntax."
  (list->string
   (string-fold-right (lambda (chr result)
                        (if (eqv? chr #\newline)
                            (cons* chr #\+ #\space result)
                            (cons chr result)))
                      '()
                      str)))

(define* (package->recutils p port
                            #:optional (width (or (and=> (getenv "WIDTH")
                                                         string->number)
                                                  80)))
  "Write to PORT a `recutils' record of package P, arranging to fit within
WIDTH columns."
  (define (description->recutils str)
    (let ((str (_ str)))
      (string->recutils
       (fill-paragraph str width
                       (string-length "description: ")))))

  ;; Note: Don't i18n field names so that people can post-process it.
  (format port "name: ~a~%" (package-name p))
  (format port "version: ~a~%" (package-version p))
  (format port "location: ~a~%"
          (or (and=> (package-location p) location->string)
              (_ "unknown")))
  (format port "home-page: ~a~%" (package-home-page p))
  (format port "license: ~a~%"
          (match (package-license p)
            (((? license? licenses) ...)
             (string-join (map license-name licenses)
                          ", "))
            ((? license? license)
             (license-name license))
            (x
             (_ "unknown"))))
  (format port "synopsis: ~a~%"
          (string-map (match-lambda
                       (#\newline #\space)
                       (chr       chr))
                      (or (and=> (package-synopsis p) _)
                          "")))
  (format port "description: ~a~%"
          (and=> (package-description p) description->recutils))
  (newline port))

(define (show-guix-usage)
  ;; TODO: Dynamically generate a summary of available commands.
  (format (current-error-port)
          (_ "Usage: guix COMMAND ARGS...~%")))

(define (run-guix-command command . args)
  ;; TODO: Gracefully report errors
  (let* ((module (resolve-interface `(guix scripts ,command)))
         (command-main (module-ref module
                                   (symbol-append 'guix- command))))
    (apply command-main args)))

(define (guix-main arg0 . args)
  (initialize-guix)
  (let ()
    (define (option? str) (string-prefix? "-" str))
    (match args
      (() (show-guix-usage) (exit 1))
      (("--help") (show-guix-usage))
      (("--version") (show-version-and-exit "guix"))
      (((? option? arg1) args ...) (show-guix-usage) (exit 1))
      ((command args ...)
       (apply run-guix-command
              (string->symbol command)
              args)))))

;;; ui.scm ends here
