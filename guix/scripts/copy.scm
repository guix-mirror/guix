;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2019 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix scripts copy)
  #:use-module (guix ui)
  #:use-module (guix scripts)
  #:use-module (guix ssh)
  #:use-module (guix store)
  #:use-module ((guix status) #:select (with-status-verbosity))
  #:use-module (guix utils)
  #:use-module (guix derivations)
  #:use-module (guix scripts build)
  #:use-module ((guix scripts archive) #:select (options->derivations+files))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:export (guix-copy))


;;;
;;; Exchanging store items over SSH.
;;;

(define (ssh-spec->user+host+port spec)
  "Parse SPEC, a string like \"user@host:port\" or just \"host\", and return
three values: the user name (or #f), the host name, and the TCP port
number (or #f) corresponding to SPEC."
  (define tokens
    (char-set #\@ #\:))

  (match (string-tokenize spec (char-set-complement tokens))
    ((host)
     (values #f host #f))
    ((left right)
     (if (string-index spec #\@)
         (values left right #f)
         (values #f left (string->number right))))
    ((user host port)
     (match (string->number port)
       ((? integer? port)
        (values user host port))
       (x
        (leave (G_ "~a: invalid TCP port number~%") port))))
    (x
     (leave (G_ "~a: invalid SSH specification~%") spec))))

(define (send-to-remote-host target opts)
  "Send ITEMS to TARGET.  ITEMS is a list of store items or package names; for ;
package names, build the underlying packages before sending them."
  (with-store local
    (set-build-options-from-command-line local opts)
    (let-values (((user host port)
                  (ssh-spec->user+host+port target))
                 ((drv items)
                  (options->derivations+files local opts)))
      (show-what-to-build local drv
                          #:use-substitutes? (assoc-ref opts 'substitutes?)
                          #:dry-run? (assoc-ref opts 'dry-run?))

      (and (or (assoc-ref opts 'dry-run?)
               (build-derivations local drv))
           (let* ((session (open-ssh-session host #:user user
                                             #:port (or port 22)))
                  (sent    (send-files local items
                                       (connect-to-remote-daemon session)
                                       #:recursive? #t)))
             (format #t "~{~a~%~}" sent)
             sent)))))

(define (retrieve-from-remote-host source opts)
  "Retrieve ITEMS from SOURCE."
  (with-store local
    (let*-values (((user host port)
                   (ssh-spec->user+host+port source))
                  ((session)
                   (open-ssh-session host #:user user #:port (or port 22)))
                  ((remote)
                   (connect-to-remote-daemon session)))
      (set-build-options-from-command-line local opts)
      ;; TODO: Here we could to compute and build the derivations on REMOTE
      ;; rather than on LOCAL (one-off offloading) but that is currently too
      ;; slow due to the many RPC round trips.  So we just assume that REMOTE
      ;; contains ITEMS.
      (let*-values (((drv items)
                     (options->derivations+files local opts))
                    ((retrieved)
                     (retrieve-files local items remote #:recursive? #t)))
        (format #t "~{~a~%~}" retrieved)
        retrieved))))


;;;
;;; Options.
;;;

(define (show-help)
  (display (G_ "Usage: guix copy [OPTION]... ITEMS...
Copy ITEMS to or from the specified host over SSH.\n"))
  (display (G_ "
      --to=HOST          send ITEMS to HOST"))
  (display (G_ "
      --from=HOST        receive ITEMS from HOST"))
  (display (G_ "
  -v, --verbosity=LEVEL  use the given verbosity LEVEL"))
  (newline)
  (show-build-options-help)
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define %options
  ;; Specifications of the command-line options.
  (cons* (option '("to") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'destination arg result)))
         (option '("from") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'source arg result)))
         (option '(#\v "verbosity") #t #f
                 (lambda (opt name arg result)
                   (let ((level (string->number* arg)))
                     (alist-cons 'verbosity level
                                 (alist-delete 'verbosity result)))))
         (option '(#\h "help") #f #f
                 (lambda args
                   (show-help)
                   (exit 0)))
         (option '(#\V "version") #f #f
                 (lambda args
                   (show-version-and-exit "guix copy")))
         (option '(#\s "system") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'system arg
                               (alist-delete 'system result eq?))))
         %standard-build-options))

(define %default-options
  `((system . ,(%current-system))
    (substitutes? . #t)
    (build-hook? . #t)
    (graft? . #t)
    (print-build-trace? . #t)
    (print-extended-build-trace? . #t)
    (multiplexed-build-output? . #t)
    (debug . 0)
    (verbosity . 2)))


;;;
;;; Entry point.
;;;

(define (guix-copy . args)
  (with-error-handling
    (let* ((opts     (parse-command-line args %options (list %default-options)))
           (source   (assoc-ref opts 'source))
           (target   (assoc-ref opts 'destination)))
      (with-status-verbosity (assoc-ref opts 'verbosity)
        (cond (target (send-to-remote-host target opts))
              (source (retrieve-from-remote-host source opts))
              (else   (leave (G_ "use '--to' or '--from'~%"))))))))
