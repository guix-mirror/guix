;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (ice-9 match)
  #:export (_
            N_
            install-locale
            leave
            show-version-and-exit
            show-bug-report-information
            call-with-error-handling
            with-error-handling
            location->string))

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

;;; ui.scm ends here
