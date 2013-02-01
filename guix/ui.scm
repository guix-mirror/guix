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
  #:use-module ((guix licenses) #:select (license? license-name))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
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
            location->string
            fill-paragraph
            string->recutils
            package->recutils))

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

;;; ui.scm ends here
