;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
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

(define-module (guix scripts import gnu)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix scripts)
  #:use-module (guix import gnu)
  #:use-module (guix scripts import)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:export (guix-import-gnu))


;;;
;;; Command-line options.
;;;

(define %default-options
  '((key-download . interactive)))

(define (show-help)
  (display (G_ "Usage: guix import gnu [OPTION...] PACKAGE
Return a package declaration template for PACKAGE, a GNU package.\n"))
  ;; '--key-download' taken from (guix scripts refresh).
  (display (G_ "
      --key-download=POLICY
                         handle missing OpenPGP keys according to POLICY:
                         'always', 'never', and 'interactive', which is also
                         used when 'key-download' is not specified"))
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define %options
  ;; Specification of the command-line options.
  (cons* (option '(#\h "help") #f #f
                 (lambda args
                   (show-help)
                   (exit 0)))
         (option '(#\V "version") #f #f
                 (lambda args
                   (show-version-and-exit "guix import gnu")))
         (option '("key-download") #t #f          ;from (guix scripts refresh)
                 (lambda (opt name arg result)
                   (match arg
                     ((or "interactive" "always" "never")
                      (alist-cons 'key-download (string->symbol arg)
                                  result))
                     (x
                      (leave (G_ "unsupported policy: ~a~%")
                             arg)))))
         %standard-import-options))


;;;
;;; Entry point.
;;;

(define (guix-import-gnu . args)
  (define (parse-options)
    ;; Return the alist of option values.
    (parse-command-line args %options (list %default-options)
                        #:build-options? #f))

  (let* ((opts (parse-options))
         (args (filter-map (match-lambda
                            (('argument . value)
                             value)
                            (_ #f))
                           (reverse opts))))
    (match args
      ((name)
       (with-error-handling
         (gnu->guix-package name
                            #:key-download (assoc-ref opts 'key-download))))
      (x
       (leave (G_ "wrong number of arguments~%"))))))

;;; gnu.scm ends here
