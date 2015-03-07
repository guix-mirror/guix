;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
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

(define-module (guix scripts import hackage)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix import hackage)
  #:use-module (guix scripts import)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:export (guix-import-hackage))


;;;
;;; Command-line options.
;;;

(define %default-options
  '((include-test-dependencies? . #t)))

(define (show-help)
  (display (_ "Usage: guix import hackage PACKAGE-NAME
Import and convert the Hackage package for PACKAGE-NAME.  If PACKAGE-NAME
includes a suffix constituted by a dash followed by a numerical version (as
used with Guix packages), then a definition for the specified version of the
package will be generated.  If no version suffix is pecified, then the
generated package definition will correspond to the latest available
version.\n"))
  (display (_ "
  -h, --help                   display this help and exit"))
  (display (_ "
  -t, --no-test-dependencies   don't include test only dependencies"))
  (display (_ "
  -V, --version                display version information and exit"))
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
                   (show-version-and-exit "guix import hackage")))
         (option '(#\t "no-test-dependencies") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'include-test-dependencies? #f
                               (alist-delete 'include-test-dependencies?
                                             result))))
         %standard-import-options))


;;;
;;; Entry point.
;;;

(define (guix-import-hackage . args)
  (define (parse-options)
    ;; Return the alist of option values.
    (args-fold* args %options
                (lambda (opt name arg result)
                  (leave (_ "~A: unrecognized option~%") name))
                (lambda (arg result)
                  (alist-cons 'argument arg result))
                %default-options))

  (let* ((opts (parse-options))
         (args (filter-map (match-lambda
                            (('argument . value)
                             value)
                            (_ #f))
                           (reverse opts))))
    (match args
      ((package-name)
       (let ((sexp (hackage->guix-package
                    package-name
                    #:include-test-dependencies?
                    (assoc-ref opts 'include-test-dependencies?))))
         (unless sexp
           (leave (_ "failed to download cabal file for package '~a'~%")
                  package-name))
         sexp))
      (()
       (leave (_ "too few arguments~%")))
      ((many ...)
       (leave (_ "too many arguments~%"))))))
