;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (guix scripts import stackage)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix scripts)
  #:use-module (guix import stackage)
  #:use-module (guix scripts import)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:export (guix-import-stackage))


;;;
;;; Command-line options.
;;;

(define %default-options
  `((lts-version . "")
    (include-test-dependencies? . #t)))

(define (show-help)
  (display (G_ "Usage: guix import stackage PACKAGE-NAME
Import and convert the LTS Stackage package for PACKAGE-NAME.\n"))
  (display (G_ "
  -l VERSION, --lts-version=VERSION
                               specify the LTS version to use"))
  (display (G_ "
  -h, --help                   display this help and exit"))
  (display (G_ "
  -r, --recursive              import packages recursively"))
  (display (G_ "
  -t, --no-test-dependencies   don't include test-only dependencies"))
  (display (G_ "
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
                   (show-version-and-exit "guix import stackage")))
         (option '(#\t "no-test-dependencies") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'include-test-dependencies? #f
                               (alist-delete 'include-test-dependencies?
                                             result))))
         (option '(#\l "lts-version") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'lts-version arg
                               (alist-delete 'lts-version
                                             result))))
         (option '(#\r "recursive") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'recursive #t result)))
         %standard-import-options))


;;;
;;; Entry point.
;;;

(define (guix-import-stackage . args)
  (define (parse-options)
    ;; Return the alist of option values.
    (parse-command-line args %options (list %default-options)
                        #:build-options? #f))

  (define (run-importer package-name opts error-fn)
    (let* ((arguments (list
                       package-name
                       #:include-test-dependencies?
                       (assoc-ref opts 'include-test-dependencies?)
                       #:lts-version (assoc-ref opts 'lts-version)))
           (sexp (if (assoc-ref opts 'recursive)
                     ;; Recursive import
                     (map (match-lambda
                            ((and ('package ('name name) . rest) pkg)
                             `(define-public ,(string->symbol name)
                                ,pkg))
                            (_ #f))
                          (apply stackage-recursive-import arguments))
                     ;; Single import
                     (apply stackage->guix-package arguments))))
      (unless sexp (error-fn))
      sexp))

  (let* ((opts (parse-options))
         (args (filter-map (match-lambda
                            (('argument . value)
                             value)
                            (_ #f))
                           (reverse opts))))
    (match args
      ((package-name)
       (with-error-handling
         (run-importer package-name opts
                       (lambda ()
                         (leave (G_ "failed to download cabal file \
for package '~a'~%")
                                package-name)))))
      (()
       (leave (G_ "too few arguments~%")))
      ((many ...)
       (leave (G_ "too many arguments~%"))))))

;;; stackage.scm ends here
