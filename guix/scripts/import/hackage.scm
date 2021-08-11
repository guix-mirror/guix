;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
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

(define-module (guix scripts import hackage)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix scripts)
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

(define ghc-default-version
  (string-append "ghc-" (package-version (@ (gnu packages haskell) ghc))))

(define %default-options
  `((include-test-dependencies? . #t)
    (read-from-stdin? . #f)
    (cabal-environment . ,`(("impl" . ,ghc-default-version)))))

(define (show-help)
  (display (G_ "Usage: guix import hackage PACKAGE-NAME
Import and convert the Hackage package for PACKAGE-NAME.  If PACKAGE-NAME
includes a suffix constituted by a at-sign followed by a numerical version (as
used with Guix packages), then a definition for the specified version of the
package will be generated.  If no version suffix is specified, then the
generated package definition will correspond to the latest available
version.\n"))
  (display (G_ "
  -e ALIST, --cabal-environment=ALIST
                               specify environment for Cabal evaluation"))
  (display (G_ "
  -h, --help                   display this help and exit"))
  (display (G_ "
  -r, --recursive              import packages recursively"))
  (display (G_ "
  -s, --stdin                  read from standard input"))
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
                   (show-version-and-exit "guix import hackage")))
         (option '(#\t "no-test-dependencies") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'include-test-dependencies? #f
                               (alist-delete 'include-test-dependencies?
                                             result))))
         (option '(#\s "stdin") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'read-from-stdin? #t
                               (alist-delete 'read-from-stdin?
                                             result))))
         (option '(#\e "cabal-environment") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'cabal-environment (read/eval arg)
                               (alist-delete 'cabal-environment
                                             result))))
         (option '(#\r "recursive") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'recursive #t result)))
         %standard-import-options))


;;;
;;; Entry point.
;;;

(define (guix-import-hackage . args)
  (define (parse-options)
    ;; Return the alist of option values.
    (parse-command-line args %options (list %default-options)
                        #:build-options? #f))

  (define (run-importer package-name opts error-fn)
    (let* ((arguments (list
                       package-name
                       #:include-test-dependencies?
                       (assoc-ref opts 'include-test-dependencies?)
                       #:port (if (assoc-ref opts 'read-from-stdin?)
                                  (current-input-port)
                                  #f)
                       #:cabal-environment
                       (assoc-ref opts 'cabal-environment)))
           (sexp (if (assoc-ref opts 'recursive)
                     ;; Recursive import
                     (map (match-lambda
                            ((and ('package ('name name) . rest) pkg)
                             `(define-public ,(string->symbol name)
                                ,pkg))
                            (_ #f))
                          (apply hackage-recursive-import arguments))
                     ;; Single import
                     (apply hackage->guix-package arguments))))
      (unless sexp (error-fn))
      sexp))

  (let* ((opts (parse-options))
         (args (filter-map (match-lambda
                            (('argument . value)
                             value)
                            (_ #f))
                           (reverse opts))))
    (if (assoc-ref opts 'read-from-stdin?)
        (match args
          (()
           (run-importer "stdin" opts
                         (lambda ()
                           (leave (G_ "failed to import cabal file \
from standard input~%")))))
          ((many ...)
           (leave (G_ "too many arguments~%"))))
        (match args
          ((package-name)
           (run-importer package-name opts
                         (lambda ()
                           (leave (G_ "failed to download cabal file \
for package '~a'~%")
                                  package-name))))
          (()
           (leave (G_ "too few arguments~%")))
          ((many ...)
           (leave (G_ "too many arguments~%")))))))
