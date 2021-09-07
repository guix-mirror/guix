;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2021 Alice Brenon <alice.brenon@ens-lyon.fr>
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

(define-module (guix scripts import opam)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix scripts)
  #:use-module (guix import opam)
  #:use-module (guix scripts import)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:export (guix-import-opam))


;;;
;;; Command-line options.
;;;

(define %default-options
  '())

(define (show-help)
  (display (G_ "Usage: guix import opam PACKAGE-NAME
Import and convert the opam package for PACKAGE-NAME.\n"))
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -r, --recursive        import packages recursively"))
  (display (G_ "
      --repo             import packages from this opam repository (name, URL or local path)
                         can be used more than once"))
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
                   (show-version-and-exit "guix import opam")))
         (option '(#f "repo") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'repo arg result)))
         (option '(#\r "recursive") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'recursive #t result)))
         %standard-import-options))


;;;
;;; Entry point.
;;;

(define (guix-import-opam . args)
  (define (parse-options)
    ;; Return the alist of option values.
    (parse-command-line args %options (list %default-options)
                        #:build-options? #f))

  (let* ((opts (parse-options))
         (repo (filter-map (match-lambda
                             (('repo . name) name)
                             (_ #f)) opts))
         (args (filter-map (match-lambda
                            (('argument . value)
                             value)
                            (_ #f))
                           (reverse opts))))
    (match args
      ((package-name)
       (if (assoc-ref opts 'recursive)
           ;; Recursive import
           (map (match-lambda
                  ((and ('package ('name name) . rest) pkg)
                   `(define-public ,(string->symbol name)
                      ,pkg))
                  (_ #f))
                (opam-recursive-import package-name #:repo repo))
           ;; Single import
           (let ((sexp (opam->guix-package package-name #:repo repo)))
             (unless sexp
               (leave (G_ "failed to download meta-data for package '~a'~%")
                      package-name))
             sexp)))
      (()
       (leave (G_ "too few arguments~%")))
      ((many ...)
       (leave (G_ "too many arguments~%"))))))
