;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
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

(define-module (guix scripts import egg)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix scripts)
  #:use-module (guix import egg)
  #:use-module (guix scripts import)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-37)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:export (guix-import-egg))


;;;
;;; Command-line options.
;;;

(define %default-options
  '())

(define (show-help)
  (display (G_ "Usage: guix import egg PACKAGE-NAME
Import and convert the egg package for PACKAGE-NAME.\n"))
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -r, --recursive        import packages recursively"))
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
                   (show-version-and-exit "guix import egg")))
         (option '(#\r "recursive") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'recursive #t result)))
         %standard-import-options))


;;;
;;; Entry point.
;;;

(define (guix-import-egg . args)
  (define (parse-options)
    ;; Return the alist of option values.
    (parse-command-line args %options (list %default-options)
                        #:build-options? #f))

  (let* ((opts (parse-options))
         (repo (and=> (assoc-ref opts 'repo) string->symbol))
         (args (filter-map (match-lambda
                            (('argument . value)
                             value)
                            (_ #f))
                           (reverse opts))))
    (match args
      ((spec)
       (let ((name version (package-name->name+version spec)))
         (if (assoc-ref opts 'recursive)
             ;; Recursive import
             (map (match-lambda
                    ((and ('package ('name name) . rest) pkg)
                     `(define-public ,(string->symbol name)
                        ,pkg))
                    (_ #f))
                  (egg-recursive-import name version))
             ;; Single import
             (let ((sexp (egg->guix-package name version)))
               (unless sexp
                 (leave (G_ "failed to download meta-data for package '~a'~%")
                        (if version
                            (string-append name "@" version)
                            name)))
               sexp))))
      (()
       (leave (G_ "too few arguments~%")))
      ((many ...)
       (leave (G_ "too many arguments~%"))))))
