;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015, 2017, 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 Martin Becze <mjbecze@riseup.net>
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

(define-module (guix scripts import cran)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix scripts)
  #:use-module (guix import cran)
  #:use-module (guix import utils)
  #:use-module (guix scripts import)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-37)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:export (guix-import-cran))


;;;
;;; Command-line options.
;;;

(define %default-options
  '())

(define (show-help)
  (display (G_ "Usage: guix import cran PACKAGE-NAME
Import and convert the CRAN package for PACKAGE-NAME.\n"))
  (display (G_ "
  -a, --archive=ARCHIVE  specify the archive repository"))
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -r, --recursive        import packages recursively"))
  (display (G_ "
  -s, --style=STYLE      choose output style, either specification or variable"))
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
                   (show-version-and-exit "guix import cran")))
         (option '(#\a "archive") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'repo (string->symbol arg)
                               (alist-delete 'repo result))))
         (option '(#\s "style") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'style (string->symbol arg)
                               (alist-delete 'style result))))
         (option '(#\r "recursive") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'recursive #t result)))
         %standard-import-options))


;;;
;;; Entry point.
;;;

(define (guix-import-cran . args)
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
    (parameterize ((%input-style (assoc-ref opts 'style)))
      (match args
        ((spec)
         (let ((name version (package-name->name+version spec)))
           (if (assoc-ref opts 'recursive)
               ;; Recursive import
               (with-error-handling
                 (map package->definition
                      (filter identity
                              (cran-recursive-import name
                                                     #:version version
                                                     #:repo (or (assoc-ref opts 'repo) 'cran)))))
               ;; Single import
               (let ((sexp (cran->guix-package name
                                               #:version version
                                               #:repo (or (assoc-ref opts 'repo) 'cran))))
                 (unless sexp
                   (leave (G_ "failed to download description for package '~a'~%")
                          name))
                 sexp))))
        (()
         (leave (G_ "too few arguments~%")))
        ((many ...)
         (leave (G_ "too many arguments~%")))))))
