;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Katherine Cox-Buday <cox.katherine.e@gmail.com>
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

(define-module (guix scripts import go)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix scripts)
  #:use-module (guix import go)
  #:use-module (guix scripts import)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:export (guix-import-go))


;;;
;;; Command-line options.
;;;

(define %default-options
  '())

(define (show-help)
  (display (G_ "Usage: guix import go PACKAGE-PATH
Import and convert the Go module for PACKAGE-PATH.\n"))
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (display (G_ "
  -r, --recursive        generate package expressions for all Go modules\
 that are not yet in Guix"))
  (display (G_ "
  -p, --goproxy=GOPROXY  specify which goproxy server to use"))
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
                   (show-version-and-exit "guix import go")))
         (option '(#\r "recursive") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'recursive #t result)))
         (option '(#\p "goproxy") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'goproxy
                               (string->symbol arg)
                               (alist-delete 'goproxy result))))
         %standard-import-options))


;;;
;;; Entry point.
;;;

(define (guix-import-go . args)
  (define (parse-options)
    ;; Return the alist of option values.
    (args-fold* args %options
                (lambda (opt name arg result)
                  (leave (G_ "~A: unrecognized option~%") name))
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
      ((module-name)
       (if (assoc-ref opts 'recursive)
           (map (match-lambda
                  ((and ('package ('name name) . rest) pkg)
                   `(define-public ,(string->symbol name)
                      ,pkg))
                  (_ #f))
                (go-module-recursive-import module-name
                                            #:goproxy-url
                                            (or (assoc-ref opts 'goproxy)
                                                "https://proxy.golang.org")))
           (let ((sexp (go-module->guix-package module-name
                                                #:goproxy-url
                                                (or (assoc-ref opts 'goproxy)
                                                    "https://proxy.golang.org"))))
             (unless sexp
               (leave (G_ "failed to download meta-data for module '~a'~%")
                      module-name))
             sexp)))
      (()
       (leave (G_ "too few arguments~%")))
      ((many ...)
       (leave (G_ "too many arguments~%"))))))
