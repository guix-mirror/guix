;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015, 2017 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (guix scripts import json)
  #:use-module (json)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix scripts)
  #:use-module (guix import utils)
  #:use-module (guix import json)
  #:use-module (guix scripts import)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-37)
  #:use-module (srfi srfi-41)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 format)
  #:export (guix-import-json))


;;;
;;; Command-line options.
;;;

(define %default-options
  '())

(define (show-help)
  (display (G_ "Usage: guix import json PACKAGE-FILE
Import and convert the JSON package definition in PACKAGE-FILE.\n"))
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
                   (show-version-and-exit "guix import json")))
         %standard-import-options))


;;;
;;; Entry point.
;;;

(define (guix-import-json . args)
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
      ((file-name)
       (or (json->code file-name)
           (leave (G_ "invalid JSON in file '~a'~%") file-name)))
      (()
       (leave (G_ "too few arguments~%")))
      ((many ...)
       (leave (G_ "too many arguments~%"))))))
