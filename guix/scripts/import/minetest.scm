;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 David Thompson <davet@gnu.org>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
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

(define-module (guix scripts import minetest)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix scripts)
  #:use-module (guix import minetest)
  #:use-module (guix import utils)
  #:use-module (guix scripts import)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:export (guix-import-minetest))


;;;
;;; Command-line options.
;;;

(define %default-options
  `((sort . ,%default-sort-key)))

(define (show-help)
  (display (G_ "Usage: guix import minetest AUTHOR/NAME
Import and convert the Minetest mod NAME by AUTHOR from ContentDB.\n"))
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -r, --recursive        import packages recursively"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (display (G_ "
      --sort=KEY         when choosing between multiple implementations,
                         choose the one with the highest value for KEY
                         (one of \"score\" (standard) or \"downloads\")"))
  (newline)
  (show-bug-report-information))

(define (verify-sort-order sort)
  "Verify SORT can be used to sort mods by."
  (unless (member sort '("score" "downloads" "reviews"))
    (leave (G_ "~a: not a valid key to sort by~%") sort))
  sort)

(define %options
  ;; Specification of the command-line options.
  (cons* (option '(#\h "help") #f #f
                 (lambda args
                   (show-help)
                   (exit 0)))
         (option '(#\V "version") #f #f
                 (lambda args
                   (show-version-and-exit "guix import minetest")))
         (option '(#\r "recursive") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'recursive #t result)))
         (option '("sort") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'sort (verify-sort-order arg) result)))
         %standard-import-options))


;;;
;;; Entry point.
;;;

(define (guix-import-minetest . args)
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
      ((name)
       (with-error-handling
         (let* ((sort (assoc-ref opts 'sort))
                (author/name (elaborate-contentdb-name name #:sort sort)))
           (if (assoc-ref opts 'recursive)
               ;; Recursive import
               (filter-map package->definition
                           (minetest-recursive-import author/name #:sort sort))
               ;; Single import
               (minetest->guix-package author/name #:sort sort)))))
      (()
       (leave (G_ "too few arguments~%")))
      ((many ...)
       (leave (G_ "too many arguments~%"))))))
