;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014, 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2016 Danny Milosavljevic <dannym+a@scratchpost.org>
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2017 Alex Kost <alezost@gmail.com>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2019 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2019 Simon Tournier <zimon.toutoune@gmail.com>
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

(define-module (guix scripts lint)
  #:use-module (guix packages)
  #:use-module (guix lint)
  #:use-module (guix ui)
  #:use-module (guix store)
  #:use-module (guix scripts)
  #:use-module (guix scripts build)
  #:use-module (gnu packages)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-37)
  #:export (guix-lint
            run-checkers))

(define (emit-warnings warnings)
  ;; Emit a warning about PACKAGE, printing the location of FIELD if it is
  ;; given, the location of PACKAGE otherwise, the full name of PACKAGE and the
  ;; provided MESSAGE.
  (for-each
   (lambda (lint-warning)
     (let ((package (lint-warning-package lint-warning))
           (loc     (lint-warning-location lint-warning)))
       (info loc (G_ "~a@~a: ~a~%")
             (package-name package) (package-version package)
             (lint-warning-message lint-warning))))
   warnings))

(define* (run-checkers package checkers #:key store)
  "Run the given CHECKERS on PACKAGE."
  (let ((tty? (isatty? (current-error-port))))
    (for-each (lambda (checker)
                (when tty?
                  (format (current-error-port) "checking ~a@~a [~a]...\x1b[K\r"
                          (package-name package) (package-version package)
                          (lint-checker-name checker))
                  (force-output (current-error-port)))
                (emit-warnings
                 (if (lint-checker-requires-store? checker)
                     ((lint-checker-check checker) package #:store store)
                     ((lint-checker-check checker) package))))
              checkers)
    (when tty?
      (format (current-error-port) "\x1b[K")
      (force-output (current-error-port)))))

(define (list-checkers-and-exit checkers)
  ;; Print information about all available checkers and exit.
  (format #t (G_ "Available checkers:~%"))
  (for-each (lambda (checker)
              (format #t "- ~a: ~a~%"
                      (lint-checker-name checker)
                      (G_ (lint-checker-description checker))))
            checkers)
  (exit 0))


;;;
;;; Command-line options.
;;;

(define %default-options
  ;; Alist of default option values.
  '())

(define (show-help)
  (display (G_ "Usage: guix lint [OPTION]... [PACKAGE]...
Run a set of checkers on the specified package; if none is specified,
run the checkers on all packages.\n"))
  (display (G_ "
  -c, --checkers=CHECKER1,CHECKER2...
                         only run the specified checkers"))
  (display (G_ "
  -L, --load-path=DIR    prepend DIR to the package module search path"))
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -l, --list-checkers    display the list of available lint checkers"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))


(define %options
  ;; Specification of the command-line options.
  ;; TODO: add some options:
  ;; * --certainty=[low,medium,high]: only run checkers that have at least this
  ;;                                  'certainty'.
  (list (option '(#\c "checkers") #t #f
                (lambda (opt name arg result)
                  (let ((names (map string->symbol (string-split arg #\,)))
                        (checker-names (map lint-checker-name %all-checkers)))
                    (for-each (lambda (c)
                                (unless (memq c checker-names)
                                  (leave (G_ "~a: invalid checker~%") c)))
                              names)
                    (alist-cons 'checkers
                                (filter (lambda (checker)
                                          (member (lint-checker-name checker)
                                                  names))
                                        %all-checkers)
                                result))))
        (option '(#\n "no-network") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'checkers
                              %local-checkers
                              (alist-delete 'checkers
                                            result))))
        (find (lambda (option)
                (member "load-path" (option-names option)))
              %standard-build-options)
        (option '(#\h "help") #f #f
                (lambda args
                  (show-help)
                  (exit 0)))
        (option '(#\l "list-checkers") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'list?
                              #t
                              result)))
        (option '(#\V "version") #f #f
                (lambda args
                  (show-version-and-exit "guix lint")))))


;;;
;;; Entry Point
;;;

(define (guix-lint . args)
  (define (parse-options)
    ;; Return the alist of option values.
    (parse-command-line args %options (list %default-options)
                        #:build-options? #f))

  (let* ((opts (parse-options))
         (args (filter-map (match-lambda
                             (('argument . value)
                              value)
                             (_ #f))
                           (reverse opts)))
         (checkers (or (assoc-ref opts 'checkers) %all-checkers)))

    (when (assoc-ref opts 'list?)
      (list-checkers-and-exit checkers))

    (let ((any-lint-checker-requires-store?
           (any lint-checker-requires-store? checkers)))

      (define (call-maybe-with-store proc)
        (if any-lint-checker-requires-store?
            (with-store store
              (proc store))
            (proc #f)))

      (call-maybe-with-store
       (lambda (store)
         (cond
          ((null? args)
           (fold-packages (lambda (p r) (run-checkers p checkers
                                                      #:store store)) '()))
          (else
           (for-each (lambda (spec)
                       (run-checkers (specification->package spec) checkers
                                     #:store store))
                     args))))))))
