;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
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

(define-module (run-system-tests)
  #:use-module (gnu tests)
  #:use-module (guix store)
  #:use-module ((guix status) #:select (with-status-verbosity))
  #:use-module (guix monads)
  #:use-module (guix derivations)
  #:use-module (guix ui)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (ice-9 match)
  #:export (run-system-tests))

(define (built-derivations* drv)
  (lambda (store)
    (guard (c ((store-protocol-error? c)
               (values #f store)))
      (values (build-derivations store drv) store))))

(define (filterm mproc lst)                       ;XXX: move to (guix monads)
  (with-monad %store-monad
    (>>= (foldm %store-monad
                (lambda (item result)
                  (mlet %store-monad ((keep? (mproc item)))
                    (return (if keep?
                                (cons item result)
                                result))))
                '()
                lst)
         (lift1 reverse %store-monad))))

(define (run-system-tests . args)
  (define tests
    ;; Honor the 'TESTS' environment variable so that one can select a subset
    ;; of tests to run in the usual way:
    ;;
    ;;   make check-system TESTS=installed-os
    (match (getenv "TESTS")
      (#f
       (all-system-tests))
      ((= string-tokenize (tests ...))
       (filter (lambda (test)
                 (member (system-test-name test) tests))
               (all-system-tests)))))

  (format (current-error-port) "Running ~a system tests...~%"
          (length tests))

  (with-store store
    (with-status-verbosity 2
      (run-with-store store
        (mlet* %store-monad ((drv (mapm %store-monad system-test-value tests))
                             (out -> (map derivation->output-path drv)))
          (mbegin %store-monad
            (show-what-to-build* drv)
            (set-build-options* #:keep-going? #t #:keep-failed? #t
                                #:print-build-trace #t
                                #:print-extended-build-trace? #t
                                #:fallback? #t)
            (built-derivations* drv)
            (mlet %store-monad ((valid  (filterm (store-lift valid-path?)
                                                 out))
                                (failed (filterm (store-lift
                                                  (negate valid-path?))
                                                 out)))
              (format #t "TOTAL: ~a\n" (length drv))
              (for-each (lambda (item)
                          (format #t "PASS: ~a~%" item))
                        valid)
              (for-each (lambda (item)
                          (format #t "FAIL: ~a~%" item))
                        failed)
              (exit (null? failed)))))))))
