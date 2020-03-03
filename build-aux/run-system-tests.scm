;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (gnu packages package-management)
  #:use-module ((gnu ci) #:select (channel-source->package))
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module ((guix status) #:select (with-status-verbosity))
  #:use-module (guix monads)
  #:use-module (guix channels)
  #:use-module (guix derivations)
  #:use-module ((guix git-download) #:select (git-predicate))
  #:use-module (guix utils)
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

(define (tests-for-current-guix source)
  "Return a list of tests for perform, using Guix built from SOURCE, a channel
instance."
  ;; Honor the 'TESTS' environment variable so that one can select a subset
  ;; of tests to run in the usual way:
  ;;
  ;;   make check-system TESTS=installed-os
  (parameterize ((current-guix-package
                  (channel-source->package source)))
    (match (getenv "TESTS")
      (#f
       (all-system-tests))
      ((= string-tokenize (tests ...))
       (filter (lambda (test)
                 (member (system-test-name test) tests))
               (all-system-tests))))))



(define (run-system-tests . args)
  (define source
    (string-append (current-source-directory) "/.."))

  (with-store store
    (with-status-verbosity 2
      (run-with-store store
        ;; Intern SOURCE so that 'build-from-source' in (guix channels) sees
        ;; "fresh" file names and thus doesn't find itself loading .go files
        ;; from ~/.cache/guile when it loads 'build-aux/build-self.scm'.
        (mlet* %store-monad ((source -> (local-file source "guix-source"
                                                    #:recursive? #t
                                                    #:select?
                                                    (or (git-predicate source)
                                                        (const #t))))
                             (tests ->  (tests-for-current-guix source))
                             (drv (mapm %store-monad system-test-value tests))
                             (out -> (map derivation->output-path drv)))
          (format (current-error-port) "Running ~a system tests...~%"
                  (length tests))

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
