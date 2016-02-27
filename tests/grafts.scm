;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-grafts)
  #:use-module (guix derivations)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix grafts)
  #:use-module (guix tests)
  #:use-module ((gnu packages) #:select (search-bootstrap-binary))
  #:use-module (srfi srfi-64)
  #:use-module (rnrs io ports))

(define %store
  (open-connection-for-tests))

(define (bootstrap-binary name)
  (let ((bin (search-bootstrap-binary name (%current-system))))
    (and %store
         (add-to-store %store name #t "sha256" bin))))

(define %bash
  (bootstrap-binary "bash"))
(define %mkdir
  (bootstrap-binary "mkdir"))


(test-begin "grafts")

(test-assert "graft-derivation"
  (let* ((build `(begin
                   (mkdir %output)
                   (chdir %output)
                   (symlink %output "self")
                   (call-with-output-file "text"
                     (lambda (output)
                       (format output "foo/~a/bar" ,%mkdir)))
                   (symlink ,%bash "sh")))
         (orig  (build-expression->derivation %store "graft" build
                                              #:inputs `(("a" ,%bash)
                                                         ("b" ,%mkdir))))
         (one   (add-text-to-store %store "bash" "fake bash"))
         (two   (build-expression->derivation %store "mkdir"
                                              '(call-with-output-file %output
                                                 (lambda (port)
                                                   (display "fake mkdir" port)))))
         (graft (graft-derivation %store orig
                                  (list (graft
                                          (origin %bash)
                                          (replacement one))
                                        (graft
                                          (origin %mkdir)
                                          (replacement two))))))
    (and (build-derivations %store (list graft))
         (let ((two   (derivation->output-path two))
               (graft (derivation->output-path graft)))
           (and (string=? (format #f "foo/~a/bar" two)
                          (call-with-input-file (string-append graft "/text")
                            get-string-all))
                (string=? (readlink (string-append graft "/sh")) one)
                (string=? (readlink (string-append graft "/self")) graft))))))

(test-assert "graft-derivation, multiple outputs"
  (let* ((build `(begin
                   (symlink (assoc-ref %build-inputs "a")
                            (assoc-ref %outputs "one"))
                   (symlink (assoc-ref %outputs "one")
                            (assoc-ref %outputs "two"))))
         (orig  (build-expression->derivation %store "grafted" build
                                              #:inputs `(("a" ,%bash))
                                              #:outputs '("one" "two")))
         (repl  (add-text-to-store %store "bash" "fake bash"))
         (grafted (graft-derivation %store orig
                                    (list (graft
                                            (origin %bash)
                                            (replacement repl))))))
    (and (build-derivations %store (list grafted))
         (let ((one (derivation->output-path grafted "one"))
               (two (derivation->output-path grafted "two")))
           (and (string=? (readlink one) repl)
                (string=? (readlink two) one))))))

(test-end)


(exit (= (test-runner-fail-count (test-runner-current)) 0))
