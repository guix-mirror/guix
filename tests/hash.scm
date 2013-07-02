;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-hash)
  #:use-module (guix hash)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports))

;; Test the (guix hash) module.

(define %empty-sha256
  ;; SHA256 hash of the empty string.
  (base16-string->bytevector
   "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))

(define %hello-sha256
  ;; SHA256 hash of "hello world"
  (base16-string->bytevector
   "b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9"))

(test-begin "hash")

(test-equal "sha256, empty"
  %empty-sha256
  (sha256 #vu8()))

(test-equal "sha256, hello"
  %hello-sha256
  (sha256 (string->utf8 "hello world")))

(test-equal "open-sha256-port, empty"
  %empty-sha256
  (let-values (((port get)
                (open-sha256-port)))
    (close-port port)
    (get)))

(test-equal "open-sha256-port, hello"
  %hello-sha256
  (let-values (((port get)
                (open-sha256-port)))
    (put-bytevector port (string->utf8 "hello world"))
    (get)))

(test-assert "port-sha256"
  (let* ((file     (search-path %load-path "ice-9/psyntax.scm"))
         (size     (stat:size (stat file)))
         (contents (call-with-input-file file get-bytevector-all)))
    (equal? (sha256 contents)
            (call-with-input-file file port-sha256))))

(test-end)


(exit (= (test-runner-fail-count (test-runner-current)) 0))
