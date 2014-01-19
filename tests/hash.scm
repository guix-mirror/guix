;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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

(define (supports-unbuffered-cbip?)
  "Return #t if unbuffered custom binary input ports (CBIPs) are supported.
In Guile <= 2.0.9, CBIPs were always fully buffered, so the
'open-sha256-input-port' does not work there."
  (false-if-exception
   (setvbuf (make-custom-binary-input-port "foo" pk #f #f #f) _IONBF)))


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

(test-skip (if (supports-unbuffered-cbip?) 0 4))

(test-equal "open-sha256-input-port, empty"
  `("" ,%empty-sha256)
  (let-values (((port get)
                (open-sha256-input-port (open-string-input-port ""))))
    (let ((str (get-string-all port)))
      (list str (get)))))

(test-equal "open-sha256-input-port, hello"
  `("hello world" ,%hello-sha256)
  (let-values (((port get)
                (open-sha256-input-port
                 (open-bytevector-input-port
                  (string->utf8 "hello world")))))
    (let ((str (get-string-all port)))
      (list str (get)))))

(test-equal "open-sha256-input-port, hello, one two"
  (list (string->utf8 "hel") (string->utf8 "lo")
        (base16-string->bytevector                ; echo -n hello | sha256sum
         "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824")
        " world")
  (let-values (((port get)
                (open-sha256-input-port
                 (open-bytevector-input-port (string->utf8 "hello world")))))
    (let* ((one   (get-bytevector-n port 3))
           (two   (get-bytevector-n port 2))
           (hash  (get))
           (three (get-string-all port)))
      (list one two hash three))))

(test-equal "open-sha256-input-port, hello, read from wrapped port"
  (list (string->utf8 "hello")
        (base16-string->bytevector                ; echo -n hello | sha256sum
         "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824")
        " world")
  (let*-values (((wrapped)
                 (open-bytevector-input-port (string->utf8 "hello world")))
                ((port get)
                 (open-sha256-input-port wrapped)))
    (let* ((hello (get-bytevector-n port 5))
           (hash  (get))

           ;; Now read from WRAPPED to make sure its current position is
           ;; correct.
           (world (get-string-all wrapped)))
      (list hello hash world))))

(test-end)


(exit (= (test-runner-fail-count (test-runner-current)) 0))
