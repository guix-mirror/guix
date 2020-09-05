;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2015, 2020 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-base32)
  #:use-module (gcrypt hash)
  #:use-module (guix base32)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports))

;; Test the (guix base32) module.

(test-begin "base32")

(test-assert "bytevector->base32-string"
  (fold (lambda (bv expected result)
          (and result
               (string=? (bytevector->base32-string bv)
                         expected)))
        #t

        ;; Examples from RFC 4648.
        (map string->utf8 '("" "f" "fo" "foo" "foob" "fooba" "foobar"))
        '(""
          "my"
          "mzxq"
          "mzxw6"
          "mzxw6yq"
          "mzxw6ytb"
          "mzxw6ytboi")))

(test-assert "base32-string->bytevector"
  (every (lambda (bv)
           (equal? (base32-string->bytevector
                    (bytevector->base32-string bv))
                   bv))
         ;; Examples from RFC 4648.
         (map string->utf8 '("" "f" "fo" "foo" "foob" "fooba" "foobar"))))

(test-assert "nix-base32-string->bytevector"
  (every (lambda (bv)
           (equal? (nix-base32-string->bytevector
                    (bytevector->nix-base32-string bv))
                   bv))
         ;; Examples from RFC 4648.
         (map string->utf8 '("" "f" "fo" "foo" "foob" "fooba" "foobar"))))

(test-equal "&invalid-base32-character"
  #\e
  (guard (c ((invalid-base32-character? c)
             (invalid-base32-character-value c)))
    (nix-base32-string->bytevector
     (string-append (make-string 51 #\a) "e"))))

(test-end)
