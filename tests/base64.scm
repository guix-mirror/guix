;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Nikita Karetnikov <nikita@karetnikov.org>
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

(define-module (test-base64)
  #:use-module (guix base64)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-64))

(define (string->base64 str)
  (base64-encode (string->utf8 str)))

;;; Test vectors from <https://tools.ietf.org/rfc/rfc4648.txt>.

(test-begin "base64")

(test-equal "empty string"
  (string->base64 "")
  "")

(test-equal "f"
  (string->base64 "f")
  "Zg==")

(test-equal "fo"
  (string->base64 "fo")
  "Zm8=")

(test-equal "foo"
  (string->base64 "foo")
  "Zm9v")

(test-equal "foob"
  (string->base64 "foob")
  "Zm9vYg==")

(test-equal "fooba"
  (string->base64 "fooba")
  "Zm9vYmE=")

(test-equal "foobar"
  (string->base64 "foobar")
  "Zm9vYmFy")

(test-end "base64")