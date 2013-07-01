;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix hash)
  #:use-module (guix config)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:export (sha256))

;;; Commentary:
;;;
;;; Cryptographic hashes.
;;;
;;; Code:


;;;
;;; Hash.
;;;

(define sha256
  (let ((hash   (pointer->procedure void
                                    (dynamic-func "gcry_md_hash_buffer"
                                                  (dynamic-link %libgcrypt))
                                    `(,int * * ,size_t)))
        (sha256 8))                        ; GCRY_MD_SHA256, as of 1.5.0
    (lambda (bv)
      "Return the SHA256 of BV as a bytevector."
      (let ((digest (make-bytevector (/ 256 8))))
        (hash sha256 (bytevector->pointer digest)
              (bytevector->pointer bv) (bytevector-length bv))
        digest))))

;;; hash.scm ends here
