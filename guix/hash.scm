;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (rnrs io ports)
  #:use-module (system foreign)
  #:use-module ((guix build utils) #:select (dump-port))
  #:use-module (srfi srfi-11)
  #:export (sha256
            open-sha256-port
            port-sha256
            open-sha256-input-port))

;;; Commentary:
;;;
;;; Cryptographic hashes.
;;;
;;; Code:


;;;
;;; Hash.
;;;

(define-syntax GCRY_MD_SHA256
  ;; Value as of Libgcrypt 1.5.2.
  (identifier-syntax 8))

(define sha256
  (let ((hash (pointer->procedure void
                                  (dynamic-func "gcry_md_hash_buffer"
                                                (dynamic-link %libgcrypt))
                                  `(,int * * ,size_t))))
    (lambda (bv)
      "Return the SHA256 of BV as a bytevector."
      (let ((digest (make-bytevector (/ 256 8))))
        (hash GCRY_MD_SHA256 (bytevector->pointer digest)
              (bytevector->pointer bv) (bytevector-length bv))
        digest))))

(define open-sha256-md
  (let ((open (pointer->procedure int
                                  (dynamic-func "gcry_md_open"
                                                (dynamic-link %libgcrypt))
                                  `(* ,int ,unsigned-int))))
    (lambda ()
      (let* ((md  (bytevector->pointer (make-bytevector (sizeof '*))))
             (err (open md GCRY_MD_SHA256 0)))
        (if (zero? err)
            (dereference-pointer md)
            (throw 'gcrypt-error err))))))

(define md-write
  (pointer->procedure void
                      (dynamic-func "gcry_md_write"
                                    (dynamic-link %libgcrypt))
                      `(* * ,size_t)))

(define md-read
  (pointer->procedure '*
                      (dynamic-func "gcry_md_read"
                                    (dynamic-link %libgcrypt))
                      `(* ,int)))

(define md-close
  (pointer->procedure void
                      (dynamic-func "gcry_md_close"
                                    (dynamic-link %libgcrypt))
                      '(*)))


(define (open-sha256-port)
  "Return two values: an output port, and a thunk.  When the thunk is called,
it returns the SHA256 hash (a bytevector) of all the data written to the
output port."
  (define sha256-md
    (open-sha256-md))

  (define digest #f)

  (define (finalize!)
    (let ((ptr (md-read sha256-md 0)))
      (set! digest (bytevector-copy (pointer->bytevector ptr 32)))
      (md-close sha256-md)))

  (define (write! bv offset len)
    (if (zero? len)
        (begin
          (finalize!)
          0)
        (let ((ptr (bytevector->pointer bv offset)))
          (md-write sha256-md ptr len)
          len)))

  (define (close)
    (unless digest
      (finalize!)))

  (values (make-custom-binary-output-port "sha256"
                                          write! #f #f
                                          close)
          (lambda ()
            (unless digest
              (finalize!))
            digest)))

(define (port-sha256 port)
  "Return the SHA256 hash (a bytevector) of all the data drained from PORT."
  (let-values (((out get)
                (open-sha256-port)))
    (dump-port port out)
    (close-port out)
    (get)))

(define (open-sha256-input-port port)
  "Return an input port that wraps PORT and a thunk to get the hash of all the
data read from PORT.  The thunk always returns the same value."
  (define md
    (open-sha256-md))

  (define (read! bv start count)
    (let ((n (get-bytevector-n! port bv start count)))
      (if (eof-object? n)
          0
          (begin
            (unless digest
              (let ((ptr (bytevector->pointer bv start)))
                (md-write md ptr n)))
            n))))

  (define digest #f)

  (define (finalize!)
    (let ((ptr (md-read md 0)))
      (set! digest (bytevector-copy (pointer->bytevector ptr 32)))
      (md-close md)))

  (define (get-hash)
    (unless digest
      (finalize!))
    digest)

  (define (unbuffered port)
    ;; Guile <= 2.0.9 does not support 'setvbuf' on custom binary input ports.
    ;; If you get a wrong-type-arg error here, the fix is to upgrade Guile.  :-)
    (setvbuf port _IONBF)
    port)

  (values (unbuffered (make-custom-binary-input-port "sha256" read! #f #f #f))
          get-hash))

;;; hash.scm ends here
