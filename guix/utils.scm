;;; Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
;;; Copyright (C) 2012 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of Guix.
;;;
;;; Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (guix utils)
  #:use-module (srfi srfi-60)
  #:use-module (rnrs bytevectors)
  #:use-module ((chop hash)
                #:select (bytevector-hash
                          hash-method/sha256))
  #:export (bytevector-quintet-length
            bytevector->base32-string
            bytevector->nix-base32-string
            sha256))

(define bytevector-quintet-ref
  (let* ((ref  bytevector-u8-ref)
         (ref+ (lambda (bv offset)
                 (let ((o (+ 1 offset)))
                   (if (>= o (bytevector-length bv))
                       0
                       (bytevector-u8-ref bv o)))))
         (ref0 (lambda (bv offset)
                 (bit-field (ref bv offset) 3 8)))
         (ref1 (lambda (bv offset)
                 (logior (ash (bit-field (ref bv offset) 0 3) 2)
                         (bit-field (ref+ bv offset) 6 8))))
         (ref2 (lambda (bv offset)
                 (bit-field (ref bv offset) 1 6)))
         (ref3 (lambda (bv offset)
                 (logior (ash (bit-field (ref bv offset) 0 1) 4)
                         (bit-field (ref+ bv offset) 4 8))))
         (ref4 (lambda (bv offset)
                 (logior (ash (bit-field (ref bv offset) 0 4) 1)
                         (bit-field (ref+ bv offset) 7 8))))
         (ref5 (lambda (bv offset)
                 (bit-field (ref bv offset) 2 7)))
         (ref6 (lambda (bv offset)
                 (logior (ash (bit-field (ref bv offset) 0 2) 3)
                         (bit-field (ref+ bv offset) 5 8))))
         (ref7 (lambda (bv offset)
                 (bit-field (ref bv offset) 0 5)))
         (refs (vector ref0 ref1 ref2 ref3 ref4 ref5 ref6 ref7)))
    (lambda (bv index)
      "Return the INDEXth quintet of BV."
      (let ((p (vector-ref refs (modulo index 8))))
        (p bv (quotient (* index 5) 8))))))

(define bytevector-quintet-ref-right
  (let* ((ref  bytevector-u8-ref)
         (ref+ (lambda (bv offset)
                 (let ((o (+ 1 offset)))
                   (if (>= o (bytevector-length bv))
                       0
                       (bytevector-u8-ref bv o)))))
         (ref0 (lambda (bv offset)
                 (bit-field (ref bv offset) 0 5)))
         (ref1 (lambda (bv offset)
                 (logior (bit-field (ref bv offset) 5 8)
                         (ash (bit-field (ref+ bv offset) 0 2) 3))))
         (ref2 (lambda (bv offset)
                 (bit-field (ref bv offset) 2 7)))
         (ref3 (lambda (bv offset)
                 (logior (bit-field (ref bv offset) 7 8)
                         (ash (bit-field (ref+ bv offset) 0 4) 1))))
         (ref4 (lambda (bv offset)
                 (logior (bit-field (ref bv offset) 4 8)
                         (ash (bit-field (ref+ bv offset) 0 1) 4))))
         (ref5 (lambda (bv offset)
                 (bit-field (ref bv offset) 1 6)))
         (ref6 (lambda (bv offset)
                 (logior (bit-field (ref bv offset) 6 8)
                         (ash (bit-field (ref+ bv offset) 0 3) 2))))
         (ref7 (lambda (bv offset)
                 (bit-field (ref bv offset) 3 8)))
         (refs (vector ref0 ref1 ref2 ref3 ref4 ref5 ref6 ref7)))
    (lambda (bv index)
      "Return the INDEXth quintet of BV, assuming quintets start from the
least-significant bits, contrary to what RFC 4648 describes."
      (let ((p (vector-ref refs (modulo index 8))))
        (p bv (quotient (* index 5) 8))))))

(define (bytevector-quintet-length bv)
  "Return the number of quintets (including truncated ones) available in BV."
  (ceiling (/ (* (bytevector-length bv) 8) 5)))

(define (bytevector-quintet-fold proc init bv)
  "Return the result of applying PROC to each quintet of BV and the result of
the previous application or INIT."
  (define len
    (bytevector-quintet-length bv))

  (let loop ((i 0)
             (r init))
    (if (= i len)
        r
        (loop (1+ i) (proc (bytevector-quintet-ref bv i) r)))))

(define (bytevector-quintet-fold-right proc init bv)
  "Return the result of applying PROC to each quintet of BV and the result of
the previous application or INIT."
  (define len
    (bytevector-quintet-length bv))

  (let loop ((i len)
             (r init))
    (if (zero? i)
        r
        (let ((j (- i 1)))
          (loop j (proc (bytevector-quintet-ref-right bv j) r))))))

(define (make-bytevector->base32-string quintet-fold base32-chars)
  (lambda (bv)
    "Return a base32 encoding of BV using BASE32-CHARS as the alphabet."
    (let ((chars (quintet-fold (lambda (q r)
                                 (cons (vector-ref base32-chars q)
                                       r))
                               '()
                               bv)))
      (list->string (reverse chars)))))

(define %nix-base32-chars
  ;; See `libutil/hash.cc'.
  #(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
    #\a #\b #\c #\d #\f #\g #\h #\i #\j #\k #\l #\m #\n
    #\p #\q #\r #\s #\v #\w #\x #\y #\z))

(define %rfc4648-base32-chars
  #(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
    #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
    #\2 #\3 #\4 #\5 #\6 #\7))

(define bytevector->base32-string
  (make-bytevector->base32-string bytevector-quintet-fold
                                  %rfc4648-base32-chars))

(define bytevector->nix-base32-string
  (make-bytevector->base32-string bytevector-quintet-fold-right
                                  %nix-base32-chars))

;;;
;;; Hash.
;;;

(define (sha256 bv)
  "Return the SHA256 of BV as a bytevector."
  (bytevector-hash hash-method/sha256 bv))

