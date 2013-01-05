;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix base32)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-60)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 vlist)
  #:export (bytevector-quintet-length
            bytevector->base32-string
            bytevector->nix-base32-string
            base32-string->bytevector
            nix-base32-string->bytevector))

;;; Commentary:
;;;
;;; A generic, customizable to convert bytevectors to/from a base32
;;; representation.
;;;
;;; Code:

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


(define bytevector-quintet-set!
  (let* ((setq! (lambda (bv offset start stop value)
                  (let ((v (bytevector-u8-ref bv offset))
                        (w (arithmetic-shift value start))
                        (m (bitwise-xor (1- (expt 2 stop))
                                        (1- (expt 2 start)))))
                    (bytevector-u8-set! bv offset
                                        (bitwise-merge m w v)))))
         (set0! (lambda (bv offset value)
                  (setq! bv offset 3 8 value)))
         (set1! (lambda (bv offset value)
                  (setq! bv offset 0 3 (bit-field value 2 5))
                  (or (= (+ 1 offset) (bytevector-length bv))
                      (setq! bv (+ 1 offset) 6 8 (bit-field value 0 2)))))
         (set2! (lambda (bv offset value)
                  (setq! bv offset 1 6 value)))
         (set3! (lambda (bv offset value)
                  (setq! bv offset 0 1 (bit-field value 4 5))
                  (or (= (+ 1 offset) (bytevector-length bv))
                      (setq! bv (+ 1 offset) 4 8 (bit-field value 0 4)))))
         (set4! (lambda (bv offset value)
                  (setq! bv offset 0 4 (bit-field value 1 5))
                  (or (= (+ 1 offset) (bytevector-length bv))
                      (setq! bv (+ 1 offset) 7 8  (bit-field value 0 1)))))
         (set5! (lambda (bv offset value)
                  (setq! bv offset 2 7 value)))
         (set6! (lambda (bv offset value)
                  (setq! bv offset 0 2 (bit-field value 3 5))
                  (or (= (+ 1 offset) (bytevector-length bv))
                      (setq! bv (+ 1 offset) 5 8 (bit-field value 0 3)))))
         (set7! (lambda (bv offset value)
                  (setq! bv offset 0 5 value)))
         (sets  (vector set0! set1! set2! set3! set4! set5! set6! set7!)))
    (lambda (bv index value)
      "Set the INDEXth quintet of BV to VALUE."
      (let ((p (vector-ref sets (modulo index 8))))
        (p bv (quotient (* index 5) 8) (logand value #x1f))))))

(define bytevector-quintet-set-right!
  (let* ((setq! (lambda (bv offset start stop value)
                  (let ((v (bytevector-u8-ref bv offset))
                        (w (arithmetic-shift value start))
                        (m (bitwise-xor (1- (expt 2 stop))
                                        (1- (expt 2 start)))))
                    (bytevector-u8-set! bv offset
                                        (bitwise-merge m w v)))))
         (set0! (lambda (bv offset value)
                  (setq! bv offset 0 5 value)))
         (set1! (lambda (bv offset value)
                  (setq! bv offset 5 8 (bit-field value 0 3))
                  (or (= (+ 1 offset) (bytevector-length bv))
                      (setq! bv (+ 1 offset) 0 2 (bit-field value 3 5)))))
         (set2! (lambda (bv offset value)
                  (setq! bv offset 2 7 value)))
         (set3! (lambda (bv offset value)
                  (setq! bv offset 7 8 (bit-field value 0 1))
                  (or (= (+ 1 offset) (bytevector-length bv))
                      (setq! bv (+ 1 offset) 0 4 (bit-field value 1 5)))))
         (set4! (lambda (bv offset value)
                  (setq! bv offset 4 8 (bit-field value 0 4))
                  (or (= (+ 1 offset) (bytevector-length bv))
                      (setq! bv (+ 1 offset) 0 1 (bit-field value 4 5)))))
         (set5! (lambda (bv offset value)
                  (setq! bv offset 1 6 value)))
         (set6! (lambda (bv offset value)
                  (setq! bv offset 6 8 (bit-field value 0 2))
                  (or (= (+ 1 offset) (bytevector-length bv))
                      (setq! bv (+ 1 offset) 0 3 (bit-field value 2 5)))))
         (set7! (lambda (bv offset value)
                  (setq! bv offset 3 8 value)))
         (sets  (vector set0! set1! set2! set3! set4! set5! set6! set7!)))
    (lambda (bv index value)
      "Set the INDEXth quintet of BV to VALUE, assuming quintets start from
the least-significant bits."
      (let ((p (vector-ref sets (modulo index 8))))
        (p bv (quotient (* index 5) 8) (logand value #x1f))))))

(define (base32-string-unfold f s)
  "Given procedure F which, when applied to a character, returns the
corresponding quintet, return the bytevector corresponding to string S."
  (define len (string-length s))

  (let ((bv (make-bytevector (quotient (* len 5) 8))))
    (string-fold (lambda (chr index)
                   (bytevector-quintet-set! bv index (f chr))
                   (+ 1 index))
                 0
                 s)
    bv))

(define (base32-string-unfold-right f s)
  "Given procedure F which, when applied to a character, returns the
corresponding quintet, return the bytevector corresponding to string S,
starting from the right of S."
  (define len (string-length s))

  (let ((bv (make-bytevector (quotient (* len 5) 8))))
    (string-fold-right (lambda (chr index)
                         (bytevector-quintet-set-right! bv index (f chr))
                         (+ 1 index))
                       0
                       s)
    bv))

(define (make-base32-string->bytevector base32-string-unfold base32-chars)
  (let ((char->value (let loop ((i 0)
                                (v vlist-null))
                       (if (= i (vector-length base32-chars))
                           v
                           (loop (+ 1 i)
                                 (vhash-consv (vector-ref base32-chars i)
                                              i v))))))
    (lambda (s)
      "Return the binary representation of base32 string S as a bytevector."
      (base32-string-unfold (lambda (chr)
                              (or (and=> (vhash-assv chr char->value) cdr)
                                  (error "invalid base32 character" chr)))
                            s))))

(define base32-string->bytevector
  (make-base32-string->bytevector base32-string-unfold %rfc4648-base32-chars))

(define nix-base32-string->bytevector
  (make-base32-string->bytevector base32-string-unfold-right %nix-base32-chars))

;;; base32.scm ends here
