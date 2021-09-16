;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2014, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
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

(define-module (guix base16)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-60)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 format)
  #:export (bytevector->base16-string
            base16-string->bytevector))

;;;
;;; Base 16.
;;;

(define (bytevector->base16-string bv)
  "Return the hexadecimal representation of BV's contents."
  (define len (bytevector-length bv))
  (define utf8 (make-bytevector (* len 2)))
  (let-syntax ((base16-octet-pairs
                (lambda (s)
                  (syntax-case s ()
                    (_
                     (string->utf8
                      (string-concatenate
                       (unfold (cut > <> 255)
                               (lambda (n)
                                 (format #f "~2,'0x" n))
                               1+
                               0))))))))
    (define octet-pairs base16-octet-pairs)
    (let loop ((i 0))
      (when (< i len)
        (bytevector-u16-native-set!
         utf8 (* 2 i)
         (bytevector-u16-native-ref octet-pairs
                                    (* 2 (bytevector-u8-ref bv i))))
        (loop (+ i 1))))
    (utf8->string utf8)))

(define base16-string->bytevector
  (let ((chars->value (fold (lambda (i r)
                              (vhash-consv (string-ref (number->string i 16)
                                                       0)
                                           i r))
                            vlist-null
                            (iota 16))))
    (lambda (s)
      "Return the bytevector whose hexadecimal representation is string S."
      (define bv
        (make-bytevector (quotient (string-length s) 2) 0))

      (string-fold (lambda (chr i)
                     (let ((j (quotient i 2))
                           (v (and=> (vhash-assv chr chars->value) cdr)))
                       (if v
                           (if (zero? (logand i 1))
                               (bytevector-u8-set! bv j
                                                   (arithmetic-shift v 4))
                               (let ((w (bytevector-u8-ref bv j)))
                                 (bytevector-u8-set! bv j (logior v w))))
                           (error "invalid hexadecimal character" chr)))
                     (+ i 1))
                   0
                   s)
      bv)))

