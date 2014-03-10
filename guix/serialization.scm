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

(define-module (guix serialization)
  #:use-module (guix utils)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (write-int read-int
            write-long-long read-long-long
            write-padding
            write-string read-string read-latin1-string
            write-string-list read-string-list
            write-string-pairs
            write-store-path read-store-path
            write-store-path-list read-store-path-list))

;;; Comment:
;;;
;;; Serialization procedures used by the RPCs and the Nar format.  This module
;;; is for internal consumption.
;;;
;;; Code:

;; Similar to serialize.cc in Nix.

(define (write-int n p)
  (let ((b (make-bytevector 8 0)))
    (bytevector-u32-set! b 0 n (endianness little))
    (put-bytevector p b)))

(define (read-int p)
  (let ((b (get-bytevector-n p 8)))
    (bytevector-u32-ref b 0 (endianness little))))

(define (write-long-long n p)
  (let ((b (make-bytevector 8 0)))
    (bytevector-u64-set! b 0 n (endianness little))
    (put-bytevector p b)))

(define (read-long-long p)
  (let ((b (get-bytevector-n p 8)))
    (bytevector-u64-ref b 0 (endianness little))))

(define write-padding
  (let ((zero (make-bytevector 8 0)))
    (lambda (n p)
      (let ((m (modulo n 8)))
        (or (zero? m)
            (put-bytevector p zero 0 (- 8 m)))))))

(define (write-string s p)
  (let* ((s (string->utf8 s))
         (l (bytevector-length s))
         (m (modulo l 8))
         (b (make-bytevector (+ 8 l (if (zero? m) 0 (- 8 m))))))
    (bytevector-u32-set! b 0 l (endianness little))
    (bytevector-copy! s 0 b 8 l)
    (put-bytevector p b)))

(define (read-string p)
  (let* ((len (read-int p))
         (m   (modulo len 8))
         (bv  (get-bytevector-n p len))
         (str (utf8->string bv)))
    (or (zero? m)
        (get-bytevector-n p (- 8 m)))
    str))

(define (read-latin1-string p)
  (let* ((len (read-int p))
         (m   (modulo len 8))
         (str (get-string-n p len)))
    (or (zero? m)
        (get-bytevector-n p (- 8 m)))
    str))

(define (write-string-list l p)
  (write-int (length l) p)
  (for-each (cut write-string <> p) l))

(define (write-string-pairs l p)
  (write-int (length l) p)
  (for-each (match-lambda
             ((first . second)
              (write-string first p)
              (write-string second p)))
            l))

(define (read-string-list p)
  (let ((len (read-int p)))
    (unfold (cut >= <> len)
            (lambda (i)
              (read-string p))
            1+
            0)))

(define (write-store-path f p)
  (write-string f p))                             ; TODO: assert path

(define (read-store-path p)
  (read-string p))                                ; TODO: assert path

(define write-store-path-list write-string-list)
(define read-store-path-list read-string-list)

;;; serialization.scm ends here
