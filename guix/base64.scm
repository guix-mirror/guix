;; -*- mode: scheme; coding: utf-8 -*-
;;
;; This module was renamed from (weinholt text base64 (1 0 20100612)) to
;; (guix base64) by Nikita Karetnikov <nikita@karetnikov.org> on
;; February 12, 2014.
;;
;; Some optimizations made by Ludovic Courtès <ludo@gnu.org>, 2015.
;; Turned into a Guile module (instead of R6RS).
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; This file incorporates work covered by the following copyright and  
;; permission notice:
;;
;;   Copyright © 2009, 2010 Göran Weinholt <goran@weinholt.se>
;;
;;   Permission is hereby granted, free of charge, to any person obtaining a
;;   copy of this software and associated documentation files (the "Software"),
;;   to deal in the Software without restriction, including without limitation
;;   the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;   and/or sell copies of the Software, and to permit persons to whom the
;;   Software is furnished to do so, subject to the following conditions:
;;
;;   The above copyright notice and this permission notice shall be included in
;;   all copies or substantial portions of the Software.
;;
;;   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;   DEALINGS IN THE SOFTWARE.

;; RFC 4648 Base-N Encodings

(define-module (guix base64)
  #:export (base64-encode
            base64-decode
            base64-alphabet
            base64url-alphabet
            get-delimited-base64
            put-delimited-base64)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-60)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports))


(define-syntax define-alias
  (syntax-rules ()
    ((_ new old)
     (define-syntax new (identifier-syntax old)))))

;; Force the use of Guile's own primitives to avoid the overhead of its 'fx'
;; procedures.

(define-alias fxbit-field bit-field)
(define-alias fxarithmetic-shift ash)
(define-alias fxarithmetic-shift-left ash)
(define-alias fxand logand)
(define-alias fxior logior)
(define-alias fxxor logxor)
(define-alias fx=? =)
(define-alias fx+ +)
(define-alias mod modulo)

(define-syntax-rule (assert exp)
  (unless exp
    (throw 'assertion-failure 'exp)))

(define base64-alphabet
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(define base64url-alphabet
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_")

(define base64-encode
  (case-lambda
    ;; Simple interface. Returns a string containing the canonical
    ;; base64 representation of the given bytevector.
    ((bv)
     (base64-encode bv 0 (bytevector-length bv) #f #f base64-alphabet #f))
    ((bv start)
     (base64-encode bv start (bytevector-length bv) #f #f base64-alphabet #f))
    ((bv start end)
     (base64-encode bv start end #f #f base64-alphabet #f))
    ((bv start end line-length)
     (base64-encode bv start end line-length #f base64-alphabet #f))
    ((bv start end line-length no-padding)
     (base64-encode bv start end line-length no-padding base64-alphabet #f))
    ((bv start end line-length no-padding alphabet)
     (base64-encode bv start end line-length no-padding alphabet #f))
    ;; Base64 encodes the bytes [start,end[ in the given bytevector.
    ;; Lines are limited to line-length characters (unless #f),
    ;; which must be a multiple of four. To omit the padding
    ;; characters (#\=) set no-padding to a true value. If port is
    ;; #f, returns a string.
    ((bv start end line-length no-padding alphabet port)
     (assert (or (not line-length) (zero? (mod line-length 4))))
     (let-values (((p extract) (if port
                                   (values port (lambda () (values)))
                                   (open-string-output-port))))
       (letrec ((put (if line-length
                         (let ((chars 0))
                           (lambda (p c)
                             (when (fx=? chars line-length)
                               (set! chars 0)
                               (put-char p #\linefeed))
                             (set! chars (fx+ chars 1))
                             (put-char p c)))
                         put-char)))
         (let lp ((i start))
           (cond ((= i end))
                 ((<= (+ i 3) end)
                  (let ((x (bytevector-uint-ref bv i (endianness big) 3)))
                    (put p (string-ref alphabet (fxbit-field x 18 24)))
                    (put p (string-ref alphabet (fxbit-field x 12 18)))
                    (put p (string-ref alphabet (fxbit-field x 6 12)))
                    (put p (string-ref alphabet (fxbit-field x 0 6)))
                    (lp (+ i 3))))
                 ((<= (+ i 2) end)
                  (let ((x (fxarithmetic-shift-left (bytevector-u16-ref bv i (endianness big)) 8)))
                    (put p (string-ref alphabet (fxbit-field x 18 24)))
                    (put p (string-ref alphabet (fxbit-field x 12 18)))
                    (put p (string-ref alphabet (fxbit-field x 6 12)))
                    (unless no-padding
                      (put p #\=))))
                 (else
                  (let ((x (fxarithmetic-shift-left (bytevector-u8-ref bv i) 16)))
                    (put p (string-ref alphabet (fxbit-field x 18 24)))
                    (put p (string-ref alphabet (fxbit-field x 12 18)))
                    (unless no-padding
                      (put p #\=)
                      (put p #\=)))))))
       (extract)))))

  ;; Decodes a base64 string. The string must contain only pure
  ;; unpadded base64 data.
  
(define base64-decode
  (case-lambda
    ((str)
     (base64-decode str base64-alphabet #f))
    ((str alphabet)
     (base64-decode str alphabet #f))
    ((str alphabet port)
     (unless (zero? (mod (string-length str) 4))
       (error 'base64-decode
              "input string must be a multiple of four characters"))
     (let-values (((p extract) (if port
                                   (values port (lambda () (values)))
                                   (open-bytevector-output-port))))
       (do ((i 0 (+ i 4)))
           ((= i (string-length str))
            (extract))
         (let ((c1 (string-ref str i))
               (c2 (string-ref str (+ i 1)))
               (c3 (string-ref str (+ i 2)))
               (c4 (string-ref str (+ i 3))))
           ;; TODO: be more clever than string-index
           (let ((i1 (string-index alphabet c1))
                 (i2 (string-index alphabet c2))
                 (i3 (string-index alphabet c3))
                 (i4 (string-index alphabet c4)))
             (cond ((and i1 i2 i3 i4)
                    (let ((x (fxior (fxarithmetic-shift-left i1 18)
                                    (fxarithmetic-shift-left i2 12)
                                    (fxarithmetic-shift-left i3 6)
                                    i4)))
                      (put-u8 p (fxbit-field x 16 24))
                      (put-u8 p (fxbit-field x 8 16))
                      (put-u8 p (fxbit-field x 0 8))))
                   ((and i1 i2 i3 (char=? c4 #\=)
                         (= i (- (string-length str) 4)))
                    (let ((x (fxior (fxarithmetic-shift-left i1 18)
                                    (fxarithmetic-shift-left i2 12)
                                    (fxarithmetic-shift-left i3 6))))
                      (put-u8 p (fxbit-field x 16 24))
                      (put-u8 p (fxbit-field x 8 16))))
                   ((and i1 i2 (char=? c3 #\=) (char=? c4 #\=)
                         (= i (- (string-length str) 4)))
                    (let ((x (fxior (fxarithmetic-shift-left i1 18)
                                    (fxarithmetic-shift-left i2 12))))
                      (put-u8 p (fxbit-field x 16 24))))
                   (else
                    (error 'base64-decode "invalid input"
                           (list c1 c2 c3 c4)))))))))))

(define (get-line-comp f port)
  (if (port-eof? port)
      (eof-object)
      (f (get-line port))))

  ;; Reads the common -----BEGIN/END type----- delimited format from
  ;; the given port. Returns two values: a string with the type and a
  ;; bytevector containing the base64 decoded data. The second value
  ;; is the eof object if there is an eof before the BEGIN delimiter.
  
(define (get-delimited-base64 port)
  (define (get-first-data-line port)
    ;; Some MIME data has header fields in the same format as mail
    ;; or http. These are ignored.
    (let ((line (get-line-comp string-trim-both port)))
      (cond ((eof-object? line) line)
            ((string-index line #\:)
             (let lp ()                           ;read until empty line
               (let ((line (get-line-comp string-trim-both port)))
                 (if (string=? line "")
                     (get-line-comp string-trim-both port)
                     (lp)))))
            (else line))))
  (let ((line (get-line-comp string-trim-both port)))
    (cond ((eof-object? line)
           (values "" (eof-object)))
          ((string=? line "")
           (get-delimited-base64 port))
          ((and (string-prefix? "-----BEGIN " line)
                (string-suffix? "-----" line))
           (let* ((type (substring line 11 (- (string-length line) 5)))
                  (endline (string-append "-----END " type "-----")))
             (let-values (((outp extract) (open-bytevector-output-port)))
               (let lp ((line (get-first-data-line port)))
                 (cond ((eof-object? line)
                        (error 'get-delimited-base64
                               "unexpected end of file"))
                       ((string-prefix? "-" line)
                        (unless (string=? line endline)
                          (error 'get-delimited-base64
                                 "bad end delimiter" type line))
                        (values type (extract)))
                       (else
                        (unless (and (= (string-length line) 5)
                                     (string-prefix? "=" line)) ;Skip Radix-64 checksum
                          (base64-decode line base64-alphabet outp))
                        (lp (get-line-comp string-trim-both port))))))))
          (else     ;skip garbage (like in openssl x509 -in foo -text output).
           (get-delimited-base64 port)))))

(define put-delimited-base64
  (case-lambda
    ((port type bv line-length)
     (display (string-append "-----BEGIN " type "-----\n") port)
     (base64-encode bv 0 (bytevector-length bv)
                    line-length #f base64-alphabet port)
     (display (string-append "\n-----END " type "-----\n") port))
    ((port type bv)
     (put-delimited-base64 port type bv 76))))
