;;;; json.scm --- JSON reader/writer
;;;; Copyright (C) 2015 Free Software Foundation, Inc.
;;;;
;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
;;;;

(define-module (guix build json)  ;; originally (ice-9 json)
  #:use-module (ice-9 match)
  #:export (read-json write-json))

;; Snarfed from
;; https://github.com/cwebber/activitystuff/blob/master/activitystuff/contrib/json.scm
;; 

;;;
;;; Reader
;;;

(define (json-error port)
  (throw 'json-error port))

(define (assert-char port char)
  "Read a character from PORT and throw an invalid JSON error if the
character is not CHAR."
  (unless (eqv? (read-char port) char)
    (json-error port)))

(define (whitespace? char)
  "Return #t if CHAR is a whitespace character."
  (char-set-contains? char-set:whitespace char))

(define (consume-whitespace port)
  "Discard characters from PORT until a non-whitespace character is
encountered.."
  (match (peek-char port)
    ((? eof-object?) *unspecified*)
    ((? whitespace?)
     (read-char port)
     (consume-whitespace port))
    (_ *unspecified*)))

(define (make-keyword-reader keyword value)
  "Parse the keyword symbol KEYWORD as VALUE."
  (let ((str (symbol->string keyword)))
    (lambda (port)
      (let loop ((i 0))
        (cond
         ((= i (string-length str)) value)
         ((eqv? (string-ref str i) (read-char port))
          (loop (1+ i)))
         (else (json-error port)))))))

(define read-true (make-keyword-reader 'true #t))
(define read-false (make-keyword-reader 'false #f))
(define read-null (make-keyword-reader 'null #nil))

(define (read-hex-digit port)
  "Read a hexadecimal digit from PORT."
  (match (read-char port)
    (#\0 0)
    (#\1 1)
    (#\2 2)
    (#\3 3)
    (#\4 4)
    (#\5 5)
    (#\6 6)
    (#\7 7)
    (#\8 8)
    (#\9 9)
    ((or #\A #\a) 10)
    ((or #\B #\b) 11)
    ((or #\C #\c) 12)
    ((or #\D #\d) 13)
    ((or #\E #\e) 14)
    ((or #\F #\f) 15)
    (_ (json-error port))))

(define (read-utf16-character port)
  "Read a hexadecimal encoded UTF-16 character from PORT."
  (integer->char
   (+ (* (read-hex-digit port) (expt 16 3))
      (* (read-hex-digit port) (expt 16 2))
      (* (read-hex-digit port) 16)
      (read-hex-digit port))))

(define (read-escape-character port)
  "Read escape character from PORT."
  (match (read-char port)
    (#\" #\")
    (#\\ #\\)
    (#\/ #\/)
    (#\b #\backspace)
    (#\f #\page)
    (#\n #\newline)
    (#\r #\return)
    (#\t #\tab)
    (#\u (read-utf16-character port))
    (_ (json-error port))))

(define (read-string port)
  "Read a JSON encoded string from PORT."
  (assert-char port #\")
  (let loop ((result '()))
    (match (read-char port)
      ((? eof-object?) (json-error port))
      (#\" (list->string (reverse result)))
      (#\\ (loop (cons (read-escape-character port) result)))
      (char (loop (cons char result))))))

(define char-set:json-digit
  (char-set #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(define (digit? char)
  (char-set-contains? char-set:json-digit char))

(define (read-digit port)
  "Read a digit 0-9 from PORT."
  (match (read-char port)
    (#\0 0)
    (#\1 1)
    (#\2 2)
    (#\3 3)
    (#\4 4)
    (#\5 5)
    (#\6 6)
    (#\7 7)
    (#\8 8)
    (#\9 9)
    (else (json-error port))))

(define (read-digits port)
  "Read a sequence of digits from PORT."
  (let loop ((result '()))
    (match (peek-char port)
      ((? eof-object?)
       (reverse result))
      ((? digit?)
       (loop (cons (read-digit port) result)))
      (else (reverse result)))))

(define (list->integer digits)
  "Convert the list DIGITS to an integer."
  (let loop ((i (1- (length digits)))
             (result 0)
             (digits digits))
    (match digits
      (() result)
      ((n . tail)
       (loop (1- i)
             (+ result (* n (expt 10 i)))
             tail)))))

(define (read-positive-integer port)
  "Read a positive integer with no leading zeroes from PORT."
  (match (read-digits port)
    ((0 . _)
     (json-error port)) ; no leading zeroes allowed
    ((digits ...)
     (list->integer digits))))

(define (read-exponent port)
  "Read exponent from PORT."
  (define (read-expt)
    (list->integer (read-digits port)))

  (unless (memv (read-char port) '(#\e #\E))
    (json-error port))

  (match (peek-char port)
    ((? eof-object?)
     (json-error port))
    (#\-
     (read-char port)
     (- (read-expt)))
    (#\+
     (read-char port)
     (read-expt))
    ((? digit?)
     (read-expt))
    (_ (json-error port))))

(define (read-fraction port)
  "Read fractional number part from PORT as an inexact number."
  (let* ((digits      (read-digits port))
         (numerator   (list->integer digits))
         (denomenator (expt 10 (length digits))))
    (/ numerator denomenator)))

(define (read-positive-number port)
  "Read a positive number from PORT."
  (let* ((integer (match (peek-char port)
                    ((? eof-object?)
                     (json-error port))
                    (#\0
                     (read-char port)
                     0)
                    ((? digit?)
                     (read-positive-integer port))
                    (_ (json-error port))))
         (fraction (match (peek-char port)
                     (#\.
                      (read-char port)
                      (read-fraction port))
                     (_ 0)))
         (exponent (match (peek-char port)
                     ((or #\e #\E)
                      (read-exponent port))
                     (_ 0)))
         (n (* (+ integer fraction) (expt 10 exponent))))

    ;; Keep integers as exact numbers, but convert numbers encoded as
    ;; floating point numbers to an inexact representation.
    (if (zero? fraction)
        n
        (exact->inexact n))))

(define (read-number port)
  "Read a number from PORT"
  (match (peek-char port)
    ((? eof-object?)
     (json-error port))
    (#\-
     (read-char port)
     (- (read-positive-number port)))
    ((? digit?)
     (read-positive-number port))
    (_ (json-error port))))

(define (read-object port)
  "Read key/value map from PORT."
  (define (read-key+value-pair)
    (let ((key (read-string port)))
      (consume-whitespace port)
      (assert-char port #\:)
      (consume-whitespace port)
      (let ((value (read-value port)))
        (cons key value))))

  (assert-char port #\{)
  (consume-whitespace port)

  (if (eqv? #\} (peek-char port))
      (begin
        (read-char port)
        '(@)) ; empty object
      (let loop ((result (list (read-key+value-pair))))
        (consume-whitespace port)
        (match (peek-char port)
          (#\, ; read another value
           (read-char port)
           (consume-whitespace port)
           (loop (cons (read-key+value-pair) result)))
          (#\} ; end of object
           (read-char port)
           (cons '@ (reverse result)))
          (_ (json-error port))))))

(define (read-array port)
  "Read array from PORT."
  (assert-char port #\[)
  (consume-whitespace port)

  (if (eqv? #\] (peek-char port))
      (begin
        (read-char port)
        '()) ; empty array
      (let loop ((result (list (read-value port))))
        (consume-whitespace port)
        (match (peek-char port)
          (#\, ; read another value
           (read-char port)
           (consume-whitespace port)
           (loop (cons (read-value port) result)))
          (#\] ; end of array
           (read-char port)
           (reverse result))
          (_ (json-error port))))))

(define (read-value port)
  "Read a JSON value from PORT."
  (consume-whitespace port)
  (match (peek-char port)
    ((? eof-object?) (json-error port))
    (#\" (read-string port))
    (#\{ (read-object port))
    (#\[ (read-array port))
    (#\t (read-true port))
    (#\f (read-false port))
    (#\n (read-null port))
    ((or #\- (? digit?))
     (read-number port))
    (_ (json-error port))))

(define (read-json port)
  "Read JSON text from port and return an s-expression representation."
  (let ((result (read-value port)))
    (consume-whitespace port)
    (unless (eof-object? (peek-char port))
      (json-error port))
    result))


;;;
;;; Writer
;;;

(define (write-string str port)
  "Write STR to PORT in JSON string format."
  (define (escape-char char)
    (display (match char
               (#\" "\\\"")
               (#\\ "\\\\")
               (#\/ "\\/")
               (#\backspace "\\b")
               (#\page "\\f")
               (#\newline "\\n")
               (#\return "\\r")
               (#\tab "\\t")
               (_ char))
             port))

  (display "\"" port)
  (string-for-each escape-char str)
  (display "\"" port))

(define (write-object alist port)
  "Write ALIST to PORT in JSON object format."
  ;; Keys may be strings or symbols.
  (define key->string
    (match-lambda
     ((? string? key) key)
     ((? symbol? key) (symbol->string key))))

  (define (write-pair pair)
    (match pair
      ((key . value)
       (write-string (key->string key) port)
       (display ":" port)
       (write-json value port))))

  (display "{" port)
  (match alist
    (() #f)
    ((front ... end)
     (for-each (lambda (pair)
                 (write-pair pair)
                 (display "," port))
          front)
     (write-pair end)))
  (display "}" port))

(define (write-array lst port)
  "Write LST to PORT in JSON array format."
  (display "[" port)
  (match lst
    (() #f)
    ((front ... end)
     (for-each (lambda (val)
                 (write-json val port)
                 (display "," port))
               front)
     (write-json end port)))
  (display "]" port))

(define (write-json exp port)
  "Write EXP to PORT in JSON format."
  (match exp
    (#t (display "true" port))
    (#f (display "false" port))
    ;; Differentiate #nil from '().
    ((and (? boolean? ) #nil) (display "null" port))
    ((? string? s) (write-string s port))
    ((? real? n) (display n port))
    (('@ . alist) (write-object alist port))
    ((vals ...) (write-array vals port))))
