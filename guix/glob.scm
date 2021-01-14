;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Giacomo Leidi <goodoldpaul@autistici.org>
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

(define-module (guix glob)
  #:use-module (ice-9 match)
  #:export (string->sglob
            compile-sglob
            string->compiled-sglob
            glob-match?))

;;; Commentary:
;;;
;;; This is a minimal implementation of "glob patterns" (info "(libc)
;;; Globbbing").  It is currently limited to simple patterns and does not
;;; support braces, for instance.
;;;
;;; Code:

(define (parse-bracket chars)
  "Parse CHARS, a list of characters that extracted from a '[...]' sequence."
  (match chars
    ((start #\- end)
     `(range ,start ,end))
    (lst
     `(set ,@lst))))

(define (string->sglob str)
  "Return an sexp, called an \"sglob\", that represents the compiled form of
STR, a glob pattern such as \"foo*\" or \"foo??bar\"."
  (define flatten
    (match-lambda
      (((? string? str)) str)
      (x x)))

  (define (cons-string chars lst)
    (match chars
      (() lst)
      (_ (cons (list->string (reverse chars)) lst))))

  (let loop ((chars   (string->list str))
             (pending '())
             (brackets 0)
             (result '()))
    (match chars
      (()
       (flatten (reverse (if (null? pending)
                             result
                             (cons-string pending result)))))
      ((#\* #\* #\/ . rest)
       (if (zero? brackets)
           (loop rest '() 0
                 (cons* '**/ (cons-string pending result)))
           (loop rest (cons '**/ pending) brackets result)))
      (((and chr (or #\? #\*)) . rest)
       (let ((wildcard (match chr
                         (#\? '?)
                         (#\* '*))))
         (if (zero? brackets)
             (loop rest '() 0
                   (cons* wildcard (cons-string pending result)))
             (loop rest (cons chr pending) brackets result))))
      ((#\[ . rest)
       (if (zero? brackets)
           (loop rest '() (+ 1 brackets)
                 (cons-string pending result))
           (loop rest (cons #\[ pending) (+ 1 brackets) result)))
      ((#\] . rest)
       (cond ((zero? brackets)
              (error "unexpected closing bracket" str))
             ((= 1 brackets)
              (loop rest '() 0
                    (cons (parse-bracket (reverse pending)) result)))
             (else
              (loop rest (cons #\] pending) (- brackets 1) result))))
      ((chr . rest)
       (loop rest (cons chr pending) brackets result)))))

(define (compile-sglob sglob)
  "Compile SGLOB into a more efficient representation."
  (if (string? sglob)
      sglob
      (let loop ((sglob sglob)
                 (result '()))
        (match sglob
          (()
           (reverse result))
          (('? . rest)
           (loop rest (cons char-set:full result)))
          ((('range start end) . rest)
           (loop rest (cons (ucs-range->char-set
                             (char->integer start)
                             (+ 1 (char->integer end)))
                            result)))
          ((('set . chars) . rest)
           (loop rest (cons (list->char-set chars) result)))
          ((head . rest)
           (loop rest (cons head result)))))))

(define string->compiled-sglob
  (compose compile-sglob string->sglob))

(define (glob-match? pattern str)
  "Return true if STR matches PATTERN, a compiled glob pattern as returned by
'compile-sglob'."
  (let loop ((pattern pattern)
             (str str))
   (match pattern
     ((? string? literal)
      (string=? literal str))
     (()
      (string-null? str))
     (('*)
      #t)
     (('**/)
      #t)
     (('**/ suffix . rest)
      (let ((rest (if (eq? '* suffix) (cdr rest) rest))
            (suffix (if (eq? '* suffix) (car rest) suffix)))
        (match (string-contains str suffix)
          (#f    #f)
          (index (loop rest (string-drop str
                                         (+ index (string-length suffix))))))))
     (('* suffix . rest)
      (match (string-contains str suffix)
        (#f    #f)
        (index (loop rest
                     (string-drop str
                                  (+ index (string-length suffix)))))))
     (((? char-set? cs) . rest)
      (and (>= (string-length str) 1)
           (let ((chr (string-ref str 0)))
             (and (char-set-contains? cs chr)
                  (loop rest (string-drop str 1))))))
     ((prefix . rest)
      (and (string-prefix? prefix str)
           (loop rest (string-drop str (string-length prefix))))))))
