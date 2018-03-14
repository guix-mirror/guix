;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
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
  #:export (compile-glob-pattern
            glob-match?))

;;; Commentary:
;;;
;;; This is a minimal implementation of "glob patterns" (info "(libc)
;;; Globbbing").  It is currently limited to simple patterns and does not
;;; support braces and square brackets, for instance.
;;;
;;; Code:

(define (wildcard-indices str)
  "Return the list of indices in STR where wildcards can be found."
  (let loop ((index 0)
             (result '()))
    (if (= index (string-length str))
        (reverse result)
        (loop (+ 1 index)
              (case (string-ref str index)
                ((#\? #\*) (cons index result))
                (else      result))))))

(define (compile-glob-pattern str)
  "Return an sexp that represents the compiled form of STR, a glob pattern
such as \"foo*\" or \"foo??bar\"."
  (define flatten
    (match-lambda
      (((? string? str)) str)
      (x x)))

  (let loop ((index   0)
             (indices (wildcard-indices str))
             (result '()))
    (match indices
      (()
       (flatten (cond ((zero? index)
                       (list str))
                      ((= index (string-length str))
                       (reverse result))
                      (else
                       (reverse (cons (string-drop str index)
                                      result))))))
      ((wildcard-index . rest)
       (let ((wildcard (match (string-ref str wildcard-index)
                         (#\? '?)
                         (#\* '*))))
         (match (substring str index wildcard-index)
           (""  (loop (+ 1 wildcard-index)
                      rest
                      (cons wildcard result)))
           (str (loop (+ 1 wildcard-index)
                      rest
                      (cons* wildcard str result)))))))))

(define (glob-match? pattern str)
  "Return true if STR matches PATTERN, a compiled glob pattern as returned by
'compile-glob-pattern'."
  (let loop ((pattern pattern)
             (str str))
   (match pattern
     ((? string? literal) (string=? literal str))
     (((? string? one))   (string=? one str))
     (('*)  #t)
     (('?) (= 1 (string-length str)))
     (()    #t)
     (('* suffix . rest)
      (match (string-contains str suffix)
        (#f    #f)
        (index (loop rest
                     (string-drop str
                                  (+ index (string-length suffix)))))))
     (('? . rest)
      (and (>= (string-length str) 1)
           (loop rest (string-drop str 1))))
     ((prefix . rest)
      (and (string-prefix? prefix str)
           (loop rest (string-drop str (string-length prefix))))))))
