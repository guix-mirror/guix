;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2019 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix repl)
  #:use-module (ice-9 match)
  #:export (send-repl-response
            machine-repl))

;;; Commentary:
;;;
;;; This module implements the "machine-readable" REPL provided by
;;; 'guix repl -t machine'.  It's a lightweight module meant to be
;;; embedded in any Guile process providing REPL functionality.
;;;
;;; Code:

(define (self-quoting? x)
  "Return #t if X is self-quoting."
  (letrec-syntax ((one-of (syntax-rules ()
                            ((_) #f)
                            ((_ pred rest ...)
                             (or (pred x)
                                 (one-of rest ...))))))
    (one-of symbol? string? keyword? pair? null? array?
            number? boolean?)))

(define (send-repl-response exp output)
  "Write the response corresponding to the evaluation of EXP to PORT, an
output port."
  (define (value->sexp value)
    (if (self-quoting? value)
        `(value ,value)
        `(non-self-quoting ,(object-address value)
                           ,(object->string value))))

  (catch #t
    (lambda ()
      (let ((results (call-with-values
                         (lambda ()
                           (primitive-eval exp))
                       list)))
        (write `(values ,@(map value->sexp results))
               output)
        (newline output)
        (force-output output)))
    (lambda (key . args)
      (write `(exception ,key ,@(map value->sexp args)))
      (newline output)
      (force-output output))))

(define* (machine-repl #:optional
                       (input (current-input-port))
                       (output (current-output-port)))
  "Run a machine-usable REPL over ports INPUT and OUTPUT.

The protocol of this REPL is meant to be machine-readable and provides proper
support to represent multiple-value returns, exceptions, objects that lack a
read syntax, and so on.  As such it is more convenient and robust than parsing
Guile's REPL prompt."
  (write `(repl-version 0 0) output)
  (newline output)
  (force-output output)

  (let loop ()
    (match (read input)
      ((? eof-object?) #t)
      (exp
       (send-repl-response exp output)
       (loop)))))
