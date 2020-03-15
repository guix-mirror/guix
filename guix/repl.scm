;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
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
            number? boolean? char?)))

(define repl-prompt
  ;; Current REPL prompt or #f.
  (make-parameter #f))

(define (stack->frames stack)
  "Return STACK's frames as a list."
  (unfold (cute >= <> (stack-length stack))
          (cut stack-ref stack <>)
          1+
          0))

(define* (send-repl-response exp output
                             #:key (version '(0 0)))
  "Write the response corresponding to the evaluation of EXP to PORT, an
output port.  VERSION is the client's protocol version we are targeting."
  (define (value->sexp value)
    (if (self-quoting? value)
        `(value ,value)
        `(non-self-quoting ,(object-address value)
                           ,(object->string value))))

  (define (frame->sexp frame)
    `(,(frame-procedure-name frame)
      ,(match (frame-source frame)
         ((_ (? string? file) (? integer? line) . (? integer? column))
          (list file line column))
         (_
          '(#f #f #f)))))

  (define (handle-exception key . args)
    (define reply
      (match version
        ((0 1 (? positive?) _ ...)
         ;; Protocol (0 1 1) and later.
         (let ((stack (if (repl-prompt)
                          (make-stack #t handle-exception (repl-prompt))
                          (make-stack #t))))
           `(exception (arguments ,key ,@(map value->sexp args))
                       (stack ,@(map frame->sexp (stack->frames stack))))))
        (_
         ;; Protocol (0 0).
         `(exception ,key ,@(map value->sexp args)))))

    (write reply output)
    (newline output)
    (force-output output))

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
    (const #t)
    handle-exception))

(define* (machine-repl #:optional
                       (input (current-input-port))
                       (output (current-output-port)))
  "Run a machine-usable REPL over ports INPUT and OUTPUT.

The protocol of this REPL is meant to be machine-readable and provides proper
support to represent multiple-value returns, exceptions, objects that lack a
read syntax, and so on.  As such it is more convenient and robust than parsing
Guile's REPL prompt."
  (define tag
    (make-prompt-tag "repl-prompt"))

  (define (loop exp version)
    (match exp
      ((? eof-object?) #t)
      (exp
       (send-repl-response exp output
                           #:version version)
       (loop (read input) version))))

  (write `(repl-version 0 1 1) output)
  (newline output)
  (force-output output)

  ;; In protocol version (0 0), clients would not send their supported
  ;; protocol version.  Thus, the code below checks for two case: (1) a (0 0)
  ;; client that directly sends an expression to evaluate, and (2) a more
  ;; recent client that sends (() repl-version ...).  This form is chosen to
  ;; be unambiguously distinguishable from a regular Scheme expression.

  (call-with-prompt tag
    (lambda ()
      (parameterize ((repl-prompt tag))
        (match (read input)
          ((() 'repl-version version ...)
           (loop (read input) version))
          (exp
           (loop exp '(0 0))))))
    (const #f)))
