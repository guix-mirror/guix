;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix memoization)
  #:export (memoize
            mlambda
            mlambdaq))

(define-syntax-rule (call/mv thunk)
  (call-with-values thunk list))
(define-syntax-rule (return/mv lst)
  (apply values lst))

(define-syntax-rule (call/1 thunk)
  (thunk))
(define-syntax-rule (return/1 value)
  value)

(define-syntax define-cache-procedure
  (syntax-rules ()
    "Define a procedure NAME that implements a cache using HASH-REF and
HASH-SET!.  Use CALL to invoke the thunk and RETURN to return its value; CALL
and RETURN are used to distinguish between multiple-value and single-value
returns."
    ((_ name hash-ref hash-set! call return)
     (define name
       (let ((%nothing '(this is nothing)))
         (lambda (cache key thunk)
           "Cache the result of THUNK under KEY in CACHE, or return the
already-cached result."
           (let ((results (hash-ref cache key %nothing)))
             (if (eq? results %nothing)
                 (let ((results (call thunk)))
                   (hash-set! cache key results)
                   (return results))
                 (return results)))))))
    ((_ name hash-ref hash-set!)
     (define-cache-procedure name hash-ref hash-set!
       call/mv return/mv))))

(define-cache-procedure cached/mv  hash-ref hash-set!)
(define-cache-procedure cachedq/mv hashq-ref hashq-set!)
(define-cache-procedure cached  hash-ref hash-set! call/1 return/1)
(define-cache-procedure cachedq hashq-ref hashq-set! call/1 return/1)

(define (memoize proc)
  "Return a memoizing version of PROC.

This is a generic version of 'mlambda' what works regardless of the arity of
'proc'.  It is more expensive since the argument list is always allocated, and
the result is returned via (apply values results)."
  (let ((cache (make-hash-table)))
    (lambda args
      (cached/mv cache args
                 (lambda ()
                   (apply proc args))))))

(define-syntax %mlambda
  (syntax-rules ()
    "Return a memoizing lambda.  This is restricted to procedures that return
exactly one value."
    ((_ cached () body ...)
     ;; The zero-argument case is equivalent to a promise.
     (let ((result #f) (cached? #f))
       (lambda ()
         (unless cached?
           (set! result (begin body ...))
           (set! cached? #t))
         result)))

    ;; Optimize the fixed-arity case such that there's no argument list
    ;; allocated.  XXX: We can't really avoid the closure allocation since
    ;; Guile 2.0's compiler will always keep it.
    ((_ cached (arg) body ...)                    ;one argument
     (let ((cache (make-hash-table))
           (proc  (lambda (arg) body ...)))
       (lambda (arg)
         (cached cache arg (lambda () (proc arg))))))
    ((_ _ (args ...) body ...)                    ;two or more arguments
     (let ((cache (make-hash-table))
           (proc  (lambda (args ...) body ...)))
       (lambda (args ...)
         ;; XXX: Always use 'cached', which uses 'equal?', to compare the
         ;; argument lists.
         (cached cache (list args ...)
                 (lambda ()
                   (proc args ...))))))))

(define-syntax-rule (mlambda formals body ...)
  "Define a memoizing lambda.  The lambda's arguments are compared with
'equal?', and BODY is expected to yield a single return value."
  (%mlambda cached formals body ...))

(define-syntax-rule (mlambdaq formals body ...)
  "Define a memoizing lambda.  If FORMALS lists a single argument, it is
compared using 'eq?'; otherwise, the argument list is compared using 'equal?'.
BODY is expected to yield a single return value."
  (%mlambda cachedq formals body ...))
