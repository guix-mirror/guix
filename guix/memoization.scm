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
  #:use-module (guix profiling)
  #:use-module (ice-9 match)
  #:autoload   (srfi srfi-1) (count)
  #:use-module (srfi srfi-9)
  #:export (invalidate-memoization!
            memoize
            mlambda
            mlambdaq))

;; Data type representation a memoization cache when profiling is on.
(define-record-type <cache>
  (make-cache table lookups hits)
  cache?
  (table   cache-table)
  (lookups cache-lookups set-cache-lookups!)
  (hits    cache-hits    set-cache-hits!))

(define-syntax-rule (define-lookup-procedure proc get)
  "Define a lookup procedure PROC.  When profiling is turned off, PROC is set
to GET; when profiling is on, PROC is a wrapper around GET that keeps tracks
of lookups and cache hits."
  (define proc
    (if (profiled? "memoization")
        (lambda (cache key default)
          (let ((result (get (cache-table cache) key default)))
            (set-cache-lookups! cache (+ 1 (cache-lookups cache)))
            (unless (eq? result default)
              (set-cache-hits! cache (+ 1 (cache-hits cache))))
            result))
        get)))

(define-syntax-rule (define-update-procedure proc put!)
  "Define an update procedure PROC.  When profiling is turned off, PROC is
equal to PUT!; when profiling is on, PROC is a wrapper around PUT and unboxes
the underlying hash table."
  (define proc
    (if (profiled? "memoization")
        (lambda (cache key value)
          (put! (cache-table cache) key value))
        put!)))

(define-lookup-procedure cache-ref hash-ref)
(define-lookup-procedure cacheq-ref hashq-ref)
(define-update-procedure cache-set! hash-set!)
(define-update-procedure cacheq-set! hashq-set!)

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

(define-cache-procedure cached/mv  cache-ref cache-set!)
(define-cache-procedure cachedq/mv cacheq-ref cacheq-set!)
(define-cache-procedure cached  cache-ref cache-set! call/1 return/1)
(define-cache-procedure cachedq cacheq-ref cacheq-set! call/1 return/1)

(define %memoization-tables
  ;; Map procedures to the underlying hash table.
  (make-weak-key-hash-table))

(define %make-hash-table*
  ;; When profiling is off, this is equivalent to 'make-hash-table'.  When
  ;; profiling is on, return a hash table wrapped in a <cache> object.
  (if (profiled? "memoization")
      (lambda (proc location)
        (let ((cache (make-cache (make-hash-table) 0 0)))
          (hashq-set! %memoization-tables proc
                      (cons cache location))
          cache))
      (lambda (proc location)
        (let ((table (make-hash-table)))
          (hashq-set! %memoization-tables proc table)
          table))))

(define-syntax-rule (make-hash-table* proc)
  (%make-hash-table* proc (current-source-location)))

(define (invalidate-memoization! proc)
  "Invalidate the memoization cache of PROC."
  (match (hashq-ref %memoization-tables proc)
    ((? hash-table? table)
     (hash-clear! table))
    (((? cache? cache) . _)
     (hash-clear! (cache-table cache)))))

(define* (show-memoization-tables #:optional (port (current-error-port)))
  "Display to PORT statistics about the memoization tables."
  (define (cache<? p1 p2)
    (match p1
      ((cache1 . _)
       (match p2
         ((cache2 . _)
          (< (hash-count (const #t) (cache-table cache1))
             (hash-count (const #t) (cache-table cache2))))))))

  (define caches
    (hash-map->list (lambda (key value)
                      value)
                    %memoization-tables))

  (match (sort caches (negate cache<?))
    (((caches . locations) ...)
     (format port "Memoization: ~a tables, ~a non-empty~%"
             (length caches)
             (count (lambda (cache)
                      (> (hash-count (const #t) (cache-table cache)) 0))
                    caches))
     (for-each (lambda (cache location)
                 (let ((size (hash-count (const #t) (cache-table cache))))
                   (unless (zero? size)
                     (format port "  ~a:~a:~a: \t~a entries, ~a lookups, ~a% hits~%"
                             (assq-ref location 'filename)
                             (and=> (assq-ref location 'line) 1+)
                             (assq-ref location 'column)
                             size
                             (cache-lookups cache)
                             (inexact->exact
                              (round
                               (* 100. (/ (cache-hits cache)
                                          (cache-lookups cache) 1.))))))))
               caches locations))))

(register-profiling-hook! "memoization" show-memoization-tables)

(define (memoize proc)
  "Return a memoizing version of PROC.

This is a generic version of 'mlambda' what works regardless of the arity of
'proc'.  It is more expensive since the argument list is always allocated, and
the result is returned via (apply values results)."
  (letrec* ((mproc (lambda args
                     (cached/mv cache args
                                (lambda ()
                                  (apply proc args)))))
            (cache (make-hash-table* mproc)))
    mproc))

(define-syntax %mlambda
  (syntax-rules ()
    "Return a memoizing lambda.  This is restricted to procedures that return
exactly one value."
    ((_ cached () body ...)
     ;; The zero-argument case is equivalent to a promise.
     (let ((result #f) (cached? #f)
           (compute (lambda () body ...)))
       (lambda ()
         (unless cached?
           (set! result (compute))
           (set! cached? #t))
         result)))

    ;; Optimize the fixed-arity case such that there's no argument list
    ;; allocated.  XXX: We can't really avoid the closure allocation since
    ;; Guile 2.0's compiler will always keep it.
    ((_ cached (arg) body ...)                    ;one argument
     (letrec* ((proc  (lambda (arg) body ...))
               (mproc (lambda (arg)
                        (cached cache arg (lambda () (proc arg)))))
               (cache (make-hash-table* mproc)))
       mproc))
    ((_ _ (args ...) body ...)                    ;two or more arguments
     (letrec* ((proc  (lambda (args ...) body ...))
               (mproc (lambda (args ...)
                        ;; XXX: Always use 'cached', which uses 'equal?', to
                        ;; compare the argument lists.
                        (cached cache (list args ...)
                                (lambda ()
                                  (proc args ...)))))
               (cache (make-hash-table* mproc)))
       mproc))))

(define-syntax-rule (mlambda formals body ...)
  "Define a memoizing lambda.  The lambda's arguments are compared with
'equal?', and BODY is expected to yield a single return value."
  (%mlambda cached formals body ...))

(define-syntax-rule (mlambdaq formals body ...)
  "Define a memoizing lambda.  If FORMALS lists a single argument, it is
compared using 'eq?'; otherwise, the argument list is compared using 'equal?'.
BODY is expected to yield a single return value."
  (%mlambda cachedq formals body ...))
