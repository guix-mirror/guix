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

(define-module (guix workers)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 match)
  #:use-module (ice-9 q)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:export (pool?
            make-pool
            pool-enqueue!
            pool-idle?
            eventually))

;;; Commentary:
;;;
;;; This module implements "worker pools".  Worker pools are the low-level
;;; mechanism that's behind futures: there's a fixed set of threads
;;; ("workers") that one can submit work to, and one of them will eventually
;;; pick the submitted tasks.
;;;
;;; Unlike futures, these worker pools are meant to be used for tasks that
;;; have a side-effect.  Thus, we never "touch" a task that was submitted like
;;; we "touch" a future.  Instead, we simply assume that the task will
;;; eventually complete.
;;;
;;; Code:

(define-record-type <pool>
  (%make-pool queue mutex condvar workers)
  pool?
  (queue    pool-queue)
  (mutex    pool-mutex)
  (condvar  pool-condition-variable)
  (workers  pool-workers))

(define-syntax-rule (without-mutex mutex exp ...)
  (dynamic-wind
    (lambda ()
      (unlock-mutex mutex))
    (lambda ()
      exp ...)
    (lambda ()
      (lock-mutex mutex))))

(define (worker-thunk mutex condvar pop-queue)
  "Return the thunk executed by worker threads."
  (define (loop)
    (match (pop-queue)
      (#f                                         ;empty queue
       (wait-condition-variable condvar mutex))
      ((? procedure? proc)
       ;; Release MUTEX while executing PROC.
       (without-mutex mutex
         (catch #t proc
           (lambda (key . args)
             ;; XXX: In Guile 2.0 ports are not thread-safe, so this could
             ;; crash (Guile 2.2 is fine).
             (display-backtrace (make-stack #t) (current-error-port))
             (print-exception (current-error-port)
                              (stack-ref (make-stack #t) 0)
                              key args))))))
    (loop))

  (lambda ()
    (with-mutex mutex
      (loop))))

(define* (make-pool #:optional (count (current-processor-count)))
  "Return a pool of COUNT workers."
  (let* ((mutex   (make-mutex))
         (condvar (make-condition-variable))
         (queue   (make-q))
         (procs   (unfold (cut >= <> count)
                          (lambda (n)
                            (worker-thunk mutex condvar
                                          (lambda ()
                                            (and (not (q-empty? queue))
                                                 (q-pop! queue)))))
                          1+
                          0))
         (threads (map (lambda (proc)
                         (call-with-new-thread proc))
                       procs)))
    (%make-pool queue mutex condvar threads)))

(define (pool-enqueue! pool thunk)
  "Enqueue THUNK for future execution by POOL."
  (with-mutex (pool-mutex pool)
    (enq! (pool-queue pool) thunk)
    (signal-condition-variable (pool-condition-variable pool))))

(define (pool-idle? pool)
  "Return true if POOL doesn't have any task in its queue."
  (with-mutex (pool-mutex pool)
    (q-empty? (pool-queue pool))))

(define-syntax-rule (eventually pool exp ...)
  "Run EXP eventually on one of the workers of POOL."
  (pool-enqueue! pool (lambda () exp ...)))

;;; Local Variables:
;;; eval: (put 'without-mutex 'scheme-indent-function 1)
;;; End:

;;; workers.scm ends here
