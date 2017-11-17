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

(define-module (test-workers)
  #:use-module (guix workers)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-64))

(test-begin "workers")

(test-equal "enqueue"
  4242
  (let* ((pool   (make-pool))
         (result 0)
         (1+!    (let ((lock (make-mutex)))
                   (lambda ()
                     (with-mutex lock
                       (set! result (+ result 1)))))))
    (let loop ((i 4242))
      (unless (zero? i)
        (pool-enqueue! pool 1+!)
        (loop (- i 1))))
    (let poll ()
      (unless (pool-idle? pool)
        (pk 'busy result)
        (sleep 1)
        (poll)))
    result))

;; Same as above, but throw exceptions within the workers and make sure they
;; remain alive.
(test-equal "exceptions"
  4242
  (let* ((pool   (make-pool 10))
         (result 0)
         (1+!    (let ((lock (make-mutex)))
                   (lambda ()
                     (with-mutex lock
                       (set! result (+ result 1)))))))
    (let loop ((i 10))
      (unless (zero? i)
        (pool-enqueue! pool (lambda ()
                              (throw 'whatever)))
        (loop (- i 1))))
    (let loop ((i 4242))
      (unless (zero? i)
        (pool-enqueue! pool 1+!)
        (loop (- i 1))))
    (let poll ()
      (unless (pool-idle? pool)
        (pk 'busy result)
        (sleep 1)
        (poll)))
    result))

(test-end)
