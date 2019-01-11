;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2019 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix profiling)
  #:use-module (ice-9 match)
  #:autoload   (ice-9 format) (format)
  #:export (profiled?
            register-profiling-hook!))

;;; Commentary:
;;;
;;; Basic support for Guix-specific profiling.
;;;
;;; Code:

(define profiled?
  (let ((profiled
         (or (and=> (getenv "GUIX_PROFILING") string-tokenize)
             '())))
    (lambda (component)
      "Return true if COMPONENT profiling is active."
      (member component profiled))))

(define %profiling-hooks
  ;; List of profiling hooks.
  (map (match-lambda
         ("after-gc"       after-gc-hook)
         ((or "exit" #f)   exit-hook))
       (or (and=> (getenv "GUIX_PROFILING_EVENTS") string-tokenize)
           '("exit"))))

(define (register-profiling-hook! component thunk)
  "Register THUNK as a profiling hook for COMPONENT, a string such as
\"rpc\"."
  (when (profiled? component)
    (for-each (lambda (hook)
                (add-hook! hook thunk))
              %profiling-hooks)))

(define (show-gc-stats)
  "Display garbage collection statistics."
  (define MiB (* 1024 1024.))
  (define stats (gc-stats))

  (format (current-error-port) "Garbage collection statistics:
  heap size:        ~,2f MiB
  allocated:        ~,2f MiB
  GC times:         ~a
  time spent in GC: ~,2f seconds (~d% of user time)~%"
          (/ (assq-ref stats 'heap-size) MiB)
          (/ (assq-ref stats 'heap-total-allocated) MiB)
          (assq-ref stats 'gc-times)
          (/ (assq-ref stats 'gc-time-taken)
             internal-time-units-per-second 1.)
          (inexact->exact
           (round (* (/ (assq-ref stats 'gc-time-taken)
                        (tms:utime (times)) 1.)
                     100)))))

(register-profiling-hook! "gc" show-gc-stats)
