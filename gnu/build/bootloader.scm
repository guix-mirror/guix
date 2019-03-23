;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu build bootloader)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 format)
  #:export (write-file-on-device
            invoke/quiet))


;;;
;;; Writing utils.
;;;

(define (write-file-on-device file size device offset)
  "Write SIZE bytes from FILE to DEVICE starting at OFFSET."
  (call-with-input-file file
    (lambda (input)
      (let ((bv (get-bytevector-n input size)))
        (call-with-output-file device
          (lambda (output)
            (seek output offset SEEK_SET)
            (put-bytevector output bv))
          #:binary #t)))))

(define-syntax-rule (G_ str) str)                 ;for xgettext

(define (open-pipe-with-stderr program . args)
  "Run PROGRAM with ARGS in an input pipe, but, unlike 'open-pipe*', redirect
both its standard output and standard error to the pipe.  Return two value:
the pipe to read PROGRAM's data from, and the PID of the child process running
PROGRAM."
  ;; 'open-pipe*' doesn't attempt to capture stderr in any way, which is why
  ;; we need to roll our own.
  (match (pipe)
    ((input .  output)
     (match (primitive-fork)
       (0
        (dynamic-wind
          (const #t)
          (lambda ()
            (close-port input)
            (dup2 (fileno output) 1)
            (dup2 (fileno output) 2)
            (apply execlp program program args))
          (lambda ()
            (primitive-exit 127))))
       (pid
        (close-port output)
        (values input pid))))))

;; TODO: Move to (guix build utils) on the next rebuild cycle.
(define (invoke/quiet program . args)
  "Invoke PROGRAM with ARGS and capture PROGRAM's standard output and standard
error.  If PROGRAM succeeds, print nothing and return the unspecified value;
otherwise, raise a '&message' error condition that includes the status code
and the output of PROGRAM."
  (define-values (pipe pid)
    (apply open-pipe-with-stderr program args))

  (let loop ((lines '()))
    (match (read-line pipe)
      ((? eof-object?)
       (close-port pipe)
       (match (waitpid pid)
         ((_ . status)
          (unless (zero? status)
            (raise (condition
                    (&message
                     (message (format #f (G_ "'~a~{ ~a~}' exited with status ~a; \
output follows:~%~%~{  ~a~%~}")
                                      program args
                                      (or (status:exit-val status)
                                          status)
                                      (reverse lines))))))))))
      (line
       (loop (cons line lines))))))
