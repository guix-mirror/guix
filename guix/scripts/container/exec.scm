;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
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

(define-module (guix scripts container exec)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-37)
  #:use-module (guix scripts)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (gnu build linux-container)
  #:export (guix-container-exec))

(define %options
  (list (option '(#\h "help") #f #f
                (lambda args
                  (show-help)
                  (exit 0)))
        (option '(#\V "version") #f #f
                (lambda args
                  (show-version-and-exit "guix container exec")))))

(define (show-help)
  (display (G_ "Usage: guix container exec PID COMMAND [ARGS...]
Execute COMMAND within the container process PID.\n"))
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define (partition-args args)
  "Split ARGS into two lists; one containing the arguments for this program,
and the other containing arguments for the command to be executed."
  (define (number-string? str)
    (false-if-exception (string->number str)))

  (let loop ((a '())
             (b args))
    (match b
      (()
       (values (reverse a) '()))
      (((? number-string? head) . tail)
       (values (reverse (cons head a)) tail))
      ((head . tail)
       (loop (cons head a) tail)))))

(define (guix-container-exec . args)
  (define (handle-argument arg result)
    (if (assoc-ref result 'pid)
        (leave (G_ "~a: extraneous argument~%") arg)
        (alist-cons 'pid (string->number* arg) result)))

  (with-error-handling
    (let-values (((args command) (partition-args args)))
      (let* ((opts (parse-command-line args %options '(())
                                       #:argument-handler
                                       handle-argument))
             (pid  (assoc-ref opts 'pid))
             (environment (filter-map (lambda (name)
                                        (let ((value (getenv name)))
                                          (and value (cons name value))))
                                      ;; Pass through the TERM environment
                                      ;; variable to inform processes about
                                      ;; the capabilities of the terminal.
                                      '("TERM"))))

        (unless pid
          (leave (G_ "no pid specified~%")))

        (when (null? command)
          (leave (G_ "no command specified~%")))

        (unless (file-exists? (string-append "/proc/" (number->string pid)))
          (leave (G_ "no such process ~d~%") pid))

        (let ((result (container-excursion pid
                        (lambda ()
                          (match command
                            ((program . program-args)
                             (for-each (match-lambda
                                         ((name . value)
                                          (setenv name value)))
                                       environment)
                             (apply execlp program program program-args)))))))
          (unless (zero? result)
            (leave (G_ "exec failed with status ~d~%") result)))))))
