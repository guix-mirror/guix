;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu tests)
  #:use-module (guix gexp)
  #:use-module (gnu system)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:export (backdoor-service-type
            marionette-operating-system))

;;; Commentary:
;;;
;;; This module provides the infrastructure to run operating system tests.
;;; The most important part of that is tools to instrument the OS under test,
;;; essentially allowing to run in a virtual machine controlled by the host
;;; system--hence the name "marionette".
;;;
;;; Code:

(define (marionette-shepherd-service imported-modules)
  "Return the Shepherd service for the marionette REPL"
  (define device
    "/dev/hvc0")

  (list (shepherd-service
         (provision '(marionette))
         (requirement '(udev))                    ;so that DEVICE is available
         (modules '((ice-9 match)
                    (srfi srfi-9 gnu)
                    (guix build syscalls)
                    (rnrs bytevectors)))
         (imported-modules `((guix build syscalls)
                             ,@imported-modules))
         (start
          #~(lambda ()
              (define (clear-echo termios)
                (set-field termios (termios-local-flags)
                           (logand (lognot (local-flags ECHO))
                                   (termios-local-flags termios))))

              (define (self-quoting? x)
                (letrec-syntax ((one-of (syntax-rules ()
                                          ((_) #f)
                                          ((_ pred rest ...)
                                           (or (pred x)
                                               (one-of rest ...))))))
                  (one-of symbol? string? pair? null? vector?
                          bytevector? number? boolean?)))

              (match (primitive-fork)
                (0
                 (dynamic-wind
                   (const #t)
                   (lambda ()
                     (let* ((repl    (open-file #$device "r+0"))
                            (termios (tcgetattr (fileno repl)))
                            (console (open-file "/dev/console" "r+0")))
                       ;; Don't echo input back.
                       (tcsetattr (fileno repl) (tcsetattr-action TCSANOW)
                                  (clear-echo termios))

                       ;; Redirect output to the console.
                       (close-fdes 1)
                       (close-fdes 2)
                       (dup2 (fileno console) 1)
                       (dup2 (fileno console) 2)
                       (close-port console)

                       (display 'ready repl)
                       (let loop ()
                         (newline repl)

                         (match (read repl)
                           ((? eof-object?)
                            (primitive-exit 0))
                           (expr
                            (catch #t
                              (lambda ()
                                (let ((result (primitive-eval expr)))
                                  (write (if (self-quoting? result)
                                             result
                                             (object->string result))
                                         repl)))
                              (lambda (key . args)
                                (print-exception (current-error-port)
                                                 (stack-ref (make-stack #t) 1)
                                                 key args)
                                (write #f repl)))))
                         (loop))))
                   (lambda ()
                     (primitive-exit 1))))
                (pid
                 pid))))
         (stop #~(make-kill-destructor)))))

(define marionette-service-type
  ;; This is the type of the "marionette" service, allowing a guest system to
  ;; be manipulated from the host.  This marionette REPL is essentially a
  ;; universal marionette.
  (service-type (name 'marionette-repl)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          marionette-shepherd-service)))))

(define* (marionette-operating-system os
                                      #:key (imported-modules '()))
  "Return a marionetteed variant of OS such that OS can be used as a marionette
in a virtual machine--i.e., controlled from the host system."
  (operating-system
    (inherit os)
    (services (cons (service marionette-service-type imported-modules)
                    (operating-system-user-services os)))))

;;; tests.scm ends here
