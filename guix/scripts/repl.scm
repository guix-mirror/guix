;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix scripts repl)
  #:use-module (guix ui)
  #:use-module (guix scripts)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:autoload   (system repl repl) (start-repl)
  #:autoload   (system repl server)
                  (make-tcp-server-socket make-unix-domain-server-socket)
  #:export (machine-repl
            guix-repl))

;;; Commentary:
;;;
;;; This command provides a Guile REPL

(define %default-options
  `((type . guile)))

(define %options
  (list (option '(#\h "help") #f #f
                (lambda args
                  (show-help)
                  (exit 0)))
        (option '(#\V "version") #f #f
                (lambda args
                  (show-version-and-exit "guix repl")))
        (option '(#\t "type") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'type (string->symbol arg) result)))
        (option '("listen") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'listen arg result)))))


(define (show-help)
  (display (G_ "Usage: guix repl [OPTIONS...]
Start a Guile REPL in the Guix execution environment.\n"))
  (display (G_ "
  -t, --type=TYPE        start a REPL of the given TYPE"))
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define (self-quoting? x)
  "Return #t if X is self-quoting."
  (letrec-syntax ((one-of (syntax-rules ()
                            ((_) #f)
                            ((_ pred rest ...)
                             (or (pred x)
                                 (one-of rest ...))))))
    (one-of symbol? string? pair? null? vector?
            bytevector? number? boolean?)))

(define user-module
  ;; Module where we execute user code.
  (let ((module (resolve-module '(guix-user) #f #f #:ensure #t)))
    (beautify-user-module! module)
    module))

(define* (machine-repl #:optional
                       (input (current-input-port))
                       (output (current-output-port)))
  "Run a machine-usable REPL over ports INPUT and OUTPUT.

The protocol of this REPL is meant to be machine-readable and provides proper
support to represent multiple-value returns, exceptions, objects that lack a
read syntax, and so on.  As such it is more convenient and robust than parsing
Guile's REPL prompt."
  (define (value->sexp value)
    (if (self-quoting? value)
        `(value ,value)
        `(non-self-quoting ,(object-address value)
                           ,(object->string value))))

  (write `(repl-version 0 0) output)
  (newline output)
  (force-output output)

  (let loop ()
    (match (read input)
      ((? eof-object?) #t)
      (exp
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
           (force-output output)))
       (loop)))))

(define (call-with-connection spec thunk)
  "Dynamically-bind the current input and output ports according to SPEC and
call THUNK."
  (if (not spec)
      (thunk)

      ;; Note: the "PROTO:" prefix in SPEC is here so that we can eventually
      ;; parse things like "fd:123" in a non-ambiguous way.
      (match (string-index spec #\:)
        (#f
         (leave (G_ "~A: invalid listen specification~%") spec))
        (index
         (let ((protocol (string-take spec index))
               (address  (string-drop spec (+ index 1))))
           (define socket
             (match protocol
               ("tcp"
                (make-tcp-server-socket #:port (string->number address)))
               ("unix"
                (make-unix-domain-server-socket #:path address))
               (_
                (leave (G_ "~A: unsupported protocol family~%")
                       protocol))))

           (listen socket 10)
           (let loop ()
             (match (accept socket)
               ((connection . address)
                (if (= AF_UNIX (sockaddr:fam address))
                    (info (G_ "accepted connection~%"))
                    (info (G_ "accepted connection from ~a~%")
                          (inet-ntop (sockaddr:fam address)
                                     (sockaddr:addr address))))
                (dynamic-wind
                  (const #t)
                  (lambda ()
                    (parameterize ((current-input-port connection)
                                   (current-output-port connection))
                      (thunk)))
                  (lambda ()
                    (false-if-exception (close-port connection))
                    (info (G_ "connection closed~%"))))))
             (loop)))))))


(define (guix-repl . args)
  (define opts
    ;; Return the list of package names.
    (args-fold* args %options
                (lambda (opt name arg result)
                  (leave (G_ "~A: unrecognized option~%") name))
                (lambda (arg result)
                  (leave (G_ "~A: extraneous argument~%") arg))
                %default-options))

  (with-error-handling
    (let ((type (assoc-ref opts 'type)))
      (call-with-connection (assoc-ref opts 'listen)
        (lambda ()
          (case type
            ((guile)
             (save-module-excursion
              (lambda ()
                (set-current-module user-module)
                ;; Do not exit repl on SIGINT.
                ((@@ (ice-9 top-repl) call-with-sigint)
                 (lambda ()
                   (start-repl))))))
            ((machine)
             (machine-repl))
            (else
             (leave (G_ "~a: unknown type of REPL~%") type))))))))

;; Local Variables:
;; eval: (put 'call-with-connection 'scheme-indent-function 1)
;; End:
