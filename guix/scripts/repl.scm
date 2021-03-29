;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020, 2021 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2020 Konrad Hinsen <konrad.hinsen@fastmail.net>
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
  #:use-module (guix repl)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:autoload   (guix describe) (current-profile)
  #:autoload   (system repl repl) (start-repl)
  #:autoload   (system repl server)
                  (make-tcp-server-socket make-unix-domain-server-socket)
  #:export (guix-repl))

;;; Commentary:
;;;
;;; This command provides a Guile script runner and REPL in an environment
;;; that contains all the modules comprising Guix.

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
                  (alist-cons 'listen arg result)))
        (option '(#\q) #f #f
                (lambda (opt name arg result)
                  (alist-cons 'ignore-dot-guile? #t result)))
        (option '(#\L "load-path") #t #f
                (lambda (opt name arg result)
                  ;; XXX: Imperatively modify the search paths.
                  (set! %load-path (cons arg %load-path))
                  (set! %load-compiled-path (cons arg %load-compiled-path))
                  result))))


(define (show-help)
  (display (G_ "Usage: guix repl [OPTIONS...] [-- FILE ARGS...]
In the Guix execution environment, run FILE as a Guile script with
command-line arguments ARGS.  If no FILE is given, start a Guile REPL.\n"))
  (display (G_ "
  -t, --type=TYPE        start a REPL of the given TYPE"))
  (display (G_ "
      --listen=ENDPOINT  listen to ENDPOINT instead of standard input"))
  (display (G_ "
  -q                     inhibit loading of ~/.guile"))
  (newline)
  (display (G_ "
  -L, --load-path=DIR    prepend DIR to the package module search path"))
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define user-module
  ;; Module where we execute user code.
  (let ((module (resolve-module '(guix-user) #f #f #:ensure #t)))
    (beautify-user-module! module)
    module))

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


(define-command (guix-repl . args)
  (category plumbing)
  (synopsis "read-eval-print loop (REPL) for interactive programming")

  (define opts
    (parse-command-line args %options (list %default-options)
                #:build-options? #f
                #:argument-handler
                (lambda (arg result)
                  (append `((script . ,arg)
                            (ignore-dot-guile? . #t))
                          result))))

  (define user-config
    (and=> (getenv "HOME")
           (lambda (home)
             (string-append home "/.guile"))))

  (define (set-user-module)
    (set-current-module user-module)
    (when (and (not (assoc-ref opts 'ignore-dot-guile?))
               user-config
               (file-exists? user-config))
      (load user-config)))

  (define script
    (reverse
     (filter-map (match-lambda
                   (('script . script) script)
                   (_ #f))
                 opts)))

  (with-error-handling

    (unless (null? script)
      ;; Run script
      (save-module-excursion
       (lambda ()
         ;; Invoke 'current-profile' so that it memoizes the correct value
         ;; based on (program-arguments), before we call
         ;; 'set-program-arguments'.  This in turn ensures that
         ;; (%package-module-path) will contain entries for the channels
         ;; available in the current profile.
         (current-profile)

         (set-program-arguments script)
         (set-user-module)

         ;; When passed a relative file name, 'load-in-vicinity' searches the
         ;; file in %LOAD-PATH.  Thus, pass (getcwd) instead of ".".
         (load-in-vicinity (getcwd) (car script)))))

    (when (null? script)
      ;; Start REPL
      (let ((type (assoc-ref opts 'type)))
        (call-with-connection (assoc-ref opts 'listen)
          (lambda ()
            (case type
              ((guile)
               (save-module-excursion
                (lambda ()
                  (set-user-module)
                  ;; Do not exit repl on SIGINT.
                  ((@@ (ice-9 top-repl) call-with-sigint)
                   (lambda ()
                     (start-repl))))))
              ((machine)
               (machine-repl))
              (else
               (leave (G_ "~a: unknown type of REPL~%") type)))))))))

;; Local Variables:
;; eval: (put 'call-with-connection 'scheme-indent-function 1)
;; End:
