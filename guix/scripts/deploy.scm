;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 David Thompson <davet@gnu.org>
;;; Copyright © 2019 Jakob L. Kreuze <zerodaysfordays@sdf.org>
;;; Copyright © 2020-2022 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix scripts deploy)
  #:use-module (gnu machine)
  #:use-module (guix discovery)
  #:use-module (guix scripts)
  #:use-module (guix scripts build)
  #:use-module (guix store)
  #:use-module (guix gexp)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix grafts)
  #:use-module ((guix status) #:select (with-status-verbosity))
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-37)
  #:use-module (srfi srfi-71)
  #:export (guix-deploy))

;;; Commentary:
;;;
;;; This program provides a command-line interface to (gnu machine), allowing
;;; users to perform remote deployments through specification files.
;;;
;;; Code:



(define (show-help)
  (display (G_ "Usage: guix deploy [OPTION] FILE...
Perform the deployment specified by FILE.\n"))
  (show-build-options-help)
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (display (G_ "
  -x, --execute          execute the following command on all the machines"))
  (newline)
  (display (G_ "
  -v, --verbosity=LEVEL  use the given verbosity LEVEL"))
  (show-bug-report-information))

(define %options
  (cons* (option '(#\h "help") #f #f
                 (lambda args
                   (show-help)
                   (exit 0)))
         (option '(#\V "version") #f #f
                 (lambda args
                   (show-version-and-exit "guix deploy")))

         (option '(#\x "execute") #f #f
                 (lambda (opt name arg result)
                   (alist-cons 'execute-command? #t result)))
         (option '(#\s "system") #t #f
                 (lambda (opt name arg result)
                   (alist-cons 'system arg
                               (alist-delete 'system result eq?))))
         (option '(#\v "verbosity") #t #f
                 (lambda (opt name arg result)
                   (let ((level (string->number* arg)))
                     (alist-cons 'verbosity level
                                 (alist-delete 'verbosity result)))))

         %standard-build-options))

(define %default-options
  ;; Alist of default option values.
  `((verbosity . 1)
    (debug . 0)
    (graft? . #t)
    (substitutes? . #t)
    (offload? . #t)
    (print-build-trace? . #t)
    (print-extended-build-trace? . #t)
    (multiplexed-build-output? . #t)))

(define (load-source-file file)
  "Load FILE as a user module."
  (let* ((guix-path (dirname (search-path %load-path "guix.scm")))
         (environment-modules (scheme-modules* guix-path "gnu/machine"))
         (module (make-user-module (append '((gnu) (gnu machine))
                                           environment-modules))))
    (load* file module)))

(define (show-what-to-deploy machines)
  "Show the list of machines to deploy, MACHINES."
  (let ((count (length machines)))
    (format (current-error-port)
            (N_ "The following ~d machine will be deployed:~%"
                "The following ~d machines will be deployed:~%"
                count)
            count)
    (display (indented-string
              (fill-paragraph (string-join (map machine-display-name machines)
                                           ", ")
                              (- (%text-width) 2) 2)
              2)
             (current-error-port))
    (display "\n\n" (current-error-port))))

(define (deploy-machine* store machine)
  "Deploy MACHINE, taking care of error handling."
  (info (G_ "deploying to ~a...~%")
        (machine-display-name machine))

  (guard* (c
           ;; On Guile 3.0, exceptions such as 'unbound-variable' are compound
           ;; and include a '&message'.  However, that message only contains
           ;; the format string.  Thus, special-case it here to avoid
           ;; displaying a bare format string.
           (((exception-predicate &exception-with-kind-and-args) c)
            (raise c))

           ((message-condition? c)
            (leave (G_ "failed to deploy ~a: ~a~%")
                   (machine-display-name machine)
                   (condition-message c)))
           ((formatted-message? c)
            (leave (G_ "failed to deploy ~a: ~a~%")
                   (machine-display-name machine)
                   (apply format #f
                          (gettext (formatted-message-string c)
                                   %gettext-domain)
                          (formatted-message-arguments c))))
           ((deploy-error? c)
            (when (deploy-error-should-roll-back c)
              (info (G_ "rolling back ~a...~%")
                    (machine-display-name machine))
              (run-with-store store (roll-back-machine machine)))
            (apply throw (deploy-error-captured-args c))))
      (run-with-store store (deploy-machine machine))

    (info (G_ "successfully deployed ~a~%")
          (machine-display-name machine))))

(define (invoke-command store machine command)
  "Invoke COMMAND, a list of strings, on MACHINE.  Display its output (if any)
and its error code if it's non-zero.  Return true if COMMAND succeeded, false
otherwise."
  (define invocation
    #~(begin
        (use-modules (ice-9 match)
                     (ice-9 rdelim)
                     (srfi srfi-11))

        (define (spawn . command)
          ;; Spawn COMMAND; return its PID and an input port to read its
          ;; standard output and standard error.
          (match (pipe)
            ((input . output)
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
                       (apply execlp (car command) command))
                     (lambda ()
                       (primitive-exit 127))))
                  (pid
                   (close-port output)
                   (values pid input))))))))

        ;; XXX: 'open-pipe*' is unsuitable here because it does not capture
        ;; stderr, so roll our own.
        (let-values (((pid pipe) (spawn #$@command)))
          (let loop ((lines '()))
            (match (read-line pipe 'concat)
              ((? eof-object?)
               (list (cdr (waitpid pid))
                     (string-concatenate-reverse lines)))
              (line
               (loop (cons line lines))))))))

  (match (run-with-store store
           (machine-remote-eval machine invocation))
    ((code output)
     (match code
       ((? zero?)
        (info (G_ "~a: command succeeded~%")
              (machine-display-name machine)))
       ((= status:exit-val code)
        (report-error (G_ "~a: command exited with code ~a~%")
                      (machine-display-name machine) code))
       ((= status:stop-sig signal)
        (report-error (G_ "~a: command stopped with signal ~a~%")
                      signal))
       ((= status:term-sig signal)
        (report-error (G_ "~a: command terminated with signal ~a~%")
                      signal)))

     (unless (string-null? output)
       (info (G_ "command output on ~a:~%")
             (machine-display-name machine))
       (display output)
       (newline))

     (zero? code))))


(define-command (guix-deploy . args)
  (synopsis "deploy operating systems on a set of machines")
  (define (handle-argument arg result)
    (alist-cons 'file arg result))

  (with-error-handling
    (let* ((args command (break (cut string=? "--" <>) args))
           (opts (parse-command-line args %options (list %default-options)
                                     #:argument-handler handle-argument))
           (file (assq-ref opts 'file))
           (machines (and file (load-source-file file)))
           (execute-command? (assoc-ref opts 'execute-command?)))
      (unless file
        (leave (G_ "missing deployment file argument~%")))

      (when (and (pair? command) (not execute-command?))
        (leave (G_ "'--' was used by '-x' was not specified~%")))

      (with-status-verbosity (assoc-ref opts 'verbosity)
        (with-store store
          (set-build-options-from-command-line store opts)
          (with-build-handler (build-notifier #:use-substitutes?
                                              (assoc-ref opts 'substitutes?)
                                              #:verbosity
                                              (assoc-ref opts 'verbosity))
            (parameterize ((%graft? (assq-ref opts 'graft?)))
              (if execute-command?
                  (match command
                    (("--" command ..1)
                     ;; Exit with zero unless COMMAND failed on one or more
                     ;; machines.
                     (exit
                      (fold (lambda (machine result)
                              (and (invoke-command store machine command)
                                   result))
                            #t
                            machines)))
                    (_
                     (leave (G_ "'-x' specified but no command given~%"))))
                  (begin
                    (show-what-to-deploy machines)
                    (map/accumulate-builds store
                                           (cut deploy-machine* store <>)
                                           machines))))))))))
