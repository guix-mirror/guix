;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020 Ludovic Courtès <ludo@gnu.org>
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

(define-module (guix remote)
  #:use-module (guix ssh)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module ((guix diagnostics) #:select (formatted-message))
  #:use-module (guix inferior)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix modules)
  #:use-module (guix derivations)
  #:use-module (guix utils)
  #:use-module (ssh popen)
  #:use-module (ssh channel)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:export (remote-eval))

;;; Commentary:
;;;
;;; Note: This API is experimental and subject to change!
;;;
;;; Evaluate a gexp on a remote machine, over SSH, ensuring that all the
;;; elements the gexp refers to are deployed beforehand.  This is useful for
;;; expressions that have side effects; for pure expressions, you would rather
;;; build a derivation remotely or offload it.
;;;
;;; Code:

(define* (remote-pipe-for-gexp lowered session #:optional become-command)
  "Return a remote pipe for the given SESSION to evaluate LOWERED.  If
BECOME-COMMAND is given, use that to invoke the remote Guile REPL."
  (define shell-quote
    (compose object->string object->string))

  (define repl-command
    (append (or become-command '())
            (list
             (string-append (derivation-input-output-path
                             (lowered-gexp-guile lowered))
                            "/bin/guile")
             "--no-auto-compile")
            (append-map (lambda (directory)
                          `("-L" ,directory))
                        (lowered-gexp-load-path lowered))
            (append-map (lambda (directory)
                          `("-C" ,directory))
                        (lowered-gexp-load-path lowered))
            `("-c"
              ,(shell-quote (lowered-gexp-sexp lowered)))))

  (let ((pipe (apply open-remote-pipe* session OPEN_READ repl-command)))
    (when (eof-object? (peek-char pipe))
      (let ((status (channel-get-exit-status pipe)))
        (close-port pipe)
        (raise (formatted-message (G_ "remote command '~{~a~^ ~}' failed \
with status ~a")
                                  repl-command status))))
    pipe))

(define* (%remote-eval lowered session #:optional become-command)
  "Evaluate LOWERED, a lowered gexp, in SESSION.  This assumes that all the
prerequisites of EXP are already available on the host at SESSION.  If
BECOME-COMMAND is given, use that to invoke the remote Guile REPL."
  (let* ((pipe   (remote-pipe-for-gexp lowered session become-command))
         (result (read-repl-response pipe)))
    (close-port pipe)
    result))

(define (trampoline exp)
  "Return a \"trampoline\" gexp that evaluates EXP and writes the evaluation
result to the current output port using the (guix repl) protocol."
  (define program
    (program-file "remote-exp.scm" exp))

  (with-imported-modules (source-module-closure '((guix repl)))
    #~(begin
        (use-modules (guix repl))

        ;; We use CURRENT-OUTPUT-PORT for REPL messages, so redirect PROGRAM's
        ;; output to CURRENT-ERROR-PORT so that it does not interfere.
        (send-repl-response '(with-output-to-port (current-error-port)
                               (lambda ()
                                 (primitive-load #$program)))
                            (current-output-port))

        (force-output))))

(define* (remote-eval exp session
                      #:key
                      (build-locally? #t)
                      (system (%current-system))
                      (module-path %load-path)
                      (socket-name (%daemon-socket-uri))
                      (become-command #f))
  "Evaluate EXP, a gexp, on the host at SESSION, an SSH session.  Ensure that
all the elements EXP refers to are built and deployed to SESSION beforehand.
When BUILD-LOCALLY? is true, said dependencies are built locally and sent to
the remote store afterwards; otherwise, dependencies are built directly on the
remote store."
  (mlet* %store-monad ((lowered (lower-gexp (trampoline exp)
                                            #:system system
                                            #:guile-for-build #f
                                            #:module-path %load-path))
                       (remote -> (connect-to-remote-daemon session
                                                            socket-name)))
    (define inputs
      (cons (lowered-gexp-guile lowered)
            (lowered-gexp-inputs lowered)))

    (define sources
      (lowered-gexp-sources lowered))

    (if build-locally?
        (let ((to-send (append (append-map derivation-input-output-paths
                                           inputs)
                               sources)))
          (mbegin %store-monad
            (built-derivations inputs)
            ((store-lift send-files) to-send remote #:recursive? #t)
            (return (close-connection remote))
            (return (%remote-eval lowered session become-command))))
        (let ((to-send (append (map (compose derivation-file-name
                                             derivation-input-derivation)
                                    inputs)
                               sources)))
          (mbegin %store-monad
            ((store-lift send-files) to-send remote #:recursive? #t)
            (return (build-derivations remote inputs))
            (return (close-connection remote))
            (return (%remote-eval lowered session become-command)))))))
