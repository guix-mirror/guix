;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (test-processes)
  #:use-module (guix scripts processes)
  #:use-module (guix store)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module ((guix utils) #:select (call-with-temporary-directory))
  #:use-module (gnu packages bootstrap)
  #:use-module (guix tests)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 match)
  #:use-module (ice-9 threads))

;; When using --system argument, binfmt-misc mechanism may be used. In that
;; case, (guix script processes) won't work because:
;;
;; * ARGV0 is qemu-user and not guix-daemon.
;; * Guix-daemon won't be able to stuff client PID in ARGV1 of forked
;;   processes.
;;
;; See: https://lists.gnu.org/archive/html/bug-guix/2019-12/msg00017.html.
;;
;; If we detect that we are running with binfmt emulation, all the following
;; tests must be skipped.

(define (binfmt-misc?)
  (let ((pid (getpid))
        (cmdline (call-with-input-file "/proc/self/cmdline" get-string-all)))
    (match (primitive-fork)
      (0 (dynamic-wind
           (const #t)
           (lambda ()
             (exit
              (not (equal?
                    (call-with-input-file (format #f "/proc/~a/cmdline" pid)
                      get-string-all)
                    cmdline))))
           (const #t)))
      (x (zero? (cdr (waitpid x)))))))

(define-syntax-rule (test-assert* description exp)
  (begin
    (when (binfmt-misc?)
      (test-skip 1))
    (test-assert description exp)))

(test-begin "processes")

(test-assert* "not a client"
  (not (find (lambda (session)
               (= (getpid)
                  (process-id (daemon-session-client session))))
             (daemon-sessions))))

(test-assert* "client"
  (with-store store
    (let* ((session (find (lambda (session)
                            (= (getpid)
                               (process-id (daemon-session-client session))))
                          (daemon-sessions)))
           (daemon  (daemon-session-process session)))
      (and (kill (process-id daemon) 0)
           (string-suffix? "guix-daemon" (first (process-command daemon)))))))

(test-assert* "client + lock"
  (with-store store
    (call-with-temporary-directory
     (lambda (directory)
       (let* ((token1  (string-append directory "/token1"))
              (token2  (string-append directory "/token2"))
              (exp     #~(begin #$(random-text)
                                (mkdir #$token1)
                                (let loop ()
                                  (unless (file-exists? #$token2)
                                    (sleep 1)
                                    (loop)))
                                (mkdir #$output)))
              (guile   (package-derivation store %bootstrap-guile))
              (drv     (run-with-store store
                         (gexp->derivation "foo" exp
                                           #:guile-for-build guile)))
              (thread  (call-with-new-thread
                        (lambda ()
                          (build-derivations store (list drv)))))
              (_       (let loop ()
                         (unless (file-exists? token1)
                           (usleep 200)
                           (loop))))
              (session (find (lambda (session)
                               (= (getpid)
                                  (process-id (daemon-session-client session))))
                             (daemon-sessions)))
              (locks   (daemon-session-locks-held (pk 'session session))))
         (call-with-output-file token2 (const #t))
         (equal? (list (string-append (derivation->output-path drv) ".lock"))
                 locks))))))

(test-end "processes")
