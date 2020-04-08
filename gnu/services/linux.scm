;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Brice Waegeneire <brice@waegenei.re>
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

(define-module (gnu services linux)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix modules)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages linux)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:export (earlyoom-configuration
            earlyoom-configuration?
            earlyoom-configuration-earlyoom
            earlyoom-configuration-minimum-available-memory
            earlyoom-configuration-minimum-free-swap
            earlyoom-configuration-prefer-regexp
            earlyoom-configuration-avoid-regexp
            earlyoom-configuration-memory-report-interval
            earlyoom-configuration-ignore-positive-oom-score-adj?
            earlyoom-configuration-show-debug-messages?
            earlyoom-configuration-send-notification-command
            earlyoom-service-type

            kernel-module-loader-service-type))


;;;
;;; Early OOM daemon.
;;;

(define-record-type* <earlyoom-configuration>
  earlyoom-configuration make-earlyoom-configuration
  earlyoom-configuration?
  (earlyoom earlyoom-configuration-earlyoom
            (default earlyoom))
  (minimum-available-memory earlyoom-configuration-minimum-available-memory
                            (default 10)) ; in percent
  (minimum-free-swap earlyoom-configuration-minimum-free-swap
                     (default 10))      ; in percent
  (prefer-regexp earlyoom-configuration-prefer-regexp ; <string>
                 (default #f))
  (avoid-regexp earlyoom-configuration-avoid-regexp  ; <string>
                (default #f))
  (memory-report-interval earlyoom-configuration-memory-report-interval
                          (default 0)) ; in seconds; 0 means disabled
  (ignore-positive-oom-score-adj?
   earlyoom-configuration-ignore-positive-oom-score-adj? (default #f))
  (run-with-higher-priority? earlyoom-configuration-run-with-higher-priority?
                             (default #f))
  (show-debug-messages? earlyoom-configuration-show-debug-messages?
                        (default #f))
  (send-notification-command
   earlyoom-configuration-send-notification-command  ; <string>
   (default #f)))

(define (earlyoom-configuration->command-line-args config)
  "Translate a <earlyoom-configuration> object to its command line arguments
representation."
  (match config
    (($ <earlyoom-configuration> earlyoom minimum-available-memory
                                 minimum-free-swap prefer-regexp avoid-regexp
                                 memory-report-interval
                                 ignore-positive-oom-score-adj?
                                 run-with-higher-priority? show-debug-messages?
                                 send-notification-command)
     `(,(file-append earlyoom "/bin/earlyoom")
       ,@(if minimum-available-memory
             (list "-m" (format #f "~s" minimum-available-memory))
             '())
       ,@(if minimum-free-swap
             (list "-s" (format #f "~s" minimum-free-swap))
             '())
       ,@(if prefer-regexp
             (list "--prefer" prefer-regexp)
             '())
       ,@(if avoid-regexp
             (list "--avoid" avoid-regexp)
             '())
       "-r" ,(format #f "~s" memory-report-interval)
       ,@(if ignore-positive-oom-score-adj?
             (list "-i")
             '())
       ,@(if run-with-higher-priority?
             (list "-p")
             '())
       ,@(if show-debug-messages?
             (list "-d")
             '())
       ,@(if send-notification-command
             (list "-N" send-notification-command)
             '())))))

(define (earlyoom-shepherd-service config)
  (shepherd-service
   (documentation "Run the Early OOM daemon.")
   (provision '(earlyoom))
   (start #~(make-forkexec-constructor
             '#$(earlyoom-configuration->command-line-args config)
             #:log-file "/var/log/earlyoom.log"))
   (stop #~(make-kill-destructor))))

(define earlyoom-service-type
  (service-type
   (name 'earlyoom)
   (default-value (earlyoom-configuration))
   (extensions
    (list (service-extension shepherd-root-service-type
                             (compose list earlyoom-shepherd-service))))
   (description "Run @command{earlyoom}, the Early OOM daemon.")))


;;;
;;; Kernel module loader.
;;;

(define kernel-module-loader-shepherd-service
  (match-lambda
    ((and (? list? kernel-modules) ((? string?) ...))
     (list
      (shepherd-service
       (documentation "Load kernel modules.")
       (provision '(kernel-module-loader))
       (requirement '(file-systems))
       (respawn? #f)
       (one-shot? #t)
       (modules `((srfi srfi-1)
                  (srfi srfi-34)
                  (srfi srfi-35)
                  (rnrs io ports)
                  ,@%default-modules))
       (start
        #~(lambda _
            (cond
             ((null? '#$kernel-modules) #t)
             ((file-exists? "/proc/sys/kernel/modprobe")
              (let ((modprobe (call-with-input-file
                               "/proc/sys/kernel/modprobe" get-line)))
                (guard (c ((message-condition? c)
                           (format (current-error-port) "~a~%"
                                   (condition-message c))
                           #f))
                  (every (lambda (module)
                         (invoke/quiet modprobe "--" module))
                         '#$kernel-modules))))
             (else
               (format (current-error-port) "error: ~a~%"
                       "Kernel is missing loadable module support.")
               #f)))))))))

(define kernel-module-loader-service-type
  (service-type
   (name 'kernel-module-loader)
   (description "Load kernel modules.")
   (extensions
    (list (service-extension shepherd-root-service-type
                             kernel-module-loader-shepherd-service)))
   (compose concatenate)
   (extend append)
   (default-value '())))
