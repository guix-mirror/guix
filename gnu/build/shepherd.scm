;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu build shepherd)
  #:use-module (gnu system file-systems)
  #:use-module (gnu build linux-container)
  #:use-module (guix build utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (make-forkexec-constructor/container))

;;; Commentary:
;;;
;;; This module provides extensions to the GNU Shepherd.  In particular, it
;;; provides a helper to start services in a container.
;;;
;;; Code:

(define (clean-up file)
  (when file
    (catch 'system-error
      (lambda ()
        (delete-file file))
      (lambda args
        (unless (= ENOENT (system-error-errno args))
          (apply throw args))))))

(define-syntax-rule (catch-system-error exp)
  (catch 'system-error
    (lambda ()
      exp)
    (const #f)))

(define (default-namespaces args)
  ;; Most daemons are here to talk to the network, and most of them expect to
  ;; run under a non-zero UID.
  (fold delq %namespaces '(net user)))

(define* (default-mounts #:key (namespaces (default-namespaces '())))
  (define (tmpfs directory)
    (file-system
      (device "none")
      (mount-point directory)
      (type "tmpfs")
      (check? #f)))

  (define accounts
    ;; This is for processes in the default user namespace but living in a
    ;; different mount namespace, so that they can lookup users.
    (list (file-system-mapping
           (source "/etc/passwd") (target source))
          (file-system-mapping
           (source "/etc/group") (target source))))

  (define nscd-socket
    (file-system-mapping
     (source "/var/run/nscd") (target source)
     (writable? #t)))

  (append (cons (tmpfs "/tmp") %container-file-systems)
          (let ((mappings `(,@(if (memq 'net namespaces)
                                  '()
                                  (cons nscd-socket
                                        %network-file-mappings))
                            ,@(if (and (memq 'mnt namespaces)
                                       (not (memq 'user namespaces)))
                                  accounts
                                  '())

                            ;; Tell the process what timezone we're in.  This
                            ;; makes sure that, for instance, its syslog
                            ;; messages have the correct timestamp.
                            ,(file-system-mapping
                              (source "/etc/localtime")
                              (target source))

                            ,%store-mapping)))    ;XXX: coarse-grain
            (map file-system-mapping->bind-mount
                 (filter (lambda (mapping)
                           (file-exists? (file-system-mapping-source mapping)))
                         mappings)))))

;; XXX: Lazy-bind the Shepherd to avoid a compile-time dependency.
(module-autoload! (current-module)
                  '(shepherd service) '(read-pid-file exec-command))

(define* (read-pid-file/container pid pid-file #:key (max-delay 5))
  "Read PID-FILE in the container namespaces of PID, which exists in a
separate mount and PID name space.  Return the \"outer\" PID. "
  (match (container-excursion* pid
           (lambda ()
             (read-pid-file pid-file
                            #:max-delay max-delay)))
    (#f
     (catch-system-error (kill pid SIGTERM))
     #f)
    ((? integer? container-pid)
     ;; XXX: When COMMAND is started in a separate PID namespace, its
     ;; PID is always 1, but that's not what Shepherd needs to know.
     pid)))

(define* (make-forkexec-constructor/container command
                                              #:key
                                              (namespaces
                                               (default-namespaces args))
                                              (mappings '())
                                              (user #f)
                                              (group #f)
                                              (log-file #f)
                                              pid-file
                                              (pid-file-timeout 5)
                                              (directory "/")
                                              (environment-variables
                                               (environ))
                                              #:rest args)
  "This is a variant of 'make-forkexec-constructor' that starts COMMAND in
NAMESPACES, a list of Linux namespaces such as '(mnt ipc).  MAPPINGS is the
list of <file-system-mapping> to make in the case of a separate mount
namespace, in addition to essential bind-mounts such /proc."
  (define container-directory
    (match command
      ((program _  ...)
       (string-append "/var/run/containers/" (basename program)))))

  (define auto-mappings
    `(,@(if log-file
            (list (file-system-mapping
                   (source log-file)
                   (target source)
                   (writable? #t)))
            '())))

  (define mounts
    (append (map file-system-mapping->bind-mount
                 (append auto-mappings mappings))
            (default-mounts #:namespaces namespaces)))

  (lambda args
    (mkdir-p container-directory)

    (when log-file
      ;; Create LOG-FILE so we can map it in the container.
      (unless (file-exists? log-file)
        (call-with-output-file log-file (const #t))))

    (let ((pid (run-container container-directory
                              mounts namespaces 1
                              (lambda ()
                                (mkdir-p "/var/run")
                                (clean-up pid-file)
                                (clean-up log-file)

                                (exec-command command
                                              #:user user
                                              #:group group
                                              #:log-file log-file
                                              #:directory directory
                                              #:environment-variables
                                              environment-variables)))))
      (if pid-file
          (if (or (memq 'mnt namespaces) (memq 'pid namespaces))
              (read-pid-file/container pid pid-file
                                       #:max-delay pid-file-timeout)
              (read-pid-file pid-file #:max-delay pid-file-timeout))
          pid))))

;; Local Variables:
;; eval: (put 'container-excursion* 'scheme-indent-function 1)
;; End:

;;; shepherd.scm ends here
