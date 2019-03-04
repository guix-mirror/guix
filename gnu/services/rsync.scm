;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (gnu services rsync)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages admin)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (rsync-configuration
            rsync-configuration?
            rsync-service-type))

;;;; Commentary:
;;;
;;; This module implements a service that to run instance of Rsync,
;;; files synchronization tool.
;;;
;;;; Code:

(define-record-type* <rsync-configuration>
  rsync-configuration
  make-rsync-configuration
  rsync-configuration?
  (package       rsync-configuration-package              ; package
                 (default rsync))
  (port-number   rsync-configuration-port-number          ; integer
                 (default 873))
  (pid-file      rsync-configuration-pid-file             ; string
                 (default "/var/run/rsyncd/rsyncd.pid"))
  (lock-file     rsync-configuration-lock-file            ; string
                 (default "/var/run/rsyncd/rsyncd.lock"))
  (log-file      rsync-configuration-log-file             ; string
                 (default "/var/log/rsyncd.log"))
  (use-chroot?   rsync-configuration-use-chroot?          ; boolean
                 (default #t))
  (share-path    rsync-configuration-share-path           ; string
                 (default "/srv/rsyncd"))
  (share-comment rsync-configuration-share-comment        ; string
                 (default "Rsync share"))
  (read-only?    rsync-configuration-read-only?           ; boolean
                 (default #f))
  (timeout       rsync-configuration-timeout              ; integer
                 (default 300))
  (user          rsync-configuration-user                 ; string
                 (default "root"))
  (group         rsync-configuration-group                ; string
                 (default "root"))
  (uid           rsync-configuration-uid                  ; string
                 (default "rsyncd"))
  (gid           rsync-configuration-gid                  ; string
                 (default "rsyncd")))

(define (rsync-account config)
  "Return the user accounts and user groups for CONFIG."
  (let ((rsync-user (if (rsync-configuration-uid config)
                        (rsync-configuration-uid config)
                        (rsync-configuration-user config)))
        (rsync-group (if (rsync-configuration-gid config)
                         (rsync-configuration-gid config)
                         (rsync-configuration-group config))))
    (list (user-group (name rsync-group) (system? #t))
          (user-account
           (name rsync-user)
           (system? #t)
           (group rsync-group)
           (comment "rsyncd privilege separation user")
           (home-directory (string-append "/var/run/"
                                          rsync-user))
           (shell (file-append shadow "/sbin/nologin"))))))

(define (rsync-activation config)
  "Return the activation GEXP for CONFIG."
  (with-imported-modules '((guix build utils))
    #~(begin
        (let ((share-directory  #$(rsync-configuration-share-path config))
              (user  (getpw (if #$(rsync-configuration-uid config)
                                #$(rsync-configuration-uid config)
                                #$(rsync-configuration-user config))))
              (group (getpw (if #$(rsync-configuration-gid config)
                                #$(rsync-configuration-gid config)
                                #$(rsync-configuration-group config)))))
          (mkdir-p (dirname #$(rsync-configuration-pid-file config)))
          (and=> share-directory mkdir-p)
          (chown share-directory
                 (passwd:uid user)
                 (group:gid group))))))

(define rsync-config-file
  ;; Return the rsync configuration file corresponding to CONFIG.
  (match-lambda
    (($ <rsync-configuration> package port-number pid-file lock-file log-file
                              use-chroot? share-path share-comment read-only?
                              timeout user group uid gid)
     (if (not (string=? user "root"))
         (cond
          ((<= port-number 1024)
           (error (string-append "rsync-service: to run on port "
                                 (number->string port-number)
                                 ", user must be root.")))
          (use-chroot?
           (error (string-append "rsync-service: to run in a chroot"
                                 ", user must be root.")))
          (uid
           (error "rsync-service: to use uid, user must be root."))
          (gid
           (error "rsync-service: to use gid, user must be root."))))
     (mixed-text-file
      "rsync.conf"
      "# Generated by 'rsync-service'.\n\n"
      "pid file = " pid-file "\n"
      "lock file = " lock-file "\n"
      "log file = " log-file "\n"
      "port = " (number->string port-number) "\n"
      "use chroot = " (if use-chroot? "true" "false") "\n"
      (if uid (string-append "uid = " uid "\n") "")
      "gid = " (if gid gid "nogroup") "\n" ; no group nobody
      "\n"
      "[files]\n"
      "path = " share-path "\n"
      "comment = " share-comment "\n"
      "read only = " (if read-only? "true" "false") "\n"
      "timeout = " (number->string timeout) "\n"))))

(define (rsync-shepherd-service config)
  "Return a <shepherd-service> for rsync with CONFIG."
  (let* ((rsync       (rsync-configuration-package config))
         (pid-file    (rsync-configuration-pid-file config))
         (port-number (rsync-configuration-port-number config))
         (user        (rsync-configuration-user config))
         (group       (rsync-configuration-group config)))
    (list (shepherd-service
           (provision '(rsync))
           (documentation "Run rsync daemon.")
           (start #~(make-forkexec-constructor
                     (list (string-append #$rsync "/bin/rsync")
                           "--config" #$(rsync-config-file config)
                           "--daemon")
                     #:pid-file #$pid-file
                     #:user #$user
                     #:group #$group))
           (stop #~(make-kill-destructor))))))

(define rsync-service-type
  (service-type
   (name 'rsync)
   (extensions
    (list (service-extension shepherd-root-service-type rsync-shepherd-service)
          (service-extension account-service-type rsync-account)
          (service-extension activation-service-type rsync-activation)))
   (default-value (rsync-configuration))))
