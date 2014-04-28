;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu services base)
  #:use-module (gnu services)
  #:use-module (gnu system shadow)                ; 'user-account', etc.
  #:use-module (gnu system linux)                 ; 'pam-service', etc.
  #:use-module (gnu packages admin)
  #:use-module ((gnu packages base)
                #:select (glibc-final))
  #:use-module (gnu packages package-management)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 format)
  #:export (host-name-service
            mingetty-service
            nscd-service
            syslog-service
            guix-service
            %base-services))

;;; Commentary:
;;;
;;; Base system services---i.e., services that 99% of the users will want to
;;; use.
;;;
;;; Code:

(define (host-name-service name)
  "Return a service that sets the host name to NAME."
  (with-monad %store-monad
    (return (service
             (documentation "Initialize the machine's host name.")
             (provision '(host-name))
             (start #~(lambda _
                        (sethostname #$name)))
             (respawn? #f)))))

(define* (mingetty-service tty
                           #:key
                           (motd (text-file "motd" "Welcome.\n"))
                           (allow-empty-passwords? #t))
  "Return a service to run mingetty on TTY."
  (mlet %store-monad ((motd motd))
    (return
     (service
      (documentation (string-append "Run mingetty on " tty "."))
      (provision (list (symbol-append 'term- (string->symbol tty))))

      ;; Since the login prompt shows the host name, wait for the 'host-name'
      ;; service to be done.
      (requirement '(host-name))

      (start  #~(make-forkexec-constructor
                 (string-append #$mingetty "/sbin/mingetty")
                 "--noclear" #$tty))
      (stop   #~(make-kill-destructor))

      (pam-services
       ;; Let 'login' be known to PAM.  All the mingetty services will have
       ;; that PAM service, but that's fine because they're all identical and
       ;; duplicates are removed.
       (list (unix-pam-service "login"
                               #:allow-empty-passwords? allow-empty-passwords?
                               #:motd motd)))))))

(define* (nscd-service #:key (glibc glibc-final))
  "Return a service that runs libc's name service cache daemon (nscd)."
  (with-monad %store-monad
    (return (service
             (documentation "Run libc's name service cache daemon (nscd).")
             (provision '(nscd))
             (start
              #~(make-forkexec-constructor (string-append #$glibc "/sbin/nscd")
                                           "-f" "/dev/null"
                                           "--foreground"))
             (stop #~(make-kill-destructor))

             (respawn? #f)))))

(define (syslog-service)
  "Return a service that runs 'syslogd' with reasonable default settings."

  ;; Snippet adapted from the GNU inetutils manual.
  (define contents "
     # Log all kernel messages, authentication messages of
     # level notice or higher and anything of level err or
     # higher to the console.
     # Don't log private authentication messages!
     *.err;kern.*;auth.notice;authpriv.none  /dev/console

     # Log anything (except mail) of level info or higher.
     # Don't log private authentication messages!
     *.info;mail.none;authpriv.none          /var/log/messages

     # Same, in a different place.
     *.info;mail.none;authpriv.none          /dev/tty12

     # The authpriv file has restricted access.
     authpriv.*                              /var/log/secure

     # Log all the mail messages in one place.
     mail.*                                  /var/log/maillog
")

  (mlet %store-monad
      ((syslog.conf (text-file "syslog.conf" contents)))
    (return
     (service
      (documentation "Run the syslog daemon (syslogd).")
      (provision '(syslogd))
      (start
       #~(make-forkexec-constructor (string-append #$inetutils
                                                   "/libexec/syslogd")
                                    "--no-detach"
                                    "--rcfile" #$syslog.conf))
      (stop #~(make-kill-destructor))))))

(define* (guix-build-accounts count #:key
                              (first-uid 30001)
                              (gid 30000)
                              (shadow shadow))
  "Return a list of COUNT user accounts for Guix build users, with UIDs
starting at FIRST-UID, and under GID."
  (with-monad %store-monad
    (return (unfold (cut > <> count)
                    (lambda (n)
                      (user-account
                       (name (format #f "guixbuilder~2,'0d" n))
                       (password "!")
                       (uid (+ first-uid n -1))
                       (gid gid)
                       (comment (format #f "Guix Build User ~2d" n))
                       (home-directory "/var/empty")
                       (shell #~(string-append #$shadow "/sbin/nologin"))))
                    1+
                    1))))

(define* (guix-service #:key (guix guix) (builder-group "guixbuild")
                       (build-user-gid 30000) (build-accounts 10))
  "Return a service that runs the build daemon from GUIX, and has
BUILD-ACCOUNTS user accounts available under BUILD-USER-GID."
  (mlet %store-monad ((accounts (guix-build-accounts build-accounts
                                                     #:gid build-user-gid)))
    (return (service
             (provision '(guix-daemon))
             (start
              #~(make-forkexec-constructor (string-append #$guix
                                                          "/bin/guix-daemon")
                                           "--build-users-group"
                                           #$builder-group))
             (stop #~(make-kill-destructor))
             (user-accounts accounts)
             (user-groups (list (user-group
                                 (name builder-group)
                                 (id build-user-gid)
                                 (members (map user-account-name
                                               user-accounts)))))))))

(define %base-services
  ;; Convenience variable holding the basic services.
  (let ((motd (text-file "motd" "
This is the GNU operating system, welcome!\n\n")))
    (list (mingetty-service "tty1" #:motd motd)
          (mingetty-service "tty2" #:motd motd)
          (mingetty-service "tty3" #:motd motd)
          (mingetty-service "tty4" #:motd motd)
          (mingetty-service "tty5" #:motd motd)
          (mingetty-service "tty6" #:motd motd)
          (syslog-service)
          (guix-service)
          (nscd-service)

          ;; FIXME: Make this an activation-time thing instead of a service.
          (host-name-service "gnu"))))

;;; base.scm ends here
