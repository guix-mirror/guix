;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu system dmd)
  #:use-module (guix store)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (guix records)
  #:use-module ((gnu packages base)
                #:select (glibc-final))
  #:use-module ((gnu packages system)
                #:select (mingetty inetutils shadow))
  #:use-module ((gnu packages package-management)
                #:select (guix))
  #:use-module ((gnu packages linux)
                #:select (net-tools))
  #:use-module (gnu system shadow)                ; for user accounts/groups
  #:use-module (gnu system linux)                 ; for PAM services
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (guix monads)
  #:export (service?
            service
            service-provision
            service-requirement
            service-respawn?
            service-start
            service-stop
            service-inputs
            service-user-accounts
            service-user-groups
            service-pam-services

            host-name-service
            syslog-service
            mingetty-service
            nscd-service
            guix-service
            static-networking-service

            dmd-configuration-file))

;;; Commentary:
;;;
;;; System services as cajoled by dmd.
;;;
;;; Code:

(define-record-type* <service>
  service make-service
  service?
  (documentation service-documentation            ; string
                 (default "[No documentation.]"))
  (provision     service-provision)               ; list of symbols
  (requirement   service-requirement              ; list of symbols
                 (default '()))
  (respawn?      service-respawn?                 ; Boolean
                 (default #t))
  (start         service-start)                   ; expression
  (stop          service-stop                     ; expression
                 (default #f))
  (inputs        service-inputs                   ; list of inputs
                 (default '()))
  (user-accounts service-user-accounts            ; list of <user-account>
                 (default '()))
  (user-groups   service-user-groups              ; list of <user-groups>
                 (default '()))
  (pam-services  service-pam-services             ; list of <pam-service>
                 (default '())))

(define (host-name-service name)
  "Return a service that sets the host name to NAME."
  (with-monad %store-monad
    (return (service
             (documentation "Initialize the machine's host name.")
             (provision '(host-name))
             (start `(lambda _
                       (sethostname ,name)))
             (respawn? #f)))))

(define* (mingetty-service tty
                           #:key
                           (motd (text-file "motd" "Welcome.\n"))
                           (allow-empty-passwords? #t))
  "Return a service to run mingetty on TTY."
  (mlet %store-monad ((mingetty-bin (package-file mingetty "sbin/mingetty"))
                      (motd         motd))
    (return
     (service
      (documentation (string-append "Run mingetty on " tty "."))
      (provision (list (symbol-append 'term- (string->symbol tty))))

      ;; Since the login prompt shows the host name, wait for the 'host-name'
      ;; service to be done.
      (requirement '(host-name))

      (start  `(make-forkexec-constructor ,mingetty-bin "--noclear" ,tty))
      (stop   `(make-kill-destructor))
      (inputs `(("mingetty" ,mingetty)
                ("motd" ,motd)))

      (pam-services
       ;; Let 'login' be known to PAM.  All the mingetty services will have
       ;; that PAM service, but that's fine because they're all identical and
       ;; duplicates are removed.
       (list (unix-pam-service "login"
                               #:allow-empty-passwords? allow-empty-passwords?
                               #:motd motd)))))))

(define* (nscd-service #:key (glibc glibc-final))
  "Return a service that runs libc's name service cache daemon (nscd)."
  (mlet %store-monad ((nscd (package-file glibc "sbin/nscd")))
    (return (service
             (documentation "Run libc's name service cache daemon (nscd).")
             (provision '(nscd))
             (start `(make-forkexec-constructor ,nscd "-f" "/dev/null"
                                                "--foreground"))
             (stop  `(make-kill-destructor))

             (respawn? #f)
             (inputs `(("glibc" ,glibc)))))))

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
      ((syslog.conf (text-file "syslog.conf" contents))
       (syslogd     (package-file inetutils "libexec/syslogd")))
    (return
     (service
      (documentation "Run the syslog daemon (syslogd).")
      (provision '(syslogd))
      (start `(make-forkexec-constructor ,syslogd "--no-detach"
                                         "--rcfile" ,syslog.conf))
      (stop  `(make-kill-destructor))
      (inputs `(("inetutils" ,inetutils)
                ("syslog.conf" ,syslog.conf)))))))

(define* (guix-build-accounts count #:key
                              (first-uid 30001)
                              (gid 30000)
                              (shadow shadow))
  "Return a list of COUNT user accounts for Guix build users, with UIDs
starting at FIRST-UID, and under GID."
  (mlet* %store-monad ((gid* -> gid)
                       (no-login (package-file shadow "sbin/nologin")))
    (return (unfold (cut > <> count)
                    (lambda (n)
                      (user-account
                       (name (format #f "guixbuilder~2,'0d" n))
                       (password "!")
                       (uid (+ first-uid n -1))
                       (gid gid*)
                       (comment (format #f "Guix Build User ~2d" n))
                       (home-directory "/var/empty")
                       (shell no-login)))
                    1+
                    1))))

(define* (guix-service #:key (guix guix) (builder-group "guixbuild")
                       (build-user-gid 30000) (build-accounts 10))
  "Return a service that runs the build daemon from GUIX, and has
BUILD-ACCOUNTS user accounts available under BUILD-USER-GID."
  (mlet %store-monad ((daemon   (package-file guix "bin/guix-daemon"))
                      (accounts (guix-build-accounts build-accounts
                                                     #:gid build-user-gid)))
    (return (service
             (provision '(guix-daemon))
             (start `(make-forkexec-constructor ,daemon
                                                "--build-users-group"
                                                ,builder-group))
             (stop  `(make-kill-destructor))
             (inputs `(("guix" ,guix)))
             (user-accounts accounts)
             (user-groups (list (user-group
                                 (name builder-group)
                                 (id build-user-gid)
                                 (members (map user-account-name
                                               user-accounts)))))))))

(define* (static-networking-service interface ip
                                    #:key
                                    gateway
                                    (name-servers '())
                                    (inetutils inetutils)
                                    (net-tools net-tools))
  "Return a service that starts INTERFACE with address IP.  If GATEWAY is
true, it must be a string specifying the default network gateway."

  ;; TODO: Eventually we should do this using Guile's networking procedures,
  ;; like 'configure-qemu-networking' does, but the patch that does this is
  ;; not yet in stock Guile.
  (mlet %store-monad ((ifconfig (package-file inetutils "bin/ifconfig"))
                      (route    (package-file net-tools "sbin/route")))
    (return
     (service
      (documentation
       (string-append "Set up networking on the '" interface
                      "' interface using a static IP address."))
      (provision '(networking))
      (start `(lambda _
                ;; Return #t if successfully started.
                (and (zero? (system* ,ifconfig ,interface ,ip "up"))
                     ,(if gateway
                          `(zero? (system* ,route "add" "-net" "default"
                                           "gw" ,gateway))
                          #t)
                     ,(if (pair? name-servers)
                          `(call-with-output-file "/etc/resolv.conf"
                             (lambda (port)
                               (display
                                "# Generated by 'static-networking-service'.\n"
                                port)
                               (for-each (lambda (server)
                                           (format port "nameserver ~a~%"
                                                   server))
                                         ',name-servers)))
                          #t))))
      (stop  `(lambda _
                ;; Return #f is successfully stopped.
                (not (and (system* ,ifconfig ,interface "down")
                          (system* ,route "del" "-net" "default")))))
      (respawn? #f)
      (inputs `(("inetutils" ,inetutils)
                ,@(if gateway
                      `(("net-tools" ,net-tools))
                      '())))))))


(define (dmd-configuration-file services)
  "Return the dmd configuration file for SERVICES."
  (define config
    `(begin
       (register-services
        ,@(map (match-lambda
                (($ <service> documentation provision requirement
                    respawn? start stop)
                 `(make <service>
                    #:docstring ,documentation
                    #:provides ',provision
                    #:requires ',requirement
                    #:respawn? ,respawn?
                    #:start ,start
                    #:stop ,stop)))
               services))
       (for-each start ',(append-map service-provision services))))

  (text-file "dmd.conf" (object->string config)))

;;; dmd.scm ends here
