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
                #:select (mingetty inetutils))
  #:use-module ((gnu packages package-management)
                #:select (guix))
  #:use-module ((gnu packages linux)
                #:select (net-tools))
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (guix monads)
  #:export (service?
            service
            service-provision
            service-requirement
            service-respawn?
            service-start
            service-stop
            service-inputs

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
  (provision     service-provision)               ; list of symbols
  (requirement   service-requirement              ; list of symbols
                 (default '()))
  (respawn?      service-respawn?                 ; Boolean
                 (default #t))
  (start         service-start)                   ; expression
  (stop          service-stop                     ; expression
                 (default #f))
  (inputs        service-inputs                   ; list of inputs
                 (default '())))

(define (host-name-service name)
  "Return a service that sets the host name to NAME."
  (with-monad %store-monad
    (return (service
             (provision '(host-name))
             (start `(lambda _
                       (sethostname ,name)))
             (respawn? #f)))))

(define (mingetty-service tty)
  "Return a service to run mingetty on TTY."
  (mlet %store-monad ((mingetty-bin (package-file mingetty "sbin/mingetty")))
    (return
     (service
      (provision (list (symbol-append 'term- (string->symbol tty))))

      ;; Since the login prompt shows the host name, wait for the 'host-name'
      ;; service to be done.
      (requirement '(host-name))

      (start `(make-forkexec-constructor ,mingetty-bin "--noclear" ,tty))
      (inputs `(("mingetty" ,mingetty)))))))

(define* (nscd-service #:key (glibc glibc-final))
  "Return a service that runs libc's name service cache daemon (nscd)."
  (mlet %store-monad ((nscd (package-file glibc "sbin/nscd")))
    (return (service
             (provision '(nscd))
             (start `(make-forkexec-constructor ,nscd "-f" "/dev/null"))

             ;; XXX: Local copy of 'make-kill-destructor' because the one upstream
             ;; uses the broken 'opt-lambda' macro.
             (stop  `(lambda* (#:optional (signal SIGTERM))
                       (lambda (pid . args)
                         (kill pid signal)
                         #f)))

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
      (provision '(syslogd))
      (start `(make-forkexec-constructor ,syslogd
                                         "--rcfile" ,syslog.conf))
      (inputs `(("inetutils" ,inetutils)
                ("syslog.conf" ,syslog.conf)))))))

(define* (guix-service #:key (guix guix) (builder-group "guixbuild"))
  "Return a service that runs the build daemon from GUIX."
  (mlet %store-monad ((daemon (package-file guix "bin/guix-daemon")))
    (return (service
             (provision '(guix-daemon))
             (start `(make-forkexec-constructor ,daemon
                                                "--build-users-group"
                                                ,builder-group))
             (inputs `(("guix" ,guix)))))))

(define* (static-networking-service interface ip
                                    #:key
                                    gateway
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
      (provision '(networking))
      (start `(lambda _
                (and (zero? (system* ,ifconfig ,interface ,ip "up"))
                     ,(if gateway
                          `(begin
                             (sleep 3)            ; XXX
                             (zero? (system* ,route "add" "-net" "default"
                                             "gw" ,gateway)))
                          #t))))
      (stop  `(lambda _
                (system* ,ifconfig ,interface "down")
                (system* ,route "del" "-net" "default")))
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
                (($ <service> provision requirement respawn? start stop)
                 `(make <service>
                    #:provides ',provision
                    #:requires ',requirement
                    #:respawn? ,respawn?
                    #:start ,start
                    #:stop ,stop)))
               services))
       (for-each start ',(append-map service-provision services))))

  (text-file "dmd.conf" (object->string config)))

;;; dmd.scm ends here
