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
  #:use-module ((gnu packages system)
                #:select (mingetty inetutils))
  #:use-module ((gnu packages package-management)
                #:select (guix))
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (service?
            service
            service-provision
            service-requirement
            service-respawn?
            service-start
            service-stop
            service-inputs

            syslog-service
            mingetty-service
            guix-service
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

(define (mingetty-service store tty)
  "Return a service to run mingetty on TTY."
  (let* ((mingetty-drv (package-derivation store mingetty))
         (mingetty-bin (string-append (derivation->output-path mingetty-drv)
                                      "/sbin/mingetty")))
    (service
     (provision (list (symbol-append 'term- (string->symbol tty))))
     (start `(make-forkexec-constructor ,mingetty-bin "--noclear" ,tty))
     (inputs `(("mingetty" ,mingetty))))))

(define (syslog-service store)
  "Return a service that runs 'syslogd' with reasonable default settings."

  (define syslog.conf
    ;; Snippet adapted from the GNU inetutils manual.
    (add-text-to-store store "syslog.conf" "
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
"))

  (let* ((inetutils-drv (package-derivation store inetutils))
         (syslogd       (string-append (derivation->output-path inetutils-drv)
                                       "/libexec/syslogd")))
    (service
     (provision '(syslogd))
     (start `(make-forkexec-constructor ,syslogd
                                        "--rcfile" ,syslog.conf))
     (inputs `(("inetutils" ,inetutils)
               ("syslog.conf" ,syslog.conf))))))

(define* (guix-service store #:key (guix guix))
  "Return a service that runs the build daemon from GUIX."
  (let* ((drv    (package-derivation store guix))
         (daemon (string-append (derivation->output-path drv)
                                "/bin/guix-daemon")))
    (service
     (provision '(guix-daemon))
     (start `(make-forkexec-constructor ,daemon))
     (inputs `(("guix" ,guix))))))


(define (dmd-configuration-file store services)
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

  (add-text-to-store store "dmd.conf"
                     (object->string config)))

;;; dmd.scm ends here
