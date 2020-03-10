;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Christopher Baines <mail@cbaines.net>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gnu services guix)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module ((gnu packages base)
                #:select (glibc-utf8-locales))
  #:use-module (gnu packages admin)
  #:use-module (gnu packages web)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services admin)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services getmail)
  #:use-module (gnu system shadow)
  #:export (<guix-data-service-configuration>
            guix-data-service-configuration
            guix-data-service-configuration?
            guix-data-service-package
            guix-data-service-user
            guix-data-service-group
            guix-data-service-port
            guix-data-service-host
            guix-data-service-getmail-idle-mailboxes
            guix-data-service-commits-getmail-retriever-configuration

            guix-data-service-type))

;;;; Commentary:
;;;
;;; This module implements a service that to run instances of the Guix Data
;;; Service, which provides data about Guix over time.
;;;
;;;; Code:

(define-record-type* <guix-data-service-configuration>
  guix-data-service-configuration make-guix-data-service-configuration
  guix-data-service-configuration?
  (package          guix-data-service-package
                    (default guix-data-service))
  (user             guix-data-service-configuration-user
                    (default "guix-data-service"))
  (group            guix-data-service-configuration-group
                    (default "guix-data-service"))
  (port             guix-data-service-port
                    (default 8765))
  (host             guix-data-service-host
                    (default "127.0.0.1"))
  (getmail-idle-mailboxes
   guix-data-service-getmail-idle-mailboxes
   (default #f))
  (commits-getmail-retriever-configuration
   guix-data-service-commits-getmail-retriever-configuration
   (default #f))
  (extra-options    guix-data-service-extra-options
                    (default '()))
  (extra-process-jobs-options
   guix-data-service-extra-process-jobs-options
   (default '())))

(define (guix-data-service-profile-packages config)
  "Return the guix-data-service package, this will populate the
ca-certificates.crt file in the system profile."
  (list
   (guix-data-service-package config)))

(define (guix-data-service-shepherd-services config)
  (match-record config <guix-data-service-configuration>
    (package user group port host extra-options extra-process-jobs-options)
    (list
     (shepherd-service
      (documentation "Guix Data Service web server")
      (provision '(guix-data-service))
      (requirement '(postgres networking))
      (start #~(make-forkexec-constructor
                (list #$(file-append package
                                     "/bin/guix-data-service")
                      "--pid-file=/var/run/guix-data-service/pid"
                      #$(string-append "--port=" (number->string port))
                      #$(string-append "--host=" host)
                      ;; Perform any database migrations when the
                      ;; service is started
                      "--update-database"
                      #$@extra-options)

                #:user #$user
                #:group #$group
                #:pid-file "/var/run/guix-data-service/pid"
                ;; Allow time for migrations to run
                #:pid-file-timeout 60
                #:environment-variables
                `(,(string-append
                    "GUIX_LOCPATH=" #$glibc-utf8-locales "/lib/locale")
                  "LC_ALL=en_US.utf8")
                #:log-file "/var/log/guix-data-service/web.log"))
      (stop #~(make-kill-destructor)))

     (shepherd-service
      (documentation "Guix Data Service process jobs")
      (provision '(guix-data-service-process-jobs))
      (requirement '(postgres
                     networking
                     ;; Require guix-data-service, as that the database
                     ;; migrations are handled through this service
                     guix-data-service))
      (start #~(make-forkexec-constructor
                (list
                 #$(file-append package
                                "/bin/guix-data-service-process-jobs")
                 #$@extra-process-jobs-options)
                #:user #$user
                #:group #$group
                #:environment-variables
                `("HOME=/var/lib/guix-data-service"
                  "GIT_SSL_CAINFO=/etc/ssl/certs/ca-certificates.crt"
                  ,(string-append
                    "GUIX_LOCPATH=" #$glibc-utf8-locales "/lib/locale")
                  "LC_ALL=en_US.utf8")
                #:log-file "/var/log/guix-data-service/process-jobs.log"))
      (stop #~(make-kill-destructor))))))

(define (guix-data-service-activation config)
  #~(begin
      (use-modules (guix build utils))

      (define %user (getpw "guix-data-service"))

      (chmod "/var/lib/guix-data-service" #o755)

      (mkdir-p "/var/log/guix-data-service")

      ;; Allow writing the PID file
      (mkdir-p "/var/run/guix-data-service")
      (chown "/var/run/guix-data-service"
             (passwd:uid %user)
             (passwd:gid %user))))

(define (guix-data-service-account config)
  (match-record config <guix-data-service-configuration>
    (user group)
    (list (user-group
           (name group)
           (system? #t))
          (user-account
           (name user)
           (group group)
           (system? #t)
           (comment "Guix Data Service user")
           (home-directory "/var/lib/guix-data-service")
           (shell (file-append shadow "/sbin/nologin"))))))

(define (guix-data-service-getmail-configuration config)
  (match config
    (($ <guix-data-service-configuration> package user group
                                          port host
                                          #f #f)
     '())
    (($ <guix-data-service-configuration> package user group
                                          port host
                                          getmail-idle-mailboxes
                                          commits-getmail-retriever-configuration)
     (list
      (getmail-configuration
       (name 'guix-data-service)
       (user user)
       (group group)
       (directory "/var/lib/getmail/guix-data-service")
       (rcfile
        (getmail-configuration-file
         (retriever commits-getmail-retriever-configuration)
         (destination
          (getmail-destination-configuration
           (type "MDA_external")
           (path (file-append
                  package
                  "/bin/guix-data-service-process-branch-updated-email"))))
         (options
          (getmail-options-configuration
           (read-all #f)
           (delivered-to #f)
           (received #f)))))
       (idle getmail-idle-mailboxes))))))

(define guix-data-service-type
  (service-type
   (name 'guix-data-service)
   (extensions
    (list
     (service-extension profile-service-type
                        guix-data-service-profile-packages)
     (service-extension shepherd-root-service-type
                        guix-data-service-shepherd-services)
     (service-extension activation-service-type
                        guix-data-service-activation)
     (service-extension account-service-type
                        guix-data-service-account)
     (service-extension getmail-service-type
                        guix-data-service-getmail-configuration)))
   (default-value
     (guix-data-service-configuration))
   (description
    "Run an instance of the Guix Data Service.")))
