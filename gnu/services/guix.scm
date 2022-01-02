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
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module ((gnu packages base)
                #:select (glibc-utf8-locales))
  #:use-module (gnu packages admin)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages web)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages package-management)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services admin)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services getmail)
  #:use-module (gnu system shadow)
  #:export (guix-build-coordinator-configuration
            guix-build-coordinator-configuration?
            guix-build-coordinator-configuration-package
            guix-build-coordinator-configuration-user
            guix-build-coordinator-configuration-group
            guix-build-coordinator-configuration-datastore-uri-string
            guix-build-coordinator-configuration-agent-communication-uri-string
            guix-build-coordinator-configuration-client-communication-uri-string
            guix-build-coordinator-configuration-allocation-strategy
            guix-build-coordinator-configuration-hooks
            guix-build-coordinator-configuration-guile

            guix-build-coordinator-service-type

            guix-build-coordinator-agent-configuration
            guix-build-coordinator-agent-configuration?
            guix-build-coordinator-agent-configuration-package
            guix-build-coordinator-agent-configuration-user
            guix-build-coordinator-agent-configuration-coordinator
            guix-build-coordinator-agent-configuration-authentication
            guix-build-coordinator-agent-configuration-systems
            guix-build-coordinator-agent-configuration-max-parallel-builds
            guix-build-coordinator-agent-configuration-max-1min-load-average
            guix-build-coordinator-agent-configuration-derivation-substitute-urls
            guix-build-coordinator-agent-configuration-non-derivation-substitute-urls

            guix-build-coordinator-agent-password-auth
            guix-build-coordinator-agent-password-auth?
            guix-build-coordinator-agent-password-auth-uuid
            guix-build-coordinator-agent-password-auth-password

            guix-build-coordinator-agent-password-file-auth
            guix-build-coordinator-agent-password-file-auth?
            guix-build-coordinator-agent-password-file-auth-uuid
            guix-build-coordinator-agent-password-file-auth-password-file

            guix-build-coordinator-agent-dynamic-auth
            guix-build-coordinator-agent-dynamic-auth?
            guix-build-coordinator-agent-dynamic-auth-agent-name
            guix-build-coordinator-agent-dynamic-auth-token

            guix-build-coordinator-agent-dynamic-auth-with-file
            guix-build-coordinator-agent-dynamic-auth-with-file?
            guix-build-coordinator-agent-dynamic-auth-with-file-agent-name
            guix-build-coordinator-agent-dynamic-auth-with-file-token-file

            guix-build-coordinator-agent-service-type

            guix-build-coordinator-queue-builds-configuration
            guix-build-coordinator-queue-builds-configuration?
            guix-build-coordinator-queue-builds-configuration-package
            guix-build-coordinator-queue-builds-configuration-user
            guix-build-coordinator-queue-builds-coordinator
            guix-build-coordinator-queue-builds-configuration-systems
            guix-build-coordinator-queue-builds-configuration-system-and-targets
            guix-build-coordinator-queue-builds-configuration-guix-data-service
            guix-build-coordinator-queue-builds-configuration-processed-commits-file

            guix-build-coordinator-queue-builds-service-type

            <guix-data-service-configuration>
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
;;; Services specifically related to GNU Guix.
;;;
;;;; Code:

(define-record-type* <guix-build-coordinator-configuration>
  guix-build-coordinator-configuration make-guix-build-coordinator-configuration
  guix-build-coordinator-configuration?
  (package                         guix-build-coordinator-configuration-package
                                   (default guix-build-coordinator))
  (user                            guix-build-coordinator-configuration-user
                                   (default "guix-build-coordinator"))
  (group                           guix-build-coordinator-configuration-group
                                   (default "guix-build-coordinator"))
  (database-uri-string
   guix-build-coordinator-configuration-datastore-uri-string
   (default "sqlite:///var/lib/guix-build-coordinator/guix_build_coordinator.db"))
  (agent-communication-uri-string
   guix-build-coordinator-configuration-agent-communication-uri-string
   (default "http://0.0.0.0:8745"))
  (client-communication-uri-string
   guix-build-coordinator-configuration-client-communication-uri-string
   (default "http://127.0.0.1:8746"))
  (allocation-strategy
   guix-build-coordinator-configuration-allocation-strategy
   (default #~basic-build-allocation-strategy))
  (hooks                           guix-build-coordinator-configuration-hooks
                                   (default '()))
  (guile                           guix-build-coordinator-configuration-guile
                                   (default guile-3.0-latest)))

(define-record-type* <guix-build-coordinator-agent-configuration>
  guix-build-coordinator-agent-configuration
  make-guix-build-coordinator-agent-configuration
  guix-build-coordinator-agent-configuration?
  (package             guix-build-coordinator-agent-configuration-package
                       (default guix-build-coordinator/agent-only))
  (user                guix-build-coordinator-agent-configuration-user
                       (default "guix-build-coordinator-agent"))
  (coordinator         guix-build-coordinator-agent-configuration-coordinator
                       (default "http://localhost:8745"))
  (authentication      guix-build-coordinator-agent-configuration-authentication)
  (systems             guix-build-coordinator-agent-configuration-systems
                       (default #f))
  (max-parallel-builds
   guix-build-coordinator-agent-configuration-max-parallel-builds
   (default 1))
  (max-1min-load-average
   guix-build-coordinator-agent-configuration-max-1min-load-average
   (default #f))
  (derivation-substitute-urls
   guix-build-coordinator-agent-configuration-derivation-substitute-urls
   (default #f))
  (non-derivation-substitute-urls
   guix-build-coordinator-agent-configuration-non-derivation-substitute-urls
   (default #f)))

(define-record-type* <guix-build-coordinator-agent-password-auth>
  guix-build-coordinator-agent-password-auth
  make-guix-build-coordinator-agent-password-auth
  guix-build-coordinator-agent-password-auth?
  (uuid                guix-build-coordinator-agent-password-auth-uuid)
  (password            guix-build-coordinator-agent-password-auth-password))

(define-record-type* <guix-build-coordinator-agent-password-file-auth>
  guix-build-coordinator-agent-password-file-auth
  make-guix-build-coordinator-agent-password-file-auth
  guix-build-coordinator-agent-password-file-auth?
  (uuid                guix-build-coordinator-agent-password-file-auth-uuid)
  (password-file
   guix-build-coordinator-agent-password-file-auth-password-file))

(define-record-type* <guix-build-coordinator-agent-dynamic-auth>
  guix-build-coordinator-agent-dynamic-auth
  make-guix-build-coordinator-agent-dynamic-auth
  guix-build-coordinator-agent-dynamic-auth?
  (agent-name          guix-build-coordinator-agent-dynamic-auth-agent-name)
  (token               guix-build-coordinator-agent-dynamic-auth-token))

(define-record-type* <guix-build-coordinator-agent-dynamic-auth-with-file>
  guix-build-coordinator-agent-dynamic-auth-with-file
  make-guix-build-coordinator-agent-dynamic-auth-with-file
  guix-build-coordinator-agent-dynamic-auth-with-file?
  (agent-name      guix-build-coordinator-agent-dynamic-auth-with-file-agent-name)
  (token-file      guix-build-coordinator-agent-dynamic-auth-with-file-token-file))

(define-record-type* <guix-build-coordinator-queue-builds-configuration>
  guix-build-coordinator-queue-builds-configuration
  make-guix-build-coordinator-queue-builds-configuration
  guix-build-coordinator-queue-builds-configuration?
  (package              guix-build-coordinator-queue-builds-configuration-package
                        (default guix-build-coordinator))
  (user                 guix-build-coordinator-queue-builds-configuration-user
                        (default "guix-build-coordinator-queue-builds"))
  (coordinator          guix-build-coordinator-queue-builds-coordinator
                        (default "http://localhost:8746"))
  (systems              guix-build-coordinator-queue-builds-configuration-systems
                        (default #f))
  (systems-and-targets
   guix-build-coordinator-queue-builds-configuration-system-and-targets
   (default #f))
  (guix-data-service
   guix-build-coordinator-queue-builds-configuration-guix-data-service
   (default "https://data.guix.gnu.org"))
  (processed-commits-file
   guix-build-coordinator-queue-builds-configuration-processed-commits-file
   (default "/var/cache/guix-build-coordinator-queue-builds/processed-commits")))

(define* (make-guix-build-coordinator-start-script database-uri-string
                                                   allocation-strategy
                                                   pid-file
                                                   guix-build-coordinator-package
                                                   #:key
                                                   agent-communication-uri-string
                                                   client-communication-uri-string
                                                   (hooks '())
                                                   (guile guile-3.0))
  (program-file
   "start-guix-build-coordinator"
   (with-extensions (cons guix-build-coordinator-package
                          ;; This is a poorly constructed Guile load path,
                          ;; since it contains things that aren't Guile
                          ;; libraries, but it means that the Guile libraries
                          ;; needed for the Guix Build Coordinator don't need
                          ;; to be individually specified here.
                          (append
                           (map second (package-inputs
                                        guix-build-coordinator-package))
                           (map second (package-propagated-inputs
                                        guix-build-coordinator-package))))
     #~(begin
         (use-modules (srfi srfi-1)
                      (ice-9 match)
                      (web uri)
                      (prometheus)
                      (guix-build-coordinator hooks)
                      (guix-build-coordinator datastore)
                      (guix-build-coordinator build-allocator)
                      (guix-build-coordinator coordinator))

         (setvbuf (current-output-port) 'line)
         (setvbuf (current-error-port) 'line)

         (simple-format #t "starting the guix-build-coordinator:\n  ~A\n"
                        (current-filename))
         (let* ((metrics-registry (make-metrics-registry
                                   #:namespace
                                   "guixbuildcoordinator"))
                (datastore (database-uri->datastore
                            #$database-uri-string
                            #:metrics-registry metrics-registry))
                (hooks
                 (list #$@(map (match-lambda
                                 ((name . hook-gexp)
                                  #~(cons '#$name #$hook-gexp)))
                               hooks)))
                (hooks-with-defaults
                 `(,@hooks
                   ,@(remove (match-lambda
                               ((name . _) (assq-ref hooks name)))
                             %default-hooks)))
                (build-coordinator (make-build-coordinator
                                    #:datastore datastore
                                    #:hooks hooks-with-defaults
                                    #:metrics-registry metrics-registry
                                    #:allocation-strategy #$allocation-strategy)))

           (run-coordinator-service
            build-coordinator
            #:update-datastore? #t
            #:pid-file #$pid-file
            #:agent-communication-uri (string->uri
                                       #$agent-communication-uri-string)
            #:client-communication-uri (string->uri
                                        #$client-communication-uri-string)))))
   #:guile guile))

(define (guix-build-coordinator-shepherd-services config)
  (match-record config <guix-build-coordinator-configuration>
    (package user group database-uri-string
             agent-communication-uri-string
             client-communication-uri-string
             allocation-strategy
             hooks
             guile)
    (list
     (shepherd-service
      (documentation "Guix Build Coordinator")
      (provision '(guix-build-coordinator))
      (requirement '(networking))
      (start #~(make-forkexec-constructor
                (list #$(make-guix-build-coordinator-start-script
                         database-uri-string
                         allocation-strategy
                         "/var/run/guix-build-coordinator/pid"
                         package
                         #:agent-communication-uri-string
                         agent-communication-uri-string
                         #:client-communication-uri-string
                         client-communication-uri-string
                         #:hooks hooks
                         #:guile guile))
                #:user #$user
                #:group #$group
                #:pid-file "/var/run/guix-build-coordinator/pid"
                ;; Allow time for migrations to run
                #:pid-file-timeout 60
                #:environment-variables
                `(,(string-append
                    "GUIX_LOCPATH=" #$glibc-utf8-locales "/lib/locale")
                  "LC_ALL=en_US.utf8"
                  "PATH=/run/current-system/profile/bin") ; for hooks
                #:log-file "/var/log/guix-build-coordinator/coordinator.log"))
      (stop #~(make-kill-destructor))))))

(define (guix-build-coordinator-activation config)
  #~(begin
      (use-modules (guix build utils))

      (define %user
        (getpw #$(guix-build-coordinator-configuration-user
                  config)))

      (chmod "/var/lib/guix-build-coordinator" #o755)

      (mkdir-p "/var/log/guix-build-coordinator")

      ;; Allow writing the PID file
      (mkdir-p "/var/run/guix-build-coordinator")
      (chown "/var/run/guix-build-coordinator"
             (passwd:uid %user)
             (passwd:gid %user))))

(define (guix-build-coordinator-account config)
  (match-record config <guix-build-coordinator-configuration>
    (user group)
    (list (user-group
           (name group)
           (system? #t))
          (user-account
           (name user)
           (group group)
           (system? #t)
           (comment "Guix Build Coordinator user")
           (home-directory "/var/lib/guix-build-coordinator")
           (shell (file-append shadow "/sbin/nologin"))))))

(define guix-build-coordinator-service-type
  (service-type
   (name 'guix-build-coordinator)
   (extensions
    (list
     (service-extension shepherd-root-service-type
                        guix-build-coordinator-shepherd-services)
     (service-extension activation-service-type
                        guix-build-coordinator-activation)
     (service-extension account-service-type
                        guix-build-coordinator-account)))
   (default-value
     (guix-build-coordinator-configuration))
   (description
    "Run an instance of the Guix Build Coordinator.")))

(define (guix-build-coordinator-agent-shepherd-services config)
  (match-record config <guix-build-coordinator-agent-configuration>
    (package user coordinator authentication max-parallel-builds
             max-1min-load-average
             derivation-substitute-urls non-derivation-substitute-urls
             systems)
    (list
     (shepherd-service
      (documentation "Guix Build Coordinator Agent")
      (provision '(guix-build-coordinator-agent))
      (requirement '(networking))
      (start #~(make-forkexec-constructor
                (list #$(file-append package "/bin/guix-build-coordinator-agent")
                      #$(string-append "--coordinator=" coordinator)
                      #$@(match authentication
                           (($ <guix-build-coordinator-agent-password-auth>
                               uuid password)
                            #~(#$(string-append "--uuid=" uuid)
                               #$(string-append "--password=" password)))
                           (($ <guix-build-coordinator-agent-password-file-auth>
                               uuid password-file)
                            #~(#$(string-append "--uuid=" uuid)
                               #$(string-append "--password-file="
                                                password-file)))
                           (($ <guix-build-coordinator-agent-dynamic-auth>
                               agent-name token)
                            #~(#$(string-append "--name=" agent-name)
                               #$(string-append "--dynamic-auth-token=" token)))
                           (($
                             <guix-build-coordinator-agent-dynamic-auth-with-file>
                             agent-name token-file)
                            #~(#$(string-append "--name=" agent-name)
                               #$(string-append "--dynamic-auth-token-file="
                                                token-file))))
                      #$(simple-format #f "--max-parallel-builds=~A"
                                       max-parallel-builds)
                      #$@(if max-1min-load-average
                             #~(#$(simple-format #f "--max-1min-load-average=~A"
                                                 max-1min-load-average))
                             #~())
                      #$@(if derivation-substitute-urls
                             #~(#$(string-append
                                   "--derivation-substitute-urls="
                                 (string-join derivation-substitute-urls " ")))
                             #~())
                      #$@(if non-derivation-substitute-urls
                             #~(#$(string-append
                                   "--non-derivation-substitute-urls="
                                   (string-join non-derivation-substitute-urls " ")))
                             #~())
                      #$@(map (lambda (system)
                                (string-append "--system=" system))
                              (or systems '())))
                #:user #$user
                #:environment-variables
                `(,(string-append
                    "GUIX_LOCPATH=" #$glibc-utf8-locales "/lib/locale")
                  ;; XDG_CACHE_HOME is used by Guix when caching narinfo files
                  "XDG_CACHE_HOME=/var/cache/guix-build-coordinator-agent"
                  "LC_ALL=en_US.utf8")
                #:log-file "/var/log/guix-build-coordinator/agent.log"))
      (stop #~(make-kill-destructor))))))

(define (guix-build-coordinator-agent-activation config)
  #~(begin
      (use-modules (guix build utils))

      (define %user
        (getpw #$(guix-build-coordinator-agent-configuration-user
                  config)))

      (mkdir-p "/var/log/guix-build-coordinator")

      ;; Create a cache directory for storing narinfo files if downloaded
      (mkdir-p "/var/cache/guix-build-coordinator-agent")
      (chown "/var/cache/guix-build-coordinator-agent"
             (passwd:uid %user)
             (passwd:gid %user))))

(define (guix-build-coordinator-agent-account config)
  (list (user-account
         (name (guix-build-coordinator-agent-configuration-user config))
         (group "nogroup")
         (system? #t)
         (comment "Guix Build Coordinator agent user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define guix-build-coordinator-agent-service-type
  (service-type
   (name 'guix-build-coordinator-agent)
   (extensions
    (list
     (service-extension shepherd-root-service-type
                        guix-build-coordinator-agent-shepherd-services)
     (service-extension activation-service-type
                        guix-build-coordinator-agent-activation)
     (service-extension account-service-type
                        guix-build-coordinator-agent-account)))
   (description
    "Run a Guix Build Coordinator agent.")))

(define (guix-build-coordinator-queue-builds-shepherd-services config)
  (match-record config <guix-build-coordinator-queue-builds-configuration>
    (package user coordinator systems systems-and-targets
             guix-data-service processed-commits-file)
    (list
     (shepherd-service
      (documentation "Guix Build Coordinator queue builds from Guix Data Service")
      (provision '(guix-build-coordinator-queue-builds))
      (requirement '(networking))
      (start
       #~(make-forkexec-constructor
          (list
           #$(file-append
              package
              "/bin/guix-build-coordinator-queue-builds-from-guix-data-service")
           #$(string-append "--coordinator=" coordinator)
           #$@(map (lambda (system)
                     (string-append "--system=" system))
                   (or systems '()))
           #$@(map (match-lambda
                     ((system . target)
                      (string-append "--system-and-target=" system "=" target)))
                   (or systems-and-targets '()))
           #$@(if guix-data-service
                  #~(#$(string-append "--guix-data-service=" guix-data-service))
                  #~())
           #$@(if processed-commits-file
                  #~(#$(string-append "--processed-commits-file="
                                      processed-commits-file))
                  #~()))
          #:user #$user
          #:environment-variables
          `(,(string-append
              "GUIX_LOCPATH=" #$glibc-utf8-locales "/lib/locale")
            "LC_ALL=en_US.utf8")
          #:log-file "/var/log/guix-build-coordinator/queue-builds.log"))
      (stop #~(make-kill-destructor))))))

(define (guix-build-coordinator-queue-builds-activation config)
  #~(begin
      (use-modules (guix build utils))

      (define %user
        (getpw #$(guix-build-coordinator-queue-builds-configuration-user
                  config)))

      (mkdir-p "/var/log/guix-build-coordinator")

      ;; Allow writing the processed commits file
      (mkdir-p "/var/cache/guix-build-coordinator-queue-builds")
      (chown "/var/cache/guix-build-coordinator-queue-builds"
             (passwd:uid %user)
             (passwd:gid %user))))

(define (guix-build-coordinator-queue-builds-account config)
  (list (user-account
         (name (guix-build-coordinator-queue-builds-configuration-user config))
         (group "nogroup")
         (system? #t)
         (comment "Guix Build Coordinator queue-builds user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define guix-build-coordinator-queue-builds-service-type
  (service-type
   (name 'guix-build-coordinator-queue-builds)
   (extensions
    (list
     (service-extension shepherd-root-service-type
                        guix-build-coordinator-queue-builds-shepherd-services)
     (service-extension activation-service-type
                        guix-build-coordinator-queue-builds-activation)
     (service-extension account-service-type
                        guix-build-coordinator-queue-builds-account)))
   (description
    "Run the guix-build-coordinator-queue-builds-from-guix-data-service
script.

This is a script to assist in having the Guix Build Coordinator build
derivations stored in an instance of the Guix Data Service.")))


;;;
;;; Guix Data Service
;;;

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
                  "LC_ALL=en_US.UTF-8")
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
                  "LC_ALL=en_US.UTF-8")
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
