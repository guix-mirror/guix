;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Christopher Baines <mail@cbaines.net>
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

(define-module (gnu tests guix)
  #:use-module (gnu tests)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system shadow)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services guix)
  #:use-module (gnu services databases)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services networking)
  #:use-module (gnu packages databases)
  #:use-module (guix packages)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:export (%test-guix-build-coordinator
            %test-guix-data-service))

;;;
;;; Guix Build Coordinator
;;;

(define %guix-build-coordinator-os
  (simple-operating-system
   (service dhcp-client-service-type)
   (service guix-build-coordinator-service-type)))

(define (run-guix-build-coordinator-test)
  (define os
    (marionette-operating-system
     %guix-build-coordinator-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define forwarded-port 8745)

  (define vm
    (virtual-machine
     (operating-system os)
     (memory-size 1024)
     (port-forwardings `((,forwarded-port . 8745)))))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-11) (srfi srfi-64)
                       (gnu build marionette)
                       (web uri)
                       (web client)
                       (web response))

          (define marionette
            (make-marionette (list #$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "guix-build-coordinator")

          (test-assert "service running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (match (start-service 'guix-build-coordinator)
                  (#f #f)
                  (('service response-parts ...)
                   (match (assq-ref response-parts 'running)
                     ((pid) (number? pid))))))
             marionette))

          (test-equal "http-get"
            200
            (let-values
                (((response text)
                  (http-get #$(simple-format
                               #f "http://localhost:~A/metrics" forwarded-port)
                            #:decode-body? #t)))
              (response-code response)))

          (test-end))))

  (gexp->derivation "guix-build-coordinator-test" test))

(define %test-guix-build-coordinator
  (system-test
   (name "guix-build-coordinator")
   (description "Connect to a running Guix Build Coordinator.")
   (value (run-guix-build-coordinator-test))))


;;;
;;; Guix Data Service
;;;

(define guix-data-service-initial-database-setup-service
  (let ((user "guix_data_service")
        (name "guix_data_service"))
    (define start-gexp
      #~(lambda ()
          (let ((pid (primitive-fork))
                (postgres (getpwnam "postgres")))
            (if (eq? pid 0)
                (dynamic-wind
                  (const #t)
                  (lambda ()
                    (setgid (passwd:gid postgres))
                    (setuid (passwd:uid postgres))
                    (primitive-exit
                     (if (and
                          (zero?
                           (system* #$(file-append postgresql "/bin/createuser")
                                    #$user))
                          (zero?
                           (system* #$(file-append postgresql "/bin/createdb")
                                    "-O" #$user #$name)))
                         0
                         1)))
                  (lambda ()
                    (primitive-exit 1)))
                (zero? (cdr (waitpid pid)))))))

    (shepherd-service
     (requirement '(postgres))
     (provision '(guix-data-service-initial-database-setup))
     (start start-gexp)
     (stop #~(const #f))
     (respawn? #f)
     (one-shot? #t)
     (documentation "Setup Guix Data Service database."))))

(define %guix-data-service-os
  (simple-operating-system
   (service dhcp-client-service-type)
   (service postgresql-service-type
            (postgresql-configuration
             (postgresql postgresql-10)
             (config-file
              (postgresql-config-file
               (hba-file
                (plain-file "pg_hba.conf"
                            "
local	all	all			trust
host	all	all	127.0.0.1/32 	trust
host	all	all	::1/128 	trust"))))))
   (service guix-data-service-type
            (guix-data-service-configuration
             (host "0.0.0.0")))
   (simple-service 'guix-data-service-database-setup
                   shepherd-root-service-type
                   (list guix-data-service-initial-database-setup-service))))

(define (run-guix-data-service-test)
  (define os
    (marionette-operating-system
     %guix-data-service-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define forwarded-port 8080)

  (define vm
    (virtual-machine
     (operating-system os)
     (memory-size 1024)
     (port-forwardings `((,forwarded-port . 8765)))))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-11) (srfi srfi-64)
                       (gnu build marionette)
                       (web uri)
                       (web client)
                       (web response))

          (define marionette
            (make-marionette (list #$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "guix-data-service")

          (test-assert "service running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (match (start-service 'guix-data-service)
                  (#f #f)
                  (('service response-parts ...)
                   (match (assq-ref response-parts 'running)
                     ((pid) (number? pid))))))
             marionette))

          (test-assert "process jobs service running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (match (start-service 'guix-data-service-process-jobs)
                  (#f #f)
                  (('service response-parts ...)
                   (match (assq-ref response-parts 'running)
                     ((pid) (number? pid))))))
             marionette))

          (test-equal "http-get"
            200
            (let-values
                (((response text)
                  (http-get #$(simple-format
                               #f "http://localhost:~A/healthcheck" forwarded-port)
                            #:decode-body? #t)))
              (response-code response)))

          (test-end))))

  (gexp->derivation "guix-data-service-test" test))

(define %test-guix-data-service
  (system-test
   (name "guix-data-service")
   (description "Connect to a running Guix Data Service.")
   (value (run-guix-data-service-test))))
