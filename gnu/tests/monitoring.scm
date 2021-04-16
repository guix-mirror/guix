;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Gábor Boskovits  <boskovits@gmail.com>
;;; Copyright © 2018, 2019 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (gnu tests monitoring)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages monitoring)
  #:use-module (gnu packages php)
  #:use-module (gnu services)
  #:use-module (gnu services monitoring)
  #:use-module (gnu services networking)
  #:use-module (gnu services databases)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services web)
  #:use-module (gnu system vm)
  #:use-module (gnu system)
  #:use-module (gnu tests)
  #:use-module (guix gexp)
  #:export (%test-prometheus-node-exporter
            %test-zabbix))


;;;
;;; Prometheus Node Exporter
;;;

(define* (run-prometheus-node-exporter-server-test name test-os)
  "Run tests in %PROMETHEUS-NODE-EXPORTER-OS, which has prometheus-node-exporter running."
  (define os
    (marionette-operating-system
     test-os
     #:imported-modules '((gnu services herd))))

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings '((8080 . 9100)))))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-11)
                       (srfi srfi-64)
                       (gnu build marionette)
                       (web client)
                       (web response))

          (define marionette
            (make-marionette (list #$vm)))

          (mkdir #$output)
          (chdir #$output)

          (test-begin #$name)

          (test-assert "prometheus-node-exporter running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (match (start-service 'prometheus-node-exporter)
                  (#f #f)
                  (('service response-parts ...)
                   (match (assq-ref response-parts 'running)
                     ((pid) (number? pid))))))
             marionette))

          (test-equal "http-get"
            200
            (begin
              (wait-for-tcp-port 9100 marionette)
              (let-values (((response text)
                            (http-get "http://localhost:8080")))
                (response-code response))))

          (test-end)
          (exit (= (test-runner-fail-count (test-runner-current)) 0)))))

  (gexp->derivation (string-append name "-test") test))

(define %prometheus-node-exporter-os
  (simple-operating-system
   (service dhcp-client-service-type)
   (service prometheus-node-exporter-service-type
            (prometheus-node-exporter-configuration))))

(define %test-prometheus-node-exporter
  (system-test
   (name "prometheus-node-exporter")
   (description "Connect to a running prometheus-node-exporter server.")
   (value (run-prometheus-node-exporter-server-test
           name %prometheus-node-exporter-os))))


;;;
;;; Zabbix
;;;

(define %psql-user-create-zabbix
  "\
sudo -u postgres psql <<< \"create user zabbix password 'zabbix';\"
")

(define %psql-db-zabbix-create-script
  "\
sudo -u postgres psql --no-align <<< \\\\du
")

(define %psql-db-create-zabbix
  "\
sudo -u postgres createdb -O zabbix -E Unicode -T template0 zabbix
")

(define %psql-db-import-zabbix
  #~(format #f "\
cat ~a | sudo -u zabbix psql zabbix;
cat ~a | sudo -u zabbix psql zabbix;
cat ~a | sudo -u zabbix psql zabbix;
"
            (string-append #$zabbix-server:schema
                           "/database/postgresql/schema.sql")
            (string-append #$zabbix-server:schema
                           "/database/postgresql/images.sql")
            (string-append #$zabbix-server:schema
                           "/database/postgresql/data.sql")))

(define* (run-zabbix-server-test name test-os)
  "Run tests in %ZABBIX-OS, which has zabbix running."
  (define os
    (marionette-operating-system
     test-os
     #:imported-modules '((gnu services herd))))

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings '((8080 . 80)))
     (memory-size 1024)))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-11)
                       (srfi srfi-64)
                       (gnu build marionette)
                       (web client)
                       (web response)
                       (ice-9 popen)
                       (ice-9 rdelim))

          (define marionette
            (make-marionette (list #$vm)))

          (mkdir #$output)
          (chdir #$output)

          (test-begin #$name)

          ;; XXX: Shepherd reads the config file *before* binding its control
          ;; socket, so /var/run/shepherd/socket might not exist yet when the
          ;; 'marionette' service is started.
          (test-assert "shepherd socket ready"
            (marionette-eval
             `(begin
                (use-modules (gnu services herd))
                (let loop ((i 10))
                  (cond ((file-exists? (%shepherd-socket-file))
                         #t)
                        ((> i 0)
                         (sleep 1)
                         (loop (- i 1)))
                        (else
                         'failure))))
             marionette))

          (test-assert "postgres service running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'postgres))
             marionette))

          ;; Add /run/setuid-programs to $PATH so that the scripts passed to
          ;; 'system' can find 'sudo'.
          (marionette-eval
           '(setenv "PATH"
                    "/run/setuid-programs:/run/current-system/profile/bin")
           marionette)

          (test-eq "postgres create zabbix user"
            0
            (marionette-eval '(begin (system #$%psql-user-create-zabbix))
                             marionette))

          (test-equal "postgres find zabbix user"
            "List of roles
Role name|Attributes|Member of
postgres|Superuser, Create role, Create DB, Replication, Bypass RLS|{}
zabbix||{}
"
            (marionette-eval
             '(begin (let* ((port (open-pipe #$%psql-db-zabbix-create-script
                                             OPEN_READ))
                            (output (read-string port))
                            (status (close-pipe port)))
                       output))
             marionette))

          (test-eq "postgres create zabbix db"
            0
            (marionette-eval '(begin (system #$%psql-db-create-zabbix))
                             marionette))

          (test-eq "postgres import zabbix db"
            0
            (marionette-eval '(begin (system #$%psql-db-import-zabbix))
                             marionette))

          ;; Wait for zabbix-server to be up and running.
          (test-assert "zabbix-server running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'zabbix-server))
             marionette))

          ;; Make sure the PID file is created.
          (test-assert "zabbix-server PID file"
            (marionette-eval
             '(file-exists? "/var/run/zabbix/zabbix_server.pid")
             marionette))

          ;; Wait for zabbix-agent to be up and running.
          (test-assert "zabbix-agent running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'zabbix-agent))
             marionette))

          ;; Make sure the PID file is created.
          (test-assert "zabbix-agent PID file"
            (marionette-eval
             '(file-exists? "/var/run/zabbix/zabbix_agent.pid")
             marionette))

          ;; Wait for php-fpm to be up and running.
          (test-assert "php-fpm running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'php-fpm))
             marionette))

          ;; Wait for nginx to be up and running.
          (test-assert "nginx running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'nginx))
             marionette))

          ;; Make sure the PID file is created.
          (test-assert "nginx PID file"
            (marionette-eval
             '(file-exists? "/var/run/nginx/pid")
             marionette))

          ;; Make sure we can access pages that correspond to our Zabbix instance.
          (letrec-syntax ((test-url
                           (syntax-rules ()
                             ((_ path code)
                              (test-equal (string-append "GET " path)
                                code
                                (let-values (((response body)
                                              (http-get (string-append
                                                         "http://localhost:8080"
                                                         path))))
                                  (response-code response))))
                             ((_ path)
                              (test-url path 200)))))
            (test-url "/")
            (test-url "/does-not-exist" 404))

          (test-end)

          (exit (= (test-runner-fail-count (test-runner-current)) 0)))))

  (gexp->derivation (string-append name "-test") test))

(define %zabbix-os
  ;; Return operating system under test.
  (let ((base-os
         (simple-operating-system
          (service dhcp-client-service-type)
          (service postgresql-service-type
                   (postgresql-configuration
                    (postgresql postgresql)))
          (service zabbix-front-end-service-type
                   (zabbix-front-end-configuration
                    (db-password "zabbix")))

          (service php-fpm-service-type
                   (php-fpm-configuration
                    (timezone "Europe/Paris")))

          (service zabbix-server-service-type
                   (zabbix-server-configuration
                    (db-password "zabbix")
                    (log-type "console")))

          (service zabbix-agent-service-type))))
    (operating-system
      (inherit base-os)
      (packages (cons* postgresql (operating-system-packages base-os))))))

(define %test-zabbix
  (system-test
   (name "zabbix")
   (description "Connect to a running Zabbix")
   (value (run-zabbix-server-test name %zabbix-os))))
