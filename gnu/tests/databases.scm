;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2020 Marius Bakke <marius@gnu.org>
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

(define-module (gnu tests databases)
  #:use-module (gnu tests)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system shadow)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services databases)
  #:use-module (gnu services networking)
  #:use-module (gnu packages databases)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:export (%test-memcached
            %test-postgresql
            %test-mysql))

(define %memcached-os
  (simple-operating-system
   (service dhcp-client-service-type)
   (service memcached-service-type)))

(define* (run-memcached-test #:optional (port 11211))
  "Run tests in %MEMCACHED-OS, forwarding PORT."
  (define os
    (marionette-operating-system
     %memcached-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings `((11211 . ,port)))))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-11) (srfi srfi-64)
                       (gnu build marionette)
                       (ice-9 rdelim))

          (define marionette
            (make-marionette (list #$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "memcached")

          ;; Wait for memcached to be up and running.
          (test-assert "service running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (match (start-service 'memcached)
                  (#f #f)
                  (('service response-parts ...)
                   (match (assq-ref response-parts 'running)
                     ((pid) (number? pid))))))
             marionette))

          (let* ((ai (car (getaddrinfo "localhost"
                                       #$(number->string port))))
                 (s  (socket (addrinfo:fam ai)
                             (addrinfo:socktype ai)
                             (addrinfo:protocol ai)))
                 (key "testkey")
                 (value "guix"))
            (connect s (addrinfo:addr ai))

            (test-equal "set"
              "STORED\r"
              (begin
                (simple-format s "set ~A 0 60 ~A\r\n~A\r\n"
                               key
                               (string-length value)
                               value)
                (read-line s)))

            (test-equal "get"
              (simple-format #f "VALUE ~A 0 ~A\r~A\r"
                             key
                             (string-length value)
                             value)
              (begin
                (simple-format s "get ~A\r\n" key)
                (string-append
                 (read-line s)
                 (read-line s))))

            (close-port s))

          ;; There should be a log file in here.
          (test-assert "log file"
            (marionette-eval
             '(file-exists? "/var/log/memcached")
             marionette))

          (test-end))))

  (gexp->derivation "memcached-test" test))

(define %test-memcached
  (system-test
   (name "memcached")
   (description "Connect to a running MEMCACHED server.")
   (value (run-memcached-test))))


;;;
;;; The PostgreSQL service.
;;;

(define %postgresql-log-directory
  "/var/log/postgresql")

(define %role-log-file
  "/var/log/postgresql_roles.log")

(define %postgresql-os
  (simple-operating-system
   (service postgresql-service-type
            (postgresql-configuration
             (postgresql postgresql-10)
             (config-file
              (postgresql-config-file
               (extra-config
                '(("session_preload_libraries" "auto_explain")
                  ("random_page_cost" 2)
                  ("auto_explain.log_min_duration" "100 ms")
                  ("work_mem" "500 MB")
                  ("debug_print_plan" #t)))))))
   (service postgresql-role-service-type
            (postgresql-role-configuration
             (roles
              (list (postgresql-role
                     (name "root")
                     (create-database? #t))))))))

(define (run-postgresql-test)
  "Run tests in %POSTGRESQL-OS."
  (define os
    (marionette-operating-system
     %postgresql-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
     (memory-size 512)))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-64)
                       (gnu build marionette))

          (define marionette
            (make-marionette (list #$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "postgresql")

          (test-assert "service running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'postgres))
             marionette))

          (test-assert "log-file"
            (marionette-eval
             '(begin
                (use-modules (ice-9 ftw)
                             (ice-9 match))
                (current-output-port
                 (open-file "/dev/console" "w0"))
                (let ((server-log-file
                       (string-append #$%postgresql-log-directory
                                      "/pg_ctl.log")))
                  (and (file-exists? server-log-file)
                       (display
                        (call-with-input-file server-log-file
                          get-string-all)))
                  #t))
             marionette))

          (test-assert "database ready"
            (begin
              (marionette-eval
               '(begin
                  (let loop ((i 10))
                    (unless (or (zero? i)
                                (and (file-exists? #$%role-log-file)
                                     (string-contains
                                      (call-with-input-file #$%role-log-file
                                        get-string-all)
                                      ";\nCREATE DATABASE")))
                      (sleep 1)
                      (loop (- i 1)))))
               marionette)))

          (test-assert "database creation"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd)
                             (ice-9 popen))
                (current-output-port
                 (open-file "/dev/console" "w0"))
                (let* ((port (open-pipe*
                              OPEN_READ
                              #$(file-append postgresql "/bin/psql")
                              "-tA" "-c" "SELECT 1 FROM pg_database WHERE
 datname='root'"))
                       (output (get-string-all port)))
                  (close-pipe port)
                  (string-contains output "1")))
             marionette))

          (test-end))))

  (gexp->derivation "postgresql-test" test))

(define %test-postgresql
  (system-test
   (name "postgresql")
   (description "Start the PostgreSQL service.")
   (value (run-postgresql-test))))


;;;
;;; The MySQL service.
;;;

(define %mysql-os
  (simple-operating-system
   (service mysql-service-type)))

(define* (run-mysql-test)
  "Run tests in %MYSQL-OS."
  (define os
    (marionette-operating-system
     %mysql-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
     (memory-size 512)))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-11) (srfi srfi-64)
                       (gnu build marionette))

          (define marionette
            (make-marionette (list #$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "mysql")

          (test-assert "service running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (match (start-service 'mysql)
                  (#f #f)
                  (('service response-parts ...)
                   (match (assq-ref response-parts 'running)
                     ((pid) (number? pid))))))
             marionette))

          (test-assert "mysql_upgrade completed"
            (wait-for-file "/var/lib/mysql/mysql_upgrade_info" marionette))

          (test-eq "create database"
            0
            (marionette-eval
             '(begin
                (system* #$(file-append mariadb "/bin/mysql")
                         "-e" "CREATE DATABASE guix;"))
             marionette))

          (test-eq "create table"
            0
            (marionette-eval
             '(begin
                (system*
                 #$(file-append mariadb "/bin/mysql") "guix"
                 "-e" "CREATE TABLE facts (id INT, data VARCHAR(12));"))
             marionette))

          (test-eq "insert data"
            0
            (marionette-eval
             '(begin
                (system* #$(file-append mariadb "/bin/mysql") "guix"
                         "-e" "INSERT INTO facts VALUES (1, 'awesome')"))
             marionette))

          (test-equal "retrieve data"
            "awesome\n"
            (marionette-eval
             '(begin
                (use-modules (ice-9 popen))
                (let* ((port (open-pipe*
                              OPEN_READ
                              #$(file-append mariadb "/bin/mysql") "guix"
                              "-NB" "-e" "SELECT data FROM facts WHERE id=1;"))
                       (output (get-string-all port)))
                  (close-pipe port)
                  output))
             marionette))

          (test-end))))

  (gexp->derivation "mysql-test" test))

(define %test-mysql
  (system-test
   (name "mysql")
   (description "Start the MySQL service.")
   (value (run-mysql-test))))
