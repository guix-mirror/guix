;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2015, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 Christopher Baines <mail@cbaines.net>
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

(define-module (gnu services databases)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages databases)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:export (postgresql-configuration
            postgresql-configuration?
            postgresql-service
            postgresql-service-type

            memcached-service-type
            <memcached-configuration>
            memcached-configuration
            memcached-configuration?
            memcached-configuration-memecached
            memcached-configuration-interfaces
            memcached-configuration-tcp-port
            memcached-configuration-udp-port
            memcached-configuration-additional-options

            <mongodb-configuration>
            mongodb-configuration
            mongodb-configuration?
            mongodb-configuration-mongodb
            mongodb-configuration-config-file
            mongodb-configuration-data-directory
            mongodb-service-type

            mysql-service
            mysql-service-type
            mysql-configuration
            mysql-configuration?

            redis-configuration
            redis-configuration?
            redis-service-type))

;;; Commentary:
;;;
;;; Database services.
;;;
;;; Code:

(define-record-type* <postgresql-configuration>
  postgresql-configuration make-postgresql-configuration
  postgresql-configuration?
  (postgresql     postgresql-configuration-postgresql ;<package>
                  (default postgresql))
  (port           postgresql-configuration-port
                  (default 5432))
  (locale         postgresql-configuration-locale
                  (default "en_US.utf8"))
  (config-file    postgresql-configuration-file)
  (data-directory postgresql-configuration-data-directory))

(define %default-postgres-hba
  (plain-file "pg_hba.conf"
              "
local	all	all			trust
host	all	all	127.0.0.1/32 	trust
host	all	all	::1/128 	trust"))

(define %default-postgres-ident
  (plain-file "pg_ident.conf"
             "# MAPNAME       SYSTEM-USERNAME         PG-USERNAME"))

(define %default-postgres-config
  (mixed-text-file "postgresql.conf"
                   "log_destination = 'syslog'\n"
                   "hba_file = '" %default-postgres-hba "'\n"
                   "ident_file = '" %default-postgres-ident "'\n"))

(define %postgresql-accounts
  (list (user-group (name "postgres") (system? #t))
        (user-account
         (name "postgres")
         (group "postgres")
         (system? #t)
         (comment "PostgreSQL server user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define postgresql-activation
  (match-lambda
    (($ <postgresql-configuration> postgresql port locale config-file data-directory)
     #~(begin
         (use-modules (guix build utils)
                      (ice-9 match))

         (let ((user (getpwnam "postgres"))
               (initdb (string-append #$postgresql "/bin/initdb"))
               (initdb-args
                (append
                 (if #$locale
                     (list (string-append "--locale=" #$locale))
                     '()))))
           ;; Create db state directory.
           (mkdir-p #$data-directory)
           (chown #$data-directory (passwd:uid user) (passwd:gid user))

           ;; Drop privileges and init state directory in a new
           ;; process.  Wait for it to finish before proceeding.
           (match (primitive-fork)
             (0
              ;; Exit with a non-zero status code if an exception is thrown.
              (dynamic-wind
                (const #t)
                (lambda ()
                  (setgid (passwd:gid user))
                  (setuid (passwd:uid user))
                  (primitive-exit
                   (apply system*
                          initdb
                          "-D"
                          #$data-directory
                          initdb-args)))
                (lambda ()
                  (primitive-exit 1))))
             (pid (waitpid pid))))))))

(define postgresql-shepherd-service
  (match-lambda
    (($ <postgresql-configuration> postgresql port locale config-file data-directory)
     (let ((start-script
            ;; Wrapper script that switches to the 'postgres' user before
            ;; launching daemon.
            (program-file "start-postgres"
                          #~(let ((user (getpwnam "postgres"))
                                  (postgres (string-append #$postgresql
                                                           "/bin/postgres")))
                              (setgid (passwd:gid user))
                              (setuid (passwd:uid user))
                              (system* postgres
                                       (string-append "--config-file="
                                                      #$config-file)
                                       "-p" (number->string #$port)
                                       "-D" #$data-directory)))))
       (list (shepherd-service
              (provision '(postgres))
              (documentation "Run the PostgreSQL daemon.")
              (requirement '(user-processes loopback syslogd))
              (start #~(make-forkexec-constructor #$start-script))
              (stop #~(make-kill-destructor))))))))

(define postgresql-service-type
  (service-type (name 'postgresql)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          postgresql-shepherd-service)
                       (service-extension activation-service-type
                                          postgresql-activation)
                       (service-extension account-service-type
                                          (const %postgresql-accounts))))))

(define* (postgresql-service #:key (postgresql postgresql)
                             (port 5432)
                             (locale "en_US.utf8")
                             (config-file %default-postgres-config)
                             (data-directory "/var/lib/postgresql/data"))
  "Return a service that runs @var{postgresql}, the PostgreSQL database server.

The PostgreSQL daemon loads its runtime configuration from @var{config-file}
and stores the database cluster in @var{data-directory}."
  (service postgresql-service-type
           (postgresql-configuration
            (postgresql postgresql)
            (port port)
            (locale locale)
            (config-file config-file)
            (data-directory data-directory))))


;;;
;;; Memcached
;;;

(define-record-type* <memcached-configuration>
  memcached-configuration make-memcached-configuration
  memcached-configuration?
  (memcached          memcached-configuration-memcached ;<package>
                      (default memcached))
  (interfaces         memcached-configuration-interfaces
                      (default '("0.0.0.0")))
  (tcp-port           memcached-configuration-tcp-port
                      (default 11211))
  (udp-port           memcached-configuration-udp-port
                      (default 11211))
  (additional-options memcached-configuration-additional-options
                      (default '())))

(define %memcached-accounts
  (list (user-group (name "memcached") (system? #t))
        (user-account
         (name "memcached")
         (group "memcached")
         (system? #t)
         (comment "Memcached server user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define memcached-activation
  #~(begin
      (use-modules (guix build utils))
      (let ((user (getpwnam "memcached")))
        (mkdir-p "/var/run/memcached")
        (chown "/var/run/memcached"
               (passwd:uid user) (passwd:gid user)))))

(define memcached-shepherd-service
  (match-lambda
    (($ <memcached-configuration> memcached interfaces tcp-port udp-port
                                  additional-options)
     (with-imported-modules (source-module-closure
                             '((gnu build shepherd)))
       (list (shepherd-service
              (provision '(memcached))
              (documentation "Run the Memcached daemon.")
              (requirement '(user-processes loopback))
              (modules '((gnu build shepherd)))
              (start #~(make-forkexec-constructor
                        `(#$(file-append memcached "/bin/memcached")
                          "-l" #$(string-join interfaces ",")
                          "-p" #$(number->string tcp-port)
                          "-U" #$(number->string udp-port)
                          "--daemon"
                          ;; Memcached changes to the memcached user prior to
                          ;; writing the pid file, so write it to a directory
                          ;; that memcached owns.
                          "-P" "/var/run/memcached/pid"
                          "-u" "memcached"
                          ,#$@additional-options)
                        #:log-file "/var/log/memcached"
                        #:pid-file "/var/run/memcached/pid"))
              (stop #~(make-kill-destructor))))))))

(define memcached-service-type
  (service-type (name 'memcached)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          memcached-shepherd-service)
                       (service-extension activation-service-type
                                          (const memcached-activation))
                       (service-extension account-service-type
                                          (const %memcached-accounts))))
                (default-value (memcached-configuration))))


;;;
;;; MongoDB
;;;

(define %default-mongodb-configuration-file
  (plain-file
   "mongodb.yaml"
   "# GNU Guix: MongoDB default configuration file
processManagement:
  pidFilePath: /var/run/mongodb/pid
storage:
  dbPath: /var/lib/mongodb
"))


(define-record-type* <mongodb-configuration>
  mongodb-configuration make-mongodb-configuration
  mongodb-configuration?
  (mongodb             mongodb-configuration-mongodb
                       (default mongodb))
  (config-file         mongodb-configuration-config-file
                       (default %default-mongodb-configuration-file))
  (data-directory      mongodb-configuration-data-directory
                       (default "/var/lib/mongodb")))

(define %mongodb-accounts
  (list (user-group (name "mongodb") (system? #t))
        (user-account
         (name "mongodb")
         (group "mongodb")
         (system? #t)
         (comment "Mongodb server user")
         (home-directory "/var/lib/mongodb")
         (shell (file-append shadow "/sbin/nologin")))))

(define mongodb-activation
  (match-lambda
    (($ <mongodb-configuration> mongodb config-file data-directory)
     #~(begin
         (use-modules (guix build utils))
         (let ((user (getpwnam "mongodb")))
           (for-each
            (lambda (directory)
              (mkdir-p directory)
              (chown directory
                     (passwd:uid user) (passwd:gid user)))
            '("/var/run/mongodb" #$data-directory)))))))

(define mongodb-shepherd-service
  (match-lambda
    (($ <mongodb-configuration> mongodb config-file data-directory)
     (shepherd-service
      (provision '(mongodb))
      (documentation "Run the Mongodb daemon.")
      (requirement '(user-processes loopback))
      (start #~(make-forkexec-constructor
                `(,(string-append #$mongodb "/bin/mongod")
                  "--config"
                  ,#$config-file)
                #:user "mongodb"
                #:group "mongodb"
                #:pid-file "/var/run/mongodb/pid"
                #:log-file "/var/log/mongodb.log"))
      (stop #~(make-kill-destructor))))))

(define mongodb-service-type
  (service-type
   (name 'mongodb)
   (description "Run the MongoDB document database server.")
   (extensions
    (list (service-extension shepherd-root-service-type
                             (compose list
                                      mongodb-shepherd-service))
          (service-extension activation-service-type
                             mongodb-activation)
          (service-extension account-service-type
                             (const %mongodb-accounts))))
   (default-value
     (mongodb-configuration))))


;;;
;;; MySQL.
;;;

(define-record-type* <mysql-configuration>
  mysql-configuration make-mysql-configuration
  mysql-configuration?
  (mysql mysql-configuration-mysql (default mariadb))
  (port mysql-configuration-port (default 3306)))

(define %mysql-accounts
  (list (user-group
         (name "mysql")
         (system? #t))
        (user-account
         (name "mysql")
         (group "mysql")
         (system? #t)
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define mysql-configuration-file
  (match-lambda
    (($ <mysql-configuration> mysql port)
     (mixed-text-file "my.cnf" "[mysqld]
datadir=/var/lib/mysql
socket=/run/mysqld/mysqld.sock
port=" (number->string port) "
"))))

(define (%mysql-activation config)
  "Return an activation gexp for the MySQL or MariaDB database server."
  (let ((mysql  (mysql-configuration-mysql config))
        (my.cnf (mysql-configuration-file config)))
    #~(begin
        (use-modules (ice-9 popen)
                     (guix build utils))
        (let* ((mysqld  (string-append #$mysql "/bin/mysqld"))
               (user    (getpwnam "mysql"))
               (uid     (passwd:uid user))
               (gid     (passwd:gid user))
               (datadir "/var/lib/mysql")
               (rundir  "/run/mysqld"))
          (mkdir-p datadir)
          (chown datadir uid gid)
          (mkdir-p rundir)
          (chown rundir uid gid)
          ;; Initialize the database when it doesn't exist.
          (when (not (file-exists? (string-append datadir "/mysql")))
            (if (string-prefix? "mysql-" (strip-store-file-name #$mysql))
                ;; For MySQL.
                (system* mysqld
                         (string-append "--defaults-file=" #$my.cnf)
                         "--initialize"
                         "--user=mysql")
                ;; For MariaDB.
                ;; XXX: The 'mysql_install_db' script doesn't work directly
                ;;      due to missing 'mkdir' in PATH.
                (let ((p (open-pipe* OPEN_WRITE mysqld
                                     (string-append
                                      "--defaults-file=" #$my.cnf)
                                     "--bootstrap"
                                     "--user=mysql")))
                  ;; Create the system database, as does by 'mysql_install_db'.
                  (display "create database mysql;\n" p)
                  (display "use mysql;\n" p)
                  (for-each
                   (lambda (sql)
                     (call-with-input-file
                         (string-append #$mysql "/share/mysql/" sql)
                       (lambda (in) (dump-port in p))))
                   '("mysql_system_tables.sql"
                     "mysql_performance_tables.sql"
                     "mysql_system_tables_data.sql"
                     "fill_help_tables.sql"))
                  ;; Remove the anonymous user and disable root access from
                  ;; remote machines, as does by 'mysql_secure_installation'.
                  (display "
DELETE FROM user WHERE User='';
DELETE FROM user WHERE User='root' AND
  Host NOT IN  ('localhost', '127.0.0.1', '::1');
FLUSH PRIVILEGES;
" p)
                  (close-pipe p))))))))

(define (mysql-shepherd-service config)
  (list (shepherd-service
         (provision '(mysql))
         (documentation "Run the MySQL server.")
         (start (let ((mysql  (mysql-configuration-mysql config))
                      (my.cnf (mysql-configuration-file config)))
                  #~(make-forkexec-constructor
                     (list (string-append #$mysql "/bin/mysqld")
                           (string-append "--defaults-file=" #$my.cnf))
                     #:user "mysql" #:group "mysql")))
         (stop #~(make-kill-destructor)))))

(define mysql-service-type
  (service-type
   (name 'mysql)
   (extensions
    (list (service-extension account-service-type
                             (const %mysql-accounts))
          (service-extension activation-service-type
                             %mysql-activation)
          (service-extension shepherd-root-service-type
                             mysql-shepherd-service)))))

(define* (mysql-service #:key (config (mysql-configuration)))
  "Return a service that runs @command{mysqld}, the MySQL or MariaDB
database server.

The optional @var{config} argument specifies the configuration for
@command{mysqld}, which should be a @code{<mysql-configuration>} object."
  (service mysql-service-type config))


;;;
;;; Redis
;;;

(define-record-type* <redis-configuration>
  redis-configuration make-redis-configuration
  redis-configuration?
  (redis             redis-configuration-redis ;<package>
                     (default redis))
  (bind              redis-configuration-bind
                     (default "127.0.0.1"))
  (port              redis-configuration-port
                     (default 6379))
  (working-directory redis-configuration-working-directory
                     (default "/var/lib/redis"))
  (config-file       redis-configuration-config-file
                     (default #f)))

(define (default-redis.conf bind port working-directory)
  (mixed-text-file "redis.conf"
                   "bind " bind "\n"
                   "port " (number->string port) "\n"
                   "dir " working-directory "\n"
                   "daemonize no\n"))

(define %redis-accounts
  (list (user-group (name "redis") (system? #t))
        (user-account
         (name "redis")
         (group "redis")
         (system? #t)
         (comment "Redis server user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define redis-activation
  (match-lambda
    (($ <redis-configuration> redis bind port working-directory config-file)
     #~(begin
         (use-modules (guix build utils)
                      (ice-9 match))

         (let ((user (getpwnam "redis")))
           (mkdir-p #$working-directory)
           (chown #$working-directory (passwd:uid user) (passwd:gid user)))))))

(define redis-shepherd-service
  (match-lambda
    (($ <redis-configuration> redis bind port working-directory config-file)
     (let ((config-file
            (or config-file
                (default-redis.conf bind port working-directory))))
       (list (shepherd-service
              (provision '(redis))
              (documentation "Run the Redis daemon.")
              (requirement '(user-processes syslogd))
              (start #~(make-forkexec-constructor
                        '(#$(file-append redis "/bin/redis-server")
                          #$config-file)
                        #:user "redis"
                        #:group "redis"))
              (stop #~(make-kill-destructor))))))))

(define redis-service-type
  (service-type (name 'redis)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          redis-shepherd-service)
                       (service-extension activation-service-type
                                          redis-activation)
                       (service-extension account-service-type
                                          (const %redis-accounts))))))
