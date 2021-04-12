;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2015, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2018 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2019 Robert Vollmert <rob@vllmrt.net>
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

(define-module (gnu services databases)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages databases)
  #:use-module (guix build-system trivial)
  #:use-module (guix build union)
  #:use-module (guix deprecation)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (postgresql-config-file
            postgresql-config-file?
            postgresql-config-file-log-destination
            postgresql-config-file-hba-file
            postgresql-config-file-ident-file
            postgresql-config-file-socket-directory
            postgresql-config-file-extra-config

            postgresql-configuration
            postgresql-configuration?
            postgresql-configuration-postgresql
            postgresql-configuration-port
            postgresql-configuration-locale
            postgresql-configuration-file
            postgresql-configuration-log-directory
            postgresql-configuration-data-directory

            postgresql-service
            postgresql-service-type

            postgresql-role
            postgresql-role?
            postgresql-role-name
            postgresql-role-permissions
            postgresql-role-create-database?
            postgresql-role-configuration
            postgresql-role-configuration?
            postgresql-role-configuration-host
            postgresql-role-configuration-roles

            postgresql-role-service-type

            memcached-service-type
            memcached-configuration
            memcached-configuration?
            memcached-configuration-memecached
            memcached-configuration-interfaces
            memcached-configuration-tcp-port
            memcached-configuration-udp-port
            memcached-configuration-additional-options

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

(define %default-postgres-hba
  (plain-file "pg_hba.conf"
              "
local	all	all			peer
host	all	all	127.0.0.1/32 	md5
host	all	all	::1/128 	md5"))

(define %default-postgres-ident
  (plain-file "pg_ident.conf"
              "# MAPNAME       SYSTEM-USERNAME         PG-USERNAME"))

(define-record-type* <postgresql-config-file>
  postgresql-config-file make-postgresql-config-file
  postgresql-config-file?
  (log-destination   postgresql-config-file-log-destination
                     (default "syslog"))
  (hba-file          postgresql-config-file-hba-file
                     (default %default-postgres-hba))
  (ident-file        postgresql-config-file-ident-file
                     (default %default-postgres-ident))
  (socket-directory  postgresql-config-file-socket-directory
                     (default #false))
  (extra-config      postgresql-config-file-extra-config
                     (default '())))

(define-gexp-compiler (postgresql-config-file-compiler
                       (file <postgresql-config-file>) system target)
  (match file
    (($ <postgresql-config-file> log-destination hba-file
                                 ident-file socket-directory
                                 extra-config)
     ;; See: https://www.postgresql.org/docs/current/config-setting.html.
    (define (format-value value)
      (cond
       ((boolean? value)
        (list (if value "on" "off")))
       ((number? value)
        (list (number->string value)))
       (else
        (list "'" value "'"))))

    (define contents
      (append-map
       (match-lambda
         ((key) '())
         ((key . #f) '())
         ((key values ...)
          `(,key " = " ,@(append-map format-value values) "\n")))

       `(("log_destination" ,log-destination)
         ("hba_file" ,hba-file)
         ("ident_file" ,ident-file)
         ,@(if socket-directory
               `(("unix_socket_directories" ,socket-directory))
               '())
         ,@extra-config)))

     (gexp->derivation
      "postgresql.conf"
      #~(call-with-output-file (ungexp output "out")
          (lambda (port)
            (display
             (string-append #$@contents)
             port)))
      #:local-build? #t))))

(define-record-type* <postgresql-configuration>
  postgresql-configuration make-postgresql-configuration
  postgresql-configuration?
  (postgresql         postgresql-configuration-postgresql) ;<package>
  (port               postgresql-configuration-port
                      (default 5432))
  (locale             postgresql-configuration-locale
                      (default "en_US.utf8"))
  (config-file        postgresql-configuration-file
                      (default (postgresql-config-file)))
  (log-directory      postgresql-configuration-log-directory
                      (default "/var/log/postgresql"))
  (data-directory     postgresql-configuration-data-directory
                      (default "/var/lib/postgresql/data"))
  (extension-packages postgresql-configuration-extension-packages
                      (default '())))

(define %postgresql-accounts
  (list (user-group (name "postgres") (system? #t))
        (user-account
         (name "postgres")
         (group "postgres")
         (system? #t)
         (comment "PostgreSQL server user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define (final-postgresql postgresql extension-packages)
  (if (null? extension-packages)
    postgresql
    (package
      (inherit postgresql)
      (source #f)
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils) (guix build union))
         #:builder
         (begin
           (use-modules (guix build utils) (guix build union) (srfi srfi-26))
           (union-build (assoc-ref %outputs "out")
                        (map (lambda (input) (cdr input))
                             %build-inputs))
           #t)))
      (inputs
       `(("postgresql" ,postgresql)
         ,@(map (lambda (extension) (list "extension" extension))
                extension-packages))))))

(define postgresql-activation
  (match-lambda
    (($ <postgresql-configuration> postgresql port locale config-file
                                   log-directory data-directory
                                   extension-packages)
     #~(begin
         (use-modules (guix build utils)
                      (ice-9 match))

         (let ((user (getpwnam "postgres"))
               (initdb (string-append
                        #$(final-postgresql postgresql
                                            extension-packages)
                        "/bin/initdb"))
               (initdb-args
                (append
                 (if #$locale
                     (list (string-append "--locale=" #$locale))
                     '()))))
           ;; Create db state directory.
           (mkdir-p #$data-directory)
           (chown #$data-directory (passwd:uid user) (passwd:gid user))

           ;; Create the socket directory.
           (let ((socket-directory
                  #$(postgresql-config-file-socket-directory config-file)))
             (when (string? socket-directory)
               (mkdir-p socket-directory)
               (chown socket-directory (passwd:uid user) (passwd:gid user))))

           ;; Create the log directory.
           (when (string? #$log-directory)
             (mkdir-p #$log-directory)
             (chown #$log-directory (passwd:uid user) (passwd:gid user)))

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
    (($ <postgresql-configuration> postgresql port locale config-file
                                   log-directory data-directory
                                   extension-packages)
     (let* ((pg_ctl-wrapper
             ;; Wrapper script that switches to the 'postgres' user before
             ;; launching daemon.
             (program-file
              "pg_ctl-wrapper"
              #~(begin
                  (use-modules (ice-9 match)
                               (ice-9 format))
                  (match (command-line)
                    ((_ mode)
                     (let ((user (getpwnam "postgres"))
                           (pg_ctl #$(file-append
                                      (final-postgresql postgresql
                                                        extension-packages)
                                                  "/bin/pg_ctl"))
                           (options (format #f "--config-file=~a -p ~d"
                                            #$config-file #$port)))
                       (setgid (passwd:gid user))
                       (setuid (passwd:uid user))
                       (execl pg_ctl pg_ctl "-D" #$data-directory
                              #$@(if (string? log-directory)
                                     (list "-l"
                                           (string-append log-directory
                                                          "/pg_ctl.log"))
                                     '())
                              "-o" options
                              mode)))))))
            (pid-file (in-vicinity data-directory "postmaster.pid"))
            (action (lambda args
                      #~(lambda _
                          (invoke #$pg_ctl-wrapper #$@args)
                          (match '#$args
                            (("start")
                             (call-with-input-file #$pid-file read))
                            (_ #t))))))
       (list (shepherd-service
              (provision '(postgres))
              (documentation "Run the PostgreSQL daemon.")
              (requirement '(user-processes loopback syslogd))
              (modules `((ice-9 match)
                         ,@%default-modules))
              (start (action "start"))
              (stop (action "stop"))))))))

(define postgresql-service-type
  (service-type
   (name 'postgresql)
   (extensions
    (list (service-extension shepherd-root-service-type
                             postgresql-shepherd-service)
          (service-extension activation-service-type
                             postgresql-activation)
          (service-extension account-service-type
                             (const %postgresql-accounts))
          (service-extension
           profile-service-type
           (compose list postgresql-configuration-postgresql))))
   (default-value (postgresql-configuration
                   (postgresql postgresql-10)))))

(define-deprecated (postgresql-service #:key (postgresql postgresql)
                                       (port 5432)
                                       (locale "en_US.utf8")
                                       (config-file (postgresql-config-file))
                                       (data-directory
                                        "/var/lib/postgresql/data")
                                       (extension-packages '()))
  postgresql-service-type
  "Return a service that runs @var{postgresql}, the PostgreSQL database
server.

The PostgreSQL daemon loads its runtime configuration from @var{config-file}
and stores the database cluster in @var{data-directory}."
  (service postgresql-service-type
           (postgresql-configuration
            (postgresql postgresql)
            (port port)
            (locale locale)
            (config-file config-file)
            (data-directory data-directory)
            (extension-packages extension-packages))))

(define-record-type* <postgresql-role>
  postgresql-role make-postgresql-role
  postgresql-role?
  (name             postgresql-role-name) ;string
  (permissions      postgresql-role-permissions
                    (default '(createdb login))) ;list
  (create-database? postgresql-role-create-database?  ;boolean
                    (default #f)))

(define-record-type* <postgresql-role-configuration>
  postgresql-role-configuration make-postgresql-role-configuration
  postgresql-role-configuration?
  (host             postgresql-role-configuration-host ;string
                    (default "/var/run/postgresql"))
  (log              postgresql-role-configuration-log ;string
                    (default "/var/log/postgresql_roles.log"))
  (roles            postgresql-role-configuration-roles
                    (default '()))) ;list

(define (postgresql-create-roles config)
  ;; See: https://www.postgresql.org/docs/current/sql-createrole.html for the
  ;; complete permissions list.
  (define (format-permissions permissions)
    (let ((dict '(bypassrls createdb createrole login replication superuser)))
      (string-join (filter-map (lambda (permission)
                                 (and (member permission dict)
                                      (string-upcase
                                       (symbol->string permission))))
                               permissions)
                   " ")))

  (define (roles->queries roles)
    (apply mixed-text-file "queries"
           (append-map
            (lambda (role)
              (match-record role <postgresql-role>
                (name permissions create-database?)
                `("SELECT NOT(EXISTS(SELECT 1 FROM pg_catalog.pg_roles WHERE \
rolname = '" ,name "')) as not_exists;\n"
"\\gset\n"
"\\if :not_exists\n"
"CREATE ROLE \"" ,name "\""
" WITH " ,(format-permissions permissions)
";\n"
,@(if create-database?
      `("CREATE DATABASE \"" ,name "\""
        " OWNER \"" ,name "\";\n")
      '())
"\\endif\n")))
            roles)))

  (let ((host (postgresql-role-configuration-host config))
        (roles (postgresql-role-configuration-roles config)))
    #~(let ((psql #$(file-append postgresql "/bin/psql")))
        (list psql "-a" "-h" #$host "-f" #$(roles->queries roles)))))

(define (postgresql-role-shepherd-service config)
  (match-record config <postgresql-role-configuration>
    (log)
    (list (shepherd-service
           (requirement '(postgres))
           (provision '(postgres-roles))
           (one-shot? #t)
           (start
            #~(lambda args
                (let ((pid (fork+exec-command
                            #$(postgresql-create-roles config)
                            #:user "postgres"
                            #:group "postgres"
                            #:log-file #$log)))
                  (zero? (cdr (waitpid pid))))))
           (documentation "Create PostgreSQL roles.")))))

(define postgresql-role-service-type
  (service-type (name 'postgresql-role)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          postgresql-role-shepherd-service)))
                (compose concatenate)
                (extend (lambda (config extended-roles)
                          (match-record config <postgresql-role-configuration>
                            (host roles)
                            (postgresql-role-configuration
                             (host host)
                             (roles (append roles extended-roles))))))
                (default-value (postgresql-role-configuration))
                (description "Ensure the specified PostgreSQL roles are
created after the PostgreSQL database is started.")))


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
;;; MySQL.
;;;

(define-record-type* <mysql-configuration>
  mysql-configuration make-mysql-configuration
  mysql-configuration?
  (mysql mysql-configuration-mysql (default mariadb))
  (bind-address mysql-configuration-bind-address (default "127.0.0.1"))
  (port mysql-configuration-port (default 3306))
  (socket mysql-configuration-socket (default "/run/mysqld/mysqld.sock"))
  (extra-content mysql-configuration-extra-content (default ""))
  (auto-upgrade? mysql-configuration-auto-upgrade? (default #t)))

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
    (($ <mysql-configuration> mysql bind-address port socket extra-content)
     (mixed-text-file "my.cnf" "[mysqld]
datadir=/var/lib/mysql
socket=" socket "
bind-address=" bind-address "
port=" (number->string port) "
" extra-content "
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
                         (string-append #$mysql:lib "/share/mysql/" sql)
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

(define (mysql-upgrade-wrapper mysql socket-file)
  ;; The MySQL socket and PID file may appear before the server is ready to
  ;; accept connections.  Ensure the socket is responsive before attempting
  ;; to run the upgrade script.
  (program-file
   "mysql-upgrade-wrapper"
   #~(begin
       (let ((mysql-upgrade #$(file-append mysql "/bin/mysql_upgrade"))
             (timeout 10))
         (begin
           (let loop ((i 0))
             (catch 'system-error
               (lambda ()
                 (let ((sock (socket PF_UNIX SOCK_STREAM 0)))
                   (connect sock AF_UNIX #$socket-file)
                   (close-port sock)
                   ;; The socket is ready!
                   (execl mysql-upgrade mysql-upgrade
                          (string-append "--socket=" #$socket-file))))
               (lambda args
                 (if (< i timeout)
                     (begin
                       (sleep 1)
                       (loop (+ 1 i)))
                     ;; No luck, give up.
                     (throw 'timeout-error
                            "MySQL server did not appear in time!"))))))))))

(define (mysql-upgrade-shepherd-service config)
  (list (shepherd-service
         (provision '(mysql-upgrade))
         (requirement '(mysql))
         (one-shot? #t)
         (documentation "Upgrade MySQL database schemas.")
         (start (let ((mysql (mysql-configuration-mysql config))
                      (socket (mysql-configuration-socket config)))
                  #~(make-forkexec-constructor
                     (list #$(mysql-upgrade-wrapper mysql socket))
                     #:user "mysql" #:group "mysql"))))))

(define (mysql-shepherd-services config)
  (if (mysql-configuration-auto-upgrade? config)
      (append (mysql-shepherd-service config)
              (mysql-upgrade-shepherd-service config))
      (mysql-shepherd-service config)))

(define mysql-service-type
  (service-type
   (name 'mysql)
   (extensions
    (list (service-extension account-service-type
                             (const %mysql-accounts))
          (service-extension activation-service-type
                             %mysql-activation)
          (service-extension shepherd-root-service-type
                             mysql-shepherd-services)))
   (default-value (mysql-configuration))))

(define-deprecated (mysql-service #:key (config (mysql-configuration)))
  mysql-service-type
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
                                          (const %redis-accounts))))
                (default-value (redis-configuration))))
