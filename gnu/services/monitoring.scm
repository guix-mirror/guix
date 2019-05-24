;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Sou Bunnbu <iyzsong@member.fsf.org>
;;; Copyright © 2018, 2019 Gábor Boskovits <boskovits@gmail.com>
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

(define-module (gnu services monitoring)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services web)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages monitoring)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix utils)
  #:use-module ((guix ui) #:select (display-hint G_))
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-35)
  #:export (darkstat-configuration
            prometheus-node-exporter-configuration
            darkstat-service-type
            prometheus-node-exporter-service-type

            zabbix-server-configuration
            zabbix-server-service-type
            zabbix-agent-configuration
            zabbix-agent-service-type
            zabbix-front-end-configuration
            zabbix-front-end-service-type
            %zabbix-front-end-configuration-nginx))


;;;
;;; darkstat
;;;

(define-record-type* <darkstat-configuration>
  darkstat-configuration make-darkstat-configuration darkstat-configuration?
  (package      darkstat-configuration-package
                (default darkstat))
  (interface    darkstat-configuration-interface)
  (port         darkstat-configuration-port
                (default "667"))
  (bind-address darkstat-configuration-bind-address
                (default "127.0.0.1"))
  (base         darkstat-configuration-base
                (default "/")))

(define %darkstat-accounts
  (list (user-account
         (name "darkstat")
         (group "darkstat")
         (system? #t)
         (comment "darkstat daemon user")
         (home-directory "/var/lib/darkstat")
         (shell (file-append shadow "/sbin/nologin")))
        (user-group
         (name "darkstat")
         (system? #t))))

(define darkstat-shepherd-service
  (match-lambda
    (($ <darkstat-configuration>
        package interface port bind-address base)
     (shepherd-service
      (documentation "Network statistics gatherer.")
      (provision '(darkstat))
      (requirement '(networking))
      (start #~(make-forkexec-constructor
                (list #$(file-append package "/sbin/darkstat")
                      "-i" #$interface
                      "-p" #$port
                      "-b" #$bind-address
                      "--base" #$base
                      "--syslog" "--no-daemon"
                      "--chroot" "/var/lib/darkstat"
                      "--user" "darkstat"
                      "--import" "darkstat.db"
                      "--export" "darkstat.db")))
      (stop #~(make-kill-destructor))))))

(define darkstat-service-type
  (service-type
   (name 'darkstat)
   (description
    "Run @command{darkstat} to serve network traffic statistics reports over
HTTP.")
   (extensions
    (list (service-extension account-service-type
                             (const %darkstat-accounts))
          (service-extension shepherd-root-service-type
                             (compose list darkstat-shepherd-service))))))

(define-record-type* <prometheus-node-exporter-configuration>
  prometheus-node-exporter-configuration
  make-prometheus-node-exporter-configuration
  prometheus-node-exporter-configuration?
  (package prometheus-node-exporter-configuration-package
           (default go-github-com-prometheus-node-exporter))
  (web-listen-address prometheus-node-exporter-web-listen-address
                      (default ":9100")))

(define prometheus-node-exporter-shepherd-service
  (match-lambda
    (( $ <prometheus-node-exporter-configuration>
         package web-listen-address)
     (shepherd-service
      (documentation "Prometheus node exporter.")
      (provision '(prometheus-node-exporter))
      (requirement '(networking))
      (start #~(make-forkexec-constructor
                (list #$(file-append package "/bin/node_exporter")
                      "--web.listen-address" #$web-listen-address)))
      (stop #~(make-kill-destructor))))))

(define prometheus-node-exporter-service-type
  (service-type
   (name 'prometheus-node-exporter)
   (description
    "Run @command{node_exporter} to serve hardware and OS metrics to
prometheus.")
   (extensions
    (list (service-extension
           shepherd-root-service-type
           (compose list prometheus-node-exporter-shepherd-service))))
   (default-value (prometheus-node-exporter-configuration))))


;;;
;;; Zabbix server
;;;

(define (uglify-field-name field-name)
  (apply string-append
         (map (lambda (str)
                (if (member (string->symbol str) '(ca db ssl))
                    (string-upcase str)
                    (string-capitalize str)))
              (string-split (string-delete #\?
                                           (symbol->string field-name))
                            #\-))))

(define (serialize-field field-name val)
  (format #t "~a=~a~%" (uglify-field-name field-name) val))

(define (serialize-number field-name val)
  (serialize-field field-name (number->string val)))

(define (serialize-list field-name val)
  (if (null? val) "" (serialize-field field-name (string-join val ","))))

(define (serialize-string field-name val)
  (if (and (string? val) (string=? val ""))
      ""
      (serialize-field field-name val)))

(define group? string?)

(define serialize-group
  (const ""))

(define include-files? list?)

(define (serialize-include-files field-name val)
  (if (null? val) "" (for-each (cut serialize-field 'include <>) val)))

(define extra-options? string?)

(define (serialize-extra-options field-name val)
  (if (null? val) "" (display val)))

(define (nginx-server-configuration-list? val)
  (and (list? val) (and-map nginx-server-configuration? val)))

(define (serialize-nginx-server-configuration-list field-name val)
  "")

(define-configuration zabbix-server-configuration
  (zabbix-server
   (package zabbix-server)
   "The zabbix-server package.")
  (user
   (string "zabbix")
   "User who will run the Zabbix server.")
  (group ;for zabbix-server-account procedure
   (group "zabbix")
   "Group who will run the Zabbix server.")
  (db-host
   (string "127.0.0.1")
   "Database host name.")
  (db-name
   (string "zabbix")
   "Database name.")
  (db-user
   (string "zabbix")
   "Database user.")
  (db-password
   (string "")
   "Database password.  Please, use @code{include-files} with
@code{DBPassword=SECRET} inside a specified file instead.")
  (db-port
   (number 5432)
   "Database port.")
  (log-type
   (string "")
   "Specifies where log messages are written to:
@itemize
@item @code{system} - syslog.
@item @code{file} - file specified with @code{log-file} parameter.
@item @code{console} - standard output.
@end itemize\n")
  (log-file
   (string "/var/log/zabbix/server.log")
   "Log file name for @code{log-type} @code{file} parameter.")
  (pid-file
   (string  "/var/run/zabbix/zabbix_server.pid")
   "Name of PID file.")
  (ssl-ca-location
   (string "/etc/ssl/certs/ca-certificates.crt")
   "The location of certificate authority (CA) files for SSL server
certificate verification.")
  (ssl-cert-location
   (string "/etc/ssl/certs")
   "Location of SSL client certificates.")
  (extra-options
   (extra-options "")
   "Extra options will be appended to Zabbix server configuration file.")
  (include-files
   (include-files '())
   "You may include individual files or all files in a directory in the
configuration file."))

(define (zabbix-server-account config)
  "Return the user accounts and user groups for CONFIG."
  (let ((zabbix-user (zabbix-server-configuration-user config))
        (zabbix-group (zabbix-server-configuration-group config)))
    (list (user-group (name zabbix-group) (system? #t))
          (user-account
           (name zabbix-user)
           (system? #t)
           (group zabbix-group)
           (comment "zabbix privilege separation user")
           (home-directory (string-append "/var/run/" zabbix-user))
           (shell (file-append shadow "/sbin/nologin"))))))

(define (zabbix-server-config-file config)
  "Return the zabbix-server configuration file corresponding to CONFIG."
  (computed-file
   "zabbix_server.conf"
   #~(begin
       (call-with-output-file #$output
         (lambda (port)
           (display "# Generated by 'zabbix-server-service'.\n" port)
           (display #$(with-output-to-string
                        (lambda ()
                          (serialize-configuration
                           config zabbix-server-configuration-fields)))
                    port)
           #t)))))

(define (zabbix-server-activation config)
  "Return the activation gexp for CONFIG."
  (with-imported-modules '((guix build utils)
                           (ice-9 rdelim))
    #~(begin
        (use-modules (guix build utils)
                     (ice-9 rdelim))
        (let ((user (getpw #$(zabbix-server-configuration-user config))))
          (for-each (lambda (file)
                      (let ((directory (dirname file)))
                        (mkdir-p directory)
                        (chown directory (passwd:uid user) (passwd:gid user))
                        (chmod directory #o755)))
                    (list #$(zabbix-server-configuration-log-file config)
                          #$(zabbix-server-configuration-pid-file config)
                          "/etc/zabbix/maintenance.inc.php"))))))

(define (zabbix-server-shepherd-service config)
  "Return a <shepherd-service> for Zabbix server with CONFIG."
  (list (shepherd-service
         (provision '(zabbix-server))
         (documentation "Run Zabbix server daemon.")
         (start #~(make-forkexec-constructor
                   (list #$(file-append (zabbix-server-configuration-zabbix-server config)
                                        "/sbin/zabbix_server")
                         "--config" #$(zabbix-server-config-file config)
                         "--foreground")
                   #:user #$(zabbix-server-configuration-user config)
                   #:group #$(zabbix-server-configuration-group config)
                   #:pid-file #$(zabbix-server-configuration-pid-file config)
                   #:environment-variables
                   (list "SSL_CERT_DIR=/run/current-system/profile\
/etc/ssl/certs"
                         "SSL_CERT_FILE=/run/current-system/profile\
/etc/ssl/certs/ca-certificates.crt")))
         (stop #~(make-kill-destructor)))))

(define zabbix-server-service-type
  (service-type
   (name 'zabbix-server)
   (extensions
    (list (service-extension shepherd-root-service-type
                             zabbix-server-shepherd-service)
          (service-extension account-service-type
                             zabbix-server-account)
          (service-extension activation-service-type
                             zabbix-server-activation)))
   (default-value (zabbix-server-configuration))))

(define (generate-zabbix-server-documentation)
  (generate-documentation
   `((zabbix-server-configuration
      ,zabbix-server-configuration-fields))
   'zabbix-server-configuration))

(define-configuration zabbix-agent-configuration
  (zabbix-agent
   (package zabbix-agentd)
   "The zabbix-agent package.")
  (user
   (string "zabbix")
   "User who will run the Zabbix agent.")
  (group
   (group "zabbix")
   "Group who will run the Zabbix agent.")
  (hostname
   (string "Zabbix server")
   "Unique, case sensitive hostname which is required for active checks and
must match hostname as configured on the server.")
  (log-type
   (string "")
   "Specifies where log messages are written to:
@itemize
@item @code{system} - syslog.
@item @code{file} - file specified with @code{log-file} parameter.
@item @code{console} - standard output.
@end itemize\n")
  (log-file
   (string "/var/log/zabbix/agent.log")
   "Log file name for @code{log-type} @code{file} parameter.")
  (pid-file
   (string "/var/run/zabbix/zabbix_agent.pid")
   "Name of PID file.")
  (server
   (list '("127.0.0.1"))
   "List of IP addresses, optionally in CIDR notation, or hostnames of Zabbix
servers and Zabbix proxies.  Incoming connections will be accepted only from
the hosts listed here.")
  (server-active
   (list '("127.0.0.1"))
   "List of IP:port (or hostname:port) pairs of Zabbix servers and Zabbix
proxies for active checks.  If port is not specified, default port is used.
If this parameter is not specified, active checks are disabled.")
  (extra-options
   (extra-options "")
   "Extra options will be appended to Zabbix server configuration file.")
  (include-files
   (include-files '())
   "You may include individual files or all files in a directory in the
configuration file."))

(define (zabbix-agent-account config)
  "Return the user accounts and user groups for CONFIG."
  (let ((zabbix-user "zabbix")
        (zabbix-group "zabbix"))
    (list (user-group (name zabbix-group) (system? #t))
          (user-account
           (name zabbix-user)
           (system? #t)
           (group zabbix-group)
           (comment "zabbix privilege separation user")
           (home-directory (string-append "/var/run/" zabbix-user))
           (shell (file-append shadow "/sbin/nologin"))))))

(define (zabbix-agent-activation config)
  "Return the activation gexp for CONFIG."
  (with-imported-modules '((guix build utils)
                           (ice-9 rdelim))
    #~(begin
        (use-modules (guix build utils)
                     (ice-9 rdelim))
        (let ((user
               (getpw #$(zabbix-agent-configuration-user config))))
          (for-each (lambda (file)
                      (let ((directory (dirname file)))
                        (mkdir-p directory)
                        (chown directory (passwd:uid user) (passwd:gid user))
                        (chmod directory #o755)))
                    (list #$(zabbix-agent-configuration-log-file config)
                          #$(zabbix-agent-configuration-pid-file config)))))))

(define (zabbix-agent-config-file config)
  "Return the zabbix-agent configuration file corresponding to CONFIG."
  (computed-file
   "zabbix_agent.conf"
   #~(begin
       (call-with-output-file #$output
         (lambda (port)
           (display "# Generated by 'zabbix-agent-service'.\n" port)
           (display #$(with-output-to-string
                        (lambda ()
                          (serialize-configuration
                           config zabbix-agent-configuration-fields)))
                    port)
           #t)))))

(define (zabbix-agent-shepherd-service config)
  "Return a <shepherd-service> for Zabbix agent with CONFIG."
  (list (shepherd-service
         (provision '(zabbix-agent))
         (documentation "Run Zabbix agent daemon.")
         (start #~(make-forkexec-constructor
                   (list #$(file-append (zabbix-agent-configuration-zabbix-agent config)
                                        "/sbin/zabbix_agentd")
                         "--config" #$(zabbix-agent-config-file config)
                         "--foreground")
                   #:user #$(zabbix-agent-configuration-user config)
                   #:group #$(zabbix-agent-configuration-group config)
                   #:pid-file #$(zabbix-agent-configuration-pid-file config)
                   #:environment-variables
                   (list "SSL_CERT_DIR=/run/current-system/profile\
/etc/ssl/certs"
                         "SSL_CERT_FILE=/run/current-system/profile\
/etc/ssl/certs/ca-certificates.crt")))
         (stop #~(make-kill-destructor)))))

(define zabbix-agent-service-type
  (service-type
   (name 'zabbix-agent)
   (extensions
    (list (service-extension shepherd-root-service-type
                             zabbix-agent-shepherd-service)
          (service-extension account-service-type
                             zabbix-agent-account)
          (service-extension activation-service-type
                             zabbix-agent-activation)))
   (default-value (zabbix-agent-configuration))))

(define (generate-zabbix-agent-documentation)
  (generate-documentation
   `((zabbix-agent-configuration
      ,zabbix-agent-configuration-fields))
   'zabbix-agent-configuration))

(define %zabbix-front-end-configuration-nginx
  (nginx-server-configuration
   (root #~(string-append #$zabbix-server:front-end "/share/zabbix/php"))
   (index '("index.php"))
   (locations
    (let ((php-location (nginx-php-location)))
      (list (nginx-location-configuration
             (inherit php-location)
             (body (append (nginx-location-configuration-body php-location)
                           (list "
fastcgi_param PHP_VALUE \"post_max_size = 16M
                          max_execution_time = 300\";
")))))))))

(define-configuration zabbix-front-end-configuration
  ;; TODO: Specify zabbix front-end package.
  ;; (zabbix-
  ;;  (package zabbix-front-end)
  ;;  "The zabbix-front-end package.")
  (nginx
   (nginx-server-configuration-list
    (list %zabbix-front-end-configuration-nginx))
   "NGINX configuration.")
  (db-host
   (string "localhost")
   "Database host name.")
  (db-port
   (number 5432)
   "Database port.")
  (db-name
   (string "zabbix")
   "Database name.")
  (db-user
   (string "zabbix")
   "Database user.")
  (db-password
   (string "")
   "Database password.  Please, use @code{db-secret-file} instead.")
  (db-secret-file
   (string "")
   "Secret file which will be appended to @file{zabbix.conf.php} file.  This
file contains credentials for use by Zabbix front-end.  You are expected to
create it manually.")
  (zabbix-host
   (string "localhost")
   "Zabbix server hostname.")
  (zabbix-port
   (number 10051)
   "Zabbix server port."))

(define (zabbix-front-end-config config)
  (match-record config <zabbix-front-end-configuration>
    (%location db-host db-port db-name db-user db-password db-secret-file
               zabbix-host zabbix-port)
    (mixed-text-file "zabbix.conf.php"
                     "\
<?php
// Zabbix GUI configuration file.
global $DB;

$DB['TYPE']     = 'POSTGRESQL';
$DB['SERVER']   = '" db-host "';
$DB['PORT']     = '" (number->string db-port) "';
$DB['DATABASE'] = '" db-name "';
$DB['USER']     = '" db-user "';
$DB['PASSWORD'] = '" (let ((file (location-file %location))
                           (line (location-line %location))
                           (column (location-column %location)))
                       (if (string-null? db-password)
                           (if (string-null? db-secret-file)
                               (raise (make-compound-condition
                                       (condition
                                        (&message
                                         (message
                                          (format #f "no '~A' or '~A' field in your '~A' record"
                                                  'db-secret-file 'db-password
                                                  'zabbix-front-end-configuration))))
                                       (condition
                                        (&error-location
                                         (location %location)))))
                               (string-trim-both
                                (with-input-from-file db-secret-file
                                  read-string)))
                           (begin
                             (display-hint (format #f (G_ "~a:~a:~a: ~a:
Consider using @code{db-secret-file} instead of @code{db-password} for better
security.") file line column 'zabbix-front-end-configuration))
                             db-password))) "';

// Schema name. Used for IBM DB2 and PostgreSQL.
$DB['SCHEMA'] = '';

$ZBX_SERVER      = '" zabbix-host "';
$ZBX_SERVER_PORT = '" (number->string zabbix-port) "';
$ZBX_SERVER_NAME = '';

$IMAGE_FORMAT_DEFAULT = IMAGE_FORMAT_PNG;
")))

(define %maintenance.inc.php
  ;; Empty php file to allow us move zabbix-frontend configs to ‘/etc/zabbix’
  ;; directory.  See ‘install-front-end’ phase in
  ;; (@ (gnu packages monitoring) zabbix-server) package.
    "\
<?php
")

(define (zabbix-front-end-activation config)
  "Return the activation gexp for CONFIG."
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p "/etc/zabbix")
      (call-with-output-file "/etc/zabbix/maintenance.inc.php"
            (lambda (port)
              (display #$%maintenance.inc.php port)))
      (copy-file #$(zabbix-front-end-config config)
                 "/etc/zabbix/zabbix.conf.php")))

(define zabbix-front-end-service-type
  (service-type
   (name 'zabbix-front-end)
   (extensions
    (list (service-extension activation-service-type
                             zabbix-front-end-activation)
          (service-extension nginx-service-type
                             zabbix-front-end-configuration-nginx)
          ;; Make sure php-fpm is instantiated.
          (service-extension php-fpm-service-type
                             (const #t))))
   (default-value (zabbix-front-end-configuration))
   (description
    "Run the zabbix-front-end web interface, which allows users to interact
with Zabbix server.")))

(define (generate-zabbix-front-end-documentation)
  (generate-documentation
   `((zabbix-front-end-configuration
      ,zabbix-front-end-configuration-fields))
   'zabbix-front-end-configuration))
