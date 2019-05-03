;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2015, 2016, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 ng0 <ng0@n0.is>
;;; Copyright © 2016, 2017, 2018 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2017 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2017 nee <nee-git@hidamari.blue>
;;; Copyright © 2017, 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2018 Pierre-Antoine Rouby <pierre-antoine.rouby@inria.fr>
;;; Copyright © 2017, 2018, 2019 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2018 Marius Bakke <mbakke@fastmail.com>
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

(define-module (gnu services web)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services admin)
  #:use-module (gnu services getmail)
  #:use-module (gnu services mail)
  #:use-module (gnu system pam)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages web)
  #:use-module (gnu packages patchutils)
  #:use-module (gnu packages php)
  #:use-module (gnu packages python)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages logging)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix modules)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module ((guix store) #:select (text-file))
  #:use-module ((guix utils) #:select (version-major))
  #:use-module ((guix packages) #:select (package-version))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:export (<httpd-configuration>
            httpd-configuration
            httpd-configuration?
            httpd-configuration-package
            httpd-configuration-pid-file
            httpd-configuration-config

            <httpd-virtualhost>
            httpd-virtualhost
            httpd-virtualhost?
            httpd-virtualhost-addresses-and-ports
            httpd-virtualhost-contents

            <httpd-config-file>
            httpd-config-file
            httpd-config-file?
            httpd-config-file-modules
            httpd-config-file-server-root
            httpd-config-file-server-name
            httpd-config-file-listen
            httpd-config-file-pid-file
            httpd-config-file-error-log
            httpd-config-file-user
            httpd-config-file-group

            <httpd-module>
            httpd-module
            httpd-module?
            %default-httpd-modules

            httpd-service-type

            <nginx-configuration>
            nginx-configuration
            nginx-configuration?
            nginx-configuartion-nginx
            nginx-configuration-log-directory
            nginx-configuration-run-directory
            nginx-configuration-server-blocks
            nginx-configuration-upstream-blocks
            nginx-configuration-server-names-hash-bucket-size
            nginx-configuration-server-names-hash-bucket-max-size
            nginx-configuration-extra-content
            nginx-configuration-file

            <nginx-server-configuration>
            nginx-server-configuration
            nginx-server-configuration?
            nginx-server-configuration-listen
            nginx-server-configuration-server-name
            nginx-server-configuration-root
            nginx-server-configuration-locations
            nginx-server-configuration-index
            nginx-server-configuration-ssl-certificate
            nginx-server-configuration-ssl-certificate-key
            nginx-server-configuration-server-tokens?
            nginx-server-configuration-raw-content

            <nginx-upstream-configuration>
            nginx-upstream-configuration
            nginx-upstream-configuration?
            nginx-upstream-configuration-name
            nginx-upstream-configuration-servers

            <nginx-location-configuration>
            nginx-location-configuration
            nginx-location-configuration?
            nginx-location-configuration-uri
            nginx-location-configuration-body

            <nginx-named-location-configuration>
            nginx-named-location-configuration
            nginx-named-location-configuration?
            nginx-named-location-configuration-name
            nginx-named-location-configuration-body

            nginx-service
            nginx-service-type

            fcgiwrap-configuration
            fcgiwrap-configuration?
            fcgiwrap-service-type

            <php-fpm-configuration>
            php-fpm-configuration
            make-php-fpm-configuration
            php-fpm-configuration?
            php-fpm-configuration-php
            php-fpm-configuration-socket
            php-fpm-configuration-user
            php-fpm-configuration-group
            php-fpm-configuration-socket-user
            php-fpm-configuration-socket-group
            php-fpm-configuration-pid-file
            php-fpm-configuration-log-file
            php-fpm-configuration-process-manager
            php-fpm-configuration-display-errors
            php-fpm-configuration-timezone
            php-fpm-configuration-workers-log-file
            php-fpm-configuration-file

            <php-fpm-dynamic-process-manager-configuration>
            php-fpm-dynamic-process-manager-configuration
            make-php-fpm-dynamic-process-manager-configuration
            php-fpm-dynamic-process-manager-configuration?
            php-fpm-dynamic-process-manager-configuration-max-children
            php-fpm-dynamic-process-manager-configuration-start-servers
            php-fpm-dynamic-process-manager-configuration-min-spare-servers
            php-fpm-dynamic-process-manager-configuration-max-spare-servers

            <php-fpm-static-process-manager-configuration>
            php-fpm-static-process-manager-configuration
            make-php-fpm-static-process-manager-configuration
            php-fpm-static-process-manager-configuration?
            php-fpm-static-process-manager-configuration-max-children

            <php-fpm-on-demand-process-manager-configuration>
            php-fpm-on-demand-process-manager-configuration
            make-php-fpm-on-demand-process-manager-configuration
            php-fpm-on-demand-process-manager-configuration?
            php-fpm-on-demand-process-manager-configuration-max-children
            php-fpm-on-demand-process-manager-configuration-process-idle-timeout

            php-fpm-service-type
            nginx-php-location

            cat-avatar-generator-service

            hpcguix-web-configuration
            hpcguix-web-configuration?
            hpcguix-web-service-type

            <tailon-configuration-file>
            tailon-configuration-file
            tailon-configuration-file?
            tailon-configuration-file-files
            tailon-configuration-file-bind
            tailon-configuration-file-relative-root
            tailon-configuration-file-allow-transfers?
            tailon-configuration-file-follow-names?
            tailon-configuration-file-tail-lines
            tailon-configuration-file-allowed-commands
            tailon-configuration-file-debug?
            tailon-configuration-file-http-auth
            tailon-configuration-file-users

            <tailon-configuration>
            tailon-configuration
            tailon-configuration?
            tailon-configuration-config-file
            tailon-configuration-package

            tailon-service-type

            <varnish-configuration>
            varnish-configuration
            varnish-configuration?
            varnish-configuration-package
            varnish-configuration-name
            varnish-configuration-backend
            varnish-configuration-vcl
            varnish-configuration-listen
            varnish-configuration-storage
            varnish-configuration-parameters
            varnish-configuration-extra-options

            varnish-service-type

            <patchwork-database-configuration>
            patchwork-database-configuration
            patchwork-database-configuration?
            patchwork-database-configuration-engine
            patchwork-database-configuration-name
            patchwork-database-configuration-user
            patchwork-database-configuration-password
            patchwork-database-configuration-host
            patchwork-database-configuration-port

            <patchwork-settings-module>
            patchwork-settings-module
            patchwork-settings-module?
            patchwork-settings-module-database-configuration
            patchwork-settings-module-secret-key
            patchwork-settings-module-allowed-hosts
            patchwork-settings-module-default-from-email
            patchwork-settings-module-static-url
            patchwork-settings-module-admins
            patchwork-settings-module-debug?
            patchwork-settings-module-enable-rest-api?
            patchwork-settings-module-enable-xmlrpc?
            patchwork-settings-module-force-https-links?
            patchwork-settings-module-extra-settings

            <patchwork-configuration>
            patchwork-configuration
            patchwork-configuration?
            patchwork-configuration-patchwork
            patchwork-configuration-settings-module
            patchwork-configuration-domain

            patchwork-virtualhost
            patchwork-service-type))

;;; Commentary:
;;;
;;; Web services.
;;;
;;; Code:

(define-record-type* <httpd-module>
  httpd-module make-httpd-module
  httpd-module?
  (name httpd-load-module-name)
  (file httpd-load-module-file))

;; Default modules for the httpd-service-type, taken from etc/httpd/httpd.conf
;; file in the httpd package.
(define %default-httpd-modules
  (map (match-lambda
         ((name file)
          (httpd-module
           (name name)
           (file file))))
       '(("authn_file_module" "modules/mod_authn_file.so")
         ("authn_core_module" "modules/mod_authn_core.so")
         ("authz_host_module" "modules/mod_authz_host.so")
         ("authz_groupfile_module" "modules/mod_authz_groupfile.so")
         ("authz_user_module" "modules/mod_authz_user.so")
         ("authz_core_module" "modules/mod_authz_core.so")
         ("access_compat_module" "modules/mod_access_compat.so")
         ("auth_basic_module" "modules/mod_auth_basic.so")
         ("reqtimeout_module" "modules/mod_reqtimeout.so")
         ("filter_module" "modules/mod_filter.so")
         ("mime_module" "modules/mod_mime.so")
         ("log_config_module" "modules/mod_log_config.so")
         ("env_module" "modules/mod_env.so")
         ("headers_module" "modules/mod_headers.so")
         ("setenvif_module" "modules/mod_setenvif.so")
         ("version_module" "modules/mod_version.so")
         ("unixd_module" "modules/mod_unixd.so")
         ("status_module" "modules/mod_status.so")
         ("autoindex_module" "modules/mod_autoindex.so")
         ("dir_module" "modules/mod_dir.so")
         ("alias_module" "modules/mod_alias.so"))))

(define-record-type* <httpd-config-file>
  httpd-config-file make-httpd-config-file
  httpd-config-file?
  (modules        httpd-config-file-modules
                  (default %default-httpd-modules))
  (server-root    httpd-config-file-server-root
                  (default httpd))
  (server-name    httpd-config-file-server-name
                  (default #f))
  (document-root  httpd-config-file-document-root
                  (default "/srv/http"))
  (listen         httpd-config-file-listen
                  (default '("80")))
  (pid-file       httpd-config-file-pid-file
                  (default "/var/run/httpd"))
  (error-log      httpd-config-file-error-log
                  (default "/var/log/httpd/error_log"))
  (user           httpd-config-file-user
                  (default "httpd"))
  (group          httpd-config-file-group
                  (default "httpd"))
  (extra-config   httpd-config-file-extra-config
                  (default
                    (list "TypesConfig etc/httpd/mime.types"))))

(define-gexp-compiler (httpd-config-file-compiler
                       (file <httpd-config-file>) system target)
  (match file
    (($ <httpd-config-file> load-modules server-root server-name
                                   document-root listen pid-file error-log
                                   user group extra-config)
     (gexp->derivation
      "httpd.conf"
      #~(call-with-output-file (ungexp output "out")
          (lambda (port)
            (display
             (string-append
              (ungexp-splicing
               `(,@(append-map
                    (match-lambda
                      (($ <httpd-module> name module)
                       `("LoadModule " ,name " " ,module "\n")))
                    load-modules)
                 ,@`("ServerRoot " ,server-root "\n")
                 ,@(if server-name
                       `("ServerName " ,server-name "\n")
                       '())
                 ,@`("DocumentRoot " ,document-root "\n")
                 ,@(append-map
                    (lambda (listen-value)
                      `("Listen " ,listen-value "\n"))
                    listen)
                 ,@(if pid-file
                       `("Pidfile " ,pid-file "\n")
                       '())
                 ,@(if error-log
                       `("ErrorLog " ,error-log "\n")
                       '())
                 ,@(if user
                       `("User " ,user "\n")
                       '())
                 ,@(if group
                       `("Group " ,group "\n")
                       '())
                 "\n\n"
                 ,@extra-config)))
             port)))
      #:local-build? #t))))

(define-record-type <httpd-virtualhost>
  (httpd-virtualhost addresses-and-ports contents)
  httpd-virtualhost?
  (addresses-and-ports httpd-virtualhost-addresses-and-ports)
  (contents            httpd-virtualhost-contents))

(define-record-type* <httpd-configuration>
  httpd-configuration make-httpd-configuration
  httpd-configuration?
  (package  httpd-configuration-package
            (default httpd))
  (pid-file httpd-configuration-pid-file
            (default "/var/run/httpd"))
  (config   httpd-configuration-config
            (default (httpd-config-file))))

(define %httpd-accounts
  (list (user-group (name "httpd") (system? #t))
        (user-account
         (name "httpd")
         (group "httpd")
         (system? #t)
         (comment "Apache HTTPD server user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define httpd-shepherd-services
  (match-lambda
    (($ <httpd-configuration> package pid-file config)
     (list (shepherd-service
            (provision '(httpd))
            (documentation "The Apache HTTP Server")
            (requirement '(networking))
            (start #~(make-forkexec-constructor
                      `(#$(file-append package "/bin/httpd")
                        #$@(if config
                               (list "-f" config)
                               '()))
                      #:pid-file #$pid-file))
            (stop #~(make-kill-destructor)))))))

(define httpd-activation
  (match-lambda
    (($ <httpd-configuration> package pid-file config)
     (match-record
      config
      <httpd-config-file>
      (error-log document-root)
      #~(begin
          (use-modules (guix build utils))

          (mkdir-p #$(dirname error-log))
          (mkdir-p #$document-root))))))

(define (httpd-process-extensions original-config extension-configs)
  (let ((config (httpd-configuration-config
                 original-config)))
    (if (httpd-config-file? config)
        (httpd-configuration
         (inherit original-config)
         (config
          (httpd-config-file
           (inherit config)
           (extra-config
            (append (httpd-config-file-extra-config config)
                    (append-map
                     (match-lambda
                       (($ <httpd-virtualhost>
                           addresses-and-ports
                           contents)
                        `(,(string-append
                            "<VirtualHost " addresses-and-ports ">\n")
                          ,@contents
                          "\n</VirtualHost>\n"))
                       ((? string? x)
                        `("\n" ,x "\n"))
                       ((? list? x)
                        `("\n" ,@x "\n")))
                     extension-configs)))))))))

(define httpd-service-type
  (service-type (name 'httpd)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          httpd-shepherd-services)
                       (service-extension activation-service-type
                                          httpd-activation)
                       (service-extension account-service-type
                                          (const %httpd-accounts))))
                (compose concatenate)
                (extend httpd-process-extensions)
                (default-value
                  (httpd-configuration))))

(define-record-type* <nginx-server-configuration>
  nginx-server-configuration make-nginx-server-configuration
  nginx-server-configuration?
  (listen              nginx-server-configuration-listen
                       (default '("80" "443 ssl")))
  (server-name         nginx-server-configuration-server-name
                       (default (list 'default)))
  (root                nginx-server-configuration-root
                       (default "/srv/http"))
  (locations           nginx-server-configuration-locations
                       (default '()))
  (index               nginx-server-configuration-index
                       (default (list "index.html")))
  (try-files           nginx-server-configuration-try-files
                       (default '()))
  (ssl-certificate     nginx-server-configuration-ssl-certificate
                       (default #f))
  (ssl-certificate-key nginx-server-configuration-ssl-certificate-key
                       (default #f))
  (server-tokens?      nginx-server-configuration-server-tokens?
                       (default #f))
  (raw-content         nginx-server-configuration-raw-content
                       (default '())))

(define-record-type* <nginx-upstream-configuration>
  nginx-upstream-configuration make-nginx-upstream-configuration
  nginx-upstream-configuration?
  (name                nginx-upstream-configuration-name)
  (servers             nginx-upstream-configuration-servers))

(define-record-type* <nginx-location-configuration>
  nginx-location-configuration make-nginx-location-configuration
  nginx-location-configuration?
  (uri                 nginx-location-configuration-uri
                       (default #f))
  (body                nginx-location-configuration-body))

(define-record-type* <nginx-named-location-configuration>
  nginx-named-location-configuration make-nginx-named-location-configuration
  nginx-named-location-configuration?
  (name                nginx-named-location-configuration-name
                       (default #f))
  (body                nginx-named-location-configuration-body))

(define-record-type* <nginx-configuration>
  nginx-configuration make-nginx-configuration
  nginx-configuration?
  (nginx         nginx-configuration-nginx          ;<package>
                 (default nginx))
  (log-directory nginx-configuration-log-directory  ;string
                 (default "/var/log/nginx"))
  (run-directory nginx-configuration-run-directory  ;string
                 (default "/var/run/nginx"))
  (server-blocks nginx-configuration-server-blocks
                 (default '()))          ;list of <nginx-server-configuration>
  (upstream-blocks nginx-configuration-upstream-blocks
                   (default '()))      ;list of <nginx-upstream-configuration>
  (server-names-hash-bucket-size nginx-configuration-server-names-hash-bucket-size
                                 (default #f))
  (server-names-hash-bucket-max-size nginx-configuration-server-names-hash-bucket-max-size
                                     (default #f))
  (extra-content nginx-configuration-extra-content
                 (default ""))
  (file          nginx-configuration-file         ;#f | string | file-like
                 (default #f)))

(define (config-domain-strings names)
 "Return a string denoting the nginx config representation of NAMES, a list
of domain names."
 (map (match-lambda
        ('default "_ ")
        ((? string? str) (list str " ")))
      names))

(define (config-index-strings names)
 "Return a string denoting the nginx config representation of NAMES, a list
of index files."
 (map (match-lambda
        ((? string? str) (list str " ")))
      names))

(define emit-nginx-location-config
  (match-lambda
    (($ <nginx-location-configuration> uri body)
     (list
      "      location " uri " {\n"
      (map (lambda (x) (list "        " x "\n")) body)
      "      }\n"))
    (($ <nginx-named-location-configuration> name body)
     (list
      "      location @" name " {\n"
      (map (lambda (x) (list "        " x "\n")) body)
      "      }\n"))))

(define (emit-nginx-server-config server)
  (let ((listen (nginx-server-configuration-listen server))
        (server-name (nginx-server-configuration-server-name server))
        (ssl-certificate (nginx-server-configuration-ssl-certificate server))
        (ssl-certificate-key
         (nginx-server-configuration-ssl-certificate-key server))
        (root (nginx-server-configuration-root server))
        (index (nginx-server-configuration-index server))
        (try-files (nginx-server-configuration-try-files server))
        (server-tokens? (nginx-server-configuration-server-tokens? server))
        (locations (nginx-server-configuration-locations server))
        (raw-content (nginx-server-configuration-raw-content server)))
    (define-syntax-parameter <> (syntax-rules ()))
    (define-syntax-rule (and/l x tail ...)
      (let ((x* x))
        (if x*
            (syntax-parameterize ((<> (identifier-syntax x*)))
              (list tail ...))
            '())))
    (list
     "    server {\n"
     (map (lambda (directive) (list "      listen " directive ";\n")) listen)
     "      server_name " (config-domain-strings server-name) ";\n"
     (and/l ssl-certificate     "      ssl_certificate " <> ";\n")
     (and/l ssl-certificate-key "      ssl_certificate_key " <> ";\n")
     "      root " root ";\n"
     "      index " (config-index-strings index) ";\n"
     (if (not (nil? try-files))
         (and/l (config-index-strings try-files) "      try_files " <> ";\n")
         "")
     "      server_tokens " (if server-tokens? "on" "off") ";\n"
     "\n"
     (map emit-nginx-location-config locations)
     "\n"
     (map (lambda (x) (list "      " x "\n")) raw-content)
     "    }\n")))

(define (emit-nginx-upstream-config upstream)
  (list
   "    upstream " (nginx-upstream-configuration-name upstream) " {\n"
   (map (lambda (server)
          (simple-format #f "      server ~A;\n" server))
        (nginx-upstream-configuration-servers upstream))
   "    }\n"))

(define (flatten . lst)
  "Return a list that recursively concatenates all sub-lists of LST."
  (define (flatten1 head out)
    (if (list? head)
        (fold-right flatten1 out head)
        (cons head out)))
  (fold-right flatten1 '() lst))

(define (default-nginx-config config)
  (match-record config
                <nginx-configuration>
                (nginx log-directory run-directory
                 server-blocks upstream-blocks
                 server-names-hash-bucket-size
                 server-names-hash-bucket-max-size
                 extra-content)
   (apply mixed-text-file "nginx.conf"
          (flatten
           "user nginx nginx;\n"
           "pid " run-directory "/pid;\n"
           "error_log " log-directory "/error.log info;\n"
           "http {\n"
           "    client_body_temp_path " run-directory "/client_body_temp;\n"
           "    proxy_temp_path " run-directory "/proxy_temp;\n"
           "    fastcgi_temp_path " run-directory "/fastcgi_temp;\n"
           "    uwsgi_temp_path " run-directory "/uwsgi_temp;\n"
           "    scgi_temp_path " run-directory "/scgi_temp;\n"
           "    access_log " log-directory "/access.log;\n"
           "    include " nginx "/share/nginx/conf/mime.types;\n"
           (if server-names-hash-bucket-size
               (string-append
                "    server_names_hash_bucket_size "
                (number->string server-names-hash-bucket-size)
                ";\n")
               "")
           (if server-names-hash-bucket-max-size
               (string-append
                "    server_names_hash_bucket_max_size "
                (number->string server-names-hash-bucket-max-size)
                ";\n")
               "")
           "\n"
           (map emit-nginx-upstream-config upstream-blocks)
           (map emit-nginx-server-config server-blocks)
           extra-content
           "\n}\n"
           "events {}\n"))))

(define %nginx-accounts
  (list (user-group (name "nginx") (system? #t))
        (user-account
         (name "nginx")
         (group "nginx")
         (system? #t)
         (comment "nginx server user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define (nginx-activation config)
  (match-record config
                <nginx-configuration>
                (nginx log-directory run-directory file)
   #~(begin
       (use-modules (guix build utils))

       (format #t "creating nginx log directory '~a'~%" #$log-directory)
       (mkdir-p #$log-directory)
       (format #t "creating nginx run directory '~a'~%" #$run-directory)
       (mkdir-p #$run-directory)
       (format #t "creating nginx temp directories '~a/{client_body,proxy,fastcgi,uwsgi,scgi}_temp'~%" #$run-directory)
       (mkdir-p (string-append #$run-directory "/client_body_temp"))
       (mkdir-p (string-append #$run-directory "/proxy_temp"))
       (mkdir-p (string-append #$run-directory "/fastcgi_temp"))
       (mkdir-p (string-append #$run-directory "/uwsgi_temp"))
       (mkdir-p (string-append #$run-directory "/scgi_temp"))
       ;; Start-up logs. Once configuration is loaded, nginx switches to
       ;; log-directory.
       (mkdir-p (string-append #$run-directory "/logs"))
       ;; Check configuration file syntax.
       (system* (string-append #$nginx "/sbin/nginx")
                "-c" #$(or file
                           (default-nginx-config config))
                  "-t"))))

(define (nginx-shepherd-service config)
  (match-record config
                <nginx-configuration>
                (nginx file run-directory)
   (let* ((nginx-binary (file-append nginx "/sbin/nginx"))
          (pid-file (in-vicinity run-directory "pid"))
          (nginx-action
           (lambda args
             #~(lambda _
                 (invoke #$nginx-binary "-c"
                         #$(or file
                               (default-nginx-config config))
                         #$@args)
                 (match '#$args
                   (("-s" . _) #f)
                   (_
                    ;; When FILE is true, we cannot be sure that PID-FILE will
                    ;; be created, so assume it won't show up.  When FILE is
                    ;; false, read PID-FILE.
                    #$(if file
                          #~#t
                          #~(read-pid-file #$pid-file))))))))

     ;; TODO: Add 'reload' action.
     (list (shepherd-service
            (provision '(nginx))
            (documentation "Run the nginx daemon.")
            (requirement '(user-processes loopback))
            (modules `((ice-9 match)
                       ,@%default-modules))
            (start (nginx-action "-p" run-directory))
            (stop (nginx-action "-s" "stop")))))))

(define nginx-service-type
  (service-type (name 'nginx)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          nginx-shepherd-service)
                       (service-extension activation-service-type
                                          nginx-activation)
                       (service-extension account-service-type
                                          (const %nginx-accounts))))
                (compose concatenate)
                (extend (lambda (config servers)
                          (nginx-configuration
                            (inherit config)
                            (server-blocks
                              (append (nginx-configuration-server-blocks config)
                              servers)))))
                (default-value
                  (nginx-configuration))))

(define-record-type* <fcgiwrap-configuration> fcgiwrap-configuration
  make-fcgiwrap-configuration
  fcgiwrap-configuration?
  (package       fcgiwrap-configuration-package ;<package>
                 (default fcgiwrap))
  (socket        fcgiwrap-configuration-socket
                 (default "tcp:127.0.0.1:9000"))
  (user          fcgiwrap-configuration-user
                 (default "fcgiwrap"))
  (group         fcgiwrap-configuration-group
                 (default "fcgiwrap")))

(define fcgiwrap-accounts
  (match-lambda
    (($ <fcgiwrap-configuration> package socket user group)
     (filter identity
             (list
              (and (equal? group "fcgiwrap")
                   (user-group
                    (name "fcgiwrap")
                    (system? #t)))
              (and (equal? user "fcgiwrap")
                   (user-account
                    (name "fcgiwrap")
                    (group group)
                    (system? #t)
                    (comment "Fcgiwrap Daemon")
                    (home-directory "/var/empty")
                    (shell (file-append shadow "/sbin/nologin")))))))))

(define fcgiwrap-shepherd-service
  (match-lambda
    (($ <fcgiwrap-configuration> package socket user group)
     (list (shepherd-service
            (provision '(fcgiwrap))
            (documentation "Run the fcgiwrap daemon.")
            (requirement '(networking))
            (start #~(make-forkexec-constructor
                      '(#$(file-append package "/sbin/fcgiwrap")
			  "-s" #$socket)
		      #:user #$user #:group #$group))
            (stop #~(make-kill-destructor)))))))

(define fcgiwrap-service-type
  (service-type (name 'fcgiwrap)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          fcgiwrap-shepherd-service)
		       (service-extension account-service-type
                                          fcgiwrap-accounts)))
                (default-value (fcgiwrap-configuration))))

(define-record-type* <php-fpm-configuration> php-fpm-configuration
  make-php-fpm-configuration
  php-fpm-configuration?
  (php              php-fpm-configuration-php ;<package>
                    (default php))
  (socket           php-fpm-configuration-socket
                    (default (string-append "/var/run/php"
                                            (version-major (package-version php))
                                            "-fpm.sock")))
  (user             php-fpm-configuration-user
                    (default "php-fpm"))
  (group            php-fpm-configuration-group
                    (default "php-fpm"))
  (socket-user      php-fpm-configuration-socket-user
                    (default "php-fpm"))
  (socket-group     php-fpm-configuration-socket-group
                    (default "nginx"))
  (pid-file         php-fpm-configuration-pid-file
                    (default (string-append "/var/run/php"
                                            (version-major (package-version php))
                                            "-fpm.pid")))
  (log-file         php-fpm-configuration-log-file
                    (default (string-append "/var/log/php"
                                            (version-major (package-version php))
                                            "-fpm.log")))
  (process-manager  php-fpm-configuration-process-manager
                    (default (php-fpm-dynamic-process-manager-configuration)))
  (display-errors   php-fpm-configuration-display-errors
                    (default #f))
  (timezone         php-fpm-configuration-timezone
                    (default #f))
  (workers-log-file php-fpm-configuration-workers-log-file
                    (default (string-append "/var/log/php"
                                            (version-major (package-version php))
                                            "-fpm.www.log")))
  (file             php-fpm-configuration-file ;#f | file-like
                    (default #f)))

(define-record-type* <php-fpm-dynamic-process-manager-configuration>
  php-fpm-dynamic-process-manager-configuration
  make-php-fpm-dynamic-process-manager-configuration
  php-fpm-dynamic-process-manager-configuration?
  (max-children         php-fpm-dynamic-process-manager-configuration-max-children
                        (default 5))
  (start-servers        php-fpm-dynamic-process-manager-configuration-start-servers
                        (default 2))
  (min-spare-servers    php-fpm-dynamic-process-manager-configuration-min-spare-servers
                        (default 1))
  (max-spare-servers    php-fpm-dynamic-process-manager-configuration-max-spare-servers
                        (default 3)))

(define-record-type* <php-fpm-static-process-manager-configuration>
  php-fpm-static-process-manager-configuration
  make-php-fpm-static-process-manager-configuration
  php-fpm-static-process-manager-configuration?
  (max-children         php-fpm-static-process-manager-configuration-max-children
                        (default 5)))

(define-record-type* <php-fpm-on-demand-process-manager-configuration>
  php-fpm-on-demand-process-manager-configuration
  make-php-fpm-on-demand-process-manager-configuration
  php-fpm-on-demand-process-manager-configuration?
  (max-children         php-fpm-on-demand-process-manager-configuration-max-children
                        (default 5))
  (process-idle-timeout php-fpm-on-demand-process-manager-configuration-process-idle-timeout
                        (default 10)))

(define php-fpm-accounts
  (match-lambda
    (($ <php-fpm-configuration> php socket user group socket-user socket-group _ _ _ _ _ _)
     (list
      (user-group (name "php-fpm") (system? #t))
      (user-group
       (name group)
       (system? #t))
      (user-account
       (name user)
       (group group)
       (supplementary-groups '("php-fpm"))
       (system? #t)
       (comment "php-fpm daemon user")
       (home-directory "/var/empty")
       (shell (file-append shadow "/sbin/nologin")))))))

(define (default-php-fpm-config socket user group socket-user socket-group
          pid-file log-file pm display-errors timezone workers-log-file)
  (apply mixed-text-file "php-fpm.conf"
         (flatten
          "[global]\n"
          "pid =" pid-file "\n"
          "error_log =" log-file "\n"
          "[www]\n"
          "user =" user "\n"
          "group =" group "\n"
          "listen =" socket "\n"
          "listen.owner =" socket-user "\n"
          "listen.group =" socket-group "\n"

          (if timezone
              (string-append "php_admin_value[date.timezone] = \"" timezone "\"\n")
              "")

          (match pm
            (($ <php-fpm-dynamic-process-manager-configuration>
                pm.max-children
                pm.start-servers
                pm.min-spare-servers
                pm.max-spare-servers)
             (list
              "pm = dynamic\n"
              "pm.max_children =" (number->string pm.max-children) "\n"
              "pm.start_servers =" (number->string pm.start-servers) "\n"
              "pm.min_spare_servers =" (number->string pm.min-spare-servers) "\n"
              "pm.max_spare_servers =" (number->string pm.max-spare-servers) "\n"))

            (($ <php-fpm-static-process-manager-configuration>
                pm.max-children)
             (list
              "pm = static\n"
              "pm.max_children =" (number->string pm.max-children) "\n"))

            (($ <php-fpm-on-demand-process-manager-configuration>
                pm.max-children
                pm.process-idle-timeout)
             (list
              "pm = ondemand\n"
              "pm.max_children =" (number->string pm.max-children) "\n"
              "pm.process_idle_timeout =" (number->string pm.process-idle-timeout) "s\n")))


          "php_flag[display_errors] = " (if display-errors "on" "off") "\n"

          (if workers-log-file
              (list "catch_workers_output = yes\n"
                    "php_admin_value[error_log] =" workers-log-file "\n"
                    "php_admin_flag[log_errors] = on\n")
              (list "catch_workers_output = no\n")))))

(define php-fpm-shepherd-service
  (match-lambda
    (($ <php-fpm-configuration> php socket user group socket-user socket-group
                                pid-file log-file pm display-errors
                                timezone workers-log-file file)
     (list (shepherd-service
            (provision '(php-fpm))
            (documentation "Run the php-fpm daemon.")
            (requirement '(networking))
            (start #~(make-forkexec-constructor
                      '(#$(file-append php "/sbin/php-fpm")
                        "--fpm-config"
                        #$(or file
                              (default-php-fpm-config socket user group
                                socket-user socket-group pid-file log-file
                                pm display-errors timezone workers-log-file)))
                      #:pid-file #$pid-file))
            (stop #~(make-kill-destructor)))))))

(define (php-fpm-activation config)
  #~(begin
      (use-modules (guix build utils))
      (let* ((user (getpwnam #$(php-fpm-configuration-user config)))
             (touch (lambda (file-name)
                      (call-with-output-file file-name (const #t))))
             (workers-log-file
              #$(php-fpm-configuration-workers-log-file config))
             (init-log-file
              (lambda (file-name)
                (when workers-log-file
                  (when (not (file-exists? file-name))
                    (touch file-name))
                  (chown file-name (passwd:uid user) (passwd:gid user))
                  (chmod file-name #o660)))))
        (init-log-file #$(php-fpm-configuration-log-file config))
        (init-log-file workers-log-file))))


(define php-fpm-service-type
  (service-type
   (name 'php-fpm)
   (description
    "Run @command{php-fpm} to provide a fastcgi socket for calling php through
a webserver.")
   (extensions
    (list (service-extension shepherd-root-service-type
                             php-fpm-shepherd-service)
          (service-extension activation-service-type
                             php-fpm-activation)
          (service-extension account-service-type
                             php-fpm-accounts)))
   (default-value (php-fpm-configuration))))

(define* (nginx-php-location
          #:key
          (nginx-package nginx)
          (socket (string-append "/var/run/php"
                                 (version-major (package-version php))
                                 "-fpm.sock")))
  "Return a nginx-location-configuration that makes nginx run .php files."
  (nginx-location-configuration
   (uri "~ \\.php$")
   (body (list
          "fastcgi_split_path_info ^(.+\\.php)(/.+)$;"
          (string-append "fastcgi_pass unix:" socket ";")
          "fastcgi_index index.php;"
          (list "include " nginx-package "/share/nginx/conf/fastcgi.conf;")))))

(define* (cat-avatar-generator-service
          #:key
          (cache-dir "/var/cache/cat-avatar-generator")
          (package cat-avatar-generator)
          (configuration (nginx-server-configuration)))
  (simple-service
    'cat-http-server nginx-service-type
    (list (nginx-server-configuration
            (inherit configuration)
            (locations
              (cons
                (let ((base (nginx-php-location)))
                  (nginx-location-configuration
                    (inherit base)
                    (body (list (string-append "fastcgi_param CACHE_DIR \""
                                               cache-dir "\";")
                                (nginx-location-configuration-body base)))))
                (nginx-server-configuration-locations configuration)))
            (root #~(string-append #$package
                                   "/share/web/cat-avatar-generator"))))))


(define-record-type* <hpcguix-web-configuration>
  hpcguix-web-configuration make-hpcguix-web-configuration
  hpcguix-web-configuration?

  (package  hpcguix-web-package (default hpcguix-web)) ;<package>

  ;; Specs is gexp of hpcguix-web configuration file
  (specs    hpcguix-web-configuration-specs))

(define %hpcguix-web-accounts
  (list (user-group
         (name "hpcguix-web")
         (system? #t))
        (user-account
         (name "hpcguix-web")
         (group "hpcguix-web")
         (system? #t)
         (comment "hpcguix-web")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define %hpcguix-web-activation
  #~(begin
      (use-modules (guix build utils))
      (let ((home-dir "/var/cache/guix/web")
            (user (getpwnam "hpcguix-web")))
        (mkdir-p home-dir)
        (chown home-dir (passwd:uid user) (passwd:gid user))
        (chmod home-dir #o755))))

(define %hpcguix-web-log-file
  "/var/log/hpcguix-web.log")

(define %hpcguix-web-log-rotations
  (list (log-rotation
         (files (list %hpcguix-web-log-file))
         (frequency 'weekly))))

(define (hpcguix-web-shepherd-service config)
  (let ((specs       (hpcguix-web-configuration-specs config))
        (hpcguix-web (hpcguix-web-package config)))
    (with-imported-modules (source-module-closure
                            '((gnu build shepherd)))
      (shepherd-service
       (documentation "hpcguix-web daemon")
       (provision     '(hpcguix-web))
       (requirement   '(networking))
       (start #~(make-forkexec-constructor
                 (list #$(file-append hpcguix-web "/bin/run")
                       (string-append "--config="
                                      #$(scheme-file "hpcguix-web.scm" specs)))
                 #:user "hpcguix-web"
                 #:group "hpcguix-web"
                 #:environment-variables
                 (list "XDG_CACHE_HOME=/var/cache"
                       "SSL_CERT_DIR=/etc/ssl/certs")
                 #:log-file #$%hpcguix-web-log-file))
       (stop #~(make-kill-destructor))))))

(define hpcguix-web-service-type
  (service-type
   (name 'hpcguix-web)
   (description "Run the hpcguix-web server.")
   (extensions
    (list (service-extension account-service-type
                             (const %hpcguix-web-accounts))
          (service-extension activation-service-type
                             (const %hpcguix-web-activation))
          (service-extension rottlog-service-type
                             (const %hpcguix-web-log-rotations))
          (service-extension shepherd-root-service-type
                             (compose list hpcguix-web-shepherd-service))))))


;;;
;;; Tailon
;;;

(define-record-type* <tailon-configuration-file>
  tailon-configuration-file make-tailon-configuration-file
  tailon-configuration-file?
  (files                   tailon-configuration-file-files
                           (default '("/var/log")))
  (bind                    tailon-configuration-file-bind
                           (default "localhost:8080"))
  (relative-root           tailon-configuration-file-relative-root
                           (default #f))
  (allow-transfers?        tailon-configuration-file-allow-transfers?
                           (default #t))
  (follow-names?           tailon-configuration-file-follow-names?
                           (default #t))
  (tail-lines              tailon-configuration-file-tail-lines
                           (default 200))
  (allowed-commands        tailon-configuration-file-allowed-commands
                           (default '("tail" "grep" "awk")))
  (debug?                  tailon-configuration-file-debug?
                           (default #f))
  (wrap-lines              tailon-configuration-file-wrap-lines
                           (default #t))
  (http-auth               tailon-configuration-file-http-auth
                           (default #f))
  (users                   tailon-configuration-file-users
                           (default #f)))

(define (tailon-configuration-files-string files)
  (string-append
   "\n"
   (string-join
    (map
     (lambda (x)
       (string-append
        "  - "
        (cond
         ((string? x)
          (simple-format #f "'~A'" x))
         ((list? x)
          (string-join
           (cons (simple-format #f "'~A':" (car x))
                 (map
                  (lambda (x) (simple-format #f "      - '~A'" x))
                  (cdr x)))
           "\n"))
         (else (error x)))))
     files)
    "\n")))

(define-gexp-compiler (tailon-configuration-file-compiler
                       (file <tailon-configuration-file>) system target)
  (match file
    (($ <tailon-configuration-file> files bind relative-root
                                    allow-transfers? follow-names?
                                    tail-lines allowed-commands debug?
                                    wrap-lines http-auth users)
     (text-file
      "tailon-config.yaml"
      (string-concatenate
       (filter-map
        (match-lambda
         ((key . #f) #f)
         ((key . value) (string-append key ": " value "\n")))

        `(("files" . ,(tailon-configuration-files-string files))
          ("bind" . ,bind)
          ("relative-root" . ,relative-root)
          ("allow-transfers" . ,(if allow-transfers? "true" "false"))
          ("follow-names" . ,(if follow-names? "true" "false"))
          ("tail-lines" . ,(number->string tail-lines))
          ("commands" . ,(string-append "["
                                        (string-join allowed-commands ", ")
                                        "]"))
          ("debug" . ,(if debug? "true" #f))
          ("wrap-lines" . ,(if wrap-lines "true" "false"))
          ("http-auth" . ,http-auth)
          ("users" . ,(if users
                          (string-concatenate
                           (cons "\n"
                                 (map (match-lambda
                                       ((user . pass)
                                        (string-append
                                         "  " user ":" pass)))
                                      users)))
                          #f)))))))))

(define-record-type* <tailon-configuration>
  tailon-configuration make-tailon-configuration
  tailon-configuration?
  (config-file tailon-configuration-config-file
               (default (tailon-configuration-file)))
  (package tailon-configuration-package
           (default tailon)))

(define tailon-shepherd-service
  (match-lambda
    (($ <tailon-configuration> config-file package)
     (list (shepherd-service
            (provision '(tailon))
            (documentation "Run the tailon daemon.")
            (start #~(make-forkexec-constructor
                      `(,(string-append #$package "/bin/tailon")
                        "-c" ,#$config-file)
                      #:user "tailon"
                      #:group "tailon"))
            (stop #~(make-kill-destructor)))))))

(define %tailon-accounts
  (list (user-group (name "tailon") (system? #t))
        (user-account
         (name "tailon")
         (group "tailon")
         (system? #t)
         (comment "tailon")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define tailon-service-type
  (service-type
   (name 'tailon)
   (description
    "Run Tailon, a Web application for monitoring, viewing, and searching log
files.")
   (extensions
    (list (service-extension shepherd-root-service-type
                             tailon-shepherd-service)
          (service-extension account-service-type
                             (const %tailon-accounts))))
   (compose concatenate)
   (extend (lambda (parameter files)
             (tailon-configuration
              (inherit parameter)
              (config-file
               (let ((old-config-file
                      (tailon-configuration-config-file parameter)))
                 (tailon-configuration-file
                  (inherit old-config-file)
                  (files (append (tailon-configuration-file-files old-config-file)
                                 files))))))))
   (default-value (tailon-configuration))))


;;;
;;; Varnish
;;;

(define-record-type* <varnish-configuration>
  varnish-configuration make-varnish-configuration
  varnish-configuration?
  (package             varnish-configuration-package          ;<package>
                       (default varnish))
  (name                varnish-configuration-name             ;string
                       (default "default"))
  (backend             varnish-configuration-backend          ;string
                       (default "localhost:8080"))
  (vcl                 varnish-configuration-vcl              ;#f | <file-like>
                       (default #f))
  (listen              varnish-configuration-listen           ;list of strings
                       (default '("localhost:80")))
  (storage             varnish-configuration-storage          ;list of strings
                       (default '("malloc,128m")))
  (parameters          varnish-configuration-parameters       ;list of string pairs
                       (default '()))
  (extra-options       varnish-configuration-extra-options    ;list of strings
                       (default '())))

(define %varnish-accounts
  (list (user-group
         (name "varnish")
         (system? #t))
        (user-account
         (name "varnish")
         (group "varnish")
         (system? #t)
         (comment "Varnish Cache User")
         (home-directory "/var/varnish")
         (shell (file-append shadow "/sbin/nologin")))))

(define varnish-shepherd-service
  (match-lambda
    (($ <varnish-configuration> package name backend vcl listen storage
                                parameters extra-options)
     (list (shepherd-service
            (provision (list (symbol-append 'varnish- (string->symbol name))))
            (documentation (string-append "The Varnish Web Accelerator"
                                          " (" name ")"))
            (requirement '(networking))
            (start #~(make-forkexec-constructor
                      (list #$(file-append package "/sbin/varnishd")
                            "-n" #$name
                            #$@(if vcl
                                   #~("-f" #$vcl)
                                   #~("-b" #$backend))
                            #$@(append-map (lambda (a) (list "-a" a)) listen)
                            #$@(append-map (lambda (s) (list "-s" s)) storage)
                            #$@(append-map (lambda (p)
                                             (list "-p" (format #f "~a=~a"
                                                                (car p) (cdr p))))
                                           parameters)
                            #$@extra-options)
                      ;; Varnish will drop privileges to the "varnish" user when
                      ;; it exists.  Not passing #:user here allows the service
                      ;; to bind to ports < 1024.
                      #:pid-file (if (string-prefix? "/" #$name)
                                     (string-append #$name "/_.pid")
                                     (string-append "/var/varnish/" #$name "/_.pid"))))
            (stop #~(make-kill-destructor)))))))

(define varnish-service-type
  (service-type
   (name 'varnish)
   (description "Run the Varnish cache server.")
   (extensions
    (list (service-extension account-service-type
                             (const %varnish-accounts))
          (service-extension shepherd-root-service-type
                             varnish-shepherd-service)))
   (default-value
     (varnish-configuration))))


;;;
;;; Patchwork
;;;

(define-record-type* <patchwork-database-configuration>
  patchwork-database-configuration make-patchwork-database-configuration
  patchwork-database-configuration?
  (engine          patchwork-database-configuration-engine
                   (default "django.db.backends.postgresql_psycopg2"))
  (name            patchwork-database-configuration-name
                   (default "patchwork"))
  (user            patchwork-database-configuration-user
                   (default "httpd"))
  (password        patchwork-database-configuration-password
                   (default ""))
  (host            patchwork-database-configuration-host
                   (default ""))
  (port            patchwork-database-configuration-port
                   (default "")))

(define-record-type* <patchwork-settings-module>
  patchwork-settings-module make-patchwork-settings-module
  patchwork-settings-module?
  (database-configuration    patchwork-settings-module-database-configuration
                             (default (patchwork-database-configuration)))
  (secret-key-file           patchwork-settings-module-secret-key-file
                             (default "/etc/patchwork/django-secret-key"))
  (allowed-hosts             patchwork-settings-module-allowed-hosts)
  (default-from-email        patchwork-settings-module-default-from-email)
  (static-url                patchwork-settings-module-static-url
                             (default "/static/"))
  (admins                    patchwork-settings-module-admins
                             (default '()))
  (debug?                    patchwork-settings-module-debug?
                             (default #f))
  (enable-rest-api?          patchwork-settings-module-enable-rest-api?
                             (default #t))
  (enable-xmlrpc?            patchwork-settings-module-enable-xmlrpc?
                             (default #t))
  (force-https-links?        patchwork-settings-module-force-https-links?
                             (default #t))
  (extra-settings            patchwork-settings-module-extra-settings
                             (default "")))

(define-record-type* <patchwork-configuration>
  patchwork-configuration make-patchwork-configuration
  patchwork-configuration?
  (patchwork                patchwork-configuration-patchwork
                            (default patchwork))
  (domain                   patchwork-configuration-domain)
  (settings-module          patchwork-configuration-settings-module)
  (static-path              patchwork-configuration-static-url
                            (default "/static/"))
  (getmail-retriever-config getmail-retriever-config))

;; Django uses a Python module for configuration, so this compiler generates a
;; Python module from the configuration record.
(define-gexp-compiler (patchwork-settings-module-compiler
                       (file <patchwork-settings-module>) system target)
  (match file
    (($ <patchwork-settings-module> database-configuration secret-key-file
                                    allowed-hosts default-from-email
                                    static-url admins debug? enable-rest-api?
                                    enable-xmlrpc? force-https-links?
                                    extra-configuration)
     (gexp->derivation
      "patchwork-settings"
      (with-imported-modules '((guix build utils))
        #~(let ((output #$output))
            (define (create-__init__.py filename)
              (call-with-output-file filename
                (lambda (port) (display "" port))))

            (use-modules (guix build utils)
                         (srfi srfi-1))

            (mkdir-p (string-append output "/guix/patchwork"))
            (create-__init__.py
             (string-append output "/guix/__init__.py"))
            (create-__init__.py
             (string-append output "/guix/patchwork/__init__.py"))

            (call-with-output-file
                (string-append output "/guix/patchwork/settings.py")
              (lambda (port)
                (display
                 (string-append "from patchwork.settings.base import *

# Configuration from Guix
with open('" #$secret-key-file "') as f:
    SECRET_KEY = f.read().strip()

ALLOWED_HOSTS = [
" #$(string-concatenate
     (map (lambda (allowed-host)
            (string-append "  '" allowed-host "'\n"))
          allowed-hosts))
"]

ADMINS = [
" #$(string-concatenate
     (map (match-lambda
            ((name email-address)
             (string-append
              "('" name "','" email-address "'),")))
          admins))
"]

DEBUG = " #$(if debug? "True" "False") "

ENABLE_REST_API = " #$(if enable-xmlrpc? "True" "False") "
ENABLE_XMLRPC = " #$(if enable-xmlrpc? "True" "False") "

FORCE_HTTPS_LINKS = " #$(if force-https-links? "True" "False") "

DATABASES = {
    'default': {
" #$(match database-configuration
      (($ <patchwork-database-configuration>
          engine name user password host port)
       (string-append
        "        'ENGINE': '" engine "',\n"
        "        'NAME': '" name "',\n"
        "        'USER': '" user "',\n"
        "        'PASSWORD': '" password "',\n"
        "        'HOST': '" host "',\n"
        "        'PORT': '" port "',\n"))) "
    },
}

" #$(if debug?
        #~(string-append "STATIC_ROOT = '"
                         #$(file-append patchwork "/share/patchwork/htdocs")
                         "'")
        #~(string-append "STATIC_URL = '" #$static-url "'")) "

STATICFILES_STORAGE = (
  'django.contrib.staticfiles.storage.StaticFilesStorage'
)

# Guix Extra Configuration
" #$extra-configuration "
") port)))
            #t))
      #:local-build? #t))))

(define patchwork-virtualhost
  (match-lambda
    (($ <patchwork-configuration> patchwork domain
                                  settings-module static-path
                                  getmail-retriever-config)
     (define wsgi.py
       (file-append patchwork
                    (string-append
                     "/lib/python"
                     (version-major+minor
                      (package-version python))
                     "/site-packages/patchwork/wsgi.py")))

     (httpd-virtualhost
      "*:8080"
      `("ServerAdmin admin@example.com`
ServerName " ,domain "

LogFormat \"%v %h %l %u %t \\\"%r\\\" %>s %b \\\"%{Referer}i\\\" \\\"%{User-Agent}i\\\"\" customformat
LogLevel info
CustomLog \"/var/log/httpd/" ,domain "-access_log\" customformat

ErrorLog /var/log/httpd/error.log

WSGIScriptAlias / " ,wsgi.py "
WSGIDaemonProcess " ,(package-name patchwork) " user=httpd group=httpd processes=1 threads=2 display-name=%{GROUP} lang='en_US.UTF-8' locale='en_US.UTF-8' python-path=" ,settings-module "
WSGIProcessGroup " ,(package-name patchwork) "
WSGIPassAuthorization On

<Files " ,wsgi.py ">
  Require all granted
</Files>

" ,@(if static-path
        `("Alias " ,static-path " " ,patchwork "/share/patchwork/htdocs/")
        '())
"
<Directory \"/srv/http/" ,domain "/\">
    AllowOverride None
    Options MultiViews Indexes SymlinksIfOwnerMatch IncludesNoExec
    Require method GET POST OPTIONS
</Directory>")))))

(define (patchwork-httpd-configuration patchwork-configuration)
  (list "WSGISocketPrefix /var/run/mod_wsgi"
        (list "LoadModule wsgi_module "
              (file-append mod-wsgi "/modules/mod_wsgi.so"))
        (patchwork-virtualhost patchwork-configuration)))

(define (patchwork-django-admin-gexp patchwork settings-module)
  #~(lambda command
      (let ((pid (primitive-fork))
            (user (getpwnam "httpd")))
        (if (eq? pid 0)
            (dynamic-wind
              (const #t)
              (lambda ()
                (setgid (passwd:gid user))
                (setuid (passwd:uid user))

                (setenv "DJANGO_SETTINGS_MODULE" "guix.patchwork.settings")
                (setenv "PYTHONPATH" #$settings-module)
                (primitive-exit
                 (if (zero?
                      (apply system*
                             #$(file-append patchwork "/bin/patchwork-admin")
                             command))
                     0
                     1)))
              (lambda ()
                (primitive-exit 1)))
            (zero? (cdr (waitpid pid)))))))

(define (patchwork-django-admin-action patchwork settings-module)
  (shepherd-action
   (name 'django-admin)
   (documentation
    "Run a django admin command for patchwork")
   (procedure (patchwork-django-admin-gexp patchwork settings-module))))

(define patchwork-shepherd-services
  (match-lambda
    (($ <patchwork-configuration> patchwork domain
                                  settings-module static-path
                                  getmail-retriever-config)
     (define secret-key-file-creation-gexp
       (if (patchwork-settings-module? settings-module)
           (with-extensions (list guile-gcrypt)
             #~(let ((secret-key-file
                      #$(patchwork-settings-module-secret-key-file
                         settings-module)))
                 (use-modules (guix build utils)
                              (gcrypt random))

                 (unless (file-exists? secret-key-file)
                   (mkdir-p (dirname secret-key-file))
                   (call-with-output-file secret-key-file
                     (lambda (port)
                       (display (random-token 30 'very-strong) port)))
                   (let* ((pw  (getpwnam "httpd"))
                          (uid (passwd:uid pw))
                          (gid (passwd:gid pw)))
                     (chown secret-key-file uid gid)
                     (chmod secret-key-file #o400)))))
           #~()))

     (list (shepherd-service
            (requirement '(postgres))
            (provision (list (string->symbol
                              (string-append (package-name patchwork)
                                             "-setup"))))
            (start
               #~(lambda ()
                   (define run-django-admin-command
                     #$(patchwork-django-admin-gexp patchwork
                                                    settings-module))

                   #$secret-key-file-creation-gexp

                   (run-django-admin-command "migrate")))
            (stop #~(const #f))
            (actions
             (list (patchwork-django-admin-action patchwork
                                                  settings-module)))
            (respawn? #f)
            (documentation "Setup Patchwork."))))))

(define patchwork-getmail-configs
  (match-lambda
    (($ <patchwork-configuration> patchwork domain
                                  settings-module static-path
                                  getmail-retriever-config)
     (list
      (getmail-configuration
       (name (string->symbol (package-name patchwork)))
       (user "httpd")
       (directory (string-append
                   "/var/lib/getmail/" (package-name patchwork)))
       (rcfile
        (getmail-configuration-file
         (retriever getmail-retriever-config)
         (destination
          (getmail-destination-configuration
           (type "MDA_external")
           (path (file-append patchwork "/bin/patchwork-admin"))
           (extra-parameters
            '((arguments . ("parsemail"))))))
         (options
          (getmail-options-configuration
           (read-all #f)
           (delivered-to #f)
           (received #f)))))
       (idle (assq-ref
              (getmail-retriever-configuration-extra-parameters
               getmail-retriever-config)
              'mailboxes))
       (environment-variables
        (list "DJANGO_SETTINGS_MODULE=guix.patchwork.settings"
              #~(string-append "PYTHONPATH=" #$settings-module))))))))

(define patchwork-service-type
  (service-type
   (name 'patchwork-setup)
   (extensions
    (list (service-extension httpd-service-type
                             patchwork-httpd-configuration)
          (service-extension shepherd-root-service-type
                             patchwork-shepherd-services)
          (service-extension getmail-service-type
                             patchwork-getmail-configs)))
   (description
    "Patchwork patch tracking system.")))
