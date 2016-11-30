;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2015, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 ng0 <ng0@we.make.ritual.n0.is>
;;; Copyright © 2016 Julien Lepiller <julien@lepiller.eu>
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
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages web)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:export (nginx-configuration
            nginx-configuration?
            nginx-vhost-configuration
            nginx-vhost-configuration?
            nginx-service
            nginx-service-type))

;;; Commentary:
;;;
;;; Web services.
;;;
;;; Code:

(define-record-type* <nginx-vhost-configuration>
  nginx-vhost-configuration make-nginx-vhost-configuration
  nginx-vhost-configuration?
  (http-port           nginx-vhost-configuration-http-port
                       (default 80))
  (https-port          nginx-vhost-configuration-https-port
                       (default 443))
  (server-name         nginx-vhost-configuration-server-name
                       (default (list 'default)))
  (root                nginx-vhost-configuration-root
                       (default "/srv/http"))
  (index               nginx-vhost-configuration-index
                       (default (list "index.html")))
  (ssl-certificate     nginx-vhost-configuration-ssl-certificate
                       (default "/etc/nginx/cert.pem"))
  (ssl-certificate-key nginx-vhost-configuration-ssl-certificate-key
                       (default "/etc/nginx/key.pem"))
  (server-tokens?      nginx-vhost-configuration-server-tokens?
                       (default #f)))

(define-record-type* <nginx-configuration>
  nginx-configuration make-nginx-configuration
  nginx-configuration?
  (nginx         nginx-configuration-nginx)         ;<package>
  (log-directory nginx-configuration-log-directory) ;string
  (run-directory nginx-configuration-run-directory) ;string
  (file          nginx-configuration-file))         ;string | file-like

(define (config-domain-strings names)
 "Return a string denoting the nginx config representation of NAMES, a list
of domain names."
 (string-join
  (map (match-lambda
        ('default "_")
        ((? string? str) str))
       names)))

(define (config-index-strings names)
 "Return a string denoting the nginx config representation of NAMES, a list
of index files."
 (string-join
  (map (match-lambda
        ((? string? str) str))
       names)))

(define (default-nginx-vhost-config vhost)
  (string-append
   "    server {\n"
   (if (nginx-vhost-configuration-http-port vhost)
       (string-append "      listen "
                      (number->string (nginx-vhost-configuration-http-port vhost))
                      ";\n")
       "")
   (if (nginx-vhost-configuration-https-port vhost)
       (string-append "      listen "
                      (number->string (nginx-vhost-configuration-https-port vhost))
                      " ssl;\n")
       "")
   "      server_name " (config-domain-strings
                         (nginx-vhost-configuration-server-name vhost))
                        ";\n"
   (if (nginx-vhost-configuration-ssl-certificate vhost)
       (string-append "      ssl_certificate "
                      (nginx-vhost-configuration-ssl-certificate vhost) ";\n")
       "")
   (if (nginx-vhost-configuration-ssl-certificate-key vhost)
       (string-append "      ssl_certificate_key "
                      (nginx-vhost-configuration-ssl-certificate-key vhost) ";\n")
       "")
   "      root " (nginx-vhost-configuration-root vhost) ";\n"
   "      index " (config-index-strings (nginx-vhost-configuration-index vhost)) ";\n"
   "      server_tokens " (if (nginx-vhost-configuration-server-tokens? vhost)
                              "on" "off") ";\n"
   "    }\n"))

(define (default-nginx-config log-directory run-directory vhost-list)
  (plain-file "nginx.conf"
              (string-append
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
               (let ((http (map default-nginx-vhost-config vhost-list)))
                 (do ((http http (cdr http))
                      (block "" (string-append (car http) "\n" block )))
                     ((null? http) block)))
               "}\n"
               "events {}\n")))

(define %nginx-accounts
  (list (user-group (name "nginx") (system? #t))
        (user-account
         (name "nginx")
         (group "nginx")
         (system? #t)
         (comment "nginx server user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define nginx-activation
  (match-lambda
    (($ <nginx-configuration> nginx log-directory run-directory config-file)
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
         ;; Check configuration file syntax.
         (system* (string-append #$nginx "/sbin/nginx")
                  "-c" #$config-file "-t")))))

(define nginx-shepherd-service
  (match-lambda
    (($ <nginx-configuration> nginx log-directory run-directory config-file)
     (let* ((nginx-binary (file-append nginx "/sbin/nginx"))
            (nginx-action
             (lambda args
               #~(lambda _
                   (zero?
                    (system* #$nginx-binary "-c" #$config-file #$@args))))))

       ;; TODO: Add 'reload' action.
       (list (shepherd-service
              (provision '(nginx))
              (documentation "Run the nginx daemon.")
              (requirement '(user-processes loopback))
              (start (nginx-action "-p" run-directory))
              (stop (nginx-action "-s" "stop"))))))))

(define nginx-service-type
  (service-type (name 'nginx)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          nginx-shepherd-service)
                       (service-extension activation-service-type
                                          nginx-activation)
                       (service-extension account-service-type
                                          (const %nginx-accounts))))))

(define* (nginx-service #:key (nginx nginx)
                        (log-directory "/var/log/nginx")
                        (run-directory "/var/run/nginx")
                        (vhost-list (list (nginx-vhost-configuration)))
                        (config-file
                         (default-nginx-config log-directory run-directory vhost-list)))
  "Return a service that runs NGINX, the nginx web server.

The nginx daemon loads its runtime configuration from CONFIG-FILE, stores log
files in LOG-DIRECTORY, and stores temporary runtime files in RUN-DIRECTORY."
  (service nginx-service-type
           (nginx-configuration
            (nginx nginx)
            (log-directory log-directory)
            (run-directory run-directory)
            (file config-file))))
