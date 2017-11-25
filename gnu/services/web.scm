;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 ng0 <ng0@we.make.ritual.n0.is>
;;; Copyright © 2016, 2017 Julien Lepiller <julien@lepiller.eu>
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

(define-module (gnu services web)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages web)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (<nginx-configuration>
            nginx-configuration
            nginx-configuration?
            nginx-configuartion-nginx
            nginx-configuration-log-directory
            nginx-configuration-run-directory
            nginx-configuration-server-blocks
            nginx-configuration-upstream-blocks
            nginx-configuration-server-names-hash-bucket-size
            nginx-configuration-server-names-hash-bucket-max-size
            nginx-configuration-file

            <nginx-server-configuration>
            nginx-server-configuration
            nginx-server-configuration?
            nginx-server-configuration-http-port
            nginx-server-configuartion-https-port
            nginx-server-configuration-server-name
            nginx-server-configuration-root
            nginx-server-configuration-locations
            nginx-server-configuration-index
            nginx-server-configuration-ssl-certificate
            nginx-server-configuration-ssl-certificate-key
            nginx-server-configuration-server-tokens?

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
            fcgiwrap-service-type))

;;; Commentary:
;;;
;;; Web services.
;;;
;;; Code:

(define-record-type* <nginx-server-configuration>
  nginx-server-configuration make-nginx-server-configuration
  nginx-server-configuration?
  (http-port           nginx-server-configuration-http-port
                       (default 80))
  (https-port          nginx-server-configuration-https-port
                       (default 443))
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
                       (default #f)))

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
  (let ((http-port (nginx-server-configuration-http-port server))
        (https-port (nginx-server-configuration-https-port server))
        (server-name (nginx-server-configuration-server-name server))
        (ssl-certificate (nginx-server-configuration-ssl-certificate server))
        (ssl-certificate-key
         (nginx-server-configuration-ssl-certificate-key server))
        (root (nginx-server-configuration-root server))
        (index (nginx-server-configuration-index server))
        (try-files (nginx-server-configuration-try-files server))
        (server-tokens? (nginx-server-configuration-server-tokens? server))
        (locations (nginx-server-configuration-locations server)))
    (define-syntax-parameter <> (syntax-rules ()))
    (define-syntax-rule (and/l x tail ...)
      (let ((x* x))
        (if x*
            (syntax-parameterize ((<> (identifier-syntax x*)))
              (list tail ...))
            '())))
    (list
     "    server {\n"
     (and/l http-port  "      listen " (number->string <>) ";\n")
     (and/l https-port "      listen " (number->string <>) " ssl;\n")
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

(define (default-nginx-config nginx log-directory run-directory server-list
                              upstream-list server-names-hash-bucket-size
                              server-names-hash-bucket-max-size)
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
          (map emit-nginx-upstream-config upstream-list)
          (map emit-nginx-server-config server-list)
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
    (($ <nginx-configuration> nginx log-directory run-directory server-blocks
                              upstream-blocks server-names-hash-bucket-size
                              server-names-hash-bucket-max-size file)
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
                             (default-nginx-config nginx log-directory
                               run-directory server-blocks upstream-blocks
                               server-names-hash-bucket-size
                               server-names-hash-bucket-max-size))
                  "-t")))))

(define nginx-shepherd-service
  (match-lambda
    (($ <nginx-configuration> nginx log-directory run-directory server-blocks
                              upstream-blocks server-names-hash-bucket-size
                              server-names-hash-bucket-max-size file)
     (let* ((nginx-binary (file-append nginx "/sbin/nginx"))
            (nginx-action
             (lambda args
               #~(lambda _
                   (zero?
                    (system* #$nginx-binary "-c"
                             #$(or file
                                   (default-nginx-config nginx log-directory
                                     run-directory server-blocks upstream-blocks
                                     server-names-hash-bucket-size
                                     server-names-hash-bucket-max-size))
                             #$@args))))))

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
