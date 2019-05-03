;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017, 2019 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2017, 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2018 Pierre-Antoine Rouby <pierre-antoine.rouby@inria.fr>
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

(define-module (gnu tests web)
  #:use-module (gnu tests)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system shadow)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services web)
  #:use-module (gnu services databases)
  #:use-module (gnu services getmail)
  #:use-module (gnu services networking)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services mail)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages patchutils)
  #:use-module (gnu packages python)
  #:use-module (gnu packages web)
  #:use-module (guix packages)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:export (%test-httpd
            %test-nginx
            %test-varnish
            %test-php-fpm
            %test-hpcguix-web
            %test-tailon
            %test-patchwork))

(define %index.html-contents
  ;; Contents of the /index.html file.
  "Hello, guix!")

(define %make-http-root
  ;; Create our server root in /srv.
  #~(begin
      (mkdir "/srv")
      (mkdir "/srv/http")
      (call-with-output-file "/srv/http/index.html"
        (lambda (port)
          (display #$%index.html-contents port)))))

(define* (run-webserver-test name test-os #:key (log-file #f) (http-port 8080))
  "Run tests in %NGINX-OS, which has nginx running and listening on
HTTP-PORT."
  (define os
    (marionette-operating-system
     test-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define forwarded-port 8080)

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings `((,http-port . ,forwarded-port)))))

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

          (mkdir #$output)
          (chdir #$output)

          (test-begin #$name)

          (test-assert #$(string-append name " service running")
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (match (start-service '#$(string->symbol name))
                  (#f #f)
                  (('service response-parts ...)
                   (match (assq-ref response-parts 'running)
                     ((#t) #t)
                     ((pid) (number? pid))))))
             marionette))

          ;; Retrieve the index.html file we put in /srv.
          (test-equal "http-get"
            '(200 #$%index.html-contents)
            (let-values
                (((response text)
                  (http-get #$(simple-format
                               #f "http://localhost:~A/index.html" forwarded-port)
                            #:decode-body? #t)))
              (list (response-code response) text)))

          #$@(if log-file
                 `((test-assert ,(string-append "log file exists " log-file)
                     (marionette-eval
                      '(file-exists? ,log-file)
                      marionette)))
                 '())

          (test-end)
          (exit (= (test-runner-fail-count (test-runner-current)) 0)))))

  (gexp->derivation (string-append name "-test") test))


;;;
;;; HTTPD
;;;

(define %httpd-os
  (simple-operating-system
   (service dhcp-client-service-type)
   (service httpd-service-type
            (httpd-configuration
             (config
              (httpd-config-file
               (listen '("8080"))))))
   (simple-service 'make-http-root activation-service-type
                   %make-http-root)))

(define %test-httpd
  (system-test
   (name "httpd")
   (description "Connect to a running HTTPD server.")
   (value (run-webserver-test name %httpd-os
                              #:log-file "/var/log/httpd/error_log"))))


;;;
;;; NGINX
;;;

(define %nginx-servers
  ;; Server blocks.
  (list (nginx-server-configuration
         (listen '("8080")))))

(define %nginx-os
  ;; Operating system under test.
  (simple-operating-system
   (service dhcp-client-service-type)
   (service nginx-service-type
            (nginx-configuration
             (log-directory "/var/log/nginx")
             (server-blocks %nginx-servers)))
   (simple-service 'make-http-root activation-service-type
                   %make-http-root)))

(define %test-nginx
  (system-test
   (name "nginx")
   (description "Connect to a running NGINX server.")
   (value (run-webserver-test name %nginx-os
                              #:log-file "/var/log/nginx/access.log"))))


;;;
;;; Varnish
;;;

(define %varnish-vcl
  (mixed-text-file
   "varnish-test.vcl"
   "vcl 4.0;
backend dummy { .host = \"127.1.1.1\"; }
sub vcl_recv { return(synth(200, \"OK\")); }
sub vcl_synth {
  synthetic(\"" %index.html-contents "\");
  set resp.http.Content-Type = \"text/plain\";
  return(deliver);
}"))

(define %varnish-os
  (simple-operating-system
   (service dhcp-client-service-type)
   ;; Pretend to be a web server that serves %index.html-contents.
   (service varnish-service-type
            (varnish-configuration
             (name "/tmp/server")
             ;; Use a small VSL buffer to fit in the test VM.
             (parameters '(("vsl_space" . "4M")))
             (vcl %varnish-vcl)))
   ;; Proxy the "server" using the builtin configuration.
   (service varnish-service-type
            (varnish-configuration
             (parameters '(("vsl_space" . "4M")))
             (backend "localhost:80")
             (listen '(":8080"))))))

(define %test-varnish
  (system-test
   (name "varnish")
   (description "Test the Varnish Cache server.")
   (value (run-webserver-test "varnish-default" %varnish-os))))


;;;
;;; PHP-FPM
;;;

(define %make-php-fpm-http-root
  ;; Create our server root in /srv.
  #~(begin
      (mkdir "/srv")
      (call-with-output-file "/srv/index.php"
        (lambda (port)
          (display "<?php
phpinfo();
echo(\"Computed by php:\".((string)(2+3)));
?>\n" port)))))

(define %php-fpm-nginx-server-blocks
  (list (nginx-server-configuration
         (root "/srv")
         (locations
          (list (nginx-php-location)))
         (listen '("8042"))
         (ssl-certificate #f)
         (ssl-certificate-key #f))))

(define %php-fpm-os
  ;; Operating system under test.
  (simple-operating-system
   (service dhcp-client-service-type)
   (service php-fpm-service-type)
   (service nginx-service-type
            (nginx-configuration
             (server-blocks %php-fpm-nginx-server-blocks)))
   (simple-service 'make-http-root activation-service-type
                   %make-php-fpm-http-root)))

(define* (run-php-fpm-test #:optional (http-port 8042))
  "Run tests in %PHP-FPM-OS, which has nginx running and listening on
HTTP-PORT, along with php-fpm."
  (define os
    (marionette-operating-system
     %php-fpm-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings `((8080 . ,http-port)))))

  (define test
    (with-imported-modules '((gnu build marionette)
                             (guix build utils))
      #~(begin
          (use-modules (srfi srfi-11) (srfi srfi-64)
                       (gnu build marionette)
                       (web uri)
                       (web client)
                       (web response))

          (define marionette
            (make-marionette (list #$vm)))

          (mkdir #$output)
          (chdir #$output)

          (test-begin "php-fpm")

          (test-assert "php-fpm running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (match (start-service 'php-fpm)
                  (#f #f)
                  (('service response-parts ...)
                   (match (assq-ref response-parts 'running)
                     ((pid) (number? pid))))))
             marionette))

          (test-assert "nginx running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'nginx))
             marionette))

          (test-equal "http-get"
            200
            (let-values (((response text)
                          (http-get "http://localhost:8080/index.php"
                                    #:decode-body? #t)))
              (response-code response)))

          (test-equal "php computed result is sent"
            "Computed by php:5"
            (let-values (((response text)
                          (http-get "http://localhost:8080/index.php"
                                    #:decode-body? #t)))
              (begin
                (use-modules (ice-9 regex))
                (let ((matches (string-match "Computed by php:5" text)))
                  (and matches
                       (match:substring matches 0))))))

          (test-end)

          (exit (= (test-runner-fail-count (test-runner-current)) 0)))))

  (gexp->derivation "php-fpm-test" test))

(define %test-php-fpm
  (system-test
   (name "php-fpm")
   (description "Test PHP-FPM through nginx.")
   (value (run-php-fpm-test))))


;;;
;;; hpcguix-web
;;;

(define* (run-hpcguix-web-server-test name test-os)
  "Run tests in %HPCGUIX-WEB-OS, which has hpcguix-web running."
  (define os
    (marionette-operating-system
     test-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings '((8080 . 5000)))))

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

          (mkdir #$output)
          (chdir #$output)

          (test-begin #$name)

          (test-assert "hpcguix-web running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (match (start-service 'hpcguix-web)
                  (#f #f)
                  (('service response-parts ...)
                   (match (assq-ref response-parts 'running)
                     ((pid) (number? pid))))))
             marionette))

          (test-equal "http-get"
            200
            (begin
              (wait-for-tcp-port 5000 marionette)
              (let-values (((response text)
                            (http-get "http://localhost:8080")))
                (response-code response))))

          (test-end)
          (exit (= (test-runner-fail-count (test-runner-current)) 0)))))

  (gexp->derivation (string-append name "-test") test))

(define %hpcguix-web-specs
  ;; Server config gexp.
  #~(define site-config
      (hpcweb-configuration
       (title-prefix "[TEST] HPCGUIX-WEB"))))

(define %hpcguix-web-os
  (simple-operating-system
   (service dhcp-client-service-type)
   (service hpcguix-web-service-type
            (hpcguix-web-configuration
             (specs %hpcguix-web-specs)))))

(define %test-hpcguix-web
  (system-test
   (name "hpcguix-web")
   (description "Connect to a running hpcguix-web server.")
   (value (run-hpcguix-web-server-test name %hpcguix-web-os))))


(define %tailon-os
  ;; Operating system under test.
  (simple-operating-system
   (service dhcp-client-service-type)
   (service tailon-service-type
            (tailon-configuration
             (config-file
              (tailon-configuration-file
               (bind "0.0.0.0:8080")))))))

(define* (run-tailon-test #:optional (http-port 8081))
  "Run tests in %TAILON-OS, which has tailon running and listening on
HTTP-PORT."
  (define os
    (marionette-operating-system
     %tailon-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings `((,http-port . 8080)))))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-11) (srfi srfi-64)
                       (ice-9 match)
                       (gnu build marionette)
                       (web uri)
                       (web client)
                       (web response))

          (define marionette
            ;; Forward the guest's HTTP-PORT, where tailon is listening, to
            ;; port 8080 in the host.
            (make-marionette (list #$vm)))

          (mkdir #$output)
          (chdir #$output)

          (test-begin "tailon")

          (test-assert "service running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'tailon))
             marionette))

          (define* (retry-on-error f #:key times delay)
            (let loop ((attempt 1))
              (match (catch
                      #t
                      (lambda ()
                        (cons #t
                              (f)))
                      (lambda args
                        (cons #f
                              args)))
                ((#t . return-value)
                 return-value)
                ((#f . error-args)
                 (if (>= attempt times)
                     error-args
                     (begin
                       (sleep delay)
                       (loop (+ 1 attempt))))))))

          (test-equal "http-get"
            200
            (retry-on-error
             (lambda ()
               (let-values (((response text)
                             (http-get #$(format
                                          #f
                                          "http://localhost:~A/"
                                          http-port)
                                       #:decode-body? #t)))
                 (response-code response)))
             #:times 10
             #:delay 5))

          (test-end)
          (exit (= (test-runner-fail-count (test-runner-current)) 0)))))

  (gexp->derivation "tailon-test" test))

(define %test-tailon
  (system-test
   (name "tailon")
   (description "Connect to a running Tailon server.")
   (value (run-tailon-test))))


;;;
;;; Patchwork
;;;

(define patchwork-initial-database-setup-service
  (match-lambda
    (($ <patchwork-database-configuration>
        engine name user password host port)

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
      (provision '(patchwork-postgresql-user-and-database))
      (start start-gexp)
      (stop #~(const #f))
      (respawn? #f)
      (documentation "Setup patchwork database.")))))

(define (patchwork-os patchwork)
  (simple-operating-system
   (service dhcp-client-service-type)
   (service httpd-service-type
            (httpd-configuration
             (config
              (httpd-config-file
               (listen '("8080"))))))
   (service postgresql-service-type)
   (service patchwork-service-type
            (patchwork-configuration
             (patchwork patchwork)
             (domain "localhost")
             (settings-module
              (patchwork-settings-module
               (allowed-hosts (list domain))
               (default-from-email "")))
             (getmail-retriever-config
              (getmail-retriever-configuration
               (type "SimpleIMAPSSLRetriever")
               (server "imap.example.com")
               (port 993)
               (username "username")
               (password "password")
               (extra-parameters
                '((mailboxes . ("INBOX"))))))))
   (simple-service 'patchwork-database-setup
                   shepherd-root-service-type
                   (list
                    (patchwork-initial-database-setup-service
                     (patchwork-database-configuration))))))

(define (run-patchwork-test patchwork)
  "Run tests in %NGINX-OS, which has nginx running and listening on
HTTP-PORT."
  (define os
    (marionette-operating-system
     (patchwork-os patchwork)
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define forwarded-port 8080)

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings `((8080 . ,forwarded-port)))))

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

          (mkdir #$output)
          (chdir #$output)

          (test-begin "patchwork")

          (test-assert "patchwork-postgresql-user-and-service started"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (match (start-service 'patchwork-postgresql-user-and-database)
                  (#f #f)
                  (('service response-parts ...)
                   (match (assq-ref response-parts 'running)
                     ((#t) #t)
                     ((pid) (number? pid))))))
             marionette))

          (test-assert "httpd running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'httpd))
             marionette))

          (test-equal "http-get"
            200
            (let-values
                (((response text)
                  (http-get #$(simple-format
                               #f "http://localhost:~A/" forwarded-port)
                            #:decode-body? #t)))
              (response-code response)))

          (test-end)
          (exit (= (test-runner-fail-count (test-runner-current)) 0)))))

  (gexp->derivation "patchwork-test" test))

(define %test-patchwork
  (system-test
   (name "patchwork")
   (description "Connect to a running Patchwork service.")
   (value (run-patchwork-test patchwork))))
