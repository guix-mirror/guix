;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
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
  #:use-module (gnu services networking)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:export (%test-nginx
            %test-php-fpm))

(define %index.html-contents
  ;; Contents of the /index.html file served by nginx.
  "Hello, nginx!")

(define %make-http-root
  ;; Create our server root in /srv.
  #~(begin
      (mkdir "/srv")
      (call-with-output-file "/srv/index.html"
        (lambda (port)
          (display #$%index.html-contents port)))))

(define %nginx-servers
  ;; Server blocks.
  (list (nginx-server-configuration
         (root "/srv")
         (listen '("8042" "443 ssl")))))

(define %nginx-os
  ;; Operating system under test.
  (simple-operating-system
   (dhcp-client-service)
   (service nginx-service-type
            (nginx-configuration
             (log-directory "/var/log/nginx")
             (server-blocks %nginx-servers)))
   (simple-service 'make-http-root activation-service-type
                   %make-http-root)))

(define* (run-nginx-test #:optional (http-port 8042))
  "Run tests in %NGINX-OS, which has nginx running and listening on
HTTP-PORT."
  (define os
    (marionette-operating-system
     %nginx-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings `((8080 . ,http-port)))))

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

          (test-begin "nginx")

          ;; Wait for nginx to be up and running.
          (test-eq "service running"
            'running!
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'nginx)
                'running!)
             marionette))

          ;; Make sure the PID file is created.
          (test-assert "PID file"
            (marionette-eval
             '(file-exists? "/var/run/nginx/pid")
             marionette))

          ;; Retrieve the index.html file we put in /srv.
          (test-equal "http-get"
            '(200 #$%index.html-contents)
            (let-values (((response text)
                          (http-get "http://localhost:8080/index.html"
                                    #:decode-body? #t)))
              (list (response-code response) text)))

          ;; There should be a log file in here.
          (test-assert "log file"
            (marionette-eval
             '(file-exists? "/var/log/nginx/access.log")
             marionette))

          (test-end)
          (exit (= (test-runner-fail-count (test-runner-current)) 0)))))

  (gexp->derivation "nginx-test" test))

(define %test-nginx
  (system-test
   (name "nginx")
   (description "Connect to a running NGINX server.")
   (value (run-nginx-test))))


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
   (dhcp-client-service)
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

          (test-eq "nginx running"
            'running!
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'nginx)
                'running!)
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
