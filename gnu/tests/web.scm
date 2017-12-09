;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Ludovic Courtès <ludo@gnu.org>
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
  #:export (%test-nginx))

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
         (http-port 8042))))

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
