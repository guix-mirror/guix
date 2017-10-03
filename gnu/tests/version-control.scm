;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (gnu tests version-control)
  #:use-module (gnu tests)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system shadow)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services version-control)
  #:use-module (gnu services web)
  #:use-module (gnu services networking)
  #:use-module (gnu packages version-control)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:export (%test-cgit))

(define %make-git-repository
  ;; Create Git repository in /srv/git/test.
  #~(begin
      (mkdir-p "/srv/git/test")
      (system* (string-append #$git "/bin/git") "-C" "/srv/git/test"
               "init" "--bare")))

(define %cgit-configuration-nginx
  (list
   (nginx-server-configuration
    (root cgit)
    (locations
     (list
      (nginx-location-configuration
       (uri "@cgit")
       (body '("fastcgi_param SCRIPT_FILENAME $document_root/lib/cgit/cgit.cgi;"
               "fastcgi_param PATH_INFO $uri;"
               "fastcgi_param QUERY_STRING $args;"
               "fastcgi_param HTTP_HOST $server_name;"
               "fastcgi_pass 127.0.0.1:9000;")))))
    (try-files (list "$uri" "@cgit"))
    (http-port 19418)
    (https-port #f)
    (ssl-certificate #f)
    (ssl-certificate-key #f))))

(define %cgit-os
  ;; Operating system under test.
  (let ((base-os
         (simple-operating-system
          (dhcp-client-service)
          (service nginx-service-type)
          (service fcgiwrap-service-type)
          (service cgit-service-type
                   (cgit-configuration
                    (nginx %cgit-configuration-nginx)))
          (simple-service 'make-git-repository activation-service-type
                          %make-git-repository))))
    (operating-system
      (inherit base-os)
      (packages (cons* git
                       (operating-system-packages base-os))))))

(define* (run-cgit-test #:optional (http-port 19418))
  "Run tests in %CGIT-OS, which has nginx running and listening on
HTTP-PORT."
  (define os
    (marionette-operating-system
     %cgit-os
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

          (test-begin "cgit")

          ;; Wait for nginx to be up and running.
          (test-eq "service running"
            'running!
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'nginx)
                'running!)
             marionette))

          ;; Wait for fcgiwrap to be up and running.
          (test-eq "service running"
            'running!
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'fcgiwrap)
                'running!)
             marionette))

          ;; Make sure the PID file is created.
          (test-assert "PID file"
            (marionette-eval
             '(file-exists? "/var/run/nginx/pid")
             marionette))

          ;; Make sure the configuration file is created.
          (test-assert "configuration file"
            (marionette-eval
             '(file-exists? "/etc/cgitrc")
             marionette))

          ;; Make sure Git test repository is created.
          (test-assert "Git test repository"
            (marionette-eval
             '(file-exists? "/srv/git/test")
             marionette))

          ;; Make sure we can access pages that correspond to our repository.
          (letrec-syntax ((test-url
                           (syntax-rules ()
                             ((_ path code)
                              (test-equal (string-append "GET " path)
                                code
                                (let-values (((response body)
                                              (http-get (string-append
                                                         "http://localhost:8080"
                                                         path))))
                                  (response-code response))))
                             ((_ path)
                              (test-url path 200)))))
            (test-url "/")
            (test-url "/test")
            (test-url "/test/log")
            (test-url "/test/tree")
            (test-url "/test/does-not-exist" 404)
            (test-url "/does-not-exist" 404))

          (test-end)
          (exit (= (test-runner-fail-count (test-runner-current)) 0)))))

  (gexp->derivation "cgit-test" test))

(define %test-cgit
  (system-test
   (name "cgit")
   (description "Connect to a running Cgit server.")
   (value (run-cgit-test))))
