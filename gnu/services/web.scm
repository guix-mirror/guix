;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
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
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages web)
  #:use-module (guix records)
  #:use-module (guix store)
  #:use-module (guix gexp)
  #:export (nginx-service))

;;; Commentary:
;;;
;;; Web services.
;;;
;;; Code:

(define (default-nginx-config log-directory run-directory)
  (plain-file "nginx.conf"
              (string-append
               "user nginx nginx;\n"
               "pid " run-directory "/pid;\n"
               "error_log " log-directory "/error.log info;\n"
               "http {\n"
               "    access_log " log-directory "/access.log;\n"
               "    root /var/www;\n"
               "    server {}\n"
               "}\n"
               "events {}\n")))

(define* (nginx-service #:key (nginx nginx)
                        (log-directory "/var/log/nginx")
                        (run-directory "/var/run/nginx")
                        (config-file
                         (default-nginx-config log-directory run-directory)))
  "Return a service that runs NGINX, the nginx web server.

The nginx daemon loads its runtime configuration from CONFIG-FIGLE, stores log
files in LOG-DIRECTORY, and stores temporary runtime files in RUN-DIRECTORY."
  (define nginx-binary
    #~(string-append #$nginx "/sbin/nginx"))

  (define (nginx-action . args)
    #~(lambda _
        (zero?
         (system* #$nginx-binary "-c" #$config-file #$@args))))

  (define activate
    #~(begin
        (use-modules (guix build utils))
        (format #t "creating nginx log directory '~a'~%" #$log-directory)
        (mkdir-p #$log-directory)
        (format #t "creating nginx run directory '~a'~%" #$run-directory)
        (mkdir-p #$run-directory)
        ;; Check configuration file syntax.
        (system* #$nginx-binary "-c" #$config-file "-t")))

  (define nologin #~(string-append #$shadow "/sbin/nologin"))

  ;; TODO: Add 'reload' action.
  (service
   (provision '(nginx))
   (documentation "Run the nginx daemon.")
   (requirement '(user-processes loopback))
   (start (nginx-action "-p" run-directory))
   (stop (nginx-action "-s" "stop"))
   (activate activate)
   (user-groups (list (user-group
                       (name "nginx")
                       (system? #t))))
   (user-accounts (list (user-account
                         (name "nginx")
                         (group "nginx")
                         (system? #t)
                         (comment "nginx server user")
                         (home-directory "/var/empty")
                         (shell nologin))))))
