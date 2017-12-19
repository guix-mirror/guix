;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 ng0 <ng0@we.make.ritual.n0.is>
;;; Copyright © 2016 Sou Bunnbu <iyzsong@member.fsf.org>
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

(define-module (gnu services certbot)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services mcron)
  #:use-module (gnu services web)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages tls)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (certbot-service-type
            certbot-configuration
            certbot-configuration?))

;;; Commentary:
;;;
;;; Automatically obtaining TLS certificates from Let's Encrypt.
;;;
;;; Code:


(define-record-type* <certbot-configuration>
  certbot-configuration make-certbot-configuration
  certbot-configuration?
  (package             certbot-configuration-package
                       (default certbot))
  (webroot             certbot-configuration-webroot
                       (default "/var/www"))
  (hosts               certbot-configuration-hosts
                       (default '()))
  (default-location    certbot-configuration-default-location
                       (default
                         (nginx-location-configuration
                          (uri "/")
                          (body
                           (list "return 301 https://$host$request_uri;"))))))

(define certbot-renewal-jobs
  (match-lambda
    (($ <certbot-configuration> package webroot hosts default-location)
     (match hosts
       ;; Avoid pinging certbot if we have no hosts.
       (() '())
       (_
        (list
         ;; Attempt to renew the certificates twice a week.
         #~(job (lambda (now)
                  (next-day-from (next-hour-from now '(3))
                                 '(2 5)))
                (string-append #$package "/bin/certbot renew"
                               (string-concatenate
                                (map (lambda (host)
                                       (string-append " -d " host))
                                     '#$hosts))))))))))

(define certbot-activation
  (match-lambda
    (($ <certbot-configuration> package webroot hosts default-location)
     (with-imported-modules '((guix build utils))
       #~(begin
	   (use-modules (guix build utils))
	   (mkdir-p #$webroot)
           (for-each
            (lambda (host)
              (unless (file-exists? (in-vicinity "/etc/letsencrypt/live" host))
                (unless (zero? (system*
                                (string-append #$certbot "/bin/certbot")
                                "certonly" "--webroot" "-w" #$webroot
                                "-d" host))
                  (error "failed to acquire cert for host" host))))
            '#$hosts))))))

(define certbot-nginx-server-configurations
  (match-lambda
    (($ <certbot-configuration> package webroot hosts default-location)
     (map
      (lambda (host)
        (nginx-server-configuration
         (listen '("80"))
         (ssl-certificate #f)
         (ssl-certificate-key #f)
         (server-name (list host))
         (locations
          (filter identity
                  (list
                   (nginx-location-configuration
                    (uri "/.well-known")
                    (body (list (list "root " webroot ";"))))
                   default-location)))))
      hosts))))

(define certbot-service-type
  (service-type (name 'certbot)
                (extensions
                 (list (service-extension nginx-service-type
                                          certbot-nginx-server-configurations)
                       (service-extension activation-service-type
                                          certbot-activation)
                       (service-extension mcron-service-type
                                          certbot-renewal-jobs)))
                (compose concatenate)
                (extend (lambda (config additional-hosts)
                          (certbot-configuration
                           (inherit config)
                           (hosts (append (certbot-configuration-hosts config)
                                          additional-hosts)))))
                (default-value (certbot-configuration))
                (description
                 "Automatically renew @url{https://letsencrypt.org, Let's
Encrypt} HTTPS certificates by adjusting the nginx web server configuration
and periodically invoking @command{certbot}.")))
