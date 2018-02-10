;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 ng0 <ng0@we.make.ritual.n0.is>
;;; Copyright © 2016 Sou Bunnbu <iyzsong@member.fsf.org>
;;; Copyright © 2017, 2018 Clément Lassieur <clement@lassieur.org>
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
  (domains             certbot-configuration-domains
                       (default '()))
  (email               certbot-configuration-email)
  (default-location    certbot-configuration-default-location
                       (default
                         (nginx-location-configuration
                          (uri "/")
                          (body
                           (list "return 301 https://$host$request_uri;"))))))

(define certbot-command
  (match-lambda
    (($ <certbot-configuration> package webroot domains email
                                default-location)
     (let* ((certbot (file-append package "/bin/certbot"))
            (commands
             (map
              (lambda (domain)
                (list certbot "certonly" "-n" "--agree-tos"
                      "-m" email
                      "--webroot" "-w" webroot
                      "-d" domain))
              domains)))
       (program-file
        "certbot-command"
        #~(let ((code 0))
            (for-each
             (lambda (command)
               (set! code (or (apply system* command) code)))
             '#$commands) code))))))

(define (certbot-renewal-jobs config)
  (list
   ;; Attempt to renew the certificates twice per day, at a random minute
   ;; within the hour.  See https://certbot.eff.org/all-instructions/.
   #~(job '(next-minute-from (next-hour '(0 12)) (list (random 60)))
          #$(certbot-command config))))

(define (certbot-activation config)
  (match config
    (($ <certbot-configuration> package webroot domains email
                                default-location)
     (with-imported-modules '((guix build utils))
       #~(begin
           (use-modules (guix build utils))
           (mkdir-p #$webroot)
           (zero? (system* #$(certbot-command config))))))))

(define certbot-nginx-server-configurations
  (match-lambda
    (($ <certbot-configuration> package webroot domains email
                                default-location)
     (map
      (lambda (domain)
        (nginx-server-configuration
         (listen '("80" "[::]:80"))
         (ssl-certificate #f)
         (ssl-certificate-key #f)
         (server-name (list domain))
         (locations
          (filter identity
                  (list
                   (nginx-location-configuration
                    (uri "/.well-known")
                    (body (list (list "root " webroot ";"))))
                   default-location)))))
      domains))))

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
                (extend (lambda (config additional-domains)
                          (certbot-configuration
                           (inherit config)
                           (domains (append
                                     (certbot-configuration-domains config)
                                     additional-domains)))))
                (description
                 "Automatically renew @url{https://letsencrypt.org, Let's
Encrypt} HTTPS certificates by adjusting the nginx web server configuration
and periodically invoking @command{certbot}.")))
