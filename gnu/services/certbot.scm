;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 ng0 <ng0@n0.is>
;;; Copyright © 2016 Sou Bunnbu <iyzsong@member.fsf.org>
;;; Copyright © 2017, 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2019 Julien Lepiller <julien@lepiller.eu>
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
  #:use-module (guix i18n)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (certbot-service-type
            certbot-configuration
            certbot-configuration?
            certificate-configuration))

;;; Commentary:
;;;
;;; Automatically obtaining TLS certificates from Let's Encrypt.
;;;
;;; Code:


(define-record-type* <certificate-configuration>
  certificate-configuration make-certificate-configuration
  certificate-configuration?
  (name                certificate-configuration-name
                       (default #f))
  (domains             certificate-configuration-domains
                       (default '()))
  (challenge           certificate-configuration-challenge
                       (default #f))
  (authentication-hook certificate-authentication-hook
                       (default #f))
  (cleanup-hook        certificate-cleanup-hook
                       (default #f))
  (deploy-hook         certificate-configuration-deploy-hook
                       (default #f)))

(define-record-type* <certbot-configuration>
  certbot-configuration make-certbot-configuration
  certbot-configuration?
  (package             certbot-configuration-package
                       (default certbot))
  (webroot             certbot-configuration-webroot
                       (default "/var/www"))
  (certificates        certbot-configuration-certificates
                       (default '()))
  (email               certbot-configuration-email)
  (rsa-key-size        certbot-configuration-rsa-key-size
                       (default #f))
  (default-location    certbot-configuration-default-location
                       (default
                         (nginx-location-configuration
                          (uri "/")
                          (body
                           (list "return 301 https://$host$request_uri;"))))))

(define certbot-command
  (match-lambda
    (($ <certbot-configuration> package webroot certificates email
                                rsa-key-size default-location)
     (let* ((certbot (file-append package "/bin/certbot"))
            (rsa-key-size (and rsa-key-size (number->string rsa-key-size)))
            (commands
             (map
              (match-lambda
                (($ <certificate-configuration> custom-name domains challenge
                                                authentication-hook cleanup-hook
                                                deploy-hook)
                 (let ((name (or custom-name (car domains))))
                   (if challenge
                     (append
                      (list name certbot "certonly" "-n" "--agree-tos"
                            "-m" email
                            "--manual"
                            (string-append "--preferred-challenges=" challenge)
                            "--cert-name" name
                            "-d" (string-join domains ","))
                      (if rsa-key-size `("--rsa-key-size" ,rsa-key-size) '())
                      (if authentication-hook
                          `("--manual-auth-hook" ,authentication-hook)
                          '())
                      (if cleanup-hook `("--manual-cleanup-hook" ,cleanup-hook) '())
                      (if deploy-hook `("--deploy-hook" ,deploy-hook) '()))
                     (append
                      (list name certbot "certonly" "-n" "--agree-tos"
                            "-m" email
                            "--webroot" "-w" webroot
                            "--cert-name" name
                            "-d" (string-join domains ","))
                      (if rsa-key-size `("--rsa-key-size" ,rsa-key-size) '())
                      (if deploy-hook `("--deploy-hook" ,deploy-hook) '()))))))
              certificates)))
       (program-file
        "certbot-command"
        #~(begin
            (use-modules (ice-9 match))
            (let ((code 0))
              (for-each
               (match-lambda
                 ((name . command)
                  (begin
                    (format #t "Acquiring or renewing certificate: ~a~%" name)
                    (set! code (or (apply system* command) code)))))
               '#$commands) code)))))))

(define (certbot-renewal-jobs config)
  (list
   ;; Attempt to renew the certificates twice per day, at a random minute
   ;; within the hour.  See https://certbot.eff.org/all-instructions/.
   #~(job '(next-minute-from (next-hour '(0 12)) (list (random 60)))
          #$(certbot-command config))))

(define (certbot-activation config)
  (let* ((certbot-directory "/var/lib/certbot")
         (script (in-vicinity certbot-directory "renew-certificates"))
         (message (format #f (G_ "~a may need to be run~%") script)))
    (match config
      (($ <certbot-configuration> package webroot certificates email
                                  rsa-key-size default-location)
       (with-imported-modules '((guix build utils))
         #~(begin
             (use-modules (guix build utils))
             (mkdir-p #$webroot)
             (mkdir-p #$certbot-directory)
             (copy-file #$(certbot-command config) #$script)
             (display #$message)))))))

(define certbot-nginx-server-configurations
  (match-lambda
    (($ <certbot-configuration> package webroot certificates email
                                rsa-key-size default-location)
     (list
      (nginx-server-configuration
       (listen '("80" "[::]:80"))
       (ssl-certificate #f)
       (ssl-certificate-key #f)
       (server-name
        (apply append (map certificate-configuration-domains certificates)))
       (locations
        (filter identity
                (list
                 (nginx-location-configuration
                  (uri "/.well-known")
                  (body (list (list "root " webroot ";"))))
                 default-location))))))))

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
                (extend (lambda (config additional-certificates)
                          (certbot-configuration
                           (inherit config)
                           (certificates
                            (append
                             (certbot-configuration-certificates config)
                             additional-certificates)))))
                (description
                 "Automatically renew @url{https://letsencrypt.org, Let's
Encrypt} HTTPS certificates by adjusting the nginx web server configuration
and periodically invoking @command{certbot}.")))
