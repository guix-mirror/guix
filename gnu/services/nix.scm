;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020, 2021 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Peng Mei Yu <i@pengmeiyu.com>
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

(define-module (gnu services nix)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages package-management)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services web)
  #:use-module (gnu services)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix store)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (guix modules)
  #:export (nix-service-type

            nix-configuration
            nix-configuration?))

;;; Commentary:
;;;
;;; This module provides a service definition for the Nix daemon.
;;;
;;; Code:

(define-record-type* <nix-configuration>
  nix-configuration make-nix-configuration
  nix-configuration?
  (package             nix-configuration-package ;file-like
                       (default nix))
  (sandbox             nix-configuration-sandbox ;boolean
                       (default #t))
  (build-sandbox-items nix-configuration-build-sandbox-items ;list of strings
                       (default '()))
  (extra-config        nix-configuration-extra-config ;list of strings
                       (default '()))
  (extra-options       nix-configuration-extra-options ;list of strings
                       (default '())))

;; Copied from gnu/services/base.scm
(define* (nix-build-accounts count #:key
                             (group "nixbld")
                             (shadow shadow))
  "Return a list of COUNT user accounts for Nix build users with the given
GID."
  (unfold (cut > <> count)
          (lambda (n)
            (user-account
             (name (format #f "nixbld~2,'0d" n))
             (system? #t)
             (group group)
             (supplementary-groups (list group "kvm"))
             (comment (format #f "Nix Build User ~2d" n))
             (home-directory "/var/empty")
             (shell (file-append shadow "/sbin/nologin"))))
          1+
          1))
(define (nix-accounts _)
  "Return the user accounts and user groups."
  (cons (user-group
         (name "nixbld")
         (system? #t)

         ;; Use a fixed GID so that we can create the store with the right
         ;; owner.
         (id 40000))
        (nix-build-accounts 10 #:group "nixbld")))

(define (nix-activation _)
  ;; Return the activation gexp.
  #~(begin
      (use-modules (guix build utils)
                   (srfi srfi-26))
      (for-each (cut mkdir-p <>) '("/nix/store" "/nix/var/log"
                                   "/nix/var/nix/gcroots/per-user"
                                   "/nix/var/nix/profiles/per-user"))
      (chown "/nix/store"
             (passwd:uid (getpw "root")) (group:gid (getpw "nixbld01")))
      (chmod "/nix/store" #o775)
      (for-each (cut chmod <> #o777) '("/nix/var/nix/profiles"
                                       "/nix/var/nix/profiles/per-user"))))

(define nix-service-etc
  (match-lambda
    (($ <nix-configuration> package sandbox build-sandbox-items extra-config)
     (let ((ref-file (references-file package)))
       `(("nix/nix.conf"
          ,(computed-file
            "nix.conf"
            #~(begin
                (use-modules (srfi srfi-26)
                             (ice-9 format))
                (with-output-to-file #$output
                  (lambda _
                    (define internal-sandbox-paths
                      (call-with-input-file #$ref-file read))

                    (format #t "sandbox = ~a~%" (if #$sandbox "true" "false"))
                    ;; config.nix captures store file names.
                    (format #t "build-sandbox-paths = ~{~a ~}~%"
                            (append (list (string-append "/bin/sh=" #$bash-minimal "/bin/sh"))
                                    internal-sandbox-paths
                                    '#$build-sandbox-items))
                    (for-each (cut display <>) '#$extra-config)))))))))))

(define nix-shepherd-service
  ;; Return a <shepherd-service> for Nix.
  (match-lambda
    (($ <nix-configuration> package _ _ _ extra-options)
     (list
      (shepherd-service
       (provision '(nix-daemon))
       (documentation "Run nix-daemon.")
       (requirement '())
       (start #~(make-forkexec-constructor
                 (list (string-append #$package "/bin/nix-daemon")
                       #$@extra-options)))
       (respawn? #f)
       (stop #~(make-kill-destructor)))))))

(define nix-service-type
  (service-type
   (name 'nix)
   (extensions
    (list (service-extension shepherd-root-service-type nix-shepherd-service)
          (service-extension account-service-type nix-accounts)
          (service-extension activation-service-type nix-activation)
          (service-extension etc-service-type nix-service-etc)
          (service-extension profile-service-type
                             (compose list nix-configuration-package))))
   (description "Run the Nix daemon.")
   (default-value (nix-configuration))))

;;; nix.scm ends here
