;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Oleg Pykhalov <go.wigust@gmail.com>
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
  #:use-module (ice-9 format)
  #:export (nix-service-type))

;;; Commentary:
;;;
;;; This module provides a service definition for the Nix daemon.
;;;
;;; Code:


;;;
;;; Accounts
;;;

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
  "Return the activation gexp."
  (with-imported-modules '((guix build utils))
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
                                         "/nix/var/nix/profiles/per-user")))))

(define (nix-shepherd-service _)
  "Return a <shepherd-service> for Nix."
  (list
   (shepherd-service
    (provision '(nix-daemon))
    (documentation "Run nix-daemon.")
    (requirement '())
    (start #~(make-forkexec-constructor
              (list (string-append #$nix "/bin/nix-daemon"))))
    (respawn? #f)
    (stop #~(make-kill-destructor)))))

(define nix-service-type
  (service-type
   (name 'nix)
   (extensions
    (list (service-extension shepherd-root-service-type nix-shepherd-service)
          (service-extension account-service-type nix-accounts)
          (service-extension activation-service-type nix-activation)))
   (default-value '())
   (description "Run the Nix daemon.")))

;;; nix.scm ends here
