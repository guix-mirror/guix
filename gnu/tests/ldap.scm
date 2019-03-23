;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu tests ldap)
  #:use-module (gnu tests)
  #:use-module (gnu system)
  #:use-module (gnu system nss)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services authentication)
  #:use-module (gnu services networking)
  #:use-module (gnu packages base)
  #:use-module (gnu packages openldap)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:export (%test-ldap))

(define %ldap-os
  (let ((simple
         (simple-operating-system
          (service dhcp-client-service-type)
          (service nslcd-service-type))))
    (operating-system
      (inherit simple)
      (name-service-switch
       (let ((services (list (name-service (name "db"))
                             (name-service (name "files"))
                             (name-service (name "ldap")))))
         (name-service-switch
          (inherit %mdns-host-lookup-nss)
          (password services)
          (shadow   services)
          (group    services)
          (netgroup services)
          (gshadow  services)))))))

(define (run-ldap-test)
  "Run tests in %LDAP-OS."
  (define os
    (marionette-operating-system
     %ldap-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine os))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-11) (srfi srfi-64)
                       (gnu build marionette))

          (define marionette
            (make-marionette (list #$vm)))

          (mkdir #$output)
          (chdir #$output)

          (test-begin "ldap")

          ;; Set up LDAP directory server
          (test-assert "LDAP server instance running"
            (marionette-eval
             '(begin
                (with-output-to-file "instance.inf"
                  (lambda ()
                    (display "[general]
config_version = 2

\n[slapd]
root_password = SECRET
user = root
group = root

\n[backend-userroot]
sample_entries = yes
suffix = dc=example,dc=com")))
                (and
                 ;; Create instance
                 (zero? (system* #$(file-append 389-ds-base "/sbin/dscreate")
                                     "-v" "from-file" "instance.inf"))
                 ;; Start instance
                 (zero? (system* #$(file-append 389-ds-base "/sbin/dsctl")
                                 "localhost" "start"))
                 ;; Create user account
                 (zero? (system* #$(file-append 389-ds-base "/sbin/dsidm")
                                 "-b" "dc=example,dc=com"
                                 "localhost" "user" "create"
                                 "--uid" "eva" "--cn" "Eva Lu Ator"
                                 "--displayName" "Eva Lu Ator"
                                 "--uidNumber" "1234" "--gidNumber" "2345"
                                 "--homeDirectory" "/home/eva"))))
             marionette))

          (test-assert "Manager can bind to LDAP server instance"
            (marionette-eval
             '(zero? (system* #$(file-append openldap "/bin/ldapwhoami")
                              "-H" "ldap://localhost" "-D"
                              "cn=Directory Manager" "-w" "SECRET"))
             marionette))

          ;; Wait for nslcd to be up and running.
          (test-assert "nslcd service running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (match (start-service 'nslcd)
                  (#f #f)
                  (('service response-parts ...)
                   (match (assq-ref response-parts 'running)
                     ((pid) (number? pid))))))
             marionette))

          (test-assert "nslcd produces a log file"
            (marionette-eval
             '(file-exists? "/var/log/nslcd")
             marionette))

          (test-assert "Can query LDAP user accounts"
            (marionette-eval
             '(begin
                ;; TODO: This shouldn't be necessary, but unfortunately it
                ;; really is needed to discover LDAP accounts with "id".
                (setenv "LD_LIBRARY_PATH"
                        #$(file-append nss-pam-ldapd "/lib"))
                (zero? (system* #$(file-append coreutils "/bin/id") "eva")))
             marionette))

          (test-assert "Can become LDAP user"
            (marionette-eval
             '(zero? (system* "/run/setuid-programs/su" "eva" "-c"
                              #$(file-append coreutils "/bin/true")))
             marionette))

          (test-end)
          (exit (= (test-runner-fail-count (test-runner-current)) 0)))))

  (gexp->derivation "ldap-test" test))

(define %test-ldap
  (system-test
   (name "ldap")
   (description "Run an LDAP directory server and authenticate against it.")
   (value (run-ldap-test))))
