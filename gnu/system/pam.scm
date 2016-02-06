;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu system pam)
  #:use-module (guix records)
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module ((guix utils) #:select (%current-system))
  #:export (pam-service
            pam-service-name
            pam-service-account
            pam-service-auth
            pam-service-password
            pam-service-session

            pam-entry
            pam-entry-control
            pam-entry-module
            pam-entry-arguments

            pam-services->directory
            unix-pam-service
            base-pam-services

            pam-root-service-type
            pam-root-service))

;;; Commentary:
;;;
;;; Configuration of the pluggable authentication modules (PAM).
;;;
;;; Code:

;; PAM services (see
;; <http://www.linux-pam.org/Linux-PAM-html/sag-configuration-file.html>.)
(define-record-type* <pam-service> pam-service
  make-pam-service
  pam-service?
  (name       pam-service-name)                   ; string

  ;; The four "management groups".
  (account    pam-service-account                 ; list of <pam-entry>
              (default '()))
  (auth       pam-service-auth
              (default '()))
  (password   pam-service-password
              (default '()))
  (session    pam-service-session
              (default '())))

(define-record-type* <pam-entry> pam-entry
  make-pam-entry
  pam-entry?
  (control    pam-entry-control)         ; string
  (module     pam-entry-module)          ; file name
  (arguments  pam-entry-arguments        ; list of string-valued g-expressions
              (default '())))

(define (pam-service->configuration service)
  "Return the derivation building the configuration file for SERVICE, to be
dumped in /etc/pam.d/NAME, where NAME is the name of SERVICE."
  (define (entry->gexp type entry)
    (match entry
      (($ <pam-entry> control module (arguments ...))
       #~(format #t "~a ~a ~a ~a~%"
                 #$type #$control #$module
                 (string-join (list #$@arguments))))))

  (match service
    (($ <pam-service> name account auth password session)
     (define builder
       #~(begin
           (with-output-to-file #$output
             (lambda ()
               #$@(append (map (cut entry->gexp "account" <>) account)
                          (map (cut entry->gexp "auth" <>) auth)
                          (map (cut entry->gexp "password" <>) password)
                          (map (cut entry->gexp "session" <>) session))
               #t))))

     (computed-file name builder))))

(define (pam-services->directory services)
  "Return the derivation to build the configuration directory to be used as
/etc/pam.d for SERVICES."
  (let ((names (map pam-service-name services))
        (files (map pam-service->configuration services)))
    (define builder
      #~(begin
          (use-modules (ice-9 match)
                       (srfi srfi-1))

          (mkdir #$output)
          (for-each (match-lambda
                      ((name file)
                       (symlink file (string-append #$output "/" name))))

                    ;; Since <pam-service> objects cannot be compared with
                    ;; 'equal?' since they contain gexps, which contain
                    ;; closures, use 'delete-duplicates' on the build-side
                    ;; instead.  See <http://bugs.gnu.org/20037>.
                    (delete-duplicates '#$(zip names files)))))

    (computed-file "pam.d" builder)))

(define %pam-other-services
  ;; The "other" PAM configuration, which denies everything (see
  ;; <http://www.linux-pam.org/Linux-PAM-html/sag-configuration-example.html>.)
  (let ((deny (pam-entry
               (control "required")
               (module "pam_deny.so"))))
    (pam-service
     (name "other")
     (account (list deny))
     (auth (list deny))
     (password (list deny))
     (session (list deny)))))

(define unix-pam-service
  (let ((unix (pam-entry
               (control "required")
               (module "pam_unix.so")))
        (env  (pam-entry ; to honor /etc/environment.
               (control "required")
               (module "pam_env.so"))))
    (lambda* (name #:key allow-empty-passwords? motd)
      "Return a standard Unix-style PAM service for NAME.  When
ALLOW-EMPTY-PASSWORDS? is true, allow empty passwords.  When MOTD is true, it
should be a file-like object used as the message-of-the-day."
      ;; See <http://www.linux-pam.org/Linux-PAM-html/sag-configuration-example.html>.
      (let ((name* name))
        (pam-service
         (name name*)
         (account (list unix))
         (auth (list (if allow-empty-passwords?
                         (pam-entry
                          (control "required")
                          (module "pam_unix.so")
                          (arguments '("nullok")))
                         unix)))
         (password (list (pam-entry
                          (control "required")
                          (module "pam_unix.so")
                          ;; Store SHA-512 encrypted passwords in /etc/shadow.
                          (arguments '("sha512" "shadow")))))
         (session (if motd
                      (list env unix
                            (pam-entry
                             (control "optional")
                             (module "pam_motd.so")
                             (arguments
                              (list #~(string-append "motd=" #$motd)))))
                      (list env unix))))))))

(define (rootok-pam-service command)
  "Return a PAM service for COMMAND such that 'root' does not need to
authenticate to run COMMAND."
  (let ((unix (pam-entry
               (control "required")
               (module "pam_unix.so"))))
    (pam-service
     (name command)
     (account (list unix))
     (auth (list (pam-entry
                  (control "sufficient")
                  (module "pam_rootok.so"))))
     (password (list unix))
     (session (list unix)))))

(define* (base-pam-services #:key allow-empty-passwords?)
  "Return the list of basic PAM services everyone would want."
  ;; TODO: Add other Shadow programs?
  (append (list %pam-other-services)

          ;; These programs are setuid-root.
          (map (cut unix-pam-service <>
                    #:allow-empty-passwords? allow-empty-passwords?)
               '("su" "passwd" "sudo"))

          ;; These programs are not setuid-root, and we want root to be able
          ;; to run them without having to authenticate (notably because
          ;; 'useradd' and 'groupadd' are run during system activation.)
          (map rootok-pam-service
               '("useradd" "userdel" "usermod"
                 "groupadd" "groupdel" "groupmod"))))


;;;
;;; PAM root service.
;;;

;; Overall PAM configuration: a list of services, plus a procedure that takes
;; one <pam-service> and returns a <pam-service>.  The procedure is used to
;; implement cross-cutting concerns such as the use of the 'elogind.so'
;; session module that keeps track of logged-in users.
(define-record-type* <pam-configuration>
  pam-configuration make-pam-configuration? pam-configuration?
  (services  pam-configuration-services)          ;list of <pam-service>
  (transform pam-configuration-transform))        ;procedure

(define (/etc-entry config)
  "Return the /etc/pam.d entry corresponding to CONFIG."
  (match config
    (($ <pam-configuration> services transform)
     (let ((services (map transform services)))
       `(("pam.d" ,(pam-services->directory services)))))))

(define (extend-configuration initial extensions)
  "Extend INITIAL with NEW."
  (let-values (((services procs)
                (partition pam-service? extensions)))
    (pam-configuration
     (services (append (pam-configuration-services initial)
                       services))
     (transform (apply compose
                       (pam-configuration-transform initial)
                       procs)))))

(define pam-root-service-type
  (service-type (name 'pam)
                (extensions (list (service-extension etc-service-type
                                                     /etc-entry)))

                ;; Arguments include <pam-service> as well as procedures.
                (compose concatenate)
                (extend extend-configuration)))

(define* (pam-root-service base #:key (transform identity))
  "The \"root\" PAM service, which collects <pam-service> instance and turns
them into a /etc/pam.d directory, including the <pam-service> listed in BASE.
TRANSFORM is a procedure that takes a <pam-service> and returns a
<pam-service>.  It can be used to implement cross-cutting concerns that affect
all the PAM services."
  (service pam-root-service-type
           (pam-configuration (services base)
                              (transform transform))))

;;; linux.scm ends here
