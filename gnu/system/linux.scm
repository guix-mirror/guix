;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu system linux)
  #:use-module (guix store)
  #:use-module (guix records)
  #:use-module (guix derivations)
  #:use-module (guix monads)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module ((guix utils) #:select (%current-system))
  #:export (pam-service
            pam-entry
            pam-services->directory
            %pam-other-services
            unix-pam-service))

;;; Commentary:
;;;
;;; Configuration of Linux-related things, including pluggable authentication
;;; modules (PAM).
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
  (control    pam-entry-control)                  ; string
  (module     pam-entry-module)                   ; file name
  (arguments  pam-entry-arguments                 ; list of strings
              (default '())))

(define (pam-service->configuration service)
  "Return the configuration string for SERVICE, to be dumped in
/etc/pam.d/NAME, where NAME is the name of SERVICE."
  (define (entry->string type entry)
    (match entry
      (($ <pam-entry> control module (arguments ...))
       (string-append type "  "
                      control " " module " "
                      (string-join arguments)
                      "\n"))))

  (match service
    (($ <pam-service> name account auth password session)
     (string-concatenate
      (append (map (cut entry->string "account" <>) account)
              (map (cut entry->string "auth" <>) auth)
              (map (cut entry->string "password" <>) password)
              (map (cut entry->string "session" <>) session))))))

(define (pam-services->directory services)
  "Return the derivation to build the configuration directory to be used as
/etc/pam.d for SERVICES."
  (mlet %store-monad
      ((names -> (map pam-service-name services))
       (files (mapm %store-monad
                    (match-lambda
                     ((and service ($ <pam-service> name))
                      (let ((config (pam-service->configuration service)))
                        (text-file (string-append name ".pam") config))))

                    ;; XXX: Eventually, SERVICES may be a list of monadic
                    ;; values instead of plain values.
                    (map return services))))
    (define builder
      '(begin
         (use-modules (ice-9 match))

         (let ((out (assoc-ref %outputs "out")))
           (mkdir out)
           (for-each (match-lambda
                      ((name . file)
                       (symlink file (string-append out "/" name))))
                     %build-inputs)
           #t)))

    (derivation-expression "pam.d" builder
                           #:inputs (zip names files))))

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
               (module "pam_unix.so"))))
    (lambda* (name #:key allow-empty-passwords? motd)
      "Return a standard Unix-style PAM service for NAME.  When
ALLOW-EMPTY-PASSWORDS? is true, allow empty passwords.  When MOTD is true, it
should be the name of a file used as the message-of-the-day."
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
         (password (list unix))
         (session (if motd
                      (list unix
                            (pam-entry
                             (control "optional")
                             (module "pam_motd.so")
                             (arguments (list (string-append "motd=" motd)))))
                      (list unix))))))))

;;; linux.scm ends here
