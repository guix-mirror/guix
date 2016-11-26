;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 John Darrington <jmd@gnu.org>
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

(define-module (gnu services kerberos)
  #:use-module (gnu packages admin)
  #:use-module (gnu services)
  #:use-module (gnu system pam)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (pam-krb5-configuration
            pam-krb5-configuration?
            pam-krb5-service-type))

(define-record-type* <pam-krb5-configuration>
  pam-krb5-configuration  make-pam-krb5-configuration
  pam-krb5-configuration?
  (pam-krb5               pam-krb5-configuration-pam-krb5
                          (default pam-krb5))
  (minimum-uid            pam-krb5-configuration-minimum-uid
                          (default 1000)))

(define (pam-krb5-pam-service config)
  "Return a PAM service for Kerberos authentication."
  (lambda (pam)
    (define pam-krb5-module
      #~(string-append #$(pam-krb5-configuration-pam-krb5 config)
                       "/lib/security/pam_krb5.so"))

    (let ((pam-krb5-sufficient
           (pam-entry
            (control "sufficient")
            (module pam-krb5-module)
            (arguments
             (list
              (format #f "minimum_uid=~a"
                      (pam-krb5-configuration-minimum-uid config)))))))
      (pam-service
       (inherit pam)
       (auth (cons* pam-krb5-sufficient
                    (pam-service-auth pam)))
       (session (cons* pam-krb5-sufficient
                       (pam-service-session pam)))
       (account (cons* pam-krb5-sufficient
                       (pam-service-account pam)))))))

(define (pam-krb5-pam-services config)
  (list (pam-krb5-pam-service config)))

(define pam-krb5-service-type
  (service-type (name 'pam-krb5)
                (extensions
                 (list
                  (service-extension pam-root-service-type
                                     pam-krb5-pam-services)))))
