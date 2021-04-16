;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2019, 2020, 2021 Christopher Baines <mail@cbaines.net>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gnu services ci)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages ci)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services admin)
  #:use-module (gnu system shadow)
  #:use-module (ice-9 match)
  #:export (laminar-configuration
            laminar-configuration?
            laminar-configuration-home-directory
            laminar-configuration-bind-http
            laminar-configuration-bind-rpc
            laminar-configuration-title
            laminar-configuration-keep-rundirs
            laminar-configuration-archive-url
            laminar-configuration-base-url

            laminar-service-type))

;;;; Commentary:
;;;
;;; This module implements a service that to run instances of Laminar, a
;;; continuous integration tool.
;;;
;;;; Code:

(define-record-type* <laminar-configuration>
  laminar-configuration make-laminar-configuration
  laminar-configuration?
  (laminar          laminars-configuration-laminar
                    (default laminar))
  (home-directory   laminar-configuration-home-directory
                    (default "/var/lib/laminar"))
  (bind-http        laminar-configuration-bind-http
                    (default "*:8080"))
  (bind-rpc         laminar-configuration-bind-rpc
                    (default "unix-abstract:laminar"))
  (title            laminar-configuration-title
                    (default "Laminar"))
  (keep-rundirs     laminar-keep-rundirs
                    (default 0))
  (archive-url      laminar-archive-url
                    (default #f))
  (base-url         laminar-base-url
                    (default #f)))

(define laminar-shepherd-service
  (match-lambda
    (($ <laminar-configuration> laminar home-directory
                                bind-http bind-rpc
                                title keep-rundirs archive-url
                                base-url)
     (list (shepherd-service
            (documentation "Run Laminar.")
            (provision '(laminar))
            (requirement '(networking))
            (start #~(make-forkexec-constructor
                      (list #$(file-append laminar "/sbin/laminard"))
                      #:environment-variables
                      `(,(string-append "LAMINAR_HOME="
                                        #$home-directory)
                        ,(string-append "LAMINAR_BIND_HTTP="
                                        #$bind-http)
                        ,(string-append "LAMINAR_TITLE="
                                        #$title)
                        ,(string-append "LAMINAR_KEEP_RUNDIRS="
                                        #$(number->string
                                           keep-rundirs))
                        ,@(if #$archive-url
                              (list
                               (string-append "LAMINAR_ARCHIVE_URL="
                                              #$archive-url))
                              '())
                        ,@(if #$base-url
                              (list
                               (string-append "LAMINAR_BASE_URL="
                                              #$base-url))
                              '()))
                      #:user "laminar"
                      #:group "laminar"))
            (stop #~(make-kill-destructor)))))))

(define (laminar-account config)
  "Return the user accounts and user groups for CONFIG."
  (list (user-group
         (name "laminar")
         (system? #t))
        (user-account
         (name "laminar")
         (group "laminar")
         (system? #t)
         (comment "Laminar privilege separation user")
         (home-directory (laminar-configuration-home-directory config))
         (shell #~(string-append #$shadow "/sbin/nologin")))))

(define laminar-service-type
  (service-type
   (name 'laminar)
   (extensions
    (list
     (service-extension shepherd-root-service-type laminar-shepherd-service)
     (service-extension account-service-type laminar-account)))
   (default-value (laminar-configuration))
   (description
    "Run the Laminar continuous integration service.")))
