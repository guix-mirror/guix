;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
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

(define-module (gnu services games)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages games)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:export (wesnothd-configuration
            wesnothd-configuration?
            wesnothd-service-type))

;;;
;;; The Battle for Wesnoth server
;;;

(define-record-type* <wesnothd-configuration>
  wesnothd-configuration make-wesnothd-configuration wesnothd-configuration?
  (package wesnothd-configuration-package
           (default wesnoth-server))
  (port wesnothd-configuration-port
        (default 15000)))

(define %wesnothd-accounts
  (list (user-account
         (name "wesnothd")
         (group "wesnothd")
         (system? #t)
         (comment "Wesnoth daemon user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))
        (user-group
         (name "wesnothd")
         (system? #t))))

(define wesnothd-shepherd-service
  (match-lambda
    (($ <wesnothd-configuration> package port)
     (with-imported-modules (source-module-closure
                             '((gnu build shepherd)))
       (shepherd-service
        (documentation "The Battle for Wesnoth server")
        (provision '(wesnoth-daemon))
        (requirement '(networking))
        (modules '((gnu build shepherd)))
        (start #~(make-forkexec-constructor/container
                  (list #$(file-append package "/bin/wesnothd")
                        "-p" #$(number->string port))
                  #:user "wesnothd" #:group "wesnothd"))
        (stop #~(make-kill-destructor)))))))

(define wesnothd-service-type
  (service-type
   (name 'wesnothd)
   (description
    "Run The Battle for Wesnoth server @command{wesnothd}.")
   (extensions
    (list (service-extension account-service-type
                             (const %wesnothd-accounts))
          (service-extension shepherd-root-service-type
                             (compose list wesnothd-shepherd-service))))
   (default-value (wesnothd-configuration))))
