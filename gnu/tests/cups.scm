;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
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

(define-module (gnu tests cups)
  #:use-module (gnu tests)
  #:use-module (gnu system)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services cups)
  #:use-module (gnu services networking)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:export (%test-cups))

;;;
;;; Test the Common Unix Printing System.
;;;

(define* (run-cups-test os-configuration #:optional (cups-port 631))
  (define os
    (marionette-operating-system os-configuration
                                 #:imported-modules '((gnu services herd))))

  (define forwarded-port 8080)

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings `((,forwarded-port . ,cups-port)))))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette)
                       (srfi srfi-11) (srfi srfi-64)
                       (web client) (web response))

          (define marionette
            (make-marionette (list #$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "cups")

          ;; Wait for the web interface to become ready.
          (wait-for-tcp-port #$cups-port marionette)

          (test-equal "http-get default page"
            200
            (let-values
                (((response text)
                  (http-get #$(simple-format
                               #f "http://localhost:~A/" forwarded-port)
                            #:decode-body? #t)))
              (response-code response)))

          (test-equal "http-get admin page"
            200
            (let-values
                (((response text)
                  (http-get #$(simple-format
                               #f "http://localhost:~A/admin" forwarded-port)
                            #:decode-body? #t)))
              (response-code response)))

          (test-end))))

  (gexp->derivation "cups-test" test))

(define %cups-os
  (simple-operating-system
   (service dhcp-client-service-type)
   (service cups-service-type
            (cups-configuration
             (web-interface? #t)
             ;; Listen on all interfaces instead of just localhost so we
             ;; can access the web interface "remotely".
             (listen '("*:631" "/var/run/cups/cups.sock"))
             ;; Add access controls for the Qemu-managed network.
             (location-access-controls
              (list (location-access-control
                     (path "/")
                     (access-controls '("Order allow,deny"
                                        "Allow from 10.0.0.0/8")))
                    (location-access-control
                     (path "/admin")
                     (access-controls '("Order allow,deny"
                                        "Allow from 10.0.0.0/8")))
                    (location-access-control
                     (path "/admin/conf")
                     (access-controls '("Order allow,deny"
                                        "AuthType Basic"
                                        "Require user @SYSTEM"
                                        "Allow localhost")))))))))

(define %test-cups
  (system-test
   (name "cups")
   (description "Test the CUPS print server")
   (value (run-cups-test %cups-os))))

