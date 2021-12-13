;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
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

(define-module (gnu tests dict)
  #:use-module (gnu tests)
  #:use-module (gnu system)
  #:use-module (gnu system nss)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services dict)
  #:use-module (gnu services networking)
  #:use-module (gnu packages wordnet)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix packages)
  #:use-module (guix modules)
  #:export (%test-dicod))

(define %dicod-os
  (simple-operating-system
   (service dhcp-client-service-type)
   (service dicod-service-type
            (dicod-configuration
             (interfaces '("0.0.0.0"))
             (handlers (list (dicod-handler
                              (name "wordnet")
                              (module "dictorg")
                              (options
                               ;; XXX: Not useful since WordNet does not
                               ;; provide DICT-formatted data?
                               (list #~(string-append "dbdir=" #$wordnet))))))
             (databases (list (dicod-database
                               (name "wordnet")
                               (complex? #t)
                               (handler "wordnet")
                               (options '("database=wn")))
                              %dicod-database:gcide))))))

(define* (run-dicod-test)
  "Run tests of 'dicod-service-type'."
  (define os
    (marionette-operating-system
     %dicod-os
     #:imported-modules
     (source-module-closure '((gnu services herd)))))

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings '((8000 . 2628)))))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (ice-9 rdelim)
                       (ice-9 regex)
                       (srfi srfi-64)
                       (gnu build marionette))
          (define marionette
            ;; Forward the guest's DICT port to local port 8000.
            (make-marionette (list #$vm)))

          (define %dico-socket
            (socket PF_INET SOCK_STREAM 0))

          (test-runner-current (system-test-runner #$output))
          (test-begin "dicod")

          ;; Wait for the service to be started.
          (test-assert "service is running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'dicod))
             marionette))

          ;; Wait until dicod is actually listening.
          ;; TODO: Use a PID file instead.
          (test-assert "connect inside"
            (wait-for-tcp-port 2628 marionette))

          (test-assert "connect"
            (let ((addr (make-socket-address AF_INET INADDR_LOOPBACK 8000)))
              (connect %dico-socket addr)
              (read-line %dico-socket 'concat)))

          (test-equal "CLIENT"
            "250 ok\r\n"
            (begin
              (display "CLIENT \"GNU Guile\"\r\n" %dico-socket)
              (read-line %dico-socket 'concat)))

          (test-assert "DEFINE"
            (begin
              (display "DEFINE ! hello\r\n" %dico-socket)
              (display "QUIT\r\n" %dico-socket)
              (let ((result (read-string %dico-socket)))
                (and (string-contains result "gcide")
                     (string-contains result "hello")
                     result))))

          (test-end))))

  (gexp->derivation "dicod" test))

(define %test-dicod
  (system-test
   (name "dicod")
   (description "Connect to the dicod DICT server.")
   (value (run-dicod-test))))
