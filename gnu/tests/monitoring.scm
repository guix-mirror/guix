;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Gábor Boskovits  <boskovits@gmail.com>
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

(define-module (gnu tests monitoring)
  #:use-module (gnu services)
  #:use-module (gnu services monitoring)
  #:use-module (gnu services networking)
  #:use-module (gnu system vm)
  #:use-module (gnu tests)
  #:use-module (guix gexp)
  #:export (%test-prometheus-node-exporter))


;;;
;;; Prometheus Node Exporter
;;;

(define* (run-prometheus-node-exporter-server-test name test-os)
  "Run tests in %PROMETHEUS-NODE-EXPORTER-OS, which has prometheus-node-exporter running."
  (define os
    (marionette-operating-system
     test-os
     #:imported-modules '((gnu services herd))))

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings '((8080 . 9100)))))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-11)
                       (srfi srfi-64)
                       (gnu build marionette)
                       (web client)
                       (web response))

          (define marionette
            (make-marionette (list #$vm)))

          (mkdir #$output)
          (chdir #$output)

          (test-begin #$name)

          (test-assert "prometheus-node-exporter running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (match (start-service 'prometheus-node-exporter)
                  (#f #f)
                  (('service response-parts ...)
                   (match (assq-ref response-parts 'running)
                     ((pid) (number? pid))))))
             marionette))

          (test-equal "http-get"
            200
            (begin
              (wait-for-tcp-port 9100 marionette)
              (let-values (((response text)
                            (http-get "http://localhost:8080")))
                (response-code response))))

          (test-end)
          (exit (= (test-runner-fail-count (test-runner-current)) 0)))))

  (gexp->derivation (string-append name "-test") test))

(define %prometheus-node-exporter-os
  (simple-operating-system
   (service dhcp-client-service-type)
   (service prometheus-node-exporter-service-type
            (prometheus-node-exporter-configuration))))

(define %test-prometheus-node-exporter
  (system-test
   (name "prometheus-node-exporter")
   (description "Connect to a running prometheus-node-exporter server.")
   (value (run-prometheus-node-exporter-server-test
           name %prometheus-node-exporter-os))))
