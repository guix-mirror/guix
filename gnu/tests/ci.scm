;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017, 2018, 2019, 2020, 2021 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2017, 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2018 Pierre-Antoine Rouby <pierre-antoine.rouby@inria.fr>
;;; Copyright © 2018 Marius Bakke <mbakke@fastmail.com>
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

(define-module (gnu tests ci)
  #:use-module (gnu tests)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system shadow)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services ci)
  #:use-module (gnu services web)
  #:use-module (gnu services networking)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:export (%test-laminar))


(define %laminar-os
  ;; Operating system under test.
  (simple-operating-system
   (service dhcp-client-service-type)
   (service laminar-service-type)))

(define* (run-laminar-test #:optional (http-port 8080))
  "Run tests in %LAMINAR-OS, which has laminar running and listening on
HTTP-PORT."
  (define os
    (marionette-operating-system
     %laminar-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings `((,http-port . 8080)))))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-11) (srfi srfi-64)
                       (ice-9 match)
                       (gnu build marionette)
                       (web uri)
                       (web client)
                       (web response))

          (define marionette
            ;; Forward the guest's HTTP-PORT, where laminar is listening, to
            ;; port 8080 in the host.
            (make-marionette (list #$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "laminar")

          (test-assert "service running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'laminar))
             marionette))

          (define* (retry-on-error f #:key times delay)
            (let loop ((attempt 1))
              (match (catch
                      #t
                      (lambda ()
                        (cons #t
                              (f)))
                      (lambda args
                        (cons #f
                              args)))
                ((#t . return-value)
                 return-value)
                ((#f . error-args)
                 (if (>= attempt times)
                     error-args
                     (begin
                       (sleep delay)
                       (loop (+ 1 attempt))))))))

          (test-equal "http-get"
            200
            (retry-on-error
             (lambda ()
               (let-values (((response text)
                             (http-get #$(format
                                          #f
                                          "http://localhost:~A/"
                                          http-port)
                                       ;; TODO: Why does decoding fail?
                                       #:decode-body? #f)))
                 (response-code response)))
             #:times 10
             #:delay 5))

          (test-end))))

  (gexp->derivation "laminar-test" test))

(define %test-laminar
  (system-test
   (name "laminar")
   (description "Connect to a running Laminar server.")
   (value (run-laminar-test))))
