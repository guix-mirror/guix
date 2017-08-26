;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Christopher Baines <mail@cbaines.net>
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

(define-module (gnu tests admin)
  #:use-module (gnu tests)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system shadow)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services networking)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:export (%test-tailon))

(define %tailon-os
  ;; Operating system under test.
  (simple-operating-system
   (dhcp-client-service)
   (service tailon-service-type
            (tailon-configuration
             (config-file
              (tailon-configuration-file
               (bind "0.0.0.0:8080")))))))

(define* (run-tailon-test #:optional (http-port 8081))
  "Run tests in %TAILON-OS, which has tailon running and listening on
HTTP-PORT."
  (define os
    (marionette-operating-system
     %tailon-os
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
            ;; Forward the guest's HTTP-PORT, where tailon is listening, to
            ;; port 8080 in the host.
            (make-marionette (list #$vm)))

          (mkdir #$output)
          (chdir #$output)

          (test-begin "tailon")

          (test-eq "service running"
            'running!
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'tailon)
                'running!)
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
                                       #:decode-body? #t)))
                 (response-code response)))
             #:times 10
             #:delay 5))

          (test-end)
          (exit (= (test-runner-fail-count (test-runner-current)) 0)))))

  (gexp->derivation "tailon-test" test))

(define %test-tailon
  (system-test
   (name "tailon")
   (description "Connect to a running Tailon server.")
   (value (run-tailon-test))))
