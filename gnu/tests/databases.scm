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

(define-module (gnu tests databases)
  #:use-module (gnu tests)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system shadow)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services databases)
  #:use-module (gnu services networking)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:export (%test-memcached))

(define %memcached-os
  (simple-operating-system
   (dhcp-client-service)
   (service memcached-service-type)))

(define* (run-memcached-test #:optional (port 11211))
  "Run tests in %MEMCACHED-OS, forwarding PORT."
  (define os
    (marionette-operating-system
     %memcached-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings `((11211 . ,port)))))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-11) (srfi srfi-64)
                       (gnu build marionette)
                       (ice-9 rdelim))

          (define marionette
            (make-marionette (list #$vm)))

          (mkdir #$output)
          (chdir #$output)

          (test-begin "memcached")

          ;; Wait for memcached to be up and running.
          (test-assert "service running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (match (start-service 'memcached)
                  (#f #f)
                  (('service response-parts ...)
                   (match (assq-ref response-parts 'running)
                     ((pid) (number? pid))))))
             marionette))

          (let* ((ai (car (getaddrinfo "localhost"
                                       #$(number->string port))))
                 (s  (socket (addrinfo:fam ai)
                             (addrinfo:socktype ai)
                             (addrinfo:protocol ai)))
                 (key "testkey")
                 (value "guix"))
            (connect s (addrinfo:addr ai))

            (test-equal "set"
              "STORED\r"
              (begin
                (simple-format s "set ~A 0 60 ~A\r\n~A\r\n"
                               key
                               (string-length value)
                               value)
                (read-line s)))

            (test-equal "get"
              (simple-format #f "VALUE ~A 0 ~A\r~A\r"
                             key
                             (string-length value)
                             value)
              (begin
                (simple-format s "get ~A\r\n" key)
                (string-append
                 (read-line s)
                 (read-line s))))

            (close-port s))

          ;; There should be a log file in here.
          (test-assert "log file"
            (marionette-eval
             '(file-exists? "/var/log/memcached")
             marionette))

          (test-end)
          (exit (= (test-runner-fail-count (test-runner-current)) 0)))))

  (gexp->derivation "memcached-test" test))

(define %test-memcached
  (system-test
   (name "memcached")
   (description "Connect to a running MEMCACHED server.")
   (value (run-memcached-test))))
