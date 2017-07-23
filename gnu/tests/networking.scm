;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
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

(define-module (gnu tests networking)
  #:use-module (gnu tests)
  #:use-module (gnu system)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services networking)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (gnu packages bash)
  #:export (%test-inetd))

(define %inetd-os
  ;; Operating system with 2 inetd services.
  (simple-operating-system
   (dhcp-client-service)
   (service inetd-service-type
            (inetd-configuration
             (entries (list
                       (inetd-entry
                        (name "echo")
                        (socket-type 'stream)
                        (protocol "tcp")
                        (wait? #f)
                        (user "root"))
                       (inetd-entry
                        (name "dict")
                        (socket-type 'stream)
                        (protocol "tcp")
                        (wait? #f)
                        (user "root")
                        (program (file-append bash
                                              "/bin/bash"))
                        (arguments
                         (list "bash" (plain-file "my-dict.sh" "\
while read line
do
    if [[ $line =~ ^DEFINE\\ (.*)$ ]]
    then
        case ${BASH_REMATCH[1]} in
            Guix)
                echo GNU Guix is a package management tool for the GNU system.
                ;;
            G-expression)
                echo Like an S-expression but with a G.
                ;;
            *)
                echo NO DEFINITION FOUND
                ;;
        esac
    else
        echo ERROR
    fi
done" ))))))))))

(define* (run-inetd-test)
  "Run tests in %INETD-OS, where the inetd service provides an echo service on
port 7, and a dict service on port 2628."
  (define os
    (marionette-operating-system %inetd-os))

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings `((8007 . 7)
                         (8628 . 2628)))))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (ice-9 rdelim)
                       (srfi srfi-64)
                       (gnu build marionette))
          (define marionette
            (make-marionette (list #$vm)))

          (mkdir #$output)
          (chdir #$output)

          (test-begin "inetd")

          ;; Make sure the PID file is created.
          (test-assert "PID file"
            (marionette-eval
             '(file-exists? "/var/run/inetd.pid")
             marionette))

          ;; Test the echo service.
          (test-equal "echo response"
            "Hello, Guix!"
            (let ((echo (socket PF_INET SOCK_STREAM 0))
                  (addr (make-socket-address AF_INET INADDR_LOOPBACK 8007)))
              (connect echo addr)
              (display "Hello, Guix!\n" echo)
              (let ((response (read-line echo)))
                (close echo)
                response)))

          ;; Test the dict service
          (test-equal "dict response"
            "GNU Guix is a package management tool for the GNU system."
            (let ((dict (socket PF_INET SOCK_STREAM 0))
                  (addr (make-socket-address AF_INET INADDR_LOOPBACK 8628)))
              (connect dict addr)
              (display "DEFINE Guix\n" dict)
              (let ((response (read-line dict)))
                (close dict)
                response)))

          (test-end)
          (exit (= (test-runner-fail-count (test-runner-current)) 0)))))

  (gexp->derivation "inetd-test" test))

(define %test-inetd
  (system-test
   (name "inetd")
   (description "Connect to a host with an INETD server.")
   (value (run-inetd-test))))
