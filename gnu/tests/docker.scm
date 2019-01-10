;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Danny Milosavljevic <dannym@scratchpost.org>
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

(define-module (gnu tests docker)
  #:use-module (gnu tests)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services dbus)
  #:use-module (gnu services networking)
  #:use-module (gnu services docker)
  #:use-module (gnu services desktop)
  #:use-module (gnu packages docker)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:export (%test-docker))

(define %docker-os
  (simple-operating-system
   (service dhcp-client-service-type)
   (dbus-service)
   (polkit-service)
   (service elogind-service-type)
   (service docker-service-type)))

(define (run-docker-test)
  "Run tests in %DOCKER-OS."
  (define os
    (marionette-operating-system
     %docker-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
     (memory-size 500)
     (disk-image-size (* 250 (expt 2 20)))
     (port-forwardings '())))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-11) (srfi srfi-64)
                       (gnu build marionette))

          (define marionette
            (make-marionette (list #$vm)))

          (mkdir #$output)
          (chdir #$output)

          (test-begin "docker")

          (test-assert "service running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (match (start-service 'dockerd)
                  (#f #f)
                  (('service response-parts ...)
                   (match (assq-ref response-parts 'running)
                     ((pid) (number? pid))))))
             marionette))

          (test-eq "fetch version"
            0
            (marionette-eval
             `(begin
                (system* ,(string-append #$docker-cli "/bin/docker")
                         "version"))
             marionette))

          (test-end)
          (exit (= (test-runner-fail-count (test-runner-current)) 0)))))

  (gexp->derivation "docker-test" test))

(define %test-docker
  (system-test
   (name "docker")
   (description "Connect to the running Docker service.")
   (value (run-docker-test))))
