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

(define-module (gnu tests virtualization)
  #:use-module (gnu tests)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services dbus)
  #:use-module (gnu services networking)
  #:use-module (gnu services virtualization)
  #:use-module (gnu packages virtualization)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:export (%test-libvirt))

(define %libvirt-os
  (simple-operating-system
   (service dhcp-client-service-type)
   (dbus-service)
   (polkit-service)
   (service libvirt-service-type)))

(define (run-libvirt-test)
  "Run tests in %LIBVIRT-OS."
  (define os
    (marionette-operating-system
     %libvirt-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
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

          (test-begin "libvirt")

          (test-assert "service running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (match (start-service 'libvirtd)
                  (#f #f)
                  (('service response-parts ...)
                   (match (assq-ref response-parts 'running)
                     ((pid) (number? pid))))))
             marionette))

          (test-eq "fetch version"
            0
            (marionette-eval
             `(begin
                (system* ,(string-append #$libvirt "/bin/virsh")
                         "-c" "qemu:///system" "version"))
             marionette))

          (test-end)
          (exit (= (test-runner-fail-count (test-runner-current)) 0)))))

  (gexp->derivation "libvirt-test" test))

(define %test-libvirt
  (system-test
   (name "libvirt")
   (description "Connect to the running LIBVIRT service.")
   (value (run-libvirt-test))))
