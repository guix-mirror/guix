;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2021 Pierre Langlois <pierre.langlois@gmx.com>
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
  #:use-module (gnu image)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system image)
  #:use-module (gnu system images hurd)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services dbus)
  #:use-module (gnu services networking)
  #:use-module (gnu services virtualization)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages ssh)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix store)
  #:export (%test-libvirt
            %test-childhurd))


;;;
;;; Libvirt.
;;;

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
                (chdir "/tmp")
                (system* ,(string-append #$libvirt "/bin/virsh")
                         "-c" "qemu:///system" "version"))
             marionette))

          (test-eq "connect"
            0
            (marionette-eval
             `(begin
                (chdir "/tmp")
                (system* ,(string-append #$libvirt "/bin/virsh")
                         "-c" "qemu:///system" "connect"))
             marionette))

          (test-end)
          (exit (= (test-runner-fail-count (test-runner-current)) 0)))))

  (gexp->derivation "libvirt-test" test))

(define %test-libvirt
  (system-test
   (name "libvirt")
   (description "Connect to the running LIBVIRT service.")
   (value (run-libvirt-test))))


;;;
;;; GNU/Hurd virtual machines, aka. childhurds.
;;;

;; Copy of `hurd-vm-disk-image', using plain disk-image for test
(define (hurd-vm-disk-image-raw config)
  (let ((os ((@@ (gnu services virtualization) secret-service-operating-system)
             (hurd-vm-configuration-os config)))
        (disk-size (hurd-vm-configuration-disk-size config)))
    (system-image
     (image
      (inherit hurd-disk-image)
      (format 'disk-image)
      (size disk-size)
      (operating-system os)))))

(define %childhurd-os
  (simple-operating-system
   (service dhcp-client-service-type)
   (service hurd-vm-service-type
            (hurd-vm-configuration
             (image (hurd-vm-disk-image-raw this-record))))))

(define (run-childhurd-test)
  (define os
    (marionette-operating-system
     %childhurd-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
     (memory-size (* 1024 3))))

  (define run-uname-over-ssh
    ;; Program that runs 'uname' over SSH and prints the result on standard
    ;; output.
    (let ()
      (define run
        (with-extensions (list guile-ssh)
          #~(begin
              (use-modules (ssh session)
                           (ssh auth)
                           (ssh popen)
                           (ice-9 match)
                           (ice-9 textual-ports))

              (let ((session (make-session #:user "root"
                                           #:port 10022
                                           #:host "localhost"
                                           #:log-verbosity 'rare)))
                (match (connect! session)
                  ('ok
                   (userauth-password! session "")
                   (display
                    (get-string-all
                     (open-remote-input-pipe* session "uname" "-on"))))
                  (status
                   (error "could not connect to childhurd over SSH"
                          session status)))))))

      (program-file "run-uname-over-ssh" run)))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette)
                       (srfi srfi-64)
                       (ice-9 match))

          (define marionette
            (make-marionette (list #$vm)))

          (mkdir #$output)
          (chdir #$output)

          (test-begin "childhurd")

          (test-assert "service running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (match (start-service 'childhurd)
                  (#f #f)
                  (('service response-parts ...)
                   (match (assq-ref response-parts 'running)
                     ((pid) (number? pid))))))
             marionette))

          (test-equal "childhurd SSH server replies"
            "SSH"
            ;; Check from within the guest whether its childhurd's SSH
            ;; server is reachable.  Do that from the guest: port forwarding
            ;; to the host won't work because QEMU listens on 127.0.0.1.
            (marionette-eval
             '(begin
                (use-modules (ice-9 match))

                (let loop ((n 60))
                  (if (zero? n)
                      'all-attempts-failed
                      (let ((s (socket PF_INET SOCK_STREAM 0))
                            (a (make-socket-address AF_INET
                                                    INADDR_LOOPBACK
                                                    10022)))
                        (format #t "connecting to childhurd SSH server...~%")
                        (connect s a)
                        (match (get-string-n s 3)
                          ((? eof-object?)
                           (close-port s)
                           (sleep 1)
                           (loop (- n 1)))
                          (str
                           (close-port s)
                           str))))))
             marionette))

          (test-equal "SSH up and running"
            "childhurd GNU\n"

            ;; Connect from the guest to the chidhurd over SSH and run the
            ;; 'uname' command.
            (marionette-eval
             '(begin
                (use-modules (ice-9 popen))

                (get-string-all
                 (open-input-pipe #$run-uname-over-ssh)))
             marionette))

          (test-end)
          (exit (= (test-runner-fail-count (test-runner-current)) 0)))))

  (gexp->derivation "childhurd-test" test))

(define %test-childhurd
  (system-test
   (name "childhurd")
   (description
    "Connect to the GNU/Hurd virtual machine service, aka. a childhurd, making
sure that the childhurd boots and runs its SSH server.")
   (value (run-childhurd-test))))
