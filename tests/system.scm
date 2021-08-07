;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (test-system)
  #:use-module (gnu)
  #:use-module ((gnu services) #:select (service-value))
  #:use-module (guix store)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

;; Test the (gnu system) module.

(define %root-fs
  (file-system
    (device (file-system-label "my-root"))
    (mount-point "/")
    (type "ext4")))

(define %os
  (operating-system
    (host-name "komputilo")
    (timezone "Europe/Berlin")
    (locale "en_US.utf8")
    (bootloader (bootloader-configuration
                 (bootloader grub-bootloader)
                 (targets '("/dev/sdX"))))
    (file-systems (cons %root-fs %base-file-systems))

    (users %base-user-accounts)))

(define %luks-device
  (mapped-device
   (source "/dev/foo") (target "my-luks-device")
   (type luks-device-mapping)))

(define %os-with-mapped-device
  (operating-system
    (host-name "komputilo")
    (timezone "Europe/Berlin")
    (locale "en_US.utf8")
    (bootloader (bootloader-configuration
                 (bootloader grub-bootloader)
                 (targets '("/dev/sdX"))))
    (mapped-devices (list %luks-device))
    (file-systems (cons (file-system
                          (inherit %root-fs)
                          (dependencies (list %luks-device)))
                        %base-file-systems))
    (users %base-user-accounts)))


(test-begin "system")

(test-assert "operating-system-store-file-system"
  ;; %BASE-FILE-SYSTEMS defines a bind-mount for /gnu/store, but this
  ;; shouldn't be a problem.
  (eq? %root-fs
       (operating-system-store-file-system %os)))

(test-assert "operating-system-store-file-system, prefix"
  (let* ((gnu (file-system
                (device "foobar")
                (mount-point (dirname (%store-prefix)))
                (type "ext5")))
         (os  (operating-system
                (inherit %os)
                (file-systems (cons* gnu %root-fs
                                     %base-file-systems)))))
    (eq? gnu (operating-system-store-file-system os))))

(test-assert "operating-system-store-file-system, store"
  (let* ((gnu (file-system
                (device "foobar")
                (mount-point (%store-prefix))
                (type "ext5")))
         (os  (operating-system
                (inherit %os)
                (file-systems (cons* gnu %root-fs
                                     %base-file-systems)))))
    (eq? gnu (operating-system-store-file-system os))))

(test-equal "operating-system-user-mapped-devices"
  '()
  (operating-system-user-mapped-devices %os-with-mapped-device))

(test-equal "operating-system-boot-mapped-devices"
  (list %luks-device)
  (operating-system-boot-mapped-devices %os-with-mapped-device))

(test-equal "operating-system-boot-mapped-devices, implicit dependency"
  (list %luks-device)

  ;; Here we expect the implicit dependency between "/" and
  ;; "/dev/mapper/my-luks-device" to be found, in spite of the lack of a
  ;; 'dependencies' field in the root file system.
  (operating-system-boot-mapped-devices
   (operating-system
     (inherit %os-with-mapped-device)
     (file-systems (cons (file-system
                           (device "/dev/mapper/my-luks-device")
                           (mount-point "/")
                           (type "ext4"))
                         %base-file-systems)))))

(test-equal "non-boot-file-system-service"
  '()

  ;; Make sure that mapped devices with at least one needed-for-boot user are
  ;; handled exclusively from the initrd.  See <https://bugs.gnu.org/31889>.
  (append-map file-system-dependencies
              (service-value
               ((@@ (gnu system) non-boot-file-system-service)
                (operating-system
                  (inherit %os-with-mapped-device)
                  (file-systems
                   (list (file-system
                           (mount-point "/foo/bar")
                           (device "qux:baz")
                           (type "none")
                           (dependencies (list %luks-device)))
                         (file-system
                           (device (file-system-label "my-root"))
                           (mount-point "/")
                           (type "ext4")
                           (dependencies (list %luks-device))))))))))

(test-end)
