;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gnu system images hurd)
  #:use-module (guix gexp)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu image)
  #:use-module (gnu packages ssh)
  #:use-module (gnu services)
  #:use-module (gnu services ssh)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system hurd)
  #:use-module (gnu system image)
  #:use-module (srfi srfi-26)
  #:export (hurd-barebones-os
            hurd-disk-image
            hurd-image-type
            hurd-qcow2-image-type
            hurd-barebones-disk-image
            hurd-barebones-qcow2-image))

(define hurd-barebones-os
  (operating-system
    (inherit %hurd-default-operating-system)
    (bootloader (bootloader-configuration
                 (bootloader grub-minimal-bootloader)
                 (target "/dev/sdX")))
    (file-systems (cons (file-system
                          (device (file-system-label "my-root"))
                          (mount-point "/")
                          (type "ext2"))
                        %base-file-systems))
    (host-name "guixygnu")
    (timezone "Europe/Amsterdam")
    (packages (cons openssh-sans-x %base-packages/hurd))
    (services (cons (service openssh-service-type
                             (openssh-configuration
                              (openssh openssh-sans-x)
                              (use-pam? #f)
                              (port-number 2222)
                              (permit-root-login #t)
                              (allow-empty-passwords? #t)
                              (password-authentication? #t)))
               %base-services/hurd))))

(define hurd-initialize-root-partition
  #~(lambda* (#:rest args)
      (apply initialize-root-partition
             (append args
                     (list #:make-device-nodes make-hurd-device-nodes
                           ;; XXX Creating a db.sqlite with journal_mode=WAL
                           ;; yields "unable to open database file" on GNU/Hurd
                           ;; for an sqlite with the hurd-locking-mode.patch;
                           ;; see <https://bugs.gnu.org/42151>.
                           #:wal-mode? #f)))))

(define hurd-disk-image
  (image
   (format 'disk-image)
   (target "i586-pc-gnu")
   (partitions
    (list (partition
           (size 'guess)
           (offset root-offset)
           (label root-label)
           (file-system "ext2")
           (file-system-options '("-o" "hurd" "-O" "ext_attr"))
           (flags '(boot))
           (initializer hurd-initialize-root-partition))))))

(define hurd-image-type
  (image-type
   (name 'hurd-raw)
   (constructor (cut image-with-os hurd-disk-image <>))))

(define hurd-qcow2-image-type
  (image-type
   (name 'hurd-qcow2)
   (constructor (lambda (os)
                  (image
                   (inherit hurd-disk-image)
                   (format 'compressed-qcow2)
                   (operating-system os))))))

(define hurd-barebones-disk-image
  (image
   (inherit
    (os->image hurd-barebones-os #:type hurd-image-type))
   (name 'hurd-barebones-disk-image)))

(define hurd-barebones-qcow2-image
  (image
   (inherit
    (os->image hurd-barebones-os #:type hurd-qcow2-image-type))
   (name 'hurd-barebones.qcow2)))

;; Return the default image.
hurd-barebones-qcow2-image
