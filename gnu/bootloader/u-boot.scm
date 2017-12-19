;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 David Craven <david@craven.ch>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (gnu bootloader u-boot)
  #:use-module (gnu bootloader extlinux)
  #:use-module (gnu bootloader)
  #:use-module (gnu system)
  #:use-module (gnu build bootloader)
  #:use-module (gnu packages bootloaders)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix records)
  #:use-module (guix utils)
  #:export (u-boot-bootloader
            u-boot-beaglebone-black-bootloader))

(define install-u-boot
  #~(lambda (bootloader device mount-point)
      (if bootloader
        (error "Failed to install U-Boot"))))

(define install-beaglebone-black-u-boot
  ;; http://wiki.beyondlogic.org/index.php?title=BeagleBoneBlack_Upgrading_uBoot
  ;; This first stage bootloader called MLO (U-Boot SPL) is expected at
  ;; 0x20000 by BBB ROM code. The second stage bootloader will be loaded by
  ;; the MLO and is expected at 0x60000.  Write both first stage ("MLO") and
  ;; second stage ("u-boot.img") images, read in BOOTLOADER directory, to the
  ;; specified DEVICE.
  #~(lambda (bootloader device mount-point)
      (let ((mlo (string-append bootloader "/libexec/MLO"))
            (u-boot (string-append bootloader "/libexec/u-boot.img")))
        (write-file-on-device mlo (* 256 512)
                              device (* 256 512))
        (write-file-on-device u-boot (* 1024 512)
                              device (* 768 512)))))



;;;
;;; Bootloader definitions.
;;;

(define u-boot-bootloader
  (bootloader
   (inherit extlinux-bootloader)
   (name 'u-boot)
   (package #f)
   (installer install-u-boot)))

(define u-boot-beaglebone-black-bootloader
  (bootloader
   (inherit u-boot-bootloader)
   (package u-boot-beagle-bone-black)
   (installer install-beaglebone-black-u-boot)))
