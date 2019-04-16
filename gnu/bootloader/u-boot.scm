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
  #:use-module (gnu packages bootloaders)
  #:use-module (guix gexp)
  #:export (u-boot-bootloader
            u-boot-a20-olinuxino-lime-bootloader
            u-boot-a20-olinuxino-lime2-bootloader
            u-boot-a20-olinuxino-micro-bootloader
            u-boot-bananapi-m2-ultra-bootloader
            u-boot-beaglebone-black-bootloader
            u-boot-mx6cuboxi-bootloader
            u-boot-nintendo-nes-classic-edition-bootloader
            u-boot-novena-bootloader
            u-boot-pine64-plus-bootloader
            u-boot-pinebook-bootloader
            u-boot-puma-rk3399-bootloader
            u-boot-wandboard-bootloader))

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

(define install-allwinner-u-boot
  #~(lambda (bootloader device mount-point)
      (let ((u-boot (string-append bootloader
                                   "/libexec/u-boot-sunxi-with-spl.bin")))
        (write-file-on-device u-boot (stat:size (stat u-boot))
                              device (* 8 1024)))))

(define install-allwinner64-u-boot
  #~(lambda (bootloader device mount-point)
      (let ((spl (string-append bootloader "/libexec/spl/sunxi-spl.bin"))
            (u-boot (string-append bootloader "/libexec/u-boot.itb")))
        (write-file-on-device spl (stat:size (stat spl))
                              device (* 8 1024))
        (write-file-on-device u-boot (stat:size (stat u-boot))
                              device (* 40 1024)))))

(define install-imx-u-boot
  #~(lambda (bootloader device mount-point)
      (let ((spl (string-append bootloader "/libexec/SPL"))
            (u-boot (string-append bootloader "/libexec/u-boot.img")))
        (write-file-on-device spl (stat:size (stat spl))
                              device (* 1 1024))
        (write-file-on-device u-boot (stat:size (stat u-boot))
                              device (* 69 1024)))))

(define install-puma-rk3399-u-boot
  #~(lambda (bootloader device mount-point)
      (let ((spl (string-append bootloader "/libexec/u-boot-spl.rksd"))
            (u-boot (string-append bootloader "/libexec/u-boot.itb")))
        (write-file-on-device spl (stat:size (stat spl))
                              device (* 64 512))
        (write-file-on-device u-boot (stat:size (stat u-boot))
                              device (* 512 512)))))



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
   (package u-boot-am335x-boneblack)
   (installer install-beaglebone-black-u-boot)))

(define u-boot-allwinner-bootloader
  (bootloader
   (inherit u-boot-bootloader)
   (installer install-allwinner-u-boot)))

(define u-boot-allwinner64-bootloader
  (bootloader
   (inherit u-boot-bootloader)
   (installer install-allwinner64-u-boot)))

(define u-boot-imx-bootloader
  (bootloader
   (inherit u-boot-bootloader)
   (installer install-imx-u-boot)))

(define u-boot-nintendo-nes-classic-edition-bootloader
  (bootloader
    (inherit u-boot-allwinner-bootloader)
    (package u-boot-nintendo-nes-classic-edition)))

(define u-boot-a20-olinuxino-lime-bootloader
  (bootloader
   (inherit u-boot-allwinner-bootloader)
   (package u-boot-a20-olinuxino-lime)))

(define u-boot-a20-olinuxino-lime2-bootloader
  (bootloader
   (inherit u-boot-allwinner-bootloader)
   (package u-boot-a20-olinuxino-lime2)))

(define u-boot-a20-olinuxino-micro-bootloader
  (bootloader
   (inherit u-boot-allwinner-bootloader)
   (package u-boot-a20-olinuxino-micro)))

(define u-boot-bananapi-m2-ultra-bootloader
  (bootloader
   (inherit u-boot-allwinner-bootloader)
   (package u-boot-bananapi-m2-ultra)))

(define u-boot-mx6cuboxi-bootloader
  (bootloader
   (inherit u-boot-imx-bootloader)
   (package u-boot-mx6cuboxi)))

(define u-boot-wandboard-bootloader
  (bootloader
   (inherit u-boot-imx-bootloader)
   (package u-boot-wandboard)))

(define u-boot-novena-bootloader
  (bootloader
   (inherit u-boot-imx-bootloader)
   (package u-boot-novena)))

(define u-boot-pine64-plus-bootloader
  (bootloader
   (inherit u-boot-allwinner64-bootloader)
   (package u-boot-pine64-plus)))

(define u-boot-pinebook-bootloader
  (bootloader
   (inherit u-boot-allwinner64-bootloader)
   (package u-boot-pinebook)))

(define u-boot-puma-rk3399-bootloader
  (bootloader
   (inherit u-boot-bootloader)
   (package u-boot-puma-rk3399)
   (installer install-puma-rk3399-u-boot)))
