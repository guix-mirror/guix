;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020 Danny Milosavljevic <dannym@scratchpost.org>
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

(define-module (gnu system images novena)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader u-boot)
  #:use-module (gnu image)
  #:use-module (gnu packages linux)
  #:use-module (gnu platforms arm)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system image)
  #:use-module (srfi srfi-26)
  #:export (novena-barebones-os
            novena-image-type
            novena-barebones-raw-image))

(define novena-barebones-os
  (operating-system
    (host-name "vignemale")
    (timezone "Europe/Paris")
    (locale "en_US.utf8")
    (bootloader (bootloader-configuration
                 (bootloader u-boot-novena-bootloader)
                 (targets '("/dev/vda"))))
    (initrd-modules '("sdhci-esdhc-imx" "ahci_imx" "i2c-dev"))
    ;(kernel linux-libre-arm-generic)
    (kernel-arguments '("console=ttymxc1,115200"))
    (file-systems (cons (file-system
                          (device (file-system-label "my-root"))
                          (mount-point "/")
                          (type "ext4"))
                        %base-file-systems))))

(define novena-image-type
  (image-type
   (name 'novena-raw)
   (constructor (cut image-with-os (raw-with-offset-disk-image) <>))))

(define novena-barebones-raw-image
  (image
   (inherit
    (os+platform->image novena-barebones-os armv7-linux
                        #:type novena-image-type))
   (name 'novena-barebones-raw-image)))

;; Return the default image.
novena-barebones-raw-image
