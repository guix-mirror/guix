;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Marius Bakke <marius@gnu.org>
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

(define-module (gnu system images rock64)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader u-boot)
  #:use-module (gnu image)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services networking)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system image)
  #:use-module (srfi srfi-26)
  #:export (rock64-barebones-os
            rock64-image-type
            rock64-barebones-raw-image))

(define rock64-barebones-os
  (operating-system
    (host-name "jiehkkevarri")
    (timezone "Europe/Oslo")
    (locale "en_US.utf8")
    (bootloader (bootloader-configuration
                 (bootloader u-boot-rock64-rk3328-bootloader)
                 (target "/dev/sda")))
    (initrd-modules '())
    (kernel linux-libre-arm64-generic)
    (file-systems (cons (file-system
                          (device (file-system-label "my-root"))
                          (mount-point "/")
                          (type "ext4"))
                        %base-file-systems))
    (services (append (list (service dhcp-client-service-type))
                      %base-services))))

(define rock64-image-type
  (image-type
   (name 'rock64-raw)
   (constructor (cut image-with-os (arm64-disk-image (expt 2 24)) <>))))

(define rock64-barebones-raw-image
  (image
   (inherit
    (os->image rock64-barebones-os #:type rock64-image-type))
   (name 'rock64-barebones-raw-image)))

rock64-barebones-raw-image
