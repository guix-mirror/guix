;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (gnu system images pinebook-pro)
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
  #:export (pinebook-pro-barebones-os
            pinebook-pro-image-type
            pinebook-pro-barebones-raw-image))

(define pinebook-pro-barebones-os
  (operating-system
    (host-name "viso")
    (timezone "Europe/Paris")
    (locale "en_US.utf8")
    (bootloader (bootloader-configuration
                 (bootloader u-boot-pinebook-pro-rk3399-bootloader)
                 (targets '("/dev/vda"))))
    (initrd-modules '())
    (kernel linux-libre-arm64-generic)
    (file-systems (cons (file-system
                          (device (file-system-label "my-root"))
                          (mount-point "/")
                          (type "ext4"))
                        %base-file-systems))
    (services (cons (service agetty-service-type
                             (agetty-configuration
                              (extra-options '("-L")) ; no carrier detect
                              (baud-rate "1500000")
                              (term "vt100")
                              (tty "ttyS2")))
                    %base-services))))

(define pinebook-pro-image-type
  (image-type
   (name 'pinebook-pro-raw)
   (constructor (cut image-with-os
                     (raw-with-offset-disk-image (* 9 (expt 2 20))) ;9MiB
                     <>))))

(define pinebook-pro-barebones-raw-image
  (image
   (inherit
    (os+platform->image pinebook-pro-barebones-os aarch64-linux
                        #:type pinebook-pro-image-type))
   (name 'pinebook-pro-barebones-raw-image)))

;; Return the default image.
pinebook-pro-barebones-raw-image
