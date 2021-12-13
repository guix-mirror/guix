;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020, 2021 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu system hurd)
  #:use-module (guix gexp)
  #:use-module (guix profiles)
  #:use-module (guix utils)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages file)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages hurd)
  #:use-module (gnu packages less)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services hurd)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system)
  #:use-module (gnu system setuid)
  #:use-module (gnu system shadow)
  #:use-module (gnu system vm)
  #:export (%base-packages/hurd
            %base-services/hurd
            %hurd-default-operating-system
            %hurd-default-operating-system-kernel
            %setuid-programs/hurd))

;;; Commentary:
;;;
;;; This module provides system-specifics for the GNU/Hurd operating system
;;; and virtual machine.
;;;
;;; Code:

(define %hurd-default-operating-system-kernel
  (if (hurd-system?)
      gnumach
      ;; A cross-built GNUmach does not work
      (with-parameters ((%current-system "i686-linux")
                        (%current-target-system #f))
        gnumach)))

(define %base-packages/hurd
  (list hurd bash coreutils file findutils grep sed
        diffutils patch gawk tar gzip bzip2 xz lzip
        guile-3.0-latest guile-colorized guile-readline
        net-base inetutils less shadow shepherd sudo which
        info-reader))

(define %base-services/hurd
  (list (service hurd-console-service-type
                 (hurd-console-configuration (hurd hurd)))
        (service hurd-getty-service-type (hurd-getty-configuration
                                          (tty "tty1")))
        (service hurd-getty-service-type (hurd-getty-configuration
                                          (tty "tty2")))
        (service static-networking-service-type
                 (list %loopback-static-networking

                       ;; QEMU user-mode networking.  To get "eth0", you need
                       ;; QEMU to emulate a device for which Mach has an
                       ;; in-kernel driver, for instance with:
                       ;; --device rtl8139,netdev=net0 --netdev user,id=net0
                       %qemu-static-networking))
        (syslog-service)
        (service guix-service-type
                 (guix-configuration
                  (extra-options '("--disable-chroot"
                                   "--disable-deduplication"))))
        (service special-files-service-type
                 `(("/bin/sh" ,(file-append bash "/bin/sh"))
                   ("/usr/bin/env" ,(file-append coreutils "/bin/env"))))))

(define %setuid-programs/hurd
  ;; Default set of setuid-root programs.
  (map file-like->setuid-program
       (list (file-append shadow "/bin/passwd")
             (file-append shadow "/bin/sg")
             (file-append shadow "/bin/su")
             (file-append shadow "/bin/newgrp")
             (file-append shadow "/bin/newuidmap")
             (file-append shadow "/bin/newgidmap")
             (file-append sudo "/bin/sudo")
             (file-append sudo "/bin/sudoedit"))))

(define %hurd-default-operating-system
  (operating-system
    (kernel %hurd-default-operating-system-kernel)
    (kernel-arguments '())
    (hurd hurd)
    (bootloader (bootloader-configuration
                 (bootloader grub-minimal-bootloader)
                 (targets '("/dev/vda"))))
    (initrd #f)
    (initrd-modules (lambda _ '()))
    (firmware '())
    (host-name "guixygnu")
    (file-systems '())
    (packages %base-packages/hurd)
    (timezone "GNUrope")
    (name-service-switch #f)
    (essential-services (hurd-default-essential-services this-operating-system))
    (setuid-programs %setuid-programs/hurd)))
