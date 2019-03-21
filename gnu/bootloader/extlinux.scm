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

(define-module (gnu bootloader extlinux)
  #:use-module (gnu bootloader)
  #:use-module (gnu packages bootloaders)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:export (extlinux-bootloader
            extlinux-bootloader-gpt))

(define* (extlinux-configuration-file config entries
                                      #:key
                                      (system (%current-system))
                                      (old-entries '()))
  "Return the U-Boot configuration file corresponding to CONFIG, a
<u-boot-configuration> object, and where the store is available at STORE-FS, a
<file-system> object.  OLD-ENTRIES is taken to be a list of menu entries
corresponding to old generations of the system."

  (define all-entries
    (append entries (bootloader-configuration-menu-entries config)))

  (define (menu-entry->gexp entry)
    (let ((label (menu-entry-label entry))
          (kernel (menu-entry-linux entry))
          (kernel-arguments (menu-entry-linux-arguments entry))
          (initrd (menu-entry-initrd entry)))
      #~(format port "LABEL ~a
  MENU LABEL ~a
  KERNEL ~a
  FDTDIR ~a/lib/dtbs
  INITRD ~a
  APPEND ~a
~%"
                #$label #$label
                #$kernel (dirname #$kernel) #$initrd
                (string-join (list #$@kernel-arguments)))))

  (define builder
    #~(call-with-output-file #$output
        (lambda (port)
          (let ((timeout #$(bootloader-configuration-timeout config)))
            (format port "# This file was generated from your Guix configuration.  Any changes
# will be lost upon reconfiguration.
UI menu.c32
MENU TITLE GNU Guix Boot Options
PROMPT ~a
TIMEOUT ~a~%"
                    (if (> timeout 0) 1 0)
                    ;; timeout is expressed in 1/10s of seconds.
                    (* 10 timeout))
            #$@(map menu-entry->gexp all-entries)

            #$@(if (pair? old-entries)
                   #~((format port "~%")
                      #$@(map menu-entry->gexp old-entries)
                      (format port "~%"))
                   #~())))))

  (computed-file "extlinux.conf" builder))




;;;
;;; Install procedures.
;;;

(define (install-extlinux mbr)
  #~(lambda (bootloader device mount-point)
      (let ((extlinux (string-append bootloader "/sbin/extlinux"))
            (install-dir (string-append mount-point "/boot/extlinux"))
            (syslinux-dir (string-append bootloader "/share/syslinux")))
        (for-each (lambda (file)
                    (install-file file install-dir))
                  (find-files syslinux-dir "\\.c32$"))
        (invoke/quiet extlinux "--install" install-dir)
        (write-file-on-device (string-append syslinux-dir "/" #$mbr)
                              440 device 0))))

(define install-extlinux-mbr
  (install-extlinux "mbr.bin"))

(define install-extlinux-gpt
  (install-extlinux "gptmbr.bin"))



;;;
;;; Bootloader definitions.
;;;

(define extlinux-bootloader
  (bootloader
   (name 'extlinux)
   (package syslinux)
   (installer install-extlinux-mbr)
   (configuration-file "/boot/extlinux/extlinux.conf")
   (configuration-file-generator extlinux-configuration-file)))

(define extlinux-bootloader-gpt
  (bootloader
   (inherit extlinux-bootloader)
   (installer install-extlinux-gpt)))
