;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020 Miguel Ángel Arruga Vivas <rosen644835@gmail.com>
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

;;; Commentary:
;;;
;;; Test boot parameters value storage and compatibility.
;;;
;;; Code:

(define-module (test-boot-parameters)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system uuid)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix tests)
  #:use-module (srfi srfi-64)
  #:use-module (rnrs bytevectors))

(define %default-label "GNU with Linux-libre 99.1.2")
(define %default-kernel-path
  (string-append (%store-prefix)
                 "/zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz-linux-libre-99.1.2"))
(define %default-kernel
  (string-append %default-kernel-path "/" (system-linux-image-file-name)))
(define %default-kernel-arguments '())
(define %default-initrd-path
  (string-append (%store-prefix) "/wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww-initrd"))
(define %default-initrd (string-append %default-initrd-path "/initrd.cpio.gz"))
(define %default-root-device (uuid "abcdef12-3456-7890-abcd-ef1234567890"))
(define %default-store-device (uuid "01234567-89ab-cdef-0123-456789abcdef"))
(define %default-btrfs-subvolume "testfs")
(define %default-store-directory-prefix
  (string-append "/" %default-btrfs-subvolume))
(define %default-store-mount-point (%store-prefix))
(define %default-multiboot-modules '())
(define %default-locale "es_ES.utf8")
(define %root-path "/")

(define %grub-boot-parameters
  (boot-parameters
   (bootloader-name 'grub)
   (bootloader-menu-entries '())
   (root-device %default-root-device)
   (label %default-label)
   (kernel %default-kernel)
   (kernel-arguments %default-kernel-arguments)
   (initrd %default-initrd)
   (multiboot-modules %default-multiboot-modules)
   (locale %default-locale)
   (store-device %default-store-device)
   (store-directory-prefix %default-store-directory-prefix)
   (store-mount-point %default-store-mount-point)))

(define %default-operating-system
  (operating-system
    (host-name "host")
    (timezone "Europe/Berlin")
    (locale %default-locale)

    (bootloader (bootloader-configuration
                 (bootloader grub-bootloader)
                 (target "/dev/sda")))
    (file-systems (cons* (file-system
                           (device %default-root-device)
                           (mount-point %root-path)
                           (type "ext4"))
		         (file-system
                           (device %default-store-device)
                           (mount-point %default-store-mount-point)
                           (type "btrfs")
                           (options
                            (string-append "subvol="
                                           %default-btrfs-subvolume)))
                         %base-file-systems))))

(define (quote-uuid uuid)
  (list 'uuid (uuid-type uuid) (uuid-bytevector uuid)))

;; Call read-boot-parameters with the desired string as input.
(define* (test-read-boot-parameters
          #:key
          (version 0)
          (bootloader-name 'grub)
          (bootloader-menu-entries '())
          (label %default-label)
          (root-device (quote-uuid %default-root-device))
          (kernel %default-kernel)
          (kernel-arguments %default-kernel-arguments)
          (initrd %default-initrd)
          (multiboot-modules %default-multiboot-modules)
          (locale %default-locale)
          (with-store #t)
          (store-device
           (quote-uuid %default-store-device))
          (store-directory-prefix %default-store-directory-prefix)
          (store-mount-point %default-store-mount-point))
  (define (generate-boot-parameters)
    (define (sexp-or-nothing fmt val)
      (cond ((eq? 'false val) (format #false fmt #false))
            (val              (format #false fmt val))
            (else             "")))
    (format #false "(boot-parameters~a~a~a~a~a~a~a~a~a~a)"
            (sexp-or-nothing " (version ~S)" version)
            (sexp-or-nothing " (label ~S)" label)
            (sexp-or-nothing " (root-device ~S)" root-device)
            (sexp-or-nothing " (kernel ~S)" kernel)
            (sexp-or-nothing " (kernel-arguments ~S)" kernel-arguments)
            (sexp-or-nothing " (initrd ~S)" initrd)
            (if with-store
                (format #false " (store~a~a~a)"
                        (sexp-or-nothing " (device ~S)" store-device)
                        (sexp-or-nothing " (mount-point ~S)"
                                         store-mount-point)
                        (sexp-or-nothing " (directory-prefix ~S)"
                                         store-directory-prefix))
                "")
            (sexp-or-nothing " (locale ~S)" locale)
            (sexp-or-nothing " (bootloader-name ~a)" bootloader-name)
            (sexp-or-nothing " (bootloader-menu-entries ~S)"
                             bootloader-menu-entries)))
  (let ((str (generate-boot-parameters)))
    (call-with-input-string str read-boot-parameters)))

(test-begin "boot-parameters")

;; XXX: <warning: unrecognized boot parameters at '#f'>
(test-assert "read, construction, mandatory fields"
  (not (or (test-read-boot-parameters #:version #false)
           (test-read-boot-parameters #:version 'false)
           (test-read-boot-parameters #:version -1)
           (test-read-boot-parameters #:version "0")
           (test-read-boot-parameters #:root-device #false)
           (test-read-boot-parameters #:kernel #false)
           (test-read-boot-parameters #:label #false))))

(test-assert "read, construction, optional fields"
  (and (test-read-boot-parameters #:bootloader-name #false)
       (test-read-boot-parameters #:bootloader-menu-entries #false)
       (test-read-boot-parameters #:kernel-arguments #false)
       (test-read-boot-parameters #:with-store #false)
       (test-read-boot-parameters #:store-device #false)
       (test-read-boot-parameters #:store-device 'false)
       (test-read-boot-parameters #:store-mount-point #false)
       (test-read-boot-parameters #:store-directory-prefix #false)
       (test-read-boot-parameters #:multiboot-modules #false)
       (test-read-boot-parameters #:locale #false)
       (test-read-boot-parameters #:bootloader-name #false
                                  #:kernel-arguments #false
                                  #:with-store #false
                                  #:locale #false)))

(test-equal "read, default equality"
  %grub-boot-parameters
  (test-read-boot-parameters))

(test-equal "read, root-device, label"
  (file-system-label "my-root")
  (boot-parameters-root-device
   (test-read-boot-parameters #:root-device '(file-system-label "my-root"))))

(test-equal "read, root-device, /dev node"
  "/dev/sda2"
  (boot-parameters-root-device
   (test-read-boot-parameters #:root-device "/dev/sda2")))

(test-equal "read, kernel, only store path"
  %default-kernel
  (boot-parameters-kernel
   (test-read-boot-parameters #:kernel %default-kernel-path)))

(test-equal "read, kernel, full-path"
  %default-kernel
  (boot-parameters-kernel
   (test-read-boot-parameters #:kernel %default-kernel)))

(test-assert "read, construction, missing initrd"
  (not (boot-parameters-initrd (test-read-boot-parameters #:initrd #false))))

(test-equal "read, initrd, old format"
  "/a/b"
  (boot-parameters-initrd
   (test-read-boot-parameters #:initrd (list 'string-append "/a" "/b"))))

 ;; Compatibility reasons specified in gnu/system.scm.
(test-eq "read, bootloader-name, default value"
  'grub
  (boot-parameters-bootloader-name
   (test-read-boot-parameters #:bootloader-name #false)))

(test-eq "read, bootloader-menu-entries, default value"
  '()
  (boot-parameters-bootloader-menu-entries
   (test-read-boot-parameters #:bootloader-menu-entries #false)))

(test-eq "read, kernel-arguments, default value"
  '()
  (boot-parameters-kernel-arguments
   (test-read-boot-parameters #:kernel-arguments #false)))

(test-assert "read, store-device, filter /dev"
  (not (boot-parameters-store-device
        (test-read-boot-parameters #:store-device "/dev/sda3"))))

(test-assert "read, no-store, filter /dev from root"
  (not (boot-parameters-store-device
        (test-read-boot-parameters #:root-device "/dev/sda3"
                                   #:with-store #false))))

(test-assert "read, no store-device, filter /dev from root"
  (not (boot-parameters-store-device
        (test-read-boot-parameters #:root-device "/dev/sda3"
                                   #:store-device #false))))

(test-assert "read, store-device #false, filter /dev from root"
  (not (boot-parameters-store-device
        (test-read-boot-parameters #:root-device "/dev/sda3"
                                   #:store-device 'false))))

(test-equal "read, store-device, label (legacy)"
  (file-system-label "my-store")
  (boot-parameters-store-device
   (test-read-boot-parameters #:store-device "my-store")))

(test-equal "read, store-device, from root"
  %default-root-device
  (boot-parameters-store-device
   (test-read-boot-parameters #:with-store #false)))

(test-equal "read, no store-mount-point, default"
  %root-path
  (boot-parameters-store-mount-point
   (test-read-boot-parameters #:store-mount-point #false)))

(test-equal "read, no store, default store-mount-point"
  %root-path
  (boot-parameters-store-mount-point
   (test-read-boot-parameters #:with-store #false)))

;; For whitebox testing
(define operating-system-boot-parameters
  (@@ (gnu system) operating-system-boot-parameters))

(test-equal "from os, locale"
  %default-locale
  (boot-parameters-locale
   (operating-system-boot-parameters %default-operating-system
                                     %default-root-device)))

(test-equal "from os, store-directory-prefix"
  %default-store-directory-prefix
  (boot-parameters-store-directory-prefix
   (operating-system-boot-parameters %default-operating-system
                                     %default-root-device)))

(test-end "boot-parameters")
