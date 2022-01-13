;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Christine Lemmer-Webber <cwebber@dustycloud.org>
;;; Copyright © 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018 Chris Marusich <cmmarusich@gmail.com>
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

(define-module (gnu system vm)
  #:use-module (guix config)
  #:use-module (guix store)
  #:use-module (guix gexp)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (guix monads)
  #:use-module (guix records)
  #:use-module (guix modules)
  #:use-module (guix utils)
  #:use-module (gcrypt hash)
  #:use-module (guix base32)
  #:use-module ((guix self) #:select (make-config.scm))

  #:use-module ((gnu build marionette)
                #:select (qemu-command))
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages guile)
  #:autoload   (gnu packages gnupg) (guile-gcrypt)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages linux)

  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu image)
  #:use-module (gnu system image)
  #:use-module (gnu system linux-container)
  #:use-module (gnu system linux-initrd)
  #:use-module (gnu bootloader)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu system uuid)

  #:use-module ((srfi srfi-1) #:hide (partition))
  #:use-module (srfi srfi-26)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)

  #:export (virtualized-operating-system
            system-qemu-image/shared-store-script

            virtual-machine
            virtual-machine?))


;;; Commentary:
;;;
;;; Tools to evaluate build expressions within virtual machines.
;;;
;;; Code:

;; By default, the msize value is 8 KiB, which according to QEMU is
;; insufficient and would degrade performance.  The msize value should roughly
;; match the bandwidth of the system's IO (see:
;; https://wiki.qemu.org/Documentation/9psetup#msize).  Use 100 MiB as a
;; conservative default.
(define %default-msize-value (* 100 (expt 2 20))) ;100 MiB

(define %linux-vm-file-systems
  ;; File systems mounted for 'derivation-in-linux-vm'.  These are shared with
  ;; the host over 9p.
  ;;
  ;; The 9p documentation says that cache=loose is "intended for exclusive,
  ;; read-only mounts", without additional details.  It's much faster than the
  ;; default cache=none, especially when copying and registering store items.
  ;; Thus, use cache=loose, except for /xchg where we want to ensure
  ;; consistency.
  (list (file-system
          (mount-point (%store-prefix))
          (device "store")
          (type "9p")
          (needed-for-boot? #t)
          (flags '(read-only))
          (options (format #f "trans=virtio,cache=loose,msize=~a"
                           %default-msize-value))
          (check? #f))
        (file-system
          (mount-point "/xchg")
          (device "xchg")
          (type "9p")
          (needed-for-boot? #t)
          (options (format #f "trans=virtio,msize=~a" %default-msize-value))
          (check? #f))
        (file-system
          (mount-point "/tmp")
          (device "tmp")
          (type "9p")
          (needed-for-boot? #t)
          (options (format #f "trans=virtio,cache=loose,msize=~a"
                           %default-msize-value))
          (check? #f))))


;;;
;;; VMs that share file systems with the host.
;;;

(define (file-system->mount-tag fs)
  "Return a 9p mount tag for host file system FS."
  ;; QEMU mount tags must be ASCII, at most 31-byte long, cannot contain
  ;; slashes, and cannot start with '_'.  Compute an identifier that
  ;; corresponds to the rules.
  (string-append "TAG"
                 (string-drop (bytevector->base32-string
                               (sha1 (string->utf8 fs)))
                              4)))

(define (mapping->file-system mapping)
  "Return a 9p file system that realizes MAPPING."
  (match mapping
    (($ <file-system-mapping> source target writable?)
     (file-system
       (mount-point target)
       (device (file-system->mount-tag source))
       (type "9p")
       (flags (if writable? '() '(read-only)))
       (options (string-append "trans=virtio"
                               (if writable? "" ",cache=loose")
                               ",msize=" (number->string %default-msize-value)))
       (check? #f)
       (create-mount-point? #t)))))

(define* (virtualized-operating-system os mappings
                                       #:key (full-boot? #f) volatile?)
  "Return an operating system based on OS suitable for use in a virtualized
environment with the store shared with the host.  MAPPINGS is a list of
<file-system-mapping> to realize in the virtualized OS."
  (define user-file-systems
    ;; Remove file systems that conflict with those added below, or that are
    ;; normally bound to real devices.
    (remove (lambda (fs)
              (let ((target (file-system-mount-point fs))
                    (source (file-system-device fs)))
                (or (string=? target (%store-prefix))
                    (string=? target "/")
                    (and (string? source)
                         (string-prefix? "/dev/" source))

                    ;; Labels and UUIDs are necessarily invalid in the VM.
                    (and (file-system-mount? fs)
                         (or (file-system-label? source)
                             (uuid? source))))))
            (operating-system-file-systems os)))

  (define virtual-file-systems
    (cons (file-system
            (mount-point "/")
            (device "/dev/vda1")
            (type "ext4"))

          (append (map mapping->file-system mappings)
                  user-file-systems)))

  (operating-system (inherit os)

    ;; XXX: Until we run QEMU with UEFI support (with the OVMF firmware),
    ;; force the traditional i386/BIOS method.
    ;; See <https://bugs.gnu.org/28768>.
    (bootloader (bootloader-configuration
                  (inherit (operating-system-bootloader os))
                  (bootloader grub-bootloader)
                  (targets '("/dev/vda"))))

    (initrd (lambda (file-systems . rest)
              (apply (operating-system-initrd os)
                     file-systems
                     #:volatile-root? volatile?
                     rest)))

    ;; Disable swap.
    (swap-devices '())

    ;; XXX: When FULL-BOOT? is true, do not add a 9p mount for /gnu/store
    ;; since that would lead the bootloader config to look for the kernel and
    ;; initrd in it.
    (file-systems (if full-boot?
                      virtual-file-systems
                      (cons
                       (file-system
                         (inherit (mapping->file-system %store-mapping))
                         (needed-for-boot? #t))
                       virtual-file-systems)))))

(define* (common-qemu-options image shared-fs
                              #:key rw-image?)
  "Return the a string-value gexp with the common QEMU options to boot IMAGE,
with '-virtfs' options for the host file systems listed in SHARED-FS."

  (define (virtfs-option fs)
    #~(format #f "-virtfs local,path=~s,security_model=none,mount_tag=~s"
              #$fs #$(file-system->mount-tag fs)))

  #~(;; Only enable kvm if we see /dev/kvm exists.
     ;; This allows users without hardware virtualization to still use these
     ;; commands.
     #$@(if (file-exists? "/dev/kvm")
            '("-enable-kvm")
            '())

     "-no-reboot"
     "-object" "rng-random,filename=/dev/urandom,id=guix-vm-rng"
     "-device" "virtio-rng-pci,rng=guix-vm-rng"

     #$@(map virtfs-option shared-fs)
     #$@(if rw-image?
            #~((format #f "-drive file=~a,if=virtio" #$image))
            #~((format #f "-drive file=~a,if=virtio,cache=writeback,werror=report,readonly=on"
                       #$image)))))

(define* (system-qemu-image/shared-store-script os
                                                #:key
                                                (system (%current-system))
                                                (target (%current-target-system))
                                                (qemu qemu)
                                                (graphic? #t)
                                                (volatile? #t)
                                                (memory-size 512)
                                                (mappings '())
                                                full-boot?
                                                (disk-image-size
                                                 (* (if full-boot? 500 70)
                                                    (expt 2 20)))
                                                (options '()))
  "Return a derivation that builds a script to run a virtual machine image of
OS that shares its store with the host.  The virtual machine runs with
MEMORY-SIZE MiB of memory.

MAPPINGS is a list of <file-system-mapping> specifying mapping of host file
systems into the guest.

When FULL-BOOT? is true, the returned script runs everything starting from the
bootloader; otherwise it directly starts the operating system kernel.  When
VOLATILE? is true, an overlay is created on top of a read-only
storage. Otherwise the storage is made persistent.  The DISK-IMAGE-SIZE
parameter specifies the size in bytes of the root disk image; it is mostly
useful when FULL-BOOT?  is true."
  (mlet* %store-monad ((os ->  (virtualized-operating-system
                                os mappings
                                #:full-boot? full-boot?
                                #:volatile? volatile?))
                       (base-image -> (system-image
                                       (image
                                        (inherit
                                         (raw-with-offset-disk-image))
                                        (operating-system os)
                                        (size disk-image-size)
                                        (shared-store?
                                         (and (not full-boot?) volatile?))
                                        (volatile-root? volatile?)))))
    (define kernel-arguments
      #~(list #$@(if graphic? #~() #~("console=ttyS0"))
              #+@(operating-system-kernel-arguments os "/dev/vda1")))

    (define rw-image
      #~(format #f "/tmp/guix-image-~a" (basename #$base-image)))

    (define qemu-exec
      #~(list #+(file-append qemu "/bin/"
                             (qemu-command (or target system)))
              ;; Tells qemu to use the terminal it was started in for IO.
              #$@(if graphic? '() #~("-nographic"))
              #$@(if full-boot?
                     #~()
                     #~("-kernel" #$(operating-system-kernel-file os)
                        "-initrd" #$(file-append os "/initrd")
                        (format #f "-append ~s"
                                (string-join #$kernel-arguments " "))))
              #$@(common-qemu-options (if volatile? base-image rw-image)
                                      (map file-system-mapping-source
                                           (cons %store-mapping mappings))
                                      #:rw-image? (not volatile?))
              "-m " (number->string #$memory-size)
              #$@options))

    (define builder
      #~(call-with-output-file #$output
          (lambda (port)
            (format port "#!~a~%"
                    #+(file-append bash "/bin/sh"))
            (when (not #$volatile?)
              (format port "~a~%"
                      #$(program-file "copy-image"
                                      #~(unless (file-exists? #$rw-image)
                                          (copy-file #$base-image #$rw-image)
                                          (chmod #$rw-image #o640)))))
            (format port "exec ~a \"$@\"~%"
                    (string-join #$qemu-exec " "))
            (chmod port #o555))))

    (gexp->derivation "run-vm.sh" builder)))


;;;
;;; High-level abstraction.
;;;

(define-record-type* <virtual-machine> %virtual-machine
  make-virtual-machine
  virtual-machine?
  (operating-system virtual-machine-operating-system) ;<operating-system>
  (qemu             virtual-machine-qemu              ;<package>
                    (default qemu-minimal))
  (volatile?        virtual-machine-volatile?    ;Boolean
                    (default #t))
  (graphic?         virtual-machine-graphic?      ;Boolean
                    (default #f))
  (memory-size      virtual-machine-memory-size   ;integer (MiB)
                    (default 256))
  (disk-image-size  virtual-machine-disk-image-size   ;integer (bytes)
                    (default 'guess))
  (port-forwardings virtual-machine-port-forwardings ;list of integer pairs
                    (default '())))

(define-syntax virtual-machine
  (syntax-rules ()
    "Declare a virtual machine running the specified OS, with the given
options."
    ((_ os)                                       ;shortcut
     (%virtual-machine (operating-system os)))
    ((_ fields ...)
     (%virtual-machine fields ...))))

(define (port-forwardings->qemu-options forwardings)
  "Return the QEMU option for the given port FORWARDINGS as a string, where
FORWARDINGS is a list of host-port/guest-port pairs."
  (string-join
   (map (match-lambda
          ((host-port . guest-port)
           (string-append "hostfwd=tcp::"
                          (number->string host-port)
                          "-:" (number->string guest-port))))
        forwardings)
   ","))

(define-gexp-compiler (virtual-machine-compiler (vm <virtual-machine>)
                                                system target)
  (match vm
    (($ <virtual-machine> os qemu volatile? graphic? memory-size
                          disk-image-size ())
     (system-qemu-image/shared-store-script os
                                            #:system system
                                            #:target target
                                            #:qemu qemu
                                            #:graphic? graphic?
                                            #:volatile? volatile?
                                            #:memory-size memory-size
                                            #:disk-image-size
                                            disk-image-size))
    (($ <virtual-machine> os qemu volatile? graphic? memory-size
                          disk-image-size forwardings)
     (let ((options
            `("-nic" ,(string-append
                       "user,model=virtio-net-pci,"
                       (port-forwardings->qemu-options forwardings)))))
       (system-qemu-image/shared-store-script os
                                              #:system system
                                              #:target target
                                              #:qemu qemu
                                              #:graphic? graphic?
                                              #:volatile? volatile?
                                              #:memory-size memory-size
                                              #:disk-image-size
                                              disk-image-size
                                              #:options options)))))

;;; vm.scm ends here
