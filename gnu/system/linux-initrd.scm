;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2017, 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu system linux-initrd)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((guix store)
                #:select (%store-prefix))
  #:use-module ((guix derivations)
                #:select (derivation->output-path))
  #:use-module (guix modules)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages guile)
  #:use-module ((gnu packages xorg)
                #:select (console-setup xkeyboard-config))
  #:use-module ((gnu packages make-bootstrap)
                #:select (%guile-static-stripped))
  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu system keyboard)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (expression->initrd
            %base-initrd-modules
            raw-initrd
            file-system-packages
            base-initrd))


;;; Commentary:
;;;
;;; Tools to build initial RAM disks (initrd's) for Linux-Libre, and in
;;; particular initrd's that run Guile.
;;;
;;; Code:


(define* (expression->initrd exp
                             #:key
                             (guile %guile-static-stripped)
                             (gzip gzip)
                             (name "guile-initrd")
                             (system (%current-system)))
  "Return as a file-like object a Linux initrd (a gzipped cpio archive)
containing GUILE and that evaluates EXP, a G-expression, upon booting.  All
the derivations referenced by EXP are automatically copied to the initrd."

  ;; General Linux overview in `Documentation/early-userspace/README' and
  ;; `Documentation/filesystems/ramfs-rootfs-initramfs.txt'.

  (define init
    (program-file "init" exp #:guile guile))

  (define (import-module? module)
    ;; Since we don't use deduplication support in 'populate-store', don't
    ;; import (guix store deduplication) and its dependencies, which includes
    ;; Guile-Gcrypt.  That way we can run tests with '--bootstrap'.
    (and (guix-module-name? module)
         (not (equal? module '(guix store deduplication)))))

  (define builder
    ;; Do not use "guile-zlib" extension here, otherwise it would drag the
    ;; non-static "zlib" package to the initrd closure.  It is not needed
    ;; anyway because the modules are stored uncompressed within the initrd.
    (with-imported-modules (source-module-closure
                            '((gnu build linux-initrd))
                            #:select? import-module?)
      #~(begin
          (use-modules (gnu build linux-initrd))

          (mkdir #$output)

          ;; The guile used in the initrd must be present in the store, so
          ;; that module loading works once the root is switched.
          ;;
          ;; To ensure that is the case, add an explicit reference to the
          ;; guile package used in the initrd to the output.
          ;;
          ;; This fixes guix-patches bug #28399, "Fix mysql activation, and
          ;; add a basic test".
          (call-with-output-file (string-append #$ output "/references")
            (lambda (port)
              (simple-format port "~A\n" #$guile)))

          (build-initrd (string-append #$output "/initrd.cpio.gz")
                        #:guile #$guile
                        #:init #$init
                        ;; Copy everything INIT refers to into the initrd.
                        #:references-graphs '("closure")
                        #:gzip (string-append #+gzip "/bin/gzip")))))

  (file-append (computed-file name builder
                              #:options
                              `(#:references-graphs (("closure" ,init))))
               "/initrd.cpio.gz"))

(define (flat-linux-module-directory linux modules)
  "Return a flat directory containing the Linux kernel modules listed in
MODULES and taken from LINUX."
  (define imported-modules
    (source-module-closure '((gnu build linux-modules)
                             (guix build utils))))

  (define build-exp
    (with-imported-modules imported-modules
      (with-extensions (list guile-zlib)
        #~(begin
            (use-modules (gnu build linux-modules)
                         (guix build utils)
                         (srfi srfi-1)
                         (srfi srfi-26))

            (define module-dir
              (string-append #$linux "/lib/modules"))

            (define modules
              (let* ((lookup  (cut find-module-file module-dir <>))
                     (modules (map lookup '#$modules)))
                (append modules
                        (recursive-module-dependencies
                         modules
                         #:lookup-module lookup))))

            (define (maybe-uncompress file)
              ;; If FILE is a compressed module, uncompress it, as the initrd
              ;; is already gzipped as a whole.
              (cond
               ((string-contains file ".ko.gz")
                (invoke #+(file-append gzip "/bin/gunzip") file))))

            (mkdir #$output)
            (for-each (lambda (module)
                        (let ((out-module
                               (string-append #$output "/"
                                              (basename module))))
                          (format #t "copying '~a'...~%" module)
                          (copy-file module out-module)
                          (maybe-uncompress out-module)))
                      (delete-duplicates modules))

            ;; Hyphen or underscore?  This database tells us.
            (write-module-name-database #$output)))))

  (computed-file "linux-modules" build-exp))

(define* (raw-initrd file-systems
                      #:key
                      (linux linux-libre)
                      (linux-modules '())
                      (mapped-devices '())
                      (keyboard-layout #f)
                      (helper-packages '())
                      qemu-networking?
                      volatile-root?
                      (on-error 'debug))
  "Return as a file-like object a raw initrd, with kernel
modules taken from LINUX.  FILE-SYSTEMS is a list of file-systems to be
mounted by the initrd, possibly in addition to the root file system specified
on the kernel command line via '--root'. LINUX-MODULES is a list of kernel
modules to be loaded at boot time. MAPPED-DEVICES is a list of device
mappings to realize before FILE-SYSTEMS are mounted.
HELPER-PACKAGES is a list of packages to be copied in the initrd. It may include
e2fsck/static or other packages needed by the initrd to check root partition.

When true, KEYBOARD-LAYOUT is a <keyboard-layout> record denoting the desired
console keyboard layout.  This is done before MAPPED-DEVICES are set up and
before FILE-SYSTEMS are mounted such that, should the user need to enter a
passphrase or use the REPL, this happens using the intended keyboard layout.

When QEMU-NETWORKING? is true, set up networking with the standard QEMU
parameters.

When VOLATILE-ROOT? is true, the root file system is writable but any changes
to it are lost.

ON-ERROR is passed to 'call-with-error-handling'; it determines what happens
upon error."
  (define device-mapping-commands
    ;; List of gexps to open the mapped devices.
    (map (lambda (md)
           (let* ((source  (mapped-device-source md))
                  (targets (mapped-device-targets md))
                  (type    (mapped-device-type md))
                  (open    (mapped-device-kind-open type)))
             (open source targets)))
         mapped-devices))

  (define file-system-scan-commands
    ;; File systems like btrfs need help to assemble multi-device file systems
    ;; but do not use manually-specified <mapped-devices>.
    (let ((file-system-types (map file-system-type file-systems)))
      (if (member "btrfs" file-system-types)
          ;; Ignore errors: if the system manages to boot anyway, the better.
          #~((system* (string-append #$btrfs-progs/static "/bin/btrfs")
                      "device" "scan"))
          #~())))

  (define kodir
    (flat-linux-module-directory linux linux-modules))

  (expression->initrd
   (with-imported-modules (source-module-closure
                           '((gnu build linux-boot)
                             (guix build utils)
                             (guix build bournish)
                             (gnu system file-systems)
                             (gnu build file-systems)))
     #~(begin
         (use-modules (gnu build linux-boot)
                      (gnu system file-systems)
                      ((guix build utils) #:hide (delete))
                      (guix build bournish)   ;add the 'bournish' meta-command
                      (srfi srfi-1)           ;for lvm-device-mapping
                      (srfi srfi-26)

                      ;; FIXME: The following modules are for
                      ;; LUKS-DEVICE-MAPPING.  We should instead propagate
                      ;; this info via gexps.
                      ((gnu build file-systems)
                       #:select (find-partition-by-luks-uuid))
                      (rnrs bytevectors))

         (with-output-to-port (%make-void-port "w")
           (lambda ()
             (set-path-environment-variable "PATH" '("bin" "sbin")
                                            '#$helper-packages)))

         (parameterize ((current-warning-port (%make-void-port "w")))
           (boot-system #:mounts
                        (map spec->file-system
                             '#$(map file-system->spec file-systems))
                        #:pre-mount (lambda ()
                                      (and #$@device-mapping-commands
                                           #$@file-system-scan-commands))
                        #:linux-modules '#$linux-modules
                        #:linux-module-directory '#$kodir
                        #:keymap-file #+(and=> keyboard-layout
                                               keyboard-layout->console-keymap)
                        #:qemu-guest-networking? #$qemu-networking?
                        #:volatile-root? '#$volatile-root?
                        #:on-error '#$on-error))))
   #:name "raw-initrd"))

(define* (file-system-packages file-systems #:key (volatile-root? #f))
  "Return the list of statically-linked, stripped packages to check
FILE-SYSTEMS."
  `(,@(if (find (lambda (fs)
                  (string-prefix? "ext" (file-system-type fs)))
                file-systems)
          (list e2fsck/static)
          '())
    ,@(if (find (lambda (fs)
                  (string-suffix? "fat" (file-system-type fs)))
                file-systems)
          (list fatfsck/static)
          '())
    ,@(if (find (file-system-type-predicate "bcachefs") file-systems)
          (list bcachefs/static)
          '())
    ,@(if (find (file-system-type-predicate "btrfs") file-systems)
          (list btrfs-progs/static)
          '())
    ,@(if (find (file-system-type-predicate "jfs") file-systems)
          (list jfs_fsck/static)
          '())
    ,@(if (find (file-system-type-predicate "ntfs") file-systems)
          (list ntfsfix/static)
          '())
    ,@(if (find (file-system-type-predicate "f2fs") file-systems)
          (list f2fs-fsck/static)
          '())
    ,@(if (find (file-system-type-predicate "xfs") file-systems)
          (list xfs_repair/static)
          '())))

(define-syntax vhash                              ;TODO: factorize
  (syntax-rules (=>)
    "Build a vhash with the given key/value mappings."
    ((_)
     vlist-null)
    ((_ (key others ... => value) rest ...)
     (vhash-cons key value
                 (vhash (others ... => value) rest ...)))
    ((_ (=> value) rest ...)
     (vhash rest ...))))

(define-syntax lookup-procedure
  (syntax-rules (else)
    "Return a procedure that lookups keys in the given dictionary."
    ((_ mapping ... (else default))
     (let ((table (vhash mapping ...)))
       (lambda (key)
         (match (vhash-assoc key table)
           (#f            default)
           ((key . value) value)))))))

(define file-system-type-modules
  ;; Given a file system type, return the list of modules it needs.
  (lookup-procedure ("cifs" => '("md4" "ecb" "cifs"))
                    ("9p" => '("9p" "9pnet_virtio"))
                    ("bcachefs" => '("bcachefs"))
                    ("btrfs" => '("btrfs"))
                    ("iso9660" => '("isofs"))
                    ("jfs" => '("jfs"))
                    ("f2fs" => '("f2fs" "crc32_generic"))
                    ("xfs" => '("xfs"))
                    (else '())))

(define (file-system-modules file-systems)
  "Return the list of Linux modules needed to mount FILE-SYSTEMS."
  (append-map (compose file-system-type-modules file-system-type)
              file-systems))

(define* (default-initrd-modules
           #:optional
           (system (or (%current-target-system)
                       (%current-system))))
  "Return the list of modules included in the initrd by default."
  (define virtio-modules
    ;; Modules for Linux para-virtualized devices, for use in QEMU guests.
    '("virtio_pci" "virtio_balloon" "virtio_blk" "virtio_net"
      "virtio_console" "virtio-rng"))

  `("ahci"                                  ;for SATA controllers
    "usb-storage" "uas"                     ;for the installation image etc.
    "usbhid" "hid-generic" "hid-apple"      ;keyboards during early boot
    "dm-crypt" "xts" "serpent_generic" "wp512" ;for encrypted root partitions
    "nls_iso8859-1"                            ;for `mkfs.fat`, et.al
    ,@(if (string-match "^(x86_64|i[3-6]86)-" system)
          '("framebuffer_coreboot"       ;for display during early (Core)boot
            "pata_acpi" "pata_atiixp"    ;for ATA controllers
            "isci")                      ;for SAS controllers like Intel C602
          '())

    ,@virtio-modules))

(define-syntax %base-initrd-modules
  ;; This more closely matches our naming convention.
  (identifier-syntax (default-initrd-modules)))

(define* (base-initrd file-systems
                      #:key
                      (linux linux-libre)
                      (linux-modules '())
                      (mapped-devices '())
                      (keyboard-layout #f)
                      qemu-networking?
                      volatile-root?
                      (extra-modules '())         ;deprecated
                      (on-error 'debug))
  "Return as a file-like object a generic initrd, with kernel
modules taken from LINUX.  FILE-SYSTEMS is a list of file-systems to be
mounted by the initrd, possibly in addition to the root file system specified
on the kernel command line via '--root'.  MAPPED-DEVICES is a list of device
mappings to realize before FILE-SYSTEMS are mounted.

When true, KEYBOARD-LAYOUT is a <keyboard-layout> record denoting the desired
console keyboard layout.  This is done before MAPPED-DEVICES are set up and
before FILE-SYSTEMS are mounted such that, should the user need to enter a
passphrase or use the REPL, this happens using the intended keyboard layout.

QEMU-NETWORKING? and VOLATILE-ROOT? behaves as in raw-initrd.

The initrd is automatically populated with all the kernel modules necessary
for FILE-SYSTEMS and for the given options.  Additional kernel
modules can be listed in LINUX-MODULES.  They will be added to the initrd, and
loaded at boot time in the order in which they appear."
  (define linux-modules*
    ;; Modules added to the initrd and loaded from the initrd.
    `(,@linux-modules
      ,@(file-system-modules file-systems)
      ,@(if volatile-root?
            '("overlay")
            '())
      ,@extra-modules))

  (define helper-packages
    (append (file-system-packages file-systems
                                  #:volatile-root? volatile-root?)
            (if keyboard-layout
                (list loadkeys-static)
                '())))

  (raw-initrd file-systems
              #:linux linux
              #:linux-modules linux-modules*
              #:mapped-devices mapped-devices
              #:helper-packages helper-packages
              #:keyboard-layout keyboard-layout
              #:qemu-networking? qemu-networking?
              #:volatile-root? volatile-root?
              #:on-error on-error))

;;; linux-initrd.scm ends here
