;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gnu system linux-initrd)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix i18n)
  #:use-module ((guix store)
                #:select (%store-prefix))
  #:use-module ((guix derivations)
                #:select (derivation->output-path))
  #:use-module (guix modules)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages guile)
  #:use-module ((gnu packages make-bootstrap)
                #:select (%guile-static-stripped))
  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices)
  #:autoload   (gnu build linux-modules)
                 (device-module-aliases matching-modules)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:export (expression->initrd
            %base-initrd-modules
            raw-initrd
            file-system-packages
            base-initrd
            check-device-initrd-modules))


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
  "Return a derivation that builds a Linux initrd (a gzipped cpio archive)
containing GUILE and that evaluates EXP, a G-expression, upon booting.  All
the derivations referenced by EXP are automatically copied to the initrd."

  ;; General Linux overview in `Documentation/early-userspace/README' and
  ;; `Documentation/filesystems/ramfs-rootfs-initramfs.txt'.

  (define init
    (program-file "init" exp #:guile guile))

  (define builder
    (with-imported-modules (source-module-closure
                            '((gnu build linux-initrd)))
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

          (build-initrd (string-append #$output "/initrd")
                        #:guile #$guile
                        #:init #$init
                        ;; Copy everything INIT refers to into the initrd.
                        #:references-graphs '("closure")
                        #:gzip (string-append #$gzip "/bin/gzip")))))

  (gexp->derivation name builder
                    #:references-graphs `(("closure" ,init))))

(define (flat-linux-module-directory linux modules)
  "Return a flat directory containing the Linux kernel modules listed in
MODULES and taken from LINUX."
  (define build-exp
    (with-imported-modules (source-module-closure
                            '((guix build utils)
                              (gnu build linux-modules)))
      #~(begin
          (use-modules (ice-9 match) (ice-9 regex)
                       (srfi srfi-1)
                       (guix build utils)
                       (gnu build linux-modules))

          (define (string->regexp str)
            ;; Return a regexp that matches STR exactly.
            (string-append "^" (regexp-quote str) "$"))

          (define module-dir
            (string-append #$linux "/lib/modules"))

          (define (lookup module)
            (let ((name (ensure-dot-ko module)))
              (match (find-files module-dir (string->regexp name))
                ((file)
                 file)
                (()
                 (error "module not found" name module-dir))
                ((_ ...)
                 (error "several modules by that name"
                        name module-dir)))))

          (define modules
            (let ((modules (map lookup '#$modules)))
              (append modules
                      (recursive-module-dependencies modules
                                                     #:lookup-module lookup))))

          (mkdir #$output)
          (for-each (lambda (module)
                      (format #t "copying '~a'...~%" module)
                      (copy-file module
                                 (string-append #$output "/"
                                                (basename module))))
                    (delete-duplicates modules)))))

  (computed-file "linux-modules" build-exp))

(define* (raw-initrd file-systems
                      #:key
                      (linux linux-libre)
                      (linux-modules '())
                      (mapped-devices '())
                      (helper-packages '())
                      qemu-networking?
                      volatile-root?
                      (on-error 'debug))
  "Return a monadic derivation that builds a raw initrd, with kernel
modules taken from LINUX.  FILE-SYSTEMS is a list of file-systems to be
mounted by the initrd, possibly in addition to the root file system specified
on the kernel command line via '--root'. LINUX-MODULES is a list of kernel
modules to be loaded at boot time. MAPPED-DEVICES is a list of device
mappings to realize before FILE-SYSTEMS are mounted.
HELPER-PACKAGES is a list of packages to be copied in the initrd. It may include
e2fsck/static or other packages needed by the initrd to check root partition.

When QEMU-NETWORKING? is true, set up networking with the standard QEMU
parameters.

When VOLATILE-ROOT? is true, the root file system is writable but any changes
to it are lost.

ON-ERROR is passed to 'call-with-error-handling'; it determines what happens
upon error."
  (define device-mapping-commands
    ;; List of gexps to open the mapped devices.
    (map (lambda (md)
           (let* ((source (mapped-device-source md))
                  (target (mapped-device-target md))
                  (type   (mapped-device-type md))
                  (open   (mapped-device-kind-open type)))
             (open source target)))
         mapped-devices))

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
                      (guix build utils)
                      (guix build bournish)   ;add the 'bournish' meta-command
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

         (boot-system #:mounts
                      (map spec->file-system
                           '#$(map file-system->spec file-systems))
                      #:pre-mount (lambda ()
                                    (and #$@device-mapping-commands))
                      #:linux-modules '#$linux-modules
                      #:linux-module-directory '#$kodir
                      #:qemu-guest-networking? #$qemu-networking?
                      #:volatile-root? '#$volatile-root?
                      #:on-error '#$on-error)))
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
    ,@(if (find (file-system-type-predicate "btrfs") file-systems)
          (list btrfs-progs/static)
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
                    ("btrfs" => '("btrfs"))
                    ("iso9660" => '("isofs"))
                    (else '())))

(define (file-system-modules file-systems)
  "Return the list of Linux modules needed to mount FILE-SYSTEMS."
  (append-map (compose file-system-type-modules file-system-type)
              file-systems))

(define* (default-initrd-modules #:optional (system (%current-system)))
  "Return the list of modules included in the initrd by default."
  (define virtio-modules
    ;; Modules for Linux para-virtualized devices, for use in QEMU guests.
    '("virtio_pci" "virtio_balloon" "virtio_blk" "virtio_net"
      "virtio_console"))

  `("ahci"                                  ;for SATA controllers
    "usb-storage" "uas"                     ;for the installation image etc.
    "usbhid" "hid-generic" "hid-apple"      ;keyboards during early boot
    "dm-crypt" "xts" "serpent_generic" "wp512" ;for encrypted root partitions
    "nls_iso8859-1"                            ;for `mkfs.fat`, et.al
    ,@(if (string-match "^(x86_64|i[3-6]86)-" system)
          '("pata_acpi" "pata_atiixp"    ;for ATA controllers
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
                      qemu-networking?
                      volatile-root?
                      (extra-modules '())         ;deprecated
                      (on-error 'debug))
  "Return a monadic derivation that builds a generic initrd, with kernel
modules taken from LINUX.  FILE-SYSTEMS is a list of file-systems to be
mounted by the initrd, possibly in addition to the root file system specified
on the kernel command line via '--root'.  MAPPED-DEVICES is a list of device
mappings to realize before FILE-SYSTEMS are mounted.

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
    (file-system-packages file-systems #:volatile-root? volatile-root?))

  (raw-initrd file-systems
              #:linux linux
              #:linux-modules linux-modules*
              #:mapped-devices mapped-devices
              #:helper-packages helper-packages
              #:qemu-networking? qemu-networking?
              #:volatile-root? volatile-root?
              #:on-error on-error))

(define (check-device-initrd-modules device linux-modules location)
  "Raise an error if DEVICE needs modules beyond LINUX-MODULES to operate.
DEVICE must be a \"/dev\" file name."
  (let ((modules (delete-duplicates
                  (append-map matching-modules
                              (device-module-aliases device)))))
    (unless (every (cute member <> linux-modules) modules)
      (raise (condition
              (&message
               (message (format #f (G_ "you may need these modules \
in the initrd for ~a:~{ ~a~}")
                                device modules)))
              (&fix-hint
               (hint (format #f (G_ "Try adding them to the
@code{initrd-modules} field of your @code{operating-system} declaration, along
these lines:

@example
 (operating-system
   ;; @dots{}
   (initrd-modules (append (list~{ ~s~})
                           %base-initrd-modules)))
@end example\n")
                             modules)))
              (&error-location
               (location (source-properties->location location))))))))

;;; linux-initrd.scm ends here
