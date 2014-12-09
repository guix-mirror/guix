;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu build vm)
  #:use-module (guix build utils)
  #:use-module (guix build store-copy)
  #:use-module (gnu build linux-boot)
  #:use-module (gnu build install)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-26)
  #:export (qemu-command
            load-in-linux-vm
            format-partition
            initialize-root-partition
            initialize-partition-table
            initialize-hard-disk))

;;; Commentary:
;;;
;;; This module provides supporting code to run virtual machines and build
;;; virtual machine images using QEMU.
;;;
;;; Code:

(define* (qemu-command #:optional (system %host-type))
  "Return the default name of the QEMU command for SYSTEM."
  (let ((cpu (substring %host-type 0
                        (string-index %host-type #\-))))
    (string-append "qemu-system-"
                   (if (string-match "^i[3456]86$" cpu)
                       "i386"
                       cpu))))

(define* (load-in-linux-vm builder
                           #:key
                           output
                           (qemu (qemu-command)) (memory-size 512)
                           linux initrd
                           make-disk-image? (disk-image-size 100)
                           (disk-image-format "qcow2")
                           (references-graphs '()))
  "Run BUILDER, a Scheme file, into a VM running LINUX with INITRD, and copy
the result to OUTPUT.

When MAKE-DISK-IMAGE? is true, OUTPUT will contain a VM image of
DISK-IMAGE-SIZE MiB resulting from the execution of BUILDER, which may access
it via /dev/hda.

REFERENCES-GRAPHS can specify a list of reference-graph files as produced by
the #:references-graphs parameter of 'derivation'."
  (define image-file
    (string-append "image." disk-image-format))

  (when make-disk-image?
    (unless (zero? (system* "qemu-img" "create" "-f" disk-image-format
                            image-file
                            (number->string disk-image-size)))
      (error "qemu-img failed")))

  (mkdir "xchg")

  (match references-graphs
    ((graph-files ...)
     ;; Copy the reference-graph files under xchg/ so EXP can access it.
     (map (lambda (file)
            (copy-file file (string-append "xchg/" file)))
          graph-files))
    (_ #f))

  (unless (zero?
           (apply system* qemu "-enable-kvm" "-nographic" "-no-reboot"
                  "-m" (number->string memory-size)
                  "-net" "nic,model=virtio"
                  "-virtfs"
                  (string-append "local,id=store_dev,path="
                                 (%store-directory)
                                 ",security_model=none,mount_tag=store")
                  "-virtfs"
                  (string-append "local,id=xchg_dev,path=xchg"
                                 ",security_model=none,mount_tag=xchg")
                  "-kernel" linux
                  "-initrd" initrd
                  "-append" (string-append "console=ttyS0 --load="
                                           builder)
                  (if make-disk-image?
                      `("-drive" ,(string-append "file=" image-file
                                                 ",if=virtio"))
                      '())))
    (error "qemu failed" qemu))

  (if make-disk-image?
      (copy-file image-file output)
      (begin
        (mkdir output)
        (copy-recursively "xchg" output))))

(define* (initialize-partition-table device partition-size
                                     #:key
                                     bootable?
                                     (label-type "msdos")
                                     (offset (expt 2 20)))
  "Create on DEVICE a partition table of type LABEL-TYPE, with a single
partition of PARTITION-SIZE bytes starting at OFFSET bytes.  When BOOTABLE? is
true, set the bootable flag on the partition.  Return #t on success."
  (format #t "creating partition table with a ~a B partition...\n"
          partition-size)
  (unless (zero? (apply system* "parted" device "mklabel" label-type
                        "mkpart" "primary" "ext2"
                        (format #f "~aB" offset)
                        (format #f "~aB" partition-size)
                        (if bootable?
                            '("set" "1" "boot" "on")
                            '())))
    (error "failed to create partition table")))

(define MS_BIND 4096)                             ; <sys/mounts.h> again!

(define* (format-partition partition type
                           #:key label)
  "Create a file system TYPE on PARTITION.  If LABEL is true, use that as the
volume name."
  (format #t "creating ~a partition...\n" type)
  (unless (zero? (apply system* (string-append "mkfs." type)
                        "-F" partition
                        (if label
                            `("-L" ,label)
                            '())))
    (error "failed to create partition")))

(define* (initialize-root-partition target-directory
                                    #:key copy-closures? register-closures?
                                    closures system-directory)
  "Initialize the root partition mounted at TARGET-DIRECTORY."
  (define target-store
    (string-append target-directory (%store-directory)))

  (when copy-closures?
    ;; Populate the store.
    (populate-store (map (cut string-append "/xchg/" <>) closures)
                    target-directory))

  ;; Populate /dev.
  (make-essential-device-nodes #:root target-directory)

  ;; Optionally, register the inputs in the image's store.
  (when register-closures?
    (unless copy-closures?
      ;; XXX: 'guix-register' wants to palpate the things it registers, so
      ;; bind-mount the store on the target.
      (mkdir-p target-store)
      (mount (%store-directory) target-store "" MS_BIND))

    (display "registering closures...\n")
    (for-each (lambda (closure)
                (register-closure target-directory
                                  (string-append "/xchg/" closure)))
              closures)
    (unless copy-closures?
      (system* "umount" target-store)))

  ;; Add the non-store directories and files.
  (display "populating...\n")
  (populate-root-file-system system-directory target-directory))

(define (register-grub.cfg-root target grub.cfg)
  "On file system TARGET, register GRUB.CFG as a GC root."
  (let ((directory (string-append target "/var/guix/gcroots")))
    (mkdir-p directory)
    (symlink grub.cfg (string-append directory "/grub.cfg"))))

(define* (initialize-hard-disk device
                               #:key
                               system-directory
                               grub.cfg
                               disk-image-size
                               (file-system-type "ext4")
                               file-system-label
                               (closures '())
                               copy-closures?
                               (bootable? #t)
                               (register-closures? #t))
  "Initialize DEVICE, a disk of DISK-IMAGE-SIZE bytes, with a FILE-SYSTEM-TYPE
partition with (optionally) FILE-SYSTEM-LABEL as its volume name, and with
GRUB installed.  When BOOTABLE? is true, set the bootable flag on that
partition.

If REGISTER-CLOSURES? is true, register all of CLOSURES is the partition's
store.  If COPY-CLOSURES? is true, copy all of CLOSURES to the partition.
SYSTEM-DIRECTORY is the name of the directory of the 'system' derivation."
  (define target-directory
    "/fs")

  (define partition
    (string-append device "1"))

  (initialize-partition-table device
                              (- disk-image-size (* 5 (expt 2 20)))
                              #:bootable? bootable?)

  (format-partition partition file-system-type
                    #:label file-system-label)

  (display "mounting partition...\n")
  (mkdir target-directory)
  (mount partition target-directory file-system-type)

  (initialize-root-partition target-directory
                             #:system-directory system-directory
                             #:copy-closures? copy-closures?
                             #:register-closures? register-closures?
                             #:closures closures)

  (install-grub grub.cfg device target-directory)

  ;; Register GRUB.CFG as a GC root.
  (register-grub.cfg-root target-directory grub.cfg)

  ;; 'guix-register' resets timestamps and everything, so no need to do it
  ;; once more in that case.
  (unless register-closures?
    (reset-timestamps target-directory))

  (zero? (system* "umount" target-directory)))

;;; vm.scm ends here
