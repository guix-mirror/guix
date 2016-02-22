;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Christopher Allan Webber <cwebber@dustycloud.org>
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
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
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:export (qemu-command
            load-in-linux-vm
            format-partition

            partition
            partition?
            partition-device
            partition-size
            partition-file-system
            partition-label
            partition-bootable?
            partition-initializer

            root-partition-initializer
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
           (apply system* qemu "-nographic" "-no-reboot"
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
                  (append
                   (if make-disk-image?
                       `("-drive" ,(string-append "file=" image-file
                                                  ",if=virtio"))
                       '())
                   ;; Only enable kvm if we see /dev/kvm exists.
                   ;; This allows users without hardware virtualization to still
                   ;; use these commands.
                   (if (file-exists? "/dev/kvm")
                       '("-enable-kvm")
                       '()))))
    (error "qemu failed" qemu))

  (if make-disk-image?
      (copy-file image-file output)
      (begin
        (mkdir output)
        (copy-recursively "xchg" output))))


;;;
;;; Partitions.
;;;

(define-record-type* <partition> partition make-partition
  partition?
  (device      partition-device (default #f))
  (size        partition-size)
  (file-system partition-file-system (default "ext4"))
  (label       partition-label (default #f))
  (bootable?   partition-bootable? (default #f))
  (initializer partition-initializer (default (const #t))))

(define (fold2 proc seed1 seed2 lst)              ;TODO: factorize
  "Like `fold', but with a single list and two seeds."
  (let loop ((result1 seed1)
             (result2 seed2)
             (lst     lst))
    (if (null? lst)
        (values result1 result2)
        (call-with-values
            (lambda () (proc (car lst) result1 result2))
          (lambda (result1 result2)
            (loop result1 result2 (cdr lst)))))))

(define* (initialize-partition-table device partitions
                                     #:key
                                     (label-type "msdos")
                                     (offset (expt 2 20)))
  "Create on DEVICE a partition table of type LABEL-TYPE, containing the given
PARTITIONS (a list of <partition> objects), starting at OFFSET bytes.  On
success, return PARTITIONS with their 'device' field changed to reflect their
actual /dev name based on DEVICE."
  (define (partition-options part offset index)
    (cons* "mkpart" "primary" "ext2"
           (format #f "~aB" offset)
           (format #f "~aB" (+ offset (partition-size part)))
           (if (partition-bootable? part)
               `("set" ,(number->string index) "boot" "on")
               '())))

  (define (options partitions offset)
    (let loop ((partitions partitions)
               (offset     offset)
               (index      1)
               (result     '()))
      (match partitions
        (()
         (concatenate (reverse result)))
        ((head tail ...)
         (loop tail
               ;; Leave one sector (512B) between partitions to placate
               ;; Parted.
               (+ offset 512 (partition-size head))
               (+ 1 index)
               (cons (partition-options head offset index)
                     result))))))

  (format #t "creating partition table with ~a partitions...\n"
          (length partitions))
  (unless (zero? (apply system* "parted" "--script"
                        device "mklabel" label-type
                        (options partitions offset)))
    (error "failed to create partition table"))

  ;; Set the 'device' field of each partition.
  (reverse
   (fold2 (lambda (part result index)
            (values (cons  (partition
                            (inherit part)
                            (device (string-append device
                                                   (number->string index))))
                           result)
                    (+ 1 index)))
          '()
          1
          partitions)))

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

(define (initialize-partition partition)
  "Format PARTITION, a <partition> object with a non-#f 'device' field, mount
it, run its initializer, and unmount it."
  (let ((target "/fs"))
   (format-partition (partition-device partition)
                     (partition-file-system partition)
                     #:label (partition-label partition))
   (mkdir-p target)
   (mount (partition-device partition) target
          (partition-file-system partition))

   ((partition-initializer partition) target)

   (umount target)
   partition))

(define* (root-partition-initializer #:key (closures '())
                                     copy-closures?
                                     (register-closures? #t)
                                     system-directory)
  "Return a procedure to initialize a root partition.

If REGISTER-CLOSURES? is true, register all of CLOSURES is the partition's
store.  If COPY-CLOSURES? is true, copy all of CLOSURES to the partition.
SYSTEM-DIRECTORY is the name of the directory of the 'system' derivation."
  (lambda (target)
    (define target-store
      (string-append target (%store-directory)))

    (when copy-closures?
      ;; Populate the store.
      (populate-store (map (cut string-append "/xchg/" <>) closures)
                      target))

    ;; Populate /dev.
    (make-essential-device-nodes #:root target)

    ;; Optionally, register the inputs in the image's store.
    (when register-closures?
      (unless copy-closures?
        ;; XXX: 'guix-register' wants to palpate the things it registers, so
        ;; bind-mount the store on the target.
        (mkdir-p target-store)
        (mount (%store-directory) target-store "" MS_BIND))

      (display "registering closures...\n")
      (for-each (lambda (closure)
                  (register-closure target
                                    (string-append "/xchg/" closure)))
                closures)
      (unless copy-closures?
        (umount target-store)))

    ;; Add the non-store directories and files.
    (display "populating...\n")
    (populate-root-file-system system-directory target)

    ;; 'guix-register' resets timestamps and everything, so no need to do it
    ;; once more in that case.
    (unless register-closures?
      (reset-timestamps target))))

(define (register-grub.cfg-root target grub.cfg)
  "On file system TARGET, register GRUB.CFG as a GC root."
  (let ((directory (string-append target "/var/guix/gcroots")))
    (mkdir-p directory)
    (symlink grub.cfg (string-append directory "/grub.cfg"))))

(define* (initialize-hard-disk device
                               #:key
                               grub.cfg
                               (partitions '()))
  "Initialize DEVICE as a disk containing all the <partition> objects listed
in PARTITIONS, and using GRUB.CFG as its bootloader configuration file.

Each partition is initialized by calling its 'initializer' procedure,
passing it a directory name where it is mounted."
  (let* ((partitions (initialize-partition-table device partitions))
         (root       (find partition-bootable? partitions))
         (target     "/fs"))
    (unless root
      (error "no bootable partition specified" partitions))

    (for-each initialize-partition partitions)

    (display "mounting root partition...\n")
    (mkdir-p target)
    (mount (partition-device root) target (partition-file-system root))
    (install-grub grub.cfg device target)

    ;; Register GRUB.CFG as a GC root.
    (register-grub.cfg-root target grub.cfg)

    (umount target)))

;;; vm.scm ends here
