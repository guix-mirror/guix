;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Christopher Allan Webber <cwebber@dustycloud.org>
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

(define-module (gnu build vm)
  #:use-module (guix build utils)
  #:use-module (guix build store-copy)
  #:use-module (guix build syscalls)
  #:use-module (guix store database)
  #:use-module (gnu build linux-boot)
  #:use-module (gnu build install)
  #:use-module (gnu system uuid)
  #:use-module (guix records)
  #:use-module ((guix combinators) #:select (fold2))
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 popen)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
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
            partition-flags
            partition-initializer

            estimated-partition-size
            root-partition-initializer
            initialize-partition-table
            initialize-hard-disk
            make-iso9660-image))

;;; Commentary:
;;;
;;; This module provides supporting code to run virtual machines and build
;;; virtual machine images using QEMU.
;;;
;;; Code:

(define* (qemu-command #:optional (system %host-type))
  "Return the default name of the QEMU command for SYSTEM."
  (let ((cpu (substring system 0
                        (string-index system #\-))))
    (string-append "qemu-system-"
                   (if (string-match "^i[3456]86$" cpu)
                       "i386"
                       cpu))))

(define* (load-in-linux-vm builder
                           #:key
                           output
                           (qemu (qemu-command)) (memory-size 512)
                           linux initrd
                           make-disk-image?
                           single-file-output?
                           target-arm32?
                           (disk-image-size (* 100 (expt 2 20)))
                           (disk-image-format "qcow2")
                           (references-graphs '()))
  "Run BUILDER, a Scheme file, into a VM running LINUX with INITRD, and copy
the result to OUTPUT.  If SINGLE-FILE-OUTPUT? is true, copy a single file from
/xchg to OUTPUT.  Otherwise, copy the contents of /xchg to a new directory
OUTPUT.

When MAKE-DISK-IMAGE? is true, OUTPUT will contain a VM image of
DISK-IMAGE-SIZE bytes resulting from the execution of BUILDER, which may
access it via /dev/hda.

REFERENCES-GRAPHS can specify a list of reference-graph files as produced by
the #:references-graphs parameter of 'derivation'."

  (define arch-specific-flags
    `(;; On ARM, a machine has to be specified. Use "virt" machine to avoid
      ;; hardware limits imposed by other machines.
      ,@(if target-arm32? '("-M" "virt") '())

      ;; Only enable kvm if we see /dev/kvm exists.  This allows users without
      ;; hardware virtualization to still use these commands.  KVM support is
      ;; still buggy on some ARM32 boards. Do not use it even if available.
      ,@(if (and (file-exists? "/dev/kvm")
                 (not target-arm32?))
            '("-enable-kvm")
            '())

      ;; Pass "panic=1" so that the guest dies upon error.
      "-append"
      ,(string-append "panic=1 --load=" builder

                      ;; The serial port name differs between emulated
                      ;; architectures/machines.
                      " console="
                      (if target-arm32? "ttyAMA0" "ttyS0"))

      ;; NIC is not supported on ARM "virt" machine, so use a user mode
      ;; network stack instead.
      ,@(if target-arm32?
            '("-device" "virtio-net-pci,netdev=mynet"
              "-netdev" "user,id=mynet")
            '("-net" "nic,model=virtio"))))

  (when make-disk-image?
    (format #t "creating ~a image of ~,2f MiB...~%"
            disk-image-format (/ disk-image-size (expt 2 20)))
    (force-output)
    (invoke "qemu-img" "create" "-f" disk-image-format output
             (number->string disk-image-size)))

  (mkdir "xchg")
  (mkdir "tmp")

  (match references-graphs
    ((graph-files ...)
     ;; Copy the reference-graph files under xchg/ so EXP can access it.
     (map (lambda (file)
            (copy-file file (string-append "xchg/" file)))
          graph-files))
    (_ #f))

  (apply invoke qemu "-nographic" "-no-reboot"
         "-m" (number->string memory-size)
         "-object" "rng-random,filename=/dev/urandom,id=guixsd-vm-rng"
         "-device" "virtio-rng-pci,rng=guixsd-vm-rng"
         "-virtfs"
         (string-append "local,id=store_dev,path="
                        (%store-directory)
                        ",security_model=none,mount_tag=store")
         "-virtfs"
         (string-append "local,id=xchg_dev,path=xchg"
                        ",security_model=none,mount_tag=xchg")
         "-virtfs"
         ;; Some programs require more space in /tmp than is normally
         ;; available in the guest.  Accommodate such programs by sharing a
         ;; temporary directory.
         (string-append "local,id=tmp_dev,path=tmp"
                        ",security_model=none,mount_tag=tmp")
         "-kernel" linux
         "-initrd" initrd
         (append
          (if make-disk-image?
              `("-device" "virtio-blk,drive=myhd"
                "-drive" ,(string-append "if=none,file=" output
                                         ",format=" disk-image-format
                                         ",id=myhd"))
              '())
          arch-specific-flags))

  ;; When MAKE-DISK-IMAGE? is true, the image is in OUTPUT already.
  (unless make-disk-image?
    (if single-file-output?
        (let ((graph? (lambda (name stat)
                        (member (basename name) references-graphs))))
          (match (find-files "xchg" (negate graph?))
            ((result)
             (copy-file result output))
            (x
             (error "did not find a single result file" x))))
        (begin
          (mkdir output)
          (copy-recursively "xchg" output)))))

(define* (register-closure prefix closure
                           #:key
                           (deduplicate? #t) (reset-timestamps? #t)
                           (schema (sql-schema)))
  "Register CLOSURE in PREFIX, where PREFIX is the directory name of the
target store and CLOSURE is the name of a file containing a reference graph as
produced by #:references-graphs..  As a side effect, if RESET-TIMESTAMPS? is
true, reset timestamps on store files and, if DEDUPLICATE? is true,
deduplicates files common to CLOSURE and the rest of PREFIX."
  (let ((items (call-with-input-file closure read-reference-graph)))
    (register-items items
                    #:prefix prefix
                    #:deduplicate? deduplicate?
                    #:reset-timestamps? reset-timestamps?
                    #:registration-time %epoch
                    #:schema schema)))


;;;
;;; Partitions.
;;;

(define-record-type* <partition> partition make-partition
  partition?
  (device      partition-device (default #f))
  (size        partition-size)
  (file-system partition-file-system (default "ext4"))
  (label       partition-label (default #f))
  (uuid        partition-uuid (default #f))
  (flags       partition-flags (default '()))
  (initializer partition-initializer (default (const #t))))

(define (estimated-partition-size graphs)
  "Return the estimated size of a partition that can store the store items
given by GRAPHS, a list of file names produced by #:references-graphs."
  ;; Simply add a 25% overhead.
  (round (* 1.25 (closure-size graphs))))

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
           (append-map (lambda (flag)
                         (list "set" (number->string index)
                               (symbol->string flag) "on"))
                       (partition-flags part))))

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

  (format #t "creating partition table with ~a partitions (~a)...\n"
          (length partitions)
          (string-join (map (compose (cut string-append <> " MiB")
                                     number->string
                                     (lambda (size)
                                       (round (/ size (expt 2. 20))))
                                     partition-size)
                            partitions)
                       ", "))
  (apply invoke "parted" "--script"
         device "mklabel" label-type
         (options partitions offset))

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

(define* (create-ext-file-system partition type
                                 #:key label uuid)
  "Create an ext-family file system of TYPE on PARTITION.  If LABEL is true,
use that as the volume name.  If UUID is true, use it as the partition UUID."
  (format #t "creating ~a partition... ~@[label: ~s~] ~@[uuid: ~s~]\n"
          type label (and uuid (uuid->string uuid)))
  (apply invoke (string-append "mkfs." type)
         "-F" partition
         `(,@(if label
                 `("-L" ,label)
                 '())
           ,@(if uuid
                 `("-U" ,(uuid->string uuid))
                 '()))))

(define* (create-fat-file-system partition
                                 #:key label uuid)
  "Create a FAT file system on PARTITION.  The number of File Allocation Tables
will be determined based on file system size.  If LABEL is true, use that as the
volume name."
  ;; FIXME: UUID is ignored!
  (format #t "creating FAT partition...\n")
  (apply invoke "mkfs.fat" partition
         (if label `("-n" ,label) '())))

(define* (format-partition partition type
                           #:key label uuid)
  "Create a file system TYPE on PARTITION.  If LABEL is true, use that as the
volume name."
  (cond ((string-prefix? "ext" type)
         (create-ext-file-system partition type #:label label #:uuid uuid))
        ((or (string-prefix? "fat" type) (string= "vfat" type))
         (create-fat-file-system partition #:label label #:uuid uuid))
        (else (error "Unsupported file system."))))

(define (initialize-partition partition)
  "Format PARTITION, a <partition> object with a non-#f 'device' field, mount
it, run its initializer, and unmount it."
  (let ((target "/fs"))
   (format-partition (partition-device partition)
                     (partition-file-system partition)
                     #:label (partition-label partition)
                     #:uuid (partition-uuid partition))
   (mkdir-p target)
   (mount (partition-device partition) target
          (partition-file-system partition))

   ((partition-initializer partition) target)

   (umount target)
   partition))

(define* (root-partition-initializer #:key (closures '())
                                     copy-closures?
                                     (register-closures? #t)
                                     system-directory
                                     (deduplicate? #t))
  "Return a procedure to initialize a root partition.

If REGISTER-CLOSURES? is true, register all of CLOSURES in the partition's
store.  If DEDUPLICATE? is true, then also deduplicate files common to
CLOSURES and the rest of the store when registering the closures.  If
COPY-CLOSURES? is true, copy all of CLOSURES to the partition.
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
        ;; XXX: 'register-closure' wants to palpate the things it registers, so
        ;; bind-mount the store on the target.
        (mkdir-p target-store)
        (mount (%store-directory) target-store "" MS_BIND))

      (display "registering closures...\n")
      (for-each (lambda (closure)
                  (register-closure target
                                    (string-append "/xchg/" closure)
                                    #:reset-timestamps? copy-closures?
                                    #:deduplicate? deduplicate?))
                closures)
      (unless copy-closures?
        (umount target-store)))

    ;; Add the non-store directories and files.
    (display "populating...\n")
    (populate-root-file-system system-directory target)

    ;; 'register-closure' resets timestamps and everything, so no need to do it
    ;; once more in that case.
    (unless register-closures?
      (reset-timestamps target))))

(define (register-bootcfg-root target bootcfg)
  "On file system TARGET, register BOOTCFG as a GC root."
  (let ((directory (string-append target "/var/guix/gcroots")))
    (mkdir-p directory)
    (symlink bootcfg (string-append directory "/bootcfg"))))

(define (install-efi grub esp config-file)
  "Write a self-contained GRUB EFI loader to the mounted ESP using CONFIG-FILE."
  (let* ((system %host-type)
         ;; Hard code the output location to a well-known path recognized by
         ;; compliant firmware. See "3.5.1.1 Removable Media Boot Behaviour":
         ;; http://www.uefi.org/sites/default/files/resources/UEFI%20Spec%202_6.pdf
         (grub-mkstandalone (string-append grub "/bin/grub-mkstandalone"))
         (efi-directory (string-append esp "/EFI/BOOT"))
         ;; Map grub target names to boot file names.
         (efi-targets (cond ((string-prefix? "x86_64" system)
                             '("x86_64-efi" . "BOOTX64.EFI"))
                            ((string-prefix? "i686" system)
                             '("i386-efi" . "BOOTIA32.EFI"))
                            ((string-prefix? "armhf" system)
                             '("arm-efi" . "BOOTARM.EFI"))
                            ((string-prefix? "aarch64" system)
                             '("arm64-efi" . "BOOTAA64.EFI")))))
    ;; grub-mkstandalone requires a TMPDIR to prepare the firmware image.
    (setenv "TMPDIR" esp)

    (mkdir-p efi-directory)
    (invoke grub-mkstandalone "-O" (car efi-targets)
            "-o" (string-append efi-directory "/"
                                (cdr efi-targets))
            ;; Graft the configuration file onto the image.
            (string-append "boot/grub/grub.cfg=" config-file))))

(define* (make-iso9660-image grub config-file os-drv target
                             #:key (volume-id "Guix_image") (volume-uuid #f)
                             register-closures? (closures '()))
  "Given a GRUB package, creates an iso image as TARGET, using CONFIG-FILE as
GRUB configuration and OS-DRV as the stuff in it."
  (define grub-mkrescue
    (string-append grub "/bin/grub-mkrescue"))

  (define target-store
    (string-append "/tmp/root" (%store-directory)))

  (define items
    ;; The store items to add to the image.
    (delete-duplicates
     (append-map (lambda (closure)
                   (map store-info-item
                        (call-with-input-file (string-append "/xchg/" closure)
                          read-reference-graph)))
                 closures)))

  (populate-root-file-system os-drv "/tmp/root")
  (mount (%store-directory) target-store "" MS_BIND)

  (when register-closures?
    (display "registering closures...\n")
    (for-each (lambda (closure)
                (register-closure
                 "/tmp/root"
                 (string-append "/xchg/" closure)

                 ;; TARGET-STORE is a read-only bind-mount so we shouldn't try
                 ;; to modify it.
                 #:deduplicate? #f
                 #:reset-timestamps? #f))
              closures)
    (register-bootcfg-root "/tmp/root" config-file))

  ;; 'grub-mkrescue' calls out to mtools programs to create 'efi.img', a FAT
  ;; file system image, and mtools honors SOURCE_DATE_EPOCH for the mtime of
  ;; those files.  The epoch for FAT is Jan. 1st 1980, not 1970, so choose
  ;; that.
  (setenv "SOURCE_DATE_EPOCH"
          (number->string
           (time-second
            (date->time-utc (make-date 0 0 0 0 1 1 1980 0)))))

  (let ((pipe
         (apply open-pipe* OPEN_WRITE
                grub-mkrescue "-o" target
                (string-append "boot/grub/grub.cfg=" config-file)
                "etc=/tmp/root/etc"
                "var=/tmp/root/var"
                "run=/tmp/root/run"
                ;; /mnt is used as part of the installation
                ;; process, as the mount point for the target
                ;; file system, so create it.
                "mnt=/tmp/root/mnt"
                "-path-list" "-"
                "--"

                ;; Set all timestamps to 1.
                "-volume_date" "all_file_dates" "=1"

                "-volid" (string-upcase volume-id)
                (if volume-uuid
                    `("-volume_date" "uuid"
                      ,(string-filter (lambda (value)
                                        (not (char=? #\- value)))
                                      (iso9660-uuid->string
                                       volume-uuid)))
                    `()))))
    ;; Pass lines like 'gnu/store/…-x=/gnu/store/…-x' corresponding to the
    ;; '-path-list -' option.
    (for-each (lambda (item)
                (format pipe "~a=~a~%"
                        (string-drop item 1) item))
              items)
    (unless (zero? (close-pipe pipe))
      (error "oh, my! grub-mkrescue failed" grub-mkrescue))))

(define* (initialize-hard-disk device
                               #:key
                               bootloader-package
                               bootcfg
                               bootcfg-location
                               bootloader-installer
                               (grub-efi #f)
                               (partitions '()))
  "Initialize DEVICE as a disk containing all the <partition> objects listed
in PARTITIONS, and using BOOTCFG as its bootloader configuration file.

Each partition is initialized by calling its 'initializer' procedure,
passing it a directory name where it is mounted."

  (define (partition-bootable? partition)
    "Return the first partition found with the boot flag set."
    (member 'boot (partition-flags partition)))

  (define (partition-esp? partition)
    "Return the first EFI System Partition."
    (member 'esp (partition-flags partition)))

  (let* ((partitions (initialize-partition-table device partitions))
         (root       (find partition-bootable? partitions))
         (esp        (find partition-esp? partitions))
         (target     "/fs"))
    (unless root
      (error "no bootable partition specified" partitions))

    (for-each initialize-partition partitions)

    (display "mounting root partition...\n")
    (mkdir-p target)
    (mount (partition-device root) target (partition-file-system root))
    (install-boot-config bootcfg bootcfg-location target)
    (when bootloader-installer
      (display "installing bootloader...\n")
      (bootloader-installer bootloader-package device target))

    (when esp
      ;; Mount the ESP somewhere and install GRUB UEFI image.
      (let ((mount-point (string-append target "/boot/efi"))
            (grub-config (string-append target "/tmp/grub-standalone.cfg")))
        (display "mounting EFI system partition...\n")
        (mkdir-p mount-point)
        (mount (partition-device esp) mount-point
               (partition-file-system esp))

        ;; Create a tiny configuration file telling the embedded grub
        ;; where to load the real thing.
        ;; XXX This is quite fragile, and can prevent the image from booting
        ;; when there's more than one volume with this label present.
        ;; Reproducible almost-UUIDs could reduce the risk (not eliminate it).
        (call-with-output-file grub-config
          (lambda (port)
            (format port
                    "insmod part_msdos~@
                    search --set=root --label Guix_image~@
                    configfile /boot/grub/grub.cfg~%")))

        (display "creating EFI firmware image...")
        (install-efi grub-efi mount-point grub-config)
        (display "done.\n")

        (delete-file grub-config)
        (umount mount-point)))

    ;; Register BOOTCFG as a GC root.
    (register-bootcfg-root target bootcfg)

    (umount target)))

;;; vm.scm ends here
