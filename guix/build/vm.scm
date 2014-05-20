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

(define-module (guix build vm)
  #:use-module (guix build utils)
  #:use-module (guix build linux-initrd)
  #:use-module (guix build install)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (load-in-linux-vm
            initialize-hard-disk))

;;; Commentary:
;;;
;;; This module provides supporting code to run virtual machines and build
;;; virtual machine images using QEMU.
;;;
;;; Code:

(define (qemu-command)
  "Return the default name of the QEMU command for the current host."
  (string-append "qemu-system-"
                 (substring %host-type 0
                            (string-index %host-type #\-))))


(define* (load-in-linux-vm builder
                           #:key
                           output
                           (qemu (qemu-command)) (memory-size 512)
                           linux initrd
                           make-disk-image? (disk-image-size 100)
                           (references-graphs '()))
  "Run BUILDER, a Scheme file, into a VM running LINUX with INITRD, and copy
the result to OUTPUT.

When MAKE-DISK-IMAGE? is true, OUTPUT will contain a VM image of
DISK-IMAGE-SIZE MiB resulting from the execution of BUILDER, which may access
it via /dev/hda.

REFERENCES-GRAPHS can specify a list of reference-graph files as produced by
the #:references-graphs parameter of 'derivation'."

  (when make-disk-image?
    (unless (zero? (system* "qemu-img" "create" "-f" "qcow2" "image.qcow2"
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
                      '("-hda" "image.qcow2")
                      '())))
    (error "qemu failed" qemu))

  (if make-disk-image?
      (copy-file "image.qcow2"            ; XXX: who mkdir'd OUTPUT?
                 output)
      (begin
        (mkdir output)
        (copy-recursively "xchg" output))))

(define (read-reference-graph port)
  "Return a list of store paths from the reference graph at PORT.
The data at PORT is the format produced by #:references-graphs."
  (let loop ((line   (read-line port))
             (result '()))
    (cond ((eof-object? line)
           (delete-duplicates result))
          ((string-prefix? "/" line)
           (loop (read-line port)
                 (cons line result)))
          (else
           (loop (read-line port)
                 result)))))

(define* (initialize-partition-table device
                                     #:key
                                     (label-type "msdos")
                                     partition-size)
  "Create on DEVICE a partition table of type LABEL-TYPE, with a single
partition of PARTITION-SIZE MiB.  Return #t on success."
  (display "creating partition table...\n")
  (zero? (system* "parted" device "mklabel" label-type
                  "mkpart" "primary" "ext2" "1MiB"
                  (format #f "~aB" partition-size))))

(define* (populate-store reference-graphs target)
  "Populate the store under directory TARGET with the items specified in
REFERENCE-GRAPHS, a list of reference-graph files."
  (define store
    (string-append target (%store-directory)))

  (define (things-to-copy)
    ;; Return the list of store files to copy to the image.
    (define (graph-from-file file)
      (call-with-input-file file read-reference-graph))

    (delete-duplicates (append-map graph-from-file reference-graphs)))

  (mkdir-p store)
  (chmod store #o1775)
  (for-each (lambda (thing)
              (copy-recursively thing
                                (string-append target thing)))
            (things-to-copy)))

(define MS_BIND 4096)                             ; <sys/mounts.h> again!

(define* (initialize-hard-disk device
                               #:key
                               grub.cfg
                               disk-image-size
                               (file-system-type "ext4")
                               (closures '())
                               copy-closures?
                               (register-closures? #t)
                               (directives '()))
  "Initialize DEVICE, a disk of DISK-IMAGE-SIZE bytes, with a
FILE-SYSTEM-TYPE partition, and with GRUB installed.  If REGISTER-CLOSURES? is
true, register all of CLOSURES is the partition's store.  If COPY-CLOSURES? is
true, copy all of CLOSURES to the partition.  Lastly, apply DIRECTIVES to
further populate the partition."
  (define target-directory
    "/fs")

  (define target-store
    (string-append target-directory (%store-directory)))

  (define partition
    (string-append device "1"))

  (unless (initialize-partition-table device
                                      #:partition-size
                                      (- disk-image-size (* 5 (expt 2 20))))
    (error "failed to create partition table"))

  (format #t "creating ~a partition...\n" file-system-type)
  (unless (zero? (system* (string-append "mkfs." file-system-type)
                          "-F" partition))
    (error "failed to create partition"))

  (display "mounting partition...\n")
  (mkdir target-directory)
  (mount partition target-directory file-system-type)

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

  ;; Evaluate the POPULATE directives.
  (display "populating...\n")
  (populate-root-file-system target-directory)

  (unless (install-grub grub.cfg device target-directory)
    (error "failed to install GRUB"))

  ;; 'guix-register' resets timestamps and everything, so no need to do it
  ;; once more in that case.
  (unless register-closures?
    (reset-timestamps target-directory))

  (zero? (system* "umount" target-directory)))

;;; vm.scm ends here
