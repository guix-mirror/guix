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
  (zero? (system* "parted" "/dev/sda" "mklabel" label-type
                  "mkpart" "primary" "ext2" "1MiB"
                  (format #f "~aB" partition-size))))

(define* (install-grub grub.cfg device mount-point)
  "Install GRUB with GRUB.CFG on DEVICE, which is assumed to be mounted on
MOUNT-POINT.  Return #t on success."
  (mkdir-p (string-append mount-point "/boot/grub"))
  (symlink grub.cfg (string-append mount-point "/boot/grub/grub.cfg"))
  (zero? (system* "grub-install" "--no-floppy"
                  "--boot-directory" (string-append mount-point "/boot")
                  device)))

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

(define (evaluate-populate-directive directive target)
  "Evaluate DIRECTIVE, an sexp describing a file or directory to create under
directory TARGET."
  (match directive
    (('directory name)
     (mkdir-p (string-append target name)))
    (('directory name uid gid)
     (let ((dir (string-append target name)))
       (mkdir-p dir)
       (chown dir uid gid)))
    ((new '-> old)
     (symlink old (string-append target new)))))

(define (reset-timestamps directory)
  "Reset the timestamps of all the files under DIRECTORY, so that they appear
as created and modified at the Epoch."
  (display "clearing file timestamps...\n")
  (for-each (lambda (file)
              (let ((s (lstat file)))
                ;; XXX: Guile uses libc's 'utime' function (not 'futime'), so
                ;; the timestamp of symlinks cannot be changed, and there are
                ;; symlinks here pointing to /gnu/store, which is the host,
                ;; read-only store.
                (unless (eq? (stat:type s) 'symlink)
                  (utime file 0 0 0 0))))
            (find-files directory "")))

(define* (initialize-hard-disk #:key
                               grub.cfg
                               disk-image-size
                               (mkfs "mkfs.ext3")
                               initialize-store?
                               (closures-to-copy '())
                               (directives '()))
  (unless (initialize-partition-table "/dev/sda"
                                      #:partition-size
                                      (- disk-image-size (* 5 (expt 2 20))))
    (error "failed to create partition table"))

  (display "creating ext3 partition...\n")
  (unless (zero? (system* mkfs "-F" "/dev/sda1"))
    (error "failed to create partition"))

  (display "mounting partition...\n")
  (mkdir "/fs")
  (mount "/dev/sda1" "/fs" "ext3")

  (when (pair? closures-to-copy)
    ;; Populate the store.
    (populate-store (map (cut string-append "/xchg/" <>)
                         closures-to-copy)
                    "/fs"))

  ;; Populate /dev.
  (make-essential-device-nodes #:root "/fs")

  ;; Optionally, register the inputs in the image's store.
  (when initialize-store?
    (for-each (lambda (closure)
                (let ((status (system* "guix-register" "--prefix" "/fs"
                                       (string-append "/xchg/" closure))))
                  (unless (zero? status)
                    (error "failed to register store items" closure))))
              closures-to-copy))

  ;; Evaluate the POPULATE directives.
  (for-each (cut evaluate-populate-directive <> "/fs")
            directives)

  (unless (install-grub grub.cfg "/dev/sda" "/fs")
    (error "failed to install GRUB"))

  (reset-timestamps "/fs")

  (zero? (system* "umount" "/fs")))

;;; vm.scm ends here
