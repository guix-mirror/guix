;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Christine Lemmer-Webber <cwebber@dustycloud.org>
;;; Copyright © 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (gnu build image)
  #:use-module (guix build store-copy)
  #:use-module (guix build syscalls)
  #:use-module (guix build utils)
  #:use-module (guix store database)
  #:use-module (gnu build bootloader)
  #:use-module (gnu build install)
  #:use-module (gnu build linux-boot)
  #:use-module (gnu image)
  #:use-module (gnu system uuid)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:export (make-partition-image
            convert-disk-image
            genimage
            initialize-efi-partition
            initialize-root-partition

            make-iso9660-image))

(define (sexp->partition sexp)
  "Take SEXP, a tuple as returned by 'partition->gexp', and turn it into a
<partition> record."
  (match sexp
    ((size file-system file-system-options label uuid)
     (partition (size size)
                (file-system file-system)
                (file-system-options file-system-options)
                (label label)
                (uuid uuid)))))

(define (size-in-kib size)
  "Convert SIZE expressed in bytes, to kilobytes and return it as a string."
  (number->string
   (inexact->exact (ceiling (/ size 1024)))))

(define (estimate-partition-size root)
  "Given the ROOT directory, evaluate and return its size.  As this doesn't
take the partition metadata size into account, take a 25% margin."
  (* 1.25 (file-size root)))

(define* (make-ext-image partition target root
                         #:key
                         (owner-uid 0)
                         (owner-gid 0))
  "Handle the creation of EXT2/3/4 partition images. See
'make-partition-image'."
  (let ((size (partition-size partition))
        (fs (partition-file-system partition))
        (fs-options (partition-file-system-options partition))
        (label (partition-label partition))
        (uuid (partition-uuid partition))
        (journal-options "lazy_itable_init=1,lazy_journal_init=1"))
    (apply invoke
           `("fakeroot" "mke2fs" "-t" ,fs "-d" ,root
             "-L" ,label "-U" ,(uuid->string uuid)
             "-E" ,(format #f "root_owner=~a:~a,~a"
                           owner-uid owner-gid journal-options)
             ,@fs-options
             ,target
             ,(format #f "~ak"
                      (size-in-kib
                       (if (eq? size 'guess)
                           (estimate-partition-size root)
                           size)))))))

(define* (make-vfat-image partition target root)
  "Handle the creation of VFAT partition images.  See 'make-partition-image'."
  (let ((size (partition-size partition))
        (label (partition-label partition)))
    (invoke "fakeroot" "mkdosfs" "-n" label "-C" target
            "-F" "16" "-S" "1024"
            (size-in-kib
             (if (eq? size 'guess)
                 (estimate-partition-size root)
                 size)))
    (for-each (lambda (file)
                (unless (member file '("." ".."))
                  (invoke "mcopy" "-bsp" "-i" target
                          (string-append root "/" file)
                          (string-append "::" file))))
              (scandir root))))

(define* (make-partition-image partition-sexp target root)
  "Create and return the image of PARTITION-SEXP as TARGET.  Use the given
ROOT directory to populate the image."
  (let* ((partition (sexp->partition partition-sexp))
         (type (partition-file-system partition)))
    (cond
     ((string-prefix? "ext" type)
      (make-ext-image partition target root))
     ((string=? type "vfat")
      (make-vfat-image partition target root))
     (else
      (raise (condition
              (&message
               (message "unsupported partition type"))))))))

(define (convert-disk-image image format output)
  "Convert IMAGE to OUTPUT according to the given FORMAT."
  (case format
    ((compressed-qcow2)
     (invoke "qemu-img" "convert" "-c" "-f" "raw"
             "-O" "qcow2" image output))
    (else
     (copy-file image output))))

(define* (genimage config)
  "Use genimage to generate in TARGET directory, the image described in the
given CONFIG file."
  ;; genimage needs a 'root' directory.
  (mkdir "root")
  (invoke "genimage" "--config" config))

(define* (register-closure prefix closure
                           #:key
                           (schema (sql-schema))
                           (wal-mode? #t))
  "Register CLOSURE in PREFIX, where PREFIX is the directory name of the
target store and CLOSURE is the name of a file containing a reference graph as
produced by #:references-graphs.  Pass WAL-MODE? to call-with-database."
  (let ((items (call-with-input-file closure read-reference-graph)))
    (parameterize ((sql-schema schema))
      (with-database (store-database-file #:prefix prefix) db
       #:wal-mode? wal-mode?
       (register-items db items
                       #:prefix prefix
                       #:registration-time %epoch)))))

(define* (initialize-efi-partition root
                                   #:key
                                   grub-efi
                                   #:allow-other-keys)
  "Install in ROOT directory, an EFI loader using GRUB-EFI."
  (install-efi-loader grub-efi root))

(define* (initialize-root-partition root
                                    #:key
                                    bootcfg
                                    bootcfg-location
                                    bootloader-package
                                    bootloader-installer
                                    (copy-closures? #t)
                                    (deduplicate? #t)
                                    references-graphs
                                    (register-closures? #t)
                                    system-directory
                                    make-device-nodes
                                    (wal-mode? #t)
                                    #:allow-other-keys)
  "Initialize the given ROOT directory. Use BOOTCFG and BOOTCFG-LOCATION to
install the bootloader configuration.

If COPY-CLOSURES? is true, copy all of REFERENCES-GRAPHS to the partition.  If
REGISTER-CLOSURES? is true, register REFERENCES-GRAPHS in the store.  If
DEDUPLICATE? is true, then also deduplicate files common to CLOSURES and the
rest of the store when registering the closures.  SYSTEM-DIRECTORY is the name
of the directory of the 'system' derivation.  Pass WAL-MODE? to
register-closure."
  (define root-store
    (string-append root (%store-directory)))

  (define tmp-store ".tmp-store")

  (populate-root-file-system system-directory root)

  (when copy-closures?
    (populate-store references-graphs root
                    #:deduplicate? deduplicate?))

  ;; Populate /dev.
  (when make-device-nodes
    (make-device-nodes root))

  (when register-closures?
    (unless copy-closures?
      ;; XXX: 'register-closure' wants to palpate the things it registers, so
      ;; create a symlink to the store.
      (rename-file root-store tmp-store)
      (symlink (%store-directory) root-store))

    (for-each (lambda (closure)
                (register-closure root closure
                                  #:wal-mode? wal-mode?))
              references-graphs)

    (unless copy-closures?
      (delete-file root-store)
      (rename-file tmp-store root-store)))

  ;; There's no point installing a bootloader if we do not populate the store.
  (when copy-closures?
    (when bootloader-installer
      (display "installing bootloader...\n")
      (bootloader-installer bootloader-package #f root))
    (when bootcfg
      (install-boot-config bootcfg bootcfg-location root))))

(define* (make-iso9660-image xorriso grub-mkrescue-environment
                             grub bootcfg system-directory root target
                             #:key (volume-id "Guix_image") (volume-uuid #f)
                             register-closures? (references-graphs '())
                             (compression? #t))
  "Given a GRUB package, creates an iso image as TARGET, using BOOTCFG as
GRUB configuration and OS-DRV as the stuff in it."
  (define grub-mkrescue
    (string-append grub "/bin/grub-mkrescue"))

  (define grub-mkrescue-sed.sh
    (string-append (getcwd) "/" "grub-mkrescue-sed.sh"))

  ;; Use a modified version of grub-mkrescue-sed.sh, see below.
  (copy-file (string-append xorriso
                            "/bin/grub-mkrescue-sed.sh")
             grub-mkrescue-sed.sh)

  ;; Force grub-mkrescue-sed.sh to use the build directory instead of /tmp
  ;; that is read-only inside the build container.
  (substitute* grub-mkrescue-sed.sh
    (("/tmp/") (string-append (getcwd) "/"))
    (("MKRESCUE_SED_XORRISO_ARGS \\$x")
     (format #f "MKRESCUE_SED_XORRISO_ARGS $(echo $x | sed \"s|/tmp|~a|\")"
             (getcwd))))

  ;; 'grub-mkrescue' calls out to mtools programs to create 'efi.img', a FAT
  ;; file system image, and mtools honors SOURCE_DATE_EPOCH for the mtime of
  ;; those files.  The epoch for FAT is Jan. 1st 1980, not 1970, so choose
  ;; that.
  (setenv "SOURCE_DATE_EPOCH"
          (number->string
           (time-second
            (date->time-utc (make-date 0 0 0 0 1 1 1980 0)))))

  ;; Our patched 'grub-mkrescue' honors this environment variable and passes
  ;; it to 'mformat', which makes it the serial number of 'efi.img'.  This
  ;; allows for deterministic builds.
  (setenv "GRUB_FAT_SERIAL_NUMBER"
          (number->string (if volume-uuid

                              ;; On 32-bit systems the 2nd argument must be
                              ;; lower than 2^32.
                              (string-hash (iso9660-uuid->string volume-uuid)
                                           (- (expt 2 32) 1))

                              #x77777777)
                          16))

  (setenv "MKRESCUE_SED_MODE" "original")
  (setenv "MKRESCUE_SED_XORRISO" (string-append xorriso "/bin/xorriso"))
  (setenv "MKRESCUE_SED_IN_EFI_NO_PT" "yes")

  (for-each (match-lambda
              ((name . value) (setenv name value)))
            grub-mkrescue-environment)

  (apply invoke grub-mkrescue
         (string-append "--xorriso=" grub-mkrescue-sed.sh)
         "-o" target
         (string-append "boot/grub/grub.cfg=" bootcfg)
         root
         "--"
         ;; Set all timestamps to 1.
         "-volume_date" "all_file_dates" "=1"

         `(,@(if compression?
                 '(;; ‘zisofs’ compression reduces the total image size by
                   ;; ~60%.
                   "-zisofs" "level=9:block_size=128k" ; highest compression
                   ;; It's transparent to our Linux-Libre kernel but not to
                   ;; GRUB.  Don't compress the kernel, initrd, and other
                   ;; files read by grub.cfg, as well as common
                   ;; already-compressed file names.
                   "-find" "/" "-type" "f"
                   ;; XXX Even after "--" above, and despite documentation
                   ;; claiming otherwise, "-or" is stolen by grub-mkrescue
                   ;; which then chokes on it (as ‘-o …’) and dies.  Don't use
                   ;; "-or".
                   "-not" "-wholename" "/boot/*"
                   "-not" "-wholename" "/System/*"
                   "-not" "-name" "unicode.pf2"
                   "-not" "-name" "bzImage"
                   "-not" "-name" "*.gz"   ; initrd & all man pages
                   "-not" "-name" "*.png"  ; includes grub-image.png
                   "-exec" "set_filter" "--zisofs"
                   "--")
                 '())
           "-volid" ,(string-upcase volume-id)
           ,@(if volume-uuid
             `("-volume_date" "uuid"
               ,(string-filter (lambda (value)
                                 (not (char=? #\- value)))
                               (iso9660-uuid->string
                                volume-uuid)))
             '()))))
