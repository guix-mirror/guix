;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gnu system image)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix monads)
  #:use-module (guix records)
  #:use-module (guix store)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module ((guix self) #:select (make-config.scm))
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu image)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system uuid)
  #:use-module (gnu system vm)
  #:use-module (guix packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages genimage)
  #:use-module (gnu packages guile)
  #:autoload   (gnu packages gnupg) (guile-gcrypt)
  #:use-module (gnu packages hurd)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mtools)
  #:use-module ((srfi srfi-1) #:prefix srfi-1:)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-35)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:export (root-offset
            root-label

            esp-partition
            root-partition

            efi-disk-image
            iso9660-image

            find-image
            system-image))


;;;
;;; Images definitions.
;;;

;; This is the offset before the first partition. GRUB will install itself in
;; this post-MBR gap.
(define root-offset (* 512 2048))

;; Generic root partition label.
(define root-label "Guix_image")

(define esp-partition
  (partition
   (size (* 40 (expt 2 20)))
   (offset root-offset)
   (label "GNU-ESP") ;cosmetic only
   ;; Use "vfat" here since this property is used when mounting.  The actual
   ;; FAT-ness is based on file system size (16 in this case).
   (file-system "vfat")
   (flags '(esp))
   (initializer (gexp initialize-efi-partition))))

(define root-partition
  (partition
   (size 'guess)
   (label root-label)
   (file-system "ext4")
   (flags '(boot))
   (initializer (gexp initialize-root-partition))))

(define efi-disk-image
  (image
   (format 'disk-image)
   (partitions (list esp-partition root-partition))))

(define iso9660-image
  (image
   (format 'iso9660)
   (partitions
    (list (partition
           (size 'guess)
           (label "GUIX_IMAGE")
           (flags '(boot)))))))


;;
;; Helpers.
;;

(define not-config?
  ;; Select (guix …) and (gnu …) modules, except (guix config).
  (match-lambda
    (('guix 'config) #f)
    (('guix rest ...) #t)
    (('gnu rest ...) #t)
    (rest #f)))

(define (partition->gexp partition)
  "Turn PARTITION, a <partition> object, into a list-valued gexp suitable for
'make-partition-image'."
  #~'(#$@(list (partition-size partition))
      #$(partition-file-system partition)
      #$(partition-file-system-options partition)
      #$(partition-label partition)
      #$(and=> (partition-uuid partition)
               uuid-bytevector)))

(define gcrypt-sqlite3&co
  ;; Guile-Gcrypt, Guile-SQLite3, and their propagated inputs.
  (srfi-1:append-map
   (lambda (package)
     (cons package
           (match (package-transitive-propagated-inputs package)
             (((labels packages) ...)
              packages))))
   (list guile-gcrypt guile-sqlite3)))

(define-syntax-rule (with-imported-modules* gexp* ...)
  (with-extensions gcrypt-sqlite3&co
    (with-imported-modules `(,@(source-module-closure
                                '((gnu build vm)
                                  (gnu build image)
                                  (gnu build hurd-boot)
                                  (gnu build linux-boot)
                                  (guix store database))
                                #:select? not-config?)
                             ((guix config) => ,(make-config.scm)))
      #~(begin
          (use-modules (gnu build vm)
                       (gnu build image)
                       (gnu build hurd-boot)
                       (gnu build linux-boot)
                       (guix store database)
                       (guix build utils))
          gexp* ...))))

(define (root-partition? partition)
  "Return true if PARTITION is the root partition, false otherwise."
  (member 'boot (partition-flags partition)))

(define (find-root-partition image)
  "Return the root partition of the given IMAGE."
  (srfi-1:find root-partition? (image-partitions image)))

(define (root-partition-index image)
  "Return the index of the root partition of the given IMAGE."
  (1+ (srfi-1:list-index root-partition? (image-partitions image))))


;;
;; Disk image.
;;

(define* (system-disk-image image
                            #:key
                            (name "disk-image")
                            bootcfg
                            bootloader
                            register-closures?
                            (inputs '()))
  "Return as a file-like object, the disk-image described by IMAGE.  Said
image can be copied on a USB stick as is.  BOOTLOADER is the bootloader that
will be installed and configured according to BOOTCFG parameter.

Raw images of the IMAGE partitions are first created.  Then, genimage is used
to assemble the partition images into a disk-image without resorting to a
virtual machine.

INPUTS is a list of inputs (as for packages).  When REGISTER-CLOSURES? is
true, register INPUTS in the store database of the image so that Guix can be
used in the image."

  (define genimage-name "image")

  (define (image->genimage-cfg image)
    ;; Return as a file-like object, the genimage configuration file
    ;; describing the given IMAGE.
    (define (format->image-type format)
      ;; Return the genimage format corresponding to FORMAT.  For now, only
      ;; the hdimage format (raw disk-image) is supported.
      (case format
        ((disk-image) "hdimage")
        (else
         (raise (condition
                 (&message
                  (message
                   (format #f (G_ "Unsupported image type ~a~%.") format))))))))

    (define (partition->dos-type partition)
      ;; Return the MBR partition type corresponding to the given PARTITION.
      ;; See: https://en.wikipedia.org/wiki/Partition_type.
      (let ((flags (partition-flags partition)))
        (cond
         ((member 'esp flags) "0xEF")
         (else "0x83"))))

    (define (partition-image partition)
      ;; Return as a file-like object, an image of the given PARTITION.  A
      ;; directory, filled by calling the PARTITION initializer procedure, is
      ;; first created within the store.  Then, an image of this directory is
      ;; created using tools such as 'mke2fs' or 'mkdosfs', depending on the
      ;; partition file-system type.
      (let* ((os (image-operating-system image))
             (schema (local-file (search-path %load-path
                                              "guix/store/schema.sql")))
             (graph (match inputs
                      (((names . _) ...)
                       names)))
             (type (partition-file-system partition))
             (image-builder
              (with-imported-modules*
               (let ((initializer #$(partition-initializer partition))
                     (inputs '#+(list e2fsprogs fakeroot dosfstools mtools))
                     (image-root "tmp-root"))
                 (sql-schema #$schema)

                 (set-path-environment-variable "PATH" '("bin" "sbin") inputs)

                 ;; Allow non-ASCII file names--e.g., 'nss-certs'--to be
                 ;; decoded.
                 (setenv "GUIX_LOCPATH"
                         #+(file-append glibc-utf8-locales "/lib/locale"))
                 (setlocale LC_ALL "en_US.utf8")

                 (initializer image-root
                              #:references-graphs '#$graph
                              #:deduplicate? #f
                              #:system-directory #$os
                              #:grub-efi #+grub-efi
                              #:bootloader-package
                              #+(bootloader-package bootloader)
                              #:bootloader-installer
                              #+(bootloader-installer bootloader)
                              #:bootcfg #$bootcfg
                              #:bootcfg-location
                              #$(bootloader-configuration-file bootloader))
                 (make-partition-image #$(partition->gexp partition)
                                       #$output
                                       image-root)))))
        (computed-file "partition.img" image-builder
                       #:options `(#:references-graphs ,inputs))))

    (define (partition->config partition)
      ;; Return the genimage partition configuration for PARTITION.
      (let ((label (partition-label partition))
            (dos-type (partition->dos-type partition))
            (image (partition-image partition))
            (offset (partition-offset partition)))
        #~(format #f "~/partition ~a {
~/~/partition-type = ~a
~/~/image = \"~a\"
~/~/offset = \"~a\"
~/}"
                  #$label
                  #$dos-type
                  #$image
                  #$offset)))

    (let* ((format (image-format image))
           (image-type (format->image-type format))
           (partitions (image-partitions image))
           (partitions-config (map partition->config partitions))
           (builder
            #~(begin
                (let ((format (@ (ice-9 format) format)))
                  (call-with-output-file #$output
                    (lambda (port)
                      (format port
                              "\
image ~a {
~/~a {}
~{~a~^~%~}
}~%" #$genimage-name #$image-type (list #$@partitions-config))))))))
      (computed-file "genimage.cfg" builder)))

  (let* ((image-name (image-name image))
         (name (if image-name
                   (symbol->string image-name)
                   name))
         (substitutable? (image-substitutable? image))
         (builder
          (with-imported-modules*
           (let ((inputs '#+(list genimage coreutils findutils))
                 (bootloader-installer
                  #+(bootloader-disk-image-installer bootloader)))
             (set-path-environment-variable "PATH" '("bin" "sbin") inputs)
             (genimage #$(image->genimage-cfg image) #$output)
             ;; Install the bootloader directly on the disk-image.
             (when bootloader-installer
               (bootloader-installer
                #+(bootloader-package bootloader)
                #$(root-partition-index image)
                (string-append #$output "/" #$genimage-name))))))
         (image-dir (computed-file "image-dir" builder)))
    (computed-file name
                   #~(symlink
                      (string-append #$image-dir "/" #$genimage-name)
                      #$output)
                   #:options `(#:substitutable? ,substitutable?))))


;;
;; ISO9660 image.
;;

(define (has-guix-service-type? os)
  "Return true if OS contains a service of the type GUIX-SERVICE-TYPE."
  (not (not (srfi-1:find (lambda (service)
                           (eq? (service-kind service) guix-service-type))
                         (operating-system-services os)))))

(define* (system-iso9660-image image
                               #:key
                               (name "iso9660-image")
                               bootcfg
                               bootloader
                               register-closures?
                               (inputs '())
                               (grub-mkrescue-environment '()))
  "Return as a file-like object a bootable, stand-alone iso9660 image.

INPUTS is a list of inputs (as for packages).  When REGISTER-CLOSURES? is
true, register INPUTS in the store database of the image so that Guix can be
used in the image. "
  (define root-label
    (match (image-partitions image)
      ((partition)
       (partition-label partition))))

  (define root-uuid
    (match (image-partitions image)
      ((partition)
       (uuid-bytevector (partition-uuid partition)))))

  (let* ((os (image-operating-system image))
         (bootloader (bootloader-package bootloader))
         (compression? (image-compression? image))
         (substitutable? (image-substitutable? image))
         (schema (local-file (search-path %load-path
                                          "guix/store/schema.sql")))
         (graph (match inputs
                  (((names . _) ...)
                   names)))
         (builder
          (with-imported-modules*
           (let* ((inputs '#$(list parted e2fsprogs dosfstools xorriso
                                   sed grep coreutils findutils gawk))
                  (image-root "tmp-root"))
             (sql-schema #$schema)

             ;; Allow non-ASCII file names--e.g., 'nss-certs'--to be decoded.
             (setenv "GUIX_LOCPATH"
                     #+(file-append glibc-utf8-locales "/lib/locale"))

             (setlocale LC_ALL "en_US.utf8")

             (set-path-environment-variable "PATH" '("bin" "sbin") inputs)

             (initialize-root-partition image-root
                                        #:references-graphs '#$graph
                                        #:deduplicate? #f
                                        #:system-directory #$os)
             (make-iso9660-image #$xorriso
                                 '#$grub-mkrescue-environment
                                 #$bootloader
                                 #$bootcfg
                                 #$os
                                 image-root
                                 #$output
                                 #:references-graphs '#$graph
                                 #:register-closures? #$register-closures?
                                 #:compression? #$compression?
                                 #:volume-id #$root-label
                                 #:volume-uuid #$root-uuid)))))
    (computed-file name builder
                   #:options `(#:references-graphs ,inputs
                               #:substitutable? ,substitutable?))))


;;
;; Image creation.
;;

(define (image->root-file-system image)
  "Return the IMAGE root partition file-system type."
  (let ((format (image-format image)))
    (if (eq? format 'iso9660)
        "iso9660"
        (partition-file-system (find-root-partition image)))))

(define (root-size image)
  "Return the root partition size of IMAGE."
  (let* ((image-size (image-size image))
         (root-partition (find-root-partition image))
         (root-size (partition-size root-partition)))
    (cond
     ((and (eq? root-size 'guess) image-size)
      image-size)
     (else root-size))))

(define* (image-with-os base-image os)
  "Return an image based on BASE-IMAGE but with the operating-system field set
to OS.  Also set the UUID and the size of the root partition."
  (define root-file-system
    (srfi-1:find
     (lambda (fs)
       (string=? (file-system-mount-point fs) "/"))
     (operating-system-file-systems os)))

  (image
   (inherit base-image)
   (operating-system os)
   (partitions
    (map (lambda (p)
           (if (root-partition? p)
               (partition
                (inherit p)
                (uuid (file-system-device root-file-system))
                (size (root-size base-image)))
               p))
         (image-partitions base-image)))))

(define (operating-system-for-image image)
  "Return an operating-system based on the one specified in IMAGE, but
suitable for image creation.  Assign an UUID to the root file-system, so that
it can be used for bootloading."
  (define volatile-root? (image-volatile-root? image))

  (define (root-uuid os)
    ;; UUID of the root file system, computed in a deterministic fashion.
    ;; This is what we use to locate the root file system so it has to be
    ;; different from the user's own file system UUIDs.
    (let ((type (if (eq? (image-format image) 'iso9660)
                    'iso9660
                    'dce)))
      (operating-system-uuid os type)))

  (let* ((root-file-system-type (image->root-file-system image))
         (base-os (image-operating-system image))
         (file-systems-to-keep
          (srfi-1:remove
           (lambda (fs)
             (string=? (file-system-mount-point fs) "/"))
           (operating-system-file-systems base-os)))
         (format (image-format image))
         (os
          (operating-system
            (inherit base-os)
            (initrd (lambda (file-systems . rest)
                      (apply (operating-system-initrd base-os)
                             file-systems
                             #:volatile-root? volatile-root?
                             rest)))
            (bootloader (if (eq? format 'iso9660)
                            (bootloader-configuration
                             (inherit
                              (operating-system-bootloader base-os))
                             (bootloader grub-mkrescue-bootloader))
                            (operating-system-bootloader base-os)))
            (file-systems (cons (file-system
                                  (mount-point "/")
                                  (device "/dev/placeholder")
                                  (type root-file-system-type))
                                file-systems-to-keep))))
         (uuid (root-uuid os)))
    (operating-system
      (inherit os)
      (file-systems (cons (file-system
                            (mount-point "/")
                            (device uuid)
                            (type root-file-system-type))
                          file-systems-to-keep)))))

(define* (system-image image)
  "Return the derivation of IMAGE.  It can be a raw disk-image or an ISO9660
image, depending on IMAGE format."
  (define substitutable? (image-substitutable? image))
  (define target (image-target image))

  (with-parameters ((%current-target-system target))
    (let* ((os (operating-system-for-image image))
           (image* (image-with-os image os))
           (register-closures? (has-guix-service-type? os))
           (bootcfg (operating-system-bootcfg os))
           (bootloader (bootloader-configuration-bootloader
                        (operating-system-bootloader os))))
      (case (image-format image)
        ((disk-image)
         (system-disk-image image*
                            #:bootcfg bootcfg
                            #:bootloader bootloader
                            #:register-closures? register-closures?
                            #:inputs `(("system" ,os)
                                       ("bootcfg" ,bootcfg))))
        ((iso9660)
         (system-iso9660-image
          image*
          #:bootcfg bootcfg
          #:bootloader bootloader
          #:register-closures? register-closures?
          #:inputs `(("system" ,os)
                     ("bootcfg" ,bootcfg))
          ;; Make sure to use a mode that does no imply
          ;; HFS+ tree creation that may fail with:
          ;;
          ;; "libisofs: FAILURE : Too much files to mangle,
          ;; cannot guarantee unique file names"
          ;;
          ;; This happens if some limits are exceeded, see:
          ;; https://lists.gnu.org/archive/html/grub-devel/2020-06/msg00048.html
          #:grub-mkrescue-environment
          '(("MKRESCUE_SED_MODE" . "mbr_only"))))))))

(define (find-image file-system-type target)
  "Find and return an image built that could match the given FILE-SYSTEM-TYPE,
built for TARGET.  This is useful to adapt to interfaces written before the
addition of the <image> record."
  (match file-system-type
    ("iso9660" iso9660-image)
    (_ (cond
        ((and target
              (hurd-triplet? target))
         (module-ref (resolve-interface '(gnu system images hurd))
                     'hurd-disk-image))
        (else
         efi-disk-image)))))

;;; image.scm ends here
