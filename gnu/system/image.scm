;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020, 2021 Mathieu Othacehe <m.othacehe@gmail.com>
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
  #:use-module (guix diagnostics)
  #:use-module (guix discovery)
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
  #:use-module (gnu platform)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system linux-container)
  #:use-module (gnu system uuid)
  #:use-module (gnu system vm)
  #:use-module (guix packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages genimage)
  #:use-module (gnu packages guile)
  #:autoload   (gnu packages gnupg) (guile-gcrypt)
  #:use-module (gnu packages hurd)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mtools)
  #:use-module (gnu packages virtualization)
  #:use-module ((srfi srfi-1) #:prefix srfi-1:)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:export (root-offset
            root-label

            esp-partition
            root-partition

            efi-disk-image
            iso9660-image
            docker-image
            raw-with-offset-disk-image

            image-with-os
            efi-raw-image-type
            qcow2-image-type
            iso-image-type
            uncompressed-iso-image-type
            docker-image-type
            raw-with-offset-image-type

            image-with-label
            system-image

            %image-types
            lookup-image-type-by-name))


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

(define docker-image
  (image
   (format 'docker)))

(define* (raw-with-offset-disk-image #:optional (offset root-offset))
  (image
   (format 'disk-image)
   (partitions
    (list (partition
           (inherit root-partition)
           (offset offset))))
   ;; FIXME: Deleting and creating "/var/run" and "/tmp" on the overlayfs
   ;; fails.
   (volatile-root? #f)))


;;;
;;; Images types.
;;;

(define-syntax-rule (image-with-os base-image os)
  "Return an image inheriting from BASE-IMAGE, with the operating-system field
set to the given OS."
  (image
   (inherit base-image)
   (operating-system os)))

(define efi-raw-image-type
  (image-type
   (name 'efi-raw)
   (constructor (cut image-with-os efi-disk-image <>))))

(define qcow2-image-type
  (image-type
   (name 'qcow2)
   (constructor (cut image-with-os
                 (image
                  (inherit efi-disk-image)
                  (name 'image.qcow2)
                  (format 'compressed-qcow2))
                 <>))))

(define iso-image-type
  (image-type
   (name 'iso9660)
   (constructor (cut image-with-os iso9660-image <>))))

(define uncompressed-iso-image-type
  (image-type
   (name 'uncompressed-iso9660)
   (constructor (cut image-with-os
                 (image
                  (inherit iso9660-image)
                  (compression? #f))
                 <>))))

(define docker-image-type
  (image-type
   (name 'docker)
   (constructor (cut image-with-os docker-image <>))))

(define raw-with-offset-image-type
  (image-type
   (name 'raw-with-offset)
   (constructor (cut image-with-os (raw-with-offset-disk-image) <>))))


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
                                '((gnu build image)
                                  (gnu build bootloader)
                                  (gnu build hurd-boot)
                                  (gnu build linux-boot)
                                  (guix store database))
                                #:select? not-config?)
                             ((guix config) => ,(make-config.scm)))
      #~(begin
          (use-modules (gnu build image)
                       (gnu build bootloader)
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
      (cond
       ((memq format '(disk-image compressed-qcow2)) "hdimage")
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

    (define (partition->gpt-type partition)
      ;; Return the genimage GPT partition type code corresponding to PARTITION.
      ;; See https://github.com/pengutronix/genimage/blob/master/README.rst
      (let ((flags (partition-flags partition)))
        (cond
          ((member 'esp flags) "U")
          (else "L"))))

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
                              #:copy-closures? (not
                                                #$(image-shared-store? image))
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
                       ;; Allow offloading so that this I/O-intensive process
                       ;; doesn't run on the build farm's head node.
                       #:local-build? #f
                       #:options `(#:references-graphs ,inputs))))

    (define (gpt-image? image)
      (eq? 'gpt (image-partition-table-type image)))

    (define (partition-type-values image partition)
      (if (gpt-image? image)
          (values "partition-type-uuid" (partition->gpt-type partition))
          (values "partition-type" (partition->dos-type partition))))

    (define (partition->config image partition)
      ;; Return the genimage partition configuration for PARTITION.
      (let-values (((partition-type-attribute partition-type-value)
                    (partition-type-values image partition)))
        (let ((label (partition-label partition))
              (image (partition-image partition))
              (offset (partition-offset partition)))
          #~(format #f "~/partition ~a {
  ~/~/~a = ~a
  ~/~/image = \"~a\"
  ~/~/offset = \"~a\"
  ~/}"
                    #$label
                    #$partition-type-attribute
                    #$partition-type-value
                    #$image
                    #$offset))))

    (define (genimage-type-options image-type image)
      (cond
        ((equal? image-type "hdimage")
         (format #f "~%~/~/gpt = ~a~%~/"
                 (if (gpt-image? image) "true" "false")))
        (else "")))

    (let* ((format (image-format image))
           (image-type (format->image-type format))
           (image-type-options (genimage-type-options image-type image))
           (partitions (image-partitions image))
           (partitions-config (map (cut partition->config image <>) partitions))
           (builder
            #~(begin
                (let ((format (@ (ice-9 format) format)))
                  (call-with-output-file #$output
                    (lambda (port)
                      (format port
                              "\
image ~a {
~/~a {~a}
~{~a~^~%~}
}~%" #$genimage-name #$image-type #$image-type-options
 (list #$@partitions-config))))))))
      (computed-file "genimage.cfg" builder)))

  (let* ((image-name (image-name image))
         (name (if image-name
                   (symbol->string image-name)
                   name))
         (format (image-format image))
         (substitutable? (image-substitutable? image))
         (builder
          (with-imported-modules*
           (let ((inputs '#+(list genimage coreutils findutils qemu-minimal))
                 (bootloader-installer
                  #+(bootloader-disk-image-installer bootloader))
                 (out-image (string-append "images/" #$genimage-name)))
             (set-path-environment-variable "PATH" '("bin" "sbin") inputs)
             (genimage #$(image->genimage-cfg image))
             ;; Install the bootloader directly on the disk-image.
             (when bootloader-installer
               (bootloader-installer
                #+(bootloader-package bootloader)
                #$(root-partition-index image)
                out-image))
             (convert-disk-image out-image '#$format #$output)))))
    (computed-file name builder
                   #:local-build? #f              ;too I/O-intensive
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
                               (name "image.iso")
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
                   ;; Allow offloading so that this I/O-intensive process
                   ;; doesn't run on the build farm's head node.
                   #:local-build? #f
                   #:options `(#:references-graphs ,inputs
                               #:substitutable? ,substitutable?))))

(define (image-with-label base-image label)
  "The volume ID of an ISO is the label of the first partition.  This procedure
returns an image record where the first partition's label is set to <label>."
  (image
    (inherit base-image)
    (partitions
      (match (image-partitions base-image)
        ((boot others ...)
         (cons
           (partition
             (inherit boot)
             (label label))
           others))))))


;;
;; Docker image.
;;

(define* (system-docker-image image
                              #:key
                              (name "docker-image"))
  "Build a docker image for IMAGE.  NAME is the base name to use for the
output file."
  (define boot-program
    ;; Program that runs the boot script of OS, which in turn starts shepherd.
    (program-file "boot-program"
                  #~(let ((system (cadr (command-line))))
                      (setenv "GUIX_NEW_SYSTEM" system)
                      (execl #$(file-append guile-3.0 "/bin/guile")
                             "guile" "--no-auto-compile"
                             (string-append system "/boot")))))

  (define shared-network?
    (image-shared-network? image))

  (let* ((os (operating-system-with-gc-roots
              (containerized-operating-system
               (image-operating-system image) '()
               #:shared-network?
               shared-network?)
              (list boot-program)))
         (substitutable? (image-substitutable? image))
         (register-closures? (has-guix-service-type? os))
         (schema (and register-closures?
                      (local-file (search-path %load-path
                                               "guix/store/schema.sql"))))
         (name (string-append name ".tar.gz"))
         (graph "system-graph"))
    (define builder
      (with-extensions (cons guile-json-3         ;for (guix docker)
                             gcrypt-sqlite3&co)   ;for (guix store database)
        (with-imported-modules `(,@(source-module-closure
                                    '((guix docker)
                                      (guix store database)
                                      (guix build utils)
                                      (guix build store-copy)
                                      (gnu build image))
                                    #:select? not-config?)
                                 ((guix config) => ,(make-config.scm)))
          #~(begin
              (use-modules (guix docker)
                           (guix build utils)
                           (gnu build image)
                           (srfi srfi-19)
                           (guix build store-copy)
                           (guix store database))

              ;; Set the SQL schema location.
              (sql-schema #$schema)

              ;; Allow non-ASCII file names--e.g., 'nss-certs'--to be decoded.
              (setenv "GUIX_LOCPATH"
                      #+(file-append glibc-utf8-locales "/lib/locale"))
              (setlocale LC_ALL "en_US.utf8")

              (set-path-environment-variable "PATH" '("bin" "sbin") '(#+tar))

              (let ((image-root (string-append (getcwd) "/tmp-root")))
                (mkdir-p image-root)
                (initialize-root-partition image-root
                                           #:references-graphs '(#$graph)
                                           #:copy-closures? #f
                                           #:register-closures? #$register-closures?
                                           #:deduplicate? #f
                                           #:system-directory #$os)
                (build-docker-image
                 #$output
                 (cons* image-root
                        (map store-info-item
                             (call-with-input-file #$graph
                               read-reference-graph)))
                 #$os
                 #:entry-point '(#$boot-program #$os)
                 #:compressor '(#+(file-append gzip "/bin/gzip") "-9n")
                 #:creation-time (make-time time-utc 0 1)
                 #:transformations `((,image-root -> ""))))))))

    (computed-file name builder
                   ;; Allow offloading so that this I/O-intensive process
                   ;; doesn't run on the build farm's head node.
                   #:local-build? #f
                   #:options `(#:references-graphs ((,graph ,os))
                               #:substitutable? ,substitutable?))))


;;
;; Image creation.
;;

(define (image->root-file-system image)
  "Return the IMAGE root partition file-system type."
  (case (image-format image)
    ((iso9660) "iso9660")
    ((docker) "dummy")
    (else
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

(define* (image-with-os* base-image os)
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
  (define volatile-root? (if (eq? (image-format image) 'iso9660)
                             #t
                             (image-volatile-root? image)))

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
             (let ((mount-point (file-system-mount-point fs)))
               (or (string=? mount-point "/")
                   (string=? mount-point "/boot/efi"))))
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
  (define platform (image-platform image))

  ;; The image platform definition may provide the appropriate "system"
  ;; architecture for the image.  If we are already running on this system,
  ;; the image can be built natively.  If we are running on a different
  ;; system, then we need to cross-compile, using the "target" provided by the
  ;; image definition.
  (define system (and=> platform platform-system))
  (define target (cond
                  ;; No defined platform, let's use the user defined
                  ;; system/target parameters.
                  ((not platform)
                   (%current-target-system))
                  ;; The current system is the same as the platform system, no
                  ;; need to cross-compile.
                  ((and system
                        (string=? system (%current-system)))
                   #f)
                  ;; If there is a user defined target let's override the
                  ;; platform target. Otherwise, we can cross-compile to the
                  ;; platform target.
                  (else
                   (or (%current-target-system)
                       (and=> platform platform-target)))))

  (with-parameters ((%current-target-system target))
    (let* ((os (operating-system-for-image image))
           (image* (image-with-os* image os))
           (image-format (image-format image))
           (register-closures? (has-guix-service-type? os))
           (bootcfg (operating-system-bootcfg os))
           (bootloader (bootloader-configuration-bootloader
                        (operating-system-bootloader os))))
      (cond
       ((memq image-format '(disk-image compressed-qcow2))
         (system-disk-image image*
                            #:bootcfg bootcfg
                            #:bootloader bootloader
                            #:register-closures? register-closures?
                            #:inputs `(("system" ,os)
                                       ("bootcfg" ,bootcfg))))
       ((memq image-format '(docker))
        (system-docker-image image*))
       ((memq image-format '(iso9660))
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


;;
;; Image detection.
;;

(define (image-modules)
  "Return the list of image modules."
  (cons (resolve-interface '(gnu system image))
        (all-modules (map (lambda (entry)
                            `(,entry . "gnu/system/images/"))
                          %load-path)
                     #:warn warn-about-load-error)))

(define %image-types
  ;; The list of publically-known image types.
  (delay (fold-module-public-variables (lambda (obj result)
                                         (if (image-type? obj)
                                             (cons obj result)
                                             result))
                                       '()
                                       (image-modules))))

(define (lookup-image-type-by-name name)
  "Return the image type called NAME."
  (or (srfi-1:find (lambda (image-type)
                     (eq? name (image-type-name image-type)))
                   (force %image-types))
      (raise
       (formatted-message (G_ "~a: no such image type") name))))

;;; image.scm ends here
