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

(define-module (gnu system vm)
  #:use-module (guix config)
  #:use-module (guix store)
  #:use-module (guix gexp)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (guix monads)
  #:use-module (guix records)
  #:use-module (guix modules)
  #:use-module (guix utils)
  #:use-module (gcrypt hash)
  #:use-module (guix base32)
  #:use-module ((guix self) #:select (make-config.scm))

  #:use-module ((gnu build vm)
                #:select (qemu-command))
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages guile)
  #:autoload   (gnu packages gnupg) (guile-gcrypt)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages less)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages zile)
  #:use-module (gnu packages linux)
  #:use-module ((gnu packages make-bootstrap)
                #:select (%guile-static-stripped))
  #:use-module (gnu packages admin)

  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu system shadow)
  #:use-module (gnu system pam)
  #:use-module (gnu system linux-initrd)
  #:use-module (gnu bootloader)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system)
  #:use-module (gnu services)
  #:use-module (gnu system uuid)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)

  #:export (expression->derivation-in-linux-vm
            qemu-image
            virtualized-operating-system
            system-qemu-image

            system-qemu-image/shared-store
            system-qemu-image/shared-store-script
            system-disk-image
            system-docker-image

            virtual-machine
            virtual-machine?))


;;; Commentary:
;;;
;;; Tools to evaluate build expressions within virtual machines.
;;;
;;; Code:

(define %linux-vm-file-systems
  ;; File systems mounted for 'derivation-in-linux-vm'.  These are shared with
  ;; the host over 9p.
  (list (file-system
          (mount-point (%store-prefix))
          (device "store")
          (type "9p")
          (needed-for-boot? #t)
          (flags '(read-only))
          (options "trans=virtio,cache=loose")
          (check? #f))

        ;; The 9p documentation says that cache=loose is "intended for
        ;; exclusive, read-only mounts", without additional details.  In
        ;; practice it seems to work well for these, and it's much faster than
        ;; the default cache=none, especially when copying and registering
        ;; store items.
        (file-system
          (mount-point "/xchg")
          (device "xchg")
          (type "9p")
          (needed-for-boot? #t)
          (options "trans=virtio,cache=loose")
          (check? #f))
        (file-system
          (mount-point "/tmp")
          (device "tmp")
          (type "9p")
          (needed-for-boot? #t)
          (options "trans=virtio,cache=loose")
          (check? #f))))

(define not-config?
  ;; Select (guix …) and (gnu …) modules, except (guix config).
  (match-lambda
    (('guix 'config) #f)
    (('guix rest ...) #t)
    (('gnu rest ...) #t)
    (rest #f)))

(define gcrypt-sqlite3&co
  ;; Guile-Gcrypt, Guile-SQLite3, and their propagated inputs.
  (append-map (lambda (package)
                (cons package
                      (match (package-transitive-propagated-inputs package)
                        (((labels packages) ...)
                         packages))))
              (list guile-gcrypt guile-sqlite3)))

(define* (expression->derivation-in-linux-vm name exp
                                             #:key
                                             (system (%current-system))
                                             (linux linux-libre)
                                             initrd
                                             (qemu qemu-minimal)
                                             (env-vars '())
                                             (guile-for-build
                                              (%guile-for-build))
                                             (file-systems
                                              %linux-vm-file-systems)

                                             (single-file-output? #f)
                                             (make-disk-image? #f)
                                             (references-graphs #f)
                                             (memory-size 256)
                                             (disk-image-format "qcow2")
                                             (disk-image-size 'guess))
  "Evaluate EXP in a QEMU virtual machine running LINUX with INITRD (a
derivation).  The virtual machine runs with MEMORY-SIZE MiB of memory.  In the
virtual machine, EXP has access to FILE-SYSTEMS, which, by default, includes a
9p share of the store, the '/xchg' where EXP should put its output file(s),
and a 9p share of /tmp.

If SINGLE-FILE-OUTPUT? is true, copy a single file from '/xchg' to OUTPUT.
Otherwise, copy the contents of /xchg to a new directory OUTPUT.

When MAKE-DISK-IMAGE? is true, then create a QEMU disk image of type
DISK-IMAGE-FORMAT (e.g., 'qcow2' or 'raw'), of DISK-IMAGE-SIZE bytes and
return it.  When DISK-IMAGE-SIZE is 'guess, estimate the image size based
based on the size of the closure of REFERENCES-GRAPHS.

When REFERENCES-GRAPHS is true, it must be a list of file name/store path
pairs, as for `derivation'.  The files containing the reference graphs are
made available under the /xchg CIFS share."
  (define user-builder
    (program-file "builder-in-linux-vm" exp))

  (define loader
    ;; Invoke USER-BUILDER instead using 'primitive-load'.  The reason for
    ;; this is to allow USER-BUILDER to dlopen stuff by using a full-featured
    ;; Guile, which it couldn't do using the statically-linked guile used in
    ;; the initrd.  See example at
    ;; <https://lists.gnu.org/archive/html/guix-devel/2017-10/msg00233.html>.
    (program-file "linux-vm-loader"
                  ;; When USER-BUILDER succeeds, reboot (indicating a
                  ;; success), otherwise die, which causes a kernel panic
                  ;; ("Attempted to kill init!").
                  #~(when (zero? (system* #$user-builder))
                      (reboot))))

  (let ((initrd (or initrd
                    (base-initrd file-systems
                                 #:on-error 'backtrace
                                 #:linux linux
                                 #:linux-modules %base-initrd-modules
                                 #:qemu-networking? #t))))

    (define builder
      ;; Code that launches the VM that evaluates EXP.
      (with-extensions gcrypt-sqlite3&co
        (with-imported-modules `(,@(source-module-closure
                                    '((guix build utils)
                                      (gnu build vm))
                                    #:select? not-config?)

                                 ;; For consumption by (gnu store database).
                                 ((guix config) => ,(make-config.scm)))
          #~(begin
              (use-modules (guix build utils)
                           (gnu build vm))

              (let* ((inputs  '#$(list qemu (canonical-package coreutils)))
                     (linux   (string-append #$linux "/"
                                             #$(system-linux-image-file-name)))
                     (initrd  #$initrd)
                     (loader  #$loader)
                     (graphs  '#$(match references-graphs
                                   (((graph-files . _) ...) graph-files)
                                   (_ #f)))
                     (size    #$(if (eq? 'guess disk-image-size)
                                    #~(+ (* 70 (expt 2 20)) ;ESP
                                         (estimated-partition-size graphs))
                                    disk-image-size)))

                (set-path-environment-variable "PATH" '("bin") inputs)

                (load-in-linux-vm loader
                                  #:output #$output
                                  #:linux linux #:initrd initrd
                                  #:memory-size #$memory-size
                                  #:make-disk-image? #$make-disk-image?
                                  #:single-file-output? #$single-file-output?
                                  ;; FIXME: ‘target-arm32?’ may not operate on
                                  ;; the right system/target values.  Rewrite
                                  ;; using ‘let-system’ when available.
                                  #:target-arm32? #$(target-arm32?)
                                  #:disk-image-format #$disk-image-format
                                  #:disk-image-size size
                                  #:references-graphs graphs))))))

    (gexp->derivation name builder
                      ;; TODO: Require the "kvm" feature.
                      #:system system
                      #:env-vars env-vars
                      #:guile-for-build guile-for-build
                      #:references-graphs references-graphs)))

(define* (iso9660-image #:key
                        (name "iso9660-image")
                        file-system-label
                        file-system-uuid
                        (system (%current-system))
                        (qemu qemu-minimal)
                        os
                        bootcfg-drv
                        bootloader
                        register-closures?
                        (inputs '()))
  "Return a bootable, stand-alone iso9660 image.

INPUTS is a list of inputs (as for packages)."
  (define schema
    (and register-closures?
         (local-file (search-path %load-path
                                  "guix/store/schema.sql"))))

  (expression->derivation-in-linux-vm
   name
   (with-extensions gcrypt-sqlite3&co
     (with-imported-modules `(,@(source-module-closure '((gnu build vm)
                                                         (guix store database)
                                                         (guix build utils))
                                                       #:select? not-config?)
                              ((guix config) => ,(make-config.scm)))
       #~(begin
           (use-modules (gnu build vm)
                        (guix store database)
                        (guix build utils))

           (sql-schema #$schema)

           (let ((inputs
                  '#$(append (list qemu parted e2fsprogs dosfstools xorriso)
                             (map canonical-package
                                  (list sed grep coreutils findutils gawk))))


                 (graphs     '#$(match inputs
                                  (((names . _) ...)
                                   names)))
                 ;; This variable is unused but allows us to add INPUTS-TO-COPY
                 ;; as inputs.
                 (to-register
                  '#$(map (match-lambda
                            ((name thing) thing)
                            ((name thing output) `(,thing ,output)))
                          inputs)))

             (set-path-environment-variable "PATH" '("bin" "sbin") inputs)
             (make-iso9660-image #$(bootloader-package bootloader)
                                 #$bootcfg-drv
                                 #$os
                                 "/xchg/guixsd.iso"
                                 #:register-closures? #$register-closures?
                                 #:closures graphs
                                 #:volume-id #$file-system-label
                                 #:volume-uuid #$(and=> file-system-uuid
                                                        uuid-bytevector))))))
   #:system system

   ;; Keep a local file system for /tmp so that we can populate it directly as
   ;; root and have files owned by root.  See <https://bugs.gnu.org/31752>.
   #:file-systems (remove (lambda (file-system)
                            (string=? (file-system-mount-point file-system)
                                      "/tmp"))
                          %linux-vm-file-systems)

   #:make-disk-image? #f
   #:single-file-output? #t
   #:references-graphs inputs))

(define* (qemu-image #:key
                     (name "qemu-image")
                     (system (%current-system))
                     (qemu qemu-minimal)
                     (disk-image-size 'guess)
                     (disk-image-format "qcow2")
                     (file-system-type "ext4")
                     file-system-label
                     file-system-uuid
                     os
                     bootcfg-drv
                     bootloader
                     (register-closures? #t)
                     (inputs '())
                     copy-inputs?)
  "Return a bootable, stand-alone QEMU image of type DISK-IMAGE-FORMAT (e.g.,
'qcow2' or 'raw'), with a root partition of type FILE-SYSTEM-TYPE.
Optionally, FILE-SYSTEM-LABEL can be specified as the volume name for the root
partition; likewise FILE-SYSTEM-UUID, if true, specifies the UUID of the root
partition (a UUID object).

The returned image is a full disk image that runs OS-DERIVATION,
with a GRUB installation that uses GRUB-CONFIGURATION as its configuration
file (GRUB-CONFIGURATION must be the name of a file in the VM.)

INPUTS is a list of inputs (as for packages).  When COPY-INPUTS? is true, copy
all of INPUTS into the image being built.  When REGISTER-CLOSURES? is true,
register INPUTS in the store database of the image so that Guix can be used in
the image."
  (define schema
    (and register-closures?
         (local-file (search-path %load-path
                                  "guix/store/schema.sql"))))

  (expression->derivation-in-linux-vm
   name
   (with-extensions gcrypt-sqlite3&co
     (with-imported-modules `(,@(source-module-closure '((gnu build vm)
                                                         (gnu build bootloader)
                                                         (guix store database)
                                                         (guix build utils))
                                                       #:select? not-config?)
                              ((guix config) => ,(make-config.scm)))
       #~(begin
           (use-modules (gnu build bootloader)
                        (gnu build vm)
                        (guix store database)
                        (guix build utils)
                        (srfi srfi-26)
                        (ice-9 binary-ports))

           (sql-schema #$schema)

           (let ((inputs
                  '#$(append (list qemu parted e2fsprogs dosfstools)
                             (map canonical-package
                                  (list sed grep coreutils findutils gawk))))

                 ;; This variable is unused but allows us to add INPUTS-TO-COPY
                 ;; as inputs.
                 (to-register
                  '#$(map (match-lambda
                            ((name thing) thing)
                            ((name thing output) `(,thing ,output)))
                          inputs)))

             (set-path-environment-variable "PATH" '("bin" "sbin") inputs)

             (let* ((graphs     '#$(match inputs
                                     (((names . _) ...)
                                      names)))
                    (initialize (root-partition-initializer
                                 #:closures graphs
                                 #:copy-closures? #$copy-inputs?
                                 #:register-closures? #$register-closures?
                                 #:system-directory #$os

                                 ;; Disable deduplication to speed things up,
                                 ;; and because it doesn't help much for a
                                 ;; single system generation.
                                 #:deduplicate? #f))
                    (root-size  #$(if (eq? 'guess disk-image-size)
                                      #~(max
                                         ;; Minimum 20 MiB root size
                                         (* 20 (expt 2 20))
                                         (estimated-partition-size
                                          (map (cut string-append "/xchg/" <>)
                                               graphs)))
                                      (- disk-image-size
                                         (* 50 (expt 2 20)))))
                    (partitions
                     (append
                      (list (partition
                             (size root-size)
                             (label #$file-system-label)
                             (uuid #$(and=> file-system-uuid
                                            uuid-bytevector))
                             (file-system #$file-system-type)
                             (flags '(boot))
                             (initializer initialize)))
                      ;; Append a small EFI System Partition for use with UEFI
                      ;; bootloaders if we are not targeting ARM because UEFI
                      ;; support in U-Boot is experimental.
                      ;;
                      ;; FIXME: ‘target-arm32?’ may be not operate on the right
                      ;; system/target values.  Rewrite using ‘let-system’ when
                      ;; available.
                      (if #$(target-arm32?)
                          '()
                          (list (partition
                                 ;; The standalone grub image is about 10MiB, but
                                 ;; leave some room for custom or multiple images.
                                 (size (* 40 (expt 2 20)))
                                 (label "GNU-ESP") ;cosmetic only
                                 ;; Use "vfat" here since this property is used
                                 ;; when mounting. The actual FAT-ness is based
                                 ;; on file system size (16 in this case).
                                 (file-system "vfat")
                                 (flags '(esp))))))))
               (initialize-hard-disk "/dev/vda"
                                     #:partitions partitions
                                     #:grub-efi #$grub-efi
                                     #:bootloader-package
                                     #$(bootloader-package bootloader)
                                     #:bootcfg #$bootcfg-drv
                                     #:bootcfg-location
                                     #$(bootloader-configuration-file bootloader)
                                     #:bootloader-installer
                                     #$(bootloader-installer bootloader)))))))
   #:system system
   #:make-disk-image? #t
   #:disk-image-size disk-image-size
   #:disk-image-format disk-image-format
   #:references-graphs inputs))

(define* (system-docker-image os
                              #:key
                              (name "guixsd-docker-image")
                              register-closures?)
  "Build a docker image.  OS is the desired <operating-system>.  NAME is the
base name to use for the output file.  When REGISTER-CLOSURES? is not #f,
register the closure of OS with Guix in the resulting Docker image.  This only
makes sense when you want to build a GuixSD Docker image that has Guix
installed inside of it.  If you don't need Guix (e.g., your GuixSD Docker
image just contains a web server that is started by the Shepherd), then you
should set REGISTER-CLOSURES? to #f."
  (define schema
    (and register-closures?
         (local-file (search-path %load-path
                                  "guix/store/schema.sql"))))

  (mlet %store-monad ((os-drv (operating-system-derivation os #:container? #t))
                      (name -> (string-append name ".tar.gz"))
                      (graph -> "system-graph"))
    (define build
      (with-extensions (cons guile-json           ;for (guix docker)
                             gcrypt-sqlite3&co)   ;for (guix store database)
        (with-imported-modules `(,@(source-module-closure
                                    '((guix docker)
                                      (guix store database)
                                      (guix build utils)
                                      (guix build store-copy)
                                      (gnu build vm))
                                    #:select? not-config?)
                                 ((guix config) => ,(make-config.scm)))
          #~(begin
              (use-modules (guix docker)
                           (guix build utils)
                           (gnu build vm)
                           (srfi srfi-19)
                           (guix build store-copy)
                           (guix store database))

              ;; Set the SQL schema location.
              (sql-schema #$schema)

              (let* (;; This initializer requires elevated privileges that are
                     ;; not normally available in the build environment (e.g.,
                     ;; it needs to create device nodes).  In order to obtain
                     ;; such privileges, we run it as root in a VM.
                     (initialize (root-partition-initializer
                                  #:closures '(#$graph)
                                  #:register-closures? #$register-closures?
                                  #:system-directory #$os-drv
                                  ;; De-duplication would fail due to
                                  ;; cross-device link errors, so don't do it.
                                  #:deduplicate? #f))
                     ;; Even as root in a VM, the initializer would fail due to
                     ;; lack of privileges if we use a root-directory that is on
                     ;; a file system that is shared with the host (e.g., /tmp).
                     (root-directory "/guixsd-system-root"))
                (set-path-environment-variable "PATH" '("bin" "sbin") '(#+tar))
                (mkdir root-directory)
                (initialize root-directory)
                (build-docker-image
                 (string-append "/xchg/" #$name) ;; The output file.
                 (cons* root-directory
                        (map store-info-item
                             (call-with-input-file
                                 (string-append "/xchg/" #$graph)
                               read-reference-graph)))
                 #$os-drv
                 #:compressor '(#+(file-append gzip "/bin/gzip") "-9n")
                 #:creation-time (make-time time-utc 0 1)
                 #:transformations `((,root-directory -> ""))))))))
    (expression->derivation-in-linux-vm
     name build
     #:make-disk-image? #f
     #:single-file-output? #t
     #:references-graphs `((,graph ,os-drv)))))


;;;
;;; VM and disk images.
;;;

(define* (operating-system-uuid os #:optional (type 'dce))
  "Compute UUID object with a deterministic \"UUID\" for OS, of the given
TYPE (one of 'iso9660 or 'dce).  Return a UUID object."
  ;; Note: For this to be deterministic, we must not hash things that contains
  ;; (directly or indirectly) procedures, for example.  That rules out
  ;; anything that contains gexps, thunk or delayed record fields, etc.

  (define service-name
    (compose service-type-name service-kind))

  (define (file-system-digest fs)
    ;; Return a hashable digest that does not contain 'dependencies' since
    ;; this field can contain procedures.
    (let ((device (file-system-device fs)))
      (list (file-system-mount-point fs)
            (file-system-type fs)
            (cond ((file-system-label? device)
                   (file-system-label->string device))
                  ((uuid? device)
                   (uuid->string device))
                  ((string? device)
                   device)
                  (else #f))
            (file-system-options fs))))

  (if (eq? type 'iso9660)
      (let ((pad (compose (cut string-pad <> 2 #\0)
                          number->string))
            (h   (hash (map service-name (operating-system-services os))
                       3600)))
        (bytevector->uuid
         (string->iso9660-uuid
          (string-append "1970-01-01-"
                         (pad (hash (operating-system-host-name os) 24)) "-"
                         (pad (quotient h 60)) "-"
                         (pad (modulo h 60)) "-"
                         (pad (hash (map file-system-digest
                                         (operating-system-file-systems os))
                                    100))))
         'iso9660))
      (bytevector->uuid
       (uint-list->bytevector
        (list (hash file-system-type
                    (- (expt 2 32) 1))
              (hash (operating-system-host-name os)
                    (- (expt 2 32) 1))
              (hash (map service-name (operating-system-services os))
                    (- (expt 2 32) 1))
              (hash (map file-system-digest (operating-system-file-systems os))
                    (- (expt 2 32) 1)))
        (endianness little)
        4)
       type)))

(define* (system-disk-image os
                            #:key
                            (name "disk-image")
                            (file-system-type "ext4")
                            (disk-image-size (* 900 (expt 2 20)))
                            (volatile? #t))
  "Return the derivation of a disk image of DISK-IMAGE-SIZE bytes of the
system described by OS.  Said image can be copied on a USB stick as is.  When
VOLATILE? is true, the root file system is made volatile; this is useful
to USB sticks meant to be read-only."
  (define normalize-label
    ;; ISO labels are all-caps (case-insensitive), but since
    ;; 'find-partition-by-label' is case-sensitive, make it all-caps here.
    (if (string=? "iso9660" file-system-type)
        string-upcase
        identity))

  (define root-label
    ;; Volume name of the root file system.
    (normalize-label "GuixSD_image"))

  (define root-uuid
    ;; UUID of the root file system, computed in a deterministic fashion.
    ;; This is what we use to locate the root file system so it has to be
    ;; different from the user's own file system UUIDs.
    (operating-system-uuid os
                           (if (string=? file-system-type "iso9660")
                               'iso9660
                               'dce)))

  (define file-systems-to-keep
    (remove (lambda (fs)
              (string=? (file-system-mount-point fs) "/"))
            (operating-system-file-systems os)))

  (let* ((os (operating-system (inherit os)
               ;; Since this is meant to be used on real hardware, don't
               ;; install QEMU networking or anything like that.  Assume USB
               ;; mass storage devices (usb-storage.ko) are available.
               (initrd (lambda (file-systems . rest)
                         (apply (operating-system-initrd os)
                                file-systems
                                #:volatile-root? #t
                                rest)))

               (bootloader (if (string=? "iso9660" file-system-type)
                               (bootloader-configuration
                                 (inherit (operating-system-bootloader os))
                                 (bootloader grub-mkrescue-bootloader))
                               (operating-system-bootloader os)))

               ;; Force our own root file system.
               (file-systems (cons (file-system
                                     (mount-point "/")
                                     (device root-uuid)
                                     (type file-system-type))
                                   file-systems-to-keep))))
        (bootcfg (operating-system-bootcfg os)))
    (if (string=? "iso9660" file-system-type)
        (iso9660-image #:name name
                       #:file-system-label root-label
                       #:file-system-uuid root-uuid
                       #:os os
                       #:register-closures? #t
                       #:bootcfg-drv bootcfg
                       #:bootloader (bootloader-configuration-bootloader
                                     (operating-system-bootloader os))
                       #:inputs `(("system" ,os)
                                  ("bootcfg" ,bootcfg)))
        (qemu-image #:name name
                    #:os os
                    #:bootcfg-drv bootcfg
                    #:bootloader (bootloader-configuration-bootloader
                                  (operating-system-bootloader os))
                    #:disk-image-size disk-image-size
                    #:disk-image-format "raw"
                    #:file-system-type file-system-type
                    #:file-system-label root-label
                    #:file-system-uuid root-uuid
                    #:copy-inputs? #t
                    #:register-closures? #t
                    #:inputs `(("system" ,os)
                               ("bootcfg" ,bootcfg))))))

(define* (system-qemu-image os
                            #:key
                            (file-system-type "ext4")
                            (disk-image-size (* 900 (expt 2 20))))
  "Return the derivation of a freestanding QEMU image of DISK-IMAGE-SIZE bytes
of the GNU system as described by OS."
  (define file-systems-to-keep
    ;; Keep only file systems other than root and not normally bound to real
    ;; devices.
    (remove (lambda (fs)
              (let ((target (file-system-mount-point fs))
                    (source (file-system-device fs)))
                (or (string=? target "/")
                    (string-prefix? "/dev/" source))))
            (operating-system-file-systems os)))

  (define root-uuid
    ;; UUID of the root file system.
    (operating-system-uuid os
                           (if (string=? file-system-type "iso9660")
                               'iso9660
                               'dce)))


  (let* ((os (operating-system (inherit os)
               ;; Assume we have an initrd with the whole QEMU shebang.

               ;; Force our own root file system.  Refer to it by UUID so that
               ;; it works regardless of how the image is used ("qemu -hda",
               ;; Xen, etc.).
               (file-systems (cons (file-system
                                     (mount-point "/")
                                     (device root-uuid)
                                     (type file-system-type))
                                   file-systems-to-keep))))
         (bootcfg (operating-system-bootcfg os)))
    (qemu-image  #:os os
                 #:bootcfg-drv bootcfg
                 #:bootloader (bootloader-configuration-bootloader
                               (operating-system-bootloader os))
                 #:disk-image-size disk-image-size
                 #:file-system-type file-system-type
                 #:file-system-uuid root-uuid
                 #:inputs `(("system" ,os)
                            ("bootcfg" ,bootcfg))
                 #:copy-inputs? #t)))


;;;
;;; VMs that share file systems with the host.
;;;

(define (file-system->mount-tag fs)
  "Return a 9p mount tag for host file system FS."
  ;; QEMU mount tags must be ASCII, at most 31-byte long, cannot contain
  ;; slashes, and cannot start with '_'.  Compute an identifier that
  ;; corresponds to the rules.
  (string-append "TAG"
                 (string-drop (bytevector->base32-string
                               (sha1 (string->utf8 fs)))
                              4)))

(define (mapping->file-system mapping)
  "Return a 9p file system that realizes MAPPING."
  (match mapping
    (($ <file-system-mapping> source target writable?)
     (file-system
       (mount-point target)
       (device (file-system->mount-tag source))
       (type "9p")
       (flags (if writable? '() '(read-only)))
       (options "trans=virtio,cache=loose")
       (check? #f)
       (create-mount-point? #t)))))

(define* (virtualized-operating-system os mappings #:optional (full-boot? #f))
  "Return an operating system based on OS suitable for use in a virtualized
environment with the store shared with the host.  MAPPINGS is a list of
<file-system-mapping> to realize in the virtualized OS."
  (define user-file-systems
    ;; Remove file systems that conflict with those added below, or that are
    ;; normally bound to real devices.
    (remove (lambda (fs)
              (let ((target (file-system-mount-point fs))
                    (source (file-system-device fs)))
                (or (string=? target (%store-prefix))
                    (string=? target "/")
                    (and (string? source)
                         (string-prefix? "/dev/" source))

                    ;; Labels and UUIDs are necessarily invalid in the VM.
                    (and (file-system-mount? fs)
                         (or (file-system-label? source)
                             (uuid? source))))))
            (operating-system-file-systems os)))

  (define virtual-file-systems
    (cons (file-system
            (mount-point "/")
            (device "/dev/vda1")
            (type "ext4"))

          (append (map mapping->file-system mappings)
                  user-file-systems)))

  (operating-system (inherit os)

    ;; XXX: Until we run QEMU with UEFI support (with the OVMF firmware),
    ;; force the traditional i386/BIOS method.
    ;; See <https://bugs.gnu.org/28768>.
    (bootloader (bootloader-configuration
                  (bootloader grub-bootloader)
                  (target "/dev/vda")))

    (initrd (lambda (file-systems . rest)
              (apply (operating-system-initrd os)
                     file-systems
                     #:volatile-root? #t
                     rest)))

    ;; Disable swap.
    (swap-devices '())

    ;; XXX: When FULL-BOOT? is true, do not add a 9p mount for /gnu/store
    ;; since that would lead the bootloader config to look for the kernel and
    ;; initrd in it.
    (file-systems (if full-boot?
                      virtual-file-systems
                      (cons
                       (file-system
                         (inherit (mapping->file-system %store-mapping))
                         (needed-for-boot? #t))
                       virtual-file-systems)))))

(define* (system-qemu-image/shared-store
          os
          #:key
          full-boot?
          (disk-image-size (* (if full-boot? 500 30) (expt 2 20))))
  "Return a derivation that builds a QEMU image of OS that shares its store
with the host.

When FULL-BOOT? is true, return an image that does a complete boot sequence,
bootloaded included; thus, make a disk image that contains everything the
bootloader refers to: OS kernel, initrd, bootloader data, etc."
  (define root-uuid
    ;; Use a fixed UUID to improve determinism.
    (operating-system-uuid os 'dce))

  (define bootcfg
    (operating-system-bootcfg os))

  ;; XXX: When FULL-BOOT? is true, we end up creating an image that contains
  ;; BOOTCFG and all its dependencies, including the output of OS.
  ;; This is more than needed (we only need the kernel, initrd, GRUB for its
  ;; font, and the background image), but it's hard to filter that.
  (qemu-image #:os os
              #:bootcfg-drv bootcfg
              #:bootloader (bootloader-configuration-bootloader
                            (operating-system-bootloader os))
              #:disk-image-size disk-image-size
              #:file-system-uuid root-uuid
              #:inputs (if full-boot?
                           `(("bootcfg" ,bootcfg))
                           '())

              ;; XXX: Passing #t here is too slow, so let it off by default.
              #:register-closures? #f
              #:copy-inputs? full-boot?))

(define* (common-qemu-options image shared-fs)
  "Return the a string-value gexp with the common QEMU options to boot IMAGE,
with '-virtfs' options for the host file systems listed in SHARED-FS."

  (define (virtfs-option fs)
    #~(format #f "-virtfs local,path=~s,security_model=none,mount_tag=~s"
              #$fs #$(file-system->mount-tag fs)))

  #~(;; Only enable kvm if we see /dev/kvm exists.
     ;; This allows users without hardware virtualization to still use these
     ;; commands.
     #$@(if (file-exists? "/dev/kvm")
            '("-enable-kvm")
            '())

     "-no-reboot"
     "-net nic,model=virtio"
     "-object" "rng-random,filename=/dev/urandom,id=guixsd-vm-rng"
     "-device" "virtio-rng-pci,rng=guixsd-vm-rng"

     #$@(map virtfs-option shared-fs)
     "-vga std"
     (format #f "-drive file=~a,if=virtio,cache=writeback,werror=report,readonly"
             #$image)))

(define* (system-qemu-image/shared-store-script os
                                                #:key
                                                (qemu qemu)
                                                (graphic? #t)
                                                (memory-size 256)
                                                (mappings '())
                                                full-boot?
                                                (disk-image-size
                                                 (* (if full-boot? 500 70)
                                                    (expt 2 20)))
                                                (options '()))
  "Return a derivation that builds a script to run a virtual machine image of
OS that shares its store with the host.  The virtual machine runs with
MEMORY-SIZE MiB of memory.

MAPPINGS is a list of <file-system-mapping> specifying mapping of host file
systems into the guest.

When FULL-BOOT? is true, the returned script runs everything starting from the
bootloader; otherwise it directly starts the operating system kernel.  The
DISK-IMAGE-SIZE parameter specifies the size in bytes of the root disk image;
it is mostly useful when FULL-BOOT?  is true."
  (mlet* %store-monad ((os ->  (virtualized-operating-system os mappings full-boot?))
                       (image  (system-qemu-image/shared-store
                                os
                                #:full-boot? full-boot?
                                #:disk-image-size disk-image-size)))
    (define kernel-arguments
      #~(list #$@(if graphic? #~() #~("console=ttyS0"))
              #+@(operating-system-kernel-arguments os "/dev/vda1")))

    (define qemu-exec
      #~(list (string-append #$qemu "/bin/" #$(qemu-command (%current-system)))
              #$@(if full-boot?
                     #~()
                     #~("-kernel" #$(operating-system-kernel-file os)
                        "-initrd" #$(file-append os "/initrd")
                        (format #f "-append ~s"
                                (string-join #$kernel-arguments " "))))
              #$@(common-qemu-options image
                                      (map file-system-mapping-source
                                           (cons %store-mapping mappings)))
              "-m " (number->string #$memory-size)
              #$@options))

    (define builder
      #~(call-with-output-file #$output
          (lambda (port)
            (format port "#!~a~% exec ~a \"$@\"~%"
                    #$(file-append bash "/bin/sh")
                    (string-join #$qemu-exec " "))
            (chmod port #o555))))

    (gexp->derivation "run-vm.sh" builder)))


;;;
;;; High-level abstraction.
;;;

(define-record-type* <virtual-machine> %virtual-machine
  make-virtual-machine
  virtual-machine?
  (operating-system virtual-machine-operating-system) ;<operating-system>
  (qemu             virtual-machine-qemu              ;<package>
                    (default qemu))
  (graphic?         virtual-machine-graphic?      ;Boolean
                    (default #f))
  (memory-size      virtual-machine-memory-size   ;integer (MiB)
                    (default 256))
  (disk-image-size  virtual-machine-disk-image-size   ;integer (bytes)
                    (default 'guess))
  (port-forwardings virtual-machine-port-forwardings ;list of integer pairs
                    (default '())))

(define-syntax virtual-machine
  (syntax-rules ()
    "Declare a virtual machine running the specified OS, with the given
options."
    ((_ os)                                       ;shortcut
     (%virtual-machine (operating-system os)))
    ((_ fields ...)
     (%virtual-machine fields ...))))

(define (port-forwardings->qemu-options forwardings)
  "Return the QEMU option for the given port FORWARDINGS as a string, where
FORWARDINGS is a list of host-port/guest-port pairs."
  (string-join
   (map (match-lambda
          ((host-port . guest-port)
           (string-append "hostfwd=tcp::"
                          (number->string host-port)
                          "-:" (number->string guest-port))))
        forwardings)
   ","))

(define-gexp-compiler (virtual-machine-compiler (vm <virtual-machine>)
                                                system target)
  ;; XXX: SYSTEM and TARGET are ignored.
  (match vm
    (($ <virtual-machine> os qemu graphic? memory-size disk-image-size ())
     (system-qemu-image/shared-store-script os
                                            #:qemu qemu
                                            #:graphic? graphic?
                                            #:memory-size memory-size
                                            #:disk-image-size
                                            disk-image-size))
    (($ <virtual-machine> os qemu graphic? memory-size disk-image-size
                          forwardings)
     (let ((options
            `("-net" ,(string-append
                       "user,"
                       (port-forwardings->qemu-options forwardings)))))
       (system-qemu-image/shared-store-script os
                                              #:qemu qemu
                                              #:graphic? graphic?
                                              #:memory-size memory-size
                                              #:disk-image-size
                                              disk-image-size
                                              #:options options)))))

;;; vm.scm ends here
