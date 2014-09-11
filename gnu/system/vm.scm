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

(define-module (gnu system vm)
  #:use-module (guix config)
  #:use-module (guix store)
  #:use-module (guix gexp)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (guix monads)
  #:use-module ((gnu build vm)
                #:select (qemu-command))
  #:use-module (gnu packages base)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages less)
  #:use-module (gnu packages qemu)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages zile)
  #:use-module (gnu packages grub)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages package-management)
  #:use-module ((gnu packages make-bootstrap)
                #:select (%guile-static-stripped))
  #:use-module (gnu packages admin)

  #:use-module (gnu system shadow)
  #:use-module (gnu system linux)
  #:use-module (gnu system linux-initrd)
  #:use-module (gnu system grub)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system)
  #:use-module (gnu services)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)

  #:export (expression->derivation-in-linux-vm
            qemu-image
            system-qemu-image
            system-qemu-image/shared-store
            system-qemu-image/shared-store-script
            system-disk-image))


;;; Commentary:
;;;
;;; Tools to evaluate build expressions within virtual machines.
;;;
;;; Code:

(define* (input->name+output tuple #:key (system (%current-system)))
  "Return as a monadic value a name/file-name pair corresponding to TUPLE, an
input tuple.  The output file name is when building for SYSTEM."
  (with-monad %store-monad
    (match tuple
      ((input (? package? package))
       (mlet %store-monad ((out (package-file package #:system system)))
         (return `(,input . ,out))))
      ((input (? package? package) sub-drv)
       (mlet %store-monad ((out (package-file package
                                              #:output sub-drv
                                              #:system system)))
         (return `(,input . ,out))))
      ((input (? derivation? drv))
       (return `(,input . ,(derivation->output-path drv))))
      ((input (? derivation? drv) sub-drv)
       (return `(,input . ,(derivation->output-path drv sub-drv))))
      ((input (and (? string?) (? store-path?) file))
       (return `(,input . ,file))))))

(define %linux-vm-file-systems
  ;; File systems mounted for 'derivation-in-linux-vm'.  The store and /xchg
  ;; directory are shared with the host over 9p.
  (list (file-system
          (mount-point (%store-prefix))
          (device "store")
          (type "9p")
          (needed-for-boot? #t)
          (options "trans=virtio")
          (check? #f))
        (file-system
          (mount-point "/xchg")
          (device "xchg")
          (type "9p")
          (needed-for-boot? #t)
          (options "trans=virtio")
          (check? #f))))

(define* (expression->derivation-in-linux-vm name exp
                                             #:key
                                             (system (%current-system))
                                             (linux linux-libre)
                                             initrd
                                             (qemu qemu-headless)
                                             (env-vars '())
                                             (modules
                                              '((gnu build vm)
                                                (gnu build install)
                                                (gnu build linux-boot)
                                                (gnu build file-systems)
                                                (guix build utils)
                                                (guix build store-copy)))
                                             (guile-for-build
                                              (%guile-for-build))

                                             (make-disk-image? #f)
                                             (references-graphs #f)
                                             (memory-size 256)
                                             (disk-image-format "qcow2")
                                             (disk-image-size
                                              (* 100 (expt 2 20))))
  "Evaluate EXP in a QEMU virtual machine running LINUX with INITRD (a
derivation).  In the virtual machine, EXP has access to all its inputs from the
store; it should put its output files in the `/xchg' directory, which is
copied to the derivation's output when the VM terminates.  The virtual machine
runs with MEMORY-SIZE MiB of memory.

When MAKE-DISK-IMAGE? is true, then create a QEMU disk image of type
DISK-IMAGE-FORMAT (e.g., 'qcow2' or 'raw'), of DISK-IMAGE-SIZE bytes and
return it.

MODULES is the set of modules imported in the execution environment of EXP.

When REFERENCES-GRAPHS is true, it must be a list of file name/store path
pairs, as for `derivation'.  The files containing the reference graphs are
made available under the /xchg CIFS share."
  (mlet* %store-monad
      ((module-dir   (imported-modules modules))
       (compiled     (compiled-modules modules))
       (user-builder (gexp->file "builder-in-linux-vm" exp))
       (loader       (gexp->file "linux-vm-loader"
                                 #~(begin
                                     (set! %load-path
                                           (cons #$module-dir %load-path))
                                     (set! %load-compiled-path
                                           (cons #$compiled
                                                 %load-compiled-path))
                                     (primitive-load #$user-builder))))
       (coreutils -> (canonical-package coreutils))
       (initrd       (if initrd                   ; use the default initrd?
                         (return initrd)
                         (base-initrd %linux-vm-file-systems
                                      #:virtio? #t
                                      #:qemu-networking? #t
                                      #:guile-modules-in-chroot? #t))))

    (define builder
      ;; Code that launches the VM that evaluates EXP.
      #~(begin
          (use-modules (guix build utils)
                       (gnu build vm))

          (let ((inputs  '#$(list qemu coreutils))
                (linux   (string-append #$linux "/bzImage"))
                (initrd  (string-append #$initrd "/initrd"))
                (loader  #$loader)
                (graphs  '#$(match references-graphs
                              (((graph-files . _) ...) graph-files)
                              (_ #f))))

            (set-path-environment-variable "PATH" '("bin") inputs)

            (load-in-linux-vm loader
                              #:output #$output
                              #:linux linux #:initrd initrd
                              #:memory-size #$memory-size
                              #:make-disk-image? #$make-disk-image?
                              #:disk-image-format #$disk-image-format
                              #:disk-image-size #$disk-image-size
                              #:references-graphs graphs))))

    (gexp->derivation name builder
                      ;; TODO: Require the "kvm" feature.
                      #:system system
                      #:env-vars env-vars
                      #:modules modules
                      #:guile-for-build guile-for-build
                      #:references-graphs references-graphs)))

(define* (qemu-image #:key
                     (name "qemu-image")
                     (system (%current-system))
                     (qemu qemu-headless)
                     (disk-image-size (* 100 (expt 2 20)))
                     (disk-image-format "qcow2")
                     (file-system-type "ext4")
                     file-system-label
                     os-derivation
                     grub-configuration
                     (register-closures? #t)
                     (inputs '())
                     copy-inputs?)
  "Return a bootable, stand-alone QEMU image of type DISK-IMAGE-FORMAT (e.g.,
'qcow2' or 'raw'), with a root partition of type FILE-SYSTEM-TYPE.
Optionally, FILE-SYSTEM-LABEL can be specified as the volume name for the root
partition.  The returned image is a full disk image that runs OS-DERIVATION,
with a GRUB installation that uses GRUB-CONFIGURATION as its configuration
file (GRUB-CONFIGURATION must be the name of a file in the VM.)

INPUTS is a list of inputs (as for packages).  When COPY-INPUTS? is true, copy
all of INPUTS into the image being built.  When REGISTER-CLOSURES? is true,
register INPUTS in the store database of the image so that Guix can be used in
the image."
  (expression->derivation-in-linux-vm
   name
   #~(begin
       (use-modules (gnu build vm)
                    (guix build utils))

       (let ((inputs
              '#$(append (list qemu parted grub e2fsprogs util-linux)
                         (map canonical-package
                              (list sed grep coreutils findutils gawk))
                         (if register-closures? (list guix) '())))

             ;; This variable is unused but allows us to add INPUTS-TO-COPY
             ;; as inputs.
             (to-register
              '#$(map (match-lambda
                       ((name thing) thing)
                       ((name thing output) `(,thing ,output)))
                      inputs)))

         (set-path-environment-variable "PATH" '("bin" "sbin") inputs)

         (let ((graphs '#$(match inputs
                            (((names . _) ...)
                             names))))
           (initialize-hard-disk "/dev/vda"
                                 #:system-directory #$os-derivation
                                 #:grub.cfg #$grub-configuration
                                 #:closures graphs
                                 #:copy-closures? #$copy-inputs?
                                 #:register-closures? #$register-closures?
                                 #:disk-image-size #$disk-image-size
                                 #:file-system-type #$file-system-type
                                 #:file-system-label #$file-system-label)
           (reboot))))
   #:system system
   #:make-disk-image? #t
   #:disk-image-size disk-image-size
   #:disk-image-format disk-image-format
   #:references-graphs inputs))


;;;
;;; VM and disk images.
;;;

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
  (define root-label
    ;; Volume name of the root file system.  Since we don't know which device
    ;; will hold it, we use the volume name to find it (using the UUID would
    ;; be even better, but somewhat less convenient.)
    "gnu-disk-image")

  (define file-systems-to-keep
    (remove (lambda (fs)
              (string=? (file-system-mount-point fs) "/"))
            (operating-system-file-systems os)))

  (let ((os (operating-system (inherit os)
              ;; Since this is meant to be used on real hardware, don't
              ;; install QEMU networking or anything like that, but make sure
              ;; USB mass storage devices are available.
              (initrd (cut base-initrd <>
                           #:volatile-root? #t
                           #:extra-modules '("usb-storage.ko")))

              ;; Force our own root file system.
              (file-systems (cons (file-system
                                    (mount-point "/")
                                    (device root-label)
                                    (title 'label)
                                    (type file-system-type))
                                  file-systems-to-keep)))))

    (mlet* %store-monad ((os-drv   (operating-system-derivation os))
                         (grub.cfg (operating-system-grub.cfg os)))
      (qemu-image #:name name
                  #:os-derivation os-drv
                  #:grub-configuration grub.cfg
                  #:disk-image-size disk-image-size
                  #:disk-image-format "raw"
                  #:file-system-type file-system-type
                  #:file-system-label root-label
                  #:copy-inputs? #t
                  #:register-closures? #t
                  #:inputs `(("system" ,os-drv)
                             ("grub.cfg" ,grub.cfg))))))

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

  (let ((os (operating-system (inherit os)
              ;; Use an initrd with the whole QEMU shebang.
              (initrd (cut base-initrd <>
                           #:virtio? #t
                           #:qemu-networking? #t))

              ;; Force our own root file system.
              (file-systems (cons (file-system
                                    (mount-point "/")
                                    (device "/dev/sda1")
                                    (type file-system-type))
                                  file-systems-to-keep)))))
    (mlet* %store-monad
        ((os-drv      (operating-system-derivation os))
         (grub.cfg    (operating-system-grub.cfg os)))
      (qemu-image  #:os-derivation os-drv
                   #:grub-configuration grub.cfg
                   #:disk-image-size disk-image-size
                   #:file-system-type file-system-type
                   #:inputs `(("system" ,os-drv)
                              ("grub.cfg" ,grub.cfg))
                   #:copy-inputs? #t))))

(define (virtualized-operating-system os)
  "Return an operating system based on OS suitable for use in a virtualized
environment with the store shared with the host."
  (operating-system (inherit os)
    (initrd (cut base-initrd <>
                 #:volatile-root? #t
                 #:virtio? #t
                 #:qemu-networking? #t))
    (file-systems (cons* (file-system
                           (mount-point "/")
                           (device "/dev/vda1")
                           (type "ext4"))
                         (file-system
                           (mount-point (%store-prefix))
                           (device "store")
                           (type "9p")
                           (needed-for-boot? #t)
                           (options "trans=virtio")
                           (check? #f))

                         ;; Remove file systems that conflict with those
                         ;; above, or that are normally bound to real devices.
                         (remove (lambda (fs)
                                   (let ((target (file-system-mount-point fs))
                                         (source (file-system-device fs)))
                                     (or (string=? target (%store-prefix))
                                         (string=? target "/")
                                         (string-prefix? "/dev/" source))))
                                 (operating-system-file-systems os))))))

(define* (system-qemu-image/shared-store
          os
          #:key (disk-image-size (* 15 (expt 2 20))))
  "Return a derivation that builds a QEMU image of OS that shares its store
with the host."
  (mlet* %store-monad
      ((os-drv      (operating-system-derivation os))
       (grub.cfg    (operating-system-grub.cfg os)))
    (qemu-image #:os-derivation os-drv
                #:grub-configuration grub.cfg
                #:disk-image-size disk-image-size
                #:inputs `(("system" ,os-drv))

                ;; XXX: Passing #t here is too slow, so let it off by default.
                #:register-closures? #f
                #:copy-inputs? #f)))

(define* (system-qemu-image/shared-store-script
          os
          #:key
          (qemu qemu)
          (graphic? #t))
  "Return a derivation that builds a script to run a virtual machine image of
OS that shares its store with the host."
  (mlet* %store-monad
      ((os ->  (virtualized-operating-system os))
       (os-drv (operating-system-derivation os))
       (image  (system-qemu-image/shared-store os)))
    (define builder
      #~(call-with-output-file #$output
          (lambda (port)
            (display
             (string-append "#!" #$bash "/bin/sh
exec " #$qemu "/bin/" #$(qemu-command (%current-system))
" -enable-kvm -no-reboot -net nic,model=virtio \
  -virtfs local,path=" #$(%store-prefix) ",security_model=none,mount_tag=store \
  -net user \
  -kernel " #$(operating-system-kernel os) "/bzImage \
  -initrd " #$os-drv "/initrd \
-append \"" #$(if graphic? "" "console=ttyS0 ")
  "--system=" #$os-drv " --load=" #$os-drv "/boot --root=/dev/vda1\" \
  -serial stdio \
  -drive file=" #$image
  ",if=virtio,cache=writeback,werror=report,readonly \
  -m 256
\n")
             port)
            (chmod port #o555))))

    (gexp->derivation "run-vm.sh" builder)))

;;; vm.scm ends here
