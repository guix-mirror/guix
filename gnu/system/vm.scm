;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix store)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module ((gnu packages base) #:select (%final-inputs guile-final))
  #:use-module (gnu packages qemu)
  #:use-module (gnu packages parted)
  #:use-module (gnu packages grub)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages linux-initrd)
  #:use-module ((gnu packages make-bootstrap)
                #:select (%guile-static-stripped))
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (expression->derivation-in-linux-vm
            qemu-image))


;;; Commentary:
;;;
;;; Tools to evaluate build expressions within virtual machines.
;;;
;;; Code:

(define* (expression->derivation-in-linux-vm store name system exp inputs
                                             #:key
                                             (linux linux-libre)
                                             (initrd qemu-initrd)
                                             (qemu qemu-kvm/smb-shares)
                                             (env-vars '())
                                             (modules '())
                                             (guile-for-build
                                              (%guile-for-build))

                                             (make-disk-image? #f)
                                             (disk-image-size
                                              (* 100 (expt 2 20))))
  "Evaluate EXP in a QEMU virtual machine running LINUX with INITRD.  In the
virtual machine, EXP has access to all of INPUTS from the store; it should put
its output files in the `/xchg' directory, which is copied to the derivation's
output when the VM terminates.

When MAKE-DISK-IMAGE? is true, then create a QEMU disk image of
DISK-IMAGE-SIZE bytes and return it."
  (define input-alist
    (map (match-lambda
          ((input package)
           `(,input . ,(package-output store package "out" system)))
          ((input package sub-drv)
           `(,input . ,(package-output store package sub-drv system))))
         inputs))

  (define exp*
    ;; EXP, but with INPUTS available.
    `(let ((%build-inputs ',input-alist))
       ,exp))

  (define builder
    ;; Code that launches the VM that evaluates EXP.
    `(begin
       (use-modules (guix build utils))

       (let ((out     (assoc-ref %outputs "out"))
             (cu      (string-append (assoc-ref %build-inputs "coreutils")
                                     "/bin"))
             (qemu    (string-append (assoc-ref %build-inputs "qemu")
                                     "/bin/qemu-system-"
                                     (car (string-split ,system #\-))))
             (img     (string-append (assoc-ref %build-inputs "qemu")
                                     "/bin/qemu-img"))
             (linux   (string-append (assoc-ref %build-inputs "linux")
                                     "/bzImage"))
             (initrd  (string-append (assoc-ref %build-inputs "initrd")
                                     "/initrd"))
             (builder (assoc-ref %build-inputs "builder")))

         ;; XXX: QEMU uses "rm -rf" when it's done to remove the temporary SMB
         ;; directory, so it really needs `rm' in $PATH.
         (setenv "PATH" cu)

         ,(if make-disk-image?
              `(zero? (system* img "create" "image.qcow2"
                               ,(number->string disk-image-size)))
              '(begin))

         (mkdir "xchg")
         (and (zero?
               (system* qemu "-nographic" "-no-reboot"
                        "-net" "nic,model=e1000"
                        "-net" (string-append "user,smb=" (getcwd))
                        "-kernel" linux
                        "-initrd" initrd
                        "-append" (string-append "console=ttyS0 --load="
                                                 builder)
                        ,@(if make-disk-image?
                              '("-hda" "image.qcow2")
                              '())))
              ,(if make-disk-image?
                   '(copy-file "image.qcow2"      ; XXX: who mkdir'd OUT?
                               out)
                   '(begin
                      (mkdir out)
                      (copy-recursively "xchg" out)))))))

  (let ((user-builder (add-text-to-store store "builder-in-linux-vm"
                                         (object->string exp*)
                                         '()))
        (->drv        (cut package-derivation store <> system))
        (coreutils    (car (assoc-ref %final-inputs "coreutils"))))
    (build-expression->derivation store name system builder
                                  `(("qemu" ,(->drv qemu))
                                    ("linux" ,(->drv linux))
                                    ("initrd" ,(->drv initrd))
                                    ("coreutils" ,(->drv coreutils))
                                    ("builder" ,user-builder)
                                    ,@(map (match-lambda
                                            ((name package sub-drv ...)
                                             `(,name ,(->drv package)
                                                     ,@sub-drv)))
                                           inputs))
                                  #:env-vars env-vars
                                  #:modules `((guix build utils)
                                              ,@modules)
                                  #:guile-for-build guile-for-build)))

(define* (qemu-image store #:key
                     (name "qemu-image")
                     (system (%current-system))
                     (disk-image-size (* 100 (expt 2 20)))
                     (linux linux-libre)
                     (initrd qemu-initrd)
                     (inputs '()))
  "Return a bootable, stand-alone QEMU image."
  (expression->derivation-in-linux-vm
   store "qemu-image" system
   `(let ((parted  (string-append (assoc-ref %build-inputs "parted")
                                  "/sbin/parted"))
          (mkfs    (string-append (assoc-ref %build-inputs "e2fsprogs")
                                  "/sbin/mkfs.ext3"))
          (grub    (string-append (assoc-ref %build-inputs "grub")
                                  "/sbin/grub-install"))
          (umount  (string-append (assoc-ref %build-inputs "util-linux")
                                  "/bin/umount")) ; XXX: add to Guile
          (initrd  (string-append (assoc-ref %build-inputs "initrd")
                                  "/initrd"))
          (linux   (string-append (assoc-ref %build-inputs "linux")
                                  "/bzImage"))
          (makedev (lambda (major minor)
                     (+ (* major 256) minor))))

      ;; GRUB is full of shell scripts.
      (setenv "PATH"
              (string-append (dirname grub) ":"
                             (assoc-ref %build-inputs "coreutils") "/bin:"
                             (assoc-ref %build-inputs "findutils") "/bin:"
                             (assoc-ref %build-inputs "sed") "/bin:"
                             (assoc-ref %build-inputs "grep") "/bin:"
                             (assoc-ref %build-inputs "gawk") "/bin"))

      (display "creating partition table...\n")
      (mknod "/dev/vda" 'block-special #o644 (makedev 8 0))
      (and (zero? (system* parted "/dev/vda" "mklabel" "msdos"
                           "mkpart" "primary" "ext2" "1MiB"
                           ,(format #f "~aB"
                                    (- disk-image-size
                                       (* 5 (expt 2 20))))))
           (begin
             (display "creating ext3 partition...\n")
             (mknod "/dev/vda1" 'block-special #o644 (makedev 8 1))
             (and (zero? (system* mkfs "-F" "/dev/vda1"))
                  (begin
                    (display "mounting partition...\n")
                    (mkdir "/fs")
                    (mount "/dev/vda1" "/fs" "ext3")
                    (mkdir "/fs/boot")
                    (mkdir "/fs/boot/grub")
                    (copy-file linux "/fs/boot/bzImage")
                    (copy-file initrd "/fs/boot/initrd")
                    (call-with-output-file "/fs/boot/grub/grub.cfg"
                      (lambda (p)
                        (display "
set timeout=10
search.file /boot/bzImage

menuentry \"Boot-to-Guile! Happy Birthday Guile 2.0! (Guile, Guix & co.)\" {
  linux /boot/bzImage --repl
  initrd /boot/initrd
}" p)))
                    (and (zero?
                          (system* grub "--no-floppy"
                                   "--boot-directory" "/fs/boot"
                                   "/dev/vda"))
                         (zero?
                          (system* umount "/fs"))))))))
   `(("parted" ,parted)
     ("grub" ,grub)
     ("e2fsprogs" ,e2fsprogs)
     ("linux" ,linux-libre)
     ("initrd" ,qemu-initrd)

     ;; For shell scripts.
     ("sed" ,(car (assoc-ref %final-inputs "sed")))
     ("grep" ,(car (assoc-ref %final-inputs "grep")))
     ("coreutils" ,(car (assoc-ref %final-inputs "coreutils")))
     ("findutils" ,(car (assoc-ref %final-inputs "findutils")))
     ("gawk" ,(car (assoc-ref %final-inputs "gawk")))
     ("util-linux" ,util-linux))
   #:make-disk-image? #t
   #:disk-image-size disk-image-size))


;;;
;;; Guile 2.0 potluck examples.
;;;

(define (example1)
  (let ((store #f))
    (dynamic-wind
      (lambda ()
        (set! store (open-connection)))
      (lambda ()
        (parameterize ((%guile-for-build (package-derivation store guile-final)))
          (expression->derivation-in-linux-vm
           store "vm-test" (%current-system)
           '(begin
              (display "hello from boot!\n")
              (call-with-output-file "/xchg/hello"
                (lambda (p)
                  (display "world" p))))
           '())))
      (lambda ()
        (close-connection store)))))

(define (example2)
  (let ((store #f))
    (dynamic-wind
      (lambda ()
        (set! store (open-connection)))
      (lambda ()
        (parameterize ((%guile-for-build (package-derivation store guile-final)))
          (qemu-image store #:disk-image-size (* 30 (expt 2 20)))))
      (lambda ()
        (close-connection store)))))

;;; vm.scm ends here
