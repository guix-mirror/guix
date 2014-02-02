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
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (guix monads)
  #:use-module ((gnu packages base)
                #:select (%final-inputs
                          guile-final gcc-final glibc-final
                          ld-wrapper binutils-final
                          coreutils findutils grep sed tzdata))
  #:use-module (gnu packages guile)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages less)
  #:use-module (gnu packages qemu)
  #:use-module (gnu packages parted)
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
  #:use-module (gnu system dmd)
  #:use-module (gnu system)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)

  #:export (expression->derivation-in-linux-vm
            qemu-image
            system-qemu-image
            system-qemu-image/shared-store
            system-qemu-image/shared-store-script))


;;; Commentary:
;;;
;;; Tools to evaluate build expressions within virtual machines.
;;;
;;; Code:

(define* (expression->derivation-in-linux-vm name exp
                                             #:key
                                             (system (%current-system))
                                             (inputs '())
                                             (linux linux-libre)
                                             initrd
                                             (qemu qemu/smb-shares)
                                             (env-vars '())
                                             (modules '())
                                             (guile-for-build
                                              (%guile-for-build))

                                             (make-disk-image? #f)
                                             (references-graphs #f)
                                             (disk-image-size
                                              (* 100 (expt 2 20))))
  "Evaluate EXP in a QEMU virtual machine running LINUX with INITRD (a
derivation).  In the virtual machine, EXP has access to all of INPUTS from the
store; it should put its output files in the `/xchg' directory, which is
copied to the derivation's output when the VM terminates.

When MAKE-DISK-IMAGE? is true, then create a QEMU disk image of
DISK-IMAGE-SIZE bytes and return it.

When REFERENCES-GRAPHS is true, it must be a list of file name/store path
pairs, as for `derivation'.  The files containing the reference graphs are
made available under the /xchg CIFS share."
  ;; FIXME: Allow use of macros from other modules, as done in
  ;; `build-expression->derivation'.

  (define input-alist
    (with-monad %store-monad
      (map (match-lambda
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
             (return `(,input . ,file))))
           inputs)))

  (define builder
    ;; Code that launches the VM that evaluates EXP.
    `(let ()
       (use-modules (guix build utils)
                    (srfi srfi-1)
                    (ice-9 rdelim))

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
              `(zero? (system* img "create" "-f" "qcow2" "image.qcow2"
                               ,(number->string disk-image-size)))
              '(begin))

         (mkdir "xchg")

         ;; Copy the reference-graph files under xchg/ so EXP can access it.
         (begin
           ,@(match references-graphs
               (((graph-files . _) ...)
                (map (lambda (file)
                       `(copy-file ,file
                                   ,(string-append "xchg/" file)))
                     graph-files))
               (#f '())))

         (and (zero?
               (system* qemu "-enable-kvm" "-nographic" "-no-reboot"
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

  (mlet* %store-monad
      ((input-alist  (sequence %store-monad input-alist))
       (exp* ->      `(let ((%build-inputs ',input-alist))
                        ,exp))
       (user-builder (text-file "builder-in-linux-vm"
                                (object->string exp*)))
       (coreutils -> (car (assoc-ref %final-inputs "coreutils")))
       (initrd       (if initrd                   ; use the default initrd?
                         (return initrd)
                         (qemu-initrd #:guile-modules-in-chroot? #t)))
       (inputs       (lower-inputs `(("qemu" ,qemu)
                                     ("linux" ,linux)
                                     ("initrd" ,initrd)
                                     ("coreutils" ,coreutils)
                                     ("builder" ,user-builder)
                                     ,@inputs))))
    (derivation-expression name builder
                           ;; TODO: Require the "kvm" feature.
                           #:system system
                           #:inputs inputs
                           #:env-vars env-vars
                           #:modules (delete-duplicates
                                      `((guix build utils)
                                        ,@modules))
                           #:guile-for-build guile-for-build
                           #:references-graphs references-graphs)))

(define* (qemu-image #:key
                     (name "qemu-image")
                     (system (%current-system))
                     (disk-image-size (* 100 (expt 2 20)))
                     grub-configuration
                     (initialize-store? #f)
                     (populate #f)
                     (inputs '())
                     (inputs-to-copy '()))
  "Return a bootable, stand-alone QEMU image.  The returned image is a full
disk image, with a GRUB installation that uses GRUB-CONFIGURATION as its
configuration file (GRUB-CONFIGURATION must be the name of a file in the VM.)

INPUTS-TO-COPY is a list of inputs (as for packages) whose closure is copied
into the image being built.  When INITIALIZE-STORE? is true, initialize the
store database in the image so that Guix can be used in the image.

POPULATE is a list of directives stating directories or symlinks to be created
in the disk image partition.  It is evaluated once the image has been
populated with INPUTS-TO-COPY.  It can be used to provide additional files,
such as /etc files."
  (define (input->name+derivation tuple)
    (with-monad %store-monad
      (match tuple
        ((name (? package? package))
         (mlet %store-monad ((drv (package->derivation package system)))
           (return `(,name . ,(derivation->output-path drv)))))
        ((name (? package? package) sub-drv)
         (mlet %store-monad ((drv (package->derivation package system)))
           (return `(,name . ,(derivation->output-path drv sub-drv)))))
        ((name (? derivation? drv))
         (return `(,name . ,(derivation->output-path drv))))
        ((name (? derivation? drv) sub-drv)
         (return `(,name . ,(derivation->output-path drv sub-drv))))
        ((input (and (? string?) (? store-path?) file))
         (return `(,input . ,file))))))

  (mlet %store-monad
      ((graph (sequence %store-monad
                        (map input->name+derivation inputs-to-copy))))
   (expression->derivation-in-linux-vm
    "qemu-image"
    `(let ()
       (use-modules (ice-9 rdelim)
                    (srfi srfi-1)
                    (guix build utils)
                    (guix build linux-initrd))

       (let ((parted  (string-append (assoc-ref %build-inputs "parted")
                                     "/sbin/parted"))
             (mkfs    (string-append (assoc-ref %build-inputs "e2fsprogs")
                                     "/sbin/mkfs.ext3"))
             (grub    (string-append (assoc-ref %build-inputs "grub")
                                     "/sbin/grub-install"))
             (umount  (string-append (assoc-ref %build-inputs "util-linux")
                                     "/bin/umount")) ; XXX: add to Guile
             (grub.cfg ,grub-configuration))

         (define (read-reference-graph port)
           ;; Return a list of store paths from the reference graph at PORT.
           ;; The data at PORT is the format produced by #:references-graphs.
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

         (define (things-to-copy)
           ;; Return the list of store files to copy to the image.
           (define (graph-from-file file)
             (call-with-input-file file
               read-reference-graph))

           ,(match inputs-to-copy
              (((graph-files . _) ...)
               `(let* ((graph-files ',(map (cut string-append "/xchg/" <>)
                                           graph-files))
                       (paths       (append-map graph-from-file graph-files)))
                  (delete-duplicates paths)))
              (#f ''())))

         ;; GRUB is full of shell scripts.
         (setenv "PATH"
                 (string-append (dirname grub) ":"
                                (assoc-ref %build-inputs "coreutils") "/bin:"
                                (assoc-ref %build-inputs "findutils") "/bin:"
                                (assoc-ref %build-inputs "sed") "/bin:"
                                (assoc-ref %build-inputs "grep") "/bin:"
                                (assoc-ref %build-inputs "gawk") "/bin"))

         (display "creating partition table...\n")
         (and (zero? (system* parted "/dev/sda" "mklabel" "msdos"
                              "mkpart" "primary" "ext2" "1MiB"
                              ,(format #f "~aB"
                                       (- disk-image-size
                                          (* 5 (expt 2 20))))))
              (begin
                (display "creating ext3 partition...\n")
                (and (zero? (system* mkfs "-F" "/dev/sda1"))
                     (let ((store (string-append "/fs" ,%store-directory)))
                       (display "mounting partition...\n")
                       (mkdir "/fs")
                       (mount "/dev/sda1" "/fs" "ext3")
                       (mkdir-p "/fs/boot/grub")
                       (symlink grub.cfg "/fs/boot/grub/grub.cfg")

                       ;; Populate the image's store.
                       (mkdir-p store)
                       (chmod store #o1775)
                       (for-each (lambda (thing)
                                   (copy-recursively thing
                                                     (string-append "/fs"
                                                                    thing)))
                                 (things-to-copy))

                       ;; Populate /dev.
                       (make-essential-device-nodes #:root "/fs")

                       ;; Optionally, register the inputs in the image's store.
                       (let* ((guix     (assoc-ref %build-inputs "guix"))
                              (register (and guix
                                             (string-append guix
                                                            "/sbin/guix-register"))))
                         ,@(if initialize-store?
                               (match inputs-to-copy
                                 (((graph-files . _) ...)
                                  (map (lambda (closure)
                                         `(system* register "--prefix" "/fs"
                                                   ,(string-append "/xchg/"
                                                                   closure)))
                                       graph-files)))
                               '(#f)))

                       ;; Evaluate the POPULATE directives.
                       ,@(let loop ((directives populate)
                                    (statements '()))
                           (match directives
                             (()
                              (reverse statements))
                             ((('directory name) rest ...)
                              (loop rest
                                    (cons `(mkdir-p ,(string-append "/fs" name))
                                          statements)))
                             ((('directory name uid gid) rest ...)
                              (let ((dir (string-append "/fs" name)))
                                (loop rest
                                      (cons* `(chown ,dir ,uid ,gid)
                                             `(mkdir-p ,dir)
                                             statements))))
                             (((new '-> old) rest ...)
                              (loop rest
                                    (cons `(symlink ,old
                                                    ,(string-append "/fs" new))
                                          statements)))))

                       (and=> (assoc-ref %build-inputs "populate")
                              (lambda (populate)
                                (chdir "/fs")
                                (primitive-load populate)
                                (chdir "/")))

                       (display "clearing file timestamps...\n")
                       (for-each (lambda (file)
                                   (let ((s (lstat file)))
                                     ;; XXX: Guile uses libc's 'utime' function
                                     ;; (not 'futime'), so the timestamp of
                                     ;; symlinks cannot be changed, and there
                                     ;; are symlinks here pointing to
                                     ;; /nix/store, which is the host,
                                     ;; read-only store.
                                     (unless (eq? (stat:type s) 'symlink)
                                       (utime file 0 0 0 0))))
                                 (find-files "/fs" ".*"))

                       (and (zero?
                             (system* grub "--no-floppy"
                                      "--boot-directory" "/fs/boot"
                                      "/dev/sda"))
                            (zero? (system* umount "/fs"))
                            (reboot))))))))
    #:system system
    #:inputs `(("parted" ,parted)
               ("grub" ,grub)
               ("e2fsprogs" ,e2fsprogs)

               ;; For shell scripts.
               ("sed" ,(car (assoc-ref %final-inputs "sed")))
               ("grep" ,(car (assoc-ref %final-inputs "grep")))
               ("coreutils" ,(car (assoc-ref %final-inputs "coreutils")))
               ("findutils" ,(car (assoc-ref %final-inputs "findutils")))
               ("gawk" ,(car (assoc-ref %final-inputs "gawk")))
               ("util-linux" ,util-linux)

               ,@(if initialize-store?
                     `(("guix" ,guix))
                     '())

               ,@inputs-to-copy)
    #:make-disk-image? #t
    #:disk-image-size disk-image-size
    #:references-graphs graph
    #:modules '((guix build utils)
                (guix build linux-initrd)))))


;;;
;;; Stand-alone VM image.
;;;

(define %demo-operating-system
  (operating-system
   (host-name "gnu")
   (timezone "Europe/Paris")
   (locale "en_US.UTF-8")
   (users (list (user-account
                 (name "guest")
                 (password "")
                 (uid 1000) (gid 100)
                 (comment "Guest of GNU")
                 (home-directory "/home/guest"))))
   (packages (list coreutils
                   bash
                   guile-2.0
                   dmd
                   gcc-final
                   ld-wrapper                    ; must come before BINUTILS
                   binutils-final
                   glibc-final
                   inetutils
                   findutils
                   grep
                   sed
                   procps
                   psmisc
                   zile
                   less
                   tzdata
                   guix))))

(define (operating-system-build-gid os)
  "Return as a monadic value the group id for build users of OS, or #f."
  (anym %store-monad
        (lambda (service)
          (and (equal? '(guix-daemon)
                       (service-provision service))
               (match (service-user-groups service)
                 ((group)
                  (user-group-id group)))))
        (operating-system-services os)))

(define (operating-system-default-contents os)
  "Return a list of directives suitable for 'system-qemu-image' describing the
basic contents of the root file system of OS."
  (define (user-directories user)
    (let ((home (user-account-home-directory user))
          ;; XXX: Deal with automatically allocated ids.
          (uid  (or (user-account-uid user) 0))
          (gid  (or (user-account-gid user) 0))
          (root (string-append "/var/nix/profiles/per-user/"
                               (user-account-name user))))
      `((directory ,root ,uid ,gid)
        (directory ,home ,uid ,gid))))

  (mlet* %store-monad ((os-drv    (operating-system-derivation os))
                       (os-dir -> (derivation->output-path os-drv))
                       (build-gid (operating-system-build-gid os))
                       (profile   (operating-system-profile-directory os)))
    (return `((directory "/nix/store" 0 ,(or build-gid 0))
              (directory "/etc")
              (directory "/var/log")                     ; for dmd
              (directory "/var/run/nscd")
              (directory "/var/nix/gcroots")
              ("/var/nix/gcroots/system" -> ,os-dir)
              (directory "/run")
              ("/run/current-system" -> ,profile)
              (directory "/bin")
              ("/bin/sh" -> "/run/current-system/bin/bash")
              (directory "/tmp")
              (directory "/var/nix/profiles/per-user/root" 0 0)

              ,@(append-map user-directories
                            (operating-system-users os))))))

(define* (system-qemu-image #:optional (os %demo-operating-system)
                            #:key (disk-image-size (* 900 (expt 2 20))))
  "Return the derivation of a QEMU image of DISK-IMAGE-SIZE bytes of the GNU
system as described by OS."
  (mlet* %store-monad
      ((os-drv      (operating-system-derivation os))
       (os-dir   -> (derivation->output-path os-drv))
       (grub.cfg -> (string-append os-dir "/grub.cfg"))
       (populate    (operating-system-default-contents os)))
    (qemu-image  #:grub-configuration grub.cfg
                 #:populate populate
                 #:disk-image-size disk-image-size
                 #:initialize-store? #t
                 #:inputs-to-copy `(("system" ,os-drv)))))

(define* (system-qemu-image/shared-store
          #:optional (os %demo-operating-system)
          #:key (disk-image-size (* 15 (expt 2 20))))
  "Return a derivation that builds a QEMU image of OS that shares its store
with the host."
  (mlet* %store-monad
      ((os-drv      (operating-system-derivation os))
       (os-dir   -> (derivation->output-path os-drv))
       (grub.cfg -> (string-append os-dir "/grub.cfg"))
       (populate    (operating-system-default-contents os)))
    ;; TODO: Initialize the database so Guix can be used in the guest.
    (qemu-image #:grub-configuration grub.cfg
                #:populate populate
                #:disk-image-size disk-image-size)))

(define* (system-qemu-image/shared-store-script
          #:optional (os %demo-operating-system)
          #:key
          (qemu (package (inherit qemu)
                  ;; FIXME/TODO: Use 9p instead of this hack.
                  (source (package-source qemu/smb-shares))))
          (graphic? #t))
  "Return a derivation that builds a script to run a virtual machine image of
OS that shares its store with the host."
  (let* ((initrd (qemu-initrd #:mounts `((cifs "/store" ,(%store-prefix)))
                              #:volatile-root? #t))
         (os     (operating-system (inherit os) (initrd initrd))))
    (define builder
      (mlet %store-monad ((image  (system-qemu-image/shared-store os))
                          (qemu   (package-file qemu
                                                "bin/qemu-system-x86_64"))
                          (bash   (package-file bash "bin/sh"))
                          (kernel (package-file (operating-system-kernel os)
                                                "bzImage"))
                          (initrd initrd)
                          (os-drv (operating-system-derivation os)))
        (return `(let ((out (assoc-ref %outputs "out")))
                   (call-with-output-file out
                     (lambda (port)
                       (display
                        (string-append "#!" ,bash "
# TODO: -virtfs local,path=XXX,security_model=none,mount_tag=store
exec " ,qemu " -enable-kvm -no-reboot -net nic,model=virtio \
  -net user,smb=$PWD \
  -kernel " ,kernel " -initrd "
  ,(string-append (derivation->output-path initrd) "/initrd") " \
-append \"" ,(if graphic? "" "console=ttyS0 ")
"--load=" ,(derivation->output-path os-drv) "/boot --root=/dev/vda1\" \
  -drive file=" ,(derivation->output-path image)
  ",if=virtio,cache=writeback,werror=report,readonly\n")
                        port)))
                   (chmod out #o555)
                   #t))))

    (mlet %store-monad ((image   (system-qemu-image/shared-store os))
                        (initrd  initrd)
                        (qemu    (package->derivation qemu))
                        (bash    (package->derivation bash))
                        (os      (operating-system-derivation os))
                        (builder builder))
      (derivation-expression "run-vm.sh" builder
                             #:inputs `(("qemu" ,qemu)
                                        ("image" ,image)
                                        ("bash" ,bash)
                                        ("initrd" ,initrd)
                                        ("os" ,os))))))

;;; vm.scm ends here
