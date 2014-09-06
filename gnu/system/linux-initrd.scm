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

(define-module (gnu system linux-initrd)
  #:use-module (guix monads)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((guix store)
                #:select (%store-prefix))
  #:use-module ((guix derivations)
                #:select (derivation->output-path))
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages guile)
  #:use-module ((gnu packages make-bootstrap)
                #:select (%guile-static-stripped))
  #:use-module (gnu system file-systems)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:export (expression->initrd
            base-initrd))


;;; Commentary:
;;;
;;; Tools to build initial RAM disks (initrd's) for Linux-Libre, and in
;;; particular initrd's that run Guile.
;;;
;;; Code:


(define* (expression->initrd exp
                             #:key
                             (guile %guile-static-stripped)
                             (cpio cpio)
                             (gzip gzip)
                             (name "guile-initrd")
                             (system (%current-system))
                             (modules '())
                             (to-copy '())
                             (linux #f)
                             (linux-modules '()))
  "Return a derivation that builds a Linux initrd (a gzipped cpio archive)
containing GUILE and that evaluates EXP, a G-expression, upon booting.

LINUX-MODULES is a list of '.ko' file names to be copied from LINUX into the
initrd.  TO-COPY is a list of additional derivations or packages to copy to
the initrd.  MODULES is a list of Guile module names to be embedded in the
initrd."

  ;; General Linux overview in `Documentation/early-userspace/README' and
  ;; `Documentation/filesystems/ramfs-rootfs-initramfs.txt'.

  (define graph-files
    (unfold-right zero?
                  number->string
                  1-
                  (length to-copy)))

  (mlet %store-monad ((source     (imported-modules modules))
                      (compiled   (compiled-modules modules))
                      (module-dir (flat-linux-module-directory linux
                                                               linux-modules)))
    (define builder
      ;; TODO: Move most of this code to (gnu build linux-initrd).
      #~(begin
          (use-modules (gnu build linux-initrd)
                       (guix build utils)
                       (guix build store-copy)
                       (ice-9 pretty-print)
                       (ice-9 popen)
                       (ice-9 match)
                       (ice-9 ftw)
                       (srfi srfi-26)
                       (system base compile)
                       (rnrs bytevectors)
                       ((system foreign) #:select (sizeof)))

          (let ((modules #$source)
                (gos     #$compiled)
                (scm-dir (string-append "share/guile/" (effective-version)))
                (go-dir  (format #f ".cache/guile/ccache/~a-~a-~a-~a"
                                 (effective-version)
                                 (if (eq? (native-endianness) (endianness little))
                                     "LE"
                                     "BE")
                                 (sizeof '*)
                                 (effective-version))))
            (mkdir #$output)
            (mkdir "contents")

            (with-directory-excursion "contents"
              (copy-recursively #$guile ".")
              (call-with-output-file "init"
                (lambda (p)
                  (format p "#!/bin/guile -ds~%!#~%" #$guile)
                  (pretty-print '#$exp p)))
              (chmod "init" #o555)
              (chmod "bin/guile" #o555)

              ;; Copy Guile modules.
              (chmod scm-dir #o777)
              (copy-recursively modules scm-dir
                                #:follow-symlinks? #t)
              (copy-recursively gos (string-append "lib/guile/"
                                                   (effective-version) "/ccache")
                                #:follow-symlinks? #t)

              ;; Compile `init'.
              (mkdir-p go-dir)
              (set! %load-path (cons modules %load-path))
              (set! %load-compiled-path (cons gos %load-compiled-path))
              (compile-file "init"
                            #:opts %auto-compilation-options
                            #:output-file (string-append go-dir "/init.go"))

              ;; Copy Linux modules.
              (mkdir "modules")
              (copy-recursively #$module-dir "modules")

              ;; Populate the initrd's store.
              (with-directory-excursion ".."
                (populate-store '#$graph-files "contents"))

              ;; Reset the timestamps of all the files that will make it in the
              ;; initrd.
              (for-each (cut utime <> 0 0 0 0)
                        (find-files "." ".*"))

              (write-cpio-archive (string-append #$output "/initrd") "."
                                  #:cpio (string-append #$cpio "/bin/cpio")
                                  #:gzip (string-append #$gzip "/bin/gzip"))))))

   (gexp->derivation name builder
                     #:modules '((guix build utils)
                                 (guix build store-copy)
                                 (gnu build linux-initrd))
                     #:references-graphs (zip graph-files to-copy))))

(define (flat-linux-module-directory linux modules)
  "Return a flat directory containing the Linux kernel modules listed in
MODULES and taken from LINUX."
  (define build-exp
    #~(begin
        (use-modules (ice-9 match) (ice-9 regex)
                     (guix build utils))

        (define (string->regexp str)
          ;; Return a regexp that matches STR exactly.
          (string-append "^" (regexp-quote str) "$"))

        (define module-dir
          (string-append #$linux "/lib/modules"))

        (mkdir #$output)
        (for-each (lambda (module)
                    (match (find-files module-dir (string->regexp module))
                      ((file)
                       (format #t "copying '~a'...~%" file)
                       (copy-file file (string-append #$output "/" module)))
                      (()
                       (error "module not found" module module-dir))
                      ((_ ...)
                       (error "several modules by that name"
                              module module-dir))))
                  '#$modules)))

  (gexp->derivation "linux-modules" build-exp
                    #:modules '((guix build utils))))

(define (file-system->spec fs)
  "Return a list corresponding to file-system FS that can be passed to the
initrd code."
  (match fs
    (($ <file-system> device title mount-point type flags options _ check?)
     (list device title mount-point type flags options check?))))

(define* (base-initrd file-systems
                      #:key
                      qemu-networking?
                      virtio?
                      volatile-root?
                      (extra-modules '())
                      guile-modules-in-chroot?)
  "Return a monadic derivation that builds a generic initrd.  FILE-SYSTEMS is
a list of file-systems to be mounted by the initrd, possibly in addition to
the root file system specified on the kernel command line via '--root'.

When QEMU-NETWORKING? is true, set up networking with the standard QEMU
parameters.  When VIRTIO? is true, load additional modules so the initrd can
be used as a QEMU guest with para-virtualized I/O drivers.

When VOLATILE-ROOT? is true, the root file system is writable but any changes
to it are lost.

The initrd is automatically populated with all the kernel modules necessary
for FILE-SYSTEMS and for the given options.  However, additional kernel
modules can be listed in EXTRA-MODULES.  They will be added to the initrd, and
loaded at boot time in the order in which they appear.

When GUILE-MODULES-IN-CHROOT? is true, make core Guile modules available in
the new root.  This is necessary is the file specified as '--load' needs
access to these modules (which is the case if it wants to even just print an
exception and backtrace!)."
  (define virtio-modules
    ;; Modules for Linux para-virtualized devices, for use in QEMU guests.
    '("virtio.ko" "virtio_ring.ko" "virtio_pci.ko"
      "virtio_balloon.ko" "virtio_blk.ko" "virtio_net.ko"))

  (define cifs-modules
    ;; Modules needed to mount CIFS file systems.
    '("md4.ko" "ecb.ko" "cifs.ko"))

  (define virtio-9p-modules
    ;; Modules for the 9p paravirtualized file system.
    '("fscache.ko" "9pnet.ko" "9p.ko" "9pnet_virtio.ko"))

  (define (file-system-type-predicate type)
    (lambda (fs)
      (string=? (file-system-type fs) type)))

  (define linux-modules
    ;; Modules added to the initrd and loaded from the initrd.
    `("libahci.ko" "ahci.ko" ; modules for SATA controllers
      ,@(if (or virtio? qemu-networking?)
            virtio-modules
            '())
      ,@(if (find (file-system-type-predicate "cifs") file-systems)
            cifs-modules
            '())
      ,@(if (find (file-system-type-predicate "9p") file-systems)
            virtio-9p-modules
            '())
      ,@(if volatile-root?
            '("fuse.ko")
            '())
      ,@extra-modules))

  (define helper-packages
    ;; Packages to be copied on the initrd.
    `(,@(if (find (lambda (fs)
                    (string-prefix? "ext" (file-system-type fs)))
                  file-systems)
            (list e2fsck/static)
            '())
      ,@(if volatile-root?
            (list unionfs-fuse/static)
            '())))

  (expression->initrd
   #~(begin
       (use-modules (gnu build linux-boot)
                    (guix build utils)
                    (srfi srfi-26))

       (with-output-to-port (%make-void-port "w")
         (lambda ()
           (set-path-environment-variable "PATH" '("bin" "sbin")
                                          '#$helper-packages)))

       (boot-system #:mounts '#$(map file-system->spec file-systems)
                    #:linux-modules '#$linux-modules
                    #:qemu-guest-networking? #$qemu-networking?
                    #:guile-modules-in-chroot? '#$guile-modules-in-chroot?
                    #:volatile-root? '#$volatile-root?))
   #:name "base-initrd"
   #:modules '((guix build utils)
               (gnu build linux-boot)
               (gnu build file-systems))
   #:to-copy helper-packages
   #:linux linux-libre
   #:linux-modules linux-modules))

;;; linux-initrd.scm ends here
