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
  #:use-module (guix utils)
  #:use-module ((guix store)
                #:select (%store-prefix))
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages guile)
  #:use-module ((gnu packages make-bootstrap)
                #:select (%guile-static-stripped))
  #:export (expression->initrd
            qemu-initrd
            gnu-system-initrd))


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
                             (linux #f)
                             (linux-modules '()))
  "Return a package that contains a Linux initrd (a gzipped cpio archive)
containing GUILE and that evaluates EXP upon booting.  LINUX-MODULES is a list
of `.ko' file names to be copied from LINUX into the initrd.  MODULES is a
list of Guile module names to be embedded in the initrd."

  ;; General Linux overview in `Documentation/early-userspace/README' and
  ;; `Documentation/filesystems/ramfs-rootfs-initramfs.txt'.

  (define builder
    `(begin
       (use-modules (guix build utils)
                    (ice-9 pretty-print)
                    (ice-9 popen)
                    (ice-9 match)
                    (ice-9 ftw)
                    (srfi srfi-26)
                    (system base compile)
                    (rnrs bytevectors)
                    ((system foreign) #:select (sizeof)))

       (let ((guile   (assoc-ref %build-inputs "guile"))
             (cpio    (string-append (assoc-ref %build-inputs "cpio")
                                     "/bin/cpio"))
             (gzip    (string-append (assoc-ref %build-inputs "gzip")
                                     "/bin/gzip"))
             (modules (assoc-ref %build-inputs "modules"))
             (gos     (assoc-ref %build-inputs "modules/compiled"))
             (scm-dir (string-append "share/guile/" (effective-version)))
             (go-dir  (format #f ".cache/guile/ccache/~a-~a-~a-~a"
                              (effective-version)
                              (if (eq? (native-endianness) (endianness little))
                                  "LE"
                                  "BE")
                              (sizeof '*)
                              (effective-version)))
             (out     (assoc-ref %outputs "out")))
         (mkdir out)
         (mkdir "contents")
         (with-directory-excursion "contents"
           (copy-recursively guile ".")
           (call-with-output-file "init"
             (lambda (p)
               (format p "#!/bin/guile -ds~%!#~%" guile)
               (pretty-print ',exp p)))
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
           (let* ((linux      (assoc-ref %build-inputs "linux"))
                  (module-dir (and linux
                                   (string-append linux "/lib/modules"))))
             (mkdir "modules")
             ,@(map (lambda (module)
                      `(match (find-files module-dir ,module)
                         ((file)
                          (format #t "copying '~a'...~%" file)
                          (copy-file file (string-append "modules/"
                                                         ,module)))
                         (()
                          (error "module not found" ,module module-dir))
                         ((_ ...)
                          (error "several modules by that name"
                                 ,module module-dir))))
                    linux-modules))

           ;; Reset the timestamps of all the files that will make it in the
           ;; initrd.
           (for-each (cut utime <> 0 0 0 0)
                     (find-files "." ".*"))

           (system* cpio "--version")
           (let ((pipe (open-pipe* OPEN_WRITE cpio "-o"
                                   "-O" (string-append out "/initrd")
                                   "-H" "newc" "--null")))
             (define print0
               (let ((len (string-length "./")))
                 (lambda (file)
                   (format pipe "~a\0" (string-drop file len)))))

             ;; Note: as per `ramfs-rootfs-initramfs.txt', always add
             ;; directory entries before the files that are inside of it: "The
             ;; Linux kernel cpio extractor won't create files in a directory
             ;; that doesn't exist, so the directory entries must go before
             ;; the files that go in those directories."
             (file-system-fold (const #t)
                               (lambda (file stat result) ; leaf
                                 (print0 file))
                               (lambda (dir stat result) ; down
                                 (unless (string=? dir ".")
                                   (print0 dir)))
                               (const #f)         ; up
                               (const #f)         ; skip
                               (const #f)
                               #f
                               ".")

             (and (zero? (close-pipe pipe))
                  (with-directory-excursion out
                    (and (zero? (system* gzip "--best" "initrd"))
                         (rename-file "initrd.gz" "initrd")))))))))

  (mlet* %store-monad
      ((source   (imported-modules modules))
       (compiled (compiled-modules modules))
       (inputs   (lower-inputs
                  `(("guile" ,guile)
                    ("cpio" ,cpio)
                    ("gzip" ,gzip)
                    ("modules" ,source)
                    ("modules/compiled" ,compiled)
                    ,@(if linux
                          `(("linux" ,linux))
                          '())))))
   (derivation-expression name builder
                          #:modules '((guix build utils))
                          #:inputs inputs)))

(define* (qemu-initrd #:key
                      guile-modules-in-chroot?
                      (mounts `((cifs "/store" ,(%store-prefix))
                                (cifs "/xchg" "/xchg"))))
  "Return a monadic derivation that builds an initrd for use in a QEMU guest
where the store is shared with the host.  MOUNTS is a list of file systems to
be mounted atop the root file system, where each item has the form:

    (FILE-SYSTEM-TYPE SOURCE TARGET)

When GUILE-MODULES-IN-CHROOT? is true, make core Guile modules available in
the new root.  This is necessary is the file specified as '--load' needs
access to these modules (which is the case if it wants to even just print an
exception and backtrace!)."
  (define cifs-modules
    ;; Modules needed to mount CIFS file systems.
    '("md4.ko" "ecb.ko" "cifs.ko"))

  (define linux-modules
    ;; Modules added to the initrd and loaded from the initrd.
    (if (assoc-ref mounts 'cifs)
        cifs-modules
        '()))

  (expression->initrd
   `(begin
      (use-modules (guix build linux-initrd))

      (boot-system #:mounts ',mounts
                   #:linux-modules ',linux-modules
                   #:qemu-guest-networking? #t
                   #:guile-modules-in-chroot? ',guile-modules-in-chroot?))
   #:name "qemu-initrd"
   #:modules '((guix build utils)
               (guix build linux-initrd))
   #:linux linux-libre
   #:linux-modules linux-modules))

(define (gnu-system-initrd)
  "Initrd for the GNU system itself, with nothing QEMU-specific."
  (qemu-initrd #:guile-modules-in-chroot? #f))

;;; linux-initrd.scm ends here
