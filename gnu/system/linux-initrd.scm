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

(define (qemu-initrd)
  "Return a monadic derivation that builds an initrd for use in a QEMU guest
where the store is shared with the host."
  (expression->initrd
   '(begin
      (use-modules (srfi srfi-1)
                   (srfi srfi-26)
                   (ice-9 match)
                   ((system base compile) #:select (compile-file))
                   (guix build utils)
                   (guix build linux-initrd))

      (display "Welcome, this is GNU's early boot Guile.\n")
      (display "Use '--repl' for an initrd REPL.\n\n")

      (mount-essential-file-systems)
      (let* ((args    (linux-command-line))
             (option  (lambda (opt)
                        (let ((opt (string-append opt "=")))
                          (and=> (find (cut string-prefix? opt <>)
                                       args)
                                 (lambda (arg)
                                   (substring arg (+ 1 (string-index arg #\=))))))))
             (to-load (option "--load"))
             (root    (option "--root")))

        (when (member "--repl" args)
          ((@ (system repl repl) start-repl)))

        (display "loading CIFS and companion modules...\n")
        (for-each (compose load-linux-module*
                           (cut string-append "/modules/" <>))
                  (list "md4.ko" "ecb.ko" "cifs.ko"))

        (unless (configure-qemu-networking)
          (display "network interface is DOWN\n"))

        ;; Make /dev nodes.
        (make-essential-device-nodes)

        ;; Prepare the real root file system under /root.
        (unless (file-exists? "/root")
          (mkdir "/root"))
        (if root
            (mount root "/root" "ext3")
            (mount "none" "/root" "tmpfs"))
        (mount-essential-file-systems #:root "/root")

        (mkdir-p "/root/xchg")
        (mkdir-p "/root/nix/store")

        (unless (file-exists? "/root/dev")
          (mkdir "/root/dev")
          (make-essential-device-nodes #:root "/root"))

        ;; Mount the host's store and exchange directory.
        (mount-qemu-smb-share "/store" "/root/nix/store")
        (mount-qemu-smb-share "/xchg" "/root/xchg")

        ;; Copy the directories that contain .scm and .go files so that the
        ;; child process in the chroot can load modules (we would bind-mount
        ;; them but for some reason that fails with EINVAL -- XXX).
        (mkdir-p "/root/share")
        (mkdir-p "/root/lib")
        (mount "none" "/root/share" "tmpfs")
        (mount "none" "/root/lib" "tmpfs")
        (copy-recursively "/share" "/root/share"
                          #:log (%make-void-port "w"))
        (copy-recursively "/lib" "/root/lib"
                          #:log (%make-void-port "w"))


        (if to-load
            (letrec ((resolve
                      (lambda (file)
                        ;; If FILE is a symlink to an absolute file name,
                        ;; resolve it as if we were under /root.
                        (let ((st (lstat file)))
                          (if (eq? 'symlink (stat:type st))
                              (let ((target (readlink file)))
                                (resolve (string-append "/root" target)))
                              file)))))
              (format #t "loading boot file '~a'...\n" to-load)
              (compile-file (resolve (string-append "/root/" to-load))
                            #:output-file "/root/loader.go"
                            #:opts %auto-compilation-options)
              (match (primitive-fork)
                (0
                 (chroot "/root")
                 (load-compiled "/loader.go")

                 ;; TODO: Remove /lib, /share, and /loader.go.
                 )
                (pid
                 (format #t "boot file loaded under PID ~a~%" pid)
                 (let ((status (waitpid pid)))
                   (reboot)))))
            (begin
              (display "no boot file passed via '--load'\n")
              (display "entering a warm and cozy REPL\n")
              ((@ (system repl repl) start-repl))))))
   #:name "qemu-initrd"
   #:modules '((guix build utils)
               (guix build linux-initrd))
   #:linux linux-libre
   #:linux-modules '("cifs.ko" "md4.ko" "ecb.ko")))

(define (gnu-system-initrd)
  "Initrd for the GNU system itself, with nothing QEMU-specific."
  (expression->initrd
   '(begin
      (use-modules (srfi srfi-1)
                   (srfi srfi-26)
                   (ice-9 match)
                   (guix build utils)
                   (guix build linux-initrd))

      (display "Welcome, this is GNU's early boot Guile.\n")
      (display "Use '--repl' for an initrd REPL.\n\n")

      (mount-essential-file-systems)
      (let* ((args    (linux-command-line))
             (option  (lambda (opt)
                        (let ((opt (string-append opt "=")))
                          (and=> (find (cut string-prefix? opt <>)
                                       args)
                                 (lambda (arg)
                                   (substring arg (+ 1 (string-index arg #\=))))))))
             (to-load (option "--load"))
             (root    (option "--root")))

        (when (member "--repl" args)
          ((@ (system repl repl) start-repl)))

        ;; Make /dev nodes.
        (make-essential-device-nodes)

        ;; Prepare the real root file system under /root.
        (mkdir-p "/root")
        (if root
            ;; Assume ROOT has a usable /dev tree.
            (mount root "/root" "ext3")
            (begin
              (mount "none" "/root" "tmpfs")
              (make-essential-device-nodes #:root "/root")))

        (mount-essential-file-systems #:root "/root")

        (mkdir-p "/root/tmp")
        (mount "none" "/root/tmp" "tmpfs")

        ;; XXX: We don't copy our fellow Guile modules to /root (see
        ;; 'qemu-initrd'), so if TO-LOAD tries to load a module (which can
        ;; happen if it throws, to display the exception!), then we're
        ;; screwed.  Hopefully TO-LOAD is a simple expression that just does
        ;; '(execlp ...)'.

        (if to-load
            (begin
              (format #t "loading '~a'...\n" to-load)
              (chroot "/root")
              (primitive-load to-load)
              (format (current-error-port)
                      "boot program '~a' terminated, rebooting~%"
                      to-load)
              (sleep 2)
              (reboot))
            (begin
              (display "no init file passed via '--load'\n")
              (display "entering a warm and cozy REPL\n")
              ((@ (system repl repl) start-repl))))))
   #:name "qemu-system-initrd"
   #:modules '((guix build linux-initrd)
               (guix build utils))
   #:linux linux-libre))

;;; linux-initrd.scm ends here
