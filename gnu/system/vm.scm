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
  #:use-module (guix config)
  #:use-module (guix store)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module ((gnu packages base) #:select (%final-inputs
                                              guile-final
                                              gcc-final
                                              glibc-final
                                              coreutils))
  #:use-module (gnu packages guile)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages qemu)
  #:use-module (gnu packages parted)
  #:use-module (gnu packages grub)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages linux-initrd)
  #:use-module (gnu packages package-management)
  #:use-module ((gnu packages make-bootstrap)
                #:select (%guile-static-stripped))
  #:use-module (gnu packages system)

  #:use-module (gnu system shadow)
  #:use-module (gnu system linux)
  #:use-module (gnu system grub)
  #:use-module (gnu system dmd)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)

  #:export (expression->derivation-in-linux-vm
            qemu-image
            system-qemu-image))


;;; Commentary:
;;;
;;; Tools to evaluate build expressions within virtual machines.
;;;
;;; Code:

(define* (expression->derivation-in-linux-vm store name exp
                                             #:key
                                             (system (%current-system))
                                             (inputs '())
                                             (linux linux-libre)
                                             (initrd qemu-initrd)
                                             (qemu qemu/smb-shares)
                                             (env-vars '())
                                             (modules '())
                                             (guile-for-build
                                              (%guile-for-build))

                                             (make-disk-image? #f)
                                             (references-graphs #f)
                                             (disk-image-size
                                              (* 100 (expt 2 20))))
  "Evaluate EXP in a QEMU virtual machine running LINUX with INITRD.  In the
virtual machine, EXP has access to all of INPUTS from the store; it should put
its output files in the `/xchg' directory, which is copied to the derivation's
output when the VM terminates.

When MAKE-DISK-IMAGE? is true, then create a QEMU disk image of
DISK-IMAGE-SIZE bytes and return it.

When REFERENCES-GRAPHS is true, it must be a list of file name/store path
pairs, as for `derivation'.  The files containing the reference graphs are
made available under the /xchg CIFS share."
  ;; FIXME: Allow use of macros from other modules, as done in
  ;; `build-expression->derivation'.

  (define input-alist
    (map (match-lambda
          ((input (? package? package))
           `(,input . ,(package-output store package "out" system)))
          ((input (? package? package) sub-drv)
           `(,input . ,(package-output store package sub-drv system)))
          ((input (? derivation? drv))
           `(,input . ,(derivation->output-path drv)))
          ((input (? derivation? drv) sub-drv)
           `(,input . ,(derivation->output-path drv sub-drv)))
          ((input (and (? string?) (? store-path?) file))
           `(,input . ,file)))
         inputs))

  (define exp*
    ;; EXP, but with INPUTS available.
    `(let ((%build-inputs ',input-alist))
       ,exp))

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
              `(zero? (system* img "create" "image.qcow2"
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
                                            ((name (? package? package)
                                                   sub-drv ...)
                                             `(,name ,(->drv package)
                                                     ,@sub-drv))
                                            ((name (? string? file))
                                             `(,name ,file))
                                            (tuple tuple))
                                           inputs))
                                  #:env-vars env-vars
                                  #:modules (delete-duplicates
                                             `((guix build utils)
                                               ,@modules))
                                  #:guile-for-build guile-for-build
                                  #:references-graphs references-graphs)))

(define* (qemu-image store #:key
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
configuration file.

INPUTS-TO-COPY is a list of inputs (as for packages) whose closure is copied
into the image being built.  When INITIALIZE-STORE? is true, initialize the
store database in the image so that Guix can be used in the image.

POPULATE is a list of directives stating directories or symlinks to be created
in the disk image partition.  It is evaluated once the image has been
populated with INPUTS-TO-COPY.  It can be used to provide additional files,
such as /etc files."
  (define input->name+derivation
    (match-lambda
     ((name (? package? package))
      `(,name . ,(derivation->output-path
                  (package-derivation store package system))))
     ((name (? package? package) sub-drv)
      `(,name . ,(derivation->output-path
                  (package-derivation store package system)
                  sub-drv)))
     ((name (? derivation? drv))
      `(,name . ,(derivation->output-path drv)))
     ((name (? derivation? drv) sub-drv)
      `(,name . ,(derivation->output-path drv sub-drv)))
     ((input (and (? string?) (? store-path?) file))
      `(,input . ,file))))

  (expression->derivation-in-linux-vm
   store "qemu-image"
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
            (grub.cfg (assoc-ref %build-inputs "grub.cfg")))

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
        (and (zero? (system* parted "/dev/vda" "mklabel" "msdos"
                             "mkpart" "primary" "ext2" "1MiB"
                             ,(format #f "~aB"
                                      (- disk-image-size
                                         (* 5 (expt 2 20))))))
             (begin
               (display "creating ext3 partition...\n")
               (and (zero? (system* mkfs "-F" "/dev/vda1"))
                    (begin
                      (display "mounting partition...\n")
                      (mkdir "/fs")
                      (mount "/dev/vda1" "/fs" "ext3")
                      (mkdir-p "/fs/boot/grub")
                      (symlink grub.cfg "/fs/boot/grub/grub.cfg")

                      ;; Populate the image's store.
                      (mkdir-p (string-append "/fs" ,%store-directory))
                      (for-each (lambda (thing)
                                  (copy-recursively thing
                                                    (string-append "/fs"
                                                                   thing)))
                                (cons grub.cfg (things-to-copy)))

                      ;; Populate /dev.
                      (make-essential-device-nodes #:root "/fs")

                      ;; Optionally, register the inputs in the image's store.
                      (let* ((guix     (assoc-ref %build-inputs "guix"))
                             (register (string-append guix
                                                      "/sbin/guix-register")))
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
                                     "/dev/vda"))
                           (zero? (system* umount "/fs"))
                           (reboot))))))))
   #:system system
   #:inputs `(("parted" ,parted)
              ("grub" ,grub)
              ("e2fsprogs" ,e2fsprogs)
              ("grub.cfg" ,grub-configuration)

              ;; For shell scripts.
              ("sed" ,(car (assoc-ref %final-inputs "sed")))
              ("grep" ,(car (assoc-ref %final-inputs "grep")))
              ("coreutils" ,(car (assoc-ref %final-inputs "coreutils")))
              ("findutils" ,(car (assoc-ref %final-inputs "findutils")))
              ("gawk" ,(car (assoc-ref %final-inputs "gawk")))
              ("util-linux" ,util-linux)

              ,@(if initialize-store?
                    `(("guix" ,guix-0.4))
                    '())

              ,@inputs-to-copy)
   #:make-disk-image? #t
   #:disk-image-size disk-image-size
   #:references-graphs (map input->name+derivation inputs-to-copy)
   #:modules '((guix build utils)
               (guix build linux-initrd))))


;;;
;;; Stand-alone VM image.
;;;

(define* (union store inputs
                #:key (guile (%guile-for-build)) (system (%current-system))
                (name "union"))
  "Return a derivation that builds the union of INPUTS.  INPUTS is a list of
input tuples."
  (define builder
    `(begin
       (use-modules (guix build union))

       (setvbuf (current-output-port) _IOLBF)
       (setvbuf (current-error-port) _IOLBF)

       (let ((output (assoc-ref %outputs "out"))
             (inputs (map cdr %build-inputs)))
         (format #t "building union `~a' with ~a packages...~%"
                 output (length inputs))
         (union-build output inputs))))

  (build-expression->derivation store name system builder
                                (map (match-lambda
                                      ((name (? package? p))
                                       `(,name ,(package-derivation store p
                                                                    system)))
                                      ((name (? package? p) output)
                                       `(,name ,(package-derivation store p
                                                                    system)
                                               ,output))
                                      (x x))
                                     inputs)
                                #:modules '((guix build union))
                                #:guile-for-build guile))

(define (system-qemu-image store)
  "Return the derivation of a QEMU image of the GNU system."
  (define motd
    (add-text-to-store store "motd" "
Happy birthday, GNU!                                http://www.gnu.org/gnu30

"))

  (define %pam-services
    ;; Services known to PAM.
    (list %pam-other-services
          (unix-pam-service "login"
                            #:allow-empty-passwords? #t
                            #:motd motd)))

  (define %dmd-services
    ;; Services run by dmd.
    (list (host-name-service store "gnu")
          (mingetty-service store "tty1")
          (mingetty-service store "tty2")
          (mingetty-service store "tty3")
          (mingetty-service store "tty4")
          (mingetty-service store "tty5")
          (mingetty-service store "tty6")
          (syslog-service store)
          (guix-service store #:guix guix-0.4)
          (nscd-service store)

          ;; QEMU networking settings.
          (static-networking-service store "eth0" "10.0.2.10"
                                     #:gateway "10.0.2.2")))

  (define resolv.conf
    ;; Name resolution for default QEMU settings.
    (add-text-to-store store "resolv.conf"
                       "nameserver 10.0.2.3\n"))

  (parameterize ((%guile-for-build (package-derivation store guile-final)))
    (let* ((bash-drv  (package-derivation store bash))
           (bash-file (string-append (derivation->output-path bash-drv)
                                     "/bin/bash"))
           (dmd-drv   (package-derivation store dmd))
           (dmd-file  (string-append (derivation->output-path dmd-drv)
                                     "/bin/dmd"))
           (dmd-conf  (dmd-configuration-file store %dmd-services))
           (accounts  (list (user-account
                             (name "root")
                             (password "")
                             (uid 0) (gid 0)
                             (comment "System administrator")
                             (home-directory "/")
                             (shell bash-file))))
           (passwd    (passwd-file store accounts))
           (shadow    (passwd-file store accounts #:shadow? #t))
           (group     (add-text-to-store store "group"
                                         "root:x:0:\n"))
           (pam.d-drv (pam-services->directory store %pam-services))
           (pam.d     (derivation->output-path pam.d-drv))

           (packages `(("coreutils" ,coreutils)
                       ("bash" ,bash)
                       ("guile" ,guile-2.0)
                       ("dmd" ,dmd)
                       ("gcc" ,gcc-final)
                       ("libc" ,glibc-final)
                       ("inetutils" ,inetutils)
                       ("guix" ,guix-0.4)))

           ;; TODO: Replace with a real profile with a manifest.
           ;; TODO: Generate bashrc from packages' search-paths.
           (profile-drv (union store packages
                               #:name "default-profile"))
           (profile  (derivation->output-path profile-drv))
           (bashrc   (add-text-to-store store "bashrc"
                                        (string-append "
export PS1='\\u@\\h\\$ '
export PATH=$HOME/.guix-profile/bin:" profile "/bin:" profile "/sbin
export CPATH=$HOME/.guix-profile/include:" profile "/include
export LIBRARY_PATH=$HOME/.guix-profile/lib:" profile "/lib
alias ls='ls -p --color'
alias ll='ls -l'
")))

           (issue    (add-text-to-store store "issue" "
This is an alpha preview of the GNU system.  Welcome.

This image features the GNU Guix package manager, which was used to
build it (http://www.gnu.org/software/guix/).  The init system is
GNU dmd (http://www.gnu.org/software/dmd/).

You can log in as 'root' with no password.
"))

           (populate `((directory "/etc")
                       (directory "/var/log")     ; for dmd
                       (directory "/var/run/nscd")
                       ("/etc/shadow" -> ,shadow)
                       ("/etc/passwd" -> ,passwd)
                       ("/etc/login.defs" -> "/dev/null")
                       ("/etc/pam.d" -> ,pam.d)
                       ("/etc/resolv.conf" -> ,resolv.conf)
                       ("/etc/profile" -> ,bashrc)
                       ("/etc/issue" -> ,issue)
                       (directory "/var/nix/gcroots")
                       ("/var/nix/gcroots/default-profile" -> ,profile)))
           (out     (derivation->output-path
                     (package-derivation store mingetty)))
           (boot    (add-text-to-store store "boot"
                                       (object->string
                                        `(execl ,dmd-file "dmd"
                                                "--config" ,dmd-conf))
                                       (list out)))
           (entries  (list (menu-entry
                            (label (string-append
                                    "GNU System with Linux-Libre "
                                    (package-version linux-libre)
                                    " (technology preview)"))
                            (linux linux-libre)
                            (linux-arguments `("--root=/dev/vda1"
                                               ,(string-append "--load=" boot)))
                            (initrd gnu-system-initrd))))
           (grub.cfg (grub-configuration-file store entries)))
      (qemu-image store
                  #:grub-configuration grub.cfg
                  #:populate populate
                  #:disk-image-size (* 500 (expt 2 20))
                  #:initialize-store? #t
                  #:inputs-to-copy `(("boot" ,boot)
                                     ("linux" ,linux-libre)
                                     ("initrd" ,gnu-system-initrd)
                                     ("pam.d" ,pam.d-drv)
                                     ("profile" ,profile-drv)

                                     ;; Configuration.
                                     ("dmd.conf" ,dmd-conf)
                                     ("etc-pam.d" ,pam.d-drv)
                                     ("etc-passwd" ,passwd)
                                     ("etc-shadow" ,shadow)
                                     ("etc-group" ,group)
                                     ("etc-resolv.conf" ,resolv.conf)
                                     ("etc-bashrc" ,bashrc)
                                     ("etc-issue" ,issue)
                                     ("etc-motd" ,motd)
                                     ,@(append-map service-inputs
                                                   %dmd-services))))))

;;; vm.scm ends here
