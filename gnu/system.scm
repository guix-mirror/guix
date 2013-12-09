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

(define-module (gnu system)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (gnu packages linux-initrd)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages system)
  #:use-module (gnu packages package-management)
  #:use-module (gnu system dmd)
  #:use-module (gnu system grub)
  #:use-module (gnu system shadow)
  #:use-module (gnu system linux)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (operating-system
            operating-system?
            operating-system-services
            operating-system-packages

            operating-system-derivation))

;;; Commentary:
;;;
;;; This module supports whole-system configuration.
;;;
;;; Code:

;; System-wide configuration.
;; TODO: Add per-field docstrings/stexi.
(define-record-type* <operating-system> operating-system
  make-operating-system
  operating-system?
  (kernel operating-system-kernel                 ; package
          (default linux-libre))
  (bootloader operating-system-bootloader         ; package
              (default grub))
  (bootloader-entries operating-system-bootloader-entries ; list
                      (default '()))
  (initrd operating-system-initrd
          (default gnu-system-initrd))

  (host-name operating-system-host-name)          ; string

  (file-systems operating-system-file-systems     ; list of fs
                (default '()))

  (users operating-system-users                   ; list of user accounts
         (default '()))
  (groups operating-system-groups                 ; list of user groups
          (default (list (user-group
                          (name "root")
                          (id 0))
                         (user-group
                          (name "users")
                          (id 100)
                          (members '("guest"))))))

  (packages operating-system-packages             ; list of (PACKAGE OUTPUT...)
            (default `(("coreutils" ,coreutils)
                       ("grep" ,grep)
                       ("guile" ,guile)
                       ("bash" ,bash)
                       ("dmd" ,(@ (gnu packages dmd) dmd))
                       ("guix" ,guix))))

  (timezone operating-system-timezone)            ; string
  (locale   operating-system-locale)              ; string

  (services operating-system-services             ; list of monadic services
            (default
              (let ((motd (text-file "motd" "
This is the GNU operating system, welcome!\n\n")))
                (list (mingetty-service "tty1" #:motd motd)
                      (mingetty-service "tty2" #:motd motd)
                      (mingetty-service "tty3" #:motd motd)
                      (mingetty-service "tty4" #:motd motd)
                      (mingetty-service "tty5" #:motd motd)
                      (mingetty-service "tty6" #:motd motd)
                      (syslog-service)
                      (guix-service)
                      (nscd-service)

                      ;; QEMU networking settings.
                      (static-networking-service "eth0" "10.0.2.10"
                                                 #:name-servers '("10.0.2.3")
                                                 #:gateway "10.0.2.2"))))))



;;;
;;; Derivation.
;;;

(define* (union inputs
                #:key (guile (%guile-for-build)) (system (%current-system))
                (name "union"))
  "Return a derivation that builds the union of INPUTS.  INPUTS is a list of
input tuples."
  (define builder
    '(begin
       (use-modules (guix build union))

       (setvbuf (current-output-port) _IOLBF)
       (setvbuf (current-error-port) _IOLBF)

       (let ((output (assoc-ref %outputs "out"))
             (inputs (map cdr %build-inputs)))
         (format #t "building union `~a' with ~a packages...~%"
                 output (length inputs))
         (union-build output inputs))))

  (mlet %store-monad
      ((inputs (sequence %store-monad
                         (map (match-lambda
                               ((name (? package? p))
                                (mlet %store-monad
                                    ((drv (package->derivation p system)))
                                  (return `(,name ,drv))))
                               ((name (? package? p) output)
                                (mlet %store-monad
                                    ((drv (package->derivation p system)))
                                  (return `(,name ,drv ,output))))
                               (x
                                (return x)))
                              inputs))))
    (derivation-expression name builder
                           #:system system
                           #:inputs inputs
                           #:modules '((guix build union))
                           #:guile-for-build guile)))

(define* (file-union files
                     #:key (inputs '()) (name "file-union"))
  "Return a derivation that builds a directory containing all of FILES.  Each
item in FILES must be a list where the first element is the file name to use
in the new directory, and the second element is the target file.

The subset of FILES corresponding to plain store files is automatically added
as an inputs; additional inputs, such as derivations, are taken from INPUTS."
  (mlet %store-monad ((inputs (lower-inputs inputs)))
    (let* ((outputs (append-map (match-lambda
                                 ((_ (? derivation? drv))
                                  (list (derivation->output-path drv)))
                                 ((_ (? derivation? drv) sub-drv ...)
                                  (map (cut derivation->output-path drv <>)
                                       sub-drv))
                                 (_ '()))
                                inputs))
           (inputs   (append inputs
                             (filter (match-lambda
                                      ((_ file)
                                       ;; Elements of FILES that are store
                                       ;; files and that do not correspond to
                                       ;; the output of INPUTS are considered
                                       ;; inputs (still here?).
                                       (and (direct-store-path? file)
                                            (not (member file outputs)))))
                                     files))))
      (derivation-expression name
                             `(let ((out (assoc-ref %outputs "out")))
                                (mkdir out)
                                (chdir out)
                                ,@(map (match-lambda
                                        ((name target)
                                         `(symlink ,target ,name)))
                                       files))

                             #:inputs inputs))))

(define (links inputs)
  "Return a directory with symbolic links to all of INPUTS.  This is
essentially useful when one wants to keep references to all of INPUTS, be they
directories or regular files."
  (define builder
    '(begin
       (use-modules (srfi srfi-1))

       (let ((out (assoc-ref %outputs "out")))
         (mkdir out)
         (chdir out)
         (fold (lambda (file number)
                 (symlink file (number->string number))
                 (+ 1 number))
               0
               (map cdr %build-inputs))
         #t)))

  (mlet %store-monad ((inputs (lower-inputs inputs)))
    (derivation-expression "links" builder
                           #:inputs inputs)))

(define* (etc-directory #:key
                        (accounts '())
                        (groups '())
                        (pam-services '())
                        (profile "/var/run/current-system/profile"))
  "Return a derivation that builds the static part of the /etc directory."
  (mlet* %store-monad
      ((services   (package-file net-base "etc/services"))
       (protocols  (package-file net-base "etc/protocols"))
       (rpc        (package-file net-base "etc/rpc"))
       (passwd     (passwd-file accounts))
       (shadow     (passwd-file accounts #:shadow? #t))
       (group      (group-file groups))
       (pam.d      (pam-services->directory pam-services))
       (login.defs (text-file "login.defs" "# Empty for now.\n"))
       (issue      (text-file "issue" "
This is an alpha preview of the GNU system.  Welcome.

This image features the GNU Guix package manager, which was used to
build it (http://www.gnu.org/software/guix/).  The init system is
GNU dmd (http://www.gnu.org/software/dmd/).

You can log in as 'guest' or 'root' with no password.
"))

       ;; TODO: Generate bashrc from packages' search-paths.
       (bashrc    (text-file "bashrc" (string-append "
export PS1='\\u@\\h\\$ '
export PATH=$HOME/.guix-profile/bin:" profile "/bin:" profile "/sbin
export CPATH=$HOME/.guix-profile/include:" profile "/include
export LIBRARY_PATH=$HOME/.guix-profile/lib:" profile "/lib
alias ls='ls -p --color'
alias ll='ls -l'
")))

       (files -> `(("services" ,services)
                   ("protocols" ,protocols)
                   ("rpc" ,rpc)
                   ("pam.d" ,(derivation->output-path pam.d))
                   ("login.defs" ,login.defs)
                   ("issue" ,issue)
                   ("profile" ,bashrc)
                   ("passwd" ,passwd)
                   ("shadow" ,shadow)
                   ("group" ,group))))
    (file-union files
                #:inputs `(("net" ,net-base)
                           ("pam.d" ,pam.d))
                #:name "etc")))

(define (operating-system-derivation os)
  "Return a derivation that builds OS."
  (mlet* %store-monad
      ((services (sequence %store-monad
                           (cons (host-name-service
                                  (operating-system-host-name os))
                                 (operating-system-services os))))
       (pam-services ->
                     ;; Services known to PAM.
                     (delete-duplicates
                      (cons %pam-other-services
                            (append-map service-pam-services services))))

       (bash-file (package-file bash "bin/bash"))
       (dmd-file  (package-file (@ (gnu packages system) dmd) "bin/dmd"))
       (accounts -> (cons (user-account
                            (name "root")
                            (password "")
                            (uid 0) (gid 0)
                            (comment "System administrator")
                            (home-directory "/")
                            (shell bash-file))
                          (append (operating-system-users os)
                                  (append-map service-user-accounts
                                              services))))
       (groups   -> (append (operating-system-groups os)
                            (append-map service-user-groups services)))
       (packages -> (operating-system-packages os))

       ;; TODO: Replace with a real profile with a manifest.
       (profile-drv (union packages
                           #:name "default-profile"))
       (profile ->  (derivation->output-path profile-drv))
       (etc-drv     (etc-directory #:accounts accounts #:groups groups
                                   #:pam-services pam-services
                                   #:profile profile))
       (etc     ->  (derivation->output-path etc-drv))
       (dmd-conf  (dmd-configuration-file services etc))


       (boot     (text-file "boot"
                            (object->string
                             `(execl ,dmd-file "dmd"
                                     "--config" ,dmd-conf))))
       (kernel  ->  (operating-system-kernel os))
       (kernel-dir  (package-file kernel))
       (initrd  ->  (operating-system-initrd os))
       (initrd-file (package-file initrd))
       (entries ->  (list (menu-entry
                           (label (string-append
                                   "GNU system with "
                                   (package-full-name kernel)
                                   " (technology preview)"))
                           (linux kernel)
                           (linux-arguments `("--root=/dev/vda1"
                                              ,(string-append "--load=" boot)))
                           (initrd initrd))))
       (grub.cfg (grub-configuration-file entries))
       (extras   (links (delete-duplicates
                         (append-map service-inputs services)))))
    (file-union `(("boot" ,boot)
                  ("kernel" ,kernel-dir)
                  ("initrd" ,initrd-file)
                  ("dmd.conf" ,dmd-conf)
                  ("bash" ,bash-file) ; XXX: should be a <user-account> input?
                  ("profile" ,profile)
                  ("grub.cfg" ,grub.cfg)
                  ("etc" ,etc)
                  ("service-inputs" ,(derivation->output-path extras)))
                #:inputs `(("kernel" ,kernel)
                           ("initrd" ,initrd)
                           ("bash" ,bash)
                           ("profile" ,profile-drv)
                           ("etc" ,etc-drv)
                           ("service-inputs" ,extras))
                #:name "system")))

;;; system.scm ends here
