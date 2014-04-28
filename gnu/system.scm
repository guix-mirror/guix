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

(define-module (gnu system)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (guix derivations)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages package-management)
  #:use-module (gnu services)
  #:use-module (gnu services dmd)
  #:use-module (gnu services base)
  #:use-module (gnu system grub)
  #:use-module (gnu system shadow)
  #:use-module (gnu system linux)
  #:use-module (gnu system linux-initrd)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (operating-system
            operating-system?
            operating-system-services
            operating-system-packages
            operating-system-bootloader-entries
            operating-system-host-name
            operating-system-kernel
            operating-system-initrd
            operating-system-users
            operating-system-groups
            operating-system-packages
            operating-system-timezone
            operating-system-locale
            operating-system-services

            operating-system-derivation
            operating-system-profile))

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
  (initrd operating-system-initrd                 ; monadic derivation
          (default (gnu-system-initrd)))

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
            (default (list coreutils              ; or just PACKAGE
                           grep
                           sed
                           findutils
                           guile
                           bash
                           (@ (gnu packages dmd) dmd)
                           guix
                           tzdata)))

  (timezone operating-system-timezone)            ; string
  (locale   operating-system-locale)              ; string

  (services operating-system-services             ; list of monadic services
            (default %base-services)))



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
                               ((or ((? package? p)) (? package? p))
                                (mlet %store-monad
                                    ((drv (package->derivation p system)))
                                  (return `(,name ,drv))))
                               (((? package? p) output)
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
                           #:guile-for-build guile
                           #:local-build? #t)))

(define* (file-union name files)
  "Return a derivation that builds a directory containing all of FILES.  Each
item in FILES must be a list where the first element is the file name to use
in the new directory, and the second element is a gexp denoting the target
file."
  (define builder
    #~(begin
        (mkdir #$output)
        (chdir #$output)
        #$@(map (match-lambda
                 ((target source)
                  #~(symlink #$source #$target)))
                files)))

  (gexp->derivation name builder))

(define* (etc-directory #:key
                        (locale "C") (timezone "Europe/Paris")
                        (accounts '())
                        (groups '())
                        (pam-services '())
                        (profile "/var/run/current-system/profile"))
  "Return a derivation that builds the static part of the /etc directory."
  (mlet* %store-monad
      ((passwd     (passwd-file accounts))
       (shadow     (passwd-file accounts #:shadow? #t))
       (group      (group-file groups))
       (pam.d      (pam-services->directory pam-services))
       (login.defs (text-file "login.defs" "# Empty for now.\n"))
       (shells     (text-file "shells"            ; used by xterm and others
                              "\
/bin/sh
/run/current-system/bin/sh
/run/current-system/bin/bash\n"))
       (issue      (text-file "issue" "
This is an alpha preview of the GNU system.  Welcome.

This image features the GNU Guix package manager, which was used to
build it (http://www.gnu.org/software/guix/).  The init system is
GNU dmd (http://www.gnu.org/software/dmd/).

You can log in as 'guest' or 'root' with no password.
"))

       ;; TODO: Generate bashrc from packages' search-paths.
       (bashrc    (text-file* "bashrc"  "
export PS1='\\u@\\h\\$ '

export LC_ALL=\"" locale "\"
export TZ=\"" timezone "\"
export TZDIR=\"" tzdata "/share/zoneinfo\"

export PATH=$HOME/.guix-profile/bin:" profile "/bin:" profile "/sbin
export CPATH=$HOME/.guix-profile/include:" profile "/include
export LIBRARY_PATH=$HOME/.guix-profile/lib:" profile "/lib
alias ls='ls -p --color'
alias ll='ls -l'
")))
    (file-union "etc"
                `(("services" ,#~(string-append #$net-base "/etc/services"))
                  ("protocols" ,#~(string-append #$net-base "/etc/protocols"))
                  ("rpc" ,#~(string-append #$net-base "/etc/rpc"))
                  ("pam.d" ,#~#$pam.d)
                  ("login.defs" ,#~#$login.defs)
                  ("issue" ,#~#$issue)
                  ("shells" ,#~#$shells)
                  ("profile" ,#~#$bashrc)
                  ("localtime" ,#~(string-append #$tzdata "/share/zoneinfo/"
                                                 #$timezone))
                  ("passwd" ,#~#$passwd)
                  ("shadow" ,#~#$shadow)
                  ("group" ,#~#$group)))))

(define (operating-system-profile os)
  "Return a derivation that builds the default profile of OS."
  ;; TODO: Replace with a real profile with a manifest.
  (union (operating-system-packages os)
         #:name "default-profile"))

(define (operating-system-accounts os)
  "Return the user accounts for OS, including an obligatory 'root' account."
  (mlet %store-monad ((services (sequence %store-monad
                                          (operating-system-services os))))
    (return (cons (user-account
                   (name "root")
                   (password "")
                   (uid 0) (gid 0)
                   (comment "System administrator")
                   (home-directory "/root"))
                  (append (operating-system-users os)
                          (append-map service-user-accounts
                                      services))))))

(define (operating-system-etc-directory os)
  "Return that static part of the /etc directory of OS."
  (mlet* %store-monad
      ((services     (sequence %store-monad (operating-system-services os)))
       (pam-services ->
                     ;; Services known to PAM.
                     (delete-duplicates
                      (cons %pam-other-services
                            (append-map service-pam-services services))))
       (accounts    (operating-system-accounts os))
       (profile-drv (operating-system-profile os))
       (groups   -> (append (operating-system-groups os)
                            (append-map service-user-groups services))))
   (etc-directory #:accounts accounts #:groups groups
                  #:pam-services pam-services
                  #:locale (operating-system-locale os)
                  #:timezone (operating-system-timezone os)
                  #:profile profile-drv)))

(define (operating-system-boot-script os)
  "Return the boot script for OS---i.e., the code started by the initrd once
we're running in the final root."
  (mlet* %store-monad
      ((services (sequence %store-monad (operating-system-services os)))
       (etc      (operating-system-etc-directory os))
       (dmd-conf (dmd-configuration-file services etc)))
    (gexp->file "boot"
                #~(execl (string-append #$dmd "/bin/dmd")
                         "dmd" "--config" #$dmd-conf))))

(define (operating-system-derivation os)
  "Return a derivation that builds OS."
  (mlet* %store-monad
      ((profile     (operating-system-profile os))
       (etc         (operating-system-etc-directory os))
       (services    (sequence %store-monad (operating-system-services os)))
       (boot        (operating-system-boot-script os))
       (kernel  ->  (operating-system-kernel os))
       (initrd      (operating-system-initrd os))
       (initrd-file -> #~(string-append #$initrd "/initrd"))
       (entries ->  (list (menu-entry
                           (label (string-append
                                   "GNU system with "
                                   (package-full-name kernel)
                                   " (technology preview)"))
                           (linux kernel)
                           (linux-arguments
                            (list "--root=/dev/sda1"
                                  #~(string-append "--load=" #$boot)))
                           (initrd initrd-file))))
       (grub.cfg (grub-configuration-file entries)))
    (file-union "system"
                `(("boot" ,#~#$boot)
                  ("kernel" ,#~#$kernel)
                  ("initrd" ,initrd-file)
                  ("profile" ,#~#$profile)
                  ("grub.cfg" ,#~#$grub.cfg)
                  ("etc" ,#~#$etc)))))

;;; system.scm ends here
