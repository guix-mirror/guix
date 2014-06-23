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
  #:use-module (gnu packages linux)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages which)
  #:use-module (gnu packages less)
  #:use-module (gnu packages zile)
  #:use-module (gnu services)
  #:use-module (gnu services dmd)
  #:use-module (gnu services base)
  #:use-module (gnu system grub)
  #:use-module (gnu system shadow)
  #:use-module (gnu system linux)
  #:use-module (gnu system linux-initrd)
  #:use-module (gnu system file-systems)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (operating-system
            operating-system?

            operating-system-bootloader
            operating-system-services
            operating-system-user-services
            operating-system-packages
            operating-system-host-name
            operating-system-kernel
            operating-system-initrd
            operating-system-users
            operating-system-groups
            operating-system-issue
            operating-system-packages
            operating-system-timezone
            operating-system-locale
            operating-system-file-systems

            operating-system-derivation
            operating-system-profile
            operating-system-grub.cfg

            %base-packages))

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
  (bootloader operating-system-bootloader)        ; <grub-configuration>

  (initrd operating-system-initrd                 ; (list fs) -> M derivation
          (default qemu-initrd))

  (host-name operating-system-host-name)          ; string

  (file-systems operating-system-file-systems     ; list of fs
                (default '()))

  (users operating-system-users                   ; list of user accounts
         (default '()))
  (groups operating-system-groups                 ; list of user groups
          (default %base-groups))

  (skeletons operating-system-skeletons           ; list of name/monadic value
             (default (default-skeletons)))
  (issue operating-system-issue                   ; string
         (default %default-issue))

  (packages operating-system-packages             ; list of (PACKAGE OUTPUT...)
            (default %base-packages))             ; or just PACKAGE

  (timezone operating-system-timezone)            ; string
  (locale   operating-system-locale)              ; string

  (services operating-system-user-services        ; list of monadic services
            (default %base-services))

  (pam-services operating-system-pam-services     ; list of PAM services
                (default (base-pam-services)))
  (setuid-programs operating-system-setuid-programs
                   (default %setuid-programs))    ; list of string-valued gexps

  (sudoers operating-system-sudoers               ; /etc/sudoers contents
           (default %sudoers-specification)))


;;;
;;; Derivation.
;;;

(define* (union inputs
                #:key (guile (%guile-for-build)) (system (%current-system))
                (name "union"))
  "Return a derivation that builds the union of INPUTS.  INPUTS is a list of
input tuples."
  (define builder
    #~(begin
        (use-modules (guix build union))

        (define inputs '#$inputs)

        (setvbuf (current-output-port) _IOLBF)
        (setvbuf (current-error-port) _IOLBF)

        (format #t "building union `~a' with ~a packages...~%"
                #$output (length inputs))
        (union-build #$output inputs)))

  (gexp->derivation name builder
                    #:system system
                    #:modules '((guix build union))
                    #:guile-for-build guile
                    #:local-build? #t))

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


;;;
;;; Services.
;;;

(define (other-file-system-services os)
  "Return file system services for the file systems of OS that are not marked
as 'needed-for-boot'."
  (define file-systems
    (remove (lambda (fs)
              (or (file-system-needed-for-boot? fs)
                  (string=? "/" (file-system-mount-point fs))))
            (operating-system-file-systems os)))

  (sequence %store-monad
            (map (match-lambda
                  (($ <file-system> device title target type flags opts
                                    #f check?)
                   (file-system-service device target type
                                        #:title title
                                        #:check? check?
                                        #:options opts)))
                 file-systems)))

(define (essential-services os)
  "Return the list of essential services for OS.  These are special services
that implement part of what's declared in OS are responsible for low-level
bookkeeping."
  (mlet* %store-monad ((root-fs   (root-file-system-service))
                       (other-fs  (other-file-system-services os))
                       (procs     (user-processes-service
                                   (map (compose first service-provision)
                                        other-fs)))
                       (host-name (host-name-service
                                   (operating-system-host-name os))))
    (return (cons* host-name procs root-fs other-fs))))

(define (operating-system-services os)
  "Return all the services of OS, including \"internal\" services that do not
explicitly appear in OS."
  (mlet %store-monad
      ((user      (sequence %store-monad (operating-system-user-services os)))
       (essential (essential-services os)))
    (return (append essential user))))


;;;
;;; /etc.
;;;

(define %base-packages
  ;; Default set of packages globally visible.  It should include anything
  ;; required for basic administrator tasks.
  (cons* procps psmisc less zile
         guile-final (@ (gnu packages admin) dmd) guix
         util-linux inetutils isc-dhcp
         net-tools                        ; XXX: remove when Inetutils suffices
         module-init-tools udev kbd

         ;; The packages below are also in %FINAL-INPUTS, so take them from
         ;; there to avoid duplication.
         (map canonical-package
              (list bash coreutils findutils grep sed))))

(define %default-issue
  ;; Default contents for /etc/issue.
  "
This is the GNU system.  Welcome.\n")

(define* (etc-directory #:key
                        kernel
                        (locale "C") (timezone "Europe/Paris")
                        (issue "Hello!\n")
                        (skeletons '())
                        (pam-services '())
                        (profile "/run/current-system/profile")
                        (sudoers ""))
  "Return a derivation that builds the static part of the /etc directory."
  (mlet* %store-monad
      ((pam.d      (pam-services->directory pam-services))
       (sudoers    (text-file "sudoers" sudoers))
       (login.defs (text-file "login.defs" "# Empty for now.\n"))
       (shells     (text-file "shells"            ; used by xterm and others
                              "\
/bin/sh
/run/current-system/profile/bin/sh
/run/current-system/profile/bin/bash\n"))
       (issue      (text-file "issue" issue))

       ;; TODO: Generate bashrc from packages' search-paths.
       (bashrc    (text-file* "bashrc"  "
export PS1='\\u@\\h\\$ '

export LC_ALL=\"" locale "\"
export TZ=\"" timezone "\"
export TZDIR=\"" tzdata "/share/zoneinfo\"

# Tell 'modprobe' & co. where to look for modules.
# XXX: The downside of doing it here is that when switching to a new config
# without rebooting, this variable possibly becomes invalid.
export LINUX_MODULE_DIRECTORY=" kernel "/lib/modules

export PATH=$HOME/.guix-profile/bin:/run/current-system/profile/bin
export PATH=/run/setuid-programs:/run/current-system/profile/sbin:$PATH
export CPATH=$HOME/.guix-profile/include:" profile "/include
export LIBRARY_PATH=$HOME/.guix-profile/lib:" profile "/lib
alias ls='ls -p --color'
alias ll='ls -l'
"))
       (skel      (skeleton-directory skeletons)))
    (file-union "etc"
                `(("services" ,#~(string-append #$net-base "/etc/services"))
                  ("protocols" ,#~(string-append #$net-base "/etc/protocols"))
                  ("rpc" ,#~(string-append #$net-base "/etc/rpc"))
                  ("pam.d" ,#~#$pam.d)
                  ("login.defs" ,#~#$login.defs)
                  ("issue" ,#~#$issue)
                  ("skel" ,#~#$skel)
                  ("shells" ,#~#$shells)
                  ("profile" ,#~#$bashrc)
                  ("localtime" ,#~(string-append #$tzdata "/share/zoneinfo/"
                                                 #$timezone))
                  ("sudoers" ,#~#$sudoers)))))

(define (operating-system-profile os)
  "Return a derivation that builds the default profile of OS."
  ;; TODO: Replace with a real profile with a manifest.
  (union (operating-system-packages os)
         #:name "default-profile"))

(define %root-account
  ;; Default root account.
  (user-account
   (name "root")
   (password "")
   (uid 0) (group "root")
   (comment "System administrator")
   (home-directory "/root")))

(define (operating-system-accounts os)
  "Return the user accounts for OS, including an obligatory 'root' account."
  (define users
    ;; Make sure there's a root account.
    (if (find (lambda (user)
                (and=> (user-account-uid user) zero?))
              (operating-system-users os))
        (operating-system-users os)
        (cons %root-account (operating-system-users os))))

  (mlet %store-monad ((services (operating-system-services os)))
    (return (append users
                    (append-map service-user-accounts services)))))

(define (operating-system-etc-directory os)
  "Return that static part of the /etc directory of OS."
  (mlet* %store-monad
      ((services     (operating-system-services os))
       (pam-services ->
                     ;; Services known to PAM.
                     (delete-duplicates
                      (append (operating-system-pam-services os)
                              (append-map service-pam-services services))))
       (profile-drv (operating-system-profile os))
       (skeletons   (operating-system-skeletons os)))
   (etc-directory #:kernel (operating-system-kernel os)
                  #:pam-services pam-services
                  #:skeletons skeletons
                  #:issue (operating-system-issue os)
                  #:locale (operating-system-locale os)
                  #:timezone (operating-system-timezone os)
                  #:sudoers (operating-system-sudoers os)
                  #:profile profile-drv)))

(define %setuid-programs
  ;; Default set of setuid-root programs.
  (let ((shadow (@ (gnu packages admin) shadow)))
    (list #~(string-append #$shadow "/bin/passwd")
          #~(string-append #$shadow "/bin/su")
          #~(string-append #$inetutils "/bin/ping")
          #~(string-append #$sudo "/bin/sudo")
          #~(string-append #$fuse "/bin/fusermount"))))

(define %sudoers-specification
  ;; Default /etc/sudoers contents: 'root' and all members of the 'wheel'
  ;; group can do anything.  See
  ;; <http://www.sudo.ws/sudo/man/1.8.10/sudoers.man.html>.
  ;; TODO: Add a declarative API.
  "root ALL=(ALL) ALL
%wheel ALL=(ALL) ALL\n")

(define (user-group->gexp group)
  "Turn GROUP, a <user-group> object, into a list-valued gexp suitable for
'active-groups'."
  #~(list #$(user-group-name group)
          #$(user-group-password group)
          #$(user-group-id group)))

(define (user-account->gexp account)
  "Turn ACCOUNT, a <user-account> object, into a list-valued gexp suitable for
'activate-users'."
  #~`(#$(user-account-name account)
      #$(user-account-uid account)
      #$(user-account-group account)
      #$(user-account-supplementary-groups account)
      #$(user-account-comment account)
      #$(user-account-home-directory account)
      ,#$(user-account-shell account)             ; this one is a gexp
      #$(user-account-password account)))

(define (operating-system-activation-script os)
  "Return the activation script for OS---i.e., the code that \"activates\" the
stateful part of OS, including user accounts and groups, special directories,
etc."
  (define %modules
    '((guix build activation)
      (guix build utils)
      (guix build linux-initrd)))

  (define (service-activations services)
    ;; Return the activation scripts for SERVICES.
    (let ((gexps (filter-map service-activate services)))
      (sequence %store-monad (map (cut gexp->file "activate-service.scm" <>)
                                  gexps))))

  (mlet* %store-monad ((services (operating-system-services os))
                       (actions  (service-activations services))
                       (etc      (operating-system-etc-directory os))
                       (modules  (imported-modules %modules))
                       (compiled (compiled-modules %modules))
                       (accounts (operating-system-accounts os)))
    (define setuid-progs
      (operating-system-setuid-programs os))

    (define user-specs
      (map user-account->gexp accounts))

    (define groups
      (append (operating-system-groups os)
              (append-map service-user-groups services)))

    (define group-specs
      (map user-group->gexp groups))

    (gexp->file "activate"
                #~(begin
                    (eval-when (expand load eval)
                      ;; Make sure 'use-modules' below succeeds.
                      (set! %load-path (cons #$modules %load-path))
                      (set! %load-compiled-path
                            (cons #$compiled %load-compiled-path)))

                    (use-modules (guix build activation))

                    ;; Populate /etc.
                    (activate-etc #$etc)

                    ;; Add users and user groups.
                    (setenv "PATH"
                            (string-append #$(@ (gnu packages admin) shadow)
                                           "/sbin"))
                    (activate-users+groups (list #$@user-specs)
                                           (list #$@group-specs))

                    ;; Activate setuid programs.
                    (activate-setuid-programs (list #$@setuid-progs))

                    ;; Run the services' activation snippets.
                    ;; TODO: Use 'load-compiled'.
                    (for-each primitive-load '#$actions)

                    ;; Set up /run/current-system.
                    (activate-current-system)))))

(define (operating-system-boot-script os)
  "Return the boot script for OS---i.e., the code started by the initrd once
we're running in the final root."
  (mlet* %store-monad ((services (operating-system-services os))
                       (activate (operating-system-activation-script os))
                       (dmd-conf (dmd-configuration-file services)))
    (gexp->file "boot"
                #~(begin
                    ;; Activate the system.
                    ;; TODO: Use 'load-compiled'.
                    (primitive-load #$activate)

                    ;; Keep track of the booted system.
                    (false-if-exception (delete-file "/run/booted-system"))
                    (symlink (readlink "/run/current-system")
                             "/run/booted-system")

                    ;; Close any remaining open file descriptors to be on the
                    ;; safe side.  This must be the very last thing we do,
                    ;; because Guile has internal FDs such as 'sleep_pipe'
                    ;; that need to be alive.
                    (let loop ((fd 3))
                      (when (< fd 1024)
                        (false-if-exception (close-fdes fd))
                        (loop (+ 1 fd))))

                    ;; Start dmd.
                    (execl (string-append #$dmd "/bin/dmd")
                           "dmd" "--config" #$dmd-conf)))))

(define (operating-system-root-file-system os)
  "Return the root file system of OS."
  (find (match-lambda
         (($ <file-system> _ _ "/") #t)
         (_ #f))
        (operating-system-file-systems os)))

(define (operating-system-initrd-file os)
  "Return a gexp denoting the initrd file of OS."
  (define boot-file-systems
    (filter (match-lambda
             (($ <file-system> device title "/")
              #t)
             (($ <file-system> device title mount-point type flags
                               options boot?)
              boot?))
            (operating-system-file-systems os)))

  (mlet %store-monad
      ((initrd ((operating-system-initrd os) boot-file-systems)))
    (return #~(string-append #$initrd "/initrd"))))

(define (operating-system-grub.cfg os)
  "Return the GRUB configuration file for OS."
  (mlet* %store-monad
      ((system      (operating-system-derivation os))
       (root-fs ->  (operating-system-root-file-system os))
       (kernel ->   (operating-system-kernel os))
       (entries ->  (list (menu-entry
                           (label (string-append
                                   "GNU system with "
                                   (package-full-name kernel)
                                   " (technology preview)"))
                           (linux kernel)
                           (linux-arguments
                            (list (string-append "--root="
                                                 (file-system-device root-fs))
                                  #~(string-append "--system=" #$system)
                                  #~(string-append "--load=" #$system
                                                   "/boot")))
                           (initrd #~(string-append #$system "/initrd"))))))
    (grub-configuration-file (operating-system-bootloader os) entries)))

(define (operating-system-derivation os)
  "Return a derivation that builds OS."
  (mlet* %store-monad
      ((profile     (operating-system-profile os))
       (etc         (operating-system-etc-directory os))
       (boot        (operating-system-boot-script os))
       (kernel  ->  (operating-system-kernel os))
       (initrd      (operating-system-initrd-file os)))
    (file-union "system"
                `(("boot" ,#~#$boot)
                  ("kernel" ,#~#$kernel)
                  ("initrd" ,initrd)
                  ("profile" ,#~#$profile)
                  ("etc" ,#~#$etc)))))

;;; system.scm ends here
