;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Alex Kost <alezost@gmail.com>
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
  #:use-module (guix profiles)
  #:use-module (guix ui)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages less)
  #:use-module (gnu packages zile)
  #:use-module (gnu packages nano)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages man)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages firmware)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages polkit)
  #:autoload   (gnu packages cryptsetup) (cryptsetup)
  #:use-module (gnu services)
  #:use-module (gnu services dmd)
  #:use-module (gnu services base)
  #:use-module (gnu system grub)
  #:use-module (gnu system shadow)
  #:use-module (gnu system nss)
  #:use-module (gnu system locale)
  #:use-module (gnu system linux)
  #:use-module (gnu system linux-initrd)
  #:use-module (gnu system file-systems)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:export (operating-system
            operating-system?

            operating-system-bootloader
            operating-system-services
            operating-system-user-services
            operating-system-packages
            operating-system-host-name
            operating-system-hosts-file
            operating-system-kernel
            operating-system-kernel-arguments
            operating-system-initrd
            operating-system-users
            operating-system-groups
            operating-system-issue
            operating-system-timezone
            operating-system-locale
            operating-system-locale-definitions
            operating-system-mapped-devices
            operating-system-file-systems
            operating-system-activation-script

            operating-system-derivation
            operating-system-profile
            operating-system-grub.cfg

            local-host-aliases
            %setuid-programs
            %base-packages
            %base-firmware

            luks-device-mapping))

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
  (kernel-arguments operating-system-kernel-arguments
                    (default '()))                ; list of gexps/strings
  (bootloader operating-system-bootloader)        ; <grub-configuration>

  (initrd operating-system-initrd                 ; (list fs) -> M derivation
          (default base-initrd))
  (firmware operating-system-firmware             ; list of packages
            (default %base-firmware))

  (host-name operating-system-host-name)          ; string
  (hosts-file operating-system-hosts-file         ; file-like | #f
              (default #f))

  (mapped-devices operating-system-mapped-devices ; list of <mapped-device>
                  (default '()))
  (file-systems operating-system-file-systems)    ; list of fs
  (swap-devices operating-system-swap-devices     ; list of strings
                (default '()))

  (users operating-system-users                   ; list of user accounts
         (default %base-user-accounts))
  (groups operating-system-groups                 ; list of user groups
          (default %base-groups))

  (skeletons operating-system-skeletons           ; list of name/monadic value
             (default (default-skeletons)))
  (issue operating-system-issue                   ; string
         (default %default-issue))

  (packages operating-system-packages             ; list of (PACKAGE OUTPUT...)
            (default %base-packages))             ; or just PACKAGE

  (timezone operating-system-timezone)            ; string
  (locale   operating-system-locale               ; string
            (default "en_US.utf8"))
  (locale-definitions operating-system-locale-definitions ; list of <locale-definition>
                      (default %default-locale-definitions))
  (name-service-switch operating-system-name-service-switch ; <name-service-switch>
                       (default %default-nss))

  (services operating-system-user-services        ; list of monadic services
            (default %base-services))

  (pam-services operating-system-pam-services     ; list of PAM services
                (default (base-pam-services)))
  (setuid-programs operating-system-setuid-programs
                   (default %setuid-programs))    ; list of string-valued gexps

  (sudoers-file operating-system-sudoers-file     ; file-like
                (default %sudoers-specification)))


;;;
;;; Derivation.
;;;

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

(define (directory-union name things)
  "Return a directory that is the union of THINGS."
  (match things
    ((one)
     ;; Only one thing; return it.
     (with-monad %store-monad (return one)))
    (_
     (gexp->derivation name
                       #~(begin
                           (use-modules (guix build union))
                           (union-build #$output '#$things))
                       #:modules '((guix build union))
                       #:local-build? #t))))


;;;
;;; Services.
;;;

(define (open-luks-device source target)
  "Return a gexp that maps SOURCE to TARGET as a LUKS device, using
'cryptsetup'."
  #~(zero? (system* (string-append #$cryptsetup "/sbin/cryptsetup")
                    "open" "--type" "luks"
                    #$source #$target)))

(define (close-luks-device source target)
  "Return a gexp that closes TARGET, a LUKS device."
  #~(zero? (system* (string-append #$cryptsetup "/sbin/cryptsetup")
                    "close" #$target)))

(define luks-device-mapping
  ;; The type of LUKS mapped devices.
  (mapped-device-kind
   (open open-luks-device)
   (close close-luks-device)))

(define (other-file-system-services os)
  "Return file system services for the file systems of OS that are not marked
as 'needed-for-boot'."
  (define file-systems
    (remove file-system-needed-for-boot?
            (operating-system-file-systems os)))

  (define (device-mappings fs)
    (filter (lambda (md)
              (string=? (string-append "/dev/mapper/"
                                       (mapped-device-target md))
                        (file-system-device fs)))
            (operating-system-mapped-devices os)))

  (define (requirements fs)
    ;; XXX: Fiddling with dmd service names is not nice.
    (append (map (lambda (fs)
                   (symbol-append 'file-system-
                                  (string->symbol
                                   (file-system-mount-point fs))))
                 (file-system-dependencies fs))
            (map (lambda (md)
                   (symbol-append 'device-mapping-
                                  (string->symbol (mapped-device-target md))))
                 (device-mappings fs))))

  (sequence %store-monad
            (map (lambda (fs)
                   (match fs
                     (($ <file-system> device title target type flags opts
                                       #f check? create?)
                      (file-system-service device target type
                                           #:title title
                                           #:requirements (requirements fs)
                                           #:check? check?
                                           #:create-mount-point? create?
                                           #:options opts
                                           #:flags flags))))
                 file-systems)))

(define (mapped-device-user device file-systems)
  "Return a file system among FILE-SYSTEMS that uses DEVICE, or #f."
  (let ((target (string-append "/dev/mapper/" (mapped-device-target device))))
    (find (lambda (fs)
            (string=? (file-system-device fs) target))
          file-systems)))

(define (operating-system-user-mapped-devices os)
  "Return the subset of mapped devices that can be installed in
user-land--i.e., those not needed during boot."
  (let ((devices      (operating-system-mapped-devices os))
        (file-systems (operating-system-file-systems os)))
   (filter (lambda (md)
             (let ((user (mapped-device-user md file-systems)))
               (or (not user)
                   (not (file-system-needed-for-boot? user)))))
           devices)))

(define (operating-system-boot-mapped-devices os)
  "Return the subset of mapped devices that must be installed during boot,
from the initrd."
  (let ((devices      (operating-system-mapped-devices os))
        (file-systems (operating-system-file-systems os)))
   (filter (lambda (md)
             (let ((user (mapped-device-user md file-systems)))
               (and user (file-system-needed-for-boot? user))))
           devices)))

(define (device-mapping-services os)
  "Return the list of device-mapping services for OS as a monadic list."
  (sequence %store-monad
            (map (lambda (md)
                   (let* ((source (mapped-device-source md))
                          (target (mapped-device-target md))
                          (type   (mapped-device-type md))
                          (open   (mapped-device-kind-open type))
                          (close  (mapped-device-kind-close type)))
                     (device-mapping-service target
                                             (open source target)
                                             (close source target))))
                 (operating-system-user-mapped-devices os))))

(define (swap-services os)
  "Return the list of swap services for OS as a monadic list."
  (sequence %store-monad
            (map swap-service (operating-system-swap-devices os))))

(define (essential-services os)
  "Return the list of essential services for OS.  These are special services
that implement part of what's declared in OS are responsible for low-level
bookkeeping."
  (define known-fs
    (map file-system-mount-point (operating-system-file-systems os)))

  (mlet* %store-monad ((mappings  (device-mapping-services os))
                       (root-fs   (root-file-system-service))
                       (other-fs  (other-file-system-services os))
                       (unmount   (user-unmount-service known-fs))
                       (swaps     (swap-services os))
                       (procs     (user-processes-service
                                   (map (compose first service-provision)
                                        other-fs)))
                       (host-name (host-name-service
                                   (operating-system-host-name os))))
    (return (cons* host-name procs root-fs unmount
                   (append other-fs mappings swaps)))))

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

(define %base-firmware
  ;; Firmware usable by default.
  (list ath9k-htc-firmware))

(define %base-packages
  ;; Default set of packages globally visible.  It should include anything
  ;; required for basic administrator tasks.
  (cons* procps psmisc which less zile nano
         (@ (gnu packages admin) dmd) guix
         lsof                                 ;for Guix's 'list-runtime-roots'
         pciutils usbutils
         util-linux inetutils isc-dhcp

         ;; wireless-tools is deprecated in favor of iw, but it's still what
         ;; many people are familiar with, so keep it around.
         iw wireless-tools

         net-tools                        ; XXX: remove when Inetutils suffices
         man-db

         ;; The 'sudo' command is already in %SETUID-PROGRAMS, but we also
         ;; want the other commands and the man pages (notably because
         ;; auto-completion in Emacs shell relies on man pages.)
         sudo

         ;; Get 'insmod' & co. from kmod, not module-init-tools, since udev
         ;; already depends on it anyway.
         kmod eudev

         e2fsprogs kbd

         bash-completion

         ;; The packages below are also in %FINAL-INPUTS, so take them from
         ;; there to avoid duplication.
         (map canonical-package
              (list guile-2.0 bash coreutils findutils grep sed
                    diffutils patch gawk tar gzip bzip2 xz lzip))))

(define %default-issue
  ;; Default contents for /etc/issue.
  "
This is the GNU system.  Welcome.\n")

(define (local-host-aliases host-name)
  "Return aliases for HOST-NAME, to be used in /etc/hosts."
  (string-append "127.0.0.1 localhost " host-name "\n"
                 "::1       localhost " host-name "\n"))

(define (default-/etc/hosts host-name)
  "Return the default /etc/hosts file."
  (plain-file "hosts" (local-host-aliases host-name)))

(define (emacs-site-file)
  "Return the Emacs 'site-start.el' file.  That file contains the necessary
settings for 'guix.el' to work out-of-the-box."
  (gexp->file "site-start.el"
              #~(progn
                 ;; Add the "normal" elisp directory to the search path;
                 ;; guix.el may be there.
                 (add-to-list
                  'load-path
                  "/run/current-system/profile/share/emacs/site-lisp")

                 ;; Attempt to load guix.el.
                 (require 'guix-init nil t)

                 ;; Attempt to load geiser.
                 (require 'geiser-install nil t))))

(define (emacs-site-directory)
  "Return the Emacs site directory, aka. /etc/emacs."
  (mlet %store-monad ((file (emacs-site-file)))
    (gexp->derivation "emacs"
                      #~(begin
                          (mkdir #$output)
                          (chdir #$output)
                          (symlink #$file "site-start.el")))))

(define (user-shells os)
  "Return the list of all the shells used by the accounts of OS.  These may be
gexps or strings."
  (mlet %store-monad ((accounts (operating-system-accounts os)))
    (return (map user-account-shell accounts))))

(define (shells-file shells)
  "Return a derivation that builds a shell list for use as /etc/shells based
on SHELLS.  /etc/shells is used by xterm, polkit, and other programs."
  (gexp->derivation "shells"
                    #~(begin
                        (use-modules (srfi srfi-1))

                        (define shells
                          (delete-duplicates (list #$@shells)))

                        (call-with-output-file #$output
                          (lambda (port)
                            (display "\
/bin/sh
/run/current-system/profile/bin/sh
/run/current-system/profile/bin/bash\n" port)
                            (for-each (lambda (shell)
                                        (display shell port)
                                        (newline port))
                                      shells))))))

(define* (etc-directory #:key
                        (locale "C") (timezone "Europe/Paris")
                        (issue "Hello!\n")
                        (skeletons '())
                        (pam-services '())
                        (profile "/run/current-system/profile")
                        hosts-file nss (shells '())
                        (sudoers-file (plain-file "sudoers" "")))
  "Return a derivation that builds the static part of the /etc directory."
  (mlet* %store-monad
      ((pam.d      (pam-services->directory pam-services))
       (login.defs (text-file "login.defs" "# Empty for now.\n"))
       (shells     (shells-file shells))
       (emacs      (emacs-site-directory))
       (issue      (text-file "issue" issue))
       (nsswitch   (text-file "nsswitch.conf"
                              (name-service-switch->string nss)))

       ;; Startup file for POSIX-compliant login shells, which set system-wide
       ;; environment variables.
       (profile    (text-file* "profile"  "\
export LANG=\"" locale "\"
export TZ=\"" timezone "\"
export TZDIR=\"" tzdata "/share/zoneinfo\"

# Tell 'modprobe' & co. where to look for modules.
export LINUX_MODULE_DIRECTORY=/run/booted-system/kernel/lib/modules

# These variables are honored by OpenSSL (libssl) and Git.
export SSL_CERT_DIR=/etc/ssl/certs
export SSL_CERT_FILE=\"$SSL_CERT_DIR/ca-certificates.crt\"
export GIT_SSL_CAINFO=\"$SSL_CERT_FILE\"

# Crucial variables that could be missing in the profiles' 'etc/profile'
# because they would require combining both profiles.
# FIXME: See <http://bugs.gnu.org/20255>.
export MANPATH=$HOME/.guix-profile/share/man:/run/current-system/profile/share/man
export INFOPATH=$HOME/.guix-profile/share/info:/run/current-system/profile/share/info
export XDG_DATA_DIRS=$HOME/.guix-profile/share:/run/current-system/profile/share
export XDG_CONFIG_DIRS=$HOME/.guix-profile/etc/xdg:/run/current-system/profile/etc/xdg

# Ignore the default value of 'PATH'.
unset PATH

# Load the system profile's settings.
GUIX_PROFILE=/run/current-system/profile \\
. /run/current-system/profile/etc/profile

# Prepend setuid programs.
export PATH=/run/setuid-programs:$PATH

if [ -f \"$HOME/.guix-profile/etc/profile\" ]
then
  # Load the user profile's settings.
  GUIX_PROFILE=\"$HOME/.guix-profile\" \\
  . \"$HOME/.guix-profile/etc/profile\"
else
  # At least define this one so that basic things just work
  # when the user installs their first package.
  export PATH=\"$HOME/.guix-profile/bin:$PATH\"
fi

# Append the directory of 'site-start.el' to the search path.
export EMACSLOADPATH=:/etc/emacs

# By default, applications that use D-Bus, such as Emacs, abort at startup
# when /etc/machine-id is missing.  Make sure these warnings are non-fatal.
export DBUS_FATAL_WARNINGS=0

# Allow Aspell to find dictionaries installed in the user profile.
export ASPELL_CONF=\"dict-dir $HOME/.guix-profile/lib/aspell\"

if [ -n \"$BASH_VERSION\" -a -f /etc/bashrc ]
then
  # Load Bash-specific initialization code.
  . /etc/bashrc
fi
"))

       (bashrc    (text-file "bashrc" "\
# Bash-specific initialization.

# The 'bash-completion' package.
if [ -f /run/current-system/profile/etc/profile.d/bash_completion.sh ]
then
  # Bash-completion sources ~/.bash_completion.  It installs a dynamic
  # completion loader that searches its own completion files as well
  # as those in ~/.guix-profile and /run/current-system/profile.
  source /run/current-system/profile/etc/profile.d/bash_completion.sh
fi\n"))
       (skel      (skeleton-directory skeletons)))
    (file-union "etc"
                `(("services" ,#~(string-append #$net-base "/etc/services"))
                  ("protocols" ,#~(string-append #$net-base "/etc/protocols"))
                  ("rpc" ,#~(string-append #$net-base "/etc/rpc"))
                  ("emacs" ,#~#$emacs)
                  ("pam.d" ,#~#$pam.d)
                  ("login.defs" ,#~#$login.defs)
                  ("issue" ,#~#$issue)
                  ("nsswitch.conf" ,#~#$nsswitch)
                  ("skel" ,#~#$skel)
                  ("shells" ,#~#$shells)
                  ("profile" ,#~#$profile)
                  ("bashrc" ,#~#$bashrc)
                  ("hosts" ,#~#$hosts-file)
                  ("localtime" ,#~(string-append #$tzdata "/share/zoneinfo/"
                                                 #$timezone))
                  ("sudoers" ,sudoers-file)))))

(define (operating-system-profile os)
  "Return a derivation that builds the system profile of OS."
  (profile-derivation (manifest (map package->manifest-entry
                                     (operating-system-packages os)))))

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

(define (maybe-string->file file-name thing)
  "If THING is a string, return a <plain-file> with THING as its content.
Otherwise just return THING.

This is for backward-compatibility of fields that used to be strings and are
now file-like objects.."
  (match thing
    ((? string?)
     (warning (_ "using a string for file '~a' is deprecated; \
use 'plain-file' instead~%")
              file-name)
     (plain-file file-name thing))
    (x
     x)))

(define (maybe-file->monadic file-name thing)
  "If THING is a value in %STORE-MONAD, return it as is; otherwise return
THING in the %STORE-MONAD.

This is for backward-compatibility of fields that used to be monadic values
and are now file-like objects."
  (with-monad %store-monad
    (match thing
      ((? procedure?)
       (warning (_ "using a monadic value for '~a' is deprecated; \
use 'plain-file' instead~%")
                file-name)
       thing)
      (x
       (return x)))))

(define (operating-system-etc-directory os)
  "Return that static part of the /etc directory of OS."
  (mlet* %store-monad
      ((services     (operating-system-services os))
       (pam-services ->
                     ;; Services known to PAM.
                     (append (operating-system-pam-services os)
                             (append-map service-pam-services services)))
       (profile-drv (operating-system-profile os))
       (skeletons   (operating-system-skeletons os))
       (/etc/hosts  (maybe-file->monadic
                     "hosts"
                     (or (operating-system-hosts-file os)
                         (default-/etc/hosts (operating-system-host-name os)))))
       (shells      (user-shells os)))
   (etc-directory #:pam-services pam-services
                  #:skeletons skeletons
                  #:issue (operating-system-issue os)
                  #:locale (operating-system-locale os)
                  #:nss (operating-system-name-service-switch os)
                  #:timezone (operating-system-timezone os)
                  #:hosts-file /etc/hosts
                  #:shells shells
                  #:sudoers-file (maybe-string->file
                                  "sudoers"
                                  (operating-system-sudoers-file os))
                  #:profile profile-drv)))

(define %setuid-programs
  ;; Default set of setuid-root programs.
  (let ((shadow (@ (gnu packages admin) shadow))
        ;; XXX Remove this hack when the main 'dbus' package is fixed.
        (dbus (@@ (gnu packages glib) dbus-fixed)))
    (list #~(string-append #$shadow "/bin/passwd")
          #~(string-append #$shadow "/bin/su")
          #~(string-append #$inetutils "/bin/ping")
          #~(string-append #$inetutils "/bin/ping6")
          #~(string-append #$sudo "/bin/sudo")
          #~(string-append #$fuse "/bin/fusermount")
          #~(string-append #$dbus "/libexec/dbus-daemon-launch-helper")  ; XXX should be group "messagebus" and mode 4550
          #~(string-append #$polkit "/bin/pkexec")
          #~(string-append #$polkit "/lib/polkit-1/polkit-agent-helper-1"))))

(define %sudoers-specification
  ;; Default /etc/sudoers contents: 'root' and all members of the 'wheel'
  ;; group can do anything.  See
  ;; <http://www.sudo.ws/sudo/man/1.8.10/sudoers.man.html>.
  ;; TODO: Add a declarative API.
  (plain-file "sudoers" "\
root ALL=(ALL) ALL
%wheel ALL=(ALL) ALL\n"))

(define (user-group->gexp group)
  "Turn GROUP, a <user-group> object, into a list-valued gexp suitable for
'active-groups'."
  #~(list #$(user-group-name group)
          #$(user-group-password group)
          #$(user-group-id group)
          #$(user-group-system? group)))

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
      #$(user-account-password account)
      #$(user-account-system? account)))

(define (modprobe-wrapper)
  "Return a wrapper for the 'modprobe' command that knows where modules live.

This wrapper is typically invoked by the Linux kernel ('call_modprobe', in
kernel/kmod.c), a situation where the 'LINUX_MODULE_DIRECTORY' environment
variable is not set---hence the need for this wrapper."
  (let ((modprobe "/run/current-system/profile/bin/modprobe"))
    (gexp->script "modprobe"
                  #~(begin
                      (setenv "LINUX_MODULE_DIRECTORY"
                              "/run/booted-system/kernel/lib/modules")
                      (apply execl #$modprobe
                             (cons #$modprobe (cdr (command-line))))))))

(define (operating-system-activation-script os)
  "Return the activation script for OS---i.e., the code that \"activates\" the
stateful part of OS, including user accounts and groups, special directories,
etc."
  (define %modules
    '((gnu build activation)
      (gnu build linux-boot)
      (gnu build linux-modules)
      (gnu build file-systems)
      (guix build utils)
      (guix build syscalls)
      (guix elf)))

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
                       (modprobe (modprobe-wrapper))
                       (firmware (directory-union
                                  "firmware" (operating-system-firmware os)))
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

    (assert-valid-users/groups accounts groups)

    (gexp->file "activate"
                #~(begin
                    (eval-when (expand load eval)
                      ;; Make sure 'use-modules' below succeeds.
                      (set! %load-path (cons #$modules %load-path))
                      (set! %load-compiled-path
                            (cons #$compiled %load-compiled-path)))

                    (use-modules (gnu build activation))

                    ;; Make sure /bin/sh is valid and current.
                    (activate-/bin/sh
                     (string-append #$(canonical-package bash)
                                    "/bin/sh"))

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

                    ;; Tell the kernel to use our 'modprobe' command.
                    (activate-modprobe #$modprobe)

                    ;; Tell the kernel where firmware is.
                    (activate-firmware
                     (string-append #$firmware "/lib/firmware"))

                    ;; Let users debug their own processes!
                    (activate-ptrace-attach)

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
                    (use-modules (guix build utils))

                    ;; Clean out /tmp and /var/run.
                    ;;
                    ;; XXX This needs to happen before service activations, so
                    ;; it has to be here, but this also implicitly assumes
                    ;; that /tmp and /var/run are on the root partition.
                    (false-if-exception (delete-file-recursively "/tmp"))
                    (false-if-exception (delete-file-recursively "/var/run"))
                    (false-if-exception (mkdir "/tmp"))
                    (false-if-exception (chmod "/tmp" #o1777))
                    (false-if-exception (mkdir "/var/run"))
                    (false-if-exception (chmod "/var/run" #o755))

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
    (filter file-system-needed-for-boot?
            (operating-system-file-systems os)))

  (define mapped-devices
    (operating-system-boot-mapped-devices os))

  (define make-initrd
    (operating-system-initrd os))

  (mlet %store-monad ((initrd (make-initrd boot-file-systems
                                           #:linux (operating-system-kernel os)
                                           #:mapped-devices mapped-devices)))
    (return #~(string-append #$initrd "/initrd"))))

(define (operating-system-locale-directory os)
  "Return the directory containing the locales compiled for the definitions
listed in OS.  The C library expects to find it under
/run/current-system/locale."
  ;; While we're at it, check whether the locale of OS is defined.
  (unless (member (operating-system-locale os)
                  (map locale-definition-name
                       (operating-system-locale-definitions os)))
    (raise (condition
            (&message (message "system locale lacks a definition")))))

  (locale-directory (operating-system-locale-definitions os)))

(define (kernel->grub-label kernel)
  "Return a label for the GRUB menu entry that boots KERNEL."
  (string-append "GNU with "
                 (string-titlecase (package-name kernel)) " "
                 (package-version kernel)
                 " (alpha)"))

(define* (operating-system-grub.cfg os #:optional (old-entries '()))
  "Return the GRUB configuration file for OS.  Use OLD-ENTRIES to populate the
\"old entries\" menu."
  (mlet* %store-monad
      ((system      (operating-system-derivation os))
       (root-fs ->  (operating-system-root-file-system os))
       (kernel ->   (operating-system-kernel os))
       (entries ->  (list (menu-entry
                           (label (kernel->grub-label kernel))
                           (linux kernel)
                           (linux-arguments
                            (cons* (string-append "--root="
                                                  (file-system-device root-fs))
                                   #~(string-append "--system=" #$system)
                                   #~(string-append "--load=" #$system
                                                    "/boot")
                                   (operating-system-kernel-arguments os)))
                           (initrd #~(string-append #$system "/initrd"))))))
    (grub-configuration-file (operating-system-bootloader os) entries
                             #:old-entries old-entries)))

(define (operating-system-parameters-file os)
  "Return a file that describes the boot parameters of OS.  The primary use of
this file is the reconstruction of GRUB menu entries for old configurations."
  (mlet %store-monad ((initrd   (operating-system-initrd-file os))
                      (root ->  (operating-system-root-file-system os))
                      (label -> (kernel->grub-label
                                 (operating-system-kernel os))))
    (gexp->file "parameters"
                #~(boot-parameters (version 0)
                                   (label #$label)
                                   (root-device #$(file-system-device root))
                                   (kernel #$(operating-system-kernel os))
                                   (kernel-arguments
                                    #$(operating-system-kernel-arguments os))
                                   (initrd #$initrd)))))

(define (operating-system-derivation os)
  "Return a derivation that builds OS."
  (mlet* %store-monad
      ((profile     (operating-system-profile os))
       (etc         (operating-system-etc-directory os))
       (boot        (operating-system-boot-script os))
       (kernel  ->  (operating-system-kernel os))
       (initrd      (operating-system-initrd-file os))
       (locale      (operating-system-locale-directory os))
       (params      (operating-system-parameters-file os)))
    (file-union "system"
                `(("boot" ,#~#$boot)
                  ("kernel" ,#~#$kernel)
                  ("parameters" ,#~#$params)
                  ("initrd" ,initrd)
                  ("profile" ,#~#$profile)
                  ("locale" ,#~#$locale)          ;used by libc
                  ("etc" ,#~#$etc)))))

;;; system.scm ends here
