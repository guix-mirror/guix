;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2016 Alex Kost <alezost@gmail.com>
;;; Copyright © 2016 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019 Meiyo Peng <meiyo.peng@gmail.com>
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
  #:use-module (guix inferior)
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
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages man)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages firmware)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services base)
  #:use-module (gnu bootloader)
  #:use-module (gnu system shadow)
  #:use-module (gnu system nss)
  #:use-module (gnu system locale)
  #:use-module (gnu system pam)
  #:use-module (gnu system linux-initrd)
  #:use-module (gnu system uuid)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (rnrs bytevectors)
  #:export (operating-system
            operating-system?
            this-operating-system

            operating-system-bootloader
            operating-system-services
            operating-system-essential-services
            operating-system-user-services
            operating-system-packages
            operating-system-host-name
            operating-system-hosts-file
            operating-system-kernel
            operating-system-kernel-file
            operating-system-kernel-arguments
            operating-system-initrd-modules
            operating-system-initrd
            operating-system-users
            operating-system-groups
            operating-system-issue
            operating-system-timezone
            operating-system-locale
            operating-system-locale-definitions
            operating-system-locale-libcs
            operating-system-mapped-devices
            operating-system-file-systems
            operating-system-store-file-system
            operating-system-user-mapped-devices
            operating-system-boot-mapped-devices
            operating-system-activation-script
            operating-system-user-accounts
            operating-system-shepherd-service-names
            operating-system-user-kernel-arguments

            operating-system-derivation
            operating-system-profile
            operating-system-bootcfg
            operating-system-etc-directory
            operating-system-locale-directory
            operating-system-boot-script

            system-linux-image-file-name

            boot-parameters
            boot-parameters?
            boot-parameters-label
            boot-parameters-root-device
            boot-parameters-bootloader-name
            boot-parameters-store-device
            boot-parameters-store-mount-point
            boot-parameters-kernel
            boot-parameters-kernel-arguments
            boot-parameters-initrd
            read-boot-parameters
            read-boot-parameters-file
            boot-parameters->menu-entry

            local-host-aliases
            %root-account
            %setuid-programs
            %base-packages
            %base-firmware))

;;; Commentary:
;;;
;;; This module supports whole-system configuration.
;;;
;;; Code:

(define (bootable-kernel-arguments system root-device)
  "Return a list of kernel arguments (gexps) to boot SYSTEM from ROOT-DEVICE."
  (list (string-append "--root="
                       (cond ((uuid? root-device)

                              ;; Note: Always use the DCE format because that's
                              ;; what (gnu build linux-boot) expects for the
                              ;; '--root' kernel command-line option.
                              (uuid->string (uuid-bytevector root-device)
                                            'dce))
                             ((file-system-label? root-device)
                              (file-system-label->string root-device))
                             (else root-device)))
        #~(string-append "--system=" #$system)
        #~(string-append "--load=" #$system "/boot")))

;; System-wide configuration.
;; TODO: Add per-field docstrings/stexi.
(define-record-type* <operating-system> operating-system
  make-operating-system
  operating-system?
  this-operating-system

  (kernel operating-system-kernel                 ; package
          (default linux-libre))
  (kernel-arguments operating-system-user-kernel-arguments
                    (default '()))                ; list of gexps/strings
  (bootloader operating-system-bootloader)        ; <bootloader-configuration>

  (keyboard-layout operating-system-keyboard-layout ;#f | <keyboard-layout>
                   (default #f))
  (initrd operating-system-initrd                 ; (list fs) -> file-like
          (default base-initrd))
  (initrd-modules operating-system-initrd-modules ; list of strings
                  (thunked)                       ; it's system-dependent
                  (default %base-initrd-modules))

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

  (skeletons operating-system-skeletons           ; list of name/file-like value
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
  (locale-libcs operating-system-locale-libcs     ; list of <packages>
                (default %default-locale-libcs))
  (name-service-switch operating-system-name-service-switch ; <name-service-switch>
                       (default %default-nss))

  (essential-services operating-system-essential-services ; list of services
                      (thunked)
                      (default (essential-services this-operating-system)))
  (services operating-system-user-services        ; list of services
            (default %base-services))

  (pam-services operating-system-pam-services     ; list of PAM services
                (default (base-pam-services)))
  (setuid-programs operating-system-setuid-programs
                   (default %setuid-programs))    ; list of string-valued gexps

  (sudoers-file operating-system-sudoers-file     ; file-like
                (default %sudoers-specification)))

(define (operating-system-kernel-arguments os root-device)
  "Return all the kernel arguments, including the ones not specified
directly by the user."
  (append (bootable-kernel-arguments os root-device)
          (operating-system-user-kernel-arguments os)))


;;;
;;; Boot parameters
;;;

(define-record-type* <boot-parameters>
  boot-parameters make-boot-parameters boot-parameters?
  (label            boot-parameters-label)
  ;; Because we will use the 'store-device' to create the GRUB search command,
  ;; the 'store-device' has slightly different semantics than 'root-device'.
  ;; The 'store-device' can be a file system uuid, a file system label, or #f,
  ;; but it cannot be a device path such as "/dev/sda3", since GRUB would not
  ;; understand that.  The 'root-device', on the other hand, corresponds
  ;; exactly to the device field of the <file-system> object representing the
  ;; OS's root file system, so it might be a device path like "/dev/sda3".
  (root-device      boot-parameters-root-device)
  (bootloader-name  boot-parameters-bootloader-name)
  (store-device     boot-parameters-store-device)
  (store-mount-point boot-parameters-store-mount-point)
  (kernel           boot-parameters-kernel)
  (kernel-arguments boot-parameters-kernel-arguments)
  (initrd           boot-parameters-initrd))

(define (ensure-not-/dev device)
  "If DEVICE starts with a slash, return #f.  This is meant to filter out
Linux device names such as /dev/sda, and to preserve GRUB device names and
file system labels."
  (if (and (string? device) (string-prefix? "/" device))
      #f
      device))

(define (read-boot-parameters port)
  "Read boot parameters from PORT and return the corresponding
<boot-parameters> object or #f if the format is unrecognized."
  (define device-sexp->device
    (match-lambda
      (('uuid (? symbol? type) (? bytevector? bv))
       (bytevector->uuid bv type))
      (('file-system-label (? string? label))
       (file-system-label label))
      ((? bytevector? bv)                         ;old format
       (bytevector->uuid bv 'dce))
      ((? string? device)
       ;; It used to be that we would not distinguish between labels and
       ;; device names.  Try to infer the right thing here.
       (if (string-prefix? "/dev/" device)
           device
           (file-system-label device)))))

  (match (read port)
    (('boot-parameters ('version 0)
                       ('label label) ('root-device root)
                       ('kernel linux)
                       rest ...)
     (boot-parameters
      (label label)
      (root-device (device-sexp->device root))

      (bootloader-name
       (match (assq 'bootloader-name rest)
         ((_ args) args)
         (#f       'grub))) ; for compatibility reasons.

      ;; In the past, we would store the directory name of the kernel instead
      ;; of the absolute file name of its image.  Detect that and correct it.
      (kernel (if (string=? linux (direct-store-path linux))
                  (string-append linux "/"
                                 (system-linux-image-file-name))
                  linux))

      (kernel-arguments
       (match (assq 'kernel-arguments rest)
         ((_ args) args)
         (#f       '())))                         ;the old format

      (initrd
       (match (assq 'initrd rest)
         (('initrd ('string-append directory file)) ;the old format
          (string-append directory file))
         (('initrd (? string? file))
          file)))

      (store-device
       ;; Linux device names like "/dev/sda1" are not suitable GRUB device
       ;; identifiers, so we just filter them out.
       (ensure-not-/dev
        (match (assq 'store rest)
          (('store ('device #f) _ ...)
           root-device)
          (('store ('device device) _ ...)
           (device-sexp->device device))
          (_                                      ;the old format
           root-device))))

      (store-mount-point
       (match (assq 'store rest)
         (('store ('device _) ('mount-point mount-point) _ ...)
          mount-point)
         (_                                       ;the old format
          "/")))))
    (x                                            ;unsupported format
     (warning (G_ "unrecognized boot parameters at '~a'~%")
              (port-filename port))
     #f)))

(define (read-boot-parameters-file system)
  "Read boot parameters from SYSTEM's (system or generation) \"parameters\"
file and returns the corresponding <boot-parameters> object or #f if the
format is unrecognized.
The object has its kernel-arguments extended in order to make it bootable."
  (let* ((file (string-append system "/parameters"))
         (params (call-with-input-file file read-boot-parameters))
         (root (boot-parameters-root-device params)))
    (boot-parameters
     (inherit params)
     (kernel-arguments (append (bootable-kernel-arguments system root)
                               (boot-parameters-kernel-arguments params))))))

(define (boot-parameters->menu-entry conf)
  (menu-entry
   (label (boot-parameters-label conf))
   (device (boot-parameters-store-device conf))
   (device-mount-point (boot-parameters-store-mount-point conf))
   (linux (boot-parameters-kernel conf))
   (linux-arguments (boot-parameters-kernel-arguments conf))
   (initrd (boot-parameters-initrd conf))))



;;;
;;; Services.
;;;

(define (non-boot-file-system-service os)
  "Return the file system service for the file systems of OS that are not
marked as 'needed-for-boot'."
  (define file-systems
    (remove file-system-needed-for-boot?
            (operating-system-file-systems os)))

  (define mapped-devices-for-boot
    (operating-system-boot-mapped-devices os))

  (define (device-mappings fs)
    (let ((device (file-system-device fs)))
      (if (string? device)                        ;title is 'device
          (filter (lambda (md)
                    (string=? (string-append "/dev/mapper/"
                                             (mapped-device-target md))
                              device))
                  (operating-system-mapped-devices os))
          '())))

  (define (add-dependencies fs)
    ;; Add the dependencies due to device mappings to FS.
    (file-system
      (inherit fs)
      (dependencies
       (delete-duplicates
        (remove (cut member <> mapped-devices-for-boot)
                (append (device-mappings fs)
                        (file-system-dependencies fs)))
        eq?))))

  (service file-system-service-type
           (map add-dependencies file-systems)))

(define (mapped-device-users device file-systems)
  "Return the subset of FILE-SYSTEMS that use DEVICE."
  (let ((target (string-append "/dev/mapper/" (mapped-device-target device))))
    (filter (lambda (fs)
              (or (member device (file-system-dependencies fs))
                  (and (string? (file-system-device fs))
                       (string=? (file-system-device fs) target))))
            file-systems)))

(define (operating-system-user-mapped-devices os)
  "Return the subset of mapped devices that can be installed in
user-land--i.e., those not needed during boot."
  (let ((devices      (operating-system-mapped-devices os))
        (file-systems (operating-system-file-systems os)))
   (filter (lambda (md)
             (let ((users (mapped-device-users md file-systems)))
               (not (any file-system-needed-for-boot? users))))
           devices)))

(define (operating-system-boot-mapped-devices os)
  "Return the subset of mapped devices that must be installed during boot,
from the initrd."
  (let ((devices      (operating-system-mapped-devices os))
        (file-systems (operating-system-file-systems os)))
   (filter (lambda (md)
             (let ((users (mapped-device-users md file-systems)))
               (any file-system-needed-for-boot? users)))
           devices)))

(define (device-mapping-services os)
  "Return the list of device-mapping services for OS as a list."
  (map device-mapping-service
       (operating-system-user-mapped-devices os)))

(define (swap-services os)
  "Return the list of swap services for OS."
  (map swap-service (operating-system-swap-devices os)))

(define* (system-linux-image-file-name #:optional (system (%current-system)))
  "Return the basename of the kernel image file for SYSTEM."
  ;; FIXME: Evaluate the conditional based on the actual current system.
  (cond
   ((string-prefix? "arm" (%current-system)) "zImage")
   ((string-prefix? "mips" (%current-system)) "vmlinuz")
   ((string-prefix? "aarch64" (%current-system)) "Image")
   (else "bzImage")))

(define (operating-system-kernel-file os)
  "Return an object representing the absolute file name of the kernel image of
OS."
  (file-append (operating-system-kernel os)
               "/" (system-linux-image-file-name os)))

(define* (operating-system-directory-base-entries os)
  "Return the basic entries of the 'system' directory of OS for use as the
value of the SYSTEM-SERVICE-TYPE service."
  (let ((locale (operating-system-locale-directory os)))
    (mlet %store-monad ((kernel -> (operating-system-kernel os))
                        (initrd -> (operating-system-initrd-file os))
                        (params    (operating-system-boot-parameters-file os)))
      (return `(("kernel" ,kernel)
                ("parameters" ,params)
                ("initrd" ,initrd)
                ("locale" ,locale))))))   ;used by libc

(define* (essential-services os)
  "Return the list of essential services for OS.  These are special services
that implement part of what's declared in OS are responsible for low-level
bookkeeping."
  (define known-fs
    (map file-system-mount-point (operating-system-file-systems os)))

  (let* ((mappings  (device-mapping-services os))
         (root-fs   (root-file-system-service))
         (other-fs  (non-boot-file-system-service os))
         (swaps     (swap-services os))
         (procs     (service user-processes-service-type))
         (host-name (host-name-service (operating-system-host-name os)))
         (entries   (operating-system-directory-base-entries os)))
    (cons* (service system-service-type entries)
           %boot-service

           ;; %SHEPHERD-ROOT-SERVICE must come last so that the gexp that
           ;; execs shepherd comes last in the boot script (XXX).  Likewise,
           ;; the cleanup service must come first so that its gexp runs before
           ;; activation code.
           (service cleanup-service-type #f)
           %activation-service
           %shepherd-root-service

           (pam-root-service (operating-system-pam-services os))
           (account-service (append (operating-system-accounts os)
                                    (operating-system-groups os))
                            (operating-system-skeletons os))
           (operating-system-etc-service os)
           (service fstab-service-type '())
           (session-environment-service
            (operating-system-environment-variables os))
           host-name procs root-fs
           (service setuid-program-service-type
                    (operating-system-setuid-programs os))
           (service profile-service-type
                    (operating-system-packages os))
           other-fs
           (append mappings swaps

                   ;; Add the firmware service.
                   (list %linux-bare-metal-service
                         (service firmware-service-type
                                  (operating-system-firmware os)))))))

(define* (operating-system-services os)
  "Return all the services of OS, including \"essential\" services."
  (instantiate-missing-services
   (append (operating-system-user-services os)
           (operating-system-essential-services os))))


;;;
;;; /etc.
;;;

(define %base-firmware
  ;; Firmware usable by default.
  (list ath9k-htc-firmware
        openfwwf-firmware))

(define %base-packages
  ;; Default set of packages globally visible.  It should include anything
  ;; required for basic administrator tasks.
  (cons* procps psmisc which less zile nano
         pciutils usbutils
         util-linux
         inetutils isc-dhcp
         (@ (gnu packages admin) shadow)          ;for 'passwd'

         ;; wireless-tools is deprecated in favor of iw, but it's still what
         ;; many people are familiar with, so keep it around.
         iw wireless-tools

         iproute
         net-tools                        ; XXX: remove when Inetutils suffices
         man-db
         info-reader                     ;the standalone Info reader (no Perl)

         ;; The 'sudo' command is already in %SETUID-PROGRAMS, but we also
         ;; want the other commands and the man pages (notably because
         ;; auto-completion in Emacs shell relies on man pages.)
         sudo

         ;; Get 'insmod' & co. from kmod, not module-init-tools, since udev
         ;; already depends on it anyway.
         kmod eudev

         e2fsprogs kbd

         bash-completion

         ;; XXX: We don't use (canonical-package guile-2.2) here because that
         ;; would create a collision in the global profile between the GMP
         ;; variant propagated by 'guile-final' and the GMP variant propagated
         ;; by 'gnutls', itself propagated by 'guix'.
         guile-2.2

         ;; The packages below are also in %FINAL-INPUTS, so take them from
         ;; there to avoid duplication.
         (map canonical-package
              (list bash coreutils findutils grep sed
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

(define* (operating-system-etc-service os)
  "Return a <service> that builds containing the static part of the /etc
directory."
  (let ((login.defs
          (plain-file "login.defs"
                      (string-append
                        "# Default paths for non-login shells started by su(1).\n"
                        "ENV_PATH    /run/setuid-programs:"
                        "/run/current-system/profile/bin:"
                        "/run/current-system/profile/sbin\n"
                        "ENV_SUPATH  /run/setuid-programs:"
                        "/run/current-system/profile/bin:"
                        "/run/current-system/profile/sbin\n")))

        (issue      (plain-file "issue" (operating-system-issue os)))
        (nsswitch   (plain-file "nsswitch.conf"
                                (name-service-switch->string
                                 (operating-system-name-service-switch os))))

        ;; Startup file for POSIX-compliant login shells, which set system-wide
        ;; environment variables.
        (profile    (mixed-text-file "profile"  "\
# Crucial variables that could be missing in the profiles' 'etc/profile'
# because they would require combining both profiles.
# FIXME: See <http://bugs.gnu.org/20255>.
export MANPATH=$HOME/.guix-profile/share/man:/run/current-system/profile/share/man
export INFOPATH=$HOME/.guix-profile/share/info:/run/current-system/profile/share/info
export XDG_DATA_DIRS=$HOME/.guix-profile/share:/run/current-system/profile/share
export XDG_CONFIG_DIRS=$HOME/.guix-profile/etc/xdg:/run/current-system/profile/etc/xdg

# Make sure libXcursor finds cursors installed into user or system profiles.  See <http://bugs.gnu.org/24445>
export XCURSOR_PATH=$HOME/.icons:$HOME/.guix-profile/share/icons:/run/current-system/profile/share/icons

# Ignore the default value of 'PATH'.
unset PATH

# Load the system profile's settings.
GUIX_PROFILE=/run/current-system/profile ; \\
. /run/current-system/profile/etc/profile

# Since 'lshd' does not use pam_env, /etc/environment must be explicitly
# loaded when someone logs in via SSH.  See <http://bugs.gnu.org/22175>.
# We need 'PATH' to be defined here, for 'cat' and 'cut'.  Do this before
# reading the user's 'etc/profile' to allow variables to be overridden.
if [ -f /etc/environment -a -n \"$SSH_CLIENT\" \\
     -a -z \"$LINUX_MODULE_DIRECTORY\" ]
then
  . /etc/environment
  export `cat /etc/environment | cut -d= -f1`
fi

# Arrange so that ~/.config/guix/current comes first.
for profile in \"$HOME/.guix-profile\" \"$HOME/.config/guix/current\"
do
  if [ -f \"$profile/etc/profile\" ]
  then
    # Load the user profile's settings.
    GUIX_PROFILE=\"$profile\" ; \\
    . \"$profile/etc/profile\"
  else
    # At least define this one so that basic things just work
    # when the user installs their first package.
    export PATH=\"$profile/bin:$PATH\"
  fi
done

# Prepend setuid programs.
export PATH=/run/setuid-programs:$PATH

# Arrange so that ~/.config/guix/current/share/info comes first.
export INFOPATH=\"$HOME/.config/guix/current/share/info:$INFOPATH\"

# Set the umask, notably for users logging in via 'lsh'.
# See <http://bugs.gnu.org/22650>.
umask 022

# Allow Hunspell-based applications (IceCat, LibreOffice, etc.) to
# find dictionaries.
export DICPATH=\"$HOME/.guix-profile/share/hunspell:/run/current-system/profile/share/hunspell\"

# Allow GStreamer-based applications to find plugins.
export GST_PLUGIN_PATH=\"$HOME/.guix-profile/lib/gstreamer-1.0\"

if [ -n \"$BASH_VERSION\" -a -f /etc/bashrc ]
then
  # Load Bash-specific initialization code.
  . /etc/bashrc
fi
"))

        (bashrc    (plain-file "bashrc" "\
# Bash-specific initialization.

# The 'bash-completion' package.
if [ -f /run/current-system/profile/etc/profile.d/bash_completion.sh ]
then
  # Bash-completion sources ~/.bash_completion.  It installs a dynamic
  # completion loader that searches its own completion files as well
  # as those in ~/.guix-profile and /run/current-system/profile.
  source /run/current-system/profile/etc/profile.d/bash_completion.sh
fi\n")))
    (etc-service
     `(("services" ,(file-append net-base "/etc/services"))
       ("protocols" ,(file-append net-base "/etc/protocols"))
       ("rpc" ,(file-append net-base "/etc/rpc"))
       ("login.defs" ,#~#$login.defs)
       ("issue" ,#~#$issue)
       ("nsswitch.conf" ,#~#$nsswitch)
       ("profile" ,#~#$profile)
       ("bashrc" ,#~#$bashrc)
       ("hosts" ,#~#$(or (operating-system-hosts-file os)
                         (default-/etc/hosts (operating-system-host-name os))))
       ;; Write the operating-system-host-name to /etc/hostname to prevent
       ;; NetworkManager from changing the system's hostname when connecting
       ;; to certain networks.  Some discussion at
       ;; https://lists.gnu.org/archive/html/help-guix/2017-09/msg00037.html
       ("hostname" ,(plain-file "hostname" (operating-system-host-name os)))
       ("localtime" ,(file-append tzdata "/share/zoneinfo/"
                                  (operating-system-timezone os)))
       ("sudoers" ,(operating-system-sudoers-file os))))))

(define %root-account
  ;; Default root account.
  (user-account
   (name "root")
   (password "")
   (uid 0) (group "root")
   (comment "System administrator")
   (home-directory "/root")))

(define (operating-system-accounts os)
  "Return the user accounts for OS, including an obligatory 'root' account,
and excluding accounts requested by services."
  ;; Make sure there's a root account.
  (if (find (lambda (user)
              (and=> (user-account-uid user) zero?))
            (operating-system-users os))
      (operating-system-users os)
      (cons %root-account (operating-system-users os))))

(define (maybe-string->file file-name thing)
  "If THING is a string, return a <plain-file> with THING as its content.
Otherwise just return THING.

This is for backward-compatibility of fields that used to be strings and are
now file-like objects.."
  (match thing
    ((? string?)
     (warning (G_ "using a string for file '~a' is deprecated; \
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
       (warning (G_ "using a monadic value for '~a' is deprecated; \
use 'plain-file' instead~%")
                file-name)
       thing)
      (x
       (return x)))))

(define (operating-system-etc-directory os)
  "Return that static part of the /etc directory of OS."
  (etc-directory
   (fold-services (operating-system-services os)
                  #:target-type etc-service-type)))

(define (operating-system-environment-variables os)
  "Return the environment variables of OS for
@var{session-environment-service-type}, to be used in @file{/etc/environment}."
  `(("LANG" . ,(operating-system-locale os))
    ;; Note: No need to set 'TZ' since (1) we provide /etc/localtime, and (2)
    ;; it doesn't work for setuid binaries.  See <https://bugs.gnu.org/29212>.
    ("TZDIR" . ,(file-append tzdata "/share/zoneinfo"))
    ;; Tell 'modprobe' & co. where to look for modules.
    ("LINUX_MODULE_DIRECTORY" . "/run/booted-system/kernel/lib/modules")
    ;; These variables are honored by OpenSSL (libssl) and Git.
    ("SSL_CERT_DIR" . "/etc/ssl/certs")
    ("SSL_CERT_FILE" . "/etc/ssl/certs/ca-certificates.crt")
    ("GIT_SSL_CAINFO" . "/etc/ssl/certs/ca-certificates.crt")

    ;; 'GTK_DATA_PREFIX' must name one directory where GTK+ themes are
    ;; searched for.
    ("GTK_DATA_PREFIX" . "/run/current-system/profile")

    ;; By default, applications that use D-Bus, such as Emacs, abort at startup
    ;; when /etc/machine-id is missing.  Make sure these warnings are non-fatal.
    ("DBUS_FATAL_WARNINGS" . "0")

    ;; XXX: Normally we wouldn't need to do this, but our glibc@2.23 package
    ;; used to look things up in 'PREFIX/lib/locale' instead of
    ;; '/run/current-system/locale' as was intended.  Keep this hack around so
    ;; that people who still have glibc@2.23-using packages in their profiles
    ;; can use them correctly.
    ;; TODO: Remove when glibc@2.23 is long gone.
    ("GUIX_LOCPATH" . "/run/current-system/locale")))

(define %setuid-programs
  ;; Default set of setuid-root programs.
  (let ((shadow (@ (gnu packages admin) shadow)))
    (list (file-append shadow "/bin/passwd")
          (file-append shadow "/bin/su")
          (file-append shadow "/bin/newuidmap")
          (file-append shadow "/bin/newgidmap")
          (file-append inetutils "/bin/ping")
          (file-append inetutils "/bin/ping6")
          (file-append sudo "/bin/sudo")
          (file-append sudo "/bin/sudoedit")
          (file-append fuse "/bin/fusermount"))))

(define %sudoers-specification
  ;; Default /etc/sudoers contents: 'root' and all members of the 'wheel'
  ;; group can do anything.  See
  ;; <http://www.sudo.ws/sudo/man/1.8.10/sudoers.man.html>.
  ;; TODO: Add a declarative API.
  (plain-file "sudoers" "\
root ALL=(ALL) ALL
%wheel ALL=(ALL) ALL\n"))

(define* (operating-system-activation-script os)
  "Return the activation script for OS---i.e., the code that \"activates\" the
stateful part of OS, including user accounts and groups, special directories,
etc."
  (let* ((services   (operating-system-services os))
         (activation (fold-services services
                                    #:target-type activation-service-type)))
    (activation-service->script activation)))

(define* (operating-system-boot-script os)
  "Return the boot script for OS---i.e., the code started by the initrd once
we're running in the final root."
  (let* ((services (operating-system-services os))
         (boot     (fold-services services #:target-type boot-service-type)))
    (service-value boot)))

(define (operating-system-user-accounts os)
  "Return the list of user accounts of OS."
  (let* ((services (operating-system-services os))
         (account  (fold-services services
                                  #:target-type account-service-type)))
    (filter user-account?
            (service-value account))))

(define (operating-system-shepherd-service-names os)
  "Return the list of Shepherd service names for OS."
  (append-map shepherd-service-provision
              (service-value
               (fold-services (operating-system-services os)
                              #:target-type
                              shepherd-root-service-type))))

(define* (operating-system-derivation os)
  "Return a derivation that builds OS."
  (let* ((services (operating-system-services os))
         (system   (fold-services services)))
    ;; SYSTEM contains the derivation as a monadic value.
    (service-value system)))

(define* (operating-system-profile os)
  "Return a derivation that builds the system profile of OS."
  (mlet* %store-monad
      ((services -> (operating-system-services os))
       (profile (fold-services services
                               #:target-type profile-service-type)))
    (match profile
      (("profile" profile)
       (return profile)))))

(define (operating-system-root-file-system os)
  "Return the root file system of OS."
  (find (lambda (fs)
          (string=? "/" (file-system-mount-point fs)))
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

  (make-initrd boot-file-systems
               #:linux (operating-system-kernel os)
               #:linux-modules
               (operating-system-initrd-modules os)
               #:mapped-devices mapped-devices
               #:keyboard-layout (operating-system-keyboard-layout os)))

(define (locale-name->definition* name)
  "Variant of 'locale-name->definition' that raises an error upon failure."
  (match (locale-name->definition name)
    (#f
     (raise (condition
             (&message
              (message (format #f (G_ "~a: invalid locale name") name))))))
    (def def)))

(define (operating-system-locale-directory os)
  "Return the directory containing the locales compiled for the definitions
listed in OS.  The C library expects to find it under
/run/current-system/locale."
  (define name
    (operating-system-locale os))

  (define definitions
    ;; While we're at it, check whether NAME is defined and add it if needed.
    (if (member name (map locale-definition-name
                          (operating-system-locale-definitions os)))
        (operating-system-locale-definitions os)
        (cons (locale-name->definition* name)
              (operating-system-locale-definitions os))))

  (locale-directory definitions
                    #:libcs (operating-system-locale-libcs os)))

(define (kernel->boot-label kernel)
  "Return a label for the bootloader menu entry that boots KERNEL."
  (cond ((package? kernel)
         (string-append "GNU with "
                        (string-titlecase (package-name kernel)) " "
                        (package-version kernel)))
        ((inferior-package? kernel)
         (string-append "GNU with "
                        (string-titlecase (inferior-package-name kernel)) " "
                        (inferior-package-version kernel)))
        (else "GNU")))

(define (store-file-system file-systems)
  "Return the file system object among FILE-SYSTEMS that contains the store."
  (match (filter (lambda (fs)
                   (and (file-system-mount? fs)
                        (not (memq 'bind-mount (file-system-flags fs)))
                        (string-prefix? (file-system-mount-point fs)
                                        (%store-prefix))))
                 file-systems)
    ((and candidates (head . tail))
     (reduce (lambda (fs1 fs2)
               (if (> (string-length (file-system-mount-point fs1))
                      (string-length (file-system-mount-point fs2)))
                   fs1
                   fs2))
             head
             candidates))))

(define (operating-system-store-file-system os)
  "Return the file system that contains the store of OS."
  (store-file-system (operating-system-file-systems os)))

(define* (operating-system-bootcfg os #:optional (old-entries '()))
  "Return the bootloader configuration file for OS.  Use OLD-ENTRIES,
a list of <menu-entry>, to populate the \"old entries\" menu."
  (let* ((root-fs         (operating-system-root-file-system os))
         (root-device     (file-system-device root-fs))
         (params          (operating-system-boot-parameters
                           os root-device
                           #:system-kernel-arguments? #t))
         (entry           (boot-parameters->menu-entry params))
         (bootloader-conf (operating-system-bootloader os)))
    (define generate-config-file
      (bootloader-configuration-file-generator
       (bootloader-configuration-bootloader bootloader-conf)))

    (generate-config-file bootloader-conf (list entry)
                          #:old-entries old-entries)))

(define* (operating-system-boot-parameters os root-device
                                           #:key system-kernel-arguments?)
  "Return a monadic <boot-parameters> record that describes the boot
parameters of OS.  When SYSTEM-KERNEL-ARGUMENTS? is true, add kernel arguments
such as '--root' and '--load' to <boot-parameters>."
  (let* ((initrd          (operating-system-initrd-file os))
         (store           (operating-system-store-file-system os))
         (bootloader      (bootloader-configuration-bootloader
                           (operating-system-bootloader os)))
         (bootloader-name (bootloader-name bootloader))
         (label           (kernel->boot-label (operating-system-kernel os))))
    (boot-parameters
     (label label)
     (root-device root-device)
     (kernel (operating-system-kernel-file os))
     (kernel-arguments
      (if system-kernel-arguments?
          (operating-system-kernel-arguments os root-device)
          (operating-system-user-kernel-arguments os)))
     (initrd initrd)
     (bootloader-name bootloader-name)
     (store-device (ensure-not-/dev (file-system-device store)))
     (store-mount-point (file-system-mount-point store)))))

(define (device->sexp device)
  "Serialize DEVICE as an sexp (really, as an object with a read syntax.)"
  (match device
    ((? uuid? uuid)
     `(uuid ,(uuid-type uuid) ,(uuid-bytevector uuid)))
    ((? file-system-label? label)
     `(file-system-label ,(file-system-label->string label)))
    (_
     device)))

(define* (operating-system-boot-parameters-file os
                                                #:key system-kernel-arguments?)
   "Return a file that describes the boot parameters of OS.  The primary use of
this file is the reconstruction of GRUB menu entries for old configurations.

When SYSTEM-KERNEL-ARGUMENTS? is true, add kernel arguments such as '--root'
and '--load' to the returned file (since the returned file is then usually
stored into the content-addressed \"system\" directory, it's usually not a
good idea to give it because the content hash would change by the content hash
being stored into the \"parameters\" file)."
   (let* ((root   (operating-system-root-file-system os))
          (device (file-system-device root))
          (params (operating-system-boot-parameters
                   os device
                   #:system-kernel-arguments?
                   system-kernel-arguments?)))
     (gexp->file "parameters"
                 #~(boot-parameters
                    (version 0)
                    (label #$(boot-parameters-label params))
                    (root-device
                     #$(device->sexp
                        (boot-parameters-root-device params)))
                    (kernel #$(boot-parameters-kernel params))
                    (kernel-arguments
                     #$(boot-parameters-kernel-arguments params))
                    (initrd #$(boot-parameters-initrd params))
                    (bootloader-name #$(boot-parameters-bootloader-name params))
                    (store
                     (device
                      #$(device->sexp (boot-parameters-store-device params)))
                     (mount-point #$(boot-parameters-store-mount-point params))))
                 #:set-load-path? #f)))

(define-gexp-compiler (operating-system-compiler (os <operating-system>)
                                                 system target)
  ((store-lift
    (lambda (store)
      ;; XXX: This is not super elegant but we can't pass SYSTEM and TARGET to
      ;; 'operating-system-derivation'.
      (run-with-store store (operating-system-derivation os)
                      #:system system
                      #:target target)))))

;;; system.scm ends here
