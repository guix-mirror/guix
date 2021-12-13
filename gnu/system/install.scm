;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Florian Pelz <pelzflorian@pelzflorian.de>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu system install)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system setuid)
  #:use-module (gnu bootloader u-boot)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix modules)
  #:use-module ((guix packages) #:select (package-version))
  #:use-module ((guix store) #:select (%store-prefix))
  #:use-module (gnu installer)
  #:use-module (gnu system locale)
  #:use-module (gnu services avahi)
  #:use-module (gnu services dbus)
  #:use-module (gnu services networking)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services ssh)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages xorg)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:export (installation-os
            a20-olinuxino-lime-installation-os
            a20-olinuxino-lime2-emmc-installation-os
            a20-olinuxino-micro-installation-os
            bananapi-m2-ultra-installation-os
            beaglebone-black-installation-os
            mx6cuboxi-installation-os
            nintendo-nes-classic-edition-installation-os
            novena-installation-os
            firefly-rk3399-installation-os
            pine64-plus-installation-os
            pinebook-installation-os
            rock64-installation-os
            rockpro64-installation-os
            rk3399-puma-installation-os
            wandboard-installation-os
            os-with-u-boot))

;;; Commentary:
;;;
;;; This module provides an 'operating-system' definition for use on images
;;; for USB sticks etc., for the installation of the GNU system.
;;;
;;; Code:


;;;
;;; Documentation service.
;;;

(define %installation-node-names
  ;; Translated name of the "System Installation" node of the manual.  Ideally
  ;; we'd extract it from the 'guix-manual' gettext domain, but that one is
  ;; usually not available at run time, hence this hack.
  '(("de" . "Systeminstallation")
    ("en" . "System Installation")
    ("es" . "Instalación del sistema")
    ("fr" . "Installation du système")
    ("ru" . "Установка системы")))

(define (log-to-info tty user)
  "Return a script that spawns the Info reader on the right section of the
manual."
  (program-file "log-to-info"
                #~(let* ((tty      (open-file #$(string-append "/dev/" tty)
                                              "r0+"))
                         (locale   (cadr (command-line)))
                         (language (string-take locale
                                                (string-index locale #\_)))
                         (infodir  "/run/current-system/profile/share/info")
                         (per-lang (string-append infodir "/guix." language
                                                  ".info.gz"))
                         (file     (if (file-exists? per-lang)
                                       per-lang
                                       (string-append infodir "/guix.info")))
                         (node     (or (assoc-ref '#$%installation-node-names
                                                  language)
                                       "System Installation")))
                    (redirect-port tty (current-output-port))
                    (redirect-port tty (current-error-port))
                    (redirect-port tty (current-input-port))

                    (let ((pw (getpwnam #$user)))
                      (setgid (passwd:gid pw))
                      (setuid (passwd:uid pw)))

                    ;; 'gunzip' is needed to decompress the doc.
                    (setenv "PATH" (string-append #$gzip "/bin"))

                    ;; Change this process' locale so that command-line
                    ;; arguments to 'info' are properly encoded.
                    (catch #t
                      (lambda ()
                        (setlocale LC_ALL locale)
                        (setenv "LC_ALL" locale))
                      (lambda _
                        ;; Sometimes LOCALE itself is not available.  In that
                        ;; case pick the one UTF-8 locale that's known to work
                        ;; instead of failing.
                        (setlocale LC_ALL "en_US.utf8")
                        (setenv "LC_ALL" "en_US.utf8")))

                    (execl #$(file-append info-reader "/bin/info")
                           "info" "-d" infodir "-f" file "-n" node))))

(define (documentation-shepherd-service tty)
  (list (shepherd-service
         (provision (list (symbol-append 'term- (string->symbol tty))))
         (requirement '(user-processes host-name udev virtual-terminal))
         (start #~(lambda* (#:optional (locale "en_US.utf8"))
                    (fork+exec-command
                     (list #$(log-to-info tty "documentation") locale)
                     #:environment-variables
                     `("GUIX_LOCPATH=/run/current-system/locale"
                       "TERM=linux"))))
         (stop #~(make-kill-destructor)))))

(define %documentation-users
  ;; User account for the Info viewer.
  (list (user-account (name "documentation")
                      (system? #t)
                      (group "nogroup")
                      (home-directory "/var/empty"))))

(define documentation-service-type
  ;; Documentation viewer service.
  (service-type (name 'documentation)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          documentation-shepherd-service)
                       (service-extension account-service-type
                                          (const %documentation-users))))
                (description "Run the Info reader on a tty.")))


(define %backing-directory
  ;; Sub-directory used as the backing store for copy-on-write.
  "/tmp/guix-inst")

(define cow-store-service-type
  (shepherd-service-type
   'cow-store
   (lambda _
     (define (import-module? module)
       ;; Since we don't use deduplication support in 'populate-store', don't
       ;; import (guix store deduplication) and its dependencies, which
       ;; includes Guile-Gcrypt.
       (and (guix-module-name? module)
            (not (equal? module '(guix store deduplication)))))

     (shepherd-service
      (requirement '(root-file-system user-processes))
      (provision '(cow-store))
      (documentation
       "Make the store copy-on-write, with writes going to \
the given target.")

      ;; This is meant to be explicitly started by the user.
      (auto-start? #f)

      (modules `((gnu build install)
                 ,@%default-modules))
      (start
       (with-imported-modules (source-module-closure
                               '((gnu build install))
                               #:select? import-module?)
         #~(case-lambda
             ((target)
              (mount-cow-store target #$%backing-directory)
              target)
             (else
              ;; Do nothing, and mark the service as stopped.
              #f))))
      (stop #~(lambda (target)
                ;; Delete the temporary directory, but leave everything
                ;; mounted as there may still be processes using it since
                ;; 'user-processes' doesn't depend on us.  The 'user-file-systems'
                ;; service will unmount TARGET eventually.
                (delete-file-recursively
                 (string-append target #$%backing-directory))))))
   (description "Make the store copy-on-write, with writes going to \
the given target.")))

(define (cow-store-service)
  "Return a service that makes the store copy-on-write, such that writes go to
the user's target storage device rather than on the RAM disk."
  ;; See <http://bugs.gnu.org/18061> for the initial report.
  (service cow-store-service-type 'mooooh!))


(define (/etc/configuration-files _)
  "Return a list of tuples representing configuration templates to add to
/etc."
  (define directory
    (computed-file "configuration-templates"
                   (with-imported-modules '((guix build utils))
                     #~(begin
                         (mkdir #$output)
                         (for-each (lambda (file target)
                                     (copy-file file
                                                (string-append #$output "/"
                                                               target)))
                                   '(#$(local-file "examples/bare-bones.tmpl")
                                     #$(local-file "examples/beaglebone-black.tmpl")
                                     #$(local-file "examples/desktop.tmpl")
                                     #$(local-file "examples/lightweight-desktop.tmpl"))
                                   '("bare-bones.scm"
                                     "beaglebone-black.scm"
                                     "desktop.scm"
                                     "lightweight-desktop.scm"))
                         #t))))

  `(("configuration" ,directory)))

(define configuration-template-service-type
  (service-type (name 'configuration-template)
                (extensions
                 (list (service-extension etc-service-type
                                          /etc/configuration-files)))))

(define %configuration-template-service
  (service configuration-template-service-type #t))


(define %nscd-minimal-caches
  ;; Minimal in-memory caching policy for nscd.
  (list (nscd-cache (database 'hosts)
                    (positive-time-to-live (* 3600 12))

                    ;; Do not cache lookup failures at all since they are
                    ;; quite likely (for instance when someone tries to ping a
                    ;; host before networking is functional.)
                    (negative-time-to-live 0)

                    (persistent? #f)
                    (max-database-size (* 5 (expt 2 20)))))) ;5 MiB


;; These define a service to load the uvesafb kernel module with the
;; appropriate options.  The GUI installer needs it when the machine does not
;; support Kernel Mode Setting.  Otherwise kmscon is missing /dev/fb0.
(define (uvesafb-shepherd-service _)
  (list (shepherd-service
         (documentation "Load the uvesafb kernel module if needed.")
         (provision '(maybe-uvesafb))
         (requirement '(file-systems))
         (start #~(lambda ()
                    ;; uvesafb is only supported on x86 and x86_64.
                    (or (not (and (string-suffix? "linux-gnu" %host-type)
                                  (or (string-prefix? "x86_64" %host-type)
                                      (string-prefix? "i686" %host-type))))
                        (file-exists? "/dev/fb0")
                        (invoke #+(file-append kmod "/bin/modprobe")
                                "uvesafb"
                                (string-append "v86d=" #$v86d "/sbin/v86d")
                                "mode_option=1024x768"))))
         (respawn? #f)
         (one-shot? #t))))

(define uvesafb-service-type
  (service-type
   (name 'uvesafb)
   (extensions
    (list (service-extension shepherd-root-service-type
                             uvesafb-shepherd-service)))
   (description
    "Load the @code{uvesafb} kernel module with the right options.")
   (default-value #t)))

(define %installation-services
  ;; List of services of the installation system.
  (let ((motd (plain-file "motd" "
\x1b[1;37mWelcome to the installation of GNU Guix!\x1b[0m

\x1b[2m\
Using this shell, you can carry out the installation process \"manually.\"
Access documentation at any time by pressing Alt-F2.\x1b[0m
")))
    (define (normal-tty tty)
      (mingetty-service (mingetty-configuration (tty tty)
                                                (auto-login "root")
                                                (login-pause? #t))))

    (define bare-bones-os
      (load "examples/bare-bones.tmpl"))

    (list (service virtual-terminal-service-type)

          (service kmscon-service-type
                   (kmscon-configuration
                    (virtual-terminal "tty1")
                    (login-program (installer-program))))

          (login-service (login-configuration
                          (motd motd)))

          ;; Documentation.  The manual is in UTF-8, but
          ;; 'console-font-service' sets up Unicode support and loads a font
          ;; with all the useful glyphs like em dash and quotation marks.
          (service documentation-service-type "tty2")

          ;; Documentation add-on.
          %configuration-template-service

          ;; A bunch of 'root' ttys.
          (normal-tty "tty3")
          (normal-tty "tty4")
          (normal-tty "tty5")
          (normal-tty "tty6")

          ;; The usual services.
          (syslog-service)

          ;; Use the Avahi daemon to discover substitute servers on the local
          ;; network.  It can be faster than fetching from remote servers.
          (service avahi-service-type)

          ;; The build daemon.  Register the default substitute server key(s)
          ;; as trusted to allow the installation process to use substitutes by
          ;; default.
          (service guix-service-type
                   (guix-configuration (authorize-key? #t)))

          ;; Start udev so that useful device nodes are available.
          ;; Use device-mapper rules for cryptsetup & co; enable the CRDA for
          ;; regulations-compliant WiFi access.
          (udev-service #:rules (list lvm2 crda))

          ;; Add the 'cow-store' service, which users have to start manually
          ;; since it takes the installation directory as an argument.
          (cow-store-service)

          ;; Install Unicode support and a suitable font.
          (service console-font-service-type
                   (map (match-lambda
                          ("tty2"
                           ;; Use a font that contains characters such as
                           ;; curly quotes as found in the manual.
                           '("tty2" . "LatGrkCyr-8x16"))
                          (tty
                           ;; Use a font that doesn't have more than 256
                           ;; glyphs so that we can use colors with varying
                           ;; brightness levels (see note in setfont(8)).
                           `(,tty . "lat9u-16")))
                        '("tty1" "tty2" "tty3" "tty4" "tty5" "tty6")))

          ;; To facilitate copy/paste.
          (service gpm-service-type)

          ;; Add an SSH server to facilitate remote installs.
          (service openssh-service-type
                   (openssh-configuration
                    (port-number 22)
                    (permit-root-login #t)
                    ;; The root account is passwordless, so make sure
                    ;; a password is set before allowing logins.
                    (allow-empty-passwords? #f)
                    (password-authentication? #t)

                    ;; Don't start it upfront.
                    (%auto-start? #f)))

          ;; Since this is running on a USB stick with a overlayfs as the root
          ;; file system, use an appropriate cache configuration.
          (nscd-service (nscd-configuration
                         (caches %nscd-minimal-caches)))

          ;; Having /bin/sh is a good idea.  In particular it allows Tramp
          ;; connections to this system to work.
          (service special-files-service-type
                   `(("/bin/sh" ,(file-append bash "/bin/sh"))))

          ;; Loopback device, needed by OpenSSH notably.
          (service static-networking-service-type
                   (list %loopback-static-networking))

          (service wpa-supplicant-service-type)
          (dbus-service)
          (service connman-service-type
                   (connman-configuration
                    (disable-vpn? #t)))

          ;; Keep a reference to BARE-BONES-OS to make sure it can be
          ;; installed without downloading/building anything.  Also keep the
          ;; things needed by 'profile-derivation' to minimize the amount of
          ;; download.
          (service gc-root-service-type
                   (append
                    (list bare-bones-os
                          glibc-utf8-locales
                          texinfo
                          guile-3.0)
                    %default-locale-libcs))

          ;; Machines without Kernel Mode Setting (those with many old and
          ;; current AMD GPUs, SiS GPUs, ...) need uvesafb to show the GUI
          ;; installer.  Some may also need a kernel parameter like nomodeset
          ;; or vga=793, but we leave that for the user to specify in GRUB.
          (service uvesafb-service-type))))

(define %issue
  ;; Greeting.
  "
\x1b[1;37mThis is an installation image of the GNU system.  Welcome.\x1b[0m

\x1b[1;33mUse Alt-F2 for documentation.\x1b[0m
")

(define installation-os
  ;; The operating system used on installation images for USB sticks etc.
  (operating-system
    (host-name "gnu")
    (timezone "Europe/Paris")
    (locale "en_US.utf8")
    (name-service-switch %mdns-host-lookup-nss)
    (bootloader (bootloader-configuration
                 (bootloader grub-bootloader)
                 (targets '("/dev/sda"))))
    (label (string-append "GNU Guix installation "
                          (package-version guix)))

    ;; XXX: The AMD Radeon driver is reportedly broken, which makes kmscon
    ;; non-functional:
    ;; <https://lists.gnu.org/archive/html/guix-devel/2019-03/msg00441.html>.
    ;; Thus, blacklist it.
    (kernel-arguments '("quiet" "modprobe.blacklist=radeon"))

    (file-systems
     ;; Note: the disk image build code overrides this root file system with
     ;; the appropriate one.
     (cons* (file-system
              (mount-point "/")
              (device (file-system-label "Guix_image"))
              (type "ext4"))

            ;; Make /tmp a tmpfs instead of keeping the overlayfs.  This
            ;; originally was used for unionfs because FUSE creates
            ;; '.fuse_hiddenXYZ' files for each open file, and this confuses
            ;; Guix's test suite, for instance (see
            ;; <http://bugs.gnu.org/23056>).  We keep this for overlayfs to be
            ;; on the safe side.
            (file-system
              (mount-point "/tmp")
              (device "none")
              (type "tmpfs")
              (check? #f))

            ;; XXX: This should be %BASE-FILE-SYSTEMS but we don't need
            ;; elogind's cgroup file systems.
            (list %pseudo-terminal-file-system
                  %shared-memory-file-system
                  %efivars-file-system
                  %immutable-store)))

    (users (list (user-account
                  (name "guest")
                  (group "users")
                  (supplementary-groups '("wheel")) ; allow use of sudo
                  (password "")
                  (comment "Guest of GNU"))))

    (issue %issue)
    (services %installation-services)

    ;; We don't need setuid programs, except for 'passwd', which can be handy
    ;; if one is to allow remote SSH login to the machine being installed.
    (setuid-programs (list (setuid-program
                            (program (file-append shadow "/bin/passwd")))))

    (pam-services
     ;; Explicitly allow for empty passwords.
     (base-pam-services #:allow-empty-passwords? #t))

    (packages (append
                (list glibc         ; for 'tzselect' & co.
                      fontconfig
                      font-dejavu font-gnu-unifont
                      grub          ; mostly so xrefs to its manual work
                      nss-certs)    ; To access HTTPS, use git, etc.
                %base-packages-disk-utilities
                %base-packages))))

(define* (os-with-u-boot os board #:key (bootloader-target "/dev/mmcblk0")
                         (triplet "arm-linux-gnueabihf"))
  "Given OS, amend it with the u-boot bootloader for BOARD,
installed to BOOTLOADER-TARGET (a drive), compiled for TRIPLET.

If you want a serial console, make sure to specify one in your
operating-system's kernel-arguments (\"console=ttyS0\" or similar)."
  (operating-system (inherit os)
    (bootloader (bootloader-configuration
                 (bootloader (bootloader (inherit u-boot-bootloader)
                              (package (make-u-boot-package board triplet))))
                 (targets (list bootloader-target))))))

(define* (embedded-installation-os bootloader bootloader-target tty
                                   #:key (extra-modules '()))
  "Return an installation os for embedded systems.
The initrd gets the extra modules EXTRA-MODULES.
A getty is provided on TTY.
The bootloader BOOTLOADER is installed to BOOTLOADER-TARGET."
  (operating-system
    (inherit installation-os)
    (bootloader (bootloader-configuration
                 (bootloader bootloader)
                 (targets (list bootloader-target))))
    (kernel linux-libre)
    (kernel-arguments
     (cons (string-append "console=" tty)
           (operating-system-user-kernel-arguments installation-os)))
    (initrd-modules (append extra-modules %base-initrd-modules))))

(define beaglebone-black-installation-os
  (embedded-installation-os u-boot-beaglebone-black-bootloader
                            "/dev/sda"
                            "ttyO0"
                            #:extra-modules
                            ;; This module is required to mount the sd card.
                            '("omap_hsmmc")))


(define a20-olinuxino-lime-installation-os
  (embedded-installation-os u-boot-a20-olinuxino-lime-bootloader
                            "/dev/mmcblk0" ; SD card storage
                            "ttyS0"))

(define a20-olinuxino-lime2-emmc-installation-os
  (embedded-installation-os u-boot-a20-olinuxino-lime2-bootloader
                            "/dev/mmcblk1" ; eMMC storage
                            "ttyS0"))

(define a20-olinuxino-micro-installation-os
  (embedded-installation-os u-boot-a20-olinuxino-micro-bootloader
                            "/dev/mmcblk0" ; SD card storage
                            "ttyS0"))

(define bananapi-m2-ultra-installation-os
  (embedded-installation-os u-boot-bananapi-m2-ultra-bootloader
                            "/dev/mmcblk1" ; eMMC storage
                            "ttyS0"))

(define firefly-rk3399-installation-os
  (embedded-installation-os u-boot-firefly-rk3399-bootloader
                            "/dev/mmcblk0" ; SD card/eMMC (SD priority) storage
                            "ttyS2")) ; UART2 connected on the Pi2 bus

(define mx6cuboxi-installation-os
  (embedded-installation-os u-boot-mx6cuboxi-bootloader
                            "/dev/mmcblk0" ; SD card storage
                            "ttymxc0"))

(define novena-installation-os
  (embedded-installation-os u-boot-novena-bootloader
                            "/dev/mmcblk1" ; SD card storage
                            "ttymxc1"))

(define nintendo-nes-classic-edition-installation-os
  (embedded-installation-os u-boot-nintendo-nes-classic-edition-bootloader
                            "/dev/mmcblk0" ; SD card (solder it yourself)
                            "ttyS0"))

(define pine64-plus-installation-os
  (embedded-installation-os u-boot-pine64-plus-bootloader
                            "/dev/mmcblk0" ; SD card storage
                            "ttyS0"))

(define pinebook-installation-os
  (embedded-installation-os u-boot-pinebook-bootloader
                            "/dev/mmcblk0" ; SD card storage
                            "ttyS0"))

(define rock64-installation-os
  (embedded-installation-os u-boot-rock64-rk3328-bootloader
                            "/dev/mmcblk0" ; SD card/eMMC (SD priority) storage
                            "ttyS2")) ; UART2 connected on the Pi2 bus

(define rockpro64-installation-os
  (embedded-installation-os u-boot-rockpro64-rk3399-bootloader
                            "/dev/mmcblk0" ; SD card/eMMC (SD priority) storage
                            "ttyS2")) ; UART2 connected on the Pi2 bus

(define rk3399-puma-installation-os
  (embedded-installation-os u-boot-puma-rk3399-bootloader
                            "/dev/mmcblk0" ; SD card storage
                            "ttyS0"))

(define wandboard-installation-os
  (embedded-installation-os u-boot-wandboard-bootloader
                            "/dev/mmcblk0" ; SD card storage
                            "ttymxc0"))

;; Return the default os here so 'guix system' can consume it directly.
installation-os

;;; install.scm ends here
