;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu tests install)
  #:use-module (gnu)
  #:use-module (gnu bootloader extlinux)
  #:use-module (gnu tests)
  #:use-module (gnu tests base)
  #:use-module (gnu system)
  #:use-module (gnu system install)
  #:use-module (gnu system vm)
  #:use-module ((gnu build vm) #:select (qemu-command))
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages ocr)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages virtualization)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix grafts)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:export (%test-installed-os
            %test-installed-extlinux-os
            %test-iso-image-installer
            %test-separate-store-os
            %test-separate-home-os
            %test-raid-root-os
            %test-encrypted-root-os
            %test-btrfs-root-os))

;;; Commentary:
;;;
;;; Test the installation of Guix using the documented approach at the
;;; command line.
;;;
;;; Code:

(define-os-with-source (%minimal-os %minimal-os-source)
  ;; The OS we want to install.
  (use-modules (gnu) (gnu tests) (srfi srfi-1))

  (operating-system
    (host-name "liberigilo")
    (timezone "Europe/Paris")
    (locale "en_US.UTF-8")

    (bootloader (bootloader-configuration
                 (bootloader grub-bootloader)
                 (target "/dev/vdb")))
    (kernel-arguments '("console=ttyS0"))
    (file-systems (cons (file-system
                          (device (file-system-label "my-root"))
                          (mount-point "/")
                          (type "ext4"))
                        %base-file-systems))
    (users (cons (user-account
                  (name "alice")
                  (comment "Bob's sister")
                  (group "users")
                  (supplementary-groups '("wheel" "audio" "video"))
                  (home-directory "/home/alice"))
                 %base-user-accounts))
    (services (cons (service marionette-service-type
                             (marionette-configuration
                              (imported-modules '((gnu services herd)
                                                  (guix combinators)))))
                    %base-services))))

(define (operating-system-add-packages os packages)
  "Append PACKAGES to OS packages list."
  (operating-system
    (inherit os)
    (packages (append packages (operating-system-packages os)))))

(define-os-with-source (%minimal-extlinux-os
                        %minimal-extlinux-os-source)
  (use-modules (gnu) (gnu tests) (gnu bootloader extlinux)
               (srfi srfi-1))

  (operating-system
    (host-name "liberigilo")
    (timezone "Europe/Paris")
    (locale "en_US.UTF-8")

    (bootloader (bootloader-configuration
                 (bootloader extlinux-bootloader-gpt)
                 (target "/dev/vdb")))
    (kernel-arguments '("console=ttyS0"))
    (file-systems (cons (file-system
                          (device (file-system-label "my-root"))
                          (mount-point "/")
                          (type "ext4"))
                        %base-file-systems))
    (services (cons (service marionette-service-type
                             (marionette-configuration
                              (imported-modules '((gnu services herd)
                                                  (guix combinators)))))
                    %base-services))))

(define (operating-system-with-current-guix os)
  "Return a variant of OS that uses the current Guix."
  (operating-system
    (inherit os)
    (services (modify-services (operating-system-user-services os)
                (guix-service-type config =>
                                   (guix-configuration
                                    (inherit config)
                                    (guix (current-guix))))))))

(define (operating-system-with-gc-roots os roots)
  "Return a variant of OS where ROOTS are registered as GC roots."
  (operating-system
    (inherit os)

    ;; We use this procedure for the installation OS, which already defines GC
    ;; roots.  Add ROOTS to those.
    (services (cons (simple-service 'extra-root
                                    gc-root-service-type roots)
                    (operating-system-user-services os)))))


(define MiB (expt 2 20))

(define %simple-installation-script
  ;; Shell script of a simple installation.
  "\
. /etc/profile
set -e -x
guix --version

export GUIX_BUILD_OPTIONS=--no-grafts
guix build isc-dhcp
parted --script /dev/vdb mklabel gpt \\
  mkpart primary ext2 1M 3M \\
  mkpart primary ext2 3M 1.2G \\
  set 1 boot on \\
  set 1 bios_grub on
mkfs.ext4 -L my-root /dev/vdb2
mount /dev/vdb2 /mnt
df -h /mnt
herd start cow-store /mnt
mkdir /mnt/etc
cp /etc/target-config.scm /mnt/etc/config.scm
guix system init /mnt/etc/config.scm /mnt --no-substitutes
sync
reboot\n")

(define %extlinux-gpt-installation-script
  ;; Shell script of a simple installation.
  ;; As syslinux 6.0.3 does not handle 64bits ext4 partitions,
  ;; we make sure to pass -O '^64bit' to mkfs.
  "\
. /etc/profile
set -e -x
guix --version

export GUIX_BUILD_OPTIONS=--no-grafts
guix build isc-dhcp
parted --script /dev/vdb mklabel gpt \\
  mkpart ext2 1M 1.2G \\
  set 1 legacy_boot on
mkfs.ext4 -L my-root -O '^64bit' /dev/vdb1
mount /dev/vdb1 /mnt
df -h /mnt
herd start cow-store /mnt
mkdir /mnt/etc
cp /etc/target-config.scm /mnt/etc/config.scm
guix system init /mnt/etc/config.scm /mnt --no-substitutes
sync
reboot\n")

(define* (run-install target-os target-os-source
                      #:key
                      (script %simple-installation-script)
                      (packages '())
                      (os (marionette-operating-system
                           (operating-system
                             ;; Since the image has no network access, use the
                             ;; current Guix so the store items we need are in
                             ;; the image and add packages provided.
                             (inherit (operating-system-add-packages
                                       (operating-system-with-current-guix
                                        installation-os)
                                       packages))
                             (kernel-arguments '("console=ttyS0")))
                           #:imported-modules '((gnu services herd)
                                                (guix combinators))))
                      (installation-disk-image-file-system-type "ext4")
                      (target-size (* 2200 MiB)))
  "Run SCRIPT (a shell script following the system installation procedure) in
OS to install TARGET-OS.  Return a VM image of TARGET-SIZE bytes containing
the installed system.  The packages specified in PACKAGES will be appended to
packages defined in installation-os."

  (mlet* %store-monad ((_      (set-grafting #f))
                       (system (current-system))
                       (target (operating-system-derivation target-os))

                       ;; Since the installation system has no network access,
                       ;; we cheat a little bit by adding TARGET to its GC
                       ;; roots.  This way, we know 'guix system init' will
                       ;; succeed.
                       (image  (system-disk-image
                                (operating-system-with-gc-roots
                                 os (list target))
                                #:disk-image-size 'guess
                                #:file-system-type
                                installation-disk-image-file-system-type)))
    (define install
      (with-imported-modules '((guix build utils)
                               (gnu build marionette))
        #~(begin
            (use-modules (guix build utils)
                         (gnu build marionette))

            (set-path-environment-variable "PATH" '("bin")
                                           (list #$qemu-minimal))

            (system* "qemu-img" "create" "-f" "qcow2"
                     #$output #$(number->string target-size))

            (define marionette
              (make-marionette
               `(,(which #$(qemu-command system))
                 "-no-reboot"
                 "-m" "800"
                 #$@(cond
                     ((string=? "ext4" installation-disk-image-file-system-type)
                      #~("-drive"
                         ,(string-append "file=" #$image
                                         ",if=virtio,readonly")))
                     ((string=? "iso9660" installation-disk-image-file-system-type)
                      #~("-cdrom" #$image))
                     (else
                      (error
                       "unsupported installation-disk-image-file-system-type:"
                       installation-disk-image-file-system-type)))
                 "-drive"
                 ,(string-append "file=" #$output ",if=virtio")
                 ,@(if (file-exists? "/dev/kvm")
                       '("-enable-kvm")
                       '()))))

            (pk 'uname (marionette-eval '(uname) marionette))

            ;; Wait for tty1.
            (marionette-eval '(begin
                                (use-modules (gnu services herd))
                                (start 'term-tty1))
                             marionette)

            (marionette-eval '(call-with-output-file "/etc/target-config.scm"
                                (lambda (port)
                                  (write '#$target-os-source port)))
                             marionette)

            (exit (marionette-eval '(zero? (system #$script))
                                   marionette)))))

    (gexp->derivation "installation" install)))

(define* (qemu-command/writable-image image #:key (memory-size 256))
  "Return as a monadic value the command to run QEMU on a writable copy of
IMAGE, a disk image.  The QEMU VM is has access to MEMORY-SIZE MiB of RAM."
  (mlet %store-monad ((system (current-system)))
    (return #~(let ((image #$image))
                ;; First we need a writable copy of the image.
                (format #t "creating writable image from '~a'...~%" image)
                (unless (zero? (system* #+(file-append qemu-minimal
                                                       "/bin/qemu-img")
                                        "create" "-f" "qcow2"
                                        "-o"
                                        (string-append "backing_file=" image)
                                        "disk.img"))
                  (error "failed to create writable QEMU image" image))

                (chmod "disk.img" #o644)
                `(,(string-append #$qemu-minimal "/bin/"
                                  #$(qemu-command system))
                  ,@(if (file-exists? "/dev/kvm")
                        '("-enable-kvm")
                        '())
                  "-no-reboot" "-m" #$(number->string memory-size)
                  "-drive" "file=disk.img,if=virtio")))))

(define %test-installed-os
  (system-test
   (name "installed-os")
   (description
    "Test basic functionality of an OS installed like one would do by hand.
This test is expensive in terms of CPU and storage usage since we need to
build (current-guix) and then store a couple of full system images.")
   (value
    (mlet* %store-monad ((image   (run-install %minimal-os %minimal-os-source))
                         (command (qemu-command/writable-image image)))
      (run-basic-test %minimal-os command
                      "installed-os")))))

(define %test-installed-extlinux-os
  (system-test
   (name "installed-extlinux-os")
   (description
    "Test basic functionality of an OS booted with an extlinux bootloader.  As
per %test-installed-os, this test is expensive in terms of CPU and storage.")
   (value
    (mlet* %store-monad ((image (run-install %minimal-extlinux-os
                                             %minimal-extlinux-os-source
                                             #:packages
                                             (list syslinux)
                                             #:script
                                             %extlinux-gpt-installation-script))
                         (command (qemu-command/writable-image image)))
      (run-basic-test %minimal-extlinux-os command
                      "installed-extlinux-os")))))


;;;
;;; Installation through an ISO image.
;;;

(define-os-with-source (%minimal-os-on-vda %minimal-os-on-vda-source)
  ;; The OS we want to install.
  (use-modules (gnu) (gnu tests) (srfi srfi-1))

  (operating-system
    (host-name "liberigilo")
    (timezone "Europe/Paris")
    (locale "en_US.UTF-8")

    (bootloader (bootloader-configuration
                 (bootloader grub-bootloader)
                 (target "/dev/vda")))
    (kernel-arguments '("console=ttyS0"))
    (file-systems (cons (file-system
                          (device (file-system-label "my-root"))
                          (mount-point "/")
                          (type "ext4"))
                        %base-file-systems))
    (users (cons (user-account
                  (name "alice")
                  (comment "Bob's sister")
                  (group "users")
                  (supplementary-groups '("wheel" "audio" "video"))
                  (home-directory "/home/alice"))
                 %base-user-accounts))
    (services (cons (service marionette-service-type
                             (marionette-configuration
                              (imported-modules '((gnu services herd)
                                                  (guix combinators)))))
                    %base-services))))

(define %simple-installation-script-for-/dev/vda
  ;; Shell script of a simple installation.
  "\
. /etc/profile
set -e -x
guix --version

export GUIX_BUILD_OPTIONS=--no-grafts
guix build isc-dhcp
parted --script /dev/vda mklabel gpt \\
  mkpart primary ext2 1M 3M \\
  mkpart primary ext2 3M 1.2G \\
  set 1 boot on \\
  set 1 bios_grub on
mkfs.ext4 -L my-root /dev/vda2
mount /dev/vda2 /mnt
df -h /mnt
herd start cow-store /mnt
mkdir /mnt/etc
cp /etc/target-config.scm /mnt/etc/config.scm
guix system init /mnt/etc/config.scm /mnt --no-substitutes
sync
reboot\n")

(define %test-iso-image-installer
  (system-test
   (name "iso-image-installer")
   (description
    "")
   (value
    (mlet* %store-monad ((image   (run-install
                                   %minimal-os-on-vda
                                   %minimal-os-on-vda-source
                                   #:script
                                   %simple-installation-script-for-/dev/vda
                                   #:installation-disk-image-file-system-type
                                   "iso9660"))
                         (command (qemu-command/writable-image image)))
      (run-basic-test %minimal-os-on-vda command name)))))


;;;
;;; Separate /home.
;;;

(define-os-with-source (%separate-home-os %separate-home-os-source)
  ;; The OS we want to install.
  (use-modules (gnu) (gnu tests) (srfi srfi-1))

  (operating-system
    (host-name "liberigilo")
    (timezone "Europe/Paris")
    (locale "en_US.utf8")

    (bootloader (bootloader-configuration
                 (bootloader grub-bootloader)
                 (target "/dev/vdb")))
    (kernel-arguments '("console=ttyS0"))
    (file-systems (cons* (file-system
                           (device (file-system-label "my-root"))
                           (mount-point "/")
                           (type "ext4"))
                         (file-system
                           (device "none")
                           (mount-point "/home")
                           (type "tmpfs"))
                         %base-file-systems))
    (users (cons* (user-account
                   (name "alice")
                   (group "users")
                   (home-directory "/home/alice"))
                  (user-account
                   (name "charlie")
                   (group "users")
                   (home-directory "/home/charlie"))
                  %base-user-accounts))
    (services (cons (service marionette-service-type
                             (marionette-configuration
                              (imported-modules '((gnu services herd)
                                                  (guix combinators)))))
                    %base-services))))

(define %test-separate-home-os
  (system-test
   (name "separate-home-os")
   (description
    "Test basic functionality of an installed OS with a separate /home
partition.  In particular, home directories must be correctly created (see
<https://bugs.gnu.org/21108>).")
   (value
    (mlet* %store-monad ((image   (run-install %separate-home-os
                                               %separate-home-os-source
                                               #:script
                                               %simple-installation-script))
                         (command (qemu-command/writable-image image)))
      (run-basic-test %separate-home-os command "separate-home-os")))))


;;;
;;; Separate /gnu/store partition.
;;;

(define-os-with-source (%separate-store-os %separate-store-os-source)
  ;; The OS we want to install.
  (use-modules (gnu) (gnu tests) (srfi srfi-1))

  (operating-system
    (host-name "liberigilo")
    (timezone "Europe/Paris")
    (locale "en_US.UTF-8")

    (bootloader (bootloader-configuration
                 (bootloader grub-bootloader)
                 (target "/dev/vdb")))
    (kernel-arguments '("console=ttyS0"))
    (file-systems (cons* (file-system
                           (device (file-system-label "root-fs"))
                           (mount-point "/")
                           (type "ext4"))
                         (file-system
                           (device (file-system-label "store-fs"))
                           (mount-point "/gnu")
                           (type "ext4"))
                         %base-file-systems))
    (users %base-user-accounts)
    (services (cons (service marionette-service-type
                             (marionette-configuration
                              (imported-modules '((gnu services herd)
                                                  (guix combinators)))))
                    %base-services))))

(define %separate-store-installation-script
  ;; Installation with a separate /gnu partition.
  "\
. /etc/profile
set -e -x
guix --version

export GUIX_BUILD_OPTIONS=--no-grafts
guix build isc-dhcp
parted --script /dev/vdb mklabel gpt \\
  mkpart primary ext2 1M 3M \\
  mkpart primary ext2 3M 100M \\
  mkpart primary ext2 100M 1.2G \\
  set 1 boot on \\
  set 1 bios_grub on
mkfs.ext4 -L root-fs /dev/vdb2
mkfs.ext4 -L store-fs /dev/vdb3
mount /dev/vdb2 /mnt
mkdir /mnt/gnu
mount /dev/vdb3 /mnt/gnu
df -h /mnt
herd start cow-store /mnt
mkdir /mnt/etc
cp /etc/target-config.scm /mnt/etc/config.scm
guix system init /mnt/etc/config.scm /mnt --no-substitutes
sync
reboot\n")

(define %test-separate-store-os
  (system-test
   (name "separate-store-os")
   (description
    "Test basic functionality of an OS installed like one would do by hand,
where /gnu lives on a separate partition.")
   (value
    (mlet* %store-monad ((image   (run-install %separate-store-os
                                               %separate-store-os-source
                                               #:script
                                               %separate-store-installation-script))
                         (command (qemu-command/writable-image image)))
      (run-basic-test %separate-store-os command "separate-store-os")))))


;;;
;;; RAID root device.
;;;

(define-os-with-source (%raid-root-os %raid-root-os-source)
  ;; An OS whose root partition is a RAID partition.
  (use-modules (gnu) (gnu tests))

  (operating-system
    (host-name "raidified")
    (timezone "Europe/Paris")
    (locale "en_US.utf8")

    (bootloader (bootloader-configuration
                 (bootloader grub-bootloader)
                 (target "/dev/vdb")))
    (kernel-arguments '("console=ttyS0"))

    ;; Add a kernel module for RAID-0 (aka. "stripe").
    (initrd-modules (cons "raid0" %base-initrd-modules))

    (mapped-devices (list (mapped-device
                           (source (list "/dev/vda2" "/dev/vda3"))
                           (target "/dev/md0")
                           (type raid-device-mapping))))
    (file-systems (cons (file-system
                          (device (file-system-label "root-fs"))
                          (mount-point "/")
                          (type "ext4")
                          (dependencies mapped-devices))
                        %base-file-systems))
    (users %base-user-accounts)
    (services (cons (service marionette-service-type
                             (marionette-configuration
                              (imported-modules '((gnu services herd)
                                                  (guix combinators)))))
                    %base-services))))

(define %raid-root-installation-script
  ;; Installation with a separate /gnu partition.  See
  ;; <https://raid.wiki.kernel.org/index.php/RAID_setup> for more on RAID and
  ;; mdadm.
  "\
. /etc/profile
set -e -x
guix --version

export GUIX_BUILD_OPTIONS=--no-grafts
parted --script /dev/vdb mklabel gpt \\
  mkpart primary ext2 1M 3M \\
  mkpart primary ext2 3M 600M \\
  mkpart primary ext2 600M 1200M \\
  set 1 boot on \\
  set 1 bios_grub on
mdadm --create /dev/md0 --verbose --level=stripe --raid-devices=2 \\
  /dev/vdb2 /dev/vdb3
mkfs.ext4 -L root-fs /dev/md0
mount /dev/md0 /mnt
df -h /mnt
herd start cow-store /mnt
mkdir /mnt/etc
cp /etc/target-config.scm /mnt/etc/config.scm
guix system init /mnt/etc/config.scm /mnt --no-substitutes
sync
reboot\n")

(define %test-raid-root-os
  (system-test
   (name "raid-root-os")
   (description
    "Test functionality of an OS installed with a RAID root partition managed
by 'mdadm'.")
   (value
    (mlet* %store-monad ((image   (run-install %raid-root-os
                                               %raid-root-os-source
                                               #:script
                                               %raid-root-installation-script
                                               #:target-size (* 1300 MiB)))
                         (command (qemu-command/writable-image image)))
      (run-basic-test %raid-root-os
                      `(,@command) "raid-root-os")))))


;;;
;;; LUKS-encrypted root file system.
;;;

(define-os-with-source (%encrypted-root-os %encrypted-root-os-source)
  ;; The OS we want to install.
  (use-modules (gnu) (gnu tests) (srfi srfi-1))

  (operating-system
    (host-name "liberigilo")
    (timezone "Europe/Paris")
    (locale "en_US.UTF-8")

    (bootloader (bootloader-configuration
                 (bootloader grub-bootloader)
                 (target "/dev/vdb")))

    ;; Note: Do not pass "console=ttyS0" so we can use our passphrase prompt
    ;; detection logic in 'enter-luks-passphrase'.

    (mapped-devices (list (mapped-device
                           (source (uuid "12345678-1234-1234-1234-123456789abc"))
                           (target "the-root-device")
                           (type luks-device-mapping))))
    (file-systems (cons (file-system
                          (device "/dev/mapper/the-root-device")
                          (mount-point "/")
                          (type "ext4"))
                        %base-file-systems))
    (users (cons (user-account
                  (name "charlie")
                  (group "users")
                  (home-directory "/home/charlie")
                  (supplementary-groups '("wheel" "audio" "video")))
                 %base-user-accounts))
    (services (cons (service marionette-service-type
                             (marionette-configuration
                              (imported-modules '((gnu services herd)
                                                  (guix combinators)))))
                    %base-services))))

(define %encrypted-root-installation-script
  ;; Shell script of a simple installation.
  "\
. /etc/profile
set -e -x
guix --version

export GUIX_BUILD_OPTIONS=--no-grafts
ls -l /run/current-system/gc-roots
parted --script /dev/vdb mklabel gpt \\
  mkpart primary ext2 1M 3M \\
  mkpart primary ext2 3M 1.2G \\
  set 1 boot on \\
  set 1 bios_grub on
echo -n thepassphrase | \\
  cryptsetup luksFormat --uuid=12345678-1234-1234-1234-123456789abc -q /dev/vdb2 -
echo -n thepassphrase | \\
  cryptsetup open --type luks --key-file - /dev/vdb2 the-root-device
mkfs.ext4 -L my-root /dev/mapper/the-root-device
mount LABEL=my-root /mnt
herd start cow-store /mnt
mkdir /mnt/etc
cp /etc/target-config.scm /mnt/etc/config.scm
guix system build /mnt/etc/config.scm
guix system init /mnt/etc/config.scm /mnt --no-substitutes
sync
reboot\n")

(define (enter-luks-passphrase marionette)
  "Return a gexp to be inserted in the basic system test running on MARIONETTE
to enter the LUKS passphrase."
  (let ((ocrad (file-append ocrad "/bin/ocrad")))
    #~(begin
        (define (passphrase-prompt? text)
          (string-contains (pk 'screen-text text) "Enter pass"))

        (define (bios-boot-screen? text)
          ;; Return true if TEXT corresponds to the boot screen, before GRUB's
          ;; menu.
          (string-prefix? "SeaBIOS" text))

        (test-assert "enter LUKS passphrase for GRUB"
          (begin
            ;; At this point we have no choice but to use OCR to determine
            ;; when the passphrase should be entered.
            (wait-for-screen-text #$marionette passphrase-prompt?
                                  #:ocrad #$ocrad)
            (marionette-type "thepassphrase\n" #$marionette)

            ;; Now wait until we leave the boot screen.  This is necessary so
            ;; we can then be sure we match the "Enter passphrase" prompt from
            ;; 'cryptsetup', in the initrd.
            (wait-for-screen-text #$marionette (negate bios-boot-screen?)
                                  #:ocrad #$ocrad
                                  #:timeout 20)))

        (test-assert "enter LUKS passphrase for the initrd"
          (begin
            ;; XXX: Here we use OCR as well but we could instead use QEMU
            ;; '-serial stdio' and run it in an input pipe,
            (wait-for-screen-text #$marionette passphrase-prompt?
                                  #:ocrad #$ocrad
                                  #:timeout 60)
            (marionette-type "thepassphrase\n" #$marionette)

            ;; Take a screenshot for debugging purposes.
            (marionette-control (string-append "screendump " #$output
                                               "/post-initrd-passphrase.ppm")
                                #$marionette))))))

(define %test-encrypted-root-os
  (system-test
   (name "encrypted-root-os")
   (description
    "Test basic functionality of an OS installed like one would do by hand.
This test is expensive in terms of CPU and storage usage since we need to
build (current-guix) and then store a couple of full system images.")
   (value
    (mlet* %store-monad ((image   (run-install %encrypted-root-os
                                               %encrypted-root-os-source
                                               #:script
                                               %encrypted-root-installation-script))
                         (command (qemu-command/writable-image image)))
      (run-basic-test %encrypted-root-os command "encrypted-root-os"
                      #:initialization enter-luks-passphrase)))))


;;;
;;; Btrfs root file system.
;;;

(define-os-with-source (%btrfs-root-os %btrfs-root-os-source)
  ;; The OS we want to install.
  (use-modules (gnu) (gnu tests) (srfi srfi-1))

  (operating-system
    (host-name "liberigilo")
    (timezone "Europe/Paris")
    (locale "en_US.UTF-8")

    (bootloader (bootloader-configuration
                 (bootloader grub-bootloader)
                 (target "/dev/vdb")))
    (kernel-arguments '("console=ttyS0"))
    (file-systems (cons (file-system
                          (device (file-system-label "my-root"))
                          (mount-point "/")
                          (type "btrfs"))
                        %base-file-systems))
    (users (cons (user-account
                  (name "charlie")
                  (group "users")
                  (home-directory "/home/charlie")
                  (supplementary-groups '("wheel" "audio" "video")))
                 %base-user-accounts))
    (services (cons (service marionette-service-type
                             (marionette-configuration
                              (imported-modules '((gnu services herd)
                                                  (guix combinators)))))
                    %base-services))))

(define %btrfs-root-installation-script
  ;; Shell script of a simple installation.
  "\
. /etc/profile
set -e -x
guix --version

export GUIX_BUILD_OPTIONS=--no-grafts
ls -l /run/current-system/gc-roots
parted --script /dev/vdb mklabel gpt \\
  mkpart primary ext2 1M 3M \\
  mkpart primary ext2 3M 2G \\
  set 1 boot on \\
  set 1 bios_grub on
mkfs.btrfs -L my-root /dev/vdb2
mount /dev/vdb2 /mnt
btrfs subvolume create /mnt/home
herd start cow-store /mnt
mkdir /mnt/etc
cp /etc/target-config.scm /mnt/etc/config.scm
guix system build /mnt/etc/config.scm
guix system init /mnt/etc/config.scm /mnt --no-substitutes
sync
reboot\n")

(define %test-btrfs-root-os
  (system-test
   (name "btrfs-root-os")
   (description
    "Test basic functionality of an OS installed like one would do by hand.
This test is expensive in terms of CPU and storage usage since we need to
build (current-guix) and then store a couple of full system images.")
   (value
    (mlet* %store-monad ((image   (run-install %btrfs-root-os
                                               %btrfs-root-os-source
                                               #:script
                                               %btrfs-root-installation-script))
                         (command (qemu-command/writable-image image)))
      (run-basic-test %btrfs-root-os command "btrfs-root-os")))))

;;; install.scm ends here
