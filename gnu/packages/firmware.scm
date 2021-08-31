;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2017 David Craven <david@craven.ch>
;;; Copyright © 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Vagrant Cascadian <vagrant@debian.org>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020, 2021, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2021 Petr Hodina <phodina@protonmail.com>
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

(define-module (gnu packages firmware)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages pkg-config))

(define-public ath9k-htc-firmware
  (package
    (name "ath9k-htc-firmware")
    (version "1.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/qca/open-ath9k-htc-firmware")
                    (commit version)))
              (sha256
               (base32
                "16jbj8avg5jkgvq5lxm0hdxxn4c3zn7fx8b4nxllvr024apk9w23"))
              (file-name (git-file-name name version))
              (patches (search-patches "ath9k-htc-firmware-objcopy.patch"
                                       "ath9k-htc-firmware-gcc-compat.patch"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'pre-configure
           (lambda* (#:key inputs native-inputs #:allow-other-keys)
             (chdir "target_firmware")

             ;; 'configure' is a simple script that runs 'cmake' with
             ;; the right flags.
             (substitute* "configure"
               (("^TOOLCHAIN=.*$")
                (string-append "TOOLCHAIN="
                               (assoc-ref (or native-inputs inputs) "cross-gcc")
                               "\n")))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out    (assoc-ref outputs "out"))
                    (fw-dir (string-append out "/lib/firmware")))
               (for-each (lambda (file)
                           (install-file file fw-dir))
                         (find-files "." "\\.fw$"))
              #t))))
       #:tests? #f))

    ;; The firmware is cross-compiled using a "bare bones" compiler (no libc.)
    ;; Use our own tool chain for that.
    (native-inputs `(("cross-gcc" ,(cross-gcc
                                    "xtensa-elf"
                                    #:xbinutils (cross-binutils "xtensa-elf"
                                                                binutils-2.33)))
                     ("cross-binutils" ,(cross-binutils "xtensa-elf" binutils-2.33))
                     ("cmake" ,cmake-minimal)
                     ("perl" ,perl)))
    (home-page "https://wireless.wiki.kernel.org/en/users/Drivers/ath9k_htc")
    (synopsis "Firmware for the Atheros AR7010 and AR9271 USB 802.11n NICs")
    (description
     "This is the firmware for the Qualcomm Atheros AR7010 and AR9271 USB
802.11n NICs (aka Wi-Fi USB dongles).  It is used by the ath9k driver of
Linux-libre.")
    (license (license:non-copyleft "http://directory.fsf.org/wiki/License:ClearBSD"))))

(define-public b43-tools
  (let ((commit "27892ef741e7f1d08cb939744f8b8f5dac7b04ae")
        (revision "1"))
    (package
      (name "b43-tools")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "http://git.bues.ch/git/b43-tools.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1wgmj4d65izbhprwb5bcwimc2ryv19b9066lqzy4sa5m6wncm9cn"))))
      (build-system gnu-build-system)
      (native-inputs
       (list flex bison))
      (arguments
       `(#:modules ((srfi srfi-1)
                    (guix build gnu-build-system)
                    (guix build utils))
         #:tests? #f                    ; no tests
         #:phases
         (let ((subdirs '("assembler" "disassembler")))
           (modify-phases %standard-phases
             (delete 'configure)        ; no configure script
             (add-before 'build 'patch-/bin/true
               (lambda _
                 (substitute* (find-files "." "Makefile")
                   (("/bin/true") ":"))
                 #t))
             (replace 'build
               (lambda _
                 (for-each (lambda (dir)
                             (invoke "make" "-C" dir "CC=gcc"))
                           subdirs)
                 #t))
             (replace 'install
               (lambda* (#:key outputs #:allow-other-keys)
                 (let ((out (assoc-ref outputs "out")))
                   (mkdir-p (string-append out "/bin"))
                   (for-each (lambda (dir)
                               (invoke "make" "-C" dir
                                       (string-append "PREFIX=" out)
                                       "install"))
                             subdirs)
                   #t)))))))
      (home-page
       "https://bues.ch/cms/hacking/misc.html#linux_b43_driver_firmware_tools")
      (synopsis "Collection of tools for the b43 wireless driver")
      (description
       "The b43 firmware tools is a collection of firmware extractor,
assembler, disassembler, and debugging tools for the Linux kernel b43 wireless
driver.")
      (license license:gpl2))))

(define-public openfwwf-firmware
  (package
    (name "openfwwf-firmware")
    (version "5.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://netweb.ing.unibs.it/~openfwwf/firmware/"
                           "openfwwf-" version ".tar.gz"))
       (sha256
        (base32
         "1p60gdi7w88s7qw82d3g9v7mk887mhvidf4l5q5hh09j10h37q4x"))))
    (build-system gnu-build-system)
    (native-inputs
     (list b43-tools))
    (arguments
     `(#:make-flags (list (string-append "PREFIX="
                                         (assoc-ref %outputs "out")
                                         "/lib/firmware/b43-open"))
       #:tests? #f                      ;no tests
       #:phases (modify-phases %standard-phases
                  (delete 'configure))))
    (home-page "http://netweb.ing.unibs.it/~openfwwf/")
    (synopsis "Firmware for BCM43xx devices")
    (description
     "This is firmware from Open FirmWare for WiFi networks (OpenFWWF) for the
Broadcom/AirForce chipset BCM43xx with Wireless-Core Revision 5.  It is used
by the b43-open driver of Linux-libre.")
    (license license:gpl2)))

(define-public eg25-manager
  (package
    (name "eg25-manager")
    (version "0.4.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/mobian1/devices/eg25-manager")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1czq2yi852aqkdnrxdifzcq669bdvlm7j40xivxq77jq04fggpmf"))))
    (build-system meson-build-system)
    (native-inputs (list curl
                         `(,glib "bin") pkg-config))
    (inputs (list libgpiod libgudev libusb))
    (synopsis "Manager daemon for the Quectel EG25 mobile broadband modem")
    (description
     "This package provides a manager daemon for the Quectel EG25 mobile
broadband modem as found, for example, on PinePhone.")
    (home-page "https://gitlab.com/mobian1/devices/eg25-manager")
    (license license:gpl3+)))

(define* (make-opensbi-package platform name #:optional (arch "riscv64"))
  (package
    (name name)
    (version "1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/riscv-software-src/opensbi")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0srqkhd9b1mq4qkqk31dlrzy4mhljr49bzjxm0saylsbwhgxq31s"))))
    (build-system gnu-build-system)
    (native-inputs
     `(,@(if (and (not (string-prefix? "riscv64" (%current-system)))
                  (string-prefix? "riscv64" arch))
           `(("cross-gcc" ,(cross-gcc "riscv64-linux-gnu" #:xgcc gcc-7))
             ("cross-binutils" ,(cross-binutils "riscv64-linux-gnu")))
           '())))
    (arguments
     `(#:tests? #f ; no check target
       #:make-flags (list (string-append "PLATFORM=" ,platform)
                          ,@(if (and (not (string-prefix? "riscv64"
                                                          (%current-system)))
                                     (string-prefix? "riscv64" arch))
                                `("CROSS_COMPILE=riscv64-linux-gnu-")
                                `("CC=gcc"))
                          "FW_PAYLOAD=n"
                          "V=1")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (bin (find-files "." "fw_.*\\.(elf|bin)$")))
               (for-each
                 (lambda (file)
                   (install-file file out))
                 bin))
             #t)))))
    (home-page "https://github.com/riscv-software-src/opensbi")
    (synopsis "RISC-V Open Source Supervisor Binary Interface")
    (description "A reference implementation of the RISC-V SBI specifications
for platform-specific firmwares executing in M-mode.")
    (license (list license:bsd-2
                   ;; lib/utils/libfdt/* is dual licensed under bsd-2 and gpl2+.
                   license:gpl2+
                   ;; platform/ariane-fpga/* is gpl2.
                   license:gpl2))))

(define-public opensbi-generic
  (make-opensbi-package "generic" "opensbi-generic"))

(define-public seabios
  (package
    (name "seabios")
    (version "1.15.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://review.coreboot.org/seabios.git")
             (commit (string-append "rel-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gnsfmbgcvihsap8sz8c2n3qs439q44i3pwrms2nv3xcnf1sclj9"))))
    (build-system gnu-build-system)
    (native-inputs (list python-wrapper))
    (arguments
     `(#:tests? #f                      ; no check target
       #:make-flags '("EXTRAVERSION=-guix") ;upstream wants distros to set this
       #:modules (,@%gnu-build-system-modules
                  (ice-9 match))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             ;; Create the ".version" file that is present in release tarballs.
             ;; Otherwise this will be regarded as an "unclean" build, and the
             ;; build system ends up encoding the build date in the binaries.
             (call-with-output-file ".version"
               (lambda (port)
                 (format port ,(package-version this-package))))
             (setenv "CC" "gcc")))
         (add-after 'build 'build-vgabios
           (lambda* (#:key (make-flags ''()) #:allow-other-keys)
             (for-each
              (match-lambda
                ((target . config)
                 (let* ((dot-config (string-append (getcwd) "/" target "/.config"))
                        (flags (append make-flags
                                      (list (string-append "KCONFIG_CONFIG="
                                                           dot-config)
                                            (string-append "OUT=" target "/")))))
                   (mkdir target)
                   (call-with-output-file dot-config
                     (lambda (port)
                       (for-each (lambda (entry)
                                   (if (string-suffix? "=n" entry)
                                       (format port "# CONFIG_~a is not set~%"
                                               (string-drop-right entry 2))
                                       (format port "CONFIG_~a~%" entry)))
                                 (cons "BUILD_VGABIOS=y" config))))
                   (apply invoke "make" (append flags '("oldnoconfig")))
                   (apply invoke "make" flags)
                   (link (string-append target "/bios.bin")
                         (string-append "out/" target ".bin")))))
              ;; These tuples are modelled after Debians packaging:
              ;; https://salsa.debian.org/qemu-team/seabios/-/blob/master/debian/rules
              '(("ati"    . ("VGA_ATI=y" "VGA_PCI=y"))
                ("bochs-display" . ("DISPLAY_BOCHS=y" "VGA_PCI=y"))
                ("cirrus" . ("VGA_CIRRUS=y" "VGA_PCI=y"))
                ("stdvga" . ("VGA_BOCHS=y" "VGA_PCI=y"))
                ("virtio" . ("VGA_BOCHS_VIRTIO=y" "VGA_PCI=y"))
                ("vmware" . ("VGA_BOCHS_VMWARE=y" "VGA_PCI=y"))
                ("qxl"    . ("VGA_BOCHS_QXL=y" "VGA_PCI=y"))
                ("isavga" . ("VGA_BOCHS=y" "VGA_PCI=n"))
                ("ramfb"  . ("VGA_RAMFB=y" "VGA_PCI=n"))))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (fmw (string-append out "/share/firmware")))
               (mkdir-p fmw)
               (copy-file "out/bios.bin" (string-append fmw "/bios.bin"))
               (for-each (lambda (bios)
                           (install-file bios fmw))
                         (find-files "out" "\\.bin$"))
               (with-directory-excursion fmw
                 ;; QEMU 1.7 and later looks only for the latter.
                 (symlink "bios.bin" "bios-256k.bin"))))))))
    (home-page "https://www.seabios.org/SeaBIOS")
    (synopsis "x86 BIOS implementation")
    (description "SeaBIOS is an implementation of a 16bit x86 BIOS.  SeaBIOS
can run in an emulator or it can run natively on X86 hardware with the use of
coreboot.")
    ;; Dual licensed.
    (license (list license:gpl3+ license:lgpl3+
                   ;; src/fw/acpi-dsdt.dsl is lgpl2
                   license:lgpl2.1
                   ;; src/fw/lzmadecode.c and src/fw/lzmadecode.h are lgpl3+ and
                   ;; cpl with a linking exception.
                   license:cpl1.0))))

;; OVMF is part of the edk2 source tree.
(define edk2-commit "13a50a6fe1dcfa6600c38456ee24e0f9ecf51b5f")
(define edk2-version (git-version "20170116" "1" edk2-commit))
(define edk2-origin
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/tianocore/edk2")
          (commit edk2-commit)))
    (file-name (git-file-name "edk2" edk2-version))
    (sha256
     (base32
      "1gy2332kdqk8bjzpcsripx10896rbvgl0ic7r344kmpiwdgm948b"))))

(define-public ovmf
  (package
    (name "ovmf")
    (version edk2-version)
    (source edk2-origin)
    (build-system gnu-build-system)
    (native-inputs
     `(("acpica" ,acpica)
       ("gcc@5" ,gcc-5)
       ("nasm" ,nasm)
       ("python-2" ,python-2)
       ("util-linux" ,util-linux "lib")))
    (arguments
     `(#:tests? #f ; No check target.
       #:phases
       (modify-phases %standard-phases
         ;; Hide the default GCC from CPLUS_INCLUDE_PATH to prevent it from
         ;; shadowing the version of GCC provided in native-inputs.
         (add-after 'set-paths 'hide-gcc7
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gcc (assoc-ref inputs "gcc")))
               (setenv "CPLUS_INCLUDE_PATH"
                       (string-join
                        (delete (string-append gcc "/include/c++")
                                (string-split (getenv "CPLUS_INCLUDE_PATH")
                                              #\:))
                        ":"))
               #t)))
         (replace 'configure
           (lambda _
             (let* ((cwd (getcwd))
                    (tools (string-append cwd "/BaseTools"))
                    (bin (string-append tools "/BinWrappers/PosixLike")))
               (setenv "WORKSPACE" cwd)
               (setenv "EDK_TOOLS_PATH" tools)
               (setenv "PATH" (string-append (getenv "PATH") ":" bin))
               ; FIXME: The below script errors out. When using 'invoke' instead
               ; of 'system*' this causes the build to fail.
               (system* "bash" "edksetup.sh")
               (substitute* "Conf/target.txt"
                 (("^TARGET[ ]*=.*$") "TARGET = RELEASE\n")
                 (("^MAX_CONCURRENT_THREAD_NUMBER[ ]*=.*$")
                  (format #f "MAX_CONCURRENT_THREAD_NUMBER = ~a~%"
                          (number->string (parallel-job-count)))))
               ;; Build build support.
               (setenv "BUILD_CC" "gcc")
               (invoke "make" "-C" tools)
               #t)))
         (replace 'build
           (lambda _
             (invoke "build" "-a" "IA32" "-t" "GCC49"
                     "-p" "OvmfPkg/OvmfPkgIa32.dsc")))
         ,@(if (string=? "x86_64-linux" (%current-system))
             '((add-after 'build 'build-x64
                (lambda _
                  (invoke "build" "-a" "X64" "-t" "GCC49"
                          "-p" "OvmfPkg/OvmfPkgX64.dsc"))))
             '())
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (fmw (string-append out "/share/firmware")))
               (mkdir-p fmw)
               (copy-file "Build/OvmfIa32/RELEASE_GCC49/FV/OVMF.fd"
                          (string-append fmw "/ovmf_ia32.bin"))
               ,@(if (string=? "x86_64-linux" (%current-system))
                   '((copy-file "Build/OvmfX64/RELEASE_GCC49/FV/OVMF.fd"
                                (string-append fmw "/ovmf_x64.bin")))
                   '()))
             #t)))))
    (supported-systems '("x86_64-linux" "i686-linux"))
    (home-page "https://www.tianocore.org")
    (synopsis "UEFI firmware for QEMU")
    (description "OVMF is an EDK II based project to enable UEFI support for
Virtual Machines.  OVMF contains a sample UEFI firmware for QEMU and KVM.")
    (license (list license:expat
                   license:bsd-2 license:bsd-3 license:bsd-4))))

(define-public ovmf-aarch64
  (package
    (inherit ovmf)
    (name "ovmf-aarch64")
    (native-inputs
     `(,@(package-native-inputs ovmf)
       ,@(if (not (string-prefix? "aarch64" (%current-system)))
           `(("cross-gcc" ,(cross-gcc "aarch64-linux-gnu"))
             ("cross-binutils" ,(cross-binutils "aarch64-linux-gnu")))
           '())))
    (arguments
     (substitute-keyword-arguments (package-arguments ovmf)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-before 'configure 'set-env
             (lambda _
               ,@(if (not (string-prefix? "aarch64" (%current-system)))
                     `((setenv "GCC49_AARCH64_PREFIX" "aarch64-linux-gnu-"))
                     '())
               #t))
           (replace 'build
             (lambda _
               (invoke "build" "-a" "AARCH64" "-t" "GCC49"
                       "-p" "ArmVirtPkg/ArmVirtQemu.dsc")))
           (delete 'build-x64)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (fmw (string-append out "/share/firmware")))
                 (mkdir-p fmw)
                 (copy-file "Build/ArmVirtQemu-AARCH64/RELEASE_GCC49/FV/QEMU_EFI.fd"
                            (string-append fmw "/ovmf_aarch64.bin"))
                 #t)))))))
    (supported-systems %supported-systems)))

(define-public ovmf-arm
  (package
    (inherit ovmf)
    (name "ovmf-arm")
    (native-inputs
     `(,@(package-native-inputs ovmf)
       ,@(if (not (string-prefix? "armhf" (%current-system)))
           `(("cross-gcc" ,(cross-gcc "arm-linux-gnueabihf"))
             ("cross-binutils" ,(cross-binutils "arm-linux-gnueabihf")))
           '())))
    (arguments
     (substitute-keyword-arguments (package-arguments ovmf)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-before 'configure 'set-env
             (lambda _
               ,@(if (not (string-prefix? "armhf" (%current-system)))
                     `((setenv "GCC49_ARM_PREFIX" "arm-linux-gnueabihf-"))
                     '())
               #t))
           (replace 'build
             (lambda _
               (invoke "build" "-a" "ARM" "-t" "GCC49"
                       "-p" "ArmVirtPkg/ArmVirtQemu.dsc")))
           (delete 'build-x64)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (fmw (string-append out "/share/firmware")))
                 (mkdir-p fmw)
                 (copy-file "Build/ArmVirtQemu-ARM/RELEASE_GCC49/FV/QEMU_EFI.fd"
                            (string-append fmw "/ovmf_arm.bin"))
                 #t)))))))
    (supported-systems %supported-systems)))

(define* (make-arm-trusted-firmware platform #:optional (arch "aarch64"))
  (package
    (name (string-append "arm-trusted-firmware-" platform))
    (version "2.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               ;; There are only GitHub generated release snapshots.
               (url "https://git.trustedfirmware.org/TF-A/trusted-firmware-a.git/")
               (commit (string-append "v" version))))
        (file-name (git-file-name "arm-trusted-firmware" version))
       (sha256
        (base32
         "1j0rn33pwgmksqliwf2npm2px84qmbyma9iq8zpllwfc7dsl6gx9"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure) ; no configure script
         ;; Remove binary blobs which do not contain source or proper license.
         (add-after 'unpack 'remove-binary-blobs
           (lambda _
             (for-each (lambda (file)
                         (delete-file file))
                       (find-files "." ".*\\.bin$"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (bin (find-files "." ".*\\.(bin|elf)$")))
               (for-each
                 (lambda (file)
                   (install-file file out))
                 bin))
             #t)))
       #:make-flags (list (string-append "PLAT=" ,platform)
                          ,@(if (and (not (string-prefix? "aarch64"
                                                          (%current-system)))
                                     (string-prefix? "aarch64" arch))
                              `("CROSS_COMPILE=aarch64-linux-gnu-")
                              '())
                          ,@(if (and (not (string-prefix? "armhf"
                                                          (%current-system)))
                                     (string-prefix? "armhf" arch))
                              `("CROSS_COMPILE=arm-linux-gnueabihf-")
                              '())
                          "DEBUG=1")
       #:tests? #f)) ; no tests
    (native-inputs
     (let ((system (%current-system)))
       (cond
        ((and (not (string-prefix? "aarch64" system))
              (string-prefix? "aarch64" arch))
         (list (cross-gcc "aarch64-linux-gnu")
               (cross-binutils "aarch64-linux-gnu")))
        ((and (not (string-prefix? "armhf" system))
              (string-prefix? "armhf" arch))
         (list (cross-gcc "arm-linux-gnueabihf")
               (cross-binutils "arm-linux-gnueabihf")))
        (else '()))))
    (home-page "https://www.trustedfirmware.org/")
    (synopsis "Implementation of \"secure world software\"")
    (description
     "ARM Trusted Firmware provides a reference implementation of secure world
software for ARMv7A and ARMv8-A, including a Secure Monitor executing at
@dfn{Exception Level 3} (EL3).  It implements various ARM interface standards,
such as:
@enumerate
@item The Power State Coordination Interface (PSCI)
@item Trusted Board Boot Requirements (TBBR, ARM DEN0006C-1)
@item SMC Calling Convention
@item System Control and Management Interface
@item Software Delegated Exception Interface (SDEI)
@end enumerate\n")
    (license (list license:bsd-3
                   license:bsd-2)))) ; libfdt

(define-public arm-trusted-firmware-sun50i-a64
  (let ((base (make-arm-trusted-firmware "sun50i_a64")))
    (package
      (inherit base)
      (name "arm-trusted-firmware-sun50i-a64"))))

(define-public arm-trusted-firmware-rk3328
  (make-arm-trusted-firmware "rk3328"))

(define-public arm-trusted-firmware-rk3399
  (let ((base (make-arm-trusted-firmware "rk3399")))
    (package
      (inherit base)
      (name "arm-trusted-firmware-rk3399")
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (prepend
             (cross-gcc "arm-none-eabi")
             (cross-binutils "arm-none-eabi")))))))

(define-public arm-trusted-firmware-imx8mq
  (let ((base (make-arm-trusted-firmware "imx8mq")))
    (package
      (inherit base)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:make-flags flags ''())
          ;; Adding debug symbols causes the size to exceed limits.
          #~(delete "DEBUG=1" #$flags)))))))
