;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2017 David Craven <david@craven.ch>
;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python))

(define-public ath9k-htc-firmware
  (package
    (name "ath9k-htc-firmware")
    (version "1.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/qca/open-ath9k-htc-firmware.git")
                    (commit version)))
              (sha256
               (base32
                "16jbj8avg5jkgvq5lxm0hdxxn4c3zn7fx8b4nxllvr024apk9w23"))
              (file-name (string-append name "-" version "-checkout"))
              (patches (search-patches "ath9k-htc-firmware-objcopy.patch"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'pre-configure
           (lambda* (#:key inputs #:allow-other-keys)
             (chdir "target_firmware")

             ;; 'configure' is a simple script that runs 'cmake' with
             ;; the right flags.
             (substitute* "configure"
               (("^TOOLCHAIN=.*$")
                (string-append "TOOLCHAIN="
                               (assoc-ref inputs "cross-gcc")
                               "\n")))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out    (assoc-ref outputs "out"))
                    (fw-dir (string-append out "/lib/firmware")))
               (mkdir-p fw-dir)
               (for-each (lambda (file)
                           (copy-file file
                                      (string-append fw-dir "/"
                                                     (basename file))))
                         (find-files "." "\\.fw$"))
              #t))))
       #:tests? #f))

    ;; The firmware is cross-compiled using a "bare bones" compiler (no libc.)
    ;; Use our own tool chain for that.
    (native-inputs `(("cross-gcc" ,(cross-gcc "xtensa-elf"))
                     ("cross-binutils" ,(cross-binutils "xtensa-elf"))
                     ("cmake" ,cmake)
                     ("perl" ,perl)))
    (home-page "http://wireless.kernel.org/en/users/Drivers/ath9k_htc")
    (synopsis "Firmware for the Atheros AR7010 and AR9271 USB 802.11n NICs")
    (description
     "This is the firmware for the Qualcomm Atheros AR7010 and AR9271 USB
802.11n NICs (aka Wi-Fi USB dongles).  It is used by the ath9k driver of
Linux-libre.")
    (license (license:non-copyleft "http://directory.fsf.org/wiki/License:ClearBSD"))))

(define-public b43-tools
  (let ((commit "8dce53297966b31b6c70a7a03c2433978dd9f288")
        (rev "1"))
    (package
      (name "b43-tools")
      (version (string-append "20140625-" rev "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "http://git.bues.ch/git/b43-tools.git")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "08k7sdr9jagm43r2zv4h03j86klhkblpk73p12444a3vzg1gy1lv"))))
      (build-system gnu-build-system)
      (native-inputs
       `(("flex" ,flex)
         ("bison" ,bison)))
      (arguments
       `(#:modules ((srfi srfi-1)
                    (guix build gnu-build-system)
                    (guix build utils))
         #:tests? #f                    ;no tests
         #:phases
         (let ((subdirs '("assembler" "disassembler")))
           (modify-phases %standard-phases
             (delete 'configure)
             (add-before 'build 'patch-/bin/true
               (lambda _
                 (substitute* (find-files "." "Makefile")
                   (("/bin/true") ":"))
                 #t))
             (replace 'build
               (lambda _
                 (every (lambda (dir)
                          (zero? (system* "make" "-C" dir "CC=gcc")))
                        subdirs)))
             (replace 'install
               (lambda* (#:key outputs #:allow-other-keys)
                 (let ((out (assoc-ref outputs "out")))
                   (mkdir-p (string-append out "/bin"))
                   (every (lambda (dir)
                            (zero?
                             (system* "make" "-C" dir
                                      (string-append "PREFIX=" out)
                                      "install")))
                          subdirs))))))))
      (home-page
       "http://bues.ch/cms/hacking/misc.html#linux_b43_driver_firmware_tools")
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
     `(("b43-tools" ,b43-tools)))
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

(define-public seabios
  (package
    (name "seabios")
    (version "1.10.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://code.coreboot.org/p/seabios/downloads/get/"
                                  "seabios-" version ".tar.gz"))
              (sha256
               (base32
                "1jyjl719drnl1v0gf0l5q6qjjmkyqcqkka6s28dfdi0yqsxdsqsh"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("python-2" ,python-2)))
    (arguments
     `(#:tests? #f ; No check target.
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             (setenv "CC" "gcc")
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (fmw (string-append out "/share/firmware")))
               (mkdir-p fmw)
               (copy-file "out/bios.bin" (string-append fmw "/bios.bin"))))))))
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
       ("nasm" ,nasm)
       ("python-2" ,python-2)
       ("util-linux" ,util-linux)))
    (arguments
     `(#:tests? #f ; No check target.
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             (let* ((cwd (getcwd))
                    (tools (string-append cwd "/BaseTools"))
                    (bin (string-append tools "/BinWrappers/PosixLike")))
               (setenv "WORKSPACE" cwd)
               (setenv "EDK_TOOLS_PATH" tools)
               (setenv "PATH" (string-append (getenv "PATH") ":" bin))
               (system* "bash" "edksetup.sh" "BaseTools")
               (substitute* "Conf/target.txt"
                 (("^TARGET[ ]*=.*$") "TARGET = RELEASE\n")
                 (("^TOOL_CHAIN_TAG[ ]*=.*$") "TOOL_CHAIN_TAG = GCC49\n")
                 (("^MAX_CONCURRENT_THREAD_NUMBER[ ]*=.*$")
                  (format #f "MAX_CONCURRENT_THREAD_NUMBER = ~a~%"
                          (number->string (parallel-job-count)))))
               ;; Build build support.
               (setenv "BUILD_CC" "gcc")
               (zero? (system* "make" "-C" (string-append tools "/Source/C"))))))
         (add-after 'build 'build-ia32
           (lambda _
             (substitute* "Conf/target.txt"
               (("^TARGET_ARCH[ ]*=.*$") "TARGET_ARCH = IA32\n")
               (("^ACTIVE_PLATFORM[ ]*=.*$")
                "ACTIVE_PLATFORM = OvmfPkg/OvmfPkgIa32.dsc\n"))
             (zero? (system* "build"))))
         ,@(if (string=? "x86_64-linux" (%current-system))
             '((add-after 'build 'build-x64
                (lambda _
                  (substitute* "Conf/target.txt"
                    (("^TARGET_ARCH[ ]*=.*$") "TARGET_ARCH = X64\n")
                    (("^ACTIVE_PLATFORM[ ]*=.*$")
                     "ACTIVE_PLATFORM = OvmfPkg/OvmfPkgX64.dsc\n"))
                  (zero? (system* "build")))))
             '())
         (delete 'build)
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
    (home-page "http://www.tianocore.org")
    (synopsis "UEFI firmware for QEMU")
    (description "OVMF is an EDK II based project to enable UEFI support for
Virtual Machines.  OVMF contains a sample UEFI firmware for QEMU and KVM.")
    (license (list license:expat
                   license:bsd-2 license:bsd-3 license:bsd-4))))
