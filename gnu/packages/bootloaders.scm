;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016, 2017, 2018 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2016, 2017 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2016, 2017 David Craven <david@craven.ch>
;;; Copyright © 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 nee <nee@cock.li>
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

(define-module (gnu packages bootloaders)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module ((gnu packages algebra) #:select (bc))
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages base)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages firmware)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages mtools)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages web)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 regex))

(define unifont
  ;; GNU Unifont, <http://gnu.org/s/unifont>.
  ;; GRUB needs it for its graphical terminal, gfxterm.
  (origin
    (method url-fetch)
    (uri
     "http://unifoundry.com/pub/unifont-7.0.06/font-builds/unifont-7.0.06.bdf.gz")
    (sha256
     (base32
      "0p2vhnc18cnbmb39vq4m7hzv4mhnm2l0a2s7gx3ar277fwng3hys"))))

(define-public grub
  (package
    (name "grub")
    (version "2.02")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/grub/grub-" version ".tar.xz"))
             (sha256
              (base32
               "03vvdfhdmf16121v7xs8is2krwnv15wpkhkf16a4yf8nsfc3f2w1"))
             (patches (search-patches "grub-check-error-efibootmgr.patch"
                                      "grub-binutils-compat.patch"
                                      "grub-efi-fat-serial-number.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-stuff
                   (lambda* (#:key inputs #:allow-other-keys)
                     (substitute* "grub-core/Makefile.in"
                       (("/bin/sh") (which "sh")))

                     ;; Give the absolute file name of 'mdadm', used to
                     ;; determine the root file system when it's a RAID
                     ;; device.  Failing to do that, 'grub-probe' silently
                     ;; fails if 'mdadm' is not in $PATH.
                     (substitute* "grub-core/osdep/linux/getroot.c"
                       (("argv\\[0\\] = \"mdadm\"")
                        (string-append "argv[0] = \""
                                       (assoc-ref inputs "mdadm")
                                       "/sbin/mdadm\"")))

                     ;; Make the font visible.
                     (copy-file (assoc-ref inputs "unifont") "unifont.bdf.gz")
                     (system* "gunzip" "unifont.bdf.gz")

                     ;; Give the absolute file name of 'ckbcomp'.
                     (substitute* "util/grub-kbdcomp.in"
                       (("^ckbcomp ")
                        (string-append (assoc-ref inputs "console-setup")
                                       "/bin/ckbcomp ")))
                     #t))
                  (add-before 'check 'disable-flaky-test
                    (lambda _
                      ;; This test is unreliable. For more information, see:
                      ;; <https://bugs.gnu.org/26936>.
                      (substitute* "Makefile.in"
                        (("grub_cmd_date grub_cmd_set_date grub_cmd_sleep")
                          "grub_cmd_date grub_cmd_sleep"))
                      #t)))
       ;; Disable tests on ARM and AARCH64 platforms.
       #:tests? ,(not (any (cute string-prefix? <> (or (%current-target-system)
                                                       (%current-system)))
                           '("arm" "aarch64")))))
    (inputs
     `(("gettext" ,gettext-minimal)

       ;; Depend on LVM2 for libdevmapper, used by 'grub-probe' and
       ;; 'grub-install' to recognize mapped devices (LUKS, etc.)
       ("lvm2" ,lvm2)

       ;; Depend on mdadm, which is invoked by 'grub-probe' and 'grub-install'
       ;; to determine whether the root file system is RAID.
       ("mdadm" ,mdadm)

       ;; Console-setup's ckbcomp is invoked by grub-kbdcomp.  It is required
       ;; for generating alternative keyboard layouts.
       ("console-setup" ,console-setup)

       ("freetype" ,freetype)
       ;; ("libusb" ,libusb)
       ;; ("fuse" ,fuse)
       ("ncurses" ,ncurses)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("unifont" ,unifont)
       ("bison" ,bison)
       ;; Due to a bug in flex >= 2.6.2, GRUB must be built with an older flex:
       ;; <http://lists.gnu.org/archive/html/grub-devel/2017-02/msg00133.html>
       ;; TODO Try building with flex > 2.6.4.
       ("flex" ,flex-2.6.1)
       ("texinfo" ,texinfo)
       ("help2man" ,help2man)

       ;; XXX: When building GRUB 2.02 on 32-bit x86, we need a binutils
       ;; capable of assembling 64-bit instructions.  However, our default
       ;; binutils on 32-bit x86 is not 64-bit capable.
       ,@(if (string-match "^i[3456]86-" (%current-system))
             (let ((binutils (package/inherit
                              binutils
                              (name "binutils-i386")
                              (arguments
                               (substitute-keyword-arguments (package-arguments binutils)
                                 ((#:configure-flags flags ''())
                                  `(cons "--enable-64-bit-bfd" ,flags)))))))
               `(("ld-wrapper" ,(make-ld-wrapper "ld-wrapper-i386"
                                                 #:binutils binutils))
                 ("binutils" ,binutils)))
             '())

       ;; Dependencies for the test suite.  The "real" QEMU is needed here,
       ;; because several targets are used.
       ("parted" ,parted)
       ("qemu" ,qemu-minimal-2.10)
       ("xorriso" ,xorriso)))
    (home-page "https://www.gnu.org/software/grub/")
    (synopsis "GRand Unified Boot loader")
    (description
     "GRUB is a multiboot bootloader.  It is used for initially loading the
kernel of an operating system and then transferring control to it.  The kernel
then goes on to load the rest of the operating system.  As a multiboot
bootloader, GRUB handles the presence of multiple operating systems installed
on the same computer; upon booting the computer, the user is presented with a
menu to select one of the installed operating systems.")
    (license license:gpl3+)
    (properties '((cpe-name . "grub2")))))

(define-public grub-efi
  (package
    (inherit grub)
    (name "grub-efi")
    (synopsis "GRand Unified Boot loader (UEFI version)")
    (inputs
     `(("efibootmgr" ,efibootmgr)
       ("mtools" ,mtools)
       ,@(package-inputs grub)))
    (arguments
     `(;; TODO: Tests need a UEFI firmware for qemu. There is one at
       ;; https://github.com/tianocore/edk2/tree/master/OvmfPkg .
       ;; Search for 'OVMF' in "tests/util/grub-shell.in".
       ,@(substitute-keyword-arguments (package-arguments grub)
           ((#:tests? _ #f) #f)
           ((#:configure-flags flags ''())
            `(cons "--with-platform=efi" ,flags))
           ((#:phases phases)
            `(modify-phases ,phases
               (add-after 'patch-stuff 'use-absolute-efibootmgr-path
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "grub-core/osdep/unix/platform.c"
                     (("efibootmgr")
                      (string-append (assoc-ref inputs "efibootmgr")
                                     "/sbin/efibootmgr")))
                   #t))
               (add-after 'patch-stuff 'use-absolute-mtools-path
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let ((mtools (assoc-ref inputs "mtools")))
                     (substitute* "util/grub-mkrescue.c"
                       (("\"mformat\"")
                        (string-append "\"" mtools
                                       "/bin/mformat\"")))
                     (substitute* "util/grub-mkrescue.c"
                       (("\"mcopy\"")
                        (string-append "\"" mtools
                                       "/bin/mcopy\"")))
                     #t))))))))))

;; Because grub searches hardcoded paths it's easiest to just build grub
;; again to make it find both grub-pc and grub-efi.  There is a command
;; line argument which allows you to specify ONE platform - but
;; grub-mkrescue will use multiple platforms if they are available
;; in the installation directory (without command line argument).
(define-public grub-hybrid
  (package
    (inherit grub-efi)
    (name "grub-hybrid")
    (synopsis "GRand Unified Boot loader (hybrid version)")
    (inputs
     `(("grub" ,grub)
       ,@(package-inputs grub-efi)))
    (arguments
     (substitute-keyword-arguments (package-arguments grub-efi)
       ((#:modules modules `((guix build utils) (guix build gnu-build-system)))
        `((ice-9 ftw) ,@modules))
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'install 'install-non-efi
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((input-dir (string-append (assoc-ref inputs "grub")
                                               "/lib/grub"))
                     (output-dir (string-append (assoc-ref outputs "out")
                                                "/lib/grub")))
                 (for-each
                  (lambda (basename)
                    (if (not (or (string-prefix? "." basename)
                                 (file-exists? (string-append output-dir "/" basename))))
                        (symlink (string-append input-dir "/" basename)
                                 (string-append output-dir "/" basename))))
                  (scandir input-dir))
                 #t)))))))))

(define-public syslinux
  (let ((commit "bb41e935cc83c6242de24d2271e067d76af3585c"))
    (package
      (name "syslinux")
      (version (git-version "6.04-pre" "1" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/geneC/syslinux")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0k8dvafd6410kqxf3kyr4y8jzmpmrih6wbjqg6gklak7945yflrc"))))
      (build-system gnu-build-system)
      (native-inputs
       `(("nasm" ,nasm)
         ("perl" ,perl)
         ("python-2" ,python-2)))
      (inputs
       `(("libuuid" ,util-linux)
         ("mtools" ,mtools)))
      (arguments
       `(#:parallel-build? #f
         #:make-flags
         (list (string-append "BINDIR=" %output "/bin")
               (string-append "SBINDIR=" %output "/sbin")
               (string-append "LIBDIR=" %output "/lib")
               (string-append "INCDIR=" %output "/include")
               (string-append "DATADIR=" %output "/share")
               (string-append "MANDIR=" %output "/share/man")
               "PERL=perl"
               "bios")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-files
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* (find-files "." "Makefile.*|ppmtolss16")
                 (("/bin/pwd") (which "pwd"))
                 (("/bin/echo") (which "echo"))
                 (("/usr/bin/perl") (which "perl")))
               (let ((mtools (assoc-ref inputs "mtools")))
                 (substitute* (find-files "." "\\.c$")
                   (("mcopy")
                    (string-append mtools "/bin/mcopy"))
                   (("mattrib")
                    (string-append mtools "/bin/mattrib"))))
               #t))
           (delete 'configure)
           (add-before 'build 'set-permissions
             (lambda _
               (invoke "chmod" "a+w" "utils/isohybrid.in")))
           (replace 'check
             (lambda _
               (setenv "CC" "gcc")
               (substitute* "tests/unittest/include/unittest/unittest.h"
                 ;; Don't look up headers under /usr.
                 (("/usr/include/") ""))
               (invoke "make" "unittest"))))))
      (home-page "https://www.syslinux.org")
      (synopsis "Lightweight Linux bootloader")
      (description "Syslinux is a lightweight Linux bootloader.")
      ;; The Makefile specifically targets i386 and x86_64 using nasm.
      (supported-systems '("i686-linux" "x86_64-linux"))
      (license (list license:gpl2+
                     license:bsd-3 ; gnu-efi/*
                     license:bsd-4 ; gnu-efi/inc/* gnu-efi/lib/*
                     ;; Also contains:
                     license:expat license:isc license:zlib)))))

(define-public dtc
  (package
    (name "dtc")
    (version "1.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kernel.org/software/utils/dtc/"
                    "dtc-" version ".tar.xz"))
              (sha256
               (base32
                "0wh10p42hf5403ipvs0dsxddb6kzfyk2sq4fgid9zqzpr51y8wn6"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)
       ("libyaml" ,libyaml)
       ("pkg-config" ,pkg-config)
       ("swig" ,swig)
       ("valgrind" ,valgrind)))
    (inputs
     `(("python-2" ,python-2)))
    (arguments
     `(#:make-flags
       (list "CC=gcc"
             (string-append "PREFIX=" (assoc-ref %outputs "out"))
             (string-append "SETUP_PREFIX=" (assoc-ref %outputs "out"))
             "INSTALL=install")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))         ; no configure script
    (home-page "https://www.devicetree.org")
    (synopsis "Compiles device tree source files")
    (description "@command{dtc} compiles
@uref{http://elinux.org/Device_Tree_Usage, device tree source files} to device
tree binary files.  These are board description files used by Linux and BSD.")
    (license license:gpl2+)))

(define u-boot
  (package
    (name "u-boot")
    (version "2019.04")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "ftp://ftp.denx.de/pub/u-boot/"
                    "u-boot-" version ".tar.bz2"))
              (sha256
               (base32
                "1vwv4bgbl7fjcm073zrphn17hnz5h5h778f88ivdsgbb2lnpgdvn"))
              (patches
               (search-patches
                "u-boot-fix-mkimage-header-verification.patch"))))
    (native-inputs
     `(("bc" ,bc)
       ("bison" ,bison)
       ("dtc" ,dtc)
       ("flex" ,flex)
       ("lz4" ,lz4)
       ("python-2" ,python-2)
       ("python2-coverage" ,python2-coverage)
       ("python2-pytest" ,python2-pytest)
       ("sdl" ,sdl)
       ("swig" ,swig)))
    (build-system  gnu-build-system)
    (home-page "https://www.denx.de/wiki/U-Boot/")
    (synopsis "ARM bootloader")
    (description "U-Boot is a bootloader used mostly for ARM boards. It
also initializes the boards (RAM etc).")
    (license license:gpl2+)))

(define-public u-boot-tools
  (package
    (inherit u-boot)
    (name "u-boot-tools")
    (arguments
     `(#:make-flags '("HOSTCC=gcc")
       #:test-target "tests"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "Makefile"
              (("/bin/pwd") (which "pwd"))
              (("/bin/false") (which "false")))
             (substitute* "tools/dtoc/fdt_util.py"
              (("'cc'") "'gcc'"))
             (substitute* "tools/patman/test_util.py"
              ;; python-coverage is simply called coverage in guix.
              (("python-coverage") "coverage")
              ;; XXX Allow for only 99% test coverage.
              ;; TODO: Find out why that is needed.
              (("if coverage != '100%':") "if not int(coverage.rstrip('%')) >= 99:"))
             (substitute* "test/run"
              ;; Make it easier to find test failures.
              (("#!/bin/bash") "#!/bin/bash -x")
              ;; pytest doesn't find it otherwise.
              (("test/py/tests/test_ofplatdata.py")
               "tests/test_ofplatdata.py")
              ;; This test would require git.
              (("\\./tools/patman/patman") (which "true"))
              ;; This test would require internet access.
              (("\\./tools/buildman/buildman") (which "true")))
             (substitute* "test/py/tests/test_sandbox_exit.py"
              (("def test_ctrl_c")
               "@pytest.mark.skip(reason='Guix has problems with SIGINT')
def test_ctrl_c"))
             (for-each (lambda (file)
                              (substitute* file
                                  ;; Disable signatures, due to GPL/Openssl
                                  ;; license incompatibilities.  See
                                  ;; https://bugs.gnu.org/34717 for details.
                                  (("CONFIG_FIT_SIGNATURE=y") "CONFIG_FIT_SIGNATURE=n")
                                  ;; This test requires a sound system, which is un-used
                                  ;; in u-boot-tools.
                                  (("CONFIG_SOUND=y") "CONFIG_SOUND=n")))
                              (find-files "configs" "sandbox_.*defconfig$"))
             #t))
         (replace 'configure
           (lambda* (#:key make-flags #:allow-other-keys)
             (call-with-output-file "configs/tools_defconfig"
               (lambda (port)
                 (display "CONFIG_SYS_TEXT_BASE=0\n" port)))
             (apply invoke "make" "tools_defconfig" make-flags)))
         (replace 'build
           (lambda* (#:key inputs make-flags #:allow-other-keys)
             (apply invoke "make" "tools-all" make-flags)))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (for-each (lambda (name)
                           (install-file name bin))
                         '("tools/netconsole"
                           "tools/jtagconsole"
                           "tools/gen_eth_addr"
                           "tools/gen_ethaddr_crc"
                           "tools/img2srec"
                           "tools/mkenvimage"
                           "tools/dumpimage"
                           "tools/mkimage"
                           "tools/proftool"
                           "tools/fdtgrep"
                           "tools/env/fw_printenv"
                           "tools/sunxi-spl-image-builder"))
               #t)))
           (delete 'check)
           (add-after 'install 'check
             (lambda* (#:key make-flags test-target #:allow-other-keys)
               (apply invoke "make" "mrproper" make-flags)
               (setenv "SDL_VIDEODRIVER" "dummy")
               (setenv "PAGER" "cat")
               (apply invoke "make" test-target make-flags)
               (symlink "build-sandbox_spl" "sandbox")
               (invoke "test/image/test-imagetools.sh"))))))
    (description "U-Boot is a bootloader used mostly for ARM boards.  It
also initializes the boards (RAM etc).  This package provides its
board-independent tools.")))

(define-public (make-u-boot-package board triplet)
  "Returns a u-boot package for BOARD cross-compiled for TRIPLET."
  (let ((same-arch? (if (string-prefix? (%current-system)
                                        (gnu-triplet->nix-system triplet))
                      `#t
                      `#f)))
    (package
      (inherit u-boot)
      (name (string-append "u-boot-"
                           (string-replace-substring (string-downcase board)
                                                     "_" "-")))
      (native-inputs
       `(,@(if (not same-arch?)
             `(("cross-gcc" ,(cross-gcc triplet #:xgcc gcc-7))
               ("cross-binutils" ,(cross-binutils triplet)))
             `(("gcc-7" ,gcc-7)))
         ,@(package-native-inputs u-boot)))
      (arguments
       `(#:modules ((ice-9 ftw)
                    (srfi srfi-1)
                    (guix build utils)
                    (guix build gnu-build-system))
         #:test-target "test"
         #:make-flags
         (list "HOSTCC=gcc"
               ,@(if (not same-arch?)
                   `((string-append "CROSS_COMPILE=" ,triplet "-"))
                   '()))
         #:phases
         (modify-phases %standard-phases
           (replace 'configure
             (lambda* (#:key outputs make-flags #:allow-other-keys)
               (let ((config-name (string-append ,board "_defconfig")))
                 (if (file-exists? (string-append "configs/" config-name))
                     (apply invoke "make" `(,@make-flags ,config-name))
                     (begin
                       (display "Invalid board name. Valid board names are:"
                                (current-error-port))
                       (let ((suffix-len (string-length "_defconfig"))
                             (entries (scandir "configs")))
                         (for-each (lambda (file-name)
                                     (when (string-suffix? "_defconfig" file-name)
                                       (format (current-error-port)
                                               "- ~A\n"
                                               (string-drop-right file-name
                                                                  suffix-len))))
                                   (sort entries string-ci<)))
                       (error "Invalid boardname ~s." ,board))))))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (libexec (string-append out "/libexec"))
                      (uboot-files (append
                                    (remove
                                     ;; Those would not be reproducible
                                     ;; because of the randomness used
                                     ;; to produce them.
                                     ;; It's expected that the user will
                                     ;; use u-boot-tools to generate them
                                     ;; instead.
                                     (lambda (name)
                                       (string-suffix?
                                        "sunxi-spl-with-ecc.bin"
                                        name))
                                     (find-files "." ".*\\.(bin|efi|img|spl|itb|dtb|rksd)$"))
                                    (find-files "." "^(MLO|SPL)$"))))
                 (mkdir-p libexec)
                 (install-file ".config" libexec)
                 ;; Useful for "qemu -kernel".
                 (install-file "u-boot" libexec)
                 (for-each
                  (lambda (file)
                    (let ((target-file (string-append libexec "/" file)))
                      (mkdir-p (dirname target-file))
                      (copy-file file target-file)))
                  uboot-files)
                 #t)))))))))

(define-public u-boot-vexpress
  (make-u-boot-package "vexpress_ca9x4" "arm-linux-gnueabihf"))

(define-public u-boot-malta
  (make-u-boot-package "malta" "mips64el-linux-gnuabi64"))

(define-public u-boot-am335x-boneblack
  (let ((base (make-u-boot-package "am335x_evm" "arm-linux-gnueabihf")))
    (package
      (inherit base)
      (name "u-boot-am335x-boneblack")
      (description "U-Boot is a bootloader used mostly for ARM boards. It
also initializes the boards (RAM etc).

This U-Boot is built for the BeagleBone Black, which was removed upstream,
adjusted from the am335x_evm build with several device trees removed so that
it fits within common partitioning schemes.")
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'unpack 'patch-defconfig
               ;; Patch out other devicetrees to build image small enough to
               ;; fit within typical partitioning schemes where the first
               ;; partition begins at sector 2048.
               (lambda _
                 (substitute* "configs/am335x_evm_defconfig"
                   (("CONFIG_OF_LIST=.*$") "CONFIG_OF_LIST=\"am335x-evm am335x-boneblack\"\n"))
                 #t)))))))))

(define-public u-boot-am335x-evm
  (make-u-boot-package "am335x_evm" "arm-linux-gnueabihf"))

(define-public (make-u-boot-sunxi64-package board triplet)
  (let ((base (make-u-boot-package board triplet)))
    (package
      (inherit base)
      (arguments
        (substitute-keyword-arguments (package-arguments base)
          ((#:phases phases)
           `(modify-phases ,phases
              (add-after 'unpack 'set-environment
                (lambda* (#:key inputs #:allow-other-keys)
                  (let ((bl31 (string-append (assoc-ref inputs "firmware")
                                             "/bl31.bin")))
                    (setenv "BL31" bl31)
                    ;; This is necessary when we're using the bundled dtc.
                    ;(setenv "PATH" (string-append (getenv "PATH") ":"
                    ;                              "scripts/dtc"))
                    )
                  #t))))))
      (native-inputs
       `(("firmware" ,arm-trusted-firmware-sun50i-a64)
         ,@(package-native-inputs base))))))

(define-public u-boot-pine64-plus
  (make-u-boot-sunxi64-package "pine64_plus" "aarch64-linux-gnu"))

(define-public u-boot-pinebook
  (make-u-boot-sunxi64-package "pinebook" "aarch64-linux-gnu"))

(define-public u-boot-bananapi-m2-ultra
  (make-u-boot-package "Bananapi_M2_Ultra" "arm-linux-gnueabihf"))

(define-public u-boot-a20-olinuxino-lime
  (make-u-boot-package "A20-OLinuXino-Lime" "arm-linux-gnueabihf"))

(define-public u-boot-a20-olinuxino-lime2
  (make-u-boot-package "A20-OLinuXino-Lime2" "arm-linux-gnueabihf"))

(define-public u-boot-a20-olinuxino-micro
  (make-u-boot-package "A20-OLinuXino_MICRO" "arm-linux-gnueabihf"))

(define-public u-boot-nintendo-nes-classic-edition
  (make-u-boot-package "Nintendo_NES_Classic_Edition" "arm-linux-gnueabihf"))

(define-public u-boot-wandboard
  (make-u-boot-package "wandboard" "arm-linux-gnueabihf"))

(define-public u-boot-mx6cuboxi
  (make-u-boot-package "mx6cuboxi" "arm-linux-gnueabihf"))

(define-public u-boot-novena
  (let ((base (make-u-boot-package "novena" "arm-linux-gnueabihf")))
    (package
      (inherit base)
      (description "U-Boot is a bootloader used mostly for ARM boards. It
also initializes the boards (RAM etc).

This U-Boot is built for Novena.  Be advised that this version, contrary
to Novena upstream, does not load u-boot.img from the first partition.")
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'unpack 'patch-novena-defconfig
               ;; Patch configuration to disable loading u-boot.img from FAT partition,
               ;; allowing it to be installed at a device offset.
               (lambda _
                 (substitute* "configs/novena_defconfig"
                   (("CONFIG_SPL_FS_FAT=y") "# CONFIG_SPL_FS_FAT is not set"))
                 #t)))))))))

(define-public u-boot-cubieboard
  (make-u-boot-package "Cubieboard" "arm-linux-gnueabihf"))

(define-public u-boot-cubietruck
  (make-u-boot-package "Cubietruck" "arm-linux-gnueabihf"))

(define-public u-boot-puma-rk3399
  (let ((base (make-u-boot-package "puma-rk3399" "aarch64-linux-gnu")))
    (package
      (inherit base)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'unpack 'set-environment
               (lambda* (#:key inputs #:allow-other-keys)
                 ;; Need to copy the firmware into u-boot build
                 ;; directory.
                 (copy-file (string-append (assoc-ref inputs "firmware")
                                           "/bl31.bin") "bl31-rk3399.bin")
                 (copy-file (string-append (assoc-ref inputs "firmware-m0")
                                           "/rk3399m0.bin") "rk3399m0.bin")
                 #t))
             (add-after 'build 'build-itb
               (lambda* (#:key make-flags #:allow-other-keys)
                 ;; The u-boot.itb is not built by default.
                 (apply invoke "make" `(,@make-flags ,"u-boot.itb"))))
             (add-after 'build-itb 'build-rksd
               (lambda* (#:key inputs #:allow-other-keys)
                 ;; Build Rockchip SD card images.
                 (invoke "./tools/mkimage" "-T" "rksd" "-n" "rk3399" "-d"
                         "spl/u-boot-spl.bin" "u-boot-spl.rksd")))))))
      (native-inputs
       `(("firmware" ,arm-trusted-firmware-puma-rk3399)
         ("firmware-m0" ,rk3399-cortex-m0)
         ,@(package-native-inputs base))))))

(define-public vboot-utils
  (package
    (name "vboot-utils")
    (version "R63-10032.B")
    (source (origin
              ;; XXX: Snapshots are available but changes timestamps every download.
              (method git-fetch)
              (uri (git-reference
                    (url (string-append "https://chromium.googlesource.com"
                                        "/chromiumos/platform/vboot_reference"))
                    (commit (string-append "release-" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0h0m3l69vp9dr6xrs1p6y7ilkq3jq8jraw2z20kqfv7lvc9l1lxj"))
              (patches
               (search-patches "vboot-utils-skip-test-workbuf.patch"
                               "vboot-utils-fix-tests-show-contents.patch"
                               "vboot-utils-fix-format-load-address.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list "CC=gcc"
                          ;; On ARM, we must pass "HOST_ARCH=arm" so that the
                          ;; ${HOST_ARCH} and ${ARCH} variables in the makefile
                          ;; match.  Otherwise, ${HOST_ARCH} will be assigned
                          ;; "armv7l", the value of `uname -m`, and will not
                          ;; match ${ARCH}, which will make the tests require
                          ;; QEMU for testing.
                          ,@(if (string-prefix? "arm"
                                                (or (%current-target-system)
                                                    (%current-system)))
                                '("HOST_ARCH=arm")
                                '())
                          (string-append "DESTDIR=" (assoc-ref %outputs "out")))
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-hard-coded-paths
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((coreutils (assoc-ref inputs "coreutils"))
                            (diffutils (assoc-ref inputs "diffutils")))
                        (substitute* "futility/misc.c"
                          (("/bin/cp") (string-append coreutils "/bin/cp")))
                        (substitute* "tests/bitmaps/TestBmpBlock.py"
                          (("/usr/bin/cmp") (string-append diffutils "/bin/cmp")))
                        (substitute* "vboot_host.pc.in"
                          (("prefix=/usr")
                           (string-append "prefix=" (assoc-ref outputs "out"))))
                        #t)))
                  (delete 'configure)
                  (add-before 'check 'patch-tests
                    (lambda _
                      ;; These tests compare diffs against known-good values.
                      ;; Patch the paths to match those in the build container.
                      (substitute* (find-files "tests/futility/expect_output")
                        (("/mnt/host/source/src/platform/vboot_reference")
                         (string-append "/tmp/guix-build-" ,name "-" ,version
                                        ".drv-0/source")))
                      ;; Tests require write permissions to many of these files.
                      (for-each make-file-writable (find-files "tests/futility"))
                      #t))
                  (add-after 'install 'install-devkeys
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (share (string-append out "/share/vboot-utils")))
                        (copy-recursively "tests/devkeys"
                                          (string-append share "/devkeys"))
                        #t))))
       #:test-target "runtests"))
    (native-inputs
     `(("pkg-config" ,pkg-config)

       ;; For tests.
       ("diffutils" ,diffutils)
       ("python@2" ,python-2)))
    (inputs
     `(("coreutils" ,coreutils)
       ("libyaml" ,libyaml)
       ("openssl" ,openssl)
       ("openssl:static" ,openssl "static")
       ("util-linux" ,util-linux)))
    (home-page
     "https://dev.chromium.org/chromium-os/chromiumos-design-docs/verified-boot")
    (synopsis "ChromiumOS verified boot utilities")
    (description
     "vboot-utils is a collection of tools to facilitate booting of
Chrome-branded devices.  This includes the @command{cgpt} partitioning
program, the @command{futility} and @command{crossystem} firmware management
tools, and more.")
    (license license:bsd-3)))

(define-public os-prober
  (package
    (name "os-prober")
    (version "1.77")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://debian/pool/main/o/os-prober/os-prober_"
                           version ".tar.xz"))
       (sha256
        (base32
         "0pvhrw4h05n21zw7ig3a3bi8aqdh6zxs0x1znz4g7vhspsps93ld"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 regex)         ; for string-match
                  (srfi srfi-26))       ; for cut
       #:make-flags (list "CC=gcc")
       #:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* (find-files ".")
               (("/usr") (assoc-ref outputs "out")))
             (substitute* (find-files "." "50mounted-tests$")
               (("mkdir") "mkdir -p"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (define (find-files-non-recursive directory)
               (find-files directory
                           (lambda (file stat)
                             (string-match (string-append "^" directory "/[^/]*$")
                                           file))
                           #:directories? #t))

             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lib (string-append out "/lib"))
                    (share (string-append out "/share")))
               (for-each (cut install-file <> bin)
                         (list "linux-boot-prober" "os-prober"))
               (install-file "newns" (string-append lib "/os-prober"))
               (install-file "common.sh" (string-append share "/os-prober"))
               (install-file "os-probes/mounted/powerpc/20macosx"
                             (string-append lib "/os-probes/mounted"))
               (for-each
                (lambda (directory)
                  (for-each
                   (lambda (file)
                     (let ((destination (string-append lib "/" directory
                                                       "/" (basename file))))
                       (mkdir-p (dirname destination))
                       (copy-recursively file destination)))
                   (append (find-files-non-recursive (string-append directory "/common"))
                           (find-files-non-recursive (string-append directory "/x86")))))
                (list "os-probes" "os-probes/mounted" "os-probes/init"
                      "linux-boot-probes" "linux-boot-probes/mounted"))
               #t))))))
    (home-page "https://joeyh.name/code/os-prober")
    (synopsis "Detect other operating systems")
    (description "os-prober probes disks on the system for other operating
systems so that they can be added to the bootloader.  It also works out how to
boot existing GNU/Linux systems and detects what distribution is installed in
order to add a suitable bootloader menu entry.")
    (license license:gpl2+)))
