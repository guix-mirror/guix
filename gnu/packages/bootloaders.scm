;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016, 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2016, 2017 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2016, 2017 David Craven <david@craven.ch>
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

(define-module (gnu packages bootloaders)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module ((gnu packages algebra) #:select (bc))
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages mtools)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages virtualization)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26))

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
               "03vvdfhdmf16121v7xs8is2krwnv15wpkhkf16a4yf8nsfc3f2w1"))))
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

       ("freetype" ,freetype)
       ;; ("libusb" ,libusb)
       ;; ("fuse" ,fuse)
       ("ncurses" ,ncurses)))
    (native-inputs
     `(("unifont" ,unifont)
       ("bison" ,bison)
       ;; Due to a bug in flex >= 2.6.2, GRUB must be built with an older flex:
       ;; <http://lists.gnu.org/archive/html/grub-devel/2017-02/msg00133.html>
       ;; TODO Try building with flex > 2.6.4.
       ("flex" ,flex-2.6.1)
       ("texinfo" ,texinfo)
       ("help2man" ,help2man)

       ;; Dependencies for the test suite.  The "real" QEMU is needed here,
       ;; because several targets are used.
       ("parted" ,parted)
       ("qemu" ,qemu-minimal)
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
       ("mtools", mtools)
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
                    (if (not (string-prefix? "." basename))
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
               (zero? (system* "chmod" "a+w" "utils/isohybrid.in"))))
           (replace 'check
             (lambda _
               (setenv "CC" "gcc")
               (substitute* "tests/unittest/include/unittest/unittest.h"
                 ;; Don't look up headers under /usr.
                 (("/usr/include/") ""))
               (zero? (system* "make" "unittest")))))))
      (home-page "http://www.syslinux.org")
      (synopsis "Lightweight Linux bootloader")
      (description "Syslinux is a lightweight Linux bootloader.")
      (license (list license:gpl2+
                     license:bsd-3 ; gnu-efi/*
                     license:bsd-4 ; gnu-efi/inc/* gnu-efi/lib/*
                     ;; Also contains:
                     license:expat license:isc license:zlib)))))

(define-public dtc
  (package
    (name "dtc")
    (version "1.4.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kernel.org/software/utils/dtc/"
                    "dtc-" version ".tar.xz"))
              (sha256
               (base32
                "08gnl39i4xy3dm8iqwlz2ygx0ml1bgc5kpiys5ll1wvah1j72b04"))
              ;; Fix build and tests on 32 bits platforms.
              ;; Will probably be fixed in 1.4.6 release.
              (patches (search-patches "dtc-format-modifier.patch"
                                       "dtc-32-bits-check.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)
       ("swig" ,swig)))
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
         (delete 'configure))))
    (home-page "https://www.devicetree.org")
    (synopsis "Compiles device tree source files")
    (description "@command{dtc} compiles
@uref{http://elinux.org/Device_Tree_Usage, device tree source files} to device
tree binary files.  These are board description files used by Linux and BSD.")
    (license license:gpl2+)))

(define u-boot
  (package
    (name "u-boot")
    (version "2017.11")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "ftp://ftp.denx.de/pub/u-boot/"
                    "u-boot-" version ".tar.bz2"))
              (sha256
               (base32
                "01bcsah5imy6m3fbjwhqywxg0pfk5fl8ks9ylb7kv3zmrb9qy0ba"))))
    (native-inputs
     `(("bc" ,bc)
       ("dtc" ,dtc)
       ("python-2" ,python-2)))
    (build-system  gnu-build-system)
    (home-page "http://www.denx.de/wiki/U-Boot/")
    (synopsis "ARM bootloader")
    (description "U-Boot is a bootloader used mostly for ARM boards. It
also initializes the boards (RAM etc).")
    (license license:gpl2+)))

(define (make-u-boot-package board triplet)
  "Returns a u-boot package for BOARD cross-compiled for TRIPLET."
  (let ((same-arch? (if (string-prefix? (%current-system)
                                        (gnu-triplet->nix-system triplet))
                      `#t
                      `#f)))
    (package
      (inherit u-boot)
      (name (string-append "u-boot-" (string-downcase board)))
      (native-inputs
       `(,@(if (not same-arch?)
             `(("cross-gcc" ,(cross-gcc triplet))
               ("cross-binutils" ,(cross-binutils triplet)))
             '())
         ,@(package-native-inputs u-boot)))
      (arguments
       `(#:modules ((ice-9 ftw) (guix build utils) (guix build gnu-build-system))
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
                     (zero? (apply system* "make" `(,@make-flags ,config-name)))
                     (begin
                       (display "Invalid board name. Valid board names are:")
                       (let ((suffix-len (string-length "_defconfig")))
                         (scandir "configs"
                                  (lambda (file-name)
                                    (when (string-suffix? "_defconfig" file-name)
                                      (format #t
                                              "- ~A\n"
                                              (string-drop-right file-name
                                                                 suffix-len))))))
                       #f)))))
           (replace 'install
             (lambda* (#:key outputs make-flags #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (libexec (string-append out "/libexec"))
                      (uboot-files (append
                                    (find-files "." ".*\\.(bin|efi|img|spl)$")
                                    (find-files "." "^MLO$"))))
                 (mkdir-p libexec)
                 (for-each
                  (lambda (file)
                    (let ((target-file (string-append libexec "/" file)))
                      (mkdir-p (dirname target-file))
                      (copy-file file target-file)))
                  uboot-files))))))))))

(define-public u-boot-vexpress
  (make-u-boot-package "vexpress_ca9x4" "arm-linux-gnueabihf"))

(define-public u-boot-malta
  (make-u-boot-package "malta" "mips64el-linux-gnuabi64"))

(define-public u-boot-beagle-bone-black
  (make-u-boot-package "am335x_boneblack" "arm-linux-gnueabihf"))

(define-public u-boot-odroid-c2
  (make-u-boot-package "odroid-c2" "aarch64-linux-gnu"))

(define-public os-prober
  (package
    (name "os-prober")
    (version "1.76")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://debian/pool/main/o/os-prober/os-prober_"
                           version ".tar.xz"))
       (sha256
        (base32
         "1vb45i76bqivlghrq7m3n07qfmmq4wxrkplqx8gywj011rhq19fk"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 regex)   ; for string-match
                  (srfi srfi-26)) ; for cut
       #:make-flags (list "CC=gcc")
       #:tests? #f ; no tests
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
