;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2014, 2015, 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015, 2016, 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Christopher Allan Webber <cwebber@dustycloud.org>
;;; Copyright © 2016, 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2016, 2017 Alex Kost <alezost@gmail.com>
;;; Copyright © 2016 Raymond Nicholson <rain1@openmailbox.org>
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2016, 2018, 2019 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2016, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016 John Darrington <jmd@gnu.org>
;;; Copyright © 2016, 2017, 2018, 2019 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2016, 2018 Rene Saavedra <pacoon@protonmail.com>
;;; Copyright © 2016 Carlos Sánchez de La Lama <csanchezdll@gmail.com>
;;; Copyright © 2016, 2017 ng0 <ng0@n0.is>
;;; Copyright © 2017, 2018 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 José Miguel Sánchez García <jmi2k@openmailbox.com>
;;; Copyright © 2017 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017, 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2017 nee <nee-git@hidamari.blue>
;;; Copyright © 2017 Dave Love <fx@gnu.org>
;;; Copyright © 2018 Pierre-Antoine Rouby <pierre-antoine.rouby@inria.fr>
;;; Copyright © 2018 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2018 Manuel Graf <graf@init.at>
;;; Copyright © 2018 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2018 Vasile Dumitrascu <va511e@yahoo.com>
;;; Copyright © 2019 Tim Gesthuizen <tim.gesthuizen@yahoo.de>
;;; Copyright © 2019 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu packages linux)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages calendar)
  #:use-module (gnu packages check)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages cryptsetup)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages file)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages netpbm)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages rrdtool)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages slang)
  #:use-module (gnu packages storage)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages selinux)
  #:use-module (gnu packages swig)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system linux-module)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match))

(define-public (system->linux-architecture arch)
  "Return the Linux architecture name for ARCH, a Guix system name such as
\"x86_64-linux\" or a target triplet such as \"arm-linux-gnueabihf\"."
  (let ((arch (car (string-split arch #\-))))
    (cond ((string=? arch "i686") "i386")
          ((string-prefix? "mips" arch) "mips")
          ((string-prefix? "arm" arch) "arm")
          ((string-prefix? "aarch64" arch) "arm64")
          ((string-prefix? "alpha" arch) "alpha")
          ((string-prefix? "powerpc" arch) "powerpc") ;including "powerpc64le"
          ((string-prefix? "s390" arch) "s390")
          (else arch))))

(define-public (system->defconfig system)
  "Some systems (notably powerpc-linux) require a special target for kernel
defconfig.  Return the appropriate make target if applicable, otherwise return
\"defconfig\"."
  (cond ((string-prefix? "powerpc-" system) "pmac32_defconfig")
        ((string-prefix? "powerpc64le-" system) "ppc64_defconfig")
        (else "defconfig")))

(define (linux-libre-urls version)
  "Return a list of URLs for Linux-Libre VERSION."
  (list (string-append
         "https://linux-libre.fsfla.org/pub/linux-libre/releases/"
         version "-gnu/linux-libre-" version "-gnu.tar.xz")

        ;; XXX: Work around <http://bugs.gnu.org/14851>.
        (string-append
         "ftp://alpha.gnu.org/gnu/guix/mirror/linux-libre-"
         version "-gnu.tar.xz")

        ;; Maybe this URL will become valid eventually.
        (string-append
         "mirror://gnu/linux-libre/" version "-gnu/linux-libre-"
         version "-gnu.tar.xz")))

(define-public linux-libre-headers
  (package
    (name "linux-libre-headers")
    (version "4.14.67")
    (source (origin
             (method url-fetch)
             (uri (linux-libre-urls version))
             (sha256
              (base32
               "050zvdxjy6sc64q75pr1gxsmh49chwav2pwxz8xlif39bvahnrpg"))))
    (build-system gnu-build-system)
    (native-inputs `(("perl" ,perl)))
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda _
             (let ((arch ,(system->linux-architecture
                          (or (%current-target-system)
                              (%current-system))))
                   (defconfig ,(system->defconfig
                                (or (%current-target-system)
                                    (%current-system)))))
               (setenv "ARCH" arch)
               (format #t "`ARCH' set to `~a'~%" (getenv "ARCH"))
               (invoke "make" defconfig)
               (invoke "make" "mrproper" "headers_check"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke "make"
                       (string-append "INSTALL_HDR_PATH=" out)
                       "headers_install")

               (mkdir (string-append out "/include/config"))
               (call-with-output-file
                   (string-append out
                                  "/include/config/kernel.release")
                 (lambda (p)
                   (format p "~a-default~%" ,version)))

               ;; Remove the '.install' and '..install.cmd' files; the
               ;; latter contains store paths, which pulls in bootstrap
               ;; binaries in the build environment, and prevents bit
               ;; reproducibility for the bootstrap binaries.
               (for-each delete-file (find-files out "\\.install"))

               #t))))
       #:allowed-references ()
       #:tests? #f))
    (home-page "https://www.gnu.org/software/linux-libre/")
    (synopsis "GNU Linux-Libre kernel headers")
    (description "Headers of the Linux-Libre kernel.")
    (license license:gpl2)))

(define %boot-logo-patch
  ;; Linux-Libre boot logo featuring Freedo and a gnu.
  (origin
    (method url-fetch)
    (uri (string-append "http://www.fsfla.org/svn/fsfla/software/linux-libre/"
                        "lemote/gnewsense/branches/3.16/100gnu+freedo.patch"))
    (sha256
     (base32
      "1hk9swxxc80bmn2zd2qr5ccrjrk28xkypwhl4z0qx4hbivj7qm06"))))

(define %linux-libre-arm-export-__sync_icache_dcache-patch
  (origin
    (method url-fetch)
    (uri (string-append
          "https://salsa.debian.org/kernel-team/linux"
          "/raw/34a7d9011fcfcfa38b68282fd2b1a8797e6834f0"
          "/debian/patches/bugfix/arm/"
          "arm-mm-export-__sync_icache_dcache-for-xen-privcmd.patch"))
    (file-name "linux-libre-4.19-arm-export-__sync_icache_dcache.patch")
    (sha256
     (base32 "1ifnfhpakzffn4b8n7x7w5cps9mzjxlkcfz9zqak2vaw8nzvl39f"))))

(define* (kernel-config arch #:key variant)
  "Return the absolute file name of the Linux-Libre build configuration file
for ARCH and optionally VARIANT, or #f if there is no such configuration."
  (let* ((name (string-append (if variant (string-append variant "-") "")
                              (if (string=? "i386" arch) "i686" arch) ".conf"))
         (file (string-append "linux-libre/" name)))
    (search-auxiliary-file file)))

;; FIXME: merge into kernel-config
(define* (kernel-config-veyron arch #:key variant)
  "Return the absolute file name of the Linux-Libre build configuration file
for ARCH and optionally VARIANT, or #f if there is no such configuration."
  (let* ((name (string-append (if variant (string-append variant "-") "")
                              (if (string=? "i386" arch) "i686" arch) "-veyron.conf"))
         (file (string-append "linux-libre/" name)))
    (search-auxiliary-file file)))

(define %default-extra-linux-options
  `(;; Modules required for initrd:
    ("CONFIG_NET_9P" . m)
    ("CONFIG_NET_9P_VIRTIO" . m)
    ("CONFIG_VIRTIO_BLK" . m)
    ("CONFIG_VIRTIO_NET" . m)
    ("CONFIG_VIRTIO_PCI" . m)
    ("CONFIG_VIRTIO_BALLOON" . m)
    ("CONFIG_VIRTIO_MMIO" . m)
    ("CONFIG_FUSE_FS" . m)
    ("CONFIG_CIFS" . m)
    ("CONFIG_9P_FS" . m)))

(define (config->string options)
  (string-join (map (match-lambda
                      ((option . 'm)
                       (string-append option "=m"))
                      ((option . #t)
                       (string-append option "=y"))
                      ((option . #f)
                       (string-append option "=n")))
                    options)
               "\n"))

(define* (make-linux-libre version hash supported-systems
                           #:key
                           ;; A function that takes an arch and a variant.
                           ;; See kernel-config for an example.
                           (extra-version #f)
                           (configuration-file #f)
                           (defconfig "defconfig")
                           (extra-options %default-extra-linux-options)
                           (patches (list %boot-logo-patch)))
  (package
    (name (if extra-version
              (string-append "linux-libre-" extra-version)
              "linux-libre"))
    (version version)
    (source (origin
              (method url-fetch)
              (uri (linux-libre-urls version))
              (sha256 (base32 hash))
              (patches patches)))
    (supported-systems supported-systems)
    (build-system gnu-build-system)
    (native-inputs
     `(("perl" ,perl)
       ("bc" ,bc)
       ("openssl" ,openssl)
       ("kmod" ,kmod)
       ("elfutils" ,elfutils)  ; Needed to enable CONFIG_STACK_VALIDATION
       ("flex" ,flex)
       ("bison" ,bison)

       ;; Build with GCC-7 for full retpoline support.
       ;; FIXME: Remove this when our default compiler has retpoline support.
       ("gcc" ,gcc-7)

       ;; These are needed to compile the GCC plugins.
       ("gmp" ,gmp)
       ("mpfr" ,mpfr)
       ("mpc" ,mpc)

       ,@(match (and configuration-file
                     (configuration-file
                      (system->linux-architecture
                       (or (%current-target-system) (%current-system)))
                      #:variant (version-major+minor version)))
           (#f                                    ;no config for this platform
            '())
           ((? string? config)
            `(("kconfig" ,config))))))
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1)
                  (ice-9 match))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-/bin/pwd
           (lambda _
             (substitute* (find-files "." "^Makefile(\\.include)?$")
               (("/bin/pwd") "pwd"))
             #t))
         (add-before 'configure 'work-around-gcc-7-include-path-issue
           (lambda _
             (unsetenv "C_INCLUDE_PATH")
             (unsetenv "CPLUS_INCLUDE_PATH")
             #t))
         (replace 'configure
           (lambda* (#:key inputs native-inputs target #:allow-other-keys)
             ;; Avoid introducing timestamps
             (setenv "KCONFIG_NOTIMESTAMP" "1")
             (setenv "KBUILD_BUILD_TIMESTAMP" (getenv "SOURCE_DATE_EPOCH"))

             ;; Set ARCH and CROSS_COMPILE
             (let ((arch ,(system->linux-architecture
                           (or (%current-target-system)
                               (%current-system)))))
               (setenv "ARCH" arch)
               (format #t "`ARCH' set to `~a'~%" (getenv "ARCH"))

               (when target
                 (setenv "CROSS_COMPILE" (string-append target "-"))
                 (format #t "`CROSS_COMPILE' set to `~a'~%"
                         (getenv "CROSS_COMPILE"))))

             (setenv "EXTRA_VERSION" ,extra-version)

             (let ((build  (assoc-ref %standard-phases 'build))
                   (config (assoc-ref (or native-inputs inputs) "kconfig")))

               ;; Use a custom kernel configuration file or a default
               ;; configuration file.
               (if config
                   (begin
                     (copy-file config ".config")
                     (chmod ".config" #o666))
                   (invoke "make" ,defconfig))

               ;; Appending works even when the option wasn't in the
               ;; file.  The last one prevails if duplicated.
               (let ((port (open-file ".config" "a"))
                     (extra-configuration ,(config->string extra-options)))
                 (display extra-configuration port)
                 (close-port port))

               (invoke "make" "oldconfig"))))
         (replace 'install
           (lambda* (#:key inputs native-inputs outputs #:allow-other-keys)
             (let* ((out    (assoc-ref outputs "out"))
                    (moddir (string-append out "/lib/modules"))
                    (dtbdir (string-append out "/lib/dtbs"))
                    (kmod   (assoc-ref (or native-inputs inputs) "kmod")))
               ;; Install kernel image, kernel configuration and link map.
               (for-each (lambda (file) (install-file file out))
                         (find-files "." "^(\\.config|bzImage|zImage|Image|vmlinuz|System\\.map|Module\\.symvers)$"))
               ;; Install device tree files
               (unless (null? (find-files "." "\\.dtb$"))
                 (mkdir-p dtbdir)
                 (invoke "make" (string-append "INSTALL_DTBS_PATH=" dtbdir)
                         "dtbs_install"))
               ;; Install kernel modules
               (mkdir-p moddir)
               (invoke "make"
                       (string-append "DEPMOD=" kmod "/bin/depmod")
                       (string-append "MODULE_DIR=" moddir)
                       (string-append "INSTALL_PATH=" out)
                       (string-append "INSTALL_MOD_PATH=" out)
                       "INSTALL_MOD_STRIP=1"
                       "modules_install")))))
       #:tests? #f))
    (home-page "https://www.gnu.org/software/linux-libre/")
    (synopsis "100% free redistribution of a cleaned Linux kernel")
    (description
     "GNU Linux-Libre is a free (as in freedom) variant of the Linux kernel.
It has been modified to remove all non-free binary blobs.")
    (license license:gpl2)))

(define %linux-libre-version "5.0.14")
(define %linux-libre-hash "1y9wfn814h8p9k75nh1h42m35qbz9jw0kzp7bvjmrw9gvwf98cdg")

(define %linux-libre-5.0-patches
  (list %boot-logo-patch
        %linux-libre-arm-export-__sync_icache_dcache-patch))

(define-public linux-libre
  (make-linux-libre %linux-libre-version
                    %linux-libre-hash
                    '("x86_64-linux" "i686-linux" "armhf-linux" "aarch64-linux")
                    #:patches %linux-libre-5.0-patches
                    #:configuration-file kernel-config))

(define-public vhba-module
  (package
    (name "vhba-module")
    (version "20170610")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://downloads.sourceforge.net/cdemu/vhba-module-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "1v6r0bgx0a65vlh36b1l2965xybngbpga6rp54k4z74xk0zwjw3r"))))
    (build-system linux-module-build-system)
    (arguments
     ;; TODO: No tests?
     `(#:tests? #f))
    (home-page "https://cdemu.sourceforge.io/")
    (synopsis "Kernel module that emulates SCSI devices")
    (description "VHBA module provides a Virtual (SCSI) HBA, which is the link
between the CDemu userspace daemon and linux kernel.")
    (license license:gpl2+)))

(define %linux-libre-4.19-version "4.19.41")
(define %linux-libre-4.19-hash "0lf1w72y4qkrg57qd78zs16r658qaqzwly0y9195nf7bdwqzb03v")

(define %linux-libre-4.19-patches
  (list %boot-logo-patch
        %linux-libre-arm-export-__sync_icache_dcache-patch))

(define-public linux-libre-4.19
  (make-linux-libre %linux-libre-4.19-version
                    %linux-libre-4.19-hash
                    '("x86_64-linux" "i686-linux" "armhf-linux" "aarch64-linux")
                    #:patches %linux-libre-4.19-patches
                    #:configuration-file kernel-config))

(define %linux-libre-4.14-version "4.14.117")
(define %linux-libre-4.14-hash "14sy035zdikl3xr0wyg3srr4b2jjlvq7wzj6b0b74y90fa2s1rz5")

(define-public linux-libre-4.14
  (make-linux-libre %linux-libre-4.14-version
                    %linux-libre-4.14-hash
                    '("x86_64-linux" "i686-linux" "armhf-linux")
                    #:configuration-file kernel-config))

(define-public linux-libre-4.9
  (make-linux-libre "4.9.174"
                    "0f7v96qbxdcrr06b00lh3n2nljp1zfbx7iqvzha4y47z76hfv144"
                    '("x86_64-linux" "i686-linux")
                    #:configuration-file kernel-config))

(define-public linux-libre-4.4
  (make-linux-libre "4.4.179"
                    "025jl50sgi3bxj8hxlihqyfshmfphrg6z3cfi043qwkc8sbdy3af"
                    '("x86_64-linux" "i686-linux")
                    #:configuration-file kernel-config
                    #:extra-options
                    (append
                     `(;; https://lists.gnu.org/archive/html/guix-devel/2014-04/msg00039.html
                       ;; This option was removed upstream in version 4.7.
                       ("CONFIG_DEVPTS_MULTIPLE_INSTANCES" . #t))
                     %default-extra-linux-options)))

(define-public linux-libre-arm-generic
  (make-linux-libre %linux-libre-version
                    %linux-libre-hash
                    '("armhf-linux")
                    #:patches %linux-libre-5.0-patches
                    #:defconfig "multi_v7_defconfig"
                    #:extra-version "arm-generic"))

(define-public linux-libre-arm-veyron
  (make-linux-libre %linux-libre-version
                    %linux-libre-hash
                    '("armhf-linux")
                    #:patches %linux-libre-5.0-patches
                    #:configuration-file kernel-config-veyron
                    #:extra-version "arm-veyron"))

(define-public linux-libre-arm-generic-4.19
  (make-linux-libre %linux-libre-4.19-version
                    %linux-libre-4.19-hash
                    '("armhf-linux")
                    #:patches %linux-libre-4.19-patches
                    #:defconfig "multi_v7_defconfig"
                    #:extra-version "arm-generic"))

(define-public linux-libre-arm-generic-4.14
  (make-linux-libre %linux-libre-4.14-version
                    %linux-libre-4.14-hash
                    '("armhf-linux")
                    #:defconfig "multi_v7_defconfig"
                    #:extra-version "arm-generic"))

(define-public linux-libre-arm-omap2plus
  (make-linux-libre %linux-libre-version
                    %linux-libre-hash
                    '("armhf-linux")
                    #:patches %linux-libre-5.0-patches
                    #:defconfig "omap2plus_defconfig"
                    #:extra-version "arm-omap2plus"))

(define-public linux-libre-arm-omap2plus-4.19
  (make-linux-libre %linux-libre-4.19-version
                    %linux-libre-4.19-hash
                    '("armhf-linux")
                    #:patches %linux-libre-4.19-patches
                    #:defconfig "omap2plus_defconfig"
                    #:extra-version "arm-omap2plus"))

(define-public linux-libre-arm-omap2plus-4.14
  (make-linux-libre %linux-libre-4.14-version
                    %linux-libre-4.14-hash
                    '("armhf-linux")
                    #:defconfig "omap2plus_defconfig"
                    #:extra-version "arm-omap2plus"))


;;;
;;; Pluggable authentication modules (PAM).
;;;

(define-public linux-pam
  (package
    (name "linux-pam")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/linux-pam/linux-pam/releases/download/v"
             version "/Linux-PAM-" version ".tar.xz"))
       (sha256
        (base32
         "1nyh9kdi3knhxcbv5v4snya0g3gff0m671lnvqcbygw3rm77mx7g"))
       (patches (search-patches "linux-pam-no-setfsuid.patch"))))

    (build-system gnu-build-system)
    (native-inputs
     `(("flex" ,flex)

       ;; TODO: optional dependencies
       ;; ("libxcrypt" ,libxcrypt)
       ;; ("cracklib" ,cracklib)
       ))
    (arguments
     `(;; Most users, such as `shadow', expect the headers to be under
       ;; `security'.
       #:configure-flags (list (string-append "--includedir="
                                              (assoc-ref %outputs "out")
                                              "/include/security")

                               ;; XXX: <rpc/rpc.h> is missing from glibc when
                               ;; cross-compiling, so we have to disable NIS
                               ;; support altogether.
                               ,@(if (%current-target-system)
                                     '("--disable-nis")
                                     '()))

       ;; XXX: Tests won't run in chroot, presumably because /etc/pam.d
       ;; isn't available.
       #:tests? #f))
    (home-page "http://www.linux-pam.org/")
    (synopsis "Pluggable authentication modules for Linux")
    (description
     "A *Free* project to implement OSF's RFC 86.0.
Pluggable authentication modules are small shared object files that can
be used through the PAM API to perform tasks, like authenticating a user
at login.  Local and dynamic reconfiguration are its key features.")
    (license license:bsd-3)))

(define-public linux-pam-1.2
  (package
    (inherit linux-pam)
    (version "1.2.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "http://www.linux-pam.org/library/"
            "Linux-PAM-" version ".tar.bz2"))
      (sha256
       (base32
        "1n9lnf9gjs72kbj1g354v1xhi2j27aqaah15vykh7cnkq08i4arl"))
      (patches (search-patches "linux-pam-no-setfsuid.patch"))))))


;;;
;;; Miscellaneous.
;;;

(define-public psmisc
  (package
    (name "psmisc")
    (version "23.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/psmisc/psmisc devel/psmisc-"
                          version ".tar.xz"))
      (sha256
       (base32
        "0s1kjhrik0wzqbm7hv4gkhywhjrwhp9ajw0ad05fwharikk6ah49"))))
    (build-system gnu-build-system)
    (inputs `(("ncurses" ,ncurses)))
    (home-page "https://gitlab.com/psmisc/psmisc")
    (synopsis
     "Small utilities that use the proc file system")
    (description
     "This PSmisc package is a set of some small useful utilities that
use the proc file system.  We're not about changing the world, but
providing the system administrator with some help in common tasks.")
    (license license:gpl2+)))

(define-public util-linux
  (package
    (name "util-linux")
    (version "2.32.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kernel.org/linux/utils/"
                                  name "/v" (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1ck7d8srw5szpjq7v0gpmjahnjs6wgqzm311ki4gazww6xx71rl6"))
              (patches (search-patches "util-linux-tests.patch"))
              (modules '((guix build utils)))
              (snippet
               ;; We take 'nologin' from Shadow, the 'logger' program from
               ;; GNU Inetutils and 'kill' from GNU Coreutils.
               '(begin
                  (substitute* "configure"
                    (("build_nologin=yes") "build_nologin=no")
                    (("build_logger=yes") "build_logger=no")
                    (("build_kill=yes") "build_kill=no"))
                  #t))))
    (build-system gnu-build-system)
    (outputs '("out"
               "static"))      ; >2 MiB of static .a libraries
    (arguments
     `(#:configure-flags (list "--disable-use-tty-group"
                               "--enable-fs-paths-default=/run/current-system/profile/sbin"
                               ;; Don't try to chown root:root mount and umount
                               "--disable-makeinstall-chown"
                               ;; Install completions where our
                               ;; bash-completion package expects them.
                               (string-append "--with-bashcompletiondir="
                                              (assoc-ref %outputs "out")
                                              "/etc/bash_completion.d"))
       #:phases (modify-phases %standard-phases
                  (add-before
                   'build 'set-umount-file-name
                   (lambda* (#:key outputs #:allow-other-keys)
                     ;; Tell 'eject' the right file name of 'umount'.
                     (let ((out (assoc-ref outputs "out")))
                       (substitute* "sys-utils/eject.c"
                         (("\"/bin/umount\"")
                          (string-append "\"" out "/bin/umount\"")))
                       #t)))
                  (add-before
                   'check 'pre-check
                   (lambda* (#:key inputs outputs #:allow-other-keys)
                     (let ((out (assoc-ref outputs "out"))
                           (net (assoc-ref inputs "net-base")))
                       ;; Change the test to refer to the right file.
                       (substitute* "tests/ts/misc/mcookie"
                         (("/etc/services")
                          (string-append net "/etc/services")))
                       #t)))
                  (add-after
                   'install 'move-static-libraries
                   (lambda* (#:key outputs #:allow-other-keys)
                     (let ((out    (assoc-ref outputs "out"))
                           (static (assoc-ref outputs "static")))
                       (mkdir-p (string-append static "/lib"))
                       (with-directory-excursion out
                         (for-each (lambda (file)
                                     (rename-file file
                                                  (string-append static "/"
                                                                 file)))
                                   (find-files "lib" "\\.a$"))

                         ;; Remove references to the static library from the '.la'
                         ;; files so that Libtool does the right thing when both
                         ;; the shared and static library is available.
                         (substitute* (find-files "lib" "\\.la$")
                           (("old_library=.*") "old_library=''\n")))

                       #t))))))
    (inputs `(("zlib" ,zlib)
              ("ncurses" ,ncurses)

              ;; XXX: This is so that the 'pre-check' phase can find it.
              ,@(if (%current-target-system)
                    `(("net-base" ,net-base))
                    '())))
    (native-inputs
     `(("perl" ,perl)
       ("net-base" ,net-base)))                   ;for tests
    (home-page "https://www.kernel.org/pub/linux/utils/util-linux/")
    (synopsis "Collection of utilities for the Linux kernel")
    (description "Util-linux is a diverse collection of Linux kernel
utilities.  It provides dmesg and includes tools for working with file systems,
block devices, UUIDs, TTYs, and many other tools.")

    ;; Note that util-linux doesn't use the same license for all the
    ;; code.  GPLv2+ is the default license for a code without an
    ;; explicitly defined license.
    (license (list license:gpl3+ license:gpl2+ license:gpl2 license:lgpl2.0+
                   license:bsd-4 license:public-domain))))

(define-public ddate
  (package
    (name "ddate")
    (version "0.2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/bo0ts/ddate/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "1bbqqq8mswj4bp9083gxjaky5ysfznax4cynsqwmy125z053yg6m"))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f))
    (home-page "https://github.com/bo0ts/ddate")
    (synopsis "PERPETUAL DATE CONVERTER FROM GREGORIAN TO POEE CALENDAR")
    (description
     "ddate displays the Discordian date and holidays of a given date.
The Discordian calendar was made popular by the \"Illuminatus!\" trilogy
by Robert Shea and Robert Anton Wilson.")
    (license license:public-domain)))

(define-public procps
  (package
    (name "procps")
    (version "3.3.15")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/procps-ng/Production/"
                                  "procps-ng-" version ".tar.xz"))
              (sha256
               (base32
                "0r84kwa5fl0sjdashcn4vh7hgfm7ahdcysig3mcjvpmkzi7p9g8h"))))
    (build-system gnu-build-system)
    (arguments
     '(#:modules ((guix build utils)
                  (guix build gnu-build-system)
                  (srfi srfi-1)
                  (srfi srfi-26))
       #:phases
       (modify-phases %standard-phases
         (add-after
          'install 'post-install
          ;; Remove commands and man pages redudant with
          ;; Coreutils.
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (dup (append-map (cut find-files out <>)
                                    '("^kill" "^uptime"))))
              (for-each delete-file dup)
              #t))))))
    (inputs `(("ncurses" ,ncurses)))
    (home-page "https://gitlab.com/procps-ng/procps/")
    (synopsis "Utilities that give information about processes")
    (description
     "Procps is the package that has a bunch of small useful utilities
that give information about processes using the Linux /proc file system.
The package includes the programs ps, top, vmstat, w, kill, free,
slabtop, and skill.")
    (license license:gpl2)))

(define-public usbutils
  (package
    (name "usbutils")
    (version "012")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kernel.org/linux/utils/usb/usbutils/"
                          "usbutils-" version ".tar.xz"))
      (sha256
       (base32 "0iiy0q7fzikavmdsjsb0sl9kp3gfh701qwyjjccvqh0qz4jlcqw8"))))
    (build-system gnu-build-system)
    (outputs (list "out" "python"))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'bootstrap 'patch-bootstrap-scripts
           (lambda _
             (substitute* "usbhid-dump/bootstrap"
               (("/bin/bash") (which "bash")))

             ;; Don't let autogen.sh run configure with bogus options & CFLAGS.
             (substitute* "autogen.sh"
               (("^\\./configure.*") ""))
             #t))
         (add-after 'install 'separate-python-output
           ;; Separating one Python script shaves more than 106 MiB from :out.
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out        (assoc-ref outputs "out"))
                   (out:python (assoc-ref outputs "python")))
               (for-each (lambda (file)
                           (let ((old (string-append out "/" file))
                                 (new (string-append out:python "/" file)))
                             (mkdir-p (dirname new))
                             (rename-file old new)))
                         (list "bin/lsusb.py"))
               #t))))))
    (inputs
     `(("eudev" ,eudev)
       ("libusb" ,libusb)
       ("python" ,python)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (home-page "http://www.linux-usb.org/")
    (synopsis
     "Tools for working with USB devices, such as lsusb")
    (description
     "Tools for working with USB devices, such as lsusb.")
    (license license:gpl2+)))

(define-public e2fsprogs
  (package
    (name "e2fsprogs")
    (version "1.44.5")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://kernel.org/linux/kernel/people/tytso/"
                   name "/v" version "/"
                   name "-" version ".tar.xz"))
             (sha256
              (base32
               "1ff56h6h1h17sj2zvlddv5c88nmbx46p1fcbh6b0s5k9kl3b6pms"))))
    (build-system gnu-build-system)
    (inputs `(("util-linux" ,util-linux)))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("texinfo" ,texinfo)       ;for the libext2fs Info manual

                     ;; For tests.
                     ("perl" ,perl)
                     ("procps" ,procps)))
    (arguments
     '(;; util-linux is the preferred source for some of the libraries and
       ;; commands, so disable them (see, e.g.,
       ;; <http://git.buildroot.net/buildroot/commit/?id=e1ffc2f791b33633>.)
       #:configure-flags (list "--disable-libblkid"
                               "--disable-libuuid" "--disable-uuidd"
                               "--disable-fsck"

                               ;; Use symlinks instead of hard links for
                               ;; 'fsck.extN' etc.  This makes the resulting nar
                               ;; smaller and is preserved across copies.
                               "--enable-symlink-install"

                               (string-append "LDFLAGS=-Wl,-rpath="
                                              (assoc-ref %outputs "out")
                                              "/lib")

                               ;; Install libext2fs et al.
                               "--enable-elf-shlibs")

       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-shells
           (lambda _
             (substitute* "configure"
               (("/bin/sh (.*)parse-types.sh" _ dir)
                (string-append (which "sh") " " dir
                               "parse-types.sh")))
             (substitute* "MCONFIG.in"
               (("INSTALL_SYMLINK = /bin/sh")
                "INSTALL_SYMLINK = sh"))
             (substitute* (find-files "." "^Makefile.in$")
               (("#!/bin/sh")
                (string-append "#!" (which "sh"))))
             #t))
           (add-after 'install 'install-libs
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (lib (string-append out "/lib")))
                 (invoke "make" "install-libs")

                 ;; Make the .a writable so that 'strip' works.
                 ;; Failing to do that, due to debug symbols, we
                 ;; retain a reference to the final
                 ;; linux-libre-headers, which refer to the
                 ;; bootstrap binaries.
                 (let ((archives (find-files lib "\\.a$")))
                   (for-each (lambda (file)
                               (chmod file #o666))
                             archives))
                 #t))))))
    (home-page "http://e2fsprogs.sourceforge.net/")
    (synopsis "Creating and checking ext2/ext3/ext4 file systems")
    (description
     "This package provides tools for manipulating ext2/ext3/ext4 file systems.")
    (license (list license:gpl2                   ;programs
                   license:lgpl2.0                ;libext2fs
                   license:x11))))                ;libuuid

(define e2fsprogs/static
  (static-package
   (package (inherit e2fsprogs)
            (arguments
             ;; Do not build shared libraries.
             (substitute-keyword-arguments (package-arguments e2fsprogs)
               ((#:configure-flags _)
                '(list "--disable-blkid"))
               ((#:make-flags _)
                '(list)))))))

(define-public e2fsck/static
  (package
    (name "e2fsck-static")
    (version (package-version e2fsprogs))
    (build-system trivial-build-system)
    (source #f)
    (inputs
     `(("e2fsprogs" ,e2fsprogs/static)))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 ftw)
                      (srfi srfi-26))

         (let ((e2fsck (string-append (assoc-ref %build-inputs "e2fsprogs")
                                      "/sbin/e2fsck"))
               (bin    (string-append (assoc-ref %outputs "out") "/sbin")))
           (mkdir-p bin)
           (with-directory-excursion bin
             (copy-file e2fsck "e2fsck")
             (remove-store-references "e2fsck")
             (chmod "e2fsck" #o555))
           #t))))
    (home-page (package-home-page e2fsprogs))
    (synopsis "Statically-linked e2fsck command from e2fsprogs")
    (description "This package provides statically-linked e2fsck command taken
from the e2fsprogs package.  It is meant to be used in initrds.")
    (license (package-license e2fsprogs))))

(define-public extundelete
  (package
    (name "extundelete")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/extundelete/"
                           "extundelete/" version "/extundelete-"
                           version ".tar.bz2"))
       (sha256
        (base32
         "1x0r7ylxlp9lbj3d7sqf6j2a222dwy2nfpff05jd6mkh4ihxvyd1"))
       (patches (search-patches "extundelete-e2fsprogs-1.44.patch"))))
    (build-system gnu-build-system)
    (inputs `(("e2fsprogs" ,e2fsprogs)))
    (home-page "http://extundelete.sourceforge.net/")
    (synopsis "Recover deleted files from ext2/3/4 partitions")
    (description
     "Extundelete is a set of tools that can recover deleted files from an
ext3 or ext4 partition.")
    (license license:gpl2)))

(define-public zerofree
  (package
    (name "zerofree")
    (version "1.1.1")
    (home-page "https://frippery.org/uml/")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page name "-" version
                                  ".tgz"))
              (sha256
               (base32
                "0rrqfa5z103ws89vi8kfvbks1cfs74ix6n1wb6vs582vnmhwhswm"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (replace 'install
           ;; The Makefile lacks an ‘install’ target.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (chmod "zerofree" #o555)
               (install-file "zerofree" bin)
               #t))))
       #:tests? #f))                    ; no tests
    (inputs `(("libext2fs" ,e2fsprogs)))
    (synopsis "Zero non-allocated regions in ext2/ext3/ext4 file systems")
    (description
     "Zerofree finds the unallocated blocks with non-zero value content in an
ext2, ext3, or ext4 file system and fills them with zeroes (or another value).
This is a simple way to make disk images more compressible.
Zerofree requires the file system to be unmounted or mounted read-only.")
    (license license:gpl2)))

(define-public strace
  (package
    (name "strace")
    (version "5.0")
    (home-page "https://strace.io")
    (source (origin
             (method url-fetch)
             (uri (string-append home-page "/files/" version
                                 "/strace-" version ".tar.xz"))
             (sha256
              (base32
               "1nj7wvsdmhpp53yffj1pnrkjn96mxrbcraa6h03wc7dqn9zdfyiv"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-/bin/sh
           (lambda _
             (substitute* "strace.c"
               (("/bin/sh") (which "sh")))
             #t)))
       ;; Don't fail if the architecture doesn't support different personalities.
       #:configure-flags '("--enable-mpers=check")
       ;; See <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=32459>.
       #:parallel-tests? #f))           ; undeterministic failures
    (native-inputs `(("perl" ,perl)))
    (synopsis "System call tracer for Linux")
    (description
     "strace is a system call tracer, i.e. a debugging tool which prints out a
trace of all the system calls made by a another process/program.")
    (license license:lgpl2.1+)))

(define-public ltrace
  (package
    (name "ltrace")
    (version "0.7.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://www.ltrace.org/ltrace_" version
                                 ".orig.tar.bz2"))
             (sha256
              (base32
               "00wmbdghqbz6x95m1mcdd3wd46l6hgcr4wggdp049dbifh3qqvqf"))))
    (build-system gnu-build-system)
    (inputs `(("libelf" ,libelf)))
    (arguments
     ;; Compilation uses -Werror by default, but it fails.
     '(#:configure-flags '("--disable-werror")))
    (home-page "https://www.ltrace.org/")
    (synopsis "Library call tracer for Linux")
    (description
     "ltrace intercepts and records dynamic library calls which are called by
an executed process and the signals received by that process.  It can also
intercept and print the system calls executed by the program.")
    (license license:gpl2+)))

(define-public alsa-lib
  (package
    (name "alsa-lib")
    (version "1.1.8")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "ftp://ftp.alsa-project.org/pub/lib/alsa-lib-"
                   version ".tar.bz2"))
             (sha256
              (base32
               "1pxf0zkmps03l3zzd0fr828xhkg6a8hxljmbxzc2cyj2ls9kmp1w"))))
    (build-system gnu-build-system)
    (home-page "https://www.alsa-project.org/")
    (synopsis "The Advanced Linux Sound Architecture libraries")
    (description
     "The Advanced Linux Sound Architecture (ALSA) provides audio and
MIDI functionality to the Linux-based operating system.")
    (license license:lgpl2.1+)))

(define-public alsa-utils
  (package
    (name "alsa-utils")
    (version "1.1.8")
    (source (origin
             (method url-fetch)
             (uri (string-append "ftp://ftp.alsa-project.org/pub/utils/"
                                 name "-" version ".tar.bz2"))
             (sha256
              (base32
               "1kx45yhrxai3k595yyqs4wj0p2n5b0c9mf0k36ljjf1bj8lgb6zx"))))
    (build-system gnu-build-system)
    (arguments
     ;; XXX: Disable man page creation until we have DocBook.
     '(#:configure-flags (list "--disable-xmlto"

                               ;; The udev rule is responsible for restoring
                               ;; the volume.
                               (string-append "--with-udev-rules-dir="
                                              (assoc-ref %outputs "out")
                                              "/lib/udev/rules.d"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'disable-broken-test
           (lambda _
             ;; XXX: The 1.1.8 release tarball is missing a header that's
             ;; required for this test to work.  Fixed in 1.1.9.
             (substitute* "axfer/test/Makefile"
               ((".*container-test.*") ""))
             #t))
         (add-before
           'install 'pre-install
           (lambda _
             ;; Don't try to mkdir /var/lib/alsa.
             (substitute* "Makefile"
               (("\\$\\(MKDIR_P\\) .*ASOUND_STATE_DIR.*")
                "true\n"))
             #t)))))
    (inputs
     `(("libsamplerate" ,libsamplerate)
       ("ncurses" ,ncurses)
       ("alsa-lib" ,alsa-lib)
       ("xmlto" ,xmlto)
       ("gettext" ,gettext-minimal)))
    (home-page "http://www.alsa-project.org/")
    (synopsis "Utilities for the Advanced Linux Sound Architecture (ALSA)")
    (description
     "The Advanced Linux Sound Architecture (ALSA) provides audio and
MIDI functionality to the Linux-based operating system.")

    ;; This is mostly GPLv2+ but a few files such as 'alsactl.c' are
    ;; GPLv2-only.
    (license license:gpl2)))

(define-public alsa-plugins
  (package
    (name "alsa-plugins")
    (version "1.1.8")
    (source (origin
             (method url-fetch)
             (uri (string-append "ftp://ftp.alsa-project.org/pub/plugins/"
                                 name "-" version ".tar.bz2"))
             (sha256
              (base32
               "152r82i6f97gfilfgiax5prxkd4xlcipciv8ha8yrk452qbxyxvz"))))
    (build-system gnu-build-system)
    ;; TODO: Split libavcodec and speex if possible. It looks like they can not
    ;; be split, there are references to both in files.
    ;; TODO: Remove OSS related plugins, they add support to run native
    ;; ALSA applications on OSS however we do not offer OSS and OSS is
    ;; obsolete.
    (outputs '("out" "pulseaudio" "jack"))
    (arguments
     `(#:configure-flags '(;; Do not install a "local" configuration targeted
                           ;; for /etc/alsa.  On Guix System plugins are loaded from
                           ;; the ALSA service, and other distributions likely
                           ;; won't use these files.
                           "--with-alsalconfdir=/tmp/noop")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'split
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Distribute the binaries to the various outputs.
             (let* ((out (assoc-ref outputs "out"))
                    (jack (assoc-ref outputs "jack"))
                    (jacklib (string-append jack "/lib/alsa-lib"))
                    (pua (assoc-ref outputs "pulseaudio"))
                    (pualib (string-append pua "/lib/alsa-lib")))
               ;; For jack.
               (mkdir-p jacklib)
               (for-each (lambda (file)
                           (rename-file file (string-append jacklib "/" (basename file))))
                         (find-files out ".*jack\\.(la|so)"))
               ;; For pulseaudio.
               (mkdir-p pualib)
               (for-each (lambda (file)
                           (rename-file file (string-append pualib "/" (basename file))))
                         (find-files out ".*pulse\\.(la|so)"))
               #t))))))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("jack" ,jack-1)
       ("speex" ,speex) ; libspeexdsp resampling plugin
       ("libsamplerate" ,libsamplerate) ; libsamplerate resampling plugin
       ("ffmpeg" ,ffmpeg) ; libavcodec resampling plugin, a52 plugin
       ("pulseaudio" ,pulseaudio))) ; PulseAudio plugin
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://www.alsa-project.org/")
    (synopsis "Plugins for the Advanced Linux Sound Architecture (ALSA)")
    (description
     "The Advanced Linux Sound Architecture (ALSA) provides audio and
MIDI functionality to the Linux-based operating system.  This package enhances ALSA
by providing additional plugins which include: upmixing, downmixing, jackd and
pulseaudio support for native alsa applications, format conversion (s16 to a52), and
external rate conversion.")
    (license (list license:gpl2+
                   ;; `rate/rate_samplerate.c': LGPL v2.1 or later.
                   license:lgpl2.1+))))

(define-public iptables
  (package
    (name "iptables")
    (version "1.6.2")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://netfilter.org/iptables/iptables-"
                   version ".tar.bz2"))
             (sha256
              (base32
               "0crp0lvh5m2f15pr8cw97h8yb8zjj10x95zj06j46cr68vx2vl2m"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("flex" ,flex)
       ("bison" ,bison)))
    (inputs
     `(("libmnl" ,libmnl)
       ("libnftnl" ,libnftnl)))
    (arguments
     '(#:tests? #f       ; no test suite
       #:configure-flags ; add $libdir to the RUNPATH of executables
       (list (string-append "LDFLAGS=-Wl,-rpath=" %output "/lib"))))
    (home-page "https://www.netfilter.org/projects/iptables/index.html")
    (synopsis "Programs to configure Linux IP packet filtering rules")
    (description
     "@command{iptables} is the user-space command line program used to
configure the Linux 2.4.x and later IPv4 packet filtering ruleset
(@dfn{firewall}), including @dfn{NAT} (Network Address Translation).

This package also includes @command{ip6tables}, which is used to configure the
IPv6 packet filter.

Both commands are targeted at system administrators.")
    (license license:gpl2+)))

(define-public ebtables
  (package
    (name "ebtables")
    (version "2.0.10-4")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://netfilter.org/ebtables/ebtables-v"
                   version ".tar.gz"))
             (sha256
              (base32
               "0pa5ljlk970yfyhpf3iqwfpbc30j8mgn90fapw9cfz909x47nvyw"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ; no test suite
       #:make-flags
       (let* ((out (assoc-ref %outputs "out"))
              (bin (string-append out "/sbin"))
              (lib (string-append out "/lib"))
              (man (string-append out "/share/man"))
              (iptables   (assoc-ref %build-inputs "iptables"))
              (ethertypes (string-append iptables "/etc/ethertypes")))
         (list (string-append "LIBDIR=" lib)
               (string-append "MANDIR=" man)
               (string-append "BINDIR=" bin)
               (string-append "ETHERTYPESFILE=" ethertypes)
               ;; With the default CFLAGS, it falis with:
               ;;   communication.c:259:58: error: variable ‘ret’ set but not
               ;;   used [-Werror=unused-but-set-variable]
               "CFLAGS=-Wall"))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           ;; no configure script
           (lambda _
             (substitute* "Makefile"
               ;; Remove user and group options from install commands,
               ;; otherwise it fails with: invalid user 'root'.
               (("-o root -g root") "")
               ;; Remove 'ethertypes' from the install target.
               (("install: .*")
                "install: $(MANDIR)/man8/ebtables.8 exec scripts\n"))
             #t)))))
    (inputs
     `(("perl" ,perl)
       ("iptables" ,iptables)))
    (synopsis "Ethernet bridge frame table administration")
    (home-page "http://ebtables.netfilter.org/")
    (description
     "ebtables is an application program used to set up and maintain the
tables of rules (inside the Linux kernel) that inspect Ethernet frames.  It is
analogous to the iptables application, but less complicated, due to the fact
that the Ethernet protocol is much simpler than the IP protocol.")
    (license license:gpl2+)))

(define-public iproute
  (package
    (name "iproute2")
    (version "5.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kernel.org/linux/utils/net/iproute2/iproute2-"
                    version ".tar.xz"))
              (sha256
               (base32
                "1fi03lb8dqr8hq633gcqsf6228vsvysxms075j1yyl4nlc17616z"))))
    (build-system gnu-build-system)
    (arguments
     `( ;; There is a test suite, but it wants network namespaces and sudo.
       #:tests? #f
       #:make-flags (let ((out (assoc-ref %outputs "out")))
                      (list "DESTDIR="
                            (string-append "BASH_COMPDIR=" out
                                           "/etc/bash_completion.d")
                            (string-append "LIBDIR=" out "/lib")
                            (string-append "HDRDIR=" out "/include")
                            (string-append "SBINDIR=" out "/sbin")
                            (string-append "CONFDIR=" out "/etc")
                            (string-append "DOCDIR=" out "/share/doc/"
                                           ,name "-" ,version)
                            (string-append "MANDIR=" out "/share/man")))
       #:phases (modify-phases %standard-phases
                  (add-before 'install 'pre-install
                    (lambda _
                      ;; Don't attempt to create /var/lib/arpd.
                      (substitute* "Makefile"
                        (("^.*ARPDDIR.*$") ""))
                      #t)))))
    (inputs
     `(("db4" ,bdb)
       ("iptables" ,iptables)))
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)
       ("pkg-config" ,pkg-config)))
    ;; For tests.
    ;; ("libmnl" ,libmnl)
    ;; ("util-linux" ,util-linux)
    (home-page
     "https://wiki.linuxfoundation.org/networking/iproute2")
    (synopsis
     "Utilities for controlling TCP/IP networking and traffic in Linux")
    (description
     "Iproute2 is a collection of utilities for controlling TCP/IP networking
and traffic with the Linux kernel.  The most important of these are
@command{ip}, which configures IPv4 and IPv6, and @command{tc} for traffic
control.

Most network configuration manuals still refer to ifconfig and route as the
primary network configuration tools, but ifconfig is known to behave
inadequately in modern network environments, and both should be deprecated.")
    (license license:gpl2+)))

(define-public net-tools
  ;; XXX: This package is basically unmaintained, but it provides a few
  ;; commands not yet provided by Inetutils, such as 'route', so we have to
  ;; live with it.
  (let ((commit "479bb4a7e11a4084e2935c0a576388f92469225b")
        (revision "0"))
    (package
      (name "net-tools")
      (version (string-append "1.60-" revision "." (string-take commit 7)))
      (source (origin
               (method url-fetch)
               (uri (string-append "https://sourceforge.net/code-snapshots/git/"
                                   "n/ne/net-tools/code.git/net-tools-code-"
                                   commit ".zip"))
               (file-name (string-append name "-" version ".zip"))
               (sha256
                (base32
                 "0hz9fda9d78spp774b6rr5xaxav7cm4h0qcpxf70rvdbrf6qx7vy"))))
      (home-page "http://net-tools.sourceforge.net/")
      (build-system gnu-build-system)
      (arguments
       '(#:modules ((guix build gnu-build-system)
                    (guix build utils)
                    (srfi srfi-1)
                    (srfi srfi-26))
         #:phases
         (modify-phases %standard-phases
           (replace 'configure
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (mkdir-p (string-append out "/bin"))
                 (mkdir-p (string-append out "/sbin"))

                 ;; Pretend we have everything...
                 (system "yes | make config")

                 ;; ... except for the things we don't have.
                 ;; HAVE_AFDECnet requires libdnet, which we don't have.
                 ;; HAVE_HWSTRIP and HAVE_HWTR require kernel headers
                 ;; that have been removed.
                 ;; XXX SELINUX and AFBLUETOOTH are removed for now, but we should
                 ;; think about adding them later.
                 (substitute* '("config.make" "config.h")
                   (("^.*HAVE_(AFDECnet|HWSTRIP|HWTR|SELINUX|AFBLUETOOTH)[ =]1.*$")
                    ""))
                 #t)))
           (add-after 'install 'remove-redundant-commands
             (lambda* (#:key outputs #:allow-other-keys)
               ;; Remove commands and man pages redundant with Inetutils.
               (let* ((out (assoc-ref outputs "out"))
                      (dup (append-map (cut find-files out <>)
                                       '("^hostname"
                                         "^(yp|nis|dns)?domainname"))))
                 (for-each delete-file dup)
                 #t))))
         ;; Binaries that depend on libnet-tools.a don't declare that
         ;; dependency, making it parallel-unsafe.
         #:parallel-build? #f

         #:tests? #f                                ; no test suite
         #:make-flags (let ((out (assoc-ref %outputs "out")))
                        (list "CC=gcc"
                              (string-append "BASEDIR=" out)
                              (string-append "INSTALLNLSDIR=" out "/share/locale")
                              (string-append "mandir=/share/man")))))
      (native-inputs `(("gettext" ,gettext-minimal)
                       ("unzip" ,unzip)))
      (synopsis "Tools for controlling the network subsystem in Linux")
      (description
       "This package includes the important tools for controlling the network
subsystem of the Linux kernel.  This includes arp, ifconfig, netstat, rarp and
route.  Additionally, this package contains utilities relating to particular
network hardware types (plipconfig, slattach) and advanced aspects of IP
configuration (iptunnel, ipmaddr).")
      (license license:gpl2+))))

(define-public libcap
  (package
    (name "libcap")
    (version "2.25")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://kernel.org/linux/libs/security/linux-privs/"
                   "libcap2/libcap-" version ".tar.xz"))
             (sha256
              (base32
               "0qjiqc5pknaal57453nxcbz3mn1r4hkyywam41wfcglq3v2qlg39"))))
    (build-system gnu-build-system)
    (arguments '(#:phases
                 (modify-phases %standard-phases
                   (replace 'configure
                            ;; Add $libdir to the RUNPATH of executables.
                            (lambda _
                              (substitute* "Make.Rules"
                                (("LDFLAGS := #-g")
                                 (string-append "LDFLAGS := -Wl,-rpath="
                                                %output "/lib")))
                              #t)))
                 #:tests? #f                      ; no 'check' target
                 #:make-flags (list "lib=lib"
                                    (string-append "prefix="
                                                   (assoc-ref %outputs "out"))
                                    "RAISE_SETFCAP=no")))
    (native-inputs `(("perl" ,perl)))
    (inputs `(("attr" ,attr)))
    (home-page "https://sites.google.com/site/fullycapable/")
    (synopsis "Library for working with POSIX capabilities")
    (description
     "Libcap2 provides a programming interface to POSIX capabilities on
Linux-based operating systems.")

    ;; License is BSD-3 or GPLv2, at the user's choice.
    (license license:gpl2)))

(define-public bridge-utils
  (package
    (name "bridge-utils")
    (version "1.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.kernel.org/pub/linux/utils/net/"
                           "bridge-utils/bridge-utils-" version ".tar.xz"))
       (sha256
        (base32 "1j16kr44csyr4yqxly26l1yw2bh4nkiasgwvask2i2gvsnsyyryc"))))
    (build-system gnu-build-system)

    ;; The tarball lacks all the generated files.
    (native-inputs `(("autoconf" ,autoconf)
                     ("automake" ,automake)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'bootstrap 'patch-stuff
           (lambda _
             ;; Fix "field ‘ip6’ has incomplete type" errors.
             (substitute* "libbridge/libbridge.h"
               (("#include <linux/if_bridge.h>")
                "#include <linux/in6.h>\n#include <linux/if_bridge.h>"))

             ;; Ensure that the entire build fails if one of the
             ;; sub-Makefiles fails.
             (substitute* "Makefile.in"
               (("\\$\\(MAKE\\) \\$\\(MFLAGS\\) -C \\$\\$x ;")
                "$(MAKE) $(MFLAGS) -C $$x || exit 1;"))

             #t)))
       #:tests? #f))                              ; no 'check' target

    (home-page
     "http://www.linuxfoundation.org/collaborate/workgroups/networking/bridge")
    (synopsis "Manipulate Ethernet bridges")
    (description
     "Utilities for Linux's Ethernet bridging facilities.  A bridge is a way
to connect two Ethernet segments together in a protocol independent way.
Packets are forwarded based on Ethernet address, rather than IP address (like
a router).  Since forwarding is done at Layer 2, all protocols can go
transparently through a bridge.")
    (license license:gpl2+)))

(define-public libnl
  (package
    (name "libnl")
    (version "3.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/thom311/libnl/releases/download/"
                    "libnl" (string-join (string-split version #\.) "_")
                    "/libnl-" version ".tar.gz"))
              (sha256
               (base32
                "1gzm444rnsib64dddv0cwlpzy1q4bycjqhp1i5pxpikimqvpca5p"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)
       ("pkg-config" ,pkg-config)
       ("swig" ,swig)
       ("libnl3-doc"
        ,(origin
           (method url-fetch)
           (uri (string-append
                 "https://github.com/thom311/libnl/releases/download/libnl"
                 (string-join (string-split version #\.) "_")
                 "/libnl-doc-" version ".tar.gz"))
           (sha256
            (base32 "1m5cnzviv31gjnz6fz5rgyl6ah4dbp2akm49j9973sgwl36gs8jx"))))))
    (inputs
     `(("python-2" ,python-2)
       ("python-3" ,python-3)))
    (outputs '("out" "doc" "python2" "python3"))
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-python
           (lambda* (#:key outputs #:allow-other-keys)
             (define (python-inst python)
               (invoke python "setup.py" "build")
               (invoke python "setup.py" "install"
                       (string-append "--prefix="
                                      (assoc-ref %outputs python)))
               (invoke python "setup.py" "clean"))
             (setenv "LDFLAGS" (format #f "-Wl,-rpath=~a/lib"
                                       (assoc-ref %outputs "out")))
             (with-directory-excursion "./python"
               (for-each python-inst '("python2" "python3")))
             #t))
         (add-after 'install 'install-doc
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((dest (string-append (assoc-ref outputs "doc")
                                        "/share/doc/libnl")))
               (mkdir-p dest)
               (invoke "tar" "xf" (assoc-ref inputs "libnl3-doc")
                       "--strip-components=1" "-C" dest)))))))
    (home-page "https://www.infradead.org/~tgr/libnl/")
    (synopsis "NetLink protocol library suite")
    (description
     "The libnl suite is a collection of libraries providing APIs to netlink
protocol based Linux kernel interfaces.  Netlink is an IPC mechanism primarily
between the kernel and user space processes.  It was designed to be a more
flexible successor to ioctl to provide mainly networking related kernel
configuration and monitoring interfaces.")

    ;; Most files are LGPLv2.1-only, but some are GPLv2-only (like
    ;; 'nl-addr-add.c'), so the result is GPLv2-only.
    (license license:gpl2)))

(define-public iw
  (package
    (name "iw")
    (version "4.14")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kernel.org/software/network/iw/iw-"
                    version ".tar.xz"))
              (sha256
               (base32
                "12ddd6vh6vs97135bnlyr0szv7hvpbnmfh48584frzab0z0725ph"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("libnl" ,libnl)))
    (arguments
     `(#:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
                          "CC=gcc")
       #:phases (modify-phases %standard-phases (delete 'configure))))
    (home-page "https://wireless.wiki.kernel.org/")
    (synopsis "Tool for configuring wireless devices")
    (description
     "iw is a new nl80211 based CLI configuration utility for wireless
devices.  It replaces @code{iwconfig}, which is deprecated.")
    (license license:isc)))

(define-public powertop
  (package
    (name "powertop")
    (version "2.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://01.org/sites/default/files/downloads/"
                           "powertop-v" version ".tar.gz"))
       (sha256
        (base32 "0xaazqccyd42v2q532dxx40nqhb9sfsa6cyx8641rl57mfg4bdyk"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; TODO: Patch some hardcoded "wlan0" in calibrate/calibrate.cpp to
         ;; allow calibrating the network interface in Guix System.
         (add-after 'unpack 'patch-absolute-file-names
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((kmod (assoc-ref inputs "kmod")))
               (substitute* (find-files "src" "\\.cpp$")
                 ;; Give the right 'modprobe' file name so that essential
                 ;; modules such as msr.ko can be loaded.
                 (("/sbin/modprobe") (string-append kmod "/bin/modprobe"))
                 ;; These programs are only needed to calibrate, so using
                 ;; relative file names avoids adding extra inputs.  When they
                 ;; are missing powertop gracefully handles it.
                 (("/usr/bin/hcitool") "hcitool")
                 (("/usr/bin/xset") "xset")
                 (("/usr/sbin/hciconfig") "hciconfig"))
               #t))))))
    (inputs
     `(("kmod" ,kmod)
       ("libnl" ,libnl)
       ("ncurses" ,ncurses)
       ("pciutils" ,pciutils)
       ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://01.org/powertop/")
    (synopsis "Analyze power consumption on Intel-based laptops")
    (description
     "PowerTOP is a Linux tool to diagnose issues with power consumption and
power management.  In addition to being a diagnostic tool, PowerTOP also has
an interactive mode where the user can experiment various power management
settings for cases where the operating system has not enabled these
settings.")
    (license license:gpl2)))

(define-public aumix
  (package
    (name "aumix")
    (version "2.9.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.jpj.net/~trevor/aumix/releases/aumix-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "0a8fwyxnc5qdxff8sl2sfsbnvgh6pkij4yafiln0fxgg6bal7knj"))))
    (build-system gnu-build-system)
    (inputs `(("ncurses" ,ncurses)))
    (home-page "http://www.jpj.net/~trevor/aumix.html")
    (synopsis "Audio mixer for X and the console")
    (description
     "Aumix adjusts an audio mixer from X, the console, a terminal,
the command line or a script.")
    (license license:gpl2+)))

(define-public iotop
  (package
    (name "iotop")
    (version "0.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://guichaz.free.fr/iotop/files/iotop-"
                           version ".tar.gz"))
       (sha256 (base32
                "1kp8mqg2pbxq4xzpianypadfxcsyfgwcaqgqia6h9fsq6zyh4z0s"))))
    (build-system python-build-system)
    (arguments
     ;; The setup.py script expects python-2.
     `(#:python ,python-2
       ;; There are currently no checks in the package.
       #:tests? #f))
    (native-inputs `(("python" ,python-2)))
    (home-page "http://guichaz.free.fr/iotop/")
    (synopsis
     "Displays the IO activity of running processes")
    (description
     "Iotop is a Python program with a top like user interface to show the
processes currently causing I/O.")
    (license license:gpl2+)))

(define-public fuse
  (package
    (name "fuse")
    (version "2.9.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/libfuse/libfuse/releases/"
                                  "download/fuse-" version
                                  "/fuse-" version ".tar.gz"))
              (sha256
               (base32
                "1qxg1r1mgysfq6qakmvid2njph3lr00w0swvydsfl9ymilfzi12y"))
              (patches (search-patches "fuse-overlapping-headers.patch"))))
    (build-system gnu-build-system)
    (inputs `(("util-linux" ,util-linux)))
    (arguments
     '(#:configure-flags (list (string-append "MOUNT_FUSE_PATH="
                                              (assoc-ref %outputs "out")
                                              "/sbin")
                               (string-append "INIT_D_PATH="
                                              (assoc-ref %outputs "out")
                                              "/etc/init.d")

                               ;; The rule makes /dev/fuse 666.
                               (string-append "UDEV_RULES_PATH="
                                              (assoc-ref %outputs "out")
                                              "/lib/udev/rules.d"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-file-names
           (lambda* (#:key inputs #:allow-other-keys)
             ;; libfuse calls out to mount(8) and umount(8).  Make sure
             ;; it refers to the right ones.
             (substitute* '("lib/mount_util.c" "util/mount_util.c")
               (("/bin/(u?)mount" _ maybe-u)
                (string-append (assoc-ref inputs "util-linux")
                               "/bin/" maybe-u "mount")))
             (substitute* '("util/mount.fuse.c")
               (("/bin/sh")
                (which "sh")))

             ;; This hack leads libfuse to search for 'fusermount' in
             ;; $PATH, where it may find a setuid-root binary, instead of
             ;; trying solely $out/sbin/fusermount and failing because
             ;; it's not setuid.
             (substitute* "lib/Makefile"
               (("-DFUSERMOUNT_DIR=[[:graph:]]+")
                "-DFUSERMOUNT_DIR=\\\"/var/empty\\\""))
             #t)))))
    (home-page "https://github.com/libfuse/libfuse")
    (synopsis "Support file systems implemented in user space")
    (description
     "As a consequence of its monolithic design, file system code for Linux
normally goes into the kernel itself---which is not only a robustness issue,
but also an impediment to system extensibility.  FUSE, for \"file systems in
user space\", is a kernel module and user-space library that tries to address
part of this problem by allowing users to run file system implementations as
user-space processes.")
    (license (list license:lgpl2.1                ;library
                   license:gpl2+))))              ;command-line utilities

(define-public unionfs-fuse
  (package
    (name "unionfs-fuse")
    (version "2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/rpodgorny/unionfs-fuse/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0hsn8l1iblvx27bpd4dvnvnbh9ri3sv2f9xzpsnfz3379kb7skgj"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("python" ,python)))
    (inputs `(("fuse" ,fuse)))
    (arguments
     ;; The tests were never actually run ("collected 0 items"), but in recent
     ;; versions of pytest that causes an error.
     '(#:tests? #f))
    (home-page "https://github.com/rpodgorny/unionfs-fuse")
    (synopsis "User-space union file system")
    (description
     "UnionFS-FUSE is a flexible union file system implementation in user
space, using the FUSE library.  Mounting a union file system allows you to
\"aggregate\" the contents of several directories into a single mount point.
UnionFS-FUSE additionally supports copy-on-write.")
    (license license:bsd-3)))

(define fuse-static
  (package (inherit fuse)
    (name "fuse-static")
    (source (origin (inherit (package-source fuse))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Normally libfuse invokes mount(8) so that /etc/mtab is
                  ;; updated.  Change calls to 'mtab_needs_update' to 0 so
                  ;; that it doesn't do that, allowing us to remove the
                  ;; dependency on util-linux (something that is useful in
                  ;; initrds.)
                  (substitute* '("lib/mount_util.c"
                                 "util/mount_util.c")
                    (("mtab_needs_update[[:blank:]]*\\([a-z_]+\\)")
                     "0")
                    (("/bin/")
                     ""))
                  #t))))))

(define-public unionfs-fuse/static
  (package (inherit unionfs-fuse)
    (synopsis "User-space union file system (statically linked)")
    (name (string-append (package-name unionfs-fuse) "-static"))
    (source (origin (inherit (package-source unionfs-fuse))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Add -ldl to the libraries, because libfuse.a needs that.
                  (substitute* "src/CMakeLists.txt"
                    (("target_link_libraries(.*)\\)" _ libs)
                     (string-append "target_link_libraries"
                                    libs " dl)")))
                  #t))))
    (arguments
     '(#:tests? #f
       #:configure-flags '("-DCMAKE_EXE_LINKER_FLAGS=-static")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'post-install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (exe (string-append out "/bin/unionfs")))
               ;; By default, 'unionfs' keeps references to
               ;; $glibc/share/locale and similar stuff.  Remove them.
               (remove-store-references exe)

               ;; 'unionfsctl' has references to glibc as well.  Since
               ;; we don't need it, remove it.
               (delete-file (string-append out "/bin/unionfsctl"))
               #t))))))
    (inputs `(("fuse" ,fuse-static)))))

(define-public sshfs
  (package
    (name "sshfs")
    (version "2.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/libfuse/sshfs/releases/"
                                  "download/sshfs-" version "/sshfs-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "00fir2iykdx11g8nv5gijg0zjrp2g3ldypnv0yi6lq3h5pg5v13h"))))
    (build-system gnu-build-system)
    (inputs
     `(("fuse" ,fuse)
       ("glib" ,glib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://github.com/libfuse/sshfs")
    (synopsis "Mount remote file systems over SSH")
    (description
     "This is a file system client based on the SSH File Transfer Protocol.
Since most SSH servers already support this protocol it is very easy to set
up: on the server side there's nothing to do; on the client side mounting the
file system is as easy as logging into the server with an SSH client.")
    (license license:gpl2+)))

(define-public sshfs-fuse
  (package (inherit sshfs)
    (name "sshfs-fuse")
    (properties `((superseded . ,sshfs)))))

(define-public archivemount
  (package
    (name "archivemount")
    (version "0.8.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.cybernoia.de/software/archivemount/"
                           "archivemount-" version ".tar.gz"))
       (sha256
        (base32 "12fb8fcmd1zwvfgzx4pay47md5cr2kgxcgq82cm6skmq75alfzi4"))))
    (build-system gnu-build-system)
    (inputs `(("fuse" ,fuse)
              ("libarchive" ,libarchive)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://www.cybernoia.de/software/archivemount.html")
    (synopsis "Tool for mounting archive files with FUSE")
    (description "archivemount is a FUSE-based file system for Unix variants,
including Linux.  Its purpose is to mount archives (i.e. tar, tar.gz, etc.) to a
mount point where it can be read from or written to as with any other file
system.  This makes accessing the contents of the archive, which may be
compressed, transparent to other programs, without decompressing them.")
    (license license:lgpl2.0+)))

(define-public numactl
  (package
    (name "numactl")
    (version "2.0.12")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/numactl/numactl/releases/download/v"
                    version "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ad7mpi3vacbfnx3aqxnvgsj64yp3mav9yxnaz8ancjv7wvdmfsm"))))
    (build-system gnu-build-system)
    (arguments
     '(;; There's a 'test' target, but it requires NUMA support in the kernel
       ;; to run, which we can't assume to have.
       #:tests? #f))

    ;; NUMA is apparently not supported on armhf, see
    ;; http://www.spinics.net/lists/linux-numa/msg01157.html
    (supported-systems (delete "armhf-linux" %supported-systems))
    (home-page "https://github.com/numactl/numactl")
    (synopsis "Tools for non-uniform memory access (NUMA) machines")
    (description
     "NUMA stands for Non-Uniform Memory Access, in other words a system whose
memory is not all in one place.  The numactl program allows you to run your
application program on specific CPU's and memory nodes.  It does this by
supplying a NUMA memory policy to the operating system before running your
program.

The package contains other commands, such as numademo, numastat and memhog.
The numademo command provides a quick overview of NUMA performance on your
system.")
    (license (list license:gpl2                   ;programs
                   license:lgpl2.1))))            ;library

(define-public kbd-neo
  (package
    (name "kbd-neo")
    (version "2486")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://svn.neo-layout.org/!svn/bc/"
                           version "/linux/console/neo.map"))
       (file-name (string-append name "-" version ".map"))
       (sha256
        (base32
         "19mfrd31vzpsjiwc7pshxm0b0sz5dd17xrz6k079cy4im1vf0r4g"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let ((out (string-append %output "/share/keymaps"))
                         (source (assoc-ref %build-inputs "source")))
                     (mkdir-p out)
                     (copy-file source (string-append out "/neo.map"))
                     #t))))
    (home-page "https://neo-layout.org")
    (synopsis "Neo2 console layout")
    (description
     "Kbd-neo provides the Neo2 keyboard layout for use with
@command{loadkeys(1)} from @code{kbd(4)}.")
    ;; The file is located in an svn directory, the entire content of
    ;; the directory is licensed as GPL3.
    (license license:gpl3+)))

(define-public kbd
  (package
    (name "kbd")
    (version "2.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kernel.org/linux/utils/kbd/kbd-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "124swm93dm4ca0pifgkrand3r9gvj3019d4zkfxsj9djpvv0mnaz"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "tests/Makefile.in"
                    ;; The '%: %.in' rule incorrectly uses @VERSION@.
                    (("@VERSION@")
                     "[@]VERSION[@]"))
                  (substitute* '("src/unicode_start" "src/unicode_stop")
                    ;; Assume the Coreutils are in $PATH.
                    (("/usr/bin/tty")
                     "tty"))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'pre-build
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gzip  (assoc-ref %build-inputs "gzip"))
                   (bzip2 (assoc-ref %build-inputs "bzip2")))
               (substitute* "src/libkeymap/findfile.c"
                 (("gzip")
                  (string-append gzip "/bin/gzip"))
                 (("bzip2")
                  (string-append bzip2 "/bin/bzip2")))
               #t)))
         (add-after 'install 'post-install
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Make sure these programs find their comrades.
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (for-each (lambda (prog)
                           (wrap-program (string-append bin "/" prog)
                             `("PATH" ":" prefix (,bin))))
                         '("unicode_start" "unicode_stop"))
               #t))))))
    (inputs `(("check" ,check)
              ("gzip" ,gzip)
              ("bzip2" ,bzip2)
              ("pam" ,linux-pam)))
    (native-search-paths
     (list (search-path-specification
            (variable "LOADKEYS_KEYMAP_PATH")
            ;; Append ‘/**’ to recursively search all directories.  One can then
            ;; run (for example) ‘loadkeys en-latin9’ instead of having to find
            ;; and type ‘i386/colemak/en-latin9’ on a mislabelled keyboard.
            (files (list "share/keymaps/**")))))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://kbd-project.org/")
    (synopsis "Linux keyboard utilities and keyboard maps")
    (description
     "This package contains keytable files and keyboard utilities compatible
for systems using the Linux kernel.  This includes commands such as
@code{loadkeys}, @code{setfont}, @code{kbdinfo}, and @code{chvt}.")
    (license license:gpl2+)))

(define-public loadkeys-static
  (package
    (inherit kbd)
    (name "loadkeys-static")
    (arguments
     (substitute-keyword-arguments (package-arguments kbd)
       ((#:configure-flags flags ''())
        `(append '("LDFLAGS=-static" "--disable-shared" "--disable-nls"
                   "--disable-vlock"              ;so we don't need libpam
                   "--disable-libkeymap")
                 ,flags))
       ((#:make-flags flags ''())
        `(cons "LDFLAGS=-all-static" ,flags))
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 ;; The binary keeps references to gzip, among other things,
                 ;; which we don't need in the initrd, so strip references.
                 (remove-store-references "src/loadkeys")

                 (install-file "src/loadkeys"
                               (string-append out "/bin"))
                 #t)))
           (delete 'post-install)))
       ((#:strip-flags _ '())
        ''("--strip-all"))
       ((#:allowed-references _ '())
        '())))

    (synopsis "Statically-linked @command{loadkeys} program")

    ;; This package is meant to be used internally in the initrd so don't
    ;; expose it.
    (properties '((hidden? . #t)))))

(define-public inotify-tools
  (package
    (name "inotify-tools")
    (version "3.20.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rvoicilas/inotify-tools.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "14dci1i4mhsd5sa33k8h3ayphk19kizynh5ql9ryibdpmcanfiyq"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (home-page "https://github.com/rvoicilas/inotify-tools/wiki")
    (synopsis "Monitor file accesses")
    (description
     "The inotify-tools packages provides a C library and command-line tools
to use Linux' inotify mechanism, which allows file accesses to be monitored.")
    (license license:gpl2+)))

(define-public kmod
  (package
    (name "kmod")
    (version "26")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "mirror://kernel.org/linux/utils/kernel/kmod/"
                              "kmod-" version ".tar.xz"))
              (sha256
               (base32
                "17dvrls70nr3b3x1wm8pwbqy4r8a5c20m0dhys8mjhsnpg425fsp"))
              (patches (search-patches "kmod-module-directory.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("xz" ,xz)
       ("zlib" ,zlib)))
    (arguments
     `(#:tests? #f                      ; FIXME: Investigate test failures
       #:configure-flags '("--with-xz" "--with-zlib")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-modprobe&co
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (for-each (lambda (tool)
                           (symlink "kmod"
                                    (string-append bin "/" tool)))
                         '("insmod" "rmmod" "lsmod" "modprobe"
                           "modinfo" "depmod"))
               #t))))))
    (home-page "https://www.kernel.org/")
    (synopsis "Kernel module tools")
    (description "Kmod is a set of tools to handle common tasks with Linux
kernel modules like insert, remove, list, check properties, resolve
dependencies and aliases.

These tools are designed on top of libkmod, a library that is shipped with
kmod.  The aim is to be compatible with tools, configurations and indices
from the module-init-tools project.")
    (license license:gpl2+))) ; library under lgpl2.1+

(define-public eudev
  ;; The post-systemd fork, maintained by Gentoo.
  (package
    (name "eudev")
    (version "3.2.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url "https://github.com/gentoo/eudev")
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1la7x7v7yqb84wnc7w0kj53sa0an0m9xp6wn01ypi8drh02wjjy2"))
              (patches (search-patches "eudev-rules-directory.patch"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-source-writable
           (lambda _
             ;; XXX: Git checkouts are read-only, but this package needs to
             ;; modify some of its files.
             (for-each make-file-writable (find-files "."))
             #t))
         (add-before 'bootstrap 'patch-file-names
           (lambda* (#:key inputs #:allow-other-keys)
            (substitute* "man/make.sh"
              (("/usr/bin/xsltproc")
                (string-append (assoc-ref inputs "xsltproc")
                               "/bin/xsltproc")))
            #t))
         (add-after 'install 'build-hwdb
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Build OUT/etc/udev/hwdb.bin.  This allows 'lsusb' and
             ;; similar tools to display product names.
             (let ((out (assoc-ref outputs "out")))
               (invoke (string-append out "/bin/udevadm")
                       "hwdb" "--update")))))
       #:configure-flags (list "--enable-manpages")))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gperf" ,gperf)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ;; For tests.
       ("perl" ,perl)
       ("python" ,python-wrapper)
       ;; For documentation.
       ("docbook-xml" ,docbook-xml-4.2)
       ("docbook-xsl" ,docbook-xsl)
       ("libxml2" ,libxml2)             ;for $XML_CATALOG_FILES
       ("xsltproc" ,libxslt)))
    (inputs
     ;; When linked against libblkid, eudev can populate /dev/disk/by-label
     ;; and similar; it also installs the '60-persistent-storage.rules' file,
     ;; which contains the rules to do that.
     `(("util-linux" ,util-linux)                 ;for blkid
       ("kmod" ,kmod)))
    (home-page "https://wiki.gentoo.org/wiki/Project:Eudev")
    (synopsis "Userspace device management")
    (description "Udev is a daemon which dynamically creates and removes
device nodes from /dev/, handles hotplug events and loads drivers at boot
time.")
    (license license:gpl2+)))

(define-public eudev-with-hwdb
  (deprecated-package "eudev-with-hwdb" eudev))

(define-public lvm2
  (package
    (name "lvm2")
    (version "2.02.177")
    (source (origin
              (method url-fetch)
              (uri (string-append "ftp://sources.redhat.com/pub/lvm2/releases/LVM2."
                                  version ".tgz"))
              (sha256
               (base32
                "1wl0isn0yz5wvglwylnlqkppafwmvhliq5bd92vjqp5ir4za49a0"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (use-modules (guix build utils))

                  ;; Honor sysconfdir.
                  (substitute* "make.tmpl.in"
                    (("confdir = .*$")
                     "confdir = @sysconfdir@\n")
                    (("DEFAULT_SYS_DIR = @DEFAULT_SYS_DIR@")
                     "DEFAULT_SYS_DIR = @sysconfdir@"))
                  #t))
              (patches (search-patches "lvm2-static-link.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("procps" ,procps)))                       ;tests use 'pgrep'
    (inputs
     `(("udev" ,eudev)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'configure 'set-makefile-shell
           (lambda _
             ;; Use 'sh', not 'bash', so that '. lib/utils.sh' works as
             ;; expected.
             (setenv "SHELL" (which "sh"))

             ;; Replace /bin/sh with the right file name.
             (patch-makefile-SHELL "make.tmpl")
             #t)))

       #:configure-flags (list (string-append "--sysconfdir="
                                              (assoc-ref %outputs "out")
                                              "/etc/lvm")
                               "--enable-udev_sync"
                               "--enable-udev_rules"
                               "--enable-pkgconfig"
                               "--enable-cmdlib"
                               "--enable-dmeventd" ; Requires '--enable-cmdlib'.

                               ;; Make sure programs such as 'dmsetup' can
                               ;; find libdevmapper.so.
                               (string-append "LDFLAGS=-Wl,-rpath="
                                              (assoc-ref %outputs "out")
                                              "/lib,-rpath="
                                              (assoc-ref %outputs "out")
                                              "/lib/device-mapper")
                               ;; TODO: Patch make.tmpl.in to take LDFLAGS
                               ;; into account so that we don't need to also
                               ;; set CLDFLAGS.
                               (string-append "CLDFLAGS=-Wl,-rpath="
                                              (assoc-ref %outputs "out")
                                              "/lib,-rpath="
                                              (assoc-ref %outputs "out")
                                              "/lib/device-mapper"))

       ;; The tests use 'mknod', which requires root access.
       #:tests? #f))
    (home-page "https://sourceware.org/lvm2/")
    (synopsis "Logical volume management for Linux")
    (description
     "LVM2 is the logical volume management tool set for Linux-based systems.
This package includes the user-space libraries and tools, including the device
mapper.  Kernel components are part of Linux-libre.")

    ;; Libraries (liblvm2, libdevmapper) are LGPLv2.1.
    ;; Command-line tools are GPLv2.
    (license (list license:gpl2 license:lgpl2.1))))

(define-public lvm2-static
  (package
    (inherit lvm2)
    (name "lvm2-static")

    ;; Propagate udev because libdevmapper.a depends on libudev.
    (inputs (alist-delete "udev" (package-inputs lvm2)))
    (propagated-inputs `(("udev" ,eudev)))

    (arguments
     (substitute-keyword-arguments (package-arguments lvm2)
       ((#:configure-flags flags '())
        ;; LVM2 doesn't use Libtool, hence the custom option.
        `(cons "--enable-static_link" ,flags))))
    (synopsis "Logical volume management for Linux (statically linked)")))

(define-public wireless-tools
  (package
    (name "wireless-tools")
    (version "30.pre9")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.hpl.hp.com/personal/Jean_Tourrilhes/Linux/wireless_tools."
                                  version ".tar.gz"))
              (sha256
               (base32
                "0qscyd44jmhs4k32ggp107hlym1pcyjzihiai48xs7xzib4wbndb"))
              (snippet
               '(begin
                  ;; Remove the older header files that are not free software.
                  (for-each (lambda (n)
                              (delete-file (format #f "wireless.~a.h" n)))
                            '(10 11 12 13 14 15 16 17 18 19 20))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "PREFIX=" %output)
             (string-append "INSTALL_MAN=" %output "/share/man")
             (string-append "LDFLAGS=-Wl,-rpath=" %output "/lib")
             "BUILD_STATIC=")
       #:phases (modify-phases %standard-phases
                  (delete 'configure))
       #:tests? #f))
    (synopsis "Tools for manipulating Linux Wireless Extensions")
    (description "Wireless Tools are used to manipulate the now-deprecated
Linux Wireless Extensions; consider using @code{iw} instead.  The Wireless
Extension was an interface allowing you to set Wireless LAN specific
parameters and get the specific stats.  It is deprecated in favor the nl80211
interface.")
    (home-page "http://www.hpl.hp.com/personal/Jean_Tourrilhes/Linux/Tools.html")
    ;; wireless.21.h and wireless.22.h are distributed under lgpl2.1+, the
    ;; other files are distributed under gpl2.
    (license (list license:gpl2 license:lgpl2.1+))))

(define-public crda
  (package
    (name "crda")
    (version "3.18")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kernel.org/software/network/crda/"
                                  "crda-" version ".tar.xz"))
              (sha256
               (base32
                "1gydiqgb08d9gbx4l6gv98zg3pljc984m50hmn3ysxcbkxkvkz23"))
              (patches (search-patches "crda-optional-gcrypt.patch"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-after 'unpack 'gzip-determinism
                    (lambda _
                      (substitute* "Makefile"
                        (("gzip") "gzip --no-name"))
                      #t))
                  (add-before
                   'build 'no-werror-no-ldconfig
                   (lambda _
                     (substitute* "Makefile"
                       (("-Werror")  "")
                       (("ldconfig") "true"))
                     #t))
                  (add-before
                   'build 'set-regulator-db-file-name
                   (lambda* (#:key inputs #:allow-other-keys)
                     ;; Tell CRDA where to find our database.
                     (let ((regdb (assoc-ref inputs "wireless-regdb")))
                       (substitute* "crda.c"
                         (("\"/lib/crda/regulatory.bin\"")
                          (string-append "\"" regdb
                                         "/lib/crda/regulatory.bin\"")))
                       #t))))
       #:test-target "verify"
       #:make-flags (let ((out   (assoc-ref %outputs "out"))
                          (regdb (assoc-ref %build-inputs "wireless-regdb")))
                      (list "CC=gcc" "V=1"

                            ;; Disable signature-checking on 'regulatory.bin'.
                            ;; The reason is that this simplifies maintenance
                            ;; on our side (no need to manage a distro key
                            ;; pair), and we can guarantee integrity of
                            ;; 'regulatory.bin' by other means anyway, such as
                            ;; 'guix gc --verify'.  See
                            ;; <https://wireless.wiki.kernel.org/en/developers/regulatory/wireless-regdb>
                            ;; for a discssion.
                            "USE_OPENSSL=0"

                            (string-append "PREFIX=" out)
                            (string-append "SBINDIR=" out "/sbin/")
                            (string-append "UDEV_RULE_DIR="
                                           out "/lib/udev/rules.d")
                            (string-append "LDFLAGS=-Wl,-rpath="
                                           out "/lib -L.")
                            (string-append "REG_BIN=" regdb
                                           "/lib/crda/regulatory.bin")))))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("python" ,python-2)
                     ("wireless-regdb" ,wireless-regdb)))
    (inputs `(("libnl" ,libnl)))
    (home-page
     "https://wireless.wiki.kernel.org/en/developers/Regulatory/CRDA")
    (synopsis "Central regulatory domain agent (CRDA) for WiFi")
    (description
     "The Central Regulatory Domain Agent (CRDA) acts as the udev helper for
communication between the kernel Linux and user space for regulatory
compliance.")
    (license license:copyleft-next)))

(define-public wireless-regdb
  (package
    (name "wireless-regdb")
    (version "2017.03.07")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kernel.org/software/network/wireless-regdb/"
                    "wireless-regdb-" version ".tar.xz"))
              (sha256
               (base32
                "1f9mcp78sdd4sci6v32vxfcl1rfjpv205jisz1p93kkfnaisy7ip"))

              ;; We're building 'regulatory.bin' by ourselves.
              (snippet '(begin
                          (delete-file "regulatory.bin")
                          #t))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'gzip-determinism
                    (lambda _
                      (substitute* "Makefile"
                        (("gzip") "gzip --no-name"))
                      #t))
                  (delete 'configure))

       ;; The 'all' target of the makefile depends on $(REGDB_CHANGED), which
       ;; is computed and can be equal to 'maintainer-clean'; when that
       ;; happens, we can end up deleting the 'regulatory.bin' file that we
       ;; just built.  Thus, build things sequentially.
       #:parallel-build? #f

       #:tests? #f                                ;no tests
       #:make-flags (let ((out (assoc-ref %outputs "out")))
                      (list (string-append "PREFIX=" out)
                            (string-append "LSB_ID=Guix")
                            (string-append "DISTRO_PUBKEY=/dev/null")
                            (string-append "DISTRO_PRIVKEY=/dev/null")
                            (string-append "REGDB_PUBKEY=/dev/null")

                            ;; Leave that empty so that db2bin.py doesn't try
                            ;; to sign 'regulatory.bin'.  This allows us to
                            ;; avoid managing a key pair for the whole distro.
                            (string-append "REGDB_PRIVKEY=")))))
    (native-inputs `(("python" ,python-2)))
    (home-page
     "https://wireless.wiki.kernel.org/en/developers/regulatory/wireless-regdb")
    (synopsis "Wireless regulatory database")
    (description
     "This package contains the wireless regulatory database Central
Regulatory Database Agent (CRDA) daemon.  The database contains information on
country-specific regulations for the wireless spectrum.")
    (license license:isc)))

(define-public lm-sensors
  (package
    (name "lm-sensors")
    (version "3.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/groeck/lm-sensors.git")
             (commit (string-append "V" (string-join
                                         (string-split version #\.) "-")))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mdrnb9r01z1xfdm6dpkywvf9yy9a4yzb59paih9sijwmigv19fj"))
       (patches (search-patches "lm-sensors-hwmon-attrs.patch"))))
    (build-system gnu-build-system)
    (inputs `(("rrdtool" ,rrdtool)
              ("perl" ,perl)
              ("kmod" ,kmod)
              ("gnuplot" ,gnuplot)))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("flex" ,flex)
                     ("bison" ,bison)
                     ("which" ,which)))
    (outputs '("lib"              ;avoid perl in closure
               "out"))
    (arguments
     `(#:tests? #f  ; no 'check' target
       #:make-flags (list (string-append "PREFIX=" %output)
                          (string-append "ETCDIR=" (assoc-ref %outputs "lib") "/etc")
                          (string-append "INCLUDEDIR="
                                         (assoc-ref %outputs "lib") "/include")
                          (string-append "MANDIR=" %output "/share/man")
                          (string-append "LIBDIR=" (assoc-ref %outputs "lib") "/lib"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'patch-exec-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "prog/detect/sensors-detect"
               (("`uname")
                (string-append "`" (assoc-ref inputs "coreutils")
                               "/bin/uname"))
               (("(`|\")modprobe" all open-quote)
                (string-append open-quote
                               (assoc-ref inputs "kmod")
                               "/bin/modprobe")))
             (substitute* '("prog/pwm/pwmconfig"
                            "prog/pwm/fancontrol")
               (("gnuplot")
                (string-append (assoc-ref inputs "gnuplot")
                               "/bin/gnuplot"))
               (("cat ")
                (string-append (assoc-ref inputs "coreutils")
                               "/bin/cat "))
               (("egrep ")
                (string-append (assoc-ref inputs "grep")
                               "/bin/egrep "))
               (("sed -e")
                (string-append (assoc-ref inputs "sed")
                               "/bin/sed -e"))
               (("cut -d")
                (string-append (assoc-ref inputs "coreutils")
                               "/bin/cut -d"))
               (("sleep ")
                (string-append (assoc-ref inputs "coreutils")
                               "/bin/sleep "))
               (("readlink -f")
                (string-append (assoc-ref inputs "coreutils")
                               "/bin/readlink -f")))
             #t)))))
    (home-page "https://hwmon.wiki.kernel.org/lm_sensors")
    (synopsis "Utilities to read temperature/voltage/fan sensors")
    (description
     "Lm-sensors is a hardware health monitoring package for Linux.  It allows
you to access information from temperature, voltage, and fan speed sensors.
It works with most newer systems.")
    (license license:gpl2+)))

(define-public iucode-tool
  (package
    (name "iucode-tool")
    (version "2.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gitlab.com/iucode-tool/releases"
                                  "/raw/latest/iucode-tool_" version ".tar.xz"))
              (sha256
               (base32
                "159gvf6ljgg3g4vlhyy6pyr0wz11rcyhp985vc4az58d9px8xf0j"))))
    (build-system gnu-build-system)
    (home-page "https://gitlab.com/iucode-tool/iucode-tool/wikis/home")
    (synopsis "Manipulate Intel microcode bundles")
    (description
     "@command{iucode_tool} is a utility to work with microcode packages for
Intel processors.  It can convert between formats, extract specific versions,
create a firmware image suitable for the Linux kernel, and more.")
    ;; cpuid.h is available for i686, x86_64, and ia64.
    (supported-systems '("i686-linux" "x86_64-linux"))
    (license license:gpl2+)))

(define-public i2c-tools
  (package
    (name "i2c-tools")
    (version "3.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://jdelvare.nerim.net/mirror/i2c-tools/i2c-tools-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "000pvg995qy1b15ks59gd0klri55hb33kqpg5czy84hw1pbdgm0l"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f  ; no 'check' target
       #:make-flags (list (string-append "prefix=" %output)
                          "CC=gcc")
       ;; no configure script
       #:phases (modify-phases %standard-phases (delete 'configure))))
    (inputs
     `(("perl" ,perl)))
    (home-page "http://jdelvare.nerim.net/devel.html#i2ctools")
    (synopsis "I2C tools for Linux")
    (description
     "The i2c-tools package contains a heterogeneous set of I2C tools for
Linux: a bus probing tool, a chip dumper, register-level SMBus access helpers,
EEPROM decoding scripts, EEPROM programming tools, and a python module for
SMBus access.")
    (license license:gpl2+)))

(define-public xsensors
  (package
    (name "xsensors")
    (version "0.70")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.linuxhardware.org/xsensors/xsensors-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1siplsfgvcxamyqf44h71jx6jdfmvhfm7mh0y1q8ps4zs6pj2zwh"))))
    (build-system gnu-build-system)
    (inputs `(("lm-sensors" ,lm-sensors "lib")
              ("gtk" ,gtk+-2)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'enable-deprecated
           (lambda _
             (substitute* "src/Makefile.in"
               (("-DGDK_DISABLE_DEPRECATED") "")
               (("-DGTK_DISABLE_DEPRECATED") ""))
             #t))
         (add-before 'configure 'remove-Werror
           (lambda _
             (substitute* '("configure" "src/Makefile.in")
               (("-Werror") ""))
             #t)))))
    (home-page "http://www.linuxhardware.org/xsensors/")
    (synopsis "Hardware health information viewer")
    (description
     "Xsensors reads data from the libsensors library regarding hardware
health such as temperature, voltage and fan speed and displays the information
in a digital read-out.")
    (license license:gpl2+)))

(define-public perf
  (package
    (name "perf")
    (version (package-version linux-libre))
    (source (package-source linux-libre))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "SHELL_PATH" (which "bash"))
             (chdir "tools/perf")
             #t)))
       #:make-flags (list (string-append "prefix="
                                         (assoc-ref %outputs "out"))
                          "WERROR=0"

                          ;; By default, 'config/Makefile' uses lib64 on
                          ;; x86_64.  Work around that.
                          "lib=lib")
       #:tests? #f))                              ;no tests
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("bison" ,bison)
       ("flex" ,flex)

       ;; There are build scripts written in these languages.
       ("perl" ,perl)
       ("python" ,python-2)))
    (inputs
     `(("slang" ,slang)                        ;for the interactive TUI
       ;; ("newt" ,newt)
       ("python" ,python-2)                    ;'perf' links against libpython
       ("elfutils" ,elfutils)
       ("libiberty" ,libiberty)      ;used alongside BDF for symbol demangling
       ("libunwind" ,libunwind)      ;better stack walking
       ("numactl" ,numactl)          ;for 'perf bench numa mem'

       ;; Documentation.
       ("libxml2" ,libxml2)                       ;for $XML_CATALOG_FILES
       ("docbook-xsl" ,docbook-xsl)
       ("xmlto" ,xmlto)
       ("asciidoc" ,asciidoc)))
    (home-page "https://perf.wiki.kernel.org/")
    (synopsis "Linux profiling with performance counters")
    (description
     "perf is a tool suite for profiling using hardware performance counters,
with support in the Linux kernel.  perf can instrument CPU performance
counters, tracepoints, kprobes, and uprobes (dynamic tracing).  It is capable
of lightweight profiling.  This package contains the user-land tools and in
particular the @code{perf} command.")
    (license (package-license linux-libre))))

(define-public pflask
  (package
    (name "pflask")
    (version "0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ghedo/pflask/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1g8fjj67dfkc2s0852l9vqi1pm61gp4rxbpzbzg780f5s5hd1fys"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f)) ; no tests
    (home-page "https://ghedo.github.io/pflask/")
    (synopsis "Simple tool for creating Linux namespace containers")
    (description "pflask is a simple tool for creating Linux namespace
containers.  It can be used for running a command or even booting an OS inside
an isolated container, created with the help of Linux namespaces.  It is
similar in functionality to chroot, although pflask provides better isolation
thanks to the use of namespaces.")
    (license license:bsd-2)))

(define-public singularity
  (package
    (name "singularity")
    (version "2.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/singularityware/singularity/"
                                  "releases/download/" version
                                  "/singularity-" version ".tar.gz"))
              (sha256
               (base32
                "1whx0hqqi1326scgdxxxa1d94vn95mnq0drid6s8wdp84ni4d3gk"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Do not create directories in /var.
                  (substitute* "Makefile.in"
                    (("\\$\\(MAKE\\) .*install-data-hook") ""))

                  ;; The original source overrides PATH so that it points to
                  ;; /bin, /usr/local/bin, etc., which obviously doesn't work
                  ;; on Guix System.  Leave PATH unchanged so we refer to the
                  ;; installed Coreutils, grep, etc.
                  (substitute* "bin/singularity.in"
                    (("^PATH=.*" all)
                     (string-append "#" all "\n")))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--disable-suid"
             "--localstatedir=/var")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-reference-to-squashfs-tools
           (lambda _
             (substitute* "libexec/cli/build.exec"
               (("if ! singularity_which mksquashfs") "if 0")
               (("if ! mksquashfs")
                (string-append "if ! " (which "mksquashfs"))))
             #t)))))
    (inputs
     `(("libarchive" ,libarchive)
       ("python" ,python-wrapper)
       ("nettle" ,nettle)
       ("zlib" ,zlib)
       ("squashfs-tools" ,squashfs-tools)))
    (home-page "https://singularity.lbl.gov/")
    (synopsis "Container platform")
    (description "Singularity is a container platform supporting a number of
container image formats.  It can build SquashFS container images or import
existing Docker images.  Singularity requires kernel support for container
isolation or root privileges.")
    (license license:bsd-3)))

(define-public hdparm
  (package
    (name "hdparm")
    (version "9.58")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/hdparm/hdparm/"
                                  "hdparm-" version ".tar.gz"))
              (sha256
               (base32
                "03z1qm8zbgpxagk3994lvp24yqsshjibkwg05v9p3q1w7y48xrws"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (let ((out (assoc-ref %outputs "out")))
                      (list (string-append "binprefix=" out)
                            (string-append "manprefix=" out)
                            "CC=gcc"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))           ; no configure script
       #:tests? #f))                    ; no test suite
    (home-page "https://sourceforge.net/projects/hdparm/")
    (synopsis "View and tune ATA disk drive parameters")
    (description
     "@command{hdparm} is a command-line utility to control ATA controllers and
disk drives.  It can increase performance and/or reliability by careful tuning
of hardware settings like power and acoustic management, DMA modes, and caching.
It can also display detailed device information, or be used as a simple
performance benchmarking tool.

@command{hdparm} provides a command line interface to various Linux kernel
interfaces provided by the SATA/ATA/SAS @code{libata} subsystem, and the older
IDE driver subsystem.  Many external USB drive enclosures with SCSI-ATA Command
Translation (@dfn{SAT}) are also supported.")
    (license (license:non-copyleft "file://LICENSE.TXT"))))

(define-public rfkill
  (package
    (name "rfkill")
    (version "0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kernel.org/software/network/"
                                  name "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "0snqj5h0y991lszbigbyyqb8swj0hxajc1vfqg2scfay44231bp0"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list "CC=gcc"
                          (string-append "PREFIX=" %output))
       #:phases (modify-phases %standard-phases
                  (delete 'configure))
       #:tests? #f))
    (home-page "https://wireless.wiki.kernel.org/en/users/Documentation/rfkill")
    (synopsis "Tool for enabling and disabling wireless devices")
    (description
     "rfkill is a simple tool for accessing the rfkill device interface,
which is used to enable and disable wireless networking devices, typically
WLAN, Bluetooth and mobile broadband.")
    (license (license:non-copyleft "file://COPYING"
                                   "See COPYING in the distribution."))
    ;; rfkill is part of util-linux as of 2.31.
    (properties `((superseded . ,util-linux)))))

(define-public acpi
  (package
    (name "acpi")
    (version "1.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/acpiclient/acpiclient/"
                                  version "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "01ahldvf0gc29dmbd5zi4rrnrw2i1ajnf30sx2vyaski3jv099fp"))))
    (build-system gnu-build-system)
    (home-page "http://acpiclient.sourceforge.net")
    (synopsis "Display information on ACPI devices")
    (description "@code{acpi} attempts to replicate the functionality of the
\"old\" @code{apm} command on ACPI systems, including battery and thermal
information.  It does not support ACPI suspending, only displays information
about ACPI devices.")
    (license license:gpl2+)))

(define-public acpid
  (package
    (name "acpid")
    (version "2.0.31")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/acpid2/acpid-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1hrc0xm6q12knbgzhq0i8g2rfrkwcvh1asd7k9rs3nc5xmlwd7gw"))))
    (build-system gnu-build-system)
    (home-page "https://sourceforge.net/projects/acpid2/")
    (synopsis "Daemon for delivering ACPI events to user-space programs")
    (description
     "acpid is designed to notify user-space programs of Advanced
Configuration and Power Interface (ACPI) events.  acpid should be started
during the system boot, and will run as a background process.  When an ACPI
event is received from the kernel, acpid will examine the list of rules
specified in /etc/acpi/events and execute the rules that match the event.")
    (license license:gpl2+)))

(define-public sysfsutils
  (package
    (name "sysfsutils")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "mirror://sourceforge/linux-diag/sysfsutils/" version "/sysfsutils-"
         version ".tar.gz"))
       (sha256
        (base32 "12i0ip11xbfcjzxz4r10cvz7mbzgq1hfcdn97w6zz7sm3wndwrg8"))))
    (build-system gnu-build-system)
    (home-page "http://linux-diag.sourceforge.net/Sysfsutils.html")
    (synopsis "System utilities based on Linux sysfs")
    (description
     "These are a set of utilities built upon sysfs, a virtual file system in
Linux kernel versions 2.5+ that exposes a system's device tree.  The package
also contains the libsysfs library.")
    ;; The library is under lgpl2.1+ (all files say "or any later version").
    ;; The rest is mostly gpl2, with a few files indicating gpl2+.
    (license (list license:gpl2 license:gpl2+ license:lgpl2.1+))))

(define-public sysfsutils-1
  (package
    (inherit sysfsutils)
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "mirror://sourceforge/linux-diag/sysfsutils/sysfsutils-" version
         "/sysfsutils-" version ".tar.gz"))
       (sha256
        (base32 "0kdhs07fm8263pxwd5blwn2x211cg4fk63fyf9ijcdkvzmwxrqq3"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (substitute* "Makefile.in"
             (("includedir = /usr/include/sysfs")
              "includedir = @includedir@"))
           (substitute* "configure"
             (("includedir='(\\$\\{prefix\\}/include)'" all orig)
              (string-append "includedir='" orig "/sysfs'")))
           #t))))
    (synopsis "System utilities based on Linux sysfs (version 1.x)")))

(define-public cpufrequtils
  (package
    (name "cpufrequtils")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://www.kernel.org/pub/linux/utils/kernel/cpufreq/cpufrequtils-"
         version ".tar.gz"))
       (sha256
        (base32 "0qfqv7nqmjfr3p0bwrdlxkiqwqr7vmx053cadaa548ybqbghxmvm"))
       (patches (search-patches "cpufrequtils-fix-aclocal.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("sysfsutils" ,sysfsutils-1)))
    (arguments
     '(#:make-flags (list (string-append "LDFLAGS=-Wl,-rpath="
                                         (assoc-ref %outputs "out") "/lib"))))
    (home-page "https://www.kernel.org/pub/linux/utils/kernel/cpufreq/")
    (synopsis "Utilities to get and set CPU frequency on Linux")
    (description
     "The cpufrequtils suite contains utilities to retrieve CPU frequency
information, and set the CPU frequency if supported, using the cpufreq
capabilities of the Linux kernel.")
    (license license:gpl2)))

(define-public libraw1394
  (package
    (name "libraw1394")
    (version "2.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kernel.org/linux/libs/ieee1394/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0pm5b415j1qdzyw38wdv8h7ff4yx20831z1727mpsb6jc6bwdk03"))))
    (build-system gnu-build-system)
    (home-page "https://ieee1394.wiki.kernel.org/index.php/Main_Page")
    (synopsis "Interface library for the Linux IEEE1394 drivers")
    (description
     "Libraw1394 is the only supported interface to the kernel side raw1394 of
the Linux IEEE-1394 subsystem, which provides direct access to the connected
1394 buses to user space.  Through libraw1394/raw1394, applications can directly
send to and receive from other nodes without requiring a kernel driver for the
protocol in question.")
    (license license:lgpl2.1+)))

(define-public libavc1394
  (package
    (name "libavc1394")
    (version "0.5.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/libavc1394/libavc1394/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "0lsv46jdqvdx5hx92v0z2cz3yh6212pz9gk0k3513sbaa04zzcbw"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("libraw1394" ,libraw1394))) ; required by libavc1394.pc
    (home-page "https://sourceforge.net/projects/libavc1394/")
    (synopsis "AV/C protocol library for IEEE 1394")
    (description
     "Libavc1394 is a programming interface to the AV/C specification from
the 1394 Trade Association.  AV/C stands for Audio/Video Control.")
    (license license:lgpl2.1+)))

(define-public libiec61883
  (package
    (name "libiec61883")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kernel.org/linux/libs/ieee1394/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "17ph458zya2l8dr2xwqnzy195qd9swrir31g78qkgb3g4xz2rq6i"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("libraw1394" ,libraw1394))) ; required by libiec61883.pc
    (home-page "https://ieee1394.wiki.kernel.org/index.php/Main_Page")
    (synopsis "Isochronous streaming media library for IEEE 1394")
    (description
     "The libiec61883 library provides a higher level API for streaming DV,
MPEG-2 and audio over Linux IEEE 1394.")
    (license license:lgpl2.1+)))

(define-public mdadm
  (package
    (name "mdadm")
    (version "4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kernel.org/linux/utils/raid/mdadm/mdadm-"
                    version ".tar.xz"))
              (sha256
               (base32
                "0jjgjgqijpdp7ijh8slzzjjw690kydb1jjadf0x5ilq85628hxmb"))))
    (build-system gnu-build-system)
    (inputs
     `(("udev" ,eudev)))
    (arguments
     `(#:make-flags (let ((out (assoc-ref %outputs "out")))
                      (list "CC=gcc"
                            "INSTALL=install"
                            "CHECK_RUN_DIR=0"
                            ;; TODO: tell it where to find 'sendmail'
                            ;; (string-append "MAILCMD=" <???> "/sbin/sendmail")
                            (string-append "BINDIR=" out "/sbin")
                            (string-append "MANDIR=" out "/share/man")
                            (string-append "UDEVDIR=" out "/lib/udev")))
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-program-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((coreutils (assoc-ref inputs "coreutils")))
               (substitute* "udev-md-raid-arrays.rules"
                 (("/usr/bin/(readlink|basename)" all program)
                  (string-append coreutils "/bin/" program))))
             #t))
         (add-before 'build 'remove-W-error
           (lambda _
             ;; We cannot build with -Werror on i686 due to a
             ;; 'sign-compare' warning in util.c.
             (substitute* "Makefile"
               (("-Werror") ""))
             #t))
         (delete 'configure))
       ;;tests must be done as root
       #:tests? #f))
    (home-page "http://neil.brown.name/blog/mdadm")
    (synopsis "Tool for managing Linux Software RAID arrays")
    (description
     "mdadm is a tool for managing Linux Software RAID arrays.  It can create,
assemble, report on, and monitor arrays.  It can also move spares between raid
arrays when needed.")
    (license license:gpl2+)))

(define-public mdadm-static
  (package
    (inherit mdadm)
    (name "mdadm-static")
    (arguments
     (substitute-keyword-arguments (package-arguments mdadm)
       ((#:make-flags flags)
        `(cons "LDFLAGS = -static" ,flags))
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'install 'remove-cruft
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out         (assoc-ref outputs "out"))
                      (precious?   (lambda (file)
                                     (member file '("." ".." "sbin"))))
                      (directories (scandir out (negate precious?))))
                 (with-directory-excursion out
                   (for-each delete-file-recursively directories)
                   (remove-store-references "sbin/mdadm")
                   (delete-file "sbin/mdmon")
                   #t))))))
       ((#:modules modules %gnu-build-system-modules)
        `((ice-9 ftw) ,@modules))
       ((#:strip-flags _ '())
        ''("--strip-all"))                        ;strip a few extra KiB
       ((#:allowed-references _ '("out"))
        '("out"))))                               ;refer only self
    (synopsis "Statically-linked 'mdadm' command for use in an initrd")))

(define-public multipath-tools
  (package
    (name "multipath-tools")
    (version "0.7.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://git.opensvc.com/?p=multipath-tools/"
                                  ".git;a=snapshot;h=" version ";sf=tgz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1jhi6bhl4ih75rfmyyjxd35ghgch5ls1gw40cjxwy9d6bd41z6q1"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Drop bundled valgrind headers.
                  (delete-file-recursively "third-party")
                  (substitute* '("multipathd/main.c"
                                 "libmultipath/debug.c")
                    (("#include \"../third-party/")
                     "#include \""))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ; no tests
       #:make-flags (list "CC=gcc"
                          (string-append "DESTDIR="
                                         (assoc-ref %outputs "out"))
                          "SYSTEMDPATH=lib"
                          (string-append "LDFLAGS=-Wl,-rpath="
                                         (assoc-ref %outputs "out")
                                         "/lib"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((lvm2 (assoc-ref inputs "lvm2"))
                   (udev (assoc-ref inputs "udev")))
               (substitute* "Makefile.inc"
                 (("\\$\\(prefix\\)/usr") "$(prefix)")
                 ;; Do not save timestamp to avoid gzip "timestamp
                 ;; out-of-range" warnings.
                 (("gzip -9") "gzip -9n"))
               (substitute* '("kpartx/Makefile" "libmultipath/Makefile")
                 (("/usr/include/libdevmapper.h")
                  (string-append lvm2 "/include/libdevmapper.h"))
                 (("/usr/include/libudev.h")
                  (string-append udev "/include/libudev.h")))
               #t)))
         (delete 'configure))))         ; no configure script
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("valgrind" ,valgrind)))
    (inputs
     `(("ceph:lib" ,ceph "lib")
       ("json-c" ,json-c)
       ("libaio" ,libaio)
       ("liburcu" ,liburcu)
       ("lvm2" ,lvm2)
       ("readline" ,readline)
       ("udev" ,eudev)))
    (home-page "http://christophe.varoqui.free.fr/")
    (synopsis "Access block devices through multiple paths")
    (description
     "This package provides the following binaries to drive the
Linux Device Mapper multipathing driver:
@enumerate
@item @command{multipath} - Device mapper target autoconfig.
@item @command{multipathd} - Multipath daemon.
@item @command{mpathpersist} - Manages SCSI persistent reservations on
@code{dm} multipath devices.
@item @command{kpartx} - Create device maps from partition tables.
@end enumerate")
    (license (list license:gpl2+             ; main distribution
                   license:lgpl2.0+))))      ; libmpathcmd/mpath_cmd.h

(define-public libaio
  (package
    (name "libaio")
    (version "0.3.111")
    (source (origin
              (method url-fetch)
              (uri (list
                    (string-append "https://releases.pagure.org/libaio/"
                                   name "-" version ".tar.gz")))
              (sha256
               (base32
                "0ajhzbqjwsmz51gwccfyw6w9k4j4gmxcl2ph30sfn2gxv0d8gkv2"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags
       (list "CC=gcc" (string-append "prefix=" %output))
       #:test-target "partcheck" ; need root for a full 'check'
       #:phases
       (modify-phases %standard-phases (delete 'configure)))) ; no configure script
    (home-page "https://pagure.io/libaio")
    (synopsis "Linux-native asynchronous I/O access library")
    (description
     "This library enables userspace to use Linux kernel asynchronous I/O
system calls, important for the performance of databases and other advanced
applications.")
    (license license:lgpl2.1+)))

(define-public sbc
  (package
    (name "sbc")
    (version "1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.kernel.org/pub/linux/bluetooth/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "02ckd2z51z0h85qgv7x8vv8ybp5czm9if1z78411j53gaz7j4476"))))
    (build-system gnu-build-system)
    (inputs
     `(("libsndfile" ,libsndfile)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://www.kernel.org/pub/linux/bluetooth/")
    (synopsis "Bluetooth subband audio codec")
    (description
     "The SBC is a digital audio encoder and decoder used to transfer data to
Bluetooth audio output devices like headphones or loudspeakers.")
    (license license:gpl2+)))

(define-public bluez
  (package
    (name "bluez")
    (version "5.50")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kernel.org/linux/bluetooth/bluez-"
                    version ".tar.xz"))
              (sha256
               (base32
                "048r91vx9gs5nwwbah2s0xig04nwk14c5s0vb7qmaqdvighsmz2z"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (let ((out (assoc-ref %outputs "out")))
         (list "--sysconfdir=/etc"
               "--localstatedir=/var"
               "--enable-library"
               "--disable-systemd"
               ;; Install dbus/udev files to the correct location.
               (string-append "--with-dbusconfdir=" out "/etc")
               (string-append "--with-udevdir=" out "/lib/udev")))
       #:phases
       (modify-phases %standard-phases
         ;; Test unit/test-gatt fails unpredictably. Seems to be a timing
         ;; issue (discussion on upstream mailing list:
         ;; https://marc.info/?t=149578476300002&r=1&w=2)
         (add-before 'check 'skip-wonky-test
            (lambda _
              (substitute* "unit/test-gatt.c"
                (("tester_init\\(&argc, &argv\\);") "return 77;"))
              #t))
         (add-after 'install 'post-install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out        (assoc-ref outputs "out"))
                    (servicedir (string-append out "/share/dbus-1/services"))
                    (service    "obexd/src/org.bluez.obex.service")
                    (rule       (string-append
                                 out "/lib/udev/rules.d/97-hid2hci.rules")))
               ;; Install the obex dbus service file.
               (substitute* service
                 (("/bin/false")
                  (string-append out "/libexec/bluetooth/obexd")))
               (install-file service servicedir)
               ;; Fix paths in the udev rule.
               (substitute* rule
                 (("hid2hci --method")
                  (string-append out "/lib/udev/hid2hci --method"))
                 (("/sbin/udevadm")
                  (string-append (assoc-ref inputs "eudev") "/bin/udevadm")))
               #t))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gettext" ,gettext-minimal)))
    (inputs
     `(("glib" ,glib)
       ("dbus" ,dbus)
       ("eudev" ,eudev)
       ("libical" ,libical)
       ("readline" ,readline)))
    (home-page "http://www.bluez.org/")
    (synopsis "Linux Bluetooth protocol stack")
    (description
     "BlueZ provides support for the core Bluetooth layers and protocols.  It
is flexible, efficient and uses a modular implementation.")
    (license license:gpl2+)))

(define-public fuse-exfat
  (package
    (name "fuse-exfat")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/relan/exfat/releases/download/v"
                    version "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "1lz00q8g4590mrdqmf13ba1s9zrqq645ymgm5p9y99ad0qv22r87"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("fuse" ,fuse)))
    (home-page "https://github.com/relan/exfat")
    (synopsis "Mount exFAT file systems")
    (description
     "This package provides a FUSE-based file system that provides read and
write access to exFAT devices.")
    (license license:gpl2+)))

(define-public fuseiso
  (package
    (name "fuseiso")
    (version "20070708")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/fuseiso/fuseiso/"
                                  version "/fuseiso-" version ".tar.bz2"))
              (sha256
               (base32
                "127xql52dcdhmh7s5m9xc6q39jdlj3zhbjar1j821kb6gl3jw94b"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("fuse" ,fuse)
       ("glib" ,glib)
       ("zlib" ,zlib)))
    (home-page "https://sourceforge.net/projects/fuseiso/")
    (synopsis "Mount ISO file system images")
    (description
     "FuseISO is a FUSE module to mount ISO file system images (.iso, .nrg,
.bin, .mdf and .img files).  It supports plain ISO9660 Level 1 and 2, Rock
Ridge, Joliet, and zisofs.")
    (license license:gpl2)))

(define-public gpm
  (package
    (name "gpm")
    (version "1.20.7")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.nico.schottelius.org/software/gpm/archives/gpm-"
                    version ".tar.bz2"))
              (patches (search-patches "gpm-glibc-2.26.patch"))
              (sha256
               (base32
                "13d426a8h403ckpc8zyf7s2p5rql0lqbg2bv0454x0pvgbfbf4gh"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'bootstrap
                    (lambda _
                      ;; The tarball was not generated with 'make dist' so we
                      ;; need to bootstrap things ourselves.
                      (substitute* "autogen.sh"
                        (("/bin/sh") (which "sh")))
                      (invoke "./autogen.sh")
                      (patch-makefile-SHELL "Makefile.include.in")
                      #t)))

       ;; Make sure programs find libgpm.so.
       #:configure-flags (list (string-append "LDFLAGS=-Wl,-rpath="
                                              (assoc-ref %outputs "out")
                                              "/lib"))))
    (native-inputs
     `(("texinfo" ,texinfo)
       ("bison" ,bison)
       ("flex" ,flex)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (home-page "http://www.nico.schottelius.org/software/gpm/")
    (synopsis "Mouse support for the Linux console")
    (description
     "The GPM (general-purpose mouse) daemon is a mouse server for
applications running on the Linux console.  It allows users to select items
and copy/paste text in the console and in xterm.")
    (license license:gpl2+)))

(define-public btrfs-progs
  (package
    (name "btrfs-progs")
    (version "4.20.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kernel.org/linux/kernel/"
                                  "people/kdave/btrfs-progs/"
                                  "btrfs-progs-v" version ".tar.xz"))
              (sha256
               (base32
                "0z0fm3j4ajzsf445381ra8r3zzciyyvfh8vvbjmbyarg2rz8n3w9"))))
    (build-system gnu-build-system)
    (outputs '("out"
               "static"))      ; static versions of the binaries in "out"
    (arguments
     '(#:phases (modify-phases %standard-phases
                 (add-after 'build 'build-static
                   (lambda _ (invoke "make" "static")))
                 (add-after 'install 'install-bash-completion
                   (lambda* (#:key outputs #:allow-other-keys)
                     (let* ((out (assoc-ref outputs "out"))
                            (bashcomp (string-append out "/etc/bash_completion.d")))
                       (mkdir-p bashcomp)
                       (copy-file "btrfs-completion"
                                  (string-append bashcomp "/btrfs"))
                       #t)))
                 (add-after 'install 'install-static
                   (let ((staticbin (string-append (assoc-ref %outputs "static")
                                                  "/bin")))
                     (lambda _
                       (invoke "make"
                               (string-append "bindir=" staticbin)
                               "install-static")))))
       #:tests? #f            ; XXX: require the 'btrfs' kernel module.
       #:test-target "test"
       #:parallel-tests? #f)) ; tests fail when run in parallel
    (inputs `(("e2fsprogs" ,e2fsprogs)
              ("libblkid" ,util-linux)
              ("libblkid:static" ,util-linux "static")
              ("libuuid" ,util-linux)
              ("libuuid:static" ,util-linux "static")
              ("lzo" ,lzo)
              ("zlib" ,zlib)
              ("zlib:static" ,zlib "static")
              ("zstd" ,zstd)))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("asciidoc" ,asciidoc)
                     ("python" ,python)
                     ("xmlto" ,xmlto)
                     ;; For building documentation.
                     ("libxml2" ,libxml2)
                     ("docbook-xsl" ,docbook-xsl)
                     ;; For tests.
                     ("acl" ,acl)
                     ("which" ,which)
                     ;; The tests need 'grep' with perl regexp support.
                     ("grep" ,grep)))
    (home-page "https://btrfs.wiki.kernel.org/index.php/Main_Page")
    (synopsis "Create and manage btrfs copy-on-write file systems")
    (description "Btrfs is a @dfn{copy-on-write} (CoW) file system for Linux
aimed at implementing advanced features while focusing on fault tolerance,
repair and easy administration.")
    ;; GPL2+: crc32.c, radix-tree.c, raid6.c, rbtree.c.
    ;; GPL2: Everything else.
    (license (list license:gpl2 license:gpl2+))))

(define-public btrfs-progs/static
  (package
    (name "btrfs-progs-static")
    (version (package-version btrfs-progs))
    (source #f)
    (build-system trivial-build-system)
    (inputs
     `(("btrfs-progs:static" ,btrfs-progs "static")))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 ftw)
                      (srfi srfi-26))

         (let* ((btrfs  (assoc-ref %build-inputs "btrfs-progs:static"))
                (out    (assoc-ref %outputs "out"))
                (source (string-append btrfs "/bin/btrfs.static"))
                (target (string-append out "/bin/btrfs")))
           (mkdir-p (dirname target))
           (copy-file source target)
           (remove-store-references target)
           (chmod target #o555)
           #t))))
    (home-page (package-home-page btrfs-progs))
    (synopsis "Statically-linked btrfs command from btrfs-progs")
    (description "This package provides the statically-linked @command{btrfs}
from the btrfs-progs package.  It is meant to be used in initrds.")
    (license (package-license btrfs-progs))))

(define-public f2fs-tools-1.7
  (package
    (name "f2fs-tools")
    (version "1.7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://git.kernel.org/cgit/linux/kernel/git/jaegeuk"
                    "/f2fs-tools.git/snapshot/f2fs-tools-" version ".tar.gz"))
              (sha256
               (base32
                "1m6bn1ibq0p53m0n97il91xqgjgn2pzlz74lb5bfzassx7159m1k"))))

    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-headers
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (out-include (string-append out "/include")))
               (install-file "include/f2fs_fs.h" out-include)
               (install-file "mkfs/f2fs_format_utils.h" out-include)
               #t))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libuuid" ,util-linux)
       ("libselinux" ,libselinux)))
    (home-page "https://f2fs.wiki.kernel.org/")
    (synopsis "Userland tools for f2fs")
    (description
     "F2FS, the Flash-Friendly File System, is a modern file system
designed to be fast and durable on flash devices such as solid-state
disks and SD cards.  This package provides the userland utilities.")
    ;; The formatting utility, libf2fs and include/f2fs_fs.h is dual
    ;; GPL2/LGPL2.1, everything else is GPL2 only. See 'COPYING'.
    (license (list license:gpl2 license:lgpl2.1))))

(define-public f2fs-tools
  (package
    (inherit f2fs-tools-1.7)
    (name "f2fs-tools")
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://git.kernel.org/cgit/linux/kernel/git/jaegeuk"
                    "/f2fs-tools.git/snapshot/f2fs-tools-" version ".tar.gz"))
              (sha256
               (base32
                "15pn2fm9knn7p1vzfzy6msnrdl14p6y1gn4m2ka6ba5bzx6lw4p2"))))
    (inputs
     `(("libuuid" ,util-linux)))))

(define-public freefall
  (package
    (name "freefall")
    (version (package-version linux-libre))
    (source (package-source linux-libre))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'enter-subdirectory
                    (lambda _
                      (chdir "tools/laptop/freefall")
                      #t))
                  (delete 'configure)
                  (add-before 'build 'increase-timeout
                    (lambda _
                      ;; The default of 2 seconds is too low: it assumes an
                      ;; open lid and AC power without actually checking.
                      (substitute* "freefall.c"
                        (("alarm\\(2\\)") "alarm(5)"))
                      #t)))
       #:make-flags (list (string-append "PREFIX="
                                         (assoc-ref %outputs "out"))
                          "CC=gcc")
       #:tests? #f)) ;no tests
    (home-page (package-home-page linux-libre))
    (synopsis "Free-fall protection for spinning laptop hard drives")
    (description
     "Prevents shock damage to the internal spinning hard drive(s) of some
HP and Dell laptops.  When sudden movement is detected, all input/output
operations on the drive are suspended and its heads are parked on the ramp,
where they are less likely to cause damage to the spinning disc.  Requires a
drive that supports the ATA/ATAPI-7 IDLE IMMEDIATE command with unload
feature, and a laptop with an accelerometer.  It has no effect on SSDs.")
    (license license:gpl2)))

(define-public thinkfan
  (package
    (name "thinkfan")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vmatare/thinkfan.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "107vw0962hrwva3wra9n3hxlbfzg82ldc10qssv3dspja88g8psr"))))
    (build-system cmake-build-system)
    (arguments
     `(#:modules ((guix build cmake-build-system)
                  (guix build utils)
                  (srfi srfi-26))
       #:tests? #f                      ; no test target
       #:configure-flags
       ;; Enable reading temperatures from hard disks via S.M.A.R.T.
       ;; Upstream ‘defaults to OFF because libatasmart seems to be horribly
       ;; inefficient’.
       `("-DUSE_ATASMART:BOOL=ON")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'create-init-scripts
           ;; CMakeLists.txt relies on build-time symptoms of OpenRC and
           ;; systemd to patch and install their service files.  Fake their
           ;; presence rather than duplicating the build system below.  Leave
           ;; things like ‘/bin/kill’ because they're not worth a dependency.
           ;; The sysvinit needs manual patching, but since upstream doesn't
           ;; even provide the option to install it: don't.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (share (string-append out "/share/" ,name)))
               (substitute* "CMakeLists.txt"
                 (("pkg_check_modules\\((OPENRC|SYSTEMD) .*" _ package)
                  (format "option(~a_FOUND \"Faked\" ON)\n" package))
                 ;; That was easy!  Now we just need to fix the destinations.
                 (("/etc" directory)
                  (string-append out directory)))
               #t))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libatasmart" ,libatasmart)
       ("yaml-cpp" ,yaml-cpp)))
    (home-page "http://thinkfan.sourceforge.net/")
    (synopsis "Simple fan control program")
    (description
     "Thinkfan is a simple fan control program.  It reads temperatures,
checks them against configured limits and switches to appropriate (also
pre-configured) fan level.  It requires a working @code{thinkpad_acpi} or any
other @code{hwmon} driver that enables temperature reading and fan control
from userspace.")
    (license license:gpl3+)))

(define-public ntfs-3g
  (package
    (name "ntfs-3g")
    (version "2017.3.23")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://tuxera.com/opensource/"
                                  "ntfs-3g_ntfsprogs-" version ".tgz"))
              (patches (search-patches "ntfs-3g-CVE-2019-9755.patch"))
              (sha256
               (base32
                "1mb228p80hv97pgk3myyvgp975r9mxq56c6bdn1n24kngcfh4niy"))
              (modules '((guix build utils)))
              (snippet '(begin
                          ;; Install under $prefix.
                          (substitute* '("src/Makefile.in" "ntfsprogs/Makefile.in")
                            (("/sbin")
                             "@sbindir@"))
                          #t))))
    (build-system gnu-build-system)
    (inputs `(("util-linux" ,util-linux)
              ("fuse" ,fuse)))                    ;libuuid
    (native-inputs `(("pkg-config" ,pkg-config)))
    (arguments
     '(#:configure-flags (list "--exec-prefix=${prefix}"
                               "--with-fuse=external" ;use our own FUSE
                               "--enable-mount-helper"
                               "--enable-posix-acls"
                               "--enable-xattr-mappings")
       #:phases
       (modify-phases %standard-phases
         ;; If users install ntfs-3g, they probably want to make it the
         ;; default driver as well, so we opt for sensible defaults and link
         ;; mount.ntfs to mount.ntfs-3g.  (libmount tries to run mount.ntfs to
         ;; mount NTFS file systems.)
         (add-after 'install 'install-link
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (sbin (string-append out "/sbin")))
               (symlink "mount.ntfs-3g"
                        (string-append sbin "/mount.ntfs")))
             #t)))))
    (home-page "https://www.tuxera.com/community/open-source-ntfs-3g/")
    (synopsis "Read-write access to NTFS file systems")
    (description
     "NTFS-3G provides read-write access to NTFS file systems, which are
commonly found on Microsoft Windows.  It is implemented as a FUSE file system.
The package provides additional NTFS tools.")
    (license license:gpl2+)))

(define-public rdma-core
  (package
    (name "rdma-core")
    (version "14")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/linux-rdma/rdma-core"
                                  "/releases/download/v" version "/rdma-core-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0w03zd49k96bmly44qc8l0s9l671sd26k4wrilsp13xaspy048kd"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f ; no tests
       ;; Upstream uses the "ninja" build system and encourage distros
       ;; to do the same for consistency. They also recommend using the
       ;; "Release" build type.
       #:build-type "Release"
       #:configure-flags (list "-GNinja")
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (invoke "ninja"
                     "-j" (number->string (parallel-job-count)))))
         (replace 'install
           (lambda _
             (invoke "ninja" "install"))))))
    (native-inputs
     `(("ninja" ,ninja)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)))
    (inputs
     `(("libnl" ,libnl)
       ("udev" ,eudev)))
    (home-page "https://github.com/linux-rdma/rdma-core")
    (synopsis "Utilities and libraries for working with RDMA devices")
    (description
     "This package provides userspace components for the InfiniBand
subsystem of the Linux kernel.  Specifically it contains userspace
libraries for the following device nodes:

@enumerate
@item @file{/dev/infiniband/uverbsX} (@code{libibverbs})
@item @file{/dev/infiniband/rdma_cm} (@code{librdmacm})
@item @file{/dev/infiniband/umadX} (@code{libibumad})
@end enumerate

The following service daemons are also provided:
@enumerate
@item @code{srp_daemon} (for the @code{ib_srp} kernel module)
@item @code{iwpmd} (for iWARP kernel providers)
@item @code{ibacm} (for InfiniBand communication management assistant)
@end enumerate")
    ;; All library code is dual licensed under GPL2 and a custom MIT
    ;; variant. The package also includes some components covered by
    ;; other licenses. Consult COPYING.md for full details.
    (license
     (list license:gpl2
           (license:x11-style "See COPYING.BSD_MIT in the distribution")
           license:bsd-2             ; Files referring to COPYING.BSD_FB
           license:cc0               ; most files in ccan/
           license:bsd-3))))         ; providers/hfi1verbs are dual GPL2/BSD-3

(define-public perftest
  (package
    (name "perftest")
    (version "4.4-0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/linux-rdma/perftest/releases/download/v"
                           version "/perftest-" version ".g0927198.tar.gz"))
       (sha256
        (base32 "11ix4h0rrmqqyi84y55a9xnkvwsmwq0sywr46hvxzm4rqz4ma8vq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-header-paths
           (lambda _
             (substitute* '("src/raw_ethernet_fs_rate.c"
                            "src/raw_ethernet_resources.c"
                            "src/raw_ethernet_resources.h"
                            "src/raw_ethernet_send_burst_lat.c"
                            "src/raw_ethernet_send_bw.c"
                            "src/raw_ethernet_send_lat.c")
               (("/usr/include/netinet/ip.h") "netinet/ip.h"))
             #t)))))
    (inputs `(("rdma-core" ,rdma-core)))
    (home-page "https://github.com/linux-rdma/perftest/")
    (synopsis "Open Fabrics Enterprise Distribution (OFED) Performance Tests")
    (description "This is a collection of tests written over uverbs intended for
use as a performance micro-benchmark. The tests may be used for hardware or
software tuning as well as for functional testing.

The collection contains a set of bandwidth and latency benchmark such as:
@enumerate
@item Send        - @code{ib_send_bw} and @code{ib_send_lat}
@item RDMA Read   - @code{ib_read_bw} and @code{ib_read_lat}
@item RDMA Write  - @code{ib_write_bw} and @code{ib_wriet_lat}
@item RDMA Atomic - @code{ib_atomic_bw} and @code{ib_atomic_lat}
@item Native Ethernet (when working with MOFED2) - @code{raw_ethernet_bw}, @code{raw_ethernet_lat}
@end enumerate")
    (license license:gpl2)))

(define-public rng-tools
  (package
    (name "rng-tools")
    (home-page "https://github.com/nhorman/rng-tools")
    (version "6.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "19f75m6mzg8h7b4snzg7d6ypvkz6nq32lrpi9ja95gqz4wsd18a5"))))
    (build-system gnu-build-system)
    (arguments
     `(;; Avoid using OpenSSL, curl, and libxml2, reducing the closure by 166 MiB.
       #:configure-flags '("--without-nistbeacon"
                           "--without-pkcs11")))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libsysfs" ,sysfsutils)))
    (synopsis "Random number generator daemon")
    (description
     "Monitor a hardware random number generator, and supply entropy
from that to the system kernel's @file{/dev/random} machinery.")
    ;; The source package is offered under the GPL2+, but the files
    ;; 'rngd_rdrand.c' and 'rdrand_asm.S' are only available under the GPL2.
    (license (list license:gpl2 license:gpl2+))))

(define-public cpupower
  (package
    (name "cpupower")
    (version (package-version linux-libre))
    (source (package-source linux-libre))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'enter-subdirectory
                    (lambda _
                      (chdir "tools/power/cpupower")
                      #t))
                  (delete 'configure)
                  (add-before 'build 'fix-makefiles
                    (lambda _
                      (substitute* "Makefile"
                        (("/usr/") "/")
                        (("/bin/(install|pwd)" _ command) command))
                      (substitute* "bench/Makefile"
                        (("\\$\\(CC\\) -o") "$(CC) $(LDFLAGS) -o"))
                      #t)))
       #:make-flags (let ((out (assoc-ref %outputs "out")))
                      (list (string-append "DESTDIR=" out)
                            (string-append "LDFLAGS=-Wl,-rpath=" out "/lib")
                            "docdir=/share/doc/cpupower"
                            "confdir=$(docdir)/examples"
                            ;; The Makefile recommends the following changes
                            "DEBUG=false"
                            "PACKAGE_BUGREPORT=bug-guix@gnu.org"))
       #:tests? #f)) ;no tests
    (native-inputs `(("gettext" ,gettext-minimal)))
    (inputs `(("pciutils" ,pciutils)))
    (home-page (package-home-page linux-libre))
    (synopsis "CPU frequency and voltage scaling tools for Linux")
    (description
     "cpupower is a set of user-space tools that use the cpufreq feature of the
Linux kernel to retrieve and control processor features related to power saving,
such as frequency and voltage scaling.")
    (license license:gpl2)))

(define-public haveged
  (package
    (name "haveged")
    (version "1.9.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jirka-h/haveged.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hrwzjd4byq4fdrg8svww3d8x449k80jxxrjy9v6jvzhfv19rvxr"))))
    (build-system gnu-build-system)
    (home-page "http://www.issihosts.com/haveged")
    (synopsis "Entropy source for the Linux random number generator")
    (description
     "haveged generates an unpredictable stream of random numbers for use by
Linux's @file{/dev/random} and @file{/dev/urandom} devices.  The kernel's
standard mechanisms for filling the entropy pool may not be sufficient for
systems with high needs or limited user interaction, such as headless servers.

@command{haveged} runs as a privileged daemon, harvesting randomness from the
indirect effects of hardware events on hidden processor state using the HArdware
Volatile Entropy Gathering and Expansion (@dfn{HAVEGE}) algorithm.  It tunes
itself to its environment and provides the same built-in test suite for the
output stream as used on certified hardware security devices.

The quality of the randomness produced by this algorithm has not been proven.
It is recommended to run it together with another entropy source like rngd, and
not as a replacement for it.")
    (license (list (license:non-copyleft "file://nist/mconf.h")
                   (license:non-copyleft "file://nist/packtest.c")
                   license:public-domain        ; nist/dfft.c
                   license:gpl3+))))            ; everything else

(define-public ecryptfs-utils
  (package
    (name "ecryptfs-utils")
    (version "111")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://launchpad.net/ecryptfs/trunk/"
                           version "/+download/ecryptfs-utils_"
                           version ".orig.tar.gz"))
       (sha256
        (base32
         "0zwq19siiwf09h7lwa7n7mgmrr8cxifp45lmwgcfr8c1gviv6b0i"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list "--disable-pywrap")
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-source-shebangs 'patch-hardcoded-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (utils-linux (assoc-ref inputs "utils-linux"))
                   (cryptsetup (assoc-ref inputs "cryptsetup"))
                   (linux-pam (assoc-ref inputs "linux-pam"))
                   (lvm2 (assoc-ref inputs "lvm2")))
               (substitute* '("src/utils/ecryptfs-mount-private"
                              "src/utils/ecryptfs-umount-private"
                              "src/utils/ecryptfs-setup-private"
                              "src/utils/ecryptfs-setup-swap"
                              "src/utils/mount.ecryptfs.c"
                              "src/utils/umount.ecryptfs.c"
                              "src/pam_ecryptfs/pam_ecryptfs.c"
                              "src/desktop/ecryptfs-mount-private.desktop.in"
                              "src/desktop/ecryptfs-setup-private.desktop.in")
                 (("/bin/mount")
                  (string-append utils-linux "/bin/mount"))
                 (("/bin/umount")
                  (string-append utils-linux "/bin/umount"))
                 (("/sbin/mount.ecryptfs_private")
                  (string-append out "/sbin/mount.ecryptfs_private"))
                 (("/sbin/umount.ecryptfs_private")
                  (string-append out "/sbin/umount.ecryptfs_private"))
                 (("/usr/bin/ecryptfs-mount-private")
                  (string-append out "/bin/ecryptfs-mount-private"))
                 (("/usr/bin/ecryptfs-rewrite-file")
                  (string-append out "/bin/ecryptfs-rewrite-file"))
                 (("/usr/bin/ecryptfs-setup-private")
                  (string-append out "/bin/ecryptfs-setup-private"))
                 (("/sbin/cryptsetup")
                  (string-append cryptsetup "/sbin/cryptsetup"))
                 (("/sbin/unix_chkpwd")
                  (string-append linux-pam "/sbin/unix_chkpwd"))
                 (("/sbin/dmsetup")
                  (string-append lvm2 "/sbin/dmsetup")))))))))
    (native-inputs
     `(("intltool" ,intltool)
       ("perl" ,perl)                   ; for pod2man
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("keyutils" ,keyutils)
       ("linux-pam" ,linux-pam)
       ("utils-linux" ,util-linux)
       ("cryptsetup" ,cryptsetup)
       ("lvm2" ,lvm2)
       ("nss" ,nss)))
    (home-page "http://ecryptfs.org/")
    (synopsis "eCryptfs cryptographic file system utilities")
    (description
     "eCryptfs is a POSIX-compliant stacked cryptographic file system for Linux.
Each file's cryptographic meta-data is stored inside the file itself, along
with the encrypted contents.  This allows individual encrypted files to be
copied between hosts and still be decrypted with the proper key.  eCryptfs is a
native Linux file system, and has been part of the Linux kernel since version
2.6.19.  This package contains the userland utilities to manage it.")
    ;; The files src/key_mod/ecryptfs_key_mod_{openssl,pkcs11_helper,tspi}.c
    ;; grant additional permission to link with OpenSSL.
    (license license:gpl2+)))

(define-public libnfsidmap
  (package
    (name "libnfsidmap")
    (version "0.27")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://fedorapeople.org/~steved/"
                           name "/" version "/" name "-" version ".tar.bz2"))
       (sha256
        (base32 "0bg2bcii424mf1bnp3fssr8jszbvhdxl7wvifm1yf6g596v8b8i5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list
                          (string-append "--with-pluginpath="
                                         (assoc-ref %outputs "out")
                                         "/lib/libnfsidmap"))))
    (native-inputs
     `(("autoconf" ,autoconf)))         ; 0.27 still needs autoheader
    (home-page
     "http://www.citi.umich.edu/projects/nfsv4/crossrealm/libnfsidmap_config.html")
    (synopsis "NFSv4 support library for name/ID mapping")
    (description "Libnfsidmap is a library holding mulitiple methods of
mapping names to ids and visa versa, mainly for NFSv4.  It provides an
extensible array of mapping functions, currently consisting of two choices:
the default @code{nsswitch} and the experimental @code{umich_ldap}.")
    (license (license:non-copyleft "file://COPYING"
                                   "See COPYING in the distribution."))))

(define-public module-init-tools
  (package
    (name "module-init-tools")
    (version "3.16")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://kernel.org/linux/utils/kernel/module-init-tools/"
                   "module-init-tools-" version ".tar.bz2"))
             (sha256
              (base32
               "0jxnz9ahfic79rp93l5wxcbgh4pkv85mwnjlbv1gz3jawv5cvwp1"))
             (patches (search-patches "module-init-tools-moduledir.patch"))))
    (build-system gnu-build-system)
    (arguments
     ;; FIXME: The upstream tarball lacks man pages, and building them would
     ;; require DocBook & co.  We used to use Gentoo's pre-built man pages,
     ;; but they vanished.  In the meantime, fake it.
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fake-docbook
           (lambda _
             (substitute* "Makefile.in"
               (("^DOCBOOKTOMAN.*$")
                "DOCBOOKTOMAN = true\n"))
             #t)))))
    (home-page "https://www.kernel.org/pub/linux/utils/kernel/module-init-tools/")
    (synopsis "Tools for loading and managing Linux kernel modules")
    (description
     "Tools for loading and managing Linux kernel modules, such as
@code{modprobe}, @code{insmod}, @code{lsmod}, and more.")
    (license license:gpl2+)))

(define-public mcelog
  (package
    (name "mcelog")
    (version "154")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://git.kernel.org/cgit/utils/cpu/mce/"
                                  "mcelog.git/snapshot/v" version ".tar.gz"))
              (sha256
               (base32
                "07628cr05f50m7lsvw26wxlnb7qcl0x6rymdpp5spqzhz91l58p3"))
              (file-name (string-append name "-" version ".tar.gz"))
              (modules '((guix build utils)))
              (snippet
               `(begin
                  ;; The snapshots lack a .git directory,
                  ;; breaking ‘git describe’.
                  (substitute* "Makefile"
                    (("\"unknown\"") (string-append "\"v" ,version "\"")))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'configure))  ; no configure script
       #:make-flags (let ((out (assoc-ref %outputs "out")))
                      (list "CC=gcc"
                            (string-append "prefix=" out)
                            (string-append "DOCDIR=" out "/share/doc/mcelog")
                            "etcprefix=$(DOCDIR)/examples"))
       ;; The tests will only run as root on certain supported CPU models.
       #:tests? #f))
    (supported-systems (list "i686-linux" "x86_64-linux"))
    (home-page "https://mcelog.org/")
    (synopsis "Machine check monitor for x86 Linux systems")
    (description
     "The mcelog daemon is required by the Linux kernel to log memory, I/O, CPU,
and other hardware errors on x86 systems.  It can also perform user-defined
tasks, such as bringing bad pages off-line, when configurable error thresholds
are exceeded.")
    (license license:gpl2)))

(define-public mtd-utils
  (package
    (name "mtd-utils")
    (version "2.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "ftp://ftp.infradead.org/pub/mtd-utils/"
                    "mtd-utils-" version ".tar.bz2"))
              (sha256
               (base32
                "1f30jszknc5v6ykmil8ajxgksmcg54q3rsp84jsancp9x0dycggv"))))
    (arguments
     '(#:configure-flags '("--enable-unit-tests")))
    (native-inputs
     `(("cmocka" ,cmocka)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("acl" ,acl) ; for XATTR
       ("libuuid" ,util-linux)
       ("lzo" ,lzo)
       ("zlib" ,zlib)))
    (build-system gnu-build-system)
    (synopsis "MTD Flash Storage Utilities")
    (description "This package provides utilities for testing, partitioning, etc
of flash storage.")
    (home-page "http://www.linux-mtd.infradead.org/")
    (license
      (list license:gpl2 ; Almost everything is gpl2 or gpl2+
            license:mpl1.1 ; All ftl* files
            license:expat)))) ; libiniparser

(define-public libseccomp
  (package
    (name "libseccomp")
    (version "2.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/seccomp/libseccomp/"
                                  "releases/download/v" version
                                  "/libseccomp-" version ".tar.gz"))
              (sha256
               (base32
                "1s06h2cgk0xxwmhwj72z33bllafc1xqnxzk2yyra2rmg959778qw"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("which" ,which)))
    (synopsis "Interface to Linux's seccomp syscall filtering mechanism")
    (description "The libseccomp library provides an easy to use, platform
independent, interface to the Linux Kernel's syscall filtering mechanism.  The
libseccomp API is designed to abstract away the underlying BPF based syscall
filter language and present a more conventional function-call based filtering
interface that should be familiar to, and easily adopted by, application
developers.")
    (home-page "https://github.com/seccomp/libseccomp")
    (license license:lgpl2.1)))

(define-public radeontop
  (package
    (name "radeontop")
    (version "1.1")
    (home-page "https://github.com/clbr/radeontop/")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page "archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1fv06j5c99imvzkac3j40lgjhr5b2i77fnyffhlvj92bli1fm1c6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  ;; getver.sh uses ‘git --describe’, isn't worth an extra git
                  ;; dependency, and doesn't even work on release(!) tarballs.
                  (add-after 'unpack 'report-correct-version
                    (lambda _
                      (substitute* "getver.sh"
                        (("ver=unknown")
                         (string-append "ver=" ,version)))
                      #t))
                  (delete 'configure))  ; no configure script
       #:make-flags (list "CC=gcc"
                          (string-append "PREFIX=" %output))
       #:tests? #f))                    ; no tests
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libdrm" ,libdrm)
       ("libpciaccess" ,libpciaccess)
       ("libxcb" ,libxcb)
       ("ncurses" ,ncurses)))
    (synopsis "Usage monitor for AMD Radeon graphics")
    (description "RadeonTop monitors resource consumption on supported AMD
Radeon Graphics Processing Units (GPUs), either in real time as bar graphs on
a terminal or saved to a file for further processing.  It measures both the
activity of the GPU as a whole, which is also accurate during OpenCL
computations, as well as separate component statistics that are only meaningful
under OpenGL graphics workloads.")
    (license license:gpl3)))

(define-public efivar
  (package
    (name "efivar")
    (version "37")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/rhboot/" name
                                  "/releases/download/" version "/" name
                                  "-" version ".tar.bz2"))
              (sha256
               (base32
                "17vvfivhsrszh7q39b6npjsrhrhsjf1cmmcpp3xrh6wh7ywzwrrw"))))
    (build-system gnu-build-system)
    (arguments
     `(;; Tests require a UEFI system and is not detected in the chroot.
       #:tests? #f
       #:make-flags (list (string-append "prefix=" %output)
                          (string-append "libdir=" %output "/lib")
                          "CC_FOR_BUILD=gcc"
                          (string-append "LDFLAGS=-Wl,-rpath=" %output "/lib"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("popt" ,popt)))
    (home-page "https://github.com/rhboot/efivar")
    (synopsis "Tool and library to manipulate EFI variables")
    (description "This package provides a library and a command line
interface to the variable facility of UEFI boot firmware.")
    (license license:lgpl2.1+)))

(define-public efibootmgr
  (package
    (name "efibootmgr")
    (version "16")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/rhinstaller/efibootmgr"
                                  "/releases/download/" version "/efibootmgr"
                                  "-" version ".tar.bz2"))
              (sha256
               (base32
                "0pzn67vxxaf7jna4cd0i4kqm60h04kb21hckksv9z82q9gxra1wm"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;no tests
       #:make-flags (list (string-append "prefix=" %output)
                          (string-append "libdir=" %output "/lib")
                          ;; EFIDIR denotes a subdirectory relative to the
                          ;; EFI System Partition where the loader will be
                          ;; installed (known as OS_VENDOR in the code).
                          ;; GRUB overrides this, as such it's only used if
                          ;; nothing else is specified on the command line.
                          "EFIDIR=gnu")
       #:phases (modify-phases %standard-phases (delete 'configure))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("efivar" ,efivar)
       ("popt" ,popt)))
    (home-page "https://github.com/rhinstaller/efibootmgr")
    (synopsis "Modify the Extensible Firmware Interface (EFI) boot manager")
    (description
     "@code{efibootmgr} is a user-space application to modify the Intel
Extensible Firmware Interface (EFI) Boot Manager.  This application can
create and destroy boot entries, change the boot order, change the next
running boot option, and more.")
    (license license:gpl2+)))

(define-public sysstat
  (package
    (name "sysstat")
    (version "11.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://perso.orange.fr/sebastien.godard/"
                                  "sysstat-" version ".tar.xz"))
              (sha256
               (base32
                "1ryf9myjzpa2279i3rvsh6fr5psm6qvr5r9kbm1sxyspapxcms82"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; No test suite.
       ;; Without this flag, it tries to install the man pages with group 'root'
       ;; and fails because /etc/passwd lacks an entry for the root user.
       #:configure-flags
       (list "--disable-file-attr"
             (string-append "conf_dir=" (assoc-ref %outputs "out") "/etc"))
       #:phases
       (modify-phases %standard-phases
         ;; The build process tries to create '/var/lib/sa', so we skip that
         ;; instruction.
         (add-after 'build 'skip-touching-var
           (lambda _
             (substitute* "Makefile"
               (("mkdir -p \\$\\(DESTDIR\\)\\$\\(SA_DIR\\)")
                ""))
             #t)))))
    (home-page "http://sebastien.godard.pagesperso-orange.fr/")
    (synopsis "Performance monitoring tools for Linux")
    (description "The sysstat utilities are a collection of performance
monitoring tools for Linux.  These include @code{mpstat}, @code{iostat},
@code{tapestat}, @code{cifsiostat}, @code{pidstat}, @code{sar}, @code{sadc},
@code{sadf} and @code{sa}.")
    (license license:gpl2+)))

(define-public light
  (package
    (name "light")
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                     "https://github.com/haikarainen/light/archive/v"
                     version ".tar.gz"))
              (sha256
               (base32
                "1gfvsw7gh5pis733l7j54vzp272pvjyzbg8a0pvapfmg0s7mip97"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)))
    (home-page "https://haikarainen.github.io/light/")
    (synopsis "GNU/Linux application to control backlights")
    (description
     "Light is a program to send commands to screen backlight controllers
under GNU/Linux.  Features include:

@itemize
@item It does not rely on X.
@item Light can automatically figure out the best controller to use, making
full use of underlying hardware.
@item It is possible to set a minimum brightness value, as some controllers
set the screen to be pitch black at a vaĺue of 0 (or higher).
@end itemize

Light is the successor of lightscript.")
    (license license:gpl3+)))

(define-public brightnessctl
  (let ((commit "6a791e7694aeeb5d027f71c6098e5182cf03371c"))
    (package
      (name "brightnessctl")
      (version (git-version "0.4" "0" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Hummer12007/brightnessctl/")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1n1gb8ldgqv3vs565yhk1w4jfvrviczp94r8wqlkv5q6ab43c8w9"))))
      (build-system gnu-build-system)
      (arguments
       '(#:tests? #f                    ; no tests
         #:make-flags (list "CC=gcc"
                            (string-append "PREFIX=" %output)
                            (string-append "UDEVDIR=" %output "/lib/udev/rules.d/"))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (add-after 'unpack 'adjust-udev-rules
             (lambda _
               (substitute* "90-brightnessctl.rules"
                 (("/bin/") "/run/current-system/profile/bin/"))
               #t)))))
      (home-page "https://github.com/Hummer12007/brightnessctl")
      (synopsis "Backlight and LED brightness control")
      (description
       "This program allows you read and control device brightness.  Devices
include backlight and LEDs.  It can also preserve current brightness before
applying the operation, such as on lid close.

The appropriate permissions must be set on the backlight or LED control
interface in sysfs, which can be accomplished with the included udev rules.")
      (license license:expat))))

(define-public tlp
  (package
    (name "tlp")
    (version "1.2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/linrunner/"
                    (string-upcase name)
                    "/archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "059kxrpxx580mm6p0z2a421nxngszyh4yqqhbgvn04b6a7dbsa2w"))))
    (inputs `(("bash" ,bash)
              ("dbus" ,dbus)
              ("ethtool" ,ethtool)
              ("eudev" ,eudev)
              ("grep" ,grep)
              ("hdparm" ,hdparm)
              ("inetutils" ,inetutils)
              ("iw" ,iw)
              ("kmod" ,kmod)
              ("pciutils" ,pciutils)
              ("perl" ,perl)
              ("rfkill" ,rfkill)
              ("sed" ,sed)
              ("usbutils" ,usbutils)
              ("util-linux" ,util-linux)
              ("wireless-tools" ,wireless-tools)))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-before 'build 'setenv
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (setenv "TLP_WITH_SYSTEMD" "0")
               (setenv "TLP_NO_INIT" "1")
               (setenv "TLP_NO_PMUTILS" "1")
               (setenv "TLP_SBIN" (string-append out "/bin"))
               (setenv "TLP_BIN" (string-append out "/bin"))
               (setenv "TLP_TLIB" (string-append out "/share/tlp"))
               (setenv "TLP_FLIB" (string-append out "/share/tlp/func.d"))
               (setenv "TLP_ULIB" (string-append out "/lib/udev"))
               (setenv "TLP_CONF" "/etc/tlp")
               (setenv "TLP_ELOD"
                       (string-append out "/lib/elogind/system-sleep"))
               (setenv "TLP_SHCPL"
                       (string-append out "/share/bash-completion/completions"))
               (setenv "TLP_MAN" (string-append out "/share/man"))
               (setenv "TLP_META" (string-append out "/share/metainfo"))
               #t)))
         (delete 'check)                ; no tests
         (add-before 'install 'fix-installation
           (lambda _
             ;; Stop the Makefile from trying to create system directories.
             (substitute* "Makefile"
               (("\\[ -f \\$\\(_CONF\\) \\]") "#")
               (("install -d -m 755 \\$\\(_VAR\\)") "#"))
             #t))
         (replace 'install
           (lambda _
             (invoke "make" "install-tlp" "install-man")))
         (add-after 'install 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((bin (string-append (assoc-ref outputs "out") "/bin"))
                    (bin-files (find-files bin ".*")))
               (define (bin-directory input-name)
                 (string-append (assoc-ref inputs input-name) "/bin"))
               (define (sbin-directory input-name)
                 (string-append (assoc-ref inputs input-name) "/sbin"))
               (for-each (lambda (program)
                           (wrap-program program
                             `("PATH" ":" prefix
                               ,(append
                                 (map bin-directory '("bash"
                                                      "coreutils"
                                                      "dbus"
                                                      "eudev"
                                                      "grep"
                                                      "inetutils"
                                                      "kmod"
                                                      "perl"
                                                      "sed"
                                                      "usbutils"
                                                      "util-linux"))
                                 (map sbin-directory '("ethtool"
                                                       "hdparm"
                                                       "iw"
                                                       "pciutils"
                                                       "rfkill"
                                                       "wireless-tools"))))))
                         bin-files)
               #t))))))
    (home-page "http://linrunner.de/en/tlp/tlp.html")
    (synopsis "Power management tool for Linux")
    (description "TLP is a power management tool for Linux.  It comes with
a default configuration already optimized for battery life.  Nevertheless,
TLP is customizable to fulfil system requirements.  TLP settings are applied
every time the power supply source is changed.")
    ;; 'COPYING' is a custom version that says that one file is GPLv3+ and the
    ;; rest is GPLv2+.
    (license (list license:gpl2+ license:gpl3+))))

(define-public lshw
  (package
    (name "lshw")
    (version "B.02.18")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.ezix.org/software/"
                                  "files/lshw-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "0brwra4jld0d53d7jsgca415ljglmmx1l2iazpj4ndilr48yy8mf"))))
    (build-system gnu-build-system)
    (arguments
      `(#:phases (modify-phases %standard-phases (delete 'configure))
        #:tests? #f ; no tests
        #:make-flags
          (list (string-append "PREFIX=" (assoc-ref %outputs "out")))))
    (synopsis "List hardware information")
    (description
     "@command{lshw} (Hardware Lister) is a small tool to provide
detailed information on the hardware configuration of the machine.
It can report exact memory configuration, firmware version, mainboard
configuration, CPU version and speed, cache configuration, bus speed,
and more on DMI-capable x86 or EFI (IA-64) systems and on some PowerPC
machines (PowerMac G4 is known to work).")
    (home-page "https://www.ezix.org/project/wiki/HardwareLiSter")
    (license license:gpl2+)))

(define-public libmnl
  (package
    (name "libmnl")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://netfilter.org/libmnl/"
                            "libmnl-" version ".tar.bz2"))
        (sha256
         (base32
          "108zampspaalv44zn0ar9h386dlfixpd149bnxa5hsi8kxlqj7qp"))))
    (build-system gnu-build-system)
    (home-page "https://www.netfilter.org/projects/libmnl/")
    (synopsis "Netlink utility library")
    (description "Libmnl is a minimalistic user-space library oriented to
Netlink developers.  There are a lot of common tasks in parsing, validating,
constructing of both the Netlink header and TLVs that are repetitive and easy to
get wrong.  This library aims to provide simple helpers that allows you to
re-use code and to avoid re-inventing the wheel.")
    (license license:lgpl2.1+)))

(define-public libnftnl
  (package
    (name "libnftnl")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://netfilter.org/libnftnl/"
                           "libnftnl-" version ".tar.bz2"))
       (sha256
        (base32 "0pffmsv41alsn5ac7mwnb9fh3qpwzqk13jrzn6c5i71wq6kbgix5"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libmnl" ,libmnl)))
    (home-page "https://www.netfilter.org/projects/libnftnl/index.html")
    (synopsis "Netlink programming interface to the Linux nf_tables subsystem")
    (description "Libnftnl is a userspace library providing a low-level netlink
programming interface to the in-kernel nf_tables subsystem.  The library
libnftnl has been previously known as libnftables.  This library is currently
used by nftables.")
    (license license:gpl2+)))

(define-public nftables
  (package
    (name "nftables")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.nftables.org/projects/nftables"
                           "/files/nftables-" version ".tar.bz2"))
       (sha256
        (base32
         "14bygs6vg2v448cw5r4pxqi8an29hw0m9vab8hpmgjmrzjsq30dd"))))
    (build-system gnu-build-system)
    (arguments `(#:configure-flags
                 '("--disable-man-doc"))) ; FIXME: Needs docbook2x.
    (inputs `(("bison" ,bison)
              ("flex" ,flex)
              ("gmp" ,gmp)
              ("libmnl" ,libmnl)
              ("libnftnl" ,libnftnl)
              ("readline" ,readline)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.nftables.org")
    (synopsis "Userspace utility for Linux packet filtering")
    (description "nftables is the project that aims to replace the existing
{ip,ip6,arp,eb}tables framework.  Basically, this project provides a new packet
filtering framework, a new userspace utility and also a compatibility layer for
{ip,ip6}tables.  nftables is built upon the building blocks of the Netfilter
infrastructure such as the existing hooks, the connection tracking system, the
userspace queueing component and the logging subsystem.")
    (license license:gpl2)))

(define-public proot
  (package
    (name "proot")
    (version "5.1.0")
    (home-page "https://github.com/proot-me/PRoot")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page "/archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "11h30i83vdhc3khlj6hrh3a21sbmmz8nhfv09vkf6b9bcs1biz2h"))
              (patches (search-patches "proot-test-fhs.patch"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags '("-C" "src")

       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-before 'build 'set-shell-file-name
                    (lambda* (#:key inputs #:allow-other-keys)
                      (substitute* (find-files "src" "\\.[ch]$")
                        (("\"/bin/sh\"")
                         (string-append "\""
                                        (assoc-ref inputs "bash")
                                        "/bin/sh\"")))
                      #t))
                  (add-before 'check 'fix-fhs-assumptions-in-tests
                    (lambda _
                      (substitute* "tests/test-c6b77b77.mk"
                        (("/bin/bash") (which "bash"))
                        (("/usr/bin/test") (which "test")))
                      (substitute* '("tests/test-16573e73.c")
                        (("/bin/([a-z-]+)" _ program)
                         (which program)))

                      (substitute* (find-files "tests" "\\.sh$")
                        ;; Some of the tests try to "bind-mount" /bin/true.
                        (("-b /bin/true:")
                         (string-append "-b " (which "true") ":"))
                        ;; Likewise for /bin.
                        (("-b /bin:") "-b /gnu:")
                        ;; Others try to run /bin/sh.
                        (("/bin/sh") (which "sh"))
                        ;; Others assume /etc/fstab exists.
                        (("/etc/fstab") "/etc/passwd"))

                      (substitute* "tests/GNUmakefile"
                        (("-b /bin:") "-b /gnu:"))

                      ;; XXX: This test fails in an obscure corner case, just
                      ;; skip it.
                      (delete-file "tests/test-kkkkkkkk.c")

                      #t))
                  (replace 'check
                    (lambda _
                      (let ((n (parallel-job-count)))
                        ;; For some reason we get lots of segfaults with
                        ;; seccomp support (x86_64, Linux-libre 4.11.0).
                        (setenv "PROOT_NO_SECCOMP" "1")

                        ;; Most of the tests expect "/bin" to be in $PATH so
                        ;; they can run things that live in $ROOTFS/bin.
                        (setenv "PATH"
                                (string-append (getenv "PATH") ":/bin"))

                        (invoke "make" "check" "-C" "tests"
                                ;;"V=1"
                                "-j" (number->string n)))))
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; The 'install' rule does nearly nothing.
                      (let* ((out (assoc-ref outputs "out"))
                             (man1 (string-append out "/share/man/man1")))
                        ;; TODO: 'make install-care' (does not even
                        ;; build currently.)
                        (invoke "make" "-C" "src" "install"
                                (string-append "PREFIX=" out))

                        (mkdir-p man1)
                        (copy-file "doc/proot/man.1"
                                   (string-append man1 "/proot.1"))
                        #t))))))
    (native-inputs `(("which" ,which)

                     ;; For 'mcookie', used by some of the tests.
                     ("util-linux" ,util-linux)))
    (inputs `(("talloc" ,talloc)))
    (synopsis "Unprivileged chroot, bind mount, and binfmt_misc")
    (description
     "PRoot is a user-space implementation of @code{chroot}, @code{mount --bind},
and @code{binfmt_misc}.  This means that users don't need any privileges or
setup to do things like using an arbitrary directory as the new root
file system, making files accessible somewhere else in the file system
hierarchy, or executing programs built for another CPU architecture
transparently through QEMU user-mode.  Also, developers can use PRoot as a
generic process instrumentation engine thanks to its extension mechanism.
Technically PRoot relies on @code{ptrace}, an unprivileged system-call
available in the kernel Linux.")
    (license license:gpl2+)))

(define-public proot-static
  (package
    (inherit proot)
    (name "proot-static")
    (synopsis
     "Unprivileged chroot, bind mount, and binfmt_misc (statically linked)")
    (inputs `(("talloc" ,talloc/static)))
    (arguments
     (substitute-keyword-arguments (package-arguments proot)
       ((#:make-flags flags)
        `(cons "LDFLAGS = -ltalloc -static -static-libgcc" ,flags))
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'strip 'remove-store-references
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out")))
                 (with-directory-excursion out
                   (remove-store-references "bin/proot")
                   #t))))))
       ((#:allowed-references _ '("out"))
        '("out"))))))

(define-public cpuid
  (package
    (name "cpuid")
    (version "20180519")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.etallen.com/cpuid/cpuid-"
                                  version ".src.tar.gz"))
              (sha256
               (base32
                "16pzwyifc9glpk1hm6bqb5d1a7cw0qnqiamh5sbvqg7j6sz26y4n"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags '("CC=gcc")
       #:tests? #f                      ; no tests
       #:phases (modify-phases %standard-phases
                  (delete 'configure)   ; no configure script
                  (add-before 'install 'fix-makefile
                    (lambda* (#:key outputs #:allow-other-keys)
                      (substitute* "Makefile"
                        (("\\$\\(BUILDROOT\\)/usr") (assoc-ref outputs "out")))
                      ;; Make the compressed manpages writable so that the
                      ;; reset-gzip-timestamps phase does not error out.
                      (substitute* "Makefile"
                        (("-m 444") "-m 644"))
                      #t)))))
    (inputs `(("perl" ,perl)))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (home-page "http://www.etallen.com/cpuid.html")
    (synopsis "Linux tool to dump x86 CPUID information about the CPU(s)")
    (description "cpuid dumps detailed information about the CPU(s) gathered
from the CPUID instruction, and also determines the exact model of CPU(s).  It
supports Intel, AMD, and VIA CPUs, as well as older Transmeta, Cyrix, UMC,
NexGen, Rise, and SiS CPUs.")
    (license license:gpl2+)))

(define-public jmtpfs
  (package
    (name "jmtpfs")
    (version "0.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/JasonFerrara/jmtpfs/archive/v"
                            version ".tar.gz"))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "10v8d7mmx8b8123x5f9y9zaaa428ms6wkngwn2ra71n5a53wrjn0"))))
    (build-system gnu-build-system)
    (inputs
     `(("file" ,file)
       ("fuse" ,fuse)
       ("libmtp" ,libmtp)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://github.com/JasonFerrara/jmtpfs")
    (synopsis "Use a FUSE file system to access data over MTP")
    (description "jmtpfs uses FUSE (file system in userspace) to provide access
to data over the Media Transfer Protocol (MTP).  Unprivileged users can mount
the MTP device as a file system.")
    (license license:gpl3)))

(define-public procenv
  (package
   (name "procenv")
   (version "0.50")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://github.com/jamesodhunt/procenv/archive/"
                         version ".tar.gz"))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32 "0dvscyf47i3j5ay0amncqmqw9kd916689r2pqdvpnsrhp6j46zp1"))))
   (build-system gnu-build-system)
   (arguments `(#:configure-flags '("--disable-silent-rules")))
   (native-inputs `(("pkg-config" ,pkg-config)))
   (inputs `(("expat" ,expat) ("libcap" ,libcap) ("check" ,check)
             ("groff" ,groff)           ; for tests
             ("libselinux" ,libselinux)))
   (synopsis "Utility to show process environment")
   (description "Procenv is a command-line tool that displays as much detail about
itself and its environment as possible.  It can be used as a test
tool, to understand the type of environment a process runs in, and for
comparing system environments.")
   (home-page "http://github.com/jamesodhunt/procenv/")
   (license license:gpl3+)))

(define-public libfabric
  (package
    (name "libfabric")
    (version "1.4.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/ofiwg/libfabric/releases/download/v"
                       version "/libfabric-" version ".tar.bz2"))
       (sha256
        (base32 "19l2m1frna1l765z4j7wl8hp4rb9wrh0hy5496685hd183hmy5pv"))))
    (build-system gnu-build-system)
    (inputs `(("rdma-core" ,rdma-core)
              ,@(match (%current-system)
                       ((member (package-supported-systems psm))
                        `(("psm" ,psm)))
                       (_ `()))
              ("libnl" ,libnl)))
    (home-page "https://ofiwg.github.io/libfabric/")
    (synopsis "Open Fabric Interfaces")
    (description
     "OpenFabrics Interfaces (OFI) is a framework focused on exporting fabric
communication services to applications.  OFI is best described as a collection
of libraries and applications used to export fabric services.  The key
components of OFI are: application interfaces, provider libraries, kernel
services, daemons, and test applications.

Libfabric is a core component of OFI.  It is the library that defines and
exports the user-space API of OFI, and is typically the only software that
applications deal with directly.  It works in conjunction with provider
libraries, which are often integrated directly into libfabric.")
    (license (list license:bsd-2 license:gpl2)))) ;dual

(define-public psm
  (package
    (name "psm")
    (version "3.3.20170428")
    (home-page "https://github.com/intel/psm")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference (url home-page)
                           (commit "604758e76dc31e68d1de736ccf5ddf16cb22355b")))
       (file-name (string-append "psm-" version ".tar.gz"))
       (sha256
        (base32 "0nsb325dmhn5ia3d2cnksqr0gdvrrx2hmvlylfgvmaqdpq76zm85"))
       (patches (search-patches
                 "psm-arch.patch"     ; uname -p returns "unknown" on Debian 9
                 "psm-ldflags.patch"  ; build shared lib with LDFLAGS
                 "psm-repro.patch"))))  ; reproducibility
    (build-system gnu-build-system)
    (inputs `(("libuuid" ,util-linux)))
    (arguments
     '(#:make-flags `("PSM_USE_SYS_UUID=1" "CC=gcc" "WERROR="
                      ,(string-append "INSTALL_PREFIX=" %output)
                      ,(string-append "LDFLAGS=-Wl,-rpath=" %output "/lib"))
       #:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-after 'unpack 'patch-/usr/include
                    (lambda _
                      (substitute* "Makefile"
                        (("\\$\\{DESTDIR}/usr/include")
                         (string-append %output "/include")))
                      (substitute* "Makefile"
                        (("/lib64") "/lib"))
                      #t))
                  (add-after 'unpack 'patch-sysmacros
                    (lambda _
                      (substitute* "ipath/ipath_proto.c"
                        (("#include <sys/poll.h>" m)
                         (string-append m "\n"
                                        "#include <sys/sysmacros.h>")))
                      #t)))))
    (synopsis "Intel Performance Scaled Messaging (PSM) Libraries")
    (description
     "The PSM Messaging API, or PSM API, is Intel's low-level user-level
communications interface for the True Scale family of products.  PSM users are
enabled with mechanisms necessary to implement higher level communications
interfaces in parallel environments.")
    ;; Only Intel-compatable processors are supported.
    (supported-systems '("i686-linux" "x86_64-linux"))
    (license (list license:bsd-2 license:gpl2)))) ;dual

(define-public snapscreenshot
  (package
    (name "snapscreenshot")
    (version "1.0.14.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://bisqwit.iki.fi/src/arch/"
                           name "-" version ".tar.bz2"))
       (sha256
        (base32 "0gzvqsbf6a2sbd1mqvj1lbm57i2bm5k0cr6ncr821d1f32gw03mk"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (let ((out (assoc-ref %outputs "out")))
         (list (string-append "BINDIR=" out "/bin")
               (string-append "MANDIR=" out "/share/man")))
       #:tests? #f                      ; no test suite
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; ./configure is a snarky no-op
         (add-before 'install 'fix-ownership
           ;; Install binaries owned by ‘root’ instead of the nonexistent ‘bin’.
           (lambda _
             (substitute* "depfun.mak"
               ((" -o bin -g bin ") " "))
             #t))
         (add-before 'install 'create-output-directories
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/share/man/man1"))
               #t))))))
    (home-page "https://bisqwit.iki.fi/source/snapscreenshot.html")
    (synopsis "Take screenshots of one or more Linux text consoles")
    (description
     "snapscreenshot saves a screenshot of one or more Linux text consoles as a
Targa (@dfn{.tga}) image.  It can be used by anyone with read access to the
relevant @file{/dev/vcs*} file(s).")
    (license license:gpl2)))

(define-public fbcat
  (package
    (name "fbcat")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/jwilk/fbcat/releases/download/"
                           version "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "0pj9hxmwhbz6kmd7847yx2jh1scl9l25zgndyi8s9vlzdkq2q8d7"))))
    (build-system gnu-build-system)
    (inputs
     ;; The ‘fbgrab’ wrapper can use one of several PPM-to-PNG converters.  We
     ;; choose netpbm simply because it's the smallest.  It still adds ~94 MiB
     ;; to an otherwise tiny package, so we put ‘fbgrab’ in its own output.
     `(("pnmtopng" ,netpbm)))
    (outputs (list "out" "fbgrab"))
    (arguments
     `(#:make-flags
       (list "CC=gcc"
             (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-after 'build 'qualify-references
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((pnmtopng (assoc-ref inputs "pnmtopng"))
                    (out (assoc-ref outputs "out")))
               (substitute* "fbgrab"
                 (("fbcat" all)
                  (string-append out "/bin/" all))
                 (("pnmtopng" all)
                  (string-append pnmtopng "/bin/" all)))
               #t)))
         (add-after 'install 'split-fbgrab-output
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (out:fbgrab (assoc-ref outputs "fbgrab")))
               (for-each (lambda (file)
                           (let ((old (string-append out "/" file))
                                 (new (string-append out:fbgrab "/" file)))
                             (mkdir-p (dirname new))
                             (rename-file old new)))
                         (list "bin/fbgrab"
                               "share/man/man1/fbgrab.1"))
               #t))))))
    (home-page "https://jwilk.net/software/fbcat")
    (synopsis "Take a screenshot of the contents of the Linux framebuffer")
    (description
     "fbcat saves the contents of the Linux framebuffer (@file{/dev/fb*}), or
a dump therof.  It supports a wide range of drivers and pixel formats.
@command{fbcat} can take screenshots of virtually any application that can be
made to write its output to the framebuffer, including (but not limited to)
text-mode or graphical applications that don't use a display server.

Also included is @command{fbgrab}, a wrapper around @command{fbcat} that
emulates the behaviour of Gunnar Monell's older fbgrab utility.")
    (license license:gpl2)))

(define-public libcgroup
  (package
    (name "libcgroup")
    (version "0.41")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://sourceforge/libcg/" name "/"
             version "/" name "-" version ".tar.bz2"))
       (sha256
        (base32 "0lgvyq37gq84sk30sg18admxaj0j0p5dq3bl6g74a1ppgvf8pqz4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f))
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)))
    (inputs
     `(("linux-pam" ,linux-pam)))
    (home-page "https://sourceforge.net/projects/libcg/")
    (synopsis "Control groups management tools")
    (description "Control groups is Linux kernel method for process resource
restriction, permission handling and more.  This package provides userspace
interface to this kernel feature.")
    (license license:lgpl2.1)))

(define-public mbpfan
  (package
    (name "mbpfan")
    (version "2.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dgraziotin/mbpfan.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0aijyxrqh01x0s80yr4cgxgd001iiqqph65pxvby7f0wz8lnxnqj"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ; tests ask to be run as root
       #:make-flags (let ((out (assoc-ref %outputs "out")))
                      (list (string-append "DESTDIR=" out)
                            "CC=gcc"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-paths
           (lambda _
             (substitute* "Makefile"
               (("/usr") ""))
             #t))
         (delete 'configure))))         ; there's no configure phase
    (home-page "https://github.com/dgraziotin/mbpfan")
    (synopsis "Control fan speed on Macbooks")
    (description
     "mbpfan is a fan control daemon for Apple Macbooks.  It uses input from
the @code{coretemp} module and sets the fan speed using the @code{applesmc}
module.  It can be executed as a daemon or in the foreground with root
privileges.")
    (license license:gpl3+)))

(define-public psm2
  (package
    (name "psm2")
    (version "10.3-46")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/intel/opa-psm2.git")
                    (commit (string-append "PSM2_" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0wadphv4rl5p38x6a3dgpbijlzqdvcn02cfafnp72nh9faz0zvlx"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags
       `(,(string-append "LDFLAGS=-Wl,-rpath=" %output "/lib"))
       #:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-after 'unpack 'patch-Makefiles
                    (lambda _
                      (substitute* "Makefile"
                        (("/lib64") "/lib")
                        (("/usr") ""))
                      (substitute* "compat/Makefile"
                        (("/lib64") "/lib")
                        (("/usr") ""))
                      #t))
                  (replace 'install
                    (lambda _
                      (setenv "DESTDIR" %output)
                      (invoke "make" "install")
                      #t)))))
    (inputs
     `(("rdma-core" ,rdma-core)
       ("numactl" ,numactl)))
    (synopsis "Intel Performance Scaled Messaging 2 (PSM2) library")
    (description
     "This package is low-level user-level Intel's communications interface.
The PSM2 API is a high-performance vendor-specific protocol that provides a
low-level communications interface for the Intel Omni-Path family of
high-speed networking devices.")
    (home-page "https://github.com/intel/opa-psm2")
    ;; Only the x86_64 architecure is supported.
    (supported-systems '("x86_64-linux"))
    (license (list license:bsd-3 license:gpl2)))) ; dual

(define-public libpfm4
  (package
    (name "libpfm4")
    (version "4.9.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/perfmon2/"
                                  name "/libpfm-" version ".tar.gz"))
              (sha256
               (base32
                "1qp4g4n6dw42p2w5rkwzdb7ynk8h7g5vg01ybpmvxncgwa7bw3yv"))))
    (build-system gnu-build-system)
    (arguments
     '(#:modules ((guix build utils)
                  (guix build gnu-build-system))
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (delete 'check)
                  (replace 'build
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out")))
                        (setenv "CC" "gcc")
                        (invoke "make")
                        #t)))
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out")))
                        (invoke "make"
                                (string-append "PREFIX=" out)
                                "install")
                        #t))))))
    (synopsis "Performance event monitoring library")
    (description
     "This package provides a library called libpfm4, which is used to develop
monitoring tools exploiting the performance monitoring events such as those
provided by the Performance Monitoring Unit (PMU) of modern processors.

Libpfm4 helps convert from an event name, expressed as a string, to the event
encoding that is either the raw event as documented by the hardware vendor or
the OS-specific encoding.  In the latter case, the library is able to prepare
the OS-specific data structures needed by the kernel to setup the event.

libpfm4 provides support for the @code{perf_events} interface, which was
introduced in Linux 2.6.31.")
    (home-page "http://perfmon2.sourceforge.net/")
    (license license:expat)))

(define-public libnfnetlink
  (package
    (name "libnfnetlink")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.netfilter.org/projects/libnfnetlink/files/"
                    "libnfnetlink-" version ".tar.bz2"))
              (sha256
               (base32
                "06mm2x4b01k3m7wnrxblk9j0mybyr4pfz28ml7944xhjx6fy2w7j"))))
    (build-system gnu-build-system)
    (home-page "https://www.netfilter.org/projects/libnfnetlink/")
    (synopsis "Low-level netfilter netlink communication library")
    (description
     "@code{libnfnetlink} is the low-level library for netfilter related
kernel/userspace communication.  It provides a generic messaging
infrastructure for in-kernel netfilter subsystems (such as nfnetlink_log,
nfnetlink_queue, nfnetlink_conntrack) and their respective users and/or
management tools in userspace.")
    (license license:gpl2)))

(define-public go-netlink
  (package
    (name "go-netlink")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/vishvananda/netlink.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0hpzghf1a4cwawzhkiwdzin80h6hd09fskl77d5ppgc084yvj8x0"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/vishvananda/netlink"))
    (native-inputs
     `(("go-golang-org-x-sys-unix" ,go-golang-org-x-sys-unix)
       ("go-netns" ,go-netns)))
    (home-page "https://github.com/vishvananda/netlink")
    (synopsis "Simple netlink library for Go")
    (description "The netlink package provides a simple netlink library for
Go.  Netlink is the interface a user-space program in Linux uses to
communicate with the kernel.  It can be used to add and remove interfaces, set
IP addresses and routes, and configure IPsec.")
    (license license:asl2.0)))

(define-public xfsprogs
  (package
    (name "xfsprogs")
    (version "4.20.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kernel.org/linux/utils/fs/xfs/xfsprogs/"
                    "xfsprogs-" version ".tar.gz"))
              (sha256
               (base32
                "0ss0r6jlxxinf9fhpc0fgf7b89n9mzirpa85xxjmi1ix9l6cls6x"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f    ;kernel/user integration tests are in package "xfstests"
       #:phases (modify-phases %standard-phases
                  (add-after 'install 'install-headers
                    (lambda _
                      (invoke "make" "install-dev"))))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("util-linux" ,util-linux)))
    (home-page "https://xfs.wiki.kernel.org/")
    (synopsis "XFS file system tools")
    (description "This package provides commands to create and check XFS
file systems.")
    ;; The library "libhandle" and the headers in "xfslibs-dev" are
    ;; licensed under lgpl2.1. the other stuff is licensed under gpl2.
    (license (list license:gpl2 license:lgpl2.1))))

(define-public genext2fs
  (package
    (name "genext2fs")
    (version "1.4.1-4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jeremie-koenig/genext2fs.git")
                    ;; 1.4.1-3 had a VCS tag but 1.4.1-4 doesn't.
                    (commit "9ee43894634998b0b2b309d636f25c64314c9421")))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0ib5icn78ciz00zhc1bgdlrwaxvsdz7wnplwblng0jirwi9ml7sq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'apply-debian-patches
           ;; Debian changes (the revision after ‘-’ in VERSION) are
           ;; maintained as separate patches.  Apply those relevant to us.
           (lambda _
             (for-each
              (lambda (file-name)
                (invoke "patch" "-p1" "-i"
                        (string-append "debian/patches/" file-name)))
              (list "blocksize+creator.diff" ; add -B/-o options
                    "byteswap_fix.diff"))
             #t)))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)))
    (home-page "https://github.com/jeremie-koenig/genext2fs")
    (synopsis "Generate ext2 file system as a normal user")
    (description "This package provides a program to generate an ext2
file system as a normal (non-root) user.  It does not require you to mount
the image file to copy files on it, nor does it require that you become
the superuser to make device nodes.")
    (license license:gpl2)))

(define-public fakeroot
  (package
    (name "fakeroot")
    (version "1.23")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://ftp.debian.org/debian/pool/main/f/"
                                  "fakeroot/fakeroot_" version ".orig.tar.xz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1xpl0s2yjyjwlf832b6kbkaa5921liybaar13k7n45ckd9lxd700"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
        (add-after 'configure 'patch-Makefile
          (lambda _
            ;; Note: The root of the problem is already in "Makefile.am".
            (substitute* "Makefile"
             (("/bin/sh") (which "sh")))
            #t))
        (add-after 'unpack 'patch-getopt
          (lambda*  (#:key inputs #:allow-other-keys)
            (substitute* "scripts/fakeroot.in"
             (("getopt")
              (string-append (assoc-ref inputs "util-linux")
                             "/bin/getopt")))
            #t))
        (add-before 'configure 'setenv
          (lambda _
            (setenv "LIBS" "-lacl")
            #t))
        (add-before 'check 'prepare-check
          (lambda _
            (setenv "SHELL" (which "bash"))
            (setenv "VERBOSE" "1")
            (substitute* "test/t.touchinstall"
             ;; We don't have the name of the root user, so use ID=0.
             (("grep root") "grep \"\\<0\\>\""))
            (substitute* "test/tartest"
             ;; We don't have the name of the root group, so use ID=0.
             (("ROOTGROUP=root") "ROOTGROUP=0")
             ;; We don't have the name of the daemon user, so use IDs.
             (("daemon:sys") "1:3")
             (("daemon:") "1:"))
            ;; We don't have an /etc/passwd entry for "root" - use numeric IDs.
            (substitute* "test/compare-tar"
             (("tar -tvf") "tar --numeric-owner -tvf"))
            #t)))))
    (native-inputs
     `(("acl" ,acl)
       ("sharutils" ,sharutils) ; for the tests
       ("xz" ,xz))) ; for the tests
    (inputs
     `(("libcap" ,libcap)
       ("util-linux" ,util-linux)))
    (synopsis "Provides a fake root environment")
    (description "@command{fakeroot} runs a command in an environment where
it appears to have root privileges for file manipulation. This is useful
for allowing users to create archives (tar, ar, .deb etc.) with files in
them with root permissions/ownership. Without fakeroot one would have to
have root privileges to create the constituent files of the archives with
the correct permissions and ownership, and then pack them up, or one would
have to construct the archives directly, without using the archiver.")
    (home-page "http://freshmeat.sourceforge.net/projects/fakeroot")
    (license license:gpl3+)))

(define-public inputattach
  (package
    (name "inputattach")
    (version "0.42.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/linuxwacom/input-wacom.git")
                    (commit (string-append "input-wacom-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "04lnn7v0rm4ppbya140im5d4igcl6c1nrqpgbsr0i8wkral0nv7j"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'bootstrap)
         (delete 'configure)
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "inputattach"
               (invoke (string-append (assoc-ref inputs "gcc")
                                      "/bin/gcc")
                       "-O2" "-o" "inputattach" "inputattach.c"))
             #t))
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((target-dir (string-append
                                (assoc-ref outputs "out")
                                "/bin/")))
               (mkdir-p target-dir)
               (copy-file "inputattach/inputattach"
                          (string-append target-dir
                                         "inputattach"))
               #t))))))
    (home-page "https://linuxwacom.github.io/")
    (synopsis "Dispatch input peripherals events to a device file")
    (description "inputattach dispatches input events from several device
types and interfaces and translates so that the X server can use them.")
    (license license:gpl2+)))
