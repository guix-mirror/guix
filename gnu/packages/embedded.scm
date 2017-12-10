;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017 Theodoros Foradis <theodoros@foradis.org>
;;; Copyright © 2016 David Craven <david@craven.ch>
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

(define-module (gnu packages embedded)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix svn-download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix build utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module ((gnu packages base) #:prefix base:)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages dejagnu)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages libftdi)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages texinfo)
  #:use-module (srfi srfi-1))

;; We must not use the released GCC sources here, because the cross-compiler
;; does not produce working binaries.  Instead we take the very same SVN
;; revision from the branch that is used for a release of the "GCC ARM
;; embedded" project on launchpad.
;; See https://launchpadlibrarian.net/218827644/release.txt
(define-public gcc-arm-none-eabi-4.9
  (let ((xgcc (cross-gcc "arm-none-eabi"
                         #:xgcc gcc-4.9
                         #:xbinutils (cross-binutils "arm-none-eabi")))
        (revision "1")
        (svn-revision 227977))
    (package (inherit xgcc)
      (version (string-append (package-version xgcc) "-"
                              revision "." (number->string svn-revision)))
      (source
       (origin
         (method svn-fetch)
         (uri (svn-reference
               (url "svn://gcc.gnu.org/svn/gcc/branches/ARM/embedded-4_9-branch/")
               (revision svn-revision)))
         (file-name (string-append "gcc-arm-embedded-" version "-checkout"))
         (sha256
          (base32
           "113r98kygy8rrjfv2pd3z6zlfzbj543pq7xyq8bgh72c608mmsbr"))

         ;; Remove the one patch that doesn't apply to this 4.9 snapshot (the
         ;; patch is for 4.9.4 and later but this svn snapshot is older).
         (patches (remove (lambda (patch)
                            (string=? (basename patch)
                                      "gcc-arm-bug-71399.patch"))
                          (origin-patches (package-source xgcc))))))
      (native-inputs
       `(("flex" ,flex)
         ,@(package-native-inputs xgcc)))
      (arguments
       (substitute-keyword-arguments (package-arguments xgcc)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'unpack 'fix-genmultilib
               (lambda _
                 (substitute* "gcc/genmultilib"
                   (("#!/bin/sh") (string-append "#!" (which "sh"))))
                 #t))))
         ((#:configure-flags flags)
          ;; The configure flags are largely identical to the flags used by the
          ;; "GCC ARM embedded" project.
          `(append (list "--enable-multilib"
                         "--with-newlib"
                         "--with-multilib-list=armv6-m,armv7-m,armv7e-m"
                         "--with-host-libstdcxx=-static-libgcc -Wl,-Bstatic,-lstdc++,-Bdynamic -lm"
                         "--enable-plugins"
                         "--disable-decimal-float"
                         "--disable-libffi"
                         "--disable-libgomp"
                         "--disable-libmudflap"
                         "--disable-libquadmath"
                         "--disable-libssp"
                         "--disable-libstdcxx-pch"
                         "--disable-nls"
                         "--disable-shared"
                         "--disable-threads"
                         "--disable-tls")
                   (delete "--disable-multilib" ,flags)))))
      (native-search-paths
       (list (search-path-specification
              (variable "CROSS_C_INCLUDE_PATH")
              (files '("arm-none-eabi/include")))
             (search-path-specification
              (variable "CROSS_CPLUS_INCLUDE_PATH")
              (files '("arm-none-eabi/include")))
             (search-path-specification
              (variable "CROSS_LIBRARY_PATH")
              (files '("arm-none-eabi/lib"))))))))

(define-public gcc-arm-none-eabi-6
  (package
    (inherit gcc-arm-none-eabi-4.9)
    (version (package-version gcc-6))
    (source (origin (inherit (package-source gcc-6))
                    (patches
                     (append
                      (origin-patches (package-source gcc-6))
                      (search-patches "gcc-6-cross-environment-variables.patch"
                                      "gcc-6-arm-none-eabi-multilib.patch")))))))

(define-public newlib-arm-none-eabi
  (package
    (name "newlib")
    (version "2.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "ftp://sourceware.org/pub/newlib/newlib-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "01i7qllwicf05vsvh39qj7qp5fdifpvvky0x95hjq39mbqiksnsl"))))
    (build-system gnu-build-system)
    (arguments
     `(#:out-of-source? #t
       ;; The configure flags are identical to the flags used by the "GCC ARM
       ;; embedded" project.
       #:configure-flags '("--target=arm-none-eabi"
                           "--enable-newlib-io-long-long"
                           "--enable-newlib-register-fini"
                           "--disable-newlib-supplied-syscalls"
                           "--disable-nls")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-references-to-/bin/sh
           (lambda _
             (substitute* '("libgloss/arm/cpu-init/Makefile.in"
                            "libgloss/arm/Makefile.in"
                            "libgloss/libnosys/Makefile.in"
                            "libgloss/Makefile.in")
               (("/bin/sh") (which "sh")))
             #t)))))
    (native-inputs
     `(("xbinutils" ,(cross-binutils "arm-none-eabi"))
       ("xgcc" ,gcc-arm-none-eabi-4.9)
       ("texinfo" ,texinfo)))
    (home-page "http://www.sourceware.org/newlib/")
    (synopsis "C library for use on embedded systems")
    (description "Newlib is a C library intended for use on embedded
systems.  It is a conglomeration of several library parts that are easily
usable on embedded products.")
    (license (license:non-copyleft
              "https://www.sourceware.org/newlib/COPYING.NEWLIB"))))

(define-public newlib-nano-arm-none-eabi
  (package (inherit newlib-arm-none-eabi)
    (name "newlib-nano")
    (arguments
     (substitute-keyword-arguments (package-arguments newlib-arm-none-eabi)
       ;; The configure flags are identical to the flags used by the "GCC ARM
       ;; embedded" project.  They optimize newlib for use on small embedded
       ;; systems with limited memory.
       ((#:configure-flags flags)
        ''("--target=arm-none-eabi"
           "--enable-multilib"
           "--disable-newlib-supplied-syscalls"
           "--enable-newlib-reent-small"
           "--disable-newlib-fvwrite-in-streamio"
           "--disable-newlib-fseek-optimization"
           "--disable-newlib-wide-orient"
           "--enable-newlib-nano-malloc"
           "--disable-newlib-unbuf-stream-opt"
           "--enable-lite-exit"
           "--enable-newlib-global-atexit"
           "--enable-newlib-nano-formatted-io"
           "--disable-nls"))))
    (synopsis "Newlib variant for small systems with limited memory")))

(define (make-libstdc++-arm-none-eabi xgcc newlib)
  (let ((libstdc++ (make-libstdc++ xgcc)))
    (package (inherit libstdc++)
      (name "libstdc++-arm-none-eabi")
      (arguments
       (substitute-keyword-arguments (package-arguments libstdc++)
         ((#:configure-flags flags)
          ``("--target=arm-none-eabi"
             "--host=arm-none-eabi"
             "--disable-libstdcxx-pch"
             "--enable-multilib"
             "--with-multilib-list=armv6-m,armv7-m,armv7e-m"
             "--disable-shared"
             "--disable-tls"
             "--disable-plugin"
             "--with-newlib"
             ,(string-append "--with-gxx-include-dir="
                             (assoc-ref %outputs "out")
                             "/arm-none-eabi/include")))))
      (native-inputs
       `(("newlib" ,newlib)
         ("xgcc" ,xgcc)
         ,@(package-native-inputs libstdc++))))))

(define (arm-none-eabi-toolchain xgcc newlib)
  "Produce a cross-compiler toolchain package with the compiler XGCC and the C
library variant NEWLIB."
  (let ((newlib-with-xgcc (package (inherit newlib)
                            (native-inputs
                             (alist-replace "xgcc" (list xgcc)
                                            (package-native-inputs newlib))))))
    (package
      (name (string-append "arm-none-eabi"
                           (if (string=? (package-name newlib-with-xgcc)
                                         "newlib-nano")
                               "-nano" "")
                           "-toolchain"))
      (version (package-version xgcc))
      (source #f)
      (build-system trivial-build-system)
      (arguments
       '(#:modules ((guix build union))
         #:builder
         (begin
           (use-modules (ice-9 match)
                        (guix build union))
           (match %build-inputs
             (((names . directories) ...)
              (union-build (assoc-ref %outputs "out")
                           directories))))))
      (propagated-inputs
       `(("binutils" ,(cross-binutils "arm-none-eabi"))
         ("libstdc++" ,(make-libstdc++-arm-none-eabi xgcc newlib-with-xgcc))
         ("gcc" ,xgcc)
         ("newlib" ,newlib-with-xgcc)))
      (synopsis "Complete GCC tool chain for ARM bare metal development")
      (description "This package provides a complete GCC tool chain for ARM
bare metal development.  This includes the GCC arm-none-eabi cross compiler
and newlib (or newlib-nano) as the C library.  The supported programming
languages are C and C++.")
      (home-page (package-home-page xgcc))
      (license (package-license xgcc)))))

(define-public arm-none-eabi-toolchain-4.9
  (arm-none-eabi-toolchain gcc-arm-none-eabi-4.9
                           newlib-arm-none-eabi))

(define-public arm-none-eabi-nano-toolchain-4.9
  (arm-none-eabi-toolchain gcc-arm-none-eabi-4.9
                           newlib-nano-arm-none-eabi))

(define-public arm-none-eabi-toolchain-6
  (arm-none-eabi-toolchain gcc-arm-none-eabi-6
                           newlib-arm-none-eabi))

(define-public arm-none-eabi-nano-toolchain-6
  (arm-none-eabi-toolchain gcc-arm-none-eabi-6
                           newlib-nano-arm-none-eabi))

(define-public gdb-arm-none-eabi
  (package
    (inherit gdb)
    (name "gdb-arm-none-eabi")
    (arguments
     `(#:configure-flags '("--target=arm-none-eabi"
                           "--enable-multilib"
                           "--enable-interwork"
                           "--enable-languages=c,c++"
                           "--disable-nls")
     ,@(package-arguments gdb)))))

(define-public libjaylink
  ;; No release tarballs available.
  (let ((commit "699b7001d34a79c8e7064503dde1bede786fd7f0")
        (revision "2"))
    (package
      (name "libjaylink")
      (version (string-append "0.1.0-" revision "."
                              (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.zapb.de/libjaylink.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "034872d44myycnzn67v5b8ixrgmg8sk32aqalvm5x7108w2byww1"))))
      (build-system gnu-build-system)
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("libtool" ,libtool)
         ("pkg-config" ,pkg-config)))
      (inputs
       `(("libusb" ,libusb)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'autoreconf
             (lambda _
               (zero? (system* "autoreconf" "-vfi")))))))
      (home-page "http://repo.or.cz/w/libjaylink.git")
      (synopsis "Library to interface Segger J-Link devices")
      (description "libjaylink is a shared library written in C to access
SEGGER J-Link and compatible devices.")
      (license license:gpl2+))))

(define-public jimtcl
  (package
    (name "jimtcl")
    (version "0.77")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/msteveb/jimtcl"
                    "/archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1cmk3qscqckg70chjyimzxa2qcka4qac0j4wq908kiijp45cax08"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Doesn't use autoconf.
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (zero? (system* "./configure"
                               (string-append "--prefix=" out)))))))))
    (home-page "http://jim.tcl.tk")
    (synopsis "Small footprint Tcl implementation")
    (description "Jim is a small footprint implementation of the Tcl programming
language.")
    (license license:bsd-2)))

(define-public openocd
  (package
    (name "openocd")
    (version "0.10.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/openocd/openocd/"
                                  version "/openocd-" version ".tar.gz"))
              (sha256
               (base32
                "09p57y3c2spqx4vjjlz1ljm1lcd0j9q8g76ywxqgn3yc34wv18zd"))
              ;; FIXME: Remove after nrf52 patch is merged.
              (patches
               (search-patches "openocd-nrf52.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("hidapi" ,hidapi)
       ("jimtcl" ,jimtcl)
       ("libftdi" ,libftdi)
       ("libjaylink" ,libjaylink)
       ("libusb-compat" ,libusb-compat)))
    (arguments
     '(#:configure-flags
       (append (list "--disable-werror"
                     "--enable-sysfsgpio"
                     "--disable-internal-jimtcl"
                     "--disable-internal-libjaylink")
               (map (lambda (programmer)
                      (string-append "--enable-" programmer))
                    '("amtjtagaccel" "armjtagew" "buspirate" "ftdi"
                      "gw16012" "jlink" "opendous" "osbdm"
                      "parport" "aice" "cmsis-dap" "dummy" "jtag_vpi"
                      "remote-bitbang" "rlink" "stlink" "ti-icdi" "ulink"
                      "usbprog" "vsllink" "usb-blaster-2" "usb_blaster"
                      "presto" "openjtag")))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'autoreconf
           (lambda _
             (zero? (system* "autoreconf" "-vfi"))))
         (add-after 'autoreconf 'change-udev-group
           (lambda _
             (substitute* "contrib/60-openocd.rules"
               (("plugdev") "dialout"))
             #t))
         (add-after 'install 'install-udev-rules
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "contrib/60-openocd.rules"
                           (string-append
                            (assoc-ref outputs "out")
                            "/lib/udev/rules.d/")))))))
    (home-page "http://openocd.org")
    (synopsis "On-Chip Debugger")
    (description "OpenOCD provides on-chip programming and debugging support
with a layered architecture of JTAG interface and TAP support.")
    (license license:gpl2+)))

;; The commits for all propeller tools are the stable versions published at
;; https://github.com/propellerinc/propgcc in the release_1_0.  According to
;; personal correspondence with the developers in July 2017, more recent
;; versions are currently incompatible with the "Simple Libraries".

(define propeller-binutils
  (let ((xbinutils (cross-binutils "propeller-elf"))
        (commit "4c46ecbe79ffbecd2ce918497ace5b956736b5a3")
        (revision "2"))
    (package
      (inherit xbinutils)
      (name "propeller-binutils")
      (version (string-append "0.0.0-" revision "." (string-take commit 9)))
      (source (origin (inherit (package-source xbinutils))
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/parallaxinc/propgcc.git")
                      (commit commit)))
                (file-name (string-append name "-" commit "-checkout"))
                (sha256
                 (base32
                  "0w0dff3s7wv2d9m78a4jhckiik58q38wx6wpbba5hzbs4yxz35ck"))
                (patch-flags (list "-p1" "--directory=binutils"))))
      (arguments
       `(;; FIXME: For some reason there are many test failures.  It's not
         ;; obvious how to fix the failures.
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _ (chdir "binutils") #t)))
         ,@(substitute-keyword-arguments (package-arguments xbinutils)
            ((#:configure-flags flags)
             `(cons "--disable-werror" ,flags)))))
      (native-inputs
       `(("bison" ,bison)
         ("flex" ,flex)
         ("texinfo" ,texinfo)
         ("dejagnu" ,dejagnu)
         ,@(package-native-inputs xbinutils))))))

(define-public propeller-gcc-6
  (let ((xgcc (cross-gcc "propeller-elf"
                         #:xbinutils propeller-binutils))
        (commit "b4f45a4725e0b6d0af59e594c4e3e35ca4105867")
        (revision "1"))
    (package (inherit xgcc)
      (name "propeller-gcc")
      (version (string-append "6.0.0-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/totalspectrum/gcc-propeller.git")
                      (commit commit)))
                (file-name (string-append name "-" commit "-checkout"))
                (sha256
                 (base32
                  "0d9kdxm2fzanjqa7q5850kzbsfl0fqyaahxn74h6nkxxacwa11zb"))
                (patches
                 (append
                  (origin-patches (package-source gcc-6))
                  (search-patches "gcc-cross-environment-variables.patch")))))
      (native-inputs
       `(("flex" ,flex)
         ,@(package-native-inputs xgcc)))
      ;; All headers and cross libraries of the propeller toolchain are
      ;; installed under the "propeller-elf" prefix.
      (native-search-paths
       (list (search-path-specification
              (variable "CROSS_C_INCLUDE_PATH")
              (files '("propeller-elf/include")))
             (search-path-specification
              (variable "CROSS_LIBRARY_PATH")
              (files '("propeller-elf/lib")))))
      (home-page "https://github.com/totalspectrum/gcc-propeller")
      (synopsis "GCC for the Parallax Propeller"))))

(define-public propeller-gcc-4
  (let ((xgcc propeller-gcc-6)
        (commit "4c46ecbe79ffbecd2ce918497ace5b956736b5a3")
        (revision "2"))
    (package (inherit xgcc)
      (name "propeller-gcc")
      (version (string-append "4.6.1-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/parallaxinc/propgcc.git")
                      (commit commit)))
                (file-name (string-append name "-" commit "-checkout"))
                (sha256
                 (base32
                  "0w0dff3s7wv2d9m78a4jhckiik58q38wx6wpbba5hzbs4yxz35ck"))
                (patch-flags (list "-p1" "--directory=gcc"))
                (patches
                 (append
                  (origin-patches (package-source gcc-4.7))
                  (search-patches "gcc-4.6-gnu-inline.patch"
                                  "gcc-cross-environment-variables.patch")))))
      (arguments
       (substitute-keyword-arguments (package-arguments propeller-gcc-6)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'unpack 'chdir
               (lambda _ (chdir "gcc") #t))))))
      (home-page "https://github.com/parallaxinc/propgcc")
      (supported-systems (delete "aarch64-linux" %supported-systems)))))

;; Version 6 is experimental and may not work correctly.  This is why we
;; default to version 4, which is also used in the binary toolchain bundle
;; provided by Parallax Inc.
(define-public propeller-gcc propeller-gcc-4)


;; FIXME: We do not build the tiny library because that would require C++
;; headers, which are not available.  This may require adding a propeller-elf
;; variant of the libstdc++ package.
(define-public proplib
  (let ((commit "4c46ecbe79ffbecd2ce918497ace5b956736b5a3")
        (revision "2"))
    (package
      (name "proplib")
      (version (string-append "0.0.0-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/parallaxinc/propgcc.git")
                      (commit commit)))
                (file-name (string-append name "-" commit "-checkout"))
                (sha256
                 (base32
                  "0w0dff3s7wv2d9m78a4jhckiik58q38wx6wpbba5hzbs4yxz35ck"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; no tests
         #:make-flags
         (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
               (string-append "BUILD="  (getcwd) "/build"))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (add-after 'unpack 'chdir
             (lambda _ (chdir "lib") #t))
           (add-after 'chdir 'fix-Makefile
             (lambda _
               (substitute* "Makefile"
                 ;; Control the installation time of the headers.
                 ((" install-includes") ""))
               #t))
           ;; The Makefile does not separate building from installation, so we
           ;; have to create the target directories at build time.
           (add-before 'build 'create-target-directories
             (lambda* (#:key make-flags #:allow-other-keys)
               (zero? (apply system* "make" "install-dirs" make-flags))))
           (add-before 'build 'set-cross-environment-variables
             (lambda* (#:key outputs #:allow-other-keys)
               (setenv "CROSS_LIBRARY_PATH"
                       (string-append (assoc-ref outputs "out")
                                      "/propeller-elf/lib:"
                                      (or (getenv "CROSS_LIBRARY_PATH") "")))
               (setenv "CROSS_C_INCLUDE_PATH"
                       (string-append (assoc-ref outputs "out")
                                      "/propeller-elf/include:"
                                      (or (getenv "CROSS_C_INCLUDE_PATH") "")))
               #t))
           (add-before 'install 'install-includes
             (lambda* (#:key make-flags #:allow-other-keys)
               (zero? (apply system* "make" "install-includes" make-flags)))))))
      (native-inputs
       `(("propeller-gcc" ,propeller-gcc)
         ("propeller-binutils" ,propeller-binutils)
         ("perl" ,perl)))
      (home-page "https://github.com/parallaxinc/propgcc")
      (synopsis "C library for the Parallax Propeller")
      (description "This is a C library for the Parallax Propeller
micro-controller.")
      ;; Most of the code is released under the Expat license.  Some of the
      ;; included code is public domain and some changes are BSD licensed.
      (license license:expat))))

(define-public propeller-toolchain
  (package
    (name "propeller-toolchain")
    (version (package-version propeller-gcc))
    (source #f)
    (build-system trivial-build-system)
    (arguments '(#:builder (mkdir %output)))
    (propagated-inputs
     `(("binutils" ,propeller-binutils)
       ("libc" ,proplib)
       ("gcc" ,propeller-gcc)))
    (synopsis "Complete GCC tool chain for Propeller micro-controllers")
    (description "This package provides a complete GCC tool chain for
Propeller micro-controller development.")
    (home-page (package-home-page propeller-gcc))
    (license (package-license propeller-gcc))))

(define-public openspin
  (package
    (name "openspin")
    (version "1.00.78")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/parallaxinc/"
                                  "OpenSpin/archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1k2dbz1v604g4r2d9qhckg2m8dnhiya760mbsqfsg4waxal87yb7"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'remove-timestamp
           (lambda _
             (substitute* "SpinSource/openspin.cpp"
               ((" Compiled on.*$") "\\n\");"))
             #t))
         ;; Makefile does not include "install" target
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out")
                                       "/bin")))
               (mkdir-p bin)
               (install-file "build/openspin" bin)
               #t))))))
    (home-page "https://github.com/parallaxinc/OpenSpin")
    (synopsis "Spin/PASM compiler for the Parallax Propeller")
    (description "OpenSpin is a compiler for the Spin/PASM language of the
Parallax Propeller.  It was ported from Chip Gracey's original x86 assembler
code.")
    (license license:expat)))

(define-public propeller-load
  (let ((commit "4c46ecbe79ffbecd2ce918497ace5b956736b5a3")
        (revision "2"))
    (package
      (name "propeller-load")
      (version "3.4.0")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/parallaxinc/propgcc.git")
                      (commit commit)))
                (file-name (string-append name "-" commit "-checkout"))
                (sha256
                 (base32
                  "0w0dff3s7wv2d9m78a4jhckiik58q38wx6wpbba5hzbs4yxz35ck"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; no tests
         #:make-flags
         (list "OS=linux"
               (string-append "TARGET=" (assoc-ref %outputs "out")))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _ (chdir "loader") #t))
           (delete 'configure))))
      (native-inputs
       `(("openspin" ,openspin)
         ("propeller-toolchain" ,propeller-toolchain)))
      (home-page "https://github.com/parallaxinc/propgcc")
      (synopsis "Loader for Parallax Propeller micro-controllers")
      (description "This package provides the tool @code{propeller-load} to
upload binaries to a Parallax Propeller micro-controller.")
      (license license:expat))))

(define-public spin2cpp
  (package
    (name "spin2cpp")
    (version "3.6.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/totalspectrum/spin2cpp/"
                                  "archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0v5vzh69bp1r2byrpz12rql1w24ff2v9msr31596zq6hd6n82lnh"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;; The tests assume that a micro-controller is connected.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'set-cross-environment-variables
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CROSS_LIBRARY_PATH"
                     (string-append (assoc-ref inputs "propeller-toolchain")
                                    "/propeller-elf/lib"))
             (setenv "CROSS_C_INCLUDE_PATH"
                     (string-append (assoc-ref inputs "propeller-toolchain")
                                    "/propeller-elf/include"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out")
                                       "/bin")))
               (for-each (lambda (file)
                           (install-file (string-append "build/" file)
                                         bin))
                         '("testlex" "spin2cpp" "fastspin")))
             #t)))))
    (native-inputs
     `(("bison" ,bison)
       ("propeller-load" ,propeller-load)
       ("propeller-toolchain" ,propeller-toolchain)))
    (home-page "https://github.com/totalspectrum/spin2cpp")
    (synopsis "Convert Spin code to C, C++, or PASM code")
    (description "This is a set of tools for converting the Spin language for
the Parallax Propeller micro-controller into C or C++ code, into PASM, or even
directly into an executable binary.  The binaries produced use LMM PASM, so
they are much faster than regular Spin bytecodes (but also quite a bit
larger).")
    (license license:expat)))

(define-public spinsim
  (let ((commit "66915a7ad1a3a2cf990a725bb341fab8d11eb620")
        (revision "1"))
    (package
      (name "spinsim")
      (version (string-append "0.75-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/parallaxinc/spinsim.git")
                      (commit commit)))
                (file-name (string-append name "-" commit "-checkout"))
                (sha256
                 (base32
                  "1n9kdhlxsdx7bz6c80w8dhi96zp633gd6qs0x9i4ii8qv4i7sj5k"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; no tests
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((bin (string-append (assoc-ref outputs "out")
                                         "/bin")))
                 (install-file "build/spinsim" bin))
               #t)))))
      (home-page "https://github.com/parallaxinc/spinsim")
      (synopsis "Spin simulator")
      (description "This package provides the tool @code{spinsim}, a simulator
and simple debugger for Spin programs written for a Parallax Propeller
micro-controller.  Spinsim supports execution from cog memory and hub
execution, but it does not support multi-tasking.  It supports about
two-thirds of the opcodes in the P2 instruction set.")
      (license license:expat))))

(define-public propeller-development-suite
  (package
    (name "propeller-development-suite")
    (version (package-version propeller-gcc))
    (source #f)
    (build-system trivial-build-system)
    (arguments '(#:builder (mkdir %output)))
    (propagated-inputs
     `(("toolchain" ,propeller-toolchain)
       ("openspin" ,openspin)
       ("propeller-load" ,propeller-load)
       ("spin2cpp" ,spin2cpp)
       ("spinsim" ,spinsim)))
    (synopsis "Complete development suite for Propeller micro-controllers")
    (description "This meta-package provides a complete environment for the
development with Parallax Propeller micro-controllers.  It includes the GCC
toolchain, the loader, the Openspin compiler, the Spin2cpp tool, and the Spin
simulator.")
    (home-page (package-home-page propeller-gcc))
    (license (package-license propeller-gcc))))

(define-public binutils-vc4
  (let ((commit "708acc851880dbeda1dd18aca4fd0a95b2573b36"))
    (package
      (name "binutils-vc4")
      (version (string-append "2.23.51-0." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/puppeh/binutils-vc4.git")
                       (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1kdrz6fki55lm15rwwamn74fnqpy0zlafsida2zymk76n3656c63"))))
      (build-system gnu-build-system)
      (arguments
       `(#:configure-flags '("--target=vc4-elf"
                             "--disable-werror"
                             "--enable-cgen-maint")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'unpack-cgen
             (lambda* (#:key inputs #:allow-other-keys)
               (copy-recursively (string-append (assoc-ref inputs "cgen")
                                                "/cgen") "cgen")
               #t))
           (add-after 'unpack-cgen 'fix-cgen-guile
             (lambda _
               (substitute* "opcodes/Makefile.in"
                 (("guile\\{,-\\}1.8") "guile"))
               (zero? (system* "which" "guile")))))))
      (native-inputs
       `(("cgen"
          ,(origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/puppeh/cgen.git")
                       (commit "d8e2a9eb70425f180fdd5bfd032884b0855f2032")))
                (sha256
                 (base32
                  "14b3h2ji740s8zq5vwm4qdcxs4aa4wxi6wb9di3bv1h39x14nyr9"))))
         ("texinfo" ,texinfo)
         ("flex" ,flex)
         ("bison" ,bison)
         ("guile-1.8" ,guile-1.8)
         ("which" ,base:which)))
      (synopsis "Binutils for VC4")
      (description "This package provides @code{binutils} for VideoCore IV,
the Raspberry Pi chip.")
      (license license:gpl3+)
      (home-page "https://github.com/puppeh/vc4-toolchain/"))))

(define-public gcc-vc4
  (let ((commit "165f6d0e11d2e76ee799533bb45bd5c92bf60dc2")
        (xgcc (cross-gcc "vc4-elf" #:xbinutils binutils-vc4)))
    (package (inherit xgcc)
      (name "gcc-vc4")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/puppeh/gcc-vc4.git")
                      (commit commit)))
                (file-name (string-append name
                                          "-"
                                          (package-version xgcc)
                                          "-checkout"))
                (sha256
                 (base32
                  "13h30qjcwnlz6lfma1d82nnvfmjnhh7abkagip4vly6vm5fpnvf2"))))
      (native-inputs
        `(("flex" ,flex)
          ,@(package-native-inputs xgcc)))
      (synopsis "GCC for VC4")
      (description "This package provides @code{gcc} for VideoCore IV,
the Raspberry Pi chip."))))

(define-public python2-libmpsse
  (package
    (name "python2-libmpsse")
    (version "1.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://storage.googleapis.com/"
                            "google-code-archive-downloads/v2/"
                            "code.google.com/libmpsse/"
                            "libmpsse-" version ".tar.gz"))
        (sha256
          (base32
            "0jq7nhqq3na8675jnpfcar3pd3dp3adhhc4lw900swkla01a1wh8"))))
    (build-system gnu-build-system)
    (inputs
     `(("libftdi" ,libftdi)
       ("python" ,python-2)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("swig" ,swig)
       ("which" ,base:which)))
    (arguments
     `(#:tests? #f ; No tests exist.
       #:make-flags
       (list (string-append "CFLAGS=-Wall -fPIC -fno-strict-aliasing -g -O2 "
                            "$(shell pkg-config --cflags libftdi1)"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-environment-up
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (chdir "src")
             (setenv "PYDEV" (string-append (assoc-ref inputs "python")
                             "/include/python2.7"))
             #t))
         (add-after 'unpack 'patch-global-variable
           (lambda _
             ;; fast_rw_buf was defined in a header file which was making
             ;; the build not reproducible.
             (substitute* "src/fast.c"
               (("^int fast_build_block_buffer") "

unsigned char fast_rw_buf[SPI_RW_SIZE + CMD_SIZE];
int fast_build_block_buffer"))
             (substitute* "src/mpsse.h"
               (("unsigned char fast_rw_buf.*") "
"))
             #t))
         (replace 'install
           (lambda* (#:key outputs make-flags #:allow-other-keys #:rest args)
             (let* ((out (assoc-ref outputs "out"))
                    (out-python (string-append out
                                               "/lib/python2.7/site-packages"))
                    (install (assoc-ref %standard-phases 'install)))
               (install #:make-flags (cons (string-append "PYLIB=" out-python)
                                           make-flags))))))))
    (home-page "https://code.google.com/archive/p/libmpsse/")
    (synopsis "Python library for MPSSE SPI I2C JTAG adapter by FTDI")
    (description "This package provides a library in order to support the
MPSSE (Multi-Protocol Synchronous Serial Engine) adapter by FTDI that can do
SPI, I2C, JTAG.")
    (license license:gpl2+)))

(define-public picprog
  (package
    (name "picprog")
    (version "1.9.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.iki.fi/hyvatti/pic/picprog-"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1r04hg1n3v2jf915qr05la3q9cxy7a5jnh9cc98j04lh6c9p4x85"))
              (patches (search-patches "picprog-non-intel-support.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; No tests exist.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-paths
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Makefile"
               (("/usr/local") (assoc-ref outputs "out"))
               ((" -o 0 -g 0 ") " ")
               (("testport") ""))
             #t))
         (add-before 'install 'mkdir
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/bin"))
               (mkdir-p (string-append out "/man/man1"))
               #t)))
         (delete 'configure))))
    (synopsis "Programs Microchip's PIC microcontrollers")
    (description "This program programs Microchip's PIC microcontrollers.")
    (home-page "http://hyvatti.iki.fi/~jaakko/pic/picprog.html")
    (license license:gpl3+)))
