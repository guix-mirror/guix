;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2020 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2021 Evgeny Pisemsky <evgeny@pisemsky.com>
;;; Copyright © 2021 Léo Le Bouter <lle-bout@zaclys.net>
;;; Copyright © 2021 Denis Carikli <GNUtoo@cyberdimension.org>
;;; Copyright © 2021 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2021 Raghav Gururajan <rg@raghavgururajan.name>
;;; Copyright © 2021 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021, 2022 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2022 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2022 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2022 Marius Bakke <marius@gnu.org>
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

(define-module (gnu packages hardware)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1))

;; This is a module for packages related to physical hardware that don't (yet)
;; have a more specific home like gps.scm, security-token.scm, &c.

(define-public hwinfo
  (package
    (name "hwinfo")
    (version "21.80")
    (home-page "https://github.com/openSUSE/hwinfo")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url home-page)
         (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07058vjqdcd3la8y4b92f7fvcqxvmw1p0q4lg5kcn85pvbbg52ag"))
       (modules
        '((guix build utils)))
       (snippet
        `(begin
           ;; Remove git2log program file.
           (delete-file "git2log")
           ;; Remove variables that depends on git2log.
           (substitute* "Makefile"
             (("GIT2LOG.*\\:=.*$") "")
             (("GITDEPS.*\\:=.*$") "")
             (("BRANCH.*\\:=.*$") ""))
           ;; Create version file.
           (call-with-output-file "VERSION"
             (lambda (port)
               (format port ,version)))))))
    (build-system gnu-build-system)
    (outputs '("out" "dev" "doc"))
    (arguments
     `(#:tests? #f                      ; no test-suite available
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (dev (assoc-ref outputs "dev"))
                    (doc (assoc-ref outputs "doc"))
                    (incl-dir (string-append dev "/include"))
                    (lib-dir (string-append dev "/lib"))
                    (sbin-dir (string-append out "/sbin"))
                    (share-dir (string-append out "/share"))
                    (doc-dir (string-append doc "/share/doc")))
               ;; Generate HTML documentation in the output "doc".
               (mkdir-p doc-dir)
               (substitute* "doc/libhd.doxy"
                 (("OUTPUT_DIRECTORY.*=.*libhd")
                  (string-append "OUTPUT_DIRECTORY = " doc-dir "/libhd")))
               ;; Correct values of the version and install directories.
               (substitute* "Makefile"
                 (("VERSION.*\\:=.*$")
                  (string-append "VERSION := " ,version "\n"))
                 (("LIBDIR.*\\?=.*$")
                  (string-append "LIBDIR ?= " lib-dir "\n"))
                 (("/usr/include") incl-dir)
                 (("/(usr|var)/(lib|lib64)") lib-dir)
                 (("/usr/sbin") sbin-dir)
                 (("/usr/share") share-dir)
                 (("\\$\\(DESTDIR\\)/sbin ") ""))
               ;; Add output "dev" to the run-path.
               (substitute* "Makefile.common"
                 (("-Lsrc")
                  (string-append "-Lsrc " "-Wl,-rpath=" lib-dir)))
               ;; Correct program name of the lexical analyzer.
               (substitute* "src/isdn/cdb/Makefile"
                 (("lex isdn_cdb.lex") "flex isdn_cdb.lex"))
               ;; Patch pkgconfig file to point to output "dev".
               (substitute* "hwinfo.pc.in"
                 (("/usr") dev)))))
         (delete 'configure)
         (replace 'build
           (lambda _
             (setenv "CC" ,(cc-for-target))
             (invoke "make" "shared")
             (invoke "make" "doc")))
         (add-after 'install 'install-manpages
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (man-dir (string-append out "/share/man"))
                    (man1-dir (string-append man-dir "/man1"))
                    (man8-dir (string-append man-dir "/man8")))
               (for-each
                (lambda (x) (install-file x man1-dir))
                (find-files "doc" "\\.1$"))
               (for-each
                (lambda (y) (install-file y man8-dir))
                (find-files "doc" "\\.8$"))))))))
    (native-inputs
     (list doxygen flex perl pkg-config))
    (inputs
     `(("libx86emu" ,libx86emu)
       ("util-linux:lib" ,util-linux "lib")))
    (synopsis "Hardware information tool")
    (description "HwInfo is used to probe for the hardware present in the system.
It can be used to generate a system overview log which can be later used for
support.")
    (license license:gpl2+)))

(define-public ddcutil
  (package
    (name "ddcutil")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.ddcutil.com/tarballs/"
                           "ddcutil-" version ".tar.gz"))
       (sha256
        (base32 "0fp7ffjn21p0bsc5b1ipf3dbpzwn9g6j5dpnwdnca052ifzk2w7i"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list eudev
           glib
           kmod
           i2c-tools
           libdrm ; enhanced diagnostics
           libusb ; support USB monitors
           libx11 ; enhanced diagnostics
           libxrandr
           zlib))
    (home-page "https://www.ddcutil.com/")
    (synopsis "Control external monitor settings")
    (description
     "ddcutil can query and modify most external monitors' settings, such as
brightness, colour levels, and input sources.  Generally speaking, any setting
that can be changed by pressing buttons on the monitor can be modified by
ddcutil.

ddcutil communicates directly with monitors implementing the Monitor Control
Command Set (@dfn{MCCS}).  It usually does so through the the Display Data
Channel Command Interface (@dfn{DDC/CI}) protocol on the I2C bus, but can also
communicate over USB as per the USB Monitor Control Class Specification.

One particular use case is in colour profile management.  Monitor calibration
is relative to the monitor colour settings currently in effect, e.g. red gain.
ddcutil allows colour-related settings to be saved at the time a monitor is
calibrated, and restored when the calibration is applied.")
    (license (list license:bsd-3        ; FindDDCUtil.cmake
                   license:gpl2+))))    ; everything else

(define-public ddcui
  (package
    (name "ddcui")
    (version "0.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rockowitz/ddcui")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0myma1zw6dlygv3xbin662d91zcnwss10syf12q2fppkrd8qdgqf"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f))                    ; No test suite
    (native-inputs
     (list pkg-config qttools))
    (inputs
     (list ddcutil glib qtbase-5))
    (home-page "https://www.ddcutil.com/")
    (synopsis "Graphical user interface for ddcutil")
    (description "ddcui is a graphical user interface for ddcutil, implemented
using Qt.  It provide a dynamic way to inspect and configure external monitors
through the Display Data Channel Command Interface (@dfn{DDC/CI}) protocol.")
    (license (list license:gpl2+))))

(define-public edid-decode
  (let ((commit "74b64180d67bb009d8d9ea1b6f18ad41aaa16396") ; 2020-04-22
        (revision "1"))
    (package
      (name "edid-decode")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (file-name (git-file-name name version))
         (uri (git-reference
               (url "git://linuxtv.org/edid-decode.git")
               (commit commit)))
         (sha256
          (base32 "0nirp5bza08zj5d8bjgcm0p869hdg3qg3mwa7999pjdrzmn7s2ah"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                     ; No test suite
         #:make-flags
         (list (string-append "DESTDIR=" (assoc-ref %outputs "out"))
               "bindir=/bin" "mandir=/share/man")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-cross-compilation
             (lambda* (#:key native-inputs target #:allow-other-keys)
               (when target
                 (substitute* "Makefile"
                   (("\\$\\(CXX\\)")
                    (string-append target "-g++"))))
               #t))
           (delete 'configure))))
      (home-page "https://git.linuxtv.org/edid-decode.git/")
      (synopsis "Decode @dfn{EDID} data in human-readable format")
      (description "edid-decode decodes @dfn{EDID} monitor description data in
human-readable format and checks if it conforms to the standards.")
      (license license:expat))))

(define-public h-client
  (let ((version "0.0a0")
        (revision 138))
    (package
      (name "h-client")
      (version (string-append version "-" (number->string revision)))
      (source
       (origin
         (method svn-fetch)
         (uri
          (svn-reference
           (url "https://svn.savannah.nongnu.org/svn/h-client/trunk/h-client")
           (revision revision)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1pdd2qhyaa5vh7z4rkpwjlby1flkwhzmp8zlglalx5y5sv95l4kp"))))
      (build-system python-build-system)
      (arguments
       `(#:python ,python-2
         ;; Tests depends on /etc/os-release which does not exist in the
         ;; build container.
         #:tests? #f))
      (inputs
       (list python-2 python2-pycurl python2-pygtk pciutils usbutils))
      (synopsis "Graphical client for the h-node hardware database
project")
      (description
       "The h-node project (https://www.h-node.org) aims to build a database of
hardware that works with fully free operating systems.
h-client is a GTK+ graphical client that is able to retrieves information on
the hardware inside the computer it's running on, and on peripherals connected
to it, and help you submit that information to the h-node project along with
whether the hardware works with a fully free operating system or not.")
      (home-page "https://savannah.nongnu.org/projects/h-client/")
      (license license:gpl3+))))

(define-public headsetcontrol
  (package
    (name "headsetcontrol")
    (version "2.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Sapd/HeadsetControl")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0a7zimzi71416pmn6z0l1dn1c2x8p702hkd0k6da9rsznff85a88"))))
    (build-system cmake-build-system)
    (inputs
     (list hidapi))
    (home-page "https://github.com/Sapd/HeadsetControl")
    (synopsis "Sidetone and Battery status for USB headsets")
    (description
     "Headsetcontrol is a tool to control certain aspects of USB-connected
headsets.  Currently, support is provided for adjusting sidetone, getting
battery state, controlling LEDs, and setting the inactive time.")
    (license license:gpl3+)))

(define-public hueplusplus
  (package
    (name "hueplusplus")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/enwi/hueplusplus")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jy8m2a0h0kf0aw8jbniz069q9j7cx67b1zlv2vz1ymq921qk0pm"))
       (patches
        (search-patches "hueplusplus-mbedtls.patch"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f)) ;; Tests require Google's gtest and gmock
    (inputs
     (list mbedtls-apache))
    (synopsis "C++ library to control Philips Hue lights")
    (description "Hueplusplus is a library for controlling Philips Hue lights.
Features:

@itemize
@item find bridges with SSDP or set an ip manually
@item all common light functions (brightness, color, temperature)
@item extended @code{alert()} functions, which alert in a specific
color (good for notifications)
@item supports sensors, rules, groups, scenes and schedules
@item streaming with entertainment mode
@item documented with doxygen
@end itemize")
    (home-page "https://github.com/enwi/hueplusplus")
    (license license:lgpl3+)))

(define-public i7z
  (let ((revision "0")
        (commit "1a41ff13db747e962456ddbb5ccb2b7fc43ca0cb"))
    (package
      (name "i7z")
      (version (git-version "0.28" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/afontenot/i7z")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0jxm63a8y1mfl1sa4mzzfs3bgnym6achj1yc0jglmp05xal16lm1"))
         (patches
          (search-patches "i7z-gcc-10.patch"))
         (modules '((guix build utils)))
         (snippet
          '(begin
             (for-each delete-file-recursively
                       (list "src/GUI"
                             "src/perfmon-i7z"
                             "scripts"))))))
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags
         (list (string-append "prefix=" (assoc-ref %outputs "out"))
               (string-append "CC=" ,(cc-for-target)))
         #:tests? #f                    ; no test suite
         #:phases
         (modify-phases %standard-phases
           (delete 'configure))))       ; no configure script
      (inputs
       (list ncurses))
      (home-page "https://github.com/afontenot/i7z")
      (synopsis "Thermal and C-state reporting on older Intel Core CPUs")
      (description
       "The @command{i7z} utility accurately measures the current frequency
and temperature of older Intel Core (i3, i5, and i7) processors including the
Nehalem, Sandy Bridge, and Ivy Bridge generations.  Reliable support for newer
CPUs is not guaranteed, as this package has not seen significant development
since 2013.

If your processor is supported, you'll get detailed reports on Turbo Boost and
clock multipliers, core voltage, and time spent in different C-states.  This
information can be viewed in real time and/or logged to a file.")
      (supported-systems (list "x86_64-linux"))
      (license license:gpl2))))

(define-public libsmbios
  (package
    (name "libsmbios")
    (version "2.4.3")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url (string-append "https://github.com/dell/" name))
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0krwwydyvb9224r884y1mlmzyxhlfrcqw73vi1j8787rl0gl5a2i"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext" ,gettext-minimal)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("perl" ,perl)
       ("python" ,python)))
    (inputs
     (list libxml2))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'bootstrap
           (lambda _ (invoke "autoreconf" "-vfi"))))))
    (synopsis "Library for interacting with Dell SMBIOS tables")
    (description
     "libsmbios provides a library to interface with the SMBIOS tables.  It
also provides extensions for proprietary methods of interfacing with Dell
specific SMBIOS tables.")
    (home-page "https://github.com/dell/libsmbios")
    (license
     (list license:osl2.1 license:gpl2+ license:bsd-3 license:boost1.0))))

;; Distinct from memtest86, which is obsolete.
(define-public memtest86+
  (package
    (name "memtest86+")
    ;; Update the description when/if UEFI support is released.
    (version "5.01")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.memtest.org/download/5.01/memtest86+-"
                           version ".tar.gz"))
       (sha256
        (base32 "0fch1l55753y6jkk0hj8f6vw4h1kinkn9ysp22dq5g9zjnvjf88l"))))
    (build-system gnu-build-system)
    (arguments
     `(#:system "i686-linux"            ; the result runs outside of any OS
       #:tests? #f                      ; no way to test this
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (replace 'build
           ;; The default 'make all' does wonderful things, like scp(1) a file to
           ;; 192.168.0.12. Build the bootable images and nothing more.
           (lambda _
             (invoke "make"
                     "memtest"          ; ELF executable
                     "memtest.bin")))   ; DOS/MBR boot sector
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib/memtest86+"))
                    (doc (string-append out "/share/doc/memtest86+-" ,version)))
               (for-each
                (lambda (file)
                  (install-file file lib))
                (list "memtest"
                      "memtest.bin"))
               (for-each
                (lambda (file)
                  (install-file file doc))
                (list "FAQ"
                      "README"))
               #t))))))
    (native-inputs
     ;; Newer GCCs fail with a deluge of "multiple definition of `__foo'" errors.
     (list gcc-4.9))
    (supported-systems (list "i686-linux" "x86_64-linux"))
    (home-page "https://www.memtest.org/")
    (synopsis "Thorough real-mode memory tester")
    (description
     "Memtest86+ is a thorough, stand-alone memory test for x86 systems.  It
repeatedly writes different patterns to all memory locations, reads them back
again, and verifies whether the result is the same as what was written.  This
can help debug even intermittent and non-deterministic errors.

It runs independently of any operating system, at computer boot-up, so that it
can scan as much of your RAM as possible for hardware defects.

Memtest86+ cannot currently be used on computers booted with UEFI.")
    (license license:gpl2)))

(define-public memtester
  (package
    (name "memtester")
    (version "4.5.1")
    (source
     (origin
       (method url-fetch)
       ;; Even the latest release is available under 'old-versions/'.
       (uri (string-append "http://pyropus.ca/software/memtester/old-versions/"
                           "memtester-" version ".tar.gz"))
       (sha256
        (base32 "0issrasdihav8jgsqb49cfyj0v564z8k9lyg2jrq9h3n4lwc4pqw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list ,(string-append "CC=" (cc-for-target)))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           ;; This is a home-brewed configuration system where the cc/ld command
           ;; lines are stored in one-line files.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (substitute* (list "conf-cc" "conf-ld")
                 (("^cc") "gcc"))
               (substitute* "Makefile"
                 (("(INSTALLPATH.*=).*" _ assignment)
                  (string-append assignment out)))
               #t)))
         (replace 'check
           ;; There is no test suite. Test some RAM for a single iteration.
           (lambda _
             (invoke "./memtester" "64K" "1"))))))
    (home-page "http://pyropus.ca/software/memtester/")
    (synopsis "User-space memory subsystem tester")
    (description
     "Memtester stress-tests the memory subsystem of your operating system and
computer.  It repeatedly writes different patterns to all memory locations,
reads them back again, and verifies whether the result is the same as what was
written.  This can help debug even intermittent and non-deterministic errors.

Memtester runs entirely in user space.  This means that you don't need to reboot
to test your memory, but also that it's not possible to test all of the RAM
installed in the system.

It can also be told to test memory starting at a particular physical address.")
    (license license:gpl2)))

(define-public msr-tools
  (package
    (name "msr-tools")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://01.org/sites/default/files/downloads/"
                           name "/" name "-" version ".zip"))
       (sha256
        (base32 "07hxmddg0l31kjfmaq84ni142lbbvgq6391r8bd79wpm819pnigr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "sbindir=" (assoc-ref %outputs "out") "/sbin"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-before 'install 'create-output-directory
           (lambda* (#:key outputs #:allow-other-keys)
             ;; 'make install' assumes that sbindir exists.
             (let* ((out  (assoc-ref outputs "out"))
                    (sbin (string-append out "/sbin")))
               (mkdir-p sbin)
               #t))))
       #:tests? #f))                    ; no test suite
    (native-inputs
     (list unzip))
    ;; These registers and the CPUID instruction only exist on (most) x86 chips.
    (supported-systems (list "i686-linux" "x86_64-linux"))
    (home-page "https://01.org/msr-tools/")
    (synopsis "Read and write Model-Specific Registers (@dfn{MSR})")
    (description
     "The MSR Tools project provides console utilities to directly access the
Model-Specific Registers (@dfn{MSR}s) and CPU ID of Intel-compatible processors:

@itemize
@item @command{cpuid}: show identification and feature information of any CPU
@item @command{rdmsr}: read MSRs from any CPU or all CPUs
@item @command{wrmsr}: write to MSRs on any CPU or all CPUs
@end itemize

These tools can be used to query and modify certain low-level CPU parameters,
such as the Turbo Boost ratio and Thermal Design Power (@dfn{TDP}) limits.

MSR addresses differ (greatly) between processors, and any such modification can
be dangerous and may void your CPU or system board's warranty.")
    (license license:gpl2)))     ; cpuid.c is gpl2, {rd,wr}msr.c are gpl2+

(define-public openhmd
  (package
    (name "openhmd")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/OpenHMD/OpenHMD")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hkpdl4zgycag5k8njvqpx01apxmm8m8pvhlsxgxpqiqy9a38ccg"))))
    (build-system meson-build-system)
    (arguments
     `(#:tests? #f)) ; no test target although there is a test folder
    (native-inputs
     (list pkg-config))
    (inputs
     (list hidapi))
    (home-page "http://www.openhmd.net/")
    (synopsis "API and drivers for immersive technology")
    (description "OpenHMD aims to provide an API and drivers for immersive
technology, such as head mounted displays with built in head tracking.")
    (license license:boost1.0)))

(define-public openrgb
  (package
    (name "openrgb")
    (version "0.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/CalcProgrammer1/OpenRGB")
             (commit (string-append "release_" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xhfaz0b74nfnh7il2cz5c0338xlzay00g6hc2h3lsncarj8d5n7"))
       (patches
        (search-patches "openrgb-unbundle-hueplusplus.patch"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Delete the bundled hueplusplus and json libraries.
           (delete-file-recursively "dependencies/hueplusplus-1.0.0")
           (delete-file-recursively "dependencies/json")))))
    (build-system cmake-build-system)
    (arguments
     (list
       #:tests? #f ; doesn't have tests
       #:make-flags
       #~(list (string-append "INSTALL_ROOT=" #$output ))
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'unbundle
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "OpenRGB.pro"
                 (("dependencies/hueplusplus-1.0.0/include/hueplusplus")
                  (string-append #$(this-package-input "hueplusplus")
                                 "/include/hueplusplus"))
                 (("dependencies/json")
                  (string-append #$(this-package-input "json-modern-cxx")
                                 "/include/nlohmann")))))
           ;; Call qmake instead of configure to create a Makefile.
           (replace 'configure
             (lambda _ (invoke "qmake" "PREFIX=/" "OpenRGB.pro"))))))
    (inputs
     (list hidapi
           hueplusplus
           json-modern-cxx
           libusb
           mbedtls-apache
           qtbase-5))
    (native-inputs
     (list pkg-config))
    (synopsis "RGB lighting control")
    (description
     "OpenRGB is lighting control that doesn't depend on manufacturer software.
ASUS, ASRock, Corsair, G.Skill, Gigabyte, HyperX, MSI, Razer, ThermalTake, and more
supported.

Features:

@itemize
@item Set colors and select effect modes for a wide variety of RGB hardware
@item Save and load profiles
@item Control lighting from third party software using the OpenRGB SDK
@item Command line interface
@item Connect multiple instances of OpenRGB to synchronize lighting across multiple PCs
@item Can operate standalone or in a client/headless server configuration
@item View device information
@item No official/manufacturer software required
@item Graphical view of device LEDs makes creating custom patterns easy
@end itemize")
    (home-page "https://openrgb.org/")
    (license license:gpl2))) ; Included libccmmk is lgpl3+, CRC is bsd-3

(define-public wavemon
  (package
    (name "wavemon")
    (version "0.9.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/uoaerg/wavemon")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0s3yz15vzx90fxyb8bgryksn0cr2gpz9inbcx4qjrgs7zfbm4pgh"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list "CC=gcc"
             ;; Makefile.in (ab)uses $(datadir) as $(docdir). Set it to Guix's
             ;; standard --docdir since it's only used as such.
             (string-append "datadir=" (assoc-ref %outputs "out")
                            "/share/doc/" ,name "-" ,version))
       #:tests? #f))                    ; no tests
    (native-inputs
     (list pkg-config))
    (inputs
     (list libcap libnl ncurses))
    (home-page "https://github.com/uoaerg/wavemon")
    (synopsis "Wireless network device monitor")
    (description
     "Wavemon is a wireless device monitor with an interactive ncurses terminal
interface.  It can display and plot signal and noise levels in real time.  It
also reports packet statistics, device configuration, network parameters, and
access points and other wireless clients of your wireless network hardware.

Wavemon should work (with varying levels of detail and features) with any device
supported by the Linux kernel.")
    ;; Source file headers still say GPL2+, but the authorial intent
    ;; (from COPYING and the F9 'about' screen) is clearly GPL3+.
    (license license:gpl3+)))

(define-public rkdeveloptool
  (let ((commit "6e92ebcf8b1812da02663494a68972f956e490d3")
        (revision "0"))
    (package
      (name "rkdeveloptool")
      (version (git-version "1.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rockchip-linux/rkdeveloptool")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0zwrkqfxd671iy69v3q0844gfdpm1yk51i9qh2rqc969bd8glxga"))))
      (build-system gnu-build-system)
      (native-inputs
       (list autoconf automake pkg-config))
      (inputs
       (list libusb))
      (home-page "https://github.com/rockchip-linux/rkdeveloptool")
      (synopsis "Read from and write to RockChicp devices over USB")
      (description
       "Rkdeveloptool can read from and write to RockChip devices over USB, such
as the Pinebook Pro.")
      (license license:gpl2+))))

(define-public libqb
  (package
    (name "libqb")
    ;; NOTE: We are using a Release Candidate version (for 2.0) here because
    ;; of the linker issues with the previous release.
    (version "1.9.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/ClusterLabs/libqb/releases/download/v"
                    version "/libqb-" version ".tar.xz"))
              (sha256
               (base32
                "008vvw504kh40br5v2xkqavnp9vpmjvf768faqzv1d00fd53ingn"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config libxml2))
    (home-page "https://clusterlabs.github.io/libqb/")
    (synopsis "Library providing high performance logging, tracing, ipc, and poll")
    (description "Libqb is a library with the primary purpose of providing
high-performance, reusable features for client-server architecture, such as
logging, tracing, inter-process communication (IPC), and polling.  Libqb is
not intended to be an all-encompassing library, but instead provide focused
APIs that are highly tuned for maximum performance for client-server
applications.")
    (license license:lgpl2.1)))

(define-public usbguard
  (package
    (name "usbguard")
    ;; Note: Use a recent snapshot to get compatibility with newer system
    ;; libraries.
    (version "1.0.0-55-g466f1f0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/USBGuard/usbguard")
                    (commit (string-append "usbguard-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0rc0213qsfap3sgx9m3m1kppxbjl2fdwmzlbn5rbmn1i33125dfi"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-bootstrap-script
           (lambda _
             ;; Don't attempt to fetch git submodules.
             (substitute* "autogen.sh"
               (("^git submodule.*")
                ""))))
         (add-after 'bootstrap 'patch-build-scripts
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "configure"
               (("/usr/include/catch")
                (dirname (search-input-file inputs "include/catch.hpp"))))
             ;; Do not create log directory.
             (substitute* "Makefile.in" ((".*/log/usbguard.*") ""))
             ;; Disable LDAP tests: they use 'sudo'.
             (substitute* "src/Tests/Makefile.in"
               (("\\$\\(am__append_2\\)") ""))))
         (add-after 'install 'delete-static-library
           (lambda* (#:key outputs #:allow-other-keys)
             ;; It can't be direclty disabled since it's needed for the tests.
             (delete-file (string-append (assoc-ref outputs "out")
                                         "/lib/libusbguard.a"))))
         (add-after 'install 'install-zsh-completion
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (site-functions
                     (string-append out "/share/zsh/site-functions")))
               (mkdir-p site-functions)
               (copy-file "scripts/usbguard-zsh-completion"
                          (string-append site-functions "/_usbguard"))))))
       #:make-flags
       (list (string-append "BASH_COMPLETION_DIR="
                            (assoc-ref %outputs "out")
                            "/etc/bash_completion.d"))
       #:configure-flags
       (list
        "--localstatedir=/var"
        "--enable-systemd=no"
        "--with-ldap"
        "--with-dbus"
        "--with-polkit")))
    (inputs
     (list audit
           catch-framework
           dbus-glib
           openldap
           libcap-ng
           libseccomp
           libsodium
           pegtl
           polkit
           protobuf
           libqb))
    (native-inputs
     (list asciidoc
           autoconf
           automake
           libtool
           bash-completion
           `(,glib "bin")
           umockdev
           libxml2
           libxslt
           pkg-config))
    (home-page "https://usbguard.github.io")
    (synopsis "Helps to protect your computer against rogue USB devices (a.k.a. BadUSB)")
    (description "USBGuard is a software framework for implementing USB device
authorization policies (what kind of USB devices are authorized) as well as
method of use policies (how a USB device may interact with the system).
Simply put, it is a USB device whitelisting tool.")
    (license license:gpl2)))

(define-public screentest
  (package
    (name "screentest")
    (version "2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/TobiX/screentest")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gv3xj9sbk1wsyijfw9xjnvy8pg7j4arjnma2r2kfi18qy32wd30"))))
    (build-system gnu-build-system)
    (inputs
     (list glib gtk+-2))
    (native-inputs
     (list autoconf
           intltool
           libtool
           `(,glib "bin")
           automake
           pkg-config))
    (synopsis "Simple screen testing tool")
    (description "This is a program for testing the quality of CRT/LCD
screens.  It displays various patterns and allows you to estimate the quality
of your CRT/LCD monitor.")
    (home-page "https://github.com/TobiX/screentest")
    (license license:gpl2)))

(define-public tpm2-tss
  (package
    (name "tpm2-tss")
    (version "3.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/tpm2-software/tpm2-tss"
                           "/releases/download/" version "/tpm2-tss-" version
                           ".tar.gz"))
       (sha256
        (base32 "05xynpwq851fp8f5fy7ac0blvz8mr5m5cbqj3gslgbwv63kjnfbq"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list curl json-c openssl))
    (home-page "https://tpm2-software.github.io/")
    (synopsis "OSS Implementation of the TCG TPM2 Software Stack (TSS2)")
    (description
     "This package provides the @acronym{TCG, Trusted Computing Group}
@acronym{TSS2, TPM2 Software Stack}.  The stack contains libtss2-fapi,
libtss2-esys, libtss2-sys, libtss2-mu, libtss2-tcti-device, libtss2-tcti-swtpm
and libtss2-tcti-mssim.")
    (license license:bsd-2)))

(define-public libcpuid
  ;; We need to remove blobs from the source, first we have to isolate the blob
  ;; source in build system.
  ;; See https://github.com/anrieff/libcpuid/pull/159.
  (let ((commit "2e61160983f32ba840b2246d3c3850c44626ab0d")
        (revision "1"))
    (package
      (name "libcpuid")
      (version (git-version "0.5.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/anrieff/libcpuid")
               (commit commit)))
         (sha256
          (base32 "1mphvkiqq6z33sq6i490fq27sbyylacwrf8bg7ccvpcjms208sww"))
         (modules '((guix build utils)))
         (snippet
          ;; Now remove blobs.
          #~(begin
              (delete-file "libcpuid/msrdriver.c")
              (delete-file-recursively "contrib/MSR Driver")))
         (file-name (git-file-name name version))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:configure-flags #~(list "-DLIBCPUID_TESTS=ON")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'absolutize
              (lambda* (#:key inputs #:allow-other-keys)
                ;; Linux specific
                (when #$(target-linux?)
                  (substitute* "libcpuid/rdmsr.c"
                    (("modprobe") (which "modprobe")))))))))
      (inputs
       (if (target-linux?)
           (list kmod)
           '()))
      (native-inputs (list python-3))   ;required by tests
      (supported-systems
       (filter (lambda (t) (or (target-x86-64? t) (target-x86-32? t)))
               %supported-systems))
      (home-page "https://libcpuid.sourceforge.net/")
      (synopsis "Small library for x86 CPU detection and feature extraction")
      (description "Libcpuid is a small C library to get vendor, model, branding
string, code name and other information from x86 CPU. This library is not to be
confused with the @code{cpuid} command line utility from package @code{cpuid}.")
      (license license:bsd-2))))
