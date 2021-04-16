;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2020 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2021 Evgeny Pisemsky <evgeny@pisemsky.com>
;;; Copyright © 2021 Léo Le Bouter <lle-bout@zaclys.net>
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
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

;; This is a module for packages related to physical hardware that don't (yet)
;; have a more specific home like gps.scm, security-token.scm, &c.

(define-public ddcutil
  (package
    (name "ddcutil")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.ddcutil.com/tarballs/"
                           "ddcutil-" version ".tar.gz"))
       (sha256
        (base32 "19kkwb9ijzn6ya3mvjanggh1c96fcc0lkbk7xnyi2qp6wsr4nhxp"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("eudev" ,eudev)
       ("glib" ,glib)
       ("libdrm" ,libdrm)               ; enhanced diagnostics
       ("libusb" ,libusb)               ; support USB monitors
       ("libx11" ,libx11)               ; enhanced diagnostics
       ("libxrandr" ,libxrandr)
       ("zlib" ,zlib)))
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
     `(("libxml2" ,libxml2)))
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
     `(("gcc" ,gcc-4.9)))
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
    (version "4.5.0")
    (source
     (origin
       (method url-fetch)
       ;; Even the latest release is available under 'old-versions/'.
       (uri (string-append "http://pyropus.ca/software/memtester/old-versions/"
                           "memtester-" version ".tar.gz"))
       (sha256
        (base32 "0dxfwayns3hjjplkxkpkm1409lmjlpi4chcrahcvdbnl0q6jpmcf"))))
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
     `(("unzip" ,unzip)))
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

(define-public wavemon
  (package
    (name "wavemon")
    (version "0.9.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/uoaerg/wavemon")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0m9n5asjxs1ir5rqprigqcrm976mgjvh4yql1jhfnbszwbf95193"))))
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
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libcap" ,libcap)
       ("libnl" ,libnl)
       ("ncurses" ,ncurses)))
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
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("pkg-config" ,pkg-config)))
      (inputs
       `(("libusb" ,libusb)))
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
     `(("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)))
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
    (version "0.7.8")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/USBGuard/usbguard/releases/download/usbguard-"
                    version "/usbguard-" version ".tar.gz"))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1il5immqfxh2cj8wn1bfk7l42inflzgjf07yqprpz7r3lalbxc25"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-build-scripts
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "configure"
               (("/usr/include/catch")
                (string-append (assoc-ref inputs "catch") "/include")))
             ;; Do not create log directory.
             (substitute* "Makefile.in" ((".*/log/usbguard.*") ""))
             ;; Disable LDAP tests: they use 'sudo'.
             (substitute* "src/Tests/Makefile.in"
               (("\\$\\(am__append_2\\)") ""))
             #t))
         (add-after 'install 'delete-static-library
           (lambda* (#:key outputs #:allow-other-keys)
             ;; It can't be direclty disabled since it's needed for the tests.
             (delete-file (string-append (assoc-ref outputs "out")
                                         "/lib/libusbguard.a"))
             #t))
         (add-after 'install 'install-zsh-completion
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (site-functions
                     (string-append out "/share/zsh/site-functions")))
               (mkdir-p site-functions)
               (copy-file "scripts/usbguard-zsh-completion"
                          (string-append site-functions "/_usbguard"))
               #t))))
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
     `(("audit" ,audit)
       ("catch" ,catch-framework)
       ("dbus-glib" ,dbus-glib)
       ("ldap" ,openldap)
       ("libcap-ng" ,libcap-ng)
       ("libseccomp" ,libseccomp)
       ("libsodium" ,libsodium)
       ("pegtl" ,pegtl)
       ("polkit" ,polkit)
       ("protobuf" ,protobuf)
       ("libqb" ,libqb)))
    (native-inputs
     `(("asciidoc" ,asciidoc)
       ("bash-completion" ,bash-completion)
       ("gdbus-codegen" ,glib "bin")
       ("umockdev" ,umockdev)
       ("xmllint" ,libxml2)
       ("xsltproc" ,libxslt)
       ("pkg-config" ,pkg-config)))
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
     `(("glib" ,glib)
       ("gtk+" ,gtk+-2)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("intltool" ,intltool)
       ("libtool" ,libtool)
       ("glib" ,glib "bin")
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)))
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
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("curl" ,curl)
       ("json-c" ,json-c)
       ("openssl" ,openssl)))
    (home-page "https://tpm2-software.github.io/")
    (synopsis "OSS Implementation of the TCG TPM2 Software Stack (TSS2)")
    (description
     "This package provides the @acronym{TCG, Trusted Computing Group}
@acronym{TSS2, TPM2 Software Stack}.  The stack contains libtss2-fapi,
libtss2-esys, libtss2-sys, libtss2-mu, libtss2-tcti-device, libtss2-tcti-swtpm
and libtss2-tcti-mssim.")
    (license license:bsd-2)))
