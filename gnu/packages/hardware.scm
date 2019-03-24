;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
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
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

;; This is a module for packages related to physical hardware that don't (yet)
;; have a more specific home like gps.scm, security-token.scm, &c.

(define-public ddcutil
  (package
    (name "ddcutil")
    (version "0.9.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.ddcutil.com/tarballs/"
                           "ddcutil-" version ".tar.gz"))
       (sha256
        (base32 "18brwj54dkjylvpx7c6ksf7fzhdjffi60avyg7qbs8vw9awnsxqz"))))
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
    (version "4.3.0")
    (source
     (origin
       (method url-fetch)
       ;; Even the latest release is available under 'old-versions/'.
       (uri (string-append "http://pyropus.ca/software/memtester/old-versions/"
                           "memtester-" version ".tar.gz"))
       (sha256
        (base32 "127xymmyzb9r6dxqrwd69v7gf8csv8kv7fjvagbglf3wfgyy5pzr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list "CC=gcc")
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
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/uoaerg/wavemon.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07cid0h3mcyr74nnrzzf8k5n1p9a4y3wij43jbiaqmkpxilcc1i6"))))
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
