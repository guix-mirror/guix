;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Manolis Fragkiskos Ragkousis <manolis837@gmail.com>
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2016, 2018, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2017 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2021 Mathieu Othacehe <othacehe@gnu.org>
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

(define-module (gnu packages flashing-tools)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages libftdi)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages tls))

(define-public flashrom
  (package
    (name "flashrom")
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://download.flashrom.org/releases/flashrom-v"
                    version ".tar.bz2"))
              (sha256
               (base32
                "0ax4kqnh7kd3z120ypgp73qy1knz47l6qxsqzrfkd97mh5cdky71"))))
    (build-system gnu-build-system)
    (inputs (list dmidecode pciutils libusb libftdi))
    (native-inputs (list pkg-config))
    (arguments
     '(#:make-flags
       (list "CC=gcc"
             (string-append "PREFIX=" %output)
             "CONFIG_ENABLE_LIBUSB0_PROGRAMMERS=no")
       #:tests? #f                      ; no 'check' target
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-before 'build 'patch-exec-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "dmi.c"
               (("\"dmidecode\"")
                (format #f "~S"
                        (search-input-file inputs "/sbin/dmidecode"))))))
         (add-before 'build 'patch-type-error
           (lambda _
             ;; See https://github.com/flashrom/flashrom/pull/133
             (substitute* "libflashrom.c"
               (("supported_boards\\[i\\].working = binfo\\[i\\].working")
                "supported_boards[i].working = (enum flashrom_test_state)binfo[i].working")
               (("supported_chipsets\\[i\\].status = chipset\\[i\\].status")
                "supported_chipsets[i].status = (enum flashrom_test_state)chipset[i].status")))))))
    (home-page "https://flashrom.org/")
    (synopsis "Identify, read, write, erase, and verify ROM/flash chips")
    (description
     "flashrom is a utility for identifying, reading, writing,
verifying and erasing flash chips.  It is designed to flash
BIOS/EFI/coreboot/firmware/optionROM images on mainboards,
network/graphics/storage controller cards, and various other
programmer devices.")
    (license license:gpl2)))

(define-public 0xffff
  (package
    (name "0xffff")
    (version "0.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pali/0xffff")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rl1xzbxl991pm2is98zbryac1lgjrc3zphmbd8agv07av0r6r6n"))))
    (build-system gnu-build-system)
    (inputs
     ;; Building with libusb-compat will succeed but the result will be broken.
     ;; See <https://github.com/pali/0xFFFF/issues/3>.
     (list libusb-0.1))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure))           ; no configure
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             "HOST_CC=gcc"
             "BUILD_DATE=GNU Guix"
             (string-append "PREFIX=" %output))
       #:tests? #f))                    ; no 'check' target
    (home-page "https://github.com/pali/0xFFFF")
    (synopsis "Flash FIASCO images on Maemo devices")
    (description
     "The Open Free Fiasco Firmware Flasher (0xFFFF) is a flashing tool
for FIASCO images.  It supports generating, unpacking, editing and
flashing of FIASCO images for Maemo devices.  Use it with care.  It can
brick your device.")
    (license license:gpl3+)))

(define-public avrdude
  (package
    (name "avrdude")
    (version "6.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://savannah/avrdude/avrdude-"
                          version ".tar.gz"))
      (sha256
       (base32 "15m1w1qad3dj7r8n5ng1qqcaiyx1gyd6hnc3p2apgjllccdp77qg"))))
    (build-system gnu-build-system)
    (inputs
     (list libelf libusb-compat libftdi))
    (native-inputs
     (list bison flex))
    (home-page "https://www.nongnu.org/avrdude/")
    (synopsis "AVR downloader and uploader")
    (description
     "AVRDUDE is a utility to download/upload/manipulate the ROM and
EEPROM contents of AVR microcontrollers using the @acronym{ISP, in-system
programming} technique.")
    (license license:gpl2+)))

(define-public dfu-programmer
  (package
    (name "dfu-programmer")
    (version "0.7.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/dfu-programmer/dfu-programmer/"
                          version "/dfu-programmer-" version ".tar.gz"))
      (sha256
       (base32
        "15gr99y1z9vbvhrkd25zqhnzhg6zjmaam3vfjzf2mazd39mx7d0x"))
      (patches (search-patches "dfu-programmer-fix-libusb.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list libusb))
    (home-page "https://dfu-programmer.github.io/")
    (synopsis "Device firmware update programmer for Atmel chips")
    (description
     "Dfu-programmer is a multi-platform command-line programmer for
Atmel (8051, AVR, XMEGA & AVR32) chips with a USB bootloader supporting
ISP.")
    (license license:gpl2+)))

(define-public dfu-util
  (package
    (name "dfu-util")
    (version "0.11")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://dfu-util.sourceforge.net/releases/dfu-util-"
                    version ".tar.gz"))
              (sha256
               (base32
                "17piiyp08pccqmbhnswv957lkypmmm92kps79hypxvw23ai3pddl"))))
    (build-system gnu-build-system)
    (inputs
     (list libusb))
    (native-inputs
     (list pkg-config))
    (synopsis "Host side of the USB Device Firmware Upgrade (DFU) protocol")
    (description
     "The DFU (Universal Serial Bus Device Firmware Upgrade) protocol is
intended to download and upload firmware to devices connected over USB.  It
ranges from small devices like micro-controller boards up to mobile phones.
With dfu-util you are able to download firmware to your device or upload
firmware from it.")
    (home-page "http://dfu-util.sourceforge.net/")
    (license license:gpl2+)))

(define-public teensy-loader-cli
  ;; The repo does not tag versions nor does it use releases, but a commit
  ;; message says "Importing 2.1", while the sourcce still says "2.0". So pin
  ;; to a fixed commit.
  (let ((commit "f289b7a2e5627464044249f0e5742830e052e360"))
    (package
      (name "teensy-loader-cli")
      (version (git-version "2.1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/PaulStoffregen/teensy_loader_cli")
                (commit commit)))
         (sha256 (base32 "0sssim56pwsxp5cp5dlf6mi9h5fx2592m6j1g7abnm0s09b0lpdx"))
         (file-name (git-file-name name version))
         (modules '((guix build utils)))
         (snippet
          `(begin
             ;; Remove example flash files and teensy rebooter flash binaries.
             (for-each delete-file (find-files "." "\\.(elf|hex)$"))
             ;; Fix the version
             (substitute* "teensy_loader_cli.c"
               (("Teensy Loader, Command Line, Version 2.0\\\\n")
                (string-append "Teensy Loader, Command Line, " ,version "\\n")))
             #t))
       (patches (search-patches "teensy-loader-cli-help.patch"))))
      (build-system gnu-build-system)
      (arguments
       '(#:tests? #f ;; Makefile has no test target
         #:make-flags (list "CC=gcc" (string-append "PREFIX=" %output))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin")))
                 (install-file "teensy_loader_cli" bin)
                 #t))))))
      (inputs
       (list libusb-compat))
      (synopsis "Command line firmware uploader for Teensy development boards")
      (description
       "The Teensy loader program communicates with your Teensy board when the
HalfKay bootloader is running, so you can upload new programs and run them.

You need to add the udev rules to make the Teensy update available for
non-root users.")
      (home-page "https://www.pjrc.com/teensy/loader_cli.html")
      (license license:gpl3))))

(define-public rkflashtool
  (let ((commit "8966c4e277de8148290554aaaa4146a3a84a3c53")
        (revision "1"))
    (package
      (name "rkflashtool")
      (version (git-version "5.2" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                (url "https://github.com/linux-rockchip/rkflashtool")
                (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "1ndyzg1zlgg20dd8js9kfqm5kq19k005vddkvf65qj20w0pcyahn"))))
      (build-system gnu-build-system)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (delete 'configure)) ; no configure
         #:make-flags (list (string-append "PREFIX=" %output))
         #:tests? #f)) ; no tests
      (native-inputs
       (list pkg-config))
      (inputs
       (list libusb))
      (home-page "https://github.com/linux-rockchip/rkflashtool")
      (synopsis "Tools for flashing Rockchip devices")
      (description "Allows flashing of Rockchip based embedded linux devices.
The list of currently supported devices is: RK2818, RK2918, RK2928, RK3026,
RK3036, RK3066, RK312X, RK3168, RK3188, RK3288, RK3368.")
      (license license:bsd-2))))

(define-public heimdall
  (package
    (name "heimdall")
    (version "1.4.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://gitlab.com/BenjaminDobell/Heimdall.git")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ygn4snvcmi98rgldgxf5hwm7zzi1zcsihfvm6awf9s6mpcjzbqz"))))
    (build-system cmake-build-system)
    (arguments
     `(#:build-type "Release"
       #:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-invocations
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* '("heimdall-frontend/source/aboutform.cpp"
                            "heimdall-frontend/source/mainwindow.cpp")
               (("start[(]\"heimdall\"")
                (string-append "start(\"" (assoc-ref outputs "out")
                               "/bin/heimdall\"")))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin"))
                   (lib (string-append (assoc-ref outputs "out") "/lib")))
               (install-file "bin/heimdall" bin)
               (install-file "bin/heimdall-frontend" bin)
               (install-file "libpit/libpit.a" lib)
               #t))))))
    (inputs
     (list libusb qtbase-5 zlib))
    (home-page "https://glassechidna.com.au/heimdall/")
    (synopsis "Flash firmware onto Samsung mobile devices")
    (description "@command{heimdall} is a tool suite used to flash firmware (aka
ROMs) onto Samsung mobile devices.  Heimdall connects to a mobile device over
USB and interacts with low-level software running on the device, known as Loke.
Loke and Heimdall communicate via the custom Samsung-developed protocol typically
referred to as the \"Odin 3 protocol\".")
    (license license:expat)))

(define-public ifdtool
  (package
    (name "ifdtool")
    (version "4.9")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/coreboot/coreboot")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0jidj29jh6p65d17k304wlzhxvp4p3c2namgcdwg2sxq8jfr0zlm"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list "CC=gcc"
             "INSTALL=install"
             (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "util/ifdtool")
             #t))
         (delete 'configure))           ; no configure script
       #:tests? #f))                    ; no test suite
    (home-page "https://github.com/corna/me_cleaner/")
    (synopsis "Intel Firmware Descriptor dumper")
    (description "This package provides @command{ifdtool}, a program to
dump Intel Firmware Descriptor data of an image file.")
    (license license:gpl2)))

(define-public intelmetool
  (package
    (name "intelmetool")
    (version "4.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://review.coreboot.org/p/coreboot")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0nw555i0fm5kljha9h47bk70ykbwv8ddfk6qhz6kfqb79vzhy4h2"))))
    (build-system gnu-build-system)
    (inputs
     (list pciutils zlib))
    (arguments
     `(#:make-flags
       (list "CC=gcc"
             "INSTALL=install"
             (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
        (add-after 'unpack 'chdir
          (lambda _
            (chdir "util/intelmetool")
            #t))
        (delete 'configure)
        (delete 'check))))
    (home-page "https://github.com/zamaudio/intelmetool")
    (synopsis "Intel Management Engine tools")
    (description "This package provides tools for working with Intel
Management Engine (ME).  You need to @code{sudo rmmod mei_me} and
@code{sudo rmmod mei} before using this tool.  Also pass
@code{iomem=relaxed} to the Linux kernel command line.")
    (license license:gpl2)

    ;; This is obviously an Intel thing, plus it requires <cpuid.h>.
    (supported-systems '("x86_64-linux" "i686-linux"))))

(define-public me-cleaner
  (package
    (name "me-cleaner")
    (version "1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/corna/me_cleaner")
                     (commit (string-append "v" version))))
              (sha256
               (base32
                "1bdj2clm13ir441vn7sv860xsc5gh71ja5lc2wn0gggnff0adxj4"))
              (file-name (git-file-name name version))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-documentation
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (man (string-append out "/share/man/man1")))
               (install-file "man/me_cleaner.1" man)
               #t))))))
    (home-page "https://github.com/corna/me_cleaner")
    (synopsis "Intel ME cleaner")
    (description "This package provides tools for disabling Intel
ME as far as possible (it only edits ME firmware image files).")
    (license license:gpl3+)

    ;; This is an Intel thing.
    (supported-systems '("x86_64-linux" "i686-linux"))))

(define-public uefitool
  (package
    (name "uefitool")
    (version "0.28.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/LongSoft/UEFITool")
                     (commit version)))
              (sha256
               (base32
                "1n2hd2dysi5bv2iyq40phh1jxc48gdwzs414vfbxvcharcwapnja"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             (invoke "qmake" "-makefile")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "UEFITool" (string-append (assoc-ref outputs "out")
                                                     "/bin")))))))
    (inputs
     (list qtbase-5))
    (home-page "https://github.com/LongSoft/UEFITool/")
    (synopsis "UEFI image editor")
    (description "@code{uefitool} is a graphical image file editor for
Unifinished Extensible Firmware Interface (UEFI) images.")
    (license license:bsd-2)))

(define-public srecord
  (package
    (name "srecord")
    (version "1.64")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/srecord/srecord/"
                           version "/srecord-" version ".tar.gz"))
       (sha256
        (base32
         "1qk75q0k5vzmm3932q9hqz2gp8n9rrdfjacsswxc02656f3l3929"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "SH="
                            (assoc-ref %build-inputs "bash")
                            "/bin/bash"))))
    (inputs
     (list boost libgcrypt))
    (native-inputs
     (list bison
           diffutils
           ghostscript
           groff
           libtool
           which))
    (home-page "http://srecord.sourceforge.net/")
    (synopsis "Tools for EPROM files")
    (description "The SRecord package is a collection of powerful tools for
manipulating EPROM load files.  It reads and writes numerous EPROM file
formats, and can perform many different manipulations.")
    (license license:gpl3+)))

(define-public uuu
  (package
    (name "uuu")
    (version "1.4.165")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/NXPmicro/mfgtools")
             (commit (string-append "uuu_" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0k309lp27d4k6x4qq0badbk8i47xsc6f3fffz73650iyfs4hcniw"))))
    (arguments
     `(#:tests? #f                      ; no tests
       #:modules ((guix build utils)
                  (ice-9 popen)
                  (srfi srfi-26)
                  (guix build cmake-build-system))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-version-gen
           (lambda _
             (call-with-output-file ".tarball-version"
               (lambda (port)
                 (display ,version port)))))
         (add-after 'install 'install-udev-rules
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (uuu (string-append out "/bin/uuu"))
                    (pipe (open-pipe* OPEN_READ uuu "-udev"))
                    (rules
                     (string-append out "/lib/udev/rules.d/70-uuu.rules")))
               (mkdir-p (string-append out "/lib/udev/rules.d"))
               (call-with-output-file rules
                 (cut dump-port pipe <>))))))))
    (build-system cmake-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list libusb bzip2 zlib libzip openssl))
    (home-page "https://github.com/NXPmicro/mfgtools")
    (synopsis "Freescale/NXP I.MX chip image deploy tools")
    (description "@code{uuu} is a command line tool, evolved out of MFGTools.
It can be used to upload images to I.MX SoC's using at least their boot ROM.")
    (license license:bsd-3)))
