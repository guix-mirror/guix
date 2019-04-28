;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2015, 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015, 2016, 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2015 Andy Patterson <ajpatter@uwaterloo.ca>
;;; Copyright © 2015, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016, 2017, 2018, 2019 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2016, 2017 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2016 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2016 Dmitry Nikolaev <cameltheman@gmail.com>
;;; Copyright © 2016 Andy Patterson <ajpatter@uwaterloo.ca>
;;; Copyright © 2016, 2017 ng0 <ng0@n0.is>
;;; Copyright © 2016, 2018, 2019 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2017 Feng Shu <tumashu@163.com>
;;; Copyright © 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017 Ethan R. Jones <doubleplusgood23@gmail.com>
;;; Copyright © 2017, 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017 Gregor Giesen <giesen@zaehlwerk.net>
;;; Copyright © 2017, 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018 Roel Janssen <roel@gnu.org>
;;; Copyright © 2018, 2019 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018, 2019 Leo Famulari <leo@famulari.name>
;;; Copyright © 2018 Brendan Tildesley <brendan.tildesley@openmailbox.org>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2018 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;; Copyright © 2018 Mark Meyer <mark@ofosos.org>
;;; Copyright © 2018 Gábor Boskovit <boskovits@gmail.com>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019 Timo Eisenmann <eisenmann@fn.de>
;;; Copyright © 2019 Arne Babenhauserheide <arne_bab@web.de>
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

(define-module (gnu packages video)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system waf)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages dejagnu)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages file)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnunet)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages man)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ocr)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages rdesktop)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages time)
  #:use-module (gnu packages upnp)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages web)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public aalib
  (package
    (name "aalib")
    (version "1.4rc5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/aa-project/aa-lib/"
                                  version "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "1vkh19gb76agvh4h87ysbrgy82hrw88lnsvhynjf4vng629dmpgv"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("makeinfo" ,texinfo)))
    (inputs
     `(("ncurses" ,ncurses)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
                  (lambda* (#:key build inputs outputs #:allow-other-keys)
                    ;; This old `configure' script doesn't support
                    ;; variables passed as arguments.
                    (let ((out     (assoc-ref outputs "out"))
                          (ncurses (assoc-ref inputs "ncurses")))
                      (setenv "CONFIG_SHELL" (which "bash"))
                      (invoke "./configure"
                              (string-append "--prefix=" out)
                              (string-append "--build=" build)
                              ;; The ancient config.guess is unable to
                              ;; guess the host triplet on mips64el.
                              ,@(if (string=? "mips64el-linux"
                                              (%current-system))
                                    '("--host=mips64el-unknown-linux-gnu")
                                    '())
                              ;; The same is also true with aarch64.
                              ,@(if (string=? "aarch64-linux"
                                              (%current-system))
                                    '("--host=aarch64-unknown-linux-gnu")
                                    '())
                              (string-append "--with-ncurses="
                                             ncurses))))))))
    (home-page "http://aa-project.sourceforge.net/aalib/")
    (synopsis "ASCII-art library")
    (description
     "AA-lib is a low level gfx library which does not require graphics device.
In fact, there is no graphical output possible.  AA-lib replaces those
old-fashioned output methods with powerful ascii-art renderer.")
    (license license:lgpl2.0+)))

(define-public liba52
  (package
    (name "liba52")
    (version "0.7.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    ;; A mirror://sourceforge URI doesn't work, presumably
                    ;; because the SourceForge project is misconfigured.
                    "http://liba52.sourceforge.net/files/a52dec-" version
                    ".tar.gz"))
              (sha256
               (base32
                "0czccp4fcpf2ykp16xcrzdfmnircz1ynhls334q374xknd5747d2"))
              (patches (search-patches "liba52-enable-pic.patch"
                                       "liba52-set-soname.patch"
                                       "liba52-use-mtune-not-mcpu.patch"
                                       "liba52-link-with-libm.patch"))))
    (build-system gnu-build-system)
    ;; XXX We need to run ./bootstrap because of the build system fixes above.
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (arguments `(#:configure-flags '("--enable-shared")
                 #:phases
                 (modify-phases %standard-phases
                   ;; XXX We need to run ./bootstrap because of the build
                   ;; system fixes above.
                   (replace 'bootstrap
                     (lambda _ (invoke "sh" "bootstrap"))))))
    (home-page "http://liba52.sourceforge.net/")
    (synopsis "ATSC A/52 stream decoder")
    (description "liba52 is a library for decoding ATSC A/52 streams.  The
A/52 standard is used in a variety of applications, including digital
television and DVD.  It is also known as AC-3.")
    (license license:gpl2+)))

(define-public libaom
  ;; The 1.0.0-errata1 release installs a broken pkg-config .pc file.  This
  ;; is fixed in libaom commit 0ddc150, but we use an even later commit.
  (let ((commit "22b150bf040608028a56d8bf39e72f771383d836")
        (revision "0"))
    (package
      (name "libaom")
      (version (git-version "1.0.0-errata1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://aomedia.googlesource.com/aom/")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1pdd5h3n42607n6qmggz4yv8izhjr2kl6knb3kh7gh4v0vy47h1r"))))
      (build-system cmake-build-system)
      (native-inputs
       `(("perl" ,perl)
         ("pkg-config" ,pkg-config)
         ("python" ,python))) ; to detect the version
      (arguments
       `(#:tests? #f  ;no check target
         #:configure-flags
           ;; build dynamic library
         (list "-DBUILD_SHARED_LIBS=YES"
               "-DENABLE_PIC=TRUE"
               "-DAOM_TARGET_CPU=generic"
               (string-append "-DCMAKE_INSTALL_PREFIX="
                                (assoc-ref %outputs "out")))))
      (home-page "https://aomedia.googlesource.com/aom/")
      (synopsis "AV1 video codec")
      (description "Libaom is the reference implementation of AV1.  It includes
a shared library and encoder and decoder command-line executables.")
      (license license:bsd-2))))

(define-public libmpeg2
  (package
    (name "libmpeg2")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              ;; A mirror://sourceforge URI doesn't work, presumably
              ;; because the SourceForge project is misconfigured.
              (uri (string-append "http://libmpeg2.sourceforge.net/files/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "1m3i322n2fwgrvbs1yck7g5md1dbg22bhq5xdqmjpz5m7j4jxqny"))))
    (inputs
     `(("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxv" ,libxv)
       ("libsm" ,libsm)
       ("libice" ,libice)
       ("sdl" ,sdl)))
    (build-system gnu-build-system)
    (home-page "http://libmpeg2.sourceforge.net/")
    (synopsis "MPEG1 and MPEG2 video decoder library")
    (description
     "libmpeg2 is a library which can decode MPEG1 and MPEG2 video streams.")
    (license license:gpl2+)))

(define-public libx264
  (package
    (name "libx264")
    (version "20180810-2245")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.videolan.org/pub/x264/snapshots/"
                                  "x264-snapshot-" version "-stable.tar.bz2"))
              (sha256
               (base32
                "0f25f39imas9pcqm7lnaa0shhjmf42hdx7jxzcnvxc7qsb7lh1bv"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("nasm" ,nasm)))
    ;; TODO: Add gpac input
    (arguments
     `(#:tests? #f  ;no check target
       #:configure-flags '("--enable-shared"
                           ;; Don't build the command-line program.  If we
                           ;; want it later, we should do so in a different
                           ;; package to avoid a circular dependency (the x264
                           ;; program depends on ffmpeg and ffmpeg depends on
                           ;; libx264).
                           "--disable-cli"

                           ;; On MIPS, we must pass "--disable-asm" or else
                           ;; configure fails after printing: "You specified a
                           ;; pre-MSA CPU in your CFLAGS. If you really want
                           ;; to run on such a CPU, configure with
                           ;; --disable-asm."
                           ,@(if (string-prefix? "mips"
                                                 (or (%current-target-system)
                                                     (%current-system)))
                                 '("--disable-asm")
                                 '()))))
    (home-page "https://www.videolan.org/developers/x264.html")
    (synopsis "H.264 video coding library")
    (description "libx264 is an advanced encoding library for creating
H.264 (MPEG-4 AVC) video streams.")
    (license (list license:gpl2+         ;most files
                   license:isc           ;common/x86/x86inc.asm
                   license:lgpl2.1+      ;extras/getopt.c
                   license:bsd-3         ;extras/inttypes.h
                   (license:non-copyleft ;extras/cl*.h
                    "file://extras/cl.h"
                    "See extras/cl.h in the distribution.")))))

(define-public mkvtoolnix
  (package
    (name "mkvtoolnix")
    (version "31.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://mkvtoolnix.download/sources/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0d8va2iamzc7y3wi71z8mk2vnqvnkgwb2p7casdfp37400x8r2pr"))
       (modules '((guix build utils)))
       (snippet '(begin
                   ;; Delete bundled libraries.
                   (for-each delete-file-recursively
                             '("lib/fmt"
                               "lib/libebml"
                               "lib/libmatroska"
                               "lib/nlohmann-json"
                               "lib/pugixml"
                               "lib/utf8-cpp"))
                   #t))))
    (build-system gnu-build-system)
    (outputs '("out" "gui")) ; "mkvtoolnix-gui" brings the closure size from ~300 MB to 1.5+ GB.
    (inputs
     `(("boost" ,boost)
       ("bzip2" ,bzip2)
       ("cmark" ,cmark)
       ("libebml" ,libebml)
       ("file" ,file)
       ("flac" ,flac)
       ("fmt" ,fmt)
       ("libmatroska" ,libmatroska)
       ("libogg" ,libogg)
       ("libvorbis" ,libvorbis)
       ("lzo" ,lzo)
       ("pugixml" ,pugixml)
       ("qtbase" ,qtbase)
       ("qtmultimedia" ,qtmultimedia)
       ("utfcpp" ,utfcpp)
       ("zlib" ,zlib)))
    (native-inputs
     `(("docbook-xsl" ,docbook-xsl)
       ("gettext" ,gettext-minimal)
       ("googletest" ,googletest)
       ("libxslt" ,libxslt)
       ("nlohmann-json-cpp" ,nlohmann-json-cpp)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("po4a" ,po4a)
       ("qttools" ,qttools)
       ("ruby" ,ruby)))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-boost="
                            (assoc-ref %build-inputs "boost"))
             (string-append "--with-docbook-xsl-root="
                            (assoc-ref %build-inputs "docbook-xsl")
                            "/xml/xsl/docbook-xsl-"
                            ,(package-version docbook-xsl))
             "--enable-update-check=no"
             "--enable-precompiled-headers=no")
        #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'add-googletest
           (lambda* (#:key inputs #:allow-other-keys)
             (symlink
               (string-append (assoc-ref inputs "googletest")
                              "/include/gtest") "lib/gtest")
             #t))
         (replace 'build
           (lambda _
             (let ((-j (list "-j" (number->string (parallel-job-count)))))
               (apply invoke "rake" -j))))
         (replace 'check
           (lambda _
             (invoke "rake" "tests/unit")))
         (replace 'install
           (lambda _
             (invoke "rake" "install")))
         (add-after 'install 'post-install
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Move the Qt interface to "gui".
             (let* ((out (assoc-ref outputs "out"))
                    (gui (assoc-ref outputs "gui"))
                    (strip-store-dir (lambda (path)
                                       (substring path (string-prefix-length out path)))))
               (for-each
                (lambda (file)
                  (mkdir-p (string-append gui (dirname file)))
                  (rename-file (string-append out file)
                               (string-append gui file)))
                (append '("/bin/mkvtoolnix-gui"
                          "/share/applications/org.bunkus.mkvtoolnix-gui.desktop"
                          "/share/metainfo/org.bunkus.mkvtoolnix-gui.appdata.xml"
                          "/share/mime/packages/org.bunkus.mkvtoolnix-gui.xml")
                        (map strip-store-dir (find-files out "\\.ogg$"))
                        (map strip-store-dir (find-files out "mkvtoolnix-gui\\.png$"))
                        (map strip-store-dir (find-files out "mkvtoolnix-gui\\.1"))))
               (for-each
                (lambda (file)
                  (delete-file-recursively (string-append out file)))
                '("/share/applications"
                  "/share/metainfo"
                  "/share/mime"
                  "/share/mkvtoolnix")))
             #t)))))
    (home-page "https://mkvtoolnix.download")
    (synopsis "Tools to create, alter and inspect Matroska files")
    (description
     "MKVToolNix provides tools for getting information about Matroska files
(@code{mkvinfo}), extracting tracks/data from Matroska files (@code{mkvextract})
and creating Matroska files from other media files (@code{mkvmerge}).")
    (license license:gpl2)))

(define-public x265
  (package
    (name "x265")
    (version "3.0")
    (outputs '("out" "static"))
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://download.videolan.org/videolan/x265/"
                            "x265_" version ".tar.gz"))
        (sha256
         (base32
          "0qh65wdpasrspkm1y0dlfa123myax568yi0sas0lmg5b1hkgrff5"))
        (patches (search-patches "x265-arm-flags.patch"))
        (modules '((guix build utils)))
        (snippet '(begin
                    (delete-file-recursively "source/compat/getopt")
                    #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ; tests are skipped if cpu-optimized code isn't built
       #:configure-flags
         ;; Ensure position independent code for everyone.
         (list "-DENABLE_PIC=TRUE"
               ,@(if (string-prefix? "armhf" (or (%current-system)
                                                 (%current-target-system)))
                     '("-DENABLE_ASSEMBLY=OFF")
                     '())
               (string-append "-DCMAKE_INSTALL_PREFIX="
                              (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'prepare-build
           (lambda _
             (delete-file-recursively "build")
             (chdir "source")
             ;; recognize armv8 in 32-bit mode as ARM
             (substitute* "CMakeLists.txt"
              (("armv6l") "armv8l"))
             #t))
         (add-before 'configure 'build-12-bit
           (lambda* (#:key (configure-flags '()) #:allow-other-keys)
             (mkdir "../build-12bit")
             (with-directory-excursion "../build-12bit"
               (apply invoke
                 "cmake" "../source"
                 "-DHIGH_BIT_DEPTH=ON"
                 "-DEXPORT_C_API=OFF"
                 "-DENABLE_CLI=OFF"
                 "-DMAIN12=ON"
                 configure-flags)
               (substitute* (cons "cmake_install.cmake"
                                  (append
                                    (find-files "CMakeFiles/x265-shared.dir" ".")
                                    (find-files "CMakeFiles/x265-static.dir" ".")))
                 (("libx265") "libx265_main12"))
               (invoke "make"))))
         (add-before 'configure 'build-10-bit
           (lambda* (#:key (configure-flags '()) #:allow-other-keys)
             (mkdir "../build-10bit")
             (with-directory-excursion "../build-10bit"
               (apply invoke
                 "cmake" "../source"
                 "-DHIGH_BIT_DEPTH=ON"
                 "-DEXPORT_C_API=OFF"
                 "-DENABLE_CLI=OFF"
                 configure-flags)
               (substitute* (cons "cmake_install.cmake"
                                  (append
                                    (find-files "CMakeFiles/x265-shared.dir" ".")
                                    (find-files "CMakeFiles/x265-static.dir" ".")))
                 (("libx265") "libx265_main10"))
               (invoke "make"))))
         (add-after 'install 'install-more-libs
           (lambda _
             (with-directory-excursion "../build-12bit"
               (invoke "make" "install"))
             (with-directory-excursion "../build-10bit"
               (invoke "make" "install"))))
         (add-before 'strip 'move-static-libs
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (static (assoc-ref outputs "static")))
               (mkdir-p (string-append static "/lib"))
               (with-directory-excursion
                 (string-append out "/lib")
                 (for-each
                   (lambda (file)
                     (rename-file file
                                  (string-append static "/lib/" file)))
                   (find-files "." "\\.a$"))))
             #t)))))
    (home-page "http://x265.org/")
    (synopsis "Library for encoding h.265/HEVC video streams")
    (description "x265 is a H.265 / HEVC video encoder application library,
designed to encode video or images into an H.265 / HEVC encoded bitstream.")
    (license license:gpl2+)))

(define-public libass
  (package
    (name "libass")
    (version "0.14.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/libass/libass/releases/download/"
                    version "/libass-" version ".tar.xz"))
              (sha256
               (base32
                "18iqznl4mabhj9ywfsz4kwvbsplcv1jjxq50nxssvbj8my1267w8"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("nasm" ,nasm)))
    (propagated-inputs
     `(("freetype" ,freetype)
       ("fribidi" ,fribidi)
       ("fontconfig" ,fontconfig)
       ("harfbuzz" ,harfbuzz)
       ("enca" ,enca)))
    (home-page "https://github.com/libass/libass")
    (synopsis "Subtitle rendering library for the ASS/SSA format")
    (description "libass is a subtitle rendering library for the
ASS/SSA (Advanced Substation Alpha/SubStation Alpha) subtitle format.")
    (license license:isc)))

(define-public libcaca
  (package
    (name "libcaca")
    (version "0.99.beta19")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://caca.zoy.org/files/libcaca/libcaca-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1x3j6yfyxl52adgnabycr0n38j9hx2j74la0hz0n8cnh9ry4d2qj"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs
     `(("freeglut" ,freeglut)
       ("ftgl" ,ftgl)
       ("imlib2" ,imlib2)
       ("libx11" ,libx11)
       ("mesa" ,mesa)
       ("ncurses" ,ncurses)
       ("zlib" ,zlib)))
    (home-page "http://caca.zoy.org/wiki/libcaca")
    (synopsis "Colour ASCII-art library")
    (description "libcaca is a graphics library that outputs text instead of
pixels, so that it can work on older video cards or text terminals.  It
supports Unicode, 2048 colors, dithering of color images, and advanced text
canvas operations.")
    (license (license:fsf-free "file://COPYING")))) ;WTFPL version 2

(define-public libdca
  (package
    (name "libdca")
    (version "0.0.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://download.videolan.org/pub/videolan/libdca/"
                    version "/libdca-" version ".tar.bz2"))
              (sha256
               (base32
                "0h0zvcn97i9kyljdpifzi8in9xnw31fx3b3ggj96p8h0l2d8mycq"))))
    (build-system gnu-build-system)
    (home-page "https://www.videolan.org/developers/libdca.html")
    (synopsis "DTS Coherent Acoustics decoder")
    (description "libdca is a library for decoding DTS Coherent Acoustics
streams.")
    (license license:gpl2+)))

(define-public libdv
  (package
    (name "libdv")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/" name "/" name "/"
                    version "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "1fl96f2xh2slkv1i1ix7kqk576a0ak1d33cylm0mbhm96d0761d3"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("libxv" ,libxv)))
    (home-page "http://libdv.sourceforge.net/")
    (synopsis "DV video (IEC 61834 and SMPTE 314M) codec")
    (description "The Quasar DV codec (libdv) is a software codec for DV
video, the encoding format used by most digital camcorders, typically those
that support the IEEE 1394 (a.k.a. FireWire or i.Link) interface.  Libdv was
developed according to the official standards for DV video: IEC 61834 and
SMPTE 314M.")
    (license license:lgpl2.1+)))

(define-public libmatroska
  (package
    (name "libmatroska")
    (version "1.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dl.matroska.org/downloads/"
                           "libmatroska/libmatroska-" version ".tar.xz"))
       (sha256
        (base32 "07md2gvy3x92ym2k449740mdji6mhknlajkndnhi507s4wcdrvzh"))))
    (build-system cmake-build-system)
    (inputs
     `(("libebml" ,libebml)))
    (arguments
     `(#:configure-flags
       (list "-DBUILD_SHARED_LIBS=YES")
       #:tests? #f))                    ; no test suite
    (home-page "https://www.matroska.org")
    (synopsis "C++ library to parse Matroska files (.mkv and .mka)")
    (description
     "Matroska aims to become the standard of multimedia container formats.
It is based on @dfn{EBML} (Extensible Binary Meta Language), a binary derivative
of XML.  EBML enables the Matroska Development Team to gain significant
advantages in terms of future format extensibility, without breaking file
support in old parsers.
libebml is a C++ library to read and write EBML files.")
    (license license:lgpl2.1)))

(define-public libva
  (package
    (name "libva")
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (list
             ;; Newer releases are only available on GitHub.
             (string-append "https://github.com/01org/libva/releases/download/"
                            version "/libva-" version ".tar.bz2")
             ;; Keep the old URL around for compatibility.
             (string-append "https://www.freedesktop.org/software/vaapi/releases/"
                            "libva/libva-" version "/libva-" version ".tar.bz2")))
       (sha256
        (base32 "14ckq67z3pcd7jrnrm3ckss440g6dzp2m0ff5rps54qmq9b309lr"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libdrm" ,libdrm)
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxfixes" ,libxfixes)
       ("mesa" ,mesa)
       ("wayland" ,wayland)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before
          'build 'fix-dlopen-paths
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (substitute* "va/drm/va_drm_auth_x11.c"
                (("\"libva-x11\\.so\\.%d\"")
                 (string-append "\"" out "/lib/libva-x11.so.%d\"")))
              #t))))
       ;; Most drivers are in mesa's $prefix/lib/dri, so use that.  (Can be
       ;; overridden at run-time via LIBVA_DRIVERS_PATH.)
       #:configure-flags
       (list (string-append "--with-drivers-path="
                            (assoc-ref %build-inputs "mesa") "/lib/dri"))
       ;; However, we can't write to mesa's store directory, so override the
       ;; following make variable to install the dummy driver to libva's
       ;; $prefix/lib/dri directory.
       #:make-flags
       (list (string-append "dummy_drv_video_ladir="
                            (assoc-ref %outputs "out") "/lib/dri"))))
    (home-page "https://www.freedesktop.org/wiki/Software/vaapi/")
    (synopsis "Video acceleration library")
    (description "The main motivation for VA-API (Video Acceleration API) is
to enable hardware accelerated video decode/encode at various
entry-points (VLD, IDCT, Motion Compensation etc.) for prevailing coding
standards (MPEG-2, MPEG-4 ASP/H.263, MPEG-4 AVC/H.264, and VC-1/VMW3).")
    (license license:expat)))

(define-public ffmpeg
  (package
    (name "ffmpeg")
    (version "4.1.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://ffmpeg.org/releases/ffmpeg-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0gdnprc7gk4b7ckq8wbxbrj7i00r76r9a5g9mj7iln40512j0c0c"))))
    (build-system gnu-build-system)
    (inputs
     `(("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("frei0r-plugins" ,frei0r-plugins)
       ("gnutls" ,gnutls)
       ("opus" ,opus)
       ("ladspa" ,ladspa)
       ("lame" ,lame)
       ("libaom" ,libaom)
       ("libass" ,libass)
       ("libbluray" ,libbluray)
       ("libcaca" ,libcaca)
       ("libcdio-paranoia" ,libcdio-paranoia)
       ("libdrm" ,libdrm)
       ("libtheora" ,libtheora)
       ("libva" ,libva)
       ("libvdpau" ,libvdpau)
       ("libvorbis" ,libvorbis)
       ("libvpx" ,libvpx)
       ("libx11" ,libx11)
       ("libx264" ,libx264)
       ("mesa" ,mesa)
       ("openal" ,openal)
       ("pulseaudio" ,pulseaudio)
       ("sdl" ,sdl2)
       ("soxr" ,soxr)
       ("speex" ,speex)
       ("twolame" ,twolame)
       ("vidstab" ,vidstab)
       ("x265" ,x265)
       ("xvid" ,xvid)
       ("zlib" ,zlib)))
    (native-inputs
     `(("bc" ,bc)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("texinfo" ,texinfo)
       ("python" ,python-2) ; scripts use interpreter python2
       ("speex" ,speex)
       ("yasm" ,yasm)))
    (arguments
     `(#:test-target "fate"
       #:configure-flags
       ;; possible additional inputs:
       ;;   --enable-avisynth        enable reading of AviSynth script
       ;;                            files [no]
       ;;   --enable-libaacplus      enable AAC+ encoding via libaacplus [no]
       ;;   --enable-libcelt         enable CELT decoding via libcelt [no]
       ;;   --enable-libdc1394       enable IIDC-1394 grabbing using libdc1394
       ;;                            and libraw1394 [no]
       ;;   --enable-libfaac         enable AAC encoding via libfaac [no]
       ;;   --enable-libfdk-aac      enable AAC de/encoding via libfdk-aac [no]
       ;;   --enable-libflite        enable flite (voice synthesis) support via
       ;;                            libflite [no]
       ;;   --enable-libgme          enable Game Music Emu via libgme [no]
       ;;   --enable-libgsm          enable GSM de/encoding via libgsm [no]
       ;;   --enable-libiec61883     enable iec61883 via libiec61883 [no]
       ;;   --enable-libilbc         enable iLBC de/encoding via libilbc [no]
       ;;   --enable-libmodplug      enable ModPlug via libmodplug [no]
       ;;   --enable-libnut          enable NUT (de)muxing via libnut,
       ;;                            native (de)muxer exists [no]
       ;;   --enable-libopencore-amrnb    enable AMR-NB de/encoding via
       ;;                                 libopencore-amrnb [no]
       ;;   --enable-libopencore-amrwb    enable AMR-WB decoding via
       ;;                                 libopencore-amrwb [no]
       ;;   --enable-libopencv       enable video filtering via libopencv [no]
       ;;   --enable-libopenjpeg     enable JPEG 2000 de/encoding via
       ;;                            OpenJPEG [no]
       ;;   --enable-librtmp         enable RTMP[E] support via librtmp [no]
       ;;   --enable-libschroedinger enable Dirac de/encoding via
       ;;                            libschroedinger [no]
       ;;   --enable-libshine        enable fixed-point MP3 encoding via
       ;;                            libshine [no]
       ;;   --enable-libssh          enable SFTP protocol via libssh [no]
       ;;                            (libssh2 does not work)
       ;;   --enable-libstagefright-h264  enable H.264 decoding via
       ;;                                 libstagefright [no]
       ;;   --enable-libutvideo      enable Ut Video encoding and decoding via
       ;;                            libutvideo [no]
       ;;   --enable-libv4l2         enable libv4l2/v4l-utils [no]
       ;;   --enable-libvo-aacenc    enable AAC encoding via libvo-aacenc [no]
       ;;   --enable-libvo-amrwbenc  enable AMR-WB encoding via
       ;;                            libvo-amrwbenc [no]
       ;;   --enable-libwavpack      enable wavpack encoding via libwavpack [no]
       ;;   --enable-libxavs         enable AVS encoding via xavs [no]
       ;;   --enable-libzmq          enable message passing via libzmq [no]
       ;;   --enable-libzvbi         enable teletext support via libzvbi [no]
       ;;   --enable-opencl          enable OpenCL code
       '("--enable-avresample"
         "--enable-gpl" ; enable optional gpl licensed parts
         "--enable-shared"
         "--enable-frei0r"
         "--enable-fontconfig"
         "--enable-gnutls"
         "--enable-ladspa"
         "--enable-libaom"
         "--enable-libass"
         "--enable-libbluray"
         "--enable-libcaca"
         "--enable-libcdio"
         "--enable-libfreetype"
         "--enable-libmp3lame"
         "--enable-libopus"
         "--enable-libpulse"
         "--enable-libsoxr"
         "--enable-libspeex"
         "--enable-libtheora"
         "--enable-libtwolame"
         "--enable-libvidstab"
         "--enable-libvorbis"
         "--enable-libvpx"
         "--enable-libxvid"
         "--enable-libx264"
         "--enable-libx265"
         "--enable-openal"
         "--enable-opengl"
         "--enable-libdrm"

         "--enable-runtime-cpudetect"

         ;; The HTML pages take 7.2 MiB
         "--disable-htmlpages"

         ;; The static libraries are 23 MiB
         "--disable-static"

         ;; Runtime cpu detection is not implemented on
         ;; MIPS, so we disable some features.
         "--disable-mips32r2"
         "--disable-mipsdsp"
         "--disable-mipsdspr2"
         "--disable-mipsfpu")
       #:phases
       (modify-phases %standard-phases
         (replace
          'configure
          ;; configure does not work followed by "SHELL=..." and
          ;; "CONFIG_SHELL=..."; set environment variables instead
          (lambda* (#:key outputs configure-flags #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (substitute* "configure"
                (("#! /bin/sh") (string-append "#!" (which "sh"))))
              (setenv "SHELL" (which "bash"))
              (setenv "CONFIG_SHELL" (which "bash"))
              (apply invoke
                     "./configure"
                     (string-append "--prefix=" out)
                     ;; Add $libdir to the RUNPATH of all the binaries.
                     (string-append "--extra-ldflags=-Wl,-rpath="
                                    out "/lib")
                     configure-flags))))
         (add-before
          'check 'set-ld-library-path
          (lambda _
            ;; Allow $(top_builddir)/ffmpeg to find its dependencies when
            ;; running tests.
            (let* ((dso  (find-files "." "\\.so$"))
                   (path (string-join (map dirname dso) ":")))
              (format #t "setting LD_LIBRARY_PATH to ~s~%" path)
              (setenv "LD_LIBRARY_PATH" path)
              #t))))))
    (home-page "https://www.ffmpeg.org/")
    (synopsis "Audio and video framework")
    (description "FFmpeg is a complete, cross-platform solution to record,
convert and stream audio and video.  It includes the libavcodec
audio/video codec library.")
    (license license:gpl2+)))

(define-public ffmpeg-3.4
  (package
    (inherit ffmpeg)
    (version "3.4.6")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://ffmpeg.org/releases/ffmpeg-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0gmqbhg5jjcfanrxrl657zn12lzz73sfs8xwryfy7n9rn6f2fwim"))))
    (arguments
     (substitute-keyword-arguments (package-arguments ffmpeg)
       ((#:configure-flags flags)
        `(delete "--enable-libaom" ,flags))))
    (inputs (alist-delete "libaom"
                          (package-inputs ffmpeg)))))

(define-public ffmpeg-for-stepmania
  (hidden-package
   (package
     (inherit ffmpeg)
     (version "2.1.3")
     (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/stepmania/ffmpeg.git")
              (commit "eda6effcabcf9c238e4635eb058d72371336e09b")))
        (sha256
         (base32 "1by8rmbva8mfrivdbbkr2gx4kga89zqygkd4cfjl76nr8mdcdamb"))
        (file-name (git-file-name "ffmpeg" version))))
     (arguments
      (substitute-keyword-arguments (package-arguments ffmpeg)
        ((#:configure-flags flags)
         '(list "--disable-programs"
                "--disable-doc"
                "--disable-debug"
                "--disable-avdevice"
                "--disable-swresample"
                "--disable-postproc"
                "--disable-avfilter"
                "--disable-shared"
                "--enable-static"))))
     (inputs '()))))

(define-public ffmpegthumbnailer
  (package
    (name "ffmpegthumbnailer")
    (version "2.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/dirkvdb/ffmpegthumbnailer.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0kl8aa547icy9b05njps02a8sw4yn4f8fzs228kig247sn09s4cp"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("ffmpeg" ,ffmpeg)
       ("libjpeg-turbo" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("gvfs" ,gvfs)))
    (arguments
     `(#:configure-flags (list "-DENABLE_GIO=ON" "-DENABLE_THUMBNAILER=ON")))
    (home-page "https://github.com/dirkvdb/ffmpegthumbnailer")
    (synopsis "Create thumbnails from video files")
    (description "FFmpegthumbnailer is a lightweight video thumbnailer that
can be used by file managers to create thumbnails for your video files.  The
thumbnailer uses ffmpeg to decode frames from the video files, so supported
videoformats depend on the configuration flags of ffmpeg.")
    (license license:gpl2+)))

(define-public vlc
  (package
    (name "vlc")
    (version "3.0.6")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://download.videolan.org/pub/videolan/vlc/"
                   (car (string-split version #\-))
                   "/vlc-" version ".tar.xz"))
             (sha256
              (base32
               "1lvyyahv6g9zv7m5g5qinyrwmw47zdsd5ysimb862j7kw15nvh8q"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("flex" ,flex)
       ("bison" ,bison)
       ("gettext" ,gettext-minimal)
       ("git" ,git) ; needed for a test
       ("pkg-config" ,pkg-config)))
    ;; FIXME: Add optional inputs once available.
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("avahi" ,avahi)
       ("dbus" ,dbus)
       ("eudev" ,eudev)
       ("flac" ,flac)
       ("ffmpeg" ,ffmpeg)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("fribidi" ,fribidi)
       ("gnutls" ,gnutls)
       ("liba52" ,liba52)
       ("libarchive" ,libarchive)
       ("libass" ,libass)
       ("libavc1394" ,libavc1394)
       ("libbluray" ,libbluray)
       ("libcaca" ,libcaca)
       ("libcddb" ,libcddb)
       ("libdca" ,libdca)
       ("libdvbpsi" ,libdvbpsi)
       ("libdvdnav" ,libdvdnav)
       ("libdvdread" ,libdvdread)
       ("libebml" ,libebml)
       ("libgcrypt" ,libgcrypt)
       ("libidn" ,libidn)
       ("libkate" ,libkate)
       ("libmad" ,libmad)
       ("libmatroska" ,libmatroska)
       ("libmicrodns" ,libmicrodns)
       ("libmodplug" ,libmodplug)
       ("libmpeg2" ,libmpeg2)
       ("libogg" ,libogg)
       ("libpng" ,libpng)
       ("libraw1394" ,libraw1394)
       ("librsvg" ,librsvg)
       ("libsamplerate" ,libsamplerate)
       ("libsecret" ,libsecret)
       ("libssh2" ,libssh2)
       ("libupnp" ,libupnp)
       ("libva" ,libva)
       ("libvdpau" ,libvdpau)
       ("libvorbis" ,libvorbis)
       ("libvpx" ,libvpx-1.7)
       ("libtheora" ,libtheora)
       ("libx264" ,libx264)
       ("libxext" ,libxext)
       ("libxi" ,libxi)
       ("libxinerama" ,libxinerama)
       ("libxml2" ,libxml2)
       ("libxpm" ,libxpm)
       ("livemedia-utils" ,livemedia-utils)
       ("lua" ,lua-5.2)
       ("mesa" ,mesa)
       ("opus" ,opus)
       ("perl" ,perl)
       ("pulseaudio" ,pulseaudio)
       ("protobuf" ,protobuf)
       ("python" ,python-wrapper)
       ("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)
       ("qtx11extras" ,qtx11extras)
       ("samba" ,samba)
       ("sdl" ,sdl)
       ("sdl-image" ,sdl-image)
       ("speex" ,speex)
       ("speexdsp" ,speexdsp)
       ("taglib" ,taglib)
       ("twolame" ,twolame)
       ("unzip" ,unzip)
       ("wayland" ,wayland)
       ("wayland-protocols" ,wayland-protocols)
       ("x265" ,x265)
       ("xcb-util-keysyms" ,xcb-util-keysyms)))
    (arguments
     `(#:configure-flags
       `("CXXFLAGS=-std=gnu++11"
         "BUILDCC=gcc"
         ,(string-append "LDFLAGS=-Wl,-rpath -Wl,"
                         (assoc-ref %build-inputs "ffmpeg")
                         "/lib"))                 ;needed for the tests

       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((livemedia-utils (assoc-ref inputs "livemedia-utils")))
               (substitute* "configure"
                 (("LIVE555_PREFIX=\\$\\{LIVE555_PREFIX-\"/usr\"\\}")
                  (string-append "LIVE555_PREFIX=" livemedia-utils)))
               ;; Some of the tests require using the display to test out VLC,
               ;; which fails in our sandboxed build system
               (substitute* "test/run_vlc.sh"
                 (("./vlc --ignore-config") "echo"))

               ;; modules/text_renderer/freetype/text_layout.c uses a
               ;; now-deprecated interface 'fribidi_get_par_embedding_levels'
               ;; from fribidi.h, so for now we enable the use of deprecated
               ;; fribidi interfaces from this file.
               ;; FIXME: Try removing this for vlc >= 3.0.3.
               (substitute* "modules/text_renderer/freetype/text_layout.c"
                 (("# define FRIBIDI_NO_DEPRECATED 1") ""))

               ;; Fix build with libssh2 > 1.8.0:
               ;; <https://trac.videolan.org/vlc/ticket/22060>
               ;; <https://git.videolan.org/?p=vlc.git;a=commit;h=11449b5cd8b415768e010d9b7c1d6ba3cea21f82>
               (substitute* "modules/access/sftp.c"
                 (("010801") "010900"))
               #t)))
         (add-after 'strip 'regenerate-plugin-cache
           (lambda* (#:key outputs #:allow-other-keys)
             ;; The 'install-exec-hook' rule in the top-level Makefile.am
             ;; generates 'lib/vlc/plugins/plugins.dat', a plugin cache, using
             ;; 'vlc-cache-gen'.  This file includes the mtime of the plugins
             ;; it references.  Thus, we first reset the timestamps of all
             ;; these files, and then regenerate the cache such that the
             ;; mtimes it includes are always zero instead of being dependent
             ;; on the build time.
             (let* ((out       (assoc-ref outputs "out"))
                    (pkglibdir (string-append out "/lib/vlc"))
                    (plugindir (string-append pkglibdir "/plugins"))
                    (cachegen  (string-append pkglibdir "/vlc-cache-gen")))
               ;; TODO: Factorize 'reset-timestamps'.
               (for-each (lambda (file)
                           (let ((s (lstat file)))
                             (unless (eq? (stat:type s) 'symlink)
                               (utime file 1 1))))
                         (find-files plugindir))
               (invoke cachegen plugindir))))
         (add-after 'install 'wrap-executable
           (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out"))
                  (plugin-path (getenv "QT_PLUGIN_PATH")))
              (wrap-program (string-append out "/bin/vlc")
                `("QT_PLUGIN_PATH" ":" prefix (,plugin-path))))
            #t)))))
    (home-page "https://www.videolan.org/")
    (synopsis "Audio and video framework")
    (description "VLC is a cross-platform multimedia player and framework
that plays most multimedia files as well as DVD, Audio CD, VCD, and various
streaming protocols.")
    (license license:gpl2+)))

(define-public mplayer
  (package
    (name "mplayer")
    (version "1.3.0")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://www.mplayerhq.hu/MPlayer/releases/MPlayer-"
                   version ".tar.xz"))
             (sha256
              (base32
               "0hwqn04bdknb2ic88xd75smffxx63scvz0zvwvjb56nqj9n89l1s"))))
    (build-system gnu-build-system)
    ;; FIXME: Add additional inputs once available.
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("cdparanoia" ,cdparanoia)
       ("ffmpeg" ,ffmpeg-3.4)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
;;        ("giflib" ,giflib) ; uses QuantizeBuffer, requires version >= 5
       ("lame" ,lame)
       ("libass" ,libass)
       ("libdvdcss" ,libdvdcss)
       ("libdvdnav" ,libdvdnav)
       ("libjpeg" ,libjpeg)
       ("libmpeg2" ,libmpeg2)
       ("libmpg123" ,mpg123)                      ; audio codec for MP3
       ("libpng" ,libpng)
       ("libtheora" ,libtheora)
       ("libvdpau" ,libvdpau)
       ("libvorbis" ,libvorbis)
       ("libx11" ,libx11)
       ("libx264" ,libx264)
       ("libxinerama" ,libxinerama)
       ("libxv" ,libxv)
       ("libxxf86dga" ,libxxf86dga)
       ("mesa" ,mesa)
       ("opus" ,opus)
       ("perl" ,perl)
       ("pulseaudio" ,pulseaudio)
       ("python" ,python-wrapper)
       ("sdl" ,sdl)
       ("speex" ,speex)
       ("yasm" ,yasm)
       ("zlib" ,zlib)))
    (arguments
     `(#:tests? #f ; no test target
       #:phases
       (modify-phases %standard-phases
        (replace 'configure
          ;; configure does not work followed by "SHELL=..." and
          ;; "CONFIG_SHELL=..."; set environment variables instead
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out"))
                  (libx11 (assoc-ref inputs "libx11")))
              (substitute* "configure"
                (("#! /bin/sh") (string-append "#!" (which "sh"))))
              (setenv "SHELL" (which "bash"))
              (setenv "CONFIG_SHELL" (which "bash"))
              (invoke "./configure"
                      (string-append "--extra-cflags=-I"
                                     libx11 "/include") ; to detect libx11
                      "--disable-ffmpeg_a" ; disables bundled ffmpeg
                      (string-append "--prefix=" out)
                      ;; Enable runtime cpu detection where supported,
                      ;; and choose a suitable target.
                      ,@(match (or (%current-target-system)
                                   (%current-system))
                          ("x86_64-linux"
                           '("--enable-runtime-cpudetection"
                             "--target=x86_64-linux"))
                          ("i686-linux"
                           '("--enable-runtime-cpudetection"
                             "--target=i686-linux"))
                          ("mips64el-linux"
                           '("--target=mips3-linux"))
                          (_ (list (string-append
                                    "--target="
                                    (or (%current-target-system)
                                        (nix-system->gnu-triplet
                                         (%current-system)))))))
                      "--disable-iwmmxt")))))))
    (home-page "https://www.mplayerhq.hu/design7/news.html")
    (synopsis "Audio and video player")
    (description "MPlayer is a movie player.  It plays most MPEG/VOB, AVI,
Ogg/OGM, VIVO, ASF/WMA/WMV, QT/MOV/MP4, RealMedia, Matroska, NUT,
NuppelVideo, FLI, YUV4MPEG, FILM, RoQ, PVA files.  One can watch VideoCD,
SVCD, DVD, 3ivx, DivX 3/4/5, WMV and H.264 movies.")
    (license license:gpl2)))

(define-public mpv
  (package
    (name "mpv")
    (version "0.29.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mpv-player/mpv.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "138921kx8g6qprim558xin09xximjhsj9ss8b71ifg2m6kclym8m"))))
    (build-system waf-build-system)
    (native-inputs
     `(("perl" ,perl) ; for zsh completion file
       ("pkg-config" ,pkg-config)
       ("python-docutils" ,python-docutils)))
    ;; Missing features: libguess, V4L2
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("enca" ,enca)
       ("ffmpeg" ,ffmpeg)
       ("jack" ,jack-1)
       ("ladspa" ,ladspa)
       ("lcms" ,lcms)
       ("libass" ,libass)
       ("libbluray" ,libbluray)
       ("libcaca" ,libcaca)
       ("libbs2b" ,libbs2b)
       ("libcdio-paranoia" ,libcdio-paranoia)
       ("libdvdread" ,libdvdread)
       ("libdvdnav" ,libdvdnav)
       ("libjpeg" ,libjpeg)
       ("libva" ,libva)
       ("libvdpau" ,libvdpau)
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxinerama" ,libxinerama)
       ("libxrandr" ,libxrandr)
       ("libxscrnsaver" ,libxscrnsaver)
       ("libxv" ,libxv)
       ;; XXX: lua > 5.2 is not currently supported; see
       ;; waftools/checks/custom.py
       ("lua" ,lua-5.2)
       ("mesa" ,mesa)
       ("mpg123" ,mpg123)
       ("pulseaudio" ,pulseaudio)
       ("rsound" ,rsound)
       ("shaderc" ,shaderc)
       ("vulkan-headers" ,vulkan-headers)
       ("vulkan-loader" ,vulkan-loader)
       ("waf" ,python-waf)
       ("wayland" ,wayland)
       ("wayland-protocols" ,wayland-protocols)
       ("libxkbcommon" ,libxkbcommon)
       ("youtube-dl" ,youtube-dl)
       ("zlib" ,zlib)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after
          'unpack 'patch-paths
          (lambda* (#:key inputs #:allow-other-keys)
            (let ((ytdl (assoc-ref inputs "youtube-dl")))
              (substitute* "player/lua/ytdl_hook.lua"
                (("\"youtube-dl\",")
                 (string-append "\"" ytdl "/bin/youtube-dl\",")))
              #t)))
         (add-before
          'configure 'setup-waf
          (lambda* (#:key inputs #:allow-other-keys)
            (let ((waf (assoc-ref inputs "waf")))
              (copy-file (string-append waf "/bin/waf") "waf"))
            (setenv "CC" "gcc")
            #t)))
       #:configure-flags (list "--enable-libmpv-shared"
                               "--enable-cdda"
                               "--enable-dvdread"
                               "--enable-dvdnav"
                               "--enable-zsh-comp"
                               "--disable-build-date")
       ;; No check function defined.
       #:tests? #f))
    (home-page "https://mpv.io/")
    (synopsis "Audio and video player")
    (description "mpv is a general-purpose audio and video player.  It is a
fork of mplayer2 and MPlayer.  It shares some features with the former
projects while introducing many more.")
    (license license:gpl2+)))

(define-public gnome-mpv
  (package
    (name "gnome-mpv")
    (version "0.16")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/gnome-mpv/gnome-mpv/releases"
                           "/download/v" version "/gnome-mpv-" version
                           ".tar.xz"))
       (sha256
        (base32
         "0jzdzvhcqp5jp1inwk2466zf7r8iimk3x69066gl8mzaay98mk92"))))
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gtk+" ,gtk+)
       ("libepoxy" ,libepoxy)
       ("mpv" ,mpv)))
    (build-system glib-or-gtk-build-system)
    (home-page "https://github.com/gnome-mpv/gnome-mpv")
    (synopsis "GTK+ frontend for the mpv media player")
    (description "GNOME MPV is a simple GTK+ frontend for the mpv media player.
GNOME MPV interacts with mpv via the client API exported by libmpv, allowing
access to mpv's powerful playback capabilities.")
    (license license:gpl3+)))

(define-public libvpx
  (package
    (name "libvpx")
    (version "1.8.0")
    (source (origin
              ;; XXX: Upstream does not provide tarballs for > 1.6.1.
              (method git-fetch)
              (uri (git-reference
                    (url "https://chromium.googlesource.com/webm/libvpx")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "079pb80am08lj8y5rx99vdr99mdqis9067f172zq12alkz849n93"))
              (patches (search-patches "libvpx-CVE-2016-2818.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list "--enable-shared"
                               "--as=yasm"
                               ;; Limit size to avoid CVE-2015-1258
                               "--size-limit=16384x16384"
                               (string-append "--prefix=" (assoc-ref %outputs "out")))
       #:make-flags  (list (string-append "LDFLAGS=-Wl,-rpath="
                                          (assoc-ref %outputs "out") "/lib"))
       #:phases (modify-phases %standard-phases
                  (replace 'configure
                    (lambda* (#:key configure-flags #:allow-other-keys)
                      ;; The configure script does not understand some of the GNU
                      ;; options, so we only add the flags specified above.
                      (apply invoke  "./configure" configure-flags))))
       #:tests? #f)) ; no check target
    (native-inputs
     `(("perl" ,perl)
       ("yasm" ,yasm)))
    (synopsis "VP8/VP9 video codec")
    (description "libvpx is a codec for the VP8/VP9 video compression format.")
    (license license:bsd-3)
    (home-page "https://www.webmproject.org/")))

;; GNU IceCat fails to build against 1.8.0, so keep this version for now.
(define-public libvpx-1.7
  (package
    (inherit libvpx)
    (version "1.7.0")
    (source (origin
              (inherit (package-source libvpx))
              (uri (git-reference
                    (url "https://chromium.googlesource.com/webm/libvpx")
                    (commit (string-append "v" version))))
              (file-name (git-file-name "libvpx" version))
              (sha256
               (base32
                "0vvh89hvp8qg9an9vcmwb7d9k3nixhxaz6zi65qdjnd0i56kkcz6"))
              (patches
               (append
                (origin-patches (package-source libvpx))
                (search-patches "libvpx-use-after-free-in-postproc.patch")))))))

(define-public youtube-dl
  (package
    (name "youtube-dl")
    (version "2019.04.24")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/rg3/youtube-dl/releases/"
                                  "download/" version "/youtube-dl-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1kzz3y2q6798mwn20i69imf48kb04gx3rznfl06hb8qv5zxm9gqz"))))
    (build-system python-build-system)
    (arguments
     ;; The problem here is that the directory for the man page and completion
     ;; files is relative, and for some reason, setup.py uses the
     ;; auto-detected sys.prefix instead of the user-defined "--prefix=FOO".
     ;; So, we need pass the prefix directly.  In addition, make sure the Bash
     ;; completion file is called 'youtube-dl' rather than
     ;; 'youtube-dl.bash-completion'.
     `(#:tests? #f ; Many tests fail. The test suite can be run with pytest.
       #:phases (modify-phases %standard-phases
                  (add-before 'install 'fix-the-data-directories
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((prefix (assoc-ref outputs "out")))
                        (mkdir "bash-completion")
                        (rename-file "youtube-dl.bash-completion"
                                     "bash-completion/youtube-dl")
                        (substitute* "setup.py"
                          (("youtube-dl\\.bash-completion")
                           "bash-completion/youtube-dl")
                          (("'etc/")
                           (string-append "'" prefix "/etc/"))
                          (("'share/")
                           (string-append "'" prefix "/share/")))
                        #t))))))
    (synopsis "Download videos from YouTube.com and other sites")
    (description
     "Youtube-dl is a small command-line program to download videos from
YouTube.com and many more sites.")
    (home-page "https://yt-dl.org")
    (license license:public-domain)))

(define-public youtube-dl-gui
  (package
    (name "youtube-dl-gui")
    (version "0.3.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Youtube-DLG" version))
       (sha256
        (base32
         "0napxwzgls5ik1bxbp99vly32l23xpc4ng5kr24hfhf21ypjyadb"))))
    (build-system python-build-system)
    (arguments
     ;; In Guix, wxpython has not yet been packaged for Python 3.
     `(#:python ,python-2
       ;; This package has no tests.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-source
           (lambda* (#:key inputs #:allow-other-keys)
             ;; The youtube-dl-gui program lets you configure options.  Some of
             ;; them are problematic, so we change their defaults.
             (substitute* "youtube_dl_gui/optionsmanager.py"
               ;; When this is true, the builder process will try (and fail) to
               ;; write logs to the builder user's home directory.
               (("'enable_log': True") "'enable_log': False")
               ;; This determines which youtube-dl program youtube-dl-gui will
               ;; run.  If we don't set this, then youtube-dl-gui might download
               ;; an arbitrary copy from the Internet into the user's home
               ;; directory and run it, so let's make sure youtube-dl-gui uses
               ;; the youtube-dl from the inputs by default.
               (("'youtubedl_path': self.config_path")
                (string-append "'youtubedl_path': '"
                               (assoc-ref inputs "youtube-dl")
                               "/bin'"))
               ;; When this is True, when youtube-dl-gui is finished downloading
               ;; a file, it will try (and possibly fail) to open the directory
               ;; containing the downloaded file.  This can fail because it
               ;; assumes that xdg-open is in PATH.  Unfortunately, simply
               ;; adding xdg-utils to the propagated inputs is not enough to
               ;; make this work, so for now we set the default to False.
               (("'open_dl_dir': True") "'open_dl_dir': False"))
             ;; The youtube-dl program from the inputs is actually a wrapper
             ;; script written in bash, so attempting to invoke it as a python
             ;; script will fail.
             (substitute* "youtube_dl_gui/downloaders.py"
               (("cmd = \\['python', self\\.youtubedl_path\\]")
                "cmd = [self.youtubedl_path]"))
             ;; Use relative paths for installing data files so youtube-dl-gui
             ;; installs the files relative to its prefix in the store, rather
             ;; than relative to /.  Also, instead of installing data files into
             ;; $prefix/usr/share, install them into $prefix/share for
             ;; consistency (see: (standards) Directory Variables).
             (substitute* "setup.py"
               (("= '/usr/share") "= 'share"))
             ;; Update get_locale_file() so it finds the installed localization
             ;; files.
             (substitute* "youtube_dl_gui/utils.py"
               (("os\\.path\\.join\\('/usr', 'share'")
                (string-append "os.path.join('"
                               (assoc-ref %outputs "out")
                               "', 'share'")))
             #t)))))
    (inputs
     `(("python2-wxpython" ,python2-wxpython)
       ("youtube-dl" ,youtube-dl)))
    (home-page "https://github.com/MrS0m30n3/youtube-dl-gui")
    (synopsis
     "GUI (Graphical User Interface) for @command{youtube-dl}")
    (description
     "Youtube-dlG is a GUI (Graphical User Interface) for
@command{youtube-dl}.  You can use it to download videos from YouTube and any
other site that youtube-dl supports.")
    (license license:unlicense)))

(define-public you-get
  (package
    (name "you-get")
    (version "0.4.1270")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/soimort/you-get.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "123g6x8sh32v4yn4ss55lfw7j79hgl3l6aiwgrk4ndq7dzhnz46q"))))
    (build-system python-build-system)
    (inputs
     `(("ffmpeg" ,ffmpeg)))             ; for multi-part and >=1080p videos
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'qualify-input-references
           ;; Explicitly invoke the input ffmpeg, instead of whichever one
           ;; happens to be in the user's $PATH at run time.
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((ffmpeg (string-append (assoc-ref inputs "ffmpeg")
                                          "/bin/ffmpeg")))
               (substitute* "src/you_get/processor/ffmpeg.py"
                 ;; Don't blindly replace all occurrences of ‘'ffmpeg'’: the
                 ;; same string is also used when sniffing ffmpeg's output.
                 (("(FFMPEG == |\\()'ffmpeg'" _ prefix)
                  (string-append prefix "'" ffmpeg "'")))
               #t))))
       #:tests? #f))                    ; XXX some tests need Internet access
    (synopsis "Download videos, audio, or images from Web sites")
    (description
     "You-Get is a command-line utility to download media contents (videos,
audio, images) from the Web.  It can use either mpv or vlc for playback.")
    (home-page "https://you-get.org/")
    (license license:expat)))

(define-public youtube-viewer
  (package
    (name "youtube-viewer")
    (version "3.5.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/trizen/youtube-viewer.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1j782m9rximybamd0qsc43hi7hgk333x9gy3ypzb61s0sifs0i6m"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)))
    ;; FIXME: Add optional dependencies once available:
    ;; perl-lwp-useragent-cached and perl-term-readline-gnu
    (inputs
     `(("perl-data-dump" ,perl-data-dump)
       ("perl-file-sharedir" ,perl-file-sharedir)
       ("perl-gtk2" ,perl-gtk2)
       ("perl-json" ,perl-json)
       ("perl-libwww" ,perl-libwww)
       ("perl-lwp-protocol-https" ,perl-lwp-protocol-https)
       ("perl-mozilla-ca" ,perl-mozilla-ca)
       ("perl-unicode-linebreak" ,perl-unicode-linebreak)))
    (arguments
     `(#:modules ((guix build perl-build-system)
                  (guix build utils)
                  (srfi srfi-26))
       #:module-build-flags '("--gtk")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-desktop
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (sharedir (string-append out "/share")))
               (install-file "share/gtk-youtube-viewer.desktop"
                             (string-append sharedir "/applications"))
               (install-file "share/icons/gtk-youtube-viewer.png"
                             (string-append sharedir "/pixmaps"))
               #t)))
         (add-after 'install 'wrap-program
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin-dir (string-append out "/bin/"))
                    (site-dir (string-append out "/lib/perl5/site_perl/"))
                    (lib-path (getenv "PERL5LIB")))
               (for-each (cut wrap-program <>
                              `("PERL5LIB" ":" prefix (,lib-path ,site-dir)))
                         (find-files bin-dir))
               #t))))))
    (synopsis
     "Lightweight application for searching and streaming videos from YouTube")
    (description
     "Youtube-viewer searches and plays YouTube videos in a native player.
It comes with various search options; it can search for videos, playlists
and/or channels.  The videos are streamed directly in a selected video player
at the best resolution (customizable) and with closed-captions (if available).
Both command-line and GTK2 interface are available.")
    (home-page "https://github.com/trizen/youtube-viewer")
    (license license:perl-license)))

(define-public libbluray
  (package
    (name "libbluray")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.videolan.org/videolan/"
                                  name "/" version "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1zxfnw1xbghcj7b3zz5djndv6gwssxda19cz1lrlqrkg8577r7kd"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--disable-bdjava-jar")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'refer-to-libxml2-in-.pc-file
           ;; Avoid the need to propagate libxml2 by referring to it
           ;; directly, as is already done for fontconfig & freetype.
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((libxml2 (assoc-ref inputs "libxml2")))
               (substitute* "configure"
                 ((" libxml-2.0") ""))
               (substitute* "src/libbluray.pc.in"
                 (("^Libs.private:" field)
                  (string-append field " -L" libxml2 "/lib -lxml2")))
               #t)))
         (add-before 'build 'fix-dlopen-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((libaacs (assoc-ref inputs "libaacs"))
                   (libbdplus (assoc-ref inputs "libbdplus")))
               (substitute* "src/libbluray/disc/aacs.c"
                 (("\"libaacs\"")
                  (string-append "\"" libaacs "/lib/libaacs\"")))
               (substitute* "src/libbluray/disc/bdplus.c"
                 (("\"libbdplus\"")
                  (string-append "\"" libbdplus "/lib/libbdplus\"")))
               #t))))))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs
     `(("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("libaacs" ,libaacs)
       ("libbdplus" ,libbdplus)
       ("libxml2" ,libxml2)))
    (home-page "https://www.videolan.org/developers/libbluray.html")
    (synopsis "Blu-Ray Disc playback library")
    (description
     "libbluray is a library designed for Blu-Ray Disc playback for media
players, like VLC or MPlayer.")
    (license license:lgpl2.1+)))

(define-public libdvdread
  (package
    (name "libdvdread")
    (version "6.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.videolan.org/videolan/"
                                  "libdvdread/" version "/"
                                  "libdvdread-" version ".tar.bz2"))
              (sha256
               (base32
                "1gfmh8ii3s2fw1c8vn57piwxc0smd3va4h7xgp9s8g48cc04zki8"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--with-libdvdcss=yes")))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("libdvdcss" ,libdvdcss)))
    (home-page "http://dvdnav.mplayerhq.hu/")
    (synopsis "Library for reading video DVDs")
    (description
     "Libdvdread provides a simple foundation for reading DVD video
disks.  It provides the functionality that is required to access many
DVDs.  It parses IFO files, reads NAV-blocks, and performs CSS
authentication and descrambling (if an external libdvdcss library is
installed).")
    (license license:gpl2+)))

(define-public dvdauthor
  (package
    (name "dvdauthor")
    (version "0.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/dvdauthor/dvdauthor-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1drfc47hikfzc9d7hjk34rw10iqw01d2vwmn91pv73ppx4nsj81h"))))
    (build-system gnu-build-system)
    (inputs
     `(("libdvdread" ,libdvdread)
       ("libpng" ,libpng)
       ("imagemagick" ,imagemagick)
       ("libxml2" ,libxml2)
       ("freetype" ,freetype)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (synopsis "Generates a DVD-Video movie from a MPEG-2 stream")
    (description "@command{dvdauthor} will generate a DVD-Video movie from a
MPEG-2 stream containing VOB packets.")
    (home-page "http://dvdauthor.sourceforge.net")
    (license license:gpl3+)))

(define-public libdvdnav
  (package
    (name "libdvdnav")
    (version "6.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.videolan.org/videolan/"
                                  name "/" version "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "062njcksmpgw9yv3737qkf93r2pzhaxi9szqjabpa8d010dp38ph"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libdvdread" ,libdvdread)))
    (home-page "http://dvdnav.mplayerhq.hu/")
    (synopsis "Library for video DVD navigation features")
    (description
     "Libdvdnav is a library for developers of multimedia
applications.  It allows easy use of sophisticated DVD navigation features
such as DVD menus, multiangle playback and even interactive DVD games.  All
this functionality is provided through a simple API which provides the DVD
playback as a single logical stream of blocks, intermitted by special
dvdnav events to report certain conditions.  The main usage of libdvdnav is
a loop regularly calling a function to get the next block, surrounded by
additional calls to tell the library of user interaction.  The whole
DVD virtual machine and internal playback states are completely
encapsulated.")
    (license license:gpl2+)))

(define-public libdvdcss
  (package
    (name "libdvdcss")
    (version "1.4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.videolan.org/pub/"
                                  name "/" version "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0x957zzpf4w2cp8zlk29prj8i2q6hay3lzdzsyz8y3cwxivyvhkq"))))
    (build-system gnu-build-system)
    (home-page "https://www.videolan.org/developers/libdvdcss.html")
    (synopsis "Library for accessing DVDs as block devices")
    (description
     "libdvdcss is a simple library designed for accessing DVDs like a block
device without having to bother about the decryption.")
    (license license:gpl2+)))

(define-public srt2vtt
  (package
    (name "srt2vtt")
    (version "0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://files.dthompson.us/srt2vtt/srt2vtt-"
                    version ".tar.gz"))
              (sha256
               (base32
                "16b377znjm6qlga5yb8aj7b7bcisa1ghcnj2lrb1d30lvxp4liif"))))
    (build-system gnu-build-system)
    (inputs
     `(("guile" ,guile-2.0)))
    (synopsis "SubRip to WebVTT subtitle converter")
    (description "srt2vtt converts SubRip formatted subtitles to WebVTT format
for use with HTML5 video.")
    (home-page "https://dthompson.us/projects/srt2vtt.html")
    (license license:gpl3+)))

(define-public avidemux
  (package
    (name "avidemux")
    (version "2.7.3")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://sourceforge/avidemux/avidemux/" version "/"
                   "avidemux_" version ".tar.gz"))
             (sha256
              (base32
               "17x2mnnr5h8pp764p55l1xcn2ljnzhbj8cykajlllvk4rc4qwxld"))
             (patches (search-patches "avidemux-install-to-lib.patch"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    ;; FIXME: Once packaged, add libraries not found during the build.
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("fribidi" ,fribidi)
       ("glu" ,glu)
       ("jack" ,jack-1)
       ("lame" ,lame)
       ("libva" ,libva)
       ("libvdpau" ,libvdpau)
       ("libvorbis" ,libvorbis)
       ("libvpx" ,libvpx)
       ("libxv" ,libxv)
       ("perl" ,perl)
       ("pulseaudio" ,pulseaudio)
       ("python" ,python-wrapper)
       ("qtbase" ,qtbase)
       ("qttools" ,qttools)
       ("sdl" ,sdl)
       ("sqlite" ,sqlite)
       ("yasm" ,yasm)
       ("zlib" ,zlib)))
    (arguments
     `(#:tests? #f                      ; no check target
       #:phases
       ;; Make sure files inside the included ffmpeg tarball are
       ;; patch-shebanged.
       (let ((ffmpeg "ffmpeg-4.1.1"))
         (modify-phases %standard-phases
           (add-before 'patch-source-shebangs 'unpack-ffmpeg
             (lambda _
               (with-directory-excursion "avidemux_core/ffmpeg_package"
                 (invoke "tar" "xf" (string-append ffmpeg ".tar.bz2"))
                 (delete-file (string-append ffmpeg ".tar.bz2")))
               #t))
           (add-after 'patch-source-shebangs 'repack-ffmpeg
             (lambda _
               (with-directory-excursion "avidemux_core/ffmpeg_package"
                 (substitute* (string-append ffmpeg "/configure")
                   (("#! /bin/sh") (string-append "#!" (which "sh"))))
                 (invoke "tar" "cjf" (string-append ffmpeg ".tar.bz2") ffmpeg
                         ;; avoid non-determinism in the archive
                         "--sort=name" "--mtime=@0"
                         "--owner=root:0" "--group=root:0")
                 (delete-file-recursively ffmpeg))
               #t))
           (replace 'configure
             (lambda _
               ;; Copy-paste settings from the cmake build system.
               (setenv "CMAKE_LIBRARY_PATH" (getenv "LIBRARY_PATH"))
               (setenv "CMAKE_INCLUDE_PATH" (getenv "C_INCLUDE_PATH"))
               #t))
           (replace 'build
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (lib (string-append out "/lib"))
                      (top (getcwd))
                      (sdl (assoc-ref inputs "sdl"))
                      (build_component
                       (lambda* (component srcdir #:optional (args '()))
                         (let ((builddir (string-append "build_" component)))
                           (mkdir builddir)
                           (with-directory-excursion builddir
                             (apply invoke "cmake"
                                    "-DCMAKE_INSTALL_RPATH_USE_LINK_PATH=TRUE"
                                    (string-append "-DCMAKE_INSTALL_PREFIX=" out)
                                    (string-append "-DCMAKE_INSTALL_RPATH=" lib)
                                    (string-append "-DCMAKE_SHARED_LINKER_FLAGS="
                                                   "\"-Wl,-rpath=" lib "\"")
                                    (string-append "-DAVIDEMUX_SOURCE_DIR=" top)
                                    (string-append "-DSDL_INCLUDE_DIR="
                                                   sdl "/include/SDL")
                                    (string-append "../" srcdir)
                                    "-DENABLE_QT5=True"
                                    args)
                             (invoke "make" "-j"
                                     (number->string (parallel-job-count)))
                             (invoke "make" "install"))))))
                 (mkdir out)
                 (build_component "core" "avidemux_core")
                 (build_component "cli" "avidemux/cli")
                 (build_component "qt4" "avidemux/qt4")
                 (build_component "plugins_common" "avidemux_plugins"
                                  '("-DPLUGIN_UI=COMMON"))
                 (build_component "plugins_cli" "avidemux_plugins"
                                  '("-DPLUGIN_UI=CLI"))
                 (build_component "plugins_qt4" "avidemux_plugins"
                                  '("-DPLUGIN_UI=QT4"))
                 (build_component "plugins_settings" "avidemux_plugins"
                                  '("-DPLUGIN_UI=SETTINGS"))
                 ;; Remove .exe and .dll file.
                 (delete-file-recursively
                  (string-append out "/share/ADM6_addons"))
                 #t)))
           (delete 'install)))))
    (home-page "http://fixounet.free.fr/avidemux/")
    (synopsis "Video editor")
    (description "Avidemux is a video editor designed for simple cutting,
filtering and encoding tasks.  It supports many file types, including AVI,
DVD compatible MPEG files, MP4 and ASF, using a variety of codecs.  Tasks
can be automated using projects, job queue and powerful scripting
capabilities.")
    (supported-systems '("x86_64-linux" "i686-linux" "armhf-linux"))
    ;; Software with various licenses is included, see License.txt.
    (license license:gpl2+)))

(define-public vapoursynth
  (package
    (name "vapoursynth")
    (version "37")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/vapoursynth/vapoursynth.git")
                    (commit (string-append "R" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ma2s7dxk6y6l04qj1jvgwia4xj7999ny3a1yx2vbk5l83giam2p"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("cython" ,python-cython)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("yasm" ,yasm)))
    (inputs
     `(("ffmpeg" ,ffmpeg)
       ("libass" ,libass)
       ("tesseract-ocr" ,tesseract-ocr)
       ("zimg" ,zimg)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'autogen
           (lambda _
             (invoke "sh" "autogen.sh"))))))
    (home-page "http://www.vapoursynth.com/")
    (synopsis "Video processing framework")
    (description "VapourSynth is a C++ library and Python module for video
manipulation.  It aims to be a modern rewrite of Avisynth, supporting
multithreading, generalized colorspaces, per frame properties, and videos with
format changes.")
    ;; src/core/cpufeatures only allows x86, ARM or PPC
    (supported-systems (fold delete %supported-systems
                             '("mips64el-linux" "aarch64-linux")))
    ;; As seen from the source files.
    (license license:lgpl2.1+)))

(define-public xvid
  (package
    (name "xvid")
    (version "1.3.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://downloads.xvid.org/downloads/xvidcore-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "1d0hy1w9sn6491a3vhyf3vmhq4xkn6yd4ralx1191s6qz5wz483w"))))
    (build-system gnu-build-system)
    (native-inputs `(("yasm" ,yasm)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before
          'configure 'pre-configure
          (lambda _
            (chdir "build/generic")
            (substitute* "configure"
              (("#! /bin/sh") (string-append "#!" (which "sh"))))
            #t)))
       ;; No 'check' target.
       #:tests? #f))
    (home-page "https://www.xvid.com/")
    (synopsis "MPEG-4 Part 2 Advanced Simple Profile video codec")
    (description "Xvid is an MPEG-4 Part 2 Advanced Simple Profile (ASP) video
codec library.  It uses ASP features such as b-frames, global and quarter
pixel motion compensation, lumi masking, trellis quantization, and H.263, MPEG
and custom quantization matrices.")
    (license license:gpl2+)))

(define-public streamlink
  (package
    (name "streamlink")
    (version "0.14.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "streamlink" version))
        (sha256
         (base32
          "0l2145fd60i76afjisfxd48cwhwyir07i7s3bnimdq5db2kzkix8"))
        (patches (search-patches "streamlink-update-test.patch"))))
    (build-system python-build-system)
    (home-page "https://github.com/streamlink/streamlink")
    (native-inputs
     `(("python-freezegun" ,python-freezegun)
       ("python-pytest" ,python-pytest)
       ("python-mock" ,python-mock)
       ("python-requests-mock" ,python-requests-mock)))
    (propagated-inputs
     `(("python-pysocks" ,python-pysocks)
       ("python-websocket-client" ,python-websocket-client)
       ("python-iso3166" ,python-iso3166)
       ("python-iso639" ,python-iso639)
       ("python-isodate" ,python-isodate)
       ("python-pycryptodome" ,python-pycryptodome)
       ("python-requests" ,python-requests)
       ("python-urllib3" ,python-urllib3)))
    (synopsis "Extract streams from various services")
    (description "Streamlink is command-line utility that extracts streams
from sites like Twitch.tv and pipes them into a video player of choice.")
    (license license:bsd-2)))

(define-public livestreamer
  (deprecated-package "livestreamer" streamlink))

(define-public twitchy
  (let ((commit "9beb36d80b16662414129693e74fa3a2fd97554e")) ; 3.4 has no tag
    (package
      (name "twitchy")
      (version (git-version "3.4" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/BasioMeusPuga/twitchy.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0di03h1j9ipp2bbnxxlxz07v87icyg2hmnsr4s7184z5ql8kpzr7"))))
      (build-system python-build-system)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "twitchy/twitchy_play.py"
                 (("\"streamlink ")
                  (string-append "\"" (assoc-ref inputs "streamlink")
                                 "/bin/streamlink ")))
               #t))
           (add-before 'check 'check-setup
             (lambda _
               (setenv "HOME" (getcwd)) ;Needs to write to ‘$HOME’.
               #t))
           (add-after 'install 'install-rofi-plugin
             (lambda* (#:key outputs #:allow-other-keys)
               (install-file "plugins/rofi-twitchy"
                             (string-append (assoc-ref outputs "out")
                                            "/bin"))
               #t)))))
      (inputs
       `(("python-requests" ,python-requests)
         ("streamlink" ,streamlink)))
      (home-page "https://github.com/BasioMeusPuga/twitchy")
      (synopsis "Command-line interface for Twitch.tv")
      (description
       "This package provides a command-line interface for Twitch.tv")
      (license license:gpl3+))))

(define-public mlt
  (package
    (name "mlt")
    (version "6.12.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mltframework/mlt.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0pzm3mjbbdl2rkbswgyfkx552xlxh2qrwzsi2a4dicfr92rfgq6w"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:make-flags '("CC=gcc" "CXX=g++ -std=gnu++11")
       #:configure-flags
       (list "--enable-gpl3"
             "--enable-gpl")
       #:phases
       (modify-phases %standard-phases
         (add-after
             'configure 'override-LDFLAGS
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "config.mak"
               (("LDFLAGS\\+=")
                (string-append "LDFLAGS+=-Wl,-rpath="
                               (assoc-ref outputs "out")
                               "/lib ")))
             #t)))))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("ffmpeg" ,ffmpeg-3.4)
       ("fftw" ,fftw)
       ("frei0r-plugins" ,frei0r-plugins)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("gtk+" ,gtk+-2)
       ("libxml2" ,libxml2)
       ("jack" ,jack-1)
       ("ladspa" ,ladspa)
       ("libsamplerate" ,libsamplerate)
       ("pulseaudio" ,pulseaudio)
       ("qtbase" ,qtbase)
       ("qtsvg" ,qtsvg)
       ("sdl" ,sdl)
       ("sox" ,sox)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://www.mltframework.org/")
    (synopsis "Author, manage, and run multitrack audio/video compositions")
    (description
     "MLT is a multimedia framework, designed and developed for television
broadcasting.  It provides a toolkit for broadcasters, video editors, media
players, transcoders, web streamers and many more types of applications.  The
functionality of the system is provided via an assortment of ready to use
tools, XML authoring components, and an extensible plug-in based API.")
    (license license:gpl3)))

(define-public v4l-utils
  (package
    (name "v4l-utils")
    (version "1.12.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://linuxtv.org/downloads/v4l-utils"
                                  "/v4l-utils-" version ".tar.bz2"))
              (sha256
               (base32
                "03g2b4rivrilimcp57mwrlsa3qvrxmk4sza08mygwmqbvcnic606"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list (string-append "--with-udevdir="
                            (assoc-ref %outputs "out")
                            "/lib/udev")
             "CXXFLAGS=-std=gnu++11")))
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("glu" ,glu)
       ("libjpeg" ,libjpeg)
       ("libx11" ,libx11)
       ("qtbase" ,qtbase)
       ("eudev" ,eudev)))
    (synopsis "Realtime video capture utilities for Linux")
    (description "The v4l-utils provide a series of libraries and utilities to
be used for realtime video capture via Linux-specific APIs.")
    (home-page "https://linuxtv.org/wiki/index.php/V4l-utils")
    ;; libv4l2 is LGPL2.1+, while utilities are GPL2 only.
    (license (list license:lgpl2.1+ license:gpl2))))

(define-public obs
  (package
    (name "obs")
    (version "23.0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/obsproject/obs-studio.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1c0a5vy4h3qwz69qw3bydyk7r651ib5a9jna4yj6c25p3p9isdvp"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ; no tests
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("curl" ,curl)
       ("eudev" ,eudev)
       ("ffmpeg" ,ffmpeg)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("jack" ,jack-1)
       ("jansson" ,jansson)
       ("libx264" ,libx264)
       ("libxcomposite" ,libxcomposite)
       ("mesa" ,mesa)
       ("pulseaudio" ,pulseaudio)
       ("qtbase" ,qtbase)
       ("qtx11extras" ,qtx11extras)
       ("speex" ,speex)
       ("v4l-utils" ,v4l-utils)
       ("zlib" ,zlib)))
    (synopsis "Live streaming software")
    (description "Open Broadcaster Software provides a graphical interface for
video recording and live streaming.  OBS supports capturing audio and video
from many input sources such as webcams, X11 (for screencasting), PulseAudio,
and JACK.")
    (home-page "https://obsproject.com")
    (supported-systems '("x86_64-linux" "i686-linux"))
    (license license:gpl2+)))

(define-public libvdpau
  (package
    (name "libvdpau")
    (version "1.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://gitlab.freedesktop.org/vdpau/libvdpau"
                            "/uploads/14b620084c027d546fa0b3f083b800c6/"
                            "libvdpau-" version ".tar.bz2"))
        (sha256
         (base32
          "01ps6g6p6q7j2mjm9vn44pmzq3g75mm7mdgmnhb1qkjjdwc9njba"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libx11" ,libx11 "out")
       ("libxext" ,libxext)
       ("xorgproto" ,xorgproto)))
    (home-page "https://wiki.freedesktop.org/www/Software/VDPAU/")
    (synopsis "Video Decode and Presentation API")
    (description "VDPAU is the Video Decode and Presentation API for UNIX.  It
provides an interface to video decode acceleration and presentation hardware
present in modern GPUs.")
    (license (license:x11-style "file://COPYING"))))

(define-public vdpauinfo
  (package
    (name "vdpauinfo")
    (version "1.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://secure.freedesktop.org/~aplattner/vdpau/"
                            name "-" version ".tar.gz"))
        (sha256
         (base32
          "1i2b0k9h8r0lnxlrkgqzmrjakgaw3f1ygqqwzx8w6676g85rcm20"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("libx11" ,libx11)))
    (propagated-inputs
     `(("libvdpau" ,libvdpau)))
    (home-page "https://wiki.freedesktop.org/www/Software/VDPAU/")
    (synopsis "Tool to query the capabilities of a VDPAU implementation")
    (description "Vdpauinfo is a tool to query the capabilities of a VDPAU
implementation.")
    (license (license:x11-style "file://COPYING"))))

(define-public libvdpau-va-gl
  (package
    (name "libvdpau-va-gl")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/i-rinat/libvdpau-va-gl/"
                            "releases/download/v" version "/libvdpau-va-gl-"
                            version ".tar.gz"))
        (sha256
         (base32
          "1x2ag1f2fwa4yh1g5spv99w9x1m33hbxlqwyhm205ssq0ra234bx"))
        (patches (search-patches "libvdpau-va-gl-unbundle.patch"))
        (modules '((guix build utils)))
        (snippet '(begin (delete-file-recursively "3rdparty")
                         #t))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f)) ; Tests require a running X11 server, with VA-API support.
    (native-inputs
     `(("libvdpau" ,libvdpau)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libva" ,libva)
       ("mesa" ,mesa)))
    (home-page "https://github.com/i-rinat/libvdpau-va-gl")
    (synopsis "VDPAU driver with VA-API/OpenGL backend")
    (description
     "Many applications can use VDPAU to accelerate portions of the video
decoding process and video post-processing to the GPU video hardware.  Since
there is no VDPAU available on Intel chips, they fall back to different drawing
techniques.  This driver uses OpenGL under the hood to accelerate drawing and
scaling and VA-API (if available) to accelerate video decoding.")
    (license license:expat)))

(define-public recordmydesktop
  (package
    (name "recordmydesktop")
    (version "0.3.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/" name "/" name "/"
                                  version "/recordmydesktop-" version ".tar.gz"))
              (sha256
               (base32
                "133kkl5j0r877d41bzj7kj0vf3xm8x80yyx2n8nqxrva304f58ik"))))
    (build-system gnu-build-system)
    (inputs `(("popt" ,popt)
              ("zlib" ,zlib)
              ("libx11" ,libx11)
              ("libice" ,libice)
              ("libsm" ,libsm)
              ("libxfixes" ,libxfixes)
              ("libxdamage" ,libxdamage)
              ("libxext" ,libxext)
              ("alsa-lib" ,alsa-lib)
              ("libvorbis" ,libvorbis)
              ("libtheora" ,libtheora)))
    (home-page "http://recordmydesktop.sourceforge.net/")
    (synopsis "Desktop session video recorder")
    (description
     "recordMyDesktop is a command-line tool that captures the activity in
your graphical desktop and encodes it as a video.  This is a useful tool for
making @dfn{screencasts}.")
    (license license:gpl2+)))

(define-public simplescreenrecorder
  (package
    (name "simplescreenrecorder")
    (version "0.3.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/MaartenBaert/ssr.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0n702dnv4qshgn3b90ixvplfafjhgz6040yir5vy8khjdpciysq4"))))
    (build-system cmake-build-system)
    ;; Although libx11, libxfixes, libxext are listed as build dependencies in
    ;; README.md, the program builds and functions properly without them.
    ;; As a result, they are omitted. Please add them back if problems appear.
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("ffmpeg" ,ffmpeg)
       ("glu" ,glu)
       ("jack" ,jack-1)
       ("libxi" ,libxi)
       ("pulseaudio" ,pulseaudio)
       ("qtbase" ,qtbase)
       ("qtx11extras" ,qtx11extras)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (arguments
     `(#:configure-flags
       (list "-DWITH_QT5=TRUE")
       #:tests? #f))                    ; no test suite
    ;; Using HTTPS causes part of the page to be displayed improperly.
    (home-page "http://www.maartenbaert.be/simplescreenrecorder/")
    (synopsis "Screen recorder")
    (description "SimpleScreenRecorder is an easy to use screen recorder with
a graphical user interface.  It supports recording the entire screen, or a
part of it, and allows encoding in many different codecs and file formats.
Other features include a live preview and live streaming.")
    (license (list license:gpl3+ ; most files
                   license:zlib ; glinject/elfhacks.*
                   license:isc ; glinject/*
                   license:x11)))) ; build-aux/install-sh

(define-public libsmpeg
  (package
    (name "libsmpeg")
    (version "0.4.5-401")
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url "svn://svn.icculus.org/smpeg/trunk/")
                    (revision 401))) ; last revision before smpeg2 (for SDL 2.0)
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "18yfkr70lr1x1hc8snn2ldnbzdcc7b64xmkqrfk8w59gpg7sl1xn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'autogen.sh
           (lambda _
             (invoke "sh" "autogen.sh"))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)))
    (inputs
     `(("sdl" ,sdl2)))
    (home-page "http://icculus.org/smpeg/")
    (synopsis "SDL MPEG decoding library")
    (description
     "SMPEG (SDL MPEG Player Library) is a free MPEG1 video player library
with sound support.  Video playback is based on the ubiquitous Berkeley MPEG
player, mpeg_play v2.2.  Audio is played through a slightly modified mpegsound
library, part of splay v0.8.2.  SMPEG supports MPEG audio (MP3), MPEG-1 video,
and MPEG system streams.")
    (license (list license:expat
                   license:lgpl2.1
                   license:lgpl2.1+
                   license:gpl2))))

;; for btanks
(define-public libsmpeg-with-sdl1
  (package (inherit libsmpeg)
    (name "libsmpeg")
    (version "0.4.5-399")
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url "svn://svn.icculus.org/smpeg/trunk/")
                    (revision 399))) ; tagged release 0.4.5
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0jfi085rf3fa5xsn0vd3nqf32my8ph9c6a9445y7a8lrlz4dms64"))))
    (inputs
     `(("sdl" ,sdl)))))

(define-public libbdplus
  (package
    (name "libbdplus")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://ftp.videolan.org/pub/videolan/libbdplus/"
                           version "/" name "-" version ".tar.bz2"))
       (sha256
        (base32 "02n87lysqn4kg2qk7d1ffrp96c44zkdlxdj0n16hbgrlrpiwlcd6"))))
    (inputs
     `(("libgcrypt" ,libgcrypt)))
    (build-system gnu-build-system)
    (home-page "https://www.videolan.org/developers/libbdplus.html")
    (synopsis "Library for decrypting certain Blu-Ray discs")
    (description "libbdplus is a library which implements the BD+ System
specifications.")
    (license license:lgpl2.1+)))

(define-public libaacs
  (package
    (name "libaacs")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://ftp.videolan.org/pub/videolan/libaacs/"
                           version "/" name "-" version ".tar.bz2"))
       (sha256
        (base32 "1kms92i0c7i1yl659kqjf19lm8172pnpik5lsxp19xphr74vvq27"))))
    (inputs
     `(("libgcrypt" ,libgcrypt)))
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)))
    (build-system gnu-build-system)
    (home-page "https://www.videolan.org/developers/libaacs.html")
    (synopsis "Library for decrypting certain Blu-Ray discs")
    (description "libaacs is a library which implements the Advanced Access
Content System specification.")
    (license license:lgpl2.1+)))

(define-public mps-youtube
  (package
    (name "mps-youtube")
    (version "0.2.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mps-youtube/mps-youtube.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1w1jhw9rg3dx7vp97cwrk5fymipkcy2wrbl1jaa38ivcjhqg596y"))))
    (build-system python-build-system)
    (arguments
     ;; Tests need to be disabled until #556 upstream is fixed. It reads as if the
     ;; test suite results differ depending on the country and also introduce
     ;; non-determinism in the tests.
     ;; https://github.com/mps-youtube/mps-youtube/issues/556
     `(#:tests? #f))
    (propagated-inputs
     `(("python-pafy" ,python-pafy)
       ("python-pygobject" ,python-pygobject))) ; For mpris2 support
    (home-page "https://github.com/mps-youtube/mps-youtube")
    (synopsis "Terminal based YouTube player and downloader")
    (description
     "@code{mps-youtube} is based on mps, a terminal based program to
search, stream and download music.  This implementation uses YouTube as
a source of content and can play and download video as well as audio.
It can use either mpv or mplayer for playback, and for conversion of
formats ffmpeg or libav is used.  Users should install one of the
supported players in addition to this package.")
    (license license:gpl3+)))

(define-public handbrake
  (package
    (name "handbrake")
    (version "1.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.handbrake.fr/releases/"
                                  version "/HandBrake-" version "-source.tar.bz2"))
              (sha256
               (base32
                "0bny0hwlr55g2c69rsamv0xvwmfh1s4a582b9vq20xv5ly84m6ms"))
              (modules '((guix build utils)))
              (snippet
               ;; Remove "contrib" and source not necessary for
               ;; building/running under a GNU environment.
               '(begin
                  (for-each delete-file-recursively
                            '("contrib" "macosx" "win"))
                  (substitute* "make/include/main.defs"
                    ;; Disable unconditional inclusion of "contrib" libraries
                    ;; (ffmpeg, libvpx, libdvdread, libdvdnav, and libbluray),
                    ;; which would lead to fetching and building of these
                    ;; libraries.  Use our own instead.
                    (("MODULES \\+= contrib") "# MODULES += contrib"))
                  #t))))
    (build-system  glib-or-gtk-build-system)
    (native-inputs
     `(("automake" ,automake)           ;gui subpackage must be bootstrapped
       ("autoconf" ,autoconf)
       ("curl" ,curl)                   ;not actually used, but tested for
       ("intltool" ,intltool)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)))           ;for configuration
    (inputs
     `(("bzip2" ,bzip2)
       ("dbus-glib" ,dbus-glib)
       ("ffmpeg" ,ffmpeg)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("glib" ,glib)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gtk+" ,gtk+)
       ("jansson" ,jansson)
       ("lame" ,lame)
       ("libass" ,libass)
       ("libbluray" ,libbluray)
       ("libdvdnav" ,libdvdnav)
       ("libdvdread" ,libdvdread)
       ("libgudev" ,libgudev)
       ("libmpeg2" ,libmpeg2)
       ("libnotify" ,libnotify)
       ("libogg" ,libogg)
       ("libopus" ,opus)
       ("libsamplerate" ,libsamplerate)
       ("libtheora" ,libtheora)
       ("libvorbis" ,libvorbis)
       ("libvpx" ,libvpx)
       ("libxml2" ,libxml2)
       ("libx264" ,libx264)
       ("x265" ,x265)
       ("zlib" ,zlib)))
    (arguments
     `(#:tests? #f             ;tests require Ruby and claim to be unsupported
       #:configure-flags
       (list (string-append "CPPFLAGS=-I"
                            (assoc-ref %build-inputs "libxml2")
                            "/include/libxml2")
             "LDFLAGS=-lx265")
       #:phases
       (modify-phases %standard-phases
         (replace 'bootstrap
           ;; Run bootstrap ahead of time so that shebangs get patched.
           (lambda _
             (setenv "CONFIG_SHELL" (which "sh"))
             (setenv "NOCONFIGURE" "1")
             ;; Patch the Makefile so that it doesn't bootstrap again.
             (substitute* "gtk/module.rules"
               ((".*autogen\\.sh.*") ""))
             (invoke "sh" "./gtk/autogen.sh")))
         (replace 'configure
           (lambda* (#:key outputs configure-flags #:allow-other-keys)
             ;; 'configure' is not an autoconf-generated script, and
             ;; errors on unrecognized arguments,
             ;; e.g. --enable-fast-install
             (let ((out (assoc-ref outputs "out")))
               (apply invoke "./configure"
                      (string-append "--prefix=" out)
                      (or configure-flags '())))))
         (add-after 'configure 'chdir-build
           (lambda _ (chdir "./build") #t)))))
    (home-page "https://handbrake.fr")
    (synopsis "Video transcoder")
    (description
     "HandBrake is a tool for converting video from any format to a selection
of modern, widely supported codecs.")
    ;; Some under GPLv2+, some under LGPLv2.1+, and portions under BSD3.
    ;; Combination under GPLv2.  See LICENSE.
    (license license:gpl2)))

(define-public openh264
  (package
    (name "openh264")
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/cisco/"
                                  name "/releases/download/v"
                                  version "/Source.Code.tar.gz.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0niha3wnn1jsndvz9vfwy2wyql8mp9j6v75vjsipy0idwan5yzgf"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("nasm" ,nasm)
       ("python" ,python)))
    (arguments
     '(#:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
                          "CC=gcc")
       #:test-target "test"
       #:phases (modify-phases %standard-phases
                  ;; no configure script
                  (delete 'configure))))
    (home-page "http://www.openh264.org/")
    (synopsis "H264 decoder library")
    (description
     "Openh264 is a library which can decode H264 video streams.")
    (license license:bsd-2)))

(define-public libmp4v2
  (package
    (name "libmp4v2")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       ;; XXX: The new location of upstream is uncertain and will become relevant the
       ;; moment when the googlecode archive shuts down. It is past the date it
       ;; should've been turned off. I tried to communicate with upstream, but this
       ;; wasn't very responsive and not very helpful. The short summary is, it is
       ;; chaos when it comes to the amount of forks and only time will tell where
       ;; the new upstream location is.
       (uri (string-append "https://storage.googleapis.com/google-"
                           "code-archive-downloads/v2/"
                           "code.google.com/mp4v2/mp4v2-" version ".tar.bz2"))
       (file-name (string-append name "-" version ".tar.bz2"))
       (sha256
        (base32
         "0f438bimimsvxjbdp4vsr8hjw2nwggmhaxgcw07g2z361fkbj683"))))
    (build-system gnu-build-system)
    (outputs '("out"
               "static")) ; 3.7MiB .a file
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-dates
           (lambda _
             ;; Make the build reproducible.
             (substitute* "configure"
               (("PROJECT_build=\"`date`\"") "PROJECT_build=\"\"")
               (("ac_abs_top_builddir=$ac_pwd") "ac_abs_top_builddir=\"\""))
             #t))
         (add-after 'install 'move-static-libraries
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Move static libraries to the "static" output.
             (let* ((out    (assoc-ref outputs "out"))
                    (lib    (string-append out "/lib"))
                    (static (assoc-ref outputs "static"))
                    (slib   (string-append static "/lib")))
               (mkdir-p slib)
               (for-each (lambda (file)
                           (install-file file slib)
                           (delete-file file))
                         (find-files lib "\\.a$"))
               #t))))))
    (native-inputs
     `(("help2man" ,help2man)
       ("dejagnu" ,dejagnu)))
    (home-page "https://code.google.com/archive/p/mp4v2/")
    (synopsis "API to create and modify mp4 files")
    (description
     "The MP4v2 library provides an API to create and modify mp4 files as defined by
ISO-IEC:14496-1:2001 MPEG-4 Systems.  This file format is derived from Apple's QuickTime
file format that has been used as a multimedia file format in a variety of platforms and
applications.  It is a very powerful and extensible format that can accommodate
practically any type of media.")
    (license license:mpl1.1)))

(define-public libmediainfo
  (package
    (name "libmediainfo")
    (version "0.7.95")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://mediaarea.net/download/source/"
                                  name "/" version"/"
                                  name "_" version ".tar.bz2"))
              (sha256
               (base32
                "1kchh6285b07z5nixv619hc9gml2ysdayicdiv30frrlqiyxqw4b"))))
    ;; TODO add a Big Buck Bunny webm for tests.
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("zlib" ,zlib)
       ("tinyxml2" ,tinyxml2)
       ("curl" ,curl)
       ("libzen" ,libzen)))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; see above TODO
       #:phases
       ;; build scripts not in root of archive
       (modify-phases %standard-phases
         (add-after 'unpack 'change-to-build-dir
           (lambda _
             (chdir "Project/GNU/Library")
             #t))
         (add-after 'change-to-build-dir 'autogen
           (lambda _
             (invoke "sh" "autogen.sh"))))))
    (home-page "https://mediaarea.net/en/MediaInfo")
    (synopsis "Library for retrieving media metadata")
    (description "MediaInfo is a library used for retrieving technical
information and other metadata about audio or video files.  A non-exhaustive
list of the information MediaInfo can retrieve from media files include:

@itemize
@item General: title, author, director, album, track number, date, duration...
@item Video: codec, aspect, fps, bitrate...
@item Audio: codec, sample rate, channels, language, bitrate...
@item Text: language of subtitle
@item Chapters: number of chapters, list of chapters
@end itemize

MediaInfo supports the following formats:

@itemize
@item Video: MKV, OGM, AVI, DivX, WMV, QuickTime, Real, MPEG-1,
MPEG-2, MPEG-4, DVD (VOB)...
@item Video Codecs: DivX, XviD, MSMPEG4, ASP, H.264, AVC...)
@item Audio: OGG, MP3, WAV, RA, AC3, DTS, AAC, M4A, AU, AIFF...
@item  Subtitles: SRT, SSA, ASS, SAMI...
@end itemize\n")
    (license license:bsd-2)))

;; TODO also have a GUI version available
(define-public mediainfo
  (package
    (name "mediainfo")
    (version "18.12")
    (source (origin
              (method url-fetch)
              ;; Warning: This source has proved unreliable 1 time at least.
              ;; Consider an alternate source or report upstream if this
              ;; happens again.
              (uri (string-append "https://mediaarea.net/download/source/"
                                  name "/" version "/"
                                  name "_" version ".tar.bz2"))
              (sha256
               (base32
                "1ix95ilcjlawcq6phh25cgplm3riqa2ii7ql82g8yagqs4ldqp6a"))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("zlib" ,zlib)
       ("libmediainfo" ,libmediainfo)
       ("libzen" ,libzen)))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; lacks tests
       #:phases
       ;; build scripts not in root of archive
       (modify-phases %standard-phases
         (add-after 'unpack 'change-to-build-dir
           (lambda _
             (chdir "Project/GNU/CLI")
             #t)))))
    (home-page "https://mediaarea.net/en/MediaInfo")
    (synopsis "Utility for reading media metadata")
    (description "MediaInfo is a utility used for retrieving technical
information and other metadata about audio or video files.  It supports the
many codecs and formats supported by libmediainfo.")
    (license license:bsd-2)))

(define-public livemedia-utils
  (package
    (name "livemedia-utils")
    (version "2019.03.06")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://download.videolan.org/contrib/live555/live."
                    version ".tar.gz"))
              (sha256
               (base32
                "1gasdl95yjabv811knkmy5laj21a54z1jdfq36jdj984k1nw5l0b"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; no tests
       #:make-flags (list "CC=gcc"
                          (string-append "LDFLAGS=-Wl,-rpath="
                                         (assoc-ref %outputs "out") "/lib")
                          (string-append "PREFIX="
                                         (assoc-ref %outputs "out")))
       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'fix-makefiles-generation
                    (lambda _
                      (substitute* "genMakefiles"
                        (("/bin/rm") "rm"))
                      #t))
                  (replace 'configure
                    (lambda _
                      (invoke "./genMakefiles"
                              "linux-with-shared-libraries"))))))
    (home-page "http://www.live555.com/liveMedia/")
    (synopsis "Set of C++ libraries for multimedia streaming")
    (description "This code forms a set of C++ libraries for multimedia
streaming, using open standard protocols (RTP/RTCP, RTSP, SIP).  The libraries
can be used to stream, receive, and process MPEG, H.265, H.264, H.263+, DV or
JPEG video, and several audio codecs.  They can easily be extended to support
additional (audio and/or video) codecs, and can also be used to build basic
RTSP or SIP clients and servers.")
    (license license:lgpl3+)))

(define-public libdvbpsi
  (package
    (name "libdvbpsi")
    (version "1.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://download.videolan.org/pub/libdvbpsi/"
                    version "/libdvbpsi-" version ".tar.bz2"))
              (sha256
               (base32
                "1zn5hfv4qbahmydbwh59a3b480s3m5ss27r6ml35gqdip7r3jkmc"))))
    (build-system gnu-build-system)
    (home-page "https://www.videolan.org/developers/libdvbpsi.html")
    (synopsis "Library for decoding and generation of MPEG TS and DVB PSI
tables")
    (description "libdvbpsi is a simple library designed for decoding and
generation of MPEG TS and DVB PSI tables according to standards ISO/IEC 13818s
and ITU-T H.222.0.")
    (license license:lgpl2.1)))

(define-public ffms2
  (package
    (name "ffms2")
    (version "2.23")
    (home-page "https://github.com/FFMS/ffms2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/FFMS/ffms2.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0dkz5b3gxq5p4xz0qqg6l2sigszrlsinz3skyf0ln4wf3zrvf8m5"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list "--enable-avresample")))
    (inputs
     `(("zlib" ,zlib)))
    (propagated-inputs
     `(("ffmpeg" ,ffmpeg)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (synopsis "Cross-platform wrapper around ffmpeg/libav")
    (description
      "FFMpegSource is a wrapper library around ffmpeg/libav that allows
programmers to access a standard API to open and decompress media files.")
    ;; sources are distributed under a different license that the binary.
    ;; see https://github.com/FFMS/ffms2/blob/master/COPYING
    (license license:gpl2+))); inherits from ffmpeg

(define-public aegisub
  (package
    (name "aegisub")
    (version "3.2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                     "http://ftp.aegisub.org/pub/archives/releases/source/"
                     name "-" version ".tar.xz"))
              (sha256
               (base32
                "11b83qazc8h0iidyj1rprnnjdivj1lpphvpa08y53n42bfa36pn5"))
              (patches (search-patches "aegisub-icu59-include-unistr.patch"
                                       "aegisub-boost68.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--disable-update-checker"
             "--without-portaudio"
             "--without-openal"
             "--without-oss"
             "CXXFLAGS=-DU_USING_ICU_NAMESPACE=1")
       ;; tests require busted, a lua package we don't have yet
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-ldflags
           (lambda _
             (setenv "LDFLAGS" "-pthread")
             #t))
         (add-after 'unpack 'fix-boost-headers
               (lambda _
                 (substitute*
                     '("src/subtitles_provider_libass.cpp"
                       "src/colour_button.cpp"
                       "src/video_provider_dummy.cpp"
                       "./src/video_frame.cpp")
                   (("#include <boost/gil/gil_all.hpp>")
                    "#include <boost/gil.hpp>"))
                 #t)))))
    (inputs
     `(("boost" ,boost)
       ("desktop-file-utils" ,desktop-file-utils)
       ("ffms2" ,ffms2)
       ("fftw" ,fftw)
       ("hunspell" ,hunspell)
       ("mesa" ,mesa)
       ("libass" ,libass)
       ("alsa-lib" ,alsa-lib)
       ("pulseaudio" ,pulseaudio)
       ("libx11" ,libx11)
       ("freetype" ,freetype)
       ("wxwidgets-gtk2" ,wxwidgets-gtk2)))
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (home-page "http://www.aegisub.org/")
    (synopsis "Subtitle engine")
    (description
      "Aegisub is a tool for creating and modifying subtitles.  Aegisub makes
it quick and easy to time subtitles to audio, and features many powerful
tools for styling them, including a built-in real-time video preview.")
    (license (list license:bsd-3 ; the package is licensed under the bsd-3, except
                   license:mpl1.1 ; for vendor/universalchardet under the mpl1.1
                   license:expat)))) ; and src/gl that is under a license similar
   ; the the Expat license, with a rewording (Software -> Materials). (called MIT
   ; by upstream). See https://github.com/Aegisub/Aegisub/blob/master/LICENCE
   ; src/MatroskaParser.(c|h) is under bsd-3 with permission from the author

(define-public gst-transcoder
  (package
    (name "gst-transcoder")
    (version "1.12.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pitivi/gst-transcoder.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0nw1zykqc6c8xs3ri55pm00pwyz93z4y4nd880apfiwj7yv5p3az"))))
    (build-system meson-build-system)
    (inputs
     `(("gobject-introspection" ,gobject-introspection)
       ("glib" ,glib)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)))
    (native-inputs
     `(("python" ,python)
       ("pkg-config" ,pkg-config)))
    (home-page "https://github.com/pitivi/gst-transcoder/")
    (synopsis "GStreamer Transcoding API")
    (description "GStreamer Transcoding API")
    (license license:lgpl2.1)))

(define-public gavl
  (package
    (name "gavl")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/gmerlin/"
                           name "/" version "/"
                           name "-" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1kikkn971a14zzm7svi7190ldc14fjai0xyhpbcmp48s750sraji"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("LIBS=-lm")))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("doxygen" ,doxygen)))
    (home-page "http://gmerlin.sourceforge.net")
    (synopsis "Low level library for multimedia API building")
    (description
     "Gavl is short for Gmerlin Audio Video Library.  It is a low level
library, upon which multimedia APIs can be built.  Gavl handles all the
details of audio and video formats like colorspaces, sample rates,
multichannel configurations, etc.  It provides standardized definitions for
those formats as well as container structures for carrying audio samples or
video images inside an application.

In addition, it handles the sometimes ugly task of converting between all
these formats and provides some elementary operations (copying, scaling,
alpha blending etc).")
    (license license:gpl3)))

(define-public frei0r-plugins
  (package
    (name "frei0r-plugins")
    (version "1.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://files.dyne.org/frei0r/"
                           "frei0r-plugins-" version ".tar.gz"))
       (sha256
        (base32
         "0pji26fpd0dqrx1akyhqi6729s394irl73dacnyxk58ijqq4dhp0"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'autotools
           (lambda _
             (invoke "sh" "autogen.sh"))))))
    ;; TODO: opencv for additional face detection filters.
    (inputs
     `(("gavl" ,gavl)
       ("cairo" ,cairo)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("libtool" ,libtool)
       ("automake" ,automake)
       ("autoconf" ,autoconf)))
    (home-page "https://www.dyne.org/software/frei0r/")
    (synopsis "Minimalistic plugin API for video effects")
    (description
     "Frei0r is a minimalistic plugin API for video effects.
The main emphasis is on simplicity for an API that will round up
the most common video effects into simple filters, sources and
mixers that can be controlled by parameters.  Frei0r wants to
provide a way to share these simple effects between many
applications, avoiding their reimplementation by different projects.
It counts more than 100 plugins.")
    (license (list license:gpl2+
                   ;; The following files are licensed as LGPL2.1+:
                   ;; src/generator/ising0r/ising0r.c
                   ;; src/generator/onecol0r/onecol0r.cpp
                   ;; src/generator/nois0r/nois0r.cpp
                   ;; src/generator/lissajous0r/lissajous0r.cpp
                   ;; src/filter/ndvi/gradientlut.hpp
                   ;; src/filter/ndvi/ndvi.cpp
                   ;; src/filter/facedetect/facedetect.cpp
                   license:lgpl2.1+))))

(define-public motion
  (package
    (name "motion")
    (version "4.2.2")
    (home-page "https://motion-project.github.io/")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Motion-Project/motion.git")
                    (commit (string-append "release-" version))))
              (sha256
               (base32
                "05c1gx75xy2hw49x6vkydvwxbr80kipsc3nr906k3hq8735svx6f"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf-wrapper)
       ("automake" ,automake)
       ("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libjpeg" ,libjpeg)
       ("ffmpeg" ,ffmpeg-3.4)
       ("libmicrohttpd" ,libmicrohttpd)
       ("sqlite" ,sqlite)))
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'bootstrap
                    (lambda _
                      (patch-shebang "version.sh")
                      (invoke "autoreconf" "-vfi"))))
       #:configure-flags '("--sysconfdir=/etc")
       #:make-flags (list (string-append "sysconfdir="
                                         (assoc-ref %outputs "out")
                                         "/etc"))

       #:tests? #f))                    ; no 'check' target
    (synopsis "Detect motion from video signals")
    (description
     "Motion is a program that monitors the video signal from one or more
cameras and is able to detect if a significant part of the picture has
changed.  Or in other words, it can detect motion.")

    ;; Some files say "version 2" and others "version 2 or later".
    (license license:gpl2)))

(define-public subdl
  (let ((commit "4cf5789b11f0ff3f863b704b336190bf968cd471")
        (revision "1"))
    (package
      (name "subdl")
      (version (git-version "1.0.3" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/alexanderwink/subdl.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0kmk5ck1j49q4ww0lvas2767kwnzhkq0vdwkmjypdx5zkxz73fn8"))))
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils))
         #:builder (begin
                     (use-modules (guix build utils))
                     (let* ((out (assoc-ref %outputs "out"))
                            (bin (string-append out "/bin"))
                            (source (assoc-ref %build-inputs "source"))
                            (python (assoc-ref %build-inputs "python")))
                       (install-file (string-append source "/subdl") bin)
                       (patch-shebang (string-append bin "/subdl")
                                      (list (string-append python "/bin")))))))
      (inputs `(("python" ,python)))
      (synopsis "Command-line tool for downloading subtitles from opensubtitles.org")
      (description "Subdl is a command-line tool for downloading subtitles from
opensubtitles.org.  By default, it will search for English subtitles, display
the results, download the highest-rated result in the requested language and
save it to the appropriate filename.")
      (license license:gpl3+)
      (home-page "https://github.com/alexanderwink/subdl"))))

(define-public l-smash
  (package
    (name "l-smash")
    (version "2.14.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/l-smash/l-smash.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0rcq9727im6kd8da8b7kzzbzxdldvmh5nsljj9pvr4m3lj484b02"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;no tests
       #:make-flags
       (list (string-append "LDFLAGS=-Wl,-L.,-rpath="
                            (assoc-ref %outputs "out") "/lib"))
       #:phases
       (modify-phases %standard-phases
         ;; configure fails if it is followed by CONFIG_SHELL
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke "./configure" (string-append "--prefix=" out)
                       "--disable-static")))))))
    (native-inputs
     `(("which" ,which)))
    (home-page "https://l-smash.github.io/l-smash/")
    (synopsis "MP4 multiplexer and demultiplexer library")
    (description
     "L-SMASH is a cross-platform library that handles the ISO base media file
format and some of its derived file formats, including MP4.  It operates as a
multiplexer and demultiplexer, and can mux video and audio in several formats
using standalone executable files.")
    (license license:isc)))

(define-public qtfaststart
  (package
    (name "qtfaststart")
    (version "1.8")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "qtfaststart" version))
              (sha256
               (base32
                "0hcjfik8hhb1syqvyh5c6aillpvzal26nkjflcq1270z64aj6i5h"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; no test suite
    (synopsis "Move QuickTime and MP4 metadata to the beginning of the file")
    (description "qtfaststart enables streaming and pseudo-streaming of
QuickTime and MP4 files by moving metadata and offset information to the
beginning of the file.  It can also print some useful information about the
structure of the file.  This program is based on qt-faststart.c from the FFmpeg
project, which is released into the public domain, as well as ISO 14496-12:2005
(the official spec for MP4), which can be obtained from the ISO or found
online.")
    (home-page "https://github.com/danielgtaylor/qtfaststart")
    (license license:expat)))

(define-public vidstab
  (package
    (name "vidstab")
    (version "1.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/georgmartius/vid.stab.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0a3frpm2kdbx7vszhg64p3alisag73bcspl7fp3a2f1kgq7rbh38"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f)) ; tests are not run as part of standard build process
    (home-page "http://public.hronopik.de/vid.stab/")
    (synopsis "Video stabilization library")
    (description "Vidstab is a video stabilization library which can be used
with FFmpeg.  A video acquired using a hand-held camera or a camera mounted on a
vehicle typically suffers from undesirable shakes and jitters.  Activities such
as surfing, skiing, riding and walking while shooting videos are especially
prone to erratic camera shakes.  Vidstab targets these video contents to help
create smoother and stable videos.")
    (license license:gpl2+)))

(define-public libopenshot
  (package
    (name "libopenshot")
    (version "0.2.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/OpenShot/libopenshot")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0r1qmr8ar5n72603xkj9h065vbpznrqsq88kxxmn9n8djyyvk03k"))
              (modules '((guix build utils)))
              (snippet '(begin
                          ;; Allow overriding of the python installation dir
                          (substitute* "src/bindings/python/CMakeLists.txt"
                            (("(SET\\(PYTHON_MODULE_PATH.*)\\)" _ set)
                             (string-append set " CACHE PATH "
                                            "\"Python bindings directory\")")))
                          (delete-file-recursively "thirdparty")
                          #t))))
    (build-system cmake-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python" ,python)
       ("swig" ,swig)
       ("unittest++" ,unittest-cpp)))
    (propagated-inputs                  ;all referenced in installed headers
     `(("cppzmq" ,cppzmq)
       ("ffmpeg" ,ffmpeg)
       ("imagemagick" ,imagemagick)
       ("jsoncpp" ,jsoncpp)
       ("libopenshot-audio" ,libopenshot-audio)
       ("qtbase" ,qtbase)
       ("qtmultimedia" ,qtmultimedia)
       ("zeromq" ,zeromq)))
    (arguments
     `(#:configure-flags
       (list (string-append "-DPYTHON_MODULE_PATH:PATH=" %output "/lib/python"
                            ,(version-major+minor (package-version python))
                            "/site-packages")
             "-DUSE_SYSTEM_JSONCPP:BOOL=ON")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-vars
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "LIBOPENSHOT_AUDIO_DIR"
                     (assoc-ref inputs "libopenshot-audio"))
             (setenv "ZMQDIR"
                     (assoc-ref inputs "zeromq"))
             (setenv "UNITTEST_DIR"
                     (string-append (assoc-ref inputs "unittest++")
                                    "/include/UnitTest++"))
             #t)))))
    (home-page "https://openshot.org")
    (synopsis "Video-editing, animation, and playback library")
    (description "OpenShot Library (libopenshot) is a powerful C++ video
editing library with a multi-threaded and feature rich video editing
API.  It includes bindings for Python, Ruby, and other languages.")
    (license license:lgpl3+)))

(define-public openshot
  (package
    (name "openshot")
    (version "2.4.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/OpenShot/openshot-qt")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0mg63v36h7l8kv2sgf6x8c1n3ygddkqqwlciz7ccxpbm4x1idqba"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (delete-file-recursively "src/images/fonts") #t))))
    (build-system python-build-system)
    (inputs
     `(("ffmpeg" ,ffmpeg)
       ("font-ubuntu" ,font-ubuntu)
       ("libopenshot" ,libopenshot)
       ("python" ,python)
       ("python-pyqt" ,python-pyqt)
       ("python-pyzmq" ,python-pyzmq)
       ("python-requests" ,python-requests)
       ("qtsvg" ,qtsvg)))
    (arguments
     `(#:tests? #f                      ;no tests
       #:modules ((guix build python-build-system)
                  (guix build qt-utils)
                  (guix build utils))
       #:imported-modules (,@%python-build-system-modules
                            (guix build qt-utils))
       #:phases (modify-phases %standard-phases
                  (delete 'build)       ;install phase does all the work
                  (add-after 'unpack 'patch-font-location
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((font (assoc-ref inputs "font-ubuntu")))
                        (substitute* "src/classes/app.py"
                          (("info.IMAGES_PATH") (string-append "\"" font "\""))
                          (("fonts") "share/fonts/truetype")))
                      #t))
                  (add-before 'install 'set-tmp-home
                    (lambda _
                      ;; src/classes/info.py "needs" to create several
                      ;; directories in $HOME when loaded during build
                      (setenv "HOME" "/tmp")
                      #t))
                  (add-after 'install 'wrap-program
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (wrap-qt-program out "openshot-qt"))
                      #t)))))
    (home-page "https://openshot.org")
    (synopsis "Video editor")
    (description "OpenShot takes your videos, photos, and music files and
helps you create the film you have always dreamed of.  Easily add sub-titles,
transitions, and effects and then export your film to many common formats.")
    (license license:gpl3+)))

(define-public dav1d
  (package
    (name "dav1d")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (list ;; The canonical download site.
                   (string-append "https://downloads.videolan.org/pub/videolan/"
                                  "dav1d/" version "/dav1d-" version ".tar.xz")

                   ;; Auto-generated tarballs from the Git repo?
                   (string-append "https://code.videolan.org/videolan/dav1d/-/"
                                  "archive/" version "/dav1d-" version ".tar.bz2")))
        (sha256
         (base32 "1llf4v486avj83d31670vdd5nshbq10qrx9vwrm1j078dh4ax4q0"))))
    (build-system meson-build-system)
    (native-inputs `(("nasm" ,nasm)))
    (home-page "https://code.videolan.org/videolan/dav1d")
    (synopsis "AV1 decoder")
    (description "dav1d is a new AV1 cross-platform decoder, and focused on
speed and correctness.")
    (license license:bsd-2)))

(define-public wlstream
  (let ((commit "182076a94562b128c3a97ecc53cc68905ea86838")
        (revision "1"))
    (package
      (name "wlstream")
      (version (git-version "0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/atomnuker/wlstream.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "01qbcgfl3g9kfwn1jf1z9pdj3bvf5lmg71d1vwkcllc2az24bjqp"))))
      (build-system meson-build-system)
      (native-inputs `(("libdrm" ,libdrm)
                       ("pkg-config" ,pkg-config)))
      (inputs `(("ffmpeg" ,ffmpeg)
                ("pulseaudio" ,pulseaudio)
                ("wayland" ,wayland)
                ("wayland-protocols" ,wayland-protocols)))
      (home-page "https://github.com/atomnuker/wlstream")
      (synopsis "Screen capture tool for Wayland sessions")
      (description "Wlstream is a screen capture tool for recording audio and
video from a Wayland session.")
      (license license:lgpl2.1+))))

(define-public gaupol
  (package
    (name "gaupol")
    (version "1.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/otsaloma/gaupol/")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0dk44fmcs86ymfxfbpdbrr4x5nn5hnv57wkqjyw61g779xjhlrd2"))))
    (build-system python-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("python-pygobject" ,python-pygobject)
       ("gtk+" ,gtk+)
       ("python-pycairo" ,python-pycairo) ; Required or else clicking on a subtitle line fails.
       ("python-chardet" ,python-chardet) ; Optional: Character encoding detection.
       ("gtkspell3" ,gtkspell3)           ; Optional: Inline spell-checking.
       ("iso-codes" ,iso-codes)           ; Optional: Translations.
       ("gstreamer" ,gstreamer)
       ("gst-libav" ,gst-libav)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gst-plugins-good" ,gst-plugins-good)
       ("gst-plugins-bad" ,gst-plugins-bad)
       ("gst-plugins-ugly" ,gst-plugins-ugly)))
    (arguments
     `(#:tests? #f                      ; Tests seem to require networking.
       #:phases
       (modify-phases %standard-phases
         ;; gaupol's setup.py script does not support one of the Python build
         ;; system's default flags, "--single-version-externally-managed".
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "python" "setup.py" "install"
                     (string-append "--prefix=" (assoc-ref outputs "out"))
                     "--root=/")))
         (add-after 'install 'wrap-gaupol
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (gst-plugin-path (getenv "GST_PLUGIN_SYSTEM_PATH"))
                   (gi-typelib-path (getenv "GI_TYPELIB_PATH")))
               (wrap-program (string-append out "/bin/gaupol")
                 `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,gst-plugin-path))
                 `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))))
             #t))
         (add-after 'unpack 'patch-data-dir
           ;; Fix some path variables that setup.py seems to garble.
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "setup.py"
                 (("DATA_DIR = \\{!r\\}\"\\.format\\(data_dir\\)")
                  (string-append "DATA_DIR = '" out "/share/gaupol'\""))
                 (("LOCALE_DIR = \\{!r\\}\"\\.format\\(locale_dir\\)")
                  (string-append "LOCALE_DIR = '" out "/share/locale'\"")))
               #t))))))
    (synopsis "Editor for text-based subtitles")
    (description
     "Gaupol supports multiple subtitle file formats and provides means of
creating subtitles, editing texts and timing subtitles to match video.  The
user interface features a builtin video player and is designed with attention
to convenience of translating and batch processing of multiple documents.")
    (home-page "https://otsaloma.io/gaupol/")
    (license license:gpl3+)))
