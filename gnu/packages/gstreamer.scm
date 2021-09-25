;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2015, 2016 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2018 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
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

(define-module (gnu packages gstreamer)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system trivial)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages java)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ocr)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages speech)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages telephony)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages xml))

(define-public openni2
  (package
    (name "openni2")
    (version "2.2.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/occipital/OpenNI2")
         (commit (string-append "v" version "-debian"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mfnyzpq53wnzgjfx91xcbx0nrl0lp1vrk1rk20a3gb3kshsr675"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; No target
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     `(("graphviz" ,graphviz)
       ("doxygen" ,doxygen)
       ("openjdk" ,openjdk14)
       ("openjdk:jdk" ,openjdk14 "jdk")
       ("python" ,python-wrapper)))
    (inputs
     `(("freeglut3" ,freeglut)
       ("libudev" ,eudev)
       ("libusb" ,libusb)))
    (synopsis "Framework for sensor-based 'Natural Interaction")
    (description "OpenNI is a framework for getting data to support
'Natural Interaction', i.e. skeleton tracking, gesture tracking, and similar
ways of getting data from humans.  It provides the interface for physical devices
and for middleware components.")
    (home-page "https://structure.io/openni")
    (license license:asl2.0)))

(define-public libdc1394
  (package
    (name "libdc1394")
    (version "2.2.6")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://sourceforge.net/projects/" name "/files/"
                              name "-2" "/" version "/" name "-" version ".tar.gz"))
              (sha256
               (base32 "1v8gq54n1pg8izn7s15yylwjf8r1l1dmzbm2yvf6pv2fmb4mz41b"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("doxygen" ,doxygen)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("glu" ,glu)
       ("libraw1394" ,libraw1394)
       ("libusb" ,libusb)
       ("libxv" ,libxv)
       ("linux-headers" ,linux-libre-headers)
       ("mesa" ,mesa)
       ("sdl" ,sdl)
       ("v4l" ,v4l-utils)))
    (synopsis "1394-Based Digital Camera Control Library")
    (description "LibDC1394 is a library that provides functionality to control
any camera that conforms to the 1394-Based Digital Camera Specification written
by the 1394 Trade Association.  It utilizes the lowlevel functionality provided
by libraw1394 to communicate with the camera.  It also uses the video1394 kernel
module for the DMA capture of the video flow.")
    (home-page "https://damien.douxchamps.net/ieee1394/libdc1394/")
    (license license:lgpl2.0+)))

(define-public ccextractor
  (package
    (name "ccextractor")
    (version "0.88")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/CCExtractor/ccextractor")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sya45hvv4d46bk7541yimmafgvgyhkpsvwfz9kv6pm4yi1lz6nb"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; No target
       #:configure-flags
       (list
        "-DWITH_FFMPEG=ON"
        "-DWITH_OCR=ON"
        "-DWITH_SHARING=ON"
        "-DWITH_HARDSUBX=ON")
       #:phases
       (modify-phases %standard-phases
         ;; The package is in a sub-dir of this repo.
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "src")
             #t))
         (add-after 'chdir 'fix-build-errors
           (lambda _
             (substitute* "CMakeLists.txt"
               (("libnanomsg")
                "nanomsg"))
             #t)))))
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)))
    (inputs
     `(("ffmeg" ,ffmpeg-3.4)
       ("nanomsg" ,nanomsg)
       ("leptonica" ,leptonica)
       ("ocr" ,tesseract-ocr)
       ("zlib" ,zlib)))
    (synopsis "Closed Caption Extractor")
    (description "CCExtractor is a tool that analyzes video files and produces
independent subtitle files from the closed captions data.  It is portable, small,
and very fast.")
    (home-page "https://www.ccextractor.org/")
    (license license:gpl2+)))

(define-public libvisual
  (package
    (name "libvisual")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/Libvisual/libvisual")
         (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02xwakwkqjsznc03pjlb6hcv1li1gw3r8xvyswqsm4msix5xq18a"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; The package is in a sub-dir of this repo.
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "libvisual")
             #t)))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("libintl" ,intltool)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("sdl" ,sdl)))
    (native-search-paths
     (list
      (search-path-specification
       (variable "LIBVISUAL_PLUGINS_BASE_DIR")
       (files '("lib/libvisual-0.4")))))
    ;; To load libvisual-plugins.
    (search-paths native-search-paths)
    (synopsis "Audio visualisation library")
    (description "Libvisual is a library that acts as a middle layer between
applications that want audio visualisation and audio visualisation plugins.")
    (home-page "http://libvisual.org/")
    (license
     (list
      ;; Libraries.
      license:lgpl2.1+
      ;; Examples and Tests.
      license:gpl2+))))

(define-public libvisual-plugins
  (package
    (name "libvisual-plugins")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/Libvisual/libvisual")
         (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02xwakwkqjsznc03pjlb6hcv1li1gw3r8xvyswqsm4msix5xq18a"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list
        "--disable-gstreamer-plugin"
        "--disable-corona"
        "--disable-gforce"
        (string-append "--with-plugins-base-dir=" (assoc-ref %outputs "out")
                       "/lib/libvisual-0.4"))
       #:phases
       (modify-phases %standard-phases
         ;; The package is in a sub-dir of this repo.
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "libvisual-plugins")
             #t)))))
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)
       ("gettext" ,gettext-minimal)
       ("libintl" ,intltool)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("esound" ,esound)
       ("gdk-pixbuf" ,gdk-pixbuf+svg)
       ("gtk+" ,gtk+-2)
       ("jack" ,jack-2)
       ("libx11" ,libx11)
       ("libxext" ,libxext)))
    (propagated-inputs
     `(("libvisual" ,libvisual)))
    (synopsis "Audio visualisation library")
    (description "Libvisual is a library that acts as a middle layer between
applications that want audio visualisation and audio visualisation plugins.")
    (home-page "http://libvisual.org/")
    (license license:gpl2+)))

(define-public esound
  (package
    (name "esound")
    (version "0.2.41")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://gitlab.gnome.org/Archive/esound.git")
         (commit "ESOUND_0_2_41")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "141jg70fim276i8k2kyypm84gy89i1k9mm4yf68mfwnybvjw1d6n"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext" ,gettext-minimal)
       ("gnome-common" ,gnome-common)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("tcsh" ,tcsh)                      ; for the tests
       ("which" ,which)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("pcaudiolib" ,pcaudiolib)
       ("tcp-wrappers" ,tcp-wrappers)))
    (propagated-inputs
     `(("audiofile" ,audiofile)))
    (synopsis "Enlightened Sound Daemon")
    (description "The Enlightened Sound Daemon mixes several audio streams for
playback by a single audio device.  You can also pre-load samples, and play them
back without having to send all the data for the sound.  Network transparency is
also built in, so you can play sounds on one machine, and listen to them on
another.")
    (home-page "https://web.archive.org/web/20160528230227/
http://www.tux.org/~ricdude/overview.html")
    (license
     (list
      ;; Libraries.
      license:lgpl2.0+
      ;; Others.
      license:gpl2+))))

(define-public orc
  (package
    (name "orc")
    (version "0.4.32")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gstreamer.freedesktop.org/data/src/"
                                  "orc/orc-" version ".tar.xz"))
              (sha256
               (base32
                "1w0qmyj3v9sb2g7ff39pp38b9850y9hyy0bag26ifrby5f7ksvm6"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-faulty-test
           (lambda _
             ;; XXX Disable the 'test-limits' and 'exec_opcodes_sys'
             ;; tests, which fail on some machines.  See:
             ;; https://bugzilla.gnome.org/show_bug.cgi?id=735273
             (substitute* '("testsuite/test-limits.c"
                            "testsuite/exec_opcodes_sys.c")
               (("if \\(error\\) return 1;")
                "if (error) return 77;"))
             #t)))))
    (native-inputs
     `(("gtk-doc" ,gtk-doc/stable)))
    (home-page "https://gstreamer.freedesktop.org/modules/orc.html")
    (synopsis "Oil runtime compiler")
    (description
     "Orc is a just-in-time compiler implemented as a library and set of
associated tools for compiling and executing simple programs that operate on
arrays of data.")
    ;; The source code implementing the Marsenne Twister algorithm is licensed
    ;; under the 3-clause BSD license, the rest is under 2-clause BSD license.
    (license (list license:bsd-2 license:bsd-3))))

(define-public gstreamer-docs
  (package
    (name "gstreamer-docs")
    (version "1.18.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://gstreamer.freedesktop.org/src/gstreamer-docs"
                    "/gstreamer-docs-" version ".tar.xz"))
              (sha256
               (base32
                "07hrgn11ll16yahyyh5684k8ms1j9npsyb8lj0skwbapin4czshm"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules ((guix build utils)))
         (let* ((source (assoc-ref %build-inputs "source"))
                (tar (assoc-ref %build-inputs "tar"))
                (xz (assoc-ref %build-inputs "xz"))
                (out (assoc-ref %outputs "out"))
                (books (string-append out "/share/devhelp/books")))
           (setenv "PATH" (string-append xz "/bin"))
           (mkdir-p books)
           (with-directory-excursion books
             (invoke (string-append tar "/bin/tar") "-xvf" source
                     "--strip-components=3"
                     (string-append ,name "-" ,version
                                    "/devhelp/books/GStreamer")))
           #t))))
    (native-inputs
     `(("tar" ,tar)
       ("xz" ,xz)))
    (home-page "https://gstreamer.freedesktop.org/")
    (synopsis "Developer documentation for GStreamer")
    (description
     "This package contains manuals, tutorials, and API reference for
the GStreamer multimedia framework.")
    ;; The documentation is covered by multiple licenses.  Anything not
    ;; explicitly mentioned below is LGPL2.1+.  See README.md for details.
    (license (list
              ;; The tutorial code can be used with either of these licenses,
              ;; at the users option.
              license:lgpl2.1+ license:bsd-2 license:expat
              ;; The developer manual and plugin writer guide carries
              ;; the Open Publication License v1.0.
              (license:fsf-free "https://opencontent.org/openpub/"
                                "The Open Publication License v1.0")
              ;; Tutorials are covered by CC-BY-SA 4.0.
              license:cc-by-sa4.0))))

;; Increase the test timeouts to accommodate slow or busy machines.
(define %common-gstreamer-phases
  '((add-after 'unpack 'increase-test-timeout
      (lambda _
        (substitute* "tests/check/meson.build"
          (("'CK_DEFAULT_TIMEOUT', '[0-9]*'")
           "'CK_DEFAULT_TIMEOUT', '600'")
          (("timeout ?: .*\\)")
           "timeout: 90 * 60)"))
        #t))))

(define-public gstreamer
  (package
    (name "gstreamer")
    (version "1.18.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "https://gstreamer.freedesktop.org/src/gstreamer/gstreamer-"
            version ".tar.xz"))
      (sha256
       (base32
        "0ijlmvr660m8zn09xlmnq1ajrziqsivp2hig5a9mabhcjx7ypkb6"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ,@%common-gstreamer-phases
         ;; FIXME: Since switching to the meson-build-system, two tests
         ;; started failing on i686.  See
         ;; <https://gitlab.freedesktop.org/gstreamer/gstreamer/issues/499>.
         ,@(if (string-prefix? "i686" (or (%current-target-system)
                                          (%current-system)))
               `((add-after 'unpack 'disable-some-tests
                   (lambda _
                     (substitute* "tests/check/gst/gstsystemclock.c"
                       (("tcase_add_test \\(tc_chain, test_stress_cleanup_unschedule.*")
                        "")
                       (("tcase_add_test \\(tc_chain, test_stress_reschedule.*")
                      ""))
                     #t)))
               '()))))
    (propagated-inputs `(("glib" ,glib))) ; required by gstreamer-1.0.pc.
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)
       ("glib" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python-wrapper" ,python-wrapper)))
    (native-search-paths
     (list (search-path-specification
            (variable "GST_PLUGIN_SYSTEM_PATH")
            (files '("lib/gstreamer-1.0")))))
    (home-page "https://gstreamer.freedesktop.org/")
    (synopsis "Multimedia library")
    (description
     "GStreamer is a library for constructing graphs of media-handling
components.  The applications it supports range from simple Ogg/Vorbis
playback, audio/video streaming to complex audio mixing and video
non-linear editing.

Applications can take advantage of advances in codec and filter technology
transparently.  Developers can add new codecs and filters by writing a
simple plugin with a clean, generic interface.

This package provides the core library and elements.")
    (license license:lgpl2.0+)))

(define-public gst-plugins-base
  (package
    (name "gst-plugins-base")
    (version "1.18.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://gstreamer.freedesktop.org/src/" name "/"
                          name "-" version ".tar.xz"))
      (patches (search-patches "gst-plugins-base-fix-id3v2-invalid-read.patch"))
      (sha256
       (base32
        "1b05kg46azrxxvq42c71071lfsnc34pw4vynnkczdqi6g0gzn16x"))))
    (build-system meson-build-system)
    (propagated-inputs
     `(("glib" ,glib)              ;required by gstreamer-sdp-1.0.pc
       ("gstreamer" ,gstreamer)    ;required by gstreamer-plugins-base-1.0.pc

       ;; XXX: Do not enable Orc optimizations on ARM systems because
       ;; it leads to two test failures.
       ;; https://gitlab.freedesktop.org/gstreamer/gst-plugins-base/issues/683
       ,@(if (string-prefix? "arm" (or (%current-target-system)
                                       (%current-system)))
             '()
             `(("orc" ,orc)))))         ;required by gstreamer-audio-1.0.pc
    (inputs
     `(("cdparanoia" ,cdparanoia)
       ("pango" ,pango)
       ("libogg" ,libogg)
       ("libtheora" ,libtheora)
       ("libvorbis" ,libvorbis)
       ("libx11" ,libx11)
       ("zlib" ,zlib)
       ("libXext" ,libxext)
       ("libxv" ,libxv)
       ("alsa-lib" ,alsa-lib)
       ;; XXX Don't build with opus on 32-bit systems:
       ;; <https://bugs.gnu.org/32360>
       ,@(if (target-64bit?)
             `(("opus" ,opus))
             '())))
    (native-inputs
      `(("pkg-config" ,pkg-config)
        ("glib:bin" ,glib "bin")
        ("gobject-introspection" ,gobject-introspection)
        ("python-wrapper" ,python-wrapper)))
    (arguments
     `(#:configure-flags '("-Dgl=disabled")
       #:phases
       (modify-phases %standard-phases
         ,@%common-gstreamer-phases
         (add-before 'configure 'patch
           (lambda _
             (substitute* "tests/check/libs/pbutils.c"
               (("/bin/sh") (which "sh")))
             #t)))))
    (home-page "https://gstreamer.freedesktop.org/")
    (synopsis
     "Plugins for the GStreamer multimedia library")
    (description "This package provides an essential exemplary set of plug-ins
for the GStreamer multimedia library.")
    (license license:lgpl2.0+)))

(define-public gst-plugins-good
  (package
    (name "gst-plugins-good")
    (version "1.18.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://gstreamer.freedesktop.org/src/" name "/"
         name "-" version ".tar.xz"))
       (patches (search-patches "gst-plugins-good-fix-test.patch"
                                "gst-plugins-good-CVE-2021-3497.patch"
                                "gst-plugins-good-CVE-2021-3498.patch"))
       (sha256
        (base32 "1929nhjsvbl4bw37nfagnfsnxz737cm2x3ayz9ayrn9lwkfm45zp"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:phases
       (modify-phases %standard-phases
         ,@%common-gstreamer-phases
         (add-before 'check 'pre-check
           (lambda _
             ;; Tests require a running X server.
             (system "Xvfb :1 +extension GLX &")
             (setenv "DISPLAY" ":1")
             ;; Tests write to $HOME.
             (setenv "HOME" (getcwd))
             ;; Tests look for $XDG_RUNTIME_DIR.
             (setenv "XDG_RUNTIME_DIR" (getcwd))
             ;; For missing '/etc/machine-id'.
             (setenv "DBUS_FATAL_WARNINGS" "0")
             #t)))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python-wrapper" ,python-wrapper)
       ("xmllint" ,libxml2)
       ("xorg-server" ,xorg-server-for-tests)))
    (inputs
     `(("aalib" ,aalib)
       ("bzip2" ,bzip2)
       ("cairo" ,cairo)
       ("flac" ,flac)
       ("gdk-pixbuf" ,gdk-pixbuf+svg)
       ("glib" ,glib)
       ("glib-networking" ,glib-networking)
       ("glu" ,glu)
       ("gtk+" ,gtk+)
       ("jack" ,jack-2)
       ("lame" ,lame)
       ("libavc1394" ,libavc1394)
       ("libcaca" ,libcaca)
       ("libdv" ,libdv)
       ("libgudev" ,libgudev)
       ("libiec61883" ,libiec61883)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libshout" ,libshout)
       ("libsoup" ,libsoup)
       ("libvpx" ,libvpx)
       ("libx11" ,libx11)
       ("libxdamage" ,libxdamage)
       ("libxfixes" ,libxfixes)
       ("libxext" ,libxext)
       ("libxshm" ,libxshmfence)
       ("mesa" ,mesa)
       ("mpg123" ,mpg123)
       ("orc" ,orc)
       ("pulseaudio" ,pulseaudio)
       ("speex" ,speex)
       ("taglib" ,taglib)
       ("twolame" ,twolame)
       ("v4l-utils" ,v4l-utils)
       ("wavpack" ,wavpack)
       ("zlib" ,zlib)))
    (propagated-inputs
     `(("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)))
    (synopsis "GStreamer plugins and helper libraries")
    (description "GStreamer-Plugins-Good is a collection of plug-ins you'd want
to have right next to you on the battlefield.  Shooting sharp and making no
mistakes, these plug-ins have it all: good looks, good code, and good
licensing.  Documented and dressed up in tests.  If you're looking for a role
model to base your own plug-in on, here it is.")
    (home-page "https://gstreamer.freedesktop.org/")
    (license license:lgpl2.0+)))

(define-public gst-plugins-bad
  (package
    (name "gst-plugins-bad")
    (version "1.18.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gstreamer.freedesktop.org/src/"
                                  name "/" name "-" version ".tar.xz"))
              (patches (search-patches "gst-plugins-bad-fix-overflow.patch"))
              (sha256
               (base32
                "06ildd4rl6cynirv3p00d2ddf5is9svj4i7mkahldzhq24pq5mca"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ,@%common-gstreamer-phases
         ,@(if (string-prefix? "arm" (or (%current-target-system)
                                         (%current-system)))
               ;; Disable test that fails on ARMv7.
               ;; https://gitlab.freedesktop.org/gstreamer/gst-plugins-bad/issues/1188
               `((add-after 'unpack 'disable-asfmux-test
                   (lambda _
                     (substitute* "tests/check/meson.build"
                       (("\\[\\['elements/asfmux\\.c'\\]\\],")
                        ""))
                     #t)))
               '())
         (add-after 'unpack 'adjust-tests
           (lambda* (#:key native-inputs inputs #:allow-other-keys)
             (let ((gst-plugins-good (assoc-ref (or native-inputs inputs)
                                                "gst-plugins-good")))
               (substitute* "tests/check/meson.build"
                 ;; Make gst-plugin-good available for tests, see
                 ;; https://gitlab.freedesktop.org/gstreamer/gst-plugins-bad/-/issues/1426
                 (("'GST_PLUGIN_SYSTEM_PATH_1_0', ''")
                  (string-append "'GST_PLUGIN_SYSTEM_PATH_1_0', '"
                                 gst-plugins-good "/lib/gstreamer-1.0'"))

                 ;; FIXME: Why is this failing.
                 ((".*elements/dash_mpd\\.c.*") "")

                 ;; These tests are flaky and occasionally time out:
                 ;; https://gitlab.freedesktop.org/gstreamer/gst-plugins-bad/-/issues/932
                 ((".*elements/curlhttpsrc\\.c.*") "")
                 ;; https://gitlab.freedesktop.org/gstreamer/gst-plugins-bad/-/issues/1412
                 ((".*elements/dtls\\.c.*") ""))
               #t))))))
    (propagated-inputs
     `(("gst-plugins-base" ,gst-plugins-base)))
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-mkenums, etc.
       ("gobject-introspection" ,gobject-introspection)
       ("gst-plugins-good" ,gst-plugins-good) ;for tests
       ("pkg-config" ,pkg-config)
       ("python" ,python)))
    (inputs
     ;; XXX: The following dependencies are missing:
     ;;  vo-amrwbenc, vo-aacenc, bs2b, chromaprint, directfb, daala, libdts,
     ;;  faac, flite, libgsm, libde265, libmms, libmimic, mjpegtools,
     ;;  mpeg2enc, libofa, opencv, openh264, openni2, libtimemmgr, wildmidi,
     ;;  openspc, gme, sbc, schroedinger, zbar, librtmp, spandsp
     `(("bluez" ,bluez)
       ("curl" ,curl)
       ("faad2" ,faad2)
       ("fluidsynth" ,fluidsynth)
       ("gtk+" ,gtk+)
       ("ladspa" ,ladspa)
       ("libass" ,libass)
       ("libdvdnav" ,libdvdnav)
       ("libdvdread" ,libdvdread)
       ("libgcrypt" ,libgcrypt)
       ("libgudev" ,libgudev)
       ("libkate" ,libkate)
       ("libmodplug" ,libmodplug)
       ("libnice" ,libnice)
       ("librsvg" ,librsvg)
       ("libsndfile" ,libsndfile)
       ("libsrtp" ,libsrtp)
       ("libssh2" ,libssh2)
       ("libusb" ,libusb)
       ("libvdpau" ,libvdpau)
       ("libwebp" ,libwebp)
       ("libxml2" ,libxml2)
       ("lrdf" ,lrdf)
       ("mesa" ,mesa)
       ("neon" ,neon)
       ("openal" ,openal)
       ("openexr" ,openexr)
       ("openjpeg" ,openjpeg)
       ("openssl" ,openssl)
       ("opus" ,opus)
       ("orc" ,orc)
       ;("qtbase" ,qtbase-5)
       ;("qtdeclarative" ,qtdeclarative)
       ;("qtx11extras" ,qtx11extras)
       ("soundtouch" ,soundtouch)
       ;; GStreamer is not yet compatible with srt > 1.4.1.
       ("srt" ,srt-1.4.1)
       ("x265" ,x265)
       ("webrtc-audio-processing" ,webrtc-audio-processing)
       ("wayland" ,wayland)))
    (home-page "https://gstreamer.freedesktop.org/")
    (synopsis "Plugins for the GStreamer multimedia library")
    (description
     "GStreamer Bad Plug-ins is a set of plug-ins whose quality aren't up to
par compared to the rest.")
    (license license:lgpl2.0+)))

(define-public gst-plugins-ugly
  (package
    (name "gst-plugins-ugly")
    (version "1.18.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://gstreamer.freedesktop.org/src/"
                       name "/" name "-" version ".tar.xz"))
       (patches (search-patches "gst-plugins-ugly-fix-out-of-bound-reads.patch"))
       (sha256
        (base32 "1nwbcv5yaib3d8icvyja3zf6lyjf5zf1hndbijrhj8j7xlia0dx3"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:phases
       (modify-phases %standard-phases
         ,@%common-gstreamer-phases
         (add-before 'check 'pre-check
           (lambda _
             ;; Tests require a running X server.
             (system "Xvfb :1 +extension GLX &")
             (setenv "DISPLAY" ":1")
             ;; Tests write to $HOME.
             (setenv "HOME" (getcwd))
             ;; Tests look for $XDG_RUNTIME_DIR.
             (setenv "XDG_RUNTIME_DIR" (getcwd))
             ;; For missing '/etc/machine-id'.
             (setenv "DBUS_FATAL_WARNINGS" "0")
             #t)))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python-wrapper" ,python-wrapper)
       ("xorg-server" ,xorg-server-for-tests)))
    (inputs
     `(("glib" ,glib)
       ("glib-networking" ,glib-networking)
       ("liba52" ,liba52)
       ("libcdio" ,libcdio)
       ("libdvdread" ,libdvdread)
       ("libmpeg2" ,libmpeg2)
       ("libx264" ,libx264)
       ("opencore-amr" ,opencore-amr)
       ("orc" ,orc)))
    (propagated-inputs
     `(("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)))
    (synopsis "GStreamer plugins and helper libraries")
    (description "Gst-Plugins-Ugly are the ones that might have a patent noose
around their neck, or a lock-up license, or any other problem that makes you
think twice about shipping them.")
    (home-page "https://gstreamer.freedesktop.org/")
    (license license:lgpl2.0+)))

(define-public gst-libav
  (package
    (name "gst-libav")
    (version "1.18.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://gstreamer.freedesktop.org/src/" name "/"
         name "-" version ".tar.xz"))
       (patches (search-patches "gst-libav-64channels-stack-corruption.patch"))
       (sha256
        (base32 "0jbzams9ggk3sq9ywv4gsl9rghyn203l2582m6l5c1sz9ka9m5in"))))
    (build-system meson-build-system)
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("ruby" ,ruby)))
    (inputs
     `(("ffmpeg" ,ffmpeg)))
    (propagated-inputs
     `(("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)))
    (synopsis "GStreamer plugins and helper libraries")
    (description "Gst-Libav contains a GStreamer plugin for using the encoders,
decoders, muxers, and demuxers provided by FFmpeg.")
    (home-page "https://gstreamer.freedesktop.org/")
    (license license:lgpl2.0+)))

(define-public gst-editing-services
  (package
    (name "gst-editing-services")
    (version "1.18.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://gstreamer.freedesktop.org/src/" name "/"
                    "gst-editing-services-" version ".tar.xz"))
              (sha256
               (base32
                "0pv2k8zlpn3vv2sdlspi3m63ixcwzi90pjly2ypbkg59ab97rb15"))))
    (build-system meson-build-system)
    (arguments
     ;; FIXME: 16/22 failing tests.
     `(#:tests? #f
       #:phases (modify-phases %standard-phases
                  ,@%common-gstreamer-phases)))
    (inputs
     `(("gst-plugins-base" ,gst-plugins-base)
       ("libxml2" ,libxml2)))
    (native-inputs
     `(("flex" ,flex)
       ("gobject-introspection" ,gobject-introspection)
       ("glib:bin" ,glib "bin")
       ("gst-plugins-bad" ,gst-plugins-bad)
       ("gst-plugins-good" ,gst-plugins-good)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python)))
    (home-page "https://gstreamer.freedesktop.org/")
    (synopsis "GStreamer library for non-linear editors")
    (description
     "This is a high-level library for facilitating the creation of audio/video
non-linear editors.")
    (license license:gpl2+)))

(define-public gst-plugins/selection
  (lambda* (pkg #:key plugins configure-flags)
    "Build PKG with only PLUGINS enabled.  Optionally, if CONFIGURE-FLAGS are
given, also pass them to the build system instead of the ones used by PKG."
    (package/inherit pkg
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:configure-flags flags `(,@(or configure-flags '())))
          `(append
            (list
             ,@(map (lambda (plugin)
                      (string-append "-D" plugin "=enabled"))
                    plugins))
            (list ,@(or configure-flags flags))))
          ((#:phases phases)
           `(modify-phases ,phases
              (add-after 'unpack 'disable-auto-plugins
                (lambda _
                  (substitute* "meson_options.txt"
                    (("'auto'") "'disabled'"))
                  #t)))))))))

(define-public gst-transcoder
  (deprecated-package
   "gst-transcoder"
   (gst-plugins/selection gst-plugins-bad
                          #:plugins '("transcoder")
                          #:configure-flags '("-Dintrospection=enabled"))))

(define-public python-gst
  (package
    (name "python-gst")
    (version "1.18.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://gstreamer.freedesktop.org/src/gst-python/"
                    "gst-python-" version ".tar.xz"))
              (sha256
               (base32
                "171qxzndii7ynn9ag3a12h9vyydxzwy1j4ip3cb8hgim1dv0z7g1"))))
    (build-system meson-build-system)
    (arguments
     `(#:modules ((guix build meson-build-system)
                  (guix build utils)
                  ((guix build python-build-system) #:prefix python:))
       #:imported-modules (,@%meson-build-system-modules
                           (guix build python-build-system))
       #:configure-flags
       (list (string-append
              "-Dpygi-overrides-dir="
              (python:site-packages %build-inputs %outputs) "gi/overrides"))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python" ,python)))
    (propagated-inputs
     `(("gst-plugins-base" ,gst-plugins-base)
       ("python-pygobject" ,python-pygobject)))
    (home-page "https://gstreamer.freedesktop.org/")
    (synopsis "GStreamer GObject Introspection overrides for Python")
    (description
     "This package contains GObject Introspection overrides for Python that can
be used by Python applications using GStreamer.")
    (license license:lgpl2.1+)))

(define-public gst123
  (package
    (name "gst123")
    (version "0.3.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://space.twc.de/~stefan/gst123/gst123-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "0zaa117n4wkya9p903vkj8hj58lmdb66pxsdx5wwcv7nffbp5d67"))))
    (build-system gnu-build-system)
    (inputs
     `(("gtk+" ,gtk+-2)
       ("ncurses" ,ncurses)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://space.twc.de/~stefan/gst123.php")
    (synopsis "Flexible command line media player based on gstreamer")
    (description "The program gst123 is designed to be a more flexible command
line player in the spirit of ogg123 and mpg123, based on the gstreamer media
framework.  It plays all file formats gstreamer supports, so if you have a
music collection which contains different file formats, like flac, ogg and
mp3, you can use gst123 to play all your music files.")
    (license license:lgpl2.0+)))
