;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2015, 2016 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2018 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages telephony)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages xml))

(define-public orc
  (package
    (name "orc")
    (version "0.4.28")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gstreamer.freedesktop.org/data/src/"
                                  "orc/orc-" version ".tar.xz"))
              (sha256
               (base32
                "1kl3rlmzr27bdpn78nvpnjs142ja1m6grvafdhw74mmhcdjprkdz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'disable-faulty-test
           (lambda _
             ;; XXX Disable the 'test-limits' and 'exec_opcodes_sys'
             ;; tests, which fail on some machines.  See:
             ;; https://bugzilla.gnome.org/show_bug.cgi?id=735273
             (substitute* '("testsuite/test-limits.c"
                            "testsuite/exec_opcodes_sys.c")
               (("if \\(error\\) return 1;")
                "if (error) return 77;"))
             #t)))))
    (home-page "https://gstreamer.freedesktop.org/modules/orc.html")
    (synopsis "Oil runtime compiler")
    (description
     "Orc is a just-in-time compiler implemented as a library and set of
associated tools for compiling and executing simple programs that operate on
arrays of data.")
    ;; The source code implementing the Marsenne Twister algorithm is licensed
    ;; under the 3-clause BSD license, the rest is under 2-clause BSD license.
    (license (list license:bsd-2 license:bsd-3))))

(define-public gstreamer
  (package
    (name "gstreamer")
    (version "1.14.4")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "https://gstreamer.freedesktop.org/src/gstreamer/gstreamer-"
            version ".tar.xz"))
      (sha256
       (base32
        "1izzhnlsy83rgr4zl3jcl1sryxqbbigrrqw3j4x3nnphqnb6ckzr"))))
    (build-system gnu-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-html-dir="
                            (assoc-ref %outputs "doc")
                            "/share/gtk-doc/html"))))
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
    (version "1.14.4")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://gstreamer.freedesktop.org/src/" name "/"
                          name "-" version ".tar.xz"))
      (sha256
       (base32
        "0qbllw4kphchwhy4p7ivdysigx69i97gyw6q0rvkx1j81r4kjqfa"))))
    (build-system gnu-build-system)
    (outputs '("out" "doc"))
    (propagated-inputs
     `(("glib" ,glib)              ;required by gstreamer-sdp-1.0.pc
       ("gstreamer" ,gstreamer)    ;required by gstreamer-plugins-base-1.0.pc
       ("orc" ,orc)))              ;required by gstreamer-audio-1.0.pc
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
     `(#:parallel-tests? #f ; 'pipelines/tcp' fails in parallel
       #:configure-flags
       (list (string-append "--with-html-dir="
                            (assoc-ref %outputs "doc")
                            "/share/gtk-doc/html"))
       #:phases
       (modify-phases %standard-phases
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
    (version "1.14.4")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "https://gstreamer.freedesktop.org/src/" name "/"
            name "-" version ".tar.xz"))
      (patches (search-patches "gst-plugins-good-libvpx-compat.patch"))
      (sha256
       (base32
        "0y89qynb4b6fry3h43z1r99qslmi3m8xhlq0i5baq2nbc0r5b2sz"))))
    (build-system gnu-build-system)
    (inputs
     `(("aalib" ,aalib)
       ("cairo" ,cairo)
       ("flac" ,flac)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gtk+" ,gtk+)
       ("jack" ,jack-1)
       ("lame" ,lame)
       ("libavc1394" ,libavc1394)
       ("libcaca" ,libcaca)
       ("libdv" ,libdv)
       ("libiec61883" ,libiec61883)
       ("libjpeg" ,libjpeg)
       ("libpng" ,libpng)
       ("libshout" ,libshout)
       ("libsoup" ,libsoup)
       ("libvpx" ,libvpx)
       ("mpg123" ,mpg123)
       ("orc" ,orc)
       ("pulseaudio" ,pulseaudio)
       ("speex" ,speex)
       ("taglib" ,taglib)
       ("twolame" ,twolame)
       ("wavpack" ,wavpack)))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("pkg-config" ,pkg-config)
       ("python-wrapper" ,python-wrapper)))
    (arguments
     `(#:make-flags '("CFLAGS=-DHAVE_VPX_1_8")   ;XXX: Remove for >1.14.
       #:phases
       (modify-phases %standard-phases
         (add-after
          'unpack 'disable-failing-tests
          (lambda _
            ;; Disable tests that fail non-deterministically.
            ;; This test fails on aarch64 on 1.12.x.
            (substitute* "tests/check/elements/alpha.c"
              (("tcase_add_test \\(tc_chain, test_chromakeying\\);" all)
               (string-append "/* " all " */")))
            #t)))))
    (home-page "https://gstreamer.freedesktop.org/")
    (synopsis
     "Plugins for the GStreamer multimedia library")
    (description "GStreamer Good Plug-ins is a set of plug-ins for the
GStreamer multimedia library.  This set contains those plug-ins which the
developers consider to have good quality code and correct functionality.")
    (license license:lgpl2.0+)))

(define-public gst-plugins-bad
  (package
    (name "gst-plugins-bad")
    (version "1.14.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gstreamer.freedesktop.org/src/"
                                  name "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1r8dma3x127rbx42yab7kwq7q1bhkmvz2ykn0rnqnzl95q74w2wi"))))
    (outputs '("out" "doc"))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; XXX: 13 of 53 tests fail
       #:configure-flags
       (list (string-append "--with-html-dir="
                            (assoc-ref %outputs "doc")
                            "/share/gtk-doc/html"))))
    (propagated-inputs
     `(("gst-plugins-base" ,gst-plugins-base)))
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-mkenums, etc.
       ("gobject-introspection" ,gobject-introspection)
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
       ;("qtbase" ,qtbase)
       ;("qtdeclarative" ,qtdeclarative)
       ;("qtx11extras" ,qtx11extras)
       ("soundtouch" ,soundtouch)
       ("x265" ,x265)
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
    (version "1.14.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://gstreamer.freedesktop.org/src/"
                           name "/" name "-" version ".tar.xz"))
       (sha256
        (base32
         "08vd1xgwmapnviah47zv5h2r02qdd20y4f07rvv5zhv6y4vxh0mc"))))
    (build-system gnu-build-system)
    (inputs
     `(("gst-plugins-base" ,gst-plugins-base)
       ("liba52" ,liba52)
       ("libcdio" ,libcdio)
       ("libmpeg2" ,libmpeg2)
       ("libdvdread" ,libdvdread)
       ("libx264" ,libx264)
       ;; TODO:
       ;; * opencore-amr (for the AMR-NB decoder and encoder and the
       ;;   AMR-WB decoder) <http://sourceforge.net/projects/opencore-amr/>
       ("orc" ,orc)))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("pkg-config" ,pkg-config)
       ("python-wrapper" ,python-wrapper)))
    (home-page "https://gstreamer.freedesktop.org/")
    (synopsis "GStreamer plugins from the \"ugly\" set")
    (description "GStreamer Ugly Plug-ins.  This set contains those plug-ins
which the developers consider to have good quality code but that might pose
distribution problems in some jurisdictions, e.g. due to patent threats.")
    (license license:lgpl2.0+)))

(define-public gst-libav
  (package
    (name "gst-libav")
    (version "1.14.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://gstreamer.freedesktop.org/src/" name "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1nk5g24z2xx5kaw5cg8dv8skdc516inahmkymcz8bxqxj28qbmyz"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Drop bundled ffmpeg.
                  (delete-file-recursively "gst-libs/ext/libav")
                  #t))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--with-system-libav")))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python" ,python)))
    (inputs
     `(("gst-plugins-base" ,gst-plugins-base)
       ("ffmpeg" ,ffmpeg-3.4)
       ("orc" ,orc)
       ("zlib" ,zlib)))
    (home-page "https://gstreamer.freedesktop.org/")
    (synopsis "Plugins for the GStreamer multimedia library")
    (description
     "This GStreamer plugin supports a large number of audio and video
compression formats through the use of the libav library.")
    (license license:gpl2+)))

(define-public python-gst
  (package
    (name "python-gst")
    (version "1.14.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://gstreamer.freedesktop.org/src/gst-python/"
                    "gst-python-" version ".tar.xz"))
              (sha256
               (base32
                "06ssx19fs6pg4d32p9ph9w4f0xwmxaw2dxfj17rqkn5njd7v5zfh"))))
    (build-system gnu-build-system)
    (arguments
     ;; XXX: Factorize python-sitedir with python-build-system.
     `(#:imported-modules (,@%gnu-build-system-modules
                           (guix build python-build-system))
       #:configure-flags
       (let* ((python (assoc-ref %build-inputs "python"))
              (python-version ((@@ (guix build python-build-system)
                                   get-python-version)
                               python))
              (python-sitedir (string-append
                               "lib/python" python-version "/site-packages")))
         (list (string-append
                "--with-pygi-overrides-dir=" %output "/" python-sitedir
                "/gi/overrides")))))
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
    (license license:lgpl2.1+)
    (properties `((python2-variant . ,(delay python2-gst))))))

(define-public python2-gst
  (package (inherit python-gst)
    (name "python2-gst")
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python" ,python-2)))
    (propagated-inputs
     `(("gst-plugins-base" ,gst-plugins-base)
       ("python-pygobject" ,python2-pygobject)))))

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
    (home-page "http://space.twc.de/~stefan/gst123.php")
    (synopsis "Flexible command line media player based on gstreamer")
    (description "The program gst123 is designed to be a more flexible command
line player in the spirit of ogg123 and mpg123, based on the gstreamer media
framework.  It plays all file formats gstreamer supports, so if you have a
music collection which contains different file formats, like flac, ogg and
mp3, you can use gst123 to play all your music files.")
    (license license:lgpl2.0+)))
