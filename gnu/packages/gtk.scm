;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2017, 2018, 2019, 2021 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015 Andy Wingo <wingo@igalia.com>
;;; Copyright © 2015 David Hashe <david.hashe@dhashe.com>
;;; Copyright © 2015, 2016, 2017, 2018, 2020, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Fabian Harfert <fhmgufs@web.de>
;;; Copyright © 2016 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2016 Patrick Hetu <patrick.hetu@auf.org>
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2017 Roel Janssen <roel@gnu.org>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017, 2019, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2018, 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 Meiyo Peng <meiyo@riseup.net>
;;; Copyright © 2019 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2020 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2020 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2020, 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Leo Famulari <leo@famulari.name>
;;; Copyright © 2021 Simon Streit <simon@netpanic.org>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2021 Wamm K. D. <jaft.r@outlook.com>
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

(define-module (gnu packages gtk)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix build utils) #:select (alist-replace))
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system waf)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages man)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages pulseaudio)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match))

(define-public atk
  (package
    (name "atk")
    (version "2.36.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1217cmmykjgkkim0zr1lv5j13733m4w5vipmy4ivw0ll6rz28xpv"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t ; To wrap binaries and/or compile schemas
       ,@(if (%current-target-system)
             `(#:configure-flags
               ;; introspection requires running binaries for the host system
               ;; on the build system.
               '("-Dintrospection=false"))
             '())))
    (propagated-inputs (list glib)) ; required by atk.pc
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib" ,glib "bin")             ; glib-mkenums, etc.
       ("gobject-introspection" ,gobject-introspection) ; g-ir-compiler, etc.
       ("pkg-config" ,pkg-config)))
    (synopsis "GNOME accessibility toolkit")
    (description
     "ATK provides the set of accessibility interfaces that are implemented
by other toolkits and applications.  Using the ATK interfaces, accessibility
tools have full access to view and control running applications.")
    (license license:lgpl2.1+)
    (home-page "https://wiki.gnome.org/Accessibility")))

(define-public cairo
  (package
    (name "cairo")
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://cairographics.org/releases/cairo-"
                       version ".tar.xz"))
       (sha256
        (base32 "0c930mk5xr2bshbdljv005j3j8zr47gqmkry3q6qgvqky6rjjysy"))
       (patches (search-patches
		 "cairo-CVE-2018-19876.patch"
		 "cairo-CVE-2020-35492.patch"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:tests? #f ; see http://lists.gnu.org/archive/html/bug-guix/2013-06/msg00085.html
       #:configure-flags
       (list
        "--disable-static"
        ;; XXX: To be enabled.
        ;; "--enable-gallium=yes"
        ;; "--enable-gl=yes"
        ;; " --enable-glesv2=yes"
        ;; "--enable-glesv3=yes"
        ;; "--enable-cogl=yes"
        ;; "--enable-directfb=yes"
        ;; "--enable-vg=yes"
        "--enable-tee=yes"              ;needed for GNU IceCat
        "--enable-xml=yes"              ;for cairo-xml support
        (string-append "--with-html-dir="
                       (assoc-ref %outputs "doc")
                       "/share/gtk-doc/html"))))
    (native-inputs
     `(("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)))
    (inputs
     `(("bash-minimal" ,bash-minimal)
       ("drm" ,libdrm)
       ("ghostscript" ,ghostscript)
       ("libspectre" ,libspectre)
       ("poppler" ,poppler)))
    (propagated-inputs
     `( ;; ("cogl" ,cogl)
       ;; ("directfb" ,directfb)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("glib" ,glib)
       ;; ("gtk+" ,gtk+)
       ("libpng" ,libpng)
       ;; ("librsvg" ,librsvg)
       ;; ("opengl" ,mesa)
       ("pixman" ,pixman)
       ("x11" ,libx11)
       ("xcb" ,libxcb)
       ("xext" ,libxext)
       ("xrender" ,libxrender)))
    (synopsis "Multi-platform 2D graphics library")
    (description "Cairo is a 2D graphics library with support for multiple output
devices.  Currently supported output targets include the X Window System (via
both Xlib and XCB), Quartz, Win32, image buffers, PostScript, PDF, and SVG file
output.  Experimental backends include OpenGL, BeOS, OS/2, and DirectFB.")
    (home-page "https://cairographics.org/")
    (license
     ;; This project is dual-licensed.
     (list
      license:lgpl2.1+
      license:mpl1.1))))

(define-public cairo-sans-poppler
  ;; Variant used to break the dependency cycle between Poppler and Cairo.
  (package/inherit cairo
    (inputs (alist-delete "poppler" (package-inputs cairo)))
    (properties `((hidden? . #t)))))

(define-public cairo-xcb
  (package/inherit cairo
    (name "cairo-xcb")
    (inputs
     `(("mesa" ,mesa)
       ,@(package-inputs cairo)))
    (arguments
     `(#:tests? #f
       #:configure-flags
       '("--enable-xlib-xcb" "--enable-gl" "--enable-egl")))
    (synopsis "2D graphics library (with X11 support)")))

(define-public harfbuzz
  (package
   (name "harfbuzz")
   (version "2.8.2")
   (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/harfbuzz/harfbuzz"
                                 "/releases/download/" version "/harfbuzz-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1rvv86wpm3y04fqns1655268rhvhvms469837709v2z2bhwn316m"))))
   (build-system gnu-build-system)
   (outputs '("out"
              "bin")) ; 160K, only hb-view depend on cairo
   (inputs
    (list cairo))
   (propagated-inputs
    ;; There are all in the Requires or Requires.private field of '.pc'.
    (list glib graphite2 icu4c))
   (native-inputs
    `(("glib:bin" ,glib "bin")          ;for glib-mkenums
      ("gobject-introspection" ,gobject-introspection)
      ("pkg-config" ,pkg-config)
      ("python" ,python-wrapper)
      ("which" ,which)))
   (arguments
    `(#:configure-flags `("--with-graphite2"
                          "--with-gobject"
                          ,(string-append
                            "--bindir=" (assoc-ref %outputs "bin") "/bin"))))
   (synopsis "OpenType text shaping engine")
   (description
    "HarfBuzz is an OpenType text shaping engine.")
   (license (license:x11-style "file://COPYING"
                       "See 'COPYING' in the distribution."))
   (home-page "https://www.freedesktop.org/wiki/Software/HarfBuzz/")))

(define-public harfbuzz-3.0
  (package
    (inherit harfbuzz)
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/harfbuzz/harfbuzz"
                                  "/releases/download/" version
                                  "/harfbuzz-" version ".tar.xz"))
              (sha256
               (base32
                "1ngk8vn06rryx3s4v5pbl91bw1j1pd4431n77rw3j5a533hhwsq3"))))))

(define-public libdatrie
  (package
    (name "libdatrie")
    (version "0.2.13")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://linux.thai.net/pub/ThaiLinux/software/"
                       "libthai/libdatrie-" version ".tar.xz"))
       (sha256
        (base32 "1gplcx9ddglpxmqm10qn38kjmvdh4hnhj14rzgqag095psr1n8qj"))))
    (build-system gnu-build-system)
    (outputs '("out" "doc"))
    (arguments
     (list #:configure-flags
           #~(list (string-append "--with-html-docdir=" #$output:doc
                                  "/share/doc/datrie/html"))

           ;; Several tests refer to the 'test.tri' file, leading to race
           ;; conditions when running tests in parallel.
           #:parallel-tests? #f))
    (native-inputs
     (list doxygen pkg-config))
    (synopsis "Double-Array Trie Library")
    (description "Libdatrie is an implementation of double-array structure for
representing trie.  Trie is a kind of digital search tree.")
    (home-page "https://linux.thai.net/~thep/datrie/datrie.html")
    (license license:lgpl2.1+)))

(define-public libthai
  (package
    (name "libthai")
    (version "0.1.28")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://linux.thai.net/pub/thailinux/software/"
                       "libthai/libthai-" version ".tar.xz"))
       (sha256
        (base32 "04g93bgxrcnay9fglpq2lj9nr7x1xh06i60m7haip8as9dxs3q7z"))))
    (build-system gnu-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:configure-flags
       (list
        (string-append "--with-html-docdir="
                       (assoc-ref %outputs "doc")
                       "/share/doc/libthai/html"))))
    (native-inputs
     `(("datrie" ,libdatrie)
       ("doxygen" ,doxygen)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("datrie" ,libdatrie)))
    (synopsis "Thai language support library")
    (description "LibThai is a set of Thai language support routines aimed to
ease developers’ tasks to incorporate Thai language support in their
applications.")
    (home-page "https://linux.thai.net/projects/libthai")
    (license license:lgpl2.1+)))

(define-public pango
  (package
    (name "pango")
    (version "1.48.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/pango/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (patches (search-patches "pango-skip-libthai-test.patch"))
              (sha256
               (base32
                "166wxhsjb6hb0dk7wkkdcmpvasl9n0a0aa64mdgagzfdidwzbq91"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t             ; To wrap binaries and/or compile schemas
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'disable-cantarell-tests
                    (lambda _
                      (substitute* "tests/meson.build"
                        ;; XXX FIXME: These tests require "font-abattis-cantarell", but
                        ;; adding it here would introduce a circular dependency.
                        (("\\[ 'test-layout'.*") "")
                        (("\\[ 'test-itemize'.*") "")
                        (("\\[ 'test-font'.*") "")
                        (("\\[ 'test-harfbuzz'.*") "")))))))
    (propagated-inputs
     ;; These are all in Requires or Requires.private of the '.pc' files.
     `(("cairo" ,cairo)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("fribidi" ,fribidi)
       ("glib" ,glib)
       ("harfbuzz" ,harfbuzz)
       ("libthai" ,libthai)
       ;; Some packages, such as Openbox, expect Pango to be built with the
       ;; optional libxft support.
       ("libxft" ,libxft)
       ("libxrender" ,libxrender)))
    (inputs
     (list bash-minimal zlib))
    (native-inputs
     `(("glib" ,glib "bin")                             ; glib-mkenums, etc.
       ("gobject-introspection" ,gobject-introspection) ; g-ir-compiler, etc.
       ("help2man" ,help2man)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)))
    (synopsis "Text and font handling library")
    (description "Pango is a library for laying out and rendering of text, with
an emphasis on internationalization.  Pango can be used anywhere that text
layout is needed, though most of the work on Pango so far has been done in the
context of the GTK+ widget toolkit.  Pango forms the core of text and font
handling for GTK+-2.x.")
    (home-page "https://pango.gnome.org/")
    (license license:lgpl2.0+)))

(define-public pango-1.42
  (package
   (inherit pango)
   (version "1.42.4")
   (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/pango/"
                                 (version-major+minor version) "/"
                                 "pango-" version ".tar.xz"))
             (sha256
              (base32
               "17bwb7dgbncrfsmchlib03k9n3xaalirb39g3yb43gg8cg6p8aqx"))))
   (build-system gnu-build-system)
   (arguments
    '(#:phases (modify-phases %standard-phases
                 (add-after 'configure 'disable-layout-test
                   (lambda _
                     ;; This test requires that fontconfig uses bitmap fonts
                     ;; such as "font-ghostscript"; however providing such a
                     ;; package alone is not enough, as the requirement comes
                     ;; from deeper in the font stack.  Since this version of
                     ;; Pango is only used for librsvg, simply disable the
                     ;; test.
                     (substitute* "tests/Makefile"
                       (("test-layout\\$\\(EXEEXT\\)") ""))
                     #t)))))))

(define-public pangox-compat
  (package
    (name "pangox-compat")
    (version "0.0.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/" name "/"
                                 (version-major+minor version)  "/"
                                 name "-" version ".tar.xz"))
             (sha256
              (base32
               "0ip0ziys6mrqqmz4n71ays0kf5cs1xflj1gfpvs4fgy2nsrr482m"))))
    (build-system gnu-build-system)
    (inputs
     (list glib pango-1.42))
    (native-inputs
     (list intltool pkg-config))
    (home-page "https://developer.gnome.org/pango")
    (synopsis "Obsolete pango functions")
    (description  "Pangox was a X backend to pango.  It is now obsolete and no
longer provided by recent pango releases.  pangox-compat provides the
functions which were removed.")
    (license license:lgpl2.0+)))

(define-public ganv
  (package
    (name "ganv")
    (version "1.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.drobilla.net/ganv-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "0pik2d3995z0rjcjhb4hsj5fsph3m8khg6j10k6mx4j2j727aq6l"))))
    (build-system waf-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-flags
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Allow 'bin/ganv_bench' to find libganv-1.so.
             (setenv "LDFLAGS"
                     (string-append "-Wl,-rpath="
                                    (assoc-ref outputs "out") "/lib"))
             #t)))
       #:tests? #f)) ; no check target
    (inputs
     `(("gtk" ,gtk+-2)
       ("gtkmm" ,gtkmm-2)))
    (native-inputs
     (list `(,glib "bin") ; for glib-genmarshal, etc.
           pkg-config))
    (home-page "https://drobilla.net/software/ganv/")
    (synopsis "GTK+ widget for interactive graph-like environments")
    (description
     "Ganv is an interactive GTK+ widget for interactive “boxes and lines” or
graph-like environments, e.g. modular synths or finite state machine
diagrams.")
    (license license:gpl3+)))

(define-public gtksourceview-2
  (package
    (name "gtksourceview")
    (version "2.10.5") ; This is the last version which builds against gtk+2
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "07hrabhpl6n8ajz10s0d960jdwndxs87szxyn428mpxi8cvpg1f5"))
              (patches
                (search-patches
                  "gtksourceview-2-add-default-directory.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     (list intltool
           `(,glib "bin") ; for glib-genmarshal, etc.
           pkg-config
           ;; For testing.
           xorg-server-for-tests
           shared-mime-info))
    (propagated-inputs
     ;; As per the pkg-config file.
     `(("gtk" ,gtk+-2)
       ("libxml2" ,libxml2)))
    (arguments
     `(#:phases
       ;; Unfortunately, some of the tests in "make check" are highly dependent
       ;; on the environment therefore, some black magic is required.
       (modify-phases %standard-phases
         (add-before 'check 'start-xserver
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((xorg-server (assoc-ref inputs "xorg-server"))
                   (mime (assoc-ref inputs "shared-mime-info")))

               ;; There must be a running X server and make check doesn't start one.
               ;; Therefore we must do it.
               (system (format #f "~a/bin/Xvfb :1 &" xorg-server))
               (setenv "DISPLAY" ":1")

               ;; The .lang files must be found in $XDG_DATA_HOME/gtksourceview-2.0
               (system "ln -s gtksourceview gtksourceview-2.0")
               (setenv "XDG_DATA_HOME" (getcwd))

               ;; Finally, the mimetypes must be available.
               (setenv "XDG_DATA_DIRS" (string-append mime "/share/")))
             #t)))))
    (synopsis "Widget that extends the standard GTK+ 2.x 'GtkTextView' widget")
    (description
     "GtkSourceView is a portable C library that extends the standard GTK+
framework for multiline text editing with support for configurable syntax
highlighting, unlimited undo/redo, search and replace, a completion framework,
printing and other features typical of a source code editor.")
    (license license:lgpl2.0+)
    (home-page "https://developer.gnome.org/gtksourceview/")))

(define-public gtksourceview
 (package
   (name "gtksourceview")
   (version "4.2.0")
   (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/gtksourceview/"
                                 (version-major+minor version) "/"
                                 "gtksourceview-" version ".tar.xz"))
             (sha256
              (base32
               "0xgnjj7jd56wbl99s76sa1vjq9bkz4mdsxwgwlcphg689liyncf4"))))
   (build-system gnu-build-system)
   (arguments
    '(#:phases
      (modify-phases %standard-phases
        (add-before
         'check 'pre-check
         (lambda* (#:key inputs #:allow-other-keys)
           (let ((xorg-server (assoc-ref inputs "xorg-server")))
             ;; Tests require a running X server.
             (system (format #f "~a/bin/Xvfb :1 &" xorg-server))
             (setenv "DISPLAY" ":1")
             ;; For the missing /etc/machine-id.
             (setenv "DBUS_FATAL_WARNINGS" "0")
             #t))))))
   (native-inputs
    `(("glib:bin" ,glib "bin") ; for glib-genmarshal, etc.
      ("intltool" ,intltool)
      ("itstool" ,itstool)
      ("gobject-introspection" ,gobject-introspection)
      ("pkg-config" ,pkg-config)
      ("vala" ,vala)
      ;; For testing.
      ("xorg-server" ,xorg-server-for-tests)
      ("shared-mime-info" ,shared-mime-info)))
   (propagated-inputs
    ;; gtksourceview-3.0.pc refers to all these.
    (list glib gtk+ libxml2))
   (home-page "https://wiki.gnome.org/Projects/GtkSourceView")
   (synopsis "GNOME source code widget")
   (description "GtkSourceView is a text widget that extends the standard
GTK+ text widget GtkTextView.  It improves GtkTextView by implementing syntax
highlighting and other features typical of a source code editor.")
   (license license:lgpl2.1+)))

(define-public gtksourceview-3
 (package (inherit gtksourceview)
   (name "gtksourceview")
   (version "3.24.10")
   (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/" name "/"
                                 (version-major+minor version) "/"
                                 name "-" version ".tar.xz"))
             (sha256
              (base32
               "16ym7jwiki4s1pilwr4incx0yg7ll94f1cajrnpndkxxs36hcm5b"))))))

(define-public gdk-pixbuf
  (package
    (name "gdk-pixbuf")
    (version "2.42.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0k9f9177qxaryaxprwrhqnv5p2gdq4a8i6y05gm98qa8izc5v77y"))))
    (build-system meson-build-system)
    (outputs '("out" "debug"))
    (arguments
     `(#:glib-or-gtk? #t             ; To wrap binaries and/or compile schemas
       #:configure-flags '("-Dinstalled_tests=false")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook
           (lambda* (#:key native-inputs inputs #:allow-other-keys)
             (with-directory-excursion "docs"
               (substitute* "meson.build"
                 (("http://docbook.sourceforge.net/release/xsl/current/")
                  (string-append (assoc-ref ,(if (%current-target-system)
                                                 '(or native-inputs inputs)
                                                 'inputs) "docbook-xsl")
                                 "/xml/xsl/docbook-xsl-1.79.2/")))
               (substitute* (find-files "." "\\.xml$")
                 (("http://www.oasis-open.org/docbook/xml/4\\.3/")
                  (string-append (assoc-ref ,(if (%current-target-system)
                                                 '(or native-inputs inputs)
                                                 'inputs) "docbook-xml")
                                 "/xml/dtd/docbook/"))))))
         (add-before 'configure 'disable-failing-tests
           (lambda _
             (substitute* "tests/meson.build"
               (("\\[ 'pixbuf-fail', \\['conform', 'slow'\\], \\],")
                ""))))
         ;; The slow tests take longer than the specified timeout.
         ,@(if (any (cute string=? <> (%current-system))
                    '("armhf-linux" "aarch64-linux"))
               '((replace 'check
                   (lambda _
                     (invoke "meson" "test" "--timeout-multiplier" "5"))))
               '()))))
    (propagated-inputs
     (list ;; Required by gdk-pixbuf-2.0.pc
           glib
           ;; Required by gdk-pixbuf-xlib-2.0.pc
           libx11
           ;; Used for testing and required at runtime.
           shared-mime-info))
    (inputs
     `(,@(if (%current-target-system)
             `(("bash-minimal" ,bash-minimal)) ; for glib-or-gtk-wrap
             '())
       ("libjpeg" ,libjpeg-turbo)
       ("libpng"  ,libpng)
       ("libtiff" ,libtiff)))
    (native-inputs
     `(("docbook-xml" ,docbook-xml-4.3)
       ("docbook-xsl" ,docbook-xsl)
       ("gettext" ,gettext-minimal)
       ("glib" ,glib "bin")                             ; glib-mkenums, etc.
       ("gobject-introspection" ,gobject-introspection) ; g-ir-compiler, etc.
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("xsltproc" ,libxslt)))
    (native-search-paths
     ;; This file is produced by the gdk-pixbuf-loaders-cache-file
     ;; profile hook.
     (list (search-path-specification
            (variable "GDK_PIXBUF_MODULE_FILE")
            (files (list %gdk-pixbuf-loaders-cache-file))
            (separator #f)              ;single valued
            (file-type 'regular))))
    (synopsis "Image loading library")
    (description "GdkPixbuf is a library that loads image data in various
formats and stores it as linear buffers in memory.  The buffers can then be
scaled, composited, modified, saved, or rendered.")
    (home-page "https://wiki.gnome.org/Projects/GdkPixbuf")
    (license license:lgpl2.1+)))

;;; A minimal variant used to prevent a cycle with Inkscape.
(define-public at-spi2-core-minimal
  (hidden-package
   (package
     (name "at-spi2-core")
     (version "2.40.0")
     (source (origin
               (method url-fetch)
               (uri (string-append "mirror://gnome/sources/" name "/"
                                   (version-major+minor version)  "/"
                                   name "-" version ".tar.xz"))
               (sha256
                (base32
                 "0a9l6cfxynjn6jcp29d72i75xbkrzs1l5kmqcwmfal801b9sg5j1"))))
     (build-system meson-build-system)
     (arguments
      '(#:glib-or-gtk? #t    ; To wrap binaries and/or compile schemas
        #:phases
        (modify-phases %standard-phases
          (add-after 'install 'check
            (lambda _
              (setenv "HOME" (getenv "TMPDIR")) ; xfconfd requires a writable HOME
              ;; Run test-suite under a dbus session.
              (setenv "XDG_DATA_DIRS" ; for finding org.xfce.Xfconf.service
                      (string-append %output "/share"))
              ;; Don't fail on missing  '/etc/machine-id'.
              (setenv "DBUS_FATAL_WARNINGS" "0") ;
              (invoke "dbus-launch" "ninja" "test")))
          (delete 'check))))
     (inputs
      (list bash-minimal))
     (propagated-inputs
      ;; atspi-2.pc refers to all these.
      (list dbus glib libx11 libxi libxtst))
     (native-inputs
      `(("gettext" ,gettext-minimal)
        ("glib" ,glib "bin")
        ("gobject-introspection" ,gobject-introspection)
        ("pkg-config" ,pkg-config)
        ("python" ,python-wrapper)))
     (synopsis "Assistive Technology Service Provider Interface, core components")
     (description
      "The Assistive Technology Service Provider Interface, core components,
is part of the GNOME accessibility project.")
     (license license:lgpl2.1+)
     (home-page "https://wiki.gnome.org/Accessibility/"))))

(define-public at-spi2-core
  (package/inherit at-spi2-core-minimal
    (outputs (cons "doc" (package-outputs at-spi2-core-minimal)))
    (arguments
     (substitute-keyword-arguments (package-arguments at-spi2-core-minimal)
       ((#:configure-flags flags ''())
        `(cons ,(if (%current-target-system)
                    "-Ddocs=false"
                    "-Ddocs=true")
               ,flags))
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'set-documentation-path
             (lambda* (#:key outputs #:allow-other-keys)
               ;; Ensure that the cross-references point to the "doc" output.
               (substitute* "doc/libatspi/meson.build"
                 (("docpath =.*")
                  (string-append "docpath = '" (assoc-ref outputs "doc")
                                 "/share/gtk-doc/html'\n")))))
           (add-before 'install 'prepare-doc-directory
             (lambda* (#:key outputs #:allow-other-keys)
               (mkdir-p (string-append (assoc-ref outputs "doc") "/share"))))
           ,@(if (%current-target-system)
                 '()
                 '((add-after 'install 'move-documentation
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let ((out (assoc-ref outputs "out"))
                             (doc (assoc-ref outputs "doc")))
                         (copy-recursively
                          (string-append out "/share/gtk-doc")
                          (string-append doc "/share/gtk-doc"))
                         (delete-file-recursively
                          (string-append out "/share/gtk-doc")))))))))))
    (native-inputs
     (append `(("docbook-xml" ,docbook-xml-4.3)
               ("gtk-doc" ,gtk-doc/stable)
               ("libxml2" ,libxml2))    ;for XML_CATALOG_FILES
         (package-native-inputs at-spi2-core-minimal)))
    (properties (alist-delete 'hidden?
                              (package-properties at-spi2-core-minimal)))))

(define-public at-spi2-atk
  (package
    (name "at-spi2-atk")
    (version "2.38.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0ks6r9sx27l80n3a7yjmkilxv48cqj183wc7cap3caw2myjhi86g"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       ;; Compiling tests requires "libxml2" to be in 'inputs'.
       ,@(if (%current-target-system)
             `(#:configure-flags '("-Dtests=false"))
             '())
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           ;; Run test-suite under a dbus session.
           (lambda _
             (setenv "DBUS_FATAL_WARNINGS" "0")
             (invoke "dbus-launch" "meson" "test"))))))
    (propagated-inputs
     (list at-spi2-core-minimal)) ; required by atk-bridge-2.0.pc
    (inputs
     (list atk glib))
    (native-inputs
     (list dbus ; For tests
           gobject-introspection libxml2 pkg-config))
    (synopsis "Assistive Technology Service Provider Interface, ATK bindings")
    (description
     "The Assistive Technology Service Provider Interface
is part of the GNOME accessibility project.")
    (license license:lgpl2.1+)
    (home-page "https://wiki.gnome.org/Accessibility/")))

(define-public gtk+-2
  (package
    (name "gtk+")
    (version "2.24.33")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1nn6kks1zyvb5xikr9y2k7r9bwjy1g4b0m0s66532bclymbwfamc"))
              (patches (search-patches "gtk2-respect-GUIX_GTK2_PATH.patch"
                                       "gtk2-respect-GUIX_GTK2_IM_MODULE_FILE.patch"
                                       "gtk2-theme-paths.patch"
                                       "gtk2-fix-builder-test.patch"))))
    (build-system gnu-build-system)
    (outputs '("out" "bin" "doc" "debug"))
    (propagated-inputs
     (list atk cairo
           (if (target-x86-64?)
             librsvg-bootstrap
             librsvg-2.40)
           glib pango))
    (inputs
     (list cups
           libx11
           libxcomposite
           libxcursor
           libxext
           libxdamage
           libxi
           libxinerama
           libxkbcommon
           libxrandr
           libxrender
           libxshmfence))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")
           gobject-introspection
           intltool
           perl
           pkg-config
           python-wrapper
           xorg-server-for-tests))
    (arguments
     `(#:parallel-tests? #f
       #:configure-flags
       (list "--with-xinput=yes"
             (string-append "--with-html-dir="
                            (assoc-ref %outputs "doc")
                            "/share/gtk-doc/html"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-failing-tests
           (lambda _
             (substitute* "gtk/Makefile.in"
               (("aliasfilescheck\\.sh") ""))
             (substitute* "gtk/tests/recentmanager.c"
               (("g_test_add_func \\(\"/recent-manager.*;") ""))
             (substitute* "gtk/tests/defaultvalue.c"
               (("return g_test_run\\(\\);") ""))
             #t))
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
             #t))
         (add-after 'install 'remove-cache
           (lambda* (#:key outputs #:allow-other-keys)
	     (for-each
	      delete-file
	      (find-files (assoc-ref outputs "out") "immodules.cache"))
             #t)))))
    (native-search-paths
     (list (search-path-specification
            (variable "GUIX_GTK2_PATH")
            (files '("lib/gtk-2.0")))))
    (search-paths native-search-paths)
    (synopsis "Cross-platform toolkit for creating graphical user interfaces")
    (description
     "GTK+, or the GIMP Toolkit, is a multi-platform toolkit for creating
graphical user interfaces.  Offering a complete set of widgets, GTK+ is
suitable for projects ranging from small one-off tools to complete
application suites.")
    (license license:lgpl2.0+)
    (home-page "https://www.gtk.org/")))

(define-public gtk+
  (package
    (inherit gtk+-2)
    (name "gtk+")
    (version "3.24.30")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version)  "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1a9vg840fjq1mmm403b67k624qrkxh9shaz9pv7z9l8a6bzvyxds"))
       (patches (search-patches "gtk3-respect-GUIX_GTK3_PATH.patch"
                                "gtk3-respect-GUIX_GTK3_IM_MODULE_FILE.patch"))))
    (propagated-inputs
     (list atk
           at-spi2-atk
           cairo
           fribidi
           fontconfig
           freetype
           (if (target-x86-64?)
             librsvg-bootstrap
             librsvg-2.40)
           glib
           libcloudproviders-minimal
           libepoxy
           libx11
           libxcomposite
           libxcursor
           libxdamage
           libxext
           libxfixes
           libxi
           libxinerama
           libxkbcommon
           libxrandr
           libxrender
           mesa
           pango
           wayland
           wayland-protocols))
    (inputs
     (list colord-minimal ;to prevent a cycle with inkscape
           cups
           graphene
           harfbuzz
           iso-codes
           json-glib-minimal
           libxml2
           rest))
    (native-inputs
     (list docbook-xml-4.1.2
           gettext-minimal
           `(,glib "bin")
           gobject-introspection
           hicolor-icon-theme
           perl
           pkg-config
           python-wrapper
           sassc
           ;; By using a special xorg-server for GTK+'s tests, we reduce the impact
           ;; of updating xorg-server directly on the master branch.
           xorg-server-for-tests
           libxslt))
    (arguments
     `(#:imported-modules ((guix build glib-or-gtk-build-system)
                           ,@%gnu-build-system-modules)
       #:modules ((guix build utils)
                  (guix build gnu-build-system)
                  ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:))
       #:disallowed-references (,xorg-server-for-tests)
       ;; 47 MiB goes to "out" (24 of which is locale data!), and 26 MiB goes
       ;; to "doc".
       #:configure-flags (list (string-append "--with-html-dir="
                                              (assoc-ref %outputs "doc")
                                              "/share/gtk-doc/html")
                               "--enable-cloudproviders"
                               ;; The header file <gdk/gdkwayland.h> is required
                               ;; by gnome-control-center
                               "--enable-wayland-backend"
                               ;; This is necessary to build both backends.
                               "--enable-x11-backend"
                               ;; This enables the HTML5 websocket backend.
                               "--enable-broadway-backend")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'generate-gdk-pixbuf-loaders-cache-file
           (assoc-ref glib-or-gtk:%standard-phases
                      'generate-gdk-pixbuf-loaders-cache-file))
         (add-after 'unpack 'disable-failing-tests
           (lambda _
             (substitute* "testsuite/gtk/Makefile.in"
               (("builderparser cellarea check-icon-names check-cursor-names")
                "builderparser cellarea check-cursor-names")
               (("notify no-gtk-init object objects-finalize papersize rbtree")
                "no-gtk-init papersize rbtree")
               (("stylecontext templates textbuffer textiter treemodel treepath")
                "stylecontext textbuffer textiter treemodel treepath"))
             (substitute* "testsuite/a11y/Makefile.in"
               (("accessibility-dump tree-performance text children derive")
                "tree-performance text children derive"))
             (substitute* "testsuite/reftests/Makefile.in"
               (("TEST_PROGS = gtk-reftest")
                "TEST_PROGS = "))
             #t))
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
             #t))
         (add-after 'install 'move-desktop-files
           ;; Move desktop files into 'bin' to avoid cycle references.
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (bin (assoc-ref outputs "bin")))
               (mkdir-p (string-append bin "/share"))
               (rename-file (string-append out "/share/applications")
                            (string-append bin "/share/applications"))
               #t))))))
    (native-search-paths
     (list (search-path-specification
            (variable "GUIX_GTK3_PATH")
            (files '("lib/gtk-3.0")))))))

(define-public gtk
  (package
    (name "gtk")
    (version "4.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version)  "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32 "1x6xlc063nqp7cg6py4kq1kpw9pkq49ifk5kki0brc667ncdmahg"))
       (patches
        (search-patches "gtk4-respect-GUIX_GTK4_PATH.patch"
                        "gtk-introspection-test.patch"))))
    (build-system meson-build-system)
    (outputs '("out" "bin" "doc"))
    (arguments
     `(#:modules ((guix build utils)
                  (guix build meson-build-system)
                  ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:))
       #:configure-flags
       (list
        "-Dbroadway-backend=true"      ;for broadway display-backend
        "-Dcloudproviders=enabled"     ;for cloud-providers support
        "-Dtracker=enabled"            ;for filechooser search support
        "-Dcolord=enabled"             ;for color printing support
        ,@(if (%current-target-system)
              ;; If true, gtkdoc-scangobj will try to execute a
              ;; cross-compiled binary.
              '("-Dgtk_doc=false")
              '("-Dgtk_doc=true"))
        "-Dman-pages=true")
       #:parallel-tests? #f             ;parallel tests are not supported
       #:test-options '("--setup=x11"   ;defaults to wayland
                        ;; Use the same test options as upstream uses for
                        ;; their CI.
                        "--suite=gtk"
                        "--no-suite=gsk-compare-broadway")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'generate-gdk-pixbuf-loaders-cache-file
           (assoc-ref glib-or-gtk:%standard-phases
                      'generate-gdk-pixbuf-loaders-cache-file))
         (add-after 'unpack 'patch
           (lambda* (#:key inputs native-inputs outputs #:allow-other-keys)
             ;; Correct DTD resources of docbook.
             (substitute* (find-files "docs" "\\.xml$")
               (("http://www.oasis-open.org/docbook/xml/4.3/")
                (string-append
                 (assoc-ref (or native-inputs inputs) "docbook-xml-4.3")
                 "/xml/dtd/docbook/")))
             ;; Disable building of icon cache.
             (substitute* "meson.build"
               (("gtk_update_icon_cache: true")
                "gtk_update_icon_cache: false"))
             ;; Disable failing tests.
             (substitute* (find-files "testsuite" "meson.build")
               (("[ \t]*'empty-text.node',") "")
               (("[ \t]*'testswitch.node',") "")
               (("[ \t]*'widgetfactory.node',") ""))
             (substitute* "testsuite/reftests/meson.build"
               (("[ \t]*'label-wrap-justify.ui',") "")) ))
         (add-before 'build 'set-cache
           (lambda _
             (setenv "XDG_CACHE_HOME" (getcwd))))
         (add-before 'check 'pre-check
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Tests require a running X server.
             (system "Xvfb :1 +extension GLX &")
             (setenv "DISPLAY" ":1")
             ;; Tests write to $HOME.
             (setenv "HOME" (getcwd))
             ;; Tests look for those variables.
             (setenv "XDG_RUNTIME_DIR" (getcwd))
             ;; For missing '/etc/machine-id'.
             (setenv "DBUS_FATAL_WARNINGS" "0")
             ;; Required for the calendar test.
             (setenv "TZDIR" (search-input-directory inputs
                                                     "share/zoneinfo"))))
         (add-after 'install 'move-files
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (assoc-ref outputs "bin"))
                    (doc (assoc-ref outputs "doc")))
               (for-each mkdir-p
                         (list
                          (string-append bin "/bin")
                          (string-append bin "/share/applications")
                          (string-append bin "/share/icons")
                          (string-append bin "/share/man")
                          (string-append bin "/share/metainfo")
                          (string-append doc "/share/doc")))
               ;; Move programs and related files to output 'bin'.
               (for-each (lambda (dir)
                           (rename-file
                            (string-append out dir)
                            (string-append bin dir)))
                         (list
                          "/bin"
                          "/share/applications"
                          "/share/icons"
                          "/share/man"
                          "/share/metainfo"))
               ;; Move HTML documentation to output 'doc'.
               (rename-file
                (string-append out "/share/doc")
                (string-append doc "/share/doc"))))))))
    (native-inputs
     `(("docbook-xml-4.3" ,docbook-xml-4.3)
       ("docbook-xsl" ,docbook-xsl)
       ("gettext-minimal" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection) ;for building introspection data
       ("graphene" ,graphene)
       ("gtk-doc" ,gtk-doc)             ;for building documentation
       ("intltool" ,intltool)
       ("libxslt" ,libxslt)             ;for building man-pages
       ("pkg-config" ,pkg-config)
       ("python-pygobject" ,python-pygobject)
       ;; These python modules are required for building documentation.
       ("python-jinja2" ,python-jinja2)
       ("python-markdown" ,python-markdown)
       ("python-markupsafe" ,python-markupsafe)
       ("python-pygments" ,python-pygments)
       ("python-toml" ,python-toml)
       ("python-typogrify" ,python-typogrify)
       ("sassc" ,sassc)                 ;for building themes
       ("tzdata" ,tzdata-for-tests)
       ("vala" ,vala)
       ("xorg-server-for-tests" ,xorg-server-for-tests)))
    (inputs
     (list colord ;for color printing support
           cups ;for CUPS print-backend
           ffmpeg ;for ffmpeg media-backend
           fribidi
           gstreamer ;for gstreamer media-backend
           gst-plugins-bad ;provides gstreamer-player
           gst-plugins-base ;provides gstreamer-gl
           harfbuzz
           iso-codes
           json-glib
           libcloudproviders ;for cloud-providers support
           librsvg
           python
           rest
           tracker))          ;for filechooser search support
    (propagated-inputs
     ;; Following dependencies are referenced in .pc files.
     `(("cairo" ,cairo)
       ("fontconfig" ,fontconfig)
       ("librsvg" ,librsvg)
       ("glib" ,glib)
       ("graphene" ,graphene)
       ("libepoxy" ,libepoxy)
       ("libx11" ,libx11)               ;for x11 display-backend
       ("libxcomposite" ,libxcomposite)
       ("libxcursor" ,libxcursor)
       ("libxdamage" ,libxdamage)
       ("libxext" ,libxext)
       ("libxfixes" ,libxfixes)
       ("libxi" ,libxi)
       ("libxinerama" ,libxinerama)     ;for xinerama support
       ("libxkbcommon" ,libxkbcommon)
       ("libxrandr" ,libxrandr)
       ("libxrender" ,libxrender)
       ("pango" ,pango)
       ("vulkan-headers" ,vulkan-headers)
       ("vulkan-loader" ,vulkan-loader) ;for vulkan graphics API support
       ("wayland" ,wayland)             ;for wayland display-backend
       ("wayland-protocols" ,wayland-protocols)))
    (native-search-paths
     (list
      (search-path-specification
       (variable "GUIX_GTK4_PATH")
       (files '("lib/gtk-4.0")))))
    (search-paths native-search-paths)
    (home-page "https://www.gtk.org/")
    (synopsis "Cross-platform widget toolkit")
    (description "GTK is a multi-platform toolkit for creating graphical user
interfaces.  Offering a complete set of widgets, GTK is suitable for projects
ranging from small one-off tools to complete application suites.")
    (license license:lgpl2.1+)))

;;;
;;; Guile bindings.
;;;

(define-public guile-cairo
  (package
    (name "guile-cairo")
    (version "1.11.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/guile-cairo/guile-cairo-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0yx0844p61ljd4d3d63qrawiygiw6ks02fwv2cqx7nav5kfd8ck2"))
              (modules '((guix build utils)))
              (snippet
               (begin
                 '(begin
                    ;; Install Scheme files in …/guile/site/X.Y.
                    (substitute* (find-files "." "^Makefile\\.in$")
                      (("^(.*)dir = (.*)/guile/site(.*)" _ name prefix suffix)
                       (string-append name "dir = " prefix
                                      "/guile/site/@GUILE_EFFECTIVE_VERSION@"
                                      suffix)))
                    #t)))))
    (build-system gnu-build-system)
    (arguments
     ;; Uses of 'scm_t_uint8' & co. are deprecated; don't stop the build
     ;; because of them.
     `(#:configure-flags '("--disable-Werror")
       #:make-flags '("GUILE_AUTO_COMPILE=0") ; to prevent guild warnings
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 rdelim)
                  (ice-9 popen))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-go-files
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (effective (read-line
                                (open-pipe* OPEN_READ
                                            "guile" "-c"
                                            "(display (effective-version))")))
                    (module-dir (string-append out "/share/guile/site/"
                                               effective))
                    (object-dir (string-append out "/lib/guile/" effective
                                               "/site-ccache"))
                    (prefix     (string-length module-dir)))
               ;; compile to the destination
               (for-each (lambda (file)
                           (let* ((base (string-drop (string-drop-right file 4)
                                                     prefix))
                                  (go   (string-append object-dir base ".go")))
                             (invoke "guild" "compile" "-L" module-dir
                                     file "-o" go)))
                         (find-files module-dir "\\.scm$"))
               #t))))))
    (inputs
     (list guile-lib expat guile-3.0))
    (propagated-inputs
     ;; The .pc file refers to 'cairo'.
     (list cairo))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.nongnu.org/guile-cairo/")
    (synopsis "Cairo bindings for GNU Guile")
    (description
     "Guile-Cairo wraps the Cairo graphics library for Guile Scheme.
Guile-Cairo is complete, wrapping almost all of the Cairo API.  It is API
stable, providing a firm base on which to do graphics work.  Finally, and
importantly, it is pleasant to use.  You get a powerful and well-maintained
graphics library with all of the benefits of Scheme: memory management,
exceptions, macros, and a dynamic programming environment.")
    (license license:lgpl3+)))

(define-public guile2.2-cairo
  (package
    (inherit guile-cairo)
    (name "guile2.2-cairo")
    (inputs
     `(("guile" ,guile-2.2)
       ("guile-lib" ,guile2.2-lib)
       ,@(fold alist-delete (package-inputs guile-cairo)
               '("guile" "guile-lib"))))))

(define-public guile-rsvg
  ;; Use a recent snapshot that supports Guile 2.2 and beyond.
  (let ((commit "05c6a2fd67e4fea1a7c3ff776729dc931bae6678")
        (revision "0"))
    (package
      (name "guile-rsvg")
      (version (string-append "2.18.1-" revision "."
                              (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.com/wingo/guile-rsvg/")
                      (commit commit)))
                (sha256
                 (base32
                  "0cnbl40df2sbhpc32cma6j6w312rfvcgbxxqaixgf0ymim3fb248"))
                (patches (search-patches "guile-rsvg-pkgconfig.patch"))
                (modules '((guix build utils)))
                (snippet
                 '(begin
                    (substitute* (find-files "." "Makefile\\.am")
                      (("/share/guile/site")
                       "/share/guile/site/@GUILE_EFFECTIVE_VERSION@"))
                    #t))
                (file-name (string-append name "-" version ".tar.gz"))))
      (build-system gnu-build-system)
      (arguments
       `(#:modules ((guix build gnu-build-system)
                    (guix build utils)
                    (ice-9 rdelim)
                    (ice-9 popen))
         #:phases
         (modify-phases %standard-phases
           (replace 'bootstrap
             (lambda _
               (invoke "autoreconf" "-vfi")))
           (add-after 'install 'install-go-files
             (lambda* (#:key outputs inputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (effective (read-line
                                  (open-pipe* OPEN_READ
                                              "guile" "-c"
                                              "(display (effective-version))")))
                      (module-dir (string-append out "/share/guile/site/"
                                                 effective))
                      (object-dir (string-append out "/lib/guile/" effective
                                                 "/site-ccache"))
                      (prefix     (string-length module-dir)))
                 ;; compile to the destination
                 (for-each (lambda (file)
                             (let* ((base (string-drop (string-drop-right file 4)
                                                       prefix))
                                    (go   (string-append object-dir base ".go")))
                               (invoke "guild" "compile" "-L" module-dir
                                       file "-o" go)))
                           (find-files module-dir "\\.scm$"))
                 #t))))))
      (native-inputs (list pkg-config autoconf automake libtool texinfo))
      (inputs (list guile-3.0
                    (librsvg-for-system) guile-lib))        ;for (unit-test)
      (propagated-inputs (list guile-cairo))
      (synopsis "Render SVG images using Cairo from Guile")
      (description
       "Guile-RSVG wraps the RSVG library for Guile, allowing you to render SVG
images onto Cairo surfaces.")
      (home-page "https://wingolog.org/projects/guile-rsvg/")
      (license license:lgpl2.1+))))

(define-public guile2.2-rsvg
  (package
    (inherit guile-rsvg)
    (name "guile2.2-rsvg")
    (inputs
     `(("guile" ,guile-2.2)
       ("guile-lib" ,guile2.2-lib)
       ,@(fold alist-delete (package-inputs guile-rsvg)
               '("guile" "guile-lib"))))
    (propagated-inputs `(("guile-cairo" ,guile2.2-cairo)))))

(define-public guile-present
  (package
    (name "guile-present")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://wingolog.org/pub/guile-present/"
                                  "guile-present-" version ".tar.gz"))
              (sha256
               (base32
                "1qam447m05sxxv6x8dlzg7qnyfc4dh8apjw1idpfhpns671gfr6m"))
              (patches (search-patches "guile-present-coding.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Allow builds with Guile 3.0.
                  (substitute* "configure"
                    (("2\\.2 2\\.0")
                     "3.0 2.2 2.0"))

                  ;; Install .go files in the right place.
                  (substitute* "Makefile.in"
                    (("/ccache") "/site-ccache"))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'post-install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (bin   (string-append out "/bin"))
                    (guile (assoc-ref inputs "guile"))
                    (version
                     ,(match (assoc "guile" (package-inputs this-package))
                        (("guile" guile)
                         (version-major+minor (package-version guile))))))
               (substitute* (find-files bin ".*")
                 (("guile")
                  (string-append guile "/bin/guile -L "
                                 out "/share/guile/site/" version " -C "
                                 out "/lib/guile/" version "/site-ccache "))))
             #t)))))
    (native-inputs (list pkg-config))
    (inputs (list guile-3.0))
    (propagated-inputs
     ;; These are used by the (present …) modules.
     (list guile-lib guile-cairo guile-rsvg))
    (home-page "https://wingolog.org/software/guile-present/")
    (synopsis "Create SVG or PDF presentations in Guile")
    (description
     "Guile-Present defines a declarative vocabulary for presentations,
together with tools to render presentation documents as SVG or PDF.
Guile-Present can be used to make presentations programmatically, but also
includes a tools to generate PDF presentations out of Org mode and Texinfo
documents.")
    (license license:lgpl3+)))

(define-public guile2.2-present
  (package
    (inherit guile-present)
    (name "guile2.2-present")
    (inputs (list guile-2.2))
    (propagated-inputs
     `(("guile-lib" ,guile2.2-lib)
       ("guile-cairo" ,guile2.2-cairo)
       ("guile-rsvg" ,guile2.2-rsvg)))))

(define-public guile-gnome
   (package
    (name "guile-gnome")
    (version "2.16.5")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "mirror://gnu/" name
                              "/guile-gnome-platform/guile-gnome-platform-"
                              version ".tar.gz"))
             (sha256
              (base32
               "1gnf3j96nip5kl99a268i0dy1hj7s1cfs66sps3zwysnkd7qr399"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config
           atk
           ;;("corba" ,corba) ; not packaged yet
           gconf
           gobject-introspection
           ;;("gthread" ,gthread) ; not packaged yet
           gnome-vfs
           gdk-pixbuf
           gtk+-2
           libglade
           libgnome
           libgnomecanvas
           libgnomeui
           pango
           libffi
           glib))
    (inputs (list guile-2.2))
    (propagated-inputs
     `(("guile-cairo" ,guile2.2-cairo)
       ("g-wrap" ,g-wrap)
       ("guile-lib" ,guile2.2-lib)))
    (arguments
      `(#:tests? #f                               ;FIXME
        #:phases (modify-phases %standard-phases
                   (add-before 'configure 'pre-configure
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let ((out (assoc-ref outputs "out")))
                         (substitute* (find-files "." "^Makefile.in$")
                           (("guilesite :=.*guile/site" all)
                            (string-append all "/@GUILE_EFFECTIVE_VERSION@")))
                         #t))))))
    (outputs '("out" "debug"))
    (synopsis "Guile interface for GTK+ programming for GNOME")
    (description
     "Includes guile-clutter, guile-gnome-gstreamer,
guile-gnome-platform (GNOME developer libraries), and guile-gtksourceview.")
    (home-page "https://www.gnu.org/software/guile-gnome/")
    (license license:gpl2+)
    (properties '((upstream-name . "guile-gnome-platform")
                  (ftp-directory . "/gnu/guile-gnome/guile-gnome-platform")))))

;;;
;;; C++ bindings.
;;;

(define-public cairomm
  (package
    (name "cairomm")
    (version "1.16.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.cairographics.org/releases/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1ya4y7qa000cjawqwswbqv26y5icfkmhs5iiiil4dxgrqn91923y"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:configure-flags
       (list
        "-Dbuild-documentation=true"
        "-Dboost-shared=true")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'move-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc")))
               (mkdir-p (string-append doc "/share"))
               (rename-file
                (string-append out "/share/doc")
                (string-append doc "/share/doc"))
               #t))))))
    (native-inputs
     `(("boost" ,boost)
       ("dot" ,graphviz)
       ("doxygen" ,doxygen)
       ("mm-common" ,mm-common)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("xsltproc" ,libxslt)))
    (inputs
     `(("fontconfig" ,fontconfig)))
    (propagated-inputs
     (list libsigc++ cairo))
    (home-page "https://cairographics.org/")
    (synopsis "C++ bindings to the Cairo 2D graphics library")
    (description
     "Cairomm provides a C++ programming interface to the Cairo 2D graphics
library.")
    (license license:lgpl2.0+)))

(define-public cairomm-1.14
  (package
    (inherit cairomm)
    (name "cairomm")
    (version "1.14.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://www.cairographics.org/releases/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "1qwdj9xw1w651kqwh82nipbryimm1ir5n3c6q34nphsx576bj9h1"))))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs cairomm)
       (prepend libsigc++-2)))))

(define-public pangomm
  (package
    (name "pangomm")
    (version "2.48.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0y2vyp6azvhrii6rzs89kr08wg8z1p562awyr812131zqdsd83ly"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:configure-flags
       (list
        "-Dbuild-documentation=true")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'move-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc")))
               (mkdir-p (string-append doc "/share"))
               (rename-file
                (string-append out "/share/doc")
                (string-append doc "/share/doc"))
               #t))))))
    (native-inputs
     `(("dot" ,graphviz)
       ("doxygen" ,doxygen)
       ("m4" ,m4)
       ("mm-common" ,mm-common)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("xsltproc" ,libxslt)))
    (propagated-inputs
     (list cairo cairomm glibmm pango))
    (home-page "https://pango.gnome.org//")
    (synopsis "C++ interface to the Pango text rendering library")
    (description
     "Pangomm provides a C++ programming interface to the Pango text rendering
library.")
    (license license:lgpl2.1+)))

(define-public pangomm-2.46
  (package
    (inherit pangomm)
    (name "pangomm")
    (version "2.46.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version)  "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "06zczkaxf5p5kjgnzrfylzi40w9a8lxpndgs7rpn12qrsq27sy6k"))))
    (propagated-inputs
     (list cairomm-1.14 glibmm-2.64 pango))))

(define-public atkmm
  (package
    (name "atkmm")
    (version "2.36.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0wwr0663jrqx2klsasffd9wpk3kqnwisj1y3ahdkjdk5hzrsjgy9"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:configure-flags
       (list
        "-Dbuild-documentation=true")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'move-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc")))
               (mkdir-p (string-append doc "/share"))
               (rename-file
                (string-append out "/share/doc")
                (string-append doc "/share/doc"))))))))
    (native-inputs
     `(("dot" ,graphviz)
       ("doxygen" ,doxygen)
       ("m4" ,m4)
       ("mm-common" ,mm-common)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("xsltproc" ,libxslt)))
    (propagated-inputs
     (list glibmm atk))
    (synopsis "C++ bindings for ATK")
    (description "ATKmm is the C++ binding for the ATK library.")
    (home-page "https://wiki.gnome.org/Accessibility")
    (license
     (list
      ;; Library
      license:lgpl2.1+
      ;; Tools
      license:gpl2+))))

(define-public atkmm-2.28
  (package
    (inherit atkmm)
    (name "atkmm")
    (version "2.28.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version)  "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "1b8vycqzr3lfvk2l73f4kk74hj48081zbh9r1r2ilr3h8xh7cs0i"))))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs atkmm)
       (replace "glibmm" glibmm-2.64)))))

(define-public gtkmm
  (package
    (name "gtkmm")
    (version "4.4.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version)  "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "1nhdf1s437k41af6frbqw2sky46qci0hgkg9h86a9rlnc0r69d1f"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:configure-flags '("-Dbuild-documentation=true")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-cache
           (lambda _
             (setenv "XDG_CACHE_HOME" (getcwd))))
         (add-before 'check 'pre-check
           (lambda _
             ;; Tests require a running X server.
             (system "Xvfb :1 +extension GLX &")
             (setenv "DISPLAY" ":1")
             ;; Tests write to $HOME.
             (setenv "HOME" (getcwd))
             (setenv "XDG_RUNTIME_DIR" (getcwd))
             ;; For missing '/etc/machine-id'.
             (setenv "DBUS_FATAL_WARNINGS" "0")))
         (add-after 'install 'move-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc")))
               (mkdir-p (string-append doc "/share"))
               (rename-file
                (string-append out "/share/doc")
                (string-append doc "/share/doc"))))))))
    (native-inputs
     `(("dot" ,graphviz)
       ("doxygen" ,doxygen)
       ("glib:bin" ,glib "bin")
       ("m4" ,m4)
       ("mm-common" ,mm-common)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("xsltproc" ,libxslt)
       ("xorg-server" ,xorg-server-for-tests)))
    (propagated-inputs
     (list atkmm cairomm glibmm gtk pangomm))
    (synopsis "C++ Interfaces for GTK+ and GNOME")
    (description "GTKmm is the official C++ interface for the popular GUI
library GTK+.  Highlights include typesafe callbacks, and a comprehensive set
of widgets that are easily extensible via inheritance.  You can create user
interfaces either in code or with the Glade User Interface designer, using
libglademm.  There's extensive documentation, including API reference and a
tutorial.")
    (home-page "https://gtkmm.org/")
    (license
     (list
      ;; Library
      license:lgpl2.1+
      ;; Tools
      license:gpl2+))))

(define-public gtkmm-3
  (package
    (inherit gtkmm)
    (name "gtkmm")
    (version "3.24.5")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version)  "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "1ri2msp3cmzi6r65ghwb8gfavfaxv0axpwi3q60nm7v8hvg36qw5"))))
    (propagated-inputs
     `(("atkmm-2.28" ,atkmm-2.28)
       ("cairomm-1.14" ,cairomm-1.14)
       ("glibmm" ,glibmm)
       ("gtk+" ,gtk+)
       ("pangomm-2.42" ,pangomm-2.46)))))

(define-public gtkmm-2
  (package
    (inherit gtkmm)
    (name "gtkmm")
    (version "2.24.5")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version)  "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "0wkbzvsx4kgw16f6xjdc1dz7f77ldngdila4yi5lw2zrgcxsb006"))))
    (build-system gnu-build-system)
    (arguments
     (strip-keyword-arguments
      '(#:configure-flags) (package-arguments gtkmm)))
    (propagated-inputs
     (list atkmm-2.28 cairomm-1.14 glibmm-2.64 gtk+-2 pangomm-2.46))))

(define-public gtksourceviewmm
  (package
    (name "gtksourceviewmm")
    (version "3.18.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32 "0fgvmhm4h4qmxig87qvangs6ijw53mi40siz7pixlxbrsgiil22i"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (propagated-inputs
     ;; In 'Requires' of gtksourceviewmm-3.0.pc.
     (list glibmm gtkmm-3 gtksourceview-3))
    (synopsis "C++ interface to the GTK+ 'GtkTextView' widget")
    (description
     "gtksourceviewmm is a portable C++ library that extends the standard GTK+
framework for multiline text editing with support for configurable syntax
highlighting, unlimited undo/redo, search and replace, a completion framework,
printing and other features typical of a source code editor.")
    (license license:lgpl2.1+)
    (home-page "https://developer.gnome.org/gtksourceview/")))

;;;
;;; Python bindings.
;;;

(define-public python-pycairo
  (package
    (name "python-pycairo")
    (version "1.20.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://github.com/pygobject/pycairo/releases/download/v"
                          version "/pycairo-" version ".tar.gz"))
      (sha256
       (base32
        "1326aa2ybhhhrvz3n4p22z5sic25m016ddb5yq0hvbprnw6a35an"))))
    (build-system python-build-system)
    (native-inputs
     (list pkg-config python-pytest))
    (propagated-inputs                  ;pycairo.pc references cairo
     (list cairo))
    (home-page "https://cairographics.org/pycairo/")
    (synopsis "Python bindings for cairo")
    (description
     "Pycairo is a set of Python bindings for the Cairo graphics library.")
    (license license:lgpl3+)
    (properties `((python2-variant . ,(delay python2-pycairo))))))

;; Pycairo no longer supports Python 2 since version 1.19.0, so we stick
;; with this older version here.
(define-public python2-pycairo
  (let ((pycairo (package-with-python2
                  (strip-python2-variant python-pycairo))))
    (package
      (inherit pycairo)
      (version "1.18.2")
      (source (origin
                (method url-fetch)
                (uri (string-append "https://github.com/pygobject/pycairo/releases"
                                    "/download/v" version "/pycairo-" version ".tar.gz"))
                (sha256
                 (base32
                  "0cb5n4r4nl0k1g90b1gz9iyk4lp7hi03db98i1p52a870bym7f6w"))))
      ;; Dual-licensed under LGPL 2.1 or Mozilla Public License 1.1
      (license (list license:lgpl2.1 license:mpl1.1)))))

(define-public python2-pygtk
  (package
    (name "python2-pygtk")
    (version "2.24.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnome/sources"
                          "/pygtk/" (version-major+minor version)
                          "/pygtk-" version ".tar.bz2"))
      (sha256
       (base32
        "04k942gn8vl95kwf0qskkv6npclfm31d78ljkrkgyqxxcni1w76d"))))
    (build-system gnu-build-system)
    (outputs '("out"
               "doc"))                            ;13 MiB of gtk-doc HTML
    (native-inputs
     (list pkg-config))
    (inputs
     `(("python" ,python-2)

       ;; XXX: The package fails to build with the latest Pango (propagated
       ;; from GTK+2), so we provide it with this older version.
       ("pango" ,pango-1.42)

       ("libglade" ,libglade)
       ("glib"   ,glib)))
    (propagated-inputs
     `(("python-pycairo"   ,python2-pycairo)     ;loaded at runtime
       ("python-pygobject" ,python2-pygobject-2) ;referenced in pc file
       ("gtk+"             ,gtk+-2)))
    (arguments
     `(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'set-gtk-doc-directory
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; Install documentation to "doc".
                      (let ((doc (assoc-ref outputs "doc")))
                        (substitute* "docs/Makefile.in"
                          (("TARGET_DIR = \\$\\(datadir\\)")
                           (string-append "TARGET_DIR = " doc))))))
                  (add-after 'configure 'fix-codegen
                    (lambda* (#:key inputs #:allow-other-keys)
                      (substitute* "pygtk-codegen-2.0"
                        (("^prefix=.*$")
                         (string-append
                          "prefix="
                          (assoc-ref inputs "python-pygobject") "\n")))))
                  (add-after 'install 'install-pth
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      ;; pygtk's modules are stored in a subdirectory of
                      ;; python's site-packages directory.  Add a .pth file so
                      ;; that python will add that subdirectory to its module
                      ;; search path.
                      (let* ((out    (assoc-ref outputs "out"))
                             (site   (string-append out "/lib/python"
                                                    ,(version-major+minor
                                                      (package-version python-2))
                                                    "/site-packages")))
                        (call-with-output-file (string-append site "/pygtk.pth")
                          (lambda (port)
                            (format port "gtk-2.0~%")))))))))
    (home-page "http://www.pygtk.org/")
    (synopsis "Python bindings for GTK+")
    (description
     "PyGTK allows you to write full featured GTK programs in Python.  It is
targeted at GTK 2.x, and can be used in conjunction with gnome-python to
write GNOME applications.")
    (license license:lgpl2.1+)))

(define-public perl-cairo
  (package
    (name "perl-cairo")
    (version "1.109")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/X/XA/XAOC/Cairo-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0zq78dv22arg35ma6kah9cwfd1zx8gg7amsibzd128qw81p766c2"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-extutils-depends perl-extutils-pkgconfig))
    (propagated-inputs
     (list cairo))
    (home-page "https://metacpan.org/release/Cairo")
    (synopsis "Perl interface to the cairo 2d vector graphics library")
    (description "Cairo provides Perl bindings for the vector graphics library
cairo.  It supports multiple output targets, including PNG, PDF and SVG.  Cairo
produces identical output on all those targets.")
    (license license:lgpl2.1+)))

(define-public perl-cairo-gobject
  (package
    (name "perl-cairo-gobject")
    (version "1.005")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/X/XA/XAOC/"
                           "Cairo-GObject-" version ".tar.gz"))
       (sha256
        (base32 "0l2wcz77ndmbgvxx34gdm919a3dxh9fixqr47p50n78ysx2692cd"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-extutils-depends perl-extutils-pkgconfig))
    (propagated-inputs
     (list perl-cairo perl-glib))
    (home-page "https://metacpan.org/dist/Cairo-GObject")
    (synopsis "Integrate Cairo into the Glib type system")
    (description "Cairo::GObject registers Cairo's types with Glib's type systems,
so that they can be used normally in signals and properties.")
    (license license:lgpl2.1+)))

(define-public perl-gtk2
  (package
    (name "perl-gtk2")
    (version "1.24993")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/X/XA/XAOC/Gtk2-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0ry9jfvfgdwzalxcvwsgr7plhk3agx7p40l0fqdf3vrf7ds47i29"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-extutils-depends perl-extutils-pkgconfig))
    (inputs
     (list gtk+-2))
    (propagated-inputs
     (list perl-pango))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'remove-broken-test
           ;; See https://gitlab.gnome.org/GNOME/perl-gtk2/issues/3.
           (lambda _
             (substitute* "t/GdkPixbuf.t"
               (("tests => 112") "tests => 111")
               (("ok \\(defined \\$pixbuf, \"Don't crash on partial pixmap data\"\\);")
                "# ok (defined $pixbuf, \"Don't crash on partial pixmap data\");")))))))
    (home-page "https://metacpan.org/release/Gtk2")
    (synopsis "Perl interface to the 2.x series of the Gimp Toolkit library")
    (description "Perl bindings to the 2.x series of the Gtk+ widget set.
This module allows you to write graphical user interfaces in a Perlish and
object-oriented way, freeing you from the casting and memory management in C,
yet remaining very close in spirit to original API.")
    (license license:lgpl2.1+)))

(define-public perl-gtk3
  (package
    (name "perl-gtk3")
    (version "0.038")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/X/XA/XAOC/Gtk3-"
                           version ".tar.gz"))
       (sha256
        (base32 "1k3sfcvxxx7ir7ail7w1lkmr4np0k3criljzw5wir63lmbr4pp3h"))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             ;; Tests require a running X server.
             (system "Xvfb :1 +extension GLX &")
             (setenv "DISPLAY" ":1"))))))
    (native-inputs
     `(("adwaita-icon-theme" ,adwaita-icon-theme)
       ("gtk+:bin" ,gtk+ "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("perl-extutils-depends" ,perl-extutils-depends)
       ("perl-extutils-pkgconfig" ,perl-extutils-pkgconfig)
       ("perl-test-simple" ,perl-test-simple)
       ("xorg-server" ,xorg-server-for-tests)))
    (propagated-inputs
     (list gtk+ perl-cairo-gobject perl-carp perl-exporter
           perl-glib-object-introspection))
    (home-page "https://metacpan.org/dist/Gtk3")
    (synopsis "Perl interface to the 3.x series of the gtk+ toolkit")
    (description "Perl bindings to the 3.x series of the gtk+ toolkit.
This module allows you to write graphical user interfaces in a Perlish and
object-oriented way, freeing you from the casting and memory management in C,
yet remaining very close in spirit to original API.")
    (license license:lgpl2.1+)))

(define-public perl-pango
  (package
    (name "perl-pango")
    (version "1.227")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/X/XA/XAOC/Pango-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0wdcidnfnb6nm79fzfs39ivawj3x8m98a147fmcxgv1zvwia9c1l"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-extutils-depends perl-extutils-pkgconfig))
    (inputs
     (list pango))
    (propagated-inputs
     (list perl-cairo perl-glib))
    (home-page "https://metacpan.org/release/Pango")
    (synopsis "Layout and render international text")
    (description "Pango is a library for laying out and rendering text, with an
emphasis on internationalization.  Pango can be used anywhere that text layout
is needed, but using Pango in conjunction with Cairo and/or Gtk2 provides a
complete solution with high quality text handling and graphics rendering.

Dynamically loaded modules handle text layout for particular combinations of
script and font backend.  Pango provides a wide selection of modules, including
modules for Hebrew, Arabic, Hangul, Thai, and a number of Indic scripts.
Virtually all of the world's major scripts are supported.

In addition to the low level layout rendering routines, Pango includes
@code{Pango::Layout}, a high level driver for laying out entire blocks of text,
and routines to assist in editing internationalized text.")
    (license license:lgpl2.1+)))

(define-public girara
  (package
    (name "girara")
    (version "0.3.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.pwmt.org/pwmt/girara")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0whwwj31fxfaf4r4qvxb4kl3mj05xj3n9c6nzdn46r30bkg9z4dw"))))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("check" ,check-0.14)
                     ("gettext" ,gettext-minimal)
                     ("glib:bin" ,glib "bin")
                     ("xorg-server" ,xorg-server-for-tests)))
    ;; Listed in 'Requires.private' of 'girara.pc'.
    (propagated-inputs (list gtk+))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'check 'start-xserver
                    ;; Tests require a running X server.
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((xorg-server (assoc-ref inputs "xorg-server"))
                            (display ":1"))
                        (setenv "DISPLAY" display)

                        ;; On busy machines, tests may take longer than
                        ;; the default of four seconds.
                        (setenv "CK_DEFAULT_TIMEOUT" "20")

                        ;; Don't fail due to missing '/etc/machine-id'.
                        (setenv "DBUS_FATAL_WARNINGS" "0")
                        (zero? (system (string-append xorg-server "/bin/Xvfb "
                                                      display " &")))))))))
    (build-system meson-build-system)
    (home-page "https://pwmt.org/projects/girara/")
    (synopsis "Library for minimalistic gtk+3 user interfaces")
    (description "Girara is a library that implements a user interface that
focuses on simplicity and minimalism.  Currently based on GTK+, a
cross-platform widget toolkit, it provides an interface that focuses on three
main components: a so-called view widget that represents the actual
application, an input bar that is used to execute commands of the
application and the status bar which provides the user with current
information.")
    (license license:zlib)))

(define-public gtk-doc
  (package
    (name "gtk-doc")
    (version "1.33.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0hxza8qp52lrq7s1vbilz2vh4170cail560zi8khl0zb42d706yc"))
              (patches
               (search-patches "gtk-doc-respect-xml-catalog.patch"))))
    (build-system meson-build-system)
    (arguments
     `(#:parallel-tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-gtk-doc-scan
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "gtk-doc.xsl"
               (("http://docbook.sourceforge.net/release/xsl/current/html/chunk.xsl")
                (string-append (assoc-ref inputs "docbook-xsl")
                               "/xml/xsl/docbook-xsl-"
                               ,(package-version docbook-xsl)
                               "/html/chunk.xsl"))
               (("http://docbook.sourceforge.net/release/xsl/current/common/en.xml")
                (string-append (assoc-ref inputs "docbook-xsl")
                               "/xml/xsl/docbook-xsl-"
                               ,(package-version docbook-xsl)
                               "/common/en.xml")))
             #t))
         (add-after 'unpack 'disable-failing-tests
           (lambda _
             (substitute* "tests/Makefile.am"
               (("annotations.sh bugs.sh empty.sh fail.sh gobject.sh program.sh")
                ""))
             #t))
         (add-after 'install 'wrap-executables
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each (lambda (prog)
                           (wrap-program prog
                             `("GUIX_PYTHONPATH" ":" prefix (,(getenv "GUIX_PYTHONPATH")))))
                         (find-files (string-append out "/bin")))))))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("itstool" ,itstool)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python-wrapper" ,python-wrapper)))
    (inputs
     (list bc
           dblatex
           docbook-xml-4.3
           docbook-xsl
           glib
           libxml2
           libxslt
           python
           python-anytree
           python-lxml
           python-parameterized
           python-pygments
           python-unittest2
           source-highlight
           yelp-tools))
    (home-page "https://wiki.gnome.org/DocumentationProject/GtkDoc")
    (synopsis "GTK+ DocBook Documentation Generator")
    (description "GtkDoc is a tool used to extract API documentation from C-code
like Doxygen, but handles documentation of GObject (including signals and
properties) that makes it very suitable for GTK+ apps and libraries.  It uses
docbook for intermediate files and can produce html by default and pdf/man-pages
with some extra work.")
    (license
     (list
      ;; Docs.
      license:fdl1.1+
      ;; Others.
      license:gpl2+))))

;; This is a variant of the 'gtk-doc' package that is not updated often.  It
;; is intended to be used as a native-input at build-time only.  This allows
;; the main 'gtk-doc', 'dblatex' and 'imagemagick' packages to be freely
;; updated on the 'master' branch without triggering an excessive number of
;; rebuilds.
(define-public gtk-doc/stable
  (hidden-package
   (package/inherit gtk-doc
     (inputs (alist-replace "dblatex" `(,dblatex/stable)
                            (package-inputs gtk-doc))))))

(define-public gtk-engines
  (package
    (name "gtk-engines")
    (version "2.20.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1db65pb0j0mijmswrvpgkdabilqd23x22d95hp5kwxvcramq1dhm"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       `("--enable-animation")))
    (native-inputs
     (list pkg-config intltool))
    (inputs
     ;; Don't propagate GTK+ to reduce "profile pollution".
     (list gtk+-2)) ; required by gtk-engines-2.pc
    (home-page "https://live.gnome.org/GnomeArt")
    (synopsis "Theming engines for GTK+ 2.x")
    (description
     "This package contains the standard GTK+ 2.x theming engines including
Clearlooks, Crux, High Contrast, Industrial, LighthouseBlue, Metal, Mist,
Redmond95 and ThinIce.")
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public murrine
  (package
    (name "murrine")
    (version "0.98.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "129cs5bqw23i76h3nmc29c9mqkm9460iwc8vkl7hs4xr07h8mip9"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       `("--enable-animation"
         "--enable-animationrtl")))
    (native-inputs
     (list pkg-config intltool))
    (propagated-inputs
     (list gtk+-2))
    (home-page "https://live.gnome.org/GnomeArt")
    (synopsis "Cairo-based theming engine for GTK+ 2.x")
    (description
     "Murrine is a cairo-based GTK+ theming engine.  It is named after the
glass artworks done by Venicians glass blowers.")
    (license license:gpl2+)))

(define-public gtkspell3
  (package
    (name "gtkspell3")
    (version "3.0.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/gtkspell/"
                                  version "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "0cjp6xdcnzh6kka42w9g0w2ihqjlq8yl8hjm9wsfnixk6qwgch5h"))))
    (build-system gnu-build-system)
    (native-inputs
     (list intltool pkg-config vala))
    (inputs
     (list gobject-introspection gtk+ pango))
    (propagated-inputs
     (list enchant))           ; gtkspell3-3.0.pc refers to it
    (home-page "http://gtkspell.sourceforge.net")
    (synopsis "Spell-checking addon for GTK's TextView widget")
    (description
     "GtkSpell provides word-processor-style highlighting and replacement of
misspelled words in a GtkTextView widget.")
    (license license:gpl2+)))

(define-public clipit
  (package
    (name "clipit")
    (version "1.4.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/CristianHenzel/ClipIt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05xi29v2y0rvb33fmvrz7r9j4l858qj7ngwd7dp4pzpkkaybjln0"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake intltool pkg-config))
    (inputs
     (list gtk+-2))
    (home-page "https://github.com/CristianHenzel/ClipIt")
    (synopsis "Lightweight GTK+ clipboard manager")
    (description
     "ClipIt is a clipboard manager with features such as a history, search
thereof, global hotkeys and clipboard item actions.  It was forked from
Parcellite and adds bugfixes and features.")
    (license license:gpl2+)))

(define-public graphene
  (package
    (name "graphene")
    (version "1.10.6")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/ebassi/graphene.git")
         (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g2jjy6xizzjxlp0dr81h1f5l16dzcnhznhj6jvhpdjqcvgp98xr"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t     ; To wrap binaries and/or compile schemas
       #:configure-flags
       (list
        "-Dinstalled_tests=false"
        ,@(if (%current-target-system)
              ;; Introspection requires running binaries for 'host' on 'build'.
              '("-Dintrospection=false")
              '()))))
    (native-inputs
     `(("git" ,git-minimal/fixed)
       ("gobject-introspection" ,gobject-introspection)
       ("mutest" ,mutest)
       ("pkg-config" ,pkg-config)))
    (inputs
     (list glib python))
    (synopsis "Thin layer of graphic data types")
    (description "Graphene provides graphic types and their relative API; it
does not deal with windowing system surfaces, drawing, scene graphs, or input.")
    (home-page "https://ebassi.github.io/graphene/")
    (license license:expat)))

(define-public spread-sheet-widget
  (package
    (name "spread-sheet-widget")
    (version "0.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://alpha.gnu.org/gnu/ssw/"
                           "spread-sheet-widget-" version ".tar.gz"))
       (sha256
        (base32 "09rzgp7gabnzab460x874a1ibgyjiibpwzsz5srn9zs6jv2jdxjb"))))
    (build-system gnu-build-system)
    (native-inputs
     (list `(,glib "bin") ; for glib-genmarshal, etc.
           pkg-config))
    ;; In 'Requires' of spread-sheet-widget.pc.
    (propagated-inputs
     (list glib gtk+))
    (home-page "https://www.gnu.org/software/ssw/")
    (synopsis "Gtk+ widget for dealing with 2-D tabular data")
    (description
     "GNU Spread Sheet Widget is a library for Gtk+ which provides a widget for
viewing and manipulating 2 dimensional tabular data in a manner similar to many
popular spread sheet programs.")
    (license license:gpl3+)))

(define-public pnmixer
  (package
    (name "pnmixer")
    (version "0.7.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nicklan/pnmixer/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0416pa933ddf4b7ph9zxhk5jppkk7ppcq1aqph6xsrfnka4yb148"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f))          ;no check target
    (native-inputs
     (list gettext-minimal pkg-config))
    (inputs
     (list alsa-lib glib gtk+ libnotify libx11))
    (home-page "https://github.com/nicklan/pnmixer/")
    (synopsis "Simple mixer application designed to run in system tray")
    (description
     "PNMixer is a simple mixer application designed to run in system tray.
It integrates nicely into desktop environments that don't have a panel that
supports applets and therefore can't run a mixer applet.  In particular, it's
been used quite a lot with fbpanel and tint2 but should run fine in any system
tray.

PNMixer is designed to work on systems that use ALSA for sound management.
Any other sound driver like OSS or FFADO are, currently, not supported.  There
is no official PulseAudio support, at the moment, but it seems that PNMixer
behaves quite well anyway when PA is running.")
    (license license:gpl3)))

(define-public volumeicon
  (package
    (name "volumeicon")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://nullwise.com/files/volumeicon/volumeicon-"
                           version ".tar.gz"))
       (sha256
        (base32 "182xl2w8syv6ky2h2bc9imc6ap8pzh0p7rp63hh8nw0xm38c3f14"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--enable-notify")))       ; optional libnotify support
    (native-inputs
     (list intltool pkg-config))
    (inputs
     (list alsa-lib gtk+ libnotify))
    (home-page "http://nullwise.com/volumeicon.html")
    (synopsis "System tray volume applet")
    (description
     "Volume Icon is a volume indicator and control applet for @acronym{the
Advanced Linux Sound Architecture, ALSA}.  It sits in the system tray,
independent of your desktop environment, and supports global key bindings.")
    (license (list license:expat        ; src/{bind.c,keybinder.h}
                   license:isc          ; src/alsa_volume_mapping.c
                   license:gpl3))))     ; the rest & combined work

(define-public yad
  (package
    (name "yad")
    (version "5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/v1cont/yad")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07rd61hvilsxxrj7lf8c9k0a8glj07s48m7ya8d45030r90g3lvc"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags
       ;; Passing --enable-foo will silently disable foo if prerequisite
       ;; inputs are missing, not abort the build as one might expect.
       ;; ‘--enable-html’ adds a huge webkitgtk dependency.  It was never
       ;; present in the past and nobody complained.
       '("--enable-icon-browser"
         "--enable-spell")              ; gspell checking support
       #:phases
       (modify-phases %standard-phases
         (add-after 'bootstrap 'intltoolize
           (lambda _
             (invoke "intltoolize" "--force" "--automake"))))))
    (inputs
     (list gspell gtk+))
    (native-inputs
     (list autoconf automake intltool pkg-config))
    (home-page "https://sourceforge.net/projects/yad-dialog/")
    (synopsis "GTK+ dialog boxes for shell scripts")
    (description
     "This program allows you to display GTK+ dialog boxes from command line or
shell scripts.  Example of how to use @code{yad} can be consulted at
@url{https://sourceforge.net/p/yad-dialog/wiki/browse_pages/}.")
    (license license:gpl3+)))

(define-public dragon-drop
  (package
   (name "dragon-drop")
   (version "1.1.1")
   (source (origin
             (method git-fetch)
             (uri
              (git-reference
               (url "https://github.com/mwh/dragon")
               (commit (string-append "v" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "0fgzz39007fdjwq72scp0qygp2v3zc5f1xkm0sxaa8zxm25g1bra"))))
   (build-system gnu-build-system)
   (inputs (list gtk+))
   (native-inputs (list pkg-config))
   (arguments
    `(#:tests? #f                       ; no check
      #:make-flags
      (list (string-append "CC=" ,(cc-for-target))
            ;; makefile uses PREFIX for the binary location
            (string-append "PREFIX=" (assoc-ref %outputs "out")
                           "/bin"))
      #:phases
      (modify-phases %standard-phases
        (delete 'configure))))                    ; no configure script
   (synopsis "Drag and drop source/target for X")
   (description
    "Dragon is a lightweight drag-and-drop source for X where you can run:

@example
dragon file.tar.gz
@end example

to get a window with just that file in it, ready to be dragged where you need it.
What if you need to drag into something? Using:

@example
dragon --target
@end example

you get a window you can drag files and text into.  Dropped items are
printed to standard output.")
   (home-page "https://github.com/mwh/dragon")
   (license license:gpl3+)))

(define-public libdbusmenu
  (package
    (name "libdbusmenu")
    (version "16.04.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://launchpad.net/libdbusmenu/"
                           (version-major+minor version) "/" version
                           "/+download/libdbusmenu-" version ".tar.gz"))
       (sha256
        (base32 "12l7z8dhl917iy9h02sxmpclnhkdjryn08r8i4sr8l3lrlm4mk5r"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       '("--sysconfdir=/etc"
         "--localstatedir=/var"
         ;; The shebang of the generated test files should be patched before
         ;; enabling tests.
         "--disable-tests")
       #:make-flags
       `(,(string-append "typelibdir=" (assoc-ref %outputs "out")
                         "/lib/girepository-1.0"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'do-not-treat-warnings-as-errors
           (lambda _
             ;; Prevent the build from failing due to deprecation warnings
             ;; from newer GLib and GTK versions.
             (substitute* (find-files "." "^Makefile.in$")
               ((" -Werror")
                ""))
             #t))
         (add-before 'configure 'set-environment
           (lambda _
             (setenv "HAVE_VALGRIND_TRUE" "")
             (setenv "HAVE_VALGRIND_FALSE" "#")
             #t)))))
    (inputs
     `(("glib" ,glib)
       ("gtk+" ,gtk+)
       ("gtk+-2" ,gtk+-2)))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("gnome-doc-utils" ,gnome-doc-utils)
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("json-glib" ,json-glib)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)
       ("vala" ,vala)))
    (home-page "https://launchpad.net/libdbusmenu")
    (synopsis "Library for passing menus over DBus")
    (description "@code{libdbusmenu} passes a menu structure across DBus so
that a program can create a menu simply without worrying about how it is
displayed on the other side of the bus.")

    ;; Dual-licensed under either LGPLv2.1 or LGPLv3.
    (license (list license:lgpl2.1 license:lgpl3))))

(define-public gtk-layer-shell
  (package
    (name "gtk-layer-shell")
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/wmww/gtk-layer-shell")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kcp4p3s7sdh9lwniybjdarfy8z69j2j23hfrw98amhwhq39gdcc"))))
    (build-system meson-build-system)
    (arguments `(#:configure-flags (list "-Dtests=true")))
    (native-inputs (list pkg-config gobject-introspection))
    (inputs (list wayland gtk+))
    (home-page "https://github.com/wmww/gtk-layer-shell")
    (synopsis "Library to create Wayland desktop components using the Layer
Shell protocol")
    (description "Layer Shell is a Wayland protocol for desktop shell
components, such as panels, notifications and wallpapers.  It can be used to
anchor windows to a corner or edge of the output, or stretch them across the
entire output.  It supports all Layer Shell features including popups and
popovers.")
    (license license:expat)))

(define-public goocanvas
  (package
    (name "goocanvas")
    (version "2.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/goocanvas/"
                           (version-major+minor version)
                           "/goocanvas-" version ".tar.xz"))
       (sha256
        (base32 "141fm7mbqib0011zmkv3g8vxcjwa7hypmq71ahdyhnj2sjvy4a67"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib-bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk-doc" ,gtk-doc/stable)
       ("pkg-config" ,pkg-config)
       ("python" ,python)))
    (inputs
     (list cairo glib gtk+ python-pygobject))
    (arguments
     `(#:configure-flags '("--disable-rebuilds"
                           "--disable-static")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-install-path
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "configure"
               (("\\(gi._overridesdir\\)")
                (string-append "((gi._overridesdir).replace(\\\""
                               (assoc-ref inputs "python-pygobject")
                               "\\\", \\\""
                               (assoc-ref outputs "out")
                               "\\\"))")))
             #t)))))
    (synopsis "Canvas widget for GTK+")
    (description "GooCanvas is a canvas widget for GTK+ that uses the cairo 2D
library for drawing.")
    (home-page "https://wiki.gnome.org/GooCanvas")
    (license license:lgpl2.0)))

(define-public gtksheet
  (package
    (name "gtksheet")
    (version "4.3.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fpaquet/gtksheet")
             (commit (string-append "V" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "13jwr1vly4ga3f09dajwky1cdrz5bmggwga3vnnd6j6zzia7dpyr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list "--enable-glade"
                               "--enable-introspection"
                               "CFLAGS=-fcommon")
       #:phases
       (modify-phases %standard-phases
         ;; The "configure" script is present, but otherwise the project is
         ;; not bootstrapped properly. Delete configure so the bootstrap phase
         ;; will take over.
         (add-after 'unpack 'delete-configure
           (lambda _
             (delete-file "configure")
             #t))
         (add-after 'unpack 'patch-for-compatibility
           (lambda _
             (substitute* "glade/glade-gtksheet-editor.c"
               (("GladeEditableIface") "GladeEditableInterface"))
             ;; Glade 3.37 renamed the macro GWA_GET_CLASS to
             ;; GLADE_WIDGET_ADAPTOR_GET_ADAPTOR_CLASS.
             (substitute* "glade/glade-gtksheet-editor.c"
               (("GWA_GET_CLASS") "GLADE_WIDGET_ADAPTOR_GET_ADAPTOR_CLASS"))))
         ;; Fix glade install directories.
         (add-before 'bootstrap 'configure-glade-directories
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "configure.ac"
               (("`\\$PKG_CONFIG --variable=catalogdir gladeui-2.0`")
                (string-append (assoc-ref outputs "out") "/share/glade/catalogs"))
               (("`\\$PKG_CONFIG --variable=moduledir gladeui-2.0`")
                (string-append (assoc-ref outputs "out") "/lib/glade/modules"))
               (("`\\$PKG_CONFIG --variable=pixmapdir gladeui-2.0`")
                (string-append (assoc-ref outputs "out") "/share/pixmaps")))
             #t)))))
    (inputs
     (list glade3 glib gtk+ libxml2))
    (native-inputs
     (list autoconf automake gobject-introspection libtool pkg-config))
    (home-page "https://fpaquet.github.io/gtksheet/")
    (synopsis "Spreadsheet widget for GTK+")
    (description "GtkSheet is a matrix widget for GTK+.  It consists of an
scrollable grid of cells where you can allocate text.  Cell contents can be
edited interactively through a specially designed entry, GtkItemEntry.  It is
also a container subclass, allowing you to display buttons, images and any
other widget in it.  You can also set many attributes such as border,
foreground and background colors, text justification and more.")
    (native-search-paths
     (list
      (search-path-specification
       (variable "GLADE_CATALOG_SEARCH_PATH")
       (files '("share/glade/catalogs")))
      (search-path-specification
       (variable "GLADE_MODULE_SEARCH_PATH")
       (files '("lib/glade/modules")))))
    (license license:lgpl2.0+)))

(define-public gtkdatabox
  (package
    (name "gtkdatabox")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/gtkdatabox/gtkdatabox-1/"
                           "gtkdatabox-" version ".tar.gz"))
       (sha256
        (base32 "1qykm551bx8j8pfgxs60l2vhpi8lv4r8va69zvn2594lchh71vlb"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list gtk+))
    (synopsis "Display widget for dynamic data")
    (description "GtkDatabox is a widget for live display of large amounts of
fluctuating numerical data.  It enables data presentation (for example, on
linear or logarithmic scales, as dots or lines, with markers/labels) as well as
user interaction (e.g.  measuring distances).")
    (home-page "https://sourceforge.net/projects/gtkdatabox/")
    (license license:lgpl2.1+)))

(define-public volctl
  (package
    (name "volctl")
    (version "0.8.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url "https://github.com/buzz/volctl")
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1cx27j83pz2qffnzb85fbl1x6pp3irv1kbw7g1hri7kaw6ky4xiz"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-path
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((pulse (assoc-ref inputs "pulseaudio"))
                   (xfixes (assoc-ref inputs "libxfixes")))
               (substitute* "volctl/lib/xwrappers.py"
                 (("libXfixes.so")
                  (string-append xfixes "/lib/libXfixes.so")))
               (substitute* "volctl/lib/pulseaudio.py"
                 (("libpulse.so.0")
                  (string-append pulse "/lib/libpulse.so.0")))
               #t))))))
    (inputs
     (list gtk+ libxfixes pulseaudio))
    (propagated-inputs
     (list python-click python-pycairo python-pygobject python-pyyaml))
    (home-page "https://buzz.github.io/volctl/")
    (synopsis "Per-application volume control and on-screen display (OSD) for graphical desktops")
    (description "Volctl is a PulseAudio-enabled tray icon volume control and
OSD applet for graphical desktops.  It's not meant to be an replacement for a
full-featured mixer application.  If you're looking for that check out the
excellent pavucontrol.")

    ;; XXX: 'setup.py' says "GPLv2" but nothing says "version 2 only".  Is
    ;; GPLv2+ intended?
    (license license:gpl2)))
