;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2017, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015 Andy Wingo <wingo@igalia.com>
;;; Copyright © 2015 David Hashe <david.hashe@dhashe.com>
;;; Coypright © 2015, 2016, 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Fabian Harfert <fhmgufs@web.de>
;;; Copyright © 2016 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2016 Patrick Hetu <patrick.hetu@auf.org>
;;; Copyright © 2016 Nils Gillmann <ng0@n0.is>
;;; Copyright © 2017 Roel Janssen <roel@gnu.org>
;;; Copyright © 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
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
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system waf)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26))

(define-public atk
  (package
   (name "atk")
   (version "2.28.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/" name "/"
                                (version-major+minor version)  "/"
                                name "-" version ".tar.xz"))
            (sha256
             (base32
              "1z7laf6qwv5zsqcnj222dm5f43c6f3liil0cgx4s4s62xjk1wfnd"))))
   (build-system gnu-build-system)
   (outputs '("out" "doc"))
   (arguments
    `(#:configure-flags
      (list (string-append "--with-html-dir="
                           (assoc-ref %outputs "doc")
                           "/share/gtk-doc/html"))))
   (propagated-inputs `(("glib" ,glib))) ; required by atk.pc
   (native-inputs
    `(("pkg-config" ,pkg-config)
      ("glib" ,glib "bin")                               ; glib-mkenums, etc.
      ("gobject-introspection" ,gobject-introspection))) ; g-ir-compiler, etc.
   (synopsis "GNOME accessibility toolkit")
   (description
    "ATK provides the set of accessibility interfaces that are implemented
by other toolkits and applications.  Using the ATK interfaces, accessibility
tools have full access to view and control running applications.")
   (license license:lgpl2.0+)
   (home-page "https://developer.gnome.org/atk/")))

(define-public cairo
  (package
   (name "cairo")
   (version "1.16.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://cairographics.org/releases/cairo-"
                                version ".tar.xz"))
            (sha256
             (base32
              "0c930mk5xr2bshbdljv005j3j8zr47gqmkry3q6qgvqky6rjjysy"))))
   (build-system gnu-build-system)
   (propagated-inputs
    `(("fontconfig" ,fontconfig)
      ("freetype" ,freetype)
      ("glib" ,glib)
      ("libpng" ,libpng)
      ("libx11" ,libx11)
      ("libxext" ,libxext)
      ("libxrender" ,libxrender)
      ("pixman" ,pixman)))
   (inputs
    `(("ghostscript" ,ghostscript)
      ("libspectre" ,libspectre)
      ("poppler" ,poppler)
      ("xorgproto" ,xorgproto)
      ("zlib" ,zlib)))
   (native-inputs
     `(("pkg-config" ,pkg-config)
      ("python" ,python-wrapper)))
    (arguments
     `(#:tests? #f  ; see http://lists.gnu.org/archive/html/bug-guix/2013-06/msg00085.html
       #:configure-flags '("--enable-tee")))  ; needed for GNU Icecat
   (synopsis "2D graphics library")
   (description
    "Cairo is a 2D graphics library with support for multiple output devices.
Currently supported output targets include the X Window System (via both
Xlib and XCB), Quartz, Win32, image buffers, PostScript, PDF, and SVG file
output.  Experimental backends include OpenGL, BeOS, OS/2, and DirectFB.

Cairo is designed to produce consistent output on all output media while
taking advantage of display hardware acceleration when available
eg. through the X Render Extension).

The cairo API provides operations similar to the drawing operators of
PostScript and PDF.  Operations in cairo including stroking and filling cubic
Bézier splines, transforming and compositing translucent images, and
antialiased text rendering.  All drawing operations can be transformed by any
affine transformation (scale, rotation, shear, etc.).")
   (license license:lgpl2.1) ; or Mozilla Public License 1.1
   (home-page "https://cairographics.org/")))

(define-public cairo-xcb
  (package
    (inherit cairo)
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
   (version "2.2.0")
   (source (origin
             (method url-fetch)
             (uri (string-append "https://www.freedesktop.org/software/"
                                 "harfbuzz/release/harfbuzz-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "047q63jr513azf3g1y7f5xn60b4jdjs9zsmrx04sfw5rasyzrk5p"))))
   (build-system gnu-build-system)
   (outputs '("out"
              "bin")) ; 160K, only hb-view depend on cairo
   (inputs
    `(("cairo" ,cairo)))
   (propagated-inputs
    ;; There are all in the Requires or Requires.private field of '.pc'.
    `(("glib" ,glib)
      ("graphite2" ,graphite2)
      ("icu4c" ,icu4c)))
   (native-inputs
    `(("gobject-introspection" ,gobject-introspection)
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

(define-public pango
  (package
   (name "pango")
   (version "1.42.4")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/pango/"
                                (version-major+minor version) "/"
                                name "-" version ".tar.xz"))
            (sha256
             (base32
              "17bwb7dgbncrfsmchlib03k9n3xaalirb39g3yb43gg8cg6p8aqx"))))
   (build-system gnu-build-system)
   (propagated-inputs
    ;; These are all in Requires or Requires.private of the '.pc' files.
    `(("cairo" ,cairo)
      ("fribidi" ,fribidi)
      ("fontconfig" ,fontconfig)
      ("freetype" ,freetype)
      ("glib" ,glib)
      ("harfbuzz" ,harfbuzz)))
   (inputs
    `(("zlib" ,zlib)

      ;; Some packages, such as Openbox, expect Pango to be built with the
      ;; optional libxft support.
      ("libxft" ,libxft)))
   (native-inputs
    `(("pkg-config" ,pkg-config)
      ("glib" ,glib "bin")                               ; glib-mkenums, etc.
      ("gobject-introspection" ,gobject-introspection))) ; g-ir-compiler, etc.
   (synopsis "GNOME text and font handling library")
   (description
    "Pango is the core text and font handling library used in GNOME
applications.  It has extensive support for the different writing systems
used throughout the world.")
   (license license:lgpl2.0+)
   (home-page "https://developer.gnome.org/pango/")))

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
     `(("glib" ,glib)
       ("pango" ,pango)))
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (home-page "https://developer.gnome.org/pango")
    (synopsis "Obsolete pango functions")
    (description  "Pangox was a X backend to pango.  It is now obsolete and no
longer provided by recent pango releases.  pangox-compat provides the
functions which were removed.")
    (license license:lgpl2.0+)))

(define-public ganv
  (package
    (name "ganv")
    (version "1.4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.drobilla.net/ganv-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "0g7s5mp14qgbfjdql0k1s8464r21g47ssn5dws6jazsnw6njhl0l"))))
    (build-system waf-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-flags
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Compile with C++11, required by gtkmm.
             (setenv "CXXFLAGS" "-std=c++11")
             ;; Allow 'bin/ganv_bench' to find libganv-1.so.
             (setenv "LDFLAGS"
                     (string-append "-Wl,-rpath="
                                    (assoc-ref outputs "out") "/lib"))
             #t)))
       #:python ,python-2 ;XXX: The bundled waf fails with Python 3.7.0.
       #:tests? #f)) ; no check target
    (inputs
     `(("gtk" ,gtk+-2)
       ("gtkmm" ,gtkmm-2)))
    (native-inputs
     `(("glib" ,glib "bin")             ; for glib-genmarshal, etc.
       ("pkg-config" ,pkg-config)))
    (home-page "https://drobilla.net/software/ganv/")
    (synopsis "GTK+ widget for interactive graph-like environments")
    (description
     "Ganv is an interactive GTK+ widget for interactive “boxes and lines” or
graph-like environments, e.g. modular synths or finite state machine
diagrams.")
    (license license:gpl3+)))

(define-public ganv-devel
  (let ((commit "12f7d6b0438c94dd87f773a92eee3453d971846e")
        (revision "1"))
    (package
      (inherit ganv)
      (name "ganv")
      (version (string-append "1.5.4-" revision "."
                              (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.drobilla.net/ganv.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1cr8w02lr6bk9mkxa12j3imq721b2an2yn4bj5wnwmpm91ddn2gi")))))))

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
                "07hrabhpl6n8ajz10s0d960jdwndxs87szxyn428mpxi8cvpg1f5"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("glib" ,glib "bin")             ; for glib-genmarshal, etc.
       ("pkg-config" ,pkg-config)
       ;; For testing.
       ("xorg-server" ,xorg-server)
       ("shared-mime-info" ,shared-mime-info)))
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
   (version "3.24.8")
   (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/" name "/"
                                 (version-major+minor version) "/"
                                 name "-" version ".tar.xz"))
             (sha256
              (base32
               "1zinqid62zjcsq7vy1y4mq1qh3hzd3zj7p8np7g0bdqd37zvi6qy"))))
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
      ("xorg-server" ,xorg-server)
      ("shared-mime-info" ,shared-mime-info)))
   (propagated-inputs
    ;; gtksourceview-3.0.pc refers to all these.
    `(("glib" ,glib)
      ("gtk+" ,gtk+)
      ("libxml2" ,libxml2)))
   (home-page "https://wiki.gnome.org/Projects/GtkSourceView")
   (synopsis "GNOME source code widget")
   (description "GtkSourceView is a text widget that extends the standard
GTK+ text widget GtkTextView.  It improves GtkTextView by implementing syntax
highlighting and other features typical of a source code editor.")
   (license license:lgpl2.1+)))

(define-public gdk-pixbuf
  (package
   (name "gdk-pixbuf")
   (version "2.38.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/" name "/"
                                (version-major+minor version)  "/"
                                name "-" version ".tar.xz"))
            (sha256
             (base32
              "0ixfmnxjylx06mjaw116apymwi1a8rnkmkbbvqaxxg2pfwy9fl6x"))))
   (build-system meson-build-system)
   (arguments
    `(#:configure-flags '("-Dinstalled_tests=false")
      #:phases
      (modify-phases %standard-phases
        (add-after
         'unpack 'disable-failing-tests
         (lambda _
           (substitute* "tests/meson.build"
             ;; XXX FIXME: This test fails on armhf machines with:
             ;; SKIP Not enough memory to load bitmap image
             ;; ERROR: cve-2015-4491 - too few tests run (expected 4, got 2)
             ((".*'cve-2015-4491'.*") "")
             ;; XXX FIXME: This test fails with:
             ;; ERROR:pixbuf-jpeg.c:74:test_type9_rotation_exif_tag:
             ;; assertion failed (error == NULL): Data differ
             ;; (gdk-pixbuf-error-quark, 0)
             ((".*'pixbuf-jpeg'.*") ""))
           #t))
        ;; The slow tests take longer than the specified timeout.
        ,@(if (any (cute string=? <> (%current-system))
                   '("armhf-linux" "aarch64-linux"))
            '((replace 'check
              (lambda _
                (invoke "meson" "test" "--timeout-multiplier" "5"))))
            '())
        (add-before 'configure 'aid-install-script
          (lambda* (#:key outputs #:allow-other-keys)
            ;; "build-aux/post-install.sh" invokes `gdk-pixbuf-query-loaders`
            ;; for updating loader.cache, but it's not on PATH.  Make it use
            ;; the one we're installing.  XXX: Won't work when cross-compiling.
            (substitute* "build-aux/post-install.sh"
              (("gdk-pixbuf-query-loaders" match)
               (string-append (assoc-ref outputs "out") "/bin/" match)))
            #t)))))
   (propagated-inputs
    `(;; Required by gdk-pixbuf-2.0.pc
      ("glib" ,glib)
      ("libpng" ,libpng)
      ;; Used for testing and required at runtime.
      ("shared-mime-info" ,shared-mime-info)))
   (inputs
    `(("libjpeg" ,libjpeg)
      ("libtiff" ,libtiff)
      ("libx11"  ,libx11)))
   (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gettext" ,gettext-minimal)
       ("glib" ,glib "bin")                               ; glib-mkenums, etc.
       ("gobject-introspection" ,gobject-introspection))) ; g-ir-compiler, etc.
   (synopsis "GNOME image loading and manipulation library")
   (description
    "GdkPixbuf is a library for image loading and manipulation developed
in the GNOME project.")
   (license license:lgpl2.0+)
   (home-page "https://developer.gnome.org/gdk-pixbuf/")))

;; To build gdk-pixbuf with SVG support, we need librsvg, and librsvg depends
;; on gdk-pixbuf, so this new varibale.  Also, librsvg adds 90MiB to the
;; closure size.
(define-public gdk-pixbuf+svg
  (package (inherit gdk-pixbuf)
    (name "gdk-pixbuf+svg")
    (inputs
     `(("librsvg" ,librsvg)
       ,@(package-inputs gdk-pixbuf)))
    (arguments
     '(#:configure-flags '("-Dinstalled-tests=false")
       #:tests? #f ; tested by the gdk-pixbuf package already
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'register-svg-loader
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (librsvg (assoc-ref inputs "librsvg"))
                    (loaders
                     (append
                      (find-files out "^libpixbufloader-.*\\.so$")
                      (find-files librsvg "^libpixbufloader-.*\\.so$")))
                    (gdk-pixbuf-query-loaders
                     (string-append out "/bin/gdk-pixbuf-query-loaders")))
               (apply invoke
                      gdk-pixbuf-query-loaders
                      "--update-cache"
                      loaders)))))))
    (synopsis
     "GNOME image loading and manipulation library, with SVG support")))

(define-public at-spi2-core
  (package
   (name "at-spi2-core")
   (version "2.26.2")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/" name "/"
                                (version-major+minor version)  "/"
                                name "-" version ".tar.xz"))
            (sha256
             (base32
              "0596ghkamkxgv08r4a1pdhm06qd5zzgcfqsv64038w9xbvghq3n8"))))
   (build-system gnu-build-system)
   (outputs '("out" "doc"))
   (arguments
    '(#:configure-flags
      (list (string-append "--with-html-dir="
                           (assoc-ref %outputs "doc")
                           "/share/gtk-doc/html"))
      #:phases
      (modify-phases %standard-phases
        (replace 'check
                 ;; Run test-suite under a dbus session.
                 (lambda _
                   ;; Don't fail on missing  '/etc/machine-id'.
                   (setenv "DBUS_FATAL_WARNINGS" "0")
                   (invoke "dbus-launch" "make" "check"))))))
   (propagated-inputs
    ;; atspi-2.pc refers to all these.
    `(("dbus" ,dbus)
      ("glib" ,glib)))
   (inputs
    `(("libxi" ,libxi)
      ("libxtst" ,libxtst)))
   (native-inputs
    `(("gobject-introspection" ,gobject-introspection)
      ("intltool" ,intltool)
      ("pkg-config" ,pkg-config)))
   (synopsis "Assistive Technology Service Provider Interface, core components")
   (description
    "The Assistive Technology Service Provider Interface, core components,
is part of the GNOME accessibility project.")
   (license license:lgpl2.0+)
   (home-page "https://projects.gnome.org/accessibility/")))

(define-public at-spi2-atk
  (package
   (name "at-spi2-atk")
   (version "2.26.2")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/" name "/"
                                (version-major+minor version)  "/"
                                name "-" version ".tar.xz"))
            (sha256
             (base32
              "0vkan52ab9vrkknnv8y4f1cspk8x7xd10qx92xk9ys71p851z2b1"))))
   (build-system gnu-build-system)
   (arguments
    '(#:phases
      (modify-phases %standard-phases
        (replace 'check
                 ;; Run test-suite under a dbus session.
                 (lambda _
                   (setenv "DBUS_FATAL_WARNINGS" "0")
                   (invoke "dbus-launch" "make" "check"))))))
   (propagated-inputs
    `(("at-spi2-core" ,at-spi2-core))) ; required by atk-bridge-2.0.pc
   (inputs
    `(("atk" ,atk)))
   (native-inputs
    `(("dbus" ,dbus) ; for testing
      ("pkg-config" ,pkg-config)))
   (synopsis "Assistive Technology Service Provider Interface, ATK bindings")
   (description
    "The Assistive Technology Service Provider Interface
is part of the GNOME accessibility project.")
   (license license:lgpl2.0+)
   (home-page "https://projects.gnome.org/accessibility/")))

(define-public gtk+-2
  (package
   (name "gtk+")
   (version "2.24.32")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/" name "/"
                                (version-major+minor version)  "/"
                                name "-" version ".tar.xz"))
            (sha256
             (base32
              "0bjq7ja9gwcv6n5q4qkvdjjx40wsdiikksz1zqxvxsm5vlyskj5n"))
            (patches (search-patches "gtk2-respect-GUIX_GTK2_PATH.patch"
                                     "gtk2-respect-GUIX_GTK2_IM_MODULE_FILE.patch"
                                     "gtk2-theme-paths.patch"))))
   (build-system gnu-build-system)
   (outputs '("out" "doc"))
   (propagated-inputs
    `(("atk" ,atk)
      ("gdk-pixbuf" ,gdk-pixbuf+svg)
      ("pango" ,pango)))
   (inputs
    `(("cups" ,cups)
      ("libxcomposite" ,libxcomposite)
      ("libxcursor" ,libxcursor)
      ("libxdamage" ,libxdamage)
      ("libxi" ,libxi)
      ("libxinerama" ,libxinerama)
      ("libxrandr" ,libxrandr)))
   (native-inputs
    `(("perl" ,perl)
      ("gettext" ,gettext-minimal)
      ("glib" ,glib "bin")
      ("gobject-introspection" ,gobject-introspection)
      ("pkg-config" ,pkg-config)
      ("python-wrapper" ,python-wrapper)))
   (arguments
    `(#:configure-flags
      (list "--with-xinput=yes"
            (string-append "--with-html-dir="
                           (assoc-ref %outputs "doc")
                           "/share/gtk-doc/html"))
      #:phases
      (alist-cons-before
       'configure 'disable-tests
       (lambda _
         ;; FIXME: re-enable tests requiring an X server
         (substitute* "gtk/Makefile.in"
           (("SUBDIRS = theme-bits . tests") "SUBDIRS = theme-bits ."))
         #t)
       %standard-phases)))
   (native-search-paths
    (list (search-path-specification
           (variable "GUIX_GTK2_PATH")
           (files '("lib/gtk-2.0")))))
   (synopsis "Cross-platform toolkit for creating graphical user interfaces")
   (description
    "GTK+, or the GIMP Toolkit, is a multi-platform toolkit for creating
graphical user interfaces.  Offering a complete set of widgets, GTK+ is
suitable for projects ranging from small one-off tools to complete
application suites.")
   (license license:lgpl2.0+)
   (home-page "https://www.gtk.org/")))

(define-public gtk+
  (package (inherit gtk+-2)
   (name "gtk+")
   ;; NOTE: When updating the version of 'gtk+', the hash of 'mate-themes' in
   ;;       mate.scm will also need to be updated.
   (version "3.24.2")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/" name "/"
                                (version-major+minor version)  "/"
                                name "-" version ".tar.xz"))
            (sha256
             (base32
              "14l8mimdm44r3h5pn5hzigl1z25jna8jxvb16l88v4nc4zj0afsv"))
            (patches (search-patches "gtk3-respect-GUIX_GTK3_PATH.patch"
                                     "gtk3-respect-GUIX_GTK3_IM_MODULE_FILE.patch"))
            (modules '((guix build utils)))
            (snippet
             '(begin
                ;; Version 3.24.2 was released with a typo that broke the build.
                ;; See upstream commit 2905fc861acda3d134a198e56ef2f6c962ad3061
                ;; at <https://gitlab.gnome.org/GNOME/gtk/tree/gtk-3-24>
                (substitute* "docs/tools/shooter.c"
                  (("gdk_screen_get_dfeault") "gdk_screen_get_default"))
                #t))))
   (outputs '("out" "bin" "doc"))
   (propagated-inputs
    `(("at-spi2-atk" ,at-spi2-atk)
      ("atk" ,atk)
      ("gdk-pixbuf" ,gdk-pixbuf+svg)
      ("libepoxy" ,libepoxy)
      ("libxcursor" ,libxcursor)
      ("libxi" ,libxi)
      ("libxinerama" ,libxinerama)
      ("libxkbcommon" ,libxkbcommon)
      ("libxdamage" ,libxdamage)
      ("mesa" ,mesa)
      ("pango" ,pango)
      ("wayland" ,wayland)
      ("wayland-protocols" ,wayland-protocols)))
   (inputs
    `(("libxml2" ,libxml2)
      ;; XXX: colord depends on mozjs (through polkit), which fails on
      ;;      on non-intel systems now.
      ;;("colord" ,colord)
      ("cups" ,cups)                            ;for printing support
      ;; XXX: rest depends on p11-kit, which fails on mips64el now.
      ;;("rest" ,rest)
      ("json-glib" ,json-glib)))
   (native-inputs
    `(("perl" ,perl)
      ("glib" ,glib "bin")
      ("gettext" ,gettext-minimal)
      ("pkg-config" ,pkg-config)
      ("gobject-introspection" ,gobject-introspection)
      ("python-wrapper" ,python-wrapper)
      ;; By using a special xorg-server for GTK+'s tests, we reduce the impact
      ;; of updating xorg-server directly on the master branch.
      ("xorg-server" ,xorg-server-for-tests)))
   (arguments
    `(#:disallowed-references (,xorg-server-for-tests)
      ;; 47 MiB goes to "out" (24 of which is locale data!), and 26 MiB goes
      ;; to "doc".
      #:configure-flags (list (string-append "--with-html-dir="
                                             (assoc-ref %outputs "doc")
                                             "/share/gtk-doc/html")
                              ;; The header file <gdk/gdkwayland.h> is required
                              ;; by gnome-control-center
                              "--enable-wayland-backend"
                              ;; This is necessary to build both backends.
                              "--enable-x11-backend"
                              ;; This enables the HTML5 websocket backend.
                              "--enable-broadway-backend")
      #:phases (modify-phases %standard-phases
        (add-before 'configure 'pre-configure
          (lambda _
            ;; Disable most tests, failing in the chroot with the message:
            ;; D-Bus library appears to be incorrectly set up; failed to read
            ;; machine uuid: Failed to open "/etc/machine-id": No such file or
            ;; directory.
            ;; See the manual page for dbus-uuidgen to correct this issue.
            (substitute* "testsuite/Makefile.in"
              (("SUBDIRS = gdk gtk a11y css reftests")
               "SUBDIRS = gdk"))
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

;;;
;;; Guile bindings.
;;;

(define-public guile-cairo
  (package
    (name "guile-cairo")
    (version "1.10.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/guile-cairo/guile-cairo-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0p6xrhf2k6n5dybn88050za7h90gnd7534n62l53vsca187pwgdf"))
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
    (inputs
     `(("guile-lib" ,guile-lib)
       ("expat" ,expat)
       ("guile" ,guile-2.2)))
    (propagated-inputs
     ;; The .pc file refers to 'cairo'.
     `(("cairo" ,cairo)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
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

(define-public guile-rsvg
  ;; Use a recent snapshot that supports Guile 2.2 and beyond.
  (let ((commit "05c6a2fd67e4fea1a7c3ff776729dc931bae6678")
        (revision "0"))
    (package
      (name "guile-rsvg")
      (version (string-append "2.18.1-" revision "."
                              (string-take commit 7)))
      (source (origin
                (method url-fetch)
                (uri (string-append "https://gitlab.com/wingo/guile-rsvg/"
                                    "repository/archive.tar.gz?ref="
                                    commit))
                (sha256
                 (base32
                  "0vdzjx8l5nc4y2xjqs0g1rqn1zrwfsm30brh5gz00r1x41a2pvv2"))
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
       `(#:phases (modify-phases %standard-phases
                    (replace 'bootstrap
                      (lambda _
                        (invoke "autoreconf" "-vfi"))))))
      (native-inputs `(("pkg-config" ,pkg-config)
                       ("autoconf" ,autoconf)
                       ("automake" ,automake)
                       ("libtool" ,libtool)
                       ("texinfo" ,texinfo)))
      (inputs `(("guile" ,guile-2.2)
                ("librsvg" ,librsvg)
                ("guile-lib" ,guile-lib)))        ;for (unit-test)
      (propagated-inputs `(("guile-cairo" ,guile-cairo)))
      (synopsis "Render SVG images using Cairo from Guile")
      (description
       "Guile-RSVG wraps the RSVG library for Guile, allowing you to render SVG
images onto Cairo surfaces.")
      (home-page "http://wingolog.org/projects/guile-rsvg/")
      (license license:lgpl2.1+))))

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
              (patches (search-patches "guile-present-coding.patch"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'post-install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (bin   (string-append out "/bin"))
                    (guile (assoc-ref inputs "guile")))
               (substitute* (find-files bin ".*")
                 (("guile")
                  (string-append guile "/bin/guile -L "
                                 out "/share/guile/site/2.0 -C "
                                 out "/share/guile/site/2.0 "))))
             #t)))))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("guile" ,guile-2.2)))
    (propagated-inputs
     ;; These are used by the (present …) modules.
     `(("guile-lib" ,guile-lib)
       ("guile-cairo" ,guile-cairo)
       ("guile-rsvg" ,guile-rsvg)))
    (home-page "http://wingolog.org/software/guile-present/")
    (synopsis "Create SVG or PDF presentations in Guile")
    (description
     "Guile-Present defines a declarative vocabulary for presentations,
together with tools to render presentation documents as SVG or PDF.
Guile-Present can be used to make presentations programmatically, but also
includes a tools to generate PDF presentations out of Org mode and Texinfo
documents.")
    (license license:lgpl3+)))

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
     `(("pkg-config" ,pkg-config)
       ("atk" ,atk)
       ;;("corba" ,corba) ; not packaged yet
       ("gconf" ,gconf)
       ("gobject-introspection" ,gobject-introspection)
       ;;("gthread" ,gthread) ; not packaged yet
       ("gnome-vfs" ,gnome-vfs)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("gtk+" ,gtk+-2)
       ("libglade" ,libglade)
       ("libgnome" ,libgnome)
       ("libgnomecanvas" ,libgnomecanvas)
       ("libgnomeui" ,libgnomeui)
       ("pango" ,pango)
       ("libffi" ,libffi)
       ("glib" ,glib)))
    (inputs `(("guile" ,guile-2.2)))
    (propagated-inputs
     `(("guile-cairo" ,guile-cairo)
       ("g-wrap" ,g-wrap)
       ("guile-lib" ,guile-lib)))
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
    (version "1.12.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.cairographics.org/releases/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "16fmigxsaz85c3lgcls7biwyz8zy8c8h3jndfm54cxxas3a7zi25"))))
    (build-system gnu-build-system)
    (arguments
     ;; The examples lack -lcairo.
     '(#:make-flags '("LDFLAGS=-lcairo")))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("libsigc++" ,libsigc++)
       ("freetype" ,freetype)
       ("fontconfig" ,fontconfig)
       ("cairo" ,cairo)))
    (home-page "https://cairographics.org/")
    (synopsis "C++ bindings to the Cairo 2D graphics library")
    (description
     "Cairomm provides a C++ programming interface to the Cairo 2D graphics
library.")
    (license license:lgpl2.0+)))

(define-public pangomm
  (package
    (name "pangomm")
    (version "2.40.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/" name "/"
                                 (version-major+minor version)  "/"
                                 name "-" version ".tar.xz"))
             (sha256
              (base32
               "1bz3gciff23bpw9bqc4v2l3lkq9w7394v3a4jxkvx0ap5lmfwqlp"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("cairo" ,cairo)
       ("cairomm" ,cairomm)
       ("glibmm" ,glibmm)
       ("pango" ,pango)))
    (home-page "http://www.pango.org/")
    (synopsis "C++ interface to the Pango text rendering library")
    (description
     "Pangomm provides a C++ programming interface to the Pango text rendering
library.")
    (license license:lgpl2.1+)))

(define-public atkmm
  (package
    (name "atkmm")
    (version "2.24.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/" name "/"
                                 (version-major+minor version)  "/"
                                 name "-" version ".tar.xz"))
             (sha256
              (base32
               "1gaqwhviadsmy0fsr47686yglv1p4mpkamj0in127bz2b5bki5gz"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("glibmm" ,glibmm) ("atk" ,atk)))
    (home-page "https://www.gtkmm.org")
    (synopsis "C++ interface to the ATK accessibility library")
    (description
     "ATKmm provides a C++ programming interface to the ATK accessibility
toolkit.")
    (license license:lgpl2.1+)))

(define-public gtkmm
  (package
    (name "gtkmm")
    (version "3.22.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/" name "/"
                                 (version-major+minor version)  "/"
                                 name "-" version ".tar.xz"))
             (sha256
              (base32
               "1400535lhyya462pfx8bp11k3mg3jsbdghlpygskd5ai665dkbwi"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("glib" ,glib "bin")        ;for 'glib-compile-resources'
                     ("xorg-server" ,xorg-server-for-tests)))
    (propagated-inputs
     `(("pangomm" ,pangomm)
       ("cairomm" ,cairomm)
       ("atkmm" ,atkmm)
       ("gtk+" ,gtk+)
       ("glibmm" ,glibmm)))
    (arguments
     `(;; XXX: Tests require C++14 or later.  Remove this when the default
       ;; compiler is >= GCC6.
       #:configure-flags '("CXXFLAGS=-std=gnu++14")
       #:disallowed-references (,xorg-server-for-tests)
       #:phases (modify-phases %standard-phases
                  (add-before 'check 'run-xvfb
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((xorg-server (assoc-ref inputs "xorg-server")))
                        ;; Tests such as 'object_move/test' require a running
                        ;; X server.
                        (system (string-append xorg-server "/bin/Xvfb :1 &"))
                        (setenv "DISPLAY" ":1")
                        ;; Don't fail because of the missing /etc/machine-id.
                        (setenv "DBUS_FATAL_WARNINGS" "0")
                        #t))))))
    (home-page "https://gtkmm.org/")
    (synopsis
     "C++ interface to the GTK+ graphical user interface library")
    (description
     "gtkmm is the official C++ interface for the popular GUI library GTK+.
Highlights include typesafe callbacks, and a comprehensive set of widgets that
are easily extensible via inheritance.  You can create user interfaces either
in code or with the Glade User Interface designer, using libglademm.  There's
extensive documentation, including API reference and a tutorial.")
    (license license:lgpl2.1+)))


(define-public gtkmm-2
  (package (inherit gtkmm)
    (name "gtkmm")
    (version "2.24.5")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/" name "/"
                                 (version-major+minor version)  "/"
                                 name "-" version ".tar.xz"))
             (sha256
              (base32
               "0wkbzvsx4kgw16f6xjdc1dz7f77ldngdila4yi5lw2zrgcxsb006"))))
    (arguments
     '(#:configure-flags '("CPPFLAGS=-std=c++11"))) ; required by libsigc++
    (native-inputs `(("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("pangomm" ,pangomm)
       ("cairomm" ,cairomm)
       ("atkmm" ,atkmm)
       ("gtk+" ,gtk+-2)
       ("glibmm" ,glibmm)))))

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
     `(("pkg-config" ,pkg-config)))
    (propagated-inputs
     ;; In 'Requires' of gtksourceviewmm-3.0.pc.
     `(("glibmm" ,glibmm)
       ("gtkmm" ,gtkmm)
       ("gtksourceview" ,gtksourceview)))
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
    (version "1.17.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://github.com/pygobject/pycairo/releases/download/v"
                          version "/pycairo-" version ".tar.gz"))
      (sha256
       (base32
        "165n0g7gp2a0qi8558snvfans17x83jv2lv7bx4vr1rxjbn3a2hg"))))
    (build-system python-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python-pytest" ,python-pytest)))
    (propagated-inputs                  ;pycairo.pc references cairo
     `(("cairo" ,cairo)))
    (home-page "https://cairographics.org/pycairo/")
    (synopsis "Python bindings for cairo")
    (description
     "Pycairo is a set of Python bindings for the Cairo graphics library.")
    (license license:lgpl3+)
    (properties `((python2-variant . ,(delay python2-pycairo))))))

(define-public python2-pycairo
  (let ((pycairo (package-with-python2
                  (strip-python2-variant python-pycairo))))
    (package
      (inherit pycairo)
      (propagated-inputs
       `(("python2-funcsigs" ,python2-funcsigs)
         ,@(package-propagated-inputs pycairo)))
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
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("python" ,python-2)
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
    (version "1.106")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/X/XA/XAOC/Cairo-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1i25kks408c54k2zxskvg54l5k3qadzm8n72ffga9jy7ic0h6j76"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-extutils-depends" ,perl-extutils-depends)
       ("perl-extutils-pkgconfig" ,perl-extutils-pkgconfig)))
    (inputs
     `(("cairo" ,cairo)))
    (home-page "https://metacpan.org/release/Cairo")
    (synopsis "Perl interface to the cairo 2d vector graphics library")
    (description "Cairo provides Perl bindings for the vector graphics library
cairo.  It supports multiple output targets, including PNG, PDF and SVG.  Cairo
produces identical output on all those targets.")
    (license license:lgpl2.1+)))

(define-public perl-gtk2
  (package
    (name "perl-gtk2")
    (version "1.24992")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/X/XA/XAOC/Gtk2-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1044rj3wbfmgaif2jb0k28m2aczli6ai2n5yvn6pr7zjyw16kvd2"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-extutils-depends" ,perl-extutils-depends)
       ("perl-extutils-pkgconfig" ,perl-extutils-pkgconfig)))
    (inputs
     `(("gtk+" ,gtk+-2)))
    (propagated-inputs
     `(("perl-pango" ,perl-pango)))
    (home-page "https://metacpan.org/release/Gtk2")
    (synopsis "Perl interface to the 2.x series of the Gimp Toolkit library")
    (description "Perl bindings to the 2.x series of the Gtk+ widget set.
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
     `(("perl-extutils-depends" ,perl-extutils-depends)
       ("perl-extutils-pkgconfig" ,perl-extutils-pkgconfig)))
    (inputs
     `(("pango" ,pango)))
    (propagated-inputs
     `(("perl-cairo" ,perl-cairo)
       ("perl-glib" ,perl-glib)))
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
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://pwmt.org/projects/girara/download/girara-"
                              version ".tar.xz"))
              (sha256
               (base32
                "1kc6n1mxjxa7wvwnqy94qfg8l9jvx9qrvrr2kc7m4g0z20x3a00p"))))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("check" ,check)
                     ("gettext" ,gettext-minimal)
                     ("glib:bin" ,glib "bin")
                     ("xorg-server" ,xorg-server-for-tests)))
    ;; Listed in 'Requires.private' of 'girara.pc'.
    (propagated-inputs `(("gtk+" ,gtk+)))
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
    (version "1.27")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0vwsdl61nvnmqswlz5j9m4hg7qirhazwcikcnqf9nx0c13vx6sz2"))))
    (build-system gnu-build-system)
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
         (add-after 'patch-gtk-doc-scan 'patch-test-out
           (lambda _
             ;; sanity.sh counts the number of status lines.  Since our
             ;; texlive regenerates the fonts every time and the font
             ;; generator metafont outputs a lot of extra lines, this
             ;; test would always fail.  Disable it for now.
             (substitute* "tests/Makefile.in"
              (("empty.sh sanity.sh") "empty.sh"))
             #t))
         (add-before 'build 'set-HOME
           (lambda _
             ;; FIXME: dblatex with texlive-union does not find the built
             ;; metafonts, so it tries to generate them in HOME.
             (setenv "HOME" "/tmp")
             #t))
         (add-before 'configure 'fix-docbook
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "configure"
               ;; The configure check is overzealous about making sure that
               ;; things are in place -- it uses the xmlcatalog tool to make
               ;; sure that docbook-xsl is available, but this tool can only
               ;; look in one catalog file, unlike the $XML_CATALOG_FILES
               ;; variable that Guix defines.  Fool the test by using the
               ;; docbook-xsl catalog explicitly and get on with life.
               (("\"\\$XML_CATALOG_FILE\" \
\"http://docbook.sourceforge.net/release/xsl/")
                (string-append (car (find-files (assoc-ref inputs "docbook-xsl")
                                                "^catalog.xml$"))
                               " \"http://docbook.sourceforge.net/release/xsl/")))
             #t))
         (add-after 'install 'wrap-executables
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each (lambda (prog)
                           (wrap-program prog
                             `("PYTHONPATH" ":" prefix (,(getenv "PYTHONPATH")))))
                         (find-files (string-append out "/bin")))
               #t))))
       #:configure-flags
       (list (string-append "--with-xml-catalog="
                            (assoc-ref %build-inputs "docbook-xml")
                            "/xml/dtd/docbook/catalog.xml"))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("itstool" ,itstool)
       ("libxml" ,libxml2)
       ("gettext" ,gettext-minimal)
       ("bc" ,bc)))
    (inputs
     `(("perl" ,perl)
       ("python" ,python)
       ("xsltproc" ,libxslt)
       ("dblatex" ,dblatex)
       ("docbook-xml" ,docbook-xml-4.3)
       ("docbook-xsl" ,docbook-xsl)
       ("source-highlight" ,source-highlight)
       ("glib" ,glib)
       ("python-six" ,python-six)))
    (home-page "https://www.gtk.org/gtk-doc/")
    (synopsis "Documentation generator from C source code")
    (description
     "GTK-Doc generates API documentation from comments added to C code.  It is
typically used to document the public API of GTK+ and GNOME libraries, but it
can also be used to document application code.")
    (license license:gpl2+)))

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
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (inputs
     ;; Don't propagate GTK+ to reduce "profile pollution".
     `(("gtk+" ,gtk+-2))) ; required by gtk-engines-2.pc
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
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (propagated-inputs
     `(("gtk+" ,gtk+-2)))
    (home-page "https://live.gnome.org/GnomeArt")
    (synopsis "Cairo-based theming engine for GTK+ 2.x")
    (description
     "Murrine is a cairo-based GTK+ theming engine.  It is named after the
glass artworks done by Venicians glass blowers.")
    (license license:gpl2+)))

(define-public gtkspell3
  (package
    (name "gtkspell3")
    (version "3.0.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/gtkspell/"
                                  version "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "09jdicmpipmj4v84gnkqwbmj4lh8v0i6pn967rb9jx4zg2ia9x54"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gobject-introspection" ,gobject-introspection)
       ("gtk+" ,gtk+)
       ("pango" ,pango)))
    (propagated-inputs
     `(("enchant" ,enchant-1.6)))          ;gtkspell3-3.0.pc refers to it
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
             (url "https://github.com/CristianHenzel/ClipIt.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05xi29v2y0rvb33fmvrz7r9j4l858qj7ngwd7dp4pzpkkaybjln0"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gtk+" ,gtk+-2)))
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
    (version "1.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/ebassi/graphene/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32 "1zd2daj7y590wnzn4jw0niyc4fnzgxrcl9i7nwhy8b25ks2hz5wq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--enable-introspection=yes")))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("which" ,which)
       ("pkg-config" ,pkg-config)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (inputs
     `(("python" ,python)
       ("python-2" ,python-2)
       ("glib" ,glib)
       ("gobject-introspection" ,gobject-introspection)))
    (home-page "http://ebassi.github.io/graphene")
    (synopsis "Thin layer of graphic data types")
    (description "This library provides graphic types and their relative API;
it does not deal with windowing system surfaces, drawing, scene graphs, or
input.")
    (license license:expat)))

(define-public spread-sheet-widget
  (package
    (name "spread-sheet-widget")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://alpha.gnu.org/gnu/ssw/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32 "1h93yyh2by6yrmkwqg38nd5knids05k5nqzcihc1hdwgzg3c4b8y"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("glib" ,glib "bin") ; for glib-genmarshal, etc.
       ("pkg-config" ,pkg-config)))
    ;; In 'Requires' of spread-sheet-widget.pc.
    (propagated-inputs
     `(("glib" ,glib)
       ("gtk+" ,gtk+)))
    (home-page "https://www.gnu.org/software/ssw/")
    (synopsis "Gtk+ widget for dealing with 2-D tabular data")
    (description
     "GNU Spread Sheet Widget is a library for Gtk+ which provides a widget for
viewing and manipulating 2 dimensional tabular data in a manner similar to many
popular spread sheet programs.")
    (license license:gpl3+)))

(define-public yad
  (package
    (name "yad")
    (version "0.41.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/v1cont/yad.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hkxiich898sbacpg3jflf6i8l4hkfnc0zh10rr376v0mnzbn6jn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       '("--with-gtk=gtk3"
         "--enable-html"
         "--enable-gio"
         "--enable-spell"
         "--enable-icon-browser")
       #:phases
       (modify-phases %standard-phases
         (replace 'bootstrap
           (lambda _
             (invoke "autoreconf" "-vif")
             (invoke "intltoolize" "--force" "--automake")
             #t)))))
    (inputs
     `(("gtk+" ,gtk+)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (home-page "https://sourceforge.net/projects/yad-dialog/")
    (synopsis "GTK+ dialog boxes for shell scripts")
    (description
     "This program allows you to display GTK+ dialog boxes from command line or
shell scripts.  Example of how to use @code{yad} can be consulted at
@url{https://sourceforge.net/p/yad-dialog/wiki/browse_pages/}.")
    (license license:gpl3+)))
