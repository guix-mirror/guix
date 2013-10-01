;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
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
  #:use-module ((guix licenses)
                #:renamer (symbol-prefix-proc 'license:))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages libjpeg)
  #:use-module (gnu packages libpng)
  #:use-module (gnu packages libtiff)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public atk
  (package
   (name "atk")
   (version "2.10.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/" name "/"
                                (string-take version 4) "/" name "-"
                                version ".tar.xz"))
            (sha256
             (base32
              "1c2hbg66wfvibsz2ia0ri48yr62751fn950i97c53j3b0fjifsb3"))))
   (build-system gnu-build-system)
   (inputs `(("glib" ,glib)
             ("pkg-config" ,pkg-config)))
   (synopsis "GNOME accessability toolkit")
   (description
    "ATK provides the set of accessibility interfaces that are implemented
by other toolkits and applications. Using the ATK interfaces, accessibility
tools have full access to view and control running applications.")
   (license license:lgpl2.0+)
   (home-page "https://developer.gnome.org/atk/")))

(define-public cairo
  (package
   (name "cairo")
   (version "1.12.16")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://cairographics.org/releases/cairo-"
                                version ".tar.xz"))
            (sha256
             (base32
              "0inqwsylqkrzcjivdirkjx5nhdgxbdc62fq284c3xppinfg9a195"))))
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
      ("pkg-config" ,pkg-config)
      ("poppler" ,poppler)
      ("python" ,python-wrapper)
      ("xextproto" ,xextproto)
      ("zlib" ,zlib)))
    (arguments
      `(#:tests? #f)) ; see http://lists.gnu.org/archive/html/bug-guix/2013-06/msg00085.html
   (synopsis "2D graphics library")
   (description
    "Cairo is a 2D graphics library with support for multiple output devices.
Currently supported output targets include the X Window System (via both
Xlib and XCB), Quartz, Win32, image buffers, PostScript, PDF, and SVG file
output. Experimental backends include OpenGL, BeOS, OS/2, and DirectFB.

Cairo is designed to produce consistent output on all output media while
taking advantage of display hardware acceleration when available
eg. through the X Render Extension).

The cairo API provides operations similar to the drawing operators of
PostScript and PDF. Operations in cairo including stroking and filling cubic
Bézier splines, transforming and compositing translucent images, and
antialiased text rendering. All drawing operations can be transformed by any
affine transformation (scale, rotation, shear, etc.)")
   (license license:lgpl2.1) ; or Mozilla Public License 1.1
   (home-page "http://cairographics.org/")))

(define-public harfbuzz
  (package
   (name "harfbuzz")
   (version "0.9.21")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://www.freedesktop.org/software/harfbuzz/release/harfbuzz-"
                                version ".tar.bz2"))
            (sha256
             (base32
              "1s6sffgf6ndy12fyln2bdnkn3cb1qfkch0rakdgkgwlq7n46zlx0"))))
   (build-system gnu-build-system)
   (inputs
    `(("cairo" ,cairo)
      ("icu4c" ,icu4c)
      ("pkg-config" ,pkg-config)
      ("python" ,python-wrapper)))
   (synopsis "opentype text shaping engine")
   (description
    "HarfBuzz is an OpenType text shaping engine.")
   (license (license:x11-style "file://COPYING"
                       "See 'COPYING' in the distribution."))
   (home-page "http://www.freedesktop.org/wiki/Software/HarfBuzz/")))

(define-public pango
  (package
   (name "pango")
   (version "1.34.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/pango/1.34/pango-"
                                version ".tar.xz"))
            (sha256
             (base32
              "0k7662qix7zzh7mf6ikdj594n8jpbfm25z8swz64zbm86kgk1shs"))))
   (build-system gnu-build-system)
   (propagated-inputs
    `(("cairo" ,cairo)
      ("harfbuzz" ,harfbuzz)))
   (inputs
    `(("pkg-config" ,pkg-config)
      ("zlib" ,zlib)))
   (synopsis "GNOME text and font handling library")
   (description
    "Pango is the core text and font handling library used in GNOME
applications. It has extensive support for the different writing systems
used throughout the world.")
   (license license:lgpl2.0+)
   (home-page "https://developer.gnome.org/pango/")))

(define-public gdk-pixbuf
  (package
   (name "gdk-pixbuf")
   (version "2.28.2")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/gdk-pixbuf/2.28/gdk-pixbuf-"
                                version ".tar.xz"))
            (sha256
             (base32
              "05s6ksvy1yan6h6zny9n3bmvygcnzma6ljl6i0z9cci2xg116c8q"))))
   (build-system gnu-build-system)
   (inputs
    `(("glib" ,glib)
      ("libjpeg" ,libjpeg)
      ("libpng" ,libpng)
      ("libtiff" ,libtiff)
      ("pkg-config" ,pkg-config)))
   (synopsis "GNOME image loading and manipulation library")
   (description
    "GdkPixbuf is a library for image loading and manipulation developed
in the GNOME project.")
   (license license:lgpl2.0+)
   (home-page "https://developer.gnome.org/gdk-pixbuf/")))

(define-public at-spi2-core
  (package
   (name "at-spi2-core")
   (version "2.10.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/" name "/"
                                (string-take version 4) "/" name "-"
                                version ".tar.xz"))
            (sha256
             (base32
              "1ns44yibdgcwzwri7sr075hfs5rh5lgxkh71247a0822az3mahcn"))))
   (build-system gnu-build-system)
   (inputs `(("dbus" ,dbus)
             ("glib" ,glib)
             ("intltool" ,intltool)
             ("libxi" ,libxi)
             ("libxtst" ,libxtst)
             ("pkg-config" ,pkg-config)))
   (arguments
    `(#:tests? #f)) ; FIXME: dbind/dbtest fails; one should disable tests in
                    ; a more fine-grained way.
   (synopsis "Assistive Technology Service Provider Interface, core components")
   (description
    "The Assistive Technology Service Provider Interface, core components,
is part of the GNOME accessibility project.")
   (license license:lgpl2.0+)
   (home-page "https://projects.gnome.org/accessibility/")))

(define-public at-spi2-atk
  (package
   (name "at-spi2-atk")
   (version "2.10.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/" name "/"
                                (string-take version 4) "/" name "-"
                                version ".tar.xz"))
            (sha256
             (base32
              "150sqc21difazqd53llwfdaqnwfy73bic9hia41xpfy9kcpzz9yy"))))
   (build-system gnu-build-system)
   (inputs `(("atk" ,atk)
             ("at-spi2-core" ,at-spi2-core)
             ("dbus" ,dbus)
             ("glib" ,glib)
             ("pkg-config" ,pkg-config)))
   (arguments
    `(#:tests? #f)) ; FIXME: droute/droute-test fails; one should disable
                    ; tests in a more fine-grained way.
   (synopsis "Assistive Technology Service Provider Interface, ATK bindings")
   (description
    "The Assistive Technology Service Provider Interface
is part of the GNOME accessibility project.")
   (license license:lgpl2.0+)
   (home-page "https://projects.gnome.org/accessibility/")))

(define-public gtk+-2
  (package
   (name "gtk+")
   (version "2.24.21")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/" name "/"
                                (string-take version 4) "/" name "-"
                                version ".tar.xz"))
            (sha256
             (base32
              "1qyw73pr9ryqhir2h1kbx3vm70km4dg2fxrgkrdlpv0rvlb94bih"))))
   (build-system gnu-build-system)
   (propagated-inputs
    `(("atk" ,atk)
      ("gdk-pixbuf" ,gdk-pixbuf)
      ("pango" ,pango)))
   (inputs
    `(("perl" ,perl)
      ("pkg-config" ,pkg-config)
      ("python-wrapper" ,python-wrapper)))
   (arguments
    `(#:phases
      (alist-replace
       'configure
       (lambda* (#:key #:allow-other-keys #:rest args)
        (let ((configure (assoc-ref %standard-phases 'configure)))
          ;; FIXME: re-enable tests requiring an X server
          (substitute* "gtk/Makefile.in"
           (("SUBDIRS = theme-bits . tests") "SUBDIRS = theme-bits ."))
          (apply configure args)))
      %standard-phases)))
   (synopsis "Cross-platform toolkit for creating graphical user interfaces")
   (description
    "GTK+, or the GIMP Toolkit, is a multi-platform toolkit for creating
graphical user interfaces. Offering a complete set of widgets, GTK+ is
suitable for projects ranging from small one-off tools to complete
application suites.")
   (license license:lgpl2.0+)
   (home-page "http://www.gtk.org/")))

(define-public gtk+
  (package (inherit gtk+-2)
   (version "3.10.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/gtk+/"
                                (string-take version 4) "/gtk+-"
                                version ".tar.xz"))
            (sha256
             (base32
              "1zjkbjvp6ay08107r6zfsrp39x7qfadbd86p3hs5v4ydc2rzwnb5"))))
   (inputs
    `(("at-spi2-atk" ,at-spi2-atk)
      ("libxi" ,libxi)
      ("libxinerama" ,libxinerama)
      ("libxml2" ,libxml2)
      ("perl" ,perl)
      ("pkg-config" ,pkg-config)
      ("python-wrapper" ,python-wrapper)
      ("xorg-server" ,xorg-server)))
   (arguments
    `(#:configure-flags '("--enable-x11-backend") ; should not be needed in > 3.10.0
      #:phases
      (alist-replace
       'configure
       (lambda* (#:key #:allow-other-keys #:rest args)
         (let ((configure (assoc-ref %standard-phases 'configure)))
           ;; Disable most tests, failing in the chroot with the message:
           ;; D-Bus library appears to be incorrectly set up; failed to read
           ;; machine uuid: Failed to open "/etc/machine-id": No such file or
           ;; directory.
           ;; See the manual page for dbus-uuidgen to correct this issue.
           (substitute* "testsuite/Makefile.in"
            (("SUBDIRS = gdk gtk a11y css reftests") "SUBDIRS = gdk"))
           (apply configure args)))
       %standard-phases)))))

;;;
;;; Guile bindings.
;;;

(define-public guile-cairo
  (package
    (name "guile-cairo")
    (version "1.4.1")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://download.gna.org/guile-cairo/guile-cairo-"
                   version
                   ".tar.gz"))
             (sha256
              (base32
               "1f5nd9n46n6cwfl1byjml02q3y2hgn7nkx98km1czgwarxl7ws3x"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (alist-cons-before
                 'configure 'set-module-directory
                 (lambda* (#:key outputs #:allow-other-keys)
                   ;; Install modules under $out/share/guile/site/2.0.
                   (let ((out (assoc-ref outputs "out")))
                     (substitute* "Makefile.in"
                       (("scmdir = ([[:graph:]]+).*" _ value)
                        (string-append "scmdir = " value "/2.0\n")))
                     (substitute* "cairo/Makefile.in"
                       (("moduledir = ([[:graph:]]+).*" _ value)
                        (string-append "moduledir = "
                                       "$(prefix)/share/guile/site/2.0/cairo\n'")))))
                 (alist-cons-after
                  'install 'install-missing-file
                  (lambda* (#:key outputs #:allow-other-keys)
                    ;; By default 'vector-types.scm' is not installed, so do
                    ;; it here.
                    (let ((out (assoc-ref outputs "out")))
                      (copy-file "cairo/vector-types.scm"
                                 (string-append out "/share/guile/site/2.0"
                                                "/cairo/vector-types.scm"))))
                  %standard-phases))))
    (inputs
     `(("guile-lib" ,guile-lib)
       ("expat" ,expat)
       ("cairo" ,cairo)
       ("pkg-config" ,pkg-config)
       ("guile" ,guile-2.0)))
    (home-page "http://www.nongnu.org/guile-cairo/")
    (synopsis "Cairo bindings for GNU Guile")
    (description
     "Guile-Cairo wraps the Cairo graphics library for Guile Scheme.
Guile-Cairo is complete, wrapping almost all of the Cairo API.  It is API
stable, providing a firm base on which to do graphics work.  Finally, and
importantly, it is pleasant to use.  You get a powerful and well-maintained
graphics library with all of the benefits of Scheme: memory management,
exceptions, macros, and a dynamic programming environment.")
    (license license:lgpl3+)))
