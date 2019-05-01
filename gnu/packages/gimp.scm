;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Leo Famulari <leo@famulari.name>
;;; Copyright © 2018 Thorsten Wilms <t_w_@freenet.de>
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

(define-module (gnu packages gimp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages image)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages python)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xorg))

(define-public babl
  (package
    (name "babl")
    (version "0.1.62")
    (source (origin
              (method url-fetch)
              (uri (list (string-append "https://download.gimp.org/pub/babl/"
                                        (version-major+minor version)
                                        "/babl-" version ".tar.bz2")
                         (string-append "https://ftp.gtk.org/pub/babl/"
                                        (version-major+minor version)
                                        "/babl-" version ".tar.bz2")
                         (string-append "ftp://ftp.gtk.org/pub/babl/"
                                        (version-major+minor version)
                                        "/babl-" version ".tar.bz2")))
              (sha256
               (base32
                "047msfzj8v4sfl61a2xhd69r9rh2pjq4lzpk3j10ijyv9qbry9yw"))))
    (build-system gnu-build-system)
    (home-page "http://gegl.org/babl/")
    (synopsis "Image pixel format conversion library")
    (description
     "Babl is a dynamic, any-to-any pixel format translation library.
It allows converting between different methods of storing pixels, known as
@dfn{pixel formats}, that have different bit depths and other data
representations, color models, and component permutations.

A vocabulary to formulate new pixel formats from existing primitives is
provided, as well as a framework to add new color models and data types.")
    (license license:lgpl3+)))

(define-public gegl
  (package
    (name "gegl")
    (version "0.4.14")
    (source (origin
              (method url-fetch)
              (uri (list (string-append "https://download.gimp.org/pub/gegl/"
                                        (string-take version 3)
                                        "/gegl-" version ".tar.bz2")))
              (sha256
               (base32
                "00crhngwi07f5b9x77kx5p7p4cl6l5g6glpz9rqv7pfqk62xa0ac"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("LDFLAGS=-lm")))
    ;; These are propagated to satisfy 'gegl-0.4.pc'.
    (propagated-inputs
     `(("babl" ,babl)
       ("glib" ,glib)
       ("json-glib" ,json-glib)))
    (inputs
     `(("cairo" ,cairo)
       ("pango" ,pango)
       ("libpng" ,libpng)
       ("libjpeg" ,libjpeg)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("glib" ,glib "bin")             ; for gtester
       ("intltool" ,intltool)))
    (home-page "http://gegl.org")
    (synopsis "Graph based image processing framework")
    (description "GEGL (Generic Graphics Library) provides infrastructure to
do demand based cached non destructive image editing on larger than RAM
buffers.")
    ;; The library itself is licensed under LGPL while the sample commandline
    ;; application and GUI binary gegl is licensed under GPL.
    (license (list license:lgpl3+ license:gpl3+))))

(define-public gimp
  (package
    (name "gimp")
    (version "2.10.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.gimp.org/pub/gimp/v"
                                  (version-major+minor version)
                                  "/gimp-" version ".tar.bz2"))
              (sha256
               (base32
                "0xwck5nbpb945s1cyij3kfqw1pchbhx8i5vf5hgywyjw4r1z5l8j"))))
    (build-system gnu-build-system)
    (outputs '("out"
               "doc"))                            ; 9 MiB of gtk-doc HTML
    (arguments
     '(#:configure-flags
       (list (string-append "--with-html-dir="
                            (assoc-ref %outputs "doc")
                            "/share/gtk-doc/html")
             ;; ./configure requests not to annoy upstream with packaging bugs.
             "--with-bug-report-url=https://bugs.gnu.org/guix")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-sitecustomize.py
           ;; Install 'sitecustomize.py' into gimp's python directory to
           ;; add pygobject and pygtk to pygimp's search path.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((pythonpath (getenv "PYTHONPATH"))
                    (out        (assoc-ref outputs "out"))
                    (sitecustomize.py
                     (string-append
                      out "/lib/gimp/2.0/python/sitecustomize.py")))
               (call-with-output-file sitecustomize.py
                 (lambda (port)
                   (format port "import site~%")
                   (format port "for dir in '~a'.split(':'):~%" pythonpath)
                   (format port "    site.addsitedir(dir)~%")))))))))
    (inputs
     `(("babl" ,babl)
       ("glib" ,glib)
       ("glib-networking" ,glib-networking)
       ("libtiff" ,libtiff)
       ("libjpeg" ,libjpeg)
       ("atk" ,atk)
       ("gexiv2" ,gexiv2)
       ("gtk+" ,gtk+-2)
       ("libmypaint" ,libmypaint)
       ("mypaint-brushes" ,mypaint-brushes)
       ("exif" ,libexif)                ; optional, EXIF + XMP support
       ("lcms" ,lcms)                   ; optional, color management
       ("librsvg" ,librsvg)             ; optional, SVG support
       ("poppler" ,poppler)             ; optional, PDF support
       ("poppler-data" ,poppler-data)
       ("python" ,python-2)             ; optional, Python support
       ("python2-pygtk" ,python2-pygtk) ; optional, Python support
       ("gegl" ,gegl)))
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-compile-resources and gdbus-codegen
       ("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (home-page "https://www.gimp.org")
    (synopsis "GNU Image Manipulation Program")
    (description
     "GIMP is an application for image manipulation tasks such as photo
retouching, composition and authoring.  It supports all common image formats
as well as specialized ones.  It features a highly customizable interface
that is extensible via a plugin system.")
    (license license:gpl3+))) ; some files are lgplv3

(define-public gimp-fourier
  (package
    (name "gimp-fourier")
    (version "0.4.3-2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://registry.gimp.org/files/fourier-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1rpacyad678lqgxa3hh2n0zpg4azs8dpa8q079bqsl12812k9184"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;no tests
       #:phases
       (modify-phases %standard-phases
         ;; FIXME: The gegl package only installs "gegl-0.4.pc", but
         ;; "gimp-2.0.pc" requires "gegl-0.3.pc", so we just copy it.
         (replace 'configure
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "tmppkgconfig")
             (copy-file (string-append (assoc-ref inputs "gegl")
                                       "/lib/pkgconfig/gegl-0.4.pc")
                        "tmppkgconfig/gegl-0.3.pc")
             (setenv "PKG_CONFIG_PATH"
                     (string-append "tmppkgconfig:"
                                    (or (getenv "PKG_CONFIG_PATH") "")))
             #t))
         (add-after 'unpack 'set-prefix
           (lambda* (#:key outputs #:allow-other-keys)
             ;; gimptool-2.0 does not allow us to install to any target
             ;; directory.
             (let ((target (string-append (assoc-ref outputs "out")
                                          "/lib/gimp/"
                                          (car (string-split ,(package-version gimp) #\.))
                                          ".0/plug-ins")))
               (substitute* "Makefile"
                 (("\\$\\(PLUGIN_INSTALL\\) fourier")
                  (string-append "cp fourier " target)))
               (mkdir-p target))
             #t)))))
    (inputs
     `(("fftw" ,fftw)
       ("gimp" ,gimp)
       ;; needed by gimp-2.0.pc
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("gegl" ,gegl)
       ("cairo" ,cairo)
       ("glib" ,glib)
       ;; needed by gimpui-2.0.pc
       ("gtk+" ,gtk+-2)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://registry.gimp.org/node/19596")
    (synopsis "GIMP plug-in to edit image in fourier space")
    (description
     "This package provides a simple plug-in to apply the fourier transform on
an image, allowing you to work with the transformed image inside GIMP.  You
can draw or apply filters in fourier space and get the modified image with an
inverse fourier transform.")
    (license license:gpl3+)))

(define-public libmypaint
  (package
    (name "libmypaint")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mypaint/libmypaint/"
                                  "releases/download/v" version "/libmypaint-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0wd6jk69vmhsq1mdw96v0fh7b28n3glkr5ca466zcq7agzaxj1va"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    ;; As needed by 'libmypaint.pc'.
    (propagated-inputs
     `(("json-c" ,json-c)
       ("gobject-introspection" ,gobject-introspection)))
    (inputs
     `(("glib" ,glib)))
    (synopsis "Artistic brushes library")
    (description "Libmypaint, also called \"brushlib\", is a library for making
brushstrokes which is used by MyPaint and GIMP.")
    (home-page "http://mypaint.org")
    (license license:isc)))

(define-public mypaint-brushes
  (package
    (name "mypaint-brushes")
    (version "1.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Jehan/mypaint-brushes.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1iz89z6v2mp8j1lrf942k561s8311i3s34ap36wh4rybb2lq15m0"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'relax-dependency-version
           (lambda _
             (substitute* "autogen.sh"
               (("automake-1.13") "automake")
               (("aclocal-1.13") "aclocal"))
             #t)))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)))
    (synopsis "Default brushes for MyPaint")
    (description "This package provides the default set of brushes for
MyPaint.")
    (home-page "https://github.com/Jehan/mypaint-brushes")
    (license license:cc0)))

(define-public gimp-resynthesizer
  ;; GIMP does not respect any plugin search path environment variable, so after
  ;; installation users have to edit their GIMP settings to include
  ;; "$HOME/.guix-profile/lib/gimp/2.0/plug-ins/" in
  ;; “Edit->Preferences->Folders->Plug Ins”.
  (package
    (name "gimp-resynthesizer")
    (version "2.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/bootchk/resynthesizer/archive/v"
			   version ".tar.gz"))
       (sha256
        (base32
         "0l3404w6rqny7h3djskxf149gzx6x4qhndgbh3403c9lbh4pi1kr"))
       (file-name (string-append name "-" version ".tar.gz"))))
    (build-system gnu-build-system)
    (arguments
     `( ;; Turn off tests to avoid:
       ;; make[1]: *** No rule to make target '../src/resynth-gui.c', needed by 'resynthesizer.pot'.  Stop.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
	 (add-after 'unpack 'set-env
	   (lambda _
	     (setenv "CONFIG_SHELL" (which "sh"))
	     #t))
	 (add-after 'configure 'set-prefix
           ;; Install plugin under $prefix, not under GIMP's libdir.
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((target (string-append (assoc-ref outputs "out")
                                          "/lib/gimp/"
                                          ,(version-major
                                            (package-version gimp))
                                          ".0")))
               (substitute* (list "src/resynthesizer/Makefile"
				  "src/resynthesizer-gui/Makefile")
		 (("GIMP_LIBDIR = .*")
                  (string-append "GIMP_LIBDIR = " target "\n")))
               (mkdir-p target)
               #t))))))
    (native-inputs
     `(("autoconf" ,autoconf-wrapper)
       ("automake" ,automake)
       ("glib" ,glib "bin")                       ; glib-gettextize
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gimp" ,gimp)
       ("gdk-pixbuf" ,gdk-pixbuf)                 ; needed by gimp-2.0.pc
       ("cairo" ,cairo)
       ("gegl" ,gegl)
       ("gtk+" ,gtk+-2)                           ; needed by gimpui-2.0.pc
       ("glib" ,glib)))
    (home-page "https://github.com/bootchk/resynthesizer")
    (synopsis "GIMP plugins for texture synthesis")
    (description
     "This package provides resynthesizer plugins for GIMP, which encompasses
tools for healing selections (content-aware fill), enlarging the canvas and
healing the border, increasing the resolution while adding detail, and
transferring the style of an image.")
    (license license:gpl3+)))
