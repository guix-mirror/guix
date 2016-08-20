;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages image)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages imagemagick))

(define-public babl
  (package
    (name "babl")
    (version "0.1.18")
    (source (origin
              (method url-fetch)
              (uri (list (string-append "https://download.gimp.org/pub/babl/"
                                        "0.1/babl-" version ".tar.bz2")
                         (string-append "http://ftp.gtk.org/pub/babl/0.1/babl-"
                                        version ".tar.bz2")
                         (string-append "ftp://ftp.gtk.org/pub/babl/0.1/babl-"
                                        version ".tar.bz2")))
              (sha256
               (base32
                "1ygvnq22pf0zvf3bj7h67vvbpz7b8hhjvrr79ribws7sr5dljfj8"))))
    (build-system gnu-build-system)
    (home-page "http://gegl.org/babl/")
    (synopsis "Image pixel format conversion library")
    (description
     "Babl is a dynamic, any to any, pixel format translation library.
It allows converting between different methods of storing pixels known as
pixel formats that have with different bitdepths and other data
representations, color models and component permutations.

A vocabulary to formulate new pixel formats from existing primitives is
provided as well as the framework to add new color models and data types.")
    (license license:lgpl3+)))

(define-public gegl
  (package
    (name "gegl")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (list (string-append "http://download.gimp.org/pub/gegl/"
                                        (string-take version 3)
                                        "/" name "-" version ".tar.bz2")))
              (sha256
               (base32
                "09nlv06li9nrn74ifpm7223mxpg0s7cii702z72cpbwrjh6nlbnz"))
              (patches (search-patches "gegl-CVE-2012-4433.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(;; More than just the one test disabled below now fails; disable them
       ;; all according to the rationale given below.
       #:tests? #f
       #:phases
       (alist-cons-before
        'build 'pre-build
        (lambda _
          ;; This test program seems to crash on exit. Specifically, whilst
          ;; g_object_unreffing bufferA and bufferB - This seems to be a bug
          ;; in the destructor.  This is just a test program so will not have
          ;; any wider effect, although might be hiding another problem.
          ;; According to advice received on irc.gimp.org#gegl although 0.2.0
          ;; is the latest released version, any bug reports against it will
          ;; be ignored.  So we are on our own.
          (substitute* "tools/img_cmp.c"
            (("g_object_unref \\(buffer.\\);") ""))

          (substitute* "tests/compositions/Makefile"
            (("/bin/sh") (which "bash"))))
        %standard-phases)))
    (inputs
     `(("babl" ,babl)
       ("glib" ,glib)
       ("cairo" ,cairo)
       ("pango" ,pango)
       ("libpng" ,libpng)
       ("libjpeg" ,libjpeg-8)))
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
    (version "2.8.18")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.gimp.org/pub/gimp/v"
                                  (version-major+minor version)
                                  "/gimp-" version ".tar.bz2"))
              (sha256
               (base32
                "0halh6sl3d2j9gahyabj6h6r3yyldcy7sfb4qrfazpkqqr3j5p9r"))))
    (build-system gnu-build-system)
    (outputs '("out"
               "doc"))                            ;8 MiB of gtk-doc HTML
    (arguments
     '(#:configure-flags (list (string-append "--with-html-dir="
                                              (assoc-ref %outputs "doc")
                                              "/share/gtk-doc/html"))
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
       ("libtiff" ,libtiff)
       ("libjpeg" ,libjpeg-8)
       ("atk" ,atk)
       ("gtk+" ,gtk+-2)
       ("exif" ,libexif)                         ;optional, EXIF + XMP support
       ("lcms" ,lcms)                            ;optional, color management
       ("librsvg" ,librsvg)                      ;optional, SVG support
       ("python" ,python-2)                      ;optional, Python support
       ("python2-pygtk" ,python2-pygtk)          ;optional, Python support
       ("gegl" ,gegl)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (home-page "http://gimp.org")
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
         (delete 'configure)
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
