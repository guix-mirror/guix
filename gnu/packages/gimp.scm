;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages image)
  #:use-module ((gnu packages ghostscript)
                #:select (lcms))
  #:use-module (gnu packages compression)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages imagemagick))

(define-public babl
  (package
    (name "babl")
    (version "0.1.10")
    (source (origin
              (method url-fetch)
              (uri (list (string-append "http://ftp.gtk.org/pub/babl/0.1/babl-"
                                        version ".tar.bz2")
                         (string-append "ftp://ftp.gtk.org/pub/babl/0.1/babl-"
                                        version ".tar.bz2")))
              (sha256
               (base32
                "1x2mb7zfbvk9d0a7h5cpdff9hhjsadxvqml2jay2bpf7x9nc6gwl"))))
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
                "09nlv06li9nrn74ifpm7223mxpg0s7cii702z72cpbwrjh6nlbnz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
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
    (version "2.8.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.gimp.org/pub/gimp/v2.8/gimp-"
                                        version ".tar.bz2"))
              (sha256
               (base32
                "1rha8yx0pplfjziqczjrxxp16vsvpmb5ziq3c218s4w9z4cqpzg7"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags       
       ;; We don't have pygtk which seems to be needed for this feature
       `("--disable-python" ))) 
    (inputs
     `(("babl" ,babl)
       ("glib" ,glib)
       ("libtiff" ,libtiff)
       ("libjpeg" ,libjpeg-8)
       ("atk" ,atk)
       ("gtk+" ,gtk+-2)
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
