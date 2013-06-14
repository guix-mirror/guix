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
  #:use-module (gnu packages libpng)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xorg))

(define-public atk
  (package
   (name "atk")
   (version "2.8.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/atk/2.8/atk-"
                                version ".tar.xz"))
            (sha256
             (base32
              "1x3dd3hg9l1j9dq70xwph13vxdp6a9wbfcnryryf1wr6c8bij9dj"))))
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
   (version "1.12.14")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://cairographics.org/releases/cairo-"
                                version ".tar.xz"))
            (sha256
             (base32
              "04xcykglff58ygs0dkrmmnqljmpjwp2qgwcz8sijqkdpz7ix3l4n"))))
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
      ("python" ,python)
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
   (version "0.9.18")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://www.freedesktop.org/software/harfbuzz/release/harfbuzz-"
                                version ".tar.bz2"))
            (sha256
             (base32
              "026rlwspf1zn5akds9fwibpqpn47kmlnmqm5fi0cp4k4dnygpw7y"))))
   (build-system gnu-build-system)
   (inputs
    `(("cairo" ,cairo)
      ("icu4c" ,icu4c)
      ("pkg-config" ,pkg-config)
      ("python" ,python)))
   (synopsis "opentype text shaping engine")
   (description
    "HarfBuzz is an OpenType text shaping engine.")
   (license (license:x11-style "file://COPYING"
                       "See 'COPYING' in the distribution."))
   (home-page "http://www.freedesktop.org/wiki/Software/HarfBuzz/")))
