;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages imagemagick)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:select (fsf-free))
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libpng)
  #:use-module (gnu packages libtiff)
  #:use-module (gnu packages libjpeg)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public imagemagick
  (package
    (name "imagemagick")
    (version "6.8.8-4")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://imagemagick/ImageMagick-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0bfxhfymkdbvardlr0nbjfmv53m47lcl9kkycipk4hxawfs927jr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (alist-cons-before
                 'build 'pre-build
                 (lambda* (#:key outputs #:allow-other-keys)
                   (substitute* "Makefile"
                     ;; Clear the `LIBRARY_PATH' setting, which otherwise
                     ;; interferes with our own use.
                     (("^LIBRARY_PATH[[:blank:]]*=.*$")
                      "")

                     ;; Since the Makefile overrides $docdir, modify it to
                     ;; refer to what we want.
                     (("^DOCUMENTATION_PATH[[:blank:]]*=.*$")
                      (let ((doc (assoc-ref outputs "doc")))
                        (string-append "DOCUMENTATION_PATH = "
                                       doc "/share/doc/"
                                       ,name "-" ,version "\n")))))
                 %standard-phases)))
    ;; TODO: Add Jasper etc.
    (inputs `(("fftw" ,fftw)
              ("graphviz" ,graphviz)
              ("ghostscript" ,ghostscript)
              ("lcms" ,lcms)
              ("libx11" ,libx11)
              ("zlib" ,zlib)
              ("libxml2" ,libxml2)
              ("libtiff" ,libtiff)
              ("libpng" ,libpng)
              ("libjpeg" ,libjpeg-8)
              ("pango" ,pango)
              ("freetype" ,freetype)
              ("bzip2" ,bzip2)
              ("xz" ,xz)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (outputs '("out"
               "doc"))                          ; 26 MiB of HTML documentation
    (home-page "http://www.imagemagick.org/")
    (synopsis "Create, edit, compose, or convert bitmap images")
    (description
     "ImageMagick® is a software suite to create, edit, compose, or convert
bitmap images. It can read and write images in a variety of formats (over 100)
including DPX, EXR, GIF, JPEG, JPEG-2000, PDF, PhotoCD, PNG, Postscript, SVG,
and TIFF. Use ImageMagick to resize, flip, mirror, rotate, distort, shear and
transform images, adjust image colors, apply various special effects, or draw
text, lines, polygons, ellipses and Bézier curves.")
    (license (fsf-free "http://www.imagemagick.org/script/license.php"))))
