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

(define-module (gnu packages geeqie)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses)
                #:renamer (symbol-prefix-proc 'l:))
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libpng)
  #:use-module ((gnu packages ghostscript)
                #:select (lcms))
  #:use-module (gnu packages compression)
  #:use-module (gnu packages xml))

(define-public exiv2                              ; XXX: move elsewhere?
  (package
    (name "exiv2")
    (version "0.23")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://www.exiv2.org/exiv2-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "04bbg2cg6mgcyz435zamx37sp5zw44n2alb59ki1daz71f851yl1"))))
    (build-system gnu-build-system)
    (arguments '(#:tests? #f))                    ; no `check' target
    (propagated-inputs
     `(("expat" ,expat) ("zlib" ,zlib)))
    (home-page "http://www.exiv2.org/")
    (synopsis "Library and command-line utility to manage image metadata")
    (description
     "Exiv2 is a C++ library and a command line utility to manage image
metadata. It provides fast and easy read and write access to the Exif, IPTC
and XMP metadata of images in various formats.")

    ;; Files under `xmpsdk' are a copy of Adobe's XMP SDK, licensed under the
    ;; 3-clause BSD license: <http://www.adobe.com/devnet/xmp/sdk/eula.html>.
    ;; The core is GPLv2+: <https://launchpad.net/ubuntu/precise/+source/exiv2/+copyright>.
    (license l:gpl2+)))

(define-public geeqie
  (package
    (name "geeqie")
    (version "1.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/geeqie/geeqie-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "1kzy39z9505xkayyx7rjj2wda76xy3ch1s5z35zn8yli54ffhi2m"))))
    (build-system gnu-build-system)
    (inputs
     `(;; ("libchamplain" ,libchamplain)
       ("lcms" ,lcms)
       ("exiv2" ,exiv2)
       ("libpng" ,libpng)
       ("gtk+" ,gtk+-2)))
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (home-page "http://geeqie.sourceforge.net")
    (synopsis "Lightweight GTK+ based image viewer")
    (description
     "Geeqie is a lightweight GTK+ based image viewer for Unix like operating
systems.  It features: EXIF, IPTC and XMP metadata browsing and editing
interoperability; easy integration with other software; geeqie works on files
and directories, there is no need to import images; fast preview for many raw
image formats; tools for image comparison, sorting and managing photo
collection.  Geeqie was initially based on GQview.")
    (license l:gpl2+)))
