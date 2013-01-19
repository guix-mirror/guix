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

(define-module (gnu packages freetype)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:renamer (symbol-prefix-proc 'license:))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public freetype
  (package
   (name "freetype")
   (version "2.4.11")
   (source (origin
            (method url-fetch)
            (uri (string-append
                   "http://download.savannah.gnu.org/releases/freetype/freetype-"
                   version ".tar.gz"))
            (sha256 (base32
                     "0gpcz6swir64kp0dk3rwgqqkmf48b90dqgczdmznjjryhrahx9r9"))))
   (build-system gnu-build-system)
   (synopsis "Freetype, a library to render fonts")
   (description
    "Freetype is a library that can be used by applications to access the
contents of font files. It provides a uniform interface to access font files.
It supports both bitmap and scalable formats, including TrueType, OpenType,
Type1, CID, CFF, Windows FON/FNT, X11 PCF, and others. It supports high-speed
anti-aliased glyph bitmap generation with 256 gray levels.")
   (license license:freetype)           ; some files have other licenses
   (home-page "http://www.freetype.org/")))
