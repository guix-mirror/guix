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

(define-module (gnu packages libpng)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module ((guix licenses) #:renamer (symbol-prefix-proc 'license:))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public libpng
  (package
   (name "libpng")
   (version "1.5.13")
   (source (origin
            (method url-fetch)
            (uri (string-append
                   "http://downloads.sourceforge.net/project/libpng/libpng15/"
                   version "/libpng-"
                   version ".tar.gz"))
            (sha256 (base32
                     "0dbh332qjhm3pa8m4ac73rk7dbbmigbqd3ch084m24ggg9qq4k0d"))))
   (build-system gnu-build-system)
   (inputs `(("zlib" ,zlib)))
   (synopsis "Libpng, a library for handling PNG files")
   (description
    "Libpng is the official PNG (Portable Network Graphics) reference
library. It supports almost all PNG features and is extensible.")
   (license license:zlib)
   (home-page "http://www.libpng.org/pub/png/libpng.html")))
