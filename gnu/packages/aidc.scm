;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darringon <jmd@gnu.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Guillaume Le Vaillant <glv@posteo.net>
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

(define-module (gnu packages aidc)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages video)
  #:use-module (guix build-system gnu))


(define-public barcode
  (package
    (name "barcode")
    (version "0.99")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/barcode/barcode-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1indapql5fjz0bysyc88cmc54y8phqrbi7c76p71fgjp45jcyzp8"))))
    (build-system gnu-build-system)
    (synopsis "Convert text strings to printed bars in various standards")
    (description "GNU Barcode is a flexible tool to produce printed barcodes
from text strings.  It supports a variety of encoding standards and sizing
measurements.  Barcodes can be output in PostScript or Encapsulated PostScript
formats.")
    (license license:gpl3+)
    (home-page "https://www.gnu.org/software/barcode/")))

(define-public qrencode
  (package
    (name "qrencode")
    (version "4.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://fukuchi.org/works/qrencode/qrencode-" version
                    ".tar.bz2"))
              (sha256
               (base32
                "1d2q5d3v8g3hsi3h5jq4n177bjhf3kawms09immw7p187f6jgjy9"))))
    (build-system gnu-build-system)
    (inputs `(("libpng" ,libpng)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (synopsis "Encode data into a QR Code symbol")
    (description "Libqrencode is a C library for encoding data in a QR Code
symbol, a kind of 2D symbology that can be scanned by handy terminals such as
a mobile phone with CCD.  The capacity of QR Code is up to 7000 digits or 4000
characters, and is highly robust.")
    (license license:lgpl2.1+)
    (home-page "https://fukuchi.org/works/qrencode")))

(define-public libdmtx
  (package
    (name "libdmtx")
    (version "0.7.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dmtx/libdmtx.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wk3fkxzf9ip75v8ia54v6ywx72ajp5s6777j4ay8barpbv869rj"))))
    (build-system gnu-build-system)
    (arguments
     ;; XXX Test suite is broken: https://github.com/dmtx/libdmtx/issues/22
     `(#:tests? #f))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (home-page "https://github.com/dmtx")
    (synopsis "Library for reading and writing Data Matrix 2D barcodes")
    (description "libdmtx is software for reading and writing Data Matrix 2D
barcodes of the modern ECC200 variety.  libdmtx is a shared library, allowing
C/C++ programs to use its capabilities without restrictions or overhead.")
    (license license:bsd-3)))

(define-public zbar
  (package
    (name "zbar")
    (version "0.23")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://linuxtv.org/downloads/zbar/zbar-"
                           version
                           ".tar.bz2"))
       (sha256
        (base32
         "0bmd93a15qpgbsq9c9j33qms18rdrgz6gbc48zi6z9w5pvrvi7z9"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags (list "--with-gtk=auto"
                               "--with-python=auto"
                               (string-append "--with-dbusconfdir="
                                              (assoc-ref %outputs "out")
                                              "/etc")
                               "CXXFLAGS=-std=c++11")))
    (native-inputs
     `(("glib" ,glib "bin")
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gobject-introspection" ,gobject-introspection)
       ("gtk+" ,gtk+)
       ("imagemagick" ,imagemagick)
       ("libjpeg" ,libjpeg-turbo)
       ("python" ,python)
       ("qtbase" ,qtbase)
       ("qtx11extras" ,qtx11extras)
       ("v4l-utils" ,v4l-utils)))
    (synopsis "Bar code reader")
    (description
     "ZBar can read barcodes from various sources, such as video streams,
image files, and raw intensity sensors.  It supports EAN-13/UPC-A, UPC-E,
EAN-8, Code 128, Code 93, Code 39, Codabar, Interleaved 2 of 5, QR Code and SQ
Code.  Included with the library are basic applications for decoding captured
bar code images and using a video device (e.g. webcam) as a bar code scanner.
For application developers, language bindings are included for C, C++ and
Python as well as GUI widgets for GTK and Qt.")
    (home-page "https://github.com/mchehab/zbar")
    (license license:lgpl2.1+)))
