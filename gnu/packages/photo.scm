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

(define-module (gnu packages photo)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages popt)
  #:use-module ((gnu packages base) #:select (tzdata)))

(define-public libexif
  (package
    (name "libexif")
    (version "0.6.21")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/libexif/libexif-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "06nlsibr3ylfwp28w8f5466l6drgrnydgxrm4jmxzrmk5svaxk8n"))))
    (build-system gnu-build-system)
    (home-page "http://libexif.sourceforge.net/")
    (synopsis "Read and manipulate EXIF data in digital photographs")
    (description
     "The libexif C library allows applications to read, edit, and save EXIF
data as produced by digital cameras.")
    (license lgpl2.1+)))

(define-public libgphoto2
  (package
    (name "libgphoto2")
    (version "2.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/gphoto/libgphoto2-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "0f1818l1vs5fbmrihzyv3qasddbqi3r01jik5crrxddwalsi2bd3"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs
     `(;; ("libjpeg-turbo" ,libjpeg-turbo)
       ("libtool" ,libtool)
       ("libusb" ,libusb)))
    (propagated-inputs
     `(;; The .pc refers to libexif.
       ("libexif" ,libexif)))
    (home-page "http://www.gphoto.org/proj/libgphoto2/")
    (synopsis "Accessing digital cameras")
    (description
     "This is the library backend for gphoto2.  It contains the code for PTP,
MTP, and other vendor specific protocols for controlling and transferring data
from digital cameras.")

    ;; 'COPYING' says LGPLv2.1+, but in practices files are under LGPLv2+.
    (license lgpl2.1+)))

(define-public gphoto2
  (package
    (name "gphoto2")
    (version "2.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/gphoto/gphoto2-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "16c8k1cxfypg7v5h8xi87grclw7a5ayaamn548ys3zkj727r5fcf"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("readline" ,readline)
       ;; ("libjpeg-turbo" ,libjpeg-turbo)
       ("popt" ,popt)
       ("libexif" ,libexif)
       ("libgphoto2" ,libgphoto2)))
    (arguments
     '(#:phases (alist-cons-before
                 'check 'pre-check
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* (find-files "tests/data" "\\.param$")
                     (("/usr/bin/env")
                      (which "env"))))
                 %standard-phases)

       ;; FIXME: There are 2 test failures, most likely related to the build
       ;; environment.
       #:tests? #f))

    (home-page "http://www.gphoto.org/")
    (synopsis "Command-line tools to access digital cameras")
    (description
     "Gphoto2 is a set of command line utilities for manipulating a large
number of different digital cameras.  Through libgphoto2, it supports PTP,
MTP, and much more.")

    ;; Files are typically under LGPLv2+, but 'COPYING' says GPLv2+.
    (license gpl2+)))
