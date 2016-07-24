;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Craven <david@craven.ch>
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

(define-module (gnu packages spice)
  #:use-module (gnu packages)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils))

(define-public usbredir
  (package
    (name "usbredir")
    (version "0.7.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                "http://spice-space.org/download/usbredir/"
                "usbredir-" version ".tar.bz2"))
              (sha256
               (base32
                "1wsnmk4wjpdhbn1zaxg6bmyxspcki2zgy0am9lk037rnl4krwzj0"))))
    (build-system gnu-build-system)
    (inputs
      `(("libusb" ,libusb)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (synopsis "Tools for sending USB device traffic over a network")
    (description "Usbredir is a network protocol for sending USB device traffic
over a network connection.  It can be used to redirect traffic from a USB device
to a different (virtual) machine than the one to which the USB device is
attached.")
    (home-page "http://www.spice-space.org")
    (license (list license:gpl2+ license:lgpl2.0+ license:lgpl2.1+))))

(define-public virglrenderer
  (package
    (name "virglrenderer")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                "https://www.freedesktop.org/software/virgl/"
                "virglrenderer-" version ".tar.bz2"))
              (sha256
               (base32
                "1dj0j8nbyr7nrpds4dqlp43ji8ixjyqhgw6ywlz1r9dn6cs5m5d1"))))
    (build-system gnu-build-system)
    (inputs
      `(("libepoxy" ,libepoxy)
        ("mesa" ,mesa)
        ("udev" ,eudev)))
    (native-inputs
      `(("pkg-config" ,pkg-config)))
    (synopsis "Virtual 3D GPU library")
    (description "A virtual 3D GPU library that enables a virtualized operating
system to use the host GPU to accelerate 3D rendering.")
    (home-page "https://virgil3d.github.io")
    (license (list license:expat license:bsd-3))))
