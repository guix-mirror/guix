;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Manolis Fragkiskos Ragkousis <manolis837@gmail.com>
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

(define-module (gnu packages libftdi)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages libusb)
  #:use-module (guix build-system cmake))

(define-public libftdi
  (package
    (name "libftdi")
    (version "1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.intra2net.com/en/developer/libftdi/download/libftdi1-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "088yh8pxd6q53ssqndydcw1dkq51cjqyahc03lm6iip22cdazcf0"))))
    (build-system cmake-build-system)
    (inputs
     `(("libusb" ,libusb)))
    (home-page "http://www.intra2net.com/en/developer/libftdi/")
    (synopsis "FTDI USB driver with bitbang mode")
    (description
     "libFTDI is a library to talk to FTDI chips: FT232BM,
FT245BM, FT2232C, FT2232D, FT245R and FT232H including the popular
bitbangmode.")
    (license lgpl2.1)))
