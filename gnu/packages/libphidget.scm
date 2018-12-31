;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages libphidget)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (gnu packages libusb))

(define-public libphidget
  (package
   (name "libphidget")
   (version "2.1.8.20180607")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://www.phidgets.com/downloads/phidget21/libraries/"
                  "linux/libphidget/libphidget_"
                  version ".tar.gz"))
            (sha256
             (base32 "1mdw8k13yy5dq3r41v9w5yijdk87alzix0qagslc2mxh1pf17npn"))))
   (build-system gnu-build-system)
   (inputs `(("libusb" ,libusb)))
   (outputs '("out" "debug"))
   (home-page "https://www.phidgets.com/")
   (license lgpl3+)
   (synopsis "C library to manipulate Phidgets")
   (description synopsis)))
