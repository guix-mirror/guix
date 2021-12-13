;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
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
   (version "2.1.9.20190409")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://www.phidgets.com/downloads/phidget21/libraries/"
                  "linux/libphidget/libphidget_"
                  version ".tar.gz"))
            (sha256
             (base32 "07w54dmr75vq2imngfy66nk1sxlvkzhl2p6g362q0a02f099jy0f"))))
   (build-system gnu-build-system)
   (inputs (list libusb))
   (outputs '("out" "debug"))
   (home-page "https://www.phidgets.com/")
   (license lgpl3+)
   (synopsis "C library to manipulate Phidgets")
   (description synopsis)))
