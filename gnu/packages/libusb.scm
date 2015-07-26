;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
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

(define-module (gnu packages libusb)
  #:use-module (gnu packages)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config))

(define-public libusb
  (package
    (name "libusb")
    (version "1.0.19")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/libusb/libusb-1.0/"
                          "libusb-" version "/libusb-" version ".tar.bz2"))
      (sha256
       (base32
        "0h38p9rxfpg9vkrbyb120i1diq57qcln82h5fr7hvy82c20jql3c"))))
    (build-system gnu-build-system)

    ;; XXX: Enabling udev is now recommended, but eudev indirectly depends on
    ;; libusb.
    (arguments `(#:configure-flags '("--disable-udev")))
    ;; (inputs `(("eudev" ,eudev)))

    (home-page "http://www.libusb.org")
    (synopsis "User-space USB library")
    (description
     "Libusb is a library that gives applications easy access to USB
devices on various operating systems.")
    (license lgpl2.1+)))

(define-public libmtp
  (package
    (name "libmtp")
    (version "1.1.8")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/libmtp/" version
                                 "/libmtp-" version ".tar.gz"))
             (sha256
               (base32
                "10i2vnj8r6hyd61xgyhmxbsissq971g50fhm1h6mc3m4d99qg7iz"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (propagated-inputs
     ;; libmtp.pc refers to all these.
     `(("libgcrypt" ,libgcrypt)
       ("libusb" ,libusb)))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-udev="
                            (assoc-ref %outputs "out")
                            "/lib/udev"))))
    (home-page "http://libmtp.sourceforge.net/")
    (synopsis "Library implementing the Media Transfer Protocol")
    (description "Libmtp implements an MTP (Media Transfer Protocol)
initiator, which means that it initiates MTP sessions with devices.  The
devices responding are known as MTP responders.  Libmtp runs on devices
with a USB host controller interface.  It implements MTP Basic, which was
proposed for standardization.")
    ;; COPYING contains lgpl2.1, while files headers give
    ;; "GNU Lesser General Public License as published by the Free Software
    ;; Foundation; either version 2 of the License, or (at your option) any
    ;; later version."
    (license lgpl2.1+)))
