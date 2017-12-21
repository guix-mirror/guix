;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Mike Gerwitz <mtg@gnu.org>
;;; Copyright © 2016 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages security-token)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xml))

(define-public ccid
  (package
    (name "ccid")
    (version "1.4.28")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://alioth.debian.org/frs/download.php/file/4230/"
                    "ccid-" version ".tar.bz2"))
              (sha256
               (base32
                "1q5dz1l049m3hmr370adhrqicdkldrr3l98svi02p5cxbnn3cn47"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list (string-append "--enable-usbdropdir=" %output
                                              "/pcsc/drivers"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-Makefile
           (lambda _
             (substitute* "src/Makefile.in"
               (("/bin/echo") (which "echo")))
             #t)))))
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libusb" ,libusb)
       ("pcsc-lite" ,pcsc-lite)))
    (home-page "https://pcsclite.alioth.debian.org/ccid.html")
    (synopsis "PC/SC driver for USB smart card devices")
    (description
     "This package provides a PC/SC IFD handler implementation for devices
compliant with the CCID and ICCD protocols.  It supports a wide range of
readers and is needed to communicate with such devices through the
@command{pcscd} resource manager.")
    (license license:lgpl2.1+)))

(define-public eid-mw
  (package
    (name "eid-mw")
    (version "4.3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/Fedict/eid-mw/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ay9znry9dkhhn783paqy8czvv3w5gdpmq8ag8znx9akza8c929z"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext" ,gnu-gettext)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("perl" ,perl)))
    (inputs
     `(("curl" ,curl)
       ("openssl" ,openssl)
       ("gtk+" ,gtk+)
       ("pcsc-lite" ,pcsc-lite)
       ("p11-kit" ,p11-kit)
       ("libproxy" ,libproxy)
       ("libxml2" ,libxml2)
       ("cyrus-sasl" ,cyrus-sasl)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; The github tarball doesn't contain a configure script.
         (add-before 'configure 'autoreconf
           (lambda _ (zero? (system* "autoreconf" "-i")))))))
    (synopsis "Belgian eID Middleware")
    (description "The Belgian eID Middleware is required to authenticate with
online services using the Belgian electronic identity card.")
    (home-page "https://github.com/Fedict/eid-mw")
    (license license:lgpl3)))

(define-public libyubikey
  (package
    (name "libyubikey")
    (version "1.13")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://developers.yubico.com/yubico-c/Releases/"
                    name "-" version ".tar.gz"))
              (sha256
               (base32
                "009l3k2zyn06dbrlja2d4p2vfnzjhlcqxi88v02mlrnb17mx1v84"))))
    (build-system gnu-build-system)
    (synopsis "Development kit for the YubiKey authentication device")
    (description
     "This package contains a C library and command-line tools that make up
the low-level development kit for the Yubico YubiKey authentication device.")
    (home-page "https://developers.yubico.com/yubico-c/")
    (license license:bsd-2)))

(define-public pcsc-lite
  (package
    (name "pcsc-lite")
    (version "1.8.23")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://alioth.debian.org/frs/download.php/file/4235/"
                    "pcsc-lite-" version ".tar.bz2"))
              (sha256
               (base32
                "1jc9ws5ra6v3plwraqixin0w0wfxj64drahrbkyrrwzghqjjc9ss"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--enable-usbdropdir=/var/lib/pcsc/drivers"
                           "--disable-libsystemd")))
    (native-inputs
     `(("perl" ,perl)                   ; for pod2man
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libudev" ,eudev)))
    (home-page "https://pcsclite.alioth.debian.org/pcsclite.html")
    (synopsis "Middleware to access a smart card using PC/SC")
    (description
     "pcsc-lite provides an interface to communicate with smartcards and
readers using the SCard API.  pcsc-lite is used to connect to the PC/SC daemon
from a client application and provide access to the desired reader.")
    (license (list license:bsd-3                ; pcsc-lite
                   license:isc                  ; src/strlcat.c src/strlcpy.c
                   license:gpl3+))))            ; src/spy/*

(define-public ykclient
  (package
    (name "ykclient")
    (version "2.15")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://developers.yubico.com/yubico-c-client/Releases/"
                    name "-" version ".tar.gz"))
              (sha256
               (base32
                "05jhx9waj3pl120ddnwap1v3bjrnbfhvf3lxs2xmhpcmwzpwsqgl"))))
    (build-system gnu-build-system)

    ;; There's just one test, and it requires network access to access
    ;; yubico.com, so skip it.
    (arguments '(#:tests? #f))

    (native-inputs `(("pkg-config" ,pkg-config)
                     ("help2man" ,help2man)))
    (inputs `(("curl" ,curl)))
    (synopsis "C library to validate one-time-password YubiKeys")
    (description
     "YubiKey C Client Library (libykclient) is a C library used to validate a
one-time-password (OTP) YubiKey against Yubico’s servers.  See the Yubico
website for more information about Yubico and the YubiKey.")
    (home-page "https://developers.yubico.com/yubico-c-client/")
    (license license:bsd-2)))
