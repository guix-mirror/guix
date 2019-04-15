;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Mike Gerwitz <mtg@gnu.org>
;;; Copyright © 2016 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
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
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages check)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml))

(define-public ccid
  (package
    (name "ccid")
    (version "1.4.30")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://ccid.apdu.fr/files/"
                    name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0z7zafdg75fr1adlv2x0zz34s07gljcjg2lsz76s1048w1xhh5xc"))))
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
     `(("pcsc-lite" ,pcsc-lite)         ; only required for headers
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libusb" ,libusb)))
    (home-page "https://ccid.apdu.fr/")
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
    (version "4.4.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Fedict/eid-mw")
             (commit (string-append "v" version))))
       (sha256
        (base32 "14bgn2k0xbd6241qdghg787pgxy7k9rvcspaf74zwwyibaqknzyx"))))
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
         (add-after 'unpack 'bootstrap
           (lambda _
             ;; configure.ac relies on ‘git --describe’ to get the version.
             ;; Patch it to just return the real version number directly.
             (substitute* "scripts/build-aux/genver.sh"
               (("/bin/sh") (which "sh"))
               (("^(GITDESC=).*" match) (string-append match ,version "\n")))
             (invoke "sh" "./bootstrap.sh"))))))
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
    (version "1.8.25")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://pcsclite.apdu.fr/files/"
                    name "-" version ".tar.bz2"))
              (sha256
               (base32
                "14l7irs1nsh8b036ag4cfy8wryyysch78scz5dw6xxqwqgnpjvfp"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--enable-usbdropdir=/var/lib/pcsc/drivers"
                           "--disable-libsystemd")))
    (native-inputs
     `(("perl" ,perl)                   ; for pod2man
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libudev" ,eudev)))
    (home-page "https://pcsclite.apdu.fr/")
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

(define-public opensc
  (package
    (name "opensc")
    (version "0.19.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/OpenSC/OpenSC/releases/download/"
                    version "/opensc-" version ".tar.gz"))
              (sha256
               (base32
                "09jqzl18z5qfrf4vf2nvbpdm3mphpgfkl3ww1clkaxh2z56hwnic"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; By setting an absolute path here, we arrange for OpenSC to
         ;; successfully dlopen libpcsclite.so.1 by default.  The user can
         ;; still override this if they want to, by specifying a custom OpenSC
         ;; configuration file at runtime.
         (add-after 'unpack 'set-default-libpcsclite.so.1-path
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((libpcsclite (string-append (assoc-ref inputs "pcsc-lite")
                                               "/lib/libpcsclite.so.1")))
               (substitute* "configure"
                 (("DEFAULT_PCSC_PROVIDER=\"libpcsclite\\.so\\.1\"")
                  (string-append
                   "DEFAULT_PCSC_PROVIDER=\"" libpcsclite "\"")))
               #t)))
         (add-before 'check 'disable-broken-test
           (lambda _
             ;; XXX: This test is fixed in git, remove this phase for >= 0.19.
             (substitute* "doc/tools/Makefile"
               (("TESTS = test-manpage.sh") "TESTS = "))
             #t)))))
    (inputs
     `(("readline" ,readline)
       ("openssl" ,openssl)
       ("pcsc-lite" ,pcsc-lite)
       ("ccid" ,ccid)))
    (native-inputs
     `(("libxslt" ,libxslt)
       ("docbook-xsl" ,docbook-xsl)
       ("pkg-config" ,pkg-config)))
    (home-page "https://github.com/OpenSC/OpenSC/wiki")
    (synopsis "Tools and libraries related to smart cards")
    (description
     "OpenSC is a set of software tools and libraries to work with smart
cards, with the focus on smart cards with cryptographic capabilities.  OpenSC
facilitate the use of smart cards in security applications such as
authentication, encryption and digital signatures.  OpenSC implements the PKCS
#15 standard and the PKCS #11 API.")
    (license license:lgpl2.1+)))

(define-public yubico-piv-tool
  (package
    (name "yubico-piv-tool")
    (version "1.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://developers.yubico.com/yubico-piv-tool/Releases/"
                    name "-" version ".tar.gz"))
              (sha256
               (base32
                "10xgdc51xvszkxmsvqnbjs8ixxz7rfnfahh3wn8glllynmszbhwi"))))
    (build-system gnu-build-system)
    (inputs
     `(("gengetopt" ,gengetopt)
       ("perl" ,perl)
       ("pcsc-lite" ,pcsc-lite)
       ("openssl" ,openssl)))
    (native-inputs
     `(("doxygen" ,doxygen)
       ("graphviz" ,graphviz)
       ("help2man" ,help2man)
       ("check" ,check)
       ("texlive-bin" ,texlive-bin)
       ("pkg-config" ,pkg-config)))
    (home-page "https://developers.yubico.com/yubico-piv-tool/")
    (synopsis "Interact with the PIV application on a YubiKey")
    (description
     "The Yubico PIV tool is used for interacting with the Privilege and
Identification Card (PIV) application on a YubiKey.  With it you may generate
keys on the device, import keys and certificates, create certificate requests,
and other operations.  It includes a library and a command-line tool.")
    ;; The file ykcs11/pkcs11.h also declares an additional, very short free
    ;; license for that one file.  Please see it for details.  The vast
    ;; majority of files are licensed under bsd-2.
    (license license:bsd-2)))

(define-public yubikey-personalization
  (package
    (name "yubikey-personalization")
    (version "1.19.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://developers.yubico.com/" name
                    "/Releases/ykpers-" version ".tar.gz"))
              (sha256
               (base32
                "0jhvnavjrpwzmmjcw486df5s48j53njqgyz36yz3dskbaz3kwlfr"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags (list (string-append "--with-udevrulesdir="
                                              (assoc-ref %outputs "out")
                                              "/lib/udev/rules.d"))))
    (inputs
     `(("json-c" ,json-c)
       ("libusb" ,libusb)
       ;; The library "libyubikey" is also known as "yubico-c".
       ("libyubikey" ,libyubikey)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("eudev" ,eudev)))
    (home-page "https://developers.yubico.com/yubikey-personalization/")
    (synopsis "Library and tools to personalize YubiKeys")
    (description
     "The YubiKey Personalization package contains a C library and command
line tools for personalizing YubiKeys.  You can use these to set an AES key,
retrieve a YubiKey's serial number, and so forth.")
    (license license:bsd-2)))
