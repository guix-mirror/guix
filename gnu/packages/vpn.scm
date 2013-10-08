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

(define-module (gnu packages vpn)
  #:use-module ((guix licenses)
                #:renamer (symbol-prefix-proc 'license:))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module ((gnu packages gettext)
                #:renamer (symbol-prefix-proc 'gnu:))
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages openssl)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xml))

(define-public vpnc
  (package
   (name "vpnc")
   (version "0.5.3")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://www.unix-ag.uni-kl.de/~massar/vpnc/vpnc-"
                                version ".tar.gz"))
            (sha256 (base32
                     "1128860lis89g1s21hqxvap2nq426c9j4bvgghncc1zj0ays7kj6"))
            (patches (list (search-patch "vpnc-script.patch")))))
   (build-system gnu-build-system)
   (inputs `(("libgcrypt" ,libgcrypt)
             ("perl" ,perl)))
   (arguments
    `(#:tests? #f ; there is no check target
      #:phases
      (alist-replace
       'configure
       (lambda* (#:key outputs #:allow-other-keys)
         (let ((out (assoc-ref outputs "out")))
           (substitute* "Makefile"
             (("PREFIX=/usr/local") (string-append "PREFIX=" out)))
           (substitute* "Makefile"
             (("ETCDIR=/etc/vpnc") (string-append "ETCDIR=" out "/etc/vpnc")))))
       %standard-phases)))
   (synopsis "vpnc, a client for cisco vpn concentrators")
   (description
    "vpnc is a VPN client compatible with Cisco's EasyVPN equipment.
It supports IPSec (ESP) with Mode Configuration and Xauth. It supports only
shared-secret IPSec authentication with Xauth, AES (256, 192, 128), 3DES,
1DES, MD5, SHA1, DH1/2/5 and IP tunneling. It runs entirely in userspace.
Only \"Universal TUN/TAP device driver support\" is needed in the kernel.")
   (license license:gpl2+) ; some file are bsd-2, see COPYING
   (home-page "http://www.unix-ag.uni-kl.de/~massar/vpnc/")))


(define-public openconnect
  (package
   (name "openconnect")
   (version "4.99")
   (source (origin
            (method url-fetch)
            (uri (string-append "ftp://ftp.infradead.org/pub/openconnect/openconnect-"
                                version ".tar.gz"))
            (sha256 (base32
                     "1rd8pap455wzkx19i0sy3cqap524b6fwcjvqynxp6lhm01di4bd6"))))
   (build-system gnu-build-system)
   (inputs
    `(("gettext" ,gnu:gettext)
      ("libxml2" ,libxml2)
      ("openssl" ,openssl)
      ("pkg-config" ,pkg-config)
      ("vpnc" ,vpnc)
      ("zlib" ,zlib)))
   (arguments
    `(#:phases
      (alist-replace
       'configure
       (lambda* (#:key inputs #:allow-other-keys #:rest args)
         (let ((vpnc (assoc-ref inputs "vpnc"))
               (configure (assoc-ref %standard-phases 'configure)))
           (apply configure
                  (append args
                          (list '#:configure-flags
                                (list (string-append "--with-vpnc-script="
                                                     vpnc
                                                     "/etc/vpnc/vpnc-script")))))))
       %standard-phases)))
   (synopsis "client for cisco vpn")
   (description
    "OpenConnect is a client for Cisco's AnyConnect SSL VPN, which is
supported by the ASA5500 Series, by IOS 12.4(9)T or later on Cisco SR500,
870, 880, 1800, 2800, 3800, 7200 Series and Cisco 7301 Routers,
and probably others.")
   (license license:lgpl2.1)
   (home-page "http://www.infradead.org/openconnect/")))
