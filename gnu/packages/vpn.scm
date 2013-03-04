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
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages perl))

(define-public vpnc
  (package
   (name "vpnc")
   (version "0.5.3")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://www.unix-ag.uni-kl.de/~massar/vpnc/vpnc-"
                                version ".tar.gz"))
            (sha256 (base32
                     "1128860lis89g1s21hqxvap2nq426c9j4bvgghncc1zj0ays7kj6"))))
   (build-system gnu-build-system)
   (inputs `(("libgcrypt" ,libgcrypt)
             ("perl" ,perl)
             ("patch/script"
                 ,(search-patch "vpnc-script.patch"))))
   (arguments
    `(#:tests? #f ; there is no check target
      #:patches (list (assoc-ref %build-inputs
                                 "patch/script"))
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
