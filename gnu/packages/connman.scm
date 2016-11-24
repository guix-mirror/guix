;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages connman)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages vpn))

(define-public connman
  (package
    (name "connman")
    (version "1.33")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kernel.org/linux/network/connman/"
                            name "-" version ".tar.xz"))
    (sha256
     (base32
      "187mknq2i907gf8dz0i79359gn1qc9mryvqkcgb280d7dw1ld2dw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--enable-nmcompat"
             ;; "--enable-polkit"
             "--enable-openconnect"
             "--enable-openvpn"
             "--enable-vpnc"
             "--enable-pptp"
             "--enable-l2tp"
             ;; location for daemon state files and internal configuration
             ;; needs to be writeable
             "--localstatedir=/var"
             (string-append
              "--with-dbusconfdir=" (assoc-ref %outputs "out") "/etc")
             (string-append
              "--with-dbusdatadir=" (assoc-ref %outputs "out") "/share"))))
    (native-inputs
     `(("pkg-config", pkg-config)
       ("python" ,python-2)))
    (inputs
     `(("dbus" ,dbus)
       ("glib" ,glib)
       ("gnutls" ,gnutls)
       ("iptables" ,iptables)
       ;; ("polkit" ,polkit) ; pkg-config cannot find polkit.pc
       ("readline" ,readline)
       ;; These inputs are needed for connman to include the interface to
       ;; these technologies so IF they are installed they can be used.
       ;; TODO: add neard, ofono
       ("openconnect" ,openconnect)
       ("openvpn" ,openvpn)
       ("ppp", ppp)
       ("vpnc" ,vpnc)
       ("wpa-supplicant" ,wpa-supplicant)))
    (home-page "https://01.org/connman")
    (synopsis "Connection management daemon")
    (description "Connman provides a daemon for managing Internet connections.
The Connection Manager is designed to be slim and to use as few resources as
possible. It is fully modular system that can be extended through plug-ins.
The plug-in approach allows for easy adaption and modification for various use
cases.  Connman implements DNS resolving and caching, DHCP clients for both
IPv4 and IPv6, link-local IPv4 address handling and tethering (IP connection
sharing) to clients via USB, ethernet, WiFi, cellular and Bluetooth.")
    (license gpl2)))
