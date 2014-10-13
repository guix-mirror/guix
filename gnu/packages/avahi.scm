;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
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

(define-module (gnu packages avahi)
  #:use-module ((guix licenses) #:select (lgpl2.1+))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages gdbm)
  #:use-module (gnu packages libdaemon)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages xml))

(define-public avahi
  (package
    (name "avahi")
    (version "0.6.31")
    (home-page "http://avahi.org")
    (source (origin
             (method url-fetch)
             (uri (string-append home-page "/download/avahi-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0j5b5ld6bjyh3qhd2nw0jb84znq0wqai7fsrdzg7bpg24jdp2wl3"))
             (patches (list (search-patch "avahi-localstatedir.patch")))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--with-distro=none"
                           "--localstatedir=/var" ; for the DBus socket
                           "--disable-python"
                           "--disable-mono"
                           "--disable-doxygen-doc"
                           "--disable-xmltoman"
                           "--enable-tests"
                           "--disable-qt3" "--disable-qt4"
                           "--disable-gtk" "--disable-gtk3")))
    (inputs
     `(("expat" ,expat)
       ("glib" ,glib)
       ("dbus" ,dbus)
       ("gdbm" ,gdbm)
       ("libdaemon" ,libdaemon)))
    (native-inputs
     `(("intltool" ,intltool)
       ("glib" ,glib "bin")
       ("pkg-config" ,pkg-config)))
    (synopsis "Avahi, an mDNS/DNS-SD implementation")
    (description
     "Avahi is a system which facilitates service discovery on a local
network.  It is an implementation of the mDNS (for \"Multicast DNS\") and
DNS-SD (for \"DNS-Based Service Discovery\") protocols.")
    (license lgpl2.1+)))

(define-public nss-mdns
  (package
    (name "nss-mdns")
    (version "0.10")
    (source (origin
              (method url-fetch)
              (uri (list
                    (string-append
                     "mirror://debian/pool/main/n/nss-mdns/nss-mdns_"
                     version ".orig.tar.gz")
                    "http://pkgs.fedoraproject.org/repo/pkgs/nss-mdns/nss-mdns-0.10.tar.gz/03938f17646efbb50aa70ba5f99f51d7/nss-mdns-0.10.tar.gz"

                    ;; This used to be the canonical URL but it vanished.
                    ;; See <http://bugs.gnu.org/18704>.
                    ;; (string-append
                    ;;  "http://0pointer.de/lennart/projects/nss-mdns/nss-mdns-"
                    ;;  version ".tar.gz")
                    ))
              (sha256
               (base32
                "0vgs6j0qsl0mwzh5a0m0bykr7x6bx79vnbyn0r3q289rghp3qs0y"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system gnu-build-system)

    ;; XXX: Stale URL, missing replacement.  See <http://bugs.gnu.org/18704>.
    (home-page "http://0pointer.de/lennart/projects/nss-mdns/")

    (synopsis "The mDNS Name Service Switch (NSS) plug-in")
    (description
     "'nss-mdns' is a plug-in for the Name Service Switch (NSS) functionality
of the GNU C Library, providing host name resolution via Multicast DNS (mDNS).
It allows for name resolution by programs in the ad-hoc mDNS domain
'.local'.")
    (license lgpl2.1+)))
