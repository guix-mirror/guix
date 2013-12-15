;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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
       ("pkg-config" ,pkg-config)))
    (synopsis "Avahi, an mDNS/DNS-SD implementation")
    (description
     "Avahi is a system which facilitates service discovery on a local
network.  It is an implementation of the mDNS (for \"Multicast DNS\") and
DNS-SD (for \"DNS-Based Service Discovery\") protocols.")
    (license lgpl2.1+)))
