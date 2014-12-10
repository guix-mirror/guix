;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Sou Bunnbu <iyzsong@gmail.com>
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

(define-module (gnu packages xfce)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk))

(define-public gtk-xfce-engine
  (package
    (name "gtk-xfce-engine")
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://archive.xfce.org/xfce/4.10/src/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "13c3ajfqkdr6jlqjyhcp4nls0ddanypr83q9qib2ciffik78zq4h"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (inputs `(("gtk+" ,gtk+-2)))
    (home-page "http://www.xfce.org/")
    (synopsis "GTK+ theme engine for Xfce")
    (description
     "Default GTK+ engine and themes for Xfce Desktop Environment.")
    (license gpl2+)))

(define-public libxfce4util
  (package
    (name "libxfce4util")
    (version "4.10.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://archive.xfce.org/xfce/"
                                  (version-major+minor version)
                                  "/src/" name "-" version ".tar.bz2"))
              (sha256
               (base32
                "13k0wwbbqvdmbj4xmk4nxdlgvrdgr5y6r3dk380mzfw053hzwy89"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (propagated-inputs `(("glib" ,glib))) ; required by libxfce4util-1.0.pc
    (home-page "http://www.xfce.org/")
    (synopsis "Basic utility library for Xfce")
    (description
     "A general-purpose utility library with core application support for the
Xfce Desktop Environment.")
    (license lgpl2.0+)))

(define-public xfconf
  (package
    (name "xfconf")
    (version "4.10.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://archive.xfce.org/xfce/"
                                  (version-major+minor version)
                                  "/src/" name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0xh520z0qh0ib0ijgnyrgii9h5d4pc53n6mx1chhyzfc86j1jlhp"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (propagated-inputs
     ;; libxfconf-0.pc refers to all these.
     `(("glib" ,glib)
       ("dbus" ,dbus)
       ("dbus-glib" ,dbus-glib)))
    (inputs
     `(("libxfce4util" ,libxfce4util)))
    (home-page "http://www.xfce.org/")
    (synopsis "Configuration storage and query system for Xfce")
    (description
     "Settings daemon for Xfce, implemented as a D-Bus-based configuration
storage system.")
    (license lgpl2.0+)))
