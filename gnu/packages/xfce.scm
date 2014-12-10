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
  #:use-module ((guix licenses) #:hide (freetype))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages web)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages image)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages gstreamer))

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

(define-public libxfce4ui
  (package
    (name "libxfce4ui")
    (version "4.10.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://archive.xfce.org/xfce/"
                                  (version-major+minor version)
                                  "/src/" name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1qm31s6568cz4c8rl9fsfq0xmf7pldxm0ki62gx1cpybihlgmfd2"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (propagated-inputs
     ;; libxfce4kbd-private-2.pc refers to all these.
     `(("gtk+" ,gtk+-2)
       ("libxfce4util" ,libxfce4util)
       ("xfconf" ,xfconf)))
    (inputs `(("libsm" ,libsm)
              ("libice" ,libice)
              ("startup-notification" ,startup-notification)))
    (home-page "http://www.xfce.org/")
    (synopsis "Widgets library for Xfce")
    (description
     "Libxfce4ui is the replacement of the old libxfcegui4 library.  It is used
to share commonly used Xfce widgets amoung the Xfce applications.")
    (license lgpl2.0+)))

(define-public exo
  (package
    (name "exo")
    (version "0.8.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://archive.xfce.org/xfce/4.10/src/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1c05pbagw14djv5zmqg34qfj40jav8sd10w2zi2wpzrad4qal8bf"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (propagated-inputs
     ;; exo-1.pc refers to all these.
     `(("gtk+" ,gtk+-2)
       ("libxfce4util" ,libxfce4util)))
    (inputs
     `(("libxfce4ui" ,libxfce4ui)
       ("perl-uri" ,perl-uri)))
    (home-page "http://www.xfce.org/")
    (synopsis "Extension library for Xfce")
    (description
     "An extension library to Xfce.  While Xfce comes with quite a few libraries
that are targeted at desktop development, libexo is targeted at application
development.")
    ;; Libraries are under LGPLv2+, and programs under GPLv2+.
    (license (list gpl2+ lgpl2.1+))))

(define-public garcon
  (package
    (name "garcon")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://archive.xfce.org/xfce/4.10/src/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0v7pkvxcayi86z4f173z5l7w270f3g369sa88z59w0y0p7ns7ph2"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("glib:bin" ,glib "bin")))
    (propagated-inputs `(("glib" ,glib))) ; required by garcon-1.pc
    (inputs `(("libxfce4util" ,libxfce4util)))
    (home-page "http://www.xfce.org/")
    (synopsis "Implementation of the freedesktop.org menu specification")
    (description
     "Garcon is a freedesktop.org compliant menu implementation based on
GLib and GIO.  It was started as a complete rewrite of the former Xfce menu
library called libxfce4menu, which, in contrast to garcon, was lacking menu
merging features essential for loading menus modified with menu editors.")
    (license lgpl2.0+)))

(define-public tumbler
  (package
    (name "tumbler")
    (version "0.1.25")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://archive.xfce.org/xfce/4.10/src/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0ijm04vm75gmhyyzrlqdr6vzchr01hlajcm84lm6j64cim8dxm82"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("glib:bin" ,glib "bin") ; need glib-genmarshal
       ("dbus-glib" ,dbus-glib))) ; need dbus-binding-tool
    (propagated-inputs
     `(("glib" ,glib))) ; required by tumbler-1.pc
    (inputs
     `(("dbus" ,dbus)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("freetype" ,freetype)
       ("libjpeg" ,libjpeg)
       ("libgsf" ,libgsf)
       ("poppler" ,poppler)
       ("gstreamer" ,gstreamer-0.10)))
    (home-page "http://www.xfce.org/")
    (synopsis "D-Bus service for applications to request thumbnails")
    (description
     "Tumbler is a D-Bus service for applications to request thumbnails for
various URI schemes and MIME types.  It is an implementation of the thumbnail
management D-Bus specification.")
    (license gpl2+)))

(define-public xfce4-panel
  (package
    (name "xfce4-panel")
    (version "4.10.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://archive.xfce.org/xfce/"
                                  (version-major+minor version)
                                  "/src/" name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1f8903nx6ivzircl8d8s9zna4vjgfy0qhjk5d2x19g9bmycgj89k"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (propagated-inputs
     `(("libxfce4util" ,libxfce4util))) ; required by libxfce4panel-1.0.pc
    (inputs
     `(("exo" ,exo)
       ("garcon", garcon)
       ("libwnck" ,libwnck-1)
       ("libxfce4ui" ,libxfce4ui)))
    (home-page "http://www.xfce.org/")
    (synopsis "Xfce desktop panel")
    (description
     "Desktop panel for Xfce, which contains program launchers, window buttons,
applications menu, workspace switcher and more.")
    ;; Libraries are under LGPLv2.1+, and programs under GPLv2+.
    (license (list gpl2+ lgpl2.1+))))

(define-public xfce4-appfinder
  (package
    (name "xfce4-appfinder")
    (version "4.10.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://archive.xfce.org/xfce/"
                                  (version-major+minor version)
                                  "/src/" name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0falckrziw8m1a72nxd7fqq84r3xfbrb6lv35flsca346rzawah4"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (inputs
     `(("garcon" ,garcon)
       ("libxfce4ui" ,libxfce4ui)))
    (home-page "http://www.xfce.org/")
    (synopsis "Xfce application finder")
    (description
     "Application finder for Xfce, it will show the applications installed on
your system in categories, so you can quickly find and launch them.")
    (license gpl2+)))

(define-public xfce4-session
  (package
    (name "xfce4-session")
    (version "4.10.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://archive.xfce.org/xfce/"
                                  (version-major+minor version)
                                  "/src/" name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1kj65jkjhd0ysf0yxsf88wzpyv6n8i8qgd3gb502hf1x9jksk2mv"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list (string-append "--with-xsession-prefix=" %output))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (inputs
     `(("iceauth" ,iceauth)
       ("libsm" ,libsm)
       ("libwnck" ,libwnck-1)
       ("libxfce4ui" ,libxfce4ui)))
    (home-page "http://www.xfce.org/")
    (synopsis "Xfce session manager")
    (description
     "Session manager for Xfce, it will restore your session on startup and
allows you to shutdown the computer from Xfce.")
    (license gpl2+)))
