;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Andreas Enge <andreas@enge.fr>
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

(define-module (gnu packages kde)
  #:use-module ((guix licenses) #:select (bsd-2 lgpl2.0+ lgpl2.1 lgpl2.1+))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xorg))

(define-public automoc4
  (package
    (name "automoc4")
    (version "0.9.88")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://download.kde.org/stable/" name
                                "/" version "/" name "-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "0jackvg0bdjg797qlbbyf9syylm0qjs55mllhn11vqjsq3s1ch93"))))
    (build-system cmake-build-system)
    (inputs
     `(("qt" ,qt-4)))
    (arguments
     `(#:tests? #f)) ; no check target
    (home-page "http://techbase.kde.org/Development/Tools/Automoc4")
    (synopsis "build tool for KDE")
    (description "KDE desktop environment")
    (license bsd-2)))

(define-public phonon
  (package
    (name "phonon")
    (version "4.7.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://download.kde.org/stable/" name
                                "/" version "/"
                                name "-" version ".tar.xz"))
             (sha256
              (base32
               "1sxrnwm16dxy32xmrqf26762wmbqing1zx8i4vlvzgzvd9xy39ac"))))
    (build-system cmake-build-system)
    ;; FIXME: Add interpreter ruby once available.
    ;; Add optional input libqtzeitgeist.
    (inputs
     `(("automoc4" ,automoc4)
       ("glib" ,glib)
       ("libx11" ,libx11)
       ("pulseaudio" ,pulseaudio)
       ("qt" ,qt-4)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (arguments
     `(#:tests? #f)) ; no test target
    (home-page "http://phonon.kde.org/")
    (synopsis "Qt 4 multimedia API")
    (description "KDE desktop environment")
    (license lgpl2.1+)))

(define-public qjson
  (package
    (name "qjson")
    (version "0.8.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/flavio/qjson/archive/"
                                 version ".tar.gz"))
             (sha256
              (base32
               "163fspi0xc705irv79qw861fmh68pjyla9vx3kqiq6xrdhb9834j"))))
    (build-system cmake-build-system)
    (inputs
     `(("qt" ,qt-4)))
    (arguments
     `(#:tests? #f)) ; no test target
    (home-page "http://qjson.sourceforge.net/")
    (synopsis "Qt-based library for handling JSON")
    (description "QJson is a Qt-based library that maps JSON data to QVariant
objects and vice versa.  JSON arrays are mapped to QVariantList instances,
while JSON objects are mapped to QVariantMap.")
    (license lgpl2.1+)))

(define-public libdbusmenu-qt
  (package
    (name "libdbusmenu-qt")
    (version "0.9.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://launchpad.net/" name "/trunk/"
                                 version "/+download/"
                                 name "-" version ".tar.bz2"))
             (sha256
              (base32
               "1v0ri5g9xw2z64ik0kx0ra01v8rpjn2kxprrxppkls1wvav1qv5f"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("qjson", qjson)
       ("qt" ,qt-4)))
    (arguments
     `(#:tests? #f ; no check target
       #:configure-flags
        '("-DWITH_DOC=OFF"))) ; FIXME: drop once input doxygen is available
    (home-page "https://launchpad.net/libdbusmenu-qt/")
    (synopsis "Qt implementation of the DBusMenu protocol")
    (description "The library provides a Qt implementation of the DBusMenu
protocol.  The DBusMenu protocol makes it possible for applications to export
and import their menus over DBus.")
    (license lgpl2.0+)))

(define-public attica
  (package
    (name "attica")
    (version "0.4.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://download.kde.org/stable/"
                                 name "/"
                                 name "-" version ".tar.bz2"))
             (sha256
              (base32
               "1y74gsyzi70dfr9d1f1b08k130rm3jaibsppg8dv5h3211vm771v"))))
    (build-system cmake-build-system)
    (inputs
     `(("qt" ,qt-4)))
    (home-page "https://projects.kde.org/projects/kdesupport/attica")
    (synopsis "Qt library for the Open Collaboration Services API")
    (description "Attica is a Qt library that implements the Open
Collaboration Services API version 1.6.  It grants easy access to the
services such as querying information about persons and contents.  The
library is used in KNewStuff3 as content provider.  In order to integrate
with KDE's Plasma Desktop, a platform plugin exists in kdebase.")
    (license lgpl2.1+)))
