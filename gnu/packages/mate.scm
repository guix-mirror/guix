;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Fabian Harfert <fhmgufs@web.de>
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

(define-module (gnu packages mate)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome))

(define-public mate-icon-theme
  (package
    (name "mate-icon-theme")
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://pub.mate-desktop.org/releases/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0d91rvl9rw3xl8hmdcbb6xvi880kfmh2ra5chhrjimrjqgl57qkp"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("gtk+" ,gtk+)
       ("icon-naming-utils" ,icon-naming-utils)))
    (home-page "http://mate-desktop.org/")
    (synopsis "The MATE desktop environment icon theme")
    (description
     "This package contains the default icon theme used by the MATE desktop.")
    (license license:lgpl3+)))

(define-public mate-themes
  (package
    (name "mate-themes")
    (version "1.12.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://pub.mate-desktop.org/releases/"
                                  (version-major+minor version) "/"
                                  name "-gtk"
                                  (version-major+minor (package-version gtk+))
                                  "-" version ".tar.xz"))
              (sha256
               (base32
                "0kyrlgs5azzj60gnxx2n9qszcligxn959wr42wr0iqnrpiygk5nf"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)))
    (inputs
     `(("gtk+" ,gtk+-2)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("gtk-engines" ,gtk-engines)
       ("murrine" ,murrine)))
    (home-page "http://mate-desktop.org/")
    (synopsis
     "Official themes for the MATE desktop")
    (description
     "This package includes the standard themes for the MATE desktop, for
example Menta, TraditionalOk, GreenLaguna or BlackMate.")
    (license (list license:lgpl2.1+ license:cc-by-sa3.0 license:gpl3+
                   license:gpl2+))))
