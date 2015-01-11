;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Andreas Enge <andreas@enge.fr>
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

(define-module (gnu packages polkit)
  #:use-module ((guix licenses) #:select (lgpl2.0+))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xml))

(define-public polkit
  (package
    (name "polkit")
    (version "0.112")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://www.freedesktop.org/software/polkit/releases/"
                   name "-" version ".tar.gz"))
             (sha256
              (base32
               "1xkary7yirdcjdva950nqyhmsz48qhrdsr78zciahj27p8yg95fn"))
             (patches (list (search-patch "polkit-drop-test.patch")))))
    (build-system gnu-build-system)
    (inputs
      `(("expat" ,expat)
        ("glib:bin" ,glib "bin") ; for glib-mkenums
        ("intltool" ,intltool)
        ("linux-pam" ,linux-pam)
        ("mozjs" ,mozjs)
        ("nspr" ,nspr)))
    (propagated-inputs
      `(("glib" ,glib))) ; required by polkit-gobject-1.pc
    (native-inputs
      `(("pkg-config", pkg-config)))
    (home-page "http://www.freedesktop.org/wiki/Software/polkit/")
    (synopsis "Authorization API for privilege management")
    (description "Polkit is an application-level toolkit for defining and
handling the policy that allows unprivileged processes to speak to
privileged processes.  It is a framework for centralizing the decision
making process with respect to granting access to privileged operations
for unprivileged applications.")
    (license lgpl2.0+)))

(define-public polkit-qt
  (package
    (name "polkit-qt")
    (version "1-0.112.0")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://download.kde.org/stable/apps/KDE4.x/admin/"
                   name "-" version ".tar.bz2"))
             (sha256
              (base32
               "1ip78x20hjqvm08kxhp6gb8hf6k5n6sxyx6kk2yvvq53djzh7yv7"))))
    (build-system cmake-build-system)
    (inputs
      `(("polkit" ,polkit)))
    (propagated-inputs
      `(("qt" ,qt-4))) ; according to the pkg-config files
    (native-inputs
      `(("pkg-config", pkg-config)))
    (arguments
      `(#:tests? #f)) ; there is a test subdirectory, but no test target
    (home-page "http://api.kde.org/kdesupport-api/polkit-qt-1-apidocs/")
    (synopsis "Qt frontend to the polkit library")
    (description "Polkit-qt is a library that lets developers use the
PolicyKit API through a Qt-styled API.  It is mainly a wrapper around
QAction and QAbstractButton that lets you integrate those two component
easily with PolicyKit.")
    (license lgpl2.0+)))
