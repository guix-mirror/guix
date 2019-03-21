;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 ng0 <ng0@n0.is>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages cinnamon)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xorg))

(define-public cinnamon-desktop
  (package
    (name "cinnamon-desktop")
    (version "3.4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/linuxmint/cinnamon-desktop/"
                                  "archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1jf24csrbfi9aiza1g70jpvsbjiqwphk0i5wilxq9kpjjsl99maq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'bootstrap 'skip-premature-configure
           (lambda _
             (setenv "NOCONFIGURE" "set")
             #t)))))
    ;; TODO: package 'libgsystem'.
    (inputs
     `(("accountsservice" ,accountsservice)
       ("gtk+" ,gtk+)
       ("glib" ,glib)
       ("gobject-introspection" ,gobject-introspection)
       ("gnome-common" ,gnome-common)
       ("libxkbfile" ,libxkbfile)
       ("libxrandr" ,libxrandr)
       ("python-2" ,python-2)
       ("pulseaudio" ,pulseaudio)
       ("xkeyboard-config" ,xkeyboard-config)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext" ,gettext-minimal)
       ("glib" ,glib "bin") ; glib-gettextize
       ("intltool" ,intltool)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (home-page "https://github.com/linuxmint/cinnamon-desktop/")
    (synopsis "Library for the Cinnamon Desktop")
    (description
     "The cinnamon-desktop package contains the libcinnamon-desktop library,
as well as some desktop-wide documents.")
    (license (list license:gpl2+ license:lgpl2.0+
                   license:expat)))) ;display-name.c , edid-parse.c
