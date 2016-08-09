;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Julien Lepiller <julien@lepiller.eu>
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

(define-module (gnu packages openbox)
  #:use-module ((guix licenses) #:select (gpl2+))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public openbox
  (package
    (name "openbox")
    (version "3.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://openbox.org/dist/openbox/" name "-"
                    version ".tar.xz"))
              (sha256
               (base32
                "0vg2y1qddsdxkjv806mzpvmkgzliab8ll4s7zm7ma5jnriamirxb"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (propagated-inputs `(("python2-pyxdg" ,python2-pyxdg)))
    (inputs `(("imlib2" ,imlib2)
              ("libxml2" ,libxml2)
              ("librsvg" ,librsvg)
              ("libsm" ,libsm)
              ("libxcursor" ,libxcursor)
              ("libxinerama" ,libxinerama)
              ("libxml2" ,libxml2)
              ("libxrandr" ,libxrandr)
              ("libxft" ,libxft)
              ("pango" ,pango)
              ("python-2" ,python-2)))
    (synopsis "Box style window manager")
    (description
     "Openbox is a highly configurable, next generation window manager with
extensive standards support.  The *box visual style is well known for its
minimalistic appearance.  Openbox uses the *box visual style, while providing
a greater number of options for theme developers than previous *box
implementations.")
    (home-page "http://openbox.org/wiki/Main_Page")
    (license gpl2+)))

;;; openbox.scm ends here
