;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Julien Lepiller <julien@lepiller.eu>
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
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public openbox
  (package
    (name "openbox")
    (version "3.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.icculus.org/openbox/releases/" name "-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0cxgb334zj6aszwiki9g10i56sm18i7w1kw52vdnwgzq27pv93qj"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("imlib2" ,imlib2)
              ("libxml2" ,libxml2)
              ("librsvg" ,librsvg)
              ("libsm" ,libsm)
              ("libxcursor" ,libxcursor)
              ("libxinerama" ,libxinerama)
              ("libxml2" ,libxml2)
              ("libxrandr" ,libxrandr)
              ("libxft" ,libxft)
              ("pango" ,pango)))
    (synopsis "Box style window manager")
    (description
     "Openbox is a highly configurable, next generation window manager with
extensive standars support.  The *box visual style is well known for its
minimalistic appearance.  Openbox uses the *box visual style, while providing
a greater number of options for theme developers than previous *box
implementations.")
    (home-page "http://openbox.org/wiki/Main_Page")
    (license gpl2+)))

;;; openbox.scm ends here
