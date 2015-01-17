;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
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

(define-module (gnu packages fvwm)
  #:use-module ((guix licenses) #:select (gpl2+))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages image)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public fvwm
  (package
    (name "fvwm")
    (version "2.6.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "ftp://ftp.fvwm.org/pub/fvwm/version-2/fvwm-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "1ks8igvmzm0m0sra05k8xzc8vkqy3gv1qskl6davw1irqnarjm11"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("xsltproc" ,libxslt)))
    (inputs
     `(("fribidi" ,fribidi)
       ("libpng" ,libpng)
       ("librsvg" ,librsvg)
       ("libxcursor" ,libxcursor)
       ("libxext" ,libxext)
       ("libxft" ,libxft)
       ("libxinerama" ,libxinerama)
       ("libxpm" ,libxpm)
       ("libxt" ,libxt)
       ("readline" ,readline)))
    (synopsis "Virtual window manager for X11")
    (description
     "FVWM is an extremely powerful ICCCM-compliant multiple virtual desktop
window manager for the X Window system.")
    (home-page "http://www.fvwm.org/")
    (license gpl2+)))
