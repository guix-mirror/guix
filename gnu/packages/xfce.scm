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
