;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
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

(define-module (gnu packages gtk)
  #:use-module ((guix licenses)
                #:renamer (symbol-prefix-proc 'license:))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages pkg-config))

(define-public atk
  (package
   (name "atk")
   (version "2.8.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/atk/2.8/atk-"
                                version ".tar.xz"))
            (sha256
             (base32
              "1x3dd3hg9l1j9dq70xwph13vxdp6a9wbfcnryryf1wr6c8bij9dj"))))
   (build-system gnu-build-system)
   (inputs `(("glib" ,glib)
             ("pkg-config" ,pkg-config)))
   (synopsis "GNOME accessability toolkit")
   (description
    "ATK provides the set of accessibility interfaces that are implemented
by other toolkits and applications. Using the ATK interfaces, accessibility
tools have full access to view and control running applications.")
   (license license:lgpl2.0+)
   (home-page "https://developer.gnome.org/atk/")))
