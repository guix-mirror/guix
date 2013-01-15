;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
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

(define-module (distro packages glib)
  #:use-module ((guix licenses) #:select (lgpl2.0+))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (distro packages base)
  #:use-module (distro packages compression)
  #:use-module ((distro packages gettext)
                #:renamer (symbol-prefix-proc 'guix:))
  #:use-module (distro packages libffi)
  #:use-module (distro packages pkg-config)
  #:use-module (distro packages python))

(define-public glib
  (package
   (name "glib")
   (version "2.34")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://ftp.gnome.org/pub/gnome/sources/"
                                name "/" version "/"
                                name "-" version ".3" ".tar.xz"))
            (sha256
             (base32 "19sq4rhl2vr8ikjvl8qh51vr38yqfhbkb3imi2s6ac5rgkwcnpw5"))))
   (build-system gnu-build-system)
   (inputs
    `(("coreutils" ,coreutils)
      ("gettext" ,guix:gettext)
      ("libffi" ,libffi)
      ("pkg-config" ,pkg-config)
      ("python" ,python)
      ("zlib" ,zlib)))
   (arguments `(#:tests? #f)) ; XXX: tests fail
   (synopsis
    "GLib, a library that provides the core application building blocks")
   (description
    "GLib provides data structure handling for C, portability wrappers,
and interfaces for such runtime functionality as an event loop, threads,
dynamic loading, and an object system.")
   (home-page "http://developer.gnome.org/glib/")
   (license lgpl2.0+))) ; some files are under lgpl2.1+