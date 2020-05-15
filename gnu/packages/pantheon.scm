;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>
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

(define-module (gnu packages pantheon)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages)
  #:use-module (guix build-system meson)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) :prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public granite
  (package
    (name "granite")
    (version "5.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/elementary/granite.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0acicv3f9gksb352v88lwap8ailjsxdrfknl2xql7blasbjzl2q0"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-icon-cache
           (lambda _
             (setenv "DESTDIR" "/")
             #t)))))
    (inputs
     `(("gtk" ,gtk+)))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib" ,glib)
       ("gobject-introspection" ,gobject-introspection)
       ("libgee" ,libgee)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (home-page "https://github.com/elementary/granite")
    (synopsis "Library that extends GTK with common widgets and utilities")
    (description "Granite is a companion library for GTK+ and GLib.  Among other
things, it provides complex widgets and convenience functions designed for use
in apps built for the Pantheon desktop.")
    (license license:lgpl3+)))
