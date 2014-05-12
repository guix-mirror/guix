;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages gimp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses)
                #:renamer (symbol-prefix-proc 'license:))
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages libpng)
  #:use-module (gnu packages libjpeg)
  #:use-module ((gnu packages ghostscript)
                #:select (lcms))
  #:use-module (gnu packages compression)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages imagemagick))

(define-public babl
  (package
    (name "babl")
    (version "0.1.10")
    (source (origin
              (method url-fetch)
              (uri (list (string-append "http://ftp.gtk.org/pub/babl/0.1/babl-"
                                        version ".tar.bz2")
                         (string-append "ftp://ftp.gtk.org/pub/babl/0.1/babl-"
                                        version ".tar.bz2")))
              (sha256
               (base32
                "1x2mb7zfbvk9d0a7h5cpdff9hhjsadxvqml2jay2bpf7x9nc6gwl"))))
    (build-system gnu-build-system)
    (home-page "http://gegl.org/babl/")
    (synopsis "Image pixel format conversion library")
    (description
     "babl is a dynamic, any to any, pixel format translation library.
It allows converting between different methods of storing pixels known as
pixel formats that have with different bitdepths and other data
representations, color models and component permutations.

A vocabulary to formulate new pixel formats from existing primitives is
provided as well as the framework to add new color models and data types.")
    (license license:lgpl3+)))
