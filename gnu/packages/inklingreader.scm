;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Roel Janssen <roel@gnu.org>
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

(define-module (gnu packages inklingreader)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages pkg-config))

(define-public inklingreader
  (package
    (name "inklingreader")
    (version "0.8")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "ftp://alpha.gnu.org/gnu/inklingreader/inklingreader-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0ikg95mgwfqh0bq4dzvkfmdiycacqvv27g91hl6adwk5y3gzl96g"))))
    (build-system gnu-build-system)
    (inputs
     (list glib cairo gtk+ librsvg libusb))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.gnu.org/software/inklingreader/")
    (synopsis "Wacom Inkling sketch format conversion and manipulation")
    (description
     "GNU InklingReader is a package to support the Wacom Inkling device,
including data conversion to various free formats, basic editing features,
and an Inkscape plugin.")
    (license license:gpl3+)))
