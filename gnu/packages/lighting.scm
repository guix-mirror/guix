;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 John J. Foerch <jjfoerch@earthlink.net>
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

(define-module (gnu packages lighting)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gnunet)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages libftdi)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf))

(define-public ola
  (package
    (name "ola")
    (version "0.10.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/OpenLightingProject/ola/releases/download/"
                    version "/ola-" version ".tar.gz"))
              (sha256
               (base32
                "09zx1c8nkj29shfdzkahrh9397m3mwnsy0gj7jrb63f89f3n2vlq"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("bison" ,bison)
       ("cppunit" ,cppunit)
       ("flex" ,flex)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libftdi" ,libftdi)
       ("libmicrohttpd" ,libmicrohttpd)
       ("libusb" ,libusb)
       ("libuuid" ,util-linux)
       ("zlib" ,zlib)))
    (propagated-inputs
     `(("protobuf" ,protobuf))) ;; for pkg-config --libs libola
    (arguments
     `(;; G++ >= 4.8 macro expansion tracking requires lots of memory, causing
       ;; build to fail on low memory systems.  We disable that with the
       ;; following configure flags.
       #:configure-flags (list "CXXFLAGS=-ftrack-macro-expansion=0")))
    (synopsis "Framework for controlling entertainment lighting equipment")
    (description "The Open Lighting Architecture is a framework for lighting
control information.  It supports a range of protocols and over a dozen USB
devices.  It can run as a standalone service, which is useful for converting
signals between protocols, or alternatively using the OLA API, it can be used
as the backend for lighting control software.  OLA runs on many different
platforms including ARM, which makes it a perfect fit for low cost Ethernet to
DMX gateways.")
    (home-page "https://www.openlighting.org/ola")
    (license license:lgpl2.1+)))
