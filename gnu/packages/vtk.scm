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

(define-module (gnu packages vtk)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix licenses)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gl))

(define-public vtk
  (package
    (name "vtk")
    (version "6.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.vtk.org/files/release/"
                                  (substring version 0
                                             (string-rindex version #\.))
                                  "/VTK-" version ".tar.gz"))
              (sha256
               (base32
                "0d7shccdkyj4mbh2riilslgx3gd28in4c7xpm0lxa1ln8w5g2zdx"))))
    (build-system cmake-build-system)
    (arguments
     ;; Build without '-g' to save space.
     '(#:configure-flags '("-DCMAKE_BUILD_TYPE=Release")
       #:tests? #f))                              ;XXX: no "test" target
    (inputs
     `(("libXt" ,libxt)
       ("xproto" ,xproto)
       ("libX11" ,libx11)
       ("mesa" ,mesa)))
    (home-page "http://www.vtk.org/")
    (synopsis "Libraries for 3D computer graphics")
    (description
     "The Visualization Toolkit (VTK) is a C++ library for 3D computer graphics,
image processing and visualization.  It supports a wide variety of
visualization algorithms including: scalar, vector, tensor, texture, and
volumetric methods; and advanced modeling techniques such as: implicit
modeling, polygon reduction, mesh smoothing, cutting, contouring, and Delaunay
triangulation.  VTK has an extensive information visualization framework, has
a suite of 3D interaction widgets, supports parallel processing, and
integrates with various databases on GUI toolkits such as Qt and Tk.")
    (license bsd-3)))
