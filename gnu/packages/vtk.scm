;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
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
  #:use-module ((guix licenses) #:select (bsd-3))
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages image)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public vtk
  (package
    (name "vtk")
    (version "7.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.vtk.org/files/release/"
                                  (version-major+minor version)
                                  "/VTK-" version ".tar.gz"))
              (sha256
               (base32
                "0yj96z58haan77gzilnqp7xpf8hg5jk11a3jx55p2ksd400s0gjz"))))
    (build-system cmake-build-system)
    (arguments
     '(#:build-type "Release"           ;Build without '-g' to save space.
       ;; -DVTK_USE_SYSTEM_NETCDF:BOOL=TRUE requires netcdf_cxx
       #:configure-flags '("-DVTK_USE_SYSTEM_EXPAT:BOOL=TRUE"
                           "-DVTK_USE_SYSTEM_FREETYPE:BOOL=TRUE"
                           "-DVTK_USE_SYSTEM_HDF5:BOOL=TRUE"
                           "-DVTK_USE_SYSTEM_JPEG:BOOL=TRUE"
                           "-DVTK_USE_SYSTEM_JSONCPP:BOOL=TRUE"
                           "-DVTK_USE_SYSTEM_LIBXML2:BOOL=TRUE"
                           "-DVTK_USE_SYSTEM_OGGTHEORA:BOOL=TRUE"
                           "-DVTK_USE_SYSTEM_PNG:BOOL=TRUE"
                           "-DVTK_USE_SYSTEM_TIFF:BOOL=TRUE"
                           "-DVTK_USE_SYSTEM_ZLIB:BOOL=TRUE")
       #:tests? #f))                              ;XXX: no "test" target
    (inputs
     `(("libXt" ,libxt)
       ("xproto" ,xproto)
       ("libX11" ,libx11)
       ("libxml2" ,libxml2)
       ("mesa" ,mesa)
       ("glu" ,glu)
       ("expat" ,expat)
       ("freetype" ,freetype)
       ("hdf5" ,hdf5)
       ("jpeg" ,libjpeg)
       ("jsoncpp" ,jsoncpp)
       ("libogg" ,libogg)
       ("libtheora" ,libtheora)
       ("png" ,libpng)
       ("tiff" ,libtiff)
       ("zlib" ,zlib)))
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
