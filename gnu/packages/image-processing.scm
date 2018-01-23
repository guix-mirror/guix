;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 John Darrington <jmd@gnu.org>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages image-processing)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

;; We use the latest snapshot of this package because the latest release is
;; from 2011 and has known vulnerabilities that cannot easily be fixed by
;; applying patches.
(define-public dcmtk
  (package
    (name "dcmtk")
    (version "3.6.1_20170228")
    (source (origin
              (method url-fetch)
              (uri (string-append "ftp://dicom.offis.de/pub/dicom/offis/"
                                  "software/dcmtk/snapshot/dcmtk-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "04cwfx8yrscqcd59mxk2fh6314ckayi9cp68iql5a57pf2pg5qld"))))
    (build-system gnu-build-system)
    (inputs
     `(("libtiff" ,libtiff)
       ("libpng" ,libpng)
       ("doxygen" ,doxygen)
       ("zlib" ,zlib)))
    (native-inputs
     `(("perl" ,perl)))
    (home-page "http://dcmtk.org")
    (synopsis "Libraries and programs implementing parts of the DICOM standard")
    (description "DCMTK is a collection of libraries and applications
implementing large parts the DICOM standard.  It includes software for
examining, constructing and converting DICOM image files, handling offline
media, sending and receiving images over a network connection, as well as
demonstrative image storage and worklist servers.")
    (license (license:fsf-free
              "file://COPYRIGHT"
              "A union of the Apache 2.0 licence and various non-copyleft
licences similar to the Modified BSD licence."))))

(define-public mia
  (package
    (name "mia")
    (version "2.4.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/mia/mia/"
                                  (version-major+minor version)
                                  "/mia-" version ".tar.xz"))
              (sha256
               (base32
                "0j4nd5z7i3v199jh7hqqhwd4g7snchizkc7rhzanpvngqg91m1pb"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list "-DMIA_CREATE_NIPYPE_INTERFACES=0"
             "-DCMAKE_CXX_FLAGS=-fpermissive")))
    (inputs
     `(("boost" ,boost)
       ("dcmtk" ,dcmtk)
       ("doxygen" ,doxygen)
       ("eigen" ,eigen)
       ("fftw" ,fftw)
       ("fftwf" ,fftwf)
       ("gsl" ,gsl)
       ("gts" ,gts)
       ("hdf5" ,hdf5)
       ("itpp" ,itpp)
       ("libjpeg" ,libjpeg)
       ("libpng" ,libpng)
       ("libtiff" ,libtiff)
       ("libxml" ,libxml2)
       ("libxml++" ,libxml++)
       ("maxflow" ,maxflow)
       ("niftilib" ,niftilib)
       ("nlopt" ,nlopt)
       ("openexr" ,openexr)
       ("python-lxml" ,python2-lxml)
       ("vtk" ,vtk)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python" ,python-2)))
    (home-page "http://mia.sourceforge.net")
    (synopsis "Toolkit for gray scale medical image analysis")
    (description "MIA provides a combination of command line tools, plug-ins,
and libraries that make it possible run image processing tasks interactively
in a command shell and to prototype using the shell's scripting language.  It
is built around a plug-in structure that makes it easy to add functionality
without compromising the original code base and it makes use of a wide variety
of external libraries that provide additional functionality.")
    (license license:gpl3+)))

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
    (license license:bsd-3)))
