;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 John Darrington <jmd@gnu.org>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages vtk))

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
    (version "2.4.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/mia/mia/"
                                  (version-major+minor version)
                                  "/mia-" version ".tar.xz"))
              (sha256
               (base32
                "124gvf8nkls59mlnx8ynq00n9zrah7a54gsywafx7qmfr0y95ra7"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list "-DMIA_CREATE_NIPYPE_INTERFACES=0"
             (string-append "-DCMAKE_INSTALL_LIBDIR="
                            (assoc-ref %outputs "out") "/lib")
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
