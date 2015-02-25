;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages graphics)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages boost))

(define-public cgal
  (package
    (name "cgal")
    (version "4.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://gforge.inria.fr/frs/download.php/file/34402/CGAL-"
                    version ".tar.xz"))
              (sha256
               (base32
                "1565ycbds92bxmhi09avc1jl6ks141ig00j110l49gqxp9swy6zv"))))
    (build-system cmake-build-system)
    (arguments
     '(;; "RelWithDebInfo" is not supported.
       #:build-type "Release"

       ;; No 'test' target.
       #:tests? #f))
    (inputs
     `(("mpfr" ,mpfr)
       ("gmp" ,gmp)
       ("boost" ,boost)))
    (home-page "http://cgal.org/")
    (synopsis "Computational geometry algorithms library")
    (description
     "CGAL provides easy access to efficient and reliable geometric algorithms
in the form of a C++ library.  CGAL is used in various areas needing geometric
computation, such as: computer graphics, scientific visualization, computer
aided design and modeling, geographic information systems, molecular biology,
medical imaging, robotics and motion planning, mesh generation, numerical
methods, etc.  It provides data structures and algorithms such as
triangulations, Voronoi diagrams, polygons, polyhedra, mesh generation, and
many more.")

    ;; The 'LICENSE' file explains that a subset is available under more
    ;; permissive licenses.
    (license license:gpl3+)))

(define-public ilmbase
  (package
    (name "ilmbase")
    (version "2.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/openexr/ilmbase-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1izddjwbh1grs8080vmaix72z469qy29wrvkphgmqmcm0sv1by7c"))))
    (build-system gnu-build-system)
    (home-page "http://www.openexr.com/")
    (synopsis "Utility C++ libraries for threads, maths, and exceptions")
    (description
     "IlmBase provides several utility libraries for C++.  Half is a class
that encapsulates ILM's 16-bit floating-point format.  IlmThread is a thread
abstraction.  Imath implements 2D and 3D vectors, 3x3 and 4x4 matrices,
quaternions and other useful 2D and 3D math functions.  Iex is an
exception-handling library.")
    (license license:bsd-3)))

(define-public openexr
  (package
    (name "openexr")
    (version "2.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/openexr/openexr-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0ca2j526n4wlamrxb85y2jrgcv0gf21b3a19rr0gh4rjqkv1581n"))
              (modules '((guix build utils)))
              (snippet
               '(substitute* (find-files "." "tmpDir\\.h")
                  (("\"/var/tmp/\"")
                   "\"/tmp/\"")))
              (patches (list (search-patch "openexr-missing-samples.patch")))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("ilmbase" ,ilmbase)                       ;used in public headers
       ("zlib" ,zlib)))                           ;OpenEXR.pc reads "-lz"
    (home-page "http://www.openexr.com")
    (synopsis #f)
    (description
     "OpenEXR is a high dynamic-range (HDR) image file format developed for
use in computer imaging applications.  The IlmImf libraries supports storage
of the \"EXR\" file format for storing 16-bit floating-point images.")
    (license license:bsd-3)))
