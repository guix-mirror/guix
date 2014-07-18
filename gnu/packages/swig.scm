;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages swig)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix licenses)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages python)
  #:use-module (gnu packages perl)
  #:export (swig))

(define swig
  (package
    (name "swig")
    (version "2.0.11")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/swig/swig-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0kj21b6syp62vx68r1j6azv9033kng68pxm1k79pm4skkzr0ny33"))))
    (build-system gnu-build-system)
    (native-inputs `(("boost" ,boost)))
    (inputs `(("pcre" ,pcre)

              ;; Provide these to run the corresponding tests.
              ("guile" ,guile-2.0)
              ("perl" ,perl)
              ("python" ,python-wrapper)))
    (home-page "http://swig.org/")
    (synopsis
     "Interface compiler that connects C/C++ code to higher-level languages")
    (description
     "SWIG is an interface compiler that connects programs written in C and
C++ with languages such as Perl, Python, Ruby, Scheme, and Tcl.  It works by
taking the declarations found in C/C++ header files and using them to generate
the wrapper code that scripting languages need to access the underlying C/C++
code.  In addition, SWIG provides a variety of customization features that let
you tailor the wrapping process to suit your application.")

    ;; See http://www.swig.org/Release/LICENSE for details.
    (license gpl3+)))
