;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Andreas Enge <andreas@enge.fr>
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

(define-module (gnu packages doxygen)
  #:use-module ((guix licenses) #:select (gpl3+))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages python))

(define-public doxygen
  (package
    (name "doxygen")
    (version "1.8.7")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://ftp.stack.nl/pub/users/dimitri/"
                                 name "-" version ".src.tar.gz"))
             (sha256
              (base32
               "1ng3dv5fninhfi2fj75ghkr5jwsl653fxv2sxhaswj11x2vcdsn6"))
             (patches (list (search-patch "doxygen-tmake.patch")
                            (search-patch "doxygen-test.patch")))))
    (build-system gnu-build-system)
    ;; The presence of graphviz is checked, but it does not seem to influence
    ;; the output: Even after adding it as an input, no reference to it is
    ;; retained. It might be an option to add it as a propagated input,
    ;; only so that it becomes installed in the user profile.
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)
       ("libxml2" ,libxml2) ; provides xmllint for the tests
       ("perl" ,perl) ; for the tests
       ("python" ,python-2))) ; for creating the documentation
    (arguments
     `(#:test-target "test"
       #:phases
         (alist-replace
          'configure
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              ;; do not pass "--enable-fast-install", which makes the
              ;; configure process fail
              (zero? (system*
                      "./configure"
                      "--prefix" out))))
          %standard-phases)))
    (home-page "http://www.stack.nl/~dimitri/doxygen/")
    (synopsis "tool for generating documentation from annotated sources")
    (description "Doxygen is the de facto standard tool for generating
documentation from annotated C++ sources, but it also supports other popular
programming languages such as C, Objective-C, C#, PHP, Java, Python,
IDL (Corba, Microsoft, and UNO/OpenOffice flavors), Fortran, VHDL, Tcl,
and to some extent D.")
    (license gpl3+)))
