;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
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

(define-module (gnu packages maths)
  #:use-module (gnu packages)
  #:use-module ((guix licenses)
                #:renamer (symbol-prefix-proc 'license:))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages compression)
  #:use-module ((gnu packages gettext)
                #:renamer (symbol-prefix-proc 'gnu:))
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages xml))

(define-public units
  (package
   (name "units")
   (version "2.02")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/units/units-" version
                                ".tar.gz"))
            (sha256 (base32
                     "16jfji9g1zc99agd5dcinajinhcxr4dgq2lrbc9md69ir5qgld1b"))))
   (build-system gnu-build-system)
   (synopsis "Conversion between thousands of scales")
   (description
    "Units is a program for converting measured quantities between units of
measure.  It can handle scale changes through adaptive usage of standard
scale prefixes (i.e. micro-, kilo-, etc.).  It can also handle nonlinear
conversions such as Fahrenheit to Celcius.  Its interpreter is powerful
enough to be used effectively as a scientific calculator.")
   (license license:gpl3+)
   (home-page "http://www.gnu.org/software/units/")))

(define-public gsl
  (package
    (name "gsl")
    (version "1.16")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/gsl/gsl-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0lrgipi0z6559jqh82yx8n4xgnxkhzj46v96dl77hahdp58jzg3k"))))
    (build-system gnu-build-system)
    (arguments
     `(#:parallel-tests? #f
       #:phases
        (alist-replace
         'configure
         (lambda* (#:key target system outputs #:allow-other-keys #:rest args)
           (let ((configure (assoc-ref %standard-phases 'configure)))
             ;; disable numerically unstable test on i686, see thread at
             ;; http://lists.gnu.org/archive/html/bug-gsl/2011-11/msg00019.html
             (if (string=? (or target system) "i686-linux")
                 (substitute* "ode-initval2/Makefile.in"
                   (("TESTS = \\$\\(check_PROGRAMS\\)") "TESTS =")))
             (apply configure args)))
         %standard-phases)))
    (home-page "http://www.gnu.org/software/gsl/")
    (synopsis "Numerical library for C and C++")
    (description
     "The GNU Scientific Library is a library for numerical analysis in C
and C++.  It includes a wide range of mathematical routines, with over 1000
functions in total.  Subject areas covered by the library include:
differential equations, linear algebra, Fast Fourier Transforms and random
numbers.")
    (license license:gpl3+)))

(define-public glpk
  (package
    (name "glpk")
    (version "4.52.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/glpk/glpk-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0nz9ngmx23c8gbjr8l8ygnfaanxj2mwbl8awpg630bgrkxdnhc9j"))))
    (build-system gnu-build-system)
    (inputs
     `(("gmp" ,gmp)))
    (arguments
     `(#:configure-flags '("--with-gmp")))
    (home-page "http://www.gnu.org/software/glpk/")
    (synopsis "NU Linear Programming Kit, supporting the MathProg language")
    (description
     "GLPK is a C library for solving large-scale linear programming (LP),
mixed integer programming (MIP), and other related problems.  It supports the
GNU MathProg modeling language, a subset of the AMPL language, and features a
translator for the language.  In addition to the C library, a stand-alone
LP/MIP solver is included in the package.")
    (license license:gpl3+)))

(define-public pspp
  (package
    (name "pspp")
    (version "0.8.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/pspp/pspp-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0qhxsdbwxd3cn1shc13wxvx2lg32lp4z6sz24kv3jz7p5xfi8j7x"))))
    (build-system gnu-build-system)
    (inputs
     `(("gettext" ,gnu:gettext)
       ("gsl" ,gsl)
       ("libxml2" ,libxml2)
       ("readline" ,readline)
       ("zlib" ,zlib)))
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (arguments
     `(#:configure-flags
       `("--without-cairo" ; FIXME: tests currently fail for lack of font
         "--without-gui"))) ; FIXME: package missing dependencies
    (home-page "http://www.gnu.org/software/pspp/")
    (synopsis "Statistical analysis")
    (description
     "PSPP is a statistical analysis program.  It can perform descriptive
statistics, T-tests, linear regression and non-parametric tests.  It features
both a graphical interface as well as command-line input. PSPP is designed to
interoperate with Gnumeric, LibreOffice and OpenOffice.  Data can be imported
from spreadsheets, text files and database sources and it can be output in
text, Postscript, PDF or HTML.")
    (license license:gpl3+)))
