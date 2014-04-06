;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
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
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages fltk)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages less)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages texlive)
  #:use-module (gnu packages xml))

(define-public units
  (package
   (name "units")
   (version "2.11")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/units/units-" version
                                ".tar.gz"))
            (sha256 (base32
                     "1gjs3wc212aaiq4r76hx9nl1h3fa39n0ljwl9420d6ixl3rdmdjk"))))
   (build-system gnu-build-system)
   (synopsis "Conversion between thousands of scales")
   (description
    "GNU Units converts numeric quantities between units of measure.  It
can handle scale changes through adaptive usage of standard scale prefixes
(micro-, kilo-, etc.).  It can also handle nonlinear conversions such as
Fahrenheit to Celsius.  Its interpreter is powerful enough to be used
effectively as a scientific calculator.")
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
    (version "4.54")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/glpk/glpk-"
                          version ".tar.gz"))
      (sha256
       (base32
        "18gr2anv8gyps6j9f22k7li6w07glvww666sdqblvlq2hh3whwmb"))))
    (build-system gnu-build-system)
    (inputs
     `(("gmp" ,gmp)))
    (arguments
     `(#:configure-flags '("--with-gmp")))
    (home-page "http://www.gnu.org/software/glpk/")
    (synopsis "GNU Linear Programming Kit, supporting the MathProg language")
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
    (version "0.8.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/pspp/pspp-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1w7h3dglgx0jlq1wb605b8pgfsk2vr1q2q2rj7bsajh9ihbcsixr"))))
    (build-system gnu-build-system)
    (inputs
     `(("cairo" ,cairo)
       ("fontconfig" ,fontconfig)
       ("gettext" ,gnu-gettext)
       ("gsl" ,gsl)
       ("libxml2" ,libxml2)
       ("pango" ,pango)
       ("readline" ,readline)
       ("gtk" ,gtk+-2)
       ("gtksourceview" ,gtksourceview)
       ("zlib" ,zlib)))
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (home-page "http://www.gnu.org/software/pspp/")
    (synopsis "Statistical analysis")
    (description
     "GNU PSPP is a statistical analysis program.  It can perform
descriptive statistics, T-tests, linear regression and non-parametric tests. 
It features both a graphical interface as well as command-line input. PSPP is
designed to interoperate with Gnumeric, LibreOffice and OpenOffice.  Data can
be imported from spreadsheets, text files and database sources and it can be
output in text, PostScript, PDF or HTML.")
    (license license:gpl3+)))

(define-public lapack
  (package
    (name "lapack")
    (version "3.5.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://www.netlib.org/lapack/lapack-"
                          version ".tgz"))
      (sha256
       (base32
        "0lk3f97i9imqascnlf6wr5mjpyxqcdj73pgj97dj2mgvyg9z1n4s"))))
    (build-system cmake-build-system)
    (home-page "http://www.netlib.org/lapack/")
    (inputs `(("fortran" ,gfortran-4.8)
              ("python" ,python-2)))
    (arguments
     `(#:modules ((guix build cmake-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       #:phases (alist-cons-before
                 'check 'patch-python
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let ((python (assoc-ref inputs "python")))
                     (substitute* "lapack_testing.py"
                       (("/usr/bin/env python") python))))
                 %standard-phases)))
    (synopsis "Library for numerical linear algebra")
    (description
     "LAPACK is a Fortran 90 library for solving the most commonly occurring
problems in numerical linear algebra.")
    (license (license:bsd-style "file://LICENSE"
                                "See LICENSE in the distribution."))))

(define-public gnuplot
  (package
    (name "gnuplot")
    (version "4.6.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/gnuplot/gnuplot/"
                                version "/gnuplot-" version ".tar.gz"))
      (sha256
       (base32
        "1xd7gqdhlk7k1p9yyqf9vkk811nadc7m4si0q3nb6cpv4pxglpyz"))))
    (build-system gnu-build-system)
    (inputs `(("readline" ,readline)
              ("cairo" ,cairo)
              ("pango" ,pango)
              ("gd" ,gd)))
    (native-inputs `(("texlive" ,texlive)
                     ("pkg-config" ,pkg-config)))
    (home-page "http://www.gnuplot.info")
    (synopsis "Command-line driven graphing utility")
    (description "Gnuplot is a portable command-line driven graphing
utility. It was originally created to allow scientists and students to
visualize mathematical functions and data interactively, but has grown to
support many non-interactive uses such as web scripting. It is also used as a
plotting engine by third-party applications like Octave.")
    ;;  X11 Style with the additional restriction that derived works may only be
    ;;  distributed as patches to the original.
    (license (license:fsf-free
	      "http://gnuplot.cvs.sourceforge.net/gnuplot/gnuplot/Copyright"))))

(define-public hdf5
  (package
    (name "hdf5")
    (version "1.8.12")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://www.hdfgroup.org/ftp/HDF5/current/src/hdf5-"
                          version ".tar.bz2"))
      (sha256
       (base32 "0f9n0v3p3lwc7564791a39c6cn1d3dbrn7d1j3ikqsi27a8hy23d"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
        (alist-cons-before
         'configure 'patch-configure
         (lambda _
           (substitute* "configure"
             (("/bin/mv") "mv")))
         %standard-phases)))
    (outputs '("out" "bin" "lib" "include"))
    (home-page "http://www.hdfgroup.org")
    (synopsis "Management suite for  extremely large and complex data")
    (description "HDF5 is a suite that makes possible the management of
extremely large and complex data collections.")
    (license (license:x11-style
              "http://www.hdfgroup.org/ftp/HDF5/current/src/unpacked/COPYING"))))


;; For a fully featured Octave, users  are strongly recommended also to install
;; the following packages: texinfo, less, ghostscript, gnuplot.
(define-public octave
  (package
    (name "octave")
    (version "3.8.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/octave/octave-"
			  version ".tar.gz"))
      (sha256
       (base32
	"0ks9pr154syw0vb3jn6xsnrkkrbvf9y7i7gaxa28rz6ngxbxvq9l"))))
    (build-system gnu-build-system)
    (inputs
     `(("lapack" ,lapack)
       ("readline" ,readline)
       ("glpk" ,glpk)
       ("curl" ,curl)
       ("pcre" ,pcre)
       ("fltk" ,fltk)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("hdf5-lib" ,hdf5 "lib")
       ("hdf5-include" ,hdf5 "include")
       ("libxft" ,libxft)
       ("mesa" ,mesa)
       ("zlib" ,zlib)))
    (native-inputs
     `(("gfortran" ,gfortran-4.8)
       ("pkg-config" ,pkg-config)
       ("perl" ,perl)
       ;; The following inputs are not actually used in the build process.  However, the
       ;; ./configure gratuitously tests for their existence and assumes that programs not
       ;; present at build time are also not, and can never be, available at run time!
       ;; If these inputs are therefore not present, support for them will be built out.
       ;; However, Octave will still run without them, albeit without the features they
       ;; provide.
       ("less" ,less)
       ("texinfo" ,texinfo)
       ("ghostscript" ,ghostscript)
       ("gnuplot" ,gnuplot)))
    (arguments
     `(#:configure-flags (list (string-append "--with-shell="
			    (assoc-ref %build-inputs "bash")
			    "/bin/sh"))))
    (home-page "http://www.gnu.org/software/octave/")
    (synopsis "High-level language for numerical computation")
    (description "GNU Octave is a high-level interpreted language that is specialized
for numerical computations.  It can be used for both linear and non-linear
applications and it provides great support for visualizing results.  Work may
be performed both at the interactive command-line as well as via script
files.")
    (license license:gpl3+)))
