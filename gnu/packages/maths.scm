;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014, 2016, 2017 John Darrington <jmd@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2017, 2018 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2014 Mathieu Lirzin <mathieu.lirzin@openmailbox.org>
;;; Copyright © 2015, 2016, 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2016, 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2015 Fabian Harfert <fhmgufs@web.de>
;;; Copyright © 2016 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2016, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017 Paul Garlick <pgarlick@tourbillion-technology.com>
;;; Copyright © 2017 ng0 <contact.ng0@cryptolab.net>
;;; Copyright © 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017 Theodoros Foradis <theodoros@foradis.org>
;;; Copyright © 2017 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Dave Love <me@fx@gnu.org>
;;; Copyright © 2018 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2018 Joshua Sierles, Nextjournal <joshua@nextjournal.com>
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
  #:use-module (ice-9 regex)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system ocaml)
  #:use-module (guix build-system r)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fltk)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages java)
  #:use-module (gnu packages less)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages netpbm)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages scheme)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xml)
  #:use-module (srfi srfi-1))

(define-public aris
  (package
    (name "aris")
    (version "2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/" name "/" name "-" version ".tar.gz"))
              (sha256 (base32
                       "1q1887ryqdr9sn0522hc7p16kqwlxxyz5dkmma8ar2nxplhgll7q"))))
    (build-system gnu-build-system)
    (inputs `(("gtk+" ,gtk+)
              ("libxml2" ,libxml2)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (synopsis "Natural deduction first-order logic interface")
    (description "Aris is a program for performing logical proofs.  It supports
propositional and predicate logic, as well as Boolean algebra and
arithmetical logic.  In addition to its predefined inference and equivalence
rules, Aris also supports references to older proofs.  Its use of standard
logical symbols and its natural deduction interface make it easy to use for
beginners.")
    (license license:gpl3+)
    (home-page "https://www.gnu.org/software/aris/")))

(define-public c-graph
  (package
   (name "c-graph")
   (version "2.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/c-graph/c-graph-" version
                                ".tar.gz"))
            (sha256 (base32
                     "1hlvpzrh7hzzf533diyfiabzskddi8zx92av9hwkjw3l46z7qv01"))))
   (build-system gnu-build-system)
   (inputs
     `(("fortran" ,gfortran)))
   (synopsis "Visualizing and demonstrating convolution")
   (description
     "GNU C-Graph is a tool for demonstrating the theory of convolution.
Thus, it can serve as an excellent aid to students of signal and systems
theory in visualizing the convolution process.  Rather than forcing the
student to write code, the program offers an intuitive interface with
interactive dialogs to guide them.")
   (license license:gpl3+)
   (home-page "https://www.gnu.org/software/c-graph/")))

(define-public coda
  (package
    (name "coda")
    (version "2.18.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/stcorp/coda/releases/download/"
                           version "/coda-" version ".tar.gz"))
       (sha256
        (base32 "1zlzgcvwmmjm8mw8w4rg2rqy0pjilz7kyyxm0y4p8cbljbbjxxz0"))
       (patches (search-patches "coda-use-system-libs.patch"))
       (modules '((guix build utils)))
       (snippet
        ;; Make sure we don't use the bundled software.
        '(for-each (lambda (d)
                     (delete-file-recursively (string-append "libcoda/" d)))
                   '("zlib" "pcre" "expat")))))
    (native-inputs
     `(("fortran" ,gfortran)
       ("python" ,python)
       ("python-numpy" ,python-numpy)))
    (inputs
     `(("zlib" ,zlib)
       ("pcre" ,pcre)
       ("expat" ,expat)
       ("hdf4" ,hdf4-alt)
       ("hdf5" ,hdf5)))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--with-hdf4" "--with-hdf5" "--enable-python"
                           "LIBS= -lz -lpcre -lexpat")))
    (synopsis "A common interface to various earth observation data formats")
    (description
     "The Common Data Access toolbox (CODA) provides a set of interfaces for
reading remote sensing data from earth observation data files.  It consists of
command line applications and interfaces to the C, Fortran, Python, and Java
programming languages.")
    (home-page "https://stcorp.nl/coda")
    (license license:gpl2+)))

(define-public units
  (package
   (name "units")
   (version "2.16")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/units/units-" version
                                ".tar.gz"))
            (sha256 (base32
                     "11hnp3gcmcc5kci2caxw4hs6m08h2mhqs3xzqq7iafx1ha2ggwyw"))))
   (build-system gnu-build-system)
   (inputs
    `(("readline" ,readline)
      ("python" ,python-wrapper)        ;for 'units_cur' script
      ("python-requests" ,python-requests)))
   (arguments
    `(#:phases (modify-phases %standard-phases
                 (add-after 'install 'wrap-units_cur
                   (lambda* (#:key outputs #:allow-other-keys)
                     (let* ((out (assoc-ref outputs "out"))
                            (bin (string-append out "/bin")))
                       (wrap-program (string-append bin "/units_cur")
                         `("PYTHONPATH" ":" prefix
                           ,(search-path-as-string->list (getenv "PYTHONPATH"))))
                       #t))))))
   (synopsis "Conversion between thousands of scales")
   (description
    "GNU Units converts numeric quantities between units of measure.  It
can handle scale changes through adaptive usage of standard scale
prefixes (micro-, kilo-, etc.).  It can also handle nonlinear
conversions such as Fahrenheit to Celsius.  Its interpreter is powerful
enough to be used effectively as a scientific calculator.")
   (license license:gpl3+)
   (home-page "https://www.gnu.org/software/units/")))

(define-public double-conversion
  (package
    (name "double-conversion")
    (version "1.1.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/floitsch/double-conversion/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0cnr8xhyjfxijay8ymkqcph3672wp2lj23qhdmr3m4kia5kpdf83"))))
    (build-system cmake-build-system)
    (arguments
     '(#:test-target "test"
       #:configure-flags '("-DBUILD_SHARED_LIBS=ON"
                           "-DBUILD_TESTING=ON")))
    (home-page "https://github.com/floitsch/double-conversion")
    (synopsis "Conversion routines for IEEE doubles")
    (description
     "The double-conversion library provides binary-decimal and decimal-binary
routines for IEEE doubles.  The library consists of efficient conversion
routines that have been extracted from the V8 JavaScript engine.")
    (license license:bsd-3)))

(define-public dionysus
  (package
    (name "dionysus")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/dionysus/dionysus-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "194pzs1mlsj4ww6v37qq3961h5hckm5h805cv0r14xj3g9wfx2sk"))))
    (build-system gnu-build-system)
    (inputs `(("tcl" ,tcl)))                      ;for 'tclsh'
    (synopsis "Local search for universal constants and scientific values")
    (description
     "GNU Dionysus is a convenient system for quickly retrieving the values of
mathematical constants used in science and engineering.  Values can be
searched using a simple command-line tool, choosing from three databases:
universal constants, atomic numbers, and constants related to
semiconductors.")
    (license license:gpl3+)
    (home-page "https://www.gnu.org/software/dionysus/")))

(define-public gsl
  (package
    (name "gsl")
    (version "2.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gsl/gsl-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "16yfs5n444s03np1naj6yp1fsysd42kdscxzkg0k2yvfjixx0ijd"))
              (patches (search-patches "gsl-test-i686.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(;; Currently there are numerous tests that fail on "exotic"
       ;; architectures such as aarch64 and ppc64le.
       ,@(if (string-prefix? "aarch64-linux"
                             (or (%current-target-system) (%current-system)))
           '(#:tests? #f)
           '())))
    (home-page "https://www.gnu.org/software/gsl/")
    (synopsis "Numerical library for C and C++")
    (description
     "The GNU Scientific Library is a library for numerical analysis in C
and C++.  It includes a wide range of mathematical routines, with over 1000
functions in total.  Subject areas covered by the library include:
differential equations, linear algebra, Fast Fourier Transforms and random
numbers.")
    (license license:gpl3+)))

(define-public ocaml-gsl
  (package
    (name "ocaml-gsl")
    (version "1.19.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://github.com/mmottl/gsl-ocaml/releases/download/v"
         version"/gsl-ocaml-" version ".tar.gz"))
       (sha256
        (base32
         "0nzp43hp8pbjqkrxnwp5lgjrabxayf61h18fjaydi0s5faq6f3xh"))))
    (build-system ocaml-build-system)
    (inputs
     `(("gsl" ,gsl)))
    (home-page "https://mmottl.github.io/gsl-ocaml")
    (synopsis "Bindings to the GNU Scientific Library")
    (description
     "GSL-OCaml is an interface to the @dfn{GNU scientific library} (GSL) for
the OCaml language.")
    (license license:gpl3+)))

(define-public ocaml4.01-gsl
  (package-with-ocaml4.01 ocaml-gsl))

(define-public glpk
  (package
    (name "glpk")
    (version "4.65")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/glpk/glpk-"
                          version ".tar.gz"))
      (sha256
       (base32
        "040sfaa9jclg2nqdh83w71sv9rc1sznpnfiripjdyr48cady50a2"))))
    (build-system gnu-build-system)
    (inputs
     `(("gmp" ,gmp)))
    (arguments
     `(#:configure-flags '("--with-gmp")))
    (home-page "https://www.gnu.org/software/glpk/")
    (synopsis "GNU Linear Programming Kit, supporting the MathProg language")
    (description
     "GLPK is a C library for solving large-scale linear programming (LP),
mixed integer programming (MIP), and other related problems.  It supports the
GNU MathProg modeling language, a subset of the AMPL language, and features a
translator for the language.  In addition to the C library, a stand-alone
LP/MIP solver is included in the package.")
    (license license:gpl3+)))

(define-public 4ti2
  (package
    (name "4ti2")
    (version "1.6.7")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://www.4ti2.de/version_" version
                          "/4ti2-" version ".tar.gz"))
      (sha256
       (base32
        "1frix3rnm9ffr93alqzw4cavxbfpf524l8rfbmcpyhwd3n1km0yl"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("which" ,(@ (gnu packages base) which)))) ; for the tests
    (inputs
     `(("glpk" ,glpk)
       ("gmp" ,gmp)))
    (home-page "http://www.4ti2.de/")
    (synopsis "Mathematical tool suite for problems on linear spaces")
    (description
     "4ti2 implements algorithms for solving algebraic, geometric and
combinatorial problems on linear spaces.  Among others, it solves systems
of linear equations, computes extreme rays of polyhedral cones, solves
integer programming problems and computes Markov bases for statistics.")
    (license license:gpl2+)))

(define-public cddlib
  (package
    (name "cddlib")
    (version "0.94h")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "ftp://ftp.math.ethz.ch/users/fukudak/cdd/cddlib-"
                          (string-delete #\. version) ".tar.gz"))
      (sha256
       (base32
        "1dasasscwfg793q8fwzgwf64xwj7w62yfvszpr8x8g38jka08vgy"))))
    (build-system gnu-build-system)
    (inputs
     `(("gmp" ,gmp)))
    (home-page "https://www.inf.ethz.ch/personal/fukudak/cdd_home/index.html")
    (synopsis "Library for convex hulls and extreme rays of polyhedra")
    (description
     "The C-library cddlib implements the Double Description Method of
Motzkin et al. for generating all vertices (i.e. extreme points) and extreme
rays of a general convex polyhedron given by a system of linear inequalities
in arbitrary dimension.  It can also be used for the converse operation of
computing convex hulls.")
    (license license:gpl2+)))

(define-public arpack-ng
  (package
    (name "arpack-ng")
    (version "3.5.0")
    (home-page "https://github.com/opencollab/arpack-ng")
    (source
     (origin
       (method url-fetch)
       (uri (string-append home-page "/archive/" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0f8jx3fifmj9qdp289zr7r651y1q48k1jya859rqxq62mvis7xsh"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'autoreconf
                    (lambda _
                      (invoke "autoreconf" "-vfi"))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (inputs
     `(("lapack" ,lapack)
       ("fortran" ,gfortran)))
    (synopsis "Fortran subroutines for solving eigenvalue problems")
    (description
     "ARPACK-NG is a collection of Fortran77 subroutines designed to solve
large scale eigenvalue problems.")
    (license (license:non-copyleft "file://COPYING"
                                "See COPYING in the distribution."))))

(define-public arpack-ng-3.3.0
  (package
    (inherit arpack-ng)
    (version "3.3.0")
    (name (package-name arpack-ng))
    (home-page (package-home-page arpack-ng))
    (source
     (origin
       (method url-fetch)
       (uri (string-append home-page "/archive/" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1cz53wqzcf6czmcpfb3vb61xi0rn5bwhinczl65hpmbrglg82ndd"))))))

(define-public arpack-ng-openmpi
  (package (inherit arpack-ng)
    (name "arpack-ng-openmpi")
    (inputs
     `(("mpi" ,openmpi)
       ,@(package-inputs arpack-ng)))
    (arguments `(#:configure-flags '("--enable-mpi")))
    (synopsis "Fortran subroutines for solving eigenvalue problems with MPI")))

(define-public lapack
  (package
    (name "lapack")
    (version "3.7.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://www.netlib.org/lapack/lapack-"
                          version ".tgz"))
      (sha256
       (base32
        "1j51r7n5w4k7r3lrvy7710xrpkg40wf4rqnmngfz6ck9ypckzign"))))
    (build-system cmake-build-system)
    (home-page "http://www.netlib.org/lapack/")
    (inputs `(("fortran" ,gfortran)
              ("python" ,python-2)))
    (arguments
     `(#:configure-flags (list
                          "-DBUILD_SHARED_LIBS:BOOL=YES"
                          "-DLAPACKE=ON"

                          ;; Build the 'LAPACKE_clatms' functions.
                          "-DLAPACKE_WITH_TMG=ON")
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'patch-python
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((python (assoc-ref inputs "python")))
               (substitute* "lapack_testing.py"
                 (("/usr/bin/env python") python)))
             #t)))))
    (synopsis "Library for numerical linear algebra")
    (description
     "LAPACK is a Fortran 90 library for solving the most commonly occurring
problems in numerical linear algebra.")
    (license (license:non-copyleft "file://LICENSE"
                                "See LICENSE in the distribution."))))

(define-public lapack-3.5
  (package
    (inherit lapack)
    (version "3.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.netlib.org/lapack/lapack-"
                           version ".tgz"))
       (sha256
        (base32
         "0lk3f97i9imqascnlf6wr5mjpyxqcdj73pgj97dj2mgvyg9z1n4s"))))))

(define-public scalapack
  (package
    (name "scalapack")
    (version "2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.netlib.org/scalapack/scalapack-"
                           version ".tgz"))
       (sha256
        (base32
         "0p1r61ss1fq0bs8ynnx7xq4wwsdvs32ljvwjnx6yxr8gd6pawx0c"))))
    (build-system cmake-build-system)
    (inputs
     `(("mpi" ,openmpi)
       ("fortran" ,gfortran)
       ("lapack" ,lapack)))             ;for testing only
    (arguments
     `(#:configure-flags `("-DBUILD_SHARED_LIBS:BOOL=YES")))
    (home-page "http://www.netlib.org/scalapack/")
    (synopsis "Library for scalable numerical linear algebra")
    (description
     "ScaLAPACK is a Fortran 90 library of high-performance linear algebra
routines on parallel distributed memory machines.  ScaLAPACK solves dense and
banded linear systems, least squares problems, eigenvalue problems, and
singular value problems.")
    (license (license:bsd-style "file://LICENSE"
                                "See LICENSE in the distribution."))))

(define-public gnuplot
  (package
    (name "gnuplot")
    (version "5.0.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/gnuplot/gnuplot/"
                                  version "/gnuplot-"
                                  version ".tar.gz"))
       (sha256
        (base32
         "0q5lr6nala3ln6f3yp6g17ziymb9r9gx9zylnw1y3hjmwl9lggjv"))))
    (build-system gnu-build-system)
    (inputs `(("readline" ,readline)
              ("cairo" ,cairo)
              ("pango" ,pango)
              ("gd" ,gd)
              ("lua" ,lua)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("texlive" ,texlive-tiny)))
    (home-page "http://www.gnuplot.info")
    (synopsis "Command-line driven graphing utility")
    (description "Gnuplot is a portable command-line driven graphing
utility.  It was originally created to allow scientists and students to
visualize mathematical functions and data interactively, but has grown to
support many non-interactive uses such as web scripting.  It is also used as a
plotting engine by third-party applications like Octave.")
    ;;  X11 Style with the additional restriction that derived works may only be
    ;;  distributed as patches to the original.
    (license (license:fsf-free
              "http://gnuplot.cvs.sourceforge.net/gnuplot/gnuplot/Copyright"))))

(define-public gctp
  (package
    (name "gctp")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/OkoSanto/GCTP/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0l9aqnqynh9laicn5dxf3rsb1n14xiks79wbyqccirzmjqd1c1x4"))))
    (native-inputs
     `(("fortran" ,gfortran)))
    (build-system gnu-build-system)
    (synopsis "General Cartographic Transformation Package (GCTP)")
    (description
     "The General Cartographic Transformation Package (GCTP) is a system of
software routines designed to permit the transformation of coordinate pairs
from one map projection to another.  The GCTP is the standard computer
software used by the National Mapping Division for map projection
computations.")
    (home-page "https://github.com/OkoSanto/GCTP")
    (license license:public-domain))) ;https://www2.usgs.gov/laws/info_policies.html

(define-public hdf4
  (package
    (name "hdf4")
    (version "4.2.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://support.hdfgroup.org/ftp/HDF/releases/HDF"
                           version "/src/hdf-" version ".tar.bz2"))
       (sha256
        (base32 "1wz0586zh91pqb95wvr0pbh71a8rz358fdj6n2ksp85x2cis9lsm"))
       (patches (search-patches "hdf4-architectures.patch"
                                "hdf4-reproducibility.patch"
                                "hdf4-shared-fortran.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gfortran" ,gfortran)
       ("bison" ,bison)
       ("flex" ,flex)))
    (inputs
     `(("zlib" ,zlib)
       ("libjpeg" ,libjpeg)))
    (arguments
     `(#:parallel-tests? #f
       #:configure-flags '("--enable-shared")
       #:phases
       (modify-phases %standard-phases
         ;; This is inspired by two of Debian's patches.
         (add-before 'configure 'add-more-aarch64-support
           (lambda _
             (substitute* '("mfhdf/ncgen/ncgen.l"
                            "mfhdf/ncgen/ncgenyy.c"
                            "mfhdf/libsrc/netcdf.h.in")
               (("AIX5L64") "__aarch64__"))
             #t))
         (add-before 'configure 'patchbuild
           (lambda _
             (substitute*
                 '("mfhdf/hdfimport/testutil.sh.in" "hdf/util/testutil.sh.in")
               (("/bin/rm") "rm")
               (("/bin/mkdir") "mkdir"))
             (substitute* (find-files "." "^Makefile\\.in$")
               (("@HDF_BUILD_XDR_TRUE@XDR_ADD = \
-R\\$\\(abs_top_builddir\\)/mfhdf/xdr/\\.libs") "")
               (("@HDF_BUILD_SHARED_TRUE@AM_LDFLAGS = \
-R\\$\\(abs_top_builddir\\)/mfhdf/libsrc/\\.libs \
-R\\$\\(abs_top_builddir\\)/hdf/src/\\.libs \\$\\(XDR_ADD\\)") ""))
             #t))
         (add-after 'configure 'patch-settings
           (lambda _
             ;; libhdf4.settings contains the full path of the
             ;; compilers used, and its contents are included in
             ;; .so-files.  We truncate the hashes to avoid
             ;; unnecessary store references to those compilers:
             (substitute* "libhdf4.settings"
               (("(/gnu/store/)([a-Z0-9]*)" all prefix hash)
                (string-append prefix (string-take hash 10) "...")))
             #t))
         )))
    (home-page "https://www.hdfgroup.org/products/hdf4/")
    (synopsis
     "Library and multi-object file format for storing and managing data")
    (description "HDF4 is a library and multi-object file format for storing
and managing data between machines.  HDF4 is an older hierarchical data format,
incompatible with HDF5.")
    (license
     (license:non-copyleft
      "https://www.hdfgroup.org/ftp/HDF/HDF_Current/src/unpacked/COPYING"))))

(define-public hdf4-alt
  (package
    (inherit hdf4)
    (name "hdf4-alt")
    (arguments
     (substitute-keyword-arguments (package-arguments hdf4)
       ((#:configure-flags flags) `(cons* "--disable-netcdf" ,flags))))
    (synopsis
     "HDF4 without netCDF API, can be combined with the regular netCDF library")))

(define-public hdf5
  (package
    (name "hdf5")
    (version "1.8.19")
    (source
     (origin
      (method url-fetch)
      (uri (list (string-append "https://support.hdfgroup.org/ftp/HDF5/releases/"
                                "hdf5-" (version-major+minor version)
                                "/hdf5-" version "/src/hdf5-"
                                version ".tar.bz2")
                 (string-append "https://support.hdfgroup.org/ftp/HDF5/"
                                "current"
                                (apply string-append
                                       (take (string-split version #\.) 2))
                                "/src/hdf5-" version ".tar.bz2")))
      (sha256
       (base32 "0f3jfbqpaaq21ighi40qzs52nb52kc2d2yjk541rjmsx20b3ih2r"))
      (patches (list (search-patch "hdf5-config-date.patch")))))
    (build-system gnu-build-system)
    (inputs
     `(("zlib" ,zlib)))
    (native-inputs
     `(("gfortran" ,gfortran)))
    (outputs '("out"       ; core library
               "fortran")) ; fortran interface
    (arguments
     `(;; Some of the users, notably Flann, need the C++ interface.
       #:configure-flags '("--enable-cxx"
                           "--enable-fortran"
                           "--enable-fortran2003")
       ;; Use -fPIC to allow the R bindings to link with the static libraries
       #:make-flags (list "CFLAGS=-fPIC"
                          "CXXFLAGS=-fPIC")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-configure
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "configure"
               (("/bin/mv") "mv"))
             (substitute* "fortran/src/Makefile.in"
               (("libhdf5_fortran_la_LDFLAGS =")
                (string-append "libhdf5_fortran_la_LDFLAGS = -Wl-rpath="
                               (assoc-ref outputs "fortran") "/lib")))
             (substitute* "hl/fortran/src/Makefile.in"
               (("libhdf5hl_fortran_la_LDFLAGS =")
                (string-append "libhdf5hl_fortran_la_LDFLAGS = -Wl,-rpath="
                               (assoc-ref outputs "fortran") "/lib")))
             #t))
         (add-after 'configure 'patch-settings
           (lambda _
             ;; libhdf5.settings contains the full path of the
             ;; compilers used, and its contents are included in
             ;; libhdf5.so.  We truncate the hashes to avoid
             ;; unnecessary store references to those compilers:
             (substitute* "src/libhdf5.settings"
              (("(/gnu/store/)([a-Z0-9]*)" all prefix hash)
               (string-append prefix (string-take hash 10) "..."))
              ;; Don't record the build-time kernel version to make the
              ;; settings file reproducible.
              (("Uname information:.*")
               "Uname information: Linux\n"))
             #t))
         (add-after 'install 'patch-references
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin"))
                   (zlib (assoc-ref inputs "zlib")))
               (substitute* (find-files bin "h5p?cc")
                 (("-lz" lib)
                  (string-append "-L" zlib "/lib " lib)))
               #t)))
         (add-after 'install 'split
            (lambda* (#:key inputs outputs #:allow-other-keys)
              ;; Move all fortran-related files
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (lib (string-append out "/lib"))
                     (inc (string-append out "/include"))
                     (ex (string-append out "/share/hdf5_examples/fortran"))
                     (fort (assoc-ref outputs "fortran"))
                     (fbin (string-append fort "/bin"))
                     (flib (string-append fort "/lib"))
                     (finc (string-append fort "/include"))
                     (fex (string-append fort "/share/hdf5_examples/fortran")))
                (mkdir-p fbin)
                (mkdir-p flib)
                (mkdir-p finc)
                (mkdir-p fex)
                (rename-file (string-append bin "/h5fc")
                             (string-append fbin "/h5fc"))
                (for-each (lambda (file)
                            (rename-file file
                                         (string-append flib "/" (basename file))))
                          (find-files lib ".*fortran.*"))
                (for-each (lambda (file)
                            (rename-file file
                                         (string-append finc "/" (basename file))))
                          (find-files inc ".*mod"))
                (for-each (lambda (file)
                            (rename-file file
                                         (string-append fex "/" (basename file))))
                          (find-files ex ".*"))
                (delete-file-recursively ex))
              #t)))))
    (home-page "http://www.hdfgroup.org")
    (synopsis "Management suite for extremely large and complex data")
    (description "HDF5 is a suite that makes possible the management of
extremely large and complex data collections.")
    (license (license:x11-style
              "http://www.hdfgroup.org/ftp/HDF5/current/src/unpacked/COPYING"))))

(define-public hdf-java
  (package
   (name "hdf-java")
   (version "3.3.2")
   (source
    (origin
      (method url-fetch)
      (uri (string-append
            "http://www.hdfgroup.org/ftp/HDF5/releases/HDF-JAVA/hdfjni-"
            version "/src/CMake-hdfjava-" version ".tar.gz"))
      (sha256
       (base32 "0m1gp2aspcblqzmpqbdpfp6giskws85ds6p5gz8sx7asyp7wznpr"))
      (modules '((guix build utils)))
      (snippet ; Make sure we don't use the bundled sources and binaries.
       `(begin
          (for-each delete-file
                    (list "SZip.tar.gz" "ZLib.tar.gz" "JPEG8d.tar.gz"
                          "HDF4.tar.gz" "HDF5.tar.gz"))
          (delete-file-recursively ,(string-append "hdfjava-" version "/lib"))))))
   (build-system gnu-build-system)
   (native-inputs
    `(("jdk" ,icedtea "jdk")
      ("automake" ,automake) ; For up to date 'config.guess' and 'config.sub'.
      ;; For tests:
      ("hamcrest-core" ,java-hamcrest-core)
      ("junit" ,java-junit)
      ("slf4j-simple" ,java-slf4j-simple)))
   (inputs
    `(("hdf4" ,hdf4)
      ("hdf5" ,hdf5)
      ("zlib" ,zlib)
      ("libjpeg" ,libjpeg)
      ("slf4j-api" ,java-slf4j-api)))
   (arguments
    `(#:configure-flags
      (list (string-append "--target=" ,(or (%current-target-system) (%current-system)))
            (string-append "--with-jdk=" (assoc-ref %build-inputs "jdk") "/include,"
                           (assoc-ref %build-inputs "jdk") "/lib" )
            (string-append "--with-hdf4=" (assoc-ref %build-inputs "hdf4") "/lib")
            (string-append "--with-hdf5=" (assoc-ref %build-inputs "hdf5") "/lib"))

      #:make-flags
      (list (string-append "HDFLIB=" (assoc-ref %build-inputs "hdf4") "/lib")
            (string-append "HDF5LIB=" (assoc-ref %build-inputs "hdf5") "/lib")
            (string-append "ZLIB=" (assoc-ref %build-inputs "zlib") "/lib/libz.so")
            (string-append "JPEGLIB="
                           (assoc-ref %build-inputs "libjpeg") "/lib/libjpeg.so")
            "LLEXT=so")

      #:phases
      (modify-phases %standard-phases
        (add-before 'configure 'chdir-to-source
          (lambda _ (chdir ,(string-append "hdfjava-" version))))
        (add-before 'configure 'patch-build
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (substitute* "configure"
              (("COPT=\"") "COPT=\"-O2 ") ; CFLAGS is ignored in Makefiles
              (("/bin/cat") (which "cat")))
            ;; Set classpath for compilation
            (substitute* '("hdf/hdf5lib/Makefile.in"
                           "hdf/hdf5lib/exceptions/Makefile.in"
                           "hdf/hdflib/Makefile.in")
              (("\\$\\(TOP\\)/lib/slf4j-api-1\\.7\\.5\\.jar")
               (string-append (assoc-ref inputs "slf4j-api")
                              "/share/java/slf4j-api.jar")))
            ;; Replace outdated config.sub and config.guess:
            (with-directory-excursion "config"
              (for-each (lambda (file)
                          (copy-file
                           (string-append (assoc-ref inputs "automake")
                                          "/share/automake-1.15/" file) file))
                        '("config.sub" "config.guess")))
            (mkdir-p (string-append (assoc-ref outputs "out")))
            ;; Set classpath for tests
            (let* ((build-dir (getcwd))
                   (lib (string-append build-dir "/lib"))
                   (jhdf (string-append lib "/jhdf.jar"))
                   (jhdf5 (string-append lib "/jhdf5.jar"))
                   (testjars
                    (map (lambda (i)
                           (string-append (assoc-ref inputs i)
                                          "/share/java/" i ".jar"))
                         '("junit" "hamcrest-core" "slf4j-api" "slf4j-simple")))
                   (class-path
                    (string-join `("." ,build-dir ,jhdf ,jhdf5 ,@testjars) ":")))

              (substitute* '("test/hdf5lib/Makefile.in"
                             "test/hdf5lib/junit.sh.in"
                             "examples/runExample.sh.in")
                (("/usr/bin/test")
                 (string-append (assoc-ref inputs "coreutils")
                                "/bin/test"))
                (("/usr/bin/uname")
                 (string-append (assoc-ref inputs "coreutils")
                                "/bin/uname"))
                (("CLASSPATH=[^\n]*")
                 (string-append "CLASSPATH=" class-path)))
              (setenv "CLASSPATH" class-path))
            #t))
        (add-before 'check 'build-examples
          (lambda _
            (zero? (apply system* `("javac"
                                    ,@(find-files "examples" ".*\\.java")))))))

      #:parallel-build? #f

      #:parallel-tests? #f ))
   (home-page "https://support.hdfgroup.org/products/java")
   (synopsis "Java interface for the HDF4 and HDF5 libraries")
   (description "Java HDF Interface (JHI) and Java HDF5 Interface (JHI5) use
the Java Native Interface to wrap the HDF4 and HDF5 libraries, which are
implemented in C.")

   ;; BSD-style license:
   (license (license:x11-style
             "https://support.hdfgroup.org/ftp/HDF5/hdf-java\
/current/src/unpacked/COPYING"))))

(define-public hdf-eos2
  (package
    (name "hdf-eos2")
    (version "19.1.0")
    (source
     (origin
       (method url-fetch)
       (uri "ftp://edhs1.gsfc.nasa.gov\
/edhs/hdfeos/latest_release/HDF-EOS2.19v1.00.tar.Z")
       (sha256
        (base32 "0c9fcz25s292ldap12wxmlrvnyz99z24p63d8fwx51bf8s0s1zrz"))
       (patches (search-patches "hdf-eos2-remove-gctp.patch"
                                "hdf-eos2-build-shared.patch"
                                "hdf-eos2-fortrantests.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gfortran" ,gfortran)))
    (inputs
     `(("hdf4" ,hdf4-alt) ; assume most HDF-EOS2 users won't use the HDF4 netCDF API
       ("zlib" ,zlib)
       ("libjpeg" ,libjpeg)
       ("gctp" ,gctp)))
    (arguments
     `( #:configure-flags '("--enable-install-include" "--enable-shared"
                            "CC=h4cc -Df2cFortran" "LIBS=-lgctp")
        #:parallel-tests? #f))
    (home-page "http://hdfeos.org/software/library.php#HDF-EOS2")
    (synopsis "HDF4-based data format for NASA's Earth Observing System")
    (description "HDF-EOS2 is a software library built on HDF4 which supports
the construction of data structures used in NASA's Earth Observing
System (Grid, Point and Swath).")

    ;; Source files carry a permissive license header.
    (license (license:non-copyleft home-page))))

(define-public hdf-eos5
  (package
    (name "hdf-eos5")
    (version "1.15")
    (source (origin
              (method url-fetch)
              (uri (string-append "ftp://edhs1.gsfc.nasa.gov\
/edhs/hdfeos5/latest_release/HDF-EOS5." version ".tar.Z"))
              (sha256
               (base32
                "1p83333nzzy8rn5chxlm0hrkjjnhh2w1ji8ac0f9q4xzg838i58i"))
              (patches (search-patches "hdf-eos5-build-shared.patch"
                                       "hdf-eos5-remove-gctp.patch"
                                       "hdf-eos5-fix-szip.patch"
                                       "hdf-eos5-fortrantests.patch"))))
    (native-inputs
     `(("gfortran" ,gfortran)))
    (build-system gnu-build-system)
    (inputs
     `(("hdf5" ,hdf5)
       ("zlib" ,zlib)
       ("gctp" ,gctp)))
    (arguments
     `(#:configure-flags '("--enable-install-include" "--enable-shared"
                           "CC=h5cc -Df2cFortran" "LIBS=-lgctp")
       #:parallel-tests? #f))
    (synopsis "HDF5-based data format for NASA's Earth Observing System")
    (description
     "HDF-EOS5 is a software library built on HDF5 to support the construction
of data structures used in NASA's Earth Observing System (Grid, Point and
Swath).")
    (home-page "http://www.hdfeos.org/software/library.php#HDF-EOS5")

    ;; Source files carry a permissive license header.
    (license (license:non-copyleft home-page))))

(define-public hdf5-parallel-openmpi
  (package (inherit hdf5)
    (name "hdf5-parallel-openmpi")
    (inputs
     `(("mpi" ,openmpi)
       ,@(package-inputs hdf5)))
    (arguments
     (substitute-keyword-arguments `(#:configure-flags '("--enable-parallel")
                                     ,@(package-arguments hdf5))
       ((#:phases phases)
        `(modify-phases ,phases
           (add-before 'check 'patch-tests
             (lambda _
               ;; OpenMPI's mpirun will exit with non-zero status if it
               ;; detects an "abnormal termination", i.e. any process not
               ;; calling MPI_Finalize().  Since the test is explicitely
               ;; avoiding MPI_Finalize so as not to have at_exit and thus
               ;; H5C_flush_cache from being called, mpirun will always
               ;; complain, so turn this test off.
               (substitute* "testpar/Makefile"
                 (("(^TEST_PROG_PARA.*)t_pflush1(.*)" front back)
                  (string-append front back "\n")))
               (substitute* "tools/h5diff/testph5diff.sh"
                 (("/bin/sh") (which "sh")))
               #t))))))
    (synopsis "Management suite for data with parallel IO support")))

(define-public h5check
  (package
    (name "h5check")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.hdfgroup.org/ftp/HDF5/tools/"
                           "h5check/src/h5check-" version ".tar.gz"))
       (sha256
        (base32
         "1gm76jbwhz9adbxgn14zx8cj33dmjdr2g5xcy0m9c2gakp8w59kj"))))
    (build-system gnu-build-system)
    (inputs `(("hdf5" ,hdf5)))                 ;h5cc for tests
    (home-page "https://www.hdfgroup.org/products/hdf5_tools/h5check.html")
    (synopsis "HDF5 format checker")
    (description "@code{h5check} is a validation tool for verifying that an
HDF5 file is encoded according to the HDF File Format Specification.")
    (license (license:x11-style "file://COPYING"))))

(define-public itpp
  (package
    (name "itpp")
    (version "4.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/itpp/itpp/"
                                  version "/itpp-"
                                  version ".tar.gz"))
       (sha256
        (base32
         "14ddy2xnb6sgp4hiax9v5sv4pr4l4dd4ps76nfha3nrpr1ikhcqm"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f)) ; Tests require googletest *sources*
    (inputs `(("lapack" ,lapack)
              ("fftw" ,fftw)))
    ;; FIXME: Even though the fonts are available dvips complains:
    ;; "Font cmmi10 not found; characters will be left blank."
    (native-inputs
     `(("texlive" ,texlive-tiny)
       ("ghostscript" ,ghostscript)
       ("doxygen" ,doxygen)))
    (home-page "http://itpp.sourceforge.net")
    (synopsis "C++ library of maths, signal processing and communication classes")
    (description "IT++ is a C++ library of mathematical, signal processing and
communication classes and functions.  Its main use is in simulation of
communication systems and for performing research in the area of
communications.  The kernel of the library consists of generic vector and
matrix classes, and a set of accompanying routines.  Such a kernel makes IT++
similar to MATLAB, GNU Octave or SciPy.")
    (license license:gpl3+)))

(define-public netcdf
  (package
    (name "netcdf")
    (version "4.4.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "ftp://ftp.unidata.ucar.edu/pub/netcdf/"
                           "netcdf-" version ".tar.gz"))
       (sha256
        (base32
         "1blc7ik5yin7i0ls2kag0a9xjk12m0dzx6v1x88az3ras3scci2d"))
       (patches (search-patches "netcdf-date-time.patch"
                                "netcdf-tst_h_par.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("m4" ,m4)
       ("doxygen" ,doxygen)
       ("graphviz" ,graphviz)))
    (inputs
     `(("hdf4" ,hdf4-alt)
       ("hdf5" ,hdf5)
       ("zlib" ,zlib)
       ("libjpeg" ,libjpeg)))
    (arguments
     `(#:configure-flags '("--enable-doxygen" "--enable-dot" "--enable-hdf4")

       #:phases (modify-phases %standard-phases
         (add-before 'configure 'fix-source-date
           (lambda _
             ;; As we ${SOURCE_DATE_EPOCH} evaluates to "1" in the build
             ;; environment, `date -u -d ${SOURCE_DATE_EPOCH}` will evaluate
             ;; to '1st hour of the current day', and therefore makes the
             ;; package not reproducible.
             (substitute* "./configure"
               (("date -u -d \"\\$\\{SOURCE_DATE_EPOCH\\}\"")
                "date --date='@0'"))
             #t))
         (add-after 'configure 'patch-settings
           (lambda _
             ;; libnetcdf.settings contains the full filename of the compilers
             ;; used to build the library.  We truncate the hashes of those
             ;; filenames to avoid unnecessary references to the corresponding
             ;; store items.
             (substitute* "libnetcdf.settings"
               (("(/gnu/store/)([a-Z0-9]*)" all prefix hash)
                (string-append prefix (string-take hash 10) "...")))
             #t)))

       #:parallel-tests? #f))           ;various race conditions
    (home-page "http://www.unidata.ucar.edu/software/netcdf/")
    (synopsis "Library for scientific data")
    (description "NetCDF is an interface for scientific data access and a
software library that provides an implementation of the interface.  The netCDF
library defines a machine-independent format for representing scientific data.
Together, the interface, library, and format support the creation, access, and
sharing of scientific data.")
    (license (license:x11-style "file://COPYRIGHT"))))

(define-public netcdf-parallel-openmpi
  (package (inherit netcdf)
    (name "netcdf-parallel-openmpi")
    (inputs
     `(("mpi" ,openmpi)
       ,@(alist-replace "hdf5" (list hdf5-parallel-openmpi)
                        (package-inputs netcdf))))
    ;; TODO: Replace pkg-config references in nc-config with absolute references
    (arguments
     (substitute-keyword-arguments (package-arguments netcdf)
       ((#:configure-flags flags)
        `(cons* "CC=mpicc" "CXX=mpicxx"
                "--enable-parallel-tests"
                ;; Shared libraries not supported with parallel IO.
                "--disable-shared" "--with-pic"
                ,flags))))))

(define-public netcdf-fortran
  (package
    (name "netcdf-fortran")
    (version "4.4.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "ftp://ftp.unidata.ucar.edu/pub/netcdf/netcdf-fortran-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0xaxdcg1p83zmypwml3swsnr3ccn38inwldyr1l3wa4dbwbrblxj"))))
    (build-system gnu-build-system)
    (arguments
     `(#:parallel-tests? #f))
    (inputs
     `(("netcdf" ,netcdf)))
    (native-inputs
     `(("gfortran" ,gfortran)))
    (synopsis "Fortran interface for the netCDF library")
    (description (package-description netcdf))
    (home-page (package-home-page netcdf))
    (license (package-license netcdf))))

(define-public nlopt
  (package
    (name "nlopt")
    (version "2.4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://ab-initio.mit.edu/nlopt/nlopt-"
                                  version ".tar.gz"))
              (sha256
               (base32 "12cfkkhcdf4zmb6h7y6qvvdvqjs2xf9sjpa3rl3bq76px4yn76c0"))))
    (build-system gnu-build-system)
    (arguments
     `(;; Shared libraries are not built by default.  They are required to
       ;; build the Guile, Octave, and Python bindings.
       #:configure-flags '("--enable-shared")

       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-libnlopt-file-name
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Make sure the Scheme module refers to the library by its
             ;; absolute file name (we cannot do that from a snippet
             ;; because the expansion of @libdir@ contains
             ;; ${exec_prefix}.)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "swig/nlopt.scm.in"
                 (("libnlopt")
                  (string-append out "/lib/libnlopt")))
               #t))))))
    (inputs `(("guile" ,guile-2.0)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://ab-initio.mit.edu/wiki/")
    (synopsis "Library for nonlinear optimization")
    (description "NLopt is a library for nonlinear optimization, providing a
common interface for a number of different free optimization routines available
online as well as original implementations of various other algorithms.")
    (license license:lgpl2.1+)))

(define-public ipopt
  (package
    (name "ipopt")
    (version "3.12.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.coin-or.org/download/source/Ipopt/Ipopt-"
                    version".tgz"))
              (sha256
               (base32
                "09bk2hqy2vgi4yi76xng9zxakddwqy3wij9nx7wf2vfbxxpazrsk"))
              (modules '((guix build utils)))
              (snippet
               ;; Make sure we don't use the bundled software.
               '(delete-file-recursively "ThirdParty"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-after 'install 'add--L-flags-in-ipopt.pc
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      ;; The '.pc' file lists '-llapack -lblas' in "Libs";
                      ;; move it to "Libs.private" where it belongs, and add a
                      ;; '-L' flag for LAPACK.
                      (let ((out    (assoc-ref outputs "out"))
                            (lapack (assoc-ref inputs "lapack")))
                        (substitute* (string-append out "/lib/pkgconfig/"
                                                    "ipopt.pc")
                          (("Libs: (.*)-llapack -lblas(.*)$" _ before after)
                           (string-append "Libs: " before " " after "\n"
                                          "Libs.private: " before
                                          "-L" lapack "/lib -llapack -lblas "
                                          after "\n")))
                        #t))))))
    (native-inputs
     `(("gfortran" ,gfortran)))
    (inputs
     ;; TODO: Maybe add dependency on COIN-MUMPS, ASL, and HSL.
     `(("lapack" ,lapack)))                    ;for both libblas and liblapack
    (home-page "http://www.coin-or.org")
    (synopsis "Large-scale nonlinear optimizer")
    (description
     "The Interior Point Optimizer (IPOPT) is a software package for
large-scale nonlinear optimization.  It provides C++, C, and Fortran
interfaces.")
    (license license:epl1.0)))

(define-public ceres
  (package
    (name "ceres-solver")
    (version "1.13.0")
    (home-page "http://ceres-solver.org/")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page "ceres-solver-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1kbxgab3q1vgyq7hjqasr1lji4b2sgn7ss351amklkb3jyhr1x0x"))))
    (build-system cmake-build-system)
    (arguments
     ;; TODO: Build HTML user documentation and install separately.
     '(#:configure-flags '("-DBUILD_EXAMPLES=OFF"
                           "-DBUILD_SHARED_LIBS=ON")

       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'set-library-directory
                    (lambda _
                      ;; Install libraries to lib/, not lib64/.
                      (substitute* "internal/ceres/CMakeLists.txt"
                        (("set\\(LIB_SUFFIX \"64\"\\)")
                         "set(LIB_SUFFIX \"\")"))
                      #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("glog" ,glog)))                           ;for #include <glog/glog.h>
    (inputs
     `(("eigen" ,eigen)
       ("blas" ,openblas)
       ("lapack" ,lapack)
       ("suitesparse" ,suitesparse)
       ("gflags" ,gflags)))
    (synopsis "C++ library for solving large optimization problems")
    (description
     "Ceres Solver is a C++ library for modeling and solving large,
complicated optimization problems.  It is a feature rich, mature and
performant library which has been used in production since 2010.  Ceres Solver
can solve two kinds of problems:
@enumerate
@item non-linear least squares problems with bounds constraints;
@item general unconstrained optimization problems.
@end enumerate\n")
    (license license:bsd-3)))

;; For a fully featured Octave, users  are strongly recommended also to install
;; the following packages: less, ghostscript, gnuplot.
(define-public octave
  (package
    (name "octave")
    (version "4.2.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/octave/octave-"
                          version ".tar.lz"))
      (sha256
       (base32
        "0pkkz1vazsh7ipffb09q0nc2jgx6q27pkkngygjij6jrpcly5zsp"))))
    (build-system gnu-build-system)
    (inputs
     `(("lapack" ,lapack)
       ("readline" ,readline)
       ("gl2ps" ,gl2ps)
       ("glpk" ,glpk)
       ("fftw" ,fftw)
       ("fftwf" ,fftwf)
       ("arpack" ,arpack-ng)
       ("pcre" ,pcre)
       ("fltk" ,fltk)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("hdf5" ,hdf5)
       ("libxft" ,libxft)
       ("mesa" ,mesa)
       ("glu" ,glu)
       ("zlib" ,zlib)
       ("curl" ,curl)
       ("texinfo" ,texinfo)
       ("graphicsmagick" ,graphicsmagick)))
    (native-inputs
     `(("lzip" ,lzip)
       ("gfortran" ,gfortran)
       ("pkg-config" ,pkg-config)
       ("perl" ,perl)
       ;; The following inputs are not actually used in the build process.
       ;; However, the ./configure gratuitously tests for their existence and
       ;; assumes that programs not present at build time are also not, and
       ;; can never be, available at run time!  If these inputs are therefore
       ;; not present, support for them will be built out.  However, Octave
       ;; will still run without them, albeit without the features they
       ;; provide.
       ("less" ,less)
       ("ghostscript" ,ghostscript)
       ("gnuplot" ,gnuplot)))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-shell="
                            (assoc-ref %build-inputs "bash")
                            "/bin/sh"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'configure-makeinfo
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "libinterp/corefcn/help.cc"
               (("Vmakeinfo_program = \"makeinfo\"")
                (string-append "Vmakeinfo_program = \""
                               (assoc-ref inputs "texinfo")
                               "/bin/makeinfo\"")))
             #t)))))
    (home-page "https://www.gnu.org/software/octave/")
    (synopsis "High-level language for numerical computation")
    (description "GNU Octave is a high-level interpreted language that is
specialized for numerical computations.  It can be used for both linear and
non-linear applications and it provides great support for visualizing results.
Work may be performed both at the interactive command-line as well as via
script files.")
    (license license:gpl3+)))

(define-public opencascade-oce
  (package
    (name "opencascade-oce")
    (version "0.17.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://github.com/tpaviot/oce/archive/OCE-"
               version
               ".tar.gz"))
        (patches (search-patches "opencascade-oce-glibc-2.26.patch"))
        (sha256
          (base32
            "0vpmnb0k5y2f7lpmwx9pg9yfq24zjvnsak5alzacncfm1hv9b6cd"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
        (list "-DOCE_TESTING:BOOL=ON"
              "-DOCE_USE_TCL_TEST_FRAMEWORK:BOOL=ON"
              "-DOCE_DRAW:BOOL=ON"
              (string-append "-DOCE_INSTALL_PREFIX:PATH="
                        (assoc-ref %outputs "out"))
              "-UCMAKE_INSTALL_RPATH")))
    (inputs
      `(("freetype" ,freetype)
        ("glu" ,glu)
        ("libxmu" ,libxmu)
        ("mesa" ,mesa)
        ("tcl" ,tcl)
        ("tk" ,tk)))
    (native-inputs
      `(("python" ,python-wrapper)))
    (home-page "https://github.com/tpaviot/oce")
    (synopsis "Libraries for 3D modeling and numerical simulation")
    (description
     "Open CASCADE is a set of libraries for the development of applications
dealing with 3D CAD data or requiring industrial 3D capabilities.  It includes
C++ class libraries providing services for 3D surface and solid modeling, CAD
data exchange, and visualization.  It is used for development of specialized
software dealing with 3D models in design (CAD), manufacturing (CAM),
numerical simulation (CAE), measurement equipment (CMM), and quality
control (CAQ) domains.

This is the ``Community Edition'' (OCE) of Open CASCADE, which gathers
patches, improvements, and experiments contributed by users over the official
Open CASCADE library.")
    (license (list license:lgpl2.1; OCE libraries, with an exception for the
                                  ; use of header files; see
                                  ; OCCT_LGPL_EXCEPTION.txt
                   license:public-domain; files
                                  ; src/Standard/Standard_StdAllocator.hxx and
                                  ; src/NCollection/NCollection_StdAllocator.hxx
                   license:expat; file src/OpenGl/OpenGl_glext.h
                   license:bsd-3)))); test framework gtest

(define-public gmsh
  (package
    (name "gmsh")
    (version "2.16.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://gmsh.info/src/gmsh-"
                          version "-source.tgz"))
      (sha256
       (base32 "1slf0bfkwrcgn6296wb4qhbk4ahz6i4wfb10hnim08x05vrylag8"))
      (modules '((guix build utils)))
      (snippet
       ;; Remove non-free METIS code
       '(delete-file-recursively "contrib/Metis"))))
    (build-system cmake-build-system)
    (propagated-inputs
     `(("fltk" ,fltk)
       ("gfortran" ,gfortran)
       ("gmp" ,gmp)
       ("hdf5" ,hdf5)
       ("lapack" ,lapack)
       ("mesa" ,mesa)
       ("glu" ,glu)
       ("opencascade-oce" ,opencascade-oce)
       ("libx11" ,libx11)
       ("libxext" ,libxext)))
    (inputs
     `(("fontconfig" ,fontconfig)
       ("libxft" ,libxft)))
    (arguments
     `(#:configure-flags `("-DENABLE_METIS:BOOL=OFF"
                           "-DENABLE_BUILD_SHARED:BOOL=ON"
                           "-DENABLE_BUILD_DYNAMIC:BOOL=ON")))
    (home-page "http://www.geuz.org/gmsh/")
    (synopsis "3D finite element grid generator")
    (description "Gmsh is a 3D finite element grid generator with a built-in
CAD engine and post-processor.  Its design goal is to provide a fast, light
and user-friendly meshing tool with parametric input and advanced
visualization capabilities.  Gmsh is built around four modules: geometry,
mesh, solver and post-processing.  The specification of any input to these
modules is done either interactively using the graphical user interface or in
ASCII text files using Gmsh's own scripting language.")
    (license license:gpl2+)))

(define-public maxflow
  (package
    (name "maxflow")
    (version "3.04")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/gerddie/maxflow.git")
                    (commit "42401fa54823d16b9da47716f04e5d9ef1605875")))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0rll38whw55h0vcjrrwdnh9ascvxby0ph7n1l0d12z17cg215kkb"))))
    (build-system cmake-build-system)
    (home-page "http://pub.ist.ac.at/~vnk/software.html")
    (synopsis "Library implementing Maxflow algorithm")
    (description "An implementation of the maxflow algorithm described in
@cite{An Experimental Comparison of Min-Cut/Max-Flow Algorithms for
Energy Minimization in Computer Vision.\n
Yuri Boykov and Vladimir Kolmogorov.\n
In IEEE Transactions on Pattern Analysis and Machine Intelligence,\n
September 2004}")
    (license license:gpl3+)))

(define-public petsc
  (package
    (name "petsc")
    (version "3.8.0")
    (source
     (origin
      (method url-fetch)
      ;; The *-lite-* tarball does not contain the *large* documentation
      (uri (string-append "http://ftp.mcs.anl.gov/pub/petsc/release-snapshots/"
                          "petsc-lite-" version ".tar.gz"))
      (sha256
       (base32 "1lajbk3c29hnh83v6cbmm3a8wv6bdykh0p70kwrr4vrnizalk88s"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("python" ,python-2)))
    (inputs
     `(("gfortran" ,gfortran)
       ("lapack" ,lapack)
       ("superlu" ,superlu)
       ;; leaving out hdf5 and fftw, as petsc expects them to be built with mpi
       ;; leaving out opengl, as configuration seems to only be for mac
       ))
    (arguments
     `(#:test-target "test"
       #:parallel-build? #f             ;build is parallel by default
       #:configure-flags
       `("--with-mpi=0"
         "--with-openmp=1"
         "--with-superlu=1"
         ,(string-append "--with-superlu-include="
                         (assoc-ref %build-inputs "superlu") "/include")
         ,(string-append "--with-superlu-lib="
                         (assoc-ref %build-inputs "superlu") "/lib/libsuperlu.a"))
       #:make-flags
       ;; Honor (parallel-job-count) for build.  Do not use --with-make-np,
       ;; whose value is dumped to $out/lib/petsc/conf/petscvariables.
       (list (format #f "MAKE_NP=~a" (parallel-job-count)))
       #:phases
       (modify-phases %standard-phases
        (replace 'configure
          ;; PETSc's configure script is actually a python script, so we can't
          ;; run it with bash.
          (lambda* (#:key outputs (configure-flags '())
                          #:allow-other-keys)
            (let* ((prefix (assoc-ref outputs "out"))
                   (flags `(,(string-append "--prefix=" prefix)
                            ,@configure-flags)))
              (format #t "build directory: ~s~%" (getcwd))
              (format #t "configure flags: ~s~%" flags)
              (zero? (apply system* "./configure" flags)))))
        (add-after 'configure 'clean-local-references
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (substitute* (find-files "." "^petsc(conf|machineinfo).h$")
                ;; Prevent build directory from leaking into compiled code
                (((getcwd)) out)
                ;; Scrub timestamp for reproducibility
                ((".*Libraries compiled on.*") ""))
              (substitute* (find-files "." "petscvariables")
                ;; Do not expose build machine characteristics, set to defaults.
                (("MAKE_NP = [:digit:]+") "MAKE_NP = 2")
                (("NPMAX = [:digit:]+") "NPMAX = 2"))
              #t)))
        (add-after 'install 'clean-install
          ;; Try to keep installed files from leaking build directory names.
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (substitute* (map (lambda (file)
                                  (string-append out "/lib/petsc/conf/" file))
                                '("petscvariables"))
                (((getcwd)) out))
              ;; Make compiler references point to the store
              (substitute* (string-append out "/lib/petsc/conf/petscvariables")
                (("= (gcc|g\\+\\+|gfortran)" _ compiler)
                 (string-append "= " (which compiler))))
              ;; PETSc installs some build logs, which aren't necessary.
              (for-each (lambda (file)
                          (let ((f (string-append out "/lib/petsc/conf/" file)))
                            (when (file-exists? f)
                              (delete-file f))))
                        '("configure.log" "make.log" "gmake.log"
                          "test.log" "error.log" "RDict.db"
                          "PETScBuildInternal.cmake"
                          ;; Once installed, should uninstall with Guix
                          "uninstall.py"))
              #t))))))
    (home-page "http://www.mcs.anl.gov/petsc")
    (synopsis "Library to solve PDEs")
    (description "PETSc, pronounced PET-see (the S is silent), is a suite of
data structures and routines for the scalable (parallel) solution of
scientific applications modeled by partial differential equations.")
    (license (license:non-copyleft
              "http://www.mcs.anl.gov/petsc/documentation/copyright.html"))))

(define-public petsc-complex
  (package (inherit petsc)
    (name "petsc-complex")
    (arguments
     (substitute-keyword-arguments (package-arguments petsc)
       ((#:configure-flags cf)
        `(cons "--with-scalar-type=complex" ,cf))))
    (synopsis "Library to solve PDEs (with complex scalars)")))

(define-public petsc-openmpi
  (package (inherit petsc)
    (name "petsc-openmpi")
    (inputs
     `(("openmpi" ,openmpi)
       ,@(package-inputs petsc)))
    (arguments
     (substitute-keyword-arguments (package-arguments petsc)
       ((#:configure-flags cf)
        ``("--with-mpiexec=mpirun"
           ,(string-append "--with-mpi-dir="
                           (assoc-ref %build-inputs "openmpi"))
           ,@(delete "--with-mpi=0" ,cf)))))
    (synopsis "Library to solve PDEs (with MPI support)")))

(define-public petsc-complex-openmpi
  (package (inherit petsc-complex)
    (name "petsc-complex-openmpi")
    (inputs
     `(("openmpi" ,openmpi)
       ,@(package-inputs petsc-complex)))
    (arguments
     (substitute-keyword-arguments (package-arguments petsc-complex)
       ((#:configure-flags cf)
        ``("--with-mpiexec=mpirun"
           ,(string-append "--with-mpi-dir="
                           (assoc-ref %build-inputs "openmpi"))
           ,@(delete "--with-mpi=0" ,cf)))))
    (synopsis "Library to solve PDEs (with complex scalars and MPI support)")))

(define-public slepc
  (package
    (name "slepc")
    (version "3.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://slepc.upv.es/download/distrib/slepc-"
                           version ".tar.gz"))
       (sha256
        (base32
         "04zd48p43rnvg68p6cp28zll0px5whglc5v0sc3s6vdj1v920z8y"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("python" ,python-2)))
    (inputs
     `(("arpack" ,arpack-ng)
       ("gfortran" ,gfortran)))
    (propagated-inputs
     `(("petsc" ,petsc)))
    (arguments
     `(#:parallel-build? #f             ;build is parallel by default
       #:configure-flags
       `(,(string-append "--with-arpack-dir="
                         (assoc-ref %build-inputs "arpack") "/lib"))
       #:make-flags                     ;honor (parallel-job-count)
       `(,(format #f "MAKE_NP=~a" (parallel-job-count)))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
          ;; configure is a python script, so we can't run it with bash.
          (lambda* (#:key inputs outputs (configure-flags '())
                    #:allow-other-keys)
            (let* ((prefix (assoc-ref outputs "out"))
                   (flags `(,(string-append "--prefix=" prefix)
                            ,@configure-flags)))
              (format #t "build directory: ~s~%" (getcwd))
              (format #t "configure flags: ~s~%" flags)
              (setenv "SLEPC_DIR" (getcwd))
              (setenv "PETSC_DIR" (assoc-ref inputs "petsc"))
              (apply invoke "./configure" flags))))
         (add-after 'install 'delete-doc
          ;; TODO: SLEPc installs HTML documentation alongside headers in
          ;; $out/include.  We'd like to move them to share/doc, but delete
          ;; them for now, as they are incomplete and installing the complete
          ;; documentation is difficult.
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out")))
              (for-each delete-file (find-files out "\\.html$")))))
         (add-after 'install 'clean-install
          ;; Clean up unnecessary build logs from installation.
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (for-each (lambda (file)
                          (let ((f (string-append out "/lib/slepc/conf/" file)))
                            (when (file-exists? f)
                              (delete-file f))))
                        '("configure.log" "make.log" "gmake.log"
                          "test.log" "error.log" "RDict.db"
                          "uninstall.py"))))))))
    (home-page "http://slepc.upv.es")
    (synopsis "Scalable library for eigenproblems")
    (description "SLEPc is a software library for the solution of large sparse
eigenproblems on parallel computers.  It can be used for the solution of
linear eigenvalue problems formulated in either standard or generalized form,
as well as other related problems such as the singular value decomposition.
The emphasis of the software is on methods and techniques appropriate for
problems in which the associated matrices are sparse, for example, those
arising after the discretization of partial differential equations.")
    (license license:bsd-2)))

(define-public slepc-complex
  (package (inherit slepc)
    (name "slepc-complex")
    (propagated-inputs
     `(("petsc" ,petsc-complex)
       ,@(alist-delete "petsc" (package-propagated-inputs slepc))))
    (synopsis "Scalable library for eigenproblems (with complex scalars)")))

(define-public slepc-openmpi
  (package (inherit slepc)
    (name "slepc-openmpi")
    (inputs
     `(("mpi" ,openmpi)
       ("arpack" ,arpack-ng-openmpi)
       ,@(alist-delete "arpack" (package-inputs slepc))))
    (propagated-inputs
     `(("petsc" ,petsc-openmpi)
       ,@(alist-delete "petsc" (package-propagated-inputs slepc))))
    (synopsis "Scalable library for eigenproblems (with MPI support)")))

(define-public slepc-complex-openmpi
  (package (inherit slepc-openmpi)
    (name "slepc-complex-openmpi")
    (propagated-inputs
     `(("petsc" ,petsc-complex-openmpi)
       ,@(alist-delete "petsc" (package-propagated-inputs slepc-openmpi))))
    (synopsis "Scalable library for eigenproblems (with complex scalars and MPI support)")))

(define-public mumps
  (package
    (name "mumps")
    (version "5.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://mumps.enseeiht.fr/MUMPS_"
                           version ".tar.gz"))
       (sha256
        (base32
         "0igyc1pfzxdhpbad3v3lb86ixkdbqa1a8gbs15b04r2294h2nabp"))
       (patches (search-patches "mumps-build-parallelism.patch"))))
    (build-system gnu-build-system)
    (inputs
     `(("fortran" ,gfortran)
       ;; These are required for linking against mumps, but we let the user
       ;; declare the dependency.
       ("blas" ,openblas)
       ("metis" ,metis)
       ("scotch" ,scotch)))
    (arguments
     `(#:modules ((ice-9 match)
                  (ice-9 popen)
                  (srfi srfi-1)
                  ,@%gnu-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
          (lambda* (#:key inputs #:allow-other-keys)
            (call-with-output-file "Makefile.inc"
              (lambda (port)
                (format port "
PLAT         =
LIBEXT       = .a
OUTC         = -o
OUTF         = -o
RM           = rm -f~:[
CC           = gcc
FC           = gfortran
FL           = gfortran
INCSEQ       = -I$(topdir)/libseq
LIBSEQ       = -L$(topdir)/libseq -lmpiseq
LIBSEQNEEDED = libseqneeded~;
CC           = mpicc
FC           = mpifort
FL           = mpifort~]
AR           = ar vr # rules require trailing space, ugh...
RANLIB       = ranlib
LIBBLAS      = -L~a -lopenblas~@[
SCALAP       = -L~a -lscalapack~]
LIBOTHERS    = -pthread
CDEFS        = -DAdd_
PIC          = -fPIC
OPTF         = -O2 -DALLOW_NON_INIT $(PIC)
OPTL         = -O2 $(PIC)
OPTC         = -O2 $(PIC)
INCS         = $(INCSEQ)
LIBS         = $(SCALAP) $(LIBSEQ)
LPORDDIR     = $(topdir)/PORD/lib
IPORD        = -I$(topdir)/PORD/include
LPORD        = -L$(LPORDDIR) -lpord
ORDERINGSF   = -Dpord~@[
METISDIR     = ~a
IMETIS       = -I$(METISDIR)/include
LMETIS       = -L$(METISDIR)/lib -lmetis
ORDERINGSF  += -Dmetis~]~@[~:{
SCOTCHDIR    = ~a
ISCOTCH      = -I$(SCOTCHDIR)/include
LSCOTCH      = -L$(SCOTCHDIR)/lib ~a-lesmumps -lscotch -lscotcherr
ORDERINGSF  += ~a~}~]
ORDERINGSC   = $(ORDERINGSF)
LORDERINGS   = $(LPORD) $(LMETIS) $(LSCOTCH)
IORDERINGSF  = $(ISCOTCH)
IORDERINGSC  = $(IPORD) $(IMETIS) $(ISCOTCH)"
                        (assoc-ref inputs "mpi")
                        (assoc-ref inputs "blas")
                        (assoc-ref inputs "scalapack")
                        (assoc-ref inputs "metis")
                        (match (list (assoc-ref inputs "pt-scotch")
                                     (assoc-ref inputs "scotch"))
                          ((#f #f)
                           #f)
                          ((#f scotch)
                           `((,scotch "" "-Dscotch")))
                          ((ptscotch _)
                           `((,ptscotch
                              "-lptesmumps -lptscotch -lptscotcherr "
                              "-Dptscotch")))))))))
         (replace 'build
          ;; By default only the d-precision library is built.  Make with "all"
          ;; target so that all precision libraries and examples are built.
          (lambda _
            (zero? (system* "make" "all"
                            (format #f "-j~a" (parallel-job-count))))))
         (replace 'check
          ;; Run the simple test drivers, which read test input from stdin:
          ;; from the "real" input for the single- and double-precision
          ;; testers, and from the "cmplx" input for complex-precision
          ;; testers.  The EXEC-PREFIX key is used by the mumps-openmpi
          ;; package to prefix execution with "mpirun".
          (lambda* (#:key (exec-prefix '()) #:allow-other-keys)
            (with-directory-excursion "examples"
              (every
               (lambda (prec type)
                 (let ((tester (apply open-pipe*
                                      `(,OPEN_WRITE
                                        ,@exec-prefix
                                        ,(string-append "./" prec
                                                        "simpletest"))))
                       (input  (open-input-file
                                (string-append "input_simpletest_" type))))
                   (begin
                     (dump-port input tester)
                     (close-port input)
                     (zero? (close-pipe tester)))))
               '("s" "d" "c" "z")
               '("real" "real" "cmplx" "cmplx")))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (libdir (string-append out "/lib")))
               (copy-recursively "lib" libdir)
               (copy-recursively "include" (string-append out "/include"))
               (when (file-exists? "libseq/libmpiseq.a")
                 (install-file "libseq/libmpiseq.a" libdir))
               #t))))))
    (home-page "http://mumps.enseeiht.fr")
    (synopsis "Multifrontal sparse direct solver")
    (description
     "MUMPS (MUltifrontal Massively Parallel sparse direct Solver) solves a
sparse system of linear equations A x = b using Guassian elimination.")
    (license license:cecill-c)))

(define-public mumps-metis
  (package (inherit mumps)
    (name "mumps-metis")
    (inputs
     (alist-delete "scotch" (package-inputs mumps)))))

(define-public mumps-openmpi
  (package (inherit mumps)
    (name "mumps-openmpi")
    (inputs
     `(("mpi" ,openmpi)
       ("scalapack" ,scalapack)
       ("pt-scotch" ,pt-scotch)
       ,@(alist-delete "scotch" (package-inputs mumps))))
    (arguments
     (substitute-keyword-arguments (package-arguments mumps)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace
            'check
            (lambda _
              ((assoc-ref ,phases 'check)
               #:exec-prefix '("mpirun" "-n" "2"))))))))
    (synopsis "Multifrontal sparse direct solver (with MPI)")))

(define-public mumps-metis-openmpi
  (package (inherit mumps-openmpi)
    (name "mumps-metis-openmpi")
    (inputs
     (alist-delete "pt-scotch" (package-inputs mumps-openmpi)))))

(define-public r-quadprog
  (package
    (name "r-quadprog")
    (version "1.5-5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "quadprog" version))
       (sha256
        (base32
         "0jg3r6abmhp8r9vkbhpx9ldjfw6vyl1m4c5vwlyjhk1mi03656fr"))))
    (build-system r-build-system)
    (native-inputs
     `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/quadprog")
    (synopsis "Functions to solve quadratic programming problems")
    (description
     "This package contains routines and documentation for solving quadratic
programming problems.")
    (license license:gpl3+)))

(define-public r-pracma
  (package
    (name "r-pracma")
    (version "2.1.4")
    (source (origin
      (method url-fetch)
      (uri (cran-uri "pracma" version))
      (sha256
        (base32 "1ygm81i7mqvh229dp9935djjyb120p3bqvaf4k572sa4q63fzjhc"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-quadprog" ,r-quadprog)))
    (home-page "https://cran.r-project.org/web/packages/pracma")
    (synopsis "Practical numerical math functions")
    (description "This package provides functions for numerical analysis and
linear algebra, numerical optimization, differential equations, plus some
special functions.  It uses Matlab function names where appropriate to simplify
porting.")
    (license license:gpl3+)))

(define-public superlu
  (package
    (name "superlu")
    (version "5.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://crd-legacy.lbl.gov/~xiaoye/SuperLU/"
                           "superlu_" version ".tar.gz"))
       (sha256
        (base32 "0qzlb7cd608q62kyppd0a8c65l03vrwqql6gsm465rky23b6dyr8"))
       (modules '((guix build utils)))
       (snippet
        ;; Replace the non-free implementation of MC64 with a stub adapted
        ;; from Debian
        '(begin
           (use-modules (ice-9 regex)
                        (ice-9 rdelim))
           (call-with-output-file "SRC/mc64ad.c"
             (lambda (port)
               (display "
#include <stdio.h>
#include <stdlib.h>
void mc64id_(int *a) {
  fprintf (stderr, \"SuperLU: non-free MC64 not available.  Aborting.\\n\");
  abort ();
}
void mc64ad_ (int *a, int *b, int *c, int *d, int *e, double *f, int *g,
              int *h, int *i, int *j, int *k, double *l, int *m, int *n) {
  fprintf (stderr, \"SuperLU: non-free MC64 not available.  Aborting.\\n\");
  abort ();
}\n" port)))
           ;; Remove the corresponding license verbiage.  MC64 license follows
           ;; a "------" line separator.
           (with-atomic-file-replacement "License.txt"
             (let ((rx (make-regexp "-{8}")))
               (lambda (in out)
                 (let loop ()
                   (let ((line (read-line in 'concat)))
                    (unless (regexp-exec rx line)
                      (display line out)
                      (loop)))))))))))
    (build-system cmake-build-system)
    (native-inputs
     `(("tcsh" ,tcsh)))
    (inputs
     `(("blas" ,openblas)
       ("gfortran" ,gfortran)))
    (arguments
     `(#:configure-flags '("-Denable_blaslib:BOOL=NO" ;do not use internal cblas
                           "-DTPL_BLAS_LIBRARIES=openblas"
                           "-DBUILD_SHARED_LIBS:BOOL=YES")))
    (home-page "http://crd-legacy.lbl.gov/~xiaoye/SuperLU/")
    (synopsis "Supernodal direct solver for sparse linear systems")
    (description
     "SuperLU is a general purpose library for the direct solution of large,
sparse, nonsymmetric systems of linear equations on high performance machines.
The library is written in C and is callable from either C or Fortran.  The
library routines perform an LU decomposition with partial pivoting and
triangular system solves through forward and back substitution.  The library
also provides threshold-based ILU factorization preconditioners.")
    (license (list license:bsd-3
                   license:gpl2+        ;EXAMPLE/*fgmr.c
                   (license:fsf-free "file://SRC/colamd.h")))))

(define-public superlu-dist
  (package
    (name "superlu-dist")
    (version "3.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://crd-legacy.lbl.gov/~xiaoye/SuperLU/"
                           "superlu_dist_" version ".tar.gz"))
       (sha256
        (base32 "1hnak09yxxp026blq8zhrl7685yip16svwngh1wysqxf8z48vzfj"))
              (modules '((guix build utils)))
       (snippet
        ;; Replace the non-free implementation of MC64 with a stub
        '(begin
           (use-modules (ice-9 regex)
                        (ice-9 rdelim))
           (call-with-output-file "SRC/mc64ad.c"
             (lambda (port)
               (display "
#include <stdio.h>
#include <stdlib.h>
void mc64id_(int *a) {
  fprintf (stderr, \"SuperLU_DIST: non-free MC64 not available.  Aborting.\\n\");
  abort ();
}
void mc64ad_ (int *a, int *b, int *c, int *d, int *e, double *f, int *g,
              int *h, int *i, int *j, int *k, double *l, int *m, int *n) {
  fprintf (stderr, \"SuperLU_DIST: non-free MC64 not available.  Aborting.\\n\");
  abort ();
}\n" port)))
           (delete-file "SRC/mc64ad.f.bak")
           (substitute* "SRC/util.c"    ;adjust default algorithm
             (("RowPerm[[:blank:]]*=[[:blank:]]*LargeDiag")
              "RowPerm = NOROWPERM"))))
       (patches (search-patches "superlu-dist-scotchmetis.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("tcsh" ,tcsh)))
    (inputs
     `(("gfortran" ,gfortran)))
    (propagated-inputs
     `(("openmpi" ,openmpi)             ;headers include MPI heades
       ("lapack" ,lapack)               ;required to link with output library
       ("pt-scotch" ,pt-scotch)))       ;same
    (arguments
     `(#:parallel-build? #f             ;race conditions using ar
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (call-with-output-file "make.inc"
               (lambda (port)
                 (format port "
PLAT        =
DSuperLUroot = ~a
DSUPERLULIB  = ~a/lib/libsuperlu_dist.a
BLASDEF     = -DUSE_VENDOR_BLAS
BLASLIB     = -L~a/lib -lblas
PARMETISLIB = -L~a/lib \
              -lptscotchparmetis -lptscotch -lptscotcherr -lptscotcherrexit \
              -lscotch -lscotcherr -lscotcherrexit
METISLIB    = -L~:*~a/lib \
              -lscotchmetis -lscotch -lscotcherr -lscotcherrexit
LIBS        = $(DSUPERLULIB) $(PARMETISLIB) $(METISLIB) $(BLASLIB)
ARCH        = ar
ARCHFLAGS   = cr
RANLIB      = ranlib
CC          = mpicc
PIC         = -fPIC
CFLAGS      = -O3 -g -DPRNTlevel=0 $(PIC)
NOOPTS      = -O0 -g $(PIC)
FORTRAN     = mpifort
FFLAGS      = -O2 -g $(PIC)
LOADER      = $(CC)
CDEFS       = -DAdd_"
                         (getcwd)
                         (assoc-ref outputs "out")
                         (assoc-ref inputs "lapack")
                         (assoc-ref inputs "pt-scotch"))))
             #t))
         (add-after 'unpack 'remove-broken-symlinks
           (lambda _
             (for-each delete-file
                       (find-files "MAKE_INC" "\\.#make\\..*"))
             #t))
         (add-before 'build 'create-install-directories
           (lambda* (#:key outputs #:allow-other-keys)
             (for-each
              (lambda (dir)
                (mkdir-p (string-append (assoc-ref outputs "out")
                                        "/" dir)))
              '("lib" "include"))
             #t))
         (replace 'check
           (lambda _
             (with-directory-excursion "EXAMPLE"
               (and
                (zero? (system* "mpirun" "-n" "2"
                                "./pddrive" "-r" "1" "-c" "2" "g20.rua"))
                (zero? (system* "mpirun" "-n" "2"
                                "./pzdrive" "-r" "1" "-c" "2" "cg20.cua"))))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Library is placed in lib during the build phase.  Copy over
             ;; headers to include.
             (let* ((out    (assoc-ref outputs "out"))
                    (incdir (string-append out "/include")))
               (for-each (lambda (file)
                           (let ((base (basename file)))
                             (format #t "installing `~a' to `~a'~%"
                                     base incdir)
                             (copy-file file
                                        (string-append incdir "/" base))))
                         (find-files "SRC" ".*\\.h$")))
             #t)))))
    (home-page (package-home-page superlu))
    (synopsis "Parallel supernodal direct solver")
    (description
     "SuperLU_DIST is a parallel extension to the serial SuperLU library.
It is targeted for distributed memory parallel machines.  SuperLU_DIST is
implemented in ANSI C, and MPI for communications.")
    (license license:bsd-3)))

(define-public scotch
  (package
    (name "scotch")
    (version "6.0.4")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://gforge.inria.fr/frs/download.php/34618/"
                          "scotch_" version ".tar.gz"))
      (sha256
       (base32 "1ir088mvrqggyqdkx9qfynmiaffqbyih5qfl5mga2nrlm1qlsgzm"))
      (patches (search-patches "scotch-test-threading.patch"
                               "pt-scotch-build-parallelism.patch"))))
    (build-system gnu-build-system)
    (inputs
     `(("zlib" ,zlib)
       ("flex" ,flex)
       ("bison" ,bison)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after
          'unpack 'chdir-to-src
          (lambda _ (chdir "src")))
         (replace
          'configure
          (lambda _
            (call-with-output-file "Makefile.inc"
              (lambda (port)
                (format port "
EXE =
LIB = .a
OBJ = .o
MAKE = make
AR = ar
ARFLAGS = -ruv
CAT = cat
CCS = gcc
CCP = mpicc
CCD = gcc
CPPFLAGS =~{ -D~a~}
CFLAGS = -O2 -g -fPIC $(CPPFLAGS)
LDFLAGS = -lz -lm -lrt -lpthread
CP = cp
LEX = flex -Pscotchyy -olex.yy.c
LN = ln
MKDIR = mkdir
MV = mv
RANLIB = ranlib
YACC = bison -pscotchyy -y -b y
"
                        '("COMMON_FILE_COMPRESS_GZ"
                          "COMMON_PTHREAD"
                          "COMMON_RANDOM_FIXED_SEED"
                          "INTSIZE64"             ;use 'long' instead of 'int'
                          ;; Prevents symbolc clashes with libesmumps
                          "SCOTCH_RENAME"
                          ;; XXX: Causes invalid frees in superlu-dist tests
                          ;; "SCOTCH_PTHREAD"
                          ;; "SCOTCH_PTHREAD_NUMBER=2"
                          "restrict=__restrict"))))))
         (add-after
          'build 'build-esmumps
          (lambda _
            (zero? (system* "make"
                            (format #f "-j~a" (parallel-job-count))
                            "esmumps"))))
         (replace
          'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (mkdir out)
              (zero? (system* "make"
                              (string-append "prefix=" out)
                              "install"))
              ;; esmumps files are not installed with the above
              (for-each (lambda (f)
                          (copy-file f (string-append out "/include/" f)))
                        (find-files "../include" ".*esmumps.h$"))
              (for-each (lambda (f)
                          (copy-file f (string-append out "/lib/" f)))
                        (find-files "../lib" "^lib.*esmumps.*"))))))))
    (home-page "http://www.labri.fr/perso/pelegrin/scotch/")
    (synopsis "Programs and libraries for graph algorithms")
    (description "SCOTCH is a set of programs and libraries which implement
the static mapping and sparse matrix reordering algorithms developed within
the SCOTCH project.  Its purpose is to apply graph theory, with a divide and
conquer approach, to scientific computing problems such as graph and mesh
partitioning, static mapping, and sparse matrix ordering, in application
domains ranging from structural mechanics to operating systems or
bio-chemistry.")
    ;; See LICENSE_en.txt
    (license license:cecill-c)))

(define-public scotch32
  ;; This is the 'INTSIZE32' variant, which uses 32-bit integers, as needed by
  ;; some applications.
  (package (inherit scotch)
    (name "scotch32")
    (arguments
     (substitute-keyword-arguments (package-arguments scotch)
       ((#:phases scotch-phases)
        `(modify-phases ,scotch-phases
          (replace
           'configure
           (lambda _
             (call-with-output-file "Makefile.inc"
               (lambda (port)
                 (format port "
EXE =
LIB = .a
OBJ = .o
MAKE = make
AR = ar
ARFLAGS = -ruv
CAT = cat
CCS = gcc
CCP = mpicc
CCD = gcc
CPPFLAGS =~{ -D~a~}
CFLAGS = -O2 -g -fPIC $(CPPFLAGS)
LDFLAGS = -lz -lm -lrt -lpthread
CP = cp
LEX = flex -Pscotchyy -olex.yy.c
LN = ln
MKDIR = mkdir
MV = mv
RANLIB = ranlib
YACC = bison -pscotchyy -y -b y
"
                        '("COMMON_FILE_COMPRESS_GZ"
                          "COMMON_PTHREAD"
                          "COMMON_RANDOM_FIXED_SEED"
                          "INTSIZE32"   ;use 32-bit integers.  See INSTALL.txt
                          ;; Prevents symbolc clashes with libesmumps
                          "SCOTCH_RENAME"
                          ;; XXX: Causes invalid frees in superlu-dist tests
                          ;; "SCOTCH_PTHREAD"
                          ;; "SCOTCH_PTHREAD_NUMBER=2"
                          "restrict=__restrict"))))))))))
    (synopsis
     "Programs and libraries for graph algorithms (32-bit integers)")))

(define-public pt-scotch
  (package (inherit scotch)
    (name "pt-scotch")
    (propagated-inputs
     `(("openmpi" ,openmpi)))           ;Headers include MPI headers
    (arguments
     (substitute-keyword-arguments (package-arguments scotch)
       ((#:phases scotch-phases)
        `(modify-phases ,scotch-phases
           (replace
            'build
            (lambda _
              (and
               (zero? (system* "make"
                               (format #f "-j~a" (parallel-job-count))
                               "ptscotch" "ptesmumps"))
               ;; Install the serial metis compatibility library
               (zero? (system* "make" "-C" "libscotchmetis" "install")))))
           (replace
            'check
            (lambda _ (zero? (system* "make" "ptcheck"))))))))
    (synopsis "Programs and libraries for graph algorithms (with MPI)")))

(define-public pt-scotch32
  (package (inherit scotch32)
    (name "pt-scotch32")
    (propagated-inputs
     `(("openmpi" ,openmpi)))                     ;headers include MPI headers
    (arguments
     (substitute-keyword-arguments (package-arguments scotch32)
       ((#:phases scotch32-phases)
        `(modify-phases ,scotch32-phases
           (replace 'build
             (lambda _
               (and
                (zero? (system* "make"
                                (format #f "-j~a" (parallel-job-count))
                                "ptscotch" "ptesmumps"))
                ;; Install the serial metis compatibility library
                (zero? (system* "make" "-C" "libscotchmetis" "install")))))
           (replace 'check
             (lambda _
               (zero? (system* "make" "ptcheck"))))))))
    (synopsis
     "Programs and libraries for graph algorithms (with MPI and 32-bit integers)")))

(define-public metis
  (package
    (name "metis")
    (version "5.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://glaros.dtc.umn.edu/gkhome/fetch/sw/metis/"
                           "metis-" version ".tar.gz"))
       (sha256
        (base32
         "1cjxgh41r8k6j029yxs8msp3z6lcnpm16g5pvckk35kc7zhfpykn"))))
    (build-system cmake-build-system)
    (inputs
     `(("blas" ,openblas)))
    (arguments
     `(#:tests? #f                      ;no tests
       #:configure-flags `("-DSHARED=ON"
                           ,(string-append "-DGKLIB_PATH=" (getcwd)
                                           "/metis-" ,version "/GKlib"))))
    (home-page "http://glaros.dtc.umn.edu/gkhome/metis/metis/overview")
    (synopsis "Graph partitioning and fill-reducing matrix ordering library")
    (description
     "METIS is a set of serial programs for partitioning graphs, partitioning
finite element meshes, and producing fill-reducing orderings for sparse
matrices.  The algorithms implemented in METIS are based on the multilevel
recursive-bisection, multilevel k-way, and multi-constraint partitioning
schemes.")
    (license license:asl2.0)))          ;As of version 5.0.3

(define-public p4est
  (package
    (name "p4est")
    (version "2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://p4est.github.io/release/p4est-"
                           version ".tar.gz"))
       (sha256
        (base32
         "16h267z256kxcxfjs390qqzv19hr58vrj4x8lndb7alnk2vca8n5"))))
    (build-system gnu-build-system)
    (inputs
     `(("fortran" ,gfortran)
       ("blas" ,openblas)
       ("lapack" ,lapack)
       ("zlib" ,zlib)))
    (arguments
     `(#:configure-flags `(,(string-append "BLAS_LIBS=-L"
                                           (assoc-ref %build-inputs "blas")
                                           " -lopenblas")
                           ,(string-append "LAPACK_LIBS=-L"
                                           (assoc-ref %build-inputs "lapack")
                                           " -llapack"))))
    (home-page "http://www.p4est.org")
    (synopsis "Adaptive mesh refinement on forests of octrees")
    (description
     "The p4est software library enables the dynamic management of a
collection of adaptive octrees, conveniently called a forest of octrees.
p4est is designed to work in parallel and scales to hundreds of thousands of
processor cores.")
    (license license:gpl2+)))

(define-public p4est-openmpi
  (package (inherit p4est)
    (name "p4est-openmpi")
    (inputs
     `(("mpi" ,openmpi)
       ,@(package-inputs p4est)))
    (arguments
     (substitute-keyword-arguments (package-arguments p4est)
       ((#:configure-flags cf)
        ``("--enable-mpi" ,@,cf))))
    (synopsis "Parallel adaptive mesh refinement on forests of octrees")))

(define-public gsegrafix
  (package
    (name "gsegrafix")
    (version "1.0.6")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/" name "/" name "-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1b13hvx063zv970y750bx41wpx6hwd5ngjhbdrna8w8yy5kmxcda"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("LDFLAGS=-lm")))
    (inputs
     `(("libgnomecanvas" ,libgnomecanvas)
       ("libbonoboui" ,libbonoboui)
       ("libgnomeui" ,libgnomeui)
       ("libgnomeprintui" ,libgnomeprintui)
       ("popt" ,popt)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://www.gnu.org/software/gsegrafix/")
    (synopsis "GNOME application to create scientific and engineering plots")
    (description  "GSEGrafix is an application which produces high-quality graphical
plots for science and engineering.  Plots are specified via simple ASCII
parameter files and data files and are presented in an anti-aliased GNOME
canvas.  The program supports rectangular two-dimensional plots, histograms,
polar-axis plots and three-dimensional plots.  Plots can be printed or saved
to BMP, JPEG or PNG image formats.")
    (license license:gpl3+)))

(define-public maxima
  (package
    (name "maxima")
    (version "5.41.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/maxima/Maxima-source/"
                           version "-source/" name "-" version ".tar.gz"))
       (sha256
        (base32
         "0x0n81z0s4pl8nwpf7ivlsbvsdphm9w42250g7qdkizl0132by6s"))
       (patches (search-patches "maxima-defsystem-mkdir.patch"))))
    (build-system gnu-build-system)
    (inputs
     `(("gcl" ,gcl)
       ("gnuplot" ,gnuplot)                       ;for plots
       ("tk" ,tk)))                               ;Tcl/Tk is used by 'xmaxima'
    (native-inputs
     `(("texinfo" ,texinfo)
       ("perl" ,perl)
       ("python" ,python)))
    (arguments
     `(#:configure-flags
       (list "--enable-gcl"
             (string-append "--with-posix-shell="
                            (assoc-ref %build-inputs "bash")
                            "/bin/sh")
             (string-append "--with-wish="
                            (assoc-ref %build-inputs "tk")
                            "/bin/wish"
                            (let ((v ,(package-version tk)))
                              (string-take v (string-index-right v #\.)))))
       ;; By default Maxima attempts to write temporary files to
       ;; '/tmp/nix-build-maxima-*', which won't exist at run time.
       ;; Work around that.
       #:make-flags (list "TMPDIR=/tmp")
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             (chmod "src/maxima" #o555)
             #t))
         ;; Make sure the doc and emacs files are found in the
         ;; standard location.  Also configure maxima to find gnuplot
         ;; without having it on the PATH.
         (add-after 'install 'post-install
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((gnuplot (assoc-ref inputs "gnuplot"))
                    (out (assoc-ref outputs "out"))
                    (datadir (string-append out "/share/maxima/" ,version))
                    (binutils (string-append (assoc-ref inputs "binutils")
                                             "/bin")))
               (with-directory-excursion out
                 (mkdir-p "share/emacs")
                 (mkdir-p "share/doc")
                 (symlink
                  (string-append datadir "/emacs/")
                  (string-append out "/share/emacs/site-lisp"))
                 (symlink
                  (string-append datadir "/doc/")
                  (string-append out "/share/doc/maxima"))
                 (with-atomic-file-replacement
                  (string-append datadir "/share/maxima-init.lisp")
                  (lambda (in out)
                    (format out "~a ~s~a~%"
                            "(setf $gnuplot_command "
                            (string-append gnuplot "/bin/gnuplot") ")")
                    (dump-port in out))))
               ;; Ensure that Maxima will have access to the GNU binutils
               ;; components at runtime.
               (wrap-program (string-append out "/bin/maxima")
                 `("PATH" prefix (,binutils))))
             #t)))))
    (home-page "http://maxima.sourceforge.net")
    (synopsis "Numeric and symbolic expression manipulation")
    (description "Maxima is a system for the manipulation of symbolic and
numerical expressions.  It yields high precision numeric results by using
exact fractions, arbitrary precision integers, and variable precision floating
point numbers.")
    ;; Some files are lgpl2.1+. Some are gpl2+.  Some explicitly state gpl1+.
    ;; Others simply say "GNU General Public License" without stating a
    ;; version (which implicitly means gpl1+).
    ;; At least one file (src/maxima.asd) says "version 2."
    ;; GPLv2 only is therefore the smallest subset.
    (license license:gpl2)))

(define-public wxmaxima
  (package
    (name "wxmaxima")
    (version "18.02.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/andrejv/" name "/archive"
                           "/Version-" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "03kr2rgfp4hcf3is8m8d8f9hj660c3xgrc50vrrfpixx4syh6wvj"))
       (patches
        (search-patches "wxmaxima-do-not-use-old-gnuplot-parameters.patch"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)))
    (inputs
     `(("wxwidgets" ,wxwidgets)
       ("maxima" ,maxima)
       ;; Runtime support.
       ("adwaita-icon-theme" ,adwaita-icon-theme)
       ("gtk+" ,gtk+)
       ("shared-mime-info" ,shared-mime-info)))
    (arguments
     `(#:tests? #f ; no check target
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (wrap-program (string-append (assoc-ref outputs "out")
                                          "/bin/wxmaxima")
               `("PATH" ":" prefix
                 (,(string-append (assoc-ref inputs "maxima")
                                  "/bin")))
               ;; For GtkFileChooserDialog.
               `("GSETTINGS_SCHEMA_DIR" =
                 (,(string-append (assoc-ref inputs "gtk+")
                                  "/share/glib-2.0/schemas")))
               `("XDG_DATA_DIRS" ":" prefix
                 (;; Needed by gdk-pixbuf to know supported icon formats.
                  ,(string-append
                    (assoc-ref inputs "shared-mime-info") "/share")
                  ;; The default icon theme of GTK+.
                  ,(string-append
                    (assoc-ref inputs "adwaita-icon-theme") "/share"))))
             #t)))))
    (home-page "https://andrejv.github.io/wxmaxima/")
    (synopsis "Graphical user interface for the Maxima computer algebra system")
    (description
     "wxMaxima is a graphical user interface for the Maxima computer algebra
system.  It eases the use of Maxima by making most of its commands available
through a menu system and by providing input dialogs for commands that require
more than one argument.  It also implements its own display engine that
outputs mathematical symbols directly instead of depicting them with ASCII
characters.

wxMaxima also features 2D and 3D inline plots, simple animations, mixing of
text and mathematical calculations to create documents, exporting of input and
output to TeX, and a browser for Maxima's manual including command index and
full text searching.")
    (license license:gpl2+)))

(define-public armadillo
  (package
    (name "armadillo")
    (version "7.800.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/arma/armadillo-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1qqzy7dp891j9v7062mv1599hdwr97vqzrd3j2fl8c3gmc00dmzg"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f)) ;no test target
    (inputs
     `(("openblas" ,openblas)
       ("lapack" ,lapack)
       ("arpack" ,arpack-ng)))
    (home-page "http://arma.sourceforge.net/")
    (synopsis "C++ linear algebra library")
    (description
     "Armadillo is a C++ linear algebra library, aiming towards a good balance
between speed and ease of use.  It is useful for algorithm development
directly in C++, or quick conversion of research code into production
environments.  It can be used for machine learning, pattern recognition,
signal processing, bioinformatics, statistics, econometrics, etc.  The library
provides efficient classes for vectors, matrices and cubes, as well as 150+
associated functions (eg. contiguous and non-contiguous submatrix views).")
    (license license:asl2.0)))

(define-public muparser
  ;; When switching download sites, muparser re-issued a 2.2.5 release with a
  ;; different hash. In order to make `guix package --upgrade` work correctly,
  ;; we set a Guix packaging revision.
  ;; When the next version of muparser is released, we can remove
  ;; UPSTREAM-VERSION and REVISION and use the plain VERSION.
  (let ((upstream-version "2.2.5")
        (revision "2"))
    (package
      (name "muparser")
      (version (string-append upstream-version "-" revision))
      (source
       (origin
         (method url-fetch)
         (uri (string-append "https://github.com/beltoforion/muparser/archive/v"
                             upstream-version ".tar.gz"))
         (file-name (string-append name "-" version ".tar.gz"))
         (sha256
          (base32
           "0277qsi5l23jsck1vhn383bmvc2n9l4a1dl5r9bf7hvjv9ayyrh6"))))
      (build-system gnu-build-system)
      (arguments
       `(#:configure-flags '("--enable-samples=no")
         #:tests? #f)) ;no "check" target
      (home-page "http://muparser.beltoforion.de/")
      (synopsis "Fast parser library for mathematical expressions")
      (description
       "muParser is an extensible high performance math parser library.  It is
based on transforming an expression into a bytecode and precalculating constant
parts of it.")
      (license license:expat))))

(define-public openblas
  (package
    (name "openblas")
    (version "0.2.20")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/xianyi/OpenBLAS/tarball/v"
                           version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1bd03c5xni0bla0wg1wba841b36b0sg13sjja955kn5xzvy4i61a"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       ;; DYNAMIC_ARCH is only supported on x86.  When it is disabled and no
       ;; TARGET is specified, OpenBLAS will tune itself to the build host, so
       ;; we need to disable substitutions.
       #:substitutable?
        ,(let ((system (or (%current-target-system) (%current-system))))
           (or (string-prefix? "x86_64" system)
               (string-prefix? "i686" system)
               (string-prefix? "mips" system)
               (string-prefix? "aarch64" system)))
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
             "SHELL=bash"
             ;; Build the library for all supported CPUs.  This allows
             ;; switching CPU targets at runtime with the environment variable
             ;; OPENBLAS_CORETYPE=<type>, where "type" is a supported CPU type.
             ;; Unfortunately, this is not supported on non-x86 architectures,
             ;; where it leads to failed builds.
             ,@(let ((system (or (%current-target-system) (%current-system))))
                 (cond
                  ((or (string-prefix? "x86_64" system)
                       (string-prefix? "i686" system))
                   '("DYNAMIC_ARCH=1"))
                  ;; On MIPS we force the "SICORTEX" TARGET, as for the other
                  ;; two available MIPS targets special extended instructions
                  ;; for Loongson cores are used.
                  ((string-prefix? "mips" system)
                   '("TARGET=SICORTEX"))
                  ;; On aarch64 force the generic 'armv8-a' target
                  ((string-prefix? "aarch64" system)
                   '("TARGET=ARMV8"))
                  (else '()))))
       ;; no configure script
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'set-extralib
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Get libgfortran found when building in utest.
             (setenv "FEXTRALIB"
                     (string-append "-L" (assoc-ref inputs "fortran-lib")
                                    "/lib"))
             #t)))))
    (inputs
     `(("fortran-lib" ,gfortran "lib")))
    (native-inputs
     `(("cunit" ,cunit)
       ("fortran" ,gfortran)
       ("perl" ,perl)))
    (home-page "http://www.openblas.net/")
    (synopsis "Optimized BLAS library based on GotoBLAS")
    (description
     "OpenBLAS is a BLAS library forked from the GotoBLAS2-1.13 BSD version.")
    (license license:bsd-3)))

(define* (make-blis implementation #:optional substitutable?)
  "Return a BLIS package with the given IMPLEMENTATION (see config/ in the
source tree for a list of implementations.)

SUBSTITUTABLE? determines whether the package is made available as a
substitute.

Currently the specialization must be selected at configure-time, but work is
underway to allow BLIS to select the right optimized kernels at run time:
<https://github.com/flame/blis/issues/129>."
  (package
    (name (if (string=? implementation "reference")
              "blis"
              (string-append "blis-" implementation)))
    (version "0.2.2")
    (home-page "https://github.com/flame/blis")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (sha256
               (base32
                "1wr79a50nm4abhw8w3sn96nmwp5mrzifcigk7khw9qcgyyyqayfh"))
              (file-name (git-file-name "blis" version))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"

       #:substitutable? ,substitutable?

       #:phases (modify-phases %standard-phases
                  (replace 'configure
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; This is a home-made 'configure' script.
                      (let ((out (assoc-ref outputs "out")))
                        (zero? (system* "./configure" "-p" out
                                        "-d" "opt"
                                        "--disable-static"
                                        "--enable-shared"
                                        "--enable-threading=openmp"

                                        ,implementation)))))
                  (add-before 'check 'show-test-output
                    (lambda _
                      ;; By default "make check" is silent.  Make it verbose.
                      (system "tail -F output.testsuite &")
                      #t)))))
    (synopsis "High-performance basic linear algebra (BLAS) routines")
    (description
     "BLIS is a portable software framework for instantiating high-performance
BLAS-like dense linear algebra libraries.  The framework was designed to
isolate essential kernels of computation that, when optimized, immediately
enable optimized implementations of most of its commonly used and
computationally intensive operations.  While BLIS exports a new BLAS-like API,
it also includes a BLAS compatibility layer which gives application developers
access to BLIS implementations via traditional BLAS routine calls.")
    (license license:bsd-3)))

(define-public blis
  ;; This is the "reference" implementation, which is the non-optimized but
  ;; portable variant (no assembly).
  (make-blis "reference" #t))

(define ignorance blis)

(define-syntax-rule (blis/x86_64 processor)
  "Expand to a package specialized for PROCESSOR."
  (package
    (inherit (make-blis processor))
    (supported-systems '("x86_64-linux"))))

(define-public blis-sandybridge
  ;; BLIS specialized for Sandy Bridge processors (launched 2011):
  ;; <http://ark.intel.com/products/codename/29900/Sandy-Bridge>.
  (blis/x86_64 "sandybridge"))

(define-public blis-haswell
  ;; BLIS specialized for Haswell processors (launched 2013):
  ;; <http://ark.intel.com/products/codename/42174/Haswell>.
  (blis/x86_64 "haswell"))

(define-public blis-knl
  ;; BLIS specialized for Knights Landing processor (launched 2016):
  ;; <http://ark.intel.com/products/series/92650/Intel-Xeon-Phi-x200-Product-Family>.
  (blis/x86_64 "knl"))


(define-public openlibm
  (package
    (name "openlibm")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/JuliaLang/openlibm/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "11czx2z7nh6dfpz45s3xl7v38hw36jxzxfvny454bk3if14pfakq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       ;; no configure script
       (modify-phases %standard-phases (delete 'configure))
       #:tests? #f)) ;the tests are part of the default target
    (home-page "http://openlibm.org/")
    (synopsis "Portable C mathematical library (libm)")
    (description
     "OpenLibm is an effort to have a high quality, portable, standalone C
mathematical library (libm).  It can be used standalone in applications and
programming language implementations.  The project was born out of a need to
have a good libm for the Julia programming language that worked consistently
across compilers and operating systems, and in 32-bit and 64-bit
environments.")
    ;; Each architecture has its own make target, and there is none for mips.
    (supported-systems (delete "mips64el-linux" %supported-systems))
    ;; See LICENSE.md for details.
    (license (list license:expat
                   license:isc
                   license:bsd-2
                   license:public-domain
                   license:lgpl2.1+))))

(define-public openspecfun
  (package
    (name "openspecfun")
    (version "0.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/JuliaLang/openspecfun/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1rs1bv8jq751fv9vq79890wqf9xlbjc7lvz3ighzyfczbyjcf18m"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ; no "check" target
       #:make-flags
       (list (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))         ; no configure script
    (inputs
     `(("fortran" ,gfortran)))
    (home-page "https://github.com/JuliaLang/openspecfun")
    (synopsis "Collection of special mathematical functions")
    (description
     "Openspecfun provides AMOS and Faddeeva.  AMOS (from Netlib) is a
portable package for Bessel Functions of a Complex Argument and Nonnegative
Order; it contains subroutines for computing Bessel functions and Airy
functions.  Faddeeva allows computing the various error functions of arbitrary
complex arguments (Faddeeva function, error function, complementary error
function, scaled complementary error function, imaginary error function, and
Dawson function); given these, one can also easily compute Voigt functions,
Fresnel integrals, and similar related functions as well.")
    ;; Faddeeva is released under the Expat license; AMOS is included as
    ;; public domain software.
    (license (list license:expat license:public-domain))))

(define-public suitesparse
  (package
    (name "suitesparse")
    (version "4.5.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://faculty.cse.tamu.edu/davis/SuiteSparse/SuiteSparse-"
             version ".tar.gz"))
       (sha256
        (base32
         "1dnr6pmjzc2qmbkmb4shigx1l74ilf6abn7svyd6brxgvph8vadr"))
       (modules '((guix build utils)))
       (snippet
        ;; Remove bundled metis source
        '(delete-file-recursively "metis-5.1.0"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f  ;no "check" target
       #:make-flags
       (list "CC=gcc"
             "BLAS=-lblas"
             "TBB=-ltbb"
             "MY_METIS_LIB=-lmetis"
             (string-append "INSTALL_LIB="
                            (assoc-ref %outputs "out") "/lib")
             (string-append "INSTALL_INCLUDE="
                            (assoc-ref %outputs "out") "/include")
             "library")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))         ;no configure script
    (inputs
     `(("tbb" ,tbb)
       ("lapack" ,lapack)
       ("metis" ,metis)))
    (home-page "http://faculty.cse.tamu.edu/davis/suitesparse.html")
    (synopsis "Suite of sparse matrix software")
    (description
     "SuiteSparse is a suite of sparse matrix algorithms, including: UMFPACK,
multifrontal LU factorization; CHOLMOD, supernodal Cholesky; SPQR,
multifrontal QR; KLU and BTF, sparse LU factorization, well-suited for circuit
simulation; ordering methods (AMD, CAMD, COLAMD, and CCOLAMD); CSparse and
CXSparse, a concise sparse Cholesky factorization package; and many other
packages.")
    ;; LGPLv2.1+:
    ;;   AMD, CAMD, BTF, COLAMD, CCOLAMD, CSparse, CXSparse, KLU, LDL
    ;; GPLv2+:
    ;;  GPUQREngine, RBio, SuiteSparse_GPURuntime, SuiteSparseQR, UMFPACK
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public atlas
  (package
    (name "atlas")
    (version "3.10.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/math-atlas/Stable/"
                                  version "/atlas" version ".tar.bz2"))
              (sha256
               (base32
                "1dyjlq3fiparvm8ypwk6rsmjzmnwk81l88gkishphpvc79ryp216"))))
    (build-system gnu-build-system)
    (home-page "http://math-atlas.sourceforge.net/")
    (inputs `(("gfortran" ,gfortran)
              ("lapack-tar" ,(package-source lapack))))
    (outputs '("out" "doc"))
    ;; For the moment we drop support for MIPS at it fails to compile. See
    ;; https://lists.gnu.org/archive/html/guix-devel/2014-11/msg00516.html
    (supported-systems (delete "mips64el-linux" %supported-systems))
    (arguments
     `(#:parallel-build? #f
       #:parallel-tests? #f

       ;; ATLAS tunes itself for the machine it is built on, as explained at
       ;; <http://lists.gnu.org/archive/html/guix-devel/2014-10/msg00305.html>.
       ;; For this reason, we want users to build it locally instead of using
       ;; substitutes.
       #:substitutable? #f

       #:modules ((srfi srfi-26)
                  (srfi srfi-1)
                  (guix build gnu-build-system)
                  (guix build utils))
       #:configure-flags
       `(;; Generate position independent code suitable for dynamic libraries
         ;; and use WALL timer to get more accurate timing.
         "-Fa" "alg" "-fPIC" "-D" "c" "-DWALL"
         ;; Set word width.
         "-b"
         ,,(if (string-match "64" (%current-system))
               "64"
               "32")
         ;; Disable parallel build as it gives errors: atlas_pthread.h is
         ;; needed to compile C files before it is generated.
         "-Ss" "pmake" "make -j 1"
         ;; Probe is failing for MIPS.  We therefore define the system
         ;; architecture explicitly by setting (-A) MACHINETYPE = 49
         ;; 'MIPSR1xK' and (-V) ISA = 1 'none'.
         ,,@(if (string-prefix? "mips" (%current-system))
              (list "-A" "49" "-V" "1")
              (list))
         ;; Generate shared libraries.
         "--shared"
         ;; Build a full LAPACK library.
         ,(string-append "--with-netlib-lapack-tarfile="
                         (assoc-ref %build-inputs "lapack-tar")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-doc
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let ((doc (string-append (assoc-ref outputs "doc")
                                       "/share/doc/atlas")))
               (mkdir-p doc)
               (fold (lambda (file previous)
                       (and previous (zero? (system* "cp" file doc))))
                     #t (find-files "../ATLAS/doc" ".*")))))
         (add-after 'check 'check-pt
           (lambda _ (zero? (system* "make" "ptcheck"))))
         ;; Fix files required to run configure.
         (add-before 'configure 'fix-/bin/sh
           (lambda _
             ;; Use `sh', not `/bin/sh'.
             (substitute* (find-files "." "Makefile|configure|SpewMakeInc\\.c")
               (("/bin/sh")
                "sh"))))
         ;; Fix /bin/sh in generated make files.
         (add-after 'configure 'fix-/bin/sh-in-generated-files
           (lambda _
             (substitute* (find-files "." "^[Mm]ake\\.inc.*")
               (("/bin/sh")
                "sh"))))
         ;; ATLAS configure program does not accepts the default flags
         ;; passed by the 'gnu-build-system'.
         (replace 'configure
           (lambda* (#:key native-inputs inputs outputs
                           (configure-flags '())
                           #:allow-other-keys #:rest args)
             (let* ((prefix     (assoc-ref outputs "out"))
                    (bash       (or (and=> (assoc-ref
                                            (or native-inputs inputs) "bash")
                                           (cut string-append <> "/bin/bash"))
                                    "/bin/sh"))
                    (flags      `(,(string-append "--prefix=" prefix)
                                  ,@configure-flags))
                    (abs-srcdir (getcwd))
                    (srcdir     (string-append "../" (basename abs-srcdir))))
               (format #t "source directory: ~s (relative from build: ~s)~%"
                       abs-srcdir srcdir)
               (mkdir "../build")
               (chdir "../build")
               (format #t "build directory: ~s~%" (getcwd))
               (format #t "configure flags: ~s~%" flags)
               (zero? (apply system* bash
                             (string-append srcdir "/configure")
                             flags))))))))
    (synopsis "Automatically Tuned Linear Algebra Software")
    (description
     "ATLAS is an automatically tuned linear algebra software library
providing C and Fortran77 interfaces to a portably efficient BLAS
implementation, as well as a few routines from LAPACK.

Optimization occurs at build time.  For this reason, the library is built on
the machine where it is installed, without resorting to pre-built substitutes.

Before building the library, CPU throttling should be disabled.  This can be
done in the BIOS, or, on GNU/Linux, with the following command:

@example
# cpupower --governor performance
@end example

Failure to do so will result in a library with poor performance.")
    (license license:bsd-3)))

(define-public glm
  (package
    (name "glm")
    (version "0.9.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/ogl-math/glm-" version
                           "/glm-" version ".zip"))
       (sha256
        (base32
         "1cnjmi033a16a95v6xfkr1bvfmkd26hzdjka8j1819hgn5b1nr8l"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "http://glm.g-truc.net")
    (synopsis "OpenGL Mathematics library")
    (description "OpenGL Mathematics (GLM) is a header-only C++ mathematics
library for graphics software based on the OpenGL Shading Language (GLSL)
specifications.")
    (license license:expat)))

(define-public lpsolve
  (package
    (name "lpsolve")
    (version "5.5.2.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/lpsolve/lpsolve/" version
                          "/lp_solve_" version "_source.tar.gz"))
      (sha256
       (base32
        "12pj1idjz31r7c2mb5w03vy1cmvycvbkx9z29s40qdmkp1i7q6i0"))
      (modules '((guix build utils)))
      (snippet
       '(substitute* (list "lp_solve/ccc" "lpsolve55/ccc")
          (("^c=cc") "c=gcc")
          ;; Pretend to be on a 64 bit platform to obtain a common directory
          ;; name for the build results on all architectures; nothing else
          ;; seems to depend on it.
          (("^PLATFORM=.*$") "PLATFORM=ux64\n")

          ;; The check for 'isnan' as it is written fails with
          ;; "non-floating-point argument in call to function
          ;; ‘__builtin_isnan’", which leads to the 'NOISNAN' cpp macro
          ;; definition, which in turn leads to bad things.  Fix the feature
          ;; test.
          (("isnan\\(0\\)") "isnan(0.)")))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no check target
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (replace 'build
           (lambda _
             (with-directory-excursion "lpsolve55"
               (invoke "bash" "ccc"))
             (with-directory-excursion "lp_solve"
               (invoke "bash" "ccc"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lib (string-append out "/lib"))
                    ;; This is where LibreOffice expects to find the header
                    ;; files, and where they are installed by Debian.
                    (include (string-append out "/include/lpsolve")))
               (install-file "lpsolve55/bin/ux64/liblpsolve55.a" lib)
               (install-file "lpsolve55/bin/ux64/liblpsolve55.so" lib)
               (install-file "lp_solve/bin/ux64/lp_solve" bin)

               ;; Install a subset of the header files as on Debian
               ;; (plus lp_bit.h, which matches the regular expression).
               (for-each (lambda (name)
                           (install-file name include))
                         (find-files "." "lp_[HMSa-z].*\\.h$"))
               (with-directory-excursion "shared"
                 (for-each (lambda (name)
                             (install-file name include))
                           (find-files "." "\\.h$")))
               #t))))))
    (home-page "http://lpsolve.sourceforge.net/")
    (synopsis "Mixed integer linear programming (MILP) solver")
    (description
     "lp_solve is a mixed integer linear programming solver based on the
revised simplex and the branch-and-bound methods.")
    (license license:lgpl2.1+)))

(define-public dealii
  (package
    (name "dealii")
    (version "8.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/dealii/dealii/releases/"
                           "download/v" version "/dealii-" version ".tar.gz"))
       (sha256
        (base32
         "1bh9rsmkrg0zi70n27b11djmac9lximghsiy7mg7w7x544n82gnk"))
       (modules '((guix build utils)))
       (snippet
        ;; Remove bundled sources: UMFPACK, TBB, muParser, and boost
        '(delete-file-recursively "bundled"))))
    (build-system cmake-build-system)
    (inputs
     `(("tbb" ,tbb)
       ("zlib" ,zlib)
       ("boost" ,boost)
       ("p4est" ,p4est)
       ("blas" ,openblas)
       ("lapack" ,lapack)
       ("arpack" ,arpack-ng)
       ("muparser" ,muparser)
       ("gfortran" ,gfortran)
       ("suitesparse" ,suitesparse)))   ;for UMFPACK
    (arguments
     `(#:build-type "DebugRelease" ;only supports Release, Debug, or DebugRelease
       #:configure-flags
       ;; Work around a bug in libsuitesparseconfig linking
       ;; see https://github.com/dealii/dealii/issues/4745
       '("-DCMAKE_POSITION_INDEPENDENT_CODE:BOOL=ON")))
    (home-page "https://www.dealii.org")
    (synopsis "Finite element library")
    (description
     "Deal.II is a C++ program library targeted at the computational solution
of partial differential equations using adaptive finite elements.  The main
aim of deal.II is to enable rapid development of modern finite element codes,
using among other aspects adaptive meshes and a wide array of tools often used
in finite element programs.")
    (license license:lgpl2.1+)))

(define-public dealii-openmpi
  (package (inherit dealii)
    (name "dealii-openmpi")
    (inputs
     `(("mpi" ,openmpi)
       ;;Supported only with MPI:
       ("p4est" ,p4est-openmpi)
       ("petsc" ,petsc-openmpi)
       ("slepc" ,slepc-openmpi)
       ("metis" ,metis)               ;for MUMPS
       ("scalapack" ,scalapack)       ;for MUMPS
       ("mumps" ,mumps-metis-openmpi) ;configure supports only metis orderings
       ("arpack" ,arpack-ng-openmpi)
       ,@(fold alist-delete (package-inputs dealii)
               '("p4est" "arpack"))))
    (arguments
     (substitute-keyword-arguments (package-arguments dealii)
       ((#:configure-flags cf)
        ``("-DMPI_C_COMPILER=mpicc"
           "-DMPI_CXX_COMPILER=mpicxx"
           "-DMPI_Fortran_COMPILER=mpifort"
           ,@,cf))))
    (synopsis "Finite element library (with MPI support)")))

(define-public flann
  (package
    (name "flann")
    (version "1.8.4")
    (source
      (origin
        (method url-fetch)
        (uri
          (string-append
            "http://www.cs.ubc.ca/research/flann/uploads/FLANN/flann-"
            version "-src.zip"))
        (sha256
          (base32
            "022w8hph7bli5zbpnk3z1qh1c2sl5hm8fw2ccim651ynn0hr7fyz"))))
    (build-system cmake-build-system)
    (outputs '("out"
               "octave"))                  ;46 MiB .mex file that pulls Octave
    (native-inputs
     `(("unzip" ,unzip)))
    (inputs
     `(("hdf5" ,hdf5)
       ("octave" ,octave)
       ("python" ,python-2) ; print syntax
       ;; ("python2-numpy" ,python2-numpy) ; only required for the tests
       ("zlib" ,zlib)))
    (arguments
     `(;; The 'share/flann/octave' contains a .mex file, which is an ELF file
       ;; taken 46 MiB unstripped, and 6 MiB stripped.
       #:strip-directories '("lib" "lib64" "libexec"
                             "bin" "sbin" "share/flann/octave")

       ;; Save 12 MiB by not installing .a files.  Passing
       ;; '-DBUILD_STATIC_LIBS=OFF' has no effect.
       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'set-octave-directory
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; Install the .mex file in the "octave" output.
                      (let ((out (assoc-ref outputs "octave")))
                        (substitute* "src/matlab/CMakeLists.txt"
                          (("share/flann/octave")
                           (string-append out "/share/flann/octave")))
                        #t)))
                  (add-after 'install 'remove-static-libraries
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (lib (string-append out "/lib")))
                        (for-each delete-file
                                  (find-files lib "\\.a$"))
                        #t))))

       #:tests? #f)) ; The test data are downloaded from the Internet.
    (home-page "http://www.cs.ubc.ca/research/flann/")
    (synopsis "Library for approximate nearest neighbors computation")
    (description "FLANN is a library for performing fast approximate
nearest neighbor searches in high dimensional spaces.  It implements a
collection of algorithms and a system for automatically choosing the best
algorithm and optimum parameters depending on the dataset.

FLANN is written in C++ and contains bindings for C, Octave and Python.")
    (license (license:non-copyleft "file://COPYING"
                                "See COPYING in the distribution."))))

(define-public wcalc
  (package
    (name "wcalc")
    (version "2.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://sourceforge/w-calc/Wcalc/" version "/"
                            "wcalc-" version ".tar.bz2"))
        (sha256
          (base32
            "1vi8dl6rccqiq1apmpwawyg2ywx6a1ic1d3cvkf2hlwk1z11fb0f"))))
    (build-system gnu-build-system)
    (inputs
     `(("mpfr" ,mpfr)
       ("readline" ,readline)))
    (home-page "http://w-calc.sourceforge.net/index.php")
    (synopsis "Flexible command-line scientific calculator")
    (description "Wcalc is a very capable calculator.  It has standard functions
(sin, asin, and sinh for example, in either radians or degrees), many
pre-defined constants (pi, e, c, etc.), support for using variables, \"active\"
variables, a command history, hex/octal/binary input and output, unit
conversions, embedded comments, and an expandable expression entry field.  It
evaluates expressions using the standard order of operations.")
    (license license:gpl2+)))

(define-public xaos
  (package
    (name "xaos")
    (version "3.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/xaos/XaoS/" version
                                  "/xaos-" version ".tar.gz"))
              (sha256
               (base32
                "15cd1cx1dyygw6g2nhjqq3bsfdj8sj8m4va9n75i0f3ryww3x7wq"))))
    (build-system gnu-build-system)
    (native-inputs `(("gettext" ,gettext-minimal)))
    (inputs `(("libx11" ,libx11)
              ("zlib" ,zlib)
              ("libpng" ,libpng)
              ("gsl" ,gsl)))
    (arguments
     `(#:tests? #f ;no "check" target
       #:make-flags '("LOCALEDIR=$DATAROOTDIR/locale")))
    (synopsis "Real-time fractal zoomer")
    (description "GNU XaoS is a graphical program that generates fractal
patterns and allows you to zoom in and out of them infinitely in a fluid,
continuous manner.  It also includes tutorials that help to explain how fractals
are built.  It can generate many different fractal types such as the Mandelbrot
set.")
    (home-page "https://www.gnu.org/software/xaos/")
    (license license:gpl2+)))

(define-public hypre
  (package
    (name "hypre")
    (version "2.11.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/LLNL/hypre/archive/"
                                  "v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0q69ia0jivzcr8p049dn3mg8yjpn6nwq4sw9iqac8vr63vi54l6m"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Remove use of __DATE__ and __TIME__ for reproducibility;
                  ;; substitute the tarball creation time.
                  (substitute* "src/utilities/HYPRE_utilities.h"
                    (("Date Compiled: .*$")
                     "Date Compiled: Mar 28 2016 20:19:59 +0000\"\n"))
                  #t))))
    (build-system gnu-build-system)
    (outputs '("out"                    ;6.1 MiB of headers and libraries
               "doc"))                  ;4.8 MiB of documentation
    (native-inputs
     `(("doc++" ,doc++)
       ("netpbm" ,netpbm)
       ("texlive" ,texlive)             ;full package required for fonts
       ("ghostscript" ,ghostscript)))
    (inputs
     `(("blas" ,openblas)
       ("lapack" ,lapack)))
    (arguments
     `(#:modules ((srfi srfi-1)
                  ,@%gnu-build-system-modules)
       #:configure-flags '("--enable-shared"
                           "--disable-fortran"
                           "--without-MPI"
                           "--with-openmp"
                           "--with-fei"
                           "--with-lapack"
                           "--with-blas")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir-src
           (lambda _ (chdir "src")))
         (replace 'configure
           (lambda* (#:key build target configure-flags
                           #:allow-other-keys #:rest args)
             (let* ((configure (assoc-ref %standard-phases 'configure)))
               (apply configure
                      (append args
                              (list #:configure-flags
                                    (cons (string-append
                                           "--host=" (or target build))
                                          configure-flags)))))))
         (add-after 'build 'build-docs
           (lambda _
             (zero? (system* "make" "-Cdocs" "pdf" "html"))))
         (replace 'check
           (lambda _
             (setenv "LD_LIBRARY_PATH" (string-append (getcwd) "/hypre/lib"))
             (setenv "PATH" (string-append "." ":" (getenv "PATH")))
             (and (system* "make" "check" "CHECKRUN=")
                  (fold (lambda (filename result)
                          (and result
                               (let ((size (stat:size (stat filename))))
                                 (when (not (zero? size))
                                   (format #t "~a size ~d; error indication~%"
                                           filename size))
                                 (zero? size))))
                        #t
                        (find-files "test" ".*\\.err$")))))
         (add-after 'install 'install-docs
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Custom install because docs/Makefile doesn't honor ${docdir}.
             (let* ((doc (assoc-ref outputs "doc"))
                    (docdir (string-append doc "/share/doc/hypre-" ,version)))
               (with-directory-excursion "docs"
                 (for-each (lambda (base)
                             (install-file (string-append base ".pdf") docdir)
                             (copy-recursively base docdir)) ;html docs
                           '("HYPRE_usr_manual"
                             "HYPRE_ref_manual")))
               #t))))))
    (home-page "http://www.llnl.gov/casc/hypre/")
    (synopsis "Library of solvers and preconditioners for linear equations")
    (description
     "HYPRE is a software library of high performance preconditioners and
solvers for the solution of large, sparse linear systems of equations.  It
features multigrid solvers for both structured and unstructured grid
problems.")
    (license license:lgpl2.1)))

(define-public hypre-openmpi
  (package (inherit hypre)
    (name "hypre-openmpi")
    (inputs
     `(("mpi" ,openmpi)
       ,@(package-inputs hypre)))
    (arguments
     (substitute-keyword-arguments (package-arguments hypre)
       ((#:configure-flags flags)
        ``("--with-MPI"
           ,@(delete "--without-MPI" ,flags)))))
    (synopsis "Parallel solvers and preconditioners for linear equations")
    (description
     "HYPRE is a software library of high performance preconditioners and
solvers for the solution of large, sparse linear systems of equations on
parallel computers.  It features parallel multigrid solvers for both
structured and unstructured grid problems.")))

(define-public matio
  (package
    (name "matio")
    (version "1.5.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/matio/matio/" version "/"
                           "matio-" version ".tar.gz"))
       (sha256
        (base32
         "0y2qymgxank8wdiwc68ap8bxdzrhvyw86i29yh3xgn4z1njfd9ir"))))
    (build-system gnu-build-system)
    (inputs
     `(("zlib" ,zlib)
       ("hdf5" ,hdf5)))
    (home-page "http://matio.sourceforge.net/")
    (synopsis "Library for reading and writing MAT files")
    (description "Matio is a library for reading and writing MAT files.  It
supports compressed MAT files, as well as newer (version 7.3) MAT files.")
    (license license:bsd-2)))

(define-public vc
  (package
    (name "vc")
    (version "1.3.3")
    (source
      (origin (method url-fetch)
              (uri (string-append "https://github.com/VcDevel/Vc/releases/"
                                  "download/" version "/Vc-" version ".tar.gz"))
              (sha256
               (base32
                "1zmlpn32jzb38smp3j834llmbix3whsrbw0h397qxysbw792kih8"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       '("-DBUILD_TESTING=ON")))
    (synopsis "SIMD vector classes for C++")
    (description "Vc provides portable, zero-overhead C++ types for explicitly
data-parallel programming.  It is a library designed to ease explicit
vectorization of C++ code.  Its types enable explicitly stating data-parallel
operations on multiple values.  The parallelism is therefore added via the type
system.  Vc has an intuitive API and provides portability between different
compilers and compiler versions as well as portability between different vector
instruction sets.  Thus, an application written with Vc can be compiled for:
@enumerate
@item AVX and AVX2
@item SSE2 up to SSE4.2 or SSE4a
@item Scalar
@item MIC
@item NEON (in development)
@item NVIDIA GPUs / CUDA (in development)
@end enumerate\n")
    (home-page "https://github.com/VcDevel/Vc")
    ;; "No support_???.cpp file exists for this architecture."
    (supported-systems '("x86_64-linux" "i686-linux"))
    (license license:bsd-3)))

(define-public reducelcs
  ;; This is the last commit which is available upstream, no
  ;; release happened since 2010.
  (let ((commit "474f88deb968061abe8cf11c959e02319b8ae5c0")
        (revision "1"))
    (package
      (name "reducelcs")
      (version (string-append "1.0-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gdv/Reduce-Expand-for-LCS")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "1rllzcfwc042c336mhq262a8ha90x6afq30kvk60r7i4761j4yjm"))))
      (build-system gnu-build-system)
      (inputs
       `(("openlibm" ,openlibm)))
      (arguments
       `(#:tests? #f ; no tests
         #:phases
         (modify-phases %standard-phases
           (delete 'configure) ; No configure script exists.
           (replace 'install ; No install phase exists.
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin")))
                 (install-file "Approximation" bin)
                 (install-file "CollectResults" bin)
                 (install-file "GenerateInstances" bin)
                 #t))))))
      (synopsis "Approximate Longest Commons Subsequence computation tool")
      (description
       "@code{reduceLCS} is an implementation of the Reduce-Expand
algorithm for LCS.  It is a fast program to compute the approximate
Longest Commons Subsequence of a set of strings.")
      (home-page "https://github.com/gdv/Reduce-Expand-for-LCS")
      (license license:gpl3+))))

(define-public jacal
  (package
    (name "jacal")
    (version "1c4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://groups.csail.mit.edu/mac/ftpdir/scm/jacal-"
                    version ".zip"))
              (sha256 (base32
                       "055zrn12a1dmy0dqkwrkq3fklbhg3yir6vn0lacp4mvbg8573a3q"))
              (patches (search-patches "jacal-fix-texinfo.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'pre-build
                     ;; Don't use upstream's script - it really doesn't fit into
                     ;; Guix's functional paradigm.
                     (lambda* (#:key inputs outputs #:allow-other-keys)
                       (substitute* "Makefile"
                         (("^install: install-script") "install: "))))
         (add-after 'install 'post-install
                    ;; Instead, we provide our own simplified script.
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((wrapper (string-append (assoc-ref outputs "out")
                                                    "/bin/jacal")))
                        (format (open wrapper (logior O_WRONLY O_CREAT))
                                (string-append "#!~a\nexec ~a/bin/scm -ip1 "
                                "-e '(slib:load \"~a/lib/jacal/math\") "
                                "(math)' \"$@\"\n")
                                (which  "bash")
                                (assoc-ref inputs "scm")
                                (assoc-ref outputs "out"))
                        (chmod wrapper #o555))))
         (replace 'configure
                  (lambda* (#:key inputs outputs #:allow-other-keys)
                    (zero? (system* "./configure"
                                    (string-append "--prefix="
                                                   (assoc-ref outputs "out")))))))))
    (inputs `(("scm" ,scm)))
    (native-inputs `(("unzip" ,unzip)
                     ("texinfo" ,texinfo)))
    (synopsis "Symbolic mathematics system")
    (description "GNU JACAL is an interactive symbolic mathematics program based on
Scheme.  It manipulate and simplify a range of mathematical expressions such
as equations, scalars, vectors, and matrices.")
    (home-page "https://www.gnu.org/software/jacal/")
    (license license:gpl3+)))

(define-public z3
  (package
    (name "z3")
    (version "4.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/Z3Prover/z3/archive/z3-"
                    version ".tar.gz"))
              (sha256
               (base32
                "032a5lvji2liwmc25jv52bdrhimqflvqbpg77ccaq1jykhiivbmf"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list "-DBUILD_PYTHON_BINDINGS=true"
             "-DINSTALL_PYTHON_BINDINGS=true"
             (string-append "-DCMAKE_INSTALL_PYTHON_PKG_DIR="
                            %output
                            "/lib/python2.7/site-packages"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'bootstrap
           (lambda _
             (zero?
              (system* "python" "contrib/cmake/bootstrap.py" "create"))))
         (add-before 'check 'make-test-z3
           (lambda _
             ;; Build the test suite executable.
             (zero? (system* "make" "test-z3" "-j"
                             (number->string (parallel-job-count))))))
         (replace 'check
           (lambda _
             ;; Run all the tests that don't require arguments.
             (zero? (system* "./test-z3" "/a")))))))
    (native-inputs
     `(("python" ,python-2)))
    (synopsis "Theorem prover")
    (description "Z3 is a theorem prover and @dfn{satisfiability modulo
theories} (SMT) solver.  It provides a C/C++ API, as well as Python bindings.")
    (home-page "https://github.com/Z3Prover/z3")
    (license license:expat)))

(define-public cubicle
  (package
    (name "cubicle")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://cubicle.lri.fr/cubicle-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1sny9c4fm14k014pk62ibpwbrjjirkx8xmhs9jg7q1hk7y7x3q2h"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("ocaml" ,ocaml)
       ("which" ,(@@ (gnu packages base) which))))
    (propagated-inputs
     `(("z3" ,z3)))
    (arguments
     `(#:configure-flags (list "--with-z3")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'configure-for-release
           (lambda _
             (substitute* "Makefile.in"
               (("SVNREV=") "#SVNREV="))))
         (add-before 'configure 'fix-/bin/sh
           (lambda _
             (substitute* "configure"
               (("/bin/sh") (which "sh")))))
         (add-before 'configure 'fix-smt-z3wrapper.ml
           (lambda _
             (substitute* "Makefile.in"
               (("\\\\n") "")))))))
    (home-page "http://cubicle.lri.fr/")
    (synopsis "Model checker for array-based systems")
    (description "Cubicle is a model checker for verifying safety properties
of array-based systems.  This is a syntactically restricted class of
parametrized transition systems with states represented as arrays indexed by
an arbitrary number of processes.  Cache coherence protocols and mutual
exclusion algorithms are typical examples of such systems.")
    (license license:asl2.0)))

(define-public elemental
  (package
    (name "elemental")
    (version "0.87.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/elemental/Elemental/"
                                  "archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1nfp82w22pi8x8fg9sc37z8kf84dqi1dhxp8bbk7571y4aygvv3v"))))
    (build-system cmake-build-system)
    (home-page "http://libelemental.org")
    (native-inputs
     `(("gfortran" ,gfortran)))
    (inputs
     `(("blas" ,openblas)
       ("gfortran:lib" ,gfortran "lib")
       ("gmp" ,gmp)
       ("lapack" ,lapack)
       ("metis" ,metis)
       ("mpc" ,mpc)
       ("mpfr" ,mpfr)
       ("mpi" ,openmpi)
       ("qd" ,qd)))
    (arguments
     `(#:build-type "Release"           ;default RelWithDebInfo not supported
       #:configure-flags `("-DEL_DISABLE_PARMETIS:BOOL=YES"
                           "-DEL_AVOID_COMPLEX_MPI:BOOL=NO"
                           "-DEL_CACHE_WARNINGS:BOOL=YES"
                           "-DEL_TESTS:BOOL=YES"
                           "-DCMAKE_INSTALL_LIBDIR=lib"
                           "-DGFORTRAN_LIB=gfortran")
       #:phases (modify-phases %standard-phases
                  (add-before 'check 'setup-tests
                    (lambda _
                      ;; Parallelism is done at the MPI layer.
                      (setenv "OMP_NUM_THREADS" "1")
                      #t))
                  (add-after 'install 'remove-tests
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; Tests are installed, with no easy configuration
                      ;; switch to prevent this, so delete them.
                      (delete-file-recursively
                       (string-append (assoc-ref outputs "out") "/bin"))
                      #t)))))
    (synopsis "Dense and sparse-direct linear algebra and optimization")
    (description "Elemental is a modern C++ library for distributed-memory
dense and sparse-direct linear algebra, conic optimization, and lattice
reduction.")
    (license license:bsd-2)))

(define-public mcrl2
  (package
    (name "mcrl2")
    (version "201707.1.15162")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.mcrl2.org/download/devel/mcrl2-"
                                  version
                                  ".tar.gz"))
              (sha256
               (base32
                "1ziww2fchsklm25hl9p2mngssxfh9w07nc114cncqaxfibqp2p8f"))))
    (native-inputs
     `(("subversion" ,subversion)))
    (inputs
     `(("boost" ,boost)
       ("glu" ,glu)
       ("mesa" ,mesa)
       ("qt" ,qt)))
    (build-system cmake-build-system)
    (synopsis "Toolset for the mCRL2 formal specification language")
    (description
     "@dfn{mCRL2} (micro Common Representation Language 2) is a formal
specification language for describing concurrent discrete event systems.  Its
toolset supports analysis and automatic verification, linearisation, simulation,
state-space exploration and generation, and tools to optimise and analyse
specifications.  Also, state spaces can be manipulated, visualised and
analysed.")
    (home-page "http://mcrl2.org")
    (license license:boost1.0)))

(define-public r-subplex
  (package
    (name "r-subplex")
    (version "1.5-2")
    (source
    (origin
      (method url-fetch)
      (uri (cran-uri "subplex" version))
      (sha256
       (base32
        "1v9xrnkapnq7v1jbhlg32ignklzf2vn8rqpayc8pzk8wvz53r33g"))))
    (build-system r-build-system)
    (native-inputs
     `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/subplex")
    (synopsis "Unconstrained optimization using the subplex algorithm")
    (description "This package implements the Subplex optimization algorithm.
It solves unconstrained optimization problems using a simplex method on
subspaces.  The method is well suited for optimizing objective functions that
are noisy or are discontinuous at the solution.")
    (license license:gpl3+)))

(define-public r-desolve
  (package
    (name "r-desolve")
    (version "1.20")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "deSolve" version))
        (sha256
         (base32
          "18nx3maww979a8p8ly4hv63y65mnjx8vbj2fpipd6rhcbf1lbsan"))))
    (properties `((upstream-name . "deSolve")))
    (build-system r-build-system)
    (native-inputs
     `(("gfortran" ,gfortran)))
    (home-page "https://desolve.r-forge.r-project.org/")
    (synopsis "Solvers for initial value problems of differential equations")
    (description "This package provides functions that solve initial
value problems of a system of first-order ordinary differential equations (ODE),
of partial differential equations (PDE), of differential algebraic equations
(DAE), and of delay differential equations.  The functions provide an interface
to the FORTRAN functions lsoda, lsodar, lsode, lsodes of the ODEPACK collection,
to the FORTRAN functions dvode and daspk and a C-implementation of solvers of
the Runge-Kutta family with fixed or variable time steps.  The package contains
routines designed for solving ODEs resulting from 1-D, 2-D and 3-D partial
differential equations (PDE) that have been converted to ODEs by numerical
differencing.")
    (license license:gpl2+)))
