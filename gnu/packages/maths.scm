;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014, 2016, 2017 John Darrington <jmd@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2014 Mathieu Lirzin <mathieu.lirzin@openmailbox.org>
;;; Copyright © 2015, 2016, 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2016, 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2015 Fabian Harfert <fhmgufs@web.de>
;;; Copyright © 2016 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016, 2018 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017, 2018 Paul Garlick <pgarlick@tourbillion-technology.com>
;;; Copyright © 2017 ng0 <ng0@n0.is>
;;; Copyright © 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017 Theodoros Foradis <theodoros@foradis.org>
;;; Copyright © 2017, 2019 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Dave Love <me@fx@gnu.org>
;;; Copyright © 2018 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2018 Joshua Sierles, Nextjournal <joshua@nextjournal.com>
;;; Copyright © 2018 Nadya Voronova <voronovank@gmail.com>
;;; Copyright © 2018 Adam Massmann <massmannak@gmail.com>
;;; Copyright © 2018 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018 Eric Brown <brown@fastmail.com>
;;; Copyright © 2018 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2018 Amin Bandali <bandali@gnu.org>
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
  #:use-module (ice-9 match)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module ((guix build utils) #:select (alist-replace))
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system r)
  #:use-module (guix build-system ruby)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
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
  #:use-module (gnu packages linux)
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
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby)
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
   (version "2.0.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/c-graph/c-graph-" version
                                ".tar.gz"))
            (sha256 (base32
                     "092412jzxy6wdvpk96pfj499hpmaww8xllavbvlqspfpr7ips9id"))))
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
    (version "2.19")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/stcorp/coda/releases/download/"
                           version "/coda-" version ".tar.gz"))
       (sha256
        (base32 "1fbxd2afm7dshd92p10yy8dwbr9gc1h1fmnnnmr7d0c5lnw80245"))
       (patches (search-patches "coda-use-system-libs.patch"))
       (modules '((guix build utils)))
       (snippet
        ;; Make sure we don't use the bundled software.
        '(begin
           (for-each (lambda (d)
                       (delete-file-recursively (string-append "libcoda/" d)))
                     '("zlib" "pcre" "expat"))
           #t))))
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

(define-public qhull
  (package
    (name "qhull")
    (version "2015.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.qhull.org/download/qhull-"
                                  (car (string-split version #\.))
                                  "-src-7.2.0.tgz"))
              (sha256
               (base32
                "0dm4b2xr3asy6w74khq2zg4gf26zsy3qf9sq7pf7lmrvbj911c3q"))))
    (build-system cmake-build-system)
    (synopsis "Calculate convex hulls and related structures")
    (description
     "@code{Qhull} computes the convex hull, Delaunay triangulation, Voronoi
diagram, halfspace intersection about a point, furthest-site Delaunay
triangulation, and furthest-site Voronoi diagram.  The source code runs in 2-d,
3-d, 4-d, and higher dimensions.  @code{Qhull} implements the Quickhull
algorithm for computing the convex hull.  It handles roundoff errors from
floating point arithmetic.  It computes volumes, surface areas, and
approximations to the convex hull.

@code{Qhull} does not support triangulation of non-convex surfaces, mesh
generation of non-convex objects, medium-sized inputs in 9-D and higher, alpha
shapes, weighted Voronoi diagrams, Voronoi volumes, or constrained Delaunay
triangulations.")
    (home-page "http://qhull.org")
    (license (license:non-copyleft "file://COPYING.txt"
                                   "See COPYING in the distribution."))))

(define-public python-cvxopt
  (package
    (name "python-cvxopt")
    (version "1.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cvxopt/cvxopt.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "05mnjil9palaa48xafdfh4f5pr4z7aqjr995rwl08qfyxs8y0crf"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'find-libraries
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CVXOPT_BLAS_LIB" "openblas")
             (setenv "CVXOPT_BUILD_FFTW" "1")
             (setenv "CVXOPT_BUILD_GLPK" "1")
             (setenv "CVXOPT_BUILD_GSL" "1")
             #t)))))
    (inputs
     `(("fftw" ,fftw)
       ("glpk" ,glpk)
       ("gsl" ,gsl)
       ("lapack" ,lapack)
       ("openblas" ,openblas)
       ("suitesparse" ,suitesparse)))
    (home-page "https://www.cvxopt.org")
    (synopsis "Python library for convex optimization")
    (description
     "CVXOPT is a package for convex optimization based on the Python
programming language.  Its main purpose is to make the development of software
for convex optimization applications straightforward by building on Python’s
extensive standard library and on the strengths of Python as a high-level
programming language.")
    (license license:gpl3+)))

(define-public python2-cvxopt
  (package-with-python2 python-cvxopt))

(define-public units
  (package
   (name "units")
   (version "2.18")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/units/units-" version
                                ".tar.gz"))
            (sha256 (base32
                     "0y26kj349i048y4z3xrk90bvciw2j6ds3rka7r7yn3183hirr5b4"))))
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
    (version "3.1.4")
    (home-page "https://github.com/google/double-conversion")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "13xwcqk2c0q8c1siw566clxcpvp0xrxvb72mra42wa3nvq9wlsv6"))))
    (build-system cmake-build-system)
    (arguments
     '(#:test-target "test"
       #:configure-flags '("-DBUILD_SHARED_LIBS=ON"
                           "-DBUILD_TESTING=ON")))
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
    (version "2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gsl/gsl-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1395y9hlhqadn5g9j8q22224fds5sd92jxi9czfavjj24myasq04"))
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
    (version "1.6.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/4ti2/4ti2/releases/download/"
                           "Release_"
                           (string-map (lambda (c) (if (char=? c #\.) #\_ c))
                                       version)
                           "/4ti2-" version ".tar.gz"))
       (sha256
        (base32 "0rj92x6p9m3la5gasjbj7sa569im527ffmka5y2sv1amgd3fflrh"))))
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
    (version "0.94i")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "ftp://ftp.math.ethz.ch/users/fukudak/cdd/cddlib-"
                          (string-delete #\. version) ".tar.gz"))
      (sha256
       (base32
        "00zdgiqb91vx6gd2103h3ijij0llspsxc6zz3iw2bll39fvkl4xq"))))
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
    (version "3.6.3")
    (home-page "https://github.com/opencollab/arpack-ng")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wljl96yqxc9v8r49c37lscwkdp58kaacfb9p6s6nvpm31haax4y"))))
    (build-system gnu-build-system)
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
    (arguments
     (substitute-keyword-arguments (package-arguments arpack-ng)
       ((#:configure-flags _ '())
        ''("--enable-mpi"))
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (add-before 'check 'mpi-setup
             ,%openmpi-setup)))))
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
         "0p1r61ss1fq0bs8ynnx7xq4wwsdvs32ljvwjnx6yxr8gd6pawx0c"))
       (patches (search-patches "scalapack-blacs-mpi-deprecations.patch"))))
    (build-system cmake-build-system)
    (inputs
     `(("mpi" ,openmpi)
       ("fortran" ,gfortran)
       ("lapack" ,lapack)))             ;for testing only
    (arguments
     `(#:configure-flags `("-DBUILD_SHARED_LIBS:BOOL=YES")
       #:phases (modify-phases %standard-phases
                  (add-before 'check 'mpi-setup
		    ,%openmpi-setup))))
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
    (version "5.2.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/gnuplot/gnuplot/"
                                  version "/gnuplot-"
                                  version ".tar.gz"))
       (sha256
        (base32 "1vllgap08nhvdmc03idmkdnk9cfl2bp81hps50q1pqrr640qzp9m"))))
    (build-system gnu-build-system)
    (inputs `(("readline" ,readline)
              ("cairo" ,cairo)
              ("pango" ,pango)
              ("gd" ,gd)
              ("lua" ,lua)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("texlive" ,texlive-tiny)))
    (arguments `(#:configure-flags (list (string-append
                                          "--with-texdir=" %output
                                          "/texmf-local/tex/latex/gnuplot"))))
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
    (version "1.8.21")
    (source
     (origin
      (method url-fetch)
      (uri (list (string-append "https://support.hdfgroup.org/ftp/HDF5/releases/"
                                "hdf5-" (version-major+minor version)
                                "/hdf5-" version "/src/hdf5-"
                                version ".tar.bz2")
                 (string-append "https://support.hdfgroup.org/ftp/HDF5/"
                                "current"
                                (match (string-split version #\.)
                                  ((major minor _ ...)
                                   (string-append major minor)))
                                "/src/hdf5-" version ".tar.bz2")))
      (sha256
       (base32 "03glk4w4wyb1jyb443g53y3y1ncnf6mj2cqwm6avfr2awkgb3cg5"))
      (patches (search-patches "hdf5-config-date.patch"
                               "hdf5-1.8-mpi-deprecations.patch"))))
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
                ;; Note: When built with --enable-parallel, the 'h5fc' file
                ;; doesn't exist, hence this condition.
                (when (file-exists? (string-append bin "/h5fc"))
                  (rename-file (string-append bin "/h5fc")
                               (string-append fbin "/h5fc")))
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

(define-public hdf5-1.10
  (package (inherit hdf5)
    (version "1.10.4")
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
       (base32 "1pr85fa1sh2ky6ai2hs3f21lp252grl2cq3wbyi4rh7dm83gyrqj"))
      (patches (search-patches "hdf5-config-date.patch"
                               "hdf5-mpi-deprecations.patch"))))))

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
       (snippet     ; Make sure we don't use the bundled sources and binaries.
        `(begin
           (for-each delete-file
                     (list "SZip.tar.gz" "ZLib.tar.gz" "JPEG8d.tar.gz"
                           "HDF4.tar.gz" "HDF5.tar.gz"))
           (delete-file-recursively ,(string-append "hdfjava-" version "/lib"))
           #t))))
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
           (lambda _ (chdir ,(string-append "hdfjava-" version)) #t))
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
                           (install-file
                            (string-append (assoc-ref inputs "automake")
                                           "/share/automake-"
                                           ,(version-major+minor (package-version automake))
                                           "/" file) "."))
                         '("config.sub" "config.guess")))

             ;; Fix embedded version number
             (let ((hdf5version (list ,@(string-split (package-version hdf5) #\.))))
               (substitute* "hdf/hdf5lib/H5.java"
                 (("1, 8, 19")
                  (string-join hdf5version ", "))))

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
             (apply invoke `("javac"
                             ,@(find-files "examples" ".*\\.java"))))))

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
     (substitute-keyword-arguments (package-arguments hdf5)
       ((#:configure-flags flags)
        ``("--enable-parallel" ,@(delete "--enable-cxx" ,flags)))
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'build 'mpi-setup
             ,%openmpi-setup)
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
    (version "3.12.12")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.coin-or.org/download/source/Ipopt/Ipopt-"
                    version".tgz"))
              (sha256
               (base32
                "07yn9rzdswjk8n246qq6ci9ssf2bcplkifcpsfz9j6cdxw9vgbkv"))
              (modules '((guix build utils)))
              (snippet
               ;; Make sure we don't use the bundled software.
               '(begin
                  (delete-file-recursively "ThirdParty")
                  #t))))
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
    (version "1.14.0")
    (home-page "http://ceres-solver.org/")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page "ceres-solver-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "13lfxy8x58w8vprr0nkbzziaijlh0vvqshgahvcgw0mrqdgh0i27"))))
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

;; For a fully featured Octave, users are strongly recommended also to install
;; the following packages: less, ghostscript, gnuplot.
(define-public octave-cli
  (package
    (name "octave-cli")
    (version "5.1.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/octave/octave-"
                          version ".tar.lz"))
      (sha256
       (base32
        "11wwxpy2q1bhxs2v41bqn05i2sb0905cj1xil6mg8l4k2kka4cq6"))))
    (build-system gnu-build-system)
    (inputs
     `(("lapack" ,lapack)
       ("qhull" ,qhull)
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
       ("graphicsmagick" ,graphicsmagick)
       ("suitesparse" ,suitesparse)
       ("libsndfile" ,libsndfile)
       ("portaudio" ,portaudio)
       ("alsa-lib" ,alsa-lib)))
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
    ;; Octave code uses this variable to detect directories holding multiple CA
    ;; certificates to verify peers with.  This is required for the networking
    ;; functions that require encryption to work properly.
    (native-search-paths
     (list (search-path-specification
            (variable "CURLOPT_CAPATH")
            (files '("etc/ssl/certs")))))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-shell="
                            (assoc-ref %build-inputs "bash")
                            "/bin/sh"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'configure-makeinfo
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "libinterp/corefcn/help.h"
               (("\"makeinfo\"")
                (string-append
                 "\"" (assoc-ref inputs "texinfo") "/bin/makeinfo\"")))
             #t)))))
    (home-page "https://www.gnu.org/software/octave/")
    (synopsis "High-level language for numerical computation")
    (description "GNU Octave is a high-level interpreted language that is
specialized for numerical computations.  It can be used for both linear and
non-linear applications and it provides great support for visualizing results.
Work may be performed both at the interactive command-line as well as via
script files.")
    (license license:gpl3+)))

(define-public octave
  (package (inherit octave-cli)
    (name "octave")
    (source (origin
              (inherit (package-source octave-cli))))
    (inputs
     `(("qscintilla" ,qscintilla)
       ("qt" ,qtbase)
       ,@(package-inputs octave-cli)))
    (native-inputs
     `(("qttools" , qttools) ;for lrelease
       ("texlive" ,texlive) ;for texi2dvi
       ,@(package-native-inputs octave-cli)))
    (arguments
     (substitute-keyword-arguments (package-arguments octave-cli)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-before 'configure 'patch-qscintilla-library-name
             (lambda* (#:key inputs #:allow-other-keys)
               ;; The QScintilla library that the Octave configure script tries
               ;; to link with should be named libqscintilla-qt5.so, but the
               ;; QScintilla input provides the shared library as
               ;; libqscintilla2_qt5.so.
               (substitute* "configure"
                 (("qscintilla2-qt5")
                  "qscintilla2_qt5"))
               #t))))))))

(define-public qtoctave
  (deprecated-package "qtoctave" octave))

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
       '(begin
          (delete-file-recursively "contrib/Metis")
          #t))))
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
    (version "3.10.4")
    (source
     (origin
      (method url-fetch)
      ;; The *-lite-* tarball does not contain the *large* documentation
      (uri (string-append "http://ftp.mcs.anl.gov/pub/petsc/release-snapshots/"
                          "petsc-lite-" version ".tar.gz"))
      (sha256
       (base32 "0fk16944zh3473ra198kdkxdn08rq7b6ap838hxy1mh1i0hb488r"))))
    (outputs '("out"                    ; libraries and headers
               "examples"))             ; ~30MiB of examples
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
       #:parallel-build? #f             ; build is parallel by default
       #:configure-flags
       `("--with-mpi=0"
         "--with-openmp=1"
         "--with-superlu=1")
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
              (apply invoke "./configure" flags))))
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
              #t)))
        (add-after 'install 'move-examples
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (examples (assoc-ref outputs "examples"))
                   (exdir (string-append out "/share/petsc/examples"))
                   (exdir' (string-append examples "/share/petsc/examples")))
              (copy-recursively exdir exdir')
              (delete-file-recursively exdir)
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
     `(("hdf5" ,hdf5-parallel-openmpi)
       ("hypre" ,hypre-openmpi)
       ("metis" ,metis)
       ("mumps" ,mumps-openmpi)
       ("openmpi" ,openmpi)
       ("scalapack" ,scalapack)
       ("scotch" ,pt-scotch32)
       ,@(package-inputs petsc)))
    (arguments
     (substitute-keyword-arguments (package-arguments petsc)
       ((#:configure-flags cf)
        ``("--with-hypre=1"
           "--with-mpiexec=mpirun"
           "--with-metis=1"
           "--with-mumps=1"
           "--with-scalapack=1"
           "--with-ptscotch=1"
           ,(string-append "--with-mpi-dir="
                           (assoc-ref %build-inputs "openmpi"))
           ,(string-append "--with-hdf5-include="
                           (assoc-ref %build-inputs "hdf5") "/include")
           ,(string-append "--with-hdf5-lib="
                           (assoc-ref %build-inputs "hdf5") "/lib/libhdf5.a")
           ,@(delete "--with-mpi=0" ,cf)))
       ((#:phases phases)
        `(modify-phases ,phases
           (add-before 'configure 'mpi-setup
             ,%openmpi-setup)))))
    (synopsis "Library to solve PDEs (with MUMPS and MPI support)")))

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

(define-public python-petsc4py
  (package
    (name "python-petsc4py")
    (version "3.10.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "petsc4py" version))
        (sha256
          (base32
            "094hcnran0r2z1wlvmjswsz3ski1m9kqrl5l0ax8jjhnk55x0flh"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'pre-build
           (lambda _
             ;; Define path to PETSc installation.
             (setenv "PETSC_DIR" (assoc-ref %build-inputs "petsc"))
             #t))
         (add-before 'check 'mpi-setup
           ,%openmpi-setup))))
    (inputs
     `(("petsc" ,petsc-openmpi)
       ("python-numpy" ,python-numpy)))
    (home-page "https://bitbucket.org/petsc/petsc4py/")
    (synopsis "Python bindings for PETSc")
    (description "PETSc, the Portable, Extensible Toolkit for
Scientific Computation, is a suite of data structures and routines for
the scalable (parallel) solution of scientific applications modeled by
partial differential equations.  It employs the MPI standard for all
message-passing communication.  @code{petsc4py} provides Python
bindings to almost all functions of PETSc.")
    (license license:bsd-3)))

(define-public python-kiwisolver
  (package
    (name "python-kiwisolver")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "kiwisolver" version))
              (sha256
               (base32
                "0y22ci86znwwwfhbmvbgdfnbi6lv5gv2xkdlxvjw7lml43ayafyf"))))
    (build-system python-build-system)
    (home-page "https://github.com/nucleic/kiwi")
    (synopsis "Fast implementation of the Cassowary constraint solver")
    (description
     "Kiwi is an efficient C++ implementation of the Cassowary constraint
solving algorithm.  Kiwi has been designed from the ground up to be
lightweight and fast.  Kiwi ranges from 10x to 500x faster than the original
Cassowary solver with typical use cases gaining a 40x improvement.  Memory
savings are consistently > 5x.")
    (license license:bsd-3)))

(define-public python2-kiwisolver
  (package-with-python2 python-kiwisolver))

(define-public slepc
  (package
    (name "slepc")
    (version "3.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://slepc.upv.es/download/distrib/slepc-"
                           version ".tar.gz"))
       (sha256
        (base32
         "188j1a133q91h8pivpnzwcf78kz8dvz2nzf6ndnjygdbqb48fizn"))))
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
              (for-each delete-file (find-files out "\\.html$"))
              #t)))
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
                          "uninstall.py"))
              #t))))))
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
    (arguments
     (substitute-keyword-arguments (package-arguments slepc)
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (add-before 'check 'mpi-setup
	     ,%openmpi-setup)))))
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

(define-public python-slepc4py
  (package
    (name "python-slepc4py")
    (version "3.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "slepc4py" version))
        (sha256
          (base32
            "0x049dyc8frgh79fvvavf4vlbqp4mgm61nsaivzdav4316vvlv1j"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'pre-build
           (lambda _
             ;; Define path to PETSc installation.
             (setenv "PETSC_DIR" (assoc-ref %build-inputs "petsc"))
             ;; Define path to SLEPc installation.
             (setenv "SLEPC_DIR" (assoc-ref %build-inputs "slepc"))
             #t))
         (add-before 'check 'mpi-setup
           ,%openmpi-setup))))
    (inputs
     `(("python-numpy" ,python-numpy)
       ("python-petsc4py" ,python-petsc4py)
       ("slepc" ,slepc-openmpi)))
    (home-page "https://bitbucket.org/slepc/slepc4py/")
    (synopsis "Python bindings for SLEPc")
    (description "SLEPc, the Scalable Library for Eigenvalue Problem
Computations, is based on PETSc, the Portable, Extensible Toolkit for
Scientific Computation.  It employs the MPI standard for all
message-passing communication.  @code{slepc4py} provides Python
bindings to almost all functions of SLEPc.")
    (license license:bsd-3)))

(define-public mumps
  (package
    (name "mumps")
    (version "5.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://mumps.enseeiht.fr/MUMPS_"
                           version ".tar.gz"))
       (sha256
        (base32
         "1s9asin08zqzmh08257sdghhivvy9vjif7c53fhaxaax2kd5qd7b"))
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
            (invoke "make" "all"
                    (format #f "-j~a" (parallel-job-count)))))
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
           (add-before 'check 'mpi-setup
	     ,%openmpi-setup)
           (replace 'check
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
    (version "2.2.2")
    (source (origin
      (method url-fetch)
      (uri (cran-uri "pracma" version))
      (sha256
        (base32 "18zhni05gwnxbphl6bmjjxmsgg5wwnnkwlb4g971cqyw3dsd83ki"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/pracma/")
    (synopsis "Practical numerical math functions")
    (description "This package provides functions for numerical analysis and
linear algebra, numerical optimization, differential equations, plus some
special functions.  It uses Matlab function names where appropriate to simplify
porting.")
    (license license:gpl3+)))

(define-public ruby-asciimath
  (package
    (name "ruby-asciimath")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "asciimath" version))
       (sha256
        (base32
         "1d80kiph5mc78zps7si1hv48kv4k12mzaq8jk5kb3pqpjdr72qmc"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; Apply this patch
         ;; https://github.com/asciidoctor/asciimath/commit/1c06fdc8086077f4785479f78b0823a4a72d7948
         (add-after 'unpack 'patch-remove-spurious-backslashes
           (lambda _
             (substitute* "spec/parser_spec.rb"
               (("\\\\\"")
                "\""))
             #t)))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)))
    (synopsis "AsciiMath parsing and conversion library")
    (description
     "A pure Ruby AsciiMath parsing and conversion library.  AsciiMath is an
easy-to-write markup language for mathematics.")
    (home-page "https://github.com/asciidoctor/asciimath")
    (license license:expat)))

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
                      (loop))))
                 #t)))))))
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
    (version "6.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://crd-legacy.lbl.gov/~xiaoye/SuperLU/"
                           "superlu_dist_" version ".tar.gz"))
       (sha256
        (base32 "0pqgcgh1yxhfzs99fas3mggajzd5wca3nbyp878rziy74gfk03dl"))
       (modules '((guix build utils)))
       (snippet
        ;; Replace the non-free implementation of MC64 with a stub
        '(begin
           (call-with-output-file "SRC/mc64ad_dist.c"
             (lambda (port)
               (display "
#include <stdio.h>
#include <stdlib.h>
void mc64id_dist(int *a) {
  fprintf (stderr, \"SuperLU_DIST: non-free MC64 not available.  Aborting.\\n\");
  abort ();
}
void mc64ad_dist (int *a, int *b, int *c, int *d, int *e, double *f, int *g,
              int *h, int *i, int *j, int *k, double *l, int *m, int *n) {
  fprintf (stderr, \"SuperLU_DIST: non-free MC64 not available.  Aborting.\\n\");
  abort ();
}\n" port)))
           (substitute* "SRC/util.c"    ;adjust default algorithm
             (("RowPerm[[:blank:]]*=[[:blank:]]*LargeDiag_MC64;")
              ;; TODO: set to "LargeDiag_AWPM" once combinatorial-blas has
              ;; general (i.e. non-square) processor-grid support.
              "RowPerm = NOROWPERM;"))
           #t))
       (patches (search-patches "superlu-dist-scotchmetis.patch"
                                "superlu-dist-awpm-grid.patch"
                                "superlu-dist-fix-mpi-deprecations.patch"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("tcsh" ,tcsh)))
    (inputs
     `(("gfortran" ,gfortran)
       ("blas" ,openblas)
       ("lapack" ,lapack)
       ("combblas" ,combinatorial-blas)))
    (propagated-inputs
     `(("mpi" ,openmpi)                 ;headers include MPI heades
       ("parmetis" ,pt-scotch32 "metis")
       ("pt-scotch" ,pt-scotch32)))
    (arguments
     `(#:parallel-tests? #f             ;tests use MPI and OpenMP
       #:configure-flags (list "-DBUILD_SHARED_LIBS:BOOL=YES"
                               "-DTPL_ENABLE_COMBBLASLIB=YES"
                               "-DTPL_BLAS_LIBRARIES=-lopenblas"
                               "-DTPL_LAPACK_LIBRARIES=-llapack"
                               (string-append "-DTPL_PARMETIS_LIBRARIES="
                                              (string-join
                                               '("ptscotchparmetis" "ptscotch" "ptscotcherr"
                                                 "scotchmetis" "scotch" "scotcherr")
                                               ";"))
                               (string-append "-DTPL_PARMETIS_INCLUDE_DIRS="
                                              (assoc-ref %build-inputs "parmetis")
                                              "/include")
                               "-DTPL_ENABLE_COMBBLASLIB=ON"
                               (string-append "-DTPL_COMBBLAS_INCLUDE_DIRS="
                                              (assoc-ref %build-inputs "combblas")
                                              "/include/CombBLAS;"
                                              (assoc-ref %build-inputs "combblas")
                                              "/include/BipartiteMatchings")
                               "-DTPL_COMBBLAS_LIBRARIES=CombBLAS")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-c++-standard
           (lambda _
             (substitute* "CMakeLists.txt"
               ;; AWPM headers require C++14
               (("CMAKE_CXX_STANDARD 11") "CMAKE_CXX_STANDARD 14"))))
	 (add-before 'check 'mpi-setup
	   ,%openmpi-setup)
         (add-before 'check 'omp-setup
           (lambda _ (setenv "OMP_NUM_THREADS" "1") #t)))))
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
    (version "6.0.6")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://gforge.inria.fr/frs/download.php/"
                          "latestfile/298/scotch_" version ".tar.gz"))
      (sha256
       (base32 "1ky4k9r6jvajhqaqnnx6h8fkmds2yxgp70dpr1qzwcyhi2nhqvv8"))
      (patches (search-patches "scotch-build-parallelism.patch"
                               "scotch-integer-declarations.patch"))))
    (build-system gnu-build-system)
    (inputs
     `(("zlib" ,zlib)
       ("flex" ,flex)
       ("bison" ,bison)))
    (outputs '("out" "metis"))
    (arguments
     `(#:make-flags (list (string-append "prefix=" %output))
       #:phases
       (modify-phases %standard-phases
         (add-after
          'unpack 'chdir-to-src
          (lambda _ (chdir "src") #t))
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
                          "INTSIZE64"             ;use 'int64_t'
                          ;; Prevents symbolc clashes with libesmumps
                          "SCOTCH_RENAME"
                          ;; XXX: Causes invalid frees in superlu-dist tests
                          ;; "SCOTCH_PTHREAD"
                          ;; "SCOTCH_PTHREAD_NUMBER=2"
                          "restrict=__restrict"))))
            #t))
         (add-after 'build 'build-esmumps
          (lambda _
            (invoke "make"
                    (format #f "-j~a" (parallel-job-count))
                    "esmumps")))
         (add-before 'install 'make-install-dirs
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir (assoc-ref outputs "out"))))
         (add-after 'install 'install-metis
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "metis")))
               (mkdir out)
               ;; metis files are not installed with 'make install'
               (for-each (lambda (f)
                           (install-file f (string-append out "/include")))
                         (find-files "../include/" ".*metis\\.h"))
               (for-each (lambda (f)
                           (install-file f (string-append out "/lib")))
                         (find-files "../lib/" ".*metis\\..*"))
               #t))))))
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
              (invoke "make" (format #f "-j~a" (parallel-job-count))
                      "ptscotch" "ptesmumps")

              ;; Install the serial metis compatibility library
              (invoke "make" "-C" "libscotchmetis" "install")))
           (add-before 'check 'mpi-setup
	     ,%openmpi-setup)
           (replace 'check
             (lambda _
               (invoke "make" "ptcheck")))))))
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
               (invoke "make" (format #f "-j~a" (parallel-job-count))
                       "ptscotch" "ptesmumps")
               ;; Install the serial metis compatibility library
               (invoke "make" "-C" "libscotchmetis" "install")))
           (add-before 'check 'mpi-setup
	     ,%openmpi-setup)
           (replace 'check
             (lambda _
               (invoke "make" "ptcheck")))))))
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
                                           " -llapack"))
       #:phases (modify-phases %standard-phases
                  (add-before 'check 'mpi-setup
		    ,%openmpi-setup))))
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
    (version "5.42.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/maxima/Maxima-source/"
                           version "-source/" name "-" version ".tar.gz"))
       (sha256
        (base32
         "0kdncy6137sg3rradirxzj10mkcvafxd892zlclwhr9sa7b12zhn"))
       (patches (search-patches "maxima-defsystem-mkdir.patch"))))
    (build-system gnu-build-system)
    (inputs
     `(("gcl" ,gcl)
       ("gnuplot" ,gnuplot)                       ;for plots
       ("sed" ,sed)
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
         (add-after 'unpack 'patch-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((sed (string-append (assoc-ref inputs "sed") "/bin/sed"))
                    (coreutils (assoc-ref inputs "coreutils"))
                    (dirname (string-append coreutils "/bin/dirname"))
                    (head (string-append coreutils "/bin/head")))
               (substitute* "src/maxima.in"
                 (("sed ") (string-append sed " "))
                 (("dirname") dirname)
                 (("head") head))
               #t)))
         (add-before 'check 'pre-check
           (lambda _
             (chmod "src/maxima" #o555)
             #t))
         (replace 'check
           (lambda _
             ;; This is derived from the testing code in the "debian/rules" file
             ;; of Debian's Maxima package.
             ;; If Maxima can successfully run this, the binary to be installed
             ;; should be fine.
             (zero?
              (system
               (string-append "./maxima-local "
                              "--lisp=gcl "
                              "--batch-string=\"run_testsuite();\" "
                              "| grep -q \"No unexpected errors found\"")))))
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
    (version "19.01.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/wxMaxima-developers/wxmaxima.git")
             (commit (string-append "Version-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1vwahx3zxkn3qlv4z0fm7v8wh0wspvs026alrh7ff7s0c2dcy95x"))))
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
    (version "9.100.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/arma/armadillo-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1ka1vd9fcmvp12qkcm4888dkfqwnalvv00x04wy29f3nx3qwczby"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f))          ; no test target
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
    (version "0.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/openblas/v" version "/OpenBLAS%20"
                           version "%20version.tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "062kg4ny1ywz7k5grpb4pbf0hba0w6manbajwkmv4f477a31sxpl"))))
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
             "MAKE_NB_JOBS=0"           ;use jobserver for submakes
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

(define-public openblas-ilp64
  (package (inherit openblas)
    (name "openblas-ilp64")
    (supported-systems '("x86_64-linux" "aarch64-linux" "mips64el-linux"))
    (arguments
     (substitute-keyword-arguments (package-arguments openblas)
       ((#:make-flags flags '())
        `(append (list "INTERFACE64=1" "LIBNAMESUFFIX=ilp64")
                 ,flags))))
    (synopsis "Optimized BLAS library based on GotoBLAS (ILP64 version)")
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
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/JuliaLang/openlibm/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0a5fpm8nra5ldhjk0cqd2rx1qh32wiarkxmcqcm5xl8z7l4kjm6l"))))
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
        '(begin
           (delete-file-recursively "metis-5.1.0")
           #t))))
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
    (version "0.9.9.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/g-truc/glm/releases/download/"
                           version  "/glm-" version ".zip"))
       (sha256
        (base32
         "17vxbqzy4pxciq5i39bgpxz54f7ifqqmcqwwq7m6xfgikwqqqawp"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "https://glm.g-truc.net/")
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
       '(begin
          (substitute* (list "lp_solve/ccc" "lpsolve55/ccc")
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
            (("isnan\\(0\\)") "isnan(0.)"))
          #t))))
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
    (version "9.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/dealii/dealii/releases/"
                           "download/v" version "/dealii-" version ".tar.gz"))
       (sha256
        (base32
         "0r7f8rhl3xr94imd372plizdcbqk0a70w73lwc3vw912dxk0sbyz"))
       (patches (search-patches "dealii-mpi-deprecations.patch"))
       (modules '((guix build utils)))
       (snippet
        ;; Remove bundled sources: UMFPACK, TBB, muParser, and boost
        '(begin
           (delete-file-recursively "bundled")
           #t))))
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
        `(cons "-DDEAL_II_WITH_MPI:BOOL=ON"
               ,cf))
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (add-before 'check 'mpi-setup
             ,%openmpi-setup)))))
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
            "022w8hph7bli5zbpnk3z1qh1c2sl5hm8fw2ccim651ynn0hr7fyz"))
        (patches (search-patches "flann-cmake-3.11.patch"))))
    (build-system cmake-build-system)
    (outputs '("out"
               "octave"))                  ;46 MiB .mex file that pulls Octave
    (native-inputs
     `(("unzip" ,unzip)))
    (inputs
     `(("hdf5" ,hdf5)
       ("octave" ,octave-cli)
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
    (version "2.14.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/LLNL/hypre/archive/"
                                  "v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0v515i73bvaz378h5465b1dy9v2gf924zy2q94cpq4qqarawvkqh"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Remove use of __DATE__ and __TIME__ for reproducibility;
                  ;; substitute the tarball creation time.
                  (substitute* "src/utilities/HYPRE_utilities.h"
                    (("Date Compiled: .*$")
                     "Date Compiled: Apr 11 2018 16:24:59 +0000\"\n"))
                  #t))))
    (build-system gnu-build-system)
    (outputs '("out"                    ;6.1 MiB of headers and libraries
               "doc"))                  ;4.8 MiB of documentation
    (native-inputs
     `(("doc++" ,doc++)
       ("netpbm" ,netpbm)
       ("perl" ,perl)                   ;needed to run 'ppmquant' during tests
       ("texlive" ,(texlive-union (list texlive-generic-xypic
                                        texlive-fonts-xypic
                                        texlive-latex-hyperref
                                        texlive-bibtex)))
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
         (add-before 'build 'set-HOME
           (lambda _
             ;; FIXME: texlive-union does not find the built
             ;; metafonts, so it tries to generate them in HOME.
             (setenv "HOME" "/tmp")
             #t))
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
             (invoke "make" "-Cdocs" "pdf" "html")))
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
           ,@(delete "--without-MPI" ,flags)))
       ((#:phases phases)
        `(modify-phases ,phases
           (add-before 'check 'mpi-setup
	     ,%openmpi-setup)))))
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
       '("-DBUILD_TESTING=ON"
         ;; By default, Vc will optimize for the CPU of the build machine.
         ;; Setting this to "none" makes it create portable binaries.  See
         ;; "cmake/OptimizeForArchitecture.cmake".
         "-DTARGET_ARCHITECTURE=none")))
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
    (version "4.8.4")
    (home-page "https://github.com/Z3Prover/z3")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "z3-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "014igqm5vwswz0yhz0cdxsj3a6dh7i79hvhgc3jmmmz3z0xm1gyn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-compatability
           ;; Versions after 4.8.3 have immintrin.h IFDEFed for Windows only.
           (lambda _
             (substitute* "src/util/mpz.cpp"
               (("#include <immintrin.h>") ""))
             #t))
         (add-before 'configure 'bootstrap
           (lambda _
             (zero?
              (system* "python" "scripts/mk_make.py"))))
         ;; work around gnu-build-system's setting --enable-fast-install
         ;; (z3's `configure' is a wrapper around the above python file,
         ;; which fails when passed --enable-fast-install)
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (invoke "./configure"
                     (string-append "--prefix=" (assoc-ref outputs "out")))))
         (add-after 'configure 'change-directory
           (lambda _
             (chdir "build")
             #t))
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
     `(("which" ,which)
       ("python" ,python-wrapper)))
    (synopsis "Theorem prover")
    (description "Z3 is a theorem prover and @dfn{satisfiability modulo
theories} (SMT) solver.  It provides a C/C++ API, as well as Python bindings.")
    (license license:expat)))

(define-public elpa
  (package
    (name "elpa")
    (version "2018.11.001")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://elpa.mpcdf.mpg.de/html/Releases/"
                                  version "/elpa-" version ".tar.gz"))
              (sha256
               (base32
                "05hv3v5i6xmziaizw350ff72y1c3k662r85fm3xfdrkclj5zw9yc"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("fortran" ,gfortran)
       ("perl" ,perl)))                 ;for configure and deps
    (inputs
     `(("blas" ,openblas)))
    (arguments
     `(#:configure-flags
       `("--enable-openmp"
         "--with-mpi=no"
         ;; ELPA unfortunately does not support runtime dispatch, so we can
         ;; only enable the "generic" kernels.  See the "Cross compilation"
         ;; section of INSTALL.md.
         "--enable-generic"
         "--disable-sse" "--disable-sse-assembly" ;Require SSE3
         "--disable-avx" "--disable-avx2" "--disable-avx512"
         ,(string-append "CFLAGS=-O3 "
                         "-funsafe-loop-optimizations -funsafe-math-optimizations "
                         "-ftree-vect-loop-version -ftree-vectorize "
                         ,(let ((system (or (%current-target-system)
                                            (%current-system))))
                            (cond
                             ((or (string-prefix? "x86_64" system)
                                  (string-prefix? "i686" system))
                              "-msse2")
                             (else "")))))
       #:parallel-tests? #f             ;tests are multi-threaded, via BLAS
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-header-generation
           (lambda _
             (substitute* "configure"
               (("^  *make.*top_srcdir=\"\\$srcdir\"" &)
                (string-append & " CPP=\"$CPP\"")))
             #t))
         (add-before 'check 'setup-tests
           (lambda _
             ;; Decrease test time and RAM use by computing fewer eigenvalues.
             ;; The flags are (MATRIX-SIZE, EIGENVALUES, BLOCK-SIZE), where
             ;; the default is (500, 250, 16) for C tests and (5000, 150, 16)
             ;; for Fortran.  This also causes several tests to pass that
             ;; otherwise would otherwise fail with matrix size 5000; possibly
             ;; due to floating point tolerances that are too tight.
             (setenv "TEST_FLAGS" "1500 50 16") ;from elpa.spec
             (setenv "OMP_NUM_THREADS" (number->string (parallel-job-count)))
             (substitute* "Makefile"
               ;; Test scripts are generated, patch the shebang
               (("#!/bin/bash") (string-append "#!" (which "sh"))))
             #t)))))
    (home-page "http://elpa.mpcdf.mpg.de")
    (synopsis "Eigenvalue solvers for symmetric matrices")
    (description
     "The ELPA library provides efficient and scalable direct eigensolvers for
symmetric matrices.")
    (license license:lgpl3)))

(define-public elpa-openmpi
  (package (inherit elpa)
    (name "elpa-openmpi")
    (inputs
     `(("mpi" ,openmpi)
       ("scalapack" ,scalapack)
       ,@(package-inputs elpa)))
    (arguments
     (substitute-keyword-arguments (package-arguments elpa)
       ((#:configure-flags cf '())
        `(cons "--with-mpi=yes" (delete "--with-mpi=no" ,cf)))
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (add-before 'check 'mpi-setup
             (lambda _
               ;; Tests use 2 mpi tasks by default, use our remaining build
               ;; cores as OpenMP threads.
               (setenv "OMP_NUM_THREADS" (number->string
                                          (max (quotient (parallel-job-count) 2)
                                               1)))
               (,%openmpi-setup)))))))
    (synopsis "Eigenvalue solvers for symmetric matrices (with MPI support)")))

(define-public elemental
  (package
    (name "elemental")
    (version "0.87.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/elemental/Elemental.git")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1687xpjjzig27y2pnqv7hv09smpijyfdpz7qjgmcxf4shfajlfkc"))))
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
                  (add-before 'check 'mpi-setup
                    ,%openmpi-setup)
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
       ("qtbase" ,qtbase)))
    (build-system cmake-build-system)
    (synopsis "Toolset for the mCRL2 formal specification language")
    (description
     "@dfn{mCRL2} (micro Common Representation Language 2) is a formal
specification language for describing concurrent discrete event systems.  Its
toolset supports analysis and automatic verification, linearisation, simulation,
state-space exploration and generation, and tools to optimise and analyse
specifications.  Also, state spaces can be manipulated, visualised and
analysed.")
    (home-page "https://mcrl2.org")
    (license license:boost1.0)))

(define-public r-subplex
  (package
    (name "r-subplex")
    (version "1.5-4")
    (source
    (origin
      (method url-fetch)
      (uri (cran-uri "subplex" version))
      (sha256
       (base32
        "10cbgbx1bgsax5z7gz6716g360xpq4mvq19cf4qqrxv02mmwz57z"))))
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
    (version "1.21")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "deSolve" version))
        (sha256
         (base32
          "0qqc4mknw1jblzcmph1dg3k1p6w42yal0k1xjh8pqk7yb3a75hs5"))))
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

(define-public tcalc
  (package
  (name "tcalc")
  (version "2.0")
  (source
    (origin
      (method url-fetch)
      (uri (string-append "https://sites.google.com/site/mohammedisam2000/tcalc/tcalc-"
                            version ".tar.gz"))
      (sha256
        (base32
          "0jq806m4dqfia85nppfm75mml9w57g0cgv4cdw9bp3zymda83s0m"))))
  (build-system gnu-build-system)
  (synopsis "The terminal calculator")
  (description
    "The terminal calculator is a small program to help users of the GNU/Linux
terminal do calculations simply and quickly.  The formula to be calculated can
be fed to @command{tcalc} through the command line.")
  (home-page "https://sites.google.com/site/mohammedisam2000/tcalc")
  (license license:gpl3+)))

(define-public sundials
  (package
    (name "sundials")
    (version "3.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://computation.llnl.gov/projects/sundials/download/"
                           "sundials-" version ".tar.gz"))
       (sha256
        (base32
         "090s8ymhd0g1s1d44fa73r5yi32hb4biwahhbfi327zd64yn8kd2"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("python" ,python-2)))    ;for tests; syntax incompatible with python 3
    (inputs
     `(("fortran" ,gfortran)            ;for fcmix
       ("blas" ,openblas)
       ("suitesparse" ,suitesparse)))   ;TODO: Add hypre
    (arguments
     `(#:configure-flags `("-DEXAMPLES_ENABLE_C:BOOL=ON"
                           "-DEXAMPLES_ENABLE_CXX:BOOL=ON"
                           "-DEXAMPLES_ENABLE_F77:BOOL=ON"
                           "-DEXAMPLES_ENABLE_F90:BOOL=ON"
                           "-DEXAMPLES_INSTALL:BOOL=OFF"

                           "-DFCMIX_ENABLE:BOOL=ON"

                           "-DKLU_ENABLE:BOOL=ON"
                           ,(string-append "-DKLU_INCLUDE_DIR="
                                           (assoc-ref %build-inputs "suitesparse")
                                           "/include")
                           ,(string-append "-DKLU_LIBRARY_DIR="
                                           (assoc-ref %build-inputs "suitesparse")
                                           "/lib"))))
    (home-page "https://computation.llnl.gov/projects/sundials")
    (synopsis "Suite of nonlinear and differential/algebraic equation solvers")
    (description "SUNDIALS is a family of software packages implemented with
the goal of providing robust time integrators and nonlinear solvers that can
easily be incorporated into existing simulation codes.")
    (license license:bsd-3)))

(define-public sundials-openmpi
  (package (inherit sundials)
    (name "sundials-openmpi")
    (inputs
     `(("mpi" ,openmpi)
       ("petsc" ,petsc-openmpi)         ;support in SUNDIALS requires MPI
       ,@(package-inputs sundials)))
    (arguments
     (substitute-keyword-arguments (package-arguments sundials)
       ((#:configure-flags flags '())
        `(cons* "-DMPI_ENABLE:BOOL=ON"
                "-DPETSC_ENABLE:BOOL=ON"
                (string-append "-DPETSC_INCLUDE_DIR="
                               (assoc-ref %build-inputs "petsc")
                               "/include")
                (string-append "-DPETSC_LIBRARY_DIR="
                               (assoc-ref %build-inputs "petsc")
                               "/lib")
                ,flags))
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (add-before 'check 'mpi-setup
	     ,%openmpi-setup)))))
    (synopsis "SUNDIALS with OpenMPI support")))

(define-public combinatorial-blas
  (package
    (name "combinatorial-blas")
    (version "1.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://eecs.berkeley.edu/~aydin/CombBLAS_FILES/"
                           "CombBLAS_beta_"
                           (match (string-split version #\.)
                            ((major minor patch)
                             (string-append major minor "_" patch))) ;e.g. "16_2"
                           ".tgz"))
       (sha256
        (base32
         "1a9wbgdqyy1whhfc0yl0yqkax3amnqa6iihhq48d063gc0jwfd9a"))
       (patches (search-patches "combinatorial-blas-awpm.patch"
                                "combinatorial-blas-io-fix.patch"))))
    (build-system cmake-build-system)
    (inputs
     `(("mpi" ,openmpi)
       ("test-data" ,(origin
                       (method url-fetch)
                       (uri (string-append "https://people.eecs.berkeley.edu/~aydin/"
                                           "CombBLAS_FILES/testdata_combblas1.6.1.tgz"))
                       (sha256
                        (base32
                         "01y2781cy3fww7znmidrp85mf8zx0c905w5vzvk1mgrmhhynim87"))))))
    (arguments
     `(#:configure-flags '("-DBUILD_SHARED_LIBS:BOOL=YES"
                           "-DCMAKE_CXX_FLAGS=-DUSE_FUNNEL")
       #:parallel-tests? #f             ;tests use 'mpiexec -n4'
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'mpi-setup
           ,%openmpi-setup)
         (add-before 'check 'test-setup
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "OMP_NUM_THREADS" "2")
             (invoke "tar" "xf" (assoc-ref inputs "test-data")))))))
    (home-page "https://people.eecs.berkeley.edu/~aydin/CombBLAS/html/")
    (synopsis "Linear algebra primitives for graph analytics")
    (description "The Combinatorial BLAS (CombBLAS) is an extensible
distributed-memory parallel graph library offering a small but powerful set of
linear algebra primitives specifically targeting graph analytics.")
    (license (list
              license:gpl2+             ;include/psort/(funnel|sort)*.h
              license:x11               ;usort and psort
              license:bsd-3))))         ;CombBLAS and MersenneTwister.h

(define-public dune-common
  (package
    (name "dune-common")
    (version "2.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dune-project.org/download/"
                           version "/dune-common-" version ".tar.gz"))
       (sha256
        (base32
         "019wcr1qf7jwyxx1y5y290wdlglylskvbb2m01ljkzcza2xnlmhw"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-tests
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "make" "build_tests" make-flags)))
         ;; These tests fail because they require a fully functional MPI
         ;; environment.
         (add-after 'unpack 'disable-failing-tests
           (lambda _
             (setenv "ARGS"
                     (string-append "--exclude-regex '("
                                    (string-join
                                     (list
                                      "remoteindicestest"
                                      "remoteindicestest-mpi-2"
                                      "syncertest"
                                      "syncertest-mpi-2"
                                      "variablesizecommunicatortest"
                                      "variablesizecommunicatortest-mpi-2"
                                      "arithmetictestsuitetest"
                                      "assertandreturntest"
                                      "assertandreturntest_ndebug"
                                      "concept"
                                      "debugaligntest"
                                      "mpicollectivecommunication"
                                      "mpicollectivecommunication-mpi-2"
                                      "mpiguardtest"
                                      "mpiguardtest-mpi-2"
                                      "mpihelpertest"
                                      "mpihelpertest-mpi-2"
                                      "mpihelpertest2"
                                      "mpihelpertest2-mpi-2")
                                     "|")
                                    ")'"))
             #t)))))
    (inputs
     `(("gmp" ,gmp)
       ("metis" ,metis)
       ("openmpi" ,openmpi)
       ("openblas" ,openblas)
       ("python" ,python)
       ("superlu" ,superlu)))
    (native-inputs
     `(("gfortran" ,gfortran)
       ("pkg-config" ,pkg-config)))
    (home-page "https://dune-project.org/")
    (synopsis "Distributed and Unified Numerics Environment")
    (description "DUNE, the Distributed and Unified Numerics Environment is a
modular toolbox for solving @dfn{partial differential equations} (PDEs) with
grid-based methods.  It supports the easy implementation of methods like
@dfn{Finite Elements} (FE), @dfn{Finite Volumes} (FV), and also @dfn{Finite
Differences} (FD).")
    ;; GPL version 2 with "runtime exception" to make it behave like LGPLv2.
    (license license:gpl2)))

(define-public dune-geometry
  (package
    (name "dune-geometry")
    (version "2.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dune-project.org/download/"
                           version "/dune-geometry-" version ".tar.gz"))
       (sha256
        (base32
         "0hlaaxjyv9j05blasvb67sy02hd0w4g9znf68gdh3l731dd1aqbn"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-tests
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "make" "build_tests" make-flags))))))
    (inputs
     `(("dune-common" ,dune-common)
       ("openmpi" ,openmpi)
       ;; Optional
       ("openblas" ,openblas)
       ("gmp" ,gmp)
       ("python" ,python)))
    (native-inputs
     `(("gfortran" ,gfortran)
       ("pkg-config" ,pkg-config)))
    (home-page "https://dune-project.org/")
    (synopsis "Distributed and Unified Numerics Environment")
    (description "DUNE, the Distributed and Unified Numerics Environment is a
modular toolbox for solving @dfn{partial differential equations} (PDEs) with
grid-based methods.  It supports the easy implementation of methods like
@dfn{Finite Elements} (FE), @dfn{Finite Volumes} (FV), and also @dfn{Finite
Differences} (FD).

This package contains the basic DUNE geometry classes.")
    ;; GPL version 2 with "runtime exception"
    (license license:gpl2)))

(define-public dune-grid
  (package
    (name "dune-grid")
    (version "2.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dune-project.org/download/"
                           version "/dune-grid-" version ".tar.gz"))
       (sha256
        (base32
         "1jp4vscm9yb9xg0lh7apzccfkhvgbnk652yahigmh3cvzpl4acd0"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-tests
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "make" "build_tests" make-flags)))
         ;; These tests fail because they require a fully functional MPI
         ;; environment.
         (add-after 'unpack 'disable-failing-tests
           (lambda _
             (setenv "ARGS"
                     (string-append "--exclude-regex '("
                                    (string-join
                                     (list
                                      "scsgmappertest"
                                      "conformvolumevtktest"
                                      "gnuplottest"
                                      "nonconformboundaryvtktest"
                                      "subsamplingvtktest"
                                      "vtktest"
                                      "vtktest-mpi-2"
                                      "vtksequencetest"
                                      "gmshtest-onedgrid"
                                      "test-dgf-yasp"
                                      "test-dgf-yasp-offset"
                                      "test-dgf-oned"
                                      "test-geogrid-yaspgrid"
                                      "test-gridinfo"
                                      "test-identitygrid"
                                      "testiteratorranges"
                                      "test-hierarchicsearch"
                                      "test-parallel-ug-mpi-2"
                                      "test-yaspgrid-backuprestore-equidistant"
                                      "test-yaspgrid-backuprestore-equidistant-mpi-2"
                                      "test-yaspgrid-backuprestore-equidistantoffset"
                                      "test-yaspgrid-backuprestore-equidistantoffset-mpi-2"
                                      "test-yaspgrid-backuprestore-tensor"
                                      "test-yaspgrid-backuprestore-tensor-mpi-2"
                                      "test-yaspgrid-tensorgridfactory"
                                      "test-yaspgrid-tensorgridfactory-mpi-2"
                                      "test-yaspgrid-yaspfactory-1d"
                                      "test-yaspgrid-yaspfactory-1d-mpi-2"
                                      "test-yaspgrid-yaspfactory-2d"
                                      "test-yaspgrid-yaspfactory-2d-mpi-2"
                                      "test-yaspgrid-yaspfactory-3d"
                                      "test-yaspgrid-yaspfactory-3d-mpi-2"
                                      "globalindexsettest"
                                      "persistentcontainertest"
                                      "structuredgridfactorytest"
                                      "tensorgridfactorytest"
                                      "vertexordertest")
                                     "|")
                                    ")'"))
             #t)))))
    (inputs
     `(("dune-common" ,dune-common)
       ("dune-geometry" ,dune-geometry)
       ("gmp" ,gmp)
       ("metis" ,metis)
       ("openblas" ,openblas)
       ("openmpi" ,openmpi)
       ("python" ,python)))
    (native-inputs
     `(("gfortran" ,gfortran)
       ("pkg-config" ,pkg-config)))
    (home-page "https://dune-project.org/")
    (synopsis "Distributed and Unified Numerics Environment")
    (description "DUNE, the Distributed and Unified Numerics Environment is a
modular toolbox for solving @dfn{partial differential equations} (PDEs) with
grid-based methods.  It supports the easy implementation of methods like
@dfn{Finite Elements} (FE), @dfn{Finite Volumes} (FV), and also @dfn{Finite
Differences} (FD).

This package contains the basic DUNE grid classes.")
    ;; GPL version 2 with "runtime exception"
    (license license:gpl2)))

(define-public dune-istl
  (package
    (name "dune-istl")
    (version "2.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dune-project.org/download/"
                           version "/dune-istl-" version ".tar.gz"))
       (sha256
        (base32
         "0l2gyrvys5w6wsmk0ckbb7295s80b7yk7qrl7x66akv2jv1nzq2w"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-tests
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "make" "build_tests" make-flags)))
         ;; These tests fail because they require a fully functional MPI
         ;; environment.
         (add-after 'unpack 'disable-failing-tests
           (lambda _
             (setenv "ARGS"
                     (string-append "--exclude-regex '("
                                    (string-join
                                     (list
                                      "galerkintest"
	                              "hierarchytest"
	                              "pamgtest"
	                              "pamg_comm_repart_test"
	                              "matrixredisttest"
	                              "vectorcommtest"
	                              "matrixmarkettest")
                                     "|")
                                    ")'"))
             #t)))))
    (inputs
     `(("dune-common" ,dune-common)
       ("openmpi" ,openmpi)
       ;; Optional
       ("metis" ,metis)
       ("superlu" ,superlu)
       ("openblas" ,openblas)
       ("gmp" ,gmp)
       ("python" ,python)))
    (native-inputs
     `(("gfortran" ,gfortran)
       ("pkg-config" ,pkg-config)))
    (home-page "https://dune-project.org/")
    (synopsis "Distributed and Unified Numerics Environment")
    (description "DUNE, the Distributed and Unified Numerics Environment is a
modular toolbox for solving @dfn{partial differential equations} (PDEs) with
grid-based methods.

This is the iterative solver template library which provides generic sparse
matrix/vector classes and a variety of solvers based on these classes.  A
special feature is the use of templates to exploit the recursive block
structure of finite element matrices at compile time.  Available solvers
include Krylov methods, (block-) incomplete decompositions and
aggregation-based algebraic multigrid.")
    ;; GPL version 2 with "runtime exception"
    (license license:gpl2)))

(define-public dune-localfunctions
  (package
    (name "dune-localfunctions")
    (version "2.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dune-project.org/download/"
                           version "/dune-localfunctions-" version ".tar.gz"))
       (sha256
        (base32
         "19c6zjinwwpy8jh4v4prhphyd438rapd4x80fj93apmwgw04nrhl"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-tests
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "make" "build_tests" make-flags))))))
    (inputs
     `(("dune-common" ,dune-common)
       ("dune-geometry" ,dune-geometry)
       ("openmpi" ,openmpi)
       ;; Optional
       ("metis" ,metis)
       ("superlu" ,superlu)
       ("gmp" ,gmp)))
    (native-inputs
     `(("gfortran" ,gfortran)
       ("pkg-config" ,pkg-config)))
    (home-page "https://dune-project.org/")
    (synopsis "Distributed and Unified Numerics Environment") ; TODO
    (description "This DUNE module provides interface and implementation for
shape functions defined on the DUNE reference elements.  In addition to the
shape function, interpolation operators and special keys are provided which
can be used to assemble global function spaces on finite-element grids.

This package provides an interface and implementation for shape functions
defined on the DUNE reference elements.  In addition to the shape function,
interpolation operators and special keys are provided which can be used to
assemble global function spaces on finite-element grids.")
    ;; GPL version 2 with "runtime exception"
    (license license:gpl2)))

(define-public dune-alugrid
  (package
    (name "dune-alugrid")
    (version "2.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dune-project.org/download/"
                           version "/dune-alugrid-" version ".tar.gz"))
       (sha256
        (base32
         "1l9adgyjpra8mvwm445s0lpjshnb63jag85fb2hisbjn6bm320yj"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ; 7 of 8 tests fail because they need a full MPI
                   ; environment
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-include
           (lambda _
             (substitute* "dune/alugrid/test/test-alugrid.cc"
               (("doc/grids/gridfactory/testgrids")
                "doc/dune-grid/grids/gridfactory/testgrids"))
             #t))
         (add-after 'build 'build-tests
           (lambda* (#:key inputs make-flags #:allow-other-keys)
             (setenv "CPLUS_INCLUDE_PATH"
                     (string-append (assoc-ref inputs "dune-grid") "/share:"
                                    (getenv "CPLUS_INCLUDE_PATH")))
             (apply invoke "make" "build_tests" make-flags))))))
    (inputs
     `(("dune-common" ,dune-common)
       ("dune-geometry" ,dune-geometry)
       ("dune-grid" ,dune-grid)
       ("openmpi" ,openmpi)
       ;; Optional
       ("metis" ,metis)
       ("openblas" ,openblas)
       ("python" ,python)
       ("superlu" ,superlu)
       ("gmp" ,gmp)
       ("zlib" ,zlib)))
    (native-inputs
     `(("gfortran" ,gfortran)
       ("pkg-config" ,pkg-config)))
    (home-page "https://dune-project.org/")
    (synopsis "Distributed and Unified Numerics Environment")
    (description "ALUGrid is an adaptive, loadbalancing, unstructured
implementation of the DUNE grid interface supporting either simplices or
cubes.")
    (license license:gpl2+)))

(define-public dune-typetree
  (package
    (name "dune-typetree")
    (version "2.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.dune-project.org/staging/dune-typetree.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0mnv6w2f22lz3j4bdpdjq55vjm8xxfx9v4vvhg9bd36xpsbjpjp9"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-tests
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "make" "build_tests" make-flags))))))
    (inputs
     `(("dune-common" ,dune-common)
       ("openmpi" ,openmpi)
       ;; Optional
       ("openblas" ,openblas)
       ("python" ,python)
       ("metis" ,metis)
       ("superlu" ,superlu)
       ("gmp" ,gmp)))
    (native-inputs
     `(("gfortran" ,gfortran)
       ("pkg-config" ,pkg-config)))
    (home-page "https://dune-project.org/")
    (synopsis "Distributed and Unified Numerics Environment")
    (description "TypeTree is a template library for constructing and
operating on statically typed trees of objects.")
    ;; Either GPL version 2 with "runtime exception" or LGPLv3+.
    (license (list license:lgpl3+ license:gpl2))))

(define-public dune-functions
  (package
    (name "dune-functions")
    (version "2.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.dune-project.org/staging/dune-functions.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1an8gb477n8j0kzpbrv7nr1snh8pxip0gsxq6w63jc83gg3dj200"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f)) ; FIXME: tests require dune-uugrid
    (inputs
     `(("dune-common" ,dune-common)
       ("dune-istl" ,dune-istl)
       ("dune-localfunctions" ,dune-localfunctions)
       ("dune-grid" ,dune-grid)
       ("dune-geometry" ,dune-geometry)
       ("dune-typetree" ,dune-typetree)
       ("openmpi" ,openmpi)
       ("openblas" ,openblas)
       ("metis" ,metis)
       ("python" ,python)
       ("superlu" ,superlu)
       ("gmp" ,gmp)))
    (native-inputs
     `(("gfortran" ,gfortran)
       ("pkg-config" ,pkg-config)))
    (home-page "https://dune-project.org/")
    (synopsis "Distributed and Unified Numerics Environment")
    (description "The dune-functions module provides an abstraction layer for
global finite element functions.  Its two main concepts are functions
implemented as callable objects, and bases of finite element spaces.")
    ;; Either GPL version 2 with "runtime exception" or LGPLv3+.
    (license (list license:lgpl3+ license:gpl2))))

(define-public dune-pdelab
  (package
    (name "dune-pdelab")
    (version "2.6.0-rc1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.dune-project.org/pdelab/dune-pdelab")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "07g0s9448z65vjrq88g5rv3340iifil85k170n8kbqchsvi4ny5v"))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f)) ; XXX: the tests cannot be compiled
    (inputs
     `(("dune-common" ,dune-common)
       ("dune-istl" ,dune-istl)
       ("dune-localfunctions" ,dune-localfunctions)
       ("dune-geometry" ,dune-geometry)
       ("dune-grid" ,dune-grid)
       ("dune-typetree" ,dune-typetree)
       ("dune-functions" ,dune-functions)
       ("openmpi" ,openmpi)
       ;; Optional
       ("openblas" ,openblas)
       ("eigen" ,eigen)
       ("metis" ,metis)
       ("python" ,python)
       ("superlu" ,superlu)
       ("gmp" ,gmp)))
    (native-inputs
     `(("gfortran" ,gfortran)
       ("pkg-config" ,pkg-config)))
    (home-page "https://dune-project.org/")
    (synopsis "Differential equations solver toolbox")
    (description "PDELab is a partial differential equations solver toolbox
built on top of DUNE, the Distributed and Unified Numerics Environment.")
    ;; Either GPL version 2 with "runtime exception" or LGPLv3+.
    (license (list license:lgpl3+ license:gpl2))))
