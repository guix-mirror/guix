;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2014, 2015, 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2014 Mathieu Lirzin <mathieu.lirzin@openmailbox.org>
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2015 Fabian Harfert <fhmgufs@web.de>
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
  #:use-module (guix svn-download)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fltk)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages less)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages tcsh)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages texlive)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages zip)
  #:use-module (srfi srfi-1))

(define-public units
  (package
   (name "units")
   (version "2.12")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/units/units-" version
                                ".tar.gz"))
            (sha256 (base32
                     "1jxvjknz2jhq773jrwx9gc1df3gfy73yqmkjkygqxzpi318yls3q"))))
   (build-system gnu-build-system)
   (synopsis "Conversion between thousands of scales")
   (description
    "GNU Units converts numeric quantities between units of measure.  It
can handle scale changes through adaptive usage of standard scale
prefixes (micro-, kilo-, etc.).  It can also handle nonlinear
conversions such as Fahrenheit to Celsius.  Its interpreter is powerful
enough to be used effectively as a scientific calculator.")
   (license license:gpl3+)
   (home-page "http://www.gnu.org/software/units/")))

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
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/dionysus/dionysus-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "1aqnvw6z33bzqgd1ga571pnx6vq2zrkckm1cz91grv45h4jr9vgs"))))
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
    (home-page "http://www.gnu.org/software/dionysus/")))

(define-public gsl
  (package
    (name "gsl")
    (version "2.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/gsl/gsl-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0rhcia9jhr3p1f1wybwyllwqfs9bggz99i3mi5lpyqcpff1hdbar"))))
    (build-system gnu-build-system)
    (arguments
     `(#:parallel-tests? #f))
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
    (version "4.59")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/glpk/glpk-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1bpbp5z0378kaj5bqmc5m2j5h9c7553p0s2j6a28badqghpbx673"))))
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
    (version "0.10.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/pspp/pspp-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1nwnxr8mvf8y4lc8h8sd2xn2njwjk42x8mzj91zzv92m2z3vyggg"))))
    (build-system gnu-build-system)
    (inputs
     `(("cairo" ,cairo)
       ("fontconfig" ,fontconfig)
       ("gettext" ,gnu-gettext)
       ("gsl" ,gsl)
       ("libxml2" ,libxml2)
       ("pango" ,pango)
       ("readline" ,readline)
       ("gtk" ,gtk+)
       ("gtksourceview" ,gtksourceview)
       ("zlib" ,zlib)))
    (native-inputs
     `(("glib" ,glib "bin")             ;for glib-genmarshal
       ("perl" ,perl)
       ("texinfo" ,texinfo)
       ("pkg-config" ,pkg-config)))
    (home-page "http://www.gnu.org/software/pspp/")
    (synopsis "Statistical analysis")
    (description
     "GNU PSPP is a statistical analysis program.  It can perform
descriptive statistics, T-tests, linear regression and non-parametric tests.
It features both a graphical interface as well as command-line input.  PSPP
is designed to interoperate with Gnumeric, LibreOffice and OpenOffice.  Data
can be imported from spreadsheets, text files and database sources and it can
be output in text, PostScript, PDF or HTML.")
    (license license:gpl3+)))

(define-public arpack-ng
  (package
    (name "arpack-ng")
    (version "3.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/opencollab/arpack-ng/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1fwch6vipms1ispzg2djvbzv5wag36f1dmmr3xs3mbp6imfyhvff"))))
    (build-system gnu-build-system)
    (home-page "https://github.com/opencollab/arpack-ng")
    (inputs
     `(("lapack" ,lapack)
       ("fortran" ,gfortran)))
    (synopsis "Fortran subroutines for solving eigenvalue problems")
    (description
     "ARPACK-NG is a collection of Fortran77 subroutines designed to solve
large scale eigenvalue problems.")
    (license (license:non-copyleft "file://COPYING"
                                "See COPYING in the distribution."))))

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
    (inputs `(("fortran" ,gfortran)
              ("python" ,python-2)))
    (arguments
     `(#:configure-flags '("-DBUILD_SHARED_LIBS:BOOL=YES")
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
    (version "5.0.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/gnuplot/gnuplot/"
                          version "/gnuplot-" version ".tar.gz"))
      (sha256
       (base32
        "146qn414z96c7cc42a1kb9a4kpjc2q2hfdwk44kjjvgmfp9k2ass"))))
    (build-system gnu-build-system)
    (inputs `(("readline" ,readline)
              ("cairo" ,cairo)
              ("pango" ,pango)
              ("gd" ,gd)))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("texlive" ,texlive-minimal)))
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

(define-public hdf5
  (package
    (name "hdf5")
    (version "1.8.12")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://www.hdfgroup.org/ftp/HDF5/releases/hdf5-"
                          version "/src/hdf5-"
                          version ".tar.bz2"))
      (sha256
       (base32 "0f9n0v3p3lwc7564791a39c6cn1d3dbrn7d1j3ikqsi27a8hy23d"))))
    (build-system gnu-build-system)
    (inputs
     `(("zlib" ,zlib)))
    (arguments
     `(#:phases
        (alist-cons-before
         'configure 'patch-configure
         (lambda _
           (substitute* "configure"
             (("/bin/mv") "mv")))
         %standard-phases)))
    (home-page "http://www.hdfgroup.org")
    (synopsis "Management suite for extremely large and complex data")
    (description "HDF5 is a suite that makes possible the management of
extremely large and complex data collections.")
    (license (license:x11-style
              "http://www.hdfgroup.org/ftp/HDF5/current/src/unpacked/COPYING"))))


;; For a fully featured Octave, users  are strongly recommended also to install
;; the following packages: texinfo, less, ghostscript, gnuplot.
(define-public octave
  (package
    (name "octave")
    (version "4.0.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/octave/octave-"
                          version ".tar.gz"))
      (sha256
       (base32
        "101jr9yck798586jz4vkjcgk36zksmxf1pxrzvipgn2xgyay0zjc"))))
    (build-system gnu-build-system)
    (inputs
     `(("lapack" ,lapack)
       ("readline" ,readline)
       ("glpk" ,glpk)
       ("fftw" ,fftw)
       ("fftwf" ,fftwf)
       ("arpack" ,arpack-ng)
       ("curl" ,curl)
       ("pcre" ,pcre)
       ("fltk" ,fltk)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("hdf5" ,hdf5)
       ("libxft" ,libxft)
       ("mesa" ,mesa)
       ("glu" ,glu)
       ("zlib" ,zlib)))
    (native-inputs
     `(("gfortran" ,gfortran)
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
       ("texinfo" ,texinfo)
       ("ghostscript" ,ghostscript)
       ("gnuplot" ,gnuplot)))
    (arguments
     `(#:configure-flags (list (string-append "--with-shell="
			    (assoc-ref %build-inputs "bash")
			    "/bin/sh"))))
    (home-page "http://www.gnu.org/software/octave/")
    (synopsis "High-level language for numerical computation")
    (description "GNU Octave is a high-level interpreted language that is
specialized for numerical computations.  It can be used for both linear and
non-linear applications and it provides great support for visualizing results.
Work may be performed both at the interactive command-line as well as via
script files.")
    (license license:gpl3+)))

(define-public gmsh
  (package
    (name "gmsh")
    (version "2.11.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://www.geuz.org/gmsh/src/gmsh-"
                          version "-source.tgz"))
      (sha256
       (base32 "1ilplibvjgf7a905grpnclrvkmqy9fgrpl7xyp3w4yl1qc682v9b"))
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
       ("libx11" ,libx11)
       ("libxext" ,libxext)))
    (arguments
     `(#:configure-flags `("-DENABLE_METIS:BOOL=OFF"
                           "-DENABLE_BUILD_SHARED:BOOL=ON"
                           "-DENABLE_BUILD_DYNAMIC:BOOL=ON")
       #:phases (modify-phases %standard-phases
                  (replace
                   'check
                   (lambda _
                     (zero? (system* "make" "test"
                                     ;; Disable this test.  See
                                     ;; https://geuz.org/trac/gmsh/ticket/271
                                     "ARGS=-E component8_in_a_box")))))))
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

(define-public petsc
  (package
    (name "petsc")
    (version "3.6.2")
    (source
     (origin
      (method url-fetch)
      ;; The *-lite-* tarball does not contain the *large* documentation
      (uri (string-append "http://ftp.mcs.anl.gov/pub/petsc/release-snapshots/"
                          "petsc-lite-" version ".tar.gz"))
      (sha256
       (base32 "13h0m5f9xsdpps4lsp59iz2m7zkapwavq2zfkfvs3ab6sndla0l9"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("python" ,python-2)
       ("perl" ,perl)))
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
       #:phases
       (alist-replace
        'configure
        ;; PETSc's configure script is actually a python script, so we can't
        ;; run it with bash.
        (lambda* (#:key outputs (configure-flags '())
                  #:allow-other-keys)
          (let* ((prefix (assoc-ref outputs "out"))
                 (flags `(,(string-append "--prefix=" prefix)
                          ,@configure-flags)))
            (format #t "build directory: ~s~%" (getcwd))
            (format #t "configure flags: ~s~%" flags)
            (zero? (apply system* "./configure" flags))))
        (alist-cons-after
         'configure 'clean-local-references
         ;; Try to keep build directory names from leaking into compiled code
         (lambda* (#:key inputs outputs #:allow-other-keys)
           (let ((out (assoc-ref outputs "out")))
             (substitute* (find-files "." "^petsc(conf|machineinfo).h$")
               (((getcwd)) out))))
         (alist-cons-after
          'install 'clean-install
          ;; Try to keep installed files from leaking build directory names.
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((out     (assoc-ref outputs "out")))
              (substitute* (map (lambda (file)
                                  (string-append out "/lib/petsc/conf/" file))
                                '("petscvariables" "PETScConfig.cmake"))
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
                          ;; Once installed, should uninstall with Guix
                          "uninstall.py"))))
          %standard-phases)))))
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
    (version "3.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://slepc.upv.es/download/download.php?"
                           "filename=slepc-" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1pv5iqz2kc8sj49zsabyz4arnfpana8mjrhq31vzgk16xldk3d1a"))))
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
       #:phases
       (modify-phases %standard-phases
         (replace
          'configure
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
              (zero? (apply system* "./configure" flags)))))
         (add-after
          'install 'delete-doc
          ;; TODO: SLEPc installs HTML documentation alongside headers in
          ;; $out/include.  We'd like to move them to share/doc, but delete
          ;; them for now, as they are incomplete and installing the complete
          ;; documentation is difficult.
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out")))
              (for-each delete-file (find-files out "\\.html$")))))
         (add-after
          'install 'clean-install
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
    (license license:lgpl3)))

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
    (version "5.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://mumps.enseeiht.fr/MUMPS_"
                           version ".tar.gz"))
       (sha256
        (base32
         "1820jfp3mbl7n85765v5mp6p0gzqpgr4d2lrnhwj4gl7cwp5ndah"))
       (patches (list (search-patch "mumps-build-parallelism.patch")))))
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
         (replace
          'configure
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
         (replace
          'build
          ;; By default only the d-precision library is built.  Make with "all"
          ;; target so that all precision libraries and examples are built.
          (lambda _
            (zero? (system* "make" "all"
                            (format #f "-j~a" (parallel-job-count))))))
         (replace
          'check
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
         (replace
          'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (copy-recursively "lib" (string-append out "/lib"))
              (copy-recursively "include" (string-append out "/include"))
              (when (file-exists? "libseq/libmpiseq.a")
                (copy-file "libseq/libmpiseq.a"
                           (string-append out "/lib/libmpiseq.a")))))))))
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

(define-public superlu
  (package
    (name "superlu")
    (version "4.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://crd-legacy.lbl.gov/~xiaoye/SuperLU/"
                           "superlu_" version ".tar.gz"))
       (sha256
        (base32 "10b785s9s4x0m9q7ihap09275pq4km3k2hk76jiwdfdr5qr2168n"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("tcsh" ,tcsh)))
    (inputs
     `(("lapack" ,lapack)
       ("gfortran" ,gfortran)))
    (arguments
     `(#:parallel-build? #f
       #:tests? #f                      ;tests are run as part of `make all`
       #:phases
       (alist-replace
        'configure
        (lambda* (#:key inputs outputs #:allow-other-keys)
          (call-with-output-file "make.inc"
            (lambda (port)
              (format port "
PLAT        =
SuperLUroot = ~a
SUPERLULIB  = ~a/lib/libsuperlu.a
TMGLIB      = libtmglib.a
BLASDEF     = -DUSE_VENDOR_BLAS
BLASLIB     = -L~a/lib -lblas
LIBS        = $(SUPERLULIB) $(BLASLIB)
ARCH        = ar
ARCHFLAGS   = cr
RANLIB      = ranlib
CC          = gcc
PIC         = -fPIC
CFLAGS      = -O3 -DPRNTlevel=0 $(PIC)
NOOPTS      = -O0 $(PIC)
FORTRAN     = gfortran
FFLAGS      = -O2 $(PIC)
LOADER      = $(CC)
CDEFS       = -DAdd_"
                      (getcwd)
                      (assoc-ref outputs "out")
                      (assoc-ref inputs "lapack")))))
        (alist-cons-before
         'build 'create-install-directories
         (lambda* (#:key outputs #:allow-other-keys)
           (for-each
            (lambda (dir)
              (mkdir-p (string-append (assoc-ref outputs "out")
                                      "/" dir)))
            '("lib" "include")))
         (alist-replace
          'install
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
                        (find-files "SRC" ".*\\.h$"))))
          %standard-phases)))))
    (home-page "http://crd-legacy.lbl.gov/~xiaoye/SuperLU/")
    (synopsis "Supernodal direct solver for sparse linear systems")
    (description
     "SuperLU is a general purpose library for the direct solution of large,
sparse, nonsymmetric systems of linear equations on high performance machines.
The library is written in C and is callable from either C or Fortran.  The
library routines perform an LU decomposition with partial pivoting and
triangular system solves through forward and back substitution.  The library
also provides threshold-based ILU factorization preconditioners.")
    (license license:bsd-3)))

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
       (patches (list (search-patch "superlu-dist-scotchmetis.patch")))))
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
       (alist-replace
        'configure
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
                      (assoc-ref inputs "pt-scotch")))))
        (alist-cons-after
         'unpack 'remove-broken-symlinks
         (lambda _
           (for-each delete-file
                     (find-files "MAKE_INC" "\\.#make\\..*")))
         (alist-cons-before
          'build 'create-install-directories
          (lambda* (#:key outputs #:allow-other-keys)
            (for-each
             (lambda (dir)
               (mkdir-p (string-append (assoc-ref outputs "out")
                                       "/" dir)))
             '("lib" "include")))
          (alist-replace
           'check
           (lambda _
             (with-directory-excursion "EXAMPLE"
               (and
                (zero? (system* "mpirun" "-n" "2"
                                "./pddrive" "-r" "1" "-c" "2" "g20.rua"))
                (zero? (system* "mpirun" "-n" "2"
                                "./pzdrive" "-r" "1" "-c" "2" "cg20.cua")))))
           (alist-replace
            'install
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
                          (find-files "SRC" ".*\\.h$"))))
            %standard-phases)))))))
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
      (patches (list (search-patch "scotch-test-threading.patch")
                     (search-patch "pt-scotch-build-parallelism.patch")))))
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
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://p4est.github.io/release/p4est-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0faina2h5qsx3m2izbzaj9bbakma1krbbjmq43wrp1hcbyijflqb"))))
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
    (home-page "http://www.gnu.org/software/gsegrafix/")
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
    (version "5.36.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/maxima/Maxima-source/"
                           version "-source/" name "-" version ".tar.gz"))
       (sha256
        (base32
         "0x1rk659sn3cq0n5c90848ilzr1gb1wf0072fl6jhkdq00qgh2s0"))
       (patches (list (search-patch "maxima-defsystem-mkdir.patch")))))
    (build-system gnu-build-system)
    (inputs
     `(("gcl" ,gcl)
       ("gnuplot" ,gnuplot)                       ;for plots
       ("tk" ,tk)))                               ;Tcl/Tk is used by 'xmaxima'
    (native-inputs
     `(("texinfo" ,texinfo)
       ("perl" ,perl)))
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
       #:phases (alist-cons-before
                 'check 'pre-check
                 (lambda _
                   (chmod "src/maxima" #o555))
                 ;; Make sure the doc and emacs files are found in the
                 ;; standard location.  Also configure maxima to find gnuplot
                 ;; without having it on the PATH.
                 (alist-cons-after
                  'install 'post-install
                  (lambda* (#:key outputs inputs #:allow-other-keys)
                    (let* ((gnuplot (assoc-ref inputs "gnuplot"))
                          (out (assoc-ref outputs "out"))
                          (datadir (string-append out "/share/maxima/" ,version)))
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
                           (dump-port in out))))))
                  %standard-phases))))
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
    (version "15.04.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/wxmaxima/wxMaxima/"
                           version "/" name "-" version ".tar.gz"))
       (sha256
        (base32
         "1fm47ah4aw5qdjqhkz67w5fwhy8yfffa5z896crp0d3hk2bh4180"))))
    (build-system gnu-build-system)
    (inputs
     `(("wxwidgets" ,wxwidgets)
       ("maxima" ,maxima)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after
                   'install 'wrap-program
                   (lambda* (#:key inputs outputs #:allow-other-keys)
                     (wrap-program (string-append (assoc-ref outputs "out")
                                                  "/bin/wxmaxima")
                       `("PATH" ":" prefix
                         (,(string-append (assoc-ref inputs "maxima")
                                          "/bin"))))
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
    (version "6.400.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/arma/armadillo-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0bsgrmldlx77w5x26n3axj1hg6iw6csyw0dwl1flrbdwl51f9701"))))
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
    (license license:mpl2.0)))

(define-public armadillo-for-rcpparmadillo
  (package (inherit armadillo)
    (version "6.200.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/arma/armadillo-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1f69rlqhnf2wv8khyn2a8vi6gx1i72qgfy8b9b760ssk85dcl763"))))))

(define-public muparser
  (package
    (name "muparser")
    (version "2.2.5")
    (source
     (origin
       (method svn-fetch)
       (uri (svn-reference
             (url "http://muparser.googlecode.com/svn/trunk/")
             (revision 34)))
       (sha256
        (base32
         "1d6bdbhx9zj3srwj3m7c9hvr18gnx1fx43h6d25my7q85gicpcwn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--enable-samples=no")
       #:tests? #f)) ;no "check" target
    (home-page "http://muparser.beltoforion.de/")
    (synopsis "Fast parser library for mathematical expressions")
    (description
     "muParser is an extensible high performance math parser library.  It is
based on transforming an expression into a bytecode and precalculating
constant parts of it.")
    (license license:expat)))

(define-public openblas
  (package
    (name "openblas")
    (version "0.2.15")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/xianyi/OpenBLAS/tarball/v"
                           version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1k5f6vjlk54qlplk5m7xkbaw6g2y7dl50lwwdv6xsbcsgsbxfcpy"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f  ;no "check" target
       ;; DYNAMIC_ARCH is only supported on x86.  When it is disabled and no
       ;; TARGET is specified, OpenBLAS will tune itself to the build host, so
       ;; we need to disable substitutions.
       #:substitutable?
        ,(let ((system (or (%current-target-system) (%current-system))))
           (or (string-prefix? "x86_64" system)
               (string-prefix? "i686" system)
               (string-prefix? "mips" system)))
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
             "SHELL=bash"
             "NO_LAPACK=1"
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
                  (else '()))))
       ;; no configure script
       #:phases (alist-delete 'configure %standard-phases)))
    (inputs
     `(("fortran" ,gfortran)))
    (native-inputs
     `(("cunit" ,cunit)
       ("perl" ,perl)))
    (home-page "http://www.openblas.net/")
    (synopsis "Optimized BLAS library based on GotoBLAS")
    (description
     "OpenBLAS is a BLAS library forked from the GotoBLAS2-1.13 BSD version.")
    (license license:bsd-3)))

(define-public openlibm
  (package
    (name "openlibm")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/JuliaLang/openlibm/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0cwqqqlblj3kzp9aq1wnpfs1fl0qd1wp1xzm5shb09w06i4rh9nn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       ;; no configure script
       (alist-delete 'configure %standard-phases)
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
    ;; See LICENSE.md for details.
    (license (list license:expat
                   license:isc
                   license:bsd-2
                   license:public-domain
                   license:lgpl2.1+))))

(define-public openspecfun
  (package
    (name "openspecfun")
    (version "0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/JuliaLang/openspecfun/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0nsa3jjmlhcqkw5ba5ypbn3n0c8b6lc22zzlxnmxkxi9shhdx65z"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f  ;no "check" target
       #:make-flags
       (list (string-append "prefix=" (assoc-ref %outputs "out")))
       ;; no configure script
       #:phases (alist-delete 'configure %standard-phases)))
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
    (version "4.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://faculty.cse.tamu.edu/davis/SuiteSparse/SuiteSparse-"
             version ".tar.gz"))
       (sha256
        (base32
         "100hdzr0mf4mzlwnqpmwpfw4pymgsf9n3g0ywb1yps2nk1zbkdy5"))))
    (build-system gnu-build-system)
    (arguments
     '(#:parallel-build? #f ;cholmod build fails otherwise
       #:tests? #f  ;no "check" target
       #:make-flags
       (list "CC=gcc"
             "BLAS=-lblas"
             "TBB=-ltbb"
             "CHOLMOD_CONFIG=-DNPARTITION" ;required when METIS is not used
             (string-append "INSTALL_LIB="
                            (assoc-ref %outputs "out") "/lib")
             (string-append "INSTALL_INCLUDE="
                            (assoc-ref %outputs "out") "/include"))
       #:phases
       (alist-cons-before
        'install 'prepare-out
        ;; README.txt states that the target directories must exist prior to
        ;; running "make install".
        (lambda _
          (mkdir-p (string-append (assoc-ref %outputs "out") "/lib"))
          (mkdir-p (string-append (assoc-ref %outputs "out") "/include")))
        ;; no configure script
        (alist-delete 'configure %standard-phases))))
    (inputs
     `(("tbb" ,tbb)
       ("lapack" ,lapack)))
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
    (version "3.10.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/math-atlas/atlas"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "0bqh4bdnjdyww4mcpg6kn0x7338mfqbdgysn97dzrwwb26di7ars"))))
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
        (alist-cons-after
         'install 'install-doc
         (lambda* (#:key outputs inputs #:allow-other-keys)
           (let ((doc (string-append (assoc-ref outputs "doc")
                                     "/share/doc/atlas")))
             (mkdir-p doc)
             (fold (lambda (file previous)
                     (and previous (zero? (system* "cp" file doc))))
                   #t (find-files "../ATLAS/doc" ".*"))))
         (alist-cons-after
          'check 'check-pt
          (lambda _ (zero? (system* "make" "ptcheck")))
          ;; Fix files required to run configure.
          (alist-cons-before
           'configure 'fix-/bin/sh
           (lambda _
             ;; Use `sh', not `/bin/sh'.
             (substitute* (find-files "." "Makefile|configure|SpewMakeInc\\.c")
               (("/bin/sh")
                "sh")))
           ;; Fix /bin/sh in generated make files.
           (alist-cons-after
            'configure 'fix-/bin/sh-in-generated-files
            (lambda _
              (substitute* (find-files "." "^[Mm]ake\\.inc.*")
                (("/bin/sh")
                 "sh")))
            ;; ATLAS configure program does not accepts the default flags
            ;; passed by the 'gnu-build-system'.
            (alist-replace
             'configure
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
                               flags))))
             %standard-phases)))))))
    (synopsis "Automatically Tuned Linear Algebra Software")
    (description
     "ATLAS is an automatically tuned linear algebra software library
providing C and Fortran77 interfaces to a portably efficient BLAS
implementation, as well as a few routines from LAPACK.

Optimization occurs at build time.  For this reason, the library is built on
the machine where it is installed, without resorting to pre-built substitutes.

Before building the library, CPU throttling should be disabled.  This can be
done in the BIOS, or, on GNU/Linux, with the following commands:

cpufreq-selector -g performance -c 0
...
cpufreq-selector -g performance -c N-1

where N is the number of cores of your CPU.  Failure to do so will result in a
library with poor performance.")
    (license license:bsd-3)))

(define-public glm
  (package
    (name "glm")
    (version "0.9.6.3")
    (source
     (origin
       (method url-fetch)
      (uri (string-append "mirror://sourceforge/ogl-math/glm-"
                          version ".zip"))
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
    (version "5.5.2.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/lpsolve/lpsolve/" version
                          "/lp_solve_" version "_source.tar.gz"))
      (sha256
       (base32
        "176c7f023mb6b8bfmv4rfqnrlw88lsg422ca74zjh19i2h5s69sq"))
      (modules '((guix build utils)))
      (snippet
       '(substitute* (list "lp_solve/ccc" "lpsolve55/ccc")
          (("^c=cc") "c=gcc")
          ;; Pretend to be on a 64 bit platform to obtain a common directory
          ;; name for the build results on all architectures; nothing else
          ;; seems to depend on it.
          (("^PLATFORM=.*$") "PLATFORM=ux64\n")))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no check target
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda _
             (with-directory-excursion "lpsolve55"
               (system* "bash" "ccc"))
             (with-directory-excursion "lp_solve"
               (system* "bash" "ccc"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lib (string-append out "/lib"))
                    ;; This is where LibreOffice expects to find the header
                    ;; files, and where they are installed by Debian.
                    (include (string-append out "/include/lpsolve")))
               (mkdir-p lib)
               (copy-file "lpsolve55/bin/ux64/liblpsolve55.a"
                          (string-append lib "/liblpsolve55.a"))
               (copy-file "lpsolve55/bin/ux64/liblpsolve55.so"
                          (string-append lib "/liblpsolve55.so"))
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
    (version "8.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/dealii/dealii/releases/"
                           "download/v" version "/dealii-" version ".tar.gz"))
       (sha256
        (base32
         "185jych0gdnpkjwxni7pd0dda149492zwq2457xdjg76bzj78mnp"))
       (patches (list (search-patch "dealii-p4est-interface.patch")))
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
       #:configure-flags '("-DCOMPAT_FILES=OFF") ;Follow new directory structure
       #:phases (modify-phases %standard-phases
                  (add-after
                   'install 'hint-example-prefix
                   ;; Set Cmake hints in examples so that they can find this
                   ;; deal.II when configuring.
                   (lambda* (#:key outputs #:allow-other-keys)
                     (let* ((out (assoc-ref %outputs "out"))
                            (exmpl (string-append out "/share/doc"
                                                  "/dealii/examples")))
                       (substitute* (find-files exmpl "CMakeLists.txt")
                         (("([[:space:]]*HINTS.*)\n" _ line)
                          (string-append line " $ENV{HOME}/.guix-profile "
                                         out "\n")))
                       #t))))))
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
    (native-inputs
     `(("unzip" ,unzip)))
    (inputs
     `(("hdf5" ,hdf5)
       ("octave" ,octave)
       ("python" ,python-2) ; print syntax
       ;; ("python2-numpy" ,python2-numpy) ; only required for the tests
       ("zlib" ,zlib)))
    (arguments
     `(#:tests? #f)) ; The test data are downloaded from the Internet.
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
        (uri
          (string-append
            "mirror://sourceforge/w-calc/wcalc-" version ".tar.bz2"))
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
              (uri (string-append "mirror://sourceforge/xaos/xaos-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "15cd1cx1dyygw6g2nhjqq3bsfdj8sj8m4va9n75i0f3ryww3x7wq"))))
    (build-system gnu-build-system)
    (native-inputs `(("gettext" ,gnu-gettext)))
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
    (home-page "http://www.gnu.org/software/xaos/")
    (license license:gpl2+)))
