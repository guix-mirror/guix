;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
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
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages bison)
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
  #:use-module (gnu packages less)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages tcsh)
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
    (version "0.8.3")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/pspp/pspp-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0vri2pzvmm38qaihfvwlry30f40lcnps4blg59ixic4q20ldxf5d"))))
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
     `(("glib" ,glib "bin")             ;for glib-genmarshal
       ("perl" ,perl)
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
    (native-inputs `(("patchelf" ,patchelf))) ;for augment-rpath
    (inputs `(("fortran" ,gfortran-4.8)
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
      (uri (string-append "http://www.hdfgroup.org/ftp/HDF5/releases/hdf5-"
                          version "/src/hdf5-"
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

(define-public gmsh
  (package
    (name "gmsh")
    (version "2.8.4")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://www.geuz.org/gmsh/src/gmsh-"
                          version "-source.tgz"))
      (sha256
       (base32 "0jv2yvk28w86rx5mvjkb0w12ff2jxih7axnpvznpd295lg5jg7hr"))
      (modules '((guix build utils)))
      (snippet
       ;; Remove non-free METIS code
       '(delete-file-recursively "contrib/Metis"))))
    (build-system cmake-build-system)
    (native-inputs `(("patchelf" ,patchelf))) ;for augment-rpath
    (propagated-inputs
     `(("fltk" ,fltk)
       ("gfortran" ,gfortran-4.8)
       ("gmp" ,gmp)
       ("hdf5-lib" ,hdf5 "lib")
       ("hdf5-include" ,hdf5 "include")
       ("lapack" ,lapack)
       ("mesa" ,mesa)
       ("libx11" ,libx11)
       ("libxext" ,libxext)))
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

(define-public petsc
  (package
    (name "petsc")
    (version "3.4.4")
    (source
     (origin
      (method url-fetch)
      ;; The *-lite-* tarball does not contain the *large* documentation
      (uri (string-append "http://ftp.mcs.anl.gov/pub/petsc/release-snapshots/"
                          "petsc-lite-" version ".tar.gz"))
      (sha256
       (base32 "0v5dg6dhdjpi5ianvd4mm6hsvxzv1bsxwnh9f9myag0a0d9xk9iv"))
      (patches
       (list (search-patch "petsc-fix-threadcomm.patch")))))
    (build-system gnu-build-system)
    (native-inputs
     `(("python" ,python-2)
       ("perl" ,perl)))
    (inputs
     `(("gfortran" ,gfortran-4.8)
       ("lapack" ,lapack)
       ("superlu" ,superlu)
       ;; leaving out hdf5 and fftw, as petsc expects them to be built with mpi
       ;; leaving out opengl, as configuration seems to only be for mac
       ))
    (arguments
     `(#:test-target "test"
       #:parallel-build? #f
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
            (let ((out     (assoc-ref outputs "out"))
                  (fortran (assoc-ref inputs  "gfortran")))
              (substitute* (map (lambda (file)
                                  (string-append out "/" file))
                                '("conf/petscvariables"
                                  "conf/PETScConfig.cmake"))
                (((getcwd)) out))
              ;; Make compiler references point to the store
              (substitute* (string-append out "/conf/petscvariables")
                (("= g(cc|\\+\\+|fortran)" _ suffix)
                 (string-append "= " fortran "/bin/g" suffix)))
              ;; PETSc installs some build logs, which aren't necessary.
              (for-each (lambda (file)
                          (let ((f (string-append out "/" file)))
                            (when (file-exists? f)
                              (delete-file f))))
                        '("conf/configure.log"
                          "conf/make.log"
                          "conf/test.log"
                          "conf/error.log"
                          "conf/RDict.db"
                          ;; Once installed, should uninstall with Guix
                          "conf/uninstall.py"))))
          %standard-phases)))))
    (home-page "http://www.mcs.anl.gov/petsc")
    (synopsis "Library to solve PDEs")
    (description "PETSc, pronounced PET-see (the S is silent), is a suite of
data structures and routines for the scalable (parallel) solution of
scientific applications modeled by partial differential equations.")
    (license (license:bsd-style
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
       ("gfortran" ,gfortran-4.8)))
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
     `(("gfortran" ,gfortran-4.8)))
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
    (version "6.0.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://gforge.inria.fr/frs/download.php/31831/"
                          "scotch_" version ".tar.gz"))
      (sha256
       (base32 "0yfqf9lk7chb3h42777x42x4adx0v3n0b41q0cdqrdmscp4iczp5"))
      (patches (list (search-patch "scotch-test-threading.patch")))))
    (build-system gnu-build-system)
    (inputs
     `(("zlib" ,zlib)
       ("flex" ,flex)
       ("bison" ,bison)))
    (arguments
     `(#:phases
       (alist-cons-after
        'unpack 'chdir-to-src
        (lambda _ (chdir "src"))
        (alist-replace
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
CCS = gcc
CCP = mpicc
CCD = gcc
CPPFLAGS =~{ -D~a~}
CFLAGS = -O2 -g $(CPPFLAGS)
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
                         ;; TODO: Define once our MPI supports
                         ;; MPI_THREAD_MULTIPLE
                         ;; "SCOTCH_PTHREAD"
                         ;; "SCOTCH_PTHREAD_NUMBER=2"
                         "restrict=__restrict")))))
         (alist-replace
          'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (mkdir out)
              (zero? (system* "make"
                              (string-append "prefix=" out)
                              "install"))))
          %standard-phases)))))
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
        `(alist-replace
          'build
          ;; TODO: Would like to add parallelism here
          (lambda _
            (and
             (zero? (system* "make" "ptscotch"))
             ;; Install the serial metis compatibility library
             (zero? (system* "make" "-C" "libscotchmetis" "install"))))
          (alist-replace
           'check
           (lambda _ (zero? (system* "make" "ptcheck")))
           (alist-replace
            'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (mkdir out)
                (zero? (system* "make"
                                (string-append "prefix=" out)
                                "install"))))
            ,scotch-phases))))))
    (synopsis "Programs and libraries for graph algorithms (with MPI)")))

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
