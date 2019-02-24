;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018 Paul Garlick <pgarlick@tourbillion-technology.com>
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

(define-module (gnu packages simulation)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1))

(define-public openfoam
  (package
    (name "openfoam")
    (version "4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://dl.openfoam.org/source/"
             (string-map (lambda (x) (if (eq? x #\.) #\- x)) version)))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0cgxh4h2hf50qbvvdg5miwc2nympb0nrv3md96vb3gbs9vk8vq9d"))
       (patches (search-patches "openfoam-4.1-cleanup.patch"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Include <sys/sysmacros.h>, which is where glibc >= 2.28 provides
           ;; 'major' and 'minor'.
           (substitute* "src/OSspecific/POSIX/fileStat.C"
             (("#include <unistd\\.h>")
              "#include <unistd.h>\n#include <sys/sysmacros.h>\n"))
           #t))))
    (build-system gnu-build-system)
    (inputs
     `(("boost" ,boost)
       ("cgal" ,cgal)
       ("flex" ,flex)
       ("git" ,git)
       ("gmp" ,gmp)
       ("libxt" ,libxt)
       ("metis" ,metis)
       ("mpfr" ,mpfr)
       ("ncurses" ,ncurses)
       ("readline" ,readline)
       ("scotch" ,pt-scotch32)
       ("zlib" ,zlib)))
    (native-inputs
     `(("bison" ,bison)))
    (propagated-inputs
     `(("gzip" ,gzip)
       ("gnuplot" ,gnuplot)
       ("openmpi" ,openmpi)))
    (outputs '("debug"                  ;~60MB
               "out"))
    (arguments
     `( ;; Executable files and shared libraries are located in the 'platforms'
       ;; subdirectory.
       #:strip-directories (list (string-append
                                  "lib/OpenFOAM-" ,version
                                  "/platforms/linux64GccDPInt32Opt/bin")
                                 (string-append
                                  "lib/OpenFOAM-" ,version
                                  "/platforms/linux64GccDPInt32Opt/lib"))
       #:tests? #f                                ; no tests to run

       #:modules ((ice-9 ftw)
                  (ice-9 regex)
                  (guix build gnu-build-system)
                  (guix build utils))

       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'rename-build-directory
                    (lambda _
                      (chdir "..")
                      ;; Use 'OpenFOAM-version' convention to match the file
                      ;; name expectations in the build phase.
                      (let ((unpack-dir (string-append
                                         (getcwd) "/"
                                         (list-ref (scandir (getcwd) (lambda (name)
                                                                       (string-match "^OpenFOAM" name))) 0)))
                            (build-dir (string-append
                                        (getcwd) "/OpenFOAM-" ,version)))
                        (rename-file unpack-dir build-dir) ; rename build directory
                        (chdir (basename build-dir))) ; move to build directory
                      #t))
                  (delete 'configure)             ; no configure phase
                  (replace 'build
                    (lambda _
                      (let ((libraries '("boost" "cgal" "gmp" "metis" "mpfr" "scotch")))
                        ;; set variables to define store paths
                        (for-each (lambda (library)
                                    (setenv (string-append
                                             (string-upcase library) "_ROOT")
                                            (assoc-ref %build-inputs library))) libraries))
                      ;; set variables to define package versions
                      (setenv "SCOTCHVERSION" ,(package-version scotch))
                      (setenv "METISVERSION" ,(package-version metis))
                      ;; set variable to pass extra 'rpath' arguments to linker
                      (setenv "LDFLAGS"
                              (string-append
                               "-Wl,"
                               "-rpath=" %output "/lib/OpenFOAM-" ,version
                               "/platforms/linux64GccDPInt32Opt/lib,"
                               "-rpath=" %output "/lib/OpenFOAM-" ,version
                               "/platforms/linux64GccDPInt32Opt/lib/dummy"))
                      ;; compile OpenFOAM libraries and applications
                      (zero? (system (format #f
                                             "source ./etc/bashrc && ./Allwmake -j~a"
                                             (parallel-job-count))))))
                  (add-after 'build 'update-configuration-files
                    (lambda _
                      ;; record store paths and package versions in
                      ;; configuration files
                      (substitute* "etc/config.sh/CGAL"
                        (("$BOOST_ROOT") (getenv "BOOST_ROOT")))
                      (substitute* "etc/config.sh/CGAL"
                        (("$CGAL_ROOT") (getenv "CGAL_ROOT")))
                      (substitute* "etc/config.sh/metis"
                        (("$METIS_ROOT") (getenv "METIS_ROOT")))
                      (substitute* "etc/config.sh/metis"
                        (("$METISVERSION") (getenv "METISVERSION")))
                      (substitute* "etc/config.sh/scotch"
                        (("$SCOTCH_ROOT") (getenv "SCOTCH_ROOT")))
                      (substitute* "etc/config.sh/scotch"
                        (("$SCOTCHVERSION") (getenv "SCOTCHVERSION")))
                      (substitute* "etc/config.sh/settings"
                        (("$GMP_ROOT") (getenv "GMP_ROOT")))
                      (substitute* "etc/config.sh/settings"
                        (("$MPFR_ROOT") (getenv "MPFR_ROOT")))
                      ;; reset lockDir variable to refer to write-enabled
                      ;; directory
                      (substitute* "wmake/wmake"
                        (("        lockDir=.*$")
                         "        lockDir=$HOME/.$WM_PROJECT/.wmake\n"))
                      (substitute* "wmake/wmakeScheduler"
                        (("lockDir=.*$")
                         "lockDir=$HOME/.$WM_PROJECT/.wmake\n"))
                      (substitute* "wmake/wmakeSchedulerUptime"
                        (("lockDir=.*$")
                         "lockDir=$HOME/.$WM_PROJECT/.wmake\n"))
                      #t))
                  (add-after 'build 'cleanup
                    ;; Avoid unncessary, voluminous object and dep files.
                    (lambda _
                      (delete-file-recursively
                       "platforms/linux64GccDPInt32Opt/src")
                      (delete-file-recursively
                       "platforms/linux64GccDPInt32OptSYSTEMOPENMPI")
                      (for-each delete-file (find-files "." "\\.o$"))
                      #t))
                  (replace 'install
                    (lambda _
                      ;; use 'OpenFOAM-version' convention
                      (let ((install-dir (string-append
                                          %output "/lib/OpenFOAM-" ,version)))
                        (mkdir-p install-dir)     ; create install directory
                        ;; move contents of build directory to install directory
                        (copy-recursively "." install-dir))))
                  (add-after 'install 'add-symbolic-link
                    (lambda _
                      ;; add symbolic link for standard 'bin' directory
                      (symlink
                       (string-append "./lib/OpenFOAM-" ,version
                                      "/platforms/linux64GccDPInt32Opt/bin")
                       (string-append %output "/bin"))
                      #t)))))
    ;; Note:
    ;;  Tutorial files are installed read-only in /gnu/store.
    ;;  To allow write permissions on files copied from the store a
    ;;  'chmod' step is needed before running the applications.  For
    ;;  example, from a user's login:
    ;;  $ source $GUIX_PROFILE/lib/OpenFOAM-4.1/etc/bashrc
    ;;  $ mkdir -p $FOAM_RUN
    ;;  $ cd $FOAM_RUN
    ;;  $ cp -r $FOAM_TUTORIALS/incompressible/simpleFoam/pitzDaily .
    ;;  $ cd pitzDaily
    ;;  $ chmod -R u+w .
    ;;  $ blockMesh
    (synopsis "Framework for numerical simulation of fluid flow")
    (description "OpenFOAM provides a set of solvers and methods for tackling
problems in the field of Computational Fluid Dynamics (CFD).  It is written in
C++.  Governing equations such as the Navier-Stokes equations can be solved in
integral form.  Physical processes such as phase change, droplet transport and
chemical reaction can be modelled.  Numerical methods are included to deal with
sharp gradients, such as those encountered in flows with shock waves and flows
with gas/liquid interfaces.  Large problems may be split into smaller, connected
problems for efficient solution on parallel systems.")
    (license license:gpl3+)
    (home-page "https://openfoam.org")))

(define-public python-fenics-dijitso
  (package
    (name "python-fenics-dijitso")
    (version "2018.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "fenics-dijitso" version))
        (sha256
          (base32
            "1qax2f52qsjbd1h5lk5i5shp448qlakxabjjybrfc1w823p0yql9"))))
    (build-system python-build-system)
    (inputs
     `(("openmpi" ,openmpi)
       ("python-numpy" ,python-numpy)))
    (native-inputs
     `(("python-pytest-cov" ,python-pytest-cov)))
    (propagated-inputs
     `(("python-mpi4py" ,python-mpi4py)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'build 'mpi-setup
           ,%openmpi-setup)
         (replace 'check
           (lambda _
             (setenv "HOME" "/tmp")
             (setenv "PYTHONPATH"
                     (string-append (getcwd) ":" (getenv "PYTHONPATH")))
             (with-directory-excursion "test"
               ;; Disable parallel tests to avoid race condition.  See
               ;; https://github.com/pytest-dev/pytest-cov/issues/237.
               (substitute* "runtests.sh"
                 (("for p in 1 4 8 16; do")
                  "for p in 1; do"))
               (invoke "./runtests.sh"))
             #t)))))
    (home-page "https://bitbucket.org/fenics-project/dijitso/")
    (synopsis "Distributed just-in-time building of shared libraries")
    (description
      "Dijitso provides a core component of the @code{FEniCS} framework,
namely the just-in-time compilation of C++ code that is generated from
Python modules.  It is called from within a C++ library, using ctypes
to import the dynamic shared library directly.

As long as the compiled code can provide a simple factory function to
a class implementing a predefined C++ interface, there is no limit to
the complexity of that interface.  Parallel support depends on the
@code{mpi4py} interface.")
    (license license:lgpl3+)))

(define-public python-fenics-ufl
  (package
    (name "python-fenics-ufl")
    (version "2018.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "fenics-ufl" version))
        (sha256
          (base32
            "1fq8yc86s1s3c8c0b1rc2vf265q0hrkzg57100fg1nghcz0p4vla"))))
    (build-system python-build-system)
    (inputs
     `(("python-numpy" ,python-numpy)))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (setenv "PYTHONPATH"
                     (string-append (getcwd) ":" (getenv "PYTHONPATH")))
             (with-directory-excursion "test"
               (invoke "py.test"))
             #t)))))
    (home-page "https://bitbucket.org/fenics-project/ufl/")
    (synopsis "Unified language for form-compilers")
    (description "The Unified Form Language (UFL) is a domain specific
language for declaration of finite element discretizations of
variational forms.  More precisely, it defines a flexible interface
for choosing finite element spaces and defining expressions for weak
forms in a notation close to mathematical notation.

UFL is part of the FEniCS Project.")
    (license license:lgpl3+)))

(define-public python-fenics-fiat
  (package
    (name "python-fenics-fiat")
    (version "2018.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "fenics-fiat" version))
        (sha256
          (base32
            "0fmjd93r6bwf6xs8csw86qzphrnr66xwv7f372w59gmq8mg6rljc"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (propagated-inputs
     `(("python-numpy" ,python-numpy)
       ("python-sympy" ,python-sympy)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (setenv "PYTHONPATH"
                     (string-append (getcwd) ":" (getenv "PYTHONPATH")))
             (with-directory-excursion "test"
               (invoke "py.test" "unit/"))
             #t)))))
    (home-page "https://bitbucket.org/fenics-project/fiat/")
    (synopsis "Tabulation of finite element function spaces")
    (description
      "The FInite element Automatic Tabulator (FIAT) supports
generation of arbitrary order instances of the Lagrange elements on
lines, triangles, and tetrahedra.  It is also capable of generating
arbitrary order instances of Jacobi-type quadrature rules on the same
element shapes.  Further, H(div) and H(curl) conforming finite element
spaces such as the families of Raviart-Thomas, Brezzi-Douglas-Marini
and Nedelec are supported on triangles and tetrahedra.  Upcoming
versions will also support Hermite and nonconforming elements.

FIAT is part of the FEniCS Project.")
    (license license:lgpl3+)))

(define-public python-fenics-ffc
  (package
    (name "python-fenics-ffc")
    (version "2018.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "fenics-ffc" version))
        (sha256
          (base32
            "1b2ia5vlkw298x7rf0k2p3ihlpwkwgc98p3s6sbpds3hqmfrzdz9"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (propagated-inputs
     `(("python-fenics-dijitso" ,python-fenics-dijitso)
       ("python-fenics-fiat" ,python-fenics-fiat)
       ("python-fenics-ufl" ,python-fenics-ufl)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (setenv "HOME" (getcwd))
             (setenv "PYTHONPATH"
                     (string-append (getcwd) ":" (getenv "PYTHONPATH")))
             (with-directory-excursion "test"
               ;; FIXME: the tests in subdirectory
               ;; 'unit/ufc/finite_element' require the ffc_factory
               ;; extension module.  This module, located in the 'libs'
               ;; subdirectory, needs to be built and made accessible
               ;; prior to running the tests.
               (invoke "py.test" "unit/" "--ignore=unit/ufc/")
               (with-directory-excursion "uflacs"
                 (invoke "py.test" "unit/")))
             #t)))))
    (home-page "https://bitbucket.org/fenics-project/ffc/")
    (synopsis "Compiler for finite element variational forms")
    (description "The FEniCS Form Compiler (FFC) is a compiler for
finite element variational forms.  From a high-level description of
the form, it generates efficient low-level C++ code that can be used
to assemble the corresponding discrete operator (tensor).  In
particular, a bilinear form may be assembled into a matrix and a
linear form may be assembled into a vector.  FFC may be used either
from the command line (by invoking the @code{ffc} command) or as a
Python module (@code{import ffc}).

FFC is part of the FEniCS Project.")
    ;; There are two files released with a public domain licence;
    ;; ufc.h and ufc_geometry.h, in subdirectory 'ffc/backends/ufc'.
    (license (list license:public-domain license:lgpl3+))))

(define-public fenics-dolfin
  (package
    (name "fenics-dolfin")
    (version "2018.1.0.post1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
              "https://bitbucket.org/fenics-project/dolfin/get/"
              version ".tar.gz"))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
           "12zkk8j3xsg6l8p0ggwsl03084vlcivw4h99b7z9kndg7k89b3ya"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            ;; Make sure we don't use the bundled test framework.
            (delete-file-recursively "test/unit/cpp/catch")
            (substitute* "test/unit/cpp/main.cpp"
              ;; Use standard search paths for 'catch' header file.
              (("#include.*")
               "#include <catch.hpp>\n"))
            (substitute* "test/unit/cpp/CMakeLists.txt"
              ;; Add extra include directories required by the unit tests.
              (("(^target_link_libraries.*)" line)
               (string-append line "\n"
                              "target_include_directories("
                              "unittests PRIVATE "
                              "${DOLFIN_SOURCE_DIR} "
                              "${DOLFIN_SOURCE_DIR}/dolfin "
                              "${DOLFIN_BINARY_DIR})\n"))
              (("(^set\\(CATCH_INCLUDE_DIR ).*(/catch\\))" _ front back)
               (string-append front
                              "$ENV{CATCH_DIR}"
                              "/include" back "\n")))
            (substitute* "demo/CMakeLists.txt"
              ;; Add extra include directories required by the demo tests.
              (("(^#find_package.*)" line)
               (string-append line "\n"
                              "include_directories("
                              "${DOLFIN_SOURCE_DIR} "
                              "${DOLFIN_SOURCE_DIR}/dolfin "
                              "${DOLFIN_BINARY_DIR})\n")))
            #t))))
    (build-system cmake-build-system)
    (inputs
     `(("blas" ,openblas)
       ("boost" ,boost)
       ("eigen" ,eigen)
       ("hdf5" ,hdf5-parallel-openmpi)
       ("lapack" ,lapack)
       ("libxml2" ,libxml2)
       ("openmpi" ,openmpi)
       ("python" ,python-3)
       ("scotch" ,pt-scotch32)
       ("suitesparse" ,suitesparse)
       ("sundials" ,sundials-openmpi)
       ("zlib" ,zlib)))
    (native-inputs
     `(("catch" ,catch-framework2)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("ffc" ,python-fenics-ffc)
       ("petsc" ,petsc-openmpi)
       ("slepc" ,slepc-openmpi)))
    (arguments
     `(#:configure-flags
       `("-DDOLFIN_ENABLE_DOCS:BOOL=OFF"
         "-DDOLFIN_ENABLE_HDF5:BOOL=ON"
         "-DDOLFIN_ENABLE_MPI:BOOL=ON"
         "-DDOLFIN_ENABLE_PARMETIS:BOOL=OFF"
         "-DDOLFIN_ENABLE_SCOTCH:BOOL=ON"
         "-DDOLFIN_ENABLE_SUNDIALS:BOOL=ON"
         "-DDOLFIN_ENABLE_TRILINOS:BOOL=OFF")
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-usr-bin-file 'mpi-setup
           ,%openmpi-setup)
         (add-after 'patch-source-shebangs 'set-paths
           (lambda _
             ;; Define paths to store locations.
             (setenv "BLAS_DIR" (assoc-ref %build-inputs "blas"))
             (setenv "CATCH_DIR" (assoc-ref %build-inputs "catch"))
             (setenv "LAPACK_DIR" (assoc-ref %build-inputs "lapack"))
             (setenv "PETSC_DIR" (assoc-ref %build-inputs "petsc"))
             (setenv "SLEPC_DIR" (assoc-ref %build-inputs "slepc"))
             (setenv "SCOTCH_DIR" (assoc-ref %build-inputs "scotch"))
             (setenv "SUNDIALS_DIR" (assoc-ref %build-inputs "sundials"))
             (setenv "UMFPACK_DIR" (assoc-ref %build-inputs "suitesparse"))
             #t))
         (add-before 'check 'pre-check
           (lambda _
             ;; The Dolfin repository uses git-lfs, whereby web links are
             ;; substituted for large files.  Guix does not currently support
             ;; git-lfs, so only the links are downloaded.  The tests that
             ;; require the absent meshes cannot run and are skipped.
             ;;
             ;; Two other serial tests fail and are skipped.
             ;; i) demo_stokes-iterative_serial,
             ;;   The MPI_Comm_rank() function was called before MPI_INIT was
             ;;   invoked
             ;; ii) demo_multimesh-stokes_serial:
             ;;   Warning: Found no facets matching domain for boundary
             ;;   condition.
             ;;
             ;; One mpi test fails and is skipped.
             ;; i) demo_stokes-iterative_mpi:
             ;;   The MPI_Comm_rank() function was called before MPI_INIT was
             ;;   invoked
             (call-with-output-file "CTestCustom.cmake"
               (lambda (port)
                 (display
                   (string-append
                    "set(CTEST_CUSTOM_TESTS_IGNORE "
                    "demo_bcs_serial "
                    "demo_bcs_mpi "
                    "demo_eigenvalue_serial "
                    "demo_eigenvalue_mpi "
                    "demo_navier-stokes_serial "
                    "demo_navier-stokes_mpi "
                    "demo_stokes-taylor-hood_serial "
                    "demo_stokes-taylor-hood_mpi "
                    "demo_subdomains_serial "
                    "demo_advection-diffusion_serial "
                    "demo_advection-diffusion_mpi "
                    "demo_auto-adaptive-navier-stokes_serial "
                    "demo_contact-vi-snes_serial "
                    "demo_contact-vi-snes_mpi "
                    "demo_contact-vi-tao_serial "
                    "demo_contact-vi-tao_mpi "
                    "demo_curl-curl_serial "
                    "demo_curl-curl_mpi "
                    "demo_dg-advection-diffusion_serial "
                    "demo_dg-advection-diffusion_mpi "
                    "demo_elasticity_serial "
                    "demo_elasticity_mpi "
                    "demo_elastodynamics_serial "
                    "demo_elastodynamics_mpi "
                    "demo_lift-drag_serial "
                    "demo_lift-drag_mpi "
                    "demo_mesh-quality_serial "
                    "demo_mesh-quality_mpi "
                    "demo_multimesh-stokes_serial "
                    "demo_stokes-iterative_serial "
                    "demo_stokes-iterative_mpi "
                    ")\n") port)))
             #t))
         (replace 'check
           (lambda _
             (and (invoke "make" "unittests")
                  (invoke "make" "demos")
                  (invoke "ctest" "-R" "unittests")
                  (invoke "ctest" "-R" "demo" "-R" "serial")
                  (invoke "ctest" "-R" "demo" "-R" "mpi")))))))
    (home-page "https://bitbucket.org/fenics-project/dolfin/")
    (synopsis "Problem solving environment for differential equations")
    (description
      "DOLFIN is a computational framework for finding numerical
solutions to problems described by differential equations.  Numerical
models in DOLFIN are constructed using general families of finite
elements.  Data structures are provided for discretizing the governing
system on a computational mesh.  A compact syntax, similar to
mathematical notation, is made available for defining function spaces
and expressing variational forms.  Interfaces to specialized matrix
solvers are provided for solving the resultant linear systems.

@code{fenics-dolfin} is part of the FEniCS project.  It is the C++
user interface to the FEniCS core components and external libraries.")
    ;; The source code for the DOLFIN C++ library is licensed under the
    ;; GNU Lesser General Public License, version 3 or later, with the
    ;; following exceptions:
    ;;
    ;; public-domain: dolfin/geometry/predicates.cpp
    ;;                dolfin/geometry/predicates.h
    ;;
    ;; zlib:          dolfin/io/base64.cpp
    ;;                dolfin/io/base64.h
    ;;
    ;; expat:         dolfin/io/pugiconfig.hpp
    ;;                dolfin/io/pugixml.cpp
    ;;                dolfin/io/pugixml.hpp
    (license (list license:public-domain
                   license:zlib
                   license:expat
                   license:lgpl3+))))

(define-public fenics
  (package (inherit fenics-dolfin)
    (name "fenics")
    (build-system python-build-system)
    (inputs
     `(("pybind11" ,pybind11)
       ("python-matplotlib" ,python-matplotlib)
       ,@(alist-delete "python" (package-inputs fenics-dolfin))))
    (native-inputs
     `(("cmake" ,cmake)
       ("ply" ,python-ply)
       ("pytest" ,python-pytest)
       ("python-decorator" ,python-decorator)
       ("python-pkgconfig" ,python-pkgconfig)
       ,@(package-native-inputs fenics-dolfin)))
    (propagated-inputs
     `(("dolfin" ,fenics-dolfin)
       ("petsc4py" ,python-petsc4py)
       ("slepc4py" ,python-slepc4py)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'patch-source-shebangs 'set-paths
           (lambda _
             ;; Define paths to store locations.
             (setenv "PYBIND11_DIR" (assoc-ref %build-inputs "pybind11"))
             ;; Move to python sub-directory.
             (chdir "python")
             #t))
         (add-after 'build 'mpi-setup
           ,%openmpi-setup)
         (add-before 'check 'pre-check
           (lambda _
             ;; Exclude tests that require meshes supplied by git-lfs.
             (substitute* "demo/test.py"
               (("(.*stem !.*)" line)
                (string-append
                 line "\n"
                 "excludeList = [\n"
                 "'multimesh-quadrature', \n"
                 "'multimesh-marking', \n"
                 "'mixed-poisson-sphere', \n"
                 "'mesh-quality', \n"
                 "'lift-drag', \n"
                 "'elastodynamics', \n"
                 "'dg-advection-diffusion', \n"
                 "'curl-curl', \n"
                 "'contact-vi-tao', \n"
                 "'contact-vi-snes', \n"
                 "'collision-detection', \n"
                 "'buckling-tao', \n"
                 "'auto-adaptive-navier-stokes', \n"
                 "'advection-diffusion', \n"
                 "'subdomains', \n"
                 "'stokes-taylor-hood', \n"
                 "'stokes-mini', \n"
                 "'navier-stokes', \n"
                 "'eigenvalue']\n"
                 "demos = ["
                 "d for d in demos if d[0].stem not in "
                 "excludeList]\n")))
             (setenv "HOME" (getcwd))
             (setenv "PYTHONPATH"
                     (string-append
                      (getcwd) "/build/lib.linux-x86_64-"
                      ,(version-major+minor (package-version python)) ":"
                      (getenv "PYTHONPATH")))
             ;; Restrict OpenBLAS to MPI-only in preference to MPI+OpenMP.
             (setenv "OPENBLAS_NUM_THREADS" "1")
             #t))
         (replace 'check
           (lambda _
             (with-directory-excursion "test"
               ;; Note: The test test_snes_set_from_options() in the file
               ;; unit/nls/test_PETScSNES_solver.py fails and is ignored.
               (and (invoke "py.test" "unit" "--ignore"
                            "unit/nls/test_PETScSNES_solver.py")
                    (invoke "mpirun" "-np" "3" "python" "-B" "-m"
                            "pytest" "unit" "--ignore"
                            "unit/nls/test_PETScSNES_solver.py")))
             (with-directory-excursion "demo"
               ;; Check demos.
               (invoke "python" "generate-demo-files.py")
               (and (invoke "python" "-m" "pytest" "-v" "test.py")
                    (invoke "python" "-m" "pytest" "-v" "test.py"
                            "--mpiexec=mpiexec" "--num-proc=3")))
             #t))
         (add-after 'install 'install-demo-files
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((demos (string-append
                            (assoc-ref outputs "out")
                            "/share/python-dolfin/demo")))
               (mkdir-p demos)
               (with-directory-excursion "demo"
                 (for-each (lambda (file)
                             (let* ((dir (dirname file))
                                    (tgt-dir (string-append demos "/" dir)))
                               (unless (equal? "." dir)
                                 (mkdir-p tgt-dir)
                                 (install-file file tgt-dir))))
                           (find-files "." ".*\\.(py|gz|xdmf)$"))))
             #t)))))
    (home-page "https://fenicsproject.org/")
    (synopsis "High-level environment for solving differential equations")
    (description
      "@code{fenics} is a computing platform for solving general classes of
problems that involve differential equations.  @code{fenics} facilitates
access to efficient methods for dealing with ordinary differential
equations (ODEs) and partial differential equations (PDEs).  Systems of
equations such as these are commonly encountered in areas of engineering,
mathematics and the physical sciences.  It is particularly well-suited to
problems that can be solved using the Finite Element Method (FEM).

@code{fenics} is the top level of the set of packages that are developed
within the FEniCS project.  It provides the python user interface to the
FEniCS core components and external libraries.")
    (license license:lgpl3+)))
