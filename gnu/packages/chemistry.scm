;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Konrad Hinsen <konrad.hinsen@fastmail.net>
;;; Copyright © 2018, 2021 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2021 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages chemistry)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gv)
  #:use-module (gnu packages image)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages xml)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python))

(define-public avogadrolibs
  (package
    (name "avogadrolibs")
    (version "1.93.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/OpenChemistry/avogadrolibs")
             (commit version)))
       (sha256
        (base32 "1xivga626n5acnmwmym8svl0pdri8hkp59czf04ri2zflnviyh39"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (native-inputs
     (list eigen
           mmtf-cpp
           msgpack
           googletest
           pkg-config
           pybind11))
    (inputs
     (list glew
           libarchive
           libmsym
           molequeue
           python
           spglib
           qtbase-5))
    (arguments
     '(#:configure-flags (list "-DENABLE_TESTING=ON"
                               (string-append "-DSPGLIB_INCLUDE_DIR="
                                              (assoc-ref %build-inputs "spglib")
                                              "/include"))))
    (home-page "https://www.openchemistry.org/projects/avogadro2/")
    (synopsis "Libraries for chemistry, bioinformatics, and related areas")
    (description
     "Avogadro libraries provide 3D rendering, visualization, analysis and data
processing useful in computational chemistry, molecular modeling,
bioinformatics, materials science, and related areas.")
    (license license:bsd-3)))

(define-public avogadro2
  (package
    (name "avogadro2")
    (version "1.93.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/OpenChemistry/avogadroapp")
             (commit version)))
       (sha256
        (base32
         "1z3pjlwja778a1dmvx9aqz2hlw5q9g3kqxhm9slz08452600jsv7"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (native-inputs
     (list eigen pkg-config))
    (inputs
     (list avogadrolibs hdf5 molequeue qtbase-5))
    ;; TODO: Enable tests with "-DENABLE_TESTING" configure flag.
    (arguments
     '(#:tests? #f))
    (home-page "https://www.openchemistry.org/projects/avogadro2/")
    (synopsis "Advanced molecule editor")
    (description
     "Avogadro 2 is an advanced molecule editor and visualizer designed for use
in computational chemistry, molecular modeling, bioinformatics, materials
science, and related areas.  It offers flexible high quality rendering and a
powerful plugin architecture.")
    (license license:bsd-3)))

(define-public domainfinder
  (package
    (name "domainfinder")
    (version "2.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://bitbucket.org/khinsen/"
                           "domainfinder/downloads/DomainFinder-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1z26lsyf7xwnzwjvimmbla7ckipx6p734w7y0jk2a2fzci8fkdcr"))))
    (build-system python-build-system)
    (inputs
     (list python2-mmtk))
    (arguments
     `(#:python ,python-2
       ;; No test suite
       #:tests? #f))
    (home-page "http://dirac.cnrs-orleans.fr/DomainFinder.html")
    (synopsis "Analysis of dynamical domains in proteins")
    (description "DomainFinder is an interactive program for the determination
and characterization of dynamical domains in proteins.  It can infer dynamical
domains by comparing two protein structures, or from normal mode analysis on a
single structure.  The software is currently not actively maintained and works
only with Python 2 and NumPy < 1.9.")
    (license license:cecill-c)))

(define-public inchi
  (package
    (name "inchi")
    ;; Update the inchi-doc native input when updating inchi.
    (version "1.06")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.inchi-trust.org/download/"
                                  (string-join (string-split version #\.) "")
                                  "/INCHI-1-SRC.zip"))
              (sha256
               (base32
                "1zbygqn0443p0gxwr4kx3m1bkqaj8x9hrpch3s41py7jq08f6x28"))
              (file-name (string-append name "-" version ".zip"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; no check target
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; no configure script
         (add-before 'build 'chdir-to-build-directory
           (lambda _ (chdir "INCHI_EXE/inchi-1/gcc") #t))
         (add-after 'build 'build-library
           (lambda _
             (chdir "../../../INCHI_API/libinchi/gcc")
             (invoke "make")))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (doc (string-append out "/share/doc/inchi"))
                    (include-dir (string-append out "/include/inchi"))
                    (lib (string-append out "/lib/inchi"))
                    (inchi-doc (assoc-ref inputs "inchi-doc"))
                    (unzip (search-input-file inputs "/bin/unzip")))
               (chdir "../../..")
               ;; Install binary.
               (with-directory-excursion "INCHI_EXE/bin/Linux"
                 (rename-file "inchi-1" "inchi")
                 (install-file "inchi" bin))
               ;; Install libraries.
               (with-directory-excursion "INCHI_API/bin/Linux"
                 (for-each (lambda (file)
                             (install-file file lib))
                           (find-files "." "libinchi\\.so\\.1\\.*")))
               ;; Install header files.
               (with-directory-excursion "INCHI_BASE/src"
                 (for-each (lambda (file)
                             (install-file file include-dir))
                           (find-files "." "\\.h$")))
               ;; Install documentation.
               (mkdir-p doc)
               (invoke unzip "-j" "-d" doc inchi-doc)
               #t))))))
    (native-inputs
     `(("unzip" ,unzip)
       ("inchi-doc"
        ,(origin
           (method url-fetch)
           (uri (string-append "http://www.inchi-trust.org/download/"
                                  (string-join (string-split version #\.) "")
                                  "/INCHI-1-DOC.zip"))
           (sha256
            (base32
             "1kyda09i9p89xfq90ninwi7w13k1w3ljpl4gqdhpfhi5g8fgxx7f"))
           (file-name (string-append name "-" version ".zip"))))))
    (home-page "https://www.inchi-trust.org")
    (synopsis "Utility for manipulating machine-readable chemical structures")
    (description
     "The @dfn{InChI} (IUPAC International Chemical Identifier) algorithm turns
chemical structures into machine-readable strings of information.  InChIs are
unique to the compound they describe and can encode absolute stereochemistry
making chemicals and chemistry machine-readable and discoverable.  A simple
analogy is that InChI is the bar-code for chemistry and chemical structures.")
    (license (license:non-copyleft
              "file://LICENCE"
              "See LICENCE in the distribution."))))

(define-public libmsym
  (package
    (name "libmsym")
    (version "0.2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mcodev31/libmsym")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "0a9j28irdsr461qpzlc9z1yjyb9kp64fh5zw7ylspc9zn3189qwk"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '("-DBUILD_SHARED_LIBS=ON")
       #:tests? #f))                    ; no check target
    (home-page "https://github.com/mcodev31/libmsym")
    (synopsis "C library dealing with point group symmetry in molecules")
    (description "libmsym is a C library dealing with point group symmetry in
molecules.")
    (license license:expat)))

(define-public mmtf-cpp
  (package
    (name "mmtf-cpp")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rcsb/mmtf-cpp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "17ylramda69plf5w0v5hxbl4ggkdi5s15z55cv0pljl12yvyva8l"))))
    (build-system cmake-build-system)
    ;; Tests require the soon-to-be-deprecated version 1 of the catch-framework.
    (arguments
     '(#:tests? #f))
    (home-page "https://mmtf.rcsb.org/")
    (synopsis "C++ API for the Macromolecular Transmission Format")
    (description "This package is a library for the
@acronym{MMTF,macromolecular transmission format}, a binary encoding of
biological structures.")
    (license license:expat)))

(define-public molequeue
  (package
    (name "molequeue")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/OpenChemistry/molequeue/"
                           "releases/download/" version "/molequeue-"
                           version ".tar.bz2"))
       (sha256
        (base32
         "1w1fgxzqrb5yxvpmnc3c9ymnvixy0z1nfafkd9whg9zw8nbgl998"))))
    (build-system cmake-build-system)
    (inputs
     (list qtbase-5))
    (arguments
     '(#:configure-flags '("-DENABLE_TESTING=ON")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-tests
           (lambda _
             ;; TODO: Fix/enable the failing message and clientserver tests.
             ;; In the message test, the floating-point value "5.36893473232" on
             ;; line 165 of molequeue/app/testing/messagetest.cpp should
             ;; (apparently) be truncated, but it is not.
             (substitute* "molequeue/app/testing/messagetest.cpp"
               (("5\\.36893473232") "5.36893"))
             ;; It is unclear why the clientserver test fails, so it is
             ;; completely disabled.
             (substitute* "molequeue/app/testing/CMakeLists.txt"
               ((".*clientserver.*") ""))
             #t))
         (add-before 'check 'set-display
           (lambda _
             ;; Make Qt render "offscreen" for the sake of tests.
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
    (home-page "https://www.openchemistry.org/projects/molequeue/")
    (synopsis "Application for coordinating computational jobs")
    (description "MoleQueue is a system-tray resident desktop application for
abstracting, managing, and coordinating the execution of tasks both locally and
 on remote computational resources.  Users can set up local and remote queues
that describe where the task will be executed.  Each queue can have programs,
with templates to facilitate the execution of the program.  Input files can be
staged, and output files collected using a standard interface.")
    (license license:bsd-3)))

(define with-numpy-1.8
  (package-input-rewriting `((,python2-numpy . ,python2-numpy-1.8))))

(define-public nmoldyn
  (package
    (name "nmoldyn")
    (version "3.0.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/khinsen/nMOLDYN3")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "016h4bqg419p6s7bcx55q5iik91gqmk26hbnfgj2j6zl0j36w51r"))))
    (build-system python-build-system)
    (inputs
     (list (with-numpy-1.8 python2-matplotlib) python2-scientific netcdf
           gv))
    (propagated-inputs
     (list python2-mmtk))
    (arguments
     `(#:python ,python-2
       #:tests? #f  ; No test suite
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'create-linux2-directory
           (lambda _
             (mkdir-p "nMOLDYN/linux2")))
         (add-before 'build 'change-PDF-viewer
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "nMOLDYN/Preferences.py"
               ;; Set the paths for external executables, substituting
               ;; gv for acroread.
               ;; There is also vmd_path, but VMD is not free software
               ;; and Guix contains currently no free molecular viewer that
               ;; could be substituted.
               (("PREFERENCES\\['acroread_path'\\] = ''")
                (format #f "PREFERENCES['acroread_path'] = '~a'"
                        (which "gv")))
               (("PREFERENCES\\['ncdump_path'\\] = ''")
                (format #f "PREFERENCES['ncdump_path'] = '~a'"
                        (which "ncdump")))
               (("PREFERENCES\\['ncgen_path'\\] = ''")
                (format #f "PREFERENCES['ncgen_path'] = '~a'"
                        (which "ncgen3")))
               (("PREFERENCES\\['task_manager_path'\\] = ''")
                (format #f "PREFERENCES['task_manager_path'] = '~a'"
                        (which "task_manager")))
               ;; Show documentation as PDF
               (("PREFERENCES\\['documentation_style'\\] = 'html'")
                "PREFERENCES['documentation_style'] = 'pdf'") ))))))
    (home-page "http://dirac.cnrs-orleans.fr/nMOLDYN.html")
    (synopsis "Analysis software for Molecular Dynamics trajectories")
    (description "nMOLDYN is an interactive analysis program for Molecular Dynamics
simulations.  It is especially designed for the computation and decomposition of
neutron scattering spectra, but also computes other quantities.  The software
is currently not actively maintained and works only with Python 2 and
NumPy < 1.9.")
    (license license:cecill)))

(define-public tng
  (package
    (name "tng")
    (version "1.8.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/gromacs/tng")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1apf2n8nb34z09xarj7k4jgriq283l769sakjmj5aalpbilvai4q"))))
    (build-system cmake-build-system)
    (inputs
     (list zlib))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-bundled-zlib
           (lambda _
             (delete-file-recursively "external")
             #t))
         (replace 'check
           (lambda _
             (invoke "../build/bin/tests/tng_testing")
             #t)))))
    (home-page "https://github.com/gromacs/tng")
    (synopsis "Trajectory Next Generation binary format manipulation library")
    (description "TRAJNG (Trajectory next generation) is a program library for
handling molecular dynamics (MD) trajectories.  It can store coordinates, and
optionally velocities and the H-matrix.  Coordinates and velocities are
stored with user-specified precision.")
    (license license:bsd-3)))

(define-public gromacs
  (package
    (name "gromacs")
    (version "2020.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://ftp.gromacs.org/pub/gromacs/gromacs-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1wyjgcdl30wy4hy6jvi9lkq53bqs9fgfq6fri52dhnb3c76y8rbl"))
              ;; Our version of tinyxml2 is far newer than the bundled one and
              ;; require fixing `testutils' code. See patch header for more info
              (patches (search-patches "gromacs-tinyxml2.patch"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list "-DGMX_DEVELOPER_BUILD=on" ; Needed to run tests
             ;; Unbundling
             "-DGMX_USE_LMFIT=EXTERNAL"
             "-DGMX_BUILD_OWN_FFTW=off"
             "-DGMX_EXTERNAL_BLAS=on"
             "-DGMX_EXTERNAL_LAPACK=on"
             "-DGMX_EXTERNAL_TNG=on"
             "-DGMX_EXTERNAL_ZLIB=on"
             "-DGMX_EXTERNAL_TINYXML2=on"
             (string-append "-DTinyXML2_DIR="
                            (assoc-ref %build-inputs "tinyxml2"))
             ;; Workaround for cmake/FindSphinx.cmake version parsing that does
             ;; not understand the guix-wrapped `sphinx-build --version' answer
             (string-append "-DSPHINX_EXECUTABLE_VERSION="
                            ,(package-version python-sphinx)))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fixes
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Still bundled: part of gromacs, source behind registration
             ;; but free software anyways
             ;;(delete-file-recursively "src/external/vmd_molfile")
             ;; Still bundled: threads-based OpenMPI-compatible fallback
             ;; designed to be bundled like that
             ;;(delete-file-recursively "src/external/thread_mpi")
             ;; Unbundling
             (delete-file-recursively "src/external/lmfit")
             (delete-file-recursively "src/external/clFFT")
             (delete-file-recursively "src/external/fftpack")
             (delete-file-recursively "src/external/build-fftw")
             (delete-file-recursively "src/external/tng_io")
             (delete-file-recursively "src/external/tinyxml2")
             (delete-file-recursively "src/external/googletest")
             (copy-recursively (assoc-ref inputs "googletest-source")
                               "src/external/googletest")
             ;; This test warns about the build host hardware, disable
             (substitute* "src/gromacs/hardware/tests/hardwaretopology.cpp"
               (("TEST\\(HardwareTopologyTest, HwlocExecute\\)")
                "void __guix_disabled()"))
             #t)))))
    (native-inputs
     `(("doxygen" ,doxygen)
       ("googletest-source" ,(package-source googletest))
       ("graphviz" ,graphviz)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("python-pygments" ,python-pygments)
       ("python-sphinx" ,python-sphinx)))
    (inputs
     (list fftwf
           `(,hwloc-2 "lib")
           lmfit
           openblas
           perl
           tinyxml2
           tng))
    (home-page "http://www.gromacs.org/")
    (synopsis "Molecular dynamics software package")
    (description "GROMACS is a versatile package to perform molecular dynamics,
i.e. simulate the Newtonian equations of motion for systems with hundreds to
millions of particles.  It is primarily designed for biochemical molecules like
proteins, lipids and nucleic acids that have a lot of complicated bonded
interactions, but since GROMACS is extremely fast at calculating the nonbonded
interactions (that usually dominate simulations) many groups are also using it
for research on non-biological systems, e.g. polymers.  GROMACS supports all the
usual algorithms you expect from a modern molecular dynamics implementation.")
    (license license:lgpl2.1+)))

(define-public openbabel
  (package
    (name "openbabel")
    (version "3.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/openbabel/openbabel/"
                                  "releases/download/openbabel-"
                                  (string-replace-substring version "." "-")
                                  "/openbabel-" version "-source.tar.bz2"))
              (sha256
               (base32
                "0s0f4zib8vshfaywsr5bjjz55jwsg6yiz2qw4i5jm8wysn0q7v56"))))
    (build-system cmake-build-system)
    (arguments
     `(;; FIXME: Disable tests on i686 to work around
       ;; https://github.com/openbabel/openbabel/issues/2041.
       #:tests? ,(or (%current-target-system)
                     (not (string=? "i686-linux" (%current-system))))
       #:configure-flags
       (list "-DOPENBABEL_USE_SYSTEM_INCHI=ON"
             (string-append "-DINCHI_LIBRARY="
                            (assoc-ref %build-inputs "inchi")
                            "/lib/inchi/libinchi.so.1")
             (string-append "-DINCHI_INCLUDE_DIR="
                            (assoc-ref %build-inputs "inchi") "/include/inchi"))
       #:test-target "test"))
    (native-inputs
     (list pkg-config))
    (inputs
     (list eigen inchi libxml2 zlib))
    (home-page "http://openbabel.org/wiki/Main_Page")
    (synopsis "Chemistry data manipulation toolbox")
    (description
     "Open Babel is a chemical toolbox designed to speak the many languages of
chemical data.  It's a collaborative project allowing anyone to search, convert,
analyze, or store data from molecular modeling, chemistry, solid-state
materials, biochemistry, or related areas.")
    (license license:gpl2)))

(define-public spglib
  (package
    (name "spglib")
    (version "1.16.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/spglib/spglib")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1kzc956m1pnazhz52vspqridlw72wd8x5l3dsilpdxl491aa2nws"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     '(#:test-target "check"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-header-install-dir
           (lambda _
             ;; As of the writing of this package, CMake and GNU build systems
             ;; install the header to two different location.  This patch makes
             ;; the CMake build system's choice of header directory compatible
             ;; with the GNU build system's choice and with what avogadrolibs
             ;; expects.
             ;; See https://github.com/spglib/spglib/issues/75 and the relevant
             ;; part of https://github.com/OpenChemistry/avogadroapp/issues/97.
             (substitute* "CMakeLists.txt"
               (("\\$\\{CMAKE_INSTALL_INCLUDEDIR\\}" include-dir)
                (string-append include-dir "/spglib")))
             #t)))))
    (home-page "https://spglib.github.io/spglib/index.html")
    (synopsis "Library for crystal symmetry search")
    (description "Spglib is a library for finding and handling crystal
symmetries written in C.  Spglib can be used to:

@enumerate
@item Find symmetry operations
@item Identify space-group type
@item Wyckoff position assignment
@item Refine crystal structure
@item Find a primitive cell
@item Search irreducible k-points
@end enumerate")
    (license license:bsd-3)))

(define-public python-pymol
  (package
    (name "python-pymol")
    (version "2.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/schrodinger/pymol-open-source")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08zmfgclkbjkqjpq8xs1mphs1i8rpqj76mcw7m2mrhvma5qj1nr5"))))
    (build-system python-build-system)
    (arguments
     '(#:configure-flags
       (list "--glut" "--testing")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-reproducible
           (lambda _
             (substitute* "create_shadertext.py"
               (("time\\.time\\(\\)") "0"))))
         (add-after 'unpack 'add-include-directories
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CPLUS_INCLUDE_PATH"
                     (string-append (assoc-ref inputs "freetype")
                                    "/include/freetype2:"
                                    (assoc-ref inputs "libxml2")
                                    "/include/libxml2:"
                                    (getenv "CPLUS_INCLUDE_PATH")))))
         ;; The setup.py script does not support one of the Python build
         ;; system's default flags, "--single-version-externally-managed".
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "python" "setup.py" "install"
                     (string-append "--prefix=" (assoc-ref outputs "out"))
                     "--root=/"))))))
    (inputs
     (list freetype
           libpng
           freeglut
           glew
           libxml2
           mmtf-cpp
           msgpack
           python-pyqt
           glm
           netcdf))
    (native-inputs
     (list catch-framework2 python-setuptools))
    (home-page "https://pymol.org")
    (synopsis "Molecular visualization system")
    (description "PyMOL is a capable molecular viewer and renderer.  It can be
used to prepare publication-quality figures, to share interactive results with
your colleagues, or to generate pre-rendered animations.")
    (license license:bsd-3)))
