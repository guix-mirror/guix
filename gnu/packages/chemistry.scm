;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Konrad Hinsen <konrad.hinsen@fastmail.net>
;;; Copyright © 2018 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
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
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gv)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xml)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python))

(define-public avogadro
  (package
    (name "avogadro")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cryos/avogadro.git")
             (commit version)))
       (sha256
        (base32 "0258py3lkba85qhs5ynancinyym61vlp0zaq9yrfs3hhnhpzv9n2"))
       (file-name (git-file-name name version))
       (patches
        (search-patches "avogadro-eigen3-update.patch"
                        "avogadro-python-eigen-lib.patch"
                        "avogadro-boost148.patch"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:configure-flags
       (list "-DENABLE_GLSL=ON"
             (string-append "-DPYTHON_LIBRARIES="
                            (assoc-ref %build-inputs "python")
                            "/lib")
             (string-append "-DPYTHON_INCLUDE_DIRS="
                            (assoc-ref %build-inputs "python")
                            "/include/python"
                            ,(version-major+minor
                               (package-version python))))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-python-lib-path
           (lambda* (#:key outputs #:allow-other-keys)
             ;; This is necessary to install the Python module in the correct
             ;; directory.
             (substitute* "libavogadro/src/python/CMakeLists.txt"
               (("^EXECUTE_PROCESS.*$") "")
               (("^.*from sys import stdout.*$") "")
               (("^.*OUTPUT_VARIABLE.*")
                (string-append "set(PYTHON_LIB_PATH \""
                               (assoc-ref outputs "out")
                               "/lib/python"
                               ,(version-major+minor
                                  (package-version python))
                               "/site-packages\")")))
             #t))
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Make sure 'avogadro' runs with the correct PYTHONPATH.
             (let* ((out (assoc-ref outputs "out")))
               (setenv "PYTHONPATH"
                       (string-append
                        (assoc-ref outputs "out")
                        "/lib/python"
                        ,(version-major+minor
                           (package-version python))
                        "/site-packages:"
                        (getenv "PYTHONPATH")))
               (wrap-program (string-append out "/bin/avogadro")
                 `("PYTHONPATH" ":" prefix (,(getenv "PYTHONPATH")))))
             #t)))))
    (native-inputs
     `(("doxygen" ,doxygen)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("boost" ,boost)
       ("eigen" ,eigen)
       ("glew" ,glew)
       ("openbabel" ,openbabel)
       ("python" ,python-2)
       ("python-numpy" ,python2-numpy)
       ("python-pyqt" ,python2-pyqt-4)
       ("python-sip" ,python2-sip)
       ("qt" ,qt-4)
       ("zlib" ,zlib)))
    (home-page "https://avogadro.cc")
    (synopsis "Advanced molecule editor")
    (description
     "Avogadro is an advanced molecule editor and visualizer designed for use
in computational chemistry, molecular modeling, bioinformatics, materials
science, and related areas.  It offers flexible high quality rendering and a
powerful plugin architecture.")
    (license license:gpl2+)))

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
     `(("python-mmtk" ,python2-mmtk)))
    (arguments
     `(#:python ,python-2
       ;; No test suite
       #:tests? #f))
    (home-page "http://dirac.cnrs-orleans.fr/DomainFinder")
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
    (version "1.05")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.inchi-trust.org/download/"
                                  (string-join (string-split version #\.) "")
                                  "/INCHI-1-SRC.zip"))
              (sha256
               (base32
                "081pcjx1z5jm23fs1pl2r3bccia0ww8wfkzcjpb7byhn7b513hsa"))
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
                    (unzip (string-append (assoc-ref inputs "unzip")
                                          "/bin/unzip")))
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
             "1id1qb2y4lwsiw91qr2yqpn6kxbwjwhjk0hb2rwk4fxhdqib6da6"))
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
     `(("python-matplotlib" ,(with-numpy-1.8 python2-matplotlib))
       ("python-scientific" ,python2-scientific)
       ("netcdf" ,netcdf)
       ("gv" ,gv)))
    (propagated-inputs
     `(("python-mmtk" ,python2-mmtk)))
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
                (format "PREFERENCES['acroread_path'] = '~a'"
                        (which "gv")))
               (("PREFERENCES\\['ncdump_path'\\] = ''")
                (format "PREFERENCES['ncdump_path'] = '~a'"
                        (which "ncdump")))
               (("PREFERENCES\\['ncgen_path'\\] = ''")
                (format "PREFERENCES['ncgen_path'] = '~a'"
                        (which "ncgen3")))
               (("PREFERENCES\\['task_manager_path'\\] = ''")
                (format "PREFERENCES['task_manager_path'] = '~a'"
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

(define-public openbabel
  (package
    (name "openbabel")
    (version "2.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/" name "/" name "/"
                                  version "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "1z3d6xm70dpfikhwdnbzc66j2l49vq105ch041wivrfz5ic3ch90"))
              (patches
               (search-patches "openbabel-fix-crash-on-nwchem-output.patch"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list "-DOPENBABEL_USE_SYSTEM_INCHI=ON"
             (string-append "-DINCHI_LIBRARY="
                            (assoc-ref %build-inputs "inchi")
                            "/lib/inchi/libinchi.so.1")
             (string-append "-DINCHI_INCLUDE_DIR="
                            (assoc-ref %build-inputs "inchi") "/include/inchi"))
       #:test-target "test"))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("eigen" ,eigen)
       ("inchi" ,inchi)
       ("libxml2" ,libxml2)
       ("zlib" ,zlib)))
    (home-page "http://openbabel.org/wiki/Main_Page")
    (synopsis "Chemistry data manipulation toolbox")
    (description
     "Open Babel is a chemical toolbox designed to speak the many languages of
chemical data.  It's a collaborative project allowing anyone to search, convert,
analyze, or store data from molecular modeling, chemistry, solid-state
materials, biochemistry, or related areas.")
    (license license:gpl2)))
