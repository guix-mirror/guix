;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Joshua Sierles, Nextjournal <joshua@nextjournal.com>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages graph)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system r)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages bioconductor)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages time)
  #:use-module (gnu packages xml))

(define-public igraph
  (package
    (name "igraph")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://igraph.org/nightly/get/c/igraph-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1pxh8sdlirgvbvsw8v65h6prn7hlm45bfsl1yfcgd6rn4w706y6r"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--with-external-glpk"
             "--with-external-blas"
             "--with-external-lapack")))
    (inputs
     `(("gmp" ,gmp)
       ("glpk" ,glpk)
       ("libxml2" ,libxml2)
       ("lapack" ,lapack)
       ("openblas" ,openblas)
       ("zlib" ,zlib)))
    (home-page "http://igraph.org")
    (synopsis "Network analysis and visualization")
    (description
     "This package provides a library for the analysis of networks and graphs.
It can handle large graphs very well and provides functions for generating
random and regular graphs, graph visualization, centrality methods and much
more.")
    (license license:gpl2+)))

(define-public python-igraph
  (package (inherit igraph)
    (name "python-igraph")
    (version "0.7.1.post6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-igraph" version))
       (sha256
        (base32
         "0xp61zz710qlzhmzbfr65d5flvsi8zf2xy78s6rsszh719wl5sm5"))))
    (build-system python-build-system)
    (arguments '())
    (inputs
     `(("igraph" ,igraph)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://pypi.python.org/pypi/python-igraph")
    (synopsis "Python bindings for the igraph network analysis library")))

(define-public r-igraph
  (package
    (name "r-igraph")
    (version "1.2.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "igraph" version))
       (sha256
        (base32
         "1074y8mvprrqlkb4vwa2qc9l03r8d7p5vaaqacj4ljjs7dvcq6l9"))))
    (build-system r-build-system)
    (native-inputs
     `(("gfortran" ,gfortran)))
    (inputs
     `(("gmp" ,gmp)
       ("glpk" ,glpk)
       ("libxml2" ,libxml2)
       ("zlib" ,zlib)))
    (propagated-inputs
     `(("r-magrittr" ,r-magrittr)
       ("r-matrix" ,r-matrix)
       ("r-pkgconfig" ,r-pkgconfig)))
    (home-page "http://igraph.org")
    (synopsis "Network analysis and visualization")
    (description
     "This package provides routines for simple graphs and network analysis.
It can handle large graphs very well and provides functions for generating
random and regular graphs, graph visualization, centrality methods and much
more.")
    (license license:gpl2+)))

(define-public r-diffusionmap
  (package
    (name "r-diffusionmap")
    (version "1.1-0.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "diffusionMap" version))
       (sha256
        (base32
         "11l4kbciawvli5nlsi4qaf8afmgk5xgqiqpdyhvaqri5mx0zhk5j"))))
    (properties `((upstream-name . "diffusionMap")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-igraph" ,r-igraph)
       ("r-matrix" ,r-matrix)
       ("r-scatterplot3d" ,r-scatterplot3d)))
    (home-page "https://www.r-project.org")
    (synopsis "Diffusion map")
    (description "This package implements the diffusion map method of data
parametrization, including creation and visualization of diffusion maps,
clustering with diffusion K-means and regression using the adaptive regression
model.")
    (license license:gpl2)))

(define-public r-rgraphviz
  (package
    (name "r-rgraphviz")
    (version "2.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "Rgraphviz" version))
       (sha256
        (base32
         "0nivz8fshaqig6ynjqbya2gmxsy4hm7jnd8fhb853z5g0ydp7g0c"))))
    (properties `((upstream-name . "Rgraphviz")))
    (build-system r-build-system)
    ;; FIXME: Rgraphviz bundles the sources of an older variant of
    ;; graphviz.  It does not build with the latest version of graphviz, so
    ;; we do not add graphviz to the inputs.
    (inputs `(("zlib" ,zlib)))
    (propagated-inputs
     `(("r-graph" ,r-graph)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://bioconductor.org/packages/Rgraphviz")
    (synopsis "Plotting capabilities for R graph objects")
    (description
     "This package interfaces R with the graphviz library for plotting R graph
objects from the @code{graph} package.")
    (license license:epl1.0)))

(define-public r-rbiofabric
  (let ((commit "666c2ae8b0a537c006592d067fac6285f71890ac")
        (revision "1"))
    (package
      (name "r-rbiofabric")
      (version (string-append "0.3-" revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/wjrl/RBioFabric.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1yahqrcrqpbcywv73y9rlmyz8apdnp08afialibrr93ch0p06f8z"))))
      (build-system r-build-system)
      (propagated-inputs
       `(("r-igraph" ,r-igraph)))
      (home-page "http://www.biofabric.org/")
      (synopsis "BioFabric network visualization")
      (description "This package provides an implementation of the function
@code{bioFabric} for creating scalable network digrams where nodes are
represented by horizontal lines, and edges are represented by vertical
lines.")
      (license license:expat))))

(define-public python-plotly
  (package
    (name "python-plotly")
    (version "2.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "plotly" version))
        (sha256
         (base32
          "0s9gk2fl53x8wwncs3fwii1vzfngr0sskv15v3mpshqmrqfrk27m"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; The tests are not distributed in the release
    (propagated-inputs
     `(("python-decorator" ,python-decorator)
       ("python-nbformat" ,python-nbformat)
       ("python-pandas" ,python-pandas)
       ("python-pytz" ,python-pytz)
       ("python-requests" ,python-requests)
       ("python-six" ,python-six)))
    (home-page "https://plot.ly/python/")
    (synopsis "Interactive plotting library for Python")
    (description "Plotly's Python graphing library makes interactive,
publication-quality graphs online.  Examples of how to make line plots, scatter
plots, area charts, bar charts, error bars, box plots, histograms, heatmaps,
subplots, multiple-axes, polar charts, and bubble charts. ")
    (license license:expat)))

(define-public python2-plotly
  (package-with-python2 python-plotly))

(define-public python-louvain
  (package
    (name "python-louvain")
    (version "0.6.1")
    ;; The tarball on Pypi does not include the tests.
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/vtraag/louvain-igraph.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0w31537sifkf65sck1iaip5i6d8g64pa3wdwad83d6p9jwkck57k"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-ddt" ,python-ddt)
       ("python-igraph" ,python-igraph)))
    (inputs
     `(("igraph" ,igraph)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python-pytest" ,python-pytest)))
    (home-page "https://github.com/vtraag/louvain-igraph")
    (synopsis "Algorithm for methods of community detection in large networks")
    (description
     "This package provides an implementation of the Louvain algorithm for use
with igraph.  Louvain is a general algorithm for methods of community
detection in large networks.

This package has been superseded by the @code{leidenalg} package and should
not be used for new projects.")
    (license license:gpl3+)))

(define-public faiss
  (package
    (name "faiss")
    (version "1.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/facebookresearch/faiss.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0pk15jfa775cy2pqmzq62nhd6zfjxmpvz5h731197c28aq3zw39w"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "utils.cpp"
                    (("#include <immintrin.h>")
                     "#ifdef __SSE__\n#include <immintrin.h>\n#endif"))
                  #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list "-DBUILD_WITH_GPU=OFF"  ; thanks, but no thanks, CUDA.
             "-DBUILD_TUTORIAL=OFF") ; we don't need those
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'prepare-build
           (lambda _
             (let ((features (list ,@(let ((system (or (%current-target-system)
                                                       (%current-system))))
                                       (cond
                                        ((string-prefix? "x86_64" system)
                                         '("-mavx" "-msse2" "-mpopcnt"))
                                        ((string-prefix? "i686" system)
                                         '("-msse2" "-mpopcnt"))
                                        (else
                                         '()))))))
               (substitute* "CMakeLists.txt"
                 (("-m64") "")
                 (("-mpopcnt") "") ; only some architectures
                 (("-msse4")
                  (string-append
                   (string-join features)
                   " -I" (getcwd)))
                 ;; Build also the shared library
                 (("ARCHIVE DESTINATION lib")
                  "LIBRARY DESTINATION lib")
                 (("add_library.*" m)
                  "\
add_library(objlib OBJECT ${faiss_cpu_headers} ${faiss_cpu_cpp})
set_property(TARGET objlib PROPERTY POSITION_INDEPENDENT_CODE 1)
add_library(${faiss_lib}_static STATIC $<TARGET_OBJECTS:objlib>)
add_library(${faiss_lib} SHARED $<TARGET_OBJECTS:objlib>)
install(TARGETS ${faiss_lib}_static ARCHIVE DESTINATION lib)
\n")))

             ;; See https://github.com/facebookresearch/faiss/issues/520
             (substitute* "IndexScalarQuantizer.cpp"
               (("#define USE_AVX") ""))

             ;; Make header files available for compiling tests.
             (mkdir-p "faiss")
             (for-each (lambda (file)
                         (mkdir-p (string-append "faiss/" (dirname file)))
                         (copy-file file (string-append "faiss/" file)))
                       (find-files "." "\\.h$"))
             #t))
         (replace 'check
           (lambda _
             (invoke "make" "-C" "tests"
                     (format #f "-j~a" (parallel-job-count)))))
         (add-after 'install 'remove-tests
           (lambda* (#:key outputs #:allow-other-keys)
             (delete-file-recursively
              (string-append (assoc-ref outputs "out")
                             "/test"))
             #t)))))
    (inputs
     `(("openblas" ,openblas)))
    (native-inputs
     `(("googletest" ,googletest)))
    (home-page "https://github.com/facebookresearch/faiss")
    (synopsis "Efficient similarity search and clustering of dense vectors")
    (description "Faiss is a library for efficient similarity search and
clustering of dense vectors.  It contains algorithms that search in sets of
vectors of any size, up to ones that possibly do not fit in RAM.  It also
contains supporting code for evaluation and parameter tuning.")
    (license license:bsd-3)))

(define-public python-faiss
  (package (inherit faiss)
    (name "python-faiss")
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "python") #t))
         (add-after 'chdir 'build-swig
           (lambda* (#:key inputs #:allow-other-keys)
             (with-output-to-file "../makefile.inc"
               (lambda ()
                 (let ((python-version ,(version-major+minor (package-version python))))
                   (format #t "\
PYTHONCFLAGS =-I~a/include/python~am/ -I~a/lib/python~a/site-packages/numpy/core/include
LIBS = -lpython~am -lfaiss
SHAREDFLAGS = -shared -fopenmp
CXXFLAGS = -fpermissive -std=c++11 -fopenmp -fPIC
CPUFLAGS = ~{~a ~}~%"
                           (assoc-ref inputs "python*") python-version
                           (assoc-ref inputs "python-numpy") python-version
                           python-version
                           (list ,@(let ((system (or (%current-target-system)
                                                     (%current-system))))
                                     (cond
                                       ((string-prefix? "x86_64" system)
                                        '("-mavx" "-msse2" "-mpopcnt"))
                                       ((string-prefix? "i686" system)
                                        '("-msse2" "-mpopcnt"))
                                       (else
                                         '()))))))))
             (substitute* "Makefile"
               (("../libfaiss.a") ""))
             (invoke "make" "cpu"))))))
    (inputs
     `(("faiss" ,faiss)
       ("openblas" ,openblas)
       ("python*" ,python)
       ("swig" ,swig)))
    (propagated-inputs
     `(("python-matplotlib" ,python-matplotlib)
       ("python-numpy" ,python-numpy)))
    (description "Faiss is a library for efficient similarity search and
clustering of dense vectors.  This package provides Python bindings to the
Faiss library.")))
