;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018, 2019, 2020, 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Joshua Sierles, Nextjournal <joshua@nextjournal.com>
;;; Copyright © 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2020 Alexander Krotov <krotov@iitp.ru>
;;; Copyright © 2020 Pierre Langlois <pierre.langlos@gmx.com>
;;; Copyright © 2021 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 Alexandre Hannud Abdo <abdo@member.fsf.org>
;;; Copyright © 2021, 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2022 Marius Bakke <marius@gnu.org>
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
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system r)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bioconductor)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages time)
  #:use-module (gnu packages xml))

(define-public plfit
  (package
    (name "plfit")
    (version "0.9.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ntamas/plfit")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "03x5jbvg8vwr92682swy58ljxrhqwmga1xzd0cpfbfmda41gm2fb"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags (list "-DBUILD_SHARED_LIBS=ON")))
    (home-page "https://github.com/ntamas/plfit")
    (synopsis "Tool for fitting power-law distributions to empirical data")
    (description "The @command{plfit} command fits power-law distributions to
empirical (discrete or continuous) data, according to the method of Clauset,
Shalizi and Newman (@cite{Clauset A, Shalizi CR and Newman MEJ: Power-law
distributions in empirical data.  SIAM Review 51, 661-703 (2009)}).")
    (license license:gpl2+)))

(define-public igraph
  (package
    (name "igraph")
    (version "0.9.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/igraph/igraph/releases/"
                           "download/" version "/igraph-" version ".tar.gz"))
       (modules '((guix build utils)))
       (snippet '(begin
                   ;; Fully unbundle igraph (see:
                   ;; https://github.com/igraph/igraph/issues/1897).
                   (delete-file-recursively "vendor")
                   (substitute* "CMakeLists.txt"
                     (("add_subdirectory\\(vendor\\).*")
                      ""))
                   ;; Help CMake to find our plfit headers.
                   (substitute* "etc/cmake/FindPLFIT.cmake"
                     (("^  NAMES plfit.h.*" all)
                      (string-append all
                                     "  PATH_SUFFIXES plfit")))
                   (substitute* '("src/CMakeLists.txt"
                                  "etc/cmake/benchmark_helpers.cmake")
                     ;; Remove bundling related variables.
                     ((".*_IS_VENDORED.*")
                      ""))))
       (sha256
        (base32 "11zkj9bpqcadb0rc4ahvjp9047dp9hna8cn3b0vl3zpc9v2rwabw"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags (list "-DBUILD_SHARED_LIBS=ON")))
    (native-inputs (list pkg-config))
    (inputs
     (list arpack-ng
           gmp
           glpk
           libxml2
           lapack
           openblas
           plfit
           suitesparse))
    (home-page "https://igraph.org")
    (synopsis "Network analysis and visualization")
    (description
     "This package provides a library for the analysis of networks and graphs.
It can handle large graphs very well and provides functions for generating
random and regular graphs, graph visualization, centrality methods and much
more.")
    (license license:gpl2+)))

(define-public python-igraph
  (package
    (inherit igraph)
    (name "python-igraph")
    (version "0.9.9")
    (source (origin
              (method git-fetch)
              ;; The PyPI archive lacks tests.
              (uri (git-reference
                    (url "https://github.com/igraph/python-igraph")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ravcww2jcr8fgi97gdxv00s5nkx59ljxy928nnniyd0231bqwlc"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'specify-libigraph-location
            (lambda _
              (let ((igraph #$(this-package-input "igraph")))
                (substitute* "setup.py"
                  (("(LIBIGRAPH_FALLBACK_INCLUDE_DIRS = ).*" _ var)
                   (string-append
                    var (format #f "[~s]~%" (string-append igraph
                                                           "/include/igraph"))))
                  (("(LIBIGRAPH_FALLBACK_LIBRARY_DIRS = ).*" _ var)
                   (string-append
                    var (format #f "[~s]~%" (string-append igraph "/lib"))))))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "pytest" "-v")))))))
    (inputs
     (list igraph))
    (propagated-inputs
     (list python-texttable))
    (native-inputs
     (list python-pytest))
    (home-page "https://igraph.org/python/")
    (synopsis "Python bindings for the igraph network analysis library")))

(define-public r-rbiofabric
  (let ((commit "666c2ae8b0a537c006592d067fac6285f71890ac")
        (revision "1"))
    (package
      (name "r-rbiofabric")
      (version (string-append "0.3-" revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/wjrl/RBioFabric")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1yahqrcrqpbcywv73y9rlmyz8apdnp08afialibrr93ch0p06f8z"))))
      (build-system r-build-system)
      (propagated-inputs
       (list r-igraph))
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
    (version "4.14.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/plotly/plotly.py")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "02wlgy7gf3v5ckiq9ab3prm53cckxkavlghqgkk9xw2sfmmrn61q"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "packages/python/plotly")
             #t))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest" "-x" "plotly/tests/test_core")
               (invoke "pytest" "-x" "plotly/tests/test_io")
               ;; FIXME: Add optional dependencies and enable their tests.
               ;; (invoke "pytest" "-x" "plotly/tests/test_optional")
               (invoke "pytest" "_plotly_utils/tests")))))))
    (native-inputs
     (list python-ipywidgets python-pytest python-xarray))
    (propagated-inputs
     (list python-ipython
           python-pandas
           python-pillow
           python-requests
           python-retrying
           python-six
           python-statsmodels))
    (home-page "https://plotly.com/python/")
    (synopsis "Interactive plotting library for Python")
    (description "Plotly's Python graphing library makes interactive,
publication-quality graphs online.  Examples of how to make line plots, scatter
plots, area charts, bar charts, error bars, box plots, histograms, heatmaps,
subplots, multiple-axes, polar charts, and bubble charts.")
    (license license:expat)))

(define-public python-plotly-2.4.1
  (package (inherit python-plotly)
    (version "2.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "plotly" version))
        (sha256
         (base32
          "0s9gk2fl53x8wwncs3fwii1vzfngr0sskv15v3mpshqmrqfrk27m"))))
   (native-inputs '())
   (propagated-inputs
    (list python-decorator
          python-nbformat
          python-pandas
          python-pytz
          python-requests
          python-six))
    (arguments
     '(#:tests? #f)))) ; The tests are not distributed in the release

(define-public python-louvain
  (package
    (name "python-louvain")
    (version "0.16")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-louvain" version))
       (sha256
        (base32 "0sx53l555rwq0z7if8agirjgw4ddp8r9b949wwz8vlig03sjvfmp"))))
    (build-system python-build-system)
    (native-inputs
     (list python-setuptools))          ;for use_2to3 support
    (propagated-inputs
     (list python-networkx python-numpy))
    (home-page "https://github.com/taynaud/python-louvain")
    (synopsis "Louvain algorithm for community detection")
    (description
     "This package provides a pure Python implementation of the Louvain
algorithm for community detection in large networks.")
    (license license:bsd-3)))

(define-public python-louvain-0.7
  (package
    (name "python-louvain")
    (version "0.7.1")
    ;; The tarball on Pypi does not include the tests.
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/vtraag/louvain-igraph")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1g6b5c2jgwagnhnqh859g61h7x6a81d8hm3g6mkin6kzwafww3g2"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'pretend-version
            ;; The version string is usually derived via setuptools-scm, but
            ;; without the git metadata available this fails.
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version)))
          (add-before 'build 'find-igraph
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "IGRAPH_EXTRA_INCLUDE_PATH"
                      (string-append (assoc-ref inputs "igraph")
                                     "/include/igraph:"
                                     (getenv "C_INCLUDE_PATH")))
              (setenv "IGRAPH_EXTRA_LIBRARY_PATH"
                      (getenv "LIBRARY_PATH")))))))
    (propagated-inputs
     (list python-ddt python-igraph))
    (inputs
     (list igraph))
    (native-inputs
     (list pkg-config
           python-pytest
           python-setuptools-scm
           python-wheel))
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
                    (url "https://github.com/facebookresearch/faiss")
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
     (list openblas))
    (native-inputs
     (list googletest))
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
CXXFLAGS = -fpermissive -fopenmp -fPIC
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
     (list python-matplotlib python-numpy))
    (description "Faiss is a library for efficient similarity search and
clustering of dense vectors.  This package provides Python bindings to the
Faiss library.")))

(define-public python-leidenalg
  (package
    (name "python-leidenalg")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "leidenalg" version))
       (sha256
        (base32
         "15fwld9hdw357rd026mzcwpah5liy4f33vc9x9kwy37g71b2rjf1"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f                      ;tests are not included
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'fix-requirements
                    (lambda _
                      (substitute* "setup.py"
                        (("python-igraph >=")
                         "igraph >=")))))))
    (native-inputs
     ;; XXX: setuptools >= 58 as shipped with Python 3.9+ removes support
     ;; for lib2to3, so use this older variant.
     (list pkg-config python-setuptools))
    (inputs
     (list igraph))
    (propagated-inputs
     (list python-igraph))
    (home-page "https://github.com/vtraag/leidenalg")
    (synopsis "Community detection in large networks")
    (description
     "Leiden is a general algorithm for methods of community detection in
large networks.  This package implements the Leiden algorithm in C++ and
exposes it to Python.  Besides the relative flexibility of the implementation,
it also scales well, and can be run on graphs of millions of nodes (as long as
they can fit in memory).  The core function is @code{find_partition} which
finds the optimal partition using the Leiden algorithm, which is an extension
of the Louvain algorithm, for a number of different methods.")
    (license license:gpl3+)))

(define-public edge-addition-planarity-suite
  (package
    (name "edge-addition-planarity-suite")
    (version "3.0.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url (string-append "https://github.com/graph-algorithms/"
                                  name))
              (commit (string-append "Version_" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "01cm7ay1njkfsdnmnvh5zwc7wg7x189hq1vbfhh9p3ihrbnmqzh8"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool))
    (synopsis "Embedding of planar graphs")
    (description "The package provides a reference implementation of the
linear time edge addition algorithm for embedding planar graphs and
isolating planarity obstructions.")
    (license license:bsd-3)
    (home-page
      "https://github.com/graph-algorithms/edge-addition-planarity-suite")))

(define-public rw
  (package
    (name "rw")
    ;; There is a version 0.8, but the tarball is broken with symlinks
    ;; to /usr/share.
    (version "0.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/rankwidth/"
                                  "rw-" version ".tar.gz"))
       (sha256
        (base32
         "1rv2v42x2506x7f10349m1wpmmfxrv9l032bkminni2gbip9cjg0"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list igraph))
    (home-page "https://sourceforge.net/projects/rankwidth/")
    (synopsis "Rank-width and rank-decomposition of graphs")
    (description "rw computes rank-width and rank-decompositions
of graphs.")
    (license license:gpl2+)))

(define-public mscgen
  (package
    (name "mscgen")
    (version "0.20")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.mcternan.me.uk/mscgen/software/mscgen-src-"
                           version ".tar.gz"))
       (sha256
        (base32
         "08yw3maxhn5fl1lff81gmcrpa4j9aas4mmby1g9w5qcr0np82d1w"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list gd))
    (home-page "http://www.mcternan.me.uk/mscgen/")
    (synopsis "Message Sequence Chart Generator")
    (description "Mscgen is a small program that parses Message Sequence Chart
descriptions and produces PNG, SVG, EPS or server side image maps (ismaps) as
the output.  Message Sequence Charts (MSCs) are a way of representing entities
and interactions over some time period and are often used in combination with
SDL.  MSCs are popular in Telecoms to specify how protocols operate although
MSCs need not be complicated to create or use.  Mscgen aims to provide a simple
text language that is clear to create, edit and understand, which can also be
transformed into common image formats for display or printing.")
    (license license:gpl2+)))

(define-public python-graph-tool
  (package
    (name "python-graph-tool")
    (version "2.43")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://downloads.skewed.de/graph-tool/graph-tool-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "0v58in4rwk9fhjarjw6xfxpx5zz2z13sy3yvd14b5kr0884yw6sz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:imported-modules (,@%gnu-build-system-modules
                           (guix build python-build-system))
       #:modules (,@%gnu-build-system-modules
                  ((guix build python-build-system) #:select (site-packages)))
       #:configure-flags
       (list (string-append "--with-boost="
                            (assoc-ref %build-inputs "boost"))
             (string-append "--with-python-module-path="
                            (site-packages %build-inputs %outputs)))))
    (native-inputs
     (list ncurses pkg-config))
    (inputs
     (list boost
           cairomm-1.14
           cgal
           expat
           gmp
           gtk+
           python-wrapper
           sparsehash))
    (propagated-inputs
     (list python-matplotlib python-numpy python-pycairo python-scipy))
    (synopsis "Manipulate and analyze graphs with Python efficiently")
    (description "Graph-tool is an efficient Python module for manipulation
and statistical analysis of graphs (a.k.a. networks).  Contrary to most other
Python modules with similar functionality, the core data structures and
algorithms are implemented in C++, making extensive use of template
metaprogramming, based heavily on the Boost Graph Library.  This confers it a
level of performance that is comparable (both in memory usage and computation
time) to that of a pure C/C++ library.")
    (home-page "https://graph-tool.skewed.de/")
    (license license:lgpl3+)))
