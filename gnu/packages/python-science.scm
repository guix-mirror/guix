;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2020, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016-2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2019 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2020 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2020, 2021 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 Greg Hogan <code@greghogan.com>
;;; Copyright © 2021 Roel Janssen <roel@gnu.org>
;;; Copyright © 2021 Paul Garlick <pgarlick@tourbillion-technology.com>
;;; Copyright © 2021 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2021 Felix Gruber <felgru@posteo.net>
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

(define-module (gnu packages python-science)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages simulation)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages time)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system python))

(define-public python-scipy
  (package
    (name "python-scipy")
    (version "1.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "scipy" version))
       (sha256
        (base32 "1gxsnw6viz2j3sm8ak2a8l7fcn4b2zm3kzfm8w57xxyyrzx7an5b"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-numpy python-matplotlib python-pyparsing))
    (inputs
     (list openblas pybind11))
    (native-inputs
     (list python-cython
           python-pydata-sphinx-theme
           python-pytest
           python-sphinx
           python-sphinx-panels
           python-numpydoc
           gfortran
           perl
           which))
    (outputs '("out" "doc"))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-pythran
           (lambda _
             (setenv "SCIPY_USE_PYTHRAN" "0")))
         (add-before 'build 'change-home-dir
           (lambda _
             ;; Change from /homeless-shelter to /tmp for write permission.
             (setenv "HOME" "/tmp")))
         (add-after 'unpack 'disable-broken-tests
           (lambda _
             (substitute* "scipy/sparse/linalg/dsolve/tests/test_linsolve.py"
               (("^( +)def test_threads_parallel\\(self\\):" m indent)
                (string-append indent
                               "@pytest.mark.skip(reason=\"Disabled by Guix\")\n"
                               m)))
             (substitute* "scipy/sparse/linalg/eigen/arpack/tests/test_arpack.py"
               (("^def test_parallel_threads\\(\\):" m)
                (string-append "@pytest.mark.skip(reason=\"Disabled by Guix\")\n"
                               m)))))
         (add-before 'build 'configure-openblas
           (lambda* (#:key inputs #:allow-other-keys)
             (call-with-output-file "site.cfg"
               (lambda (port)
                 (format port
                         "[blas]
libraries = openblas
library_dirs = ~a/lib
include_dirs = ~a/include

# backslash-n to make emacs happy
\n[atlas]
library_dirs = ~a/lib
atlas_libs = openblas
"
                         (assoc-ref inputs "openblas")
                         (assoc-ref inputs "openblas")
                         (assoc-ref inputs "openblas"))))))
         (add-after 'install 'install-doc
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((data (string-append (assoc-ref outputs "doc") "/share"))
                    (doc (string-append data "/doc/" ,name "-" ,version))
                    (html (string-append doc "/html"))
                    (pyver ,(string-append "PYVER=" (version-major+minor
                                                     (package-version python))))
                    ;; By default it tries to run sphinx-build through the Python
                    ;; interpreter which won't work with our shell wrapper.
                    (sphinxbuild "SPHINXBUILD=LANG=C sphinx-build"))
               ;; Make installed package available for building the
               ;; documentation
               (add-installed-pythonpath inputs outputs)
               (with-directory-excursion "doc"
                 ;; Fix generation of images for mathematical expressions.
                 (substitute* (find-files "source" "conf\\.py")
                   (("pngmath_use_preview = True")
                    "pngmath_use_preview = False"))
                 (mkdir-p html)
                 (invoke "make" "html" pyver sphinxbuild)
                 (with-directory-excursion "build/html"
                   (for-each (lambda (file)
                               (let* ((dir (dirname file))
                                      (tgt-dir (string-append html "/" dir)))
                                 (install-file file html)))
                             (find-files ".")))))))
         ;; Tests can only be run after the library has been installed and not
         ;; within the source directory.
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (with-directory-excursion "/tmp"
                 (invoke "python" "-c"
                         "import scipy; scipy.test(verbose=2)"))))))))
    (home-page "https://www.scipy.org/")
    (synopsis "The Scipy library provides efficient numerical routines")
    (description "The SciPy library is one of the core packages that make up
the SciPy stack.  It provides many user-friendly and efficient numerical
routines such as routines for numerical integration and optimization.")
    (properties `((python2-variant . ,(delay python2-scipy))))
    (license license:bsd-3)))

;; Version 1.2.2 is the last version to support Python 2
(define-public python2-scipy
  (package
    (inherit (package-with-python2
              (strip-python2-variant python-scipy)))
    (version "1.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "scipy" version))
       (sha256
        (base32
         "1cgvgin8fvckv96hjh3ikmwkra5rif51bdb75ifzf7xbil5iwcx4"))))
    (native-inputs
     (list python2-cython
           python2-pytest
           python2-sphinx
           python2-numpydoc
           gfortran-7
           gcc-7
           perl
           which))))

(define-public python2-weave
  (package
    (name "python2-weave")
    (version "0.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "weave" version))
       (sha256
        (base32 "0jnm3584mfichgwgrd1gk5i42ll9c08nkw9716n947n4338f6ghs"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "nosetests" "-v"
                     "--exclude"
                     "test_(user|incorrect_ownership|char_fail|obj_fail)"))))))
    (propagated-inputs
     (list python2-numpy))
    (native-inputs
     (list python2-nose))
    (home-page "https://www.scipy.org/")
    (synopsis "Tools for including C/C++ code within Python code")
    (description "Weave is the stand-alone version of the obsolete Scipy
submodule @code{scipy.weave}.  It is Python 2.x only, and is provided for
users that need new versions of Scipy but have existing code that still
depends on @code{scipy.weave}.  For new code, users are recommended to use
Cython.")
    (license license:bsd-3)))

(define-public python-scikit-fuzzy
  (package
    (name "python-scikit-fuzzy")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "scikit-fuzzy" version))
       (sha256
        (base32 "0bp1n771fj44kdp7a00bcvfwirvv2rc803b7g6yf3va7v0j29c8s"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))   ;XXX: not compatible with newer numpy.testing
    (native-inputs
     (list python-nose))
    (propagated-inputs
     (list python-networkx python-numpy python-scipy))
    (home-page "https://github.com/scikit-fuzzy/scikit-fuzzy")
    (synopsis "Fuzzy logic toolkit for SciPy")
    (description
     "This package implements many useful tools for projects involving fuzzy
logic, also known as grey logic.")
    (license license:bsd-3)))

(define-public python-scikit-image
  (package
    (name "python-scikit-image")
    (version "0.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "scikit-image" version))
       (sha256
        (base32 "0wgisa03smhrphcjnhq7waa5vyyd32b67hblapjbqrqqj751idpv"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'change-home-dir
           (lambda _
             ;; Change from /homeless-shelter to /tmp for write permission.
             (setenv "HOME" "/tmp")
             #t))
         (replace 'build
           (lambda _
             (invoke "make")))
         (replace 'check
           (lambda _
             ;; The following tests require online data.
             (invoke "python" "-m" "pytest" "skimage" "--doctest-modules" "-k"
                     (string-append "not test_ndim"
                                    " and not test_skin")))))))
    ;; See requirements/ for the list of build and run time requirements.
    ;; NOTE: scikit-image has an optional dependency on python-pooch, however
    ;; propagating it would enable many more tests that require online data.
    (propagated-inputs
     (list python-cloudpickle
           python-dask
           python-imageio
           python-matplotlib
           python-networkx
           python-numpy
           python-pillow
           python-pywavelets
           python-scipy
           python-tifffile))
    (native-inputs
     (list python-codecov
           python-cython
           python-flake8
           python-pytest
           python-pytest-cov
           python-pytest-localserver
           python-wheel))
    (home-page "https://scikit-image.org/")
    (synopsis "Image processing in Python")
    (description
     "Scikit-image is a collection of algorithms for image processing.")
    (license license:bsd-3)))

(define-public python-sgp4
  (package
    (name "python-sgp4")
    (version "2.12")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sgp4" version))
       (sha256
        (base32 "0dncp9i5b6afkg7f8mj9j0qzsp008b8v73yc0qkmizhpns7mvwvx"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-numpy))
    (home-page "https://github.com/brandon-rhodes/python-sgp4")
    (synopsis "Track earth satellite TLE orbits using SGP4")
    (description
     "This package provides a Python implementation of the most recent version
of the SGP4 satellite tracking algorithm.")
    (license license:expat)))

(define-public python-pandas
  (package
    (name "python-pandas")
    (version "1.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pandas" version))
       (sha256
        (base32 "1wd92ra8xcjgigbypid53gvby89myg68ica6r8hdw4hhvvsqahhy"))))
    (build-system python-build-system)
    (arguments
     `(#:modules ((guix build utils)
                  (guix build python-build-system)
                  (ice-9 ftw)
                  (srfi srfi-1)
                  (srfi srfi-26))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-which
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((which (assoc-ref inputs "which")))
               (substitute* "pandas/io/clipboard/__init__.py"
                 (("^WHICH_CMD = .*")
                  (string-append "WHICH_CMD = \"" which "\"\n"))))))
         (add-before 'check 'prepare-x
           (lambda _
             (system "Xvfb &")
             (setenv "DISPLAY" ":0")
             ;; xsel needs to write a log file.
             (setenv "HOME" "/tmp")))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (let ((build-directory
                    (string-append
                     (getcwd) "/build/"
                     (first (scandir "build"
                                     (cut string-prefix? "lib." <>))))))
               (substitute* "pyproject.toml"
                 ;; Not all data files are distributed with the tarball.
                 (("--strict-data-files ") ""))
               (with-directory-excursion build-directory
                 (when tests?
                   (invoke "pytest" "-vv" "pandas" "--skip-slow"
                           "--skip-network"
                           "-k"
                           (string-append
                            ;; These test access the internet (see:
                            ;; https://github.com/pandas-dev/pandas/issues/45085).:
                            ;; pandas/tests/io/xml/test_xml.py::test_wrong_url[lxml]
                            ;; pandas/tests/io/xml/test_xml.py::test_wrong_url[etree]
                            "not test_wrong_url"
                            ;; TODO: Missing input
                            " and not TestS3"
                            " and not s3"))))))))))
    (propagated-inputs
     (list python-jinja2
           python-numpy
           python-openpyxl
           python-pytz
           python-dateutil
           python-xlrd
           python-xlsxwriter))
    (inputs
     (list which xclip xsel))
    (native-inputs
     (list python-cython
           python-beautifulsoup4
           python-lxml
           python-html5lib
           python-pytest
           python-pytest-mock
           ;; Needed to test clipboard support.
           xorg-server-for-tests))
    (home-page "https://pandas.pydata.org")
    (synopsis "Data structures for data analysis, time series, and statistics")
    (description
     "Pandas is a Python package providing fast, flexible, and expressive data
structures designed to make working with structured (tabular,
multidimensional, potentially heterogeneous) and time series data both easy
and intuitive.  It aims to be the fundamental high-level building block for
doing practical, real world data analysis in Python.")
    (properties `((python2-variant . ,(delay python2-pandas))))
    (license license:bsd-3)))

;; Pandas 0.24.x are the last versions that support Python 2.
(define-public python2-pandas
  (let ((pandas (package-with-python2
                 (strip-python2-variant python-pandas))))
    (package
      (inherit pandas)
      (version "0.24.2")
      (source (origin
                (method url-fetch)
                (uri (pypi-uri "pandas" version))
                (sha256
                 (base32
                  "18imlm8xbhcbwy4wa957a1fkamrcb0z988z006jpfda3ki09z4ag"))
                (modules '((guix build utils)))
                (snippet
                 '(begin
                    ;; Adjust for renamed error message in Python 2.7.17.  Taken
                    ;; from <https://github.com/pandas-dev/pandas/pull/29294>.
                    (substitute* "pandas/io/parsers.py"
                      (("if 'NULL byte' in msg:")
                       "if 'NULL byte' in msg or 'line contains NUL' in msg:"))))))
      (arguments
       `(#:modules ((guix build utils)
                    (guix build python-build-system)
                    (ice-9 ftw)
                    (srfi srfi-26))
         #:python ,python-2
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-which
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((which (assoc-ref inputs "which")))
                 (substitute* "pandas/io/clipboard/__init__.py"
                   (("^CHECK_CMD = .*")
                    (string-append "CHECK_CMD = \"" which "\"\n"))))))
           (replace 'check
             (lambda _
               (let ((build-directory
                      (string-append
                       (getcwd) "/build/"
                       (car (scandir "build"
                                     (cut string-prefix? "lib." <>))))))
                 ;; Disable the "strict data files" option which causes
                 ;; the build to error out if required data files are
                 ;; not available (as is the case with PyPI archives).
                 (substitute* "setup.cfg"
                   (("addopts = --strict-data-files") "addopts = "))
                 (with-directory-excursion build-directory
                   ;; Delete tests that require "moto" which is not yet
                   ;; in Guix.
                   (for-each delete-file
                             '("pandas/tests/io/conftest.py"
                               "pandas/tests/io/json/test_compression.py"
                               "pandas/tests/io/parser/test_network.py"
                               "pandas/tests/io/test_parquet.py"))
                   (invoke "pytest" "-vv" "pandas" "--skip-slow"
                           "--skip-network" "-k"
                           ;; XXX: Due to the deleted tests above.
                           "not test_read_s3_jsonl"))))))))
      (propagated-inputs
       (list python2-numpy python2-openpyxl python2-pytz python2-dateutil
             python2-xlrd))
      (inputs
       (list which))
      (native-inputs
       (list python2-cython
             python2-beautifulsoup4
             python2-lxml
             python2-html5lib
             python2-nose
             python2-pytest
             python2-pytest-mock)))))

(define-public python2-pyflow
  (package
    (name "python2-pyflow")
    (version "1.1.20")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/Illumina/pyflow/releases/download/v"
                    version "/pyflow-" version ".tar.gz"))
              (sha256
               (base32
                "1bvfvviw58cndyn862qnv9nj3d9cd3a0dm4vc4sd9vwq8a6z1riv"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; There is no test suite.
       ;; There is no official Python 3-compatible version and upstream is
       ;; dead. See https://github.com/Illumina/pyflow/issues/20.
       #:python ,python-2))
    (home-page "https://illumina.github.io/pyflow/")
    (synopsis "Tool to manage tasks in a task dependency graph")
    (description "This package is a Python module to manage tasks in the
context of a task dependency graph.  It has some similarities to make.")
    (license license:bsd-2)))

(define-public python-bottleneck
  (package
    (name "python-bottleneck")
    (version "1.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Bottleneck" version))
       (sha256
        (base32 "0wz5320jx3n4q2nsvwvc7cpi66b46qbals9v53m955rmcq5ry5r0"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "python" "setup.py" "pytest"))))))
    (native-inputs
     (list python-hypothesis python-pytest python-pytest-runner))
    (propagated-inputs
     (list python-numpy))
    (home-page "https://github.com/pydata/bottleneck")
    (synopsis "Fast NumPy array functions written in C")
    (description
     "Bottleneck is a collection of fast, NaN-aware NumPy array functions
written in C.")
    (license license:bsd-2)))

(define-public python-baycomp
  (package
    (name "python-baycomp")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "baycomp" version))
       (sha256
        (base32 "1c1354a7b3g8slychjgyjxqdm8z40z9kviyl9n4g9kfpdg0p4d64"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-matplotlib python-numpy python-scipy))
    (home-page "https://github.com/janezd/baycomp")
    (synopsis "Library for comparison of Bayesian classifiers")
    (description
     "Baycomp is a library for Bayesian comparison of classifiers.  Functions
in the library compare two classifiers on one or on multiple data sets.  They
compute three probabilities: the probability that the first classifier has
higher scores than the second, the probability that differences are within the
region of practical equivalence (rope), or that the second classifier has
higher scores.")
    (license license:expat)))

(define-public python-xarray
  (package
    (name "python-xarray")
    (version "0.15.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "xarray" version))
              (sha256
               (base32
                "1yx8j66b7rn10m2l6gmn8yr9cn38pi5cj0x0wwpy4hdnhy6i7qv4"))))
    (build-system python-build-system)
    (native-inputs
     (list python-setuptools-scm python-pytest))
    (propagated-inputs
     (list python-numpy python-pandas))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "pytest"))))))
    (home-page "https://github.com/pydata/xarray")
    (synopsis "N-D labeled arrays and datasets")
    (description "Xarray (formerly xray) makes working with labelled
multi-dimensional arrays simple, efficient, and fun!

Xarray introduces labels in the form of dimensions, coordinates and attributes
on top of raw NumPy-like arrays, which allows for a more intuitive, more
concise, and less error-prone developer experience.  The package includes a
large and growing library of domain-agnostic functions for advanced analytics
and visualization with these data structures.")
    (license license:asl2.0)))

(define-public python-msgpack-numpy
  (package
    (name "python-msgpack-numpy")
    (version "0.4.6.post0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "msgpack-numpy" version))
       (sha256
        (base32
         "0syzy645mwcy7lfjwz6pc8f9p2vv1qk4limc8iina3l5nnf0rjyz"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-msgpack python-numpy))
    (home-page "https://github.com/lebedov/msgpack-numpy")
    (synopsis
     "Numpy data serialization using msgpack")
    (description
     "This package provides encoding and decoding routines that enable the
serialization and deserialization of numerical and array data types provided
by numpy using the highly efficient @code{msgpack} format.  Serialization of
Python's native complex data types is also supported.")
    (license license:bsd-3)))

(define-public python-ruffus
  (package
    (name "python-ruffus")
    (version "2.8.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ruffus" version))
       (sha256
        (base32
         "1ai673k1s94s8b6pyxai8mk17p6zvvyi87rl236fs6ls8mpdklvc"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (with-directory-excursion "ruffus/test"
                 (invoke "bash" "run_all_unit_tests3.cmd"))))))))
    (native-inputs
     (list python-pytest))
    (home-page "http://www.ruffus.org.uk")
    (synopsis "Light-weight computational pipeline management")
    (description
     "Ruffus is designed to allow scientific and other analyses to be
automated with the minimum of fuss and the least effort.")
    (license license:expat)))

(define-public python-statannot
  (package
    (name "python-statannot")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "statannot" version))
       (sha256
        (base32
         "1f8c2sylzr7lpjbyqxsqlp9xi8rj3d8c9hfh98x4jbb83zxc4026"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-numpy python-seaborn python-matplotlib python-pandas
           python-scipy))
    (home-page
     "https://github.com/webermarcolivier/statannot")
    (synopsis "Add annotations to existing plots generated by seaborn")
    (description
     "This is a Python package to compute statistical test and add statistical
annotations on an existing boxplots and barplots generated by seaborn.")
    (license license:expat)))

(define-public python-upsetplot
  (package
    (name "python-upsetplot")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "UpSetPlot" version))
       (sha256
        (base32
         "11zrykwnb00w5spx4mnsnm0f9gwrphdczainpmwkyyi50vipaa2l"))
       (modules '((guix build utils)))
       (snippet
        ;; Patch for compatibility with newer setuptools:
        ;; https://github.com/jnothman/UpSetPlot/pull/178
        '(substitute* "upsetplot/data.py"
           (("import distutils")
            "from distutils.version import LooseVersion")
           (("if distutils\\.version\\.LooseVersion")
            "if LooseVersion")))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest" "-v" "--doctest-modules")))))))
    (propagated-inputs
     (list python-matplotlib python-pandas))
    (native-inputs
     (list python-pytest-runner python-pytest-cov))
    (home-page "https://upsetplot.readthedocs.io")
    (synopsis "Draw UpSet plots with Pandas and Matplotlib")
    (description
     "This is a Python implementation of UpSet plots by Lex et al.
UpSet plots are used to visualize set overlaps; like Venn diagrams but more
readable.")
    (license license:bsd-3)))

(define-public python-vedo
  (package
    (name "python-vedo")
    (version "2021.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/marcomusy/vedo")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "18i3ajh5jzhpc86di15lwh4jv97jhm627ii877sa4yhv6abzjfpn"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'build 'mpi-setup
           ,%openmpi-setup)
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (setenv "HOME" (getcwd))
             (add-installed-pythonpath inputs outputs)
             (with-directory-excursion "tests"
               (for-each (lambda (dir)
                           (with-directory-excursion dir
                             (invoke "./run_all.sh")))
                         '("common" "dolfin")))
             #t)))))
    (inputs        ; for the check phase
     `(("dolfin" ,fenics)
       ("pkgconfig" ,python-pkgconfig)
       ("matplotlib" ,python-matplotlib)))
    (native-inputs ; for python-pkgconfig
     (list pkg-config))
    (propagated-inputs
     `(("numpy" ,python-numpy)
       ("vtk" ,vtk)))
    (home-page "https://github.com/marcomusy/vedo")
    (synopsis
     "Analysis and visualization of 3D objects and point clouds")
    (description
     "@code{vedo} is a fast and lightweight python module for
scientific analysis and visualization.  The package provides a wide
range of functionalities for working with three-dimensional meshes and
point clouds.  It can also be used to generate high quality
two-dimensional renderings such as scatter plots and histograms.
@code{vedo} is based on @code{vtk} and @code{numpy}, with no other
dependencies.")
    ;; vedo is released under the Expat license.  Included fonts are
    ;; covered by the OFL license and textures by the CC0 license.
    ;; The earth images are in the public domain.
    (license (list license:expat
                   license:silofl1.1
                   license:cc0
                   license:public-domain))))

(define-public python-pandas-flavor
  (package
    (name "python-pandas-flavor")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pandas_flavor" version))
       (sha256
        (base32
         "12g4av8gpl6l83yza3h97j3f2jblqv69frlidrvdq8ny2rc6awbq"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-pandas python-xarray))
    (home-page "https://github.com/Zsailer/pandas_flavor")
    (synopsis "Write your own flavor of Pandas")
    (description "Pandas 0.23 added a simple API for registering accessors
with Pandas objects.  Pandas-flavor extends Pandas' extension API by

@itemize
@item adding support for registering methods as well
@item making each of these functions backwards compatible with older versions
of Pandas
@end itemize")
    (license license:expat)))

(define-public python-pingouin
  (package
    (name "python-pingouin")
    (version "0.5.0")
    (source
     ;; The PyPI tarball does not contain the tests.
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/raphaelvallat/pingouin")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "01aaq023q4bymffrc2wm56af87da32wcvy5d5156i4g7qgvh346r"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; On loading, Pingouin uses the outdated package to check if a newer
         ;; version is available on PyPI. This check adds an extra dependency
         ;; and is irrelevant to Guix users. So, disable it.
         (add-after 'unpack 'remove-outdated-check
           (lambda _
             (substitute* "setup.py"
               (("'outdated',") ""))
             (substitute* "pingouin/__init__.py"
               (("^from outdated[^\n]*") "")
               (("^warn_if_outdated[^\n]*") ""))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest")))))))
    (native-inputs
     (list python-pytest python-pytest-cov))
    (propagated-inputs
     (list python-matplotlib
           python-mpmath
           python-numpy
           python-pandas
           python-pandas-flavor
           python-scikit-learn
           python-scipy
           python-seaborn
           python-statsmodels
           python-tabulate))
    (home-page "https://pingouin-stats.org/")
    (synopsis "Statistical package for Python")
    (description "Pingouin is a statistical package written in Python 3 and
based mostly on Pandas and NumPy.  Its features include

@itemize
@item ANOVAs: N-ways, repeated measures, mixed, ancova
@item Pairwise post-hocs tests (parametric and non-parametric) and pairwise
correlations
@item Robust, partial, distance and repeated measures correlations
@item Linear/logistic regression and mediation analysis
@item Bayes Factors
@item Multivariate tests
@item Reliability and consistency
@item Effect sizes and power analysis
@item Parametric/bootstrapped confidence intervals around an effect size or a
correlation coefficient
@item Circular statistics
@item Chi-squared tests
@item Plotting: Bland-Altman plot, Q-Q plot, paired plot, robust correlation,
and more
@end itemize")
    (license license:gpl3)))

(define-public python-distributed
  (package
    (name "python-distributed")
    (version "2021.11.2")
    (source
     (origin
       ;; The test files are not included in the archive on pypi
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dask/distributed")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1p20cbyabzl7rs8y3ydzszsskh4kw088m252ghgairhs0p2f95hl"))
       ;; Delete bundled copy of python-versioneer.
       (snippet '(delete-file "versioneer.py"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'versioneer
           (lambda _
             (invoke "versioneer" "install")))
         (add-after 'unpack 'fix-references
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* '("distributed/comm/tests/test_ucx_config.py"
                            "distributed/tests/test_client.py"
                            "distributed/tests/test_queues.py"
                            "distributed/tests/test_variable.py"
                            "distributed/cli/tests/test_tls_cli.py"
                            "distributed/cli/tests/test_dask_spec.py"
                            "distributed/cli/tests/test_dask_worker.py"
                            "distributed/cli/tests/test_dask_scheduler.py")
               (("\"dask-scheduler\"")
                (format #false "\"~a/bin/dask-scheduler\""
                        (assoc-ref outputs "out")))
               (("\"dask-worker\"")
                (format #false "\"~a/bin/dask-worker\""
                        (assoc-ref outputs "out"))))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (setenv "DISABLE_IPV6" "1")
               (invoke "pytest" "-vv" "distributed"
                       "-m" "not slow and not gpu and not ipython and not avoid_ci"
                       "-k"
                       ;; TODO: These tests fail for unknown reasons:
                       (string-append
                        ;; TimeoutExpired
                        "not test_text"
                        ;; AssertionError
                        " and not test_version_option"
                        ;; "The 'distributed' distribution was not found"
                        " and not test_register_backend_entrypoint"
                        ;; "AttributeError: module 'distributed.dashboard' has no attribute 'scheduler'"
                        " and not test_get_client_functions_spawn_clusters"))))))))
    (propagated-inputs
     (list python-click
           python-cloudpickle
           python-cryptography
           python-dask
           python-msgpack
           python-psutil
           python-pyyaml
           python-setuptools
           python-sortedcontainers
           python-tblib
           python-toolz
           python-tornado-6
           python-zict))
    (native-inputs
     (list python-pytest python-versioneer))
    (home-page "https://distributed.dask.org")
    (synopsis "Distributed scheduler for Dask")
    (description "Dask.distributed is a lightweight library for distributed
computing in Python.  It extends both the @code{concurrent.futures} and
@code{dask} APIs to moderate sized clusters.")
    (license license:bsd-3)))

(define-public python-modin
  (package
    (name "python-modin")
    (version "0.10.1")
    (source
     (origin
       ;; The archive on pypi does not include all required files.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/modin-project/modin")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "128ghfb9ncmnn8km409xjcdppvn9nr9jqw8rkvsfavh7wnwlk509"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-files-writable
           (lambda _
             (for-each make-file-writable (find-files "."))))
         (add-after 'unpack 'loosen-requirements
           (lambda _
             (substitute* "setup.py"
               ;; Don't depend on a specific version of Pandas.
               (("pandas==")
                "pandas>="))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (setenv "MODIN_ENGINE" "dask")
               (invoke "python" "-m" "pytest"
                       "modin/pandas/test/test_concat.py")
               (setenv "MODIN_ENGINE" "python")
               (invoke "python" "-m" "pytest"
                       "modin/pandas/test/test_concat.py")))))))
    (propagated-inputs
     (list python-cloudpickle
           python-dask
           python-distributed
           python-numpy
           python-packaging
           python-pandas))
    (native-inputs
     (list python-coverage
           python-jinja2
           python-lxml
           python-matplotlib
           python-msgpack
           python-openpyxl
           python-psutil
           python-pyarrow
           python-pytest
           python-pytest-benchmark
           python-pytest-cov
           python-pytest-xdist
           python-scipy
           python-sqlalchemy
           python-tables
           python-tqdm
           python-xarray
           python-xlrd))
    (home-page "https://github.com/modin-project/modin")
    (synopsis "Make your pandas code run faster")
    (description
     "Modin uses Ray or Dask to provide an effortless way to speed up your
pandas notebooks, scripts, and libraries.  Unlike other distributed DataFrame
libraries, Modin provides seamless integration and compatibility with existing
pandas code.")
    (license license:asl2.0)))
