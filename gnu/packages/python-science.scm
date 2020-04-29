;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017, 2018, 2019, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2019 Giacomo Leidi <goodoldpaul@autistici.org>
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
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages time)
  #:use-module (gnu packages xml)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system python))

(define-public python-scipy
  (package
    (name "python-scipy")
    (version "1.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "scipy" version))
       (sha256
        (base32 "192d8dsybvhv19igkrsafbdafx198nz7pibkjgrqjhlr66s3jfd0"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-numpy" ,python-numpy)
       ("python-matplotlib" ,python-matplotlib)
       ("python-pyparsing" ,python-pyparsing)))
    (inputs
     `(("lapack" ,lapack)
       ("openblas" ,openblas)))
    (native-inputs
     `(("python-cython" ,python-cython)
       ("python-pytest" ,python-pytest)
       ("python-sphinx" ,python-sphinx)
       ("python-numpydoc" ,python-numpydoc)
       ("gfortran" ,gfortran)
       ("perl" ,perl)
       ("which" ,which)))
    (outputs '("out" "doc"))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
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
                               m)))
             #t))
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
                         (assoc-ref inputs "openblas"))))
             #t))
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
                             (find-files "." ".*")))))
             #t))
         ;; Tests can only be run after the library has been installed and not
         ;; within the source directory.
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (with-directory-excursion "/tmp"
               (invoke "python" "-c"
                       "import scipy; scipy.test(verbose=2)")))))))
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
         "1cgvgin8fvckv96hjh3ikmwkra5rif51bdb75ifzf7xbil5iwcx4"))))))

(define-public python-scikit-image
  (package
    (name "python-scikit-image")
    (version "0.14.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "scikit-image" version))
       (sha256
        (base32 "07qchljkyxvg5nrm12fvszi7pmjk4m01qp0w0z8syxzxxs20pz8s"))))
    (build-system python-build-system)
    (arguments
     ;; TODO: Some tests require running X11 server. Disable them?
     '(#:tests? #f))
    ;; See DEPENDS.txt for the list of build and run time requiremnts
    (propagated-inputs
     `(("python-cloudpickle" ,python-cloudpickle)
       ("python-dask" ,python-dask)
       ("python-matplotlib" ,python-matplotlib)
       ("python-networkx" ,python-networkx)
       ("python-numpy" ,python-numpy)
       ("python-pillow" ,python-pillow)
       ("python-pywavelets" ,python-pywavelets)
       ("python-scipy" ,python-scipy)
       ("python-six" ,python-six)))
    (native-inputs
     `(("python-cython" ,python-cython)))
    (home-page "https://scikit-image.org/")
    (synopsis "Image processing in Python")
    (description
     "Scikit-image is a collection of algorithms for image processing.")
    (license license:bsd-3)))

(define-public python-pandas
  (package
    (name "python-pandas")
    (version "0.25.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pandas" version))
       (sha256
        (base32 "1gp2pvzdiakvgjmykdzdlzrsfbg4vjm49jjdl9s0ha0a3yfs34fa"))))
    (build-system python-build-system)
    (arguments
     `(#:modules ((guix build utils)
                  (guix build python-build-system)
                  (ice-9 ftw)
                  (srfi srfi-26))
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-which
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((which (assoc-ref inputs "which")))
                        (substitute* "pandas/io/clipboard/__init__.py"
                          (("^CHECK_CMD = .*")
                           (string-append "CHECK_CMD = \"" which "\"\n"))))
                      #t))
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
     `(("python-numpy" ,python-numpy)
       ("python-openpyxl" ,python-openpyxl)
       ("python-pytz" ,python-pytz)
       ("python-dateutil" ,python-dateutil)
       ("python-xlrd" ,python-xlrd)))
    (inputs
     `(("which" ,which)))
    (native-inputs
     `(("python-cython" ,python-cython)
       ("python-beautifulsoup4" ,python-beautifulsoup4)
       ("python-lxml" ,python-lxml)
       ("python-html5lib" ,python-html5lib)
       ("python-nose" ,python-nose)
       ("python-pytest" ,python-pytest)
       ("python-pytest-mock" ,python-pytest-mock)))
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
                       "if 'NULL byte' in msg or 'line contains NUL' in msg:"))
                    #t)))))))
