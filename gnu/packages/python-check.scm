;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2019 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020 Julien Lepiller <julien@lepiller.eu>
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

(define-module (gnu packages python-check)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages web)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python))

(define-public python-coveralls
  (package
    (name "python-coveralls")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "coveralls" version))
       (sha256
        (base32
         "1dswhd2q2412wrldi97hdwlsymj9pm79v7pvjx53z5wh2d33w8bg"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-coverage" ,python-coverage)
       ("python-docopt" ,python-docopt)
       ("python-pyyaml" ,python-pyyaml)
       ("python-requests" ,python-requests)
       ("python-sh" ,python-sh)
       ("python-urllib3" ,python-urllib3)))
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest)))
    (home-page "https://github.com/coveralls-clients/coveralls-python")
    (synopsis "Show coverage stats online via coveralls.io")
    (description
     "Coveralls.io is a service for publishing code coverage statistics online.
This package provides seamless integration with coverage.py (and thus pytest,
nosetests, etc...) in Python projects.")
    (license license:expat)))

(define-public python-vcrpy
  (package
    (name "python-vcrpy")
    (version "2.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "vcrpy" version))
        (sha256
         (base32
          "0kws7l3hci1dvjv01nxw3805q9v2mwldw58bgl8s90wqism69gjp"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f)) ; tests require more packages for python-pytest-httpbin
    (propagated-inputs
     `(("python-pyyaml" ,python-pyyaml)
       ("python-six" ,python-six)
       ("python-wrapt" ,python-wrapt)
       ("python-yarl" ,python-yarl)))
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest)
       ("python-pytest-httpbin" ,python-pytest-httpbin)))
    (home-page "https://github.com/kevin1024/vcrpy")
    (synopsis "Automatically mock your HTTP interactions")
    (description
     "VCR.py simplifies and speeds up tests that make HTTP requests.  The first
time you run code that is inside a VCR.py context manager or decorated function,
VCR.py records all HTTP interactions that take place through the libraries it
supports and serializes and writes them to a flat file (in yaml format by
default).  This flat file is called a cassette.  When the relevant piece of code
is executed again, VCR.py will read the serialized requests and responses from
the aforementioned cassette file, and intercept any HTTP requests that it
recognizes from the original test run and return the responses that corresponded
to those requests.  This means that the requests will not actually result in
HTTP traffic, which confers several benefits including:
@enumerate
@item The ability to work offline
@item Completely deterministic tests
@item Increased test execution speed
@end enumerate
If the server you are testing against ever changes its API, all you need to do
is delete your existing cassette files, and run your tests again.  VCR.py will
detect the absence of a cassette file and once again record all HTTP
interactions, which will update them to correspond to the new API.")
    (license license:expat)))

(define-public python-pytest-checkdocs
  (package
    (name "python-pytest-checkdocs")
    (version "1.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-checkdocs" version))
       (sha256
        (base32 "0j6j1gvj6x451y3qsx4xbaq9p1w9gg3mwk7n0w80cy8vdyjkngb0"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-docutils" ,python-docutils)
       ("python-importlib-metadata" ,python-importlib-metadata)
       ("python-more-itertools" ,python-more-itertools)))
    (native-inputs
     `(("python-setuptools-scm" ,python-setuptools-scm)))
    (home-page "https://github.com/jaraco/pytest-checkdocs")
    (synopsis "Check the README when running tests")
    (description
     "This package provides a pytest plugin that checks the long description
of the project to ensure it renders properly.")
    (license license:expat)))

(define-public python-pytest-flake8
  (package
    (name "python-pytest-flake8")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-flake8" version))
       (sha256
        (base32
         "1h30gd21fjsafqxwclf25sdh89vrdz7rsh4lzw11aiw7ww9mq8jd"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-flake8" ,python-flake8)))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (home-page "https://github.com/tholo/pytest-flake8")
    (synopsis "Pytest plugin to check FLAKE8 requirements")
    (description
     "This package provides a pytest plugin for efficiently checking PEP8
compliance.")
    (license license:bsd-3)))

(define-public python-pytest-isort
  (package
    (name "python-pytest-isort")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-isort" version))
       (sha256
        (base32 "06myn5hhxs5yp8dqr1yjsgcnnxnsrvsqannm00bvaw0qml6ydzjb"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (setenv "PYTHONPATH"
                     (string-append (getcwd) ":"
                                    (getenv "PYTHONPATH")))
             (invoke "pytest"))))))
    (propagated-inputs
     `(("python-isort" ,python-isort)
       ("python-pytest" ,python-pytest)))
    (home-page "https://github.com/moccu/pytest-isort/")
    (synopsis "Pytest plugin to check import ordering using isort")
    (description
     "This package provides a pytest plugin to check import ordering using
isort.")
    (license license:bsd-3)))

(define-public python-pytest-shutil
  (package
    (name "python-pytest-shutil")
    (version "1.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-shutil" version))
       (sha256
        (base32
         "0q8j0ayzmnvlraml6i977ybdq4xi096djhf30n2m1rvnvrhm45nq"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-tests
           (lambda _
             (mkdir "/tmp/bin")
             (substitute* "tests/integration/test_cmdline_integration.py"
               (("dirname = '/bin'")
                "dirname = '/tmp/bin'")
               (("bindir = os.path.realpath\\('/bin'\\)")
                "bindir = os.path.realpath('/tmp/bin')"))
             #t)))))
    (propagated-inputs
     `(("python-contextlib2" ,python-contextlib2)
       ("python-execnet" ,python-execnet)
       ("python-pathpy" ,python-pathpy)
       ("python-termcolor" ,python-termcolor)))
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest)
       ("python-setuptools-git" ,python-setuptools-git)))
    (home-page "https://github.com/manahl/pytest-plugins")
    (synopsis "Assorted shell and environment tools for py.test")
    (description
     "This package provides assorted shell and environment tools for the
py.test testing framework.")
    (license license:expat)))

(define-public python-pytest-fixture-config
  (package
    (name "python-pytest-fixture-config")
    (version "1.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-fixture-config" version))
       (sha256
        (base32
         "13i1qpz22w3x4dmw8vih5jdnbqfqvl7jiqs0dg764s0zf8bp98a1"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-setuptools-git" ,python-setuptools-git)))
    (home-page "https://github.com/manahl/pytest-plugins")
    (synopsis "Fixture configuration utils for py.test")
    (description
     "This package provides fixture configuration utilities for the py.test
testing framework.")
    (license license:expat)))

(define-public python-pytest-virtualenv
  (package
    (name "python-pytest-virtualenv")
    (version "1.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-virtualenv" version))
       (sha256
        (base32
         "03w2zz3crblj1p6i8nq17946hbn3zqp9z7cfnifw47hi4a4fww12"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Reference the virtualenv executable directly, to avoid the need
         ;; for PYTHONPATH, which gets cleared when instantiating a new
         ;; virtualenv with pytest-virtualenv.
         (add-after 'unpack 'patch-virtualenv-executable
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((virtualenv (assoc-ref inputs "python-virtualenv"))
                    (virtualenv-bin (string-append virtualenv
                                                   "/bin/virtualenv")))
               (substitute* "pytest_virtualenv.py"
                 (("^DEFAULT_VIRTUALENV_FIXTURE_EXECUTABLE.*$")
                  (format #f "DEFAULT_VIRTUALENV_FIXTURE_EXECUTABLE = '~a'"
                          virtualenv-bin)))
               #t))))))
    (propagated-inputs
     `(("python-pytest-shutil" ,python-pytest-shutil)
       ("python-pytest-fixture-config" ,python-pytest-fixture-config)))
    (inputs
     `(("python-virtualenv" ,python-virtualenv)))
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest)
       ("python-setuptools-git" ,python-setuptools-git)))
    (home-page "https://github.com/manahl/pytest-plugins")
    (synopsis "Virtualenv fixture for py.test")
    (description "This package provides a virtualenv fixture for the py.test
framework.")
    (license license:expat)))

(define-public python-codacy-coverage
  (package
    (name "python-codacy-coverage")
    (version "1.3.11")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "codacy-coverage" version))
        (sha256
         (base32
          "1g0c0w56xdkmqb8slacyw5qhzrkp814ng3ddh2lkiij58y9m2imr"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)); no tests
    (propagated-inputs
     `(("python-check-manifest" ,python-check-manifest)))
    (home-page "https://github.com/codacy/python-codacy-coverage")
    (synopsis "Codacy coverage reporter for Python")
    (description "This package analyses Python test suites and reports how much
of the code is covered by them.  This tool is part of the Codacy suite for
analysing code quality.")
    (license license:expat)))

(define-public python-httmock
  (package
    (name "python-httmock")
    (version "1.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "httmock" version))
        (sha256
         (base32
          "1zj1fcm0n6f0wr9mr0hmlqz9430fnr5cdwd5jkcvq9j44bnsrfz0"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)); no tests
    (propagated-inputs
     `(("python-requests" ,python-requests)))
    (home-page "https://github.com/patrys/httmock")
    (synopsis "Mocking library for requests.")
    (description "This package provides a library for replying fake data to
Python software under test, when they make an HTTP query.")
    (license license:asl2.0)))
