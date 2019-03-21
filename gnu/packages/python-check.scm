;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
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
