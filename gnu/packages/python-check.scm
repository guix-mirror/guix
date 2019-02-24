;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
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
