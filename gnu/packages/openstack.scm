;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Cyril Roelandt <tipecaml@gmail.com>
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

(define-module (gnu packages openstack)
  #:use-module (gnu packages python)
  #:use-module (gnu packages version-control)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module ((guix licenses)
                #:select (asl2.0))
  #:use-module (guix packages))

(define-public python-pbr
  (package
    (name "python-pbr")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://pypi.python.org/packages/source/p/pbr/pbr-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "1l2mls8wnwpkqj6hxsphq7xibbbsf40gg37wc30nj4r600zgqhqm"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ;; Most tests seem to use the Internet.
    (propagated-inputs
      `(("python-testrepository" ,python-testrepository)
        ("git" ,git))) ;; pbr actually uses the "git" binary.
    (inputs
      `(("python-fixtures" ,python-fixtures)
        ("python-mimeparse" ,python-mimeparse)
        ("python-mock" ,python-mock)
        ("python-setuptools" ,python-setuptools)
        ("python-six" ,python-six)
        ("python-sphinx" ,python-sphinx)
        ("python-testrepository" ,python-testrepository)
        ("python-testresources" ,python-testresources)
        ("python-testscenarios" ,python-testscenarios)
        ("python-testtools" ,python-testtools)
        ("python-virtualenv" ,python-virtualenv)))
    (home-page "https://launchpad.net/pbr")
    (synopsis "Change the default behavior of Python’s setuptools")
    (description
      "Python Build Reasonableness (PBR) is a library that injects some useful
and sensible default behaviors into your setuptools run.")
    (license asl2.0)))

(define-public python2-pbr
  (package-with-python2 python-pbr))
