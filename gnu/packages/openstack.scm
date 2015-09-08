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

(define-public python-mox3
  (package
    (name "python-mox3")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://pypi.python.org/packages/source/m/mox3/mox3-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "1dwj9lkifdqvrcympqa47bj55l0n0j9jhzv2gj03h0dpzg6mgfkj"))))
    (build-system python-build-system)
    (inputs
      `(("python-fixtures" ,python-fixtures)
        ("python-pbr" ,python-pbr)
        ("python-setuptools" ,python-setuptools)
        ("python-six" ,python-six)
        ("python-testtools" ,python-testtools)))
    (home-page "http://www.openstack.org/")
    (synopsis "Mock object framework for Python")
    (description
      "Mox3 is an unofficial port of the Google mox framework
(http://code.google.com/p/pymox/) to Python 3. It was meant to be as compatible
with mox as possible, but small enhancements have been made. The library was
tested on Python version 3.2, 2.7 and 2.6.")
    (license asl2.0)))

(define-public python-os-client-config
  (package
    (name "python-os-client-config")
    (version "1.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://pypi.python.org/packages/source/o/os-client-config/os-client-config-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "14png6ml3zbbilh8bihav24f8vig9lyijwynnjcvazdxxrzvwq9j"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ;; Circular dependency with python-oslotest
    (inputs
      `(("python-appdirs" ,python-appdirs)
        ("python-fixtures" ,python-fixtures)
        ("python-mimeparse" ,python-mimeparse)
        ("python-pbr" ,python-pbr)
        ("python-pyyaml" ,python-pyyaml)
        ("python-testrepository" ,python-testrepository)
        ("python-setuptools" ,python-setuptools)
        ("python-testscenarios" ,python-testscenarios)
        ("python-testtools" ,python-testtools)))
    (home-page "http://www.openstack.org/")
    (synopsis
      "OpenStack Client Configuration Library")
    (description
      "The OpenStack Client Configuration Library is a library for collecting
  client configuration for using an OpenStack cloud in a consistent and
  comprehensive manner.")
    (license asl2.0)))

(define-public python2-os-client-config
  (package-with-python2 python-os-client-config))

(define-public python2-mox3
  (package-with-python2 python-mox3))

(define-public python-pbr
  (package
    (name "python-pbr")
    (version "1.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://pypi.python.org/packages/source/p/pbr/pbr-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "1lg1klrczvzfan89y3bl9ykrknl3nb01vvai37fkww24apzyibjf"))))
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

;; Packages from the Oslo library
(define-public python-oslo.i18n
  (package
    (name "python-oslo.i18n")
    (version "2.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://pypi.python.org/packages/source/o/oslo.i18n/oslo.i18n-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "1kg72mqldlri3x0bhxai7j979czrd7mf8s3iflvvv0x9kn9ah4cw"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-babel" ,python-babel)
        ("python-six" ,python-six)))
    (inputs
      `(("python-pbr" ,python-pbr)
        ("python-setuptools" ,python-setuptools)
        ;; Tests
        ("python-mock" ,python-mock)
        ("python-mox3" ,python-mox3)
        ("python-oslotest" ,python-oslotest)
        ("python-testscenarios" ,python-testscenarios)))
    (home-page "http://launchpad.net/oslo")
    (synopsis "Oslo internationalization (i18n) library")
    (description
      "The oslo.i18n library contain utilities for working with
internationalization (i18n) features, especially translation for text strings
in an application or library.")
    (license asl2.0)))

(define-public python2-oslo.i18n
  (package-with-python2 python-oslo.i18n))

(define-public python-oslotest
  (package
    (name "python-oslotest")
    (version "1.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://pypi.python.org/packages/source/o/oslotest/oslotest-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "0l3ny48ddz5xbf0v4r0jv1yhbdzinc2vy0lybhdkmx3xy0b886fs"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-fixtures" ,python-fixtures)
        ("python-mock" ,python-mock)
        ("python-six" ,python-six)))
    (inputs
      `(("python-pbr" ,python-pbr)
        ("python-mox3" ,python-mox3)
        ("python-os-client-config" ,python-os-client-config)
        ("python-setuptools" ,python-setuptools)
        ("python-subunit" ,python-subunit)
        ("python-testrepository" ,python-testrepository)
        ("python-testscenarios" ,python-testscenarios)
        ("python-testtools" ,python-testtools)))
    (home-page "http://launchpad.net/oslo")
    (synopsis "Oslo test framework")
    (description
      "The Oslo Test framework provides common fixtures, support for debugging,
and better support for mocking results.")
    (license asl2.0)))

(define-public python2-oslotest
  (package-with-python2 python-oslotest))
