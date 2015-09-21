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

(define-public python-bandit
  (package
    (name "python-bandit")
    (version "0.13.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/b/bandit/bandit-"
             version ".tar.gz"))
       (sha256
        (base32
         "03g3cflvrc99ncjd611iy5nnnscsc2vgnrx4mjaqyx8glbfw8y7g"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-appdirs" ,python-appdirs)
        ("python-pyyaml" ,python-pyyaml)
        ("python-six" ,python-six)
        ("python-stevedore" ,python-stevedore)))
    (inputs
      `(("python-pbr" ,python-pbr)
        ("python-setuptools" ,python-setuptools)
        ;; Tests
        ("python-fixtures" ,python-fixtures)
        ("python-mock" ,python-mock)
        ("python-testrepository" ,python-testrepository)
        ("python-testscenarios" ,python-testscenarios)
        ("python-testtools" ,python-testtools)))
    (home-page "https://wiki.openstack.org/wiki/Security/Projects/Bandit")
    (synopsis "Security oriented static analyser for python code.")
    (description
      "Bandit is a tool designed to find common security issues in Python code.
To do this Bandit processes each file, builds an AST from it, and runs
appropriate plugins against the AST nodes.  Once Bandit has finished scanning
all the files it generates a report.")
    (license asl2.0)))

(define-public python2-bandit
  (package-with-python2 python-bandit))

(define-public python-debtcollector
  (package
    (name "python-debtcollector")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://pypi.python.org/packages/source/d/debtcollector/"
               "debtcollector-" version ".tar.gz"))
        (sha256
          (base32
            "0amlcg5f98lk2mfzdg44slh1nsi2y4ds123g5d57376fjk2b3njd"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-six" ,python-six)
       ("python-wrapt" ,python-wrapt)))
    (inputs
      `(("python-babel" ,python-babel)
        ("python-pbr" ,python-pbr)
        ("python-setuptools" ,python-setuptools)
        ;; Tests.
        ("python-oslotest" ,python-oslotest)))
    (home-page "http://www.openstack.org/")
    (synopsis
      "Find deprecated patterns and strategies in Python code")
    (description
      "This package provides a collection of Python deprecation patterns and
strategies that help you collect your technical debt in a non-destructive
manner.")
    (license asl2.0)))

(define-public python2-debtcollector
  (package-with-python2 python-debtcollector))

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

(define-public python-requests-mock
  (package
    (name "python-requests-mock")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/r/requests-mock/"
             "requests-mock-" version ".tar.gz"))
       (sha256
        (base32
         "0gmd88c224y53b1ai8cfsrcxm9kw3gdqzysclmnaqspg7zjhxwd1"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-requests" ,python-requests)
        ("python-six" ,python-six)))
    (inputs
      `(("python-mock" ,python-mock)
        ("python-pbr" ,python-pbr)
        ("python-setuptools" ,python-setuptools)))
    (home-page "https://requests-mock.readthedocs.org/")
    (synopsis "Mock out responses from the requests package")
    (description
      "This module provides a building block to stub out the HTTP requests
portions of your testing code.")
    (license asl2.0)))

(define-public python2-requests-mock
  (package-with-python2 python-requests-mock))

(define-public python-stevedore
  (package
    (name "python-stevedore")
    (version "1.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/s/stevedore/stevedore-"
             version
             ".tar.gz"))
       (sha256
         (base32
          "149pjc0c3z6khjisn4yil3f94qjnzwafz093wc8rrzbw828qdkv8"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-six" ,python-six)))
    (inputs
      `(("python-pbr" ,python-pbr)
        ("python-setuptools" ,python-setuptools)
        ;; Tests
        ("python-docutils" ,python-docutils)
        ("python-mock" ,python-mock)
        ("python-oslotest" ,python-oslotest)
        ("python-sphinx" ,python-sphinx)))
    (home-page "https://github.com/dreamhost/stevedore")
    (synopsis "Manage dynamic plugins for Python applications")
    (description
      "Python makes loading code dynamically easy, allowing you to configure
and extend your application by discovering and loading extensions (“plugins”)
at runtime.  Many applications implement their own library for doing this,
using __import__ or importlib.  stevedore avoids creating yet another extension
mechanism by building on top of setuptools entry points.  The code for managing
entry points tends to be repetitive, though, so stevedore provides manager
classes for implementing common patterns for using dynamically loaded
extensions.")
    (license asl2.0)))

(define-public python2-stevedore
  (package-with-python2 python-stevedore))

;; Packages from the Oslo library
(define-public python-oslo.config
  (package
    (name "python-oslo.config")
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/o/oslo.config/oslo.config-"
             version
             ".tar.gz"))
       (sha256
         (base32
          "13r778jfb0fhna37c2pd1f2xipnsbd7zli7qhn96acrzymrwj5k1"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-netaddr" ,python-netaddr)
        ("python-six" ,python-six)
        ("python-stevedore" ,python-stevedore)))
    (inputs
      `(("python-pbr" ,python-pbr)
        ("python-setuptools" ,python-setuptools)
        ;; Tests
        ("python-oslo.i18n" ,python-oslo.i18n)
        ("python-mock" ,python-mock)
        ("python-oslotest" ,python-oslotest)
        ("python-testscenarios" ,python-testscenarios)))
    (home-page "https://launchpad.net/oslo")
    (synopsis "Oslo Configuration API")
    (description
      "The Oslo configuration API supports parsing command line arguments and
.ini style configuration files.")
    (license asl2.0)))

(define-public python2-oslo.config
  (package-with-python2 python-oslo.config))

(define-public python-oslo.context
  (package
    (name "python-oslo.context")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/o/oslo.context/"
             "oslo.context-" version ".tar.gz"))
       (sha256
        (base32
         "16wr9qrkc3lb94ssb14qid4liza66x316fvzjw0izg67h1a0fm86"))))
    (build-system python-build-system)
    (inputs
      `(("python-babel" ,python-babel)
        ("python-pbr" ,python-pbr)
        ("python-setuptools" ,python-setuptools)
        ;; Tests.
        ("python-oslotest" ,python-oslotest)))
    (home-page "http://launchpad.net/oslo")
    (synopsis "Oslo context library")
    (description
      "The Oslo context library has helpers to maintain useful information
about a request context.  The request context is usually populated in the WSGI
pipeline and used by various modules such as logging.")
    (license asl2.0)))

(define-public python2-oslo.context
  (package-with-python2 python-oslo.context))

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

(define-public python-oslo.serialization
  (package
    (name "python-oslo.serialization")
    (version "1.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/o/oslo.serialization/"
             "oslo.serialization-" version ".tar.gz"))
       (sha256
        (base32
         "00qaxg155s61ylh4fqc7m5fh0gijf33khhai9xvcsc9k106i3c9c"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-iso8601" ,python-iso8601)
        ("python-netaddr" ,python-netaddr)
        ("python-oslo.utils" ,python-oslo.utils)
        ("python-simplejson" ,python-simplejson)
        ("python-six" ,python-six)
        ("python-pytz" ,python-pytz)))
    (inputs
      `(("python-babel" ,python-babel)
        ("python-pbr" ,python-pbr)
        ("python-setuptools" ,python-setuptools)
        ;; Tests.
        ("python-mock" ,python-mock)
        ("python-oslo.i18n" ,python-oslo.i18n)
        ("python-oslotest" ,python-oslotest)))
    (home-page "http://launchpad.net/oslo")
    (synopsis "Oslo serialization library")
    (description
      "The oslo.serialization library provides support for representing objects
in transmittable and storable formats, such as JSON and MessagePack.")
    (license asl2.0)))

(define-public python2-oslo.serialization
  (package-with-python2 python-oslo.serialization))

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

(define-public python-oslo.utils
  (package
    (name "python-oslo.utils")
    (version "2.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://pypi.python.org/packages/source/o/oslo.utils/oslo.utils-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "11b073gblhzkxhi1j6sqk3apq2ll8xhi9h9g9kxzx9dycqdq0qp0"))
        (snippet
         '(begin
            ;; FIXME: setuptools fails to import this file during the test
            ;; phase.
            (delete-file "oslo_utils/tests/test_netutils.py")))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-debtcollector" ,python-debtcollector)
        ("python-oslo.i18n" ,python-oslo.i18n)
        ("python-iso8601" ,python-iso8601)
        ("python-monotonic" ,python-monotonic)
        ("python-netaddr" ,python-netaddr)
        ("python-netifaces" ,python-netifaces)
        ("python-pytz" ,python-pytz)
        ("python-six" ,python-six)))
    (inputs
      `(("python-babel" ,python-babel)
        ("python-pbr" ,python-pbr)
        ("python-setuptools" ,python-setuptools)
        ;; Tests.
        ("python-oslotest" ,python-oslotest)
        ("python-mock" ,python-mock)
        ("python-mox3" ,python-mox3)
        ("python-testscenarios" ,python-testscenarios)))
    (home-page "http://launchpad.net/oslo")
    (synopsis "Oslo utility library")
    (description
      "The @code{oslo.utils} library provides support for common utility type
functions, such as encoding, exception handling, string manipulation, and time
handling.")
    (license asl2.0)))

(define-public python2-oslo.utils
  (package-with-python2 python-oslo.utils))
