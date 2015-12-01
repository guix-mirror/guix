;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2015 Efraim Flashner <efraim@flashner.co.il>
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
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "debtcollector" version))
        (sha256
          (base32
           "0g4dfskaiy47rhsh4gh66l5vmdsrgq0qk68pl3ix1cj3ffvfndzv"))))
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

(define-public python-hacking
  (package
    (name "python-hacking")
    (version "0.10.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hacking" version))
       (sha256
        (base32
         "1a310k3dv04jg7zvmk37h2ql7y9kf4hvdxb74bjlwdxgmy6h4wap"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-flake8-2.2.4" ,python-flake8-2.2.4)
        ("python-mccabe-0.2.1" ,python-mccabe-0.2.1)
        ("python-pbr" ,python-pbr)
        ("python-pep8-1.5.7" ,python-pep8-1.5.7)
        ("python-pyflakes-0.8.1" ,python-pyflakes-0.8.1)
        ("python-six" ,python-six)))
    (inputs
      `(("python-setuptools" ,python-setuptools)
        ;; Tests
        ("python-testscenarios" ,python-testscenarios)))
    (home-page "http://github.com/openstack-dev/hacking")
    (synopsis "OpenStack hacking guideline enforcement")
    (description
      "Python-hacking is a set of flake8 plugins that test and enforce the
@uref{http://docs.openstack.org/developer/hacking/, OpenStack style
guidelines}.")
    (license asl2.0)))

(define-public python2-hacking
  (package-with-python2 python-hacking))

(define-public python-mox3
  (package
    (name "python-mox3")
    (version "0.12.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "mox3" version))
        (sha256
          (base32
           "1pwz98q098cb8xxf8yryq21nvklc7hla880bsrq4y3j6bprw3iaj"))))
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
    (version "1.12.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "os-client-config" version))
        (sha256
          (base32
           "1vjn7667pswnmpqv6ngwyqm2xn46w90hi5b4pv2grwfz751cn1lf"))))
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

(define-public python-os-testr
  (package
    (name "python-os-testr")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "os-testr" version))
       (sha256
        (base32
         "0474z0mxb7y3vfk4s097wf1mzji5d135vh27cvlh9q17rq3x9r3w"))))
    (build-system python-build-system)
    (arguments
     ;; os-testr uses itself to run the tests. It seems like pbr writes the
     ;; exectuable in the virtualenv when using tox. Not sure how to do this
     ;; when building the package. Skip the tests for now.
     `(#:tests? #f))
    (propagated-inputs
     `(("python-pbr" ,python-pbr)
       ("python-subunit" ,python-subunit)
       ("python-testtools" ,python-testtools)))
    (inputs
      `(("python-babel" ,python-babel)
        ("python-setuptools" ,python-setuptools)))
    (home-page "http://www.openstack.org/")
    (synopsis "Testr wrapper to provide functionality for OpenStack projects")
    (description
      "Os-testr provides developers with a testr wrapper and an output filter
  for subunit.")
    (license asl2.0)))

(define-public python2-os-testr
  (package-with-python2 python-os-testr))

(define-public python-pbr
  (package
    (name "python-pbr")
    (version "1.8.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://pypi.python.org/packages/source/p/pbr/pbr-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "0jcny36cf3s8ar5r4a575npz080hndnrfs4np1fqhv0ym4k7c4p2"))))
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
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "requests-mock" version))
       (sha256
        (base32
         "0s6mrpiv2w0km39qvl1pq2d56xblnm57p369qdp5j1a55ncica7f"))))
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
    (version "1.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "stevedore" version))
       (sha256
         (base32
          "01pcrdqsb6ca7hmqwm11b3baj6ml8yz9pxawrgvxb3j9824906fc"))))
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

(define-public python-tempest-lib
  (package
    (name "python-tempest-lib")
    (version "0.11.0")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "tempest-lib" version))
      (sha256
       (base32
        "1q4wpqcg0yv99mr5gc43wsfirlqdjz90npyghy3mn5f6lby2yikg"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before
          'check 'pre-check
          (lambda _
            (substitute* "tempest_lib/tests/cli/test_execute.py"
              (("/bin/ls") (which "ls"))))))))
    (propagated-inputs
      `(("python-fixtures" ,python-fixtures)
        ("python-httplib2" ,python-httplib2)
        ("python-iso8601" ,python-iso8601)
        ("python-jsonschema" ,python-jsonschema)
        ("python-oslo.log" ,python-oslo.log)
        ("python-paramiko" ,python-paramiko)
        ("python-pbr" ,python-pbr)
        ("python-six" ,python-six)))
    (inputs
      `(("python-babel" ,python-babel)
        ("python-mock" ,python-mock)
        ("python-os-testr" ,python-os-testr)
        ("python-oslotest" ,python-oslotest)
        ("python-setuptools" ,python-setuptools)))
    (home-page "http://www.openstack.org/")
    (synopsis "OpenStack functional testing library")
    (description
      "Tempest-lib is a functional testing library for OpenStack.  It provides
common features used in Tempest.")
    (license asl2.0)))

(define-public python2-tempest-lib
  (package-with-python2 python-tempest-lib))

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
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "oslo.context" version))
       (sha256
        (base32
         "0kvha0rs9295njyl2z6n6zm5dapi5mrl5zwjm0m6ldqrvccyf8c3"))))
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
    (version "3.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "oslo.i18n" version))
        (sha256
          (base32
           "0bpb1c20sm8my650gl824nzaip83bfn8hr91s65k5ncmyh8hb6pl"))))
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

(define-public python-oslo.log
  (package
  (name "python-oslo.log")
  (version "1.6.0")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "https://pypi.python.org/packages/source/o/oslo.log/oslo.log-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "1fhy6yvbd565nv4x4i3ppyrlbmz3yy9d0xsvw5nkqsa7g43nmf8z"))))
  (build-system python-build-system)
  (propagated-inputs
   `(("python-debtcollector" ,python-debtcollector)
     ("python-oslo.config" ,python-oslo.config)
     ("python-oslo.context" ,python-oslo.context)
     ("python-oslo.i18n" ,python-oslo.i18n)
     ("python-oslo.utils" ,python-oslo.utils)
     ("python-oslo.serialization" ,python-oslo.serialization)
     ("python-six" ,python-six)))
  (inputs
    `(("python-babel" ,python-babel)
      ("python-iso8601" ,python-iso8601)
      ("python-mock" ,python-mock)
      ("python-oslotest" ,python-oslotest)
      ("python-pbr" ,python-pbr)
      ("python-setuptools" ,python-setuptools)))
  (home-page "http://launchpad.net/oslo")
  (synopsis "Python logging library of the Oslo project")
  (description
    "The oslo.log (logging) configuration library provides standardized
configuration for all OpenStack projects.  It also provides custom formatters,
handlers and support for context specific logging (like resource id’s etc).")
  (license asl2.0)))

(define-public python2-oslo.log
  (package-with-python2 python-oslo.log))

(define-public python-oslo.serialization
  (package
    (name "python-oslo.serialization")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "oslo.serialization" version))
       (sha256
        (base32
         "1hnkc69sa4r1qhx6hdwlrk2ng7wypgwr063iq5r815a0bv0qr1ad"))))
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

(define-public python-oslosphinx
  (package
    (name "python-oslosphinx")
    (version "3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "oslosphinx" version))
       (sha256
        (base32
         "0zcshdc9s1f7hnvg0fm2ps5rak3dpnm8kqg4i21lknhmsvb7p5cb"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-requests" ,python-requests)))
    (inputs
      `(("python-pbr" ,python-pbr)
        ("python-docutils" ,python-docutils)
        ("python-hacking" ,python-hacking)
        ("python-setuptools" ,python-setuptools)
        ("python-sphinx" ,python-sphinx)))
    (home-page "http://www.openstack.org/")
    (synopsis "OpenStack sphinx extensions and theme")
    (description
      "This package provides themes and extensions for Sphinx documentation
from the OpenStack project.")
    (license asl2.0)))

(define-public python2-oslosphinx
  (package-with-python2 python-oslosphinx))

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
        ("python-mox3" ,python-mox3)
        ("python-six" ,python-six)))
    (inputs
      `(("python-pbr" ,python-pbr)
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
    (version "3.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "oslo.utils" version))
        (sha256
          (base32
           "1c4jrbvfs4hs37fics8frqlyhmsv7v92ncv2cpbm0av9x0ic6pnj"))
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
