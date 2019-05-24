;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2015, 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017, 2019 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Marius Bakke <mbakke@fastmail.com>
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
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module ((guix licenses)
                #:select (asl2.0))
  #:use-module (guix packages)
  #:use-module (srfi srfi-1))

(define-public python-bandit
  (package
    (name "python-bandit")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "bandit" version))
       (sha256
        (base32
         "1m5bm42120zyazky4k0lp3d9r0jwhjmp6sb108xfr0vz952p15yb"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'check)
                  (add-after 'install 'check
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      ;; Tests require the 'bandit' executable in PATH.
                      ;; It's only built during install time.
                      (add-installed-pythonpath inputs outputs)
                      (setenv "PATH" (string-append (assoc-ref outputs "out")
                                                    "/bin:" (getenv "PATH")))
                      (invoke "python" "setup.py" "testr"))))))
    (propagated-inputs
      `(("python-gitpython" ,python-gitpython)
        ("python-pyyaml" ,python-pyyaml)
        ("python-six" ,python-six)
        ("python-stevedore" ,python-stevedore)))
    (native-inputs
      `(;; Tests.
        ("python-beautifulsoup4" ,python-beautifulsoup4)
        ("python-fixtures" ,python-fixtures)
        ("python-mock" ,python-mock)
        ("python-subunit" ,python-subunit)
        ("python-testrepository" ,python-testrepository)
        ("python-testscenarios" ,python-testscenarios)
        ("python-testtools" ,python-testtools)))
    (home-page "https://wiki.openstack.org/wiki/Security/Projects/Bandit")
    (synopsis "Security oriented static analyser for python code")
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
    (version "1.19.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "debtcollector" version))
        (sha256
          (base32
           "06c7vyn184y9f0lsrwaz13aq63hdz5fjrd191b8nifx6acsni42f"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pbr" ,python-pbr)
       ("python-six" ,python-six)
       ("python-wrapt" ,python-wrapt)))
    (native-inputs
     `(;; Tests.
       ("python-subunit" ,python-subunit)
       ("python-testrepository" ,python-testrepository)
       ("python-testtools" ,python-testtools)))
    (home-page "https://www.openstack.org/")
    (synopsis
     "Find deprecated patterns and strategies in Python code")
    (description
      "This package provides a collection of Python deprecation patterns and
strategies that help you collect your technical debt in a non-destructive
manner.")
    (properties `((python2-variant . ,(delay python2-debtcollector))))
    (license asl2.0)))

(define-public python2-debtcollector
  (let ((base (package-with-python2 (strip-python2-variant
                                     python-debtcollector))))
    (package
      (inherit base)
      (propagated-inputs
       `(("python2-funcsigs" ,python2-funcsigs)
         ,@(package-propagated-inputs base))))))

(define-public python-hacking
  (package
    (name "python-hacking")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hacking" version))
       (sha256
        (base32
         "0s9l99s64jsyvm28fa4hzllbdi21sb7jn4gzdf1pd5ckvy7p4b0k"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-flake8" ,python-flake8-2.5)
       ("python-mccabe-0.2.1" ,python-mccabe-0.2.1)
       ("python-pbr" ,python-pbr)
       ("python-pep8-1.5.7" ,python-pep8-1.5.7)
       ("python-pyflakes-0.8.1" ,python-pyflakes-0.8.1)
       ("python-six" ,python-six)))
    (native-inputs
     `( ;; Tests
       ("python-eventlet" ,python-eventlet)
       ("python-mock" ,python-mock)
       ("python-reno" ,python-reno)
       ("python-testrepository" ,python-testrepository)
       ("python-testscenarios" ,python-testscenarios)))
    (home-page "https://github.com/openstack-dev/hacking")
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
    (version "0.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "mox3" version))
        (patches (search-patches "python-mox3-python3.6-compat.patch"))
        (sha256
          (base32
           "0w58adwv7q9wzvmq9mlrk2asfk73myq9fpwy7mjkzsz3baa95zf5"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-fixtures" ,python-fixtures)
       ("python-pbr" ,python-pbr)))
    (native-inputs
      `(("python-openstackdocstheme" ,python-openstackdocstheme)
        ("python-sphinx" ,python-sphinx)
        ("python-subunit" ,python-subunit)
        ("python-testrepository" ,python-testrepository)
        ("python-testtools" ,python-testtools)))
    (home-page "https://www.openstack.org/")
    (synopsis "Mock object framework for Python")
    (description
      "Mox3 is an unofficial port of the @uref{https://code.google.com/p/pymox/,
Google mox framework} to Python 3.  It was meant to be as compatible
with mox as possible, but small enhancements have been made.")
    (license asl2.0)))

(define-public python2-mox3
  (package-with-python2 python-mox3))

(define-public python-openstackdocstheme
  (package
    (name "python-openstackdocstheme")
    (version "1.18.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "openstackdocstheme" version))
              (sha256
               (base32
                "1ki5204rjdqjvr8xr9w2qc1z6b6d2i5jas0i70xzkf9njlzjzv2r"))))
    (build-system python-build-system)
    (arguments
     ;; FIXME: Tests require an old version of python-hacking, which in
     ;; turn depends on mox3 which depends on this package.
     `(#:tests? #f))
    (propagated-inputs
     `(("python-dulwich" ,python-dulwich)
       ("python-pbr" ,python-pbr)))
    (native-inputs
     `(("python-sphinx" ,python-sphinx)))
    (home-page "https://docs.openstack.org/openstackdocstheme/latest/")
    (synopsis "OpenStack Docs Theme")
    (description
     "This package provides themes and extensions for Sphinx for publishing
to docs.openstack.org and developer.openstack.org.")
    (license asl2.0)))

(define-public python2-openstackdocstheme
  (package-with-python2 python-openstackdocstheme))

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
    (propagated-inputs
      `(("python-appdirs" ,python-appdirs)
        ("python-pyyaml" ,python-pyyaml)))
    (native-inputs
      `(("python-pbr" ,python-pbr)
        ("python-fixtures" ,python-fixtures)
        ("python-mimeparse" ,python-mimeparse)
        ("python-testrepository" ,python-testrepository)
        ("python-testscenarios" ,python-testscenarios)
        ("python-testtools" ,python-testtools)))
    (home-page "https://www.openstack.org/")
    (synopsis
      "OpenStack Client Configuration Library")
    (description
      "The OpenStack Client Configuration Library is a library for collecting
  client configuration for using an OpenStack cloud in a consistent and
  comprehensive manner.")
    (license asl2.0)))

(define-public python2-os-client-config
  (package-with-python2 python-os-client-config))

(define-public python-os-testr
  (package
    (name "python-os-testr")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "os-testr" version))
       (sha256
        (base32
         "0mknd9hlmxmihr755gjkxyjp180380jajq5i3zm34q7y7bi62lss"))))
    (build-system python-build-system)
    (arguments
     ;; os-testr uses itself to run the tests. It seems like pbr writes the
     ;; exectuable in the virtualenv when using tox. Not sure how to do this
     ;; when building the package. Skip the tests for now.
     `(#:tests? #f))
    (propagated-inputs
     `(("python-subunit" ,python-subunit)))
    (native-inputs
     `(("python-pbr" ,python-pbr)
       ("python-testtools" ,python-testtools)
       ("python-babel" ,python-babel)))
    (home-page "https://www.openstack.org/")
    (synopsis "Testr wrapper to provide functionality for OpenStack projects")
    (description
      "Os-testr provides developers with a testr wrapper and an output filter
  for subunit.")
    (license asl2.0)))

(define-public python2-os-testr
  (package-with-python2 python-os-testr))

(define-public python-stevedore
  (package
    (name "python-stevedore")
    (version "1.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "stevedore" version))
       (sha256
        (base32
         "02ynfgwma84g59834dmvzr39mcppy5s229zf1w23c0qngf753izi"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pbr" ,python-pbr)
       ("python-six" ,python-six)))
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-sphinx" ,python-sphinx)
       ("python-testrepository" ,python-testrepository)))
    (home-page "https://github.com/dreamhost/stevedore")
    (synopsis "Manage dynamic plugins for Python applications")
    (description
      "Python makes loading code dynamically easy, allowing you to configure
and extend your application by discovering and loading extensions (\"plugins\")
at runtime.  Many applications implement their own library for doing this,
using __import__ or importlib.  Stevedore avoids creating yet another extension
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
    (version "1.0.0")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "tempest-lib" version))
      (sha256
       (base32
        "1cpp2vwmawpd29hjsklsps181lq2ah91cl412qvpnz228nf9sqn5"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; FIXME: Requires oslo.log >= 1.14.0.
       #:phases
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
    (native-inputs
      `(("python-babel" ,python-babel)
        ("python-mock" ,python-mock)
        ("python-os-testr" ,python-os-testr)
        ("python-oslotest" ,python-oslotest)))
    (home-page "https://www.openstack.org/")
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
    (version "5.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "oslo.config" version))
       (sha256
         (base32
          "0ymf7jxbq29fifyvkwhfiys1qvljqfxdw8ajwzwaf3yiqidgpxqd"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-debtcollector" ,python-debtcollector)
       ("python-netaddr" ,python-netaddr)
       ("python-oslo.i18n" ,python-oslo.i18n)
       ("python-pbr" ,python-pbr)
       ("python-rfc3986" ,python-rfc3986)
       ("python-six" ,python-six)
       ("python-stevedore" ,python-stevedore)
       ("python-pyyaml" ,python-pyyaml)))
    (native-inputs
     `(("python-bandit" ,python-bandit)
       ("python-coverage" ,python-coverage)
       ("python-mock" ,python-mock)
       ("python-openstackdocstheme" ,python-openstackdocstheme)
       ("python-oslotest" ,python-oslotest)
       ("python-reno" ,python-reno)
       ("python-sphinx" ,python-sphinx)
       ("python-testrepository" ,python-testrepository)
       ("python-testscenarios" ,python-testscenarios)
       ("python-testtools" ,python-testtools)))
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
    (version "2.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "oslo.context" version))
       (sha256
        (base32
         "0iiq9rpwg6wrdqnhf3d8z8g0g7fjhs5zn6qw6igvxplz2c3rbvvx"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-debtcollector" ,python-debtcollector)
       ("python-pbr" ,python-pbr)))
    (native-inputs
     `(("python-fixtures" ,python-fixtures)
       ("python-hacking" ,python-hacking)
       ("python-oslotest" ,python-oslotest)))
    (home-page "https://launchpad.net/oslo")
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
    (version "3.20.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "oslo.i18n" version))
        (sha256
          (base32
           "0kjcdw4bk3mi4vqmqwhhq053kxbbbj05si6nwxd1pzx33z067ky3"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-babel" ,python-babel)
        ("python-six" ,python-six)))
    (native-inputs
      `(("python-pbr" ,python-pbr)
        ;; Tests
        ("python-mock" ,python-mock)
        ("python-mox3" ,python-mox3)
        ("python-oslotest" ,python-oslotest)
        ("python-testscenarios" ,python-testscenarios)))
    (home-page "https://launchpad.net/oslo")
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
  (version "3.36.0")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "oslo.log" version))
      (sha256
        (base32
          "0h7hplf1h8k24v75m3mq1jlrl74x5ynyr4hwgffsg5campxnza4x"))))
  (build-system python-build-system)
  (propagated-inputs
   `(("python-dateutil" ,python-dateutil)
     ("python-debtcollector" ,python-debtcollector)
     ("python-monotonic" ,python-monotonic)
     ("python-oslo.config" ,python-oslo.config)
     ("python-oslo.context" ,python-oslo.context)
     ("python-oslo.i18n" ,python-oslo.i18n)
     ("python-oslo.utils" ,python-oslo.utils)
     ("python-oslo.serialization" ,python-oslo.serialization)
     ("python-pbr" ,python-pbr)
     ("python-pyinotify" ,python-pyinotify)
     ("python-six" ,python-six)))
  (native-inputs
    `(("python-mock" ,python-mock)
      ("python-oslotest" ,python-oslotest)
      ("python-subunit" ,python-subunit)
      ("python-testrepository" ,python-testrepository)
      ("python-testtools" ,python-testtools)))
  (home-page "https://launchpad.net/oslo")
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
    (version "2.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "oslo.serialization" version))
       (sha256
        (base32
         "08bxkp98c617y58x630xq44iiffm7f0f3cwh6zbnlkgq0zgh7jk1"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-msgpack" ,python-msgpack)
        ("python-netaddr" ,python-netaddr)
        ("python-oslo.utils" ,python-oslo.utils)
        ("python-six" ,python-six)
        ("python-pytz" ,python-pytz)))
    (native-inputs
      `(("python-pbr" ,python-pbr)
        ;; Tests.
        ("python-mock" ,python-mock)
        ("python-oslo.i18n" ,python-oslo.i18n)
        ("python-oslotest" ,python-oslotest)))
    (home-page "https://launchpad.net/oslo")
    (synopsis "Oslo serialization library")
    (description
      "The oslo.serialization library provides support for representing objects
in transmittable and storable formats, such as JSON and MessagePack.")
    (properties `((python2-variant . ,(delay python2-oslo.serialization))))
    (license asl2.0)))

(define-public python2-oslo.serialization
  (let ((base (package-with-python2 (strip-python2-variant
                                     python-oslo.serialization))))
    (package
      (inherit base)
      (native-inputs
       `(("python2-ipaddress" ,python2-ipaddress)
         ,@(package-native-inputs base))))))

(define-public python-reno
  (package
    (name "python-reno")
    (version "2.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "reno" version))
        (sha256
          (base32 "0gwzi5dvacqx43smxl3rd1z33npn7gfhm50bvgmq90fib2q431wc"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'init-git
           (lambda _
             ;; reno expects a git repo
             (invoke "git" "init"))))))
    (propagated-inputs
      `(("python-dulwich" ,python-dulwich)
        ("python-pbr" ,python-pbr)
        ("python-pyyaml" ,python-pyyaml)
        ("python-six" ,python-six)))
    (native-inputs
      `(("python-testtools" ,python-testtools)
        ("python-testscenarios" ,python-testscenarios)
        ("python-testrepository" ,python-testrepository)
        ("python-mock" ,python-mock)
        ("python-docutils" ,python-docutils)
        ("python-sphinx" ,python-sphinx)
        ("gnupg" ,gnupg)
        ("git" ,git-minimal)))
    (home-page "http://docs.openstack.org/developer/reno/")
    (synopsis "Release notes manager")
    (description "Reno is a tool for storing release notes in a git repository
and building documentation from them.")
    (license asl2.0)))

(define-public python2-reno
  (package-with-python2 python-reno))

(define-public python-oslosphinx
  (package
    (name "python-oslosphinx")
    (version "4.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "oslosphinx" version))
       (sha256
        (base32
         "09mxqyabi68f3s3arvdhlhq0mn38vf74jbsfcg84151hcj6czhnl"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             ;; Note: Upstream tests would have also built the release notes.
             ;; That only would work if we were in a git checkout.
             ;; Therefore, we don't do it here.
             (invoke "python" "setup.py" "build_sphinx"))))))
    (propagated-inputs
      `(("python-requests" ,python-requests)))
    (native-inputs
      `(("python-pbr" ,python-pbr)
        ("python-docutils" ,python-docutils)
        ("python-hacking" ,python-hacking)
        ("python-sphinx" ,python-sphinx)))
    (home-page "https://www.openstack.org/")
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
    (version "3.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "oslotest" version))
        (sha256
          (base32
            "1pp8lq61d548cxcqi451czvrz5i5b3hyi2ry00wmngdgiswcqj1h"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-fixtures" ,python-fixtures)
        ("python-mock" ,python-mock)
        ("python-mox3" ,python-mox3)
        ("python-os-client-config" ,python-os-client-config)
        ("python-six" ,python-six)
        ("python-subunit" ,python-subunit)
        ("python-testrepository" ,python-testrepository)
        ("python-testtools" ,python-testtools)))
    (native-inputs
      `(("python-pbr" ,python-pbr)
        ("python-testscenarios" ,python-testscenarios)))
    (home-page "https://launchpad.net/oslo")
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
    (version "3.36.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "oslo.utils" version))
        (sha256
          (base32
           "1ipjcgg9z697wmibhcbg5lqpk5gafakdx4qkff3w255zr0mvw04r"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-debtcollector" ,python-debtcollector)
        ("python-oslo.i18n" ,python-oslo.i18n)
        ("python-iso8601" ,python-iso8601)
        ("python-monotonic" ,python-monotonic)
        ("python-netaddr" ,python-netaddr)
        ("python-netifaces" ,python-netifaces)
        ("python-pyparsing" ,python-pyparsing)
        ("python-pytz" ,python-pytz)
        ("python-six" ,python-six)))
    (native-inputs
      `(("python-pbr" ,python-pbr)
        ;; Tests.
        ("python-bandit" ,python-bandit)
        ("python-ddt" ,python-ddt)
        ("python-fixtures" ,python-fixtures)
        ("python-oslo.config" ,python-oslo.config)
        ("python-oslotest" ,python-oslotest)
        ("python-mock" ,python-mock)
        ("python-testrepository" ,python-testrepository)
        ("python-testscenarios" ,python-testscenarios)
        ("python-testtools" ,python-testtools)))
    (home-page "https://launchpad.net/oslo")
    (synopsis "Oslo utility library")
    (description
      "The @code{oslo.utils} library provides support for common utility type
functions, such as encoding, exception handling, string manipulation, and time
handling.")
    (license asl2.0)))

(define-public python2-oslo.utils
  (package-with-python2 python-oslo.utils))

(define-public python-keystoneclient
  (package
    (name "python-keystoneclient")
    (version "1.8.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "python-keystoneclient" version))
        (sha256
         (base32
          "1w4csvkah67rfpxylxnvs2s3594i0f9isy8pf4gnsqs5zirvjaa4"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; FIXME: Many tests are failing.
    (native-inputs
     `(("python-sphinx" ,python-sphinx)
       ;; and some packages for the tests
       ("openssl" ,openssl)
       ("python-coverage" ,python-coverage)
       ("python-discover" ,python-discover)
       ("python-fixtures" ,python-fixtures)
       ("python-hacking" ,python-hacking)
       ("python-keyring" ,python-keyring)
       ("python-lxml" ,python-lxml)
       ("python-mock" ,python-mock)
       ("python-mox3" ,python-mox3)
       ("python-oauthlib" ,python-oauthlib)
       ("python-oslosphinx" ,python-oslosphinx)
       ("python-oslotest" ,python-oslotest)
       ("python-pycrypto" ,python-pycrypto)
       ("python-requests-mock" ,python-requests-mock)
       ("python-temptest-lib" ,python-tempest-lib)
       ("python-testrepository" ,python-testrepository)
       ("python-testresources" ,python-testresources)
       ("python-testtools" ,python-testtools)
       ("python-webob" ,python-webob)))
    (propagated-inputs
     `(("python-babel" ,python-babel)
       ("python-debtcollector" ,python-debtcollector)
       ("python-iso8601" ,python-iso8601)
       ("python-netaddr" ,python-netaddr)
       ("python-oslo.config" ,python-oslo.config)
       ("python-oslo.i18n" ,python-oslo.i18n)
       ("python-oslo.serialization" ,python-oslo.serialization)
       ("python-oslo.utils" ,python-oslo.utils)
       ("python-pbr" ,python-pbr)
       ("python-prettytable" ,python-prettytable)
       ("python-requests" ,python-requests)
       ("python-six" ,python-six)
       ("python-stevedore" ,python-stevedore)))
    (home-page "https://www.openstack.org/")
    (synopsis "Client Library for OpenStack Identity")
    (description
     "Python-keystoneclient is the identity service used by OpenStack for
authentication (authN) and high-level authorization (authZ).  It currently
supports token-based authN with user/service authZ, and is scalable to support
OAuth, SAML, and OpenID in future versions.  Out of the box, Keystone uses
SQLite for its identity store database, with the option to connect to external
LDAP.")
    (license asl2.0)))

(define-public python2-keystoneclient
  (let ((keystoneclient (package-with-python2 python-keystoneclient)))
    (package (inherit keystoneclient)
      (propagated-inputs
       `(("python2-requests" ,python2-requests)
         ,@(alist-delete "python-requests"
                         (package-propagated-inputs keystoneclient))))
      (native-inputs
       `(("python2-oauthlib" ,python2-oauthlib)
         ("python2-oslosphinx" ,python2-oslosphinx)
         ("python2-requests-mock" ,python2-requests-mock)
         ("python2-tempest-lib" ,python2-tempest-lib)
         ,@(fold alist-delete (package-native-inputs keystoneclient)
            '("python-oauthlib" "python-oslosphinx" "python-requests-mock" "python-tempest-lib")))))))

(define-public python-swiftclient
  (package
    (name "python-swiftclient")
    (version "2.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "python-swiftclient" version))
        (sha256
         (base32
          "1j33l4z9vqh0scfncl4fxg01zr1hgqxhhai6gvcih1gccqm4nd7p"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pbr" ,python-pbr)
       ("python-sphinx" ,python-sphinx)
       ;; The folloing packages are needed for the tests.
       ("python-coverage" ,python-coverage)
       ("python-discover" ,python-discover)
       ("python-hacking" ,python-hacking)
       ("python-mock" ,python-mock)
       ("python-oslosphinx" ,python-oslosphinx)
       ("python-keystoneclient" ,python-keystoneclient)
       ("python-testrepository" ,python-testrepository)
       ("python-testtools" ,python-testtools)))
    (propagated-inputs
     `(("python-requests" ,python-requests)
       ("python-six" ,python-six)))
    (home-page "https://www.openstack.org/")
    (synopsis "OpenStack Object Storage API Client Library")
    (description
     "OpenStack Object Storage (code-named Swift) creates redundant, scalable
object storage using clusters of standardized servers to store petabytes of
accessible data.  It is not a file system or real-time data storage system, but
rather a long-term storage system for a more permanent type of static data that
can be retrieved, leveraged, and then updated if necessary.  Primary examples of
data that best fit this type of storage model are virtual machine images, photo
storage, email storage and backup archiving.  Having no central \"brain\" or
master point of control provides greater scalability, redundancy and
permanence.")
    (properties `((python2-variant . ,(delay python2-swiftclient))))
    (license asl2.0)))

(define-public python2-swiftclient
  (let ((swiftclient (package-with-python2
                      (strip-python2-variant python-swiftclient))))
    (package (inherit swiftclient)
      (propagated-inputs
       `(("python2-futures" ,python2-futures)
         ,@(package-propagated-inputs swiftclient))))))

(define-public python-git-review
  (package
    (name "python-git-review")
    (version "1.27.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "git-review" version))
       (sha256
        (base32
         "0xkllc8ql401sfqbjqf7i451mkgwgv0j4gysxdlyzqb27kfsyc3s"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f ; tests require a running Gerrit server
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (git (assoc-ref inputs "git"))
                    (openssh (assoc-ref inputs "openssh")))
               (wrap-program (string-append out "/bin/git-review")
                 `("PATH" ":" prefix
                   ,(map (lambda (dir)
                           (string-append dir "/bin"))
                         (list git openssh)))))
             #t)))))
    (native-inputs
     `(("python-pbr" ,python-pbr)))
    (propagated-inputs
     `(("python-requests" ,python-requests)))
    (inputs
     `(("git" ,git)
       ("openssh" ,openssh)))
    (home-page "https://docs.openstack.org/infra/git-review/")
    (synopsis "Command-line tool for Gerrit")
    (description
     "Git-review is a command-line tool that helps submitting Git branches to
Gerrit for review, or fetching existing ones.")
    (license asl2.0)))

(define-public python2-git-review
  (package-with-python2 python-git-review))
