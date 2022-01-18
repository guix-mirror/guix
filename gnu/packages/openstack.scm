;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2015, 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017, 2019, 2021 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2022 Tanguy Le Carrour <tanguy@bioneland.org>
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
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xml)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module ((guix licenses)
                #:select (asl2.0))
  #:use-module (guix packages)
  #:use-module (srfi srfi-1))

(define-public python-bandit
  (package
    (name "python-bandit")
    (version "1.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "bandit" version))
       (sha256
        (base32
         "0rb034c99pyhb4a60z7f2kz40cjydhm8m9v2blaal1rmhlam7rs1"))))
    (build-system python-build-system)
    (arguments
     ;; The tests are disabled to avoid a circular dependency with
     ;; python-stestr.
     `(#:tests? #f))
    (propagated-inputs
     (list python-gitpython python-pyyaml python-six python-stevedore))
    (native-inputs
     (list python-pbr))
    (home-page "https://github.com/PyCQA/bandit")
    (synopsis "Security oriented static analyser for python code")
    (description "Bandit is a tool designed to find common security issues in
Python code.  To do this Bandit processes each file, builds an AST from it,
and runs appropriate plugins against the AST nodes.  Once Bandit has finished
scanning all the files it generates a report.")
    (license asl2.0)))

(define-public python-cliff
  (package
    (name "python-cliff")
    (version "3.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cliff" version))
       (sha256
        (base32
         "0n8pzr0mnn9lq2mykds69ij2xrn0fsirh4ndmkx0mzydbx5niysv"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (native-inputs
     (list python-pbr))
    (propagated-inputs
     (list python-cmd2
           python-prettytable
           python-pyparsing
           python-pyyaml
           python-bandit
           python-stevedore))
    (home-page "https://opendev.org/openstack/cliff")
    (synopsis "Framework for building command line programs")
    (description "The @code{cliff} framework allows creating multi-level
commands such as those of @command{subversion} and @command{git}, where the
main program handles some basic argument parsing and then invokes a
sub-command to do the work.  It uses plugins to define sub-commands, output
formatters, and other extensions.")
    (license asl2.0)))

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
     (list python-pbr python-six python-wrapt))
    (native-inputs
     (list ;; Tests.
           python-subunit python-testrepository python-testtools))
    (home-page "https://www.openstack.org/")
    (synopsis
     "Find deprecated patterns and strategies in Python code")
    (description
      "This package provides a collection of Python deprecation patterns and
strategies that help you collect your technical debt in a non-destructive
manner.")
    (license asl2.0)))

(define-public python-hacking
  (package
    (name "python-hacking")
    (version "4.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hacking" version))
       (sha256
        (base32
         "0fg19rlcky3n1y1ri61xyjp7534yzf8r102z9dw3zqg93f4kj20m"))))
    (build-system python-build-system)
     (propagated-inputs
     (list python-flake8-3.8))
    (native-inputs
     (list ;; Tests
           python-coverage
           python-ddt
           python-dnspython
           python-fixtures
           python-eventlet
           python-mock
           python-monotonic
           python-subunit
           python-stestr
           python-testscenarios
           python-testtools))
    (home-page "https://github.com/openstack-dev/hacking")
    (synopsis "OpenStack hacking guideline enforcement")
    (description
     "Python-hacking is a set of flake8 plugins that test and enforce the
@uref{http://docs.openstack.org/developer/hacking/, OpenStack style
guidelines}.")
    (license asl2.0)))

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
     (list python-fixtures python-pbr))
    (native-inputs
      (list python-openstackdocstheme python-sphinx python-subunit
            python-testrepository python-testtools))
    (home-page "https://www.openstack.org/")
    (synopsis "Mock object framework for Python")
    (description
      "Mox3 is an unofficial port of the @uref{https://code.google.com/p/pymox/,
Google mox framework} to Python 3.  It was meant to be as compatible
with mox as possible, but small enhancements have been made.")
    (license asl2.0)))

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
     (list python-dulwich python-pbr))
    (native-inputs
     (list python-sphinx))
    (home-page "https://docs.openstack.org/openstackdocstheme/latest/")
    (synopsis "OpenStack Docs Theme")
    (description
     "This package provides themes and extensions for Sphinx for publishing
to docs.openstack.org and developer.openstack.org.")
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
    (propagated-inputs
      (list python-appdirs python-pyyaml))
    (native-inputs
      (list python-pbr
            python-fixtures
            python-mimeparse
            python-testrepository
            python-testscenarios
            python-testtools))
    (home-page "https://www.openstack.org/")
    (synopsis
      "OpenStack Client Configuration Library")
    (description
      "The OpenStack Client Configuration Library is a library for collecting
  client configuration for using an OpenStack cloud in a consistent and
  comprehensive manner.")
    (license asl2.0)))

(define-public python-os-testr
  (package
    (name "python-os-testr")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "os-testr" version))
       (sha256
        (base32
         "10xaqg3wxly13652hdvh9c69y4s12ird0ircffya3kvpl5pky0pz"))))
    (build-system python-build-system)
    (arguments
     ;; os-testr uses itself to run the tests. It seems like pbr writes the
     ;; exectuable in the virtualenv when using tox. Not sure how to do this
     ;; when building the package. Skip the tests for now.
     `(#:tests? #f))
    (propagated-inputs
     (list python-stestr))
    (native-inputs
     (list python-babel python-pbr python-testrepository python-testtools))
    (home-page "https://www.openstack.org/")
    (synopsis "Testr wrapper to provide functionality for OpenStack projects")
    (description
      "Os-testr provides developers with a testr wrapper and an output filter
  for subunit.")
    (license asl2.0)))

(define-public python-stevedore
  (package
    (name "python-stevedore")
    (version "3.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "stevedore" version))
       (sha256
        (base32
         "1w11lm293afzb73iq0ba9wnmr2rjwymnhr92km4a4xrs7a5qcigq"))))
    (build-system python-build-system)
    (arguments
     ;; The tests are disabled to avoid a circular dependency with
     ;; python-stestr.
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'use-pbr-3
           (lambda _
             (substitute* '("setup.py"
                            "requirements.txt")
               (("pbr!=2.1.0,>=2.0.0") "pbr>=3.0.0")))))))
    (propagated-inputs
     (list python-pbr))
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
      (list python-fixtures
            python-httplib2
            python-iso8601
            python-jsonschema
            python-oslo.log
            python-paramiko
            python-pbr
            python-six))
    (native-inputs
      (list python-babel python-mock python-os-testr python-oslotest))
    (home-page "https://www.openstack.org/")
    (synopsis "OpenStack functional testing library")
    (description
      "Tempest-lib is a functional testing library for OpenStack.  It provides
common features used in Tempest.")
    (license asl2.0)))


;;;
;;; Packages from the Oslo library
;;;

(define-public python-oslo.config
  (package
    (name "python-oslo.config")
    (version "8.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "oslo.config" version))
       (sha256
        (base32
         "0q3v4yicqls9zsfxkmh5mrgz9dailaz3ir25p458gj6dg3bldhx0"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))          ;XXX circular dependency on oslo.log
    (propagated-inputs
     (list python-debtcollector
           python-netaddr
           python-oslo.i18n
           python-rfc3986
           python-requests
           python-stevedore
           python-pyyaml))
    (home-page "https://launchpad.net/oslo")
    (synopsis "Oslo Configuration API")
    (description
     "The Oslo configuration API supports parsing command line arguments and
.ini style configuration files.")
    (license asl2.0)))

(define-public python-oslo.context
  (package
    (name "python-oslo.context")
    (version "3.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "oslo.context" version))
       (sha256
        (base32
         "1l2z186rkd9acrb2ygf53yrdc1lgf7cy1akbhm21kgkzind4p2r6"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'relax-requirements
                    (lambda _
                      (substitute* "test-requirements.txt"
                        (("hacking>=3.0.1,<3.1.0")
                         "hacking>=3.0.1"))
                      #t)))))
    (propagated-inputs
     (list python-debtcollector))
    (native-inputs
     (list python-bandit
           python-coverage
           python-fixtures
           python-hacking
           python-oslotest
           python-pbr
           python-stestr))
    (home-page "https://launchpad.net/oslo")
    (synopsis "Oslo context library")
    (description
      "The Oslo context library has helpers to maintain useful information
about a request context.  The request context is usually populated in the WSGI
pipeline and used by various modules such as logging.")
    (license asl2.0)))

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
    (arguments
     '(#:tests? #f))                 ;avoid circular dependency on oslo.config
    (propagated-inputs
     (list python-babel python-six))
    (native-inputs
     (list python-pbr))
    (home-page "https://launchpad.net/oslo")
    (synopsis "Oslo internationalization (i18n) library")
    (description
     "The oslo.i18n library contain utilities for working with
internationalization (i18n) features, especially translation for text strings
in an application or library.")
    (license asl2.0)))

(define-public python-oslo.log
  (package
  (name "python-oslo.log")
  (version "4.6.1")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "oslo.log" version))
      (sha256
        (base32
          "0dlnxjci9mpwhgfv19fy1z7xrdp8m95skrj5dr60all3pr7n22f6"))))
  (build-system python-build-system)
  (arguments
   '(#:phases (modify-phases %standard-phases
                (replace 'check
                  (lambda* (#:key tests? #:allow-other-keys)
                    (when tests? (invoke "stestr" "run")))))))
  (propagated-inputs
   (list python-dateutil
         python-debtcollector
         python-oslo.config
         python-oslo.context
         python-oslo.i18n
         python-oslo.utils
         python-oslo.serialization
         python-pbr
         python-pyinotify
         python-six))
  (native-inputs
    (list python-fixtures python-oslotest python-stestr python-testtools))
  (home-page "https://launchpad.net/oslo")
  (synopsis "Python logging library of the Oslo project")
  (description
    "The oslo.log (logging) configuration library provides standardized
configuration for all OpenStack projects.  It also provides custom formatters,
handlers and support for context specific logging (like resource id’s etc).")
  (license asl2.0)))

(define-public python-oslo.serialization
  (package
    (name "python-oslo.serialization")
    (version "4.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "oslo.serialization" version))
       (sha256
        (base32
         "10sdgvyb0d3lcmb8b4l5gs40bkfbai08kvsdwp658dxd2yqf21rh"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests? (invoke "stestr" "run")))))))
    (propagated-inputs
      (list python-msgpack python-oslo.utils python-pbr python-pytz))
    (native-inputs
     ;; For tests.
      (list python-netaddr python-oslo.i18n python-oslotest python-stestr))
    (home-page "https://launchpad.net/oslo")
    (synopsis "Oslo serialization library")
    (description
      "The oslo.serialization library provides support for representing objects
in transmittable and storable formats, such as JSON and MessagePack.")
    (license asl2.0)))

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
      (list python-dulwich python-pbr python-pyyaml python-six))
    (native-inputs
      `(("python-testtools" ,python-testtools)
        ("python-testscenarios" ,python-testscenarios)
        ("python-testrepository" ,python-testrepository)
        ("python-mock" ,python-mock)
        ("python-docutils" ,python-docutils)
        ("python-sphinx" ,python-sphinx)
        ("gnupg" ,gnupg)
        ("git" ,git-minimal)))
    (home-page "https://docs.openstack.org/reno/latest/")
    (synopsis "Release notes manager")
    (description "Reno is a tool for storing release notes in a git repository
and building documentation from them.")
    (license asl2.0)))

(define-public python-oslosphinx
  (package
    (name "python-oslosphinx")
    (version "4.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "oslosphinx" version))
       (sha256
        (base32
         "1xm41857vzrzjmnyi6bqirg4i5qa61v7wxcsdc4q1nzgr3ndgz5k"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'relax-requirements
           (lambda _
             (substitute* "test-requirements.txt"
               (("hacking!=0.13.0,<0.14,>=0.12.0")
                "hacking!=0.13.0,>=0.12.0"))
             #t)))))
    (propagated-inputs
     (list python-requests))
    (native-inputs
     (list python-hacking python-openstackdocstheme python-pbr
           python-reno python-sphinx))
    (home-page "https://www.openstack.org/")
    (synopsis "OpenStack sphinx extensions and theme")
    (description "This package provides themes and extensions for Sphinx
documentation from the OpenStack project.")
    (license asl2.0)))

(define-public python-oslotest
  (package
    (name "python-oslotest")
    (version "4.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "oslotest" version))
       (sha256
        (base32
         "0r50sz55m8ljv2vk1k7sp88iz1iqq4p9w6kb8hn8g8c50r9zdi5i"))))
    (build-system python-build-system)
    (arguments
     ;; The tests are disabled to avoid a circular dependency with oslo.config.
     `(#:tests? #f))
    (propagated-inputs
     (list python-fixtures python-six python-subunit python-testtools))
    (home-page "https://launchpad.net/oslo")
    (synopsis "Oslo test framework")
    (description "The Oslo Test framework provides common fixtures, support
for debugging, and better support for mocking results.")
    (license asl2.0)))

(define-public python-oslo.utils
  (package
    (name "python-oslo.utils")
    (version "4.12.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "oslo.utils" version))
        (sha256
          (base32
           "0kfgr6lr3r34nzmkvnyywr0x3lkwpwy35m1dj4rkk3ydqvi1xaip"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests? (invoke "stestr" "run")))))))
    (propagated-inputs
      (list python-debtcollector
            python-oslo.i18n
            python-iso8601
            python-netaddr
            python-netifaces
            python-pbr
            python-packaging-next
            python-pyparsing
            python-pytz))
    (native-inputs
     ;; For tests.
      (list python-ddt
            python-eventlet
            python-fixtures
            python-oslotest
            python-stestr
            python-testscenarios
            python-testtools))
    (home-page "https://launchpad.net/oslo")
    (synopsis "Oslo utility library")
    (description
      "The @code{oslo.utils} library provides support for common utility type
functions, such as encoding, exception handling, string manipulation, and time
handling.")
    (license asl2.0)))

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
     (list python-babel
           python-debtcollector
           python-iso8601
           python-netaddr
           python-oslo.config
           python-oslo.i18n
           python-oslo.serialization
           python-oslo.utils
           python-pbr
           python-prettytable
           python-requests
           python-six
           python-stevedore))
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
     (list python-pbr
           python-sphinx
           ;; The folloing packages are needed for the tests.
           python-coverage
           python-discover
           python-hacking
           python-mock
           python-oslosphinx
           python-keystoneclient
           python-testrepository
           python-testtools))
    (propagated-inputs
     (list python-requests python-six))
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
    (license asl2.0)))

(define-public python-git-review
  (package
    (name "python-git-review")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "git-review" version))
       (sha256
        (base32 "1mhywsbisyv028lsj2ksg4g5l8kyimpwxgwzqi08rymi8mb7fv1s"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f                     ; tests require a running Gerrit server
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
     (list python-pbr))
    (propagated-inputs
     (list python-requests))
    (inputs
     (list git openssh))
    (home-page "https://docs.openstack.org/infra/git-review/")
    (synopsis "Command-line tool for Gerrit")
    (description
     "Git-review is a command-line tool that helps submitting Git branches to
Gerrit for review, or fetching existing ones.")
    (license asl2.0)))
