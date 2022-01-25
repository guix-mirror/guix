;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Eric Dvorsak <eric@dvorsak.fr>
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2016, 2017 Danny Milosavljevic <dannym+a@scratchpost.org>
;;; Copyright © 2013, 2014, 2015, 2016, 2020 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016, 2017, 2019-2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2021 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016, 2017, 2020 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2014, 2017, 2021 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2015, 2016, 2017, 2019 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2019 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2016–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2015, 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2015, 2016 Christine Lemmer-Webber <cwebber@dustycloud.org>
;;; Copyright © 2017 Adriano Peluso <catonano@gmail.com>
;;; Copyright © 2016 Dylan Jeffers <sapientech@sapientech@openmailbox.org>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2017 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2015, 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2017 Mark Meyer <mark@ofosos.org>
;;; Copyright © 2018 Tomáš Čech <sleep_walker@gnu.org>
;;; Copyright © 2018, 2019, 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2018 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2018, 2020, 2021, 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2019 Vagrant Cascadian <vagrant@debian.org>
;;; Copyright © 2019 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2019 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2019, 2020 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Evan Straw <evan.straw99@gmail.com>
;;; Copyright © 2020 Alexandros Theodotou <alex@zrythm.org>
;;; Copyright © 2020 Holger Peters <holger.peters@posteo.de>
;;; Copyright © 2020 Noisytoot <noisytoot@gmail.com>
;;; Copyright © 2020 Edouard Klein <edk@beaver-labs.com>
;;; Copyright © 2020, 2021, 2022 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2020 Konrad Hinsen <konrad.hinsen@fastmail.net>
;;; Copyright © 2020, 2022 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2021 Ekaitz Zarraga <ekaitz@elenq.tech>
;;; Copyright © 2021 Greg Hogan <code@greghogan.com>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2021 Pradana Aumars <paumars@courrier.dev>
;;; Copyright © 2021 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2021 jgart <jgart@dismail.de>
;;; Copyright © 2021 Alice Brenon <alice.brenon@ens-lyon.fr>
;;; Copyright © 2022 John Kehayias <john.kehayias@protonmail.com>
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

(define-module (gnu packages python-web)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages django)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages node)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages time)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (srfi srfi-1))

(define-public python-prawcore
  (package
    (name "python-prawcore")
    (version "2.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "prawcore" version))
       (sha256
        (base32 "0vgmhjddqxnz5vy70dyqvakak51fg1nk6j3xavkc83d8nzacrwfs"))))
    (build-system python-build-system)
    (native-inputs
     (list python-betamax
           python-betamax-matchers
           python-betamax-serializers
           python-mock
           python-pytest
           python-testfixtures))
    (propagated-inputs
     (list python-requests))
    (synopsis "Core component of PRAW")
    (description "PRAWcore is a low-level communication layer used by PRAW.")
    (home-page "https://praw.readthedocs.io/en/latest/")
    (license license:bsd-2)))

(define-public python-praw
  (package
    (name "python-praw")
    (version "7.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "praw" version))
       (sha256
        (base32 "1nqcwz8r8xp4rfpy2i11x2fjga8fmmf6zw94xjk1h1yxgn1gq6zr"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-failing-tests
           (lambda _
             (with-directory-excursion "tests"
               ;; Integration tests depend on files that are not included.
               (for-each delete-file-recursively
                         '("integration/models" "unit/models"))
               ;; The configuration file does not seem to exist.
               (delete-file "unit/test_config.py"))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest" "-k"
                       ;; These tests depend on test files that don't exist.
                       (string-append "not test_bad_request_without_json_text_plain_response"
                                      " and not test_bad_request_without_json_text_html_response"))))))))
    (native-inputs
     (list python-betamax python-betamax-matchers python-pytest))
    (propagated-inputs
     (list python-prawcore python-update-checker python-websocket-client))
    (synopsis "Python Reddit API Wrapper")
    (description "PRAW is a Python package that allows for simple access to
Reddit’s API.  It aims to be easy to use and internally follows all of Reddit’s
API rules.")
    (home-page "https://praw.readthedocs.io/en/latest/")
    (license license:bsd-2)))

(define-public python-frozenlist
  (package
    (name "python-frozenlist")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "frozenlist" version))
       (sha256
        (base32 "1pkr23by7pk9lsmsh0wiqirpkq3f1f08b0615nbzysn51bk1n838"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest" "tests")))))))
    (native-inputs (list python-pytest))
    (home-page "https://github.com/aio-libs/frozenlist")
    (synopsis "List-like data structure for Python")
    (description "@code{frozenlist.FrozenList} is a list-like structure which
implements @code{collections.abc.MutableSequence}.  It can be made immutable
by calling @code{FrozenList.freeze}.")
    (license license:asl2.0)))

(define-public python-aiosignal
  (package
    (name "python-aiosignal")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "aiosignal" version))
       (sha256
        (base32 "1wkxbdgw07ay8yzx3pg1jcm46p3d21rfb5g4k17ysz3vdkdngvbq"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest" "tests")))))))
    (propagated-inputs (list python-frozenlist))
    (native-inputs (list python-pytest python-pytest-asyncio))
    (home-page "https://github.com/aio-libs/aiosignal")
    (synopsis "Callback manager for Python @code{asyncio} projects")
    (description "This Python module provides @code{Signal}, an abstraction to
register asynchronous callbacks.  The @code{Signal} abstraction can be used
for adding, removing and dropping callbacks.")
    (license license:asl2.0)))

(define-public python-aiohttp
  (package
    (name "python-aiohttp")
    (version "3.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "aiohttp" version))
       (sha256
        (base32 "0y3m1dzl4h6frg8vys0fc3m83ijd1plfpihv3kvmxqadlphp2m7w"))
       ;; TODO: Unbundle the llhttp sources.
       ;; (modules '((guix build utils)))
       ;; (snippet
       ;;  '((delete-file-recursively "vendor")))
       ))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-tests
           (lambda _
             ;; disable brotli tests, because we’re not providing that optional library
             (substitute* "tests/test_http_parser.py"
               (("    async def test_feed_eof_no_err_brotli")
                "    @pytest.mark.xfail\n    async def test_feed_eof_no_err_brotli"))
             ;; make sure the timestamp of this file is > 1990, because a few
             ;; tests like test_static_file_if_modified_since_past_date depend on it
             (let ((late-90s (* 60 60 24 365 30)))
               (utime "tests/data.unknown_mime_type" late-90s late-90s))

             ;; Disable test that attempts to access httpbin.org.
             (substitute* "tests/test_formdata.py"
               (("async def test_mark_formdata_as_processed.*" all)
                (string-append "@pytest.mark.xfail\n" all)))

             ;; Don't test the aiohttp pytest plugin to avoid a dependency loop.
             (delete-file "tests/test_pytest_plugin.py")))
         (add-before 'build 'cythonize
           (lambda _
             ;; Adapted from the Makefile.
             (with-directory-excursion "aiohttp"
               (for-each
                (lambda (file)
                  (invoke "cython" "-3"
                          file "-I" "."))
                (find-files "." "_.*\\.pyx$")))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; This tests requires the 'proxy.py' module, not yet
               ;; packaged.
               (delete-file "tests/test_proxy_functional.py")
               (invoke "pytest" "-vv"
                       ;; Disable loading the aiohttp coverage plugin
                       ;; to avoid a circular dependency (code coverage
                       ;; is not very interesting to us anyway).
                       "-o" "addopts=''" "--ignore=aiohttp"
                       "-n" (number->string (parallel-job-count))
                       "-k" (string-append
                             ;; This test probably requires to be run with the
                             ;; library loaded from the the build directory.
                             "not test_c_parser_loaded and "
                             ;; Disable the following tests as they require
                             ;; networking.
                             "not TestDeflateBuffer and "
                             "not test_client_session_timeout_zero and "
                             "not test_empty_body and "
                             "not test_mark_formdata_as_processed[pyloop] and "
                             "not test_receive_runtime_err[pyloop]"))))))))
    (propagated-inputs
     (list python-aiodns
           python-aiosignal
           python-attrs
           python-async-timeout
           python-charset-normalizer
           python-frozenlist
           python-idna-ssl
           python-multidict
           python-typing-extensions
           python-yarl))
    (native-inputs
     (list gunicorn-bootstrap
           python-async-generator
           python-cython
           python-freezegun
           python-pytest
           python-pytest-mock
           python-pytest-xdist
           python-re-assert))
    (home-page "https://github.com/aio-libs/aiohttp/")
    (synopsis "Async HTTP client/server framework (asyncio)")
    (description "@code{aiohttp} is an asynchronous HTTP client/server
framework.

Its main features are:
@itemize
@item Supports both client and server side of HTTP protocol.
@item Supports both client and server Web-Sockets out-of-the-box without the
Callback Hell.
@item Web-server has middlewares and pluggable routing.
@end itemize")
    (license license:asl2.0)))

(define-public python-aiohttp-socks
  (package
    (name "python-aiohttp-socks")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "aiohttp_socks" version))
       (sha256
        (base32
         "04w010bvi719ifpc3sshav95k10hf9nq8czn9yglkj206yxcypdr"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-aiohttp python-attrs python-socks))
    (home-page "https://github.com/romis2012/aiohttp-socks")
    (synopsis "SOCKS proxy connector for aiohttp")
    (description "This package provides a SOCKS proxy connector for
aiohttp.  It supports SOCKS4(a) and SOCKS5.")
    (license license:asl2.0)))

(define-public python-aiodns
  (package
    (name "python-aiodns")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "aiodns" version))
       (sha256
        (base32
         "1snr5paql8dgvc676n8xq460wypjsb1xj53cf3px1s4wczf7lryq"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-pycares))
    (arguments
     `(#:tests? #f))                    ;tests require internet access
    (home-page "https://github.com/saghul/aiodns")
    (synopsis "Simple DNS resolver for asyncio")
    (description "@code{aiodns} provides a simple way for doing
asynchronous DNS resolutions with a synchronous looking interface by
using @url{https://github.com/saghul/pycares,pycares}.")
    (license license:expat)))

(define-public python-aiorpcx
  (package
    (name "python-aiorpcx")
    (version "0.22.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "aiorpcX" version))
       (sha256
        (base32
         "0lx54bcinp44fmr8q4bbffsqbkg8kdcwykf9i5jj0bj3sfzgf9k0"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-attrs))
    (home-page "https://github.com/kyuupichan/aiorpcX")
    (synopsis "Generic asyncio RPC implementation")
    (description
     "The aiorpcX library is a generic asyncio implementation of RPC suitable
for an application that is a client, server or both.

The package includes a module with full coverage of JSON RPC versions 1.0 and
2.0, JSON RPC protocol auto-detection, and arbitrary message framing.  It also
comes with a SOCKS proxy client.")
    (license (list license:expat license:bsd-2))))

(define-public python-aiorpcx-0.18
  (package
    (inherit python-aiorpcx)
    (version "0.18.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "aiorpcX" version))
       (sha256
        (base32
         "1rswrspv27x33xa5bnhrkjqzhv0sknv5kd7pl1vidw9d2z4rx2l0"))))))

(define-public python-asgiref
  (package
    (name "python-asgiref")
    (version "3.4.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "asgiref" version))
              (sha256
               (base32 "1saqgpgbdvb8awzm0f0640j0im55hkrfzvcw683cgqw4ni3apwaf"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest" "-vv")))))))
    (native-inputs
     (list python-pytest python-pytest-asyncio))
    (home-page "https://github.com/django/asgiref/")
    (synopsis "ASGI specs, helper code, and adapters")
    (description
     "ASGI is a standard for Python asynchronous web apps and servers to
communicate with each other, and positioned as an asynchronous successor to
WSGI.  This package includes libraries for implementing ASGI servers.")
    (license license:bsd-3)))

(define-public python-css-html-js-minify
  (package
    (name "python-css-html-js-minify")
    (version "2.5.5")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "css-html-js-minify" version ".zip"))
              (sha256
               (base32
                "0v3l2dqdk2y4r6ax259gs4ij1zzm9yxg6491s6254vs9w3vi37sa"))))
    (build-system python-build-system)
    ;; XXX: The git repository has no tags, and the PyPI releases do not
    ;; contain tests.
    (arguments '(#:tests? #f))
    (native-inputs (list unzip))
    (home-page "https://github.com/juancarlospaco/css-html-js-minify")
    (synopsis "CSS/HTML/JS minifier")
    (description
     "This package provides a single-file minifier for CSS, HTML, and JavaScript.")
    ;; XXX: The README just says "GNU GPL and GNU LGPL and MIT".  From
    ;; <https://github.com/juancarlospaco/css-html-js-minify/issues/9> it
    ;; looks like the user can choose a license.
    (license (list license:gpl3+ license:lgpl3+ license:expat))))

(define-public python-aws-sam-translator
  (package
    (name "python-aws-sam-translator")
    (version "1.40.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "aws-sam-translator" version))
              (sha256
               (base32
                "1hq5ggbzcq4k3ks439hki493w4sasgaxns6j5x57xsj822acalmf"))))
    (build-system python-build-system)
    (arguments
     `(;; XXX: Tests are not distributed with the PyPI archive, and would
       ;; introduce a circular dependency on python-cfn-lint.
       #:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'loosen-requirements
                    (lambda _
                      ;; The package needlessly specifies exact versions
                      ;; of dependencies, when it works fine with others.
                      (substitute* "requirements/base.txt"
                        (("(.*)(~=[0-9\\.]+)" all package version)
                         package)))))))
    (propagated-inputs
     (list python-boto3 python-jsonschema python-six))
    (home-page "https://github.com/aws/serverless-application-model")
    (synopsis "Transform AWS SAM templates into AWS CloudFormation templates")
    (description
     "AWS SAM Translator is a library that transform @dfn{Serverless Application
Model} (SAM) templates into AWS CloudFormation templates.")
    (license license:asl2.0)))

(define-public python-aws-xray-sdk
  (package
    (name "python-aws-xray-sdk")
    (version "2.6.0")
    (home-page "https://github.com/aws/aws-xray-sdk-python")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12fzr0ylpa1lx3xr1x2f1jx8iiyzcr6g57fb9jign0j0lxdlbzpv"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'disable-tests
                    (lambda _
                      (for-each delete-file
                                '(;; These tests require packages not yet in Guix.
                                  "tests/ext/aiobotocore/test_aiobotocore.py"
                                  "tests/ext/aiohttp/test_middleware.py"
                                  "tests/ext/pg8000/test_pg8000.py"
                                  "tests/ext/psycopg2/test_psycopg2.py"
                                  "tests/ext/pymysql/test_pymysql.py"
                                  "tests/ext/pynamodb/test_pynamodb.py"
                                  "tests/test_async_recorder.py"

                                  ;; FIXME: Why is this failing?
                                  "tests/test_patcher.py"

                                  ;; TODO: How to configure Django for these tests.
                                  "tests/ext/django/test_db.py"
                                  "tests/ext/django/test_middleware.py"

                                  ;; These tests want to access httpbin.org.
                                  "tests/ext/requests/test_requests.py"
                                  "tests/ext/httplib/test_httplib.py"
                                  "tests/ext/aiohttp/test_client.py"))))
                  (replace 'check
                    (lambda _
                      ;; Allow "import tests.utils" to work as expected.
                      (setenv "PYTHONPATH" (getcwd))
                      (invoke "pytest" "-vv" "tests"))))))
    (native-inputs
     (list ;; These are required for the test suite.
           python-bottle
           python-flask
           python-flask-sqlalchemy
           python-pymysql
           python-pytest
           python-pytest-aiohttp
           python-requests
           python-sqlalchemy
           python-webtest))
    (propagated-inputs
     (list python-aiohttp
           python-botocore
           python-future
           python-jsonpickle
           python-urllib3
           python-wrapt))
    (synopsis "Profile applications on AWS X-Ray")
    (description
     "The AWS X-Ray SDK for Python enables Python developers to record and
emit information from within their applications to the AWS X-Ray service.")
    (license license:asl2.0)))

(define-public python-cfn-lint
  (package
    (name "python-cfn-lint")
    (version "0.54.3")
    (home-page "https://github.com/aws-cloudformation/cfn-lint")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "106qf19n2k6sdjkb4006aidibd24qqiw901c1613xgjpnyw4dyl6"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (let ((out (assoc-ref outputs "out")))
                 ;; Remove test for the documentation update scripts
                 ;; to avoid a dependency on 'git'.
                 (delete-file
                  "test/unit/module/maintenance/test_update_documentation.py")
                 (delete-file
                  "test/unit/module/maintenance/test_update_resource_specs.py")
                 (add-installed-pythonpath inputs outputs)
                 (setenv "PATH" (string-append out "/bin:"
                                               (getenv "PATH")))
                 (invoke "python" "-m" "unittest" "discover"
                         "-s" "test"))))))))
    (native-inputs
     (list python-pydot python-mock))
    (propagated-inputs
     (list python-aws-sam-translator
           python-jsonpatch
           python-jsonschema
           python-junit-xml
           python-networkx
           python-pyyaml
           python-six))
    (synopsis "Validate CloudFormation templates")
    (description
     "This package lets you validate CloudFormation YAML/JSON templates against
the CloudFormation spec and additional checks.  Includes checking valid values
for resource properties and best practices.")
    (license license:expat)))

(define-public python-falcon
  (package
    (name "python-falcon")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "falcon" version))
       (sha256
        (base32
         "1z6mqfv574x6jiawf67ib52g4kk20c2x7xk7wrn1573b8v7r79gf"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (delete-file-recursively "falcon/vendor")
           (substitute* "setup.py"
             ((".*falcon\\.vendor\\.mimeparse.*") ""))
           (substitute* '("falcon/media/handlers.py"
                          "falcon/request.py")
             (("from falcon\\.vendor ") ""))
           (substitute* "falcon.egg-info/SOURCES.txt"
             (("falcon/vendor.*") ""))
           #t))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Skip orjson, which requires rust to build.
             (substitute* "tests/test_media_handlers.py"
               (("== 'CPython") "!= 'CPython"))
             (add-installed-pythonpath inputs outputs)
             (invoke "pytest" "--ignore" "falcon"))))))
    (propagated-inputs
     (list python-mimeparse))
    (native-inputs
     (list python-cython ;for faster binaries
           python-mujson
           python-msgpack
           python-pytest
           python-pytest-runner
           python-pyyaml
           python-rapidjson
           python-requests
           python-testtools
           python-ujson))
    (home-page "https://falconframework.org")
    (synopsis
     "Web framework for building APIs and application backends")
    (description
     "Falcon is a web API framework for building microservices, application
backends and higher-level frameworks.  Among its features are:
@itemize
@item Optimized and extensible code base
@item Routing via URI templates and REST-inspired resource
classes
@item Access to headers and bodies through request and response
classes
@item Request processing via middleware components and hooks
@item Idiomatic HTTP error responses
@item Straightforward exception handling
@item Unit testing support through WSGI helpers and mocks
@item Compatible with both CPython and PyPy
@item Cython support for better performance when used with CPython
@end itemize")
    (license license:asl2.0)))

(define-public python-falcon-cors
  (package
    (name "python-falcon-cors")
    (version "1.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "falcon-cors" version))
       (sha256
        (base32
         "12pym7hwsbd8b0c1azn95nas8gm3f1qpr6lpyx0958xm65ffr20p"))))
    (build-system python-build-system)
    (native-inputs
     (list python-falcon))
    (home-page
     "https://github.com/lwcolton/falcon-cors")
    (synopsis "Falcon @dfn{cross-origin resource sharing} (CORS) library")
    (description "This middleware provides @dfn{cross-origin resource
sharing} (CORS) support for Falcon.  It allows applying a specially crafted
CORS object to the incoming requests, enabling the ability to serve resources
over a different origin than that of the web application.")
    (license license:asl2.0)))

(define-public python-furl
  (package
    (name "python-furl")
    (version "2.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "furl" version))
        (sha256
          (base32
            "0knc76pm8pzigs3bpx9fccfsfxqrgblqphar46hq9i364vz8hqas"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-six python-orderedmultidict))
    (native-inputs
     (list python-flake8))
    (home-page "https://github.com/gruns/furl")
    (synopsis "URL manipulation in Python")
    (description "Furl provides an easy-to-use alternative to the
@code{urllib} and @code{urlparse} modules for manipulating URLs.")
    (license license:unlicense)))

(define-public python-httplib2
  (package
    (name "python-httplib2")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "httplib2" version))
       (sha256
        (base32
         "126rsryvw9vhbf3qmsfw9lf4l4xm2srmgs439lgma4cpag4s3ay3"))))
    (build-system python-build-system)
    (home-page "https://github.com/jcgregorio/httplib2")
    (synopsis "Comprehensive HTTP client library")
    (description
     "A comprehensive HTTP client library supporting many features left out of
other HTTP libraries.")
    (license license:expat)))

(define-public python2-httplib2
  (package-with-python2 python-httplib2))

(define-public httpie
  (package
    (name "httpie")
    (version "2.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "httpie" version))
       (sha256
        (base32
         "15ngl3yc186gkgqdx8iav9bpj8gxjpzz26y32z92jwyhj4cmfh6m"))))
    (build-system python-build-system)
    (arguments
     ;; The tests attempt to access external web servers, so we cannot run them.
     '(#:tests? #f))
    (propagated-inputs
     (list python-colorama python-pygments python-requests
           python-requests-toolbelt))
    (home-page "https://httpie.io")
    (synopsis "cURL-like tool for humans")
    (description
     "A command line HTTP client with an intuitive UI, JSON support,
syntax highlighting, wget-like downloads, plugins, and more.  It consists of
a single http command designed for painless debugging and interaction with
HTTP servers, RESTful APIs, and web services.")
    ;; This was fixed in 1.0.3.
    (properties `((lint-hidden-cve . ("CVE-2019-10751"))))
    (license license:bsd-3)))

(define-public python-html2text
  (package
    (name "python-html2text")
    (version "2020.1.16")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "html2text" version))
       (sha256
        (base32
         "1fvv4z6dblii2wk1x82981ag8yhxbim1v2ksgywxsndh2s7335p2"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "pytest" "test/"))))))
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/Alir3z4/html2text")
    (synopsis "Convert HTML into plain text")
    (description "html2text takes HTML and converts it into plain ASCII text
which is also valid markdown.  html2text was originally written by Aaron
Swartz.")
    (license license:gpl3+)))

(define-public python-jose
  (package
    (name "python-jose")
    (version "3.3.0")
    (home-page "https://github.com/mpdavis/python-jose")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "18whsdpllg8574ma4r0qawkgw4nam6lsf63pi6761j38rvl84lg9"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (if tests?
                 (invoke "pytest" "-vv")
                 (format #t "test suite not run~%"))
             #t)))))
    (native-inputs
     (list ;; All native inputs are for tests.
           python-pyasn1 python-pytest python-pytest-cov
           python-pytest-runner))
    (propagated-inputs
     (list python-cryptography python-ecdsa python-rsa python-six))
    (synopsis "JOSE implementation in Python")
    (description
     "The @dfn{JavaScript Object Signing and Encryption} (JOSE) technologies
- JSON Web Signature (JWS), JSON Web Encryption (JWE), JSON Web Key (JWK), and
JSON Web Algorithms (JWA) - collectively can be used to encrypt and/or sign
content using a variety of algorithms.")
    (license license:expat)))

(define-public python-pyscss
  (package
    (name "python-pyscss")
    (version "1.3.7")
    (source
     (origin
       (method git-fetch)               ; no tests in PyPI release
       (uri (git-reference
             (url "https://github.com/Kronuz/pyScss")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0701hziiiw67blafgpmjhzspmrss8mfvif7fw0rs8fikddwwc9g6"))))
    (build-system python-build-system)
    (arguments
     ;; XXX: error in test collection, possible incompatibility with Pytest 6.
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "python" "-m" "pytest" "--pyargs" "scss")))))))
    (native-inputs
     (list python-pytest python-pytest-cov))
    (inputs
     (list pcre))
    (home-page "https://github.com/Kronuz/pyScss")
    (synopsis "Scss compiler for Python")
    (description "@code{pyScss} is a compiler for Sass, a superset language of
CSS3 that adds programming capabilities and some other syntactic sugar.")
    (license license:expat)))

(define-public python-jsonpickle
  (package
    (name "python-jsonpickle")
    (version "1.5.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "jsonpickle" version))
              (sha256
               (base32
                "0n93h9b9ad58lxdfbvgsh4b25mkg146qikzcgghyc75vjk7rp2cy"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda _
                      (invoke "pytest" "-vv"
                              ;; Prevent running the flake8 and black
                              ;; pytest plugins, which only tests style
                              ;; and frequently causes harmless failures.
                              "-o" "addopts=''"))))))
    (native-inputs
     (list python-setuptools-scm
           python-toml ;XXX: for setuptools_scm[toml]
           ;; For tests.
           python-numpy
           python-pandas
           python-pytest))
    (home-page "https://jsonpickle.github.io/")
    (synopsis "Serialize object graphs into JSON")
    (description
     "This package provides a Python library for serializing any arbitrary
object graph to and from JSON.")
    (license license:bsd-3)))

(define-public python-mechanicalsoup
  (package
    (name "python-mechanicalsoup")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "MechanicalSoup" version))
       (sha256
        (base32 "01sddjxy3rznh63hnl5lbv1hhk6xyiviwmkiw4x7v4ap35fb3lrp"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-beautifulsoup4 python-lxml python-requests python-six))
    (native-inputs
     (list python-pytest-cov
           python-pytest-flake8
           python-pytest-httpbin
           python-pytest-mock
           python-pytest-runner
           python-requests-mock))
    (home-page "https://mechanicalsoup.readthedocs.io/")
    (synopsis "Python library for automating website interaction")
    (description
     "MechanicalSoup is a Python library for automating interaction with
websites.  It automatically stores and sends cookies, follows redirects, and can
follow links and submit forms.  It doesn’t do JavaScript.")
    (license license:expat)))

(define-public python-hyperframe
  (package
    (name "python-hyperframe")
    (version "6.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hyperframe" version))
       (sha256
        (base32 "055951gyhnjqpa2al52rj34g8yrls9inyn56n7nfkj0x4d300ldf"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "pytest" "-vv" "test")))))))
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/python-hyper/hyperframe")
    (synopsis "HTTP/2 framing layer for Python")
    (description
     "This library contains the HTTP/2 framing code used in the hyper project.
It provides a pure-Python codebase that is capable of decoding a binary stream
into HTTP/2 frames.")
    (license license:expat)))

(define-public python-hpack
  (package
    (name "python-hpack")
    (version "4.0.0")
    (source
     (origin
       ;; PyPI tarball is missing some files necessary for the tests.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/python-hyper/hpack")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11qdayvz5a8zlzdcdm37f2z1fgnl67pz6j8xj2dz5rfa5lds29yq"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "pytest" "-vv" "test")))))))
    (native-inputs
     (list python-pytest))
    (home-page "https://hyper.rtfd.org")
    (synopsis "Pure-Python HPACK header compression")
    (description
     "This module contains a pure-Python HTTP/2 header encoding (HPACK) logic
for use in Python programs that implement HTTP/2.")
    (license license:expat)))

(define-public python-h11
  (package
    (name "python-h11")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "h11" version))
       (sha256
        (base32 "0hk0nll6qazsambp3kl8cxxsbl4gv5y9252qadyk0jky0sv2q8j7"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest" "-vv")))))))
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/python-hyper/h11")
    (synopsis "Pure-Python, bring-your-own-I/O implementation of HTTP/1.1")
    (description
     "This is a little HTTP/1.1 library written from scratch in Python, heavily
inspired by hyper-h2.  It's a bring-your-own-I/O library; h11 contains no IO
code whatsoever.  This means you can hook h11 up to your favorite network API,
and that could be anything you want.")
    (license license:expat)))

(define-public python-h2
  (package
    (name "python-h2")
    (version "4.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "h2" version))
       (sha256
        (base32 "1fraip114fm1ha5w37pdc0sk8dn9pb0ck267zrwwpap7zc4clfm8"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "python" "-m" "pytest" "-vv" "test")))))))
    (native-inputs
     (list python-hypothesis-6.23 python-pytest))
    (propagated-inputs
     (list python-hpack python-hyperframe))
    (home-page "https://github.com/python-hyper/h2")
    (synopsis "HTTP/2 State-Machine based protocol implementation")
    (description
     "This module contains a pure-Python implementation of a HTTP/2 protocol
stack.  It does not provide a parsing layer, a network layer, or any rules
about concurrency.  Instead, it's a purely in-memory solution, defined in
terms of data actions and HTTP/2 frames.  This is one building block of a full
Python HTTP implementation.")
    (license license:expat)))

(define-public python-sockjs-tornado
  (package
    (name "python-sockjs-tornado")
    (version "1.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sockjs-tornado" version))
       (sha256
        (base32
         "15dgv6hw6c7h3m310alw1h6p5443lrm9pyqhcv2smc13fz1v04pc"))))
    (build-system python-build-system)
    (arguments
     `(;; There are no tests, and running the test phase requires missing
       ;; dependencies
       #:tests? #f))
    (propagated-inputs
     (list python-tornado))
    (home-page "https://github.com/mrjoes/sockjs-tornado/")
    (synopsis
     "SockJS Python server implementation on top of the Tornado framework")
    (description
     "SockJS-tornado provides the server-side counterpart to a SockJS client
library, through the Tornado framework.

SockJS provides a low-latency, full-duplex, cross-domain communication channel
between a web browser and web server.")
    (license license:expat)))

(define-public python2-sockjs-tornado
  (package-with-python2 python-sockjs-tornado))

(define-public python-flask-assets
  (package
    (name "python-flask-assets")
    (version "2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Flask-Assets" version))
       (sha256
        (base32 "1hmqldxc7zciksmcl35jx0wbyrrxc7vk2a57mmmd8i07whsymz8x"))))
    (build-system python-build-system)
    (arguments
     ;; Tests require python-flask-script which is incompatible with Flask2.
     `(#:tests? #f))
    (propagated-inputs
     (list python-flask python-webassets))
    (home-page "https://github.com/miracle2k/flask-assets")
    (synopsis "Asset management for Flask")
    (description "This package integrates @code{webassets} with Flask, adding
support for merging, minifying and compiling CSS and Javascript files.")
    (license license:bsd-2)))

(define-public python-flask-babel
  (package
    (name "python-flask-babel")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "Flask-Babel" version))
        (sha256
          (base32
            "0gmb165vkwv5v7dxsxa2i3zhafns0fh938m2zdcrv4d8z5l099yn"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda _
                      (with-directory-excursion "tests"
                        (invoke "python" "tests.py")))))))
    (propagated-inputs
     (list python-flask python-babel python-jinja2 python-pytz))
    (home-page "https://github.com/python-babel/flask-babel")
    (synopsis "Add i18n/l10n support to Flask applications")
    (description "This package implements internationalization and localization
support for Flask.  This is based on the Python babel module as well as pytz -
both of which are installed automatically if you install this library.")
    (license license:bsd-3)))

(define-public python-flask-cors
  (package
    (name "python-flask-cors")
    (version "3.0.10")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "Flask-Cors" version))
              (sha256
               (base32
                "1pl16615fn1pc5n0vdrqlxm45mqsdjjxqv3gfkrs111v7wwkj25n"))))
    (build-system python-build-system)
    (native-inputs
     (list python-flask python-nose python-packaging))
    (propagated-inputs
     (list python-six))
    (home-page "https://flask-cors.readthedocs.io/en/latest/")
    (synopsis "Handle Cross-Origin Resource Sharing with Flask")
    (description
     "This package provides a Flask extension for handling @acronym{CORS,Cross
Origin Resource Sharing}, making cross-origin AJAX possible.")
    (license license:expat)))

(define-public python-flask-markdown
  (package
    (name "python-flask-markdown")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Flask-Markdown" version))
       (sha256
        (base32
         "0l32ikv4f7va926jlq4f7gx0xid247bhlxl6bd9av5dk8ljz1hyq"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f))        ; Tests seem to be incompatible with latest python
    (propagated-inputs
     (list python-markdown python-flask))
    (native-inputs
     (list python-nose))
    (home-page "https://github.com/dcolish/flask-markdown")
    (synopsis "Small extension to help with using Markdown in Flask")
    (description
     "Flask-Markdown supports several extensions for Markdown and integrates
into Jinja2 by default.")
    (license license:bsd-3)))

(define-public python-flask-misaka
  (package
    (name "python-flask-misaka")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "Flask-Misaka" version))
        (sha256
          (base32
            "12gm6hq3lvlj0ddw8p6lk5pky8jk3pw758ihffjl49shnnzc68zl"))))
    (build-system python-build-system)
    (native-inputs
      (list python-coverage python-mock))
    (propagated-inputs
      (list python-flask python-misaka))
    (home-page "https://github.com/singingwolfboy/flask-misaka/")
    (synopsis "Flask interface to Misaka, a Markdown parsing library")
    (description
      "This package provides an interface between the Flask web framework and
the Misaka Markdown parser.")
    (license license:expat)))

(define-public python-flask-session
  (package
    (name "python-flask-session")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Flask-Session" version))
       (sha256
        (base32
         "0ihzlhdhss8f93p3njzva9rdm7kmhaakdlzz680wmi583wr59vf9"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; Tests require the various storage backends to be present
    (propagated-inputs
     (list python-cachelib python-flask))
    (home-page "https://github.com/fengsp/flask-session")
    (synopsis "Adds server-side session support to your Flask application")
    (description
     "Flask-Session is an extension for Flask that adds support for
Server-side sessions, with a variety of different backends for session
storage.")
    (license license:bsd-3)))

(define-public python-html5lib
  (package
    (name "python-html5lib")
    (version "1.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "html5lib" version))
        (sha256
          (base32
            "0vqlhk0hgbsfkh7ybmby93xhlx8dq6pr5blf356ka3z2c41b9rdj"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-six python-webencodings
           ;; Required by Calibre 5.
           python-chardet))
    (arguments
     `(#:test-target "check"))
    (home-page
      "https://github.com/html5lib/html5lib-python")
    (synopsis
      "Python HTML parser based on the WHATWG HTML specification")
    (description
      "Html5lib is an HTML parser based on the WHATWG HTML specification
and written in Python.")
    (license license:expat)))

(define-public python2-html5lib
  (package-with-python2 python-html5lib))

;; Needed for python-bleach, a dependency of python-notebook
(define-public python-html5lib-0.9
  (package
    (inherit python-html5lib)
    (version "0.999")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "html5lib" version))
       (sha256
        (base32
         "17n4zfsj6ynmbwdwviywmj8r6nzr3xvfx2zs0xhndmvm51z7z263"))))))

(define-public python2-html5lib-0.9
  (package-with-python2 python-html5lib-0.9))

(define-public python-html5-parser
  (package
    (name "python-html5-parser")
    (version "0.4.9")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "html5-parser" version))
              (sha256
               (base32
                "13yl3vnf3sxl05m0nhpngnrz3g1jvyahd33lys3m3hfb91l8zzi5"))))
    (build-system python-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list libxml2))
    (propagated-inputs
     (list python-lxml python-beautifulsoup4 python-chardet))
    (home-page "https://html5-parser.readthedocs.io")
    (synopsis "Fast C-based HTML5 parsing for Python")
    (description "This package provides a fast implementation of the HTML5
parsing spec for Python.  Parsing is done in C using a variant of the gumbo
parser.  The gumbo parse tree is then transformed into an lxml tree, also in
C, yielding parse times that can be a thirtieth of the html5lib parse times.")
    ;; src/as-python-tree.[c|h] are licensed GPL3.  The other files
    ;; indicate ASL2.0, including the LICENSE file for the whole project.
    (license (list license:asl2.0 license:gpl3))))

(define-public python-minio
  (package
    (name "python-minio")
    (version "6.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "minio" version))
              (sha256
               (base32
                "1cxpa0m7mdvpdbc1g6wlihq6ja4g4paxkl6f3q84bbnx07zpbllp"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'check 'disable-failing-tests
                    (lambda _
                      ;; This test requires network access.
                      (delete-file "tests/unit/credentials_test.py")
                      #t)))))
    (native-inputs
     (list python-faker python-mock python-nose))
    (propagated-inputs
     (list python-certifi python-configparser python-dateutil python-pytz
           python-urllib3))
    (home-page "https://github.com/minio/minio-py")
    (synopsis "Programmatically access Amazon S3 from Python")
    (description
     "This package provides a Python library for interacting with any
Amazon S3 compatible object storage server.")
    (license license:asl2.0)))

(define-public python-pycurl
  (package
    (name "python-pycurl")
    (version "7.43.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pycurl" version))
       (sha256
        (base32 "1cwlb76vddqp2mxqvjbhf367caddzy82rhangddjjhjqaj8x4zgc"))))
    (build-system python-build-system)
    (arguments
     ;; The tests attempt to access external web servers, so we cannot run
     ;; them.  Furthermore, they are skipped altogether when using Python 2.
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                    (add-before 'build 'configure-tls-backend
                      (lambda _
                        ;; XXX: PycURL fails to automatically determine which TLS
                        ;; backend to use when cURL is built with --disable-static.
                        ;; See setup.py and <https://github.com/pycurl/pycurl/pull/147>.
                        (setenv "PYCURL_SSL_LIBRARY" "gnutls")
                        #t)))))
    (native-inputs
     (list python-nose python-bottle))
    (inputs
     (list curl gnutls))
    (home-page "http://pycurl.io/")
    (synopsis "Lightweight Python wrapper around libcurl")
    (description "Pycurl is a lightweight wrapper around libcurl.  It provides
high-speed transfers via libcurl and frequently outperforms alternatives.")

    ;; Per 'README.rst', this is dual-licensed: users can redistribute pycurl
    ;; under the terms of LGPLv2.1+ or Expat.
    (license (list license:lgpl2.1+ license:expat))))

(define-public python2-pycurl
  (package-with-python2 python-pycurl))

(define-public python-webencodings
  (package
    (name "python-webencodings")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "webencodings" version))
              (sha256
               (base32
                "08qrgrc4hrximb2gqnl69g01s93rhf2842jfxdjljc1dbwj1qsmk"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "py.test" "-v" "webencodings/tests.py")
             #t)))))
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/SimonSapin/python-webencodings")
    (synopsis "Character encoding aliases for legacy web content")
    (description
     "In order to be compatible with legacy web content when interpreting
something like @code{Content-Type: text/html; charset=latin1}, tools need
to use a particular set of aliases for encoding labels as well as some
overriding rules.  For example, @code{US-ASCII} and @code{iso-8859-1} on
the web are actually aliases for @code{windows-1252}, and a @code{UTF-8}
or @code{UTF-16} BOM takes precedence over any other encoding declaration.
The WHATWG @url{https://encoding.spec.whatwg.org/,Encoding} standard
defines all such details so that implementations do not have to
reverse-engineer each other.

This module implements the Encoding standard and has encoding labels and
BOM detection, but the actual implementation for encoders and decoders
is Python’s.")
    (license license:bsd-3)))

(define-public python2-webencodings
  (package-with-python2 python-webencodings))

(define-public python-openid
  (package
    (name "python-openid")
    (version "3.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python3-openid" version))
       (sha256
        (base32
         "1bxf9a3ny1js422j962zfzl4a9dhj192pvai05whn7j0iy9gdyrk"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
        (replace 'check
          (lambda* (#:key tests? #:allow-other-keys)
            (when tests?
              (invoke "coverage" "run" "-m"
                      "unittest" "openid.test.test_suite")))))))
    (propagated-inputs
     (list python-defusedxml))
    (native-inputs
     (list python-coverage python-psycopg2 python-django))
    (home-page "https://github.com/necaris/python3-openid")
    (synopsis "OpenID support for servers and consumers")
    (description "This library provides OpenID authentication for Python, both
for clients and servers.")
    (license license:asl2.0)))

(define-public python-cssutils
  (package
    (name "python-cssutils")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "cssutils" version))
        (sha256
         (base32
          "1bxchrbqzapwijap0yhlxdil1w9bmwvgx77aizlkhc2mcxjg1z52"))))
    (build-system python-build-system)
    (native-inputs
     (list unzip))               ; for unpacking the source
    (arguments
     `(#:tests? #f))                    ; tests require python-pbr < 1.7.0
    (home-page "http://cthedot.de/cssutils/")
    (synopsis
      "CSS Cascading Style Sheets library for Python")
    (description
      "Cssutils is a Python package for parsing and building CSS
Cascading Style Sheets.  Currently it provides a DOM only and no rendering
options.")
    (license license:lgpl3+)))

(define-public python-css-parser
  (package
    (inherit python-cssutils)
    (name "python-css-parser")
    (version "1.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "css-parser" version ".tar.gz"))
       (sha256
        (base32 "0bmg4kiiir6pj9x3sd12x4dz2c1xpp2bn5nn60fxnbk2lnl4im2f"))))
    (home-page "https://github.com/ebook-utils/css-parser")
    (synopsis "Fork of cssutils modified for parsing ebooks")
    (description
      "Css-parser is a Python package for parsing and building CSS
Cascading Style Sheets.  Currently it provides a DOM only and no rendering
options.

It's a fork of cssutils 1.0.2, updated and modified for parsing ebooks, due to
cssutils not receiving updates as of 1.0.2.")
    (license license:lgpl3+)))

(define-public python-cssselect
  (package
    (name "python-cssselect")
    (version "1.1.0")
    (source (origin
              ;; The PyPI release does not contain tests.
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/scrapy/cssselect")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0xslrnhbrmgakp4xg6k26qffay3kqffp3a2z2sk27c65rwxa79kc"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda _
                      (invoke "pytest" "-vv"))))))
    (native-inputs
     (list python-lxml python-pytest))
    (home-page "https://github.com/scrapy/cssselect")
    (synopsis "CSS3 selector parser and translator to XPath 1.0")
    (description
     "Cssselect ia a Python module that parses CSS3 Selectors and translates
them to XPath 1.0 expressions.  Such expressions can be used in lxml or
another XPath engine to find the matching elements in an XML or HTML document.")
    (license license:bsd-3)))

(define-public python-databricks-cli
  (package
    (name "python-databricks-cli")
    (version "0.14.1")
    (home-page "https://github.com/databricks/databricks-cli")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "03w19rzh72jll9phai23wp0c2mlv39qsrv50mhckziy39z60yxh8"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda _
                      (invoke "pytest" "tests" "-vv"
                              ;; XXX: This fails with newer Pytest
                              ;; (upstream uses Pytest 3..).
                              "-k" "not test_get_request_with_list"))))))
    (native-inputs
     (list ;; For tests.
           python-decorator python-mock python-pytest python-requests-mock))
    (propagated-inputs
     (list python-click python-configparser python-requests python-six
           python-tabulate))
    (synopsis "Command line interface for Databricks")
    (description
     "The Databricks Command Line Interface is a tool which provides an easy
to use interface to the Databricks platform.  The CLI is built on top of the
Databricks REST APIs.")
    (license license:asl2.0)))

(define-public python-openid-cla
  (package
    (name "python-openid-cla")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-openid-cla" version))
       (sha256
        (base32
         "102hy2qisvjxp5s0v9lvwqi4f2dk0dhns40vjgn008yxc7k0h3cr"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f)) ; No tests.
    (home-page "https://github.com/puiterwijk/python-openid-cla/")
    (synopsis "Implementation of the OpenID CLA extension for python-openid")
    (description "@code{openid-cla} is an implementation of the OpenID
contributor license agreement extension for python-openid.")
    (license license:bsd-3)))

(define-public python-openid-teams
  (package
    (name "python-openid-teams")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-openid-teams" version))
       (sha256
        (base32
         "05zrh78alav24rxkbqlpbad6d3x2nljk6z6j7kflxf3vdqa7w969"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f)) ; No tests.
    (home-page "https://github.com/puiterwijk/python-openid-teams/")
    (synopsis "Implementation of the OpenID teams extension for python-openid")
    (description
     "@code{openid-teams} is an implementation of the OpenID
teams extension for python-openid.")
    (license license:bsd-3)))

(define-public python-priority
  (package
    (name "python-priority")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "priority" version))
       (sha256
        (base32 "1gpzn9k9zgks0iw5wdmad9b4dry8haiz2sbp6gycpjkzdld9dhbb"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (invoke "pytest" "-vv" "test" "-k"
                     ;; This test exceeded the Hypothesis deadline.
                     "not test_period_of_repetition"))))))
    (native-inputs
     (list python-hypothesis python-pytest python-pytest-cov
           python-pytest-xdist))
    (home-page "https://python-hyper.org/projects/priority/en/latest/")
    (synopsis "Pure-Python implementation of the HTTP/2 priority tree")
    (description
     "Priority is a pure-Python implementation of the priority logic for HTTP/2,
set out in RFC 7540 Section 5.3 (Stream Priority).")
    (license license:expat)))

(define-public python-wsproto
  (package
    (name "python-wsproto")
    (version "0.15.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "wsproto" version))
       (sha256
        (base32 "17gsxlli4w8am1wwwl3k90hpdfa213ax40ycbbvb7hjx1v1rhiv1"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (invoke "pytest" "-vv" "test"))))))
    (native-inputs
     (list python-pytest))
    (propagated-inputs
     (list python-h11))
    (home-page "https://github.com/python-hyper/wsproto/")
    (synopsis "WebSockets state-machine based protocol implementation")
    (description
     "@code{wsproto} is a pure-Python implementation of a WebSocket protocol
stack.  It's written from the ground up to be embeddable in whatever program you
choose to use, ensuring that you can communicate via WebSockets, as defined in
RFC6455, regardless of your programming paradigm.")
    (license license:expat)))

(define-public hypercorn
  (package
    (name "hypercorn")
    (version "0.11.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Hypercorn" version))
       (sha256
        (base32 "16kai5d12f05jr89mj611zslxqri4cd7ixcgd6yhl211qlcyg8av"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "python" "-m" "pytest")))))))
    ;; Propagate because Hypercorn also exposes functionality over a module.
    (propagated-inputs
     (list python-h11
           python-h2
           python-priority
           python-toml
           python-typing-extensions
           python-wsproto))
    (native-inputs
     (list python-hypothesis
           python-mock
           python-pytest
           python-pytest-asyncio
           python-pytest-cov
           python-pytest-trio
           python-trio))
    (home-page "https://gitlab.com/pgjones/hypercorn/")
    (synopsis "ASGI Server based on Hyper libraries")
    (description
     "Hypercorn is an ASGI web server based on the sans-io hyper, h11, h2, and
wsproto libraries and inspired by Gunicorn.  It supports HTTP/1, HTTP/2,
WebSockets (over HTTP/1 and HTTP/2), ASGI/2, and ASGI/3 specifications.  It can
utilise asyncio, uvloop, or trio worker types.")
    (license license:expat)))

(define-public python-hypercorn
  (deprecated-package "python-hypercorn" hypercorn))

(define-public python-querystring-parser
  (package
    (name "python-querystring-parser")
    (version "1.2.4")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "querystring_parser" version))
              (sha256
               (base32
                "0qlar8a0wa003hm2z6wcpb625r6vjj0a70rsni9h8lz0zwfcwkv4"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda _
                      ;; XXX FIXME: This test is broken with Python 3.7:
                      ;; https://github.com/bernii/querystring-parser/issues/35
                      (substitute* "querystring_parser/tests.py"
                        (("self\\.assertEqual\\(self\\.knownValuesNormalized, result\\)")
                         "True"))
                      (invoke "python" "querystring_parser/tests.py"))))))
    (propagated-inputs
     (list python-six))
    (home-page "https://github.com/bernii/querystring-parser")
    (synopsis "QueryString parser that correctly handles nested dictionaries")
    (description
     "This package provides a query string parser for Python and Django
projects that correctly creates nested dictionaries from sent form/querystring
data.")
    (license license:expat)))

(define-public python-tornado
  (package
    (name "python-tornado")
    (version "5.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tornado" version))
       (sha256
        (base32
         "02clqk2116jbnq8lnaqmdw3p52nqrd9ib59r4xz2ll43fpcmhlaf"))))
    (build-system python-build-system)
    (arguments
     '(;; FIXME: Two tests error out with:
       ;; AssertionError: b'Error in atexit._run_exitfuncs:\nFileNotF[44 chars]ry\n' != b''
       ;; #:phases
       ;; (modify-phases %standard-phases
       ;;   (replace 'check
       ;;     (lambda _
       ;;       ;; 'setup.py test' hits an AssertionError on BSD-specific
       ;;       ;; "tornado/platform/kqueue.py". This is the supported method:
       ;;       (invoke "python" "-m" "tornado.test.runtests")
       ;;       #t)))
       #:tests? #f))
    (native-inputs
     (list python-certifi))
    (home-page "https://www.tornadoweb.org/")
    (synopsis "Python web framework and asynchronous networking library")
    (description
     "Tornado is a Python web framework and asynchronous networking library,
originally developed at FriendFeed.  By using non-blocking network I/O,
Tornado can scale to tens of thousands of open connections, making it ideal
for long polling, WebSockets, and other applications that require a long-lived
connection to each user.")
    (license license:asl2.0)
    (properties `((python2-variant . ,(delay python2-tornado))))))

(define-public python-tornado-6
  (package
    (name "python-tornado")
    (version "6.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tornado" version))
       (sha256
        (base32
         "14cpzdv6p6qvk6vn02krdh5rcfdi174ifdbr5s6lcnymgcfyiiik"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "python" "-m" "tornado.test.runtests")
             #t)))))
    (native-inputs
     (list python-certifi))
    (home-page "https://www.tornadoweb.org/")
    (synopsis "Python web framework and asynchronous networking library")
    (description
     "Tornado is a Python web framework and asynchronous networking library,
originally developed at FriendFeed.  By using non-blocking network I/O,
Tornado can scale to tens of thousands of open connections, making it ideal
for long polling, WebSockets, and other applications that require a long-lived
connection to each user.")
    (license license:asl2.0)))

(define-public python2-tornado
  (let ((tornado (package-with-python2 (strip-python2-variant python-tornado))))
    (package/inherit tornado
      (propagated-inputs
       `(("python2-backport-ssl-match-hostname"
          ,python2-backport-ssl-match-hostname)
         ("python2-backports-abc" ,python2-backports-abc)
         ("python2-singledispatch" ,python2-singledispatch)
          ,@(package-propagated-inputs tornado))))))

(define-public python-tornado-http-auth
  (package
    (name "python-tornado-http-auth")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tornado-http-auth" version))
       (sha256
        (base32 "0hyc5f0a09i5yb99pk4bxpg6w9ichbrb5cv7hc9hff7rxd8w0v0x"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-tornado))
    (home-page "https://github.com/gvalkov/tornado-http-auth")
    (synopsis "Digest and basic authentication module for Tornado")
    (description
     "Provides support for adding authentication to services using the Tornado
web framework, either via the basic or digest authentication schemes.")
    (license license:asl2.0)))

(define-public python-terminado
  (package
    (name "python-terminado")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "terminado" version))
       (sha256
        (base32
         "1smvra3sc9sg64w49kfn5yhagshq3x55839748ck5dvxvk4hgza6"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-tornado-6 python-ptyprocess))
    (native-inputs
     (list python-pytest))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _ (invoke "pytest" "-vv"))))))
    (home-page "https://github.com/jupyter/terminado")
    (synopsis "Terminals served to term.js using Tornado websockets")
    (description "This package provides a Tornado websocket backend for the
term.js Javascript terminal emulator library.")
    (license license:bsd-2)))

(define-public python-wsgi-intercept
  (package
    (name "python-wsgi-intercept")
    (version "1.2.2")
    (source (origin
             (method url-fetch)
             (uri (pypi-uri "wsgi_intercept" version))
             (sha256
              (base32
               "0kjj2v2dvmnpdd5h5gk9rzz0f54rhjb0yiz3zg65bmp65slfw65d"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-six))
    (native-inputs
     (list python-pytest python-httplib2 python-requests python-urllib3))
    (synopsis "Puts a WSGI application in place of a real URI for testing")
    (description "Wsgi_intercept installs a WSGI application in place of a real
URI for testing.  Testing a WSGI application normally involves starting a
server at a local host and port, then pointing your test code to that address.
Instead, this library lets you intercept calls to any specific host/port
combination and redirect them into a WSGI application importable by your test
program.  Thus, you can avoid spawning multiple processes or threads to test
your Web app.")
    (home-page "https://github.com/cdent/wsgi-intercept")
    (license license:expat)))

(define-public python-webob
  (package
    (name "python-webob")
    (version "1.8.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "WebOb" version))
       (sha256
        (base32
          "026i3z99nr3px75isa9mbnky5i7rffiv4d124h5kxfjjsxz92fma"))))
    (build-system python-build-system)
    (native-inputs
      (list python-nose))
    (home-page "https://webob.org/")
    (synopsis "WSGI request and response object")
    (description
      "WebOb provides wrappers around the WSGI request environment, and an
object to help create WSGI responses.")
    (license license:expat)))

(define-public python2-webob
  (package-with-python2 python-webob))

(define-public python-zope-event
  (package
    (name "python-zope-event")
    (version "4.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "zope.event" version))
       (sha256
        (base32
         "1ksbc726av9xacml6jhcfyn828hlhb9xlddpx6fcvnlvmpmpvhk9"))))
    (build-system python-build-system)
    (home-page "https://pypi.org/project/zope.event/")
    (synopsis "Event publishing system for Python")
    (description "Zope.event provides an event publishing API, intended for
use by applications which are unaware of any subscribers to their events.  It
is a simple event-dispatching system on which more sophisticated event
dispatching systems can be built.")
    (license license:zpl2.1)))

(define-public python2-zope-event
  (package-with-python2 python-zope-event))

(define-public python-zope-interface
  (package
    (name "python-zope-interface")
    (version "5.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "zope.interface" version))
       (sha256
        (base32
         "03nrl6b8cb600dnnh46y149awvrm0gxyqgwq5hdw3lvys8mw9r20"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))  ; test suite can't find python-zope-testing
    (native-inputs
     (list python-coverage python-nose python-zope-event
           python-zope-testing))
    (home-page "https://github.com/zopefoundation/zope.interface")
    (synopsis "Python implementation of the \"design by contract\"
methodology")
    (description "Zope.interface provides an implementation of \"object
interfaces\" for Python.  Interfaces are a mechanism for labeling objects as
conforming to a given API or contract.")
    (license license:zpl2.1)))

(define-public python2-zope-interface
  (package-with-python2 python-zope-interface))

(define-public python-zope-exceptions
  (package
    (name "python-zope-exceptions")
    (version "4.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "zope.exceptions" version))
       (sha256
        (base32
         "1nkgfwawswmyc6i0b8g3ymvja4mb507m8yhid8s4rbxq3dmqhwhd"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (if tests?
                 (invoke "zope-testrunner" "--test-path=src")
                 (format #t "test suite not run~%")))))))
    (native-inputs
     `(("python-zope-testrunner" ,python-zope-testrunner-bootstrap)))
    (propagated-inputs
     (list python-zope-interface))
    (home-page "https://pypi.org/project/zope.exceptions/")
    (synopsis "Zope exceptions")
    (description "Zope.exceptions provides general-purpose exception types
that have uses outside of the Zope framework.")
    (license license:zpl2.1)))

(define (python-zope-bootstrap-package orig)
  (package
    (inherit orig)
    (name (string-append (package-name orig) "-bootstrap"))
    (arguments
     (if (null? (package-arguments orig))
         `(#:tests? #f
           #:phases (modify-phases %standard-phases
                      (delete 'sanity-check)))
         (substitute-keyword-arguments (package-arguments orig)
           ((#:tests? _ #f) #f)
           ((#:phases phases '%standard-phases)
            `(modify-phases ,phases
               (delete 'sanity-check))))))
    (propagated-inputs `())
    (native-inputs `())
    (properties `((hidden? . #t)))))

(define-public python-zope-exceptions-bootstrap
  (python-zope-bootstrap-package python-zope-exceptions))

(define-public python2-zope-exceptions
  (package-with-python2 python-zope-exceptions))

(define-public python-zope-testing
  (package
    (name "python-zope-testing")
    (version "4.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "zope.testing" version))
       (sha256
        (base32
         "1sh3c3i0m8n8fnhqiry0bk3rr356i56ry7calmn57s1pvv8yhsyn"))))
    (build-system python-build-system)
    (home-page "https://pypi.org/project/zope.testing/")
    (synopsis "Zope testing helpers")
    (description "Zope.testing provides a number of testing utilities for HTML
forms, HTTP servers, regular expressions, and more.")
    (license license:zpl2.1)))

(define-public python2-zope-testing
  (package-with-python2 python-zope-testing))

(define-public python-zope-testrunner
  (package
    (name "python-zope-testrunner")
    (version "5.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "zope.testrunner" version))
       (sha256
        (base32
         "0jyyf1dcz156q95x2y7yw2v420q2xn3cff0c5aci7hmdmcbn0gc7"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f                    ;FIXME: Tests can't find zope.interface.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-problematic-test
           (lambda _
             ;; This test contains invalid syntax, which breaks bytecode
             ;; compilation.  For simplicity just remove it.
             (delete-file
              "src/zope/testrunner/tests/testrunner-ex/sample2/badsyntax.py"))))))
    (native-inputs
     (list python-zope-testing))
    (propagated-inputs
     (list python-six python-zope-exceptions python-zope-interface))
    (home-page "https://pypi.org/project/zope.testrunner/")
    (synopsis "Zope testrunner script")
    (description "Zope.testrunner provides a script for running Python
tests.")
    (license license:zpl2.1)))

(define-public python-zope-testrunner-bootstrap
  (package
    (inherit (python-zope-bootstrap-package python-zope-testrunner))
    (propagated-inputs
     `(("python-six" ,python-six)
       ("python-zope-exceptions" ,python-zope-exceptions-bootstrap)))
    (properties `((hidden? . #t)))))

(define-public python2-zope-testrunner
  (package-with-python2 python-zope-testrunner))

(define-public python-zope-i18nmessageid
  (package
    (name "python-zope-i18nmessageid")
    (version "5.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "zope.i18nmessageid" version))
       (sha256
        (base32
         "0ndhn4w1qgwkfbwf9vm2bgq418z5g0wmfsgl0d9nz62cd0mi8d4m"))))
    (build-system python-build-system)
    (native-inputs
     (list python-coverage python-zope-testrunner))
    (propagated-inputs
     (list python-six))
    (home-page "https://pypi.org/project/zope.i18nmessageid/")
    (synopsis "Message identifiers for internationalization")
    (description "Zope.i18nmessageid provides facilities for declaring
internationalized messages within program source text.")
    (license license:zpl2.1)))

(define-public python2-zope-i18nmessageid
  (package-with-python2 python-zope-i18nmessageid))

(define-public python-zope-schema
  (package
    (name "python-zope-schema")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "zope.schema" version))
       (sha256
        (base32
          "09jg47bxhfg1ahr1jxb5y0cbiszyk1j6fn1r1r7s6svjl3lbryr0"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (if tests?
               (invoke "zope-testrunner" "--test-path=src")
               #t))))))
    (propagated-inputs
     (list python-zope-event python-zope-interface))
    (native-inputs
     (list python-zope-i18nmessageid python-zope-testing
           python-zope-testrunner))
    (home-page "https://pypi.org/project/zope.schema/")
    (synopsis "Zope data schemas")
    (description "Zope.scheme provides extensions to zope.interface for
defining data schemas.")
    (license license:zpl2.1)))

(define-public python2-zope-schema
  (package-with-python2 python-zope-schema))

(define-public python-zope-configuration
  (package
    (name "python-zope-configuration")
    (version "4.4.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "zope.configuration" version))
              (sha256
               (base32
                "0g6vrl7y27z9cj5xyrww9xlzk4npj55mgmlrcd9d2nj08jn2pw79"))))
    (build-system python-build-system)
    (native-inputs
     (list python-manuel python-zope-testing python-zope-testrunner))
    (propagated-inputs
     (list python-zope-i18nmessageid python-zope-interface
           python-zope-schema))
    (home-page "https://pypi.org/project/zope.configuration/")
    (synopsis "Zope Configuration Markup Language")
    (description "Zope.configuration implements ZCML, the Zope Configuration
Markup Language.")
    (license license:zpl2.1)))

(define-public python-zope-configuration-bootstrap
  (python-zope-bootstrap-package python-zope-configuration))

(define-public python2-zope-configuration
  (package-with-python2 python-zope-configuration))

(define-public python-zope-copy
  (package
    (name "python-zope-copy")
    (version "4.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "zope.copy" version))
        (sha256
         (base32
          "06m75434krl57n6p73c2qj55k5i3fixg887j8ss01ih6zw4rvfs7"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "zope-testrunner" "--test-path=src" "\\[]"))))))
    (propagated-inputs
     (list python-zope-interface))
    (native-inputs
     `(("python-zope-component" ,python-zope-component-bootstrap)
       ("python-zope-location" ,python-zope-location-bootstrap)
       ("python-zope-testing" ,python-zope-testing)
       ("python-zope-testrunner" ,python-zope-testrunner)))
    (home-page "https://github.com/zopefoundation/zope.copy")
    (synopsis "Pluggable object copying mechanism")
    (description
     "This package provides a pluggable mechanism for copying persistent objects.")
    (license license:zpl2.1)))

(define-public python-zope-proxy
  (package
    (name "python-zope-proxy")
    (version "4.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "zope.proxy" version))
       (sha256
        (base32
         "14h7nyfbl5vpfk0rbviy4ygdfx0yx5kncvg6jpbdb0dhwna0ssm6"))))
    (build-system python-build-system)
    (native-inputs
     (list python-zope-security-bootstrap python-zope-testrunner))
    (propagated-inputs
     (list python-zope-interface))
    (home-page "https://pypi.org/project/zope.proxy/")
    (synopsis "Generic, transparent proxies")
    (description "Zope.proxy provides generic, transparent proxies for Python.
Proxies are special objects which serve as mostly-transparent wrappers around
another object, intervening in the apparent behavior of the wrapped object
only when necessary to apply the policy (e.g., access checking, location
brokering, etc.) for which the proxy is responsible.")
    (license license:zpl2.1)))

(define-public python-zope-proxy-bootstrap
  (python-zope-bootstrap-package python-zope-proxy))

(define-public python2-zope-proxy
  (package-with-python2 python-zope-proxy))

(define-public python-zope-hookable
  (package
    (name "python-zope-hookable")
    (version "5.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "zope.hookable" version))
        (sha256
         (base32
          "0hc82lfr7bk53nvbxvjkibkarngyrzgfk2i6bg8wshl0ly0pdl19"))))
    (build-system python-build-system)
    (native-inputs
     (list python-coverage python-zope-testing))
    (home-page "https://github.com/zopefoundation/zope.hookable")
    (synopsis "Zope hookable")
    (description "This package supports the efficient creation of hookable
objects, which are callable objects that are meant to be optionally replaced.
The idea is that you create a function that does some default thing and make i
hookable.  Later, someone can modify what it does by calling its sethook method
and changing its implementation.  All users of the function, including those
that imported it, will see the change.")
    (license license:zpl2.1)))

(define-public python-zope-location
  (package
    (name "python-zope-location")
    (version "4.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "zope.location" version))
       (sha256
        (base32
         "1b40pzl8v00d583d3gsxv1qjdw2dhghlgkbgxl3m07d5r3izj857"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; FIXME: Tests can't find zope.interface.
    (native-inputs
     (list python-zope-testrunner))
    (propagated-inputs
     (list python-zope-interface python-zope-proxy python-zope-schema))
    (home-page "https://pypi.org/project/zope.location/")
    (synopsis "Zope location library")
    (description "Zope.location implements the concept of \"locations\" in
Zope3, which are are special objects that have a structural location.")
    (license license:zpl2.1)))

(define-public python-zope-location-bootstrap
  (python-zope-bootstrap-package python-zope-location))

(define-public python2-zope-location
  (package-with-python2 python-zope-location))

(define-public python-zope-security
  (package
    (name "python-zope-security")
    (version "5.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "zope.security" version))
       (sha256
        (base32
         "11lfw67cigscfax9c5j63xcvz2qcj724zx5fcdqyc94am2glim0h"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-zope-component
           python-zope-i18nmessageid
           python-zope-interface
           python-zope-location
           python-zope-proxy
           python-zope-schema))
    (native-inputs
     (list python-btrees
           python-zope-component-bootstrap
           python-zope-configuration-bootstrap
           python-zope-location-bootstrap
           python-zope-testing
           python-zope-testrunner))
    (home-page "https://pypi.org/project/zope.security/")
    (synopsis "Zope security framework")
    (description "Zope.security provides a generic mechanism to implement
security policies on Python objects.")
    (license license:zpl2.1)))

(define-public python-zope-security-bootstrap
  (package
    (inherit (python-zope-bootstrap-package python-zope-security))
    (propagated-inputs
     `(("python-zope-i18nmessageid" ,python-zope-i18nmessageid)
       ("python-zope-interface" ,python-zope-interface)
       ("python-zope-proxy" ,python-zope-proxy-bootstrap)
       ("python-zope-schema" ,python-zope-schema)))))

(define-public python2-zope-security
  (package-with-python2 python-zope-security))

(define-public python-zope-component
  (package
    (name "python-zope-component")
    (version "4.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "zope.component" version))
       (sha256
        (base32
         "14iwp95hh6q5dj4k9h1iw75cbp89bs27nany4dinyglb44c8jqli"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (if tests?
                 (invoke "python" "setup.py" "test")
                 (format #t "test suite not run~%")))))))
    (native-inputs
     `(("python-persistent" ,python-persistent)
       ("python-zope-configuration" ,python-zope-configuration-bootstrap)
       ("python-zope-i18nmessageid" ,python-zope-i18nmessageid)
       ("python-zope-location" ,python-zope-location-bootstrap)
       ("python-zope-proxy" ,python-zope-proxy-bootstrap)
       ("python-zope-security" ,python-zope-security-bootstrap)
       ("python-zope-testing" ,python-zope-testing)
       ("python-zope-testrunner" ,python-zope-testrunner)))
    (propagated-inputs
     (list python-zope-deferredimport python-zope-deprecation
           python-zope-event python-zope-hookable python-zope-interface))
    (home-page "https://github.com/zopefoundation/zope.component")
    (synopsis "Zope Component Architecture")
    (description "Zope.component represents the core of the Zope Component
Architecture.  Together with the zope.interface package, it provides
facilities for defining, registering and looking up components.")
    (license license:zpl2.1)))

(define-public python-zope-component-bootstrap
  (python-zope-bootstrap-package python-zope-component))

(define-public python2-zope-component
  (package-with-python2 python-zope-component))

(define-public python-zope-deferredimport
  (package
    (name "python-zope-deferredimport")
    (version "4.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "zope.deferredimport" version))
        (sha256
         (base32
          "1q89v54dwniiqypjbwywwdfjdr4kdkqlyqsgrpplgvsygdg39cjp"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-zope-proxy))
    (native-inputs
     (list python-zope-testrunner))
    (home-page "https://github.com/zopefoundation/zope.deferredimport")
    (synopsis "Defer imports until used by code")
    (description
     "Often, especially for package modules, you want to import names for
convenience, but not actually perform the imports until necessary.  The
@code{zope.deferredimport} package provided facilities for defining names in
modules that will be imported from somewhere else when used.  You can also cause
deprecation warnings to be issued when a variable is used.")
    (license license:zpl2.1)))

(define-public python-ndg-httpsclient
  (package
    (name "python-ndg-httpsclient")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "ndg_httpsclient" version))
              (sha256
                (base32
                  "0412b7i1s4vj7lz9r72nmb28h9syd4q2x89bdirkkc3a6z8awbyp"))))
    (build-system python-build-system)
    (arguments
     '(;; The tests appear to require networking.
       #:tests? #f))
    (propagated-inputs
     (list python-pyasn1 python-pyopenssl))
    (synopsis "HTTPS support for Python's httplib and urllib2")
    (description "This is a HTTPS client implementation for httplib and urllib2
based on PyOpenSSL.  PyOpenSSL provides a more fully-featured SSL implementation
over the default provided with Python and, importantly, enables full
verification of the SSL peer.")
    (home-page "https://github.com/cedadev/ndg_httpsclient/")
    (license license:bsd-3)))

(define-public python-websocket-client
  (package
    (name "python-websocket-client")
    (version "1.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "websocket-client" version))
       (sha256
        (base32 "1xba9z6b211pandrlk2l5p8wj6gn7yfkpq1sxfbqjl6c19n8258k"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-network-test
           (lambda _
             ;; This test requires networking.
             (substitute* "websocket/tests/test_http.py"
               (("def testConnect") "def _testConnect"))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest" "-vv" "websocket/tests")))))))
    (native-inputs
     (list python-pysocks python-pytest python-websockets))
    (home-page "https://github.com/websocket-client/websocket-client")
    (synopsis "WebSocket client for Python")
    (description "The Websocket-client module provides the low level APIs for
WebSocket usage in Python programs.")
    (license license:lgpl2.1+)))

(define-public python-websocket-client-0.59
  (package
    (inherit python-websocket-client)
    (version "0.59.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "websocket-client" version))
       (sha256
        (base32 "0p0cz2mdissq7iw1n7jrmsfir0jfmgs1dvnpnrx477ffx9hbsxnk"))))))

(define-public python-purl
  (package
    (name "python-purl")
    (version "1.5")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "purl" version))
        (sha256
          (base32
            "15ibnz1xrh5msmn04j0nr00sz4n7jwx6cwd6zlx99kkz3vpin53m"))))
    (build-system python-build-system)
    (propagated-inputs (list python-six))
    (home-page
      "https://github.com/codeinthehole/purl")
    (synopsis
      "Python package for URL manipulation")
    (description
      "Purl is a Python package for handling URLs.")
    (license license:expat)))

(define-public python-apiron
  (package
    (name "python-apiron")
    (version "5.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "apiron" version))
       (sha256
        (base32 "1qwbqn47sf0aqznj1snbv37v8ijx476qqkjf5l9pac7xjkxsr8qk"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (invoke "pytest" "-vv" "--cov" "-k"
                     ;; This test tries to connect to the internet.
                     "not test_call"))))))
    (propagated-inputs
     (list python-requests))
    (native-inputs
     (list python-pytest python-pytest-cov))
    (home-page "https://github.com/ithaka/apiron")
    (synopsis "Python wrapper for interacting with RESTful APIs")
    (description
     "@code{apiron} provides a declarative, structured configuration of
services and endpoints with a unified interface for interacting with RESTful
APIs.")
    (license license:expat)))

(define-public python-beren
  (package
    (name "python-beren")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "beren" version))
       (sha256
        (base32 "1v3mdwfqsyza892zvs124ym9w1bkng1j56b7l4dwfjir3723xcgf"))))
    (build-system python-build-system)
    (arguments
     ;; The test tries to open a connection to a remote server.
     `(#:tests? #f))
    (propagated-inputs
     (list python-apiron))
    (home-page "https://github.com/teffalump/beren")
    (synopsis "REST client for Orthanc DICOM servers")
    (description
     "@code{beren} provides a REST client for Orthanc, a DICOM server.")
    (license license:gpl3+)))

(define-public python-requests
  (package
    (name "python-requests")
    (version "2.26.0")
    (source (origin
             (method url-fetch)
             (uri (pypi-uri "requests" version))
             (sha256
              (base32
               "19q73fq7hip7b74fwls3p9x6zwvfwqcwpn6kha3zsgvrrzw5iamq"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-certifi python-chardet python-charset-normalizer
           python-idna python-urllib3))
    (arguments
     ;; FIXME: Some tests require network access.
     '(#:tests? #f))
    (home-page "http://python-requests.org/")
    (synopsis "Python HTTP library")
    (description
     "Requests is a Python HTTP client library.  It aims to be easier to use
than Python’s urllib2 library.")
    (license license:asl2.0)
    (properties `((python2-variant . ,(delay python2-requests))))))

(define-public python2-requests
  (let ((base (package-with-python2 (strip-python2-variant python-requests))))
    (package
      (inherit base)
      ;; The python-charset-normalizer dependency is necessary on Python 3
      ;; only.
      (propagated-inputs (modify-inputs (package-propagated-inputs base)
                           (delete "python-charset-normalizer"))))))

(define-public python-requests-unixsocket
  (package
    (name "python-requests-unixsocket")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "requests-unixsocket" version))
       (sha256
        (base32
         "1sn12y4fw1qki5gxy9wg45gmdrxhrndwfndfjxhpiky3mwh1lp4y"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'relax-requirements
           (lambda _
             (substitute* "test-requirements.txt"
               (("(.*)==(.*)" _ name) (string-append name "\n")))))
         (replace 'check
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             ;; Avoid a deprecation error.
             (substitute* "pytest.ini"
               (("--pep8") ""))
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "pytest" "-vv")))))))
    (propagated-inputs
     (list python-pbr python-requests python-urllib3))
    (native-inputs
     (list python-apipkg
           python-appdirs
           python-execnet
           python-packaging
           python-pep8
           python-py
           python-pyparsing
           python-pytest
           python-pytest-cache
           python-pytest-pep8
           python-six
           python-waitress))
    (home-page "https://github.com/msabramo/requests-unixsocket")
    (synopsis "Talk HTTP via a UNIX domain socket")
    (description
     "This Python package lets you use the @code{requests} library to talk
HTTP via a UNIX domain socket.")
    (license license:asl2.0)))

(define-public python-requests_ntlm
  (package
    (name "python-requests_ntlm")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "requests_ntlm" version))
       (sha256
        (base32
         "0wgbqzaq9w7bas16b7brdb75f91bh3275fb459093bk1ihpck2ci"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-cryptography python-ntlm-auth python-requests))
    (home-page "https://github.com/requests/requests-ntlm")
    (synopsis
     "NTLM authentication support for Requests")
    (description
     "This package allows for HTTP NTLM authentication using the requests
library.")
    (license license:isc)))

(define-public python-requests-mock
  (package
    (name "python-requests-mock")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "requests-mock" version))
       (sha256
        (base32
         "09nj8fmyj7xz2mgwyvbw0fl9zybmx2d3qd2hf529vvjc9s24d3z6"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-requests python-six))
    (native-inputs
     (list python-pbr
           python-discover
           python-docutils
           python-fixtures
           python-mock
           python-purl
           python-pytest
           python-sphinx
           python-testrepository))
    (home-page "https://requests-mock.readthedocs.org/")
    (synopsis "Mock out responses from the requests package")
    (description
      "This module provides a building block to stub out the HTTP requests
portions of your testing code.")
    (license license:asl2.0)))

(define-public python-requests-toolbelt
  (package
    (name "python-requests-toolbelt")
    (version "0.9.1")
    (source (origin
             (method url-fetch)
             (uri (pypi-uri "requests-toolbelt" version))
             (sha256
              (base32
               "1h3gm88dcjbd7gm229a7x5qkkhnsqsjz0m0l2xyavm2ab3a8k04n"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'delete-problematic-tests
                    (lambda _
                      ;; Fails because of expired certificate.
                      (delete-file "tests/test_x509_adapter.py")
                      ;; Fails due to networking (socket.gaierror: [Errno -2]
                      ;; Name or service not known).
                      (delete-file "tests/test_multipart_encoder.py"))))))
    (native-inputs
     (list python-betamax python-mock python-pytest))
    (propagated-inputs
     (list python-requests))
    (synopsis "Extensions to python-requests")
    (description "This is a toolbelt of useful classes and functions to be used
with python-requests.")
    (home-page "https://github.com/requests/toolbelt/")
    (license license:asl2.0)))

(define-public python-requests-ftp
  (package
    (name "python-requests-ftp")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "requests-ftp" version))
       (sha256
        (base32
         "0yh5v21v36dsjsgv4y9dx4mmz35741l5jf6pbq9w19d8rfsww13m"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-requests))
    (home-page
     "https://github.com/Lukasa/requests-ftp")
    (synopsis "FTP Transport Adapter for Requests")
    (description
     "Requests-FTP is an implementation of a simple FTP transport
adapter for use with the Requests library.")
    (license license:asl2.0)))

(define-public python-oauthlib
  (package
    (name "python-oauthlib")
    (version "3.1.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "oauthlib" version))
              (sha256
               (base32
                "12gqnabwck30gdlpwm6af3s28qm9p2yc7b1w8s4fk9ncbz1irr5y"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda _
                      (invoke "pytest" "-vv"))))))
    (native-inputs
     (list python-pytest python-pytest-cov python-mock))
    (propagated-inputs
     (list python-cryptography python-pyjwt python-blinker))
    (home-page "https://github.com/oauthlib/oauthlib")
    (synopsis "OAuth implementation for Python")
    (description
     "Oauthlib is a generic, spec-compliant, thorough implementation of the
OAuth request-signing logic.")
    (license license:bsd-3)))

(define-public python2-oauthlib
  (package-with-python2 python-oauthlib))

(define-public python-rauth
  (package
    (name "python-rauth")
    (version "0.7.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "rauth" version))
        (sha256
         (base32
          "02kv8w8l98ky223avyq7vw7x1f2ya9chrm59r77ylq45qb0xnk2j"))))
    (build-system python-build-system)
    (arguments
     `(#:test-target "check"))
    (propagated-inputs
     (list python-requests))
    (home-page "https://github.com/litl/rauth")
    (synopsis "Python library for OAuth 1.0/a, 2.0, and Ofly")
    (description
     "Rauth is a Python library for OAuth 1.0/a, 2.0, and Ofly.  It also
provides service wrappers for convenient connection initialization and
authenticated session objects providing things like keep-alive.")
    (license license:expat)
    (properties `((python2-variant . ,(delay python2-rauth))))))

(define-public python2-rauth
  (let ((base (package-with-python2 (strip-python2-variant python-rauth))))
    (package/inherit base
      (native-inputs `(("python2-unittest2" ,python2-unittest2)
                       ,@(package-native-inputs base))))))

(define-public python-urllib3
  (package
    (name "python-urllib3")
    (version "1.26.7")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "urllib3" version))
        (sha256
         (base32
          "1kkf6gi8a1fs0dqkf6kpmdpsy97iirvliz8q1krxp8ppaiawd1s9"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f))
    (propagated-inputs
     (list ;; These 5 inputs are used to build urrlib3[secure]
           python-certifi
           python-cryptography
           python-idna
           python-pyopenssl
           python-pysocks))
    (home-page "https://urllib3.readthedocs.io/")
    (synopsis "HTTP library with thread-safe connection pooling")
    (description
     "Urllib3 supports features left out of urllib and urllib2 libraries.  It
can reuse the same socket connection for multiple requests, it can POST files,
supports url redirection and retries, and also gzip and deflate decoding.")
    (properties `((python2-variant . ,(delay python2-urllib3))))
    (license license:expat)))

(define-public python2-urllib3
  (let ((base (package-with-python2 (strip-python2-variant python-urllib3))))
    (package/inherit
     base
     (propagated-inputs
      `(("python-ipaddress" ,python2-ipaddress)
        ,@(package-propagated-inputs base))))))

(define-public awscli
  (package
    ;; Note: updating awscli typically requires updating botocore as well.
    (name "awscli")
    (version "1.21.11")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri name version))
       (sha256
        (base32
         "0fkivwbx4nind5b7l4jhqm5bb9drgqsclcslrg4aggf9rcs4g4s0"))))
    (build-system python-build-system)
    (arguments
     ;; FIXME: The 'pypi' release does not contain tests.
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-reference-to-groff
           (lambda _
             (substitute* "awscli/help.py"
               (("if not self._exists_on_path\\('groff'\\):") "")
               (("raise ExecutableNotFoundError\\('groff'\\)") "")
               (("cmdline = \\['groff'")
                (string-append "cmdline = ['" (which "groff") "'"))))))))
    (propagated-inputs
     (list python-colorama-for-awscli
           python-botocore
           python-s3transfer
           python-docutils-0.15
           python-pyyaml
           python-rsa))
    (native-inputs
     (list groff))
    (home-page "https://aws.amazon.com/cli/")
    (synopsis "Command line client for AWS")
    (description "AWS CLI provides a unified command line interface to the
Amazon Web Services (AWS) API.")
    (license license:asl2.0)))

(define-public python-wsgiproxy2
  (package
    (name "python-wsgiproxy2")
    (version "0.4.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "WSGIProxy2" version ".tar.gz"))
       (sha256
        (base32 "16jch5nic0hia28lps3c678s9s9mjdq8n87igxncjg0rpi5adqnf"))))
    (build-system python-build-system)
    (native-inputs
     (list python-webtest))
    (propagated-inputs
     (list python-requests python-six python-urllib3 python-webob))
    (home-page "https://github.com/gawel/WSGIProxy2/")
    (synopsis "WSGI Proxy with various http client backends")
    (description "WSGI turns HTTP requests into WSGI function calls.
WSGIProxy turns WSGI function calls into HTTP requests.
It also includes code to sign requests and pass private data,
and to spawn subprocesses to handle requests.")
    (license license:expat)))

(define-public python-pastedeploy
  (package
    (name "python-pastedeploy")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PasteDeploy" version))
       (sha256
        (base32 "05s88qdjdwd9d9qs13fap7nqgxs7qs5qfzzjbrc5va13k2mxdskd"))))
    (build-system python-build-system)
    (arguments
     '(#:test-target "pytest"))
    (native-inputs
     (list python-pytest python-pytest-runner))
    (home-page "https://pylonsproject.org/")
    (synopsis
     "Load, configure, and compose WSGI applications and servers")
    (description
     "This tool provides code to load WSGI applications and servers from URIs;
these URIs can refer to Python Eggs for INI-style configuration files.  Paste
Script provides commands to serve applications based on this configuration
file.")
    (license license:expat)))

(define-public python2-pastedeploy
  (package-with-python2 python-pastedeploy))

(define-public python-webtest
  (package
    (name "python-webtest")
    (version "2.0.35")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "WebTest" version))
       (sha256
        (base32 "11xhgdj251zkvz5w30fvspii08ki2vrpr1im9sph1wmlnasnihda"))))
    (build-system python-build-system)
    (arguments
     ;; Tests require python-pyquery, which creates a circular dependency.
     `(#:tests? #f))
    (propagated-inputs
     (list python-waitress python-webob python-six python-beautifulsoup4))
    (home-page "https://docs.pylonsproject.org/projects/webtest/")
    (synopsis "Helper to test WSGI applications")
    (description "Webtest allows you to test your Python web applications
without starting an HTTP server.  It supports anything that supports the
minimum of WSGI.")
    (license license:expat)))

(define-public python-flask
  (package
    (name "python-flask")
    (version "2.0.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "Flask" version))
              (sha256
               (base32
                "1qilnrdakhbw5k951kczdy8ia0wczh0dpp1vi4qhgmfx6klvhbvv"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest" "-vv" "tests")))))))
    (native-inputs
     (list python-pytest))
    (propagated-inputs
     (list python-itsdangerous python-jinja2 python-click python-werkzeug))
    (home-page "https://www.palletsprojects.com/p/flask/")
    (synopsis "Microframework based on Werkzeug, Jinja2 and good intentions")
    (description "Flask is a micro web framework based on the Werkzeug toolkit
and Jinja2 template engine.  It is called a micro framework because it does not
presume or force a developer to use a particular tool or library.")
    (license license:bsd-3)))

(define-public python-flask-wtf
  (package
    (name "python-flask-wtf")
    (version "0.14.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Flask-WTF" version))
       (sha256
        (base32
         "086pvg2x69n0nczcq7frknfjd8am1zdy8qqpva1sanwb02hf65yl"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "pytest" "-vv"))))))
    (propagated-inputs
     (list python-flask-babel python-babel python-wtforms))
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/lepture/flask-wtf")
    (synopsis "Simple integration of Flask and WTForms")
    (description "Flask-WTF integrates Flask and WTForms, including CSRF, file
upload, and reCAPTCHA.")
    (license license:bsd-3)))

(define-public python-flask-multistatic
  (package
    (name "python-flask-multistatic")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "flask-multistatic" version))
       (sha256
        (base32
         "0p4v50rwv64wcd0zlq7rzl4waprwr4hj19s3cgf1isywa7jcisgm"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-flask))
    (home-page "https://pagure.io/flask-multistatic")
    (synopsis "Flask plugin to allow overriding static files")
    (description "@code{flask-multistatic} is a flask plugin that adds support
for overriding static files.")
    (license license:gpl3+)))

(define-public python-cookies
  (package
    (name "python-cookies")
    (version "2.2.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "cookies" version))
              (sha256
               (base32
                "13pfndz8vbk4p2a44cfbjsypjarkrall71pgc97glk5fiiw9idnn"))))
    (build-system python-build-system)
    (arguments
     `(;; test are broken: https://gitlab.com/sashahart/cookies/issues/3
       #:tests? #f))
    (native-inputs
     (list python-pytest))
    (synopsis "HTTP cookie parser and renderer")
    (description "A RFC 6265-compliant HTTP cookie parser and renderer in
Python.")
    (home-page "https://gitlab.com/sashahart/cookies")
    (license license:expat)))

(define-public python2-cookies
  (package-with-python2 python-cookies))

(define-public python-responses
  (package
    (name "python-responses")
    (version "0.10.6")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "responses" version))
              (sha256
               (base32
                "147pacwkkqy3qf3hr33fnl1xbzgw0zsm3qppvvy9qhq8h069qbah"))))
    (build-system python-build-system)
    (arguments
     `(;; Test suite is not distributed:
       ;; https://github.com/getsentry/responses/issues/38
       #:tests? #f))
    (native-inputs
     (list python-mock))
    (propagated-inputs
     (list python-requests python-cookies python-six))
    (home-page "https://github.com/getsentry/responses")
    (synopsis "Utility for mocking out the `requests` Python library")
    (description "A utility library for mocking out the `requests` Python
library.")
    (license license:asl2.0)))

(define-public python2-responses
  (package-with-python2 python-responses))

(define-public python-grequests
  (package
    (name "python-grequests")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "grequests" version))
       (sha256
        (base32
         "1j9icncllbkv7x5719b20mx670c6q1jrdx1sakskkarvx3pc8h8g"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-gevent python-requests))
    (native-inputs
     (list python-nose python-zope-interface python-zope-event))
    (home-page "https://github.com/kennethreitz/grequests")
    (synopsis "Python library for asynchronous HTTP requests")
    (description "GRequests is a Python library that allows you to use
@code{Requests} with @code{Gevent} to make asynchronous HTTP Requests easily")
    (license license:bsd-2)))

(define-public python-dpkt
  (package
    (name "python-dpkt")
    (version "1.9.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "dpkt" version))
       (sha256
        (base32
         "1d28r8pmhzjjd6hrn1xcddinfhwv8lcl1s59ygmqa8kfmz5pkrgl"))))
    (build-system python-build-system)
    (home-page "https://github.com/kbandla/dpkt")
    (synopsis "Packet generator and parser for TCP/IP protocols")
    (description "The dpkt module is a fast, simple packet generator and parser
for the basic TCP/IP protocols.")
    (license license:bsd-3)))

(define-public python-geventhttpclient
  (package
    (name "python-geventhttpclient")
    (version "1.5.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "geventhttpclient" version))
              (sha256
               (base32
                "104p14p67xa5gch8dy2zqmzmjra31fflk1c1alrry8dp8bzwj3nq"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete pre-compiled files.
                  (for-each delete-file (find-files "src/geventhttpclient"
                                                    ".*\\.pyc"))))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'delete-network-tests
           (lambda _
             (delete-file "src/geventhttpclient/tests/test_client.py")))
         (add-after 'unpack 'fix-compatibility-issue
           ;; See: https://github.com/gwik/geventhttpclient/issues/137.
           (lambda _
             (substitute* "src/geventhttpclient/tests/test_ssl.py"
               ((".*sock.last_seen_sni = None.*")
                ""))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest"  "src/geventhttpclient/tests" "-v"
                       ;; Append the test modules to sys.path to avoid
                       ;; namespace conflict which breaks SSL tests.
                       "--import-mode=append")))))))
    (native-inputs
     (list python-dpkt python-pytest))
    (propagated-inputs
     (list python-brotli python-certifi python-gevent python-six))
    (home-page "https://github.com/gwik/geventhttpclient")
    (synopsis "HTTP client library for gevent")
    (description "@code{python-geventhttpclient} is a high performance,
concurrent HTTP client library for python using @code{gevent}.")
    (license license:expat)))

(define-public python-requests-oauthlib
  (package
    (name "python-requests-oauthlib")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "requests-oauthlib" version))
       (sha256
        (base32
         "0mrglgcvq7k48pf27s4gifdk0za8xmgpf55jy15yjj471qrk6rdx"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; removes tests that require network access
         (add-before 'check 'pre-check
           (lambda _
             (delete-file "tests/test_core.py")
             #t)))))
    (native-inputs
     (list python-pyjwt python-requests-mock python-mock))
    (propagated-inputs
     (list python-oauthlib python-requests))
    (home-page
     "https://github.com/requests/requests-oauthlib")
    (synopsis
     "OAuthlib authentication support for Requests")
    (description
     "Requests-OAuthlib uses the Python Requests and OAuthlib libraries to
provide an easy-to-use Python interface for building OAuth1 and OAuth2 clients.")
    (license license:isc)))

(define-public python-url
  (package
    (name "python-url")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "url" version))
              (sha256
               (base32
                "0v879yadcz9qxfl41ak6wkga1kimp9cflla9ddz03hjjvgkqy5ki"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-publicsuffix))
    (native-inputs
     (list python-coverage python-nose))
    (arguments
     `(#:tests? #f)) ; FIXME: tests fail with "ImportError: No module named 'tests'"
    (home-page "https://github.com/seomoz/url-py")
    (synopsis "URL Parsing")
    (description "Library for parsing urls.")
    (license license:expat)))

(define-public python-cachecontrol
  (package
    (name "python-cachecontrol")
    (version "0.12.10")
    (source
     (origin
       (method git-fetch)
       ;; Pypi does not have tests.
       (uri (git-reference
             (url "https://github.com/ionrock/cachecontrol")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0fviacb13h72l5c77p35lgr6kvlidfdb9xsicg3v6lblmp9cn2ws"))))
    (build-system python-build-system)
    (arguments
     ;; Versions > 0.11.6 depend on CherryPy for testing.
     ;; It's too much work to package CherryPy for now.
     `(#:tests? #f))
    (propagated-inputs
     (list python-requests python-msgpack python-lockfile))
    (home-page "https://github.com/ionrock/cachecontrol")
    (synopsis "The httplib2 caching algorithms for use with requests")
    (description "CacheControl is a port of the caching algorithms in
@code{httplib2} for use with @code{requests} session objects.")
    (license license:asl2.0)))

(define-public python-cachecontrol-0.11
  (package
    (inherit python-cachecontrol)
    (name "python-cachecontrol")
    (version "0.11.7")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "CacheControl" version))
        (sha256
         (base32
          "07jsfhlbcwgqg6ayz8nznzaqg5rmxqblbzxz1qvg5wc44pcjjy4g"))))))

(define-public python-betamax
  (package
    (name "python-betamax")
    (version "0.8.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "betamax" version))
        (sha256
         (base32
          "1hki1c2vs7adq7zr56wi6i5bhrkia4s2ywpv2c98ibnczz709w2v"))))
    (build-system python-build-system)
    (arguments
     '(;; Many tests fail because they require networking.
       #:tests? #f))
    (propagated-inputs
     (list python-requests))
    (home-page "https://github.com/sigmavirus24/betamax")
    (synopsis "Record HTTP interactions with python-requests")
    (description "Betamax will record your test suite's HTTP interactions and
replay them during future tests.  It is designed to work with python-requests.")
    (license license:expat)))

(define-public python2-betamax
  (package-with-python2 python-betamax))

(define-public python-betamax-matchers
  (package
    (name "python-betamax-matchers")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "betamax-matchers" version))
       (sha256
        (base32
         "07qpwjyq2i2aqhz5iwghnj4pqr2ys5n45v1vmpcfx9r5mhwrsq43"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-betamax python-requests-toolbelt))
    (home-page "https://github.com/sigmavirus24/betamax_matchers")
    (synopsis "VCR imitation for python-requests")
    (description "@code{betamax-matchers} provides a set of Matchers for
Betamax.")
    (license license:asl2.0)))

(define-public python-betamax-serializers
  (package
    (name "python-betamax-serializers")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "betamax-serializers" version))
       (sha256
        (base32 "0ja9isbjmzzhxdj69s0kdsvw8nkp073w6an6a4liy5vk3fdl2p1l"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-betamax python-pyyaml))
    (synopsis "Set of third-party serializers for Betamax")
    (description "Betamax-Serializers are an experimental set of Serializers for
Betamax that may possibly end up in the main package.")
    (home-page "https://gitlab.com/betamax/serializers")
    (license license:asl2.0)))

(define-public python-s3transfer
  (package
    (name "python-s3transfer")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "s3transfer" version))
              (sha256
               (base32
                "0k6sc956yrrv9b4laa0r79jhxajpyxr21jcd1ka8m1n53lz85vah"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; Some of the 'integration' tests require network access or
               ;; login credentials.
               (invoke "nosetests" "--exclude=integration")))))))
    (native-inputs
     (list python-docutils python-mock python-nose))
    (propagated-inputs
     (list python-botocore python-urllib3))
    (synopsis "Amazon S3 Transfer Manager")
    (description "S3transfer is a Python library for managing Amazon S3
transfers.")
    (home-page "https://github.com/boto/s3transfer")
    (license license:asl2.0)))

(define-public python-slimit
  (package
    (name "python-slimit")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "slimit" version ".zip"))
       (sha256
        (base32
         "02vj2x728rs1127q2nc27frrqra4fczivnb7gch6n5lzi7pxqczl"))))
    (build-system python-build-system)
    (native-inputs
     (list unzip))
    (propagated-inputs
     (list python-ply))
    (home-page "https://slimit.readthedocs.io/")
    (synopsis "JavaScript minifier, parser and lexer written in Python")
    (description
     "SlimIt is a JavaScript minifier written in Python.  It compiles
JavaScript into more compact code so that it downloads and runs faster.
SlimIt also provides a library that includes a JavaScript parser, lexer,
pretty printer and a tree visitor.")
    (license license:expat)))

(define-public python-flask-restful
  (package
    (name "python-flask-restful")
    (version "0.3.8")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "Flask-RESTful" version))
        (patches (search-patches "python-flask-restful-werkzeug-compat.patch"))
        (sha256
         (base32
          "05b9lzx5yc3wgml2bcq50lq35h66m8zpj6dc9advcb5z3acsbaay"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-imports
           (lambda _
             (substitute* "flask_restful/__init__.py"
               (("flask\\.helpers") "flask.scaffold")))))))
    (propagated-inputs
      (list python-aniso8601 python-flask python-pycrypto python-pytz))
    (native-inputs
      (list ;; Optional dependency of Flask. Tests need it.
            python-blinker python-mock ; For tests
            python-nose))  ;for tests
    (home-page
      "https://www.github.com/flask-restful/flask-restful/")
    (synopsis
      "Flask module for creating REST APIs")
    (description
      "This package contains a Flask module for creating REST APIs.")
    (license license:bsd-3)))

(define-public python-flask-basicauth
  (package
    (name "python-flask-basicauth")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Flask-BasicAuth" version))
       (sha256
        (base32
         "1zq1spkjr4sjdnalpp8wl242kdqyk6fhbnhr8hi4r4f0km4bspnz"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'fix-imports
                    (lambda _
                      (substitute* '("docs/index.rst"
                                     "docs/conf.py"
                                     "flask_basicauth.py"
                                     "test_basicauth.py")
                        (("flask\\.ext\\.basicauth")
                         "flask_basicauth"))
                      #t)))))
    (propagated-inputs
     (list python-flask))
    (home-page "https://github.com/jpvanhal/flask-basicauth")
    (synopsis "HTTP basic access authentication for Flask")
    (description
     "This package provides HTTP basic access authentication for Flask.")
    (license license:bsd-3)))

(define-public python-flask-htpasswd
  (package
    (name "python-flask-htpasswd")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "flask-htpasswd" version))
        (sha256
          (base32
            "14q1y1y9i9bhabdnwd25jqzc4ljli23smxfyyh8abxz1vq93pxra"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-flask python-itsdangerous python-passlib python-tox))
    (home-page "https://github.com/carsongee/flask-htpasswd")
    (synopsis "Basic authentication via htpasswd files in Flask applications")
    (description "This package provides Basic authentication via
@file{htpasswd} files and access_token authentication in Flask
applications.")
    (license license:bsd-3)))

(define-public python-flask-sqlalchemy
  (package
    (name "python-flask-sqlalchemy")
    (version "2.5.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "Flask-SQLAlchemy" version))
              (sha256
               (base32
                "04jrx4sjrz1b20j38qk4qin975xwz30krzq59rfv3b3w7ss49nib"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-flask python-sqlalchemy))
    (home-page "https://github.com/mitsuhiko/flask-sqlalchemy")
    (synopsis "Module adding SQLAlchemy support to your Flask application")
    (description
     "This package adds SQLAlchemy support to your Flask application.")
    (license license:bsd-3)))

(define-public python-flask-restful-swagger
  (package
    (name "python-flask-restful-swagger")
    (version "0.20.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "flask-restful-swagger" version))
       (sha256
        (base32
         "1p66f98b5zpypnnz56pxpbirchqj6aniw6qyrp8h572l0dn9xlvq"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))          ;no tests
    (propagated-inputs
     (list python-flask-restful))
    (home-page "https://github.com/rantav/flask-restful-swagger")
    (synopsis "Extract Swagger specs from Flask-Restful projects")
    (description "This package lets you extract Swagger API documentation
specs from your Flask-Restful projects.")
    (license license:expat)))

(define-public python-htmlmin
  (package
    (name "python-htmlmin")
    (version "0.1.12")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "htmlmin" version))
       (sha256
        (base32
         "0y51xhabw6x8jk8k93xl8vznshpz3jb6l28075r5sjip613fzhah"))))
    (arguments
     `(#:tests? #f))                    ; no tests
    (build-system python-build-system)
    (home-page "https://htmlmin.readthedocs.org/en/latest/")
    (synopsis "HTML minifier")
    (description "@code{htmlmin} is an HTML minifier that just works.
It comes with safe defaults and easily configurable options.")
    (license license:bsd-3)))

(define-public python-flask-htmlmin
  (package
    (name "python-flask-htmlmin")
    (version "1.2")
    (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "Flask-HTMLmin" version))
      (sha256
       (base32
        "1n6zlq72kakkw0z2jpq6nh74lfsmxybm4g053pwhc14fbr809348"))))
    (propagated-inputs
     (list python-flask python-htmlmin))
    (build-system python-build-system)
    (home-page "https://github.com/hamidfzm/Flask-HTMLmin")
    (synopsis "HTML response minifier for Flask")
    (description
     "Minify @code{text/html} MIME type responses when using @code{Flask}.")
    (license license:bsd-3)))

(define-public python-jsmin
  (package
    (name "python-jsmin")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jsmin" version))
       (sha256
        (base32
         "1z1brjsvni0260bypldkl8a05sgp0qk18x560zl44igr3q99m5f0"))))
    (build-system python-build-system)
    (home-page "https://github.com/tikitu/jsmin/")
    (synopsis "Python JavaScript minifier")
    (description
     "@code{jsmin} is a JavaScript minifier, usable from both Python code and
on the command line.")
    (license license:expat)))

(define-public python-flask-login
  (package
    (name "python-flask-login")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/maxcountryman/flask-login")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11ac924w0y4m0kf3mxnxdlidy88jfa7njw5yyrq16dvnx4iwd8gg"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-flask))
    (native-inputs
     ;; For tests.
     (list python-blinker
           python-coverage
           python-mock
           python-pycodestyle
           python-pyflakes
           python-pytest
           python-semantic-version
           python-werkzeug))
    (home-page "https://github.com/maxcountryman/flask-login")
    (synopsis "User session management for Flask")
    (description
     "@code{Flask-Login} provides user session management for Flask.  It
handles the common tasks of logging in, logging out, and remembering your
users' sessions over extended periods of time.")
    (license license:expat)))

(define-public python-oauth2client
  (package
    (name "python-oauth2client")
    (version "4.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "oauth2client" version))
       (sha256
        (base32
         "1irqqap2zibysf8dba8sklfqikia579srd0phm5n754ni0h59gl0"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (propagated-inputs
     (list python-httplib2 python-pyasn1 python-pyasn1-modules python-rsa
           python-six))
    (home-page "https://github.com/google/oauth2client/")
    (synopsis "OAuth 2.0 client library")
    (description "@code{python-oauth2client} provides an OAuth 2.0 client
library for Python")
    (license license:asl2.0)))

(define-public python2-oauth2client
  (package-with-python2 python-oauth2client))

(define-public python-flask-oidc
  (package
    (name "python-flask-oidc")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "flask-oidc" version))
       (sha256
        (base32
         "0klgwpn2iy5y7011xh2c8zkryxdwkpxh7qjs3hp5cynl748ia4hc"))))
    (build-system python-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (invoke "nosetests")))))))
    (propagated-inputs
     (list python-flask python-itsdangerous python-oauth2client
           python-six))
    (native-inputs
     (list python-nose python-mock))
    (home-page "https://github.com/puiterwijk/flask-oidc")
    (synopsis "OpenID Connect extension for Flask")
    (description "@code{python-flask-oidc} provides an OpenID Connect extension
for Flask.")
    (license license:bsd-2)))

(define-public python-webassets
  (package
    (name "python-webassets")
    (version "2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "webassets" version))
       (sha256
        (base32
         "1kc1042jydgk54xpgcp0r1ib4gys91nhy285jzfcxj3pfqrk4w8n"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'check 'adjust-tests
                    (lambda _
                      ;; Fix for Python 3.9 compatibility.
                      (substitute* "tests/test_script.py"
                        (("self\\.t\\.isAlive")
                         "self.t.is_alive"))
                      ;; This test requires 'postcss' and 'babel' which are
                      ;; not yet available in Guix.
                      (delete-file "tests/test_filters.py")))
                  (replace 'check
                    (lambda _
                      (invoke "pytest" "-vv"))))))
    (native-inputs
     (list python-jinja2 python-mock python-nose python-pytest))
    (home-page "https://github.com/miracle2k/webassets")
    (synopsis "Media asset management")
    (description "Merges, minifies and compresses Javascript and CSS files,
supporting a variety of different filters, including YUI, jsmin, jspacker or
CSS tidy.  Also supports URL rewriting in CSS files.")
    (license license:bsd-2)))

(define-public python-cssmin
  (package
    (name "python-cssmin")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "cssmin" version))
        (sha256
         (base32
          "1dk723nfm2yf8cp4pj785giqlwv42l0kj8rk40kczvq1hk6g04p0"))))
    (build-system python-build-system)
    (home-page "https://github.com/zacharyvoase/cssmin")
    (synopsis "Python port of the YUI CSS Compressor")
    (description "Python port of the YUI CSS Compressor.")
    (license (list license:expat license:bsd-3))))

(define-public python-elasticsearch
  (package
    (name "python-elasticsearch")
    (version "7.13.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "elasticsearch" version))
        (sha256
         (base32
          "1q38w9nh2j2yi82d8rhzb57597l4lq5zx7xzfg45xf7ffrgsipaj"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-certifi python-urllib3))
    (arguments
     ;; tests require the test_elasticsearch module but it is not distributed.
     `(#:tests? #f))
    (home-page "https://github.com/elastic/elasticsearch-py")
    (synopsis "Low-level client for Elasticsearch")
    (description "Official low-level client for Elasticsearch.  Its goal is to
provide common ground for all Elasticsearch-related code in Python; because of
this it tries to be opinion-free and very extendable.")
    (license license:expat)))

(define-public python-engineio
  (package
    (name "python-engineio")
    (version "4.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-engineio" version))
       (sha256
        (base32
         "0xqkjjxbxakz9fd7v94rkr2r5r9nrkap2c3gf3abbd0j6ld5qmxv"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-aiohttp python-requests python-websocket-client))
    (arguments '(#:tests? #f))        ; Tests not included in release tarball.
    (home-page "https://github.com/miguelgrinberg/python-engineio/")
    (synopsis "Engine.IO server")
    (description "Python implementation of the Engine.IO realtime client and
server.")
    (license license:expat)))

(define-public python-flask-migrate
  (package
    (name "python-flask-migrate")
    (version "3.1.0")
    (home-page "https://github.com/miguelgrinberg/flask-migrate/")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (sha256
               (base32
                "0zj7qpknvlhrh4fsp5sx4fwyx3sp41ynclka992zympm3xym9zyq"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-flask python-alembic python-flask-sqlalchemy))
    (synopsis "SQLAlchemy database migrations for Flask programs using
Alembic")
    (description "This package contains SQLAlchemy database migration tools
for Flask programs that are using @code{python-alembic}.")
    (license license:expat)))

(define-public python-genshi
  (package
    (name "python-genshi")
    (version "0.7.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/edgewall/genshi")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04i0caywiwrgw09grz988n15qr9lr31d9n6a529p8v80cy1fv23c"))))
    (propagated-inputs
     (list python-six))
    (build-system python-build-system)
    (home-page "https://genshi.edgewall.org/")
    (synopsis "Toolkit for generation of output for the web")
    (description "Genshi is a Python library that provides an integrated set
of components for parsing, generating, and processing HTML, XML or other
textual content for output generation on the web.")
    (license license:bsd-3)))

(define-public python2-genshi
  (package-with-python2 python-genshi))

(define-public python-flask-principal
  (package
    (name "python-flask-principal")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "Flask-Principal" version))
        (sha256
          (base32
           "0lwlr5smz8vfm5h9a9i7da3q1c24xqc6vm9jdywdpgxfbi5i7mpm"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-blinker))
    (native-inputs
     (list python-flask python-nose))
    (home-page "https://pythonhosted.org/Flask-Principal/")
    (synopsis "Identity management for Flask")
    (description "@code{flask_principal} is a identity management library for
Flask.  It supports managing both authentication and authorization data in a
thread-local variable.")
    (license license:expat)))

(define-public python-flask-httpauth
  (package
    (name "python-flask-httpauth")
    (version "3.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Flask-HTTPAuth" version))
       (sha256
        (base32
         "13gff5w1mqpzm5nccyg02v3ifb9ifqh5k866cssjhghhg6msfjsz"))))
    (build-system python-build-system)
    (native-inputs
     (list python-flask))
    (home-page "https://github.com/miguelgrinberg/flask-httpauth/")
    (synopsis "Basic and Digest HTTP authentication for Flask routes")
    (description "@code{flask_httpauth} provides Basic and Digest HTTP
authentication for Flask routes.")
    (license license:expat)))

(define-public python-uritemplate
  (package
    (name "python-uritemplate")
    (version "4.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "uritemplate" version))
       (sha256
        (base32
         "1w14a775d92mx9pdhb5zimifpfr2lfcn0vfdpjagcy9vbkyfsij3"))))
    (build-system python-build-system)
    (home-page "https://uritemplate.readthedocs.org")
    (synopsis "Library to deal with URI Templates")
    (description "@code{uritemplate} provides Python library to deal with URI
Templates.")
    ;; The software is made available under the terms of *either* of the
    ;; licenses found in LICENSE.APACHE or LICENSE.BSD.  Contributions
    ;; are made under *both* licenses (excerpt from the LICENSE file).
    (license (list license:bsd-2 license:asl2.0))))

(define-public python-publicsuffix
  (package
    (name "python-publicsuffix")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "publicsuffix" version))
              (sha256
               (base32
                "1adx520249z2cy7ykwjr1k190mn2888wqn9jf8qm27ly4qymjxxf"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; tests use the internet
    (home-page "https://www.tablix.org/~avian/git/publicsuffix.git")
    (synopsis "Get suffix for a domain name")
    (description "Get a public suffix for a domain name using the Public Suffix
List.")
    (license license:expat)))

(define-public python-publicsuffix2
  (package
    (name "python-publicsuffix2")
    (version "2.20191221")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "publicsuffix2" version))
       (sha256
        (base32 "0yzysvfj1najr1mb4pcqrbmjir3xpb69rlffln95a3cdm8qwry00"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'ignore-maintainer-inputs
           (lambda _
             ;; Comment out a demand for python-requests, which is used only by
             ;; the unused ‘update_psl’ helper command.
             (substitute* "setup.py"
               (("'requests " match)
                (format #f "# ~a" match)))
             #t)))
       #:tests? #f))                  ; the test suite requires network access
    (home-page "https://github.com/pombredanne/python-publicsuffix2")
    (synopsis "Get a public suffix for a domain name using the Public Suffix List")
    (description "Get a public suffix for a domain name using the Public Suffix
List.  Forked from and using the same API as the publicsuffix package.")
    (license (list license:expat license:mpl2.0))))

(define-public python-werkzeug
  (package
    (name "python-werkzeug")
    (version "2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Werkzeug" version))
       (sha256
        (base32
         "16nvv9dh37ssf5pkny9yj2li0n6wyzsygh8a9i86r3gfipybcaxa"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "python" "-m" "pytest"
                       ;; Test tries to use the network.
                       "-k not test_reloader_sys_path")))))))
    (propagated-inputs
     (list python-requests))
    (native-inputs
     (list python-pytest python-pytest-timeout python-pytest-xprocess))
    (home-page "https://palletsprojects.com/p/werkzeug/")
    (synopsis "Utilities for WSGI applications")
    (description "One of the most advanced WSGI utility modules.  It includes a
powerful debugger, full-featured request and response objects, HTTP utilities to
handle entity tags, cache control headers, HTTP dates, cookie handling, file
uploads, a powerful URL routing system and a bunch of community-contributed
addon modules.")
    (license license:x11)))

(define-public python-werkzeug-1.0
  (package
    (inherit python-werkzeug)
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "Werkzeug" version))
              (sha256
               (base32
                "0z74sa1xw5h20yin9faj0vvdbq713cgbj84klc72jr9nmpjv303c"))
              (patches (search-patches "python-werkzeug-tests.patch"))))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (invoke "python" "-m" "pytest"))))))
    (propagated-inputs
     (list python-requests))
    (native-inputs
     (list python-pytest python-pytest-timeout))))

(define-public python-bottle
  (package
    (name "python-bottle")
    (version "0.12.19")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "bottle" version))
      (sha256
        (base32 "0b6s50vc4iad97b6bb3xnyrgajb3nj6n6jbr5p54a4vapky3zmx9"))))
    (build-system python-build-system)
    (home-page "https://bottlepy.org/")
    (synopsis "WSGI framework for small web-applications")
    (description "@code{python-bottle} is a WSGI framework for small web-applications.")
    (license license:expat)))

(define-public python2-bottle
  (package-with-python2 python-bottle))

(define-public python-wtforms
  (package
    (name "python-wtforms")
    (version "2.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "WTForms" version))
       (sha256
        (base32
         "17427m7p9nn9byzva697dkykykwcp2br3bxvi8vciywlmkh5s6c1"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'delete-bundled-test
           (lambda _
             ;; Delete test copied from a third party package that fails
             ;; with newer SQLAlchemy.  This can be removed for 3.0.
             ;; See <https://github.com/wtforms/wtforms/issues/696>.
             (delete-file "tests/ext_sqlalchemy.py")))
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "python" "setup.py" "compile_catalog")
               (invoke "python" "tests/runtests.py")))))))
    (native-inputs
     (list python-dateutil python-sqlalchemy))
    (propagated-inputs
     (list python-babel python-email-validator python-markupsafe))
    (home-page "http://wtforms.simplecodes.com/")
    (synopsis
     "Form validation and rendering library for Python web development")
    (description
     "WTForms is a flexible forms validation and rendering library
for Python web development.  It is very similar to the web form API
available in Django, but is a standalone package.")
    (license license:bsd-3)))

(define-public python-paste
  (package
    (name "python-paste")
    (version "3.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Paste" version))
       (sha256
        (base32
         "14lbi9asn5agsdf7r97prkjpz7amgmp529lbvfhf0nv881xczah6"))
       (patches (search-patches "python-paste-remove-timing-test.patch"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; This test calls out to the internet.
           (delete-file "tests/test_proxy.py") #t))))
    (build-system python-build-system)
    (native-inputs
     (list python-pytest python-pytest-runner python-nose))
    (propagated-inputs
     (list python-six))
    (home-page "https://pythonpaste.readthedocs.io/")
    (synopsis
     "Python web development tools, focusing on WSGI")
    (description
     "Paste provides a variety of web development tools and middleware which
can be nested together to build web applications.  Paste's design closely
follows ideas flowing from WSGI (Web Standard Gateway Interface).")
    (license license:expat)))

(define-public python-pastescript
  (package
    (name "python-pastescript")
    (version "2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PasteScript" version))
       (sha256
        (base32
         "1h3nnhn45kf4pbcv669ik4faw04j58k8vbj1hwrc532k0nc28gy0"))))
    (build-system python-build-system)
    (native-inputs
     (list python-nose))
    (propagated-inputs
     (list python-paste python-pastedeploy))
    (home-page (string-append "https://web.archive.org/web/20161025192515/"
                              "http://pythonpaste.org/script/"))
    (arguments
     '(;; Unfortunately, this requires the latest unittest2,
       ;; but that requires traceback2 which requires linecache2 which requires
       ;; unittest2.  So we're skipping tests for now.
       ;; (Note: Apparently linetest2 only needs unittest2 for its tests,
       ;; so in theory we could get around this situation somehow.)
       #:tests? #f))
    (synopsis
     "Pluggable command line tool for serving web applications and more")
    (description
     "PasteScript is a plugin-friendly command line tool which provides a
variety of features, from launching web applications to bootstrapping project
layouts.")
    (license license:expat)))

(define-public python-urlgrabber
  (package
    (name "python-urlgrabber")
    (version "4.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "urlgrabber" version))
       (sha256
        (base32 "0fg16zlw49cw7pjq9dhpc5vd35d5zz1mlki55m464qxfmfpzhnh7"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "python" "test/runtests.py")))))))
    (propagated-inputs
     (list python-pycurl python-setuptools python-six))
    (home-page "http://urlgrabber.baseurl.org/") ; no HTTPS
    (synopsis "High-level cross protocol url-grabber")
    (description
     "@code{urlgrabber} is a library that unifies access to files available on
the web, FTP or locally.  It supports HTTP, FTP and file:// protocols, it
supports features like HTTP keep-alive, reget, throttling and more.")
    (license license:lgpl2.1+)))

(define-public python-pycares
  (package
    (name "python-pycares")
    (version "2.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pycares" version))
       (sha256
        (base32
         "0h4fxw5drrhfyslzmfpljk0qnnpbhhb20hnnndzahhbwylyw1x1n"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))                    ;tests require internet access
    (home-page "https://github.com/saghul/pycares")
    (synopsis "Python interface for @code{c-ares}")
    (description "@code{pycares} is a Python module which provides an
interface to @code{c-ares}, a C library that performs DNS requests and
name resolutions asynchronously.")
    (license license:expat)))

(define-public python-yarl
  (package
    (name "python-yarl")
    (version "1.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "yarl" version))
       (sha256
        (base32
         "045z4ssg8g5h0qhz8hnx74hswgkndaldqq1xi5l1n5s0j996d44a"))
       (modules '((guix build utils)))
       (snippet
         #~(begin
             (delete-file "yarl/_quoting_c.c")))))
    (build-system python-build-system)
    (arguments
      (list #:tests? #f     ; test suite can't find yarl._quoting_c
            #:phases
            #~(modify-phases %standard-phases
                (add-after 'unpack 'cythonize-code
                  (lambda _
                    (invoke "cython" "yarl/_quoting_c.pyx")))
                (replace 'check
                  (lambda* (#:key tests? inputs outputs #:allow-other-keys)
                    (when tests?
                      (substitute* "setup.cfg"
                        (("--cov=yarl") ""))
                      (add-installed-pythonpath inputs outputs)
                      (invoke "python" "-m" "pytest")))))))
    (native-inputs
     (list python-cython python-pytest python-pytest-runner))
    (propagated-inputs
     (list python-idna python-multidict))
    (home-page "https://github.com/aio-libs/yarl/")
    (synopsis "Yet another URL library")
    (description "@code{yarl} module provides handy @code{URL} class
for URL parsing and changing.")
    (license license:asl2.0)))

(define-public python-canvasapi
  (package
    (name "python-canvasapi")
    (version "2.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ucfopen/canvasapi")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0i13wrq2czcaz3h98pvnsl237104v611y9636jf32b1nn76sbp0p"))))
    (build-system python-build-system)
    (propagated-inputs (list python-pytz python-requests))
    (native-inputs (list python-requests-mock))
    (home-page "https://github.com/ucfopen/canvasapi")
    (synopsis "API wrapper for the Canvas LMS")
    (description
     "CanvasAPI is a Python library for accessing Instructure’s Canvas LMS API.
The library enables developers to programmatically manage Canvas courses,
users, gradebooks, and more.")
    (license license:expat)))

(define-public python-google
  (package
    (name "python-google")
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "google" version))
              (sha256
               (base32
                "1gncv3l11za0mpxvmpaf5n5j3jzp282rz62yml4ha4z55q930d8l"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; There are no tests.
    (home-page "https://breakingcode.wordpress.com/")
    (synopsis "Python bindings to the Google search engine")
    (description "This package provides Python bindings for using the
Google search engine.  Its module is called @code{googlesearch}.")
    (license license:bsd-3)))

(define-public python-google-api-client
  (package
    (name "python-google-api-client")
    (version "1.6.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "google-api-python-client" version))
       (sha256
        (base32
         "1wpbbbxfpy9mwxdy3kn352cb590ladv574j1aa2l4grjdqw3ln05"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; tests require internet access
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-setup-py
           (lambda _
             (substitute* "setup.py"
               (("googleapiclient/discovery_cache")
                "googleapiclient.discovery_cache"))
             #t)))))
    (native-inputs
     (list python-httplib2 python-six python-oauth2client
           python-uritemplate))
    (home-page "https://github.com/google/google-api-python-client")
    (synopsis "Core Python library for accessing Google APIs")
    (description "Python client library for Google's discovery based APIs")
    (license license:asl2.0)))

(define-public python-hawkauthlib
  (package
    (name "python-hawkauthlib")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hawkauthlib" version))
       (sha256
        (base32
         "03ai47s4h8nfnrf25shbfvkm1b9n1ccd4nmmj280sg1fayi69zgg"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-requests python-webob))
    (home-page "https://github.com/mozilla-services/hawkauthlib")
    (synopsis "Hawk Access Authentication protocol")
    (description
     "This is a low-level Python library for implementing Hawk Access Authentication,
a simple HTTP request-signing scheme.")
    (license license:mpl2.0)))

(define-public python-pybrowserid
  (package
    (name "python-pybrowserid")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PyBrowserID" version))
       (sha256
        (base32
         "1qvi79kfb8x9kxkm5lw2mp42hm82cpps1xknmsb5ghkwx1lpc8kc"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-requests))
    (native-inputs
     (list python-mock))
    (home-page "https://github.com/mozilla/PyBrowserID")
    (synopsis "Python library for the BrowserID protocol")
    (description
     "This is a Python client library for the BrowserID protocol that
underlies Mozilla Persona.")
    (license license:mpl2.0)))

(define-public python-pyfxa
  (package
    (name "python-pyfxa")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PyFxA" version))
       (sha256
        (base32
         "0axl16fyrz2r88gnw4b12mk7dpkqscv8c4wsc1y5hicl7bsbc4fm"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f)) ; 17 tests require network access
    (propagated-inputs
     (list python-cryptography python-hawkauthlib python-pybrowserid
           python-requests python-six))
    (native-inputs
     (list python-grequests python-mock python-responses python-unittest2))
    (home-page "https://github.com/mozilla/PyFxA")
    (synopsis "Firefox Accounts client library for Python")
    (description
     "This is a Python library for interacting with the Firefox Accounts
ecosystem.")
    (license license:mpl2.0)))

(define-public python-hyperlink
  (package
    (name "python-hyperlink")
    (version "19.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "hyperlink" version))
        (sha256
         (base32
          "0m2nhi0j8wmgfscf974wd5v1xfq8mah286hil6npy1ys0m3y7222"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-idna))
    (home-page "https://github.com/python-hyper/hyperlink")
    (synopsis "Python module to create immutable URLs according to spec")
    (description "This package provides a Python module to create immutable, and
correct URLs for Python according to RFCs 3986 and 3987.")
    (license license:expat)))

(define-public python-treq
  (package
    (name "python-treq")
    (version "18.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "treq" version))
        (sha256
         (base32
          "0j4zwq9p1c9piv1vc66nxcv9s6hdinf90jwkbsm91k14npv9zq4i"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-attrs
           python-idna
           python-incremental
           python-requests
           python-service-identity
           python-twisted))
    (home-page "https://github.com/twisted/treq")
    (synopsis "Requests-like API built on top of twisted.web's Agent")
    (description "This package provides an HTTP library inspired by
@code{requests}} but written on top of Twisted's @code{Agents}.  It offers a
high level API for making HTTP requests when using Twisted.")
    (license license:expat)))

(define-public python-autobahn
  (package
    (name "python-autobahn")
    (version "19.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "autobahn" version))
        (sha256
         (base32
          "1mm7j24ls01c7jb1ad5p5cpyxvzgydiyf8b04ihykh2v8g98j0x7"))))
    (build-system python-build-system)
    (arguments
      ;; The tests fail to run:
      ;; https://github.com/crossbario/autobahn-python/issues/1117
     `(#:tests? #f))
    (propagated-inputs
     (list python-cffi python-twisted python-txaio))
    (home-page "https://crossbar.io/autobahn/")
    (synopsis "Web Application Messaging Protocol implementation")
    (description "This package provides an implementation of the @dfn{Web Application
Messaging Protocol} (WAMP).  WAMP connects components in distributed
applications using Publish and Subscribe (PubSub) and routed Remote Procedure
Calls (rRPC).  It is ideal for distributed, multi-client and server applications
such as IoT applications or multi-user database-driven business applications.")
    (license license:expat)))

(define-public python-ws4py
  (package
    (name "python-ws4py")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ws4py" version))
       (sha256
        (base32
         "10slbbf2jm4hpr92jx7kh7mhf48sjl01v2w4d8z3f1p0ybbp7l19"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'python3.7-compatibility
           (lambda _
             (substitute* '("ws4py/server/tulipserver.py"
                            "ws4py/async_websocket.py")
               (("asyncio.async")
                "asyncio.ensure_future"))
             #t))
         ;; We don't have a package for cherrypy.
         (add-after 'unpack 'remove-cherrypy-support
           (lambda _
             (delete-file "ws4py/server/cherrypyserver.py")
             #t)))))
    (propagated-inputs
     (list python-gevent python-tornado))
    (home-page "https://github.com/Lawouach/WebSocket-for-Python")
    (synopsis "WebSocket client and server library")
    (description
     "This package provides a WebSocket client and server library for
Python.")
    (license license:bsd-3)))

(define-public python-slugify
  (package
    (name "python-slugify")
    (version "5.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-slugify" version))
       (sha256
        (base32 "1aww2ncglyii4jkbfjxqhinivawf9zmwifcj32d69gpwp6h86czi"))))
    (propagated-inputs
     (list python-unidecode python-text-unidecode))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "python" "test.py")))))))
    (build-system python-build-system)
    (home-page "https://github.com/un33k/python-slugify")
    (synopsis "Python Slugify application that handles Unicode")
    (description "This package provides a @command{slufigy} command and
library to create slugs from unicode strings while keeping it DRY.")
    (license license:expat)))

(define-public python-branca
  (package
    (name "python-branca")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "branca" version))
       (sha256
        (base32
         "0pmigd521j2228xf8x34vbx0niwvms7xl7za0lymywj0vydjqxiy"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-jinja2 python-six))
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/python-visualization/branca")
    (synopsis "Generate complex HTML+JS pages with Python")
    (description "Generate complex HTML+JS pages with Python")
    (license license:expat)))

(define-public python-tinycss
  (package
    (name "python-tinycss")
    (version "0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tinycss" version))
       (sha256
        (base32 "0vkifr595h28ymkjhrswwf0bm23lhznh5f44xyp7x7jy1ssnyc0j"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-flake8-isort
           ;; Flake8 and isort tests fail.
           (lambda _
             (substitute* "setup.cfg" ((" --flake8 --isort") ""))
             #t))
         (replace 'check
           (lambda _
             ;; Disable failing test.
             (invoke "python" "-m" "pytest" "-k"
                     "not test_speedups"))))))
    (native-inputs
     (list python-pytest-cov python-pytest-flake8 python-pytest-isort
           python-pytest-runner))
    (home-page "https://tinycss.readthedocs.io/")
    (synopsis "Complete yet simple CSS parser for Python")
    (description
     "@code{tinycss} is a complete yet simple CSS parser for Python.  It
supports the full syntax and error handling for CSS 2.1 as well as some CSS 3
modules:

@itemize
@item CSS Color 3
@item CSS Fonts 3
@item CSS Paged Media 3
@end itemize")
    (license license:bsd-3)))

(define-public python-tinycss2
  (package
    (name "python-tinycss2")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Kozea/tinycss2")
             (commit (string-append "v" version))
             (recursive? #true)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zyc48vbmczpqj7f3f0d7zb3bz29fyj50dg0m6bbwbr5i88kq3sq"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             ;; A ZIP archive should be generated, but it fails with "ZIP does
             ;; not support timestamps before 1980".  Luckily,
             ;; SOURCE_DATE_EPOCH is respected, which we set to some time in
             ;; 1980.
             (setenv "SOURCE_DATE_EPOCH" "315532800")
             (invoke "flit" "build")))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (let ((out (assoc-ref outputs "out")))
               (for-each (lambda (wheel)
                           (format #true wheel)
                           (invoke "python" "-m" "pip" "install"
                                   wheel (string-append "--prefix=" out)))
                         (find-files "dist" "\\.whl$")))))
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (invoke "pytest" "-vv"))))))
    (propagated-inputs
     (list python-webencodings))
    (native-inputs
     (list python-flit python-pytest python-pytest-cov
           python-pytest-flake8 python-pytest-isort))
    (home-page "https://tinycss2.readthedocs.io/")
    (synopsis "Low-level CSS parser for Python")
    (description "@code{tinycss2} can parse strings, return Python objects
representing tokens and blocks, and generate CSS strings corresponding to
these objects.

Based on the CSS Syntax Level 3 specification, @code{tinycss2} knows the
grammar of CSS but doesn’t know specific rules, properties or values supported
in various CSS modules.")
    (license license:bsd-3)))

(define-public python-cssselect2
  (package
    (name "python-cssselect2")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cssselect2" version))
       (sha256
        (base32 "1j2fcr217rsvkipsg6zjq03rl64rxnvb5hqqpx0dv58fhspvkywk"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-linters
           ;; Their check fails; none of our business.
           (lambda _
             (substitute* '("setup.py" "pyproject.toml")
               (("'pytest-flake8',") "")
               (("'pytest-isort',") "")
               (("--flake8") "")
               (("--isort") ""))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (lambda _ (invoke "pytest"))))))))
    (propagated-inputs
     (list python-tinycss2))
    (native-inputs
     (list python-pytest-cov python-pytest-runner))
    (home-page "https://cssselect2.readthedocs.io/")
    (synopsis "CSS selectors for Python ElementTree")
    (description "@code{cssselect2} is a straightforward implementation of
CSS3 Selectors for markup documents (HTML, XML, etc.) that can be read by
ElementTree-like parsers (including cElementTree, lxml, html5lib, etc.).

Unlike the Python package @code{cssselect}, it does not translate selectors to
XPath and therefore does not have all the correctness corner cases that are
hard or impossible to fix in cssselect.")
    (license license:bsd-3)))

(define-public python-uvloop
  (package
    (name "python-uvloop")
    (version "0.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "uvloop" version))
       (sha256
        (base32 "0a0jzwrhkszknh14alflrp1db6dyjp7ph730f9yc5lb7gc6c4jzp"))
       (modules '((guix build utils)))
        (snippet
         '(begin (delete-file-recursively "vendor")
                 (delete-file  "uvloop/loop.c")))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'preparations
           (lambda _
             ;; Use packaged libuv.
             (substitute* "setup.py" (("self.use_system_libuv = False")
                                      "self.use_system_libuv = True"))
             ;; Replace hardcoded shell command.
             (substitute* "uvloop/loop.pyx"
               (("b'/bin/sh'") (string-append "b'" (which "sh") "'")))
             #t))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; Remove Python module, which conflicts with the installed version,
               ;; but lacks the built C module.
               (delete-file-recursively "uvloop")
               ;; The tests are prone to get stuck. Use pytest-timeout’s --timeout
               ;; flag to get a meaningful idea about where.
               (invoke "pytest" "-vv" "--timeout=300"
                       "-k" ,(string-append
                              ;; Timeout, because SIGINT cannot be sent to child.
                              "not test_signals_sigint_pycode_continue "
                              "and not test_signals_sigint_pycode_stop "
                              "and not test_signals_sigint_uvcode "
                              "and not test_signals_sigint_uvcode_two_loop_runs "
                              ;; It looks like pytest is preventing
                              ;; custom stdout/stderr redirection,
                              ;; even with -s.
                              "and not test_process_streams_redirect "))))))))
    (native-inputs
     (list python-aiohttp
           python-cython
           python-psutil
           python-pyopenssl
           python-pytest
           python-pytest-timeout))
    (inputs
     (list libuv))
    (home-page "https://github.com/MagicStack/uvloop")
    (synopsis "Fast implementation of asyncio event loop on top of libuv")
    (description
     "@code{uvloop} is a fast, drop-in replacement of the built-in asyncio
event loop.  It is implemented in Cython and uses libuv under the hood.")
    (license license:expat)))

(define-public gunicorn
  (package
    (name "gunicorn")
    (version "20.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "gunicorn" version))
       (sha256
        (base32
         "1s7670qw36x90bgmazmgib170i5gnpyb2ypxzlla7y0mpasniag0"))))
    (outputs '("out" "doc"))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-doc
           (lambda _
             (invoke "make" "-C" "docs" "PAPER=a4" "html" "info")
             (delete-file "docs/build/texinfo/Makefile")
             (delete-file "docs/build/texinfo/Gunicorn.texi")))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (if tests?
                 (begin
                   (invoke "pytest" "-vv"
                           ;; Disable the geventlet tests because eventlet uses
                           ;; dnspython, which does not work in the build
                           ;; container due to lack of /etc/resolv.conf, etc.
                           "--ignore=tests/workers/test_geventlet.py"))
                 (format #t "test suite not run~%"))))
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((doc (string-append (assoc-ref outputs "doc")
                                        "/share/doc/" ,name "-" ,version))
                    (html (string-append doc "/html"))
                    (info (string-append doc "/info"))
                    (examples (string-append doc "/examples")))
               (mkdir-p html)
               (mkdir-p info)
               (mkdir-p examples)
               (copy-recursively "docs/build/html" html)
               (copy-recursively "docs/build/texinfo" info)
               (copy-recursively "examples" examples)
               (for-each (lambda (file)
                           (copy-file file (string-append doc "/" file)))
                         '("README.rst" "NOTICE" "LICENSE" "THANKS")))))
         ;; XXX: The wrap phase includes native inputs on PYTHONPATH, (see
         ;; <https://bugs.gnu.org/25235>), leading to an inflated closure
         ;; size.  Override it to only add the essential entries.
         (replace 'wrap
           (lambda* (#:key native-inputs inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (python (assoc-ref (or native-inputs inputs) "python"))
                    (sitedir (string-append "/lib/python"
                                            (python-version python)
                                            "/site-packages")))
               (wrap-program (string-append out "/bin/gunicorn")
                 `("PYTHONPATH" ":" prefix
                   ,(map (lambda (output)
                           (string-append output sitedir))
                         (list python out))))))))))
    (native-inputs
     (list binutils ;; for ctypes.util.find_library()
           python-aiohttp
           python-gevent
           python-pytest
           python-pytest-cov
           python-sphinx
           texinfo))
    (home-page "https://gunicorn.org/")
    (synopsis "Python WSGI HTTP Server for UNIX")
    (description "Gunicorn ‘Green Unicorn’ is a Python WSGI HTTP
Server for UNIX.  It’s a pre-fork worker model ported from Ruby’s
Unicorn project.  The Gunicorn server is broadly compatible with
various web frameworks, simply implemented, light on server resources,
and fairly speedy.")
  (license license:expat)))

;; break cyclic dependency for python-aiohttp, which depends on gunicorn for
;; its tests
(define-public gunicorn-bootstrap
  (package
    (inherit gunicorn)
    (name "gunicorn")
    (arguments `(#:tests? #f))
    (properties '((hidden? . #t)))
    (native-inputs `())))

(define-public python-httptools
  (package
    (name "python-httptools")
    (version "0.1.1")
    (source
     (origin
       ;; PyPI tarball comes with a vendored http-parser and no tests.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/MagicStack/httptools")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g08128x2ixsiwrzskxc6c8ymgzs39wbzr5mhy0mjk30q9pqqv77"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'preparations
           (lambda _
             ;; Skip a failing test (AssertionError).  Bug report:
             ;; https://github.com/MagicStack/httptools/issues/10.
             (substitute* "tests/test_parser.py"
               (("    def test_parser_response_1")
                (string-append
                 "    @unittest.skip(\"Disabled.\")\n"
                 "    def test_parser_response_1")))
             ;; Use packaged http-parser.
             (substitute* "setup.py" (("self.use_system_http_parser = False")
                                      "self.use_system_http_parser = True"))
             ;; This path is hardcoded.  Hardcode our own.
             (substitute* "httptools/parser/cparser.pxd"
               (("../../vendor/http-parser")
                (string-append (assoc-ref %build-inputs "http-parser")
                               "/include")))
             ;; Don't force Cython version.
             (substitute* "setup.py" (("Cython==") "Cython>="))
             #t)))))
    (native-inputs
     (list python-cython python-pytest))
    (inputs
     (list http-parser))
    (home-page "https://github.com/MagicStack/httptools")
    (synopsis "Collection of framework independent HTTP protocol utils")
    (description
     "@code{httptools} is a Python binding for the nodejs HTTP parser.")
    (license license:expat)))

(define-public python-uvicorn
  (package
    (name "python-uvicorn")
    (version "0.13.2")
    (source
     (origin
       ;; PyPI tarball has no tests.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/encode/uvicorn")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04zgmp9z46k72ay6cz7plga6d3w3a6x41anabm7ramp7jdqf6na9"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (invoke "pytest" "-vv"))))))
    (native-inputs
     (list python-pytest python-pytest-mock python-requests
           python-trustme python-wsproto))
    (propagated-inputs
     (list python-click
           python-h11
           python-httptools
           python-pyyaml
           python-uvloop
           python-watchgod
           python-websockets))
    (home-page "https://github.com/encode/uvicorn")
    (synopsis "Fast ASGI server implementation")
    (description
     "@code{uvicorn} is a fast ASGI server implementation, using @code{uvloop}
and @code{httptools}.  It currently supports HTTP/1.1 and WebSockets.  Support
for HTTP/2 is planned.")
    (license license:bsd-3)))

(define-public python-translation-finder
  (package
    (name "python-translation-finder")
    (version "1.7")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "translation-finder" version))
        (sha256
         (base32
          "1pcy9z8gmb8x41gjhw9x0lkr0d2mv5mdxcs2hwg6q8mxs857j589"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'remove-failing-test
           (lambda _
             (delete-file "translation_finder/test_api.py")
             #t)))))
    (propagated-inputs
     (list python-chardet python-pathlib2 python-ruamel.yaml python-six))
    (native-inputs
     (list python-codecov python-codacy-coverage python-pytest-cov
           python-pytest-runner python-twine))
    (home-page "https://weblate.org/")
    (synopsis "Translation file finder for Weblate")
    (description "This package provides a function to find translation file in
the source code of a project.  It supports many translation file formats and
is part of the Weblate translation platform.")
    (license license:gpl3+)))

(define-public python-gitlab
  (package
    (name "python-gitlab")
    (version "1.15.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "python-gitlab" version))
        (sha256
         (base32
          "0zl6kz8v8cg1bcy2r78b2snb0lpw0b573gdx2x1ps0nhsh75l4j5"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-requests python-six))
    (native-inputs
     (list python-httmock python-mock))
    (home-page
      "https://github.com/python-gitlab/python-gitlab")
    (synopsis "Interact with GitLab API")
    (description "This package provides an extended library for interacting
with GitLab instances through their API.")
    (license license:lgpl3+)))

(define-public python-path-and-address
  (package
    (name "python-path-and-address")
    (version "2.0.1")
    (source
     (origin
       ;; The source distributed on PyPI doesn't include tests.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/joeyespo/path-and-address")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0b0afpsaim06mv3lhbpm8fmawcraggc11jhzr6h72kdj1cqjk5h6"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (invoke "py.test"))))))
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/joeyespo/path-and-address")
    (synopsis "Functions for command-line server tools used by humans")
    (description "Path-and-address resolves ambiguities of command-line
interfaces, inferring which argument is the path, and which is the address.")
    (license license:expat)))

(define-public grip
  ;; No release by upstream for quite some time, some bugs fixed since. See:
  ;; https://github.com/joeyespo/grip/issues/304
  (let ((commit "27a4d6d87ea1d0ea7f7f120de55baabee3de73e3"))
    (package
      (name "grip")
      (version (git-version "4.5.2" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/joeyespo/grip")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0kx5hgb3q19i4l18a4vqdq9af390xgpk88lp2ay75qi96k0dc68w"))))
      (build-system python-build-system)
      (propagated-inputs
       (list python-docopt
             python-flask
             python-markdown
             python-path-and-address
             python-pygments
             python-requests))
      (native-inputs
       (list python-pytest python-responses))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (add-installed-pythonpath inputs outputs)
               (setenv "PATH" (string-append
                                (getenv "PATH") ":"
                                (assoc-ref %outputs "out") "/bin"))
               (invoke "py.test" "-m" "not assumption"))))))
      (home-page "https://github.com/joeyespo/grip")
      (synopsis "Preview Markdown files using the GitHub API")
      (description "Grip is a command-line server application written in Python
that uses the GitHub Markdown API to render a local Markdown file.  The styles
and rendering come directly from GitHub, so you'll know exactly how it will
appear.  Changes you make to the file will be instantly reflected in the browser
without requiring a page refresh.")
      (license license:expat))))

(define-public python-port-for
  (package
    (name "python-port-for")
    (version "0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "port-for" version))
       (sha256
        (base32
         "1pncxlj25ggw99r0ijfbkq70gd7cbhqdx5ivsxy4jdp0z14cpda7"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'use-urllib3
           (lambda _
             (substitute* "port_for/_download_ranges.py"
               (("urllib2") "urllib3"))
             #t)))))
    (propagated-inputs
     (list python-urllib3))
    (native-inputs
     (list python-mock))
    (home-page "https://github.com/kmike/port-for/")
    (synopsis "TCP localhost port finder and association manager")
    (description
     "This package provides a utility that helps with local TCP ports
management.  It can find an unused TCP localhost port and remember the
association.")
    (license license:expat)))

(define-public python-livereload
  (package
    (name "python-livereload")
    (version "2.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "livereload" version))
       (sha256
        (base32
         "0rhggz185bxc3zjnfpmhcvibyzi86i624za1lfh7x7ajsxw4y9c9"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-six python-tornado))
    (home-page "https://github.com/lepture/python-livereload")
    (synopsis "Python LiveReload")
    (description
     "Python LiveReload provides a command line utility, @command{livereload},
for starting a web server in a directory.  It can trigger arbitrary commands
and serve updated contents upon changes to the directory.")
    (license license:bsd-3)))

(define-public python-vf-1
  (package
    (name "python-vf-1")
    (version "0.0.11")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "VF-1" version))
       (sha256
        (base32
         "0xlqsaxsiayk1sd07kpz8abbcnab582y29a1y4882fq6j4gma5xi"))))
    (build-system python-build-system)
    (home-page "https://github.com/solderpunk/VF-1")
    (synopsis "Command line gopher client")
    (description "@code{VF-1} is a command line gopher client with
@acronym{TLS, Transport Layer Security} support.")
    (license license:bsd-2)))

(define-public python-httpcore
  (package
    (name "python-httpcore")
    (version "0.12.2")
    (source
     (origin
       ;; PyPI tarball does not contain tests.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/encode/httpcore")
             (commit  version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nrwwfdqjfc2a1k3j41cdwkprwvplf95fwmypdl2aq2qgp3209q0"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; Tests hang at 98%
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "pytest" "-vv" "--cov=httpcore"
                       "--cov=tests" "tests"))
             #t)))))
    (native-inputs
     (list python-autoflake
           python-flake8
           python-flake8-bugbear
           python-flake8-pie
           python-isort
           python-mypy
           python-pytest
           python-pytest-asyncio
           python-pytest-cov
           python-pytest-trio
           python-uvicorn
           python-trustme))
    (propagated-inputs
     (list python-h11 python-h2 python-sniffio python-trio
           python-trio-typing))
    (home-page "https://github.com/encode/httpcore")
    (synopsis "Minimal, low-level HTTP client")
    (description
     "HTTP Core provides a minimal and low-level HTTP client, which does one
thing only: send HTTP requests.

Some things HTTP Core does do:

@itemize
@item Sending HTTP requests.
@item Provides both sync and async interfaces.
@item Supports HTTP/1.1 and HTTP/2.
@item Async backend support for asyncio and trio.
@item Automatic connection pooling.
@item HTTP(S) proxy support.
@end itemize")
    (license license:bsd-3)))

(define-public python-httpx
  (package
    (name "python-httpx")
    (version "0.16.1")
    (source
     (origin
       ;; PyPI tarball does not contain tests.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/encode/httpx")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00gmq45fckcqkj910bvd7pyqz1mvgsdvz4s0k7dzbnc5czzq1f4a"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest" "-vv" "-k"
                       ;; These tests try to open an outgoing connection.
                       (string-append
                        "not test_connect_timeout"
                        " and not test_that_send_cause_async_client_to_be_not_"
                        "closed"
                        " and not test_that_async_client_caused_warning_when_"
                        "being_deleted"
                        " and not test_that_send_cause_client_to_be_not_closed"
                        " and not test_async_proxy_close"
                        " and not test_sync_proxy_close"))))))))
    (native-inputs
     (list python-autoflake
           python-black
           python-cryptography
           python-flake8
           python-flake8-bugbear
           python-flake8-pie
           python-isort
           python-mypy
           python-pytest
           python-pytest-asyncio
           python-pytest-trio
           python-pytest-cov
           python-trio
           python-trio-typing
           python-trustme
           python-uvicorn))
    (propagated-inputs
     (list python-brotli
           python-certifi
           python-chardet
           python-httpcore
           python-idna
           python-rfc3986
           python-sniffio))
    (home-page "https://www.python-httpx.org/")
    (synopsis "HTTP client for Python")
    (description
     "HTTPX is a fully featured HTTP client for Python 3, which provides sync
and async APIs, and support for both HTTP/1.1 and HTTP/2.

HTTPX builds on the well-established usability of requests, and gives you:

@itemize
@item A broadly requests-compatible API.
@item Standard synchronous interface, but with async support if you need it.
@item HTTP/1.1 and HTTP/2 support.
@item Ability to make requests directly to WSGI applications or ASGI applications.
@item Strict timeouts everywhere.
@item Fully type annotated.
@item 99% test coverage.
@end itemize

Plus all the standard features of requests:

@itemize
@item International Domains and URLs
@item Keep-Alive & Connection Pooling
@item Sessions with Cookie Persistence
@item Browser-style SSL Verification
@item Basic/Digest Authentication
@item Elegant Key/Value Cookies
@item Automatic Decompression
@item Automatic Content Decoding
@item Unicode Response Bodies
@item Multipart File Uploads
@item HTTP(S) Proxy Support
@item Connection Timeouts
@item Streaming Downloads
@item .netrc Support
@item Chunked Requests
@end itemize")
    (license license:bsd-3)))

(define-public python-wsgiprox
  (package
    (name "python-wsgiprox")
    (version "1.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "wsgiprox" version))
       (sha256
        (base32
         "11fsm199pvwbmqx2lccznvws65aam1rqqv0w79gal8hispwgd5rs"))))
    (build-system python-build-system)
    (arguments
     ;; The test suite hangs (see:
     ;; https://github.com/webrecorder/wsgiprox/issues/6).
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-pytest-argument
           (lambda _
             ;; See: https://github.com/webrecorder/wsgiprox/issues/7.
             (substitute* "setup.py"
               (("--doctest-module")
                "--doctest-modules")))))))
    (propagated-inputs
     (list python-certauth python-gevent python-websocket-client))
    (native-inputs
     (list python-mock python-pytest-cov python-waitress))
    (home-page "https://github.com/webrecorder/wsgiprox")
    (synopsis "HTTP/S proxy with WebSockets over WSGI")
    (description "@code{wsgiprox} is a Python WSGI (Web Server Gateway
Interface) middle-ware for adding HTTP and HTTPS proxy support to a WSGI
application.  The library accepts HTTP and HTTPS proxy connections, and routes
them to a designated prefix.")
    (license license:asl2.0)))

(define-public python-warcio
  ;; The PyPI release is missing some test support files (see:
  ;; https://github.com/webrecorder/warcio/issues/132).
  (let ((revision "0")
        (commit "aa702cb321621b233c6e5d2a4780151282a778be"))
    (package
      (name "python-warcio")
      (version (git-version "1.7.4" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/webrecorder/warcio")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "11afr6zy3r6rda81010iq496dazg4xid0izg3smg6ighpmvsnzf2"))))
      (build-system python-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'skip-problematic-tests
             (lambda _
               ;; These tests fail due to networking requirements.
               (substitute* "setup.py"
                 (("pytest.main\\(\\[" all)
                  (string-append all "'-k', '"
                                 (string-append "not test_post_chunked and "
                                                "not test_remote") "'"))))))))
      (native-inputs
       ;; These inputs are required for the test suite.
       (list python-httpbin python-pytest-cov python-requests
             python-wsgiprox))
      (home-page "https://github.com/webrecorder/warcio")
      (synopsis "Streaming web archival archive (WARC) library")
      (description "warcio is a Python library to read and write the WARC format
commonly used in Web archives. It is designed for fast, low-level access to
web archival content, oriented around a stream of WARC records rather than
files.")
      (license license:asl2.0))))

(define-public python-websockets
  (package
    (name "python-websockets")
    (version "8.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "websockets" version))
        (sha256
         (base32
          "03s3ml6sbki24aajllf8aily0xzrn929zxi84p50zkkbikdd4raw"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f  ; Tests not included in release tarball.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-websockets-package-name-requirement
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Python package names use dot as separator.
             (substitute* "setup.py"
               (("websockets/extensions") "websockets.extensions")))))))
    (home-page "https://github.com/aaugustin/websockets")
    (synopsis
     "Python implementation of the WebSocket Protocol (RFC 6455 & 7692)")
    (description
     "@code{websockets} is a library for building WebSocket servers and clients
in Python with a focus on correctness and simplicity.

Built on top of @code{asyncio}, Python's standard asynchronous I/O framework,
it provides an elegant coroutine-based API.")
    (license license:bsd-3)))

(define-public python-selenium
  (package
    (name "python-selenium")
    (version "3.141.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "selenium" version))
       (sha256
        (base32
         "039hf9knvl4s3hp21bzwsp1g5ri9gxsh504dp48lc6nr1av35byy"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-urllib3))
    (home-page
     "https://github.com/SeleniumHQ/selenium/")
    (synopsis "Python bindings for Selenium")
    (description "Selenium enables web browser automation.
Selenium specifically provides infrastructure for the W3C WebDriver specification
— a platform and language-neutral coding interface compatible with all
major web browsers.")
    (license license:asl2.0)))

(define-public python-rapidjson
  (package
    (name "python-rapidjson")
    (version "0.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "python-rapidjson" version))
        (sha256
         (base32
          "18cl2dhx3gds5vg52jxmh9wjlbiy8dx06c3n482rfpdi9dzbv05d"))
        (modules '((guix build utils)))
        (snippet
         '(begin (delete-file-recursively "rapidjson") #t))))
    (build-system python-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--rj-include-dir="
                            (assoc-ref %build-inputs "rapidjson")
                            "/include/rapidjson"))
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (invoke "python" "setup.py" "build"
                     (string-append "--rj-include-dir="
                                    (assoc-ref %build-inputs "rapidjson")
                                    "/include/rapidjson"))))
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             ;; Some tests are broken.
             (delete-file "tests/test_base_types.py")
             (delete-file "tests/test_validator.py")
             (invoke "python" "-m" "pytest" "tests"))))))
    (native-inputs
     (list rapidjson python-pytest python-pytz))
    (home-page "https://github.com/python-rapidjson/python-rapidjson")
    (synopsis "Python wrapper around rapidjson")
    (description "This package provides a python wrapper around rapidjson.")
    (license license:expat)))

(define-public python-venusian
  (package
    (name "python-venusian")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "venusian" version))
       (sha256
        (base32 "0f7f67dkgxxcjfhpdd5frb9pszkf04lyzzpn5069q0xi89r2p17n"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-runner" ,python-pytest-runner)
       ("python-pytest-cov" ,python-pytest-cov)))
    (arguments '(#:test-target "pytest"))
    (home-page "https://docs.pylonsproject.org/projects/venusian")
    (synopsis "Library for deferring decorator actions")
    (description
     "Venusian is a library which allows framework authors to defer decorator
actions.  Instead of taking actions when a function (or class) decorator is
executed at import time, you can defer the action usually taken by the
decorator until a separate scan phase.")
    (license license:repoze)))

(define-public python-zope-deprecation
  (package
    (name "python-zope-deprecation")
    (version "4.4.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "zope.deprecation" version))
              (sha256
               (base32
                "1pz2cv7gv9y1r3m0bdv7ks1alagmrn5msm5spwdzkb2by0w36i8d"))))
    (build-system python-build-system)
    (native-inputs `())
    (propagated-inputs `())
    (home-page "https://zopedeprecation.readthedocs.io/")
    (synopsis "Function for marking deprecations")
    (description "The @code{zope.deprecation} module provides a function for
marking modules, classes, functions, methods and properties as deprecated,
displaying warnings when usaged in application code.")
    (license license:zpl2.1)))

(define-public python-translationstring
  (package
    (name "python-translationstring")
    (version "1.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "translationstring" version))
              (sha256
               (base32
                "0bdpcnd9pv0131dl08h4zbcwmgc45lyvq3pa224xwan5b3x4rr2f"))))
    (build-system python-build-system)
    (home-page "http://docs.pylonsproject.org/projects/translationstring")
    (synopsis "Internationalization tooling for the Pylons project")
    (description "This package provides a library used by various Pylons
project packages for internationalization (i18n) duties related to
translation.")
    (license license:repoze)))

(define-public python-plaster
  (package
    (name "python-plaster")
    (version "1.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "plaster" version))
              (sha256
               (base32
                "1hy8k0nv2mxq94y5aysk6hjk9ryb4bsd13g83m60hcyzxz3wflc3"))))
    (build-system python-build-system)
    (native-inputs
     (list python-pytest))
    (home-page "https://docs.pylonsproject.org/projects/plaster/en/latest/")
    (synopsis "Configuration loader for multiple config file formats")
    (description
     "Plaster is a loader interface around multiple config file formats.  It
exists to define a common API for applications to use when they wish to load
configuration.  The library itself does not aim to handle anything except a
basic API that applications may use to find and load configuration settings.
Any specific constraints should be implemented in a pluggable loader which can
be registered via an entrypoint.")
    (license license:repoze)))

(define-public python-plaster-pastedeploy
  (package
    (name "python-plaster-pastedeploy")
    (version "0.7")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "plaster_pastedeploy" version))
              (sha256
               (base32
                "1zg7gcsvc1kzay1ry5p699rg2qavfsxqwl17mqxzr0gzw6j9679r"))))
    (build-system python-build-system)
    (native-inputs
     (list python-pytest))
    (propagated-inputs
     (list python-plaster python-pastedeploy))
    (home-page "https://github.com/Pylons/plaster_pastedeploy")
    (synopsis "Plugin for python-plaster adding PasteDeploy syntax")
    (description
     "This plugin for @code{python-plaster} adds support for PasteDeploy
syntax, it provides a plaster @code{Loader} object that can parse ini files
according to the standard set by PasteDeploy")
    (license license:expat)))

(define-public python-hupper
  (package
    (name "python-hupper")
    (version "1.10.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "hupper" version))
              (sha256
               (base32
                "1nbc648d110jx6ziji980cdmzsd14p8fqrcarsdvr1vm5jvm2vyd"))))
    (build-system python-build-system)
    (arguments '(#:test-target "pytest"))
    (native-inputs
     (list python-pytest python-pytest-runner python-watchdog python-mock
           python-pytest-cov))
    (propagated-inputs
     (list python-pytz))
    (home-page "https://readthedocs.org/projects/hupper")
    (synopsis "Integrated process monitor tracking changes to imported Python files")
    (description
     "Hupper is an integrated process monitor that will track changes to any
imported Python files in sys.modules as well as custom paths.  When files are
changed the process is restarted.")
    (license license:expat)))

(define-public python-pyramid
  (package
    (name "python-pyramid")
    (version "1.10.4")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pyramid" version))
              (sha256
               (base32
                "0rkxs1ajycg2zh1c94xlmls56mx5m161sn8112skj0amza6cn36q"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-hupper
           python-plaster-pastedeploy
           python-translationstring
           python-venusian
           python-webob
           python-zope-deprecation
           python-zope-interface
           python-webtest
           python-zope-component
           python-plaster))
    (home-page "https://trypyramid.com/")
    (synopsis "Python web-framework suitable for small and large sites")
    (description
     "Pyramid makes it easy to write web applications.  From minimal
request/response web apps to larger, grown applications.")
    (license license:repoze)))

(define-public python-random-user-agent
  (package
    (name "python-random-user-agent")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "random_user_agent" version))
       (sha256
        (base32
         "04nhzdh2ki7ybhjrmghxci6hcm6i03vvin2q2ynj87fbr1pa534g"))))
    (build-system python-build-system)
    (home-page "https://github.com/Luqman-Ud-Din/random_user_agent")
    (synopsis "List of user agents")
    (description
     "This package provides a list of user agents, from a collection of more
than 326,000 known user-agents.  Users can pick a random one, or select one
based on filters.")
    (license license:expat)))

(define-public python-flask-restx
  (package
    (name "python-flask-restx")
    (version "0.5.1")
    (source
     ;; We fetch from the Git repo because there are no tests in the PyPI
     ;; archive.
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/python-restx/flask-restx")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18vrmknyxw6adn62pz3kr9kvazfgjgl4pgimdf8527fyyiwcqy15"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-aniso8601 python-flask python-jsonschema python-pytz))
    (native-inputs
     (list python-blinker
           python-faker
           python-pytest
           python-pytest-benchmark
           python-pytest-flask
           python-pytest-mock))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "pytest" "--benchmark-skip" "-k"
                     ;; Those tests need internet access
                     "not test_check and not test_valid_value_check \
and not test_override_app_level"))))))
    (home-page "https://github.com/python-restx/flask-restx")
    (synopsis
     "Framework for fast, easy and documented API development with Flask")
    (description
     "Flask-RESTX is an extension for Flask that adds support for quickly building
REST APIs.  Flask-RESTX encourages best practices with minimal setup.  If you are familiar
 with Flask, Flask-RESTX should be easy to pick up.  It provides a coherent collection of
decorators and tools to describe your API and expose its documentation properly using
Swagger.")
    (license license:bsd-3)))

(define-public python-flask-restplus
  (deprecated-package "python-flask-restplus" python-flask-restx))

(define-public python-flask-socketio
  (package
    (name "python-flask-socketio")
    (version "5.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Flask-SocketIO" version))
       (sha256
        (base32
         "09r2gpj2nbn72v2zaf6xsvlazln77pgqzp2pg2021nja47sijhsw"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-flask python-socketio))
    (arguments '(#:tests? #f))        ; Tests not included in release tarball.
    (home-page "https://github.com/miguelgrinberg/Flask-SocketIO/")
    (synopsis "Socket.IO integration for Flask applications")
    (description "Socket.IO integration for Flask applications")
    (license license:expat)))

(define-public python-manuel
  (package
    (name "python-manuel")
    (version "1.10.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "manuel" version))
        (sha256
         (base32
          "1bdzay7j70fly5fy6wbdi8fbrxjrrlxnxnw226rwry1c8a351rpy"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-six))
    (native-inputs
     (list python-zope-testing))
    (home-page "https://pypi.org/project/manuel/")
    (synopsis "Build tested documentation")
    (description
     "Manuel lets you mix and match traditional doctests with custom test syntax.")
    (license license:asl2.0)))

(define-public python-persistent
  (package
    (name "python-persistent")
    (version "4.6.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "persistent" version))
        (sha256
         (base32
          "0imm9ji03lhkpcfmhid7x5209ix8g2rlgki9ik1qxks4b8sm8gzq"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-cffi python-zope-interface))
    (native-inputs
     (list python-manuel python-zope-testrunner))
    (home-page "https://github.com/zopefoundation/persistent/")
    (synopsis "Translucent persistent objects")
    (description "This package contains a generic persistence implementation for
Python.  It forms the core protocol for making objects interact
\"transparently\" with a database such as the ZODB.")
    (license license:zpl2.1)))

(define-public python-btrees
  (package
    (name "python-btrees")
    (version "4.7.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "BTrees" version))
        (sha256
         (base32
          "0iiq0g9k1g6qgqq84q9h6639vlvzznk1rgdm0rfcnnqkbkmsbr3w"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-persistent python-zope-interface))
    (native-inputs
     (list python-persistent python-transaction python-zope-testrunner))
    (home-page "https://github.com/zopefoundation/BTrees")
    (synopsis "Scalable persistent object containers")
    (description
     "This package contains a set of persistent object containers built around a
modified BTree data structure.  The trees are optimized for use inside ZODB's
\"optimistic concurrency\" paradigm, and include explicit resolution of
conflicts detected by that mechanism.")
    (license license:zpl2.1)))

(define-public python-transaction
  (package
    (name "python-transaction")
    (version "3.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "transaction" version))
        (sha256
         (base32
          "0bdaks31bgfh78wnj3sij24bfysmqk25crsis6amz8kzrc0d82iv"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-zope-interface))
    (native-inputs
     (list python-coverage python-mock python-nose))
    (home-page "https://github.com/zopefoundation/transaction")
    (synopsis "Transaction management for Python")
    (description "This package contains a generic transaction implementation
for Python.  It is mainly used by the ZODB.")
    (license license:zpl2.1)))

(define-public python-robot-detection
  (package
    (name "python-robot-detection")
    (version "0.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "robot-detection" version))
        (sha256
         (base32
          "1xd2jm3yn31bnk1kqzggils2rxj26ylxsfz3ap7bhr3ilhnbg3rx"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f)) ; Tests not shipped in pypi release.
    (propagated-inputs (list python-six))
    (home-page "https://github.com/rory/robot-detection")
    (synopsis "Detect web crawlers")
    (description
     "@code{robot_detection} is a python module to detect if a given HTTP User
Agent is a web crawler.  It uses the list of registered robots from
@url{http://www.robotstxt.org}.")
    (license license:gpl3+)))

(define-public python-pysolr
  (package
    (name "python-pysolr")
    (version "3.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pysolr" version))
        (sha256
         (base32
          "1rj5jmscvxjwcmlfi6hmkj44l4x6n3ln5p7d8d18j566hzmmzw3f"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; Tests require network access.
    (propagated-inputs
     (list python-requests))
    (native-inputs
     (list python-setuptools-scm))
    (home-page "https://github.com/django-haystack/pysolr/")
    (synopsis "Lightweight python wrapper for Apache Solr")
    (description
     "This module provides an interface that queries the Apache Solr server
using a pure Python implementation.")
    (license license:bsd-3)))

(define-public python-pyjsparser
  (package
    (name "python-pyjsparser")
    (version "2.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyjsparser" version))
       (sha256
        (base32 "0ycmf9fsvwliqmm1n6sfz7x71y7i2kbfgn39d8lsbiccfxmxlq5y"))))
    (build-system python-build-system)
    (home-page "https://github.com/PiotrDabkowski/pyjsparser")
    (synopsis "Fast JavaScript parser")
    (description "This package provides a fast JavaScript parser (based on
esprima.js)")
    (license license:expat)))

(define-public python-js2py
  (package
    (name "python-js2py")
    (version "0.71")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Js2Py" version))
       (sha256
        (base32 "1kkzkys6dfcbdv51vqxr9cmak350ab4mmykb8dysx60lvl4i06x4"))))
    (build-system python-build-system)
    (arguments '(#:tests? #false)) ; none included
    (propagated-inputs
     (list python-pyjsparser python-six python-tzlocal))
    (home-page "https://github.com/PiotrDabkowski/Js2Py")
    (synopsis "JavaScript to Python translator")
    (description
     "This package provides a JavaScript to Python translator and a JavaScript
interpreter written in pure Python.")
    (license license:expat)))

(define-public python-http-ece
  (package
    (name "python-http-ece")
    (version "1.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/web-push-libs/encrypted-content-encoding")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0bp4cc0xc123i72h80ax3qz3ixfwx3j7pw343kc7i6kdvfi8klx7"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'change-directory
           (lambda _ (chdir "python") #t)))))
    (propagated-inputs
     (list python-cryptography))
    (native-inputs
     (list python-coverage python-flake8 python-mock python-nose))
    (home-page "https://github.com/web-push-libs/encrypted-content-encoding")
    (synopsis "Encrypted Content Encoding for HTTP")
    (description
     "This package provices a simple implementation of Encrypted Content
Encoding for HTTP.")
    (license license:expat)))

(define-public python-cloudscraper
  (package
    (name "python-cloudscraper")
    (version "1.2.58")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/VeNoMouS/cloudscraper")
             ;; Corresponds to 1.2.58
             (commit "f3a3d067ea8b5238e9a0948aed0c3fa0d9c29b96")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18fbp086imabjxly04rrchbf6n6m05bpd150zxbw7z2w3mjnpsqd"))
       (modules '((guix build utils)))
       (snippet
        '(with-directory-excursion "cloudscraper"
           (for-each delete-file
                     '("captcha/9kw.py"
                       "captcha/anticaptcha.py"))
           (substitute* "__init__.py"
             ;; Perhaps it's a joke, but don't promote proprietary software.
             (("([Th]is feature is not available) in the .*'" _ prefix)
              (string-append prefix ".'")))))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; XXX: Dependencies, that have not yet been packaged
         ;;      and cause an import error when included.
         (add-after 'unpack 'drop-unsupported-sources
           (lambda _
             (with-directory-excursion "cloudscraper"
               (for-each delete-file
                         '("interpreters/v8.py")))))
         (add-after 'unpack 'fix-references
           (lambda _
             (substitute* "cloudscraper/interpreters/nodejs.py"
               (("'node'")
                (string-append "'" (which "node") "'")))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest" "-vv"
                       "-k" "not test_getCookieString_challenge_js_challenge1_16_05_2020")))))))
    (inputs
     (list node))
    (propagated-inputs
     (list python-js2py
           python-polling2
           python-requests
           python-requests-toolbelt
           python-responses
           python-pyparsing-2.4.7))
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/venomous/cloudscraper")
    (synopsis "Cloudflare anti-bot bypass")
    (description
     "This module acts as a webbrowser solving Cloudflare's Javascript
challenges.")
    (license license:expat)))

(define-public python-imap-tools
  (package
    (name "python-imap-tools")
    (version "0.29.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "imap_tools" version))
        (sha256
          (base32
            "0x122jwpc74wwyw2rsv2fvh6p12y31019ndfr9717jzjkj2d3lhb"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))          ; tests require internet access
    (home-page "https://github.com/ikvk/imap_tools")
    (synopsis "Work with email and mailbox by IMAP")
    (description
      "This Python library provides tools to deal with email and mailboxes
over IMAP:

@itemize
@item Parsed email message attributes
@item Query builder for searching emails
@item Work with emails in folders (copy, delete, flag, move, seen)
@item Work with mailbox folders (list, set, get, create, exists, rename, delete, status)
@end itemize")
    (license license:asl2.0)))

(define-public python-giturlparse
  (package
    (name "python-giturlparse")
    (version "0.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "giturlparse" version))
        (sha256
         (base32 "0dxk7sqy8ibaqdl6jzxd0ac1p7shzp4p9f3lhkd7qw9h3llsp595"))))
    (build-system python-build-system)
    (home-page "https://github.com/nephila/giturlparse")
    (synopsis "Git URL parsing module")
    (description "This package provides a git URL parsing module which supports
parsing and rewriting of remote git URLs from various hosting providers.")
    (license license:asl2.0)))

(define-public python-hstspreload
  (package
    (name "python-hstspreload")
    (version "2020.10.20")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hstspreload" version))
       (sha256
        (base32
         "1qah80p2xlib1rhivvdj9v5y3girxrj7dwp1mnh8mwaj5wy32y8a"))))
    (build-system python-build-system)
    (home-page
     "https://github.com/sethmlarson/hstspreload")
    (synopsis
     "Chromium HSTS Preload list as a Python package")
    (description
     "@code{python-hstspreload} contains Chromium HSTS Preload list
as a Python package.")
    (license license:bsd-3)))

(define-public python-sanic
  (package
    (name "python-sanic")
    (version "20.12.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sanic" version))
       (sha256
        (base32
         "0axfc151s7nrykzypzciyvkxxrs5ayx8kxv4r620hjb9w3jjhfnp"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'use-recent-pytest
           ;; Allow using recent dependencies.
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "setup.py"
               (("pytest==5.2.1") "pytest")
               (("multidict>=5.0,<6.0") "multidict")
               (("httpx==0\\.15\\.4") "httpx"))
             #t))
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (invoke "pytest" "-vv" "./tests" "-k"
                     (string-append "not test_zero_downtime "
                                    "and not test_gunicorn_worker "
                                    "and not test_logo_")))))))
    (propagated-inputs
     (list python-aiofiles
           python-httptools
           python-httpx
           python-multidict
           python-ujson
           python-uvloop
           python-websockets))
    (native-inputs
     (list gunicorn
           python-beautifulsoup4
           python-hstspreload
           python-httpcore
           python-pytest
           python-pytest-cov
           python-pytest-benchmark
           python-pytest-sanic
           python-pytest-sugar
           python-pytest-asyncio
           python-urllib3
           python-uvicorn))
    (home-page
     "https://github.com/sanic-org/sanic/")
    (synopsis
     "Async Python web server/framework")
    (description
     "Sanic is a Python web server and web framework
that's written to go fast.  It allows the usage of the
@code{async/await} syntax added in Python 3.5, which makes
your code non-blocking and speedy.")
    (license license:expat)))

(define-public python-socketio
  (package
    (name "python-socketio")
    (version "5.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-socketio" version))
       (sha256
        (base32
         "14vhpxdn54lz54mhcqlgcks0ssbws9gd1y7ii16a2g3gpfdc531k"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-aiohttp
           python-bidict
           python-engineio
           python-requests
           python-websocket-client
           python-websockets))
    (arguments '(#:tests? #f))        ; Tests not included in release tarball.
    (home-page "https://github.com/miguelgrinberg/python-socketio/")
    (synopsis "Python Socket.IO server")
    (description
     "Python implementation of the Socket.IO realtime client and server.")
    (license license:expat)))

(define-public python-socks
  (package
    (name "python-socks")
    (version "1.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-socks" version))
       (sha256
        (base32
         "1n6xb18jy41ybgkmamakg6psp3qididd45qknxiggngaiibz43kx"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f                      ; tests not included
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest" "tests/" "-s"))
             #t)))))
    (propagated-inputs
     (list python-async-timeout python-curio python-trio))
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/romis2012/python-socks")
    (synopsis
     "Core proxy (SOCKS4, SOCKS5, HTTP tunneling) functionality for Python")
    (description
     "Socks is a library providing core proxy (SOCKS4, SOCKS5, HTTP tunneling)
 functionality.")
    (license license:asl2.0)))

(define-public python-azure-nspkg
  (package
    (name "python-azure-nspkg")
    (version "3.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "azure-nspkg" version ".zip"))
       (sha256
        (base32
         "1l4xwdh0fcnvrv0mzig4g2kgqkfbsy64zjm1ggc6grk3mykcxlz7"))))
    (build-system python-build-system)
    (native-inputs (list unzip))
    (home-page "https://github.com/Azure/azure-sdk-for-python")
    (synopsis "Azure namespace internals")
    (description
     "This package is an internal Azure namespace package.")
    (license license:expat)))

(define-public python-azure-storage-nspkg
  (package
    (name "python-azure-storage-nspkg")
    (version "3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "azure-storage-nspkg" version))
       (sha256
        (base32
         "049qcmgshz7dj9yaqma0fwcgbxwddgwyfcw4gmv45xfmaa3bwfvg"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-azure-nspkg))
    (home-page "https://github.com/Azure/azure-storage-python")
    (synopsis "Microsoft Azure Storage Namespace package")
    (description
     "This project provides a client library in Python that makes it easy to
communicate with Microsoft Azure Storage services.")
    (license license:expat)))

(define-public python-w3lib
  (package
    (name "python-w3lib")
    (version "1.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "w3lib" version))
       (sha256
        (base32
         "1pv02lvvmgz2qb61vz1jkjc04fgm4hpfvaj5zm4i3mjp64hd1mha"))))
    (build-system python-build-system)
    (native-inputs
     (list python-six))
    (home-page "https://github.com/scrapy/w3lib")
    (synopsis "Python library of web-related functions")
    (description
     "This is a Python library of web-related functions, such as: remove
comments, or tags from HTML snippets, extract base url from HTML snippets,
translate entities on HTML strings, among other things.")
    (license license:bsd-3)))

(define-public python-webcolors
  (package
    (name "python-webcolors")
    (version "1.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "webcolors" version))
       (sha256
        (base32 "1rkda75h2p65zx6r84c9mjavn4xpviqvqrklvdvcklapd5in1wvn"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "pytest")))))))
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/ubernostrum/webcolors")
    (synopsis "HTML/CSS color definitions library")
    (description "@code{python-webcolors} is a module for working with
HTML/CSS color definitions.  Normalizing and converting between the following
formats is supported.
@itemize
@item Specification-defined color names
@item Six-digit hexadecimal
@item Three-digit hexadecimal
@item Integer rgb() triplet
@item Percentage rgb() triplet
@end itemize
Only the RGB colorspace is supported.  Conversion to/from the HSL colorspace
can be handled by the @code{colorsys} module in the Python standard library.")
    (license license:bsd-3)))

(define-public python-woob
  (package
    (name "python-woob")
    (version "3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "woob" version))
       (sha256
        (base32 "09hpxy5zhn2b8li0xjf3zd7s46lawb0315p5mdcsci3bj3s4v1j7"))))
    (build-system python-build-system)
    ;; A small number of tests for optional applications fails due to missing
    ;; inputs.
    (arguments `(#:tests? #f))
    (propagated-inputs
     (list python-babel
           python-colorama
           python-cssselect
           python-dateutil
           python-feedparser
           python-html2text
           python-lxml
           python-pillow
           python-prettytable
           python-pyqt
           python-pyyaml
           python-requests
           python-six
           python-unidecode))
    (native-inputs
     (list python-coverage python-flake8 python-nose python-selenium
           python-xunitparser))
    (home-page "https://woob.tech/")
    (synopsis "Woob, Web Outside Of Browsers")
    (description "Woob is a collection of applications able to interact with
websites, without requiring the user to open them in a browser.  It also
provides well-defined APIs to talk to websites lacking one.")
    (license license:lgpl3+)))

(define-public python-flask-combo-jsonapi
  (package
    (name "python-flask-combo-jsonapi")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AdCombo/flask-combo-jsonapi")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07fhcjiyif80z1vyh35za29sqx1mmqh568jrbrrs675j4a797sj1"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'loosen-requirements
                    (lambda _
                      ;; Don't pin specific versions of dependencies.
                      (substitute* "requirements.txt"
                        (("^sqlalchemy[=<>].*") "sqlalchemy\n")
                        (("^marshmallow[=<>].*") "marshmallow\n")
                        (("^Flask[=<>].*") "Flask\n"))))
                  (replace 'check
                    (lambda _
                      (invoke "pytest" "-vv"))))))
    (propagated-inputs
     (list python-flask
           python-marshmallow
           python-marshmallow-jsonapi
           python-simplejson
           python-sqlalchemy
           python-apispec
           python-simplejson
           python-six))
    (native-inputs
     (list python-coverage python-coveralls python-pytest
           python-pytest-runner))
    (home-page "https://github.com/AdCombo/flask-combo-jsonapi")
    (synopsis "Flask extension to quickly create JSON:API 1.0 REST Web APIs")
    (description
     "Flask-COMBO-JSONAPI is a Python Flask extension for building REST Web APIs
compliant with the @uref{https://jsonapi.org, JSON:API 1.0} specification.

It tries to combine the power of Flask-Restless with the flexibility of
Flask-RESTful to quickly build APIs that fit the complexity of existing
real-life projects with legacy data and diverse storage providers.")
    (license license:expat)))

(define-public python-mwparserfromhell
  (package
    (name "python-mwparserfromhell")
    (version "0.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mwparserfromhell" version))
       (sha256
        (base32 "0zh9zaqbac18s7mivqk8am9xw97lfkgcj9hhxj0d4208pkqpkmqs"))))
    (build-system python-build-system)
    (native-inputs
     (list python-pytest python-pytest-runner))
    (home-page "https://github.com/earwig/mwparserfromhell")
    (synopsis "Python parser for MediaWiki wikicode")
    (description
     "The MediaWiki Parser From Hell is a python library package that provides
a parser for MediaWiki.

It exposes parses as normal string objects with additional methods giving
access to the special Wikicode features it contains (hyperlinks, tags,
templates…).  The parser doesn't interpolate strings at all, it remains at a
purely formal level.

Full documentation may be found at
@uref{https://mwparserfromhell.readthedocs.io, ReadTheDocs}")
    (license license:expat)))

(define-public python-tweepy
  (package
    (name "python-tweepy")
    (version "4.4.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/tweepy/tweepy")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0jl3j20iqvzqqw5q5ldval5wrc2pdx94zff3b6b87j51yjx3qjhr"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "python" "-m" "unittest" "discover" "tests")))))))
    (propagated-inputs
     (list python-aiohttp python-requests python-requests-oauthlib))
    (native-inputs
     (list python-vcrpy))
    (home-page "https://www.tweepy.org/")
    (synopsis "Twitter library for Python")
    (description "This package provides @code{Tweepy}, an easy-to-use Python
library for accessing the Twitter API.")
    (license license:expat)))
