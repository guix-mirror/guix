;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2016, 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Vijayalakshmi Vedantham <vijimay12@gmail.com>
;;; Copyright © 2019 Sam <smbaines8@gmail.com>
;;; Copyright © 2020, 2021, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Luis Felipe López Acevedo <luis.felipe.la@protonmail.com>
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

(define-module (gnu packages django)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix build-system python)
  #:use-module (guix deprecation)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages check)
  #:use-module (gnu packages finance)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages time)
  #:use-module (gnu packages xml))

(define-public python-django-4.0
  (package
    (name "python-django")
    (version "4.0.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "Django" version))
              (sha256
               (base32
                "0jlmxylag7dah9jl3wm2swnn9kbisx1gqnddfbh5kjifn67va3qi"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda* (#:key inputs #:allow-other-keys)
             ;; The test-suite tests timezone-dependent functions, thus tzdata
             ;; needs to be available.
             (setenv "TZDIR"
                     (search-input-directory inputs "share/zoneinfo"))

             ;; Disable test for incorrect timezone: it only raises the
             ;; expected error when /usr/share/zoneinfo exists, even though
             ;; the machinery gracefully falls back to TZDIR.  According to
             ;; django/conf/__init__.py, lack of /usr/share/zoneinfo is
             ;; harmless, so just ignore this test.
             (substitute* "tests/settings_tests/tests.py"
               ((".*def test_incorrect_timezone.*" all)
                (string-append "    @unittest.skipIf(True, 'Disabled by Guix')\n"
                               all)))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (if tests?
                 (with-directory-excursion "tests"
                   ;; Tests expect PYTHONPATH to contain the root directory.
                   (setenv "PYTHONPATH" "..")
                   (invoke "python" "runtests.py"
                           ;; By default tests run in parallel, which may cause
                           ;; various race conditions.  Run sequentially for
                           ;; consistent results.
                           "--parallel=1"))
                 (format #t "test suite not run~%"))))
         ;; XXX: The 'wrap' phase adds native inputs as runtime dependencies,
         ;; see <https://bugs.gnu.org/25235>.  The django-admin script typically
         ;; runs in an environment that has Django and its dependencies on
         ;; PYTHONPATH, so just disable the wrapper to reduce the size from
         ;; ~710 MiB to ~203 MiB.
         (delete 'wrap))))
    ;; TODO: Install extras/django_bash_completion.
    (native-inputs
     (list tzdata-for-tests
           ;; Remaining packages are test requirements taken from
           ;; tests/requirements/py3.txt
           python-docutils
           ;; optional for tests: ("python-geoip2" ,python-geoip2)
           ;; optional for tests: ("python-memcached" ,python-memcached)
           python-numpy
           python-pillow
           python-pyyaml
           ;; optional for tests: ("python-selenium" ,python-selenium)
           python-tblib))
    (propagated-inputs
     (list python-asgiref
           python-sqlparse
           ;; Optional dependencies.
           python-argon2-cffi
           python-bcrypt
           ;; This input is not strictly required, but in practice many Django
           ;; libraries need it for test suites and similar.
           python-jinja2))
    (native-search-paths
     ;; Set TZDIR when 'tzdata' is available so that timezone functionality
     ;; works (mostly) out of the box in containerized environments.
     ;; Note: This search path actually belongs to 'glibc'.
     (list (search-path-specification
            (variable "TZDIR")
            (files '("share/zoneinfo")))))
    (home-page "https://www.djangoproject.com/")
    (synopsis "High-level Python Web framework")
    (description
     "Django is a high-level Python Web framework that encourages rapid
development and clean, pragmatic design.  It provides many tools for building
any Web site.  Django focuses on automating as much as possible and adhering
to the @dfn{don't repeat yourself} (DRY) principle.")
    (license license:bsd-3)
    (properties `((cpe-name . "django")))))

(define-public python-django-3.2
  (package
    (inherit python-django-4.0)
    (version "3.2.12")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "Django" version))
              (sha256
               (base32
                "1qj1kvb6mk2f4b33n4n5l4rh5kqllrk2v0v076crxr83ay9ycwlp"))))
    (native-search-paths '())           ;no need for TZDIR
    (propagated-inputs
     (modify-inputs (package-propagated-inputs python-django-4.0)
       ;; Django 4.0 deprecated pytz in favor of Pythons built-in zoneinfo.
       (append python-pytz)))))

(define-public python-django-2.2
  (package
    (inherit python-django-3.2)
    (version "2.2.27")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "Django" version))
              (sha256
               (base32
                "04y9knxd8v9jn54ws5rbdwxyq5im69kx009s7gl62axzn1371qqy"))))
    (native-inputs
     (modify-inputs (package-native-inputs python-django-3.2)
       (prepend ;; 2.2 requires Selenium for the test suite.
                python-selenium)))))

;; Use 3.2 LTS as the default until packages gain support for 4.x.
(define-public python-django python-django-3.2)

(define-public python-django-extensions
  (package
    (name "python-django-extensions")
    (version "3.0.6")
    (source
     (origin
       (method git-fetch)
       ;; Fetch from the git repository, so that the tests can be run.
       (uri (git-reference
             (url "https://github.com/django-extensions/django-extensions")
             (commit version)))
       (file-name (string-append name "-" version))
       (sha256
        (base32
         "0sra6hazqvspxd1pnx5cj7gia1rkaz3hn06ib4wd0frc167f5afy"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ;XXX: requires a Postgres or MySQL database
    (propagated-inputs
     (list python-six python-vobject python-werkzeug python-dateutil
           python-django))
    (native-inputs
     (list python-mock
           python-factory-boy
           python-tox
           python-pytest
           python-pytest-cov
           python-pytest-django
           python-shortuuid))
    (home-page
     "https://github.com/django-extensions/django-extensions")
    (synopsis "Custom management extensions for Django")
    (description
     "Django-extensions extends Django providing, for example, management
commands, additional database fields and admin extensions.")
    (license license:expat)))

(define-public python-django-localflavor
  (package
    (name "python-django-localflavor")
    (version "3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "django-localflavor" version))
       (sha256
        (base32 "0i1s0ijfd9rv2cp5x174jcyjpwn7fyg7s1wpbvlwm96bpdvs6bxc"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (setenv "PYTHONPATH"
                       (string-append ".:"
                                      (getenv "GUIX_PYTHONPATH")))
               (invoke "invoke" "test")))))))
    (native-inputs
     (list python-coverage python-invoke python-pytest-django which))
    (propagated-inputs
     (list python-django python-stdnum))
    (home-page "https://django-localflavor.readthedocs.io/en/latest/")
    (synopsis "Country-specific Django helpers")
    (description "Django-LocalFlavor is a collection of assorted pieces of code
that are useful for particular countries or cultures.")
    (license license:bsd-3)))

(define-public python-django-simple-math-captcha
  (package
    (name "python-django-simple-math-captcha")
    (version "1.0.9")
    (home-page "https://github.com/alsoicode/django-simple-math-captcha")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0fhy9k8haqa1296v0qpg1b5w7y3pyw9qi9z9laj5ijry1gk35qaw"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-six-imports
                    (lambda _
                      ;; Django no longer bundles six, adjust the imports
                      ;; accordingly.  The six dependency can likely be
                      ;; removed in the next version.
                      (substitute* (find-files "." "\\.py$")
                        (("from django\\.utils import six")
                         "import six"))
                      #t))
                  (replace 'check
                    (lambda _
                      (invoke "python" "runtests.py"))))))
    (native-inputs
     (list python-mock))
    (propagated-inputs
     (list python-django python-six))
    (synopsis "Easy-to-use math field/widget captcha for Django forms")
    (description
     "A multi-value-field that presents a human answerable question,
with no settings.py configuration necessary, but instead can be configured
with arguments to the field constructor.")
    (license license:asl2.0)))

(define-public python-django-classy-tags
  (package
    (name "python-django-classy-tags")
    (version "2.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "django-classy-tags" version))
        (sha256
         (base32
          "1javam3zqi3y3j0r490mm61v48yh75jaha99gb7lsxkaz6yri7fm"))))
    (build-system python-build-system)
    ;; FIXME: How to make the test templates available to Django?
    (arguments '(#:tests? #f))
    (propagated-inputs
     (list python-django))
    (home-page "https://github.com/divio/django-classy-tags")
    (synopsis "Class based template tags for Django")
    (description
     "@code{django-classy-tags} is an approach at making writing template tags
in Django easier, shorter and more fun.  It provides an extensible argument
parser which reduces most of the boiler plate code you usually have to write
when coding custom template tags.")
    (license license:bsd-3)))

(define-public python-django-taggit
  (package
    (name "python-django-taggit")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "django-taggit" version))
       (sha256
        (base32
         "0bbkabbs77z229ps0800gxfhf75yagp4x4j5jzfysbac3zvkp0sa"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "python3" "-m" "django" "test" "--settings=tests.settings"))))))
    (propagated-inputs
     (list python-django python-isort))
    (native-inputs
     (list python-pytest python-mock))
    (home-page
     "https://github.com/jazzband/django-taggit")
    (synopsis
     "Reusable Django application for simple tagging")
    (description
     "Django-taggit is a reusable Django application for simple tagging.")
    (license license:bsd-3)))

(define-public python-easy-thumbnails
  (package
    (name "python-easy-thumbnails")
    (version "2.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "easy-thumbnails" version))
       (sha256
        (base32
         "14gzp5cv24z0qhxb7f7k7v9jgzpaj4n8yhjq83ynpx8183fs1rz4"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-django python-pillow))
    (home-page "https://github.com/SmileyChris/easy-thumbnails")
    (synopsis "Easy thumbnails for Django")
    (description
     "Easy thumbnails is a Django plugin to dynamically create thumbnails
based on source images.  Multiple thumbnails can be created from a single
source image, using different options to control parameters like the image
size and quality.")
    (license license:bsd-3)))

(define-public python-pytest-django
  (package
    (name "python-pytest-django")
    (version "4.4.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pytest-django" version))
              (sha256
               (base32
                "0mglnz0w6k7dgw1jn6giv56pmdjd6a3zwwkhxb2kyzmzk0viw5xm"))))
    (build-system python-build-system)
    (arguments
     ;; The test suite is disabled because there are many test failures (see:
     ;; https://github.com/pytest-dev/pytest-django/issues/943).
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             (if tests?
               (begin
                 (setenv "PYTEST_DJANGO_TEST_RUNNER" "pytest")
                 (setenv "DJANGO_SETTINGS_MODULE"
                         "pytest_django_test.settings_sqlite_file")
                 (invoke "python" "-m" "pytest" "-vv" "-k"
                         ;; FIXME: these tests fail to locate Django templates ...
                         (string-append "not test_django_not_loaded_without_settings"
                                        " and not test_settings"
                                        ;; ... and this does not discover
                                        ;; 'pytest_django_test'.
                                        " and not test_urls_cache_is_cleared")))
               (format #t "test suite not run~%")))))))
    (native-inputs
     (list python-django python-setuptools-scm
           ;; For tests.
           python-pytest-xdist-next))
    (propagated-inputs
     (list python-pytest))
    (home-page "https://pytest-django.readthedocs.org/")
    (synopsis "Django plugin for py.test")
    (description "Pytest-django is a plugin for py.test that provides a set of
useful tools for testing Django applications and projects.")
    (license license:bsd-3)))

(define-public python-django-haystack
  (package
    (name "python-django-haystack")
    (version "3.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "django-haystack" version))
        (sha256
         (base32
          "10kaa5641cakpra2x3jqgys085gdkjcyns26plfyrmfpjmmpa1bd"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'loosen-verion-restrictions
           (lambda _
             (substitute* "setup.py"
               (("geopy.*") "geopy\",\n"))))
         (add-before 'check 'set-gdal-lib-path
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "GDAL_LIBRARY_PATH"
                     (string-append (assoc-ref inputs "gdal")
                                    "/lib"))))
         ;; Importing this module requires setting up a Django project.
         (delete 'sanity-check))
       #:tests? #f)) ; OSError: libgdal.so.27: cannot open shared object file
    (propagated-inputs
     (list python-django))
    (native-inputs
     (list gdal
           python-coverage
           python-dateutil
           python-geopy
           python-mock
           python-nose
           python-requests
           python-setuptools-scm
           python-pysolr
           python-whoosh))
    (home-page "http://haystacksearch.org/")
    (synopsis "Pluggable search for Django")
    (description "Haystack provides modular search for Django.  It features a
unified, familiar API that allows you to plug in different search backends
(such as Solr, Elasticsearch, Whoosh, Xapian, etc.) without having to modify
your code.")
    (license license:bsd-3)))

(define-public python-django-filter
  (package
    (name "python-django-filter")
    (version "2.3.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "django-filter" version))
              (sha256
               (base32
                "1bz5qzdk9pk4a2lp2yacrdnqmkv24vxnz4k3lykrnpc3b7bkvrhi"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "python" "runtests.py"))))))
    (native-inputs
     (list python-django python-django-rest-framework
           python-django-crispy-forms python-mock))
    (home-page "https://django-filter.readthedocs.io/en/latest/")
    (synopsis "Reusable Django application to filter querysets dynamically")
    (description
     "Django-filter is a generic, reusable application to alleviate writing
some of the more mundane bits of view code.  Specifically, it allows users to
filter down a queryset based on a model’s fields, displaying the form to let
them do this.")
    (license license:bsd-3)))

(define-public python-django-allauth
  (package
    (name "python-django-allauth")
    (version "0.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "django-allauth" version))
       (sha256
        (base32
         "0c0x8izvrnjhrr48w6pwsfk9ddbi6yfxg7v3hh5dm1vz1d0hjwpi"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (setenv "DJANGO_SETTINGS_MODULE" "test_settings")
             (invoke "django-admin" "test" "allauth.tests"
                     "--pythonpath=."))))))
    (propagated-inputs
     (list python-openid python-requests python-requests-oauthlib))
    (native-inputs
     (list python-mock))
    (inputs
     (list python-django))
    (home-page "https://github.com/pennersr/django-allauth")
    (synopsis "Set of Django applications addressing authentication")
    (description
     "Integrated set of Django applications addressing authentication,
registration, account management as well as 3rd party (social)
account authentication.")
    (license license:expat)))

(define-public python-django-debug-toolbar
  (package
    (name "python-django-debug-toolbar")
    (version "3.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jazzband/django-debug-toolbar")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1m1j2sx7q0blma0miswj3c8hrfi5q4y5cq2b816v8gagy89xgc57"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-sqlparse python-django))
    (native-inputs
     (list python-django-jinja python-html5lib))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "make" "test"))))))
    (home-page
     "https://github.com/jazzband/django-debug-toolbar")
    (synopsis "Toolbar to help with developing Django applications")
    (description
     "A configurable set of panels that display information about the current
request and response as a toolbar on the rendered page.")
    (license license:bsd-3)))

(define-public python-django-debug-toolbar-alchemy
  (package
    (name "python-django-debug-toolbar-alchemy")
    (version "0.1.5")
    (home-page "https://github.com/miki725/django-debug-toolbar-alchemy")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "django-debug-toolbar-alchemy" version))
              (sha256
               (base32
                "1kmpzghnsc247bc1dl22s4y62k9ijgy1pjms227018h5a4frsa5b"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))          ;XXX: 'make check' does "echo TODO"
    (propagated-inputs
     (list python-django python-django-debug-toolbar python-jsonplus
           python-six python-sqlalchemy))
    (synopsis "Django Debug Toolbar panel for SQLAlchemy")
    (description
     "This package completely mimics the default Django Debug Toolbar SQL
panel (internally it is actually subclassed), but instead of displaying
queries done via the Django ORM, SQLAlchemy generated queries are displayed.")
    (license license:expat)))

(define-public python-django-gravatar2
  (package
    (name "python-django-gravatar2")
    (version "1.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "django-gravatar2" version))
       (sha256
        (base32
         "1vn921fb6jjx7rf5dzhy66rkb71nwmh9ydd0xs9ys72icw4jh4y8"))))
    (build-system python-build-system)
    (arguments
     '(;; TODO: The django project for the tests is missing from the release.
       #:tests? #f))
    (inputs
     (list python-django))
    (home-page "https://github.com/twaddington/django-gravatar")
    (synopsis "Gravatar support for Django, improved version")
    (description
     "Essential Gravatar support for Django.  Features helper methods,
templatetags and a full test suite.")
    (license license:expat)))

(define-public python-django-assets
  (package
    (name "python-django-assets")
    (version "2.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "django-assets" version))
              (sha256
               (base32
                "0fc6i77faxxv1gjlp06lv3kw64b5bhdiypaygfxh5djddgk83fwa"))))
    (build-system python-build-system)
    (native-inputs
     (list python-nose))
    (propagated-inputs
     (list python-django python-webassets))
    (home-page "https://github.com/miracle2k/django-assets")
    (synopsis "Asset management for Django")
    (description
      "Asset management for Django, to compress and merge CSS and Javascript
files.  Integrates the webassets library with Django, adding support for
merging, minifying and compiling CSS and Javascript files.")
    (license license:bsd-2)))

(define-public python-django-jinja
  (package
    (name "python-django-jinja")
    (version "2.9.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/niwinz/django-jinja")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0p9pkn6jjzagpnvcrl9c2vjqamkms7ymvyhhmaqqqhrlv89qnzp7"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-django python-jinja2 python-pytz python-django-pipeline))
    (arguments
     '(;; TODO Tests currently fail due to issues with the configuration for
       ;; django-pipeline
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (or
              (not tests?)
              (with-directory-excursion "testing"
                (invoke "python" "runtests.py"))))))))
    (home-page
     "https://niwinz.github.io/django-jinja/latest/")
    (synopsis "Simple jinja2 templating backend for Django")
    (description
     "This package provides a templating backend for Django, using Jinja2.  It
provides certain advantages over the builtin Jinja2 backend in Django, for
example, explicit calls to callables from templates and better performance.")
    (license license:bsd-3)))

(define-public python-dj-database-url
  (package
    (name "python-dj-database-url")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "dj-database-url" version))
              (sha256
               (base32
                "0qs16g5y3lflxibsl8gwkwap21crhmmv98l60rdq6x1wawgypsja"))))
    (build-system python-build-system)
    (home-page "https://github.com/kennethreitz/dj-database-url")
    (synopsis "Use Database URLs in your Django Application")
    (description
      "This simple Django utility allows you to utilize the 12factor inspired
DATABASE_URL environment variable to configure your Django application.

The dj_database_url.config method returns a Django database connection
dictionary, populated with all the data specified in your URL.  There is also a
conn_max_age argument to easily enable Django’s connection pool.")
    (license license:bsd-2)))

(define-public python-django-picklefield
  (package
    (name "python-django-picklefield")
    (version "3.0.1")
    (home-page "https://github.com/gintas/django-picklefield")
    ;; Use a git checkout because the PyPI release lacks tests.
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url home-page)
              (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0ni7bc86k0ra4pc8zv451pzlpkhs1nyil1sq9jdb4m2mib87b5fk"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda _
                      (invoke "python" "-m" "django" "test" "-v2"
                              "--settings=tests.settings"))))))
    (propagated-inputs
     ;; XXX: Picklefield has not been updated in 10+ years and fails tests
     ;; with Django 3.2.
     `(("python-django@2.2" ,python-django-2.2)))
    (synopsis "Pickled object field for Django")
    (description "Pickled object field for Django")
    (license license:expat)))

(define-public python-django-bulk-update
  (package
    (name "python-django-bulk-update")
    (version "2.2.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "django-bulk-update" version))
              (sha256
               (base32
                "0dxkmrm3skyw82i0qa8vklxw1ma1y308kh9w2hcnvhpacn5cxdss"))))
    (build-system python-build-system)
    (arguments
     ;; XXX: Tests require a Postgres database.
     `(#:tests? #f))
    (propagated-inputs
     (list python-django))
    (home-page "https://github.com/aykut/django-bulk-update")
    (synopsis "Simple bulk update over Django ORM or with helper function")
    (description
      "Simple bulk update over Django ORM or with helper function.  This
project aims to bulk update given objects using one query over Django ORM.")
    (license license:expat)))

(define-public python-django-contact-form
  (package
    (name "python-django-contact-form")
    (version "1.9")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "django-contact-form" version))
              (sha256
               (base32
                "1my9hkrylckp5vfqg9b0kncrdlxjnwxll56sdciqn4v19i4wbq1y"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "coverage" "run" "--source" "contact_form"
                     "runtests.py"))))))
    (native-inputs
     (list python-coverage))
    (propagated-inputs
     (list python-django))
    (home-page "https://github.com/ubernostrum/django-contact-form")
    (synopsis "Contact form for Django")
    (description
      "This application provides simple, extensible contact-form functionality
for Django sites.")
    (license license:bsd-3)))

(define-public python-django-contrib-comments
  (package
    (name "python-django-contrib-comments")
    (version "1.9.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "django-contrib-comments" version))
              (sha256
               (base32
                "0ccdiv784a5vnpfal36km4dyg12340rwhpr0riyy0k89wfnjn8yi"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-django python-six))
    (home-page "https://github.com/django/django-contrib-comments")
    (synopsis "Comments framework")
    (description
      "Django used to include a comments framework; since Django 1.6 it's been
separated to a separate project.  This is that project.  This framework can be
used to attach comments to any model, so you can use it for comments on blog
entries, photos, book chapters, or anything else.")
    (license license:bsd-3)))

(define-public python-django-pipeline
  (package
    (name "python-django-pipeline")
    (version "2.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "django-pipeline" version))
       (sha256
        (base32
         "19vrbd5s12qw4qlg5n8ldv7zz2rs5y2sdid1i7lvgp92m71dayvc"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* "tests/tests/test_compiler.py"
               (("\\/usr\\/bin\\/env")
                (which "env")))))
         (replace 'check
           (lambda*(#:key tests? #:allow-other-keys)
             (when tests?
               (setenv "DJANGO_SETTINGS_MODULE" "tests.settings")
               (invoke "django-admin" "test" "tests"
                       "--pythonpath=.")))))))
    (propagated-inputs
     (list python-css-html-js-minify python-django python-slimit
           python-jsmin))
    (home-page
     "https://github.com/jazzband/django-pipeline")
    (synopsis "Asset packaging library for Django")
    (description
     "Pipeline is an asset packaging library for Django, providing both CSS
and JavaScript concatenation and compression, built-in JavaScript template
support, and optional data-URI image and font embedding.")
    (license license:expat)))

(define-public python-django-redis
  (package
    (name "python-django-redis")
    (version "4.12.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "django-redis" version))
              (sha256
               (base32
                "0qvsm8yjchl0d3i7g20wba6px9lb5gv8kp3fcnr6hr0y0b3qjr9h"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "redis-server" "--daemonize" "yes")
             (with-directory-excursion "tests"
               (invoke "python" "runtests.py")))))))
    (native-inputs
     (list python-fakeredis python-hiredis python-mock python-msgpack
           redis))
    (propagated-inputs
     (list python-django python-redis))
    (home-page "https://github.com/niwibe/django-redis")
    (synopsis "Full featured redis cache backend for Django")
    (description
      "Full featured redis cache backend for Django.")
    (license license:bsd-3)))

(define-public python-django-rq
  (package
    (name "python-django-rq")
    (version "2.3.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "django-rq" version))
              (sha256
               (base32
                "0lksnjn3q3f7y72bj2yr8870w28a5b6x0vjnd9nhpq2ah6xfz6pf"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "redis-server" "--daemonize" "yes")
             (invoke "django-admin.py" "test" "django_rq"
                     "--settings=django_rq.tests.settings"
                     "--pythonpath=."))))))
    (native-inputs
     (list python-django-redis python-mock python-rq-scheduler redis))
    (propagated-inputs
     (list python-django python-rq))
    (home-page "https://github.com/ui/django-rq")
    (synopsis "Django integration with RQ")
    (description
      "Django integration with RQ, a Redis based Python queuing library.
Django-RQ is a simple app that allows you to configure your queues in django's
settings.py and easily use them in your project.")
    (license license:expat)))

(define-public python-django-q
  (package
    (name "python-django-q")
    (version "1.3.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "django-q" version))
        (sha256
         (base32 "03z1pf6wlf47i7afr79a8fiiidfk1vq19yaqnv0m4qdny7f58gaj"))))
    (build-system python-build-system)
    ;; FIXME: Tests require disque, Redis, MongoDB, Docker.
    (arguments '(#:tests? #f))
    (propagated-inputs
     (list python-arrow python-blessed python-django
           python-django-picklefield))
    (home-page "https://django-q.readthedocs.io/")
    (synopsis "Multiprocessing distributed task queue for Django")
    (description
     "Django Q is a native Django task queue, scheduler and worker application
using Python multiprocessing.")
    (license license:expat)))

(define-public python-django-sortedm2m
  (package
    (name "python-django-sortedm2m")
    (version "3.0.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "django-sortedm2m" version))
              (sha256
               (base32
                "0z0yymmrr2l5cznqbzwziw624df0qsiflvbpqwrpan52nww3dk4a"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda _
                      (invoke "django-admin"
                              "test" "--settings=test_project.settings"
                              "--pythonpath=."))))))
    (propagated-inputs
     (list python-django))
    (home-page "https://github.com/jazzband/django-sortedm2m")
    (synopsis "Drop-in replacement for django's own ManyToManyField")
    (description
      "Sortedm2m is a drop-in replacement for django's own ManyToManyField.
The provided SortedManyToManyField behaves like the original one but remembers
the order of added relations.")
    (license license:bsd-3)))

(define-public python-django-appconf
  (package
    (name "python-django-appconf")
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "django-appconf" version))
              (sha256
               (base32
                "101k8nkc7xlffpjdi2qbrp9pc4v8hzvmkzi12qp7vms39asxwn5y"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda _
                      (setenv "DJANGO_SETTINGS_MODULE" "tests.test_settings")
                      (invoke "django-admin" "test" "--pythonpath=."))))))
    (propagated-inputs
     (list python-django))
    (home-page "https://github.com/django-compressor/django-appconf")
    (synopsis "Handle configuration defaults of packaged Django apps")
    (description
      "This app precedes Django's own AppConfig classes that act as \"objects
[to] store metadata for an application\" inside Django's app loading mechanism.
In other words, they solve a related but different use case than
django-appconf and can't easily be used as a replacement.  The similarity in
name is purely coincidental.")
    (license license:bsd-3)))

(define-public python-django-statici18n
  (package
    (name "python-django-statici18n")
    (version "2.1.0")
    (home-page "https://github.com/zyegfryed/django-statici18n")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0x0xvfqd40is2ks43d65awgqkx3wk10lvdim15scvbjhkh301b6v"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda _
                      (setenv "PYTHONPATH" "./tests/test_project")
                      (setenv "DJANGO_SETTINGS_MODULE" "project.settings")
                      (invoke "pytest" "-vv"))))))
    (native-inputs
     (list python-pytest python-pytest-django))
    (propagated-inputs
     `(("python-django" ,python-django)
       ("django-appconf" ,python-django-appconf)))
    (synopsis "Generate JavaScript catalog to static files")
    (description
      "A Django app that provides helper for generating JavaScript catalog to
static files.")
    (license license:bsd-3)))

(define-public python-django-tagging
  (package
    (name "python-django-tagging")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "django-tagging" version))
       (sha256
        (base32
         "13afxx30chssclxzd9gqnvwm9qyrdpnlbs6iswdfa18phfj8zmi8"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (setenv "DJANGO_SETTINGS_MODULE" "tagging.tests.settings")
             (invoke "django-admin" "test" "--pythonpath=."))))))
    (inputs
     (list python-django))
    (home-page "https://github.com/Fantomas42/django-tagging")
    (synopsis "Generic tagging application for Django")
    (description "This package provides a generic tagging application for
Django projects, which allows association of a number of tags with any
@code{Model} instance and makes retrieval of tags simple.")
    (license license:bsd-3)))

(define-public python-django-rest-framework
  (package
    (name "python-django-rest-framework")
    (version "3.13.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/encode/django-rest-framework")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "11wfb156yin6mlgcdzfmi267jsq1cld131mxgd13aqsrj06zlray"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? inputs #:allow-other-keys)
             (if tests?
                 (invoke "python" "runtests.py")
                 (format #t "test suite not run~%")))))))
    (native-inputs
     (list python-pytest python-pytest-django tzdata-for-tests))
    (propagated-inputs
     (list python-django python-pytz))
    (home-page "https://www.django-rest-framework.org")
    (synopsis "Toolkit for building Web APIs with Django")
    (description
     "The Django REST framework is for building Web APIs with Django.  It
provides features like a Web-browsable API and authentication policies.")
    (license license:bsd-2)))

(define-public python-djangorestframework
  (deprecated-package "python-djangorestframework" python-django-rest-framework))

(define-public python-django-sekizai
  (package
    (name "python-django-sekizai")
    (version "2.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "django-sekizai" version))
        (sha256
         (base32
          "0vrkli625b5s1wldri3dyrfvqbxg7zxy2pg0rpjixw3b1ndz0ag8"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f)) ; Tests not included with release.
    (propagated-inputs
     (list python-django python-django-classy-tags python-six))
    (home-page "https://github.com/divio/django-sekizai")
    (synopsis "Template blocks for Django projects")
    (description "Sekizai means blocks in Japanese, and that is what this app
provides.  A fresh look at blocks.  With @code{django-sekizai} you can define
placeholders where your blocks get rendered and at different places in your
templates append to those blocks.  This is especially useful for css and
javascript.  Your subtemplates can now define css and javscript files to be
included, and the css will be nicely put at the top and the javascript to the
bottom, just like you should.  Also sekizai will ignore any duplicate content in
a single block.")
    (license license:bsd-3)))

(define-public python-django-crispy-forms
  (package
    (name "python-django-crispy-forms")
    (version "1.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "django-crispy-forms" version))
       (sha256
        (base32
         "0fxlf233f49hjax786p4r650rd0ilvhnpyvw8hv1d1aqnkxy1wgj"))))
    (build-system python-build-system)
    (arguments
     '(;; No included tests
       #:tests? #f))
    (propagated-inputs
     (list python-django))
    (home-page
     "http://github.com/maraujop/django-crispy-forms")
    (synopsis "Tool to control Django forms without custom templates")
    (description
     "@code{django-crispy-forms} lets you easily build, customize and reuse
forms using your favorite CSS framework, without writing template code.")
    (license license:expat)))

(define-public python-django-compressor
  (package
    (name "python-django-compressor")
    (version "2.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "django_compressor" version))
        (sha256
         (base32 "1q0m0hfg7sqmj5km924g4dgy3nx51aszzsprlp6gsin10mv0fn1k"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (if tests?
               (begin
                 (setenv "DJANGO_SETTINGS_MODULE" "compressor.test_settings")
                 (invoke "django-admin" "test"
                         "--pythonpath=."))
               #t))))
       ;; Tests fail with beautifulsoup 4.9+
       ;; https://github.com/django-compressor/django-compressor/issues/998
       #:tests? #f))
    (propagated-inputs
     (list python-django-appconf python-rcssmin python-rjsmin))
    (native-inputs
     (list python-beautifulsoup4 python-brotli python-csscompressor
           python-django-sekizai python-mock))
    (home-page "https://django-compressor.readthedocs.io/en/latest/")
    (synopsis
     "Compress linked and inline JavaScript or CSS into single cached files")
    (description
     "Django Compressor combines and compresses linked and inline Javascript or
CSS in a Django templates into cacheable static files by using the compress
template tag.")
    (license license:expat)))

(define-public python-django-override-storage
  (package
    (name "python-django-override-storage")
    (version "0.3.0")
    (home-page "https://github.com/danifus/django-override-storage")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "081kzfk7mmybhihvc92d3hsdg0r2k20ydq88fs1fgd348sq1ax51"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda _
                      (invoke "python" "runtests.py"))))))
    (native-inputs
     (list python-mock))
    (propagated-inputs
     (list python-django))
    (synopsis "Django test helpers to manage file storage side effects")
    (description
     "This project provides tools to help reduce the side effects of using
FileFields during tests.")
    (license license:expat)))

(define-public python-django-auth-ldap
  (package
    (name "python-django-auth-ldap")
    (version "4.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "django-auth-ldap" version))
              (sha256
               (base32
                "0fajn4bk7m1hk0mjz97q7vlfzh7ibzv8f4qn7zhkq26f4kk7jvr7"))))
    (build-system python-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'build
                 (lambda _
                   ;; Set file modification times to the early 80's because
                   ;; the Zip format does not support earlier timestamps.
                   (setenv "SOURCE_DATE_EPOCH"
                           (number->string (* 10 366 24 60 60)))
                   (invoke "python" "-m" "build" "--wheel"
                           "--no-isolation" ".")))
               (replace 'check
                 (lambda* (#:key inputs #:allow-other-keys)
                   (setenv "SLAPD" (search-input-file inputs "/libexec/slapd"))
                   (setenv "SCHEMA"
                           (search-input-directory inputs "etc/openldap/schema"))
                   (invoke "python" "-m" "django" "test"
                           "--settings" "tests.settings")))
               (replace 'install
                 (lambda _
                   (let ((whl (car (find-files "dist" "\\.whl$"))))
                     (invoke "pip" "--no-cache-dir" "--no-input"
                             "install" "--no-deps" "--prefix" #$output whl)))))))
    (native-inputs
     (list openldap-2.6 python-wheel python-setuptools-scm python-toml

           ;; These can be removed after <https://bugs.gnu.org/46848>.
           python-pypa-build python-pip))
    (propagated-inputs
     (list python-django python-ldap))
    (home-page "https://github.com/django-auth-ldap/django-auth-ldap")
    (synopsis "Django LDAP authentication backend")
    (description
     "This package provides an LDAP authentication backend for Django.")
    (license license:bsd-2)))

(define-public python-django-logging-json
  (package
    (name "python-django-logging-json")
    (version "1.15")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "django-logging-json" version))
              (sha256
               (base32
                "06041a8icazzp73kg93c7k1ska12wvkq7fpcad0l0sm1qnxx5yx7"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f                      ;no tests
       #:phases (modify-phases %standard-phases
                  ;; Importing this module requires a Django project.
                  (delete 'sanity-check))))
    (propagated-inputs
     (list python-certifi python-django python-elasticsearch python-six))
    (home-page "https://github.com/cipriantarta/django-logging")
    (synopsis "Log requests/responses in various formats")
    (description
     "This package provides a Django library that logs request, response,
and exception details in a JSON document.  It can also send logs directly
to ElasticSearch.")
    (license license:bsd-2)))

(define-public python-django-netfields
  (package
    (name "python-django-netfields")
    (version "1.2.4")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "django-netfields" version))
              (sha256
               (base32
                "0jwlbyaxk91fq69g2y0zpfjgmjgh6l0lqm5mhys7m5968lkihvgp"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))      ;XXX: Requires a running PostgreSQL server
    (propagated-inputs
     (list python-django python-netaddr python-psycopg2 python-six))
    (home-page "https://github.com/jimfunk/django-postgresql-netfields")
    (synopsis "PostgreSQL netfields implementation for Django")
    (description
     "This package provides mappings for the PostgreSQL @code{INET} and
@code{CIDR} fields for use in Django projects.")
    (license license:bsd-3)))

(define-public python-django-url-filter
  (package
    (name "python-django-url-filter")
    (version "0.3.15")
    (home-page "https://github.com/miki725/django-url-filter")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0r4zhqhs8y6cnplwyvcb0zpijizw1ifnszs38n4w8138657f9026"))
              (modules '((guix build utils)))
              (snippet
               ;; Patch for Django 4.0 compatibility, taken from upstream pull
               ;; request: https://github.com/miki725/django-url-filter/pull/103
               '(substitute* "url_filter/validators.py"
                  ((" ungettext_lazy")
                   " ngettext_lazy")))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f            ;FIXME: Django raises "Apps aren't loaded yet"!?
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'loosen-requirements
                    (lambda _
                      ;; Do not depend on compatibility package for old
                      ;; Python versions.
                      (substitute* "requirements.txt"
                        (("enum-compat") ""))))
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (if tests?
                          (begin
                            (setenv "DJANGO_SETTINGS_MODULE"
                                    "test_project.settings")
                            (invoke "pytest" "-vv" "--doctest-modules"
                                    "tests/" "url_filter/"))
                          (format #t "test suite not run~%")))))))
    (propagated-inputs
     (list python-cached-property python-django python-six))
    (synopsis "Filter data via human-friendly URLs")
    (description
     "The main goal of Django URL Filter is to provide an easy URL interface
for filtering data.  It allows the user to safely filter by model attributes
and also specify the lookup type for each filter (very much like
Django's filtering system in ORM).")
    (license license:expat)))

(define-public python-django-svg-image-form-field
  (package
    (name "python-django-svg-image-form-field")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/artrey/django-svg-image-form-field")
             (commit (string-append version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "131m545khn8l20j4x2bvlvz36dlbnhj9pc98i2dw72s3bw8pgws0"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-defusedxml python-django python-pillow))
    (home-page "https://github.com/artrey/django-svg-image-form-field")
    (synopsis "Form field to validate SVG and other images")
    (description "This form field allows users to provide SVG images for
models that use Django's standard @code{ImageField}, in addition to the
image files already supported by it.")
    (license license:expat)))
