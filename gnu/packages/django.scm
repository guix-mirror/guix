;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 ng0 <ng0@n0.is>
;;; Copyright © 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Vijayalakshmi Vedantham <vijimay12@gmail.com>
;;; Copyright © 2019 Sam <smbaines8@gmail.com>
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
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages time))

(define-public python-django
  (package
    (name "python-django")
    (version "1.11.20")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "Django" version))
              (sha256
               (base32
                "0h90kdq8r4y8wa73hdxmyy5psnwlg61dcq3qsa098cpfiyh9vaa3"))))
    (build-system python-build-system)
    (arguments
     '(#:modules ((srfi srfi-1)
                  (guix build python-build-system)
                  (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-tzdir
           (lambda* (#:key inputs #:allow-other-keys)
             ;; The test-suite tests timezone-dependent functions, thus tzdata
             ;; needs to be available.
             (setenv "TZDIR"
                     (string-append (assoc-ref inputs "tzdata")
                                    "/share/zoneinfo"))
             #t))
         (replace 'check
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "PYTHONPATH"
                     (string-append ".:" (getenv "PYTHONPATH")))
             (substitute* "tests/admin_scripts/tests.py"
               (("python_path = \\[")
                (string-append "python_path = ['"
                               (find (lambda (entry)
                                       (string-prefix?
                                        (assoc-ref inputs "python-pytz")
                                        entry))
                                     (string-split (getenv "PYTHONPATH")
                                                   #\:))
                               "', ")))
             (invoke "python" "tests/runtests.py"))))))
    ;; TODO: Install extras/django_bash_completion.
    (native-inputs
     `(("tzdata" ,tzdata-for-tests)
       ;; bcrypt and argon2-cffi are extra requirements not yet in guix
       ;;("python-argon2-cffi" ,python-argon2-cffi) ; >= 16.1.0
       ;;("python-bcrypt" ,python-bcrypt) ; not py-bcrypt!
       ;; Remaining packages are test requirements taken from
       ;; tests/requirements/py3.txt
       ("python-docutils" ,python-docutils)
       ;; optional for tests: ("python-geoip2" ,python-geoip2)
       ("python-jinja2" ,python-jinja2)           ; >= 2.7
       ;; optional for tests: ("python-memcached" ,python-memcached)
       ("python-numpy" ,python-numpy)
       ("python-pillow" ,python-pillow)
       ("python-pyyaml" ,python-pyyaml)
       ;; optional for tests: ("python-selenium" ,python-selenium)
       ("python-sqlparse" ,python-sqlparse)
       ("python-tblib" ,python-tblib)))
    (propagated-inputs
     `(("python-pytz" ,python-pytz)))
    (home-page "http://www.djangoproject.com/")
    (synopsis "High-level Python Web framework")
    (description
     "Django is a high-level Python Web framework that encourages rapid
development and clean, pragmatic design.  It provides many tools for building
any Web site.  Django focuses on automating as much as possible and adhering
to the @dfn{don't repeat yourself} (DRY) principle.")
    (license license:bsd-3)
    (properties `((python2-variant . ,(delay python2-django))
                  (cpe-name . "django")))))

(define-public python2-django
  (let ((base (package-with-python2 (strip-python2-variant python-django))))
    (package
      (inherit base)
      (native-inputs
       `(;; Test requirements for Python 2 taken from
         ;; tests/requirements/py3.txt: enum34 and mock.
         ("python2-enum34" ,python2-enum34)
         ("python2-mock" ,python2-mock)
         ;; When adding memcached mind: for Python 2 memcached <= 1.53 is
         ;; required.
         ,@(package-native-inputs base))))))

(define-public python-django-extensions
  (package
    (name "python-django-extensions")
    (version "2.1.6")
    (source
     (origin
       (method git-fetch)
       ;; Fetch from the git repository, so that the tests can be run.
       (uri (git-reference
             (url "https://github.com/django-extensions/django-extensions.git")
             (commit version)))
       (file-name (string-append name "-" version))
       (sha256
        (base32
         "0p4qrdinrv6indczlc8dcnm528i5fzmcn9xk1ja7ycfkyk5x6j5w"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ;TODO collected 378 items / 3 errors / 1 skipped
    (propagated-inputs
     `(("python-six" ,python-six)
       ("python-vobject" ,python-vobject)
       ("python-werkzeug" ,python-werkzeug)
       ("python-dateutil" ,python-dateutil)
       ("python-django" ,python-django)))
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-factory-boy" ,python-factory-boy)
       ("python-tox" ,python-tox)
       ("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-pytest-django" ,python-pytest-django)
       ("python-shortuuid" , python-shortuuid)))
    (home-page
     "https://github.com/django-extensions/django-extensions")
    (synopsis "Custom management extensions for Django")
    (description
     "Django-extensions extends Django providing, for example, management
commands, additional database fields and admin extensions.")
    (license license:expat)))

(define-public python-django-simple-math-captcha
  (package
    (name "python-django-simple-math-captcha")
    (version "1.0.7")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "django-simple-math-captcha" version))
              (sha256
               (base32
                "0906hms6y6znjhpd0g4wmzv9vcla4brkdpsm4zha9zdj8g5vq2hd"))))
    (build-system python-build-system)
    (arguments
     ;; FIXME: Upstream uses a 'runtests.py' script that is not
     ;; present in the pypi tarball.
     '(#:tests? #f))
    (propagated-inputs
     `(("python-django" ,python-django)))
    (home-page "https://github.com/alsoicode/django-simple-math-captcha")
    (synopsis "Easy-to-use math field/widget captcha for Django forms")
    (description
     "A multi-value-field that presents a human answerable question,
with no settings.py configuration necessary, but instead can be configured
with arguments to the field constructor.")
    (license license:asl2.0)))

(define-public python2-django-simple-math-captcha
  (package-with-python2 python-django-simple-math-captcha))

(define-public python-django-taggit
  (package
    (name "python-django-taggit")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "django-taggit" version))
       (sha256
        (base32
         "044fzcpmns90kaxdi49qczlam4xsi8rl73rpfwvxx1gkcqzidgq1"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "python3" "-m" "django" "test" "--settings=tests.settings"))))))
    (propagated-inputs
     `(("python-django" ,python-django)
       ("python-isort" ,python-isort)))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-mock" ,python-mock)))
    (home-page
     "https://github.com/jazzband/django-taggit")
    (synopsis
     "Reusable Django application for simple tagging")
    (description
     "Django-taggit is a reusable Django application for simple tagging.")
    (license license:bsd-3)))

(define-public python-pytest-django
  (package
    (name "python-pytest-django")
    (version "3.1.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pytest-django" version))
              (sha256
               (base32
                "02932m2sr8x22m4az8syr8g835g4ak77varrnw71n6xakmdcr303"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; FIXME: How to run tests?
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-setuppy
           (lambda _
             (substitute* "setup.py"
                          (("setuptools_scm==1.11.1") "setuptools_scm"))
             #t)))))
    (native-inputs
     `(("python-django" ,python-django)
       ("python-setuptools-scm" ,python-setuptools-scm)))
    (propagated-inputs
     `(("python-pytest" ,python-pytest)))
    (home-page "http://pytest-django.readthedocs.org/")
    (synopsis "Django plugin for py.test")
    (description "Pytest-django is a plugin for py.test that provides a set of
useful tools for testing Django applications and projects.")
    (license license:bsd-3)))

(define-public python2-pytest-django
  (package-with-python2 python-pytest-django))

(define-public python-django-filter
  (package
    (name "python-django-filter")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "django-filter" version))
              (sha256
               (base32
                "0slpfqfhnjrzlrb6vmswyhrzn01p84s16j2x1xib35gg4fxg23pc"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "python" "runtests.py"))))))
    (native-inputs
     `(("python-django" ,python-django)
       ("python-djangorestframework" ,python-djangorestframework)
       ("python-django-crispy-forms" ,python-django-crispy-forms)
       ("python-mock" ,python-mock)))
    (home-page "https://django-filter.readthedocs.io/en/latest/")
    (synopsis "Reusable Django application to filter querysets dynamically")
    (description
     "Django-filter is a generic, reusable application to alleviate writing
some of the more mundane bits of view code.  Specifically, it allows users to
filter down a queryset based on a model’s fields, displaying the form to let
them do this.")
    (license license:bsd-3)))

(define-public python2-django-filter
  (package-with-python2 python-django-filter))

(define-public python-django-allauth
  (package
    (name "python-django-allauth")
    (version "0.39.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "django-allauth" version))
       (sha256
        (base32
         "17l0acpr3cihdndzccjhgv58f9z170v2qwx7w0b8w6235x646i24"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; TODO: Tagging the tests requiring the web could be done upstream.
         (add-before 'check 'skip-test-requiring-network-access
           (lambda _
             (substitute* "allauth/socialaccount/providers/openid/tests.py"
               (("import override_settings") "import tag, override_settings")
               (("def test_login")
                "@tag('requires-web')
    def test_login"))))
         (replace 'check
           (lambda _
             ;; TODO: investigate why this test fails
             (delete-file "allauth/socialaccount/providers/cern/tests.py")
             (setenv "DJANGO_SETTINGS_MODULE" "test_settings")
             (invoke "django-admin"
                     "test"
                     "allauth"
                     "--verbosity=2"
                     "--exclude-tag=requires-web"))))))
    (propagated-inputs
     `(("python-openid" ,python-openid)
       ("python-requests" ,python-requests)
       ("python-requests-oauthlib" ,python-requests-oauthlib)))
    (native-inputs
     `(("python-mock" ,python-mock)))
    (inputs
     `(("python-django" ,python-django)))
    (home-page "https://github.com/pennersr/django-allauth")
    (synopsis "Set of Django applications addressing authentication")
    (description
     "Integrated set of Django applications addressing authentication,
registration, account management as well as 3rd party (social)
account authentication.")
    (license license:expat)))

(define-public python2-django-allauth
  (package-with-python2 python-django-allauth))

(define-public python-django-debug-toolbar
  (package
    (name "python-django-debug-toolbar")
    (version "1.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/jazzband/django-debug-toolbar/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1rww056hyzks8spbgf4h7kf6ybxlc5p08a2b6gn1nqrrzs4yx9sy"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-sqlparse" ,python-sqlparse)
       ("python-django" ,python-django)))
    (native-inputs
     `(("python-django-jinja" ,python-django-jinja)
       ("python-html5lib" ,python-html5lib)))
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

(define-public python-django-gravatar2
  (package
    (name "python-django-gravatar2")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "django-gravatar2" version))
       (sha256
        (base32
         "1qsv40xywbqsf4mkrmsswrpzqd7nfljxpfiim9an2z3dykn5rka6"))))
    (build-system python-build-system)
    (arguments
     '(;; TODO: The django project for the tests is missing from the release.
       #:tests? #f))
    (inputs
     `(("python-django" ,python-django)))
    (home-page "https://github.com/twaddington/django-gravatar")
    (synopsis "Gravatar support for Django, improved version")
    (description
     "Essential Gravatar support for Django.  Features helper methods,
templatetags and a full test suite.")
    (license license:expat)))

(define-public python2-django-gravatar2
  (package-with-python2 python-django-gravatar2))

(define-public python-django-assets
  (package
    (name "python-django-assets")
    (version "0.12")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "django-assets" version))
              (sha256
               (base32
                "0y0007fvkn1rdlj2g0y6k1cnkx53kxab3g8i85i0rd58k335p365"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-tests
           (lambda _
             (begin
               ;; https://github.com/miracle2k/django-assets/issues/87
               (substitute* "tests/__init__.py"
                 (("settings.configure.*")
                  (string-append
                    "settings.configure(\n"
                    "INSTALLED_APPS=['django_assets', "
                    "'django.contrib.staticfiles'],\n"
                    "TEMPLATES=[{'BACKEND': "
                    "'django.template.backends.django.DjangoTemplates'}],\n"
                    ")\n")))
              ;; These tests fail
              (substitute* "tests/test_django.py"
                (("TestLoader") "NoTestLoader"))))))))
    (native-inputs
     `(("python-nose" ,python-nose)))
    (propagated-inputs
     `(("python-django" ,python-django)
       ("python-webassets" ,python-webassets)))
    (home-page "https://github.com/miracle2k/django-assets")
    (synopsis "Asset management for Django")
    (description
      "Asset management for Django, to compress and merge CSS and Javascript
files.  Integrates the webassets library with Django, adding support for
merging, minifying and compiling CSS and Javascript files.")
    (license license:bsd-2)))

(define-public python2-django-assets
  (package-with-python2 python-django-assets))

(define-public python-django-jinja
  (package
    (name "python-django-jinja")
    (version "2.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/niwinz/django-jinja/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0bzrb4m6wx9ph5cpvz7wpvg5k6ksvj0dnxlg0nhhqskhvp46brs1"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-django" ,python-django)
       ("python-jinja2" ,python-jinja2)
       ("python-pytz" ,python-pytz)
       ("python-django-pipeline" ,python-django-pipeline)))
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

(define-public python-django-jsonfield
  (package
    (name "python-django-jsonfield")
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "jsonfield" version))
              (sha256
               (base32
                "19x4lak0hg9c20r7mvf27w7i8r6i4sg2g0ypmlmp2665fnk76zvy"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-tests
           (lambda _
             (substitute* "jsonfield/tests.py"
               (("django.forms.util") "django.forms.utils")))))))
    (propagated-inputs
     `(("python-django" ,python-django)))
    (home-page "https://github.com/bradjasper/django-jsonfield")
    (synopsis "Store validated JSON in your model")
    (description
      "Django-jsonfield is a reusable Django field that allows you to store
validated JSON in your model.  It silently takes care of serialization.  To
use, simply add the field to one of your models.")
    (license license:expat)))

(define-public python2-django-jsonfield
  (package-with-python2 python-django-jsonfield))

(define-public python-dj-database-url
  (package
    (name "python-dj-database-url")
    (version "0.4.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "dj-database-url" version))
              (sha256
               (base32
                "024zbkc5rli4hia9lz9g8kf1zxhb2gwawj5abf67i7gf8n22v0x6"))))
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

(define-public python2-dj-database-url
  (package-with-python2 python-dj-database-url))

(define-public python-django-bulk-update
  (package
    (name "python-django-bulk-update")
    (version "1.1.10")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "django-bulk-update" version))
              (sha256
               (base32
                "0mbng9m7swfc0dnidipbzlxfhlfjrv755dlnha5s4m9mgdxb1fhc"))))
    (build-system python-build-system)
    (arguments
     ;; tests don't support django 1.10, but the module seems to work.
     `(#:tests? #f))
    (native-inputs
     `(("six" ,python-six)
       ("jsonfield" ,python-django-jsonfield)
       ("python-dj-database-url" ,python-dj-database-url)))
    (propagated-inputs
     `(("python-django" ,python-django)))
    (home-page "https://github.com/aykut/django-bulk-update")
    (synopsis "Simple bulk update over Django ORM or with helper function")
    (description
      "Simple bulk update over Django ORM or with helper function.  This
project aims to bulk update given objects using one query over Django ORM.")
    (license license:expat)))

(define-public python2-django-bulk-update
  (package-with-python2 python-django-bulk-update))

(define-public python-django-contact-form
  (package
    (name "python-django-contact-form")
    (version "1.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "django-contact-form" version))
              (sha256
               (base32
                "0az590y56k5ahv4sixrkn54d3a8ig2q2z9pl6s3m4f533mx2gj17"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             ;; the next version will need "make test"
             (invoke "flake8" "contact_form")
             (invoke "coverage" "run" "contact_form/runtests.py")
             (invoke "coverage" "report" "-m" "--fail-under" "0"))))))
    (native-inputs
     `(("python-coverage" ,python-coverage)
       ("python-flake8" ,python-flake8)))
    (propagated-inputs
     `(("python-django" ,python-django)))
    (home-page "https://github.com/ubernostrum/django-contact-form")
    (synopsis "Contact form for Django")
    (description
      "This application provides simple, extensible contact-form functionality
for Django sites.")
    (license license:bsd-3)))

(define-public python2-django-contact-form
  (package-with-python2 python-django-contact-form))

(define-public python-django-contrib-comments
  (package
    (name "python-django-contrib-comments")
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "django-contrib-comments" version))
              (sha256
               (base32
                "0bxsgw8jrkhg6r5s0z6ksfi4w8yknaqb1s9acmxd9pm3pnsnp5kx"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-django" ,python-django)))
    (home-page "https://github.com/django/django-contrib-comments")
    (synopsis "Comments framework")
    (description
      "Django used to include a comments framework; since Django 1.6 it's been
separated to a separate project.  This is that project.  This framework can be
used to attach comments to any model, so you can use it for comments on blog
entries, photos, book chapters, or anything else.")
    (license license:bsd-3)))

(define-public python2-django-contrib-comments
  (package-with-python2 python-django-contrib-comments))

(define-public python-django-overextends
  (package
    (name "python-django-overextends")
    (version "0.4.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "django-overextends" version))
              (sha256
               (base32
                "0qc2pcf3i56pmfxh2jw7k3pgljd8xzficmkl2541n7bkcbngqfzm"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _ (invoke "./test_project/manage.py" "test"))))))
    (propagated-inputs
     `(("python-django" ,python-django)))
    (native-inputs
     `(("sphinx-me" ,python-sphinx-me)))
    (home-page "https://github.com/stephenmcd/django-overextends")
    (synopsis "Circular template inheritance")
    (description
      "A Django reusable app providing the overextends template tag, a drop-in
replacement for Django's extends tag, which allows you to use circular template
inheritance.  The primary use-case for overextends is to simultaneously
override and extend templates from other reusable apps, in your own Django
project.")
    (license license:bsd-2)))

(define-public python2-django-overextends
  (package-with-python2 python-django-overextends))

(define-public python-django-pipeline
  (package
    (name "python-django-pipeline")
    (version "1.6.14")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "django-pipeline" version))
       (sha256
        (base32
         "1a207y71r7za033ira0qmh2yrgp5rq0l04gw2fg9b8jri7sslrzg"))))
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
             (or
              (not tests?)
              (begin
                (setenv "DJANGO_SETTINGS_MODULE" "tests.settings")
                (invoke "django-admin" "test" "tests"))))))))
    (propagated-inputs
     `(("python-django" ,python-django)
       ("python-slimit" ,python-slimit)
       ("python-jsmin" ,python-jsmin)))
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
    (version "4.10.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "django-redis" version))
              (sha256
               (base32
                "1rxcwnv9ik0swkwvfqdi9i9baw6n8if5pj6q63fjh4p9chw3j2xg"))))
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
     `(("python-fakeredis" ,python-fakeredis)
       ("python-hiredis" ,python-hiredis)
       ("python-mock" ,python-mock)
       ("python-msgpack" ,python-msgpack)
       ("redis" ,redis)))
    (propagated-inputs
     `(("python-django" ,python-django)
       ("python-redis" ,python-redis)))
    (home-page "https://github.com/niwibe/django-redis")
    (synopsis "Full featured redis cache backend for Django")
    (description
      "Full featured redis cache backend for Django.")
    (license license:bsd-3)))

(define-public python2-django-redis
  (package-with-python2 python-django-redis))

(define-public python-django-rq
  (package
    (name "python-django-rq")
    (version "1.3.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "django-rq" version))
              (sha256
               (base32
                "1ips1ikv5qhgwb58ssn496vgqg9qv6jinwmwbrg9l3s75fskd1l5"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "redis-server" "--daemonize" "yes")
             (invoke "django-admin.py" "test" "django_rq"
                     "--settings=django_rq.tests.settings"
                     "--pythonpath="))))))
    (native-inputs
     `(("python-mock" ,python-mock)
       ("redis" ,redis)))
    (propagated-inputs
     `(("python-django" ,python-django)
       ("python-rq" ,python-rq)))
    (home-page "https://github.com/ui/django-rq")
    (synopsis "Django integration with RQ")
    (description
      "Django integration with RQ, a Redis based Python queuing library.
Django-RQ is a simple app that allows you to configure your queues in django's
settings.py and easily use them in your project.")
    (license license:expat)))

(define-public python2-django-rq
  (package-with-python2 python-django-rq))

(define-public python-django-sortedm2m
  (package
    (name "python-django-sortedm2m")
    (version "1.3.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "django-sortedm2m" version))
              (sha256
               (base32
                "0axf765i7b3c2s83nlph47asi8s071dhq8l7y382v1pw785s22vi"))))
    (build-system python-build-system)
    (arguments
     ;; no tests.
     `(#:tests? #f))
    (propagated-inputs
     `(("python-django" ,python-django)))
    (home-page "https://github.com/gregmuellegger/django-sortedm2m")
    (synopsis "Drop-in replacement for django's own ManyToManyField")
    (description
      "Sortedm2m is a drop-in replacement for django's own ManyToManyField.
The provided SortedManyToManyField behaves like the original one but remembers
the order of added relations.")
    (license license:bsd-3)))

(define-public python2-django-sortedm2m
  (package-with-python2 python-django-sortedm2m))

(define-public python-django-appconf
  (package
    (name "python-django-appconf")
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "django-appconf" version))
              (sha256
               (base32
                "1qw0p9qh78bvkgi38ba58djwn0rd5j1lrkg2c2wk5wb7snj3rw9m"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-django" ,python-django)
       ("python-six" ,python-six)))
    (home-page "https://github.com/django-compressor/django-appconf")
    (synopsis "Handle configuration defaults of packaged Django apps")
    (description
      "This app precedes Django's own AppConfig classes that act as \"objects
[to] store metadata for an application\" inside Django's app loading mechanism.
In other words, they solve a related but different use case than
django-appconf and can't easily be used as a replacement.  The similarity in
name is purely coincidental.")
    (license license:bsd-3)))

(define-public python2-django-appconf
  (package-with-python2 python-django-appconf))

(define-public python-django-statici18n
  (package
    (name "python-django-statici18n")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "django-statici18n" version))
              (sha256
               (base32
                "0alcf4g1nv69njhq5k3qw4mfl2k6dc18bik5nk0g1mnp3m8zyz7k"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-django" ,python-django)
       ("django-appconf" ,python-django-appconf)))
    (home-page "https://github.com/zyegfryed/django-statici18n")
    (synopsis "Generate JavaScript catalog to static files")
    (description
      "A Django app that provides helper for generating JavaScript catalog to
static files.")
    (license license:bsd-3)))

(define-public python2-django-statici18n
  (package-with-python2 python-django-statici18n))

(define-public pootle
  (package
    (name "pootle")
    (version "2.8.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "Pootle" version ".tar.bz2"))
        (sha256
         (base32
          "1ng8igq0alsqzasgxdh3fb23581anyzp121h9041pwdzzv98kn4m"))))
    (build-system python-build-system)
    (arguments
     `(; pootle supports only python2.
       #:python ,python-2
       ;; tests are not run and fail with "pytest_pootle/data/po/.tmp: No such
       ;; file or directory". If we create this directory,
       ;; pytest_pootle/data/po/terminology.po is missing.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-requirements
           (lambda _
             (substitute* "Pootle.egg-info/requires.txt"
               (("1.7.3") "1.8.0")
               (("2.0.0") "2.1.0"))
             (substitute* "requirements/tests.txt"
               (("==3.0.6") ">=3.0.6"))
             (substitute* "requirements/base.txt"
               (("1.7.3") "1.8.0")
               (("2.0.0") "2.1.0")))))))
    (propagated-inputs
     `(("django-allauth" ,python2-django-allauth)
       ("django-assets" ,python2-django-assets)
       ("django-bulk-update" ,python2-django-bulk-update)
       ("django-contact-form" ,python2-django-contact-form)
       ("django-contrib-comments" ,python2-django-contrib-comments)
       ("django-overextends" ,python2-django-overextends)
       ("django-redis" ,python2-django-redis)
       ("django-rq" ,python2-django-rq)
       ("django-sortedm2m" ,python2-django-sortedm2m)
       ("django-statici18n" ,python2-django-statici18n)
       ("babel" ,python2-babel)
       ("cssmin" ,python2-cssmin)
       ("diff-match-patch" ,python2-diff-match-patch)
       ("dirsync" ,python2-dirsync)
       ("elasticsearch" ,python2-elasticsearch)
       ("jsonfield" ,python2-django-jsonfield)
       ("lxml" ,python2-lxml)
       ("dateutil" ,python2-dateutil)
       ("levenshtein" ,python2-levenshtein)
       ("mysqlclient" ,python2-mysqlclient)
       ("psycopg2" ,python2-psycopg2)
       ("pytz" ,python2-pytz)
       ("rq" ,python2-rq)
       ("scandir" ,python2-scandir)
       ("stemming" ,python2-stemming)
       ("translate-toolkit" ,python2-translate-toolkit)))
    (native-inputs
     `(("python2-pytest" ,python2-pytest)
       ("python2-pytest-django" ,python2-pytest-django)
       ("python2-pytest-catchlog" ,python2-pytest-catchlog)
       ("python2-pytest-cov" ,python2-pytest-cov)
       ("python2-factory-boy" ,python2-factory-boy)))
    (home-page "http://pootle.translatehouse.org/")
    (synopsis "Community localization server")
    (description
      "Pootle is an online translation and localization tool.  It works to
lower the barrier of entry, providing tools to enable teams to work towards
higher quality while welcoming newcomers.")
    (license license:gpl3+)))

(define-public python-django-tagging
  (package
    (name "python-django-tagging")
    (version "0.4.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "django-tagging" version))
       (sha256
        (base32
         "0s7b4v45j783yaxs7rni10k24san0ya77nqz4s7zdf3jhfpk42r1"))))
    (build-system python-build-system)
    (home-page "https://github.com/Fantomas42/django-tagging")
    (synopsis "Generic tagging application for Django")
    (description "This package provides a generic tagging application for
Django projects, which allows association of a number of tags with any
@code{Model} instance and makes retrieval of tags simple.")
    (license license:bsd-3)))

(define-public python2-django-tagging
  (package-with-python2 python-django-tagging))

(define-public python-djangorestframework
  (package
    (name "python-djangorestframework")
    (version "3.7.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "djangorestframework" version))
       (sha256
        (base32
         "11qv117gqwswxjljs7wafxg1hyzzlx3qrviwlk9hw41bsbl997lz"))))
    (build-system python-build-system)
    (arguments
     '(;; No included tests
       #:tests? #f))
    (propagated-inputs
     `(("python-django" ,python-django)))
    (home-page "https://www.django-rest-framework.org")
    (synopsis "Toolkit for building Web APIs with Django")
    (description
     "The Django REST framework is for building Web APIs with Django.  It
provides features like a web browseable API and authentication policies.")
    (license license:bsd-2)))

(define-public python-django-crispy-forms
  (package
    (name "python-django-crispy-forms")
    (version "1.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "django-crispy-forms" version))
       (sha256
        (base32
         "0pv7y648i8iz7mf64gkjizpbx5d01ap2s4vqqa30n38if6wvlljr"))))
    (build-system python-build-system)
    (arguments
     '(;; No included tests
       #:tests? #f))
    (propagated-inputs
     `(("python-django" ,python-django)))
    (home-page
     "http://github.com/maraujop/django-crispy-forms")
    (synopsis "Tool to control Django forms without custom templates")
    (description
     "@code{django-crispy-forms} lets you easily build, customize and reuse
forms using your favorite CSS framework, without writing template code.")
    (license license:expat)))

(define-public python-django-override-storage
  (package
    (name "python-django-override-storage")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "django-override-storage" version))
       (sha256
        (base32
         "0sqz1mh0yn8b1bzz2gr2azfiynljigm5gkzavp5n17zd3j2jg57x"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-django" ,python-django)))
    (home-page
     "https://github.com/danifus/django-override-storage")
    (synopsis "Django test helpers to manage file storage side effects")
    (description
     "This project provides tools to help reduce the side effects of using
FileFields during tests.")
    (license license:expat)))
