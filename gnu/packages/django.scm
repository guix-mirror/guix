;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 ng0 <contact.ng0@cryptolab.net>
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
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages python))

(define-public python-django
  (package
    (name "python-django")
    (version "1.10.7")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "Django" version))
              (sha256
               (base32
                "1f5hnn2dzfr5szk4yc47bs4kk2nmrayjcvgpqi2s4l13pjfpfgar"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
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
           (lambda _
             (setenv "PYTHONPATH"
                     (string-append ".:" (getenv "PYTHONPATH")))
             (zero? (system* "python" "tests/runtests.py")))))))
    ;; TODO: Install extras/django_bash_completion.
    (native-inputs
     `(("tzdata", tzdata)
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
       ("python-pytz" ,python-pytz)
       ;; optional for tests: ("python-selenium" ,python-selenium)
       ("python-sqlparse" ,python-sqlparse)
       ("python-tblib" ,python-tblib)))
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

(define-public python-pytest-django
  (package
    (name "python-pytest-django")
    (version "2.9.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pytest-django" version))
              (sha256
               (base32
                "1mmc7zsz3dlhs6sx4sppkj1vgshabi362r1a8b8wpj1qfximpqcb"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; FIXME: How to run tests?
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-setuppy
           (lambda _
             (substitute* "setup.py"
                          (("setuptools_scm==1.8.0") "setuptools_scm"))
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
    (version "0.14.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "django-filter" version))
              (sha256
               (base32
                "0f78hmk8c903zwfzlsiw7ivgag81ymmb5hi73rzxbhnlg2v0l3fx"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (zero? (system* "python" "runtests.py")))))))
    (native-inputs
     `(("python-django" ,python-django)
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
    (version "0.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "django-allauth" version))
       (sha256
        (base32
         "1fslqc5qqb0b66yscvkyjwfv8cnbfx5nlkpnwimyb3pf1nc1w7r3"))))
    (build-system python-build-system)
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

(define-public python-django-gravatar2
  (package
    (name "python-django-gravatar2")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "django-gravatar2" version))
       (sha256
        (base32
         "1v4qyj6kms321yw0z2g1kch6b2dskmv6fjd6sfxzwr4xshq9mccl"))))
    (build-system python-build-system)
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
