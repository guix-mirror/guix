;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 David Thompson <davet@gnu.org>
;;; Copyright © 2015, 2017, 2019, 2020, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2016, 2017, 2018, 2019 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Danny Milosavljevic <dannym+a@scratchpost.org>
;;; Copyright © 2017, 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Frederick M. Muriithi <fredmanglis@gmail.com>
;;; Copyright © 2017 Christopher Allan Webber <cwebber@dustycloud.org>
;;; Copyright © 2017 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019, 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2019 Alexandros Theodotou <alex@zrythm.org>
;;; Copyright © 2019 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2021 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2021 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 Hugo Lecomte <hugo.lecomte@inria.fr>
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

(define-module (gnu packages sphinx)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages time)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages graph))

(define-public python-sphinx
  (package
    (name "python-sphinx")
    (version "3.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Sphinx" version))
       (sha256
        (base32
         "0023vc2i29pjxmvdqbz1wdbi7gbj56y1br1b2z8h8wa44li5k38y"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             ;; Requires Internet access.
             (delete-file "tests/test_build_linkcheck.py")
             (substitute* "tests/test_build_latex.py"
               (("@pytest.mark.sphinx\\('latex', testroot='images'\\)")
                "@pytest.mark.skip()"))
             (invoke "make" "test"))))))
    (propagated-inputs
     `(("python-babel" ,python-babel)
       ("python-docutils" ,python-docutils)
       ("python-jinja2" ,python-jinja2)
       ("python-imagesize" ,python-imagesize)
       ("python-packaging" ,python-packaging)
       ("python-pygments" ,python-pygments)
       ("python-requests" ,python-requests)
       ("python-snowballstemmer" ,python-snowballstemmer)
       ("python-sphinx-alabaster-theme"
        ,python-sphinx-alabaster-theme)
       ("python-sphinxcontrib-applehelp" ,python-sphinxcontrib-applehelp)
       ("python-sphinxcontrib-devhelp" ,python-sphinxcontrib-devhelp)
       ("python-sphinxcontrib-htmlhelp" ,python-sphinxcontrib-htmlhelp)
       ("python-sphinxcontrib-jsmath" ,python-sphinxcontrib-jsmath)
       ("python-sphinxcontrib-qthelp" ,python-sphinxcontrib-qthelp)
       ("python-sphinxcontrib-serializinghtml"
        ,python-sphinxcontrib-serializinghtml)))
    (native-inputs
     `(("graphviz" ,graphviz)
       ("imagemagick" ,imagemagick)                    ;for "convert"
       ("python-html5lib" ,python-html5lib)
       ("python-mock" ,python-mock)
       ("python-nose" ,python-nose)
       ("python-pytest" ,python-pytest)))
    (home-page "https://www.sphinx-doc.org")
    (synopsis "Python documentation generator")
    (description "Sphinx is a tool that makes it easy to create documentation
for Python projects or other documents consisting of multiple reStructuredText
sources.")
    (license license:bsd-2)
    (properties `((python2-variant . ,(delay python2-sphinx))))))

;; Sphinx 2 does not support Python 2, so we stick with this older version here.
(define-public python2-sphinx
  (let ((base (package-with-python2 (strip-python2-variant python-sphinx))))
    (package
      (inherit base)
      (version "1.7.7")
      (source (origin
                (method url-fetch)
                (uri (pypi-uri "Sphinx" version))
                (sha256
                 (base32
                  "0pkkbfj7cl157q550gcs45am5y78ps0h7q6455d64s1zmw01jlvi"))))
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-before 'check 'disable-broken-tests
               (lambda _
                 (for-each delete-file
                           ;; These tests are broken when using Python2:
                           ;; <https://github.com/sphinx-doc/sphinx/issues/4710>.
                           '("tests/test_api_translator.py"
                             "tests/test_setup_command.py"
                             ;; This one fails for unknown reasons.
                             "tests/test_correct_year.py"))))))))
      (native-inputs `(("python2-mock" ,python2-mock)
                       ("python2-enum34" ,python2-enum34)
                       ,@(package-native-inputs base)))
      ;; Sphinx 2 has some dependencies that do not support Python 2, so
      ;; we keep our own propagated-inputs here instead of inheriting.
      (propagated-inputs
       `(("python2-pytz" ,python2-pytz)
         ("python2-typing" ,python2-typing)
         ("python2-imagesize" ,python2-imagesize)
         ("python2-sphinx-alabaster-theme" ,python2-sphinx-alabaster-theme)
         ("python2-babel" ,python2-babel)
         ("python2-snowballstemmer" ,python2-snowballstemmer)
         ("python2-docutils" ,python2-docutils-0.14)
         ("python2-jinja2" ,python2-jinja2)
         ("python2-packaging" ,python2-packaging)
         ("python2-pygments" ,python2-pygments)
         ("python2-requests" ,python2-requests)
         ("python2-six" ,python2-six)
         ("python2-sphinxcontrib-websupport" ,python2-sphinxcontrib-websupport))))))

(define-public python-sphinxcontrib-applehelp
  (package
    (name "python-sphinxcontrib-applehelp")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sphinxcontrib-applehelp" version))
              (sha256
               (base32
                "0n5wrn4l7x6gxvi1g7c6y72hkxgc223axz1jykipaxhfr1g76wm0"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))                    ;XXX: circular dependency on Sphinx
    (home-page "https://github.com/sphinx-doc/sphinxcontrib-applehelp")
    (synopsis "Sphinx extension for creating Apple help books")
    (description
     "@code{sphinxcontrib-applehelp} is a Sphinx extension which outputs
Apple help books.")
    (license license:bsd-2)))

(define-public python-sphinx-copybutton
  (package
    (name "python-sphinx-copybutton")
    (version "0.2.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinx-copybutton" version))
       (sha256
        (base32
         "0ny9jpygj93z8jyjpi4v6jjz90pmns74s389wq16igzzq2mpgfjm"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; XXX: Check requires network access.
    (propagated-inputs
     `(("python-flit" ,python-flit)
       ("python-setuptools" ,python-setuptools)
       ("python-sphinx" ,python-sphinx)
       ("python-wheel" ,python-wheel)))
    (home-page "https://github.com/choldgraf/sphinx-copybutton")
    (synopsis "Sphinx extension to add \"copy\" buttons to code blocks")
    (description
     "This package provides a small sphinx extension to add \"copy\" buttons
to code blocks.")
    (license license:expat)))

(define-public python-sphinxcontrib-devhelp
  (package
    (name "python-sphinxcontrib-devhelp")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sphinxcontrib-devhelp" version))
              (sha256
               (base32
                "1r1qngsbjqbg4rj93kpj44qqy7n4x5khldkr0c3ffhlnggx1lzzz"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))                    ;XXX: circular dependency on Sphinx
    (home-page "https://github.com/sphinx-doc/sphinxcontrib-devhelp")
    (synopsis "Sphinx extension for creating Devhelp documents")
    (description
     "@code{sphinxcontrib-devhelp} is a Sphinx extension which outputs
@url{Devhelp,https://wiki.gnome.org/Apps/Devhelp} documents.")
    (license license:bsd-2)))

(define-public python-sphinxcontrib-github-alt
  (package
    (name "python-sphinxcontrib-github-alt")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinxcontrib_github_alt" version))
       (sha256
        (base32
         "1x9af78vamjjcdrrhiah3wg613jv7gm8yh9vvqfrmf4vam6mimyg"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-docutils" ,python-docutils)
       ("python-sphinx" ,python-sphinx)))
    (home-page "https://github.com/jupyter/sphinxcontrib_github_alt")
    (synopsis "Link to GitHub pages from Sphinx docs")
    (description
     "This package lets you link to GitHub issues, pull requests, commits and
users from Sphinx docs.")
    (license license:bsd-2)))

(define-public python-sphinxcontrib-htmlhelp
  (package
    (name "python-sphinxcontrib-htmlhelp")
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sphinxcontrib-htmlhelp" version))
              (sha256
               (base32
                "16v5jdnibwrqjdr5aqchgyzpwy3rgamwhdf4kidv5nxj65zbpxg8"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))                    ;XXX: circular dependency on Sphinx
    (home-page "https://github.com/sphinx-doc/sphinxcontrib-htmlhelp")
    (synopsis "Sphinx extension for rendering HTML help files")
    (description
     "@code{sphinxcontrib-htmlhelp} is a Sphinx extension which renders
HTML help files.")
    (license license:bsd-2)))

(define-public python-sphinxcontrib-jsmath
  (package
    (name "python-sphinxcontrib-jsmath")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sphinxcontrib-jsmath" version))
              (sha256
               (base32
                "1f64w19j33sp151jimibraw6qrbhd5gxy8hs3797w9478m55x4m9"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))                    ;XXX: circular dependency on Sphinx
    (home-page "https://github.com/sphinx-doc/sphinxcontrib-jsmath")
    (synopsis "Sphinx extension to render math equations")
    (description
     "@code{sphinxcontrib-jsmath} is a Sphinx extension which renders display
math in HTML via JavaScript.")
    (license license:bsd-2)))

(define-public python-sphinxcontrib-newsfeed
  (package
    (name "python-sphinxcontrib-newsfeed")
    (version "0.1.4")
    (source (origin
             (method url-fetch)
             (uri (pypi-uri "sphinxcontrib-newsfeed" version))
             (sha256
              (base32
               "1d7gam3mn8v4in4p16yn3v10vps7nnaz6ilw99j4klij39dqd37p"))))
    (arguments '(#:tests? #f)) ; No tests.
    (build-system python-build-system)
    (propagated-inputs
     `(("python-sphinx" ,python-sphinx)))
    (synopsis "News Feed extension for Sphinx")
    (description "Sphinxcontrib-newsfeed is an extension for adding a simple
Blog, News or Announcements section to a Sphinx website.")
    (home-page "https://bitbucket.org/prometheus/sphinxcontrib-newsfeed")
    (license license:bsd-2)))

(define-public python-sphinxcontrib-programoutput
  (package
    (name "python-sphinxcontrib-programoutput")
    (version "0.15")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sphinxcontrib-programoutput" version))
              (sha256
               (base32
                "155bz0z2cz544pphkd3nk7ph83mz7nn0ql135kzi62kqmd75ppc0"))))
    (build-system python-build-system)
    (arguments
     ;; FIXME: Many tests are failing.
     '(#:tests? #f))
    (propagated-inputs
     `(("python-sphinx" ,python-sphinx)))
    (synopsis "Sphinx extension to include program output")
    (description "A Sphinx extension to literally insert the output of arbitrary
commands into documents, helping you to keep your command examples up to date.")
    (home-page "https://github.com/NextThought/sphinxcontrib-programoutput")
    (license license:bsd-2)))

(define-public python2-sphinxcontrib-programoutput
  (package-with-python2 python-sphinxcontrib-programoutput))

(define-public python-sphinxcontrib-qthelp
  (package
    (name "python-sphinxcontrib-qthelp")
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sphinxcontrib-qthelp" version))
              (sha256
               (base32
                "0wjsp96d262shzkx7pb7pra7mmf0j8c5rz56i6x0vdsqw1z7ccsc"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))                    ;XXX: circular dependency on Sphinx
    (home-page "https://github.com/sphinx-doc/sphinxcontrib-qthelp")
    (synopsis "Sphinx extension to output QtHelp documents")
    (description
     "@code{sphinxcontrib-qthelp} is a Sphinx extension which outputs QtHelp
documents.")
    (license license:bsd-2)))

(define-public python-sphinxcontrib-serializinghtml
  (package
    (name "python-sphinxcontrib-serializinghtml")
    (version "1.1.4")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sphinxcontrib-serializinghtml" version))
              (sha256
               (base32
                "1g3pbx0g88zd9xzcrbaypa2k60axrh92vf5j76wsk0p9hv6fr87a"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))                    ;XXX: circular dependency on Sphinx
    (home-page "https://github.com/sphinx-doc/sphinxcontrib-serializinghtml")
    (synopsis "Sphinx extension to serialize HTML files")
    (description
     "@code{sphinxcontrib-serializinghtml} is a Sphinx extension which outputs
\"serialized\" HTML files.")
    (license license:bsd-2)))

(define-public python-sphinxcontrib-svg2pdfconverter
  (package
    (name "python-sphinxcontrib-svg2pdfconverter")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sphinxcontrib-svg2pdfconverter" version))
              (sha256
               (base32
                "1hhaxmqc7wh8gnyw5jaqckliknvk0p21y12wk3bdmncgkaf9ar8f"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-sphinx" ,python-sphinx)))
    (home-page "https://github.com/missinglinkelectronics/sphinxcontrib-svg2pdfconverter")
    (synopsis "Sphinx SVG to PDF converter extension")
    (description "A Sphinx extension to convert SVG images to PDF in case the
builder does not support SVG images natively (e.g. LaTeX).")
    (license license:bsd-2)))

(define-public python-sphinxcontrib-websupport
  (package
    (name "python-sphinxcontrib-websupport")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sphinxcontrib-websupport" version))
              (sha256
               (base32
                "1smma7r0rhwcmbfvvkfs5djfz1qm8wbpcvp084ca6dmw2b9zplxs"))))
    (build-system python-build-system)
    (arguments
     ;; FIXME: Tests depend on Sphinx, which depends on this.
     `(#:tests? #f))
    (home-page "https://sphinx-doc.org/")
    (synopsis "Sphinx API for web applications")
    (description "This package provides a Python API to easily integrate
Sphinx documentation into your web application.  It provides tools to
integrate Sphinx documents in web templates and to handle searches.")
    (license license:bsd-3)))

(define-public python2-sphinxcontrib-websupport
  (package-with-python2 python-sphinxcontrib-websupport))

(define-public python-sphinx-gallery
  (package
    (name "python-sphinx-gallery")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinx-gallery" version))
       (sha256
        (base32 "14zyhr7m92nafhhnzfvnbgkf5m91krd9mjyi24zn59bjq6zyr8hl"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'writable-files-for-tests
           (lambda _
             (for-each make-file-writable (find-files "."))))
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "python" "-m" "pytest" "--pyargs" "sphinx_gallery" "-k"
                       (string-append
                        ;; These tests require online data.
                        "not test_embed_code_links_get_data"
                        " and not test_run_sphinx"
                        ;; AssertionError.
                        " and not test_embed_links_and_styles"))))))))
    (native-inputs
     `(("python-joblib" ,python-joblib)
       ("python-matplotlib" ,python-matplotlib)
       ("python-numpy" ,python-numpy)
       ("python-pillow" ,python-pillow)
       ("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-sphinx" ,python-sphinx)))
    (home-page "https://sphinx-gallery.github.io/stable/index.html")
    (synopsis "Generate an examples gallery automatically")
    (description
     "@code{sphinx_gallery} is a Sphinx extension that builds an HTML version
from any set of Python scripts and puts it into an examples gallery.")
    (license license:bsd-3)))

(define-public python-sphinx-me
  (package
    (name "python-sphinx-me")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinx-me" version))
       (sha256
        (base32
         "06jzgp213zihnvpcy2y5jy3ykid3apc2ncp2pg6a2g05lhiziglq"))))
    (build-system python-build-system)
    (home-page "https://github.com/stephenmcd/sphinx-me")
    (synopsis "Create a Sphinx documentation shell")
    (description
      "Create a Sphinx documentation shell for your project and include the
README file as the documentation index.  It handles extracting the required
meta data such as the project name, author and version from your project for
use in your Sphinx docs.")
    (license license:bsd-2)))

(define-public python2-sphinx-me
  (package-with-python2 python-sphinx-me))

(define-public python-sphinx-repoze-autointerface
  (package
    (name "python-sphinx-repoze-autointerface")
    (version "0.8")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "repoze.sphinx.autointerface" version))
              (sha256
               (base32
                "08ycivzf7bh4a1zcyp31hbyqs1b2c9r26raa3vxjwwmbfqr3iw4f"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f)) ; No tests.
    (propagated-inputs
     `(("python-sphinx" ,python-sphinx)
       ("python-zope-interface" ,python-zope-interface)))
    (synopsis "Auto-generate Sphinx API docs from Zope interfaces")
    (description "This package defines an extension for the Sphinx documentation
system.  The extension allows generation of API documentation by
introspection of @code{zope.interface} instances in code.")
    (home-page "https://github.com/repoze/repoze.sphinx.autointerface")
    (license license:repoze)))

(define-public python2-sphinx-repoze-autointerface
  (package-with-python2 python-sphinx-repoze-autointerface))

(define-public python-sphinx-alabaster-theme
  (package
    (name "python-sphinx-alabaster-theme")
    (version "0.7.12")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "alabaster" version))
              (sha256
               (base32
                "00nwwjj2d2ym4s2kk217x7jkx1hnczc3fvm8yxbqmsp6b0nxfqd6"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pygments" ,python-pygments)))
    (home-page "https://alabaster.readthedocs.io/")
    (synopsis "Configurable sidebar-enabled Sphinx theme")
    (description "Alabaster is a visually (c)lean, responsive, configurable
theme for the Sphinx documentation system.  It's the default theme of Sphinx.")
    (license license:bsd-3)))

(define-public python2-sphinx-alabaster-theme
  (package-with-python2 python-sphinx-alabaster-theme))

(define-public python-sphinx-argparse
  (package
    (name "python-sphinx-argparse")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinx-argparse" version))
       (sha256
        (base32 "05wc8f5hb3jsg2vh2jf7jsyan8d4i09ifrz2c8fp6f7x1zw9iav0"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-sphinx" ,python-sphinx)))
    (native-inputs
     `(("python-commonmark" ,python-commonmark)
       ("python-pytest" ,python-pytest)
       ("python-sphinx-rtd-theme" ,python-sphinx-rtd-theme)))
    (home-page "https://github.com/ribozz/sphinx-argparse")
    (synopsis "Sphinx extension for documenting argparse commands and options")
    (description
     "This package is a sphinx extension that automatically documents
argparse commands and options")
    (license license:expat)))

(define-public python-sphinx-cloud-sptheme
  (package
    (name "python-sphinx-cloud-sptheme")
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "cloud_sptheme" version))
              (sha256
               (base32
                "1dniqb6a39yh786f86c4jn666rwnyi1jvzn4616zhcchb7sfdshd"))))
    (build-system python-build-system)
    ;; FIXME: The 'pypi' release archive does not contain tests.
    (arguments '(#:tests? #f))
    (native-inputs
     `(("python-sphinx" ,python-sphinx)))
    (home-page "https://bitbucket.org/ecollins/cloud_sptheme")
    (synopsis "'Cloud' theme for Sphinx documenter")
    (description "This package contains the \"Cloud\" theme for Sphinx and some
related extensions.")
    (license license:bsd-3)))

(define-public python2-sphinx-cloud-sptheme
  (package-with-python2 python-sphinx-cloud-sptheme))

(define-public python-guzzle-sphinx-theme
  (package
    (name "python-guzzle-sphinx-theme")
    (version "0.7.11")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "guzzle_sphinx_theme" version))
        (sha256
         (base32
          "1rnkzrrsbnifn3vsb4pfaia3nlvgvw6ndpxp7lzjrh23qcwid34v"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-sphinx" ,python-sphinx)))
    (home-page "https://github.com/guzzle/guzzle_sphinx_theme")
    (synopsis "Sphinx theme used by Guzzle")
    (description "This package provides guzzle_sphinx_theme, a theme for the
Sphinx documentation system, used by @uref{http://docs.guzzlephp.org, Guzzle}
and several other projects.")
    (license license:expat)))

(define-public python2-guzzle-sphinx-theme
  (package-with-python2 python-guzzle-sphinx-theme))

(define-public python-sphinx-rtd-theme
  (package
    (name "python-sphinx-rtd-theme")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinx_rtd_theme" version))
       (sha256
        (base32
         "05rlhjzdyapr2w74jjs7mrm8hi69qskcr5vya9f9drpsys7lpxrd"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f)) ; No tests.
    (propagated-inputs
     `(("python-sphinx" ,python-sphinx)))
    (home-page "https://github.com/snide/sphinx_rtd_theme/")
    (synopsis "ReadTheDocs.org theme for Sphinx")
    (description "A theme for Sphinx used by ReadTheDocs.org.")
    (license license:expat)))

(define-public python2-sphinx-rtd-theme
  (package-with-python2 python-sphinx-rtd-theme))

(define-public python-breathe
  (package
    (name "python-breathe")
    (version "4.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "breathe" version))
       (sha256
        (base32
         "055h95fkdld7s49878fqjx1nri1drj1czc184vrb7i60mf2yqg9n"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-docutils" ,python-docutils)
       ("python-sphinx" ,python-sphinx)))
    (home-page "https://github.com/michaeljones/breathe")
    (synopsis "ReStructuredText and Sphinx bridge to Doxygen")
    (description "This package is an extension to reStructuredText and Sphinx
to be able to read and render the Doxygen xml output.")
    (license license:bsd-3)))

(define-public python-sphinx-intl
  (package
    (name "python-sphinx-intl")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinx-intl" version))
       (sha256
        (base32 "1d1q0sanjp4nkfvhsxi75zf3xjyyi8nzxvl3v7l0jy9ld70nwnmj"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-sphinx" ,python-sphinx)
       ("python-click" ,python-click)))
    (home-page "https://github.com/sphinx-doc/sphinx-intl")
    (synopsis
     "Sphinx utility that makes it easy to translate and to apply translation")
    (description
     "A utility tool that provides several features that make it easy to
translate and to apply translation to Sphinx generated document.")
    (license license:bsd-2)))

(define-public python-sphinx-autobuild
  (package
    (name "python-sphinx-autobuild")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinx-autobuild" version))
       (sha256
        (base32
         "0kn753dyh3b1s0h77lbk704niyqc7bamvq6v3s1f6rj6i20qyf36"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; See https://github.com/GaretJax/sphinx-autobuild/pull/72
         (add-after 'unpack 'use-later-port-for
           (lambda _
             (substitute* "requirements.txt"
               (("port_for==.*") "port_for\n"))
             #t))
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "pytest" "-v"))
             #t)))))
    (propagated-inputs
     `(("python-argh" ,python-argh)
       ("python-livereload" ,python-livereload)
       ("python-pathtools" ,python-pathtools)
       ("python-port-for" ,python-port-for)
       ("python-pyyaml" ,python-pyyaml)
       ("python-tornado" ,python-tornado)
       ("python-watchdog" ,python-watchdog)))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (home-page "https://github.com/GaretJax/sphinx-autobuild")
    (synopsis "Rebuild Sphinx documentation when a change is detected")
    (description
     "This package lets you watch a Sphinx directory and rebuild the
documentation when a change is detected.  It also includes a livereload
enabled web server.")
    (license license:expat)))

(define-public python-sphinx-autodoc-typehints
  (package
    (name "python-sphinx-autodoc-typehints")
    (version "1.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinx-autodoc-typehints" version))
       (sha256
        (base32
         "086v9mg21pvfx0lfqjx2xf36hnzrsripfg345xi59f7xwb9scjr4"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-setuptools-scm" ,python-setuptools-scm)
       ("python-sphinx" ,python-sphinx)))
    (native-inputs
     `(("python-dataclasses" ,python-dataclasses)
       ("python-pytest" ,python-pytest)
       ("python-sphinx" ,python-sphinx)
       ("python-sphobjinv" ,python-sphobjinv)
       ("python-typing-extensions" ,python-typing-extensions)))
    (home-page "https://pypi.org/project/sphinx-autodoc-typehints/")
    (synopsis "Type hints for the Sphinx autodoc extension")
    (description "This extension allows you to use Python 3 annotations for
documenting acceptable argument types and return value types of functions.")
    (license license:expat)))

(define-public python-nbsphinx
  (package
    (name "python-nbsphinx")
    (version "0.7.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "nbsphinx" version))
        (sha256
          (base32
            "0j56bxdj08vn3q1804qwb1ywhga1mdg1awgm7i64wfpfwi8df2zm"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-docutils" ,python-docutils)
        ("python-jinja2" ,python-jinja2)
        ("python-nbconvert" ,python-nbconvert)
        ("python-nbformat" ,python-nbformat)
        ("python-sphinx" ,python-sphinx)
        ("python-traitlets" ,python-traitlets)))
    (home-page "https://nbsphinx.readthedocs.io/")
    (synopsis "Jupyter Notebook Tools for Sphinx")
    (description "@code{python-nbsphinx} is a Sphinx extension that
provides a source parser for @code{*.ipynb} files.  Custom Sphinx
directives are used to show Jupyter Notebook code cells (and of course
their results) in both HTML and LaTeX output.  Un-evaluated notebooks
- i.e. notebooks without stored output cells - will be automatically
executed during the Sphinx build process.")
    (license license:expat)))

(define-public python-sphobjinv
  (package
    (name "python-sphobjinv")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphobjinv" version))
       (sha256
        (base32
         "126lgm54c94ay3fci512ap4l607gak90pbz0fk98syxvj5izrrzx"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-attrs" ,python-attrs)
       ("python-certifi" ,python-certifi)
       ("python-fuzzywuzzy" ,python-fuzzywuzzy)
       ("python-jsonschema" ,python-jsonschema)
       ("python-levenshtein" ,python-levenshtein)))
    (home-page "https://github.com/bskinn/sphobjinv")
    (synopsis "Sphinx cross-reference tool")
    (description "Sphinx objects.inv inspection/manipulation tool.")
    (license license:expat)))

(define-public python-jupyter-sphinx
  (package
    (name "python-jupyter-sphinx")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jupyter_sphinx" version))
       (sha256
        (base32
         "1wma60787m2451nn4bc4jw7bzqksplplb84wqxm34iaw70499z1p"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-ipython" ,python-ipython)
       ("python-ipywidgets" ,python-ipywidgets)
       ("python-nbconvert" ,python-nbconvert)
       ("python-nbformat" ,python-nbformat)))
    (native-inputs
     `(("python-sphinx" ,python-sphinx)))
    (home-page "https://github.com/jupyter/jupyter-sphinx/")
    (synopsis "Jupyter Sphinx Extensions")
    (description
     "Jupyter-sphinx is a Sphinx extension that executes embedded code in a
Jupyter kernel, and embeds outputs of that code in the document.  It has
support for rich output such as images, LaTeX math and even JavaScript
widgets, and supports thebelab for live code execution with minimal effort.")
    (license license:bsd-3)))

(define-public python-sphinxcontrib-autoprogram
  (package
    (name "python-sphinxcontrib-autoprogram")
    (version "0.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinxcontrib-autoprogram" version))
       (sha256
        (base32
         "06hzim0d3fd72kf30fyjbbm5n8ibyybic0kf62gm79qp50zjwr5w"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-six" ,python-six)))
    (native-inputs
     `(("python-sphinx" ,python-sphinx)))
    (home-page "https://github.com/sphinx-contrib/autoprogram")
    (synopsis "Documenting CLI programs")
    (description
     "This Sphinx extension, @code{sphinxcontrib.autoprogram}, provides an
automated way to document command-line programs.  It scans
@code{argparse.ArgumentParser} object, and then expands it into a set of
@code{.. program::} and @code{.. option::} directives.")
    (license license:bsd-2)))

(define-public python-pydata-sphinx-theme
  (package
    (name "python-pydata-sphinx-theme")
    (version "0.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pydata-sphinx-theme" version))
       (sha256
        (base32
         "055bh3hyh72pafiylvgpsjlk18wm15gg4azc5rjlsww5z475iq1j"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-beautifulsoup4" ,python-beautifulsoup4)))
    (native-inputs
     `(("python-beautifulsoup4" ,python-beautifulsoup4)
       ("python-docutils" ,python-docutils)
       ("python-jupyter-sphinx" ,python-jupyter-sphinx)
       ("python-numpy" ,python-numpy)
       ("python-numpydoc" ,python-numpydoc)
       ("python-pandas" ,python-pandas)
       ("python-plotly" ,python-plotly)
       ("python-pytest" ,python-pytest)
       ("python-pytest-regressions"
        ,python-pytest-regressions)
       ("python-recommonmark" ,python-recommonmark)
       ("python-sphinx" ,python-sphinx)
       ("python-xarray" ,python-xarray)
       ("python-docutils" ,python-docutils)
       ("python-sphinx" ,python-sphinx)))
    (home-page "https://github.com/pydata/pydata-sphinx-theme")
    (synopsis "Bootstrap-based Sphinx theme")
    (description
     "This package provides a Bootstrap-based Sphinx theme from the PyData
community.")
    (license license:bsd-3)))
