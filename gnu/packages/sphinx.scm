;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 David Thompson <davet@gnu.org>
;;; Copyright © 2015, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2016, 2017, 2018, 2019 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Danny Milosavljevic <dannym+a@scratchpost.org>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Frederick M. Muriithi <fredmanglis@gmail.com>
;;; Copyright © 2017 Christopher Allan Webber <cwebber@dustycloud.org>
;;; Copyright © 2017 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 Nicolas Goaziou <mail@nicolasgoaziou.fr>
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
  #:use-module (guix utils)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages time))

(define-public python-sphinx
  (package
    (name "python-sphinx")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Sphinx" version))
       (sha256
        (base32
         "0js0rnbzm0nsb4gm2v4z79wlbmr05awqzic57kfl8ydkdxj80cj2"))))
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
     `(("python-imagesize" ,python-imagesize)
       ("python-sphinx-alabaster-theme"
        ,python-sphinx-alabaster-theme)
       ("python-babel" ,python-babel)
       ("python-snowballstemmer" ,python-snowballstemmer)
       ("python-docutils" ,python-docutils)
       ("python-jinja2" ,python-jinja2)
       ("python-packaging" ,python-packaging)
       ("python-pygments" ,python-pygments)
       ("python-requests" ,python-requests)
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
    (home-page "http://sphinx-doc.org/")
    (synopsis "Python documentation generator")
    (description "Sphinx is a tool that makes it easy to create documentation
for Python projects or other documents consisting of multiple reStructuredText
sources.")
    (license license:bsd-3)
    (properties `((python2-variant . ,(delay python2-sphinx))))))

;; Sphinx 2 does not support Python 2, so we stick with this older version here.
;; Remove this package once python2-pbcore no longer requires it.
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
                 ;; These tests are broken when using Python2:
                 ;; <https://github.com/sphinx-doc/sphinx/issues/4710>.
                 (for-each delete-file '("tests/test_api_translator.py"
                                         "tests/test_setup_command.py"))
                 #t))))))
      (native-inputs `(("python2-mock" ,python2-mock)
                       ("python2-enum34" ,python2-enum34)
                       ,@(package-native-inputs base)))
      ;; Sphinx 2 has some dependencies that do not support Python 2, so
      ;; we keep our own propagated-inputs here instead of inheriting.
      (propagated-inputs `(("python2-pytz" ,python2-pytz)
                           ("python2-typing" ,python2-typing)
                           ("python2-imagesize" ,python2-imagesize)
                           ("python2-sphinx-alabaster-theme"
                            ,python2-sphinx-alabaster-theme)
                           ("python2-babel" ,python2-babel)
                           ("python2-snowballstemmer" ,python2-snowballstemmer)
                           ("python2-docutils" ,python2-docutils)
                           ("python2-jinja2" ,python2-jinja2)
                           ("python2-packaging" ,python2-packaging)
                           ("python2-pygments" ,python2-pygments)
                           ("python2-requests" ,python2-requests)
                           ("python2-six" ,python2-six)
                           ("python2-sphinxcontrib-websupport"
                            ,python2-sphinxcontrib-websupport))))))

(define-public python-sphinxcontrib-applehelp
  (package
    (name "python-sphinxcontrib-applehelp")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sphinxcontrib-applehelp" version))
              (sha256
               (base32
                "15rqmgphj4wqf4m5wnzxgmwxx5jwfzb0j0nb94ql0x5wnar0mapd"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))                    ;XXX: circular dependency on Sphinx
    (home-page "https://github.com/sphinx-doc/sphinxcontrib-applehelp")
    (synopsis "Sphinx extension for creating Apple help books")
    (description
     "@code{sphinxcontrib-applehelp} is a Sphinx extension which outputs
Apple help books.")
    (license license:bsd-2)))

(define-public python-sphinxcontrib-devhelp
  (package
    (name "python-sphinxcontrib-devhelp")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sphinxcontrib-devhelp" version))
              (sha256
               (base32
                "0d2a57kqxl72i55rns0ly1i044y2x234b9sdi89ajc3kjdvv0r3c"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))                    ;XXX: circular dependency on Sphinx
    (home-page "https://github.com/sphinx-doc/sphinxcontrib-devhelp")
    (synopsis "Sphinx extension for creating Devhelp documents")
    (description
     "@code{sphinxcontrib-devhelp} is a Sphinx extension which outputs
@url{Devhelp,https://wiki.gnome.org/Apps/Devhelp} documents.")
    (license license:bsd-2)))

(define-public python-sphinxcontrib-htmlhelp
  (package
    (name "python-sphinxcontrib-htmlhelp")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sphinxcontrib-htmlhelp" version))
              (sha256
               (base32
                "08l4x8a2l4xjqdd5rhvmfsqihmlgg4prdayj9b6pigaii6gzjw26"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))                    ;XXX: circular dependency on Sphinx
    (home-page "https://github.com/sphinx-doc/sphinxcontrib-htmlhelp")
    (synopsis "Sphinx exension for rendering HTML help files")
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
    (license license:bsd-3)))

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
    (version "0.10")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sphinxcontrib-programoutput" version))
              (sha256
               (base32
                "153hhnlbx4688zj9wd64819ps5znc2jlyp5crkgzvn5hxgy99vpx"))))
    (build-system python-build-system)
    (arguments
     ;; FIXME: Many tests are failing and the upstream is gone.
     '(#:tests? #f))
    (propagated-inputs
     `(("python-sphinx" ,python-sphinx)))
    (synopsis "Sphinx extension to include program output")
    (description "A Sphinx extension to literally insert the output of arbitrary
commands into documents, helping you to keep your command examples up to date.")
    (home-page "https://github.com/lunaryorn/sphinxcontrib-programoutput")
    (license license:bsd-2)))

(define-public python2-sphinxcontrib-programoutput
  (package-with-python2 python-sphinxcontrib-programoutput))

(define-public python-sphinxcontrib-qthelp
  (package
    (name "python-sphinxcontrib-qthelp")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sphinxcontrib-qthelp" version))
              (sha256
               (base32
                "0vs09m6kf5vhiivpi5s5pks59iq0lqlsbkdycpqlysg53bhmqikr"))))
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
    (version "1.1.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sphinxcontrib-serializinghtml" version))
              (sha256
               (base32
                "09sj3nwahwr4iymg86gczbh151cfczqhf2kclbblzh2jh0zv7vy0"))))
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
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sphinxcontrib-svg2pdfconverter" version))
              (sha256
               (base32
                "1abvbgkkii13q8nsb10r0gc5lm0p9iq1iwhfhakn5ifn6asa0183"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-sphinx" ,python-sphinx)))
    (home-page "https://github.com/missinglinkelectronics/sphinxcontrib-svg2pdfconverter/releases")
    (synopsis "Sphinx SVG to PDF converter extension")
    (description "A Sphinx extension to convert SVG images to PDF in case the builder does not support
SVG images natively (e.g. LaTeX).")
    (license license:bsd-3)))

(define-public python-sphinxcontrib-websupport
  (package
    (name "python-sphinxcontrib-websupport")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "sphinxcontrib-websupport" version))
              (sha256
               (base32
                "1ff3ix76xi1y6m99qxhaq5161ix9swwzydilvdya07mgbcvpzr4x"))))
    (build-system python-build-system)
    (arguments
     ;; FIXME: Tests depend on Sphinx, which depends on this.
     `(#:tests? #f))
    (home-page "http://sphinx-doc.org/")
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
    (version "0.1.13")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/sphinx-gallery/sphinx-gallery"
                                  "/archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "03fs99mcb1r7qp0xixqv07vcz98sk21yq19ffdysi0infdcpzfkd"))))
    (build-system python-build-system)
    (arguments
     ;; FIXME: Tests attempt to download <https://docs.python.org/3/objects.inv>,
     ;; <https://docs.scipy.org/doc/numpy/objects.inv>, and
     ;; <https://matplotlib.org/objects.inv>.
     `(#:tests? #f))
    (native-inputs
     `(("python-pytest-runner" ,python-pytest-runner)))
    (home-page "https://sphinx-gallery.github.io/")
    (synopsis "Generate an examples gallery automatically")
    (description
     "@code{sphinx_gallery} is a Sphinx extension that builds an HTML version
from any set of Python scripts and puts it into an examples gallery.")
    (license license:bsd-3)))

(define-public python2-sphinx-gallery
  (package-with-python2 python-sphinx-gallery))

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
