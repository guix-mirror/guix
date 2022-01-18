;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Theodoros Foradis <theodoros@foradis.org>
;;; Copyright © 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2018 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2021 Bonface Munyoki Kilyungi <me@bonfacemunyoki.com>
;;; Copyright © 2021 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2021 Justin Veilleux <terramorpha@cock.li>
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

(define-module (gnu packages graphviz)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages image)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module ((guix licenses) #:prefix license:))

(define-public graphviz
  (package
    (name "graphviz")
    (version "2.49.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gitlab.com/api/v4/projects/4207231"
                                  "/packages/generic/graphviz-releases/"
                                  version "/graphviz-" version ".tar.xz"))
              (sha256
               (base32
                "042s6mbi681pwgffqww2ap780230nrsrfpfiz9a41dcjb5a0m524"))))
    (build-system gnu-build-system)
    (arguments
     ;; FIXME: rtest/rtest.sh is a ksh script (!).  Add ksh as an input.
     (list #:tests? #f
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'move-docs
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((out (assoc-ref outputs "out"))
                         (doc (assoc-ref outputs "doc")))
                     (mkdir-p (string-append doc "/share/graphviz"))
                     (rename-file (string-append out "/share/graphviz/doc")
                                  (string-append doc "/share/graphviz/doc")))))
               (add-after 'move-docs 'move-guile-bindings
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out"))
                          (lib (string-append out "/lib"))
                          (extdir (string-append lib "/guile/"
                                                 #$(version-major+minor
                                                    (package-version
                                                     (this-package-input "guile")))
                                                 "/extensions")))
                     (mkdir-p extdir)
                     (rename-file (string-append
                                   lib "/graphviz/guile/libgv_guile.so")
                                  (string-append extdir
                                                 "/libgv_guile.so"))))))))
    (inputs
     `(("libXrender" ,libxrender)
       ("libX11" ,libx11)
       ("gts" ,gts)
       ("gd" ,gd)
       ("guile" ,guile-3.0)                       ;Guile bindings
       ("pango" ,pango)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("libltdl" ,libltdl)
       ("libXaw" ,libxaw)
       ("expat" ,expat)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)))
    (native-inputs
     (list bison swig pkg-config))
    (outputs '("out" "doc"))                      ; 5 MiB of html + pdfs
    (home-page "https://www.graphviz.org/")
    (synopsis "Graph visualization software")
    (description
     "Graphviz is a graph visualization tool suite.  Graph visualization is a
way of representing structural information as diagrams of abstract graphs and
networks.  It has important applications in networking, bioinformatics,
software engineering, database and web design, machine learning, and in visual
interfaces for other technical domains.")
    (license license:epl1.0)))

;; Older Graphviz needed for pygraphviz.  See
;; https://github.com/pygraphviz/pygraphviz/issues/175
(define-public graphviz-2.38
  ;; This commit corresponds to the changelog change for version 2.38.0.
  ;; There are no tags.
  (let ((commit "f54ac2c9313ae80ccf76ef4ac6aa9be820a23126")
        (revision "1"))
    (package (inherit graphviz)
      (name "graphviz")
      (version (git-version "2.38.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.com/graphviz/graphviz.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1vjg308gflmi1khgjmcj431cnkrlv12bg4cqah39mwhny92jy92x"))))
      (arguments
       (substitute-keyword-arguments (package-arguments graphviz)
         ((#:phases phases)
          #~(modify-phases #$phases
              (add-after 'unpack 'prepare-bootstrap
                (lambda _
                  (substitute* "autogen.sh"
                    (("/bin/sh") (which "sh"))
                    (("\\$GRAPHVIZ_VERSION_DATE") "0"))
                  (setenv "CONFIG_SHELL" (which "sh"))
                  (setenv "SHELL" (which "sh"))

                  (map make-file-writable (find-files "." ".*"))
                  #t))
              (replace 'bootstrap
                (lambda _
                  (invoke (which "sh") "autogen.sh" "NOCONFIG") #t))))))
      (native-inputs
       (modify-inputs (package-native-inputs graphviz)
         (prepend autoconf
                  automake
                  libtool
                  flex
                  perl
                  tcl))))))

(define-public python-graphviz
  (package
    (name "python-graphviz")
    (version "0.13.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "graphviz" version ".zip"))
              (sha256
               (base32
                "009alrilzx0v7kl41khbq7k6k8b8pxyvbsi1b1ai933f6kpbxb30"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests #:allow-other-keys)
                      (if tests
                          (invoke "pytest" "-vv")
                          (format #t "test suite not run~%"))
                      #t)))))
    (native-inputs
     (list unzip
           ;; For tests.
           graphviz
           python-mock
           python-pytest
           python-pytest-cov
           python-pytest-mock))
    (home-page "https://github.com/xflr6/graphviz")
    (synopsis "Simple Python interface for Graphviz")
    (description
     "This package provides a simple Python interface for the Graphviz graph
visualization tool suite.")
    (license license:expat)))

(define-public python2-graphviz
  (package-with-python2 python-graphviz))

(define-public python-pygraphviz
  (package
    (name "python-pygraphviz")
    (version "1.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pygraphviz/pygraphviz")
             (commit (string-append "pygraphviz-" version))))
       (file-name (string-append "pygraphviz-" version "-checkout"))
       (sha256
        (base32
         "0jqc3dzy9n0hn3b99zq8jp53901zpjzvvi5ns5mbaxg8kdrb1lfx"))))
    (build-system python-build-system)
    (inputs
     (list graphviz))
    (native-inputs
     (list python-nose python-mock python-pytest
           python-doctest-ignore-unicode))
    (home-page "https://pygraphviz.github.io")
    (synopsis "Python interface to Graphviz")
    (description "PyGraphviz is a Python interface to the Graphviz graph
layout and visualization package.  With PyGraphviz you can create, edit, read,
write, and draw graphs using Python to access the Graphviz graph data
structure and layout algorithms.")
    (license license:bsd-3)))

(define-public python-uqbar
  (package
    (name "python-uqbar")
    (version "0.5.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/josiah-wolf-oberholtzer/uqbar")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1ml3x2mf7nlnvrh9lari5yk0sz2mmg39jwsbjxnpzhnw4kcwpdrs"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch
           (lambda _
             (substitute* "setup.py"
               ;; Latest versions of sphink-rtd-theme require npm to build.
               (("sphinx-rtd-theme >= 0.5.0") "sphinx-rtd-theme >= 0.2.4")
               (("black") "black >= 19.10b0"))
             #t))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "python" "-m" "pytest" "tests"))
             #t)))))
    (native-inputs
     (list graphviz
           python-flake8
           python-isort
           python-mypy
           python-pytest
           python-pytest-cov))
    (propagated-inputs
     (list python-black python-sphinx python-sphinx-rtd-theme
           python-unidecode))
    (home-page "https://github.com/josiah-wolf-oberholtzer/uqbar")
    (synopsis "Tools for building documentation with Sphinx, Graphviz and LaTeX")
    (description
     "This package contains tools for building documentation with Sphinx,
Graphviz and LaTeX.")
    (license license:expat)))

(define-public gts
  (package
    (name "gts")
    (version "0.7.6")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/gts/gts/" version
                                 "/gts-" version ".tar.gz"))
             (sha256
              (base32
               "07mqx09jxh8cv9753y2d2jsv7wp8vjmrd7zcfpbrddz3wc9kx705"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             (chmod "test/boolean/test.sh" #o777)
             #t)))

       ;; Some data files used by the test suite are missing.
       ;; See <http://sourceforge.net/p/gts/bugs/41/>.
       #:tests? #f))
    (native-inputs
     (list pkg-config))
    (propagated-inputs
     ;; The gts.pc file has glib-2.0 as required.
     (list glib))
    (home-page "http://gts.sourceforge.net/")

    ;; Note: Despite the name, this is not official GNU software.
    (synopsis "Triangulated Surface Library")
    (description
     "Library intended to provide a set of useful functions to deal with
3D surfaces meshed with interconnected triangles.")
    (license license:lgpl2.0+)))

(define-public xdot
  (package
    (name "xdot")
    (version "1.1")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "xdot" version))
      (sha256
       (base32
        "0cr4rh7dz4dfzyxrk5pzhm0d15gkrgkfp3i5lw178xy81pc56p71"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; We wrap xdot, so that we don't propagate gtk+ and graphviz
         (add-after 'install 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (wrap-program (string-append (assoc-ref outputs "out") "/bin/xdot")
               `("GI_TYPELIB_PATH" ":" prefix
                 (,(string-append
                    (assoc-ref inputs "gtk+") "/lib/girepository-1.0"
                    ":" (assoc-ref inputs "pango") "/lib/girepository-1.0"
                    ":" (assoc-ref inputs "gdk-pixbuf") "/lib/girepository-1.0"
                    ":" (assoc-ref inputs "atk") "/lib/girepository-1.0"
                    ":" (assoc-ref inputs "harfbuzz") "/lib/girepository-1.0")))
               `("PATH" ":" prefix
                 (,(dirname (search-input-file inputs "bin/dot"))))))))))
    (inputs
     (list atk
           librsvg
           harfbuzz
           graphviz
           gtk+
           python-pycairo
           python-pygobject))
    (home-page "https://pypi.org/project/xdot/")
    (synopsis "Interactive viewer for graphviz dot files")
    (description "Xdot is an interactive viewer for graphs written in
@code{graphviz}’s dot language.  Internally, it uses the xdot output format as
an intermediate format, and @code{gtk} and @code{cairo} for rendering.  Xdot
can be used either as a standalone application, or as a Python library.")
    (license license:lgpl3+)))

(define-public python-pydot
  (package
    (name "python-pydot")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pydot" version))
       (sha256
        (base32
         "0z80zwldf7ffkwrpm28hixsiqp3053j7g281xd6phmnbkfiq3014"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           ;; Taken from .travis.yaml
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (with-directory-excursion "test"
                 (invoke "python" "pydot_unittest.py"))))))))
    (native-inputs
     ;; For tests.
     (list graphviz python-chardet))
    (propagated-inputs
     ;; XXX: Two test failures with 3.0+:
     ;; https://github.com/pydot/pydot/issues/277
     (list python-pyparsing-2.4.7))
    (home-page "https://github.com/pydot/pydot")
    (synopsis "Python interface to Graphviz's DOT language")
    (description
     "Pydot provides an interface to create, handle, modify and process
graphs in Graphviz's DOT language, written in pure Python.")
    (license license:expat)))

(define-public python2-pydot
  (package-with-python2 python-pydot))

(define-public dot2tex
  (package
    (name "dot2tex")
    (version "2.11.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "dot2tex" version))
              (sha256
               (base32
                "1kp77wiv7b5qib82i3y3sn9r49rym43aaqm5aw1bwnzfbbq2m6i9"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2))
    (inputs
     (list texlive-latex-preview graphviz))
    (propagated-inputs
     (list python2-pyparsing))
    (home-page "https://github.com/kjellmf/dot2tex")
    (synopsis "Graphviz to LaTeX converter")
    (description
     "The purpose of @code{dot2tex} is to give graphs generated by Graphviz a
more LaTeX friendly look and feel.  This is accomplished by converting
@code{xdot} output from Graphviz to a series of PSTricks or PGF/TikZ commands.
This approach allows:

@itemize @bullet
@item Typesetting labels with LaTeX, allowing mathematical notation
@item Using native PSTricks and PGF/TikZ commands for drawing arrows
@item Using backend specific styles to customize the output
@end itemize")
    (license license:expat)))

(define-public gprof2dot
  (package
    (name "gprof2dot")
    (version "2021.02.21")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jrfonseca/gprof2dot")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1jjhsjf5fdi1fkn7mvhnzkh6cynl8gcjrygd3cya5mmda3akhzic"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "python" "tests/test.py")))))))
    (native-inputs
     (list graphviz))
    (home-page "https://github.com/jrfonseca/gprof2dot")
    (synopsis "Generate a dot graph from the output of several profilers")
    (description "This package provides a Python script to convert the output
from many profilers into a dot graph.

It can:

@itemize

@item prune nodes and edges below a certain threshold;
@item use an heuristic to propagate time inside mutually recursive functions;
@item use color efficiently to draw attention to hot-spots;
@item work on any platform where Python and Graphviz is available.

@end itemize")
    (license license:lgpl3+)))
