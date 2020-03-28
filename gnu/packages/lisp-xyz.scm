;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016, 2017 ng0 <ng0@n0.is>
;;; Copyright © 2016, 2017 Andy Patterson <ajpatter@uwaterloo.ca>
;;; Copyright © 2017, 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Benjamin Slade <slade@jnanam.net>
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018, 2019 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2019, 2020 Katherine Cox-Buday <cox.katherine.e@gmail.com>
;;; Copyright © 2019 Jesse Gildersleve <jessejohngildersleve@protonmail.com>
;;; Copyright © 2019, 2020 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2019 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 Konrad Hinsen <konrad.hinsen@fastmail.net>
;;; Copyright © 2020 Dimakis Dimakakos <me@bendersteed.tech>
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

;;; This file only contains Common Lisp libraries.
;;; Common Lisp compilers and tooling go to lisp.scm.
;;; Common Lisp applications should go to the most appropriate file,
;;; e.g. StumpWM is in wm.scm.

(define-module (gnu packages lisp-xyz)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix utils)
  #:use-module (guix build-system asdf)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages c)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xdisorg)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-19))

(define-public sbcl-alexandria
  (let ((revision "1")
        (commit "3b849bc0116ea70f215ee6b2fbf354e862aaa9dd"))
    (package
      (name "sbcl-alexandria")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.common-lisp.net/alexandria/alexandria.git")
               (commit commit)))
         (sha256
          (base32
           "04amwvx2vl691f0plcfbqqwxgib9zimih7jrn5zl7mbwvrxy022b"))
         (file-name (git-file-name name version))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       `(("rt" ,sbcl-rt)))
      (synopsis "Collection of portable utilities for Common Lisp")
      (description
       "Alexandria is a collection of portable utilities.  It does not contain
conceptual extensions to Common Lisp.  It is conservative in scope, and
portable between implementations.")
      (home-page "https://common-lisp.net/project/alexandria/")
      (license license:public-domain))))

(define-public cl-alexandria
  (sbcl-package->cl-source-package sbcl-alexandria))

(define-public ecl-alexandria
  (sbcl-package->ecl-package sbcl-alexandria))

(define-public sbcl-net.didierverna.asdf-flv
  (package
    (name "sbcl-net.didierverna.asdf-flv")
    (version "2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/didierverna/asdf-flv")
             (commit (string-append "version-" version))))
       (file-name (git-file-name "asdf-flv" version))
       (sha256
        (base32 "1fi2y4baxan103jbg4idjddzihy03kwnj2mzbwrknw4d4x7xlgwj"))))
    (build-system asdf-build-system/sbcl)
    (synopsis "Common Lisp ASDF extension to provide support for file-local variables")
    (description "ASDF-FLV provides support for file-local variables through
ASDF.  A file-local variable behaves like @code{*PACKAGE*} and
@code{*READTABLE*} with respect to @code{LOAD} and @code{COMPILE-FILE}: a new
dynamic binding is created before processing the file, so that any
modification to the variable becomes essentially file-local.

In order to make one or several variables file-local, use the macros
@code{SET-FILE-LOCAL-VARIABLE(S)}.")
    (home-page "https://www.lrde.epita.fr/~didier/software/lisp/misc.php#asdf-flv")
    (license (license:non-copyleft
              "https://www.gnu.org/prep/maintain/html_node/License-Notices-for-Other-Files.html"
              "GNU All-Permissive License"))))

(define-public cl-net.didierverna.asdf-flv
  (sbcl-package->cl-source-package sbcl-net.didierverna.asdf-flv))

(define-public ecl-net.didierverna.asdf-flv
  (sbcl-package->ecl-package sbcl-net.didierverna.asdf-flv))

(define-public sbcl-fiveam
  (package
    (name "sbcl-fiveam")
    (version "1.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sionescu/fiveam.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name "fiveam" version))
       (sha256
        (base32 "1q3d38pwafnwnw42clq0f8g5xw7pbzr287jl9jsqmb1vb0n1vrli"))))
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("net.didierverna.asdf-flv" ,sbcl-net.didierverna.asdf-flv)
       ("trivial-backtrace" ,sbcl-trivial-backtrace)))
    (build-system asdf-build-system/sbcl)
    (synopsis "Common Lisp testing framework")
    (description "FiveAM is a simple (as far as writing and running tests
goes) regression testing framework.  It has been designed with Common Lisp's
interactive development model in mind.")
    (home-page "https://common-lisp.net/project/fiveam/")
    (license license:bsd-3)))

(define-public cl-fiveam
  (sbcl-package->cl-source-package sbcl-fiveam))

(define-public ecl-fiveam
  (sbcl-package->ecl-package sbcl-fiveam))

(define-public sbcl-bordeaux-threads
  (package
    (name "sbcl-bordeaux-threads")
    (version "0.8.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sionescu/bordeaux-threads.git")
                    (commit (string-append "v" version))))
              (sha256
               (base32 "1whpfmyxp2fsw6viqj45fqgsifgr534c575bfh5vaqw5m84b6alp"))
              (file-name
               (git-file-name "bordeaux-threads" version))))
    (inputs `(("alexandria" ,sbcl-alexandria)))
    (native-inputs `(("fiveam" ,sbcl-fiveam)))
    (build-system asdf-build-system/sbcl)
    (synopsis "Portable shared-state concurrency library for Common Lisp")
    (description "BORDEAUX-THREADS is a proposed standard for a minimal
MP/Threading interface.  It is similar to the CLIM-SYS threading and lock
support.")
    (home-page "https://common-lisp.net/project/bordeaux-threads/")
    (license license:x11)))

(define-public cl-bordeaux-threads
  (sbcl-package->cl-source-package sbcl-bordeaux-threads))

(define-public ecl-bordeaux-threads
  (sbcl-package->ecl-package sbcl-bordeaux-threads))

(define-public sbcl-trivial-gray-streams
  (let ((revision "1")
        (commit "0483ade330508b4b2edeabdb47d16ec9437ee1cb"))
    (package
      (name "sbcl-trivial-gray-streams")
      (version (string-append "0.0.0-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/trivial-gray-streams/trivial-gray-streams.git")
           (commit commit)))
         (sha256
          (base32 "0m3rpf2x0zmdk3nf1qfa01j6a55vj7gkwhyw78qslcgbjlgh8p4d"))
         (file-name
          (string-append "trivial-gray-streams-" version "-checkout"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Compatibility layer for Gray streams implementations")
      (description "Gray streams is an interface proposed for inclusion with
ANSI CL by David N. Gray.  The proposal did not make it into ANSI CL, but most
popular CL implementations implement it.  This package provides an extremely
thin compatibility layer for gray streams.")
      (home-page "https://www.cliki.net/trivial-gray-streams")
      (license license:x11))))

(define-public cl-trivial-gray-streams
  (sbcl-package->cl-source-package sbcl-trivial-gray-streams))

(define-public ecl-trivial-gray-streams
  (sbcl-package->ecl-package sbcl-trivial-gray-streams))

(define-public sbcl-fiasco
  (let ((commit "d62f7558b21addc89f87e306f65d7f760632655f")
        (revision "1"))
    (package
      (name "sbcl-fiasco")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/joaotavora/fiasco.git")
               (commit commit)))
         (file-name (git-file-name "fiasco" version))
         (sha256
          (base32
           "1zwxs3d6iswayavcmb49z2892xhym7n556d8dnmvalc32pm9bkjh"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("trivial-gray-streams" ,sbcl-trivial-gray-streams)))
      (synopsis "Simple and powerful test framework for Common Lisp")
      (description "A Common Lisp test framework that treasures your failures,
logical continuation of Stefil.  It focuses on interactive debugging.")
      (home-page "https://github.com/joaotavora/fiasco")
      ;; LICENCE specifies this is public-domain unless the legislation
      ;; doesn't allow or recognize it.  In that case it falls back to a
      ;; permissive licence.
      (license (list license:public-domain
                     (license:x11-style "file://LICENCE"))))))

(define-public cl-fiasco
  (sbcl-package->cl-source-package sbcl-fiasco))

(define-public ecl-fiasco
  (sbcl-package->ecl-package sbcl-fiasco))

(define-public sbcl-flexi-streams
  (package
    (name "sbcl-flexi-streams")
    (version "1.0.18")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/edicl/flexi-streams.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name "flexi-streams" version))
       (sha256
        (base32 "0bjv7fd2acknidc5dyi3h85pn10krxv5jyxs1xg8jya2rlfv7f1j"))))
    (build-system asdf-build-system/sbcl)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-git-checkout-writable
           (lambda _
             (for-each make-file-writable (find-files "."))
             #t)))))
    (inputs `(("trivial-gray-streams" ,sbcl-trivial-gray-streams)))
    (synopsis "Implementation of virtual bivalent streams for Common Lisp")
    (description "Flexi-streams is an implementation of \"virtual\" bivalent
streams that can be layered atop real binary or bivalent streams and that can
be used to read and write character data in various single- or multi-octet
encodings which can be changed on the fly.  It also supplies in-memory binary
streams which are similar to string streams.")
    (home-page "http://weitz.de/flexi-streams/")
    (license license:bsd-3)))

(define-public cl-flexi-streams
  (sbcl-package->cl-source-package sbcl-flexi-streams))

(define-public ecl-flexi-streams
  (sbcl-package->ecl-package sbcl-flexi-streams))

(define-public sbcl-cl-ppcre
  (package
    (name "sbcl-cl-ppcre")
    (version "2.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/edicl/cl-ppcre.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name "cl-ppcre" version))
       (sha256
        (base32 "0dwvr29diqzcg5n6jvbk2rnd90i05l7n828hhw99khmqd0kz7xsi"))))
    (build-system asdf-build-system/sbcl)
    (native-inputs `(("flexi-streams" ,sbcl-flexi-streams)))
    (synopsis "Portable regular expression library for Common Lisp")
    (description "CL-PPCRE is a portable regular expression library for Common
Lisp, which is compatible with perl.  It is pretty fast, thread-safe, and
compatible with ANSI-compliant Common Lisp implementations.")
    (home-page "http://weitz.de/cl-ppcre/")
    (license license:bsd-2)))

(define-public cl-ppcre
  (sbcl-package->cl-source-package sbcl-cl-ppcre))

(define-public ecl-cl-ppcre
  (sbcl-package->ecl-package sbcl-cl-ppcre))

(define sbcl-cl-unicode-base
  (package
    (name "sbcl-cl-unicode-base")
    (version "0.1.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/edicl/cl-unicode.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ykx2s9lqfl74p1px0ik3l2izd1fc9jd1b4ra68s5x34rvjy0hza"))))
    (build-system asdf-build-system/sbcl)
    (arguments
     '(#:asd-file "cl-unicode.asd"
       #:asd-system-name "cl-unicode/base"))
    (inputs
     `(("cl-ppcre" ,sbcl-cl-ppcre)))
    (home-page "http://weitz.de/cl-unicode/")
    (synopsis "Portable Unicode library for Common Lisp")
    (description "CL-UNICODE is a portable Unicode library Common Lisp, which
is compatible with perl.  It is pretty fast, thread-safe, and compatible with
ANSI-compliant Common Lisp implementations.")
    (license license:bsd-2)))

(define-public sbcl-cl-unicode
  (package
    (inherit sbcl-cl-unicode-base)
    (name "sbcl-cl-unicode")
    (inputs
     `(("cl-unicode/base" ,sbcl-cl-unicode-base)
       ,@(package-inputs sbcl-cl-unicode-base)))
    (native-inputs
     `(("flexi-streams" ,sbcl-flexi-streams)))
    (arguments '())))

(define-public ecl-cl-unicode
  (sbcl-package->ecl-package sbcl-cl-unicode))

(define-public cl-unicode
  (sbcl-package->cl-source-package sbcl-cl-unicode))

(define-public sbcl-zpb-ttf
  (package
    (name "sbcl-zpb-ttf")
    (version "1.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xach/zpb-ttf.git")
             (commit (string-append "release-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1wh66vjijzqlydnrihynpwp6796917xwrh0i9li93c17kyxa74ih"))))
    (build-system asdf-build-system/sbcl)
    (home-page "https://github.com/xach/zpb-ttf")
    (synopsis "TrueType font file access for Common Lisp")
    (description
     "ZPB-TTF is a TrueType font file parser that provides an interface for
reading typographic metrics, glyph outlines, and other information from the
file.")
    (license license:bsd-2)))

(define-public ecl-zpb-ttf
  (sbcl-package->ecl-package sbcl-zpb-ttf))

(define-public cl-zpb-ttf
  (sbcl-package->cl-source-package sbcl-zpb-ttf))

(define-public sbcl-cl-aa
  (package
    (name "sbcl-cl-aa")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://projects.tuxee.net/cl-vectors/"
                           "files/cl-vectors-" version ".tar.gz"))
       (sha256
        (base32
         "04lhwi0kq8pkwhgd885pk80m1cp9sfvjjn5zj70s1dnckibhdmqh"))))
    (build-system asdf-build-system/sbcl)
    (arguments '(#:asd-file "cl-aa.asd"))
    (home-page "http://projects.tuxee.net/cl-vectors/")
    (synopsis "Polygon rasterizer")
    (description
     "This is a Common Lisp library implementing the AA polygon rasterization
algorithm from the @url{http://antigrain.com, Antigrain} project.")
    (license license:expat)))

(define-public ecl-cl-aa
  (sbcl-package->ecl-package sbcl-cl-aa))

(define-public cl-aa
  (sbcl-package->cl-source-package sbcl-cl-aa))

(define-public sbcl-cl-paths
  (package
    (inherit sbcl-cl-aa)
    (name "sbcl-cl-paths")
    (arguments '(#:asd-file "cl-paths.asd"))
    (synopsis "Facilities to create and manipulate vectorial paths")
    (description
     "This package provides facilities to create and manipulate vectorial
paths.")))

(define-public ecl-cl-paths
  (sbcl-package->ecl-package sbcl-cl-paths))

(define-public cl-paths
  (sbcl-package->cl-source-package sbcl-cl-paths))

(define-public sbcl-cl-paths-ttf
  (package
    (inherit sbcl-cl-aa)
    (name "sbcl-cl-paths-ttf")
    (arguments '(#:asd-file "cl-paths-ttf.asd"))
    (inputs
     `(("cl-paths" ,sbcl-cl-paths)
       ("zpb-ttf" ,sbcl-zpb-ttf)))
    (synopsis "Facilities to create and manipulate vectorial paths")
    (description
     "This package provides facilities to create and manipulate vectorial
paths.")))

(define-public ecl-cl-paths-ttf
  (sbcl-package->ecl-package sbcl-cl-paths-ttf))

(define-public cl-paths-ttf
  (sbcl-package->cl-source-package sbcl-cl-paths-ttf))

(define-public sbcl-cl-vectors
  (package
    (inherit sbcl-cl-aa)
    (name "sbcl-cl-vectors")
    (arguments '(#:asd-file "cl-vectors.asd"))
    (inputs
     `(("cl-aa" ,sbcl-cl-aa)
       ("cl-paths" ,sbcl-cl-paths)))
    (synopsis "Create, transform and render anti-aliased vectorial paths")
    (description
     "This is a pure Common Lisp library to create, transform and render
anti-aliased vectorial paths.")))

(define-public ecl-cl-vectors
  (sbcl-package->ecl-package sbcl-cl-vectors))

(define-public cl-vectors
  (sbcl-package->cl-source-package sbcl-cl-vectors))

(define-public sbcl-spatial-trees
  ;; There have been no releases.
  (let ((commit "81fdad0a0bf109c80a53cc96eca2e093823400ba")
        (revision "1"))
    (package
      (name "sbcl-spatial-trees")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rpav/spatial-trees.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "11rhc6h501dwcik2igkszz7b9n515cr99m5pjh4r2qfwgiri6ysa"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       '(#:tests? #f           ; spatial-trees.test requires spatial-trees.nns
         #:asd-file "spatial-trees.asd"
         #:test-asd-file "spatial-trees.test.asd"))
      (native-inputs
       `(("fiveam" ,sbcl-fiveam)))
      (home-page "https://github.com/rpav/spatial-trees")
      (synopsis "Dynamic index data structures for spatially-extended data")
      (description
       "Spatial-trees is a set of dynamic index data structures for
spatially-extended data.")
      (license license:bsd-3))))

(define-public ecl-spatial-trees
  (sbcl-package->ecl-package sbcl-spatial-trees))

(define-public cl-spatial-trees
  (sbcl-package->cl-source-package sbcl-spatial-trees))

(define-public sbcl-flexichain
  ;; There are no releases.
  (let ((commit "13d2a6c505ed0abfcd4c4ec7d7145059b06855d6")
        (revision "1"))
    (package
      (name "sbcl-flexichain")
      (version "1.5.1")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/robert-strandh/Flexichain.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0pfyvhsfbjd2sjb30grfs52r51a428xglv7bwydvpg2lc117qimg"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/robert-strandh/Flexichain.git")
      (synopsis "Dynamically add elements to or remove them from sequences")
      (description
       "This package provides an implementation of the flexichain protocol,
allowing client code to dynamically add elements to, and delete elements from
a sequence (or chain) of such elements.")
      (license license:lgpl2.1+))))

(define-public ecl-flexichain
  (sbcl-package->ecl-package sbcl-flexichain))

(define-public cl-flexichain
  (sbcl-package->cl-source-package sbcl-flexichain))

(define-public sbcl-cl-pdf
  ;; There are no releases
  (let ((commit "752e337e6d6fc206f09d091a982e7f8e5c404e4e")
        (revision "1"))
    (package
      (name "sbcl-cl-pdf")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mbattyani/cl-pdf.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1cg3k3m3r11ipb8j008y8ipynj97l3xjlpi2knqc9ndmx4r3kb1r"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("iterate" ,sbcl-iterate)
         ("zpb-ttf" ,sbcl-zpb-ttf)))
      (home-page "https://github.com/mbattyani/cl-pdf")
      (synopsis "Common Lisp library for generating PDF files")
      (description
       "CL-PDF is a cross-platform Common Lisp library for generating PDF
files.")
      (license license:bsd-2))))

(define-public ecl-cl-pdf
  (sbcl-package->ecl-package sbcl-cl-pdf))

(define-public cl-pdf
  (sbcl-package->cl-source-package sbcl-cl-pdf))

(define-public sbcl-clx
  (package
    (name "sbcl-clx")
    (version "0.7.5")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/sharplispers/clx.git")
         (commit version)))
       (sha256
        (base32
         "1vi67z9hpj5rr4xcmfbfwzmlcc0ah7hzhrmfid6lqdkva238v2wf"))
       (file-name (string-append "clx-" version))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     `(("fiasco" ,sbcl-fiasco)))
    (home-page "https://www.cliki.net/portable-clx")
    (synopsis "X11 client library for Common Lisp")
    (description "CLX is an X11 client library for Common Lisp.  The code was
originally taken from a CMUCL distribution, was modified somewhat in order to
make it compile and run under SBCL, then a selection of patches were added
from other CLXes around the net.")
    (license license:x11)))

(define-public cl-clx
  (sbcl-package->cl-source-package sbcl-clx))

(define-public ecl-clx
  (sbcl-package->ecl-package sbcl-clx))

(define-public sbcl-cl-ppcre-unicode
  (package (inherit sbcl-cl-ppcre)
    (name "sbcl-cl-ppcre-unicode")
    (arguments
     `(#:tests? #f ; tests fail with "Component :CL-PPCRE-TEST not found"
       #:asd-file "cl-ppcre-unicode.asd"))
    (inputs
     `(("sbcl-cl-ppcre" ,sbcl-cl-ppcre)
       ("sbcl-cl-unicode" ,sbcl-cl-unicode)))))

(define-public ecl-cl-ppcre-unicode
  (sbcl-package->ecl-package sbcl-cl-ppcre-unicode))

;; The slynk that users expect to install includes all of slynk's contrib
;; modules.  Therefore, we build the base module and all contribs first; then
;; we expose the union of these as `sbcl-slynk'.  The following variable
;; describes the base module.
(define sbcl-slynk-boot0
  (let ((revision "2")
        (commit "cbf84c36c4eca8b032e3fd16177a7bc02df3ec4c"))
    (package
      (name "sbcl-slynk-boot0")
      (version (string-append "1.0.0-beta-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/joaotavora/sly.git")
           (commit commit)))
         (sha256
          (base32 "13dyhsravn591p7g6is01mp2ynzjnnj7pwgi57r6xqmd4611y9vh"))
         (file-name (string-append "slynk-" version "-checkout"))
         (modules '((guix build utils)
                    (ice-9 ftw)))
         (snippet
          '(begin
             ;; Move the contribs into the main source directory for easier
             ;; access
             (substitute* "slynk/slynk.asd"
               (("\\.\\./contrib")
                "contrib")
               (("\\(defsystem :slynk/util")
                "(defsystem :slynk/util :depends-on (:slynk)")
               ((":depends-on \\(:slynk :slynk/util\\)")
                ":depends-on (:slynk :slynk-util)"))
             (substitute* "contrib/slynk-trace-dialog.lisp"
               (("\\(slynk::reset-inspector\\)") ; Causes problems on load
                "nil"))
             (substitute* "contrib/slynk-profiler.lisp"
               (("slynk:to-line")
                "slynk-pprint-to-line"))
             (substitute* "contrib/slynk-fancy-inspector.lisp"
               (("slynk/util") "slynk-util")
               ((":compile-toplevel :load-toplevel") ""))
             (rename-file "contrib" "slynk/contrib")
             ;; Move slynk's contents into the base directory for easier
             ;; access
             (for-each (lambda (file)
                         (unless (string-prefix? "." file)
                           (rename-file (string-append "slynk/" file)
                                        (string-append "./" (basename file)))))
                       (scandir "slynk"))
             #t))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:tests? #f ; No test suite
         #:asd-system-name "slynk"))
      (synopsis "Common Lisp IDE for Emacs")
      (description "SLY is a fork of SLIME, an IDE backend for Common Lisp.
It also features a completely redesigned REPL based on Emacs's own
full-featured comint.el, live code annotations, and a consistent interactive
button interface.  Everything can be copied to the REPL.  One can create
multiple inspectors with independent history.")
      (home-page "https://github.com/joaotavora/sly")
      (license license:public-domain)
      (properties `((cl-source-variant . ,(delay cl-slynk)))))))

(define-public cl-slynk
  (package
    (inherit (sbcl-package->cl-source-package sbcl-slynk-boot0))
    (name "cl-slynk")))

(define ecl-slynk-boot0
  (sbcl-package->ecl-package sbcl-slynk-boot0))

(define sbcl-slynk-arglists
  (package
    (inherit sbcl-slynk-boot0)
    (name "sbcl-slynk-arglists")
    (inputs `(("slynk" ,sbcl-slynk-boot0)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-slynk-boot0)
       ((#:asd-file _ "") "slynk.asd")
       ((#:asd-system-name _ #f) "slynk/arglists")))))

(define ecl-slynk-arglists
  (sbcl-package->ecl-package sbcl-slynk-arglists))

(define sbcl-slynk-util
  (package
    (inherit sbcl-slynk-boot0)
    (name "sbcl-slynk-util")
    (inputs `(("slynk" ,sbcl-slynk-boot0)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-slynk-boot0)
       ((#:asd-file _ "") "slynk.asd")
       ((#:asd-system-name _ #f) "slynk/util")))))

(define ecl-slynk-util
  (sbcl-package->ecl-package sbcl-slynk-util))

(define sbcl-slynk-fancy-inspector
  (package
    (inherit sbcl-slynk-arglists)
    (name "sbcl-slynk-fancy-inspector")
    (inputs `(("slynk-util" ,sbcl-slynk-util)
              ,@(package-inputs sbcl-slynk-arglists)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-slynk-arglists)
       ((#:asd-system-name _ #f) "slynk/fancy-inspector")))))

(define ecl-slynk-fancy-inspector
  (sbcl-package->ecl-package sbcl-slynk-fancy-inspector))

(define sbcl-slynk-package-fu
  (package
    (inherit sbcl-slynk-arglists)
    (name "sbcl-slynk-package-fu")
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-slynk-arglists)
       ((#:asd-system-name _ #f) "slynk/package-fu")))))

(define ecl-slynk-package-fu
  (sbcl-package->ecl-package sbcl-slynk-package-fu))

(define sbcl-slynk-mrepl
  (package
    (inherit sbcl-slynk-fancy-inspector)
    (name "sbcl-slynk-mrepl")
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-slynk-arglists)
       ((#:asd-system-name _ #f) "slynk/mrepl")))))

(define ecl-slynk-mrepl
  (sbcl-package->ecl-package sbcl-slynk-mrepl))

(define sbcl-slynk-trace-dialog
  (package
    (inherit sbcl-slynk-arglists)
    (name "sbcl-slynk-trace-dialog")
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-slynk-arglists)
       ((#:asd-system-name _ #f) "slynk/trace-dialog")))))

(define ecl-slynk-trace-dialog
  (sbcl-package->ecl-package sbcl-slynk-trace-dialog))

(define sbcl-slynk-profiler
  (package
    (inherit sbcl-slynk-arglists)
    (name "sbcl-slynk-profiler")
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-slynk-arglists)
       ((#:asd-system-name _ #f) "slynk/profiler")))))

(define ecl-slynk-profiler
  (sbcl-package->ecl-package sbcl-slynk-profiler))

(define sbcl-slynk-stickers
  (package
    (inherit sbcl-slynk-arglists)
    (name "sbcl-slynk-stickers")
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-slynk-arglists)
       ((#:asd-system-name _ #f) "slynk/stickers")))))

(define ecl-slynk-stickers
  (sbcl-package->ecl-package sbcl-slynk-stickers))

(define sbcl-slynk-indentation
  (package
    (inherit sbcl-slynk-arglists)
    (name "sbcl-slynk-indentation")
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-slynk-arglists)
       ((#:asd-system-name _ #f) "slynk/indentation")))))

(define ecl-slynk-indentation
  (sbcl-package->ecl-package sbcl-slynk-indentation))

(define sbcl-slynk-retro
  (package
    (inherit sbcl-slynk-arglists)
    (name "sbcl-slynk-retro")
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-slynk-arglists)
       ((#:asd-system-name _ #f) "slynk/retro")))))

(define ecl-slynk-retro
  (sbcl-package->ecl-package sbcl-slynk-retro))

(define slynk-systems
  '("slynk"
    "slynk-util"
    "slynk-arglists"
    "slynk-fancy-inspector"
    "slynk-package-fu"
    "slynk-mrepl"
    "slynk-profiler"
    "slynk-trace-dialog"
    "slynk-stickers"
    "slynk-indentation"
    "slynk-retro"))

(define-public sbcl-slynk
  (package
    (inherit sbcl-slynk-boot0)
    (name "sbcl-slynk")
    (inputs
     `(("slynk" ,sbcl-slynk-boot0)
       ("slynk-util" ,sbcl-slynk-util)
       ("slynk-arglists" ,sbcl-slynk-arglists)
       ("slynk-fancy-inspector" ,sbcl-slynk-fancy-inspector)
       ("slynk-package-fu" ,sbcl-slynk-package-fu)
       ("slynk-mrepl" ,sbcl-slynk-mrepl)
       ("slynk-profiler" ,sbcl-slynk-profiler)
       ("slynk-trace-dialog" ,sbcl-slynk-trace-dialog)
       ("slynk-stickers" ,sbcl-slynk-stickers)
       ("slynk-indentation" ,sbcl-slynk-indentation)
       ("slynk-retro" ,sbcl-slynk-retro)))
    (native-inputs `(("sbcl" ,sbcl)))
    (build-system trivial-build-system)
    (source #f)
    (outputs '("out" "image"))
    (arguments
     `(#:modules ((guix build union)
                  (guix build utils)
                  (guix build lisp-utils))
       #:builder
       (begin
         (use-modules (ice-9 match)
                      (srfi srfi-1)
                      (guix build union)
                      (guix build lisp-utils))

         (union-build
          (assoc-ref %outputs "out")
          (filter-map
           (match-lambda
             ((name . path)
              (if (string-prefix? "slynk" name) path #f)))
           %build-inputs))

         (prepend-to-source-registry
          (string-append (assoc-ref %outputs "out") "//"))

         (parameterize ((%lisp-type "sbcl")
                        (%lisp (string-append (assoc-ref %build-inputs "sbcl")
                                              "/bin/sbcl")))
           (build-image (string-append
                         (assoc-ref %outputs "image")
                         "/bin/slynk")
                        %outputs
                        #:dependencies ',slynk-systems))
         #t)))))

(define-public ecl-slynk
  (package
    (inherit sbcl-slynk)
    (name "ecl-slynk")
    (inputs
     (map (match-lambda
            ((name pkg . _)
             (list name (sbcl-package->ecl-package pkg))))
          (package-inputs sbcl-slynk)))
    (native-inputs '())
    (outputs '("out"))
    (arguments
     '(#:modules ((guix build union))
       #:builder
       (begin
         (use-modules (ice-9 match)
                      (guix build union))
         (match %build-inputs
           (((names . paths) ...)
            (union-build (assoc-ref %outputs "out")
                         paths)
            #t)))))))

(define-public sbcl-parse-js
  (let ((commit "fbadc6029bec7039602abfc06c73bb52970998f6")
        (revision "1"))
    (package
      (name "sbcl-parse-js")
      (version (string-append "0.0.0-" revision "." (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "http://marijn.haverbeke.nl/git/parse-js")
               (commit commit)))
         (file-name (string-append name "-" commit "-checkout"))
         (sha256
          (base32
           "1wddrnr5kiya5s3gp4cdq6crbfy9fqcz7fr44p81502sj3bvdv39"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://marijnhaverbeke.nl/parse-js/")
      (synopsis "Parse JavaScript")
      (description "Parse-js is a Common Lisp package for parsing
JavaScript (ECMAScript 3).  It has basic support for ECMAScript 5.")
      (license license:zlib))))

(define-public cl-parse-js
  (sbcl-package->cl-source-package sbcl-parse-js))

(define-public sbcl-parse-number
  (package
    (name "sbcl-parse-number")
    (version "1.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/sharplispers/parse-number/")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0sk06ib1bhqv9y39vwnnw44vmbc4b0kvqm37xxmkxd4dwchq82d7"))))
    (build-system asdf-build-system/sbcl)
    (home-page "https://www.cliki.net/PARSE-NUMBER")
    (synopsis "Parse numbers")
    (description "@code{parse-number} is a library of functions for parsing
strings into one of the standard Common Lisp number types without using the
reader.  @code{parse-number} accepts an arbitrary string and attempts to parse
the string into one of the standard Common Lisp number types, if possible, or
else @code{parse-number} signals an error of type @code{invalid-number}.")
    (license license:bsd-3)))

(define-public cl-parse-number
  (sbcl-package->cl-source-package sbcl-parse-number))

(define-public sbcl-iterate
  (package
    (name "sbcl-iterate")
    (version "1.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://common-lisp.net/project/iterate/releases/"
                           "iterate-" version ".tar.gz"))
       (sha256
        (base32
         "1lqsbhrkfl0yif46aymvb7l3nb9wdcmj4jyw485blj32jb4famzn"))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     `(("rt" ,sbcl-rt)))
    (home-page "https://common-lisp.net/project/iterate/")
    (synopsis "Iteration construct for Common Lisp")
    (description "@code{iterate} is an iteration construct for Common Lisp.
It is similar to the @code{CL:LOOP} macro, with these distinguishing marks:

@itemize
@item it is extensible,
@item it helps editors like Emacs indent iterate forms by having a more
  lisp-like syntax, and
@item it isn't part of the ANSI standard for Common Lisp.
@end itemize\n")
    (license license:expat)))

(define-public cl-iterate
  (sbcl-package->cl-source-package sbcl-iterate))

(define-public ecl-iterate
  (sbcl-package->ecl-package sbcl-iterate))

(define-public sbcl-cl-uglify-js
  ;; There have been many bug fixes since the 2010 release.
  (let ((commit "429c5e1d844e2f96b44db8fccc92d6e8e28afdd5")
        (revision "1"))
    (package
      (name "sbcl-cl-uglify-js")
      (version (string-append "0.1-" revision "." (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mishoo/cl-uglify-js.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0k39y3c93jgxpr7gwz7w0d8yknn1fdnxrjhd03057lvk5w8js27a"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("sbcl-parse-js" ,sbcl-parse-js)
         ("sbcl-cl-ppcre" ,sbcl-cl-ppcre)
         ("sbcl-cl-ppcre-unicode" ,sbcl-cl-ppcre-unicode)
         ("sbcl-parse-number" ,sbcl-parse-number)
         ("sbcl-iterate" ,sbcl-iterate)))
      (home-page "https://github.com/mishoo/cl-uglify-js")
      (synopsis "JavaScript compressor library for Common Lisp")
      (description "This is a Common Lisp version of UglifyJS, a JavaScript
compressor.  It works on data produced by @code{parse-js} to generate a
@dfn{minified} version of the code.  Currently it can:

@itemize
@item reduce variable names (usually to single letters)
@item join consecutive @code{var} statements
@item resolve simple binary expressions
@item group most consecutive statements using the @code{sequence} operator (comma)
@item remove unnecessary blocks
@item convert @code{IF} expressions in various ways that result in smaller code
@item remove some unreachable code
@end itemize\n")
      (license license:zlib))))

(define-public cl-uglify-js
  (sbcl-package->cl-source-package sbcl-cl-uglify-js))

(define-public uglify-js
  (package
    (inherit sbcl-cl-uglify-js)
    (name "uglify-js")
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (let* ((bin    (string-append (assoc-ref %outputs "out") "/bin/"))
              (script (string-append bin "uglify-js")))
         (use-modules (guix build utils))
         (mkdir-p bin)
         (with-output-to-file script
           (lambda _
             (format #t "#!~a/bin/sbcl --script
 (require :asdf)
 (push (truename \"~a/lib/sbcl\") asdf:*central-registry*)"
                     (assoc-ref %build-inputs "sbcl")
                     (assoc-ref %build-inputs "sbcl-cl-uglify-js"))
             ;; FIXME: cannot use progn here because otherwise it fails to
             ;; find cl-uglify-js.
             (for-each
              write
              '(;; Quiet, please!
                (let ((*standard-output* (make-broadcast-stream))
                      (*error-output* (make-broadcast-stream)))
                  (asdf:load-system :cl-uglify-js))
                (let ((file (cadr *posix-argv*)))
                  (if file
                      (format t "~a"
                              (cl-uglify-js:ast-gen-code
                               (cl-uglify-js:ast-mangle
                                (cl-uglify-js:ast-squeeze
                                 (with-open-file (in file)
                                                 (parse-js:parse-js in))))
                               :beautify nil))
                      (progn
                       (format *error-output*
                               "Please provide a JavaScript file.~%")
                       (sb-ext:exit :code 1))))))))
         (chmod script #o755)
         #t)))
    (inputs
     `(("sbcl" ,sbcl)
       ("sbcl-cl-uglify-js" ,sbcl-cl-uglify-js)))
    (synopsis "JavaScript compressor")))

(define-public sbcl-cl-strings
  (let ((revision "1")
        (commit "c5c5cbafbf3e6181d03c354d66e41a4f063f00ae"))
    (package
      (name "sbcl-cl-strings")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/diogoalexandrefranco/cl-strings")
               (commit commit)))
         (sha256
          (base32
           "00754mfaqallj480lwd346nkfb6ra8pa8xcxcylf4baqn604zlmv"))
         (file-name (string-append "cl-strings-" version "-checkout"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Portable, dependency-free set of utilities to manipulate strings in Common Lisp")
      (description
       "@command{cl-strings} is a small, portable, dependency-free set of
utilities that make it even easier to manipulate text in Common Lisp.  It has
100% test coverage and works at least on sbcl, ecl, ccl, abcl and clisp.")
      (home-page "https://github.com/diogoalexandrefranco/cl-strings")
      (license license:expat))))

(define-public cl-strings
  (sbcl-package->cl-source-package sbcl-cl-strings))

(define-public ecl-cl-strings
  (sbcl-package->ecl-package sbcl-cl-strings))

(define-public sbcl-trivial-features
  (package
    (name "sbcl-trivial-features")
    (version "0.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/trivial-features/trivial-features.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name "trivial-features" version))
       (sha256
        (base32 "0ccv7dqyrk55xga78i5vzlic7mdwp28in3g1a8fqhlk6626scsq9"))))
    (build-system asdf-build-system/sbcl)
    (arguments '(#:tests? #f))
    (home-page "https://cliki.net/trivial-features")
    (synopsis "Ensures consistency of @code{*FEATURES*} in Common Lisp")
    (description "Trivial-features ensures that @code{*FEATURES*} is
consistent across multiple Common Lisp implementations.")
    (license license:expat)))

(define-public cl-trivial-features
  (sbcl-package->cl-source-package sbcl-trivial-features))

(define-public ecl-trivial-features
  (sbcl-package->ecl-package sbcl-trivial-features))

(define-public sbcl-hu.dwim.asdf
  (package
    (name "sbcl-hu.dwim.asdf")
    (version "20190521")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://beta.quicklisp.org/archive/hu.dwim.asdf/"
                           "2019-05-21/hu.dwim.asdf-" version "-darcs.tgz"))
       (sha256
        (base32
         "0rsbv71vyszy8w35yjwb5h6zcmknjq223hkzir79y72qdsc6sabn"))))
    (build-system asdf-build-system/sbcl)
    (home-page "https://hub.darcs.net/hu.dwim/hu.dwim.asdf")
    (synopsis "Extensions to ASDF")
    (description "Various ASDF extensions such as attached test and
documentation system, explicit development support, etc.")
    (license license:public-domain)))

(define-public cl-hu.dwim.asdf
  (sbcl-package->cl-source-package sbcl-hu.dwim.asdf))

(define-public ecl-hu.dwim.asdf
  (sbcl-package->ecl-package sbcl-hu.dwim.asdf))

(define-public sbcl-hu.dwim.stefil
  (let ((commit "ab6d1aa8995878a1b66d745dfd0ba021090bbcf9"))
    (package
      (name "sbcl-hu.dwim.stefil")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://gitlab.common-lisp.net/xcvb/hu.dwim.stefil.git")
           (commit commit)))
         (sha256
          (base32 "1d8yccw65zj3zh46cbi3x6nmn1dwdb76s9d0av035077mvyirqqp"))
         (file-name (git-file-name "hu.dwim.stefil" version))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       `(("asdf:cl-hu.dwim.asdf" ,sbcl-hu.dwim.asdf)))
      (inputs
       `(("sbcl-alexandria" ,sbcl-alexandria)))
      (home-page "https://hub.darcs.net/hu.dwim/hu.dwim.stefil")
      (synopsis "Simple test framework")
      (description "Stefil is a simple test framework for Common Lisp,
with a focus on interactive development.")
      (license license:public-domain))))

(define-public cl-hu.dwim.stefil
  (sbcl-package->cl-source-package sbcl-hu.dwim.stefil))

(define-public ecl-hu.dwim.stefil
  (sbcl-package->ecl-package sbcl-hu.dwim.stefil))

(define-public sbcl-babel
  (package
    (name "sbcl-babel")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cl-babel/babel.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name "babel" version))
       (sha256
        (base32 "139a8rn2gnhj082n8jg01gc8fyr63hkj57hgrnmb3d1r327yc77f"))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     `(("tests:cl-hu.dwim.stefil" ,sbcl-hu.dwim.stefil)))
    (inputs
     `(("sbcl-alexandria" ,sbcl-alexandria)
       ("sbcl-trivial-features" ,sbcl-trivial-features)))
    (home-page "https://common-lisp.net/project/babel/")
    (synopsis "Charset encoding and decoding library")
    (description "Babel is a charset encoding and decoding library, not unlike
GNU libiconv, but completely written in Common Lisp.")
    (license license:expat)))

(define-public cl-babel
  (sbcl-package->cl-source-package sbcl-babel))

(define-public ecl-babel
  (sbcl-package->ecl-package sbcl-babel))

(define-public sbcl-cl-yacc
  (package
    (name "sbcl-cl-yacc")
    (version "0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jech/cl-yacc")
             (commit (string-append "cl-yacc-" version))))
       (sha256
        (base32
         "16946pzf8vvadnyfayvj8rbh4zjzw90h0azz2qk1mxrvhh5wklib"))
       (file-name (string-append "cl-yacc-" version "-checkout"))))
    (build-system asdf-build-system/sbcl)
    (arguments
     `(#:asd-file "yacc.asd"
       #:asd-system-name "yacc"))
    (synopsis "LALR(1) parser generator for Common Lisp, similar in spirit to Yacc")
    (description
     "CL-Yacc is a LALR(1) parser generator for Common Lisp, similar in spirit
to AT&T Yacc, Berkeley Yacc, GNU Bison, Zebu, lalr.cl or lalr.scm.

CL-Yacc uses the algorithm due to Aho and Ullman, which is the one also used
by AT&T Yacc, Berkeley Yacc and Zebu.  It does not use the faster algorithm due
to DeRemer and Pennello, which is used by Bison and lalr.scm (not lalr.cl).")
    (home-page "https://www.irif.fr/~jch//software/cl-yacc/")
    (license license:expat)))

(define-public cl-yacc
  (sbcl-package->cl-source-package sbcl-cl-yacc))

(define-public ecl-cl-yacc
  (sbcl-package->ecl-package sbcl-cl-yacc))

(define-public sbcl-jpl-util
  (let ((commit "0311ed374e19a49d43318064d729fe3abd9a3b62"))
    (package
      (name "sbcl-jpl-util")
      (version "20151005")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               ;; Quicklisp uses this fork.
               (url "https://github.com/hawkir/cl-jpl-util")
               (commit commit)))
         (file-name
          (git-file-name "jpl-util" version))
         (sha256
          (base32
           "0nc0rk9n8grkg3045xsw34whmcmddn2sfrxki4268g7kpgz0d2yz"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Collection of Common Lisp utility functions and macros")
      (description
       "@command{cl-jpl-util} is a collection of Common Lisp utility functions
and macros, primarily for software projects written in CL by the author.")
      (home-page "https://www.thoughtcrime.us/software/cl-jpl-util/")
      (license license:isc))))

(define-public cl-jpl-util
  (sbcl-package->cl-source-package sbcl-jpl-util))

(define-public ecl-jpl-util
  (sbcl-package->ecl-package sbcl-jpl-util))

(define-public sbcl-jpl-queues
  (package
    (name "sbcl-jpl-queues")
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://www.thoughtcrime.us/software/jpl-queues/jpl-queues-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1wvvv7j117h9a42qaj1g4fh4mji28xqs7s60rn6d11gk9jl76h96"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     `(("jpl-util" ,sbcl-jpl-util)
       ("bordeaux-threads" ,sbcl-bordeaux-threads)))
    (arguments
     ;; Tests seem to be broken.
     `(#:tests? #f))
    (synopsis "Common Lisp library implementing a few different kinds of queues")
    (description
     "A Common Lisp library implementing a few different kinds of queues:

@itemize
@item Bounded and unbounded FIFO queues.
@item Lossy bounded FIFO queues that drop elements when full.
@item Unbounded random-order queues that use less memory than unbounded FIFO queues.
@end itemize

Additionally, a synchronization wrapper is provided to make any queue
conforming to the @command{jpl-queues} API thread-safe for lightweight
multithreading applications.  (See Calispel for a more sophisticated CL
multithreaded message-passing library with timeouts and alternation among
several blockable channels.)")
    (home-page "https://www.thoughtcrime.us/software/jpl-queues/")
    (license license:isc)))

(define-public cl-jpl-queues
  (sbcl-package->cl-source-package sbcl-jpl-queues))

(define-public ecl-jpl-queues
  (sbcl-package->ecl-package sbcl-jpl-queues))

(define-public sbcl-eos
  (let ((commit "b0faca83781ead9a588661e37bd47f90362ccd94"))
    (package
      (name "sbcl-eos")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/adlai/Eos")
               (commit commit)))
         (sha256
          (base32
           "1bq8cfg087iyxmxi1mwgx5cfgy3b8ydrf81xljcis8qbgb2vszph"))
         (file-name (git-file-name "eos" version))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Unit Testing for Common Lisp")
      (description
       "Eos was a unit testing library for Common Lisp.
It began as a fork of FiveAM; however, FiveAM development has continued, while
that of Eos has not.  Thus, Eos is now deprecated in favor of FiveAM.")
      (home-page "https://github.com/adlai/Eos")
      (license license:expat))))

(define-public cl-eos
  (sbcl-package->cl-source-package sbcl-eos))

(define-public ecl-eos
  (sbcl-package->ecl-package sbcl-eos))

(define-public sbcl-esrap
  (let ((commit "133be8b05c2aae48696fe5b739eea2fa573fa48d"))
    (package
      (name "sbcl-esrap")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/nikodemus/esrap")
               (commit commit)))
         (sha256
          (base32
           "02d5clihsdryhf7pix8c5di2571fdsffh75d40fkzhws90r5mksl"))
         (file-name (git-file-name "esrap" version))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       `(("eos" ,sbcl-eos)))            ;For testing only.
      (inputs
       `(("alexandria" ,sbcl-alexandria)))
      (synopsis "Common Lisp packrat parser")
      (description
       "A packrat parser for Common Lisp.
In addition to regular Packrat / Parsing Grammar / TDPL features ESRAP supports:

@itemize
@item dynamic redefinition of nonterminals
@item inline grammars
@item semantic predicates
@item introspective facilities (describing grammars, tracing, setting breaks)
@end itemize\n")
      (home-page "https://nikodemus.github.io/esrap/")
      (license license:expat))))

(define-public cl-esrap
  (sbcl-package->cl-source-package sbcl-esrap))

(define-public ecl-esrap
  (sbcl-package->ecl-package sbcl-esrap))

(define-public sbcl-split-sequence
  (package
    (name "sbcl-split-sequence")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sharplispers/split-sequence")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "0jcpnx21hkfwqj5fvp7kc6pn1qcz9hk7g2s5x8h0349x1j2irln0"))
       (file-name (git-file-name "split-sequence" version))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     `(("fiveam" ,sbcl-fiveam)))
    (synopsis "Member of the Common Lisp Utilities family of programs")
    (description
     "Splits sequence into a list of subsequences delimited by objects
satisfying the test.")
    (home-page "https://cliki.net/split-sequence")
    (license license:expat)))

(define-public cl-split-sequence
  (sbcl-package->cl-source-package sbcl-split-sequence))

(define-public ecl-split-sequence
  (sbcl-package->ecl-package sbcl-split-sequence))

(define-public sbcl-html-encode
  (package
    (name "sbcl-html-encode")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://beta.quicklisp.org/archive/html-encode/2010-10-06/html-encode-"
             version ".tgz"))
       (sha256
        (base32
         "06mf8wn95yf5swhmzk4vp0xr4ylfl33dgfknkabbkd8n6jns8gcf"))
       (file-name (string-append "colorize" version "-checkout"))))
    (build-system asdf-build-system/sbcl)
    (synopsis "Common Lisp library for encoding text in various web-savvy encodings")
    (description
     "A library for encoding text in various web-savvy encodings.")
    (home-page "http://quickdocs.org/html-encode/")
    (license license:expat)))

(define-public cl-html-encode
  (sbcl-package->cl-source-package sbcl-html-encode))

(define-public ecl-html-encode
  (sbcl-package->ecl-package sbcl-html-encode))

(define-public sbcl-colorize
  (let ((commit "ea676b584e0899cec82f21a9e6871172fe3c0eb5"))
    (package
      (name "sbcl-colorize")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/kingcons/colorize")
               (commit commit)))
         (sha256
          (base32
           "1pdg4kiaczmr3ivffhirp7m3lbr1q27rn7dhaay0vwghmi31zcw9"))
         (file-name (git-file-name "colorize" version))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("split-sequence" ,sbcl-split-sequence)
         ("html-encode" ,sbcl-html-encode)))
      (synopsis "Common Lisp for syntax highlighting")
      (description
       "@command{colorize} is a Lisp library for syntax highlighting
supporting the following languages: Common Lisp, Emacs Lisp, Scheme, Clojure,
C, C++, Java, Python, Erlang, Haskell, Objective-C, Diff, Webkit.")
      (home-page "https://github.com/kingcons/colorize")
      ;; TODO: Missing license?
      (license license:expat))))

(define-public cl-colorize
  (sbcl-package->cl-source-package sbcl-colorize))

(define-public ecl-colorize
  (sbcl-package->ecl-package sbcl-colorize))

(define-public sbcl-3bmd
  (let ((commit "192ea13435b605a96ef607df51317056914cabbd"))
    (package
      (name "sbcl-3bmd")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/3b/3bmd")
               (commit commit)))
         (sha256
          (base32
           "1rgv3gi7wf963ikmmpk132wgn0icddf226gq3bmcnk1fr3v9gf2f"))
         (file-name (git-file-name "3bmd" version))))
      (build-system asdf-build-system/sbcl)
      (arguments
       ;; FIXME: We need to specify the name because the build-system thinks
       ;; "3" is a version marker.
       `(#:asd-system-name "3bmd"))
      (inputs
       `(("esrap" ,sbcl-esrap)
         ("split-sequence" ,sbcl-split-sequence)))
      (synopsis "Markdown processor in Command Lisp using esrap parser")
      (description
       "Common Lisp Markdown -> HTML converter, using @command{esrap} for
parsing, and grammar based on @command{peg-markdown}.")
      (home-page "https://github.com/3b/3bmd")
      (license license:expat))))

(define-public cl-3bmd
  (sbcl-package->cl-source-package sbcl-3bmd))

(define-public ecl-3bmd
  (sbcl-package->ecl-package sbcl-3bmd))

(define-public sbcl-3bmd-ext-code-blocks
  (let ((commit "192ea13435b605a96ef607df51317056914cabbd"))
    (package
      (inherit sbcl-3bmd)
      (name "sbcl-3bmd-ext-code-blocks")
      (arguments
       `(#:asd-system-name "3bmd-ext-code-blocks"
         #:asd-file "3bmd-ext-code-blocks.asd"))
      (inputs
       `(("3bmd" ,sbcl-3bmd)
         ("colorize" ,sbcl-colorize)))
      (synopsis "3bmd extension which adds support for GitHub-style fenced
code blocks")
      (description
       "3bmd extension which adds support for GitHub-style fenced code blocks,
with @command{colorize} support."))))

(define-public cl-3bmd-ext-code-blocks
  (sbcl-package->cl-source-package sbcl-3bmd-ext-code-blocks))

(define-public ecl-3bmd-ext-code-blocks
  (sbcl-package->ecl-package sbcl-3bmd-ext-code-blocks))

(define-public sbcl-cl-fad
  (package
    (name "sbcl-cl-fad")
    (version "0.7.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/edicl/cl-fad/")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "1gc8i82v6gks7g0lnm54r4prk2mklidv2flm5fvbr0a7rsys0vpa"))
       (file-name (string-append "cl-fad" version "-checkout"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     `(("bordeaux-threads" ,sbcl-bordeaux-threads)))
    (synopsis "Portable pathname library for Common Lisp")
    (description
     "CL-FAD (for \"Files and Directories\") is a thin layer atop Common
Lisp's standard pathname functions.  It is intended to provide some
unification between current CL implementations on Windows, OS X, Linux, and
Unix.  Most of the code was written by Peter Seibel for his book Practical
Common Lisp.")
    (home-page "https://edicl.github.io/cl-fad/")
    (license license:bsd-2)))

(define-public cl-fad
  (sbcl-package->cl-source-package sbcl-cl-fad))

(define-public ecl-cl-fad
  (sbcl-package->ecl-package sbcl-cl-fad))

(define-public sbcl-rt
  (let ((commit "a6a7503a0b47953bc7579c90f02a6dba1f6e4c5a")
        (revision "1"))
    (package
      (name "sbcl-rt")
      (version (git-version "1990.12.19" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "http://git.kpe.io/rt.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "13si2rrxaagbr0bkvg6sqicxxpyshabx6ad6byc9n2ik5ysna69b"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "MIT Regression Tester")
      (description
       "RT provides a framework for writing regression test suites.")
      (home-page "https://www.cliki.net/rt")
      (license license:expat))))

(define-public cl-rt
  (sbcl-package->cl-source-package sbcl-rt))

(define-public ecl-rt
  (sbcl-package->ecl-package sbcl-rt))

(define-public sbcl-nibbles
  (package
    (name "sbcl-nibbles")
    (version "0.14")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sharplispers/nibbles/")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "1v7qfgpvdr6nz7v63dj69d26dis0kff3rd8xamr1llfdvza2pm8f"))
       (file-name (git-file-name "nibbles" version))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     ;; Tests only.
     `(("rt" ,sbcl-rt)))
    (synopsis "Common Lisp library for accessing octet-addressed blocks of data")
    (description
     "When dealing with network protocols and file formats, it's common to
have to read or write 16-, 32-, or 64-bit datatypes in signed or unsigned
flavors.  Common Lisp sort of supports this by specifying :element-type for
streams, but that facility is underspecified and there's nothing similar for
read/write from octet vectors.  What most people wind up doing is rolling their
own small facility for their particular needs and calling it a day.

This library attempts to be comprehensive and centralize such
facilities.  Functions to read 16-, 32-, and 64-bit quantities from octet
vectors in signed or unsigned flavors are provided; these functions are also
SETFable.  Since it's sometimes desirable to read/write directly from streams,
functions for doing so are also provided.  On some implementations,
reading/writing IEEE singles/doubles (i.e. single-float and double-float) will
also be supported.")
    (home-page "https://github.com/sharplispers/nibbles")
    (license license:bsd-3)))

(define-public cl-nibbles
  (sbcl-package->cl-source-package sbcl-nibbles))

(define-public ecl-nibbles
  (sbcl-package->ecl-package sbcl-nibbles))

(define-public sbcl-ironclad
  (package
    (name "sbcl-ironclad")
    (version "0.48")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sharplispers/ironclad/")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "1wzczpgvgjc5h8ghz75kxi7iykmqxqchdhgdhkif9j99kyqvbyam"))
       (file-name (git-file-name name version))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     ;; Tests only.
     `(("rt" ,sbcl-rt)))
    (inputs
     `(("bordeaux-threads" ,sbcl-bordeaux-threads)
       ("flexi-streams" ,sbcl-flexi-streams)))
    (synopsis "Cryptographic toolkit written in Common Lisp")
    (description
     "Ironclad is a cryptography library written entirely in Common Lisp.
It includes support for several popular ciphers, digests, MACs and public key
cryptography algorithms.  For several implementations that support Gray
streams, support is included for convenient stream wrappers.")
    (home-page "https://github.com/sharplispers/ironclad")
    (license license:bsd-3)))

(define-public cl-ironclad
  (sbcl-package->cl-source-package sbcl-ironclad))

(define-public ecl-ironclad
  (sbcl-package->ecl-package sbcl-ironclad))

(define-public sbcl-named-readtables
  (let ((commit "4dfb89fa1af6b305b6492b8af042f5190c11e9fc")
        (revision "1"))
    (package
      (name "sbcl-named-readtables")
      (version (string-append "0.9-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/melisgl/named-readtables.git")
               (commit commit)))
         (sha256
          (base32 "083kgh5462iqbb4px6kq8s7sggvpvkm36hx4qi9rnaw53b6ilqkk"))
         (file-name (git-file-name "named-readtables" version))))
      (build-system asdf-build-system/sbcl)
      (arguments
       ;; Tests seem to be broken.
       `(#:tests? #f))
      (home-page "https://github.com/melisgl/named-readtables/")
      (synopsis "Library that creates a namespace for named readtables")
      (description "Named readtables is a library that creates a namespace for
named readtables, which is akin to package namespacing in Common Lisp.")
      (license license:bsd-3))))

(define-public cl-named-readtables
  (sbcl-package->cl-source-package sbcl-named-readtables))

(define-public ecl-named-readtables
  (sbcl-package->ecl-package sbcl-named-readtables))

(define-public sbcl-pythonic-string-reader
  (let ((commit "47a70ba1e32362e03dad6ef8e6f36180b560f86a"))
    (package
      (name "sbcl-pythonic-string-reader")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/smithzvk/pythonic-string-reader/")
               (commit commit)))
         (sha256
          (base32 "1b5iryqw8xsh36swckmz8rrngmc39k92si33fgy5pml3n9l5rq3j"))
         (file-name (git-file-name "pythonic-string-reader" version))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("named-readtables" ,sbcl-named-readtables)))
      (home-page "https://github.com/smithzvk/pythonic-string-reader")
      (synopsis "Read table modification inspired by Python's three quote strings")
      (description "This piece of code sets up some reader macros that make it
simpler to input string literals which contain backslashes and double quotes
This is very useful for writing complicated docstrings and, as it turns out,
writing code that contains string literals that contain code themselves.")
      (license license:bsd-3))))

(define-public cl-pythonic-string-reader
  (sbcl-package->cl-source-package sbcl-pythonic-string-reader))

(define-public ecl-pythonic-string-reader
  (sbcl-package->ecl-package sbcl-pythonic-string-reader))

;; SLIME does not have a ASDF system definition to build all of Swank.  As a
;; result, the asdf-build-system/sbcl will produce an almost empty package.
;; Some work was done to fix this at
;; https://github.com/sionescu/slime/tree/swank-asdf but it was never merged
;; and is now lagging behind.  Building SBCL fasls might not be worth the
;; hassle, so let's just ship the source then.
(define-public cl-slime-swank
  (package
    (name "cl-slime-swank")
    (version "2.24")
    (source
     (origin
       (file-name (string-append name "-" version ".tar.gz"))
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/slime/slime/")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "0js24x42m7b5iymb4rxz501dff19vav5pywnzv50b673rbkaaqvh"))))
    (build-system asdf-build-system/source)
    (home-page "https://github.com/slime/slime")
    (synopsis "Common Lisp Swank server")
    (description
     "This is only useful if you want to start a Swank server in a Lisp
processes that doesn't run under Emacs.  Lisp processes created by
@command{M-x slime} automatically start the server.")
    (license (list license:gpl2+ license:public-domain))))

(define-public sbcl-slime-swank
  (deprecated-package "sbcl-slime-swank" cl-slime-swank))

(define-public sbcl-mgl-pax
  (let ((commit "818448418d6b9de74620f606f5b23033c6082769"))
    (package
      (name "sbcl-mgl-pax")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/melisgl/mgl-pax")
               (commit commit)))
         (sha256
          (base32
           "1p97zfkh130bdxqqxwaw2j9psv58751wakx7czbfpq410lg7dd7i"))
         (file-name (git-file-name "mgl-pax" version))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("3bmd" ,sbcl-3bmd)
         ("3bmd-ext-code-blocks" ,sbcl-3bmd-ext-code-blocks)
         ("babel" ,sbcl-babel)
         ("cl-fad" ,sbcl-cl-fad)
         ("ironclad" ,sbcl-ironclad)
         ("named-readtables" ,sbcl-named-readtables)
         ("pythonic-string-reader" ,sbcl-pythonic-string-reader)))
      (propagated-inputs
       ;; Packages having mgl-pax as input complain that it can't find
       ;; swank if we put it in inputs, so let's put it in propageted-inputs.
       `(("swank" ,cl-slime-swank)))
      (synopsis "Exploratory programming environment and documentation generator")
      (description
       "PAX provides an extremely poor man's Explorable Programming
environment.  Narrative primarily lives in so called sections that mix markdown
docstrings with references to functions, variables, etc, all of which should
probably have their own docstrings.

The primary focus is on making code easily explorable by using SLIME's
@command{M-.} (@command{slime-edit-definition}).  See how to enable some
fanciness in Emacs Integration.  Generating documentation from sections and all
the referenced items in Markdown or HTML format is also implemented.

With the simplistic tools provided, one may accomplish similar effects as with
Literate Programming, but documentation is generated from code, not vice versa
and there is no support for chunking yet.  Code is first, code must look
pretty, documentation is code.")
      (home-page "http://quotenil.com/")
      (license license:expat))))

(define-public cl-mgl-pax
  (sbcl-package->cl-source-package sbcl-mgl-pax))

(define-public ecl-mgl-pax
  (sbcl-package->ecl-package sbcl-mgl-pax))

(define-public sbcl-lisp-unit
  (let ((commit "89653a232626b67400bf9a941f9b367da38d3815"))
    (package
      (name "sbcl-lisp-unit")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/OdonataResearchLLC/lisp-unit")
               (commit commit)))
         (sha256
          (base32
           "0p6gdmgr7p383nvd66c9y9fp2bjk4jx1lpa5p09g43hr9y9pp9ry"))
         (file-name (git-file-name "lisp-unit" version))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Common Lisp Test framework inspired by JUnit to be simple of use")
      (description
       "@command{lisp-unit} is a Common Lisp library that supports unit
testing.  It is an extension of the library written by Chris Riesbeck.")
      (home-page "https://github.com/OdonataResearchLLC/lisp-unit")
      (license license:expat))))

(define-public cl-lisp-unit
  (sbcl-package->cl-source-package sbcl-lisp-unit))

(define-public ecl-lisp-unit
  (sbcl-package->ecl-package sbcl-lisp-unit))

(define-public sbcl-anaphora
  (package
    (name "sbcl-anaphora")
    (version "0.9.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tokenrove/anaphora")
             (commit version)))
       (sha256
        (base32
         "19wfrk3asimznkli0x2rfy637hwpdgqyvwj3vhq9x7vjvyf5vv6x"))
       (file-name (git-file-name "anaphora" version))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     `(("rt" ,sbcl-rt)))
    (synopsis "The anaphoric macro collection from Hell")
    (description
     "Anaphora is the anaphoric macro collection from Hell: it includes many
new fiends in addition to old friends like @command{aif} and
@command{awhen}.")
    (home-page "https://github.com/tokenrove/anaphora")
    (license license:public-domain)))

(define-public cl-anaphora
  (sbcl-package->cl-source-package sbcl-anaphora))

(define-public ecl-anaphora
  (sbcl-package->ecl-package sbcl-anaphora))

(define-public sbcl-lift
  (let ((commit "7d49a66c62759535624037826891152223d4206c"))
    (package
      (name "sbcl-lift")
      (version (git-version "1.7.1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gwkkwg/lift")
               (commit commit)))
         (sha256
          (base32
           "127v5avpz1i4m0lkaxqrq8hrl69rdazqaxf6s8awf0nd7wj2g4dp"))
         (file-name (git-file-name "lift" version))
         (modules '((guix build utils)))
         (snippet
          ;; Don't keep the bundled website
          `(begin
             (delete-file-recursively "website")
             #t))))
      (build-system asdf-build-system/sbcl)
      (arguments
       ;; The tests require a debugger, but we run with the debugger disabled.
       '(#:tests? #f))
      (synopsis "LIsp Framework for Testing")
      (description
       "The LIsp Framework for Testing (LIFT) is a unit and system test tool for LISP.
Though inspired by SUnit and JUnit, it's built with Lisp in mind.  In LIFT,
testcases are organized into hierarchical testsuites each of which can have
its own fixture.  When run, a testcase can succeed, fail, or error.  LIFT
supports randomized testing, benchmarking, profiling, and reporting.")
      (home-page "https://github.com/gwkkwg/lift")
      (license license:expat))))

(define-public cl-lift
  (sbcl-package->cl-source-package sbcl-lift))

(define-public ecl-lift
  (sbcl-package->ecl-package sbcl-lift))

(define-public sbcl-let-plus
  (let ((commit "5f14af61d501ecead02ec6b5a5c810efc0c9fdbb"))
    (package
      (name "sbcl-let-plus")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sharplispers/let-plus")
               (commit commit)))
         (sha256
          (base32
           "0i050ca2iys9f5mb7dgqgqdxfnc3b0rnjdwv95sqd490vkiwrsaj"))
         (file-name (git-file-name "let-plus" version))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("anaphora" ,sbcl-anaphora)))
      (native-inputs
       `(("lift" ,sbcl-lift)))
      (synopsis "Destructuring extension of let*")
      (description
       "This library implements the let+ macro, which is a dectructuring
extension of let*.  It features:

@itemize
@item Clean, consistent syntax and small implementation (less than 300 LOC,
not counting tests)
@item Placeholder macros allow editor hints and syntax highlighting
@item @command{&ign} for ignored values (in forms where that makes sense)
@item Very easy to extend
@end itemize\n")
      (home-page "https://github.com/sharplispers/let-plus")
      (license license:boost1.0))))

(define-public cl-let-plus
  (sbcl-package->cl-source-package sbcl-let-plus))

(define-public ecl-let-plus
  (sbcl-package->ecl-package sbcl-let-plus))

(define-public sbcl-cl-colors
  (let ((commit "827410584553f5c717eec6182343b7605f707f75"))
    (package
      (name "sbcl-cl-colors")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tpapp/cl-colors")
               (commit commit)))
         (sha256
          (base32
           "0l446lday4hybsm9bq3jli97fvv8jb1d33abg79vbylpwjmf3y9a"))
         (file-name (git-file-name "cl-colors" version))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("let-plus" ,sbcl-let-plus)))
      (synopsis "Simple color library for Common Lisp")
      (description
       "This is a very simple color library for Common Lisp, providing

@itemize
@item Types for representing colors in HSV and RGB spaces.
@item Simple conversion functions between the above types (and also
hexadecimal representation for RGB).
@item Some predefined colors (currently X11 color names – of course the
library does not depend on X11).Because color in your terminal is nice.
@end itemize

This library is no longer supported by its author.")
      (home-page "https://github.com/tpapp/cl-colors")
      (license license:boost1.0))))

(define-public cl-colors
  (sbcl-package->cl-source-package sbcl-cl-colors))

(define-public ecl-cl-colors
  (sbcl-package->ecl-package sbcl-cl-colors))

(define-public sbcl-cl-ansi-text
  (let ((commit "53badf7878f27f22f2d4a2a43e6df458e43acbe9"))
    (package
      (name "sbcl-cl-ansi-text")
      (version (git-version "1.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/pnathan/cl-ansi-text")
               (commit commit)))
         (sha256
          (base32
           "11i27n0dbz5lmygiw65zzr8lx0rac6b6yysqranphn31wls6ja3v"))
         (file-name (git-file-name "cl-ansi-text" version))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("cl-colors" ,sbcl-cl-colors)))
      (native-inputs
       `(("fiveam" ,sbcl-fiveam)))
      (synopsis "ANSI terminal color implementation for Common Lisp")
      (description
       "@command{cl-ansi-text} provides utilities which enable printing to an
ANSI terminal with colored text.  It provides the macro @command{with-color}
which causes everything printed in the body to be displayed with the provided
color.  It further provides functions which will print the argument with the
named color.")
      (home-page "https://github.com/pnathan/cl-ansi-text")
      (license license:llgpl))))

(define-public cl-ansi-text
  (sbcl-package->cl-source-package sbcl-cl-ansi-text))

(define-public ecl-cl-ansi-text
  (sbcl-package->ecl-package sbcl-cl-ansi-text))

(define-public sbcl-prove-asdf
  (let ((commit "4f9122bd393e63c5c70c1fba23070622317cfaa0"))
    (package
      (name "sbcl-prove-asdf")
      (version (git-version "1.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/fukamachi/prove")
               (commit commit)))
         (sha256
          (base32
           "07sbfw459z8bbjvx1qlmfa8qk2mvbjnnzi2mi0x72blaj8bkl4vc"))
         (file-name (git-file-name "prove" version))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:asd-file "prove-asdf.asd"))
      (synopsis "Test requirement for the Common Lisp 'prove' library")
      (description
       "Test requirement for the Common Lisp @command{prove} library.")
      (home-page "https://github.com/fukamachi/prove")
      (license license:expat))))

(define-public cl-prove-asdf
  (sbcl-package->cl-source-package sbcl-prove-asdf))

(define-public ecl-prove-asdf
  (sbcl-package->ecl-package sbcl-prove-asdf))

(define-public sbcl-prove
  (package
    (inherit sbcl-prove-asdf)
    (name "sbcl-prove")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cl-ppcre" ,sbcl-cl-ppcre)
       ("cl-ansi-text" ,sbcl-cl-ansi-text)))
    (native-inputs
     `(("prove-asdf" ,sbcl-prove-asdf)))
    (arguments
     `(#:asd-file "prove.asd"))
    (synopsis "Yet another unit testing framework for Common Lisp")
    (description
     "This project was originally called @command{cl-test-more}.
@command{prove} is yet another unit testing framework for Common Lisp.  The
advantages of @command{prove} are:

@itemize
@item Various simple functions for testing and informative error messages
@item ASDF integration
@item Extensible test reporters
@item Colorizes the report if it's available (note for SLIME)
@item Reports test durations
@end itemize\n")))

(define-public cl-prove
  (sbcl-package->cl-source-package sbcl-prove))

(define-public ecl-prove
  (sbcl-package->ecl-package sbcl-prove))

(define-public sbcl-proc-parse
  (let ((commit "ac3636834d561bdc2686c956dbd82494537285fd"))
    (package
      (name "sbcl-proc-parse")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/fukamachi/proc-parse")
               (commit commit)))
         (sha256
          (base32
           "06rnl0h4cx6xv2wj3jczmmcxqn2703inmmvg1s4npbghmijsybfh"))
         (file-name (git-file-name "proc-parse" version))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("babel" ,sbcl-babel)))
      (native-inputs
       `(("prove" ,sbcl-prove)
         ("prove-asdf" ,sbcl-prove-asdf)))
      (arguments
       ;; TODO: Tests don't find "proc-parse-test", why?
       `(#:tests? #f))
      (synopsis "Procedural vector parser")
      (description
       "This is a string/octets parser library for Common Lisp with speed and
readability in mind.  Unlike other libraries, the code is not a
pattern-matching-like, but a char-by-char procedural parser.")
      (home-page "https://github.com/fukamachi/proc-parse")
      (license license:bsd-2))))

(define-public cl-proc-parse
  (sbcl-package->cl-source-package sbcl-proc-parse))

(define-public ecl-proc-parse
  (sbcl-package->ecl-package sbcl-proc-parse))

(define-public sbcl-parse-float
  (let ((commit "2aae569f2a4b2eb3bfb5401a959425dcf151b09c"))
    (package
      (name "sbcl-parse-float")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/soemraws/parse-float")
               (commit commit)))
         (sha256
          (base32
           "08xw8cchhmqcc0byng69m3f5a2izc9y2290jzz2k0qrbibp1fdk7"))
         (file-name (git-file-name "proc-parse" version))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("babel" ,sbcl-babel)))
      (native-inputs
       `(("prove" ,sbcl-prove)
         ("prove-asdf" ,sbcl-prove-asdf)))
      (arguments
       ;; TODO: Tests don't find "proc-parse-test", why?
       `(#:tests? #f))
      (synopsis "Parse a floating point value from a string in Common Lisp")
      (description
       "This package exports the following function to parse floating-point
values from a string in Common Lisp.")
      (home-page "https://github.com/soemraws/parse-float")
      (license license:public-domain))))

(define-public cl-parse-float
  (sbcl-package->cl-source-package sbcl-parse-float))

(define-public ecl-parse-float
  (sbcl-package->ecl-package sbcl-parse-float))

(define-public sbcl-ascii-strings
  (let ((revision "1")
        (changeset "5048480a61243e6f1b02884012c8f25cdbee6d97"))
    (package
      (name "sbcl-ascii-strings")
      (version (string-append "0-" revision "." (string-take changeset 7)))
      (source
       (origin
         (method hg-fetch)
         (uri (hg-reference
               (url "https://bitbucket.org/vityok/cl-string-match/")
               (changeset changeset)))
         (sha256
          (base32
           "01wn5qx562w43ssy92xlfgv79w7p0nv0wbl76mpmba131n9ziq2y"))
         (file-name (git-file-name "cl-string-match" version))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("babel" ,sbcl-babel)))
      (arguments
       `(#:asd-file "ascii-strings.asd"))
      (synopsis "Operations on ASCII strings")
      (description
       "Operations on ASCII strings.  Essentially this can be any kind of
single-byte encoded strings.")
      (home-page "https://bitbucket.org/vityok/cl-string-match/")
      (license license:bsd-3))))

(define-public cl-ascii-strings
  (sbcl-package->cl-source-package sbcl-ascii-strings))

(define-public ecl-ascii-strings
  (sbcl-package->ecl-package sbcl-ascii-strings))

(define-public sbcl-simple-scanf
  (package
    (inherit sbcl-ascii-strings)
    (name "sbcl-simple-scanf")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("iterate" ,sbcl-iterate)
       ("proc-parse" ,sbcl-proc-parse)
       ("parse-float" ,sbcl-parse-float)))
    (arguments
     `(#:asd-file "simple-scanf.asd"))
    (synopsis "Simple scanf-like functionality implementation")
    (description
     "A simple scanf-like functionality implementation.")))

(define-public cl-simple-scanf
  (sbcl-package->cl-source-package sbcl-simple-scanf))

(define-public ecl-simple-scanf
  (sbcl-package->ecl-package sbcl-simple-scanf))

(define-public sbcl-cl-string-match
  (package
    (inherit sbcl-ascii-strings)
    (name "sbcl-cl-string-match")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("ascii-strings" ,sbcl-ascii-strings)
       ("yacc" ,sbcl-cl-yacc)
       ("jpl-util" ,sbcl-jpl-util)
       ("jpl-queues" ,sbcl-jpl-queues)
       ("mgl-pax" ,sbcl-mgl-pax)
       ("iterate" ,sbcl-iterate)))
    ;; TODO: Tests are not evaluated properly.
    (native-inputs
     ;; For testing:
     `(("lisp-unit" ,sbcl-lisp-unit)
       ("simple-scanf" ,sbcl-simple-scanf)))
    (arguments
     `(#:tests? #f
       #:asd-file "cl-string-match.asd"))
    (synopsis "Portable, dependency-free set of utilities to manipulate strings in Common Lisp")
    (description
     "@command{cl-strings} is a small, portable, dependency-free set of
utilities that make it even easier to manipulate text in Common Lisp.  It has
100% test coverage and works at least on sbcl, ecl, ccl, abcl and clisp.")))

(define-public cl-string-match
  (sbcl-package->cl-source-package sbcl-cl-string-match))

(define-public ecl-cl-string-match
  (sbcl-package->ecl-package sbcl-cl-string-match))

(define-public sbcl-ptester
  (let ((commit "fe69fde54f4bce00ce577feb918796c293fc7253")
        (revision "1"))
    (package
      (name "sbcl-ptester")
      (version (git-version "2.1.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "http://git.kpe.io/ptester.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1l0lfl7cdnr2qf4zh38hi4llxg22c49zkm639bdkmvlkzwj3ndwf"))))
      (build-system asdf-build-system/sbcl)
      (home-page "http://quickdocs.org/ptester/")
      (synopsis "Portable test harness package")
      (description
       "@command{ptester} is a portable testing framework based on Franz's
tester module.")
      (license license:llgpl))))

(define-public cl-ptester
  (sbcl-package->cl-source-package sbcl-ptester))

(define-public ecl-ptester
  (sbcl-package->ecl-package sbcl-ptester))

(define-public sbcl-puri
  (let ((commit "ef5afb9e5286c8e952d4344f019c1a636a717b97")
        (revision "1"))
    (package
      (name "sbcl-puri")
      (version (git-version "1.5.7" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "http://git.kpe.io/puri.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1vm25pdl92laj72p5vyd538kf3cjy2655z6bdc99h20ana2p231s"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       `(("ptester" ,sbcl-ptester)))
      (home-page "http://quickdocs.org/puri/")
      (synopsis "Portable URI Library")
      (description
       "This is a portable Universal Resource Identifier library for Common
Lisp programs.  It parses URI according to the RFC 2396 specification.")
      (license license:llgpl))))

(define-public cl-puri
  (sbcl-package->cl-source-package sbcl-puri))

(define-public ecl-puri
  (sbcl-package->ecl-package sbcl-puri))

(define-public sbcl-queues
  (let ((commit "47d4da65e9ea20953b74aeeab7e89a831b66bc94"))
    (package
      (name "sbcl-queues")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/oconnore/queues")
               (commit commit)))
         (file-name (git-file-name "queues" version))
         (sha256
          (base32
           "0wdhfnzi4v6d97pggzj2aw55si94w4327br94jrmyvwf351wqjvv"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/oconnore/queues")
      (synopsis "Common Lisp queue library")
      (description
       "This is a simple queue library for Common Lisp with features such as
non-consing thread safe queues and fibonacci priority queues.")
      (license license:expat))))

(define-public cl-queues
  (sbcl-package->cl-source-package sbcl-queues))

(define-public ecl-queues
  (sbcl-package->ecl-package sbcl-queues))

(define-public sbcl-queues.simple-queue
  (package
    (inherit sbcl-queues)
    (name "sbcl-queues.simple-queue")
    (inputs
     `(("sbcl-queues" ,sbcl-queues)))
    (arguments
     `(#:asd-file "queues.simple-queue.asd"))
    (synopsis "Simple queue implementation")
    (description
     "This is a simple queue library for Common Lisp with features such as
non-consing thread safe queues and fibonacci priority queues.")
    (license license:expat)))

(define-public cl-queues.simple-queue
  (sbcl-package->cl-source-package sbcl-queues.simple-queue))

(define-public ecl-queues.simple-queue
  (sbcl-package->ecl-package sbcl-queues.simple-queue))

(define-public sbcl-queues.simple-cqueue
  (package
    (inherit sbcl-queues)
    (name "sbcl-queues.simple-cqueue")
    (inputs
     `(("sbcl-queues" ,sbcl-queues)
       ("sbcl-queues.simple-queue" ,sbcl-queues.simple-queue)
       ("bordeaux-threads" ,sbcl-bordeaux-threads)))
    (arguments
     `(#:asd-file "queues.simple-cqueue.asd"))
    (synopsis "Thread safe queue implementation")
    (description
     "This is a simple queue library for Common Lisp with features such as
non-consing thread safe queues and fibonacci priority queues.")
    (license license:expat)))

(define-public cl-queues.simple-cqueue
  (sbcl-package->cl-source-package sbcl-queues.simple-cqueue))

(define-public ecl-queues.simple-cqueue
  (sbcl-package->ecl-package sbcl-queues.simple-cqueue))

(define-public sbcl-queues.priority-queue
  (package
    (inherit sbcl-queues)
    (name "sbcl-queues.priority-queue")
    (inputs
     `(("sbcl-queues" ,sbcl-queues)))
    (arguments
     `(#:asd-file "queues.priority-queue.asd"))
    (synopsis "Priority queue (Fibonacci) implementation")
    (description
     "This is a simple queue library for Common Lisp with features such as
non-consing thread safe queues and fibonacci priority queues.")
    (license license:expat)))

(define-public cl-queues.priority-queue
  (sbcl-package->cl-source-package sbcl-queues.priority-queue))

(define-public ecl-queues.priority-queue
  (sbcl-package->ecl-package sbcl-queues.priority-queue))

(define-public sbcl-queues.priority-cqueue
  (package
    (inherit sbcl-queues)
    (name "sbcl-queues.priority-cqueue")
    (inputs
     `(("sbcl-queues" ,sbcl-queues)
       ("sbcl-queues.priority-queue" ,sbcl-queues.priority-queue)
       ("bordeaux-threads" ,sbcl-bordeaux-threads)))
    (arguments
     `(#:asd-file "queues.priority-cqueue.asd"))
    (synopsis "Thread safe fibonacci priority queue implementation")
    (description
     "This is a simple queue library for Common Lisp with features such as
non-consing thread safe queues and fibonacci priority queues.")
    (license license:expat)))

(define-public cl-queues.priority-cqueue
  (sbcl-package->cl-source-package sbcl-queues.priority-cqueue))

(define-public ecl-queues.priority-cqueue
  (sbcl-package->ecl-package sbcl-queues.priority-cqueue))

(define sbcl-cffi-bootstrap
  (package
    (name "sbcl-cffi-bootstrap")
    (version "0.21.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cffi/cffi.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name "cffi-bootstrap" version))
       (sha256
        (base32 "1qalargz9bhp850qv60ffwpdqi4xirzar4l3g6qcg8yc6xqf2cjk"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     `(("libffi" ,libffi)
       ("alexandria" ,sbcl-alexandria)
       ("babel" ,sbcl-babel)
       ("trivial-features" ,sbcl-trivial-features)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "libffi/libffi.lisp"
               (("libffi.so.6" all) (string-append
                                     (assoc-ref inputs "libffi")
                                     "/lib/" all)))
             (substitute* "toolchain/c-toolchain.lisp"
               (("\"cc\"") (format #f "~S" (which "gcc")))))))
       #:asd-system-name "cffi"
       #:tests? #f))
    (home-page "https://common-lisp.net/project/cffi/")
    (synopsis "Common Foreign Function Interface for Common Lisp")
    (description "The Common Foreign Function Interface (CFFI)
purports to be a portable foreign function interface for Common Lisp.
The CFFI library is composed of a Lisp-implementation-specific backend
in the CFFI-SYS package, and a portable frontend in the CFFI
package.")
    (license license:expat)))

(define-public sbcl-cffi-toolchain
  (package
    (inherit sbcl-cffi-bootstrap)
    (name "sbcl-cffi-toolchain")
    (inputs
     `(("libffi" ,libffi)
       ("sbcl-cffi" ,sbcl-cffi-bootstrap)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cffi-bootstrap)
       ((#:asd-system-name _) #f)
       ((#:tests? _) #t)))))

(define-public sbcl-cffi-libffi
  (package
    (inherit sbcl-cffi-toolchain)
    (name "sbcl-cffi-libffi")
    (inputs
     `(("cffi" ,sbcl-cffi-bootstrap)
       ("cffi-grovel" ,sbcl-cffi-grovel)
       ("trivial-features" ,sbcl-trivial-features)
       ("libffi" ,libffi)))))

(define-public sbcl-cffi-grovel
  (package
    (inherit sbcl-cffi-toolchain)
    (name "sbcl-cffi-grovel")
    (inputs
     `(("libffi" ,libffi)
       ("cffi" ,sbcl-cffi-bootstrap)
       ("cffi-toolchain" ,sbcl-cffi-toolchain)
       ("alexandria" ,sbcl-alexandria)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cffi-toolchain)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'build 'install-headers
             (lambda* (#:key outputs #:allow-other-keys)
               (install-file "grovel/common.h"
                             (string-append
                              (assoc-ref outputs "out")
                              "/include/grovel"))))))))))

(define-public sbcl-cffi
  (package
    (inherit sbcl-cffi-toolchain)
    (name "sbcl-cffi")
    (inputs (package-inputs sbcl-cffi-bootstrap))
    (native-inputs
     `(("cffi-grovel" ,sbcl-cffi-grovel)
       ("cffi-libffi" ,sbcl-cffi-libffi)
       ("rt" ,sbcl-rt)
       ("bordeaux-threads" ,sbcl-bordeaux-threads)
       ,@(package-native-inputs sbcl-cffi-bootstrap)))))

(define-public cl-cffi
  (sbcl-package->cl-source-package sbcl-cffi))

(define-public sbcl-cffi-uffi-compat
  (package
    (inherit sbcl-cffi-toolchain)
    (name "sbcl-cffi-uffi-compat")
    (native-inputs
     `(,@(package-inputs sbcl-cffi-bootstrap))) ; For fix-paths phase
    (inputs
     `(("cffi" ,sbcl-cffi)))
    (synopsis "UFFI Compatibility Layer for CFFI")))

(define-public cl-cffi-uffi-compat
  (sbcl-package->cl-source-package sbcl-cffi-uffi-compat))

(define-public sbcl-cl-sqlite
  (let ((commit "c738e66d4266ef63a1debc4ef4a1b871a068c112"))
    (package
      (name "sbcl-cl-sqlite")
      (version (git-version "0.2" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/dmitryvk/cl-sqlite")
               (commit commit)))
         (file-name (git-file-name "cl-sqlite" version))
         (sha256
          (base32
           "1ng45k1hdb84sqjryrfx93g66bsbybmpy301wd0fdybnc5jzr36q"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("iterate" ,sbcl-iterate)
         ("cffi" ,sbcl-cffi)
         ("sqlite" ,sqlite)))
      (native-inputs
       `(("fiveam" ,sbcl-fiveam)
         ("bordeaux-threads" ,sbcl-bordeaux-threads)))
      (arguments
       `(#:tests? #f                    ; Upstream seems to have issues with tests: https://github.com/dmitryvk/cl-sqlite/issues/7
         #:asd-file "sqlite.asd"
         #:asd-system-name "sqlite"
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "sqlite-ffi.lisp"
                 (("libsqlite3" all) (string-append
                                      (assoc-ref inputs "sqlite")"/lib/" all))))))))
      (home-page "https://common-lisp.net/project/cl-sqlite/")
      (synopsis "Common Lisp binding for SQLite")
      (description
       "The @command{cl-sqlite} package is an interface to the SQLite embedded
relational database engine.")
      (license license:public-domain))))

(define-public cl-sqlite
  (sbcl-package->cl-source-package sbcl-cl-sqlite))

(define-public sbcl-parenscript
  (let ((commit "061d8e286c81c3f45c84fb2b11ee7d83f590a8f8"))
    (package
      (name "sbcl-parenscript")
      (version (git-version "2.6" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.common-lisp.net/parenscript/parenscript")
               (commit commit)))
         (file-name (git-file-name "parenscript" version))
         (sha256
          (base32
           "1kbhgsjbikc73m5cwdp4d4fdafyqcr1b7b630qjrziql0nh6mi3k"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("cl-ppcre" ,sbcl-cl-ppcre)
         ("anaphora" ,sbcl-anaphora)
         ("named-readtables" ,sbcl-named-readtables)))
      (home-page "https://common-lisp.net/project/parenscript/")
      (synopsis "Translator from a subset of Common Lisp to JavaScript")
      (description
       "Parenscript is a translator from an extended subset of Common Lisp to
JavaScript.  Parenscript code can run almost identically on both the
browser (as JavaScript) and server (as Common Lisp).

Parenscript code is treated the same way as Common Lisp code, making the full
power of Lisp macros available for JavaScript.  This provides a web
development environment that is unmatched in its ability to reduce code
duplication and provide advanced meta-programming facilities to web
developers.

At the same time, Parenscript is different from almost all other \"language
X\" to JavaScript translators in that it imposes almost no overhead:

@itemize
@item No run-time dependencies: Any piece of Parenscript code is runnable
as-is.  There are no JavaScript files to include.
@item Native types: Parenscript works entirely with native JavaScript data
types.  There are no new types introduced, and object prototypes are not
touched.
@item Native calling convention: Any JavaScript code can be called without the
need for bindings.  Likewise, Parenscript can be used to make efficient,
self-contained JavaScript libraries.
@item Readable code: Parenscript generates concise, formatted, idiomatic
JavaScript code.  Identifier names are preserved.  This enables seamless
debugging in tools like Firebug.
@item Efficiency: Parenscript introduces minimal overhead for advanced Common
Lisp features.  The generated code is almost as fast as hand-written
JavaScript.
@end itemize\n")
      (license license:bsd-3))))

(define-public cl-parenscript
  (sbcl-package->cl-source-package sbcl-parenscript))

(define-public ecl-parenscript
  (sbcl-package->ecl-package sbcl-parenscript))

(define-public sbcl-cl-json
  (let ((commit "6dfebb9540bfc3cc33582d0c03c9ec27cb913e79"))
    (package
      (name "sbcl-cl-json")
      (version (git-version "0.5" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/hankhero/cl-json")
               (commit commit)))
         (file-name (git-file-name "cl-json" version))
         (sha256
          (base32
           "0fx3m3x3s5ji950yzpazz4s0img3l6b3d6l3jrfjv0lr702496lh"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       `(("fiveam" ,sbcl-fiveam)))
      (home-page "https://github.com/hankhero/cl-json")
      (synopsis "JSON encoder and decoder for Common-Lisp")
      (description
       "@command{cl-json} provides an encoder of Lisp objects to JSON format
and a corresponding decoder of JSON data to Lisp objects.  Both the encoder
and the decoder are highly customizable; at the same time, the default
settings ensure a very simple mode of operation, similar to that provided by
@command{yason} or @command{st-json}.")
      (license license:expat))))

(define-public cl-json
  (sbcl-package->cl-source-package sbcl-cl-json))

(define-public ecl-cl-json
  (sbcl-package->ecl-package sbcl-cl-json))

(define-public sbcl-unix-opts
  (package
    (name "sbcl-unix-opts")
    (version "0.1.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libre-man/unix-opts")
             (commit version)))
       (file-name (git-file-name "unix-opts" version))
       (sha256
        (base32
         "08djdi1ard09fijb7w9bdmhmwd98b1hzmcnjw9fqjiqa0g3b44rr"))))
    (build-system asdf-build-system/sbcl)
    (home-page "https://github.com/hankhero/cl-json")
    (synopsis "Unix-style command line options parser")
    (description
     "This is a minimalistic parser of command line options.  The main
advantage of the library is the ability to concisely define command line
options once and then use this definition for parsing and extraction of
command line arguments, as well as printing description of command line
options (you get --help for free).  This way you don't need to repeat
yourself.  Also, @command{unix-opts} doesn't depend on anything and allows to
precisely control behavior of the parser via Common Lisp restarts.")
    (license license:expat)))

(define-public cl-unix-opts
  (sbcl-package->cl-source-package sbcl-unix-opts))

(define-public ecl-unix-opts
  (sbcl-package->ecl-package sbcl-unix-opts))

(define-public sbcl-trivial-garbage
  (package
    (name "sbcl-trivial-garbage")
    (version "0.21")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/trivial-garbage/trivial-garbage.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name "trivial-garbage" version))
       (sha256
        (base32 "0122jicfg7pca1wxw8zak1n92h5friqy60988ns0ysksj3fphw9n"))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     `(("rt" ,sbcl-rt)))
    (home-page "https://common-lisp.net/project/trivial-garbage/")
    (synopsis "Portable GC-related APIs for Common Lisp")
    (description "@command{trivial-garbage} provides a portable API to
finalizers, weak hash-tables and weak pointers on all major implementations of
the Common Lisp programming language.")
    (license license:public-domain)))

(define-public cl-trivial-garbage
  (sbcl-package->cl-source-package sbcl-trivial-garbage))

(define-public ecl-trivial-garbage
  (sbcl-package->ecl-package sbcl-trivial-garbage))

(define-public sbcl-closer-mop
  (let ((commit "fac29ce90e3a46e1fc6cf182190e193526fa9dbc"))
    (package
      (name "sbcl-closer-mop")
      (version (git-version  "1.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/pcostanza/closer-mop")
               (commit commit)))
         (sha256
          (base32 "0hvh77y869h8fg9di5snyg85fxq6fdh9gj1igmx1g6j6j5x915dl"))
         (file-name (git-file-name "closer-mop" version ))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/pcostanza/closer-mop")
      (synopsis "Rectifies absent or incorrect CLOS MOP features")
      (description "Closer to MOP is a compatibility layer that rectifies many
of the absent or incorrect CLOS MOP features across a broad range of Common
Lisp implementations.")
      (license license:expat))))

(define-public cl-closer-mop
  (sbcl-package->cl-source-package sbcl-closer-mop))

(define-public ecl-closer-mop
  (sbcl-package->ecl-package sbcl-closer-mop))

(define sbcl-cl-cffi-gtk-boot0
  (let ((commit "29443c5aaca975709df8025c4649366d882033cb"))
    (package
      (name "sbcl-cl-cffi-gtk-boot0")
      (version (git-version "0.11.2" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Ferada/cl-cffi-gtk/")
               (commit commit)))
         (file-name (git-file-name "cl-cffi-gtk" version))
         (sha256
          (base32
           "0f6s92sf8xyzh1yksqx8bsy1sv0zmy0c13j3b8bavaba5hlxpxah"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("iterate" ,sbcl-iterate)
         ("cffi" ,sbcl-cffi)
         ("trivial-features" ,sbcl-trivial-features)))
      (home-page "https://github.com/Ferada/cl-cffi-gtk/")
      (synopsis "Common Lisp binding for GTK+3")
      (description
       "@command{cl-cffi-gtk} is a Lisp binding to GTK+ 3 (GIMP Toolkit) which
is a library for creating graphical user interfaces.")
      (license license:lgpl3))))

(define-public sbcl-cl-cffi-gtk-glib
  (package
    (inherit sbcl-cl-cffi-gtk-boot0)
    (name "sbcl-cl-cffi-gtk-glib")
    (inputs
     `(("glib" ,glib)
       ,@(package-inputs sbcl-cl-cffi-gtk-boot0)))
    (arguments
     `(#:asd-file "glib/cl-cffi-gtk-glib.asd"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "glib/glib.init.lisp"
               (("libglib|libgthread" all) (string-append
                                            (assoc-ref inputs "glib") "/lib/" all))))))))))

(define-public sbcl-cl-cffi-gtk-gobject
  (package
    (inherit sbcl-cl-cffi-gtk-boot0)
    (name "sbcl-cl-cffi-gtk-gobject")
    (inputs
     `(("glib" ,glib)
       ("cl-cffi-gtk-glib" ,sbcl-cl-cffi-gtk-glib)
       ("trivial-garbage" ,sbcl-trivial-garbage)
       ("bordeaux-threads" ,sbcl-bordeaux-threads)
       ("closer-mop" ,sbcl-closer-mop)
       ,@(package-inputs sbcl-cl-cffi-gtk-boot0)))
    (arguments
     `(#:asd-file "gobject/cl-cffi-gtk-gobject.asd"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "gobject/gobject.init.lisp"
               (("libgobject" all) (string-append
                                    (assoc-ref inputs "glib") "/lib/" all))))))))))

(define-public sbcl-cl-cffi-gtk-gio
  (package
    (inherit sbcl-cl-cffi-gtk-boot0)
    (name "sbcl-cl-cffi-gtk-gio")
    (inputs
     `(("glib" ,glib)
       ("cl-cffi-gtk-glib" ,sbcl-cl-cffi-gtk-glib)
       ("cl-cffi-gtk-gobject" ,sbcl-cl-cffi-gtk-gobject)
       ,@(package-inputs sbcl-cl-cffi-gtk-boot0)))
    (arguments
     `(#:asd-file "gio/cl-cffi-gtk-gio.asd"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "gio/gio.init.lisp"
               (("libgio" all)
                (string-append
                 (assoc-ref inputs "glib") "/lib/" all))))))))))

(define-public sbcl-cl-cffi-gtk-cairo
  (package
    (inherit sbcl-cl-cffi-gtk-boot0)
    (name "sbcl-cl-cffi-gtk-cairo")
    (inputs
     `(("cairo" ,cairo)
       ("cl-cffi-gtk-glib" ,sbcl-cl-cffi-gtk-glib)
       ,@(package-inputs sbcl-cl-cffi-gtk-boot0)))
    (arguments
     `(#:asd-file "cairo/cl-cffi-gtk-cairo.asd"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "cairo/cairo.init.lisp"
               (("libcairo" all)
                (string-append
                 (assoc-ref inputs "cairo") "/lib/" all))))))))))

(define-public sbcl-cl-cffi-gtk-pango
  (package
    (inherit sbcl-cl-cffi-gtk-boot0)
    (name "sbcl-cl-cffi-gtk-pango")
    (inputs
     `(("pango" ,pango)
       ("cl-cffi-gtk-glib" ,sbcl-cl-cffi-gtk-glib)
       ("cl-cffi-gtk-gobject" ,sbcl-cl-cffi-gtk-gobject)
       ("cl-cffi-gtk-cairo" ,sbcl-cl-cffi-gtk-cairo)
       ,@(package-inputs sbcl-cl-cffi-gtk-boot0)))
    (arguments
     `(#:asd-file "pango/cl-cffi-gtk-pango.asd"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "pango/pango.init.lisp"
               (("libpango" all)
                (string-append
                 (assoc-ref inputs "pango") "/lib/" all))))))))))

(define-public sbcl-cl-cffi-gtk-gdk-pixbuf
  (package
    (inherit sbcl-cl-cffi-gtk-boot0)
    (name "sbcl-cl-cffi-gtk-gdk-pixbuf")
    (inputs
     `(("gdk-pixbuf" ,gdk-pixbuf)
       ("cl-cffi-gtk-gobject" ,sbcl-cl-cffi-gtk-gobject)
       ,@(package-inputs sbcl-cl-cffi-gtk-boot0)))
    (arguments
     `(#:asd-file "gdk-pixbuf/cl-cffi-gtk-gdk-pixbuf.asd"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "gdk-pixbuf/gdk-pixbuf.init.lisp"
               (("libgdk_pixbuf" all)
                (string-append
                 (assoc-ref inputs "gdk-pixbuf") "/lib/" all))))))))))

(define-public sbcl-cl-cffi-gtk-gdk
  (package
    (inherit sbcl-cl-cffi-gtk-boot0)
    (name "sbcl-cl-cffi-gtk-gdk")
    (inputs
     `(("gtk" ,gtk+)
       ("cl-cffi-gtk-gobject" ,sbcl-cl-cffi-gtk-gobject)
       ("cl-cffi-gtk-gio" ,sbcl-cl-cffi-gtk-gio)
       ("cl-cffi-gtk-gdk-pixbuf" ,sbcl-cl-cffi-gtk-gdk-pixbuf)
       ("cl-cffi-gtk-cairo" ,sbcl-cl-cffi-gtk-cairo)
       ("cl-cffi-gtk-pango" ,sbcl-cl-cffi-gtk-pango)
       ,@(package-inputs sbcl-cl-cffi-gtk-boot0)))
    (arguments
     `(#:asd-file "gdk/cl-cffi-gtk-gdk.asd"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "gdk/gdk.init.lisp"
               (("libgdk" all)
                (string-append
                 (assoc-ref inputs "gtk") "/lib/" all)))
             (substitute* "gdk/gdk.package.lisp"
               (("libgtk" all)
                (string-append
                 (assoc-ref inputs "gtk") "/lib/" all))))))))))

(define-public sbcl-cl-cffi-gtk
  (package
    (inherit sbcl-cl-cffi-gtk-boot0)
    (name "sbcl-cl-cffi-gtk")
    (inputs
     `(("cl-cffi-gtk-glib" ,sbcl-cl-cffi-gtk-glib)
       ("cl-cffi-gtk-gobject" ,sbcl-cl-cffi-gtk-gobject)
       ("cl-cffi-gtk-gio" ,sbcl-cl-cffi-gtk-gio)
       ("cl-cffi-gtk-gdk" ,sbcl-cl-cffi-gtk-gdk)
       ,@(package-inputs sbcl-cl-cffi-gtk-boot0)))
    (native-inputs
     `(("fiveam" ,sbcl-fiveam)))
    (arguments
     `(#:asd-file "gtk/cl-cffi-gtk.asd"
       #:test-asd-file "test/cl-cffi-gtk-test.asd"
       ;; TODO: Tests fail with memory fault.
       ;; See https://github.com/Ferada/cl-cffi-gtk/issues/24.
       #:tests? #f))))

(define-public cl-cffi-gtk
  (sbcl-package->cl-source-package sbcl-cl-cffi-gtk))

(define-public sbcl-cl-webkit
  (let ((commit "4832c99c31e0eb1fcce3779d119343ae8a423952"))
    (package
      (name "sbcl-cl-webkit")
      (version (git-version "2.4" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/joachifm/cl-webkit")
               (commit commit)))
         (file-name (git-file-name "cl-webkit" version))
         (sha256
          (base32
           "0sn7m181wfg1q49q45dlsry8c38x7pziqcs0frnymk6yvgndybxd"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("cffi" ,sbcl-cffi)
         ("cl-cffi-gtk" ,sbcl-cl-cffi-gtk)
         ("webkitgtk" ,webkitgtk)))
      (arguments
       `(#:asd-file "webkit2/cl-webkit2.asd"
         #:asd-system-name "cl-webkit2"
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "webkit2/webkit2.init.lisp"
                 (("libwebkit2gtk" all)
                  (string-append
                   (assoc-ref inputs "webkitgtk") "/lib/" all))))))))
      (home-page "https://github.com/joachifm/cl-webkit")
      (synopsis "Binding to WebKitGTK+ for Common Lisp")
      (description
       "@command{cl-webkit} is a binding to WebKitGTK+ for Common Lisp,
currently targeting WebKit version 2.  The WebKitGTK+ library adds web
browsing capabilities to an application, leveraging the full power of the
WebKit browsing engine.")
      (license license:expat))))

(define-public cl-webkit
  (sbcl-package->cl-source-package sbcl-cl-webkit))

(define-public sbcl-lparallel
  (package
    (name "sbcl-lparallel")
    (version "2.8.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lmj/lparallel/")
             (commit (string-append "lparallel-" version))))
       (file-name (git-file-name "lparallel" version))
       (sha256
        (base32
         "0g0aylrbbrqsz0ahmwhvnk4cmc2931fllbpcfgzsprwnqqd7vwq9"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("bordeaux-threads" ,sbcl-bordeaux-threads)
       ("trivial-garbage" ,sbcl-trivial-garbage)))
    (home-page "https://lparallel.org/")
    (synopsis "Parallelism for Common Lisp")
    (description
     "@command{lparallel} is a library for parallel programming in Common
Lisp, featuring:

@itemize
@item a simple model of task submission with receiving queue,
@item constructs for expressing fine-grained parallelism,
@item asynchronous condition handling across thread boundaries,
@item parallel versions of map, reduce, sort, remove, and many others,
@item promises, futures, and delayed evaluation constructs,
@item computation trees for parallelizing interconnected tasks,
@item bounded and unbounded FIFO queues,
@item high and low priority tasks,
@item task killing by category,
@item integrated timeouts.
@end itemize\n")
    (license license:expat)))

(define-public cl-lparallel
  (sbcl-package->cl-source-package sbcl-lparallel))

(define-public ecl-lparallel
  (sbcl-package->ecl-package sbcl-lparallel))

(define-public sbcl-cl-markup
  (let ((commit "e0eb7debf4bdff98d1f49d0f811321a6a637b390"))
    (package
      (name "sbcl-cl-markup")
      (version (git-version "0.1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/arielnetworks/cl-markup/")
               (commit commit)))
         (file-name (git-file-name "cl-markup" version))
         (sha256
          (base32
           "10l6k45971dl13fkdmva7zc6i453lmq9j4xax2ci6pjzlc6xjhp7"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/arielnetworks/cl-markup/")
      (synopsis "Markup generation library for Common Lisp")
      (description
       "A modern markup generation library for Common Lisp that features:

@itemize
@item Fast (even faster through compiling the code)
@item Safety
@item Support for multiple document types (markup, xml, html, html5, xhtml)
@item Output with doctype
@item Direct output to stream
@end itemize\n")
      (license license:lgpl3+))))

(define-public cl-markup
  (sbcl-package->cl-source-package sbcl-cl-markup))

(define-public ecl-cl-markup
  (sbcl-package->ecl-package sbcl-cl-markup))

(define-public sbcl-cl-css
  (let ((commit "8fe654c8f0cf95b300718101cce4feb517f78e2f"))
    (package
      (name "sbcl-cl-css")
      (version (git-version "0.1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/inaimathi/cl-css/")
               (commit commit)))
         (file-name (git-file-name "cl-css" version))
         (sha256
          (base32
           "1lc42zi2sw11fl2589sc19nr5sd2p0wy7wgvgwaggxa5f3ajhsmd"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/inaimathi/cl-css/")
      (synopsis "Non-validating, inline CSS generator for Common Lisp")
      (description
       "This is a dead-simple, non validating, inline CSS generator for Common
Lisp.  Its goals are axiomatic syntax, simple implementation to support
portability, and boilerplate reduction in CSS.")
      (license license:expat))))

(define-public cl-css
  (sbcl-package->cl-source-package sbcl-cl-css))

(define-public ecl-cl-css
  (sbcl-package->ecl-package sbcl-cl-css))

(define-public sbcl-portable-threads
  (let ((commit "c0e61a1faeb0583c80fd3f20b16cc4c555226920"))
    (package
      (name "sbcl-portable-threads")
      (version (git-version "2.3" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/binghe/portable-threads/")
               (commit commit)))
         (file-name (git-file-name "portable-threads" version))
         (sha256
          (base32
           "03fmxyarc0xf4kavwkfa0a2spkyfrz6hbgbi9y4q7ny5aykdyfaq"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(;; Tests seem broken.
         #:tests? #f))
      (home-page "https://github.com/binghe/portable-threads")
      (synopsis "Portable threads (and scheduled and periodic functions) API for Common Lisp")
      (description
       "Portable Threads (and Scheduled and Periodic Functions) API for Common
Lisp (from GBBopen project).")
      (license license:asl2.0))))

(define-public cl-portable-threads
  (sbcl-package->cl-source-package sbcl-portable-threads))

(define-public ecl-portable-threada
  (sbcl-package->ecl-package sbcl-portable-threads))

(define sbcl-usocket-boot0
  ;; usocket's test rely on usocket-server which depends on usocket itself.
  ;; We break this cyclic dependency with -boot0 that packages usocket.
  (package
    (name "sbcl-usocket-boot0")
    (version "0.8.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/usocket/usocket/")
             (commit (string-append "v" version))))
       (file-name (git-file-name "usocket" version))
       (sha256
        (base32
         "0x746wr2324l6bn7skqzgkzcbj5kd0zp2ck0c8rldrw0rzabg826"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     `(("split-sequence" ,sbcl-split-sequence)))
    (arguments
     `(#:tests? #f
       #:asd-system-name "usocket"))
    (home-page "https://common-lisp.net/project/usocket/")
    (synopsis "Universal socket library for Common Lisp (server side)")
    (description
     "This library strives to provide a portable TCP/IP and UDP/IP socket
interface for as many Common Lisp implementations as possible, while keeping
the abstraction and portability layer as thin as possible.")
    (license license:expat)))

(define-public sbcl-usocket-server
  (package
    (inherit sbcl-usocket-boot0)
    (name "sbcl-usocket-server")
    (inputs
     `(("bordeaux-threads" ,sbcl-bordeaux-threads)
       ("usocket" ,sbcl-usocket-boot0)))
    (arguments
     '(#:asd-system-name "usocket-server"))
    (synopsis "Universal socket library for Common Lisp (server side)")))

(define-public cl-usocket-server
  (sbcl-package->cl-source-package sbcl-usocket-server))

(define-public ecl-socket-server
  (sbcl-package->ecl-package sbcl-usocket-server))

(define-public sbcl-usocket
  (package
    (inherit sbcl-usocket-boot0)
    (name "sbcl-usocket")
    (arguments
     ;; FIXME: Tests need network access?
     `(#:tests? #f))
    (native-inputs
     ;; Testing only.
     `(("usocket-server" ,sbcl-usocket-server)
       ("rt" ,sbcl-rt)))))

(define-public cl-usocket
  (sbcl-package->cl-source-package sbcl-usocket))

(define-public ecl-usocket
  (sbcl-package->ecl-package sbcl-usocket))

(define-public sbcl-s-xml
  (package
    (name "sbcl-s-xml")
    (version "3")
    (source
     (origin
       (method url-fetch)
       (uri "https://common-lisp.net/project/s-xml/s-xml.tgz")
       (sha256
        (base32
         "061qcr0dzshsa38s5ma4ay924cwak2nq9gy59dw6v9p0qb58nzjf"))))
    (build-system asdf-build-system/sbcl)
    (home-page "https://common-lisp.net/project/s-xml/")
    (synopsis "Simple XML parser implemented in Common Lisp")
    (description
     "S-XML is a simple XML parser implemented in Common Lisp.  This XML
parser implementation has the following features:

@itemize
@item It works (handling many common XML usages).
@item It is very small (the core is about 700 lines of code, including
comments and whitespace).
@item It has a core API that is simple, efficient and pure functional, much
like that from SSAX (see also http://ssax.sourceforge.net).
@item It supports different DOM models: an XSML-based one, an LXML-based one
and a classic xml-element struct based one.
@item It is reasonably time and space efficient (internally avoiding garbage
generatation as much as possible).
@item It does support CDATA.
@item It should support the same character sets as your Common Lisp
implementation.
@item It does support XML name spaces.
@end itemize

This XML parser implementation has the following limitations:

@itemize
@item It does not support any special tags (like processing instructions).
@item It is not validating, even skips DTD's all together.
@end itemize\n")
    (license license:lgpl3+)))

(define-public cl-s-xml
  (sbcl-package->cl-source-package sbcl-s-xml))

(define-public ecl-s-xml
  (sbcl-package->ecl-package sbcl-s-xml))

(define-public sbcl-s-xml-rpc
  (package
    (name "sbcl-s-xml-rpc")
    (version "7")
    (source
     (origin
       (method url-fetch)
       (uri "https://common-lisp.net/project/s-xml-rpc/s-xml-rpc.tgz")
       (sha256
        (base32
         "02z7k163d51v0pzk8mn1xb6h5s6x64gjqkslhwm3a5x26k2gfs11"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     `(("s-xml" ,sbcl-s-xml)))
    (home-page "https://common-lisp.net/project/s-xml-rpc/")
    (synopsis "Implementation of XML-RPC in Common Lisp for both client and server")
    (description
     "S-XML-RPC is an implementation of XML-RPC in Common Lisp for both
client and server.")
    (license license:lgpl3+)))

(define-public cl-s-xml-rpc
  (sbcl-package->cl-source-package sbcl-s-xml-rpc))

(define-public ecl-s-xml-rpc
  (sbcl-package->ecl-package sbcl-s-xml-rpc))

(define-public sbcl-trivial-clipboard
  (let ((commit "5af3415d1484e6d69a1b5c178f24680d9fd01796"))
    (package
      (name "sbcl-trivial-clipboard")
      (version (git-version "0.0.0.0" "2" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/snmsts/trivial-clipboard")
               (commit commit)))
         (file-name (git-file-name "trivial-clipboard" version))
         (sha256
          (base32
           "1gb515z5yq6h5548pb1fwhmb0hhq1ssyb78pvxh4alq799xipxs9"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("xclip" ,xclip)))
      (native-inputs
       `(("fiveam" ,sbcl-fiveam)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "src/text.lisp"
                 (("\\(executable-find \"xclip\"\\)")
                  (string-append "(executable-find \""
                                 (assoc-ref inputs "xclip")
                                 "/bin/xclip\")"))))))))
      (home-page "https://github.com/snmsts/trivial-clipboard")
      (synopsis "Access system clipboard in Common Lisp")
      (description
       "@command{trivial-clipboard} gives access to the system clipboard.")
      (license license:expat))))

(define-public cl-trivial-clipboard
  (sbcl-package->cl-source-package sbcl-trivial-clipboard))

(define-public ecl-trivial-clipboard
  (sbcl-package->ecl-package sbcl-trivial-clipboard))

(define-public sbcl-trivial-backtrace
  (let ((commit "ca81c011b86424a381a7563cea3b924f24e6fbeb")
        (revision "1"))
    (package
     (name "sbcl-trivial-backtrace")
     (version (git-version "0.0.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gwkkwg/trivial-backtrace.git")
             (commit commit)))
       (file-name (git-file-name "trivial-backtrace" version))
       (sha256
        (base32 "10p41p43skj6cimdg8skjy7372s8v2xpkg8djjy0l8rm45i654k1"))))
     (build-system asdf-build-system/sbcl)
     (inputs
      `(("sbcl-lift" ,sbcl-lift)))
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-after 'check 'delete-test-results
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((test-results (string-append (assoc-ref outputs "out")
                                                 "/share/common-lisp/"
                                                 (%lisp-type) "-source"
                                                 "/trivial-backtrace"
                                                 "/test-results")))
                (when (file-exists? test-results)
                  (delete-file-recursively test-results)))
              #t)))))
     (home-page "https://common-lisp.net/project/trivial-backtrace/")
     (synopsis "Portable simple API to work with backtraces in Common Lisp")
     (description
      "On of the many things that didn't quite get into the Common Lisp
standard was how to get a Lisp to output its call stack when something has
gone wrong.  As such, each Lisp has developed its own notion of what to
display, how to display it, and what sort of arguments can be used to
customize it.  @code{trivial-backtrace} is a simple solution to generating a
backtrace portably.")
     (license license:expat))))

(define-public cl-trivial-backtrace
  (sbcl-package->cl-source-package sbcl-trivial-backtrace))

(define-public sbcl-rfc2388
  (let ((commit "591bcf7e77f2c222c43953a80f8c297751dc0c4e")
        (revision "1"))
    (package
     (name "sbcl-rfc2388")
     (version (git-version "0.0.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jdz/rfc2388.git")
             (commit commit)))
       (file-name (git-file-name "rfc2388" version))
       (sha256
        (base32 "0phh5n3clhl9ji8jaxrajidn22d3f0aq87mlbfkkxlnx2pnw694k"))))
     (build-system asdf-build-system/sbcl)
     (home-page "https://github.com/jdz/rfc2388/")
     (synopsis "An implementation of RFC 2388 in Common Lisp")
     (description
      "This package contains an implementation of RFC 2388, which is used to
process form data posted with HTTP POST method using enctype
\"multipart/form-data\".")
     (license license:bsd-2))))

(define-public cl-rfc2388
  (sbcl-package->cl-source-package sbcl-rfc2388))

(define-public sbcl-md5
  (package
    (name "sbcl-md5")
    (version "2.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/pmai/md5/archive/release-" version ".tar.gz"))
       (sha256
        (base32 "19yl9n0pjdz5gw4qi711lka97xcd9f81ylg434hk7jwn9f2s6w11"))))
    (build-system asdf-build-system/sbcl)
    (home-page "https://github.com/pmai/md5")
    (synopsis
     "Common Lisp implementation of the MD5 Message-Digest Algorithm (RFC 1321)")
    (description
     "This package implements The MD5 Message-Digest Algorithm, as defined in
RFC 1321 by R. Rivest, published April 1992.")
    (license license:public-domain)))

(define-public cl-md5
  (sbcl-package->cl-source-package sbcl-md5))

(define-public sbcl-cl+ssl
  (let ((commit "141ae91416bc40f1618dc07e48429b84388aa599")
        (revision "1"))
    (package
      (name "sbcl-cl+ssl")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/cl-plus-ssl/cl-plus-ssl.git")
               (commit commit)))
         (file-name (git-file-name "cl+ssl" version))
         (sha256
          (base32 "1s0hg1h9sf8q89v0yrxmzg5f5sng29rgx3n21r9h9yql8351myan"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "src/reload.lisp"
                 (("libssl.so" all)
                  (string-append
                   (assoc-ref inputs "openssl") "/lib/" all))))))))
      (inputs
       `(("openssl" ,openssl)
         ("sbcl-cffi" ,sbcl-cffi)
         ("sbcl-trivial-gray-streams" ,sbcl-trivial-gray-streams)
         ("sbcl-flexi-streams" ,sbcl-flexi-streams)
         ("sbcl-bordeaux-threads" ,sbcl-bordeaux-threads)
         ("sbcl-trivial-garbage" ,sbcl-trivial-garbage)
         ("sbcl-alexandria" ,sbcl-alexandria)
         ("sbcl-trivial-features" ,sbcl-trivial-features)))
      (home-page "https://common-lisp.net/project/cl-plus-ssl/")
      (synopsis "Common Lisp bindings to OpenSSL")
      (description
       "This library is a fork of SSL-CMUCL.  The original SSL-CMUCL source
code was written by Eric Marsden and includes contributions by Jochen Schmidt.
Development into CL+SSL was done by David Lichteblau.")
      (license license:expat))))

(define-public cl-cl+ssl
  (sbcl-package->cl-source-package sbcl-cl+ssl))

(define-public sbcl-kmrcl
  (let ((version "1.109.0")
        (commit "5260068b2eb735af6796740c2db4955afac21636")
        (revision "1"))
    (package
      (name "sbcl-kmrcl")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "http://git.kpe.io/kmrcl.git/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1va7xjgzfv674bpsli674i7zj3f7wg5kxic41kz18r6hh4n52dfv"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       ;; Tests fail with: :FORCE and :FORCE-NOT arguments not allowed in a
       ;; nested call to ASDF/OPERATE:OPERATE unless identically to toplevel
       '(#:tests? #f))
      (inputs
       `(("sbcl-rt" ,sbcl-rt)))
      (home-page "http://files.kpe.io/kmrcl/")
      (synopsis "General utilities for Common Lisp programs")
      (description
       "KMRCL is a collection of utilities used by a number of Kevin
Rosenberg's CL packages.")
      (license license:llgpl))))

(define-public cl-kmrcl
  (sbcl-package->cl-source-package sbcl-kmrcl))

(define-public sbcl-cl-base64
  (let ((version "3.3.3"))
    (package
      (name "sbcl-cl-base64")
      (version version)
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "http://git.kpe.io/cl-base64.git")
               (commit (string-append "v" version))))
         (file-name (git-file-name "cl-base64" version))
         (sha256
          (base32 "1dw6j7n6gsd2qa0p0rbsjxj00acxx3i9ca1qkgl0liy8lpnwkypl"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       ;; Tests fail with: :FORCE and :FORCE-NOT arguments not allowed
       ;; in a nested call to ASDF/OPERATE:OPERATE unless identically
       ;; to toplevel
       '(#:tests? #f))
      (inputs
       `(("sbcl-ptester" ,sbcl-ptester)
         ("sbcl-kmrcl" ,sbcl-kmrcl)))
      (home-page "http://files.kpe.io/cl-base64/")
      (synopsis
       "Common Lisp package to encode and decode base64 with URI support")
      (description
       "This package provides highly optimized base64 encoding and decoding.
Besides conversion to and from strings, integer conversions are supported.
Encoding with Uniform Resource Identifiers is supported by using a modified
encoding table that uses only URI-compatible characters.")
      (license license:bsd-3))))

(define-public cl-base64
  (sbcl-package->cl-source-package sbcl-cl-base64))

(define-public sbcl-chunga
  (package
    (name "sbcl-chunga")
    (version "1.1.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/edicl/chunga.git")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jzn3nyb3f22gm983rfk99smqs3mhb9ivjmasvhq9qla5cl9pyhd"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     `(("sbcl-trivial-gray-streams" ,sbcl-trivial-gray-streams)))
    (home-page "https://edicl.github.io/chunga/")
    (synopsis "Portable chunked streams for Common Lisp")
    (description
     "Chunga implements streams capable of chunked encoding on demand as
defined in RFC 2616.")
    (license license:bsd-2)))

(define-public cl-chunga
  (sbcl-package->cl-source-package sbcl-chunga))

(define-public sbcl-cl-who
  (let ((version "1.1.4")
        (commit "2c08caa4bafba720409af9171feeba3f32e86d32")
        (revision "1"))
    (package
      (name "sbcl-cl-who")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/edicl/cl-who.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0yjb6sr3yazm288m318kqvj9xk8rm9n1lpimgf65ymqv0i5agxsb"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       `(("sbcl-flexi-streams" ,sbcl-flexi-streams)))
      (home-page "https://edicl.github.io/cl-who/")
      (synopsis "Yet another Lisp markup language")
      (description
       "There are plenty of Lisp Markup Languages out there - every Lisp
programmer seems to write at least one during his career - and CL-WHO (where
WHO means \"with-html-output\" for want of a better acronym) is probably just
as good or bad as the next one.")
      (license license:bsd-2))))

(define-public cl-cl-who
  (sbcl-package->cl-source-package sbcl-cl-who))

(define-public sbcl-chipz
  (let ((version "0.8")
        (commit "75dfbc660a5a28161c57f115adf74c8a926bfc4d")
        (revision "1"))
    (package
      (name "sbcl-chipz")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/froydnj/chipz.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0plx4rs39zbs4gjk77h4a2q11zpy75fh9v8hnxrvsf8fnakajhwg"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       `(("sbcl-flexi-streams" ,sbcl-flexi-streams)))
      (home-page "http://method-combination.net/lisp/chipz/")
      (synopsis
       "Common Lisp library for decompressing deflate, zlib, gzip, and bzip2
data")
      (description
       "DEFLATE data, defined in RFC1951, forms the core of popular
compression formats such as zlib (RFC 1950) and gzip (RFC 1952).  As such,
Chipz also provides for decompressing data in those formats as well.  BZIP2 is
the format used by the popular compression tool bzip2.")
      ;; The author describes it as "MIT-like"
      (license license:expat))))

(define-public cl-chipz
  (sbcl-package->cl-source-package sbcl-chipz))

(define-public sbcl-drakma
  (package
    (name "sbcl-drakma")
    (version "2.0.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/edicl/drakma.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1441idnyif9xzx3ln1p3fg36k2v9h4wasjqrzc8y52j61420qpci"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     `(("sbcl-puri" ,sbcl-puri)
       ("sbcl-cl-base64" ,sbcl-cl-base64)
       ("sbcl-chunga" ,sbcl-chunga)
       ("sbcl-flexi-streams" ,sbcl-flexi-streams)
       ("sbcl-cl-ppcre" ,sbcl-cl-ppcre)
       ("sbcl-chipz" ,sbcl-chipz)
       ("sbcl-usocket" ,sbcl-usocket)
       ("sbcl-cl+ssl" ,sbcl-cl+ssl)))
    (native-inputs
     `(("sbcl-fiveam" ,sbcl-fiveam)))
    (home-page "https://edicl.github.io/drakma/")
    (synopsis "HTTP client written in Common Lisp")
    (description
     "Drakma is a full-featured HTTP client implemented in Common Lisp.  It
knows how to handle HTTP/1.1 chunking, persistent connections, re-usable
sockets, SSL, continuable uploads, file uploads, cookies, and more.")
    (license license:bsd-2)))

(define-public cl-drakma
  (sbcl-package->cl-source-package sbcl-drakma))

(define-public ecl-drakma
  (sbcl-package->ecl-package sbcl-drakma))

(define-public sbcl-hunchentoot
  (package
    (name "sbcl-hunchentoot")
    (version "1.2.38")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/edicl/hunchentoot.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name "hunchentoot" version))
       (sha256
        (base32 "1anpcad7w045m4rsjs1f3xdhjwx5cppq1h0vlb3q7dz81fi3i6yq"))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     `(("sbcl-cl-who" ,sbcl-cl-who)
       ("sbcl-drakma" ,sbcl-drakma)))
    (inputs
     `(("sbcl-chunga" ,sbcl-chunga)
       ("sbcl-cl-base64" ,sbcl-cl-base64)
       ("sbcl-cl-fad" ,sbcl-cl-fad)
       ("sbcl-cl-ppcre" ,sbcl-cl-ppcre)
       ("sbcl-flexi-streams" ,sbcl-flexi-streams)
       ("sbcl-cl+ssl" ,sbcl-cl+ssl)
       ("sbcl-md5" ,sbcl-md5)
       ("sbcl-rfc2388" ,sbcl-rfc2388)
       ("sbcl-trivial-backtrace" ,sbcl-trivial-backtrace)
       ("sbcl-usocket" ,sbcl-usocket)))
    (home-page "https://edicl.github.io/hunchentoot/")
    (synopsis "Web server written in Common Lisp")
    (description
     "Hunchentoot is a web server written in Common Lisp and at the same
time a toolkit for building dynamic websites.  As a stand-alone web server,
Hunchentoot is capable of HTTP/1.1 chunking (both directions), persistent
connections (keep-alive), and SSL.")
    (license license:bsd-2)))

(define-public cl-hunchentoot
  (sbcl-package->cl-source-package sbcl-hunchentoot))

(define-public sbcl-trivial-types
  (package
    (name "sbcl-trivial-types")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/m2ym/trivial-types.git")
             (commit "ee869f2b7504d8aa9a74403641a5b42b16f47d88")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s4cp9bdlbn8447q7w7f1wkgwrbvfzp20mgs307l5pxvdslin341"))))
    (build-system asdf-build-system/sbcl)
    (home-page "https://github.com/m2ym/trivial-types")
    (synopsis "Trivial type definitions for Common Lisp")
    (description
     "TRIVIAL-TYPES provides missing but important type definitions such as
PROPER-LIST, ASSOCIATION-LIST, PROPERTY-LIST and TUPLE.")
    (license license:llgpl)))

(define-public cl-trivial-types
  (sbcl-package->cl-source-package sbcl-trivial-types))

(define-public sbcl-cl-syntax
  (package
    (name "sbcl-cl-syntax")
    (version "0.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/m2ym/cl-syntax.git")
             (commit "03f0c329bbd55b8622c37161e6278366525e2ccc")))
       (file-name (git-file-name "cl-syntax" version))
       (sha256
        (base32 "17ran8xp77asagl31xv8w819wafh6whwfc9p6dgx22ca537gyl4y"))))
    (build-system asdf-build-system/sbcl)
    (arguments
     '(#:asd-file "cl-syntax.asd"
       #:asd-system-name "cl-syntax"))
    (inputs `(("sbcl-trivial-types" ,sbcl-trivial-types)
              ("sbcl-named-readtables" ,sbcl-named-readtables)))
    (home-page "https://github.com/m2ym/cl-syntax")
    (synopsis "Reader Syntax Coventions for Common Lisp and SLIME")
    (description
     "CL-SYNTAX provides Reader Syntax Coventions for Common Lisp and SLIME.")
    (license license:llgpl)))

(define-public cl-syntax
  (sbcl-package->cl-source-package sbcl-cl-syntax))

(define-public sbcl-cl-annot
  (let ((commit "c99e69c15d935eabc671b483349a406e0da9518d")
        (revision "1"))
    (package
      (name "sbcl-cl-annot")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/m2ym/cl-annot.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1wq1gs9jjd5m6iwrv06c2d7i5dvqsfjcljgbspfbc93cg5xahk4n"))))
      (build-system asdf-build-system/sbcl)
    (arguments
     '(#:asd-file "cl-annot.asd"
       #:asd-system-name "cl-annot"))
      (inputs
       `(("sbcl-alexandria" ,sbcl-alexandria)))
      (home-page "https://github.com/m2ym/cl-annot")
      (synopsis "Python-like Annotation Syntax for Common Lisp.")
      (description
       "@code{cl-annot} is an general annotation library for Common Lisp.")
      (license license:llgpl))))

(define-public cl-annot
  (sbcl-package->cl-source-package sbcl-cl-annot))

(define-public sbcl-cl-syntax-annot
  (package
    (name "sbcl-cl-syntax-annot")
    (version "0.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/m2ym/cl-syntax.git")
             (commit "03f0c329bbd55b8622c37161e6278366525e2ccc")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17ran8xp77asagl31xv8w819wafh6whwfc9p6dgx22ca537gyl4y"))))
    (build-system asdf-build-system/sbcl)
    (arguments
     '(#:asd-file "cl-syntax-annot.asd"
       #:asd-system-name "cl-syntax-annot"))
    (inputs
     `(("sbcl-cl-syntax" ,sbcl-cl-syntax)
       ("sbcl-cl-annot" ,sbcl-cl-annot)))
    (home-page "https://github.com/m2ym/cl-syntax")
    (synopsis "Reader Syntax Coventions for Common Lisp and SLIME")
    (description
     "CL-SYNTAX provides Reader Syntax Coventions for Common Lisp and
SLIME.")
    (license license:llgpl)))

(define-public cl-syntax-annot
  (sbcl-package->cl-source-package sbcl-cl-syntax-annot))

(define-public sbcl-cl-utilities
  (let ((commit "dce2d2f6387091ea90357a130fa6d13a6776884b")
        (revision "1"))
    (package
      (name "sbcl-cl-utilities")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method url-fetch)
         (uri
          (string-append
           "https://gitlab.common-lisp.net/cl-utilities/cl-utilities/-/"
           "archive/" commit "/cl-utilities-" commit ".tar.gz"))
         (sha256
          (base32 "1r46v730yf96nk2vb24qmagv9x96xvd08abqwhf02ghgydv1a7z2"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       '(#:asd-file "cl-utilities.asd"
         #:asd-system-name "cl-utilities"
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "rotate-byte.lisp"
                 (("in-package :cl-utilities)" all)
                  "in-package :cl-utilities)\n\n#+sbcl\n(require :sb-rotate-byte)")))))))
      (home-page "http://common-lisp.net/project/cl-utilities")
      (synopsis "A collection of semi-standard utilities")
      (description
       "On Cliki.net <http://www.cliki.net/Common%20Lisp%20Utilities>, there
is a collection of Common Lisp Utilities, things that everybody writes since
they're not part of the official standard.  There are some very useful things
there; the only problems are that they aren't implemented as well as you'd
like (some aren't implemented at all) and they aren't conveniently packaged
and maintained.  It takes quite a bit of work to carefully implement utilities
for common use, commented and documented, with error checking placed
everywhere some dumb user might make a mistake.")
      (license license:public-domain))))

(define-public cl-utilities
  (sbcl-package->cl-source-package sbcl-cl-utilities))

(define-public sbcl-map-set
  (let ((commit "7b4b545b68b8")
        (revision "1"))
    (package
      (name "sbcl-map-set")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method url-fetch)
         (uri (string-append
               "https://bitbucket.org/tarballs_are_good/map-set/get/"
               commit ".tar.gz"))
         (sha256
          (base32 "1sx5j5qdsy5fklspfammwb16kjrhkggdavm922a9q86jm5l0b239"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://bitbucket.org/tarballs_are_good/map-set")
      (synopsis "Set-like data structure")
      (description
       "Implementation of a set-like data structure with constant time
addition, removal, and random selection.")
      (license license:bsd-3))))

(define-public cl-map-set
  (sbcl-package->cl-source-package sbcl-map-set))

(define-public sbcl-quri
  (let ((commit "76b75103f21ead092c9f715512fa82441ef61185")
        (revision "1"))
    (package
      (name "sbcl-quri")
      (version (git-version "0.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/fukamachi/quri.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1ccbxsgzdibmzq33mmbmmz9vwl6l03xh6nbpsh1hkdvdcl7q0a60"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       ;; Tests fail with: Component QURI-ASD::QURI-TEST not found,
       ;; required by #<SYSTEM "quri">. Why?
       '(#:tests? #f))
      (native-inputs `(("sbcl-prove-asdf" ,sbcl-prove-asdf)
                       ("sbcl-prove" ,sbcl-prove)))
      (inputs `(("sbcl-babel" ,sbcl-babel)
                ("sbcl-split-sequence" ,sbcl-split-sequence)
                ("sbcl-cl-utilities" ,sbcl-cl-utilities)
                ("sbcl-alexandria" ,sbcl-alexandria)))
      (home-page "https://github.com/fukamachi/quri")
      (synopsis "Yet another URI library for Common Lisp")
      (description
       "QURI (pronounced \"Q-ree\") is yet another URI library for Common
Lisp. It is intended to be a replacement of PURI.")
      (license license:bsd-3))))

(define-public cl-quri
  (sbcl-package->cl-source-package sbcl-quri))

(define-public sbcl-myway
  (let ((commit "286230082a11f879c18b93f17ca571c5f676bfb7")
        (revision "1"))
    (package
     (name "sbcl-myway")
     (version (git-version "0.1.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fukamachi/myway.git")
             (commit commit)))
       (file-name (git-file-name "myway" version))
       (sha256
        (base32 "0briia9bk3lbr0frnx39d1qg6i38dm4j6z9w3yga3d40k6df4a90"))))
     (build-system asdf-build-system/sbcl)
     (arguments
      ;; Tests fail with: Component MYWAY-ASD::MYWAY-TEST not found, required
      ;; by #<SYSTEM "myway">. Why?
      '(#:tests? #f))
     (native-inputs
      `(("sbcl-prove-asdf" ,sbcl-prove-asdf)
        ("sbcl-prove" ,sbcl-prove)))
     (inputs
      `(("sbcl-cl-ppcre" ,sbcl-cl-ppcre)
        ("sbcl-quri" ,sbcl-quri)
        ("sbcl-map-set" ,sbcl-map-set)))
     (home-page "https://github.com/fukamachi/myway")
     (synopsis "Sinatra-compatible URL routing library for Common Lisp")
     (description "My Way is a Sinatra-compatible URL routing library.")
     (license license:llgpl))))

(define-public cl-myway
  (sbcl-package->cl-source-package sbcl-myway))

(define-public sbcl-xsubseq
  (let ((commit "5ce430b3da5cda3a73b9cf5cee4df2843034422b")
        (revision "1"))
    (package
     (name "sbcl-xsubseq")
     (version (git-version "0.0.1" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fukamachi/xsubseq")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xz79q0p2mclf3sqjiwf6izdpb6xrsr350bv4mlmdlm6rg5r99px"))))
     (build-system asdf-build-system/sbcl)
     (arguments
      ;; Tests fail with: Component XSUBSEQ-ASD::XSUBSEQ-TEST not found,
      ;; required by #<SYSTEM "xsubseq">. Why?
      '(#:tests? #f))
     (native-inputs
      `(("sbcl-prove-asdf" ,sbcl-prove-asdf)
        ("sbcl-prove" ,sbcl-prove)))
     (home-page "https://github.com/fukamachi/xsubseq")
     (synopsis "Efficient way to use \"subseq\"s in Common Lisp")
     (description
      "XSubseq provides functions to be able to handle \"subseq\"s more
effieiently.")
     (license license:bsd-2))))

(define-public cl-xsubseq
  (sbcl-package->cl-source-package sbcl-xsubseq))

(define-public sbcl-smart-buffer
  (let ((commit "09b9a9a0b3abaa37abe9a730f5aac2643dca4e62")
        (revision "1"))
    (package
      (name "sbcl-smart-buffer")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/fukamachi/smart-buffer")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0qz1zzxx0wm5ff7gpgsq550a59p0qj594zfmm2rglj97dahj54l7"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       ;; Tests fail with: Component SMART-BUFFER-ASD::SMART-BUFFER-TEST not
       ;; found, required by #<SYSTEM "smart-buffer">. Why?
       `(#:tests? #f))
      (native-inputs
       `(("sbcl-prove-asdf" ,sbcl-prove-asdf)
         ("sbcl-prove" ,sbcl-prove)))
      (inputs
       `(("sbcl-xsubseq" ,sbcl-xsubseq)
         ("sbcl-flexi-streams" ,sbcl-flexi-streams)))
      (home-page "https://github.com/fukamachi/smart-buffer")
      (synopsis "Smart octets buffer")
      (description
       "Smart-buffer provides an output buffer which changes the destination
depending on content size.")
      (license license:bsd-3))))

(define-public cl-smart-buffer
  (sbcl-package->cl-source-package sbcl-smart-buffer))

(define-public sbcl-fast-http
  (let ((commit "f9e7597191bae380503e20724fd493a24d024935")
        (revision "1"))
    (package
      (name "sbcl-fast-http")
      (version (git-version "0.2.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/fukamachi/fast-http")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0qdmwv2zm0sizxdb8nnclgwl0nfjcbjaimbakavikijw7lr9b4jp"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       ;; Tests fail with: Component FAST-HTTP-ASD::FAST-HTTP-TEST not found,
       ;; required by #<SYSTEM "fast-http">. Why?
       `(#:tests? #f))
      (native-inputs
       `(("sbcl-prove-asdf" ,sbcl-prove-asdf)
         ("sbcl-prove" ,sbcl-prove)))
      (inputs
       `(("sbcl-alexandria" ,sbcl-alexandria)
         ("sbcl-proc-parse" ,sbcl-proc-parse)
         ("sbcl-xsubseq" ,sbcl-xsubseq)
         ("sbcl-smart-buffer" ,sbcl-smart-buffer)
         ("sbcl-cl-utilities" ,sbcl-cl-utilities)))
      (home-page "https://github.com/fukamachi/fast-http")
      (synopsis "HTTP request/response parser for Common Lisp")
      (description
       "@code{fast-http} is a HTTP request/response protocol parser for Common
Lisp.")
      ;; Author specified the MIT license
      (license license:expat))))

(define-public cl-fast-http
  (sbcl-package->cl-source-package sbcl-fast-http))

(define-public sbcl-static-vectors
  (package
    (name "sbcl-static-vectors")
    (version "1.8.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sionescu/static-vectors.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qvf9z6bhwhm8n45fjwkm7j8dcb58szfvndky65cyn4lpdval7m1"))))
    (native-inputs
     `(("sbcl-fiveam" ,sbcl-fiveam)))
    (inputs
     `(("sbcl-cffi-grovel" ,sbcl-cffi-grovel)
       ("sbcl-cffi" ,sbcl-cffi)))
    (build-system asdf-build-system/sbcl)
    (home-page "https://github.com/sionescu/static-vectors")
    (synopsis "Allocate SIMPLE-ARRAYs in static memory")
    (description
     "With @code{static-vectors}, you can create vectors allocated in static
memory.")
    (license license:expat)))

(define-public cl-static-vectors
  (sbcl-package->cl-source-package sbcl-static-vectors))

(define-public ecl-static-vectors
  (sbcl-package->ecl-package sbcl-static-vectors))

(define-public sbcl-marshal
  (let ((commit "eff1b15f2b0af2f26f71ad6a4dd5c4beab9299ec")
        (revision "1"))
    (package
     (name "sbcl-marshal")
     (version (git-version "1.3.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/wlbr/cl-marshal.git")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08qs6fhk38xpkkjkpcj92mxx0lgy4ygrbbzrmnivdx281syr0gwh"))))
     (build-system asdf-build-system/sbcl)
     (home-page "https://github.com/wlbr/cl-marshal")
     (synopsis "Simple (de)serialization of Lisp datastructures")
     (description
      "Simple and fast marshalling of Lisp datastructures.  Convert any object
into a string representation, put it on a stream an revive it from there.
Only minimal changes required to make your CLOS objects serializable.")
     (license license:expat))))

(define-public cl-marshal
  (sbcl-package->cl-source-package sbcl-marshal))

(define-public sbcl-checkl
  (let ((commit "80328800d047fef9b6e32dfe6bdc98396aee3cc9")
        (revision "1"))
    (package
      (name "sbcl-checkl")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rpav/CheckL.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0bpisihx1gay44xmyr1dmhlwh00j0zzi04rp9fy35i95l2r4xdlx"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       ;; Error while trying to load definition for system checkl-test from
       ;; pathname [...]/checkl-test.asd: The function CHECKL:DEFINE-TEST-OP
       ;; is undefined.
       '(#:tests? #f))
      (native-inputs
       `(("sbcl-fiveam" ,sbcl-fiveam)))
      (inputs
       `(("sbcl-marshal" ,sbcl-marshal)))
      (home-page "https://github.com/rpav/CheckL/")
      (synopsis "Dynamic testing for Common Lisp")
      (description
       "CheckL lets you write tests dynamically, it checks resulting values
against the last run.")
      ;; The author specifies both LLGPL and "BSD", but the "BSD" license
      ;; isn't specified anywhere, so I don't know which kind.  LLGPL is the
      ;; stronger of the two and so I think only listing this should suffice.
      (license license:llgpl))))

(define-public cl-checkl
  (sbcl-package->cl-source-package sbcl-checkl))

(define-public sbcl-fast-io
  (let ((commit "dc3a71db7e9b756a88781ae9c342fe9d4bbab51c")
        (revision "1"))
    (package
     (name "sbcl-fast-io")
     (version (git-version "1.0.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rpav/fast-io.git")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jsp6xvi26ln6fdy5j5zi05xvan8jsqdhisv552dy6xg6ws8i1yq"))))
     (build-system asdf-build-system/sbcl)
     (arguments
      ;; Error while trying to load definition for system fast-io-test from
      ;; pathname [...]/fast-io-test.asd: The function CHECKL:DEFINE-TEST-OP
      ;; is undefined.
      '(#:tests? #f))
     (native-inputs
      `(("sbcl-fiveam" ,sbcl-fiveam)
        ("sbcl-checkl" ,sbcl-checkl)))
     (inputs
      `(("sbcl-alexandria" ,sbcl-alexandria)
        ("sbcl-trivial-gray-streams" ,sbcl-trivial-gray-streams)
        ("sbcl-static-vectors" ,sbcl-static-vectors)))
     (home-page "https://github.com/rpav/fast-io")
     (synopsis "Fast octet-vector/stream I/O for Common Lisp")
     (description
      "Fast-io is about improving performance to octet-vectors and octet
streams (though primarily the former, while wrapping the latter).")
     ;; Author specifies this as NewBSD which is an alias
     (license license:bsd-3))))

(define-public cl-fast-io
  (sbcl-package->cl-source-package sbcl-fast-io))

(define-public sbcl-jonathan
  (let ((commit "1f448b4f7ac8265e56e1c02b32ce383e65316300")
        (revision "1"))
    (package
     (name "sbcl-jonathan")
     (version (git-version "0.1.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Rudolph-Miller/jonathan.git")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14x4iwz3mbag5jzzzr4sb6ai0m9r4q4kyypbq32jmsk2dx1hi807"))))
     (build-system asdf-build-system/sbcl)
     (arguments
      ;; Tests fail with: Component JONATHAN-ASD::JONATHAN-TEST not found,
      ;; required by #<SYSTEM "jonathan">. Why?
      `(#:tests? #f))
     (native-inputs
      `(("sbcl-prove-asdf" ,sbcl-prove-asdf)
        ("sbcl-prove" ,sbcl-prove)))
     (inputs
      `(("sbcl-cl-syntax" ,sbcl-cl-syntax)
        ("sbcl-cl-syntax-annot" ,sbcl-cl-syntax-annot)
        ("sbcl-fast-io" ,sbcl-fast-io)
        ("sbcl-proc-parse" ,sbcl-proc-parse)
        ("sbcl-cl-ppcre" ,sbcl-cl-ppcre)))
     (home-page "https://rudolph-miller.github.io/jonathan/overview.html")
     (synopsis "JSON encoder and decoder")
     (description
      "High performance JSON encoder and decoder.  Currently support: SBCL,
CCL.")
     ;; Author specifies the MIT license
     (license license:expat))))

(define-public cl-jonathan
  (sbcl-package->cl-source-package sbcl-jonathan))

(define-public sbcl-http-body
  (let ((commit "dd01dc4f5842e3d29728552e5163acce8386eb73")
        (revision "1"))
    (package
     (name "sbcl-http-body")
     (version (git-version "0.1.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fukamachi/http-body")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jd06snjvxcprhapgfq8sx0y5lrldkvhf206ix6d5a23dd6zcmr0"))))
     (build-system asdf-build-system/sbcl)
     (arguments
      ;; Tests fail with: Component HTTP-BODY-ASD::HTTP-BODY-TEST not
      ;; found, required by #<SYSTEM "http-body">. Why?
      `(#:tests? #f))
     (native-inputs
      `(("sbcl-prove-asdf" ,sbcl-prove-asdf)
        ("sbcl-prove" ,sbcl-prove)))
     (inputs
      `(("sbcl-fast-http" ,sbcl-fast-http)
        ("sbcl-jonathan" ,sbcl-jonathan)
        ("sbcl-quri" ,sbcl-quri)))
     (home-page "https://github.com/fukamachi/http-body")
     (synopsis "HTTP POST data parser")
     (description
      "HTTP-Body parses HTTP POST data and returns POST parameters.  It
supports application/x-www-form-urlencoded, application/json, and
multipart/form-data.")
     (license license:bsd-2))))

(define-public cl-http-body
  (sbcl-package->cl-source-package sbcl-http-body))

(define-public sbcl-circular-streams
  (let ((commit "e770bade1919c5e8533dd2078c93c3d3bbeb38df")
        (revision "1"))
    (package
     (name "sbcl-circular-streams")
     (version (git-version "0.1.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fukamachi/circular-streams")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wpw6d5cciyqcf92f7mvihak52pd5s47kk4qq6f0r2z2as68p5rs"))))
     (build-system asdf-build-system/sbcl)
     (arguments
      ;; The tests depend on cl-test-more which is now prove. Prove
      ;; tests aren't working for some reason.
      `(#:tests? #f))
     (inputs
      `(("sbcl-fast-io" ,sbcl-fast-io)
        ("sbcl-trivial-gray-streams" ,sbcl-trivial-gray-streams)))
     (home-page "https://github.com/fukamachi/circular-streams")
     (synopsis "Circularly readable streams for Common Lisp")
     (description
      "Circular-Streams allows you to read streams circularly by wrapping real
streams. Once you reach end-of-file of a stream, it's file position will be
reset to 0 and you're able to read it again.")
     (license license:llgpl))))

(define-public cl-circular-streams
  (sbcl-package->cl-source-package sbcl-circular-streams))

(define-public sbcl-lack-request
  (let ((commit "abff8efeb0c3a848e6bb0022f2b8b7fa3a1bc88b")
        (revision "1"))
    (package
     (name "sbcl-lack-request")
     (version (git-version "0.1.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fukamachi/lack.git")
             (commit commit)))
       (file-name (git-file-name "lack-request" version))
       (sha256
        (base32 "1avh4ygcj9xcx4m17nj0wnxxaisk26w4ljs2bibzxaln24x7pi85"))))
     (build-system asdf-build-system/sbcl)
     (arguments
      '(#:asd-file "lack-request.asd"
        #:asd-system-name "lack-request"
        #:test-asd-file "t-lack-request.asd"
        ;; XXX: Component :CLACK-TEST not found
        #:tests? #f))
     (native-inputs
      `(("sbcl-prove-asdf" ,sbcl-prove-asdf)
        ("sbcl-prove" ,sbcl-prove)))
     (inputs
      `(("sbcl-quri" ,sbcl-quri)
        ("sbcl-http-body" ,sbcl-http-body)
        ("sbcl-circular-streams" ,sbcl-circular-streams)))
     (home-page "https://github.com/fukamachi/lack")
     (synopsis "Lack, the core of Clack")
     (description
      "Lack is a Common Lisp library which allows web applications to be
constructed of modular components.  It was originally a part of Clack, however
it's going to be rewritten as an individual project since Clack v2 with
performance and simplicity in mind.")
     (license license:llgpl))))

(define-public cl-lack-request
  (sbcl-package->cl-source-package sbcl-lack-request))

(define-public sbcl-local-time
  (let ((commit "beac054eef428552b63d4ae7820c32ffef9a3015")
        (revision "1"))
    (package
     (name "sbcl-local-time")
     (version (git-version "1.0.6" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dlowe-net/local-time.git")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xhkmgxh41dg2wwlsp0h2l41jp144xn4gpxhh0lna6kh0560w2cc"))))
     (build-system asdf-build-system/sbcl)
     (arguments
      ;; TODO: Component :STEFIL not found, required by #<SYSTEM
      ;; "local-time/test">
      '(#:tests? #f))
     (native-inputs
      `(("stefil" ,sbcl-hu.dwim.stefil)))
     (inputs
      `(("sbcl-cl-fad" ,sbcl-cl-fad)))
     (home-page "https://common-lisp.net/project/local-time/")
     (synopsis "Time manipulation library for Common Lisp")
     (description
      "The LOCAL-TIME library is a Common Lisp library for the manipulation of
dates and times.  It is based almost entirely upon Erik Naggum's paper \"The
Long Painful History of Time\".")
     (license license:expat))))

(define-public cl-local-time
  (sbcl-package->cl-source-package sbcl-local-time))

(define-public sbcl-lack-response
  (let ((commit "abff8efeb0c3a848e6bb0022f2b8b7fa3a1bc88b")
        (revision "1"))
    (package
     (name "sbcl-lack-response")
     (version (git-version "0.1.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fukamachi/lack.git")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1avh4ygcj9xcx4m17nj0wnxxaisk26w4ljs2bibzxaln24x7pi85"))))
     (build-system asdf-build-system/sbcl)
     (arguments
      '(#:asd-file "lack-response.asd"
        #:asd-system-name "lack-response"
        ;; XXX: no tests for lack-response.
        #:tests? #f))
     (native-inputs
      `(("sbcl-prove-asdf" ,sbcl-prove-asdf)
        ("sbcl-prove" ,sbcl-prove)))
     (inputs
      `(("sbcl-quri" ,sbcl-quri)
        ("sbcl-http-body" ,sbcl-http-body)
        ("sbcl-circular-streams" ,sbcl-circular-streams)
        ("sbcl-local-time" ,sbcl-local-time)))
     (home-page "https://github.com/fukamachi/lack")
     (synopsis "Lack, the core of Clack")
     (description
      "Lack is a Common Lisp library which allows web applications to be
constructed of modular components.  It was originally a part of Clack, however
it's going to be rewritten as an individual project since Clack v2 with
performance and simplicity in mind.")
     (license license:llgpl))))

(define-public cl-lack-response
  (sbcl-package->cl-source-package sbcl-lack-response))

(define-public sbcl-lack-component
  (let ((commit "abff8efeb0c3a848e6bb0022f2b8b7fa3a1bc88b")
        (revision "1"))
    (package
     (name "sbcl-lack-component")
     (version (git-version "0.0.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fukamachi/lack.git")
             (commit commit)))
       (file-name (git-file-name "lack-component" version))
       (sha256
        (base32 "1avh4ygcj9xcx4m17nj0wnxxaisk26w4ljs2bibzxaln24x7pi85"))))
     (build-system asdf-build-system/sbcl)
     (arguments
      '(#:asd-file "lack-component.asd"
        #:asd-system-name "lack-component"
        #:test-asd-file "t-lack-component.asd"
        ;; XXX: Component :LACK-TEST not found
        #:tests? #f))
     (native-inputs
      `(("prove-asdf" ,sbcl-prove-asdf)))
     (home-page "https://github.com/fukamachi/lack")
     (synopsis "Lack, the core of Clack")
     (description
      "Lack is a Common Lisp library which allows web applications to be
constructed of modular components.  It was originally a part of Clack, however
it's going to be rewritten as an individual project since Clack v2 with
performance and simplicity in mind.")
     (license license:llgpl))))

(define-public cl-lack-component
  (sbcl-package->cl-source-package sbcl-lack-component))

(define-public sbcl-lack-util
  (let ((commit "abff8efeb0c3a848e6bb0022f2b8b7fa3a1bc88b")
        (revision "1"))
    (package
     (name "sbcl-lack-util")
     (version (git-version "0.1.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fukamachi/lack.git")
             (commit commit)))
       (file-name (git-file-name "lack-util" version))
       (sha256
        (base32 "1avh4ygcj9xcx4m17nj0wnxxaisk26w4ljs2bibzxaln24x7pi85"))))
     (build-system asdf-build-system/sbcl)
     (arguments
      '(#:asd-file "lack-util.asd"
        #:asd-system-name "lack-util"
        #:test-asd-file "t-lack-util.asd"
        ;; XXX: Component :LACK-TEST not found
        #:tests? #f))
     (native-inputs
      `(("prove-asdf" ,sbcl-prove-asdf)))
     (inputs
      `(("sbcl-ironclad" ,sbcl-ironclad)))
     (home-page "https://github.com/fukamachi/lack")
     (synopsis "Lack, the core of Clack")
     (description
      "Lack is a Common Lisp library which allows web applications to be
constructed of modular components.  It was originally a part of Clack, however
it's going to be rewritten as an individual project since Clack v2 with
performance and simplicity in mind.")
     (license license:llgpl))))

(define-public cl-lack-util
  (sbcl-package->cl-source-package sbcl-lack-util))

(define-public sbcl-lack-middleware-backtrace
  (let ((commit "abff8efeb0c3a848e6bb0022f2b8b7fa3a1bc88b")
        (revision "1"))
    (package
     (name "sbcl-lack-middleware-backtrace")
     (version (git-version "0.1.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fukamachi/lack.git")
             (commit commit)))
       (file-name (git-file-name "lack-middleware-backtrace" version))
       (sha256
        (base32 "1avh4ygcj9xcx4m17nj0wnxxaisk26w4ljs2bibzxaln24x7pi85"))))
     (build-system asdf-build-system/sbcl)
     (arguments
      '(#:asd-file "lack-middleware-backtrace.asd"
        #:asd-system-name "lack-middleware-backtrace"
        #:test-asd-file "t-lack-middleware-backtrace.asd"
        ;; XXX: Component :LACK not found
        #:tests? #f))
     (native-inputs
      `(("prove-asdf" ,sbcl-prove-asdf)))
     (home-page "https://github.com/fukamachi/lack")
     (synopsis "Lack, the core of Clack")
     (description
      "Lack is a Common Lisp library which allows web applications to be
constructed of modular components.  It was originally a part of Clack, however
it's going to be rewritten as an individual project since Clack v2 with
performance and simplicity in mind.")
     (license license:llgpl))))

(define-public cl-lack-middleware-backtrace
  (sbcl-package->cl-source-package sbcl-lack-middleware-backtrace))

(define-public sbcl-trivial-mimes
  (let ((commit "303f8ac0aa6ca0bc139aa3c34822e623c3723fab")
        (revision "1"))
    (package
      (name "sbcl-trivial-mimes")
      (version (git-version "1.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shinmera/trivial-mimes.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "17jxgl47r695bvsb7wi3n2ws5rp1zzgvw0zii8cy5ggw4b4ayv6m"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after
               'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((anchor "#p\"/etc/mime.types\""))
                 (substitute* "mime-types.lisp"
                   ((anchor all)
                    (string-append
                     anchor "\n"
                     "(asdf:system-relative-pathname :trivial-mimes "
                     "\"../../share/common-lisp/" (%lisp-type)
                     "-source/trivial-mimes/mime.types\")")))))))))
      (native-inputs
       `(("stefil" ,sbcl-hu.dwim.stefil)))
      (inputs
       `(("sbcl-cl-fad" ,sbcl-cl-fad)))
      (home-page "https://shinmera.github.io/trivial-mimes/")
      (synopsis "Tiny Common Lisp library to detect mime types in files")
      (description
       "This is a teensy library that provides some functions to determine the
mime-type of a file.")
      (license license:artistic2.0))))

(define-public cl-trivial-mimes
  (sbcl-package->cl-source-package sbcl-trivial-mimes))

(define-public ecl-trivial-mimes
  (sbcl-package->ecl-package sbcl-trivial-mimes))

(define-public sbcl-lack-middleware-static
  (let ((commit "abff8efeb0c3a848e6bb0022f2b8b7fa3a1bc88b")
        (revision "1"))
    (package
     (name "sbcl-lack-middleware-static")
     (version (git-version "0.1.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fukamachi/lack.git")
             (commit commit)))
       (file-name (git-file-name "lack-middleware-static" version))
       (sha256
        (base32 "1avh4ygcj9xcx4m17nj0wnxxaisk26w4ljs2bibzxaln24x7pi85"))))
     (build-system asdf-build-system/sbcl)
     (arguments
      '(#:asd-file "lack-middleware-static.asd"
        #:asd-system-name "lack-middleware-static"
        #:test-asd-file "t-lack-middleware-static.asd"
        ;; XXX: Component :LACK not found
        #:tests? #f))
     (native-inputs
      `(("prove-asdf" ,sbcl-prove-asdf)))
     (inputs
      `(("sbcl-ironclad" ,sbcl-ironclad)
        ("sbcl-trivial-mimes" ,sbcl-trivial-mimes)
        ("sbcl-local-time" ,sbcl-local-time)))
     (home-page "https://github.com/fukamachi/lack")
     (synopsis "Lack, the core of Clack")
     (description
      "Lack is a Common Lisp library which allows web applications to be
constructed of modular components.  It was originally a part of Clack, however
it's going to be rewritten as an individual project since Clack v2 with
performance and simplicity in mind.")
     (license license:llgpl))))

(define-public cl-lack-middleware-static
  (sbcl-package->cl-source-package sbcl-lack-middleware-static))

(define-public sbcl-lack
  (let ((commit "abff8efeb0c3a848e6bb0022f2b8b7fa3a1bc88b")
        (revision "1"))
    (package
     (name "sbcl-lack")
     (version (git-version "0.1.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fukamachi/lack.git")
             (commit commit)))
       (file-name (git-file-name "lack" version))
       (sha256
        (base32 "1avh4ygcj9xcx4m17nj0wnxxaisk26w4ljs2bibzxaln24x7pi85"))))
     (build-system asdf-build-system/sbcl)
     (arguments
      '(#:test-asd-file "t-lack.asd"
        ;; XXX: Component :CLACK not found
        #:tests? #f))
     (native-inputs
      `(("prove-asdf" ,sbcl-prove-asdf)))
     (inputs
      `(("sbcl-lack-component" ,sbcl-lack-component)
        ("sbcl-lack-util" ,sbcl-lack-util)))
     (home-page "https://github.com/fukamachi/lack")
     (synopsis "Lack, the core of Clack")
     (description
      "Lack is a Common Lisp library which allows web applications to be
constructed of modular components.  It was originally a part of Clack, however
it's going to be rewritten as an individual project since Clack v2 with
performance and simplicity in mind.")
     (license license:llgpl))))

(define-public cl-lack
  (sbcl-package->cl-source-package sbcl-lack))

(define-public sbcl-ningle
  (let ((commit "50bd4f09b5a03a7249bd4d78265d6451563b25ad")
        (revision "1"))
    (package
      (name "sbcl-ningle")
      (version (git-version "0.3.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/fukamachi/ningle.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1bsl8cnxhacb8p92z9n89vhk1ikmij5zavk0m2zvmj7iqm79jzgw"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       ;; TODO: pull in clack-test
       '(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (delete 'cleanup-files)
           (delete 'cleanup)
           (add-before 'cleanup 'combine-fasls
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (lib (string-append out "/lib/sbcl"))
                      (ningle-path (string-append lib "/ningle"))
                      (fasl-files (find-files out "\\.fasl$")))
                 (mkdir-p ningle-path)
                 (let ((fasl-path (lambda (name)
                                    (string-append ningle-path
                                                   "/"
                                                   (basename name)
                                                   "--system.fasl"))))
                   (for-each (lambda (file)
                               (rename-file file
                                            (fasl-path
                                             (basename file ".fasl"))))
                             fasl-files))
                 fasl-files)
               #t)))))
      (native-inputs
       `(("sbcl-prove-asdf" ,sbcl-prove-asdf)
         ("sbcl-prove" ,sbcl-prove)))
      (inputs
       `(("sbcl-cl-syntax" ,sbcl-cl-syntax)
         ("sbcl-cl-syntax-annot" ,sbcl-cl-syntax-annot)
         ("sbcl-myway" ,sbcl-myway)
         ("sbcl-lack-request" ,sbcl-lack-request)
         ("sbcl-lack-response" ,sbcl-lack-response)
         ("sbcl-lack-component" ,sbcl-lack-component)
         ("sbcl-alexandria" ,sbcl-alexandria)
         ("sbcl-babel" ,sbcl-babel)))
      (home-page "https://8arrow.org/ningle/")
      (synopsis "Super micro framework for Common Lisp")
      (description
       "Ningle is a lightweight web application framework for Common Lisp.")
      (license license:llgpl))))

(define-public cl-ningle
  (sbcl-package->cl-source-package sbcl-ningle))

(define-public sbcl-clack
  (let ((commit "e3e032843bb1220ab96263c411aa7f2feb4746e0")
        (revision "1"))
    (package
     (name "sbcl-clack")
     (version (git-version "2.0.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fukamachi/clack.git")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ymzs6qyrwhlj6cgqsnpyn6g5cbp7a3s1vgxwna20y2q7y4iacy0"))))
     (build-system asdf-build-system/sbcl)
     (inputs
      `(("sbcl-lack" ,sbcl-lack)
        ("sbcl-lack-middleware-backtrace" ,sbcl-lack-middleware-backtrace)
        ("sbcl-bordeaux-threads" ,sbcl-bordeaux-threads)))
     (home-page "https://github.com/fukamachi/clack")
     (synopsis "Web Application Environment for Common Lisp")
     (description
      "Clack is a web application environment for Common Lisp inspired by
Python's WSGI and Ruby's Rack.")
     (license license:llgpl))))

(define-public cl-clack
  (sbcl-package->cl-source-package sbcl-clack))

(define-public sbcl-log4cl
  (let ((commit "611e094458504b938d49de904eab141285328c7c")
        (revision "1"))
    (package
      (name "sbcl-log4cl")
      (build-system asdf-build-system/sbcl)
      (version "1.1.2")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sharplispers/log4cl")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "08jly0s0g26b56hhpfizxsb4j0yvbh946sd205gr42dkzv8l7dsc"))))
      ;; FIXME: tests require stefil, sbcl-hu.dwim.stefil wont work
      (arguments
       `(#:tests? #f))
      (inputs `(("bordeaux-threads" ,sbcl-bordeaux-threads)))
      (synopsis "Common Lisp logging framework, modeled after Log4J")
      (home-page "https://github.com/7max/log4cl")
      (description "This is a Common Lisp logging framework that can log at
various levels and mix text with expressions.")
      (license license:asl2.0))))

(define-public cl-log4cl
  (sbcl-package->cl-source-package sbcl-log4cl))

(define-public ecl-log4cl
  (sbcl-package->ecl-package sbcl-log4cl))

(define-public sbcl-find-port
  (let ((commit "00c96a25af93a0f8681d34ec548861f2d7485478")
        (revision "1"))
    (package
      (name "sbcl-find-port")
      (build-system asdf-build-system/sbcl)
      (version "0.1")
      (home-page "https://github.com/eudoxia0/find-port")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0d6dzbb45jh0rx90wgs6v020k2xa87mvzas3mvfzvivjvqqlpryq"))))
      (native-inputs
       `(("fiveam" ,sbcl-fiveam)))
      (inputs
       `(("sbcl-usocket" ,sbcl-usocket)))
      (synopsis "Find open ports programmatically in Common Lisp")
      (description "This is a small Common Lisp library that finds an open
port within a range.")
      (license license:expat))))

(define-public cl-find-port
  (sbcl-package->cl-source-package sbcl-find-port))

(define-public ecl-find-port
  (sbcl-package->ecl-package sbcl-find-port))

(define-public sbcl-clunit
  (let ((commit "6f6d72873f0e1207f037470105969384f8380628")
        (revision "1"))
    (package
      (name "sbcl-clunit")
      (version (git-version "0.2.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tgutu/clunit.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1idf2xnqzlhi8rbrqmzpmb3i1l6pbdzhhajkmhwbp6qjkmxa4h85"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "CLUnit is a Common Lisp unit testing framework")
      (description
       "CLUnit is a Common Lisp unit testing framework.  It is designed
to be easy to use so that you can quickly start testing.  CLUnit
provides a rich set of features aimed at improving your unit testing
experience.")
      (home-page "https://tgutu.github.io/clunit/")
      ;; MIT License
      (license license:expat))))

(define-public cl-clunit
  (sbcl-package->cl-source-package sbcl-clunit))

(define-public ecl-clunit
  (sbcl-package->ecl-package sbcl-clunit))

(define-public sbcl-py4cl
  (let ((commit "4c8a2b0814fd311f978964f825ce012290f60136")
        (revision "1"))
    (package
      (name "sbcl-py4cl")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/bendudson/py4cl.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "15mk7qdqjkj56gdnbyrdyz6r7m1h26ldvn6ch96pmvg5vmr1m45r"))
         (modules '((guix build utils)))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       `(("sbcl-clunit" ,sbcl-clunit)))
      (inputs
       `(("sbcl-trivial-garbage" ,sbcl-trivial-garbage)))
      (propagated-inputs
       ;; This package doesn't do anything without python available
       `(("python" ,python)
         ;; For multi-dimensional array support
         ("python-numpy" ,python-numpy)))
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'replace-*base-directory*-var
             (lambda* (#:key outputs #:allow-other-keys)
               ;; In the ASD, the author makes an attempt to
               ;; programatically determine the location of the
               ;; source-code so lisp can call into "py4cl.py". We can
               ;; hard-code this since we know where this file will
               ;; reside.
               (substitute* "src/callpython.lisp"
                 (("py4cl/config:\\*base-directory\\*")
                  (string-append
                   "\""
                   (assoc-ref outputs "out")
                   "/share/common-lisp/sbcl-source/py4cl/"
                   "\""))))))))
      (synopsis "Call python from Common Lisp")
      (description
       "Py4CL is a bridge between Common Lisp and Python, which enables Common
Lisp to interact with Python code.  It uses streams to communicate with a
separate python process, the approach taken by cl4py.  This is different to
the CFFI approach used by burgled-batteries, but has the same goal.")
      (home-page "https://github.com/bendudson/py4cl")
      ;; MIT License
      (license license:expat))))

(define-public cl-py4cl
  (sbcl-package->cl-source-package sbcl-py4cl))

(define-public ecl-py4cl
  (sbcl-package->ecl-package sbcl-py4cl))

(define-public sbcl-parse-declarations
  (let ((commit "549aebbfb9403a7fe948654126b9c814f443f4f2")
        (revision "1"))
    (package
      (name "sbcl-parse-declarations")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url (string-append
                     "https://gitlab.common-lisp.net/parse-declarations/"
                     "parse-declarations.git"))
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "03g5qks4c59nmxa48pbslxkfh77h8hn8566jddp6m9pl15dzzpxd"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:asd-file "parse-declarations-1.0.asd"
         #:asd-system-name "parse-declarations-1.0"))
      (home-page "https://common-lisp.net/project/parse-declarations/")
      (synopsis "Parse, filter, and build declarations")
      (description
       "Parse-Declarations is a Common Lisp library to help writing
macros which establish bindings.  To be semantically correct, such
macros must take user declarations into account, as these may affect
the bindings they establish.  Yet the ANSI standard of Common Lisp does
not provide any operators to work with declarations in a convenient,
high-level way.  This library provides such operators.")
      ;; MIT License
      (license license:expat))))

(define-public cl-parse-declarations
  (sbcl-package->cl-source-package sbcl-parse-declarations))

(define-public ecl-parse-declarations
  (sbcl-package->ecl-package sbcl-parse-declarations))

(define-public sbcl-cl-quickcheck
  (let ((commit "807b2792a30c883a2fbecea8e7db355b50ba662f")
        (revision "1"))
    (package
      (name "sbcl-cl-quickcheck")
      (version (git-version "0.0.4" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mcandre/cl-quickcheck.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "165lhypq5xkcys6hvzb3jq7ywnmqvzaflda29qk2cbs3ggas4767"))))
      (build-system asdf-build-system/sbcl)
      (synopsis
       "Common Lisp port of the QuickCheck unit test framework")
      (description
       "Common Lisp port of the QuickCheck unit test framework")
      (home-page "https://github.com/mcandre/cl-quickcheck")
      ;; MIT
      (license license:expat))))

(define-public cl-cl-quickcheck
  (sbcl-package->cl-source-package sbcl-cl-quickcheck))

(define-public ecl-cl-quickcheck
  (sbcl-package->ecl-package sbcl-cl-quickcheck))

(define-public sbcl-burgled-batteries3
  (let ((commit "9c0f6667e1a71ddf77e21793a0bea524710fef6e")
        (revision "1"))
    (package
      (name "sbcl-burgled-batteries3")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/snmsts/burgled-batteries3.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0b726kz2xxcg5l930gz035rsdvhxrzmp05iwfwympnb4z4ammicb"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       '(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'set-*cpython-include-dir*-var
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "grovel-include-dir.lisp"
                 (("\\(defparameter \\*cpython-include-dir\\* \\(detect-python\\)\\)")
                  (string-append
                   "(defparameter *cpython-include-dir* \""
                   (assoc-ref inputs "python")
                   "/include/python3.7m"
                   "\")")))
               (substitute* "ffi-interface.lisp"
                 (("\\*cpython-lib\\*")
                  (format #f "'(\"~a/lib/libpython3.so\")"
                          (assoc-ref inputs "python"))))
               #t)))))
      (native-inputs
       `(("python" ,python)
         ("sbcl-cl-fad" ,sbcl-cl-fad)
         ("sbcl-lift" ,sbcl-lift)
         ("sbcl-cl-quickcheck" ,sbcl-cl-quickcheck)))
      (inputs
       `(("sbcl-cffi" ,sbcl-cffi)
         ("sbcl-cffi-grovel" ,sbcl-cffi-grovel)
         ("sbcl-alexandria" , sbcl-alexandria)
         ("sbcl-parse-declarations-1.0" ,sbcl-parse-declarations)
         ("sbcl-trivial-garbage" ,sbcl-trivial-garbage)))
      (synopsis "Bridge between Python and Lisp (FFI bindings, etc.)")
      (description
       "This package provides a shim between Python3 (specifically, the
CPython implementation of Python) and Common Lisp.")
      (home-page "https://github.com/snmsts/burgled-batteries3")
      ;; MIT
      (license license:expat))))

(define-public cl-burgled-batteries3
  (sbcl-package->cl-source-package sbcl-burgled-batteries3))

(define-public ecl-burgled-batteries3
  (sbcl-package->ecl-package sbcl-burgled-batteries3))

(define-public sbcl-metabang-bind
  (let ((commit "c93b7f7e1c18c954c2283efd6a7fdab36746ab5e")
        (revision "1"))
    (package
      (name "sbcl-metabang-bind")
      (version (git-version "0.8.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gwkkwg/metabang-bind.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0hd0kr91795v77akpbcyqiss9p0p7ypa9dznrllincnmgvsxlmf0"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       `(("sbcl-lift" ,sbcl-lift)))
      (synopsis "Macro that generalizes @code{multiple-value-bind} etc.")
      (description
       "Bind extends the idea of of let and destructing to provide a uniform
syntax for all your accessor needs.  It combines @code{let},
@code{destructuring-bind}, @code{with-slots}, @code{with-accessors}, structure
editing, property or association-lists, and @code{multiple-value-bind} and a
whole lot more into a single form.")
      (home-page "https://common-lisp.net/project/metabang-bind/")
      ;; MIT License
      (license license:expat))))

(define-public cl-metabang-bind
  (sbcl-package->cl-source-package sbcl-metabang-bind))

(define-public ecl-metabang-bind
  (sbcl-package->ecl-package sbcl-metabang-bind))

(define-public sbcl-fare-utils
  (let ((commit "66e9c6f1499140bc00ccc22febf2aa528cbb5724")
        (revision "1"))
    (package
      (name "sbcl-fare-utils")
      (version (git-version "1.0.0.5" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url
            "https://gitlab.common-lisp.net/frideau/fare-utils.git")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "01wsr1aap3jdzhn4hrqjbhsjx6qci9dbd3gh4gayv1p49rbg8aqr"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:test-asd-file "test/fare-utils-test.asd"))
      (native-inputs
       `(("sbcl-hu.dwim.stefil" ,sbcl-hu.dwim.stefil)))
      (synopsis "Collection of utilities and data structures")
      (description
       "fare-utils is a small collection of utilities.  It contains a lot of
basic everyday functions and macros.")
      (home-page "https://gitlab.common-lisp.net/frideau/fare-utils")
      ;; MIT License
      (license license:expat))))

(define-public cl-fare-utils
  (sbcl-package->cl-source-package sbcl-fare-utils))

(define-public ecl-fare-utils
  (sbcl-package->ecl-package sbcl-fare-utils))

(define-public sbcl-trivial-utf-8
  (let ((commit "4d427cfbb1c452436a0efb71c3205c9da67f718f")
        (revision "1"))
    (package
      (name "sbcl-trivial-utf-8")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url (string-append "https://gitlab.common-lisp.net/"
                               "trivial-utf-8/trivial-utf-8.git"))
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1jz27gz8gvqdmvp3k9bxschs6d5b3qgk94qp2bj6nv1d0jc3m1l1"))))
      (arguments
       ;; Guix incorrectly assumes the "8" is part of the version
       ;; number and lobs it off.
       `(#:asd-file "trivial-utf-8.asd"
         #:asd-system-name "trivial-utf-8"))
      (build-system asdf-build-system/sbcl)
      (synopsis "UTF-8 input/output library")
      (description
       "The Babel library solves a similar problem while understanding more
encodings.  Trivial UTF-8 was written before Babel existed, but for new
projects you might be better off going with Babel.  The one plus that Trivial
UTF-8 has is that it doesn't depend on any other libraries.")
      (home-page "https://common-lisp.net/project/trivial-utf-8/")
      (license license:bsd-3))))

(define-public cl-trivial-utf-8
  (sbcl-package->cl-source-package sbcl-trivial-utf-8))

(define-public ecl-trivial-utf-8
  (sbcl-package->ecl-package sbcl-trivial-utf-8))

(define-public sbcl-idna
  (package
    (name "sbcl-idna")
    (build-system asdf-build-system/sbcl)
    (version "0.2.2")
    (home-page "https://github.com/antifuchs/idna")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "00nbr3mffxhlq14gg9d16pa6691s4qh35inyw76v906s77khm5a2"))))
    (inputs
     `(("split-sequence" ,sbcl-split-sequence)))
    (synopsis "IDNA string encoding and decoding routines for Common Lisp")
    (description "This Common Lisp library provides string encoding and
decoding routines for IDNA, the International Domain Names in Applications.")
    (license license:expat)))

(define-public cl-idna
  (sbcl-package->cl-source-package sbcl-idna))

(define-public ecl-idna
  (sbcl-package->ecl-package sbcl-idna))

(define-public sbcl-swap-bytes
  (package
    (name "sbcl-swap-bytes")
    (build-system asdf-build-system/sbcl)
    (version "1.2")
    (home-page "https://github.com/sionescu/swap-bytes")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1hw1v1lw26rifyznpnj1csphha9jgzwpiic16ni3pvs6hcsni9rz"))))
    (inputs
     `(("trivial-features" ,sbcl-trivial-features)))
    (native-inputs
     `(("fiveam" ,sbcl-fiveam)))
    (synopsis "Efficient endianness conversion for Common Lisp")
    (description "This Common Lisp library provides optimized byte-swapping
primitives.  The library can change endianness of unsigned integers of length
1/2/4/8.  Very useful in implementing various network protocols and file
formats.")
    (license license:expat)))

(define-public cl-swap-bytes
  (sbcl-package->cl-source-package sbcl-swap-bytes))

(define-public ecl-swap-bytes
  (sbcl-package->ecl-package sbcl-swap-bytes))

(define-public sbcl-iolib.asdf
  ;; Latest release is from June 2017.
  (let ((commit "81e20614c0d27f9605bf9766214e236fd31b99b4")
        (revision "1"))
    (package
      (name "sbcl-iolib.asdf")
      (build-system asdf-build-system/sbcl)
      (version "0.8.3")
      (home-page "https://github.com/sionescu/iolib")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1j81r0wm7nfbwl991f26s4npcy7kybzybd3m47rbxy31h0cfcmdm"))))
      (inputs
       `(("alexandria" ,sbcl-alexandria)))
      (arguments
       '(#:asd-file "iolib.asdf.asd"))
      (synopsis "ASDF component classes for IOLib, a Common Lisp I/O library")
      (description "IOlib is to be a better and more modern I/O library than
the standard Common Lisp library.  It contains a socket library, a DNS
resolver, an I/O multiplexer(which supports @code{select(2)}, @code{epoll(4)}
and @code{kqueue(2)}), a pathname library and file-system utilities.")
      (license license:expat))))

(define-public sbcl-iolib.conf
  (package
    (inherit sbcl-iolib.asdf)
    (name "sbcl-iolib.conf")
    (inputs
     `(("iolib.asdf" ,sbcl-iolib.asdf)))
    (arguments
     '(#:asd-file "iolib.conf.asd"))
    (synopsis "Compile-time configuration for IOLib, a Common Lisp I/O library")))

(define-public sbcl-iolib.common-lisp
  (package
    (inherit sbcl-iolib.asdf)
    (name "sbcl-iolib.common-lisp")
    (inputs
     `(("iolib.asdf" ,sbcl-iolib.asdf)
       ("iolib.conf" ,sbcl-iolib.conf)))
    (arguments
     '(#:asd-file "iolib.common-lisp.asd"))
    (synopsis "Slightly modified Common Lisp for IOLib, a Common Lisp I/O library")))

(define-public sbcl-iolib.base
  (package
    (inherit sbcl-iolib.asdf)
    (name "sbcl-iolib.base")
    (inputs
     `(("iolib.asdf" ,sbcl-iolib.asdf)
       ("iolib.conf" ,sbcl-iolib.conf)
       ("iolib.common-lisp" ,sbcl-iolib.common-lisp)
       ("split-sequence" ,sbcl-split-sequence)))
    (arguments
     '(#:asd-file "iolib.base.asd"))
    (synopsis "Base package for IOLib, a Common Lisp I/O library")))

(define-public sbcl-iolib.grovel
  (package
    (inherit sbcl-iolib.asdf)
    (name "sbcl-iolib.grovel")
    (inputs
     `(("iolib.asdf" ,sbcl-iolib.asdf)
       ("iolib.conf" ,sbcl-iolib.conf)
       ("iolib.base", sbcl-iolib.base)
       ("cffi", sbcl-cffi)))
    (arguments
     '(#:asd-file "iolib.grovel.asd"
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-header
           (lambda* (#:key outputs #:allow-other-keys)
             ;; This header is required by sbcl-iolib.
             (install-file "src/grovel/grovel-common.h"
                           (string-append (assoc-ref outputs "out")
                                          "/lib/sbcl"))
             #t)))))
    (synopsis "CFFI Groveller for IOLib, a Common Lisp I/O library")))

(define sbcl-iolib+syscalls
  (package
    (inherit sbcl-iolib.asdf)
    (name "sbcl-iolib+syscalls")
    (inputs
     `(("iolib.asdf" ,sbcl-iolib.asdf)
       ("iolib.conf" ,sbcl-iolib.conf)
       ("iolib.grovel" ,sbcl-iolib.grovel)
       ("iolib.base" ,sbcl-iolib.base)
       ("bordeaux-threads" ,sbcl-bordeaux-threads)
       ("idna" ,sbcl-idna)
       ("swap-bytes" ,sbcl-swap-bytes)
       ("libfixposix" ,libfixposix)
       ("cffi" ,sbcl-cffi)))
    (native-inputs
     `(("fiveam" ,sbcl-fiveam)))
    (arguments
     '(#:asd-file "iolib.asd"
       #:asd-system-name "iolib/syscalls"
       #:test-asd-file "iolib.tests.asd"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/syscalls/ffi-functions-unix.lisp"
               (("\\(:default \"libfixposix\"\\)")
                (string-append
                 "(:default \""
                 (assoc-ref inputs "libfixposix") "/lib/libfixposix\")")))
             ;; Socket tests need Internet access, disable them.
             (substitute* "iolib.tests.asd"
               (("\\(:file \"sockets\" :depends-on \\(\"pkgdcl\" \"defsuites\"\\)\\)")
                "")))))))
    (synopsis "Common Lisp I/O library")))

(define sbcl-iolib+multiplex
  (package
    (inherit sbcl-iolib+syscalls)
    (name "sbcl-iolib+multiplex")
    (inputs
     `(("iolib+syscalls" ,sbcl-iolib+syscalls)
       ,@(package-inputs sbcl-iolib+syscalls)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-iolib+syscalls)
       ((#:asd-system-name _) "iolib/multiplex")))))



(define sbcl-iolib+streams
  (package
    (inherit sbcl-iolib+syscalls)
    (name "sbcl-iolib+streams")
    (inputs
     `(("iolib+multiplex" ,sbcl-iolib+multiplex)
       ,@(package-inputs sbcl-iolib+syscalls)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-iolib+syscalls)
       ((#:asd-system-name _) "iolib/streams")))))

(define sbcl-iolib+sockets
  (package
    (inherit sbcl-iolib+syscalls)
    (name "sbcl-iolib+sockets")
    (inputs
     `(("iolib+syscalls" ,sbcl-iolib+syscalls)
       ("iolib+streams" ,sbcl-iolib+streams)
       ,@(package-inputs sbcl-iolib+syscalls)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-iolib+syscalls)
       ((#:asd-system-name _) "iolib/sockets")))))

(define-public sbcl-iolib
  (package
    (inherit sbcl-iolib+syscalls)
    (name "sbcl-iolib")
    (inputs
     `(("iolib+multiplex" ,sbcl-iolib+multiplex)
       ("iolib+streams" ,sbcl-iolib+streams)
       ("iolib+sockets" ,sbcl-iolib+sockets)
       ,@(package-inputs sbcl-iolib+syscalls)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-iolib+syscalls)
       ((#:asd-system-name _) "iolib")))))

(define-public cl-iolib
  (sbcl-package->cl-source-package sbcl-iolib))

(define-public sbcl-ieee-floats
  (let ((commit "566b51a005e81ff618554b9b2f0b795d3b29398d")
        (revision "1"))
    (package
      (name "sbcl-ieee-floats")
      (build-system asdf-build-system/sbcl)
      (version (git-version "20170924" revision commit))
      (home-page "https://github.com/marijnh/ieee-floats/")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1xyj49j9x3lc84cv3dhbf9ja34ywjk1c46dklx425fxw9mkwm83m"))))
      (native-inputs
       `(("fiveam" ,sbcl-fiveam)))
      (synopsis "IEEE 754 binary representation for floats in Common Lisp")
      (description "This is a Common Lisp library that allows to convert
floating point values to IEEE 754 binary representation.")
      (license license:bsd-3))))

(define-public cl-ieee-floats
  (sbcl-package->cl-source-package sbcl-ieee-floats))

(define sbcl-closure-common
  (let ((commit "e3c5f5f454b72b01b89115e581c3c52a7e201e5c")
        (revision "1"))
    (package
      (name "sbcl-closure-common")
      (build-system asdf-build-system/sbcl)
      (version (git-version "20101006" revision commit))
      (home-page "https://common-lisp.net/project/cxml/")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sharplispers/closure-common")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0k5r2qxn122pxi301ijir3nayi9sg4d7yiy276l36qmzwhp4mg5n"))))
      (inputs
       `(("trivial-gray-streams" ,sbcl-trivial-gray-streams)
         ("babel" ,sbcl-babel)))
      (synopsis "Support Common Lisp library for CXML")
      (description "Closure-common is an internal helper library.  The name
Closure is a reference to the web browser it was originally written for.")
      ;; TODO: License?
      (license #f))))

(define-public sbcl-cxml+xml
  (let ((commit "00b22bf4c4cf11c993d5866fae284f95ab18e6bf")
        (revision "1"))
    (package
      (name "sbcl-cxml+xml")
      (build-system asdf-build-system/sbcl)
      (version (git-version "0.0.0" revision commit))
      (home-page "https://common-lisp.net/project/cxml/")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sharplispers/cxml")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "13kif7rf3gqdycsk9zq0d7y0g9y81krkl0z87k0p2fkbjfgrph37"))))
      (inputs
       `(("closure-common" ,sbcl-closure-common)
         ("puri" ,sbcl-puri)
         ("trivial-gray-streams" ,sbcl-trivial-gray-streams)))
      (arguments
       `(#:asd-file "cxml.asd"
         #:asd-system-name "cxml/xml"))
      (synopsis "Common Lisp XML parser")
      (description "CXML implements a namespace-aware, validating XML 1.0
parser as well as the DOM Level 2 Core interfaces.  Two parser interfaces are
offered, one SAX-like, the other similar to StAX.")
      (license license:llgpl))))

(define sbcl-cxml+dom
  (package
    (inherit sbcl-cxml+xml)
    (name "sbcl-cxml+dom")
    (inputs
     `(("closure-common" ,sbcl-closure-common)
       ("puri" ,sbcl-puri)
       ("cxml+xml" ,sbcl-cxml+xml)))
    (arguments
     `(#:asd-file "cxml.asd"
       #:asd-system-name "cxml/dom"))))

(define sbcl-cxml+klacks
  (package
    (inherit sbcl-cxml+xml)
    (name "sbcl-cxml+klacks")
    (inputs
     `(("closure-common" ,sbcl-closure-common)
       ("puri" ,sbcl-puri)
       ("cxml+xml" ,sbcl-cxml+xml)))
    (arguments
     `(#:asd-file "cxml.asd"
       #:asd-system-name "cxml/klacks"))))

(define sbcl-cxml+test
  (package
    (inherit sbcl-cxml+xml)
    (name "sbcl-cxml+test")
    (inputs
     `(("closure-common" ,sbcl-closure-common)
       ("puri" ,sbcl-puri)
       ("cxml+xml" ,sbcl-cxml+xml)))
    (arguments
     `(#:asd-file "cxml.asd"
       #:asd-system-name "cxml/test"))))

(define-public sbcl-cxml
  (package
    (inherit sbcl-cxml+xml)
    (name "sbcl-cxml")
    (inputs
     `(("closure-common" ,sbcl-closure-common)
       ("puri" ,sbcl-puri)
       ("trivial-gray-streams" ,sbcl-trivial-gray-streams)
       ("cxml+dom" ,sbcl-cxml+dom)
       ("cxml+klacks" ,sbcl-cxml+klacks)
       ("cxml+test" ,sbcl-cxml+test)))
    (arguments
     `(#:asd-file "cxml.asd"
       #:asd-system-name "cxml"
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'install-dtd
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "catalog.dtd"
                           (string-append
                            (assoc-ref outputs "out")
                            "/lib/" (%lisp-type)))))
         (add-after 'create-asd 'remove-component
           ;; XXX: The original .asd has no components, but our build system
           ;; creates an entry nonetheless.  We need to remove it for the
           ;; generated .asd to load properly.  See trivia.trivial for a
           ;; similar problem.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (asd (string-append out "/lib/sbcl/cxml.asd")))
               (substitute* asd
                 (("  :components
")
                  ""))
               (substitute* asd
                 ((" *\\(\\(:compiled-file \"cxml--system\"\\)\\)")
                  ""))))))))))

(define-public cl-cxml
  (sbcl-package->cl-source-package sbcl-cxml))

(define-public sbcl-cl-reexport
  (let ((commit "312f3661bbe187b5f28536cd7ec2956e91366c3b")
        (revision "1"))
    (package
      (name "sbcl-cl-reexport")
      (build-system asdf-build-system/sbcl)
      (version (git-version "0.1" revision commit))
      (home-page "https://github.com/takagi/cl-reexport")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1cwpn1m3wrl0fl9plznn7p464db646gnfc8zkyk97dyxski2aq0x"))))
      (inputs
       `(("alexandria" ,sbcl-alexandria)))
      (arguments
       ;; TODO: Tests fail because cl-test-more is missing, but I can't find it online.
       `(#:tests? #f))
      (synopsis "HTTP cookie manager for Common Lisp")
      (description "cl-cookie is a Common Lisp library featuring parsing of
cookie headers, cookie creation, cookie jar creation and more.")
      (license license:llgpl))))

(define-public cl-reexport
  (sbcl-package->cl-source-package sbcl-cl-reexport))

(define-public sbcl-cl-cookie
  (let ((commit "cea55aed8b9ad25fafd13defbcb9fe8f41b29546")
        (revision "1"))
    (package
      (name "sbcl-cl-cookie")
      (build-system asdf-build-system/sbcl)
      (version (git-version "0.9.10" revision commit))
      (home-page "https://github.com/fukamachi/cl-cookie")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "090g7z75h98zvc1ldx0vh4jn4086dhjm2w30jcwkq553qmyxwl8h"))))
      (inputs
       `(("proc-parse" ,sbcl-proc-parse)
         ("alexandria" ,sbcl-alexandria)
         ("quri" ,sbcl-quri)
         ("cl-ppcre" ,sbcl-cl-ppcre)
         ("local-time" ,sbcl-local-time)))
      (native-inputs
       `(("prove-asdf" ,sbcl-prove-asdf)
         ("prove" ,sbcl-prove)))
      (arguments
       ;; TODO: Tests fail because cl-cookie depends on cl-cookie-test.
       `(#:tests? #f))
      (synopsis "HTTP cookie manager for Common Lisp")
      (description "cl-cookie is a Common Lisp library featuring parsing of
cookie headers, cookie creation, cookie jar creation and more.")
      (license license:bsd-2))))

(define-public cl-cookie
  (sbcl-package->cl-source-package sbcl-cl-cookie))

(define-public sbcl-dexador
  (let ((commit "a2714d126cc94bc7a9a6e1e3c08de455b3a66378")
        (revision "1"))
    (package
      (name "sbcl-dexador")
      (build-system asdf-build-system/sbcl)
      (version (git-version "0.9.10" revision commit))
      (home-page "https://github.com/fukamachi/dexador")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0nbqgn4v3l2z6m1k1bdxfnqpfrk84nxdmz7csz11zzcfs4flkv79"))))
      (inputs
       `(("trivial-gray-streams" ,sbcl-trivial-gray-streams)
         ("babel" ,sbcl-babel)
         ("usocket" ,sbcl-usocket)
         ("fast-http" ,sbcl-fast-http)
         ("quri" ,sbcl-quri)
         ("fast-io" ,sbcl-fast-io)
         ("chunga" ,sbcl-chunga)
         ("cl-ppcre" ,sbcl-cl-ppcre)
         ("cl-cookie" ,sbcl-cl-cookie)
         ("trivial-mimes" ,sbcl-trivial-mimes)
         ("chipz" ,sbcl-chipz)
         ("cl-base64" ,sbcl-cl-base64)
         ("cl-reexport" ,sbcl-cl-reexport)
         ("cl+ssl" ,sbcl-cl+ssl)
         ("bordeaux-threads" ,sbcl-bordeaux-threads)
         ("alexandria" ,sbcl-alexandria)))
      (native-inputs
       `(("prove" ,sbcl-prove)
         ("prove-asdf" ,sbcl-prove-asdf)
         ("lack-request" ,sbcl-lack-request)
         ("clack" ,sbcl-clack)
         ("babel" ,sbcl-babel)
         ("alexandria" ,sbcl-alexandria)
         ("cl-ppcre" ,sbcl-cl-ppcre)
         ("local-time" ,sbcl-local-time)))
      (arguments
       ;; TODO: Circular dependency: tests depend on clack-test which depends on dexador.
       `(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-permissions
             (lambda _ (make-file-writable "t/data/test.gz") #t)))))
      (synopsis "Yet another HTTP client for Common Lisp")
      (description "Dexador is yet another HTTP client for Common Lisp with
neat APIs and connection-pooling.  It is meant to supersede Drakma.")
      (license license:expat))))

(define-public cl-dexador
  (package
    (inherit (sbcl-package->cl-source-package sbcl-dexador))
    (arguments
     `(#:phases
       ;; asdf-build-system/source has its own phases and does not inherit
       ;; from asdf-build-system/sbcl phases.
       (modify-phases %standard-phases/source
         (add-after 'unpack 'fix-permissions
           (lambda _ (make-file-writable "t/data/test.gz") #t)))))))

(define-public ecl-dexador
  (sbcl-package->ecl-package sbcl-dexador))

(define-public sbcl-lisp-namespace
  (let ((commit "28107cafe34e4c1c67490fde60c7f92dc610b2e0")
        (revision "1"))
    (package
      (name "sbcl-lisp-namespace")
      (build-system asdf-build-system/sbcl)
      (version (git-version "0.1" revision commit))
      (home-page "https://github.com/guicho271828/lisp-namespace")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1jw2wykp06z2afb9nm1lgfzll5cjlj36pnknjx614057zkkxq4iy"))))
      (inputs
       `(("alexandria" ,sbcl-alexandria)))
      (native-inputs
       `(("fiveam" ,sbcl-fiveam)))
      (arguments
       `(#:test-asd-file "lisp-namespace.test.asd"
        ;; XXX: Component LISP-NAMESPACE-ASD::LISP-NAMESPACE.TEST not found
         #:tests? #f))
      (synopsis "LISP-N, or extensible namespaces in Common Lisp")
      (description "Common Lisp already has major 2 namespaces, function
namespace and value namespace (or variable namespace), but there are actually
more — e.g., class namespace.
This library offers macros to deal with symbols from any namespace.")
      (license license:llgpl))))

(define-public cl-lisp-namespace
  (sbcl-package->cl-source-package sbcl-lisp-namespace))

(define-public sbcl-trivial-cltl2
  (let ((commit "8eec8407df833e8f27df8a388bc10913f16d9e83")
        (revision "1"))
    (package
      (name "sbcl-trivial-cltl2")
      (build-system asdf-build-system/sbcl)
      (version (git-version "0.1.1" revision commit))
      (home-page "https://github.com/Zulu-Inuoe/trivial-cltl2")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1dyyxz17vqv8hlfwq287gl8xxbvcnq798ajb7p5jdjz91wqf4bgk"))))
      (synopsis "Simple CLtL2 compatibility layer for Common Lisp")
      (description "This library is a portable compatibility layer around
\"Common Lisp the Language, 2nd
Edition\" (@url{https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node102.html})
and it exports symbols from implementation-specific packages.")
      (license license:llgpl))))

(define-public cl-trivial-cltl2
  (sbcl-package->cl-source-package sbcl-trivial-cltl2))

(define-public sbcl-introspect-environment
  (let ((commit "fff42f8f8fd0d99db5ad6c5812e53de7d660020b")
        (revision "1"))
    (package
      (name "sbcl-introspect-environment")
      (build-system asdf-build-system/sbcl)
      (version (git-version "0.1" revision commit))
      (home-page "https://github.com/Bike/introspect-environment")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1i305n0wfmpac63ni4i3vixnnkl8daw5ncxy0k3dv92krgx6qzhp"))))
      (native-inputs
       `(("fiveam" ,sbcl-fiveam)))
      (synopsis "Common Lisp environment introspection portability layer")
      (description "This library is a small interface to portable but
nonstandard introspection of Common Lisp environments.  It is intended to
allow a bit more compile-time introspection of environments in Common Lisp.

Quite a bit of information is available at the time a macro or compiler-macro
runs; inlining info, type declarations, that sort of thing.  This information
is all standard - any Common Lisp program can @code{(declare (integer x))} and
such.

This info ought to be accessible through the standard @code{&environment}
parameters, but it is not.  Several implementations keep the information for
their own purposes but do not make it available to user programs, because
there is no standard mechanism to do so.

This library uses implementation-specific hooks to make information available
to users.  This is currently supported on SBCL, CCL, and CMUCL.  Other
implementations have implementations of the functions that do as much as they
can and/or provide reasonable defaults.")
      (license license:wtfpl2))))

(define-public cl-introspect-environment
  (sbcl-package->cl-source-package sbcl-introspect-environment))

(define-public sbcl-type-i
  (let ((commit "dea233f45f94064105ec09f0767de338f67dcbe2")
        (revision "1"))
    (package
      (name "sbcl-type-i")
      (build-system asdf-build-system/sbcl)
      (version (git-version "0.1" revision commit))
      (home-page "https://github.com/guicho271828/type-i")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "039g5pbrhh65s0bhr9314gmd2nwc2y5lp2377c5qrc2lxky89qs3"))))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("introspect-environment" ,sbcl-introspect-environment)
         ("trivia.trivial" ,sbcl-trivia.trivial)))
      (native-inputs
       `(("fiveam" ,sbcl-fiveam)))
      (arguments
       `(#:test-asd-file "type-i.test.asd"))
      (synopsis "Type inference utility on unary predicates for Common Lisp")
      (description "This library tries to provide a way to detect what kind of
type the given predicate is trying to check.  This is different from inferring
the return type of a function.")
      (license license:llgpl))))

(define-public cl-type-i
  (sbcl-package->cl-source-package sbcl-type-i))

(define-public sbcl-optima
  (let ((commit "373b245b928c1a5cce91a6cb5bfe5dd77eb36195")
        (revision "1"))
    (package
      (name "sbcl-optima")
      (build-system asdf-build-system/sbcl)
      (version (git-version "1.0" revision commit))
      (home-page "https://github.com/m2ym/optima")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1yw4ymq7ms89342kkvb3aqxgv0w38m9kd8ikdqxxzyybnkjhndal"))))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("closer-mop" ,sbcl-closer-mop)))
      (native-inputs
       `(("eos" ,sbcl-eos)))
      (arguments
       ;; XXX: Circular dependencies: tests depend on optima.ppcre which depends on optima.
       `(#:tests? #f
         #:test-asd-file "optima.test.asd"))
      (synopsis "Optimized pattern matching library for Common Lisp")
      (description "Optima is a fast pattern matching library which uses
optimizing techniques widely used in the functional programming world.")
      (license license:expat))))

(define-public cl-optima
  (sbcl-package->cl-source-package sbcl-optima))

(define-public sbcl-fare-quasiquote
  (let ((commit "640d39a0451094071b3e093c97667b3947f43639")
        (revision "1"))
    (package
      (name "sbcl-fare-quasiquote")
      (build-system asdf-build-system/sbcl)
      (version (git-version "1.0.1" revision commit))
      (home-page "https://gitlab.common-lisp.net/frideau/fare-quasiquote")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url (string-append "https://gitlab.common-lisp.net/frideau/"
                                   "fare-quasiquote.git"))
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1g6q11l50kgija9f55lzqpcwvaq0ljiw8v1j265hnyg6nahjwjvg"))))
      (inputs
       `(("fare-utils" ,sbcl-fare-utils)))
      (arguments
       ;; XXX: Circular dependencies: Tests depend on subsystems,
       ;; which depend on the main systems.
       `(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           ;; XXX: Require 1.0.0 version of fare-utils, and we package some
           ;; commits after 1.0.0.5, but ASDF fails to read the
           ;; "-REVISION-COMMIT" part generated by Guix.
           (add-after 'unpack 'patch-requirement
             (lambda _
               (substitute* "fare-quasiquote.asd"
                 (("\\(:version \"fare-utils\" \"1.0.0\"\\)")
                  "\"fare-utils\"")))))))
      (synopsis "Pattern-matching friendly implementation of quasiquote")
      (description "The main purpose of this n+2nd reimplementation of
quasiquote is enable matching of quasiquoted patterns, using Optima or
Trivia.")
      (license license:expat))))

(define-public cl-fare-quasiquote
  (sbcl-package->cl-source-package sbcl-fare-quasiquote))

(define-public sbcl-fare-quasiquote-optima
  (package
    (inherit sbcl-fare-quasiquote)
    (name "sbcl-fare-quasiquote-optima")
    (inputs
     `(("optima" ,sbcl-optima)
       ("fare-quasiquote" ,sbcl-fare-quasiquote)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-requirement
           (lambda _
             (substitute* "fare-quasiquote-optima.asd"
               (("\\(:version \"optima\" \"1\\.0\"\\)")
                "\"optima\""))
             #t)))))))

(define-public cl-fare-quasiquote-optima
  (sbcl-package->cl-source-package sbcl-fare-quasiquote-optima))

(define-public sbcl-fare-quasiquote-readtable
  (package
    (inherit sbcl-fare-quasiquote)
    (name "sbcl-fare-quasiquote-readtable")
    (inputs
     `(("fare-quasiquote" ,sbcl-fare-quasiquote)
       ("named-readtables" ,sbcl-named-readtables)))
    (description "The main purpose of this n+2nd reimplementation of
quasiquote is enable matching of quasiquoted patterns, using Optima or
Trivia.

This package uses fare-quasiquote with named-readtable.")))

(define-public cl-fare-quasiquote-readtable
  (sbcl-package->cl-source-package sbcl-fare-quasiquote-readtable))

;; TODO: Add support for component-less system in asdf-build-system/sbcl.
(define-public sbcl-fare-quasiquote-extras
  (package
    (inherit sbcl-fare-quasiquote)
    (name "sbcl-fare-quasiquote-extras")
    (build-system asdf-build-system/sbcl)
    (inputs
     `(("fare-quasiquote-optima" ,sbcl-fare-quasiquote-optima)
       ("fare-quasiquote-readtable" ,sbcl-fare-quasiquote-readtable)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib/" (%lisp-type))))
               (mkdir-p lib)
               (install-file "fare-quasiquote-extras.asd" lib)
               (make-file-writable
                (string-append lib "/fare-quasiquote-extras.asd"))
               #t)))
         (add-after 'create-asd-file 'fix-asd-file
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib/" (%lisp-type)))
                    (asd (string-append lib "/fare-quasiquote-extras.asd")))
               (substitute* asd
                 ((":class")
                  "")
                 (("asdf/bundle:prebuilt-system")
                  "")
                 ((":components")
                  "")
                 (("\\(\\(:compiled-file \"fare-quasiquote-extras--system\"\\)\\)")
                  "")))
             #t)))))
    (description "This library combines @code{fare-quasiquote-readtable} and
@code{fare-quasiquote-optima}.")))

(define-public cl-fare-quasiquote-extras
  (package
    (inherit cl-fare-quasiquote)
    (name "cl-fare-quasiquote-extras")
    (build-system asdf-build-system/source)
    (propagated-inputs
     `(("fare-quasiquote" ,cl-fare-quasiquote)
       ("fare-quasiquote-optima" ,cl-fare-quasiquote-optima)
       ("fare-quasiquote-readtable" ,cl-fare-quasiquote-readtable)))
    (description "This library combines @code{fare-quasiquote-readtable} and
@code{fare-quasiquote-optima}.")))

(define-public sbcl-trivia.level0
  (let ((commit "574901ac376ae54c9d37169fd5efab9b3b8fcc63")
        (revision "1"))
    (package
      (name "sbcl-trivia.level0")
      (build-system asdf-build-system/sbcl)
      (version (git-version "0.0.0" revision commit))
      (home-page "https://github.com/guicho271828/trivia")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0hf53n71xzr9sjwrxis5fivx3drclf4s39h5ck8vh21ndqj9zvdk"))))
      (inputs
       `(("alexandria" ,sbcl-alexandria)))
      (synopsis "Pattern matching in Common Lisp")
      (description "Trivia is a pattern matching compiler that is compatible
with Optima, another pattern matching library for Common Lisp.  It is meant to
be faster and more extensible than Optima.")
      (license license:llgpl))))

(define-public sbcl-trivia.level1
  (package
    (inherit sbcl-trivia.level0)
    (name "sbcl-trivia.level1")
    (inputs
     `(("trivia.level0" ,sbcl-trivia.level0)))
    (description "Trivia is a pattern matching compiler that is compatible
with Optima, another pattern matching library for Common Lisp.  It is meant to
be faster and more extensible than Optima.

This system contains the core patterns of Trivia.")))

(define-public sbcl-trivia.level2
  (package
    (inherit sbcl-trivia.level0)
    (name "sbcl-trivia.level2")
    (inputs
     `(("trivia.level1" ,sbcl-trivia.level1)
       ("lisp-namespace" ,sbcl-lisp-namespace)
       ("trivial-cltl2" ,sbcl-trivial-cltl2)
       ("closer-mop" ,sbcl-closer-mop)))
    (description "Trivia is a pattern matching compiler that is compatible
with Optima, another pattern matching library for Common Lisp.  It is meant to
be faster and more extensible than Optima.

This system contains a non-optimized pattern matcher compatible with Optima,
with extensible optimizer interface.")))

(define-public sbcl-trivia.trivial
  (package
    (inherit sbcl-trivia.level0)
    (name "sbcl-trivia.trivial")
    (inputs
     `(("trivia.level2" ,sbcl-trivia.level2)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'create-asd-file
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib/" (%lisp-type)))
                    (level2 (assoc-ref inputs "trivia.level2")))
               (mkdir-p lib)
               (install-file "trivia.trivial.asd" lib)
               ;; XXX: This .asd does not have any component and the build
               ;; system fails to work in this case.  We should update the
               ;; build system to handle component-less .asd.
               ;; TODO: How do we append to file in Guile?  It seems that
               ;; (open-file ... "a") gets a "Permission denied".
               (substitute* (string-append lib "/trivia.trivial.asd")
                 (("\"\\)")
                  (string-append "\")

(progn (asdf/source-registry:ensure-source-registry)
       (setf (gethash
               \"trivia.level2\"
               asdf/source-registry:*source-registry*)
             #p\""
                                 level2
                                 "/share/common-lisp/sbcl-bundle-systems/trivia.level2.asd\"))")))))))))
    (description "Trivia is a pattern matching compiler that is compatible
with Optima, another pattern matching library for Common Lisp.  It is meant to
be faster and more extensible than Optima.

This system contains the base level system of Trivia with a trivial optimizer.")))

(define-public sbcl-trivia.balland2006
  (package
    (inherit sbcl-trivia.level0)
    (name "sbcl-trivia.balland2006")
    (inputs
     `(("trivia.trivial" ,sbcl-trivia.trivial)
       ("iterate" ,sbcl-iterate)
       ("type-i" ,sbcl-type-i)
       ("alexandria" ,sbcl-alexandria)))
    (arguments
     ;; Tests are done in trivia itself.
     `(#:tests? #f))
    (description "Trivia is a pattern matching compiler that is compatible
with Optima, another pattern matching library for Common Lisp.  It is meant to
be faster and more extensible than Optima.

This system contains the base level system of Trivia with a trivial optimizer.")))

(define-public sbcl-trivia.ppcre
  (package
    (inherit sbcl-trivia.level0)
    (name "sbcl-trivia.ppcre")
    (inputs
     `(("trivia.trivial" ,sbcl-trivia.trivial)
       ("cl-ppcre" ,sbcl-cl-ppcre)))
    (description "Trivia is a pattern matching compiler that is compatible
with Optima, another pattern matching library for Common Lisp.  It is meant to
be faster and more extensible than Optima.

This system contains the PPCRE extension.")))

(define-public sbcl-trivia.quasiquote
  (package
    (inherit sbcl-trivia.level0)
    (name "sbcl-trivia.quasiquote")
    (inputs
     `(("trivia.trivial" ,sbcl-trivia.trivial)
       ("fare-quasiquote" ,sbcl-fare-quasiquote)
       ("fare-quasiquote-readtable" ,sbcl-fare-quasiquote-readtable)))
    (description "Trivia is a pattern matching compiler that is compatible
with Optima, another pattern matching library for Common Lisp.  It is meant to
be faster and more extensible than Optima.

This system contains the fare-quasiquote extension.")))

(define-public sbcl-trivia.cffi
  (package
    (inherit sbcl-trivia.level0)
    (name "sbcl-trivia.cffi")
    (inputs
     `(("cffi" ,sbcl-cffi)
       ("trivia.trivial" ,sbcl-trivia.trivial)))
    (description "Trivia is a pattern matching compiler that is compatible
with Optima, another pattern matching library for Common Lisp.  It is meant to
be faster and more extensible than Optima.

This system contains the CFFI foreign slot access extension.")))

(define-public sbcl-trivia
  (package
    (inherit sbcl-trivia.level0)
    (name "sbcl-trivia")
    (inputs
     `(("trivia.balland2006" ,sbcl-trivia.balland2006)))
    (native-inputs
     `(("fiveam" ,sbcl-fiveam)
       ("trivia.ppcre" ,sbcl-trivia.ppcre)
       ("trivia.quasiquote" ,sbcl-trivia.quasiquote)
       ("trivia.cffi" ,sbcl-trivia.cffi)
       ("optima" ,sbcl-optima)))
    (arguments
     `(#:test-asd-file "trivia.test.asd"
       #:phases
       (modify-phases %standard-phases
         (add-after 'create-asd 'remove-component
           ;; XXX: The original .asd has no components, but our build system
           ;; creates an entry nonetheless.  We need to remove it for the
           ;; generated .asd to load properly.  See trivia.trivial for a
           ;; similar problem.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (asd (string-append out "/lib/" (%lisp-type) "/trivia.asd")))
               (substitute* asd
                 (("  :components
")
                  ""))
               (substitute* asd
                 ((" *\\(\\(:compiled-file \"trivia--system\"\\)\\)")
                  ""))))))))
    (description "Trivia is a pattern matching compiler that is compatible
with Optima, another pattern matching library for Common Lisp.  It is meant to
be faster and more extensible than Optima.")))

(define-public cl-trivia
  (sbcl-package->cl-source-package sbcl-trivia))

(define-public sbcl-mk-string-metrics
  (package
    (name "sbcl-mk-string-metrics")
    (version "0.1.2")
    (home-page "https://github.com/cbaggers/mk-string-metrics/")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit version)))
              (sha256
               (base32 "0bg0bv2mfd4k0g3x72x563hvmrx18xavaffr6xk5rh4if5j7kcf6"))
              (file-name (git-file-name name version))))
    (build-system asdf-build-system/sbcl)
    (synopsis "Calculate various string metrics efficiently in Common Lisp")
    (description "This library implements efficient algorithms that calculate
various string metrics in Common Lisp:

@itemize
@item Damerau-Levenshtein distance
@item Hamming distance
@item Jaccard similarity coefficient
@item Jaro distance
@item Jaro-Winkler distance
@item Levenshtein distance
@item Normalized Damerau-Levenshtein distance
@item Normalized Levenshtein distance
@item Overlap coefficient
@end itemize\n")
    (license license:x11)))

(define-public cl-mk-string-metrics
  (sbcl-package->cl-source-package sbcl-mk-string-metrics))

(define-public sbcl-cl-str
  (let ((commit "3d5ec86e3a0199e5973aacde951086dfd754b5e5"))
    (package
      (name "sbcl-cl-str")
      (version (git-version "0.8" "1" commit))
      (home-page "https://github.com/vindarel/cl-str")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (sha256
                 (base32 "0szzzbygw9h985yxz909vvqrp69pmpcpahn7hn350lnyjislk9ga"))
                (file-name (git-file-name name version))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("cl-ppcre" ,sbcl-cl-ppcre)
         ("cl-ppcre-unicode" ,sbcl-cl-ppcre-unicode)))
      (native-inputs
       `(("prove" ,sbcl-prove)
         ("prove-asdf" ,sbcl-prove-asdf)))
      (arguments
       `(#:asd-file "str.asd"
         #:asd-system-name "str"
         #:test-asd-file "str.test.asd"))
      (synopsis "Modern, consistent and terse Common Lisp string manipulation library")
      (description "A modern and consistent Common Lisp string manipulation
library that focuses on modernity, simplicity and discoverability:
@code{(str:trim s)} instead of @code{(string-trim '(#\\Space ...) s)}), or
@code{str:concat strings} instead of an unusual format construct; one
discoverable library instead of many; consistency and composability, where
@code{s} is always the last argument, which makes it easier to feed pipes and
arrows.")
      (license license:expat))))

(define-public cl-str
  (sbcl-package->cl-source-package sbcl-cl-str))

(define-public sbcl-cl-xmlspam
  (let ((commit "ea06abcca2a73a9779bcfb09081e56665f94e22a"))
    (package
      (name "sbcl-cl-xmlspam")
      (build-system asdf-build-system/sbcl)
      (version (git-version "0.0.0" "1" commit))
      (home-page "https://github.com/rogpeppe/cl-xmlspam")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (string-append name "-" version))
         (sha256
          (base32
           "0w4rqvrgdgk3fwfq3kx4r7wwdr2bv3b6n3bdqwsiriw9psqzpz2s"))))
      (inputs
       `(("cxml" ,sbcl-cxml)
         ("cl-ppcre" ,sbcl-cl-ppcre)))
      (synopsis "Concise, regexp-like pattern matching on streaming XML for Common Lisp")
      (description "CXML does an excellent job at parsing XML elements, but what
do you do when you have a XML file that's larger than you want to fit in
memory, and you want to extract some information from it?  Writing code to deal
with SAX events, or even using Klacks, quickly becomes tedious.
@code{cl-xmlspam} (for XML Stream PAttern Matcher) is designed to make it easy
to write code that mirrors the structure of the XML that it's parsing.  It
also makes it easy to shift paradigms when necessary - the usual Lisp control
constructs can be used interchangeably with pattern matching, and the full
power of CXML is available when necessary.")
      (license license:bsd-3))))

;; TODO: dbus uses ASDF's package-inferred-system which is not supported by
;; asdf-build-system/sbcl as of 2019-08-02.  We should fix
;; asdf-build-system/sbcl.
(define-public cl-dbus
  (let ((commit "24b452df3a45ca5dc95015500f34baad175c981a")
        (revision "1"))
    (package
      (name "cl-dbus")
      (build-system asdf-build-system/source)
      (version (git-version "20190408" revision commit))
      (home-page "https://github.com/death/dbus")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0fw2q866yddbf23nk9pxphm9gsasx35vjyss82xzvndnjmzlqfl5"))))
      ;; Inputs must be propagated or else packages depending on this won't
      ;; have the necessary packages.
      (propagated-inputs
       `(("alexandria" ,sbcl-alexandria)
         ("trivial-garbage" ,sbcl-trivial-garbage)
         ("babel" ,sbcl-babel)
         ("iolib" ,sbcl-iolib)
         ("ieee-floats" ,sbcl-ieee-floats)
         ("flexi-streams" ,sbcl-flexi-streams)
         ("cl-xmlspam" ,sbcl-cl-xmlspam)
         ("ironclad" ,sbcl-ironclad)))
      (synopsis "D-Bus client library for Common Lisp")
      (description "This is a Common Lisp library that allows to publish D-Bus
objects as well as send and notify other objects connected to a bus.")
      (license license:bsd-2))))

(define-public sbcl-cl-hooks
  (let ((commit "5b638083f3b4f1221a52631d9c8a0a265565cac7")
        (revision "1"))
    (package
      (name "sbcl-cl-hooks")
      (build-system asdf-build-system/sbcl)
      (version (git-version "0.2.1" revision commit))
      (home-page "https://github.com/scymtym/architecture.hooks")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0bg3l0a28lw5gqqjp6p6b5nhwqk46sgkb7184w5qbfngw1hk8x9y"))))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("let-plus" ,sbcl-let-plus)
         ("trivial-garbage" ,sbcl-trivial-garbage)
         ("closer-mop" ,sbcl-closer-mop)))
      (native-inputs
       `(("fiveam" ,sbcl-fiveam)))
      (synopsis "Hooks extension point mechanism (as in Emacs) for Common Lisp")
      (description "A hook, in the present context, is a certain kind of
extension point in a program that allows interleaving the execution of
arbitrary code with the execution of a the program without introducing any
coupling between the two.  Hooks are used extensively in the extensible editor
Emacs.

In the Common LISP Object System (CLOS), a similar kind of extensibility is
possible using the flexible multi-method dispatch mechanism.  It may even seem
that the concept of hooks does not provide any benefits over the possibilities
of CLOS.  However, there are some differences:

@itemize

@item There can be only one method for each combination of specializers and
qualifiers.  As a result this kind of extension point cannot be used by
multiple extensions independently.
@item Removing code previously attached via a @code{:before}, @code{:after} or
@code{:around} method can be cumbersome.
@item There could be other or even multiple extension points besides @code{:before}
and @code{:after} in a single method.
@item Attaching codes to individual objects using eql specializers can be
cumbersome.
@item Introspection of code attached a particular extension point is
cumbersome since this requires enumerating and inspecting the methods of a
generic function.
@end itemize

This library tries to complement some of these weaknesses of method-based
extension-points via the concept of hooks.")
      (license license:llgpl))))

(define-public cl-hooks
  (sbcl-package->cl-source-package sbcl-cl-hooks))

(define-public ecl-cl-hooks
  (sbcl-package->ecl-package sbcl-cl-hooks))

(define-public sbcl-s-sysdeps
  (let ((commit "d28246b5dffef9e73a0e0e6cfbc4e878006fe34d")
        (revision "1"))
    (package
      (name "sbcl-s-sysdeps")
      (build-system asdf-build-system/sbcl)
      (version (git-version "1" revision commit))
      (home-page "https://github.com/svenvc/s-sysdeps")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "14b69b81yrxmjlvmm3lfxk04x5v7hqz4fql121334wh72czznfh9"))))
      (synopsis "Common Lisp abstraction layer over platform dependent functionality")
      (description "@code{s-sysdeps} is an abstraction layer over platform
dependent functionality.  This simple package is used as a building block in a
number of other open source projects.

@code{s-sysdeps} abstracts:

@itemize
@item managing processes,
@item implementing a standard TCP/IP server,
@item opening a client TCP/IP socket stream,
@item working with process locks.
@end itemize\n")
      (license license:llgpl))))

(define-public cl-s-sysdeps
  (sbcl-package->cl-source-package sbcl-s-sysdeps))

(define-public ecl-s-sysdeps
  (sbcl-package->ecl-package sbcl-s-sysdeps))

(define-public sbcl-cl-prevalence
  (let ((commit "c163c227ed85d430b82cb1e3502f72d4f88e3cfa")
        (revision "1"))
    (package
      (name "sbcl-cl-prevalence")
      (build-system asdf-build-system/sbcl)
      (version (git-version "5" revision commit))
      (home-page "https://github.com/40ants/cl-prevalence")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1i9zj1q2ahgwch56an21yzbgkynz0kab9fyxkq9mg8p3xrv38jjn"))))
      (inputs
       `(("s-sysdeps" ,sbcl-s-sysdeps)
         ("s-xml" ,sbcl-s-xml)))
      (synopsis "Implementation of object prevalence for Common Lisp")
      (description "This Common Lisp library implements object prevalence (see
@url{https://en.wikipedia.org/wiki/System_prevalence}).  It allows
for (de)serializing to and from s-exps as well as XML.  Serialization of arbitrary
classes and cyclic data structures are supported.")
      (license license:llgpl))))

(define-public cl-prevalence
  (sbcl-package->cl-source-package sbcl-cl-prevalence))

(define-public ecl-cl-prevalence
  (sbcl-package->ecl-package sbcl-cl-prevalence))

(define-public sbcl-series
  (let ((commit "da9061b336119d1e5214aff9117171d494d5a58a")
        (revision "1"))
    (package
      (name "sbcl-series")
      (version (git-version "2.2.11" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "git://git.code.sf.net/p/series/series")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "07hk2lhfx42zk018pxqvn4gs77vd4n4g8m4xxbqaxgca76mifwfw"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       ;; Disable the tests, they are apparently buggy and I didn't find
       ;; a simple way to make them run and pass.
       '(#:tests? #f))
      (synopsis "Series data structure for Common Lisp")
      (description
       "This Common Lisp library provides a series data structure much like
a sequence, with similar kinds of operations.  The difference is that in many
situations, operations on series may be composed functionally and yet execute
iteratively, without the need to construct intermediate series values
explicitly.  In this manner, series provide both the clarity of a functional
programming style and the efficiency of an iterative programming style.")
      (home-page "http://series.sourceforge.net/")
      (license license:expat))))

(define-public cl-series
  (sbcl-package->cl-source-package sbcl-series))

(define-public ecl-series
  (sbcl-package->ecl-package sbcl-series))

(define-public sbcl-periods
  (let ((commit "983d4a57325db3c8def942f163133cec5391ec28")
        (revision "1"))
    (package
      (name "sbcl-periods")
      (version (git-version "0.0.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jwiegley/periods.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0z30jr3lxz3cmi019fsl4lgcgwf0yqpn95v9zkkkwgymdrkd4lga"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("local-time" ,sbcl-local-time)))
      (synopsis "Common Lisp library for manipulating date/time objects")
      (description
       "Periods is a Common Lisp library providing a set of utilities for
manipulating times, distances between times, and both contiguous and
discontiguous ranges of time.")
      (home-page "https://github.com/jwiegley/periods")
      (license license:bsd-3))))

(define-public cl-periods
  (sbcl-package->cl-source-package sbcl-periods))

(define-public ecl-periods
  (sbcl-package->ecl-package sbcl-periods))

(define-public sbcl-periods-series
  (package
    (inherit sbcl-periods)
    (name "sbcl-periods-series")
    (inputs
     `(("periods" ,sbcl-periods)
       ("series" ,sbcl-series)))
    (arguments
     '(#:asd-file "periods-series.asd"
       #:asd-system-name "periods-series"))
    (description
     "Periods-series is an extension of the periods Common Lisp library
providing functions compatible with the series Common Lisp library.")))

(define-public cl-periods-series
  (sbcl-package->cl-source-package sbcl-periods-series))

(define-public ecl-periods-series
  (sbcl-package->ecl-package sbcl-periods-series))

(define-public sbcl-metatilities-base
  (let ((commit "6eaa9e3ff0939a93a92109dd0fcd218de85417d5")
        (revision "1"))
    (package
      (name "sbcl-metatilities-base")
      (version (git-version "0.6.6" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gwkkwg/metatilities-base.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0xpa86pdzlnf4v5g64j3ifaplx71sx2ha8b7vvakswi652679ma0"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       `(("lift" ,sbcl-lift)))
      (synopsis "Core of the metatilities Common Lisp library")
      (description
       "Metatilities-base is the core of the metatilities Common Lisp library
which implements a set of utilities.")
      (home-page "https://common-lisp.net/project/metatilities-base/")
      (license license:expat))))

(define-public cl-metatilities-base
  (sbcl-package->cl-source-package sbcl-metatilities-base))

(define-public ecl-metatilities-base
  (sbcl-package->ecl-package sbcl-metatilities-base))

(define-public sbcl-cl-containers
  (let ((commit "b2980bac9ac87ad32b63b722ce520fa26cb36ee6")
        (revision "2"))
    (package
      (name "sbcl-cl-containers")
      (version (git-version "0.12.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gwkkwg/cl-containers.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "19hmlax19hq0xjaqr8za90vwf06ymxw1m29sj053a309k3hm84gx"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       `(("lift" ,sbcl-lift)))
      (inputs
       `(("metatilities-base" ,sbcl-metatilities-base)))
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'relax-version-checks
             (lambda _
               (substitute* "cl-containers.asd"
                 (("\\(:version \"metatilities-base\" \"0\\.6\\.6\"\\)")
                  "\"metatilities-base\""))
               (substitute* "cl-containers-test.asd"
                 (("\\(:version \"lift\" \"1\\.7\\.0\"\\)")
                  "\"lift\""))
               #t)))))
      (synopsis "Container library for Common Lisp")
      (description
       "Common Lisp ships with a set of powerful built in data structures
including the venerable list, full featured arrays, and hash-tables.
CL-containers enhances and builds on these structures by adding containers
that are not available in native Lisp (for example: binary search trees,
red-black trees, sparse arrays and so on), and by providing a standard
interface so that they are simpler to use and so that changing design
decisions becomes significantly easier.")
      (home-page "https://common-lisp.net/project/cl-containers/")
      (license license:expat))))

(define-public cl-containers
  (sbcl-package->cl-source-package sbcl-cl-containers))

(define-public ecl-cl-containers
  (sbcl-package->ecl-package sbcl-cl-containers))

(define-public sbcl-xlunit
  (let ((commit "3805d34b1d8dc77f7e0ee527a2490194292dd0fc")
        (revision "1"))
    (package
      (name "sbcl-xlunit")
      (version (git-version "0.6.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "http://git.kpe.io/xlunit.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0argfmp9nghs4sihyj3f8ch9qfib2b7ll07v5m9ziajgzsfl5xw3"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-tests
             (lambda _
               (substitute* "xlunit.asd"
                 ((" :force t") ""))
               #t)))))
      (synopsis "Unit testing package for Common Lisp")
      (description
       "The XLUnit package is a toolkit for building test suites.  It is based
on the XPTest package by Craig Brozensky and the JUnit package by Kent Beck.")
      (home-page "http://quickdocs.org/xlunit/")
      (license license:bsd-3))))

(define-public cl-xlunit
  (sbcl-package->cl-source-package sbcl-xlunit))

(define-public ecl-xlunit
  (sbcl-package->ecl-package sbcl-xlunit))

(define-public sbcl-fprog
  (let ((commit "7016d1a98215f82605d1c158e7a16504ca1f4636")
        (revision "1"))
    (package
      (name "sbcl-fprog")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jwiegley/cambl.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "103mry04j2k9vznsxm7wcvccgxkil92cdrv52miwcmxl8daa4jiz"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Functional programming utilities for Common Lisp")
      (description
       "@code{fprog} is a Common Lisp library allowing iteration over
immutable lists sharing identical sublists.")
      (home-page "https://github.com/jwiegley/cambl")
      (license license:bsd-3))))

(define-public cl-fprog
  (sbcl-package->cl-source-package sbcl-fprog))

(define-public ecl-fprog
  (sbcl-package->ecl-package sbcl-fprog))

(define-public sbcl-cambl
  (let ((commit "7016d1a98215f82605d1c158e7a16504ca1f4636")
        (revision "1"))
    (package
      (inherit sbcl-fprog)
      (name "sbcl-cambl")
      (version (git-version "4.0.0" revision commit))
      (native-inputs
       `(("xlunit" ,sbcl-xlunit)))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("cl-containers" ,sbcl-cl-containers)
         ("local-time" ,sbcl-local-time)
         ("periods" ,sbcl-periods)
         ("fprog" ,sbcl-fprog)))
      (synopsis "Commoditized amounts and balances for Common Lisp")
      (description
       "CAMBL is a Common Lisp library providing a convenient facility for
working with commoditized values.  It does not allow compound units (and so is
not suited for scientific operations) but does work rather nicely for the
purpose of financial calculations."))))

(define-public cl-cambl
  (sbcl-package->cl-source-package sbcl-cambl))

(define-public ecl-cambl
  (sbcl-package->ecl-package sbcl-cambl))

(define-public sbcl-cl-ledger
  (let ((commit "08e0be41795e804cd36142e51756ad0b1caa377b")
        (revision "1"))
    (package
      (name "sbcl-cl-ledger")
      (version (git-version "4.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ledger/cl-ledger.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1via0qf6wjcyxnfbmfxjvms0ik9j8rqbifgpmnhrzvkhrq9pv8h1"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("cambl" ,sbcl-cambl)
         ("cl-ppcre" ,sbcl-cl-ppcre)
         ("local-time" ,sbcl-local-time)
         ("periods-series" ,sbcl-periods-series)))
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-system-definition
             (lambda _
               (substitute* "cl-ledger.asd"
                 (("  :build-operation program-op") "")
                 (("  :build-pathname \"cl-ledger\"") "")
                 (("  :entry-point \"ledger::main\"") ""))
               #t)))))
      (synopsis "Common Lisp port of the Ledger accounting system")
      (description
       "CL-Ledger is a Common Lisp port of the Ledger double-entry accounting
system.")
      (home-page "https://github.com/ledger/cl-ledger")
      (license license:bsd-3))))

(define-public cl-ledger
  (sbcl-package->cl-source-package sbcl-cl-ledger))

(define-public ecl-cl-ledger
  (sbcl-package->ecl-package sbcl-cl-ledger))

(define-public sbcl-bst
  (let ((commit "34f9c7e8e0a9f2c952fe310ab36cb630d5d9c15a")
        (revision "1"))
    (package
      (name "sbcl-bst")
      (version (git-version "1.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/glv2/bst.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1amxns7hvvh4arwbh8ciwfzplg127vh37dnbamv1m1kmmnmihfc8"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       `(("alexandria" ,sbcl-alexandria)
         ("fiveam" ,sbcl-fiveam)))
      (synopsis "Binary search tree for Common Lisp")
      (description
       "BST is a Common Lisp library for working with binary search trees that
can contain any kind of values.")
      (home-page "https://github.com/glv2/bst")
      (license license:gpl3))))

(define-public cl-bst
  (sbcl-package->cl-source-package sbcl-bst))

(define-public ecl-bst
  (sbcl-package->ecl-package sbcl-bst))

(define-public sbcl-cl-octet-streams
  (package
    (name "sbcl-cl-octet-streams")
    (version "1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/glv2/cl-octet-streams.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1d7mn6ydv0j2x4r7clpc9ijjwrnfpxmvhifv8n5j7jh7s744sf8d"))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     `(("fiveam" ,sbcl-fiveam)))
    (inputs
     `(("trivial-gray-streams" ,sbcl-trivial-gray-streams)))
    (synopsis "In-memory octet streams for Common Lisp")
    (description
     "CL-octet-streams is a library implementing in-memory octet
streams for Common Lisp.  It was inspired by the trivial-octet-streams and
cl-plumbing libraries.")
    (home-page "https://github.com/glv2/cl-octet-streams")
    (license license:gpl3+)))

(define-public cl-octet-streams
  (sbcl-package->cl-source-package sbcl-cl-octet-streams))

(define-public ecl-cl-octet-streams
  (sbcl-package->ecl-package sbcl-cl-octet-streams))

(define-public sbcl-lzlib
  (let ((commit "0de1db7129fef9a58a059d35a2fa2ecfc5b47b47")
        (revision "1"))
    (package
      (name "sbcl-lzlib")
      (version (git-version "1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/glv2/cl-lzlib.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "12ny7vj52fgnd8hb8fc8mry92vq4c1x72x2350191m4476j95clz"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       `(("fiveam" ,sbcl-fiveam)))
      (inputs
       `(("cffi" ,sbcl-cffi)
         ("cl-octet-streams" ,sbcl-cl-octet-streams)
         ("lzlib" ,lzlib)))
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "src/lzlib.lisp"
                 (("liblz\\.so")
                  (string-append (assoc-ref inputs "lzlib") "/lib/liblz.so")))
               #t)))))
      (synopsis "Common Lisp library for lzip (de)compression")
      (description
       "This Common Lisp library provides functions for lzip (LZMA)
compression/decompression using bindings to the lzlib C library.")
      (home-page "https://github.com/glv2/cl-lzlib")
      (license license:gpl3+))))

(define-public cl-lzlib
  (sbcl-package->cl-source-package sbcl-lzlib))

(define-public ecl-lzlib
  (sbcl-package->ecl-package sbcl-lzlib))

(define-public sbcl-chanl
  (let ((commit "56e90a126c78b39bb621a01585e8d3b985238e8c")
        (revision "1"))
    (package
      (name "sbcl-chanl")
      (version (git-version "0.4.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/zkat/chanl.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0b1cf6c12qx5cy1fw2z42jgh566rp3l8nv5qf0qqc569s7bgmrh4"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       `(("fiveam" ,sbcl-fiveam)))
      (inputs
       `(("bordeaux-threads" ,sbcl-bordeaux-threads)))
      (synopsis "Portable channel-based concurrency for Common Lisp")
      (description "Common Lisp library for channel-based concurrency.  In
a nutshell, you create various threads sequentially executing tasks you need
done, and use channel objects to communicate and synchronize the state of these
threads.")
      (home-page "https://github.com/zkat/chanl")
      (license (list license:expat license:bsd-3)))))

(define-public cl-chanl
  (sbcl-package->cl-source-package sbcl-chanl))

(define-public ecl-chanl
  (sbcl-package->ecl-package sbcl-chanl))

(define-public sbcl-cl-store
  (let ((commit "c787337a16ea8cf8a06227f35933a4ec774746b3")
        (revision "1"))
    (package
      (name "sbcl-cl-store")
      (version (git-version "0.8.11" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/skypher/cl-store.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "194srkg8nrym19c6i7zbnkzshc1qhqa82m53qnkirz9fw928bqxr"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       `(("rt" ,sbcl-rt)))
      (synopsis "Common Lisp library to serialize data")
      (description
       "CL-STORE is a portable serialization package which should give you the
ability to store all Common Lisp data types into streams.")
      (home-page "https://www.common-lisp.net/project/cl-store/")
      (license license:expat))))

(define-public cl-store
  (sbcl-package->cl-source-package sbcl-cl-store))

(define-public ecl-cl-store
  (sbcl-package->ecl-package sbcl-cl-store))

(define-public sbcl-cl-gobject-introspection
  (let ((commit "7b703e2384945ea0ac39d9b766de434a08d81560")
        (revision "0"))
    (package
      (name "sbcl-cl-gobject-introspection")
      (version (git-version "0.3" revision commit))
      (home-page "https://github.com/andy128k/cl-gobject-introspection")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1zcqd2qj14f6b38vys8gr89s6cijsp9r8j43xa8lynilwva7bwyh"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("cffi" ,sbcl-cffi)
         ("iterate" ,sbcl-iterate)
         ("trivial-garbage" ,sbcl-trivial-garbage)
         ("glib" ,glib)
         ("gobject-introspection" ,gobject-introspection)))
      (native-inputs
       `(("fiveam" ,sbcl-fiveam)))
      (arguments
       ;; TODO: Tests fail, see
       ;; https://github.com/andy128k/cl-gobject-introspection/issues/70.
       '(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (add-after (quote unpack) (quote fix-paths)
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "src/init.lisp"
                 (("libgobject-2\\.0\\.so")
                  (string-append (assoc-ref inputs "glib") "/lib/libgobject-2.0.so"))
                 (("libgirepository-1\\.0\\.so")
                  (string-append (assoc-ref inputs "gobject-introspection")
                                 "/lib/libgirepository-1.0.so")))
               #t)))))
      (synopsis "Common Lisp bindings to GObject Introspection")
      (description
       "This library is a bridge between Common Lisp and GObject
Introspection, which enables Common Lisp programs to access the full interface
of C+GObject libraries without the need of writing dedicated bindings.")
      (license (list license:bsd-3
                     ;; Tests are under a different license.
                     license:llgpl)))))

(define-public cl-gobject-introspection
  (sbcl-package->cl-source-package sbcl-cl-gobject-introspection))

(define-public sbcl-string-case
  (let ((commit "718c761e33749e297cd2809c7ba3ade1985c49f7")
        (revision "0"))
    (package
      (name "sbcl-string-case")
      (version (git-version "0.0.2" revision commit))
      (home-page "https://github.com/pkhuong/string-case")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1n5i3yh0h5s636rcnwn7jwqy3rjflikra04lymimhpcshhjsk0md"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Efficient string= case in Common Lisp")
      (description
       "@code{string-case} is a Common Lisp macro that generates specialised decision
trees to dispatch on string equality.")
      (license license:bsd-3))))

(define-public cl-string-case
  (sbcl-package->cl-source-package sbcl-string-case))

(define-public ecl-string-case
  (sbcl-package->ecl-package sbcl-string-case))

(define-public sbcl-global-vars
  (let ((commit "c749f32c9b606a1457daa47d59630708ac0c266e")
        (revision "0"))
    (package
      (name "sbcl-global-vars")
      (version (git-version "1.0.0" revision commit))
      (home-page "https://github.com/lmj/global-vars")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "06m3xc8l3pgsapl8fvsi9wf6y46zs75cp9zn7zh6dc65v4s5wz3d"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Efficient global variables in Common Lisp")
      (description
       "In Common Lisp, a special variable that is never dynamically bound
typically serves as a stand-in for a global variable.  The @code{global-vars}
library provides true global variables that are implemented by some compilers.
An attempt to rebind a global variable properly results in a compiler error.
That is, a global variable cannot be dynamically bound.

Global variables therefore allow us to communicate an intended usage that
differs from special variables.  Global variables are also more efficient than
special variables, especially in the presence of threads.")
      (license license:expat))))

(define-public cl-global-vars
  (sbcl-package->cl-source-package sbcl-global-vars))

(define-public ecl-global-vars
  (sbcl-package->ecl-package sbcl-global-vars))

(define-public sbcl-trivial-file-size
  (let ((commit "1c1d672a01a446ba0391dbb4ffc40be3b0476f23")
        (revision "0"))
    (package
      (name "sbcl-trivial-file-size")
      (version (git-version "0.0.0" revision commit))
      (home-page "https://github.com/ruricolist/trivial-file-size")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "17pp86c9zs4y7i1sh7q9gbfw9iqv6655k7fz8qbj9ly1ypgxp4qs"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       `(("fiveam" ,sbcl-fiveam)))
      (synopsis "Size of a file in bytes in Common Lisp")
      (description
       "The canonical way to determine the size of a file in bytes, using Common Lisp,
is to open the file with an element type of (unsigned-byte 8) and then
calculate the length of the stream.  This is less than ideal.  In most cases
it is better to get the size of the file from its metadata, using a system
call.

This library exports a single function, file-size-in-octets.  It returns the
size of a file in bytes, using system calls when possible.")
      (license license:expat))))

(define-public cl-trivial-file-size
  (sbcl-package->cl-source-package sbcl-trivial-file-size))

(define-public ecl-trivial-file-size
  (sbcl-package->ecl-package sbcl-trivial-file-size))

(define-public sbcl-trivial-macroexpand-all
  (let ((commit "933270ac7107477de1bc92c1fd641fe646a7a8a9")
        (revision "0"))
    (package
      (name "sbcl-trivial-macroexpand-all")
      (version (git-version "0.0.0" revision commit))
      (home-page "https://github.com/cbaggers/trivial-macroexpand-all")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "191hnn4b5j4i3crydmlzbm231kj0h7l8zj6mzj69r1npbzkas4bd"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       `(("fiveam" ,sbcl-fiveam)))
      (synopsis "Portable macroexpand-all for Common Lisp")
      (description
       "This library provides a macroexpand-all function that calls the
implementation specific equivalent.")
      (license license:unlicense))))

(define-public cl-trivial-macroexpand-all
  (sbcl-package->cl-source-package sbcl-trivial-macroexpand-all))

(define-public ecl-trivial-macroexpand-all
  (sbcl-package->ecl-package sbcl-trivial-macroexpand-all))

(define-public sbcl-serapeum
  (let ((commit "65837f8a0d65b36369ec8d000fff5c29a395b5fe")
        (revision "0"))
    (package
      (name "sbcl-serapeum")
      (version (git-version "0.0.0" revision commit))
      (home-page "https://github.com/ruricolist/serapeum")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0clwf81r2lvk1rbfvk91s9zmbkas9imf57ilqclw12mxaxlfsnbw"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("trivia" ,sbcl-trivia)
         ("trivia.quasiquote" ,sbcl-trivia.quasiquote)
         ("split-sequence" ,sbcl-split-sequence)
         ("string-case" ,sbcl-string-case)
         ("parse-number" ,sbcl-parse-number)
         ("trivial-garbage" ,sbcl-trivial-garbage)
         ("bordeaux-threads" ,sbcl-bordeaux-threads)
         ("named-readtables" ,sbcl-named-readtables)
         ("fare-quasiquote-extras" ,sbcl-fare-quasiquote-extras)
         ("parse-declarations-1.0" ,sbcl-parse-declarations)
         ("global-vars" ,sbcl-global-vars)
         ("trivial-file-size" ,sbcl-trivial-file-size)
         ("trivial-macroexpand-all" ,sbcl-trivial-macroexpand-all)))
      (native-inputs
       `(("fiveam" ,sbcl-fiveam)
         ("local-time" ,sbcl-local-time)))
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'disable-failing-tests
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "serapeum.asd"
                 ;; Guix does not have Quicklisp, and probably never will.
                 (("\\(:file \"quicklisp\"\\)") ""))
               #t)))))
      (synopsis "Common Lisp utility library beyond Alexandria")
      (description
       "Serapeum is a conservative library of Common Lisp utilities.  It is a
supplement, not a competitor, to Alexandria.")
      (license license:expat))))

(define-public cl-serapeum
  (sbcl-package->cl-source-package sbcl-serapeum))

(define-public sbcl-arrows
  (let ((commit "df7cf0067e0132d9697ac8b1a4f1b9c88d4f5382")
        (revision "0"))
    (package
      (name "sbcl-arrows")
      (version (git-version "0.2.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.com/Harleqin/arrows.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "042k9vkssrqx9nhp14wdzm942zgdxvp35mba0p2syz98i75im2yy"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       `(("hu.dwim.stefil" ,sbcl-hu.dwim.stefil)))
      (synopsis "Clojure-like arrow macros for Common Lisp")
      (description
       "This library implements the @code{->} and @code{->>} macros from
Clojure, as well as several expansions on the idea.")
      (home-page "https://gitlab.com/Harleqin/arrows")
      (license license:public-domain))))

(define-public cl-arrows
  (sbcl-package->cl-source-package sbcl-arrows))

(define-public ecl-arrows
  (sbcl-package->ecl-package sbcl-arrows))

(define-public sbcl-simple-parallel-tasks
  (let ((commit "db460f7a3f7bbfe2d3a2223ed21e162068d04dda")
        (revision "0"))
    (package
      (name "sbcl-simple-parallel-tasks")
      (version (git-version "1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/glv2/simple-parallel-tasks.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0amw3qk23wnlyrsgzszs6rs7y4zvxv8dr03rnqhc60mnm8ds4dd5"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       `(("fiveam" ,sbcl-fiveam)))
      (inputs
       `(("chanl" ,sbcl-chanl)))
      (synopsis "Common Lisp library to evaluate some forms in parallel")
      (description "This is a simple Common Lisp library to evaluate some
forms in parallel.")
      (home-page "https://github.com/glv2/simple-parallel-tasks")
      (license license:gpl3))))

(define-public cl-simple-parallel-tasks
  (sbcl-package->cl-source-package sbcl-simple-parallel-tasks))

(define-public ecl-simple-parallel-tasks
  (sbcl-package->ecl-package sbcl-simple-parallel-tasks))

(define-public sbcl-cl-heap
  (package
    (name "sbcl-cl-heap")
    (version "0.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://common-lisp.net/project/cl-heap/releases/"
                           "cl-heap_" version ".tar.gz"))
       (sha256
        (base32
         "163hb07p2nxz126rpq3cj5dyala24n0by5i5786n2qcr1w0bak4i"))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     `(("xlunit" ,sbcl-xlunit)))
    (arguments
     `(#:test-asd-file "cl-heap-tests.asd"))
    (synopsis "Heap and priority queue data structures for Common Lisp")
    (description
     "CL-HEAP provides various implementations of heap data structures (a
binary heap and a Fibonacci heap) as well as an efficient priority queue.")
    (home-page "https://common-lisp.net/project/cl-heap/")
    (license license:gpl3+)))

(define-public cl-heap
  (sbcl-package->cl-source-package sbcl-cl-heap))

(define-public ecl-cl-heap
  (sbcl-package->ecl-package sbcl-cl-heap))

(define-public sbcl-curry-compose-reader-macros
  (let ((commit "beaa92dedf392726c042184bfd6149fa8d9e6ac2")
        (revision "0"))
    (package
      (name "sbcl-curry-compose-reader-macros")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/eschulte/curry-compose-reader-macros.git")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0rv9bl8xrad5wfcg5zs1dazvnpmvqz6297lbn8bywsrcfnlf7h98"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("named-readtables" ,sbcl-named-readtables)))
      (synopsis "Reader macros for partial application and composition")
      (description
       "This Common Lisp library provides reader macros for concise expression
of function partial application and composition.")
      (home-page "https://eschulte.github.io/curry-compose-reader-macros/")
      (license license:public-domain))))

(define-public cl-curry-compose-reader-macros
  (sbcl-package->cl-source-package sbcl-curry-compose-reader-macros))

(define-public ecl-curry-compose-reader-macros
  (sbcl-package->ecl-package sbcl-curry-compose-reader-macros))

(define-public sbcl-yason
  (package
    (name "sbcl-yason")
    (version "0.7.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/phmarek/yason.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0479rbjgbj80jpk5bby18inlv1kfp771a82rlcq5psrz65qqa9bj"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("trivial-gray-streams" ,sbcl-trivial-gray-streams)))
    (synopsis "Common Lisp JSON parser/encoder")
    (description
     "YASON is a Common Lisp library for encoding and decoding data in the
JSON interchange format.")
    (home-page "https://github.com/phmarek/yason")
    (license license:bsd-3)))

(define-public cl-yason
  (sbcl-package->cl-source-package sbcl-yason))

(define-public ecl-yason
  (sbcl-package->ecl-package sbcl-yason))

(define-public sbcl-stefil
  (let ((commit "0398548ec95dceb50fc2c2c03e5fb0ce49b86c7a")
        (revision "0"))
    (package
      (name "sbcl-stefil")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.common-lisp.net/stefil/stefil.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0bqz64q2szzhf91zyqyssmvrz7da6442rs01808pf3wrdq28bclh"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("iterate" ,sbcl-iterate)
         ("metabang-bind" ,sbcl-metabang-bind)))
      (propagated-inputs
       ;; Swank doesn't have a pre-compiled package, therefore we must
       ;; propagate its sources.
       `(("swank" ,cl-slime-swank)))
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'drop-unnecessary-dependency
             (lambda _
               (substitute* "package.lisp"
                 ((":stefil-system") ""))
               #t)))))
      (home-page "https://common-lisp.net/project/stefil/index-old.shtml")
      (synopsis "Simple test framework")
      (description
       "Stefil is a simple test framework for Common Lisp, with a focus on
interactive development.")
      (license license:public-domain))))

(define-public cl-stefil
  (sbcl-package->cl-source-package sbcl-stefil))

(define-public sbcl-graph
  (let ((commit "78bf9ec930d8eae4f0861b5be76765fb1e45e24f")
        (revision "0"))
    (package
      (name "sbcl-graph")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/eschulte/graph.git")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1qpij4xh8bqwc2myahpilcbh916v7vg0acz2fij14d3y0jm02h0g"))
         (patches (search-patches "sbcl-graph-asdf-definitions.patch"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       `(("stefil" ,sbcl-stefil)))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("cl-heap" ,sbcl-cl-heap)
         ("curry-compose-reader-macros" ,sbcl-curry-compose-reader-macros)
         ("metabang-bind" ,sbcl-metabang-bind)
         ("named-readtables" ,sbcl-named-readtables)))
      (arguments
       '(#:test-asd-file "graph-test.asd"))
      (synopsis "Graph data structure and algorithms for Common Lisp")
      (description
       "The GRAPH Common Lisp library provides a data structures to represent
graphs, as well as some graph manipulation and analysis algorithms (shortest
path, maximum flow, minimum spanning tree, etc.).")
      (home-page "https://eschulte.github.io/graph/")
      (license license:gpl3+))))

(define-public cl-graph
  (sbcl-package->cl-source-package sbcl-graph))

(define-public sbcl-graph-dot
  (package
    (inherit sbcl-graph)
    (name "sbcl-graph-dot")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cl-ppcre" ,sbcl-cl-ppcre)
       ("curry-compose-reader-macros" ,sbcl-curry-compose-reader-macros)
       ("graph" ,sbcl-graph)
       ("metabang-bind" ,sbcl-metabang-bind)
       ("named-readtables" ,sbcl-named-readtables)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-graph)
       ((#:asd-file _ "") "graph-dot.asd")
       ((#:asd-system-name _ #f) "graph-dot")))
    (synopsis "Serialize graphs to and from DOT format")))

(define-public sbcl-graph-json
  (package
    (inherit sbcl-graph)
    (name "sbcl-graph-json")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("curry-compose-reader-macros" ,sbcl-curry-compose-reader-macros)
       ("graph" ,sbcl-graph)
       ("metabang-bind" ,sbcl-metabang-bind)
       ("named-readtables" ,sbcl-named-readtables)
       ("yason" ,sbcl-yason)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-graph)
       ((#:asd-file _ "") "graph-json.asd")
       ((#:asd-system-name _ #f) "graph-json")))
    (synopsis "Serialize graphs to and from JSON format")))

(define-public sbcl-trivial-indent
  (let ((commit "2d016941751647c6cc5bd471751c2cf68861c94a")
        (revision "0"))
    (package
      (name "sbcl-trivial-indent")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/Shinmera/trivial-indent")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1sj90nqz17w4jq0ixz00gb9g5g6d2s7l8r17zdby27gxxh51w266"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Simple Common Lisp library to allow indentation hints for SWANK")
      (description
       "This library allows you to define custom indentation hints for your
macros if the one recognised by SLIME automatically produces unwanted
results.")
      (home-page "https://shinmera.github.io/trivial-indent/")
      (license license:zlib))))

(define-public cl-trivial-indent
  (sbcl-package->cl-source-package sbcl-trivial-indent))

(define-public sbcl-documentation-utils
  (let ((commit "98630dd5f7e36ae057fa09da3523f42ccb5d1f55")
        (revision "0"))
    (package
      (name "sbcl-documentation-utils")
      (version (git-version "1.2.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/Shinmera/documentation-utils.git")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "098qhkqskmmrh4wix34mawf7p5c87yql28r51r75yjxj577k5idq"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("trivial-indent" ,sbcl-trivial-indent)))
      (synopsis "Few simple tools to document Common Lisp libraries")
      (description
       "This is a small library to help you with managing the Common Lisp
docstrings for your library.")
      (home-page "https://shinmera.github.io/documentation-utils/")
      (license license:zlib))))

(define-public cl-documentation-utils
  (sbcl-package->cl-source-package sbcl-documentation-utils))

(define-public ecl-documentation-utils
  (sbcl-package->ecl-package sbcl-documentation-utils))

(define-public sbcl-form-fiddle
  (let ((commit "e0c23599dbb8cff3e83e012f3d86d0764188ad18")
        (revision "0"))
    (package
      (name "sbcl-form-fiddle")
      (version (git-version "1.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/Shinmera/form-fiddle")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "041iznc9mpfyrl0sv5893ys9pbb2pvbn9g3clarqi7gsfj483jln"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("documentation-utils" ,sbcl-documentation-utils)))
      (synopsis "Utilities to destructure Common Lisp lambda forms")
      (description
       "Often times we need to destructure a form definition in a Common Lisp
macro.  This library provides a set of simple utilities to help with that.")
      (home-page "https://shinmera.github.io/form-fiddle/")
      (license license:zlib))))

(define-public cl-form-fiddle
  (sbcl-package->cl-source-package sbcl-form-fiddle))

(define-public sbcl-parachute
  (let ((commit "ca04dd8e43010a6dfffa26dbe1d62af86008d666")
        (revision "0"))
    (package
      (name "sbcl-parachute")
      (version (git-version "1.1.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/Shinmera/parachute")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1mvsm3r0r6a2bg75nw0q7n9vlby3ch45qjl7hnb5k1z2n5x5lh60"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("documentation-utils" ,sbcl-documentation-utils)
         ("form-fiddle" ,sbcl-form-fiddle)))
      (synopsis "Extensible and cross-compatible testing framework for Common Lisp")
      (description
       "Parachute is a simple-to-use and extensible testing framework.
In Parachute, things are organised as a bunch of named tests within a package.
Each test can contain a bunch of test forms that make up its body.")
      (home-page "https://shinmera.github.io/parachute/")
      (license license:zlib))))

(define-public cl-parachute
  (sbcl-package->cl-source-package sbcl-parachute))

(define-public sbcl-array-utils
  (let ((commit "f90eb9070d0b2205af51126a35033574725e5c56")
        (revision "0"))
    (package
      (name "sbcl-array-utils")
      (version (git-version "1.1.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/Shinmera/array-utils")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0zhwfbpr53vs1ii4sx75dz2k9yhh1xpwdqqpg8nmfndxkmhpbi3x"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       `(("parachute" ,sbcl-parachute)))
      (inputs
       `(("documentation-utils" ,sbcl-documentation-utils)))
      (synopsis "Tiny collection of array and vector utilities for Common Lisp")
      (description
       "A miniature toolkit that contains some useful shifting/popping/pushing
functions for arrays and vectors.  Originally from Plump.")
      (home-page "https://shinmera.github.io/array-utils/")
      (license license:zlib))))

(define-public cl-array-utils
  (sbcl-package->cl-source-package sbcl-array-utils))

(define-public sbcl-plump
  (let ((commit "16f1231bf706cfbc54d9e55a853ca945e4452a08")
        (revision "0"))
    (package
      (name "sbcl-plump")
      (version (git-version "2.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/Shinmera/plump")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0705k8pbip51v74rccgwscwph439f2pma9f915qf1h4bhjx999ip"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("array-utils" ,sbcl-array-utils)
         ("documentation-utils" ,sbcl-documentation-utils)))
      (synopsis "Lenient XML / XHTML / HTML parser for Common Lisp")
      (description
       "Plump is a parser for HTML/XML-like documents, focusing on being
lenient towards invalid markup.  It can handle things like invalid attributes,
bad closing tag order, unencoded entities, inexistent tag types, self-closing
tags and so on.  It parses documents to a class representation and offers a
small set of DOM functions to manipulate it.  It can be extended to parse to
your own classes.")
      (home-page "https://shinmera.github.io/plump/")
      (license license:zlib))))

(define-public cl-plump
  (sbcl-package->cl-source-package sbcl-plump))

(define-public sbcl-antik-base
  (let ((commit "e4711a69b3d6bf37b5727af05c3cfd03e8428ba3")
        (revision "1"))
    (package
      (name "sbcl-antik-base")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.common-lisp.net/antik/antik.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "047ga2c38par2xbgg4qx6hwv06qhf1c1f67as8xvir6s80lip1km"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("cl-ppcre" ,sbcl-cl-ppcre)
         ("iterate" ,sbcl-iterate)
         ("metabang-bind" ,sbcl-metabang-bind)
         ("named-readtables" ,sbcl-named-readtables)
         ("split-sequence" ,sbcl-split-sequence)))
      (native-inputs
       `(("lisp-unit" ,sbcl-lisp-unit)))
      (synopsis "Scientific and engineering computation in Common Lisp")
      (description
       "Antik provides a foundation for scientific and engineering
computation in Common Lisp.  It is designed not only to facilitate
numerical computations, but to permit the use of numerical computation
libraries and the interchange of data and procedures, whether
foreign (non-Lisp) or Lisp libraries.  It is named after the
Antikythera mechanism, one of the oldest examples of a scientific
computer known.")
      (home-page "https://common-lisp.net/project/antik/")
      (license license:gpl3))))

(define-public cl-antik-base
  (sbcl-package->cl-source-package sbcl-antik-base))

(define-public ecl-antik-base
  (sbcl-package->ecl-package sbcl-antik-base))

(define-public sbcl-foreign-array
  (package
    (inherit sbcl-antik-base)
    (name "sbcl-foreign-array")
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-antik-base)
       ((#:asd-file _ "") "foreign-array.asd")
       ((#:asd-system-name _ #f) "foreign-array")))
    (inputs
     `(("antik-base" ,sbcl-antik-base)
       ("cffi" ,sbcl-cffi)
       ("trivial-garbage" ,sbcl-trivial-garbage)
       ("static-vectors" ,sbcl-static-vectors)))
    (synopsis "Common Lisp library providing access to foreign arrays")))

(define-public cl-foreign-array
  (sbcl-package->cl-source-package sbcl-foreign-array))

(define-public ecl-foreign-array
  (sbcl-package->ecl-package sbcl-foreign-array))

(define-public sbcl-physical-dimension
  (package
    (inherit sbcl-antik-base)
    (name "sbcl-physical-dimension")
    (inputs
     `(("fare-utils" ,sbcl-fare-utils)
       ("foreign-array" ,sbcl-foreign-array)
       ("trivial-utf-8" ,sbcl-trivial-utf-8)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-antik-base)
       ((#:asd-file _ "") "physical-dimension.asd")
       ((#:asd-system-name _ #f) "physical-dimension")))
    (synopsis
     "Common Lisp library providing computations with physical units")))

(define-public cl-physical-dimension
  (sbcl-package->cl-source-package sbcl-physical-dimension))

(define-public sbcl-science-data
  (package
    (inherit sbcl-antik-base)
    (name "sbcl-science-data")
    (inputs
     `(("physical-dimension" ,sbcl-physical-dimension)
       ("drakma" ,sbcl-drakma)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-antik-base)
       ((#:asd-file _ "") "science-data.asd")
       ((#:asd-system-name _ #f) "science-data")))
    (synopsis
     "Common Lisp library for scientific and engineering numerical data")))

(define-public cl-science-data
  (sbcl-package->cl-source-package sbcl-science-data))

(define-public sbcl-gsll
  (let ((commit "1a8ada22f9cf5ed7372d352b2317f4ccdb6ab308")
        (revision "1"))
    (package
      (name "sbcl-gsll")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.common-lisp.net/antik/gsll.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0z5nypfk26hxihb08p085644afawicrgb4xvadh3lmrn46qbjfn4"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       `(("lisp-unit" ,sbcl-lisp-unit)))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("cffi-grovel" ,sbcl-cffi-grovel)
         ("cffi-libffi" ,sbcl-cffi-libffi)
         ("foreign-array" ,sbcl-foreign-array)
         ("gsl" ,gsl)
         ("metabang-bind" ,sbcl-metabang-bind)
         ("trivial-features" ,sbcl-trivial-features)
         ("trivial-garbage" ,sbcl-trivial-garbage)))
      (arguments
       `(#:tests? #f
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-cffi-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "gsll.asd"
                 ((":depends-on \\(#:foreign-array")
                  ":depends-on (#:foreign-array #:cffi-libffi"))
               (substitute* "init/init.lisp"
                 (("libgslcblas.so" all)
                  (string-append
                   (assoc-ref inputs "gsl") "/lib/" all)))
               (substitute* "init/init.lisp"
                 (("libgsl.so" all)
                  (string-append
                   (assoc-ref inputs "gsl") "/lib/" all))))))))
      (synopsis "GNU Scientific Library for Lisp")
      (description
       "The GNU Scientific Library for Lisp (GSLL) allows the use of the
GNU Scientific Library (GSL) from Common Lisp.  This library provides a
full range of common mathematical operations useful to scientific and
engineering applications.  The design of the GSLL interface is such
that access to most of the GSL library is possible in a Lisp-natural
way; the intent is that the user not be hampered by the restrictions
of the C language in which GSL has been written.  GSLL thus provides
interactive use of GSL for getting quick answers, even for someone not
intending to program in Lisp.")
      (home-page "https://common-lisp.net/project/gsll/")
      (license license:gpl3))))

(define-public cl-gsll
  (sbcl-package->cl-source-package sbcl-gsll))

(define-public sbcl-antik
  (package
    (inherit sbcl-antik-base)
    (name "sbcl-antik")
    (inputs
     `(("gsll" ,sbcl-gsll)
       ("physical-dimension" ,sbcl-physical-dimension)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-antik-base)
       ((#:asd-file _ "") "antik.asd")
       ((#:asd-system-name _ #f) "antik")))))

(define-public cl-antik
  (sbcl-package->cl-source-package sbcl-antik))

(define-public sbcl-cl-interpol
  (let ((commit "1fd288d861db85bc4677cff3cdd6af75fda1afb4")
        (revision "1"))
    (package
      (name "sbcl-cl-interpol")
      (version (git-version "0.2.6" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/edicl/cl-interpol.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1hnikak52hmcq1r5f616m6qq1108qnkw80pja950nv1fq5p0ppjn"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("cl-unicode" ,sbcl-cl-unicode)
         ("named-readtables" ,sbcl-named-readtables)))
      (native-inputs
       `(("flexi-streams" ,sbcl-flexi-streams)))
      (synopsis "String interpolation for Common Lisp")
      (description
       "CL-INTERPOL is a library for Common Lisp which modifies the
reader so that you can have interpolation within strings similar to
Perl or Unix Shell scripts.  It also provides various ways to insert
arbitrary characters into literal strings even if your editor/IDE
doesn't support them.")
      (home-page "https://edicl.github.io/cl-interpol/")
      (license license:bsd-3))))

(define-public cl-interpol
  (sbcl-package->cl-source-package sbcl-cl-interpol))

(define-public ecl-cl-interpol
  (sbcl-package->ecl-package sbcl-cl-interpol))

(define sbcl-symbol-munger-boot0
  ;; There is a cyclical dependency between symbol-munger and lisp-unit2.
  ;; See https://github.com/AccelerationNet/symbol-munger/issues/4
  (let ((commit "cc2bb4b7acd454d756484aec81ba487648385fc3")
        (revision "1"))
    (package
      (name "sbcl-symbol-munger-boot0")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/AccelerationNet/symbol-munger.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0diav5ricqsybqvbp4bkxyj3bn3v9n7xb2pqqc4vg1algsw2pyjl"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:asd-file "symbol-munger.asd"
         #:asd-system-name "symbol-munger"))
      (inputs
       `(("iterate" ,sbcl-iterate)
         ("alexandria" ,sbcl-alexandria)))
      (native-inputs
       `(("lisp-unit" ,sbcl-lisp-unit)))
      (synopsis
       "Capitalization and spacing conversion functions for Common Lisp")
      (description
       "This is a Common Lisp library to change the capitalization and spacing
of a string or a symbol.  It can convert to and from Lisp, english, underscore
and camel-case rules.")
      (home-page "https://github.com/AccelerationNet/symbol-munger")
      ;; The package declares a BSD license, but all of the license
      ;; text is MIT.
      ;; See https://github.com/AccelerationNet/symbol-munger/issues/5
      (license license:expat))))

(define sbcl-lisp-unit2-boot0
  ;; There is a cyclical dependency between symbol-munger and lisp-unit2.
  ;; See https://github.com/AccelerationNet/symbol-munger/issues/4
  (let ((commit "fb9721524d1e4e73abb223ee036d74ce14a5505c")
        (revision "1"))
    (package
      (name "sbcl-lisp-unit2-boot0")
      (version (git-version "0.2.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/AccelerationNet/lisp-unit2.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1rsqy8y0jqll6xn9a593848f5wvd5ribv4csry1ly0hmdhfnqzlp"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:asd-file "lisp-unit2.asd"
         #:asd-system-name "lisp-unit2"))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("cl-interpol" ,sbcl-cl-interpol)
         ("iterate" ,sbcl-iterate)
         ("symbol-munger-boot0" ,sbcl-symbol-munger-boot0)))
      (synopsis "Test Framework for Common Lisp")
      (description
       "LISP-UNIT2 is a Common Lisp library that supports unit testing in the
style of JUnit for Java.  It is a new version of the lisp-unit library written
by Chris Riesbeck.")
      (home-page "https://github.com/AccelerationNet/lisp-unit2")
      (license license:expat))))

(define-public sbcl-symbol-munger
  (let ((commit "97598d4c3c53fd5da72ab78908fbd5d8c7a13416")
        (revision "1"))
    (package
      (name "sbcl-symbol-munger")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/AccelerationNet/symbol-munger.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0y8jywjy0ldyhp7bxf16fdvdd2qgqnd7nlhlqfpfnzxcqk4xy1km"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("iterate" ,sbcl-iterate)))
      (native-inputs
       `(("lisp-unit2-boot0" ,sbcl-lisp-unit2-boot0)))
      (synopsis
       "Capitalization and spacing conversion functions for Common Lisp")
      (description
       "This is a Common Lisp library to change the capitalization and spacing
of a string or a symbol.  It can convert to and from Lisp, english, underscore
and camel-case rules.")
      (home-page "https://github.com/AccelerationNet/symbol-munger")
      ;; The package declares a BSD license, but all of the license
      ;; text is MIT.
      ;; See https://github.com/AccelerationNet/symbol-munger/issues/5
      (license license:expat))))

(define-public cl-symbol-munger
  (sbcl-package->cl-source-package sbcl-symbol-munger))

(define-public ecl-symbol-munger
  (sbcl-package->ecl-package sbcl-symbol-munger))

(define-public sbcl-lisp-unit2
  (package
    (inherit sbcl-lisp-unit2-boot0)
    (name "sbcl-lisp-unit2")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cl-interpol" ,sbcl-cl-interpol)
       ("iterate" ,sbcl-iterate)
       ("symbol-munger" ,sbcl-symbol-munger)))))

(define-public cl-lisp-unit2
  (sbcl-package->cl-source-package sbcl-lisp-unit2))

(define-public ecl-lisp-unit2
  (sbcl-package->ecl-package sbcl-lisp-unit2))

(define-public sbcl-cl-csv
  (let ((commit "3eba29c8364b033fbe0d189c2500559278b6a362")
        (revision "1"))
    (package
      (name "sbcl-cl-csv")
      (version (git-version "1.0.6" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/AccelerationNet/cl-csv.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "07h4ni89jzx93clx453hlnnb5g53hhlcmz5hghqv6ysam48lc8g6"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       ;; See: https://github.com/AccelerationNet/cl-csv/pull/34
       `(#:tests? #f))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("cl-interpol" ,sbcl-cl-interpol)
         ("iterate" ,sbcl-iterate)))
      (native-inputs
       `(("lisp-unit2" ,sbcl-lisp-unit2)))
      (synopsis "Common lisp library for comma-separated values")
      (description
       "This is a Common Lisp library providing functions to read/write CSV
from/to strings, streams and files.")
      (home-page "https://github.com/AccelerationNet/cl-csv")
      (license license:bsd-3))))

(define-public cl-csv
  (sbcl-package->cl-source-package sbcl-cl-csv))

(define-public ecl-cl-csv
  (sbcl-package->ecl-package sbcl-cl-csv))

(define-public sbcl-external-program
  (let ((commit "5888b8f1fd3953feeeacecbba4384ddda584a749")
        (revision "1"))
    (package
      (name "sbcl-external-program")
      (version (git-version "0.0.6" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sellout/external-program.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0vww1x3yilb3bjwg6k184vaj4vxyxw4vralhnlm6lk4xac67kc9z"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("trivial-features" ,sbcl-trivial-features)))
      (native-inputs
       `(("fiveam" ,sbcl-fiveam)))
      (synopsis "Common Lisp library for running external programs")
      (description
       "EXTERNAL-PROGRAM enables running programs outside the Lisp
process.  It is an attempt to make the RUN-PROGRAM functionality in
implementations like SBCL and CCL as portable as possible without
sacrificing much in the way of power.")
      (home-page "https://github.com/sellout/external-program")
      (license license:llgpl))))

(define-public cl-external-program
  (sbcl-package->cl-source-package sbcl-external-program))

(define-public ecl-external-program
  (sbcl-package->ecl-package sbcl-external-program))

(define sbcl-cl-ana-boot0
  (let ((commit "fa7cee4c50aa1c859652813049ba0da7c18a0df9")
        (revision "1"))
    (package
     (name "sbcl-cl-ana-boot0")
     (version (git-version "0.0.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ghollisjr/cl-ana.git")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0mr47l57m276dbpap7irr4fcnk5fgknhf6mgv4043s8h73amk5qh"))))
     (build-system asdf-build-system/sbcl)
     (synopsis "Common Lisp data analysis library")
     (description
      "CL-ANA is a data analysis library in Common Lisp providing tabular and
binned data analysis along with nonlinear least squares fitting and
visualization.")
     (home-page "https://github.com/ghollisjr/cl-ana")
     (license license:gpl3))))

(define-public sbcl-cl-ana.pathname-utils
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.pathname-utils")
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "pathname-utils/cl-ana.pathname-utils.asd")
       ((#:asd-system-name _ #f) "cl-ana.pathname-utils")))))

(define-public cl-ana.pathname-utils
  (sbcl-package->cl-source-package sbcl-cl-ana.pathname-utils))

(define-public ecl-cl-ana.pathname-utils
  (sbcl-package->ecl-package sbcl-cl-ana.pathname-utils))

(define-public sbcl-cl-ana.package-utils
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.package-utils")
    (inputs
     `(("alexandria" ,sbcl-alexandria)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "package-utils/cl-ana.package-utils.asd")
       ((#:asd-system-name _ #f) "cl-ana.package-utils")))))

(define-public cl-ana.package-utils
  (sbcl-package->cl-source-package sbcl-cl-ana.package-utils))

(define-public ecl-cl-ana.package-utils
  (sbcl-package->ecl-package sbcl-cl-ana.package-utils))

(define-public sbcl-cl-ana.string-utils
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.string-utils")
    (inputs
     `(("split-sequence" ,sbcl-split-sequence)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "string-utils/cl-ana.string-utils.asd")
       ((#:asd-system-name _ #f) "cl-ana.string-utils")))))

(define-public cl-ana.string-utils
  (sbcl-package->cl-source-package sbcl-cl-ana.string-utils))

(define-public ecl-cl-ana.string-utils
  (sbcl-package->ecl-package sbcl-cl-ana.string-utils))

(define-public sbcl-cl-ana.functional-utils
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.functional-utils")
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "functional-utils/cl-ana.functional-utils.asd")
       ((#:asd-system-name _ #f) "cl-ana.functional-utils")))))

(define-public cl-ana.functional-utils
  (sbcl-package->cl-source-package sbcl-cl-ana.functional-utils))

(define-public ecl-cl-ana.functional-utils
  (sbcl-package->ecl-package sbcl-cl-ana.functional-utils))

(define-public sbcl-cl-ana.list-utils
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.list-utils")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cl-ana.functional-utils" ,sbcl-cl-ana.functional-utils)
       ("cl-ana.string-utils" ,sbcl-cl-ana.string-utils)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "list-utils/cl-ana.list-utils.asd")
       ((#:asd-system-name _ #f) "cl-ana.list-utils")))))

(define-public cl-ana.list-utils
  (sbcl-package->cl-source-package sbcl-cl-ana.list-utils))

(define-public ecl-cl-ana.list-utils
  (sbcl-package->ecl-package sbcl-cl-ana.list-utils))

(define-public sbcl-cl-ana.generic-math
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.generic-math")
    (inputs
     `(("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.package-utils" ,sbcl-cl-ana.package-utils)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "generic-math/cl-ana.generic-math.asd")
       ((#:asd-system-name _ #f) "cl-ana.generic-math")))))

(define-public cl-ana.generic-math
  (sbcl-package->cl-source-package sbcl-cl-ana.generic-math))

(define-public ecl-cl-ana.generic-math
  (sbcl-package->ecl-package sbcl-cl-ana.generic-math))

(define-public sbcl-cl-ana.math-functions
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.math-functions")
    (inputs
     `(("cl-ana.generic-math" ,sbcl-cl-ana.generic-math)
       ("gsll" ,sbcl-gsll)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "math-functions/cl-ana.math-functions.asd")
       ((#:asd-system-name _ #f) "cl-ana.math-functions")))))

(define-public cl-ana.math-functions
  (sbcl-package->cl-source-package sbcl-cl-ana.math-functions))

(define-public sbcl-cl-ana.calculus
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.calculus")
    (inputs
     `(("cl-ana.generic-math" ,sbcl-cl-ana.generic-math)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "calculus/cl-ana.calculus.asd")
       ((#:asd-system-name _ #f) "cl-ana.calculus")))))

(define-public cl-ana.calculus
  (sbcl-package->cl-source-package sbcl-cl-ana.calculus))

(define-public ecl-cl-ana.calculus
  (sbcl-package->ecl-package sbcl-cl-ana.calculus))

(define-public sbcl-cl-ana.symbol-utils
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.symbol-utils")
    (inputs
     `(("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "symbol-utils/cl-ana.symbol-utils.asd")
       ((#:asd-system-name _ #f) "cl-ana.symbol-utils")))))

(define-public cl-ana.symbol-utils
  (sbcl-package->cl-source-package sbcl-cl-ana.symbol-utils))

(define-public ecl-cl-ana.symbol-utils
  (sbcl-package->ecl-package sbcl-cl-ana.symbol-utils))

(define-public sbcl-cl-ana.macro-utils
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.macro-utils")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.string-utils" ,sbcl-cl-ana.string-utils)
       ("cl-ana.symbol-utils" ,sbcl-cl-ana.symbol-utils)
       ("split-sequence" ,sbcl-split-sequence)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "macro-utils/cl-ana.macro-utils.asd")
       ((#:asd-system-name _ #f) "cl-ana.macro-utils")))))

(define-public cl-ana.macro-utils
  (sbcl-package->cl-source-package sbcl-cl-ana.macro-utils))

(define-public ecl-cl-ana.macro-utils
  (sbcl-package->ecl-package sbcl-cl-ana.macro-utils))

(define-public sbcl-cl-ana.binary-tree
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.binary-tree")
    (inputs
     `(("cl-ana.functional-utils" ,sbcl-cl-ana.functional-utils)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.macro-utils" ,sbcl-cl-ana.macro-utils)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "binary-tree/cl-ana.binary-tree.asd")
       ((#:asd-system-name _ #f) "cl-ana.binary-tree")))))

(define-public cl-ana.binary-tree
  (sbcl-package->cl-source-package sbcl-cl-ana.binary-tree))

(define-public ecl-cl-ana.binary-tree
  (sbcl-package->ecl-package sbcl-cl-ana.binary-tree))

(define-public sbcl-cl-ana.tensor
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.tensor")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cl-ana.generic-math" ,sbcl-cl-ana.generic-math)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.macro-utils" ,sbcl-cl-ana.macro-utils)
       ("cl-ana.symbol-utils" ,sbcl-cl-ana.symbol-utils)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "tensor/cl-ana.tensor.asd")
       ((#:asd-system-name _ #f) "cl-ana.tensor")))))

(define-public cl-ana.tensor
  (sbcl-package->cl-source-package sbcl-cl-ana.tensor))

(define-public ecl-cl-ana.tensor
  (sbcl-package->ecl-package sbcl-cl-ana.tensor))

(define-public sbcl-cl-ana.error-propogation
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.error-propogation")
    (inputs
     `(("cl-ana.generic-math" ,sbcl-cl-ana.generic-math)
       ("cl-ana.math-functions" ,sbcl-cl-ana.math-functions)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "error-propogation/cl-ana.error-propogation.asd")
       ((#:asd-system-name _ #f) "cl-ana.error-propogation")))))

(define-public cl-ana.error-propogation
  (sbcl-package->cl-source-package sbcl-cl-ana.error-propogation))

(define-public sbcl-cl-ana.quantity
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.quantity")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cl-ana.error-propogation" ,sbcl-cl-ana.error-propogation)
       ("cl-ana.generic-math" ,sbcl-cl-ana.generic-math)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.macro-utils" ,sbcl-cl-ana.macro-utils)
       ("cl-ana.symbol-utils" ,sbcl-cl-ana.symbol-utils)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "quantity/cl-ana.quantity.asd")
       ((#:asd-system-name _ #f) "cl-ana.quantity")))))

(define-public cl-ana.quantity
  (sbcl-package->cl-source-package sbcl-cl-ana.quantity))

(define-public sbcl-cl-ana.table
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.table")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cl-ana.functional-utils" ,sbcl-cl-ana.functional-utils)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.macro-utils" ,sbcl-cl-ana.macro-utils)
       ("cl-ana.string-utils" ,sbcl-cl-ana.string-utils)
       ("cl-ana.symbol-utils" ,sbcl-cl-ana.symbol-utils)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "table/cl-ana.table.asd")
       ((#:asd-system-name _ #f) "cl-ana.table")))))

(define-public cl-ana.table
  (sbcl-package->cl-source-package sbcl-cl-ana.table))

(define-public ecl-cl-ana.table
  (sbcl-package->ecl-package sbcl-cl-ana.table))

(define-public sbcl-cl-ana.table-utils
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.table-utils")
    (inputs
     `(("cl-ana.string-utils" ,sbcl-cl-ana.string-utils)
       ("cl-ana.symbol-utils" ,sbcl-cl-ana.symbol-utils)
       ("cl-ana.table" ,sbcl-cl-ana.table)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "table-utils/cl-ana.table-utils.asd")
       ((#:asd-system-name _ #f) "cl-ana.table-utils")))))

(define-public cl-ana.table-utils
  (sbcl-package->cl-source-package sbcl-cl-ana.table-utils))

(define-public ecl-cl-ana.table-utils
  (sbcl-package->ecl-package sbcl-cl-ana.table-utils))

(define-public sbcl-cl-ana.hdf-cffi
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.hdf-cffi")
    (inputs
     `(("cffi" ,sbcl-cffi)
       ("hdf5" ,hdf5-parallel-openmpi)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "hdf-cffi/cl-ana.hdf-cffi.asd")
       ((#:asd-system-name _ #f) "cl-ana.hdf-cffi")
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "hdf-cffi/hdf-cffi.lisp"
                 (("/usr/lib/i386-linux-gnu/hdf5/serial/libhdf5.so")
                  (string-append
                   (assoc-ref inputs "hdf5")
                   "/lib/libhdf5.so")))))))))))

(define-public cl-ana.hdf-cffi
  (sbcl-package->cl-source-package sbcl-cl-ana.hdf-cffi))

(define-public ecl-cl-ana.hdf-cffi
  (sbcl-package->ecl-package sbcl-cl-ana.hdf-cffi))

(define-public sbcl-cl-ana.int-char
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.int-char")
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "int-char/cl-ana.int-char.asd")
       ((#:asd-system-name _ #f) "cl-ana.int-char")))))

(define-public cl-ana.int-char
  (sbcl-package->cl-source-package sbcl-cl-ana.int-char))

(define-public ecl-cl-ana.int-char
  (sbcl-package->ecl-package sbcl-cl-ana.int-char))

(define-public sbcl-cl-ana.memoization
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.memoization")
    (inputs
     `(("alexandria" ,sbcl-alexandria)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "memoization/cl-ana.memoization.asd")
       ((#:asd-system-name _ #f) "cl-ana.memoization")))))

(define-public cl-ana.memoization
  (sbcl-package->cl-source-package sbcl-cl-ana.memoization))

(define-public ecl-cl-ana.memoization
  (sbcl-package->ecl-package sbcl-cl-ana.memoization))

(define-public sbcl-cl-ana.typespec
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.typespec")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cffi" ,sbcl-cffi)
       ("cl-ana.int-char" ,sbcl-cl-ana.int-char)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.memoization" ,sbcl-cl-ana.memoization)
       ("cl-ana.string-utils" ,sbcl-cl-ana.string-utils)
       ("cl-ana.symbol-utils" ,sbcl-cl-ana.symbol-utils)
       ("cl-ana.tensor" ,sbcl-cl-ana.tensor)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "typespec/cl-ana.typespec.asd")
       ((#:asd-system-name _ #f) "cl-ana.typespec")))))

(define-public cl-ana.typespec
  (sbcl-package->cl-source-package sbcl-cl-ana.typespec))

(define-public ecl-cl-ana.typespec
  (sbcl-package->ecl-package sbcl-cl-ana.typespec))

(define-public sbcl-cl-ana.hdf-typespec
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.hdf-typespec")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cffi" ,sbcl-cffi)
       ("cl-ana.hdf-cffi" ,sbcl-cl-ana.hdf-cffi)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.memoization" ,sbcl-cl-ana.memoization)
       ("cl-ana.string-utils" ,sbcl-cl-ana.string-utils)
       ("cl-ana.symbol-utils" ,sbcl-cl-ana.symbol-utils)
       ("cl-ana.typespec" ,sbcl-cl-ana.typespec)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "hdf-typespec/cl-ana.hdf-typespec.asd")
       ((#:asd-system-name _ #f) "cl-ana.hdf-typespec")))))

(define-public cl-ana.hdf-typespec
  (sbcl-package->cl-source-package sbcl-cl-ana.hdf-typespec))

(define-public ecl-cl-ana.hdf-typespec
  (sbcl-package->ecl-package sbcl-cl-ana.hdf-typespec))

(define-public sbcl-cl-ana.hdf-utils
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.hdf-utils")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cffi" ,sbcl-cffi)
       ("cl-ana.hdf-cffi" ,sbcl-cl-ana.hdf-cffi)
       ("cl-ana.hdf-typespec" ,sbcl-cl-ana.hdf-typespec)
       ("cl-ana.macro-utils" ,sbcl-cl-ana.macro-utils)
       ("cl-ana.memoization" ,sbcl-cl-ana.memoization)
       ("cl-ana.pathname-utils" ,sbcl-cl-ana.pathname-utils)
       ("cl-ana.string-utils" ,sbcl-cl-ana.string-utils)
       ("cl-ana.typespec" ,sbcl-cl-ana.typespec)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "hdf-utils/cl-ana.hdf-utils.asd")
       ((#:asd-system-name _ #f) "cl-ana.hdf-utils")))))

(define-public cl-ana.hdf-utils
  (sbcl-package->cl-source-package sbcl-cl-ana.hdf-utils))

(define-public ecl-cl-ana.hdf-utils
  (sbcl-package->ecl-package sbcl-cl-ana.hdf-utils))

(define-public sbcl-cl-ana.typed-table
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.typed-table")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.string-utils" ,sbcl-cl-ana.string-utils)
       ("cl-ana.symbol-utils" ,sbcl-cl-ana.symbol-utils)
       ("cl-ana.table" ,sbcl-cl-ana.table)
       ("cl-ana.typespec" ,sbcl-cl-ana.typespec)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "typed-table/cl-ana.typed-table.asd")
       ((#:asd-system-name _ #f) "cl-ana.typed-table")))))

(define-public cl-ana.typed-table
  (sbcl-package->cl-source-package sbcl-cl-ana.typed-table))

(define-public ecl-cl-ana.typed-table
  (sbcl-package->ecl-package sbcl-cl-ana.typed-table))

(define-public sbcl-cl-ana.hdf-table
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.hdf-table")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cl-ana.binary-tree" ,sbcl-cl-ana.binary-tree)
       ("cl-ana.hdf-cffi" ,sbcl-cl-ana.hdf-cffi)
       ("cl-ana.hdf-typespec" ,sbcl-cl-ana.hdf-typespec)
       ("cl-ana.hdf-utils" ,sbcl-cl-ana.hdf-utils)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.memoization" ,sbcl-cl-ana.memoization)
       ("cl-ana.table" ,sbcl-cl-ana.table)
       ("cl-ana.typed-table" ,sbcl-cl-ana.typed-table)
       ("cl-ana.typespec" ,sbcl-cl-ana.typespec)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "hdf-table/cl-ana.hdf-table.asd")
       ((#:asd-system-name _ #f) "cl-ana.hdf-table")))))

(define-public cl-ana.hdf-table
  (sbcl-package->cl-source-package sbcl-cl-ana.hdf-table))

(define-public ecl-cl-ana.hdf-table
  (sbcl-package->ecl-package sbcl-cl-ana.hdf-table))

(define-public sbcl-cl-ana.gsl-cffi
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.gsl-cffi")
    (inputs
     `(("cffi" ,sbcl-cffi)
       ("gsl" ,gsl)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "gsl-cffi/cl-ana.gsl-cffi.asd")
       ((#:asd-system-name _ #f) "cl-ana.gsl-cffi")
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "gsl-cffi/gsl-cffi.lisp"
                 (("define-foreign-library gsl-cffi" all)
                  (string-append all " (:unix "
                                 (assoc-ref inputs "gsl")
                                 "/lib/libgsl.so)")))))))))))

(define-public cl-ana.gsl-cffi
  (sbcl-package->cl-source-package sbcl-cl-ana.gsl-cffi))

(define-public ecl-cl-ana.gsl-cffi
  (sbcl-package->ecl-package sbcl-cl-ana.gsl-cffi))

(define-public sbcl-cl-ana.ntuple-table
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.ntuple-table")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cffi" ,sbcl-cffi)
       ("cl-ana.gsl-cffi" ,sbcl-cl-ana.gsl-cffi)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.table" ,sbcl-cl-ana.table)
       ("cl-ana.typed-table" ,sbcl-cl-ana.typed-table)
       ("cl-ana.typespec" ,sbcl-cl-ana.typespec)
       ("gsll" ,sbcl-gsll)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "ntuple-table/cl-ana.ntuple-table.asd")
       ((#:asd-system-name _ #f) "cl-ana.ntuple-table")))))

(define-public cl-ana.ntuple-table
  (sbcl-package->cl-source-package sbcl-cl-ana.ntuple-table))

(define-public sbcl-cl-ana.csv-table
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.csv-table")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("antik" ,sbcl-antik)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.table" ,sbcl-cl-ana.table)
       ("cl-csv" ,sbcl-cl-csv)
       ("iterate" ,sbcl-iterate)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "csv-table/cl-ana.csv-table.asd")
       ((#:asd-system-name _ #f) "cl-ana.csv-table")))))

(define-public cl-ana.csv-table
  (sbcl-package->cl-source-package sbcl-cl-ana.csv-table))

(define-public sbcl-cl-ana.reusable-table
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.reusable-table")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cl-ana.table" ,sbcl-cl-ana.table)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "reusable-table/cl-ana.reusable-table.asd")
       ((#:asd-system-name _ #f) "cl-ana.reusable-table")))))

(define-public cl-ana.reusable-table
  (sbcl-package->cl-source-package sbcl-cl-ana.reusable-table))

(define-public ecl-cl-ana.reusable-table
  (sbcl-package->ecl-package sbcl-cl-ana.reusable-table))

(define-public sbcl-cl-ana.linear-algebra
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.linear-algebra")
    (inputs
     `(("cl-ana.generic-math" ,sbcl-cl-ana.generic-math)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.math-functions" ,sbcl-cl-ana.math-functions)
       ("cl-ana.tensor" ,sbcl-cl-ana.tensor)
       ("gsll" ,sbcl-gsll)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "linear-algebra/cl-ana.linear-algebra.asd")
       ((#:asd-system-name _ #f) "cl-ana.linear-algebra")))))

(define-public cl-ana.linear-algebra
  (sbcl-package->cl-source-package sbcl-cl-ana.linear-algebra))

(define-public sbcl-cl-ana.lorentz
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.lorentz")
    (inputs
     `(("cl-ana.generic-math" ,sbcl-cl-ana.generic-math)
       ("cl-ana.linear-algebra" ,sbcl-cl-ana.linear-algebra)
       ("cl-ana.tensor" ,sbcl-cl-ana.tensor)
       ("iterate" ,sbcl-iterate)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "lorentz/cl-ana.lorentz.asd")
       ((#:asd-system-name _ #f) "cl-ana.lorentz")))))

(define-public cl-ana.lorentz
  (sbcl-package->cl-source-package sbcl-cl-ana.lorentz))

(define-public sbcl-cl-ana.clos-utils
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.clos-utils")
    (inputs
     `(("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.symbol-utils" ,sbcl-cl-ana.symbol-utils)
       ("cl-ana.tensor" ,sbcl-cl-ana.tensor)
       ("closer-mop" ,sbcl-closer-mop)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "clos-utils/cl-ana.clos-utils.asd")
       ((#:asd-system-name _ #f) "cl-ana.clos-utils")))))

(define-public cl-ana.clos-utils
  (sbcl-package->cl-source-package sbcl-cl-ana.clos-utils))

(define-public ecl-cl-ana.clos-utils
  (sbcl-package->ecl-package sbcl-cl-ana.clos-utils))

(define-public sbcl-cl-ana.hash-table-utils
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.hash-table-utils")
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "hash-table-utils/cl-ana.hash-table-utils.asd")
       ((#:asd-system-name _ #f) "cl-ana.hash-table-utils")))))

(define-public cl-ana.hash-table-utils
  (sbcl-package->cl-source-package sbcl-cl-ana.hash-table-utils))

(define-public ecl-cl-ana.hash-table-utils
  (sbcl-package->ecl-package sbcl-cl-ana.hash-table-utils))

(define-public sbcl-cl-ana.map
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.map")
    (inputs
     `(("cl-ana.hash-table-utils" ,sbcl-cl-ana.hash-table-utils)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "map/cl-ana.map.asd")
       ((#:asd-system-name _ #f) "cl-ana.map")))))

(define-public cl-ana.map
  (sbcl-package->cl-source-package sbcl-cl-ana.map))

(define-public ecl-cl-ana.map
  (sbcl-package->ecl-package sbcl-cl-ana.map))

(define-public sbcl-cl-ana.fitting
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.fitting")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cl-ana.error-propogation" ,sbcl-cl-ana.error-propogation)
       ("cl-ana.generic-math" ,sbcl-cl-ana.generic-math)
       ("cl-ana.map" ,sbcl-cl-ana.map)
       ("cl-ana.math-functions" ,sbcl-cl-ana.math-functions)
       ("gsll" ,sbcl-gsll)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "fitting/cl-ana.fitting.asd")
       ((#:asd-system-name _ #f) "cl-ana.fitting")))))

(define-public cl-ana.fitting
  (sbcl-package->cl-source-package sbcl-cl-ana.fitting))

(define-public sbcl-cl-ana.histogram
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.histogram")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("iterate" ,sbcl-iterate)
       ("cl-ana.binary-tree" ,sbcl-cl-ana.binary-tree)
       ("cl-ana.clos-utils" ,sbcl-cl-ana.clos-utils)
       ("cl-ana.fitting" ,sbcl-cl-ana.fitting)
       ("cl-ana.functional-utils" ,sbcl-cl-ana.functional-utils)
       ("cl-ana.generic-math" ,sbcl-cl-ana.generic-math)
       ("cl-ana.hash-table-utils" ,sbcl-cl-ana.hash-table-utils)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.macro-utils" ,sbcl-cl-ana.macro-utils)
       ("cl-ana.map" ,sbcl-cl-ana.map)
       ("cl-ana.tensor" ,sbcl-cl-ana.tensor)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "histogram/cl-ana.histogram.asd")
       ((#:asd-system-name _ #f) "cl-ana.histogram")))))

(define-public cl-ana.histogram
  (sbcl-package->cl-source-package sbcl-cl-ana.histogram))

(define-public sbcl-cl-ana.file-utils
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.file-utils")
    (inputs
     `(("external-program" ,sbcl-external-program)
       ("split-sequence" ,sbcl-split-sequence)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "file-utils/cl-ana.file-utils.asd")
       ((#:asd-system-name _ #f) "cl-ana.file-utils")))))

(define-public cl-ana.file-utils
  (sbcl-package->cl-source-package sbcl-cl-ana.file-utils))

(define-public ecl-cl-ana.file-utils
  (sbcl-package->ecl-package sbcl-cl-ana.file-utils))

(define-public sbcl-cl-ana.statistics
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.statistics")
    (inputs
     `(("cl-ana.generic-math" ,sbcl-cl-ana.generic-math)
       ("cl-ana.histogram" ,sbcl-cl-ana.histogram)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.macro-utils" ,sbcl-cl-ana.macro-utils)
       ("cl-ana.map" ,sbcl-cl-ana.map)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "statistics/cl-ana.statistics.asd")
       ((#:asd-system-name _ #f) "cl-ana.statistics")))))

(define-public cl-ana.statistics
  (sbcl-package->cl-source-package sbcl-cl-ana.statistics))

(define-public sbcl-cl-ana.gnuplot-interface
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.gnuplot-interface")
    (inputs
     `(("external-program" ,sbcl-external-program)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "gnuplot-interface/cl-ana.gnuplot-interface.asd")
       ((#:asd-system-name _ #f) "cl-ana.gnuplot-interface")))))

(define-public cl-ana.gnuplot-interface
  (sbcl-package->cl-source-package sbcl-cl-ana.gnuplot-interface))

(define-public ecl-cl-ana.gnuplot-interface
  (sbcl-package->ecl-package sbcl-cl-ana.gnuplot-interface))

(define-public sbcl-cl-ana.plotting
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.plotting")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cl-ana.error-propogation" ,sbcl-cl-ana.error-propogation)
       ("cl-ana.functional-utils" ,sbcl-cl-ana.functional-utils)
       ("cl-ana.generic-math" ,sbcl-cl-ana.generic-math)
       ("cl-ana.gnuplot-interface" ,sbcl-cl-ana.gnuplot-interface)
       ("cl-ana.histogram" ,sbcl-cl-ana.histogram)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.macro-utils" ,sbcl-cl-ana.macro-utils)
       ("cl-ana.map" ,sbcl-cl-ana.map)
       ("cl-ana.math-functions" ,sbcl-cl-ana.math-functions)
       ("cl-ana.pathname-utils" ,sbcl-cl-ana.pathname-utils)
       ("cl-ana.string-utils" ,sbcl-cl-ana.string-utils)
       ("cl-ana.tensor" ,sbcl-cl-ana.tensor)
       ("external-program" ,sbcl-external-program)
       ("split-sequence" ,sbcl-split-sequence)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "plotting/cl-ana.plotting.asd")
       ((#:asd-system-name _ #f) "cl-ana.plotting")))))

(define-public cl-ana.plotting
  (sbcl-package->cl-source-package sbcl-cl-ana.plotting))

(define-public sbcl-cl-ana.table-viewing
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.table-viewing")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cl-ana.generic-math" ,sbcl-cl-ana.generic-math)
       ("cl-ana.histogram" ,sbcl-cl-ana.histogram)
       ("cl-ana.macro-utils" ,sbcl-cl-ana.macro-utils)
       ("cl-ana.plotting" ,sbcl-cl-ana.plotting)
       ("cl-ana.string-utils" ,sbcl-cl-ana.string-utils)
       ("cl-ana.table" ,sbcl-cl-ana.table)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "table-viewing/cl-ana.table-viewing.asd")
       ((#:asd-system-name _ #f) "cl-ana.table-viewing")))))

(define-public cl-ana.table-viewing
  (sbcl-package->cl-source-package sbcl-cl-ana.table-viewing))

(define-public sbcl-cl-ana.serialization
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.serialization")
    (inputs
     `(("cl-ana.error-propogation" ,sbcl-cl-ana.error-propogation)
       ("cl-ana.hdf-utils" ,sbcl-cl-ana.hdf-utils)
       ("cl-ana.hdf-table" ,sbcl-cl-ana.hdf-table)
       ("cl-ana.histogram" ,sbcl-cl-ana.histogram)
       ("cl-ana.int-char" ,sbcl-cl-ana.int-char)
       ("cl-ana.macro-utils" ,sbcl-cl-ana.macro-utils)
       ("cl-ana.typespec" ,sbcl-cl-ana.typespec)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "serialization/cl-ana.serialization.asd")
       ((#:asd-system-name _ #f) "cl-ana.serialization")))))

(define-public cl-ana.serialization
  (sbcl-package->cl-source-package sbcl-cl-ana.serialization))

(define-public sbcl-cl-ana.makeres
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.makeres")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cl-ana.error-propogation" ,sbcl-cl-ana.error-propogation)
       ("cl-ana.file-utils" ,sbcl-cl-ana.file-utils)
       ("cl-ana.functional-utils" ,sbcl-cl-ana.functional-utils)
       ("cl-ana.generic-math" ,sbcl-cl-ana.generic-math)
       ("cl-ana.hash-table-utils" ,sbcl-cl-ana.hash-table-utils)
       ("cl-ana.hdf-utils" ,sbcl-cl-ana.hdf-utils)
       ("cl-ana.histogram" ,sbcl-cl-ana.histogram)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.macro-utils" ,sbcl-cl-ana.macro-utils)
       ("cl-ana.map" ,sbcl-cl-ana.map)
       ("cl-ana.memoization" ,sbcl-cl-ana.memoization)
       ("cl-ana.pathname-utils" ,sbcl-cl-ana.pathname-utils)
       ("cl-ana.plotting" ,sbcl-cl-ana.plotting)
       ("cl-ana.reusable-table" ,sbcl-cl-ana.reusable-table)
       ("cl-ana.serialization" ,sbcl-cl-ana.serialization)
       ("cl-ana.string-utils" ,sbcl-cl-ana.string-utils)
       ("cl-ana.symbol-utils" ,sbcl-cl-ana.symbol-utils)
       ("cl-ana.table" ,sbcl-cl-ana.table)
       ("external-program" ,sbcl-external-program)))
    (native-inputs
     `(("cl-fad" ,sbcl-cl-fad)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "makeres/cl-ana.makeres.asd")
       ((#:asd-system-name _ #f) "cl-ana.makeres")))))

(define-public cl-ana.makeres
  (sbcl-package->cl-source-package sbcl-cl-ana.makeres))

(define-public sbcl-cl-ana.makeres-macro
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.makeres-macro")
    (inputs
     `(("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.makeres" ,sbcl-cl-ana.makeres)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "makeres-macro/cl-ana.makeres-macro.asd")
       ((#:asd-system-name _ #f) "cl-ana.makeres-macro")))))

(define-public cl-ana.makeres-macro
  (sbcl-package->cl-source-package sbcl-cl-ana.makeres-macro))

(define-public sbcl-cl-ana.makeres-block
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.makeres-block")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.macro-utils" ,sbcl-cl-ana.macro-utils)
       ("cl-ana.makeres" ,sbcl-cl-ana.makeres)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "makeres-block/cl-ana.makeres-block.asd")
       ((#:asd-system-name _ #f) "cl-ana.makeres-block")))))

(define-public cl-ana.makeres-block
  (sbcl-package->cl-source-package sbcl-cl-ana.makeres-block))

(define-public sbcl-cl-ana.makeres-progress
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.makeres-progress")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cl-ana.generic-math" ,sbcl-cl-ana.generic-math)
       ("cl-ana.makeres" ,sbcl-cl-ana.makeres)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "makeres-progress/cl-ana.makeres-progress.asd")
       ((#:asd-system-name _ #f) "cl-ana.makeres-progress")))))

(define-public cl-ana.makeres-progress
  (sbcl-package->cl-source-package sbcl-cl-ana.makeres-progress))

(define-public sbcl-cl-ana.makeres-table
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.makeres-table")
    (inputs
     `(("cl-ana.csv-table" ,sbcl-cl-ana.csv-table)
       ("cl-ana.hash-table-utils" ,sbcl-cl-ana.hash-table-utils)
       ("cl-ana.hdf-table" ,sbcl-cl-ana.hdf-table)
       ("cl-ana.hdf-utils" ,sbcl-cl-ana.hdf-utils)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.macro-utils" ,sbcl-cl-ana.macro-utils)
       ("cl-ana.makeres" ,sbcl-cl-ana.makeres)
       ("cl-ana.makeres-macro" ,sbcl-cl-ana.makeres-macro)
       ("cl-ana.memoization" ,sbcl-cl-ana.memoization)
       ("cl-ana.ntuple-table" ,sbcl-cl-ana.ntuple-table)
       ("cl-ana.reusable-table" ,sbcl-cl-ana.reusable-table)
       ("cl-ana.string-utils" ,sbcl-cl-ana.string-utils)
       ("cl-ana.table" ,sbcl-cl-ana.table)))
    (native-inputs
     `(("cl-fad" ,sbcl-cl-fad)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "makeres-table/cl-ana.makeres-table.asd")
       ((#:asd-system-name _ #f) "cl-ana.makeres-table")))))

(define-public cl-ana.makeres-table
  (sbcl-package->cl-source-package sbcl-cl-ana.makeres-table))

(define-public sbcl-cl-ana.makeres-graphviz
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.makeres-graphviz")
    (inputs
     `(("cl-ana.makeres" ,sbcl-cl-ana.makeres)
       ("external-program" ,sbcl-external-program)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "makeres-graphviz/cl-ana.makeres-graphviz.asd")
       ((#:asd-system-name _ #f) "cl-ana.makeres-graphviz")))))

(define-public cl-ana.makeres-graphviz
  (sbcl-package->cl-source-package sbcl-cl-ana.makeres-graphviz))

(define-public sbcl-cl-ana.makeres-branch
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.makeres-branch")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cl-ana.generic-math" ,sbcl-cl-ana.generic-math)
       ("cl-ana.hash-table-utils" ,sbcl-cl-ana.hash-table-utils)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.map" ,sbcl-cl-ana.map)
       ("cl-ana.makeres" ,sbcl-cl-ana.makeres)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "makeres-branch/cl-ana.makeres-branch.asd")
       ((#:asd-system-name _ #f) "cl-ana.makeres-branch")))))

(define-public cl-ana.makeres-branch
  (sbcl-package->cl-source-package sbcl-cl-ana.makeres-branch))

(define-public sbcl-cl-ana.makeres-utils
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.makeres-utils")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cl-ana.file-utils" ,sbcl-cl-ana.file-utils)
       ("cl-ana.fitting" ,sbcl-cl-ana.fitting)
       ("cl-ana.functional-utils" ,sbcl-cl-ana.functional-utils)
       ("cl-ana.generic-math" ,sbcl-cl-ana.generic-math)
       ("cl-ana.histogram" ,sbcl-cl-ana.histogram)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.macro-utils" ,sbcl-cl-ana.macro-utils)
       ("cl-ana.makeres" ,sbcl-cl-ana.makeres)
       ("cl-ana.map" ,sbcl-cl-ana.map)
       ("cl-ana.pathname-utils" ,sbcl-cl-ana.pathname-utils)
       ("cl-ana.plotting" ,sbcl-cl-ana.plotting)
       ("cl-ana.reusable-table" ,sbcl-cl-ana.reusable-table)
       ("cl-ana.string-utils" ,sbcl-cl-ana.string-utils)
       ("cl-ana.symbol-utils" ,sbcl-cl-ana.symbol-utils)
       ("cl-ana.table" ,sbcl-cl-ana.table)))
    (native-inputs
     `(("cl-fad" ,sbcl-cl-fad)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "makeres-utils/cl-ana.makeres-utils.asd")
       ((#:asd-system-name _ #f) "cl-ana.makeres-utils")))))

(define-public cl-ana.makeres-utils
  (sbcl-package->cl-source-package sbcl-cl-ana.makeres-utils))

(define-public sbcl-cl-ana.statistical-learning
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana.statistical-learning")
    (inputs
     `(("cl-ana.generic-math" ,sbcl-cl-ana.generic-math)
       ("cl-ana.functional-utils" ,sbcl-cl-ana.functional-utils)
       ("cl-ana.histogram" ,sbcl-cl-ana.histogram)
       ("cl-ana.linear-algebra" ,sbcl-cl-ana.linear-algebra)
       ("cl-ana.list-utils" ,sbcl-cl-ana.list-utils)
       ("cl-ana.macro-utils" ,sbcl-cl-ana.macro-utils)
       ("cl-ana.math-functions" ,sbcl-cl-ana.math-functions)
       ("cl-ana.map" ,sbcl-cl-ana.map)
       ("cl-ana.statistics" ,sbcl-cl-ana.statistics)))
    (native-inputs
     `(("cl-fad" ,sbcl-cl-fad)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "")
        "statistical-learning/cl-ana.statistical-learning.asd")
       ((#:asd-system-name _ #f) "cl-ana.statistical-learning")))))

(define-public cl-ana.statistical-learning
  (sbcl-package->cl-source-package sbcl-cl-ana.statistical-learning))

(define-public sbcl-cl-ana
  (package
    (inherit sbcl-cl-ana-boot0)
    (name "sbcl-cl-ana")
    (inputs
     `(("cl-ana.binary-tree" ,sbcl-cl-ana.binary-tree)
       ("cl-ana.calculus" ,sbcl-cl-ana.calculus)
       ("cl-ana.clos-utils" ,sbcl-cl-ana.clos-utils)
       ("cl-ana.csv-table" ,sbcl-cl-ana.csv-table)
       ("cl-ana.error-propogation" ,sbcl-cl-ana.error-propogation)
       ("cl-ana.file-utils" ,sbcl-cl-ana.file-utils)
       ("cl-ana.fitting" ,sbcl-cl-ana.fitting)
       ("cl-ana.generic-math" ,sbcl-cl-ana.generic-math)
       ("cl-ana.hash-table-utils" ,sbcl-cl-ana.hash-table-utils)
       ("cl-ana.hdf-table" ,sbcl-cl-ana.hdf-table)
       ("cl-ana.histogram" ,sbcl-cl-ana.histogram)
       ("cl-ana.int-char" ,sbcl-cl-ana.int-char)
       ("cl-ana.linear-algebra" ,sbcl-cl-ana.linear-algebra)
       ("cl-ana.lorentz" ,sbcl-cl-ana.lorentz)
       ("cl-ana.map" ,sbcl-cl-ana.map)
       ("cl-ana.makeres" ,sbcl-cl-ana.makeres)
       ("cl-ana.makeres-block" ,sbcl-cl-ana.makeres-block)
       ("cl-ana.makeres-branch" ,sbcl-cl-ana.makeres-branch)
       ("cl-ana.makeres-graphviz" ,sbcl-cl-ana.makeres-graphviz)
       ("cl-ana.makeres-macro" ,sbcl-cl-ana.makeres-macro)
       ("cl-ana.makeres-progress" ,sbcl-cl-ana.makeres-progress)
       ("cl-ana.makeres-table" ,sbcl-cl-ana.makeres-table)
       ("cl-ana.makeres-utils" ,sbcl-cl-ana.makeres-utils)
       ("cl-ana.math-functions" ,sbcl-cl-ana.math-functions)
       ("cl-ana.ntuple-table" ,sbcl-cl-ana.ntuple-table)
       ("cl-ana.package-utils" ,sbcl-cl-ana.package-utils)
       ("cl-ana.pathname-utils" ,sbcl-cl-ana.pathname-utils)
       ("cl-ana.plotting" ,sbcl-cl-ana.plotting)
       ("cl-ana.quantity" ,sbcl-cl-ana.quantity)
       ("cl-ana.reusable-table" ,sbcl-cl-ana.reusable-table)
       ("cl-ana.serialization" ,sbcl-cl-ana.serialization)
       ("cl-ana.statistics" ,sbcl-cl-ana.statistics)
       ("cl-ana.statistical-learning" ,sbcl-cl-ana.statistical-learning)
       ("cl-ana.table" ,sbcl-cl-ana.table)
       ("cl-ana.table-utils" ,sbcl-cl-ana.table-utils)
       ("cl-ana.table-viewing" ,sbcl-cl-ana.table-viewing)
       ("cl-ana.tensor" ,sbcl-cl-ana.tensor)
       ("libffi" ,libffi)))
    (native-inputs
     `(("cl-fad" ,sbcl-cl-fad)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-ana-boot0)
       ((#:asd-file _ "") "cl-ana.asd")
       ((#:asd-system-name _ #f) "cl-ana")))))

(define-public cl-ana
  (sbcl-package->cl-source-package sbcl-cl-ana))

(define-public sbcl-archive
  (let ((commit "631271c091ed02994bec3980cb288a2cf32c7cdc")
        (revision "1"))
    (package
      (name "sbcl-archive")
      (version (git-version "0.9" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/sharplispers/archive.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0pvsc9fmybx7rxd0kmzq4shi6hszdpwdc1sfy7jwyfxf8n3hnv4p"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("cl-fad" ,sbcl-cl-fad)
         ("trivial-gray-streams" ,sbcl-trivial-gray-streams)))
      (synopsis "Common Lisp library for tar and cpio archives")
      (description
       "This is a Common Lisp library to read and write disk-based file
archives such as those generated by the tar and cpio programs on Unix.")
      (home-page "https://github.com/sharplispers/archive")
      (license license:bsd-3))))

(define-public cl-archive
  (sbcl-package->cl-source-package sbcl-archive))

(define-public ecl-archive
  (sbcl-package->ecl-package sbcl-archive))

(define-public sbcl-misc-extensions
  (let ((commit "101c05112bf2f1e1bbf527396822d2f50ca6327a")
        (revision "1"))
    (package
      (name "sbcl-misc-extensions")
      (version (git-version "3.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.common-lisp.net/misc-extensions/devel.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0gz5f4p70qzilnxsnf5lih2n9m4wjcw8hlw4w8mpn9jyhyppyyv0"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Collection of small macros and extensions for Common Lisp")
      (description
       "This project is intended as a catchall for small, general-purpose
extensions to Common Lisp.  It contains:

@itemize
@item @code{new-let}, a macro that combines and generalizes @code{let},
@code{let*} and @code{multiple-value-bind},
@item @code{gmap}, an iteration macro that generalizes @code{map}.
@end itemize\n")
      (home-page "https://common-lisp.net/project/misc-extensions/")
      (license license:public-domain))))

(define-public cl-misc-extensions
  (sbcl-package->cl-source-package sbcl-misc-extensions))

(define-public ecl-misc-extensions
  (sbcl-package->ecl-package sbcl-misc-extensions))

(define-public sbcl-mt19937
  (package
    (name "sbcl-mt19937")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://common-lisp.net/project/asdf-packaging/"
                           "mt19937-latest.tar.gz"))
       (sha256
        (base32
         "1iw636b0iw5ygkv02y8i41lh7xj0acglv0hg5agryn0zzi2nf1xv"))))
    (build-system asdf-build-system/sbcl)
    (synopsis "Mersenne Twister pseudo-random number generator")
    (description
     "MT19937 is a portable Mersenne Twister pseudo-random number generator
for Common Lisp.")
    (home-page "https://www.cliki.net/mt19937")
    (license license:public-domain)))

(define-public cl-mt19937
  (sbcl-package->cl-source-package sbcl-mt19937))

(define-public ecl-mt19937
  (sbcl-package->ecl-package sbcl-mt19937))

(define-public sbcl-fset
  (let ((commit "6d2f9ded8934d2b42f2571a0ba5bda091037d852")
        (revision "1"))
    (package
      (name "sbcl-fset")
      (version (git-version "1.3.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/slburson/fset")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "127acblwrbqicx47h6sgvknz1cqyfn8p4xkhkn1m7hxh8w5gk1zy"))
         (snippet '(begin
                     ;; Remove obsolete copy of system definition.
                     (delete-file "Code/fset.asd")
                     #t))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("misc-extensions" ,sbcl-misc-extensions)
         ("mt19937" ,sbcl-mt19937)
         ("named-readtables" ,sbcl-named-readtables)))
      (synopsis "Functional set-theoretic collections library")
      (description
       "FSet is a functional set-theoretic collections library for Common Lisp.
Functional means that all update operations return a new collection rather than
modifying an existing one in place.  Set-theoretic means that collections may
be nested arbitrarily with no additional programmer effort; for instance, sets
may contain sets, maps may be keyed by sets, etc.")
      (home-page "https://common-lisp.net/project/fset/Site/index.html")
      (license license:llgpl))))

(define-public cl-fset
  (sbcl-package->cl-source-package sbcl-fset))

(define-public sbcl-cl-cont
  (let ((commit "fc1fa7e6eb64894fdca13e688e6015fad5290d2a")
        (revision "1"))
    (package
      (name "sbcl-cl-cont")
      (version (git-version "0.3.8" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.common-lisp.net/cl-cont/cl-cont.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1zf8zvb0i6jm3hhfks4w74hibm6avgc6f9s1qwgjrn2bcik8lrvz"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("closer-mop" ,sbcl-closer-mop)))
      (native-inputs
       `(("rt" ,sbcl-rt)))
      (synopsis "Delimited continuations for Common Lisp")
      (description
       "This is a library that implements delimited continuations by
transforming Common Lisp code to continuation passing style.")
      (home-page "https://common-lisp.net/project/cl-cont/")
      (license license:llgpl))))

(define-public cl-cont
  (sbcl-package->cl-source-package sbcl-cl-cont))

(define-public ecl-cl-cont
  (sbcl-package->ecl-package sbcl-cl-cont))

(define-public sbcl-cl-coroutine
  (let ((commit "de098f8d5debd8b14ef6864b5bdcbbf5ddbcfd72")
        (revision "1"))
    (package
      (name "sbcl-cl-coroutine")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/takagi/cl-coroutine.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1cqdhdjxffgfs116l1swjlsmcbly0xgcgrckvaajd566idj9yj4l"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("cl-cont" ,sbcl-cl-cont)))
      (native-inputs
       `(("prove" ,sbcl-prove)))
      (arguments
       `(;; TODO: Fix the tests. They fail with:
         ;; "Component CL-COROUTINE-ASD::CL-COROUTINE-TEST not found"
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-tests
             (lambda _
               (substitute* "cl-coroutine-test.asd"
                 (("cl-test-more")
                  "prove"))
               #t)))))
      (synopsis "Coroutine library for Common Lisp")
      (description
       "This is a coroutine library for Common Lisp implemented using the
continuations of the @code{cl-cont} library.")
      (home-page "https://github.com/takagi/cl-coroutine")
      (license license:llgpl))))

(define-public cl-coroutine
  (sbcl-package->cl-source-package sbcl-cl-coroutine))

(define-public ecl-cl-coroutine
  (sbcl-package->ecl-package sbcl-cl-coroutine))

(define-public sbcl-vom
  (let ((commit "1aeafeb5b74c53741b79497e0ef4acf85c92ff24")
        (revision "1"))
    (package
      (name "sbcl-vom")
      (version (git-version "0.1.4" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/orthecreedence/vom.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0536kppj76ax4lrxhv42npkfjsmx45km2g439vf9jmw3apinz9cy"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Tiny logging utility for Common Lisp")
      (description
       "Vom is a logging library for Common Lisp.  It's goal is to be useful
and small.  It does not provide a lot of features as other loggers do, but
has a small codebase that's easy to understand and use.")
      (home-page "https://github.com/orthecreedence/vom")
      (license license:expat))))

(define-public cl-vom
  (sbcl-package->cl-source-package sbcl-vom))

(define-public ecl-vom
  (sbcl-package->ecl-package sbcl-vom))

(define-public sbcl-cl-libuv
  (let ((commit "32100c023c518038d0670a103eaa4d50dd785d29")
        (revision "1"))
    (package
      (name "sbcl-cl-libuv")
      (version (git-version "0.1.6" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/orthecreedence/cl-libuv.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1kwix4si8a8hza34ab2k7whrh7z0yrmx39v2wc3qblv9m244jkh1"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("cffi" ,sbcl-cffi)
         ("cffi-grovel" ,sbcl-cffi-grovel)
         ("libuv" ,libuv)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "lib.lisp"
                 (("/usr/lib/libuv.so")
                  (string-append (assoc-ref inputs "libuv")
                                 "/lib/libuv.so")))
               #t))
           (add-after 'fix-paths 'fix-system-definition
             (lambda _
               (substitute* "cl-libuv.asd"
                 (("#:cffi #:alexandria")
                  "#:cffi #:cffi-grovel #:alexandria"))
               #t)))))
      (synopsis "Common Lisp bindings to libuv")
      (description
       "This library provides low-level libuv bindings for Common Lisp.")
      (home-page "https://github.com/orthecreedence/cl-libuv")
      (license license:expat))))

(define-public cl-libuv
  (sbcl-package->cl-source-package sbcl-cl-libuv))

(define-public ecl-cl-libuv
  (sbcl-package->ecl-package sbcl-cl-libuv))

(define-public sbcl-cl-async-base
  (let ((commit "f6423e44404a44434d803605e0d2e17199158e28")
        (revision "1"))
    (package
      (name "sbcl-cl-async-base")
      (version (git-version "0.6.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/orthecreedence/cl-async.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "11xi9dxb8mjgwzrkj88i0xkgk26z9w9ddxzbv6xsvfc1d4x5cf4x"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("bordeaux-threads" ,sbcl-bordeaux-threads)
         ("cffi" ,sbcl-cffi)
         ("cl-libuv" ,sbcl-cl-libuv)))
      (arguments
       `(#:asd-file "cl-async.asd"))
      (synopsis "Base system for cl-async")
      (description
       "Cl-async is a library for general purpose, non-blocking programming in
Common Lisp.  It uses the libuv library as backend.")
      (home-page "https://orthecreedence.github.io/cl-async/")
      (license license:expat))))

(define-public cl-async-base
  (sbcl-package->cl-source-package sbcl-cl-async-base))

(define-public ecl-cl-async-base
  (sbcl-package->ecl-package sbcl-cl-async-base))

(define-public sbcl-cl-async-util
  (package
    (inherit sbcl-cl-async-base)
    (name "sbcl-cl-async-util")
    (inputs
     `(("bordeaux-threads" ,sbcl-bordeaux-threads)
       ("cffi" ,sbcl-cffi)
       ("cl-async-base" ,sbcl-cl-async-base)
       ("cl-libuv" ,sbcl-cl-libuv)
       ("cl-ppcre" ,sbcl-cl-ppcre)
       ("fast-io" ,sbcl-fast-io)
       ("vom" ,sbcl-vom)))
    (synopsis "Internal utilities for cl-async")))

(define-public cl-async-util
  (sbcl-package->cl-source-package sbcl-cl-async-util))

(define-public ecl-cl-async-util
  (sbcl-package->ecl-package sbcl-cl-async-util))

(define-public sbcl-cl-async
  (package
    (inherit sbcl-cl-async-base)
    (name "sbcl-cl-async")
    (inputs
     `(("babel" ,sbcl-babel)
       ("cffi" ,sbcl-cffi)
       ("cl-async-base" ,sbcl-cl-async-base)
       ("cl-async-util" ,sbcl-cl-async-util)
       ("cl-libuv" ,sbcl-cl-libuv)
       ("cl-ppcre" ,sbcl-cl-ppcre)
       ("static-vectors" ,sbcl-static-vectors)
       ("trivial-features" ,sbcl-trivial-features)
       ("trivial-gray-streams" ,sbcl-trivial-gray-streams)))
    (synopsis "Asynchronous operations for Common Lisp")))

(define-public cl-async
  (sbcl-package->cl-source-package sbcl-cl-async))

(define-public ecl-cl-async
  (sbcl-package->ecl-package sbcl-cl-async))

(define-public sbcl-cl-async-repl
  (package
    (inherit sbcl-cl-async-base)
    (name "sbcl-cl-async-repl")
    (inputs
     `(("bordeaux-threads" ,sbcl-bordeaux-threads)
       ("cl-async" ,sbcl-cl-async)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-async-base)
       ((#:asd-file _ "") "cl-async-repl.asd")))
    (synopsis "REPL integration for cl-async")))

(define-public cl-async-repl
  (sbcl-package->cl-source-package sbcl-cl-async-repl))

(define-public ecl-cl-async-repl
  (sbcl-package->ecl-package sbcl-cl-async-repl))

(define-public sbcl-cl-async-ssl
  (package
    (inherit sbcl-cl-async-base)
    (name "sbcl-cl-async-ssl")
    (inputs
     `(("cffi" ,sbcl-cffi)
       ("cl-async" ,sbcl-cl-async)
       ("openssl" ,openssl)
       ("vom" ,sbcl-vom)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cl-async-base)
       ((#:asd-file _ "") "cl-async-ssl.asd")
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "src/ssl/package.lisp"
                 (("libcrypto\\.so")
                  (string-append (assoc-ref inputs "openssl")
                                 "/lib/libcrypto.so"))
                 (("libssl\\.so")
                  (string-append (assoc-ref inputs "openssl")
                                 "/lib/libssl.so")))
               #t))))))
    (synopsis "SSL wrapper around cl-async socket implementation")))

(define-public cl-async-ssl
  (sbcl-package->cl-source-package sbcl-cl-async-ssl))

(define-public ecl-cl-async-ssl
  (sbcl-package->ecl-package sbcl-cl-async-ssl))

(define-public sbcl-blackbird
  (let ((commit "d361f81c1411dec07f6c2dcb11c78f7aea9aaca8")
        (revision "1"))
    (package
      (name "sbcl-blackbird")
      (version (git-version "0.5.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/orthecreedence/blackbird.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0xfds5yaya64arzr7w1x38karyz11swzbhxx1afldpradj9dh19c"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("vom" ,sbcl-vom)))
      (native-inputs
       `(("cl-async" ,sbcl-cl-async)
         ("fiveam" ,sbcl-fiveam)))
      (synopsis "Promise implementation for Common Lisp")
      (description
       "This is a standalone promise implementation for Common Lisp.  It is
the successor to the now-deprecated cl-async-future project.")
      (home-page "https://orthecreedence.github.io/blackbird/")
      (license license:expat))))

(define-public cl-blackbird
  (sbcl-package->cl-source-package sbcl-blackbird))

(define-public ecl-blackbird
  (sbcl-package->ecl-package sbcl-blackbird))

(define-public sbcl-cl-async-future
  (let ((commit "ee36c22a69a9516407458d2ed8b475f1fc473959")
        (revision "1"))
    (package
      (name "sbcl-cl-async-future")
      (version (git-version "0.4.4.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/orthecreedence/cl-async-future.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0z0sc7qlzzxk99f4l26zp6rai9kv0kj0f599sxai5s44p17zbbvh"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("blackbird" ,sbcl-blackbird)))
      (native-inputs
       `(("cl-async" ,sbcl-cl-async)
         ("eos" ,sbcl-eos)))
      (synopsis "Futures implementation for Common Lisp")
      (description
       "This is futures implementation for Common Lisp.  It plugs in nicely
to cl-async.")
      (home-page "https://orthecreedence.github.io/cl-async/future")
      (license license:expat))))

(define-public cl-async-future
  (sbcl-package->cl-source-package sbcl-cl-async-future))

(define-public ecl-cl-async-future
  (sbcl-package->ecl-package sbcl-cl-async-future))

(define-public sbcl-green-threads
  (let ((commit "fff5ebecb441a37e5c511773716aafd84a3c5840")
        (revision "1"))
    (package
      (name "sbcl-green-threads")
      (version (git-version "0.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/thezerobit/green-threads.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1czw7nr0dwfps76h8hjvglk1wdh53yqbfbvv30whwbgqx33iippz"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("cl-async-future" ,sbcl-cl-async-future)
         ("cl-cont" ,sbcl-cl-cont)))
      (native-inputs
       `(("prove" ,sbcl-prove)))
      (arguments
       `(;; TODO: Fix the tests. They fail with:
         ;; "The function BLACKBIRD::PROMISE-VALUES is undefined"
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-tests
             (lambda _
               (substitute* "green-threads-test.asd"
                 (("cl-test-more")
                  "prove"))
               #t)))))
      (synopsis "Cooperative multitasking library for Common Lisp")
      (description
       "This library allows for cooperative multitasking with help of cl-cont
for continuations.  It tries to mimic the API of bordeaux-threads as much as
possible.")
      (home-page "https://github.com/thezerobit/green-threads")
      (license license:bsd-3))))

(define-public cl-green-threads
  (sbcl-package->cl-source-package sbcl-green-threads))

(define-public ecl-green-threads
  (sbcl-package->ecl-package sbcl-green-threads))

(define-public sbcl-cl-base32
  (let ((commit "8cdee06fab397f7b0a19583b57e7f0c98405be85")
        (revision "1"))
    (package
      (name "sbcl-cl-base32")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/hargettp/cl-base32.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "17jrng8jb05d64ggyd11hp308c2fl5drvf9g175blgrkkl8l4mf8"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       `(("lisp-unit" ,sbcl-lisp-unit)))
      (synopsis "Common Lisp library for base32 encoding and decoding")
      (description
       "This package provides functions for base32 encoding and decoding as
defined in RFC4648.")
      (home-page "https://github.com/hargettp/cl-base32")
      (license license:expat))))

(define-public cl-base32
  (sbcl-package->cl-source-package sbcl-cl-base32))

(define-public ecl-cl-base32
  (sbcl-package->ecl-package sbcl-cl-base32))

(define-public sbcl-cl-z85
  (let ((commit "85b3951a9cfa2603acb6aee15567684f9a108098")
        (revision "1"))
    (package
      (name "sbcl-cl-z85")
      (version (git-version "1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/glv2/cl-z85.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0r27pidjaxbm7k1rr90nnajwl5xm2kp65g1fv0fva17lzy45z1mp"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       `(("cl-octet-streams" ,sbcl-cl-octet-streams)
         ("fiveam" ,sbcl-fiveam)))
      (synopsis "Common Lisp library for Z85 encoding and decoding")
      (description
       "This package provides functions to encode or decode byte vectors or
byte streams using the Z85 format, which is a base-85 encoding used by
ZeroMQ.")
      (home-page "https://github.com/glv2/cl-z85")
      (license license:gpl3+))))

(define-public cl-z85
  (sbcl-package->cl-source-package sbcl-cl-z85))

(define-public ecl-cl-z85
  (sbcl-package->ecl-package sbcl-cl-z85))

(define-public sbcl-ltk
  (package
    (name "sbcl-ltk")
    (version "0.992")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/herth/ltk.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13l2q4mskzilya9xh5wy2xvy30lwn104bd8wrq6ifds56r82iy3x"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     `(("imagemagick" ,imagemagick)
       ("tk" ,tk)))
    (arguments
     `(#:asd-file "ltk/ltk.asd"
       #:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'fix-paths
                    (lambda* (#:key inputs #:allow-other-keys)
                      (substitute* "ltk/ltk.lisp"
                        (("#-freebsd \"wish\"")
                         (string-append "#-freebsd \""
                                        (assoc-ref inputs "tk")
                                        "/bin/wish\""))
                        (("do-execute \"convert\"")
                         (string-append "do-execute \""
                                        (assoc-ref inputs "imagemagick")
                                        "/bin/convert\"")))
                      #t)))))
    (synopsis "Common Lisp bindings for the Tk GUI toolkit")
    (description
     "LTK is a Common Lisp binding for the Tk graphics toolkit.  It is written
in pure Common Lisp and does not require any Tk knowledge for its usage.")
    (home-page "http://www.peter-herth.de/ltk/")
    (license license:llgpl)))

(define-public cl-ltk
  (sbcl-package->cl-source-package sbcl-ltk))

(define-public ecl-ltk
  (sbcl-package->ecl-package sbcl-ltk))

(define-public sbcl-ltk-mw
  (package
    (inherit sbcl-ltk)
    (name "sbcl-ltk-mw")
    (inputs
     `(("ltk" ,sbcl-ltk)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-ltk)
       ((#:asd-file _) "ltk/ltk-mw.asd")
       ((#:phases _) '%standard-phases)))
    (synopsis "Extra widgets for LTK")
    (description
     "This is a collection of higher-level widgets built on top of LTK.")))

(define-public cl-ltk-mw
  (sbcl-package->cl-source-package sbcl-ltk-mw))

(define-public ecl-ltk-mw
  (sbcl-package->ecl-package sbcl-ltk-mw))

(define-public sbcl-ltk-remote
  (package
    (inherit sbcl-ltk)
    (name "sbcl-ltk-remote")
    (inputs
     `(("ltk" ,sbcl-ltk)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-ltk)
       ((#:asd-file _) "ltk/ltk-remote.asd")
       ((#:phases _) '%standard-phases)))
    (synopsis "Remote GUI support for LTK")
    (description
     "This LTK extension allows the GUI to be displayed on a computer different
from the one running the Lisp program by using a TCP connection.")))

(define-public cl-ltk-remote
  (sbcl-package->cl-source-package sbcl-ltk-remote))

(define-public sbcl-cl-lex
  (let ((commit "f2dbbe25ef553005fb402d9a6203180c3fa1093b")
        (revision "1"))
    (package
      (name "sbcl-cl-lex")
      (version (git-version "1.1.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/djr7C4/cl-lex.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1kg50f76bfpfxcv4dfivq1n9a0xlsra2ajb0vd68lxwgbidgyc2y"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("cl-ppcre" ,sbcl-cl-ppcre)))
      (synopsis "Common Lisp macros for generating lexical analyzers")
      (description
       "This is a Common Lisp library providing a set of macros for generating
lexical analyzers automatically.  The lexers generated using @code{cl-lex} can
be used with @code{cl-yacc}.")
      (home-page "https://github.com/djr7C4/cl-lex")
      (license license:gpl3))))

(define-public cl-lex
  (sbcl-package->cl-source-package sbcl-cl-lex))

(define-public ecl-cl-lex
  (sbcl-package->ecl-package sbcl-cl-lex))

(define-public sbcl-clunit2
  (let ((commit "5e28343734eb9b7aee39306a614af92c1062d50b")
        (revision "1"))
    (package
      (name "sbcl-clunit2")
      (version (git-version "0.2.4" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://notabug.org/cage/clunit2.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1ngiapfki6nm8a555mzhb5p7ch79i3w665za5bmb5j7q34fy80vw"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Unit testing framework for Common Lisp")
      (description
       "CLUnit is a Common Lisp unit testing framework.  It is designed to be
easy to use so that you can quickly start testing.")
      (home-page "https://notabug.org/cage/clunit2")
      (license license:expat))))

(define-public cl-clunit2
  (sbcl-package->cl-source-package sbcl-clunit2))

(define-public ecl-clunit2
  (sbcl-package->ecl-package sbcl-clunit2))

(define-public sbcl-cl-colors2
  (let ((commit "795aedee593b095fecde574bd999b520dd03ed24")
        (revision "1"))
    (package
      (name "sbcl-cl-colors2")
      (version (git-version "0.2.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://notabug.org/cage/cl-colors2.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0hlyf4h5chkjdp9armla5w4kw5acikk159sym7y8c4jbjp9x47ih"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       `(("clunit2" ,sbcl-clunit2)))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("cl-ppcre" ,sbcl-cl-ppcre)))
      (synopsis "Color library for Common Lisp")
      (description
       "This is a very simple color library for Common Lisp, providing:

@itemize
@item Types for representing colors in HSV and RGB spaces.
@item Simple conversion functions between the above types (and also
hexadecimal representation for RGB).
@item Some predefined colors (currently X11 color names -- of course
the library does not depend on X11).
@end itemize\n")
      (home-page "https://notabug.org/cage/cl-colors2")
      (license license:boost1.0))))

(define-public cl-colors2
  (sbcl-package->cl-source-package sbcl-cl-colors2))

(define-public ecl-cl-colors2
  (sbcl-package->ecl-package sbcl-cl-colors2))

(define-public sbcl-cl-jpeg
  (let ((commit "ec557038128df6895fbfb743bfe8faf8ec2534af")
        (revision "1"))
    (package
      (name "sbcl-cl-jpeg")
      (version (git-version "2.8" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sharplispers/cl-jpeg.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1bkkiqz8fqldlj1wbmrccjsvxcwj98h6s4b6gslr3cg2wmdv5xmy"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "JPEG image library for Common Lisp")
      (description
       "This is a baseline JPEG codec written in Common Lisp.  It can be used
for reading and writing JPEG image files.")
      (home-page "https://github.com/sharplispers/cl-jpeg")
      (license license:bsd-3))))

(define-public cl-jpeg
  (sbcl-package->cl-source-package sbcl-cl-jpeg))

(define-public ecl-cl-jpeg
  (sbcl-package->ecl-package sbcl-cl-jpeg))

(define-public sbcl-nodgui
  (let ((commit "bc59ed9b787dfc9e68ae3bd7f7e8507c5c619212")
        (revision "1"))
    (package
      (name "sbcl-nodgui")
      (version (git-version "0.0.5" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://notabug.org/cage/nodgui.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0xx0dk54d882i598ydnwmy7mnfk0b7vib3ddsgpqxhjck1rwq8l8"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("bordeaux-threads" ,sbcl-bordeaux-threads)
         ("cl-colors2" ,sbcl-cl-colors2)
         ("cl-jpeg" ,sbcl-cl-jpeg)
         ("cl-lex" ,sbcl-cl-lex)
         ("cl-ppcre-unicode" ,sbcl-cl-ppcre-unicode)
         ("cl-unicode" ,sbcl-cl-unicode)
         ("cl-yacc" ,sbcl-cl-yacc)
         ("clunit2" ,sbcl-clunit2)
         ("named-readtables" ,sbcl-named-readtables)
         ("parse-number" ,sbcl-parse-number)
         ("tk" ,tk)))
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (add-after 'unpack 'fix-paths
                      (lambda* (#:key inputs #:allow-other-keys)
                        (substitute* "src/wish-communication.lisp"
                          (("#-freebsd \"wish\"")
                           (string-append "#-freebsd \""
                                          (assoc-ref inputs "tk")
                                          "/bin/wish\"")))
                        #t)))))
      (synopsis "Common Lisp bindings for the Tk GUI toolkit")
      (description
       "Nodgui (@emph{No Drama GUI}) is a Common Lisp binding for the Tk GUI
toolkit.  It also provides a few additional widgets more than the standard Tk
ones.")
      (home-page "https://www.autistici.org/interzona/nodgui.html")
      (license license:llgpl))))

(define-public cl-nodgui
  (sbcl-package->cl-source-package sbcl-nodgui))

(define-public ecl-nodgui
  (sbcl-package->ecl-package sbcl-nodgui))

(define-public sbcl-salza2
  (package
    (name "sbcl-salza2")
    (version "2.0.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xach/salza2.git")
             (commit (string-append "release-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0p38rj4gq7j5k807php7hrz7l2zyyfshv8i9yms7i8lkgg3433ki"))))
    (build-system asdf-build-system/sbcl)
    (synopsis "Common Lisp library for zlib, deflate and gzip compression")
    (description
     "Salza2 is a Common Lisp library for creating compressed data in the zlib,
deflate, or gzip data formats, described in RFC 1950, RFC 1951, and RFC 1952,
respectively.")
    (home-page "https://www.xach.com/lisp/salza2/")
    (license license:bsd-2)))

(define-public cl-salza2
  (sbcl-package->cl-source-package sbcl-salza2))

(define-public ecl-salza2
  (sbcl-package->ecl-package sbcl-salza2))

(define-public sbcl-png-read
  (let ((commit "ec29f38a689972b9f1373f13bbbcd6b05deada88")
        (revision "1"))
    (package
      (name "sbcl-png-read")
      (version (git-version "0.3.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Ramarren/png-read.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0vyczbcwskrygrf1hgrsnk0jil8skmvf1kiaalw5jps4fjrfdkw0"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("babel" ,sbcl-babel)
         ("chipz" ,sbcl-chipz)
         ("iterate" ,sbcl-iterate)))
      (synopsis "PNG decoder for Common Lisp")
      (description "This is a Common Lisp library for reading PNG images.")
      (home-page "https://github.com/Ramarren/png-read")
      (license license:bsd-3))))

(define-public cl-png-read
  (sbcl-package->cl-source-package sbcl-png-read))

(define-public ecl-png-read
  (sbcl-package->ecl-package sbcl-png-read))

(define-public sbcl-zpng
  (package
    (name "sbcl-zpng")
    (version "1.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xach/zpng.git")
             (commit (string-append "release-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0b3ag3jhl3z7kdls3ahdsdxsfhhw5qrizk769984f4wkxhb69rcm"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     `(("salza2" ,sbcl-salza2)))
    (synopsis "PNG encoder for Common Lisp")
    (description "This is a Common Lisp library for creating PNG images.")
    (home-page "https://www.xach.com/lisp/zpng/")
    (license license:bsd-2)))

(define-public cl-zpng
  (sbcl-package->cl-source-package sbcl-zpng))

(define-public ecl-zpng
  (sbcl-package->ecl-package sbcl-zpng))

(define-public sbcl-cl-qrencode
  (package
    (name "sbcl-cl-qrencode")
    (version "0.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jnjcc/cl-qrencode.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1l5k131dchbf6cj8a8xqa731790p01p3qa1kdy2wa9dawy3ymkxr"))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     `(("lisp-unit" ,sbcl-lisp-unit)))
    (inputs
     `(("zpng" ,sbcl-zpng)))
    (synopsis "QR code encoder for Common Lisp")
    (description
     "This Common Lisp library provides function to make QR codes and to save
them as PNG files.")
    (home-page "https://github.com/jnjcc/cl-qrencode")
    (license license:gpl2+)))

(define-public cl-qrencode
  (sbcl-package->cl-source-package sbcl-cl-qrencode))

(define-public ecl-cl-qrencode
  (sbcl-package->ecl-package sbcl-cl-qrencode))

(define-public sbcl-hdf5-cffi
  (let ((commit "5b5c88f191e470e4fe96b462334e3ce0806eed5c")
        (revision "1"))
    (package
      (name "sbcl-hdf5-cffi")
      (version (git-version "1.8.18" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/hdfgroup/hdf5-cffi.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0vda3075423xz83qky998lpac5b04dwfv7bwgh9jq8cs5v0zrxjf"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Common Lisp bindings for the HDF5 library")
      (description
       "@code{hdf5-cffi} is a CFFI wrapper for the HDF5 library.")
      (home-page "https://github.com/hdfgroup/hdf5-cffi")
      (license (license:non-copyleft
                (string-append "https://github.com/HDFGroup/hdf5-cffi/raw/"
                               commit
                               "/LICENSE")))
      (inputs
       `(("cffi" ,sbcl-cffi)
         ("cffi-grovel" ,sbcl-cffi-grovel)
         ("hdf5" ,hdf5-1.10)))
      (native-inputs
       `(("fiveam" ,sbcl-fiveam)))
      (arguments
       `(#:asd-system-name "hdf5-cffi"
         #:asd-file "hdf5-cffi.asd"
         #:test-asd-file "hdf5-cffi.test.asd"
         ;; Tests depend on hdf5-cffi.examples.asd in addition to hdf5-cffi.asd,
         ;; I don't know if there is a way to tell asdf-build-system to load
         ;; an additional system first, so tests are disabled.
         #:tests? #f
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "src/library.lisp"
                 (("libhdf5.so")
                  (string-append
                   (assoc-ref inputs "hdf5")
                   "/lib/libhdf5.so")))))
           (add-after 'unpack 'fix-dependencies
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "hdf5-cffi.asd"
                 ((":depends-on \\(:cffi\\)")
                  ":depends-on (:cffi :cffi-grovel)"))
               (substitute* "hdf5-cffi.test.asd"
                 ((":depends-on \\(:cffi :hdf5-cffi")
                  ":depends-on (:cffi :cffi-grovel :hdf5-cffi"))))))))))

(define-public cl-hdf5-cffi
  (sbcl-package->cl-source-package sbcl-hdf5-cffi))

(define-public ecl-hdf5-cffi
  (sbcl-package->ecl-package sbcl-hdf5-cffi))

(define-public sbcl-cl-randist
  (package
    (name "sbcl-cl-randist")
    (version "0.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lvaruzza/cl-randist.git")
             (commit "f088a54b540a7adefab7c04094a6103f9edda3d0")))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0l8hyd6nbxb7f50vyxz3rbbm7kgr1fnadc40jywy4xj5vi5kpj5g"))))
    (build-system asdf-build-system/sbcl)
    (synopsis "Random distributions for Common Lisp")
    (description
     "Manual translation from C to Common Lisp of some random number
generation functions from the GSL library.")
    (home-page "https://github.com/lvaruzza/cl-randist")
    (license license:bsd-2)
    (arguments
     `(#:asd-system-name "cl-randist"
       #:asd-file "cl-randist.asd"
       #:tests? #f))))

(define-public cl-randist
  (sbcl-package->cl-source-package sbcl-cl-randist))

(define-public ecl-cl-randist
  (sbcl-package->ecl-package sbcl-cl-randist))

(define-public sbcl-float-features
  (package
    (name "sbcl-float-features")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Shinmera/float-features.git")
             (commit "d3ef60181635b0849aa28cfc238053b7ca4644b0")))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0yj419k7n59x6rh3grwr6frgwwyria2il6f7wxpfazm8cskv4lzr"))))
    (build-system asdf-build-system/sbcl)
    (synopsis "Common Lisp IEEE float portability library")
    (description
     "Portability library for IEEE float features that are not
covered by the Common Lisp standard.")
    (home-page "https://github.com/Shinmera/float-features")
    (license license:zlib)
    (inputs
     `(("documentation-utils" ,sbcl-documentation-utils)))
    (arguments
     `(#:asd-system-name "float-features"
       #:asd-file "float-features.asd"
       #:tests? #f))))

(define-public cl-float-features
  (sbcl-package->cl-source-package sbcl-float-features))

(define-public ecl-float-features
  (sbcl-package->ecl-package sbcl-float-features))

(define-public sbcl-function-cache
  (package
    (name "sbcl-function-cache")
    (version "1.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AccelerationNet/function-cache.git")
             (commit "6a5ada401e57da2c8abf046f582029926e61fce8")))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "000vmd3f5rx5hs9nvphfric0gkzaadns31c6mxaslpv0k7pkrmc6"))))
    (build-system asdf-build-system/sbcl)
    (synopsis "Function caching / memoization library for Common Lisp")
    (description
     "A common lisp library that provides extensible function result
caching based on arguments (an expanded form of memoization).")
    (home-page "https://github.com/AccelerationNet/function-cache")
    (license
     (license:non-copyleft
      "https://github.com/AccelerationNet/function-cache/blob/master/README.md"))
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cl-interpol" ,sbcl-cl-interpol)
       ("iterate" ,sbcl-iterate)
       ("symbol-munger" ,sbcl-symbol-munger)
       ("closer-mop" ,sbcl-closer-mop)))
    (arguments
     `(#:asd-system-name "function-cache"
       #:asd-file "function-cache.asd"
       #:tests? #f))))

(define-public cl-function-cache
  (sbcl-package->cl-source-package sbcl-function-cache))

(define-public ecl-function-cache
  (sbcl-package->ecl-package sbcl-function-cache))

(define-public sbcl-type-r
  (let ((commit "83c89e38f2f7a7b16f1012777ecaf878cfa6a267")
        (revision "1"))
    (package
      (name "sbcl-type-r")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/guicho271828/type-r.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1arsxc2539rg8vbrdirz4xxj1b06mc6g6rqndz7a02g127qvk2sm"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Parser interface for Common Lisp built-in compound types")
      (description
       "Collections of accessor functions and patterns to access
the elements in compound type specifier, e.g. @code{dimensions} in
@code{(array element-type dimensions)}")
      (home-page "https://github.com/guicho271828/type-r")
      (license license:lgpl3+)
      (inputs
       `(("trivia" ,sbcl-trivia)
         ("alexandria" ,sbcl-alexandria)))
      (native-inputs
       `(("fiveam" ,sbcl-fiveam)))
      (arguments
       `(#:asd-system-name "type-r"
         #:asd-file "type-r.asd"
         #:test-asd-file "type-r.test.asd")))))

(define-public cl-type-r
  (sbcl-package->cl-source-package sbcl-type-r))

(define-public sbcl-trivialib-type-unify
  (let ((commit "62492ebf04db567dcf435ae84c50b7b8202ecf99")
        (revision "1"))
    (package
      (name "sbcl-trivialib-type-unify")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/guicho271828/trivialib.type-unify.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1bkyfzbwv75p50zp8n1n9rh2r29pw3vgz91gmn2gzzkyq3khj1vh"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Common Lisp type unification")
      (description
       "Unifies a parametrized type specifier against an actual type specifier.
Importantly, it handles complicated array-subtypes and number-related types
correctly.")
      (home-page "https://github.com/guicho271828/trivialib.type-unify")
      (license license:lgpl3+)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("trivia" ,sbcl-trivia)
         ("introspect-environment" ,sbcl-introspect-environment)
         ("type-r" ,sbcl-type-r)))
      (native-inputs
       `(("fiveam" ,sbcl-fiveam)))
      (arguments
       `(#:asd-system-name "trivialib.type-unify"
         #:asd-file "trivialib.type-unify.asd"
         #:test-asd-file "trivialib.type-unify.test.asd")))))

(define-public cl-trivialib-type-unify
  (sbcl-package->cl-source-package sbcl-trivialib-type-unify))

(define-public sbcl-specialized-function
  (let ((commit "b96b6afaf8358bf91cc0703e62a5a4ee20d2b7bc")
        (revision "1"))
    (package
      (name "sbcl-specialized-function")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/numcl/specialized-function.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "12j45ff0n26578vmfbhb9mfbdchw4wy023k0m2ppgl9s0z4bhjaj"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Julia-like dispatch for Common Lisp")
      (description
       "This library is part of NUMCL.  It provides a macro
@code{SPECIALIZED} that performs a Julia-like dispatch on the arguments,
lazily compiling a type-specific version of the function from the same
code.  The main target of this macro is speed.")
      (home-page "https://github.com/numcl/specialized-function")
      (license license:lgpl3+)
      (inputs
       `(("trivia" ,sbcl-trivia)
         ("alexandria" ,sbcl-alexandria)
         ("iterate" ,sbcl-iterate)
         ("lisp-namespace" ,sbcl-lisp-namespace)
         ("type-r" ,sbcl-type-r)
         ("trivial-cltl2" ,sbcl-trivial-cltl2)))
      (native-inputs
       `(("fiveam" ,sbcl-fiveam)))
      (arguments
       `(#:asd-system-name "specialized-function"
         #:asd-file "specialized-function.asd"
         #:test-asd-file "specialized-function.test.asd")))))

(define-public cl-specialized-function
  (sbcl-package->cl-source-package sbcl-specialized-function))

(define-public sbcl-constantfold
  (let ((commit "0ff1d97a3fbcb89264f6a2af6ce62b73e7b421f4")
        (revision "1"))
    (package
      (name "sbcl-constantfold")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/numcl/constantfold.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "153h0569z6bff1qbad0bdssplwwny75l7ilqwcfqfdvzsxf9jh06"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Support library for numcl")
      (description
       "Support library for numcl.  Registers a function as an
additional form that is considered as a candidate for a constant.")
      (home-page "https://github.com/numcl/constantfold")
      (license license:lgpl3+)
      (inputs
       `(("trivia" ,sbcl-trivia)
         ("alexandria" ,sbcl-alexandria)
         ("iterate" ,sbcl-iterate)
         ("lisp-namespace" ,sbcl-lisp-namespace)))
      (native-inputs
       `(("fiveam" ,sbcl-fiveam)))
      (arguments
       `(#:asd-system-name "constantfold"
         #:asd-file "constantfold.asd"
         #:test-asd-file "constantfold.test.asd")))))

(define-public cl-constantfold
  (sbcl-package->cl-source-package sbcl-constantfold))

(define-public sbcl-gtype
  (let ((commit "42275e3606242ae91e9c8dfa30c18ced50a35b66")
        (revision "1"))
    (package
      (name "sbcl-gtype")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/numcl/gtype.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1f56dba998v945jcxhha391557n6md1ql25b7icfwwfivhmlaa9b"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "C++/Julia-like parametric types in Common Lisp")
      (description
       "Support library for numcl that provides Julia-like runtime parametric
type correctness in Common Lisp.  It is based on CLtL2 extensions.")
      (home-page "https://github.com/numcl/gtype")
      (license license:lgpl3+)
      (inputs
       `(("trivialib.type-unify" ,sbcl-trivialib-type-unify)
         ("trivial-cltl2" ,sbcl-trivial-cltl2)
         ("trivia" ,sbcl-trivia)
         ("alexandria" ,sbcl-alexandria)
         ("iterate" ,sbcl-iterate)
         ("type-r" ,sbcl-type-r)))
      (native-inputs
       `(("fiveam" ,sbcl-fiveam)))
      (arguments
       `(#:asd-system-name "gtype"
         #:asd-file "gtype.asd"
         #:test-asd-file "gtype.test.asd")))))

(define-public cl-gtype
  (sbcl-package->cl-source-package sbcl-gtype))

(define-public sbcl-numcl
  (let ((commit "1cf7dfa59f763a24a501092870e9c5ee745d0c17")
        (revision "1"))
    (package
      (name "sbcl-numcl")
      (version (git-version "0.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/numcl/numcl.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0i3jby9hf4ii7blivgyza80g0vmjfhk8537i5i7kqqk0i5sdnym2"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Numpy clone in Common Lisp")
      (description
       "This is a Numpy clone in Common Lisp.  At the moment the
library is written in pure Common Lisp, focusing more on correctness
and usefulness, not speed.  Track the progress at
@url{https://github.com/numcl/numcl/projects/1}.")
      (home-page "https://github.com/numcl/numcl")
      (license license:lgpl3+)
      (inputs
       `(("trivia" ,sbcl-trivia)
         ("alexandria" ,sbcl-alexandria)
         ("iterate" ,sbcl-iterate)
         ("lisp-namespace" ,sbcl-lisp-namespace)
         ("type-r" ,sbcl-type-r)
         ("constantfold" ,sbcl-constantfold)
         ("cl-randist" ,sbcl-cl-randist)
         ("float-features" ,sbcl-float-features)
         ("function-cache" ,sbcl-function-cache)
         ("specialized-function" ,sbcl-specialized-function)
         ("gtype" ,sbcl-gtype)))
      (native-inputs
       `(("fiveam" ,sbcl-fiveam)))
      (arguments
       `(#:asd-system-name "numcl"
         #:asd-file "numcl.asd"
         #:test-asd-file "numcl.test.asd")))))

(define-public cl-numcl
  (sbcl-package->cl-source-package sbcl-numcl))

(define-public sbcl-pzmq
  (let ((commit "7c7390eedc469d033c72dc497984d1536ee75826")
        (revision "1"))
    (package
      (name "sbcl-pzmq")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/orivej/pzmq.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0gmwzf7h90wa7v4wnk49g0hv2mdalljpwhyigxcb967wzv8lqci9"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       `(("bordeaux-threads" ,sbcl-bordeaux-threads)
         ("fiveam" ,sbcl-fiveam)
         ("let-plus" ,sbcl-let-plus)))
      (inputs
       `(("cffi" ,sbcl-cffi)
         ("cffi-grovel" ,sbcl-cffi-grovel)
         ("zeromq" ,zeromq)))
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (add-after 'unpack 'fix-paths
                      (lambda* (#:key inputs #:allow-other-keys)
                        (substitute* "c-api.lisp"
                          (("\"libzmq")
                           (string-append "\""
                                          (assoc-ref inputs "zeromq")
                                          "/lib/libzmq")))
                        #t)))))
      (synopsis "Common Lisp bindings for the ZeroMQ library")
      (description "This Common Lisp library provides bindings for the ZeroMQ
lightweight messaging kernel.")
      (home-page "https://github.com/orivej/pzmq")
      (license license:unlicense))))

(define-public cl-pzmq
  (sbcl-package->cl-source-package sbcl-pzmq))

(define-public ecl-pzmq
  (sbcl-package->ecl-package sbcl-pzmq))

(define-public sbcl-clss
  (let ((revision "1")
        (commit "2a8e8615ab55870d4ca01928f3ed3bbeb4e75c8d"))
    (package
      (name "sbcl-clss")
      (version (git-version "0.3.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/Shinmera/clss.git")
           (commit commit)))
         (sha256
          (base32 "0la4dbcda78x29szanylccrsljqrn9d1mhh569sqkyp44ni5fv91"))
         (file-name (git-file-name name version))))
      (inputs
       `(("array-utils" ,sbcl-array-utils)
         ("plump" ,sbcl-plump)))
      (build-system asdf-build-system/sbcl)
      (synopsis "DOM tree searching engine based on CSS selectors")
      (description "CLSS is a DOM traversal engine based on CSS
selectors.  It makes use of the Plump-DOM and is used by lQuery.")
      (home-page "https://github.com/Shinmera/clss")
      (license license:zlib))))

(define-public cl-clss
  (sbcl-package->cl-source-package sbcl-clss))

(define-public ecl-clss
  (sbcl-package->ecl-package sbcl-clss))

(define-public sbcl-lquery
  (let ((revision "1")
        (commit "8048111c6b83956daa632e7a3ffbd8c9c203bd8d"))
    (package
      (name "sbcl-lquery")
      (version (git-version "3.2.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/Shinmera/lquery.git")
           (commit commit)))
         (sha256
          (base32 "0520mcpxc2d6fdm8z61arpgd2z38kan7cf06qs373n5r64rakz6w"))
         (file-name (git-file-name name version))))
      (native-inputs
       `(("fiveam" ,sbcl-fiveam)))
      (inputs
       `(("array-utils" ,sbcl-array-utils)
         ("form-fiddle" ,sbcl-form-fiddle)
         ("plump" ,sbcl-plump)
         ("clss" ,sbcl-clss)))
      (build-system asdf-build-system/sbcl)
      (synopsis "Library to allow jQuery-like HTML/DOM manipulation")
      (description "@code{lQuery} is a DOM manipulation library written in
Common Lisp, inspired by and based on the jQuery syntax and
functions.  It uses Plump and CLSS as DOM and selector engines.  The
main idea behind lQuery is to provide a simple interface for crawling
and modifying HTML sites, as well as to allow for an alternative
approach to templating.")
      (home-page "https://github.com/Shinmera/lquery")
      (license license:zlib))))

(define-public cl-lquery
  (sbcl-package->cl-source-package sbcl-lquery))

(define-public ecl-lquery
  (sbcl-package->ecl-package sbcl-lquery))

(define-public sbcl-cl-mysql
  (let ((commit "ab56c279c1815aec6ca0bfe85164ff7e85cfb6f9")
        (revision "1"))
    (package
      (name "sbcl-cl-mysql")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/hackinghat/cl-mysql.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0dg5ynx2ww94d0qfwrdrm7plkn43h64hs4iiq9mj2s1s4ixnp3lr"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       `(("stefil" ,sbcl-stefil)))
      (inputs
       `(("cffi" ,sbcl-cffi)
         ("mariadb-lib" ,mariadb "lib")))
      (arguments
       `(#:tests? #f ; TODO: Tests require a running server
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "system.lisp"
                 (("libmysqlclient_r" all)
                  (string-append (assoc-ref inputs "mariadb-lib")
                                 "/lib/"
                                 all)))
               #t)))))
      (synopsis "Common Lisp wrapper for MySQL")
      (description
       "@code{cl-mysql} is a Common Lisp implementation of a MySQL wrapper.")
      (home-page "http://www.hackinghat.com/index.php/cl-mysql")
      (license license:expat))))

(define-public cl-mysql
  (sbcl-package->cl-source-package sbcl-cl-mysql))

(define-public sbcl-simple-date
  (let ((commit "74469b25bbda990ec9b77e0d0eccdba0cd7e721a")
        (revision "1"))
    (package
      (name "sbcl-simple-date")
      (version (git-version "1.19" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/marijnh/Postmodern.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0im7ymnyxjhn2w74jfg76k5gpr0gl33n31akx33hl28722ljd0hd"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       `(("fiveam" ,sbcl-fiveam)))
      (synopsis "Basic date and time objects for Common Lisp")
      (description
       "@code{simple-date} is a very basic implementation of date and time
objects, used to support storing and retrieving time-related SQL types.")
      (home-page "https://marijnhaverbeke.nl/postmodern/")
      (license license:zlib))))

(define-public cl-simple-date
  (sbcl-package->cl-source-package sbcl-simple-date))

(define-public ecl-simple-date
  (sbcl-package->ecl-package sbcl-simple-date))

(define-public sbcl-cl-postgres
  (package
    (inherit sbcl-simple-date)
    (name "sbcl-cl-postgres")
    (native-inputs
     `(("fiveam" ,sbcl-fiveam)
       ("simple-date" ,sbcl-simple-date)))
    (inputs
     `(("md5" ,sbcl-md5)
       ("split-sequence" ,sbcl-split-sequence)
       ("usocket" ,sbcl-usocket)))
    (arguments
     `(#:tests? #f)) ; TODO: Break simple-date/postgres-glue circular dependency
    (synopsis "Common Lisp interface for PostgreSQL")
    (description
     "@code{cl-postgres} is a low-level library used for interfacing with
a PostgreSQL server over a socket.")))

(define-public cl-postgres
  (sbcl-package->cl-source-package sbcl-cl-postgres))

(define-public sbcl-simple-date-postgres-glue
  (package
    (inherit sbcl-simple-date)
    (name "sbcl-simple-date-postgres-glue")
    (inputs
     `(("cl-postgres" ,sbcl-cl-postgres)
       ("simple-date" ,sbcl-simple-date)))
    (arguments
     `(#:asd-file "simple-date.asd"
       #:asd-system-name "simple-date/postgres-glue"))))

(define-public cl-simple-date-postgres-glue
  (sbcl-package->cl-source-package sbcl-simple-date-postgres-glue))

(define-public sbcl-s-sql
  (package
    (inherit sbcl-simple-date)
    (name "sbcl-s-sql")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cl-postgres" ,sbcl-cl-postgres)))
    (arguments
     `(#:tests? #f)) ; TODO: Break postmodern circular dependency
    (synopsis "Lispy DSL for SQL")
    (description
     "@code{s-sql} is a Common Lisp library that can be used to compile
s-expressions to strings of SQL code, escaping any Lisp values inside, and
doing as much as possible of the work at compile time.")))

(define-public cl-s-sql
  (sbcl-package->cl-source-package sbcl-s-sql))

(define-public sbcl-postmodern
  (package
    (inherit sbcl-simple-date)
    (name "sbcl-postmodern")
    (native-inputs
     `(("fiveam" ,sbcl-fiveam)
       ("simple-date" ,sbcl-simple-date)
       ("simple-date-postgres-glue" ,sbcl-simple-date-postgres-glue)))
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("bordeaux-threads" ,sbcl-bordeaux-threads)
       ("cl-postgres" ,sbcl-cl-postgres)
       ("closer-mop" ,sbcl-closer-mop)
       ("global-vars" ,sbcl-global-vars)
       ("s-sql" ,sbcl-s-sql)
       ("split-sequence" ,sbcl-split-sequence)))
    (arguments
     ;; TODO: Fix missing dependency errors for simple-date/postgres-glue,
     ;; cl-postgres/tests and s-sql/tests.
     `(#:tests? #f))
    (synopsis "Common Lisp library for interacting with PostgreSQL")
    (description
     "@code{postmodern} is a Common Lisp library for interacting with
PostgreSQL databases.  It provides the following features:

@itemize
@item Efficient communication with the database server without need for
foreign libraries.
@item Support for UTF-8 on Unicode-aware Lisp implementations.
@item A syntax for mixing SQL and Lisp code.
@item Convenient support for prepared statements and stored procedures.
@item A metaclass for simple database-access objects.
@end itemize\n")))

(define-public cl-postmodern
  (sbcl-package->cl-source-package sbcl-postmodern))

(define-public sbcl-dbi
  (package
    (name "sbcl-dbi")
    (version "0.9.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fukamachi/cl-dbi.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0m1fsravfaizamdak84adh3jxc4j91maqnrb4m08bfbmb04cdqhs"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     `(("bordeaux-threads" ,sbcl-bordeaux-threads)
       ("closer-mop" ,sbcl-closer-mop)
       ("split-sequence" ,sbcl-split-sequence)))
    (arguments
     `(#:tests? #f)) ; TODO: Break circular dependency with dbd-*
    (synopsis "Database independent interface for Common Lisp")
    (description
     "@code{dbi} is a Common Lisp library providing a database independent
interface for MySQL, PostgreSQL and SQLite.")
    (home-page "https://github.com/fukamachi/cl-dbi")
    (license license:llgpl)))

(define-public cl-dbi
  (sbcl-package->cl-source-package sbcl-dbi))

(define-public sbcl-dbd-mysql
  (package
   (inherit sbcl-dbi)
   (name "sbcl-dbd-mysql")
   (inputs
    `(("cl-mysql" ,sbcl-cl-mysql)
      ("dbi" ,sbcl-dbi)))
   (synopsis "Database driver for MySQL")))

(define-public cl-dbd-mysql
  (sbcl-package->cl-source-package sbcl-dbd-mysql))

(define-public sbcl-dbd-postgres
  (package
   (inherit sbcl-dbi)
   (name "sbcl-dbd-postgres")
   (inputs
    `(("cl-postgres" ,sbcl-cl-postgres)
      ("dbi" ,sbcl-dbi)
      ("trivial-garbage" ,sbcl-trivial-garbage)))
   (synopsis "Database driver for PostgreSQL")))

(define-public cl-dbd-postgres
  (sbcl-package->cl-source-package sbcl-dbd-postgres))

(define-public sbcl-dbd-sqlite3
  (package
   (inherit sbcl-dbi)
   (name "sbcl-dbd-sqlite3")
   (inputs
    `(("cl-sqlite" ,sbcl-cl-sqlite)
      ("dbi" ,sbcl-dbi)
      ("trivial-garbage" ,sbcl-trivial-garbage)))
   (synopsis "Database driver for SQLite3")))

(define-public cl-dbd-sqlite3
  (sbcl-package->cl-source-package sbcl-dbd-sqlite3))

(define-public sbcl-uffi
  (package
    (name "sbcl-uffi")
    (version "2.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "http://git.kpe.io/uffi.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hqszvz0a3wk4s9faa83sc3vjxcb5rxmjclyr17yzwg55z733kry"))))
    (build-system asdf-build-system/sbcl)
    (arguments
     `(#:tests? #f ; TODO: Fix use of deprecated ASDF functions
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-permissions
           (lambda _
             (make-file-writable "doc/html.tar.gz")
             #t)))))
    (synopsis "Universal foreign function library for Common Lisp")
    (description
     "UFFI provides a universal foreign function interface (FFI)
 for Common Lisp.")
    (home-page "http://quickdocs.org/uffi/")
    (license license:llgpl)))

(define-public cl-uffi
  (package
    (inherit (sbcl-package->cl-source-package sbcl-uffi))
    (arguments
     `(#:phases
       ;; asdf-build-system/source has its own phases and does not inherit
       ;; from asdf-build-system/sbcl phases.
       (modify-phases %standard-phases/source
         (add-after 'unpack 'fix-permissions
           (lambda _
             (make-file-writable "doc/html.tar.gz")
             #t)))))))

(define-public sbcl-clsql
  (package
    (name "sbcl-clsql")
    (version "6.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "http://git.kpe.io/clsql.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1v1k3s5bsy3lgd9gk459bzpb1r0kdjda25s29samxw4gsgf1fqvp"))
       (snippet
        '(begin
           ;; Remove precompiled libraries.
           (delete-file "db-mysql/clsql_mysql.dll")
           (delete-file "uffi/clsql_uffi.dll")
           (delete-file "uffi/clsql_uffi.lib")
           #t))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     `(("cffi-uffi-compat" ,sbcl-cffi-uffi-compat)
       ("rt" ,sbcl-rt)
       ("uffi" ,sbcl-uffi)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-permissions
           (lambda _
             (make-file-writable "doc/html.tar.gz")
             #t))
         (add-after 'unpack 'fix-tests
           (lambda _
             (substitute* "clsql.asd"
               (("clsql-tests :force t")
                "clsql-tests"))
             #t)))))
    (synopsis "Common Lisp SQL Interface library")
    (description
     "@code{clsql} is a Common Lisp interface to SQL RDBMS based on the
Xanalys CommonSQL interface for Lispworks.  It provides low-level database
interfaces as well as a functional and an object oriented interface.")
    (home-page "http://clsql.kpe.io/")
    (license license:llgpl)))

(define-public cl-clsql
  (package
    (inherit (sbcl-package->cl-source-package sbcl-clsql))
    (native-inputs
     `(("rt" ,cl-rt)))
    (inputs
     `(("mysql" ,mysql)
       ("postgresql" ,postgresql)
       ("sqlite" ,sqlite)
       ("zlib" ,zlib)))
    (propagated-inputs
     `(("cl-postgres" ,cl-postgres)
       ("cffi-uffi-compat" ,cl-cffi-uffi-compat)
       ("md5" ,cl-md5)
       ("uffi" ,cl-uffi)))
    (arguments
     `(#:phases
       ;; asdf-build-system/source has its own phases and does not inherit
       ;; from asdf-build-system/sbcl phases.
       (modify-phases %standard-phases/source
         (add-after 'unpack 'fix-permissions
           (lambda _
             (make-file-writable "doc/html.tar.gz")
             #t)))))))

(define-public sbcl-clsql-uffi
  (package
    (inherit sbcl-clsql)
    (name "sbcl-clsql-uffi")
    (inputs
     `(("cffi-uffi-compat" ,sbcl-cffi-uffi-compat)
       ("clsql" ,sbcl-clsql)
       ("uffi" ,sbcl-uffi)))
    (synopsis "UFFI helper functions for Common Lisp SQL interface library")))

(define-public sbcl-clsql-sqlite3
  (package
    (inherit sbcl-clsql)
    (name "sbcl-clsql-sqlite3")
    (inputs
     `(("clsql" ,sbcl-clsql)
       ("clsql-uffi" ,sbcl-clsql-uffi)
       ("sqlite" ,sqlite)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-clsql)
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "db-sqlite3/sqlite3-loader.lisp"
                 (("libsqlite3")
                  (string-append (assoc-ref inputs "sqlite")
                                 "/lib/libsqlite3")))
               #t))))))
    (synopsis "SQLite3 driver for Common Lisp SQL interface library")))

(define-public sbcl-clsql-postgresql
  (package
    (inherit sbcl-clsql)
    (name "sbcl-clsql-postgresql")
    (inputs
     `(("clsql" ,sbcl-clsql)
       ("clsql-uffi" ,sbcl-clsql-uffi)
       ("postgresql" ,postgresql)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-clsql)
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "db-postgresql/postgresql-loader.lisp"
                 (("libpq")
                  (string-append (assoc-ref inputs "postgresql")
                                 "/lib/libpq")))
               #t))))))
    (synopsis "PostgreSQL driver for Common Lisp SQL interface library")))

(define-public sbcl-clsql-postgresql-socket3
  (package
    (inherit sbcl-clsql)
    (name "sbcl-clsql-postgresql-socket3")
    (inputs
     `(("cl-postgres" ,sbcl-cl-postgres)
       ("clsql" ,sbcl-clsql)
       ("md5" ,sbcl-md5)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-clsql)
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (add-after 'create-asd-file 'fix-asd-file
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (lib (string-append out "/lib/" (%lisp-type)))
                      (asd (string-append lib "/clsql-postgresql-socket3.asd")))
                 (substitute* asd
                   (("CLSQL-POSTGRESQL-SOCKET-SYSTEM::")
                    "")))
               #t))))))
    (synopsis "PostgreSQL driver for Common Lisp SQL interface library")))

(define-public sbcl-clsql-mysql
  (package
    (inherit sbcl-clsql)
    (name "sbcl-clsql-mysql")
    (inputs
     `(("mysql" ,mysql)
       ("sbcl-clsql" ,sbcl-clsql)
       ("sbcl-clsql-uffi" ,sbcl-clsql-uffi)
       ("zlib" ,zlib)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-clsql)
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((lib (string-append "#p\""
                                         (assoc-ref outputs "out")
                                         "/lib/\"")))
                 (substitute* "clsql-mysql.asd"
                   (("#p\"/usr/lib/clsql/clsql_mysql\\.so\"")
                    lib))
                 (substitute* "db-mysql/mysql-loader.lisp"
                   (("libmysqlclient" all)
                    (string-append (assoc-ref inputs "mysql") "/lib/" all))
                   (("clsql-mysql-system::\\*library-file-dir\\*")
                    lib)))
               #t))
           (add-before 'build 'build-helper-library
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((mysql (assoc-ref inputs "mysql"))
                      (inc-dir (string-append mysql "/include/mysql"))
                      (lib-dir (string-append mysql "/lib"))
                      (shared-lib-dir (string-append (assoc-ref outputs "out")
                                                     "/lib"))
                      (shared-lib (string-append shared-lib-dir
                                                 "/clsql_mysql.so")))
                 (mkdir-p shared-lib-dir)
                 (invoke "gcc" "-fPIC" "-shared"
                         "-I" inc-dir
                         "db-mysql/clsql_mysql.c"
                         "-Wl,-soname=clsql_mysql"
                         "-L" lib-dir "-lmysqlclient" "-lz"
                         "-o" shared-lib)
                 #t)))))))
    (synopsis "MySQL driver for Common Lisp SQL interface library")))

(define-public sbcl-sycamore
  (let ((commit "fd2820fec165ad514493426dea209728f64e6d18"))
    (package
      (name "sbcl-sycamore")
      (version "0.0.20120604")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ndantam/sycamore/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "00bv1aj89q5vldmq92zp2364jq312zjq2mbd3iyz1s2b4widzhl7"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:asd-file "src/sycamore.asd"))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("cl-ppcre" ,sbcl-cl-ppcre)))
      (synopsis "Purely functional data structure library in Common Lisp")
      (description
       "Sycamore is a fast, purely functional data structure library in Common Lisp.
If features:

@itemize
@item Fast, purely functional weight-balanced binary trees.
@item Leaf nodes are simple-vectors, greatly reducing tree height.
@item Interfaces for tree Sets and Maps (dictionaries).
@item Ropes.
@item Purely functional pairing heaps.
@item Purely functional amortized queue.
@end itemize\n")
      (home-page "http://ndantam.github.io/sycamore/")
      (license license:bsd-3))))

(define-public cl-sycamore
  (sbcl-package->cl-source-package sbcl-sycamore))

(define-public sbcl-trivial-package-local-nicknames
  (package
    (name "sbcl-trivial-package-local-nicknames")
    (version "0.2")
    (home-page "https://github.com/phoe/trivial-package-local-nicknames")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit "16b7ad4c2b120f50da65154191f468ea5598460e")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18qc27xkjzdcqrilpk3pm7djldwq5rm3ggd5h9cr8hqcd54i2fqg"))))
    (build-system asdf-build-system/sbcl)
    (synopsis "Common Lisp compatibility library for package local nicknames")
    (description
     "This library is a portable compatibility layer around package local nicknames (PLN).
This was done so there is a portability library for the PLN API not included
in DEFPACKAGE.")
    (license license:unlicense)))

(define-public cl-trivial-package-local-nicknames
  (sbcl-package->cl-source-package sbcl-trivial-package-local-nicknames))
