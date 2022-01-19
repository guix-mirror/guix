;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2018 Mark Meyer <mark@ofosos.org>
;;; Copyright © 2018 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2018 Fis Trivial <ybbs.daans@hotmail.com>
;;; Copyright © 2018 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2018 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;; Copyright © 2019 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2019, 2020 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2019 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 Konrad Hinsen <konrad.hinsen@fastmail.net>
;;; Copyright © 2020 Edouard Klein <edk@beaver-labs.com>
;;; Copyright © 2020, 2021, 2022 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2020, 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu packages machine-learning)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix svn-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system ocaml)
  #:use-module (guix build-system python)
  #:use-module (guix build-system r)
  #:use-module (guix git-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages dejagnu)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rpc)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (ice-9 match))

(define-public fann
  ;; The last release is >100 commits behind, so we package from git.
  (let ((commit "d71d54788bee56ba4cf7522801270152da5209d7"))
    (package
      (name "fann")
      (version (string-append "2.2.0-1." (string-take commit 8)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/libfann/fann")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "0ibwpfrjs6q2lijs8slxjgzb2llcl6rk3v2ski4r6215g5jjhg3x"))))
      (build-system cmake-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out")))
                 (with-directory-excursion (string-append (getcwd) "/tests")
                   (invoke "./fann_tests"))))))))
      (home-page "http://leenissen.dk/fann/wp/")
      (synopsis "Fast Artificial Neural Network")
      (description
       "FANN is a neural network library, which implements multilayer
artificial neural networks in C with support for both fully connected and
sparsely connected networks.")
      (license license:lgpl2.1))))

(define-public libsvm
  (package
    (name "libsvm")
    (version "3.23")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.csie.ntu.edu.tw/~cjlin/libsvm/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32 "0jpaq0rr92x38p4nk3gjan79ip67m6p80anb28z1d8601miysyi5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no "check" target
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'build 'build-lib
           (lambda _
             (invoke "make" "lib")))
         (replace 'install              ; no ‘install’ target
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/"))
                    (lib (string-append out "/lib/"))
                    (inc (string-append out "/include/libsvm")))
               (mkdir-p bin)
               (for-each (lambda (file)
                           (copy-file file (string-append bin file)))
                         '("svm-train"
                           "svm-predict"
                           "svm-scale"))
               (mkdir-p lib)
               (install-file "libsvm.so.2" lib)
               (mkdir-p inc)
               (install-file "svm.h" inc)))))))
    (home-page "https://www.csie.ntu.edu.tw/~cjlin/libsvm/")
    (synopsis "Library for Support Vector Machines")
    (description
     "LIBSVM is a machine learning library for support vector
classification, (C-SVC, nu-SVC), regression (epsilon-SVR, nu-SVR) and
distribution estimation (one-class SVM).  It supports multi-class
classification.")
    (license license:bsd-3)))

(define-public python-libsvm
  (package (inherit libsvm)
    (name "python-libsvm")
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no "check" target
       #:make-flags '("-C" "python")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace
          'install                      ; no ‘install’ target
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((site (string-append (assoc-ref outputs "out")
                                       "/lib/python"
                                       (string-take
                                        (string-take-right
                                         (assoc-ref inputs "python") 5) 3)
                                       "/site-packages/")))
              (substitute* "python/svm.py"
                (("../libsvm.so.2") "libsvm.so.2"))
              (mkdir-p site)
              (for-each (lambda (file)
                          (copy-file file (string-append site (basename file))))
                        (find-files "python" "\\.py"))
              (copy-file "libsvm.so.2"
                         (string-append site "libsvm.so.2")))
            #t)))))
    (inputs
     (list python))
    (synopsis "Python bindings of libSVM")))

(define-public ghmm
  ;; The latest release candidate is several years and a couple of fixes have
  ;; been published since.  This is why we download the sources from the SVN
  ;; repository.
  (let ((svn-revision 2341))
    (package
      (name "ghmm")
      (version (string-append "0.9-rc3-0." (number->string svn-revision)))
      (source (origin
                (method svn-fetch)
                (uri (svn-reference
                      (url "http://svn.code.sf.net/p/ghmm/code/trunk")
                      (revision svn-revision)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "0qbq1rqp94l530f043qzp8aw5lj7dng9wq0miffd7spd1ff638wq"))))
      (build-system gnu-build-system)
      (arguments
       `(#:imported-modules (,@%gnu-build-system-modules
                             (guix build python-build-system))
         #:modules          ((guix build python-build-system)
                             ,@%gnu-build-system-modules)
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'enter-dir
             (lambda _ (chdir "ghmm")))
           (add-after 'enter-dir 'fix-runpath
             (lambda* (#:key outputs #:allow-other-keys)
               (substitute* "ghmmwrapper/setup.py"
                 (("^(.*)extra_compile_args = \\[" line indent)
                  (string-append indent
                                 "extra_link_args = [\"-Wl,-rpath="
                                 (assoc-ref outputs "out") "/lib\"],\n"
                                 line
                                 "\"-Wl,-rpath="
                                 (assoc-ref outputs "out")
                                 "/lib\", ")))))
           (add-after 'enter-dir 'disable-broken-tests
             (lambda _
               (substitute* "tests/Makefile.am"
                 ;; GHMM_SILENT_TESTS is assumed to be a command.
                 (("TESTS_ENVIRONMENT.*") "")
                 ;; Do not build broken tests.
                 (("chmm .*") "")
                 (("read_fa .*") "")
                 (("mcmc .*") "")
                 (("label_higher_order_test.*$")
                  "label_higher_order_test\n"))

               ;; These Python unittests are broken as there is no gato.
               ;; See https://sourceforge.net/p/ghmm/support-requests/3/
               (substitute* "ghmmwrapper/ghmmunittests.py"
                 (("^(.*)def (testNewXML|testMultipleTransitionClasses|testNewXML)"
                   line indent)
                  (string-append indent
                                 "@unittest.skip(\"Disabled by Guix\")\n"
                                 line))))))))
      (inputs
       `(("python" ,python-2) ; only Python 2 is supported
         ("libxml2" ,libxml2)))
      (native-inputs
       (list pkg-config
             dejagnu
             swig
             autoconf
             automake
             libtool))
      (home-page "http://ghmm.org")
      (synopsis "Hidden Markov Model library")
      (description
       "The General Hidden Markov Model library (GHMM) is a C library with
additional Python bindings implementing a wide range of types of @dfn{Hidden
Markov Models} (HMM) and algorithms: discrete, continuous emissions, basic
training, HMM clustering, HMM mixtures.")
      (license license:lgpl2.0+))))

(define-public guile-aiscm
  (package
    (name "guile-aiscm")
    (version "0.23.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/wedesoft/aiscm")
                    (commit "c78b91edb7c17c6fbf3b294452f44e91d75e3c67")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09rdbcr8dinzijyx9h940ann91yjlbg0fangx365llhvy354n840"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags
      #~(list (string-append "GUILE_CACHE=" #$output "/lib/guile/3.0/site-ccache")
              (string-append "GUILE_EXT=" #$output "/lib/guile/3.0/extensions")
              (string-append "GUILE_SITE=" #$output "/share/guile/site/3.0"))
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'build-reproducibly
           (lambda _
             (substitute* "doc/Makefile.am"
               (("\\$\\(DATE\\)") "1970-01-01"))))
         (add-after 'unpack 'use-llvm-config
           (lambda _
             (substitute* "m4/ax_llvmc.m4"
               (("llvm-config-13") "llvm-config")
               ;; For some reason this library is not on the link list.
               (("(LLVM_LIBS=\"\\$\\(\\$ac_llvm_config_path --libs \\$1\\))\"" _ m)
                (string-append m " -lLLVMMCJIT\"")))

             ;; Because of this message:
             ;; symbol lookup error: ./.libs/libguile-aiscm-core.so: undefined symbol: LLVMInitializeX86TargetInfo
             ;; This probably needs to differ when building on architectures
             ;; other than x86_64p
             (substitute* "aiscm/Makefile.am"
               (("LLVM_LIBS\\)") "LLVM_LIBS) \
-lLLVMX86AsmParser -lLLVMX86CodeGen -lLLVMX86Desc -lLLVMX86Info"))))
         ;; Use Clang instead of GCC.
         (add-before 'configure 'prepare-build-environment
           (lambda _
             (setenv "AR" "llvm-ar")
             (setenv "NM" "llvm-nm")
             (setenv "CC" "clang")
             (setenv "CXX" "clang++"))))))
    (inputs
     (list ffmpeg
           freeglut
           guile-3.0
           imagemagick
           libjpeg-turbo
           libomp
           libxi
           libxmu
           libxpm
           libxt
           libxv
           mesa
           mjpegtools
           pandoc
           pulseaudio))
    (native-inputs
     (list clang-13
           llvm-13
           pkg-config
           autoconf
           automake
           gettext-minimal
           libtool
           which))
    (home-page "https://wedesoft.github.io/aiscm/")
    (synopsis "Guile extension for numerical arrays and tensors")
    (description "AIscm is a Guile extension for numerical arrays and tensors.
Performance is achieved by using the LLVM JIT compiler.")
    (license license:gpl3+)))

(define-public guile-aiscm-next
  (let ((commit "b17ed538c303badc419a7c358d91f266d2a8c354")
        (revision "1"))
    (package
      (inherit guile-aiscm)
      (name "guile-aiscm-next")
      (version (git-version "0.23.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/wedesoft/aiscm")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0px7r7lfskbp1prdrfrcvrsc4wjrk3ahkigsw4pqvny6zs7jnvc0"))))
      (arguments
       (substitute-keyword-arguments (package-arguments guile-aiscm)
         ((#:configure-flags flags '())
          #~(list (string-append "OPENCV_CFLAGS=-I" #$(this-package-input "opencv")
                                 "/include/opencv4")
                  (let ((modules
                         (list "aruco" "barcode" "bgsegm" "bioinspired"
                               "calib3d" "ccalib" "core" "datasets" "dnn"
                               "dnn_objdetect" "dnn_superres" "dpm" "face"
                               "features2d" "flann" "freetype" "fuzzy" "hdf"
                               "hfs" "highgui" "img_hash" "imgcodecs" "imgproc"
                               "intensity_transform" "line_descriptor" "mcc"
                               "ml" "objdetect" "optflow" "phase_unwrapping"
                               "photo" "plot" "quality" "rapid" "reg" "rgbd"
                               "saliency" "shape" "stereo" "stitching"
                               "structured_light" "superres" "surface_matching"
                               "text" "tracking" "video" "videoio" "videostab"
                               "wechat_qrcode" "ximgproc" "xobjdetect" "xphoto")))
                    (format #false "OPENCV_LIBS=~{-lopencv_~a~^ ~}" modules))))
         ((#:phases phases '%standard-phases)
          `(modify-phases ,phases
             (add-after 'unpack 'find-clearsilver
               (lambda* (#:key inputs #:allow-other-keys)
                 (substitute* "configure.ac"
                   (("/usr/local/include/ClearSilver")
                    (string-append (assoc-ref inputs "clearsilver")
                                   "/include/ClearSilver")))
                 (substitute* "aiscm/Makefile.am"
                   (("-lneo_utl" m)
                    (string-append m " -lstreamhtmlparser")))
                 (setenv "C_INCLUDE_PATH"
                         (string-append (assoc-ref inputs "clearsilver")
                                        "/include/ClearSilver:"
                                        (or (getenv "C_INCLUDE_PATH") "")))))
             ;; This test fails because our version of tensorflow is too old
             ;; to provide tf-string-length.
             (add-after 'unpack 'disable-broken-test
               (lambda _
                 (substitute* "tests/test_tensorflow.scm"
                   (("\\(test-eqv \"determine string length" m)
                    (string-append "#;" m)))))))))
      (inputs
       (modify-inputs (package-inputs guile-aiscm)
         (append clearsilver opencv tensorflow libgc)))
      (native-inputs
       (modify-inputs (package-native-inputs guile-aiscm)
         (append protobuf-c))))))

(define-public mcl
  (package
    (name "mcl")
    (version "14.137")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://micans.org/mcl/src/mcl-"
                    (string-replace-substring version "." "-")
                    ".tar.gz"))
              (sha256
               (base32
                "15xlax3z31lsn62vlg94hkm75nm40q4679amnfg13jm8m2bnhy5m"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list "--enable-blast"
                               "CFLAGS=-fcommon")))
    (inputs
     (list perl))
    (home-page "http://micans.org/mcl/")
    (synopsis "Clustering algorithm for graphs")
    (description
     "The MCL algorithm is short for the @dfn{Markov Cluster Algorithm}, a
fast and scalable unsupervised cluster algorithm for graphs (also known as
networks) based on simulation of (stochastic) flow in graphs.")
    ;; In the LICENCE file and web page it says "The software is licensed
    ;; under the GNU General Public License, version 3.", but in several of
    ;; the source code files it suggests GPL3 or later.
    ;; http://listserver.ebi.ac.uk/pipermail/mcl-users/2016/000376.html
    (license license:gpl3)))

(define-public ocaml-mcl
  (package
    (name "ocaml-mcl")
    (version "12-068oasis4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fhcrc/mcl")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0009dc3h2jp3qg5val452wngpqnbfyhbcxylghq0mrjqxx0jdq5p"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-paths
           (lambda _
             (substitute* "setup.ml"
               (("LDFLAGS=-fPIC")
                (string-append "LDFLAGS=-fPIC\"; \"SHELL=" (which "sh")))
               (("-std=c89") "-std=gnu99 -fcommon")

               ;; This is a mutable string, which is no longer supported.  Use
               ;; a byte buffer instead.
               (("String.make \\(String.length s\\)")
                "Bytes.make (String.length s)")

               ;; These two belong together.
               (("OASISString.replace_chars")
                "Bytes.to_string (OASISString.replace_chars")
               ((" s;")
                " s);"))
             (substitute* "myocamlbuild.ml"
               (("std=c89") "std=gnu99 -fcommon"))
             ;; Since we build with a more recent OCaml, we have to use C99 or
             ;; later.  This causes problems with the old C code.
             (substitute* "src/impala/matrix.c"
               (("restrict") "restrict_"))
             #t)))))
    (native-inputs
     (list ocamlbuild))
    (home-page "https://github.com/fhcrc/mcl")
    (synopsis "OCaml wrappers around MCL")
    (description
     "This package provides OCaml bindings for the MCL graph clustering
algorithm.")
    (license license:gpl3)))

(define-public randomjungle
  (package
    (name "randomjungle")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://www.imbs.uni-luebeck.de/fileadmin/files/Software"
             "/randomjungle/randomjungle-" version ".tar_.gz"))
       (patches (search-patches "randomjungle-disable-static-build.patch"))
       (sha256
        (base32
         "12c8rf30cla71swx2mf4ww9mfd8jbdw5lnxd7dxhyw1ygrvg6y4w"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--disable-static"
             (string-append "--with-boost="
                            (assoc-ref %build-inputs "boost")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-compatibility-errors
           (lambda _
             (substitute* "src/library/IAM2WayImportance.h"
               (("= std::make_pair.*")
                "= std::minmax(varID1, varID2);"))
             (substitute* "src/library/DataFrame.h"
               (("isFirst\\?.*")
                "if (isFirst) { isFirst = false; } else { os << par.delimiter; }\n"))))
         (add-before 'configure 'set-CXXFLAGS
           (lambda _ (setenv "CXXFLAGS" "-fpermissive "))))))
    (inputs
     (list boost gsl libxml2 zlib))
    (native-inputs
     `(("gfortran" ,gfortran)
       ("gfortran:lib" ,gfortran "lib")))
    ;; Non-portable assembly instructions are used so building fails on
    ;; platforms other than x86_64 or i686.
    (supported-systems '("x86_64-linux" "i686-linux"))
    (home-page "https://www.imbs.uni-luebeck.de/forschung/software/details.html#c224")
    (synopsis "Implementation of the Random Forests machine learning method")
    (description
     "Random Jungle is an implementation of Random Forests.  It is supposed to
analyse high dimensional data.  In genetics, it can be used for analysing big
Genome Wide Association (GWA) data.  Random Forests is a powerful machine
learning method.  Most interesting features are variable selection, missing
value imputation, classifier creation, generalization error estimation and
sample proximities between pairs of cases.")
    (license license:gpl3+)))

(define-public openfst
  (package
    (name "openfst")
    (version "1.7.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.openfst.org/twiki/pub/FST/"
                                  "FstDownload/openfst-" version ".tar.gz"))
              (sha256
               (base32
                "1pmx1yhn2gknj0an0zwqmzgwjaycapi896244np50a8y3nrsw6ck"))))
    (build-system gnu-build-system)
    (home-page "http://www.openfst.org")
    (synopsis "Library for weighted finite-state transducers")
    (description "OpenFst is a library for constructing, combining,
optimizing, and searching weighted finite-state transducers (FSTs).")
    (license license:asl2.0)))

(define-public shogun
  (package
    (name "shogun")
    (version "6.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "ftp://shogun-toolbox.org/shogun/releases/"
             (version-major+minor version)
             "/sources/shogun-" version ".tar.bz2"))
       (sha256
        (base32
         "1rn9skm3nw6hr7mr3lgp2gfqhi7ii0lyxck7qmqnf8avq349s5jp"))
       (modules '((guix build utils)
                  (ice-9 rdelim)))
       (snippet
        '(begin
           ;; Remove non-free sources and files referencing them
           (for-each delete-file
                     (find-files "src/shogun/classifier/svm/"
                                 "SVMLight\\.(cpp|h)"))
           (for-each delete-file
                     (find-files "examples/undocumented/libshogun/"
                                 (string-append
                                  "(classifier_.*svmlight.*|"
                                  "evaluation_cross_validation_locked_comparison).cpp")))
           ;; Remove non-free functions.
           (define (delete-ifdefs file)
             (with-atomic-file-replacement file
               (lambda (in out)
                 (let loop ((line (read-line in 'concat))
                            (skipping? #f))
                   (if (eof-object? line)
                       #t
                       (let ((skip-next?
                              (or (and skipping?
                                       (not (string-prefix?
                                             "#endif //USE_SVMLIGHT" line)))
                                  (string-prefix?
                                   "#ifdef USE_SVMLIGHT" line))))
                         (when (or (not skipping?)
                                   (and skipping? (not skip-next?)))
                           (display line out))
                         (loop (read-line in 'concat) skip-next?)))))))
           (for-each delete-ifdefs
                     (append
                      (find-files "src/shogun/classifier/mkl"
                                  "^MKLClassification\\.cpp")
                      (find-files "src/shogun/classifier/svm"
                                  "^SVMLightOneClass\\.(cpp|h)")
                      (find-files "src/shogun/multiclass"
                                  "^ScatterSVM\\.(cpp|h)")
                      (find-files "src/shogun/kernel/"
                                  "^(Kernel|CombinedKernel|ProductKernel)\\.(cpp|h)")
                      (find-files "src/shogun/regression/svr"
                                  "^(MKLRegression|SVRLight)\\.(cpp|h)")
                      (find-files "src/shogun/transfer/domain_adaptation"
                                  "^DomainAdaptationSVM\\.(cpp|h)")))
           #t))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f ;no check target
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'delete-broken-symlinks
           (lambda _
             (for-each delete-file '("applications/arts/data"
                                     "applications/asp/data"
                                     "applications/easysvm/data"
                                     "applications/msplicer/data"
                                     "applications/ocr/data"
                                     "examples/meta/data"
                                     "examples/undocumented/data"))
             #t))
         (add-after 'unpack 'change-R-target-path
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* '("src/interfaces/r/CMakeLists.txt"
                            "examples/meta/r/CMakeLists.txt")
               (("\\$\\{R_COMPONENT_LIB_PATH\\}")
                (string-append (assoc-ref outputs "out")
                               "/lib/R/library/")))
             #t))
         (add-after 'unpack 'fix-octave-modules
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "src/interfaces/octave/CMakeLists.txt"
               (("^include_directories\\(\\$\\{OCTAVE_INCLUDE_DIRS\\}")
                "include_directories(${OCTAVE_INCLUDE_DIRS} ${OCTAVE_INCLUDE_DIRS}/octave")
               ;; change target directory
               (("\\$\\{OCTAVE_OCT_LOCAL_API_FILE_DIR\\}")
                (string-append (assoc-ref outputs "out")
                               "/share/octave/packages")))
             (substitute* '("src/interfaces/octave/swig_typemaps.i"
                            "src/interfaces/octave/sg_print_functions.cpp")
               ;; "octave/config.h" and "octave/oct-obj.h" deprecated in Octave.
               (("octave/config\\.h") "octave/octave-config.h")
               (("octave/oct-obj.h") "octave/ovl.h"))
             #t))
         (add-after 'unpack 'move-rxcpp
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((rxcpp-dir "shogun/third-party/rxcpp"))
               (mkdir-p rxcpp-dir)
               (install-file (assoc-ref inputs "rxcpp") rxcpp-dir)
               #t)))
         (add-before 'build 'set-HOME
           ;; $HOME needs to be set at some point during the build phase
           (lambda _ (setenv "HOME" "/tmp") #t)))
       #:configure-flags
       (list "-DCMAKE_BUILD_WITH_INSTALL_RPATH=TRUE"
             "-DUSE_SVMLIGHT=OFF" ;disable proprietary SVMLIGHT
             "-DBUILD_META_EXAMPLES=OFF" ;requires unpackaged ctags
             ;;"-DINTERFACE_JAVA=ON" ;requires unpackaged jblas
             ;;"-DINTERFACE_RUBY=ON" ;requires unpackaged ruby-narray
             ;;"-DINTERFACE_PERL=ON" ;"FindPerlLibs" does not exist
             ;;"-DINTERFACE_LUA=ON"  ;fails because lua doesn't build pkgconfig file
             "-DINTERFACE_OCTAVE=ON"
             "-DINTERFACE_PYTHON=ON"
             "-DINTERFACE_R=ON")))
    (inputs
     `(("python" ,python)
       ("numpy" ,python-numpy)
       ("r-minimal" ,r-minimal)
       ("octave" ,octave-cli)
       ("swig" ,swig)
       ("eigen" ,eigen)
       ("hdf5" ,hdf5)
       ("atlas" ,atlas)
       ("arpack" ,arpack-ng)
       ("lapack" ,lapack)
       ("glpk" ,glpk)
       ("libxml2" ,libxml2)
       ("lzo" ,lzo)
       ("zlib" ,zlib)))
    (native-inputs
     (list pkg-config rxcpp))
    ;; Non-portable SSE instructions are used so building fails on platforms
    ;; other than x86_64.
    (supported-systems '("x86_64-linux"))
    (home-page "https://shogun-toolbox.org/")
    (synopsis "Machine learning toolbox")
    (description
     "The Shogun Machine learning toolbox provides a wide range of unified and
efficient Machine Learning (ML) methods.  The toolbox seamlessly
combines multiple data representations, algorithm classes, and general purpose
tools.  This enables both rapid prototyping of data pipelines and extensibility
in terms of new algorithms.")
    (license license:gpl3+)))

(define-public onnx
  (package
    (name "onnx")
    (version "1.9.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/onnx/onnx")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "1xnii361f68x0masxgfc4ai7hh3wlxxk56aznwf4m4yr6wqx47ml"))
              (file-name (git-file-name name version))
              (patches (search-patches "onnx-use-system-googletest.patch"
                                       "onnx-shared-libraries.patch"
                                       "onnx-skip-model-downloads.patch"))
              (modules '((guix build utils)))
              (snippet '(delete-file-recursively "third_party"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'build 'pass-cmake-arguments
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; Pass options to the CMake-based build process.
                      (define out
                        (assoc-ref outputs "out"))

                      (define args
                        ;; Copy arguments from 'cmake-build-system', plus ask
                        ;; for shared libraries.
                        (list "-DCMAKE_BUILD_TYPE=RelWithDebInfo"
                              (string-append "-DCMAKE_INSTALL_PREFIX=" out)
                              "-DCMAKE_INSTALL_LIBDIR=lib"
                              "-DCMAKE_INSTALL_RPATH_USE_LINK_PATH=TRUE"
                              (string-append "-DCMAKE_INSTALL_RPATH=" out
                                             "/lib")
                              "-DCMAKE_VERBOSE_MAKEFILE=ON"

                              "-DBUILD_SHARED_LIBS=ON"))

                      ;; This environment variable is honored by 'setup.py',
                      ;; which passes it down to 'cmake'.
                      (setenv "CMAKE_ARGS" (string-join args))

                      ;; This one is honored by 'setup.py' and passed to 'make
                      ;; -j'.
                      (setenv "MAX_JOBS"
                              (number->string (parallel-job-count)))))
                  (add-before 'check 'make-test-directory-writable
                    (lambda _
                      ;; Make things writable for tests.
                      (setenv "HOME" (getcwd))
                      (for-each make-file-writable
                                (find-files "onnx/examples" "."
                                            #:directories? #t))))
                  (add-after 'install 'install-from-cmake
                    (lambda _
                      ;; Run "make install" in the build tree 'setup.py'
                      ;; created for CMake so that libonnx.so,
                      ;; libonnx_proto.so, etc. are installed.
                      (invoke "make" "install"
                              "-C" ".setuptools-cmake-build"))))))
    (native-inputs
     (list cmake
           googletest
           pybind11
           python-coverage
           python-nbval
           python-pytest
           python-pytest-runner))
    (inputs
     (list protobuf))
    (propagated-inputs
     (list python-numpy python-protobuf python-six python-tabulate
           python-typing-extensions))
    (home-page "https://onnx.ai/")
    (synopsis "Open Neural Network Exchange")
    (description
     "Open Neural Network Exchange (ONNX) provides an open source format for
AI models, both deep learning and traditional ML.  It defines an extensible
computation graph model, as well as definitions of built-in operators and
standard data types.")
    (license license:expat)))

(define-public python-onnx
  ;; This used to be called "python-onnx" because it provided nothing but
  ;; Python bindings.  The package now provides shared libraries and C++
  ;; headers, hence the name change.
  (deprecated-package "python-onnx" onnx))

(define-public onnx-optimizer
  (package
    (name "onnx-optimizer")
    ;; Note: 0.2.x is *more* recent than 1.5.0.
    (version "0.2.6")
    (home-page "https://github.com/onnx/optimizer")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "1wkqqdxcxpfbf8zpbdfdd3zz5jkw775g31gyykj11z4y6pp659l6"))
              (file-name (git-file-name name version))
              (patches (search-patches "onnx-optimizer-system-library.patch"))
              (modules '((guix build utils)))
              (snippet '(delete-file-recursively "third_party"))))
    (build-system python-build-system)
    (arguments (package-arguments onnx))          ;reuse build system tweaks
    (native-inputs
     (list cmake python-pytest python-pytest-runner python-nbval
           python-coverage))
    (inputs
     (list onnx protobuf pybind11))
    (propagated-inputs
     (list python-numpy))
    (synopsis "Library to optimize ONNX models")
    (description
     "This package provides a C++ and Python library for performing arbitrary
optimizations on ONNX models, as well as a growing list of prepackaged
optimization passes.

Not all possible optimizations can be directly implemented on ONNX graphs---
some will need additional backend-specific information---but many can, and the
aim is to provide all such passes along with ONNX so that they can be re-used
with a single function call.")
    (license license:expat)))

(define-public rxcpp
  (package
    (name "rxcpp")
    (version "4.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ReactiveX/RxCpp")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1rdpa3jlc181jd08nk437aar085h28i45s6nzrv65apb3xyyz0ij"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-werror
           (lambda _
             (substitute* (find-files ".")
               (("-Werror") ""))
             #t))
         (replace 'check
           (lambda _
             (invoke "ctest"))))))
    (native-inputs
     (list catch-framework))
    (home-page "http://reactivex.io/")
    (synopsis "Reactive Extensions for C++")
    (description
     "The Reactive Extensions for C++ (RxCpp) is a library of algorithms for
values-distributed-in-time.  ReactiveX is a library for composing asynchronous
and event-based programs by using observable sequences.

It extends the observer pattern to support sequences of data and/or events and
adds operators that allow you to compose sequences together declaratively while
abstracting away concerns about things like low-level threading,
synchronization, thread-safety, concurrent data structures, and non-blocking
I/O.")
    (license license:asl2.0)))


(define-public gemmlowp
  (let ((commit "f9959600daa42992baace8a49544a00a743ce1b6")
        (version "0.1")
        (revision "1"))
    (package
      (name "gemmlowp")
      (version (git-version version revision commit))
      (home-page "https://github.com/google/gemmlowp")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1hzfhlhzcb827aza6a7drydc67dw5fm3qfqilb9ibskan8dsf0c6"))))
      (arguments
       `(#:configure-flags
         (list ,@(match (%current-system)
                   ((or "x86_64-linux" "i686-linux")
                    '("-DCMAKE_CXX_FLAGS=-msse2"))
                   (_ '())))
         #:phases
         (modify-phases %standard-phases
           ;; This directory contains the CMakeLists.txt.
           (add-after 'unpack 'chdir
             (lambda _ (chdir "contrib") #t))
           ;; There is no install target
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (lib (string-append out "/lib/"))
                      (inc (string-append out "/include/")))
                 (install-file "../build/libeight_bit_int_gemm.so" lib)
                 (for-each (lambda (dir)
                             (let ((target
                                    (string-append inc "/gemmlowp/" dir)))
                               (for-each (lambda (h)
                                           (install-file h target))
                                         (find-files (string-append "../" dir)
                                                     "\\.h$"))))
                           '("meta" "profiling" "public" "fixedpoint"
                             "eight_bit_int_gemm" "internal"))))))))
      (build-system cmake-build-system)
      (synopsis "Small self-contained low-precision GEMM library")
      (description
       "This is a small self-contained low-precision @dfn{general matrix
multiplication} (GEMM) library.  It is not a full linear algebra library.
Low-precision means that the input and output matrix entries are integers on
at most 8 bits.  To avoid overflow, results are internally accumulated on more
than 8 bits, and at the end only some significant 8 bits are kept.")
      (license license:asl2.0))))

(define-public gemmlowp-for-tensorflow
  ;; The commit hash is taken from "tensorflow/workspace.bzl".
  (let ((commit "38ebac7b059e84692f53e5938f97a9943c120d98")
        (revision "2"))
    (package
      (inherit gemmlowp)
      (version (git-version "0" revision commit))
      (source (origin
                (method url-fetch)
                (uri (string-append "https://mirror.bazel.build/"
                                    "github.com/google/gemmlowp/archive/"
                                    commit ".zip"))
                (file-name (string-append "gemmlowp-" version ".zip"))
                (sha256
                 (base32
                  "0n56s2g8hrssm4w8qj1v58gfm56a04n9v992ixkmvk6zjiralzxq"))))
      (arguments
       (substitute-keyword-arguments (package-arguments gemmlowp)
         ((#:phases phases)
          `(modify-phases ,phases
             (replace 'install
               (lambda* (#:key outputs #:allow-other-keys)
                 (let* ((out (assoc-ref outputs "out"))
                        (lib (string-append out "/lib/"))
                        (inc (string-append out "/include/")))
                   (install-file "../build/libeight_bit_int_gemm.so" lib)
                   (for-each (lambda (dir)
                               ;; Note: Install headers straight into
                               ;; $includedir instead of $includedir/gemmlowp.
                               (let ((target (string-append inc "/" dir)))
                                 (for-each (lambda (h)
                                             (install-file h target))
                                           (find-files (string-append "../" dir)
                                                       "\\.h$"))))
                             '("meta" "profiling" "public" "fixedpoint"
                               "eight_bit_int_gemm" "internal")))))))))
      (native-inputs
       (list unzip))
      (properties '((hidden? . #t))))))

(define-public dlib
  (package
    (name "dlib")
    (version "19.20")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://dlib.net/files/dlib-" version ".tar.bz2"))
              (sha256
               (base32
                "139jyi19qz37wwmmy48gil9d1kkh2r3w3bwdzabha6ayxmba96nz"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete ~13MB of bundled dependencies.
                  (delete-file-recursively "dlib/external")
                  (delete-file-recursively "docs/dlib/external")
                  #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DBUILD_SHARED_LIBS=ON")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-asserts
           (lambda _
             ;; config.h recommends explicitly enabling or disabling asserts
             ;; when building as a shared library. By default neither is set.
             (substitute* "dlib/config.h"
               (("^//#define DLIB_DISABLE_ASSERTS") "#define DLIB_DISABLE_ASSERTS"))
             #t))
         (add-after 'disable-asserts 'disable-failing-tests
           (lambda _
             ;; One test times out on MIPS, so we need to disable it.
             ;; Others are flaky on some platforms.
             (let* ((system ,(or (%current-target-system)
                                 (%current-system)))
                    (disabled-tests (cond
                                     ((string-prefix? "mips64" system)
                                      '("object_detector" ; timeout
                                        "data_io"))
                                     ((string-prefix? "armhf" system)
                                      '("learning_to_track"))
                                     ((string-prefix? "i686" system)
                                      '("optimization"))
                                     (else '()))))
               (for-each
                (lambda (test)
                  (substitute* "dlib/test/makefile"
                    (((string-append "SRC \\+= " test "\\.cpp")) "")))
                disabled-tests)
               #t)))
         (replace 'check
           (lambda _
             ;; No test target, so we build and run the unit tests here.
             (let ((test-dir (string-append "../dlib-" ,version "/dlib/test")))
               (with-directory-excursion test-dir
                 (invoke "make" "-j" (number->string (parallel-job-count)))
                 (invoke "./dtest" "--runall"))
               #t))))))
    (native-inputs
     (list pkg-config
           ;; For tests.
           libnsl))
    (inputs
     `(("giflib" ,giflib)
       ("lapack" ,lapack)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libx11" ,libx11)
       ("openblas" ,openblas)
       ("zlib" ,zlib)))
    (synopsis
     "Toolkit for making machine learning and data analysis applications in C++")
    (description
     "Dlib is a modern C++ toolkit containing machine learning algorithms and
tools.  It is used in both industry and academia in a wide range of domains
including robotics, embedded devices, mobile phones, and large high performance
computing environments.")
    (home-page "http://dlib.net")
    (license license:boost1.0)))

(define-public python-scikit-learn
  (package
    (name "python-scikit-learn")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/scikit-learn/scikit-learn")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1rli53544vlsnmx4v4xcb8fdqcy5n3zksl4plwp76gsmrppb2lig"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-ext
           (lambda _ (invoke "python" "setup.py" "build_ext" "--inplace")))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; Restrict OpenBLAS threads to prevent segfaults while testing!
               (setenv "OPENBLAS_NUM_THREADS" "1")

               ;; Some tests require write access to $HOME.
               (setenv "HOME" "/tmp")

               (invoke "pytest" "sklearn" "-m" "not network"
                       ;; This test tries to access the internet.
                       "-k" "not test_load_boston_alternative")))))))
    (inputs
     (list openblas))
    (native-inputs
     (list python-pytest python-pandas ;for tests
           python-cython))
    (propagated-inputs
     (list python-numpy python-threadpoolctl python-scipy python-joblib))
    (home-page "https://scikit-learn.org/")
    (synopsis "Machine Learning in Python")
    (description
     "Scikit-learn provides simple and efficient tools for data mining and
data analysis.")
    (properties `((python2-variant . ,(delay python2-scikit-learn))))
    (license license:bsd-3)))

;; scikit-learn 0.22 and later only supports Python 3, so we stick with
;; an older version here.
(define-public python2-scikit-learn
  (let ((base (package-with-python2 (strip-python2-variant python-scikit-learn))))
    (package
      (inherit base)
      (version "0.20.4")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/scikit-learn/scikit-learn")
                      (commit version)))
                (file-name (git-file-name "python-scikit-learn" version))
                (sha256
                 (base32
                  "08zbzi8yx5wdlxfx9jap61vg1malc9ajf576w7a0liv6jvvrxlpj"))))
      (arguments
       `(#:python ,python-2
         #:phases
         (modify-phases %standard-phases
           (add-after 'build 'build-ext
             (lambda _ (invoke "python" "setup.py" "build_ext" "--inplace")))
           (replace 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (when tests?
                 ;; Restrict OpenBLAS threads to prevent segfaults while testing!
                 (setenv "OPENBLAS_NUM_THREADS" "1")

                 ;; Some tests require write access to $HOME.
                 (setenv "HOME" "/tmp")

                 (invoke "pytest" "sklearn" "-m" "not network"
                         "-k"
                         (string-append
                          ;; This test tries to access the internet.
                          "not test_load_boston_alternative"
                          ;; This test fails for unknown reasons
                          " and not test_rank_deficient_design"))))))))
      (inputs
       (list openblas))
      (native-inputs
       (list python2-pytest python2-pandas ;for tests
             python2-cython))
      (propagated-inputs
       (list python2-numpy python2-scipy python2-joblib)))))

(define-public python-threadpoolctl
  (package
    (name "python-threadpoolctl")
    (version "2.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "threadpoolctl" version))
        (sha256
         (base32
          "0szsxcm2fbxrn83iynn42bnvrdh7mfsmkhfn8pdn7swblfb7rifx"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "pytest"))
             #t)))))
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/joblib/threadpoolctl")
    (synopsis "Python helpers for common threading libraries")
    (description "Thread-pool Controls provides Python helpers to limit the
number of threads used in the threadpool-backed of common native libraries used
for scientific computing and data science (e.g. BLAS and OpenMP).")
    (license license:bsd-3)))

(define-public python-pynndescent
  (package
    (name "python-pynndescent")
    (version "0.5.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pynndescent" version))
       (sha256
        (base32 "10pqqqc3jkpw03cyzy04slxmpgyhqnlgbyk0c1cv7kqr5d0zhzbs"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "python" "-m" "pytest" "--pyargs" "pynndescent")))))))
    (native-inputs
     (list python-pytest))
    (propagated-inputs
     (list python-joblib python-llvmlite python-numba python-scikit-learn
           python-scipy))
    (home-page "https://github.com/lmcinnes/pynndescent")
    (synopsis "Nearest neighbor descent for approximate nearest neighbors")
    (description
     "PyNNDescent provides a Python implementation of Nearest Neighbor Descent
for k-neighbor-graph construction and approximate nearest neighbor search.")
    (license license:bsd-2)))

(define-public python-opentsne
  (package
    (name "python-opentsne")
    (version "0.6.1")
    (source
     (origin
       (method git-fetch) ; no tests in PyPI release
       (uri (git-reference
             (url "https://github.com/pavlin-policar/openTSNE")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "124nid27lfq1ipfjd2gkynqcmb4khisjb4r05jv42ckfkk4dbsxs"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Benchmarks require the 'macosko2015' data files.
         (add-after 'unpack 'delete-benchmark
           (lambda _
             (delete-file-recursively "benchmarks")))
         (add-after 'unpack 'skip-test
           (lambda _ ;; TODO: figure out why this test fails.
             (substitute* "tests/test_correctness.py"
               (("def test_iris\\(self\\)") "def _test_iris(self)"))))
         ;; Numba needs a writable dir to cache functions.
         (add-before 'check 'set-numba-cache-dir
           (lambda _
             (setenv "NUMBA_CACHE_DIR" "/tmp"))))))
    (native-inputs
     (list python-cython))
    (inputs
     (list fftw))
    (propagated-inputs
     (list python-numpy python-pynndescent python-scikit-learn
           python-scipy))
    (home-page "https://github.com/pavlin-policar/openTSNE")
    (synopsis "Extensible, parallel implementations of t-SNE")
    (description
     "This is a modular Python implementation of t-Distributed Stochastic
Neighbor Embedding (t-SNE), a popular dimensionality-reduction algorithm for
visualizing high-dimensional data sets.")
    (license license:bsd-3)))

(define-public python-scikit-rebate
  (package
    (name "python-scikit-rebate")
    (version "0.62")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "skrebate" version))
              (sha256
               (base32
                "0n55ghvnv7rxqa5agq6a4892ad0ghha165b0g4ghwr9gqm6ss3dj"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))          ;no tests on PyPI and no tags in repo
    (propagated-inputs
     (list python-numpy python-scipy python-scikit-learn python-joblib))
    (home-page "https://epistasislab.github.io/scikit-rebate/")
    (synopsis "Relief-based feature selection algorithms for Python")
    (description "Scikit-rebate is a scikit-learn-compatible Python
implementation of ReBATE, a suite of Relief-based feature selection algorithms
for Machine Learning.  These algorithms excel at identifying features that are
predictive of the outcome in supervised learning problems, and are especially
good at identifying feature interactions that are normally overlooked by
standard feature selection algorithms.")
    (license license:expat)))

(define-public python-cmaes
  (package
    (name "python-cmaes")
    (version "0.8.2")
    (source
     (origin
       (method git-fetch) ;no tests in PyPI
       (uri (git-reference
             (url "https://github.com/CyberAgent/cmaes")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1jyckaifir528dz6m95nvky8hvqmz5gz6dlp65baahhbca0danzb"))
       (file-name (git-file-name name version))))
    (build-system python-build-system)
    (native-inputs
     (list python-setuptools ;build fails without this
           python-wheel))
    (propagated-inputs
     (list python-numpy))
    (home-page "https://github.com/CyberAgent/cmaes")
    (synopsis "CMA-ES implementation for Python")
    (description "This package provides provides an implementation of the
Covariance Matrix Adaptation Evolution Strategy (CMA-ES) for Python.")
    (license license:expat)))

(define-public python-autograd
  (let* ((commit "442205dfefe407beffb33550846434baa90c4de7")
         (revision "0")
         (version (git-version "0.0.0" revision commit)))
    (package
      (name "python-autograd")
      (home-page "https://github.com/HIPS/autograd")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (sha256
                 (base32
                  "189sv2xb0mwnjawa9z7mrgdglc1miaq93pnck26r28fi1jdwg0z4"))
                (file-name (git-file-name name version))))
      (version version)
      (build-system python-build-system)
      (native-inputs
       (list python-nose python-pytest))
      (propagated-inputs
       (list python-future python-numpy))
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (replace 'check
                      (lambda _
                        (invoke "py.test" "-v"))))))
      (synopsis "Efficiently computes derivatives of NumPy code")
      (description "Autograd can automatically differentiate native Python and
NumPy code.  It can handle a large subset of Python's features, including loops,
ifs, recursion and closures, and it can even take derivatives of derivatives
of derivatives.  It supports reverse-mode differentiation
(a.k.a. backpropagation), which means it can efficiently take gradients of
scalar-valued functions with respect to array-valued arguments, as well as
forward-mode differentiation, and the two can be composed arbitrarily.  The
main intended application of Autograd is gradient-based optimization.")
      (license license:expat))))

(define-public python2-autograd
  (package-with-python2 python-autograd))

(define-public lightgbm
  (package
    (name "lightgbm")
    (version "2.0.12")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/Microsoft/LightGBM")
                     (commit (string-append "v" version))))
              (sha256
               (base32
                "0jlvyn7k81dzrh9ij3zw576wbgiwmmr26rzpdxjn1dbpc3njpvzi"))
              (file-name (git-file-name name version))))
    (native-inputs
     (list python-pytest python-nose))
    (inputs
     (list openmpi))
    (propagated-inputs
     (list python-numpy python-scipy))
    (arguments
     `(#:configure-flags
       '("-DUSE_MPI=ON")
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (with-directory-excursion "../source"
               (invoke "pytest" "tests/c_api_test/test_.py")))))))
    (build-system cmake-build-system)
    (home-page "https://github.com/Microsoft/LightGBM")
    (synopsis "Gradient boosting framework based on decision tree algorithms")
    (description "LightGBM is a gradient boosting framework that uses tree
based learning algorithms.  It is designed to be distributed and efficient with
the following advantages:

@itemize
@item Faster training speed and higher efficiency
@item Lower memory usage
@item Better accuracy
@item Parallel and GPU learning supported (not enabled in this package)
@item Capable of handling large-scale data
@end itemize\n")
    (license license:expat)))

(define-public vowpal-wabbit
  ;; Language bindings not included.
  (package
    (name "vowpal-wabbit")
    (version "8.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/JohnLangford/vowpal_wabbit")
                     (commit version)))
              (sha256
               (base32
                "04bwzk6ifgnz3fmzid8b7avxf9n5pnx9xcjm61nkjng1vv0bpj8x"))
              (file-name (git-file-name name version))))
    (inputs
     (list boost zlib))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-boost="
                            (assoc-ref %build-inputs "boost")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-files-writable
           (lambda _
             (for-each make-file-writable (find-files "." ".*")) #t))
         (add-after 'install 'install-more-headers
           (lambda* (#:key outputs #:allow-other-keys)
             (for-each
               (lambda (file)
                 (install-file file (string-append
                                      (assoc-ref outputs "out")
                                      "/include/vowpalwabbit")))
               (find-files "vowpalwabbit" "\\.h$"))
             #t)))))
    (build-system gnu-build-system)
    (home-page "https://github.com/JohnLangford/vowpal_wabbit")
    (synopsis "Fast machine learning library for online learning")
    (description "Vowpal Wabbit is a machine learning system with techniques
such as online, hashing, allreduce, reductions, learning2search, active, and
interactive learning.")
    (license license:bsd-3)))

(define-public python2-fastlmm
  (package
    (name "python2-fastlmm")
    (version "0.2.21")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "fastlmm" version ".zip"))
       (sha256
        (base32
         "1q8c34rpmwkfy3r4d5172pzdkpfryj561897z9r3x22gq7813x1m"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; some test files are missing
       #:python ,python-2)) ; only Python 2.7 is supported
    (propagated-inputs
     (list python2-numpy
           python2-scipy
           python2-matplotlib
           python2-pandas
           python2-scikit-learn
           python2-pysnptools))
    (native-inputs
     (list unzip python2-cython python2-mock python2-nose))
    (home-page "http://research.microsoft.com/en-us/um/redmond/projects/mscompbio/fastlmm/")
    (synopsis "Perform genome-wide association studies on large data sets")
    (description
     "FaST-LMM, which stands for Factored Spectrally Transformed Linear Mixed
Models, is a program for performing both single-SNP and SNP-set genome-wide
association studies (GWAS) on extremely large data sets.")
    (license license:asl2.0)))

(define-public python-hyperopt
  (package
    (name "python-hyperopt")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hyperopt" version))
       (sha256
        (base32 "1k4ma8ci0bxghw7g4ms944zak1pi83yv2d6bxd7fcslm1zalfq5w"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "python" "-m" "pytest" "--ignore"
                       ;; Needs python-pyspark.
                       "hyperopt/tests/test_spark.py"
                       ;; Needs both python-scikit-learn and python-lightgbm.
                       "--ignore" "hyperopt/tests/test_atpe_basic.py"
                       ;; The tests below need python-lightgbm.
                       "-k" (string-append "not test_branin"
                                           " and not test_distractor"
                                           " and not test_q1lognormal"
                                           " and not test_quadratic1"
                                           " and not test_twoarms"))))))))
    (propagated-inputs
     (list python-cloudpickle
           python-future
           python-networkx
           python-numpy
           python-scipy
           python-six
           python-tqdm))
    (native-inputs
     (list python-black
           python-ipython
           python-ipyparallel
           python-nose
           python-pymongo
           python-pytest))
    (home-page "https://hyperopt.github.io/hyperopt/")
    (synopsis "Library for hyperparameter optimization")
    (description "Hyperopt is a Python library for serial and parallel
optimization over awkward search spaces, which may include real-valued,
discrete, and conditional dimensions.")
    (license license:bsd-3)))

;; There have been no proper releases yet.
(define-public kaldi
  (let ((commit "d4791c0f3fc1a09c042dac365e120899ee2ad21e")
        (revision "2"))
    (package
      (name "kaldi")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/kaldi-asr/kaldi")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "07k80my6f19mhrkwbzhjsnpf9871wmrwkl0ym468i830w67qyjrz"))))
      (build-system gnu-build-system)
      (arguments
       `(#:test-target "test"
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _ (chdir "src") #t))
           (replace 'configure
             (lambda* (#:key build system inputs outputs #:allow-other-keys)
               (when (not (or (string-prefix? "x86_64" system)
                              (string-prefix? "i686" system)))
                 (substitute* "makefiles/linux_openblas.mk"
                   (("-msse -msse2") "")))
               (substitute* "makefiles/default_rules.mk"
                 (("/bin/bash") (which "bash")))
               (substitute* "Makefile"
                 (("ext_depend: check_portaudio")
                  "ext_depend:"))
               (substitute* '("online/Makefile"
                              "onlinebin/Makefile"
                              "gst-plugin/Makefile")
                 (("../../tools/portaudio/install")
                  (assoc-ref inputs "portaudio")))

               ;; This `configure' script doesn't support variables passed as
               ;; arguments, nor does it support "prefix".
               (let ((out (assoc-ref outputs "out"))
                     (openblas (assoc-ref inputs "openblas"))
                     (openfst (assoc-ref inputs "openfst")))
                 (substitute* "configure"
                   (("check_for_slow_expf;") "")
                   ;; This affects the RPATH and also serves as the installation
                   ;; directory.
                   (("KALDILIBDIR=`pwd`/lib")
                    (string-append "KALDILIBDIR=" out "/lib")))
                 (mkdir-p out) ; must exist
                 (setenv "CONFIG_SHELL" (which "bash"))
                 (setenv "OPENFST_VER" ,(package-version openfst))
                 (invoke "./configure"
                         "--use-cuda=no"
                         "--shared"
                         (string-append "--openblas-root=" openblas)
                         (string-append "--fst-root=" openfst)))))
           (add-after 'build 'build-ext-and-gstreamer-plugin
             (lambda _
               (invoke "make" "-C" "online" "depend")
               (invoke "make" "-C" "online")
               (invoke "make" "-C" "onlinebin" "depend")
               (invoke "make" "-C" "onlinebin")
               (invoke "make" "-C" "gst-plugin" "depend")
               (invoke "make" "-C" "gst-plugin")
               #t))
           ;; TODO: also install the executables.
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (inc (string-append out "/include"))
                      (lib (string-append out "/lib")))
                 (mkdir-p lib)
                 ;; The build phase installed symlinks to the actual
                 ;; libraries.  Install the actual targets.
                 (for-each (lambda (file)
                             (let ((target (readlink file)))
                               (delete-file file)
                               (install-file target lib)))
                           (find-files lib "\\.so"))
                 ;; Install headers
                 (for-each (lambda (file)
                             (let ((target-dir (string-append inc "/" (dirname file))))
                               (install-file file target-dir)))
                           (find-files "." "\\.h"))
                 (install-file "gst-plugin/libgstonlinegmmdecodefaster.so"
                               (string-append lib "/gstreamer-1.0"))
                 #t))))))
      (inputs
       (list alsa-lib
             `(,gfortran "lib")
             glib
             gstreamer
             jack-1
             openblas
             openfst
             portaudio
             python))
      (native-inputs
       (list `(,glib "bin") ; glib-genmarshal
             grep
             sed
             pkg-config
             which))
      (home-page "https://kaldi-asr.org/")
      (synopsis "Speech recognition toolkit")
      (description "Kaldi is an extensible toolkit for speech recognition
written in C++.")
      (license license:asl2.0))))

(define-public gst-kaldi-nnet2-online
  (let ((commit "cb227ef43b66a9835c14eb0ad39e08ee03c210ad")
        (revision "2"))
    (package
      (name "gst-kaldi-nnet2-online")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/alumae/gst-kaldi-nnet2-online")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1i6ffwiavxx07ri0lxix6s8q0r31x7i4xxvhys5jxkixf5q34w8g"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ; there are none
         #:make-flags
         (list (string-append "SHELL="
                              (assoc-ref %build-inputs "bash") "/bin/bash")
               (string-append "KALDI_ROOT="
                              (assoc-ref %build-inputs "kaldi-src"))
               (string-append "KALDILIBDIR="
                              (assoc-ref %build-inputs "kaldi") "/lib")
               "KALDI_FLAVOR=dynamic")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _ (chdir "src") #t))
           (replace 'configure
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((glib (assoc-ref inputs "glib")))
                 (setenv "CXXFLAGS" "-fPIC")
                 (setenv "CPLUS_INCLUDE_PATH"
                         (string-append glib "/include/glib-2.0:"
                                        glib "/lib/glib-2.0/include:"
                                        (assoc-ref inputs "gstreamer")
                                        "/include/gstreamer-1.0")))
               (substitute* "Makefile"
                 (("include \\$\\(KALDI_ROOT\\)/src/kaldi.mk") "")
                 (("\\$\\(error Cannot find") "#"))
               #t))
           (add-before 'build 'build-depend
             (lambda* (#:key make-flags #:allow-other-keys)
               (apply invoke "make" "depend" make-flags)))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (lib (string-append out "/lib/gstreamer-1.0")))
                 (install-file "libgstkaldinnet2onlinedecoder.so" lib)
                 #t))))))
      (inputs
       (list glib gstreamer jansson openfst kaldi))
      (native-inputs
       `(("bash" ,bash)
         ("glib:bin" ,glib "bin")       ; glib-genmarshal
         ("kaldi-src" ,(package-source kaldi))
         ("pkg-config" ,pkg-config)))
      (home-page "https://kaldi-asr.org/")
      (synopsis "Gstreamer plugin for decoding speech")
      (description "This package provides a GStreamer plugin that wraps
Kaldi's @code{SingleUtteranceNnet2Decoder}.  It requires iVector-adapted DNN
acoustic models.  The iVectors are adapted to the current audio stream
automatically.")
      (license license:asl2.0))))

(define-public kaldi-gstreamer-server
  ;; This is the tip of the py3 branch
  (let ((commit "f68cab490be7eb0da2af1475fbc16655f50a60cb")
        (revision "2"))
    (package
      (name "kaldi-gstreamer-server")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/alumae/kaldi-gstreamer-server")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "17lh1368vkg8ngrcbn2phvigzlmalrqg6djx2gg61qq1a0nj87dm"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; there are no tests that can be run automatically
         #:modules ((guix build utils)
                    (guix build gnu-build-system)
                    (srfi srfi-26))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (replace 'build
             (lambda* (#:key outputs #:allow-other-keys)
               ;; Disable hash randomization to ensure the generated .pycs
               ;; are reproducible.
               (setenv "PYTHONHASHSEED" "0")
               (with-directory-excursion "kaldigstserver"
                 ;; See https://github.com/alumae/kaldi-gstreamer-server/issues/232
                 (substitute* "master_server.py"
                   (("\\.replace\\('\\\\.*") ")"))

                 ;; This is a Python 2 file
                 (delete-file "decoder_test.py")
                 (delete-file "test-buffer.py")

                 (for-each (lambda (file)
                             (apply invoke
                                    `("python"
                                      "-m" "compileall"
                                      "-f" ; force rebuild
                                      ,file)))
                           (find-files "." "\\.py$")))
               #t))
           (replace 'install
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin"))
                      (share (string-append out "/share/kaldi-gstreamer-server/")))
                 ;; Install Python files
                 (with-directory-excursion "kaldigstserver"
                   (for-each (cut install-file <> share)
                             (find-files "." ".*")))

                 ;; Install sample configuration files
                 (for-each (cut install-file <> share)
                           (find-files "." "\\.yaml"))

                 ;; Install executables
                 (mkdir-p bin)
                 (let* ((server (string-append bin "/kaldi-gst-server"))
                        (client (string-append bin "/kaldi-gst-client"))
                        (worker (string-append bin "/kaldi-gst-worker"))
                        (PYTHONPATH (getenv "GUIX_PYTHONPATH"))
                        (GST_PLUGIN_PATH (string-append
                                          (assoc-ref inputs "gst-kaldi-nnet2-online")
                                          "/lib/gstreamer-1.0:${GST_PLUGIN_PATH}"))
                        (wrap (lambda (wrapper what)
                                (with-output-to-file wrapper
                                  (lambda _
                                    (format #t
                                            "#!~a
export GUIX_PYTHONPATH=~a
export GST_PLUGIN_PATH=~a
exec ~a ~a/~a \"$@\"~%"
                                            (which "bash") PYTHONPATH GST_PLUGIN_PATH
                                            (which "python") share what)))
                                (chmod wrapper #o555))))
                   (for-each wrap
                             (list server client worker)
                             (list "master_server.py"
                                   "client.py"
                                   "worker.py")))
                 #t))))))
      (inputs
       `(("gst-kaldi-nnet2-online" ,gst-kaldi-nnet2-online)
         ("python" ,python-wrapper)
         ("python-pygobject" ,python-pygobject)
         ("python-pyyaml" ,python-pyyaml)
         ("python-tornado" ,python-tornado-6)))
      (home-page "https://github.com/alumae/kaldi-gstreamer-server")
      (synopsis "Real-time full-duplex speech recognition server")
      (description "This is a real-time full-duplex speech recognition server,
based on the Kaldi toolkit and the GStreamer framework and implemented in
Python.")
      (license license:bsd-2))))

;; Note that Tensorflow includes a "third_party" directory, which seems to not
;; only contain modified subsets of upstream library source code, but also
;; adapter headers provided by Google (such as the fft.h header, which is not
;; part of the upstream project code).  The Tensorflow code includes headers
;; from the "third_party" directory.  It does not look like we can replace
;; these headers with unmodified upstream files, so we keep them.
(define-public tensorflow
  (package
    (name "tensorflow")
    (version "1.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tensorflow/tensorflow")
             (commit (string-append "v" version))))
       (file-name (string-append "tensorflow-" version "-checkout"))
       (sha256
        (base32
         "0a9kwha395g3wgxfwln5j8vn9nkspmd75xldrlqdq540w996g8xa"))
       (patches
        (search-patches "tensorflow-c-api-fix.patch"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; no "check" target
       #:build-type "Release"
       #:configure-flags
       (let ((protobuf (assoc-ref %build-inputs "protobuf"))
             (protobuf:native (assoc-ref %build-inputs "protobuf:native"))
             (jsoncpp (assoc-ref %build-inputs "jsoncpp"))
             (snappy (assoc-ref %build-inputs "snappy"))
             (sqlite (assoc-ref %build-inputs "sqlite")))
         (list
          ;; Use protobuf from Guix
          (string-append "-Dprotobuf_STATIC_LIBRARIES="
                         protobuf "/lib/libprotobuf.so")
          (string-append "-DPROTOBUF_PROTOC_EXECUTABLE="
                         protobuf:native "/bin/protoc")

          ;; Use snappy from Guix
          (string-append "-Dsnappy_STATIC_LIBRARIES="
                         snappy "/lib/libsnappy.so")
          ;; Yes, this is not actually the include directory but a prefix...
          (string-append "-Dsnappy_INCLUDE_DIR=" snappy)

          ;; Use jsoncpp from Guix
          (string-append "-Djsoncpp_STATIC_LIBRARIES="
                         jsoncpp "/lib/libjsoncpp.so")
          ;; Yes, this is not actually the include directory but a prefix...
          (string-append "-Djsoncpp_INCLUDE_DIR=" jsoncpp)

          ;; Use sqlite from Guix
          (string-append "-Dsqlite_STATIC_LIBRARIES="
                         sqlite "/lib/libsqlite.a")

          ;; Use system libraries wherever possible.  Currently, this
          ;; only affects zlib.
          "-Dsystemlib_ALL=ON"
          "-Dtensorflow_ENABLE_POSITION_INDEPENDENT_CODE=ON"
          "-Dtensorflow_BUILD_SHARED_LIB=ON"
          "-Dtensorflow_OPTIMIZE_FOR_NATIVE_ARCH=OFF"
          "-Dtensorflow_ENABLE_SSL_SUPPORT=OFF"
          "-Dtensorflow_BUILD_CONTRIB_KERNELS=OFF"))
       #:make-flags
       (list "CC=gcc")
       #:modules ((ice-9 ftw)
                  (guix build utils)
                  (guix build cmake-build-system)
                  ((guix build python-build-system)
                   #:select (python-version)))
       #:imported-modules (,@%cmake-build-system-modules
                           (guix build python-build-system))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-source-file-times-to-1980
           ;; At the end of the tf_python_build_pip_package target, a ZIP
           ;; archive should be generated via bdist_wheel, but it fails with
           ;; "ZIP does not support timestamps before 1980".  Luckily,
           ;; SOURCE_DATE_EPOCH is respected, which we set to some time in
           ;; 1980.
           (lambda _ (setenv "SOURCE_DATE_EPOCH" "315532800") #t))
         (add-after 'unpack 'python3.9-compatibility
           (lambda _
             ;; See https://github.com/tensorflow/tensorflow/issues/20517#issuecomment-406373913
             (substitute* '("tensorflow/python/eager/pywrap_tfe_src.cc"
                            "tensorflow/python/lib/core/ndarray_tensor.cc"
                            "tensorflow/python/lib/core/py_func.cc")
               (("PyUnicode_AsUTF8") "(char *)PyUnicode_AsUTF8"))
             (substitute* "tensorflow/c/eager/c_api.h"
               (("unsigned char async")
                "unsigned char is_async"))

             ;; Remove dependency on tensorboard, a complicated but probably
             ;; optional package.
             (substitute* "tensorflow/tools/pip_package/setup.py"
               ((".*'tensorboard >.*") ""))

             ;; Fix the build with python-3.8, taken from rejected upstream patch:
             ;; https://github.com/tensorflow/tensorflow/issues/34197
             (substitute* (find-files "tensorflow/python" ".*\\.cc$")
               (("(nullptr,)(\\ +/. tp_print)" _ _ tp_print)
                (string-append "NULL,   " tp_print)))

             ;; Fix the build with numpy >= 1.19.
             ;; Suggested in https://github.com/tensorflow/tensorflow/issues/41086#issuecomment-656833081
             (substitute* "tensorflow/python/lib/core/bfloat16.cc"
               (("void BinaryUFunc\\(char\\*\\* args, npy_intp\\* dimensions, npy_intp\\* steps,")
                "void BinaryUFunc(char** args, npy_intp const* dimensions, npy_intp const* steps,")
               (("void CompareUFunc\\(char\\*\\* args, npy_intp\\* dimensions, npy_intp\\* steps,")
                "void CompareUFunc(char** args, npy_intp const* dimensions, npy_intp const* steps,"))))
         (add-after 'python3.9-compatibility 'chdir
           (lambda _ (chdir "tensorflow/contrib/cmake")))
         (add-after 'chdir 'disable-downloads
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* (find-files "external" "\\.cmake$")
               (("GIT_REPOSITORY.*") "")
               (("GIT_TAG.*") "")
               (("PREFIX ")
                "DOWNLOAD_COMMAND \"\"\nPREFIX "))

             ;; Use packages from Guix
             (let ((grpc (assoc-ref inputs "grpc")))
               (substitute* "CMakeLists.txt"
                 ;; Sqlite
                 (("include\\(sqlite\\)") "")
                 (("\\$\\{sqlite_STATIC_LIBRARIES\\}")
                  (search-input-file inputs "/lib/libsqlite3.so"))
                 (("sqlite_copy_headers_to_destination") "")

                 ;; PNG
                 (("include\\(png\\)") "")
                 (("\\$\\{png_STATIC_LIBRARIES\\}")
                  (search-input-file inputs "/lib/libpng16.so"))
                 (("png_copy_headers_to_destination") "")

                 ;; JPEG
                 (("include\\(jpeg\\)") "")
                 (("\\$\\{jpeg_STATIC_LIBRARIES\\}")
                  (search-input-file inputs "/lib/libjpeg.so"))
                 (("jpeg_copy_headers_to_destination") "")

                 ;; GIF
                 (("include\\(gif\\)") "")
                 (("\\$\\{gif_STATIC_LIBRARIES\\}")
                  (search-input-file inputs "/lib/libgif.so"))
                 (("gif_copy_headers_to_destination") "")

                 ;; lmdb
                 (("include\\(lmdb\\)") "")
                 (("\\$\\{lmdb_STATIC_LIBRARIES\\}")
                  (search-input-file inputs "/lib/liblmdb.so"))
                 (("lmdb_copy_headers_to_destination") "")

                 ;; Protobuf
                 (("include\\(protobuf\\)") "")
                 (("protobuf_copy_headers_to_destination") "")
                 (("^ +protobuf") "")

                 ;; gRPC
                 (("include\\(grpc\\)")
                  "find_package(grpc REQUIRED NAMES gRPC)")
                 (("list\\(APPEND tensorflow_EXTERNAL_DEPENDENCIES grpc\\)") "")

                 ;; Eigen
                 (("include\\(eigen\\)")
                  (string-append "find_package(eigen REQUIRED NAMES Eigen3)
set(eigen_INCLUDE_DIRS ${CMAKE_CURRENT_BINARY_DIR}/external/eigen_archive "
                                 (assoc-ref inputs "eigen") "/include/eigen3)"))
                 (("^ +eigen") "")

                 ;; snappy
                 (("include\\(snappy\\)")
                  "add_definitions(-DTF_USE_SNAPPY)")
                 (("list\\(APPEND tensorflow_EXTERNAL_DEPENDENCIES snappy\\)") "")

                 ;; jsoncpp
                 (("include\\(jsoncpp\\)") "")
                 (("^ +jsoncpp") ""))

               (substitute* "tf_core_framework.cmake"
                 ((" grpc") "")
                 (("\\$\\{GRPC_BUILD\\}/grpc_cpp_plugin")
                  (which "grpc_cpp_plugin"))
                 ;; Link with gRPC libraries
                 (("add_library\\(tf_protos_cc.*" m)
                  (string-append m
                                 (format #f "\ntarget_link_libraries(tf_protos_cc PRIVATE \
~a/lib/libgrpc++_unsecure.a \
~a/lib/libgrpc_unsecure.a \
~a/lib/libaddress_sorting.a \
~a/lib/libgpr.a \
~a//lib/libcares.so
)\n"
                                         grpc grpc grpc grpc
                                         (assoc-ref inputs "c-ares"))))))
             (substitute* "tf_tools.cmake"
               (("add_dependencies\\(\\$\\{proto_text.*") ""))
             ;; Remove dependency on bundled grpc
             (substitute* "tf_core_distributed_runtime.cmake"
               (("tf_core_cpu grpc") "tf_core_cpu"))

             ;; This directory is a dependency of many targets.
             (mkdir-p "protobuf")))
         (add-after 'configure 'unpack-third-party-sources
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; This is needed to configure bundled packages properly.
             (setenv "CONFIG_SHELL" (which "bash"))
             (for-each
              (lambda (name)
                (let* ((what  (assoc-ref inputs (string-append name "-src")))
                       (name* (string-map (lambda (c)
                                            (if (char=? c #\-)
                                                #\_ c)) name))
                       (where (string-append "../build/" name* "/src/" name*)))
                  (cond
                   ((string-suffix? ".zip" what)
                    (mkdir-p where)
                    (with-directory-excursion where
                      (invoke "unzip" what)))
                   ((string-suffix? ".tar.gz" what)
                    (mkdir-p where)
                    (invoke "tar" "xf" what
                            "-C" where "--strip-components=1"))
                   (else
                    (let ((parent (dirname where)))
                      (mkdir-p parent)
                      (with-directory-excursion parent
                        (when (file-exists? name*)
                          (delete-file-recursively name*))
                        (copy-recursively what name*)
                        (map make-file-writable
                             (find-files name* ".*"))))))))
              (list "boringssl"
                    "cub"
                    "double-conversion"
                    "farmhash"
                    "fft2d"
                    "highwayhash"
                    "nsync"
                    "re2"))

             (rename-file "../build/cub/src/cub/cub-1.8.0/"
                          "../build/cub/src/cub/cub/")

             (setenv "LDFLAGS"
                     (string-append "-Wl,-rpath="
                                    (assoc-ref outputs "out") "/lib"))))
         (add-after 'unpack 'fix-python-build
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (mkdir-p "protobuf-src")
             (invoke "tar" "xf" (assoc-ref inputs "protobuf:src")
                     "-C" "protobuf-src" "--strip-components=1")
             (mkdir-p "eigen-src")
             (copy-recursively (assoc-ref inputs "eigen:src") "eigen-src")

             (substitute* "tensorflow/contrib/cmake/tf_python.cmake"
               ;; Take protobuf source files from our source package.
               (("\\$\\{CMAKE_CURRENT_BINARY_DIR\\}/protobuf/src/protobuf/src/google")
                (string-append (getcwd) "/protobuf-src/src/google")))

             (substitute* '("tensorflow/contrib/cmake/tf_shared_lib.cmake"
                            "tensorflow/contrib/cmake/tf_python.cmake")
               ;; Take Eigen source files from our source package.
               (("\\$\\{CMAKE_CURRENT_BINARY_DIR\\}/eigen/src/eigen/")
                (string-append (getcwd) "/eigen-src/"))
               ;; Take Eigen headers from our own package.
               (("\\$\\{CMAKE_CURRENT_BINARY_DIR\\}/external/eigen_archive")
                (search-input-directory inputs "/include/eigen3")))

             ;; Correct the RUNPATH of ops libraries generated for Python.
             ;; TODO: this doesn't work :(
             ;; /gnu/store/...-tensorflow-1.9.0/lib/python3.7/site-packages/tensorflow/contrib/seq2seq/python/ops/lib_beam_search_ops.so:
             ;; warning: RUNPATH contains bogus entries: ("/tmp/guix-build-tensorflow-1.9.0.drv-0/source/tensorflow/contrib/build")
             ;; /gnu/store/...-tensorflow-1.9.0/lib/python3.7/site-packages/tensorflow/contrib/seq2seq/python/ops/lib_beam_search_ops.so:
             ;; error: depends on 'libpywrap_tensorflow_internal.so', which
             ;; cannot be found in RUNPATH ...
             (substitute* "tensorflow/contrib/cmake/tf_cc_ops.cmake"
               (("set_target_properties.*")
                (string-append "set_target_properties(${_AT_TARGET} PROPERTIES \
COMPILE_FLAGS ${target_compile_flags} \
INSTALL_RPATH_USE_LINK_PATH TRUE \
INSTALL_RPATH " (assoc-ref outputs "out") "/lib)\n")))))
         (add-after 'unpack 'patch-cmake-file-to-install-c-headers
           (lambda _
             (substitute* "tensorflow/contrib/cmake/tf_c.cmake"
               (("if\\(tensorflow_BUILD_PYTHON_BINDINGS" m)
                (string-append
                 "install(DIRECTORY ${tensorflow_source_dir}/tensorflow/c/ \
DESTINATION include/tensorflow/c FILES_MATCHING PATTERN \"*.h\")\n" m)))))
         (add-after 'build 'build-c-bindings
           (lambda* (#:key outputs parallel-build? #:allow-other-keys)
             (invoke "make" "-j" (if parallel-build?
                                     (number->string (parallel-job-count))
                                     "1")
                     "tf_c")))
         (add-after 'install 'build-pip-package
           (lambda* (#:key outputs parallel-build? #:allow-other-keys)
             (invoke "make" "-j" (if parallel-build?
                                     (number->string (parallel-job-count))
                                     "1")
                     "tf_python_build_pip_package")))
         (add-after 'build-pip-package 'install-python
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (wheel (car (find-files "../build/tf_python/dist/" "\\.whl$")))
                   (python-version (python-version
                                    (assoc-ref inputs "python"))))
               (invoke "python" "-m" "pip" "install" wheel
                       (string-append "--prefix=" out))

               ;; XXX: broken RUNPATH, see fix-python-build phase.
               (delete-file
                (string-append
                 out "/lib/python" python-version
                 "/site-packages/tensorflow/contrib/"
                 "seq2seq/python/ops/lib_beam_search_ops.so"))))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("protobuf:native" ,protobuf-3.6) ; protoc
       ("protobuf:src" ,(package-source protobuf-3.6))
       ("eigen:src" ,(package-source eigen-for-tensorflow))
       ;; install_pip_packages.sh wants setuptools 39.1.0 specifically.
       ("python-setuptools" ,python-setuptools-for-tensorflow)

       ;; The commit hashes and URLs for third-party source code are taken
       ;; from "tensorflow/workspace.bzl".
       ("boringssl-src"
        ,(let ((commit "ee7aa02")
               (revision "1"))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://boringssl.googlesource.com/boringssl")
                   (commit commit)))
             (file-name (string-append "boringssl-0-" revision
                                       (string-take commit 7)
                                       "-checkout"))
             (sha256
              (base32
               "1jf693q0nw0adsic6cgmbdx6g7wr4rj4vxa8j1hpn792fqhd8wgw")))))
       ("cub-src"
        ,(let ((version "1.8.0"))
           (origin
             (method url-fetch)
             (uri (string-append "https://mirror.bazel.build/github.com/NVlabs/"
                                 "cub/archive/" version ".zip"))
             (file-name (string-append "cub-" version ".zip"))
             (sha256
              (base32
               "1hsqikqridb90dkxkjr2918dcry6pfh46ccnwrzawl56aamhdykb")))))
       ("double-conversion-src"
        ,(let ((commit "5664746")
               (revision "1"))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/google/double-conversion")
                   (commit commit)))
             (file-name
              (git-file-name "double-conversion"
                             (string-append "0-" revision "."
                                            (string-take commit 7))))
             (sha256
              (base32
               "1h5lppqqxcvdg5jq42i5msgwx20ryij3apvmndflngrgdpc04gn1")))))
       ("farmhash-src"
        ,(let ((commit "816a4ae622e964763ca0862d9dbd19324a1eaf45"))
           (origin
             (method url-fetch)
             (uri (string-append
                   "https://mirror.bazel.build/github.com/google/farmhash/archive/"
                   commit ".tar.gz"))
             (file-name (string-append "farmhash-0-" (string-take commit 7)
                                       ".tar.gz"))
             (sha256
              (base32
               "185b2xdxl4d4cnsnv6abg8s22gxvx8673jq2yaq85bz4cdy58q35")))))
       ;; The license notice on the home page at
       ;; http://www.kurims.kyoto-u.ac.jp/~ooura/fft.html says:
       ;;   Copyright Takuya OOURA, 1996-2001
       ;;
       ;;   You may use, copy, modify and distribute this code for any purpose
       ;;   (include commercial use) and without fee. Please refer to this
       ;;   package when you modify this code.
       ;;
       ;; We take the identical tarball from the Bazel mirror, because the URL
       ;; at the home page is not versioned and might change.
       ("fft2d-src"
        ,(origin
           (method url-fetch)
           (uri "https://mirror.bazel.build/www.kurims.kyoto-u.ac.jp/~ooura/fft.tgz")
           (file-name "fft2d.tar.gz")
           (sha256
            (base32
             "15jjkfvhqvl2c0753d2di8hz0pyzn598g74wqy79awdrf1y67fsj"))))
       ("highwayhash-src"
        ,(let ((commit "be5edafc2e1a455768e260ccd68ae7317b6690ee")
               (revision "1"))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/google/highwayhash")
                   (commit commit)))
             (file-name (string-append "highwayhash-0-" revision
                                       (string-take commit 7)
                                       "-checkout"))
             (sha256
              (base32
               "154jwf98cyy54hldr94pgjn85zynly3abpnc1avmb8a18lzwjyb6")))))
       ("nsync-src"
        ,(let ((version "0559ce013feac8db639ee1bf776aca0325d28777")
               (revision "1"))
           (origin
             (method url-fetch)
             (uri (string-append "https://mirror.bazel.build/"
                                 "github.com/google/nsync/archive/"
                                 version ".tar.gz"))
             (file-name (string-append "nsync-0." revision
                                       "-" (string-take version 7)
                                       ".tar.gz"))
             (sha256
              (base32
               "0qdkyqym34x739mmzv97ah5r7ph462v5xkxqxvidmcfqbi64b132")))))
       ("re2-src"
        ,(let ((commit "e7efc48")
               (revision "1"))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/google/re2")
                   (commit commit)))
             (file-name (string-append "re2-0-" revision
                                       (string-take commit 7)
                                       "-checkout"))
             (sha256
              (base32
               "161g9841rjfsy5pn52fcis0s9hdr7rxvb06pad38j5rppfihvign")))))
       ("googletest" ,googletest)
       ("swig" ,swig)
       ("unzip" ,unzip)))
    (propagated-inputs
     `(("python-absl-py" ,python-absl-py)
       ("python-astor" ,python-astor)
       ("python-gast" ,python-gast)
       ("python-grpcio" ,python-grpcio)
       ("python-numpy" ,python-numpy)
       ("python-protobuf" ,python-protobuf-3.6)
       ("python-six" ,python-six)
       ("python-termcolo" ,python-termcolor)
       ("python-wheel" ,python-wheel)))
    (inputs
     `(("c-ares" ,c-ares)
       ("eigen" ,eigen-for-tensorflow)
       ("gemmlowp" ,gemmlowp-for-tensorflow)
       ("lmdb" ,lmdb)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("giflib" ,giflib)
       ("grpc" ,grpc-1.16.1 "static")
       ("grpc:bin" ,grpc-1.16.1)
       ("jsoncpp" ,jsoncpp-for-tensorflow)
       ("snappy" ,snappy)
       ("sqlite" ,sqlite)
       ("protobuf" ,protobuf-3.6)
       ("python" ,python-wrapper)
       ("zlib" ,zlib)))
    (home-page "https://tensorflow.org")
    (synopsis "Machine learning framework")
    (description
     "TensorFlow is a flexible platform for building and training machine
learning models.  It provides a library for high performance numerical
computation and includes high level Python APIs, including both a sequential
API for beginners that allows users to build models quickly by plugging
together building blocks and a subclassing API with an imperative style for
advanced research.")
    (license license:asl2.0)))

(define-public tensorflow-lite
  (package
    (name "tensorflow-lite")
    (version "2.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tensorflow/tensorflow")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1jdw2i1rq06zqd6aabh7bbm0avsg4pygnfmd7gviv0blhih9054l"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #false                  ; no "check" target
       #:build-type "Release"
       #:configure-flags
       (list
        "-DTFLITE_ENABLE_GPU=OFF"
        "-DTFLITE_ENABLE_RUY=OFF"

        ;; TODO: The build system attempts to build xnnpack from source.  We
        ;; would like to use our xnnpack package here, but this requires more
        ;; work.
        "-DTFLITE_ENABLE_XNNPACK=OFF"

        ;; Pretend we've already fetched abseil.  We won't actually build it
        ;; but use the existing package.
        "-Dabseil-cpp_POPULATED=TRUE"

        ;; Don't fetch the sources.  We have already built flatbuffers.
        "-Dflatbuffers_POPULATED=TRUE"

        "-DFFT2D_SOURCE_DIR=/tmp/fft2d"
        "-Dneon2sse_SOURCE_DIR=/tmp/neon2sse"
        "-Dneon2sse_BINARY_DIR=/tmp/neon2sse-bin"
        "-DFARMHASH_SOURCE_DIR=/tmp/farmhash"
        "-Dgemmlowp_SOURCE_DIR=/tmp/gemmlowp"
        (string-append "-DRUY_SOURCE_DIR="
                       (assoc-ref %build-inputs "ruy-src")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "tensorflow/lite")))
         (add-after 'chdir 'copy-sources
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Use external cmake finders instead of these stubs that won't
             ;; find anything but the bundled sources.
             (delete-file "tools/cmake/modules/Findabsl.cmake")
             (delete-file "tools/cmake/modules/Findeigen.cmake")

             (substitute* "CMakeLists.txt"
               (("find_package\\(eigen REQUIRED")
                "find_package(eigen REQUIRED NAMES Eigen3"))
             (substitute* "tools/cmake/modules/Findflatbuffers.cmake"
               (("get_target_property.*")
                (format #false "set(FLATBUFFERS_INCLUDE_DIRS ~a/include)\n"
                        (assoc-ref inputs "flatbuffers"))))

             ;; Don't fetch source code; we already have everything we need.
             (substitute* '("tools/cmake/modules/fft2d.cmake"
                            "tools/cmake/modules/ruy.cmake"
                            "tools/cmake/modules/farmhash.cmake"
                            "tools/cmake/modules/neon2sse.cmake"
                            "tools/cmake/modules/gemmlowp.cmake")
               (("OverridableFetchContent_Populate.*") ""))

             (mkdir-p "/tmp/farmhash")
             (with-directory-excursion "/tmp/farmhash"
               (invoke "tar" "--strip-components=1"
                       "-xf" (assoc-ref inputs "farmhash-src")))

             (mkdir-p "/tmp/fft2d")
             (with-directory-excursion "/tmp/fft2d"
               (invoke "tar" "--strip-components=1"
                       "-xf" (assoc-ref inputs "fft2d-src")))

             (copy-recursively (assoc-ref inputs "neon2sse-src")
                               "/tmp/neon2sse/")
             (copy-recursively (assoc-ref inputs "gemmlowp-src")
                               "/tmp/gemmlowp/")))
         (add-after 'copy-sources 'prepare-shared-library-build
           (lambda _ (chdir "c")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (headers (string-append out "/include/tensorflow/lite")))
               (install-file "../build/libtensorflowlite_c.so" lib)
               (with-directory-excursion ".."
                 (for-each
                  (lambda (file)
                    (let ((target-dir (string-append headers "/" (dirname file))))
                      (install-file file target-dir)))
                  (find-files "." "\\.h$")))))))))
    (inputs
     `(("abseil-cpp" ,abseil-cpp-20200923.3)
       ("eigen" ,eigen-for-tensorflow-lite)
       ("flatbuffers" ,flatbuffers)
       ("python" ,python)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gemmlowp-src"
        ;; The commit hash is taken from
        ;; "tensorflow/lite/tools/cmake/modules/gemmlowp.cmake".
        ,(let ((commit "fda83bdc38b118cc6b56753bd540caa49e570745"))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/google/gemmlowp")
                   (commit commit)))
             (file-name (git-file-name "gemmlowp" (string-take commit 8)))
             (sha256
              (base32
               "1sbp8kmr2azwlvfbzryy1frxi99jhsh1nc93bdbxdf8zdgpv0kxl")))))
       ("neon2sse-src"
        ,(let ((commit "a1652fd5253afbf3e39357b012974f93511f6108"))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/intel/ARM_NEON_2_x86_SSE")
                   (commit commit)))
             (file-name (git-file-name "neon2sse" (string-take commit 8)))
             (sha256
              (base32
               "1q8gkxag9wlnwdwad2pclsrkwzrdjy94hyrkayrsvxyj7szb5y8i")))))
       ("farmhash-src"
        ,(let ((commit "816a4ae622e964763ca0862d9dbd19324a1eaf45"))
           (origin
             (method url-fetch)
             (uri (string-append
                   "https://mirror.bazel.build/github.com/google/farmhash/archive/"
                   commit ".tar.gz"))
             (file-name (git-file-name "farmhash" (string-take commit 8)))
             (sha256
              (base32
               "185b2xdxl4d4cnsnv6abg8s22gxvx8673jq2yaq85bz4cdy58q35")))))
       ("fft2d-src"
        ,(origin
           (method url-fetch)
           (uri (string-append "https://storage.googleapis.com/"
                               "mirror.tensorflow.org/"
                               "www.kurims.kyoto-u.ac.jp/~ooura/fft2d.tgz"))
           (file-name "fft2d.tar.gz")
           (sha256
            (base32
             "1jfflzi74fag9z4qmgwvp90aif4dpbr1657izmxlgvf4hy8fk9xd"))))
       ("ruy-src"
        ,(let ((commit "9c56af3fce210a8a103eda19bd6f47c08a9e3d90"))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/google/ruy")
                   (commit commit)
                   (recursive? #true)))
             (file-name (git-file-name "ruy" (string-take commit 8)))
             (sha256
              (base32
               "1cfd5gk6kaj8kbl3h98gx1ap8czd59y6p8qq8nr28fklpyzf5cis")))))))
    (home-page "https://tensorflow.org")
    (synopsis "Machine learning framework")
    (description
     "TensorFlow is a flexible platform for building and training machine
learning models.  This package provides the \"lite\" variant for mobile
devices.")
    (license license:asl2.0)))

(define-public dmlc-core
  (package
    (name "dmlc-core")
    (version "0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dmlc/dmlc-core")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1x4ad1jhn84fywlk031fmv1kxyiscclmrqn9hhj8gz0mh7z9vcrh"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list "-DGOOGLE_TEST=ON")))
    (native-inputs
     `(("googletest" ,googletest)
       ("python" ,python-wrapper)))
    (home-page "https://github.com/dmlc/dmlc-core")
    (synopsis "Common bricks library for machine learning")
    (description
     "DMLC-Core is the backbone library to support all DMLC projects,
offers the bricks to build efficient and scalable distributed machine
learning libraries.")
    (license license:asl2.0)))

(define-public xgboost
  (package
    (name "xgboost")
    (version "1.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dmlc/xgboost")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (patches (search-patches "xgboost-use-system-dmlc-core.patch"))
       (sha256
        (base32 "0qx04y7cz8z7qv6bk9q7d7ba9b7xzj53l83l2x9ykdwhzacc3dn0"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags (list "-DGOOGLE_TEST=ON")))
    (native-inputs
     `(("googletest" ,googletest)
       ("python" ,python-wrapper)))
    (inputs
     (list dmlc-core))
    (home-page "https://xgboost.ai/")
    (synopsis "Gradient boosting (GBDT, GBRT or GBM) library")
    (description
     "XGBoost is an optimized distributed gradient boosting library designed
to be highly efficient, flexible and portable.  It implements machine learning
algorithms under the Gradient Boosting framework.  XGBoost provides a parallel
tree boosting (also known as GBDT, GBM) that solve many data science problems
in a fast and accurate way.")
    (license license:asl2.0)))

(define-public python-xgboost
  (package
    (inherit xgboost)
    (name "python-xgboost")
    (source (package-source xgboost))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'preparations
           (lambda _
             ;; Move python-package content to parent directory to silence
             ;; some warnings about files not being found if we chdir.
             (rename-file "python-package/xgboost" "xgboost")
             (rename-file "python-package/README.rst" "README.rst")
             (rename-file "python-package/setup.cfg" "setup.cfg")
             (rename-file "python-package/setup.py" "setup.py")
             ;; Skip rebuilding libxgboost.so.
             (substitute* "setup.py"
               (("ext_modules=\\[CMakeExtension\\('libxgboost'\\)\\],") "")
               (("'install_lib': InstallLib,") ""))))
         (add-after 'install 'install-version-and-libxgboost
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (pylib (string-append out "/lib/python"
                                          ,(version-major+minor
                                            (package-version python))
                                          "/site-packages"))
                    (xgbdir (string-append pylib "/xgboost"))
                    (version-file (string-append xgbdir "/VERSION"))
                    (libxgboost (string-append (assoc-ref inputs "xgboost")
                                               "/lib/libxgboost.so")))
               (with-output-to-file version-file
                 (lambda ()
                   (display ,(package-version xgboost))))
               (mkdir-p (string-append xgbdir "/lib"))
               (symlink libxgboost (string-append xgbdir "/lib"
                                                  "/libxgboost.so")))))
         (replace 'check
           ;; Python-specific tests are located in tests/python.
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "pytest" "tests/python"
                       ;; FIXME: CLI tests fail with PermissionError.
                       "--ignore" "tests/python/test_cli.py" "-k"
                       (string-append
                        "not test_cli_regression_demo"
                        ;; The tests below open a network connection.
                        " and not test_model_compatibility"
                        " and not test_get_group"
                        " and not test_cv_no_shuffle"
                        " and not test_cv"
                        " and not test_training"
                        ;; "'['./runexp.sh']' returned non-zero exit status 1"
                        " and not test_cli_binary_classification"))))))))
    (native-inputs
     (list python-pandas python-pytest python-scikit-learn))
    (inputs
     (list xgboost))
    (propagated-inputs
     (list python-numpy python-scipy))
    (synopsis "Python interface for the XGBoost library")))

(define-public python-iml
  (package
    (name "python-iml")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "iml" version))
       (sha256
        (base32
         "1k8szlpm19rcwcxdny9qdm3gmaqq8akb4xlvrzyz8c2d679aak6l"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-ipython python-numpy python-pandas python-scipy))
    (native-inputs
     (list python-nose))
    (home-page "https://github.com/interpretable-ml/iml")
    (synopsis "Interpretable Machine Learning (iML) package")
    (description "Interpretable ML (iML) is a set of data type objects,
visualizations, and interfaces that can be used by any method designed to
explain the predictions of machine learning models (or really the output of
any function).  It currently contains the interface and IO code from the Shap
project, and it will potentially also do the same for the Lime project.")
    (license license:expat)))

(define-public python-keras-applications
  (package
    (name "python-keras-applications")
    (version "1.0.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Keras_Applications" version))
       (sha256
        (base32
         "1rcz31ca4axa6kzhjx4lwqxbg4wvlljkj8qj9a7p9sfd5fhzjyam"))))
    (build-system python-build-system)
    ;; The tests require Keras, but this package is needed to build Keras.
    (arguments '(#:tests? #f))
    (propagated-inputs
     (list python-h5py python-numpy))
    (native-inputs
     (list python-pytest python-pytest-cov python-pytest-pep8
           python-pytest-xdist))
    (home-page "https://github.com/keras-team/keras-applications")
    (synopsis "Reference implementations of popular deep learning models")
    (description
     "This package provides reference implementations of popular deep learning
models for use with the Keras deep learning framework.")
    (license license:expat)))

(define-public python-keras-preprocessing
  (package
    (name "python-keras-preprocessing")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Keras_Preprocessing" version))
       (sha256
        (base32
         "1r98nm4k1svsqjyaqkfk23i31bl1kcfcyp7094yyj3c43phfp3as"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-numpy python-six))
    (native-inputs
     (list python-pandas
           python-pillow
           python-pytest
           python-pytest-cov
           python-pytest-xdist
           tensorflow))
    (home-page "https://github.com/keras-team/keras-preprocessing/")
    (synopsis "Data preprocessing and augmentation for deep learning models")
    (description
     "Keras Preprocessing is the data preprocessing and data augmentation
module of the Keras deep learning library.  It provides utilities for working
with image data, text data, and sequence data.")
    (license license:expat)))

(define-public python-keras
  (package
    (name "python-keras")
    (version "2.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Keras" version))
       (patches (search-patches "python-keras-integration-test.patch"))
       (sha256
        (base32
         "1j8bsqzh49vjdxy6l1k4iwax5vpjzniynyd041xjavdzvfii1dlh"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-tests-for-unavailable-features
           (lambda _
             (delete-file "keras/backend/theano_backend.py")
             (delete-file "keras/backend/cntk_backend.py")
             (delete-file "tests/keras/backend/backend_test.py")
             ;; FIXME: This doesn't work because Tensorflow is missing the
             ;; coder ops library.
             (delete-file "tests/keras/test_callbacks.py")))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; These tests attempt to download data files from the internet.
               (delete-file "tests/integration_tests/test_datasets.py")
               (delete-file "tests/integration_tests/imagenet_utils_test.py")
               ;; Backport https://github.com/keras-team/keras/pull/12479.
               (substitute* "tests/keras/engine/test_topology.py"
                 (("np.ones\\(\\(3, 2\\)\\)")
                  "1."))
               (invoke "python" "-m" "pytest" "tests"
                       "-p" "no:pep8"
                       ;; FIXME: python-build-system lacks PARALLEL-TESTS?
                       "-n" (number->string (parallel-job-count))
                       "-k"
                       (string-append
                        ;; The following test fails only in the build
                        ;; container; skip it.
                        "not test_selu "
                        ;; The following test was found flaky and removed in
                        ;; recent versions.
                        "and not test_stateful_metrics"))))))))
    (propagated-inputs
     (list python-h5py
           python-keras-applications
           python-keras-preprocessing
           python-numpy
           python-pydot
           python-pyyaml
           python-scipy
           python-six
           tensorflow
           graphviz))
    (native-inputs
     (list python-pandas
           python-pytest
           python-pytest-cov
           python-pytest-pep8
           python-pytest-timeout
           python-pytest-xdist
           python-sphinx
           python-requests))
    (home-page "https://github.com/keras-team/keras")
    (synopsis "High-level deep learning framework")
    (description "Keras is a high-level neural networks API, written in Python
and capable of running on top of TensorFlow.  It was developed with a focus on
enabling fast experimentation.  Use Keras if you need a deep learning library
that:
@itemize
@item Allows for easy and fast prototyping (through user friendliness,
  modularity, and extensibility).
@item Supports both convolutional networks and recurrent networks, as well as
  combinations of the two.
@item Runs seamlessly on CPU and GPU.
@end itemize\n")
    (license license:expat)))

(define-public gloo
  (let ((version "0.0.0") ; no proper version tag
        (commit "c22a5cfba94edf8ea4f53a174d38aa0c629d070f")
        (revision "1"))
    (package
      (name "gloo")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/facebookincubator/gloo")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1crmqgybzkgkpbmcx16912gsl5qsj49swa0ikx6mhqgph0chrh11"))))
      (build-system cmake-build-system)
      (native-inputs
       (list googletest))
      (inputs
       (list openssl))
      (arguments
       `(#:configure-flags '("-DBUILD_TEST=1")
         #:phases
         (modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (when tests?
                 (invoke "make" "gloo_test")))))))
      (synopsis "Collective communications library")
      (description
       "Gloo is a collective communications library.  It comes with a
number of collective algorithms useful for machine learning applications.
These include a barrier, broadcast, and allreduce.")
      (home-page "https://github.com/facebookincubator/gloo")
      (license license:bsd-3))))

(define-public python-umap-learn
  (package
    (name "python-umap-learn")
    (version "0.3.10")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "umap-learn" version))
       (sha256
        (base32
         "02ada2yy6km6zgk2836kg1c97yrcpalvan34p8c57446finnpki1"))))
    (build-system python-build-system)
    (native-inputs
     (list python-joblib python-nose))
    (propagated-inputs
     (list python-numba python-numpy python-scikit-learn python-scipy))
    (home-page "https://github.com/lmcinnes/umap")
    (synopsis
     "Uniform Manifold Approximation and Projection")
    (description
     "Uniform Manifold Approximation and Projection is a dimension reduction
technique that can be used for visualisation similarly to t-SNE, but also for
general non-linear dimension reduction.")
    (license license:bsd-3)))

(define-public nnpack
  (let ((version "0.0")
        (commit "c07e3a0400713d546e0dea2d5466dd22ea389c73")
        (revision "1"))
    (package
      (name "nnpack")
      (version (git-version version revision commit))
      (home-page "https://github.com/Maratyszcza/NNPACK")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0s0kk3a35w3yzf0q447p72350sbsh4qhg6vm3y2djbj4xpg7jc8v"))
                (patches (search-patches "nnpack-system-libraries.patch"))))
      (build-system cmake-build-system)
      ;; XXX: The test suite runs but it's very expensive, and on x86_64 CPUs
      ;; that lack the right ISA extensions, tests fail with:
      ;;
      ;; Expected equality of these values:
      ;;   nnp_status_success
      ;;     Which is: 0
      ;;   status
      ;;     Which is: 51
      ;;
      ;; where 51 is 'nnp_status_unsupported_hardware'.
      (arguments '(#:tests? #f))
      (synopsis "Acceleration package for neural network computations")
      (description
       "NNPACK is an acceleration package for neural network computations.
NNPACK aims to provide high-performance implementations of convnet layers for
multi-core CPUs.

NNPACK is not intended to be directly used by machine learning researchers;
instead it provides low-level performance primitives leveraged in leading deep
learning frameworks, such as PyTorch, Caffe2, MXNet, tiny-dnn, Caffe, Torch,
and Darknet.")
      (inputs
       (list cpuinfo
             fp16
             fxdiv
             psimd
             pthreadpool
             googletest))
      (native-inputs
       (list python python-peachpy python-six))
      (license license:bsd-2))))

(define-public xnnpack
  ;; There's currently no tag on this repo.
  (let ((version "0.0")
        (commit "bbe88243aba847f6a3dd86defec0fea4a0e415a1")
        (revision "1"))
    (package
      (name "xnnpack")
      (version (git-version version revision commit))
      (home-page "https://github.com/google/XNNPACK") ;fork of QNNPACK
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "19j605x1l2h95mjhcj90zwjh1153pdgmqggl35ya5w0wll628iiz"))
                (patches (search-patches "xnnpack-system-libraries.patch"))))
      (build-system cmake-build-system)
      (arguments
       '(#:configure-flags '("-DXNNPACK_USE_SYSTEM_LIBS=YES"
                             "-DBUILD_SHARED_LIBS=ON"
                             "-DXNNPACK_LIBRARY_TYPE=shared"
                             "-DXNNPACK_BUILD_TESTS=FALSE" ;FIXME: see below
                             "-DXNNPACK_BUILD_BENCHMARKS=FALSE")

         ;; FIXME: Building tests leads to a CMake error:
         ;;
         ;;   ADD_LIBRARY cannot create target "all_microkernels" because
         ;;   another target with the same name already exists.
         #:tests? #f))
      (inputs
       (list cpuinfo
             pthreadpool
             googletest
             googlebenchmark
             fxdiv
             fp16
             psimd))
      (synopsis "Optimized floating-point neural network inference operators")
      (description
       "XNNPACK is a highly optimized library of floating-point neural network
inference operators for ARM, WebAssembly, and x86 platforms.  XNNPACK is not
intended for direct use by deep learning practitioners and researchers;
instead it provides low-level performance primitives for accelerating
high-level machine learning frameworks, such as TensorFlow Lite,
TensorFlow.js, PyTorch, and MediaPipe.")
      (license license:bsd-3))))

(define-public python-pytorch
  (package
    (name "python-pytorch")
    (version "1.10.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pytorch/pytorch")
                    (commit (string-append "v" version))
                    (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ihsjw48qqbikmhxxn17bcdvk2zsjabvkq61q6pvj7dzvrdpkb60"))
              (patches (search-patches "python-pytorch-system-libraries.patch"
                                       "python-pytorch-runpath.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; XXX: Let's be clear: this package is a bundling fest.  We
                  ;; delete as much as we can, but there's still a lot left.
                  (for-each (lambda (directory)
                              (delete-file-recursively
                               (string-append "third_party/" directory)))
                            '("benchmark" "cpuinfo" "eigen"

                              ;; FIXME: QNNPACK (of which XNNPACK is a fork)
                              ;; needs these.
                              ;; "FP16" "FXdiv" "gemmlowp" "psimd"

                              "gloo" "googletest" "ios-cmake" "NNPACK"
                              "onnx" "protobuf" "pthreadpool"
                              "pybind11" "python-enum" "python-peachpy"
                              "python-six" "tbb" "XNNPACK" "zstd"))

                  ;; Adjust references to the onnx-optimizer headers.
                  (substitute* "caffe2/onnx/backend.cc"
                    (("onnx/optimizer/")
                     "onnxoptimizer/"))))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'build 'use-system-libraries
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; Tell 'setup.py' to let 'CMakeLists.txt' know that we
                      ;; want to use "system libraries" instead of the bundled
                      ;; ones.
                      (setenv "USE_SYSTEM_LIBS" "1")

                      ;; XXX: Disable that for simplicity for now.
                      (setenv "USE_FBGEMM" "0")))
                  (add-before 'build 'make-things-writable
                    (lambda _
                      ;; The 'build_caffe2' function in
                      ;; 'tools/build_pytorch_libs.py', called from the
                      ;; top-level 'setup.py', needs write access to this
                      ;; directory.
                      (for-each make-file-writable
                                (find-files "caffe2/proto" "."
                                            #:directories? #t))))
                  (replace 'check
                    (lambda* (#:key inputs outputs tests? #:allow-other-keys)
                      ;; Run the test suite following the instructions in
                      ;; 'CONTRIBUTING.md'.  XXX: Unfortunately this doesn't
                      ;; work, unless you set GUIX_PYTHONPATH presumably.
                      (when tests?
                        (add-installed-pythonpath inputs outputs)
                        (invoke "python" "test/run_test.py"))))
                  (add-after 'install 'remove-test-executables
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      ;; Remove test executables, but keep other executables
                      ;; such as 'torch_shm_manager' and and .so files such as
                      ;; 'libtorch_global_deps.so'.
                      (let ((python-site (site-packages inputs outputs)))
                        (for-each delete-file
                                  (find-files python-site
                                              "(^test_cpp_rpc|_test)$"))))))

       ;; XXX: Tests attempt to download data such as
       ;; <https://raw.githubusercontent.com/pytorch/test-infra/master/stats/slow-tests.json>.
       ;; We're also missing some Python modules, such as expecttest.
       #:tests? #f))
    (native-inputs
     (list cmake ninja))
    (inputs
     (list eigen
           ;; ("fmt" ,fmt)
           fp16
           gemmlowp
           googletest
           googlebenchmark
           gloo
           nnpack
           openblas
           openmpi
           pthreadpool
           protobuf
           pybind11
           sleef
           xnnpack
           zstd))
    (propagated-inputs
     (list python-astunparse
           python-click
           python-numpy
           python-pyyaml
           python-cffi
           python-typing-extensions
           python-future
           python-six
           python-requests
           onnx ;propagated for its Python modules
           onnx-optimizer
           cpuinfo))
    (home-page "https://pytorch.org/")
    (synopsis "Python library for tensor computation and deep neural networks")
    (description
     "PyTorch is a Python package that provides two high-level features:

@itemize
@item tensor computation (like NumPy) with strong GPU acceleration;
@item deep neural networks (DNNs) built on a tape-based autograd system.
@end itemize

You can reuse Python packages such as NumPy, SciPy, and Cython to extend
PyTorch when needed.

Note: currently this package does not provide GPU support.")
    (license license:bsd-3)))

(define-public python-hmmlearn
  (package
    (name "python-hmmlearn")
    (version "0.2.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hmmlearn" version))
       (sha256
        (base32
         "1my0j3rzp17438idr32ssh0j969a98yjblx5igx5kgiiigr9qa1a"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (with-directory-excursion (string-append (assoc-ref outputs "out") "/lib")
                 (invoke "python" "-m" "pytest"))))))))
    (propagated-inputs
     (list python-cython python-numpy python-scikit-learn python-scipy
           python-setuptools-scm))
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/hmmlearn/hmmlearn")
    (synopsis "Hidden Markov Models with scikit-learn like API")
    (description
     "Hmmlearn is a set of algorithms for unsupervised learning and inference
of Hidden Markov Models.")
    (license license:bsd-3)))
