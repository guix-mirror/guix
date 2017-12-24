;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Vicente Vera Parra <vicentemvp@gmail.com>
;;; Copyright © 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2016 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2016, 2017 Raoul Bonnal <ilpuccio.febo@gmail.com>
;;; Copyright © 2017 Kyle Meyer <kyle@kyleam.com>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages statistics)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix hg-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system r)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages java)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages base)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-1))


(define-public pspp
  (package
    (name "pspp")
    (version "1.0.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/pspp/pspp-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1r8smr5057993h90nx0mdnff8nxw9x546zzh6qpy4h3xblp1la5s"))))
    (build-system gnu-build-system)
    (inputs
     `(("cairo" ,cairo)
       ("gettext" ,gettext-minimal)
       ("gsl" ,gsl)
       ("libxml2" ,libxml2)
       ("pango" ,pango)
       ("readline" ,readline)
       ("gtk" ,gtk+)
       ("gtksourceview" ,gtksourceview)
       ("zlib" ,zlib)))
    (native-inputs
     `(("glib" ,glib "bin")             ;for glib-genmarshal
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (home-page "https://www.gnu.org/software/pspp/")
    (synopsis "Statistical analysis")
    (description
     "GNU PSPP is a statistical analysis program.  It can perform
descriptive statistics, T-tests, linear regression and non-parametric tests.
It features both a graphical interface as well as command-line input.  PSPP
is designed to interoperate with Gnumeric, LibreOffice and OpenOffice.  Data
can be imported from spreadsheets, text files and database sources and it can
be output in text, PostScript, PDF or HTML.")
    (license license:gpl3+)))

;; Update this package together with the set of recommended packages: r-boot,
;; r-class, r-cluster, r-codetools, r-foreign, r-kernsmooth, r-lattice,
;; r-mass, r-matrix, r-mgcv, r-nlme, r-nnet, r-rpart, r-spatial, r-survival.
(define-public r-minimal
  (package
    (name "r-minimal")
    (version "3.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cran/src/base/R-"
                                  (version-major version) "/R-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "09pl0w01fr09bsrwd7nz2r5psysj0z93w4chz3hm2havvqqvhg3s"))))
    (build-system gnu-build-system)
    (arguments
     `(#:disallowed-references (,tzdata-2017a)
       #:make-flags
       (list (string-append "LDFLAGS=-Wl,-rpath="
                            (assoc-ref %outputs "out")
                            "/lib/R/lib")
             ;; This affects the embedded timestamp of only the core packages.
             "PKG_BUILT_STAMP=1970-01-01")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'do-not-compress-serialized-files
           (lambda* (#:key inputs #:allow-other-keys)
             ;; This ensures that Guix can detect embedded store references;
             ;; see bug #28157 for details.
             (substitute* "src/library/base/makebasedb.R"
               (("compress = TRUE") "compress = FALSE"))
             #t))
         (add-before 'configure 'patch-uname
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((uname-bin (string-append (assoc-ref inputs "coreutils")
                                             "/bin/uname")))
               (substitute* "src/scripts/R.sh.in"
                 (("uname") uname-bin)))
             #t))
         (add-after 'unpack 'build-reproducibly
           (lambda _
             ;; The documentation contains time stamps to demonstrate
             ;; documentation generation in different phases.
             (substitute* "src/library/tools/man/Rd2HTML.Rd"
               (("\\\\%Y-\\\\%m-\\\\%d at \\\\%H:\\\\%M:\\\\%S")
                "(removed for reproducibility)"))

             ;; Remove timestamp from tracing environment.  This fixes
             ;; reproducibility of "methods.rd{b,x}".
             (substitute* "src/library/methods/R/trace.R"
               (("dateCreated = Sys.time\\(\\)")
                "dateCreated = as.POSIXct(\"1970-1-1 00:00:00\", tz = \"UTC\")"))

             ;; Ensure that gzipped files are reproducible.
             (substitute* '("src/library/grDevices/Makefile.in"
                            "doc/manual/Makefile.in")
               (("R_GZIPCMD\\)" line)
                (string-append line " -n")))

             ;; The "srcfile" procedure in "src/library/base/R/srcfile.R"
             ;; queries the mtime of a given file and records it in an object.
             ;; This is acceptable at runtime to detect stale source files,
             ;; but it destroys reproducibility at build time.
             ;;
             ;; Instead of disabling this feature, which may have unexpected
             ;; consequences, we reset the mtime of generated files before
             ;; passing them to the "srcfile" procedure.
             (substitute* "src/library/Makefile.in"
               (("@\\(cd base && \\$\\(MAKE\\) mkdesc\\)" line)
                (string-append line "\n	find $(top_builddir)/library/tools | xargs touch -d '1970-01-01'; \n"))
               (("@\\$\\(MAKE\\) Rdobjects" line)
                (string-append "@find $(srcdir)/tools | xargs touch -d '1970-01-01'; \n	"
                               line)))
             (substitute* "src/library/tools/Makefile.in"
               (("@\\$\\(INSTALL_DATA\\) all.R \\$\\(top_builddir\\)/library/\\$\\(pkg\\)/R/\\$\\(pkg\\)" line)
                (string-append
                 line
                 "\n	find $(srcdir)/$(pkg) $(top_builddir)/library/$(pkg) | xargs touch -d \"1970-01-01\"; \n")))

             ;; This library is installed using "install_package_description",
             ;; so we need to pass the "builtStamp" argument.
             (substitute* "src/library/tools/Makefile.in"
               (("(install_package_description\\(.*\"')\\)\"" line prefix)
                (string-append prefix ", builtStamp='1970-01-01')\"")))

             ;; R bundles an older version of help2man, which does not respect
             ;; SOURCE_DATE_EPOCH.  We cannot just use the latest help2man,
             ;; because that breaks a test.
             (with-fluids ((%default-port-encoding "ISO-8859-1"))
               (substitute* "tools/help2man.pl"
                 (("my \\$date = strftime \"%B %Y\", localtime" line)
                  (string-append line " 1"))))
             #t))
         (add-before 'configure 'set-default-pager
          ;; Set default pager to "cat", because otherwise it is "false",
          ;; making "help()" print nothing at all.
          (lambda _ (setenv "PAGER" "cat") #t))
         (add-before 'check 'set-timezone
           ;; Some tests require the timezone to be set.  However, the
           ;; timezone may not just be "UTC", or else a brittle regression
           ;; test in reg-tests-1d will fail.
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "TZ" "UTC+1")
             (setenv "TZDIR"
                     (string-append (assoc-ref inputs "tzdata")
                                    "/share/zoneinfo"))
             #t))
         (add-after 'build 'make-info
          (lambda _ (zero? (system* "make" "info"))))
         (add-after 'build 'install-info
          (lambda _ (zero? (system* "make" "install-info")))))
       #:configure-flags
       '(;; Do not build the recommended packages.  The build system creates
         ;; random temporary directories and embeds their names in some
         ;; package files.  We build these packages with the r-build-system
         ;; instead.
         "--without-recommended-packages"
         "--with-cairo"
         "--with-blas=-lopenblas"
         "--with-libpng"
         "--with-jpeglib"
         "--with-libtiff"
         "--with-ICU"
         "--enable-R-shlib"
         "--enable-BLAS-shlib"
         "--with-system-tre")))
    ;; R has some support for Java.  When the JDK is available at configure
    ;; time environment variables pointing to the JDK will be recorded under
    ;; $R_HOME/etc and ./tools/getsp.java will be compiled which is used by "R
    ;; CMD javareconf".  "R CMD javareconf" appears to only be used to update
    ;; the recorded environment variables in $R_HOME/etc.  Refer to
    ;; https://cran.r-project.org/doc/manuals/r-release/R-admin.html#Java-support
    ;; for additional information.

    ;; As the JDK is a rather large input with only very limited effects on R,
    ;; we decided to drop it.
    (native-inputs
     `(("bzip2" ,bzip2)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("texinfo" ,texinfo) ; for building HTML manuals
       ("tzdata" ,tzdata-2017a)
       ("xz" ,xz)))
    (inputs
     `(;; We need not only cairo here, but pango to ensure that tests for the
       ;; "cairo" bitmapType plotting backend succeed.
       ("pango" ,pango)
       ("coreutils" ,coreutils)
       ("curl" ,curl)
       ("openblas" ,openblas)
       ("gfortran" ,gfortran)
       ("icu4c" ,icu4c)
       ("libjpeg" ,libjpeg)
       ("libpng" ,libpng)
       ("libtiff" ,libtiff)
       ("libxt" ,libxt)
       ("pcre" ,pcre)
       ("readline" ,readline)
       ("which" ,which)
       ("zlib" ,zlib)))
    (native-search-paths
     (list (search-path-specification
            (variable "R_LIBS_SITE")
            (files (list "site-library/")))))
    (home-page "http://www.r-project.org/")
    (synopsis "Environment for statistical computing and graphics")
    (description
     "R is a language and environment for statistical computing and graphics.
It provides a variety of statistical techniques, such as linear and nonlinear
modeling, classical statistical tests, time-series analysis, classification
and clustering.  It also provides robust support for producing
publication-quality data plots.  A large amount of 3rd-party packages are
available, greatly increasing its breadth and scope.")
    (license license:gpl3+)))

(define-public rmath-standalone
  (package (inherit r-minimal)
    (name "rmath-standalone")
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'configure 'chdir
           (lambda _ (chdir "src/nmath/standalone/") #t)))))
    (synopsis "Standalone R math library")
    (description
     "This package provides the R math library as an independent package.")))

(define-public r-boot
  (package
    (name "r-boot")
    (version "1.3-20")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "boot" version))
       (sha256
        (base32
         "0ai1qpm0p4z07xr0dvag8sdn9jrxcwanrsk9khzmww094jvr1jxd"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/boot")
    (synopsis "Bootstrap functions for R")
    (description
     "This package provides functions and datasets for bootstrapping from the
book \"Bootstrap Methods and Their Application\" by A.C. Davison and
D.V. Hinkley (1997, CUP), originally written by Angelo Canty for S.")
    ;; Unlimited distribution
    (license (license:non-copyleft "file://R/bootfuns.q"))))

(define-public r-mass
  (package
    (name "r-mass")
    (version "7.3-47")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "MASS" version))
       (sha256
        (base32
         "1gy6z7ly9wn86rfn9xrmqiqq1ijw3pkasrr2299kbzsgx2mwsi7d"))))
    (properties `((upstream-name . "MASS")))
    (build-system r-build-system)
    (home-page "http://www.stats.ox.ac.uk/pub/MASS4/")
    (synopsis "Support functions and datasets for Venables and Ripley's MASS")
    (description
     "This package provides functions and datasets for the book \"Modern
Applied Statistics with S\" (4th edition, 2002) by Venables and Ripley.")
    ;; Either version may be picked.
    (license (list license:gpl2 license:gpl3))))

(define-public r-class
  (package
    (name "r-class")
    (version "7.3-14")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "class" version))
       (sha256
        (base32
         "173b8a16lh1i0zjmr784l0xr0azp9v8bgslh12hfdswbq7dpdf0q"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-mass" ,r-mass)))
    (home-page "http://www.stats.ox.ac.uk/pub/MASS4/")
    (synopsis "R functions for classification")
    (description
     "This package provides various functions for classification, including
k-nearest neighbour, Learning Vector Quantization and Self-Organizing Maps.")
    ;; Either of the two versions can be picked.
    (license (list license:gpl2 license:gpl3))))

(define-public r-cluster
  (package
    (name "r-cluster")
    (version "2.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "cluster" version))
       (sha256
        (base32
         "1z4gbz7chxxi4ly6c0yjlikwgf8aa8dlg05cn5cd6pjr21zvh97l"))))
    (build-system r-build-system)
    (inputs
     `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/cluster")
    (synopsis "Methods for cluster analysis")
    (description
     "This package provides methods for cluster analysis.  It is a much
extended version of the original from Peter Rousseeuw, Anja Struyf and Mia
Hubert, based on Kaufman and Rousseeuw (1990) \"Finding Groups in Data\".")
    (license license:gpl2+)))

(define-public r-codetools
  (package
    (name "r-codetools")
    (version "0.2-15")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "codetools" version))
       (sha256
        (base32
         "0h7sjmvvsi35041jp47cxhsqzgf1y8jrw6fxii7n26i8g7nrh1sf"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/codetools")
    (synopsis "Code analysis tools for R")
    (description "This package provides code analysis tools for R to check R
code for possible problems.")
    ;; Any version of the GPL.
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-foreign
  (package
    (name "r-foreign")
    (version "0.8-69")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "foreign" version))
       (sha256
        (base32
         "0s1lxmd85dd0kxx8hwk02w9l7pmpk4bpy7787fbyh2dbq5g9ys0k"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/foreign")
    (synopsis "Read data stored by other statistics software")
    (description
     "This package provides functions for reading and writing data stored by
some versions of Epi Info, Minitab, S, SAS, SPSS, Stata, Systat and Weka and
for reading and writing some dBase files.")
    (license license:gpl2+)))

(define-public r-kernsmooth
  (package
    (name "r-kernsmooth")
    (version "2.23-15")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "KernSmooth" version))
       (sha256
        (base32
         "1xhha8kw10jv8pv8b61hb5in9qiw3r2a9kdji3qlm991s4zd4wlb"))))
    (properties `((upstream-name . "KernSmooth")))
    (build-system r-build-system)
    (inputs
     `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/KernSmooth")
    (synopsis "Functions for kernel smoothing")
    (description
     "This package provides functions for kernel smoothing (and density
estimation) corresponding to the book: Wand, M.P. and Jones, M.C. (1995)
\"Kernel Smoothing\".")
    ;; Unlimited use and distribution
    (license (license:non-copyleft "file://LICENCE.note"))))

(define-public r-lattice
  (package
    (name "r-lattice")
    (version "0.20-35")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "lattice" version))
              (sha256
               (base32
                "0pcnmaz3lr62ly0dcy5hnnqxshc4yqd43hrvlz3almgc9l7sna88"))))
    (build-system r-build-system)
    (home-page "http://lattice.r-forge.r-project.org/")
    (synopsis "High-level data visualization system")
    (description
     "The lattice package provides a powerful and elegant high-level data
visualization system inspired by Trellis graphics, with an emphasis on
multivariate data.  Lattice is sufficient for typical graphics needs, and is
also flexible enough to handle most nonstandard requirements.")
    (license license:gpl2+)))

(define-public r-matrix
  (package
    (name "r-matrix")
    (version "1.2-12")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Matrix" version))
       (sha256
        (base32
         "1wm45hg4x5ay15y03k6rmgkd1n9r01da72mszk24vafwd7pimr8n"))))
    (properties `((upstream-name . "Matrix")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-lattice" ,r-lattice)))
    (home-page "http://Matrix.R-forge.R-project.org/")
    (synopsis "Sparse and dense matrix classes and methods")
    (description
     "This package provides classes and methods for dense and sparse matrices
and operations on them using LAPACK and SuiteSparse.")
    (license license:gpl2+)))

(define-public r-nlme
  (package
    (name "r-nlme")
    (version "3.1-131")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "nlme" version))
       (sha256
        (base32
         "0k2nvdzhic6bzhfsbq6la6q6a1i5nlj4pnh6lpdxiiwvxdks3nkr"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-lattice" ,r-lattice)))
    (native-inputs
     `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/nlme")
    (synopsis "Linear and nonlinear mixed effects models")
    (description
     "This package provides tools to fit and compare Gaussian linear and
nonlinear mixed-effects models.")
    (license license:gpl2+)))

(define-public r-mgcv
  (package
   (name "r-mgcv")
   (version "1.8-22")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "mgcv" version))
     (sha256
      (base32
       "1546p6aflg3z6xl2mns1n2c3j8q2spr9cjggj9rn33vrrhsv4fss"))))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-matrix" ,r-matrix)
      ("r-nlme" ,r-nlme)))
   (home-page "https://cran.r-project.org/web/packages/mgcv")
   (synopsis "Mixed generalised additive model computation")
   (description
    "GAMs, GAMMs and other generalized ridge regression with multiple smoothing
parameter estimation by GCV, REML or UBRE/AIC.  The library includes a
@code{gam()} function, a wide variety of smoothers, JAGS support and
distributions beyond the exponential family.")
   (license license:gpl2+)))

(define-public r-nnet
  (package
    (name "r-nnet")
    (version "7.3-12")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "nnet" version))
       (sha256
        (base32
         "17amqnw9dpap2w8ivx53hxha2xrm0drwfnj32li0xk41hlz548r7"))))
    (build-system r-build-system)
    (home-page "http://www.stats.ox.ac.uk/pub/MASS4/")
    (synopsis "Feed-forward neural networks and multinomial log-linear models")
    (description
     "This package provides functions for feed-forward neural networks with a
single hidden layer, and for multinomial log-linear models.")
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-rpart
  (package
    (name "r-rpart")
    (version "4.1-11")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rpart" version))
       (sha256
        (base32
         "165djqj7lk81jr7z5fwccq3h7ayys26hx1kj9hndvg2rkyaq1arq"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/rpart")
    (synopsis "Recursive partitioning and regression trees")
    (description
     "This package provides recursive partitioning functions for
classification, regression and survival trees.")
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-spatial
  (package
    (name "r-spatial")
    (version "7.3-11")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "spatial" version))
       (sha256
        (base32
         "04aw8j533sn63ybyrf4hyhrqm4058vfcb7yhjy07kq92mk94hi32"))))
    (build-system r-build-system)
    (home-page "http://www.stats.ox.ac.uk/pub/MASS4/")
    (synopsis "Functions for kriging and point pattern analysis")
    (description
     "This package provides functions for kriging and point pattern
analysis.")
    ;; Either version may be picked.
    (license (list license:gpl2 license:gpl3))))

(define-public r-survival
  (package
    (name "r-survival")
    (version "2.41-3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "survival" version))
       (sha256
        (base32
         "07cnr0hnki6ybbjll54l4s5lllhk19vni5f8m0mvsfp99ls7qygk"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-matrix" ,r-matrix)))
    (home-page "https://github.com/therneau/survival")
    (synopsis "Survival analysis")
    (description
     "This package contains the core survival analysis routines, including
definition of Surv objects, Kaplan-Meier and Aalen-Johansen (multi-state)
curves, Cox models, and parametric accelerated failure time models.")
    (license license:lgpl2.0+)))

(define-public r
  (package (inherit r-minimal)
    (name "r")
    (source #f)
    (build-system trivial-build-system)
    (arguments '(#:builder (mkdir %output)))
    (propagated-inputs
     `(("r-minimal" ,r-minimal)
       ("r-boot" ,r-boot)
       ("r-class" ,r-class)
       ("r-cluster" ,r-cluster)
       ("r-codetools" ,r-codetools)
       ("r-foreign" ,r-foreign)
       ("r-kernsmooth" ,r-kernsmooth)
       ("r-lattice" ,r-lattice)
       ("r-mass" ,r-mass)
       ("r-matrix" ,r-matrix)
       ("r-mgcv" ,r-mgcv)
       ("r-nlme" ,r-nlme)
       ("r-nnet" ,r-nnet)
       ("r-rpart" ,r-rpart)
       ("r-spatial" ,r-spatial)
       ("r-survival" ,r-survival)))))

(define-public r-bit
  (package
    (name "r-bit")
    (version "1.1-12")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "bit" version))
       (sha256
        (base32
         "0a6ig6nnjzq80r2ll4hc74za3xwzbzig6wlyb4dby0knzf3iqa6f"))))
    (build-system r-build-system)
    (home-page "http://ff.r-forge.r-project.org")
    (synopsis "Class for vectors of 1-bit booleans")
    (description
     "This package provides bitmapped vectors of booleans (no @code{NA}s),
coercion from and to logicals, integers and integer subscripts, fast boolean
operators and fast summary statistics.  With @code{bit} class vectors of true
binary booleans, @code{TRUE} and @code{FALSE} can be stored with 1 bit only.")
    (license license:gpl2)))

(define-public r-bit64
  (package
    (name "r-bit64")
    (version "0.9-7")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "bit64" version))
       (sha256
        (base32
         "07znvz9vp1nz1y5ljng4qyskvm943cdbmy996s67560ijxzsm6kv"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bit" ,r-bit)))
    (home-page "http://ff.r-forge.r-project.org/")
    (synopsis "S3 class for vectors of 64 bit integers")
    (description
     "The bit64 package provides serializable S3 atomic 64 bit (signed)
integers that can be used in vectors, matrices, arrays and @code{data.frames}.
Methods are available for coercion from and to logicals, integers, doubles,
characters and factors as well as many elementwise and summary functions.
Many fast algorithmic operations such as @code{match} and @code{order} support
interactive data exploration and manipulation and optionally leverage
caching.")
    (license license:gpl2)))

(define-public r-dichromat
  (package
    (name "r-dichromat")
    (version "2.0-0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "dichromat" version))
       (sha256
        (base32 "1l8db1nk29ccqg3mkbafvfiw0775iq4gapysf88xq2zp6spiw59i"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/dichromat")
    (synopsis "Color schemes for dichromats")
    (description
     "Dichromat collapses red-green or green-blue distinctions to simulate the
effects of different types of color-blindness.")
    (license license:gpl2+)))

(define-public r-digest
  (package
    (name "r-digest")
    (version "0.6.13")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "digest" version))
       (sha256
        (base32 "1bsgl07bvf4nk6bn7n3l2ilvk4qvn3nk7yxp22miil7x405xdks6"))))
    (build-system r-build-system)
    ;; Vignettes require r-knitr, which requires r-digest, so we have to
    ;; disable them and the tests.
    (arguments
     `(#:tests? #f
       #:configure-flags (list "--no-build-vignettes")))
    (home-page "http://dirk.eddelbuettel.com/code/digest.html")
    (synopsis "Create cryptographic hash digests of R objects")
    (description
     "This package contains an implementation of a function 'digest()' for the
creation of hash digests of arbitrary R objects (using the md5, sha-1,
sha-256, crc32, xxhash and murmurhash algorithms) permitting easy comparison
of R language objects, as well as a function 'hmac()' to create hash-based
message authentication code.

Please note that this package is not meant to be deployed for cryptographic
purposes for which more comprehensive (and widely tested) libraries such as
OpenSSL should be used.")
    (license license:gpl2+)))

(define-public r-estimability
  (package
    (name "r-estimability")
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "estimability" version))
              (sha256
               (base32
                "13b80bpnbrarazjvnpnk91ljjsqgfm2fm3gy66aj09cmmsmv199h"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/estimability")
    (synopsis "Tools for assessing estimability of linear predictions")
    (description "Provides tools for determining estimability of linear
functions of regression coefficients, and 'epredict' methods that handle
non-estimable cases correctly.")
    (license license:gpl2+)))

(define-public r-pheatmap
  (package
    (name "r-pheatmap")
    (version "1.0.8")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "pheatmap" version))
       (sha256
        (base32
         "1ik0k69kb4n7xl3bkx4p09kw08ri93855zcsxq1c668171jqfiji"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-gtable" ,r-gtable)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-scales" ,r-scales)))
    (home-page
     "https://cran.r-project.org/web/packages/pheatmap")
    (synopsis "Pretty heatmaps")
    (description
     "This package provides an implementation of heatmaps that offers more
control over dimensions and appearance.")
    (license license:gpl2+)))

(define-public r-labeling
  (package
    (name "r-labeling")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "labeling" version))
       (sha256
        (base32 "13sk7zrrrzry6ky1bp8mmnzcl9jhvkig8j4id9nny7z993mnk00d"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/labeling")
    (synopsis "Axis labeling algorithms")
    (description "The labeling package provides a range of axis labeling
algorithms.")
    (license license:expat)))

(define-public r-magrittr
  (package
    (name "r-magrittr")
    (version "1.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "magrittr" version))
       (sha256
        (base32 "1s1ar6rag8m277qcqmdp02gn4awn9bdj9ax0r8s32i59mm1mki05"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/magrittr/index.html")
    (synopsis "A forward-pipe operator for R")
    (description
     "Magrittr provides a mechanism for chaining commands with a new
forward-pipe operator, %>%.  This operator will forward a value, or the result
of an expression, into the next function call/expression.  There is flexible
support for the type of right-hand side expressions.  For more information,
see package vignette.  To quote Rene Magritte, \"Ceci n'est pas un pipe.\"")
    (license license:expat)))

(define-public r-munsell
  (package
    (name "r-munsell")
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "munsell" version))
       (sha256
        (base32 "0jdxlbjslkzaqgp058da1cgm85qvqi09wpcgpvp4hvwnmy83qz1r"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-colorspace" ,r-colorspace)))
    (home-page "https://cran.r-project.org/web/packages/munsell")
    (synopsis "Munsell colour system")
    (description
     "The Munsell package contains Functions for exploring and using the
Munsell colour system.")
    (license license:expat)))

(define-public r-permute
  (package
   (name "r-permute")
   (version "0.9-4")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "permute" version))
     (sha256
      (base32
       "1w8wzk1fg9q7wvisnfp2js70dg0m9wi12gkdhpyngpbdcgssahd5"))))
   (build-system r-build-system)
   ;; Tests do not run correctly, but running them properly would entail a
   ;; circular dependency with vegan.
   (home-page "https://github.com/gavinsimpson/permute")
   (synopsis "Functions for Generating Restricted Permutations of Data")
   (description
    "This package provides a set of restricted permutation designs for freely
exchangeable, line transects (time series), spatial grid designs and permutation
of blocks (groups of samples).  @code{permute} also allows split-plot designs,
in which the whole-plots or split-plots or both can be freely exchangeable.")
   (license license:gpl2+)))

(define-public r-plyr
  (package
    (name "r-plyr")
    (version "1.8.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "plyr" version))
       (sha256
        (base32 "1igar5pcjqh0jyxv0z3jah8rz617vfa86vw0r5c7c031b7bj5db0"))))
    (build-system r-build-system)
    (native-inputs `(("r-rcpp" ,r-rcpp)))
    (home-page "http://had.co.nz/plyr")
    (synopsis "Tools for Splitting, Applying and Combining Data")
    (description
     "Plyr is a set of tools that solves a common set of problems: you need to
break a big problem down into manageable pieces, operate on each piece and
then put all the pieces back together.  For example, you might want to fit a
model to each spatial location or time point in your study, summarise data by
panels or collapse high-dimensional arrays to simpler summary statistics.")
    (license license:expat)))

(define-public r-proto
  (package
    (name "r-proto")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "proto" version))
       (sha256
        (base32 "1l843p8vckjckdhgv37ngv47fga5jzy0n00pmipvp05nnaixk54j"))))
    (build-system r-build-system)
    (home-page "https://github.com/hadley/proto")
    (synopsis "Prototype object-based programming")
    (description
     "Proto is an object oriented system using object-based, also called
prototype-based, rather than class-based object oriented ideas.")
    (license license:gpl2+)))

(define-public r-rcolorbrewer
  (package
    (name "r-rcolorbrewer")
    (version "1.1-2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "RColorBrewer" version))
       (sha256
        (base32 "1pfcl8z1pnsssfaaz9dvdckyfnnc6rcq56dhislbf571hhg7isgk"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/RColorBrewer")
    (synopsis "ColorBrewer palettes")
    (description
     "This package provides color schemes for maps (and other graphics)
designed by Cynthia Brewer as described at http://colorbrewer2.org")
    ;; Includes code licensed under bsd-4
    (license license:asl2.0)))

(define-public r-sendmailr
  (package
    (name "r-sendmailr")
    (version "1.2-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "sendmailR" version))
       (sha256
        (base32
         "0z7ipywnzgkhfvl4zb2fjwl1xq7b5wib296vn9c9qgbndj6b1zh4"))))
    (properties `((upstream-name . "sendmailR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-base64enc" ,r-base64enc)))
    (home-page
     "https://cran.r-project.org/web/packages/sendmailR")
    (synopsis "Send email using R")
    (description
     "This package contains a simple SMTP client which provides a portable
solution for sending email, including attachments, from within R.")
    (license license:gpl2+)))

(define-public r-stringi
  (package
    (name "r-stringi")
    (version "1.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "stringi" version))
       (sha256
        (base32
         "122im5m8x9bqpahc0hbxmvdq6hnsmgnxwwyjpvbihyv2jq5kd44m"))))
    (build-system r-build-system)
    (inputs `(("icu4c" ,icu4c)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://stringi.rexamine.com/")
    (synopsis "Character string processing facilities")
    (description
     "This package allows for fast, correct, consistent, portable, as well as
convenient character string/text processing in every locale and any native
encoding.  Owing to the use of the ICU library, the package provides R users
with platform-independent functions known to Java, Perl, Python, PHP, and Ruby
programmers.  Among available features there are: pattern searching
 (e.g.  via regular expressions), random string generation, string collation,
transliteration, concatenation, date-time formatting and parsing, etc.")
    (license license:bsd-3)))

(define-public r-stringr
  (package
    (name "r-stringr")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "stringr" version))
       (sha256
        (base32 "16hj2rmv8x03lp6cp2jk0k2plibvbggf444kp05przdvd03v7l31"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-magrittr" ,r-magrittr)
       ("r-stringi" ,r-stringi)))
    (home-page "https://github.com/hadley/stringr")
    (synopsis "Simple, consistent wrappers for common string operations")
    (description
     "Stringr is a consistent, simple and easy to use set of wrappers around
the fantastic 'stringi' package.  All function and argument names (and
positions) are consistent, all functions deal with \"NA\"'s and zero length
vectors in the same way, and the output from one function is easy to feed into
the input of another.")
    (license license:gpl2+)))

(define-public r-reshape2
  (package
    (name "r-reshape2")
    (version "1.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "reshape2" version))
       (sha256
        (base32 "03ki5ka1dj208fc0dclbm0b4xp9d769pah2j9cs34l776p4r9zwa"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-plyr" ,r-plyr)
       ("r-rcpp" ,r-rcpp)
       ("r-stringr" ,r-stringr)))
    (home-page "https://github.com/hadley/reshape")
    (synopsis "Flexibly reshape data: a reboot of the \"reshape\" package")
    (description
     "Reshape2 is an R library to flexibly restructure and aggregate data
using just two functions: melt and dcast (or acast).")
    (license license:expat)))

(define-public r-scales
  (package
    (name "r-scales")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "scales" version))
       (sha256
        (base32 "0zg9wfzmsdjxpbld0nzv7hcpq5r0wazqxmn7grvvif2agj0w1z6v"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-dichromat" ,r-dichromat)
       ("r-labeling" ,r-labeling)
       ("r-munsell" ,r-munsell)
       ("r-plyr" ,r-plyr)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-rcpp" ,r-rcpp)
       ("r-r6" ,r-r6)
       ("r-viridislite" ,r-viridislite)))
    (home-page "https://github.com/hadley/scales")
    (synopsis "Scale functions for visualization")
    (description
     "This package provides graphical scales that map data to aesthetics, and
provides methods for automatically determining breaks and labels for axes and
legends.")
    (license license:expat)))

(define-public r-ggplot2
  (package
    (name "r-ggplot2")
    (version "2.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ggplot2" version))
       (sha256
        (base32 "0543782ddv2hp6s0l702mnxfg8n7a6qlbm8bm55x22hnqgz8kg2z"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-digest" ,r-digest)
       ("r-gtable" ,r-gtable)
       ("r-plyr" ,r-plyr)
       ("r-lazyeval" ,r-lazyeval)
       ("r-mass" ,r-mass)
       ("r-tibble" ,r-tibble)
       ("r-reshape2" ,r-reshape2)
       ("r-scales" ,r-scales)
       ("r-svglite" ,r-svglite))) ; Needed for 'ggsave'
    (home-page "http://ggplot2.org")
    (synopsis "An implementation of the grammar of graphics")
    (description
     "Ggplot2 is an implementation of the grammar of graphics in R.  It
combines the advantages of both base and lattice graphics: conditioning and
shared axes are handled automatically, and you can still build up a plot step
by step from multiple data sources.  It also implements a sophisticated
multidimensional conditioning system and a consistent interface to map data to
aesthetic attributes.")
    (license license:gpl2+)))

(define-public r-ggdendro
  (package
    (name "r-ggdendro")
    (version "0.1-20")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "ggdendro" version))
              (sha256
               (base32
                "1zzq1hxd0d1qa5hrzwfkdw6fzscpcafbwbpkrb62dm559y8awp0j"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-mass" ,r-mass)
       ("r-knitr" ,r-knitr)))
    (home-page "https://github.com/andrie/ggdendro")
    (synopsis "Create Dendrograms and Tree Diagrams Using ggplot2")
    (description "This is a set of tools for dendrograms and tree plots using
ggplot2.  The ggplot2 philosophy is to clearly separate data from the
presentation.  Unfortunately the plot method for dendrograms plots directly
to a plot device with out exposing the data.  The ggdendro package resolves
this by making available functions that extract the dendrogram plot data.
The package provides implementations for tree, rpart, as well as diana and
agnes cluster diagrams.")
    (license license:gpl2+)))

(define-public r-gdtools
  (package
    (name "r-gdtools")
    (version "0.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "gdtools" version))
       (sha256
        (base32
         "103wnc8sq0iwcnj4j14hd420d2dqdyf13s3f25icsznrlyzdkhf5"))))
    (build-system r-build-system)
    (native-inputs
     `(("r-rcpp" ,r-rcpp)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("cairo" ,cairo)))
    (propagated-inputs
     `(("r-withr" ,r-withr)))
    (home-page "https://cran.r-project.org/web/packages/gdtools")
    (synopsis "Utilities for graphical rendering")
    (description
     "The @code{gdtools} package provides functionalities to get font metrics
and to generate base64 encoded string from raster matrix.")
    (license license:gpl3)))

(define-public r-svglite
  (package
    (name "r-svglite")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "svglite" version))
       (sha256
        (base32
         "1bws3lc4hlhln11zd7lalhiyc43yk6c6vrzy41fkxk1dkjazfx51"))))
    (build-system r-build-system)
    (native-inputs  `(("r-rcpp" ,r-rcpp)))
    (propagated-inputs
     `(("r-bh" ,r-bh)
       ("r-gdtools" ,r-gdtools)))
    (home-page "https://github.com/hadley/svglite")
    (synopsis "SVG graphics device")
    (description
     "@code{svglite} is a graphics device that produces clean
@dfn{SVG} (Scalable Vector Graphics) output, suitable for use on the web, or
hand editing.  Compared to the built-in @code{svg()}, @code{svglite} is
considerably faster, produces smaller files, and leaves text as is.")
    (license license:gpl2+)))

(define-public r-assertthat
  (package
    (name "r-assertthat")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "assertthat" version))
              (sha256
               (base32
                "1wp5znk3xy338x6hknppk702jn596yr735d9i7c3wabm3sdzfgnp"))))
    (build-system r-build-system)
    (home-page "https://github.com/hadley/assertthat")
    (synopsis "Easy pre and post assertions")
    (description
     "Assertthat is an extension to stopifnot() that makes it easy to declare
the pre and post conditions that your code should satisfy, while also
producing friendly error messages so that your users know what they've done
wrong.")
    (license license:gpl3+)))

(define-public r-lazyeval
  (package
    (name "r-lazyeval")
    (version "0.2.1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "lazyeval" version))
              (sha256
               (base32
                "0q9zx78ncy2ffzrf9sh053069kd3w1xn1ss3gsbyf3y4jhza9cw3"))))
    (build-system r-build-system)
    (home-page "https://github.com/hadley/lazyeval")
    (synopsis "Lazy (non-standard) evaluation in R")
    (description
     "This package provides the tools necessary to do non-standard
evaluation (NSE) in R.")
    (license license:gpl3+)))

(define-public r-dbi
  (package
    (name "r-dbi")
    (version "0.7")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "DBI" version))
              (sha256
               (base32
                "04fyrxdpqcygg5wviy637y6lgk64xqjjq31lvv4hwqj5kbaxamr5"))))
    (build-system r-build-system)
    (home-page "https://github.com/rstats-db/DBI")
    (synopsis "R database interface")
    (description
     "The DBI package provides a database interface (DBI) definition for
communication between R and relational database management systems.  All
classes in this package are virtual and need to be extended by the various
R/DBMS implementations.")
    (license license:lgpl2.0+)))

(define-public r-bh
  (package
    (name "r-bh")
    (version "1.65.0-1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "BH" version))
              (sha256
               (base32
                "0n6byzrvl1w4hjdxz479q4a7w1118f9ckqc4gqydq7lgzs5agfl2"))))
    (build-system r-build-system)
    (home-page "https://github.com/eddelbuettel/bh")
    (synopsis "R package providing subset of Boost headers")
    (description
     "This package aims to provide the most useful subset of Boost libraries
for template use among CRAN packages.")
    (license license:boost1.0)))

(define-public r-evaluate
  (package
    (name "r-evaluate")
    (version "0.10.1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "evaluate" version))
              (sha256
               (base32
                "070vvmnbdlp7sz2zhza7fhd2a6mlwiln8fn4hyzhsiizbn4n79y9"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-stringr" ,r-stringr)))
    (home-page "https://github.com/hadley/evaluate")
    (synopsis "Parsing and evaluation tools for R")
    (description
     "This package provides tools that allow you to recreate the parsing,
evaluation and display of R code, with enough information that you can
accurately recreate what happens at the command line.  The tools can easily be
adapted for other output formats, such as HTML or LaTeX.")
    (license license:gpl3+)))

(define-public r-formatr
  (package
    (name "r-formatr")
    (version "1.5")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "formatR" version))
              (sha256
               (base32
                "19sd23vgs4ac0fwlw40j3676k6mramb0ajlq8hdw23kjwdx1jk47"))))
    (build-system r-build-system)
    (home-page "http://yihui.name/formatR")
    (synopsis "Format R code automatically")
    (description
     "This package provides a function to format R source code.  Spaces and
indent will be added to the code automatically, and comments will be preserved
under certain conditions, so that R code will be more human-readable and tidy.
There is also a Shiny app as a user interface in this package.")
    (license license:gpl3+)))

(define-public r-highr
  (package
    (name "r-highr")
    (version "0.6")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "highr" version))
              (sha256
               (base32
                "0n9v44dxdy5fhkdmpbpa2p78whyd9z3rhhy42ipdz5m5vsr55qa3"))))
    (build-system r-build-system)
    (home-page "https://github.com/yihui/highr")
    (synopsis "Syntax highlighting for R source code")
    (description
     "This package provides syntax highlighting for R source code.  Currently
it supports LaTeX and HTML output.  Source code of other languages is
supported via Andre Simon's highlight package.")
    (license license:gpl3+)))

(define-public r-mime
  (package
    (name "r-mime")
    (version "0.5")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "mime" version))
              (sha256
               (base32
                "0i91m3ivaja1k33jwcvz16pfjypkci27awm8glil7sxhmwaj3izw"))))
    (build-system r-build-system)
    (home-page "https://github.com/yihui/mime")
    (synopsis "R package to map filenames to MIME types")
    (description
     "This package guesses the MIME type from a filename extension using the
data derived from /etc/mime.types in UNIX-type systems.")
    (license license:gpl2)))

(define-public r-markdown
  (package
    (name "r-markdown")
    (version "0.8")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "markdown" version))
              (sha256
               (base32
                "1vcgsh2m2f5kfgappgg71nbf04ff0j1sbk668krjs3r2n89dk3sk"))))
    (build-system r-build-system)
    ;; Skip check phase because the tests require the r-knitr package to be
    ;; installed. This prevents installation failures. Knitr normally
    ;; shouldn't be available since r-markdown is a dependency of the r-knitr
    ;; package.
    (arguments `(#:tests? #f))
    (propagated-inputs
     `(("r-mime" ,r-mime)))
    (home-page "https://github.com/rstudio/markdown")
    (synopsis "Markdown rendering for R")
    (description
     "This package provides R bindings to the Sundown Markdown rendering
library (https://github.com/vmg/sundown).  Markdown is a plain-text formatting
syntax that can be converted to XHTML or other formats.")
    (license license:gpl2)))

(define-public r-yaml
  (package
    (name "r-yaml")
    (version "2.1.16")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "yaml" version))
              (sha256
               (base32
                "1xlsmqal607w6c9rx86061y1fwpbyd5lqp9bad5n7cc9a0blpnkm"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/yaml/")
    (synopsis "Methods to convert R data to YAML and back")
    (description
     "This package implements the libyaml YAML 1.1 parser and
emitter (http://pyyaml.org/wiki/LibYAML) for R.")
    (license license:bsd-3)))

(define-public r-knitr
  (package
    (name "r-knitr")
    (version "1.17")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "knitr" version))
              (sha256
               (base32
                "00gljbz57n9lkvbd6az4n37gpbs8vg3fxx5p9biamhmhqyra514l"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-evaluate" ,r-evaluate)
       ("r-digest" ,r-digest)
       ("r-highr" ,r-highr)
       ("r-markdown" ,r-markdown)
       ("r-stringr" ,r-stringr)
       ("r-yaml" ,r-yaml)))
    (home-page "http://yihui.name/knitr/")
    (synopsis "General-purpose package for dynamic report generation in R")
    (description
     "This package provides a general-purpose tool for dynamic report
generation in R using Literate Programming techniques.")
    ;; The code is released under any version of the GPL.  As it is used by
    ;; r-markdown which is available under GPLv2 only, we have chosen GPLv2+
    ;; here.
    (license license:gpl2+)))

(define-public r-knitrbootstrap
  (package
    (name "r-knitrbootstrap")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "knitrBootstrap" version))
       (sha256
        (base32
         "089147g7nqp99rwdxxsfmklsrwc8aia09sr8ndmrc335r33v6r6p"))))
    (properties `((upstream-name . "knitrBootstrap")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-knitr" ,r-knitr)
       ("r-rmarkdown" ,r-rmarkdown)))
    (home-page "https://github.com/jimhester/knitrBootstrap")
    (synopsis "Knitr bootstrap framework")
    (description
     "This package provides a framework to create Bootstrap 3 HTML reports
from knitr Rmarkdown.")
    (license license:expat)))

(define-public r-microbenchmark
  (package
    (name "r-microbenchmark")
    (version "1.4-2.1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "microbenchmark" version))
              (sha256
               (base32
                "0qn5r1a6qidghcisc2hpbdmj62pnixc3zz6p4ipk8mvakf0hdsvg"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)))
    (home-page "https://cran.r-project.org/web/packages/microbenchmark/")
    (synopsis "Accurate timing functions for R")
    (description
     "This package provides infrastructure to accurately measure and compare
the execution time of R expressions.")
    (license license:bsd-2)))

(define-public r-pryr
  (package
    (name "r-pryr")
    (version "0.1.3")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "pryr" version))
              (sha256
               (base32
                "1b1rcywq9sqzgjsnh3ynnx16gp41n94kkzfalm3y4kyy3ls8ikba"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-stringr" ,r-stringr)
       ("r-codetools" ,r-codetools)))
    (native-inputs
     `(("r-rcpp" ,r-rcpp)))
    (home-page "https://github.com/hadley/pryr")
    (synopsis "Tools for computing on the R language")
    (description
     "This package provides useful tools to pry back the covers of R and
understand the language at a deeper level.")
    (license license:gpl2)))

(define-public r-memoise
  (package
    (name "r-memoise")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "memoise" version))
              (sha256
               (base32
                "034qfc2xlh30x1q2vya239w34a3ir3y2fwnx2agbgbi6592zjxmj"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-digest" ,r-digest)))
    (home-page "https://github.com/hadley/memoise")
    (synopsis "Memoise functions for R")
    (description
     "This R package allows to cache the results of a function so that when
you call it again with the same arguments it returns the pre-computed value.")
    (license license:expat)))

(define-public r-crayon
  (package
    (name "r-crayon")
    (version "1.3.4")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "crayon" version))
              (sha256
               (base32
                "0s7s6vc3ww8pzanpjisym4jjvwcc5pi2qg8srx7jqlz9j3wrnvpw"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-memoise" ,r-memoise)))
    (home-page "https://github.com/gaborcsardi/crayon")
    (synopsis "Colored terminal output for R")
    (description
     "Colored terminal output on terminals that support ANSI color and
highlight codes.  It also works in Emacs ESS.  ANSI color support is
automatically detected.  Colors and highlighting can be combined and nested.
New styles can also be created easily.  This package was inspired by the
\"chalk\" JavaScript project.")
    (license license:expat)))

(define-public r-praise
  (package
    (name "r-praise")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "praise" version))
       (sha256
        (base32
         "1gfyypnvmih97p2r0php9qa39grzqpsdbq5g0fdsbpq5zms5w0sw"))))
    (build-system r-build-system)
    (home-page "https://github.com/gaborcsardi/praise")
    (synopsis "Functions to praise users")
    (description
     "This package provides template functions to assist in building friendly
R packages that praise their users.")
    (license license:expat)))

(define-public r-testthat
  (package
    (name "r-testthat")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "testthat" version))
              (sha256
               (base32
                "155l53kb69jga5d8c5nvdwqlvlgfmk4vzyyl4d0108j53jnlgh1v"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-cli" ,r-cli)
       ("r-crayon" ,r-crayon)
       ("r-digest" ,r-digest)
       ("r-magrittr" ,r-magrittr)
       ("r-praise" ,r-praise)
       ("r-r6" ,r-r6)
       ("r-rlang" ,r-rlang)
       ("r-withr" ,r-withr)))
    (home-page "https://github.com/hadley/testthat")
    (synopsis "Unit testing for R")
    (description
     "This package provides a unit testing system for R designed to be fun,
flexible and easy to set up.")
    (license license:expat)))

(define-public r-r6
  (package
    (name "r-r6")
    (version "2.2.2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "R6" version))
              (sha256
               (base32
                "13xfdr19ca7ymisidsanm6w7hsk3qmy5l8c0mlz3nk48f7s5cxq8"))))
    (build-system r-build-system)
    (home-page "https://github.com/wch/R6/")
    (synopsis "Classes with reference semantics in R")
    (description
     "The R6 package allows the creation of classes with reference semantics,
similar to R's built-in reference classes.  Compared to reference classes, R6
classes are simpler and lighter-weight, and they are not built on S4 classes
so they do not require the methods package.  These classes allow public and
private members, and they support inheritance, even when the classes are
defined in different packages.")
    (license license:expat)))

(define-public r-rlang
  (package
    (name "r-rlang")
    (version "0.1.4")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "rlang" version))
              (sha256
               (base32
                "1x5rwwsx7xrlyspy41yl5nhyvfgzl73195nsdb4rc6z85ab6r6wd"))))
    (build-system r-build-system)
    (home-page "http://rlang.tidyverse.org")
    (synopsis "Functions for base types, core R and Tidyverse features")
    (description "This package provides a toolbox for working with base types,
core R features like the condition system, and core @code{Tidyverse} features
like tidy evaluation.")
    (license license:gpl3)))

(define-public r-tibble
  (package
    (name "r-tibble")
    (version "1.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "tibble" version))
       (sha256
        (base32
         "02vn6yqzcvmazy5jaqar3wwbrmh83a1bfgsqgk0hgz38i80zgvm7"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rlang" ,r-rlang)
       ("r-rcpp" ,r-rcpp)))
    (home-page "https://github.com/hadley/tibble")
    (synopsis "Simple data frames")
    (description
     "This package provides a @code{tbl_df} class that offers better checking
and printing capabilities than traditional data frames.")
    (license license:expat)))

(define-public r-dplyr
  (package
    (name "r-dplyr")
    (version "0.7.4")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "dplyr" version))
              (sha256
               (base32
                "1hm8ml7yaraag1ak6kvz2mxx6if568c759ix8a1n9d7va03wj7vv"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-assertthat" ,r-assertthat)
       ("r-r6" ,r-r6)
       ("r-magrittr" ,r-magrittr)
       ("r-rlang" ,r-rlang)
       ("r-plogr" ,r-plogr)
       ("r-glue" ,r-glue)
       ("r-pkgconfig" ,r-pkgconfig)
       ("r-bindrcpp" ,r-bindrcpp)
       ("r-tibble" ,r-tibble)))
    (native-inputs
     `(("r-rcpp" ,r-rcpp)
       ("r-bh" ,r-bh)))
    (home-page "https://github.com/hadley/dplyr")
    (synopsis "Tools for working with data frames in R")
    (description
     "dplyr is the next iteration of plyr.  It is focussed on tools for
working with data frames.  It has three main goals: 1) identify the most
important data manipulation tools needed for data analysis and make them easy
to use in R; 2) provide fast performance for in-memory data by writing key
pieces of code in C++; 3) use the same code interface to work with data no
matter where it is stored, whether in a data frame, a data table or
database.")
    (license license:expat)))

(define-public r-dbplyr
  (package
    (name "r-dbplyr")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "dbplyr" version))
       (sha256
        (base32
         "17gn8vr4a6m9ynarjbm9xsrhcvgn5lnxhb2qhiiglmhh5mm4a7kv"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-assertthat" ,r-assertthat)
       ("r-dbi" ,r-dbi)
       ("r-dplyr" ,r-dplyr)
       ("r-glue" ,r-glue)
       ("r-purrr" ,r-purrr)
       ("r-r6" ,r-r6)
       ("r-rlang" ,r-rlang)
       ("r-tibble" ,r-tibble)))
    (home-page "https://github.com/tidyverse/dbplyr")
    (synopsis "Dplyr back end for databases")
    (description
     "This package provides a dplyr back end for databases that allows you to
work with remote database tables as if they are in-memory data frames.  Basic
features works with any database that has a @code{DBI} back end; more advanced
features require SQL translation to be provided by the package author.")
    (license license:expat)))

(define-public r-acepack
  (package
    (name "r-acepack")
    (version "1.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "acepack" version))
       (sha256
        (base32
         "1f98rpfjmhd92rdc3j004plyfpjailz6j0ycysbac0kgj83haxc2"))))
    (build-system r-build-system)
    (inputs
     `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/acepack")
    (synopsis "Functions for regression transformations")
    (description
     "This package provides ACE and AVAS methods for choosing regression
transformations.")
    (license license:expat)))

(define-public r-formula
  (package
    (name "r-formula")
    (version "1.2-2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Formula" version))
       (sha256
        (base32
         "0ad49bzip1zqmpj1d8jajwl4bd81fm3k6dq8p26x6mvlzc04dvwd"))))
    (properties `((upstream-name . "Formula")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/Formula")
    (synopsis "Extended model formulas")
    (description
     "This package provides a new class @code{Formula}, which extends the base
class @code{formula}.  It supports extended formulas with multiple parts of
regressors on the right-hand side and/or multiple responses on the left-hand
side.")
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-locfit
  (package
    (name "r-locfit")
    (version "1.5-9.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "locfit" version))
       (sha256
        (base32
         "0lafrmq1q7x026m92h01hc9cjjiximqqi3v1g2hw7ai9vf7i897m"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-lattice" ,r-lattice)))
    (home-page "https://cran.r-project.org/web/packages/locfit")
    (synopsis "Local regression, likelihood and density estimation")
    (description
     "This package provides functions used for local regression, likelihood
and density estimation.")
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-chron
  (package
    (name "r-chron")
    (version "2.3-51")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "chron" version))
              (sha256
               (base32
                "05aznigw9nwv3hbwjnjbvqhfjqkwsw2csgrjx8500gzr2fvla5w8"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/chron")
    (synopsis "Chronological R objects which can handle dates and times")
    (description
     "This package provides chronological R objects which can handle dates and
times.")
    (license license:gpl2)))

(define-public r-data-table
  (package
    (name "r-data-table")
    (version "1.10.4-3")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "data.table" version))
              (sha256
               (base32
                "12caklgr19vy9wprdks6zh825vr1iw1cajgwcnbpyvmijqdlz2xs"))))
    (build-system r-build-system)
    (home-page "https://github.com/Rdatatable/data.table/wiki")
    (synopsis "Enhanced version of data.frame R object")
    (description
     "The R package @code{data.table} is an extension of @code{data.frame}
providing functions for fast aggregation of large data (e.g. 100GB in RAM),
fast ordered joins, fast add/modify/delete of columns by group, column listing
and fast file reading.")
    (license license:gpl3+)))

(define-public r-xtable
  (package
    (name "r-xtable")
    (version "1.8-2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "xtable" version))
       (sha256
        (base32
         "0398qkpvlw3dv0myz4mjcyqwpwc2m31l127r8vdzwc71wb6s28qn"))))
    (build-system r-build-system)
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "http://xtable.r-forge.r-project.org/")
    (synopsis "Export R tables to LaTeX or HTML")
    (description
     "This package provides tools to export R data as LaTeX and HTML tables.")
    (license license:gpl2+)))

(define-public python-patsy
  (package
    (name "python-patsy")
    (version "0.4.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "patsy" version ".zip"))
              (sha256
               (base32
                "1m6knyq8hbqlx242y4da02j0x86j4qggs1j7q186w3jv0j0c476w"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check (lambda _ (zero? (system* "nosetests" "-v")))))))
    (propagated-inputs
     `(("python-numpy" ,python-numpy)
       ("python-scipy" ,python-scipy)
       ("python-six" ,python-six)))
    (native-inputs
     `(("python-nose" ,python-nose)
       ("unzip" ,unzip)))
    (home-page "https://github.com/pydata/patsy")
    (synopsis "Describe statistical models and build design matrices")
    (description
     "Patsy is a Python package for describing statistical models and for
building design matrices.")
    ;; The majority of the code is distributed under BSD-2.  The module
    ;; patsy.compat contains code derived from the Python standard library,
    ;; and is covered by the PSFL.
    (license (list license:bsd-2 license:psfl))))

(define-public python2-patsy
  (package-with-python2 python-patsy))

(define-public python-statsmodels
  (package
    (name "python-statsmodels")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "statsmodels" version))
       (sha256
        (base32
         "0j30v3932shnj9368c9jr3svkyrvfj90h2l7nxnqkbpv0svilhr6"))
       (patches (search-patches "python-statsmodels-fix-tests.patch"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; tests must be run after installation
         (delete 'check)
         (add-after 'unpack 'set-matplotlib-backend-to-agg
          (lambda _
            ;; Set the matplotlib backend to Agg to avoid problems using the
            ;; GTK backend without a display.
            (substitute* (append (find-files "statsmodels/graphics/tests" "\\.py")
                                 '("statsmodels/tsa/vector_ar/tests/test_var.py"
                                   "statsmodels/duration/tests/test_survfunc.py"))
              (("import matplotlib\\.pyplot as plt" line)
               (string-append "import matplotlib;matplotlib.use('Agg');"
                              line)))
            #t))
         (add-after 'install 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Make installed package available for running the tests
             (add-installed-pythonpath inputs outputs)
             (with-directory-excursion "/tmp"
               (zero? (system* "nosetests"
                               "--stop"
                               "-v" "statsmodels"))))))))
    (propagated-inputs
     `(("python-numpy" ,python-numpy)
       ("python-scipy" ,python-scipy)
       ("python-pandas" ,python-pandas)
       ("python-patsy" ,python-patsy)
       ("python-matplotlib" ,python-matplotlib)))
    (native-inputs
     `(("python-cython" ,python-cython)
       ("python-nose" ,python-nose)
       ("python-sphinx" ,python-sphinx)))
    (home-page "http://statsmodels.sourceforge.net/")
    (synopsis "Statistical modeling and econometrics in Python")
    (description
     "Statsmodels is a Python package that provides a complement to scipy for
statistical computations including descriptive statistics and estimation and
inference for statistical models.")
    (license license:bsd-3)))

(define-public python2-statsmodels
  (let ((stats (package-with-python2 python-statsmodels)))
    (package (inherit stats)
      (propagated-inputs
       `(("python2-pytz" ,python2-pytz)
         ("python2-numpy" ,python2-numpy)
         ("python2-scipy" ,python2-scipy)
         ("python2-pandas" ,python2-pandas)
         ("python2-patsy" ,python2-patsy)
         ("python2-matplotlib" ,python2-matplotlib))))))

(define-public r-coda
  (package
    (name "r-coda")
    (version "0.19-1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "coda" version))
              (sha256
               (base32
                "14a4a8df4ygj05h37chmdn8kzcqs07fpbflxfrq530563mrza7yl"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-lattice" ,r-lattice)))
    (home-page "https://cran.r-project.org/web/packages/coda")
    (synopsis "This is a package for Output Analysis and Diagnostics for MCMC")
    (description "This package provides functions for summarizing and plotting
the output from Markov Chain Monte Carlo (MCMC) simulations, as well as
diagnostic tests of convergence to the equilibrium distribution of the Markov
chain.")
    (license license:gpl2+)))

(define-public r-ade4
  (package
    (name "r-ade4")
    (version "1.7-10")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "ade4" version))
        (sha256
          (base32
            "0zk81x0yn30gbyc0jpzyw1nxd08ccihl6vyk0ijvj3aw3nr5flc6"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-mass" ,r-mass)))
    (home-page "http://pbil.univ-lyon1.fr/ADE-4")
    (synopsis "Multivariate data analysis and graphical display")
    (description
     "The ade4 package contains data analysis functions to analyze ecological
and environmental data in the framework of Euclidean exploratory methods.")
    (license license:gpl2+)))

(define-public r-xml2
  (package
    (name "r-xml2")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "xml2" version))
       (sha256
        (base32
         "05iifrcgzx53w5ciw8bbm4vdwc3giv4xsvazv8chqxkndfvf7wq0"))))
    (build-system r-build-system)
    (inputs
     `(("libxml2" ,libxml2)))
    (propagated-inputs
     `(("r-rcpp" ,r-rcpp)
       ("r-bh" ,r-bh)))
    (home-page "https://github.com/hadley/xml2")
    (synopsis "Parse XML with R")
    (description
     "This package provides a simple, consistent interface to working with XML
files in R.  It is built on top of the libxml2 C library.")
    (license license:gpl2+)))

(define-public r-multitaper
  (package
    (name "r-multitaper")
    (version "1.0-14")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "multitaper" version))
       (sha256
        (base32
         "04wd9bbhyx7697pfy0fpj02v1csr48hkpqj62h9p8a6w84ji4k68"))))
    (build-system r-build-system)
    (native-inputs
     `(("gfortran" ,gfortran)))
    (home-page "https://github.com/wesleyburr/multitaper/")
    (synopsis "Multitaper spectral analysis tools")
    (description
     "This package implements multitaper spectral estimation
techniques using prolate spheroidal sequences (Slepians) and sine
tapers for time series analysis.  It includes an adaptive weighted
multitaper spectral estimate, a coherence estimate, Thomson's Harmonic
F-test, and complex demodulation.  The Slepians sequences are
generated efficiently using a tridiagonal matrix solution, and
jackknifed confidence intervals are available for most estimates.")
    (license license:gpl2+)))

(define-public r-rversions
  (package
    (name "r-rversions")
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "rversions" version))
              (sha256
               (base32
                "0i2gi05nrvknr7g89rbppkswyfcwwd4r9gp75fdfhpah8sgq1l11"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-curl" ,r-curl)
       ("r-xml2" ,r-xml2)))
    (home-page "https://github.com/metacran/rversions")
    (synopsis "Query R versions, including 'r-release' and 'r-oldrel'")
    (description
     "This package provides functions to query the main R repository to find
the versions that @code{r-release} and @code{r-oldrel} refer to, and also all
previous R versions and their release dates.")
    (license license:expat)))

(define-public r-whisker
  (package
    (name "r-whisker")
    (version "0.3-2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "whisker" version))
              (sha256
               (base32
                "0z4cn115gxcl086d6bnqr8afi67b6a7xqg6ivmk3l4ng1x8kcj28"))))
    (build-system r-build-system)
    (home-page "https://github.com/edwindj/whisker")
    (synopsis "Logicless mustache templating for R")
    (description
     "This package provides logicless templating, with a syntax that is not
limited to R.")
    (license license:gpl3+)))

(define-public r-backports
  (package
    (name "r-backports")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "backports" version))
       (sha256
        (base32
         "0mml9h3xagi7144pyb3jj9zbh9qzns7izkhdg7df20v7bikr6nz8"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/backports")
    (synopsis "Reimplementations of functions introduced since R 3.0.0")
    (description
     "Provides implementations of functions which have been introduced in R
since version 3.0.0.  The backports are conditionally exported which results
in R resolving the function names to the version shipped with R (if available)
and uses the implemented backports as fallback.  This way package developers
can make use of the new functions without worrying about the minimum required
R version.")
    (license license:gpl2+)))

(define-public r-checkmate
  (package
    (name "r-checkmate")
    (version "1.8.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "checkmate" version))
       (sha256
        (base32
         "1q6igk50lq4fp5d3imgcn1j063h3gsp214ra4nlf534hf4wjlkg9"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-backports" ,r-backports)))
    (home-page "https://github.com/mllg/checkmate")
    (synopsis "Fast and versatile argument checks")
    (description
     "This package provides tests and assertions to perform frequent argument
checks.  A substantial part of the package was written in C to minimize any
worries about execution time overhead.")
    (license license:bsd-3)))

(define-public r-bbmisc
  (package
    (name "r-bbmisc")
    (version "1.11")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "BBmisc" version))
       (sha256
        (base32
         "1lh1n4bvxzivb5rbz69mvd8xdgr3gr2bnqd68a39sd1530l8r90y"))))
    (properties `((upstream-name . "BBmisc")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-checkmate" ,r-checkmate)))
    (home-page "https://github.com/berndbischl/BBmisc")
    (synopsis "Miscellaneous functions for R package development")
    (description
     "This package provides miscellaneous helper functions for the development
of R packages.")
    (license license:bsd-3)))

(define-public r-fail
  (package
    (name "r-fail")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "fail" version))
       (sha256
        (base32
         "0vfm6kmpmgsamda5p0sl771kbnsscan31l2chzssyw93kwmams7d"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bbmisc" ,r-bbmisc)
       ("r-checkmate" ,r-checkmate)))
    (home-page "https://github.com/mllg/fail")
    (synopsis "File abstraction interface layer (FAIL)")
    (description
     "This package provides a more comfortable interface to work with R data
or source files in a key-value fashion.")
    (license license:bsd-3)))

(define-public r-batchjobs
  (package
    (name "r-batchjobs")
    (version "1.7")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "BatchJobs" version))
       (sha256
        (base32
         "035658marnw57p4f38g99rwmvmb6hpbq0fhlxp3qbw22zfnnkvs9"))))
    (properties `((upstream-name . "BatchJobs")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-backports" ,r-backports)
       ("r-bbmisc" ,r-bbmisc)
       ("r-brew" ,r-brew)
       ("r-checkmate" ,r-checkmate)
       ("r-data-table" ,r-data-table)
       ("r-dbi" ,r-dbi)
       ("r-digest" ,r-digest)
       ("r-rsqlite" ,r-rsqlite)
       ("r-sendmailr" ,r-sendmailr)
       ("r-stringi" ,r-stringi)))
    (home-page "https://github.com/tudo-r/BatchJobs")
    (synopsis "Batch computing with R")
    (description
     "This package provides @code{Map}, @code{Reduce} and @code{Filter}
variants to generate jobs on batch computing systems like PBS/Torque, LSF,
SLURM and Sun Grid Engine.  Multicore and SSH systems are also supported.")
    (license license:bsd-2)))

(define-public r-brew
  (package
    (name "r-brew")
    (version "1.0-6")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "brew" version))
              (sha256
               (base32
                "1vghazbcha8gvkwwcdagjvzx6yl8zm7kgr0i9wxr4jng06d1l3fp"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/brew")
    (synopsis "Templating framework for report generation")
    (description
     "The brew package implements a templating framework for mixing text and R
code for report generation.  The template syntax is similar to PHP, Ruby's erb
module, Java Server Pages, and Python's psp module.")
    (license license:gpl2+)))

(define-public r-desc
  (package
    (name "r-desc")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "desc" version))
       (sha256
        (base32
         "0k07qighac1xzmm8k988zi7i88a0yfvia3gk5hbz0fyvb2v9kzrj"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-assertthat" ,r-assertthat)
       ("r-crayon" ,r-crayon)
       ("r-r6" ,r-r6)
       ("r-rprojroot" ,r-rprojroot)))
    (home-page "https://github.com/r-pkgs/desc")
    (synopsis "Manipulate DESCRIPTION Files")
    (description
     "This package provides tools to read, write, create, and manipulate
DESCRIPTION files.  It is intended for packages that create or manipulate
other packages.")
    (license license:expat)))

(define-public r-commonmark
  (package
    (name "r-commonmark")
    (version "1.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "commonmark" version))
       (sha256
        (base32
         "1vb8r9khpvcf0q7acv8rsplfjjwcll302bg5kp596cvn6aacypl6"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/commonmark")
    (synopsis "CommonMark and Github Markdown Rendering in R")
    (description
     "The CommonMark specification defines a rationalized version of markdown
syntax.  This package uses the 'cmark' reference implementation for converting
markdown text into various formats including HTML, LaTeX and groff man.  In
addition, it exposes the markdown parse tree in XML format.  The latest
version of this package also adds support for Github extensions including
tables, autolinks and strikethrough text.")
    (license license:bsd-2)))

(define-public r-roxygen2
  (package
    (name "r-roxygen2")
    (version "6.0.1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "roxygen2" version))
              (sha256
               (base32
                "0xpzziminf225kjwhyl51kgkzhplyzhk5farhf5s822krl2xqbfj"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-brew" ,r-brew)
       ("r-commonmark" ,r-commonmark)
       ("r-desc" ,r-desc)
       ("r-digest" ,r-digest)
       ("r-r6" ,r-r6)
       ("r-rcpp" ,r-rcpp)
       ("r-stringi" ,r-stringi)
       ("r-stringr" ,r-stringr)
       ("r-xml2" ,r-xml2)))
    (home-page "https://github.com/klutometis/roxygen")
    (synopsis "In-source documentation system for R")
    (description
     "Roxygen2 is a Doxygen-like in-source documentation system for Rd,
collation, and NAMESPACE files.")
    (license license:gpl2+)))

(define-public r-openssl
  (package
    (name "r-openssl")
    (version "0.9.9")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "openssl" version))
       (sha256
        (base32
         "1ir7ggs0rxgn83h40r6jg9dzhb7hbcwhkcj5nsdzj7jz61sfgv77"))))
    (build-system r-build-system)
    (inputs
     `(("libressl" ,libressl)))
    (home-page "https://github.com/jeroenooms/openssl")
    (synopsis "Toolkit for encryption, signatures and certificates")
    (description
     "This package provides R bindings to OpenSSL libssl and libcrypto, plus
custom SSH pubkey parsers.  It supports RSA, DSA and NIST curves P-256, P-384
and P-521.  Cryptographic signatures can either be created and verified
manually or via x509 certificates.  AES block cipher is used in CBC mode for
symmetric encryption; RSA for asymmetric (public key) encryption.  High-level
envelope functions combine RSA and AES for encrypting arbitrary sized data.
Other utilities include key generators, hash functions (md5, sha1, sha256,
etc), base64 encoder, a secure random number generator, and @code{bignum} math
methods for manually performing crypto calculations on large multibyte
integers.")
    (license license:expat)))

(define-public r-httr
  (package
    (name "r-httr")
    (version "1.3.1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "httr" version))
              (sha256
               (base32
                "0n7jz2digbgv48rbr9vmzv4vmf4rahl9jjy31izs7sxj4rs4s4r2"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-curl" ,r-curl)
       ("r-jsonlite" ,r-jsonlite)
       ("r-openssl" ,r-openssl)
       ("r-mime" ,r-mime)
       ("r-r6" ,r-r6)))
    (home-page "https://github.com/hadley/httr")
    (synopsis "Tools for working with URLs and HTTP")
    (description
     "The aim of httr is to provide a wrapper for RCurl customised to the
demands of modern web APIs.  It provides useful tools for working with HTTP
organised by HTTP verbs (@code{GET()}, @code{POST()}, etc).  Configuration
functions make it easy to control additional request components.")
    (license license:expat)))

(define-public r-git2r
  (package
    (name "r-git2r")
    (version "0.20.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "git2r" version))
              (sha256
               (base32
                "1pqggijvsalb5cc2pr5gwfj3s713s63f4xii1xrd0qagfgbgz846"))))
    (build-system r-build-system)
    ;; This R package contains modified sources of libgit2.  This modified
    ;; version of libgit2 is built as the package is built.  Hence libgit2 is
    ;; not among the inputs of this package.
    (inputs
     `(("libssh2" ,libssh2)
       ("openssl" ,openssl)
       ("zlib" ,zlib)))
    (home-page "https://github.com/ropensci/git2r")
    (synopsis "Access Git repositories with R")
    (description
     "This package provides an R interface to the libgit2 library, which is a
pure C implementation of the Git core methods.")
    ;; GPLv2 only with linking exception.
    (license license:gpl2)))

(define-public r-rstudioapi
  (package
    (name "r-rstudioapi")
    (version "0.7")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "rstudioapi" version))
              (sha256
               (base32
                "133s75q2hr6jg28m1wvs96qrbc9c4vw87migwhkjqb88xxvbqhd5"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/rstudioapi")
    (synopsis "Safely access the RStudio API")
    (description
     "This package provides functions to access the RStudio API and provide
informative error messages when it's not available.")
    (license license:expat)))

(define-public r-devtools
  (package
    (name "r-devtools")
    (version "1.13.4")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "devtools" version))
              (sha256
               (base32
                "1az1qf50vyhm4w6k2a6wqw3pqc8jv2iqzdhs36cg079hx9swm0mb"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-digest" ,r-digest)
       ("r-git2r" ,r-git2r)
       ("r-httr" ,r-httr)
       ("r-jsonlite" ,r-jsonlite)
       ("r-memoise" ,r-memoise)
       ("r-rstudioapi" ,r-rstudioapi)
       ("r-whisker" ,r-whisker)
       ("r-withr" ,r-withr)))
    (home-page "https://github.com/hadley/devtools")
    (synopsis "Tools to make developing R packages easier")
    (description "The devtools package is a collection of package development
tools to simplify the devolpment of R packages.")
    (license license:gpl2+)))

(define-public r-withr
  (package
    (name "r-withr")
    (version "2.1.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "withr" version))
              (sha256
               (base32
                "12v263fj6l3ds9nrsj6q7h6zhbivp3w2jk7w9dpk1610d3nx9gf8"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-lattice" ,r-lattice)))
    (home-page "https://github.com/jimhester/withr")
    (synopsis "Run code with temporarily modified global state")
    (description
     "This package provides a set of functions to run R code in an environment
in which global state has been temporarily modified.  Many of these functions
were originally a part of the r-devtools package.")
    (license license:gpl2+)))

(define-public r-hms
  (package
    (name "r-hms")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "hms" version))
       (sha256
        (base32
         "07sh75flp15lx2b00s3nxh04xr9gk4qhlbh125qx25isf71zw6zl"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rlang" ,r-rlang)
       ("r-pkgconfig" ,r-pkgconfig)))
    (home-page "https://github.com/rstats-db/hms")
    (synopsis "Pretty time of day")
    (description
     "This package implements an S3 class for storing and formatting
time-of-day values, based on the @code{difftime} class.")
    (license license:gpl3+)))

(define-public r-readr
  (package
    (name "r-readr")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "readr" version))
              (sha256
               (base32
                "1cvw5wdcqk88cp5fyv678mnmp66l3whcd2yh33p2qvx0168bja8s"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rcpp" ,r-rcpp)
       ("r-hms" ,r-hms)
       ("r-tibble" ,r-tibble)
       ("r-r6" ,r-r6)
       ("r-bh" ,r-bh)))
    (home-page "https://github.com/hadley/readr")
    (synopsis "Read tabular data")
    (description
     "This package provides functions to read flat or tabular text files from
disk (or a connection).")
    (license license:gpl2+)))

(define-public r-plotrix
  (package
    (name "r-plotrix")
    (version "3.7")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "plotrix" version))
              (sha256
               (base32
                "0rw81n9p3d2i03b4pgcfj5blryc94f29bm9a4j9bnp5h8qjj6pry"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/plotrix")
    (synopsis "Various plotting functions")
    (description
     "This package provides lots of plotting, various labeling, axis and color
scaling functions for R.")
    (license license:gpl2+)))

(define-public r-gridbase
  (package
    (name "r-gridbase")
    (version "0.4-7")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "gridBase" version))
              (sha256
               (base32
                "09jzw4rzwf2y5lcz7b16mb68pn0fqigv34ff7lr6w3yi9k91i1xy"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/gridBase")
    (synopsis "Integration of base and grid graphics")
    (description
     "This package provides an integration of base and grid graphics for R.")
    (license license:gpl2+)))

(define-public r-latticeextra
  (package
    (name "r-latticeextra")
    (version "0.6-28")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "latticeExtra" version))
       (sha256
        (base32
         "1hkyqsa7klk5glj9y1hg3rxr5qilqw8h0017zc4c3nps7lr9a1kq"))))
    (properties `((upstream-name . "latticeExtra")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-lattice" ,r-lattice)
       ("r-rcolorbrewer" ,r-rcolorbrewer)))
    (home-page "http://latticeextra.r-forge.r-project.org/")
    (synopsis "Extra graphical utilities based on lattice")
    (description
     "Building on the infrastructure provided by the lattice package, this
package provides several new high-level graphics functions and methods, as
well as additional utilities such as panel and axis annotation functions.")
    (license license:gpl2+)))

(define-public r-rcpparmadillo
  (package
    (name "r-rcpparmadillo")
    (version "0.8.300.1.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "RcppArmadillo" version))
              (sha256
               (base32
                "0p6cbnwxgzigf7n5qhqvxdr3nd3pq3c2qq6pskqz7avzf813fy83"))))
    (properties `((upstream-name . "RcppArmadillo")))
    (build-system r-build-system)
    (native-inputs
     `(("r-knitr" ,r-knitr)))  ; needed for vignettes
    (propagated-inputs
     `(("r-rcpp" ,r-rcpp)))
    (home-page "https://github.com/RcppCore/RcppArmadillo")
    (synopsis "Rcpp integration for the Armadillo linear algebra library")
    (description
     "Armadillo is a templated C++ linear algebra library that aims towards a
good balance between speed and ease of use.  Integer, floating point and
complex numbers are supported, as well as a subset of trigonometric and
statistics functions.  Various matrix decompositions are provided through
optional integration with LAPACK and ATLAS libraries.  This package includes
the header files from the templated Armadillo library.")
    ;; Armadillo is licensed under the MPL 2.0, while RcppArmadillo (the Rcpp
    ;; bindings to Armadillo) is licensed under the GNU GPL version 2 or
    ;; later, as is the rest of 'Rcpp'.
    (license license:gpl2+)))

(define-public r-bitops
  (package
    (name "r-bitops")
    (version "1.0-6")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "bitops" version))
              (sha256
               (base32
                "176nr5wpnkavn5z0yy9f7d47l37ndnn2w3gv854xav8nnybi6wwv"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/bitops")
    (synopsis "Bitwise operations")
    (description
     "This package provides functions for bitwise operations on integer
vectors.")
    (license license:gpl2+)))

(define-public r-catools
  (package
    (name "r-catools")
    (version "1.17.1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "caTools" version))
              (sha256
               (base32
                "1x4szsn2qmbzpyjfdaiz2q7jwhap2gky9wq0riah74q0pzz76ank"))))
    (properties `((upstream-name . "caTools")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bitops" ,r-bitops)))
    (home-page "https://cran.r-project.org/web/packages/caTools")
    (synopsis "Various tools including functions for moving window statistics")
    (description
     "This package contains several basic utility functions including:
moving (rolling, running) window statistic functions, read/write for GIF and
ENVI binary files, fast calculation of AUC, LogitBoost classifier, base64
encoder/decoder, round-off-error-free sum and cumsum, etc.")
    (license license:gpl3+)))

(define-public r-rprojroot
  (package
    (name "r-rprojroot")
    (version "1.3-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rprojroot" version))
       (sha256
        (base32
         "1jigr2jh3hzy35h94im52yq81lyikw7nfvmbxij84a1b9c32r332"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-backports" ,r-backports)))
    (home-page "https://github.com/krlmlr/rprojroot")
    (synopsis "Finding files in project subdirectories")
    (description
     "This package helps accessing files relative to a project root.  It
provides helpers for robust, reliable and flexible paths to files below a
project root.  The root of a project is defined as a directory that matches a
certain criterion, e.g., it contains a certain regular file.")
    (license license:gpl3)))

(define-public r-rmarkdown
  (package
    (name "r-rmarkdown")
    (version "1.8")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "rmarkdown" version))
        (sha256
          (base32
            "0q1y1qx87xp6vf9h7mfl77rqhmd01xl53l9ap9qj99bfvpabb572"))))
    (properties `((upstream-name . "rmarkdown")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-base64enc" ,r-base64enc)
       ("r-evaluate" ,r-evaluate)
       ("r-htmltools" ,r-htmltools)
       ("r-jsonlite" ,r-jsonlite)
       ("r-knitr" ,r-knitr)
       ("r-mime" ,r-mime)
       ("r-rprojroot" ,r-rprojroot)
       ("r-stringr" ,r-stringr)
       ("r-yaml" ,r-yaml)
       ("ghc-pandoc" ,ghc-pandoc)))
    (home-page "http://rmarkdown.rstudio.com")
    (synopsis "Convert R Markdown documents into a variety of formats")
    (description
     "This package provides tools to convert R Markdown documents into a
variety of formats.")
    (license license:gpl3+)))

(define-public r-gtable
  (package
    (name "r-gtable")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "gtable" version))
              (sha256
               (base32
                "0vz7073m0a2q12qzzihrfh5c2kx5jqi5l7z470fxmwqghdllh7l0"))))
    (properties `((upstream-name . "gtable")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/gtable")
    (synopsis "R library to arrange grobs in tables")
    (description
     "Gtable is a collection of tools to make it easier to work with
\"tables\" of grobs.")
    (license license:gpl2+)))

(define-public r-gridextra
  (package
    (name "r-gridextra")
    (version "2.3")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "gridExtra" version))
              (sha256
               (base32
                "0fwfk3cdwxxim7nd55pn7m31bcaqj48y06j7an2k1v1pybk0rdl1"))))
    (properties `((upstream-name . "gridExtra")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-gtable" ,r-gtable)))
    (native-inputs
     `(("r-knitr" ,r-knitr))) ;for building vignettes
    (home-page "https://github.com/baptiste/gridextra")
    (synopsis "Miscellaneous functions for \"Grid\" graphics")
    (description
     "This package provides a number of user-level functions to work with
@code{grid} graphics, notably to arrange multiple grid-based plots on a page,
and draw tables.")
    (license license:gpl2+)))

(define-public r-pkgconfig
  (package
    (name "r-pkgconfig")
    (version "2.0.1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "pkgconfig" version))
              (sha256
               (base32
                "0h2sp93fqqjbfqgq82a3i94ybnndx6ghaal8pbf99firnsjb40mb"))))
    (build-system r-build-system)
    (home-page "https://github.com/gaborcsardi/pkgconfig")
    (synopsis "Private configuration for R packages")
    (description "This package provides the functionality to set configuration
options on a per-package basis.  Options set by a given package only apply to
that package, other packages are unaffected.")
    (license license:expat)))

(define-public r-blob
  (package
    (name "r-blob")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "blob" version))
              (sha256
               (base32
                "05pazzcyz3c3vd2l70zq9cf172cgjff4dnf419zigfnxycyn1mhn"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-tibble" ,r-tibble)))
    (home-page "https://github.com/hadley/blob")
    (synopsis "Simple S3 Class for representing vectors of binary data")
    (description "Raw vectors in R are useful for storing a single binary
object.  What if you want to put a vector of them in a data frame?  The blob
package provides the blob object, a list of raw vectors, suitable for use as
a column in data frame.")
    (license license:gpl3+)))

(define-public r-rsqlite
  (package
    (name "r-rsqlite")
    (version "2.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "RSQLite" version))
              (sha256
               (base32
                "1xpg1i1jkzkzv44k512k90c2vvl960pl2d4a3spwcha6yclyc3vz"))))
    (properties `((upstream-name . "RSQLite")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-dbi" ,r-dbi)
       ("r-bh" ,r-bh)
       ("r-memoise" ,r-memoise)
       ("r-plogr" ,r-plogr)
       ("r-rcpp" ,r-rcpp)
       ("r-bit64" ,r-bit64)
       ("r-blob" ,r-blob)
       ("r-pkgconfig" ,r-pkgconfig)))
    (home-page "https://github.com/rstats-db/RSQLite")
    (synopsis "SQLite interface for R")
    (description
     "This package embeds the SQLite database engine in R and provides an
interface compliant with the DBI package.  The source for the SQLite
engine (version 3.8.8.2) is included.")
    (license license:lgpl2.0+)))

(define-public r-rcurl
  (package
    (name "r-rcurl")
    (version "1.95-0.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.bioconductor.org/packages/"
                                  "release/extra/src/"
                                  "contrib/RCurl_" version ".tar.gz"))
              (sha256
               (base32
                "0l7qi45jxlf898n0jazabnam1yyczvqfdknd00bdirhhiplpd1sc"))))
    (properties `((upstream-name . "RCurl")))
    (build-system r-build-system)
    (inputs
     `(("libcurl" ,curl)))
    (propagated-inputs
     `(("r-bitops" ,r-bitops)))
    (home-page "http://www.omegahat.org/RCurl")
    (synopsis "General network client interface for R")
    (description
     "The package allows one to compose general HTTP requests and provides
convenient functions to fetch URIs, GET and POST forms, etc. and process the
results returned by the Web server.  This provides a great deal of control
over the HTTP/FTP/... connection and the form of the request while providing a
higher-level interface than is available just using R socket connections.
Additionally, the underlying implementation is robust and extensive,
supporting FTP/FTPS/TFTP (uploads and downloads), SSL/HTTPS, telnet, dict,
ldap, and also supports cookies, redirects, authentication, etc.")
    (license license:bsd-3)))

(define-public r-xml
  (package
    (name "r-xml")
    (version "3.98-1.9")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "XML" version))
              (sha256
               (base32
                "1hzkdkgs0nzmfg9wm7kyh1j9i0i2g7925whzlihqvg9grdlh3dx3"))))
    (properties
     `((upstream-name . "XML")))
    (build-system r-build-system)
    (inputs
     `(("libxml2" ,libxml2)))
    (home-page "http://www.omegahat.org/RSXML")
    (synopsis "Tools for parsing and generating XML within R")
    (description
     "Many approaches for both reading and creating XML (and HTML)
documents (including DTDs), both local and accessible via HTTP or FTP.  Also
offers access to an XPath \"interpreter\".")
    (license license:bsd-2)))

(define-public r-xnomial
  (package
    (name "r-xnomial")
    (version "1.0.4")
    (source
     (origin (method url-fetch)
             (uri (cran-uri "XNomial" version))
             (sha256
              (base32
               "1mwx302576rmsjllbq2clfxilm3hkyp5bw0wmwqbn0kgv5wpy8z6"))))
    (properties (quasiquote ((upstream-name . "XNomial"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/XNomial")
    (synopsis "Goodness-of-Fit test for multinomial data")
    (description
     "This package provides an exact Goodness-of-Fit test for
multinomial data with fixed probabilities.  It can be used to
determine whether a set of counts fits a given expected ratio.  To see
whether a set of observed counts fits an expectation, one can examine
all possible outcomes with @code{xmulti()} or a random sample of them
with @code{xmonte()} and find the probability of an observation
deviating from the expectation by at least as much as the observed.
As a measure of deviation from the expected, one can use the
log-likelihood ratio, the multinomial probability, or the classic
chi-square statistic.  A histogram of the test statistic can also be
plotted and compared with the asymptotic curve.")
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-lambda-r
  (package
    (name "r-lambda-r")
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "lambda.r" version))
              (sha256
               (base32
                "0vql32np716dpd0kjn7s7wgawd02ysgp2a5il4kb19nlw661ii3x"))))
    (properties `((upstream-name . "lambda.r")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/lambda.r")
    (synopsis "Functional programming extension for R")
    (description
     "This package provides a language extension to efficiently write
functional programs in R.  Syntax extensions include multi-part function
definitions, pattern matching, guard statements, built-in (optional) type
safety.")
    (license license:lgpl3+)))

(define-public r-futile-options
  (package
    (name "r-futile-options")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "futile.options" version))
              (sha256
               (base32
                "1hp82h6xqq5cck67h7lpf22n3j7mg3v1mla5y5ivnzrrb7iyr17f"))))
    (properties
     `((upstream-name . "futile.options")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/futile.options")
    (synopsis "Options management framework")
    (description
     "The futile.options subsystem provides an easy user-defined options
management system that is properly scoped.  This means that options created
via @code{futile.options} are fully self-contained and will not collide with
options defined in other packages.")
    (license license:lgpl3+)))

(define-public r-futile-logger
  (package
    (name "r-futile-logger")
    (version "1.4.3")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "futile.logger" version))
              (sha256
               (base32
                "1r3nayk0z9n1svbf8640vw90dal5q07nkn0gv4bnva3pbzb352sy"))))
    (properties `((upstream-name . "futile.logger")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-futile-options" ,r-futile-options)
       ("r-lambda-r" ,r-lambda-r)))
    (home-page "https://cran.r-project.org/web/packages/futile.logger")
    (synopsis "Logging utility for R")
    (description
     "This package provides a simple yet powerful logging utility.  Based
loosely on log4j, futile.logger takes advantage of R idioms to make logging a
convenient and easy to use replacement for @code{cat} and @code{print}
statements.")
    (license license:lgpl3+)))

(define-public r-segmented
  (package
    (name "r-segmented")
    (version "0.5-3.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "segmented" version))
       (sha256
        (base32
         "0nrik5fyq59hwiwjcpbi4p5yfavgfjq6wyrynhkrbm4k6v1g1wlq"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/segmented")
    (synopsis "Regression models with breakpoints estimation")
    (description
     "Given a regression model, segmented updates the model by adding one or
more segmented (i.e., piecewise-linear) relationships.  Several variables with
multiple breakpoints are allowed.")
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-snow
  (package
    (name "r-snow")
    (version "0.4-2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "snow" version))
              (sha256
               (base32
                "1mxbrkpnmq32x4wd0194d541661yvfrrjlr3lsf7qq53ms3h21zf"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/snow")
    (synopsis "Support for simple parallel computing in R")
    (description
     "The snow package provides support for simple parallel computing on a
network of workstations using R.  A master R process calls @code{makeCluster}
to start a cluster of worker processes; the master process then uses functions
such as @code{clusterCall} and @code{clusterApply} to execute R code on the
worker processes and collect and return the results on the master.")
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-sparsem
  (package
    (name "r-sparsem")
    (version "1.77")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "SparseM" version))
              (sha256
               (base32
                "0p6ljma2h12cq1xmy0cxb48ih8dhxxbnwkqzvx3cckxf2kprycm9"))))
    (properties
     `((upstream-name . "SparseM")))
    (inputs
     `(("gfortran" ,gfortran)))
    (build-system r-build-system)
    (home-page "http://www.econ.uiuc.edu/~roger/research/sparse/sparse.html")
    (synopsis "Sparse linear algebra")
    (description
     "This package provides some basic linear algebra functionality for sparse
matrices.  It includes Cholesky decomposition and backsolving as well as
standard R subsetting and Kronecker products.")
    (license license:gpl2+)))

(define-public r-iterators
  (package
    (name "r-iterators")
    (version "1.0.9")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "iterators" version))
       (sha256
        (base32
         "16sycjq912ix52fjxjhcwiaqr0yj1v5iqmrvjljd3z857031w06y"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/iterators")
    (synopsis "Iterator construct for R")
    (description
     "This package provides support for iterators, which allow a programmer to
traverse through all the elements of a vector, list, or other collection of
data.")
    (license license:asl2.0)))

(define-public r-foreach
  (package
    (name "r-foreach")
    (version "1.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "foreach" version))
       (sha256
        (base32
         "0j2yj0rn0d5nbzz9nq5rqqgnxhp9pbd92q4klsarl2xpsn8119y0"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-codetools" ,r-codetools)
       ("r-iterators" ,r-iterators)))
    (home-page "https://cran.r-project.org/web/packages/foreach")
    (synopsis "Foreach looping construct for R")
    (description
     "This package provides support for the @code{foreach} looping construct.
@code{foreach} is an idiom that allows for iterating over elements in a
collection, without the use of an explicit loop counter.  This package in
particular is intended to be used for its return value, rather than for its
side effects.  In that sense, it is similar to the standard @code{lapply}
function, but doesn't require the evaluation of a function.  Using
@code{foreach} without side effects also facilitates executing the loop in
parallel.")
    (license license:asl2.0)))

(define-public r-doparallel
  (package
    (name "r-doparallel")
    (version "1.0.11")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "doParallel" version))
       (sha256
        (base32
         "0sppgxk3d8mfsrb3cjdyn0mv0s4i7pcy8g8c3cjzbr6k8vmx5jsc"))))
    (properties `((upstream-name . "doParallel")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-foreach" ,r-foreach)
       ("r-iterators" ,r-iterators)))
    (home-page "https://cran.r-project.org/web/packages/doParallel")
    (synopsis "Foreach parallel adaptor for the 'parallel' package")
    (description
     "This package provides a parallel backend for the @code{%dopar%} function
using the parallel package.")
    (license license:gpl2+)))

(define-public r-domc
  (package
    (name "r-domc")
    (version "1.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "doMC" version))
       (sha256
        (base32
         "1vfrykvfvsyq12mypd266867ml1dcwc3rj5k9c3wrn5bddcm88kr"))))
    (properties `((upstream-name . "doMC")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-foreach" ,r-foreach)
       ("r-iterators" ,r-iterators)))
    (home-page "https://cran.r-project.org/web/packages/doMC")
    (synopsis "Foreach parallel adaptor for the 'parallel' package")
    (description
     "This package provides a parallel backend for the @code{%dopar%} function
using the multicore functionality of the parallel package.")
    (license license:gpl2+)))

(define-public r-dt
  (package
    (name "r-dt")
    (version "0.2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "DT" version))
              (sha256
               (base32
                "1g86p0jcjqi2ph5rhm45jkzibsa6yfcj8n5cg3giy90sqgjzkdx1"))))
    (properties
     `((upstream-name . "DT")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-htmltools" ,r-htmltools)
       ("r-htmlwidgets" ,r-htmlwidgets)
       ("r-magrittr" ,r-magrittr)))
    (home-page "http://rstudio.github.io/DT")
    (synopsis "R wrapper of the DataTables JavaScript library")
    (description
     "This package allows for data objects in R to be rendered as HTML tables
using the JavaScript library 'DataTables' (typically via R Markdown or Shiny).
The 'DataTables' library has been included in this R package.")
    ;; The DT package as a whole is distributed under GPLv3.  The DT package
    ;; inludes other software components under different licenses:
    ;;
    ;;   * Expat: jQuery, jquery.highlight.js, DataTables
    ;;   * ASL2.0: selectize.js
    ;;   * WTFPL: noUiSlider
    (license (list license:gpl3
                   license:expat
                   license:asl2.0
                   (license:non-copyleft "http://www.wtfpl.net/txt/copying/")))))

(define-public r-base64enc
  (package
    (name "r-base64enc")
    (version "0.1-3")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "base64enc" version))
              (sha256
               (base32
                "13b89fhg1nx7zds82a0biz847ixphg9byf5zl2cw9kab6s56v1bd"))))
    (build-system r-build-system)
    (home-page "http://www.rforge.net/base64enc")
    (synopsis "Tools for Base64 encoding")
    (description
     "This package provides tools for handling Base64 encoding.  It is more
flexible than the orphaned \"base64\" package.")
    (license license:gpl2+)))

(define-public r-irlba
  (package
    (name "r-irlba")
    (version "2.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "irlba" version))
       (sha256
        (base32
         "1dclzzkm75sbbqda5scj2hlqnw2sksgxbiqagxs4359fwfszi0k0"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-matrix" ,r-matrix)))
    (home-page "https://cran.r-project.org/web/packages/irlba")
    (synopsis "Methods for eigendecomposition of large matrices")
    (description
     "This package provides fast and memory efficient methods for truncated
singular and eigenvalue decompositions, as well as for principal component
analysis of large sparse or dense matrices.")
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-glmnet
  (package
   (name "r-glmnet")
   (version "2.0-13")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "glmnet" version))
     (sha256
      (base32
       "1zdqp6wnqxzp5qn2ky47phbkrxv3cpgbwmdp896h3xxjvp58sa7k"))))
   (build-system r-build-system)
   (inputs
    `(("gfortran" ,gfortran)))
   (propagated-inputs
    `(("r-foreach" ,r-foreach)
      ("r-matrix" ,r-matrix)))
   (home-page "http://www.jstatsoft.org/v33/i01")
   (synopsis "Lasso and elastic-net regularized generalized linear models")
   (description
    "The glmnet package provides efficient procedures for fitting the entire
lasso or elastic-net regularization path for linear and Poisson regression, as
well as logistic, multinomial, Cox, multiple-response Gaussian and grouped
multinomial models.  The algorithm uses cyclical coordinate descent in a
path-wise fashion.")
   (license license:gpl2+)))

(define-public r-pkgmaker
  (package
    (name "r-pkgmaker")
    (version "0.22")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "pkgmaker" version))
       (sha256
        (base32
         "0vrqnd3kg6liqvpbd969jjsdx0f0rvmmxgdbwwrp6xfmdg0pib8r"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-codetools" ,r-codetools)
       ("r-digest" ,r-digest)
       ("r-registry" ,r-registry)
       ("r-stringr" ,r-stringr)
       ("r-xtable" ,r-xtable)))
    (home-page "https://renozao.github.io/pkgmaker")
    (synopsis "Package development utilities")
    (description
     "This package provides some low-level utilities to use for R package
development.  It currently provides managers for multiple package specific
options and registries, vignette, unit test and bibtex related utilities.")
    (license license:gpl2+)))

 (define-public r-registry
   (package
     (name "r-registry")
     (version "0.5")
     (source
      (origin
        (method url-fetch)
        (uri (cran-uri "registry" version))
        (sha256
         (base32
          "1yqfl1g6vsl28zn8brzc39659k8lqsmfms7900j7p64ilydyb2sx"))))
     (build-system r-build-system)
     (home-page "https://cran.r-project.org/web/packages/registry")
     (synopsis "Infrastructure for R package registries")
     (description
      "This package provides a generic infrastructure for creating and using R
package registries.")
     (license license:gpl2+)))

(define-public r-rngtools
  (package
    (name "r-rngtools")
    (version "1.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rngtools" version))
       (sha256
        (base32
         "1fcgfqrrb48z37xgy8sffx91p9irp39yqzxv7nqp1x2hnwsrh097"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-digest" ,r-digest)
       ("r-pkgmaker" ,r-pkgmaker)
       ("r-stringr" ,r-stringr)))
    (home-page "https://renozao.github.io/rngtools")
    (synopsis "Utility functions for working with random number generators")
    (description
     "This package contains a set of functions for working with Random Number
Generators (RNGs).  In particular, it defines a generic S4 framework for
getting/setting the current RNG, or RNG data that are embedded into objects
for reproducibility.  Notably, convenient default methods greatly facilitate
the way current RNG settings can be changed.")
    (license license:gpl3+)))

(define-public r-rtsne
  (package
    (name "r-rtsne")
    (version "0.13")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Rtsne" version))
       (sha256
        (base32
         "17crbdi80q4g2pwp9v7j3bdaznk96qlrqx01zvj3wwqippizyfqw"))))
    (properties `((upstream-name . "Rtsne")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rcpp" ,r-rcpp)))
    (home-page "https://github.com/jkrijthe/Rtsne")
    (synopsis "T-distributed stochastic neighbor embedding")
    (description
     "This package provides an R wrapper around the fast T-distributed
Stochastic Neighbor Embedding using a Barnes-Hut implementation.")
    ;; The declared license for this package is BSD-3, but it also includes
    ;; code licensed under BSD-4.
    (license (list license:bsd-3 license:bsd-4))))

(define-public r-e1071
  (package
    (name "r-e1071")
    (version "1.6-8")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "e1071" version))
       (sha256
        (base32
         "08n6i26nfckjpxjkzi8phhanc3ahsrirkv5rz38y2jcv7ds031pn"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-class" ,r-class)))
    (home-page "https://cran.r-project.org/web/packages/e1071")
    (synopsis "Miscellaneous functions for probability theory")
    (description
     "This package provides functions for latent class analysis, short time
Fourier transform, fuzzy clustering, support vector machines, shortest path
computation, bagged clustering, naive Bayes classifier, and more.")
    (license license:gpl2+)))

(define-public r-bigmemory-sri
  (package
    (name "r-bigmemory-sri")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "bigmemory.sri" version))
       (sha256
        (base32 "0mg14ilwdkd64q2ri9jdwnk7mp55dqim7xfifrs65sdsv1934h2m"))))
    (properties
     `((upstream-name . "bigmemory.sri")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/bigmemory.sri")
    (synopsis "Shared resource interface for the bigmemory package")
    (description "This package provides a shared resource interface for the
bigmemory and synchronicity packages.")
    ;; Users can choose either LGPLv3 or ASL2.0.
    (license (list license:lgpl3 license:asl2.0))))

(define-public r-synchronicity
  (package
    (name "r-synchronicity")
    (version "1.1.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "synchronicity" version))
       (sha256
        (base32
         "0d9skpwmsnkn4xb3f2kgyyv8bhdi0r9p1kj3cvi0s92fjjnpi00c"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bh" ,r-bh)
       ("r-bigmemory-sri" ,r-bigmemory-sri)
       ("r-rcpp" ,r-rcpp)))
    (home-page "http://www.bigmemory.org")
    (synopsis "Boost mutex functionality in R")
    (description "This package provides support for synchronization
via mutexes and may eventually support interprocess communication and
message passing.")
    ;; Users can choose either LGPLv3 or ASL2.0.
    (license (list license:lgpl3 license:asl2.0))))

(define-public r-bigmemory
  (package
    (name "r-bigmemory")
    (version "4.5.31")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "bigmemory" version))
       (sha256
        (base32
         "0wv0p746zz5a6r1yd0fgpj2kfcyzk2f4rxqz92p6lp9jzakv4lfs"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bh" ,r-bh)
       ("r-rcpp" ,r-rcpp)
       ("r-bigmemory-sri" ,r-bigmemory-sri)))
    (home-page "http://www.bigmemory.org")
    (synopsis "Manage large matrices with shared memory or memory-mapped files")
    (description "This package provides methods to create, store, access, and
manipulate large matrices.  Matrices are allocated to shared memory and may use
memory-mapped files.")
    ;; Users can choose either LGPLv3 or ASL2.0.
    (license (list license:lgpl3 license:asl2.0))))

(define-public r-nmf
  (package
    (name "r-nmf")
    (version "0.20.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "NMF" version))
       (sha256
        (base32
         "0mmh9bz0zjwd8h9jplz4rq3g94npaqj8s4px51vcv47csssd9k6z"))))
    (properties `((upstream-name . "NMF")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-cluster" ,r-cluster)
       ("r-bigmemory" ,r-bigmemory)
       ("r-synchronicity" ,r-synchronicity)
       ("r-colorspace" ,r-colorspace)
       ("r-digest" ,r-digest)
       ("r-doparallel" ,r-doparallel)
       ("r-foreach" ,r-foreach)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gridbase" ,r-gridbase)
       ("r-pkgmaker" ,r-pkgmaker)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-registry" ,r-registry)
       ("r-reshape2" ,r-reshape2)
       ("r-rngtools" ,r-rngtools)
       ("r-stringr" ,r-stringr)))
    (home-page "http://renozao.github.io/NMF")
    (synopsis "Algorithms and framework for nonnegative matrix factorization")
    (description
     "This package provides a framework to perform Non-negative Matrix
Factorization (NMF).  The package implements a set of already published
algorithms and seeding methods, and provides a framework to test, develop and
plug new or custom algorithms.  Most of the built-in algorithms have been
optimized in C++, and the main interface function provides an easy way of
performing parallel computations on multicore machines.")
    (license license:gpl2+)))

(define-public r-r-methodss3
  (package
    (name "r-r-methodss3")
    (version "1.7.1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "R.methodsS3" version))
              (sha256
               (base32
                "11z6v2i7jl647wxi9p5z66yvfnnqv6s7fxqmz7w2gkb6j8wl1f24"))))
    (properties `((upstream-name . "R.methodsS3")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/R.methodsS3")
    (synopsis "S3 methods simplified")
    (description
     "This package provides methods that simplify the setup of S3 generic
functions and S3 methods.  Major effort has been made in making definition of
methods as simple as possible with a minimum of maintenance for package
developers.  For example, generic functions are created automatically, if
missing, and naming conflict are automatically solved, if possible.  The
method @code{setMethodS3()} is a good start for those who in the future may
want to migrate to S4.")
    (license license:lgpl2.1+)))

(define-public r-r-oo
  (package
    (name "r-r-oo")
    (version "1.21.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "R.oo" version))
              (sha256
               (base32
                "0723gxjazgqq7v3lwnl7axw3brzcnizvhbd71ijkkv8mz31fwp34"))))
    (properties `((upstream-name . "R.oo")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-r-methodss3" ,r-r-methodss3)))
    (home-page "https://github.com/HenrikBengtsson/R.oo")
    (synopsis "R object-oriented programming with or without references")
    (description
     "This package provides methods and classes for object-oriented
programming in R with or without references.  Large effort has been made on
making definition of methods as simple as possible with a minimum of
maintenance for package developers.")
    (license license:lgpl2.1+)))

(define-public r-r-utils
  (package
    (name "r-r-utils")
    (version "2.6.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "R.utils" version))
              (sha256
               (base32
                "03j7hrs03kyj9qrjxyp5kqv4lpqqpk6xwbkzx6j15d8928yrjr2x"))))
    (properties `((upstream-name . "R.utils")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-r-methodss3" ,r-r-methodss3)
       ("r-r-oo" ,r-r-oo)))
    (home-page "https://github.com/HenrikBengtsson/R.utils")
    (synopsis "Various programming utilities")
    (description
     "This package provides utility functions useful when programming and
developing R packages.")
    (license license:lgpl2.1+)))

(define-public r-r-cache
  (package
    (name "r-r-cache")
    (version "0.12.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "R.cache" version))
              (sha256
               (base32
                "006x52w9r8phw5hgqmyp0bz8z42vn8p5yibibnzi1sfa1xlw8iyx"))))
    (properties `((upstream-name . "R.cache")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-digest" ,r-digest)
       ("r-r-methodss3" ,r-r-methodss3)
       ("r-r-oo" ,r-r-oo)
       ("r-r-utils" ,r-r-utils)))
    (home-page "https://github.com/HenrikBengtsson/R.cache")
    (synopsis "Light-weight caching of objects and results")
    (description
     "This package provides methods for caching or memoization of objects and
results.  With this package, any R object can be cached in a key-value storage
where the key can be an arbitrary set of R objects.  The cache memory is
persistent (on the file system).")
    (license license:lgpl2.1+)))

(define-public r-r-rsp
  (package
    (name "r-r-rsp")
    (version "0.41.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "R.rsp" version))
              (sha256
               (base32
                "01l430avj5ggmdsla2kiriix72g0dlzx9klniq321bs0za808v3c"))))
    (properties `((upstream-name . "R.rsp")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-r-cache" ,r-r-cache)
       ("r-r-methodss3" ,r-r-methodss3)
       ("r-r-oo" ,r-r-oo)
       ("r-r-utils" ,r-r-utils)))
    (home-page "https://github.com/HenrikBengtsson/R.rsp")
    (synopsis "Dynamic generation of scientific reports")
    (description
     "The RSP markup language provides a powerful markup for controlling the
content and output of LaTeX, HTML, Markdown, AsciiDoc, Sweave and knitr
documents (and more), e.g. @code{Today's date is <%=Sys.Date()%>}.  Contrary
to many other literate programming languages, with RSP it is straightforward
to loop over mixtures of code and text sections, e.g.  in month-by-month
summaries.  RSP has also several preprocessing directives for incorporating
static and dynamic contents of external files (local or online) among other
things.  RSP is ideal for self-contained scientific reports and R package
vignettes.")
    (license license:lgpl2.1+)))

(define-public r-mvtnorm
  (package
    (name "r-mvtnorm")
    (version "1.0-6")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "mvtnorm" version))
              (sha256
               (base32
                "0i74s9dl4jf1gln6agra8h38qn9ifd5v0gi13caj1da5nrbmn0aa"))))
    (build-system r-build-system)
    (inputs
     `(("gfortran" ,gfortran)))
    (home-page "http://mvtnorm.R-forge.R-project.org")
    (synopsis "Package for multivariate normal and t-distributions")
    (description "This package can compute multivariate normal and
t-probabilities, quantiles, random deviates and densities.")
    (license license:gpl2)))

(define-public r-matrixstats
  (package
    (name "r-matrixstats")
    (version "0.52.2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "matrixStats" version))
              (sha256
               (base32
                "19fjf19cr0p3j9pj17myz39zpi5bh7ci72db3la9my09n6k6mnir"))))
    (properties `((upstream-name . "matrixStats")))
    (build-system r-build-system)
    (native-inputs
     `(("r-r-rsp" ,r-r-rsp))) ;used to build vignettes
    (home-page "https://github.com/HenrikBengtsson/matrixStats")
    (synopsis "Methods applying to vectors and matrix rows and columns")
    (description
     "This package provides methods operating on rows and columns of matrices,
e.g.  @code{rowMedians()}, @code{rowRanks()}, and @code{rowSds()}.  There are
also some vector-based methods, e.g. @code{binMeans()}, @code{madDiff()} and
@code{weightedMedians()}.  All methods have been optimized for speed and
memory usage.")
    (license license:artistic2.0)))

(define-public r-viridis
  (package
    (name "r-viridis")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "viridis" version))
              (sha256
               (base32
                "03mha2935k1mw7mjjkq7mrx185hl0m3vq3967iiwbknpivbdxllk"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-gridextra" ,r-gridextra)
       ("r-viridislite" ,r-viridislite)))
    (home-page "https://github.com/sjmgarnier/viridis")
    (synopsis "Matplotlib default color map")
    (description
     "This package is a port of the new @url{matplotlib,
http://matplotlib.org/} color maps (@code{viridis}--the default--,
@code{magma}, @code{plasma}, and @code{inferno}) to R.  These color maps are
designed in such a way that they will analytically be perfectly
perceptually-uniform, both in regular form and also when converted to
black-and-white.  They are also designed to be perceived by readers with the
most common form of color blindness.")
    (license license:x11)))

(define-public r-viridislite
  (package
    (name "r-viridislite")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "viridisLite" version))
       (sha256
        (base32
         "1546h44ng4dxs130jkh6lkh990hmp90m1w45sq5p47n546gr0k9d"))))
    (properties `((upstream-name . "viridisLite")))
    (build-system r-build-system)
    (home-page "https://github.com/sjmgarnier/viridisLite")
    (synopsis "Default color maps from matplotlib")
    (description
     "This package is a port of the new @code{matplotlib} color maps ('viridis',
'magma', 'plasma' and 'inferno') to R.  matplotlib is a popular plotting
library for Python.  These color maps are designed in such a way that they
will analytically be perfectly perceptually-uniform, both in regular form and
also when converted to black-and-white.  They are also designed to be
perceived by readers with the most common form of color blindness.  This is
the 'lite' version of the more complete @code{viridis} package.")
    (license license:expat)))

(define-public r-tidyselect
  (package
    (name "r-tidyselect")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "tidyselect" version))
       (sha256
        (base32
         "0b0w30yq30c7bwpaghp3hp0nncqwcmvrra9j58fhx9jihay3l68c"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-glue" ,r-glue)
       ("r-purrr" ,r-purrr)
       ("r-rcpp" ,r-rcpp)
       ("r-rlang" ,r-rlang)))
    (home-page "https://cran.r-project.org/web/packages/tidyselect")
    (synopsis "Select from a set of strings")
    (description
     "This package provides a backend for the selecting functions of the
tidyverse.  It makes it easy to implement select-like functions in your own
packages in a way that is consistent with other tidyverse interfaces for
selection.")
    (license license:gpl3)))

(define-public r-tidyr
  (package
    (name "r-tidyr")
    (version "0.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "tidyr" version))
       (sha256
        (base32
         "1700fry2b3d3ksj7x2f09xl6agjrdnx1rqsc1r8gvzsp5cpflb06"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-dplyr" ,r-dplyr)
       ("r-magrittr" ,r-magrittr)
       ("r-glue" ,r-glue)
       ("r-purrr" ,r-purrr)
       ("r-rlang" ,r-rlang)
       ("r-tidyselect" ,r-tidyselect)
       ("r-rcpp" ,r-rcpp)
       ("r-stringi" ,r-stringi)
       ("r-tibble" ,r-tibble)))
    (home-page "https://github.com/hadley/tidyr")
    (synopsis "Tidy data with `spread()` and `gather()` functions")
    (description
     "tidyr is a reframing of the reshape2 package designed to accompany the
tidy data framework, and to work hand-in-hand with magrittr and dplyr to build
a solid pipeline for data analysis.  It is designed specifically for tidying
data, not the general reshaping that reshape2 does, or the general aggregation
that reshape did.  In particular, built-in methods only work for data frames,
and tidyr provides no margins or aggregation.")
    (license license:expat)))

(define-public r-hexbin
  (package
    (name "r-hexbin")
    ;; The package tarball was updated in place, resulting in a change in the
    ;; hash value.  We decided to bump the version to 1.27.1-1 instead of
    ;; keeping the version at upstream's 1.27.1.
    (version "1.27.1-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "hexbin" "1.27.1"))
       (sha256
        (base32
         "025d609z1nyy684hwvp34b9mjzkgvild7fvfr95f941dmsikan87"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-lattice" ,r-lattice)))
    (native-inputs
     `(("gfortran" ,gfortran)))
    (home-page "https://github.com/edzer/hexbin")
    (synopsis "Hexagonal binning routines")
    (description
     "This package provides binning and plotting functions for hexagonal bins.
It uses and relies on grid graphics and formal (S4) classes and methods.")
    (license license:gpl2+)))

(define-public r-purrr
  (package
    (name "r-purrr")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "purrr" version))
       (sha256
        (base32
         "0wgbnbrqncjpggg8f9bdpcd1gy351s5f0az5k8lc55cvs9lhz3gd"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-tibble" ,r-tibble)
       ("r-magrittr" ,r-magrittr)
       ("r-rlang" ,r-rlang)))
    (home-page "https://github.com/hadley/purrr")
    (synopsis "Functional programming tools")
    (description
     "This package completes R's functional programming tools with missing
features present in other programming languages.")
    (license license:gpl3+)))

(define-public r-plotly
  (package
    (name "r-plotly")
    (version "4.7.1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "plotly" version))
              (sha256
               (base32
                "0wj9lw7w28z8w9ip9vadv6sydjhqyg65kfiai9m3bndzz50b1m3w"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-base64enc" ,r-base64enc)
       ("r-crosstalk" ,r-crosstalk)
       ("r-digest" ,r-digest)
       ("r-data-table" ,r-data-table)
       ("r-dplyr" ,r-dplyr)
       ("r-ggplot2" ,r-ggplot2)
       ("r-hexbin" ,r-hexbin)
       ("r-htmltools" ,r-htmltools)
       ("r-htmlwidgets" ,r-htmlwidgets)
       ("r-httr" ,r-httr)
       ("r-jsonlite" ,r-jsonlite)
       ("r-lazyeval" ,r-lazyeval)
       ("r-magrittr" ,r-magrittr)
       ("r-purrr" ,r-purrr)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-scales" ,r-scales)
       ("r-tibble" ,r-tibble)
       ("r-tidyr" ,r-tidyr)
       ("r-viridislite" ,r-viridislite)))
    (home-page "https://plot.ly/r")
    (synopsis "Create interactive web graphics")
    (description
     "This package enables the translation of ggplot2 graphs to an interactive
web-based version and/or the creation of custom web-based visualizations
directly from R.  Once uploaded to a plotly account, plotly graphs (and the
data behind them) can be viewed and modified in a web browser.")
    (license license:x11)))

(define-public r-biased-urn
  (package
   (name "r-biased-urn")
   (version "1.07")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "BiasedUrn" version))
     (sha256
      (base32
       "13i2lgfnjhlbbm2yxfc2l5hswqw6x03pwba5csjmirv8kpjw4xr3"))))
   (properties `((upstream-name . "BiasedUrn")))
   (build-system r-build-system)
   (home-page "http://www.agner.org/random/")
   (synopsis "Biased urn model distributions")
   (description
    "This package provides statistical models of biased sampling in the form
of univariate and multivariate noncentral hypergeometric distributions,
including Wallenius' noncentral hypergeometric distribution and Fisher's
noncentral hypergeometric distribution (also called extended hypergeometric
distribution).")
   (license license:gpl3+)))

(define-public r-rematch
  (package
    (name "r-rematch")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rematch" version))
       (sha256
        (base32
         "0y3mshvpvz9csmq8hk8jbabx4nxlv5sckvfzvm6920ndg34xw2d4"))))
    (build-system r-build-system)
    (home-page "https://github.com/MangoTheCat/rematch")
    (synopsis "Match regular expressions with a nicer API")
    (description
     "This package provides a small wrapper on @code{regexpr} to extract the
matches and captured groups from the match of a regular expression to a
character vector.")
    (license license:expat)))

(define-public r-cellranger
  (package
    (name "r-cellranger")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "cellranger" version))
       (sha256
        (base32
         "16fgi3annn34c3cxi0pxf62mmmmxi21hp0zzlv7bkfsjqy4g4f2x"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rematch" ,r-rematch)
       ("r-tibble" ,r-tibble)))
    (home-page "https://github.com/rsheets/cellranger")
    (synopsis "Translate spreadsheet cell ranges to rows and columns")
    (description
     "This package provides helper functions to work with spreadsheets and the
@code{A1:D10} style of cell range specification.")
    (license license:expat)))

(define-public r-googlesheets
  (package
    (name "r-googlesheets")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "googlesheets" version))
       (sha256
        (base32
         "18q0xmxn09b52rmky7gr5flp0awndcnsgb7zcvkzvkrkvmwad52b"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-cellranger" ,r-cellranger)
       ("r-dplyr" ,r-dplyr)
       ("r-httr" ,r-httr)
       ("r-jsonlite" ,r-jsonlite)
       ("r-purrr" ,r-purrr)
       ("r-readr" ,r-readr)
       ("r-stringr" ,r-stringr)
       ("r-tidyr" ,r-tidyr)
       ("r-xml2" ,r-xml2)))
    (home-page "https://github.com/jennybc/googlesheets")
    (synopsis "Manage Google spreadsheets from R")
    (description "This package provides tools to interact with Google Sheets
from within R.")
    (license license:expat)))

(define-public r-spams
  (package
    (name "r-spams")
    (version "2.6-2017-03-22")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://gforge.inria.fr/frs/download.php/36615/"
                           "spams-R-v" version ".tar.gz"))
       (sha256
        (base32
         "13z2293jixf1r9g8dyy856xrhvpjr2ln2n9smn6644126r9hmhkx"))))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "spams") #t))
         ;; Don't tune for the building machine
         (add-after 'chdir 'no-mtune
           (lambda _
             (substitute* "src/Makevars"
               (("-mtune=native") ""))
             #t)))))
    (propagated-inputs
     `(("r-lattice" ,r-lattice)
       ("r-matrix" ,r-matrix)))
    (home-page "http://spams-devel.gforge.inria.fr")
    (synopsis "Toolbox for solving sparse estimation problems")
    (description "SPAMS (SPArse Modeling Software) is an optimization toolbox
for solving various sparse estimation problems.  It includes tools for the
following problems:

@enumerate
@item Dictionary learning and matrix factorization (NMF, sparse @dfn{principle
 component analysis} (PCA), ...)
@item Solving sparse decomposition problems with LARS, coordinate descent,
 OMP, SOMP, proximal methods
@item Solving structured sparse decomposition problems (l1/l2, l1/linf, sparse
 group lasso, tree-structured regularization, structured sparsity with
 overlapping groups,...).
@end enumerate\n")
    (license license:gpl3+)))

(define-public r-base64
  (package
    (name "r-base64")
    (version "2.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "base64" version))
       (sha256
        (base32
         "1labh0ycdm2xcjssj8bhnyjvbk44mcdsi0rb2p8rfqa428mrq9cf"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-openssl" ,r-openssl)))
    (home-page "https://cran.r-project.org/web/packages/base64")
    (synopsis "Base64 encoder and decoder")
    (description
     "This package is a compatibility wrapper to replace the orphaned package
by Romain Francois.  New applications should use the openssl or base64enc
package instead.")
    (license license:expat)))

(define-public r-hmisc
  (package
    (name "r-hmisc")
    (version "4.0-3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Hmisc" version))
       (sha256
        (base32
         "1a7i7azag6pldgala85d8hh7wnx1shamyiriy4jfc65nxrr2lq8w"))))
    (properties `((upstream-name . "Hmisc")))
    (build-system r-build-system)
    (native-inputs
     `(("gfortran" ,gfortran)))
    (propagated-inputs
     `(("r-acepack" ,r-acepack)
       ("r-base64enc" ,r-base64enc)
       ("r-cluster" ,r-cluster)
       ("r-data-table" ,r-data-table)
       ("r-foreign" ,r-foreign)
       ("r-formula" ,r-formula)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gridextra" ,r-gridextra)
       ("r-gtable" ,r-gtable)
       ("r-lattice" ,r-lattice)
       ("r-latticeextra" ,r-latticeextra)
       ("r-htmltable" ,r-htmltable)
       ("r-htmltools" ,r-htmltools)
       ("r-nnet" ,r-nnet)
       ("r-rpart" ,r-rpart)
       ("r-survival" ,r-survival)
       ("r-viridis" ,r-viridis)))
    (home-page "http://biostat.mc.vanderbilt.edu/Hmisc")
    (synopsis "Miscellaneous data analysis and graphics functions")
    (description
     "This package contains many functions useful for data analysis,
high-level graphics, utility operations, functions for computing sample size
and power, importing and annotating datasets, imputing missing values,
advanced table making, variable clustering, character string manipulation,
conversion of R objects to LaTeX code, and recoding variables.")
    (license license:gpl2+)))

(define-public r-runit
  (package
    (name "r-runit")
    (version "0.4.31")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "RUnit" version))
       (sha256
        (base32
         "1jqr871jkll2xmk7wk5hv1z3a36hyn2ibgivw7bwk4b346940xlx"))))
    (properties `((upstream-name . "RUnit")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/RUnit")
    (synopsis "R unit test framework")
    (description
     "This package provides R functions implementing a standard unit testing
framework, with additional code inspection and report generation tools.")
    (license license:gpl2+)))

(define-public r-dynamictreecut
  (package
    (name "r-dynamictreecut")
    (version "1.63-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "dynamicTreeCut" version))
       (sha256
        (base32
         "1fadbql7g5r2vvlkr89nlrjxwp4yx4xrdqmv077qvmnx9vv0f4w3"))))
    (properties `((upstream-name . "dynamicTreeCut")))
    (build-system r-build-system)
    (home-page
     "http://www.genetics.ucla.edu/labs/horvath/CoexpressionNetwork/BranchCutting/")
    (synopsis "Detect clusters in hierarchical clustering dendrograms")
    (description
     "This package contains methods for the detection of clusters in
hierarchical clustering dendrograms.")
    (license license:gpl2+)))

(define-public r-preprocesscore
  (package
    (name "r-preprocesscore")
    (version "1.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (bioconductor-uri "preprocessCore" version))
       (sha256
        (base32
         "0mrdgbkb9dcfr9gsqyxfpshnxx60mq97pl98b8yl00l2gfwz48ph"))))
    (properties
     `((upstream-name . "preprocessCore")))
    (build-system r-build-system)
    (home-page "https://github.com/bmbolstad/preprocessCore")
    (synopsis "Collection of pre-processing functions")
    (description
     "This package provides a library of core pre-processing and normalization
routines.")
    (license license:lgpl2.0+)))

(define-public r-fastcluster
  (package
    (name "r-fastcluster")
    (version "1.1.24")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "fastcluster" version))
       (sha256
        (base32
         "15drhl22wm8whsy6b3vv754skfddiydb068zn1whrw5sknvkkjc2"))))
    (build-system r-build-system)
    (home-page "http://danifold.net/fastcluster.html")
    (synopsis "Fast hierarchical clustering routines")
    (description
     "This package implements fast hierarchical, agglomerative clustering
routines.  Part of the functionality is designed as drop-in replacement for
existing routines: @code{linkage()} in the SciPy package
@code{scipy.cluster.hierarchy}, @code{hclust()} in R's @code{stats} package,
and the @code{flashClust} package.  It provides the same functionality with
the benefit of a much faster implementation.  Moreover, there are
memory-saving routines for clustering of vector data, which go beyond what the
existing packages provide.")
    (license license:bsd-2)))

(define-public r-sfsmisc
  (package
    (name "r-sfsmisc")
    (version "1.1-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "sfsmisc" version))
       (sha256
        (base32
         "0jzmbywlyzfxs7hlmyd0iynghfc9qp5sa5lnhr73y8r360yv1ahf"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/sfsmisc")
    (synopsis "Utilities from \"Seminar fuer Statistik\" ETH Zurich")
    (description
     "This package provides useful utilities from Seminar fuer Statistik ETH
Zurich, including many that are related to graphics.")
    (license license:gpl2+)))

(define-public r-gtools
  (package
    (name "r-gtools")
    (version "3.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "gtools" version))
       (sha256
        (base32
         "1xknwk9xlsj027pg0nwiizigcrsc84hdrig0jn0cgcyxj8dabdl6"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/gtools")
    (synopsis "Various R programming tools")
    (description
     "This package contains a collection of various functions to assist in R
programming, such as tools to assist in developing, updating, and maintaining
R and R packages, calculating the logit and inverse logit transformations,
tests for whether a value is missing, empty or contains only @code{NA} and
@code{NULL} values, and many more.")
    (license license:gpl2)))

(define-public r-gdata
  (package
    (name "r-gdata")
    (version "2.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "gdata" version))
       (sha256
        (base32
         "0zwdj7lscgxr8r62ii8hbdh4mb7sa9w4f5nv32zzrxdvymcpya2b"))))
    (build-system r-build-system)
    (inputs
     `(("perl" ,perl)))
    (propagated-inputs
     `(("r-gtools" ,r-gtools)))
    (home-page "https://cran.r-project.org/web/packages/gdata")
    (synopsis "Various R programming tools for data manipulation")
    (description
     "This package provides various R programming tools for data manipulation,
including:

@itemize
@item medical unit conversions
@item combining objects
@item character vector operations
@item factor manipulation
@item obtaining information about R objects
@item manipulating MS-Excel formatted files
@item generating fixed-width format files
@item extricating components of date and time objects
@item operations on columns of data frames
@item matrix operations
@item operations on vectors and data frames
@item value of last evaluated expression
@item wrapper for @code{sample} that ensures consistent behavior for
  both scalar and vector arguments
@end itemize\n")
    (license license:gpl2+)))

(define-public r-gplots
  (package
    (name "r-gplots")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "gplots" version))
       (sha256
        (base32
         "02nb8n3s7c1zxq2s7ycaq2ys72y7mzirxrwj954h6gdc4x1zhg9l"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-catools" ,r-catools)
       ("r-gdata" ,r-gdata)
       ("r-gtools" ,r-gtools)
       ("r-kernsmooth" ,r-kernsmooth)))
    (home-page "https://cran.r-project.org/web/packages/gplots")
    (synopsis "Various R programming tools for plotting data")
    (description
     "This package provides various R programming tools for plotting data,
including:

@itemize
@item calculating and plotting locally smoothed summary function
@item enhanced versions of standard plots
@item manipulating colors
@item calculating and plotting two-dimensional data summaries
@item enhanced regression diagnostic plots
@item formula-enabled interface to @code{stats::lowess} function
@item displaying textual data in plots
@item baloon plots
@item plotting \"Venn\" diagrams
@item displaying Open-Office style plots
@item plotting multiple data on same region, with separate axes
@item plotting means and confidence intervals
@item spacing points in an x-y plot so they don't overlap
@end itemize\n")
    (license license:gpl2+)))

(define-public r-rocr
  (package
    (name "r-rocr")
    (version "1.0-7")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ROCR" version))
       (sha256
        (base32
         "1jay8cm7lgq56i967vm5c2hgaxqkphfpip0gn941li3yhh7p3vz7"))))
    (properties `((upstream-name . "ROCR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-gplots" ,r-gplots)))
    (home-page "http://rocr.bioinf.mpi-sb.mpg.de/")
    (synopsis "Visualizing the performance of scoring classifiers")
    (description
     "ROCR is a flexible tool for creating cutoff-parameterized 2D performance
curves by freely combining two from over 25 performance measures (new
performance measures can be added using a standard interface).  Curves from
different cross-validation or bootstrapping runs can be averaged by different
methods, and standard deviations, standard errors or box plots can be used to
visualize the variability across the runs.  The parameterization can be
visualized by printing cutoff values at the corresponding curve positions, or
by coloring the curve according to cutoff.  All components of a performance
plot can be quickly adjusted using a flexible parameter dispatching
mechanism.")
    (license license:gpl2+)))

(define-public r-zoo
  (package
    (name "r-zoo")
    (version "1.8-0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "zoo" version))
              (sha256
               (base32
                "0mqklbx92ifwa0awm7gpm4r9dvwa09p55zjxjnypiqsxy532r4h9"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-lattice" ,r-lattice)))
    (home-page "http://zoo.R-Forge.R-project.org/")
    (synopsis "S3 infrastructure for regular and irregular time series")
    (description "This package contains an S3 class with methods for totally
ordered indexed observations.  It is particularly aimed at irregular time
series of numeric vectors/matrices and factors.")
    (license license:gpl2+)))

(define-public r-ztable
  (package
    (name "r-ztable")
    (version "0.1.5")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "ztable" version))
              (sha256
               (base32
                "1jfqnqy9544gfvz3bsb48v4177nwp4b4n9l2743asq8sbq305b5r"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/ztable")
    (synopsis "Zebra-striped tables in LaTeX and HTML formats for R")
    (description
     "This package provides functions to make zebra-striped tables (tables
with alternating row colors) in LaTeX and HTML formats easily from
@code{data.frame}, @code{matrix}, @code{lm}, @code{aov}, @code{anova},
@code{glm}, @code{coxph}, @code{nls}, @code{fitdistr}, @code{mytable} and
@code{cbind.mytable} objects.")
    (license license:gpl2+)))

(define-public r-vipor
  (package
    (name "r-vipor")
    (version "0.4.5")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "vipor" version))
              (sha256
               (base32
                "112gc0d7f8iavgf56pnzfxb7hy75yhd0zlyjzshdcfbnqcd2a6bx"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/vipor")
    (synopsis "Plot categorical data using noise and density estimates")
    (description
     "This package provides tools to generate a violin point plot, a
combination of a violin/histogram plot and a scatter plot by offsetting points
within a category based on their density using quasirandom noise.")
    (license license:gpl2+)))

(define-public r-beeswarm
  (package
    (name "r-beeswarm")
    (version "0.2.3")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "beeswarm" version))
              (sha256
               (base32
                "0hy89bwv7jixlg91li1fywa77916am2whqp1m1fx1khd45g44581"))))
    (build-system r-build-system)
    (home-page "http://www.cbs.dtu.dk/~eklund/beeswarm/")
    (synopsis "Implementation of bee swarm plots")
    (description
     "This package provides an implementation of bee swarm plots.  The bee
swarm plot is a one-dimensional scatter plot like stripchart, but with
closely-packed, non-overlapping points.")
    (license license:artistic2.0)))

(define-public r-sourcetools
  (package
    (name "r-sourcetools")
    (version "0.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "sourcetools" version))
       (sha256
        (base32
         "0l8c6fql82cb246qh7hfgxb4s35qn0qfgy6dzvkx0zkz1cpqvx69"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/sourcetools")
    (synopsis "Tools for reading, tokenizing and parsing R code")
    (description
     "The sourcetools package provides both an R and C++ interface for the
tokenization of R code, and helpers for interacting with the tokenized
representation of R code.")
    (license license:expat)))

(define-public r-ggbeeswarm
  (package
    (name "r-ggbeeswarm")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "ggbeeswarm" version))
              (sha256
               (base32
                "0crk29p5vi1r3a988kms4y7r0iqwgwzsikgvh18r9wbzyr98bb5v"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-beeswarm" ,r-beeswarm)
       ("r-ggplot2" ,r-ggplot2)
       ("r-vipor" ,r-vipor)))
    (home-page "https://github.com/eclarke/ggbeeswarm")
    (synopsis "Categorical scatter (violin point) plots")
    (description
     "This package provides two methods of plotting categorical scatter plots
such that the arrangement of points within a category reflects the density of
data at that region, and avoids over-plotting.")
    (license license:gpl2+)))

(define-public r-ggthemes
  (package
    (name "r-ggthemes")
    (version "3.4.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "ggthemes" version))
              (sha256
               (base32
                "1jj8lp7jbk3489kpgbw4b5phpn01gkfmksc21c6sn3x6wmzyn6hs"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-assertthat" ,r-assertthat)
       ("r-colorspace" ,r-colorspace)
       ("r-ggplot2" ,r-ggplot2)
       ("r-scales" ,r-scales)))
    (home-page "https://cran.rstudio.com/web/packages/ggthemes")
    (synopsis "Extra themes, scales and geoms for @code{ggplot2}")
    (description "This package provides extra themes and scales for
@code{ggplot2} that replicate the look of plots by Edward Tufte and
Stephen Few in Fivethirtyeight, The Economist, Stata, Excel, and The
Wall Street Journal, among others.  This package also provides
@code{geoms} for Tufte's box plot and range frame.")
    (license license:gpl2)))

(define-public r-statmod
  (package
    (name "r-statmod")
    (version "1.4.30")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "statmod" version))
              (sha256
               (base32
                "07v4x8af60alcw6vbiwf5fp25bhra61kvxz9kqx64lszm0i1fb4x"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/statmod")
    (native-inputs
     `(("gfortran" ,gfortran)))
    (synopsis "Statistical modeling")
    (description
     "This package provides a collection of algorithms and functions to aid
statistical modeling.  It includes growth curve comparisons, limiting dilution
analysis (aka ELDA), mixed linear models, heteroscedastic regression,
inverse-Gaussian probability calculations, Gauss quadrature and a secure
convergence algorithm for nonlinear models.  It also includes advanced
generalized linear model functions that implement secure convergence,
dispersion modeling and Tweedie power-law families.")
    ;; Statmod is distributed under either license
    (license (list license:gpl2 license:gpl3))))

(define-public r-rann
  (package
    (name "r-rann")
    (version "2.5.1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "RANN" version))
              (sha256
               (base32
                "0il5i99vbcagnxvb15af5n37g04a4q1x96bz73zh3jhki9fpw9vm"))))
    (properties
     `((upstream-name . "RANN")))
    (build-system r-build-system)
    (home-page "https://github.com/jefferis/RANN")
    (synopsis "Fast nearest neighbour search")
    (description
     "This package finds the k nearest neighbours for every point in a given
dataset in O(N log N) time using Arya and Mount's ANN library.  Provides
approximate, exact searches, fixed radius searches, bd and kb trees.")
    (license license:gpl3+)))

(define-public r-fivethirtyeight
  (package
    (name "r-fivethirtyeight")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://mran.microsoft.com/src/contrib/"
                           "fivethirtyeight_" version ".tar.gz"))
       (sha256
        (base32
         "0fcc8rq745nsghp27dk0lgih90y4zx8hrzcvsn6ih786yv7qxhvl"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-knitr" ,r-knitr)
       ("r-rmarkdown" ,r-rmarkdown)
       ("r-dplyr" ,r-dplyr)
       ("r-readr" ,r-readr)
       ("r-ggplot2" ,r-ggplot2)
       ("r-magrittr" ,r-magrittr)
       ("r-stringr" ,r-stringr)))
    (home-page "https://mran.microsoft.com/package/fivethirtyeight/")
    (synopsis "Data and code behind the stories at FiveThirtyEight")
    (description "This R package provides access to the code and data sets
published by the statistics blog FiveThirtyEight.")
    (license license:expat)))

(define-public r-compquadform
  (package
    (name "r-compquadform")
    (version "1.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "CompQuadForm" version))
       (sha256
        (base32
         "1i30hrqdk64q17vsn918c3q79brchgx2wzh1gbsgbn0dh1ncabq4"))))
    (properties `((upstream-name . "CompQuadForm")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/CompQuadForm")
    (synopsis "Distribution function of quadratic forms in normal variables")
    (description
     "This package provides functions to compute the distribution function of
quadratic forms in normal variables using Imhof's method, Davies's algorithm,
Farebrother's algorithm or Liu et al.'s algorithm.")
    (license license:gpl2+)))

(define-public r-cowplot
  (package
    (name "r-cowplot")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "cowplot" version))
       (sha256
        (base32
         "13yjw7yv7imyqiawqqp304hkp6x36iv6rf6gn03dwzwkj9zwx4lb"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-gtable" ,r-gtable)
       ("r-plyr" ,r-plyr)
       ("r-scales" ,r-scales)))
    (home-page "https://github.com/wilkelab/cowplot")
    (synopsis "Streamlined plot theme and plot annotations for ggplot2")
    (description
     "This package provides some helpful extensions and modifications to the
ggplot2 package to combine multiple ggplot2 plots into one and label them with
letters, as is often required for scientific publications.")
    (license license:gpl2)))

(define-public r-mixtools
  (package
    (name "r-mixtools")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "mixtools" version))
       (sha256
        (base32
         "13wdm0xs5bakhpa8ypg6lvhjaqkxyabwz4glxdwn0jwdvkcdhgsl"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-mass" ,r-mass)
       ("r-segmented" ,r-segmented)
       ("r-survival" ,r-survival)))
    (home-page "https://cran.r-project.org/web/packages/mixtools")
    (synopsis "Tools for analyzing finite mixture models")
    (description
     "This package provides a collection of R functions for analyzing finite
mixture models.")
    (license license:gpl2+)))

(define-public r-lars
  (package
    (name "r-lars")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "lars" version))
       (sha256
        (base32
         "0blj44wqrx6lmym1m9v6wkz8zxzbjax2zl6swgdczci0ixb5nx34"))))
    (build-system r-build-system)
    (inputs
     `(("gfortran" ,gfortran)))
    (home-page "http://www-stat.stanford.edu/~hastie/Papers/#LARS")
    (synopsis "Least angle regression software")
    (description
     "Least Angle Regression (\"LAR\") is a model selection algorithm; a
useful and less greedy version of traditional forward selection methods.  A
simple modification of the LAR algorithm implements Tibshirani's Lasso; the
Lasso modification of LARS calculates the entire Lasso path of coefficients
for a given problem at the cost of a single least squares fit.  Another LARS
modification efficiently implements epsilon Forward Stagewise linear
regression.")
    (license license:gpl2)))

(define-public r-fastica
  (package
    (name "r-fastica")
    (version "1.2-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "fastICA" version))
       (sha256
        (base32
         "108z2ymby5y4h8l4l2krqwm28rya93gq09yylgilnm3afvfrfabg"))))
    (properties `((upstream-name . "fastICA")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/fastICA")
    (synopsis "FastICA algorithms to perform ICA and projection pursuit")
    (description
     "This package provides an implementation of the FastICA algorithm to
perform @dfn{independent component analysis} (ICA) and projection pursuit.")
    ;; Any GPL version.
    (license license:gpl3+)))

(define-public r-randomforest
  (package
    (name "r-randomforest")
    (version "4.6-12")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "randomForest" version))
       (sha256
        (base32
         "1i43idaihhl6nwqw42v9dqpl6f8z3ykcn2in32lh2755i27jylbf"))))
    (properties `((upstream-name . "randomForest")))
    (build-system r-build-system)
    (home-page "https://www.stat.berkeley.edu/~breiman/RandomForests/")
    (native-inputs
     `(("gfortran" ,gfortran)))
    (synopsis "Breiman and Cutler's random forests for classification and regression")
    (description
"This package provides the Breiman and Cutler's random forests algorithm, based on a
forest of trees using random inputs, for classification and regression.")
    (license license:gpl2+)))

(define-public r-diptest
  (package
    (name "r-diptest")
    (version "0.75-7")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "diptest" version))
       (sha256
        (base32
         "06xnc5gv1284ll0addxnxb6ljz6fn8dbyrp5vchyz6551h800aa6"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/diptest")
    (synopsis "Hartigan's dip test statistic for unimodality")
    (description
     "This package computes Hartigan's dip test statistic for unimodality,
multimodality and provides a test with simulation based p-values, where the
original public code has been corrected.")
    (license license:gpl2+)))

(define-public r-modeltools
  (package
    (name "r-modeltools")
    (version "0.2-21")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "modeltools" version))
       (sha256
        (base32
         "0ynds453xprxv0jqqzi3blnv5w6vrdww9pvd1sq4lrr5ar3k3cq7"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/modeltools")
    (synopsis "Tools and classes for statistical models")
    (description
     "This package provides a collection of tools to deal with statistical
models.  The functionality is experimental and the user interface is likely
to change in the future.")
    (license license:gpl2)))

(define-public r-flexmix
  (package
    (name "r-flexmix")
    (version "2.3-14")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "flexmix" version))
       (sha256
        (base32
         "0sl4zxh1sb2sr5q7svjw9ihrm219jzn82yrc9d43q6r1b8bpyz43"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-lattice" ,r-lattice)
       ("r-modeltools" ,r-modeltools)
       ("r-nnet" ,r-nnet)))
    (home-page "https://cran.r-project.org/web/packages/flexmix")
    (synopsis "Flexible mixture modeling")
    (description
     "This package implements a general framework for finite mixtures of
regression models using the EM algorithm.  FlexMix provides the E-step and
all data handling, while the M-step can be supplied by the user to easily
define new models.  Existing drivers implement mixtures of standard linear
models, generalized linear models and model-based clustering.")
    (license license:gpl2+)))

(define-public r-mclust
  (package
    (name "r-mclust")
    (version "5.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "mclust" version))
       (sha256
        (base32
         "1i3v1q8qq0al3ifvjhcvxfsg68cx9mq8jz67jwmiqai78mw0aqvk"))))
    (build-system r-build-system)
    (native-inputs
     `(("gfortran" ,gfortran)))
    (home-page "http://www.stat.washington.edu/mclust/")
    (synopsis "Gaussian mixture modelling for model-based clustering etc.")
    (description
     "This package provides Gaussian finite mixture models fitted via EM
algorithm for model-based clustering, classification, and density estimation,
including Bayesian regularization, dimension reduction for visualisation,
and resampling-based inference.")
    (license license:gpl2+)))

(define-public r-prabclus
  (package
    (name "r-prabclus")
    (version "2.2-6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "prabclus" version))
       (sha256
        (base32
         "0qjsxrx6yv338bxm4ki0w9h8hind1l98abdrz828588bwj02jya1"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-mass" ,r-mass)
       ("r-mclust" ,r-mclust)))
    (home-page "https://cran.r-project.org/web/packages/prabclus")
    (synopsis "Parametric bootstrap tests for spatial neighborhood clustering")
    (description
     "This package provides distance-based parametric bootstrap tests for
clustering with spatial neighborhood information.  It implements some distance
measures, clustering of presence-absence, abundance and multilocus genetical
data for species delimitation, nearest neighbor based noise detection.")
    (license license:gpl2+)))

(define-public r-deoptimr
  (package
    (name "r-deoptimr")
    (version "1.0-8")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "DEoptimR" version))
       (sha256
        (base32
         "1vz546hyjyhly70z62h5n3mn62b8llhhmim8ffp9y6jnnb0i2sc4"))))
    (properties `((upstream-name . "DEoptimR")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/DEoptimR")
    (synopsis "Differential evolution optimization in pure R")
    (description
     "This package provides a differential evolution (DE) stochastic
algorithms for global optimization of problems with and without constraints.
The aim is to curate a collection of its state-of-the-art variants that (1) do
not sacrifice simplicity of design, (2) are essentially tuning-free, and (3)
can be efficiently implemented directly in the R language.")
    (license license:gpl2+)))

(define-public r-robustbase
  (package
    (name "r-robustbase")
    (version "0.92-8")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "robustbase" version))
       (sha256
        (base32
         "080ynchq8pvhnmbivj4fp74n0x9lyr9923sg0s9gq5irgm68kgxw"))))
    (build-system r-build-system)
    (inputs
     `(("gfortran" ,gfortran)))
    (propagated-inputs
     `(("r-deoptimr" ,r-deoptimr)))
    (home-page "http://robustbase.r-forge.r-project.org/")
    (synopsis "Basic robust statistics")
    (description
     "This packages allows to analyze data with robust methods such as
regression methodology including model selections and multivariate statistics.")
    (license license:gpl2+)))

(define-public r-pcapp
  (package
    (name "r-pcapp")
    (version "1.9-72")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "pcaPP" version))
       (sha256
        (base32
         "0i2822bv86dpqmk3q17x61nsp3gjjnn4nr8191rwvbiib7xhpgaq"))))
    (properties `((upstream-name . "pcaPP")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-mvtnorm" ,r-mvtnorm)))
    (home-page "https://cran.r-project.org/web/packages/pcaPP")
    (synopsis "Robust PCA by projection pursuit")
    (description
     "This package provides functions for robust @dfn{principal component
analysis} (PCA) by projection pursuit.")
    (license license:gpl3+)))

(define-public r-rrcov
  (package
    (name "r-rrcov")
    (version "1.4-3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rrcov" version))
       (sha256
        (base32
         "0kagqggi634mvrhd67ia7mpjrj5v6w2wq0z3kyppg5xkvzh335nq"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-cluster" ,r-cluster)
       ("r-lattice" ,r-lattice)
       ("r-mvtnorm" ,r-mvtnorm)
       ("r-pcapp" ,r-pcapp)
       ("r-robustbase" ,r-robustbase)))
    (native-inputs
     `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/rrcov")
    (synopsis "Scalable robust estimators with high breakdown Point")
    (description
     "This package provides an implementation of robust location and scatter
estimation and robust multivariate analysis with high breakdown point.")
    (license license:gpl2+)))

(define-public r-fit-models
  (package
    (name "r-fit-models")
    (version "0.5-14")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "fit.models" version))
       (sha256
        (base32
         "0vjbzmx0ambm6yzidb4vbgmhclwzwv2iz2cwl54ccdkvx4cx3fck"))))
    (properties `((upstream-name . "fit.models")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-lattice" ,r-lattice)))
    (home-page "https://cran.r-project.org/web/packages/fit.models")
    (synopsis "Compare fitted models")
    (description
     "The @code{fit.models} function and its associated methods (coefficients, print,
summary, plot, etc.) were originally provided in the @code{robust} package to
compare robustly and classically fitted model objects.  The aim of the
@code{fit.models} package is to separate this fitted model object comparison
functionality from the robust package and to extend it to support fitting
methods (e.g., classical, robust, Bayesian, regularized, etc.) more
generally.")
    ;; Any version of the GPL
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-robust
  (package
    (name "r-robust")
    (version "0.4-18")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "robust" version))
       (sha256
        (base32
         "1b7qh1aff500nd6dh4y2ipmjgdiq8991shflb63pc39vpc0ny6g4"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-fit-models" ,r-fit-models)
       ("r-lattice" ,r-lattice)
       ("r-mass" ,r-mass)
       ("r-robustbase" ,r-robustbase)
       ("r-rrcov" ,r-rrcov)))
    (native-inputs
     `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/robust")
    (synopsis "Port of the S+ \"Robust Library\"")
    (description
     "This package is a port of the S+ \"Robust Library\".  It provides
methods for robust statistics, notably for robust regression and robust
multivariate analysis.")
    (license license:gpl2)))

(define-public r-trimcluster
  (package
    (name "r-trimcluster")
    (version "0.1-2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "trimcluster" version))
       (sha256
        (base32
         "0lsgbg93hm0w1rdb813ry0ks2l0jfpyqzqkf3h3bj6fch0avcbv2"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/trimcluster")
    (synopsis "Cluster analysis with trimming")
    (description
     "The trimmed k-means clustering method by Cuesta-Albertos, Gordaliza and
Matran (1997).  This optimizes the k-means criterion under trimming a portion
of the points.")
    ;; Any GPL version
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-fpc
  (package
    (name "r-fpc")
    (version "2.1-10")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "fpc" version))
       (sha256
        (base32
         "15m0p9l9w2v7sl0cnzyg81i2fmx3hrhvr3371544mwn3fpsca5sx"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-class" ,r-class)
       ("r-cluster" ,r-cluster)
       ("r-diptest" ,r-diptest)
       ("r-flexmix" ,r-flexmix)
       ("r-kernlab" ,r-kernlab)
       ("r-mass" ,r-mass)
       ("r-mclust" ,r-mclust)
       ("r-mvtnorm" ,r-mvtnorm)
       ("r-prabclus" ,r-prabclus)
       ("r-robustbase" ,r-robustbase)
       ("r-trimcluster" ,r-trimcluster)))
    (home-page "https://cran.r-project.org/web/packages/fpc")
    (synopsis "Flexible procedures for clustering")
    (description
     "This package provides various methods for clustering and cluster validation.
For example, it provides fixed point clustering, linear regression clustering,
clustering by merging Gaussian mixture components, as well as symmetric and
asymmetric discriminant projections for visualisation of the separation of
groupings.")
  (license license:gpl2+)))

(define-public r-vgam
  (package
    (name "r-vgam")
    (version "1.0-4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "VGAM" version))
       (sha256
        (base32
         "0r59hyp3afmvms890b3v2yyckf8v0qkjf2w11rnqd2zgg1gri0g5"))))
    (properties `((upstream-name . "VGAM")))
    (build-system r-build-system)
    (inputs
     `(("gfortran" ,gfortran)))
    (home-page "https://www.stat.auckland.ac.nz/~yee/VGAM")
    (synopsis "Vector generalized linear and additive models")
    (description
    "This package is an implementation of about 6 major classes of statistical
regression models.  Currently only fixed-effects models are implemented, i.e.,
no random-effects models.  Many (150+) models and distributions are estimated
by maximum likelihood estimation (MLE) or penalized MLE, using Fisher scoring.
VGLMs can be loosely thought of as multivariate generalised linear models.")
    (license license:gpl2+)))

(define-public r-pbapply
  (package
    (name "r-pbapply")
    (version "1.3-3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "pbapply" version))
       (sha256
        (base32
         "0iqhymf65jffh00qf056h1p76xf92bfmij6aymlmgnvn24fv4ybk"))))
    (build-system r-build-system)
    (home-page "https://github.com/psolymos/pbapply")
    (synopsis "Adding progress bar to apply functions")
    (description
     "This lightweight package that adds progress bar to vectorized R
functions apply.  The implementation can easily be added to functions where
showing the progress is useful e.g. bootstrap.")
    (license license:gpl2)))

(define-public r-fnn
  (package
    (name "r-fnn")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "FNN" version))
       (sha256
        (base32
         "1kncmiaraq1mrykb9fj3fsxswabk3l71fnp1vks0x9aay5xfk8mj"))))
    (properties `((upstream-name . "FNN")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/FNN")
    (synopsis "Fast nearest neighbor search algorithms and applications")
    (description
     "This package provides cover-tree and kd-tree fast k-nearest neighbor
search algorithms and related applications including KNN classification,
regression and information measures.")
    (license license:gpl2+)))

(define-public r-minqa
  (package
    (name "r-minqa")
    (version "1.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "minqa" version))
       (sha256
        (base32
         "036drja6xz7awja9iwb76x91415p26fb0jmg7y7v0p65m6j978fg"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rcpp" ,r-rcpp)))
    (inputs
     `(("gfortran" ,gfortran)))
    (home-page "http://optimizer.r-forge.r-project.org")
    (synopsis "Derivative-free optimization algorithms by quadratic approximation")
    (description
      "This package provides a derivative-free optimization by quadratic approximation
based on an interface to Fortran implementations by M. J. D. Powell.")
    (license license:gpl2)))

(define-public r-rcppeigen
  (package
    (name "r-rcppeigen")
    (version "0.3.3.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "RcppEigen" version))
       (sha256
        (base32
         "093g2wmqyk86693l0lzh2va8119w3k7rvw5q24p842jdfv5x5z8l"))))
    (properties `((upstream-name . "RcppEigen")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rcpp" ,r-rcpp)
       ("r-matrix" ,r-matrix)))
    (home-page "http://eigen.tuxfamily.org")
    (synopsis "Rcpp integration for the Eigen templated linear algebra library")
    (description
      "This package provides an integration of Eigen in R using a C++ template
library for linear algebra: matrices, vectors, numerical solvers and related algorithms.
It supports dense and sparse matrices on integer, floating point and complex numbers,
decompositions of such matrices, and solutions of linear systems.")
    (license license:gpl2+)))

(define-public r-modelmetrics
  (package
    (name "r-modelmetrics")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
        (uri (cran-uri "ModelMetrics" version))
        (sha256
         (base32
          "119xxmzb5biq7k1yxqsf0jmmarmfn6lds9x9hfgv593xlpym6za8"))))
    (properties `((upstream-name . "ModelMetrics")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rcpp" ,r-rcpp)))
    (home-page "https://cran.r-project.org/web/packages/ModelMetrics")
    (synopsis "Rapid calculation of model metrics")
    (description
     "Written in C++ using @code{Rcpp}, this package provides a collection of
metrics for evaluating models.")
    (license license:gpl2+)))

(define-public r-matrixmodels
  (package
    (name "r-matrixmodels")
    (version "0.4-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "MatrixModels" version))
       (sha256
        (base32
         "0cyfvhci2p1vr2x52ymkyqqs63x1qchn856dh2j94yb93r08x1zy"))))
    (properties `((upstream-name . "MatrixModels")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-matrix" ,r-matrix)))
    (home-page "https://cran.r-project.org/web/packages/MatrixModels")
    (synopsis "Modelling with sparse and dense matrices")
    (description
     "This package models with sparse and dense matrix matrices,
using modular prediction and response module classes.")
    (license license:gpl2+)))

(define-public r-quantreg
  (package
    (name "r-quantreg")
    (version "5.34")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "quantreg" version))
       (sha256
        (base32
         "0bmgfgnmzqwmwwaraliry682yb8kcr396q3rlzrd2qzaa9sgxxh4"))))
    (build-system r-build-system)
    (native-inputs
     `(("gfortran" ,gfortran)))
    (propagated-inputs
     `(("r-matrix" ,r-matrix)
       ("r-matrixmodels" ,r-matrixmodels)
       ("r-sparsem" ,r-sparsem)))
    (home-page "http://www.r-project.org")
    (synopsis "Quantile regression")
    (description
     "This package provides an estimation and inference methods for models
of conditional quantiles: linear and nonlinear parametric and non-parametric
models for conditional quantiles of a univariate response and several methods
for handling censored survival data.  Portfolio selection methods based on
expected shortfall risk are also included.")
    (license license:gpl2+)))

(define-public r-nloptr
  (package
    (name "r-nloptr")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "nloptr" version))
       (sha256
        (base32
         "1sz1xj3785x4vsm4nd6in298bk32hs2jk5nsxma7ivxi7jcmn8l4"))))
    (build-system r-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("nlopt" ,nlopt)))
    (home-page "https://cran.r-project.org/web/packages/nloptr")
    (synopsis "R interface to NLopt")
    (description
     "This package is interface to NLopt, a library for nonlinear
optimization.  NLopt is a library for nonlinear optimization, providing a
common interface for a number of different free optimization routines
available online as well as original implementations of various other
algorithms.")
    (license license:lgpl3)))

(define-public r-lme4
  (package
    (name "r-lme4")
    (version "1.1-14")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "lme4" version))
       (sha256
        (base32
         "1vars93604q7kyz6y1f8f886l05jkgj8bs78033l2czypvzki5la"))))
    (build-system r-build-system)
    (native-inputs
     `(("r-rcpp" ,r-rcpp)
       ("r-rcppeigen" ,r-rcppeigen)))
    (propagated-inputs
     `(("r-lattice" ,r-lattice)
       ("r-matrix" ,r-matrix)
       ("r-minqa" ,r-minqa)
       ("r-nloptr" ,r-nloptr)
       ("r-mass" ,r-mass)
       ("r-nlme" ,r-nlme)))
    (home-page "https://cran.r-project.org/web/packages/lme4")
    (synopsis "Linear mixed-effects models using eigen and S4")
    (description
      "This package provides fit linear and generalized linear mixed-effects
models.  The models and their components are represented using S4 classes and
methods.  The core computational algorithms are implemented using the Eigen
C++ library for numerical linear algebra and RcppEigen glue.")
    (license license:gpl2+)))

(define-public r-pbkrtest
  (package
    (name "r-pbkrtest")
    (version "0.4-7")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "pbkrtest" version))
       (sha256
        (base32
         "1si3bhi59xc51a0pgjjglccq3h4aljyhw2k1b8574s145fnh7fsw"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-lme4" ,r-lme4)
       ("r-mass" ,r-mass)
       ("r-matrix" ,r-matrix)))
    (home-page "http://people.math.aau.dk/~sorenh/software/pbkrtest/")
    (synopsis "Methods for linear mixed model comparison")
    (description
     "This package implements a parametric bootstrap test and a Kenward Roger
modification of F-tests for linear mixed effects models and a parametric
bootstrap test for generalized linear mixed models.")
    (license license:gpl2+)))

(define-public r-car
  (package
    (name "r-car")
    (version "2.1-6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "car" version))
       (sha256
        (base32
         "14l4d9j87xnb0bz2wgqqbxnlnbcdfb5vx2fw6kdxj91kyrk6lhnj"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-mass" ,r-mass)
       ("r-mgcv" ,r-mgcv)
       ("r-nnet" ,r-nnet)
       ("r-pbkrtest" ,r-pbkrtest)
       ("r-quantreg" ,r-quantreg)))
    (home-page "https://r-forge.r-project.org/projects/car/")
    (synopsis "Companion to applied regression")
    (description
      "This package provides functions and datasets from book Companion
to Applied regression, Second Edition, Sage, 2011.")
    (license license:gpl2+)))

(define-public r-caret
  (package
    (name "r-caret")
    (version "6.0-78")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "caret" version))
       (sha256
        (base32
         "0h1nxzii2h80aslp1zsjczrlfmaks44sskabk4yq9c5rafc7ka6y"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-foreach" ,r-foreach)
       ("r-ggplot2" ,r-ggplot2)
       ("r-lattice" ,r-lattice)
       ("r-modelmetrics" ,r-modelmetrics)
       ("r-nlme" ,r-nlme)
       ("r-plyr" ,r-plyr)
       ("r-recipes" ,r-recipes)
       ("r-reshape2" ,r-reshape2)
       ("r-withr" ,r-withr)))
    (home-page "https://github.com/topepo/caret")
    (synopsis "Classification and regression training")
    (description
     "This package provides misc functions for training and plotting
classification and regression models.")
    (license license:gpl2+)))

(define-public r-rcppprogress
  (package
    (name "r-rcppprogress")
    (version "0.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "RcppProgress" version))
       (sha256
        (base32
         "09ayw6d69i0lplmaj5md25p5hn1nmiyp6a4sh60v93nrrs1cq7iv"))))
    (properties `((upstream-name . "RcppProgress")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-devtools" ,r-devtools)
       ("r-rcpp" ,r-rcpp)))
    (home-page "https://github.com/kforner/rcpp_progress")
    (synopsis "Interruptible progress bar for C++ in R packages")
    (description
     "This package allows to display a progress bar in the R console for long running
computations taking place in C++ code, and support for interrupting those computations
even in multithreaded code, typically using OpenMP.")
    (license license:gpl3+)))

(define-public r-mnormt
  (package
    (name "r-mnormt")
    (version "1.5-5")
    (source
     (origin
       (method url-fetch)
        (uri (cran-uri "mnormt" version))
        (sha256
          (base32
           "1b34xxrnf35khsx82mhvmk96sgfr2flyasaah7qkb2976pwxay7z"))))
    (build-system r-build-system)
    (native-inputs
     `(("gfortran" ,gfortran)))
    (home-page "http://azzalini.stat.unipd.it/SW/Pkg-mnormt")
    (synopsis "Multivariate normal and \"t\" distributions")
    (description
     "This package provides functions for computing the density and the
distribution function of multivariate normal and \"t\" random variables, and
for generating random vectors sampled from these distributions.  Probabilities
are computed via non-Monte Carlo methods.")
    (license license:gpl2+)))

(define-public r-numderiv
  (package
    (name "r-numderiv")
    (version "2016.8-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "numDeriv" version))
       (sha256
        (base32
         "07ni52rwiap4wilfz94w5mrqaxr59axxmgn57857ip4p6qkiss0v"))))
    (properties `((upstream-name . "numDeriv")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/numDeriv")
    (synopsis "Accurate numerical derivatives")
    (description
     "This package provides methods for calculating accurate numerical
first and second order derivatives.")
    (license license:gpl2)))

(define-public r-sn
  (package
    (name "r-sn")
    (version "1.5-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "sn" version))
       (sha256
        (base32
         "0l7qlah6dwkp8jswnr2mn3phlm15naba5zkmv0g5pcqy5ix0gz7w"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-mnormt" ,r-mnormt)
       ("r-numderiv" ,r-numderiv)))
    (home-page "http://azzalini.stat.unipd.it/SN")
    (synopsis "The skew-normal and skew-t distributions")
    (description
     "This packages provides functionalities to build and manipulate
probability distributions of the skew-normal family and some related
ones, notably the skew-t family, and provides related statistical
methods for data fitting and diagnostics, in the univariate and the
multivariate case.")
    (license license:gpl2+)))

(define-public r-tclust
  (package
    (name "r-tclust")
    (version "1.3-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "tclust" version))
       (sha256
        (base32
         "1li62wynv81kb17fx7nk63a26qlb78l8fdf63in8yzcl7fkpji7y"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-cluster" ,r-cluster)
       ("r-mclust" ,r-mclust)
       ("r-mvtnorm" ,r-mvtnorm)
       ("r-sn" ,r-sn)))
    (home-page "https://cran.r-project.org/web/packages/tclust")
    (synopsis "Robust trimmed clustering")
    (description
     "This package implements different robust clustering
algorithms (@code{tclust}) based on trimming and including some graphical
diagnostic tools (@code{ctlcurves} and @code{DiscrFact}).")
    (license license:gpl3)))

(define-public r-ranger
  (package
    (name "r-ranger")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ranger" version))
       (sha256
        (base32
         "1fdbm879wx3hlng8s4c4f8f2x5yxz57llakcqnpa0lymybidw3vz"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rcpp" ,r-rcpp)
       ("r-matrix" ,r-matrix)
       ("r-rcppeigen" ,r-rcppeigen)))
    (home-page "https://github.com/imbs-hl/ranger")
    (synopsis "Fast implementation of random forests")
    (description
     "This package provides a fast implementation of Random Forests,
particularly suited for high dimensional data.  Ensembles of classification,
regression, survival and probability prediction trees are supported.  Data from
genome-wide association studies can be analyzed efficiently.")
    (license license:gpl3)))

(define-public r-tsne
  (package
    (name "r-tsne")
    (version "0.1-3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "tsne" version))
       (sha256
        (base32
         "0s8cv2pndkddq62rzlgzgfdjp1vjv5hz5i5957sllnb97vbzbzb6"))))
    (build-system r-build-system)
    (home-page "https://github.com/jdonaldson/rtsne/")
    (synopsis "t-Distributed Stochastic Neighbor Embedding for R")
    (description
     "This package provides a pure R implementation of the t-SNE algorithm.")
    (license license:gpl2+)))

(define-public r-cairo
  (package
    (name "r-cairo")
    (version "1.5-9")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Cairo" version))
       (sha256
        (base32
         "1x1q99r3r978rlkkm5gixkv03p0mcr6k7ydcqdmisrwnmrn7p1ia"))))
    (properties `((upstream-name . "Cairo")))
    (build-system r-build-system)
    (inputs
     `(("cairo" ,cairo)
       ("libxt" ,libxt)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://www.rforge.net/Cairo/")
    (synopsis "R graphics device using Cairo graphics library")
    (description
     "This package provides a Cairo graphics device that can be use to
create high-quality vector (PDF, PostScript and SVG) and bitmap
output (PNG,JPEG,TIFF), and high-quality rendering in displays (X11
and Win32).  Since it uses the same back-end for all output, copying
across formats is WYSIWYG.  Files are created without the dependence
on X11 or other external programs.  This device supports alpha
channel (semi-transparent drawing) and resulting images can contain
transparent and semi-transparent regions.  It is ideal for use in
server environments (file output) and as a replacement for other
devices that don't have Cairo's capabilities such as alpha support or
anti-aliasing.  Backends are modular such that any subset of backends
is supported.")
    (license license:gpl2)))

(define-public r-lubridate
  (package
    (name "r-lubridate")
    (version "1.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "lubridate" version))
       (sha256
        (base32
         "0pzvazzybzvln3y676yims7whsc74g8i7kgf2nsfbxdq5943z349"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rcpp" ,r-rcpp)
       ("r-stringr" ,r-stringr)))
    (home-page
     "https://cran.r-project.org/web/packages/lubridate")
    (synopsis "Make dealing with dates a little easier")
    (description
     "This package provides functions to work with date-times and time-spans:
fast and user friendly parsing of date-time data, extraction and updating of
components of a date-time (years, months, days, hours, minutes, and seconds),
algebraic manipulation on date-time and time-span objects.  The 'lubridate'
package has a consistent and memorable syntax that makes working with dates
easy and fun.")
    (license license:gpl2)))

(define-public r-fdrtool
  (package
    (name "r-fdrtool")
    (version "1.2.15")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "fdrtool" version))
       (sha256
        (base32
         "1h46frlk7d9f4qx0bg6p55nrm9wwwz2sv6d1nz7061wdfsm69yb5"))))
    (build-system r-build-system)
    (home-page "http://strimmerlab.org/software/fdrtool/")
    (synopsis "Estimation of false discovery rates and higher criticism")
    (description
     "This package provides tools to estimate tail area-based false discovery
rates as well as local false discovery rates for a variety of null
models (p-values, z-scores, correlation coefficients, t-scores).  The
proportion of null values and the parameters of the null distribution are
adaptively estimated from the data.  In addition, the package contains
functions for non-parametric density estimation (Grenander estimator), for
monotone regression (isotonic regression and antitonic regression with
weights), for computing the @dfn{greatest convex minorant} (GCM) and the
@dfn{least concave majorant} (LCM), for the half-normal and correlation
distributions, and for computing empirical @dfn{higher criticism} (HC) scores
and the corresponding decision threshold.")
    (license license:gpl3+)))

(define-public r-forcats
  (package
    (name "r-forcats")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "forcats" version))
       (sha256
        (base32
         "1mvwkynvvgz2vi8dyz11x7xrp53kadjawjcja34hwk1d89qf7g5m"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-magrittr" ,r-magrittr)
       ("r-tibble" ,r-tibble)))
    (home-page "http://forcats.tidyverse.org")
    (synopsis "Tools for working with factors")
    (description "This package provides helpers for reordering factor
levels (including moving specified levels to front, ordering by first
appearance, reversing, and randomly shuffling), and tools for modifying factor
levels (including collapsing rare levels into other, \"anonymizing\", and
manually \"recoding\").")
    (license license:gpl3)))

(define-public r-tgstat
  (let ((changeset "4f8e60c03598f49aff6f5beeab40f2b995377e9f")
        (revision "1"))
    (package
      (name "r-tgstat")
      (version (string-append "1.0.2-" revision "." (string-take changeset 7)))
      (source
       (origin
         (method hg-fetch)
         (uri (hg-reference
               (url "https://bitbucket.org/tanaylab/tgstat")
               (changeset changeset)))
         (sha256
          (base32
           "0ilkkyximy77zbncm91kdfqbxf0qyndg16pd3q3p6a3xc9qcmxvn"))))
      (build-system r-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-isnan
             (lambda _
               (substitute* "src/tgstat.h"
                 (("#define isnan ::isnan")
                  "#define isnan std::isnan"))
               #t)))))
      (propagated-inputs
       `(("r-rcpp" ,r-rcpp)))
      (home-page "https://bitbucket.org/tanaylab/tgstat/")
      (synopsis "Tanay's group statistical utilities")
      (description
       "The goal of tgstat is to provide fast and efficient statistical
tools.")
      (license license:gpl2))))

(define-public r-tgconfig
  (let ((changeset "1e02c7614713bd0866c46f0c679a058f8c6d627e")
        (revision "1"))
    (package
      (name "r-tgconfig")
      (version (string-append "0.0.0.9000-" revision "." (string-take changeset 7)))
      (source
       (origin
         (method hg-fetch)
         (uri (hg-reference
               (url "https://bitbucket.org/tanaylab/tgconfig")
               (changeset changeset)))
         (sha256
          (base32
           "0xy6c7s7mn1yx191154bwbv1bl424bnvc80syqpl1vdl28ba46rj"))))
      (build-system r-build-system)
      (propagated-inputs
       `(("r-yaml" ,r-yaml)))
      (home-page "https://bitbucket.org/tanaylab/tgconfig/")
      (synopsis "Infrastructure for managing package parameters")
      (description
       "The goal of tgconfig is to provide infrastructure for managing package
parameters.")
      (license license:gpl3))))

(define-public r-directlabels
  (package
    (name "r-directlabels")
    (version "2017.03.31")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "directlabels" version))
       (sha256
        (base32
         "01ahw0c56mnzwl611s8hnghicww4c4dgf5v2wv6bl1s1pramw9pr"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-quadprog" ,r-quadprog)))
    (home-page "http://directlabels.r-forge.r-project.org/")
    (synopsis "Direct labels for multicolor plots")
    (description
     "This package provides an extensible framework for automatically placing
direct labels onto multicolor plots.  Label positions are described using
positioning methods that can be re-used across several different plots.  There
are heuristics for examining @code{trellis} and @code{ggplot} objects and
inferring an appropriate positioning method.")
    (license license:gpl3)))
