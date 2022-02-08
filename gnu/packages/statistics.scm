;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Vicente Vera Parra <vicentemvp@gmail.com>
;;; Copyright © 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016, 2017, 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2016 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2016, 2017 Raoul Bonnal <ilpuccio.febo@gmail.com>
;;; Copyright © 2017 Kyle Meyer <kyle@kyleam.com>
;;; Copyright © 2017, 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Alex Kost <alezost@gmail.com>
;;; Copyright © 2018 Alex Branham <alex.branham@gmail.com>
;;; Copyright © 2020 Tim Howes <timhowes@lavabit.com>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Bonface Munyoki Kilyungi <me@bonfacemunyoki.com>
;;; Copyright © 2021 Lars-Dominik Braun <lars@6xq.net>
;;; Copyright © 2021 Frank Pursel <frank.pursel@gmail.com>
;;; Copyright © 2022 Simon Tournier <zimon.toutoune@gmail.com>
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
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system r)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages java)
  #:use-module (gnu packages javascript)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages base)
  #:use-module (gnu packages uglifyjs)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))


(define-public pspp
  (package
    (name "pspp")
    (version "1.4.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/pspp/pspp-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0lqrash677b09zxdlxp89z6k02y4i23mbqg83956dwl69wc53dan"))))
    (build-system gnu-build-system)
    (inputs
     (list cairo
           gettext-minimal
           gsl
           libxml2
           pango
           readline
           gtk+
           gtksourceview-3
           spread-sheet-widget
           zlib))
    (native-inputs
     (list autoconf ;for tests
           `(,glib "bin") ;for glib-genmarshal
           perl
           pkg-config
           python-2 ;for tests
           texinfo))
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

(define-public jags
  (package
    (name "jags")
    (version "4.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/mcmc-jags/JAGS/"
                                  (version-major version) ".x/Source/"
                                  "JAGS-" version ".tar.gz"))
              (sha256
               (base32
                "1z3icccg2ic56vmhyrpinlsvpq7kcaflk1731rgpvz9bk1bxvica"))))
    (build-system gnu-build-system)
    (home-page "http://mcmc-jags.sourceforge.net/")
    (native-inputs
     (list gfortran lapack))
    (synopsis "Gibbs sampler")
    (description "JAGS is Just Another Gibbs Sampler.  It is a program for
analysis of Bayesian hierarchical models using Markov Chain Monte Carlo (MCMC)
simulation not wholly unlike BUGS.  JAGS was written with three aims in mind:

@enumerate
@item To have a cross-platform engine for the BUGS language;
@item To be extensible, allowing users to write their own functions,
  distributions and samplers;
@item To be a platform for experimentation with ideas in Bayesian modelling.
@end enumerate\n")
    (license license:gpl2)))

(define-public libxls
  (package
    (name "libxls")
    (version "1.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/libxls/libxls/releases/download/"
                           "v" version "/libxls-" version ".tar.gz"))
       (sha256
        (base32 "0wg3ymr43aa1j3scyl9x83b2xgg7wilzpil0dj91a8dzji6w7b2x"))))
    (build-system gnu-build-system)
    (home-page "https://github.com/libxls/libxls")
    (synopsis "Read binary (.xls) Excel spreadsheet files")
    (description
     "libxls is a C library to read .xls spreadsheet files in the binary OLE
BIFF8 format as created by Excel 97 and later versions.  It cannot write them.

This package also provides @command{xls2csv} to export Excel files to CSV.")
    (license license:bsd-2)))

;; Update this package together with the set of recommended packages: r-boot,
;; r-class, r-cluster, r-codetools, r-foreign, r-kernsmooth, r-lattice,
;; r-mass, r-matrix, r-mgcv, r-nlme, r-nnet, r-rpart, r-spatial, r-survival.
(define r-with-tests
  (package
    (name "r-with-tests")
    (version "4.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cran/src/base/R-"
                                  (version-major version) "/R-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1brqmc7mmb0z94kcs4crgl6v9a6sxhm9fm3y177d81vjkxg24di0"))))
    (build-system gnu-build-system)
    (arguments
     `(#:disallowed-references (,tzdata-for-tests)
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
               (("compress = TRUE") "compress = FALSE"))))
         (add-before 'configure 'patch-coreutils-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((uname-bin (search-input-file inputs "/bin/uname"))
                   (rm-bin (search-input-file inputs "/bin/rm")))
               (substitute* "src/scripts/R.sh.in"
                 (("uname") uname-bin))
               (substitute* "src/unix/sys-std.c"
                 (("rm -Rf ") (string-append rm-bin " -Rf "))))))
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

             ;; Similarly, the "srcfilecopy" procedure records the current
             ;; time.  We change both of them to respect SOURCE_DATE_EPOCH.
             (substitute* "src/library/base/R/srcfile.R"
               (("timestamp <- (timestamp.*|file.mtime.*)" _ time)
                (string-append "timestamp <- \
as.POSIXct(if (\"\" != Sys.getenv(\"SOURCE_DATE_EPOCH\")) {\
  as.numeric(Sys.getenv(\"SOURCE_DATE_EPOCH\"))\
} else { " time "}, origin=\"1970-01-01\")\n")))

             ;; This library is installed using "install_package_description",
             ;; so we need to pass the "builtStamp" argument.
             (substitute* "src/library/tools/Makefile.in"
               (("(install_package_description\\(.*\"')\\)\"" line prefix)
                (string-append prefix ", builtStamp='1970-01-01')\"")))

             (substitute* "src/library/Recommended/Makefile.in"
               (("INSTALL_OPTS =" m)
                (string-append m " --built-timestamp=1970-01-01" m)))

             ;; R bundles an older version of help2man, which does not respect
             ;; SOURCE_DATE_EPOCH.  We cannot just use the latest help2man,
             ;; because that breaks a test.
             (with-fluids ((%default-port-encoding "ISO-8859-1"))
               (substitute* "tools/help2man.pl"
                 (("my \\$date = strftime \"%B %Y\", localtime" line)
                  (string-append line " 1"))))

             ;; The "References" section of this file when converted to
             ;; package.rds is sometimes stored with a newline, sometimes with
             ;; a space.  We avoid this problem by removing the line break
             ;; that is suspected to be the culprit.
             (substitute* "src/library/methods/DESCRIPTION.in"
               (("\\(2008\\)\n") "(2008) ")
               (("  ``Software") "``Software")
               (("Data Analysis:.") "Data Analysis:\n")
               (("Programming with R") "  Programming with R"))))
         (add-before 'build 'set-locales
           (lambda _
             (setlocale LC_ALL "C")
             (setenv "LC_ALL" "C")))
         (add-before 'configure 'set-default-pager
          ;; Set default pager to "cat", because otherwise it is "false",
          ;; making "help()" print nothing at all.
          (lambda _ (setenv "PAGER" "cat")))
         (add-before 'check 'set-timezone
           ;; Some tests require the timezone to be set.  However, the
           ;; timezone may not just be "UTC", or else a brittle regression
           ;; test in reg-tests-1d will fail.
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "TZ" "UTC+1")
             (setenv "TZDIR"
                     (search-input-directory inputs
                                             "share/zoneinfo"))))
         (add-after 'build 'make-info
          (lambda _ (invoke "make" "info")))
         (add-after 'build 'install-info
          (lambda _ (invoke "make" "install-info"))))
       #:configure-flags
       `(;; We build the recommended packages here, because they are needed in
         ;; order to run the test suite.  We disable them in the r-minimal
         ;; package.
         "--with-cairo"
         "--with-blas=-lopenblas"
         "--with-libpng"
         "--with-jpeglib"
         "--with-libtiff"
         "--with-ICU"
         "--with-tcltk"
         ,(string-append "--with-tcl-config="
                         (assoc-ref %build-inputs "tcl")
                         "/lib/tclConfig.sh")
         ,(string-append "--with-tk-config="
                         (assoc-ref %build-inputs "tk")
                         "/lib/tkConfig.sh")
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
       ("texlive" ,(texlive-updmap.cfg (list texlive-ae
                                        texlive-inconsolata
                                        texlive-fonts-ec
                                        texlive-grfext
                                        texlive-amsfonts
                                        texlive-latex-base
                                        texlive-latex-fancyvrb
                                        texlive-latex-graphics
                                        texlive-hyperref
                                        texlive-oberdiek
                                        texlive-latex-tools
                                        texlive-latex-upquote
                                        texlive-url
                                        texlive-latex-xkeyval)))
       ("tzdata" ,tzdata-for-tests)
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
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libtiff" ,libtiff)
       ("libxt" ,libxt)
       ("pcre2" ,pcre2)
       ("readline" ,readline)
       ;; This avoids a reference to the ungraftable static bash.  R uses the
       ;; detected shell for the "system" procedure.
       ("bash" ,bash-minimal)
       ("tcl" ,tcl)
       ("tk" ,tk)
       ("which" ,which)
       ("zlib" ,zlib)))
    (native-search-paths
     (list (search-path-specification
            (variable "R_LIBS_SITE")
            (files (list "site-library/")))))
    (home-page "https://www.r-project.org/")
    (synopsis "Environment for statistical computing and graphics")
    (description
     "R is a language and environment for statistical computing and graphics.
It provides a variety of statistical techniques, such as linear and nonlinear
modeling, classical statistical tests, time-series analysis, classification
and clustering.  It also provides robust support for producing
publication-quality data plots.  A large amount of 3rd-party packages are
available, greatly increasing its breadth and scope.")
    (license license:gpl3+)))

(define-public r-minimal
  (package (inherit r-with-tests)
    (name "r-minimal")
    (arguments
     `(#:tests? #f
       ,@(substitute-keyword-arguments (package-arguments r-with-tests)
           ((#:disallowed-references refs '())
            (cons perl refs))
           ((#:configure-flags flags)
            ;; Do not build the recommended packages.  The build system creates
            ;; random temporary directories and embeds their names in some
            ;; package files.  We build these packages with the r-build-system
            ;; instead.
            `(cons "--without-recommended-packages" ,flags))
           ((#:phases phases '%standard-phases)
            `(modify-phases ,phases
               (add-after 'install 'remove-extraneous-references
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (let ((out (assoc-ref outputs "out")))
                     (substitute* (string-append out "/lib/R/etc/Makeconf")
                       (("^# configure.*")
                        "# Removed to avoid extraneous references\n"))
                     (substitute* (string-append out "/lib/R/bin/libtool")
                       (((string-append
                          "(-L)?("
                          (assoc-ref inputs "bzip2")
                          "|"
                          (assoc-ref inputs "perl")
                          "|"
                          (assoc-ref inputs "texlive")
                          "|"
                          (assoc-ref inputs "texlive-bin")
                          "|"
                          (assoc-ref inputs "texinfo")
                          "|"
                          (assoc-ref inputs "xz")
                          "|"
                          (format #false
                                  "/gnu/store/[^-]+-(~{~a~^|~})-[^/]+"
                                  '("glibc-utf8-locales"
                                    "libselinux"
                                    "libsepol"
                                    "file"
                                    "texlive-bin"
                                    "util-macros"
                                    "graphite2"))
                          "|"
                          "/gnu/store/[^-]+-glibc-[^-]+-static"
                          ")/lib")) ""))))))))))))

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
    (version "1.3-28")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "boot" version))
       (sha256
        (base32
         "0cjafhqv1c1mrjjcasqr767vs96wjcc6am9r1icryr8l4zymhwcz"))))
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
    (version "7.3-55")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "MASS" version))
       (sha256
        (base32
         "191i1iylvh2w2hi99fnm06731lma7z5snpjknfff1m9ziyy9qab5"))))
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
    (version "7.3-20")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "class" version))
       (sha256
        (base32
         "10slys2jny4k2j8l8k97hwrwd6ryzajfnzywqpq2yc9bqxmh8nz6"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-mass))
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
    (version "2.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "cluster" version))
       (sha256
        (base32
         "168l653ygkg21fwrqrf7lsqi5p6jzhgmcvhrwv7lznkdzdhag2jw"))))
    (build-system r-build-system)
    (native-inputs
     (list gfortran))
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
    (version "0.2-18")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "codetools" version))
       (sha256
        (base32
         "0a2c115glq8jxixwfigrpvjabhxchn9r4mc40y41dg9dg6wsd7hs"))))
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
    (version "0.8-82")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "foreign" version))
       (sha256
        (base32
         "1yw07yqzfm4w1h92ijyamrmf7rmkqxgamqfd74x7zv4vsn20dvgq"))))
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
    (version "2.23-20")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "KernSmooth" version))
       (sha256
        (base32
         "0fgclmzk3ksjsh3j47jqzm3jhqrwn12rkp7f84yr6wr43q2pbsr0"))))
    (properties `((upstream-name . "KernSmooth")))
    (build-system r-build-system)
    (native-inputs
     (list gfortran))
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
    (version "0.20-45")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "lattice" version))
              (sha256
               (base32
                "1b87fmyjzbg854vpi03my1p25n3d0bchhwsdm2frblxppn98sf12"))))
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
    (version "1.4-0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Matrix" version))
       (sha256
        (base32
         "1xh62bwcly1blpvnagbhjym1qgw87yir3c72ylhvcla05rq67d62"))))
    (properties `((upstream-name . "Matrix")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-lattice))
    (home-page "http://Matrix.R-forge.R-project.org/")
    (synopsis "Sparse and dense matrix classes and methods")
    (description
     "This package provides classes and methods for dense and sparse matrices
and operations on them using LAPACK and SuiteSparse.")
    (license license:gpl2+)))

(define-public r-nlme
  (package
    (name "r-nlme")
    (version "3.1-155")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "nlme" version))
       (sha256
        (base32 "0kzzpl63l298yy18x62ypgbhdw61fgm30lc4nlhjjhjj5220yfcz"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-lattice))
    (native-inputs
     (list gfortran))
    (home-page "https://cran.r-project.org/web/packages/nlme")
    (synopsis "Linear and nonlinear mixed effects models")
    (description
     "This package provides tools to fit and compare Gaussian linear and
nonlinear mixed-effects models.")
    (license license:gpl2+)))

(define-public r-mgcv
  (package
   (name "r-mgcv")
   (version "1.8-38")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "mgcv" version))
     (sha256
      (base32 "1fyx8b0xjyx75mp90v314gayzydkca5y90p76jnwxzfnhxbys4nd"))))
   (build-system r-build-system)
   (propagated-inputs
    (list r-matrix r-nlme))
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
    (version "7.3-17")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "nnet" version))
       (sha256
        (base32
         "0rws3rkd7dz1maz2mkld29jfrixjgacay8rqz7nmi82a2sw0nxgf"))))
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
    (version "4.1.16")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rpart" version))
       (sha256
        (base32
         "0p22657j6ps6z1dn0ja2fhq4k5xd1rv3cpwzk6nmjd2si8jpbv17"))))
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
    (version "7.3-15")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "spatial" version))
       (sha256
        (base32
         "1vwc7ingskdj1k4nq418x9yl3cw699pckb9xh5a1yp3g9plknqg5"))))
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
    (version "3.2-13")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "survival" version))
       (sha256
        (base32
         "0zrikancm9shixvgng89f1610sphlwljczi0fnjbdqn4l85rrarz"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-matrix))
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
    (arguments '(#:builder (begin (mkdir %output) #t)))
    (propagated-inputs
     (list r-minimal
           r-boot
           r-class
           r-cluster
           r-codetools
           r-foreign
           r-kernsmooth
           r-lattice
           r-mass
           r-matrix
           r-mgcv
           r-nlme
           r-nnet
           r-rpart
           r-spatial
           r-survival))))

(define-public r-bit
  (package
    (name "r-bit")
    (version "4.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "bit" version))
       (sha256
        (base32
         "0s7isadibxp2wr62r5cpbyh9z31sczzfz4j3rm7gxgjfpqgq8174"))))
    (build-system r-build-system)
    (native-inputs
     (list r-knitr))
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
    (version "4.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "bit64" version))
       (sha256
        (base32
         "0y0m7q1rwam1g88cjx7zyi07mj5dipxd9jkl90f294syx8k6ipr5"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bit))
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

(define-public r-chorddiag
  (package
    (name "r-chorddiag")
    (version "0.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mattflor/chorddiag")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1bpy9q861l1hyyiqbx2f7qzp7j7im8bkcfdwgxzk5fm0250p359a"))
       ;; Delete minified JavaScript file
       (snippet
        '(delete-file "inst/htmlwidgets/lib/d3/d3.min.js"))))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'process-javascript
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "inst/htmlwidgets/lib/d3"
               (let ((source (assoc-ref inputs "d3.v4.js"))
                     (target "d3.min.js"))
                 (format #true "Processing ~a --> ~a~%"
                         source target)
                 (invoke "esbuild" source "--minify"
                         (string-append "--outfile=" target)))))))))
    (propagated-inputs
     (list r-htmlwidgets r-rcolorbrewer))
    (native-inputs
     `(("esbuild" ,esbuild)
       ("r-knitr" ,r-knitr)
       ("d3.v4.js"
        ,(origin
           (method url-fetch)
           (uri "https://d3js.org/d3.v4.js")
           (sha256
            (base32
             "0y7byf6kcinfz9ac59jxc4v6kppdazmnyqfav0dm4h550fzfqqlg"))))))
    (home-page "https://github.com/mattflor/chorddiag")
    (synopsis "Create D3 chord diagram")
    (description
     "This package provides tools to create interactive chords diagrams via
the D3 Javascript library.  Chord diagrams show directed relationships among a
group of entities.  This package is based on
@url{http://bl.ocks.org/mbostock/4062006} with some modifications (fading) and
additions (tooltips, bipartite diagram type).")
    (license license:gpl3+)))

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
    (version "0.6.29")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "digest" version))
       (sha256
        (base32 "1ncwc5fwsylwlcp9vhkhszw8m5sifg74apig2m2pf168lha1yb3r"))))
    (build-system r-build-system)
    ;; Vignettes require r-knitr, which requires r-digest, so we have to
    ;; disable them and the tests.
    (arguments
     `(#:tests? #f
       #:configure-flags (list "--no-build-vignettes")))
    (home-page "http://dirk.eddelbuettel.com/code/digest.html")
    (synopsis "Create cryptographic hash digests of R objects")
    (description
     "This package contains an implementation of a function @code{digest()} for
the creation of hash digests of arbitrary R objects (using the md5, sha-1,
sha-256, crc32, xxhash and murmurhash algorithms) permitting easy comparison
of R language objects, as well as a function @code{hmac()} to create hash-based
message authentication code.

Please note that this package is not meant to be deployed for cryptographic
purposes for which more comprehensive (and widely tested) libraries such as
OpenSSL should be used.")
    (license license:gpl2+)))

(define-public r-estimability
  (package
    (name "r-estimability")
    (version "1.3")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "estimability" version))
              (sha256
               (base32
                "0cifdaa71spkcxl4db4z884jrya865sg3dhcv4isd8fnzg2pjcd3"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/estimability")
    (synopsis "Tools for assessing estimability of linear predictions")
    (description "Provides tools for determining estimability of linear
functions of regression coefficients, and @code{epredict} methods that handle
non-estimable cases correctly.")
    (license license:gpl2+)))

(define-public r-labeling
  (package
    (name "r-labeling")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "labeling" version))
       (sha256
        (base32 "0rfikd9gy70b8qz87q9axcwv8nmn9mbxfdwypxi0sghpfs9df8p0"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/labeling")
    (synopsis "Axis labeling algorithms")
    (description "The labeling package provides a range of axis labeling
algorithms.")
    (license license:expat)))

(define-public r-magrittr
  (package
    (name "r-magrittr")
    (version "2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "magrittr" version))
       (sha256
        (base32 "19k3q76qlri459iyyfgn9gl0w9lnv9v604yhpbc94nx71l6zvrkv"))))
    (build-system r-build-system)
    ;; knitr needs magrittr
    #;
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://cran.r-project.org/web/packages/magrittr/index.html")
    (synopsis "Forward-pipe operator for R")
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
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "munsell" version))
       (sha256
        (base32 "16g1fzisbpqb15yh3pqf3iia4csppva5dnv1z88x9dg263xskwyh"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-colorspace))
    (home-page "https://cran.r-project.org/web/packages/munsell")
    (synopsis "Munsell colour system")
    (description
     "The Munsell package contains Functions for exploring and using the
Munsell colour system.")
    (license license:expat)))

(define-public r-permute
  (package
   (name "r-permute")
   (version "0.9-7")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "permute" version))
     (sha256
      (base32
       "1h4dyhcsv8p3h3qxsy98pib9v79dddvrnq7qx6abkblsazxqzy7g"))))
   (build-system r-build-system)
   (native-inputs (list r-knitr))
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
    (version "1.8.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "plyr" version))
       (sha256
        (base32 "11sjjdn146w95s1vsfmmgdls082cbnm1slv98xvyjhsl2mpx4mga"))))
    (build-system r-build-system)
    (propagated-inputs (list r-rcpp))
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
     (list r-base64enc))
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
    (version "1.7.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "stringi" version))
       (sha256
        (base32
         "1v0rj5svgvn1mlf69yzm6jm00fxmrsxazs9xymzrf0apxjpxb8qf"))))
    (build-system r-build-system)
    (inputs (list icu4c))
    (native-inputs (list pkg-config))
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
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "stringr" version))
       (sha256
        (base32 "1p9ip7p87gbbg4s6d3d392svvzz2b5dqdq2c8ilgvn4s78nlsq47"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-glue r-magrittr r-stringi))
    (home-page "https://github.com/hadley/stringr")
    (synopsis "Simple, consistent wrappers for common string operations")
    (description
     "Stringr is a consistent, simple and easy to use set of wrappers around
the fantastic @code{stringi} package.  All function and argument names (and
positions) are consistent, all functions deal with \"NA\"'s and zero length
vectors in the same way, and the output from one function is easy to feed into
the input of another.")
    (license license:gpl2+)))

(define-public r-reshape2
  (package
    (name "r-reshape2")
    (version "1.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "reshape2" version))
       (sha256
        (base32 "1n0jrajpvc8hjkh9z4g8bwq63qy5vy5cgl2pzjardyih4ngcz3fq"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-plyr r-rcpp r-stringr))
    (home-page "https://github.com/hadley/reshape")
    (synopsis "Flexibly reshape data: a reboot of the \"reshape\" package")
    (description
     "Reshape2 is an R library to flexibly restructure and aggregate data
using just two functions: melt and dcast (or acast).")
    (license license:expat)))

(define-public r-ggplot2
  (package
    (name "r-ggplot2")
    (version "3.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ggplot2" version))
       (sha256
        (base32 "1si46bf9p09qzs208hqffwcb40305p361whmwhc1pwrsmx7jjxdh"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-digest
           r-glue
           r-gtable
           r-isoband
           r-mass
           r-mgcv
           r-tibble
           r-rlang
           r-scales
           r-svglite ; Needed for 'ggsave'
           r-withr))
    (native-inputs
     (list r-knitr))
    (home-page "https://ggplot2.tidyverse.org")
    (synopsis "Implementation of the grammar of graphics")
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
    (version "0.1.22")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "ggdendro" version))
              (sha256
               (base32
                "0skbj487b6f6pj2iz4yq2b9gbgb39cv5dyzhdl3w7ay1k0s5z9ph"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-ggplot2 r-mass))
    (native-inputs
     (list r-knitr))
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
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "gdtools" version))
       (sha256
        (base32
         "1p3ip0qwpg8f63jfx0b8vbac6l20ddid8xvxzkfi6i858pybnawp"))))
    (build-system r-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list cairo freetype zlib))
    (propagated-inputs
     (list r-rcpp r-systemfonts))
    (home-page "https://cran.r-project.org/web/packages/gdtools")
    (synopsis "Utilities for graphical rendering")
    (description
     "The @code{gdtools} package provides functionalities to get font metrics
and to generate base64 encoded string from raster matrix.")
    (license license:gpl3)))

(define-public r-svglite
  (package
    (name "r-svglite")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "svglite" version))
       (sha256
        (base32
         "19ggknw5322yw7r1bzhw0bqggcrpn0qxddk7kblpqnra2zz2brkn"))))
    (build-system r-build-system)
    (inputs
     (list libpng zlib))
    (propagated-inputs
     (list r-cpp11 r-systemfonts))
    (native-inputs
     (list r-knitr))
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
    (version "0.2.1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "assertthat" version))
              (sha256
               (base32
                "17wy5bdfzg73sg2clisg1k3zyn1adkj59x56m5nwia2k8z67zkw5"))))
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
    (version "0.2.2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "lazyeval" version))
              (sha256
               (base32
                "1m10i059csrcqkcn59a8wspn784alxsq3symzhn24mhhl894346n"))))
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
    (version "1.1.2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "DBI" version))
              (sha256
               (base32
                "0n2gf5flmcrkad7p0pzsnsgwjl5006yi7lyz9lisqxhw8xykgv2n"))))
    (build-system r-build-system)
    (native-inputs
     (list r-knitr))
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
    (version "1.78.0-0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "BH" version))
              (sha256
               (base32
                "1cwc1q8w9d6i81nclgi7lycsnpj0dcbxlv9rdb0f04r0d03rv7iv"))))
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
    (version "0.14")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "evaluate" version))
              (sha256
               (base32
                "0a2y7j534gbgixkwj9r1z76l2vssw4g1hznzbpclc076wkdqpj58"))))
    (build-system r-build-system)
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
    (version "1.11")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "formatR" version))
              (sha256
               (base32
                "0353aj975mj0yjc91z76bbfy86y7d5cvlqqyfr93cdng14nnd0dx"))))
    (build-system r-build-system)
    (native-inputs
     (list r-knitr))
    (home-page "https://yihui.org/formatr/")
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
    (version "0.9")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "highr" version))
              (sha256
               (base32
                "0kgdv2vf1lz3b5kbal9s83gg6812nw7fvrq0rkyr0v4k1lwi3zxy"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-xfun))
    ;; We cannot add knitr to the inputs, because highr depends on xfun, which
    ;; is an input to knitr.
    #;
    (native-inputs
     `(("r-knitr" ,r-knitr)))
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
    (version "0.12")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "mime" version))
              (sha256
               (base32
                "0j9qbq9jfsp61h2d0xmb87pa2wi8nhb1h2wih7l5drf1sr8i0059"))))
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
    (version "1.1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "markdown" version))
              (sha256
               (base32
                "06zwbrp14bri3470anadd7dvgmw06xf8df6v2pk64wx3f9sd934d"))))
    (build-system r-build-system)
    ;; Skip check phase because the tests require the r-knitr package to be
    ;; installed. This prevents installation failures. Knitr normally
    ;; shouldn't be available since r-markdown is a dependency of the r-knitr
    ;; package.
    (arguments `(#:tests? #f))
    (propagated-inputs
     (list r-mime r-xfun))
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
    (version "2.2.2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "yaml" version))
              (sha256
               (base32
                "0c7pk5fq79mkqpc398hg8zj01frv4araglyppli49sqr4yx6mp55"))))
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
    (version "1.37")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "knitr" version))
              (sha256
               (base32
                "0qyp78v3hz4qimlc3w4hj5dzp44n2w085ay0l1zvxsxs9142mk9r"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-evaluate r-highr r-stringr r-xfun r-yaml))
    (home-page "https://yihui.org/knitr/")
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
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "knitrBootstrap" version))
       (sha256
        (base32
         "1aj60j7f0gcs120fdrnfbnb7vk7lfn1phil0mghg6a5zldz4cqs3"))))
    (properties `((upstream-name . "knitrBootstrap")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-knitr r-rmarkdown r-markdown))
    (home-page "https://github.com/jimhester/knitrBootstrap")
    (synopsis "Knitr bootstrap framework")
    (description
     "This package provides a framework to create Bootstrap 3 HTML reports
from knitr Rmarkdown.")
    (license license:expat)))

(define-public r-microbenchmark
  (package
    (name "r-microbenchmark")
    (version "1.4.9")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "microbenchmark" version))
              (sha256
               (base32
                "1lpzpffnjiwnxxl0jhrvyj88fvxqksnpccbpq953xwqf6ypjqga4"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/microbenchmark/")
    (synopsis "Accurate timing functions for R")
    (description
     "This package provides infrastructure to accurately measure and compare
the execution time of R expressions.")
    (license license:bsd-2)))

(define-public r-pryr
  (package
    (name "r-pryr")
    (version "0.1.5")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "pryr" version))
              (sha256
               (base32
                "02vp1y7zhv22id43j5c0gdcgn9171dyypqp8rqrlc3w5a7n565kv"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-codetools r-lobstr r-rcpp r-stringr))
    (home-page "https://github.com/hadley/pryr")
    (synopsis "Tools for computing on the R language")
    (description
     "This package provides useful tools to pry back the covers of R and
understand the language at a deeper level.")
    (license license:gpl2)))

(define-public r-memoise
  (package
    (name "r-memoise")
    (version "2.0.1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "memoise" version))
              (sha256
               (base32
                "1srdzv2bp0splislrabmf1sfbqfi3hn189nq7kxhgjn8k3p38l7q"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-cachem r-rlang))
    (home-page "https://github.com/hadley/memoise")
    (synopsis "Memoise functions for R")
    (description
     "This R package caches the results of a function so that when
you call it again with the same arguments it returns the pre-computed value.")
    (license license:expat)))

(define-public r-crayon
  (package
    (name "r-crayon")
    (version "1.4.2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "crayon" version))
              (sha256
               (base32
                "0mxq87qsvz5znfncrb6yjchzv6iwpn9lr386i02y6xiycizkjd7f"))))
    (build-system r-build-system)
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
    (version "3.1.2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "testthat" version))
              (sha256
               (base32
                "0ndnmv3gbhlsyqya382wqfkkhlrgpdjmh7mymsv6ja52ihbachgd"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-brio
           r-callr
           r-cli
           r-crayon
           r-desc
           r-digest
           r-ellipsis
           r-evaluate
           r-jsonlite
           r-lifecycle
           r-magrittr
           r-pkgload
           r-praise
           r-processx
           r-ps
           r-r6
           r-rlang
           r-waldo
           r-withr))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/hadley/testthat")
    (synopsis "Unit testing for R")
    (description
     "This package provides a unit testing system for R designed to be fun,
flexible and easy to set up.")
    (license license:expat)))

(define-public r-r6
  (package
    (name "r-r6")
    (version "2.5.1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "R6" version))
              (sha256
               (base32
                "0j5z0b0myzjyyykk310xsa9n2mcm9bz8yqbq4xgz2yzdq8lvv4ld"))))
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
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "rlang" version))
              (sha256
               (base32
                "0skf75rhq9y6f09shcjlm1dlh7rlwpyr4mx1l8xn201igg4k8qdb"))))
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
    (version "3.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "tibble" version))
       (sha256
        (base32
         "14l2i6ri3v5y0h8g0ahc4kz0pdqsp5yabxk2w8wavss6y44xjcsv"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-ellipsis
           r-fansi
           r-lifecycle
           r-magrittr
           r-pkgconfig
           r-pillar
           r-rlang
           r-vctrs))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/hadley/tibble")
    (synopsis "Simple data frames")
    (description
     "This package provides a @code{tbl_df} class that offers better checking
and printing capabilities than traditional data frames.")
    (license license:expat)))

(define-public r-dplyr
  (package
    (name "r-dplyr")
    (version "1.0.7")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "dplyr" version))
              (sha256
               (base32
                "0g8jsxclj9cgm7a2lyazy78sj62rbzw812lsw3fdsbz0pknkmznj"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-ellipsis
           r-generics
           r-glue
           r-lifecycle
           r-magrittr
           r-pillar
           r-r6
           r-rlang
           r-tibble
           r-tidyselect
           r-vctrs))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/hadley/dplyr")
    (synopsis "Tools for working with data frames in R")
    (description
     "dplyr is the next iteration of plyr.  It is focused on tools for
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
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "dbplyr" version))
       (sha256
        (base32
         "025wqpmxdhzblb0pf58m3qh5h6bf5x8qvkf47vyl1cjsp13wz95b"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-assertthat
           r-blob
           r-dbi
           r-dplyr
           r-ellipsis
           r-glue
           r-lifecycle
           r-magrittr
           r-purrr
           r-r6
           r-rlang
           r-tibble
           r-tidyselect
           r-vctrs
           r-withr))
    (native-inputs
     (list r-knitr))
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
     (list gfortran))
    (home-page "https://cran.r-project.org/web/packages/acepack")
    (synopsis "Functions for regression transformations")
    (description
     "This package provides ACE and AVAS methods for choosing regression
transformations.")
    (license license:expat)))

(define-public r-formula
  (package
    (name "r-formula")
    (version "1.2-4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Formula" version))
       (sha256
        (base32
         "1bsw282i5m8rlirbhz7hqvf25zbxschk7yrp152whbzdnmry6w6b"))))
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
    (version "1.5-9.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "locfit" version))
       (sha256
        (base32
         "1vcw16qww4v4547w0gfvhqg60l051bsdlxa6zfcgcj9xbxf6dlyr"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-lattice))
    (home-page "https://cran.r-project.org/web/packages/locfit")
    (synopsis "Local regression, likelihood and density estimation")
    (description
     "This package provides functions used for local regression, likelihood
and density estimation.")
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-chron
  (package
    (name "r-chron")
    (version "2.3-56")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "chron" version))
              (sha256
               (base32
                "0p9zz96jfyf139924hdwkzhr8knkjvx6547ac53rknm3a6wwngl6"))))
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
    (version "1.14.2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "data.table" version))
              (sha256
               (base32
                "0arx5wna0sh0vf5q8rjhh8nqdmnvg2pdpbhljl9l0x4kwm8vjhgp"))))
    (build-system r-build-system)
    (inputs
     (list zlib))
    (native-inputs
     (list pkg-config r-knitr))
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
    (version "1.8-4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "xtable" version))
       (sha256
        (base32
         "077xfm0gphvhsay75amd9v90zk57kjgrrlgih04fyrbqqblc1gjs"))))
    (build-system r-build-system)
    (native-inputs
     (list r-knitr)) ; for vignettes
    (home-page "http://xtable.r-forge.r-project.org/")
    (synopsis "Export R tables to LaTeX or HTML")
    (description
     "This package provides tools to export R data as LaTeX and HTML tables.")
    (license license:gpl2+)))

(define-public python-patsy
  (package
    (name "python-patsy")
    (version "0.5.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "patsy" version))
              (sha256
               (base32
                "17dn72519gvwifw3i8mzwlslxmxkl8ihzfrxg1iblsk70iwdwlsh"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests? (invoke "pytest" "-vv")))))))
    (propagated-inputs
     (list python-numpy python-scipy python-six))
    (native-inputs
     (list python-pytest))
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
    (version "0.13.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "statsmodels" version))
       (sha256
        (base32 "0sbsyxgpzhys5padhkhrj71z4i1q41sm938pz0x8ff6jjvcchvh0"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (for-each delete-file (find-files "." "\\.c$"))))))
    (build-system python-build-system)
    (arguments
     `(;; The test suite is very large and rather brittle.  Tests often fail
       ;; because of minor changes in dependencies that upstream hasn't fixed
       ;; in a new release.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
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
            #t)))))
    (propagated-inputs
     (list python-numpy python-scipy python-pandas python-patsy
           python-matplotlib))
    (native-inputs
     (list python-cython python-nose python-sphinx))
    (home-page "http://statsmodels.sourceforge.net/")
    (synopsis "Statistical modeling and econometrics in Python")
    (description
     "Statsmodels is a Python package that provides a complement to scipy for
statistical computations including descriptive statistics and estimation and
inference for statistical models.")
    (license license:bsd-3)))

(define-public python2-statsmodels
  (let ((stats (package-with-python2 python-statsmodels)))
    (package/inherit stats
      ;; Version 0.11.1 and later does not work with Python 2.  It may build
      ;; fine but loading modules fails.
      (version "0.9.0")
      (source
       (origin
         (method url-fetch)
         (uri (pypi-uri "statsmodels" version))
         (sha256
          (base32
           "0fxs8a7sp4d7jvqlm36yi45i2d28kjfvraf9q8i9jr1chhxgjqb4"))))
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
    (version "0.19-4")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "coda" version))
              (sha256
               (base32
                "13z5dwfpnyyhpsbpg4xr9g5c1685jhqj90f4x4qkcykr6kykqba2"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-lattice))
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
    (version "1.7-18")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "ade4" version))
        (sha256
          (base32
            "09f5kh2mwygqpnvf53k0r54k0cvaafxm9x4nm819gwv05k2g9dpc"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-mass r-pixmap r-sp))
    (home-page "http://pbil.univ-lyon1.fr/ADE-4")
    (synopsis "Multivariate data analysis and graphical display")
    (description
     "The ade4 package contains data analysis functions to analyze ecological
and environmental data in the framework of Euclidean exploratory methods.")
    (license license:gpl2+)))

(define-public r-xml2
  (package
    (name "r-xml2")
    (version "1.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "xml2" version))
       (sha256
        (base32
         "138w7hb487al9cbahmnk5rhi23k8a9g7nk7s5dyxd3k1646rqknb"))))
    (build-system r-build-system)
    (inputs
     (list libxml2 zlib))
    (native-inputs
     (list pkg-config r-knitr))
    (home-page "https://github.com/hadley/xml2")
    (synopsis "Parse XML with R")
    (description
     "This package provides a simple, consistent interface to working with XML
files in R.  It is built on top of the libxml2 C library.")
    (license license:gpl2+)))

(define-public r-multitaper
  (package
    (name "r-multitaper")
    (version "1.0-15")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "multitaper" version))
       (sha256
        (base32
         "1gm3wr8xqpqiby7q1dr3zxim77v0bvvrqi0hlazf5g3gnkrp2zc3"))))
    (build-system r-build-system)
    (native-inputs
     (list gfortran))
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
    (version "2.1.1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "rversions" version))
              (sha256
               (base32
                "1cbwwbz8lqx2n33xr4q904whvld22s63rpyy1nn93395l7ssrakr"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-curl r-xml2))
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
    (version "0.4")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "whisker" version))
              (sha256
               (base32
                "1a7vz0dk95xfjvi38wbpw8vmf5qn3g8p490msz2rw0piwidmk1ks"))))
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
    (version "1.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "backports" version))
       (sha256
        (base32
         "0x144hij0rzhrxdq3k5ph8fk7pdgl1avjca25j4mlpmhzdckqp44"))))
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
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "checkmate" version))
       (sha256
        (base32
         "193gkifr9spp0x0rwnjq1spdhjkfkh4mv27iklskcj604075phhd"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-backports))
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
     (list r-checkmate))
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
     (list r-bbmisc r-checkmate))
    (home-page "https://github.com/mllg/fail")
    (synopsis "File abstraction interface layer (FAIL)")
    (description
     "This package provides a more comfortable interface to work with R data
or source files in a key-value fashion.")
    (license license:bsd-3)))

(define-public r-batchjobs
  (package
    (name "r-batchjobs")
    (version "1.8")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "BatchJobs" version))
       (sha256
        (base32
         "1zz4sspj596pijxlz321g0jy2rg9ki83j6fihbwisjwr66p2vk1m"))))
    (properties `((upstream-name . "BatchJobs")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-backports
           r-bbmisc
           r-brew
           r-checkmate
           r-data-table
           r-dbi
           r-digest
           r-rsqlite
           r-sendmailr
           r-stringi))
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
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "desc" version))
       (sha256
        (base32
         "0jfnwn1kpiill8v4hmzdk90yz4gk2ha0prr2p0hq36s40v3y8842"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-crayon r-r6 r-rprojroot))
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
    (version "1.7")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "commonmark" version))
       (sha256
        (base32
         "024iz1qp2kv6iz6y69ir0jk3qhjps86rhkzlcmhqsxx97rx7cjni"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/commonmark")
    (synopsis "CommonMark and Github Markdown Rendering in R")
    (description
     "The CommonMark specification defines a rationalized version of markdown
syntax.  This package uses the @code{cmark} reference implementation for
converting markdown text into various formats including HTML, LaTeX and groff
man.  In addition, it exposes the markdown parse tree in XML format.  The latest
version of this package also adds support for Github extensions including
tables, autolinks and strikethrough text.")
    (license license:bsd-2)))

(define-public r-roxygen2
  (package
    (name "r-roxygen2")
    (version "7.1.2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "roxygen2" version))
              (sha256
               (base32
                "1zf55wywlpy7mk7zfyi3s9fhhg1m9kvafzj46iqw5cbvnlg3ssdk"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-brew
           r-commonmark
           r-cpp11
           r-desc
           r-digest
           r-knitr
           r-pkgload
           r-purrr
           r-r6
           r-rlang
           r-stringi
           r-stringr
           r-xml2))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/klutometis/roxygen")
    (synopsis "In-source documentation system for R")
    (description
     "Roxygen2 is a Doxygen-like in-source documentation system for Rd,
collation, and NAMESPACE files.")
    (license license:gpl2+)))

(define-public r-openssl
  (package
    (name "r-openssl")
    (version "1.4.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "openssl" version))
       (sha256
        (base32
         "0na3yficxg0hjql9pvz96f66yh4g5k2hfwcricb705z7f2pk5f23"))))
    (build-system r-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'do-not-use-versioned-linking
           (lambda _
             (substitute* "configure"
               (("PKG_LIBS=\"\\$\\{PKG_LIBS_VERSIONED\\}\"")
                "PKG_LIBS=\"${PKG_LIBS}\"")))))))
    (inputs
     (list openssl))
    (native-inputs
     (list pkg-config r-knitr))
    (propagated-inputs
     (list r-askpass))
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
    (version "1.4.2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "httr" version))
              (sha256
               (base32
                "1y8y1g1dkgv9jafrk1kj6pzxpw95c0rr9lplblfq2byrs1pfsas6"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-curl r-jsonlite r-openssl r-mime r-r6))
    (native-inputs
     (list r-knitr))
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
    (version "0.29.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "git2r" version))
              (sha256
               (base32
                "13nlrhma6kavvhranb43w3003vadfkyrn2f4l3r63iqavj0s3xzq"))))
    (build-system r-build-system)
    (inputs
     (list libgit2 zlib))
    (native-inputs
     (list pkg-config))
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
    (version "0.13")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "rstudioapi" version))
              (sha256
               (base32
                "12vdfzzjc6mv4h105l8cp108j3hjk0mqmg23m6mqr3jarfymphxa"))))
    (build-system r-build-system)
    (native-inputs
     (list r-knitr))
    (home-page "https://cran.r-project.org/web/packages/rstudioapi")
    (synopsis "Safely access the RStudio API")
    (description
     "This package provides functions to access the RStudio API and provide
informative error messages when it's not available.")
    (license license:expat)))

(define-public r-devtools
  (package
    (name "r-devtools")
    (version "2.4.3")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "devtools" version))
              (sha256
               (base32
                "0kvbw218jp8pd2lgdjyn94v2iji75s385hkn1qbikcm0pjlmj8g4"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-callr
           r-cli
           r-desc
           r-ellipsis
           r-fs
           r-httr
           r-lifecycle
           r-memoise
           r-pkgbuild
           r-pkgload
           r-rcmdcheck
           r-remotes
           r-rlang
           r-roxygen2
           r-rstudioapi
           r-rversions
           r-sessioninfo
           r-testthat
           r-usethis
           r-withr))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/hadley/devtools")
    (synopsis "Tools to make developing R packages easier")
    (description "The devtools package is a collection of package development
tools to simplify the devolpment of R packages.")
    (license license:gpl2+)))

(define-public r-withr
  (package
    (name "r-withr")
    (version "2.4.3")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "withr" version))
              (sha256
               (base32
                "0k4x2bd9wb24n3dyg7rdjicdjs5ay2kk5lfgkrcjsv6cki2wgnlv"))))
    (build-system r-build-system)
    (native-inputs
     (list r-knitr))
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
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "hms" version))
       (sha256
        (base32
         "0qlh4s1g9rbm6c3s906dhw3xsz28m8fg6cyynlkhvis533dk0pvb"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-ellipsis r-lifecycle r-pkgconfig r-rlang r-vctrs))
    (home-page "https://github.com/rstats-db/hms")
    (synopsis "Pretty time of day")
    (description
     "This package implements an S3 class for storing and formatting
time-of-day values, based on the @code{difftime} class.")
    (license license:gpl3+)))

(define-public r-readr
  (package
    (name "r-readr")
    (version "2.1.2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "readr" version))
              (sha256
               (base32
                "06qcw57yz0fhh3ni41d5w68cbmwmb6zkp47chllwzax43wxd1bwl"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-cli
           r-clipr
           r-cpp11
           r-crayon
           r-hms
           r-lifecycle
           r-tibble
           r-r6
           r-rlang
           r-tzdb
           r-vroom))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/hadley/readr")
    (synopsis "Read tabular data")
    (description
     "This package provides functions to read flat or tabular text files from
disk (or a connection).")
    (license license:gpl2+)))

(define-public r-plotrix
  (package
    (name "r-plotrix")
    (version "3.8-2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "plotrix" version))
              (sha256
               (base32
                "07xwq3505qb2yak7pfda22yqwifj6m78f8b5rm0ym74808qrawmv"))))
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
    (version "0.6-29")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "latticeExtra" version))
       (sha256
        (base32
         "04fzwcqs594rk5qp98yilh3kdh3sw4xf8wld5rp94ggpaqfw7bbc"))))
    (properties `((upstream-name . "latticeExtra")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-jpeg r-lattice r-png r-rcolorbrewer))
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
    (version "0.10.8.1.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "RcppArmadillo" version))
              (sha256
               (base32
                "1x7mk85rc4pmn17nm63k6rbkarsmghjapznias268545nfpib97g"))))
    (properties `((upstream-name . "RcppArmadillo")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-rcpp))
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
    (version "1.0-7")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "bitops" version))
              (sha256
               (base32
                "1i0phmq5yynla5x0frqll1gbf5ww59p3sgz1s06a354zqf9grdg9"))))
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
    (version "1.18.2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "caTools" version))
              (sha256
               (base32
                "14q2ry8gaszjl0m97qg62dxv5bpj6k02qwyi7q2lnxgcmwai3mkm"))))
    (properties `((upstream-name . "caTools")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bitops))
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
    (version "2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rprojroot" version))
       (sha256
        (base32
         "07wy07yhms8zln9qb0iwx69dq08h4lrdi8kavjcplfxcskq638az"))))
    (build-system r-build-system)
    (native-inputs
     (list r-knitr))
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
    (version "2.11")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "rmarkdown" version))
        (sha256
          (base32 "0xcpg9d009a71p5i6jczw02qcp907p5d52lp6vclrsp7019jawck"))))
    (properties `((upstream-name . "rmarkdown")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-evaluate
           r-htmltools
           r-jquerylib
           r-jsonlite
           r-knitr
           r-stringr
           r-tinytex
           r-xfun
           r-yaml
           pandoc))
    (native-inputs
     (list r-knitr))
    (home-page "https://rmarkdown.rstudio.com")
    (synopsis "Convert R Markdown documents into a variety of formats")
    (description
     "This package provides tools to convert R Markdown documents into a
variety of formats.")
    (license license:gpl3+)))

(define-public r-gtable
  (package
    (name "r-gtable")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "gtable" version))
              (sha256
               (base32
                "1lyncxf2dqdjgw1071cn9c8zwzkz6sldnd5cgmicf70bc726qf7x"))))
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
     (list r-gtable))
    (native-inputs
     (list r-knitr)) ;for building vignettes
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
    (version "2.0.3")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "pkgconfig" version))
              (sha256
               (base32
                "0l1qph8zyi2sic3k2qcd7vlfl7rzfh1q7z7zvjkl5f7y1x2fy3rk"))))
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
    (version "1.2.2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "blob" version))
              (sha256
               (base32
                "1yn7f13icaix0apxp4drnciwdn8bx8xmbd129jd7ck4rcly0axj9"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-rlang r-vctrs))
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
    (version "2.2.9")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "RSQLite" version))
              (sha256
               (base32
                "075vsnbkcwpwnn87b06mhbg2fflqhjvbylhbkg0d3v3rl7zg28s4"))))
    (properties `((upstream-name . "RSQLite")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-dbi
           r-memoise
           r-plogr
           r-rcpp
           r-bit64
           r-blob
           r-pkgconfig))
    (native-inputs
     (list r-knitr))
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
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'respect-CURL_CA_BUNDLE
           (lambda _
             (substitute* "R/options.S"
               (("\\.els = rev\\(merge\\(list\\(\\.\\.\\.\\), \\.opts\\)\\)" m)
                (string-append "\
certs = Sys.getenv(\"CURL_CA_BUNDLE\")
if (certs != \"\") { .opts = merge.list(.opts, list(cainfo=certs)) }
" m)))
             #t)))))
    (inputs
     `(("libcurl" ,curl)))
    (propagated-inputs
     (list r-bitops))
    (home-page "http://www.omegahat.net/RCurl")
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
    (version "3.99-0.8")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "XML" version))
              (sha256
               (base32
                "0l3bsgqw4bicv3sl2bfag7p4rh1y2pcyg0ajr73kkbg85qf6j7q8"))))
    (properties
     `((upstream-name . "XML")))
    (build-system r-build-system)
    (inputs
     (list libxml2 zlib))
    (native-inputs
     (list pkg-config))
    (home-page "http://www.omegahat.net/RSXML")
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
    (version "1.2.4")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "lambda.r" version))
              (sha256
               (base32
                "1mh1g0gsd58gng0hb29vww2yqb2jfs07kba5kxnnqck5j3izwlnj"))))
    (properties `((upstream-name . "lambda.r")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-formatr))
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
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "futile.options" version))
              (sha256
               (base32
                "0w15agpi88y3qkv6fl72zy2pzyplzgvnj41a4ixhg64mw1sck73s"))))
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
     (list r-futile-options r-lambda-r))
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
    (version "1.4-0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "segmented" version))
       (sha256
        (base32
         "0xn28gyqzmyz0kv352rkkznbj806arxj2am5a9gqsn1qzv9l0s9h"))))
    (build-system r-build-system)
    (propagated-inputs (list r-mass))
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
    (version "0.4-4")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "snow" version))
              (sha256
               (base32
                "1j8kvf3imxijsqkdjz4i9s7qggfxqrpas46y5wz6za92y937yn44"))))
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
    (version "1.81")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "SparseM" version))
              (sha256
               (base32
                "0csbqd9wm6r4162mday1iaigzf9wv2rp1zq8hnihys6f38w8z0xx"))))
    (properties
     `((upstream-name . "SparseM")))
    (native-inputs
     (list gfortran))
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
    (version "1.0.13")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "iterators" version))
       (sha256
        (base32
         "190i1y2493v5c9z8978rb3idlwsjymy67k1assa9znljqbj313kp"))))
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
    (version "1.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "foreach" version))
       (sha256
        (base32
         "1qb83b1jvpmk8d6kfibkm5q2hiki0x5gipy758mwa62n56gdcnpv"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-codetools r-iterators))
    (native-inputs
     (list r-knitr))
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
    (version "1.0.16")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "doParallel" version))
       (sha256
        (base32
         "1cjf0mxrxgls5pl7aj22fv9j488craib1wkc9np4f3gkckwjdfzi"))))
    (properties `((upstream-name . "doParallel")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-foreach r-iterators))
    (home-page "https://cran.r-project.org/web/packages/doParallel")
    (synopsis "Foreach parallel adaptor for the 'parallel' package")
    (description
     "This package provides a parallel backend for the @code{%dopar%} function
using the parallel package.")
    (license license:gpl2+)))

(define-public r-domc
  (package
    (name "r-domc")
    (version "1.3.7")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "doMC" version))
       (sha256
        (base32
         "0kdmfhp5m1wisjbfyl9d040jrmjmhcgjb0vdi536g2i9vixb5yny"))))
    (properties `((upstream-name . "doMC")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-foreach r-iterators))
    (home-page "https://cran.r-project.org/web/packages/doMC")
    (synopsis "Foreach parallel adaptor for the 'parallel' package")
    (description
     "This package provides a parallel backend for the @code{%dopar%} function
using the multicore functionality of the parallel package.")
    (license license:gpl2+)))

(define-public r-dt
  (let ((javascript-sources
         '(("https://cdn.datatables.net/1.10.20/js/jquery.dataTables.js"
            "03ln7ys1q1hy3xpsrjxnjpg9hq3lfpqz0firrxsgjzj8fsw20is3"
            "datatables")
           ("https://cdn.datatables.net/1.10.20/js/dataTables.bootstrap.js"
            "16clrnxm7axn6cdimyf3qbskxg10gpn9ld5ls2xdfw5q1qf2i4ml"
            "datatables")
           ("https://cdn.datatables.net/1.10.20/js/dataTables.bootstrap4.js"
            "16v49zqxr1zil19bcx3wdnv95zdpiz2m979aazan7z04ymqb2rzb"
            "datatables")
           ("https://cdn.datatables.net/1.10.20/js/dataTables.foundation.js"
            "1gpjm1pi2pl0hxsn0pg3s3f382y2s7nsr06866vxld6gb8054lld"
            "datatables")
           ("https://cdn.datatables.net/1.10.20/js/dataTables.jqueryui.js"
            "0nxd8jph34vsk8k8whs2yiybrn6nsnwzhri0bxn2k1dzmcvpn24i"
            "datatables")
           ("https://cdn.datatables.net/1.10.20/js/dataTables.semanticui.js"
            "1477f49xyxs4phias789mbspv23w8alxchhl5b5iy0aw6vd35c43"
            "datatables")

           ("https://cdn.datatables.net/autofill/2.3.4/js/dataTables.autoFill.js"
            "04i6n7r3512gzfihl5wnhrvm0klnjp41g1z6cny3j803hvmnp8zk"
            "datatables-extensions/AutoFill")
           ("https://cdn.datatables.net/autofill/2.3.4/js/autoFill.bootstrap.js"
            "1zi7iiq63i5qx3p9cyynn6am4idxwj8xaz8mp4n3klm1x68sc0ja"
            "datatables-extensions/AutoFill")
           ("https://cdn.datatables.net/autofill/2.3.4/js/autoFill.bootstrap4.js"
            "1vk2smcz14raf0cz88a65yf36a7mnmbml02q03apg2b8bqy91m7w"
            "datatables-extensions/AutoFill")
           ("https://cdn.datatables.net/autofill/2.3.4/js/autoFill.foundation.js"
            "0sbcib1461pkglk69fzzqi73g4abylah74f264v0f79dc5247yzz"
            "datatables-extensions/AutoFill")
           ("https://cdn.datatables.net/autofill/2.3.4/js/autoFill.jqueryui.js"
            "1dw9vbbchzbd2c7id8na2p1cxac2kgjbzjrvqafra715hr0i4z3z"
            "datatables-extensions/AutoFill")
           ("https://cdn.datatables.net/autofill/2.3.4/js/autoFill.semanticui.js"
            "07ck81y6wpqchq8jfym6gjgc57xwj6vv9h5w9grc1gps6p7q9xnr"
            "datatables-extensions/AutoFill")

           ("https://cdn.datatables.net/buttons/1.6.1/js/dataTables.buttons.js"
            "15l9kd9898zm8xf996d5c761rwl55k4w718k9k5fzz2gh91g21g5"
            "datatables-extensions/Buttons")
           ("https://cdn.datatables.net/buttons/1.6.1/js/buttons.colVis.js"
            "1rqlv5pacipl652xgyzsdq1gbfwv52rwl4mr2fx9a3py21yskppk"
            "datatables-extensions/Buttons")
           ("https://cdn.datatables.net/buttons/1.6.1/js/buttons.flash.js"
            "150r2ypxl017kl5agrn17cnyvwpf7x2x7vkqbc1qxif8vclf35kj"
            "datatables-extensions/Buttons")
           ("https://cdn.datatables.net/buttons/1.6.1/js/buttons.html5.js"
            "1d8is99yrh95hycjijzbrbxy1anfslab6krmhj2xbwsmssyn16xh"
            "datatables-extensions/Buttons")
           ("https://cdn.datatables.net/buttons/1.6.1/js/buttons.print.js"
            "1irgspv2zidv6v0ay92152d8cvhz2zyrwb71xk3nw903223vc2gl"
            "datatables-extensions/Buttons")
           ("https://cdn.datatables.net/buttons/1.6.1/js/buttons.semanticui.js"
            "1p02r953ampxlzfzpay227ya6qdzsxz2anjxpnx3q8qs6gv6y2jl"
            "datatables-extensions/Buttons")
           ("https://cdn.datatables.net/buttons/1.6.1/js/buttons.foundation.js"
            "0aykm1sk8rwvxp5r4qnvbb2scx2bln5kh88h36829mcqcdksfc50"
            "datatables-extensions/Buttons")
           ("https://cdn.datatables.net/buttons/1.6.1/js/buttons.jqueryui.js"
            "1im6f6jw3yc3959rw1i3bghvz863kmp3wgfvz661r1r2wjzzfs4d"
            "datatables-extensions/Buttons")
           ("https://cdn.datatables.net/buttons/1.6.1/js/buttons.bootstrap.js"
            "1mzmpabhrk0iag7hb16n8bhghx4cprq39p2vqn3v65mpklajzznc"
            "datatables-extensions/Buttons")
           ("https://cdn.datatables.net/buttons/1.6.1/js/buttons.bootstrap4.js"
            "0hfclipg43wr9p7irrcn9vp5wji8z7gz6y5mclkq88z1mlpwklzf"
            "datatables-extensions/Buttons")
           ;; It is not clear what version the bundled file corresponds to.
           ("https://cdnjs.cloudflare.com/ajax/libs/jszip/2.5.0/jszip.js"
            "18hpj9vwgki56ijb20l5gwmqa5p1d0xprn57z2mlg3ph6ll0bdkd"
            "datatables-extensions/Buttons")
           ("https://cdnjs.cloudflare.com/ajax/libs/pdfmake/0.1.36/pdfmake.js"
            "1sfw80az2cgzin5wk7q1p2n9zm66c35cz0m6isdygml81i594wia"
            "datatables-extensions/Buttons")
           ("https://cdnjs.cloudflare.com/ajax/libs/pdfmake/0.1.36/vfs_fonts.js"
            "1k324s0hw4lfpd71bb1cnv4j5096k8smk64fjdsh81sl0ykizf2w"
            "datatables-extensions/Buttons")

           ("https://cdn.datatables.net/colreorder/1.5.2/js/dataTables.colReorder.js"
            "1dalc28km19xzzszsa82hsd9alikrqpzjvf9vzxkccjpf7m2sdqg"
            "datatables-extensions/ColReorder")
           ("https://cdn.datatables.net/colreorder/1.5.2/js/colReorder.foundation.js"
            "0nrddc8swkmsfzji518kh6ks55ykyk9p8r4x5fmf8ckr9fhjkh0s"
            "datatables-extensions/ColReorder")
           ("https://cdn.datatables.net/colreorder/1.5.2/js/colReorder.bootstrap.js"
            "0crgmjwcn817yz6ibjkji6gsickvv2a4las9asyldfcpj2c99x84"
            "datatables-extensions/ColReorder")
           ("https://cdn.datatables.net/colreorder/1.5.2/js/colReorder.bootstrap4.js"
            "065fhw4v2d9rp3ic9zfb1q5d7pfq4f2949rr24hdjbspf19m3ymd"
            "datatables-extensions/ColReorder")
           ("https://cdn.datatables.net/colreorder/1.5.2/js/colReorder.semanticui.js"
            "17kw143ny0nq0yidsffw3cpghvlg2bzlzavfi0ihkamcn26ymxcp"
            "datatables-extensions/ColReorder")
           ("https://cdn.datatables.net/colreorder/1.5.2/js/colReorder.jqueryui.js"
            "1rd8hijz3prg2y36fvqczrpdzixibjy2dxgs2fmgr8wrm8k01rrm"
            "datatables-extensions/ColReorder")

           ("https://cdn.datatables.net/fixedcolumns/3.3.0/js/dataTables.fixedColumns.js"
            "0vsqk2fv59n351bdfcbvhmvpq38qwf41j1cn810xz1l1i07cg4hg"
            "datatables-extensions/FixedColumns")
           ("https://cdn.datatables.net/fixedcolumns/3.3.0/js/fixedColumns.bootstrap.js"
            "1j4wvg694l960gk5dg7wghwa3dpgq8mnrcgp78ghm92i08djb1wy"
            "datatables-extensions/FixedColumns")
           ("https://cdn.datatables.net/fixedcolumns/3.3.0/js/fixedColumns.bootstrap4.js"
            "1p79k9bjslyvmp1bdhmg4nm2l9nbfsi4kgw7rx3vjka3n50qy730"
            "datatables-extensions/FixedColumns")
           ("https://cdn.datatables.net/fixedcolumns/3.3.0/js/fixedColumns.foundation.js"
            "0f0xkrsapzgma58f6l63rpn68xid098dxwqqddsyddl0hy0x1z82"
            "datatables-extensions/FixedColumns")
           ("https://cdn.datatables.net/fixedcolumns/3.3.0/js/fixedColumns.jqueryui.js"
            "0lw35c8vkajx75pg4ddik4gyzmjak1jaw3flq850frwgnzsvhahx"
            "datatables-extensions/FixedColumns")
           ("https://cdn.datatables.net/fixedcolumns/3.3.0/js/fixedColumns.semanticui.js"
            "1kqsap9y0d25a7m5zjakipifl5qi2qr72kfj4ap3zxavd8md2wyn"
            "datatables-extensions/FixedColumns")

           ("https://cdn.datatables.net/fixedheader/3.1.6/js/dataTables.fixedHeader.js"
            "1ml5ilnm8nirr6rsgmzn75l1k0hcjz3sqk6h1y1gy8cpwpklvqri"
            "datatables-extensions/FixedHeader")
           ("https://cdn.datatables.net/fixedheader/3.1.6/js/fixedHeader.bootstrap.js"
            "1qf3pkb3svpia7g8bwyql7ma3x2g4zj5bp0d14pnv8xpc9h52r93"
            "datatables-extensions/FixedHeader")
           ("https://cdn.datatables.net/fixedheader/3.1.6/js/fixedHeader.bootstrap4.js"
            "19jcvnk7zh4k6fd5si3b743x70qzlkqiw3m10jbc5jzbpz8sj6qd"
            "datatables-extensions/FixedHeader")
           ("https://cdn.datatables.net/fixedheader/3.1.6/js/fixedHeader.foundation.js"
            "0xmpx1r76vykqygksyjaf4d1ql1fid69rqhvk4k857iybqz3gdcv"
            "datatables-extensions/FixedHeader")
           ("https://cdn.datatables.net/fixedheader/3.1.6/js/fixedHeader.jqueryui.js"
            "1lc0g2cag1sj3bqmh7rh5z00pmfv1srxfhwi32y4mgpzhrzpfzxn"
            "datatables-extensions/FixedHeader")
           ("https://cdn.datatables.net/fixedheader/3.1.6/js/fixedHeader.semanticui.js"
            "1v0i6dc68h8l8673fb5970igzkl7as36riv504iyg82glfi7n877"
            "datatables-extensions/FixedHeader")

           ("https://cdn.datatables.net/keytable/2.5.1/js/dataTables.keyTable.js"
            "16iib2icxsjh93x5hd42gpsl7bzpcsqb7zjgj0m1s02ls45bdlv5"
            "datatables-extensions/KeyTable")
           ("https://cdn.datatables.net/keytable/2.5.1/js/keyTable.bootstrap.js"
            "0hnhk6am4yl6h6bb7as935k8h2syil9hf8g7nn409yd3ws736xpj"
            "datatables-extensions/KeyTable")
           ("https://cdn.datatables.net/keytable/2.5.1/js/keyTable.bootstrap4.js"
            "0r85mp5yf9hgl5ayzzs46dfbxa231bjlvgb8lqpyzik1m6afa51i"
            "datatables-extensions/KeyTable")
           ("https://cdn.datatables.net/keytable/2.5.1/js/keyTable.foundation.js"
            "11fr14p33lyvs0wfcx228m600i4qcaqb44q3hk723jxcz59k17dw"
            "datatables-extensions/KeyTable")
           ("https://cdn.datatables.net/keytable/2.5.1/js/keyTable.jqueryui.js"
            "0572rxrvwyprdr8l5jkgacj2bkmhmgxjy5vybm65n54g9j19l6bc"
            "datatables-extensions/KeyTable")
           ("https://cdn.datatables.net/keytable/2.5.1/js/keyTable.semanticui.js"
            "157mqn9mhmmf7vas2das4hbpwipk3wshs8n0808q04rbijr0g2bz"
            "datatables-extensions/KeyTable")

           ("https://cdn.datatables.net/responsive/2.2.3/js/dataTables.responsive.js"
            "1jnsx4sqf7qjd1gz5ag9hn6n76cwwfms23rzw37lgbd6h54yqzwr"
            "datatables-extensions/Responsive")
           ("https://cdn.datatables.net/responsive/2.2.3/js/responsive.foundation.js"
            "1vzzqpd9l8xv0am42g4cilx9igmq60mgk0hab4ssqvbicrmrgq9z"
            "datatables-extensions/Responsive")
           ("https://cdn.datatables.net/responsive/2.2.3/js/responsive.semanticui.js"
            "1cjiwcf0d07482k08dhn5ffsizshw4hqgz5l58p03pq9g6wc9pvm"
            "datatables-extensions/Responsive")
           ("https://cdn.datatables.net/responsive/2.2.3/js/responsive.jqueryui.js"
            "10nykak2kf4sai64girh26xdmdil29jvw3zja2rpp2qzjg4172z9"
            "datatables-extensions/Responsive")
           ("https://cdn.datatables.net/responsive/2.2.3/js/responsive.bootstrap.js"
            "1xxlh01vmzmfwwlsa611pl2nrl2sx58rp8xmx301bfsylmp2v5b2"
            "datatables-extensions/Responsive")
           ("https://cdn.datatables.net/responsive/2.2.3/js/responsive.bootstrap4.js"
            "1zjh15p7n1038sggaxv1xvcwbkhw2nk1ndx745s6cxiqb69y3i0h"
            "datatables-extensions/Responsive")

           ("https://cdn.datatables.net/rowgroup/1.1.1/js/dataTables.rowGroup.js"
            "0s4q7ir2d6q36g29nn9mqk7vrqrdig2mm5zbcv0sn2lixqi29pkj"
            "datatables-extensions/RowGroup")
           ("https://cdn.datatables.net/rowgroup/1.1.1/js/rowGroup.bootstrap.js"
            "1xfdhqgznz9x1v8spvql6b0wbna13h8cbzvkjza14nqsmccxck66"
            "datatables-extensions/RowGroup")
           ("https://cdn.datatables.net/rowgroup/1.1.1/js/rowGroup.bootstrap4.js"
            "1xm53sda4fabwdaglngrj09bpiygkn9mm17grxbykn1jazqqdp62"
            "datatables-extensions/RowGroup")
           ("https://cdn.datatables.net/rowgroup/1.1.1/js/rowGroup.foundation.js"
            "0832i10vils1wv1sm10qvsnd4i2k2xkhskz6i9y2q0axkmk73hcd"
            "datatables-extensions/RowGroup")
           ("https://cdn.datatables.net/rowgroup/1.1.1/js/rowGroup.jqueryui.js"
            "0n53cd294s9mjblkykkqvd9n414bsc26wpcg5spxdscjl6hxh79p"
            "datatables-extensions/RowGroup")
           ("https://cdn.datatables.net/rowgroup/1.1.1/js/rowGroup.semanticui.js"
            "010wls5nf387p21fdc2k952bxq89r5kxkv7j4wbvwf8k2a18cmc9"
            "datatables-extensions/RowGroup")

           ("https://cdn.datatables.net/rowreorder/1.2.6/js/dataTables.rowReorder.js"
            "13ymbn3h9755pgb0gmlb9gl54vz9nqnz4mws7g6mlmz53r3sqhmj"
            "datatables-extensions/RowReorder")
           ("https://cdn.datatables.net/rowreorder/1.2.6/js/rowReorder.bootstrap.js"
            "185if2pxgc940rm49hdgln57pc5h9cszlii3bfpdf3pdc1fjhckm"
            "datatables-extensions/RowReorder")
           ("https://cdn.datatables.net/rowreorder/1.2.6/js/rowReorder.bootstrap4.js"
            "14129x4md57i4ff7j18m49jn5fw8r716np84cdrcawlydgjsxp4a"
            "datatables-extensions/RowReorder")
           ("https://cdn.datatables.net/rowreorder/1.2.6/js/rowReorder.foundation.js"
            "0zg94jckymxzda2xjyj9p38y5v61cji55kak1ylq72l6a9sw8sg6"
            "datatables-extensions/RowReorder")
           ("https://cdn.datatables.net/rowreorder/1.2.6/js/rowReorder.jqueryui.js"
            "08gm419xcixgqw0i5yv2mxyyvafhzviibifp6nv129vdxx0a5d8v"
            "datatables-extensions/RowReorder")
           ("https://cdn.datatables.net/rowreorder/1.2.6/js/rowReorder.semanticui.js"
            "1zjrx2rlgw3qannsqa88pcp3i4pc87pwv7rmgfw1dar8namkr9kk"
            "datatables-extensions/RowReorder")

           ("https://cdn.datatables.net/scroller/2.0.1/js/dataTables.scroller.js"
            "0zfjjdvwwlsnps24i9l4c97hmway2qs6addks1is5bxl4k1r6d16"
            "datatables-extensions/Scroller")
           ("https://cdn.datatables.net/scroller/2.0.1/js/scroller.foundation.js"
            "04bk6ink8wqay7655v93jvv86m3bn6asrsfb22i99rgxdvm8gn1z"
            "datatables-extensions/Scroller")
           ("https://cdn.datatables.net/scroller/2.0.1/js/scroller.bootstrap.js"
            "19dl40dl8ir21xvs1j7xhm2a4py1m21xbypwn499fg2awj8vaidi"
            "datatables-extensions/Scroller")
           ("https://cdn.datatables.net/scroller/2.0.1/js/scroller.bootstrap4.js"
            "0pbkgncijlafwdmyh4l65dabd18hzjh8r01cad3b9iy8cfif6iwd"
            "datatables-extensions/Scroller")
           ("https://cdn.datatables.net/scroller/2.0.1/js/scroller.jqueryui.js"
            "1md5mpx5in7wzsr38yn801cmv3phm0i0ikdnpd0b1nsna5ccpj14"
            "datatables-extensions/Scroller")
           ("https://cdn.datatables.net/scroller/2.0.1/js/scroller.semanticui.js"
            "1dfbblbzbryjgiv31qfdjnijz19lmyijg12win3y8gsgfd4fp9zz"
            "datatables-extensions/Scroller")

           ("https://cdn.datatables.net/searchbuilder/1.0.0/js/dataTables.searchBuilder.js"
            "0n5g0j0yfzqvdpsmwb27bj1rd8zx864fsx2k7b2kpv6mqqavzpqc"
            "datatables-extensions/SearchBuilder")
           ("https://cdn.datatables.net/searchbuilder/1.0.0/js/searchBuilder.bootstrap.js"
            "1gnd8rjcg9c96xayshn9rwinzgmlwzddczjlpfmf2j33npmyka2y"
            "datatables-extensions/SearchBuilder")
           ("https://cdn.datatables.net/searchbuilder/1.0.0/js/searchBuilder.bootstrap4.js"
            "0vdv5mi6zbp2dspmj0lw2vaqxvfadcydlmc6frqv4a68rms7wz05"
            "datatables-extensions/SearchBuilder")
           ("https://cdn.datatables.net/searchbuilder/1.0.0/js/searchBuilder.dataTables.js"
            "0fbzfnaqswb2xq7m1vdzcg7l7qi0wmyz64ml6k4002kp0dm4xnlx"
            "datatables-extensions/SearchBuilder")

           ("https://cdn.datatables.net/searchpanes/1.1.1/js/dataTables.searchPanes.js"
            "1s697avk42h24fsaq79d1kkw66dqig7xgpx9bvmhwncv8amkmz6i"
            "datatables-extensions/SearchPanes")
           ("https://cdn.datatables.net/searchpanes/1.1.1/js/searchPanes.bootstrap.js"
            "0n3z4fdx1nsga4l5hmd4s93piv9k0v607xd7q9h2zpq613if7sld"
            "datatables-extensions/SearchPanes")
           ("https://cdn.datatables.net/searchpanes/1.1.1/js/searchPanes.bootstrap4.js"
            "1i1arnvxp57z01wc207jxnw9h8clcish6l96c2gnmachgkaz8lqa"
            "datatables-extensions/SearchPanes")
           ("https://cdn.datatables.net/searchpanes/1.1.1/js/searchPanes.dataTables.js"
            "04zzg7i46igcd6gfvdln5alpgjn7m663yf9bf2f3fk9va4fvis6y"
            "datatables-extensions/SearchPanes")
           ("https://cdn.datatables.net/searchpanes/1.1.1/js/searchPanes.foundation.js"
            "0m78wdajxn1m3j9jn9jfwqf73wwsxrsfw4zf84h5y6saj4rrcz72"
            "datatables-extensions/SearchPanes")
           ("https://cdn.datatables.net/searchpanes/1.1.1/js/searchPanes.jqueryui.js"
            "0zb2x736isb8nxrmd7j8nb78lj8h0h9j3axnbjiybwzzk819xw1m"
            "datatables-extensions/SearchPanes")
           ("https://cdn.datatables.net/searchpanes/1.1.1/js/searchPanes.semanticui.js"
            "1781d0xmx7xz0jly0wsw2zbrdmfc1crahmcdbsfbj5s66kdsnd7c"
            "datatables-extensions/SearchPanes")

           ("https://cdn.datatables.net/select/1.3.1/js/dataTables.select.js"
            "0a7bkbz1cizhiq4h417b4rcdr7998pn8q4dlyzx8449xdp0h0n0v"
            "datatables-extensions/Select")
           ("https://cdn.datatables.net/select/1.3.1/js/select.bootstrap.js"
            "0mm5ly3p2iprlfi8ajz548rjqx8lz1sbjj5ysgqmwqg14gw7l9k7"
            "datatables-extensions/Select")
           ("https://cdn.datatables.net/select/1.3.1/js/select.bootstrap4.js"
            "1hv6d9lwgflmxhy7mdfb9rvli2wa2cbkdhqjz64zkf1a1a7wlb5q"
            "datatables-extensions/Select")
           ("https://cdn.datatables.net/select/1.3.1/js/select.foundation.js"
            "1zzygcbngvrqh7m22x0s23k8m5xj5fv1p466pzjs23p94qq24a2r"
            "datatables-extensions/Select")
           ("https://cdn.datatables.net/select/1.3.1/js/select.jqueryui.js"
            "1hv5zlmfifd27hylfqsji09y2hbp3m2hnb7j41418sjrxs63f6x6"
            "datatables-extensions/Select")
           ("https://cdn.datatables.net/select/1.3.1/js/select.semanticui.js"
            "0q6q3vb6pa5nmkxy7zcnjs0bkn4ldw8ykdcfrc04bf1d2hjjaw47"
            "datatables-extensions/Select"))))
    (package
      (name "r-dt")
      (version "0.20")
      (source (origin
                (method url-fetch)
                (uri (cran-uri "DT" version))
                (sha256
                 (base32
                  "17xs18i439p15agyk7z2xyyrwgrpn5pw09kd3jwxn7qhxi4pyvf6"))
                (modules '((guix build utils)))
                (snippet
                 '(for-each delete-file
                            (find-files "inst/htmlwidgets/lib" "\\.min\\.js$")))))
      (properties
       `((upstream-name . "DT")))
      (build-system r-build-system)
      (arguments
       `(#:modules
         ((guix build r-build-system)
          (guix build minify-build-system)
          (guix build utils)
          (ice-9 match))
         #:imported-modules
         (,@%r-build-system-modules
          (guix build minify-build-system))
         #:phases
         (modify-phases (@ (guix build r-build-system) %standard-phases)
           (add-after 'unpack 'process-javascript
             (lambda* (#:key inputs #:allow-other-keys)
               (with-directory-excursion "inst/htmlwidgets/lib/"
                 (for-each (match-lambda
                             ((url hash dir)
                              (let* ((input (string-append "js:" (basename url)))
                                     (source (assoc-ref inputs input))
                                     (target (string-append dir "/js/"
                                                            (basename url ".js")
                                                            ".min.js")))
                                (mkdir-p dir)
                                (minify source #:target target))))
                           ',javascript-sources)
                 (minify (string-append (assoc-ref inputs "datatables-plugins")
                                        "/features/scrollResize/dataTables.scrollResize.js")
                         #:target "datatables-plugins/features/scrollResize/source.min.js")
                 (minify (string-append (assoc-ref inputs "datatables-plugins")
                                        "/features/searchHighlight/dataTables.searchHighlight.js")
                         #:target "datatables-plugins/features/searchHighlight/source.min.js")
                 (minify (assoc-ref inputs "js-nouislider")
                         #:target "nouislider/jquery.nouislider.min.js")

                 (let ((replace-file (lambda (old new)
                                       (format #t "replacing ~a with ~a\n" old new)
                                       (symlink new old))))
                   (replace-file "selectize/selectize.min.js"
                                 (string-append (assoc-ref inputs "js-selectize")
                                                "/share/javascript/selectize.min.js")))))))))
      (propagated-inputs
       (list r-crosstalk
             r-htmltools
             r-htmlwidgets
             r-jquerylib
             r-jsonlite
             r-magrittr
             r-promises))
      (inputs
       (list js-selectize))
      (native-inputs
       `(("r-knitr" ,r-knitr)
         ("uglifyjs" ,node-uglify-js)
         ("datatables-plugins"
          ,(let ((version "1.10.20"))
             (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/DataTables/Plugins.git")
                     (commit version)))
               (file-name (git-file-name "datatables-plugins" version))
               (sha256
                (base32
                 "05zni20863ml1711lfllljdfkb3k05h0kpqhkijkbp0bp7q0ak94")))))
         ("js-nouislider"
          ,(let ((version "7.0.10"))
             (origin
               (method url-fetch)
               (uri (string-append "https://raw.githubusercontent.com/leongersen/noUiSlider/"
                                   version "/distribute/jquery.nouislider.js"))
               (sha256
                (base32
                 "1f7vsfcn7wwzngib6j0wpl0psd6qriiaa6kv728ynfn5da73zfxm")))))
         ,@(map (match-lambda
                  ((url hash dir)
                   `(,(string-append "js:" (basename url))
                     ,(origin (method url-fetch)
                              (uri url)
                              (sha256 (base32 hash))))))
                javascript-sources)))
      (home-page "https://rstudio.github.io/DT")
      (synopsis "R wrapper of the DataTables JavaScript library")
      (description
       "This package allows for data objects in R to be rendered as HTML tables
using the JavaScript library @code{DataTables} (typically via R Markdown or
Shiny).  The @code{DataTables} library has been included in this R package.")
      ;; The DT package as a whole is distributed under GPLv3.  The DT package
      ;; inludes other software components under different licenses:
      ;;
      ;;   * Expat: jquery.highlight.js, DataTables
      ;;   * WTFPL: noUiSlider
      (license (list license:gpl3
                     license:expat
                     license:wtfpl2)))))

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
    (home-page "https://www.rforge.net/base64enc")
    (synopsis "Tools for Base64 encoding")
    (description
     "This package provides tools for handling Base64 encoding.  It is more
flexible than the orphaned \"base64\" package.")
    (license license:gpl2+)))

(define-public r-irlba
  (package
    (name "r-irlba")
    (version "2.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "irlba" version))
       (sha256
        (base32
         "1fcv4vazg2sbsda8qqjjsjn2wagcp2bsahvznwm443j66q6qrz16"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-matrix))
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
   (version "4.1-3")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "glmnet" version))
     (sha256
      (base32 "0nij8v44b5dvp1vc843sfkl9ds83n6g687m2p37q1rdn82m3bg34"))))
   (build-system r-build-system)
   (native-inputs
    (list gfortran r-knitr))
   (propagated-inputs
    (list r-foreach
          r-matrix
          r-rcpp
          r-rcppeigen
          r-shape
          r-survival))
   (home-page "https://www.jstatsoft.org/article/view/v033i01")
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
    (version "0.32.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "pkgmaker" version))
       (sha256
        (base32
         "14ggsd24n5g5rvn0wl4w90ipxzmywqikh28llj89q6kpxwnv4iff"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-assertthat
           r-codetools
           r-digest
           r-registry
           r-stringr
           r-withr
           r-xtable))
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
     (version "0.5-1")
     (source
      (origin
        (method url-fetch)
        (uri (cran-uri "registry" version))
        (sha256
         (base32
          "1k3j6dx350awamr0dwwgkhfs46vsnj4nf08iw5byq0x7n3nkdsnz"))))
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
    (version "1.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rngtools" version))
       (sha256
        (base32
         "0kd7x214cqw7hzpmk1iqy1bn7j6x0ady0yz2hsdbclbq9k57d33z"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-digest))
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
    (version "0.15")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Rtsne" version))
       (sha256
        (base32
         "0v17vxizrs1msay24xl2bckfajr2c82wpqj07lyssbrq197nwdsn"))))
    (properties `((upstream-name . "Rtsne")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-rcpp))
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
    (version "1.7-9")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "e1071" version))
       (sha256
        (base32
         "09i2hmyf1bxansqp24p9n9djlzzarclqs173gjjv3fg0giga3ycv"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-class r-proxy))
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
    (version "1.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "synchronicity" version))
       (sha256
        (base32
         "1kgsk64aifjm3mfj102y3va7x1abypq2zi0cqbnjhl8fqyzp69hx"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bh r-bigmemory-sri r-rcpp r-uuid))
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
    (version "4.5.36")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "bigmemory" version))
       (sha256
        (base32
         "03pg8mxdc7q0249visjmc8bc3xmwxsfg3i2n6higicj4cfz7zihq"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-bh r-rcpp r-bigmemory-sri))
    (home-page "http://www.bigmemory.org")
    (synopsis "Manage large matrices with shared memory or memory-mapped files")
    (description "This package provides methods to create, store, access, and
manipulate large matrices.  Matrices are allocated to shared memory and may use
memory-mapped files.")
    ;; Users can choose either LGPLv3 or ASL2.0.
    (license (list license:lgpl3 license:asl2.0))))

(define-public r-r-methodss3
  (package
    (name "r-r-methodss3")
    (version "1.8.1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "R.methodsS3" version))
              (sha256
               (base32
                "16v1br7lf7vxv40z65giv9jnqkpnyqagcmgqa0s1jy7spj0zp64a"))))
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
    (version "1.24.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "R.oo" version))
              (sha256
               (base32
                "1divisff26fmiqg6vmabnwlrx079c066r8qvkakfp336vnwdm89p"))))
    (properties `((upstream-name . "R.oo")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-r-methodss3))
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
    (version "2.11.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "R.utils" version))
              (sha256
               (base32
                "1n7y4dzls7dc8fdkm9zncq4cpiblb2a9vwirwjkf72zpjpwn0a32"))))
    (properties `((upstream-name . "R.utils")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-r-methodss3 r-r-oo))
    (home-page "https://github.com/HenrikBengtsson/R.utils")
    (synopsis "Various programming utilities")
    (description
     "This package provides utility functions useful when programming and
developing R packages.")
    (license license:lgpl2.1+)))

(define-public r-r-cache
  (package
    (name "r-r-cache")
    (version "0.15.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "R.cache" version))
              (sha256
               (base32
                "09pipn1974ivjh4sm3n5fjlh31rilf87p30qwq7y25vriyqd7d5d"))))
    (properties `((upstream-name . "R.cache")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-HOME
           (lambda _ (setenv "HOME" "/tmp"))))))
    (propagated-inputs
     (list r-digest r-r-methodss3 r-r-oo r-r-utils))
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
    (version "0.44.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "R.rsp" version))
              (sha256
               (base32
                "0n9sxrffahgxcc8h9vgxg5i446g14cibixpf82nkpm5bvidhfsc9"))))
    (properties `((upstream-name . "R.rsp")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-HOME
           (lambda _ (setenv "HOME" "/tmp"))))))
    (propagated-inputs
     (list r-digest r-r-cache r-r-methodss3 r-r-oo r-r-utils))
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
    (version "1.1-3")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "mvtnorm" version))
              (sha256
               (base32
                "0j14q2bkc14v35s5k03sw44zsssrd2qjljlwzj014qxs74hk0kpz"))))
    (build-system r-build-system)
    (native-inputs
     (list gfortran))
    (home-page "http://mvtnorm.R-forge.R-project.org")
    (synopsis "Package for multivariate normal and t-distributions")
    (description "This package can compute multivariate normal and
t-probabilities, quantiles, random deviates and densities.")
    (license license:gpl2)))

(define-public r-matrixstats
  (package
    (name "r-matrixstats")
    (version "0.61.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "matrixStats" version))
              (sha256
               (base32
                "16pxsba5i51ifbbgvxln2w6nigbwd3290b2ckgzn5bmib7nc1lyv"))))
    (properties `((upstream-name . "matrixStats")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-HOME
           (lambda _ (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list r-r-rsp)) ;used to build vignettes
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
    (version "0.6.2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "viridis" version))
              (sha256
               (base32
                "048kwhbhd49g86cq11fl7vm0whwhjl5gs9xjn040lwcjv78qrdb9"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-ggplot2 r-gridextra r-viridislite))
    (native-inputs
     (list r-knitr)) ; for vignettes
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
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "viridisLite" version))
       (sha256
        (base32
         "0siwr0vq80gs3sbyd542zjb0cl9av5zqdm0fqnym5g6ribf5b6c4"))))
    (properties `((upstream-name . "viridisLite")))
    (build-system r-build-system)
    (home-page "https://github.com/sjmgarnier/viridisLite")
    (synopsis "Default color maps from matplotlib")
    (description
     "This package is a port of the new @code{matplotlib} color maps
(@code{viridis}, @code{magma}, @code{plasma} and @code{inferno}) to R.
matplotlib is a popular plotting library for Python.  These color maps are
designed in such a way that they will analytically be perfectly
perceptually-uniform, both in regular form and also when converted to
black-and-white.  They are also designed to be perceived by readers with the
most common form of color blindness.  This is the @code{lite} version of the
more complete @code{viridis} package.")
    (license license:expat)))

(define-public r-tidyselect
  (package
    (name "r-tidyselect")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "tidyselect" version))
       (sha256
        (base32
         "0ss8mq05mwif64cj8zmrfx0ljgy3n0fvyv7f37782shr8rknmsqq"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-ellipsis r-glue r-purrr r-rlang r-vctrs))
    (native-inputs
     (list r-knitr))
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
    (version "1.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "tidyr" version))
       (sha256
        (base32
         "0h8jfsmn4r7zgfyjmg2qkjd54n0sncq07xjh4mdf2cx4k2z9h30b"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-cpp11
           r-dplyr
           r-ellipsis
           r-magrittr
           r-glue
           r-lifecycle
           r-purrr
           r-rlang
           r-tidyselect
           r-tibble
           r-vctrs))
    (native-inputs
     (list r-knitr))
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
    (version "1.28.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "hexbin" version))
       (sha256
        (base32
         "0bbhs5pwxh474w62sq6gqvih7habancxng9wd4f2rgn6lv9zhhb2"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-lattice))
    (native-inputs
     (list gfortran r-knitr)) ; for vignettes
    (home-page "https://github.com/edzer/hexbin")
    (synopsis "Hexagonal binning routines")
    (description
     "This package provides binning and plotting functions for hexagonal bins.
It uses and relies on grid graphics and formal (S4) classes and methods.")
    (license license:gpl2+)))

(define-public r-purrr
  (package
    (name "r-purrr")
    (version "0.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "purrr" version))
       (sha256
        (base32
         "1cj091rsjdj2xz16qhynyw72gh5cyhznifcfbrbygndfr4xwksr3"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-magrittr r-rlang))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/hadley/purrr")
    (synopsis "Functional programming tools")
    (description
     "This package completes R's functional programming tools with missing
features present in other programming languages.")
    (license license:gpl3+)))

(define-public r-plotly
  (package
    (name "r-plotly")
    (version "4.10.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "plotly" version))
              (sha256
               (base32
                "16iqj7sv49mva6siibsci7iijsbnk7pqvfns9al0k35w9mjmr6dx"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-base64enc
           r-crosstalk
           r-digest
           r-data-table
           r-dplyr
           r-ggplot2
           r-htmltools
           r-htmlwidgets
           r-httr
           r-jsonlite
           r-lazyeval
           r-magrittr
           r-promises
           r-purrr
           r-rcolorbrewer
           r-rlang
           r-scales
           r-tibble
           r-tidyr
           r-vctrs
           r-viridislite))
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
   (home-page "https://www.agner.org/random/")
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
     (list r-rematch r-tibble))
    (home-page "https://github.com/rsheets/cellranger")
    (synopsis "Translate spreadsheet cell ranges to rows and columns")
    (description
     "This package provides helper functions to work with spreadsheets and the
@code{A1:D10} style of cell range specification.")
    (license license:expat)))

(define-public r-googlesheets
  (package
    (name "r-googlesheets")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "googlesheets" version))
       (sha256
        (base32
         "11q07nxys72wkxx9mawmjyf20gvwvrb7h3gpa73h6lgh2vgrwnv8"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-cellranger
           r-dplyr
           r-httr
           r-jsonlite
           r-purrr
           r-readr
           r-stringr
           r-tibble
           r-tidyr
           r-xml2))
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
       ;; Use the ‘Latest version’ link for a stable URI across releases.
       (uri (string-append "https://gforge.inria.fr/frs/download.php/"
                           "latestfile/4531/spams-R-v" version ".tar.gz"))
       (sha256
        (base32
         "13z2293jixf1r9g8dyy856xrhvpjr2ln2n9smn6644126r9hmhkx"))))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "spams") #t))
         ;; Don't tune for the building machine.
         (add-after 'chdir 'no-mtune
           (lambda _
             (substitute* "src/Makevars"
               (("-mtune=native") ""))
             #t)))))
    (propagated-inputs
     (list r-lattice r-matrix))
    (home-page "https://spams-devel.gforge.inria.fr")
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
     (list r-openssl))
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
    (version "4.6-0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Hmisc" version))
       (sha256
        (base32 "1bd1c8sichyijprfpnqj5g72nmgs9jvi0zyw8v4nsg1kn83fj71c"))))
    (properties `((upstream-name . "Hmisc")))
    (build-system r-build-system)
    (native-inputs
     (list gfortran))
    (propagated-inputs
     (list r-base64enc
           r-cluster
           r-data-table
           r-foreign
           r-formula
           r-ggplot2
           r-gridextra
           r-gtable
           r-lattice
           r-latticeextra
           r-htmltable
           r-htmltools
           r-nnet
           r-rpart
           r-survival
           r-viridis))
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
    (version "0.4.32")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "RUnit" version))
       (sha256
        (base32
         "1wc1gwb7yw7phf8b0gkig6c23klya3ax11c6i4s0f049k42r78r3"))))
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

(define-public r-fastcluster
  (package
    (name "r-fastcluster")
    (version "1.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "fastcluster" version))
       (sha256
        (base32
         "009amz7i5yndqw2008fgd3p11n4fsb291k2ypg3pip6dw4lr28hz"))))
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
    (version "1.1-12")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "sfsmisc" version))
       (sha256
        (base32
         "10h76pgzcxv1y5pkmba9607lvjrsyq62sq63s2n7ry7z5151h4lv"))))
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
    (version "3.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "gtools" version))
       (sha256
        (base32
         "0lx97yafrms2mjym8rhcq5586z7s1iqgy8qfm4px3xl1yn5qkc83"))))
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
     (list perl))
    (propagated-inputs
     (list r-gtools))
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
    (version "3.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "gplots" version))
       (sha256
        (base32
         "0f8khaymz383w2ksnk80d4kpnvgmdk37pbycpsnl2vabaz11kbpr"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-catools r-gtools r-kernsmooth))
    (native-inputs
     (list r-knitr))
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
@item balloon plots
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
    (version "1.0-11")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ROCR" version))
       (sha256
        (base32
         "0amvvrkiflmr3qygrsgrsja4gaf2v6r6h6i2bgpsm8r069vmlf2p"))))
    (properties `((upstream-name . "ROCR")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-gplots))
    (native-inputs
     (list r-knitr))
    (home-page "https://rocr.bioinf.mpi-sb.mpg.de/")
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

(define-public r-ztable
  (package
    (name "r-ztable")
    (version "0.2.3")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "ztable" version))
              (sha256
               (base32
                "1ps7ix0hc84s0p0ra6bkjxky3cffs1rvdcagqzj78lbdq22dk5j8"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-flextable
           r-magrittr
           r-officer
           r-rcolorbrewer
           r-rstudioapi
           r-scales
           r-stringr))
    (native-inputs
     (list r-knitr))
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
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "beeswarm" version))
              (sha256
               (base32
                "016mqcbdksialkmap56rprzna9b6cd6896ml9gl2n2h8yjdk7x2i"))))
    (build-system r-build-system)
    (home-page "https://www.cbs.dtu.dk/~eklund/beeswarm/")
    (synopsis "Implementation of bee swarm plots")
    (description
     "This package provides an implementation of bee swarm plots.  The bee
swarm plot is a one-dimensional scatter plot like stripchart, but with
closely-packed, non-overlapping points.")
    (license license:artistic2.0)))

(define-public r-sourcetools
  (package
    (name "r-sourcetools")
    (version "0.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "sourcetools" version))
       (sha256
        (base32
         "1jnjir0q2dj724f1mjm6p5h77yzyx6xcqy9r2g7gmcxkxw349627"))))
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
     (list r-beeswarm r-ggplot2 r-vipor))
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
    (version "4.2.4")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "ggthemes" version))
              (sha256
               (base32
                "0rw5f6axyz1pqn6qx9jwm38hjzn8bqs1nfjkvm96z3xnyn61cdbv"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-ggplot2 r-purrr r-scales r-stringr r-tibble))
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
    (version "1.4.36")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "statmod" version))
              (sha256
               (base32
                "1hkvklcav6gj4zib4g3b4xwsdsbsmnzdblr0v6jaqv227p49gs0l"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/statmod")
    (native-inputs
     (list gfortran))
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
    (version "2.6.1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "RANN" version))
              (sha256
               (base32
                "10kid40w9w7vkz2hpcfkdpxr4afxzms5dzvfwr0sl5xynzgw76dj"))))
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
     (list r-knitr
           r-rmarkdown
           r-dplyr
           r-readr
           r-ggplot2
           r-magrittr
           r-stringr))
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
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "cowplot" version))
       (sha256
        (base32
         "0j7d5vhzdxn1blrsfafx5z8lhq122rp8230hp9czrpsnnhjydp67"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-ggplot2 r-gtable r-rlang r-scales))
    (native-inputs
     (list r-knitr))
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
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "mixtools" version))
       (sha256
        (base32
         "08whic8hmmzi55b7azwj11l2x5r9s5qbyrv7s9jr08156vqkw0zg"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-mass r-kernlab r-segmented r-survival))
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
     (list gfortran))
    (home-page "https://web.stanford.edu/~hastie/Papers/LARS/")
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
    (version "1.2-3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "fastICA" version))
       (sha256
        (base32
         "0l49cmxm1n4dzrv8q3q3mq4drcjgby4f1dmpwfdb8jxn9ij85vz9"))))
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
    (version "4.6-14")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "randomForest" version))
       (sha256
        (base32
         "0kbmm0l42fc2d1rdq0l7k09d34kd87q4lx651ffsic4y84h8kf7l"))))
    (properties `((upstream-name . "randomForest")))
    (build-system r-build-system)
    (home-page "https://www.stat.berkeley.edu/~breiman/RandomForests/")
    (native-inputs
     (list gfortran))
    (synopsis "Breiman and Cutler's random forests for classification and regression")
    (description
"This package provides the Breiman and Cutler's random forests algorithm, based on a
forest of trees using random inputs, for classification and regression.")
    (license license:gpl2+)))

(define-public r-diptest
  (package
    (name "r-diptest")
    (version "0.76-0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "diptest" version))
       (sha256
        (base32
         "10629vwn6r2949jjrmj5slbb5v0vnm3w0v8mrl7ws68m2sxmx2jh"))))
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
    (version "0.2-23")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "modeltools" version))
       (sha256
        (base32
         "1vqh69256h344sbj5affm0kmc77dakrxp6442xfdnfd0y5d8sgkb"))))
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
    (version "2.3-17")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "flexmix" version))
       (sha256
        (base32
         "0jangf89174p0idh9afbicfiln2sz8jxc3bjc6n0j9036dw9n09n"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-lattice r-modeltools r-nnet))
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
    (version "5.4.9")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "mclust" version))
       (sha256
        (base32
         "0s4mva0n04f3pl41msmzhlh7pnk0zbmf06n826smxkw6mz327wb5"))))
    (build-system r-build-system)
    (native-inputs
     (list gfortran r-knitr))
    (home-page "https://www.stat.washington.edu/mclust/")
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
    (version "2.3-2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "prabclus" version))
       (sha256
        (base32
         "0hg4d7y1w18jpgvw10z8833bbbcnlkwiasx0wh6iwa2pnnybq8gl"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-mass r-mclust))
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
    (version "1.0-10")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "DEoptimR" version))
       (sha256
        (base32
         "1m9ag073i7pbxsy74vkj0n4hjn6vmyc4n0k78nmslwwwmjh7nkvp"))))
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
    (version "0.93-9")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "robustbase" version))
       (sha256
        (base32
         "0i9wf4aq8zpn2np6z2rjja21ks9n740dgkiv0qfwdzk3ah3vapyp"))))
    (build-system r-build-system)
    (native-inputs
     (list gfortran))
    (propagated-inputs
     (list r-deoptimr))
    (home-page "http://robustbase.r-forge.r-project.org/")
    (synopsis "Basic robust statistics")
    (description
     "This package analyzes data with robust methods such as
regression methodology including model selections and multivariate statistics.")
    (license license:gpl2+)))

(define-public r-pcapp
  (package
    (name "r-pcapp")
    (version "1.9-74")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "pcaPP" version))
       (sha256
        (base32
         "1dlrz5plb8b89vr3j6sn9jwryyhcwflqkiilxkybbr379m1pp0sh"))))
    (properties `((upstream-name . "pcaPP")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-mvtnorm))
    (home-page "https://cran.r-project.org/web/packages/pcaPP")
    (synopsis "Robust PCA by projection pursuit")
    (description
     "This package provides functions for robust @dfn{principal component
analysis} (PCA) by projection pursuit.")
    (license license:gpl3+)))

(define-public r-rrcov
  (package
    (name "r-rrcov")
    (version "1.6-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rrcov" version))
       (sha256
        (base32
         "142x60zcgz61iw0ps78hh4cmny3ch0hb3lac6z87bhys5c7m0fwz"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-lattice r-mvtnorm r-pcapp r-robustbase))
    (native-inputs
     (list gfortran))
    (home-page "https://cran.r-project.org/web/packages/rrcov")
    (synopsis "Scalable robust estimators with high breakdown Point")
    (description
     "This package provides an implementation of robust location and scatter
estimation and robust multivariate analysis with high breakdown point.")
    (license license:gpl2+)))

(define-public r-fit-models
  (package
    (name "r-fit-models")
    (version "0.64")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "fit.models" version))
       (sha256
        (base32
         "1nk4x2q8cv79zcls61saf627ac0fci6jcrd6lmzk61asm2zhc27p"))))
    (properties `((upstream-name . "fit.models")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-lattice))
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
    (version "0.6-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "robust" version))
       (sha256
        (base32
         "0z8i0qqlsa6vpl457qir3dlsynhjfn2g22436d776vxwyqjx4vs9"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-fit-models r-lattice r-mass r-robustbase r-rrcov))
    (native-inputs
     (list gfortran))
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
    (version "0.1-5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "trimcluster" version))
       (sha256
        (base32
         "12siv8yx8dcavsz8jk96lwscbj257ar8jpaxksl2zb06987g4fcj"))))
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
    (version "2.2-9")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "fpc" version))
       (sha256
        (base32
         "0f7sfmpcycr9y7cy5gasyjm2ardxa62kglqms92mcr68jrp01c19"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-class
           r-cluster
           r-diptest
           r-flexmix
           r-kernlab
           r-mass
           r-mclust
           r-prabclus
           r-robustbase))
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
    (version "1.1-5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "VGAM" version))
       (sha256
        (base32 "1fx123h4x5awzshv0m4jqbbllrdjfpslb2i8gl9phm1y1wahn69h"))))
    (properties `((upstream-name . "VGAM")))
    (build-system r-build-system)
    (native-inputs
     (list gfortran))
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
    (version "1.5-0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "pbapply" version))
       (sha256
        (base32
         "0m8a0ygwl98cs0vcha5gs9f7z8whcplwxhravhs9bfp5hvigxzgg"))))
    (build-system r-build-system)
    (home-page "https://github.com/psolymos/pbapply")
    (synopsis "Adding progress bar to apply functions")
    (description
     "This lightweight package that adds progress bar to vectorized R
functions apply.  The implementation can easily be added to functions where
showing the progress is useful e.g. bootstrap.")
    (license license:gpl2)))

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
     (list r-rcpp))
    (inputs
     (list gfortran))
    (home-page "http://optimizer.r-forge.r-project.org")
    (synopsis "Derivative-free optimization algorithms by quadratic approximation")
    (description
      "This package provides a derivative-free optimization by quadratic approximation
based on an interface to Fortran implementations by M. J. D. Powell.")
    (license license:gpl2)))

(define-public r-rcppeigen
  (package
    (name "r-rcppeigen")
    (version "0.3.3.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "RcppEigen" version))
       (sha256
        (base32
         "0qp9gypar280lm84pc8w0fxw4jgwqygwi3qnl5sl52kpkcj8c14a"))))
    (properties `((upstream-name . "RcppEigen")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-rcpp r-matrix))
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
    (version "1.2.2.2")
    (source
     (origin
       (method url-fetch)
        (uri (cran-uri "ModelMetrics" version))
        (sha256
         (base32
          "0mrlsw4c5y1vdsqynxr2dcvmhh5h37pnd71jw5a5djpbda9g21jy"))))
    (properties `((upstream-name . "ModelMetrics")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-rcpp r-data-table))
    (home-page "https://cran.r-project.org/web/packages/ModelMetrics")
    (synopsis "Rapid calculation of model metrics")
    (description
     "Written in C++ using @code{Rcpp}, this package provides a collection of
metrics for evaluating models.")
    (license license:gpl2+)))

(define-public r-matrixmodels
  (package
    (name "r-matrixmodels")
    (version "0.5-0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "MatrixModels" version))
       (sha256
        (base32
         "1va6i6bpkq71rzl9i4xgyc5x7qcksa3nfzihlaggf6aj30dayzx8"))))
    (properties `((upstream-name . "MatrixModels")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-matrix))
    (home-page "https://cran.r-project.org/web/packages/MatrixModels")
    (synopsis "Modelling with sparse and dense matrices")
    (description
     "This package models with sparse and dense matrix matrices,
using modular prediction and response module classes.")
    (license license:gpl2+)))

(define-public r-quantreg
  (package
    (name "r-quantreg")
    (version "5.87")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "quantreg" version))
       (sha256
        (base32 "13m3znkrrkf19s3lz6984lzrznng4nma38yy17pqjfzmwl4yzmws"))))
    (build-system r-build-system)
    (native-inputs
     (list gfortran))
    (propagated-inputs
     (list r-matrix r-matrixmodels r-sparsem))
    (home-page "https://www.r-project.org")
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
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "nloptr" version))
       (sha256
        (base32
         "16k124ll14p3ygny6i1zd7yza83bv2sk87x922n1bfn9rx4k3jk5"))))
    (build-system r-build-system)
    (native-inputs
     (list r-knitr ; for building vignettes
           pkg-config gfortran))
    (inputs
     (list nlopt))
    (propagated-inputs (list r-testthat))
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
    (version "1.1-27.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "lme4" version))
       (sha256
        (base32
         "1i9f8kl2qj48j75dbn1vmwxz1xlbikdr7v2yq542w6dq74z8gyi5"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-boot
           r-lattice
           r-mass
           r-matrix
           r-minqa
           r-nloptr
           r-nlme
           r-rcpp
           r-rcppeigen))
    (native-inputs
     (list r-knitr))
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
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "pbkrtest" version))
       (sha256
        (base32
         "1if7msv9i7jhg1as1f7m81a95dnwhwj3yfs24bqr0f6r0ch4b8xj"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-broom
           r-dplyr
           r-lme4
           r-magrittr
           r-mass
           r-matrix
           r-numderiv))
    (native-inputs
     (list r-knitr))
    (home-page "https://people.math.aau.dk/~sorenh/software/pbkrtest/")
    (synopsis "Methods for linear mixed model comparison")
    (description
     "This package implements a parametric bootstrap test and a Kenward Roger
modification of F-tests for linear mixed effects models and a parametric
bootstrap test for generalized linear mixed models.")
    (license license:gpl2+)))

(define-public r-cardata
  (package
    (name "r-cardata")
    (version "3.0-5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "carData" version))
       (sha256
        (base32 "06j52rpbqi6fj7chfjwjbwbr6slrbb7i3aygv66gnfiyndcp3rq2"))))
    (properties `((upstream-name . "carData")))
    (build-system r-build-system)
    (home-page "https://r-forge.r-project.org/projects/car/")
    (synopsis "Data Sets for the book Companion to Applied Regression")
    (description
     "This package provides datasets to accompany J. Fox and S. Weisberg, An R
Companion to Applied Regression, Third Edition, Sage.")
    (license license:gpl2+)))

(define-public r-car
  (package
    (name "r-car")
    (version "3.0-12")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "car" version))
       (sha256
        (base32 "00kk8l71508f73kyn21is1mx6kjbrzdq3ls94c5ajhiqmvpsd6dq"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-abind
           r-cardata
           r-lme4
           r-maptools
           r-mass
           r-mgcv
           r-nlme
           r-nnet
           r-pbkrtest
           r-quantreg))
    (native-inputs
     (list r-knitr))
    (home-page "https://r-forge.r-project.org/projects/car/")
    (synopsis "Companion to applied regression")
    (description
      "This package provides functions and datasets from book Companion
to Applied regression, Second Edition, Sage, 2011.")
    (license license:gpl2+)))

(define-public r-rcppprogress
  (package
    (name "r-rcppprogress")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "RcppProgress" version))
       (sha256
        (base32
         "0j2b37gwjpgmrnr00srdzm751hzlpsjb54ph63xxmcdfnwhlnqmi"))))
    (properties `((upstream-name . "RcppProgress")))
    (build-system r-build-system)
    (home-page "https://github.com/kforner/rcpp_progress")
    (synopsis "Interruptible progress bar for C++ in R packages")
    (description
     "This package displays a progress bar in the R console for long running
computations taking place in C++ code, and support for interrupting those computations
even in multithreaded code, typically using OpenMP.")
    (license license:gpl3+)))

(define-public r-tmvnsim
  (package
    (name "r-tmvnsim")
    (version "1.0-2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "tmvnsim" version))
       (sha256
        (base32
         "03xsvsg9bqvgl98ywid3h91mmlhax5s6wvmypp3hq91vmc5kvxlp"))))
    (properties `((upstream-name . "tmvnsim")))
    (build-system r-build-system)
    (native-inputs (list gfortran))
    (home-page "https://www.r-project.org")
    (synopsis "Truncated multivariate normal simulation")
    (description
     "This package implements importance sampling from the truncated
multivariate normal using the @dfn{Geweke-Hajivassiliou-Keane} (GHK)
simulator.  Unlike Gibbs sampling which can get stuck in one truncation
sub-region depending on initial values, this package allows truncation based
on disjoint regions that are created by truncation of absolute values.  The
GHK algorithm uses simple Cholesky transformation followed by recursive
simulation of univariate truncated normals hence there are also no convergence
issues.  Importance sample is returned along with sampling weights, based on
which, one can calculate integrals over truncated regions for multivariate
normals.")
    (license license:gpl2)))

(define-public r-mnormt
  (package
    (name "r-mnormt")
    (version "2.0.2")
    (source
     (origin
       (method url-fetch)
        (uri (cran-uri "mnormt" version))
        (sha256
          (base32
           "0d7ka4l4fl1wi77r2r81dc91n6cbj2xrb3lsizz5y0ziscva0sjw"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-tmvnsim))
    (native-inputs
     (list gfortran))
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
    (version "2016.8-1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "numDeriv" version))
       (sha256
        (base32
         "0idk02pqkziik932bd8k72d1q775g1is3m4bc861pcxfz6gx3i6q"))))
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
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "sn" version))
       (sha256
        (base32
         "1z58kwvq8igjm3biyyjdlhs2qq8hfjq5d9x0lzy80yk5hw08kcw6"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-mnormt r-numderiv r-quantreg))
    (home-page "http://azzalini.stat.unipd.it/SN")
    (synopsis "The skew-normal and skew-t distributions")
    (description
     "This package provides functionalities to build and manipulate
probability distributions of the skew-normal family and some related
ones, notably the skew-t family, and provides related statistical
methods for data fitting and diagnostics, in the univariate and the
multivariate case.")
    (license license:gpl2+)))

(define-public r-tclust
  (package
    (name "r-tclust")
    (version "1.4-2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "tclust" version))
       (sha256
        (base32
         "1nb5iky4kc832627jf28lzj19wrvgwg5da6fyl3kyf0npmyx1p4m"))))
    (build-system r-build-system)
    ;; These are all suggested packages, not build dependencies.
    (propagated-inputs
     (list r-cluster r-mclust r-mvtnorm r-sn))
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
    (version "0.13.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ranger" version))
       (sha256
        (base32
         "02idcc6zbdz4wsi1mcwh7qyhmlbwvnzxwkdvvppxw7n2rh54z4v0"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-rcpp r-matrix r-rcppeigen))
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
    (version "1.5-14")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Cairo" version))
       (sha256
        (base32
         "0ysc40smkjwk8fmp5dk0q3nbp9xkxzz9pd4z5zkp2mrvrvx52xq6"))))
    (properties `((upstream-name . "Cairo")))
    (build-system r-build-system)
    (inputs
     (list cairo libxt zlib))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.rforge.net/Cairo/")
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
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "lubridate" version))
       (sha256
        (base32
         "199b00cql07gf3rf4hh5ba34amnk0ai40zhx73dq1mpkn7ynxml7"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-generics r-cpp11))
    (native-inputs
     (list r-knitr))
    (home-page "https://cran.r-project.org/web/packages/lubridate/")
    (synopsis "Make dealing with dates a little easier")
    (description
     "This package provides functions to work with date-times and time-spans:
fast and user friendly parsing of date-time data, extraction and updating of
components of a date-time (years, months, days, hours, minutes, and seconds),
algebraic manipulation on date-time and time-span objects.  The @code{lubridate}
package has a consistent and memorable syntax that makes working with dates
easy and fun.")
    (license license:gpl2)))

(define-public r-fdrtool
  (package
    (name "r-fdrtool")
    (version "1.2.17")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "fdrtool" version))
       (sha256
        (base32
         "1pf554vb902vavgqc4c0kgghywbgcvr3lkkr414bxngavcd60lil"))))
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
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "forcats" version))
       (sha256
        (base32
         "12d2nv2w15085jscgidmjdnr1ryymciflcmal6ldmgp2fkl9dyy4"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-ellipsis r-magrittr r-tibble r-rlang))
    (native-inputs
     (list r-knitr))
    (home-page "https://forcats.tidyverse.org")
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
         (file-name (string-append name "-" version "-checkout"))
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
       (list r-rcpp))
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
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "0xy6c7s7mn1yx191154bwbv1bl424bnvc80syqpl1vdl28ba46rj"))))
      (build-system r-build-system)
      (propagated-inputs
       (list r-yaml))
      (home-page "https://bitbucket.org/tanaylab/tgconfig/")
      (synopsis "Infrastructure for managing package parameters")
      (description
       "The goal of tgconfig is to provide infrastructure for managing package
parameters.")
      (license license:gpl3))))

(define-public r-catterplots
  (let ((commit "ae17cd5e49ddda4ecfe0eba8a4c21df8c88e72c4")
        (revision "3"))
    (package
      (name "r-catterplots")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Gibbsdavidl/CatterPlots")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0qa8liylffpxgdg8xcgjar5dsvczqhn3akd4w35113hnyg1m4xyg"))))
      (build-system r-build-system)
      (propagated-inputs
       (list r-png))
      (home-page "https://github.com/Gibbsdavidl/CatterPlots")
      (synopsis "Scatter plots with cat shaped points")
      (description "Did you ever wish you could make scatter plots with cat
shaped points?  Now you can!")
      (license license:asl2.0))))

(define-public r-colorout
  (package
    (name "r-colorout")
    (version "1.2-2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jalvesaq/colorout")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rsx69wjpa73c6x2hacvvvbzdzxn7wg06gizf97kasjdlb7azmp3"))))
    (build-system r-build-system)
    (home-page "https://github.com/jalvesaq/colorout")
    (synopsis "Colorize output in the R REPL")
    (description "@code{colorout} is an R package that colorizes R output when
running in terminal emulator.

R STDOUT is parsed and numbers, negative numbers, dates in the standard
format, strings, and R constants are identified and wrapped by special ANSI
scape codes that are interpreted by terminal emulators as commands to colorize
the output.  R STDERR is also parsed to identify the expressions warning and
error and their translations to many languages.  If these expressions are
found, the output is colorized accordingly; otherwise, it is colorized as
STDERROR (blue, by default).

You can customize the colors according to your taste, guided by the color
table made by the command @code{show256Colors()}.  You can also set the colors
to any arbitrary string.  In this case, it is up to you to set valid values.")
    (license license:gpl3+)))

(define-public r-nnlm
  (let ((commit "4574bca9456fe2285b668b4c22a908cffbad10a0")
        (revision "1"))
    (package
      (name "r-nnlm")
      (version (git-version "0.4.4" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/linxihui/NNLM")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1qzvav4ch0j1k7jcqzk9cvl8dx79fapmvdzmzzyl8smscybhfgba"))))
      (properties `((upstream-name . "NNLM")))
      (build-system r-build-system)
      (propagated-inputs (list r-rcpp r-rcpparmadillo r-rcppprogress))
      (native-inputs (list r-knitr))
      (home-page "https://github.com/linxihui/NNLM")
      (synopsis "Fast and versatile non-negative matrix factorization")
      (description
       "This is a package for @dfn{Non-Negative Linear Models} (NNLM).  It
implements fast sequential coordinate descent algorithms for non-negative
linear regression and @dfn{non-negative matrix factorization} (NMF).  It
supports mean square error and Kullback-Leibler divergence loss.  Many other
features are also implemented, including missing value imputation, domain
knowledge integration, designable W and H matrices and multiple forms of
regularizations.")
      (license license:bsd-2))))

(define-public r-swne
  (let ((commit "05fc3ee4e09b2c34d99c69d3b97cece4c1c34143")
        (revision "1"))
    (package
      (name "r-swne")
      (version (git-version "0.6.20" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/yanwu2014/swne")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0crlpg9kclbv4v8250p3086a3lk6f2hcq79psqkdylc1qnrx3kfx"))))
      (properties `((upstream-name . "swne")))
      (build-system r-build-system)
      (propagated-inputs
       (list r-fnn
             r-ggplot2
             r-ggrepel
             r-hash
             r-ica
             r-igraph
             r-irlba
             r-jsonlite
             r-liger
             r-mass
             r-matrix
             r-mgcv
             r-nnlm ;not listed but required at install time
             r-plyr
             r-proxy
             r-rcolorbrewer
             r-rcpp
             r-rcpparmadillo
             r-rcppeigen
             r-reshape
             r-reshape2
             r-snow
             r-umap
             r-usedist))
      (home-page "https://github.com/yanwu2014/swne")
      (synopsis "Visualize high dimensional datasets")
      (description
       "@dfn{Similarity Weighted Nonnegative Embedding} (SWNE) is a method for
visualizing high dimensional datasets.  SWNE uses Nonnegative Matrix
Factorization to decompose datasets into latent factors, projects those
factors onto 2 dimensions, and embeds samples and key features in 2 dimensions
relative to the factors.  SWNE can capture both the local and global dataset
structure, and allows relevant features to be embedded directly onto the
visualization, facilitating interpretation of the data.")
      (license license:gpl2))))

(define-public python-rpy2
  (package
    (name "python-rpy2")
    (version "3.4.5")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "rpy2" version))
        (sha256
         (base32
          "1cysswxr5glrdblyl2zsmywcj7xhxn3wmyihxinrz9gm8gmaacax"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let ((cwd (getcwd)))
               (setenv "TZ" "UTC"))
             ;; test_vector_complex has issues when run in our environment.
             (invoke "pytest" "-v" "rpy2/tests/"
                     "-k" "not test_vector_complex"))))))
    (propagated-inputs
     (list python-cffi
           python-six
           python-jinja2
           python-numpy
           python-pandas
           python-pytz
           python-ipython
           python-tzlocal))
    (inputs
     (list readline
           icu4c
           pcre
           r-minimal
           r-survival
           r-ggplot2
           r-rsqlite
           r-dplyr
           r-dbplyr
           python-numpy))
    (native-inputs
     (list zlib python-pytest))
    (home-page "https://rpy2.github.io")
    (synopsis "Python interface to the R language")
    (description "rpy2 is a redesign and rewrite of rpy.  It is providing a
low-level interface to R from Python, a proposed high-level interface,
including wrappers to graphical libraries, as well as R-like structures and
functions.")
    ;; Any of these licenses can be picked for the R interface.  The whole
    ;; project is released under GPLv2+ according to the license declaration
    ;; in "setup.py".
    (license (list license:mpl2.0 license:gpl2+ license:lgpl2.1+))))

(define-public java-jdistlib
  (package
    (name "java-jdistlib")
    (version "0.4.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/jdistlib/jdistlib-"
                                  version "-src.jar"))
              (sha256
               (base32
                "1pkj8aahw9ydr1isbaqrkd05nvq98ik5jwwhf3yf3rky3z869v11"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jdistlib.jar"
       #:jdk ,icedtea-8
       #:tests? #f ; no dedicated test directory
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-broken-encoding
           (lambda _
             (with-fluids ((%default-port-encoding "ISO-8859-1"))
               (substitute* "src/jdistlib/Beta.java"
                 (("Scheff.+-Tukey") "Scheffe-Tukey")))
             #t)))))
    (propagated-inputs
     (list java-jtransforms))
    (native-inputs
     (list java-junit))
    (home-page "http://jdistlib.sourceforge.net/")
    (synopsis "Java library of statistical distributions")
    (description "JDistlib is the Java Statistical Distribution Library, a
Java package that provides routines for various statistical distributions.")
    ;; The files that were translated from R code are under GPLv2+; some files
    ;; are under the GPLv3, which is a mistake.  The author confirmed in an
    ;; email that this whole project should be under GPLv2+.
    (license license:gpl2+)))

(define-public emacs-ess
  ;; Latest release is old.  This is not the latest commit either due to bug
  ;; reported here: <https://github.com/emacs-ess/ESS/issues/987>.
  (let ((commit "24da603184ce39246611dd5b8602e769d7ebd5bf")
        (version "18.10.2")
        (revision "0"))
    (package
      (name "emacs-ess")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/emacs-ess/ESS")
               (commit commit)))
         (sha256
          (base32 "0j98lv07nzwzd54d4dgcfz01wy5gj48m0mnirxzh5r45ik2myh1r"))
         (file-name (git-file-name name version))
         (modules '((guix build utils)))
         (snippet
          '(begin
             ;; Stop ESS from trying to bundle an external julia-mode.el.
             (substitute* "lisp/Makefile"
               ((" \\$\\(JULIAS)") "")
               (("\ttest.*julia-mode.*\\.el") ""))
             ;; Only build docs in info format.
             (substitute* "doc/Makefile"
               (("all  : info text")
                "all  : info")
               (("install: install-info install-other-docs")
                "install: install-info"))
             ;; Stop install-info from trying to update the info directory.
             (substitute* "doc/Makefile"
               ((".*/dir.*") ""))
             ;; Fix r-help-mode test.
             (substitute* "test/ess-test-r.el"
               (("\\(equal ess-help-object \"plot.default\")") "t"))
             ;; Avoid generating ess-autoloads.el twice.
             (substitute* "Makefile"
               (("all: lisp doc etc autoloads")
                "all: lisp doc etc"))
             ;; Install to correct directories.
             (substitute* "Makefile"
               (("mkdir -p \\$\\(ESSDESTDIR)")
                "$(MAKE) -C lisp install; $(MAKE) -C doc install")
               (("\\$\\(INSTALL) -R \\./\\* \\$\\(ESSDESTDIR)/")
                "$(MAKE) -C etc install"))
             #t))))
      (build-system gnu-build-system)
      (arguments
       (let ((base-directory "/share/emacs/site-lisp"))
         `(#:make-flags (list (string-append "PREFIX=" %output)
                              (string-append "ETCDIR=" %output
                                             ,base-directory "/etc")
                              (string-append "LISPDIR=" %output
                                             ,base-directory)
                              (string-append "INFODIR=" %output
                                             "/share/info"))
           #:phases
           (modify-phases %standard-phases
             (delete 'configure)
             (replace 'check
               (lambda _ (invoke "make" "test")))))))
      (native-inputs
       (list perl r-roxygen2 texinfo))
      (inputs
       `(("emacs" ,emacs-minimal)
         ("r-minimal" ,r-minimal)))
      (propagated-inputs
       (list emacs-julia-mode))
      (home-page "https://ess.r-project.org/")
      (synopsis "Emacs mode for statistical analysis programs")
      (description
       "Emacs Speaks Statistics (ESS) is an add-on package for GNU Emacs.  It
is designed to support editing of scripts and interaction with various
statistical analysis programs such as R, Julia, and JAGS.")
      (license license:gpl3+))))

(define-public emacs-poly-r
  (package
    (name "emacs-poly-r")
    (version "0.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/polymode/poly-R")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0a4wx73jkngw5nbq1fa4jfhba6bsmyn6vnsf887x3xhb5v3ykhsg"))))
    (build-system emacs-build-system)
    (propagated-inputs
     (list emacs-ess emacs-poly-noweb emacs-polymode-markdown
           emacs-polymode))
    (properties '((upstream-name . "poly-R")))
    (home-page "https://github.com/polymode/poly-markdown")
    (synopsis "Polymodes for the R language")
    (description
     "This package provides a number of polymodes for working with mixed R
files, including Rmarkdown files.")
    (license license:gpl3+)))

(define-public readstat
  (package
    (name "readstat")
    (version "1.1.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/WizardMac/ReadStat")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00sdmaq0qzp6kyv53fpfi6jf3iv4pd0ap0gmw3mbfip52bbnl55w"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake gnu-gettext libtool))
    (inputs
     (list zlib))                 ; libz
    (synopsis "Convert SAS, Stata, and SPSS files")
    (description "Command-line tool and C library for reading files from
popular stats packages like SAS, Stata and SPSS.")
    (home-page "https://github.com/WizardMac/ReadStat")
    (license license:expat)))

(define-public r-quantpsyc
  (package
    (name "r-quantpsyc")
    (version "1.5")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "QuantPsyc" version))
        (sha256
          (base32
            "1i9bh88r8zxndzjqsj14qw64gnvm5a9kvhjhzk3qsrvl3qzjgh93"))))
    (properties `((upstream-name . "QuantPsyc")))
    (build-system r-build-system)
    (propagated-inputs
      (list r-boot r-mass))
    (home-page "https://cran.r-project.org/web/packages/QuantPsyc/")
    (synopsis "Quantitative Psychology Tools")
    (description
      "Contains functions useful for data screening, testing moderation,
mediation and estimating power.")
    (license license:gpl2+)))

(define-public r-clubsandwich
  (package
    (name "r-clubsandwich")
    (version "0.5.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "clubSandwich" version))
       (sha256
        (base32
         "1ifnc2rg5agf7lpilmm27iq0af7ghfhkvwykajhvykgf6h4clvhf"))))
    (properties `((upstream-name . "clubSandwich")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-sandwich))
    (native-inputs (list r-knitr))
    (home-page "https://github.com/jepusto/clubSandwich")
    (synopsis "Cluster-Robust (Sandwich) Variance Estimators with Small-Sample
Corrections")
    (description
     "Provides several cluster-robust variance estimators (i.e.,
sandwich estimators) for ordinary and weighted least squares linear regression
models, including the bias-reduced linearization estimator introduced by Bell
and McCaffrey (2002)
@url{http://www.statcan.gc.ca/pub/12-001-x/2002002/article/9058-eng.pdf} and
developed further by Pustejovsky and Tipton (2017)
@url{doi:10.1080/07350015.2016.1247004}.  The package includes functions for estimating
the variance- covariance matrix and for testing single- and multiple-
contrast hypotheses based on Wald test statistics.  Tests of single regression
coefficients use Satterthwaite or saddle-point corrections.  Tests of multiple-
contrast hypotheses use an approximation to Hotelling's T-squared distribution.
Methods are provided for a variety of fitted models, including @code{lm()} and
@code{mlm} objects, @code{glm()}, ivreg (from package @code{AER}), @code{plm()}
(from package @code{plm}), @code{gls()} and @code{lme()} (from @code{nlme}),
@code{robu()} (from @code{robumeta}), and @code{rma.uni()} and @code{rma.mv()}
(from @code{metafor}).")
    (license license:gpl3)))

(define-public r-puniform
  (package
    (name "r-puniform")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "puniform" version))
       (sha256
        (base32
         "0v2977y9cwjx74xk0ig745g09wn7nrcsrg4f6v315sglsm18iaa8"))))
    (properties `((upstream-name . "puniform")))
    (build-system r-build-system)
    (propagated-inputs
     (list r-adgoftest r-metafor r-rcpp r-rcpparmadillo))
    (home-page
     "https://github.com/RobbievanAert/puniform")
    (synopsis
     "Meta-Analysis Methods Correcting for Publication Bias")
    (description
     "This package provides meta-analysis methods that correct for publication
bias and outcome reporting bias.  Four methods and a visual tool are currently
included in the package.

@enumerate
@item The p-uniform method as described in van Assen, van Aert, and Wicherts
(2015) @url{doi:10.1037/met0000025} can be used for estimating the average
effect size, testing the null hypothesis of no effect, and testing for
publication bias using only the statistically significant effect sizes of
primary studies.

@item The p-uniform* method as described in van Aert and van Assen (2019)
@url{doi:10.31222/osf.io/zqjr9}.  This method is an extension of the p-uniform
method that allows for estimation of the average effect size and the
between-study variance in a meta-analysis, and uses both the statistically
significant and nonsignificant effect sizes.

@item The hybrid method as described in van Aert and van Assen (2017)
@url{doi:10.3758/s13428-017-0967-6}.  The hybrid method is a meta-analysis
method for combining an original study and replication and while taking into
account statistical significance of the  original study.  The p-uniform and
hybrid method are based on the statistical theory that the distribution of
p-values is uniform conditional on the population effect size.

@item
The fourth method in the package is the Snapshot Bayesian Hybrid Meta-Analysis
Method as described in van Aert and van Assen (2018)
@url{doi:10.1371/journal.pone.0175302}.  This method computes posterior
probabilities for four true effect sizes (no, small, medium, and large) based
on an original study and replication while taking into account publication bias
in the original study.  The method can also be used for computing the required
sample size of the replication akin to power analysis in null hypothesis
significance testing.
@end enumerate

The meta-plot is a visual tool for meta-analysis that
provides information on the primary studies in the meta-analysis, the results
of the meta-analysis, and characteristics of the research on the effect under
study (van Assen and others, 2020).

Helper functions to apply the Correcting for Outcome Reporting Bias (CORB)
method to correct for outcome reporting bias in a meta-analysis (van Aert &
Wicherts, 2020).")
    (license license:gpl2+)))

(define-public r-esc
  (package
    (name "r-esc")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "esc" version))
        (sha256
          (base32
            "0gns7gz55p6pha05413r3hlvd7f2v1amxkg13d197mab1ypqic7q"))))
    (properties `((upstream-name . "esc")))
    (build-system r-build-system)
    (home-page "https://strengejacke.github.io/esc/")
    (synopsis
      "Effect Size Computation for Meta Analysis")
    (description
      "Implementation of the web-based
@url{http://www.campbellcollaboration.org/escalc/html/EffectSizeCalculator-Home.php,'Practical
Meta-Analysis Effect Size Calculator'} from David B. Wilson in R.  Based on the
input, the effect size can be returned as standardized mean difference, Cohen's
f, Hedges' g, Pearson's r or Fisher's transformation z, odds ratio or log odds,
or eta squared effect size.")
    (license license:gpl3)))

(define-public r-kknn
  (package
    (name "r-kknn")
    (version "1.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "kknn" version))
        (sha256
          (base32
            "1nzkg3dxaiqp87p56wm895qx5xn86hv5hjr73qvl1yiaxiq0x112"))))
    (properties `((upstream-name . "kknn")))
    (build-system r-build-system)
    (propagated-inputs
      (list r-igraph r-matrix))
    (home-page "https://github.com/KlausVigo/kknn")
    (synopsis "Weighted k-Nearest Neighbors")
    (description
      "Weighted k-Nearest Neighbors for Classification, Regression and Clustering.")
    (license license:gpl2+)))

(define-public r-logspline
  (package
    (name "r-logspline")
    (version "2.1.16")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "logspline" version))
        (sha256
          (base32
            "12hkdi77vkic05p2vhap025xdcg1n53ywm239v18713pihdlj63l"))))
    (properties `((upstream-name . "logspline")))
    (build-system r-build-system)
    (native-inputs (list gfortran))
    (home-page
      "https://cran.r-project.org/web/packages/logspline/")
    (synopsis
      "Routines for Logspline Density Estimation")
    (description
      "Contains routines for logspline density estimation.  The function
@code{oldlogspline()} uses the same algorithm as the logspline package version
1.0.x; i.e.  the Kooperberg and Stone (1992) algorithm (with an improved
interface).  The recommended routine @code{logspline()} uses an algorithm from
@url{doi:10.1214/aos/1031594728,Stone et al (1997)}.")
    (license license:asl2.0)))

(define-public r-effects
  (package
    (name "r-effects")
    (version "4.2-1")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "effects" version))
        (sha256
          (base32
            "0h3n2kd43fddy29hlv1nckw3wglykwrgk1hp8khb0pnm2i12b5sk"))))
    (properties `((upstream-name . "effects")))
    (build-system r-build-system)
    (propagated-inputs
      (list r-cardata
            r-colorspace
            r-estimability
            r-insight
            r-lattice
            r-lme4
            r-nnet
            r-survey))
    (native-inputs
      (list r-knitr r-car))
    (home-page "https://www.r-project.org")
    (synopsis
      "Effect Displays for Linear, Generalized Linear, and Other Models")
    (description
      "Graphical and tabular effect displays, e.g., of interactions, for
various statistical models with linear predictors.")
    (license license:gpl2+)))

(define-public r-bayesfactor
  (package
    (name "r-bayesfactor")
    (version "0.9.12-4.3")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "BayesFactor" version))
        (sha256
          (base32
            "171hbr445v8mq4hqi1k8q3h8zqsccwjv0a2mj98h9b77667ziyfw"))))
    (properties `((upstream-name . "BayesFactor")))
    (build-system r-build-system)
    (propagated-inputs
      (list r-coda
            r-gtools
            r-hypergeo
            r-matrix
            r-matrixmodels
            r-mvtnorm
            r-pbapply
            r-rcpp
            r-rcppeigen
            r-stringr))
    (native-inputs (list r-knitr))
    (home-page
      "https://richarddmorey.github.io/BayesFactor/")
    (synopsis
      "Computation of Bayes Factors for Common Designs")
    (description
      "This package provides a suite of functions for computing various Bayes
factors for simple designs, including contingency tables, one- and two-sample
designs, one-way designs, general ANOVA designs, and linear regression.")
    (license license:gpl2)))

(define-public r-norm
  (package
    (name "r-norm")
    (version "1.0-9.5")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "norm" version))
              (sha256
               (base32
                "01j1h412yfjx5r4dd0w8rhlf55997spgb6zd6pawy19rgw0byp1h"))))
    (build-system r-build-system)
    (native-inputs
     (list gfortran))
    (home-page "https://cran.r-project.org/web/packages/norm/")
    (synopsis "Analysis of multivariate normal datasets with missing values")
    (description "Multiple imputation of multivariate continuous data under a
normal model.")
    ;; Custom license, see https://cran.r-project.org/web/packages/norm/LICENSE.
    (license (license:non-copyleft "file://LICENSE"))))

(define-public r-naniar
  (package
    (name "r-naniar")
    (version "0.6.1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "naniar" version))
              (sha256
               (base32
                "0l3l2x85v3srilww483kpgp4zlwixyml257b0cqly8kcpwawlinm"))))
    (build-system r-build-system)
    (propagated-inputs
     (list r-dplyr
           r-norm
           r-forcats
           r-ggplot2
           r-glue
           r-magrittr
           r-purrr
           r-rlang
           r-tibble
           r-tidyr
           r-upsetr
           r-viridis
           r-visdat))
    (native-inputs
     (list r-knitr))
    (home-page "https://github.com/njtierney/naniar")
    (synopsis
     "Data structures, summaries, and visualisations for missing data")
    (description
     "Missing values are ubiquitous in data and need to be explored and
handled in the initial stages of analysis.  The package provides data structures
and functions that facilitate the plotting of missing values and examination of
imputations.  This allows missing data dependencies to be explored with minimal
deviation from the common work patterns of @code{ggplot2} and tidy data.")
    (license license:expat)))

(define-public r-glinternet
  (package
    (name "r-glinternet")
    (version "1.0.12")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "glinternet" version))
       (sha256
        (base32
         "0aphdscj0s6kqxlkgqkw2d6snrylz6hiis6307pl8ldh7q5cvm33"))))
    (build-system r-build-system)
    (home-page "http://web.stanford.edu/~hastie/Papers/glinternet_jcgs.pdf")
    (synopsis "Learning interactions via hierarchical group-lasso regularization")
    (description "Group-Lasso INTERaction-NET.  Fits linear pairwise-interaction
models that satisfy strong hierarchy: if an interaction coefficient is estimated
to be nonzero, then its two associated main effects also have nonzero estimated
coefficients.  Accommodates categorical variables (factors) with arbitrary
numbers of levels, continuous variables, and combinations thereof.  Implements
the machinery described in the paper \"Learning interactions via hierarchical
group-lasso regularization\" (JCGS 2015, Volume 24, Issue 3).
Michael Lim & Trevor Hastie (2015)")
    (license license:gpl2)))

(define-public r-datasaurus
  (package
    (name "r-datasaurus")
    (version "0.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "datasauRus" version))
        (sha256
          (base32
            "1w1yhwwrmh95bklacz44wjwynxd8cj3z8b9zvsnzmk18m5a4k0fl"))))
    (properties `((upstream-name . "datasauRus")))
    (build-system r-build-system)
    (native-inputs (list r-knitr))
    (home-page
      "https://github.com/lockedata/datasauRus")
    (synopsis "Datasets from the Datasaurus Dozen")
    (description
     "The Datasaurus Dozen is a set of datasets with the same summary
statistics.  They retain the same summary statistics despite having radically
different distributions.  The datasets represent a larger and quirkier object
lesson that is typically taught via Anscombe's Quartet (available in the
'datasets' package).  Anscombe's Quartet contains four very different
distributions with the same summary statistics and as such highlights the value
of visualisation in understanding data, over and above summary statistics.  As
well as being an engaging variant on the Quartet, the data is generated in a
novel way.  The simulated annealing process used to derive datasets from the
original Datasaurus is detailed in \"Same Stats, Different Graphs: Generating
Datasets with Varied Appearance and Identical Statistics through Simulated
Annealing\" @url{doi:10.1145/3025453.3025912}.")
    (license license:expat)))

(define-public r-lmom
  (package
    (name "r-lmom")
    (version "2.8")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "lmom" version))
        (sha256
          (base32 "1lnj41gynaar5isyijg5nbll64qdxa12dnqvz7lxhaclqcjskqna"))))
    (properties `((upstream-name . "lmom")))
    (build-system r-build-system)
    (native-inputs (list gfortran))
    (home-page "https://cran.r-project.org/package=lmom")
    (synopsis "L-Moments for R")
    (description
      "This package provides functions related to L-moments: computation
of L-moments and trimmed L-moments of distributions and data samples;
parameter estimation; L-moment ratio diagram; plot vs.  quantiles of an
extreme-value distribution.")
    (license license:cpl1.0)))

(define-public r-gld
  (package
    (name "r-gld")
    (version "2.6.4")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "gld" version))
        (sha256
          (base32 "08rkv7g6ci969vh9fbyg3lrpfmjhbiwzdhcjs9j6s9bf3gpdmdhd"))))
    (properties `((upstream-name . "gld")))
    (build-system r-build-system)
    (propagated-inputs (list r-e1071 r-lmom))
    (home-page
      "https://cran.r-project.org/package=gld")
    (synopsis
      "Estimation and Use of the Generalised (Tukey) Lambda Distribution")
    (description
      "The generalised lambda distribution, or Tukey lambda distribution,
provides a wide variety of shapes with one functional form.  This package
provides random numbers, quantiles, probabilities, densities and
density quantiles for four different types of the distribution, the FKML
(Freimer et al 1988), RS (Ramberg and Schmeiser 1974), GPD (van Staden
and Loots 2009) and FM5 - see documentation for details.  It provides the
density function, distribution function, and Quantile-Quantile plots.
It implements a variety of estimation methods for the distribution,
including diagnostic plots.  Estimation methods include the starship (all
4 types), method of L-Moments for the GPD and FKML types, and a number
of methods for only the FKML type.  These include maximum likelihood,
maximum product of spacings, Titterington's method, Moments, Trimmed
L-Moments and Distributional Least Absolutes.")
    (license license:gpl2+)))

(define-public r-exact
  (package
    (name "r-exact")
    (version "3.1")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "Exact" version))
        (sha256
          (base32 "1kz3hw11sl142mai4ly2r85rwdbjz8q7b38ybmjym6r0n5qmqzyn"))))
    (properties `((upstream-name . "Exact")))
    (build-system r-build-system)
    (propagated-inputs (list r-rootsolve))
    (home-page "https://cran.r-project.org/package=Exact")
    (synopsis "Unconditional Exact Test")
    (description
      "Performs unconditional exact tests and power calculations for 2x2
contingency tables.  For comparing two independent proportions, performs
@url{doi:10.1038/156177a0, Barnard's test (1945)} using the original CSM
test (@url{doi:10.1093/biomet/34.1-2.123, Barnard (1947)}), using Fisher's
p-value referred to as @url{doi:10.1111/j.1467-9574.1970.tb00104.x,
Boschloo's test (1970)}, or using a Z-statistic (@url{doi:10.2307/2981892,
Suissa and Shuster (1985)}).  For comparing two binary proportions,
performs unconditional exact test using McNemar's Z-statistic
(@url{doi:10.1191/0962280203sm312ra, Berger and Sidik (2003)}), using
McNemar's Z-statistic with continuity correction, or using CSM test.
Calculates confidence intervals for the difference in proportion.")
    (license license:gpl2)))

(define-public r-desctools
  (package
    (name "r-desctools")
    (version "0.99.44")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "DescTools" version))
        (sha256
          (base32 "04wp3sp6sahgq167465y38y13h1ia5a4zqazysacpjxd1if0578j"))))
    (properties `((upstream-name . "DescTools")))
    (build-system r-build-system)
    (propagated-inputs
      (list r-bh
            r-boot
            r-data-table
            r-exact
            r-expm
            r-gld
            r-mass
            r-mvtnorm
            r-rcpp
            r-rstudioapi))
    (native-inputs (list gfortran))
    (home-page "https://andrisignorell.github.io/DescTools/")
    (synopsis "Tools for Descriptive Statistics")
    (description
      "This package provides a collection of miscellaneous basic statistic
functions and convenience wrappers for efficiently describing data.
The author's intention was to create a toolbox, which facilitates the
(notoriously time consuming) first descriptive tasks in data analysis,
consisting of calculating descriptive statistics, drawing graphical
summaries and reporting the results.  The package contains furthermore
functions to produce documents using MS Word (or PowerPoint) and functions
to import data from Excel.  Many of the included functions can be found
scattered in other packages and other sources written partly by Titans
of R.  The reason for collecting them here, was primarily to have them
consolidated in ONE instead of dozens of packages (which themselves might
depend on other packages which are not needed at all), and to provide a
common and consistent interface as far as function and arguments naming,
NA handling, recycling rules etc.  are concerned.  Google style guides
were used as naming rules (in absence of convincing alternatives).
The BigCamelCase style was consequently applied to functions borrowed
from contributed R packages as well.")
    (license license:gpl2+)))

(define-public xlispstat
  (let ((commit "f1bea6053df658ee48612bf1f63c35de99e2c649")
        (revision "0"))
    (package
      (name "xlispstat")
      (version (git-version "3.52.23" revision commit))
      (source (origin
	        (method git-fetch)
	        (uri (git-reference
		      (url "https://github.com/jhbadger/xlispstat.git")
		      (commit commit)))
                (file-name (git-file-name name version))
	        (sha256
	         (base32
	          "1p0cmgy19kbkxia139cb5w9dnkp2cdqp5n3baag6cq3prn3n71mf"))))
      (build-system gnu-build-system)
      (arguments
       `(#:parallel-build? #f   ; Parallel builds are not supported
         #:configure-flags (list "--with-gcc")
         #:phases
         (modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (when tests?
                 (with-output-to-file "exit.lsp"
                   (lambda () (display "(exit)")))
                 (invoke "./xlisp" "tests/test" "exit")))))))
      (inputs (list tcsh
                    libx11
                    libxmu
                    libxext
                    libxpm
                    libxaw
                    ncurses
                    gnuplot))
      (native-inputs (list pkg-config))
      (synopsis "Statistical analysis environment with interactive graphics")
      (description "XLISP-STAT is a statistical environment based on a Lisp
dialect called XLISP.  To facilitate statistical computations, standard
functions for addition, logarithms, etc., have been modified to operate on
lists and arrays of numbers, and a number of basic statistical functions have
been added.  Many of these functions have been written in Lisp, and additional
functions can be added easily by a user.  Several basic forms of plots,
including histograms, scatterplots, rotatable plots and scatterplot matrices
are provided.  These plots support various forms of interactive highlighting
operations and can be linked so points highlighted in one plot will be
highlighted in all linked plots.  Interactions with the plots are controlled
by the mouse, menus and dialog boxes.  An object-oriented programming system
is used to allow menus, dialogs, and the response to mouse actions to be
 customized.")
      (home-page "http://homepage.divms.uiowa.edu/~luke/xls/xlsinfo/")
      (license license:expat))))
