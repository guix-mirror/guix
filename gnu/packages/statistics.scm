;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system r)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages java)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages texlive)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages base)
  #:use-module (gnu packages xorg))

(define-public r
  (package
    (name "r")
    (version "3.2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cran/src/base/R-"
                                  (version-prefix version 1) "/R-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "07a6s865bjnh7w0fqsrkv1pva76w99v86w0w787qpdil87km54cw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "LDFLAGS=-Wl,-rpath="
                            (assoc-ref %outputs "out")
                            "/lib/R/lib"))
       #:phases
       (modify-phases %standard-phases
         (add-before
          'configure 'set-default-pager
          ;; Set default pager to "cat", because otherwise it is "false",
          ;; making "help()" print nothing at all.
          (lambda _ (setenv "PAGER" "cat") #t))
         (add-before
          'check 'set-timezone
          ;; Some tests require the timezone to be set.
          (lambda _ (setenv "TZ" "UTC") #t)))
       #:configure-flags
       '("--with-blas=openblas"
         "--with-lapack"
         "--with-cairo"
         "--with-libpng"
         "--with-jpeglib"
         "--with-libtiff"
         "--with-ICU"
         "--enable-R-shlib"
         "--enable-BLAS-shlib"
         "--with-system-zlib"
         "--with-system-bzlib"
         "--with-system-pcre"
         "--with-system-tre"
         "--with-system-xz")))
    (native-inputs
     `(("bzip2" ,bzip2)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("texlive" ,texlive) ; needed to make vignettes
       ("texinfo" ,texinfo) ; for building HTML manuals
       ("which" ,which) ; for tests/Examples/base-Ex.R
       ("xz" ,xz)))
    (inputs
     `(("openblas" ,openblas)
       ("cairo" ,cairo)
       ("gfortran" ,gfortran)
       ("icu4c" ,icu4c)
       ("icedtea6" ,icedtea6 "jdk")
       ("lapack" ,lapack)
       ("libjpeg" ,libjpeg)
       ("libpng" ,libpng)
       ("libtiff" ,libtiff)
       ("libxt" ,libxt)
       ("pcre" ,pcre)
       ("readline" ,readline)
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

(define-public r-colorspace
  (package
    (name "r-colorspace")
    (version "1.2-6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cran/src/contrib/colorspace_"
                           version ".tar.gz"))
       (sha256
        (base32 "0y8n4ljwhbdvkysdwgqzcnpv107pb3px1jip3k6svv86p72nacds"))))
    (build-system r-build-system)
    (home-page "http://cran.r-project.org/web/packages/colorspace")
    (synopsis "Color space manipulation")
    (description
     "This package carries out a mapping between assorted color spaces
including RGB, HSV, HLS, CIEXYZ, CIELUV, HCL (polar CIELUV), CIELAB and polar
CIELAB.  Qualitative, sequential, and diverging color palettes based on HCL
colors are provided.")
    (license license:bsd-3)))

(define-public r-dichromat
  (package
    (name "r-dichromat")
    (version "2.0-0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cran/src/contrib/dichromat_"
                           version ".tar.gz"))
       (sha256
        (base32 "1l8db1nk29ccqg3mkbafvfiw0775iq4gapysf88xq2zp6spiw59i"))))
    (build-system r-build-system)
    (home-page "http://cran.r-project.org/web/packages/dichromat")
    (synopsis "Color schemes for dichromats")
    (description
     "Dichromat collapses red-green or green-blue distinctions to simulate the
effects of different types of color-blindness.")
    (license license:gpl2+)))

(define-public r-digest
  (package
    (name "r-digest")
    (version "0.6.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cran/src/contrib/digest_"
                           version ".tar.gz"))
       (sha256
        (base32 "0m9grqv67hhf51lz10whymhw0g0d98466ka694kya5x95hn44qih"))))
    (build-system r-build-system)
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

(define-public r-gtable
  (package
    (name "r-gtable")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cran/src/contrib/gtable_"
                           version ".tar.gz"))
       (sha256
        (base32 "0k9hfj6r5y238gqh92s3cbdn34biczx3zfh79ix5xq0c5vkai2xh"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/gtable")
    (synopsis "R library to arrange grobs in tables")
    (description
     "Gtable is a collection of tools to make it easier to work with
\"tables\" of grobs.")
    (license license:gpl2+)))

(define-public r-labeling
  (package
    (name "r-labeling")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cran/src/contrib/labeling_"
                           version ".tar.gz"))
       (sha256
        (base32 "13sk7zrrrzry6ky1bp8mmnzcl9jhvkig8j4id9nny7z993mnk00d"))))
    (build-system r-build-system)
    (home-page "http://cran.r-project.org/web/packages/labeling")
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
       (uri (string-append "mirror://cran/src/contrib/magrittr_"
                           version ".tar.gz"))
       (sha256
        (base32 "1s1ar6rag8m277qcqmdp02gn4awn9bdj9ax0r8s32i59mm1mki05"))))
    (build-system r-build-system)
    (home-page "http://cran.r-project.org/web/packages/magrittr/index.html")
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
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cran/src/contrib/munsell_"
                           version ".tar.gz"))
       (sha256
        (base32 "1bi5yi0i80778bbzx2rm4f0glpc34kvh24pwwfhm4v32izsqgrw4"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-colorspace" ,r-colorspace)))
    (home-page "http://cran.r-project.org/web/packages/munsell")
    (synopsis "Munsell colour system")
    (description
     "The Munsell package contains Functions for exploring and using the
Munsell colour system.")
    (license license:expat)))

(define-public r-rcpp
  (package
    (name "r-rcpp")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cran/src/contrib/Rcpp_"
                           version ".tar.gz"))
       (sha256
        (base32 "182109z0yc1snqgd833ssl2cix6cbq83bcxmy5344b15ym820y38"))))
    (build-system r-build-system)
    (home-page "http://www.rcpp.org")
    (synopsis "Seamless R and C++ Integration")
    (description
     "The Rcpp package provides R functions as well as C++ classes which offer
a seamless integration of R and C++.  Many R data types and objects can be
mapped back and forth to C++ equivalents which facilitates both writing of new
code as well as easier integration of third-party libraries.  Documentation
about Rcpp is provided by several vignettes included in this package, via the
'Rcpp Gallery' site at <http://gallery.rcpp.org>, the paper by Eddelbuettel
and Francois (2011, JSS), and the book by Eddelbuettel (2013, Springer); see
'citation(\"Rcpp\")' for details on these last two.")
    (license license:gpl2+)))

(define-public r-plyr
  (package
    (name "r-plyr")
    (version "1.8.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cran/src/contrib/plyr_"
                           version ".tar.gz"))
       (sha256
        (base32 "06v4zxawpjz37rp2q2ii5q43g664z9s29j4ydn0cz3crn7lzl6pk"))))
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
    (version "0.3-10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cran/src/contrib/proto_" version ".tar.gz"))
       (sha256
        (base32 "03mvzi529y6kjcp9bkpk7zlgpcakb3iz73hca6rpjy14pyzl3nfh"))))
    (build-system r-build-system)
    (home-page "http://r-proto.googlecode.com")
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
       (uri (string-append "mirror://cran/src/contrib/RColorBrewer_"
                           version ".tar.gz"))
       (sha256
        (base32 "1pfcl8z1pnsssfaaz9dvdckyfnnc6rcq56dhislbf571hhg7isgk"))))
    (build-system r-build-system)
    (home-page "http://cran.r-project.org/web/packages/RColorBrewer")
    (synopsis "ColorBrewer palettes")
    (description
     "This package provides color schemes for maps (and other graphics)
designed by Cynthia Brewer as described at http://colorbrewer2.org")
    ;; Includes code licensed under bsd-4
    (license license:asl2.0)))

(define-public r-stringi
  (package
    (name "r-stringi")
    (version "0.5-5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cran/src/contrib/stringi_"
             version
             ".tar.gz"))
       (sha256
        (base32
         "183wrrjhpgl1wbnn9lhghyvhz7l2mc64mpcmzplckal7y9j7pmhw"))))
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
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cran/src/contrib/stringr_"
                           version ".tar.gz"))
       (sha256
        (base32 "0jnz6r9yqyf7dschr2fnn1slg4wn6b4ik5q00j4zrh43bfw7s9pq"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-magrittr" ,r-magrittr)
       ("r-stringi" ,r-stringi)))
    (home-page "")
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
    (version "1.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cran/src/contrib/reshape2_"
                           version ".tar.gz"))
       (sha256
        (base32 "0hl082dyk3pk07nqprpn5dvnrkqhnf6zjnjig1ijddxhlmsrzm7v"))))
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
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cran/src/contrib/scales_"
                           version ".tar.gz"))
       (sha256
        (base32 "12xrmn1vh64dl46bq7n7pa427aicb2ifjrby9in3m32nyvir0kac"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-dichromat" ,r-dichromat)
       ("r-labeling" ,r-labeling)
       ("r-munsell" ,r-munsell)
       ("r-plyr" ,r-plyr)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-rcpp" ,r-rcpp)))
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
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cran/src/contrib/ggplot2_"
                           version ".tar.gz"))
       (sha256
        (base32 "0794kjqi3lrxb33lr1mykd58959hlgkhdn259vj8fxrh65mqw920"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-digest" ,r-digest)
       ("r-gtable" ,r-gtable)
       ("r-plyr" ,r-plyr)
       ("r-proto" ,r-proto)
       ("r-reshape2" ,r-reshape2)
       ("r-scales" ,r-scales)))
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
