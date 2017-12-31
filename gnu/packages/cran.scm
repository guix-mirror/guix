;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Roel Janssen <roel@gnu.org>
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

(define-module (gnu packages cran)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system r)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages web))

(define-public r-colorspace
  (package
    (name "r-colorspace")
    (version "1.3-2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "colorspace" version))
       (sha256
        (base32 "0d1ya7hx4y58n5ivwmdmq2zgh0g2sbv7ykh13n85c1355csd57yx"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/colorspace")
    (synopsis "Color space manipulation")
    (description
     "This package carries out a mapping between assorted color spaces
including RGB, HSV, HLS, CIEXYZ, CIELUV, HCL (polar CIELUV), CIELAB and polar
CIELAB.  Qualitative, sequential, and diverging color palettes based on HCL
colors are provided.")
    (license license:bsd-3)))

(define-public r-glue
  (package
    (name "r-glue")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "glue" version))
       (sha256
        (base32
         "0vi9y8ix95g6hjqdz9c9y4119apbdjcypdv0ag8cl6vaxqs5n9qr"))))
    (build-system r-build-system)
    (home-page "https://github.com/tidyverse/glue")
    (synopsis "Interpreted string literals")
    (description
     "This package provides an implementation of interpreted string literals,
inspired by Python's Literal String Interpolation (PEP-0498) and
Docstrings (PEP-0257) and Julia's Triple-Quoted String Literals.")
    (license license:expat)))

(define-public r-plogr
  (package
    (name "r-plogr")
    (version "0.1-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "plogr" version))
       (sha256
        (base32
         "13zliqlbkl8b04k9ga0sx5jsh7k867gracgl84l2a9kcqy9mqx92"))))
    (build-system r-build-system)
    (home-page "https://github.com/krlmlr/plogr")
    (synopsis "R bindings for the plog C++ logging library")
    (description
     "This package provides the header files for a stripped-down version of
the plog header-only C++ logging library, and a method to log to R's standard
error stream.")
    (license license:expat)))

(define-public r-rcpp
  (package
    (name "r-rcpp")
    (version "0.12.14")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Rcpp" version))
       (sha256
        (base32 "0qvyxggddyg87lza45hjl0a2prabdyshkqzkz418vi777zygqa6s"))))
    (build-system r-build-system)
    (home-page "http://www.rcpp.org")
    (synopsis "Seamless R and C++ integration")
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

(define-public r-bindr
  (package
    (name "r-bindr")
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "bindr" version))
       (sha256
        (base32
         "0d95ifm0x4mrfzi20xf39f5pzd7rfzqsld0vjqf6xzga5rhnd8fc"))))
    (build-system r-build-system)
    (home-page "https://github.com/krlmlr/bindr")
    (synopsis "Parametrized active bindings")
    (description
     "This package provides a simple interface for creating active bindings
where the bound function accepts additional arguments.")
    (license license:expat)))

(define-public r-bindrcpp
  (package
    (name "r-bindrcpp")
    (version "0.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "bindrcpp" version))
       (sha256
        (base32
         "0l1l22zl87wiyl79m3gj2vlxmkhxvrkl4alhyy08h55q7hqs3vyh"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bindr" ,r-bindr)
       ("r-plogr" ,r-plogr)
       ("r-rcpp" ,r-rcpp)))
    (home-page "https://github.com/krlmlr/bindrcpp")
    (synopsis "Rcpp interface to active bindings")
    (description
     "This package provides an easy way to fill an environment with active
bindings that call a C++ function.")
    (license license:expat)))

(define-public r-auc
  (package
    (name "r-auc")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "AUC" version))
       (sha256
        (base32
         "0ripcib2qz0m7rgr1kiz68nx8f6p408l1ww7j78ljqik7p3g41g7"))))
    (properties `((upstream-name . "AUC")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/AUC")
    (synopsis "Compute the area under the curve of selected measures")
    (description
     "This package includes functions to compute the area under the curve of
selected measures: the area under the sensitivity curve (AUSEC), the area
under the specificity curve (AUSPC), the area under the accuracy
curve (AUACC), and the area under the receiver operating characteristic
curve (AUROC).  The curves can also be visualized.  Support for partial areas
is provided.")
    (license license:gpl2+)))

(define-public r-calibrate
  (package
    (name "r-calibrate")
    (version "1.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "calibrate" version))
       (sha256
        (base32
         "010nb1nb9y7zhw2k6d2i2drwy5brp7b83mjj2w7i3wjp9xb6l1kq"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-mass" ,r-mass)))
    (home-page "https://cran.r-project.org/web/packages/calibrate")
    (synopsis "Calibration of scatterplot and biplot axes")
    (description
     "This is a package for drawing calibrated scales with tick marks
on (non-orthogonal) variable vectors in scatterplots and biplots.")
    (license license:gpl2)))

(define-public r-shape
  (package
    (name "r-shape")
    (version "1.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "shape" version))
       (sha256
        (base32
         "1v9xp60p813rnx41vchkh32qmcb4z2zp9l7r1a8a6f8aqylnq3vj"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/shape")
    (synopsis "Functions for plotting graphical shapes")
    (description
     "This package provides functions for plotting graphical shapes such as
ellipses, circles, cylinders, arrows, ...")
    (license license:gpl3+)))

(define-public r-globaloptions
  (package
    (name "r-globaloptions")
    (version "0.0.12")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "GlobalOptions" version))
       (sha256
        (base32
         "1abpc03cfvazbwj2sx6qgngs5pgpzysvxkana20hyvb4n7ws77f0"))))
    (properties `((upstream-name . "GlobalOptions")))
    (build-system r-build-system)
    (home-page "https://github.com/jokergoo/GlobalOptions")
    (synopsis "Generate functions to get or set global options")
    (description
     "This package provides more controls on the option values such as
validation and filtering on the values, making options invisible or private.")
    (license license:gpl2+)))

(define-public r-circlize
  (package
    (name "r-circlize")
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "circlize" version))
       (sha256
        (base32
         "0pg947gkryygikfbv6admx8gsg02b6g219j6a1sdnqij2908yxna"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-colorspace" ,r-colorspace)
       ("r-globaloptions" ,r-globaloptions)
       ("r-shape" ,r-shape)))
    (home-page "https://github.com/jokergoo/circlize")
    (synopsis "Circular visualization")
    (description
     "Circular layout is an efficient way to visualise huge amounts of
information.  This package provides an implementation of circular layout
generation in R as well as an enhancement of available software.  Its
flexibility is based on the usage of low-level graphics functions such that
self-defined high-level graphics can be easily implemented by users for
specific purposes.  Together with the seamless connection between the powerful
computational and visual environment in R, it gives users more convenience and
freedom to design figures for better understanding complex patterns behind
multi-dimensional data.")
    (license license:gpl2+)))

(define-public r-powerlaw
  (package
    (name "r-powerlaw")
    (version "0.70.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "poweRlaw" version))
       (sha256
        (base32
         "04sr0nhdd1v915m0zf5gasznzgi08ykcy20kkwdw0l5mvvdbic8m"))))
    (properties `((upstream-name . "poweRlaw")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-vgam" ,r-vgam)))
    (home-page "https://github.com/csgillespie/poweRlaw")
    (synopsis "Tools for the analysis of heavy tailed distributions")
    (description
     "This package provides an implementation of maximum likelihood estimators
for a variety of heavy tailed distributions, including both the discrete and
continuous power law distributions.  Additionally, a goodness-of-fit based
approach is used to estimate the lower cut-off for the scaling region.")
    ;; Any of these GPL versions.
    (license (list license:gpl2 license:gpl3))))

(define-public r-compare
  (package
    (name "r-compare")
    (version "0.2-6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "compare" version))
       (sha256
        (base32
         "0k9zms930b5dz9gy8414li21wy0zg9x9vp7301v5cvyfi0g7xzgw"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/compare")
    (synopsis "Comparing objects for differences")
    (description
     "This package provides functions to compare a model object to a
comparison object.  If the objects are not identical, the functions can be
instructed to explore various modifications of the objects (e.g., sorting
rows, dropping names) to see if the modified versions are identical.")
    (license license:gpl2+)))

(define-public r-dendextend
  (package
    (name "r-dendextend")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "dendextend" version))
       (sha256
        (base32
         "0pichh08zi12lpxzc061dmv4smj6lizygd1xymln8wfz18sf7923"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-fpc" ,r-fpc)
       ("r-ggplot2" ,r-ggplot2)
       ("r-magrittr" ,r-magrittr)
       ("r-viridis" ,r-viridis)
       ("r-whisker" ,r-whisker)))
    (home-page "https://cran.r-project.org/web/packages/dendextend")
    (synopsis "Extending 'dendrogram' functionality in R")
    (description
     "This package offers a set of functions for extending @code{dendrogram}
objects in R, letting you visualize and compare trees of hierarchical
clusterings.  You can adjust a tree's graphical parameters (the color, size,
type, etc of its branches, nodes and labels) and visually and statistically
compare different dendrograms to one another.")
    ;; Any of these versions
    (license (list license:gpl2 license:gpl3))))

(define-public r-getoptlong
  (package
    (name "r-getoptlong")
    (version "0.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "GetoptLong" version))
       (sha256
        (base32
         "1d98gcvlvp9nz5lbnzr0kkpc2hbkx74hlhrnybqhg1gdwc3g09pm"))))
    (properties `((upstream-name . "GetoptLong")))
    (build-system r-build-system)
    (inputs
     `(("perl" ,perl)))
    (propagated-inputs
     `(("r-globaloptions" ,r-globaloptions)
       ("r-rjson" ,r-rjson)))
    (home-page "https://github.com/jokergoo/GetoptLong")
    (synopsis "Parsing command-line arguments and variable interpolation")
    (description
     "This is yet another command-line argument parser which wraps the
powerful Perl module @code{Getopt::Long} and with some adaptation for easier
use in R.  It also provides a simple way for variable interpolation in R.")
    (license license:gpl2+)))

(define-public r-fastmatch
  (package
    (name "r-fastmatch")
    (version "1.1-0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "fastmatch" version))
       (sha256
        (base32
         "0z80jxkygmzn11sq0c2iz357s9bpki548lg926g85gldhfj1md90"))))
    (build-system r-build-system)
    (home-page "http://www.rforge.net/fastmatch")
    (synopsis "Fast match function")
    (description
     "This package provides a fast @code{match} replacement for cases that
require repeated look-ups.  It is slightly faster that R's built-in
@code{match} function on first match against a table, but extremely fast on
any subsequent lookup as it keeps the hash table in memory.")
    (license license:gpl2)))

(define-public r-ff
  (package
    (name "r-ff")
    (version "2.2-13")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ff" version))
       (sha256
        (base32
         "1nvd6kx46xzyc99a44mgynd94pvd2h495m5a7b1g67k5w2phiywb"))))
    (build-system r-build-system)
    (propagated-inputs `(("r-bit" ,r-bit)))
    (home-page "http://ff.r-forge.r-project.org/")
    (synopsis "Memory-efficient storage of large data on disk and access functions")
    (description
     "This package provides data structures that are stored on disk but
behave (almost) as if they were in RAM by transparently mapping only a section
in main memory.")
    ;; error Architecture not supported.
    (supported-systems (delete "aarch64-linux" %supported-systems))
    (license license:gpl2)))

(define-public r-ffbase
  (package
    (name "r-ffbase")
    (version "0.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ffbase" version))
       (sha256
        (base32
         "1nz97bndxxkzp8rq6va8ff5ky9vkaib1jybm6j852awwb3n9had5"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bit" ,r-bit)
       ("r-fastmatch" ,r-fastmatch)
       ("r-ff" ,r-ff)))
    (home-page "http://github.com/edwindj/ffbase")
    (synopsis "Basic statistical functions for package 'ff'")
    (description
     "This package extends the out of memory vectors of @code{ff} with
statistical functions and other utilities to ease their usage.")
    (license license:gpl3)))

(define-public r-prettyunits
  (package
    (name "r-prettyunits")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "prettyunits" version))
       (sha256
        (base32
         "0p3z42hnk53x7ky4d1dr2brf7p8gv3agxr71i99m01n2hq2ri91m"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-assertthat" ,r-assertthat)
       ("r-magrittr" ,r-magrittr)))
    (home-page "https://github.com/gaborcsardi/prettyunits")
    (synopsis "Pretty, human readable formatting of quantities")
    (description
     "This package provides tools for pretty, human readable formatting of
quantities.")
    (license license:expat)))

(define-public r-reshape
  (package
    (name "r-reshape")
    (version "0.8.7")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "reshape" version))
       (sha256
        (base32
         "14ir3w4bb3bsz8jsak27nj7kpn227pdgr9653gjq5wc93rywi9ig"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-plyr" ,r-plyr)
       ("r-rcpp" ,r-rcpp)))
    (home-page "http://had.co.nz/reshape")
    (synopsis "Flexibly reshape data")
    (description
     "Flexibly restructure and aggregate data using just two functions:
@code{melt} and @code{cast}.  This package provides them.")
    (license license:expat)))

(define-public r-progress
  (package
    (name "r-progress")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "progress" version))
       (sha256
        (base32
         "1fxakchfjr5vj59s9sxynd7crpz97xj42438rmkhkf3rjpyspx59"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-prettyunits" ,r-prettyunits)
       ("r-r6" ,r-r6)))
    (home-page "https://github.com/gaborcsardi/progress")
    (synopsis "Terminal progress bars")
    (description
     "This package provides configurable progress bars.  They may include
percentage, elapsed time, and/or the estimated completion time.  They work in
terminals, in Emacs ESS, RStudio, Windows Rgui, and the macOS R.app.  The
package also provides a C++ API, that works with or without Rcpp.")
    (license license:expat)))

(define-public r-ggally
  (package
    (name "r-ggally")
    (version "1.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "GGally" version))
       (sha256
        (base32
         "02px7j28wzbhcgcwm2m0pxb6g7s5zvphl64ix55pkvag4m2ky57l"))))
    (properties `((upstream-name . "GGally")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-gtable" ,r-gtable)
       ("r-plyr" ,r-plyr)
       ("r-progress" ,r-progress)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-reshape" ,r-reshape)))
    (home-page "https://ggobi.github.io/ggally")
    (synopsis "Extension to ggplot2")
    (description
     "The R package ggplot2 is a plotting system based on the grammar of
graphics.  GGally extends ggplot2 by adding several functions to reduce the
complexity of combining geometric objects with transformed data.  Some of
these functions include a pairwise plot matrix, a two group pairwise plot
matrix, a parallel coordinates plot, a survival plot, and several functions to
plot networks.")
    (license license:gpl2+)))

(define-public r-proxy
  (package
    (name "r-proxy")
    (version "0.4-20")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "proxy" version))
       (sha256
        (base32
         "15g6dacdmlbkcnimblscghl23aj732cn6qwbs583r4im9v5nvbla"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/proxy")
    (synopsis "Distance and similarity measures")
    (description
     "This package provides an extensible framework for the efficient
calculation of auto- and cross-proximities, along with implementations of the
most popular ones.")
    (license license:gpl2)))

(define-public r-sp
  (package
    (name "r-sp")
    (version "1.2-5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "sp" version))
       (sha256
        (base32
         "09yydnacp33yx0kn537k96fjlbf75fjafqfknpa5gvci4l888bqd"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-lattice" ,r-lattice)))
    (home-page "https://cran.r-project.org/web/packages/sp")
    (synopsis "Classes and methods for spatial data")
    (description
     "This package provides classes and methods for spatial data; the classes
document where the spatial location information resides, for 2D or 3D data.
Utility functions are provided, e.g. for plotting data as maps, spatial
selection, as well as methods for retrieving coordinates, for subsetting,
print, summary, etc.")
    (license license:gpl2+)))

(define-public r-rmtstat
  (package
    (name "r-rmtstat")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "RMTstat" version))
       (sha256
        (base32
         "1nn25q4kmh9kj975sxkrpa97vh5irqrlqhwsfinbck6h6ia4rsw1"))))
    (properties `((upstream-name . "RMTstat")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/RMTstat")
    (synopsis "Distributions, statistics and tests derived from random matrix theory")
    (description
     "This package provides functions for working with the Tracy-Widom laws
and other distributions related to the eigenvalues of large Wishart
matrices.")
    (license license:bsd-3)))

(define-public r-rmpi
  (package
    (name "r-rmpi")
    (version "0.6-6")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "Rmpi" version))
              (sha256
               (base32
                "0fm6z049aaq2c9xagm8n64d9560hg9d8hyb0m359fii672nhkz6q"))))
    (properties `((upstream-name . "Rmpi")))
    (build-system r-build-system)
    (arguments
     `(#:configure-flags '("--configure-args=\"--with-Rmpi-type=OPENMPI\"")))
    (inputs
     `(("openmpi" ,openmpi)))
    (home-page "http://www.stats.uwo.ca/faculty/yu/Rmpi")
    (synopsis "R interface to message-passing interface (MPI)")
    (description
     "This package provides an interface (wrapper) to MPI APIs.  It also
provides an interactive R manager and worker environment.")
    (license license:gpl2+)))

(define-public r-lmoments
  (package
    (name "r-lmoments")
    (version "1.2-3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Lmoments" version))
       (sha256
        (base32
         "13p0r4w16jvjnyjmkhkp3dwdfr1gap2l0k4k5jy41m8nc5fvcx79"))))
    (properties `((upstream-name . "Lmoments")))
    (build-system r-build-system)
    (home-page "http://www.tilastotiede.fi/juha_karvanen.html")
    (synopsis "L-moments and quantile mixtures")
    (description
     "This package contains functions to estimate L-moments and trimmed
L-moments from the data.  It also contains functions to estimate the
parameters of the normal polynomial quantile mixture and the Cauchy polynomial
quantile mixture from L-moments and trimmed L-moments.")
    (license license:gpl2)))

(define-public r-distillery
  (package
    (name "r-distillery")
    (version "1.0-4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "distillery" version))
       (sha256
        (base32
         "1m0pgmlvk7bsb6q3kxagnq422babk61sf73naavac68v8x2q8fix"))))
    (build-system r-build-system)
    (home-page "http://www.ral.ucar.edu/staff/ericg")
    (synopsis "Functions for confidence intervals and object information")
    (description
     "This package provides some very simple method functions for confidence
interval calculation and to distill pertinent information from a potentially
complex object; primarily used in common with the packages extRemes and
SpatialVx.")
    (license license:gpl2+)))

(define-public r-extremes
  (package
    (name "r-extremes")
    (version "2.0-8")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "extRemes" version))
       (sha256
        (base32
         "0pnpib3g2r9x8hfqhvq23j8m3jh62lp28ipnqir5yadnzv850gfm"))))
    (properties `((upstream-name . "extRemes")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-car" ,r-car)
       ("r-distillery" ,r-distillery)
       ("r-lmoments" ,r-lmoments)))
    (home-page "http://www.assessment.ucar.edu/toolkit/")
    (synopsis "Extreme value analysis")
    (description
     "ExtRemes is a suite of functions for carrying out analyses on the
extreme values of a process of interest; be they block maxima over long blocks
or excesses over a high threshold.")
    (license license:gpl2+)))

(define-public r-lmtest
  (package
    (name "r-lmtest")
    (version "0.9-35")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "lmtest" version))
       (sha256
        (base32
         "107br1l7p52wxvazs031f4h5ryply97qywg9dzrkw4ydnvqq4j9g"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-zoo" ,r-zoo)))
    (native-inputs
     `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/lmtest")
    (synopsis "Testing linear regression models")
    (description
     "This package provides a collection of tests, data sets, and examples for
diagnostic checking in linear regression models.  Furthermore, some generic
tools for inference in parametric models are provided.")
    ;; Either version is okay
    (license (list license:gpl2 license:gpl3))))

(define-public r-inline
  (package
    (name "r-inline")
    (version "0.3.14")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "inline" version))
              (sha256
               (base32
                "0cf9vya9h4znwgp6s1nayqqmh6mwyw7jl0isk1nx4j2ijszxcd7x"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/inline")
    (synopsis "Functions to inline C, C++, Fortran function calls from R")
    (description
     "This package provides functionality to dynamically define R functions
and S4 methods with inlined C, C++ or Fortran code supporting @code{.C} and
@code{.Call} calling conventions.")
    ;; Any version of the LGPL.
    (license license:lgpl3+)))

(define-public r-bbmle
  (package
    (name "r-bbmle")
    (version "1.0.20")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "bbmle" version))
       (sha256
        (base32
         "1xzij7swrrzl5ly8l3lw6awh486zcm00251dwqws1y23fbgyh3vc"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-lattice" ,r-lattice)
       ("r-mass" ,r-mass)
       ("r-numderiv" ,r-numderiv)))
    (home-page "https://cran.r-project.org/web/packages/bbmle")
    (synopsis "Tools for General Maximum Likelihood Estimation")
    (description
     "This package provides methods and functions for fitting maximum
likelihood models in R.  This package modifies and extends the @code{mle}
classes in the @code{stats4} package.")
    ;; Any version of the GPL
    (license license:gpl2+)))

(define-public r-emdbook
  (package
    (name "r-emdbook")
    (version "1.3.9")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "emdbook" version))
       (sha256
        (base32
         "09xbdyw8a4pvrsg3ryr8drby0njy4avc5wsjj4ffibdaicpchy69"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bbmle" ,r-bbmle)
       ("r-coda" ,r-coda)
       ("r-lattice" ,r-lattice)
       ("r-mass" ,r-mass)
       ("r-plyr" ,r-plyr)
       ("r-rcpp" ,r-rcpp)))
    (home-page "http://www.math.mcmaster.ca/bolker/emdbook")
    (synopsis "Support functions and data for \"Ecological Models and Data\"")
    (description
     "This package provides auxiliary functions and data sets for \"Ecological
Models and Data\", a book presenting maximum likelihood estimation and related
topics for ecologists (ISBN 978-0-691-12522-0).")
    ;; Any GPL version
    (license (list license:gpl2 license:gpl3))))

(define-public r-lpsolve
  (package
    (name "r-lpsolve")
    (version "5.6.13")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "lpSolve" version))
       (sha256
        (base32
         "13a9ry8xf5j1f2j6imqrxdgxqz3nqp9sj9b4ivyx9sid459irm6m"))))
    (properties `((upstream-name . "lpSolve")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/lpSolve")
    (synopsis "R interface to Lp_solve to solve linear/integer programs")
    (description
     "Lp_solve is software for solving linear, integer and mixed integer
programs.  This implementation supplies a \"wrapper\" function in C and some R
functions that solve general linear/integer problems, assignment problems, and
transportation problems.")
    (license license:lgpl2.0)))

(define-public r-limsolve
  (package
    (name "r-limsolve")
    (version "1.5.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "limSolve" version))
       (sha256
        (base32
         "1ll6ir42h3g2fzf0wqai213bm82gpwjj2hfma2np3mz024sc09rg"))))
    (properties `((upstream-name . "limSolve")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-lpsolve" ,r-lpsolve)
       ("r-mass" ,r-mass)
       ("r-quadprog" ,r-quadprog)))
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/limSolve")
    (synopsis "Solving linear inverse models")
    (description
     "This package provides functions that:

@enumerate
@item find the minimum/maximum of a linear or quadratic function,
@item sample an underdetermined or overdetermined system,
@item solve a linear system Ax=B for the unknown x.
@end enumerate

It includes banded and tridiagonal linear systems.  The package calls Fortran
functions from LINPACK.")
    ;; Any GPL version.
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-fitdistrplus
  (package
    (name "r-fitdistrplus")
    (version "1.0-9")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "fitdistrplus" version))
       (sha256
        (base32
         "18x9454g598d54763k3hvi33iszifk7sxvhd1zg5r8z1vpixx3z6"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-mass" ,r-mass)
       ("r-survival" ,r-survival)))
    (home-page "http://riskassessment.r-forge.r-project.org")
    (synopsis "Fitting a parametric distribution from data")
    (description
     "This package extends the @code{fitdistr} function of the MASS package
with several functions to help the fit of a parametric distribution to
non-censored or censored data.  Censored data may contain left-censored,
right-censored and interval-censored values, with several lower and upper
bounds.  In addition to @dfn{maximum likelihood estimation} (MLE), the package
provides moment matching (MME), quantile matching (QME) and maximum
goodness-of-fit estimation (MGE) methods (available only for non-censored
data).  Weighted versions of MLE, MME and QME are available.")
    (license license:gpl2+)))

(define-public r-energy
  (package
    (name "r-energy")
    (version "1.7-2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "energy" version))
       (sha256
        (base32
         "19c7bgjnm4ggf7w5mk64c5shkma3sa9wc8x117iqv7pk1bvvyy3p"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-boot" ,r-boot)
       ("r-rcpp" ,r-rcpp)))
    (home-page "https://cran.r-project.org/web/packages/energy")
    (synopsis "Multivariate inference via the energy of data")
    (description
     "This package provides e-statistics (energy) tests and statistics for
multivariate and univariate inference, including distance correlation,
one-sample, two-sample, and multi-sample tests for comparing multivariate
distributions, are implemented.  Measuring and testing multivariate
independence based on distance correlation, partial distance correlation,
multivariate goodness-of-fit tests, clustering based on energy distance,
testing for multivariate normality, distance components (disco) for
non-parametric analysis of structured data, and other energy
statistics/methods are implemented.")
    (license license:gpl2+)))

(define-public r-suppdists
  (package
    (name "r-suppdists")
    (version "1.1-9.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "SuppDists" version))
       (sha256
        (base32
         "1ffx8wigqqvz2pnh06jjc0fnf4vq9z2rhwk2y3f9aszn18ap3dgw"))))
    (properties `((upstream-name . "SuppDists")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/SuppDists")
    (synopsis "Supplementary distributions")
    (description
     "This package provides ten distributions supplementing those built into
R.  Inverse Gauss, Kruskal-Wallis, Kendall's Tau, Friedman's chi squared,
Spearman's rho, maximum F ratio, the Pearson product moment correlation
coefficient, Johnson distributions, normal scores and generalized
hypergeometric distributions.  In addition two random number generators of
George Marsaglia are included.")
    (license license:gpl2+)))

(define-public r-ksamples
  (package
    (name "r-ksamples")
    (version "1.2-7")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "kSamples" version))
       (sha256
        (base32
         "0f19rjngk0lg6s8c6h5l55qpxp8sl4vxj3kpi05cizzncny9b9bj"))))
    (properties `((upstream-name . "kSamples")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-suppdists" ,r-suppdists)))
    (home-page "https://cran.r-project.org/web/packages/kSamples")
    (synopsis "K-Sample rank tests and their combinations")
    (description
     "This package provides tools to compares k samples using the
Anderson-Darling test, Kruskal-Wallis type tests with different rank score
criteria, Steel's multiple comparison test, and the Jonckheere-Terpstra (JT)
test.  It computes asymptotic, simulated or (limited) exact P-values, all
valid under randomization, with or without ties, or conditionally under random
sampling from populations, given the observed tie pattern.  Except for Steel's
test and the JT test it also combines these tests across several blocks of
samples.")
    (license license:gpl2+)))

(define-public r-cvst
  (package
    (name "r-cvst")
    (version "0.2-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "CVST" version))
       (sha256
        (base32
         "17xacyi8cf37rr2xswx96qy7pwkaqq394awdlswykz3qlyzx4zx2"))))
    (properties `((upstream-name . "CVST")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-kernlab" ,r-kernlab)
       ("r-matrix" ,r-matrix)))
    (home-page "https://cran.r-project.org/web/packages/CVST")
    (synopsis "Fast cross-validation via sequential testing")
    (description
     "This package implements the fast cross-validation via sequential
testing (CVST) procedure.  CVST is an improved cross-validation procedure
which uses non-parametric testing coupled with sequential analysis to
determine the best parameter set on linearly increasing subsets of the data.
Additionally to the CVST the package contains an implementation of the
ordinary k-fold cross-validation with a flexible and powerful set of helper
objects and methods to handle the overall model selection process.  The
implementations of the Cochran's Q test with permutations and the sequential
testing framework of Wald are generic and can therefore also be used in other
contexts.")
    (license license:gpl2+)))

(define-public r-lava
  (package
    (name "r-lava")
    (version "1.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "lava" version))
       (sha256
        (base32
         "1vcm04h9i39gmf2prl5d4j4il4gs6skzr6y2fvl1vn4hklna87f4"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-numderiv" ,r-numderiv)
       ("r-survival" ,r-survival)))
    (home-page "https://github.com/kkholst/lava")
    (synopsis "Latent variable models")
    (description
     "This package provides tools for the estimation and simulation of latent
variable models.")
    (license license:gpl3)))

(define-public r-drr
  (package
    (name "r-drr")
    (version "0.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "DRR" version))
       (sha256
        (base32
         "1scfwp6ry6apxzqjclsmn2frxp9qfw6zxsxn5w0j0q3sz42hz1h2"))))
    (properties `((upstream-name . "DRR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-cvst" ,r-cvst)
       ("r-kernlab" ,r-kernlab)
       ("r-matrix" ,r-matrix)))
    (home-page "https://cran.r-project.org/web/packages/DRR")
    (synopsis "Dimensionality reduction via regression")
    (description
     "This package provides an implementation of dimensionality reduction via
regression using Kernel Ridge Regression.")
    (license license:gpl3)))

(define-public r-prodlim
  (package
    (name "r-prodlim")
    (version "1.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "prodlim" version))
       (sha256
        (base32
         "0m51rkivx1zr6whdqwj66jpnkmp4385m06kkha3dp8qqf4jna9iz"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-kernsmooth" ,r-kernsmooth)
       ("r-lava" ,r-lava)
       ("r-rcpp" ,r-rcpp)
       ("r-survival" ,r-survival)))
    (home-page "https://cran.r-project.org/web/packages/prodlim")
    (synopsis "Product-limit estimation for censored event history analysis")
    (description
     "This package provides a fast and user-friendly implementation of
nonparametric estimators for censored event history (survival) analysis with
the Kaplan-Meier and Aalen-Johansen methods.")
    (license license:gpl2+)))

(define-public r-dimred
  (package
    (name "r-dimred")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "dimRed" version))
       (sha256
        (base32
         "0fasca5fsbrxdwv30hch7vb9snb844l7l8p5fjf239dq45xfy37v"))))
    (properties `((upstream-name . "dimRed")))
    (build-system r-build-system)
    (propagated-inputs `(("r-drr" ,r-drr)))
    (home-page "https://github.com/gdkrmr/dimRed")
    (synopsis "Framework for dimensionality reduction")
    (description
     "This package provides a collection of dimensionality reduction
techniques from R packages and provides a common interface for calling the
methods.")
    (license license:gpl3)))

(define-public r-timedate
  (package
    (name "r-timedate")
    (version "3042.101")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "timeDate" version))
       (sha256
        (base32
         "0vcckaw1gqz3j4v69r9jn41vlmk5a5c7572xam1nl75ki5v4r3bc"))))
    (properties `((upstream-name . "timeDate")))
    (build-system r-build-system)
    (home-page "https://www.rmetrics.org")
    (synopsis "Chronological and calendar objects")
    (description
     "This package provides an environment for teaching \"Financial
Engineering and Computational Finance\" and for managing chronological and
calendar objects.")
    (license license:gpl2+)))

(define-public r-ddalpha
  (package
    (name "r-ddalpha")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ddalpha" version))
       (sha256
        (base32
         "0pczw9543y7f92m7gyk7rxcjn8vsjaldc5vl0r56ywip9i374zbh"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bh" ,r-bh)
       ("r-class" ,r-class)
       ("r-mass" ,r-mass)
       ("r-rcpp" ,r-rcpp)
       ("r-robustbase" ,r-robustbase)
       ("r-sfsmisc" ,r-sfsmisc)))
    (native-inputs
     `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/ddalpha")
    (synopsis "Depth-Based classification and calculation of data depth")
    (description
     "This package contains procedures for depth-based supervised learning,
which are entirely non-parametric, in particular the DDalpha-procedure (Lange,
Mosler and Mozharovskyi, 2014).  The training data sample is transformed by a
statistical depth function to a compact low-dimensional space, where the final
classification is done.  It also offers an extension to functional data and
routines for calculating certain notions of statistical depth functions.  50
multivariate and 5 functional classification problems are included.")
    (license license:gpl2)))

(define-public r-gower
  (package
    (name "r-gower")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "gower" version))
       (sha256
        (base32
         "1mbrj1lam3jfbby2j32shmmj5cn09zx3rkxbamq7q8sdg39b54gb"))))
    (build-system r-build-system)
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/markvanderloo/gower")
    (synopsis "Gower's distance")
    (description
     "This package provides tools to compute Gower's distance (or similarity)
coefficient between records, and to compute the top-n matches between records.
Core algorithms are executed in parallel on systems supporting OpenMP.")
    (license license:gpl3)))

(define-public r-rcpproll
  (package
    (name "r-rcpproll")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "RcppRoll" version))
       (sha256
        (base32
         "19xzvxym8zbighndygkq4imfwc0abh4hqyq3qrr8aakyd096iisi"))))
    (properties `((upstream-name . "RcppRoll")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rcpp" ,r-rcpp)))
    (home-page "https://cran.r-project.org/web/packages/RcppRoll")
    (synopsis "Efficient rolling and windowed operations")
    (description
     "This package provides fast and efficient routines for common rolling /
windowed operations.  Routines for the efficient computation of windowed mean,
median, sum, product, minimum, maximum, standard deviation and variance are
provided.")
    (license license:gpl2+)))

(define-public r-ipred
  (package
    (name "r-ipred")
    (version "0.9-6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ipred" version))
       (sha256
        (base32
         "1vrw1pqcpnc04x1r2h9grdfm6bivs358sww5gg90jwlvxcw69lxq"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-class" ,r-class)
       ("r-mass" ,r-mass)
       ("r-nnet" ,r-nnet)
       ("r-prodlim" ,r-prodlim)
       ("r-rpart" ,r-rpart)
       ("r-survival" ,r-survival)))
    (home-page "https://cran.r-project.org/web/packages/ipred")
    (synopsis "Improved predictors")
    (description
     "This package provides improved predictive models by indirect
classification and bagging for classification, regression and survival
problems as well as resampling based estimators of prediction error.")
    (license license:gpl2+)))

(define-public r-psych
  (package
    (name "r-psych")
    (version "1.7.8")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "psych" version))
       (sha256
        (base32
         "0daismb8pdk392vdy304hqx0m3jx62gx3a0hygjygc125rhfla7k"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-foreign" ,r-foreign)
       ("r-lattice" ,r-lattice)
       ("r-mnormt" ,r-mnormt)
       ("r-nlme" ,r-nlme)))
    (home-page "https://cran.r-project.org/web/packages/psych/")
    (synopsis "Procedures for psychological, psychometric, and personality research")
    (description
     "This package provides a general purpose toolbox for personality,
psychometric theory and experimental psychology.  Functions are primarily for
multivariate analysis and scale construction using factor analysis, principal
component analysis, cluster analysis and reliability analysis, although others
provide basic descriptive statistics.  Item Response Theory is done using
factor analysis of tetrachoric and polychoric correlations.  Functions for
analyzing data at multiple levels include within and between group statistics,
including correlations and factor analysis.  Functions for simulating and
testing particular item and test structures are included.  Several functions
serve as a useful front end for structural equation modeling.  Graphical
displays of path diagrams, factor analysis and structural equation models are
created using basic graphics.")
    (license license:gpl2+)))

(define-public r-broom
  (package
    (name "r-broom")
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "broom" version))
       (sha256
        (base32
         "119pc2jnxvm13cvd77c7d14p3bn68f4jm310vj3yfck40101n9if"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-dplyr" ,r-dplyr)
       ("r-nlme" ,r-nlme)
       ("r-plyr" ,r-plyr)
       ("r-psych" ,r-psych)
       ("r-reshape2" ,r-reshape2)
       ("r-stringr" ,r-stringr)
       ("r-tidyr" ,r-tidyr)))
    (home-page "http://github.com/tidyverse/broom")
    (synopsis "Convert statistical analysis objects into tidy data frames")
    (description
     "This package provides tools to convert statistical analysis objects from
R into tidy data frames, so that they can more easily be combined, reshaped
and otherwise processed with tools like @code{dplyr}, @code{tidyr} and
@code{ggplot2}.  The package provides three S3 generics: @code{tidy}, which
summarizes a model's statistical findings such as coefficients of a
regression; @code{augment}, which adds columns to the original data such as
predictions, residuals and cluster assignments; and @code{glance}, which
provides a one-row summary of model-level statistics.")
    (license license:expat)))

(define-public r-recipes
  (package
    (name "r-recipes")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "recipes" version))
       (sha256
        (base32
         "0id46c7iaf49miw4kxpidsbg3hdywav43n1lh2zd1vg5946bzg04"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-broom" ,r-broom)
       ("r-ddalpha" ,r-ddalpha)
       ("r-dimred" ,r-dimred)
       ("r-dplyr" ,r-dplyr)
       ("r-gower" ,r-gower)
       ("r-ipred" ,r-ipred)
       ("r-lubridate" ,r-lubridate)
       ("r-magrittr" ,r-magrittr)
       ("r-matrix" ,r-matrix)
       ("r-purrr" ,r-purrr)
       ("r-rcpproll" ,r-rcpproll)
       ("r-rlang" ,r-rlang)
       ("r-tibble" ,r-tibble)
       ("r-tidyselect" ,r-tidyselect)
       ("r-timedate" ,r-timedate)))
    (home-page "https://github.com/topepo/recipes")
    (synopsis "Preprocessing tools to create design matrices")
    (description
     "Recipes is an extensible framework to create and preprocess design
matrices.  Recipes consist of one or more data manipulation and analysis
\"steps\".  Statistical parameters for the steps can be estimated from an
initial data set and then applied to other data sets.  The resulting design
matrices can then be used as inputs into statistical or machine learning
models.")
    (license license:gpl2)))

(define-public r-pdist
  (package
    (name "r-pdist")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "pdist" version))
       (sha256
        (base32
         "18nd3mgad11f2zmwcp0w3sxlch4a9y6wp8dfdyzvjn7y4b4bq0dd"))))
    (build-system r-build-system)
    (home-page "https://github.com/jeffwong/pdist")
    (synopsis "Partitioned distance function")
    (description
     "Pdist computes the euclidean distance between rows of a matrix X and
rows of another matrix Y.  Previously, this could be done by binding the two
matrices together and calling @code{dist}, but this creates unnecessary
computation by computing the distances between a row of X and another row of
X, and likewise for Y.  Pdist strictly computes distances across the two
matrices, not within the same matrix, making computations significantly faster
for certain use cases.")
    (license license:gpl3+)))

(define-public r-ggrepel
  (package
    (name "r-ggrepel")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ggrepel" version))
       (sha256
        (base32
         "0g0qfm6g71rv27423c1x846ipilnj213knyzrcr09vrpxc87l618"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-rcpp" ,r-rcpp)
       ("r-scales" ,r-scales)))
    (home-page "http://github.com/slowkow/ggrepel")
    (synopsis "Repulsive text and label geometries for ggplot2")
    (description
     "This package provides text and label geometries for ggplot2 that help to
avoid overlapping text labels.  Labels repel away from each other and away
from the data points.")
    (license license:gpl3)))

(define-public r-corrplot
  (package
    (name "r-corrplot")
    (version "0.84")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "corrplot" version))
       (sha256
        (base32
         "1k03qd8db7pwg1v318xapx5mpiypiz2n07qr19c4b45diri5xkhd"))))
    (build-system r-build-system)
    (home-page "https://github.com/taiyun/corrplot")
    (synopsis "Visualization of a correlation matrix")
    (description
     "This package provides a graphical display of a correlation matrix or
general matrix.  It also contains some algorithms to do matrix reordering.  In
addition, corrplot is good at details, including choosing color, text labels,
color labels, layout, etc.")
    ;; Any version of the GPL
    (license license:gpl2+)))

(define-public r-stringdist
  (package
    (name "r-stringdist")
    (version "0.9.4.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "stringdist" version))
       (sha256
        (base32
         "0j5b7hr53nr9xq44famkpwzi4khix93ffani5pwldqkf8bpdvhci"))))
    (build-system r-build-system)
    (home-page "https://github.com/markvanderloo/stringdist")
    (synopsis "Approximate string matching and string distance functions")
    (description
     "This package implements an approximate string matching version of R's
native @code{match} function.  It can calculate various string distances based
on edits (Damerau-Levenshtein, Hamming, Levenshtein, optimal sting alignment),
qgrams (q- gram, cosine, jaccard distance) or heuristic metrics (Jaro,
Jaro-Winkler).  An implementation of soundex is provided as well.  Distances
can be computed between character vectors while taking proper care of encoding
or between integer vectors representing generic sequences.")
    (license license:gpl3+)))

(define-public r-mice
  (package
    (name "r-mice")
    (version "2.46.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "mice" version))
       (sha256
        (base32
         "1gjvlk67zvgipfczsca8zqk97vg3sivv82hblsdwp14s7smhjcax"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-lattice" ,r-lattice)
       ("r-mass" ,r-mass)
       ("r-nnet" ,r-nnet)
       ("r-rcpp" ,r-rcpp)
       ("r-rpart" ,r-rpart)
       ("r-survival" ,r-survival)))
    (home-page "https://cran.r-project.org/web/packages/mice/")
    (synopsis "Multivariate imputation by chained equations")
    (description
     "Multiple imputation using @dfn{Fully Conditional Specification} (FCS)
implemented by the MICE algorithm as described in @url{Van Buuren and
Groothuis-Oudshoorn (2011), http://doi.org/10.18637/jss.v045.i03}.  Each
variable has its own imputation model.  Built-in imputation models are
provided for continuous data (predictive mean matching, normal), binary
data (logistic regression), unordered categorical data (polytomous logistic
regression) and ordered categorical data (proportional odds).  MICE can also
impute continuous two-level data (normal model, pan, second-level variables).
Passive imputation can be used to maintain consistency between variables.
Various diagnostic plots are available to inspect the quality of the
imputations.")
    ;; Any of these two versions.
    (license (list license:gpl2 license:gpl3))))

(define-public r-truncnorm
  (package
    (name "r-truncnorm")
    (version "1.0-7")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "truncnorm" version))
       (sha256
        (base32
         "1qac05z50618y4bw1d7yznsli1bv82s0g8h37iacrjrdkv87bmy7"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/truncnorm/")
    (synopsis "Truncated normal distribution")
    (description "This package provides functions for the truncated normal
distribution with mean equal to @code{mean} and standard deviation equal to
@code{sd}.  It includes density, distribution, quantile, and expected value
functions, as well as a random generation function.")
    (license license:gpl2)))

(define-public r-rsolnp
  (package
    (name "r-rsolnp")
    (version "1.16")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Rsolnp" version))
       (sha256
        (base32
         "0w7nkj6igr0gi7r7jg950lsx7dj6aipgxi6vbjsf5f5yc9h7fhii"))))
    (properties `((upstream-name . "Rsolnp")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-truncnorm" ,r-truncnorm)))
    (home-page "https://cran.r-project.org/web/packages/Rsolnp/")
    (synopsis "General non-linear optimization")
    (description "The Rsolnp package implements a general non-linear augmented
Lagrange multiplier method solver, a @dfn{sequential quadratic
programming} (SQP) based solver).")
    ;; Any version of the GPL.
    (license license:gpl2+)))

(define-public r-hardyweinberg
  (package
    (name "r-hardyweinberg")
    (version "1.5.9")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "HardyWeinberg" version))
       (sha256
        (base32
         "0qk3lly5qczn61rj0q9xzscppspvk238yjgr4p71pkzkjhiv40jz"))))
    (properties `((upstream-name . "HardyWeinberg")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-mice" ,r-mice)
       ("r-rcpp" ,r-rcpp)
       ("r-rsolnp" ,r-rsolnp)))
    (home-page "https://cran.r-project.org/package=HardyWeinberg")
    (synopsis "Statistical tests and graphics for Hardy-Weinberg equilibrium")
    (description
     "This package contains tools for exploring Hardy-Weinberg equilibrium for
diallelic genetic marker data.  All classical tests (chi-square, exact,
likelihood-ratio and permutation tests) for Hardy-Weinberg equilibrium are
included in the package, as well as functions for power computation and for
the simulation of marker data under equilibrium and disequilibrium.  Routines
for dealing with markers on the X-chromosome are included.  Functions for
testing equilibrium in the presence of missing data by using multiple
imputation are also provided.  Implements several graphics for exploring the
equilibrium status of a large set of diallelic markers: ternary plots with
acceptance regions, log-ratio plots and Q-Q plots.")
    (license license:gpl2+)))

(define-public r-sm
  (package
    (name "r-sm")
    (version "2.2-5.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "sm" version))
       (sha256
        (base32
         "0hnq5s2fv94gaj0nyqc1vjdjd64vsp9z23nqa8hxvjcaf996rwj9"))))
    (build-system r-build-system)
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "http://www.stats.gla.ac.uk/~adrian/sm/")
    (synopsis "Smoothing methods for nonparametric regression and density estimation")
    (description
     "This is software accompanying the book 'Applied Smoothing Techniques for
Data Analysis---The Kernel Approach with S-Plus Illustrations', Oxford
University Press.  It provides smoothing methods for nonparametric regression
and density estimation")
    (license license:gpl2+)))

(define-public r-vioplot
  (package
    (name "r-vioplot")
    (version "0.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "vioplot" version))
       (sha256
        (base32
         "16wkb26kv6qr34hv5zgqmgq6zzgysg9i78pvy2c097lr60v087v0"))))
    (build-system r-build-system)
    (propagated-inputs `(("r-sm" ,r-sm)))
    (home-page "http://wsopuppenkiste.wiso.uni-goettingen.de/~dadler")
    (synopsis "Violin plot")
    (description
     "This package provides a violin plot, which is a combination of a box
plot and a kernel density plot.")
    (license license:bsd-3)))

(define-public r-rsofia
  (package
    (name "r-rsofia")
    (version "1.1")
    (source (origin
              (method url-fetch)
              ;; This package has been removed from CRAN, so we can
              ;; only fetch it from the archives.
              (uri (string-append "https://cran.r-project.org/src/"
                                  "contrib/Archive/RSofia/RSofia_"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0q931y9rcf6slb0s2lsxhgqrzy4yqwh8hb1124nxg0bjbxvjbihn"))))
    (properties `((upstream-name . "RSofia")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rcpp" ,r-rcpp)))
    (home-page "https://cran.r-project.org/src/contrib/Archive/RSofia")
    (synopsis "Port of sofia-ml to R")
    (description "This package is a port of sofia-ml to R.  Sofia-ml is a
suite of fast incremental algorithms for machine learning that can be used for
training models for classification or ranking.")
    (license license:asl2.0)))

(define-public r-xts
  (package
    (name "r-xts")
    (version "0.10-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "xts" version))
       (sha256
        (base32
         "1j9ddh9v9081zrqlpylsk13fldilhjdyaw6r6pa726fhgnrsf8a2"))))
    (build-system r-build-system)
    (propagated-inputs `(("r-zoo" ,r-zoo)))
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "https://github.com/joshuaulrich/xts")
    (synopsis "Extensible time series")
    (description
     "This package provides for uniform handling of R's different time-based
data classes by extending @code{zoo}, maximizing native format information
preservation and allowing for user-level customization and extension, while
simplifying cross-class interoperability.")
    (license license:gpl2+)))

(define-public r-performanceanalytics
  (package
    (name "r-performanceanalytics")
    (version "1.4.3541")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "PerformanceAnalytics" version))
       (sha256
        (base32
         "1czchsccsbdfjw743j6rm101q2q01pggyl8zmlva213pwm86zb3v"))))
    (properties
     `((upstream-name . "PerformanceAnalytics")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-xts" ,r-xts)
       ("r-zoo" ,r-zoo)))
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "http://r-forge.r-project.org/projects/returnanalytics/")
    (synopsis "Econometric tools for performance and risk analysis")
    (description "This is a collection of econometric functions for
performance and risk analysis.  This package aims to aid practitioners and
researchers in utilizing the latest research in analysis of non-normal return
streams.  In general, it is most tested on return (rather than price) data on
a regular scale, but most functions will work with irregular return data as
well, and increasing numbers of functions will work with P&L or price data
where possible.")
    ;; Either version may be picked.
    (license (list license:gpl2 license:gpl3))))

(define-public r-laeken
  (package
    (name "r-laeken")
    (version "0.4.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "laeken" version))
       (sha256
        (base32
         "1rhkv1kk508pwln1d325iq4fink2ncssps0ypxi52j9d7wk78la6"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-boot" ,r-boot)
       ("r-mass" ,r-mass)))
    (home-page "https://cran.r-project.org/web/packages/laeken/")
    (synopsis "Estimation of indicators on social exclusion and poverty")
    (description "This package provides tools for the estimation of indicators
on social exclusion and poverty, as well as an implementation of Pareto tail
modeling for empirical income distributions.")
    (license license:gpl2+)))

(define-public r-vcd
  (package
    (name "r-vcd")
    (version "1.4-4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "vcd" version))
       (sha256
        (base32
         "1lp99h0wvsc61l1dgcqjxdrcgpgw88ak430cdsv43kmm43qssqd5"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-colorspace" ,r-colorspace)
       ("r-lmtest" ,r-lmtest)
       ("r-mass" ,r-mass)))
    (home-page "https://cran.r-project.org/web/packages/vcd/")
    (synopsis "Visualizing categorical data")
    (description "This package provides visualization techniques, data sets,
summary and inference procedures aimed particularly at categorical data.
Special emphasis is given to highly extensible grid graphics.  The package was
originally inspired by the book \"Visualizing Categorical Data\" by Michael
Friendly and is now the main support package for a new book, \"Discrete Data
Analysis with R\" by Michael Friendly and David Meyer (2015).")
    (license license:gpl2)))

(define-public r-ica
  (package
    (name "r-ica")
    (version "1.0-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ica" version))
       (sha256
        (base32
         "1bkl4a72l0k6gm82l3jxnib898z20cw17zg81jj39l9dn65rlmcq"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/ica/")
    (synopsis "Independent component analysis")
    (description "This package provides tools for @dfn{Independent Component
Analysis} (ICA) using various algorithms: FastICA,
Information-Maximization (Infomax), and @dfn{Joint Approximate Diagonalization
of Eigenmatrices} (JADE).")
    (license license:gpl2+)))

(define-public r-dtw
  (package
    (name "r-dtw")
    (version "1.18-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "dtw" version))
       (sha256
        (base32
         "1b91vahba09cqlb8b1ry4dlv4rbldb4s2p6w52gmyw31vxdv5nnr"))))
    (build-system r-build-system)
    (propagated-inputs `(("r-proxy" ,r-proxy)))
    (home-page "http://dtw.r-forge.r-project.org/")
    (synopsis "Dynamic Time Warping Algorithms")
    (description "This package provides a comprehensive implementation of
@dfn{dynamic time warping} (DTW) algorithms in R.  DTW computes the
optimal (least cumulative distance) alignment between points of two time
series.  Common DTW variants covered include local (slope) and global (window)
constraints, subsequence matches, arbitrary distance definitions,
normalizations, minimum variance matching, and so on.")
    (license license:gpl2+)))

(define-public r-sdmtools
  (package
    (name "r-sdmtools")
    (version "1.1-221")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "SDMTools" version))
       (sha256
        (base32
         "1kacrpamshv7wz83yn45sfbw4m9c44xrrngzcklnwx8gcxx2knm6"))))
    (properties `((upstream-name . "SDMTools")))
    (build-system r-build-system)
    (propagated-inputs `(("r-r-utils" ,r-r-utils)))
    (home-page "http://www.rforge.net/SDMTools/")
    (synopsis "Species distribution modelling tools")
    (description "This packages provides a set of tools for post processing
the outcomes of species distribution modeling exercises.  It includes novel
methods for comparing models and tracking changes in distributions through
time.  It further includes methods for visualizing outcomes, selecting
thresholds, calculating measures of accuracy and landscape fragmentation
statistics, etc.")
    (license license:gpl3+)))

(define-public r-scatterplot3d
  (package
    (name "r-scatterplot3d")
    (version "0.3-40")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "scatterplot3d" version))
       (sha256
        (base32
         "0ababcj87kx7860mica9y2ydlhskxmgj9n46crx036cila512jc2"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/scatterplot3d/")
    (synopsis "3D scatter plot")
    (description "This package provides an implementation of scatter plots for
plotting.  a three dimensional point cloud.")
    (license license:gpl2)))

(define-public r-ggridges
  (package
    (name "r-ggridges")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ggridges" version))
       (sha256
        (base32
         "0kfa3icqdalqdg1klxjrhfl7if35d3wnsiyl86bprw5dyqyh3lh3"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-plyr" ,r-plyr)
       ("r-scales" ,r-scales)))
    (home-page "https://github.com/clauswilke/ggridges")
    (synopsis "Ridgeline plots in ggplot2")
    (description
     "Ridgeline plots provide a convenient way of visualizing changes in
distributions over time or space.  This package enables the creation of such
plots in @code{ggplot2}.")
    (license license:gpl2)))

(define-public r-ggjoy
  (package
    (name "r-ggjoy")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ggjoy" version))
       (sha256
        (base32
         "1s24zn7gmk52bvjjhanxby7rxbnc2yfl0nx5nv7x7z0zja8gg7nb"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-ggridges" ,r-ggridges)))
    (home-page "https://github.com/clauswilke/ggjoy")
    (synopsis "Joyplots in ggplot2")
    (description "Joyplots provide a convenient way of visualizing changes in
distributions over time or space.  This package enables the creation of such
plots in @code{ggplot2}.")
    (license license:gpl2)))

(define-public r-cli
  (package
    (name "r-cli")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "cli" version))
       (sha256
        (base32
         "07as3dr7vwx02p3qgzlmxz1dlrd3x3lysrzp222ip9jcjpydp8wg"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-assertthat" ,r-assertthat)
       ("r-crayon" ,r-crayon)))
    (home-page "https://github.com/r-lib/cli#readme")
    (synopsis "Helpers for developing command line interfaces")
    (description "This package provides a suite of tools designed to build
attractive command line interfaces (CLIs).  It includes tools for drawing
rules, boxes, trees, and Unicode symbols with ASCII alternatives.")
    (license license:expat)))
