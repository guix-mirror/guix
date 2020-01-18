;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016, 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2016, 2017, 2018 Roel Janssen <roel@gnu.org>
;;; Copyright © 2017, 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Raoul Bonnal <ilpuccio.febo@gmail.com>
;;; Copyright © 2018 Vijayalakshmi Vedantham <vijimay12@gmail.com>
;;; Copyright © 2018 Sahithi Yarlagadda <sahi@swecha.net>
;;; Copyright © 2018 Sandeep Subramanian <sandeepsubramanian94@gmail.com>
;;; Copyright © 2018 Charlie Ritter <chewzeirta@posteo.net>
;;; Copyright © 2018 Konrad Hinsen <konrad.hinsen@fastmail.net>
;;; Copyright © 2018, 2020 Mădălin Ionel Patrașcu <madalinionel.patrascu@mdc-berlin.de>
;;; Copyright © 2018 Laura Lazzati <laura.lazzati.15@gmail.com>
;;; Copyright © 2018 Leo Famulari <leo@famulari.name>
;;; Copyright © 2018 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018 Eric Brown <brown@fastmail.com>
;;; Copyright © 2018, 2019 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2019 Nicolò Balzarotti <anothersms@gmail.com>
;;; Copyright © 2019 Wiktor Żelazny <wzelazny@vurv.cz>
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
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system r)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages c)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages graph)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages java)
  #:use-module (gnu packages javascript)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xorg))

(define-public r-clipr
  (package
    (name "r-clipr")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "clipr" version))
       (sha256
        (base32
         "1qn2p13d0c1bpqss6mv9hk60980rzhznfqpyaf5x0fy65svy9903"))))
    (build-system r-build-system)
    (home-page "https://github.com/mdlincoln/clipr")
    (synopsis "Read and write from the system clipboard")
    (description
     "This package provides simple utility functions to read from and write to
the system clipboards.")
    (license license:gpl3)))

(define-public r-scales
  (package
    (name "r-scales")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "scales" version))
       (sha256
        (base32 "00rdbfj5mwc3kr8pskidn3n2zkp4ms6cx36xazz54pxw3pysdr0y"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-farver" ,r-farver)
       ("r-labeling" ,r-labeling)
       ("r-lifecycle" ,r-lifecycle)
       ("r-munsell" ,r-munsell)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-r6" ,r-r6)
       ("r-viridislite" ,r-viridislite)))
    (home-page "https://github.com/hadley/scales")
    (synopsis "Scale functions for visualization")
    (description
     "This package provides graphical scales that map data to aesthetics, and
provides methods for automatically determining breaks and labels for axes and
legends.")
    (license license:expat)))

(define-public r-pheatmap
  (package
    (name "r-pheatmap")
    (version "1.0.12")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "pheatmap" version))
       (sha256
        (base32
         "1hdh74az3vyzz6dqa311rhxdm74n46lyr03p862kn80p0kp9d7ap"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-gtable" ,r-gtable)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-scales" ,r-scales)))
    (home-page "https://cran.r-project.org/web/packages/pheatmap")
    (synopsis "Pretty heatmaps")
    (description
     "This package provides an implementation of heatmaps that offers more
control over dimensions and appearance.")
    (license license:gpl2+)))

(define-public r-ellipsis
  (package
    (name "r-ellipsis")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ellipsis" version))
       (sha256
        (base32
         "01z9gq311nzwv3a0sa49jhm5ylqd59srip4vjkrf23hzgb5i9y0b"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rlang" ,r-rlang)))
    (home-page "https://github.com/hadley/ellipsis")
    (synopsis "Tools for working with additional arguments")
    (description
     "In S3 generics, it's useful to take @code{...} so that methods can have
additional arguments.  But this flexibility comes at a cost: misspelled
arguments will be silently ignored.  The @code{ellipsis} package is an
experiment that allows a generic to warn if any arguments passed in @code{...}
are not used.")
    (license license:gpl3)))

(define-public r-grr
  (package
    (name "r-grr")
    (version "0.9.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "grr" version))
       (sha256
        (base32
         "0arbcgrvhkwb5xk4nry1ffg2qj0v8ivhjghdr505ib4357g0c9i9"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/grr")
    (synopsis "Alternative implementations of base R functions")
    (description
     "This package provides alternative implementations of some base R
functions, including @code{sort}, @code{order}, and @code{match}.  The
functions are simplified but can be faster or have other advantages.")
    (license license:gpl3)))

(define-public r-matrix-utils
  (package
    (name "r-matrix-utils")
    (version "0.9.8")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Matrix.utils" version))
       (sha256
        (base32
         "0a5fq1scykqk0kc9j051j6fix6j2dqwz5wbgb0amaxsiywz9vigb"))))
    (properties `((upstream-name . "Matrix.utils")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-grr" ,r-grr)
       ("r-matrix" ,r-matrix)))
    (home-page "https://github.com/cvarrichio/Matrix.utils")
    (synopsis
     "Data.frame-Like Operations on Sparse and Dense Matrix Objects")
    (description
     "This package implements data manipulation methods such as @code{cast},
@code{aggregate}, and @code{merge}/@code{join} for Matrix and Matrix-like
objects.")
    (license license:gpl3)))

(define-public r-sys
  (package
    (name "r-sys")
    (version "3.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "sys" version))
       (sha256
        (base32
         "14wvy46i2iz9jn7lj3cvifmps932s3395wq681hniva0f8m7q8d6"))))
    (build-system r-build-system)
    (home-page "https://github.com/jeroen/sys")
    (synopsis "Powerful and reliable tools for running system commands in R")
    (description
     "This package provides drop-in replacements for the base @code{system2()}
function with fine control and consistent behavior across platforms.  It
supports clean interruption, timeout, background tasks, and streaming STDIN /
STDOUT / STDERR over binary or text connections.  The package also provides
functions for evaluating expressions inside a temporary fork.  Such
evaluations have no side effects on the main R process, and support reliable
interrupts and timeouts.  This provides the basis for a sandboxing
mechanism.")
    (license license:expat)))

(define-public r-askpass
  (package
    (name "r-askpass")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "askpass" version))
       (sha256
        (base32
         "07q0ik8jzk44vpwh48rr3fnpd7dzsdhjjsl4l850rffv3dyq4h6v"))))
    (build-system r-build-system)
    (propagated-inputs `(("r-sys" ,r-sys)))
    (home-page "https://github.com/jeroen/askpass")
    (synopsis "Safe password entry for R")
    (description
     "This package provides cross-platform utilities for prompting the user
for credentials or a passphrase, for example to authenticate with a server or
read a protected key.")
    (license license:expat)))

(define-public r-vegan
  (package
    (name "r-vegan")
    (version "2.5-6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "vegan" version))
       (sha256
        (base32
         "0g60rgn1i7wqf9pf5m1yki1m45gcp7i5hmjic0ci0f6vng70mh5k"))))
    (build-system r-build-system)
    (native-inputs
     `(("gfortran" ,gfortran)))
    (propagated-inputs
     `(("r-cluster" ,r-cluster)
       ("r-knitr" ,r-knitr)             ; needed for vignettes
       ("r-lattice" ,r-lattice)
       ("r-mass" ,r-mass)
       ("r-mgcv" ,r-mgcv)
       ("r-permute" ,r-permute)))
    (home-page "https://cran.r-project.org/web/packages/vegan")
    (synopsis "Functions for community ecology")
    (description
     "The vegan package provides tools for descriptive community ecology.  It
has most basic functions of diversity analysis, community ordination and
dissimilarity analysis.  Most of its multivariate tools can be used for other
data types as well.")
    (license license:gpl2+)))

(define-public r-tidyverse
  (package
    (name "r-tidyverse")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "tidyverse" version))
       (sha256
        (base32
         "02gyys08qv2v4cl2d66gml4d31ipxay0iyfwwksvxyclx60wp2kd"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-broom" ,r-broom)
       ("r-cli" ,r-cli)
       ("r-crayon" ,r-crayon)
       ("r-dbplyr" ,r-dbplyr)
       ("r-dplyr" ,r-dplyr)
       ("r-forcats" ,r-forcats)
       ("r-ggplot2" ,r-ggplot2)
       ("r-haven" ,r-haven)
       ("r-hms" ,r-hms)
       ("r-httr" ,r-httr)
       ("r-jsonlite" ,r-jsonlite)
       ("r-lubridate" ,r-lubridate)
       ("r-magrittr" ,r-magrittr)
       ("r-modelr" ,r-modelr)
       ("r-pillar" ,r-pillar)
       ("r-purrr" ,r-purrr)
       ("r-readr" ,r-readr)
       ("r-readxl" ,r-readxl)
       ("r-reprex" ,r-reprex)
       ("r-rlang" ,r-rlang)
       ("r-rstudioapi" ,r-rstudioapi)
       ("r-rvest" ,r-rvest)
       ("r-stringr" ,r-stringr)
       ("r-tibble" ,r-tibble)
       ("r-tidyr" ,r-tidyr)
       ("r-xml2" ,r-xml2)))
    (home-page "https://tidyverse.tidyverse.org")
    (synopsis "Install and load packages from the \"Tidyverse\"")
    (description
     "The @code{tidyverse} is a set of packages that work in harmony because
they share common data representations and API design.  This package is
designed to make it easy to install and load multiple tidyverse packages in a
single step.")
    (license license:gpl3)))

(define-public r-rvest
  (package
    (name "r-rvest")
    (version "0.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rvest" version))
       (sha256
        (base32 "0r0a5jic09xw5pk0x42pr99r3zab5m9s4x85ymx1sl769jz42zqf"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-httr" ,r-httr)
       ("r-magrittr" ,r-magrittr)
       ("r-selectr" ,r-selectr)
       ("r-xml2" ,r-xml2)))
    (home-page "https://github.com/hadley/rvest")
    (synopsis "Simple web scraping for R")
    (description
     "@code{r-rvest} helps you scrape information from web pages.  It is
designed to work with @code{magrittr} to make it easy to express common web
scraping tasks, inspired by libraries like @code{BeautifulSoup}.")
    (license license:gpl3)))

(define-public r-selectr
  (package
    (name "r-selectr")
    (version "0.4-2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "selectr" version))
       (sha256
        (base32 "09y1n3iy297g49xlpl7xrjpwgnm57pskx5991lyfcpizbz8ax22m"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-stringr" ,r-stringr)
       ("r-r6" ,r-r6)))
    (home-page "https://sjp.co.nz/projects/selectr/")
    (synopsis "Translate CSS selectors to XPath expressions")
    (description
     "@code{r-selectr} translates a CSS3 selector into an equivalent XPath
expression.  This allows you to use CSS selectors when working with the XML
package as it can only evaluate XPath expressions.  Also provided are
convenience functions useful for using CSS selectors on XML nodes.  This
package is a port of the Python package @code{cssselect}.")
    (license license:bsd-3)))

(define-public r-reprex
  (package
    (name "r-reprex")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "reprex" version))
       (sha256
        (base32
         "0v7vxzs8alwz8y1cjicpimp5yimf1g9gb8x5wy3zhvrz6kk2lg10"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-callr" ,r-callr)
       ("r-clipr" ,r-clipr)
       ("r-fs" ,r-fs)
       ("r-rlang" ,r-rlang)
       ("r-rmarkdown" ,r-rmarkdown)
       ("r-whisker" ,r-whisker)
       ("r-withr" ,r-withr)))
    (home-page "https://github.com/tidyverse/reprex")
    (synopsis "Prepare reproducible R code examples for sharing")
    (description
     "This package provides a convenience wrapper that uses the
@code{rmarkdown} package to render small snippets of code to target formats
that include both code and output.  The goal is to encourage the sharing of
small, reproducible, and runnable examples on code-oriented websites or email.
@code{reprex} also extracts clean, runnable R code from various common formats,
such as copy/paste from an R session.")
    (license license:expat)))

(define-public r-callr
  (package
    (name "r-callr")
    (version "3.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "callr" version))
       (sha256
        (base32
         "0bdlp0labwyfl36jqslj2g7zmw7zwr58v9gam435kiblhjimb8fc"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-r6" ,r-r6)
       ("r-processx" ,r-processx)))
    (home-page "https://github.com/r-lib/callr#readme")
    (synopsis "Call R from R")
    (description
     "It is sometimes useful to perform a computation in a separate R process,
without affecting the current R process at all.  This package does exactly
that.")
    (license license:expat)))

(define-public r-readxl
  (package
    (name "r-readxl")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "readxl" version))
       (sha256
        (base32
         "15mambxr8c7k2ikdfsl1w3vxvm54dsnk0cl1qvks6iig7rql3d14"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-cellranger" ,r-cellranger)
       ("r-progress" ,r-progress)
       ("r-rcpp" ,r-rcpp)
       ("r-tibble" ,r-tibble)))
    (home-page "https://readxl.tidyverse.org")
    (synopsis "Read Excel files")
    (description
     "This package lets you import Excel files into R.  It supports
@file{.xls} via the embedded @code{libxls} C library and @file{.xlsx} via
the embedded @code{RapidXML} C++ library.")
    ;; XXX: This package bundles a copy of 'libxsl' which is BSD-2 and
    ;; 'rapidxml' which is Boost.
    (license (list license:gpl3 license:bsd-2 license:boost1.0))))

(define-public r-modelr
  (package
    (name "r-modelr")
    (version "0.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "modelr" version))
       (sha256
        (base32
         "1x2m34m4qirb401krmgc5wg3g7ndbcglfab2l0655rmky3fz7rfp"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-broom" ,r-broom)
       ("r-dplyr" ,r-dplyr)
       ("r-magrittr" ,r-magrittr)
       ("r-purrr" ,r-purrr)
       ("r-rlang" ,r-rlang)
       ("r-tibble" ,r-tibble)
       ("r-tidyr" ,r-tidyr)
       ("r-tidyselect" ,r-tidyselect)))
    (home-page "https://github.com/tidyverse/modelr")
    (synopsis "Helper functions for modelling in pipelines")
    (description
     "Functions for modelling that help you seamlessly integrate modelling
into a pipeline of data manipulation and visualisation.")
    (license license:gpl3)))

(define-public r-httpuv
  (package
    (name "r-httpuv")
    (version "1.5.2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "httpuv" version))
              (sha256
               (base32
                "13ax0hs2lc39ilznh1zarwqdzahcbhb8adilrfik3xg0fkljpcwk"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bh" ,r-bh)
       ("r-later" ,r-later)
       ("r-promises" ,r-promises)
       ("r-r6" ,r-r6)
       ("r-rcpp" ,r-rcpp)))
    (home-page "https://github.com/rstudio/httpuv")
    (synopsis "HTTP and WebSocket server library for R")
    (description
     "The httpuv package provides low-level socket and protocol support for
handling HTTP and WebSocket requests directly from within R.  It is primarily
intended as a building block for other packages, rather than making it
particularly easy to create complete web applications using httpuv alone.")
    ;; This package includes third-party code that was originally released
    ;; under various non-copyleft licenses.  Full licensing information can be
    ;; obtained here: https://github.com/rstudio/httpuv/blob/master/LICENSE
    (license license:gpl3+)))

(define-public r-jsonlite
  (package
    (name "r-jsonlite")
    (version "1.6.1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "jsonlite" version))
              (sha256
               (base32
                "0xrki07wc17bkmhz0h7jay784j1sls2i9bf1mfpj6yl59791v4kl"))))
    (build-system r-build-system)
    (home-page "https://arxiv.org/abs/1403.2805")
    (synopsis "Robust, high performance JSON parser and generator for R")
    (description
     "The jsonlite package provides a fast JSON parser and generator optimized
for statistical data and the web.  It offers flexible, robust, high
performance tools for working with JSON in R and is particularly powerful for
building pipelines and interacting with a web API.  In addition to converting
JSON data from/to R objects, jsonlite contains functions to stream, validate,
and prettify JSON data.  The unit tests included with the package verify that
all edge cases are encoded and decoded consistently for use with dynamic data
in systems and applications.")
    (license license:expat)))

(define-public r-servr
  (package
    (name "r-servr")
    (version "0.16")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "servr" version))
              (sha256
               (base32
                "106skz04iq4dkblr17idxsxfcfqic6rcaz8mahydkwjjppnhp5fc"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-httpuv" ,r-httpuv)
       ("r-jsonlite" ,r-jsonlite)
       ("r-mime" ,r-mime)
       ("r-xfun" ,r-xfun)))
    (home-page "https://github.com/yihui/servr")
    (synopsis "Simple HTTP server to serve static files or dynamic documents")
    (description
     "Servr provides an HTTP server in R to serve static files, or dynamic
documents that can be converted to HTML files (e.g., R Markdown) under a given
directory.")
    (license license:expat)))

(define-public r-htmltools
  (package
    (name "r-htmltools")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "htmltools" version))
              (sha256
               (base32
                "06l17d8jkf438yk2mchpsp4j90bynnapz3nabh5vkcc324p5a62v"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-digest" ,r-digest)
       ("r-rcpp" ,r-rcpp)
       ("r-rlang" ,r-rlang)))
    (home-page "https://cran.r-project.org/web/packages/htmltools")
    (synopsis "R tools for HTML")
    (description
     "This package provides tools for HTML generation and output in R.")
    (license license:expat)))

(define-public r-htmlwidgets
  (package
    (name "r-htmlwidgets")
    (version "1.5.1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "htmlwidgets" version))
              (sha256
               (base32
                "10fp306l1nybkah6jrlrqwwdb6zvklbddp8i3w9v9naj8la5jbnl"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-htmltools" ,r-htmltools)
       ("r-jsonlite" ,r-jsonlite)
       ("r-yaml" ,r-yaml)))
    (home-page "https://github.com/ramnathv/htmlwidgets")
    (synopsis "HTML Widgets for R")
    (description
     "HTML widgets is a framework for creating HTML widgets that render in
various contexts including the R console, R Markdown documents, and Shiny web
applications.")
    (license license:expat)))

(define-public r-htmltable
  (package
    (name "r-htmltable")
    (version "1.13.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "htmlTable" version))
       (sha256
        (base32
         "0g9r156k9yl1f092hfw3b9wjx11akf0shbi3x0d0mvpnflvc8nfl"))))
    (properties `((upstream-name . "htmlTable")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-checkmate" ,r-checkmate)
       ("r-htmltools" ,r-htmltools)
       ("r-htmlwidgets" ,r-htmlwidgets)
       ("r-knitr" ,r-knitr)
       ("r-magrittr" ,r-magrittr)
       ("r-rstudioapi" ,r-rstudioapi)
       ("r-stringr" ,r-stringr)))
    (home-page "http://gforge.se/packages/")
    (synopsis "Advanced tables for Markdown/HTML")
    (description
     "This package provides functions to build tables with advanced layout
elements such as row spanners, column spanners, table spanners, zebra
striping, and more.  While allowing advanced layout, the underlying
CSS-structure is simple in order to maximize compatibility with word
processors such as LibreOffice.  The package also contains a few text
formatting functions that help outputting text compatible with HTML or
LaTeX.")
    (license license:gpl3+)))

(define-public r-curl
  (package
    (name "r-curl")
    (version "4.3")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "curl" version))
              (sha256
               (base32
                "1nrf6md41b37j424y6rvifdj9zb3j14f60fj7q71k9jhpf2x81kl"))))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; The environment variable CURL_CA_BUNDLE is only respected when
         ;; running Windows, so we disable the platform checks.
         ;; This can be removed once the libcurl has been patched.
         (add-after 'unpack 'allow-CURL_CA_BUNDLE
           (lambda _
             (substitute* "R/onload.R"
               (("if \\(!grepl\\(\"mingw\".*")
                "if (FALSE)\n"))
             (substitute* "src/handle.c"
               (("#ifdef _WIN32") "#if 1"))
             #t)))))
    (inputs
     `(("libcurl" ,curl)
       ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://github.com/jeroenooms/curl")
    (synopsis "HTTP client for R")
    (description
     "The @code{curl()} and @code{curl_download()} functions provide highly
configurable drop-in replacements for base @code{url()} and
@code{download.file()} with better performance, support for encryption, gzip
compression, authentication, and other @code{libcurl} goodies.  The core of
the package implements a framework for performing fully customized requests
where data can be processed either in memory, on disk, or streaming via the
callback or connection interfaces.")
    (license license:expat)))

(define-public r-hwriter
  (package
    (name "r-hwriter")
    (version "1.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "hwriter" version))
       (sha256
        (base32
         "0arjsz854rfkfqhgvpqbm9lfni97dcjs66isdsfvwfd2wz932dbb"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/hwriter")
    (synopsis "Output R objects in HTML format")
    (description
     "This package provides easy-to-use and versatile functions to output R
objects in HTML format.")
    (license license:lgpl2.1+)))

(define-public r-rjson
  (package
    (name "r-rjson")
    (version "0.2.20")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rjson" version))
       (sha256
        (base32
         "0v1zvdd3svnavklh7y5xbwrrkbvx6053r4c5hgnk7hz7bqg7qa1s"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/rjson")
    (synopsis "JSON library for R")
    (description
     "This package provides functions to convert R objects into JSON objects
and vice-versa.")
    (license license:gpl2+)))

(define-public r-fastmap
  (package
    (name "r-fastmap")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "fastmap" version))
       (sha256
        (base32
         "1v7sp56xiha0bh78g3w92k52p9vkp7ryzpw0z66nyddxzrfv0y27"))))
    (properties `((upstream-name . "fastmap")))
    (build-system r-build-system)
    (home-page "https://r-lib.github.io/fastmap/")
    (synopsis "Fast implementation of a key-value store")
    (description
     "This package provides a fast implementation of a key-value store.
Environments are commonly used as key-value stores, but every time a new key
is used, it is added to R's global symbol table, causing a small amount of
memory leakage.  This can be problematic in cases where many different keys
are used.  Fastmap avoids this memory leak issue by implementing the map using
data structures in C++.")
    (license license:expat)))

(define-public r-shiny
  (package
    (name "r-shiny")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rstudio/shiny.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "17ac48g414h9dhi0k4wrah4gyik0q5r0xw3kc01c02qfjwimqsx7"))))
    (build-system r-build-system)
    (arguments
     `(#:modules ((guix build r-build-system)
                  (guix build minify-build-system)
                  (guix build utils)
                  (ice-9 match))
       #:imported-modules (,@%r-build-system-modules
                           (guix build minify-build-system))
       #:phases
       (modify-phases (@ (guix build r-build-system) %standard-phases)
         (add-after 'unpack 'replace-bundled-minified-JavaScript
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((replace-file (lambda (old new)
                                   (format #t "replacing ~a with ~a\n" old new)
                                   (delete-file old)
                                   (symlink new old))))
               ;; NOTE: Files in ./inst/www/shared/datepicker/js/locales/
               ;; contain just data.  They are not minified code, so we don't
               ;; replace them.
               (with-directory-excursion "inst/www/shared"
                 (replace-file "bootstrap/shim/respond.min.js"
                               (string-append (assoc-ref inputs "js-respond")
                                              "/share/javascript/respond.min.js"))
                 (replace-file "bootstrap/shim/html5shiv.min.js"
                               (string-append (assoc-ref inputs "js-html5shiv")
                                              "/share/javascript/html5shiv.min.js"))
                 (replace-file "json2-min.js"
                               (string-append (assoc-ref inputs "js-json2")
                                              "/share/javascript/json2-min.js"))
                 (replace-file "strftime/strftime-min.js"
                               (string-append (assoc-ref inputs "js-strftime")
                                              "/share/javascript/strftime.min.js"))
                 (replace-file "highlight/highlight.pack.js"
                               (string-append (assoc-ref inputs "js-highlight")
                                              "/share/javascript/highlight.min.js"))
                 (replace-file "datatables/js/jquery.dataTables.min.js"
                               (string-append (assoc-ref inputs "js-datatables")
                                              "/share/javascript/jquery.dataTables.min.js"))
                 (replace-file "selectize/js/selectize.min.js"
                               (string-append (assoc-ref inputs "js-selectize")
                                              "/share/javascript/selectize.min.js"))
                 (replace-file "selectize/js/es5-shim.min.js"
                               (string-append (assoc-ref inputs "js-es5-shim")
                                              "/share/javascript/es5-shim.min.js"))
                 (for-each (match-lambda
                             ((source . target)
                              (delete-file target)
                              (minify source #:target target)))
                           '(("jqueryui/jquery-ui.js" .
                              "jqueryui/jquery-ui.min.js")
                             ("datepicker/js/bootstrap-datepicker.js" .
                              "datepicker/js/bootstrap-datepicker.min.js")
                             ("ionrangeslider/js/ion.rangeSlider.js" .
                              "ionrangeslider/js/ion.rangeSlider.min.js")
                             ("bootstrap/js/bootstrap.js" .
                              "bootstrap/js/bootstrap.min.js")
                             ("shiny.js" .
                              "shiny.min.js")
                             ("jquery.js" .
                              "jquery.min.js")
                             ("legacy/jquery.js" .
                              "legacy/jquery.min.js")
                             ("showdown/src/showdown.js" .
                              "showdown/compressed/showdown.js")))))
             #t)))))
    (propagated-inputs
     `(("r-crayon" ,r-crayon)
       ("r-fastmap" ,r-fastmap)
       ("r-httpuv" ,r-httpuv)
       ("r-mime" ,r-mime)
       ("r-jsonlite" ,r-jsonlite)
       ("r-xtable" ,r-xtable)
       ("r-digest" ,r-digest)
       ("r-htmltools" ,r-htmltools)
       ("r-r6" ,r-r6)
       ("r-sourcetools" ,r-sourcetools)))
    (inputs
     `(("js-datatables" ,js-datatables)
       ("js-html5shiv" ,js-html5shiv)
       ("js-json2" ,js-json2)
       ("js-respond" ,js-respond)
       ("js-selectize" ,js-selectize)
       ("js-strftime" ,js-strftime)
       ("js-highlight" ,js-highlight)
       ("js-es5-shim" ,js-es5-shim)))
    (native-inputs
     `(("uglify-js" ,uglify-js)))
    (home-page "http://shiny.rstudio.com")
    (synopsis "Easy interactive web applications with R")
    (description
     "Makes it incredibly easy to build interactive web applications
with R.  Automatic \"reactive\" binding between inputs and outputs and
extensive prebuilt widgets make it possible to build beautiful,
responsive, and powerful applications with minimal effort.")
    (license license:artistic2.0)))

;; This package includes minified JavaScript files.  When upgrading please
;; check that there are no new minified JavaScript files.
(define-public r-shinytree
  (package
    (name "r-shinytree")
    (version "0.2.7")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "shinyTree" version))
       (sha256
        (base32
         "0jfx2capckv7hf2yx3fn8i4rcmhi222ah91jnmhg497x8wgz31s3"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Delete minified JavaScript
           (for-each delete-file
                     '("inst/www/jsTree-3.3.7/libs/require.js"
                       "inst/www/jsTree-3.3.7/libs/jquery.js"
                       "inst/www/jsTree-3.3.7/jstree.min.js"))
           #t))))
    (properties `((upstream-name . "shinyTree")))
    (build-system r-build-system)
    (arguments
     `(#:modules ((guix build utils)
                  (guix build r-build-system)
                  (srfi srfi-1)
                  (ice-9 popen))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'replace-minified-javascript
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "inst/www/jsTree-3.3.7/"
               (symlink (string-append (assoc-ref inputs "js-requirejs")
                                       "/share/javascript/require.min.js")
                        "libs/require.js")
               (call-with-values
                   (lambda ()
                     (unzip2
                      `((,(assoc-ref inputs "js-jquery")
                         "libs/jquery.js")
                        ("jstree.js"
                         "jstree.min.js"))))
                 (lambda (sources targets)
                   (for-each (lambda (source target)
                               (format #t "Processing ~a --> ~a~%"
                                       source target)
                               (let ((minified (open-pipe* OPEN_READ "uglify-js" source)))
                                 (call-with-output-file target
                                   (lambda (port)
                                     (dump-port minified port)))))
                             sources targets))))
             #t)))))
    (propagated-inputs
     `(("r-htmlwidgets" ,r-htmlwidgets)
       ("r-jsonlite" ,r-jsonlite)
       ("r-promises" ,r-promises)
       ("r-shiny" ,r-shiny)
       ("r-stringr" ,r-stringr)))
    (inputs
     `(("js-requirejs" ,js-requirejs)))
    (native-inputs
     `(("uglify-js" ,uglify-js)
       ("js-jquery"
        ,(origin
           (method url-fetch)
           (uri "https://code.jquery.com/jquery-3.3.1.js")
           (sha256
            (base32
             "1b8zxrp6xwzpw25apn8j4qws0f6sr7qr7h2va5h1mjyfqvn29anq"))))))
    (home-page "https://cran.r-project.org/web/packages/shinyTree/")
    (synopsis "jsTree bindings for Shiny")
    (description
     "This package exposes R bindings to jsTree, a JavaScript library that
supports interactive trees, to enable rich, editable trees in Shiny.")
    (license license:expat)))

(define-public r-shinydashboard
  (package
    (name "r-shinydashboard")
    (version "0.7.1")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "shinydashboard" version))
              (sha256
               (base32
                "0khac8b27q3swdw07kl609hm0fjfjsjv591b388q99mqqr2rk92i"))))
    (build-system r-build-system)
    ;; The directory inst/AdminLTE/ contains a minified JavaScript file.
    ;; Regenerate it from the included sources.
    (arguments
     `(#:modules ((guix build utils)
                  (guix build r-build-system)
                  (ice-9 popen))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'generate-minified-javascript
           (lambda _
             (with-directory-excursion "inst/AdminLTE"
               (delete-file "app.min.js")
               (let ((minified (open-pipe* OPEN_READ "uglify-js" "app.js")))
                 (call-with-output-file "app.min.js"
                   (lambda (port)
                     (dump-port minified port))))))))))
    (propagated-inputs
     `(("r-htmltools" ,r-htmltools)
       ("r-promises" ,r-promises)
       ("r-shiny" ,r-shiny)))
    (native-inputs
     `(("uglify-js" ,uglify-js)))
    (home-page "https://rstudio.github.io/shinydashboard/")
    (synopsis "Create dashboards with shiny")
    (description "This package provides an extension to the Shiny web
application framework for R, making it easy to create attractive dashboards.")
    ;; This package includes software that was released under the Expat
    ;; license, but the whole package is released under GPL version 2 or
    ;; later.
    (license license:gpl2+)))

(define-public r-shinyfiles
  (package
    (name "r-shinyfiles")
    (version "0.7.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "shinyFiles" version))
       (sha256
        (base32 "1143m941hma9hc77c3xcw26c0ygfhn9ii2sbp9wrydxv4gc7mr8a"))))
    (properties `((upstream-name . "shinyFiles")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-fs" ,r-fs)
       ("r-htmltools" ,r-htmltools)
       ("r-jsonlite" ,r-jsonlite)
       ("r-shiny" ,r-shiny)
       ("r-tibble" ,r-tibble)))
    (home-page "https://github.com/thomasp85/shinyFiles")
    (synopsis "Server-side file system viewer for Shiny")
    (description
     "This package provides functionality for client-side navigation of the
server side file system in shiny apps.  In case the app is running locally
this gives the user direct access to the file system without the need to
\"download\" files to a temporary location.  Both file and folder selection as
well as file saving is available.")
    (license license:gpl2+)))

(define-public r-shinythemes
  (package
    (name "r-shinythemes")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "shinythemes" version))
       (sha256
        (base32
         "12miz44n2zxfswnia7p8dirxj3miw0aqn4pkx2111ikz67ax84rf"))))
    (properties `((upstream-name . "shinythemes")))
    (build-system r-build-system)
    (propagated-inputs `(("r-shiny" ,r-shiny)))
    (home-page "https://rstudio.github.io/shinythemes/")
    (synopsis "Themes for Shiny")
    (description
     "This package provides themes for use with Shiny.  It includes several
Bootstrap themes, which are packaged for use with Shiny applications.")
    ;; The package is released under version 3 of the GPL, but it includes
    ;; source files that are covered by the Expat license.  It also includes
    ;; fonts under SIL or the ASL.
    (license (list license:gpl3 license:expat
                   license:silofl1.1 license:asl2.0))))

;; The package sources include minified variants of d3.js and non-minified
;; source code of d3-jetpack.
(define-public r-d3r
  (package
    (name "r-d3r")
    (version "0.8.7")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "d3r" version))
       (sha256
        (base32
         "0xl3im76lp7pd5lhp8jfyqdm4j4zvjrx5a5fl81xv2cf7x3n4f2a"))))
    (build-system r-build-system)
    (arguments
     `(#:modules ((guix build utils)
                  (guix build r-build-system)
                  (srfi srfi-1)
                  (ice-9 popen))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'process-javascript
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "inst/www/d3/"
               (call-with-values
                   (lambda ()
                     (unzip2
                      `((,(assoc-ref inputs "d3.v3.js")
                         "v3/dist/d3.min.js")
                        (,(assoc-ref inputs "d3.v4.js")
                         "v4/dist/d3.min.js")
                        (,(assoc-ref inputs "d3.v5.js")
                         "v5/dist/d3.min.js"))))
                 (lambda (sources targets)
                   (for-each (lambda (source target)
                               (format #t "Processing ~a --> ~a~%"
                                       source target)
                               (delete-file target)
                               (let ((minified (open-pipe* OPEN_READ "uglify-js" source)))
                                 (call-with-output-file target
                                   (lambda (port)
                                     (dump-port minified port)))))
                             sources targets))))
             #t)))))
    (propagated-inputs
     `(("r-dplyr" ,r-dplyr)
       ("r-htmltools" ,r-htmltools)
       ("r-tidyr" ,r-tidyr)))
    (native-inputs
     `(("uglify-js" ,uglify-js)
       ("d3.v3.js"
        ,(origin
           (method url-fetch)
           (uri "https://d3js.org/d3.v3.js")
           (sha256
            (base32
             "1arr7sr08vy7wh0nvip2mi7dpyjw4576vf3bm45rp4g5lc1k1x41"))))
       ("d3.v4.js"
        ,(origin
           (method url-fetch)
           (uri "https://d3js.org/d3.v4.js")
           (sha256
            (base32
             "0y7byf6kcinfz9ac59jxc4v6kppdazmnyqfav0dm4h550fzfqqlg"))))
       ("d3.v5.js"
        ,(origin
           (method url-fetch)
           (uri "https://d3js.org/d3.v5.js")
           (sha256
            (base32
             "0kxvx5pfagxn6nhavdwsdnzyd26g0z5dsfi1pi5dvcmb0c8ipcdn"))))))
    (home-page "https://github.com/timelyportfolio/d3r")
    (synopsis "d3.js utilities for R")
    (description
     "This package provides a suite of functions to help ease the use of the
d3.js visualization library in R.  These helpers include
@code{htmltools::htmlDependency} functions, hierarchy builders, and conversion
tools for @code{partykit}, @code{igraph}, @code{table}, and @code{data.frame}
R objects into the JSON format that the d3.js library expects.")
    (license license:bsd-3)))

;; We use the latest commit here because the last release was in 2016 while
;; the latest commit was in 2018.
(define-public r-sankeyd3
  (let ((commit "fd50a74e29056e0d67d75b4d04de47afb2f932bc")
        (revision "1"))
    (package
      (name "r-sankeyd3")
      (version (git-version "0.3.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/fbreitwieser/sankeyD3.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0jrcnfax321pszbpjdifnkbrgbjr43bjzvlzv1p5a8wskksqwiyx"))))
      (build-system r-build-system)
      (propagated-inputs
       `(("r-d3r" ,r-d3r)
         ("r-htmlwidgets" ,r-htmlwidgets)
         ("r-shiny" ,r-shiny)
         ("r-magrittr" ,r-magrittr)))
      (home-page "https://github.com/fbreitwieser/sankeyD3")
      (synopsis "Sankey network graphs from R")
      (description
       "This package provides an R library to generate Sankey network graphs
in R and Shiny via the D3 visualization library.")
      ;; The R code is licensed under GPLv3+.  It includes the non-minified
      ;; JavaScript source code of d3-sankey, which is released under the
      ;; 3-clause BSD license.
      (license (list license:gpl3+ license:bsd-3)))))

(define-public r-crosstalk
  (package
    (name "r-crosstalk")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "crosstalk" version))
       (sha256
        (base32
         "0lfa89vhrzi7a1rghmygcjr8gzddw35sinb3jx6g49mc9jias7mk"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-htmltools" ,r-htmltools)
       ("r-jsonlite" ,r-jsonlite)
       ("r-lazyeval" ,r-lazyeval)
       ("r-r6" ,r-r6)
       ("r-shiny" ,r-shiny)))
    (home-page "https://rstudio.github.io/crosstalk/")
    (synopsis "Inter-widget interactivity for HTML widgets")
    (description
     "This package provides building blocks for allowing HTML widgets to
communicate with each other, with Shiny or without (i.e.  static @code{.html}
files).  It currently supports linked brushing and filtering.")
    (license license:expat)))

(define-public r-rook
  (package
    (name "r-rook")
    (version "1.1-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Rook" version))
       (sha256
        (base32
         "00s9a0kr9rwxvlq433daxjk4ji8m0w60hjdprf502msw9kxfrx00"))))
    (properties `((upstream-name . "Rook")))
    (build-system r-build-system)
    (propagated-inputs `(("r-brew" ,r-brew)))
    (home-page "https://cran.r-project.org/web/packages/Rook")
    (synopsis "Web server interface for R")
    (description
     "This package contains the Rook specification and convenience software
for building and running Rook applications.  A Rook application is an R
reference class object that implements a @code{call} method or an R closure
that takes exactly one argument, an environment, and returns a list with three
named elements: the @code{status}, the @code{headers}, and the @code{body}.")
    (license license:gpl2)))

(define-public r-miniui
  (package
    (name "r-miniui")
    (version "0.1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "miniUI" version))
       (sha256
        (base32
         "1h5h2sc57h95d6bsgs95l26911g38hvjc1v50bc31xl9689l2as5"))))
    (properties `((upstream-name . "miniUI")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-htmltools" ,r-htmltools)
       ("r-shiny" ,r-shiny)))
    (home-page "https://cran.r-project.org/web/packages/miniUI/")
    (synopsis "Shiny UI widgets for small screens")
    (description
     "This package provides UI widget and layout functions for writing Shiny apps that
work well on small screens.")
    (license license:gpl3)))

(define-public r-feather
  (package
    (name "r-feather")
    (version "0.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "feather" version))
       (sha256
        (base32
         "1gxd0h2m56sjjlzn4dry6s13nddxc4l5i11gsvavaf2dwbahdzsh"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-hms" ,r-hms)
       ("r-rcpp" ,r-rcpp)
       ("r-tibble" ,r-tibble)))
    (home-page "https://github.com/wesm/feather")
    (synopsis "R Bindings to the Feather API")
    (description "Read and write feather files, a lightweight binary columnar
data store designed for maximum speed.")
    (license license:asl2.0)))

(define-public r-maps
  (package
    (name "r-maps")
    (version "3.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "maps" version))
       (sha256
        (base32
         "05i2ppl5z4p8rawgqmy3z4ia05fcblpq1vvrmrkgkkpdlhczx6hr"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/maps")
    (synopsis "Draw geographical maps")
    (description "This package provides an R module for display of maps.
Projection code and larger maps are in separate packages (@code{mapproj} and
@code{mapdata}).")
    (license license:gpl2)))

(define-public r-mapproj
  (package
    (name "r-mapproj")
    (version "1.2.7")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "mapproj" version))
       (sha256
        (base32
         "0nscsfq8md6ri9258xz57c3dj81wdl6kdwf4a9qcrwwbn20i427h"))))
    (build-system r-build-system)
    (propagated-inputs `(("r-maps" ,r-maps)))
    (home-page "https://cran.r-project.org/web/packages/mapproj")
    (synopsis "Map projection in R")
    (description "This package converts latitude/longitude into projected
coordinates.")
    (license (list license:gpl2          ; The R interface
                   (license:non-copyleft ; The C code
                    "https://www.gnu.org/licenses/license-list.en.html#lucent102"
                    "Lucent Public License Version 1.02")))))

(define-public r-rgooglemaps
  (package
    (name "r-rgooglemaps")
    (version "1.4.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "RgoogleMaps" version))
       (sha256
        (base32
         "02v8k0bw70q4qwx4lcdy8p25q7n3ql2ll46lfpqllxa1p26svmfi"))))
    (properties `((upstream-name . "RgoogleMaps")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-png" ,r-png)
       ("r-sp" ,r-sp)))
    (home-page "https://cran.r-project.org/web/packages/RgoogleMaps")
    (synopsis "Use Google Maps in R")
    (description "This package serves two purposes:
@enumerate
@item Provide a comfortable R interface to query the Google server for static
  maps, and
@item Use the map as a background image to overlay plots within R.  This
  requires proper coordinate scaling.
@end enumerate\n")
    (license license:gpl2+)))

(define-public r-geosphere
  (package
    (name "r-geosphere")
    (version "1.5-10")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "geosphere" version))
       (sha256
        (base32
         "15xlgsmn0vwky1l13n6acdz6jn2b2na3gf6x367y3qh1f5w4zkan"))))
    (build-system r-build-system)
    (propagated-inputs `(("r-sp" ,r-sp)))
    (home-page "https://cran.r-project.org/web/packages/geosphere")
    (synopsis "Spherical trigonometry")
    (description "This package computes spherical trigonometry for geographic
applications.  That is, compute distances and related measures for angular
(longitude/latitude) locations.")
    (license license:gpl3+)))

(define-public r-jpeg
  (package
    (name "r-jpeg")
    (version "0.1-8.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "jpeg" version))
       (sha256
        (base32
         "1a8mi70x79a691r40yiw684jkg1mr9n8agkxlcksxcnrdybs9c0x"))))
    (build-system r-build-system)
    (inputs `(("libjpeg" ,libjpeg)))
    (home-page "https://www.rforge.net/jpeg/")
    (synopsis "Read and write JPEG images with R")
    (description "This package provides a way to read, write and display
bitmap images stored in the JPEG format with R.  It can read and write both
files and in-memory raw vectors.")
    (license license:gpl2+)))

(define-public r-ggmap
  (package
    (name "r-ggmap")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ggmap" version))
       (sha256
        (base32
         "13dmzl6z62pzjiffilarkji46vy0sacxa8a7mhrhc3biq3ylzhln"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bitops" ,r-bitops)
       ("r-digest" ,r-digest)
       ("r-dplyr" ,r-dplyr)
       ("r-ggplot2" ,r-ggplot2)
       ("r-glue" ,r-glue)
       ("r-httr" ,r-httr)
       ("r-jpeg" ,r-jpeg)
       ("r-magrittr" ,r-magrittr)
       ("r-plyr" ,r-plyr)
       ("r-png" ,r-png)
       ("r-purrr" ,r-purrr)
       ("r-rgooglemaps" ,r-rgooglemaps)
       ("r-rjson" ,r-rjson)
       ("r-scales" ,r-scales)
       ("r-stringr" ,r-stringr)
       ("r-tibble" ,r-tibble)
       ("r-tidyr" ,r-tidyr)))
    (home-page "https://github.com/dkahle/ggmap")
    (synopsis "Spatial visualization with ggplot2")
    (description "This package provides a collection of functions to visualize
spatial data and models on top of static maps from various online sources (e.g
Google Maps and Stamen Maps).  It includes tools common to those tasks,
including functions for geolocation and routing.")
    (license license:gpl2)))

(define-public r-haven
  (package
    (name "r-haven")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "haven" version))
       (sha256
        (base32
         "0g9d6mxqmrw2zdms78jpx2sx73pczlyy771v1h5hmxqz9sqyk7hr"))))
    (build-system r-build-system)
    (inputs
     `(("zlib" ,zlib)))
    (propagated-inputs
     `(("r-forcats" ,r-forcats)
       ("r-hms" ,r-hms)
       ("r-rcpp" ,r-rcpp)
       ("r-rlang" ,r-rlang)
       ("r-readr" ,r-readr)
       ("r-tibble" ,r-tibble)
       ("r-tidyselect" ,r-tidyselect)))
    (home-page "https://haven.tidyverse.org")
    (synopsis "Import and Export 'SPSS', 'Stata' and 'SAS' Files")
    (description
     "This package lets you mport foreign statistical formats into R via the
embedded @url{https://github.com/WizardMac/ReadStat,ReadStat} C library.")
    (license license:expat)))

(define-public r-amap
  (package
    (name "r-amap")
    (version "0.8-18")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "amap" version))
              (sha256
               (base32
                "0zpcb73w413na23f6giml9311jh0j0y766w2fh9i40d2h7bbvyvs"))))
    (build-system r-build-system)
    (native-inputs
     `(("gfortran" ,gfortran)))
    (home-page "http://mulcyber.toulouse.inra.fr/projects/amap/")
    (synopsis "Another multidimensional analysis package")
    (description "This package provides tools for clustering and principal
component analysis (with robust methods, and parallelized functions).")
    (license license:gpl2+)))

(define-public r-ape
  (package
    (name "r-ape")
    (version "5.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ape" version))
       (sha256
        (base32
         "08wbk1kxhs32bmmvqlqanbdg1w235amd35k8m00fngsj9h9xzc08"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-lattice" ,r-lattice)
       ("r-nlme" ,r-nlme)
       ("r-rcpp" ,r-rcpp)))
    (home-page "http://ape-package.ird.fr/")
    (synopsis "Analyses of phylogenetics and evolution")
    (description
     "This package provides functions for reading, writing, plotting, and
manipulating phylogenetic trees, analyses of comparative data in a
phylogenetic framework, ancestral character analyses, analyses of
diversification and macroevolution, computing distances from DNA sequences,
and several other tools.")
    (license license:gpl2+)))

(define-public r-abbyyr
  (package
    (name "r-abbyyr")
    (version "0.5.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "abbyyR" version))
       (sha256
        (base32
         "1vldnd3dg89aj6a73nhirirqddbfdrnzhb5m3679i60sark8nk6r"))))
    (properties `((upstream-name . "abbyyR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-curl" ,r-curl)
       ("r-httr" ,r-httr)
       ("r-plyr" ,r-plyr)
       ("r-progress" ,r-progress)
       ("r-readr" ,r-readr)
       ("r-xml" ,r-xml)))
    (home-page "https://github.com/soodoku/abbyyR")
    (synopsis "Access to Abbyy Optical Character Recognition (OCR) API")
    (description
     "This package provides tools to get text from images of text using Abbyy
Cloud Optical Character Recognition (OCR) API.  With abbyyyR, one can easily
OCR images, barcodes, forms, documents with machine readable zones, e.g.
passports and get the results in a variety of formats including plain text and
XML.  To learn more about the Abbyy OCR API, see @url{http://ocrsdk.com/}.")
    (license license:expat)))

(define-public r-colorspace
  (package
    (name "r-colorspace")
    (version "1.4-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "colorspace" version))
       (sha256
        (base32 "0wyny3ah2d74hqv80s6imrarpna09gq3j9rjnz6zx2qg0lx72gb9"))))
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
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "glue" version))
       (sha256
        (base32
         "1a1ycg9r3gd91visp49q49rsrdgyf8kr9dxdy3hk99kikn4z5hag"))))
    (build-system r-build-system)
    (home-page "https://github.com/tidyverse/glue")
    (synopsis "Interpreted string literals")
    (description
     "This package provides an implementation of interpreted string literals,
inspired by Python's Literal String Interpolation (PEP-0498) and
Docstrings (PEP-0257) and Julia's Triple-Quoted String Literals.")
    (license license:expat)))

(define-public r-pastecs
  (package
   (name "r-pastecs")
   (version "1.3.21")
   (source (origin
            (method url-fetch)
            (uri (cran-uri "pastecs" version))
            (sha256
             (base32
              "0z4dic94ar646w7zc2ggi5hgvf2qnznsani94c5pyql8zspz47lc"))))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-boot" ,r-boot)))
   (home-page "http://www.sciviews.org/pastecs")
   (synopsis "Analysis of space-time ecological series")
   (description
    "This package provides functions for regulation, decomposition and analysis
of space-time series.  The @code{pastecs} library is a PNEC-Art4 and IFREMER
initiative to bring PASSTEC 2000 functionalities to R.")
   (license license:gpl2+)))

(define-public r-plogr
  (package
    (name "r-plogr")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "plogr" version))
       (sha256
        (base32
         "0a8dhzlna79ggyhfr0nncgh15a9n6r0dsz664pz0ah323wpblqqf"))))
    (build-system r-build-system)
    (home-page "https://github.com/krlmlr/plogr")
    (synopsis "R bindings for the plog C++ logging library")
    (description
     "This package provides the header files for a stripped-down version of
the plog header-only C++ logging library, and a method to log to R's standard
error stream.")
    (license license:expat)))

(define-public r-pls
  (package
    (name "r-pls")
    (version "2.7-2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "pls" version))
       (sha256
        (base32 "121byimd6bg7jbrq5wz5fpi0vxq0vh8g724vkhnjzszbvcv1xsb7"))))
    (build-system r-build-system)
    (home-page "https://mevik.net/work/software/pls.html")
    (synopsis "Partial Least Squares and Principal Component Regression")
    (description
     "The pls package implements multivariate regression methods: Partial Least
Squares Regression (@dfn{PLSR}), Principal Component Regression (@dfn{PCR}), and
Canonical Powered Partial Least Squares (@dfn{CPPLS}).  It supports:

@itemize
@item several algorithms: the traditional orthogonal scores (@dfn{NIPALS}) PLS
algorithm, kernel PLS, wide kernel PLS, Simpls, and PCR through @code{svd}
@item multi-response models (or @dfn{PLS2})
@item flexible cross-validation
@item Jackknife variance estimates of regression coefficients
@item extensive and flexible plots: scores, loadings, predictions, coefficients,
(R)MSEP, R², and correlation loadings
@item formula interface, modelled after @code{lm()}, with methods for predict,
print, summary, plot, update, etc.
@item extraction functions for coefficients, scores, and loadings
@item MSEP, RMSEP, and R² estimates
@item multiplicative scatter correction (@dfn{MSC})
@end itemize\n")
    (license license:gpl2)))

(define-public r-ps
  (package
    (name "r-ps")
    (version "1.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ps" version))
       (sha256
        (base32 "0127q6pw9iw8hhcfp231gmdh29nahh2n5jzc38avrzy7yrm4bwl9"))))
    (build-system r-build-system)
    (home-page "https://ps.r-lib.org")
    (synopsis "List, query, and manipulate system processes")
    (description
     "The ps package implements an API to list, query, and manipulate system
processes.  Most of its code is based on the @code{psutil} Python package.")
    (license license:bsd-3)))

(define-public r-pkgbuild
  (package
    (name "r-pkgbuild")
    (version "1.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "pkgbuild" version))
       (sha256
        (base32 "0xnlz6ivhkbmncg9hfw5p69lm4rjy3wn5lyxmygxyf4rrfnnqwxx"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-callr" ,r-callr)
       ("r-cli" ,r-cli)
       ("r-crayon" ,r-crayon)
       ("r-desc" ,r-desc)
       ("r-prettyunits" ,r-prettyunits)
       ("r-r6" ,r-r6)
       ("r-rprojroot" ,r-rprojroot)
       ("r-withr" ,r-withr)))
    (home-page "https://github.com/r-pkgs/pkgbuild")
    (synopsis "Find tools needed to build R packages")
    (description
     "This package provides functions used to build R packages.  It locates
compilers needed to build R packages on various platforms and ensures the PATH
is configured appropriately so R can use them.")
    (license license:gpl3)))

(define-public r-pkgload
  (package
    (name "r-pkgload")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "pkgload" version))
       (sha256
        (base32
         "0z7jvharafahi2gv5547mk1n499isjzw06kfwymmxc0gd575d1ii"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-desc" ,r-desc)
       ("r-pkgbuild" ,r-pkgbuild)
       ("r-rlang" ,r-rlang)
       ("r-rprojroot" ,r-rprojroot)
       ("r-rstudioapi" ,r-rstudioapi)
       ("r-withr" ,r-withr)))
    (home-page "https://github.com/r-lib/pkgload")
    (synopsis "Simulate package installation and attach")
    (description
     "This package simulates the process of installing a package and then
attaching it.  This is a key part of the @code{devtools} package as it allows
you to rapidly iterate while developing a package.")
    (license license:gpl3)))

(define-public r-rcpp
  (package
    (name "r-rcpp")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Rcpp" version))
       (sha256
        (base32 "03h3zyjq948y0hrrs95lfk4zgx6wfrg64hjlrfrzf5na7bfh0d9b"))))
    (build-system r-build-system)
    (native-inputs
     `(("r-knitr" ,r-knitr))) ; for vignettes
    (home-page "http://www.rcpp.org")
    (synopsis "Seamless R and C++ integration")
    (description
     "The Rcpp package provides R functions as well as C++ classes which offer
a seamless integration of R and C++.  Many R data types and objects can be
mapped back and forth to C++ equivalents which facilitates both writing of new
code as well as easier integration of third-party libraries.  Documentation
about Rcpp is provided by several vignettes included in this package, via the
@code{Rcpp Gallery} site at <http://gallery.rcpp.org>, the paper by Eddelbuettel
and Francois (2011, JSS), and the book by Eddelbuettel (2013, Springer); see
@code{citation(\"Rcpp\")} for details on these last two.")
    (license license:gpl2+)))

(define-public r-bindr
  (package
    (name "r-bindr")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "bindr" version))
       (sha256
        (base32
         "1l05fpk2yql3jka321c0bdgx6mqq9pvfrg2844lbjfpbgjkmqy3w"))))
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
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "bindrcpp" version))
       (sha256
        (base32
         "0rz4ibjdjsxl99ff3ha79z7cnjmilx4rx58fk9kk7ld9xc4hf4s8"))))
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
    (version "1.7.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "calibrate" version))
       (sha256
        (base32 "1s423nr176l2sc66wp7hzgqkv7c2bq8d2bjrrvrrm5qa9y3zdx1k"))))
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
    (version "1.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "shape" version))
       (sha256
        (base32
         "0hadk3mapkhbh8xjkiz52vxdagmmgvm15xwpzb90ikw4giyipjzl"))))
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
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "GlobalOptions" version))
       (sha256
        (base32 "0x89hfz80avq4zcskxl71i4zi0mgniqqxfrvz050aa2189wfyja2"))))
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
    (version "0.4.8")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "circlize" version))
       (sha256
        (base32
         "0jvr9hmxyhg0zx101iiqkrg8wfaj86kp62xpv42n2j9fkn5r1mi2"))))
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
    (version "0.70.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "poweRlaw" version))
       (sha256
        (base32
         "1asr6ikr7hmj78jyg8r1gwvcjg14addkxdiz92nh06lv71a183r4"))))
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
    (version "1.13.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "dendextend" version))
       (sha256
        (base32
         "1pjbz6sb4pgh3d5pm53vmf3q8y6lq3hrgjd6547xxs3m63sb8mn4"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-magrittr" ,r-magrittr)
       ("r-viridis" ,r-viridis)))
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
    (version "0.1.8")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "GetoptLong" version))
       (sha256
        (base32
         "1l8xkvyl152bsyvxazsvx2sm1vkygn75x0lsg3sbg7xp6drdn3kc"))))
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
    (home-page "https://www.rforge.net/fastmatch")
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
    (version "2.2-14")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ff" version))
       (sha256
        (base32
         "1w724q4jpzbvzpilb2ifviaxkjgk9lzwxz9gksnvicbmfa20fqqw"))))
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
    (version "0.12.7")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ffbase" version))
       (sha256
        (base32
         "04kxx2f3f0743c5nvpb7x1x0pcd220dazpd5ag1pidxbz3xa85nw"))))
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
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "prettyunits" version))
       (sha256
        (base32
         "1ibmzgknw5896q2i6r59jz2izblxwgb29ivvjzx50pkd1jl9l6cs"))))
    (build-system r-build-system)
    (home-page "https://github.com/gaborcsardi/prettyunits")
    (synopsis "Pretty, human readable formatting of quantities")
    (description
     "This package provides tools for pretty, human readable formatting of
quantities.")
    (license license:expat)))

(define-public r-reshape
  (package
    (name "r-reshape")
    (version "0.8.8")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "reshape" version))
       (sha256
        (base32
         "0s6i0sqxg1vldxs6miv8mi0zydxbqzgpmzfiwkj8y7jix3yrfmad"))))
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
    (version "1.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "progress" version))
       (sha256
        (base32
         "0dgzb362641aqm8xd88iqa8jmpdm43xs0aba0d5kk6fvapnxi95l"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-crayon" ,r-crayon)
       ("r-hms" ,r-hms)
       ("r-prettyunits" ,r-prettyunits)
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
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "GGally" version))
       (sha256
        (base32
         "1zjmcc5bzagvy7c5cmdcl39xmx07fwi98yrj4i05w7y40kqcsiws"))))
    (properties `((upstream-name . "GGally")))
    (build-system r-build-system)
    (inputs
     `(("libressl" ,libressl)))
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-gtable" ,r-gtable)
       ("r-plyr" ,r-plyr)
       ("r-progress" ,r-progress)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-reshape" ,r-reshape)
       ("r-rlang" ,r-rlang)))
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
    (version "0.4-23")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "proxy" version))
       (sha256
        (base32
         "17b6qfllqrhzrxqgx7dccffgybnkcria5a68ap5ly3plg04ypm4x"))))
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
    (version "1.4-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "sp" version))
       (sha256
        (base32 "0fbh865r4py89g6ln8bslig2vbxxwa642p5k5g02rskyhajg35lg"))))
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
    (version "0.6-9")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "Rmpi" version))
              (sha256
               (base32
                "1rhycla98hxgnnxlxxldr1x51djak7c2jjvlrv3jcsvgwp1ymqdj"))))
    (properties `((upstream-name . "Rmpi")))
    (build-system r-build-system)
    (arguments
     `(#:configure-flags '("--configure-args=\"--with-Rmpi-type=OPENMPI\"")
       #:phases (modify-phases %standard-phases
                  (add-before 'install 'mpi-setup
                    ,%openmpi-setup))))
    (inputs
     `(("openmpi" ,openmpi)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://www.stats.uwo.ca/faculty/yu/Rmpi")
    (synopsis "R interface to message-passing interface (MPI)")
    (description
     "This package provides an interface (wrapper) to MPI APIs.  It also
provides an interactive R manager and worker environment.")
    (license license:gpl2+)))

(define-public r-lmoments
  (package
    (name "r-lmoments")
    (version "1.3-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Lmoments" version))
       (sha256
        (base32
         "0pc63bj9a8hzr5m3yssrc4kin39fffwkl8rggs3sagzr12d4i7bw"))))
    (properties `((upstream-name . "Lmoments")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rcpp" ,r-rcpp)
       ("r-rcpparmadillo" ,r-rcpparmadillo)))
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
    (version "1.0-6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "distillery" version))
       (sha256
        (base32
         "1mi3ig9jq0kd7yrwc5m37lmrw04p1b4lirnbsxi10z3n5yay4429"))))
    (build-system r-build-system)
    (home-page "https://ral.ucar.edu/staff/ericg/")
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
    (version "2.0-11")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "extRemes" version))
       (sha256
        (base32
         "0hmgynxhzswqnhwb2sxrkczgam8c17s3vpxqc5bcz0bwczpxxyvm"))))
    (properties `((upstream-name . "extRemes")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-distillery" ,r-distillery)
       ("r-lmoments" ,r-lmoments)))
    (home-page "https://www.assessment.ucar.edu/toolkit/")
    (synopsis "Extreme value analysis")
    (description
     "ExtRemes is a suite of functions for carrying out analyses on the
extreme values of a process of interest; be they block maxima over long blocks
or excesses over a high threshold.")
    (license license:gpl2+)))

(define-public r-lmtest
  (package
    (name "r-lmtest")
    (version "0.9-37")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "lmtest" version))
       (sha256
        (base32
         "02nasm0j2vwkhz11dxqixs23msy1s3yj0jps6949fmgh9gwjkjfx"))))
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

(define-public r-idr
  (package
    (name "r-idr")
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "idr" version))
              (sha256
               (base32
                "05nvgw1xdg670bsjjrxkgd1mrdkciccpw4krn0zcgdf2r21dzgwb"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/idr/")
    (synopsis "Irreproducible discovery rate")
    (description
     "This is a package for estimating the copula mixture model and plotting
correspondence curves in \"Measuring reproducibility of high-throughput
experiments\" (2011), Annals of Applied Statistics, Vol. 5, No. 3, 1752-1779,
by Li, Brown, Huang, and Bickel")
    (license license:gpl2+)))

(define-public r-inline
  (package
    (name "r-inline")
    (version "0.3.15")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "inline" version))
              (sha256
               (base32
                "0s4wssvpan189fijahknxq5s22ww9bzmdlmyhnra748r7khky17z"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/inline")
    (synopsis "Functions to inline C, C++, Fortran function calls from R")
    (description
     "This package provides functionality to dynamically define R functions
and S4 methods with inlined C, C++ or Fortran code supporting @code{.C} and
@code{.Call} calling conventions.")
    ;; Any version of the LGPL.
    (license license:lgpl3+)))

(define-public r-bdsmatrix
  (package
    (name "r-bdsmatrix")
    (version "1.3-4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "bdsmatrix" version))
       (sha256
        (base32
         "1sh6pg43rgqvips4fx0k4vmp5i9lmniix0bqwj2yq5m06gs227i5"))))
    (properties `((upstream-name . "bdsmatrix")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/bdsmatrix/")
    (synopsis "Routines for block diagonal symmetric matrices")
    (description
     "This package provides procedures to work with block diagonal symmetric
matrices, a special case of sparse matrices.")
    (license license:lgpl2.0)))

(define-public r-bbmle
  (package
    (name "r-bbmle")
    (version "1.0.23.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "bbmle" version))
       (sha256
        (base32
         "0p3l9shbr2846qmw8n0fyzf4j7gmi08aypl82jml3dwh26q1whk0"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bdsmatrix" ,r-bdsmatrix)
       ("r-lattice" ,r-lattice)
       ("r-mass" ,r-mass)
       ("r-matrix" ,r-matrix)
       ("r-mvtnorm" ,r-mvtnorm)
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
    (version "1.3.12")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "emdbook" version))
       (sha256
        (base32
         "0ls3zxxlwmdv7zn1v9i1y9zc2sn0hbgmyjvsj7zn3ajsw7wwlih6"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bbmle" ,r-bbmle)
       ("r-coda" ,r-coda)
       ("r-lattice" ,r-lattice)
       ("r-mass" ,r-mass)
       ("r-plyr" ,r-plyr)))
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
    (version "5.6.15")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "lpSolve" version))
       (sha256
        (base32
         "1fpkyjyqykwa1dxnhiky01pm09syxg169lm7hpy39bdbg10vw9s6"))))
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
    (version "1.5.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "limSolve" version))
       (sha256
        (base32
         "1829rd2cnd8qj80z9a7sgc7gx4sf3kvl5g6d2a0lqqw30f9sjzmr"))))
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
    (version "1.0-14")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "fitdistrplus" version))
       (sha256
        (base32
         "10q08wsv8v3w7797jdvvv60bgrf1bi6438wf0jcqv81ays82a245"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-mass" ,r-mass)
       ("r-npsurv" ,r-npsurv)
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
    (version "1.7-7")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "energy" version))
       (sha256
        (base32
         "13wnx5nwk7nsv7vf5sxhz4y0rxrnzm76ldgywk1bxrz67srqzf37"))))
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
    (version "1.1-9.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "SuppDists" version))
       (sha256
        (base32
         "01j6p94m1g363nph2158fq2rmd6z3h5dvcv6aidh2d6syw131xak"))))
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
    (version "1.2-9")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "kSamples" version))
       (sha256
        (base32
         "1zs22p68d6320kcylisnk0b5wmpapxkyz15py09czxzw7npw8gms"))))
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
    (version "0.2-2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "CVST" version))
       (sha256
        (base32
         "05l3yzkfrbds09ah9cdwn2sn4ryhq78lz33ryzrgkv176jc8qjw5"))))
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

(define-public r-squarem
  (package
    (name "r-squarem")
    (version "2020.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "SQUAREM" version))
       (sha256
        (base32
         "1j6sa93xjvynnzx9jx79a3fysbykmbqyknknsqif5bcha6xp6cvf"))))
    (properties `((upstream-name . "SQUAREM")))
    (build-system r-build-system)
    (home-page "https://coah.jhu.edu/people/Faculty_personal_Pages/Varadhan.html")
    (synopsis "Squared Extrapolation Methods for Accelerating EM-Like Monotone Algorithms")
    (description
     "This package provides algorithms for accelerating the convergence of
slow, monotone sequences from smooth, contraction mapping such as the EM
algorithm.  It can be used to accelerate any smooth, linearly convergent
acceleration scheme.  A tutorial style introduction to this package is
available in a vignette.")
    (license license:gpl2+)))

(define-public r-lava
  (package
    (name "r-lava")
    (version "1.6.7")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "lava" version))
       (sha256
        (base32
         "0ffzxbb8pvfh1m6j61az4ga37snyhylq2941fyc76w7w9i2sixv3"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-numderiv" ,r-numderiv)
       ("r-squarem" ,r-squarem)
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
    (version "0.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "DRR" version))
       (sha256
        (base32
         "1y70si1gig4l7jx5jiqsqliyywfsvimkx53x3zh1lc3yj2j6bqwk"))))
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
    (version "2019.11.13")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "prodlim" version))
       (sha256
        (base32
         "03wvh3kirp1prac5nky6a5whs97rvaf4hc27x0fnh51sa17r42b8"))))
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
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "dimRed" version))
       (sha256
        (base32
         "110d6y83ib1nfpxzmvkvb3fn3brskwkdbsk4dqrdrswrd4znxrg6"))))
    (properties `((upstream-name . "dimRed")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-drr" ,r-drr)
       ("r-magrittr" ,r-magrittr)))
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
    (version "3043.102")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "timeDate" version))
       (sha256
        (base32
         "0wvl5pq261rvbgly7vilk3x3m9xk3ly6il1i5scwdf6srl1vlz1p"))))
    (properties `((upstream-name . "timeDate")))
    (build-system r-build-system)
    (home-page "https://www.rmetrics.org")
    (synopsis "Chronological and calendar objects")
    (description
     "This package provides an environment for teaching \"Financial
Engineering and Computational Finance\" and for managing chronological and
calendar objects.")
    (license license:gpl2+)))

(define-public r-magic
  (package
    (name "r-magic")
    (version "1.5-9")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "magic" version))
       (sha256
        (base32
         "0snmdh6vk0p6ar1swsihisinxrx7l8371dri5lk0z24ysgr5w7gs"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-abind" ,r-abind)))
    (home-page "https://github.com/RobinHankin/magic.git")
    (synopsis "Create and investigate magic squares")
    (description
     "This package provides a collection of efficient, vectorized algorithms
for the creation and investigation of magic squares and hypercubes, including
a variety of functions for the manipulation and analysis of arbitrarily
dimensioned arrays.")
    (license license:gpl2)))

(define-public r-rmysql
  (package
    (name "r-rmysql")
    (version "0.10.19")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "RMySQL" version))
       (sha256
        (base32
         "0269pfq791qvax69lrwvvzgllyzm3cy3sdp9sakwxzvw5pxk2vhv"))))
    (properties `((upstream-name . "RMySQL")))
    (build-system r-build-system)
    (inputs
     `(("mariadb" ,mariadb "lib")
       ("mariadb-dev" ,mariadb "dev")
       ("zlib" ,zlib)))
    (propagated-inputs
     `(("r-dbi" ,r-dbi)))
    (home-page "https://github.com/r-dbi/RMySQL")
    (synopsis "Database interface and MySQL driver for R")
    (description
     "This package provides a DBI interface to MySQL / MariaDB.  The RMySQL
package contains an old implementation based on legacy code from S-PLUS which
is being phased out.  A modern MySQL client based on Rcpp is available from
the RMariaDB package.")
    (license license:gpl2)))

(define-public r-rpostgresql
  (package
    (name "r-rpostgresql")
    (version "0.6-2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "RPostgreSQL" version))
       (sha256
        (base32
         "1mdhw5821v2h7hpa53v10wz53k4i90r0vb6a3dia5gq8f9j1h088"))))
    (properties `((upstream-name . "RPostgreSQL")))
    (build-system r-build-system)
    (inputs
     `(("postgresql" ,postgresql)))
    (propagated-inputs
     `(("r-dbi" ,r-dbi)))
    (home-page "https://github.com/tomoakin/RPostgreSQL")
    (synopsis "R interface to the PostgreSQL database system")
    (description
     "This package provides a Database Interface (DBI) compliant driver for R
to access PostgreSQL database systems.")
    ;; The whole package is released under GPL version 2.  It includes code
    ;; under the PostgreSQL license.
    (license license:gpl2)))

(define-public r-linprog
  (package
    (name "r-linprog")
    (version "0.9-2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "linprog" version))
       (sha256
        (base32
         "1ki14an0pmhs2mnmfjjvdzd76pshiyvi659zf7hqvqwj0viv4dw9"))))
    (build-system r-build-system)
    (propagated-inputs `(("r-lpsolve" ,r-lpsolve)))
    (home-page "http://linprog.r-forge.r-project.org/")
    (synopsis "Linear programming and optimization")
    (description
     "This package can be used to solve Linear Programming / Linear
Optimization problems by using the simplex algorithm.")
    (license license:gpl2+)))

(define-public r-geometry
  (package
    (name "r-geometry")
    (version "0.4.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "geometry" version))
       (sha256
        (base32
         "1n10l8ax3783v3lgaacb15qsn8b3f0wpmhg3k39j31s6ciyd3vcg"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-magic" ,r-magic)
       ("r-linprog" ,r-linprog)
       ("r-lpsolve" ,r-lpsolve)
       ("r-rcpp" ,r-rcpp)
       ("r-rcppprogress" ,r-rcppprogress)))
    (home-page "http://geometry.r-forge.r-project.org/")
    (synopsis "Mesh generation and surface tesselation")
    (description
     "This package makes the qhull library available in R, in a similar manner
as in Octave.  Qhull computes convex hulls, Delaunay triangulations, halfspace
intersections about a point, Voronoi diagrams, furthest-site Delaunay
triangulations, and furthest-site Voronoi diagrams.  It runs in 2-d, 3-d, 4-d,
and higher dimensions.  It implements the Quickhull algorithm for computing
the convex hull.  Qhull does not support constrained Delaunay triangulations,
or mesh generation of non-convex objects, but the package does include some R
functions that allow for this.  Currently the package only gives access to
Delaunay triangulation and convex hull computation.")
    ;; The Qhull sources are included and are distributed under a custom
    ;; non-copyleft license.  The R sources are released under GPL version 2.
    (license (list license:gpl2
                   (license:non-copyleft "http://www.qhull.org/COPYING.txt")))))

(define-public r-ddalpha
  (package
    (name "r-ddalpha")
    (version "1.3.11")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ddalpha" version))
       (sha256
        (base32
         "1sdnb47r534nh138zk3a6b2mgi74nvshc7p5m304vjs9jlx4l2y3"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bh" ,r-bh)
       ("r-class" ,r-class)
       ("r-geometry" ,r-geometry)
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
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "gower" version))
       (sha256
        (base32
         "007ivwn1nagpi26qq8iih1c2l61c53glvv60n90hi341ry8vwgxg"))))
    (build-system r-build-system)
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
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "RcppRoll" version))
       (sha256
        (base32
         "0srzfhzkk42kzrdjnhbb37946jp1p688rgysy6k3i2is8jb21zyb"))))
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
    (version "0.9-9")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ipred" version))
       (sha256
        (base32
         "0vs1hqfx7yd0xdbmfsf2gim7spkni0845cj6gswn0nhdfdq7ma0d"))))
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
    (version "1.9.12.31")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "psych" version))
       (sha256
        (base32
         "02i9p6appf15hjdsi58g39bzs9as40f9qhy8m7ki30hd1fz1vrr5"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-lattice" ,r-lattice)
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

(define-public r-generics
  (package
    (name "r-generics")
    (version "0.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "generics" version))
       (sha256
        (base32
         "0xk1xhpy7gpv3pvaygzhpfdxj72zmb38pb4nscfyg2ff36vx3cvi"))))
    (build-system r-build-system)
    (home-page "https://github.com/r-lib/generics")
    (synopsis "Common S3 generics not provided by base R methods")
    (description
     "In order to reduce potential package dependencies and conflicts,
generics provides a number of commonly used S3 generics that are not provided
by base R methods related to model fitting.")
    (license license:gpl2)))

(define-public r-broom
  (package
    (name "r-broom")
    (version "0.5.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "broom" version))
       (sha256
        (base32
         "0n7zd64263kfavdi28rl2bxrsa00c3m4vjhhjdrfwvvmrcxj39fx"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-backports" ,r-backports)
       ("r-dplyr" ,r-dplyr)
       ("r-generics" ,r-generics)
       ("r-nlme" ,r-nlme)
       ("r-purrr" ,r-purrr)
       ("r-reshape2" ,r-reshape2)
       ("r-stringr" ,r-stringr)
       ("r-tibble" ,r-tibble)
       ("r-tidyr" ,r-tidyr)))
    (home-page "https://github.com/tidyverse/broom")
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
    (version "0.1.9")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "recipes" version))
       (sha256
        (base32
         "1fmnka583sqm6v5bhxbllb4cd5xfqbf268aij2xgxiwckv3c0ynm"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-dplyr" ,r-dplyr)
       ("r-generics" ,r-generics)
       ("r-glue" ,r-glue)
       ("r-gower" ,r-gower)
       ("r-ipred" ,r-ipred)
       ("r-lubridate" ,r-lubridate)
       ("r-magrittr" ,r-magrittr)
       ("r-matrix" ,r-matrix)
       ("r-purrr" ,r-purrr)
       ("r-rlang" ,r-rlang)
       ("r-tibble" ,r-tibble)
       ("r-tidyr" ,r-tidyr)
       ("r-tidyselect" ,r-tidyselect)
       ("r-timedate" ,r-timedate)
       ("r-withr" ,r-withr)))
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
    (version "0.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ggrepel" version))
       (sha256
        (base32
         "1qaifn3dazdqbqlii210xhw7yf142iw7g9p2axmmxbz90p0by08d"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-rcpp" ,r-rcpp)
       ("r-scales" ,r-scales)))
    (native-inputs
     `(("r-knitr" ,r-knitr))) ; for vignettes
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
    (version "0.9.5.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "stringdist" version))
       (sha256
        (base32 "1dqfakclzaf878x7mhwmqrcpcql2h9cv19fz5f3ygpajf3si5kqi"))))
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

(define-public r-ucminf
  (package
    (name "r-ucminf")
    (version "1.1-4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ucminf" version))
       (sha256
        (base32
         "01vggwg1w71k98qs6fhb0x1843vi322mf4g3hbclks94kcpkisx2"))))
    (build-system r-build-system)
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/ucminf/")
    (synopsis "General-purpose unconstrained non-linear optimization")
    (description
     "This package provides an implementation of an algorithm for
general-purpose unconstrained non-linear optimization.  The algorithm is of
quasi-Newton type with BFGS updating of the inverse Hessian and soft line
search with a trust region type monitoring of the input to the line search
algorithm.  The interface of @code{ucminf} is designed for easy interchange
with the package @code{optim}.")
    (license license:gpl2+)))

(define-public r-ordinal
  (package
    (name "r-ordinal")
    (version "2019.12-10")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ordinal" version))
       (sha256
        (base32
         "09bpmjmbf4x82kgf6bm4bkncq2apdv9mk20zj4zgma2jx2vyfhbs"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-mass" ,r-mass)
       ("r-matrix" ,r-matrix)
       ("r-numderiv" ,r-numderiv)
       ("r-ucminf" ,r-ucminf)))
    (home-page "https://github.com/runehaubo/ordinal")
    (synopsis "Regression models for ordinal data")
    (description
     "This package provides an implementation of cumulative link (mixed)
models also known as ordered regression models, proportional odds models,
proportional hazards models for grouped survival times and ordered models.
Estimation is via maximum likelihood and mixed models are fitted with the
Laplace approximation and adaptive Gauss-Hermite quadrature.")
    (license license:gpl2+)))

(define-public r-jomo
  (package
    (name "r-jomo")
    (version "2.6-10")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "jomo" version))
       (sha256
        (base32
         "1k9l4290g350zbw1pjs871q9bxj3j2h1dilxpp06v4wy4n7d8qs0"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-lme4" ,r-lme4)
       ("r-mass" ,r-mass)
       ("r-ordinal" ,r-ordinal)
       ("r-survival" ,r-survival)))
    (home-page "https://cran.r-project.org/web/packages/jomo/")
    (synopsis "Multilevel Joint Modelling Multiple Imputation")
    (description
     "Similarly to Schafer's package pan, jomo is a package for multilevel
joint modelling multiple imputation @url{Carpenter and Kenward (2013),
http://doi.org/10.1002/9781119942283}.  Novel aspects of jomo are the
possibility of handling binary and categorical data through latent normal
variables, the option to use cluster-specific covariance matrices and to
impute compatibly with the substantive model.")
    (license license:gpl2)))

(define-public r-pan
  (package
    (name "r-pan")
    (version "1.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "pan" version))
       (sha256
        (base32
         "1dk3jjj826p7xrz10qz04vyc068xnypg7bp0pj4c32z3da0xzh5d"))))
    (build-system r-build-system)
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/pan/")
    (synopsis "Multiple imputation for multivariate panel or clustered data")
    (description
     "This package implements multiple imputation for multivariate panel or
clustered data.")
    (license license:gpl3)))

(define-public r-mitml
  (package
    (name "r-mitml")
    (version "0.3-7")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "mitml" version))
       (sha256
        (base32
         "0yqyxkyi1kmv5k63wxj5kkg5g8igk1axk2csb4xhj6wz0p89dxy6"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-haven" ,r-haven)
       ("r-jomo" ,r-jomo)
       ("r-pan" ,r-pan)))
    (home-page "https://cran.r-project.org/web/packages/mitml/")
    (synopsis "Tools for multiple imputation in multilevel modeling")
    (description
     "This package provides tools for multiple imputation of missing data in
multilevel modeling.  It includes a user-friendly interface to the packages
pan and jomo, and several functions for visualization, data management and the
analysis of multiply imputed data sets.")
    (license license:gpl2+)))

(define-public r-mice
  (package
    (name "r-mice")
    (version "3.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "mice" version))
       (sha256
        (base32
         "1ibybvigqkkla4xwhqs2w4m6c68alfcdsljx99nn4p92rzb1ig04"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-broom" ,r-broom)
       ("r-dplyr" ,r-dplyr)
       ("r-lattice" ,r-lattice)
       ("r-rcpp" ,r-rcpp)
       ("r-tidyr" ,r-tidyr)))
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
    (version "1.0-8")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "truncnorm" version))
       (sha256
        (base32
         "0zn88wdd58223kibk085rhsikl4yhlrwiyq109hzjg06hy6lwmj9"))))
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
    (version "1.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "HardyWeinberg" version))
       (sha256
        (base32
         "1irz44q6nf95h37av868f47aakwv3jgwgw217xfsfw0afkm7s25f"))))
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
    (version "2.2-5.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "sm" version))
       (sha256
        (base32
         "0c4whcx879gb4lwvqnzxl5n9xgpcqh2c54ip9ami3mwfprzcv45q"))))
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

(define-public r-venndiagram
  (package
    (name "r-venndiagram")
    (version "1.6.20")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "VennDiagram" version))
              (sha256
               (base32
                "1ic1jaxzw98si2p4n1fl4n3myhd7fpw0njb634cwhviwybzv6775"))))
    (properties `((upstream-name . "VennDiagram")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-futile-logger" ,r-futile-logger)))
    (home-page "https://cran.r-project.org/web/packages/VennDiagram/")
    (synopsis "Generate High-Resolution Venn and Euler Plots")
    (description
     "This package provides a set of functions to generate high-resolution
Venn and Euler plots.  It includes handling for several special cases,
including two-case scaling, and extensive customization of plot shape and
structure.")
    (license license:gpl2+)))

(define-public r-vioplot
  (package
    (name "r-vioplot")
    (version "0.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "vioplot" version))
       (sha256
        (base32
         "1fsklymilspzz5fzlj7666x09aglaw0v4x0yfjjzy4vr5qpjc529"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-sm" ,r-sm)
       ("r-zoo" ,r-zoo)))
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
    (version "0.12-0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "xts" version))
       (sha256
        (base32
         "0q4cc8ynp7ndmgll1jj3lxyl6wmgg89ad3wq09kjc2ngszdfc4fz"))))
    (build-system r-build-system)
    (propagated-inputs `(("r-zoo" ,r-zoo)))
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
    (version "2.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "PerformanceAnalytics" version))
       (sha256
        (base32
         "0ci26hsj4wnw9g0mh4vrn0cg986cpcpx169rvw6v6rbnjxq718bq"))))
    (properties
     `((upstream-name . "PerformanceAnalytics")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-quadprog" ,r-quadprog)
       ("r-xts" ,r-xts)
       ("r-zoo" ,r-zoo)))
    (home-page "https://r-forge.r-project.org/projects/returnanalytics/")
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
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "laeken" version))
       (sha256
        (base32
         "199rjkhjjygpr6cjzab87as46acb5npi44m4yycvk7lnd0blma8s"))))
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
    (version "1.4-6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "vcd" version))
       (sha256
        (base32
         "0rjz49py5l6wnaimw6k8rcyzlvs8cyz5g2xwqj2qis92ly0l103z"))))
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
    (version "1.0-2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ica" version))
       (sha256
        (base32
         "0ya1nph1zwhad0bfz4yxs27kl45yk1dhnphdlrq34p8pqrpmj8g7"))))
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
    (version "1.21-3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "dtw" version))
       (sha256
        (base32
         "02hyhx1sy5h3vzh9zixy18a7d47df4k5d0wyflcvlcbsbcl6p90s"))))
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
    (version "1.1-221.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "SDMTools" version))
       (sha256
        (base32
         "1xvcd97ikqsfdpk2fddy3k0z1ajqga7nv9bgac9c1wnjk1gqrpgh"))))
    (properties `((upstream-name . "SDMTools")))
    (build-system r-build-system)
    (propagated-inputs `(("r-r-utils" ,r-r-utils)))
    (home-page "https://www.rforge.net/SDMTools/")
    (synopsis "Species distribution modelling tools")
    (description "This package provides a set of tools for post processing
the outcomes of species distribution modeling exercises.  It includes novel
methods for comparing models and tracking changes in distributions through
time.  It further includes methods for visualizing outcomes, selecting
thresholds, calculating measures of accuracy and landscape fragmentation
statistics, etc.")
    (license license:gpl3+)))

(define-public r-scatterplot3d
  (package
    (name "r-scatterplot3d")
    (version "0.3-41")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "scatterplot3d" version))
       (sha256
        (base32
         "152xqz9c70qab86mpgng049gxsg5f4fpf1m8dh93fb9v1avjd0sc"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/scatterplot3d/")
    (synopsis "3D scatter plot")
    (description "This package provides an implementation of scatter plots for
plotting.  a three dimensional point cloud.")
    (license license:gpl2)))

(define-public r-ggridges
  (package
    (name "r-ggridges")
    (version "0.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ggridges" version))
       (sha256
        (base32
         "03pz257aw0mkh5k75rby9givkc1ky3n5scvhjhjiz9vry9fpffmh"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-plyr" ,r-plyr)
       ("r-scales" ,r-scales)
       ("r-withr" ,r-withr)))
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
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ggjoy" version))
       (sha256
        (base32
         "012md2m0jqfcccb933j423m3ck31v3p0pd41gjxpyg9082y7ixyj"))))
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
    (version "2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "cli" version))
       (sha256
        (base32
         "1dhkah6jvr96k4h4agcc2rfls75bpjb0j58fzaz3dc0fp3jk8229"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-assertthat" ,r-assertthat)
       ("r-crayon" ,r-crayon)
       ("r-fansi" ,r-fansi)
       ("r-glue" ,r-glue)))
    (home-page "https://github.com/r-lib/cli#readme")
    (synopsis "Helpers for developing command line interfaces")
    (description "This package provides a suite of tools designed to build
attractive command line interfaces (CLIs).  It includes tools for drawing
rules, boxes, trees, and Unicode symbols with ASCII alternatives.")
    (license license:expat)))

(define-public r-argparser
  (package
    (name "r-argparser")
    (version "0.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "argparser" version))
       (sha256
        (base32
         "1pxiz9jlhlnpzqn1lz349r07i7glw708w202x6dlnxp112fg7k4x"))))
    (build-system r-build-system)
    (home-page "https://bitbucket.org/djhshih/argparser")
    (synopsis "Command-line argument parser")
    (description
     "This package provides a cross-platform command-line argument parser
written purely in R with no external dependencies.  It is useful with the
Rscript front-end and facilitates turning an R script into an executable
script.")
    (license license:gpl3+)))

(define-public r-debugme
  (package
    (name "r-debugme")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "debugme" version))
       (sha256
        (base32
         "1c9sg55zvf10h8198jdnpamm6f66lzw3c3jnmdp9ls6na0j0xbjd"))))
    (build-system r-build-system)
    (propagated-inputs `(("r-crayon" ,r-crayon)))
    (home-page "https://github.com/r-lib/debugme#readme")
    (synopsis "Debug R packages")
    (description
     "This package allows the user to specify debug messages as special string
constants, and control debugging of packages via environment variables.")
    (license license:expat)))

(define-public r-processx
  (package
    (name "r-processx")
    (version "3.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "processx" version))
       (sha256
        (base32
         "02awswxq6wh3rl99dncw5n6c1xmd0v81xcdp2dfyi6vm6b3gz84l"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ps" ,r-ps)
       ("r-r6" ,r-r6)))
    (home-page "https://github.com/r-lib/processx3")
    (synopsis "Execute and control system processes")
    (description
     "This package provides portable tools to run system processes in the
background.  It can check if a background process is running; wait on a
background process to finish; get the exit status of finished processes; kill
background processes and their children; restart processes.  It can read the
standard output and error of the processes, using non-blocking connections.
@code{processx} can poll a process for standard output or error, with a
timeout.  It can also poll several processes at once.")
    (license license:expat)))

(define-public r-tsp
  (package
    (name "r-tsp")
    (version "1.1-9")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "TSP" version))
       (sha256
        (base32
         "183m6crb0dv17llj86059n3hbgsahbhfcqlx0ijdzkssg11i1cy7"))))
    (properties `((upstream-name . "TSP")))
    (build-system r-build-system)
    (propagated-inputs `(("r-foreach" ,r-foreach)))
    (home-page "https://cran.r-project.org/web/packages/TSP/")
    (synopsis "Traveling salesperson problem (TSP)")
    (description "This package provides basic infrastructure and some
algorithms for the @dfn{traveling salesperson problem}(TSP) (also known as the
traveling salesman problem).")
    (license license:gpl3)))

(define-public r-qap
  (package
    (name "r-qap")
    (version "0.1-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "qap" version))
       (sha256
        (base32
         "0d2d1ni1camixyi45lfy00f4pn3p063k7bsi8gj5scp6n15mdgb0"))))
    (build-system r-build-system)
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/qap/")
    (synopsis "Heuristics for the quadratic assignment problem (QAP)")
    (description "This package implements heuristics for the @dfn{quadratic
assignment problem} (QAP).  Currently only a simulated annealing heuristic is
available.")
    (license license:gpl3)))

(define-public r-gclus
  (package
    (name "r-gclus")
    (version "1.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "gclus" version))
       (sha256
        (base32
         "1cz0g0i972955hhaji30rx8448x7f3as7z1sww9i5h86ybgirilw"))))
    (build-system r-build-system)
    (propagated-inputs `(("r-cluster" ,r-cluster)))
    (home-page "https://cran.r-project.org/web/packages/gclus/")
    (synopsis "Clustering graphics")
    (description "This package orders panels in scatterplot matrices and
parallel coordinate displays by some merit index.  It contains various indices
of merit, ordering functions, and enhanced versions of @code{pairs} and
@code{parcoord} which color panels according to their merit level.")
    (license license:gpl2+)))

(define-public r-webshot
  (package
    (name "r-webshot")
    (version "0.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "webshot" version))
       (sha256
        (base32
         "0gq4h8cw51z95yvsnf38kj5l58wgljkm0dalmi8mn1sp06bxr0zi"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-callr" ,r-callr)
       ("r-jsonlite" ,r-jsonlite)
       ("r-magrittr" ,r-magrittr)))
    (home-page "https://github.com/wch/webshot/")
    (synopsis "Take screenshots of web pages")
    (description
     "Webshot makes it easy to take screenshots of web pages from within R.
It can also run Shiny applications locally and take screenshots of the
application; and it can render and screenshot static as well as interactive R
Markdown documents.")
    (license license:gpl2)))

(define-public r-seriation
  (package
    (name "r-seriation")
    (version "1.2-8")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "seriation" version))
       (sha256
        (base32
         "1zbdxq0s5rc5v307b69fw9k52m0654ls7pf22lh35ggirig6lwsk"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-cluster" ,r-cluster)
       ("r-colorspace" ,r-colorspace)
       ("r-dendextend" ,r-dendextend)
       ("r-gclus" ,r-gclus)
       ("r-gplots" ,r-gplots)
       ("r-mass" ,r-mass)
       ("r-qap" ,r-qap)
       ("r-registry" ,r-registry)
       ("r-tsp" ,r-tsp)))
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "https://s2.smu.edu/IDA/seriation/")
    (synopsis "Infrastructure for ordering objects using seriation")
    (description
     "This package provides infrastructure for seriation with an
implementation of several seriation/sequencing techniques to reorder matrices,
dissimilarity matrices, and dendrograms.  It also provides (optimally)
reordered heatmaps, color images and clustering visualizations like
dissimilarity plots, and visual assessment of cluster tendency plots (VAT and
iVAT).")
    (license license:gpl3)))

(define-public r-xfun
  (package
    (name "r-xfun")
    (version "0.12")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "xfun" version))
       (sha256
        (base32 "0sgh8kafi9x1glmmcp1ly827pm8q7fsfngbplr41fbb4nc9363df"))))
    (build-system r-build-system)
    (home-page "https://github.com/yihui/xfun")
    (synopsis "Miscellaneous functions")
    (description
     "This package provides miscellaneous functions commonly used in other
packages maintained by Yihui Xie.")
    (license license:expat)))

(define-public r-utf8
  (package
    (name "r-utf8")
    (version "1.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "utf8" version))
       (sha256
        (base32
         "0m0ywg8k3blfiahxvh1i4zn9dksrlc937d2lbza5fc38zjnrrnpn"))))
    (build-system r-build-system)
    (home-page "https://github.com/patperry/r-utf8")
    (synopsis "Unicode text processing")
    (description
     "This package provides tools to process and print UTF-8 encoded
international text (Unicode).  Input, validate, normalize, encode, format, and
display.")
    (license license:asl2.0)))

(define-public r-zeallot
  (package
    (name "r-zeallot")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "zeallot" version))
       (sha256
        (base32
         "1sd1igcfnv27pa3bqxlbyxchi562h7grnjg1l7wxx3bwr49i57s3"))))
    (build-system r-build-system)
    (home-page "https://github.com/nteetor/zeallot")
    (synopsis "Multiple, unpacking, and destructuring assignment")
    (description
     "This package provides a @code{%<-%} operator to perform multiple,
unpacking, and destructuring assignment in R.  The operator unpacks the
right-hand side of an assignment into multiple values and assigns these values
to variables on the left-hand side of the assignment.")
    (license license:expat)))

(define-public r-vctrs
  (package
    (name "r-vctrs")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "vctrs" version))
       (sha256
        (base32
         "15sgzs6afvmhssk6jcg41rn3bvmzmbm4sgca6f6x8lfrsazvdj6w"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-digest" ,r-digest)
       ("r-ellipsis" ,r-ellipsis)
       ("r-glue" ,r-glue)
       ("r-rlang" ,r-rlang)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/r-lib/vctrs")
    (synopsis "Vector helpers")
    (description
     "There are three main goals to the @code{vctrs} package:

@enumerate
@item To propose @code{vec_size()} and @code{vec_type()} as alternatives to
  @code{length()} and @code{class()}.  These definitions are paired with a
  framework for type-coercion and size-recycling.
@item To define type- and size-stability as desirable function properties, use
  them to analyse existing base function, and to propose better alternatives.
  This work has been particularly motivated by thinking about the ideal
  properties of @code{c()}, @code{ifelse()}, and @code{rbind()}.
@item To provide a new @code{vctr} base class that makes it easy to create new
  S3 vectors.  @code{vctrs} provides methods for many base generics in terms of
  a few new @code{vctrs} generics, making implementation considerably simpler
  and more robust.
@end enumerate\n")
    (license license:gpl3)))

(define-public r-pillar
  (package
    (name "r-pillar")
    (version "1.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "pillar" version))
       (sha256
        (base32
         "02sn7zw80wq33jgxk2i6m5jb83sk7y72dfhgyy0apfinv05w92ss"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-cli" ,r-cli)
       ("r-crayon" ,r-crayon)
       ("r-fansi" ,r-fansi)
       ("r-rlang" ,r-rlang)
       ("r-utf8" ,r-utf8)
       ("r-vctrs" ,r-vctrs)))
    (home-page "https://github.com/r-lib/pillar")
    (synopsis "Coloured formatting for columns")
    (description
     "This package provides a @code{pillar} generic designed for formatting
columns of data using the full range of colours provided by modern
terminals.")
    (license license:gpl3)))

(define-public r-uuid
  (package
    (name "r-uuid")
    (version "0.1-4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "uuid" version))
       (sha256
        (base32
         "0gm9ii7ncvllxbvyk6srsiinjmqyj7lmh84w43x4nhqpvafj9q4q"))))
    (build-system r-build-system)
    (home-page "https://www.rforge.net/uuid")
    (synopsis "Tools for generating and handling of UUIDs")
    (description
     "This package provides tools for generating and handling of
@dfn{Universally Unique Identifiers} (UUIDs).")
    (license license:expat)))

(define-public r-tinytex
  (package
    (name "r-tinytex")
    (version "0.20")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "tinytex" version))
       (sha256
        (base32
         "0n8v8inpsc99r0snvqbjhqlc6nm9hxjsw120hrxc2mw03pa5fvkg"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-xfun" ,r-xfun)))
    (home-page "https://github.com/yihui/tinytex")
    (synopsis "Helper functions for TeX Live and compiling LaTeX documents")
    (description
     "This package provides helper functions to install and maintain the LaTeX
distribution named TinyTeX, a lightweight, cross-platform, portable, and
easy-to-maintain version of TeX Live.  This package also contains helper
functions to compile LaTeX documents, and install missing LaTeX packages
automatically.")
    (license license:expat)))

(define-public r-network
  (package
    (name "r-network")
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "network" version))
       (sha256
        (base32
         "0dnf1wl3za2lhx2lwd8smhlijl1cfhckgr8zz9piiirrfi2m2kx2"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-magrittr" ,r-magrittr)
       ("r-tibble" ,r-tibble)))
    (home-page "https://statnet.org/")
    (synopsis "Classes for relational data")
    (description
     "This package provides tools to create and modify network objects.  The
@code{network} class can represent a range of relational data types, and
supports arbitrary vertex/edge/graph attributes.")
    (license license:gpl2+)))

(define-public r-statnet-common
  (package
    (name "r-statnet-common")
    (version "4.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "statnet.common" version))
        (sha256
          (base32
            "0ng90i0wm9wlyhjbnmnylc1bbqw396p1dr7f402dyry9x9ck6jl3"))))
    (properties
      `((upstream-name . "statnet.common")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-coda" ,r-coda)))
    (home-page "https://statnet.org")
    (synopsis "R scripts and utilities used by the Statnet software")
    (description "This package provides non-statistical utilities used by the
software developed by the Statnet Project.")
    (license license:gpl3)))

(define-public r-statcheck
  (package
    (name "r-statcheck")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "statcheck" version))
       (sha256
        (base32
         "0ivybdcrymlsfv6pg6p5bv70qdvgxf2vgp0kf4r0pf2fcvav1mcp"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-plyr" ,r-plyr)
       ("r-rmarkdown" ,r-rmarkdown)))
    (home-page "https://cran.r-project.org/web/packages/statcheck/")
    (synopsis "Extract statistics from articles and recompute p-values")
    (description "This package can automatically extract statistical
null-hypothesis significant testing (NHST) results from articles and recompute
the p-values based on the reported test statistic and degrees of freedom to
detect possible inconsistencies.")
    (license license:gpl2)))

(define-public r-sna
  (package
    (name "r-sna")
    (version "2.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "sna" version))
       (sha256
        (base32
         "1j3i6300m686qqfmyvadim377cd3mplzgj6mandygw8brg50id8k"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-network" ,r-network)
       ("r-statnet-common" ,r-statnet-common)))
    (home-page "https://statnet.org")
    (synopsis "Tools for social network analysis")
    (description
     "This package provides a range of tools for social network analysis,
including node and graph-level indices, structural distance and covariance
methods, structural equivalence detection, network regression, random graph
generation, and 2D/3D network visualization.")
    (license license:gpl2+)))

(define-public r-tfisher
  (package
    (name "r-tfisher")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "TFisher" version))
       (sha256
        (base32
         "0vz74ww1lf1prfwz74hfsi3a8nzq8ss7aqjr85c1d87vss2796xx"))))
    (properties `((upstream-name . "TFisher")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-matrix" ,r-matrix)
       ("r-mvtnorm" ,r-mvtnorm)
       ("r-sn" ,r-sn)))
    (home-page "https://cran.r-project.org/web/packages/TFisher/")
    (synopsis "Optimal thresholding Fisher's p-value combination method")
    (description
     "This package provides the @dfn{cumulative distribution function} (CDF),
quantile, and statistical power calculator for a collection of thresholding
Fisher's p-value combination methods, including Fisher's p-value combination
method, truncated product method and, in particular, soft-thresholding
Fisher's p-value combination method which is proven to be optimal in some
context of signal detection.  The p-value calculator for the omnibus version
of these tests are also included.")
    (license license:gpl2)))

(define-public r-ttr
  (package
    (name "r-ttr")
    (version "0.23-6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "TTR" version))
       (sha256
        (base32
         "0rg22ma3x07s9djlxscfw5jcq1gbir05cwhgvwfi53x1sf4hmhdg"))))
    (properties `((upstream-name . "TTR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-curl" ,r-curl)
       ("r-xts" ,r-xts)
       ("r-zoo" ,r-zoo)))
    (home-page "https://github.com/joshuaulrich/TTR")
    (synopsis "Technical trading rules")
    (description
     "This package provides functions and data to construct technical trading
rules with R.")
    (license license:gpl2)))

(define-public r-leaps
  (package
    (name "r-leaps")
    (version "3.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "leaps" version))
       (sha256
        (base32
         "1dn3yl1p03n0iynd1vsdkrr0fhmvgrmfkv37y7n371765h83lz1x"))))
    (build-system r-build-system)
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/leaps/")
    (synopsis "Regression subset selection")
    (description
     "This package provides tools for regression subset selection, including
exhaustive search.")
    (license license:gpl2+)))

(define-public r-splus2r
  (package
    (name "r-splus2r")
    (version "1.2-2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "splus2R" version))
       (sha256
        (base32
         "0xrbj8vxy0pc6hl7m8abv71d3hjw47cl51s7j7priadyqczkq6sz"))))
    (properties `((upstream-name . "splus2R")))
    (build-system r-build-system)
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/splus2R/")
    (synopsis "Supplemental S-PLUS functionality in R")
    (description
     "Currently there are many functions in S-PLUS that are missing in R.  To
facilitate the conversion of S-PLUS packages to R packages, this package
provides some missing S-PLUS functionality in R.")
    (license license:gpl2)))

(define-public r-ifultools
  (package
    (name "r-ifultools")
    (version "2.0-5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ifultools" version))
       (sha256
        (base32
         "040kvbczcmmbaiaz0k0pdq9af541pjj6iwzh1a3w4szh9w6b5a3j"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-mass" ,r-mass)
       ("r-splus2r" ,r-splus2r)))
    (home-page "https://cran.r-project.org/web/packages/ifultools/")
    (synopsis "Insightful research tools")
    (description "This package provides C code used by the wmtsa, fractal, and
sapa R packages.")
    (license license:gpl2)))

(define-public r-sapa
  (package
    (name "r-sapa")
    (version "2.0-2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "sapa" version))
       (sha256
        (base32
         "056xlh14dnzq4x7sbp7ff2k61jxy7110a742b502vz549qfrr5ds"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ifultools" ,r-ifultools)
       ("r-splus2r" ,r-splus2r)))
    (home-page "https://cran.r-project.org/web/packages/sapa/")
    (synopsis "Spectral analysis for physical applications")
    (description "This package provides software for the book Spectral
Analysis for Physical Applications, Donald B. Percival and Andrew T. Walden,
Cambridge University Press, 1993.")
    (license license:gpl2)))

(define-public r-aggregation
  (package
    (name "r-aggregation")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "aggregation" version))
       (sha256
        (base32
         "0j9g604m2ccc7hcy02539yja9cf3xcbl25gvp838bp4x8w18my46"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/aggregation/")
    (synopsis "Methods for p-value aggregation")
    (description
     "This package contains functionality for performing the following methods
of p-value aggregation: Fisher's method, the Lancaster method (weighted
Fisher's method), and Sidak correction.")
    (license license:gpl3)))

(define-public r-quantmod
  (package
    (name "r-quantmod")
    (version "0.4-16")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "quantmod" version))
       (sha256
        (base32
         "12l5br8abr1yagxqjnjvqzp79sqsv5vx56cxs37gk73r474f4vc2"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-curl" ,r-curl)
       ("r-ttr" ,r-ttr)
       ("r-xts" ,r-xts)
       ("r-zoo" ,r-zoo)))
    (home-page "https://cran.r-project.org/web/packages/quantmod/")
    (synopsis "Quantitative financial modelling framework")
    (description "This package provides a quantitative financial modelling
framework to allow users to specify, build, trade, and analyse quantitative
financial trading strategies.")
    (license license:gpl3)))

(define-public r-tseries
  (package
    (name "r-tseries")
    (version "0.10-47")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "tseries" version))
       (sha256
        (base32
         "0yzvc9djp3angvxdxqi60wi726y76ablsb71q88ycvw0avgpf8r0"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-quadprog" ,r-quadprog)
       ("r-quantmod" ,r-quantmod)
       ("r-zoo" ,r-zoo)))
    (native-inputs
     `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/tseries/")
    (synopsis "Time series analysis and computational finance")
    (description
     "This package provides functions relating to time series analysis and
computational finance.")
    (license license:gpl2)))

(define-public r-wmtsa
  (package
    (name "r-wmtsa")
    (version "2.0-3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "wmtsa" version))
       (sha256
        (base32
         "1q436krz5p1f4a7a7sya6a9rh9x9mi8zzcgq66gbk9w9w4hcqcj6"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ifultools" ,r-ifultools)
       ("r-mass" ,r-mass)
       ("r-splus2r" ,r-splus2r)))
    (home-page "https://cran.r-project.org/web/packages/wmtsa/")
    (synopsis "Wavelet methods for time series analysis")
    (description
     "This package provides software to accompany the book \"Wavelet Methods
for Time Series Analysis\", Donald B. Percival and Andrew T. Walden, Cambridge
University Press, 2000.")
    (license license:gpl2)))

(define-public r-tsa
  (package
    (name "r-tsa")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "TSA" version))
       (sha256
        (base32
         "0gjfqibwdznz0nka95k4fjm935svxjpnqfywwz403crn2lh30h6q"))))
    (properties `((upstream-name . "TSA")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-leaps" ,r-leaps)
       ("r-locfit" ,r-locfit)
       ("r-mgcv" ,r-mgcv)))
    (home-page "https://homepage.divms.uiowa.edu/~kchan/TSA.htm")
    (synopsis "Time series analysis")
    (description
     "This package contains R functions and datasets detailed in the book
\"Time Series Analysis with Applications in R (second edition)\" by Jonathan
Cryer and Kung-Sik Chan.")
    (license license:gpl2+)))

(define-public r-extradistr
  (package
    (name "r-extradistr")
    (version "1.8.11")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "extraDistr" version))
       (sha256
        (base32
         "1vvqv1d4hxa025gmm8cbiph63qsqy87l3ri5idd524gyz3chbcl3"))))
    (properties `((upstream-name . "extraDistr")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rcpp" ,r-rcpp)))
    (home-page "https://github.com/twolodzko/extraDistr")
    (synopsis "Additional univariate and multivariate distributions")
    (description
     "This package implements density, distribution functions, quantile
functions and random generation functions for a large number of univariate and
multivariate distributions.")
    (license license:gpl2)))

(define-public r-fractal
  (package
    (name "r-fractal")
    (version "2.0-4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "fractal" version))
       (sha256
        (base32
         "18lr9z0gslvfc3z8vyj3krqj3bfhg60zv1fzinrwwkc4cpk1w7mp"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ifultools" ,r-ifultools)
       ("r-mass" ,r-mass)
       ("r-sapa" ,r-sapa)
       ("r-scatterplot3d" ,r-scatterplot3d)
       ("r-splus2r" ,r-splus2r)
       ("r-wmtsa" ,r-wmtsa)))
    (home-page "https://cran.r-project.org/web/packages/fractal/")
    (synopsis "Fractal time series modeling and analysis")
    (description
     "This package provides tools for stochastic fractal and deterministic
chaotic time series analysis.")
    (license license:gpl2)))

(define-public r-urca
  (package
    (name "r-urca")
    (version "1.3-0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "urca" version))
       (sha256
        (base32
         "1akaqwf3fvvvx4sgfn641fd4sj51s0701pvfl6s5hnz2k0iwh732"))))
    (build-system r-build-system)
    (propagated-inputs `(("r-nlme" ,r-nlme)))
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/urca/")
    (synopsis "Unit root and cointegration tests for time series data")
    (description
     "This package provides unit root and cointegration tests encountered in
applied econometric analysis.")
    (license license:gpl2+)))

(define-public r-cubature
  (package
    (name "r-cubature")
    (version "2.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "cubature" version))
       (sha256
        (base32
         "0jpyq8j7x06dpiz29w48av879ygldzgls9z810192hsymkmfaz6r"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rcpp" ,r-rcpp)))
    (home-page "https://github.com/bnaras/cubature")
    (synopsis "Adaptive multivariate integration over hypercubes")
    (description
     "This package is an R wrapper around the cubature C library for adaptive
multivariate integration over hypercubes.  This version provides both
@code{hcubature} and @code{pcubature} routines in addition to a vector
interface.")
    ;; The included cubature C library is released under GPLv2+, but the
    ;; wrapper declares the license to be GPLv3+.
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-trend
  (package
    (name "r-trend")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "trend" version))
       (sha256
        (base32
         "09b6ycyfgs4xlhx6kn6qm5rl2acp58hzhv8qclzn3kb1wjjyvxy5"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-extradistr" ,r-extradistr)))
    (native-inputs
     `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/trend/")
    (synopsis "Non-parametric trend tests and change-point detection")
    (description
     "The analysis of environmental data often requires the detection of
trends and change-points.  This package includes tests for trend
detection (Cox-Stuart Trend Test, Mann-Kendall Trend Test, (correlated)
Hirsch-Slack Test, partial Mann-Kendall Trend Test, multivariate (multisite)
Mann-Kendall Trend Test, (Seasonal) Sen's slope, partial Pearson and Spearman
correlation trend test), change-point detection (Lanzante's test procedures,
Pettitt's test, Buishand Range Test, Buishand U Test, Standard Normal
Homogeinity Test), detection of non-randomness (Wallis-Moore Phase Frequency
Test, Bartels rank von Neumann's ratio test, Wald-Wolfowitz Test) and the two
sample Robust Rank-Order Distributional Test.")
    (license license:gpl3)))

(define-public r-expm
  (package
    (name "r-expm")
    (version "0.999-4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "expm" version))
       (sha256
        (base32
         "15k0acg2aqb2ajhwal6l7vhhp03m4lg579805d34554cl0kn9l2q"))))
    (build-system r-build-system)
    (propagated-inputs `(("r-matrix" ,r-matrix)))
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "https://r-forge.r-project.org/projects/expm/")
    (synopsis "Tools for matrix exponentials and related quantities")
    (description
     "This package provides tools for the computation of the matrix
exponential, logarithm, square root, and related quantities.")
    (license license:gpl2+)))

(define-public r-complexplus
  (package
    (name "r-complexplus")
    (version "2.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "complexplus" version))
       (sha256
        (base32
         "16w9v7d1ckavqmr86l34frr37pkvdn0iqnb17ssb8xaggns5lgqx"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-expm" ,r-expm)
       ("r-matrix" ,r-matrix)))
    (home-page "https://cran.r-project.org/web/packages/complexplus/")
    (synopsis "Functions of complex or real variables")
    (description
     "This package extends several functions to the complex domain, including
the matrix exponential and logarithm, and the determinant.")
    (license license:gpl2)))

(define-public r-phontools
  (package
    (name "r-phontools")
    (version "0.2-2.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "phonTools" version))
       (sha256
        (base32
         "01i481mhswsys3gpasw9gn6nxkfmi7bz46g5c84m13pg0cv8hxc7"))))
    (properties `((upstream-name . "phonTools")))
    (build-system r-build-system)
    (home-page "http://www.santiagobarreda.com/rscripts.html")
    (synopsis "Tools for phonetic and acoustic analyses")
    (description
     "This package contains tools for the organization, display, and analysis
of the sorts of data frequently encountered in phonetics research and
experimentation, including the easy creation of IPA vowel plots, and the
creation and manipulation of WAVE audio files.")
    (license license:bsd-2)))

(define-public r-np
  (package
    (name "r-np")
    (version "0.60-10")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "np" version))
       (sha256
        (base32
         "06h8k0kdv5s258jr6s08fabvlycrbs7iq34jk2f2hfmqm2y4nyx2"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-boot" ,r-boot)
       ("r-cubature" ,r-cubature)
       ("r-quadprog" ,r-quadprog)
       ("r-quantreg" ,r-quantreg)))
    (home-page "https://github.com/JeffreyRacine/R-Package-np")
    (synopsis "Non-parametric kernel smoothing methods for mixed data types")
    (description "This package provides non-parametric (and semi-parametric)
kernel methods that seamlessly handle a mix of continuous, unordered, and
ordered factor data types.")
    ;; Any version of the GPL.
    (license license:gpl3+)))

(define-public r-powerplus
  (package
    (name "r-powerplus")
    (version "3.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "powerplus" version))
       (sha256
        (base32
         "0ayp6x34hkzgris4j3zbbs0r23n81bhww3wgfyy630ri4sk6brrn"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-complexplus" ,r-complexplus)
       ("r-expm" ,r-expm)
       ("r-mass" ,r-mass)
       ("r-matrix" ,r-matrix)
       ("r-phontools" ,r-phontools)))
    (home-page "https://cran.r-project.org/web/packages/powerplus/")
    (synopsis "Exponentiation operations")
    (description
     "This package provides tools for the computation of matrix and scalar
exponentiation.")
    (license license:gpl2)))

(define-public r-egg
  (package
    (name "r-egg")
    (version "0.4.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "egg" version))
       (sha256
        (base32
         "1fy7srpiavfn8kyrr1m84an7acgwi6ydzrg71m3b0vk7y9ybmj0m"))))
    (properties `((upstream-name . "egg")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-gridextra" ,r-gridextra)
       ("r-gtable" ,r-gtable)))
    (home-page "https://cran.r-project.org/web/packages/egg")
    (synopsis "Extensions for ggplot2")
    (description
     "This package provides miscellaneous functions to help customize ggplot2
objects.  High-level functions are provided to post-process ggplot2 layouts
and allow alignment between plot panels, as well as setting panel sizes to
fixed values.  Other functions include a custom @code{geom}, and helper
functions to enforce symmetric scales or add tags to facetted plots.")
    (license license:gpl3)))

(define-public r-heatmaply
  (package
    (name "r-heatmaply")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "heatmaply" version))
       (sha256
        (base32
         "0576gml3bcl7r1biigzj1rag2xzz422knbw7arc8d2gsakjj757g"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-assertthat" ,r-assertthat)
       ("r-colorspace" ,r-colorspace)
       ("r-dendextend" ,r-dendextend)
       ("r-egg" ,r-egg)
       ("r-ggplot2" ,r-ggplot2)
       ("r-htmlwidgets" ,r-htmlwidgets)
       ("r-magrittr" ,r-magrittr)
       ("r-plotly" ,r-plotly)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-reshape2" ,r-reshape2)
       ("r-scales" ,r-scales)
       ("r-seriation" ,r-seriation)
       ("r-viridis" ,r-viridis)
       ("r-webshot" ,r-webshot)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://cran.r-project.org/package=heatmaply")
    (synopsis "Interactive cluster heat maps using plotly")
    (description
     "This package enables you to create interactive cluster heatmaps that can
be saved as a stand-alone HTML file, embedded in R Markdown documents or in a
Shiny app, and made available in the RStudio viewer pane.  Hover the mouse
pointer over a cell to show details or drag a rectangle to zoom.  A heatmap is
a popular graphical method for visualizing high-dimensional data, in which a
table of numbers is encoded as a grid of colored cells.  The rows and columns
of the matrix are ordered to highlight patterns and are often accompanied by
dendrograms.")
    ;; Either version of the license.
    (license (list license:gpl2 license:gpl3))))

(define-public r-h5
  (package
    (name "r-h5")
    (version "0.9.9")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "h5" version))
       (sha256
        (base32
         "14p7i1sj24ky87kd7qr3n9fc9l64s0bp0rwbyl6i2x69xn75gpsx"))))
    (build-system r-build-system)
    (inputs
     `(("zlib" ,zlib)
       ("hdf5" ,hdf5)))
    (native-inputs
     `(("which" ,which)))
    (propagated-inputs
     `(("r-rcpp" ,r-rcpp)))
    (home-page "https://github.com/mannau/h5")
    (synopsis "Interface to the HDF5 Library")
    (description
     "This package provides an S4 interface to the HDF5 library supporting
fast storage and retrieval of R-objects like vectors, matrices and arrays to
binary files in a language independent format.  The HDF5 format can therefore
be used as an alternative to R's save/load mechanism.  Since h5 is able to
access only subsets of stored data it can also handle data sets which do not
fit into memory.")
    (license license:bsd-2)))

(define-public r-cgdsr
  (package
    (name "r-cgdsr")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "cgdsr" version))
       (sha256
        (base32
         "07yc819hkabpzzh0g0cbqza6bcfy67b2marrzz1lj97f9iba78ja"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-httr" ,r-httr)
       ("r-r-methodss3" ,r-r-methodss3)
       ("r-r-oo" ,r-r-oo)))
    (home-page "https://github.com/cBioPortal/cgdsr")
    (synopsis "R-based API for accessing the MSKCC Cancer Genomics Data Server")
    (description
     "This package provides a basic set of R functions for querying the Cancer
Genomics Data Server (CGDS), hosted by the Computational Biology Center at
Memorial-Sloan-Kettering Cancer Center (MSKCC).")
    (license license:lgpl3)))

(define-public r-import
  (package
    (name "r-import")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "import" version))
       (sha256
        (base32
         "0blf9539rbfwcmw8zsb4k58slb4pdnc075v34vmyjw752fznhcji"))))
    (build-system r-build-system)
    (home-page "https://github.com/smbache/import")
    (synopsis "Import mechanism for R")
    (description
     "This is an alternative mechanism for importing objects from packages.
The syntax allows for importing multiple objects from a package with a single
command in an expressive way.  The import package bridges some of the gap
between using @code{library} (or @code{require}) and direct (single-object)
imports.  Furthermore the imported objects are not placed in the current
environment.  It is also possible to import objects from stand-alone @code{.R}
files.")
    (license license:expat)))

(define-public r-shinyace
  (package
    (name "r-shinyace")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "shinyAce" version))
       (sha256
        (base32
         "1m33dfm2kjirvgix7ybv1kbzgjkicdpv411g9c0q3fw6rnyhfxxn"))))
    (properties `((upstream-name . "shinyAce")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-shiny" ,r-shiny)
       ("r-jsonlite" ,r-jsonlite)))
    (home-page "http://cran.r-project.org/web/packages/shinyAce")
    (synopsis "Ace editor bindings for Shiny")
    (description
     "This package provides Ace editor bindings to enable a rich text editing
environment within Shiny.")
    (license license:expat)))

(define-public r-base64url
  (package
    (name "r-base64url")
    (version "1.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "base64url" version))
       (sha256
        (base32
         "0n1c2b68vza1dh7sk38v6biiwm72c4jpl79kpdg1bsb0hq9qy18x"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-backports" ,r-backports)))
    (home-page "https://github.com/mllg/base64url")
    (synopsis "Fast and URL-safe base64 encoder and decoder")
    (description
     "This package provides a URL-safe base64 encoder and decoder.  In
contrast to RFC3548, the 62nd character (@code{+}) is replaced with @code{-},
the 63rd character (@code{/}) is replaced with @code{_}.  Furthermore, the
encoder does not fill the string with trailing @code{=}.  The resulting
encoded strings comply to the regular expression pattern @code{[A-Za-z0-9_-]}
and thus are safe to use in URLs or for file names.  The package also comes
with a simple base32 encoder/decoder suited for case insensitive file
systems.")
    (license license:gpl3)))

(define-public r-radiant-data
  (package
    (name "r-radiant-data")
    (version "1.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "radiant.data" version))
       (sha256
        (base32
         "08x7zasxf429m021482p86lx3zc6dqz2mih0id8s34isg4gafapg"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Delete files that are under CC-NC-SA.
           (delete-file-recursively "inst/app/tools/help")
           #t))))
    (properties `((upstream-name . "radiant.data")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-base64enc" ,r-base64enc)
       ("r-broom" ,r-broom)
       ("r-car" ,r-car)
       ("r-curl" ,r-curl)
       ("r-dplyr" ,r-dplyr)
       ("r-dt" ,r-dt)
       ("r-glue" ,r-glue)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gridextra" ,r-gridextra)
       ("r-import" ,r-import)
       ("r-jsonlite" ,r-jsonlite)
       ("r-knitr" ,r-knitr)
       ("r-lubridate" ,r-lubridate)
       ("r-magrittr" ,r-magrittr)
       ("r-markdown" ,r-markdown)
       ("r-plotly" ,r-plotly)
       ("r-psych" ,r-psych)
       ("r-readr" ,r-readr)
       ("r-readxl" ,r-readxl)
       ("r-rlang" ,r-rlang)
       ("r-rmarkdown" ,r-rmarkdown)
       ("r-rstudioapi" ,r-rstudioapi)
       ("r-scales" ,r-scales)
       ("r-shiny" ,r-shiny)
       ("r-shinyfiles" ,r-shinyfiles)
       ("r-shinyace" ,r-shinyace)
       ("r-stringi" ,r-stringi)
       ("r-tibble" ,r-tibble)
       ("r-tidyr" ,r-tidyr)
       ("r-writexl" ,r-writexl)))
    (home-page "https://github.com/radiant-rstats/radiant.data")
    (synopsis "Data menu for Radiant: business analytics using R and Shiny")
    (description
     "The Radiant Data menu includes interfaces for loading, saving, viewing,
visualizing, summarizing, transforming, and combining data.  It also contains
functionality to generate reproducible reports of the analyses conducted in
the application.")
    (license license:agpl3)))

(define-public r-algdesign
  (package
    (name "r-algdesign")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "AlgDesign" version))
       (sha256
        (base32 "0ammlg148gk0p24fh700116nd66636np0jb1wwh0axq5jphwk1pz"))))
    (properties `((upstream-name . "AlgDesign")))
    (build-system r-build-system)
    (home-page "https://github.com/jvbraun/AlgDesign")
    (synopsis "Algorithmic experimental design")
    (description
     "This package provides tools to calculate exact and approximate theory
experimental designs for D, A, and I criteria.  Very large designs may be
created.  Experimental designs may be blocked or blocked designs created from
a candidate list, using several criteria.  The blocking can be done when whole
and within plot factors interact.")
    (license license:gpl2+)))

(define-public r-signal
  (package
    (name "r-signal")
    (version "0.7-6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "signal" version))
       (sha256
        (base32
         "1vsxramz5qd9q9s3vlqzmfdpmwl2rhlb2n904zw6f0fg0xxjfq3b"))))
    (build-system r-build-system)
    (propagated-inputs `(("r-mass" ,r-mass)))
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/signal/")
    (synopsis "Signal processing")
    (description
     "This package provides a set of signal processing functions originally
written for Matlab and GNU Octave.  It includes filter generation utilities,
filtering functions, resampling routines, and visualization of filter models.
It also includes interpolation functions.")
    (license license:gpl2)))

(define-public r-gsubfn
  (package
    (name "r-gsubfn")
    (version "0.7")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "gsubfn" version))
       (sha256
        (base32
         "00j6b8b6xsx6v370h220x233rpk6asca78165y3d48jpwvwisdc9"))))
    (build-system r-build-system)
    (propagated-inputs `(("r-proto" ,r-proto)))
    (home-page "https://code.google.com/archive/p/gsubfn/")
    (synopsis "Utilities for strings and function arguments.")
    (description
     "This package provides @code{gsubfn} which is like @code{gsub} but can
take a replacement function or certain other objects instead of the
replacement string.  Matches and back references are input to the replacement
function and replaced by the function output.  @code{gsubfn} can be used to
split strings based on content rather than delimiters and for quasi-perl-style
string interpolation.  The package also has facilities for translating
formulas to functions and allowing such formulas in function calls instead of
functions.")
    (license license:gpl2+)))

(define-public r-sqldf
  (package
    (name "r-sqldf")
    (version "0.4-11")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "sqldf" version))
       (sha256
        (base32
         "0q12vsb53p2wchgp8wfz5bk08wfnm0jxjrakclj4jyy6x3a7ksff"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-chron" ,r-chron)
       ("r-dbi" ,r-dbi)
       ("r-gsubfn" ,r-gsubfn)
       ("r-proto" ,r-proto)
       ("r-rsqlite" ,r-rsqlite)))
    (home-page "https://github.com/ggrothendieck/sqldf")
    (synopsis "Manipulate R data frames using SQL")
    (description
     "The @code{sqldf} function is typically passed a single argument which is
an SQL select statement where the table names are ordinary R data frame names.
@code{sqldf} transparently sets up a database, imports the data frames into
that database, performs the SQL statement and returns the result using a
heuristic to determine which class to assign to each column of the returned
data frame.  The @code{sqldf} or @code{read.csv.sql} functions can also be
used to read filtered files into R even if the original files are larger than
R itself can handle.")
    (license license:gpl2)))

(define-public r-abind
  (package
    (name "r-abind")
    (version "1.4-5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "abind" version))
       (sha256
        (base32
         "0b1zd8jbnl6l292cr9rb50m09fy3ylxvzkpgi5lfb1nbzddcwfis"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/abind/")
    (synopsis "Combine multidimensional arrays")
    (description
     "This package provides tools to combine multidimensional arrays into a
single array.  This is a generalization of @code{cbind} and @code{rbind}.  It
works with vectors, matrices, and higher-dimensional arrays.  It also provides
the functions @code{adrop}, @code{asub}, and @code{afill} for manipulating,
extracting and replacing data in arrays.")
    (license license:lgpl2.0+)))

(define-public r-prroc
  (package
    (name "r-prroc")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "PRROC" version))
       (sha256
        (base32
         "1m28h8pcd78049lz2qixhkcr9h5b3jik3maqzfbvq9y58z71i4a7"))))
    (properties `((upstream-name . "PRROC")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/PRROC/")
    (synopsis "Precision-Recall and ROC curves for weighted and unweighted data")
    (description
     "This package computes the areas under the @dfn{precision-recall} (PR)
and ROC curve for weighted (e.g. soft-labeled) and unweighted data.  In
contrast to other implementations, the interpolation between points of the PR
curve is done by a non-linear piecewise function.  In addition to the areas
under the curves, the curves themselves can also be computed and plotted by a
specific S3-method.")
    (license license:gpl3)))

(define-public r-vim
  (package
    (name "r-vim")
    (version "5.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "VIM" version))
       (sha256
        (base32
         "0w22ir0gvym7gqd6nw2j7w5ivlb3az1dkfxv33imimkb7c83056a"))))
    (properties `((upstream-name . "VIM")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-car" ,r-car)
       ("r-colorspace" ,r-colorspace)
       ("r-data-table" ,r-data-table)
       ("r-e1071" ,r-e1071)
       ("r-laeken" ,r-laeken)
       ("r-magrittr" ,r-magrittr)
       ("r-mass" ,r-mass)
       ("r-nnet" ,r-nnet)
       ("r-ranger" ,r-ranger)
       ("r-rcpp" ,r-rcpp)
       ("r-robustbase" ,r-robustbase)
       ("r-sp" ,r-sp)
       ("r-vcd" ,r-vcd)))
    (home-page "https://github.com/alexkowa/VIM")
    (synopsis "Visualization and imputation of missing values")
    (description
     "This package provides tools for the visualization of missing and/or
imputed values are introduced, which can be used for exploring the data and
the structure of the missing and/or imputed values.  Depending on this
structure of the missing values, the corresponding methods may help to
identify the mechanism generating the missing values and allows to explore the
data including missing values.  In addition, the quality of imputation can be
visually explored using various univariate, bivariate, multiple and
multivariate plot methods.")
    (license license:gpl2+)))

(define-public r-fnn
  (package
    (name "r-fnn")
    (version "1.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "FNN" version))
       (sha256
        (base32
         "0cllqlnynm5yaj4r64mqyyfc8phkb38rwssq8k8ikgfgr4jklxny"))))
    (properties `((upstream-name . "FNN")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/FNN")
    (synopsis "Fast nearest neighbor search algorithms and applications")
    (description
     "This package provides cover-tree and kd-tree fast k-nearest neighbor
search algorithms.  Related applications including KNN classification,
regression and information measures are implemented.")
    ;; The DESCRIPTION file erroneously states that GPL version 2.1 or
    ;; later can be used.
    (license license:gpl2+)))

(define-public r-smoother
  (package
    (name "r-smoother")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "smoother" version))
       (sha256
        (base32
         "0nqr1bvlr5bnasqg74zmknjjl4x28kla9h5cxpga3kq5z215pdci"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ttr" ,r-ttr)))
    (home-page "https://cran.r-project.org/web/packages/smoother")
    (synopsis "Functions relating to the smoothing of numerical data")
    (description
     "This package provides a collection of methods for smoothing numerical
data, commencing with a port of the Matlab gaussian window smoothing function.
In addition, several functions typically used in smoothing of financial data
are included.")
    (license license:gpl2)))

(define-public r-riverplot
  (package
    (name "r-riverplot")
    (version "0.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "riverplot" version))
       (sha256
        (base32
         "0q1icpny8nkxyjhawyjzwrw8qlz0ayn2xyrsqrm4vkxyv6c9xk8z"))))
    (build-system r-build-system)
    (home-page "https://logfc.wordpress.com")
    (synopsis "Sankey or ribbon plots")
    (description
     "Sankey plots are a type of diagram that is convenient to illustrate how
flow of information, resources etc. separates and joins, much like observing
how rivers split and merge.  For example, they can be used to compare
different clusterings.  This package provides an implementation of Sankey
plots for R.")
    (license license:gpl2+)))

(define-public r-dyn
  (package
    (name "r-dyn")
    (version "0.2-9.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "dyn" version))
       (sha256
        (base32
         "16fqv9k7yxdgybwzafjkyqm16qpgqz13lcjpi6a1nc8xbzlzh0gb"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-zoo" ,r-zoo)))
    (home-page "https://cran.r-project.org/web/packages/dyn")
    (synopsis "Time series regression")
    (description
     "This package provides the dyn class interfaces @code{ts}, @code{irts},
@code{zoo} and @code{zooreg} time series classes to @code{lm}, @code{glm},
@code{loess}, @code{quantreg::rq}, @code{MASS::rlm},
@code{MCMCpack::MCMCregress()}, @code{quantreg::rq()},
@code{randomForest::randomForest()} and other regression functions, allowing
those functions to be used with time series including specifications that may
contain lags, diffs and missing values.")
    ;; Any GPL version.
    (license license:gpl2+)))

(define-public r-catdap
  (package
    (name "r-catdap")
    (version "1.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "catdap" version))
       (sha256
        (base32
         "0fyhl69z2lznymvpzxra9qvcg85ggzkfjy68c6mzdmf1ja44d2k5"))))
    (build-system r-build-system)
    (native-inputs
     `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/catdap/")
    (synopsis "Tools for categorical data analysis")
    (description
     "This package provides functions for analyzing multivariate data.
Dependencies of the distribution of the specified variable (response
variable) to other variables (explanatory variables) are derived and
evaluated by the @dfn{Akaike Information Criterion} (AIC).")
    (license license:gpl2+)))

(define-public r-arules
  (package
    (name "r-arules")
    (version "1.6-4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "arules" version))
       (sha256
        (base32
         "003c5cd3xzq39h7c19px077ygm0n1v7k83icy5zzrnkagyds2p8n"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-matrix" ,r-matrix)))
    (home-page "https://github.com/mhahsler/arules")
    (synopsis "Mining association rules and frequent itemsets")
    (description
     "This package provides an infrastructure for representing, manipulating
and analyzing transaction data and patterns (frequent itemsets and association rules).
It also provides C implementations of the association mining algorithms Apriori
and Eclat.")
    (license license:gpl3)))

(define-public r-parsedate
  (package
    (name "r-parsedate")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "parsedate" version))
       (sha256
        (base32
         "0gb3w6hmwxayhijpf36p5dk4h6bbdps57x3cgikwvvxkgi83rarr"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rematch2" ,r-rematch2)))
    (home-page "https://github.com/gaborcsardi/parsedate")
    (synopsis
     "Recognize and parse dates in various formats")
    (description
     "This package provides three functions for dealing with dates:
@code{parse_iso_8601} recognizes and parses all valid ISO 8601 date and
time formats, @code{parse_date} parses dates in unspecified formats,
and @code{format_iso_8601} formats a date in ISO 8601 format.")
    (license license:gpl2)))

(define-public r-abc-data
  (package
    (name "r-abc-data")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "abc.data" version))
       (sha256
        (base32
         "1bv1n68ah714ws58cf285n2s2v5vn7382lfjca4jxph57lyg8hmj"))))
    (properties `((upstream-name . "abc.data")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/abc.data/")
    (synopsis "Data for Approximate Bayesian Computation (ABC) package")
    (description
     "This package contains data which are used by functions of the abc
package which implements several @dfn{Approximate Bayesian Computation} (ABC)
algorithms for performing parameter estimation, model selection, and
goodness-of-fit.")
    (license license:gpl3+)))

(define-public r-abc
  (package
    (name "r-abc")
    (version "2.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "abc" version))
       (sha256
        (base32
         "0ngzaaz2y2s03fhngvwipmy4kq38xrmyddaz6a6l858rxvadrlhb"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-abc-data" ,r-abc-data)
       ("r-locfit" ,r-locfit)
       ("r-mass" ,r-mass)
       ("r-nnet" ,r-nnet)
       ("r-quantreg" ,r-quantreg)))
    (home-page "https://cran.r-project.org/web/packages/abc/")
    (synopsis "Tools for Approximate Bayesian Computation (ABC)")
    (description
     "This package implements several @dfn{Approximate Bayesian
Computation} (ABC) algorithms for performing parameter estimation, model
selection, and goodness-of-fit.  Cross-validation tools are also available for
measuring the accuracy of ABC estimates, and to calculate the
misclassification probabilities of different models.")
    (license license:gpl3+)))

(define-public r-zip
  (package
    (name "r-zip")
    (version "2.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "zip" version))
       (sha256
        (base32
         "1c02amk3pl6xir5jnbfiwiv2wvpkpbkkb1w71y6lf2yk7g3d0pdb"))))
    (build-system r-build-system)
    (home-page "https://github.com/gaborcsardi/zip")
    (synopsis "Cross-platform Zip compression")
    (description
     "This package provides a cross-platform Zip compression library for R.
It is a replacement for the @code{zip} function, that does not require any
additional external tools on any platform.")
    (license license:cc0)))

(define-public r-openxlsx
  (package
    (name "r-openxlsx")
    (version "4.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "openxlsx" version))
       (sha256
        (base32
         "1mwxldw9i9nfksx1i6h1kfs7vmsz9fgyllbsipar4vnfyqhqp8q7"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rcpp" ,r-rcpp)
       ("r-stringi" ,r-stringi)
       ("r-zip" ,r-zip)))
    (home-page "https://github.com/awalker89/openxlsx")
    (synopsis "Read, write and edit XLSX files")
    (description
     "This package simplifies the creation of Excel @code{.xlsx} files by
providing a high level interface to writing, styling and editing worksheets.
Through the use of Rcpp, read/write times are comparable to the @code{xlsx}
and @code{XLConnect} packages with the added benefit of removing the
dependency on Java.")
    (license license:gpl3)))

(define-public r-rio
  (package
    (name "r-rio")
    (version "0.5.16")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rio" version))
       (sha256
        (base32
         "0rfl56fdawlhc98451a9lcb6a6m56kw0i7dvd5hx58z025d8vsyk"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-curl" ,r-curl)
       ("r-data-table" ,r-data-table)
       ("r-foreign" ,r-foreign)
       ("r-haven" ,r-haven)
       ("r-openxlsx" ,r-openxlsx)
       ("r-readxl" ,r-readxl)
       ("r-tibble" ,r-tibble)))
    (home-page "https://github.com/leeper/rio")
    (synopsis "Swiss-army knife for data I/O")
    (description
     "This package provides streamlined data import and export infrastructure
by making assumptions that the user is probably willing to make: @code{import}
and @code{export} determine the data structure from the file extension,
reasonable defaults are used for data import and export (e.g.,
@code{stringsAsFactors=FALSE}), web-based import is natively
supported (including from SSL/HTTPS), compressed files can be read directly
without explicit decompression, and fast import packages are used where
appropriate.  An additional convenience function, @code{convert}, provides a
simple method for converting between file types.")
    (license license:gpl2)))

(define-public r-maptools
  (package
    (name "r-maptools")
    (version "0.9-9")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "maptools" version))
       (sha256
        (base32
         "0v4llkxk8qs61vq4ykvaim4k23aagdaz0p62ns7zfq02sln3pfk9"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-foreign" ,r-foreign)
       ("r-lattice" ,r-lattice)
       ("r-sp" ,r-sp)))
    (home-page "http://r-forge.r-project.org/projects/maptools/")
    (synopsis "Tools for reading and handling spatial objects")
    (description
     "This package provides a set of tools for manipulating and reading
geographic data, in particular ESRI Shapefiles.  It includes binary access to
GSHHG shoreline files.  The package also provides interface wrappers for
exchanging spatial objects with other R packages.")
    ;; The C source files from shapelib are released under the Expat license.
    ;; The R code is released under GPL version 2 or later.
    (license (list license:gpl2+
                   license:expat))))

(define-public r-later
  (package
    (name "r-later")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "later" version))
       (sha256
        (base32
         "11xjavj7siz0xv2ffq1ld4bwl35jyrcfpvvs4p3ilpifxx49hyr7"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bh" ,r-bh)
       ("r-rcpp" ,r-rcpp)
       ("r-rlang" ,r-rlang)))
    (home-page "https://github.com/r-lib/later")
    (synopsis "Utilities for delaying function execution")
    (description
     "This package provides tools to execute arbitrary R or C functions some
time after the current time, after the R execution stack has emptied.")
    (license license:gpl2+)))

(define-public r-promises
  (package
    (name "r-promises")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "promises" version))
       (sha256
        (base32
         "01l0ydjvvy6afcg5d6pzvk1ikd3djq8n2flv8c831ksn68z0zsn8"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-later" ,r-later)
       ("r-magrittr" ,r-magrittr)
       ("r-r6" ,r-r6)
       ("r-rcpp" ,r-rcpp)
       ("r-rlang" ,r-rlang)))
    (home-page "https://rstudio.github.io/promises")
    (synopsis "Abstractions for promise-based asynchronous programming")
    (description
     "This package provides fundamental abstractions for doing asynchronous
programming in R using promises.  Asynchronous programming is useful for
allowing a single R process to orchestrate multiple tasks in the background
while also attending to something else.  Semantics are similar to JavaScript
promises, but with a syntax that is idiomatic R.")
    (license license:expat)))

(define-public r-dosnow
  (package
    (name "r-dosnow")
    (version "1.0.18")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "doSNOW" version))
       (sha256
        (base32
         "0rj72z5505cprh6wykhhiz08l9bmd966srqh2qypwivf321bvrvh"))))
    (properties `((upstream-name . "doSNOW")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-foreach" ,r-foreach)
       ("r-iterators" ,r-iterators)
       ("r-snow" ,r-snow)))
    (home-page "https://cran.r-project.org/web/packages/doSNOW")
    (synopsis "Foreach parallel adaptor for the snow package")
    (description
     "This package provides a parallel backend for the @code{%dopar%} function
using the @code{snow} package.")
    (license license:gpl2)))

(define-public r-snowfall
  (package
   (name "r-snowfall")
   (version "1.84-6.1")
   (source (origin
            (method url-fetch)
            (uri (cran-uri "snowfall" version))
            (sha256
             (base32 "13941rlw1jsdjsndp1plzj1cq5aqravizkrqn6l25r9im7rnsi2w"))))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-snow" ,r-snow)))
   (home-page "http://cran.r-project.org/web/packages/snowfall/")
   (synopsis "Easier cluster computing")
   (description "This package is a usability wrapper around snow for easier
development of parallel R programs.  This package offers e.g. extended error
checks, and additional functions.  All functions work in sequential mode, too,
if no cluster is present or wished.  The package is also designed as connector
to the cluster management tool @code{sfCluster}, but can also used without
it.")
   (license license:gpl2+)))

(define-public r-rappdirs
  (package
    (name "r-rappdirs")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rappdirs" version))
       (sha256
        (base32
         "0ji6sg3bdn5gazkq14xmmcq7jnbsyxw4lzmmbgv6526j2vn93n1g"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/rappdirs/")
    (synopsis "Determine where to save data, caches, and logs")
    (description
     "This package provides an easy way to determine which directories on the
user's computer should be used to save data, caches and logs.  It is a port of
Python's @url{https://github.com/ActiveState/appdirs,Appdirs} to R.")
    (license license:expat)))

(define-public r-renv
  (package
    (name "r-renv")
    (version "0.9.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "renv" version))
       (sha256
        (base32
         "02ayfgbpw4b58148dcxz31anaxncwczmxs1zzkskwj0rzhb2x60r"))))
    (properties `((upstream-name . "renv")))
    (build-system r-build-system)
    (home-page "https://rstudio.github.io/renv")
    (synopsis "Project environments")
    (description
     "This package provides a dependency management toolkit for R.  Using
renv, you can create and manage project-local R libraries, save the state of
these libraries to a lockfile, and later restore your library as required.
Together, these tools can help make your projects more isolated, portable, and
reproducible.")
    (license license:expat)))

(define-public r-learnr
  (package
    (name "r-learnr")
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "learnr" version))
       (sha256
        (base32
         "08xwmms6x58y3dsfbl9c6d03145hb4ij97nqr1cc9dxxilmr6x31"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-checkmate" ,r-checkmate)
       ("r-ellipsis" ,r-ellipsis)
       ("r-evaluate" ,r-evaluate)
       ("r-htmltools" ,r-htmltools)
       ("r-htmlwidgets" ,r-htmlwidgets)
       ("r-jsonlite" ,r-jsonlite)
       ("r-knitr" ,r-knitr)
       ("r-markdown" ,r-markdown)
       ("r-rappdirs" ,r-rappdirs)
       ("r-renv" ,r-renv)
       ("r-rmarkdown" ,r-rmarkdown)
       ("r-rprojroot" ,r-rprojroot)
       ("r-shiny" ,r-shiny)
       ("r-withr" ,r-withr)))
    (home-page "https://rstudio.github.io/learnr/")
    (synopsis "Interactive tutorials for R")
    (description
     "This package provides tools to create interactive tutorials using R
Markdown.  Use a combination of narrative, figures, videos, exercises, and
quizzes to create self-paced tutorials for learning about R and R packages.")
    (license license:asl2.0)))

(define-public r-analytics
  (package
    (name "r-analytics")
    (version "3.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "analytics" version))
       (sha256
        (base32
         "0js3c8lwj3knccb55nq03cbjlf4w390p9aid2mi5x80l3ayd9in1"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-car" ,r-car)
       ("r-cluster" ,r-cluster)
       ("r-fractal" ,r-fractal)
       ("r-lmtest" ,r-lmtest)
       ("r-mass" ,r-mass)
       ("r-np" ,r-np)
       ("r-powerplus" ,r-powerplus)
       ("r-robust" ,r-robust)
       ("r-trend" ,r-trend)
       ("r-tsa" ,r-tsa)
       ("r-urca" ,r-urca)
       ("r-vim" ,r-vim)))
    (home-page "https://cran.r-project.org/web/packages/analytics/")
    (synopsis "Collection of data analysis tools")
    (description
     "This package is a collection of data analysis tools.  It includes tools
for regression outlier detection in a fitted linear model, stationary
bootstrap using a truncated geometric distribution, a comprehensive test for
weak stationarity, column means by group, weighted biplots, and a heuristic to
obtain a better initial configuration in non-metric MDS.")
    (license license:gpl2)))

(define-public r-reticulate
  (package
    (name "r-reticulate")
    (version "1.14")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "reticulate" version))
       (sha256
        (base32
         "0icb9zl9zlx75njv7y1dr450k0yw2n3q2jkr4nicnphdyfn69gkn"))))
    (build-system r-build-system)
    (inputs `(("python" ,python)))
    (propagated-inputs
     `(("r-rappdirs" ,r-rappdirs)
       ("r-jsonlite" ,r-jsonlite)
       ("r-matrix" ,r-matrix)
       ("r-rcpp" ,r-rcpp)))
    (home-page "https://github.com/rstudio/reticulate")
    (synopsis "R interface to Python")
    (description
     "This package provides an interface from R to Python modules, classes,
and functions.  When calling into Python, R data types are automatically
converted to their equivalent Python types.  When values are returned from
Python to R they are converted back to R types.")
    (license license:asl2.0)))

(define-public r-bibtex
  (package
    (name "r-bibtex")
    (version "0.4.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "bibtex" version))
       (sha256
        (base32
         "140hkjzdp3033cvji861rd06z1ixgpnn4n5amygqsmhnixk8ff07"))))
    (build-system r-build-system)
    (propagated-inputs `(("r-stringr" ,r-stringr)))
    (home-page "https://github.com/romainfrancois/bibtex")
    (synopsis "Bibtex parser")
    (description "This package provides a utility for R to parse a bibtex
file.")
    (license license:gpl2+)))

(define-public r-ggseqlogo
  (package
    (name "r-ggseqlogo")
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ggseqlogo" version))
       (sha256
        (base32
         "13q6kcpxrqxqbji889fx63p0nsi08lk5yymkchig75r5k1d18ky1"))))
    (build-system r-build-system)
    (propagated-inputs `(("r-ggplot2" ,r-ggplot2)))
    (home-page "https://github.com/omarwagih/ggseqlogo")
    (synopsis "ggplot2 extension for drawing genetic sequence logos")
    (description
     "The range of functions provided by this package makes it possible to
draw highly versatile genomic sequence logos.  Features include, but are not
limited to, modifying colour schemes and fonts used to draw the logo,
generating multiple logo plots, and aiding the visualisation with annotations.
Sequence logos can easily be combined with other ggplot2 plots.")
    ;; Unspecified version of the LGPL.
    (license license:lgpl3+)))

(define-public r-ggsci
  (package
    (name "r-ggsci")
    (version "2.9")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ggsci" version))
       (sha256
        (base32
         "0g73x6grbka7ahjh6z23m3wrcifp5rdfdiasbl8lq4sp6rplxwaa"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-scales" ,r-scales)))
    (home-page "https://nanx.me/ggsci/")
    (synopsis "Scientific journal and sci-fi themed color palettes for ggplot2")
    (description
     "This package provides a collection of ggplot2 color palettes inspired by
plots in scientific journals, data visualization libraries, science fiction
movies, and TV shows.")
    (license license:gpl3)))

(define-public r-ggsignif
  (package
    (name "r-ggsignif")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ggsignif" version))
       (sha256
        (base32
         "17j9hg967k1wp9xw3x84mqss58jkb8pvlrnlchz4i1hklgykxqbg"))))
    (build-system r-build-system)
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)))
    (home-page "https://github.com/const-ae/ggsignif")
    (synopsis "Significance brackets for ggplot2")
    (description
     "Enrich your ggplots with group-wise comparisons.  This package provides
an easy way to indicate if two groups are significantly different.  Commonly
this is shown by a bracket on top connecting the groups of interest which
itself is annotated with the level of significance.  The package provides a
single layer that takes the groups for comparison and the test as arguments
and adds the annotation to the plot.")
    (license license:gpl3)))

(define-public r-ggpubr
  (package
    (name "r-ggpubr")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ggpubr" version))
       (sha256
        (base32
         "0kb3hpmnhj4mkbx1kx0kv5y22himr8dijqx7ra0h8hi0pf2l2ha7"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-cowplot" ,r-cowplot)
       ("r-dplyr" ,r-dplyr)
       ("r-ggplot2" ,r-ggplot2)
       ("r-ggrepel" ,r-ggrepel)
       ("r-ggsci" ,r-ggsci)
       ("r-ggsignif" ,r-ggsignif)
       ("r-glue" ,r-glue)
       ("r-gridextra" ,r-gridextra)
       ("r-magrittr" ,r-magrittr)
       ("r-polynom" ,r-polynom)
       ("r-purrr" ,r-purrr)
       ("r-rlang" ,r-rlang)
       ("r-scales" ,r-scales)
       ("r-tidyr" ,r-tidyr)))
    (home-page "http://www.sthda.com/english/rpkgs/ggpubr")
    (synopsis "ggplot2-based publication-ready plots")
    (description
     "The ggplot2 package is an excellent and flexible package for elegant
data visualization in R.  However the default generated plots require some
formatting before we can send them for publication.  The ggpubr package
provides some easy-to-use functions for creating and customizing ggplot2-based
publication-ready plots.")
    (license license:gpl2)))

(define-public r-ellipse
  (package
    (name "r-ellipse")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ellipse" version))
       (sha256
        (base32
         "0g82vc51m3c1k0hnpp2zla6amxxgk2mmkl8ssnsc49jv3599r6hs"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/ellipse/")
    (synopsis "Functions for drawing ellipses and ellipse-like confidence regions")
    (description
     "This package contains various routines for drawing ellipses and
ellipse-like confidence regions, implementing the plots described in Murdoch
and Chow (1996), A graphical display of large correlation matrices, The
American Statistician 50, 178-180.  There are also routines implementing the
profile plots described in Bates and Watts (1988), Nonlinear Regression
Analysis and its Applications.")
    (license license:gpl2+)))

(define-public r-flashclust
  (package
    (name "r-flashclust")
    (version "1.01-2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "flashClust" version))
       (sha256
        (base32
         "0l4lpz451ll7f7lfxmb7ds24ppzhfg1c3ypvydglcc35p2dq99s8"))))
    (properties `((upstream-name . "flashClust")))
    (build-system r-build-system)
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/flashClust/")
    (synopsis "Implementation of optimal hierarchical clustering")
    (description
     "This package provides a fast implementation of hierarchical
clustering.")
    (license license:gpl2+)))

(define-public r-factominer
  (package
    (name "r-factominer")
    (version "2.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "FactoMineR" version))
       (sha256
        (base32
         "0ldgf3daksh6lpblhqys67m4mxqx3q9s9n5plfam6dwshfik0ky6"))))
    (properties `((upstream-name . "FactoMineR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-car" ,r-car)
       ("r-cluster" ,r-cluster)
       ("r-ellipse" ,r-ellipse)
       ("r-flashclust" ,r-flashclust)
       ("r-ggplot2" ,r-ggplot2)
       ("r-ggrepel" ,r-ggrepel)
       ("r-lattice" ,r-lattice)
       ("r-leaps" ,r-leaps)
       ("r-mass" ,r-mass)
       ("r-scatterplot3d" ,r-scatterplot3d)))
    (home-page "http://factominer.free.fr")
    (synopsis "Multivariate exploratory data analysis and data mining")
    (description
     "This package provides exploratory data analysis methods to summarize,
visualize and describe datasets.  The main principal component methods are
available, those with the largest potential in terms of applications:
principal component analysis (PCA) when variables are quantitative,
correspondence analysis (CA) and multiple correspondence analysis (MCA) when
variables are categorical, Multiple Factor Analysis when variables are
structured in groups, etc. and hierarchical cluster analysis.")
    (license license:gpl2+)))

(define-public r-factoextra
  (package
    (name "r-factoextra")
    (version "1.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "factoextra" version))
       (sha256
        (base32
         "0bpsbcmp6jpa9qk53dhfzghrz98dh0h0n68brl7rjz724yjbvhn8"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-abind" ,r-abind)
       ("r-cluster" ,r-cluster)
       ("r-dendextend" ,r-dendextend)
       ("r-factominer" ,r-factominer)
       ("r-ggplot2" ,r-ggplot2)
       ("r-ggpubr" ,r-ggpubr)
       ("r-ggrepel" ,r-ggrepel)
       ("r-reshape2" ,r-reshape2)
       ("r-tidyr" ,r-tidyr)))
    (home-page "http://www.sthda.com/english/rpkgs/factoextra")
    (synopsis "Extract and visualize the results of multivariate data analyses")
    (description
     "This package provides some easy-to-use functions to extract and
visualize the output of multivariate data analyses, including
@code{PCA} (Principal Component Analysis), @code{CA} (Correspondence
Analysis), @code{MCA} (Multiple Correspondence Analysis), @code{FAMD} (Factor
Analysis of Mixed Data), @code{MFA} (Multiple Factor Analysis) and
@code{HMFA} (Hierarchical Multiple Factor Analysis) functions from different R
packages.  It contains also functions for simplifying some clustering analysis
steps and provides ggplot2-based elegant data visualization.")
    (license license:gpl2)))

(define-public r-fansi
  (package
    (name "r-fansi")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "fansi" version))
       (sha256
        (base32
         "028ywjy538psnmdnddvy5jr3idzffr4hikzr4x97x0m30g4fws9w"))))
    (build-system r-build-system)
    (native-inputs
     `(("r-knitr" ,r-knitr))) ; for vignettes
    (home-page "https://github.com/brodieG/fansi")
    (synopsis "ANSI control sequence aware string functions")
    (description
     "This package provides counterparts to R string manipulation functions
that account for the effects of ANSI text formatting control sequences.")
    (license license:gpl2+)))

(define-public r-nbclust
  (package
    (name "r-nbclust")
    (version "3.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "NbClust" version))
       (sha256
        (base32
         "1vwb48zy6ln1ddpqmfngii1i80n8qmqyxnzdp6gbaq96lakl3w3c"))))
    (properties `((upstream-name . "NbClust")))
    (build-system r-build-system)
    (home-page "https://sites.google.com/site/malikacharrad/research/nbclust-package")
    (synopsis "Determine the best number of clusters in a data set")
    (description
     "NbClust provides 30 indexes for determining the optimal number of
clusters in a data set and offers the best clustering scheme from different
results to the user.")
    (license license:gpl2)))

(define-public r-hdf5r
  (package
    (name "r-hdf5r")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "hdf5r" version))
       (sha256
        (base32
         "0hvi2cvyv6zlxgpawnmsihxclp2ln88slbrnmaxagmjswskxsrpx"))))
    (build-system r-build-system)
    (inputs
     `(("hdf5" ,hdf5)
       ("zlib" ,zlib)))
    (propagated-inputs
     `(("r-bit64" ,r-bit64)
       ("r-r6" ,r-r6)))
    (home-page "https://hhoeflin.github.io/hdf5r")
    (synopsis "Interface to the HDF5 binary data format")
    (description
     "HDF5 is a data model, library and file format for storing and managing
large amounts of data.  This package provides a nearly feature complete,
object oriented wrapper for the HDF5 API using R6 classes.  Additionally,
functionality is added so that HDF5 objects behave very similar to their
corresponding R counterparts.")
    (license license:asl2.0)))

(define-public r-itertools
  (package
    (name "r-itertools")
    (version "0.1-3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "itertools" version))
       (sha256
        (base32
         "1ls5biiva10pb1dj3ph4griykb9vam02hkrdmlr5a5wf660hg6xn"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-iterators" ,r-iterators)))
    (home-page "https://cran.r-project.org/web/packages/itertools/")
    (synopsis "Iterator tools")
    (description
     "This package provides various tools for creating iterators, many
patterned after functions in the Python @code{itertools} module, and others
patterned after functions in the snow package.")
    (license license:gpl2)))

(define-public r-polynom
  (package
    (name "r-polynom")
    (version "1.4-0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "polynom" version))
       (sha256
        (base32
         "1pflscwc0qzdf0y60j7s0dkglgmz18xajywfbn6s263idyr8idy5"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/polynom/")
    (synopsis "Functions for univariate polynomial manipulations")
    (description
     "This package provides a collection of functions to implement a class for
univariate polynomial manipulations.")
    (license license:gpl2)))

(define-public r-gbrd
  (package
    (name "r-gbrd")
    (version "0.4-11")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "gbRd" version))
       (sha256
        (base32
         "06x97rw5i6v6cgjxkfhxnw4dn7lghn5q6ra7ri5ag1x9dkfzcl82"))))
    (properties `((upstream-name . "gbRd")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/gbRd/")
    (synopsis "Utilities for processing Rd objects and files")
    (description
     "This package provides utilities for processing Rd objects and files.
Extract argument descriptions and other parts of the help pages of
functions.")
    (license license:gpl2+)))

(define-public r-rjags
  (package
    (name "r-rjags")
    (version "4-10")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rjags" version))
       (sha256
        (base32
         "1nhaim84ww8fd6m8xlpmngqcnp2qpql29ahc38366fxja3ghngmx"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-coda" ,r-coda)))
    (inputs
     `(("jags" ,jags)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://mcmc-jags.sourceforge.net")
    (synopsis "Bayesian graphical models using MCMC")
    (description
     "This package provides an R interface to the JAGS MCMC library.  JAGS is
Just Another Gibbs Sampler.  It is a program for analysis of Bayesian
hierarchical models using Markov Chain Monte Carlo (MCMC) simulation.")
    (license license:gpl2)))

(define-public r-rdpack
  (package
    (name "r-rdpack")
    (version "0.11-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Rdpack" version))
       (sha256
        (base32
         "080y15p2hl4jsq91ak2f1y2kx1iqq5c5wzyx3zyhjwp01cahy0jq"))))
    (properties `((upstream-name . "Rdpack")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bibtex" ,r-bibtex)
       ("r-gbrd" ,r-gbrd)))
    (home-page "https://github.com/GeoBosh/Rdpack")
    (synopsis "Update and manipulate Rd documentation objects")
    (description
     "This package provides functions for manipulation of R documentation
objects, including functions @code{reprompt()} and @code{ereprompt()} for
updating Rd documentation for functions, methods and classes; it also includes
Rd macros for citations and import of references from bibtex files for use in
Rd files and roxygen2 comments, as well as many functions for manipulation of
references and Rd files.")
    (license license:gpl2+)))

(define-public r-officer
  (package
    (name "r-officer")
    (version "0.3.7")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "officer" version))
       (sha256
        (base32
         "1zyrcf0xxm67bskynjac8dhil2as7rb0mmqaq30zp9lrgfwiycpz"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-digest" ,r-digest)
       ("r-magrittr" ,r-magrittr)
       ("r-r6" ,r-r6)
       ("r-uuid" ,r-uuid)
       ("r-xml2" ,r-xml2)
       ("r-zip" ,r-zip)))
    (home-page "https://davidgohel.github.io/officer")
    (synopsis "Manipulation of Word and PowerPoint documents")
    (description
     "This package provides tools to access and manipulate Word and PowerPoint
documents from R.  The package focuses on tabular and graphical reporting from
R; it also provides two functions that let users get document content into
data objects.  A set of functions lets add and remove images, tables and
paragraphs of text in new or existing documents.  When working with PowerPoint
presentations, slides can be added or removed; shapes inside slides can also
be added or removed.  When working with Word documents, a cursor can be used
to help insert or delete content at a specific location in the document.")
    (license license:gpl3)))

(define-public r-abn
  (package
    (name "r-abn")
    (version "2.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "abn" version))
       (sha256
        (base32
         "19w6bdjyp4zwqs6p0flry4qxqynf9rh8ykdrfrp61wrdf7kysw0d"))))
    (build-system r-build-system)
    (inputs
     `(("gsl" ,gsl)))
    (propagated-inputs
     `(("r-lme4" ,r-lme4)
       ("r-mass" ,r-mass)
       ("r-nnet" ,r-nnet)
       ("r-rcpp" ,r-rcpp)
       ("r-rcpparmadillo" ,r-rcpparmadillo)
       ("r-rjags" ,r-rjags)))
    (home-page "https://r-bayesian-networks.org/")
    (synopsis "Modelling multivariate data with additive bayesian networks")
    (description
     "Bayesian network analysis is a form of probabilistic graphical models
which derives from empirical data a directed acyclic graph, DAG, describing
the dependency structure between random variables.  An additive Bayesian
network model consists of a form of a DAG where each node comprises a
@dfn{generalized linear model} (GLM).  Additive Bayesian network models are
equivalent to Bayesian multivariate regression using graphical modelling, they
generalises the usual multivariable regression, GLM, to multiple dependent
variables.  This package provides routines to help determine optimal Bayesian
network models for a given data set, where these models are used to identify
statistical dependencies in messy, complex data.")
    (license license:gpl2+)))

(define-public r-acd
  (package
    (name "r-acd")
    (version "1.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ACD" version))
       (sha256
        (base32
         "1a67bi3hklq8nlc50r0qnyr4k7m9kpvijy8sqqpm54by5hsysfd6"))))
    (properties `((upstream-name . "ACD")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/ACD/")
    (synopsis "Categorical data analysis with complete or missing responses")
    (description
     "This package provides tools for categorical data analysis with complete
or missing responses.")
    (license license:gpl2+)))

(define-public r-acdm
  (package
    (name "r-acdm")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ACDm" version))
       (sha256
        (base32
         "0b4f02ga5ra66mbrm79g0bnlzmii82rks9kmxixxqgf18yhlyjil"))))
    (properties `((upstream-name . "ACDm")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-dplyr" ,r-dplyr)
       ("r-ggplot2" ,r-ggplot2)
       ("r-plyr" ,r-plyr)
       ("r-rsolnp" ,r-rsolnp)
       ("r-zoo" ,r-zoo)))
    (home-page "https://cran.r-project.org/web/packages/ACDm/")
    (synopsis "Tools for Autoregressive Conditional Duration Models")
    (description
     "ACDm is a package for Autoregressive Conditional Duration (ACD, Engle
and Russell, 1998) models.  It creates trade, price or volume durations from
transactions (tic) data, performs diurnal adjustments, fits various ACD models
and tests them.")
    (license license:gpl2+)))

(define-public r-overlap
  (package
    (name "r-overlap")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "overlap" version))
       (sha256
        (base32
         "1j3m6ir1chdz0si2fhcw6gs7c9h09bv0chz18rpzxsywww6d4rzy"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/overlap/")
    (synopsis "Estimates of coefficient of overlapping for animal activity patterns")
    (description
     "This package provides functions to fit kernel density functions to data
on temporal activity patterns of animals; estimate coefficients of overlapping
of densities for two species; and calculate bootstrap estimates of confidence
intervals.")
    (license license:gpl3+)))

(define-public r-snakecase
  (package
    (name "r-snakecase")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "snakecase" version))
       (sha256
        (base32
         "1ky1x2cp5rd0ffd9m1fji9sq4z4jsrpxzg30brw8bb4ihfjj114r"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-stringi" ,r-stringi)
       ("r-stringr" ,r-stringr)))
    (home-page "https://github.com/Tazinho/snakecase")
    (synopsis "Convert strings into any case")
    (description
     "This package provides a consistent, flexible and easy to use tool to
parse and convert strings into cases like snake or camel among others.")
    (license license:gpl3)))

(define-public r-prediction
  (package
    (name "r-prediction")
    (version "0.3.14")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "prediction" version))
       (sha256
        (base32
         "0awlq5lxfia6m2b91w73rksp93rbwv5gwqb36wbji4rgq41rzbrx"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-data-table" ,r-data-table)))
    (home-page "https://github.com/leeper/prediction")
    (synopsis "Tidy, type-safe prediction methods")
    (description
     "This package provides the @code{prediction()} function, a type-safe
alternative to @code{predict()} that always returns a data frame.  The package
currently supports common model types (e.g., @code{\"lm\"}, @code{\"glm\"})
from the @code{stats} package, as well as numerous other model classes from
other add-on packages.")
    (license license:expat)))

(define-public r-insight
  (package
    (name "r-insight")
    (version "0.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "insight" version))
       (sha256
        (base32
         "0fjf7dwpv1a7qfbzixppg348z1ksq19kdjm08vcb2am7w0k3plcj"))))
    (build-system r-build-system)
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://easystats.github.io/insight/")
    (synopsis "Easy access to model information for various model objects")
    (description
     "This package provides a tool to provide an easy, intuitive and
consistent access to information contained in various R models, like model
formulas, model terms, information about random effects, data that was used to
fit the model or data from response variables.  The package mainly revolves
around two types of functions: Functions that find (the names of) information,
starting with @code{find_}, and functions that get the underlying data,
starting with @code{get_}.  The package has a consistent syntax and works with
many different model objects, where otherwise functions to access these
information are missing.")
    (license license:gpl3)))

(define-public r-sjlabelled
  (package
    (name "r-sjlabelled")
    (version "1.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "sjlabelled" version))
       (sha256
        (base32
         "1amq7i9sxf0pkxhskqc53xq5wvc9rdxm7cxyb4b6xh6qsskjnlsj"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-haven" ,r-haven)
       ("r-insight" ,r-insight)))
    (home-page "https://github.com/strengejacke/sjlabelled")
    (synopsis "Labelled data utility functions")
    (description
     "This package provides a collection of functions dealing with labelled
data, like reading and writing data between R and other statistical software
packages.  This includes easy ways to get, set or change value and variable
label attributes, to convert labelled vectors into factors or numeric (and
vice versa), or to deal with multiple declared missing values.")
    (license license:gpl3)))

(define-public r-sjmisc
  (package
    (name "r-sjmisc")
    (version "2.8.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "sjmisc" version))
       (sha256
        (base32
         "0w8l9grmp4q775jrf4q6rxx36ld5daz9b0gdxyyh42xfihk6m62h"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-dplyr" ,r-dplyr)
       ("r-insight" ,r-insight)
       ("r-magrittr" ,r-magrittr)
       ("r-purrr" ,r-purrr)
       ("r-rlang" ,r-rlang)
       ("r-sjlabelled" ,r-sjlabelled)
       ("r-tidyselect" ,r-tidyselect)))
    (home-page "https://github.com/strengejacke/sjmisc")
    (synopsis "Data and variable transformation functions")
    (description
     "This package is a collection of miscellaneous utility functions,
supporting data transformation tasks like recoding, dichotomizing or grouping
variables, setting and replacing missing values.  The data transformation
functions also support labelled data, and all integrate seamlessly into a
tidyverse workflow.")
    (license license:gpl3)))

(define-public r-nortest
  (package
    (name "r-nortest")
    (version "1.0-4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "nortest" version))
       (sha256
        (base32
         "17r0wpz72z9312c70nwi1i1kp1v9fm1h6jg7q5cx1mc1h420m1d3"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/nortest/")
    (synopsis "Tests for normality")
    (description
     "This package provides five omnibus tests for testing the composite
hypothesis of normality.")
    (license license:gpl2+)))

(define-public r-moonbook
  (package
    (name "r-moonbook")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "moonBook" version))
       (sha256
        (base32
         "0hys56mwbm776ff7dibi8wzyf69qiais9rs1jazv79lk6h56s9s6"))))
    (properties `((upstream-name . "moonBook")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-magrittr" ,r-magrittr)
       ("r-nortest" ,r-nortest)
       ("r-purrr" ,r-purrr)
       ("r-sjmisc" ,r-sjmisc)
       ("r-stringr" ,r-stringr)
       ("r-survival" ,r-survival)))
    (home-page "https://github.com/cardiomoon/moonBook")
    (synopsis "Functions and datasets for the book by Keon-Woong Moon")
    (description
     "This package provides several analysis-related functions for the book
entitled \"R statistics and graph for medical articles\" (written in Korean),
version 1, by Keon-Woong Moon with Korean demographic data with several plot
functions.")
    (license license:gpl2)))

(define-public r-flextable
  (package
    (name "r-flextable")
    (version "0.5.9")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "flextable" version))
       (sha256
        (base32
         "1rkz0nhwjy1l8sb0kmna5v492n2ydnk7gydswnmf88r8vfyjczhw"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-base64enc" ,r-base64enc)
       ("r-data-table" ,r-data-table)
       ("r-gdtools" ,r-gdtools)
       ("r-htmltools" ,r-htmltools)
       ("r-knitr" ,r-knitr)
       ("r-officer" ,r-officer)
       ("r-rlang" ,r-rlang)
       ("r-rmarkdown" ,r-rmarkdown)
       ("r-xml2" ,r-xml2)))
    (home-page "https://davidgohel.github.io/flextable")
    (synopsis "Functions for tabular reporting")
    (description
     "This package provides tools to create pretty tables for HTML documents
and other formats.  Functions are provided to let users create tables, modify
and format their content.  It extends the @code{officer} package and can be
used within R markdown documents when rendering to HTML and to Word
documents.")
    (license license:gpl3)))

(define-public r-writexl
  (package
    (name "r-writexl")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "writexl" version))
       (sha256
        (base32
         "09fhdip6igcg97fjx4c7727cx2lb49l4d74l4i8rg2bag2s5lrj3"))))
    (build-system r-build-system)
    (inputs `(("zlib" ,zlib)))
    (home-page "https://github.com/ropensci/writexl")
    (synopsis "Export data frames to xlsx format")
    (description
     "This package provides a data frame to xlsx exporter based on
libxlsxwriter.")
    (license license:bsd-2)))

(define-public r-biasedurn
  (package
    (name "r-biasedurn")
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
    (home-page "https://cran.r-project.org/web/packages/BiasedUrn/")
    (synopsis "Biased Urn model distributions")
    (description
     "This package provides statistical models of biased sampling in the form
of univariate and multivariate noncentral hypergeometric distributions,
including Wallenius' noncentral hypergeometric distribution and Fisher's
noncentral hypergeometric distribution (also called extended hypergeometric
distribution).")
    (license license:gpl3)))

(define-public r-goplot
  (package
    (name "r-goplot")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "GOplot" version))
       (sha256
        (base32
         "1y8dv0kbzpr9za91njw0x233vx5d13vqml9hmpddcyi9s6va5nix"))))
    (properties `((upstream-name . "GOplot")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggdendro" ,r-ggdendro)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gridextra" ,r-gridextra)
       ("r-rcolorbrewer" ,r-rcolorbrewer)))
    (home-page "https://github.com/wencke/wencke.github.io")
    (synopsis "Visualization of functional analysis data")
    (description
     "This package provides an implementation of multilayered visualizations
for enhanced graphical representation of functional analysis data.  It
combines and integrates omics data derived from expression and functional
annotation enrichment analyses.  Its plotting functions have been developed
with an hierarchical structure in mind: starting from a general overview to
identify the most enriched categories (modified bar plot, bubble plot) to a
more detailed one displaying different types of relevant information for the
molecules in a given set of categories (circle plot, chord plot, cluster plot,
Venn diagram, heatmap).")
    (license license:gpl2)))

(define-public r-getopt
  (package
    (name "r-getopt")
    (version "1.20.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "getopt" version))
       (sha256
        (base32
         "0zzmzgwl9a4y3s34600vmih22d6y32294f9bvxrnmffnvkgmy7sk"))))
    (build-system r-build-system)
    (home-page "https://github.com/trevorld/getopt")
    (synopsis "Command-line option processor for R")
    (description
     "This package is designed to be used with Rscript to write shebang
scripts that accept short and long options.  Many users will prefer to
use the packages @code{optparse} or @code{argparse} which add extra
features like automatically generated help options and usage texts,
support for default values, positional argument support, etc.")
    (license license:gpl2+)))

(define-public r-findpython
  (package
    (name "r-findpython")
    (version "1.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "findpython" version))
       (sha256
        (base32
         "0icifm4z6hhpmcjrg75a875iph0ci890ss02kdv3725pijc236iy"))))
    (build-system r-build-system)
    (home-page "https://github.com/trevorld/findpython")
    (synopsis "Functions to find an acceptable Python binary")
    (description
     "This package was designed to find an acceptable Python binary that
matches version and feature constraints.")
    (license license:expat)))

;; This in not the same as "r-argparser"
(define-public r-argparse
  (package
    (name "r-argparse")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "argparse" version))
       (sha256
        (base32
         "1as7h6z7kzgv0fqzpnp76qbm96b4jcd37azd58b7rz0l1n94764l"))))
    (build-system r-build-system)
    (inputs `(("python" ,python)))
    (propagated-inputs
     `(("r-findpython" ,r-findpython)
       ("r-jsonlite" ,r-jsonlite)
       ("r-r6" ,r-r6)))
    (home-page "https://github.com/trevorld/argparse")
    (synopsis "Command line optional and positional argument parser")
    (description
     "This package provides a command line parser to be used with Rscript to
write shebang scripts that gracefully accept positional and optional arguments
and automatically generate usage notices.")
    (license license:gpl2+)))

(define-public r-hash
  (package
    (name "r-hash")
    (version "2.2.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "hash" version))
       (sha256
        (base32
         "0b3fl0rvgwb992knl81vm99lsldg5clvaqjh6mamm6zqmb6dz056"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/hash/")
    (synopsis "Implementation of hash/associated arrays/dictionaries")
    (description
     "This package implements a data structure similar to hashes in Perl and
dictionaries in Python but with a purposefully R flavor.  For objects of
appreciable size, access using hashes outperforms native named lists and
vectors.")
    (license license:gpl2+)))

(define-public r-orddom
  (package
    (name "r-orddom")
    (version "3.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "orddom" version))
       (sha256
        (base32
         "165axs15fvwhrp89xd87l81q3h2qjll1vrwcsap645cwvb85nwsh"))))
    (build-system r-build-system)
    (propagated-inputs `(("r-psych" ,r-psych)))
    (home-page "https://cran.r-project.org/web/packages/orddom/")
    (synopsis "Ordinal dominance statistics")
    (description
     "This package provides tools to compute ordinal, statistics and effect
sizes as an alternative to mean comparison: Cliff's delta or success rate
difference (SRD), Vargha and Delaney's A or the Area Under a Receiver
Operating Characteristic Curve (AUC), the discrete type of McGraw & Wong's
Common Language Effect Size (CLES) or Grissom & Kim's Probability of
Superiority (PS), and the Number needed to treat (NNT) effect size.  Moreover,
comparisons to Cohen's d are offered based on Huberty & Lowman's Percentage of
Group (Non-)Overlap considerations.")
    (license license:gpl2)))

(define-public r-deriv
  (package
    (name "r-deriv")
    (version "4.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Deriv" version))
       (sha256
        (base32
         "03mlfy8jzzzbh2l18gnmw0a71n9savx4cw72yhkxq93v2xj8fy3n"))))
    (properties `((upstream-name . "Deriv")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/Deriv")
    (synopsis "Symbolic differentiation")
    (description
     "This package provides an R-based solution for symbolic differentiation.
It admits user-defined functions as well as function substitution in arguments
of functions to be differentiated.  Some symbolic simplification is part of
the work.")
    (license license:gpl3+)))

(define-public r-doby
  (package
    (name "r-doby")
    (version "4.6.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "doBy" version))
       (sha256
        (base32
         "1ckazh701b4ilg8bj17ji903538jmb49d997gm49ah5j5jc1x0g7"))))
    (properties `((upstream-name . "doBy")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-broom" ,r-broom)
       ("r-deriv" ,r-deriv)
       ("r-dplyr" ,r-dplyr)
       ("r-magrittr" ,r-magrittr)
       ("r-mass" ,r-mass)
       ("r-matrix" ,r-matrix)
       ("r-plyr" ,r-plyr)
       ("r-pbkrtest" ,r-pbkrtest)
       ("r-tibble" ,r-tibble)))
    (home-page "http://people.math.aau.dk/~sorenh/software/doBy/")
    (synopsis "Groupwise statistics, LSmeans, linear contrasts, and utilities")
    (description
     "This package contains:

@itemize
@item facilities for working with grouped data: @code{do}
  something to data stratified @code{by} some variables.
@item implementations of least-squares means, general linear contrasts, and
@item miscellaneous other utilities.
@end itemize\n")
    (license license:gpl2+)))

(define-public r-refgenome
  (package
    (name "r-refgenome")
    (version "1.7.7")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "refGenome" version))
       (sha256
        (base32
         "1za89bn3am1zgvm641qi1ab6kaqpll4rb9p9f1sjwvcgqq6065g5"))))
    (properties `((upstream-name . "refGenome")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-dbi" ,r-dbi)
       ("r-doby" ,r-doby)
       ("r-rsqlite" ,r-rsqlite)))
    (home-page "https://cran.r-project.org/web/packages/refGenome/")
    (synopsis
     "Gene and splice site annotation using annotation data from Ensembl and UCSC")
    (description
     "This package contains functionality for importing and managing of
downloaded genome annotation data from the Ensembl genome browser (European
Bioinformatics Institute) and from the UCSC genome browser (University of
California, Santa Cruz) and annotation routines for genomic positions and
splice site positions.")
    (license license:gpl2)))

(define-public r-basix
  (package
    (name "r-basix")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "BASIX" version))
       (sha256
        (base32
         "18dkvv1iwskfnlpl6xridcgqpalbbpm2616mvc3hfrc0b26v01id"))))
    (properties `((upstream-name . "BASIX")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/BASIX/")
    (synopsis "Efficient C/C++ toolset for R")
    (description
     "BASIX provides some efficient C/C++ implementations of native R
procedures to speed up calculations in R.")
    (license license:gpl2)))

(define-public r-blockfest
  (package
    (name "r-blockfest")
    (version "1.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "BlockFeST" version))
       (sha256
        (base32
         "0hj7a5as7nxbgjac7lbj6qfwffx3g8x8phpf9a55f1c9cdzi73a5"))))
    (properties `((upstream-name . "BlockFeST")))
    (build-system r-build-system)
    (propagated-inputs `(("r-basix" ,r-basix)))
    (home-page "https://cran.r-project.org/web/packages/BlockFeST/")
    (synopsis "Bayesian calculation of region-specific fixation index")
    (description
     "This package provides an R implementation of an extension of the
BayeScan software for codominant markers, adding the option to group
individual SNPs into pre-defined blocks.  A typical application of this new
approach is the identification of genomic regions, genes, or gene sets
containing one or more SNPs that evolved under directional selection.")
    (license license:gpl2)))

(define-public r-proc
  (package
    (name "r-proc")
    (version "1.16.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "pROC" version))
       (sha256
        (base32
         "0qkp1byl2xspxaaf0by6mvvrhg7wlz6fxmynz2hkh0ds24w7ig9m"))))
    (properties `((upstream-name . "pROC")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-plyr" ,r-plyr)
       ("r-rcpp" ,r-rcpp)))
    (home-page "http://expasy.org/tools/pROC/")
    (synopsis "Display and analyze ROC curves")
    (description
     "This package provides tools for visualizing, smoothing and comparing
receiver operating characteristic (ROC curves).  The area under the
curve (AUC) can be compared with statistical tests based on U-statistics or
bootstrap.  Confidence intervals can be computed for (p)AUC or ROC curves.")
    (license license:gpl3+)))

(define-public r-rootsolve
  (package
    (name "r-rootsolve")
    (version "1.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rootSolve" version))
       (sha256
        (base32
         "0rj7c4zcrzgz7sb0vgvh7swpfafnw4040cxp7ypas3s8fnihn54l"))))
    (properties `((upstream-name . "rootSolve")))
    (build-system r-build-system)
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/rootSolve/")
    (synopsis "Tools for the analysis of ordinary differential equations")
    (description
     "This package provides routines to find the root of nonlinear functions,
and to perform steady-state and equilibrium analysis of @dfn{ordinary
differential equations} (ODE).  It includes routines that:

@enumerate
@item generate gradient and jacobian matrices (full and banded),
@item find roots of non-linear equations by the Newton-Raphson method,
@item estimate steady-state conditions of a system of (differential) equations
  in full, banded or sparse form, using the Newton-Raphson method, or by
  dynamically running,
@item solve the steady-state conditions for uni- and multicomponent 1-D, 2-D,
  and 3-D partial differential equations, that have been converted to ordinary
  differential equations by numerical differencing (using the method-of-lines
  approach).
@end enumerate\n")
    (license license:gpl2+)))

(define-public r-abcanalysis
  (package
    (name "r-abcanalysis")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ABCanalysis" version))
       (sha256
        (base32 "0wac1ksmnxa36v99ca4hv8k0rsh3igwpcllmlv9wf7i9kgqviqwi"))))
    (properties `((upstream-name . "ABCanalysis")))
    (build-system r-build-system)
    (propagated-inputs `(("r-plotrix" ,r-plotrix)))
    (home-page "https://www.uni-marburg.de/fb12/arbeitsgruppen/datenbionik/software-en/")
    (synopsis "Computed ABC Analysis")
    (description
     "Multivariate data sets often differ in several factors or derived statistical
parameters, which have to be selected for a valid interpretation.  Basing this
selection on traditional statistical limits leads occasionally to the perception
of losing information from a data set.  This package provides tools to calculate
these limits on the basis of the mathematical properties of the distribution of
the analyzed items.")
    (license license:gpl3)))

(define-public r-slam
  (package
    (name "r-slam")
    (version "0.1-47")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "slam" version))
       (sha256
        (base32 "12fggw2c7hz3bpvsaqm24g3r6lbpq6jgli24g7x5j859iak5cqv9"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/slam/")
    (synopsis "Sparse lightweight arrays and matrices")
    (description
     "This package contains data structures and algorithms for sparse arrays and matrices,
based on index arrays and simple triplet representations, respectively.")
    (license license:gpl2)))

(define-public r-manipulatewidget
  (package
    (name "r-manipulatewidget")
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "manipulateWidget" version))
       (sha256
        (base32 "1vi71sjh7z1a880wffk8qqw7iysvk42q78giqxmm2sqz2a912qlx"))))
    (properties
     `((upstream-name . "manipulateWidget")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-base64enc" ,r-base64enc)
       ("r-codetools" ,r-codetools)
       ("r-htmltools" ,r-htmltools)
       ("r-htmlwidgets" ,r-htmlwidgets)
       ("r-knitr" ,r-knitr)
       ("r-miniui" ,r-miniui)
       ("r-shiny" ,r-shiny)
       ("r-webshot" ,r-webshot)))
    (home-page "https://github.com/rte-antares-rpackage/manipulateWidget/")
    (synopsis "Add even more interactivity to interactive charts")
    (description
     "This package lets you create in just a few lines of R code a nice user interface to
modify the data or the graphical parameters of one or multiple interactive
charts.  It is useful to quickly explore visually some data or for package
developers to generate user interfaces easy to maintain.")
    (license license:gpl2+)))

(define-public r-a3
  (package
    (name "r-a3")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "A3" version))
       (sha256
        (base32 "017hq9pjsv1h9i7cqk5cfx27as54shlhdsdvr6jkhb8jfkpdb6cw"))))
    (properties `((upstream-name . "A3")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-pbapply" ,r-pbapply)
       ("r-xtable" ,r-xtable)))
    (home-page "https://cran.r-project.org/web/packages/A3/")
    (synopsis "Error metrics for predictive models")
    (description
     "This package supplies tools for tabulating and analyzing the results of predictive
models.  The methods employed are applicable to virtually any predictive model
and make comparisons between different methodologies straightforward.")
    (license license:gpl2+)))

(define-public r-infotheo
  (package
    (name "r-infotheo")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "infotheo" version))
       (sha256
        (base32
         "18xacczfq3z3xpy434js4nf3l19lczngzd0lq26wh22pvg1yniwv"))))
    (build-system r-build-system)
    (home-page "http://homepage.meyerp.com/software")
    (synopsis "Information-theoretic measures")
    (description
     "This package implements various measures of information theory based on
several entropy estimators.")
    (license license:gpl3+)))

(define-public r-abcoptim
  (package
    (name "r-abcoptim")
    (version "0.15.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ABCoptim" version))
       (sha256
        (base32 "1ih0xk88qhsmpvnxf56041wx5sk8as2f4f2gdnpnwdym9mbr9n4b"))))
    (properties `((upstream-name . "ABCoptim")))
    (build-system r-build-system)
    (propagated-inputs `(("r-rcpp" ,r-rcpp)))
    (home-page "https://github.com/gvegayon/ABCoptim/")
    (synopsis "Optimization of Artificial Bee Colony algorithm")
    (description
     "Artificial Bee Colony (ABC) is one of the most recently defined algorithms by Dervis
Karaboga in 2005, motivated by the intelligent behavior of honey bees.  It is as
simple as Particle Swarm Optimization (PSO) and Differential Evolution (DE)
algorithms, and uses only common control parameters such as colony size and
maximum cycle number.  The @code{r-abcoptim} implements the Artificial bee
colony optimization algorithm @url{http://mf.erciyes.edu.tr/abc/pub/tr06_2005.pdf}.
  This version is a work-in-progress and is written in R code.")
    (license license:expat)))

(define-public r-abcp2
  (package
    (name "r-abcp2")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ABCp2" version))
       (sha256
        (base32 "1s2skkxpzss7c29i8600psgrp0hl46jcrxqrmy2b4db8hc0kcnbx"))))
    (properties `((upstream-name . "ABCp2")))
    (build-system r-build-system)
    (propagated-inputs `(("r-mass" ,r-mass)))
    (home-page "https://cran.r-project.org/web/packages/ABCp2/")
    (synopsis "Approximate Bayesian Computational Model for Estimating P2")
    (description
     "This package tests the goodness of fit of a distribution of offspring to the Normal,
Poisson, and Gamma distribution and estimates the proportional paternity of the
second male (P2) based on the best fit distribution.")
    (license license:gpl2)))

(define-public r-abcrf
  (package
    (name "r-abcrf")
    (version "1.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "abcrf" version))
       (sha256
        (base32 "1ghbd24yhqy3xhdxas6ccn84nkavqpgldx5ck8kijknc7qjm8k27"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-doparallel" ,r-doparallel)
       ("r-foreach" ,r-foreach)
       ("r-mass" ,r-mass)
       ("r-matrixstats" ,r-matrixstats)
       ("r-ranger" ,r-ranger)
       ("r-rcpp" ,r-rcpp)
       ("r-rcpparmadillo" ,r-rcpparmadillo)
       ("r-readr" ,r-readr)
       ("r-stringr" ,r-stringr)))
    (home-page "https://cran.r-project.org/web/packages/abcrf/")
    (synopsis "Approximate bayesian computation via random forests")
    (description
     "This package performs approximate bayesian computation (ABC) model choice and
parameter inference via random forests.  This machine learning tool named random
forests (RF) can conduct selection among the highly complex models covered by
ABC algorithms.")
    (license license:gpl2+)))

(define-public r-abctools
  (package
    (name "r-abctools")
    (version "1.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "abctools" version))
       (sha256
        (base32 "07s9dg10i8lsxl73b4n2hynca2fjgb0ykb0dz8c3zv6cgw3cyx97"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-abc" ,r-abc)
       ("r-abind" ,r-abind)
       ("r-hmisc" ,r-hmisc)
       ("r-plyr" ,r-plyr)))
    (home-page "https://github.com/dennisprangle/abctools/")
    (synopsis "Tools for ABC analyses")
    (description
     "This @code{r-abctools} package provides tools for approximate Bayesian computation
including summary statistic selection and assessing coverage.  This includes
recent dimension reduction algorithms to tune the choice of summary statistics,
and coverage methods to tune the choice of threshold.")
    (license license:gpl2+)))

(define-public r-ggstance
  (package
    (name "r-ggstance")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ggstance" version))
       (sha256
        (base32 "0kdksay61hyb6612b07r84chh7a9aibjyclk3qcypvr9aang8hkh"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-plyr" ,r-plyr)
       ("r-rlang" ,r-rlang)
       ("r-withr" ,r-withr)))
    (home-page "https://cran.r-project.org/web/packages/ggstance/")
    (synopsis "Horizontal and vertical versions of @code{r-ggplot2}")
    (description
     "This package is a @code{r-ggplot2} extension that provides flipped components:
@enumerate
@item horizontal versions of @code{r-ggplot2} stats and @code{r-ggplot2} geoms;
@item vertical versions of @code{r-ggplot2} positions.
@end enumerate")
    (license license:gpl3)))

(define-public r-mosaiccore
  (package
    (name "r-mosaiccore")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "mosaicCore" version))
       (sha256
        (base32 "1klw97h6lchw1cpcl8s637ikcl428cckmjq0czi7mibh9q9mw72z"))))
    (properties `((upstream-name . "mosaicCore")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-dplyr" ,r-dplyr)
       ("r-lazyeval" ,r-lazyeval)
       ("r-mass" ,r-mass)
       ("r-rlang" ,r-rlang)
       ("r-tidyr" ,r-tidyr)))
    (home-page "https://github.com/ProjectMOSAIC/mosaicCore/")
    (synopsis "Common utilities for mosaic family packages")
    (description
     "Common utilities used in other Mosaic family packages are collected here.")
    (license license:gpl2+)))

(define-public r-ggformula
  (package
    (name "r-ggformula")
    (version "0.9.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ggformula" version))
       (sha256
        (base32 "04vdhg1bbc1psrx9ggaphz7cx4fw5xsmhkqpqfcg2w4ba2bjy46f"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggforce" ,r-ggforce)
       ("r-ggplot2" ,r-ggplot2)
       ("r-ggstance" ,r-ggstance)
       ("r-magrittr" ,r-magrittr)
       ("r-mosaiccore" ,r-mosaiccore)
       ("r-rlang" ,r-rlang)
       ("r-stringr" ,r-stringr)
       ("r-tibble" ,r-tibble)))
    (home-page "https://github.com/ProjectMOSAIC/ggformula/")
    (synopsis "Formula interface for the @code{r-ggplot2}")
    (description
     "The @code{r-ggformula} introduces a family of graphics functions, gf_point(),
gf_density(), and so on, bring the formula interface to ggplot().  This captures
and extends the excellent simplicity of the lattice-graphics formula interface,
while providing the intuitive capabilities of @code{r-ggplot2}.")
    (license license:expat)))

(define-public r-mosaicdata
  (package
    (name "r-mosaicdata")
    (version "0.17.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "mosaicData" version))
       (sha256
        (base32 "04z0mdm52mykqsxsinhmsihn181zf6cw321gayk2rjp7lj7mwdq9"))))
    (properties `((upstream-name . "mosaicData")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/mosaicData/")
    (synopsis "Data sets for project Mosaic")
    (description
     "This package provides data sets from project Mosaic @url{http://mosaic-web.org}
used to teach mathematics, statistics, computation and modeling.")
    (license license:gpl2+)))

(define-public r-raster
  (package
    (name "r-raster")
    (version "3.0-12")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "raster" version))
       (sha256
        (base32
         "0rrbsigkqxsdic8fly6nrsc79zsliwvr1x2b4xqpl9d34vr50dvg"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rcpp" ,r-rcpp)
       ("r-sp" ,r-sp)))
    (home-page "https://www.rspatial.org/")
    (synopsis "Geographic data analysis and modeling")
    (description
     "The package implements basic and high-level functions for reading,
writing, manipulating, analyzing and modeling of gridded spatial data.
Processing of very large files is supported.")
    (license license:gpl3+)))

(define-public r-mosaic
  (package
   (name "r-mosaic")
   (version "1.4.0")
   (source
    (origin
     (method url-fetch)
     (uri (cran-uri "mosaic" version))
     (sha256
      (base32 "10jbrg8kli00kfgbh2f67bymm5cnlancc9dplb1j7fl552yjddn2"))))
   (build-system r-build-system)
   (propagated-inputs
    `(("r-broom" ,r-broom)
      ("r-dplyr" ,r-dplyr)
      ("r-ggdendro" ,r-ggdendro)
      ("r-ggformula" ,r-ggformula)
      ("r-ggplot2" ,r-ggplot2)
      ("r-ggrepel" ,r-ggrepel)
      ("r-glue" ,r-glue)
      ("r-gridextra" ,r-gridextra)
      ("r-lattice" ,r-lattice)
      ("r-latticeextra" ,r-latticeextra)
      ("r-lazyeval" ,r-lazyeval)
      ("r-mass" ,r-mass)
      ("r-matrix" ,r-matrix)
      ("r-mosaiccore" ,r-mosaiccore)
      ("r-mosaicdata" ,r-mosaicdata)
      ("r-readr" ,r-readr)
      ("r-tidyr" ,r-tidyr)))
   (native-inputs
    `(("r-knitr" ,r-knitr)))
   (home-page "https://github.com/ProjectMOSAIC/mosaic/")
   (synopsis "Mathematics, statistics, and computation teaching utilities")
   (description
    "This package contain data sets and utilities from
@url{http://mosaic-web.org, Project MOSAIC} used to teach mathematics,
statistics, computation and modeling.  Project MOSAIC is a community of
educators working to tie together aspects of quantitative work that students
in science, technology, engineering and mathematics will need in their
professional lives, but which are usually taught in isolation, if at all.")
   (license license:gpl2+)))

(define-public r-abd
  (package
    (name "r-abd")
    (version "0.2-8")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "abd" version))
       (sha256
        (base32 "191gspqzdv573vaw624ri0f5cm6v4j524bjs74d4a1hn3kn6r9b7"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-lattice" ,r-lattice)
       ("r-mosaic" ,r-mosaic)
       ("r-nlme" ,r-nlme)))
    (home-page "https://cran.r-project.org/web/packages/abd/")
    (synopsis "Analysis of biological data")
    (description
     "The @code{r-abd} package contains data sets and sample code for the Analysis of
biological data by Michael Whitlock and Dolph Schluter.")
    (license license:gpl2)))

(define-public r-svgui
  (package
    (name "r-svgui")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "svGUI" version))
       (sha256
        (base32 "1r7ab0p4yr8q03gj02hmj7k1ghksgkg4nx750c0ajfs2q9y1dxfc"))))
    (properties `((upstream-name . "svGUI")))
    (build-system r-build-system)
    (home-page "https://github.com/SciViews/svGUI/")
    (synopsis "Functions for managing GUI clients in R")
    (description
     "The SciViews @code{svGUI} package eases the management of Graphical User
Interfaces (GUI) in R.  It is independent from any particular GUI widgets.  It
centralizes info about GUI elements currently used, and it dispatches GUI
calls to the particular toolkits in use in function of the context.")
    (license license:gpl2)))

(define-public r-svdialogs
  (package
    (name "r-svdialogs")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "svDialogs" version))
       (sha256
        (base32 "0xqppydfawnwk84kb5qiybwbcmv38vn4imgz01mz2pnq4xb80p97"))))
    (properties `((upstream-name . "svDialogs")))
    (build-system r-build-system)
    (inputs
     `(("yad" ,yad)
       ("zenity" ,zenity)))
    (propagated-inputs
     `(("r-rstudioapi" ,r-rstudioapi)
       ("r-svgui" ,r-svgui)))
    (home-page "https://github.com/SciViews/svDialogs/")
    (synopsis "Portable dialog boxes")
    (description
     "This package helps to construct standard dialog boxes for your GUI, including
message boxes, input boxes, list, file or directory selection, and others.  In
case R cannot display GUI dialog boxes, a simpler command line version of these
interactive elements is also provided as a fallback solution.")
    (license license:gpl2)))

(define-public r-abe
  (package
    (name "r-abe")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "abe" version))
       (sha256
        (base32
         "1f19h3xzzmjhvwc1rrb8z0rai3ip03y4gdi2gg9bfr5sg2nfklk6"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/abe/")
    (synopsis "Augmented backward elimination")
    (description
     "This package performs augmented backward elimination and checks the
stability of the obtained model.  Augmented backward elimination combines
significance or information based criteria with the change in estimate to
either select the optimal model for prediction purposes or to serve as a tool
to obtain a practically sound, highly interpretable model.")
    (license license:gpl2+)))

(define-public r-abf2
  (package
    (name "r-abf2")
    (version "0.7-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "abf2" version))
       (sha256
        (base32 "0d65mc1w4pbiv7xaqzdlw1bfsxf25587rv597hh41vs0j0zlfpxx"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/abf2/")
    (synopsis "Load gap-free axon @code{r-abf2} files")
    (description
     "This package loads electrophysiology data from ABF2 files, as created by
Axon Instruments/Molecular Devices software.  Only files recorded in gap-free
mode are currently supported.")
    (license license:artistic2.0)))

(define-public r-abhgenotyper
  (package
    (name "r-abhgenotyper")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ABHgenotypeR" version))
       (sha256
        (base32 "08cpmnaaxsm5c5bjifnfxdlvg5inrf13biqpcl2yq5zpqjmiki0l"))))
    (properties `((upstream-name . "ABHgenotypeR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-reshape2" ,r-reshape2)))
    (home-page "https://github.com/StefanReuscher/ABHgenotypeR/")
    (synopsis "Visualize and manipulate ABH genotypes")
    (description
     "The @code{r-abhgenotyper} package provides simple imputation,
error-correction and plotting capacities for genotype data.  The package is
supposed to serve as an intermediate but independent analysis tool between the
TASSEL GBS pipeline and the @code{r-qtl} package.  It provides functionalities
not found in either TASSEL or @code{r-qtl} in addition to visualization of
genotypes as \"graphical genotypes\".")
    (license license:gpl3)))

(define-public r-furrr
  (package
    (name "r-furrr")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "furrr" version))
       (sha256
        (base32
         "1ld9aa9hydna94hgm6p91zjbfv1dz1vsgchjlpknkg6irbvkfafx"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-future" ,r-future)
       ("r-globals" ,r-globals)
       ("r-purrr" ,r-purrr)
       ("r-rlang" ,r-rlang)))
    (home-page "https://github.com/DavisVaughan/furrr")
    (synopsis "Apply mapping functions in parallel using futures")
    (description
     "This package provides implementations of the family of @code{map()}
functions from the @code{purrr} package that can be resolved using any
@code{future}-supported backend, e.g. parallel on the local machine or
distributed on a compute cluster.")
    (license license:lgpl2.1+)))

(define-public r-abjutils
  (package
    (name "r-abjutils")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "abjutils" version))
       (sha256
        (base32 "0n4zps65y3zg0gfzlv97w91si52a9izkncirskbkj5x9hk0nhxcv"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-devtools" ,r-devtools)
       ("r-dplyr" ,r-dplyr)
       ("r-furrr" ,r-furrr)
       ("r-future" ,r-future)
       ("r-glue" ,r-glue)
       ("r-httr" ,r-httr)
       ("r-magrittr" ,r-magrittr)
       ("r-progress" ,r-progress)
       ("r-purrr" ,r-purrr)
       ("r-readr" ,r-readr)
       ("r-rlang" ,r-rlang)
       ("r-rstudioapi" ,r-rstudioapi)
       ("r-scales" ,r-scales)
       ("r-stringi" ,r-stringi)
       ("r-stringr" ,r-stringr)
       ("r-tibble" ,r-tibble)
       ("r-tidyr" ,r-tidyr)))
    (home-page "https://github.com/abjur/abjutils/")
    (synopsis "Collection of tools for jurimetrical analysis")
    (description
     "This package implements general purpose tools, such as functions for
sampling and basic manipulation of Brazilian lawsuits identification number.
It also implements functions for text cleaning, such as accentuation
removal.")
    (license license:expat)))

(define-public r-abnormality
  (package
    (name "r-abnormality")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "abnormality" version))
       (sha256
        (base32 "1fzfskl9akl06nliy8hkv2a0pznpj8pwcypg3gj5r2nzvr3kan9v"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-mass" ,r-mass)
       ("r-matrix" ,r-matrix)))
    (home-page "https://cran.r-project.org/web/packages/abnormality/")
    (synopsis "Measure a subject's abnormality with respect to a reference population")
    (description
     "This package contains functions to implement the methodology and
considerations laid out by Marks et al. in the article \"Measuring abnormality
in high dimensional spaces: applications in biomechanical gait analysis\".
Using high-dimensional datasets to measure a subject's overall level of
abnormality as compared to a reference population is often needed in outcomes
research.")
    (license license:expat)))

(define-public r-abodoutlier
  (package
    (name "r-abodoutlier")
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "abodOutlier" version))
       (sha256
        (base32 "1pvhgxmh23br84r0fbmv7g53z2427birdja96a67vqgz18r3fdvj"))))
    (properties `((upstream-name . "abodOutlier")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-cluster" ,r-cluster)))
    (home-page "https://cran.r-project.org/web/packages/abodOutlier/")
    (synopsis "Angle-based outlier detection")
    (description
     "This package performs angle-based outlier detection on a given data
frame.  It offers three methods to process data:
@enumerate
@item full but slow implementation using all the data that has cubic
  complexity;
@item a fully randomized method;
@item a method using k-nearest neighbours.
@end enumerate
These algorithms are well suited for high dimensional data outlier
detection.")
    (license license:expat)))

(define-public r-abps
  (package
    (name "r-abps")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ABPS" version))
       (sha256
        (base32 "0n3f66nmfi5v94il1mxy026mi84w01ph2aljk60vn3mrz8kwf2ll"))))
    (properties `((upstream-name . "ABPS")))
    (build-system r-build-system)
    (propagated-inputs `(("r-kernlab" ,r-kernlab)))
    (home-page "https://cran.r-project.org/web/packages/ABPS/")
    (synopsis "Abnormal blood profile score to detect blood doping")
    (description
     "This package offers an implementation of the @dfn{Abnormal blood profile score} (ABPS).
The ABPS is a part of the Athlete biological passport program of the World
anti-doping agency, which combines several blood parameters into a single
score in order to detect blood doping.  The package also contains functions to
calculate other scores used in anti-doping programs, such as the ratio of
hemoglobin to reticulocytes (OFF-score), as well as example data.")
    (license license:gpl2+)))

(define-public r-parmigene
  (package
    (name "r-parmigene")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "parmigene" version))
       (sha256
        (base32
         "1fsm6pkr17jcbzkj1hbn91jf890fviqk1lq6ls8pihsdgah1zb4d"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/parmigene/")
    (synopsis "Mutual information estimation for gene network reconstruction")
    (description
     "This package provides a parallel estimation of the mutual information
based on entropy estimates from k-nearest neighbors distances and algorithms
for the reconstruction of gene regulatory networks.")
    (license license:agpl3+)))

(define-public r-pscl
  (package
    (name "r-pscl")
    (version "1.5.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "pscl" version))
       (sha256
        (base32 "0vzf5wazs92bhqhqd66v3vwmbfmnh67gb2466g1xxawim649nk05"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-mass" ,r-mass)))
    (home-page "https://github.com/atahk/pscl/")
    (synopsis "Political science computational laboratory")
    (description
     "The @code{pscl} is an R package providing classes and methods for:
@enumerate
@item Bayesian analysis of roll call data (item-response models);
@item elementary Bayesian statistics;
@item maximum likelihood estimation of zero-inflated and hurdle models for count
data;
@item utility functions.
@end enumerate")
    (license license:gpl2)))

(define-public r-accelmissing
  (package
    (name "r-accelmissing")
    (version "1.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "accelmissing" version))
       (sha256
        (base32 "1nql9inx6azdzi3z4sfm2vdml2mms6krl8wzlf1dn1c97ahn57fy"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-mice" ,r-mice)
       ("r-pscl" ,r-pscl)))
    (home-page "https://cran.r-project.org/web/packages/accelmissing/")
    (synopsis "Missing value imputation for accelerometer data")
    (description
     "This package provides a statistical method to impute the missing values in
accelerometer data.  The methodology includes both parametric and
semi-parametric multiple imputations under the zero-inflated Poisson lognormal
model.  It also provides multiple functions to preprocess the accelerometer data
previous to the missing data imputation.  These include detecting the wearing
and the non-wearing time, selecting valid days and subjects, and creating plots.")
    (license license:gpl2+)))

(define-public r-mhsmm
  (package
    (name "r-mhsmm")
    (version "0.4.16")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "mhsmm" version))
       (sha256
        (base32 "009dj0zkj1zry7jr9hf4cknb686z50a2l967if64xm0dvjmp7dgs"))))
    (build-system r-build-system)
    (propagated-inputs `(("r-mvtnorm" ,r-mvtnorm)))
    (home-page "https://github.com/jaredo/mhsmm/")
    (synopsis "Inference for hidden Markov and semi-Markov models")
    (description
     "The @code{r-mhsmm} package implements estimation and prediction methods for
hidden Markov and semi-Markov models for multiple observation sequences.  Such
techniques are of interest when observed data is thought to be dependent on some
unobserved (or hidden) state.  Also, this package is suitable for equidistant
time series data, with multivariate and/or missing data.  Allows user defined
emission distributions.")
    (license license:gpl2+)))

(define-public r-nleqslv
  (package
    (name "r-nleqslv")
    (version "3.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "nleqslv" version))
       (sha256
        (base32 "1v9znvncyigw9r25wx2ma0b7ib179b488dl0qsrhp5zrcz7mcjgm"))))
    (build-system r-build-system)
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/nleqslv/")
    (synopsis "Solve systems of nonlinear equations")
    (description
     "The @code{r-nleqslv} package solves a system of nonlinear equations using a
Broyden or a Newton method with a choice of global strategies such as line
search and trust region.  There are options for using a numerical or user
supplied Jacobian, for specifying a banded numerical Jacobian and for allowing a
singular or ill-conditioned Jacobian.")
    (license license:gpl2+)))

(define-public r-physicalactivity
  (package
    (name "r-physicalactivity")
    (version "0.2-2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "PhysicalActivity" version))
       (sha256
        (base32 "14z6plgwyr46vs9m997rvlz8sdglfs9g087an8668zqkzzs2w4ln"))))
    (properties
     `((upstream-name . "PhysicalActivity")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/PhysicalActivity/")
    (synopsis "Procesing accelerometer data for physical activity measurement")
    (description
     "This @code{r-physicalactivity} package provides a function @code{wearingMarking}
for classification of monitor wear and nonwear time intervals in accelerometer
data collected to assess physical activity.  The package also contains functions
for making plots of accelerometer data and obtaining the summary of various
information including daily monitor wear time and the mean monitor wear time
during valid days.  The revised package version 0.2-1 improved the functions
regarding speed, robustness and add better support for time zones and daylight
saving.  In addition, several functions were added:
@enumerate
@item the @code{markDelivery} can classify days for ActiGraph delivery by mail;
@item the @code{markPAI} can categorize physical activity intensity level based
on user-defined cut-points of accelerometer counts.
@end enumerate
  It also supports importing ActiGraph (AGD) files with @code{readActigraph} and
@code{queryActigraph} functions.")
    (license license:gpl3+)))

(define-public r-acc
  (package
    (name "r-acc")
    (version "1.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "acc" version))
       (sha256
        (base32 "1ii2vm47djxbixa75h690q1s2f9m9x6i8nkygik93j6dayr6kr1m"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-circlize" ,r-circlize)
       ("r-dbi" ,r-dbi)
       ("r-ggplot2" ,r-ggplot2)
       ("r-iterators" ,r-iterators)
       ("r-mhsmm" ,r-mhsmm)
       ("r-nleqslv" ,r-nleqslv)
       ("r-physicalactivity" ,r-physicalactivity)
       ("r-plyr" ,r-plyr)
       ("r-r-utils" ,r-r-utils)
       ("r-rcpp" ,r-rcpp)
       ("r-rcpparmadillo" ,r-rcpparmadillo)
       ("r-rsqlite" ,r-rsqlite)
       ("r-zoo" ,r-zoo)))
    (home-page "https://cran.r-project.org/web/packages/acc/")
    (synopsis "Exploring accelerometer data")
    (description
     "This package processes accelerometer data from uni-axial and tri-axial devices
and generates data summaries.  Also, includes functions to plot, analyze, and
simulate accelerometer data.")
    (license license:gpl2+)))

(define-public r-rbenchmark
  (package
    (name "r-rbenchmark")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rbenchmark" version))
       (sha256
        (base32 "010fn3qwnk2k411cbqyvra1d12c3bhhl3spzm8kxffmirj4p2al9"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/rbenchmark/")
    (synopsis "Benchmarking routine for R")
    (description
     "This @code{r-rbenchmark} package is inspired by the Perl module Benchmark,
and is intended to facilitate benchmarking of arbitrary R code.  The library
consists of just one function, benchmark, which is a simple wrapper around
system.time.  Given a specification of the benchmarking process (counts of
replications, evaluation environment) and an arbitrary number of expressions,
benchmark evaluates each of the expressions in the specified environment,
replicating the evaluation as many times as specified, and returning the results
conveniently wrapped into a data frame.")
    (license license:gpl2+)))

(define-public r-mitools
  (package
    (name "r-mitools")
    (version "2.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "mitools" version))
       (sha256
        (base32
         "0c2x2n1p53lcw0vx4vmy5j7m2f95i7g2iwbryl89imr99rvz617j"))))
    (build-system r-build-system)
    (propagated-inputs `(("r-dbi" ,r-dbi)))
    (home-page "https://cran.r-project.org/web/packages/mitools/")
    (synopsis "Tools for multiple imputation of missing data")
    (description
     "This package provides tools to perform analyses and combine results from
multiple-imputation datasets.")
    (license license:gpl2)))

(define-public r-magick
  (package
    (name "r-magick")
    (version "2.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "magick" version))
       (sha256
        (base32
         "182b4wahkq9q0scn99mql4vm9fp92nja0r5yizc4x9rjl492ahd8"))))
    (build-system r-build-system)
    (inputs
     `(("imagemagick" ,imagemagick)
       ("zlib" ,zlib)))
    (propagated-inputs
     `(("r-curl" ,r-curl)
       ("r-magrittr" ,r-magrittr)
       ("r-rcpp" ,r-rcpp)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://github.com/ropensci/magick")
    (synopsis "Advanced graphics and image-processing in R")
    (description
     "This package provides bindings to ImageMagick, a comprehensive image
processing library.  It supports many common formats (PNG, JPEG, TIFF, PDF,
etc.)  and manipulations (rotate, scale, crop, trim, flip, blur, etc).  All
operations are vectorized via the Magick++ STL meaning they operate either on
a single frame or a series of frames for working with layers, collages, or
animation.  In RStudio, images are automatically previewed when printed to the
console, resulting in an interactive editing environment.")
    (license license:expat)))

(define-public r-survey
  (package
    (name "r-survey")
    (version "3.37")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "survey" version))
       (sha256
        (base32
         "1f31dvh48gzzan13pdrwh84ls35x9116095i7mdrcbrhz809r8dy"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-lattice" ,r-lattice)
       ("r-matrix" ,r-matrix)
       ("r-minqa" ,r-minqa)
       ("r-mitools" ,r-mitools)
       ("r-numderiv" ,r-numderiv)
       ("r-survival" ,r-survival)))
    (home-page "http://r-survey.r-forge.r-project.org/survey/")
    (synopsis "Analysis of complex survey samples")
    (description
     "This package provides tools for the analysis of complex survey samples.
The provided features include: summary statistics, two-sample tests, rank
tests, generalised linear models, cumulative link models, Cox models,
loglinear models, and general maximum pseudolikelihood estimation for
multistage stratified, cluster-sampled, unequally weighted survey samples;
variances by Taylor series linearisation or replicate weights;
post-stratification, calibration, and raking; two-phase subsampling designs;
graphics; PPS sampling without replacement; principal components, and factor
analysis.")
    ;; Either version of the GPL.
    (license (list license:gpl2 license:gpl3))))

(define-public r-gee
  (package
    (name "r-gee")
    (version "4.13-20")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "gee" version))
       (sha256
        (base32
         "167pzgnmj4cjc41ykps1mfwi6s7y32zxyycn5z17vn4v0pp4q0ak"))))
    (properties `((upstream-name . "gee")))
    (build-system r-build-system)
    (native-inputs
     `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/gee/")
    (synopsis "Generalized estimation equation solver")
    (description
     "This package provides a solver for generalized estimation equations.")
    (license license:gpl2)))

(define-public r-tab
  (package
    (name "r-tab")
    (version "4.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "tab" version))
       (sha256
        (base32
         "0ds8n6gncidb66in7hlqkcmil5yfsf7ihqvmls789hrm2iz9xlfm"))))
    (properties `((upstream-name . "tab")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-dplyr" ,r-dplyr)
       ("r-gee" ,r-gee)
       ("r-knitr" ,r-knitr)
       ("r-mass" ,r-mass)
       ("r-survey" ,r-survey)
       ("r-survival" ,r-survival)
       ("r-xtable" ,r-xtable)))
    (home-page "https://cran.r-project.org/web/packages/tab/")
    (synopsis "Create summary tables for statistical reports")
    (description
     "This package contains functions for creating various types of summary
tables, e.g. comparing characteristics across levels of a categorical variable
and summarizing fitted generalized linear models, generalized estimating
equations, and Cox proportional hazards models.  Functions are available to
handle data from simple random samples as well as complex surveys.")
    (license license:gpl3+)))

(define-public r-dvmisc
  (package
    (name "r-dvmisc")
    (version "1.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "dvmisc" version))
       (sha256
        (base32 "01v6sixx0f3nrn6ymfifb3pvd2msfrwm21kmdv38laxq29vc4rsi"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-cubature" ,r-cubature)
       ("r-data-table" ,r-data-table)
       ("r-dplyr" ,r-dplyr)
       ("r-ggplot2" ,r-ggplot2)
       ("r-mass" ,r-mass)
       ("r-mvtnorm" ,r-mvtnorm)
       ("r-pracma" ,r-pracma)
       ("r-rbenchmark" ,r-rbenchmark)
       ("r-rcpp" ,r-rcpp)
       ("r-survey" ,r-survey)
       ("r-tab" ,r-tab)))
    (home-page "https://cran.r-project.org/web/packages/dvmisc/")
    (synopsis "Faster computation of common statistics and miscellaneous functions")
    (description
     "This package implements faster versions of base R functions (e.g. mean, standard
deviation, covariance, weighted mean), mostly written in C++, along with
miscellaneous functions for various purposes (e.g. create the histogram with
fitted probability density function or probability mass function curve, create
the body mass index groups, assess the linearity assumption in logistic
regression).")
    (license license:gpl2)))

(define-public r-accelerometry
  (package
    (name "r-accelerometry")
    (version "3.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "accelerometry" version))
       (sha256
        (base32 "13xzrwhr4i1nj9c8vrmfdg2rmrc8n446iihcyxmy99sm99hpzyip"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-dvmisc" ,r-dvmisc)
       ("r-rcpp" ,r-rcpp)))
    (home-page "https://cran.r-project.org/web/packages/accelerometry/")
    (synopsis "Functions for processing accelerometer data")
    (description
     "This package provides a collection of functions that perform operations on
time-series accelerometer data, such as identify the non-wear time, flag minutes
that are part of an activity bout, and find the maximum 10-minute average count
value.  The functions are generally very flexible, allowing for a variety of
algorithms to be implemented.")
    (license license:gpl3)))

(define-public r-absim
  (package
    (name "r-absim")
    (version "0.2.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "AbSim" version))
       (sha256
        (base32 "16ddjk8b6xw80ch4jis1y751i9561wdxh0gifbf15qiz3vjckq8m"))))
    (properties `((upstream-name . "AbSim")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ape" ,r-ape)
       ("r-powerlaw" ,r-powerlaw)))
    (home-page "https://cran.r-project.org/web/packages/AbSim/")
    (synopsis "Time resolved simulations of antibody repertoires")
    (description
     "This package provides simulation methods for the evolution of antibody repertoires.
  The heavy and light chain variable region of both human and C57BL/6 mice can
be simulated in a time-dependent fashion.  Both single lineages using one set of
V-, D-, and J-genes or full repertoires can be simulated.  The algorithm begins
with an initial V-D-J recombination event, starting the first phylogenetic tree.
  Upon completion, the main loop of the algorithm begins, with each iteration
representing one simulated time step.  Various mutation events are possible at
each time step, contributing to a diverse final repertoire.")
    (license license:gpl2)))

(define-public r-quic
  (package
    (name "r-quic")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "QUIC" version))
       (sha256
        (base32 "021bp9xbaih60qmss015ycblbv6d1dvb1z89y93zpqqnc2qhpv3c"))))
    (properties `((upstream-name . "QUIC")))
    (build-system r-build-system)
    (home-page "https://www.cs.utexas.edu/users/sustik/QUIC/")
    (synopsis "Regularized sparse inverse covariance matrix estimation")
    (description
     "This package implements the regularized Gaussian maximum likelihood
estimation of the inverse of a covariance matrix.  It uses Newton's method and
coordinate descent to solve the regularized inverse covariance matrix
estimation problem.")
    ;; The project home page states that the release is under GPLv3 or later.
    ;; The CRAN page only says GPL-3.
    (license license:gpl3+)))

(define-public r-abundant
  (package
    (name "r-abundant")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "abundant" version))
       (sha256
        (base32 "1m76qdmqvwpgm0sihazi2dna7cgsz9rljal18vgffb5wamwmg9k7"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-quic" ,r-quic)))
    (home-page "https://cran.r-project.org/web/packages/abundant/")
    (synopsis "Abundant regression and high-dimensional principal fitted components")
    (description
     "This package provides tools to fit and predict with the high-dimensional
principal fitted components model.  This model is described by Cook, Forzani,
and Rothman (2012) @url{doi:10.1214/11-AOS962}.")
    ;; The DESCRIPTION file states GPL-2, but since it directly depends on a
    ;; GPLv3+ package (QUIC) this likely means GPLv2+.
    (license license:gpl2+)))

(define-public r-ac3net
  (package
    (name "r-ac3net")
    (version "1.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Ac3net" version))
       (sha256
        (base32 "1ns4n0xxz6p34c11bj0k7nzgmyqr9mis2b0g5nfz37dbikndyqyz"))))
    (properties `((upstream-name . "Ac3net")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-data-table" ,r-data-table)))
    (home-page "https://cran.r-project.org/web/packages/Ac3net/")
    (synopsis "Inferring directional conservative causal core gene networks")
    (description "This package infers directional Conservative causal core
(gene) networks (C3NET).  This is a version of the algorithm C3NET with
directional network.")
    (license license:gpl3+)))

(define-public r-aca
  (package
    (name "r-aca")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ACA" version))
       (sha256
        (base32 "1i3hm27nvnkvc39xlh0d1blq8q0q02czmvgi3cazmjx3jvxay0vq"))))
    (properties `((upstream-name . "ACA")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/ACA/")
    (synopsis "Abrupt change-point or aberration detection in point series")
    (description
     "This package offers an interactive function for the detection of breakpoints in
series.")
    ;; Any version of the GPL
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-acceptancesampling
  (package
    (name "r-acceptancesampling")
    (version "1.0-6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "AcceptanceSampling" version))
       (sha256
        (base32 "1z3rmln63ki2kik9kinbwr9qhr32ggbmh4mm3xqy6di119n47ca9"))))
    (properties
     `((upstream-name . "AcceptanceSampling")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/AcceptanceSampling/")
    (synopsis "Creation and evaluation of acceptance sampling plans")
    (description
     "This @code{r-acceptancesampling} provides functionality for creating and evaluating
acceptance sampling plans.  Acceptance sampling is a methodology commonly used
in quality control and improvement.  International standards of acceptance
sampling provide sampling plans for specific circumstances.  The aim of this
package is to provide an easy-to-use interface to visualize single, double or
multiple sampling plans.  In addition, methods have been provided to enable the
user to assess sampling plans against pre-specified levels of performance, as
measured by the probability of acceptance for a given level of quality in the
lot.")
    (license license:gpl3+)))

(define-public r-acclma
  (package
    (name "r-acclma")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ACCLMA" version))
       (sha256
        (base32 "1na27sp18fq12gp6vxgqw1ffsz2yi1d8xvrxbrzx5g1kqxrayy0v"))))
    (properties `((upstream-name . "ACCLMA")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/ACCLMA/")
    (synopsis "ACC & LMA graph plotting")
    (description
     "This package contains a function that imports data from a @acronym{CSV,
Comma-Separated Values} file, or uses manually entered data from the format (x,
y, weight) and plots the appropriate @acronym{ACC, Absolute Concentration
Curve} vs @acronym{LOI, Line of Independence} graph and
@acronym{LMA, @acronym{LOI} Minus @acronym{ACC}} graph.  The main
function is @code{plotLMA} (source file, header) that takes a data set and plots the
appropriate @acronym{LMA} and @acronym{ACC} graphs.  If no source file (a
string) was passed, a manual data entry window is opened.  The header parameter
indicates by TRUE/FALSE (false by default) if the source @acronym{CSV} file has
a header row or not.  The dataset should contain only one independent variable
(x) and one dependent variable (y) and can contain a weight for each
observation.")
    (license license:gpl2)))

(define-public r-aspi
  (package
    (name "r-aspi")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "aspi" version))
       (sha256
        (base32 "0rhvxw243vvdv3hxa6pi343gcjc2cbxq1jzqirl9k1l4i3897l87"))))
    (build-system r-build-system)
    (home-page
     "https://cran.r-project.org/web/packages/aspi/")
    (synopsis
     "Analysis of symmetry of parasitic infections")
    (description
     "This package provides tools for the analysis and visualization of bilateral
      asymmetry in parasitic infections.")
    (license license:gpl3+)))

(define-public r-sandwich
  (package
    (name "r-sandwich")
    (version "2.5-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "sandwich" version))
       (sha256
        (base32
         "1mk685b9wq7k566pbml52rj96i5h6b3vf215k9picgmq296nzvyv"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-zoo" ,r-zoo)))
    (home-page "https://cran.r-project.org/web/packages/sandwich/")
    (synopsis "Robust Covariance Matrix Estimators")
    (description
     "This package provides model-robust standard error estimators for
cross-sectional, time series, clustered, panel, and longitudinal data.")
    ;; Either version of the license.
    (license (list license:gpl2 license:gpl3))))

(define-public r-th-data
  (package
    (name "r-th-data")
    (version "1.0-10")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "TH.data" version))
       (sha256
        (base32
         "0mgz7aj2d9abbmdr65zgmg1ddp3fdbs3mfj83r5xadh5ldkir2k1"))))
    (properties `((upstream-name . "TH.data")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-mass" ,r-mass)
       ("r-survival" ,r-survival)))
    (home-page "https://cran.r-project.org/web/packages/TH.data/")
    (synopsis "Shared data sets")
    (description
     "This package contains supporting data sets that are used in other
packages maintained by Torsten Hothorn.")
    (license license:gpl3)))

(define-public r-multcomp
  (package
    (name "r-multcomp")
    (version "1.4-12")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "multcomp" version))
       (sha256
        (base32
         "14c2f10rz546w7ly5f4r6wnd07yj5gic38an17gxny1vf2nsff0b"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-codetools" ,r-codetools)
       ("r-mvtnorm" ,r-mvtnorm)
       ("r-sandwich" ,r-sandwich)
       ("r-survival" ,r-survival)
       ("r-th-data" ,r-th-data)))
    (home-page "https://cran.r-project.org/web/packages/multcomp/")
    (synopsis "Simultaneous inference in general parametric models")
    (description
     "Simultaneous tests and confidence intervals for general linear
hypotheses in parametric models, including linear, generalized linear, linear
mixed effects, and survival models.  The package includes demos reproducing
analyzes presented in the book \"Multiple Comparisons Using R\" (Bretz,
Hothorn, Westfall, 2010, CRC Press).")
    (license license:gpl2)))

(define-public r-emmeans
  (package
    (name "r-emmeans")
    (version "1.4.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "emmeans" version))
       (sha256
        (base32
         "10fmvmd6q4zjr6b18hhc85mwrzv778qzj6lwl9kbs2fsfvsgw7mm"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-estimability" ,r-estimability)
       ("r-mvtnorm" ,r-mvtnorm)
       ("r-numderiv" ,r-numderiv)
       ("r-plyr" ,r-plyr)
       ("r-xtable" ,r-xtable)))
    (home-page "https://github.com/rvlenth/emmeans")
    (synopsis "Estimated marginal means, aka least-squares means")
    (description
     "This package provides tools to obtain @dfn{estimated marginal
means} (EMMs) for many linear, generalized linear, and mixed models.  It can
be used to compute contrasts or linear functions of EMMs, trends, and
comparisons of slopes.")
    ;; Either version of the license.
    (license (list license:gpl2 license:gpl3))))

(define-public r-pwr
  (package
    (name "r-pwr")
    (version "1.2-2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "pwr" version))
       (sha256
        (base32
         "0r5g781lr677vp3zyhgmi7r68c87l8gd05l1s3ffnxgn5wf043sm"))))
    (build-system r-build-system)
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/heliosdrm/pwr")
    (synopsis "Basic functions for power analysis")
    (description
     "This package provides power analysis functions along the lines of
Cohen (1988).")
    (license license:gpl3+)))

(define-public r-libcoin
  (package
    (name "r-libcoin")
    (version "1.0-5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "libcoin" version))
       (sha256
        (base32
         "1cm9x1dlg9f7fh7n5nw3x4a7rl88c7ylrlc8x3rx4mq5w1j42x0a"))))
    (build-system r-build-system)
    (propagated-inputs `(("r-mvtnorm" ,r-mvtnorm)))
    (home-page "https://cran.r-project.org/web/packages/libcoin")
    (synopsis "Linear test statistics for permutation inference")
    (description
     "This package provides basic infrastructure for linear test statistics
and permutation inference in the framework of Strasser and Weber (1999).")
    (license license:gpl2)))

(define-public r-coin
  (package
    (name "r-coin")
    (version "1.3-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "coin" version))
       (sha256
        (base32
         "0qi03fyqw42a2vnqcia5l2m1mzyarj2q1iblknx9n19bdsd53qjx"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-libcoin" ,r-libcoin)
       ("r-matrixstats" ,r-matrixstats)
       ("r-modeltools" ,r-modeltools)
       ("r-multcomp" ,r-multcomp)
       ("r-mvtnorm" ,r-mvtnorm)
       ("r-survival" ,r-survival)))
    (home-page "http://coin.r-forge.r-project.org")
    (synopsis "Conditional inference procedures in a permutation test framework")
    (description
     "This package provides conditional inference procedures for the general
independence problem including two-sample, K-sample (non-parametric ANOVA),
correlation, censored, ordered and multivariate problems.")
    (license license:gpl2)))

(define-public r-bayesplot
  (package
    (name "r-bayesplot")
    (version "1.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "bayesplot" version))
       (sha256
        (base32
         "0sq0ajnm96hmlqf1cv5n2gshh3qdij4n1zbm7qrniz2q6b5aj342"))))
    (build-system r-build-system)
    (inputs
     `(("pandoc" ,ghc-pandoc)
       ("pandoc-citeproc" ,ghc-pandoc-citeproc)))
    (propagated-inputs
     `(("r-dplyr" ,r-dplyr)
       ("r-ggplot2" ,r-ggplot2)
       ("r-ggridges" ,r-ggridges)
       ("r-glue" ,r-glue)
       ("r-reshape2" ,r-reshape2)
       ("r-rlang" ,r-rlang)
       ("r-tibble" ,r-tibble)
       ("r-tidyselect" ,r-tidyselect)))
    (home-page "https://mc-stan.org/bayesplot")
    (synopsis "Plotting for Bayesian models")
    (description
     "This package provides plotting functions for posterior analysis, model
checking, and MCMC diagnostics.  The package is designed not only to provide
convenient functionality for users, but also a common set of functions that
can be easily used by developers working on a variety of R packages for
Bayesian modeling.")
    (license license:gpl3+)))

(define-public r-tmb
  (package
    (name "r-tmb")
    (version "1.7.16")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "TMB" version))
       (sha256
        (base32
         "0lly12hdi99iklwr0vg9xkyhi038w4gncbf895qcwbndmqp0lx44"))))
    (properties `((upstream-name . "TMB")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-matrix" ,r-matrix)
       ("r-rcppeigen" ,r-rcppeigen)))
    (home-page "http://tmb-project.org")
    (synopsis "Template model builder: a general random effect tool")
    (description
     "With this tool, a user should be able to quickly implement complex
random effect models through simple C++ templates.  The package combines
@code{CppAD} (C++ automatic differentiation), @code{Eigen} (templated
matrix-vector library) and @code{CHOLMOD} (sparse matrix routines available
from R) to obtain an efficient implementation of the applied Laplace
approximation with exact derivatives.  Key features are: Automatic sparseness
detection, parallelism through BLAS and parallel user templates.")
    (license license:gpl2)))

(define-public r-sjstats
  (package
    (name "r-sjstats")
    (version "0.17.9")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "sjstats" version))
       (sha256
        (base32 "0938ac6ixvkif07azd2msa8g3qnn9rabj6jg17almbysl83kg4nm"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bayestestr" ,r-bayestestr)
       ("r-broom" ,r-broom)
       ("r-dplyr" ,r-dplyr)
       ("r-effectsize" ,r-effectsize)
       ("r-emmeans" ,r-emmeans)
       ("r-insight" ,r-insight)
       ("r-lme4" ,r-lme4)
       ("r-magrittr" ,r-magrittr)
       ("r-mass" ,r-mass)
       ("r-modelr" ,r-modelr)
       ("r-parameters" ,r-parameters)
       ("r-performance" ,r-performance)
       ("r-purrr" ,r-purrr)
       ("r-rlang" ,r-rlang)
       ("r-sjlabelled" ,r-sjlabelled)
       ("r-sjmisc" ,r-sjmisc)
       ("r-tidyr" ,r-tidyr)))
    (home-page "https://github.com/strengejacke/sjstats")
    (synopsis "Functions for common statistical computations")
    (description
     "This package provides a collection of convenient functions for common
statistical computations, which are not directly provided by R's @code{base}
or @code{stats} packages.  This package aims at providing, first, shortcuts
for statistical measures, which otherwise could only be calculated with
additional effort.  Second, these shortcut functions are generic, and can be
applied not only to vectors, but also to other objects as well.  The focus of
most functions lies on summary statistics or fit measures for regression
models, including generalized linear models, mixed effects models and Bayesian
models.")
    (license license:gpl3)))

(define-public r-glmmtmb
  (package
    (name "r-glmmtmb")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "glmmTMB" version))
       (sha256
        (base32
         "1md4yw4kbng953rz1n9g1v2xffk3cxx1qmp0kvbdayg1s82l1rgx"))))
    (properties `((upstream-name . "glmmTMB")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-lme4" ,r-lme4)
       ("r-matrix" ,r-matrix)
       ("r-nlme" ,r-nlme)
       ("r-rcppeigen" ,r-rcppeigen)
       ("r-tmb" ,r-tmb)))
    (native-inputs
     `(("r-knitr" ,r-knitr))) ; for vignettes
    (home-page "https://github.com/glmmTMB")
    (synopsis "Generalized linear mixed models")
    (description
     "Fit linear and generalized linear mixed models with various extensions,
including zero-inflation.  The models are fitted using maximum likelihood
estimation via the Template Model Builder.  Random effects are assumed to be
Gaussian on the scale of the linear predictor and are integrated out using the
Laplace approximation.  Gradients are calculated using automatic
differentiation.")
    (license license:agpl3+)))

(define-public r-bayestestr
  (package
    (name "r-bayestestr")
    (version "0.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "bayestestR" version))
       (sha256
        (base32
         "1l0kqw793d8fnk0745fllmqwjwnj9srvqzv4baq5s05ls1gbi2mw"))))
    (properties `((upstream-name . "bayestestR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-insight" ,r-insight)))
    (home-page "https://github.com/easystats/bayestestR")
    (synopsis "Describe Bayesian models and posterior distributions")
    (description
     "This package provides utilities to understand and describe posterior
distributions and Bayesian models.  It includes point-estimates such as
@dfn{Maximum A Posteriori} (MAP), measures of dispersion such as @dfn{Highest
Density Interval} (HDI), and indices used for null-hypothesis testing (such as
ROPE percentage and pd).")
    (license license:gpl3)))

(define-public r-performance
  (package
    (name "r-performance")
    (version "0.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "performance" version))
       (sha256
        (base32
         "18h9y66cpsb3k6xnaya87vnpv2s3chf4bzsc4ym3n5sxhh41j7la"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bayestestr" ,r-bayestestr)
       ("r-insight" ,r-insight)))
    (home-page "https://easystats.github.io/performance/")
    (synopsis "Assessment of regression models performance")
    (description
     "This package provides utilities for computing measures to assess model
quality, which are not directly provided by R's @code{base} or @code{stats}
packages.  These include e.g. measures like r-squared, intraclass correlation
coefficient, root mean squared error or functions to check models for
overdispersion, singularity or zero-inflation and more.  Functions apply to a
large variety of regression models, including generalized linear models, mixed
effects models and Bayesian models.")
    (license license:gpl3)))

(define-public r-ggeffects
  (package
    (name "r-ggeffects")
    (version "0.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ggeffects" version))
       (sha256
        (base32
         "07pvs18vlybp96zz1wjr7cwiwi1cvjwkb9ahxzq0vd6adnd6ya59"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-insight" ,r-insight)
       ("r-mass" ,r-mass)
       ("r-sjlabelled" ,r-sjlabelled)))
    (home-page "https://github.com/strengejacke/ggeffects")
    (synopsis "Create tidy data frames of marginal effects for ggplot")
    (description
     "This package provides tools to compute marginal effects from statistical
models and return the result as tidy data frames.  These data frames are ready
to use with the @code{ggplot2} package.  Marginal effects can be calculated
for many different models.  Interaction terms, splines and polynomial terms
are also supported.  The two main functions are @code{ggpredict()} and
@code{ggeffect()}.  There is a generic @code{plot()} method to plot the
results using @code{ggplot2}.")
    (license license:gpl3)))

(define-public r-effectsize
  (package
    (name "r-effectsize")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "effectsize" version))
       (sha256
        (base32
         "1liix8rf8xq3hzi7684bl0s20iflsq6g5mxr5k59wp2qwn1i6aww"))))
    (properties `((upstream-name . "effectsize")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bayestestr" ,r-bayestestr)
       ("r-insight" ,r-insight)
       ("r-parameters" ,r-parameters)))
    (home-page "https://github.com/easystats/effectsize")
    (synopsis "Indices of effect size and standardized parameters")
    (description
     "This package provides utilities to work with indices of effect size and
standardized parameters for a wide variety of models, allowing computation and
conversion of indices such as Cohen's d, r, odds, etc.")
    (license license:gpl3)))

(define-public r-sjplot
  (package
    (name "r-sjplot")
    (version "2.8.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "sjPlot" version))
       (sha256
        (base32 "0ip1rkjlhyf3axlc8qqss1qq1f0xrda890c1jmcbhm98wwjw264f"))))
    (properties `((upstream-name . "sjPlot")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bayestestr" ,r-bayestestr)
       ("r-dplyr" ,r-dplyr)
       ("r-effectsize" ,r-effectsize)
       ("r-ggeffects" ,r-ggeffects)
       ("r-ggplot2" ,r-ggplot2)
       ("r-insight" ,r-insight)
       ("r-knitr" ,r-knitr)
       ("r-mass" ,r-mass)
       ("r-parameters" ,r-parameters)
       ("r-performance" ,r-performance)
       ("r-purrr" ,r-purrr)
       ("r-rlang" ,r-rlang)
       ("r-scales" ,r-scales)
       ("r-sjlabelled" ,r-sjlabelled)
       ("r-sjmisc" ,r-sjmisc)
       ("r-sjstats" ,r-sjstats)
       ("r-tidyr" ,r-tidyr)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://strengejacke.github.io/sjPlot/")
    (synopsis "Data visualization for statistics in social science")
    (description
     "This package represents a collection of plotting and table output
functions for data visualization.  Results of various statistical
analyses (that are commonly used in social sciences) can be visualized using
this package, including simple and cross tabulated frequencies, histograms,
box plots, (generalized) linear models, mixed effects models, principal
component analysis and correlation matrices, cluster analyses, scatter plots,
stacked scales, effects plots of regression models (including interaction
terms) and much more.  This package supports labelled data.")
    (license license:gpl3)))

(define-public r-ini
  (package
    (name "r-ini")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ini" version))
       (sha256
        (base32
         "04yqij344dwm0xqgara8xia42mlmij3i8711qbb5534w05a1l6bv"))))
    (build-system r-build-system)
    (home-page "https://github.com/dvdscripter/ini")
    (synopsis "Read and write configuration files")
    (description
     "This package provides tools to parse simple @code{.ini} configuration
files to an structured list.  Users can manipulate this resulting list with
@code{lapply()} functions.  This same structured list can be used to write
back to file after modifications.")
    (license license:gpl3)))

(define-public r-gh
  (package
    (name "r-gh")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "gh" version))
       (sha256
        (base32
         "1bc9bn1078s664hc806dh0y1ncxif77q479rfmxfir9z7hwaz7yy"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-cli" ,r-cli)
       ("r-httr" ,r-httr)
       ("r-ini" ,r-ini)
       ("r-jsonlite" ,r-jsonlite)))
    (home-page "https://github.com/r-lib/gh#readme")
    (synopsis "Access the GitHub API via R")
    (description
     "This package provides a minimal R client to access the GitHub API.")
    (license license:expat)))

(define-public r-fs
  (package
    (name "r-fs")
    (version "1.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "fs" version))
       (sha256
        (base32
         "1w30bflx4d7a6f3dn96bf7s7v6aqpvz2yzzxal6qz9jyhb16bxaz"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rcpp" ,r-rcpp)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://fs.r-lib.org")
    (synopsis "Cross-platform file system operations based on libuv")
    (description
     "This package provides a cross-platform interface to file system
operations, built on top of the libuv C library.")
    (license license:gpl3)))

(define-public r-clisymbols
  (package
    (name "r-clisymbols")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "clisymbols" version))
       (sha256
        (base32
         "1q7gi2zmykhzas9v8fdnbpdq7pzdcpbhim1yxvd2062l777g4j86"))))
    (build-system r-build-system)
    (home-page "https://github.com/gaborcsardi/clisymbols")
    (synopsis "Unicode symbols at the R prompt")
    (description
     "This package provides a small subset of Unicode symbols, that are useful
when building command line applications.  They fall back to alternatives on
terminals that do not support Unicode.")
    (license license:expat)))

(define-public r-usethis
  (package
    (name "r-usethis")
    (version "1.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "usethis" version))
       (sha256
        (base32
         "07an5wbikilg7cb3q6x5aykw8dfqnjrc3wpfb7gjmy0d9fh20fcy"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-clipr" ,r-clipr)
       ("r-clisymbols" ,r-clisymbols)
       ("r-crayon" ,r-crayon)
       ("r-curl" ,r-curl)
       ("r-desc" ,r-desc)
       ("r-fs" ,r-fs)
       ("r-gh" ,r-gh)
       ("r-git2r" ,r-git2r)
       ("r-glue" ,r-glue)
       ("r-purrr" ,r-purrr)
       ("r-rlang" ,r-rlang)
       ("r-rprojroot" ,r-rprojroot)
       ("r-rstudioapi" ,r-rstudioapi)
       ("r-whisker" ,r-whisker)
       ("r-withr" ,r-withr)
       ("r-yaml" ,r-yaml)))
    (home-page "https://github.com/r-lib/usethis")
    (synopsis "Automate R package and project setup")
    (description
     "This package helps you to automate R package and project setup tasks
that are otherwise performed manually.  This includes setting up unit testing,
test coverage, continuous integration, Git, GitHub integration, licenses,
Rcpp, RStudio projects, and more.")
    (license license:gpl3)))

(define-public r-sessioninfo
  (package
    (name "r-sessioninfo")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "sessioninfo" version))
       (sha256
        (base32
         "0j5f3l58fynxx3v0w62vqpii7miabszgljpja36xx9s8hikh8sqn"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-cli" ,r-cli)
       ("r-withr" ,r-withr)))
    (home-page "https://github.com/r-lib/sessioninfo#readme")
    (synopsis "R session information")
    (description
     "This package provides tools to query and print information about the
current R session.  It is similar to @code{utils::sessionInfo()}, but includes
more information about packages, and where they were installed from.")
    (license license:gpl2)))

(define-public r-remotes
  (package
    (name "r-remotes")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "remotes" version))
       (sha256
        (base32
         "12qinl7jv54f2524ri13gcrv6d523rg1harpi500j676zi30fnaf"))))
    (build-system r-build-system)
    (home-page "https://github.com/r-lib/remotes#readme")
    (synopsis "R package installation from remote repositories")
    (description
     "Download and install R packages stored in GitHub, BitBucket, or plain
subversion or git repositories.  This package is a lightweight replacement of
the @code{install_*} functions in the @code{devtools} package.  Indeed most of
the code was copied over from @code{devtools}.")
    (license license:gpl2+)))

(define-public r-xopen
  (package
    (name "r-xopen")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "xopen" version))
       (sha256
        (base32
         "1vrvgdika1d63dwygynbv2wmd87ll8dji5dy89hj576n8hw601z2"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-processx" ,r-processx)))
    (home-page "https://github.com/r-lib/xopen#readme")
    (synopsis "Open system files, URLs, anything")
    (description
     "This package provides a cross-platform solution to open files,
directories or URLs with their associated programs.")
    (license license:expat)))

(define-public r-rcmdcheck
  (package
    (name "r-rcmdcheck")
    (version "1.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rcmdcheck" version))
       (sha256
        (base32
         "1d4kzgfqy72r6b7bn1j4znyksrycgypx1jjvpv9lrmvn37mpkdhs"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-callr" ,r-callr)
       ("r-cli" ,r-cli)
       ("r-crayon" ,r-crayon)
       ("r-desc" ,r-desc)
       ("r-digest" ,r-digest)
       ("r-pkgbuild" ,r-pkgbuild)
       ("r-prettyunits" ,r-prettyunits)
       ("r-r6" ,r-r6)
       ("r-rprojroot" ,r-rprojroot)
       ("r-sessioninfo" ,r-sessioninfo)
       ("r-withr" ,r-withr)
       ("r-xopen" ,r-xopen)))
    (home-page "https://github.com/r-Lib/rcmdcheck#readme")
    (synopsis "Run R CMD check from R and capture results")
    (description
     "Run @code{R CMD check} from R programmatically, and capture the results
of the individual checks.")
    (license license:expat)))

(define-public r-rapportools
  (package
    (name "r-rapportools")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rapportools" version))
       (sha256
        (base32
         "1sgv4sc737i12arh5dc3263kjsz3dzg06qihfmrqyax94mv2d01b"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-pander" ,r-pander)
       ("r-plyr" ,r-plyr)
       ("r-reshape" ,r-reshape)))
    (home-page "https://cran.r-project.org/web/packages/rapportools/")
    (synopsis "Miscellaneous helper functions with sane defaults for reporting")
    (description
     "This package provides helper functions that act as wrappers to more
advanced statistical methods with the advantage of having sane defaults for
quick reporting.")
    (license license:agpl3+)))

(define-public r-pander
  (package
    (name "r-pander")
    (version "0.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "pander" version))
       (sha256
        (base32
         "1bd9sdghlsppmff18k5fg3i0visq9f4wc82rlhwq5m82bmgdgnyi"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-digest" ,r-digest)
       ("r-rcpp" ,r-rcpp)))
    (home-page "https://rapporter.github.io/pander")
    (synopsis "Render R objects into Pandoc's markdown")
    (description
     "The main aim of the pander R package is to provide a minimal and easy
tool for rendering R objects into Pandoc's markdown.  The package is also
capable of exporting/converting complex Pandoc documents (reports) in various
ways.")
    ;; This package is licensed under either the AGPLv3+ or the very rarely
    ;; used OSL 3.0.
    (license license:agpl3+)))

(define-public r-summarytools
  (package
    (name "r-summarytools")
    (version "0.9.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "summarytools" version))
       (sha256
        (base32
         "03pcb2ild1rb9f15yq8b68p9bg10z5wk2x7ahgyzkwdh5f81vbq2"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-base64enc" ,r-base64enc)
       ("r-checkmate" ,r-checkmate)
       ("r-dplyr" ,r-dplyr)
       ("r-htmltools" ,r-htmltools)
       ("r-lubridate" ,r-lubridate)
       ("r-magick" ,r-magick)
       ("r-matrixstats" ,r-matrixstats)
       ("r-pander" ,r-pander)
       ("r-pryr" ,r-pryr)
       ("r-rapportools" ,r-rapportools)
       ("r-tibble" ,r-tibble)
       ("r-tidyr" ,r-tidyr)))
    (home-page "https://github.com/dcomtois/summarytools")
    (synopsis "Tools to quickly and neatly summarize data")
    (description
     "This package provides tools for data frame summaries, cross-tabulations,
weight-enabled frequency tables and common univariate statistics in concise
tables available in a variety of formats (plain ASCII, Markdown and HTML).  A
good point-of-entry for exploring data, both for experienced and new R
users.")
    (license license:gpl2)))

(define-public r-lsei
  (package
    (name "r-lsei")
    (version "1.2-0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "lsei" version))
       (sha256
        (base32
         "1xl06fb3is744pxlh42wx5hn1h0ab1k31wnmsmh0524kxzcyp0a7"))))
    (build-system r-build-system)
    (native-inputs
     `(("gfortran" ,gfortran)))
    (home-page "https://www.stat.auckland.ac.nz/~yongwang")
    (synopsis "Solve regression problems under equality/inequality constraints")
    (description
     "It contains functions that solve least squares linear regression
problems under linear equality/inequality constraints.  Functions for solving
quadratic programming problems are also available, which transform such
problems into least squares ones first.")
    (license license:gpl2+)))

(define-public r-npsurv
  (package
    (name "r-npsurv")
    (version "0.4-0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "npsurv" version))
       (sha256
        (base32
         "1wq4c9yfha5azjhrn40iiqkshmvh611sa90jp3lh82n4bl9zfk20"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-lsei" ,r-lsei)))
    (home-page "https://www.stat.auckland.ac.nz/~yongwang")
    (synopsis "Nonparametric survival analysis")
    (description
     "This package contains functions for non-parametric survival analysis of
exact and interval-censored observations.")
    (license license:gpl2+)))

(define-public r-clusteval
  (package
    (name "r-clusteval")
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "clusteval" version))
       (sha256
        (base32
         "1ld0bdl4fy8dsfzm3k7a37cyxc6pfc9qs31x4pxd3z5rslghz7rj"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-mvtnorm" ,r-mvtnorm)
       ("r-rcpp" ,r-rcpp)))
    (home-page "https://cran.r-project.org/web/packages/clusteval/")
    (synopsis "Evaluation of clustering algorithms")
    (description
     "This R package provides a suite of tools to evaluate clustering
algorithms, clusterings, and individual clusters.")
    (license license:expat)))

(define-public r-tweedie
  (package
    (name "r-tweedie")
    (version "2.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "tweedie" version))
       (sha256
        (base32
         "10fv998qjxsyx0h94fi0xbh6xbf24nwgh254n9zfnmix9vk2cqls"))))
    (build-system r-build-system)
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/tweedie/")
    (synopsis "Evaluation of Tweedie exponential family models")
    (description
     "Maximum likelihood computations for Tweedie families, including the
series expansion (Dunn and Smyth, 2005; <doi10.1007/s11222-005-4070-y>) and
the Fourier inversion (Dunn and Smyth, 2008; <doi:10.1007/s11222-007-9039-6>),
and related methods.")
    (license license:gpl2+)))

(define-public r-rcppgsl
  (package
    (name "r-rcppgsl")
    (version "0.3.7")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "RcppGSL" version))
       (sha256
        (base32 "0cnw2k7cfqrm79r6j283aybflxig80x4n4rjkfp2317wf10mrsa5"))))
    (properties `((upstream-name . "RcppGSL")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rcpp" ,r-rcpp)
       ("gsl" ,gsl)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))           ; for vignettes
    (home-page "https://cran.r-project.org/web/packages/RcppGSL/")
    (synopsis "Rcpp integration for GSL vectors and matrices")
    (description
     "The GNU Scientific Library (or GSL) is a collection of numerical
routines for scientific computing.  It is particularly useful for C and C++
programs as it provides a standard C interface to a wide range of mathematical
routines.  There are over 1000 functions in total with an extensive test
suite.  The RcppGSL package provides an easy-to-use interface between GSL data
structures and R using concepts from Rcpp which is itself a package that eases
the interfaces between R and C++.")
    (license license:gpl2+)))

(define-public r-mvabund
  (package
    (name "r-mvabund")
    (version "4.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "mvabund" version))
       (sha256
        (base32
         "1z58h4dk3mc2hfnfvc7pghk471cbp7ah2s1z2ria5igw4s80962b"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-mass" ,r-mass)
       ("r-rcpp" ,r-rcpp)
       ("r-rcppgsl" ,r-rcppgsl)
       ("r-statmod" ,r-statmod)
       ("r-tweedie" ,r-tweedie)))
    (home-page "https://cran.r-project.org/web/packages/mvabund/")
    (synopsis "Statistical methods for analysing multivariate abundance data")
    (description
     "This package provides a set of tools for displaying, modeling and
analysing multivariate abundance data in community ecology.")
    (license license:lgpl2.1+)))

(define-public r-afex
  (package
    (name "r-afex")
    (version "0.26-0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "afex" version))
       (sha256
        (base32
         "0h3p1svgk1ap3lj08fi8nzdb3710h99bv150krf1x8wci1a0r1if"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-car" ,r-car)
       ("r-lme4" ,r-lme4)
       ("r-lmertest" ,r-lmertest)
       ("r-pbkrtest" ,r-pbkrtest)
       ("r-reshape2" ,r-reshape2)))
    (home-page "https://afex.singmann.science/")
    (synopsis "Analysis of factorial experiments")
    (description
     "This package provides convenience functions for analyzing factorial
experiments using ANOVA or mixed models.")
    (license license:gpl2+)))

(define-public r-lmertest
  (package
    (name "r-lmertest")
    (version "3.1-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "lmerTest" version))
       (sha256
        (base32
         "0r2vvs3nl6p8xla3gd943khb4ixp0alvspqpnz2y6n3wk8zgh3jj"))))
    (properties `((upstream-name . "lmerTest")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-lme4" ,r-lme4)
       ("r-mass" ,r-mass)
       ("r-numderiv" ,r-numderiv)))
    (home-page "https://github.com/runehaubo/lmerTestR")
    (synopsis "Tests in linear mixed effects models")
    (description
     "This package provides p-values in type I, II or III anova and summary
tables for @code{lmer} model fits via Satterthwaite's degrees of freedom
method.  A Kenward-Roger method is also available via the @code{pbkrtest}
package.  Model selection methods include step, drop1 and anova-like tables
for random effects (ranova).  Methods for Least-Square means (LS-means) and
tests of linear contrasts of fixed effects are also available.")
    (license license:gpl2+)))

(define-public r-r2glmm
  (package
    (name "r-r2glmm")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "r2glmm" version))
       (sha256
        (base32
         "0iim92blpa59vgz97c2pi05yhbjjmaffdbkbmk5kplfb2vmazgiy"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-afex" ,r-afex)
       ("r-data-table" ,r-data-table)
       ("r-dplyr" ,r-dplyr)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gridextra" ,r-gridextra)
       ("r-lmertest" ,r-lmertest)
       ("r-mass" ,r-mass)
       ("r-matrix" ,r-matrix)
       ("r-mgcv" ,r-mgcv)
       ("r-pbkrtest" ,r-pbkrtest)))
    (home-page "https://github.com/bcjaeger/r2glmm")
    (synopsis "Compute R squared for mixed (multilevel) models")
    (description
     "This package computes model and semi partial R squared with confidence
limits for the linear and generalized linear mixed model (LMM and GLMM).  The
R squared measure from L. J. Edwards et al. (2008) is extended to the GLMM
using @dfn{penalized quasi-likelihood} (PQL) estimation (see Jaeger et
al. (2016)).")
    (license license:gpl2)))

(define-public r-weights
  (package
    (name "r-weights")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "weights" version))
       (sha256
        (base32
         "1ka2kvzg464vn80qziqy4mrciy9wwd3jfasgq0d33wbiblhmxkj5"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-gdata" ,r-gdata)
       ("r-hmisc" ,r-hmisc)
       ("r-mice" ,r-mice)))
    (home-page
     "https://cran.r-project.org/web/packages/weights/")
    (synopsis "Weighting and weighted statistics")
    (description "This package Provides a variety of functions for producing
simple weighted statistics, such as weighted Pearson's correlations, partial
correlations, Chi-Squared statistics, histograms, and t-tests.  Also now
includes some software for quickly recoding survey data and plotting point
estimates from interaction terms in regressions (and multiply imputed
regressions).  NOTE: Weighted partial correlation calculations pulled to
address a bug.")
  (license license:gpl2+)))

(define-public r-rcppannoy
  (package
    (name "r-rcppannoy")
    (version "0.0.16")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "RcppAnnoy" version))
       (sha256
        (base32
         "0bfa35lp6vc4b0h3ymvdx50br233q8vvyjml34ngi81rj0imz3fr"))))
    (properties `((upstream-name . "RcppAnnoy")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rcpp" ,r-rcpp)))
    (home-page "https://cran.r-project.org/web/packages/RcppAnnoy/")
    (synopsis "Rcpp bindings for Annoy, a library for Approximate Nearest Neighbors")
    (description
     "Annoy is a small C++ library for Approximate Nearest Neighbors written
for efficient memory usage as well an ability to load from and save to disk.
This package provides an R interface.")
    ;; Annoy is released under ASL 2.0, but this wrapper is released under
    ;; GPLv2+.
    (license (list license:gpl2+ license:asl2.0))))

(define-public r-rcpphnsw
  (package
    (name "r-rcpphnsw")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "RcppHNSW" version))
       (sha256
        (base32
         "0gqdkw7vkcm544rz45g0hplg836ygzbfwk9gh9wr0817icvdb3qv"))))
    (properties `((upstream-name . "RcppHNSW")))
    (build-system r-build-system)
    (propagated-inputs `(("r-rcpp" ,r-rcpp)))
    (home-page "https://cran.r-project.org/web/packages/RcppHNSW/")
    (synopsis "Rcpp bindings for hnswlib, a library for approximate nearest neighbors")
    (description
     "Hnswlib is a C++ library for approximate nearest neighbors.  This
package provides a minimal R interface by relying on the Rcpp package.")
    ;; hnswlib is released under Version 2.0 of the Apache License.
    (license (list license:gpl3 license:asl2.0))))

(define-public r-rcppparallel
  (package
    (name "r-rcppparallel")
    (version "5.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "RcppParallel" version))
       (sha256
        (base32
         "1mhd6vp47xmfw533h0pkvydv96m57fspvd85g8m7iqb5rcxvhhdb"))))
    (properties `((upstream-name . "RcppParallel")))
    (build-system r-build-system)
    (home-page "https://rcppcore.github.io/RcppParallel/")
    (synopsis "Parallel programming tools for Rcpp")
    (description
     "This package provides high level functions for parallel programming with
Rcpp.  For example, the @code{parallelFor()} function can be used to convert
the work of a standard serial @code{for} loop into a parallel one and the
@code{parallelReduce()} function can be used for accumulating aggregates or
other values.")
    (license license:gpl2)))

(define-public r-ncdf4
  (package
    (name "r-ncdf4")
    (version "1.17")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ncdf4" version))
       (sha256
        (base32
         "1xls44ln2zjrrlimxl8v4bk2ni3g45c9j0gxdnjx31rikmrc95fv"))))
    (build-system r-build-system)
    (inputs
     `(("netcdf" ,netcdf)
       ("zlib" ,zlib)))
    (home-page "https://cran.r-project.org/web/packages/ncdf4/index.html")
    (synopsis "R interface to Unidata netCDF format data files")
    (description
     "This package provides a high-level R interface to data files written
using Unidata's netCDF library (version 4 or earlier), which are binary data
files that are portable across platforms and include metadata information in
addition to the data sets.  Using this package, netCDF files can be opened and
data sets read in easily.  It is also easy to create new netCDF dimensions,
variables, and files, in either version 3 or 4 format, and manipulate existing
netCDF files.")
    (license license:gpl3+)))

(define-public r-biocmanager
  (package
    (name "r-biocmanager")
    (version "1.30.10")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "BiocManager" version))
       (sha256
        (base32 "03n9s2vf7vgpgb5alpxwamf9xfkn32cbzngwyn6spq1bnh9a9dzk"))))
    (properties `((upstream-name . "BiocManager")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/BiocManager/")
    (synopsis "Access the Bioconductor project package repository")
    (description
     "This package provides a convenient tool to install and update
Bioconductor packages.")
    (license license:artistic2.0)))

(define-public r-rgl
  (package
    (name "r-rgl")
    (version "0.100.50")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rgl" version))
       (sha256
        (base32
         "165p932ml7dpjkm41zc47p5cdxar69il0m5yvg0avi8q01vr17ay"))))
    (build-system r-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("freetype" ,freetype)
       ("libpng" ,libpng)
       ("glu" ,glu)
       ("libx11" ,libx11)
       ("ghc-pandoc" ,ghc-pandoc)
       ("zlib" ,zlib)))
    (propagated-inputs
     `(("r-crosstalk" ,r-crosstalk)
       ("r-htmltools" ,r-htmltools)
       ("r-htmlwidgets" ,r-htmlwidgets)
       ("r-jsonlite" ,r-jsonlite)
       ("r-knitr" ,r-knitr)
       ("r-magrittr" ,r-magrittr)
       ("r-manipulatewidget" ,r-manipulatewidget)
       ("r-shiny" ,r-shiny)))
    (home-page "https://r-forge.r-project.org/projects/rgl/")
    (synopsis "3D visualization using OpenGL")
    (description
     "This package provides medium to high level functions for 3D interactive graphics,
including functions modelled on base graphics (@code{plot3d()}, etc.) as well
as functions for constructing representations of geometric
objects (@code{cube3d()}, etc.).  Output may be on screen using OpenGL, or to
various standard 3D file formats including WebGL, PLY, OBJ, STL as well as 2D
image formats, including PNG, Postscript, SVG, PGF.")
    ;; Any version of the GPL.
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-multicool
  (package
    (name "r-multicool")
    (version "0.1-11")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "multicool" version))
       (sha256
        (base32
         "0xk408qbz9cxwf51j3pmy55gcjcnws8mc6j3vyn9zhramxj7x40w"))))
    (build-system r-build-system)
    (propagated-inputs `(("r-rcpp" ,r-rcpp)))
    (home-page "https://cran.r-project.org/web/packages/multicool/")
    (synopsis "Permutations of multisets in cool-lex order")
    (description
     "This package provides a set of tools to permute multisets without loops
or hash tables and to generate integer partitions.  Cool-lex order is similar
to colexicographical order.")
    (license license:gpl2)))

(define-public r-misc3d
  (package
    (name "r-misc3d")
    (version "0.8-4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "misc3d" version))
       (sha256
        (base32
         "0qjzpw3h09qi2gfz52b7nhzd95p7yyxsd03fldc9wzzn6wi3vpkm"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/misc3d/")
    (synopsis "Miscellaneous 3D Plots")
    (description
     "This package provides a collection of miscellaneous 3d plots, including
isosurfaces.")
    ;; Any version of the GPL.
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-ks
  (package
    (name "r-ks")
    (version "1.11.7")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ks" version))
       (sha256
        (base32 "1f6jvxy0hmngyvnvrknzbmhl42njk0vqyycvydm4qnp8cqirqvba"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-fnn" ,r-fnn)
       ("r-kernlab" ,r-kernlab)
       ("r-kernsmooth" ,r-kernsmooth)
       ("r-matrix" ,r-matrix)
       ("r-mclust" ,r-mclust)
       ("r-mgcv" ,r-mgcv)
       ("r-multicool" ,r-multicool)
       ("r-mvtnorm" ,r-mvtnorm)))
    (home-page "http://www.mvstat.net/tduong/")
    (synopsis "Kernel smoothing")
    (description
     "This package provides kernel smoothers for univariate and multivariate
data, including density functions, density derivatives, cumulative
distributions, modal clustering, discriminant analysis, and two-sample
hypothesis testing.")
    ;; Either version of the GPL.
    (license (list license:gpl2 license:gpl3))))

(define-public r-feature
  (package
    (name "r-feature")
    (version "1.2.13")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "feature" version))
       (sha256
        (base32
         "07hkw0bv38naj2hdsx4xxrm2dngi6w3rbvgr7s50bjic8hlgy1ra"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ks" ,r-ks)
       ("r-misc3d" ,r-misc3d)
       ("r-rgl" ,r-rgl)))
    (home-page "http://www.mvstat.net/tduong/")
    (synopsis "Inferential feature significance for kernel density estimation")
    (description
     "The feature package contains functions to display and compute kernel
density estimates, significant gradient and significant curvature regions.
Significant gradient and/or curvature regions often correspond to significant
features (e.g. local modes).")
    ;; Either version of the GPL.
    (license (list license:gpl2 license:gpl3))))

(define-public r-arm
  (package
    (name "r-arm")
    (version "1.10-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "arm" version))
       (sha256
        (base32
         "0vvp90jygajd6ydky57z66wqjq9msfbl88irj5jbsray574mh4bg"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-abind" ,r-abind)
       ("r-coda" ,r-coda)
       ("r-lme4" ,r-lme4)
       ("r-mass" ,r-mass)
       ("r-matrix" ,r-matrix)
       ("r-nlme" ,r-nlme)))
    (home-page "https://cran.r-project.org/web/packages/arm/")
    (synopsis "Data analysis using regression and multilevel/hierarchical models")
    (description
     "This package provides functions to accompany A. Gelman and J. Hill,
Data Analysis Using Regression and Multilevel/Hierarchical Models, Cambridge
University Press, 2007.")
    (license license:gpl3+)))

(define-public r-circular
  (package
    (name "r-circular")
    (version "0.4-93")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "circular" version))
       (sha256
        (base32
         "0hki85rs8wc5950pjaw28q54rly2napfbcrx3pchlfap6wwy5kkn"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-boot" ,r-boot)
       ("r-mvtnorm" ,r-mvtnorm)))
    (native-inputs
     `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/circular/")
    (synopsis "Circular statistics")
    (description
     "This package provides tools for circular statistics, from \"Topics in
circular Statistics\" (2001) S. Rao Jammalamadaka and A. SenGupta, World
Scientific.")
    (license license:gpl2+)))

(define-public r-activity
  (package
    (name "r-activity")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "activity" version))
       (sha256
        (base32
         "12imqj366dp6pam5gap6ji56p5wf1073xz5g4iikfxf5l8snxw92"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-circular" ,r-circular)
       ("r-insol" ,r-insol)
       ("r-pbapply" ,r-pbapply)))
    (home-page "https://cran.r-project.org/web/packages/activity/")
    (synopsis "Animal activity statistics")
    (description
     "This package provides functions to fit kernel density functions to
animal activity time data; plot activity distributions; quantify overall
levels of activity; statistically compare activity metrics through
bootstrapping; and evaluate variation in linear variables with time (or other
circular variables).")
    (license license:gpl3)))

(define-public r-ouch
  (package
    (name "r-ouch")
    (version "2.14-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ouch" version))
       (sha256
        (base32
         "0ddf9bw5lhj8vb0ja78jf99i0smq4rgmm842k4a4ygap41vdyn2b"))))
    (build-system r-build-system)
    (propagated-inputs `(("r-subplex" ,r-subplex)))
    (home-page "https://kingaa.github.io/ouch/")
    (synopsis "Ornstein-Uhlenbeck models for phylogenetic comparative hypotheses")
    (description
     "This package provides tools to fit and compare Ornstein-Uhlenbeck models
for evolution along a phylogenetic tree.")
    (license license:gpl2+)))

(define-public r-fmsb
  (package
    (name "r-fmsb")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "fmsb" version))
       (sha256
        (base32
         "0x1wkzfdvv4s5xmr0whcwjz4aac71gacwymj2c3mzj2bbswwlw45"))))
    (build-system r-build-system)
    (home-page "http://minato.sip21c.org/msb/")
    (synopsis "Functions for medical statistics book with demographic data")
    (description
     "This package provides several utility functions for the book entitled
\"Practices of Medical and Health Data Analysis using R\" (Pearson Education
Japan, 2007) with Japanese demographic data and some demographic analysis
related functions.")
    (license license:gpl2+)))

(define-public r-stabledist
  (package
    (name "r-stabledist")
    (version "0.7-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "stabledist" version))
       (sha256
        (base32
         "0scar396wiq6wkbkvwp4qrxqc1m075y56p37i6iry5rw796p1i86"))))
    (build-system r-build-system)
    (home-page "http://www.rmetrics.org")
    (synopsis "Stable distribution functions")
    (description
     "This package provides density, probability and quantile functions, and
random number generation for (skew) stable distributions, using the
parametrizations of Nolan.")
    (license license:gpl2+)))

(define-public r-gsl
  (package
    (name "r-gsl")
    (version "2.1-6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "gsl" version))
       (sha256
        (base32
         "0p4rh7npp6qbfc5sxjq86xjn7c9ivf3pd60qf1hldwckjqin7m7m"))))
    (build-system r-build-system)
    (inputs
     `(("gsl" ,gsl)))
    (home-page "https://cran.r-project.org/web/packages/gsl")
    (synopsis "Wrapper for the GNU Scientific Library")
    (description
     "This package provides an R wrapper for the special functions and quasi
random number generators of the GNU Scientific Library.")
    (license license:gpl2+)))

(define-public r-adgoftest
  (package
    (name "r-adgoftest")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ADGofTest" version))
       (sha256
        (base32
         "0ik817qzqp6kfbckjp1z7srlma0w6z2zcwykh0jdiv7nahwk3ncw"))))
    (properties `((upstream-name . "ADGofTest")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/ADGofTest")
    (synopsis "Anderson-Darling GoF test")
    (description
     "This package provides an implementation of the Anderson-Darling GoF test
with p-value calculation based on Marsaglia's 2004 paper \"Evaluating the
Anderson-Darling Distribution\".")
    ;; Any version of the GPL.
    (license license:gpl3+)))

(define-public r-softimpute
  (package
    (name "r-softimpute")
    (version "1.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "softImpute" version))
       (sha256
        (base32
         "07cxbzkl08q58m1455i139952rmryjlic4s2f2hscl5zxxmfdxcq"))))
    (properties `((upstream-name . "softImpute")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-matrix" ,r-matrix)))
    (native-inputs
     `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/softImpute")
    (synopsis "Matrix completion via iterative soft-thresholded SVD")
    (description
     "This package provides iterative methods for matrix completion that use
nuclear-norm regularization.  The package includes procedures for centering
and scaling rows, columns or both, and for computing low-rank @dfn{single
value decompositions} (SVDs) on large sparse centered matrices (i.e. principal
components).")
    (license license:gpl2)))

(define-public r-fftwtools
  (package
    (name "r-fftwtools")
    (version "0.9-8")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "fftwtools" version))
       (sha256
        (base32
         "1nqvpzda281rxi1cmwajxxsn3sc3gz7scv8bvs5jm34kf36whha6"))))
    (build-system r-build-system)
    (inputs `(("fftw" ,fftw)))
    (home-page "https://github.com/krahim/fftwtools")
    (synopsis "Wrapper for FFTW3")
    (description
     "This package provides a wrapper for several FFTW functions.  It provides
access to the two-dimensional FFT, the multivariate FFT, and the
one-dimensional real to complex FFT using the FFTW3 library.  The package
includes the functions @code{fftw()} and @code{mvfftw()} which are designed to
mimic the functionality of the R functions @code{fft()} and @code{mvfft()}.
The FFT functions have a parameter that allows them to not return the
redundant complex conjugate when the input is real data.")
    (license license:gpl2+)))

(define-public r-tiff
  (package
    (name "r-tiff")
    (version "0.1-5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "tiff" version))
       (sha256
        (base32
         "0asf2bws3x3yd3g3ixvk0f86b0mdf882pl8xrqlxrkbgjalyc54m"))))
    (build-system r-build-system)
    (inputs
     `(("libtiff" ,libtiff)
       ("libjpeg" ,libjpeg)
       ("zlib" ,zlib)))
    (home-page "https://www.rforge.net/tiff/")
    (synopsis "Read and write TIFF images")
    (description
     "This package provides an easy and simple way to read, write and display
bitmap images stored in the TIFF format.  It can read and write both files and
in-memory raw vectors.")
    ;; Either of these two license versions.
    (license (list license:gpl2 license:gpl3))))

(define-public r-nlp
  (package
    (name "r-nlp")
    (version "0.2-0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "NLP" version))
       (sha256
        (base32
         "0xbhkrnxcbf322jfw31xcn4y2gnk5y7ccq1bz4h3prf44h0whr7w"))))
    (properties `((upstream-name . "NLP")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/NLP/")
    (synopsis "Natural language processing infrastructure")
    (description
     "This package provides basic classes and methods for Natural Language
Processing.")
    (license license:gpl3)))

(define-public r-tm
  (package
    (name "r-tm")
    (version "0.7-7")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "tm" version))
       (sha256
        (base32
         "0pyics8j7a4wkh5gzin46l0qars5vgbb1886xqpdqjs1z0gy9nyh"))))
    (properties `((upstream-name . "tm")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bh" ,r-bh)
       ("r-nlp" ,r-nlp)
       ("r-rcpp" ,r-rcpp)
       ("r-slam" ,r-slam)
       ("r-xml2" ,r-xml2)))
    (home-page "http://tm.r-forge.r-project.org/")
    (synopsis "Text mining package")
    (description
     "This package provides a framework for text mining applications within R.")
    (license license:gpl3)))

(define-public r-waveslim
  (package
    (name "r-waveslim")
    (version "1.7.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "waveslim" version))
       (sha256
        (base32
         "166ai4q3mlh3v338si952z7fbgph80h0l4ws6bna8ig5jx81rnmz"))))
    (build-system r-build-system)
    (native-inputs
     `(("gfortran" ,gfortran)))
    (home-page "http://waveslim.blogspot.com")
    (synopsis "Basic wavelet routines for signal processing")
    (description
     "This package provides basic wavelet routines for time series (1D),
image (2D) and array (3D) analysis.  The code provided here is based on
wavelet methodology developed in Percival and Walden (2000); Gencay, Selcuk
and Whitcher (2001); the dual-tree complex wavelet transform (DTCWT) from
Kingsbury (1999, 2001) as implemented by Selesnick; and Hilbert wavelet
pairs (Selesnick 2001, 2002).")
    (license license:bsd-3)))

(define-public r-wordcloud
  (package
    (name "r-wordcloud")
    (version "2.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "wordcloud" version))
       (sha256
        (base32
         "0j96yyvm6bcrrpbdx4w26piqx44a0vbsr3px9cb4zk8a8da6jwak"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-rcpp" ,r-rcpp)
       ;; The "tm" package is only "suggested" according to CRAN, but the
       ;; wordcloud package cannot be loaded without it.
       ("r-tm" ,r-tm)))
    (home-page "https://cran.r-project.org/web/packages/wordcloud")
    (synopsis "Word clouds")
    (description
     "This package provides functionality to create pretty word clouds,
visualize differences and similarity between documents, and avoid
over-plotting in scatter plots with text.")
    (license license:lgpl2.1)))

(define-public r-colorramps
  (package
    (name "r-colorramps")
    (version "2.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "colorRamps" version))
       (sha256
        (base32
         "0shbjh83x1axv4drm5r3dwgbyv70idih8z4wlzjs4hiac2qfl41z"))))
    (properties `((upstream-name . "colorRamps")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/colorRamps")
    (synopsis "Build color tables")
    (description "This package provides features to build gradient color
maps.")
    ;; Any version of the GPL
    (license license:gpl3+)))

(define-public r-tidytree
  (package
    (name "r-tidytree")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "tidytree" version))
       (sha256
        (base32 "0dx9jn19mfykn20camsmq1amlgg0w6z5hn5rdqygs1fk1l5aazad"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ape" ,r-ape)
       ("r-dplyr" ,r-dplyr)
       ("r-lazyeval" ,r-lazyeval)
       ("r-magrittr" ,r-magrittr)
       ("r-rlang" ,r-rlang)
       ("r-tibble" ,r-tibble)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/GuangchuangYu/tidytree")
    (synopsis "Tidy tool for phylogenetic tree data manipulation")
    (description
     "Phylogenetic trees generally contain multiple components including nodes,
edges, branches and associated data.  This package provides an approach to
convert tree objects to tidy data frames.  It also provides tidy interfaces to
manipulate tree data.")
    (license license:artistic2.0)))

(define-public r-rvcheck
  (package
    (name "r-rvcheck")
    (version "0.1.8")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rvcheck" version))
       (sha256
        (base32 "0627bc8qmhxmd63yh6f90qni3qw1zwdpxjln2qbychzmzd4am9ac"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-biocmanager" ,r-biocmanager)
       ("r-rlang" ,r-rlang)))
    (home-page "https://cran.r-project.org/web/packages/rvcheck")
    (synopsis "R package version check")
    (description
     "This package provides tools to check the latest release version of R and
R packages (on CRAN, Bioconductor or Github).")
    (license license:artistic2.0)))

(define-public r-docopt
  (package
    (name "r-docopt")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "docopt" version))
       (sha256
        (base32
         "06zknnd0c5s2y0hbddzdlr3m63ib783izpck6pgz7sjbab5pd068"))))
    (build-system r-build-system)
    (home-page "https://github.com/docopt/docopt.R")
    (synopsis "Command-line interface specification language")
    (description
     "This package enables you to define a command-line interface by just
giving it a description in the specific format.")
    (license license:expat)))

(define-public r-sparsesvd
  (package
    (name "r-sparsesvd")
    (version "0.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "sparsesvd" version))
       (sha256
        (base32
         "1xm969fjq3fv1p2sqza2apz8picibj4s2agpwf1sx9nwn3b587qs"))))
    (build-system r-build-system)
    (propagated-inputs `(("r-matrix" ,r-matrix)))
    (home-page "http://tedlab.mit.edu/~dr/SVDLIBC/")
    (synopsis "Sparse truncated singular value decomposition")
    (description
     "This package provides a Wrapper around the SVDLIBC library
for (truncated) singular value decomposition of a sparse matrix.  Currently,
only sparse real matrices in Matrix package format are supported.")
    ;; SVDLIBC is released under BSD-2.  The R interface is released under
    ;; BSD-3.
    (license (list license:bsd-3 license:bsd-2))))

(define-public r-speedglm
  (package
    (name "r-speedglm")
    (version "0.3-2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "speedglm" version))
       (sha256
        (base32
         "1b25zimk0z7ad62yacqdg0zk0qs0jja4i918ym942xfw4j1z3jjz"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-mass" ,r-mass)
       ("r-matrix" ,r-matrix)))
    (home-page "https://cran.r-project.org/web/packages/speedglm")
    (synopsis "Fit linear and generalized linear models to large data sets")
    (description
     "This package provides tools for fitting linear models and generalized
linear models to large data sets by updating algorithms.")
    ;; Any version of the GPL
    (license license:gpl2+)))

(define-public r-densityclust
  (package
    (name "r-densityclust")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "densityClust" version))
       (sha256
        (base32
         "1zry0vafajzmr37aylglxfvwplhdygbkb9cvzvh8cy0xgnjrnx13"))))
    (properties `((upstream-name . "densityClust")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-fnn" ,r-fnn)
       ("r-ggplot2" ,r-ggplot2)
       ("r-ggrepel" ,r-ggrepel)
       ("r-gridextra" ,r-gridextra)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-rcpp" ,r-rcpp)
       ("r-rtsne" ,r-rtsne)))
    (home-page "https://cran.r-project.org/web/packages/densityClust")
    (synopsis "Clustering by fast search and find of density peaks")
    (description
     "This package provides an improved implementation (based on k-nearest
neighbors) of the density peak clustering algorithm, originally described by
Alex Rodriguez and Alessandro Laio (Science, 2014 vol. 344).  It can handle
large datasets (> 100,000 samples) very efficiently.")
    (license license:gpl2+)))

(define-public r-combinat
  (package
    (name "r-combinat")
    (version "0.0-8")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "combinat" version))
       (sha256
        (base32
         "1h9hr88gigihc4na7lb5i7rn4az1xa7sb34zvnznaj6pdrmwy4qm"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/combinat")
    (synopsis "Combinatorics utilities")
    (description "This package provides assorted routines for combinatorics.")
    (license license:gpl2)))

(define-public r-qlcmatrix
  (package
    (name "r-qlcmatrix")
    (version "0.9.7")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "qlcMatrix" version))
       (sha256
        (base32
         "0iqkcvvy8rxlk0s83sjq57dd6fadb18p5z31lzy0gnzv1hsy1x8y"))))
    (properties `((upstream-name . "qlcMatrix")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-docopt" ,r-docopt)
       ("r-matrix" ,r-matrix)
       ("r-slam" ,r-slam)
       ("r-sparsesvd" ,r-sparsesvd)))
    (home-page "https://cran.r-project.org/web/packages/qlcMatrix")
    (synopsis "Sparse matrix functions for quantitative language comparison")
    (description
     "This package provides an extension of the functionality of the Matrix
package for using sparse matrices.  Some of the functions are very general,
while other are highly specific for the special data format used for
@dfn{quantitative language comparison} (QLC).")
    (license license:gpl3)))

(define-public r-ddrtree
  (package
    (name "r-ddrtree")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "DDRTree" version))
       (sha256
        (base32
         "16s5fjw7kwlxhrkzdny62sx32fvmg3rxjc3wrh6krd31jh1fqlfk"))))
    (properties `((upstream-name . "DDRTree")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bh" ,r-bh)
       ("r-irlba" ,r-irlba)
       ("r-rcpp" ,r-rcpp)
       ("r-rcppeigen" ,r-rcppeigen)))
    (home-page "https://cran.r-project.org/web/packages/DDRTree")
    (synopsis "Learning principal graphs with DDRTree")
    (description
     "This package provides an implementation of the framework of
@dfn{reversed graph embedding} (RGE) which projects data into a reduced
dimensional space while constructs a principal tree which passes through the
middle of the data simultaneously.  DDRTree shows superiority to
alternatives (Wishbone, DPT) for inferring the ordering as well as the
intrinsic structure of single cell genomics data.  In general, it could be
used to reconstruct the temporal progression as well as the bifurcation
structure of any data type.")
    (license license:asl2.0)))

(define-public r-corpcor
  (package
    (name "r-corpcor")
    (version "1.6.9")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "corpcor" version))
       (sha256
        (base32
         "1hi3i9d3841snppq1ks5pd8cliq1b4rm4dpsczmfqvwksg8snkrf"))))
    (build-system r-build-system)
    (home-page "http://strimmerlab.org/software/corpcor/")
    (synopsis "Efficient estimation of covariance and (partial) correlation")
    (description
     "This package implements a James-Stein-type shrinkage estimator for the
covariance matrix, with separate shrinkage for variances and correlations.
Furthermore, functions are available for fast singular value decomposition,
for computing the pseudoinverse, and for checking the rank and positive
definiteness of a matrix.")
    (license license:gpl3+)))

(define-public r-rspectra
  (package
    (name "r-rspectra")
    (version "0.16-0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "RSpectra" version))
       (sha256
        (base32
         "1ab45as2ysjrvkhvmx7y3nbhd0y1w4j9k2a789lcd973zz4wzwda"))))
    (properties `((upstream-name . "RSpectra")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-matrix" ,r-matrix)
       ("r-rcpp" ,r-rcpp)
       ("r-rcppeigen" ,r-rcppeigen)))
    (home-page "https://github.com/yixuan/RSpectra")
    (synopsis "Solvers for large-scale Eigenvalue and SVD problems")
    (description
     "This package provides an R interface to the Spectra library for
large-scale eigenvalue and SVD problems.  It is typically used to compute a
few eigenvalues/vectors of an n by n matrix, e.g., the k largest eigenvalues,
which is usually more efficient than @code{eigen()} if k << n.")
    ;; MPL 2 or later.
    (license license:mpl2.0)))

(define-public r-vbsr
  (package
    (name "r-vbsr")
    (version "0.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "vbsr" version))
       (sha256
        (base32
         "1avskbxxyinjjdga4rnghcfvd4sypv4m39ysfaij5avvmi89bx3b"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/vbsr")
    (synopsis "Variational Bayes spike regression regularized linear models")
    (description
     "This package provides an efficient algorithm for solving ultra-sparse
regularized regression models using a variational Bayes algorithm with a spike
prior.  The algorithm is solved on a path, with coordinate updates, and is
capable of generating very sparse models.  Very general model
diagnostics for controlling type-1 errors are also provided.")
    (license license:gpl2)))

(define-public r-flare
  (package
    (name "r-flare")
    (version "1.6.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "flare" version))
       (sha256
        (base32
         "1ybrsx1djqldw0l5l1iz4pfh6xxb8ckkg1ric7wnsr51wm9ljlh5"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-igraph" ,r-igraph)
       ("r-lattice" ,r-lattice)
       ("r-mass" ,r-mass)
       ("r-matrix" ,r-matrix)))
    (home-page "https://cran.r-project.org/web/packages/flare")
    (synopsis "Family of Lasso regression implementations")
    (description
     "This package provides implementations of a family of Lasso variants
including Dantzig Selector, LAD Lasso, SQRT Lasso, Lq Lasso for estimating
high dimensional sparse linear models.")
    (license license:gpl2)))

(define-public r-lassopv
  (package
    (name "r-lassopv")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "lassopv" version))
       (sha256
        (base32
         "0yawnjw063jypk3riy9xab9cmliv6c9dnabi18670khd3gzb2r9z"))))
    (build-system r-build-system)
    (propagated-inputs `(("r-lars" ,r-lars)))
    (home-page "https://github.com/lingfeiwang/lassopv")
    (synopsis "Non-parametric p-value estimation for predictors in Lasso")
    (description
     "This package enables you to estimate the p-values for predictors x
against target variable y in Lasso regression, using the regularization
strength when each predictor enters the active set of regularization path for
the first time as the statistic.")
    (license license:gpl3)))

(define-public r-splitstackshape
  (package
    (name "r-splitstackshape")
    (version "1.4.8")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "splitstackshape" version))
       (sha256
        (base32
         "0mpyf2kkfdl69pdc6brl1r6101vyc6pgr7z17s55ppg3y71k4q35"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-data-table" ,r-data-table)))
    (home-page "https://github.com/mrdwab/splitstackshape")
    (synopsis "Stack and reshape datasets after splitting concatenated values")
    (description
     "Online data collection tools like Google Forms often export
multiple-response questions with data concatenated in cells.  The
@code{concat.split} (cSplit) family of functions provided by this package
splits such data into separate cells.  This package also includes functions to
stack groups of columns and to reshape wide data, even when the data are
\"unbalanced\"---something which @code{reshape} (from base R) does not handle,
and which @code{melt} and @code{dcast} from @code{reshape2} do not easily
handle.")
    (license license:gpl3)))

(define-public r-tfmpvalue
  (package
    (name "r-tfmpvalue")
    (version "0.0.8")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "TFMPvalue" version))
       (sha256
        (base32
         "0h9qkl15k8v17v3g9bdnfwvh2s04ywjgg5y0xn2077dmywlja1bd"))))
    (properties `((upstream-name . "TFMPvalue")))
    (build-system r-build-system)
    (propagated-inputs `(("r-rcpp" ,r-rcpp)))
    (home-page "https://github.com/ge11232002/TFMPvalue")
    (synopsis "P-value computation for position weight matrices")
    (description
     "In putative @dfn{Transcription Factor Binding Sites} (TFBSs)
identification from sequence/alignments, we are interested in the significance
of certain match scores.  TFMPvalue provides the accurate calculation of a
p-value with a score threshold for position weight matrices, or the score with
a given p-value.  It is an interface to code originally made available by
Helene Touzet and Jean-Stephane Varre, 2007, Algorithms Mol Biol:2, 15.
Touzet and Varre (2007).")
    (license license:gpl2)))

(define-public r-rnifti
  (package
    (name "r-rnifti")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "RNifti" version))
       (sha256
        (base32
         "1z8ninp3aq18w0slcfn8r2fp48cdz8l0k0namsrnvgyp8lzcpqpn"))))
    (properties `((upstream-name . "RNifti")))
    (build-system r-build-system)
    (propagated-inputs `(("r-rcpp" ,r-rcpp)))
    (home-page "https://github.com/jonclayden/RNifti")
    (synopsis "Fast R and C++ access to NIfTI images")
    (description
     "This package provides very fast read and write access to images stored
in the NIfTI-1 and ANALYZE-7.5 formats, with seamless synchronisation between
compiled C and interpreted R code.  It also provides a C/C++ API that can be
used by other packages.")
    (license license:gpl2)))

(define-public r-shades
  (package
    (name "r-shades")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "shades" version))
       (sha256
        (base32
         "1zg95sjhrfvbdlfc387g9p0vnb8nb6agdk1mb3wq3kwkm2da0bqj"))))
    (build-system r-build-system)
    (home-page "https://github.com/jonclayden/shades")
    (synopsis "Simple color manipulation")
    (description
     "This package provides functions for easily manipulating colors,
creating color scales and calculating color distances.")
    (license license:bsd-3)))

(define-public r-ore
  (package
    (name "r-ore")
    (version "1.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ore" version))
       (sha256
        (base32 "1vh6w3arrhgkfjjjw7ci91xmz4wpfr3cmwd5zkqch89dgn07skkv"))))
    (build-system r-build-system)
    (home-page "https://github.com/jonclayden/ore")
    (synopsis "R interface to the Onigmo regular expression library")
    (description
     "This package provides an alternative to R's built-in functionality for
handling regular expressions, based on the Onigmo library.  It offers
first-class compiled regex objects, partial matching and function-based
substitutions, amongst other features.")
    (license license:bsd-3)))

(define-public r-reportr
  (package
    (name "r-reportr")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "reportr" version))
       (sha256
        (base32
         "0zynplxqvbmf23cm2rsz3wz2jx6mv55z94mn1k44ny3lx625cnpw"))))
    (build-system r-build-system)
    (propagated-inputs `(("r-ore" ,r-ore)))
    (home-page "https://github.com/jonclayden/reportr")
    (synopsis "General message and error reporting system")
    (description
     "This package provides a system for reporting messages, which offers
certain useful features over the standard R system, such as the incorporation
of output consolidation, message filtering, assertions, expression
substitution, automatic generation of stack traces for debugging, and
conditional reporting based on the current \"output level\".")
    (license license:gpl2)))

(define-public r-tractor-base
  (package
    (name "r-tractor-base")
    (version "3.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "tractor.base" version))
       (sha256
        (base32
         "0y5gm0y4chl30f5qqq8qiiw4j8g32s4i9xrvyp3cwg902kf2p86i"))))
    (properties `((upstream-name . "tractor.base")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ore" ,r-ore)
       ("r-reportr" ,r-reportr)
       ("r-rnifti" ,r-rnifti)
       ("r-shades" ,r-shades)))
    (home-page "https://www.tractor-mri.org.uk")
    (synopsis "Read, manipulate and visualize magnetic resonance images")
    (description
     "This package provides functions for working with magnetic resonance
images.  It supports reading and writing of popular file formats (DICOM,
Analyze, NIfTI-1, NIfTI-2, MGH); interactive and non-interactive
visualization; flexible image manipulation; metadata and sparse image
handling.")
    (license license:gpl2)))

(define-public r-grimport
  (package
    (name "r-grimport")
    (version "0.9-3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "grImport" version))
       (sha256
        (base32
         "109mrdvq06xq3zgn9ngz0c7zzgqkv5zbpvsb2i636vmlk6y4dpkd"))))
    (properties `((upstream-name . "grImport")))
    (build-system r-build-system)
    (inputs
     `(("ghostscript" ,ghostscript)))
    (propagated-inputs
     `(("r-xml" ,r-xml)))
    (home-page "https://cran.r-project.org/web/packages/grImport")
    (synopsis "Convert, import, and draw PostScript pictures")
    (description
     "This package provides functions for converting, importing, and drawing
PostScript pictures in R plots.")
    (license license:gpl2+)))

(define-public r-grimport2
  (package
    (name "r-grimport2")
    (version "0.2-0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "grImport2" version))
       (sha256
        (base32
         "19q0dd8fpp1g4xf6sg5f8dxybwxjfw553ra6wgjd8b74fzca40m1"))))
    (properties `((upstream-name . "grImport2")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-base64enc" ,r-base64enc)
       ("r-jpeg" ,r-jpeg)
       ("r-png" ,r-png)
       ("r-xml" ,r-xml)))
    (home-page "https://cran.r-project.org/web/packages/grImport2/")
    (synopsis "Import SVG graphics")
    (description
     "This package provides functions for importing external vector images and
drawing them as part of R plots.  This package is different from the
@code{grImport} package because, where that package imports PostScript format
images, this package imports SVG format images.  Furthermore, this package
imports a specific subset of SVG, so external images must be preprocessed
using a package like @code{rsvg} to produce SVG that this package can import.
SVG features that are not supported by R graphics, such as gradient fills, can
be imported and then exported via the @code{gridSVG} package.")
    (license license:gpl2+)))

(define-public r-kohonen
  (package
    (name "r-kohonen")
    (version "3.0.10")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "kohonen" version))
       (sha256
        (base32
         "1ck7j13x701g67bx81x7plszz804jfhl1yg42krcj9x88vm5cscr"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rcpp" ,r-rcpp)))
    (home-page "https://cran.r-project.org/web/packages/kohonen")
    (synopsis "Supervised and unsupervised self-organising maps")
    (description
     "This package provides functions to train @dfn{self-organising
maps} (SOMs).  Also interrogation of the maps and prediction using trained
maps are supported.  The name of the package refers to Teuvo Kohonen, the
inventor of the SOM.")
    (license license:gpl2+)))

(define-public r-nnls
  (package
    (name "r-nnls")
    (version "1.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "nnls" version))
       (sha256
        (base32
         "07vcrrxvswrvfiha6f3ikn640yg0m2b4yd9lkmim1g0jmsmpfp8f"))))
    (build-system r-build-system)
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/nnls")
    (synopsis "Lawson-Hanson algorithm for non-negative least squares")
    (description
     "This package provides an R interface to the Lawson-Hanson implementation
of an algorithm for @dfn{non-negative least squares} (NNLS).  It also allows
the combination of non-negative and non-positive constraints.")
    (license license:gpl2+)))

(define-public r-iso
  (package
    (name "r-iso")
    (version "0.0-18")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Iso" version))
       (sha256
        (base32
         "014mm5b1f7i6nwlz3kyg1biph0y542kcx5bd13p68cv5a928qzid"))))
    (properties `((upstream-name . "Iso")))
    (build-system r-build-system)
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "https://www.stat.auckland.ac.nz/~rolf/")
    (synopsis "Functions to perform isotonic regression")
    (description
     "This package provides support for linear order and unimodal
order (univariate) isotonic regression and bivariate isotonic regression with
linear order on both variables.")
    (license license:gpl2+)))

(define-public r-chemometricswithr
  (package
    (name "r-chemometricswithr")
    (version "0.1.13")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ChemometricsWithR" version))
       (sha256
        (base32
         "166va1g3m1wv21qkmw4wpz0bsrclh3jih8smxphdc13l9pqgclpq"))))
    (properties
     `((upstream-name . "ChemometricsWithR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-devtools" ,r-devtools)
       ("r-kohonen" ,r-kohonen)
       ("r-mass" ,r-mass)
       ("r-pls" ,r-pls)))
    (home-page "https://github.com/rwehrens/CWR")
    (synopsis "Chemometrics with R")
    (description
     "This package provides functions and scripts used in the book
\"Chemometrics with R - Multivariate Data Analysis in the Natural Sciences and
Life Sciences\" by Ron Wehrens, Springer (2011).")
    (license license:gpl2+)))

(define-public r-als
  (package
    (name "r-als")
    (version "0.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ALS" version))
       (sha256
        (base32
         "1swrn39vy50fazkpf97r7c542gkj6mlvy8gmcxllg7mf2mqx546a"))))
    (properties `((upstream-name . "ALS")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-iso" ,r-iso)
       ("r-nnls" ,r-nnls)))
    (home-page "https://cran.r-project.org/web/packages/ALS")
    (synopsis "Multivariate curve resolution alternating least squares")
    (description
     "Alternating least squares is often used to resolve components
contributing to data with a bilinear structure; the basic technique may be
extended to alternating constrained least squares.  This package provides an
implementation of @dfn{multivariate curve resolution alternating least
squares} (MCR-ALS).

Commonly applied constraints include unimodality, non-negativity, and
normalization of components.  Several data matrices may be decomposed
simultaneously by assuming that one of the two matrices in the bilinear
decomposition is shared between datasets.")
    (license license:gpl2+)))

(define-public r-strucchange
  (package
    (name "r-strucchange")
    (version "1.5-2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "strucchange" version))
       (sha256
        (base32
         "1y022363a4pp0mnji91sjh1qiyspkh09sybqwj03r9pmwrd7q93x"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-sandwich" ,r-sandwich)
       ("r-zoo" ,r-zoo)))
    (home-page "https://cran.r-project.org/web/packages/strucchange")
    (synopsis "Testing, monitoring, and dating structural changes")
    (description
     "This package provides tools for testing, monitoring and dating
structural changes in (linear) regression models.  It features tests/methods
from the generalized fluctuation test framework as well as from the F
test (Chow test) framework.  This includes methods to fit, plot and test
fluctuation processes (e.g., CUSUM, MOSUM, recursive/moving estimates) and F
statistics, respectively.  It is possible to monitor incoming data online
using fluctuation processes.  Finally, the breakpoints in regression models
with structural changes can be estimated together with confidence intervals.
Emphasis is always given to methods for visualizing the data.")
    ;; Either of these two GPL versions
    (license (list license:gpl2 license:gpl3))))

(define-public r-pixmap
  (package
    (name "r-pixmap")
    (version "0.4-11")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "pixmap" version))
       (sha256
        (base32
         "04klxp6jndw1bp6z40v20fbmdmdpfca2g0czmmmgbkark9s1183g"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/pixmap")
    (synopsis "Tools for bitmap images")
    (description
     "This package provides functions for importing, exporting, plotting and
other manipulations of bitmapped images.")
    (license license:gpl2)))

(define-public r-rapidjsonr
  (package
    (name "r-rapidjsonr")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rapidjsonr" version))
       (sha256
        (base32
         "07zdirhbzmvq3cp4xn8ngk1lgxbbabzays315zxbs3sxrz6lzjb2"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/rapidjsonr")
    (synopsis "JSON parser")
    (description
     "This package provides JSON parsing capability through the Rapidjson
library.")
    (license license:expat)))

(define-public r-ontologyindex
  (package
    (name "r-ontologyindex")
    (version "2.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ontologyIndex" version))
       (sha256
        (base32
         "127hlf0z5fmbgnq4p9h8nvn6p72d2fpcn846zzb99s213421jnry"))))
    (properties `((upstream-name . "ontologyIndex")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/ontologyIndex")
    (synopsis "Functions for processing ontologies in R")
    (description
     "This package provides functions for reading ontologies into R as lists
and manipulating sets of ontological terms.")
    (license license:gpl2+)))

(define-public r-gargle
  (package
    (name "r-gargle")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "gargle" version))
       (sha256
        (base32
         "08zhfk2sl342w35i5n2c93ayypg3z0kbl0020l3y9adqka1vazgx"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-fs" ,r-fs)
       ("r-glue" ,r-glue)
       ("r-httr" ,r-httr)
       ("r-jsonlite" ,r-jsonlite)
       ("r-rlang" ,r-rlang)
       ("r-withr" ,r-withr)))
    (home-page "https://gargle.r-lib.org")
    (synopsis "Utilities for working with Google APIs")
    (description
     "This package provides utilities for working with Google APIs.  This
includes functions and classes for handling common credential types and for
preparing, executing, and processing HTTP requests.")
    (license license:expat)))

(define-public r-bigrquery
  (package
    (name "r-bigrquery")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "bigrquery" version))
       (sha256
        (base32
         "1ggh2gngr5x0g6y7d55y6kvn94anf7qi1bkc28cjmw61hxjq38fb"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-assertthat" ,r-assertthat)
       ("r-bit64" ,r-bit64)
       ("r-curl" ,r-curl)
       ("r-dbi" ,r-dbi)
       ("r-gargle" ,r-gargle)
       ("r-glue" ,r-glue)
       ("r-httr" ,r-httr)
       ("r-jsonlite" ,r-jsonlite)
       ("r-prettyunits" ,r-prettyunits)
       ("r-progress" ,r-progress)
       ("r-rapidjsonr" ,r-rapidjsonr)
       ("r-rcpp" ,r-rcpp)
       ("r-rlang" ,r-rlang)
       ("r-tibble" ,r-tibble)))
    (home-page "https://github.com/rstats-db/bigrquery")
    (synopsis "R interface to Google's BigQuery API")
    (description
     "This package provides an R interface to Google's BigQuery database.")
    (license license:gpl3)))

(define-public r-gmp
  (package
    (name "r-gmp")
    (version "0.5-13.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "gmp" version))
       (sha256
        (base32
         "0j2sz2nw41y9306rl1b8hbn0spz7453z5iawcq0bvslyrhc1d9ir"))))
    (build-system r-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-CC
           (lambda _ (setenv "CC" "gcc") #t)))))
    (inputs `(("gmp" ,gmp)))
    (home-page "https://cran.r-project.org/web/packages/gmp")
    (synopsis "Multiple precision arithmetic")
    (description
     "This package supports multiple precision arithmetic (big integers and
rationals, prime number tests, matrix computation), \"arithmetic without
limitations\" using the GNU Multiple Precision library.")
    ;; Any version of the GPL.
    (license license:gpl3+)))

(define-public r-rmpfr
  (package
    (name "r-rmpfr")
    (version "0.8-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Rmpfr" version))
       (sha256
        (base32
         "09kw7hyca8xc09r2d88qj81cclar8acaq5q9q5rw9f49iffda0rr"))))
    (properties `((upstream-name . "Rmpfr")))
    (build-system r-build-system)
    (inputs
     `(("mpfr" ,mpfr)
       ("gmp" ,gmp)))
    (propagated-inputs
     `(("r-gmp" ,r-gmp)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://rmpfr.r-forge.r-project.org/")
    (synopsis "R bindings to the MPFR library")
    (description
     "This package supports arithmetic (via S4 classes and methods) for
arbitrary precision floating point numbers, including transcendental
functions.  To this end, the package interfaces with the @dfn{Multiple
Precision Floating-Point Reliable} (MPFR) library.")
    (license license:gpl2+)))

(define-public r-assertive-base
  (package
    (name "r-assertive-base")
    (version "0.0-7")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "assertive.base" version))
       (sha256
        (base32
         "1xs3ysvj0z57c58jw57pckq2rynia6ks4rmjmc02alczhk54wbgh"))))
    (properties
     `((upstream-name . "assertive.base")))
    (build-system r-build-system)
    (home-page "https://bitbucket.org/richierocks/assertive.base")
    (synopsis "Core of the assertive package")
    (description
     "This package provides a minimal set of predicates and assertions used by
the assertive package.  This is mainly for use by other package developers who
want to include run-time testing features in their own packages.")
    (license license:gpl3+)))

(define-public r-assertive-properties
  (package
    (name "r-assertive-properties")
    (version "0.0-4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "assertive.properties" version))
       (sha256
        (base32
         "0sqs54acs9qk9kvm32rxzfbzxz1l8mjahpfnw7r30z2brgz661jw"))))
    (properties
     `((upstream-name . "assertive.properties")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-assertive-base" ,r-assertive-base)))
    (home-page "https://bitbucket.org/richierocks/assertive.properties")
    (synopsis "Assertions to check properties of variables")
    (description
     "This package provides a set of predicates and assertions for checking
the properties of variables, such as length, names and attributes.  This is
mainly for use by other package developers who want to include run-time
testing features in their own packages.")
    (license license:gpl3+)))

(define-public r-assertive-numbers
  (package
    (name "r-assertive-numbers")
    (version "0.0-2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "assertive.numbers" version))
       (sha256
        (base32
         "0jc3ss64j4m7bjydhagwwmka5n7c72vpw4kfcch0m5jvkq5qrqds"))))
    (properties
     `((upstream-name . "assertive.numbers")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-assertive-base" ,r-assertive-base)))
    (home-page "https://bitbucket.org/richierocks/assertive.numbers")
    (synopsis "Assertions to check properties of numbers")
    (description
     "This package provides a set of predicates and assertions for checking
the properties of numbers.  This is mainly for use by other package developers
who want to include run-time testing features in their own packages.")
    (license license:gpl3+)))

(define-public r-assertive-sets
  (package
    (name "r-assertive-sets")
    (version "0.0-3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "assertive.sets" version))
       (sha256
        (base32
         "1cqvh2syvh5b6d85h601zjmsdbbf3h8q98ids4dfl4frdshpasc7"))))
    (properties
     `((upstream-name . "assertive.sets")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-assertive-base" ,r-assertive-base)))
    (home-page "https://bitbucket.org/richierocks/assertive.sets")
    (synopsis "Assertions to check properties of sets")
    (description
     "This package provides a set of predicates and assertions for checking
the properties of sets.  This is mainly for use by other package developers
who want to include run-time testing features in their own packages.")
    (license license:gpl3+)))

(define-public r-assertive-matrices
  (package
    (name "r-assertive-matrices")
    (version "0.0-2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "assertive.matrices" version))
       (sha256
        (base32
         "16sykzcndv6y2d43x6v9n7m95kv76364h39kh10w4z0xw6ksfqil"))))
    (properties
     `((upstream-name . "assertive.matrices")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-assertive-base" ,r-assertive-base)))
    (home-page "https://bitbucket.org/richierocks/assertive.matrices")
    (synopsis "Assertions to check properties of matrices")
    (description
     "This package provides a set of predicates and assertions for checking
the properties of matrices.  This is mainly for use by other package
developers who want to include run-time testing features in their own
packages.")
    (license license:gpl3+)))

(define-public r-assertive-models
  (package
    (name "r-assertive-models")
    (version "0.0-2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "assertive.models" version))
       (sha256
        (base32
         "0bn4j4v5qvb2d672cgri61p8d9v258pmz35y3lvm6b9mdxwdi9mr"))))
    (properties
     `((upstream-name . "assertive.models")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-assertive-base" ,r-assertive-base)))
    (home-page "https://bitbucket.org/richierocks/assertive.models")
    (synopsis "Assertions to check properties of models")
    (description
     "This package provides a set of predicates and assertions for checking
the properties of models.  This is mainly for use by other package developers
who want to include run-time testing features in their own packages.")
    (license license:gpl3+)))

(define-public r-assertive-reflection
  (package
    (name "r-assertive-reflection")
    (version "0.0-4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "assertive.reflection" version))
       (sha256
        (base32
         "19zmsbn00crfqm0kwd9ys5gv87xs3gi6wmlikrz9xiwzm7hp4dhj"))))
    (properties
     `((upstream-name . "assertive.reflection")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-assertive-base" ,r-assertive-base)))
    (home-page "https://bitbucket.org/richierocks/assertive.reflection")
    (synopsis "Assertions for checking the state of R")
    (description
     "This package provides a set of predicates and assertions for checking
the state and capabilities of R, the operating system it is running on, and
the IDE being used.  This is mainly for use by other package developers who
want to include run-time testing features in their own packages.")
    (license license:gpl3+)))

(define-public r-assertive-types
  (package
    (name "r-assertive-types")
    (version "0.0-3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "assertive.types" version))
       (sha256
        (base32
         "0zxq1jfrzgw95ll7alvm0xnk7aihjdksngq4ya2whyvfjbmv4vdb"))))
    (properties
     `((upstream-name . "assertive.types")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-assertive-base" ,r-assertive-base)
       ("r-assertive-properties" ,r-assertive-properties)
       ("r-codetools" ,r-codetools)))
    (home-page "https://bitbucket.org/richierocks/assertive.types")
    (synopsis "Assertions to check types of variables")
    (description
     "This package provides a set of predicates and assertions for checking
the types of variables.  This is mainly for use by other package developers
who want to include run-time testing features in their own packages.")
    (license license:gpl3+)))

(define-public r-assertive-files
  (package
    (name "r-assertive-files")
    (version "0.0-2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "assertive.files" version))
       (sha256
        (base32
         "02pfz8j5vwcj5kl6zca46894li7lxwnlrr29j922f14ay6kdssmy"))))
    (properties
     `((upstream-name . "assertive.files")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-assertive-base" ,r-assertive-base)
       ("r-assertive-numbers" ,r-assertive-numbers)))
    (home-page "https://bitbucket.org/richierocks/assertive.files")
    (synopsis "Assertions to check properties of files")
    (description
     "This package provides a set of predicates and assertions for checking
the properties of files and connections.  This is mainly for use by other
package developers who want to include run-time testing features in their own
packages.")
    (license license:gpl3+)))

(define-public r-assertive-code
  (package
    (name "r-assertive-code")
    (version "0.0-3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "assertive.code" version))
       (sha256
        (base32
         "1qhbp668zfvhqs8avkhg9amp4zyazz6dsy4fc6kpdmw3sv8yi07g"))))
    (properties
     `((upstream-name . "assertive.code")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-assertive-base" ,r-assertive-base)
       ("r-assertive-properties" ,r-assertive-properties)
       ("r-assertive-types" ,r-assertive-types)))
    (home-page "https://bitbucket.org/richierocks/assertive.code")
    (synopsis "Assertions to check properties of code")
    (description
     "This package provides a set of predicates and assertions for checking
the properties of code.  This is mainly for use by other package developers
who want to include run-time testing features in their own packages.")
    (license license:gpl3+)))

(define-public r-assertive-datetimes
  (package
    (name "r-assertive-datetimes")
    (version "0.0-2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "assertive.datetimes" version))
       (sha256
        (base32
         "00a98fx8p3pr3ckayh8wmxmm4rz01s67wah9697m92yci6pv3m78"))))
    (properties
     `((upstream-name . "assertive.datetimes")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-assertive-base" ,r-assertive-base)
       ("r-assertive-types" ,r-assertive-types)))
    (home-page "https://bitbucket.org/richierocks/assertive.datetimes")
    (synopsis "Assertions to check properties of dates and times")
    (description
     "This package provides a set of predicates and assertions for checking
the properties of dates and times.  This is mainly for use by other package
developers who want to include run-time testing features in their own
packages.")
    (license license:gpl3+)))

(define-public r-assertive-strings
  (package
    (name "r-assertive-strings")
    (version "0.0-3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "assertive.strings" version))
       (sha256
        (base32
         "0n6jrk88670g4ym0r8ii40a08a90z1xadj8wcryk8h0nl04dchfm"))))
    (properties
     `((upstream-name . "assertive.strings")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-assertive-base" ,r-assertive-base)
       ("r-assertive-types" ,r-assertive-types)
       ("r-stringi" ,r-stringi)))
    (home-page "https://bitbucket.org/richierocks/assertive.strings")
    (synopsis "Assertions to check properties of strings")
    (description
     "This package provides a set of predicates and assertions for checking
the properties of strings.  This is mainly for use by other package developers
who want to include run-time testing features in their own packages.")
    (license license:gpl3+)))

(define-public r-assertive-data-us
  (package
    (name "r-assertive-data-us")
    (version "0.0-2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "assertive.data.us" version))
       (sha256
        (base32
         "1bgspn0sccmp9z7s7djvdvprgxlyc5vrxznp4zfjb79kwvgn83hq"))))
    (properties
     `((upstream-name . "assertive.data.us")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-assertive-base" ,r-assertive-base)
       ("r-assertive-strings" ,r-assertive-strings)))
    (home-page "https://bitbucket.org/richierocks/assertive.data.us")
    (synopsis "Assertions to check properties of strings")
    (description
     "This package provides a set of predicates and assertions for checking
the properties of US-specific complex data types.  This is mainly for use by
other package developers who want to include run-time testing features in
their own packages.")
    (license license:gpl3+)))

(define-public r-assertive-data-uk
  (package
    (name "r-assertive-data-uk")
    (version "0.0-2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "assertive.data.uk" version))
       (sha256
        (base32
         "1fzjvhwp7mwkqqix29khvs6zcrc82n6j4czvzzb473vyjyvdlj5b"))))
    (properties
     `((upstream-name . "assertive.data.uk")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-assertive-base" ,r-assertive-base)
       ("r-assertive-strings" ,r-assertive-strings)))
    (home-page "https://bitbucket.org/richierocks/assertive.data.uk")
    (synopsis "Assertions to check properties of strings")
    (description
     "This package provides a set of predicates and assertions for checking
the properties of UK-specific complex data types.  This is mainly for use by
other package developers who want to include run-time testing features in
their own packages.")
    (license license:gpl3+)))

(define-public r-assertive-data
  (package
    (name "r-assertive-data")
    (version "0.0-3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "assertive.data" version))
       (sha256
        (base32
         "00cvg2g36mdl8plrzx40m63qd55742mddqrchwy9n3c7mm4gn02s"))))
    (properties
     `((upstream-name . "assertive.data")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-assertive-base" ,r-assertive-base)
       ("r-assertive-strings" ,r-assertive-strings)))
    (home-page "https://bitbucket.org/richierocks/assertive.data")
    (synopsis "Assertions to check properties of data")
    (description
     "This package provides a set of predicates and assertions for checking
the properties of (country independent) complex data types.  This is mainly
for use by other package developers who want to include run-time testing
features in their own packages.")
    (license license:gpl3+)))

(define-public r-assertive
  (package
    (name "r-assertive")
    (version "0.3-5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "assertive" version))
       (sha256
        (base32
         "0blbbhlxcb5ffdxqxi62xs33ljiawh6s22a0pyvbbh79jf46rzr3"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-assertive-base" ,r-assertive-base)
       ("r-assertive-code" ,r-assertive-code)
       ("r-assertive-data" ,r-assertive-data)
       ("r-assertive-data-uk" ,r-assertive-data-uk)
       ("r-assertive-data-us" ,r-assertive-data-us)
       ("r-assertive-datetimes" ,r-assertive-datetimes)
       ("r-assertive-files" ,r-assertive-files)
       ("r-assertive-matrices" ,r-assertive-matrices)
       ("r-assertive-models" ,r-assertive-models)
       ("r-assertive-numbers" ,r-assertive-numbers)
       ("r-assertive-properties" ,r-assertive-properties)
       ("r-assertive-reflection" ,r-assertive-reflection)
       ("r-assertive-sets" ,r-assertive-sets)
       ("r-assertive-strings" ,r-assertive-strings)
       ("r-assertive-types" ,r-assertive-types)
       ("r-knitr" ,r-knitr)))
    (home-page "https://bitbucket.org/richierocks/assertive")
    (synopsis "Readable check functions to ensure code integrity")
    (description
     "This package provides lots of predicates (@code{is_*} functions) to
check the state of your variables, and assertions (@code{assert_*} functions)
to throw errors if they aren't in the right form.")
    (license license:gpl3+)))

(define-public r-dotcall64
  (package
    (name "r-dotcall64")
    (version "1.0-0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "dotCall64" version))
       (sha256
        (base32
         "1b8p7m3w0m7bp977c6jz74xkd611cxg11j49yza59k5fp338scb9"))))
    (properties `((upstream-name . "dotCall64")))
    (build-system r-build-system)
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "https://git.math.uzh.ch/reinhard.furrer/dotCall64")
    (synopsis "Enhanced foreign function interface supporting long vectors")
    (description
     "This package provides @code{.C64()}, an enhanced version of @code{.C()}
and @code{.Fortran()} from the R foreign function interface.  @code{.C64()}
supports long vectors, arguments of type 64-bit integer, and provides a
mechanism to avoid unnecessary copies of read-only and write-only arguments.
This makes it a convenient and fast interface to C/C++ and Fortran code.")
    (license license:gpl2+)))

(define-public r-spam
  (package
    (name "r-spam")
    (version "2.5-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "spam" version))
       (sha256
        (base32 "0ry0a76cljlmilrzcriiizcidxyhq1i7i9bqhvl1qda81ld8hifi"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-dotcall64" ,r-dotcall64)))
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "https://www.math.uzh.ch/pages/spam/")
    (synopsis "Sparse matrix algebra")
    (description
     "This package provides a set of functions for sparse matrix algebra.
Differences with other sparse matrix packages are:

@enumerate
@item it only supports (essentially) one sparse matrix format;
@item it is based on transparent and simple structure(s);
@item it is tailored for MCMC calculations within G(M)RF;
@item and it is fast and scalable (with the extension package @code{spam64}).
@end enumerate\n")
    ;; Either of these licenses
    (license (list license:bsd-3 license:lgpl2.0))))

(define-public r-fields
  (package
    (name "r-fields")
    (version "10.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "fields" version))
       (sha256
        (base32 "12k97vfjlz5h8vynirnvik1nyj1iw25n8xl7awmx9mpd6wvgy2s9"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-maps" ,r-maps)
       ("r-spam" ,r-spam)))
    (native-inputs
     `(("gfortran" ,gfortran)))
    (home-page "https://www.image.ucar.edu/fields")
    (synopsis "Tools for spatial data")
    (description
     "This is a package for curve, surface and function fitting with an
emphasis on splines, spatial data and spatial statistics.  The major methods
include cubic, and thin plate splines, Kriging, and compactly supported
covariance functions for large data sets.")
    (license license:gpl2+)))

(define-public r-spatialextremes
  (package
    (name "r-spatialextremes")
    (version "2.0-8")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "SpatialExtremes" version))
       (sha256
        (base32
         "0r2byz5xxc46zqnigdax28q7446ibmzmsmi10lmm2hdks3ml6sl3"))))
    (properties
     `((upstream-name . "SpatialExtremes")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-fields" ,r-fields)
       ("r-maps" ,r-maps)))
    (home-page "http://spatialextremes.r-forge.r-project.org/")
    (synopsis "Modelling spatial extremes")
    (description
     "This package provides tools for the statistical modelling of spatial
extremes using max-stable processes, copula or Bayesian hierarchical models.
More precisely, this package allows (conditional) simulations from various
parametric max-stable models, analysis of the extremal spatial dependence, the
fitting of such processes using composite likelihoods or least square (simple
max-stable processes only), model checking and selection and prediction.")
    (license license:gpl2+)))

(define-public r-drc
  (package
    (name "r-drc")
    (version "3.0-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "drc" version))
       (sha256
        (base32
         "0c8xn8ripzq270hy8d16fcnx02l02alddznd7fqwk3jyi6113h1y"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-car" ,r-car)
       ("r-gtools" ,r-gtools)
       ("r-mass" ,r-mass)
       ("r-multcomp" ,r-multcomp)
       ("r-plotrix" ,r-plotrix)
       ("r-scales" ,r-scales)))
    (home-page "https://cran.r-project.org/web/packages/drc")
    (synopsis "Analysis of dose-response curves")
    (description
     "This package provides a suite of flexible and versatile model fitting
and after-fitting functions for the analysis of dose-response data.")
    (license license:gpl2+)))

(define-public r-rmeta
  (package
    (name "r-rmeta")
    (version "3.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rmeta" version))
       (sha256
        (base32
         "0vkbnxp579v8zmcv1isdbzj5swpr6fq17zwparxcvzswjc2x9ydr"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/rmeta")
    (synopsis "Tools for meta-analysis")
    (description
     "This package provides functions for simple fixed and random effects
meta-analysis for two-sample comparisons and cumulative meta-analyses.  It
draws standard summary plots, funnel plots, and computes summaries and tests
for association and heterogeneity.")
    (license license:gpl2)))

(define-public r-bootstrap
  (package
    (name "r-bootstrap")
    (version "2019.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "bootstrap" version))
       (sha256
        (base32
         "1546jqhhw5h177ii8jkdikyd26rv6gwkav816np1zks4p7zgsljj"))))
    (build-system r-build-system)
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/bootstrap")
    (synopsis "Functions for the book \"An Introduction to the Bootstrap\"")
    (description
     "This package provides software and data for the book \"An Introduction
to the Bootstrap\" by B. Efron and R. Tibshirani, 1993, Chapman and Hall.
This package is primarily provided for projects already based on it, and for
support of the book.  New projects should preferentially use the recommended
package \"boot\".")
    (license license:bsd-3)))

(define-public r-survivalroc
  (package
    (name "r-survivalroc")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "survivalROC" version))
       (sha256
        (base32
         "0wnd65ff5w679hxa1zrpfrx9qg47q21pjxppsga6m3h4iq1yfj8l"))))
    (properties `((upstream-name . "survivalROC")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/survivalROC")
    (synopsis "Time-dependent ROC curve estimation from censored survival data")
    (description
     "Compute time-dependent ROC curve from censored survival data using
Kaplan-Meier (KM) or Nearest Neighbor Estimation (NNE) method of Heagerty,
Lumley & Pepe (Biometrics, Vol 56 No 2, 2000, PP 337-344)")
    (license license:gpl2+)))

(define-public r-longitudinal
  (package
    (name "r-longitudinal")
    (version "1.1.12")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "longitudinal" version))
       (sha256
        (base32
         "1d83ws28nxi3kw5lgd5n5y7865djq7ky72fw3ddi1fkkhg1r9y6l"))))
    (build-system r-build-system)
    (propagated-inputs `(("r-corpcor" ,r-corpcor)))
    (home-page "http://strimmerlab.org/software/longitudinal/")
    (synopsis "Analysis of multiple time course data")
    (description
     "This package contains general data structures and functions for
longitudinal data with multiple variables, repeated measurements, and
irregularly spaced time points.  It also implements a shrinkage estimator of
dynamical correlation and dynamical covariance.")
    (license license:gpl3+)))

(define-public r-genenet
  (package
    (name "r-genenet")
    (version "1.2.14")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "GeneNet" version))
       (sha256
        (base32
         "0cdhrj15rz0w0pyw3r8mikrzsdh95y5i1c0pa3cn0c2bjnjx3x3n"))))
    (properties `((upstream-name . "GeneNet")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-corpcor" ,r-corpcor)
       ("r-fdrtool" ,r-fdrtool)
       ("r-longitudinal" ,r-longitudinal)))
    (home-page "http://strimmerlab.org/software/genenet/")
    (synopsis "Modeling and inferring gene networks")
    (description
     "This package analyzes gene expression (time series) data with focus on
the inference of gene networks.  In particular, GeneNet implements the methods
of Schaefer and Strimmer (2005a,b,c) and Opgen-Rhein and Strimmer (2006, 2007)
for learning large-scale gene association networks (including assignment of
putative directions).")
    (license license:gpl3+)))

(define-public r-rbamtools
  (package
    (name "r-rbamtools")
    (version "2.16.17")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rbamtools" version))
       (sha256
        (base32
         "0qj37ljdva3v29s01dkrbg31mcfzy3bl145cp40d54v4h9xhcghc"))))
    (build-system r-build-system)
    (inputs `(("zlib" ,zlib)))
    (propagated-inputs
     `(("r-refgenome" ,r-refgenome)))
    (home-page "https://cran.r-project.org/web/packages/rbamtools")
    (synopsis "Read and write BAM (binary alignment) files")
    (description
     "This package provides an R interface to functions of the SAMtools
library.")
    (license license:artistic2.0)))

(define-public r-protviz
  (package
    (name "r-protviz")
    (version "0.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "protViz" version))
       (sha256
        (base32
         "1ldciqh3f43xr9663yyhd9r6qwrg4c4vmkprlcancbnd460wakg7"))))
    (properties `((upstream-name . "protViz")))
    (build-system r-build-system)
    (propagated-inputs `(("r-rcpp" ,r-rcpp)))
    (home-page "https://github.com/protViz/protViz/")
    (synopsis "Visualizing and analyzing mass spectrometry data in proteomics")
    (description
     "This package helps with quality checks, visualizations and analysis of
mass spectrometry data, coming from proteomics experiments.  The package is
developed, tested and used at the Functional Genomics Center Zurich, where it
is used mainly for prototyping, teaching, and having fun with proteomics data.
But it can also be used to do data analysis for small scale data sets.")
    (license license:gpl3)))

(define-public r-cmprsk
  (package
    (name "r-cmprsk")
    (version "2.2-9")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "cmprsk" version))
       (sha256
        (base32 "0xhgfg5b4i9skkaxp7gzkafgg5bqs5q1rp4hpw2jjmykg2nifn99"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-survival" ,r-survival)))
    (native-inputs
     `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/cmprsk")
    (synopsis "Subdistribution analysis of competing risks")
    (description
     "This package provides tool for estimation, testing and regression
modeling of subdistribution functions in competing risks, as described in
Gray (1988), A class of K-sample tests for comparing the cumulative incidence
of a competing risk, Ann. Stat. 16:1141-1154, and Fine JP and Gray RJ (1999),
A proportional hazards model for the subdistribution of a competing risk,
JASA, 94:496-509.")
    (license license:gpl2+)))

(define-public r-etm
  (package
    (name "r-etm")
    (version "1.0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "etm" version))
       (sha256
        (base32
         "0m41pm277sd50pharigcqzr1a2g92wnmdf6fcab6fx16ia2fzrm7"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-data-table" ,r-data-table)
       ("r-lattice" ,r-lattice)
       ("r-rcpp" ,r-rcpp)
       ("r-rcpparmadillo" ,r-rcpparmadillo)
       ("r-survival" ,r-survival)))
    (home-page "https://cran.r-project.org/web/packages/etm")
    (synopsis "Empirical transition matrix")
    (description
     "The @dfn{empirical transition matrix} (etm) package permits to estimate
the matrix of transition probabilities for any time-inhomogeneous multistate
model with finite state space using the Aalen-Johansen estimator.")
    (license license:expat)))

(define-public r-epi
  (package
    (name "r-epi")
    (version "2.40")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Epi" version))
       (sha256
        (base32
         "046y10vwks5y84pzccmrn6d4pd6qz70imvp1hw5ywp8fnwzfh4g5"))))
    (properties `((upstream-name . "Epi")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-cmprsk" ,r-cmprsk)
       ("r-data-table" ,r-data-table)
       ("r-etm" ,r-etm)
       ("r-mass" ,r-mass)
       ("r-matrix" ,r-matrix)
       ("r-mgcv" ,r-mgcv)
       ("r-numderiv" ,r-numderiv)
       ("r-plyr" ,r-plyr)
       ("r-survival" ,r-survival)
       ("r-zoo" ,r-zoo)))
    (home-page "https://BendixCarstensen.com/Epi/")
    (synopsis "Statistical analysis in epidemiology")
    (description
     "This package provides functions for demographic and epidemiological
analysis in the Lexis diagram, i.e. register and cohort follow-up data, in
particular representation, manipulation and simulation of multistate data -
the Lexis suite of functions, which includes interfaces to the @code{mstate},
@code{etm} and @code{cmprsk} packages.  It also contains functions for
Age-Period-Cohort and Lee-Carter modeling and a function for interval censored
data and some useful functions for tabulation and plotting, as well as a
number of epidemiological data sets.")
    (license license:gpl2)))

(define-public r-ppls
  (package
    (name "r-ppls")
    (version "1.6-1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ppls" version))
       (sha256
        (base32
         "1zyrisy3c4cz896j1bjh61sf57wdl9p8ywdq268cl819szfq78mx"))))
    (build-system r-build-system)
    (propagated-inputs `(("r-mass" ,r-mass)))
    (home-page "https://cran.r-project.org/web/packages/ppls")
    (synopsis "Penalized partial least squares")
    (description
     "This package contains linear and nonlinear regression methods based on
partial least squares and penalization techniques.  Model parameters are
selected via cross-validation, and confidence intervals ans tests for the
regression coefficients can be conducted via jackknifing.")
    (license license:gpl2+)))

(define-public r-huge
  (package
    (name "r-huge")
    (version "1.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "huge" version))
       (sha256
        (base32 "07n3j1va2z4v30rj22cww72khgzbz2xsp0yc0qswlrwyxi4my5i3"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-igraph" ,r-igraph)
       ("r-mass" ,r-mass)
       ("r-matrix" ,r-matrix)
       ("r-rcpp" ,r-rcpp)
       ("r-rcppeigen" ,r-rcppeigen)))
    (home-page "https://cran.r-project.org/web/packages/huge")
    (synopsis "High-dimensional undirected graph estimation")
    (description
     "This package provides a general framework for high-dimensional
undirected graph estimation.  It integrates data preprocessing, neighborhood
screening, graph estimation, and model selection techniques into a pipeline.")
    (license license:gpl2)))

(define-public r-parcor
  (package
    (name "r-parcor")
    (version "0.2-6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "parcor" version))
       (sha256
        (base32
         "0vgs6k92vdr0cmb8cwbv2ff6qavw30agskfd8bfh17hsskrisvx0"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-epi" ,r-epi)
       ("r-genenet" ,r-genenet)
       ("r-glmnet" ,r-glmnet)
       ("r-mass" ,r-mass)
       ("r-ppls" ,r-ppls)))
    (home-page "https://cran.r-project.org/web/packages/parcor")
    (synopsis "Regularized estimation of partial correlation matrices")
    (description
     "This package estimates the matrix of partial correlations based on
different regularized regression methods: lasso, adaptive lasso, PLS, and
Ridge Regression.  In addition, the package provides model selection for
lasso, adaptive lasso and Ridge regression based on cross-validation.")
    (license license:gpl2+)))

(define-public r-mcmc
  (package
    (name "r-mcmc")
    (version "0.9-6.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "mcmc" version))
       (sha256
        (base32
         "1i1nhdapyijvm58zx38q28zk01ndmi6smjivxk5xs2cx9b6v2av9"))))
    (build-system r-build-system)
    (home-page "https://www.stat.umn.edu/geyer/mcmc/")
    (synopsis "Markov chain Monte Carlo")
    (description
     "This package simulates continuous distributions of random vectors using
@dfn{Markov chain Monte Carlo} (MCMC).  Users specify the distribution by an R
function that evaluates the log unnormalized density.  Algorithms are random
walk Metropolis algorithm (function @code{metrop}), simulated
tempering (function @code{temper}), and morphometric random walk
Metropolis (function @code{morph.metrop}), which achieves geometric ergodicity
by change of variable.")
    (license license:expat)))

(define-public r-listenv
  (package
    (name "r-listenv")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "listenv" version))
       (sha256
        (base32
         "0ps8bk7zlhbviawrw7vw25skjq81hkk3ijyi6g74dmfqy8zsyapx"))))
    (build-system r-build-system)
    (native-inputs
     `(("r-r-rsp" ,r-r-rsp))) ; vignette builder
    (home-page "https://github.com/HenrikBengtsson/listenv")
    (synopsis "Environments behaving (almost) as lists")
    (description
     "This package implements list environments.  List environments are
environments that have list-like properties.  For instance, the elements of a
list environment are ordered and can be accessed and iterated over using index
subsetting.")
    (license license:lgpl2.1+)))

(define-public r-globals
  (package
    (name "r-globals")
    (version "0.12.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "globals" version))
       (sha256
        (base32
         "1ha8iasgijp4q3v2b0b17y1wh7cd3nvzd9b03w49qm2bidkaf68m"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-codetools" ,r-codetools)))
    (home-page "https://github.com/HenrikBengtsson/globals")
    (synopsis "Identify global objects in R expressions")
    (description
     "This package provides tools to identify global (\"unknown\" or \"free\")
objects in R expressions by code inspection using various strategies, e.g.
conservative or liberal.  The objective of this package is to make it as
simple as possible to identify global objects for the purpose of exporting
them in distributed compute environments.")
    (license license:lgpl2.1+)))

(define-public r-future
  (package
    (name "r-future")
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "future" version))
       (sha256
        (base32
         "1xaqh0b2knf5bp23mc0kriq0iqhqna31q3b7d960piqjhzrb03dm"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-digest" ,r-digest)
       ("r-globals" ,r-globals)
       ("r-listenv" ,r-listenv)))
    (native-inputs
     `(("r-r-rsp" ,r-r-rsp))) ; vignette builder
    (home-page "https://github.com/HenrikBengtsson/future")
    (synopsis "Unified parallel and distributed processing in R")
    (description
     "The purpose of this package is to provide a lightweight and unified
Future API for sequential and parallel processing of R expression via futures.
This package implements sequential, multicore, multisession, and cluster
futures.  With these, R expressions can be evaluated on the local machine, in
parallel a set of local machines, or distributed on a mix of local and remote
machines.  Extensions to this package implement additional backends for
processing futures via compute cluster schedulers etc.  Because of its unified
API, there is no need to modify any code in order to switch from sequential on
the local machine to, say, distributed processing on a remote compute cluster.")
    (license license:lgpl2.1+)))

(define-public r-future-apply
  (package
    (name "r-future-apply")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "future.apply" version))
       (sha256
        (base32
         "1kgq6dv96hdy35kysqkn606nj7s9dp4ibgpm6n46gqhc5n75lzkk"))))
    (properties `((upstream-name . "future.apply")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-future" ,r-future)
       ("r-globals" ,r-globals)))
    (native-inputs
     `(("r-r-rsp" ,r-r-rsp))) ; vignette builder
    (home-page "https://github.com/HenrikBengtsson/future.apply")
    (synopsis "Apply function to elements in parallel using futures")
    (description
     "This package provides implementations of @code{apply()},
@code{eapply()}, @code{lapply()}, @code{Map()}, @code{mapply()},
@code{replicate()}, @code{sapply()}, @code{tapply()}, and @code{vapply()} that
can be resolved using any future-supported backend, e.g. parallel on the local
machine or distributed on a compute cluster.")
    (license license:gpl2+)))

(define-public r-rsvd
  (package
    (name "r-rsvd")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rsvd" version))
       (sha256
        (base32
         "1fvrw46fl5xb2akaa4mp8nja4h7nn4bdhnjdrk22fsdfqc7hwmhk"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-matrix" ,r-matrix)))
    (home-page "https://github.com/erichson/rSVD")
    (synopsis "Randomized singular value decomposition")
    (description
     "Low-rank matrix decompositions are fundamental tools and widely used for
data analysis, dimension reduction, and data compression.  Classically, highly
accurate deterministic matrix algorithms are used for this task.  However, the
emergence of large-scale data has severely challenged our computational
ability to analyze big data.  The concept of randomness has been demonstrated
as an effective strategy to quickly produce approximate answers to familiar
problems such as the @dfn{singular value decomposition} (SVD).  This package
provides several randomized matrix algorithms such as the randomized singular
value decomposition (@code{rsvd}), randomized principal component
analysis (@code{rpca}), randomized robust principal component
analysis (@code{rrpca}), randomized interpolative decomposition (@code{rid}),
and the randomized CUR decomposition (@code{rcur}).  In addition several plot
functions are provided.")
    (license license:gpl3+)))

(define-public r-sloop
  (package
    (name "r-sloop")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "sloop" version))
       (sha256
        (base32
         "00fk5fr5zsk2qxc1kfhmshhjxgnamm3401089sx8m2l529zd6r8j"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-codetools" ,r-codetools)
       ("r-crayon" ,r-crayon)
       ("r-purrr" ,r-purrr)
       ("r-rlang" ,r-rlang)
       ("r-tibble" ,r-tibble)))
    (home-page "https://github.com/r-lib/sloop")
    (synopsis "Helpers for object-oriented programming in R")
    (description
     "This package provides a collection of helper functions designed to
help you to better understand object oriented programming in R, particularly
using @code{S3}.")
    (license license:gpl3)))

(define-public r-capushe
  (package
    (name "r-capushe")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "capushe" version))
       (sha256
        (base32
         "1aa76ir1kp67hiz7dr60azyc71yzslshyc640fjh0fpw0sp5kwbc"))))
    (build-system r-build-system)
    (propagated-inputs `(("r-mass" ,r-mass)))
    (home-page "https://cran.r-project.org/web/packages/capushe/index.html")
    (synopsis "Calibrating penalties using slope heuristics")
    (description
     "This package provides tools for the calibration of penalized criteria
for model selection.  The calibration methods available are based on the slope
heuristics.")
    (license license:gpl2+)))

(define-public r-dorng
  (package
    (name "r-dorng")
    (version "1.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "doRNG" version))
       (sha256
        (base32
         "1jff27zzrvd1fd61x2m9468h8xn3s1c9f6wibviy5zdhj5dx9s9k"))))
    (properties `((upstream-name . "doRNG")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-foreach" ,r-foreach)
       ("r-iterators" ,r-iterators)
       ("r-rngtools" ,r-rngtools)))
    (home-page "https://renozao.github.io/doRNG/")
    (synopsis "Generic reproducible parallel backend for foreach loops")
    (description
     "This package provides functions to perform reproducible parallel
@code{foreach} loops, using independent random streams as generated by
L'Ecuyer's combined multiple-recursive generator.  It enables to easily
convert standard @code{%dopar%} loops into fully reproducible loops,
independently of the number of workers, the task scheduling strategy, or the
chosen parallel environment and associated foreach backend.")
    (license license:gpl2+)))

(define-public r-blockmodeling
  (package
    (name "r-blockmodeling")
    (version "0.3.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "blockmodeling" version))
       (sha256
        (base32
         "12paf76l3wlxad14bkxn37lw9rg6ka473m86wlcf3yhriw8kbaiz"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-matrix" ,r-matrix)))
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/blockmodeling")
    (synopsis "Generalized and classical blockmodeling of valued networks")
    (description
     "This package is primarily meant as an implementation of generalized
blockmodeling for valued networks.  In addition, measures of similarity or
dissimilarity based on structural equivalence and regular equivalence (REGE
algorithms) can be computed and partitioned matrices can be plotted.")
    (license license:gpl2+)))

(define-public r-upsetr
  (package
    (name "r-upsetr")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "UpSetR" version))
       (sha256
        (base32
         "007i0njnjjy7vbrxabwav7a1kk2n0hn2mkvqsdzzfk10ckp5y7im"))))
    (properties `((upstream-name . "UpSetR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-gridextra" ,r-gridextra)
       ("r-plyr" ,r-plyr)
       ("r-scales" ,r-scales)))
    (home-page "https://github.com/hms-dbmi/UpSetR")
    (synopsis "Visualize intersecting sets")
    (description
     "This package provides a more scalable alternative to Venn and Euler
diagrams for visualizing intersecting sets.  Create visualizations of
intersecting sets using a novel matrix design, along with visualizations of
several common set, element and attribute related tasks.")
    (license license:expat)))

;; This package includes a JavaScript file, which is not minified.  When
;; upgrading please check that there are no new minified JavaScript files.
(define-public r-shinybs
  (package
    (name "r-shinybs")
    (version "0.61")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "shinyBS" version))
       (sha256
        (base32
         "0rhim4mbp4x9vvm7xkmpl7mhb9qd1gr96cr4dv330v863ra2kgji"))))
    (properties `((upstream-name . "shinyBS")))
    (build-system r-build-system)
    ;; The tests spawn Shiny browser apps.  They cannot be run
    ;; non-interactively.
    (arguments '(#:tests? #f))
    (propagated-inputs
     `(("r-htmltools" ,r-htmltools)
       ("r-shiny" ,r-shiny)))
    (home-page "https://ebailey78.github.io/shinyBS/")
    (synopsis "Twitter Bootstrap components for Shiny")
    (description
     "This package adds additional Twitter Bootstrap components to Shiny.")
    (license license:gpl3)))

(define-public r-outliers
  (package
    (name "r-outliers")
    (version "0.14")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "outliers" version))
       (sha256
        (base32
         "0vcqfqmmv4yblyp3s6bd25r49pxb7hjzipiic5a82924nqfqzkmn"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/outliers/index.html")
    (synopsis "Tests for outliers")
    (description
     "This package provides a collection of some tests commonly used for
identifying outliers.")
    (license license:gpl2+)))

(define-public r-bayesm
  (package
    (name "r-bayesm")
    (version "3.1-4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "bayesm" version))
       (sha256
        (base32 "154glks7rsjkza0sfi1kj7wj727py9sl1ba6sswflwmwc9n226q6"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rcpp" ,r-rcpp)
       ("r-rcpparmadillo" ,r-rcpparmadillo)))
    (home-page "http://www.perossi.org/home/bsm-1")
    (synopsis "Bayesian inference for marketing/micro-econometrics")
    (description
     "This package covers many important models used in marketing and
micro-econometrics applications, including Bayes Regression (univariate or
multivariate dep var), Bayes Seemingly Unrelated Regression (SUR), Binary and
Ordinal Probit, Multinomial Logit (MNL) and Multinomial Probit (MNP),
Multivariate Probit, Negative Binomial (Poisson) Regression, Multivariate
Mixtures of Normals (including clustering), Dirichlet Process Prior Density
Estimation with normal base, Hierarchical Linear Models with normal prior and
covariates, Hierarchical Linear Models with a mixture of normals prior and
covariates, Hierarchical Multinomial Logits with a mixture of normals prior
and covariates, Hierarchical Multinomial Logits with a Dirichlet Process prior
and covariates, Hierarchical Negative Binomial Regression Models, Bayesian
analysis of choice-based conjoint data, Bayesian treatment of linear
instrumental variables models, Analysis of Multivariate Ordinal survey data
with scale usage heterogeneity, and Bayesian Analysis of Aggregate Random
Coefficient Logit Models.")
    (license license:gpl2+)))

(define-public r-tensora
  (package
    (name "r-tensora")
    (version "0.36.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "tensorA" version))
       (sha256
        (base32
         "176hjy3bvg3in62r97wxbhq187sjz6c1gwy9x6spaxl6k4my3zy7"))))
    (properties `((upstream-name . "tensorA")))
    (build-system r-build-system)
    (home-page "http://www.stat.boogaart.de/tensorA")
    (synopsis "Advanced tensor arithmetic with named indices")
    (description
     "This package provides convenience functions for advanced linear algebra
with tensors and computation with datasets of tensors on a higher level
abstraction.  It includes Einstein and Riemann summing conventions, dragging,
co- and contravariate indices, and parallel computations on sequences of
tensors.")
    (license license:gpl2+)))

(define-public r-rarpack
  (package
    (name "r-rarpack")
    (version "0.11-0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rARPACK" version))
       (sha256
        (base32
         "12h2y46xcfldhjdmm960swgn9b23zvkj5vg2bi42s9qxwgi02d63"))))
    (properties `((upstream-name . "rARPACK")))
    (build-system r-build-system)
    (propagated-inputs `(("r-rspectra" ,r-rspectra)))
    (home-page "https://github.com/yixuan/rARPACK")
    (synopsis "Solvers for large scale eigenvalue and SVD problems")
    (description
     "This package was previously an R wrapper of the ARPACK library, and now
a shell of the R package RSpectra, an R interface to the Spectra library for
solving large scale eigenvalue/vector problems.  The current version of
rARPACK simply imports and exports the functions provided by RSpectra.  New
users of rARPACK are advised to switch to the RSpectra package.")
    (license license:bsd-3)))

(define-public r-compositions
  (package
    (name "r-compositions")
    (version "1.40-4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "compositions" version))
       (sha256
        (base32
         "0z40llyij3cc80ac1vzzrpykk6ysp89bn6dyyh40fbnc4anwx69a"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bayesm" ,r-bayesm)
       ("r-robustbase" ,r-robustbase)
       ("r-tensora" ,r-tensora)))
    (home-page "http://www.stat.boogaart.de/compositions")
    (synopsis "Compositional data analysis")
    (description
     "This package provides functions for the consistent analysis of
compositional data (e.g. portions of substances) and positive
numbers (e.g. concentrations).")
    (license license:gpl2+)))

(define-public r-cobs
  (package
    (name "r-cobs")
    (version "1.3-4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "cobs" version))
       (sha256
        (base32
         "0hiw5smk6kgk0gb9840kcqkhkybl7n30s77xhjc395x09izbgix1"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-quantreg" ,r-quantreg)
       ("r-sparsem" ,r-sparsem)))
    (home-page "https://cran.r-project.org/web/packages/cobs")
    (synopsis "Constrained B-Splines (sparse matrix based)")
    (description
     "This package provides qualitatively constrained (regression) smoothing
splines via linear programming and sparse matrices.")
    (license license:gpl2+)))

(define-public r-drimpute
  (package
    (name "r-drimpute")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "DrImpute" version))
       (sha256
        (base32
         "1adzarrwqb282pqgx2yqswp9rpwd1naxsmar54kddr6qyd6b923b"))))
    (properties `((upstream-name . "DrImpute")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rcpp" ,r-rcpp)
       ("r-rcpparmadillo" ,r-rcpparmadillo)))
    (home-page "https://github.com/ikwak2/DrImpute")
    (synopsis "Imputing dropout events in single-cell RNA-Seq data")
    (description
     "This is an R package for imputing dropout events.  Many statistical
methods in cell type identification, visualization and lineage reconstruction
do not account for dropout events.  DrImpute can improve the performance of
such software by imputing dropout events.")
    (license license:gpl3)))

(define-public r-gamlss-dist
  (package
    (name "r-gamlss-dist")
    (version "5.1-6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "gamlss.dist" version))
       (sha256
        (base32 "1p904x0b07z4amaqdn2xhs7qzbq8lisr6lqc844s3pkxzmny7w1z"))))
    (properties `((upstream-name . "gamlss.dist")))
    (build-system r-build-system)
    (propagated-inputs `(("r-mass" ,r-mass)))
    (home-page "http://www.gamlss.org/")
    (synopsis "Distributions for Generalized Additive Models for location scale and shape")
    (description
     "This package provides a set of distributions which can be used for
modelling the response variables in Generalized Additive Models for Location
Scale and Shape.  The distributions can be continuous, discrete or mixed
distributions.  Extra distributions can be created, by transforming, any
continuous distribution defined on the real line, to a distribution defined on
ranges 0 to infinity or 0 to 1, by using a @code{log} or a @code{logit}
transformation, respectively.")
    ;; Either version of the GPL.
    (license (list license:gpl2 license:gpl3))))

;; This package includes JavaScript files, which are not minified.  When
;; upgrading please check that there are no new minified JavaScript files.
(define-public r-shinyjs
  (package
    (name "r-shinyjs")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "shinyjs" version))
       (sha256
        (base32
         "14k8y313ppj23m9rhlk8jc94x6sbn3qrsnx6xrijiyv8m8dii1l9"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-digest" ,r-digest)
       ("r-htmltools" ,r-htmltools)
       ("r-jsonlite" ,r-jsonlite)
       ("r-shiny" ,r-shiny)))
    (home-page "https://deanattali.com/shinyjs")
    (synopsis "Improve the user experience of your Shiny apps")
    (description
     "Perform common useful JavaScript operations in Shiny apps that will
greatly improve your apps without having to know any JavaScript.  Examples
include: hiding an element, disabling an input, resetting an input back to its
original value, delaying code execution by a few seconds, and many more useful
functions for both the end user and the developer.  Shinyjs can also be used
to easily call your own custom JavaScript functions from R.")
    (license license:agpl3+)))

;; This package includes minified JavaScript files.  When upgrading please
;; check that there are no new minified JavaScript files.
(define-public r-colourpicker
  (package
    (name "r-colourpicker")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "colourpicker" version))
       (sha256
        (base32
         "0z3v2083g7kwdp21x9s2n1crfh24agpdq3yxkcdzc2awn2pwpnpi"))))
    (build-system r-build-system)
    (arguments
     `(#:modules ((guix build utils)
                  (guix build r-build-system)
                  (srfi srfi-1)
                  (ice-9 popen))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'process-javascript
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "inst"
               (call-with-values
                   (lambda ()
                     (unzip2
                      `((,(assoc-ref inputs "js-salvattore")
                         "examples/colourInput/www/salvattore.min.js")
                        (,(assoc-ref inputs "js-jquery")
                         "htmlwidgets/lib/jquery/jquery.min.js")
                        ("www/shared/colourpicker/js/colourpicker.js"
                         "www/shared/colourpicker/js/colourpicker.min.js"))))
                 (lambda (sources targets)
                   (for-each (lambda (source target)
                               (format #t "Processing ~a --> ~a~%"
                                       source target)
                               (delete-file target)
                               (let ((minified (open-pipe* OPEN_READ "uglify-js" source)))
                                 (call-with-output-file target
                                   (lambda (port)
                                     (dump-port minified port)))))
                             sources targets))))
             #t)))))
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-htmltools" ,r-htmltools)
       ("r-htmlwidgets" ,r-htmlwidgets)
       ("r-jsonlite" ,r-jsonlite)
       ("r-miniui" ,r-miniui)
       ("r-shiny" ,r-shiny)
       ("r-shinyjs" ,r-shinyjs)))
    (native-inputs
     `(("uglify-js" ,uglify-js)
       ("js-jquery"
        ,(origin
           (method url-fetch)
           (uri "https://code.jquery.com/jquery-3.3.1.js")
           (sha256
            (base32
             "1b8zxrp6xwzpw25apn8j4qws0f6sr7qr7h2va5h1mjyfqvn29anq"))))
       ("js-salvattore"
        ,(origin
           (method url-fetch)
           (uri "https://raw.githubusercontent.com/rnmp/salvattore/v1.0.9/dist/salvattore.js")
           (sha256
            (base32
             "0lfrbx7l9w5x89jpc6njmd0pk7h8fpvg537vklai2vf7b1r2nnk5"))))))
    (home-page "https://github.com/daattali/colourpicker")
    (synopsis "Color picker tool for Shiny and for selecting colors in plots")
    (description
     "This package provides a color picker that can be used as an input in
Shiny apps or Rmarkdown documents.  The color picker supports alpha opacity,
custom color palettes, and many more options.  A plot color helper tool is
available as an RStudio Addin, which helps you pick colors to use in your
plots.  A more generic color picker RStudio Addin is also provided to let you
select colors to use in your R code.")
    (license license:expat)))

(define-public r-ggextra
  (package
    (name "r-ggextra")
    (version "0.9")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ggExtra" version))
       (sha256
        (base32
         "18mbi6gblqmrsciad1d2c9ngllk6mayaqj43k40hjq9ydqnvjbgj"))))
    (properties `((upstream-name . "ggExtra")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-colourpicker" ,r-colourpicker)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gtable" ,r-gtable)
       ("r-miniui" ,r-miniui)
       ("r-r6" ,r-r6)
       ("r-scales" ,r-scales)
       ("r-shiny" ,r-shiny)
       ("r-shinyjs" ,r-shinyjs)))
    (home-page "https://github.com/daattali/ggExtra")
    (synopsis "Marginal histograms for ggplot2 and other enhancements")
    (description
     "This package is a collection of functions and layers to enhance ggplot2.
The flagship function is @code{ggMarginal()}, which can be used to add
marginal histograms/boxplots/density plots to ggplot2 scatterplots.")
    (license license:expat)))

(define-public r-minpack-lm
  (package
    (name "r-minpack-lm")
    (version "1.2-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "minpack.lm" version))
       (sha256
        (base32
         "18ym2pdql5vzngc7q5gn66d153hrfrnd8ilv8yh6vd7j7sx7vjql"))))
    (properties `((upstream-name . "minpack.lm")))
    (build-system r-build-system)
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/minpack.lm")
    (synopsis "Levenberg-Marquardt Nonlinear Least-Squares algorithm")
    (description
     "The @code{nls.lm} function provides an R interface to @code{lmder} and
@code{lmdif} from the MINPACK library, for solving nonlinear least-squares
problems by a modification of the Levenberg-Marquardt algorithm, with support
for lower and upper parameter bounds.  The implementation can be used via
@code{nls}-like calls using the @code{nlsLM} function.")
    (license license:gpl3)))

(define-public r-moments
  (package
    (name "r-moments")
    (version "0.14")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "moments" version))
       (sha256
        (base32
         "0f9y58w1hxcz4bqivirx25ywlmc80gbi6dfx5cnhkpdg1pk82fra"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/moments")
    (synopsis "Moments, cumulants, skewness, kurtosis and related tests")
    (description
     "This package provides functions to calculate: moments, Pearson's
kurtosis, Geary's kurtosis and skewness; it also includes tests related to
them (Anscombe-Glynn, D'Agostino, Bonett-Seier).")
    (license license:gpl2+)))

(define-public r-msir
  (package
    (name "r-msir")
    (version "1.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "msir" version))
       (sha256
        (base32
         "0pvc3q162vqq3k39nni732x05zzfz4y9y2zf56d83185ypszv9kb"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-mclust" ,r-mclust)))
    (home-page "https://cran.r-project.org/web/packages/msir")
    (synopsis "Model-based sliced inverse regression")
    (description
     "This is an R package for dimension reduction based on finite Gaussian
mixture modeling of inverse regression.")
    (license license:gpl2+)))

(define-public r-pbivnorm
  (package
    (name "r-pbivnorm")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "pbivnorm" version))
       (sha256
        (base32
         "05jzrjqxzbcf6z245hlk7sjxiszv9paadaaimvcx5y5qgi87vhq7"))))
    (build-system r-build-system)
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "https://github.com/brentonk/pbivnorm")
    (synopsis "Vectorized bivariate normal CDF")
    (description
     "This package provides a vectorized R function for calculating
probabilities from a standard bivariate normal CDF.")
    (license license:gpl2+)))

(define-public r-lavaan
  (package
    (name "r-lavaan")
    (version "0.6-5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "lavaan" version))
       (sha256
        (base32
         "04kvsh2m6mnzlhv83phr3hjzy4sx1ck6f7dgsm7xb8cs84dnxszy"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-mass" ,r-mass)
       ("r-mnormt" ,r-mnormt)
       ("r-numderiv" ,r-numderiv)
       ("r-pbivnorm" ,r-pbivnorm)))
    (home-page "http://lavaan.ugent.be")
    (synopsis "Latent variable analysis")
    (description
     "This package provides tools to fit a variety of latent variable models,
including confirmatory factor analysis, structural equation modeling and
latent growth curve models.")
    (license license:gpl2+)))

(define-public r-nonnest2
  (package
    (name "r-nonnest2")
    (version "0.5-2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "nonnest2" version))
       (sha256
        (base32
         "1bq44qqmm59j91m0sny4xnqmxqlga4cm48qdsw8xfs3x19xwmxk6"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-compquadform" ,r-compquadform)
       ("r-lavaan" ,r-lavaan)
       ("r-mvtnorm" ,r-mvtnorm)
       ("r-sandwich" ,r-sandwich)))
    (home-page "https://cran.r-project.org/web/packages/nonnest2/")
    (synopsis "Tests of non-nested models")
    (description
     "This package allows for testing of non-nested models.  It includes tests
of model distinguishability and of model fit that can be applied to both
nested and non-nested models.  The package also includes functionality to
obtain confidence intervals associated with AIC and BIC.")
    ;; Either version of the GPL.
    (license (list license:gpl2 license:gpl3))))

(define-public r-penalized
  (package
    (name "r-penalized")
    (version "0.9-51")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "penalized" version))
       (sha256
        (base32
         "1zcrwa93mc27qj3g4ayc2k895r6g8q0g6qb2azmvj7wqk750va7a"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rcpp" ,r-rcpp)
       ("r-rcpparmadillo" ,r-rcpparmadillo)
       ("r-survival" ,r-survival)))
    (home-page "https://cran.r-project.org/web/packages/penalized/")
    (synopsis "Penalized estimation in GLMs and in the Cox model")
    (description
     "This package provides tools for fitting possibly high dimensional
penalized regression models.  The penalty structure can be any combination of
an L1 penalty (lasso and fused lasso), an L2 penalty (ridge) and a positivity
constraint on the regression coefficients.  The supported regression models
are linear, logistic and Poisson regression and the Cox Proportional Hazards
model.  Cross-validation routines allow optimization of the tuning
parameters.")
    (license license:gpl2+)))

(define-public r-zim
  (package
    (name "r-zim")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ZIM" version))
       (sha256
        (base32
         "0scyfjn4ilsvha3x41c3b8bcfi31hlhwm77wn2a8hj5dsvnnmzig"))))
    (properties `((upstream-name . "ZIM")))
    (build-system r-build-system)
    (propagated-inputs `(("r-mass" ,r-mass)))
    (home-page "https://github.com/biostatstudio/ZIM")
    (synopsis "Zero-inflated models (ZIM) for count time series with excess zeros")
    (description
     "Analyze count time series with excess zeros.  Two types of statistical
models are supported: Markov regression and state-space models.  They are also
known as observation-driven and parameter-driven models respectively in the
time series literature.  The functions used for Markov regression or
observation-driven models can also be used to fit ordinary regression models
with independent data under the zero-inflated Poisson (ZIP) or zero-inflated
negative binomial (ZINB) assumption.  The package also contains miscellaneous
functions to compute density, distribution, quantile, and generate random
numbers from ZIP and ZINB distributions.")
    (license license:gpl3)))

(define-public r-nor1mix
  (package
    (name "r-nor1mix")
    (version "1.3-0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "nor1mix" version))
       (sha256
        (base32
         "1817wcvlmxs70vs4db0jkxd7i037744zz8ay3c2a9949z29fxr4w"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/nor1mix/")
    (synopsis "Normal (1-d) mixture models")
    (description
     "This package provides S3 classes and methods for one-dimensional normal
mixture models, for, e.g., density estimation or clustering algorithms
research and teaching; it provides the widely used Marron-Wand densities.  It
also provides tools for efficient random number generation and graphics.")
    (license license:gpl2+)))

(define-public r-beanplot
  (package
    (name "r-beanplot")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "beanplot" version))
       (sha256
        (base32
         "0wmkr704fl8kdxkjwmaxw2a2h5dwzfgsgpncnk2p2wd4768jknj9"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/beanplot/")
    (synopsis "Visualization via beanplots")
    (description
     "This package provides beanplots, an alternative to
boxplot/stripchart/violin plots.  It can be used to plot univariate comparison
graphs.")
    (license license:gpl2)))

(define-public r-pbdzmq
  (package
    (name "r-pbdzmq")
    (version "0.3-3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "pbdZMQ" version))
       (sha256
        (base32
         "1jkfcfhspvqra7vbllrvkz3jx8j7d0ang6zzcdjgpb7200sc29mf"))))
    (properties `((upstream-name . "pbdZMQ")))
    (build-system r-build-system)
    (inputs
     `(("zeromq" ,zeromq)
       ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://pbdr.org/")
    (synopsis "R interface to ZeroMQ")
    (description
     "ZeroMQ is a well-known library for high-performance asynchronous
messaging in scalable, distributed applications.  This package provides high
level R wrapper functions to easily utilize ZeroMQ.  The main focus is on
interactive client/server programming frameworks.  A few wrapper functions
compatible with @code{rzmq} are also provided.")
    (license license:gpl3)))

(define-public r-repr
  (package
    (name "r-repr")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "repr" version))
       (sha256
        (base32
         "15jz780w4nd9qjd1g3gq8f5lkh60p2v3ig3hm5kl1rg3z4cf0gvl"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-base64enc" ,r-base64enc)
       ("r-htmltools" ,r-htmltools)
       ("r-jsonlite" ,r-jsonlite)
       ("r-pillar" ,r-pillar)))
    (home-page "https://cran.r-project.org/web/packages/repr/")
    (synopsis "Serializable representations")
    (description
     "This package provides string and binary representations of objects for
several formats and MIME types.")
    (license license:gpl3)))

(define-public r-irdisplay
  (package
    (name "r-irdisplay")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "IRdisplay" version))
       (sha256
        (base32
         "12chk53nf4zckgc4yl7gbvd7m5dvli52inp5b3f0zvcjvfncksli"))))
    (properties `((upstream-name . "IRdisplay")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-repr" ,r-repr)))
    (home-page "https://cran.r-project.org/web/packages/IRdisplay/")
    (synopsis "Jupyter display machinery")
    (description
     "This package provides an interface to the rich display capabilities of
Jupyter front-ends (e.g. Jupyter Notebook).  It is designed to be used from a
running IRkernel session.")
    (license license:expat)))

(define-public r-irkernel
  (package
    (name "r-irkernel")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "IRkernel" version))
       (sha256
        (base32
         "1viqxs91dys1z4cf7gb59rmqvzb8lc7jdp4azrpmhgwa8qf46s94"))))
    (properties `((upstream-name . "IRkernel")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-kernelspec
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (setenv "HOME" "/tmp")
               (invoke "jupyter" "kernelspec" "install"
                       "--name" "ir"
                       "--prefix" out
                       (string-append out "/site-library/IRkernel/kernelspec"))
               ;; Record the absolute file name of the 'R' executable in
               ;; 'kernel.json'.
               (substitute* (string-append out "/share/jupyter"
                                           "/kernels/ir/kernel.json")
                 (("\\[\"R\",")
                  (string-append "[\"" (which "R") "\",")))
               #t))))))
    (inputs
     `(("jupyter" ,jupyter)))
    (propagated-inputs
     `(("r-crayon" ,r-crayon)
       ("r-digest" ,r-digest)
       ("r-evaluate" ,r-evaluate)
       ("r-irdisplay" ,r-irdisplay)
       ("r-jsonlite" ,r-jsonlite)
       ;; sets R_LIBS_SITE, so R can actually find this package (IRkernel)
       ("r-minimal" ,r-minimal)
       ("r-pbdzmq" ,r-pbdzmq)
       ("r-repr" ,r-repr)
       ("r-uuid" ,r-uuid)))
    (home-page "https://cran.r-project.org/web/packages/IRkernel/")
    (synopsis "Native R kernel for Jupyter")
    (description
     "The R kernel for the Jupyter environment executes R code which the
front-end (Jupyter Notebook or other front-ends) submits to the kernel via the
network.")
    (license license:expat)))

(define-public r-gmodels
  (package
    (name "r-gmodels")
    (version "2.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "gmodels" version))
       (sha256
        (base32
         "0s8kd8krqk4kwv2zqxpsfy3w8qdwf5naf4b5l383vidq9sil0qb2"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-gdata" ,r-gdata)
       ("r-mass" ,r-mass)))
    (home-page "https://cran.r-project.org/web/packages/gmodels/")
    (synopsis "Various R programming tools for model fitting")
    (description
     "This package provides various R programming tools for model fitting.")
    (license license:gpl2)))

(define-public r-apcluster
  (package
    (name "r-apcluster")
    (version "1.4.8")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "apcluster" version))
       (sha256
        (base32
         "0lzf2jqm56i74wif6x5sw3j0w2qc4sni49zq2fgbl89b7lwkvchj"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-matrix" ,r-matrix)
       ("r-rcpp" ,r-rcpp)))
    (home-page "https://cran.r-project.org/web/packages/apcluster/")
    (synopsis "Affinity propagation clustering")
    (description
     "This package implements affinity propagation clustering introduced by
Frey and Dueck (2007).  The package further provides leveraged affinity
propagation and an algorithm for exemplar-based agglomerative clustering that
can also be used to join clusters obtained from affinity propagation.  Various
plotting functions are available for analyzing clustering results.")
    (license license:gpl2+)))

(define-public r-valr
  (package
    (name "r-valr")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "valr" version))
       (sha256
        (base32
         "14jhrwkiwmha3vlmm7b50n2xxyizj6ddmy89gb20mpzq7qhz1ika"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-broom" ,r-broom)
       ("r-dplyr" ,r-dplyr)
       ("r-ggplot2" ,r-ggplot2)
       ("r-rcpp" ,r-rcpp)
       ("r-readr" ,r-readr)
       ("r-rlang" ,r-rlang)
       ("r-stringr" ,r-stringr)
       ("r-tibble" ,r-tibble)))
    (home-page "http://github.com/rnabioco/valr")
    (synopsis "Genome interval arithmetic in R")
    (description
     "This package enables you to read and manipulate genome intervals and
signals.  It provides functionality similar to command-line tool suites within
R, enabling interactive analysis and visualization of genome-scale data.")
    (license license:expat)))

(define-public r-rematch2
  (package
    (name "r-rematch2")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rematch2" version))
       (sha256
        (base32
         "00cznm6rk33b53w7zybkz7549bnydc66znpi5mb0xd24pmqp0rvq"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-tibble" ,r-tibble)))
    (home-page "https://github.com/r-lib/rematch2")
    (synopsis "Tidy output from regular expression matching")
    (description
     "This package provides wrappers on @code{regexpr} and @code{gregexpr} to
return the match results in tidy data frames.")
    (license license:expat)))

(define-public r-picante
  (package
    (name "r-picante")
    (version "1.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "picante" version))
       (sha256
        (base32
         "1b16zm8zjjsl181b8krkdcrbcw347kf772c4w7y5332qmfi7jhz0"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ape" ,r-ape)
       ("r-nlme" ,r-nlme)
       ("r-vegan" ,r-vegan)))
    (home-page "https://cran.r-project.org/web/packages/picante/")
    (synopsis "Integrating phylogenies and ecology")
    (description
     "This package provides functions for phylocom integration, community
analyses, null-models, traits and evolution.  It implements numerous
ecophylogenetic approaches including measures of community phylogenetic and
trait diversity, phylogenetic signal, estimation of trait values for
unobserved taxa, null models for community and phylogeny randomizations, and
utility functions for data input/output and phylogeny plotting.  A full
description of package functionality and methods are provided by Kembel et
al. (2010).")
    (license license:gpl2)))

(define-public r-reinforcelearn
  (package
    (name "r-reinforcelearn")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "reinforcelearn" version))
       (sha256
        (base32
         "176z2q69p24i29a8sh19xxn2zl3h1z2ixdssr5i6m4yvkvdrvv3b"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-checkmate" ,r-checkmate)
       ("r-nnet" ,r-nnet)
       ("r-purrr" ,r-purrr)
       ("r-r6" ,r-r6)))
    (home-page "https://markusdumke.github.io/reinforcelearn")
    (synopsis "Reinforcement learning")
    (description
     "This package implements reinforcement learning environments and
algorithms as described in Sutton & Barto (1998).  The Q-Learning algorithm
can be used with function approximation, eligibility traces (Singh & Sutton,
1996) and experience replay (Mnih et al., 2013).")
    (license license:expat)))

(define-public r-lemon
  (package
    (name "r-lemon")
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "lemon" version))
       (sha256
        (base32
         "0wsn5bfg10wq4dnrgpyraz2bzx9p19c7hf1pwj3h4zmpqfgsdbpw"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-gridextra" ,r-gridextra)
       ("r-gtable" ,r-gtable)
       ("r-knitr" ,r-knitr)
       ("r-lattice" ,r-lattice)
       ("r-plyr" ,r-plyr)
       ("r-scales" ,r-scales)))
    (home-page "https://github.com/stefanedwards/lemon")
    (synopsis "Freshen up your ggplot2 plots")
    (description
     "This package provides functions for working with legends and axis lines
of ggplot2, facets that repeat axis lines on all panels, and some knitr
extensions.")
    (license license:gpl3)))

(define-public r-wgaim
  (package
    (name "r-wgaim")
    (version "2.0-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "wgaim" version))
       (sha256
        (base32 "1qiyfkpsbzjr9xsq5kqq6rlqpndngkn2irdfh3gyi45h6hn118j4"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-qtl" ,r-qtl)))
    (home-page "https://cran.r-project.org/web/packages/wgaim")
    (synopsis "Whole genome average interval mapping for QTL detection")
    (description
     "This package integrates sophisticated mixed modelling methods with a
whole genome approach to detecting significant QTL in linkage maps.")
    (license license:gpl2+)))

(define-public r-bedr
  (package
    (name "r-bedr")
    (version "1.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "bedr" version))
       (sha256
        (base32
         "0zpqvyjgwyqawxm8qrhcv8zq2b3yxgcqkkc87br29yrl7sjb8h6j"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-data-table" ,r-data-table)
       ("r-r-utils" ,r-r-utils)
       ("r-testthat" ,r-testthat)
       ("r-venndiagram" ,r-venndiagram)
       ("r-yaml" ,r-yaml)
       ("bedops" ,bedops)
       ("bedtools" ,bedtools)
       ("htslib" ,htslib))) ; for tabix
    (native-inputs
     `(("r-knitr" ,r-knitr))) ; for vignettes
    (home-page "https://cran.r-project.org/web/packages/bedr")
    (synopsis "Genomic region processing")
    (description
     "This package is for genomic regions processing using command line tools
such as BEDTools, BEDOPS and Tabix.  These tools offer scalable and efficient
utilities to perform genome arithmetic e.g indexing, formatting and merging.
The bedr package's API enhances access to these tools as well as offers
additional utilities for genomic regions processing.")
    (license license:gpl2)))

(define-public r-sets
  (package
    (name "r-sets")
    (version "1.0-18")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "sets" version))
       (sha256
        (base32
         "16v7650p47khqrbbw0z98llmwmmhswqmhri0n7nrfhdqwmby1lbl"))))
    (properties `((upstream-name . "sets")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/sets")
    (synopsis "Sets, generalized sets, customizable sets and intervals")
    (description
     "This package provides data structures and basic operations for ordinary
sets, generalizations such as fuzzy sets, multisets, and fuzzy multisets,
customizable sets, and intervals.")
    (license license:gpl2)))

(define-public r-partitions
  (package
    (name "r-partitions")
    (version "1.9-22")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "partitions" version))
       (sha256
        (base32
         "1qqy4df28wy4q0g572azrj171jlhvrnzbh7x0wr2g7v6gr20y0ns"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-gmp" ,r-gmp)
       ("r-polynom" ,r-polynom)
       ("r-sets" ,r-sets)))
    (home-page "https://cran.r-project.org/web/packages/partitions")
    (synopsis "Additive partitions of integers")
    (description
     "This package provides tools to enumerates the partitions, unequal
partitions, and restricted partitions of an integer; the three corresponding
partition functions are also given.")
    ;; Any version of the GPL
    (license license:gpl2+)))

(define-public r-brobdingnag
  (package
    (name "r-brobdingnag")
    (version "1.2-6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Brobdingnag" version))
       (sha256
        (base32
         "1m3ajvcksqfck5l5hj5xiflj4ry6d896ybv4f0xxks8chgnwmv0r"))))
    (properties `((upstream-name . "Brobdingnag")))
    (build-system r-build-system)
    (home-page "https://github.com/RobinHankin/Brobdingnag.git")
    (synopsis "Very large numbers in R")
    (description
     "This package handles very large numbers in R.  Real numbers are held
using their natural logarithms, plus a logical flag indicating sign.  The
package includes a vignette that gives a step-by-step introduction to using S4
methods.")
    ;; Any version of the GPL
    (license license:gpl2+)))

(define-public r-untb
  (package
    (name "r-untb")
    (version "1.7-4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "untb" version))
       (sha256
        (base32
         "1i7m4vfslsix98dwx4jlrsldm7fhhfp25gr7aapcxqxms7ryaby6"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-brobdingnag" ,r-brobdingnag)
       ("r-partitions" ,r-partitions)
       ("r-polynom" ,r-polynom)))
    (home-page "https://github.com/RobinHankin/untb.git")
    (synopsis "Ecological drift under the UNTB")
    (description
     "This package provides numerical simulations, and visualizations, of
Hubbell's @dfn{Unified Neutral Theory of Biodiversity} (UNTB).")
    (license license:gpl2+)))

(define-public r-stepwise
  (package
    (name "r-stepwise")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "stepwise" version))
       (sha256
        (base32
         "1lbx1bxwkf9dw6q46w40pp7h5nkxgghmx8rkpaymm6iybc7gyir2"))))
    (build-system r-build-system)
    (home-page "http://stat.sfu.ca/statgen/research/stepwise.html")
    (synopsis "Stepwise detection of recombination breakpoints")
    (description
     "This package provides a stepwise approach to identifying recombination
breakpoints in a genomic sequence alignment.")
    (license license:gpl2+)))

(define-public r-snpmaxsel
  (package
    (name "r-snpmaxsel")
    (version "1.0-3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "SNPmaxsel" version))
       (sha256
        (base32
         "0pjvixwqzjd3jwccc8yqq9c76afvbmfq0z1w0cwyj8bblrjpx13z"))))
    (properties `((upstream-name . "SNPmaxsel")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-combinat" ,r-combinat)
       ("r-mvtnorm" ,r-mvtnorm)))
    (home-page "https://cran.r-project.org/web/packages/SNPmaxsel/index.html")
    (synopsis "Maximally selected statistics for SNP data")
    (description
     "This package implements asymptotic methods related to maximally selected
statistics, with applications to @dfn{single-nucleotide polymorphism} (SNP)
data.")
    (license license:gpl2+)))

(define-public r-acsnminer
  (package
    (name "r-acsnminer")
    (version "0.16.8.25")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "ACSNMineR" version))
              (sha256
               (base32
                "0gh604s8qall6zfjlwcg2ilxjvz08dplf9k5g47idhv43scm748l"))))
    (properties `((upstream-name . "ACSNMineR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-gridextra" ,r-gridextra)))
    (home-page "https://cran.r-project.org/web/packages/ACSNMineR")
    (synopsis "Gene enrichment analysis")
    (description
     "This package provides tools to compute and represent gene set enrichment
or depletion from your data based on pre-saved maps from the @dfn{Atlas of
Cancer Signalling Networks} (ACSN) or user imported maps.  The gene set
enrichment can be run with hypergeometric test or Fisher exact test, and can
use multiple corrections.  Visualization of data can be done either by
barplots or heatmaps.")
    (license license:gpl2+)))

(define-public r-seqinr
  (package
    (name "r-seqinr")
    (version "3.6-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "seqinr" version))
       (sha256
        (base32
         "0j30za6kji6y3v09cvcydiacnp65pv6ig8aw7cydl47l5s9chky4"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ade4" ,r-ade4)
       ("r-segmented" ,r-segmented)))
    (inputs
     `(("zlib" ,zlib)))
    (home-page "http://seqinr.r-forge.r-project.org/")
    (synopsis "Biological sequences retrieval and analysis")
    (description
     "This package provides tools for exploratory data analysis and data
visualization of biological sequence (DNA and protein) data.  It also includes
utilities for sequence data management under the ACNUC system.")
    (license license:gpl2+)))

(define-public r-units
  (package
    (name "r-units")
    (version "0.6-5")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "units" version))
        (sha256
          (base32
            "02nls8m0r1r7kljs4x35naz3szq62hyqyd5vracf1xwi1kz5kdsh"))))
    (build-system r-build-system)
    (inputs
     `(("udunits" ,udunits)))
    (propagated-inputs
     `(("r-rcpp" ,r-rcpp)))
    (home-page "https://github.com/r-quantities/units/")
    (synopsis "Measurement Units for R Vectors")
    (description
      "This package provides support for measurement units in R vectors,
matrices and arrays: automatic propagation, conversion, derivation and
simplification of units; raising errors in case of unit incompatibility.  It
is compatible with the @code{POSIXct}, @code{Date} and @code{difftime}
classes.")
    (license license:gpl2)))

(define-public r-classint
  (package
    (name "r-classint")
    (version "0.4-2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "classInt" version))
       (sha256
        (base32
         "0w980hrw8sgfdfyd5dsimalq7gwhvqm7507abk7k363pvgks23dv"))))
    (properties `((upstream-name . "classInt")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-class" ,r-class)
       ("r-e1071" ,r-e1071)
       ("r-kernsmooth" ,r-kernsmooth)))
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "https://github.com/r-spatial/classInt/")
    (synopsis "Choose univariate class intervals")
    (description
     "This package provides selected commonly used methods for choosing
univariate class intervals for mapping or other graphics purposes.")
    (license license:gpl2+)))

(define-public r-spdata
  (package
    (name "r-spdata")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "spData" version))
       (sha256
        (base32
         "1v66qkvsx77hvv5c78v760yp0hknf7xzcjir2ri3ha456mz79yl5"))))
    (properties `((upstream-name . "spData")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-raster" ,r-raster)
       ("r-sp" ,r-sp)))
    (home-page "https://github.com/Nowosad/spData")
    (synopsis "Datasets for spatial analysis")
    (description
     "This a package containing diverse spatial datasets for demonstrating,
benchmarking and teaching spatial data analysis.  It includes R data of class
@code{sf}, @code{Spatial}, and @code{nb}.  It also contains data stored in a
range of file formats including GeoJSON, ESRI Shapefile and GeoPackage.  Some
of the datasets are designed to illustrate specific analysis techniques.
@code{cycle_hire()} and @code{cycle_hire_osm()}, for example, are designed to
illustrate point pattern analysis techniques.")
    (license license:cc0)))

(define-public r-learnbayes
  (package
    (name "r-learnbayes")
    (version "2.15.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "LearnBayes" version))
       (sha256
        (base32
         "0ch54v2zz2yyyk0lvn5rfikdmyz1qh9j1wk3585wl8v58mc0h4cv"))))
    (properties `((upstream-name . "LearnBayes")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/LearnBayes")
    (synopsis "Functions for learning Bayesian inference")
    (description
     "This package provides a collection of functions helpful in learning the
basic tenets of Bayesian statistical inference.  It contains functions for
summarizing basic one and two parameter posterior distributions and predictive
distributions.  It contains MCMC algorithms for summarizing posterior
distributions defined by the user.  It also contains functions for regression
models, hierarchical models, Bayesian tests, and illustrations of Gibbs
sampling.")
    (license license:gpl2+)))

(define-public r-deldir
  (package
    (name "r-deldir")
    (version "0.1-25")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "deldir" version))
       (sha256
        (base32
         "0kdglv8rc1pb4ilcid4xc9wpv5kkj2y6x6wg7919k3hya7mz58ph"))))
    (build-system r-build-system)
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/deldir")
    (synopsis "Delaunay triangulation and Dirichlet (Voronoi) tessellation")
    (description
     "This package provides tools for calculating the Delaunay triangulation
and the Dirichlet or Voronoi tessellation (with respect to the entire plane)
of a planar point set.  It plots triangulations and tessellations in various
ways, clips tessellations to sub-windows, calculates perimeters of
tessellations, and summarizes information about the tiles of the
tessellation.")
    (license license:gpl2+)))

(define-public r-sf
  (package
    (name "r-sf")
    (version "0.8-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "sf" version))
       (sha256
        (base32
         "17wyv74m1ppc3s5jdv60r74jj23nidxpzl46vjhfclfhnpvm7sx9"))))
    (build-system r-build-system)
    (inputs
     `(("gdal" ,gdal)
       ("geos" ,geos)
       ("proj" ,proj.4)
       ("zlib" ,zlib)))
    (propagated-inputs
     `(("r-classint" ,r-classint)
       ("r-dbi" ,r-dbi)
       ("r-magrittr" ,r-magrittr)
       ("r-rcpp" ,r-rcpp)
       ("r-units" ,r-units)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://github.com/r-spatial/sf/")
    (synopsis "Simple features for R")
    (description
     "This package provides support for simple features, a standardized way to
encode spatial vector data.  It binds to GDAL for reading and writing data, to
GEOS for geometrical operations, and to PROJ for projection conversions and
datum transformations.")
    ;; Either of these licenses
    (license (list license:gpl2 license:expat))))

(define-public r-spdep
  (package
    (name "r-spdep")
    (version "1.1-3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "spdep" version))
       (sha256
        (base32
         "1f8cjffqqc6rnb3n4qym70ca6nz2kvrsd3g587wrqdr79nnbwnrk"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-boot" ,r-boot)
       ("r-coda" ,r-coda)
       ("r-deldir" ,r-deldir)
       ("r-expm" ,r-expm)
       ("r-gmodels" ,r-gmodels)
       ("r-learnbayes" ,r-learnbayes)
       ("r-mass" ,r-mass)
       ("r-matrix" ,r-matrix)
       ("r-nlme" ,r-nlme)
       ("r-sf" ,r-sf)
       ("r-sp" ,r-sp)
       ("r-spdata" ,r-spdata)))
    (home-page "https://github.com/r-spatial/spdep/")
    (synopsis "Spatial dependence: weighting schemes, statistics and models")
    (description
     "This package provides a collection of functions to create spatial
weights matrix objects from polygon contiguities, from point patterns by
distance and tessellations, for summarizing these objects, and for permitting
their use in spatial data analysis, including regional aggregation by minimum
spanning tree.")
    (license license:gpl2+)))

(define-public r-adegenet
  (package
    (name "r-adegenet")
    (version "2.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "adegenet" version))
       (sha256
        (base32
         "01fgrgbiddz2q4l3mx637hhwbs7r0c43yw7vpwl8p8pwbm3nykz0"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ade4" ,r-ade4)
       ("r-ape" ,r-ape)
       ("r-boot" ,r-boot)
       ("r-dplyr" ,r-dplyr)
       ("r-ggplot2" ,r-ggplot2)
       ("r-igraph" ,r-igraph)
       ("r-mass" ,r-mass)
       ("r-reshape2" ,r-reshape2)
       ("r-seqinr" ,r-seqinr)
       ("r-shiny" ,r-shiny)
       ("r-spdep" ,r-spdep)
       ("r-vegan" ,r-vegan)))
    (home-page "https://github.com/thibautjombart/adegenet")
    (synopsis "Exploratory analysis of genetic and genomic data")
    (description
     "This package provides a toolset for the exploration of genetic and
genomic data.  Adegenet provides formal (S4) classes for storing and handling
various genetic data, including genetic markers with varying ploidy and
hierarchical population structure (@code{genind} class), alleles counts by
populations (@code{genpop}), and genome-wide SNP data (@code{genlight}).  It
also implements original multivariate methods (DAPC, sPCA), graphics,
statistical tests, simulation tools, distance and similarity measures, and
several spatial methods.  A range of both empirical and simulated datasets is
also provided to illustrate various methods.")
    (license license:gpl2+)))

(define-public r-pegas
  (package
    (name "r-pegas")
    (version "0.13")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "pegas" version))
       (sha256
        (base32 "002i7s4g0nhnq0v05gs0yssqiyhpq2f7rw2rhn31hsbgxc86frvy"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-adegenet" ,r-adegenet)
       ("r-ape" ,r-ape)))
    (home-page "http://ape-package.ird.fr/pegas.html")
    (synopsis "Population and evolutionary genetics analysis system")
    (description
     "This package provides functions for reading, writing, plotting,
analysing, and manipulating allelic and haplotypic data, including from VCF
files, and for the analysis of population nucleotide sequences and
micro-satellites including coalescent analyses, linkage disequilibrium,
population structure (Fst, Amova) and equilibrium (HWE), haplotype networks,
minimum spanning tree and network, and median-joining networks.")
    (license license:gpl2+)))

(define-public r-rmetasim
  (package
    (name "r-rmetasim")
    (version "3.1.14")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rmetasim" version))
       (sha256
        (base32
         "0rdkhfgyr97r2d1kd9g8ipb2pn563qxm1y4m9z678q0kqan2ddl0"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ade4" ,r-ade4)
       ("r-adegenet" ,r-adegenet)
       ("r-gtools" ,r-gtools)
       ("r-pegas" ,r-pegas)))
    (home-page "https://cran.r-project.org/web/packages/rmetasim")
    (synopsis "Individual-based population genetic simulation environment")
    (description
     "This package provides an interface between R and the metasim simulation
engine.  The simulation environment is documented in: Strand, A.(2002),
Metasim 1.0: an individual-based environment for simulating population
genetics of complex population dynamics.")
    ;; Any GPL version
    (license license:gpl2+)))

(define-public r-genetics
  (package
    (name "r-genetics")
    (version "1.3.8.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "genetics" version))
       (sha256
        (base32
         "1v0ylnia6c44v356dsmnkx6054vcxazpzsrdh3yph5ch5vg6gjrh"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-combinat" ,r-combinat)
       ("r-gdata" ,r-gdata)
       ("r-gtools" ,r-gtools)
       ("r-mass" ,r-mass)
       ("r-mvtnorm" ,r-mvtnorm)))
    (home-page "https://cran.r-project.org/web/packages/genetics/")
    (synopsis "Population genetics")
    (description
     "This package provides classes and methods for handling genetic data.
It includes classes to represent genotypes and haplotypes at single markers up
to multiple markers on multiple chromosomes.  Function include allele
frequencies, flagging homo/heterozygotes, flagging carriers of certain
alleles, estimating and testing for Hardy-Weinberg disequilibrium, estimating
and testing for linkage disequilibrium, ...")
    ;; Any GPL version.
    (license license:gpl2+)))

(define-public r-snp-plotter
  (package
    (name "r-snp-plotter")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "snp.plotter" version))
       (sha256
        (base32
         "16apsqvkah5l0d5qcwp3lq2jspkb6n62wzr0wskmj84jblx483vv"))))
    (properties `((upstream-name . "snp.plotter")))
    (build-system r-build-system)
    (propagated-inputs `(("r-genetics" ,r-genetics)))
    (home-page "https://cran.r-project.org/web/packages/snp.plotter/")
    (synopsis "Plot p-values using single SNP and/or haplotype data")
    (description
     "This package helps you create plots of p-values using single SNP and/or
haplotype data.  Main features of the package include options to display a
@dfn{linkage disequilibrium} (LD) plot and the ability to plot multiple
datasets simultaneously.  Plots can be created using global and/or individual
haplotype p-values along with single SNP p-values.  Images are created as
either PDF/EPS files.")
    (license license:gpl2+)))

(define-public r-polspline
  (package
    (name "r-polspline")
    (version "1.1.17")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "polspline" version))
       (sha256
        (base32 "0c7fnxpqpy3hibiim4yib6l6bq363s97wwvllxp4lp8h06fjcyyn"))))
    (build-system r-build-system)
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/polspline/")
    (synopsis "Polynomial spline routines")
    (description
     "This package provides routines for the polynomial spline fitting
routines hazard regression, hazard estimation with flexible tails, logspline,
lspec, polyclass, and polymars.")
    (license license:gpl2+)))

(define-public r-rms
  (package
    (name "r-rms")
    (version "5.1-4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rms" version))
       (sha256
        (base32 "19knh1sw0icw6jh9wfb2hq5jf49i2qfvp9myvqm5paa495689x9q"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-hmisc" ,r-hmisc)
       ("r-htmltable" ,r-htmltable)
       ("r-htmltools" ,r-htmltools)
       ("r-lattice" ,r-lattice)
       ("r-multcomp" ,r-multcomp)
       ("r-nlme" ,r-nlme)
       ("r-polspline" ,r-polspline)
       ("r-quantreg" ,r-quantreg)
       ("r-rpart" ,r-rpart)
       ("r-sparsem" ,r-sparsem)
       ("r-survival" ,r-survival)))
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "http://biostat.mc.vanderbilt.edu/rms")
    (synopsis "Regression modeling strategies")
    (description
     "This is a package for regression modeling, testing, estimation,
validation, graphics, prediction, and typesetting by storing enhanced model
design attributes in the fit.  The rms package is a collection of functions
that assist with and streamline modeling.  It also contains functions for
binary and ordinal logistic regression models, ordinal models for continuous Y
with a variety of distribution families, and the Buckley-James multiple
regression model for right-censored responses, and implements penalized
maximum likelihood estimation for logistic and ordinary linear models.  The
package works with almost any regression model, but it was especially written
to work with binary or ordinal regression models, Cox regression, accelerated
failure time models, ordinary linear models, the Buckley-James model,
generalized least squares for serially or spatially correlated observations,
generalized linear models, and quantile regression.")
    (license license:gpl2+)))

(define-public r-haplo-stats
  (package
    (name "r-haplo-stats")
    (version "1.7.9")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "haplo.stats" version))
       (sha256
        (base32
         "19kxascqq5qz0zdxx0w837ji207y1z2ggxkl4vmlbay03k2dw2mx"))))
    (properties `((upstream-name . "haplo.stats")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rms" ,r-rms)))
    (native-inputs
     `(("r-r-rsp" ,r-r-rsp))) ; for vignettes
    (home-page "https://www.mayo.edu/research/labs/statistical-genetics-genetic-epidemiology/software")
    (synopsis "Analysis of haplotypes when linkage phase is ambiguous")
    (description
     "This package provides routines for the analysis of indirectly measured
haplotypes.  The statistical methods assume that all subjects are unrelated
and that haplotypes are ambiguous (due to unknown linkage phase of the genetic
markers).  The main functions are: @code{haplo.em()}, @code{haplo.glm()},
@code{haplo.score()}, and @code{haplo.power()}; all of which have detailed
examples in the vignette.")
    (license license:gpl2+)))

(define-public r-bqtl
  (package
    (name "r-bqtl")
    (version "1.0-32")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "bqtl" version))
       (sha256
        (base32
         "0jjqgsm9fmvz5nkgz608xfljjpmaf4rs4f7kxvpqn4b1l9s5lhci"))))
    (build-system r-build-system)
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "http://famprevmed.ucsd.edu/faculty/cberry/bqtl/")
    (synopsis "Bayesian QTL mapping toolkit")
    (description
     "This is a QTL mapping toolkit for inbred crosses and recombinant inbred
lines.  It includes maximum likelihood and Bayesian tools.")
    (license license:gpl2+)))

(define-public r-ibdreg
  (package
    (name "r-ibdreg")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ibdreg" version))
       (sha256
        (base32
         "1kaa5q1byi30wzr0mw4w2cv1ssxprzcwf91wrpqwkgcsdy7dkh2g"))))
    (build-system r-build-system)
    (home-page "https://www.mayo.edu/research/labs/\
statistical-genetics-genetic-epidemiology/software")
    (synopsis "Regression methods for IBD linkage with covariates")
    (description
     "This package provides a method to test genetic linkage with covariates
by regression methods with response IBD sharing for relative pairs.  Account
for correlations of IBD statistics and covariates for relative pairs within
the same pedigree.")
    (license license:gpl2+)))

(define-public r-dlmap
  (package
    (name "r-dlmap")
    (version "1.13")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "dlmap" version))
       (sha256
        (base32
         "0s6wlkggkm3qndwyvw72xv1n0mcjb7ss3ajbq2ll6rv30splq0db"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ibdreg" ,r-ibdreg)
       ("r-mgcv" ,r-mgcv)
       ("r-nlme" ,r-nlme)
       ("r-qtl" ,r-qtl)
       ("r-wgaim" ,r-wgaim)))
    (home-page "https://cran.r-project.org/web/packages/dlmap/")
    (synopsis "Detection localization mapping for QTL")
    (description
     "This is package for QTL mapping in a mixed model framework with separate
detection and localization stages.  The first stage detects the number of QTL
on each chromosome based on the genetic variation due to grouped markers on
the chromosome; the second stage uses this information to determine the most
likely QTL positions.  The mixed model can accommodate general fixed and
random effects, including spatial effects in field trials and pedigree
effects.  It is applicable to backcrosses, doubled haploids, recombinant
inbred lines, F2 intercrosses, and association mapping populations.")
    (license license:gpl2)))

(define-public r-ldheatmap
  (package
    (name "r-ldheatmap")
    (version "0.99-7")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "LDheatmap" version))
       (sha256
        (base32
         "1r0j8bihi5z1x0sgaf7dwzpsw9i0nc1vylvipvc0cia2ka1lr9dc"))))
    (properties `((upstream-name . "LDheatmap")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-genetics" ,r-genetics)
       ("r-rcpp" ,r-rcpp)
       ("r-snpstats" ,r-snpstats)))
    (home-page "http://stat.sfu.ca/statgen/research/ldheatmap.html")
    (synopsis "Graphical display of pairwise linkage disequilibria between SNPs")
    (description
     "This package provides tools to produce a graphical display, as a heat
map, of measures of pairwise linkage disequilibria between SNPs.  Users may
optionally include the physical locations or genetic map distances of each SNP
on the plot.")
    (license license:gpl3)))

(define-public r-hwde
  (package
    (name "r-hwde")
    (version "0.67")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "hwde" version))
       (sha256
        (base32
         "0wb2f9i5qi7w77ygh8bvydfpr7j5x8dyvnnhdkajaz0wdcpkyaqy"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/hwde/")
    (synopsis "Models and tests for departure from Hardy-Weinberg equilibrium")
    (description
     "This package fits models for genotypic disequilibria, as described in
Huttley and Wilson (2000), Weir (1996) and Weir and Wilson (1986).  Contrast
terms are available that account for first order interactions between loci.
It also implements, for a single locus in a single population, a conditional
exact test for Hardy-Weinberg equilibrium.")
    (license license:gpl2+)))

(define-public r-tdthap
  (package
    (name "r-tdthap")
    (version "1.1-11")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "tdthap" version))
       (sha256
        (base32
         "15qlj2bivvz3pizd8dq34wczbkbxhzqh3cqp1ixkdkprlyvcxj5k"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/tdthap/")
    (synopsis "TDT tests for extended haplotypes")
    (description
     "Functions and examples are provided for transmission/disequilibrium
tests for extended marker haplotypes, as in Clayton, D. and Jones, H. (1999)
\"Transmission/disequilibrium tests for extended marker haplotypes\".")
    (license license:artistic2.0)))

(define-public r-sparql
  (package
    (name "r-sparql")
    (version "1.16")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "SPARQL" version))
              (sha256
               (base32
                "0gak1q06yyhdmcxb2n3v0h9gr1vqd0viqji52wpw211qp6r6dcrc"))))
    (properties `((upstream-name . "SPARQL")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rcurl" ,r-rcurl)
       ("r-xml" ,r-xml)))
    (home-page "https://cran.r-project.org/web/packages/SPARQL")
    (synopsis "SPARQL client for R")
    (description "This package provides an interface to use SPARQL to pose
SELECT or UPDATE queries to an end-point.")
    ;; The only license indication is found in the DESCRIPTION file,
    ;; which states GPL-3.  So we cannot assume GPLv3+.
    (license license:gpl3)))

(define-public r-bookdown
  (package
    (name "r-bookdown")
    (version "0.18")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "bookdown" version))
              (sha256
               (base32
                "1my6g16phx21v5cvfqcnjibh3zcv02xkix347aafd6a7r3hnxpq5"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-htmltools" ,r-htmltools)
       ("r-knitr" ,r-knitr)
       ("r-rmarkdown" ,r-rmarkdown)
       ("r-tinytex" ,r-tinytex)
       ("r-xfun" ,r-xfun)
       ("pandoc" ,ghc-pandoc)))
    (home-page "https://github.com/rstudio/bookdown")
    (synopsis "Authoring books and technical documents with R markdown")
    (description "This package provides output formats and utilities for
authoring books and technical documents with R Markdown.")
    (license license:gpl3)))

(define-public r-optparse
  (package
    (name "r-optparse")
    (version "1.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "optparse" version))
       (sha256
        (base32
         "0wyrc42ja3ab5szx46zmz8lm7vzfqxkjca0m0sms8g9hqbmmay6d"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-getopt" ,r-getopt)))
    (home-page "https://github.com/trevorld/optparse")
    (synopsis "Command line option parser")
    (description
     "This package provides a command line parser inspired by Python's
@code{optparse} library to be used with Rscript to write shebang scripts
that accept short and long options.")
    (license license:gpl2+)))

(define-public r-wgcna
  (package
    (name "r-wgcna")
    (version "1.69")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "WGCNA" version))
       (sha256
        (base32
         "022hkprnrafvggi8pkjffkvk1qlnibmbbxxrni00wkrdbga5589f"))))
    (properties `((upstream-name . "WGCNA")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-annotationdbi" ,r-annotationdbi)
       ("r-doparallel" ,r-doparallel)
       ("r-dynamictreecut" ,r-dynamictreecut)
       ("r-fastcluster" ,r-fastcluster)
       ("r-foreach" ,r-foreach)
       ("r-go-db" ,r-go-db)
       ("r-hmisc" ,r-hmisc)
       ("r-impute" ,r-impute)
       ("r-rcpp" ,r-rcpp)
       ("r-survival" ,r-survival)
       ("r-matrixstats" ,r-matrixstats)
       ("r-preprocesscore" ,r-preprocesscore)))
    (home-page
     "http://www.genetics.ucla.edu/labs/horvath/CoexpressionNetwork/Rpackages/WGCNA/")
    (synopsis "Weighted correlation network analysis")
    (description
     "This package provides functions necessary to perform Weighted
Correlation Network Analysis on high-dimensional data.  It includes functions
for rudimentary data cleaning, construction and summarization of correlation
networks, module identification and functions for relating both variables and
modules to sample traits.  It also includes a number of utility functions for
data manipulation and visualization.")
    (license license:gpl2+)))

(define-public r-kernlab
  (package
    (name "r-kernlab")
    (version "0.9-29")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "kernlab" version))
       (sha256
        (base32 "0vqhndl4zm7pvkfvq0f6i9cbrm7pij6kmdp7d7w39pa100x6knn3"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/kernlab")
    (synopsis "Kernel-based machine learning tools")
    (description
     "This package provides kernel-based machine learning methods for
classification, regression, clustering, novelty detection, quantile regression
and dimensionality reduction.  Among other methods @code{kernlab} includes
Support Vector Machines, Spectral Clustering, Kernel PCA, Gaussian Processes
and a QP solver.")
    (license license:gpl2)))

(define-public r-hierfstat
  (package
    (name "r-hierfstat")
    (version "0.04-22")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "hierfstat" version))
       (sha256
        (base32
         "1fav2v2996v5kb1ffa6v5wxfm921syxg6as034vd3j4jfhdibyfx"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ade4" ,r-ade4)
       ("r-adegenet" ,r-adegenet)
       ("r-gtools" ,r-gtools)))
    (home-page "https://cran.r-project.org/web/packages/hierfstat/")
    (synopsis "Estimation and tests of hierarchical F-statistics")
    (description
     "This package allows the estimation of hierarchical F-statistics from
haploid or diploid genetic data with any numbers of levels in the hierarchy,
following the algorithm of Yang (Evolution, 1998, 52(4):950-956).  Functions
are also given to test via randomisations the significance of each F and
variance components, using the likelihood-ratio statistics G.")
    (license license:gpl2+)))

(define-public r-hapassoc
  (package
    (name "r-hapassoc")
    (version "1.2-8")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "hapassoc" version))
       (sha256
        (base32
         "0qs5jl0snzfchgpp6pabncwywxcmi743g91jvjiyyzw0lw85yv4s"))))
    (build-system r-build-system)
    (home-page "https://stat.sfu.ca/statgen/research/hapassoc.html")
    (synopsis "Inference of trait associations with SNP haplotypes")
    (description
     "Hapassoc performs likelihood inference of trait associations with
haplotypes and other covariates in @dfn{generalized linear models} (GLMs). The
functions are developed primarily for data collected in cohort or
cross-sectional studies.  They can accommodate uncertain haplotype phase and
handle missing genotypes at some SNPs.")
    (license license:gpl2)))

(define-public r-sampling
  (package
    (name "r-sampling")
    (version "2.8")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "sampling" version))
       (sha256
        (base32
         "06pj7dan0mknpsblmlnk7am78qrnwgnql5vvx7vmbfvib7rj6s9m"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-lpsolve" ,r-lpsolve)
       ("r-mass" ,r-mass)))
    (home-page "https://cran.r-project.org/web/packages/sampling/")
    (synopsis "Survey sampling")
    (description
     "This package provides functions for drawing and calibrating samples.")
    (license license:gpl2+)))

(define-public r-r2html
  (package
    (name "r-r2html")
    (version "2.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "R2HTML" version))
       (sha256
        (base32
         "00kxny7hajs9r2kw63qk7d03ggdxx2j1g8vbrmzp806y8aczvik9"))))
    (properties `((upstream-name . "R2HTML")))
    (build-system r-build-system)
    (home-page "https://github.com/nalimilan/R2HTML")
    (synopsis "HTML export for R objects")
    (description
     "This package includes HTML functions and methods to write in an HTML
file.  Thus, making HTML reports is easy.  It includes a function that allows
redirection on the fly, which appears to be very useful for teaching purposes,
as the student can keep a copy of the produced output to keep all that they
did during the course.  The package comes with a vignette describing how to
write HTML reports for statistical analysis.  Finally, a driver for Sweave
allows to parse HTML flat files containing R code and to automatically write
the corresponding outputs (tables and graphs).")
    (license license:gpl2+)))

(define-public r-rjava
  (package
    (name "r-rjava")
    (version "0.9-11")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rJava" version))
       (sha256
        (base32
         "0s9cjy1wh7snmbqwznh8f1r4ipylr7mgda4a979z963a8lqy32n2"))))
    (properties `((upstream-name . "rJava")))
    (build-system r-build-system)
    (arguments
     `(#:modules ((guix build utils)
                  (guix build r-build-system)
                  (ice-9 match))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-JAVA_HOME
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((jdk (assoc-ref inputs "jdk")))
               (setenv "JAVA_HOME" jdk)
               (setenv "JAVA" (which "java"))
               (setenv "JAR" (which "jar"))
               (setenv "JAVAC" (which "javac"))
               (setenv "JAVAH" (which "javah"))
               (setenv "JAVA_CPPFLAGS"
                       (string-append "-I" jdk "/include "
                                      "-I" jdk "/include/linux"))
               (match (find-files (string-append jdk "/jre/lib/") "libjvm.so")
                 ((lib) (setenv "JAVA_LIBS" lib))
                 (_ (error "Could not find libjvm.so"))))
             #t)))))
    (inputs
     `(("icu4c" ,icu4c)
       ("jdk" ,icedtea-8 "jdk")
       ("pcre" ,pcre)
       ("zlib" ,zlib)))
    (home-page "https://www.rforge.net/rJava/")
    (synopsis "Low-Level R to Java interface")
    (description
     "This package provides a low-level interface to the Java VM very much
like .C/.Call and friends.  It allows the creation of objects, calling methods
and accessing fields.")
    (license license:gpl2)))

(define-public r-svmisc
  (package
    (name "r-svmisc")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "svMisc" version))
       (sha256
        (base32
         "01r2a73wx2sh1njky961fxabx5wgddqqjqba6vjg0f3h8r3abmn2"))))
    (properties `((upstream-name . "svMisc")))
    (build-system r-build-system)
    (home-page "https://github.com/SciViews/svMisc")
    (synopsis "Miscellaneous functions for SciViews")
    (description
     "This package provides miscellaneous functions for SciViews or general
use, including tools to manage a temporary environment attached to the search
path for temporary variables you do not want to @code{save()} or
@code{load()}; test the current platform; showing progress bars, etc.")
    (license license:gpl2)))

(define-public r-xyz
  (package
    (name "r-xyz")
    (version "0.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "xyz" version))
       (sha256
        (base32
         "13w4sb4pvgciwr8wsz785dafj2k2kpx7znz46r5d32wx88vkycp4"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rcpp" ,r-rcpp)))
    (home-page "https://cran.r-project.org/web/packages/xyz/")
    (synopsis "Algorithm for fast interaction search in high-dimensional data")
    (description
     "High dimensional interaction search by brute force requires a quadratic
computational cost in the number of variables.  The xyz algorithm provably
finds strong interactions in almost linear time.  For details of the algorithm
see: G. Thanei, N. Meinshausen and R. Shah (2016).  The xyz algorithm for fast
interaction search in high-dimensional data.")
    ;; Any version of the GPL.
    (license license:gpl2+)))

(define-public r-rttf2pt1
  (package
    (name "r-rttf2pt1")
    (version "1.3.8")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Rttf2pt1" version))
       (sha256
        (base32
         "0b3f2zkmbyshn19cnaaf042d0zwf43l9jnkqizfhxxwb93a4c1jn"))))
    (properties `((upstream-name . "Rttf2pt1")))
    (build-system r-build-system)
    (home-page "https://github.com/wch/Rttf2pt1")
    (synopsis "Font conversion utility")
    (description
     "This package contains the program @code{ttf2pt1}, for use with the
@code{extrafont} package.")
    ;; Most of the files are covered under the Expat license.  Some files are
    ;; covered under BSD-3.  Deviations for individual files are recorded in
    ;; the LICENSE file.
    (license (list license:bsd-3 license:expat
                   (license:non-copyleft "file://LICENSE")))))

(define-public r-extrafontdb
  (package
    (name "r-extrafontdb")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "extrafontdb" version))
       (sha256
        (base32
         "115n42hfvv5h4nn4cfkfmkmn968py4lpy8zd0d6w5yylwpzbm8gs"))))
    (build-system r-build-system)
    (home-page "https://github.com/wch/extrafontdb")
    (synopsis "Database for the extrafont package")
    (description
     "This package holds the database for the @code{extrafont} package.")
    (license license:gpl2)))

(define-public r-extrafont
  (package
    (name "r-extrafont")
    (version "0.17")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "extrafont" version))
       (sha256
        (base32
         "0b9k2n9sk23bh45hjgnkxpjyvpdrz1hx7kmxvmb4nhlhm1wpsv9g"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-extrafontdb" ,r-extrafontdb)
       ("r-rttf2pt1" ,r-rttf2pt1)))
    (home-page "https://github.com/wch/extrafont")
    (synopsis "Tools for using fonts in R")
    (description
     "The extrafont package makes it easier to use fonts other than the basic
PostScript fonts that R uses.  Fonts that are imported into extrafont can be
used with PDF or PostScript output files.  There are two hurdles for using
fonts in PDF (or Postscript) output files:

@enumerate
@item Making R aware of the font and the dimensions of the characters.
@item Embedding the fonts in the PDF file so that the PDF can be displayed
  properly on a device that doesn't have the font.  This is usually needed if
  you want to print the PDF file or share it with others.
@end enumerate

The extrafont package makes both of these things easier.")
    (license license:gpl2)))

(define-public r-xkcd
  (package
    (name "r-xkcd")
    (version "0.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "xkcd" version))
       (sha256
        (base32
         "1z2y0ihn68ppay7xkglhw7djki5654g6z4bbpyy41if57z9q554f"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-extrafont" ,r-extrafont)
       ("r-ggplot2" ,r-ggplot2)
       ("r-hmisc" ,r-hmisc)))
    (home-page "https://cran.r-project.org/web/packages/xkcd/")
    (synopsis "Plot ggplot2 graphics in the XKCD style")
    (description
     "This package provides the means to plot ggplot2 graphs in the style of
the XKCD web comic.")
    (license license:gpl3)))

(define-public r-msigdbr
  (package
    (name "r-msigdbr")
    (version "7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "msigdbr" version))
       (sha256
        (base32
         "19p8z617m3my8la7n1qgb1s2msf940r372im3q30qkbcx3qxg3sd"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-dplyr" ,r-dplyr)
       ("r-magrittr" ,r-magrittr)
       ("r-rlang" ,r-rlang)
       ("r-tibble" ,r-tibble)))
    (home-page "https://github.com/igordot/msigdbr")
    (synopsis "MSigDB gene sets for multiple organisms")
    (description
     "This package provides the @dfn{Molecular Signatures Database} (MSigDB)
gene sets typically used with the @dfn{Gene Set Enrichment Analysis} (GSEA)
software in a standard R data frame with key-value pairs.  Included are the
original human gene symbols and Entrez IDs as well as the equivalents for
various frequently studied model organisms such as mouse, rat, pig, fly, and
yeast.")
    ;; The package is covered under the Expat license, but the upstream MSigDB
    ;; files are made available under the Creative Commons Attribution 4.0
    ;; International license.
    (license (list license:expat license:cc-by4.0))))

(define-public r-gridgraphics
  (package
    (name "r-gridgraphics")
    (version "0.5-0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "gridGraphics" version))
       (sha256
        (base32
         "0rlyc3xk8kfrfzbfd8n4javq3yxqd7lsfmi4q5n6s61srnrl6c1r"))))
    (properties `((upstream-name . "gridGraphics")))
    (build-system r-build-system)
    (home-page "https://github.com/pmur002/gridgraphics")
    (synopsis "Redraw base graphics using @code{grid} graphics")
    (description
     "This package provides functions to convert a page of plots drawn with
the @code{graphics} package into identical output drawn with the @code{grid}
package.  The result looks like the original @code{graphics}-based plot, but
consists of @code{grid} grobs and viewports that can then be manipulated with
@code{grid} functions (e.g., edit grobs and revisit viewports).")
    (license license:gpl2+)))

(define-public r-farver
  (package
    (name "r-farver")
    (version "2.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "farver" version))
       (sha256
        (base32
         "1k75v07dsfkjra2gsgxg9s55cw3b46b1nh960kqphq7cg7gr058f"))))
    (build-system r-build-system)
    (home-page "https://github.com/thomasp85/farver")
    (synopsis "Vectorized color conversion and comparison")
    (description
     "The encoding of color can be handled in many different ways, using
different color spaces.  As different color spaces have different uses,
efficient conversion between these representations are important.  This
package provides a set of functions that gives access to very fast color space
conversion and comparisons implemented in C++, and offers 100-fold speed
improvements over the @code{convertColor} function in the @code{grDevices}
package.")
    (license license:expat)))

(define-public r-ggplotify
  (package
    (name "r-ggplotify")
    (version "0.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ggplotify" version))
       (sha256
        (base32
         "0pfnp4lrissf21z7867kdm6slr979kchyva8iaf83i1302kscph3"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-gridgraphics" ,r-gridgraphics)
       ("r-rvcheck" ,r-rvcheck)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/GuangchuangYu/ggplotify")
    (synopsis "Convert plots to @code{grob} or @code{ggplot} object")
    (description
     "This package provides tools to convert plot function calls (using
expression or formula) to @code{grob} or @code{ggplot} objects that are
compatible with the @code{grid} and @code{ggplot2} environment.  With this
package, we are able to e.g. use @code{cowplot} to align plots produced by
@code{base} graphics, @code{grid}, @code{lattice}, @code{vcd} etc. by
converting them to @code{ggplot} objects.")
    (license license:artistic2.0)))

(define-public r-triebeard
  (package
    (name "r-triebeard")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "triebeard" version))
       (sha256
        (base32
         "1hqyz57gph02c9fdc07lxz113bbklif3g18sw8jan6pakhhdc7dz"))))
    (build-system r-build-system)
    (propagated-inputs `(("r-rcpp" ,r-rcpp)))
    (home-page "https://github.com/Ironholds/triebeard/")
    (synopsis "Radix trees in Rcpp")
    (description
     "Radix trees, or tries, are key-value data structures optimized for
efficient lookups, similar in purpose to hash tables.  This package provides
an implementation of radix trees for use in R programming and in developing
packages with Rcpp.")
    (license license:expat)))

(define-public r-tweenr
  (package
    (name "r-tweenr")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "tweenr" version))
       (sha256
        (base32
         "0sq90pbln6lkc2q3zflhkxxwpqdw5dd7igrxhdnlynkdrmi83mpg"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-farver" ,r-farver)
       ("r-magrittr" ,r-magrittr)
       ("r-rcpp" ,r-rcpp)
       ("r-rlang" ,r-rlang)))
    (home-page "https://github.com/thomasp85/tweenr")
    (synopsis "Interpolate data for smooth animations")
    (description
     "In order to create smooth animation between states of data, tweening is
necessary.  This package provides a range of functions for creating tweened
data that can be used as basis for animation.  Furthermore it adds a number of
vectorized interpolaters for common R data types such as numeric, date and
color.")
    (license license:expat)))

(define-public r-polyclip
  (package
    (name "r-polyclip")
    (version "1.10-0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "polyclip" version))
       (sha256
        (base32
         "0jyk4maqiblvj095jd59dr76kbniyli3v3xvy0a72ljszq6vrnkl"))))
    (build-system r-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://www.angusj.com/delphi/clipper.php")
    (synopsis "Polygon clipping")
    (description
     "This package provides an R port of the library Clipper.  It performs
polygon clipping operations (intersection, union, set minus, set difference)
for polygonal regions of arbitrary complexity, including holes.  It computes
offset polygons (spatial buffer zones, morphological dilations, Minkowski
dilations) for polygonal regions and polygonal lines.  It computes the
Minkowski Sum of general polygons.  There is a function for removing
self-intersections from polygon data.")
    (license license:boost1.0)))

(define-public r-urltools
  (package
    (name "r-urltools")
    (version "1.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "urltools" version))
       (sha256
        (base32
         "04x3my655dd287cbsszbnf75q0swmjlxxrblcsay7a8n3df3a830"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rcpp" ,r-rcpp)
       ("r-triebeard" ,r-triebeard)))
    (home-page "https://github.com/Ironholds/urltools/")
    (synopsis "Vectorized tools for URL handling and parsing")
    (description
     "This package provides a toolkit for all URL-handling needs, including
encoding and decoding, parsing, parameter extraction and modification.  All
functions are designed to be both fast and entirely vectorized.  It is
intended to be useful for people dealing with web-related datasets, such as
server-side logs, although may be useful for other situations involving large
sets of URLs.")
    (license license:expat)))

(define-public r-ggforce
  (package
    (name "r-ggforce")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ggforce" version))
       (sha256
        (base32
         "04926cqrda6psvy2nzkkw4czwyxdp7fnxg76byp14v12kgd72lm0"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-gtable" ,r-gtable)
       ("r-mass" ,r-mass)
       ("r-polyclip" ,r-polyclip)
       ("r-rcpp" ,r-rcpp)
       ("r-rcppeigen" ,r-rcppeigen)
       ("r-rlang" ,r-rlang)
       ("r-scales" ,r-scales)
       ("r-tidyselect" ,r-tidyselect)
       ("r-tweenr" ,r-tweenr)
       ("r-withr" ,r-withr)))
    (home-page "https://ggforce.data-imaginist.com")
    (synopsis "Accelerating ggplot2")
    (description
     "The aim of the ggplot2 package is to aid in visual data investigations.
This focus has led to a lack of facilities for composing specialized plots.
Thi package aims to be a collection of mainly new statistics and geometries
that fills this gap.")
    (license license:expat)))

(define-public r-europepmc
  (package
    (name "r-europepmc")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "europepmc" version))
       (sha256
        (base32
         "1ngqs1sqzkbwv98dd5z4cxj8bnz41wyd0g060a2vpqi3s99s4i2h"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-dplyr" ,r-dplyr)
       ("r-httr" ,r-httr)
       ("r-jsonlite" ,r-jsonlite)
       ("r-plyr" ,r-plyr)
       ("r-progress" ,r-progress)
       ("r-purrr" ,r-purrr)
       ("r-urltools" ,r-urltools)
       ("r-xml2" ,r-xml2)))
    (home-page "https://github.com/ropensci/europepmc/")
    (synopsis "R Interface to the Europe PubMed Central RESTful Web Service")
    (description
     "This package provides an R Client for the
@url{https://europepmc.org/RestfulWebService,Europe PubMed Central RESTful Web
Service}.  It gives access to both metadata on life science literature and
open access full texts.  Europe PMC indexes all PubMed content and other
literature sources including Agricola, a bibliographic database of citations
to the agricultural literature, or Biological Patents.  In addition to
bibliographic metadata, the client allows users to fetch citations and
reference lists.  Links between life-science literature and other EBI
databases, including ENA, PDB or ChEMBL are also accessible.")
    (license license:gpl3)))

(define-public r-ggraph
  (package
    (name "r-ggraph")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ggraph" version))
       (sha256
        (base32
         "0q2yqsb981a2arf9j530kqs61vw38ixyxbhb6yvv7skgbck02r6y"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-digest" ,r-digest)
       ("r-dplyr" ,r-dplyr)
       ("r-ggforce" ,r-ggforce)
       ("r-ggplot2" ,r-ggplot2)
       ("r-ggrepel" ,r-ggrepel)
       ("r-graphlayouts" ,r-graphlayouts)
       ("r-gtable" ,r-gtable)
       ("r-igraph" ,r-igraph)
       ("r-mass" ,r-mass)
       ("r-rcpp" ,r-rcpp)
       ("r-rlang" ,r-rlang)
       ("r-scales" ,r-scales)
       ("r-tidygraph" ,r-tidygraph)
       ("r-viridis" ,r-viridis)))
    (home-page "https://cran.r-project.org/web/packages/ggraph/")
    (synopsis "Implementation of grammar of graphics for graphs and networks")
    (description
     "The grammar of graphics as implemented in ggplot2 is a poor fit for
graph and network visualizations due to its reliance on tabular data input.
The ggraph package is an extension of the ggplot2 API tailored to graph
visualizations and provides the same flexible approach to building up plots
layer by layer.")
    (license license:gpl3)))

(define-public r-varselrf
  (package
    (name "r-varselrf")
    (version "0.7-8")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "varSelRF" version))
       (sha256
        (base32
         "0h49rl1j13yfh97rsfsyh9s2c4wajny4rzms2qw77d0cavxqg53i"))))
    (properties `((upstream-name . "varSelRF")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-randomforest" ,r-randomforest)))
    (home-page "https://www.ligarto.org/rdiaz/software/software")
    (synopsis "Variable selection using random forests")
    (description
     "This package provides tools for the variable selection from random
forests using both backwards variable elimination (for the selection of small
sets of non-redundant variables) and selection based on the importance
spectrum (somewhat similar to scree plots; for the selection of large,
potentially highly-correlated variables).  The main applications are in
high-dimensional data (e.g., microarray data, and other genomics and
proteomics applications).")
    (license license:gpl2+)))

(define-public r-pamr
  (package
    (name "r-pamr")
    (version "1.56.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "pamr" version))
       (sha256
        (base32
         "0ycpgkk23y3zzkb42n2skcyl35ps1n7jmyzfj7pbxr3f6gr2grfh"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-cluster" ,r-cluster)
       ("r-survival" ,r-survival)))
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/pamr/")
    (synopsis "Prediction Analysis for Microarrays")
    (description
     "This package provides some functions for sample classification in
microarrays.")
    (license license:gpl2)))

(define-public r-rda
  (package
    (name "r-rda")
    (version "1.0.2-2.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rda" version))
       (sha256
        (base32
         "1y4fawslr3i6crjaxhsdb47kfsqkyszdx6avq3r5far5a4pvc639"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/rda/")
    (synopsis "Shrunken centroids regularized discriminant analysis")
    (description
     "This package provides tools for shrunken centroids regularized
discriminant analysis for the purpose of classifying high dimensional data.")
    (license license:gpl2+)))

(define-public r-ggvis
  (package
    (name "r-ggvis")
    (version "0.4.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ggvis" version))
       (sha256
        (base32
         "091i9f17912j8qcyxppjgwzjnyqj7769ixs9d2gjg6f2clskqdw2"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-assertthat" ,r-assertthat)
       ("r-dplyr" ,r-dplyr)
       ("r-htmltools" ,r-htmltools)
       ("r-jsonlite" ,r-jsonlite)
       ("r-lazyeval" ,r-lazyeval)
       ("r-magrittr" ,r-magrittr)
       ("r-shiny" ,r-shiny)))
    (home-page "https://ggvis.rstudio.com/")
    (synopsis "Interactive grammar of graphics")
    (description
     "This package is a data visualization package for R providing an
implementation of an interactive grammar of graphics, taking the best parts of
ggplot2, combining them with the reactive framework of Shiny and drawing web
graphics using Vega.")
    (license license:gpl2)))

(define-public r-gbm
  (package
    (name "r-gbm")
    (version "2.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "gbm" version))
       (sha256
        (base32
         "0vs6ljaqhwwpgr8wlbhmm4v147rd82kl16rpaijqiylxcc8dxyq6"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-gridextra" ,r-gridextra)
       ("r-lattice" ,r-lattice)
       ("r-survival" ,r-survival)))
    (home-page "https://github.com/gbm-developers/gbm")
    (synopsis "Generalized boosted regression models")
    (description
     "This package is an implementation of extensions to Freund and Schapire's
AdaBoost algorithm and Friedman's gradient boosting machine.  It includes
regression methods for least squares, absolute loss, t-distribution loss,
quantile regression, logistic, multinomial logistic, Poisson, Cox proportional
hazards partial likelihood, AdaBoost exponential loss, Huberized hinge loss,
and Learning to Rank measures (LambdaMart).")
    (license license:gpl2+)))

(define-public r-threejs
  (package
    (name "r-threejs")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "threejs" version))
       (sha256
        (base32
         "1711h351nzxfkbbdwvfzyhciyvi9c6wx3jq1g97lzcqgnb45kivn"))))
    (build-system r-build-system)
    (arguments
     `(#:modules ((guix build utils)
                  (guix build r-build-system)
                  (srfi srfi-1)
                  (ice-9 popen))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'process-javascript
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "inst"
               (call-with-values
                   (lambda ()
                     (unzip2
                      `((,(assoc-ref inputs "js-jquery")
                         "htmlwidgets/lib/jquery/jquery.min.js")
                        (,(assoc-ref inputs "js-threejs-111")
                         "htmlwidgets/lib/threejs-111/three.min.js"))))
                 (lambda (sources targets)
                   (for-each (lambda (source target)
                               (format #t "Processing ~a --> ~a~%"
                                       source target)
                               (delete-file target)
                               (let ((minified (open-pipe* OPEN_READ "uglify-js" source)))
                                 (call-with-output-file target
                                   (lambda (port)
                                     (dump-port minified port)))))
                             sources targets))))
             #t)))))
    (propagated-inputs
     `(("r-base64enc" ,r-base64enc)
       ("r-crosstalk" ,r-crosstalk)
       ("r-htmlwidgets" ,r-htmlwidgets)
       ("r-igraph" ,r-igraph)))
    (native-inputs
     `(("uglify-js" ,uglify-js)
       ("js-jquery"
        ,(origin
           (method url-fetch)
           (uri "https://code.jquery.com/jquery-1.12.4.js")
           (sha256
            (base32
             "0x9mrc1668icvhpwzvgafm8xm11x9lfai9nwr66aw6pjnpwkc3s3"))))
       ("js-threejs-111"
        ,(origin
           (method url-fetch)
           (uri "https://raw.githubusercontent.com/mrdoob/three.js/r111/build/three.js")
           (sha256
            (base32
             "1cxdkw3plmlw1xvhbx5dm39gqczgzxip2dm887v6whhsxqxl9cky"))))))
    (home-page "https://bwlewis.github.io/rthreejs")
    (synopsis "Interactive 3D scatter plots, networks and globes")
    (description
     "Create interactive 3D scatter plots, network plots, and globes in R
using the three.js visualization library.")
    (license license:expat)))

(define-public r-mlbench
  (package
    (name "r-mlbench")
    (version "2.1-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "mlbench" version))
       (sha256
        (base32
         "1rp035qxfgh5ail92zjh9jh57dj0b8babw3wsg29v8ricpal30bl"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/mlbench/")
    (synopsis "Machine learning benchmark problems")
    (description
     "This package provides a collection of artificial and real-world machine
learning benchmark problems, including, e.g., several data sets from the UCI
repository.")
    (license license:gpl2)))

(define-public r-mpm
  (package
    (name "r-mpm")
    (version "1.0-22")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "mpm" version))
       (sha256
        (base32
         "0wijw8v0wmbfrda5564cmnp788qmlkk21yn5cp5qk8aprm9l1fnk"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-kernsmooth" ,r-kernsmooth)
       ("r-mass" ,r-mass)))
    (home-page "http://mpm.r-forge.r-project.org")
    (synopsis "Multivariate projection methods")
    (description
     "This is a package for exploratory graphical analysis of multivariate
data, specifically gene expression data with different projection methods:
principal component analysis, correspondence analysis, spectral map
analysis.")
    (license license:gpl2+)))

(define-public r-png
  (package
    (name "r-png")
    (version "0.1-7")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "png" version))
              (sha256
               (base32
                "0g2mcp55lvvpx4kd3mn225mpbxqcq73wy5qx8b4lyf04iybgysg2"))))
    (build-system r-build-system)
    (inputs
     `(("libpng" ,libpng)
       ("zlib" ,zlib)))
    (home-page "https://www.rforge.net/png/")
    (synopsis "Read and write PNG images")
    (description
     "This package provides an easy and simple way to read, write and display
bitmap images stored in the PNG format.  It can read and write both files and
in-memory raw vectors.")
    ;; Any of these GPL versions.
    (license (list license:gpl2 license:gpl3))))

(define-public r-ggcorrplot
  (package
    (name "r-ggcorrplot")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ggcorrplot" version))
       (sha256
        (base32
         "0hi9lz121ya1l2lbm7rqlxg6fs6bvxck396dngnidrhl5fvqb41b"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-reshape2" ,r-reshape2)))
    (home-page "http://www.sthda.com/english/wiki/ggcorrplot")
    (synopsis "Visualization of a correlation matrix using ggplot2")
    (description
     "The ggcorrplot package can be used to visualize easily a correlation
matrix using ggplot2.  It provides a solution for reordering the correlation
matrix and displays the significance level on the plot.  It also includes a
function for computing a matrix of correlation p-values.")
    (license license:gpl2)))

(define-public r-flexdashboard
  (package
    (name "r-flexdashboard")
    (version "0.5.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "flexdashboard" version))
       (sha256
        (base32
         "0fy3nbrr67zqgd44r2mc850s5sp0hzfcw3zqs15m8kxzj1aw067x"))))
    (build-system r-build-system)
    (arguments
     `(#:modules ((guix build utils)
                  (guix build r-build-system)
                  (srfi srfi-1)
                  (srfi srfi-26)
                  (ice-9 popen)
                  (ice-9 textual-ports))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'process-javascript
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "inst"
               ;; Concatenate all components of prism.js
               (let ((contents (string-join
                                (map (lambda (name)
                                       (call-with-input-file
                                           (assoc-ref inputs name)
                                         get-string-all))
                                     (list "js-prism"
                                           "js-prism-r"
                                           "js-prism-line-numbers"))
                                "\n")))
                 (call-with-output-file "prism-src.js"
                   (cut display contents <>)))
               (call-with-values
                   (lambda ()
                     (unzip2
                      `(("www/stickytableheaders/jquery.stickytableheaders.js"
                         "www/stickytableheaders/jquery.stickytableheaders.min.js")
                        ("www/sly/sly.js"
                         "www/sly/sly.min.js")
                        ("prism-src.js"
                         "www/prism/prism.js")
                        (,(assoc-ref inputs "js-raphael")
                         "htmlwidgets/lib/raphael/raphael-2.1.4.min.js")
                        (,(assoc-ref inputs "js-featherlight")
                         "www/featherlight/featherlight.min.js"))))
                 (lambda (sources targets)
                   (for-each (lambda (source target)
                               (format #t "Processing ~a --> ~a~%"
                                       source target)
                               (delete-file target)
                               (let ((minified (open-pipe* OPEN_READ "uglify-js" source)))
                                 (call-with-output-file target
                                   (lambda (port)
                                     (dump-port minified port)))))
                             sources targets))))
             #t)))))
    (propagated-inputs
     `(("r-htmltools" ,r-htmltools)
       ("r-htmlwidgets" ,r-htmlwidgets)
       ("r-jsonlite" ,r-jsonlite)
       ("r-knitr" ,r-knitr)
       ("r-rmarkdown" ,r-rmarkdown)
       ("r-shiny" ,r-shiny)))
    (native-inputs
     `(("uglify-js" ,uglify-js)
       ("js-raphael"
        ,(origin
           (method url-fetch)
           (uri "https://raw.githubusercontent.com/DmitryBaranovskiy/raphael/v2.1.4/raphael.js")
           (sha256
            (base32
             "1h4c4akrgcj7wra9j1z1rv2406j0yf68y9c0wg8v7w9ibw2iwf1x"))))
       ("js-prism"
        ,(origin
           (method url-fetch)
           (uri "https://raw.githubusercontent.com/PrismJS/prism/v1.16.0/prism.js")
           (sha256
            (base32
             "0gqa9irbp9k8p5r3d98cszajzhjnssnl43nrsc5aiy7ki52z500c"))))
       ("js-prism-r"
        ,(origin
           (method url-fetch)
           (uri "https://raw.githubusercontent.com/PrismJS/prism/v1.16.0/components/prism-r.js")
           (sha256
            (base32
             "1x31glci7wdgr2305njy0bm2lncb0jyn0j1s2g72rqi29xid9aki"))))
       ("js-prism-line-numbers"
        ,(origin
           (method url-fetch)
           (uri "https://raw.githubusercontent.com/PrismJS/prism/v1.16.0/plugins/line-numbers/prism-line-numbers.js")
           (sha256
            (base32
             "1543wgf3iynrilyb27jq8px3h5gvfz5xmdib5ik2ki400c1sl991"))))
       ("js-featherlight"
        ,(origin
           (method url-fetch)
           (uri "https://raw.githubusercontent.com/noelboss/featherlight/1.3.4/src/featherlight.js")
           (sha256
            (base32
             "14kkhwzvp8rxq2mrck5i0xcm8v5rqwqhwnmncbng8h4qq42zx3sb"))))))
    (home-page "https://rmarkdown.rstudio.com/flexdashboard")
    (synopsis "R Markdown format for flexible dashboards")
    (description
     "This package provides an R Markdown format for converting an R Markdown
document to a grid-oriented dashboard.  The dashboard flexibly adapts the size
of its components to the containing web page.")
    (license license:expat)))

(define-public r-preseqr
  (package
    (name "r-preseqr")
    (version "4.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "preseqR" version))
       (sha256
        (base32
         "1g2rnnmi45649vpy6z45v5i3wxm54s138ajqrzwi3a5r7x3xnhq1"))))
    (properties `((upstream-name . "preseqR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-polynom" ,r-polynom)))
    (home-page "https://cran.r-project.org/web/packages/preseqR/")
    (synopsis "Predicting species accumulation curves")
    (description
     "This package can be used to predict the r-species accumulation
curve (r-SAC), which is the number of species represented at least r times as
a function of the sampling effort.  When r = 1, the curve is known as the
species accumulation curve, or the library complexity curve in high-throughput
genomic sequencing.  The package includes both parametric and nonparametric
methods, as described by Deng C, et al. (2018).")
    (license license:gpl3)))

(define-public r-mapplots
  (package
    (name "r-mapplots")
    (version "1.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "mapplots" version))
       (sha256
        (base32
         "18s2y66f8vi8g2r8a25zbgp2xm079r8v8qxv0w71h8krycs6vs9p"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/mapplots/")
    (synopsis "Data visualization on maps")
    (description
     "This package helps you create simple maps; add sub-plots like pie plots
to a map or any other plot; format, plot and export gridded data.  The package
was developed for displaying fisheries data but most functions can be used for
more generic data visualisation.")
    (license license:gpl2+)))

(define-public r-pmcmr
  (package
    (name "r-pmcmr")
    (version "4.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "PMCMR" version))
       (sha256
        (base32
         "09bvdj2h1086r2cgy3myrhlylplxxlliv8nwx09c8kb1vn02i2ij"))))
    (properties `((upstream-name . "PMCMR")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/PMCMR/")
    (synopsis "Calculate pairwise multiple comparisons of mean rank sums")
    (description
     "This is a deprecated package for calculating pairwise multiple
comparisons of mean rank sums.  This package is superseded by the novel
PMCMRplus package.  The PMCMR package is no longer maintained, but kept for
compatibility of dependent packages for some time.")
    (license license:gpl3+)))

(define-public r-downloader
  (package
    (name "r-downloader")
    (version "0.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "downloader" version))
       (sha256
        (base32
         "1axggnsc27zzgr7snf41j3zd1vp3nfpmq4zj4d01axc709dyg40q"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-digest" ,r-digest)))
    (home-page "https://github.com/wch/downloader")
    (synopsis "Download files over HTTP and HTTPS")
    (description
     "This package provides a wrapper for the @code{download.file} function,
making it possible to download files over HTTPS across platforms.  The
@code{RCurl} package provides this functionality (and much more) but has
external dependencies.  This package has is implemented purely in R.")
    (license license:gpl2)))

(define-public r-rex
  (package
    (name "r-rex")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rex" version))
       (sha256
        (base32
         "0alsadgjgass3wr8y5d247j12qqzg454sc84vpskclrkmz778g5x"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-lazyeval" ,r-lazyeval)
       ("r-magrittr" ,r-magrittr)))
    (home-page "https://github.com/kevinushey/rex")
    (synopsis "Friendly regular expressions")
    (description
     "This package provides a friendly interface for the construction of
regular expressions.  Regular expressions are a very powerful feature, however
they are often difficult to interpret.  Rex allows you to build complex
regular expressions from human readable expressions")
    (license license:expat)))

(define-public r-xmlparsedata
  (package
    (name "r-xmlparsedata")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "xmlparsedata" version))
       (sha256
        (base32
         "0gjr3l5z5dp276lchr2649as1rkj56d2mlvbr66yg393zzw50lsh"))))
    (properties `((upstream-name . "xmlparsedata")))
    (build-system r-build-system)
    (home-page "https://github.com/r-lib/xmlparsedata#readme")
    (synopsis "Parse data of @code{R} code as an @code{XML} tree")
    (description
     "This package provides tools to convert the output of
@code{utils::getParseData()} to an @code{XML} tree, that one can search via
@code{XPath}, and is easier to manipulate in general.")
    (license license:expat)))

(define-public r-cyclocomp
  (package
    (name "r-cyclocomp")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "cyclocomp" version))
       (sha256
        (base32
         "0gky3svk02wiajw7nfjh30684h3qxili4bvsab0m7b6cggw6bgyd"))))
    (properties `((upstream-name . "cyclocomp")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-callr" ,r-callr)
       ("r-crayon" ,r-crayon)
       ("r-desc" ,r-desc)
       ("r-remotes" ,r-remotes)
       ("r-withr" ,r-withr)))
    (home-page "https://github.com/MangoTheCat/cyclocomp")
    (synopsis "Cyclomatic complexity of R code")
    (description
     "Cyclomatic complexity is a software metric, used to indicate the
complexity of a program.  It is a quantitative measure of the number of
linearly independent paths through a program's source code.  This package
provides tools to compute this metric.")
    (license license:expat)))

(define-public r-lintr
  (package
    (name "r-lintr")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "lintr" version))
       (sha256
        (base32
         "14yfh641afg6griaadbdciyr3k94fl55s055qwzghgk5gdsj61zy"))))
    (properties `((upstream-name . "lintr")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-codetools" ,r-codetools)
       ("r-crayon" ,r-crayon)
       ("r-cyclocomp" ,r-cyclocomp)
       ("r-digest" ,r-digest)
       ("r-httr" ,r-httr)
       ("r-jsonlite" ,r-jsonlite)
       ("r-knitr" ,r-knitr)
       ("r-rex" ,r-rex)
       ("r-rstudioapi" ,r-rstudioapi)
       ("r-testthat" ,r-testthat)
       ("r-xml2" ,r-xml2)
       ("r-xmlparsedata" ,r-xmlparsedata)))
    (home-page "https://github.com/jimhester/lintr")
    (synopsis "Linter for R code")
    (description "This package checks adherence to a given style, syntax
errors and possible semantic issues.  It supports on the fly checking of R
code edited with @code{RStudio IDE}, @code{Emacs} and @code{Vim}.")
    (license license:expat)))

(define-public r-sctransform
  (package
    (name "r-sctransform")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "sctransform" version))
       (sha256
        (base32
         "07v3lzccsrkh1glfxd1q20r8f8gl9ls5az0s1dvxm4vcls0hlhyn"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-future-apply" ,r-future-apply)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gridextra" ,r-gridextra)
       ("r-mass" ,r-mass)
       ("r-matrix" ,r-matrix)
       ("r-rcpp" ,r-rcpp)
       ("r-rcppeigen" ,r-rcppeigen)
       ("r-reshape2" ,r-reshape2)))
    (home-page "https://github.com/ChristophH/sctransform")
    (synopsis "Variance stabilizing transformations for Single Cell UMI Data")
    (description
     "This package provides a normalization method for single-cell UMI count
data using a variance stabilizing transformation.  The transformation is based
on a negative binomial regression model with regularized parameters.  As part
of the same regression framework, this package also provides functions for
batch correction, and data correction.")
    (license license:gpl3)))

(define-public r-styler
  (package
    (name "r-styler")
    (version "1.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "styler" version))
       (sha256
        (base32
         "1waglhsy2c53qjgd2qhlzda3z0lbzbwx9fkrfhac41y6h91mgkrz"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-backports" ,r-backports)
       ("r-cli" ,r-cli)
       ("r-magrittr" ,r-magrittr)
       ("r-purrr" ,r-purrr)
       ("r-r-cache" ,r-r-cache)
       ("r-rematch2" ,r-rematch2)
       ("r-rlang" ,r-rlang)
       ("r-rprojroot" ,r-rprojroot)
       ("r-tibble" ,r-tibble)
       ("r-withr" ,r-withr)
       ("r-xfun" ,r-xfun)))
    (home-page "https://github.com/r-lib/styler")
    (synopsis "Non-invasive pretty printing of R code")
    (description
     "This is a package for pretty-printing R code without changing the user's
formatting intent.")
    (license license:gpl3)))

(define-public r-scrime
  (package
    (name "r-scrime")
    (version "1.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "scrime" version))
       (sha256
        (base32
         "0y2mh9fsffjf3i15bafpasa17z99c1s75r8g6h4hgcwfgpjx75sx"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/scrime/")
    (synopsis "Analysis of high-dimensional categorical data such as SNP data")
    (description
     "This package provides tools for the analysis of high-dimensional data
developed/implemented at the group \"Statistical Complexity Reduction In
Molecular Epidemiology\" (SCRIME).  The main focus is on SNP data, but most of
the functions can also be applied to other types of categorical data.")
    (license license:gpl2)))

(define-public r-pbmcapply
  (package
    (name "r-pbmcapply")
    (version "1.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "pbmcapply" version))
       (sha256
        (base32
         "0i58gcqpnbyvc448qfgm45b7rpbmrnagsvk1h1hsqchbbicfslnz"))))
    (build-system r-build-system)
    (home-page "https://github.com/kvnkuang/pbmcapply")
    (synopsis "Track the progress of apply procedures with a progress bar")
    (description
     "This light-weight package helps you track and visualize the progress of
parallel versions of vectorized R functions of the @code{mc*apply} family.")
    (license license:expat)))

(define-public r-blme
  (package
    (name "r-blme")
    (version "1.0-4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "blme" version))
       (sha256
        (base32
         "1ca2b0248k0fj3lczn9shfjplz1sl4ay4v6djldizp2ch2vwdgy2"))))
    (build-system r-build-system)
    (propagated-inputs `(("r-lme4" ,r-lme4)))
    (home-page "https://github.com/vdorie/blme")
    (synopsis "Bayesian linear mixed-effects models")
    (description
     "This package provides tools for maximum a posteriori estimation for
linear and generalized linear mixed-effects models in a Bayesian setting.  It
extends the lme4 package.")
    (license license:gpl2+)))

(define-public r-batchtools
  (package
    (name "r-batchtools")
    (version "0.9.12")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "batchtools" version))
       (sha256
        (base32
         "16x524hvy9d8p7r4fi1c8mixcvzgsjbf3y0vxaa56ssbbab4p7f9"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-backports" ,r-backports)
       ("r-base64url" ,r-base64url)
       ("r-brew" ,r-brew)
       ("r-checkmate" ,r-checkmate)
       ("r-data-table" ,r-data-table)
       ("r-digest" ,r-digest)
       ("r-fs" ,r-fs)
       ("r-progress" ,r-progress)
       ("r-r6" ,r-r6)
       ("r-rappdirs" ,r-rappdirs)
       ("r-stringi" ,r-stringi)
       ("r-withr" ,r-withr)))
    (home-page "https://github.com/mllg/batchtools")
    (synopsis "Tools for computation on batch systems")
    (description
     "As a successor of the packages BatchJobs and BatchExperiments, this
package provides a parallel implementation of the Map function for high
performance computing systems managed by various schedulers.  A multicore and
socket mode allow the parallelization on a local machines, and multiple
machines can be hooked up via SSH to create a makeshift cluster.  Moreover,
the package provides an abstraction mechanism to define large-scale computer
experiments in a well-organized and reproducible way.")
    (license license:lgpl3)))

(define-public r-clue
  (package
    (name "r-clue")
    (version "0.3-57")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "clue" version))
       (sha256
        (base32
         "05rdcahawxlxci3fjxihjvvh33wqpxw50sx015165ab4nh3rsdkf"))))
    (build-system r-build-system)
    (propagated-inputs `(("r-cluster" ,r-cluster)))
    (home-page "https://cran.r-project.org/web/packages/clue/")
    (synopsis "Tools for analyzing cluster ensembles")
    (description "Cluster ensembles are collections of individual solutions to
a given clustering problem which are useful or necessary to consider in a wide
range of applications.  This R package provides an extensible computational
environment for creating and analyzing cluster ensembles, with basic data
structures for representing partitions and hierarchies, and facilities for
computing on them, including methods for measuring proximity and obtaining
consensus and secondary clusterings.")
    (license license:gpl2)))

(define-public r-sitmo
  (package
    (name "r-sitmo")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "sitmo" version))
       (sha256
        (base32
         "0apdhwy3kxs39agsbvx5vn3xsgb22bf3jrwmr2cmqk9kmxbx740c"))))
    (build-system r-build-system)
    (propagated-inputs `(("r-rcpp" ,r-rcpp)))
    (home-page "https://github.com/coatless/sitmo/")
    (synopsis "Parallel pseudo random number generator header files")
    (description
     "This package provides two high quality and fast PPRNGs that may be used
in an OpenMP parallel environment.  In addition, there is a generator for one
dimensional low-discrepancy sequence.")
    (license license:expat)))

(define-public r-dqrng
  (package
    (name "r-dqrng")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "dqrng" version))
       (sha256
        (base32
         "0rp8q5zijlvaqmpnkwr314w9w40sj4fz7sqsdgsffcfvn42w2jg1"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bh" ,r-bh)
       ("r-rcpp" ,r-rcpp)
       ("r-sitmo" ,r-sitmo)))
    (home-page "https://www.daqana.org/dqrng")
    (synopsis "Fast pseudo random number generators")
    (description
     "Several fast random number generators are provided as C++ header-only
libraries: the PCG family as well as Xoroshiro128+ and Xoshiro256+.
Additionally, fast functions for generating random numbers according to a
uniform, normal and exponential distribution are included.  The latter two use
the Ziggurat algorithm originally proposed by Marsaglia and Tsang.  These
functions are exported to R and as a C++ interface and are enabled for use
with the default 64 bit generator from the PCG family, Xoroshiro128+ and
Xoshiro256+ as well as the 64 bit version of the 20 rounds Threefry
engine (Salmon et al., 2011) as provided by the package @code{sitmo}.")
    ;; This package includes code under CC0 and Apache 2.0 or Expat, but as a
    ;; whole is distributed under the terms of the AGPL 3.
    (license license:agpl3)))

(define-public r-dalex
  (package
    (name "r-dalex")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "DALEX" version))
       (sha256
        (base32
         "1jbyn57vn6d281a5y15h5d8ljin8cdb9lr7lbgggc950blyfv4g0"))))
    (properties `((upstream-name . "DALEX")))
    (build-system r-build-system)
    (propagated-inputs `(("r-ggplot2" ,r-ggplot2)))
    (home-page "https://pbiecek.github.io/DALEX/")
    (synopsis "Descriptive machine learning explanations")
    (description
     "Machine Learning models are widely used and have various applications in
classification or regression.  Models created with boosting, bagging, stacking
or similar techniques are often used due to their high performance, but such
black-box models usually lack interpretability.  The DALEX package contains
various explainers that help to understand the link between input variables
and model output.")
    ;; Any version of the GPL
    (license license:gpl3+)))

(define-public r-enrichr
  (package
    (name "r-enrichr")
    (version "2.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "enrichR" version))
       (sha256
        (base32
         "0ymhzs9d2wl0s9rvbqc1hqb78mlzwhlc7mmijpfqkm5r720pf6m1"))))
    (properties `((upstream-name . "enrichR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-httr" ,r-httr)
       ("r-rjson" ,r-rjson)))
    (home-page "https://cran.r-project.org/web/packages/enrichR/")
    (synopsis "R Interface to Enrichr database for analyzing gene sets")
    (description
     "This package provides an R interface to all Enrichr databases, a
web-based tool for analyzing gene sets and returns any enrichment of common
annotated biological functions.")
    (license license:gpl2+)))

(define-public r-plot3d
  (package
    (name "r-plot3d")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "plot3D" version))
       (sha256
        (base32
         "1jfzndnlci9a975qbcv66jiy2l46hs3f2049654x4jp3i4kyrr5r"))))
    (properties `((upstream-name . "plot3D")))
    (build-system r-build-system)
    (propagated-inputs `(("r-misc3d" ,r-misc3d)))
    (home-page "https://cran.r-project.org/web/packages/plot3D")
    (synopsis "Plot multi-dimensional data")
    (description
     "This package provides functions for viewing 2D and 3D data, including
perspective plots, slice plots, surface plots, scatter plots, etc.  It
includes data sets from oceanography.")
    (license license:gpl3+)))

(define-public r-ggfortify
  (package
    (name "r-ggfortify")
    (version "0.4.9")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ggfortify" version))
       (sha256
        (base32
         "1p6knrbyaynaqwd939w09hpf1zz1gn95cb46sfgppl8l98krb2h5"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-dplyr" ,r-dplyr)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gridextra" ,r-gridextra)
       ("r-scales" ,r-scales)
       ("r-stringr" ,r-stringr)
       ("r-tibble" ,r-tibble)
       ("r-tidyr" ,r-tidyr)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/sinhrks/ggfortify")
    (synopsis "Data visualization tools for statistical analysis results")
    (description
     "This package provides unified plotting tools for statistics commonly
used, such as GLM, time series, PCA families, clustering and survival
analysis.  The package offers a single plotting interface for these analysis
results and plots in a unified style using the @code{ggplot2} package.")
    (license license:gpl2)))

(define-public r-refmanager
  (package
    (name "r-refmanager")
    (version "1.2.12")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "RefManageR" version))
       (sha256
        (base32
         "1hfxa1qacfryk36mpaqdhdgws5jwxiyy489ikd3wa18bp1wz8dkp"))))
    (properties `((upstream-name . "RefManageR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bibtex" ,r-bibtex)
       ("r-httr" ,r-httr)
       ("r-jsonlite" ,r-jsonlite)
       ("r-lubridate" ,r-lubridate)
       ("r-plyr" ,r-plyr)
       ("r-stringr" ,r-stringr)
       ("r-xml2" ,r-xml2)))
    (home-page "https://github.com/ropensci/RefManageR/")
    (synopsis "Straightforward BibTeX and BibLaTeX bibliography management")
    (description
     "This package provides tools for importing and working with bibliographic
references.  It greatly enhances the @code{bibentry} class by providing a
class @code{BibEntry} which stores BibTeX and BibLaTeX references, supports
UTF-8 encoding, and can be easily searched by any field, by date ranges, and
by various formats for name lists (author by last names, translator by full
names, etc.).  Entries can be updated, combined, sorted, printed in a number
of styles, and exported.  BibTeX and BibLaTeX @code{.bib} files can be read
into R and converted to @code{BibEntry} objects.")
    ;; Any of these licenses may be picked.
    (license (list license:gpl2 license:gpl3 license:bsd-3))))

(define-public r-citr
  (package
    (name "r-citr")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "citr" version))
       (sha256
        (base32
         "1qbarvafjb8jgkrnrhh6jw7mcglmjwf7dpdiibxf39jkmlhf7las"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-assertthat" ,r-assertthat)
       ("r-curl" ,r-curl)
       ("r-httr" ,r-httr)
       ("r-miniui" ,r-miniui)
       ("r-refmanager" ,r-refmanager)
       ("r-rstudioapi" ,r-rstudioapi)
       ("r-shiny" ,r-shiny)
       ("r-shinyjs" ,r-shinyjs)
       ("r-yaml" ,r-yaml)))
    (home-page "https://github.com/crsh/citr")
    (synopsis "RStudio add-in to insert Markdown citations")
    (description
     "This package provides functions and an RStudio add-in that search a
BibTeX or BibLaTeX file to create and insert formatted Markdown citations into
the current document.")
    (license license:expat)))

(define-public r-xgboost
  (package
    (name "r-xgboost")
    (version "0.90.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "xgboost" version))
       (sha256
        (base32
         "1gy9rzg43mjpfis893vf15drmbigfn0481zrzss9ajnmnk0q8194"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-data-table" ,r-data-table)
       ("r-magrittr" ,r-magrittr)
       ("r-matrix" ,r-matrix)
       ("r-stringi" ,r-stringi)))
    (home-page "https://github.com/dmlc/xgboost")
    (synopsis "Extreme gradient boosting")
    (description
     "This package provides an R interface to Extreme Gradient Boosting, which
is an efficient implementation of the gradient boosting framework from Chen
and Guestrin (2016).  The package includes efficient linear model solver and
tree learning algorithms.  The package can automatically do parallel
computation on a single machine.  It supports various objective functions,
including regression, classification and ranking.  The package is made to be
extensible, so that users are also allowed to define their own objectives
easily.")
    (license license:asl2.0)))

(define-public r-umap
  (package
    (name "r-umap")
    (version "0.2.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "umap" version))
       (sha256
        (base32
         "0qp8zbh6fn8kn6q2h2lyjgmq3pr6gqwsd8ymqx25px13zjhxch9d"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-openssl" ,r-openssl)
       ("r-rcpp" ,r-rcpp)
       ("r-reticulate" ,r-reticulate)
       ("r-rspectra" ,r-rspectra)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/tkonopka/umap")
    (synopsis "Uniform manifold approximation and projection")
    (description
     "Uniform manifold approximation and projection is a technique for
dimension reduction.  This package provides an interface to the UMAP algorithm
in R, including a translation of the original algorithm into R.")
    (license license:expat)))

(define-public r-uwot
  (package
    (name "r-uwot")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "uwot" version))
       (sha256
        (base32
         "0pz9wa89xq4d119q86lskrznf979m0r1db8iaprcz7kxbi6b8lrj"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-dqrng" ,r-dqrng)
       ("r-fnn" ,r-fnn)
       ("r-irlba" ,r-irlba)
       ("r-matrix" ,r-matrix)
       ("r-rcpp" ,r-rcpp)
       ("r-rcppannoy" ,r-rcppannoy)
       ("r-rcppparallel" ,r-rcppparallel)
       ("r-rcppprogress" ,r-rcppprogress)
       ("r-rspectra" ,r-rspectra)))
    (home-page "https://github.com/jlmelville/uwot")
    (synopsis "Uniform manifold approximation and projection")
    (description
     "This package provides an implementation of the Uniform Manifold
Approximation and Projection dimensionality reduction by McInnes et
al. (2018).  It also provides means to transform new data and to carry out
supervised dimensionality reduction.  An implementation of the related
LargeVis method of Tang et al. (2016) is also provided.")
    (license license:gpl3)))

(define-public r-kableextra
  (package
    (name "r-kableextra")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "kableExtra" version))
       (sha256
        (base32
         "1nicvw06xsf3a1f5c10mih07b76m2v5s5h165vmz0qx6n1a3492i"))))
    (properties `((upstream-name . "kableExtra")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-digest" ,r-digest)
       ("r-glue" ,r-glue)
       ("r-htmltools" ,r-htmltools)
       ("r-knitr" ,r-knitr)
       ("r-magrittr" ,r-magrittr)
       ("r-readr" ,r-readr)
       ("r-rmarkdown" ,r-rmarkdown)
       ("r-rstudioapi" ,r-rstudioapi)
       ("r-rvest" ,r-rvest)
       ("r-scales" ,r-scales)
       ("r-stringr" ,r-stringr)
       ("r-viridislite" ,r-viridislite)
       ("r-webshot" ,r-webshot)
       ("r-xml2" ,r-xml2)))
    (home-page "https://haozhu233.github.io/kableExtra/")
    (synopsis "Construct complex tables with pipe syntax")
    (description
     "Build complex HTML or LaTeX tables using @code{kable()} from
@code{knitr} and the piping syntax from @code{magrittr}.  The function
@code{kable()} is a light weight table generator coming from @code{knitr}.
This package simplifies the way to manipulate the HTML or LaTeX codes
generated by @code{kable()} and allows users to construct complex tables and
customize styles using a readable syntax.")
    (license license:expat)))

(define-public r-glasso
  (package
    (name "r-glasso")
    (version "1.11")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "glasso" version))
       (sha256
        (base32 "02p3612rpydk195n2qr77lp1j2w8zsw1ckkk98c8angm4r5q8dsc"))))
    (build-system r-build-system)
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "https://statweb.stanford.edu/~tibs/glasso/")
    (synopsis "Graphical Lasso: estimation of Gaussian graphical models")
    (description
     "This is a package for estimation of a sparse inverse covariance matrix
using a lasso (L1) penalty.  Facilities are provided for estimates along a
path of values for the regularization parameter.")
    (license license:gpl2)))

(define-public r-rhpcblasctl
  (package
    (name "r-rhpcblasctl")
    (version "0.20-17")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "RhpcBLASctl" version))
       (sha256
        (base32
         "0iwc06blr5sx7rylwczi2jrha8sk8qs0jklflwpidl0zj1jxdggp"))))
    (properties `((upstream-name . "RhpcBLASctl")))
    (build-system r-build-system)
    (home-page "http://prs.ism.ac.jp/~nakama/Rhpc/")
    (synopsis "Control the number of threads on BLAS")
    (description
     "This package allows you to control the number of threads the BLAS
library uses.  It is also possible to control the number of threads in
OpenMP.")
    (license license:agpl3+)))

(define-public r-lda
  (package
    (name "r-lda")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "lda" version))
       (sha256
        (base32
         "03r4h5kgr8mfy44p66mfj5bp4k00g8zh4a1mhn46jw14pkhs21jn"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/lda/")
    (synopsis "Collapsed Gibbs sampling methods for topic models")
    (description
     "This package implements @dfn{latent Dirichlet allocation} (LDA) and
related models.  This includes (but is not limited to) sLDA, corrLDA, and the
mixed-membership stochastic blockmodel.  Inference for all of these models is
implemented via a fast collapsed Gibbs sampler written in C.  Utility
functions for reading/writing data typically used in topic models, as well as
tools for examining posterior distributions are also included.")
    ;; Any version of the LGPL
    (license license:lgpl3+)))

(define-public r-rann-l1
  (package
    (name "r-rann-l1")
    (version "2.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "RANN.L1" version))
       (sha256
        (base32
         "1hanh3my84mdr5wy6b89fawqzfc184vff1y65wy4l5ld9qza1n44"))))
    (properties `((upstream-name . "RANN.L1")))
    (build-system r-build-system)
    (home-page "https://github.com/jefferis/RANN/tree/master-L1")
    (synopsis "Fast nearest neighbour search using L1 metric")
    (description
     "This package provides tools to find the k nearest neighbours for every
point in a given dataset in O(N log N) time using Arya and Mount's ANN
library.  There is support for approximate as well as exact searches, fixed
radius searches and @code{bd} as well as @code{kd} trees.  The distance is
computed using the L1 (Manhattan, taxicab) metric.")
    (license license:gpl3+)))

(define-public r-leiden
  (package
    (name "r-leiden")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "leiden" version))
       (sha256
        (base32
         "1hh6bmbz6cpqwl4i94gxylgv9x92zbqdg81r8r4ymfy8c70f3df2"))))
    (properties `((upstream-name . "leiden")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-igraph" ,r-igraph)
       ("r-matrix" ,r-matrix)
       ("r-reticulate" ,r-reticulate)))
    (home-page "https://github.com/TomKellyGenetics/leiden")
    (synopsis "R implementation of Leiden clustering algorithm")
    (description
     "This package implements the Python @code{leidenalg} module to be called
in R.  It enables clustering using the Leiden algorithm for partitioning a
graph into communities.  See also Traag et al (2018) \"From Louvain to Leiden:
guaranteeing well-connected communities.\" <arXiv:1810.08473>.")
    (license license:gpl3)))

(define-public r-patchwork
  ;; There has been no public release yet.
  (let ((commit "fd7958bae3e7a1e30237c751952e412a0a1d1242")
        (revision "1"))
    (package
      (name "r-patchwork")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/thomasp85/patchwork.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "00fq520xwy1ysg4k8x48x9b0yy9wyi8y8zj6dvxjg4bwx0yyp6s4"))))
      (build-system r-build-system)
      (propagated-inputs
       `(("r-ggplot2" ,r-ggplot2)
         ("r-gtable" ,r-gtable)))
      (home-page "https://github.com/thomasp85/patchwork")
      (synopsis "Compose ggplot2 plots")
      (description
       "The @code{ggplot2} package provides a strong API for sequentially
building up a plot, but does not concern itself with composition of multiple
plots.  Patchwork is a package that expands the API to allow for arbitrarily
complex composition of plots by providing mathmatical operators for combining
multiple plots.")
      (license license:expat))))

(define-public r-liger
  (package
    (name "r-liger")
    (version "0.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/MacoskoLab/liger.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "16dzwwcpw6n78pxlc5w3kraigki35ix7zhd2cbx5f3y60bbkhlmx"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (delete-file "inst/java/ModularityOptimizer.jar")
           #t))))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'build-java-part
           (lambda* (#:key inputs #:allow-other-keys)
             (invoke "unzip" (assoc-ref inputs "optimizer-src"))
             (for-each (lambda (file) (invoke "javac" file))
                       (find-files "." "\\.java$"))
             (apply invoke "jar" "cf" "inst/java/ModularityOptimizer.jar"
                    (find-files "." "\\.class$"))
             #t)))))
    (propagated-inputs
     `(("r-cowplot" ,r-cowplot)
       ("r-dosnow" ,r-dosnow)
       ("r-dplyr" ,r-dplyr)
       ("r-fnn" ,r-fnn)
       ("r-foreach" ,r-foreach)
       ("r-ggplot2" ,r-ggplot2)
       ("r-ggrepel" ,r-ggrepel)
       ("r-hmisc" ,r-hmisc)
       ("r-ica" ,r-ica)
       ("r-irlba" ,r-irlba)
       ("r-matrix" ,r-matrix)
       ("r-mclust" ,r-mclust)
       ("r-patchwork" ,r-patchwork)
       ("r-plyr" ,r-plyr)
       ("r-rann-l1" ,r-rann-l1)
       ("r-rcpp" ,r-rcpp)
       ("r-rcpparmadillo" ,r-rcpparmadillo)
       ("r-riverplot" ,r-riverplot)
       ("r-rtsne" ,r-rtsne)
       ("r-snow" ,r-snow)))
    (native-inputs
     `(("jdk" ,icedtea "jdk")
       ;; See https://github.com/MacoskoLab/liger/issues/96
       ;; The optimizer is released under the Expat license.
       ("optimizer-src"
        ,(origin
           (method url-fetch)
           (uri "http://www.ludowaltman.nl/slm/modularity_optimizer_source.zip")
           (sha256
            (base32
             "01hmm6sapcmldvayknqx2w4cav3qv71mwwkdkwj4qgq6dss09g18"))))
       ("unzip" ,unzip)
       ("r-knitr" ,r-knitr))) ; for vignettes
    (home-page "https://github.com/MacoskoLab/liger")
    (synopsis "Integrate and analyze multiple single-cell datasets")
    (description
     "LIGER is a package for integrating and analyzing multiple single-cell
datasets, developed and maintained by the Macosko lab.  It relies on
integrative non-negative matrix factorization to identify shared and
dataset-specific factors.")
    (license license:gpl3)))

(define-public r-harmony
  ;; There are no tagged commits
  (let ((commit "4d1653870d4dd70fff1807c182882db1fbf9af5a")
        (revision "1"))
    (package
      (name "r-harmony")
      (version (git-version "1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/immunogenomics/harmony")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1gasdldr4aalr9h2q9kmm3y4i7azkgnhdn4bmvsszs7lg9xacw85"))))
      (build-system r-build-system)
      (propagated-inputs
       `(("r-cowplot" ,r-cowplot)
         ("r-dplyr" ,r-dplyr)
         ("r-ggplot2" ,r-ggplot2)
         ("r-irlba" ,r-irlba)
         ("r-matrix" ,r-matrix)
         ("r-rcpp" ,r-rcpp)
         ("r-rcpparmadillo" ,r-rcpparmadillo)
         ("r-rcppprogress" ,r-rcppprogress)
         ("r-rlang" ,r-rlang)
         ("r-tibble" ,r-tibble)
         ("r-tidyr" ,r-tidyr)))
      (home-page "https://github.com/immunogenomics/harmony")
      (synopsis "Integration of single cell sequencing data")
      (description
       "This package provides an implementation of the Harmony algorithm for
single cell integration, described in Korsunsky et al
@url{doi.org/10.1101/461954}.  The package includes a standalone Harmony
function and interfaces to external frameworks.")
      (license license:gpl3))))

(define-public r-covr
  (package
    (name "r-covr")
    (version "3.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "covr" version))
       (sha256
        (base32 "1pvr95h7jg9hqq1qq1cccy323pkxldrwafl08151cc410499k4fb"))))
    (properties `((upstream-name . "covr")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-crayon" ,r-crayon)
       ("r-digest" ,r-digest)
       ("r-httr" ,r-httr)
       ("r-jsonlite" ,r-jsonlite)
       ("r-rex" ,r-rex)
       ("r-withr" ,r-withr)
       ("r-yaml" ,r-yaml)))
    (native-inputs
     `(("r-knitr" ,r-knitr))) ; for vignettes
    (home-page "https://github.com/r-lib/covr")
    (synopsis "Test coverage for R packages")
    (description
     "Thisp package enables you to track and report code coverage for your
package and (optionally) upload the results to a coverage service.  Code
coverage is a measure of the amount of code being exercised by a set of tests.
It is an indirect measure of test quality and completeness.  This package is
compatible with any testing methodology or framework and tracks coverage of
both R code and compiled C/C++/FORTRAN code.")
    (license license:gpl3)))

(define-public r-systemfonts
  (package
    (name "r-systemfonts")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "systemfonts" version))
       (sha256
        (base32
         "0m0ljid683xcam2f14x7k2zv1yx4npac38a3gfv11vhxfbnpgp0z"))))
    (properties `((upstream-name . "systemfonts")))
    (build-system r-build-system)
    (inputs
     `(("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://github.com/r-lib/systemfonts")
    (synopsis "System native font finding")
    (description
     "This package provides system native access to the font catalogue.  As
font handling varies between systems it is difficult to correctly locate
installed fonts across different operating systems.  The 'systemfonts' package
provides bindings to the native libraries for finding font files that can then
be used further by e.g. graphic devices.")
    (license license:expat)))

(define-public r-graphlayouts
  (package
    (name "r-graphlayouts")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "graphlayouts" version))
       (sha256
        (base32
         "1la016m37kp79zk8p1yx9kaha8y6d4w52w39h0mzv1mfsi6d75w0"))))
    (properties `((upstream-name . "graphlayouts")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-igraph" ,r-igraph)
       ("r-rcpp" ,r-rcpp)
       ("r-rcpparmadillo" ,r-rcpparmadillo)))
    (home-page "https://github.com/schochastics/graphlayouts")
    (synopsis "Additional layout algorithms for network visualizations")
    (description
     "This package provides several layout algorithms to visualize networks
which are not part of the igraph library.  Most are based on the concept of
stress majorization by Gansner et al. (2004)
<doi:10.1007/978-3-540-31843-9_25>.  Some more specific algorithms allow to
emphasize hidden group structures in networks or focus on specific nodes.")
    (license license:expat)))

(define-public r-tidygraph
  (package
    (name "r-tidygraph")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "tidygraph" version))
       (sha256
        (base32
         "1zpsarm74afbc7p5dlyb0whc09670qdcddw1ckb25cfc9hfh0hjn"))))
    (properties `((upstream-name . "tidygraph")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-dplyr" ,r-dplyr)
       ("r-igraph" ,r-igraph)
       ("r-magrittr" ,r-magrittr)
       ("r-pillar" ,r-pillar)
       ("r-r6" ,r-r6)
       ("r-rcpp" ,r-rcpp)
       ("r-rlang" ,r-rlang)
       ("r-tibble" ,r-tibble)
       ("r-tidyr" ,r-tidyr)))
    (home-page "https://github.com/thomasp85/tidygraph")
    (synopsis "Tidy API for graph manipulation")
    (description
     "This package provides a graph implementation that can be thought of as
two tidy data frames describing node and edge data respectively.  It provides
an approach to manipulate these two virtual data frames using the API defined
in the @code{dplyr} package, and it also provides tidy interfaces to a lot of
common graph algorithms.")
    (license license:expat)))

(define-public r-soupx
  (let ((commit "a3354be76fb52fd795be6ddf163cf056c05c6cb8")
        (revision "1"))
    (package
      (name "r-soupx")
      (version (git-version "0.3.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/constantAmateur/SoupX")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1zmlyzrl0fz6l79gn2wswid670p88mm3y292is89sa5p3h7frr99"))))
      (properties `((upstream-name . "SoupX")))
      (build-system r-build-system)
      (propagated-inputs
       `(("r-ggplot2" ,r-ggplot2)
         ("r-matrix" ,r-matrix)
         ("r-seurat" ,r-seurat)))
      (home-page "https://github.com/constantAmateur/SoupX")
      (synopsis "Single cell mRNA Soup eXterminator")
      (description
       "This package provides a package for quantifying, profiling and
removing cell free mRNA contamination (the \"soup\") from droplet based single
cell RNA-seq experiments.")
      (license license:gpl2))))

(define-public r-assertr
  (package
    (name "r-assertr")
    (version "2.7")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "assertr" version))
       (sha256
        (base32
         "1x868pd4dac1c7b1q5390wqh2f6s50km8nqcpim3nxcj2l4qkqdb"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-dplyr" ,r-dplyr)
       ("r-mass" ,r-mass)
       ("r-rlang" ,r-rlang)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))           ; needed for vignette
    (home-page "https://github.com/ropensci/assertr")
    (synopsis "Assertive programming for R analysis pipelines")
    (description
     "This package provides functionality to assert conditions that have to be
met so that errors in data used in analysis pipelines can fail quickly.  It is
similar to @code{stopifnot()} but more powerful, friendly, and easier for use
in pipelines.")
    (license license:expat)))

(define-public r-parameters
  (package
    (name "r-parameters")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "parameters" version))
       (sha256
        (base32
         "0zin3ikc013hc713n5zs0dbhc3m4nfw1vhc3924z0mrww8r241xn"))))
    (properties `((upstream-name . "parameters")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bayestestr" ,r-bayestestr)
       ("r-insight" ,r-insight)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://cran.r-project.org/web/packages/parameters")
    (synopsis "Processing of model parameters")
    (description
     "This package provides utilities for processing the parameters of various
statistical models.  Beyond computing p values, CIs, and other indices for a
wide variety of models, this package implements features like standardization
or bootstrapping of parameters and models, feature reduction (feature
extraction and variable selection) as well as conversion between indices of
effect size.")
    (license license:gpl3)))

(define-public r-rgdal
  (package
    (name "r-rgdal")
    (version "1.4-8")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rgdal" version))
       (sha256
        (base32 "1jd691amf3ghznq5im15gvhl6v6k25klpl75m4ngrqf9xlxaa3as"))))
    (properties `((upstream-name . "rgdal")))
    (build-system r-build-system)
    (inputs
     `(("gdal" ,gdal)
       ("proj.4" ,proj.4)
       ("zlib" ,zlib)))
    (propagated-inputs
     `(("r-sp" ,r-sp)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://rgdal.r-forge.r-project.org")
    (synopsis "Bindings for the Geospatial Data Abstraction Library")
    (description
     "This package provides bindings to the Geospatial Data Abstraction
Library (GDAL) and access to projection/transformation operations from the
PROJ.4 library.")
    (license license:gpl2+)))

(define-public r-insol
  (package
    (name "r-insol")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "insol" version))
       (sha256
        (base32
         "1d2vqmbp94ff80mfkmq4a8y12r6ryym8hh1568ip9qbn7snc64v1"))))
    (properties `((upstream-name . "insol")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-raster" ,r-raster)))
    (native-inputs
     `(("gfortran" ,gfortran)))
    (home-page "https://meteoexploration.com/R/insol/index.html")
    (synopsis "Tools for calculating solar radiation")
    (description
     "This package provides functions to compute insolation on tilted
surfaces, computes atmospheric transmittance and related parameters such as:
Earth radius vector, declination, sunset and sunrise, daylength, equation of
time, vector in the direction of the sun, vector normal to surface, and some
atmospheric physics.")
    (license license:gpl2+)))

(define-public r-lifecycle
  (package
    (name "r-lifecycle")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "lifecycle" version))
       (sha256
        (base32
         "0912865c6675fsblrfdk6s568krsj1x8qbk1kipy7m05xs6nwx19"))))
    (properties `((upstream-name . "lifecycle")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-glue" ,r-glue)
       ("r-rlang" ,r-rlang)))
    (native-inputs
     `(("r-knitr" ,r-knitr))) ; for vignettes
    (home-page "https://github.com/r-lib/lifecycle")
    (synopsis "Manage the life cycle of your package functions")
    (description
     "Manage the life cycle of your exported functions with shared
conventions, documentation badges, and non-invasive deprecation warnings.  The
lifecycle package defines four development stages (experimental, maturing,
stable, and questioning) and three deprecation stages (soft-deprecated,
deprecated, and defunct).  It makes it easy to insert badges corresponding to
these stages in your documentation.  Usage of deprecated functions are
signalled with increasing levels of non-invasive verbosity.")
    (license license:gpl3)))

(define-public r-assertable
  (package
    (name "r-assertable")
    (version "0.2.7")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "assertable" version))
       (sha256
        (base32
         "1npks9rcrnchmd0silq6qrvqkmdkp9fwjkyyvvp1lqjclyxk6vkk"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-data-table" ,r-data-table)))
    (home-page "https://cran.r-project.org/web/packages/assertable/")
    (synopsis "Verbose assertions for tabular data (data.frames and data.tables)")
    (description "This package provides simple, flexible assertions on
data.frame or data.table objects with verbose output for vetting.  While other
assertion packages apply towards more general use-cases, @code{assertable} is
tailored towards tabular data.  It includes functions to check variable names
and values, whether the dataset contains all combinations of a given set of
unique identifiers, and whether it is a certain length.  In addition,
@code{assertable} includes utility functions to check the existence of target
files and to efficiently import multiple tabular data files into one
data.table.")
    (license license:gpl3)))

(define-public r-quadprog
  (package
    (name "r-quadprog")
    (version "1.5-8")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "quadprog" version))
       (sha256
        (base32 "1ka9g8zak8sg4y2xbz93dfrldznlk9qpd4pq9z21cdcdn3b8s4i2"))))
    (build-system r-build-system)
    (native-inputs
     `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/quadprog")
    (synopsis "Functions to solve quadratic programming problems")
    (description
     "This package contains routines and documentation for solving quadratic
programming problems.")
    (license license:gpl3+)))

(define-public r-desolve
  (package
    (name "r-desolve")
    (version "1.28")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "deSolve" version))
       (sha256
        (base32 "0jasvdzig0pzhzspmy20089az19r91xjfb9q6h8gj7c4mr6fymac"))))
    (properties `((upstream-name . "deSolve")))
    (build-system r-build-system)
    (native-inputs
     `(("gfortran" ,gfortran)))
    (home-page "https://desolve.r-forge.r-project.org/")
    (synopsis "Solvers for initial value problems of differential equations")
    (description "This package provides functions that solve initial value
problems of a system of first-order @dfn{ordinary differential
equations} (ODE), of @dfn{partial differential equations} (PDE), of
@dfn{differential algebraic equations} (DAE), and of delay differential
equations.  The functions provide an interface to the FORTRAN functions
@code{lsoda}, @code{lsodar}, @code{lsode}, @code{lsodes} of the ODEPACK
collection, to the FORTRAN functions @code{dvode} and @code{daspk} and a
C-implementation of solvers of the Runge-Kutta family with fixed or variable
time steps.  The package contains routines designed for solving ODEs resulting
from 1-D, 2-D and 3-D partial differential equations that have been converted
to ODEs by numerical differencing.")
    (license license:gpl2+)))

(define-public r-pracma
  (package
    (name "r-pracma")
    (version "2.2.9")
    (source (origin
              (method url-fetch)
              (uri (cran-uri "pracma" version))
              (sha256
               (base32 "07mzhzz73wsjjw1q05l024gcd13hwnzsxf873q9dyhw6x3shzshc"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/pracma/")
    (synopsis "Practical numerical math functions")
    (description "This package provides functions for numerical analysis and
linear algebra, numerical optimization, differential equations, plus some
special functions.  It uses Matlab function names where appropriate to simplify
porting.")
    (license license:gpl3+)))

(define-public r-subplex
  (package
    (name "r-subplex")
    (version "1.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "subplex" version))
       (sha256
        (base32
         "1yfbfjvpbhgky7vihw3f4jl41pxpqb39z4nc045d5z7z48bdl18d"))))
    (build-system r-build-system)
    (native-inputs
     `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/subplex")
    (synopsis "Unconstrained optimization using the subplex algorithm")
    (description
     "This package implements the Subplex optimization algorithm.
It solves unconstrained optimization problems using a simplex method on
subspaces.  The method is well suited for optimizing objective functions that
are noisy or are discontinuous at the solution.")
    (license license:gpl3+)))

(define-public r-txtplot
  (package
    (name "r-txtplot")
    (version "1.0-3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "txtplot" version))
       (sha256
        (base32
         "1949ab1bzvysdb79g8x1gaknj0ih3d6g63pv9512h5m5l3a6c31h"))))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/txtplot/")
    (synopsis "Text-based plotting")
    (description "This package provides functions to produce rudimentary ASCII
graphics directly in the terminal window.  This package provides a basic
plotting function (and equivalents of curve, density, acf and barplot) as well
as a boxplot function.")
    (license license:lgpl3+)))

(define-public r-bio3d
  (package
    (name "r-bio3d")
    (version "2.4-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "bio3d" version))
       (sha256
        (base32
         "07rw6c2d95gb5myxh31727j0jrchd0xisa3x89jjmf4zzs3vv7v7"))))
    (properties `((upstream-name . "bio3d")))
    (build-system r-build-system)
    (inputs `(("zlib" ,zlib)))
    (propagated-inputs `(("r-rcpp" ,r-rcpp)))
    (home-page "http://thegrantlab.org/bio3d/")
    (synopsis "Biological structure analysis")
    (description
     "This package provides utilities to process, organize and explore protein
structure, sequence and dynamics data.  Features include the ability to read
and write structure, sequence and dynamic trajectory data, perform sequence
and structure database searches, data summaries, atom selection, alignment,
superposition, rigid core identification, clustering, torsion analysis,
distance matrix analysis, structure and sequence conservation analysis, normal
mode analysis, principal component analysis of heterogeneous structure data,
and correlation network analysis from normal mode and molecular dynamics data.
In addition, various utility functions are provided to enable the statistical
and graphical power of the R environment to work with biological sequence and
structural data.")
    (license license:gpl2+)))

(define-public r-bios2cor
  (package
    (name "r-bios2cor")
    (version "2.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Bios2cor" version))
       (sha256
        (base32
         "04wk1cjrrxhpv1kdhd67r1qvjv268xxi0z0r105wy912110z9m6x"))))
    (properties `((upstream-name . "Bios2cor")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bigmemory" ,r-bigmemory)
       ("r-bio3d" ,r-bio3d)
       ("r-circular" ,r-circular)
       ("r-igraph" ,r-igraph)))
    (home-page "https://cran.r-project.org/web/packages/Bios2cor/")
    (synopsis "From biological sequences and simulations to correlation analysis")
    (description
     "This package provides utilities for computation and analysis of
correlation/covariation in multiple sequence alignments and in side chain
motions during molecular dynamics simulations.  Features include the
computation of correlation/covariation scores using a variety of scoring
functions between either sequence positions in alignments or side chain
dihedral angles in molecular dynamics simulations and utilities to analyze the
correlation/covariation matrix through a variety of tools including network
representation and principal components analysis.  In addition, several
utility functions are based on the R graphical environment to provide friendly
tools for help in data interpretation.")
    (license license:gpl2+)))

;; This package includes minified JavaScript files.  When upgrading please
;; check that there are no new minified JavaScript files.
(define-public r-networkd3
  (package
    (name "r-networkd3")
    (version "0.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "networkD3" version))
       (sha256
        (base32
         "02wxki67drppgfka1is1ykg1f2rxf0x0657c0crj7ipfy62jbf1k"))
       (snippet
        '(begin
           (delete-file "inst/htmlwidgets/lib/d3-4.5.0/d3.min.js")
           #t))))
    (properties `((upstream-name . "networkD3")))
    (build-system r-build-system)
    (arguments
     `(#:modules ((guix build utils)
                  (guix build r-build-system)
                  (srfi srfi-1)
                  (ice-9 popen))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'process-javascript
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "inst/htmlwidgets/lib/"
               (call-with-values
                   (lambda ()
                     (unzip2
                      `((,(assoc-ref inputs "d3.v4.js")
                         "d3-4.5.0/d3.min.js"))))
                 (lambda (sources targets)
                   (for-each (lambda (source target)
                               (format #t "Processing ~a --> ~a~%"
                                       source target)
                               (let ((minified (open-pipe* OPEN_READ "uglify-js" source)))
                                 (call-with-output-file target
                                   (lambda (port)
                                     (dump-port minified port)))))
                             sources targets))))
             #t)))))
    (native-inputs
     `(("uglify-js" ,uglify-js)
       ;; NOTE: Make sure that this version of d3 is still valid when
       ;; upgrading the package.
       ("d3.v4.js"
        ,(origin
           (method url-fetch)
           (uri "https://d3js.org/d3.v4.js")
           (sha256
            (base32
             "0y7byf6kcinfz9ac59jxc4v6kppdazmnyqfav0dm4h550fzfqqlg"))))))
    (propagated-inputs
     `(("r-htmlwidgets" ,r-htmlwidgets)
       ("r-igraph" ,r-igraph)
       ("r-magrittr" ,r-magrittr)))
    (home-page "https://cran.r-project.org/package=networkD3")
    (synopsis "D3 JavaScript network graphs from R")
    (description
     "This package creates D3 JavaScript network, tree, dendrogram, and Sankey
graphs from R.")
    (license license:gpl3+)))

(define-public r-aasea
  (package
    (name "r-aasea")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "aaSEA" version))
       (sha256
        (base32
         "0him4r8qyp0xssgrmdxjs45yn4d28h5anv4jyxxbbs9phb0m6j3h"))))
    (properties `((upstream-name . "aaSEA")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bios2cor" ,r-bios2cor)
       ("r-dt" ,r-dt)
       ("r-hmisc" ,r-hmisc)
       ("r-magrittr" ,r-magrittr)
       ("r-networkd3" ,r-networkd3)
       ("r-plotly" ,r-plotly)
       ("r-seqinr" ,r-seqinr)
       ("r-shiny" ,r-shiny)
       ("r-shinydashboard" ,r-shinydashboard)))
    (home-page "https://cran.r-project.org/web/packages/aaSEA/")
    (synopsis "Amino acid substitution effect analyzer")
    (description
     "Given a protein multiple sequence alignment, it is a daunting task to
assess the effects of substitutions along sequence length.  The aaSEA package
is intended to help researchers to rapidly analyze property changes caused by
single, multiple and correlated amino acid substitutions in proteins.")
    (license license:gpl3)))

(define-public r-abacus
  (package
    (name "r-abacus")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ABACUS" version))
       (sha256
        (base32
         "0m1dnkwjr1522l9ddbzzx7ayxvli17sbmk6s28adpmzzjwh2kd1i"))))
    (properties `((upstream-name . "ABACUS")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-shiny" ,r-shiny)))
    (home-page "https://shiny.abdn.ac.uk/Stats/apps/")
    (synopsis "Apps-based activities for communicating and understanding statistics")
    (description
     "This package provides a set of Shiny apps for effective communication
and understanding in statistics.  The current version includes properties of
normal distribution, properties of sampling distribution, one-sample z and t
tests, two samples independent (unpaired) t test and analysis of variance.")
    (license license:gpl3)))

(define-public r-abc-rap
  (package
    (name "r-abc-rap")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ABC.RAP" version))
       (sha256
        (base32
         "1kdspln17v0krvahcd55vib4dv5azp60b3r1zf489x10qqbp1mxk"))))
    (properties `((upstream-name . "ABC.RAP")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/ABC.RAP/")
    (synopsis "Array-based CpG region analysis pipeline")
    (description
     "This package aims to identify candidate genes that are differentially
methylated between cases and controls.  It applies Student's t-test and delta
beta analysis to identify candidate genes containing multiple CpG sites.")
    (license license:gpl3)))

(define-public r-abcadm
  (package
    (name "r-abcadm")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "abcADM" version))
       (sha256
        (base32
         "0vcabnnnwc0psv9v3rda5aap9s8cq1pjh02zva3ki64hlavf2a10"))))
    (properties `((upstream-name . "abcADM")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bh" ,r-bh)
       ("r-rcpp" ,r-rcpp)))
    (home-page "https://cran.r-project.org/web/packages/abcADM/")
    (synopsis "Fit accumulated damage models and estimate reliability using ABC")
    (description
     "This package provides tools to estimate parameters of accumulated
damage (load duration) models based on failure time data under a Bayesian
framework, using @dfn{Approximate Bayesian Computation} (ABC), and to assess
long-term reliability under stochastic load profiles.")
    (license license:gpl3)))

(define-public r-rglpk
  (package
    (name "r-rglpk")
    (version "0.6-4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Rglpk" version))
       (sha256
        (base32
         "19mzpyimzq9zqnbi05j79b2di3nzaln8swggs9p8sqdr60qvr3d2"))))
    (properties `((upstream-name . "Rglpk")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-slam" ,r-slam)))
    (inputs
     `(("glpk" ,glpk)))
    (home-page "https://r-forge.r-project.org/projects/rglp/")
    (synopsis "R interface to the GNU Linear Programming Kit")
    (description
     "This package provides an R interface to the GNU Linear Programming Kit,
software for solving large-scale @dfn{linear programming} (LP), @dfn{mixed
integer linear programming} (MILP) and other related problems.")
    ;; Either license
    (license (list license:gpl2 license:gpl3))))

(define-public r-abcdefba
  (package
    (name "r-abcdefba")
    (version "0.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "abcdeFBA" version))
       (sha256
        (base32
         "1rxjripy8v6bxi25vdfjnbk24zkmf752qbl73cin6nvnqflwxkx4"))))
    (properties `((upstream-name . "abcdeFBA")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-corrplot" ,r-corrplot)
       ("r-lattice" ,r-lattice)
       ("r-rgl" ,r-rgl)
       ("r-rglpk" ,r-rglpk)))
    (home-page "https://cran.r-project.org/web/packages/abcdeFBA/")
    (synopsis "A-Biologist-Can-Do-Everything of Flux Balance Analysis with this package")
    (description
     "This package provides functions for Constraint Based Simulation using
Flux Balance Analysis and informative analysis of the data generated during
simulation.")
    (license license:gpl2)))

(define-public r-abcrlda
  (package
    (name "r-abcrlda")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "abcrlda" version))
       (sha256
        (base32
         "0zjdrbg3zx0znqnh0dvmifs9c12b8vjhbaf3nbwab9xh25nsmg75"))))
    (properties `((upstream-name . "abcrlda")))
    (build-system r-build-system)
    (home-page "https://ieeexplore.ieee.org/document/8720003/")
    (synopsis "Asymptotically bias-corrected regularized linear discriminant analysis")
    (description
     "This package offers methods to perform @dfn{asymptotically
bias-corrected regularized linear discriminant analysis} (ABC_RLDA) for
cost-sensitive binary classification.  The bias-correction is an estimate of
the bias term added to regularized discriminant analysis that minimizes the
overall risk.")
    (license license:gpl3)))

(define-public r-abemus
  (package
    (name "r-abemus")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "abemus" version))
       (sha256
        (base32
         "1dhllb184byp1yl15rg2w02zgw3iajag7cxshirg47mnmm7n70bb"))))
    (properties `((upstream-name . "abemus")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-data-table" ,r-data-table)))
    (home-page "https://cran.r-project.org/web/packages/abemus/")
    (synopsis "Adaptive base error model in ultra-deep sequencing data")
    (description
     "This package provides an implementation of @dfn{Adaptive Base Error
Model in Ultra-deep Sequencing data} (ABEMUS), which combines
platform-specific genetic knowledge and empirical signal to readily detect and
quantify somatic @dfn{single nucleotide variants} (SNVs) in @dfn{circulating
cell free DNA} (cfDNA).")
    (license license:gpl3)))

;; This package includes minified JavaScript files.  When upgrading please
;; check that there are no new minified JavaScript files.
(define-public r-rintrojs
  (package
    (name "r-rintrojs")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rintrojs" version))
       (sha256
        (base32
         "0vyqb3pyrh12saddar71ac9csn2vkd2j8ln6ygpqys8ky1lc3427"))))
    (properties `((upstream-name . "rintrojs")))
    (build-system r-build-system)
    (arguments
     `(#:modules ((guix build utils)
                  (guix build r-build-system)
                  (srfi srfi-1)
                  (ice-9 popen))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'process-javascript
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "inst/javascript/introjs/"
               (call-with-values
                   (lambda ()
                     (unzip2
                      `((,(assoc-ref inputs "intro.js")
                         "intro.min.js"))))
                 (lambda (sources targets)
                   (for-each (lambda (source target)
                               (format #t "Processing ~a --> ~a~%"
                                       source target)
                               (let ((minified (open-pipe* OPEN_READ "uglify-js" source)))
                                 (call-with-output-file target
                                   (lambda (port)
                                     (dump-port minified port)))))
                             sources targets))))
             #t)))))
    (native-inputs
     `(("uglify-js" ,uglify-js)
       ("intro.js"
        ,(origin
           (method url-fetch)
           (uri "https://raw.githubusercontent.com/usablica/intro.js/v2.9.3/intro.js")
           (sha256
            (base32
             "1qf8n1sfy9qkiqqnfgg0xbhmfgh0g3mqsjas8qhz230h3zzlzxj8"))))))
    (propagated-inputs
     `(("r-jsonlite" ,r-jsonlite)
       ("r-shiny" ,r-shiny)))
    (home-page "https://github.com/carlganz/rintrojs")
    (synopsis "Wrapper for the Intro.js library")
    (description
     "This package provides a wrapper for the @url{http://www.introjs.com,
Intro.js} library.  This package makes it easy to include step-by-step
introductions, and clickable hints in a Shiny application.  It supports both
static introductions in the UI, and programmatic introductions from the
server-side.")
    (license license:agpl3+)))

(define-public r-sysfonts
  (package
    (name "r-sysfonts")
    (version "0.8")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "sysfonts" version))
       (sha256
        (base32
         "0wng902plryf2d8fc7k7m3jx11acz51kb2d91cqbyhq7xpk06z43"))))
    (properties `((upstream-name . "sysfonts")))
    (build-system r-build-system)
    (inputs
     `(("freetype" ,freetype)
       ("libpng" ,libpng)
       ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://github.com/yixuan/sysfonts")
    (synopsis "Loading fonts into R")
    (description
     "This is a package to simplify loading of system fonts and Google Fonts
into R, in order to support other packages.")
    (license license:gpl2)))

(define-public r-showtextdb
  (package
    (name "r-showtextdb")
    (version "2.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "showtextdb" version))
       (sha256
        (base32
         "1qwwj9x2jvadvwn60h75k99c9xi7yhqjsgaakahz5paxgj583bsh"))))
    (properties `((upstream-name . "showtextdb")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-sysfonts" ,r-sysfonts)))
    (home-page "https://cran.r-project.org/web/packages/showtextdb/")
    (synopsis "Font files for the 'showtext' package")
    (description
     "This package provides font files that can be used by the @code{showtext}
package.")
    (license license:asl2.0)))

(define-public r-showtext
  (package
    (name "r-showtext")
    (version "0.7-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "showtext" version))
       (sha256
        (base32
         "0a5gg72bfrc7wm0phj1aflj1wc08kfi81ac32na6ya9s2ivyimw5"))))
    (properties `((upstream-name . "showtext")))
    (build-system r-build-system)
    (inputs
     `(("freetype" ,freetype)
       ("libpng" ,libpng)
       ("zlib" ,zlib)))
    (propagated-inputs
     `(("r-showtextdb" ,r-showtextdb)
       ("r-sysfonts" ,r-sysfonts)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://github.com/yixuan/showtext")
    (synopsis "Using fonts more easily in R graphs")
    (description
     "This package aims to make it easy to use various types of
fonts (TrueType, OpenType, Type 1, web fonts, etc.) in R graphs, and supports
most output formats of R graphics including PNG, PDF and SVG.  Text glyphs
will be converted into polygons or raster images, hence after the plot has
been created, it no longer relies on the font files.  No external software
such as Ghostscript is needed to use this package.")
    (license license:asl2.0)))

(define-public r-emojifont
  (package
    (name "r-emojifont")
    (version "0.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "emojifont" version))
       (sha256
        (base32
         "1cdrrl3hvrs8rskyy6zgr7q2mmg8yb9k8sld1m64zsp7y009g19k"))))
    (properties `((upstream-name . "emojifont")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-proto" ,r-proto)
       ("r-showtext" ,r-showtext)
       ("r-sysfonts" ,r-sysfonts)))
    (home-page "https://guangchuangyu.github.io/emojifont")
    (synopsis "Emoji and Font Awesome in R graphics")
    (description
     "This package enables the use of emoji and the Font Awesome glyphs in
both base and ggplot2 graphics.")
    (license license:artistic2.0)))

(define-public r-abstractr
  (package
    (name "r-abstractr")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "abstractr" version))
       (sha256
        (base32
         "1ymwp7syrynwd4i8aj2x5n8jdi9d96fjzl6jb09n0bnr5fgl7vig"))))
    (properties `((upstream-name . "abstractr")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-colourpicker" ,r-colourpicker)
       ("r-emojifont" ,r-emojifont)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gridextra" ,r-gridextra)
       ("r-rintrojs" ,r-rintrojs)
       ("r-shiny" ,r-shiny)
       ("r-shinythemes" ,r-shinythemes)))
    (home-page "https://matt-kumar.shinyapps.io/portfolio")
    (synopsis "R-Shiny application for creating visual abstracts")
    (description
     "This package provides an R Shiny application to create visual abstracts
for original research.  A variety of user defined options and formatting are
included.")
    (license license:gpl3)))

(define-public r-abtest
  (package
    (name "r-abtest")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "abtest" version))
       (sha256
        (base32
         "1ky3cf827kj24bhcpk00v5zl5jdkii1gca0x81ay1cjkzfispgws"))))
    (properties `((upstream-name . "abtest")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-matrix" ,r-matrix)
       ("r-mvtnorm" ,r-mvtnorm)
       ("r-plotrix" ,r-plotrix)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-rcpp" ,r-rcpp)
       ("r-sn" ,r-sn)
       ("r-truncnorm" ,r-truncnorm)
       ("r-vgam" ,r-vgam)))
    (home-page "https://cran.r-project.org/web/packages/abtest/")
    (synopsis "Bayesian A/B testing")
    (description
     "This package provides functions for Bayesian A/B testing including prior
elicitation options based on Kass and Vaidyanathan (1992)
@url{doi:10.1111/j.2517-6161.1992.tb01868.x}.")
    (license license:gpl2+)))

(define-public r-accept
  (package
    (name "r-accept")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "accept" version))
       (sha256
        (base32
         "1r4mhy9g4wjcjgdd0gwdarmr09292il3vdkmx0hz7vh9mffyr9kx"))))
    (properties `((upstream-name . "accept")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-dplyr" ,r-dplyr)
       ("r-extrafont" ,r-extrafont)
       ("r-mass" ,r-mass)
       ("r-plotly" ,r-plotly)
       ("r-stringr" ,r-stringr)
       ("r-viridis" ,r-viridis)))
    (home-page "https://cran.r-project.org/web/packages/accept/")
    (synopsis "Acute COPD Exacerbation Prediction Tool (ACCEPT)")
    (description
     "This package allows clinicians to predict the rate and severity of
future acute exacerbation in @dfn{Chronic Obstructive Pulmonary
Disease} (COPD) patients, based on the clinical prediction model published in
Adibi et al. (2019) @url{doi:10.1101/651901}.")
    (license license:gpl3)))

(define-public r-smpracticals
  (package
    (name "r-smpracticals")
    (version "1.4-3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "SMPracticals" version))
       (sha256
        (base32
         "0zxq84f9i3b86xx6msb25b61gyj9k09iab2b7wg4d93yas9qzayf"))))
    (properties `((upstream-name . "SMPracticals")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ellipse" ,r-ellipse)
       ("r-mass" ,r-mass)
       ("r-nlme" ,r-nlme)
       ("r-survival" ,r-survival)))
    (home-page "http://statwww.epfl.ch/davison/SM/")
    (synopsis "Practicals for use with Davison (2003) Statistical Models")
    (description
     "This package contains the datasets and a few functions for use with the
practicals outlined in Appendix A of the book Statistical Models (Davison,
2003, Cambridge University Press).  The practicals themselves can be found at
@url{http://statwww.epfl.ch/davison/SM/}.")
    (license license:gpl2+)))

(define-public r-fgui
  (package
    (name "r-fgui")
    (version "1.0-8")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "fgui" version))
       (sha256
        (base32
         "024fzd1c7iwqprn26hwjb9l2qlvvyzl449d7iixy0x69djwsrysv"))))
    (properties `((upstream-name . "fgui")))
    (build-system r-build-system)
    (home-page
     "https://sites.google.com/site/thomashoffmannproject/software/fgui")
    (synopsis "Create GUI for R functions")
    (description
     "Rapidly create a GUI for a function you created by automatically
creating widgets for arguments of the function.  This package automatically
parses help routines for context-sensitive help to these arguments.  The
interface is essentially a wrapper to some Tcl/Tk routines to both simplify
and facilitate GUI creation.  More advanced Tcl/Tk routines/GUI objects can be
incorporated into the interface for greater customization for the more
experienced.")
    ;; Any version of the GPL.
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-tcltk2
  (package
    (name "r-tcltk2")
    (version "1.2-11")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "tcltk2" version))
       (sha256
        (base32
         "1ibxld379600xx7kiqq3fck083s8psry12859980218rnzikl65d"))))
    (properties `((upstream-name . "tcltk2")))
    (build-system r-build-system)
    (inputs
     `(("tcl" ,tcl)
       ("tk" ,tk)))
    (home-page "https://www.sciviews.org/SciViews-R")
    (synopsis "Tcl/Tk additions")
    (description
     "This package provides a series of additional Tcl commands and Tk widgets
with style and various functions to supplement the tcltk package")
    (license license:lgpl3)))

(define-public r-accrual
  (package
    (name "r-accrual")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "accrual" version))
       (sha256
        (base32
         "11clm9s5c5518nmp6hd6pjnp0s28y92b2i2x0xgj4j5g816p4j3z"))))
    (properties `((upstream-name . "accrual")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-fgui" ,r-fgui)
       ("r-smpracticals" ,r-smpracticals)
       ("r-tcltk2" ,r-tcltk2)))
    (home-page "https://cran.r-project.org/web/packages/accrual/")
    (synopsis "Bayesian accrual prediction")
    (description
     "Subject recruitment for medical research is challenging.  Slow patient
accrual leads to delay in research.  Accrual monitoring during the process of
recruitment is critical.  Researchers need reliable tools to manage the
accrual rate.  This package provides an implementation of a Bayesian method
that integrates researcher's experience on previous trials and data from the
current study, providing reliable prediction on accrual rate for clinical
studies.  It provides functions for Bayesian accrual prediction which can be
easily used by statisticians and clinical researchers.")
    (license license:gpl2)))

(define-public r-accrued
  (package
    (name "r-accrued")
    (version "1.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "accrued" version))
       (sha256
        (base32
         "05g1jb5914z18rcai1ahn7nihn27vr2rnadwv94gc1j7ivvikvs5"))))
    (properties `((upstream-name . "accrued")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/accrued/")
    (synopsis "Data quality visualization tools for partially accruing data")
    (description
     "This is a package for visualizing data quality of partially accruing
data.")
    (license license:gpl3)))

(define-public r-mda
  (package
    (name "r-mda")
    (version "0.4-10")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "mda" version))
       (sha256
        (base32
         "19g6kn6g0shidrjfffklbmzc5w7mcimrxhagx4nmpslg59ibqdkh"))))
    (properties `((upstream-name . "mda")))
    (build-system r-build-system)
    (propagated-inputs `(("r-class" ,r-class)))
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/mda/")
    (synopsis "Mixture and flexible discriminant analysis")
    (description
     "This is a package for mixture and flexible discriminant analysis,
@dfn{multivariate adaptive regression splines} (MARS), BRUTO, and so on.")
    (license license:gpl2)))

(define-public r-elasticnet
  (package
    (name "r-elasticnet")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "elasticnet" version))
       (sha256
        (base32
         "0p9dplnsp28z4s2fl6afbwrgd0aj339fak8mmndicmrh7bb7rpmb"))))
    (properties `((upstream-name . "elasticnet")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-lars" ,r-lars)))
    (home-page "http://users.stat.umn.edu/~zouxx019/")
    (synopsis "Elastic-Net for sparse estimation and sparse PCA")
    (description
     "This package provides functions for fitting the entire solution path of
the Elastic-Net and also provides functions for estimating sparse Principal
Components.  The Lasso solution paths can be computed by the same function.")
    (license license:gpl2+)))

(define-public r-sparselda
  (package
    (name "r-sparselda")
    (version "0.1-9")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "sparseLDA" version))
       (sha256
        (base32
         "1k3sw9kc40yxnfss4vrsx34qxmv8ssddyhbfjhxrdldvblhbwchb"))))
    (properties `((upstream-name . "sparseLDA")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-elasticnet" ,r-elasticnet)
       ("r-mass" ,r-mass)
       ("r-mda" ,r-mda)))
    (home-page "https://www.imm.dtu.dk/~lkhc/")
    (synopsis "Sparse discriminant analysis")
    (description
     "This package performs sparse linear discriminant analysis for Gaussians
and mixture of Gaussian models.")
    (license license:gpl2+)))

(define-public r-accsda
  (package
    (name "r-accsda")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "accSDA" version))
       (sha256
        (base32
         "0sgxy5y8kkc1n35657kifwfjsba7y5m1vbr7rkk5lmbpkzahqm61"))))
    (properties `((upstream-name . "accSDA")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-ggthemes" ,r-ggthemes)
       ("r-gridextra" ,r-gridextra)
       ("r-mass" ,r-mass)
       ("r-rarpack" ,r-rarpack)
       ("r-sparselda" ,r-sparselda)))
    (home-page "https://github.com/gumeo/accSDA/wiki")
    (synopsis "Accelerated sparse discriminant analysis")
    (description
     "This package provides an implementation of sparse linear discriminant
analysis, which is a supervised classification method for multiple classes.
Various novel optimization approaches to this problem are implemented
including @dfn{alternating direction method of multipliers} (ADMM),
@dfn{proximal gradient} (PG) and @dfn{accelerated proximal gradient} (APG).
Functions for performing cross validation are also supplied along with basic
prediction and plotting functions.  @dfn{Sparse zero variance
discriminant} (SZVD) analysis is also included in the package.")
    (license license:gpl2+)))

(define-public r-ace2fastq
  (package
    (name "r-ace2fastq")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ace2fastq" version))
       (sha256
        (base32
         "09kk3yyqnr2xp820g0p3aai9a21figigjr9lxkr3zjq2d8gzwfic"))))
    (properties `((upstream-name . "ace2fastq")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-stringr" ,r-stringr)))
    (home-page "https://github.com/c5sire/ace2fastq")
    (synopsis "ACE file to FASTQ converter")
    (description
     "The ACE file format is used in genomics to store contigs from sequencing
machines.  This tools converts it into FASTQ format.  Both formats contain the
sequence characters and their corresponding quality information.  Unlike the
FASTQ file, the ACE file stores the quality values numerically.  The
conversion algorithm uses the standard Sanger formula.  The package
facilitates insertion into pipelines, and content inspection.")
    (license license:gpl3)))

(define-public r-rngwell
  (package
    (name "r-rngwell")
    (version "0.10-6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rngWELL" version))
       (sha256
        (base32
         "0pjjcs9pqj7mf0mhb2cwd0aanqpwnm65bm86hk6mi2vw8rgnj2vv"))))
    (properties `((upstream-name . "rngWELL")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/rngWELL/")
    (synopsis "Toolbox for WELL random number generators")
    (description
     "This is a dedicated package to WELL pseudo random generators, which were
introduced in Panneton et al. (2006), ``Improved Long-Period Generators Based
on Linear Recurrences Modulo 2'', ACM Transactions on Mathematical Software.")
    (license license:bsd-3)))

(define-public r-randtoolbox
  (package
    (name "r-randtoolbox")
    (version "1.30.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "randtoolbox" version))
       (sha256
        (base32
         "0qg20ar6qns858jdzqhmfq7yji81czhr6cim257958gqpj66sn95"))))
    (properties `((upstream-name . "randtoolbox")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rngwell" ,r-rngwell)))
    (native-inputs
     `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/randtoolbox/")
    (synopsis "Toolbox for pseudo and quasi random number generation")
    (description
     "This package provides

@enumerate
@item pseudo random generators, such as general linear
congruential generators, multiple recursive generators and generalized
feedback shift register (SF-Mersenne Twister algorithm and WELL
generators)

@item quasi random generators, such as the Torus algorithm, the Sobol
sequence, the Halton sequence (including the Van der Corput sequence), and

@item some generator tests: the gap test, the serial test, the poker test.
@end enumerate

See e.g. Gentle (2003) @url{doi:10.1007/b97336}.")
    (license license:bsd-3)))

(define-public r-lhs
  (package
    (name "r-lhs")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "lhs" version))
       (sha256
        (base32
         "0lzaqr7xi3ckln5nglv5xf5njm359slpz1jc6s02hpsqdw6armd4"))))
    (properties `((upstream-name . "lhs")))
    (build-system r-build-system)
    (propagated-inputs `(("r-rcpp" ,r-rcpp)))
    (home-page "https://github.com/bertcarnell/lhs")
    (synopsis "Latin Hypercube Samples")
    (description
     "This package provides a number of methods for creating and augmenting
Latin Hypercube Samples.")
    (license license:gpl3)))

(define-public r-acebayes
  (package
    (name "r-acebayes")
    (version "1.9")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "acebayes" version))
       (sha256
        (base32
         "1imfwm1vpbb24vfmfn1nljhmaz8429mwjihw34892p387s8h7xz2"))))
    (properties `((upstream-name . "acebayes")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-compare" ,r-compare)
       ("r-lhs" ,r-lhs)
       ("r-randtoolbox" ,r-randtoolbox)
       ("r-rcpp" ,r-rcpp)
       ("r-rcpparmadillo" ,r-rcpparmadillo)))
    (home-page "https://cran.r-project.org/web/packages/acebayes/")
    (synopsis "Optimal Bayesian experimental design using the ACE algorithm")
    (description
     "Finding an optimal Bayesian experimental design involves maximizing an
objective function given by the expectation of some appropriately chosen
utility function with respect to the joint distribution of unknown
quantities (including responses).  This objective function is usually not
available in closed form and the design space can be continuous and of high
dimensionality.  This package uses @dfn{Approximate Coordinate Exchange} (ACE)
to maximise an approximation to the expectation of the utility function.")
    (license license:gpl2)))

(define-public r-acet
  (package
    (name "r-acet")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ACEt" version))
       (sha256
        (base32
         "0626d6kg485xjya164wagrr5z223jvi93ywbwpdns7fkm03c0dlq"))))
    (properties `((upstream-name . "ACEt")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bh" ,r-bh)
       ("r-mass" ,r-mass)
       ("r-rcpp" ,r-rcpp)
       ("r-rcpparmadillo" ,r-rcpparmadillo)))
    (home-page "https://cran.r-project.org/web/packages/ACEt/")
    (synopsis "Estimating dynamic heritability and twin model comparison")
    (description
     "This package supports twin models that are able to estimate the dynamic
behaviour of the variance components in the classical twin models with respect
to age using B-splines and P-splines.")
    (license license:gpl2+)))

(define-public r-acfmperiod
  (package
    (name "r-acfmperiod")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "acfMPeriod" version))
       (sha256
        (base32
         "1yww8isfrbs2v9s94hx7p2imyszcgadwafdgpj438n2ik0q6p9d5"))))
    (properties `((upstream-name . "acfMPeriod")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-mass" ,r-mass)))
    (home-page "https://cran.r-project.org/web/packages/acfMPeriod/")
    (synopsis "Estimation of the ACF from the M-periodogram")
    (description
     "This package support non-robust and robust computations of the sample
autocovariance (ACOVF) and sample autocorrelation functions (ACF) of
univariate and multivariate processes.  The methodology consists in reversing
the diagonalization procedure involving the periodogram or the
cross-periodogram and the Fourier transform vectors, and, thus, obtaining the
ACOVF or the ACF as discussed in Fuller (1995)
@url{doi:10.1002/9780470316917}.  The robust version is obtained by fitting
robust M-regressors to obtain the M-periodogram or M-cross-periodogram as
discussed in Reisen et al. (2017) @url{doi:10.1016/j.jspi.2017.02.008}.")
    (license license:gpl2+)))

(define-public r-gamlss-data
  (package
    (name "r-gamlss-data")
    (version "5.1-4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "gamlss.data" version))
       (sha256
        (base32
         "1dgfspbmps6ipzcmw681wjdp320nm50dwsxafgrcwxndqgc7fdqd"))))
    (properties `((upstream-name . "gamlss.data")))
    (build-system r-build-system)
    (home-page "http://www.gamlss.org/")
    (synopsis "GAMLSS data")
    (description
     "This package provides data used as examples to demonstrate GAMLSS
models.")
    ;; Either version of the license
    (license (list license:gpl2 license:gpl3))))

(define-public r-gamlss
  (package
    (name "r-gamlss")
    (version "5.1-6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "gamlss" version))
       (sha256
        (base32
         "16b7ick1khvldbvfmmpw9cjs1vznnrisvifq7717fxzd8c9s5jdr"))))
    (properties `((upstream-name . "gamlss")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-gamlss-data" ,r-gamlss-data)
       ("r-gamlss-dist" ,r-gamlss-dist)
       ("r-mass" ,r-mass)
       ("r-nlme" ,r-nlme)
       ("r-survival" ,r-survival)))
    (home-page "http://www.gamlss.org/")
    (synopsis "Generalized additive models for location scale and shape")
    (description
     "This package provides functions for fitting the generalized additive
models for location scale and shape introduced by Rigby and
Stasinopoulos (2005), @url{doi:10.1111/j.1467-9876.2005.00510.x}.  The models
use a distributional regression approach where all the parameters of the
conditional distribution of the response variable are modelled using
explanatory variables.")
    ;; Either version of the license
    (license (list license:gpl2 license:gpl3))))

(define-public r-acid
  (package
    (name "r-acid")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "acid" version))
       (sha256
        (base32
         "030i0y8s283ivbsmjccpbv9v7mgbcg2jk9df7vgcbbns74swf9hd"))))
    (properties `((upstream-name . "acid")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-gamlss" ,r-gamlss)
       ("r-gamlss-dist" ,r-gamlss-dist)
       ("r-hmisc" ,r-hmisc)))
    (home-page "https://cran.r-project.org/web/packages/acid/")
    (synopsis "Analysing conditional income distributions")
    (description
     "This package provides functions for the analysis of income distributions
for subgroups of the population as defined by a set of variables like age,
gender, region, etc.  This entails a Kolmogorov-Smirnov test for a mixture
distribution as well as functions for moments, inequality measures, entropy
measures and polarisation measures of income distributions.  This package thus
aides the analysis of income inequality by offering tools for the exploratory
analysis of income distributions at the disaggregated level.")
    (license license:gpl3)))

(define-public r-acm4r
  (package
    (name "r-acm4r")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "acm4r" version))
       (sha256
        (base32
         "1wqzc35i1rshx0zlmas8y4qkkvy6h9r4i4apscjjv1xg2wjflzxa"))))
    (properties `((upstream-name . "acm4r")))
    (build-system r-build-system)
    (propagated-inputs `(("r-mass" ,r-mass)))
    (home-page "https://cran.r-project.org/web/packages/acm4r/")
    (synopsis "Align-and-count method comparisons of RFLP data")
    (description
     "This is a package to compare sequence fragment lengths or molecular
weights from pairs of lanes.  The number of matching bands in the
@dfn{Restriction Fragment Length Polymorphism} (RFLP) data is calculated using
the align-and-count method.")
    ;; Any version of the GPL
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-filematrix
  (package
    (name "r-filematrix")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "filematrix" version))
       (sha256
        (base32
         "1v3aj1ng742msb0sfdnjsbqb508mqjf8jlq2v33vxldhradw5w0b"))))
    (properties `((upstream-name . "filematrix")))
    (build-system r-build-system)
    ;; These inputs are needed for vignettes
    (native-inputs
     `(("r-knitr" ,r-knitr)
       ("r-rmarkdown" ,r-rmarkdown)
       ("pandoc-citeproc" ,ghc-pandoc-citeproc)))
    (home-page "https://github.com/andreyshabalin/filematrix")
    (synopsis "File-backed matrix class with convenient read and write access")
    (description
     "This package provides an interface for working with large matrices
stored in files, not in computer memory.  It supports multiple non-character
data types (double, integer, logical and raw) of various sizes (e.g.  8 and 4
byte real values).  Access to parts of the matrix is done by indexing, exactly
as with usual R matrices.  It supports very large matrices; the package has
been tested on multi-terabyte matrices.  It allows for more than 2^32 rows or
columns, ad allows for quick addition of extra columns to a filematrix.")
    (license license:lgpl3)))

(define-public r-acmeeqtl
  (package
    (name "r-acmeeqtl")
    (version "1.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ACMEeqtl" version))
       (sha256
        (base32
         "049xjv2ym35bbn43zwi68cq27fwdh404vp0r2ca5gxgmmx8kj1cz"))))
    (properties `((upstream-name . "ACMEeqtl")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-filematrix" ,r-filematrix)))
    (home-page "https://github.com/andreyshabalin/ACMEeqtl")
    (synopsis "Estimation of interpretable eQTL effect sizes")
    (description
     "This package provides a non-linear model, termed ACME, that reflects a
parsimonious biological model for allelic contributions of cis-acting eQTLs.
With non-linear least-squares algorithm the maximum likelihood parameters can
be estimated.  The ACME model provides interpretable effect size estimates and
p-values with well controlled Type-I error.")
    (license license:lgpl3)))

(define-public r-acmer
  (package
    (name "r-acmer")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "acmeR" version))
       (sha256
        (base32
         "000b2hqlhj93958nddw0fqb15ahigs08najv2miivym046x04mf7"))))
    (properties `((upstream-name . "acmeR")))
    (build-system r-build-system)
    (propagated-inputs `(("r-foreign" ,r-foreign)))
    (home-page "https://cran.r-project.org/web/packages/acmeR/")
    (synopsis "ACME estimator of bird and bat mortality by wind turbines")
    (description
     "This package provides an implementation of the ACME estimator, described
in Wolpert (2015), ACME: A Partially Periodic Estimator of Avian & Chiropteran
Mortality at Wind Turbines.  Unlike most other models, this estimator supports
decreasing-hazard Weibull model for persistence; decreasing search proficiency
as carcasses age; variable bleed-through at successive searches; and interval
mortality estimates.  The package provides, based on search data, functions
for estimating the mortality inflation factor in Frequentist and Bayesian
settings.")
    (license license:expat)))

(define-public r-r-huge
  (package
    (name "r-r-huge")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "R.huge" version))
       (sha256
        (base32
         "13p558qalv60pgr24nsm6mi92ryj65rsbqa6pgdwy0snjqx12bgi"))))
    (properties `((upstream-name . "R.huge")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-r-methodss3" ,r-r-methodss3)
       ("r-r-oo" ,r-r-oo)
       ("r-r-utils" ,r-r-utils)))
    (home-page "https://github.com/HenrikBengtsson/R.huge")
    (synopsis "Methods for accessing huge amounts of data")
    (description
     "This is a deprecated package for accessing huge amounts of data.
  Cross-platform alternatives are the following packages: bigmemory (CRAN),
ff (CRAN), or BufferedMatrix (Bioconductor).  The main usage of it was inside
the @code{aroma.affymetrix} package.")
    (license license:lgpl2.1+)))

(define-public r-r-filesets
  (package
    (name "r-r-filesets")
    (version "2.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "R.filesets" version))
       (sha256
        (base32
         "124rygq0bl9n4akxcm868nl30cyk3rz0iprb98zlpk62gci9f5fg"))))
    (properties `((upstream-name . "R.filesets")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-digest" ,r-digest)
       ("r-r-cache" ,r-r-cache)
       ("r-r-methodss3" ,r-r-methodss3)
       ("r-r-oo" ,r-r-oo)
       ("r-r-utils" ,r-r-utils)))
    (home-page "https://github.com/HenrikBengtsson/R.filesets")
    (synopsis "Easy handling of and access to files")
    (description
     "This package provides classes and methods to locate, setup, subset,
navigate and iterate file sets, i.e. sets of files located in one or more
directories on the file system.  The API is designed such that these classes
can be extended via inheritance to provide a richer API for special file
formats.  Moreover, a specific name format is defined such that filenames and
directories can be considered to have full names which consists of a name
followed by comma-separated tags.  This adds additional flexibility to
identify file sets and individual files.")
    (license license:lgpl2.1+)))

(define-public r-r-devices
  (package
    (name "r-r-devices")
    (version "2.16.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "R.devices" version))
       (sha256
        (base32
         "15zlnq3g27whq26fbcy5zfl5hiddm256h4rga4frblg6wqlbkvdd"))))
    (properties `((upstream-name . "R.devices")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-base64enc" ,r-base64enc)
       ("r-r-methodss3" ,r-r-methodss3)
       ("r-r-oo" ,r-r-oo)
       ("r-r-utils" ,r-r-utils)))
    (home-page "https://github.com/HenrikBengtsson/R.devices")
    (synopsis "Unified handling of graphics devices")
    (description
     "This package provides functions for creating plots and image files in a
unified way regardless of output format (EPS, PDF, PNG, SVG, TIFF, WMF, etc.).
Default device options as well as scales and aspect ratios are controlled in a
uniform way across all device types.  Switching output format requires minimal
changes in code.  This package is ideal for large-scale batch processing,
because it will never leave open graphics devices or incomplete image files
behind, even on errors or user interrupts.")
    (license license:lgpl2.1+)))

(define-public r-acnr
  (package
    (name "r-acnr")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "acnr" version))
       (sha256
        (base32
         "087hq4i7jp67ba2finzsqjfnqbiprl33na6ryjv9zqzsdawj9cym"))))
    (properties `((upstream-name . "acnr")))
    (build-system r-build-system)
    (home-page "https://github.com/mpierrejean/acnr")
    (synopsis "Annotated copy-number regions")
    (description
     "This package provides SNP array data from different types of copy-number
regions.  These regions were identified manually by the authors of the package
and may be used to generate realistic data sets with known truth.")
    (license license:lgpl2.1+)))

(define-public r-acopula
  (package
    (name "r-acopula")
    (version "0.9.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "acopula" version))
       (sha256
        (base32
         "0vvbbw8pfs9jwfz5c57lw48pr0qj661r0ys007q6zf9jmlrhx1ln"))))
    (properties `((upstream-name . "acopula")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/acopula/")
    (synopsis "Modelling dependence with multivariate Archimax copulas")
    (description
     "Archimax copulas are a mixture of Archimedean and EV copulas.  This
package provides definitions of several parametric families of generator and
dependence function, computes CDF and PDF, estimates parameters, tests for
goodness of fit, generates random sample and checks copula properties for
custom constructs.  In the 2-dimensional case explicit formulas for density
are used, contrary to higher dimensions when all derivatives are linearly
approximated.  Several non-archimax families (normal, FGM, Plackett) are
provided as well.")
    (license license:gpl2)))

(define-public r-tuner
  (package
    (name "r-tuner")
    (version "1.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "tuneR" version))
       (sha256
        (base32
         "0av978m4h2iqazyfq6n2cgkh4wpllihh7s29lah2nb8ngc0w5hxx"))))
    (properties `((upstream-name . "tuneR")))
    (build-system r-build-system)
    (propagated-inputs `(("r-signal" ,r-signal)))
    (home-page "https://cran.r-project.org/web/packages/tuneR/")
    (synopsis "Analysis of music and speech")
    (description
     "This is a package for the analysis of music and speech.  Analyze music
and speech, extract features like MFCCs, handle wave files and their
representation in various ways, read MP3, read MIDI, perform steps of a
transcription, ...")
    ;; Either of these versions.
    (license (list license:gpl2 license:gpl3))))

(define-public r-seewave
  (package
    (name "r-seewave")
    (version "2.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "seewave" version))
       (sha256
        (base32
         "1qg8f5gik9pw6f9mcxqmrc9x3003s8vdm6g01pjjpyc9qaqiz2vi"))))
    (properties `((upstream-name . "seewave")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-tuner" ,r-tuner)))
    (home-page "http://rug.mnhn.fr/seewave")
    (synopsis "Sound analysis and synthesis")
    (description
     "This package provides functions for analysing, manipulating, displaying,
editing and synthesizing time waves (particularly sound).  This package
processes time analysis (oscillograms and envelopes), spectral content,
resonance quality factor, entropy, cross correlation and autocorrelation,
zero-crossing, dominant frequency, analytic signal, frequency coherence, 2D
and 3D spectrograms and many other analyses.")
    (license license:gpl2+)))

(define-public r-acousticndlcoder
  (package
    (name "r-acousticndlcoder")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "AcousticNDLCodeR" version))
       (sha256
        (base32
         "1fgzgwanpv2pzy74xdk3hamc44p8qch467wh163dxby8jr9ik0sb"))))
    (properties
     `((upstream-name . "AcousticNDLCodeR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-seewave" ,r-seewave)
       ("r-tuner" ,r-tuner)
       ("r-zoo" ,r-zoo)))
    (home-page "https://cran.r-project.org/web/packages/AcousticNDLCodeR/")
    (synopsis "Coding sound files for use with NDL")
    (description
     "Make acoustic cues to use with the R package @code{ndl}.
The package implements functions used in the PLoS ONE paper \"Words from
spontaneous conversational speech can be recognized with human-like accuracy
by an error-driven learning algorithm that discriminates between meanings
straight from smart acoustic features, bypassing the phoneme as recognition
unit.\" @url{doi:10.1371/journal.pone.0174623}")
    (license license:gpl2+)))

(define-public r-acp
  (package
    (name "r-acp")
    (version "2.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "acp" version))
       (sha256
        (base32
         "0lcwbjcyyr32m6qjmjqh25qjwrbyqj1n092xhgbhxzd8fslppnmn"))))
    (properties `((upstream-name . "acp")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-quantmod" ,r-quantmod)
       ("r-tseries" ,r-tseries)))
    (home-page "https://cran.r-project.org/web/packages/acp/")
    (synopsis "Autoregressive conditional Poisson")
    (description
     "This package supports the analysis of count data exhibiting
autoregressive properties, using the @dfn{Autoregressive Conditional Poisson}
model (ACP(p,q)) proposed by Heinen (2003).")
    (license license:gpl2)))

(define-public r-ada
  (package
    (name "r-ada")
    (version "2.0-5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ada" version))
       (sha256
        (base32
         "1h3a07czp0w3hrhjcg1fz721y8vsfclzqi3rq8qfzgpfb4h1f06r"))))
    (properties `((upstream-name . "ada")))
    (build-system r-build-system)
    (propagated-inputs `(("r-rpart" ,r-rpart)))
    (home-page "https://cran.r-project.org/web/packages/ada/")
    (synopsis "Stochastic boosting")
    (description
     "This package provides a straightforward, well-documented, and broad
boosting routine for classification, ideally suited for small to
moderate-sized data sets.  It performs discrete, real, and gentle boost under
both exponential and logistic loss on a given data set.")
    ;; Any version of the GPL.
    (license (list license:gpl2+ license:gpl3+))))

(define-public r-genalg
  (package
    (name "r-genalg")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "genalg" version))
       (sha256
        (base32
         "1wzfamq8k5yhwbdx0wy1w5bks93brj0p890xxc4yqrja4w38ja3s"))))
    (properties `((upstream-name . "genalg")))
    (build-system r-build-system)
    (home-page "https://github.com/egonw/genalg")
    (synopsis "R based genetic algorithm")
    (description
     "This package provides an R based genetic algorithm for binary and
floating point chromosomes.")
    (license license:gpl2)))

(define-public r-kernelfactory
  (package
    (name "r-kernelfactory")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "kernelFactory" version))
       (sha256
        (base32
         "001kw9k3ivd4drd4mwqapkkk3f4jgljiaprhg2630hmll064s89j"))))
    (properties `((upstream-name . "kernelFactory")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-auc" ,r-auc)
       ("r-genalg" ,r-genalg)
       ("r-kernlab" ,r-kernlab)
       ("r-randomforest" ,r-randomforest)))
    (home-page "https://cran.r-project.org/web/packages/kernelFactory/")
    (synopsis "Ensemble of kernel machines")
    (description
     "Kernel factory is an ensemble method where each base classifier (random
forest) is fit on the kernel matrix of a subset of the training data.")
    (license license:gpl2+)))

(define-public r-dummies
  (package
    (name "r-dummies")
    (version "1.5.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "dummies" version))
       (sha256
        (base32
         "01f84crqx17xd6xy55qxlvsj3knm8lhw7jl26p2rh2w3y0nvqlbm"))))
    (properties `((upstream-name . "dummies")))
    (build-system r-build-system)
    (home-page "https://decisionpatterns.com")
    (synopsis "Create dummy/indicator variables flexibly and efficiently")
    (description
     "This package lets you expand factors, characters and other eligible
classes into dummy/indicator variables.")
    (license license:gpl2+)))

(define-public r-acrm
  (package
    (name "r-acrm")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "aCRM" version))
       (sha256
        (base32
         "0kzp568hd9c9a9qgniia5s5gv0q5f89xfvvwpzb197gqhs3x092v"))))
    (properties `((upstream-name . "aCRM")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ada" ,r-ada)
       ("r-dummies" ,r-dummies)
       ("r-kernelfactory" ,r-kernelfactory)
       ("r-randomforest" ,r-randomforest)))
    (home-page "https://cran.r-project.org/web/packages/aCRM/")
    (synopsis "Convenience functions for analytical customer relationship management")
    (description
     "This package provides convenience functions for data preparation and
modeling often used in @dfn{analytical customer relationship
management} (aCRM).")
    (license license:gpl2+)))

(define-public r-treeclust
  (package
    (name "r-treeclust")
    (version "1.1-7")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "treeClust" version))
       (sha256
        (base32
         "1s7kh6q0bkixsygrip95zf1bi10ihddsa5lq9dfxd68yh8rsby6z"))))
    (properties `((upstream-name . "treeClust")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-cluster" ,r-cluster)
       ("r-rpart" ,r-rpart)))
    (home-page "https://cran.r-project.org/web/packages/treeClust/")
    (synopsis "Cluster distances through trees")
    (description
     "This package provides tools to create a measure of inter-point
dissimilarity useful for clustering mixed data, and, optionally, perform the
clustering.")
    (license license:gpl2+)))

(define-public r-acrosstic
  (package
    (name "r-acrosstic")
    (version "1.0-3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "AcrossTic" version))
       (sha256
        (base32
         "03180h79jhjd66ibrnsfp3yyp2jlfysp7cymw46phzj2palghsc0"))))
    (properties `((upstream-name . "AcrossTic")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-lpsolve" ,r-lpsolve)
       ("r-treeclust" ,r-treeclust)))
    (home-page "https://cran.r-project.org/web/packages/AcrossTic/")
    (synopsis "Cost-minimal regular spanning subgraph with TreeClust")
    (description
     "This is a package for constructing minimum-cost regular spanning
subgraph as part of a non-parametric two-sample test for equality of
distribution.")
    (license license:gpl2+)))

(define-public r-acrt
  (package
    (name "r-acrt")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "acrt" version))
       (sha256
        (base32
         "0y9ndcq8ffpfrv7w9rikm4zn68jpsj6baqisq9kp2433xrwzdb6s"))))
    (properties `((upstream-name . "acrt")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rcpp" ,r-rcpp)
       ("r-rcppeigen" ,r-rcppeigen)
       ("r-sandwich" ,r-sandwich)))
    (home-page "https://cran.r-project.org/web/packages/acrt/")
    (synopsis "Autocorrelation robust testing")
    (description
     "This package provides functions for testing affine hypotheses on the
regression coefficient vector in regression models with autocorrelated
errors.")
    (license license:gpl2)))

(define-public r-acs
  (package
    (name "r-acs")
    (version "2.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "acs" version))
       (sha256
        (base32
         "0ajw9rf8l8akcvgqvbxjvryc6wjx74521xyxswz2b0bky3m6kah5"))))
    (properties `((upstream-name . "acs")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-httr" ,r-httr)
       ("r-plyr" ,r-plyr)
       ("r-rcpp" ,r-rcpp)
       ("r-stringr" ,r-stringr)
       ("r-xml" ,r-xml)))
    (home-page "https://dusp.mit.edu/faculty/ezra-haber-glenn")
    (synopsis "Work with data from the US Census")
    (description
     "This package provides a general toolkit for downloading, managing,
analyzing, and presenting data from the
@url{https://www.census.gov/data/developers/data-sets.html, U.S.  Census},
including SF1 (Decennial short-form), SF3 (Decennial long-form), and the
American Community Survey (ACS).  Confidence intervals provided with ACS data
are converted to standard errors to be bundled with estimates in complex
@code{acs} objects.  The package provides new methods to conduct standard
operations on @code{acs} objects and present/plot data in statistically
appropriate ways.")
    (license license:gpl3)))

(define-public r-acss-data
  (package
    (name "r-acss-data")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "acss.data" version))
       (sha256
        (base32
         "09kl4179ipr8bq19g89xcdi1xxs397zcx5cvgp6viy8gn687ilgv"))))
    (properties `((upstream-name . "acss.data")))
    (build-system r-build-system)
    (home-page "http://complexitycalculator.com/methodology.html")
    (synopsis "Data for algorithmic complexity of short strings")
    (description
     "This is a data only package providing the algorithmic complexity of
short strings, computed using the coding theorem method.  For a given set of
symbols in a string, all possible or a large number of random samples of
Turing machines with a given number of states (e.g., 5) and number of symbols
corresponding to the number of symbols in the strings were simulated until
they reached a halting state or failed to end.  This package contains data on
4.5 million strings from length 1 to 12 simulated on Turing machines with 2,
4, 5, 6, and 9 symbols.  The complexity of the string corresponds to the
distribution of the halting states.")
    (license license:gpl2+)))

(define-public r-acss
  (package
    (name "r-acss")
    (version "0.2-5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "acss" version))
       (sha256
        (base32
         "0cqa60544f58l5qd7h6xmsir40b9hqnq6pqgd5hfx2j2l5n7qhmk"))))
    (properties `((upstream-name . "acss")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-acss-data" ,r-acss-data)
       ("r-zoo" ,r-zoo)))
    (home-page "http://complexitycalculator.com/methodology.html")
    (synopsis "Algorithmic complexity for short strings")
    (description
     "The main purpose of this package is to provide the algorithmic
complexity for short strings, an approximation of the Kolmogorov Complexity of
a short string using the coding theorem method.  While the database containing
the complexity is provided in the data only package @code{acss.data}, this
package provides functions accessing the data such as @code{prob_random}
returning the posterior probability that a given string was produced by a
random process.  In addition, two traditional (but problematic) measures of
complexity are also provided: entropy and change complexity.")
    (license license:gpl2+)))

(define-public r-acswr
  (package
    (name "r-acswr")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ACSWR" version))
       (sha256
        (base32
         "195vjrkang5cl7gwsna0aq4p0h4jym9xg9yh94bnf8vq6wf8j83n"))))
    (properties `((upstream-name . "ACSWR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-mass" ,r-mass)))
    (home-page "https://cran.r-project.org/web/packages/ACSWR/")
    (synopsis "Companion package for the book \"A Course in Statistics with R\"")
    (description
     "This is a companion package for the book \"A Course in Statistics with
R\" (ISBN 978-1-119-15272-9.)")
    (license license:gpl2)))

(define-public r-alabama
  (package
    (name "r-alabama")
    (version "2015.3-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "alabama" version))
       (sha256
        (base32
         "0mlgk929gdismikwx4k2ndqq57nnqj7mlgvd3479b214hksgq036"))))
    (properties `((upstream-name . "alabama")))
    (build-system r-build-system)
    (propagated-inputs `(("r-numderiv" ,r-numderiv)))
    (home-page "https://cran.r-project.org/web/packages/alabama/")
    (synopsis "Constrained nonlinear optimization")
    (description
     "Alabama stands for Augmented Lagrangian Adaptive Barrier Minimization
Algorithm; it is used for optimizing smooth nonlinear objective functions with
constraints.  Linear or nonlinear equality and inequality constraints are
allowed.")
    (license license:gpl2+)))

(define-public r-gdina
  (package
    (name "r-gdina")
    (version "2.7.9")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "GDINA" version))
       (sha256
        (base32
         "13pmj069r04h38hg61ibyn1ab15zdy9m0qv60vi25ahgsmg6ccvx"))))
    (properties `((upstream-name . "GDINA")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-alabama" ,r-alabama)
       ("r-ggplot2" ,r-ggplot2)
       ("r-mass" ,r-mass)
       ("r-nloptr" ,r-nloptr)
       ("r-numderiv" ,r-numderiv)
       ("r-rcpp" ,r-rcpp)
       ("r-rcpparmadillo" ,r-rcpparmadillo)
       ("r-rsolnp" ,r-rsolnp)
       ("r-shiny" ,r-shiny)
       ("r-shinydashboard" ,r-shinydashboard)))
    (home-page "https://github.com/Wenchao-Ma/GDINA")
    (synopsis "Generalized DINA model framework")
    (description
     "This package provides a set of psychometric tools for cognitive
diagnosis modeling based on the generalized deterministic inputs, noisy and
gate (G-DINA) model by de la Torre (2011) @url{doi:10.1007/s11336-011-9207-7}
and its extensions, including the sequential G-DINA model by Ma and de la
Torre (2016) @url{doi:10.1111/bmsp.12070} for polytomous responses, and the
polytomous G-DINA model by Chen and de la Torre
@url{doi:10.1177/0146621613479818} for polytomous attributes.  Joint attribute
distribution can be independent, saturated, higher-order, loglinear smoothed
or structured.  Q-matrix validation, item and model fit statistics, model
comparison at test and item level and differential item functioning can also
be conducted.  A graphical user interface is also provided.")
    (license license:gpl3)))

(define-public r-actcd
  (package
    (name "r-actcd")
    (version "1.2-0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ACTCD" version))
       (sha256
        (base32
         "0mzjxxr2zfdidw8ibh6w8mvpkw3q3nvngqi05g9ind26ckvk053p"))))
    (properties `((upstream-name . "ACTCD")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-gdina" ,r-gdina)
       ("r-r-methodss3" ,r-r-methodss3)))
    (native-inputs
     `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/ACTCD/")
    (synopsis "Asymptotic classification theory for cognitive diagnosis")
    (description
     "This is a package supporting cluster analysis for cognitive diagnosis
based on the Asymptotic Classification Theory (Chiu, Douglas & Li, 2009;
@url{doi:10.1007/s11336-009-9125-0}).  Given the sample statistic of
sum-scores, cluster analysis techniques can be used to classify examinees into
latent classes based on their attribute patterns.  In addition to the
algorithms used to classify data, three labeling approaches are proposed to
label clusters so that examinees' attribute profiles can be obtained.")
    (license license:gpl2+)))

(define-public r-ineq
  (package
    (name "r-ineq")
    (version "0.2-13")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ineq" version))
       (sha256
        (base32
         "09fsxyrh0j7mwmb5hkhmrzgcy7kf85jxkh7zlwpgqgcsyl1n91z0"))))
    (properties `((upstream-name . "ineq")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/ineq/")
    (synopsis "Measuring inequality, concentration, and poverty")
    (description
     "This package provides tools for measuring inequality, concentration, and
poverty measures.  It provides both empirical and theoretical Lorenz curves.")
    ;; Either of these two versions.
    (license (list license:gpl2 license:gpl3))))

(define-public r-actfrag
  (package
    (name "r-actfrag")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ActFrag" version))
       (sha256
        (base32
         "08r3gwjz4fkyy85dxqix0ffm5xyq45032qv3snnzxnlqxslxbqn1"))))
    (properties `((upstream-name . "ActFrag")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-accelerometry" ,r-accelerometry)
       ("r-dplyr" ,r-dplyr)
       ("r-ineq" ,r-ineq)
       ("r-survival" ,r-survival)
       ("r-tidyr" ,r-tidyr)))
    (home-page "https://github.com/junruidi/ActFrag")
    (synopsis "Activity fragmentation metrics extraction")
    (description
     "This package provides functions to extract commonly used fragmentation
metrics to quantify time accumulation strategies based on minute level
actigraphy-measured activity counts data.")
    (license license:gpl3)))

(define-public r-fda
  (package
    (name "r-fda")
    (version "2.4.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "fda" version))
       (sha256
        (base32
         "0g50kj1dx7zarjv0lgwyzd2c7bv6di7nkndmywday5vjywgl8m7a"))))
    (properties `((upstream-name . "fda")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-matrix" ,r-matrix)))
    (home-page "https://www.functionaldata.org")
    (synopsis "Functional data analysis")
    (description
     "These functions were developed to support functional data analysis as
described in Ramsay, J. O. and Silverman, B. W. (2005) Functional Data
Analysis.  The package includes data sets and script files working many
examples.")
    (license license:gpl2+)))

(define-public r-actigraphy
  (package
    (name "r-actigraphy")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Actigraphy" version))
       (sha256
        (base32
         "0bpmvszzv5fm72nar3wgnmfl5am7znqfajmlbv38ihkbi7jbwk20"))))
    (properties `((upstream-name . "Actigraphy")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-fda" ,r-fda)))
    (home-page "https://cran.r-project.org/web/packages/Actigraphy/")
    (synopsis "Actigraphy data analysis")
    (description
     "This package provides tools for functional linear modeling and analysis
of actigraphy data.")
    (license license:asl2.0)))

(define-public r-activedriver
  (package
    (name "r-activedriver")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ActiveDriver" version))
       (sha256
        (base32
         "10c7ga48fwvpd5mc4dqiyw4kc2l0iz5qn4hg7xk15r1qmm5rsipa"))))
    (properties `((upstream-name . "ActiveDriver")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-mass" ,r-mass)))
    (home-page "https://cran.r-project.org/web/packages/ActiveDriver/")
    (synopsis "Tools for finding cancer driver proteins")
    (description
     "This package provides a mutation analysis tool that discovers cancer
driver genes with frequent mutations in protein signalling sites such as
post-translational modifications (phosphorylation, ubiquitination, etc).  The
Poisson generalized linear regression model identifies genes where cancer
mutations in signalling sites are more frequent than expected from the
sequence of the entire gene.  Integration of mutations with signalling
information helps find new driver genes and propose candidate mechanisms to
known drivers.")
    (license license:gpl2+)))

(define-public r-activitycounts
  (package
    (name "r-activitycounts")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "activityCounts" version))
       (sha256
        (base32
         "0zgxr2sk3a8kmygfxx1p5hnrfwdkxx7php6jlrhm8wv6052ck8jz"))))
    (properties
     `((upstream-name . "activityCounts")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-lubridate" ,r-lubridate)
       ("r-magrittr" ,r-magrittr)
       ("r-seewave" ,r-seewave)
       ("r-signal" ,r-signal)
       ("r-tibble" ,r-tibble)))
    (home-page "https://github.com/walkabillylab/activityCounts")
    (synopsis "Generate ActiLife counts")
    (description
     "ActiLife generates activity counts from data collected by Actigraph
accelerometers.  Actigraph is one of the most common research-grade
accelerometers.  There is considerable research validating and developing
algorithms for human activity using ActiLife counts.  Unfortunately, ActiLife
counts are proprietary and difficult to implement if researchers use different
accelerometer brands.  The code creates ActiLife counts from raw acceleration
data for different accelerometer brands.")
    (license license:gpl3)))

(define-public r-activityindex
  (package
    (name "r-activityindex")
    (version "0.3.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ActivityIndex" version))
       (sha256
        (base32
         "14k6d78s15j7kb7jhixf4msrdjdl28d0r264cbvy41p8dkq7ysvk"))))
    (properties `((upstream-name . "ActivityIndex")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-data-table" ,r-data-table)
       ("r-matrixstats" ,r-matrixstats)
       ("r-r-utils" ,r-r-utils)))
    (home-page "https://cran.r-project.org/web/packages/ActivityIndex/")
    (synopsis "Activity Index calculation using raw accelerometry data")
    (description
     "This is a package to read raw accelerometry from GT3X+ accelerometry
data and plain table data to calculate the Activity Index from Bai et
al. (2016) @url{doi:10.1371/journal.pone.0160644}.")
    (license license:gpl3)))

(define-public r-activpal
  (package
    (name "r-activpal")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "activPAL" version))
       (sha256
        (base32
         "1h6hp5z89ji73gdzxy1dgbfwjysiy5lvcqh90xagpb7sa7ahs3na"))))
    (properties `((upstream-name . "activPAL")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-devtools" ,r-devtools)
       ("r-dplyr" ,r-dplyr)
       ("r-ggplot2" ,r-ggplot2)
       ("r-lubridate" ,r-lubridate)
       ("r-magrittr" ,r-magrittr)
       ("r-tidyr" ,r-tidyr)))
    (home-page "https://cran.r-project.org/web/packages/activPAL")
    (synopsis "Processing and chart generation from activPAL events files")
    (description
     "This package contains functions to generate pre-defined summary
statistics from activPAL events files.  The package also contains functions to
produce informative graphics that visualize physical activity behaviour and
trends.  This includes generating graphs that align physical activity
behaviour with additional time based observations described by other data
sets, such as sleep diaries and continuous glucose monitoring data.")
    (license license:gpl3)))

(define-public r-activpalprocessing
  (package
    (name "r-activpalprocessing")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "activpalProcessing" version))
       (sha256
        (base32
         "1y0bjx2qx53iy930y9iww4q1yzjj8y16cwgixk1mq3w4g1f116d1"))))
    (properties
     `((upstream-name . "activpalProcessing")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-chron" ,r-chron)))
    (home-page "https://cran.r-project.org/web/packages/activpalProcessing/")
    (synopsis "Process activPAL events files")
    (description
     "This package performs estimation of physical activity and sedentary
behavior variables from activPAL events files.")
    ;; Either version of the GPL.
    (license (list license:gpl2 license:gpl3))))

(define-public r-actogrammr
  (package
    (name "r-actogrammr")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "actogrammr" version))
       (sha256
        (base32
         "1jzvarmd41yqlrkagzlc8m19n5mn0w0b36fy50lyvgrfsafjfbqa"))))
    (properties `((upstream-name . "actogrammr")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-dplyr" ,r-dplyr)
       ("r-ggplot2" ,r-ggplot2)
       ("r-lubridate" ,r-lubridate)
       ("r-readr" ,r-readr)
       ("r-tidyr" ,r-tidyr)))
    (home-page "https://cran.r-project.org/web/packages/actogrammr/")
    (synopsis "Read in activity data and plot actograms")
    (description
     "Read in activity measurements from standard file formats used by
circadian rhythm researchers, currently only ClockLab format, and process and
plot the data.  The central type of plot is the actogram, as first described
in \"Activity and distribution of certain wild mice in relation to biotic
communities\" by MS Johnson (1926) @url{doi:10.2307/1373575}.")
    (license license:gpl3)))

(define-public r-expint
  (package
    (name "r-expint")
    (version "0.1-6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "expint" version))
       (sha256
        (base32
         "0iai25cglcdnf44d2d1fz1xpw4q600my4zq4493fk4cs5673mlf7"))))
    (properties `((upstream-name . "expint")))
    (build-system r-build-system)
    (home-page "https://gitlab.com/vigou3/expint")
    (synopsis "Exponential integral and incomplete Gamma function")
    (description
     "This package provides the exponential integrals @code{E_1(x)},
@code{E_2(x)}, @code{E_n(x)} and @code{Ei(x)}, and the incomplete gamma
function @code{G(a, x)} defined for negative values of its first argument.
The package also gives easy access to the underlying C routines through an
API; see the package vignette for details.")
    (license license:gpl2+)))

(define-public r-actuar
  (package
    (name "r-actuar")
    (version "2.3-3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "actuar" version))
       (sha256
        (base32
         "0aw3hlan5y22mdqk1wvnw9ksqhwp4yy5hi0dpv21p7s0hyxhphih"))))
    (properties `((upstream-name . "actuar")))
    (build-system r-build-system)
    (propagated-inputs `(("r-expint" ,r-expint)))
    (home-page "https://gitlab.com/vigou3/actuar")
    (synopsis "Actuarial functions and heavy tailed distributions")
    (description
     "This package provides functions and data sets for actuarial science:
modeling of loss distributions; risk theory and ruin theory; simulation of
compound models, discrete mixtures and compound hierarchical models;
credibility theory.  It boasts support for many additional probability
distributions to model insurance loss amounts and loss frequency: 19
continuous heavy tailed distributions; the Poisson-inverse Gaussian discrete
distribution; zero-truncated and zero-modified extensions of the standard
discrete distributions.  It also supports phase-type distributions commonly
used to compute ruin probabilities.")
    (license license:gpl2+)))

(define-public r-bmp
  (package
    (name "r-bmp")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "bmp" version))
       (sha256
        (base32
         "0jd67r11bn98hjwgyr6gas423787xy7ji2hq7ay80blkkcj91xxx"))))
    (properties `((upstream-name . "bmp")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/bmp/")
    (synopsis "Read Bitmap (BMP) images")
    (description
     "This package provides pure R tools to read BMP format images.  It is
currently limited to 8 bit greyscale images and 24, 32 bit (A)RGB images.")
    (license license:gpl2+)))

(define-public r-readbitmap
  (package
    (name "r-readbitmap")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "readbitmap" version))
       (sha256
        (base32
         "14825906l326w59g6apy00j55jg3h5kx2r6s031f4gdkbrc7szbk"))))
    (properties `((upstream-name . "readbitmap")))
    (build-system r-build-system)
    (inputs
     `(("libjpeg" ,libjpeg)
       ("libpng" ,libpng)))
    (propagated-inputs
     `(("r-bmp" ,r-bmp)
       ("r-jpeg" ,r-jpeg)
       ("r-png" ,r-png)
       ("r-tiff" ,r-tiff)))
    (home-page "https://github.com/jefferis/readbitmap")
    (synopsis "Unified interface to read bitmap images (BMP, JPEG, PNG, TIFF)")
    (description
     "This package provides tools to identify and read BMP, JPEG, PNG, and
TIFF format bitmap images.  Identification defaults to the use of the magic
number embedded in the file rather than the file extension.")
    (license license:gpl2+)))

(define-public r-imager
  (package
    (name "r-imager")
    (version "0.42.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "imager" version))
       (sha256
        (base32
         "1d7a49lcna77wyfjf5q1b89jck3p3vnysnkgz4drb0qkpy6hz76b"))))
    (properties `((upstream-name . "imager")))
    (build-system r-build-system)
    (inputs
     `(("fftw" ,fftw)
       ("libtiff" ,libtiff)
       ("libx11" ,libx11)
       ("zlib" ,zlib)))
    (propagated-inputs
     `(("r-downloader" ,r-downloader)
       ("r-igraph" ,r-igraph)
       ("r-jpeg" ,r-jpeg)
       ("r-magrittr" ,r-magrittr)
       ("r-png" ,r-png)
       ("r-purrr" ,r-purrr)
       ("r-rcpp" ,r-rcpp)
       ("r-readbitmap" ,r-readbitmap)
       ("r-stringr" ,r-stringr)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://dahtah.github.io/imager/")
    (synopsis "Image processing library")
    (description
     "This is a package for fast image processing for images in up to 4
dimensions (two spatial dimensions, one time/depth dimension, one color
dimension).  It provides most traditional image processing tools (filtering,
morphology, transformations, etc.) as well as various functions for easily
analyzing image data using R.  The package wraps @url{http://cimg.eu, CImg}, a
simple, modern C++ library for image processing.")
    (license license:lgpl3)))

(define-public r-acuityview
  (package
    (name "r-acuityview")
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "AcuityView" version))
       (sha256
        (base32
         "0f0iclmnwdc8ixiiai4svk4x1g3pjy7dhm3cm58fv6ckx12d3d2l"))))
    (properties `((upstream-name . "AcuityView")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-fftwtools" ,r-fftwtools)
       ("r-imager" ,r-imager)
       ("r-plotrix" ,r-plotrix)))
    (home-page "https://cran.r-project.org/web/packages/AcuityView/")
    (synopsis "Display scenes as seen by an animal with less acute vision")
    (description
     "This package provides a simple method for representing a visual scene as
it may be seen by an animal with less acute vision.")
    (license license:gpl2+)))

(define-public r-caret
  (package
    (name "r-caret")
    (version "6.0-85")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "caret" version))
       (sha256
        (base32
         "0jxbf2zcvbb5s2pnjzg182awjvylc57q7z5plx6gs6gm62zxjafs"))))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-foreach" ,r-foreach)
       ("r-ggplot2" ,r-ggplot2)
       ("r-lattice" ,r-lattice)
       ("r-modelmetrics" ,r-modelmetrics)
       ("r-nlme" ,r-nlme)
       ("r-plyr" ,r-plyr)
       ("r-proc" ,r-proc)
       ("r-recipes" ,r-recipes)
       ("r-reshape2" ,r-reshape2)
       ("r-withr" ,r-withr)))
    (home-page "https://github.com/topepo/caret")
    (synopsis "Classification and regression training")
    (description
     "This package provides miscellaneous functions for training and plotting
classification and regression models.")
    (license license:gpl2+)))

(define-public r-adabag
  (package
    (name "r-adabag")
    (version "4.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "adabag" version))
       (sha256
        (base32
         "109wrl1pwvmyv2l909hrvk7dg4aa9pv449mvdycp50zwrsw9w0a7"))))
    (properties `((upstream-name . "adabag")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-caret" ,r-caret)
       ("r-doparallel" ,r-doparallel)
       ("r-foreach" ,r-foreach)
       ("r-rpart" ,r-rpart)))
    (home-page "https://cran.r-project.org/web/packages/adabag/")
    (synopsis "Multiclass AdaBoost.M1, SAMME and Bagging")
    (description
     "This package implements Freund and Schapire's Adaboost.M1 algorithm and
Breiman's Bagging algorithm using classification trees as individual
classifiers.  Once these classifiers have been trained, they can be used to
predict on new data.  Also, cross validation estimation of the error can be
done.")
    (license license:gpl2+)))

(define-public r-adagio
  (package
    (name "r-adagio")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "adagio" version))
       (sha256
        (base32
         "1h9l0ddrxq8y35iy9hdkxdvdwsqpnpkzzbkbwwhm4380lq1m7a3k"))))
    (properties `((upstream-name . "adagio")))
    (build-system r-build-system)
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/adagio/")
    (synopsis "Discrete and global optimization routines")
    (description
     "This package provides methods and algorithms for discrete optimization,
e.g. knapsack and subset sum procedures, derivative-free Nelder-Mead and
Hooke-Jeeves minimization, and some (evolutionary) global optimization
functions.")
    (license license:gpl3+)))

(define-public r-univoutl
  (package
    (name "r-univoutl")
    (version "0.1-5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "univOutl" version))
       (sha256
        (base32
         "193wrpkvgmlrx43nag8w3ivrlqm37nm6g86wcvd3bgw3hchs70gi"))))
    (properties `((upstream-name . "univOutl")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-hmisc" ,r-hmisc)
       ("r-robustbase" ,r-robustbase)))
    (home-page "https://github.com/marcellodo/univOutl")
    (synopsis "Detection of univariate outliers")
    (description
     "This package provides well-known outlier detection techniques in the
univariate case.  Methods to deal with skewed distribution are included too.
The Hidiroglou-Berthelot (1986) method to search for outliers in ratios of
historical data is implemented as well.  When available, survey weights can be
used in outliers detection.")
    (license license:gpl2+)))

(define-public r-tolerance
  (package
    (name "r-tolerance")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "tolerance" version))
       (sha256
        (base32
         "001snzr2ipag3zprynydlbi9prkjzrllc054qh7m0qwkb3r19jjd"))))
    (properties `((upstream-name . "tolerance")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-mass" ,r-mass)
       ("r-rgl" ,r-rgl)))
    (home-page "https://cran.r-project.org/web/packages/tolerance/")
    (synopsis "Statistical tolerance intervals and regions")
    (description
     "This package provides functions for estimating tolerance
limits (intervals) for various univariate distributions (binomial, Cauchy,
discrete Pareto, exponential, two-parameter exponential, extreme value,
hypergeometric, Laplace, logistic, negative binomial, negative hypergeometric,
normal, Pareto, Poisson-Lindley, Poisson, uniform, and Zipf-Mandelbrot),
Bayesian normal tolerance limits, multivariate normal tolerance regions,
nonparametric tolerance intervals, tolerance bands for regression
settings (linear regression, nonlinear regression, nonparametric regression,
and multivariate regression), and analysis of variance tolerance intervals.
Visualizations are also available for most of these settings.")
    (license license:gpl2+)))

(define-public r-additivitytests
  (package
    (name "r-additivitytests")
    (version "1.1-4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "additivityTests" version))
       (sha256
        (base32
         "048ds90wqjdjy1nyhna3m06asdklbh8sx1n556kss2j1r1pma1sw"))))
    (properties
     `((upstream-name . "additivityTests")))
    (build-system r-build-system)
    (home-page "https://github.com/simecek/additivityTests")
    (synopsis "Additivity tests in the two way Anova with single sub-class numbers")
    (description
     "This package provides an implementation of the Tukey, Mandel,
Johnson-Graybill, LBI, Tusell and modified Tukey non-additivity tests.")
    (license license:gpl3)))

(define-public r-flexclust
  (package
    (name "r-flexclust")
    (version "1.4-0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "flexclust" version))
       (sha256
        (base32
         "0x7wxk8y46ndyz6fdacym0rd6p9wh3pcfr28chjcg5d7fm849zl2"))))
    (properties `((upstream-name . "flexclust")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-class" ,r-class)
       ("r-lattice" ,r-lattice)
       ("r-modeltools" ,r-modeltools)))
    (home-page "https://cran.r-project.org/web/packages/flexclust/")
    (synopsis "Flexible cluster algorithms")
    (description
     "The main function @code{kcca} implements a general framework for
k-centroids cluster analysis supporting arbitrary distance measures and
centroid computation.  Further cluster methods include hard competitive
learning, neural gas, and QT clustering.  There are numerous visualization
methods for cluster results (neighborhood graphs, convex cluster hulls,
barcharts of centroids, ...), and bootstrap methods for the analysis of
cluster stability.")
    (license license:gpl2)))

(define-public r-biclust
  (package
    (name "r-biclust")
    (version "2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "biclust" version))
       (sha256
        (base32
         "1pk7mvwlg4hkc4cn4w6wr2c192qx03d1xfwlzclk5bw1nmcg483b"))))
    (properties `((upstream-name . "biclust")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-additivitytests" ,r-additivitytests)
       ("r-colorspace" ,r-colorspace)
       ("r-flexclust" ,r-flexclust)
       ("r-ggplot2" ,r-ggplot2)
       ("r-lattice" ,r-lattice)
       ("r-mass" ,r-mass)
       ("r-tidyr" ,r-tidyr)))
    (home-page "https://cran.r-project.org/web/packages/biclust/")
    (synopsis "BiCluster algorithms")
    (description
     "The main function @code{biclust()} provides several algorithms to find
biclusters in two-dimensional data, spectral, plaid model, xmotifs, and bimax.
In addition, the package provides methods for data
preprocessing (normalization and discretization), visualization, and
validation of bicluster solutions.")
    (license license:gpl3)))

(define-public r-icge
  (package
    (name "r-icge")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ICGE" version))
       (sha256
        (base32
         "0xin7zml1nbygyi08hhg3wwr2jr1zcsvrlgia89zp4xanxlzgaqa"))))
    (properties `((upstream-name . "ICGE")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-cluster" ,r-cluster)
       ("r-mass" ,r-mass)))
    (home-page "https://cran.r-project.org/web/packages/ICGE/")
    (synopsis "Cluster estimation and identification of atypical units")
    (description
     "ICGE is a package that helps to estimate the number of real clusters in
data as well as to identify atypical units.  The underlying methods are based
on distances rather than on unit x variables.")
    (license license:gpl2+)))

(define-public r-depth
  (package
    (name "r-depth")
    (version "2.1-1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "depth" version))
       (sha256
        (base32
         "0szy0027nh4ps1z919i740i50app5q7cfyg1fj7pdyl45nbl8k6m"))))
    (properties `((upstream-name . "depth")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-abind" ,r-abind)
       ("r-circular" ,r-circular)
       ("r-rgl" ,r-rgl)))
    (native-inputs
     `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/depth/")
    (synopsis "Nonparametric depth functions for multivariate analysis")
    (description
     "This package provides tools for depth functions methodology applied to
multivariate analysis.  Besides allowing calculation of depth values and
depth-based location estimators, the package includes functions or drawing
contour plots and perspective plots of depth functions.  Euclidian and
spherical depths are supported.")
    (license license:gpl2)))

(define-public r-archetypes
  (package
    (name "r-archetypes")
    (version "2.2-0.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "archetypes" version))
       (sha256
        (base32
         "0ibxsr173ib77gjhid91m85s8gjii4mi2w3d52q5301igv20p7r0"))))
    (properties `((upstream-name . "archetypes")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-modeltools" ,r-modeltools)
       ("r-nnls" ,r-nnls)))
    (home-page "https://cran.r-project.org/web/packages/archetypes")
    (synopsis "Archetypal analysis")
    (description
     "The main function @code{archetypes} implements a framework for
archetypal analysis supporting arbitrary problem solving mechanisms for the
different conceptual parts of the algorithm.")
    (license license:gpl2+)))

(define-public r-shapes
  (package
    (name "r-shapes")
    (version "1.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "shapes" version))
       (sha256
        (base32
         "0gfpdydfysp5mwg7qmkn73s67gvh2szb40mzqrx97h41ijgcgd8s"))))
    (properties `((upstream-name . "shapes")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-mass" ,r-mass)
       ("r-minpack-lm" ,r-minpack-lm)
       ("r-rgl" ,r-rgl)
       ("r-scatterplot3d" ,r-scatterplot3d)))
    (home-page "http://www.maths.nottingham.ac.uk/~ild/shapes")
    (synopsis "Statistical shape analysis")
    (description
     "This package provides routines for the statistical analysis of landmark
shapes, including Procrustes analysis, graphical displays, principal
components analysis, permutation and bootstrap tests, thin-plate spline
transformation grids and comparing covariance matrices.  See Dryden, I.L.  and
Mardia, K.V. (2016).  Statistical shape analysis, with Applications in R (2nd
Edition), John Wiley and Sons.")
    (license license:gpl2)))

(define-public r-anthropometry
  (package
    (name "r-anthropometry")
    (version "1.13")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Anthropometry" version))
       (sha256
        (base32
         "1f568ri1s6psaby8y737vrkarbjg64v89d4jyw23hy17apdmszr8"))))
    (properties `((upstream-name . "Anthropometry")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-archetypes" ,r-archetypes)
       ("r-biclust" ,r-biclust)
       ("r-cluster" ,r-cluster)
       ("r-depth" ,r-depth)
       ("r-fnn" ,r-fnn)
       ("r-icge" ,r-icge)
       ("r-nnls" ,r-nnls)
       ("r-rgl" ,r-rgl)
       ("r-shapes" ,r-shapes)))
    (home-page "https://cran.r-project.org/web/packages/Anthropometry/")
    (synopsis "Statistical methods for anthropometric data")
    (description
     "This package provides statistical methods especially developed to
analyze anthropometric data.  These methods are aimed at providing effective
solutions to some commons problems related to Ergonomics and Anthropometry.
They are based on clustering, the statistical concept of data depth,
statistical shape analysis and archetypal analysis.")
    (license license:gpl2+)))

(define-public r-adamethods
  (package
    (name "r-adamethods")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "adamethods" version))
       (sha256
        (base32
         "0mp73zh5x6h18gv29v981kb9n632kb58lvlcxwr6vcvrx393nrxh"))))
    (properties `((upstream-name . "adamethods")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-anthropometry" ,r-anthropometry)
       ("r-archetypes" ,r-archetypes)
       ("r-fnn" ,r-fnn)
       ("r-foreach" ,r-foreach)
       ("r-nnls" ,r-nnls)
       ("r-tolerance" ,r-tolerance)
       ("r-univoutl" ,r-univoutl)))
    (home-page "https://cran.r-project.org/web/packages/adamethods/")
    (synopsis "Archetypoid algorithms and anomaly detection")
    (description
     "This package is a collection of several algorithms to obtain
archetypoids with small and large databases and with both classical
multivariate data and functional data (univariate and multivariate).  Some of
these algorithms also allow to detect anomalies (outliers).")
    (license license:gpl2+)))

(define-public r-idpmisc
  (package
    (name "r-idpmisc")
    (version "1.1.20")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "IDPmisc" version))
       (sha256
        (base32
         "0zy6mxqa8arq0vvhsdcifzm3085c23rnwa1n36fhircph1xwvfdw"))))
    (properties `((upstream-name . "IDPmisc")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-lattice" ,r-lattice)))
    (home-page "https://cran.r-project.org/web/packages/IDPmisc/")
    (synopsis "Functions for data analyses and visualization")
    (description
     "This package provides different high-level graphics functions for
displaying large datasets, displaying circular data in a very flexible way,
finding local maxima, brewing color ramps, drawing nice arrows, zooming
2D-plots, creating figures with differently colored margin and plot region.
In addition, the package contains auxiliary functions for data manipulation
like omitting observations with irregular values or selecting data by logical
vectors, which include NAs.  Other functions are especially useful in
spectroscopy and analyses of environmental data: robust baseline fitting,
finding peaks in spectra, converting humidity measures.")
    (license license:gpl3+)))

(define-public r-qqman
  (package
    (name "r-qqman")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "qqman" version))
       (sha256
        (base32
         "1v9s9ag1hfb47py87wb2nad4mbsfx35832hdmrh5kxrb2f11zl1s"))))
    (properties `((upstream-name . "qqman")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-calibrate" ,r-calibrate)))
    (home-page "https://cran.r-project.org/web/packages/qqman/")
    (synopsis "Q-Q and Manhattan plots for GWAS data")
    (description
     "This package allows you to create Q-Q and Manhattan plots for GWAS data
from PLINK results.")
    (license license:gpl3)))

(define-public r-ggplot-multistats
  (package
    (name "r-ggplot-multistats")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ggplot.multistats" version))
       (sha256
        (base32
         "1afzfa86vc484bcwpg7m1ky03jpax584rigzgai8w06pifkmz0il"))))
    (properties
     `((upstream-name . "ggplot.multistats")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ggplot2" ,r-ggplot2)
       ("r-hexbin" ,r-hexbin)
       ("r-rlang" ,r-rlang)
       ("r-scales" ,r-scales)))
    (home-page "https://github.com/flying-sheep/ggplot.multistats")
    (synopsis "Multiple summary statistics for binned stats/geometries")
    (description
     "This package provides the ggplot binning layer @code{stat_summaries_hex()},
which functions similar to its singular form, but allows the use of multiple
statistics per bin.  Those statistics can be mapped to multiple bin
aesthetics.")
    (license license:gpl3)))

(define-public r-knn-covertree
  (package
    (name "r-knn-covertree")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "knn.covertree" version))
       (sha256
        (base32
         "0msfcmwydws7q7m5jdb0dxab0nkbl7mq5llg6v3r4qrnlvrdggvz"))))
    (properties `((upstream-name . "knn.covertree")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-matrix" ,r-matrix)
       ("r-rcpp" ,r-rcpp)
       ("r-rcppeigen" ,r-rcppeigen)))
    (home-page "https://github.com/flying-sheep/knn.covertree")
    (synopsis "Accurate kNN Implementation with multiple distance measures")
    (description
     "Similarly to the FNN package, this package allows calculation of the k
nearest neighbors (kNN) of a data matrix.  The implementation is based on
cover trees introduced by Alina Beygelzimer, Sham Kakade, and John
Langford (2006) @url{doi:10.1145/1143844.1143857}.")
    (license license:agpl3+)))

(define-public r-poibin
  (package
    (name "r-poibin")
    (version "1.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "poibin" version))
       (sha256
        (base32
         "1sxryvwwz6ldsnkzdy56p8c895s5yvpcai9ndyjv1x5q3l05wf9v"))))
    (properties `((upstream-name . "poibin")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/poibin/")
    (synopsis "Poisson binomial distribution")
    (description
     "This package provides an implementation of both the exact and
approximation methods for computing the @dfn{cumulative distribution
function} (CDF) of the Poisson binomial distribution.  It also provides the
@dfn{probability mass function} (PMF), quantile function, and random number
generation for the Poisson binomial distribution.")
    (license license:gpl2)))

(define-public r-diagram
  (package
    (name "r-diagram")
    (version "1.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "diagram" version))
       (sha256
        (base32
         "0f6ffprn5k0ir1s7m9s7izc64aa17r3gnygagz5bihrlsvawaavw"))))
    (properties `((upstream-name . "diagram")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-shape" ,r-shape)))
    (home-page "https://cran.r-project.org/web/packages/diagram/")
    (synopsis "Visualize simple graphs (networks) and plot flow diagrams")
    (description
     "This package provides tools to visualize simple graphs (networks) based
on a transition matrix, utilities to plot flow diagrams, visualizing webs,
electrical networks, etc.  It also includes supporting material for the book
\"A practical guide to ecological modelling - using R as a simulation
platform\" by Karline Soetaert and Peter M.J. Herman (2009) and the book
\"Solving Differential Equations in R\" by Karline Soetaert, Jeff Cash and
Francesca Mazzia (2012).")
    (license license:gpl2+)))

(define-public r-lim
  (package
    (name "r-lim")
    (version "1.4.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "LIM" version))
       (sha256
        (base32
         "03x1gnm06bw1wrzc01110bjzd2mvjdzbc2mbrazh22jrmb32w5d8"))))
    (properties `((upstream-name . "LIM")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-diagram" ,r-diagram)
       ("r-limsolve" ,r-limsolve)))
    (home-page "https://cran.r-project.org/web/packages/LIM/")
    (synopsis "Linear inverse model examples and solution methods")
    (description
     "This package provides functions that read and solve linear inverse
problems (food web problems, linear programming problems).")
    (license license:gpl2+)))

(define-public r-shinycssloaders
  (package
    (name "r-shinycssloaders")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "shinycssloaders" version))
       (sha256
        (base32
         "1gzq1lhcnhqd145ys3ixf0l13l560fiqr2sc3m2nrijwxlgcw54d"))))
    (properties
     `((upstream-name . "shinycssloaders")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-digest" ,r-digest)
       ("r-glue" ,r-glue)
       ("r-shiny" ,r-shiny)))
    (home-page "https://github.com/andrewsali/shinycssloaders")
    (synopsis "Add CSS loading animations to Shiny outputs")
    (description
     "This package provides tools to create a lightweight Shiny wrapper for
the css-loaders created by Luke Hass
@url{https://github.com/lukehaas/css-loaders}.  Wrapping a Shiny output will
automatically show a loader when the output is (re)calculating.")
    (license license:gpl3)))

(define-public r-rsvg
  (package
    (name "r-rsvg")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rsvg" version))
       (sha256
        (base32
         "11mccgf6hfskg45wqc114sx3qy2r494y6axdf73z6xwhs1wpm97g"))))
    (properties `((upstream-name . "rsvg")))
    (build-system r-build-system)
    (inputs
     `(("librsvg" ,librsvg)
       ("zlib" ,zlib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://github.com/jeroen/rsvg#readme")
    (synopsis "Render SVG images into PDF, PNG, PostScript, or Bitmap arrays")
    (description
     "This package allows you to render vector-based SVG images into
high-quality custom-size bitmap arrays using the librsvg2 library.  The
resulting bitmap can be written to e.g. PNG, JPEG or WEBP format.  In
addition, the package can convert images directly to various formats such as
PDF or PostScript.")
    (license license:expat)))

(define-public r-influencer
  (package
    (name "r-influencer")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "influenceR" version))
       (sha256
        (base32
         "12p9362hkndlnz1rd8j2rykg57kbm6l7ks60by3rd25xg50k5jag"))))
    (properties `((upstream-name . "influenceR")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-igraph" ,r-igraph)
       ("r-matrix" ,r-matrix)))
    (home-page "https://github.com/rcc-uchicago/influenceR")
    (synopsis "Tools to quantify structural importance of nodes in a network")
    (description
     "This package provides functionality to compute various node centrality
measures on networks.  Included are functions to compute betweenness
centrality (by utilizing Madduri and Bader's SNAP library), implementations of
Burt's constraint and @dfn{effective network size} (ENS) metrics, Borgatti's
algorithm to identify key players, and Valente's bridging metric.  The
betweenness, Key Players, and bridging implementations are parallelized with
OpenMP.")
    (license license:gpl2)))

(define-public r-emplik
  (package
    (name "r-emplik")
    (version "1.0-4.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "emplik" version))
       (sha256
        (base32
         "1g4hz85bvw29c77zs0ig487z92jjl682vv457x81l077h0psvk7c"))))
    (properties `((upstream-name . "emplik")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-quantreg" ,r-quantreg)))
    (home-page "https://www.ms.uky.edu/~mai/EmpLik.html")
    (synopsis "Empirical likelihood ratio for censored/truncated data")
    (description
     "This package provides empirical likelihood ratio tests for
means/quantiles/hazards from possibly censored and/or truncated data.  It also
does regression.")
    (license license:gpl2+)))

(define-public r-imputeyn
  (package
    (name "r-imputeyn")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "imputeYn" version))
       (sha256
        (base32
         "1b21w1aa5f7yiq8k0wa86wvbg4ij7f6ldwn6asfqwb0b90rvsgvs"))))
    (properties `((upstream-name . "imputeYn")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-boot" ,r-boot)
       ("r-emplik" ,r-emplik)
       ("r-mvtnorm" ,r-mvtnorm)
       ("r-quadprog" ,r-quadprog)
       ("r-survival" ,r-survival)))
    (home-page "https://cran.r-project.org/web/packages/imputeYn/")
    (synopsis "Impute last largest censored observation under weighted least squares")
    (description
     "This package allows for the imputation of the last largest censored
observantions.  This method brings less bias and more efficient estimates for
AFT models.")
    (license license:gpl2)))

(define-public r-adapenetclass
  (package
    (name "r-adapenetclass")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "AdapEnetClass" version))
       (sha256
        (base32
         "01k3mj4g1ckbng7wkzzn9h0k9yf01cpnnkly0sjda574c5jhj0rc"))))
    (properties `((upstream-name . "AdapEnetClass")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-glmnet" ,r-glmnet)
       ("r-imputeyn" ,r-imputeyn)
       ("r-lars" ,r-lars)
       ("r-quadprog" ,r-quadprog)))
    (home-page "https://cran.r-project.org/web/packages/AdapEnetClass/")
    (synopsis "Class of adaptive elastic net methods for censored data")
    (description
     "This package provides methods for variable selection for AFT models.")
    (license license:gpl2)))

(define-public r-flock
  (package
    (name "r-flock")
    (version "0.7")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "flock" version))
       (sha256
        (base32
         "1zg93p74icj4bhxnmnssj2xp6vw4yaksyavq03497v33xfpdxss7"))))
    (properties `((upstream-name . "flock")))
    (build-system r-build-system)
    (propagated-inputs `(("r-rcpp" ,r-rcpp)))
    (home-page "https://cran.r-project.org/web/packages/flock/")
    (synopsis "Process synchronization using file locks")
    (description
     "This package implements synchronization between R processes (spawned by
using the @code{parallel} package for instance) using file locks.  It supports
both exclusive and shared locking.")
    (license license:asl2.0)))

(define-public r-archivist
  (package
    (name "r-archivist")
    (version "2.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "archivist" version))
       (sha256
        (base32
         "1i11hrcq1910jgd6diw6h3sxx624v57zjianm49pqvb2dvd0b8y7"))))
    (properties `((upstream-name . "archivist")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-dbi" ,r-dbi)
       ("r-digest" ,r-digest)
       ("r-flock" ,r-flock)
       ("r-httr" ,r-httr)
       ("r-lubridate" ,r-lubridate)
       ("r-magrittr" ,r-magrittr)
       ("r-rcurl" ,r-rcurl)
       ("r-rsqlite" ,r-rsqlite)))
    (home-page "https://pbiecek.github.io/archivist/")
    (synopsis "Tools for storing, restoring and searching for R objects")
    (description
     "Data exploration and modelling is a process in which a lot of data
artifacts are produced.  Artifacts like: subsets, data aggregates, plots,
statistical models, different versions of data sets and different versions of
results.  Archivist helps to store and manage artifacts created in R.  It
allows you to store selected artifacts as binary files together with their
metadata and relations.  Archivist allows sharing artifacts with others.  It
can look for already created artifacts by using its class, name, date of the
creation or other properties.  It also makes it easy to restore such
artifacts.")
    (license license:gpl2)))

(define-public r-versions
  (package
    (name "r-versions")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "versions" version))
       (sha256
        (base32
         "0zm49j785dgv7fyr1yl9q5f0dsk8hhpim5q5bpkgrkzv7pwjribd"))))
    (properties `((upstream-name . "versions")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/versions/")
    (synopsis "Query and install specific versions of CRAN packages")
    (description
     "This package allows you to install specified versions of R packages
hosted on CRAN and provides functions to list available versions and the
versions of currently installed packages.")
    (license license:bsd-3)))

(define-public r-adapr
  (package
    (name "r-adapr")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "adapr" version))
       (sha256
        (base32
         "1s57jdbi5pik56xjz1d4438ax6cywg2yq2s47h5g6wrwvpgr1qfw"))))
    (properties `((upstream-name . "adapr")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-archivist" ,r-archivist)
       ("r-devtools" ,r-devtools)
       ("r-digest" ,r-digest)
       ("r-doparallel" ,r-doparallel)
       ("r-gdata" ,r-gdata)
       ("r-ggplot2" ,r-ggplot2)
       ("r-git2r" ,r-git2r)
       ("r-igraph" ,r-igraph)
       ("r-knitr" ,r-knitr)
       ("r-plotly" ,r-plotly)
       ("r-plyr" ,r-plyr)
       ("r-rmarkdown" ,r-rmarkdown)
       ("r-shiny" ,r-shiny)
       ("r-shinydashboard" ,r-shinydashboard)
       ("r-versions" ,r-versions)))
    (home-page "https://cran.r-project.org/web/packages/adapr/")
    (synopsis "Implementation of an accountable data analysis process")
    (description
     "This package tracks reading and writing within R scripts that are
organized into a directed acyclic graph.  It contains an interactive Shiny
application @code{adaprApp()}.  It uses Git and file hashes to track version
histories of inputs and outputs.")
    (license license:lgpl2.0)))

(define-public r-adapsamp
  (package
    (name "r-adapsamp")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "AdapSamp" version))
       (sha256
        (base32
         "1jayjrsiib2ij4rxxj59g71r3xhzl5yqh0lhi8k6cfy03i7dkvis"))))
    (properties `((upstream-name . "AdapSamp")))
    (build-system r-build-system)
    (propagated-inputs `(("r-pracma" ,r-pracma)))
    (home-page "https://cran.r-project.org/web/packages/AdapSamp/")
    (synopsis "Adaptive sampling algorithms")
    (description
     "For distributions whose probability density functions are log-concave,
the adaptive rejection sampling algorithm can be used to build envelope
functions for sampling.  For others, the modified adaptive rejection sampling
algorithm, the concave-convex adaptive rejection sampling algorithm, and the
adaptive slice sampling algorithm can be used.  This R package mainly includes
these four functions: @code{rARS()}, @code{rMARS()}, @code{rCCARS()}, and
@code{rASS()}.  These functions can realize sampling based on the algorithms
above.")
    (license license:gpl2)))

(define-public r-adaptalint
  (package
    (name "r-adaptalint")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "adaptalint" version))
       (sha256
        (base32
         "15qdcvnnbgcps8j5k79354wsc9alswijv8lcafg2i3lghaw536yf"))))
    (properties `((upstream-name . "adaptalint")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-dplyr" ,r-dplyr)
       ("r-lintr" ,r-lintr)
       ("r-purrr" ,r-purrr)))
    (home-page "https://cran.r-project.org/web/packages/adaptalint/")
    (synopsis "Check R code style")
    (description
     "This package provides tools to infer the code style (which style rules
are followed and which ones are not) from one package and use it to check
another.  This makes it easier to find and correct the most important problems
first.")
    (license license:gpl3)))

(define-public r-fracdiff
  (package
    (name "r-fracdiff")
    (version "1.5-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "fracdiff" version))
       (sha256
        (base32
         "1dhfjlhr9sb38qgpsx0sm73l9lgc13d0fk32l7fmjfnalhr3n45q"))))
    (properties `((upstream-name . "fracdiff")))
    (build-system r-build-system)
    (home-page "https://github.com/mmaechler/fracdiff")
    (synopsis
     "Fractionally differenced ARIMA aka ARFIMA(P,d,q) models")
    (description
     "This package provides tools for the maximum likelihood estimation of the
parameters of a fractionally differenced ARIMA(p,d,q) model (Haslett and
Raftery, Appl.Statistics, 1989); it includes inference and basic methods.")
    (license license:gpl2+)))

(define-public r-forecast
  (package
    (name "r-forecast")
    (version "8.11")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "forecast" version))
       (sha256
        (base32
         "0ayidhnk9cxav2qi83jrvqlg2jh4zlf4lki4xw48gdqsmjvih9x1"))))
    (properties `((upstream-name . "forecast")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-colorspace" ,r-colorspace)
       ("r-fracdiff" ,r-fracdiff)
       ("r-ggplot2" ,r-ggplot2)
       ("r-lmtest" ,r-lmtest)
       ("r-magrittr" ,r-magrittr)
       ("r-nnet" ,r-nnet)
       ("r-rcpp" ,r-rcpp)
       ("r-rcpparmadillo" ,r-rcpparmadillo)
       ("r-timedate" ,r-timedate)
       ("r-tseries" ,r-tseries)
       ("r-urca" ,r-urca)
       ("r-zoo" ,r-zoo)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))           ; needed for vignettes
    (home-page "https://pkg.robjhyndman.com/forecast/")
    (synopsis "Forecasting functions for time series and linear models")
    (description
     "This package provides methods and tools for displaying and analysing
univariate time series forecasts including exponential smoothing via state
space models and automatic ARIMA modelling.")
    (license license:gpl3)))

(define-public r-xmisc
  (package
    (name "r-xmisc")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Xmisc" version))
       (sha256
        (base32
         "11gwlcyxhz1p50m68cnqrxmisdk99v8vrsbvyr7k67f0kvsznzs1"))))
    (properties `((upstream-name . "Xmisc")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/package=Xmisc")
    (synopsis
     "Xiaobei's miscellaneous classes and functions")
    (description
     "This package provides Xiaobei's miscellaneous classes and functions,
which are useful when developing R packages for @dfn{object oriented
programming} (OOP) using R Reference Class.")
    (license license:gpl2+)))

(define-public r-proxyc
  (package
    (name "r-proxyc")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "proxyC" version))
       (sha256
        (base32
         "159bc42x4shm6n3rh9fc8ziv3ivq0ipmpbasrh279hhn1prc8gg6"))))
    (properties `((upstream-name . "proxyC")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-matrix" ,r-matrix)
       ("r-rcpp" ,r-rcpp)
       ("r-rcpparmadillo" ,r-rcpparmadillo)
       ("r-rcppparallel" ,r-rcppparallel)))
    (home-page "https://cran.r-project.org/package=proxyC")
    (synopsis "Compute proximity in large sparse matrices")
    (description
     "This package provides efficient tools to compute the proximity between
rows or columns of large matrices.  Functions are optimised for large sparse
matrices using the Armadillo and Intel TBB libraries.  Among several built-in
similarity/distance measures, computation of correlation, cosine similarity
and Euclidean distance is particularly fast.")
    (license license:gpl3)))

(define-public r-isocodes
  (package
    (name "r-isocodes")
    (version "2019.12.22")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ISOcodes" version))
       (sha256
        (base32
         "1k2f2258bwzs0b3nxma9kcw395qkljvk514a7047rx4dn0iwd874"))))
    (properties `((upstream-name . "ISOcodes")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/package=ISOcodes")
    (synopsis "Selected ISO codes")
    (description
     "This package provides ISO language, territory, currency, script and
character codes.  It provides ISO 639 language codes, ISO 3166 territory
codes, ISO 4217 currency codes, ISO 15924 script codes, and the ISO 8859
character codes as well as the UN M.49 area codes.")
    (license license:gpl2)))

(define-public r-stopwords
  (package
    (name "r-stopwords")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "stopwords" version))
       (sha256
        (base32
         "1nmi0bpd0c238g5b8ch1v034m5ng9llhs519cgxdrj3sh9fplwlv"))))
    (properties `((upstream-name . "stopwords")))
    (build-system r-build-system)
    (propagated-inputs `(("r-isocodes" ,r-isocodes)))
    (home-page "https://github.com/quanteda/stopwords")
    (synopsis "Multilingual stopword lists")
    (description
     "This package provides multiple sources of stopwords, for use in text
analysis and natural language processing.")
    (license license:expat)))

(define-public r-spacyr
  (package
    (name "r-spacyr")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "spacyr" version))
       (sha256
        (base32
         "1b2ccgwsiqkvp7w37x8k7699c676q16vfrybkrfvyczyhki4s6nw"))))
    (properties `((upstream-name . "spacyr")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-data-table" ,r-data-table)
       ("r-reticulate" ,r-reticulate)))
    (home-page "https://spacyr.quanteda.io")
    (synopsis "R wrapper for the spaCy NLP library")
    (description
     "This package provides an R wrapper to the Python @dfn{natural language
processing} (NLP) library @code{spaCy}, from @url{http://spacy.io}.")
    (license license:gpl3)))

(define-public r-snowballc
  (package
    (name "r-snowballc")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "SnowballC" version))
       (sha256
        (base32
         "0b7pqdavf5jbf8si4ybnii5fff39p3b1rb5rym05j8s48hs7sqb1"))))
    (properties `((upstream-name . "SnowballC")))
    (build-system r-build-system)
    (home-page "https://r-forge.r-project.org/projects/r-temis/")
    (synopsis "Snowball stemmers based on the C libstemmer UTF-8 library")
    (description
     "This package provides an R interface to the C @code{libstemmer} library
that implements Porter's word stemming algorithm for collapsing words to a
common root to aid comparison of vocabulary.  Currently supported languages
are Danish, Dutch, English, Finnish, French, German, Hungarian, Italian,
Norwegian, Portuguese, Romanian, Russian, Spanish, Swedish and Turkish.")
    (license license:bsd-3)))

(define-public r-quanteda
  (package
    (name "r-quanteda")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "quanteda" version))
       (sha256
        (base32
         "0snr610vahbdqkmma0zy5mg5qbb6n1b8sgsclqb2sccn1gg38vyy"))))
    (properties `((upstream-name . "quanteda")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-data-table" ,r-data-table)
       ("r-extrafont" ,r-extrafont)
       ("r-fastmatch" ,r-fastmatch)
       ("r-ggplot2" ,r-ggplot2)
       ("r-ggrepel" ,r-ggrepel)
       ("r-jsonlite" ,r-jsonlite)
       ("r-magrittr" ,r-magrittr)
       ("r-matrix" ,r-matrix)
       ("r-network" ,r-network)
       ("r-proxyc" ,r-proxyc)
       ("r-rcpp" ,r-rcpp)
       ("r-rcpparmadillo" ,r-rcpparmadillo)
       ("r-rcppparallel" ,r-rcppparallel)
       ("r-sna" ,r-sna)
       ("r-snowballc" ,r-snowballc)
       ("r-stopwords" ,r-stopwords)
       ("r-stringi" ,r-stringi)
       ("r-xml2" ,r-xml2)
       ("r-yaml" ,r-yaml)))
    (home-page "https://quanteda.io")
    (synopsis "Quantitative analysis of textual data")
    (description
     "This package provides a fast, flexible, and comprehensive framework for
quantitative text analysis in R.  It provides functionality for corpus
management, creating and manipulating tokens and ngrams, exploring keywords in
context, forming and manipulating sparse matrices of documents by features and
feature co-occurrences, analyzing keywords, computing feature similarities and
distances, applying content dictionaries, applying supervised and unsupervised
machine learning, visually representing text and text analyses, and more.")
    (license license:gpl3)))

(define-public r-topicmodels
  (package
    (name "r-topicmodels")
    (version "0.2-9")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "topicmodels" version))
       (sha256
        (base32
         "1757r5x8bsl4dk106xg6481mvdkdz9vwg87n7rpbvdkavsvhyxs0"))))
    (properties `((upstream-name . "topicmodels")))
    (build-system r-build-system)
    (native-inputs
     `(("gsl" ,gsl)))
    (propagated-inputs
     `(("r-modeltools" ,r-modeltools)
       ("r-slam" ,r-slam)
       ("r-tm" ,r-tm)))
    (home-page "https://cran.r-project.org/package=topicmodels")
    (synopsis "Topic models")
    (description
     "This package provides an interface to the C code for @dfn{Latent
Dirichlet Allocation} (LDA) models and @dfn{Correlated Topics Models} (CTM) by
David M. Blei and co-authors and the C++ code for fitting LDA models using
Gibbs sampling by Xuan-Hieu Phan and co-authors.")
    (license license:gpl2)))

(define-public r-stm
  (package
    (name "r-stm")
    (version "1.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "stm" version))
       (sha256
        (base32
         "1yyfxaxqc6yq0yq68zhdnhpwpvsyp71dlmivn7zxixfmp932s6cn"))))
    (properties `((upstream-name . "stm")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-data-table" ,r-data-table)
       ("r-glmnet" ,r-glmnet)
       ("r-lda" ,r-lda)
       ("r-matrix" ,r-matrix)
       ("r-matrixstats" ,r-matrixstats)
       ("r-quadprog" ,r-quadprog)
       ("r-quanteda" ,r-quanteda)
       ("r-rcpp" ,r-rcpp)
       ("r-rcpparmadillo" ,r-rcpparmadillo)
       ("r-slam" ,r-slam)
       ("r-stringr" ,r-stringr)))
    (home-page "http://www.structuraltopicmodel.com/")
    (synopsis "Estimation of the Structural Topic Model")
    (description
     "The @dfn{Structural Topic Model} (STM) allows researchers to estimate
topic models with document-level covariates.  The package also includes tools
for model selection, visualization, and estimation of topic-covariate
regressions.")
    (license license:expat)))

(define-public r-polycor
  (package
    (name "r-polycor")
    (version "0.7-10")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "polycor" version))
       (sha256
        (base32
         "0wzwzklflbhi8sv9m7ijwr16v9zmkk0j0v4pbcpf32f8lbn3psna"))))
    (properties `((upstream-name . "polycor")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-matrix" ,r-matrix)
       ("r-mvtnorm" ,r-mvtnorm)))
    (home-page "https://r-forge.r-project.org/projects/polycor/")
    (synopsis "Polychoric and polyserial correlations")
    (description
     "This package provides tools to compute polychoric and polyserial
correlations by quick \"two-step\" methods or ML, optionally with standard
errors; tetrachoric and biserial correlations are special cases.")
    (license license:gpl2+)))

(define-public r-msm
  (package
    (name "r-msm")
    (version "1.6.8")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "msm" version))
       (sha256
        (base32
         "1d32y8f0vb2dfv3999liigpns788j145nrvd1xpxb9i2lsg8mwgk"))))
    (properties `((upstream-name . "msm")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-expm" ,r-expm)
       ("r-mvtnorm" ,r-mvtnorm)
       ("r-survival" ,r-survival)))
    (home-page "https://github.com/chjackson/msm")
    (synopsis "Multi-state Markov and hidden Markov models in continuous time")
    (description
     "This package provides functions for fitting continuous-time Markov and
hidden Markov multi-state models to longitudinal data.  It was designed for
processes observed at arbitrary times in continuous time (panel data) but some
other observation schemes are supported.  Both Markov transition rates and the
hidden Markov output process can be modelled in terms of covariates, which may
be constant or piecewise-constant in time.")
    (license license:gpl2+)))

(define-public r-ltm
  (package
    (name "r-ltm")
    (version "1.1-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ltm" version))
       (sha256
        (base32
         "1qrgzwx5l58qf5rfp1knxc84r0g943q5sdr3ky74zzwpnmrf2vf7"))))
    (properties `((upstream-name . "ltm")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-mass" ,r-mass)
       ("r-msm" ,r-msm)
       ("r-polycor" ,r-polycor)))
    (home-page "https://github.com/drizopoulos/ltm")
    (synopsis "Latent trait models under IRT")
    (description
     "This is a package supporting the analysis of multivariate dichotomous
and polytomous data using latent trait models under the Item Response Theory
approach.  It includes the Rasch, the Two-Parameter Logistic, the Birnbaum's
Three-Parameter, the Graded Response, and the Generalized Partial Credit
Models.")
    (license license:gpl2+)))

(define-public r-mi
  (package
    (name "r-mi")
    (version "1.0")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "mi" version))
        (sha256
          (base32
            "1h47k5mpbvhid83277dvvj2di493bgzz9iarpyv3r30y219l7x1l"))))
    (properties `((upstream-name . "mi")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-arm" ,r-arm)
       ("r-matrix" ,r-matrix)))
    (home-page "http://www.stat.columbia.edu/~gelman/")
    (synopsis "Missing data imputation and model checking")
    (description
     "This package provides functions for data manipulation, imputing missing
values in an approximate Bayesian framework, diagnostics of the models used to
generate the imputations, confidence-building mechanisms to validate some of
the assumptions of the imputation algorithm, and functions to analyze multiply
imputed data sets with the appropriate degree of sampling uncertainty.")
    (license license:gpl2+)))

(define-public r-matrixcalc
  (package
    (name "r-matrixcalc")
    (version "1.0-3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "matrixcalc" version))
       (sha256
        (base32
         "1c4w9dhi5w98qj1wwh9bbpnfk39rhiwjbanalr8bi5nmxkpcmrhp"))))
    (properties `((upstream-name . "matrixcalc")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/web/packages/matrixcalc/")
    (synopsis "Collection of functions for matrix calculations")
    (description
     "This package provides a collection of functions to support matrix
calculations for probability, econometric and numerical analysis.  There are
additional functions that are comparable to APL functions which are useful for
actuarial models such as pension mathematics.")
    (license license:gpl2+)))

(define-public r-sem
  (package
    (name "r-sem")
    (version "3.1-9")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "sem" version))
       (sha256
        (base32
         "1f9c6g6pfx66gd2pappcsqh484ah6a0x4z47hpd46rah0817hcsa"))))
    (properties `((upstream-name . "sem")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-boot" ,r-boot)
       ("r-mass" ,r-mass)
       ("r-matrixcalc" ,r-matrixcalc)
       ("r-mi" ,r-mi)))
    (home-page "https://cran.r-project.org/package=sem")
    (synopsis "Structural equation models")
    (description
     "This package provides functions for fitting general linear structural
equation models (with observed and latent variables) using the RAM approach,
and for fitting structural equations in observed-variable models by two-stage
least squares.")
    (license license:gpl2+)))

(define-public r-semtools
  (package
    (name "r-semtools")
    (version "0.5-2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "semTools" version))
       (sha256
        (base32
         "1zj841pszfsikzp82cmh463qyc4xhdrqjqcnhc2r8mcflv12irv6"))))
    (properties `((upstream-name . "semTools")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-lavaan" ,r-lavaan)))
    (home-page "https://github.com/simsem/semTools/wiki")
    (synopsis "Useful tools for structural equation modeling")
    (description
     "This package provides useful tools for structural equation modeling.")
    (license license:gpl2+)))

(define-public r-regsem
  (package
    (name "r-regsem")
    (version "1.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "regsem" version))
       (sha256
        (base32
         "0ch057010xfsw0nqcsarzakdbiplvxaldyqlbbacspqs65ax1yk7"))))
    (properties `((upstream-name . "regsem")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-lavaan" ,r-lavaan)
       ("r-rcpp" ,r-rcpp)
       ("r-rcpparmadillo" ,r-rcpparmadillo)
       ("r-rsolnp" ,r-rsolnp)))
    (home-page "https://cran.r-project.org/package=regsem")
    (synopsis "Regularized structural equation modeling")
    (description
     "This package uses both ridge and lasso penalties (and extensions) to
penalize specific parameters in structural equation models.  The package
offers additional cost functions, cross validation, and other extensions
beyond traditional structural equation models.  It also contains a function to
perform @dfn{exploratory mediation} (XMed).")
    (license license:gpl2+)))

(define-public r-stanheaders
  (package
    (name "r-stanheaders")
    (version "2.19.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "StanHeaders" version))
       (sha256
        (base32
         "0cmk0fzczx7dcywcw1dhm6gfq84qlsx77qrsk4z3bf3dhr4bznam"))))
    (properties `((upstream-name . "StanHeaders")))
    (build-system r-build-system)
    (inputs `(("pandoc" ,ghc-pandoc)))
    (native-inputs
     `(("gfortran" ,gfortran)
       ("r-knitr" ,r-knitr))) ; for vignettes
    (home-page "https://mc-stan.org/")
    (synopsis "C++ header files for Stan")
    (description
     "The C++ header files of the Stan project are provided by this package.
There is a shared object containing part of the @code{CVODES} library, but it
is not accessible from R.  @code{r-stanheaders} is only useful for developers
who want to utilize the @code{LinkingTo} directive of their package's
DESCRIPTION file to build on the Stan library without incurring unnecessary
dependencies.

The Stan project develops a probabilistic programming language that implements
full or approximate Bayesian statistical inference via Markov Chain Monte
Carlo or variational methods and implements (optionally penalized) maximum
likelihood estimation via optimization.  The Stan library includes an advanced
automatic differentiation scheme, templated statistical and linear algebra
functions that can handle the automatically differentiable scalar types (and
doubles, ints, etc.), and a parser for the Stan language.  The @code{r-rstan}
package provides user-facing R functions to parse, compile, test, estimate,
and analyze Stan models.")
    (license license:bsd-3)))

(define-public r-rpf
  (package
    (name "r-rpf")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rpf" version))
       (sha256
        (base32
         "1i2kqd7nx55nn35qnw89xmnqk23x9c8xhkh736c2xg7k2ai84ybl"))))
    (properties `((upstream-name . "rpf")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-lifecycle" ,r-lifecycle)
       ("r-mvtnorm" ,r-mvtnorm)
       ("r-rcpp" ,r-rcpp)
       ("r-rcppeigen" ,r-rcppeigen)))
    (home-page "https://github.com/jpritikin/rpf")
    (synopsis "Response probability functions")
    (description
     "The purpose of this package is to factor out logic and math common to
Item Factor Analysis fitting, diagnostics, and analysis.  It is envisioned as
core support code suitable for more specialized IRT packages to build upon.
Complete access to optimized C functions is made available with
@code{R_RegisterCCallable()}.")
    (license license:gpl3+)))

(define-public r-openmx
  (package
    (name "r-openmx")
    (version "2.17.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "OpenMx" version))
       (sha256
        (base32
         "1s2pcg281ag3qz2wz8yi826f2d3kj3qg916js7zz0nsrljcyv5bc"))))
    (properties `((upstream-name . "OpenMx")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bh" ,r-bh)
       ("r-digest" ,r-digest)
       ("r-mass" ,r-mass)
       ("r-matrix" ,r-matrix)
       ("r-rcpp" ,r-rcpp)
       ("r-rcppeigen" ,r-rcppeigen)
       ("r-rpf" ,r-rpf)
       ("r-stanheaders" ,r-stanheaders)))
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "http://openmx.ssri.psu.edu")
    (synopsis "Extended structural equation modelling")
    (description
     "This package allows for the estimation of a wide variety of advanced
multivariate statistical models.  It consists of a library of functions and
optimizers that allow you to quickly and flexibly define an SEM model and
estimate parameters given observed data.")
    (license license:asl2.0)))

(define-public r-kutils
  (package
    (name "r-kutils")
    (version "1.69")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "kutils" version))
       (sha256
        (base32
         "12pg26a85h0jxlfcyai68dbh4bq1gnq8v1ngi8k9qvafbrpc6gx8"))))
    (properties `((upstream-name . "kutils")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-foreign" ,r-foreign)
       ("r-lavaan" ,r-lavaan)
       ("r-openxlsx" ,r-openxlsx)
       ("r-plyr" ,r-plyr)
       ("r-runit" ,r-runit)
       ("r-xtable" ,r-xtable)))
    (home-page "https://cran.r-project.org/package=kutils")
    (synopsis "Project management tools")
    (description
     "This package provides tools for data importation, recoding, and
inspection.  There are functions to create new project folders, R code
templates, create uniquely named output directories, and to quickly obtain a
visual summary for each variable in a data frame.  The main feature here is
the systematic implementation of the \"variable key\" framework for data
importation and recoding.")
    (license license:gpl2)))

(define-public r-rockchalk
  (package
    (name "r-rockchalk")
    (version "1.8.144")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rockchalk" version))
       (sha256
        (base32
         "07dp1n155b9gfvk8l30h6bhjbhbylsjxfzns08mryn4mxj3nqpnb"))))
    (properties `((upstream-name . "rockchalk")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-cardata" ,r-cardata)
       ("r-kutils" ,r-kutils)
       ("r-lme4" ,r-lme4)
       ("r-mass" ,r-mass)))
    (home-page "https://cran.r-project.org/package=rockchalk")
    (synopsis "Regression estimation and presentation")
    (description
     "This package provides a collection of functions for interpretation and
presentation of regression analysis.  These functions are used to produce the
statistics lectures in @url{http://pj.freefaculty.org/guides}.  The package
includes regression diagnostics, regression tables, and plots of interactions
and \"moderator\" variables.  The emphasis is on \"mean-centered\" and
\"residual-centered\" predictors.  The vignette @code{rockchalk} offers a
fairly comprehensive overview.")
    (license license:gpl3+)))

(define-public r-lisreltor
  (package
    (name "r-lisreltor")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "lisrelToR" version))
       (sha256
        (base32
         "0zicq0z3hhixan1p1apybnf3v5s6v6ysll4pcz8ivygwr2swv3p5"))))
    (properties `((upstream-name . "lisrelToR")))
    (build-system r-build-system)
    (home-page "https://cran.r-project.org/package=lisrelToR")
    (synopsis "Import output from LISREL into R")
    (description
     "This is an unofficial package aimed at automating the import of LISREL
output in R.")
    (license license:gpl2)))

(define-public r-bdgraph
  (package
    (name "r-bdgraph")
    (version "2.62")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "BDgraph" version))
       (sha256
        (base32
         "1b1vfar940swvn3pcil848qsp8ji50fjjll8jjzp6y2adx0f8pby"))))
    (properties `((upstream-name . "BDgraph")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-igraph" ,r-igraph)))
    (home-page "https://www.uva.nl/profile/a.mohammadi")
    (synopsis "Bayesian structure learning in graphical models")
    (description
     "This package provides statistical tools for Bayesian structure learning
in undirected graphical models for continuous, discrete, and mixed data.  It
uses a trans-dimensional @dfn{Markov Chain Monte Carlo} (MCMC) approach based
on a continuous-time birth-death process.")
    (license license:gpl2+)))

(define-public r-d3network
  (package
    (name "r-d3network")
    (version "0.5.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "d3Network" version))
       (sha256
        (base32
         "1gh979z9wksyxxxdzlfzibn0ysvf6h1ij7vwpd55fvbwr308syaw"))))
    (properties `((upstream-name . "d3Network")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-plyr" ,r-plyr)
       ("r-rjson" ,r-rjson)
       ("r-whisker" ,r-whisker)))
    (home-page "http://christophergandrud.github.io/d3Network/")
    (synopsis "Create D3 JavaScript network, tree, dendrogram, and Sankey graphs")
    (description
     "This package is intended to make it easy to create D3 JavaScript
network, tree, dendrogram, and Sankey graphs from R using data frames.")
    (license license:gpl3+)))

(define-public r-qgraph
  (package
    (name "r-qgraph")
    (version "1.6.5")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "qgraph" version))
       (sha256
        (base32
         "0pwys9irxvp0ap158drplyypkplbmwqinv0fmlsblk7q875cr592"))))
    (properties `((upstream-name . "qgraph")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-abind" ,r-abind)
       ("r-bdgraph" ,r-bdgraph)
       ("r-colorspace" ,r-colorspace)
       ("r-corpcor" ,r-corpcor)
       ("r-d3network" ,r-d3network)
       ("r-dplyr" ,r-dplyr)
       ("r-fdrtool" ,r-fdrtool)
       ("r-ggplot2" ,r-ggplot2)
       ("r-ggraph" ,r-ggraph)
       ("r-glasso" ,r-glasso)
       ("r-gtools" ,r-gtools)
       ("r-hmisc" ,r-hmisc)
       ("r-huge" ,r-huge)
       ("r-igraph" ,r-igraph)
       ("r-jpeg" ,r-jpeg)
       ("r-lavaan" ,r-lavaan)
       ("r-matrix" ,r-matrix)
       ("r-pbapply" ,r-pbapply)
       ("r-plyr" ,r-plyr)
       ("r-png" ,r-png)
       ("r-psych" ,r-psych)
       ("r-rcpp" ,r-rcpp)
       ("r-reshape2" ,r-reshape2)
       ("r-tidygraph" ,r-tidygraph)))
    (home-page "http://sachaepskamp.com/qgraph/")
    (synopsis "Weighted network visualization and analysis")
    (description
     "This package implements tools for weighted network visualization and
analysis, as well as Gaussian graphical model computation.  It contains graph
plotting methods, and tools for psychometric data visualization and graphical
model estimation.  See Epskamp et al. (2012)
@url{doi:10.18637/jss.v048.i04}.")
    (license license:gpl2)))

(define-public r-semplot
  (package
    (name "r-semplot")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "semPlot" version))
       (sha256
        (base32
         "0l1v9yi1pv59iwfknw4dh9qskk5y8r347jq1vq13gnfd3bmd71xr"))))
    (properties `((upstream-name . "semPlot")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-colorspace" ,r-colorspace)
       ("r-corpcor" ,r-corpcor)
       ("r-igraph" ,r-igraph)
       ("r-lavaan" ,r-lavaan)
       ("r-lisreltor" ,r-lisreltor)
       ("r-openmx" ,r-openmx)
       ("r-plyr" ,r-plyr)
       ("r-qgraph" ,r-qgraph)
       ("r-regsem" ,r-regsem)
       ("r-rockchalk" ,r-rockchalk)
       ("r-sem" ,r-sem)
       ("r-xml" ,r-xml)))
    (home-page "https://github.com/SachaEpskamp/semPlot")
    (synopsis "Unified visualizations of structural equation models")
    (description
     "Structural equation modeling (SEM) has a long history of representing
models graphically as path diagrams.  The semPlot package for R fills the gap
between advanced, but time-consuming, graphical software and the limited
graphics produced automatically by SEM software.  In addition, semPlot offers
more functionality than drawing path diagrams: it can act as a common ground
for importing SEM results into R.  Any result usable as input to semPlot can
also be represented in any of the three popular SEM frame-works, as well as
translated to input syntax for the R packages @code{sem} and @code{lavaan}.")
    (license license:gpl2)))

(define-public r-cdm
  (package
    (name "r-cdm")
    (version "7.5-15")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "CDM" version))
       (sha256
        (base32
         "159ny2dz0rf1r3k1mqlfwambffc8rx425sggf5bn51nybpzanq3l"))))
    (properties `((upstream-name . "CDM")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-mvtnorm" ,r-mvtnorm)
       ("r-polycor" ,r-polycor)
       ("r-rcpp" ,r-rcpp)
       ("r-rcpparmadillo" ,r-rcpparmadillo)))
    (home-page
     "https://github.com/alexanderrobitzsch/CDM")
    (synopsis "Cognitive diagnosis modeling")
    (description
     "This package provides functions for cognitive diagnosis modeling and
multidimensional item response modeling for dichotomous and polytomous item
responses.  It enables the estimation of the DINA and DINO model, the multiple
group (polytomous) GDINA model, the multiple choice DINA model, the general
diagnostic model (GDM), the structured latent class model (SLCA), and
regularized latent class analysis.  See George, Robitzsch, Kiefer, Gross, and
Uenlue (2017) @url{doi:10.18637/jss.v074.i02} for further details on
estimation and the package structure.  For tutorials on how to use the CDM
package see George and Robitzsch (2015, @url{doi:10.20982/tqmp.11.3.p189}) as
well as Ravand and Robitzsch (2015).")
    (license license:gpl2+)))

(define-public r-tam
  (package
    (name "r-tam")
    (version "3.4-26")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "TAM" version))
       (sha256
        (base32
         "111d7qkxhwh1lfvldyh9d61pdb5vb6x8lr8n9h95ssvw07vjqvk9"))))
    (properties `((upstream-name . "TAM")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-cdm" ,r-cdm)
       ("r-rcpp" ,r-rcpp)
       ("r-rcpparmadillo" ,r-rcpparmadillo)))
    (home-page "http://www.edmeasurementsurveys.com/TAM/Tutorials/")
    (synopsis "Test analysis modules")
    (description
     "This package includes tools for marginal maximum likelihood estimation
and joint maximum likelihood estimation for unidimensional and
multidimensional item response models.  The package functionality covers the
Rasch model, 2PL model, 3PL model, generalized partial credit model,
multi-faceted Rasch model, nominal item response model, structured latent
class model, mixture distribution IRT models, and located latent class models.
Latent regression models and plausible value imputation are also supported.")
    (license license:gpl2+)))

(define-public r-erm
  (package
    (name "r-erm")
    (version "1.0-1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "eRm" version))
       (sha256
        (base32
         "0njqzznnhnkvalmhiq5yq1w7gwp2myki5cv61w42ydvd27hdyyg9"))))
    (properties `((upstream-name . "eRm")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-colorspace" ,r-colorspace)
       ("r-lattice" ,r-lattice)
       ("r-mass" ,r-mass)
       ("r-matrix" ,r-matrix)
       ("r-psych" ,r-psych)))
    (native-inputs `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/package=eRm")
    (synopsis "Extended Rasch modeling")
    (description
     "This package provides tools to fit @dfn{Rasch models} (RM), @dfn{linear
logistic test models} (LLTM), @dfn{rating scale model} (RSM), @dfn{linear
rating scale models} (LRSM), @dfn{partial credit models} (PCM), and
@dfn{linear partial credit models} (LPCM).  Missing values are allowed in the
data matrix.  Additional features are the ML estimation of the person
parameters, Andersen's LR-test, item-specific Wald test, Martin-Loef-Test,
nonparametric Monte-Carlo Tests, itemfit and personfit statistics including
infit and outfit measures, ICC and other plots, automated stepwise item
elimination, and a simulation module for various binary data matrices.")
    (license license:gpl3)))

(define-public r-irtoys
  (package
    (name "r-irtoys")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "irtoys" version))
       (sha256
        (base32
         "0h6iiaxikhbxhbyksbjnb09qdxinlkwr2v9yzs5wslbni14paq5q"))))
    (properties `((upstream-name . "irtoys")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-ltm" ,r-ltm)
       ("r-sm" ,r-sm)))
    (home-page "https://cran.r-project.org/package=irtoys")
    (synopsis "Collection of functions related to Item Response Theory (IRT)")
    (description
     "This package provides a collection of functions useful in learning and
practicing @dfn{Item Response Theory} (IRT), which can be combined into larger
programs.  It provides basic CTT analysis, a simple common interface to the
estimation of item parameters in IRT models for binary responses with three
different programs (ICL, BILOG-MG, and ltm), ability estimation (MLE, BME,
EAP, WLE, plausible values), item and person fit statistics, scaling
methods (MM, MS, Stocking-Lord, and the complete Hebaera method), and a rich
array of parametric and non-parametric (kernel) plots.  It estimates and plots
Haberman's interaction model when all items are dichotomously scored.")
    (license license:gpl2+)))

(define-public r-iheatmapr
  (package
    (name "r-iheatmapr")
    (version "0.4.12")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "iheatmapr" version))
       (sha256
        (base32
         "0s479j9l35xiss599vablxgvg6i2j9zq9sxphsq4vdk3bafg84bw"))))
    (properties `((upstream-name . "iheatmapr")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-fastcluster" ,r-fastcluster)
       ("r-ggdendro" ,r-ggdendro)
       ("r-htmlwidgets" ,r-htmlwidgets)
       ("r-jsonlite" ,r-jsonlite)
       ("r-knitr" ,r-knitr)
       ("r-magrittr" ,r-magrittr)
       ("r-plyr" ,r-plyr)
       ("r-rcolorbrewer" ,r-rcolorbrewer)
       ("r-s4vectors" ,r-s4vectors)
       ("r-scales" ,r-scales)))
    (home-page "https://docs.ropensci.org/iheatmapr")
    (synopsis "Interactive, Complex Heatmaps")
    (description
     "iheatmapr is an R package for building complex, interactive heatmaps
using modular building blocks.  \"Complex\" heatmaps are heatmaps in which
subplots along the rows or columns of the main heatmap add more information
about each row or column.  For example, a one column additional heatmap may
indicate what group a particular row or column belongs to.  Complex heatmaps
may also include multiple side by side heatmaps which show different types of
data for the same conditions.  Interactivity can improve complex heatmaps by
providing tooltips with information about each cell and enabling zooming into
interesting features. iheatmapr uses the plotly library for interactivity.")
    (license license:expat)))

(define-public r-packrat
  (package
    (name "r-packrat")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "packrat" version))
       (sha256
        (base32
         "1xy5dd2hrpqa07jfl4s7dsrya05mf36ms74j833scdz0zf89586n"))))
    (properties `((upstream-name . "packrat")))
    (build-system r-build-system)
    (home-page "https://github.com/rstudio/packrat/")
    (synopsis "Dependency management R projects")
    (description
     "This package provides a dependency manager for R projects that allows
you to manage the R packages your project depends on in an isolated, portable,
and reproducible way.")
    (license license:gpl2)))

(define-public r-rsconnect
  (package
    (name "r-rsconnect")
    (version "0.8.16")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rsconnect" version))
       (sha256
        (base32
         "05ii0p0p7xpf8z0c1594s5q7wpwcs7lmlddrd67s5p2ka5m8qwiz"))))
    (properties `((upstream-name . "rsconnect")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-curl" ,r-curl)
       ("r-digest" ,r-digest)
       ("r-jsonlite" ,r-jsonlite)
       ("r-openssl" ,r-openssl)
       ("r-packrat" ,r-packrat)
       ("r-rstudioapi" ,r-rstudioapi)
       ("r-yaml" ,r-yaml)))
    (home-page "https://github.com/rstudio/rsconnect")
    (synopsis "Deployment interface for R Markdown documents and Shiny applications")
    (description
     "This package provides a programmatic deployment interface for RPubs,
shinyapps.io, and RStudio Connect.  Supported content types include R Markdown
documents, Shiny applications, Plumber APIs, plots, and static web content.")
    (license license:gpl2)))

;; This package includes minified JavaScript files.  When upgrading please
;; check that there are no new minified JavaScript files.
(define-public r-dygraphs
  (package
    (name "r-dygraphs")
    (version "1.1.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "dygraphs" version))
       (sha256
        (base32
         "022j007mzfa9k2n31yg4aizcsf571vv3jip092h23rqj03rk3ly3"))))
    (properties `((upstream-name . "dygraphs")))
    (build-system r-build-system)
    (arguments
     `(#:modules ((guix build utils)
                  (guix build r-build-system)
                  (srfi srfi-1)
                  (ice-9 popen))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'process-javascript
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "inst/htmlwidgets/lib/"
               (call-with-values
                   (lambda ()
                     (unzip2
                      `(("dygraphs/dygraph-combined-dev.js"
                         "dygraph-combined.js")
                        (,(assoc-ref inputs "js-jquery")
                         "jquery/jquery.min.js")
                        (,(assoc-ref inputs "js-fquarter")
                         "fquarter/moment-fquarter.min.js"))))
                 (lambda (sources targets)
                   (for-each (lambda (source target)
                               (format #t "Processing ~a --> ~a~%"
                                       source target)
                               (let ((minified (open-pipe* OPEN_READ "uglify-js" source)))
                                 (call-with-output-file target
                                   (lambda (port)
                                     (dump-port minified port)))))
                             sources targets))))
             #t)))))
    (native-inputs
     `(("uglify-js" ,uglify-js)
       ;; They actually use version 1.11.1, but this more recent version
       ;; should be just fine.
       ("js-jquery"
        ,(origin
           (method url-fetch)
           (uri "https://code.jquery.com/jquery-1.12.4.js")
           (sha256
            (base32
             "0x9mrc1668icvhpwzvgafm8xm11x9lfai9nwr66aw6pjnpwkc3s3"))))
       ("js-fquarter"
        ,(origin
           (method url-fetch)
           (uri (string-append "https://raw.githubusercontent.com/robgallen/"
                               "moment-fquarter/1.0.1/moment-fquarter.js"))
           (sha256
            (base32
             "01mdnsaibm9jy2f1qpbn692hpv309lhj5si9nagib4dawmrkffij"))))))
    (propagated-inputs
     `(("r-htmltools" ,r-htmltools)
       ("r-htmlwidgets" ,r-htmlwidgets)
       ("r-magrittr" ,r-magrittr)
       ("r-xts" ,r-xts)
       ("r-zoo" ,r-zoo)))
    (home-page "https://github.com/rstudio/dygraphs")
    (synopsis "Interface to Dygraphs interactive time series charting library")
    (description
     "This package provides an R interface to the dygraphs JavaScript charting
library (a copy of which is included in the package).  It provides rich
facilities for charting time-series data in R, including highly configurable
series- and axis-display and interactive features like zoom/pan and
series/point highlighting.")
    (license license:expat)))

(define-public r-shinystan
  (package
    (name "r-shinystan")
    (version "2.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "shinystan" version))
       (sha256
        (base32
         "18alf5kiqw7y2l6m5nxxizwc2znsf9frxfsqhvgcad8hld9cbya5"))))
    (properties `((upstream-name . "shinystan")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-bayesplot" ,r-bayesplot)
       ("r-colourpicker" ,r-colourpicker)
       ("r-dt" ,r-dt)
       ("r-dygraphs" ,r-dygraphs)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gridextra" ,r-gridextra)
       ("r-gtools" ,r-gtools)
       ("r-markdown" ,r-markdown)
       ("r-reshape2" ,r-reshape2)
       ("r-rsconnect" ,r-rsconnect)
       ("r-rstan" ,r-rstan)
       ("r-shiny" ,r-shiny)
       ("r-shinyjs" ,r-shinyjs)
       ("r-shinythemes" ,r-shinythemes)
       ("r-threejs" ,r-threejs)
       ("r-xtable" ,r-xtable)
       ("r-xts" ,r-xts)))
    (home-page "https://mc-stan.org/")
    (synopsis "Interactive visual and numerical analysis for Bayesian models")
    (description
     "This package provides a graphical user interface for interactive
@dfn{Markov chain Monte Carlo} (MCMC) diagnostics and plots and tables helpful
for analyzing a posterior sample.  The interface is powered by the Shiny web
application framework and works with the output of MCMC programs written in
any programming language (and has extended functionality for Stan models fit
using the @code{rstan} and @code{rstanarm} packages).")
    (license license:gpl3+)))

(define-public r-rstantools
  (package
    (name "r-rstantools")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rstantools" version))
       (sha256
        (base32
         "1il0pn4ksbdkska5fmhvgaicvwnnc6cs08g6ags9fj1xkjiqmrsa"))))
    (properties `((upstream-name . "rstantools")))
    (build-system r-build-system)
    (inputs `(("pandoc" ,ghc-pandoc)))
    (propagated-inputs
     `(("r-desc" ,r-desc)
       ("r-rcpp" ,r-rcpp)))
    (home-page "https://mc-stan.org/rstantools/")
    (synopsis "Tools for developing R packages interfacing with Stan")
    (description
     "This package provides various tools for developers of R packages
interfacing with @url{https://mc-stan.org, Stan}, including functions to set
up the required package structure, S3 generics and default methods to unify
function naming across Stan-based R packages, and vignettes with
recommendations for developers.")
    (license license:gpl3+)))

(define-public r-loo
  (package
    (name "r-loo")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "loo" version))
       (sha256
        (base32
         "1hq1zcj76x55z9kic6cwf7mfq9pzqfbr341jbc9wp7x8ac4zcva6"))))
    (properties `((upstream-name . "loo")))
    (build-system r-build-system)
    (inputs
     `(("pandoc" ,ghc-pandoc)
       ("pandoc-citeproc" ,ghc-pandoc-citeproc)))
    (propagated-inputs
     `(("r-checkmate" ,r-checkmate)
       ("r-matrixstats" ,r-matrixstats)))
    (home-page "https://mc-stan.org/loo/")
    (synopsis "Leave-One-Out cross-validation and WAIC for Bayesian models")
    (description
     "This package provides an implementation of efficient approximate
@dfn{leave-one-out} (LOO) cross-validation for Bayesian models fit using
Markov chain Monte Carlo, as described in @url{doi:10.1007/s11222-016-9696-4}.
The approximation uses @dfn{Pareto smoothed importance sampling} (PSIS), a new
procedure for regularizing importance weights.  As a byproduct of the
calculations, we also obtain approximate standard errors for estimated
predictive errors and for the comparison of predictive errors between models.
The package also provides methods for using stacking and other model weighting
techniques to average Bayesian predictive distributions.")
    (license license:gpl3+)))

(define-public r-rstan
  (package
    (name "r-rstan")
    (version "2.19.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rstan" version))
       (sha256
        (base32
         "128ndwjrhf8b1qvvqz4bl13qlm8718z9qs5ryc6gsdr3vk65s0np"))))
    (properties `((upstream-name . "rstan")))
    (build-system r-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'set-timezone
           ;; This package is picky about timezones.
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "TZ" "UTC+1")
             (setenv "TZDIR"
                     (string-append (assoc-ref inputs "tzdata")
                                    "/share/zoneinfo"))
             #t)))))
    (native-inputs
     `(("tzdata" ,tzdata)
       ("pandoc" ,ghc-pandoc)))
    (propagated-inputs
     `(("r-bh" ,r-bh)
       ("r-ggplot2" ,r-ggplot2)
       ("r-gridextra" ,r-gridextra)
       ("r-inline" ,r-inline)
       ("r-loo" ,r-loo)
       ("r-pkgbuild" ,r-pkgbuild)
       ("r-rcpp" ,r-rcpp)
       ("r-rcppeigen" ,r-rcppeigen)
       ("r-stanheaders" ,r-stanheaders)))
    (home-page "https://discourse.mc-stan.org/")
    (synopsis "R interface to Stan")
    (description
     "User-facing R functions are provided to parse, compile, test, estimate,
and analyze Stan models by accessing the header-only Stan library provided by
the StanHeaders package.  The Stan project develops a probabilistic
programming language that implements full Bayesian statistical inference via
Markov Chain Monte Carlo, rough Bayesian inference via 'variational'
approximation, and (optionally penalized) maximum likelihood estimation via
optimization.  In all three cases, automatic differentiation is used to
quickly and accurately evaluate gradients without burdening the user with the
need to derive the partial derivatives.")
    (license license:gpl3+)))

(define-public r-rstanarm
  (package
    (name "r-rstanarm")
    (version "2.19.3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "rstanarm" version))
       (sha256
        (base32
         "0gxjq8bdlvdd8kn3dhp12xlymdab036r7n12lzmd3xlkl4cnxq3s"))))
    (properties `((upstream-name . "rstanarm")))
    (build-system r-build-system)
    (inputs
     `(("pandoc" ,ghc-pandoc)
       ("pandoc-citeproc" ,ghc-pandoc-citeproc)))
    (propagated-inputs
     `(("r-bayesplot" ,r-bayesplot)
       ("r-bh" ,r-bh)
       ("r-ggplot2" ,r-ggplot2)
       ("r-lme4" ,r-lme4)
       ("r-loo" ,r-loo)
       ("r-matrix" ,r-matrix)
       ("r-nlme" ,r-nlme)
       ("r-rcpp" ,r-rcpp)
       ("r-rcppeigen" ,r-rcppeigen)
       ("r-rcppparallel" ,r-rcppparallel)
       ("r-rstan" ,r-rstan)
       ("r-rstantools" ,r-rstantools)
       ("r-shinystan" ,r-shinystan)
       ("r-stanheaders" ,r-stanheaders)
       ("r-survival" ,r-survival)))
    (home-page "https://mc-stan.org/rstanarm/")
    (synopsis "Bayesian applied regression modeling via Stan")
    (description
     "This package estimates previously compiled regression models using the
@code{rstan} package, which provides the R interface to the Stan C++ library
for Bayesian estimation.  Users specify models via the customary R syntax with
a formula and @code{data.frame} plus some additional arguments for priors.")
    (license license:gpl3+)))

(define-public r-kendall
  (package
    (name "r-kendall")
    (version "2.2")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Kendall" version))
       (sha256
        (base32
         "0z2yr3x2nvdm81w2imb61hxwcbmg14kfb2bxgh3wmkmv3wfjwkwn"))))
    (properties `((upstream-name . "Kendall")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-boot" ,r-boot)))
    (native-inputs
     `(("gfortran" ,gfortran)))
    (home-page "https://cran.r-project.org/web/packages/Kendall/")
    (synopsis "Kendall rank correlation and Mann-Kendall trend test")
    (description
     "This package computes the Kendall rank correlation and Mann-Kendall
trend test.")
    (license license:gpl2+)))

(define-public r-zyp
  (package
    (name "r-zyp")
    (version "0.10-1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "zyp" version))
       (sha256
        (base32
         "03cxpkfbhrx1fy8l0dl9a13ghz93cqq6877wa8rig09ksdiivaw9"))))
    (properties `((upstream-name . "zyp")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-kendall" ,r-kendall)))
    (home-page "https://cran.r-project.org/web/packages/zyp/")
    (synopsis "Zhang + Yue-Pilon Trends Package")
    (description
     "This package contains an efficient implementation of Sen's slope
method (Sen, 1968) plus implementation of Xuebin Zhang's (Zhang, 1999) and
Yue-Pilon's (Yue, 2002) pre-whitening approaches to determining trends in
climate data.")
    (license license:lgpl2.1)))

(define-public r-zvcv
  (package
    (name "r-zvcv")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ZVCV" version))
       (sha256
        (base32
         "1npw836q2skx54843lgxvb0rfwafckjc8k8dljykm60ad3z7zak8"))))
    (properties `((upstream-name . "ZVCV")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-abind" ,r-abind)
       ("r-glmnet" ,r-glmnet)
       ("r-mvtnorm" ,r-mvtnorm)
       ("r-partitions" ,r-partitions)
       ("r-rcpp" ,r-rcpp)
       ("r-rcpparmadillo" ,r-rcpparmadillo)))
    (home-page "https://cran.r-project.org/web/packages/ZVCV/")
    (synopsis "Zero-Variance Control Variates")
    (description
     "@dfn{Zero-variance control variates} (ZV-CV) is a post-processing method
to reduce the variance of Monte Carlo estimators of expectations using the
derivatives of the log target.  Once the derivatives are available, the only
additional computational effort is in solving a linear regression problem.
This method has been extended to higher dimensions using regularisation.  This
package can be used to easily perform ZV-CV or regularised ZV-CV when a set of
samples, derivatives and function evaluations are available.  Additional
functions for applying ZV-CV to two estimators for the normalising constant of
the posterior distribution in Bayesian statistics are also supplied.")
    (license license:gpl2+)))

(define-public r-ztype
  (package
    (name "r-ztype")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ztype" version))
       (sha256
        (base32
         "0brbq2rgkl4mhjbb70kkfv47lzs66k9ppfs2klavcbripirxn5fx"))))
    (properties `((upstream-name . "ztype")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-assertthat" ,r-assertthat)
       ("r-dplyr" ,r-dplyr)
       ("r-ggplot2" ,r-ggplot2)
       ("r-lubridate" ,r-lubridate)
       ("r-magrittr" ,r-magrittr)
       ("r-rvest" ,r-rvest)
       ("r-stringr" ,r-stringr)))
    (home-page "https://cran.r-project.org/web/packages/ztype/")
    (synopsis "Run a Ztype game loaded with R functions")
    (description
     "How fast can you type R functions on your keyboard?  Find out by running
a @code{zty.pe} game: export R functions as instructions to type to destroy
opponents' vessels.")
    (license license:gpl3)))

(define-public r-zseq
  (package
    (name "r-zseq")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Zseq" version))
       (sha256
        (base32
         "06dndi2b1q79bmxax11bv0l5ifcz1mhpvbn90y6a6xymrgcq0ivi"))))
    (properties `((upstream-name . "Zseq")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-gmp" ,r-gmp)))
    (home-page "https://cran.r-project.org/web/packages/Zseq/")
    (synopsis "Integer sequence generator")
    (description
     "This package generates well-known integer sequences.  The @code{gmp}
package is adopted for computing with arbitrarily large numbers.  Every
function has a hyperlink to its corresponding item in the @dfn{On-Line
Encyclopedia of Integer Sequences} (OEIS) in the function help page.")
    (license license:gpl3+)))

(define-public r-isoband
  (package
    (name "r-isoband")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "isoband" version))
       (sha256
        (base32
         "1r023s73qypnvpx18znr9ymylr022m90v65mz2jasn0a1kjrfcbq"))))
    (properties `((upstream-name . "isoband")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-rcpp" ,r-rcpp)
       ("r-testthat" ,r-testthat)))
    (home-page "https://github.com/wilkelab/isoband")
    (synopsis "Generate isolines and isobands from regularly spaced elevation grids")
    (description
     "This package provides a fast C++ implementation to generate contour
lines (isolines) and contour polygons (isobands) from regularly spaced grids
containing elevation data.")
    (license license:expat)))

(define-public r-ppcor
  (package
    (name "r-ppcor")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "ppcor" version))
       (sha256
        (base32
         "1x9b2kb8s0bp92b17gby0jwzzr3i4cf3ap9c4nq7m8fav72g0y3a"))))
    (properties `((upstream-name . "ppcor")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-mass" ,r-mass)))
    (home-page "https://cran.r-project.org/web/packages/ppcor/")
    (synopsis "Partial and semi-partial correlation")
    (description
     "This package provides users not only with a function to readily
calculate the higher-order partial and semi-partial correlations but also with
statistics and p-values of the correlation coefficients.")
    (license license:gpl2)))

(define-public r-hrbrthemes
  (package
    (name "r-hrbrthemes")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "hrbrthemes" version))
       (sha256
        (base32 "057h60b5p53dcyjyfwlgjc1ry968s9s64dw78p443w8717zk7zpc"))))
    (properties `((upstream-name . "hrbrthemes")))
    (build-system r-build-system)
    (propagated-inputs
     `(("r-extrafont" ,r-extrafont)
       ("r-gdtools" ,r-gdtools)
       ("r-ggplot2" ,r-ggplot2)
       ("r-htmltools" ,r-htmltools)
       ("r-knitr" ,r-knitr)
       ("r-magrittr" ,r-magrittr)
       ("r-rmarkdown" ,r-rmarkdown)
       ("r-scales" ,r-scales)))
    (native-inputs
     `(("r-knitr" ,r-knitr)))
    (home-page "https://github.com/hrbrmstr/hrbrthemes/")
    (synopsis "Additional themes, theme components and utilities for @code{ggplot2}")
    (description
     "This package provides a compilation of extra @code{ggplot2} themes,
scales and utilities, including a spell check function for plot label fields
and an overall emphasis on typography.")
    (license license:expat)))
