;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
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

(define-module (gnu packages maths)
  #:use-module (gnu packages)
  #:use-module ((guix licenses)
                #:renamer (symbol-prefix-proc 'license:))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages compression)
  #:use-module ((gnu packages gettext)
                #:renamer (symbol-prefix-proc 'gnu:))
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages xml))

(define-public units
  (package
   (name "units")
   (version "2.01")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/units/units-" version
                                ".tar.gz"))
            (sha256 (base32
                     "1wl8kaxgk4l5jgc1f71mx1rwa6ib84krjmzyzn2f987z1g3i52qk"))))
   (build-system gnu-build-system)
   (synopsis "Conversion between thousands of scales")
   (description
    "GNU Units converts quantities expressed in various systems of
measurement to their equivalents in other systems of measurement.  Like
many similar programs, it can handle multiplicative scale changes.  It can
also handle nonlinear conversions such as Fahrenheit to Celsius or wire
gauge, and it can convert from and to sums of units, such as converting
between meters and feet plus inches.

Beyond simple unit conversions, GNU Units can be used as a general-purpose
scientific calculator that keeps track of units in its calculations.  You
can form arbitrary complex mathematical expressions of dimensions including
sums, products, quotients, powers, and even roots of dimensions.  Thus you
can ensure accuracy and dimensional consistency when working with long
expressions that involve many different units that may combine in complex
ways.

The units are defined in an external data file.  You can use the extensive
data file that comes with this program, or you can provide your own data
file to suit your needs.  You can also use your own data file to supplement
the standard data file.")
   (license license:gpl3+)
   (home-page "http://www.gnu.org/software/units/")))

(define-public pspp
  (package
    (name "pspp")
    (version "0.8.0a")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/pspp/pspp-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1pgkb3z8b4wk4gymnafclhkrqq7n05wq83mra3v53jdl6bnllmyq"))))
    (build-system gnu-build-system)
    (inputs
     `(("gettext" ,gnu:gettext)
       ("gsl" ,gsl)
       ("libxml2" ,libxml2)
       ("readline" ,readline)
       ("zlib" ,zlib)))
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (arguments
     `(#:configure-flags
       `("--without-cairo" ; FIXME: tests currently fail for lack of font
         "--without-gui"))) ; FIXME: package missing dependencies
    (home-page "http://www.gnu.org/software/pspp/")
    (synopsis "Statistical analysis")
    (description
     "PSPP is a program for statistical analysis of sampled data.  It is a
free replacement for the proprietary program SPSS, and appears very similar
to it.

PSPP can perform descriptive statistics, T-tests, anova, linear and logistic
regression, cluster analysis, factor analysis, non-parametric tests and
more.  Its backend is designed to perform its analyses as fast as possible,
regardless of the size of the input data.  You can use PSPP with its
graphical interface or the more traditional syntax commands.")
    (license license:gpl3+)))
