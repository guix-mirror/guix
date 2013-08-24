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
  #:use-module (guix build-system gnu))

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
