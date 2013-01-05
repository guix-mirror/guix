;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Andreas Enge <andreas@enge.fr>
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

(define-module (distro packages algebra)
  #:use-module (distro)
  #:use-module (distro packages multiprecision)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))


(define-public mpfrcx
  (package
   (name "mpfrcx")
   (version "0.4.1")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "http://www.multiprecision.org/mpfrcx/download/mpfrcx-"
                  version ".tar.gz"))
            (sha256
             (base32
              "1rrc75chxyicqjgg5mfhgbz7p9mx1fgh0qlx14a82m25vfhifnd1"))))
   (build-system gnu-build-system)
   (inputs `(("gmp" ,gmp)
             ("mpfr" ,mpfr)
             ("mpc"  ,mpc)))
   (synopsis "mpfrcx, a library for the arithmetic of univariate polynomials
over arbitrary precision real or complex numbers")
   (description
    "mpfrcx is a library for the arithmetic of univariate polynomials over
arbitrary precision real (mpfr) or complex (mpc) numbers, without control
on the rounding. For the time being, only the few functions needed to
implement the floating point approach to complex multiplication are
implemented. On the other hand, these comprise asymptotically fast
multiplication routines such as Toom–Cook and the FFT. ")
   (license lgpl2.1+)
   (home-page "http://mpfrcx.multiprecision.org/")))


(define-public fplll
  (package
   (name "fplll")
   (version "4.0.1")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "http://perso.ens-lyon.fr/damien.stehle/fplll/libfplll-"
                  version ".tar.gz"))
            (sha256 (base32
                     "122bpqdlikshhd7nmq0l5qfc0agyk7x21gvplv1l9hb77l8cy9rw"))))
   (build-system gnu-build-system)
   (inputs `(("gmp" ,gmp)
             ("mpfr" ,mpfr)))
   (synopsis "fplll, a library for LLL-reduction of euclidean lattices")
   (description
    "fplll LLL-reduces euclidean lattices. Since version 3, it can also
solve the shortest vector problem.")
   (license lgpl2.1+)
   (home-page "http://perso.ens-lyon.fr/damien.stehle/fplll/")))
