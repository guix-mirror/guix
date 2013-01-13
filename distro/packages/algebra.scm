;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (distro packages perl)
  #:use-module (distro packages readline)
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

(define-public gsl
  (package
    (name "gsl")
    (version "1.15")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/gsl/gsl-"
                          version ".tar.gz"))
      (sha256
       (base32
        "18qf6jzz1r3mzb5qynywv4xx3z9g61hgkbpkdrhbgqh2g7jhgfc5"))))
    (build-system gnu-build-system)
    (home-page "http://www.gnu.org/software/gsl/")
    (synopsis "The GNU Scientific Library, a large numerical library")
    (description
     "The GNU Scientific Library (GSL) is a numerical library for C
and C++ programmers.  It is free software under the GNU General
Public License.

The library provides a wide range of mathematical routines such
as random number generators, special functions and least-squares
fitting.  There are over 1000 functions in total with an
extensive test suite.")
    (license gpl3+)))

(define-public pari-gp
  (package
   (name "pari-gp")
   (version "2.5.3")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "http://pari.math.u-bordeaux.fr/pub/pari/unix/pari-"
                  version ".tar.gz"))
            (sha256 (base32
                     "0zsjccnnv00kwj2gk3ww2v530kjin1rgj8p8hbl4pwcnwc7m68gl"))))
   (build-system gnu-build-system)
   (inputs `(("gmp" ,gmp)
             ("perl" ,perl)
             ("readline" ,readline)))
   (arguments
    (lambda (system)
      `(#:make-flags '("gp")
;; FIXME: building the documentation requires tex; once this is available,
;; replace "gp" by "all"
        #:test-target "dobench"
        #:phases
        (alist-replace
         'configure
         (lambda* (#:key inputs outputs #:allow-other-keys)
           (let ((out (assoc-ref outputs "out"))
                 (readline (assoc-ref inputs "readline"))
                 (gmp (assoc-ref inputs "gmp")))
             (zero?
              (system* "./Configure"
                       (string-append "--prefix=" out)
                       (string-append "--with-readline=" readline)
                       (string-append "--with-gmp=" gmp)))))
;; FIXME: readline and gmp will be detected automatically in the next
;; stable release
         %standard-phases))))
   (synopsis "PARI/GP, a computer algebra system for number theory")
   (description
    "PARI/GP is a widely used computer algebra system designed for fast
computations in number theory (factorisations, algebraic number theory,
elliptic curves...), but it also contains a large number of other useful
functions to compute with mathematical entities such as matrices,
polynomials, power series, algebraic numbers, etc., and a lot of
transcendental functions.
PARI is also available as a C library to allow for faster computations.")
   (license gpl2+)
   (home-page "http://pari.math.u-bordeaux.fr/")))
