;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages multiprecision)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (gnu packages m4)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu))

(define-public gmp
  (package
   (name "gmp")
   (version "6.1.2")
   (source (origin
            (method url-fetch)
            (uri
             (string-append "mirror://gnu/gmp/gmp-"
                            version ".tar.xz"))
            (sha256
             (base32
              "04hrwahdxyqdik559604r7wrj9ffklwvipgfxgj4ys4skbl6bdc7"))
            (patches (search-patches "gmp-faulty-test.patch"))))
   (build-system gnu-build-system)
   (native-inputs `(("m4" ,m4)))
   (outputs '("out" "debug"))
   (arguments `(#:parallel-tests? #f ; mpz/reuse fails otherwise
                #:configure-flags
                '(;; Build a "fat binary", with routines for several
                  ;; sub-architectures.
                  "--enable-fat"
                  "--enable-cxx"
                  ,@(cond ((target-mingw?)
                           ;; Static and shared cannot be built in one go:
                           ;; they produce different headers.  We need shared.
                           `("--disable-static"
                             "--enable-shared"))
                          (else '())))))
   (synopsis "Multiple-precision arithmetic library")
   (description
    "GMP is a library for arbitrary precision arithmetic, operating on
signed integers, rational numbers and floating point numbers.  The precision
is only limited by the available memory.  The library is highly optimized,
with a design focus on execution speed.  It is aimed at use in, for example,
cryptography and computational algebra.")
   (license lgpl3+)
   (home-page "https://gmplib.org/")))

(define-public gmp-6.0
  ;; We keep this one around to bootstrap GCC, to work around a compilation
  ;; issue on ARM.  See
  ;; <https://gmplib.org/list-archives/gmp-bugs/2015-December/003848.html>.
  (package
    (inherit gmp)
    (version "6.0.0a")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gmp/gmp-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0r5pp27cy7ch3dg5v0rsny8bib1zfvrza6027g2mp5f6v8pd6mli"))
              (patches (search-patches "gmp-arm-asm-nothumb.patch"
                                       "gmp-faulty-test.patch"))))))

(define-public mpfr
  (package
   (name "mpfr")
   (version "3.1.5")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/mpfr/mpfr-" version
                                ".tar.xz"))
            (sha256 (base32
                     "1g32l2fg8f62lcyzzh88y3fsh6rk539qc6ahhdgvx7wpnf1dwpq1"))))
   (build-system gnu-build-system)
   (outputs '("out" "debug"))
   (propagated-inputs `(("gmp" ,gmp)))            ; <mpfr.h> refers to <gmp.h>
   (synopsis "C library for arbitrary-precision floating-point arithmetic")
   (description
    "GNU@tie{}@dfn{MPFR} (Multiple Precision Floating-Point Reliably) is a C
library for performing multiple-precision, floating-point computations with
correct rounding.")
   (license lgpl3+)
   (home-page "http://www.mpfr.org/")))

(define-public mpc
  (package
   (name "mpc")
   (version "1.0.3")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "mirror://gnu/mpc/mpc-" version ".tar.gz"))
            (sha256
              (base32
                "1hzci2zrrd7v3g1jk35qindq05hbl0bhjcyyisq9z209xb3fqzb1"))))
   (build-system gnu-build-system)
   (outputs '("out" "debug"))
   (propagated-inputs `(("gmp" ,gmp)              ; <mpc.h> refers to both
                        ("mpfr" ,mpfr)))
   (synopsis "C library for arbitrary-precision complex arithmetic")
   (description
    "GNU@tie{}@dfn{MPC} (Multiple Precision Complex library) is a C library for
performing arithmetic on complex numbers.  It supports arbitrarily high
precision and correctly rounds the results.")
   (license lgpl3+)
   (home-page "http://multiprecision.org/mpc/")))

(define-public mpfi
  (package
    (name "mpfi")
    (version "1.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://gforge.inria.fr/frs/download.php"
                           "/latestfile/181/" name "-" version ".tar.bz2"))
       (sha256
        (base32 "0bqr8yibl7jbrp0bw7xk1lm7nis7rv26jsz6y8ycvih8n9bx90r3"))))
    (build-system gnu-build-system)
    (propagated-inputs `(("gmp" ,gmp)   ; <mpfi.h> refers to both
                         ("mpfr" ,mpfr)))
    (synopsis "C library for arbitrary-precision interval arithmetic")
    (description
     "@dfn{MPFI} (Multiple Precision Floating-point Interval) is a portable C
library for arbitrary-precision interval arithmetic, with intervals represented
using MPFR reliable floating-point numbers.  It's based on the @dfn{GMP} (GNU
Multiple Precision Arithmetic) and GNU@tie{}@dfn{MPFR} (Multiple Precision
Floating-Point Reliably) libraries.

The purpose of arbitrary-precision interval arithmetic is to get results that
are both guaranteed, thanks to interval computation, and accurate, thanks to
multiple-precision arithmetic.")
    (license lgpl2.1+)
    (home-page "https://perso.ens-lyon.fr/nathalie.revol/software.html")))
