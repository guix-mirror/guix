;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
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
   (version "6.0.0a")
   (source (origin
            (method url-fetch)
            (uri
             (string-append "mirror://gnu/gmp/gmp-"
                            version ".tar.xz"))
            (sha256
             (base32
              "0r5pp27cy7ch3dg5v0rsny8bib1zfvrza6027g2mp5f6v8pd6mli"))
            (patches (list (search-patch "gmp-arm-asm-nothumb.patch")))))
   (build-system gnu-build-system)
   (native-inputs `(("m4" ,m4)))
   (outputs '("out" "debug"))
   (arguments `(#:configure-flags
                '(;; Build a "fat binary", with routines for several
                  ;; sub-architectures.
                  "--enable-fat"
                  "--enable-cxx")))
   (synopsis "Multiple-precision arithmetic library")
   (description
    "GMP is a library for arbitrary precision arithmetic, operating on
signed integers, rational numbers and floating point numbers.  The precision
is only limited by the available memory.  The library is highly optimized,
with a design focus on execution speed.  It is aimed at use in, for example,
cryptography and computational algebra.")
   (license lgpl3+)
   (home-page "http://gmplib.org/")))

(define-public mpfr
  (package
   (name "mpfr")
   (version "3.1.3")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/mpfr/mpfr-" version
                                ".tar.xz"))
            (sha256 (base32
                     "05jaa5z78lvrayld09nyr0v27c1m5dm9l7kr85v2bj4jv65s0db8"))))
   (build-system gnu-build-system)
   (outputs '("out" "debug"))
   (propagated-inputs `(("gmp" ,gmp)))            ; <mpfr.h> refers to <gmp.h>
   (synopsis "C library for arbitrary precision floating-point arithmetic")
   (description
    "GNU MPFR is a C library for performing multiple-precision,
floating-point computations with correct rounding.")
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
   (synopsis "C library for arbitrary precision complex arithmetic")
   (description
    "GNU MPC is a C library for performing arithmetic on complex numbers.
It supports arbitrarily high precision and it correctly rounds the results.")
   (license lgpl3+)
   (home-page "http://mpc.multiprecision.org/")))
