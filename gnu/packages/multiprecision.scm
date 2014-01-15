;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix build-system gnu))

(define-public gmp
  (package
   (name "gmp")
   (version "5.1.3")
   (source (origin
            (method url-fetch)
            (uri
             (string-append "mirror://gnu/gmp/gmp-"
                            version ".tar.xz"))
            (sha256
             (base32
              "0wbhn3wih61vjcs94q531fipfvvzqfq2v4qr03rl3xaggyiyvqny"))))
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
   (version "3.1.2")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/mpfr/mpfr-" version
                                ".tar.xz"))
            (sha256 (base32
                     "0fs501qi8l523gs3cpy4jjcnvwxggyfbklcys80wq236xx3hz79r"))))
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
   (version "1.0.2")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "mirror://gnu/mpc/mpc-" version ".tar.gz"))
            (sha256 (base32
                     "1264h3ivldw5idph63x35dqqdzqqbxrm5vlir0xyx727i96zaqdm"))))
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
