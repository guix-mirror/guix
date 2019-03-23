;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2018 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages gcc)
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
    "@dfn{GMP} (the GNU Multiple Precision Arithmetic Library) is a library for
arbitrary-precision arithmetic, operating on signed integers, rational numbers
and floating point numbers.  The precision is only limited by the available
memory.  The library is highly optimized, with a design focus on execution
speed.  It is aimed at use in, for example, cryptography and computational
algebra.")
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
   (version "4.0.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/mpfr/mpfr-" version
                                ".tar.xz"))
            (sha256 (base32
                     "0vp1lrc08gcmwdaqck6bpzllkrykvp06vz5gnqpyw0v3h9h4m1v7"))))
   (build-system gnu-build-system)
   (outputs '("out" "debug"))
   (propagated-inputs `(("gmp" ,gmp)))            ; <mpfr.h> refers to <gmp.h>
   (synopsis "C library for arbitrary-precision floating-point arithmetic")
   (description
    "GNU@tie{}@dfn{MPFR} (Multiple Precision Floating-Point Reliably) is a C
library for performing multiple-precision, floating-point computations with
correct rounding.")
   (license lgpl3+)
   (home-page "https://www.mpfr.org/")))

(define-public mpc
  (package
   (name "mpc")
   (version "1.1.0")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "mirror://gnu/mpc/mpc-" version ".tar.gz"))
            (sha256
              (base32
                "0biwnhjm3rx3hc0rfpvyniky4lpzsvdcwhmcn7f0h4iw2hwcb1b9"))))
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

(define-public irram
  (package
    (name "irram")
    (version "2013_01")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://irram.uni-trier.de/irram-files/iRRAM_"
                           version ".tar.bz2"))
       (sha256
        (base32 "1cdmvb4hsa161rfdjqyhd9sb3fcr43p3a6nsj7cb4kn9f94qmjpj"))))
    (build-system gnu-build-system)
    (propagated-inputs `(("gmp" ,gmp)   ; <mpfi.h> refers to both
                         ("mpfr" ,mpfr)))
    (arguments
     `(#:parallel-build? #f))
    (synopsis "C++ package for real arithmetic based on the Real-RAM concept")
    (description
     "@dfn{iRRAM} is a C++ package for error-free real arithmetic based on
the concept of a Real-RAM.  Its capabilities range from ordinary arithmetic
over trigonometric functions to linear algebra and differential
equations.  A program using iRRAM is coded in ordinary C++, but may use a
special class that behaves like real numbers without any
error.  Additionally, iRRAM uses the concept of multi-valued functions.")
    (license lgpl2.0+)
    (home-page "http://irram.uni-trier.de/")))

(define-public qd
  (package
    (name "qd")
    (version "2.3.22")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://crd.lbl.gov/~dhbailey/mpdist/qd-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0wpgdzjcbanwd0c9mk90n04nas0q5fwc5zkrlbxyn6yjd2n8k3i6"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gfortran" ,gfortran)))
    (arguments
     `(#:configure-flags `("--disable-enable_fma" ;weird :/
                           "--enable-shared"
                           ,,@(if (string-prefix? "aarch64"
                                                  (or (%current-target-system)
                                                      (%current-system)))
                                  ;; XXX: The qd_test test fails numerical
                                  ;; accuracy checks for 'dd_real::exp()' on
                                  ;; aarch64 with GCC 5.4 at -O2.  Disabling
                                  ;; expensive optimizations lets it pass.
                                  '("CXXFLAGS=-O3 -fno-expensive-optimizations")
                                  '("CXXFLAGS=-O3")))))
    (home-page "http://crd-legacy.lbl.gov/~dhbailey/mpdist/")
    (synopsis "Double-double and quad-double library")
    (description "This package supports both a double-double
datatype (approx. 32 decimal digits) and a quad-double datatype (approx. 64
decimal digits).  The computational library is written in C++.  Both C++ and
Fortran-90 high-level language interfaces are provided to permit one to
convert an existing C++ or Fortran-90 program to use the library with only
minor changes to the source code.  In most cases only a few type statements
and (for Fortran-90 programs) read/write statements need to be changed.  PSLQ
and numerical quadrature programs are included.")
    (license bsd-3)))

(define-public tomsfastmath
  (package
    (name "tomsfastmath")
    (version "0.13.1")
    (synopsis "Large integer arithmetic library")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/libtom/tomsfastmath/"
                                  "releases/download/v" version "/"
                                  "tfm-" version ".tar.xz"))
              (sha256
               (base32
                "0f0pmiaskh89sp0q933pafxb914shpaj5ad8sb5rzk1wv8d7mja7"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("libtool" ,libtool)))
    (arguments
     `(#:make-flags (list "-f" "makefile.shared"
                          (string-append "LIBPATH=" %output "/lib")
                          (string-append "INCPATH=" %output "/include")
                          "GROUP=root" "USER=root"
                          "CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configuration
         (replace 'check
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "make"
                    "stest" "test_standalone"
                    make-flags)
             (invoke "./stest")
             (invoke "./test")))
         (add-before 'install 'install-nogroup
           (lambda _
             ;; Let permissions inherit from the current process.
             (substitute* "makefile.shared"
               (("-g \\$\\(GROUP\\) -o \\$\\(USER\\)") ""))
             #t))
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((docdir (string-append (assoc-ref outputs "out")
                                          "/share/doc/tomsfastmath")))
               (install-file "doc/tfm.pdf" docdir)
               #t)))
         (add-after 'install 'install-pc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (pc-dir (string-append out "/lib/pkgconfig")))
               (call-with-output-file "tomsfastmath.pc"
                 (lambda (port)
                   (format port "~
Name: TomsFastMath
Description: ~a
Version: ~a
Libs: -L~a/lib -ltfm~%"
                           ,synopsis ,version out)))
               (install-file "tomsfastmath.pc" pc-dir)
               #t))))))
    (home-page "https://www.libtom.net/TomsFastMath/")
    (description "TomsFastMath is a large integer library written in portable
ISO C.  It is a port of LibTomMath with optional support for inline assembler
multiplies.")
    (license public-domain)))

(define-public libtommath
  (package
    (name "libtommath")
    (version "1.1.0")
    (outputs '("out" "static"))
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/libtom/libtommath/releases/"
                            "download/v" version "/ltm-" version ".tar.xz"))
        (sha256
         (base32
          "1bbyagqzfdbg37k1n08nsqzdf44z8zsnjjinqbsyj7rxg246qilh"))
        (patches (search-patches "libtommath-fix-linkage.patch"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure) ; no configure
         (add-after 'unpack 'prepare-build
           (lambda _
             ;; Don't pull in coreutils.
             (substitute* "makefile_include.mk"
               (("arch") "uname -m"))

             ;; We want the shared library by default so force it to be the
             ;; default makefile target.
             (delete-file "makefile")
             (symlink "makefile.shared" "makefile")
             #t))
         (add-after 'install 'remove-static-library
           (lambda* (#:key outputs #:allow-other-keys)
             (delete-file (string-append (assoc-ref outputs "out")
                                         "/lib/libtommath.a"))
             #t))
         (replace 'check
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "make" "test_standalone" make-flags)
             (invoke "sh" "test")))
         (add-after 'install 'install-static-library
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "make" "-f" "makefile.unix" "install"
                     (string-append "PREFIX=" (assoc-ref outputs "static"))
                     (string-append "CC=" (which "gcc"))))))
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
                          "CC=gcc")))
    (native-inputs
     `(("libtool" ,libtool)))
    (home-page "https://www.libtom.net/LibTomMath/")
    (synopsis "Portable number theoretic multiple-precision integer library")
    (description "LibTomMath is a portable number theoretic multiple-precision
integer library written entirely in C.  It's designed to provide an API that is
simple to work with that provides fairly efficient routines that build out of
the box without configuration.")
    (license unlicense)))

(define-public libtommath-1.0
  (package
    (inherit libtommath)
    (version "1.0.1")
    (outputs '("out"))
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/libtom/libtommath/releases/"
                            "download/v" version "/ltm-" version ".tar.xz"))
        (sha256
         (base32
          "0sbccdwbkfc680id2fi0x067j23biqcjqilwkk7y9339knrjy0s7"))))
    (arguments
      (substitute-keyword-arguments (package-arguments libtommath)
        ((#:phases phases)
         `(modify-phases ,phases
            (delete 'install-static-library)))))))
