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

(define-module (gnu packages algebra)
  #:use-module (gnu packages)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages flex)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))


(define-public mpfrcx
  (package
   (name "mpfrcx")
   (version "0.4.2")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "http://www.multiprecision.org/mpfrcx/download/mpfrcx-"
                  version ".tar.gz"))
            (sha256
             (base32
              "0grw66b255r574lvll1bqccm5myj2m8ajzsjaygcyq9zjnnbnhhy"))))
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
   (version "4.0.4")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "http://perso.ens-lyon.fr/damien.stehle/fplll/libfplll-"
                  version ".tar.gz"))
            (sha256 (base32
                     "1cbiby7ykis4z84swclpysrljmqhfcllpkcbll1m08rzskgb1a6b"))))
   (build-system gnu-build-system)
   (inputs `(("gmp" ,gmp)
             ("mpfr" ,mpfr)))
   (synopsis "fplll, a library for LLL-reduction of euclidean lattices")
   (description
    "fplll LLL-reduces euclidean lattices. Since version 3, it can also
solve the shortest vector problem.")
   (license lgpl2.1+)
   (home-page "http://perso.ens-lyon.fr/damien.stehle/fplll/")))

(define-public pari-gp
  (package
   (name "pari-gp")
   (version "2.5.5")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "http://pari.math.u-bordeaux.fr/pub/pari/unix/pari-"
                  version ".tar.gz"))
            (sha256 (base32
                     "058nw1fhggy7idii4f124ami521lv3izvngs9idfz964aks8cvvn"))))
   (build-system gnu-build-system)
   (inputs `(("gmp" ,gmp)
             ("perl" ,perl)
             ("readline" ,readline)))
   (arguments
    '(#:make-flags '("gp")
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
       %standard-phases)))
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

(define-public bc
  (package
    (name "bc")
    (version "1.06")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/bc/bc-" version ".tar.gz"))
             (sha256
              (base32
               "0cqf5jkwx6awgd2xc2a0mkpxilzcfmhncdcfg7c9439wgkqxkxjf"))))
    (build-system gnu-build-system)
    (inputs `(("readline" ,readline)
              ("flex" ,flex)))
    (arguments
     '(#:phases
       (alist-replace 'configure
                      (lambda* (#:key outputs #:allow-other-keys)
                        ;; This old `configure' script doesn't support
                        ;; variables passed as arguments.
                        (let ((out (assoc-ref outputs "out")))
                          (setenv "CONFIG_SHELL" (which "bash"))
                          (zero?
                           (system* "./configure"
                                    (string-append "--prefix=" out)))))
                      %standard-phases)))
    (home-page "http://www.gnu.org/software/bc/")
    (synopsis "Arbitrary precision numeric processing language")
    (description
     "bc is an arbitrary precision numeric processing language. Syntax
is similar to C, but differs in many substantial areas. It supports
interactive execution of statements.  bc is a utility included in the
POSIX P1003.2/D11 draft standard.

Since the POSIX document does not specify how bc must be implemented,
this version does not use the historical method of having bc be a
compiler for the dc calculator.  This version has a single executable
that both compiles the language and runs the resulting `byte code'. The
byte code is not the dc language.")
    (license gpl2+)))

(define-public fftw
  (package
    (name "fftw")
    (version "3.3.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "ftp://ftp.fftw.org/pub/fftw/fftw-"
                                 version".tar.gz"))
             (sha256
              (base32
               "1wwp9b2va7vkq3ay7a9jk22nr4x5q6m37rzqy2j8y3d11c5grkc5"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--enable-shared" "--enable-openmp")
       #:phases (alist-cons-before
                 'build 'no-native
                 (lambda _
                   ;; By default '-mtune=native' is used.  However, that may
                   ;; cause the use of ISA extensions (SSE2, etc.) that are
                   ;; not necessarily available on the user's machine when
                   ;; that package is built on a different machine.
                   (substitute* (find-files "." "Makefile$")
                     (("-mtune=native") "")))
                 %standard-phases)))
    (native-inputs `(("perl" ,perl)))
    (home-page "http://fftw.org")
    (synopsis "Computing the discrete Fourier transform")
    (description
     "FFTW is a C subroutine library for computing the discrete Fourier
transform (DFT) in one or more dimensions, of arbitrary input size, and of
both real and complex data (as well as of even/odd data---i.e. the discrete
cosine/ sine transforms or DCT/DST).")
    (license gpl2+)))
