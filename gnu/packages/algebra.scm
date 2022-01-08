;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013, 2015, 2017, 2018, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016–2022 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2014, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2018, 2019, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017, 2019, 2021 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020, 2021 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 Lars-Dominik Braun <ldb@leibniz-psychology.org>
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
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages ed)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fltk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages java)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (guix build-system r)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))


(define-public mpfrcx
  (package
   (name "mpfrcx")
   (version "0.6.3")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "http://www.multiprecision.org/downloads/mpfrcx-"
                  version ".tar.gz"))
            (sha256
             (base32
              "1545vgizpypqi2rrriad0ybqv0qwbn9zr0ibxpk00gha9ihv7acx"))))
   (build-system gnu-build-system)
   (propagated-inputs
     (list gmp mpfr mpc)) ; Header files are included by mpfrcx.h.
   (synopsis "Arithmetic of polynomials over arbitrary precision numbers")
   (description
    "Mpfrcx is a library for the arithmetic of univariate polynomials over
arbitrary precision real (mpfr) or complex (mpc) numbers, without control
on the rounding.  For the time being, only the few functions needed to
implement the floating point approach to complex multiplication are
implemented.  On the other hand, these comprise asymptotically fast
multiplication routines such as Toom–Cook and the FFT.")
   (license license:lgpl3+)
   (home-page "http://www.multiprecision.org/mpfrcx/")))

(define-public gf2x
  (package
   (name "gf2x")
   (version "1.2")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://gforge.inria.fr/frs/download.php/file/36934/gf2x-"
                  version ".tar.gz"))
            (sha256
             (base32
              "0d6vh1mxskvv3bxl6byp7gxxw3zzpkldrxnyajhnl05m0gx7yhk1"))))
   (build-system gnu-build-system)
   (synopsis "Arithmetic of polynomials over binary finite fields")
   (description
    "The gf2x library provides arithmetic of polynomials over finite fields
of characteristic 2.  It implements the multiplication, squaring and
greatest common divisor operations.")
   (license license:gpl3+)
   (home-page "https://gforge.inria.fr/projects/gf2x/")))

(define-public cm
  (package
   (name "cm")
   (version "0.3.1")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "http://www.multiprecision.org/downloads/cm-"
                  version ".tar.gz"))
            (sha256
             (base32
              "0qq6b1kwb1byj8ws33ya5awq0ilkpm32037pi1l4cf2737fg9m42"))))
   (build-system gnu-build-system)
   (propagated-inputs
     (list mpfrcx zlib)) ; Header files included from cm_common.h.
   (inputs
     (list pari-gp))
   (synopsis "CM constructions for elliptic curves")
   (description
    "The CM software implements the construction of ring class fields of
imaginary quadratic number fields and of elliptic curves with complex
multiplication via floating point approximations.  It consists of libraries
that can be called from within a C program and of executable command
line applications.")
   (license license:gpl3+)
   (home-page "http://www.multiprecision.org/cm/")))

(define-public fplll
  (package
    (name "fplll")
    (version "5.3.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/fplll/fplll")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "06nyfidagp8pc2kfcw88ldgb2b1xm0a8z31n0sln7j72ihlmd8zj"))
              (patches (search-patches "fplll-std-fenv.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (propagated-inputs ; header files pulled in by fplll/defs.h
     (list gmp mpfr))
    (home-page "https://github.com/fplll/fplll")
    (synopsis "Library for LLL-reduction of euclidean lattices")
    (description
     "fplll contains implementations of several lattice algorithms.
The implementation relies on floating-point orthogonalization, and LLL
is central to the code, hence the name.

It includes implementations of floating-point LLL reduction
algorithms, offering different speed/guarantees ratios.  It contains
a @emph{wrapper} choosing the estimated best sequence of variants in
order to provide a guaranteed output as fast as possible.  In the case
of the wrapper, the succession of variants is oblivious to the user.

It includes an implementation of the BKZ reduction algorithm,
including the BKZ-2.0 improvements (extreme enumeration
pruning, pre-processing of blocks, early termination).  Additionally,
Slide reduction and self dual BKZ are supported.

It also includes a floating-point implementation of the
Kannan-Fincke-Pohst algorithm that finds a shortest non-zero lattice
vector.  For the same task, the GaussSieve algorithm is also available
in fplll.  Finally, it contains a variant of the enumeration algorithm
that computes a lattice vector closest to a given vector belonging to
the real span of the lattice.")
    (license license:lgpl2.1+)))

(define-public python-fpylll
  (package
    (name "python-fpylll")
    (version "0.5.2")
    (source
     (origin
       ;; Pypi contains and older release, so we use a tagged release from
       ;; Github instead.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fplll/fpylll")
             (commit (string-append version "dev"))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1a25iibihph626jl4wbs4b77xc4a2c4nfc2ypscf9wpani3dnhjf"))))
    (build-system python-build-system)
    (inputs
     (list fplll gmp mpfr pari-gp))
    (propagated-inputs
     (list python-cysignals python-cython python-flake8 python-numpy
           python-pytest))
    (home-page "https://github.com/fplll/fpylll")
    (synopsis "Python interface for fplll")
    (description "fpylll is a Python wrapper for fplll.")
    (license license:gpl2+)))

(define-public pari-gp
  (package
    (name "pari-gp")
    (version "2.13.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://pari.math.u-bordeaux.fr/pub/pari/unix/pari-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1jm1cz1687cz8pl8lgvmyk3l33cms1xbayv38ca4z1f60qb7zfnc"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("texlive" ,(texlive-updmap.cfg
                    (list texlive-amsfonts)))))
    (inputs (list gmp libx11 perl readline))
    (arguments
     '(#:make-flags '("all")
       #:test-target "dobench"
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "./Configure"
                     (string-append "--prefix="
                                    (assoc-ref outputs "out"))))))))
    (synopsis "PARI/GP, a computer algebra system for number theory")
    (description
     "PARI/GP is a widely used computer algebra system designed for fast
computations in number theory (factorisations, algebraic number theory,
elliptic curves...), but it also contains a large number of other useful
functions to compute with mathematical entities such as matrices,
polynomials, power series, algebraic numbers, etc., and a lot of
transcendental functions.
PARI is also available as a C library to allow for faster computations.")
    (license license:gpl2+)
    (home-page "https://pari.math.u-bordeaux.fr/")))

(define-public gp2c
  (package
   (name "gp2c")
   (version "0.0.12")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://pari.math.u-bordeaux.fr/pub/pari/GP2C/gp2c-"
                  version ".tar.gz"))
            (sha256
              (base32
                "039ip7qkwwv46wrcdrz7y12m30kazzkjr44kqbc0h137g4wzd7zf"))))
   (build-system gnu-build-system)
   (native-inputs (list perl))
   (inputs (list pari-gp))
   (arguments
    '(#:configure-flags
      (list (string-append "--with-paricfg="
                           (assoc-ref %build-inputs "pari-gp")
                           "/lib/pari/pari.cfg"))))
   (synopsis "PARI/GP, a computer algebra system for number theory")
   (description
    "PARI/GP is a widely used computer algebra system designed for fast
computations in number theory (factorisations, algebraic number theory,
elliptic curves...), but it also contains a large number of other useful
functions to compute with mathematical entities such as matrices,
polynomials, power series, algebraic numbers, etc., and a lot of
transcendental functions.
PARI is also available as a C library to allow for faster computations.

GP2C, the GP to C compiler, translates GP scripts to PARI programs.")
   (license license:gpl2)
   (home-page "https://pari.math.u-bordeaux.fr/")))

(define-public cmh
  (package
   (name "cmh")
   (version "1.1.0")
   (source (origin
            (method url-fetch)
            ;; Git repo at <https://gitlab.inria.fr/cmh/cmh>.
            (uri (string-append "http://www.multiprecision.org/downloads/cmh-"
                                version ".tar.gz"))
            (sha256
             (base32
              "1ws2yhzxmm2l5xqqqcjcimmg40f9qq5l9i6d4i5434an9v9s8531"))))
   (build-system gnu-build-system)
   (inputs
     (list gmp
           mpfr
           mpc
           mpfrcx
           fplll
           pari-gp))
   (synopsis "Igusa class polynomial computations")
   (description
    "The CMH software computes Igusa (genus 2) class polynomials, which
parameterize the CM points in the moduli space of 2-dimensional abelian
varieties, i.e. Jacobians of hyperelliptic curves.
It can also be used to compute theta constants at arbitrary
precision.")
   (license license:gpl3+)
   (home-page "http://www.multiprecision.org/cmh/home.html")))

(define-public giac
  (package
    (name "giac")
    (version "1.7.0-45")
    (source
     (origin
       (method url-fetch)
       ;; "~parisse/giac" is not used because the maintainer regularly
       ;; overwrites the release tarball there, introducing a checksum
       ;; mismatch every time.  See
       ;; <https://www-fourier.ujf-grenoble.fr/~parisse/debian/dists/stable/main/source/README>
       (uri (string-append "https://www-fourier.ujf-grenoble.fr/"
                           "~parisse/debian/dists/stable/main/source/"
                           "giac_" version ".tar.gz"))
       (sha256
        (base32 "19hxbx27n5zby96d4pzhxxqn7mzk29g8sxn08fi638l17lr9x2q2"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((ice-9 ftw)
                  (guix build utils)
                  (guix build gnu-build-system))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-bin-cp
           ;; Some Makefiles contain hard-coded "/bin/cp".
           (lambda _
             (substitute* (cons "micropython-1.12/xcas/Makefile"
                                (find-files "doc" "^Makefile"))
               (("/bin/cp") (which "cp")))))
         (add-after 'unpack 'disable-failing-test
           ;; FIXME: Test failing.  Not sure why.
           (lambda _
             (substitute* "check/Makefile.in"
               (("chk_fhan11") ""))))
         (add-after 'install 'fix-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; Most French documentation has a non-commercial
               ;; license, so we need to remove it.
               (with-directory-excursion (string-append out "/share/giac/doc/fr")
                 (for-each delete-file-recursively
                           '("cascas" "casexo" "casgeo" "casrouge" "cassim"
                             "castor")))
               ;; Remove duplicate documentation in
               ;; "%out/share/doc/giac/", where Xcas does not expect
               ;; to find it.
               (delete-file-recursively (string-append out "/share/doc/giac")))))
         (add-after 'install 'remove-unnecessary-executable
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (delete-file (string-append out "/bin/xcasnew"))))))))
    (inputs
     ;; TODO: Add libnauty, unbundle "libmicropython.a".
     (list ao
           fltk
           glpk-4
           gmp
           gsl
           lapack
           libjpeg-turbo
           libpng
           libsamplerate
           libx11
           libxft
           libxinerama
           libxt
           mesa
           mpfi
           mpfr
           ntl
           pari-gp
           perl
           tcsh))
    (native-inputs
     (list bison
           flex
           hevea
           python-wrapper
           readline
           texlive-tiny))
    (home-page "https://www-fourier.ujf-grenoble.fr/~parisse/giac.html")
    (synopsis "Computer algebra system")
    (description
     "Giac/Xcas is a computer algebra system.  It has a compatibility mode for
maple, mupad and the TI89.  It is available as a standalone program (graphic
or text interfaces) or as a C++ library.")
    (license license:gpl3+)))

(define-public flint
  (package
   (name "flint")
   (version "2.8.4")
   (source
    (origin
      (method url-fetch)
      (uri (string-append "http://flintlib.org/flint-" version ".tar.gz"))
      (sha256
       (base32 "1gp4wm2s8c27g2hh53d09cys62da1bsxfwbcsj9cd7cfikm95pv1"))))
   (build-system gnu-build-system)
   (inputs
    (list ntl))
   (propagated-inputs
    (list gmp mpfr)) ; header files from both are included by flint/arith.h
   (arguments
    `(#:parallel-tests? #f              ; seems to be necessary on arm
      #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'newer-c++
           (lambda _
             (substitute* "configure"
               (("-ansi") ""))
             #t))
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (gmp (assoc-ref inputs "gmp"))
                   (mpfr (assoc-ref inputs "mpfr"))
                   (ntl (assoc-ref inputs "ntl")))
               ;; Do not pass "--enable-fast-install", which makes the
               ;; homebrew configure process fail.
               (invoke "./configure"
                       (string-append "--prefix=" out)
                       (string-append "--with-gmp=" gmp)
                       (string-append "--with-mpfr=" mpfr)
                       (string-append "--with-ntl=" ntl))
               #t))))))
   (synopsis "Fast library for number theory")
   (description
    "FLINT is a C library for number theory.  It supports arithmetic
with numbers, polynomials, power series and matrices over many base
rings, including multiprecision integers and rationals, integers
modulo n, p-adic numbers, finite fields (prime and non-prime order)
and real and complex numbers (via the Arb extension library).

Operations that can be performed include conversions, arithmetic,
GCDs, factoring, solving linear systems, and evaluating special
functions.  In addition, FLINT provides various low-level routines for
fast arithmetic.")
   (license license:lgpl2.1+)
   (home-page "http://flintlib.org/")))

(define-public arb
  (package
    (name "arb")
    (version "2.21.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/fredrik-johansson/arb")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1lmcl122qy6mr1g1y65bm7dk9fj0sym7gzmvar5vdgk7ln03c5iq"))))
    (build-system gnu-build-system)
    (propagated-inputs
     (list flint))               ; flint.h is included by arf.h
    (inputs
     (list gmp mpfr))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (flint (assoc-ref inputs "flint"))
                   (gmp (assoc-ref inputs "gmp"))
                   (mpfr (assoc-ref inputs "mpfr")))
               ;; Do not pass "--enable-fast-install", which makes the
               ;; homebrew configure process fail.
               (invoke "./configure"
                       (string-append "--prefix=" out)
                       (string-append "--with-flint=" flint)
                       (string-append "--with-gmp=" gmp)
                       (string-append "--with-mpfr=" mpfr))))))))
    (home-page "https://arblib.org")
    (synopsis "Arbitrary precision floating-point ball arithmetic")
    (description
     "Arb is a C library for arbitrary-precision floating-point ball
arithmetic.  It supports efficient high-precision computation with
polynomials, power series, matrices and special functions over the
real and complex numbers, with automatic, rigorous error control.")
    (license license:lgpl2.1+)))

(define-public python-flint
  (package
    (name "python-flint")
    (version "0.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/fredrik-johansson/python-flint")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1v0anazbj1cfi68nl2j6dbd31kgkc1563xmr0zk5xk3xj78569pw"))
              (patches (search-patches "python-flint-includes.patch"))))
    (build-system python-build-system)
    (native-inputs
     (list python-cython))
    (propagated-inputs
     (list python-numpy))
    (inputs
     (list arb flint))
    (synopsis "Python module wrapping ARB and FLINT")
    (description
     "Python-flint is a Python extension module wrapping FLINT
(Fast Library for Number Theory) and Arb (arbitrary-precision ball
arithmetic).  It supports integers, rationals, modular integers,
real and complex ball arithmetic, polynomials and matrices over all
these types and other mathematical functions.")
    (license license:expat)
    (home-page "https://fredrikj.net/python-flint/")))

(define-public ntl
  (package
   (name "ntl")
   (version "11.4.4")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://shoup.net/ntl/ntl-"
                                version ".tar.gz"))
            (sha256
             (base32
              "1nr1h27j2gdz6badzz9lk2pknxhdijqdxqhd3haryh0sw616wzwx"))
            (modules '((guix build utils)))
            (snippet
             '(begin
                (delete-file-recursively "src/libtool-origin")
                #t))))
   (build-system gnu-build-system)
   (native-inputs
    (list libtool perl)) ; for configuration
   (inputs
    (list gmp gf2x))
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (replace 'configure
         (lambda* (#:key inputs outputs #:allow-other-keys)
           (chdir "src")
           (invoke "./configure"
                    (string-append "PREFIX=" (assoc-ref outputs "out"))
                    (string-append "LIBTOOL=" (assoc-ref inputs "libtool") "/bin/libtool")
                    ;; set the library prefixes explicitly so that they get
                    ;; embedded in the .la file
                    (string-append "GMP_PREFIX=" (assoc-ref inputs "gmp"))
                    (string-append "GF2X_PREFIX=" (assoc-ref inputs "gf2x"))
                    ;; Do not build especially for the build machine.
                    "NATIVE=off"
                    "NTL_GF2X_LIB=on"
                    "SHARED=on"))))))
   (synopsis "C++ library for number theory")
   (description
    "NTL is a C++ library providing data structures and algorithms
for manipulating signed, arbitrary length integers, and for vectors,
matrices, and polynomials over the integers and over finite fields.")
   (license license:gpl2+)
   (home-page "https://shoup.net/ntl/")))

(define-public singular
  (package
   (name "singular")
   (version "4.2.1")
   (source
    (origin
      (method url-fetch)
      (uri
       (string-append "http://www.mathematik.uni-kl.de/ftp/pub/Math/"
                      "Singular/SOURCES/"
                      (string-join
                       (string-split
                        (let ((index (string-index version #\p)))
                          (if index (string-take version index)
                                    version))
                        #\.) "-")
                      "/singular-" version ".tar.gz"))
             (sha256 (base32
                      "13gy1gdng8zijwlr1fn5sixw53z0zf9czzlg0vh1dcc59zw6v998"))))
   (build-system gnu-build-system)
   (native-inputs
    (list doxygen graphviz perl))
   (inputs
    `(("cddlib" ,cddlib)
      ("gmp" ,gmp)
      ("flint" ,flint)
      ("mpfr" ,mpfr)
      ("ntl" ,ntl)
      ("python" ,python-2)
      ("readline" ,readline)))
   (arguments
    `(#:configure-flags
      (list (string-append "--with-ntl="
                           (assoc-ref %build-inputs "ntl")))))
   (synopsis "Computer algebra system for polynomial computations")
   (description
    "Singular is a computer algebra system for polynomial computations,
with special emphasis on commutative and non-commutative algebra, algebraic
geometry and singularity theory.")
   ;; Singular itself is dual licensed gpl2 or gpl3, but some of the
   ;; libraries with which it links are licensed under lgpl3+, so the
   ;; combined work becomes gpl3. See COPYING in the source code.
   (license license:gpl3)
   (home-page "https://www.singular.uni-kl.de/index.php")))

(define-public gmp-ecm
  (package
   (name "gmp-ecm")
   (version "7.0.4")
   (source (origin
             (method url-fetch)
             (uri
               (let ((hash "00c4c691a1ef8605b65bdf794a71539d"))
                    (string-append "https://gitlab.inria.fr/zimmerma/ecm/"
                                   "uploads/" hash "/ecm-" version
                                   ".tar.gz")))
             (sha256 (base32
                      "0hxs24c2m3mh0nq1zz63z3sb7dhy1rilg2s1igwwcb26x3pb7xqc"))))
   (build-system gnu-build-system)
   (inputs
    (list gmp))
   (arguments
    `(#:configure-flags '("--enable-shared"
                          ;; Disable specific assembly routines, which depend
                          ;; on the subarchitecture of the build machine,
                          ;; and use gmp instead.
                          "--disable-asm-redc")))
   (synopsis "Integer factorization library using the elliptic curve method")
   (description
    "GMP-ECM factors integers using the elliptic curve method (ECM) as well
as the P-1 and P+1 algorithms.  It provides a library and a stand-alone
binary.")
   ;; Most files are under lgpl3+, but some are under gpl3+ or gpl2+,
   ;; so the combined work is under gpl3+.
   (license license:gpl3+)
   (home-page "http://ecm.gforge.inria.fr/")))

(define-public bc
  (package
    (name "bc")
    (version "1.07.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/bc/bc-" version ".tar.gz"))
             (sha256
              (base32
               "0amh9ik44jfg66csyvf4zz1l878c4755kjndq9j0270akflgrbb2"))
             (patches (search-patches "bc-fix-cross-compilation.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     (list automake autoconf ed flex texinfo))
    (inputs
     (list readline))
    (arguments
     '(#:configure-flags
       (list "--with-readline")
       #:phases
       (modify-phases %standard-phases
         (replace 'bootstrap
           (lambda _
             (invoke "autoreconf" "-vif"))))))
    (home-page "https://www.gnu.org/software/bc/")
    (synopsis "Arbitrary precision numeric processing language")
    (description
     "bc is an arbitrary precision numeric processing language.  It includes
an interactive environment for evaluating mathematical statements.  Its
syntax is similar to that of C, so basic usage is familiar.  It also includes
\"dc\", a reverse-polish calculator.")
    (license license:gpl2+)))

;; The original kiss-fft does not have a complete build system and does not
;; build any shared libraries.  This is a fork used by Extempore.
(define-public kiss-fft-for-extempore
  (package
    (name "kiss-fft-for-extempore")
    (version "1.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/extemporelang/kiss_fft")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0jasbmqy4wkqrqx3w64s1dfmj34875xmsl72mb26aa4hpyn14bi2"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f)) ; no tests included
    ;; Extempore refuses to build on architectures other than x86_64
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/extemporelang/kiss_fft")
    (synopsis "Mixed-radix Fast Fourier Transform")
    (description
     "Kiss FFT attempts to be a reasonably efficient, moderately useful FFT
that can use fixed or floating data types and can easily be incorporated into
a C program.")
    (license license:bsd-3)))

(define-public fftw
  (package
    (name "fftw")
    (version "3.3.8")
    (source (origin
             (method url-fetch)
             (uri (string-append "ftp://ftp.fftw.org/pub/fftw/fftw-"
                                 version".tar.gz"))
             (sha256
              (base32
               "00z3k8fq561wq2khssqg0kallk0504dzlx989x3vvicjdqpjc4v1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       '("--enable-shared" "--enable-openmp" "--enable-threads"
         ,@(let ((system (or (%current-target-system) (%current-system))))
             ;; Enable SIMD extensions for codelets.  See details at:
             ;; <http://fftw.org/fftw3_doc/Installation-on-Unix.html>.
             (cond
              ((string-prefix? "x86_64" system)
               '("--enable-sse2" "--enable-avx" "--enable-avx2"
                 "--enable-avx512" "--enable-avx-128-fma"))
              ((string-prefix? "i686" system)
               '("--enable-sse2"))
              ((string-prefix? "aarch64" system)
               ;; Note that fftw supports NEON on 32-bit ARM only when
               ;; compiled for single-precision.
               '("--enable-neon"))
              (else
               '())))
         ;; By default '-mtune=native' is used.  However, that may cause the
         ;; use of ISA extensions (e.g. AVX) that are not necessarily
         ;; available on the user's machine when that package is built on a
         ;; different machine.
         "ax_cv_c_flags__mtune_native=no")))
    (native-inputs (list perl))
    (home-page "http://fftw.org")
    (synopsis "Computing the discrete Fourier transform")
    (description
     "FFTW is a C subroutine library for computing the discrete Fourier
transform (DFT) in one or more dimensions, of arbitrary input size, and of
both real and complex data (as well as of even/odd data---i.e. the discrete
cosine/ sine transforms or DCT/DST).")
    (license license:gpl2+)))

(define-public fftwf
  (package/inherit fftw
    (name "fftwf")
    (arguments
     (substitute-keyword-arguments (package-arguments fftw)
       ((#:configure-flags fftw-configure-flags)
        `(cons* "--enable-single"
                ,@(if (string-prefix? "arm" (or (%current-target-system)
                                                (%current-system)))
                      ;; fftw supports NEON on 32-bit ARM only when compiled
                      ;; for single-precision, so add it here.
                      '("--enable-neon")
                      '())
                ,fftw-configure-flags))))
    (description
     (string-append (package-description fftw)
                    "  Single-precision version."))))

(define-public fftw-openmpi
  (package/inherit fftw
    (name "fftw-openmpi")
    (inputs
     `(("openmpi" ,openmpi)
       ,@(package-inputs fftw)))
    (arguments
     (substitute-keyword-arguments (package-arguments fftw)
       ((#:configure-flags cf)
        `(cons "--enable-mpi" ,cf))
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (add-before 'check 'mpi-setup
             ,%openmpi-setup)))))
    (description
     (string-append (package-description fftw)
                    "  With OpenMPI parallelism support."))))

(define-public java-la4j
  (package
    (name "java-la4j")
    (version "0.6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/vkostyukov/la4j")
                    (commit version)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1qir8dr978cfvz9k12m2kbdwpyf6cqdf1d0ilb7lnkhbgq5i53w3"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "la4j.jar"
       #:jdk ,icedtea-8
       #:test-exclude (list "**/Abstract*.java"
                            "**/MatrixTest.java"
                            "**/DenseMatrixTest.java"
                            "**/SparseMatrixTest.java"
                            "**/VectorTest.java"
                            "**/SparseVectorTest.java"
                            "**/DenseVectorTest.java")))
    (native-inputs
     (list java-junit java-hamcrest-core))
    (home-page "http://la4j.org/")
    (synopsis "Java library that provides Linear Algebra primitives and algorithms")
    (description "The la4j library is a Java library that provides Linear
Algebra primitives (matrices and vectors) and algorithms.  The key features of
the la4j library are:

@itemize
@item No dependencies and tiny size
@item Fluent object-oriented/functional API
@item Sparse (CRS, CCS) and dense (1D/2D arrays) matrices
@item Linear systems solving (Gaussian, Jacobi, Zeidel, Square Root, Sweep and other)
@item Matrices decomposition (Eigenvalues/Eigenvectors, SVD, QR, LU, Cholesky and other)
@item MatrixMarket/CSV IO formats support for matrices and vectors
@end itemize\n")
    (license license:asl2.0)))

(define-public java-jlargearrays
  (package
    (name "java-jlargearrays")
    (version "1.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/"
                                  "pl/edu/icm/JLargeArrays/"
                                  version "/JLargeArrays-" version
                                  "-sources.jar"))
              (file-name (string-append name "-" version ".jar"))
              (sha256
               (base32
                "0v05iphpxbjnd7f4jf1rlqq3m8hslhcm0imdbsgxr20pi3xkaf2a"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jlargearrays.jar"
       #:tests? #f ; tests are not included in the release archive
       #:jdk ,icedtea-8))
    (propagated-inputs
     (list java-commons-math3))
    (home-page "https://gitlab.com/ICM-VisLab/JLargeArrays")
    (synopsis "Library of one-dimensional arrays that can store up to 263 elements")
    (description "JLargeArrays is a Java library of one-dimensional arrays
that can store up to 263 elements.")
    (license license:bsd-2)))

(define-public java-jtransforms
  (package
    (name "java-jtransforms")
    (version "3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/"
                                  "com/github/wendykierp/JTransforms/"
                                  version "/JTransforms-" version "-sources.jar"))
              (sha256
               (base32
                "1haw5m8shv5srgcpwkl853dz8bv6h90bzlhcps6mdpb4cixjirsg"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jtransforms.jar"
       #:tests? #f ; tests are not included in the release archive
       #:jdk ,icedtea-8))
    (propagated-inputs
     (list java-commons-math3 java-jlargearrays))
    (home-page "https://github.com/wendykierp/JTransforms")
    (synopsis "Multithreaded FFT library written in pure Java")
    (description "JTransforms is a multithreaded FFT library written in pure
Java.  Currently, four types of transforms are available: @dfn{Discrete
Fourier Transform} (DFT), @dfn{Discrete Cosine Transform} (DCT), @dfn{Discrete
Sine Transform} (DST) and @dfn{Discrete Hartley Transform} (DHT).")
    (license license:bsd-2)))

(define-public lmfit
  (package
    (name "lmfit")
    (version "8.2.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://jugit.fz-juelich.de/mlz/lmfit.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00bch77a6qgnw6vzsjn2a42n8n683ih3xm0wpr454jxa15hw78vf"))))
    (build-system cmake-build-system)
    (native-inputs
     (list perl))                   ; for pod2man
    (home-page "https://jugit.fz-juelich.de/mlz/lmfit")
    (synopsis "Levenberg-Marquardt minimization and least-squares fitting")
    (description "lmfit is a C library for Levenberg-Marquardt least-squares
minimization and curve fitting.  It is mature code, based on decades-old
algorithms from the FORTRAN library MINPACK.")
    (license license:bsd-2)))

(define-public symengine
  (package
    (name "symengine")
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/symengine/symengine")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "129iv9maabmb42ylfdv0l0g94mcbf3y4q3np175008rcqdr8z6h1"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       ;; These are the suggested build options in the README.
       '("-DCMAKE_BUILD_TYPE=Release"
         "-DWITH_GMP=on"
         "-DWITH_MPFR=on"
         "-DWITH_MPC=on"
         "-DINTEGER_CLASS=flint"
         "-DWITH_SYMENGINE_THREAD_SAFE=on"
         "-DBUILD_SHARED_LIBS=on")))    ;also build libsymengine
    (inputs
     (list flint gmp mpc mpfr))
    (home-page "https://github.com/symengine/symengine")
    (synopsis "Fast symbolic manipulation library")
    (description
     "SymEngine is a standalone fast C++ symbolic manipulation library.
Optional thin wrappers allow usage of the library from other languages.")
    (license (list license:expat        ;SymEngine
                   license:bsd-3))))    ;3rd party code

(define-public ginac
  (package
    (name "ginac")
    (version "1.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.ginac.de/ginac-"
                           version ".tar.bz2"))
       (sha256
        (base32 "1az1ypfcny4jdz0mic1kywwa9nynr547cl5s7zpn2w0qdfymssgi"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list "--disable-static")))
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper))) ; Python is required
    (inputs
     (list cln readline))
    (home-page "https://www.ginac.de/")
    (synopsis "Library for symbolic computation")
    (description "GiNaC is a C++ library for symbolic computation.  Contrary
to other CAS it does not try to provide extensive algebraic capabilities and a
simple programming language but instead accepts a given language (C++) and
extends it by a set of algebraic capabilities.")
    (license license:gpl2+)))

(define-public eigen
  (package
    (name "eigen")
    (version "3.3.8")
    (source (origin
              (method url-fetch)
              (uri (list
                     (string-append "https://bitbucket.org/eigen/eigen/get/"
                                    version ".tar.bz2")
                     (string-append "mirror://debian/pool/main/e/eigen3/eigen3_"
                                    version ".orig.tar.bz2")))
              (sha256
               (base32
                "1vxrsncfnkyq6gwxpsannpryp12mk7lc8f42ybvz3saf7icwc582"))
              (file-name (string-append name "-" version ".tar.bz2"))
              (patches (search-patches "eigen-remove-openmp-error-counting.patch"
                                       "eigen-stabilise-sparseqr-test.patch"))
              (modules '((guix build utils)))
              (snippet
               ;; There are 3 test failures in the "unsupported" directory,
               ;; but maintainers say it's a known issue and it's unsupported
               ;; anyway, so just skip them.
               '(begin
                  (substitute* "unsupported/CMakeLists.txt"
                    (("add_subdirectory\\(test.*")
                     "# Do not build the tests for unsupported features.\n"))
                  #t))))
    (build-system cmake-build-system)
    (arguments
     '(;; Turn off debugging symbols to save space.
       #:build-type "Release"

       #:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda _
                      (let* ((cores  (parallel-job-count))
                             (dash-j (format #f "-j~a" cores)))
			(setenv "EIGEN_SEED" "1") ;for reproducibility
                        ;; First build the tests, in parallel.  See
                        ;; <http://eigen.tuxfamily.org/index.php?title=Tests>.
                        (invoke "make" "buildtests" dash-j)

                        ;; Then run 'CTest' with -V so we get more
                        ;; details upon failure.
                        (invoke "ctest" "-V" dash-j)))))))
    (home-page "https://eigen.tuxfamily.org")
    (synopsis "C++ template library for linear algebra")
    (description
     "Eigen is a C++ template library for linear algebra: matrices, vectors,
numerical solvers, and related algorithms.  It provides an elegant API based
on \"expression templates\".  It is versatile: it supports all matrix sizes,
all standard numeric types, various matrix decompositions and geometry
features, and more.")

    ;; Most of the code is MPLv2, with a few files under LGPLv2.1+ or BSD-3.
    ;; See 'COPYING.README' for details.
    (license license:mpl2.0)))

(define-public eigen-benchmarks
  (package
    (inherit eigen)
    (name "eigen-benchmarks")
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (replace 'build
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (bin (string-append out "/bin")))
                        (define (compile file)
                          (format #t "compiling '~a'...~%" file)
                          (let ((target
                                 (string-append bin "/"
                                                (basename file ".cpp"))))
                            (invoke "c++" "-o" target file
                                    "-I" ".." "-O2" "-g"
                                    "-lopenblas" "-Wl,--as-needed")))

                        (mkdir-p bin)
                        (with-directory-excursion "bench"
                          ;; There are more benchmarks, of varying quality.
                          ;; Here we pick some that appear to be useful.
                          (for-each compile
                                    '("benchBlasGemm.cpp"
                                      "benchCholesky.cpp"
                                      ;;"benchEigenSolver.cpp"
                                      "benchFFT.cpp"
                                      "benchmark-blocking-sizes.cpp"))))))
                  (delete 'install))))
    (inputs (list boost openblas))

    ;; Mark as tunable to take advantage of SIMD code in Eigen.
    (properties '((tunable? . #t)))

    (synopsis "Micro-benchmarks of the Eigen linear algebra library")))

(define-public eigen-for-tensorflow
  (let ((changeset "fd6845384b86")
        (revision "1"))
    (package (inherit eigen)
      (name "eigen-for-tensorflow")
      (version (string-append "3.3.5-" revision "." changeset))
      (source (origin
                (method hg-fetch)
                (uri (hg-reference
                      (url "https://bitbucket.org/eigen/eigen")
                      (changeset changeset)))
                (sha256
                 (base32
                  "12cwgah63wqwb66xji048hcxc1z5zjg8a7701zlia5zbilnnk1n5"))
                (file-name (string-append name "-" version "-checkout"))
                (modules '((guix build utils)))
                (snippet
                 ;; There are 3 test failures in the "unsupported" directory,
                 ;; but maintainers say it's a known issue and it's unsupported
                 ;; anyway, so just skip them.
                 '(begin
                    (substitute* "unsupported/CMakeLists.txt"
                      (("add_subdirectory\\(test.*")
                       "# Do not build the tests for unsupported features.\n"))))))
      (native-inputs
       (list gcc-7)))))

(define-public eigen-for-tensorflow-lite
  ;; This commit was taken from
  ;; tensorflow/lite/tools/cmake/modules/eigen.cmake
  (let ((commit "d10b27fe37736d2944630ecd7557cefa95cf87c9")
        (revision "1"))
    (package (inherit eigen)
      (name "eigen-for-tensorflow-lite")
      (version (git-version "3.3.7" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.com/libeigen/eigen")
                      (commit commit)))
                (sha256
                 (base32
                  "0v8a20cwvwmp3hw4275b37frw33v92z0mr8f4dn6y8k0rz92hrrf"))
                (file-name (git-file-name name version))
                (modules '((guix build utils)))
                (snippet
                 ;; Ther are test failures in the "unsupported" directory, but
                 ;; maintainers say it's unsupported anyway, so just skip
                 ;; them.
                 '(begin
                    (substitute* "unsupported/CMakeLists.txt"
                      (("add_subdirectory\\(test.*")
                       "# Do not build the tests for unsupported features.\n")))))))))

(define-public xtensor
  (package
    (name "xtensor")
    (version "0.20.10")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/xtensor-stack/xtensor")
                    (commit version)))
              (sha256
               (base32
                "1fmv2hpx610xwhxrndfsfvlbqfyk4l3gi5q5d7pa9m82kblxjj9l"))
              (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (native-inputs
     (list googletest xtl))
    (arguments
     `(#:configure-flags
       '("-DBUILD_TESTS=ON")
       #:test-target "xtest"))
    (home-page "https://xtensor.readthedocs.io/en/latest/")
    (synopsis "C++ tensors with broadcasting and lazy computing")
    (description "xtensor is a C++ library meant for numerical analysis with
multi-dimensional array expressions.

xtensor provides:
@itemize
@item an extensible expression system enabling lazy broadcasting.
@item an API following the idioms of the C++ standard library.
@item tools to manipulate array expressions and build upon xtensor.
@end itemize")
    (license license:bsd-3)))

(define-public xtensor-benchmark
  (package
    (inherit xtensor)
    (name "xtensor-benchmark")
    (arguments
     `(#:configure-flags (list "-DBUILD_BENCHMARK=ON"
                               "-DDOWNLOAD_GBENCHMARK=OFF")
       #:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'remove-march=native
                    (lambda _
                      (substitute* "benchmark/CMakeLists.txt"
                        (("-march=native") ""))))
                  (add-after 'unpack 'link-with-googlebenchmark
                    (lambda _
                      (substitute* "benchmark/CMakeLists.txt"
                        (("find_package\\(benchmark.*" all)
                         (string-append
                          all "\n"
                          "set(GBENCHMARK_LIBRARIES benchmark)\n")))))
                  (replace 'build
                    (lambda _
                      (invoke "make" "benchmark_xtensor" "-j"
                              (number->string (parallel-job-count)))))
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; Install nothing but the executable.
                      (let ((out (assoc-ref outputs "out")))
                        (install-file "benchmark/benchmark_xtensor"
                                      (string-append out "/bin"))))))))
    (synopsis "Benchmarks of the xtensor library")
    (native-inputs '())
    (inputs
     (modify-inputs (package-native-inputs xtensor)
       (prepend googlebenchmark xsimd)))

    ;; Mark as tunable to take advantage of SIMD code in xsimd/xtensor.
    (properties '((tunable? . #t)))))

(define-public gap
  (package
    (name "gap")
    (version "4.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://files.gap-system.org/gap-"
                           (version-major+minor version)
                           "/tar.bz2/gap-"
                           version
                           ".tar.bz2"))
       (sha256
        (base32 "00l6hvy4iggnlrib4vp805sxdm3j7n3hzpv5zs9hbiiavh80l1xz"))
       (modules '((guix build utils) (ice-9 ftw) (srfi srfi-1)))
       (snippet
        '(begin
           ;; Delete the external gmp and zlib libraries
           ;; and a subdirectory not needed for our build.
           (for-each delete-file-recursively
                     '("extern" "hpcgap"))
           ;; Delete a failing test.
           ;; FIXME: This might be fixed in the next release, see
           ;; https://github.com/gap-system/gap/issues/3292
           (delete-file "tst/testinstall/dir.tst")
           ;; Delete all packages except for a fixed list,
           ;; given by their names up to version numbers.
           (with-directory-excursion "pkg"
             (for-each delete-file-recursively
               (lset-difference
                 (lambda (all keep) (string-prefix? keep all))
                 (scandir ".")
                 '("." ".."
                   ;; Necessary packages.
                   "GAPDoc-"
                   "primgrp-"
                   "SmallGrp-"   ; artistic2.0
                   "transgrp"    ; artistic2.0 for data,
                                 ; gpl2 or gpl3 for code
                   ;; Recommended package.
                   "io-"         ; gpl3+
                   ;; Optional packages, searched for at start,
                   ;; and their depedencies.
                   "alnuth-"
                   "autpgrp-"
                   "crisp-"      ; bsd-2
                   "ctbllib"     ; gpl3+, clarified in the next release;
                                 ; see
                                 ; http://www.math.rwth-aachen.de/~Thomas.Breuer/ctbllib/README.md
                   "FactInt-"
                   "fga"
                   "irredsol-"   ; bsd-2
                   "laguna-"
                   "polenta-"
                   "polycyclic-"
                   "radiroot-"
                   "resclasses-"
                   "sophus-"
                   "tomlib-"
                   "utils-"))))
           #t))))
    (build-system gnu-build-system)
    (inputs
     (list gmp readline zlib))
    (arguments
     `(#:modules ((ice-9 ftw)
                  (srfi srfi-26)
                  (guix build gnu-build-system)
                  (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-packages
           ;; Compile all packages that have not been deleted by the
           ;; code snippet above.
           (lambda _
             (setenv "CONFIG_SHELL" (which "bash"))
             (with-directory-excursion "pkg"
               (invoke "../bin/BuildPackages.sh")
             #t)))
         (add-after 'build-packages 'build-doc
           ;; The documentation is bundled, but we create it from source.
           (lambda _
             (with-directory-excursion "doc"
               (invoke "./make_doc"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (prog (string-append bin "/gap"))
                    (prog-real (string-append bin "/.gap-real"))
                    (share (string-append out "/share/gap")))
               ;; Install only the gap binary; the gac compiler is left
               ;; for maybe later. "Wrap" it in a shell script that calls
               ;; the binary with the correct parameter.
               ;; The make target install-bin is supposed to do that, but
               ;; is not currently working.
               (mkdir-p bin)
               (copy-file "gap" prog-real)
               (call-with-output-file prog
                 (lambda (port)
                   (format port
                           "#!~a~%exec ~a -l ~a \"$@\"~%"
                           (which "bash")
                           prog-real
                           share)))
               (chmod prog #o755)
               ;; Install the headers and library, which are needed by Sage.
               (invoke "make" "install-headers")
               (invoke "make" "install-libgap")
               ;; Remove information on the build directory from sysinfo.gap.
               (substitute* "sysinfo.gap"
                 (("GAP_BIN_DIR=\".*\"") "GAP_BIN_DIR=\"\"")
                 (("GAP_LIB_DIR=\".*\"") "GAP_LIB_DIR=\"\"")
                 (("GAP_CPPFLAGS=\".*\"") "GAP_CPPFLAGS=\"\""))
               (invoke "make" "install-gaproot")
               ;; Copy the directory of compiled packages; the make target
               ;; install-pkg is currently empty.
               (copy-recursively "pkg" (string-append share "/pkg")))
             #t)))))
    (home-page "https://www.gap-system.org/")
    (synopsis
     "System for computational group theory")
    (description
     "GAP is a system for computational discrete algebra, with particular
emphasis on computational group theory.  It provides a programming language,
a library of thousands of functions implementing algebraic algorithms
written in the GAP language as well as large data libraries of algebraic
objects.")
    ;; Some packages have different licenses (effectively forcing the
    ;; combined work to be licensed as gpl3+); if this is the case, this
    ;; is mentioned above next to their name.
    ;; Some packages have no license mentioned explicitly; supposedly this
    ;; means that the gpl2+ licence of GAP itself applies, but to be on the
    ;; safe side, we drop them for now.
    (license license:gpl2+)))

(define-public gappa
  (package
   (name "gappa")
   (version "1.3.5")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://gforge.inria.fr/frs/download.php/latestfile/"
                                "2699/gappa-" version ".tar.gz"))
            (sha256
             (base32
              "0q1wdiwqj6fsbifaayb1zkp20bz8a1my81sqjsail577jmzwi07w"))))
   (build-system gnu-build-system)
   (inputs
    (list boost gmp mpfr))
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'patch-remake-shell
          (lambda _
            (substitute* "remake.cpp"
             (("/bin/sh") (which "sh")))
            #t))
        (replace 'build
          (lambda _ (invoke "./remake" "-s" "-d")))
        (replace 'install
          (lambda _ (invoke "./remake" "-s" "-d" "install")))
        (replace 'check
          (lambda _ (invoke "./remake" "check"))))))
   (home-page "http://gappa.gforge.inria.fr/")
   (synopsis "Proof generator for arithmetic properties")
   (description "Gappa is a tool intended to help verifying and formally
proving properties on numerical programs dealing with floating-point or
fixed-point arithmetic.  It has been used to write robust floating-point
filters for CGAL and it is used to certify elementary functions in CRlibm.
While Gappa is intended to be used directly, it can also act as a backend
prover for the Why3 software verification platform or as an automatic tactic
for the Coq proof assistant.")
   (license (list license:gpl3+ license:cecill-c)))) ; either/or

(define-public givaro
  (package
    (name "givaro")
    (version "4.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/linbox-team/givaro")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "11wz57q6ijsvfs5r82masxgr319as92syi78lnl9lgdblpc6xigk"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool))
    (propagated-inputs
     (list gmp)) ; gmp++.h includes gmpxx.h
    (synopsis "Algebraic computations with exact rings and fields")
    (description
     "Givaro is a C++ library implementing the basic arithmetic of various
algebraic objects: prime fields, extension fields, finite fields, finite
rings, polynomials, algebraic numbers, arbitrary precision integers and
rationals (C++ wrappers over gmp), fixed precision integers.  It also
provides data-structures and templated classes for the manipulation of
compound objects, such as vectors, matrices and univariate polynomials.")
    (license license:cecill-b)
    (home-page "https://github.com/linbox-team/givaro")))

(define-public fflas-ffpack
  (package
    (name "fflas-ffpack")
    (version "2.4.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/linbox-team/fflas-ffpack")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ynbjd72qrwp0b4kpn0p5d7gddpvj8dlb5fwdxajr5pvkvi3if74"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (inputs
     (list openblas))
    (propagated-inputs
     (list givaro)) ; required according to the .pc file
    (arguments
     `(#:configure-flags
       (list (string-append "--with-blas-libs="
                            (assoc-ref %build-inputs "openblas")
                            "/lib/libopenblas.so"))))
    (synopsis "C++ library for linear algebra over finite fields")
    (description
     "FFLAS-FFPACK is a C++ template library for basic linear algebra
operations over a finite field.
FFLAS (Finite Field Linear Algebra Subprograms) provides the implementation
of a subset of routines of the numerical BLAS; it also supports sparse
matrix-vector products.
FFPACK (Finite Field Linear Algebra Package) is inspired by the LAPACK
library to provide functionalities of higher level, using the kernel
of a BLAS.  Additionally, it provides routines specific to exact linear
algebra, such as the row echelon form.")
    (license license:lgpl2.1+)
    (home-page "https://linbox-team.github.io/fflas-ffpack/")))

(define-public linbox
  (package
    (name "linbox")
    (version "1.6.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/linbox-team/linbox")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "10j6dspbsq7d2l4q3y0c1l1xwmaqqba2fxg59q5bhgk9h5d7q571"))
              (patches (search-patches "linbox-fix-pkgconfig.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (propagated-inputs
     (list fflas-ffpack))
    (synopsis "C++ library for linear algebra over exact rings")
    (description
     "LinBox is a C++ template library for exact linear algebra computation
with dense, sparse, and structured matrices over the integers and over
finite fields.")
    (license license:lgpl2.1+)
    (home-page "https://linbox-team.github.io/linbox/")))

(define-public m4ri
  (package
    (name "m4ri")
    (version "20140914")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://bitbucket.org/malb/m4ri")
                    (commit (string-append "release-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0xfg6pffbn8r1s0y7bn9b8i55l00d41dkmhrpf7pwk53qa3achd3"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (inputs
     (list libpng))
    (synopsis "Arithmetic of dense matrices over F_2")
    (description "M4RI is a library for fast arithmetic with dense matrices
over F2.  The name M4RI comes from the first implemented algorithm: The
Method of the Four Russians inversion algorithm published by Gregory Bard.
This algorithm in turn is named after the Method of the Four Russians
multiplication algorithm.")
    (license license:gpl2+)
    (home-page "https://bitbucket.org/malb/m4ri/")))

(define-public symmetrica
  (package
    (name "symmetrica")
    (version "2.0")
    (source (origin
              (method url-fetch/tarbomb)
              (uri (let ((v (string-join (string-split version #\.) "_")))
                     (string-append "http://www.algorithm.uni-bayreuth.de/"
                                    "en/research/SYMMETRICA/"
                                    "SYM" v "_tar.gz")))
              (sha256
               (base32
                "1qhfrbd5ybb0sinl9pad64rscr08qvlfzrzmi4p4hk61xn6phlmz"))
              ;; Taken from <https://git.sagemath.org/sage.git/plain/build/pkgs/symmetrica/patches/>
              (patches (search-patches "symmetrica-bruch.patch"
                                       "symmetrica-int32.patch"
                                       "symmetrica-return_values.patch"
                                       "symmetrica-sort_sum_rename.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;no test
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-makefile
           (lambda _
             (substitute* "makefile"
               (("cc -c") "gcc -c -fPIC"))
             #t))
         (add-after 'fix-makefile 'turn-off-banner
           (lambda _
             (substitute* "de.c"
               (("(INT no_banner = )FALSE" _ pre) (string-append pre "TRUE")))
             #t))
         (delete 'configure)            ;no configure script
         (replace 'install              ;no install target
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (inc (string-append out "/include/symmetrica"))
                    (doc (string-append out "/share/doc/symmetrica-" ,version))
                    (static "libsymmetrica.a"))
               ;; Build static library.
               (apply invoke "ar" "crs" static (find-files "." "\\.o$"))
               (invoke "ranlib" static)
               ;; Install static library and headers.
               (for-each (lambda (f) (install-file f inc))
                         (find-files "." "\\.h$"))
               (install-file "libsymmetrica.a" lib)
               ;; Install documentation.
               (for-each (lambda (f) (install-file f doc))
                         (find-files "." "\\.doc$"))
               #t))))))
    (home-page "http://www.algorithm.uni-bayreuth.de/en/research/SYMMETRICA/")
    (synopsis "Combinatoric C Library")
    (description "Symmetrica is a library for combinatorics.  It has support
for the representation theory of the symmetric group and related groups,
combinatorics of tableaux, symmetric functions and polynomials, Schubert
polynomials, and the representation theory of Hecke algebras of type A_n.")
    (license license:public-domain)))

(define-public m4rie
  (package
    (name "m4rie")
    (version "20150908")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://bitbucket.org/malb/m4rie")
                    (commit (string-append "release-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0r8lv46qx5mkz5kp3ay2jnsp0mbhlqr5z2z220wdk73wdshcznss"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool))
    (inputs
     (list m4ri))
    (synopsis "Arithmetic of dense matrices over F_{2^e}")
    (description "M4RI is a library for fast arithmetic with dense matrices
over finite fields of characteristic 2.  So it extends the functionality
of M4RI from F_2 to F_{2^e}.")
    (license license:gpl2+)
    (home-page "https://bitbucket.org/malb/m4rie/")))

(define-public eclib
  (package
    (name "eclib")
    (version "20190909")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/JohnCremona/eclib/")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1gw27lqc3f525n8qdcmr2nyn16y9g10z9f6dnmckyyxcdzvhq35n"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool))
    (inputs
     (list ntl pari-gp))
    (synopsis "Ranks of elliptic curves and modular symbols")
    (description "The eclib package includes mwrank (for 2-descent on
elliptic curves over Q) and modular symbol code; it has been written by
John Cremona to compute his elliptic curve database.")
    (license license:gpl2+)
    (home-page (string-append "http://homepages.warwick.ac.uk/staff/"
                              "J.E.Cremona/mwrank/index.html"))))

(define-public lrcalc
  (package
    (name "lrcalc")
    (version "1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://bitbucket.org/asbuch/lrcalc")
                    (commit (string-append "lrcalc-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1c12d04jdyxkkav4ak8d1aqrv594gzihwhpxvc6p9js0ry1fahss"))
              (patches (search-patches "lrcalc-includes.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-permission
           (lambda _
             (chmod "lrcalc.maple.src" #o644)
             #t)))))
    (synopsis "Littlewood-Richardson calculator in algebraic combinatorics")
    (description "The Littlewood-Richardson Calculator (lrcalc) is a
program designed to compute Littlewood-Richardson coefficients.  It computes
single Littlewood-Richardson coefficients, products of Schur functions, or
skew Schur functions.  In addition it computes products in the small quantum
cohomology ring of a Grassmann variety.  The software package also includes
a program that performs fast computation of the more general multiplicative
structure constants of Schubert polynomials.")
    (license license:gpl2+)
    (home-page "https://sites.math.rutgers.edu/~asbuch/lrcalc/")))

(define-public iml
  (package
    (name "iml")
    (version "1.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.cs.uwaterloo.ca/~astorjoh/iml-"
                           version ".tar.bz2"))
       (sha256
        (base32
         "0akwhhz9b40bz6lrfxpamp7r7wkk48p455qbn04mfnl9a1l6db8x"))))
    (build-system gnu-build-system)
    (inputs
     `(("gmp" ,gmp)
       ("cblas" ,openblas))) ; or any other BLAS library; the documentation
                             ; mentions ATLAS in particular
    (arguments
     `(#:configure-flags
       (list
        "--enable-shared"
        (string-append "--with-gmp-include="
                       (assoc-ref %build-inputs "gmp") "/include")
        (string-append "--with-gmp-lib="
                       (assoc-ref %build-inputs "gmp") "/lib")
        "--with-cblas=-lopenblas"
        (string-append "--with-cblas-include="
                       (assoc-ref %build-inputs "cblas") "/include")
        (string-append "--with-cblas-lib="
                       (assoc-ref %build-inputs "cblas") "/lib"))))
    (home-page "https://cs.uwaterloo.ca/~astorjoh/iml.html")
    (synopsis
     "Solver for systems of linear equations over the integers")
    (description
     "IML is a C library implementing algorithms for computing exact
solutions to dense systems of linear equations over the integers.
Currently, IML provides the following functionality:

@itemize
@item Nonsingular rational system solving:
compute the unique rational solution X to the system AX=B, where A and B
are integer matrices, A nonsingular.
@item Compute the right nullspace or kernel of an integer matrix.
@item Certified linear system solving:
compute a minimal denominator solution x to a system Ax=b, where b is an
integer vector and A is an integer matrix with arbitrary shape and
rank profile.
@end itemize

In addition, IML provides some low level routines for a variety of mod p
matrix operations: computing the row-echelon form, determinant, rank
profile, and inverse of a mod p matrix. These mod p routines are not
general purpose; they require that p satisfy some preconditions based on
the dimension of the input matrix (usually p should be prime and should be
no more than about 20 bits long).")
    (license license:bsd-3)))

(define-public r-dtt
  (package
    (name "r-dtt")
    (version "0.1-2")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "dtt" version))
        (sha256
          (base32
            "0n8gj5iylfagdbaqirpykb01a9difsy4zl6qq55f0ghvazxqdvmn"))))
    (properties `((upstream-name . "dtt")))
    (build-system r-build-system)
    (home-page "http://www.r-project.org")
    (synopsis "Discrete Trigonometric Transforms")
    (description
      "This package provides functions for 1D and 2D Discrete Cosine Transform
(@dfn{DCT}), Discrete Sine Transform (@dfn{DST}) and Discrete Hartley Transform
(@dfn{DHT}).")
    (license license:gpl2+)))

(define-public sollya
  (package
   (name "sollya")
   (version "7.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://www.sollya.org/releases/"
                                "sollya-" version "/sollya-" version ".tar.bz2"))
            (sha256
             (base32
              "11290ivi9h665cxi8f1shlavhy10vzb8s28m57hrcgnxyxqmhx0m"))))
   (build-system gnu-build-system)
   (inputs
    (list fplll
          gmp
          gnuplot
          libxml2
          mpfi
          mpfr))
   (arguments
    `(#:configure-flags
      (list (string-append "--docdir=${datadir}/doc/sollya-" ,version))
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'patch-test-shebang
          (lambda _
            (substitute* (list "tests-tool/Makefile.in"
                               "tests-lib/Makefile.in")
             (("#!/bin/sh") (string-append "#!" (which "sh"))))
            #t))
        (add-before 'build 'patch-gnuplot-reference
          (lambda _
            (substitute* "general.c"
             (("\"gnuplot\"") (string-append "\"" (which "gnuplot") "\"")))
            #t)))))
   (home-page "https://www.sollya.org")
   (synopsis "Development environment for safe floating-point code")
   (description "Sollya is a computer program whose purpose is to
provide an environment for safe floating-point code development.  It
is particularly targeted to the automated implementation of
mathematical floating-point libraries (libm).  Amongst other features,
it offers a certified infinity norm, an automatic polynomial
implementer, and a fast Remez algorithm.")
   (license license:cecill-c)))
