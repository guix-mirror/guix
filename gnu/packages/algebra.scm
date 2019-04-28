;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013, 2015, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017, 2018, 2019 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2014, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017, 2019 Eric Bavier <bavier@member.fsf.org>
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
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages ed)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fltk)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages java)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))


(define-public mpfrcx
  (package
   (name "mpfrcx")
   (version "0.5")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "http://www.multiprecision.org/downloads/mpfrcx-"
                  version ".tar.gz"))
            (sha256
             (base32
              "1s968480ymv6w0rnvfp9mxvx98hvi29fkvw8nk4ggzc6azxgwybs"))))
   (build-system gnu-build-system)
   (propagated-inputs
     `(("gmp" ,gmp)
       ("mpfr" ,mpfr)
       ("mpc"  ,mpc))) ; Header files are included by mpfrcx.h.
   (synopsis "Arithmetic of polynomials over arbitrary precision numbers")
   (description
    "Mpfrcx is a library for the arithmetic of univariate polynomials over
arbitrary precision real (mpfr) or complex (mpc) numbers, without control
on the rounding.  For the time being, only the few functions needed to
implement the floating point approach to complex multiplication are
implemented.  On the other hand, these comprise asymptotically fast
multiplication routines such as Toom–Cook and the FFT.")
   (license license:lgpl3+)
   (home-page "http://mpfrcx.multiprecision.org/")))

(define-public cm
  (package
   (name "cm")
   (version "0.3")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "http://www.multiprecision.org/cm/download/cm-"
                  version ".tar.gz"))
            (sha256
             (base32
              "1nf5kr0nqmhbzrsrinky18z0ighjpsmb5cr8zyg8jf04bfbyrfmc"))))
   (build-system gnu-build-system)
   (propagated-inputs
     `(("mpfrcx" ,mpfrcx)
       ("zlib" ,zlib))) ; Header files included from cm_common.h.
   (inputs
     `(("pari-gp"  ,pari-gp)))
   (synopsis "CM constructions for elliptic curves")
   (description
    "The CM software implements the construction of ring class fields of
imaginary quadratic number fields and of elliptic curves with complex
multiplication via floating point approximations.  It consists of libraries
that can be called from within a C program and of executable command
line applications.")
   (license license:gpl3+)
   (home-page "http://cm.multiprecision.org/")))

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
   (synopsis "Library for LLL-reduction of euclidean lattices")
   (description
    "fplll LLL-reduces euclidean lattices.  Since version 3, it can also
solve the shortest vector problem.")
   (license license:lgpl2.1+)
   (home-page "http://perso.ens-lyon.fr/damien.stehle/fplll/")))

(define-public pari-gp
  (package
    (name "pari-gp")
    (version "2.11.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://pari.math.u-bordeaux.fr/pub/pari/unix/pari-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1jfax92jpydjd02fwl30r6b8kfzqqd6sm4yx94gidyz9lqjb7a94"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("texlive" ,(texlive-union
                    (list texlive-fonts-amsfonts
                          texlive-latex-amsfonts)))))
    (inputs `(("gmp" ,gmp)
              ("libx11" ,libx11)
              ("perl" ,perl)
              ("readline" ,readline)))
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
   (version "0.0.11pl1")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://pari.math.u-bordeaux.fr/pub/pari/GP2C/gp2c-"
                  version ".tar.gz"))
            (sha256
              (base32
                "1c6f6vmncw032kfzrfyr8bynw6yd3faxpy2285r009fmr0zxfs5s"))))
   (build-system gnu-build-system)
   (native-inputs `(("perl" ,perl)))
   (inputs `(("pari-gp" ,pari-gp)))
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
   (version "1.0")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://gforge.inria.fr/frs/download.php/33497/cmh-"
                  version ".tar.gz"))
            (sha256
             (base32
              "1a28xr9bs0igms0ik99x0w8lnb0jyfcmvyi26pbyh9ggcdivd33p"))))
   (build-system gnu-build-system)
   (inputs
     `(("gmp" ,gmp)
       ("mpfr" ,mpfr)
       ("mpc" ,mpc)
       ("mpfrcx" ,mpfrcx)
       ("fplll" ,fplll)
       ("pari-gp"  ,pari-gp)))
   (synopsis "Igusa class polynomial computations")
   (description
    "The CMH software computes Igusa (genus 2) class polynomials, which
parameterize the CM points in the moduli space of 2-dimensional abelian
varieties, i.e. Jacobians of hyperelliptic curves.
It can also be used to compute theta constants at arbitrary
precision.")
   (license license:gpl3+)
   (home-page "http://cmh.gforge.inria.fr/")))

(define-public giac
  (package
    (name "giac")
    (version "1.5.0-49")
    (source (origin
              (method url-fetch)
              ;; "~parisse/giac" is not used because the maintainer regularly
              ;; overwrites the release tarball there, introducing a checksum
              ;; mismatch every time.  See
              ;; <https://www-fourier.ujf-grenoble.fr/~parisse/debian/dists/stable/main/source/README>
              (uri (string-append "https://www-fourier.ujf-grenoble.fr/"
                                  "~parisse/debian/dists/stable/main/"
                                  "source/giac_" version ".tar.gz"))
              (sha256
               (base32
                "0f4pkand9vmqfayw18jm5qxbhcwi1405qfd7ibzh9lwzz6amkm3l"))))
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
             (substitute* (find-files "doc" "^Makefile")
               (("/bin/cp") (which "cp")))
             #t))
         (add-after 'unpack 'disable-failing-test
           ;; FIXME: Test failing.  Not sure why.
           (lambda _
             (substitute* "check/Makefile.in"
               (("chk_fhan11") ""))
             #t))
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
               (delete-file-recursively (string-append out "/share/doc/giac"))
               #t)))
         (add-after 'install 'remove-unnecessary-executable
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (delete-file (string-append out "/bin/xcasnew"))
               #t))))))
    (inputs
     ;;; TODO: Add libnauty.
     `(("fltk" ,fltk)
       ("glpk" ,glpk)
       ("gmp" ,gmp)
       ("gsl" ,gsl)
       ("lapack" ,lapack)
       ("libao" ,ao)
       ("libjpeg" ,libjpeg)
       ("libpng" ,libpng)
       ("libsamplerate" ,libsamplerate)
       ("libx11" ,libx11)
       ("libxinerama" ,libxinerama)
       ("libxft" ,libxft)
       ("libxt" ,libxt)
       ("mesa" ,mesa)
       ("mpfi" ,mpfi)
       ("mpfr" ,mpfr)
       ("ntl" ,ntl)
       ("perl" ,perl)
       ("pari-gp" ,pari-gp)
       ("tcsh" ,tcsh)))
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)
       ("readline" ,readline)
       ("texlive" ,texlive-tiny)))
    (home-page "https://www-fourier.ujf-grenoble.fr/~parisse/giac.html")
    (synopsis "Computer algebra system")
    (description
     "Giac/Xcas is a computer algebra system.  It has a compatibility mode for
maple, mupad and the TI89.  It is available as a standalone program (graphic
or text interfaces) or as a C++ library.")
    (license license:gpl3+)))

(define-public giac-xcas
  (deprecated-package "giac-xcas" giac))

(define-public flint
  (package
   (name "flint")
   (version "2.5.2")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "http://flintlib.org/flint-"
                  version ".tar.gz"))
            (sha256 (base32
                     "11syazv1a8rrnac3wj3hnyhhflpqcmq02q8pqk2m6g2k6h0gxwfb"))
            (patches (search-patches "flint-ldconfig.patch"))))
   (build-system gnu-build-system)
   (propagated-inputs
    `(("gmp" ,gmp)
      ("mpfr" ,mpfr))) ; header files from both are included by flint/arith.h
   (arguments
    `(#:parallel-tests? #f ; seems to be necessary on arm
      #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (gmp (assoc-ref inputs "gmp"))
                   (mpfr (assoc-ref inputs "mpfr")))
               ;; do not pass "--enable-fast-install", which makes the
               ;; homebrew configure process fail
               (invoke "./configure"
                       (string-append "--prefix=" out)
                       (string-append "--with-gmp=" gmp)
                       (string-append "--with-mpfr=" mpfr))
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
   (license license:gpl2+)
   (home-page "http://flintlib.org/")))

(define-public arb
  (package
    (name "arb")
    (version "2.16.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/fredrik-johansson/arb.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0478671wfwy3gl26sbxh1jq1ih36z4k72waa8y2y2lvn649gb7cd"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("flint" ,flint)))               ; flint.h is included by arf.h
    (inputs
     `(("gmp" ,gmp)
       ("mpfr" ,mpfr)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (flint (assoc-ref inputs "flint"))
                   (gmp (assoc-ref inputs "gmp"))
                   (mpfr (assoc-ref inputs "mpfr")))
               ;; do not pass "--enable-fast-install", which makes the
               ;; homebrew configure process fail
               (invoke "./configure"
                       (string-append "--prefix=" out)
                       (string-append "--with-flint=" flint)
                       (string-append "--with-gmp=" gmp)
                       (string-append "--with-mpfr=" mpfr))))))))
    (synopsis "Arbitrary precision floating-point ball arithmetic")
    (description
     "Arb is a C library for arbitrary-precision floating-point ball
arithmetic.  It supports efficient high-precision computation with
polynomials, power series, matrices and special functions over the
real and complex numbers, with automatic, rigorous error control.")
    (license license:lgpl2.1+)
    (home-page "http://fredrikj.net/arb/")))

(define-public python-flint
  (package
    (name "python-flint")
    (version "0.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/fredrik-johansson/python-flint.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1v0anazbj1cfi68nl2j6dbd31kgkc1563xmr0zk5xk3xj78569pw"))
              (patches (search-patches "python-flint-includes.patch"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-cython" ,python-cython)))
    (propagated-inputs
     `(("python-numpy" ,python-numpy)))
    (inputs
     `(("arb" ,arb)
       ("flint" ,flint)))
    (synopsis "Python module wrapping ARB and FLINT")
    (description
     "Python-flint is a Python extension module wrapping FLINT
(Fast Library for Number Theory) and Arb (arbitrary-precision ball
arithmetic).  It supports integers, rationals, modular integers,
real and complex ball arithmetic, polynomials and matrices over all
these types and other mathematical functions.")
    (license license:expat)
    (home-page "http://fredrikj.net/python-flint/")))

(define-public ntl
  (package
   (name "ntl")
   (version "9.7.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://shoup.net/ntl/ntl-"
                                version ".tar.gz"))
            (sha256 (base32
                     "115frp5flyvw9wghz4zph1b3llmr5nbxk1skgsggckr81fh3gmxq"))))
   (build-system gnu-build-system)
   (native-inputs
    `(("libtool" ,libtool)
      ("perl" ,perl))) ; for configuration
   ;; FIXME: Add optional input gf2x once available; then also add
   ;; configure flag "NTL_GF2X_LIB=on".
   (inputs
    `(("gmp" ,gmp)))
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (replace 'configure
         (lambda* (#:key outputs #:allow-other-keys)
           (chdir "src")
           (system* "./configure"
                    (string-append "PREFIX=" (assoc-ref outputs "out"))
                    ;; Do not build especially for the build machine.
                    "NATIVE=off"
                    ;; Also do not tune to the build machine.
                    "WIZARD=off"
                    "SHARED=on")
           #t)))))
   (synopsis "C++ library for number theory")
   (description
    "NTL is a C++ library providing data structures and algorithms
for manipulating signed, arbitrary length integers, and for vectors,
matrices, and polynomials over the integers and over finite fields.")
   (license license:gpl2+)
   (home-page "http://shoup.net/ntl/")))

(define-public singular
  (package
   (name "singular")
   (version "4.1.2p1")
   (source
    (origin
      (method url-fetch)
      (uri
       (string-append "http://www.mathematik.uni-kl.de/ftp/pub/Math/"
                      "Singular/SOURCES/"
                      (string-join
                       (string-split
                        (string-trim-right version #\p
                                           0 (1- (string-length version)))
                        #\.) "-")
                      "/singular-" version ".tar.gz"))
             (sha256 (base32
                      "0kvd55353fiqyq1msmi0kka66n5h0aqs7m3km60r01b1w2f8085m"))))
   (build-system gnu-build-system)
   (native-inputs
    `(("doxygen" ,doxygen)
      ("graphviz" ,graphviz)
      ("perl" ,perl)))
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
   (home-page "http://www.singular.uni-kl.de/index.php")))

(define-public gmp-ecm
  (package
   (name "gmp-ecm")
   (version "7.0.4")
   (source (origin
             (method url-fetch)
             ;; Use the ‘Latest version’ link for a stable URI across releases.
             (uri (string-append "https://gforge.inria.fr/frs/download.php/"
                                 "latestfile/160/ecm-" version ".tar.gz"))
             (sha256 (base32
                      "0hxs24c2m3mh0nq1zz63z3sb7dhy1rilg2s1igwwcb26x3pb7xqc"))))
   (build-system gnu-build-system)
   (inputs
    `(("gmp" ,gmp)))
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
               "0amh9ik44jfg66csyvf4zz1l878c4755kjndq9j0270akflgrbb2"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("ed" ,ed)
       ("flex" ,flex)
       ("texinfo" ,texinfo)))
    (arguments
     '(#:configure-flags
       (list "--with-readline")))
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
                    (url "https://github.com/extemporelang/kiss_fft.git")
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
    (native-inputs `(("perl" ,perl)))
    (home-page "http://fftw.org")
    (synopsis "Computing the discrete Fourier transform")
    (description
     "FFTW is a C subroutine library for computing the discrete Fourier
transform (DFT) in one or more dimensions, of arbitrary input size, and of
both real and complex data (as well as of even/odd data---i.e. the discrete
cosine/ sine transforms or DCT/DST).")
    (license license:gpl2+)))

(define-public fftwf
  (package (inherit fftw)
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
  (package (inherit fftw)
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
                    (url "https://github.com/vkostyukov/la4j.git")
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
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)))
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
              (uri (string-append "http://search.maven.org/remotecontent?"
                                  "filepath=pl/edu/icm/JLargeArrays/"
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
     `(("java-commons-math3" ,java-commons-math3)))
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
              (uri (string-append "http://search.maven.org/remotecontent?"
                                  "filepath=com/github/wendykierp/JTransforms/"
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
     `(("java-commons-math3" ,java-commons-math3)
       ("java-jlargearrays" ,java-jlargearrays)))
    (home-page "https://github.com/wendykierp/JTransforms")
    (synopsis "Multithreaded FFT library written in pure Java")
    (description "JTransforms is a multithreaded FFT library written in pure
Java.  Currently, four types of transforms are available: @dfn{Discrete
Fourier Transform} (DFT), @dfn{Discrete Cosine Transform} (DCT), @dfn{Discrete
Sine Transform} (DST) and @dfn{Discrete Hartley Transform} (DHT).")
    (license license:bsd-2)))

(define-public eigen
  (package
    (name "eigen")
    (version "3.3.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://bitbucket.org/eigen/eigen/get/"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "1qh3yrwn78ms5yhwbpl5wvblk4gbz02cacdygxylr7i9xbrvylkk"))
              (file-name (string-append name "-" version ".tar.bz2"))
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
                       "# Do not build the tests for unsupported features.\n"))
                    #t)))))))

(define-public xtensor
  (package
    (name "xtensor")
    (version "0.17.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/QuantStack/xtensor.git")
                    (commit version)))
              (sha256
               (base32
                "0w40v5lp0hp8ihf8nnvak373sb5xx0768pxgiqh3nzn57wf8px4r"))
              (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (native-inputs
     `(("googletest" ,googletest)
       ("xtl" ,xtl)))
    (arguments
     `(#:configure-flags
       '("-DBUILD_TESTS=ON")
       #:test-target "xtest"))
    (home-page "http://quantstack.net/xtensor")
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

(define-public gap
  (package
    (name "gap")
    (version "4.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.gap-system.org/pub/gap/gap-"
                           (version-major+minor version)
                           "/tar.bz2/gap-"
                           version
                           ".tar.bz2"))
       (sha256
        (base32
         "1dmb8v4p7j1nnf7sx8sg54b49yln36bi9acwp7w1d3a1nxj17ird"))
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
           ;; Delete all packages except for a fixed list.
           (with-directory-excursion "pkg"
             (for-each delete-file-recursively
               (lset-difference string=? (scandir ".")
                 '("." ".."
                   ;; Necessary packages.
                   "GAPDoc-1.6.2"
                   "primgrp-3.3.2"
                   "SmallGrp-1.3"    ; artistic2.0
                   "transgrp"        ; artistic2.0 for data,
                                     ; gpl2 or gpl3 for code
                   ;; Recommanded package.
                   "io-4.5.4"        ; gpl3+
                   ;; Optional packages, searched for at start,
                   ;; and their depedencies.
                   "alnuth-3.1.0"
                   "AutoDoc-2018.09.20"
                   "autpgrp-1.10"
                   "crisp-1.4.4"     ; bsd-2
                   ; "ctbllib"       ; no explicit license, drop
                   "FactInt-1.6.2"
                   "fga"
                   "irredsol-1.4"    ; bsd-2
                   "laguna-3.9.0"
                   "polenta-1.3.8"
                   "polycyclic-2.14"
                   "radiroot-2.8"
                   "resclasses-4.7.1"
                   "sophus-1.24"
                   ; "tomlib-1.2.7"  ; no explicit license, drop
                   "utils-0.59"))))
           #t))))
    (build-system gnu-build-system)
    (inputs
     `(("gmp" ,gmp)
       ("zlib" ,zlib)))
    (arguments
     `(#:phases
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
         (replace 'check
           (lambda _
             ;; "make check" is expected to appear in gap-4.10.1
             (invoke "./gap" "tst/testinstall.g")
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (prog (string-append bin "/gap"))
                    (prog-real (string-append bin "/.gap-real"))
                    (share (string-append out "/share/gap"))
                    (include (string-append out "/include/gap")))
               ;; Install only the gap binary; the gac compiler is left
               ;; for maybe later. "Wrap" it in a shell script that calls
               ;; the binary with the correct parameter.
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
               ;; Install the headers and the library, which are needed by
               ;; Sage. The Makefile targets are available in gap-4.10.0,
               ;; but planned to be removed in gap-4.10.1.
               (invoke "make" "install-headers")
               (invoke "make" "install-libgap")
               (install-file "gen/config.h" include)
               ;; Install a certain number of files and directories to
               ;; SHARE, where the wrapped shell script expects them.
               ;; Remove information on the build directory from sysinfo.gap.
               (substitute* "sysinfo.gap"
                 (("GAP_BIN_DIR=\".*\"") "GAP_BIN_DIR=\"\"")
                 (("GAP_LIB_DIR=\".*\"") "GAP_LIB_DIR=\"\"")
                 (("GAP_CPPFLAGS=\".*\"") "GAP_CPPFLAGS=\"\""))
               (install-file "sysinfo.gap" share)
               (copy-recursively "grp" (string-append share "/grp"))
               (copy-recursively "pkg" (string-append share "/pkg"))
               ;; The following is not the C library libgap.so, but a
               ;; library of GAP code.
               (copy-recursively "lib" (string-append share "/lib"))
               ;; The gap binary looks for documentation inside SHARE.
               (copy-recursively "doc" (string-append share "/doc")))
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
