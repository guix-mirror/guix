;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2019 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2021 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages sagemath)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages image)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz))


(define-public python-cypari2
  (package
    (name "python-cypari2")
    (version "2.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cypari2" version))
       (sha256
        (base32
         "0ymc4i9y60aazscc1blivirkr1rflzz6akkmvfzyn5l7mgnlbk83"))))
    (build-system python-build-system)
    (native-inputs
     (list python-cython))
    (propagated-inputs
     (list python-cysignals))
    (inputs
     (list gmp pari-gp))
    (home-page "https://cypari2.readthedocs.io/")
    (synopsis
     "Python interface to the number theory library libpari")
    (description
     "Cypari2 provides a Python interface to the number theory library
PARI/GP.  It has been spun off from the SageMath mathematics software system,
but it can be used independently.")
    (license license:gpl2+)))

(define-public python2-cypari2
  (package-with-python2 python-cypari2))

;; The stable version of the following package is not young enough to be
;; used with Sage, since it does not support cython; so we use a beta
;; release.
(define-public python-gmpy2
  (package
    (name "python-gmpy2")
    (version "2.1.0b1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/aleaxit/gmpy")
                    (commit (string-append "gmpy2-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ljvnmhxqdfsp0yy4c2hynhk5sggm63kkqsq4iwq4k9vsnx2xm97"))))
    (build-system python-build-system)
    (native-inputs
     (list unzip))
    (inputs
     (list gmp mpfr mpc))
    (home-page "https://github.com/aleaxit/gmpy")
    (synopsis
     "GMP/MPIR, MPFR, and MPC interface to Python 2.6+ and 3.x")
    (description
     "This package provides a Python interface to the GNU multiprecision
libraries GMO, MPFR and MPC.")
    (license license:lgpl3+)))

(define-public python2-gmpy2
  (package-with-python2 python-gmpy2))

(define-public cliquer
  (package
    (name "cliquer")
    (version "1.21")
    ;; The original source package is available from the home page and
    ;; has not seen any release since 2010; it comes with only a Makefile
    ;; without an "install" target. Instead, there is an autotoolized
    ;; tarball available from the Sage project.
    (source
     (origin
       (method url-fetch)
       (uri "http://users.ox.ac.uk/~coml0531/sage/cliquer-1.21.tar.gz")
       (sha256
        (base32
         "1hdzrmrx0nvvj8kbwxrs8swqgkd284khzl623jizixcv28xb77aq"))))
    (build-system gnu-build-system)
    (synopsis "C routines for finding cliques in weighted graphs")
    (description "Cliquer is a set of reentrant C routines for finding
cliques in a weighted or unweighted graph.  It uses an exact
branch-and-bound algorithm.  It can search for maximum or maximum-weight
cliques or cliques with size or weight within a given range, restrict the
search to maximal cliques, store cliques in memory and call a user-defined
function for every found clique.")
    (license license:gpl2+)
    (home-page "https://users.aalto.fi/~pat/cliquer.html")))

(define-public libbraiding
  (package
    (name "libbraiding")
    (version "1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url (string-append "https://github.com/miguelmarco/"
                                  name))
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0l68rikfr7k2l547gb3pp3g8cj5zzxwipm79xrb5r8ffj466ydxg"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool))
    (synopsis "Computations with braid groups")
    (description "libbraiding performs computations with braid groups,
in particular it computes normal forms of group elements.")
    (license license:gpl2+)
    (home-page "https://github.com/miguelmarco/libbraiding")))

(define-public libhomfly
  (package
    (name "libhomfly")
    (version "1.02r6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url (string-append "https://github.com/miguelmarco/"
                                  name))
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0sv3cwrf9v9sb5a8wbhjmarxvya13ma3j8y8592f9ymxlk5y0ldk"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool))
    (inputs
     (list libgc))
    (synopsis "Computation of homfly polynomials of links")
    (description "libhomfly computes homfly polynomials of links,
represented as strings.")
    (license license:public-domain)
    (home-page "https://github.com/miguelmarco/libhomfly")))

;; Sage 9.1 doesn't build with ECL 20.  This won't be necessary once 9.2 is
;; released.  See https://trac.sagemath.org/ticket/22191
(define-public ecl-16
  (package
    (inherit ecl)
    (version "16.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://common-lisp.net/project/ecl/static/files/release/ecl"
             "-" version ".tgz"))
       (sha256
        (base32 "0m0j24w5d5a9dwwqyrg0d35c0nys16ijb4r0nyk87yp82v38b9bn"))
       (patches (search-patches
                  "ecl-16-libffi.patch"
                  "ecl-16-ignore-stderr-write-error.patch"
                  "ecl-16-format-directive-limit.patch"))))
    ;; Current ECL uses LGPL 2.1+
    (license license:lgpl2.0+)))

(define-public pynac
  (package
    (name "pynac")
    (version "0.7.26")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/pynac/pynac/")
              (commit (string-append "pynac-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09d2p74x1arkydlxy6pw4p4byi7r8q7f29w373h4d8a215kadc6d"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (inputs
     (list flint gmp python singular))
    (synopsis "Sage fork of GiNaC")
    (description "Pynac is a derivative of the C++ library GiNaC, which
allows manipulation of symbolic expressions.  It currently provides the
backend for symbolic expressions in Sage.  The main difference between
Pynac and GiNaC is that Pynac relies on Sage to provide the operations
on numerical types, while GiNaC depends on CLN for this purpose.")
    (license license:gpl2+)
    (home-page "http://pynac.org/")))

;; Sage has become upstream of the following package.
(define-public zn-poly
  (package
    (name "zn-poly")
    (version "0.9.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url (string-append "https://gitlab.com/sagemath/"
                                  "zn_poly.git/"))
              (commit version)))
       (file-name (git-file-name "zn_poly" version))
       (sha256
        (base32 "1wbc3apxcldxfcw1dnwnn7fvlfb6bwvlr8glvgv6hf79p9r2s4j0"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("python" ,python-2)))
    (inputs
     (list gmp))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           ;; The configure script chokes on --enable-fast-install.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (invoke "./configure"
                     (string-append "--prefix=" (assoc-ref outputs "out"))
                     "--cflags=-O3 -fPIC")))
         (add-before 'build 'prepare-build
           (lambda _
             (setenv "CC" "gcc")
             #t))
         (add-after 'build 'build-so
           (lambda _
             (invoke "make" "libzn_poly.so")))
         (add-after 'install 'install-so
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (soname (string-append "libzn_poly-" ,version ".so"))
                    (target (string-append lib "/" soname)))
               (install-file "libzn_poly.a" lib)
               (install-file soname lib)
               (symlink target
                        (string-append lib "/libzn_poly.so"))
               (symlink target
                        (string-append lib "/libzn_poly-"
                                       ,(version-major+minor version)
                                       ".so")))
             #t)))))
    (synopsis "Arithmetic for polynomials over Z/NZ")
    (description "zn_poly implements the arithmetic of polynomials the
coefficients of which are modular integers.")
    (license (list license:gpl2 license:gpl3)) ; dual licensed
    (home-page "https://gitlab.com/sagemath/zn_poly")))

(define-public brial
  (package
    (name "brial")
    (version "1.2.8")
    (source
    (origin
      (method git-fetch)
      (uri (git-reference
             (url "https://github.com/BRiAl/BRiAl/")
             (commit version)))
      (file-name (git-file-name name version))
      (sha256
       (base32 "0qhgckd4fvbs40jw14mvw89rccv94d3df27kipd27hxd4cx7y80y"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (inputs
     (list boost libpng m4ri))
    (arguments
    ;; We are missing the boost unit test framework.
     `(#:tests? #f
       #:configure-flags (list "--without-boost-unit-test-framework")))
    (synopsis "Arithmetic of polynomials over boolean rings")
    (description "BRiAl is the successor to  PolyBoRi maintained by the
Sage community.  Its core is a C++ library, which provides high-level data
types for Boolean polynomials and monomials, exponent vectors, as well as
for the underlying polynomial rings and subsets of the powerset of the
Boolean variables.  As a unique approach, binary decision diagrams are
used as internal storage type for polynomial structures.")
    (license license:gpl2+)
    (home-page "https://github.com/BRiAl/BRiAl/")))

(define-public lcalc
  (package
    (name "lcalc")
    (version "1.23")
    ;; The original home page of the project has disappeared, as well as
    ;; code hosted by the original author on Google Code. The latter has
    ;; been copied to gitlab.com/sagemath and purportedly contains patches
    ;; for a never released version 1.3, that supposedly follows 1.23.
    ;; We use the tarball as well as the patches hosted inside the sage
    ;; package system distributed with the sage tarball.
    (source
     (origin
       (method url-fetch)
       (uri (string-append "ftp://ftp.fu-berlin.de/unix/misc/sage/spkg/"
                           "upstream/lcalc/lcalc-1.23.tar.bz2"))
       (sha256
        (base32
         "1c6dsdshgxhqppjxvxhp8yhpxaqvnz3d1mlh26r571gkq8z2bm43"))
       (patches (search-patches "lcalc-lcommon-h.patch"
                                "lcalc-default-parameters-1.patch"
                                "lcalc-default-parameters-2.patch"
                                "lcalc-using-namespace-std.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'prepare-build
           (lambda* (#:key outputs #:allow-other-keys)
             (chdir "src")
             (let ((out (assoc-ref outputs "out")))
               (substitute* "Makefile"
                 (("^INSTALL_DIR= /usr/local")
                  (string-append "INSTALL_DIR=" out))))
             #t))
         (add-before 'install 'make-output-dirs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lib (string-append out "/lib"))
                    (include (string-append out "/include")))
               (mkdir-p bin)
               (mkdir-p lib)
               (mkdir-p include))
             #t)))))
    ;; FIXME:
    ;; We need to add pari-gp and probably pari related patches from the
    ;; sage project, as well as uncomment the line setting PARI_DEFINE in
    ;; the Makefile to get the full functionality of this package.
    ;; For the time being, we hope that sage can be compiled without.
    (synopsis "C++ library for L-functions")
    (description "Lcalc computes L-functions, in particular the Riemann
zeta function and its twists by quadratic characters.")
    (license license:gpl2+)
    (home-page "https://gitlab.com/sagemath/sage")))

(define-public ratpoints
  (package
    (name "ratpoints")
    (version "2.1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.mathe2.uni-bayreuth.de/stoll/programs/"
                    "ratpoints-" version ".tar.gz"))
              (sha256
               (base32
                "0zhad84sfds7izyksbqjmwpfw4rvyqk63yzdjd3ysd32zss5bgf4"))
              (patches
               ;; Taken from
               ;; <https://git.sagemath.org/sage.git/plain/build/pkgs/ratpoints/patches/>
               (search-patches "ratpoints-sturm_and_rp_private.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:make-flags
       (list (string-append "INSTALL_DIR=" (assoc-ref %outputs "out"))
             "CCFLAGS=-fPIC")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ;no configure script
         (add-before 'install 'create-install-directories
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p out)
               (with-directory-excursion out
                 (for-each (lambda (d) (mkdir-p d))
                           '("bin" "include" "lib"))))
             #t)))))
    (inputs
     (list gmp))
    (home-page "http://www.mathe2.uni-bayreuth.de/stoll/programs/")
    (synopsis "Find rational points on hyperelliptic curves")
    (description "Ratpoints tries to find all rational points within
a given height bound on a hyperelliptic curve in a very efficient way,
by using an optimized quadratic sieve algorithm.")
    (license license:gpl2+)))
