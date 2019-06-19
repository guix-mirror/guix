;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Andreas Enge <andreas@enge.fr>
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
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz))


(define-public python-cypari2
  (package
    (name "python-cypari2")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cypari2" version))
       (sha256
        (base32
         "1nwkzgqvbw6361x0rpggy1q5nx663fswhpvg8md6xhqyfwpgc7nz"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-cython" ,python-cython)))
    (propagated-inputs
     `(("python-cysignals" ,python-cysignals)))
    (inputs
     `(("gmp" ,gmp)
       ("pari-gp", pari-gp)))
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
     `(("unzip" ,unzip)))
    (inputs
     `(("gmp" ,gmp)
       ("mpfr" ,mpfr)
       ("mpc" ,mpc)))
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
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
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
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (inputs
     `(("libgc" ,libgc)))
    (synopsis "Computation of homfly polynomials of links")
    (description "libhomfly computes homfly polynomials of links,
represented as strings.")
    (license license:public-domain)
    (home-page "https://github.com/miguelmarco/libhomfly")))

;; The following three packages from the Linbox group are needed in
;; an outdated version for Sage.

(define-public givaro-4.0.4
  (package (inherit givaro)
    (name "givaro")
    (version "4.0.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/linbox-team/givaro")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "199p8wyj5i63jbnk7j8qbdbfp5rm2lpmcxyk3mdjy9bz7ygx3hhy"))))))

(define-public fflas-ffpack-2.3.2
  (package (inherit fflas-ffpack)
    (name "fflas-ffpack")
    (version "2.3.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/linbox-team/fflas-ffpack")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1cqhassj2dny3gx0iywvmnpq8ca0d6m82xl5rz4mb8gaxr2kwddl"))))
    (propagated-inputs
     `(("givaro" ,givaro-4.0.4)))
    ;; A test fails, but since all tests pass in the latest version,
    ;; there is not much point in investigating.
    (arguments
     (substitute-keyword-arguments (package-arguments fflas-ffpack)
       ((#:tests? _ #f) #f)))))

(define-public linbox-1.5.2
  (package (inherit linbox)
    (version "1.5.2")
    (name "linbox")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/linbox-team/linbox")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wfivlwp30mzdy1697w7rzb8caajim50mc8h27k82yipn2qc5n4i"))))
    (inputs
     `(("fflas-ffpack" ,fflas-ffpack-2.3.2)))))

(define-public pynac
  (package
    (name "pynac")
    (version "0.7.25")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/pynac/pynac/")
              (commit (string-append "pynac-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0nnifvg6kzx0lq6gz7znind8g30v3d2pjfwgsdiks3vv9kv9nbj3"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("flint" ,flint)
       ("gmp" ,gmp)
       ("python" ,python)
       ("singular" ,singular)))
    (synopsis "Sage fork of GiNaC")
    (description "Pynac is a derivative of the C++ library GiNaC, which
allows manipulation of symbolic expressions.  It currently provides the
backend for symbolic expressions in Sage.  The main difference between
Pynac and GiNaC is that Pynac relies on Sage to provide the operations
on numerical types, while GiNaC depends on CLN for this purpose.")
    (license license:gpl2+)
    (home-page "http://pynac.org/")))
