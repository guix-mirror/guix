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
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages multiprecision)
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
;; used with Sage, since it does not support cython. One would need to
;; use an alpha release. On the other hand, Sage can be built without it.
(define-public python-gmpy2
  (package
    (name "python-gmpy2")
    (version "2.0.8")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "gmpy2" version ".zip"))
              (sha256
               (base32
                "0grx6zmi99iaslm07w6c2aqpnmbkgrxcqjrqpfq223xri0r3w8yx"))))
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
