;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages perl-maths)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system perl)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check))

(define-public perl-math-cephes
  (package
    (name "perl-math-cephes")
    (version "0.5305")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/S/SH/SHLOMIF/Math-Cephes-"
                    version ".tar.gz"))
              (sha256
               (base32
                "18c3xg53d1vv7hlj43601jj7ks119fm6ndpwpv94irr2905806jn"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Math-Cephes")
    (synopsis "Perl interface to the Cephes math library")
    (description "The Math::Cephes module provides a Perl interface to over
150 functions of the Cephes math library.")
    (license license:perl-license)))

(define-public perl-math-matrixreal
  (package
    (name "perl-math-matrixreal")
    (version "2.13")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/L/LE/LETO/Math-MatrixReal-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1cml5wqd99hm398gl8f147ccsck9v179l7a6vqjj4kfkdnja37sg"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl-test-most" ,perl-test-most)))
    (home-page "https://metacpan.org/release/Math-MatrixReal")
    (synopsis "Manipulate NxN matrices of real numbers")
    (description "This package provides the @code{Math::MatrixReal} module.
It implements the data type \"matrix of real numbers\" (and consequently also
\"vector of real numbers\").")
    (license license:perl-license)))
