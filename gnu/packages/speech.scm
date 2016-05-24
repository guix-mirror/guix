;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Thompson <davet@gnu.org>
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

(define-module (gnu packages speech)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages gcc))

(define-public mitlm
  (package
    (name "mitlm")
    (version "0.4.1")
    ;; No official release tarballs, so for now we use the one from Debian
    ;; that is maintained by one of the project developers.
    ;;
    ;; See: https://github.com/mitlm/mitlm/issues/54
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://debian/pool/main/m/mitlm/mitlm_"
                                  version ".orig.tar.gz"))
              (sha256
               (base32
                "12m09xxx8jbir9cnzzaysvni5sfijpfj96z1x1520qqvmpc8lmn7"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gfortran" ,gfortran)))
    (synopsis "The MIT Language Modeling toolkit")
    (description "The MIT Language Modeling (MITLM) toolkit is a set of
tools designed for the efficient estimation of statistical n-gram language
models involving iterative parameter estimation.  It achieves much of its
efficiency through the use of a compact vector representation of n-grams.")
    (home-page "https://github.com/mitlm/mitlm")
    (license license:expat)))
