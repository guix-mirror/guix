;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Justus Winter <justus@sequoia-pgp.org>
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

(define-module (gnu packages openpgp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages multiprecision))

(define-public libtmcg
  (package
    (name "libtmcg")
    (version "1.3.18")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/libtmcg/libTMCG-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "179b5jx3mqs9hgsj8cfwk6x8qib60kw9szk9fkz6s1gl3v83mnyx"))))
    (build-system gnu-build-system)
    (arguments '(#:configure-flags '("--enable-silent-rules")))
    (inputs `(("gmp" ,gmp)
              ("libgcrypt" ,libgcrypt)))
    (synopsis
     "C++ library for creating secure and fair online card games")
    (description
     "The library provides a sort of useful classes, algorithms, and
high-level protocols to support an application programmer in writing such
software.  The most remarkable feature is the absence of a trusted third
party (TTP), i.e. neither a central game server nor trusted hardware
components are necessary.

The corresponding cryptographic problem, actually called Mental Poker, has
been studied since 1979 (Shamir, Rivest, and Adleman) by many authors.
LibTMCG provides the first practical implementation of such protocols.")
    (home-page "https://www.nongnu.org/libtmcg/")
    (license license:gpl2+)))
