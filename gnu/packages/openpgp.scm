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
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages web))

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
    (inputs (list gmp libgcrypt))
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

(define-public dkgpg
  (package
    (name "dkgpg")
    (version "1.1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/dkgpg/dkgpg-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "1hpfg7akd5icj49i03z74hp9zj0xwl90bndn0hnw0hpb8lk7qcxg"))))
    (build-system gnu-build-system)
    (arguments '(#:configure-flags
                 '("--enable-silent-rules")
                 ;; https://savannah.nongnu.org/bugs/?58772
                 #:parallel-tests? #f))
    (inputs (list bzip2 gmp libgcrypt libtmcg zlib))
    (synopsis
     "Distributed Key Generation and Threshold Cryptography for OpenPGP")
    (description
     "The Distributed Privacy Guard (DKGPG) implements Distributed Key
Generation (DKG) and Threshold Cryptography for OpenPGP.  The generated public
keys are compatible with the standard and thus can be used by any
RFC4880-compliant application (e.g. GnuPG).  The main purposes of this
software are distributing power among multiple parties, eliminating single
points of failure, and increasing the difficulty of side-channel attacks on
private key material.

DKGPG consists of a bunch of simple command-line programs.  The current
implementation is in experimental state and should NOT be used in production
environments.")
    (home-page "https://www.nongnu.org/dkgpg/")
    (license license:gpl2+)))

(define-public rnp
  ;; Packaging the currently released version requires a large number of
  ;; patches.  For now, we package a snapshot instead.
  (let ((commit "203224f0b1505dba17837c03da603e5b98ab125a")
        (revision "0")
        (last-version "0.13.1")
        (day-of-release "2020-07-21"))
    (package
      (name "rnp")
      (version (git-version last-version revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/rnpgp/rnp")
                      (commit commit)))
                (file-name
                 (string-append name "-" (string-take commit 7) "-checkout"))
                (sha256
                 (base32
                  "1rnwhc9ys4v4mv584hmmrl0ycnqmsaigpffzm31qq337hz24zqya"))
                (patches
                 (search-patches "rnp-unbundle-googletest.patch"
                                 "rnp-disable-ruby-rnp-tests.patch"
                                 "rnp-add-version.cmake.patch"))))
      (build-system cmake-build-system)
      (arguments `(#:configure-flags
                   '("-DBUILD_SHARED_LIBS=on"
                     "-DBUILD_TESTING=on")
                   #:phases
                   (modify-phases %standard-phases
                     (add-after 'unpack 'fixes
                       (lambda* (#:key inputs #:allow-other-keys)
                         (copy-recursively (assoc-ref inputs "googletest-source")
                                           "src/tests/googletest-src")
                         (substitute* "src/tests/support.cpp"
                           (("\"cp\"") (string-append "\"" (which "cp") "\"")))
                         ;; Produce a version stamp in the format the upstream
                         ;; project uses for unreleased revisions.
                         (with-output-to-file "version.txt"
                           (lambda _
                             (display
                              (string-append ,last-version
                                             "-" ,revision
                                             "-g" ,(string-take commit 7)))))
                         #t))
                     (replace 'check
                       (lambda _
                         ;; Some OpenPGP certificates used by the tests expire.
                         ;; To work around that, set the time to roughly the
                         ;; release date.
                         (invoke "faketime" ,day-of-release "make" "test"))))))
      (native-inputs
       `(("gnupg" ,gnupg) ; for tests
         ("googletest-source" ,(package-source googletest)) ; for tests
         ("libfaketime" ,libfaketime) ; for tests
         ("pkg-config" ,pkg-config)
         ("python" ,python)
         ("python2" ,python-2.7)))
      (inputs (list botan bzip2 json-c zlib))
      (synopsis
       "RFC4880-compliant OpenPGP library written in C++")
      (description
       "Set of OpenPGP (RFC4880) tools that works on Linux, *BSD and macOS as a
replacement of GnuPG.  It is maintained by Ribose after being forked from
NetPGP, itself originally written for NetBSD.

librnp is the library used by rnp for all OpenPGP functions, useful for
developers to build against.  It is a “real” library, not a wrapper like GPGME
of GnuPG.")
      (home-page "https://www.rnpgp.com/")
      (license
       ;; RNP contains code written by Ribose and code derived from netpgp.
       (list
        ;; Ribose's BSD 2-Clause License and NetBSD's BSD 2-Clause License
        ;; (netpgp).
        license:bsd-2
        ;; Nominet UK's Apache 2.0 Licence (netpgp).
        license:asl2.0
        ;; Nominet UK's BSD 3-Clause License (netpgp).
        license:bsd-3)))))
