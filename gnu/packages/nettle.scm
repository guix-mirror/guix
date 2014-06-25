;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages nettle)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages m4))

(define-public nettle
  (package
    (name "nettle")
    (version "2.7.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/nettle/nettle-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0h2vap31yvi1a438d36lg1r1nllfx3y19r4rfxv7slrm6kafnwdw"))))
    (build-system gnu-build-system)
    (arguments
     ;; 'sexp-conv' and other programs need to have their RUNPATH point to
     ;; $libdir, which is not the case by default.  Work around it.
     '(#:configure-flags (list (string-append "LDFLAGS=-Wl,-rpath="
                                              (assoc-ref %outputs "out")
                                              "/lib"))))
    (native-inputs `(("m4" ,m4)))
    (propagated-inputs `(("gmp" ,gmp)))
    (home-page "http://www.lysator.liu.se/~nisse/nettle/")
    (synopsis "C library for low-level cryptographic functionality")
    (description
     "GNU Nettle is a low-level cryptographic library.  It is designed to
fit in easily in almost any context.  It can be easily included in
cryptographic toolkits for object-oriented languages or in applications
themselves.")
    (license gpl2+)))

(define-public nettle-3
  ;; This version is not API-compatible with version 2.  In particular GnuTLS
  ;; cannot use it yet.  So keep it separate.
  (package (inherit nettle)
    (version "3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/nettle/nettle-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "04yrpjz33vrj6j0zxc153b00f93i8hs41syr1ryp7sr64fyw0lcn"))))))
