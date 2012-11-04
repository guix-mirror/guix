;;; Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
;;; Copyright (C) 2012 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of Guix.
;;;
;;; Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (distro packages libtool)
  #:use-module (distro)
  #:use-module (distro packages m4)
  #:use-module (distro packages perl)
  #:use-module (guix packages)
  #:use-module (guix http)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu))

(define-public libtool
  (package
   (name "libtool")
   (version "2.4.2")
   (source (origin
            (method http-fetch)
            (uri (string-append "http://ftp.gnu.org/gnu/libtool/libtool-"
                                version ".tar.gz"))
            (sha256
             (base32
              "0649qfpzkswgcj9vqkkr9rn4nlcx80faxpyqscy2k1x9c94f93dk"))))
   (build-system gnu-build-system)
   (native-inputs `(("m4" ,m4)
                    ("perl" ,perl)))
   (arguments
    ;; TODO: Use `TESTSUITEFLAGS=-jN' for tests.
    `(#:patches (list (assoc-ref %build-inputs "patch/skip-tests"))))
   (inputs `(("patch/skip-tests"
              ,(search-patch "libtool-skip-tests.patch"))))
   (synopsis "GNU Libtool, a generic library support script")
   (description
    "GNU libtool is a generic library support script.  Libtool hides the
complexity of using shared libraries behind a consistent, portable interface.

To use libtool, add the new generic library building commands to your
Makefile, Makefile.in, or Makefile.am.  See the documentation for
details.")
   (license "GPLv3+")
   (home-page "http://www.gnu.org/software/libtool/")))
