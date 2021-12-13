;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages pure)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages multiprecision))

(define-public pure
  (package
    (name "pure")
    (version "0.68")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/agraef/pure-lang/releases/"
                           "download/pure-" version "/"
                           "pure-" version ".tar.gz"))
       (sha256
        (base32
         "0px6x5ivcdbbp2pz5n1r1cwg1syadklhjw8piqhl63n91i4r7iyb"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "LDFLAGS=-Wl,-rpath="
                                         (assoc-ref %outputs "out")
                                         "/lib"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-llvm-lookup
           (lambda _
             (substitute* "configure"
               (("-lLLVM-[$][{]llvm_version[}]")
                "`$LLVMCONF --libs`"))
             #t)))))
    (inputs
     (list gmp llvm-3.5 mpfr))
    (home-page "https://agraef.github.io/pure-lang/")
    (synopsis "Pure programming Language")
    (description "@code{pure} is a programming language based on term
rewriting.  It offers equational definitions with pattern matching,
full symbolic rewriting capabilities, dynamic typing, eager and lazy
evaluation, lexical closures, built-in list and matrix support and
a C interface.")
    (license license:gpl3+)))
