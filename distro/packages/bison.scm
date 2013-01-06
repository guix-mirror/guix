;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Ludovic Courtès <ludo@gnu.org>
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

(define-module (distro packages bison)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (distro packages m4)
  #:use-module (distro packages perl))

(define-public bison
  (package
    (name "bison")
    (version "2.6.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/bison/bison-"
                          version ".tar.xz"))
      (sha256
       (base32
        "0y9svfkbw8jc8yv280hqzilpvlwg60gayck83jj98djmzaxr1w86"))))
    (build-system gnu-build-system)
    (inputs `(("perl" ,perl)))
    (propagated-inputs `(("m4" ,m4)))
    (home-page "http://www.gnu.org/software/bison/")
    (synopsis
     "GNU Bison, a Yacc-compatible parser generator")
    (description
     "Bison is a general-purpose parser generator that converts an
annotated context-free grammar into an LALR(1) or GLR parser for
that grammar.  Once you are proficient with Bison, you can use
it to develop a wide range of language parsers, from those used
in simple desk calculators to complex programming languages.

Bison is upward compatible with Yacc: all properly-written Yacc
grammars ought to work with Bison with no change.  Anyone
familiar with Yacc should be able to use Bison with little
trouble.  You need to be fluent in C or C++ programming in order
to use Bison.")
    (license gpl3+)))
