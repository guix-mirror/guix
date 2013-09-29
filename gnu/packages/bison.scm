;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages bison)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages flex)
  #:use-module (srfi srfi-1)
  #:export (bison))

(define bison
  (package
    (name "bison")
    (version "3.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/bison/bison-"
                          version ".tar.xz"))
      (sha256
       (base32
        "1j14fqgi9wzqgsy4fhkcdrv4hv6rrvhvn84axs520w9b022mbb79"))))
    (build-system gnu-build-system)
    (native-inputs `(("perl" ,perl)))
    (inputs `(("flex" ,flex)))
    (propagated-inputs `(("m4" ,m4)))
    (home-page "http://www.gnu.org/software/bison/")
    (synopsis "Parser generator")
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
