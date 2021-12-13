;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages flex)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages man)
  #:use-module (gnu packages bison)
  #:use-module (srfi srfi-1))

(define-public flex
  (package
    (name "flex")
    (version "2.6.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/westes/flex"
                    "/releases/download/v" version "/"
                    "flex-" version ".tar.gz"))
              (sha256
               (base32
                "15g9bv236nzi665p9ggqjlfn4dwck5835vf0bbw2cz7h5c1swyp8"))))
    (build-system gnu-build-system)
    (inputs
     (let ((bison-for-tests
            (package
              (inherit bison)
              (arguments
               ;; Disable tests, since they require flex.
               (substitute-keyword-arguments (package-arguments bison)
                 ((#:tests? _ #f) #f)))
              (inputs (alist-delete "flex" (package-inputs bison))))))
       `(("bison" ,bison-for-tests))))
    ;; m4 is not present in PATH when cross-building
    (native-inputs
     (list help2man m4))
    (propagated-inputs (list m4))
    (home-page "https://github.com/westes/flex")
    (synopsis "Fast lexical analyser generator")
    (description
     "Flex is a tool for generating scanners.  A scanner, sometimes
called a tokenizer, is a program which recognizes lexical patterns in
text.  The flex program reads user-specified input files, or its standard
input if no file names are given, for a description of a scanner to
generate.  The description is in the form of pairs of regular expressions
and C code, called rules.  Flex generates a C source file named,
\"lex.yy.c\", which defines the function yylex().  The file \"lex.yy.c\"
can be compiled and linked to produce an executable.  When the executable
is run, it analyzes its input for occurrences of text matching the
regular expressions for each rule.  Whenever it finds a match, it
executes the corresponding C code.")
    (license (non-copyleft "file://COPYING"
                           "See COPYING in the distribution."))))
