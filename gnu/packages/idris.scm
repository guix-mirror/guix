;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2016, 2017 David Craven <david@craven.ch>
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

(define-module (gnu packages idris)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (guix build-system haskell)
  #:use-module (guix build-system idris)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public idris
  (package
    (name "idris")
    (version "0.99")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://hackage.haskell.org/package/"
                    "idris-" version "/idris-" version ".tar.gz"))
              (sha256
               (base32
                "1sd4vy5rx0mp32xj99qijhknkgw4d2rxvz6wiy3pym6kaqmc497i"))))
    (build-system haskell-build-system)
    (inputs
     `(("gmp" ,gmp)
       ("ncurses" ,ncurses)
       ("ghc-aeson" ,ghc-aeson)
       ("ghc-async" ,ghc-async)
       ("ghc-annotated-wl-pprint" ,ghc-annotated-wl-pprint)
       ("ghc-ansi-terminal" ,ghc-ansi-terminal)
       ("ghc-ansi-wl-pprint" ,ghc-ansi-wl-pprint)
       ("ghc-base64-bytestring" ,ghc-base64-bytestring)
       ("ghc-blaze-html" ,ghc-blaze-html)
       ("ghc-blaze-markup" ,ghc-blaze-markup)
       ("ghc-cheapskate" ,ghc-cheapskate)
       ("ghc-fingertree" ,ghc-fingertree)
       ("ghc-fsnotify" ,ghc-fsnotify)
       ("ghc-ieee754" ,ghc-ieee754)
       ("ghc-mtl" ,ghc-mtl)
       ("ghc-network" ,ghc-network)
       ("ghc-optparse-applicative" ,ghc-optparse-applicative)
       ("ghc-parsers" ,ghc-parsers)
       ("ghc-regex-tdfa" ,ghc-regex-tdfa)
       ("ghc-safe" ,ghc-safe)
       ("ghc-split" ,ghc-split)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-golden" ,ghc-tasty-golden)
       ("ghc-tasty-rerun" ,ghc-tasty-rerun)
       ("ghc-terminal-size" ,ghc-terminal-size)
       ("ghc-text" ,ghc-text)
       ("ghc-trifecta" ,ghc-trifecta)
       ("ghc-uniplate" ,ghc-uniplate)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-utf8-string" ,ghc-utf8-string)
       ("ghc-vector-binary-instances" ,ghc-vector-binary-instances)
       ("ghc-vector" ,ghc-vector)
       ("ghc-zip-archive" ,ghc-zip-archive)
       ("ghc-zlib" ,ghc-zlib)))
    (arguments
     `(#:tests? #f ; FIXME: Test suite doesn't run in a sandbox.
       #:configure-flags
       (list (string-append "--datasubdir="
                            (assoc-ref %outputs "out") "/lib/idris"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-cc-command
           (lambda _
             (setenv "CC" "gcc")
             #t))
         (add-after 'install 'fix-libs-install-location
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib/idris"))
                    (modules (string-append lib "/libs")))
               (for-each
                (lambda (module)
                  (symlink (string-append modules "/" module)
                           (string-append lib "/" module)))
                '("prelude" "base" "contrib" "effects" "pruviloj"))))))))
    (native-search-paths
     (list (search-path-specification
            (variable "IDRIS_LIBRARY_PATH")
            (files '("lib/idris")))))
    (home-page "http://www.idris-lang.org")
    (synopsis "General purpose language with full dependent types")
    (description "Idris is a general purpose language with full dependent
types.  It is compiled, with eager evaluation.  Dependent types allow types to
be predicated on values, meaning that some aspects of a program's behaviour
can be specified precisely in the type.  The language is closely related to
Epigram and Agda.")
    (license license:bsd-3)))
