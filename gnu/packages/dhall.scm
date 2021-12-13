;;; Copyright © 2020 John Soo <jsoo1@asu.edu>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages dhall)
  #:use-module (gnu packages)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-crypto)
  #:use-module (gnu packages haskell-web)
  #:use-module (guix download)
  #:use-module (guix build-system haskell)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public dhall
  (package
    (name "dhall")
    (version "1.39.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/dhall/dhall-"
                           version ".tar.gz"))
       (sha256
        (base32 "1by2d84fbckspczddl4npfsf89q6nprmbg0i5g8yr1psp0fpl4ab"))))
    (build-system haskell-build-system)
    (inputs
     (list ghc-aeson
           ghc-aeson-pretty
           ghc-ansi-terminal
           ghc-atomic-write-0.2.0.7
           ghc-case-insensitive
           ghc-cborg
           ghc-cborg-json
           ghc-contravariant
           ghc-data-fix
           ghc-diff
           ghc-dotgen
           ghc-either
           ghc-exceptions
           ghc-half
           ghc-hashable
           ghc-lens-family-core
           ghc-megaparsec
           ghc-memory
           ghc-mmorph
           ghc-network-uri
           ghc-optparse-applicative
           ghc-parsers
           ghc-parser-combinators
           ghc-prettyprinter
           ghc-prettyprinter-ansi-terminal
           ghc-pretty-simple
           ghc-profunctors
           ghc-pretty-simple
           ghc-repline
           ghc-serialise
           ghc-scientific
           ghc-text-manipulate
           ghc-th-lift-instances
           ghc-transformers-compat
           ghc-unordered-containers
           ghc-uri-encode
           ghc-vector
           ghc-cryptonite
           ghc-http-types
           ghc-http-client
           ghc-http-client-tls))
    (native-inputs
     (list ghc-foldl
           ghc-generic-random-1.3.0.1
           ghc-quickcheck
           ghc-quickcheck-instances
           ghc-semigroups
           ghc-special-values
           ghc-spoon
           ghc-tasty
           ghc-tasty-expected-failure
           ghc-tasty-hunit
           ghc-tasty-quickcheck
           ghc-tasty-silver
           ghc-turtle
           ghc-mockery
           ghc-doctest))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-network-tests
           (lambda _
             (with-directory-excursion "dhall-lang/tests"
               (for-each
                delete-file
                '("import/success/customHeadersA.dhall"
                  "import/success/noHeaderForwardingA.dhall"
                  "import/success/unit/RemoteAsTextA.dhall"
                  "import/success/unit/SimpleRemoteA.dhall"
                  "import/success/unit/asLocation/RemoteChain1A.dhall"
                  "import/success/unit/asLocation/RemoteChain2A.dhall"
                  "import/success/unit/asLocation/RemoteChain3A.dhall"
                  "import/success/unit/asLocation/RemoteChainEnvA.dhall"
                  "import/success/unit/asLocation/RemoteChainMissingA.dhall"
                  "type-inference/success/CacheImportsA.dhall"
                  "type-inference/success/CacheImportsCanonicalizeA.dhall")))
             (substitute* "src/Dhall/Tutorial.hs"
               (((string-append
                  "-- >>> input auto "
                  "\"https://raw.githubusercontent.com/dhall-lang"
                  "/dhall-haskell/18e4e9a18dc53271146df3ccf5b4177c3552236b/"
                  "examples/True\" :: IO Bool"))
                "")
               (((string-append
                  "-- >>> input auto "
                  "\"False == "
                  "https://raw.githubusercontent.com/dhall-lang"
                  "/dhall-haskell/18e4e9a18dc53271146df3ccf5b4177c3552236b/"
                  "examples/True\" :: IO Bool"))
                ""))
             #t)))))
    (home-page "https://dhall-lang.org/")
    (synopsis "Configuration language guaranteed to terminate")
    (description
     "Dhall is an explicitly typed configuration language that is not Turing
complete.  Despite being Turing incomplete, Dhall is a real programming
language with a type-checker and evaluator.

Use this library to parse, type-check, evaluate, and pretty-print the Dhall
configuration language.  This package also includes an executable which
type-checks a Dhall file and reduces the file to a fully evaluated normal
form.")
    (license license:bsd-3)))
