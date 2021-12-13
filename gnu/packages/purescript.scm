;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 John Soo <jsoo1@asu.edu>
;;; Copyright © 2020 Bonface Munyoki Kilyungi <bonfacemunyoki@gmail.com>
;;; Copyright © 2021 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages purescript)
  #:use-module (gnu packages)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-crypto)
  #:use-module (gnu packages haskell-web)
  #:use-module ((gnu packages python) #:select (python))
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system haskell)
  #:use-module ((guix licenses) #:prefix license:))

(define-public purescript
  (package
    (name "purescript")
    (version "0.14.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/purescript/purescript-"
             version
             ".tar.gz"))
       (sha256
        (base32 "06f318hdah076vkviw1ryyg2p0gpbabsp8lbm5x03f2qv92n9j1n"))))
    (build-system haskell-build-system)
    (inputs
     (list ghc-glob
           ghc-aeson
           ghc-aeson-better-errors
           ghc-aeson-pretty
           ghc-ansi-terminal
           ghc-base-compat
           ghc-blaze-html
           ghc-bower-json
           ghc-boxes
           ghc-cborg
           ghc-cheapskate
           ghc-clock
           ghc-cryptonite
           ghc-data-ordlist
           ghc-dlist
           ghc-edit-distance
           ghc-file-embed
           ghc-fsnotify
           ghc-happy
           ghc-language-javascript
           ghc-lifted-async
           ghc-lifted-base
           ghc-memory
           ghc-microlens-platform
           ghc-monad-control
           ghc-monad-logger
           ghc-network
           ghc-parallel
           ghc-pattern-arrows
           ghc-protolude
           ghc-purescript-cst
           ghc-regex-tdfa
           ghc-safe
           ghc-scientific
           ghc-semialign
           ghc-semigroups
           ghc-serialise
           ghc-sourcemap
           ghc-split
           ghc-stringsearch
           ghc-syb
           ghc-these
           ghc-transformers-base
           ghc-transformers-compat
           ghc-unordered-containers
           ghc-utf8-string
           ghc-vector
           ghc-ansi-wl-pprint
           ghc-http-types
           ghc-network
           ghc-optparse-applicative-0.15.1.0 ; XXX: needs specific version
           ghc-wai
           ghc-wai-websockets
           ghc-warp
           ghc-websockets))
    (native-inputs
     (list ghc-happy
           ghc-hunit
           ghc-hspec
           hspec-discover
           ghc-tasty
           ghc-tasty-golden
           ghc-tasty-hspec))
    (arguments
     `(;; Tests require npm
       #:tests? #f
       #:configure-flags '("--flags=release")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'update-constraints
           (lambda _
             (substitute* "purescript.cabal"
               (("\\b(ansi-terminal|cryptonite|dlist|language-javascript)\\s+[^,]+" all dep)
                dep)))))))
    (home-page "https://www.purescript.org/")
    (synopsis "Haskell inspired programming language compiling to JavaScript")
    (description
     "Purescript is a small strongly, statically typed programming language with
expressive types, inspired by Haskell and compiling to JavaScript.")
    (license license:bsd-3)))

(define-public ghc-purescript-cst
  (package
    (name "ghc-purescript-cst")
    (version "0.4.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/purescript-cst/purescript-cst-"
               version
               ".tar.gz"))
        (sha256
          (base32 "0r3f5lr9lrv9wpgkwj6nyl42lvxryj2lvr1w7ld4gki8ylq24n8g"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'update-constraints
           (lambda _
             (substitute* "purescript-cst.cabal"
               (("\\b(dlist)\\s+[^,]+" all dep)
                dep)))))))
    (inputs
      (list ghc-aeson
            ghc-base-compat
            ghc-dlist
            ghc-microlens
            ghc-protolude
            ghc-scientific
            ghc-semigroups
            ghc-serialise
            ghc-vector))
    (native-inputs (list ghc-happy))
    (home-page "https://www.purescript.org/")
    (synopsis "PureScript Programming Language Concrete Syntax Tree")
    (description
     "This package implements parser for the PureScript programming language.")
    (license license:bsd-3)))

(define-public ghc-optparse-applicative-0.15.1.0
  (package
    (inherit ghc-optparse-applicative)
    (name "ghc-optparse-applicative")
    (version "0.15.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/optparse-applicative/optparse-applicative-"
               version
               ".tar.gz"))
        (sha256
          (base32 "1ws6y3b3f6hsgv0ff0yp6lw4hba1rps4dnvry3yllng0s5gngcsd"))))
    (inputs
      (list ghc-transformers-compat ghc-ansi-wl-pprint))
    (native-inputs (list ghc-quickcheck))
    (arguments
      `(#:cabal-revision
        ("1" "0zmhqkd96v2z1ilhqdkd9z4jgsnsxb8yi2479ind8m5zm9363zr9")))))
