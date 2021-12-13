;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Robert Vollmert <rob@vllmrt.net>
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

(define-module (gnu packages elm)
  #:use-module (gnu packages)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-crypto)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages haskell-web)
  #:use-module (guix build-system haskell)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

;; The full elm build calls out to itself via Template Haskell to
;; compile the elm reactor web app. elm reactor isn't required to
;; compile elm applications, so we take this part out of this
;; bootstrap package.
(define-public elm-compiler
  (package
    (name "elm-compiler")
    (version "0.19.0")
    (source
     (origin
       (method git-fetch)
       (file-name (git-file-name name version))
       (uri (git-reference
             (url "https://github.com/elm/compiler/")
             (commit version)))
       (sha256
        (base32 "0s93z9vr0vp5w894ghc5s34nsq09sg1msf59zfiba87sid5vgjqy"))
       (patches
        (search-patches "elm-compiler-disable-reactor.patch"
                        "elm-compiler-fix-map-key.patch"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'update-constraints
           (lambda _
             (substitute* "elm.cabal"
               (("(ansi-terminal|containers|network|http-client|language-glsl)\\s+[^,]+" all dep)
                dep)))))))
    (inputs
     (list ghc-ansi-terminal
           ghc-ansi-wl-pprint
           ghc-edit-distance
           ghc-file-embed
           ghc-http
           ghc-http-client
           ghc-http-client-tls
           ghc-http-types
           ghc-language-glsl
           ghc-logict
           ghc-network
           ghc-raw-strings-qq
           ghc-scientific
           ghc-sha
           ghc-snap-core
           ghc-snap-server
           ghc-unordered-containers
           ghc-utf8-string
           ghc-vector
           ghc-zip-archive))
    (home-page "https://elm-lang.org")
    (synopsis "Programming language for Web applications")
    (description
     "This package provides Elm, a statically-typed functional programming
language for the browser.  It includes commands for developers such as
@command{elm make} and @command{elm repl}.")
    (license license:bsd-3)))
