;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
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

(define-module (test-hackage)
  #:use-module (guix import hackage)
  #:use-module (guix tests)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match))

(define test-cabal-1
  "name: foo
version: 1.0.0
homepage: http://test.org
synopsis: synopsis
description: description
license: BSD3
executable cabal
  build-depends:
    HTTP       >= 4000.2.5 && < 4000.3,
    mtl        >= 2.0      && < 3
")

;; Use TABs to indent lines and to separate keys from value.
(define test-cabal-2
  "name:	foo
version:	1.0.0
homepage:	http://test.org
synopsis:	synopsis
description:	description
license:	BSD3
executable cabal
	build-depends:	HTTP       >= 4000.2.5 && < 4000.3,
		mtl        >= 2.0      && < 3
")

;; Use indentation with comma as found, e.g., in 'haddock-api'.
(define test-cabal-3
  "name: foo
version: 1.0.0
homepage: http://test.org
synopsis: synopsis
description: description
license: BSD3
executable cabal
    build-depends:
        HTTP       >= 4000.2.5 && < 4000.3
      , mtl        >= 2.0      && < 3
")

(define test-cond-1
  "(os(darwin) || !(flag(debug))) && flag(cips)")

(define read-cabal
  (@@ (guix import hackage) read-cabal))

(define eval-cabal-keywords
  (@@ (guix import hackage) eval-cabal-keywords))

(define conditional->sexp-like
  (@@ (guix import hackage) conditional->sexp-like))

(test-begin "hackage")

(define (eval-test-with-cabal test-cabal)
  (mock
   ((guix import hackage) hackage-fetch
    (lambda (name-version)
      (call-with-input-string test-cabal
        read-cabal)))
   (match (hackage->guix-package "foo")
     (('package
        ('name "ghc-foo")
        ('version "1.0.0")
        ('source
         ('origin
           ('method 'url-fetch)
           ('uri ('string-append
                  "http://hackage.haskell.org/package/foo/foo-"
                  'version
                  ".tar.gz"))
           ('sha256
            ('base32
             (? string? hash)))))
        ('build-system 'haskell-build-system)
        ('inputs
         ('quasiquote
          (("ghc-http" ('unquote 'ghc-http))
           ("ghc-mtl" ('unquote 'ghc-mtl)))))
        ('home-page "http://test.org")
        ('synopsis (? string?))
        ('description (? string?))
        ('license 'bsd-3))
      #t)
     (x
      (pk 'fail x #f)))))

(test-assert "hackage->guix-package test 1"
  (eval-test-with-cabal test-cabal-1))

(test-assert "hackage->guix-package test 2"
  (eval-test-with-cabal test-cabal-2))

(test-assert "hackage->guix-package test 3"
  (eval-test-with-cabal test-cabal-3))

(test-assert "conditional->sexp-like"
  (match
    (eval-cabal-keywords
     (conditional->sexp-like test-cond-1)
     '(("debug" . "False")))
    (('and ('or ('string-match "darwin" ('%current-system)) ('not '#f)) '#t)
     #t)
    (x
     (pk 'fail x #f))))

(test-end "hackage")


(exit (= (test-runner-fail-count (test-runner-current)) 0))
