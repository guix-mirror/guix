;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2019 Robert Vollmert <rob@vllmrt.net>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
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
  #:use-module (guix import cabal)
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

(define test-cabal-2
  "name: foo
version: 1.0.0
homepage: http://test.org
synopsis: synopsis
description: description
license: BSD3
executable cabal {
build-depends:
  HTTP       >= 4000.2.5 && < 4000.3,
  mtl        >= 2.0      && < 3
}
")

;; Check compiler implementation test with and without spaces.
(define test-cabal-3
  "name: foo
version: 1.0.0
homepage: http://test.org
synopsis: synopsis
description: description
license: BSD3
library
  if impl(ghc >= 7.2 && < 7.6)
    Build-depends: ghc-a
  if impl(ghc>=7.2&&<7.6)
    Build-depends: ghc-b
  if impl(ghc == 7.8)
    Build-depends: 
      HTTP       >= 4000.2.5 && < 4000.3,
      mtl        >= 2.0      && < 3
")

;; Check "-any", "-none" when name is different.
(define test-cabal-4
  "name: foo
version: 1.0.0
homepage: http://test.org
synopsis: synopsis
description: description
license: BSD3
library
  if impl(ghcjs -any)
    Build-depends: ghc-a
  if impl(ghc>=7.2&&<7.6)
    Build-depends: ghc-b
  if impl(ghc == 7.8)
    Build-depends: 
      HTTP       >= 4000.2.5 && < 4000.3,
      mtl        >= 2.0      && < 3
")

;; Check "-any", "-none".
(define test-cabal-5
  "name: foo
version: 1.0.0
homepage: http://test.org
synopsis: synopsis
description: description
license: BSD3
library
  if impl(ghc == 7.8)
    Build-depends: 
      HTTP       >= 4000.2.5 && < 4000.3,
  if impl(ghc -any)
    Build-depends: mtl        >= 2.0      && < 3
  if impl(ghc>=7.2&&<7.6)
    Build-depends: ghc-b
")

;; Check "custom-setup".
(define test-cabal-6
  "name: foo
build-type: Custom
version: 1.0.0
homepage: http://test.org
synopsis: synopsis
description: description
license: BSD3
custom-setup
  setup-depends: base >= 4.7 && < 5,
                 Cabal >= 1.24,
                 haskell-gi == 0.21.*
library
  if impl(ghc>=7.2&&<7.6)
    Build-depends: ghc-b
  if impl(ghc == 7.8)
    Build-depends: 
      HTTP       >= 4000.2.5 && < 4000.3,
      mtl        >= 2.0      && < 3
")

;; A fragment of a real Cabal file with minor modification to check precedence
;; of 'and' over 'or', missing final newline, spaces between keywords and
;; parentheses and between key and column.
(define test-read-cabal-1
  "name: test-me
library
  -- Choose which library versions to use.
  if flag(base4point8)
    Build-depends: base >= 4.8 && < 5
  else
    if flag(base4)
      Build-depends: base >= 4 && < 4.8
    else
      if flag(base3)
        Build-depends: base >= 3 && < 4
      else
        Build-depends: base < 3
  if flag(base4point8) || flag (base4) && flag(base3)
    Build-depends: random
  Build-depends : containers

  -- Modules that are always built.
  Exposed-Modules:
    Test.QuickCheck.Exception")

(test-begin "hackage")

(define-syntax-rule (define-package-matcher name pattern)
  (define* (name obj)
    (match obj
      (pattern #t)
      (x       (pk 'fail x #f)))))

(define-package-matcher match-ghc-foo
  ('package
    ('name "ghc-foo")
    ('version "1.0.0")
    ('source
     ('origin
       ('method 'url-fetch)
       ('uri ('hackage-uri "foo" 'version))
       ('sha256
        ('base32
         (? string? hash)))))
    ('build-system 'haskell-build-system)
    ('inputs ('list 'ghc-http))
    ('home-page "http://test.org")
    ('synopsis (? string?))
    ('description (? string?))
    ('license 'license:bsd-3)))

(define* (eval-test-with-cabal test-cabal matcher #:key (cabal-environment '()))
  (define port (open-input-string test-cabal))
  (matcher (hackage->guix-package "foo" #:port port #:cabal-environment cabal-environment)))

(test-assert "hackage->guix-package test 1"
  (eval-test-with-cabal test-cabal-1 match-ghc-foo))

(test-assert "hackage->guix-package test 2"
  (eval-test-with-cabal test-cabal-2 match-ghc-foo))

(test-assert "hackage->guix-package test 3"
  (eval-test-with-cabal test-cabal-3 match-ghc-foo
                        #:cabal-environment '(("impl" . "ghc-7.8"))))

(test-assert "hackage->guix-package test 4"
  (eval-test-with-cabal test-cabal-4 match-ghc-foo
                        #:cabal-environment '(("impl" . "ghc-7.8"))))

(test-assert "hackage->guix-package test 5"
  (eval-test-with-cabal test-cabal-5 match-ghc-foo
                        #:cabal-environment '(("impl" . "ghc-7.8"))))

(define-package-matcher match-ghc-foo-6
  ('package
    ('name "ghc-foo")
    ('version "1.0.0")
    ('source
     ('origin
       ('method 'url-fetch)
       ('uri ('hackage-uri "foo" 'version))
       ('sha256
        ('base32
         (? string? hash)))))
    ('build-system 'haskell-build-system)
    ('inputs ('list 'ghc-b 'ghc-http))
    ('native-inputs ('list 'ghc-haskell-gi))
    ('home-page "http://test.org")
    ('synopsis (? string?))
    ('description (? string?))
    ('license 'license:bsd-3)))

(test-assert "hackage->guix-package test 6"
  (eval-test-with-cabal test-cabal-6 match-ghc-foo-6))

;; Check multi-line layouted description.
(define test-cabal-multiline-layout
  "name: foo
version: 1.0.0
homepage: http://test.org
synopsis: synopsis
description:   first line
               second line
license: BSD3
executable cabal
  build-depends:
    HTTP       >= 4000.2.5 && < 4000.3,
    mtl        >= 2.0      && < 3
")

(test-assert "hackage->guix-package test multiline desc (layout)"
  (eval-test-with-cabal test-cabal-multiline-layout match-ghc-foo))

;; Check multi-line braced description.
(define test-cabal-multiline-braced
  "name: foo
version: 1.0.0
homepage: http://test.org
synopsis: synopsis
description: {
first line
second line
}
license: BSD3
executable cabal
  build-depends:
    HTTP       >= 4000.2.5 && < 4000.3,
    mtl        >= 2.0      && < 3
")

(test-assert "hackage->guix-package test multiline desc (braced)"
  (eval-test-with-cabal test-cabal-multiline-braced match-ghc-foo))

;; Check mixed layout. Compare e.g. warp.
(define test-cabal-mixed-layout
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
  ghc-options: -Wall
")

;; Fails: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=35743
(test-expect-fail 1)
(test-assert "hackage->guix-package test mixed layout"
  (eval-test-with-cabal test-cabal-mixed-layout match-ghc-foo))

;; Check flag executable. Compare e.g. darcs.
(define test-cabal-flag-executable
  "name: foo
version: 1.0.0
homepage: http://test.org
synopsis: synopsis
description: description
license: BSD3
flag executable
  description: Build executable
  default:     True
executable cabal
  if !flag(executable)
    buildable: False
  else
    buildable: True

  build-depends:
    HTTP       >= 4000.2.5 && < 4000.3,
    mtl        >= 2.0      && < 3
")

(test-assert "hackage->guix-package test flag executable"
  (eval-test-with-cabal test-cabal-flag-executable match-ghc-foo))

;; Check Hackage Cabal revisions.
(define test-cabal-revision
  "name: foo
version: 1.0.0
x-revision: 2
homepage: http://test.org
synopsis: synopsis
description: description
license: BSD3
executable cabal
  build-depends:
    HTTP       >= 4000.2.5 && < 4000.3,
    mtl        >= 2.0      && < 3
")

(define-package-matcher match-ghc-foo-revision
  ('package
    ('name "ghc-foo")
    ('version "1.0.0")
    ('source
     ('origin
       ('method 'url-fetch)
       ('uri ('hackage-uri "foo" 'version))
       ('sha256
        ('base32
         (? string? hash)))))
    ('build-system 'haskell-build-system)
    ('inputs ('list 'ghc-http))
    ('arguments
     ('quasiquote
      ('#:cabal-revision
       ("2" "0xxd88fb659f0krljidbvvmkh9ppjnx83j0nqzx8whcg4n5qbyng"))))
    ('home-page "http://test.org")
    ('synopsis (? string?))
    ('description (? string?))
    ('license 'license:bsd-3)))

(test-assert "hackage->guix-package test cabal revision"
  (eval-test-with-cabal test-cabal-revision match-ghc-foo-revision))

(test-assert "read-cabal test 1"
  (match (call-with-input-string test-read-cabal-1 read-cabal)
    ((("name" ("test-me"))
      ('section 'library
                (('if ('flag "base4point8")
                      (("build-depends" ("base >= 4.8 && < 5")))
                      (('if ('flag "base4")
                            (("build-depends" ("base >= 4 && < 4.8")))
                            (('if ('flag "base3")
                                  (("build-depends" ("base >= 3 && < 4")))
                                  (("build-depends" ("base < 3"))))))))
                 ('if ('or ('flag "base4point8")
                           ('and ('flag "base4") ('flag "base3")))
                      (("build-depends" ("random")))
                      ())
                 ("build-depends" ("containers"))
                 ("exposed-modules" ("Test.QuickCheck.Exception")))))
     #t)
    (x (pk 'fail x #f))))

(define test-cabal-import
  "name: foo
version: 1.0.0
homepage: http://test.org
synopsis: synopsis
description: description
license: BSD3
common commons
  build-depends:
    HTTP       >= 4000.2.5 && < 4000.3,
    mtl        >= 2.0      && < 3

executable cabal
  import: commons
")

(define-package-matcher match-ghc-foo-import
  ('package
    ('name "ghc-foo")
    ('version "1.0.0")
    ('source
     ('origin
       ('method 'url-fetch)
       ('uri ('hackage-uri "foo" 'version))
       ('sha256
        ('base32
         (? string? hash)))))
    ('build-system 'haskell-build-system)
    ('inputs ('list 'ghc-http))
    ('home-page "http://test.org")
    ('synopsis (? string?))
    ('description (? string?))
    ('license 'license:bsd-3)))

(test-assert "hackage->guix-package test cabal import"
  (eval-test-with-cabal test-cabal-import match-ghc-foo-import))

(test-end "hackage")
