;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015, 2016, 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2017 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2017 rsiddharth <s@ricketyspace.net>
;;; Copyright © 2017, 2018, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Tonton <tonton@riseup.net>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2019 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2020 John Soo <jsoo1@asu.edu>
;;; Copyright © 2020 Carlo Holl <carloholl@gmail.com>
;;; Copyright © 2021 John Kehayias <john.kehayias@protonmail.com>
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

(define-module (gnu packages haskell-check)
  #:use-module (gnu packages)
  #:use-module (gnu packages haskell-crypto)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (guix build-system haskell)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public ghc-tasty-ant-xml
  (package
    (name "ghc-tasty-ant-xml")
    (version "1.1.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/tasty-ant-xml/tasty-ant-xml-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0h9mllhw9cd0rn34xhj8grwmbny7z7hpd8qmp9lfcdj0s4qx9vx8"))))
    (build-system haskell-build-system)
    (inputs
     (list ghc-generic-deriving ghc-xml ghc-tagged ghc-tasty))
    (home-page
     "https://github.com/ocharles/tasty-ant-xml")
    (synopsis
     "Render tasty output to XML for Jenkins")
    (description
     "A tasty ingredient to output test results in XML, using the Ant
schema.  This XML can be consumed by the Jenkins continuous integration
framework.")
    (license license:bsd-3)))

(define-public ghc-tasty-smallcheck
  (package
    (name "ghc-tasty-smallcheck")
    (version "0.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/tasty-smallcheck/tasty-smallcheck-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0csgwn3vch0jnpqyyfnrfjq4z0dpl67imh5a7byll3hhlyidgjym"))))
    (build-system haskell-build-system)
    (inputs
     (list ghc-tasty ghc-smallcheck ghc-async ghc-tagged))
    (home-page "https://documentup.com/feuerbach/tasty")
    (synopsis "SmallCheck support for the Tasty test framework")
    (description "This package provides SmallCheck support for the Tasty
Haskell test framework.")
    (license license:bsd-3)))

(define-public ghc-tasty-quickcheck
  (package
    (name "ghc-tasty-quickcheck")
    (version "0.10.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/tasty-quickcheck/"
             "tasty-quickcheck-" version ".tar.gz"))
       (sha256
        (base32
         "0i1i78587znqzwps49milyr5n2k388ld2kr9ysz1vw8gcw51qq49"))))
    (build-system haskell-build-system)
    (inputs
     (list ghc-quickcheck
           ghc-tagged
           ghc-tasty
           ghc-random
           ghc-ansi-terminal
           ghc-tasty-hunit
           ghc-pcre-light))
    (home-page "http://documentup.com/feuerbach/tasty")
    (synopsis "QuickCheck support for the Tasty test framework")
    (description "This package provides QuickCheck support for the Tasty
Haskell test framework.")
    (license license:expat)))

(define-public ghc-tasty-golden
  (package
    (name "ghc-tasty-golden")
    (version "2.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/tasty-golden/tasty-golden-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1nskavqgfxx1cw7q6c0cmizlwj54rnlv93yhgssaa77gv1nbvwpn"))))
    (build-system haskell-build-system)
    (inputs
     (list ghc-async
           ghc-optparse-applicative
           ghc-tagged
           ghc-tasty
           ghc-temporary
           ghc-unix-compat))
    (native-inputs (list ghc-tasty-hunit))
    (home-page
     "https://github.com/feuerbach/tasty-golden")
    (synopsis "Golden tests support for tasty")
    (description
     "This package provides support for @code{golden testing}.  A @dfn{golden
test} is an IO action that writes its result to a file.  To pass the test, this
output file should be identical to the corresponding @code{golden} file, which
contains the correct result for the test.")
    (license license:expat)))

(define-public ghc-tasty
  (package
    (name "ghc-tasty")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/tasty/tasty-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0574hbqzxzyv6vsk5kzbf04kz58y0iy8x9ydcj4b8fpncgmgy63g"))))
    (build-system haskell-build-system)
    (inputs
     (list ghc-tagged
           ghc-regex-tdfa
           ghc-optparse-applicative
           ghc-unbounded-delays
           ghc-async
           ghc-ansi-terminal
           ghc-clock-bootstrap
           ghc-wcwidth-bootstrap))
    (home-page "http://documentup.com/feuerbach/tasty")
    (synopsis "Modern and extensible testing framework")
    (description "Tasty is a modern testing framework for Haskell.  It lets
you combine your unit tests, golden tests, QuickCheck/SmallCheck properties,
and any other types of tests into a single test suite.")
    (license license:expat)))

(define-public ghc-tasty-hedgehog
  (package
    (name "ghc-tasty-hedgehog")
    (version "1.1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "tasty-hedgehog/tasty-hedgehog-" version ".tar.gz"))
       (sha256
        (base32
         "0cy49z8n124xh2ra2482vfy5if1n6d9lbdjma2zg1mxfj0k0zyfb"))))
    (build-system haskell-build-system)
    (inputs
     (list ghc-tagged ghc-tasty ghc-hedgehog))
    (native-inputs
     (list ghc-tasty-expected-failure))
    (home-page "https://github.com/qfpl/tasty-hedgehog")
    (synopsis "Integration for tasty and hedgehog")
    (description "This package provides the means for integrating the
@url{https://hackage.haskell.org/package/hedgehog, hedgehog testing library}
with the @url{https://hackage.haskell.org/package/tasty, tasty testing
framework}.")
    (license license:bsd-3)))

(define-public ghc-tasty-hspec
  (package
    (name "ghc-tasty-hspec")
    (version "1.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/tasty-hspec/tasty-hspec-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "02s82ijs2ringqxsqbm7m3vcy5brmwxa617azxv0v2phi3rdkjvl"))))
    (build-system haskell-build-system)
    (inputs
     (list ghc-hspec
           ghc-hspec-core
           ghc-quickcheck
           ghc-tagged
           ghc-tasty
           ghc-tasty-smallcheck
           ghc-tasty-quickcheck))
    (arguments
     `(#:cabal-revision
       ("1" "0za15rg0szacxq9yfxxjzddr77ai7ng5827a20pj9dr5anjlnajj")))
    (home-page
     "https://github.com/mitchellwrosen/tasty-hspec")
    (synopsis
     "Hspec support for the Tasty test framework")
    (description
     "This package provides a Tasty provider for Hspec test suites.")
    (license license:bsd-3)))

(define-public ghc-tasty-hunit
  (package
    (name "ghc-tasty-hunit")
    (version "0.10.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/tasty-hunit/tasty-hunit-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0gz6zz3w7s44pymw33xcxnawryl27zk33766sab96nz2xh91kvxp"))))
    (build-system haskell-build-system)
    (inputs
     (list ghc-call-stack-boot ghc-tasty))
    (home-page "http://documentup.com/feuerbach/tasty")
    (synopsis "HUnit support for the Tasty test framework")
    (description "This package provides HUnit support for the Tasty Haskell
test framework.")
    (license license:expat)))

(define-public ghc-tasty-kat
  (package
    (name "ghc-tasty-kat")
    (version "0.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "tasty-kat/tasty-kat-" version ".tar.gz"))
              (sha256
               (base32
                "14yvlpli6cv6bn3kh8mlfp4x1l6ns4fvmfv6hmj75cvxyzq029d7"))))
    (build-system haskell-build-system)
    (inputs
     (list ghc-tasty ghc-tasty-quickcheck ghc-tasty-hunit))
    (home-page "https://github.com/vincenthz/tasty-kat")
    (synopsis "Known Answer Tests (KAT) framework for tasty")
    (description
     "This package provides a @dfn{Known Answer Tests} (KAT) framework for
tasty.")
    (license license:expat)))

(define-public ghc-tasty-lua
  (package
    (name "ghc-tasty-lua")
    (version "0.2.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "tasty-lua/tasty-lua-" version ".tar.gz"))
       (sha256
        (base32
         "0wa73ihkjcxi50lgpdzwwdx7s903lqi79hw7hxlvhbcvdly1cq53"))))
    (build-system haskell-build-system)
    (inputs
     (list ghc-file-embed ghc-hslua ghc-tasty))
    (native-inputs
     (list ghc-tasty-hunit))
    (home-page "https://github.com/hslua/tasty-lua")
    (synopsis "Write tests in Lua, integrate into tasty")
    (description "This package gives users the ability to define tasty tests
from Lua.")
    (license license:expat)))

(define-public ghc-tasty-th
  (package
    (name "ghc-tasty-th")
    (version "0.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/tasty-th/tasty-th-"
             version ".tar.gz"))
       (sha256
        (base32
         "0b2ivrw2257m4cy4rjnkwqlarh83j1y3zywnmaqqqbvy667sqnj3"))))
    (build-system haskell-build-system)
    (inputs
     (list ghc-haskell-src-exts ghc-tasty ghc-tasty-hunit))
    (home-page "https://github.com/bennofs/tasty-th")
    (synopsis "Automatically generate tasty TestTrees")
    (description
      "Tasty-th automatically generates tasty TestTrees from functions of the
current module, using TemplateHaskell.  This is a fork the original
test-framework-th package, modified to work with tasty instead of
test-framework.")
    (license license:bsd-3)))

(define-public ghc-tasty-rerun
  (package
    (name "ghc-tasty-rerun")
    (version "1.1.18")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://hackage.haskell.org/package/tasty-rerun/"
                    "tasty-rerun-" version ".tar.gz"))
              (sha256
               (base32
                "0sccp5zx9v2rx741nbmgd8mzjhy5m4v74hk26d23xz93ph8aqx7s"))))
    (build-system haskell-build-system)
    (inputs
     (list ghc-optparse-applicative ghc-reducers ghc-split ghc-tagged
           ghc-tasty))
    (home-page "https://github.com/ocharles/tasty-rerun")
    (synopsis "Run tests by filtering the test tree")
    (description "This package adds the ability to run tests by filtering the
test tree based on the result of a previous test run.  You can use this to run
only those tests that failed in the last run, or to only run the tests that have
been added since previous test run.")
  (license license:bsd-3)))

(define-public ghc-tasty-expected-failure
  (package
    (name "ghc-tasty-expected-failure")
    (version "0.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "tasty-expected-failure/tasty-expected-failure-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0zlgxs24d54byfhvwdg85xk1572zpjs71bjlxxrxcvralrfcq1yb"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f)) ; TODO: Loops.
;    (native-inputs
;     `(("ghc-tasty-hunit" ,ghc-tasty-hunit)
;       ("ghc-tasty-golden" ,ghc-tasty-golden)
;       ("ghc-hedgehog" ,ghc-hedgehog)
;       ("ghc-tasty-hedgehog" ,ghc-tasty-hedgehog)))
    (inputs
     (list ghc-tagged ghc-tasty ghc-unbounded-delays))
    (home-page "https://github.com/nomeata/tasty-expected-failure")
    (synopsis "Mark tasty tests as failure expected")
    (description
     "With the function @code{Test.Tasty.ExpectedFailure.expectFail} in the
provided module @code{Test.Tasty.ExpectedFailure}, you can mark that you
expect test cases to fail, and not to pass. This can be used for test-driven
development.")
    (license license:expat)))

(define-public ghc-quickcheck-instances
  (package
    (name "ghc-quickcheck-instances")
    (version "0.3.25.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/"
             "quickcheck-instances/quickcheck-instances-"
             version ".tar.gz"))
       (sha256
        (base32
         "0ihqbarl2ddrfgq3mq09lswwn8213qpw13g49qxs5mjkcm6gbk3h"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("2" "1lsa3pbg4ljlk29fhm3mdklnx3hwffyga1nr5krbpcyc3ywq8fq8")))
    (inputs
     (list ghc-case-insensitive
           ghc-data-fix
           ghc-hashable
           ghc-integer-logarithms
           ghc-old-time
           ghc-quickcheck
           ghc-scientific
           ghc-splitmix
           ghc-strict
           ghc-tagged
           ghc-these
           ghc-time-compat
           ghc-transformers-compat
           ghc-unordered-containers
           ghc-uuid-types
           ghc-vector))
    (home-page "https://github.com/aslatter/qc-instances")
    (synopsis "Common quickcheck instances")
    (description "This package provides QuickCheck instances for types
provided by the Haskell Platform.")
    (license license:bsd-3)))

(define-public ghc-quickcheck-unicode
  (package
    (name "ghc-quickcheck-unicode")
    (version "1.0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/quickcheck-unicode/"
             "quickcheck-unicode-" version ".tar.gz"))
       (sha256
        (base32
         "0s43s1bzbg3gwsjgm7fpyksd1339f0m26dlw2famxwyzgvm0a80k"))))
    (build-system haskell-build-system)
    (inputs (list ghc-quickcheck))
    (home-page
     "https://github.com/bos/quickcheck-unicode")
    (synopsis "Generator functions Unicode-related tests")
    (description "This package provides generator and shrink functions for
testing Unicode-related software.")
    (license license:bsd-3)))

(define-public ghc-quickcheck-io
  (package
    (name "ghc-quickcheck-io")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/quickcheck-io/quickcheck-io-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "08k4v7pkgjf30pv5j2dfv1gqv6hclxlniyq2sps8zq4zswcr2xzv"))))
    (build-system haskell-build-system)
    (inputs
     (list ghc-quickcheck ghc-hunit))
    (home-page
     "https://github.com/hspec/quickcheck-io#readme")
    (synopsis "Use HUnit assertions as QuickCheck properties")
    (description "This package provides an orphan instance that allows you to
use HUnit assertions as QuickCheck properties.")
    (license license:expat)))

(define-public ghc-quickcheck
  (package
    (name "ghc-quickcheck")
    (version "2.14.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/QuickCheck/QuickCheck-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1wrnrm9sq4s0bly0q58y80g4153q45iglqa34xsi2q3bd62nqyyq"))))
    (build-system haskell-build-system)
    (inputs
     (list ghc-random ghc-splitmix-bootstrap))
    (home-page "https://github.com/nick8325/quickcheck")
    (synopsis "Automatic testing of Haskell programs")
    (description
     "QuickCheck is a library for random testing of program properties.  The
programmer provides a specification of the program, in the form of properties
which functions should satisfy, and QuickCheck then tests that the properties
hold in a large number of randomly generated cases.  Specifications are
expressed in Haskell, using combinators defined in the QuickCheck library.")
    (license license:bsd-3)))

(define-public ghc-quickcheck-assertions
  (package
    (name "ghc-quickcheck-assertions")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "quickcheck-assertions/"
                           "quickcheck-assertions-" version ".tar.gz"))
       (sha256
        (base32 "1kyam4cy7qmnizjwjm8jamq43w7f0fs6ljfplwj0ib6wi2kjh0wv"))))
    (build-system haskell-build-system)
    (native-inputs
     (list ghc-hspec))
    (inputs
     (list ghc-ieee754 ghc-pretty-show ghc-quickcheck))
    (home-page "https://github.com/s9gf4ult/quickcheck-assertions")
    (synopsis "HUnit-like assertions for QuickCheck")
    (description
     "This Haskell library provides convenient assertions with pretty-printed
failure messages for QuickCheck properties, that are similar to those of
HUnit.")
    (license license:lgpl3)))

(define-public ghc-test-framework
  (package
    (name "ghc-test-framework")
    (version "0.8.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/test-framework/"
                           "test-framework-" version ".tar.gz"))
       (sha256
        (base32
         "1hhacrzam6b8f10hyldmjw8pb7frdxh04rfg3farxcxwbnhwgbpm"))))
    (build-system haskell-build-system)
    (arguments
     `(#:tests? #f  ; FIXME: Tests do not build.
       #:cabal-revision
       ("6" "0wbq9wiaag69nsqxwijzhs5y1hb9kbpkp1x65dvx158cxp8i9w9r")))
    (native-inputs
     (list ghc-hunit ghc-quickcheck))
    (inputs
     `(("ghc-ansi-terminal" ,ghc-ansi-terminal)
       ("ghc-ansi-wl-pprint" ,ghc-ansi-wl-pprint)
       ("ghc-hostname" ,ghc-hostname)
       ("ghc-old-locale" ,ghc-old-locale)
       ("ghc-random" ,ghc-random)
       ("ghc-regex-posix" ,ghc-regex-posix)
       ("ghc-xml" ,ghc-xml)
       ("ghc-libxml" ,ghc-libxml)
       ("ghc-semigroups" ,ghc-semigroups-bootstrap)))
    (home-page "https://batterseapower.github.io/test-framework/")
    (synopsis "Framework for running and organising tests")
    (description
     "This package allows tests such as QuickCheck properties and HUnit test
cases to be assembled into test groups, run in parallel (but reported in
deterministic order, to aid diff interpretation) and filtered and controlled
by command line options.  All of this comes with colored test output, progress
reporting and test statistics output.")
    (license license:bsd-3)))

(define-public ghc-test-framework-hunit
  (package
    (name "ghc-test-framework-hunit")
    (version "0.3.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "test-framework-hunit/test-framework-hunit-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1y0b6vg8nfm43v90lxxcydhi6qlxhfy4vpxbzm5ic2w55bh8xjwm"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("3" "0i9mlalv7cl1iq43ld5myrnpszq5rxmd79hk495dcb08rglhgl3z")))
    (inputs
     (list ghc-extensible-exceptions ghc-hunit ghc-test-framework))
    (home-page "https://batterseapower.github.io/test-framework/")
    (synopsis "HUnit support for test-framework")
    (description
     "This package provides HUnit support for the test-framework package.")
    (license license:bsd-3)))

(define-public ghc-test-framework-quickcheck2
  (package
    (name "ghc-test-framework-quickcheck2")
    (version "0.3.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "test-framework-quickcheck2/"
                           "test-framework-quickcheck2-" version ".tar.gz"))
       (sha256
        (base32
         "0ngf9vvby4nrdf1i7dxf5m9jn0g2pkq32w48xdr92n9hxka7ixn9"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("3" "0mglqfimla4vvv80mg08aj76zf4993wmngqlirh05h8i9nmgv6lh")))
    (inputs
     (list ghc-extensible-exceptions ghc-quickcheck ghc-random
           ghc-test-framework))
    (home-page "https://batterseapower.github.io/test-framework/")
    (synopsis "QuickCheck2 support for test-framework")
    (description
     "This package provides QuickCheck2 support for the test-framework
package.")
    (license license:bsd-3)))

(define-public ghc-test-framework-smallcheck
  (package
    (name "ghc-test-framework-smallcheck")
    (version "0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "test-framework-smallcheck/"
                           "test-framework-smallcheck-" version ".tar.gz"))
       (sha256
        (base32 "1xpgpk1gp4w7w46b4rhj80fa0bcyz8asj2dcjb5x1c37b7rw90b0"))))
    (build-system haskell-build-system)
    (inputs
     (list ghc-smallcheck ghc-test-framework))
    (home-page "https://github.com/Bodigrim/smallcheck")
    (synopsis "SmallCheck support for test-framework")
    (description
     "This package lets programmers use SmallCheck properties in Haskell's
test-framework.  New projects should use ghc-tasty-smallcheck instead.")
    (license license:bsd-3)))

(define-public ghc-test-framework-th
  (package
    (name "ghc-test-framework-th")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "test-framework-th-" version "/"
                           "test-framework-th-" version ".tar.gz"))
       (sha256
        (base32
         "12lw7yj02jb9s0i7rb98jjam43j2h0gzmnbj9zi933fx7sg0sy4b"))))
    (build-system haskell-build-system)
    (inputs
     (list ghc-test-framework ghc-language-haskell-extract
           ghc-haskell-src-exts ghc-regex-posix))
    (home-page "https://github.com/finnsson/test-generator")
    (synopsis "Auto generate the HUnit- and Quickcheck-bulk-code
using Template Haskell")
    (description "This library contains two functions:
@code{defaultMainGenerator} and @code{testGroupGenerator}.

@code{defaultMainGenerator} will extract all functions beginning with
@code{case_}, @code{prop_}, or @code{test_} in the module and put them in a
@code{testGroup}.

@code{testGroupGenerator} is like @code{defaultMainGenerator} but without
@code{defaultMain}.  It is useful if you need a function for the testgroup
\(e.g. if you want to be able to call the testgroup from another module).")
    (license license:bsd-3)))

(define-public ghc-hunit
  (package
    (name "ghc-hunit")
    (version "1.6.2.0")
    (outputs '("out" "static" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/HUnit/"
                           "HUnit-" version ".tar.gz"))
       (sha256
        (base32
         "1as4sw5y39c3zrmr6sb8zbw74c9gdn4401y0dx45ih7zf6457dxh"))))
    (build-system haskell-build-system)
    (inputs
     ;; We cannot use ghc-call-stack there, because it depends on
     ;; ghc-nanospec, which depends on ghc-hunit.
     (list ghc-call-stack-boot))
    (home-page "http://hunit.sourceforge.net/")
    (synopsis "Unit testing framework for Haskell")
    (description
     "HUnit is a unit testing framework for Haskell, inspired by the
JUnit tool for Java.")
    (license license:bsd-3)))

(define-public hspec-discover
  (package
    (name "hspec-discover")
    (version "2.7.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "hspec-discover/hspec-discover-"
                           version ".tar.gz"))
       (sha256
        (base32
         "13yzvd3b679skvs1insk4s0wc4zvmz6hs38kc8q0j6vzqq06smqa"))))
    (build-system haskell-build-system)
    (native-inputs
     (list ghc-quickcheck ghc-hspec-meta))
    (home-page "https://hspec.github.io/")
    (synopsis "Automatically discover and run Hspec tests")
    (description "hspec-discover is a tool which automatically discovers and
runs Hspec tests.")
    (license license:expat)))

(define-public ghc-hspec-core
  (package
    (name "ghc-hspec-core")
    (version "2.7.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/hspec-core/"
                           "hspec-core-" version ".tar.gz"))
       (sha256
        (base32
         "12k9yp5gznrda449ir60d5wv3xl7nnyffkb5mhfc0svw9f8lxlv1"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f)) ; FIXME: testing libraries are missing.
    (inputs
     (list ghc-setenv
           ghc-ansi-terminal
           ghc-clock
           ghc-quickcheck-io
           ghc-hunit
           ghc-quickcheck
           ghc-hspec-expectations
           ghc-silently
           ghc-tf-random))
    (home-page "https://hspec.github.io/")
    (synopsis "Testing framework for Haskell")
    (description "This library exposes internal types and functions that can
be used to extend Hspec's functionality.")
    (license license:expat)))

(define-public ghc-hspec-meta
  (package
    (name "ghc-hspec-meta")
    (version "2.7.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/hspec-meta/"
                           "hspec-meta-" version ".tar.gz"))
       (sha256
        (base32
         "0sfj0n2hy1r8ifysgbcmfdygcd7vyzr13ldkcp0l2ml337f8j0si"))))
    (build-system haskell-build-system)
    (inputs
     (list ghc-quickcheck
           ghc-hunit
           ghc-ansi-terminal
           ghc-clock
           ghc-hspec-expectations
           ghc-setenv
           ghc-random
           ghc-quickcheck-io))
    (home-page "https://hspec.github.io/")
    (synopsis "Version of Hspec to test Hspec itself")
    (description "This library provides a stable version of Hspec which is
used to test the in-development version of Hspec.")
    (license license:expat)))

(define-public ghc-hspec
  (package
    (name "ghc-hspec")
    (version "2.7.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/hspec/"
                           "hspec-" version ".tar.gz"))
       (sha256
        (base32
         "0z0lwrmrqkglr78n6k2c36n4h68142bh785ys0x4jaibjshvs6rw"))))
    (build-system haskell-build-system)
    (outputs '("out" "static" "doc"))
    (inputs
     (list ghc-hspec-core
           hspec-discover
           ghc-hspec-expectations
           ghc-quickcheck
           ghc-hunit
           ghc-stringbuilder
           ghc-hspec-meta))
    (home-page "https://hspec.github.io/")
    (synopsis "Testing Framework for Haskell")
    (description "This library provides the Hspec testing framework for
Haskell, inspired by the Ruby library RSpec.")
    (license license:expat)))

(define-public ghc-hspec-contrib
  (package
    (name "ghc-hspec-contrib")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "hspec-contrib/hspec-contrib-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0hhzxaa3fxz5mk5qcsrnfr98a7bn3szx2ydgr0x9mbqmm1jg06rc"))))
    (build-system haskell-build-system)
    (inputs
     (list ghc-hspec-core ghc-hunit ghc-hspec ghc-quickcheck))
    (native-inputs
     (list hspec-discover))
    (home-page "https://hspec.github.io/")
    (synopsis "Contributed functionality for Hspec")
    (description
     "This package provides contributed Hspec extensions.")
    (license license:expat)))

(define-public ghc-hspec-expectations
  (package
    (name "ghc-hspec-expectations")
    (version "0.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "hspec-expectations/hspec-expectations-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1vxl9zazbaapijr6zmcj72j9wf7ka1pirrjbwddwwddg3zm0g5l1"))))
    (build-system haskell-build-system)
    ;; Tests depend on ghc-nanospec.
    (arguments '(#:tests? #f))
    (inputs (list ghc-hunit))
    (home-page "https://github.com/sol/hspec-expectations")
    (synopsis "Catchy combinators for HUnit")
    (description "This library provides catchy combinators for HUnit, see
@uref{https://github.com/sol/hspec-expectations#readme, the README}.")
    (license license:expat)))

(define-public ghc-nanospec
  (package
    (name "ghc-nanospec")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "nanospec/nanospec-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1rcmhl9bhyfvanalnf1r86wkx6rq6wdvagnw1h011jcnnb1cq56g"))))
    (build-system haskell-build-system)
    (inputs
     (list ghc-hspec ghc-silently))
    (home-page "https://github.com/hspec/nanospec#readme")
    (synopsis "Lightweight implementation of a subset of Hspec's API")
    (description
     "Nanospec is a lightweight implementation of a subset of Hspec's API with
minimal dependencies.")
    (license license:expat)))

(define-public ghc-crypto-cipher-tests
  (package
    (name "ghc-crypto-cipher-tests")
    (version "0.0.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "crypto-cipher-tests-" version "/"
                           "crypto-cipher-tests-" version ".tar.gz"))
       (sha256
        (base32
         "19wqignlq90qwpam01hnmmrxaxh5lkax9l1l6rlbi4a07nvp1dnz"))))
    (build-system haskell-build-system)
    (inputs (list ghc-quickcheck
                  ghc-hunit
                  ghc-test-framework
                  ghc-test-framework-quickcheck2
                  ghc-test-framework-hunit
                  ghc-byteable
                  ghc-securemem
                  ghc-crypto-cipher-types))
    (home-page "https://github.com/vincenthz/hs-crypto-cipher")
    (synopsis "Generic cryptography cipher tests for Haskell")
    (description " This Haskell package contains generic tests for
cryptographic ciphers, and is used by the test runners of various Haskell
implementations of cryptographic ciphers.")
    (license license:bsd-3)))

(define-public ghc-hedgehog
  (package
    (name "ghc-hedgehog")
    (version "1.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/hedgehog/hedgehog-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1qsqs8lmxa3wmw228cwi98vvvh9hqbc9d43i1sy2c9igw9xlhfi6"))))
    (build-system haskell-build-system)
    (inputs
     (list ghc-ansi-terminal
           ghc-async
           ghc-concurrent-output
           ghc-erf
           ;("ghc-exceptions" ,ghc-exceptions)
           ghc-lifted-async
           ghc-mmorph
           ghc-monad-control
           ghc-pretty-show
           ghc-primitive
           ghc-random
           ghc-resourcet
           ghc-transformers-base
           ghc-wl-pprint-annotated))
    (home-page "https://hedgehog.qa")
    (synopsis "Property-based testing in the spirt of QuickCheck")
    (description
     "Hedgehog is a property-based testing system, in the spirit of
QuickCheck.  Hedgehog uses integrated shrinking, so shrinks obey the invariants
of generated values by construction.

To get started quickly, see the examples:
@uref{https://github.com/hedgehogqa/haskell-hedgehog/tree/master/hedgehog-example}")
    (license license:bsd-3)))

(define-public cabal-doctest
  (package
    (name "cabal-doctest")
    (version "1.0.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "cabal-doctest/cabal-doctest-"
                           version ".tar.gz"))
       (sha256
        (base32
         "03if74imlhhk7m56nci5f1wclniwqdmwl4hl177040j1gnlac9i0"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("2" "05v1awad3d1wvc763xcgvxm4n6n7bs7byc6s14kdbw35zcaddlcb")))
    (home-page "https://github.com/phadej/cabal-doctest")
    (synopsis "Setup.hs helper for running doctests")
    (description
     "To properly work, the @code{doctest} package needs plenty of
configuration.  This library provides the common bits for writing custom
@file{Setup.hs} files.")
    (license license:bsd-3)))

(define-public ghc-testing-type-modifiers
  (package
    (name "ghc-testing-type-modifiers")
    (version "0.1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://hackage.haskell.org/package/"
                            "testing-type-modifiers/testing-type-modifiers-"
                            version ".tar.gz"))
        (sha256
          (base32
            "1wh2n95n39ivv6kbqn42vbzrj8zagsmk6f2al2qj40bg5kgdl2q5"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/testing-type-modifiers")
    (synopsis "Data type modifiers for property based testing")
    (description "Property based testing libraries such as QuickCheck tend to
include type modifiers.  Most of them are used to quantify over subsets of a
type.  This library is intended to supply these modifiers to be used by
testing libraries, in an effort to make properties more portable between
testing frameworks.")
    (license license:unlicense)))

(define-public ghc-testing-feat
  (package
    (name "ghc-testing-feat")
    (version "1.1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://hackage.haskell.org/package/"
                            "testing-feat/testing-feat-" version ".tar.gz"))
        (sha256
          (base32
            "1v2qzzpf1s008g7q6q67glf7vbm1pkpq4rc3ii74f4g6vhfx610r"))))
    (build-system haskell-build-system)
    (inputs
      (list ghc-quickcheck ghc-size-based ghc-testing-type-modifiers
            ghc-semigroups))
    (home-page "https://github.com/JonasDuregard/testing-feat")
    (synopsis "Functional Enumeration of Algebraic Types")
    (description "Feat (Functional Enumeration of Algebraic Types)
provides enumerations as functions from natural numbers to
values (similar to @code{toEnum} but for any algebraic data type).  This
can be used for SmallCheck-style systematic testing, QuickCheck-style
random testing, and hybrids of the two.")
    (license license:bsd-3)))

(define-public ghc-inspection-testing
  (package
    (name "ghc-inspection-testing")
    (version "0.4.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/inspection-testing/"
             "inspection-testing-" version ".tar.gz"))
       (sha256
        (base32
         "0qz1npyycj4bvyly9xmjbnhw569l52h38gx02rk0r7zhapw83aig"))))
    (build-system haskell-build-system)
    (home-page
     "https://github.com/nomeata/inspection-testing")
    (synopsis "GHC plugin to do inspection testing")
    (description
     "Some carefully crafted libraries make promises to their users beyond
functionality and performance.

Examples are: Fusion libraries promise intermediate data structures to be
eliminated.  Generic programming libraries promise that the generic
implementation is identical to the hand-written one.  Some libraries may
promise allocation-free or branch-free code.

Conventionally, the modus operandi in all these cases is that the library
author manually inspects the (intermediate or final) code produced by the
compiler.  This is not only tedious, but makes it very likely that some change,
either in the library itself or the surrounding eco-system, breaks the
library's promised without anyone noticing.

This package provides a disciplined way of specifying such properties, and
have them checked by the compiler.  This way, this checking can be part of the
regular development cycle and regressions caught early.

See the documentation in \"Test.Inspection\" or the project webpage for more
examples and more information.")
    (license license:expat)))

(define-public ghc-quickcheck-classes
  (package
    (name "ghc-quickcheck-classes")
    (version "0.6.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/quickcheck-classes/quickcheck-classes-"
               version
               ".tar.gz"))
        (sha256
          (base32 "19iw15mvb7gws3ljdxqwsbb4pmfc0sfflf8szgmrhiqr3k82mqv2"))))
    (build-system haskell-build-system)
    (inputs
      (list ghc-quickcheck
            ghc-primitive
            ghc-primitive-addr
            ghc-quickcheck-classes-base
            ghc-aeson
            ghc-semigroupoids
            ghc-semirings
            ghc-vector))
    (native-inputs
      (list ghc-base-orphans
            ghc-tagged
            ghc-base-orphans
            ghc-tagged
            ghc-tasty
            ghc-tasty-quickcheck))
    (home-page "https://github.com/andrewthad/quickcheck-classes#readme")
    (synopsis "QuickCheck common typeclasses")
    (description
      "This library provides QuickCheck properties to ensure that typeclass
instances adhere to the set of laws that they are supposed to.  There are
other libraries that do similar things, such as @code{genvalidity-hspec} and
@code{checkers}.  This library differs from other solutions by not introducing
any new typeclasses that the user needs to learn.  /Note:/ on GHC < 8.5, this
library uses the higher-kinded typeclasses (@code{Data.Functor.Classes.Show1},
@code{Data.Functor.Classes.Eq1}, @code{Data.Functor.Classes.Ord1}, etc.), but
on GHC >= 8.5, it uses @code{-XQuantifiedConstraints} to express these
constraints more cleanly.")
    (license license:bsd-3)))

(define-public ghc-quickcheck-classes-base
  (package
    (name "ghc-quickcheck-classes-base")
    (version "0.6.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/quickcheck-classes-base/quickcheck-classes-base-"
               version
               ".tar.gz"))
        (sha256
          (base32 "16c6gq4cqpkwnq1pzkhm6r7mrwk4an50ha5w77bmiia2qkhla6ch"))))
    (build-system haskell-build-system)
    (inputs
      (list ghc-quickcheck
            ghc-contravariant
            ghc-bifunctors
            ghc-semigroups
            ghc-fail
            ghc-tagged))
    (home-page "https://github.com/andrewthad/quickcheck-classes#readme")
    (synopsis "QuickCheck common typeclasses from `base`")
    (description
      "This library is a minimal variant of `quickcheck-classes`
that only provides laws for typeclasses from `base`. The main
purpose of splitting this out is so that `primitive` can depend
on `quickcheck-classes-base` in its test suite, avoiding the
circular dependency that arises if `quickcheck-classes` is used
instead. . This library provides QuickCheck properties to ensure
that typeclass instances adhere to the set of laws that they are
supposed to. There are other libraries that do similar things,
such as `genvalidity-hspec` and `checkers`. This library differs
from other solutions by not introducing any new typeclasses that
the user needs to learn. . /Note:/ on GHC < 8.5, this library
uses the higher-kinded typeclasses ('Data.Functor.Classes.Show1',
'Data.Functor.Classes.Eq1', 'Data.Functor.Classes.Ord1', etc.),
but on GHC >= 8.5, it uses `-XQuantifiedConstraints` to express
these constraints more cleanly.")
    (license license:bsd-3)))

(define-public ghc-doctest-lib
  (package
    (name "ghc-doctest-lib")
    (version "0.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/doctest-lib/doctest-lib-"
               version
               ".tar.gz"))
        (sha256
          (base32 "1vswam0dhw52dihgnzirh18gqs8rj8h6jd7pl6y1mg2f9f9zmih2"))))
    (build-system haskell-build-system)
    (home-page "https://hub.darcs.net/thielema/doctest-lib/")
    (synopsis "Parts of doctest exposed as library")
    (description
      "Parts of doctest exposed as library. For use with the doctest-extract utility.")
    (license license:expat)))

(define-public ghc-doctest-exitcode-stdio
  (package
    (name "ghc-doctest-exitcode-stdio")
    (version "0.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/doctest-exitcode-stdio/doctest-exitcode-stdio-"
               version
               ".tar.gz"))
        (sha256
          (base32 "1g3c7yrqq2mwqbmvs8vkx1a3cf0p0x74b7fnn344dsk7bsfpgv0x"))))
    (build-system haskell-build-system)
    (inputs
      (list ghc-doctest-lib ghc-quickcheck ghc-semigroups))
    (home-page "https://hub.darcs.net/thielema/doctest-exitcode-stdio/")
    (synopsis "Run Doctests in a @code{Cabal.Test.exitcode-stdio} environment")
    (description
      "This package allows on to run Doctests in a Cabal.Test.exitcode-stdio
environment.")
    (license license:bsd-3)))

(define-public ghc-cabal-doctest
  (package
    (name "ghc-cabal-doctest")
    (version "1.0.8")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/cabal-doctest/cabal-doctest-"
               version
               ".tar.gz"))
        (sha256
          (base32 "03if74imlhhk7m56nci5f1wclniwqdmwl4hl177040j1gnlac9i0"))))
    (build-system haskell-build-system)
    (arguments
      `(#:cabal-revision
        ("2" "05v1awad3d1wvc763xcgvxm4n6n7bs7byc6s14kdbw35zcaddlcb")))
    (home-page "https://github.com/phadej/cabal-doctest")
    (synopsis "@file{Setup.hs} helper for Doctests running")
    (description
      "This package provides helpers for running Doctests in @file{Setup.hs}.")
    (license license:bsd-3)))

(define-public ghc-tasty-silver
  (package
    (name "ghc-tasty-silver")
    (version "3.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/tasty-silver/tasty-silver-"
               version
               ".tar.gz"))
        (sha256
          (base32 "0nvh2k8iqqkanmp7lpwd3asimyarzisly8wavbdahcxryn0j4xb7"))))
    (build-system haskell-build-system)
    (inputs
      (list ghc-ansi-terminal
            ghc-async
            ghc-optparse-applicative
            ghc-process-extras
            ghc-regex-tdfa
            ghc-semigroups
            ghc-tagged
            ghc-tasty
            ghc-temporary))
    (native-inputs
     (list ghc-tasty-hunit ghc-silently))
    (home-page "https://github.com/phile314/tasty-silver")
    (synopsis "Fancy test runner, including support for golden tests")
    (description
      "This package provides a fancy test runner and support for @dfn{golden
testing}.  A golden test is an IO action that writes its result to a file.  To
pass the test, this output file should be identical to the corresponding
``golden'' file, which contains the correct result for the test.  The test
runner allows filtering tests using regexes, and to interactively inspect the
result of golden tests.")
    (license license:expat)))

