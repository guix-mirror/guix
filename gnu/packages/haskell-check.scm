;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015, 2016, 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 ng0 <ng0@n0.is>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2017 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2017 rsiddharth <s@ricketyspace.net>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Tonton <tonton@riseup.net>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
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
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-crypto)
  #:use-module (guix build-system haskell)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public ghc-tasty-ant-xml
  (package
    (name "ghc-tasty-ant-xml")
    (version "1.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/tasty-ant-xml/tasty-ant-xml-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0v0gsb90kh6hwlgxbclzawsskywc6yf7n8xhiifia97l4y0yx2m8"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-generic-deriving" ,ghc-generic-deriving)
       ("ghc-xml" ,ghc-xml)
       ("ghc-stm" ,ghc-stm)
       ("ghc-tagged" ,ghc-tagged)
       ("ghc-tasty" ,ghc-tasty)))
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
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/tasty-smallcheck/tasty-smallcheck-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1n66ngzllf3xrlqykwszlkwsi96n5nkm7xbpfq7774vpvfnafjri"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-smallcheck" ,ghc-smallcheck)
       ("ghc-async" ,ghc-async)
       ("ghc-tagged" ,ghc-tagged)))
    (home-page "https://documentup.com/feuerbach/tasty")
    (synopsis "SmallCheck support for the Tasty test framework")
    (description "This package provides SmallCheck support for the Tasty
Haskell test framework.")
    (license license:bsd-3)))

(define-public ghc-tasty-quickcheck
  (package
    (name "ghc-tasty-quickcheck")
    (version "0.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/tasty-quickcheck/"
             "tasty-quickcheck-" version ".tar.gz"))
       (sha256
        (base32
         "0vr6szbbz3s5461i0zr8zpq347zfvidfzv5gf3xwxhm0yk731z8h"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-tagged" ,ghc-tagged)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-random" ,ghc-random)
       ("ghc-ansi-terminal" ,ghc-ansi-terminal)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-pcre-light" ,ghc-pcre-light)))
    (home-page "http://documentup.com/feuerbach/tasty")
    (synopsis "QuickCheck support for the Tasty test framework")
    (description "This package provides QuickCheck support for the Tasty
Haskell test framework.")
    (license license:expat)))

(define-public ghc-tasty-golden
  (package
    (name "ghc-tasty-golden")
    (version "2.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/tasty-golden/tasty-golden-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0k3ibjhjc9vcwzrjnl4rnwvfm8l81q347nb7dgvcib6n5wm3s404"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-temporary" ,ghc-temporary)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-optparse-applicative" ,ghc-optparse-applicative)
       ("ghc-tagged" ,ghc-tagged)
       ("ghc-async" ,ghc-async)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-temporary-rc" ,ghc-temporary-rc)))
    (home-page
     "https://github.com/feuerbach/tasty-golden")
    (synopsis "Golden tests support for tasty")
    (description
     "This package provides support for 'golden testing'.  A @dfn{golden test}
is an IO action that writes its result to a file.  To pass the test, this
output file should be identical to the corresponding 'golden' file, which
contains the correct result for the test.")
    (license license:expat)))

;; This package builds `clock` without tests, since the tests rely on tasty
;; and tasty-quickcheck, which in turn require clock to build.
(define ghc-clock-bootstrap
  (package
    (name "ghc-clock-bootstrap")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/"
             "clock/"
             "clock-" version ".tar.gz"))
       (sha256
        (base32 "1ncph7vi2q6ywwc8ysxl1ibw6i5dwfvln88ssfazk8jgpj4iyykw"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f)) ;; Testing suite depends on tasty and
                               ;; tasty-quickcheck, which need clock to build.
    (home-page "https://hackage.haskell.org/package/clock")
    (synopsis "High-resolution clock for Haskell")
    (description "A package for convenient access to high-resolution clock and
timer functions of different operating systems via a unified API.")
    (license license:bsd-3)))

(define-public ghc-tasty
  (package
    (name "ghc-tasty")
    (version "1.1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/tasty/tasty-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "14riid753hjqr6lca1kgxpnvq0wykf0k3qc5jpag42hh8bszav22"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-stm" ,ghc-stm)
       ("ghc-tagged" ,ghc-tagged)
       ("ghc-regex-tdfa" ,ghc-regex-tdfa)
       ("ghc-optparse-applicative" ,ghc-optparse-applicative)
       ("ghc-unbounded-delays" ,ghc-unbounded-delays)
       ("ghc-async" ,ghc-async)
       ("ghc-ansi-terminal" ,ghc-ansi-terminal)
       ("ghc-clock-bootstrap" ,ghc-clock-bootstrap)
       ("ghc-wcwidth" ,ghc-wcwidth-bootstrap)))
    (home-page "http://documentup.com/feuerbach/tasty")
    (synopsis "Modern and extensible testing framework")
    (description "Tasty is a modern testing framework for Haskell.  It lets
you combine your unit tests, golden tests, QuickCheck/SmallCheck properties,
and any other types of tests into a single test suite.")
    (license license:expat)))

(define-public ghc-tasty-hunit
  (package
    (name "ghc-tasty-hunit")
    (version "0.10.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/tasty-hunit/tasty-hunit-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0j3hgga6c3s8h5snzivb8a75h96207ia2rlbxzj07xbf4zpkp44g"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-call-stack" ,ghc-call-stack-boot)
       ("ghc-tasty" ,ghc-tasty)))
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
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)))
    (home-page "https://github.com/vincenthz/tasty-kat")
    (synopsis "Known Answer Tests (KAT) framework for tasty")
    (description
     "This package provides a @dfn{Known Answer Tests} (KAT) framework for
tasty.")
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
     `(("ghc-haskell-src-exts" ,ghc-haskell-src-exts)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)))
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
    (version "1.1.12")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://hackage.haskell.org/package/tasty-rerun/"
                    "tasty-rerun-" version ".tar.gz"))
              (sha256
               (base32
                "05lp4zy6lwd916snq6hs43848n62j9vdfl3s8sfivqydrax0vvd8"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-optparse-applicative" ,ghc-optparse-applicative)
       ("ghc-reducers" ,ghc-reducers)
       ("ghc-split" ,ghc-split)
       ("ghc-stm" ,ghc-stm)
       ("ghc-tagged" ,ghc-tagged)
       ("ghc-tasty" ,ghc-tasty)))
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
    (version "0.11.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "tasty-expected-failure/tasty-expected-failure-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1i2s809m644b7hgiblqay9j364r3fjj1rwbrahsn1pgr5q6mr6ji"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-tagged" ,ghc-tagged)
       ("ghc-tasty" ,ghc-tasty)))
    (home-page "http://github.com/nomeata/tasty-expected-failure")
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
    (version "0.3.18")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/"
             "quickcheck-instances/quickcheck-instances-"
             version ".tar.gz"))
       (sha256
        (base32
         "1bh1pzz5fdcqvzdcirqxna6fnjms02min5md716299g5niz46w55"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("1" "1sngfq3v71bvgjsl8cj5kh65m3fziwy8dkvwjzs0kxfrzr87faly")))
    (inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-base-compat" ,ghc-base-compat)
       ("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-old-time" ,ghc-old-time)
       ("ghc-scientific" ,ghc-scientific)
       ("ghc-tagged" ,ghc-tagged)
       ("ghc-text" ,ghc-text)
       ("ghc-transformers-compat" ,ghc-transformers-compat)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-uuid-types" ,ghc-uuid-types)
       ("ghc-vector" ,ghc-vector)))
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
    (inputs `(("ghc-quickcheck" ,ghc-quickcheck)))
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
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-hunit" ,ghc-hunit)))
    (home-page
     "https://github.com/hspec/quickcheck-io#readme")
    (synopsis "Use HUnit assertions as QuickCheck properties")
    (description "This package provides an orphan instance that allows you to
use HUnit assertions as QuickCheck properties.")
    (license license:expat)))

(define-public ghc-quickcheck
  (package
    (name "ghc-quickcheck")
    (version "2.11.3")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/QuickCheck/QuickCheck-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0xhqk35fkzlbjcqbabg6962jkv8d688nzmz7ng4bm84x2d95d328"))))
    (build-system haskell-build-system)
    (arguments
     `(#:tests? #f))  ; FIXME: currently missing libraries used for tests.
    (inputs
     `(("ghc-random" ,ghc-random)
       ("ghc-tf-random" ,ghc-tf-random)))
    (home-page "https://github.com/nick8325/quickcheck")
    (synopsis "Automatic testing of Haskell programs")
    (description
     "QuickCheck is a library for random testing of program properties.  The
programmer provides a specification of the program, in the form of properties
which functions should satisfy, and QuickCheck then tests that the properties
hold in a large number of randomly generated cases.  Specifications are
expressed in Haskell, using combinators defined in the QuickCheck library.")
    (license license:bsd-3)))

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
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'update-constraints
           (lambda _
             (substitute* "test-framework.cabal"
               (("QuickCheck     >= 2\\.3 && < 2\\.10")
                "QuickCheck     >= 2.3 && < 2.12")))))))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)))
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
     `(("ghc-extensible-exceptions" ,ghc-extensible-exceptions)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-test-framework" ,ghc-test-framework)))
    (home-page "https://batterseapower.github.io/test-framework/")
    (synopsis "HUnit support for test-framework")
    (description
     "This package provides HUnit support for the test-framework package.")
    (license license:bsd-3)))

(define-public ghc-test-framework-quickcheck2
  (package
    (name "ghc-test-framework-quickcheck2")
    (version "0.3.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "test-framework-quickcheck2/"
                           "test-framework-quickcheck2-" version ".tar.gz"))
       (sha256
        (base32
         "0vj834337r6jzr3258cv68ly2sv5999mklpsrfngyk51kywsyqyp"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("1" "147ngmfdkskyg7mwsp5w73a4dbx3rp5s38bci3z03kn1m093lxff")))
    (inputs
     `(("ghc-extensible-exceptions" ,ghc-extensible-exceptions)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-random" ,ghc-random)
       ("ghc-test-framework" ,ghc-test-framework)))
    (home-page "https://batterseapower.github.io/test-framework/")
    (synopsis "QuickCheck2 support for test-framework")
    (description
     "This packages provides QuickCheck2 support for the test-framework
package.")
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
     `(("ghc-test-framework" ,ghc-test-framework)
       ("ghc-language-haskell-extract" ,ghc-language-haskell-extract)
       ("ghc-haskell-src-exts" ,ghc-haskell-src-exts)
       ("ghc-regex-posix" ,ghc-regex-posix)))
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
    (version "1.6.0.0")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/HUnit/"
                           "HUnit-" version ".tar.gz"))
       (sha256
        (base32
         "1pnhwqq5v8h48gbp3ibv3skfj25mf4zs5svxcavq93p9cswycj3l"))))
    (build-system haskell-build-system)
    (inputs
     ;; We cannot use ghc-call-stack there, because it depends on
     ;; ghc-nanospec, which depends on ghc-hunit.
     `(("ghc-call-stack" ,ghc-call-stack-boot)))
    (home-page "http://hunit.sourceforge.net/")
    (synopsis "Unit testing framework for Haskell")
    (description
     "HUnit is a unit testing framework for Haskell, inspired by the
JUnit tool for Java.")
    (license license:bsd-3)))

(define-public hspec-discover
  (package
    (name "hspec-discover")
    (version "2.5.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "hspec-discover/hspec-discover-"
                           version ".tar.gz"))
       (sha256
        (base32
         "04aidzi91ccr9bygmfkjzshz34z9vh8wvqj4zinx2clxq6r7gqfz"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-hspec-meta" ,ghc-hspec-meta)))
    (home-page "https://hspec.github.io/")
    (synopsis "Automatically discover and run Hspec tests")
    (description "hspec-discover is a tool which automatically discovers and
runs Hspec tests.")
    (license license:expat)))

(define-public ghc-hspec-core
  (package
    (name "ghc-hspec-core")
    (version "2.5.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/hspec-core/"
                           "hspec-core-" version ".tar.gz"))
       (sha256
        (base32
         "1vfrqlpn32s9wiykmkxbnrnd5p56yznw20pf8fwzw78ar4wpz55x"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f)) ; FIXME: testing libraries are missing.
    (inputs
     `(("ghc-setenv" ,ghc-setenv)
       ("ghc-ansi-terminal" ,ghc-ansi-terminal)
       ("ghc-async" ,ghc-async)
       ("ghc-clock" ,ghc-clock)
       ("ghc-quickcheck-io" ,ghc-quickcheck-io)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-hspec-expectations" ,ghc-hspec-expectations)
       ("ghc-silently" ,ghc-silently)))
    (home-page "https://hspec.github.io/")
    (synopsis "Testing framework for Haskell")
    (description "This library exposes internal types and functions that can
be used to extend Hspec's functionality.")
    (license license:expat)))

(define-public ghc-hspec-meta
  (package
    (name "ghc-hspec-meta")
    (version "2.4.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/hspec-meta/"
                           "hspec-meta-" version ".tar.gz"))
       (sha256
        (base32
         "0qmvk01n79j6skn79r6zalg2pd0x0nqqn9qn8mhg0pgyzcdnfc9b"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-ansi-terminal" ,ghc-ansi-terminal)
       ("ghc-async" ,ghc-async)
       ("ghc-hspec-expectations" ,ghc-hspec-expectations)
       ("ghc-setenv" ,ghc-setenv)
       ("ghc-random" ,ghc-random)
       ("ghc-quickcheck-io" ,ghc-quickcheck-io)))
    (home-page "https://hspec.github.io/")
    (synopsis "Version of Hspec to test Hspec itself")
    (description "This library provides a stable version of Hspec which is
used to test the in-development version of Hspec.")
    (license license:expat)))

(define-public ghc-hspec
  (package
    (name "ghc-hspec")
    (version "2.5.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/hspec/"
                           "hspec-" version ".tar.gz"))
       (sha256
        (base32
         "1yv4k5b5kkig2q3waj28587sq28wms7wfav5a3lq4dra6jybimfm"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-hspec-core" ,ghc-hspec-core)
       ("hspec-discover" ,hspec-discover)
       ("ghc-hspec-expectations" ,ghc-hspec-expectations)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-stringbuilder" ,ghc-stringbuilder)
       ("ghc-hspec-meta" ,ghc-hspec-meta)))
    (home-page "https://hspec.github.io/")
    (synopsis "Testing Framework for Haskell")
    (description "This library provides the Hspec testing framework for
Haskell, inspired by the Ruby library RSpec.")
    (license license:expat)))

(define-public ghc-hspec-contrib
  (package
    (name "ghc-hspec-contrib")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "hspec-contrib/hspec-contrib-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "13579xdqwbsy8k0vxdcvgy932d4p76mij1rzkzbpqbspfn7399yv"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-hspec-core" ,ghc-hspec-core)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (native-inputs
     `(("hspec-discover" ,hspec-discover)))
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
    (inputs `(("ghc-hunit" ,ghc-hunit)))
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
     `(("ghc-hspec" ,ghc-hspec)
       ("ghc-silently" ,ghc-silently)))
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
    (inputs `(("ghc-quickcheck" ,ghc-quickcheck)
              ("ghc-hunit" ,ghc-hunit)
              ("ghc-test-framework" ,ghc-test-framework)
              ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)
              ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
              ("ghc-byteable" ,ghc-byteable)
              ("ghc-securemem" ,ghc-securemem)
              ("ghc-crypto-cipher-types" ,ghc-crypto-cipher-types)))
    (home-page "https://github.com/vincenthz/hs-crypto-cipher")
    (synopsis "Generic cryptography cipher tests for Haskell")
    (description " This Haskell package contains generic tests for
cryptographic ciphers, and is used by the test runners of various Haskell
implementations of cryptographic ciphers.")
    (license license:bsd-3)))
