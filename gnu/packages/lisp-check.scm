;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017 Andy Patterson <ajpatter@uwaterloo.ca>
;;; Copyright © 2018, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2019, 2020, 2021 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2020 Katherine Cox-Buday <cox.katherine.e@gmail.com>
;;; Copyright © 2019, 2020, 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2021 Charles Jackson <charles.b.jackson@protonmail.com>
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

;;; This module only contains Common Lisp libraries related to code testing
;;; facilities.

(define-module (gnu packages lisp-check)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system asdf))

(define-public sbcl-1am
  (let ((commit "8b1da94eca4613fd8a20bdf63f0e609e379b0ba5"))
    (package
      (name "sbcl-1am")
      (version (git-version "0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/lmj/1am")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "05ss4nz1jb9kb796295482b62w5cj29msfj8zis33sp2rw2vmv2g"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/lmj/1am")
      (synopsis "Minimal testing framework for Common Lisp")
      (description "A minimal testing framework for Common Lisp.")
      (license license:expat))))

(define-public cl-1am
  (sbcl-package->cl-source-package sbcl-1am))

(define-public ecl-1am
  (sbcl-package->ecl-package sbcl-1am))

(define-public sbcl-check-it
  (let ((commit "b79c9103665be3976915b56b570038f03486e62f"))
    (package
      (name "sbcl-check-it")
      (version (git-version "0.1.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/DalekBaldwin/check-it/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1kbjwpniffdpv003igmlz5r0vy65m7wpfnhg54fhwirp1227hgg7"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria sbcl-closer-mop sbcl-optima))
      (native-inputs
       (list sbcl-stefil))
      (home-page "https://github.com/arclanguage/Clamp")
      (synopsis "Randomized specification-based testing for Common Lisp")
      (description
       "This is a randomized property-based testing library for Common Lisp.
Rather than being a full-fledged general test framework in its own right, it's
designed to embed randomized tests in whatever framework you like.")
      (license license:llgpl))))

(define-public cl-check-it
  (sbcl-package->cl-source-package sbcl-check-it))

(define-public ecl-check-it
  (sbcl-package->ecl-package sbcl-check-it))

(define-public sbcl-checkl
  (let ((commit "80328800d047fef9b6e32dfe6bdc98396aee3cc9")
        (revision "1"))
    (package
      (name "sbcl-checkl")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rpav/CheckL")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0bpisihx1gay44xmyr1dmhlwh00j0zzi04rp9fy35i95l2r4xdlx"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       ;; Error while trying to load definition for system checkl-test from
       ;; pathname [...]/checkl-test.asd: The function CHECKL:DEFINE-TEST-OP
       ;; is undefined.
       '(#:asd-files '("checkl.asd")
         #:tests? #f))
      (native-inputs
       (list sbcl-fiveam))
      (inputs
       (list sbcl-marshal))
      (home-page "https://github.com/rpav/CheckL/")
      (synopsis "Dynamic testing for Common Lisp")
      (description
       "CheckL lets you write tests dynamically, it checks resulting values
against the last run.")
      ;; The author specifies both LLGPL and "BSD", but the "BSD" license
      ;; isn't specified anywhere, so I don't know which kind.  LLGPL is the
      ;; stronger of the two and so I think only listing this should suffice.
      (license license:llgpl))))

(define-public cl-checkl
  (sbcl-package->cl-source-package sbcl-checkl))

(define-public ecl-checkl
  (sbcl-package->ecl-package sbcl-checkl))

(define-public sbcl-cl-mock
  ;; .asd version only got updated recently, despired the old GitHug "1.0.1" release.
  (let ((commit "7988dca2093358911b67597a2cd1570c785dfe76"))
    (package
      (name "sbcl-cl-mock")
      (version (git-version "1.0.1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Ferada/cl-mock/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0f40wikcf783jx26ip0nnhwjjfjvjiw7njqsqrb6kaphc8bgw0i1"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria sbcl-closer-mop sbcl-trivia))
      (native-inputs
       (list sbcl-fiveam))
      (home-page "https://github.com/Ferada/cl-mock")
      (synopsis "Mocking functions for Common Lisp testing")
      (description
       "This small library provides a way to replace the actual implementation
of either regular or generic functions with mocks.")
      (license license:agpl3))))

(define-public ecl-cl-mock
  (sbcl-package->ecl-package sbcl-cl-mock))

(define-public cl-mock
  (sbcl-package->cl-source-package sbcl-cl-mock))

(define-public sbcl-cl-quickcheck
  (let ((commit "807b2792a30c883a2fbecea8e7db355b50ba662f")
        (revision "1"))
    (package
      (name "sbcl-cl-quickcheck")
      (version (git-version "0.0.4" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mcandre/cl-quickcheck")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "165lhypq5xkcys6hvzb3jq7ywnmqvzaflda29qk2cbs3ggas4767"))))
      (build-system asdf-build-system/sbcl)
      (synopsis
       "Common Lisp port of the QuickCheck unit test framework")
      (description
       "Common Lisp port of the QuickCheck unit test framework")
      (home-page "https://github.com/mcandre/cl-quickcheck")
      ;; MIT
      (license license:expat))))

(define-public cl-quickcheck
  (sbcl-package->cl-source-package sbcl-cl-quickcheck))

(define-public ecl-cl-quickcheck
  (sbcl-package->ecl-package sbcl-cl-quickcheck))

(define-public sbcl-clunit
  (let ((commit "6f6d72873f0e1207f037470105969384f8380628")
        (revision "1"))
    (package
      (name "sbcl-clunit")
      (version (git-version "0.2.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tgutu/clunit")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1idf2xnqzlhi8rbrqmzpmb3i1l6pbdzhhajkmhwbp6qjkmxa4h85"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "CLUnit is a Common Lisp unit testing framework")
      (description
       "CLUnit is a Common Lisp unit testing framework.  It is designed
to be easy to use so that you can quickly start testing.  CLUnit
provides a rich set of features aimed at improving your unit testing
experience.")
      (home-page "https://tgutu.github.io/clunit/")
      ;; MIT License
      (license license:expat))))

(define-public cl-clunit
  (sbcl-package->cl-source-package sbcl-clunit))

(define-public ecl-clunit
  (sbcl-package->ecl-package sbcl-clunit))

(define-public sbcl-clunit2
  (let ((commit "5e28343734eb9b7aee39306a614af92c1062d50b")
        (revision "1"))
    (package
      (name "sbcl-clunit2")
      (version (git-version "0.2.4" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://notabug.org/cage/clunit2.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1ngiapfki6nm8a555mzhb5p7ch79i3w665za5bmb5j7q34fy80vw"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Unit testing framework for Common Lisp")
      (description
       "CLUnit is a Common Lisp unit testing framework.  It is designed to be
easy to use so that you can quickly start testing.")
      (home-page "https://notabug.org/cage/clunit2")
      (license license:expat))))

(define-public cl-clunit2
  (sbcl-package->cl-source-package sbcl-clunit2))

(define-public ecl-clunit2
  (sbcl-package->ecl-package sbcl-clunit2))

(define-public sbcl-eos
  (let ((commit "b4413bccc4d142cbe1bf49516c3a0a22c9d99243")
        (revision "2"))
    (package
      (name "sbcl-eos")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/adlai/Eos")
               (commit commit)))
         (sha256
          (base32 "1afllvmlnx97yzz404gycl3pa3kwx427k3hrbf37rpmjlv47knhk"))
         (file-name (git-file-name "eos" version))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Unit Testing for Common Lisp")
      (description
       "Eos was a unit testing library for Common Lisp.
It began as a fork of FiveAM; however, FiveAM development has continued, while
that of Eos has not.  Thus, Eos is now deprecated in favor of FiveAM.")
      (home-page "https://github.com/adlai/Eos")
      (license license:expat))))

(define-public cl-eos
  (sbcl-package->cl-source-package sbcl-eos))

(define-public ecl-eos
  (sbcl-package->ecl-package sbcl-eos))

(define-public sbcl-fiasco
  (let ((commit "bb47d2fef4eb24cc16badc1c9a73d73c3a7e18f5")
        (revision "2"))
    (package
      (name "sbcl-fiasco")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/joaotavora/fiasco")
               (commit commit)))
         (file-name (git-file-name "fiasco" version))
         (sha256
          (base32
           "1k8i2kq57201bvy3zfpsxld530hd104dgbglxigqb6i408c1a7aw"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria sbcl-trivial-gray-streams))
      (synopsis "Simple and powerful test framework for Common Lisp")
      (description "A Common Lisp test framework that treasures your failures,
logical continuation of Stefil.  It focuses on interactive debugging.")
      (home-page "https://github.com/joaotavora/fiasco")
      ;; LICENCE specifies this is public-domain unless the legislation
      ;; doesn't allow or recognize it.  In that case it falls back to a
      ;; permissive licence.
      (license (list license:public-domain
                     (license:x11-style "file://LICENCE"))))))

(define-public cl-fiasco
  (sbcl-package->cl-source-package sbcl-fiasco))

(define-public ecl-fiasco
  (sbcl-package->ecl-package sbcl-fiasco))

(define-public sbcl-fiveam
  (package
    (name "sbcl-fiveam")
    (version "1.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sionescu/fiveam")
             (commit (string-append "v" version))))
       (file-name (git-file-name "fiveam" version))
       (sha256
        (base32 "04mh5plmlb15jbq3dkd8b9jl1dmbbg4hnd3k7859vpf6s12k5p4j"))))
    (inputs
     (list sbcl-alexandria sbcl-net.didierverna.asdf-flv
           sbcl-trivial-backtrace))
    (build-system asdf-build-system/sbcl)
    (synopsis "Common Lisp testing framework")
    (description "FiveAM is a simple (as far as writing and running tests
goes) regression testing framework.  It has been designed with Common Lisp's
interactive development model in mind.")
    (home-page "https://common-lisp.net/project/fiveam/")
    (license license:bsd-3)))

(define-public cl-fiveam
  (sbcl-package->cl-source-package sbcl-fiveam))

(define-public ecl-fiveam
  (sbcl-package->ecl-package sbcl-fiveam))

(define-public sbcl-hu.dwim.stefil
  (let ((commit "414902c6f575818c39a8a156b8b61b1adfa73dad"))
    (package
      (name "sbcl-hu.dwim.stefil")
      (version (git-version "0.0.0" "2" commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/hu-dwim/hu.dwim.stefil")
           (commit commit)))
         (sha256
          (base32 "14izmjjim590rh74swrssavdmdznj2z8vhqixy780sjhpcr5pmkc"))
         (file-name (git-file-name "hu.dwim.stefil" version))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       (list sbcl-hu.dwim.asdf))
      (inputs
       (list sbcl-alexandria))
      (home-page "http://dwim.hu/project/hu.dwim.stefil")
      (synopsis "Simple test framework")
      (description "Stefil is a simple test framework for Common Lisp,
with a focus on interactive development.")
      (license license:public-domain))))

(define-public cl-hu.dwim.stefil
  (sbcl-package->cl-source-package sbcl-hu.dwim.stefil))

(define-public ecl-hu.dwim.stefil
  (sbcl-package->ecl-package sbcl-hu.dwim.stefil))

(define-public sbcl-kaputt
  (let ((commit "f26c9b0f8219fe61d86249198ef85174eecafc10")
        (revision "1"))
    (package
      (name "sbcl-kaputt")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/foretspaisibles/cl-kaputt")
               (commit commit)))
         (file-name (git-file-name "kaputt" version))
         (sha256
          (base32 "10a78032vnf12kjjpfmq9ign38cad237ycyq37dwnx922nxjjaj4"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-cl-ppcre))
      (home-page "https://github.com/foretspaisibles/cl-kaputt")
      (synopsis "Simple interactive test framework for Common Lisp")
      (description
       "KAPUTT is a test framework for Common Lisp that focuses on the
following features:

@itemize
@item KAPUTT is simple, it only defines three abstractions testcase, assertion
and protocol and does not add any artefact on the backtrace when errors occur.

@item KAPUTT is extensible, it is possible to add problem-specific assertions
to make test code more informative.

@item KAPUTT fits well interactive development.
@end itemize\n")
      (license license:cecill-b))))

(define-public ecl-kaputt
  (sbcl-package->ecl-package sbcl-kaputt))

(define-public cl-kaputt
  (sbcl-package->cl-source-package sbcl-kaputt))

(define-public sbcl-lift
  (let ((commit "2594160d6ca3a77d8750110dfa63214256aab852")
        (revision "2"))
    (package
      (name "sbcl-lift")
      (version (git-version "1.7.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gwkkwg/lift")
               (commit commit)))
         (sha256
          (base32 "01xvz9sl5l5lai4h9dabmcjnm659wf5zllaxqbs55lffskp6jwq3"))
         (file-name (git-file-name "lift" version))
         (modules '((guix build utils)))
         (snippet
          ;; Don't keep the bundled website
          `(begin
             (delete-file-recursively "website")
             #t))))
      (build-system asdf-build-system/sbcl)
      (arguments
       ;; The tests require a debugger, but we run with the debugger disabled.
       '(#:tests? #f))
      (synopsis "LIsp Framework for Testing")
      (description
       "The LIsp Framework for Testing (LIFT) is a unit and system test tool for LISP.
Though inspired by SUnit and JUnit, it's built with Lisp in mind.  In LIFT,
testcases are organized into hierarchical testsuites each of which can have
its own fixture.  When run, a testcase can succeed, fail, or error.  LIFT
supports randomized testing, benchmarking, profiling, and reporting.")
      (home-page "https://github.com/gwkkwg/lift")
      (license license:expat))))

(define-public cl-lift
  (sbcl-package->cl-source-package sbcl-lift))

(define-public ecl-lift
  (sbcl-package->ecl-package sbcl-lift))

(define-public sbcl-lisp-unit
  (let ((commit "89653a232626b67400bf9a941f9b367da38d3815"))
    (package
      (name "sbcl-lisp-unit")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/OdonataResearchLLC/lisp-unit")
               (commit commit)))
         (sha256
          (base32
           "0p6gdmgr7p383nvd66c9y9fp2bjk4jx1lpa5p09g43hr9y9pp9ry"))
         (file-name (git-file-name "lisp-unit" version))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Common Lisp Test framework inspired by JUnit to be simple of use")
      (description
       "@command{lisp-unit} is a Common Lisp library that supports unit
testing.  It is an extension of the library written by Chris Riesbeck.")
      (home-page "https://github.com/OdonataResearchLLC/lisp-unit")
      (license license:expat))))

(define-public cl-lisp-unit
  (sbcl-package->cl-source-package sbcl-lisp-unit))

(define-public ecl-lisp-unit
  (sbcl-package->ecl-package sbcl-lisp-unit))

(define-public sbcl-lisp-unit2
  ;; There is a cyclical dependency between symbol-munger and lisp-unit2.
  ;; See https://github.com/AccelerationNet/symbol-munger/issues/4
  (let ((commit "fb9721524d1e4e73abb223ee036d74ce14a5505c")
        (revision "1"))
    (package
      (name "sbcl-lisp-unit2")
      (version (git-version "0.2.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/AccelerationNet/lisp-unit2")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1rsqy8y0jqll6xn9a593848f5wvd5ribv4csry1ly0hmdhfnqzlp"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria sbcl-cl-interpol sbcl-iterate
             sbcl-symbol-munger))
      (synopsis "Test Framework for Common Lisp")
      (description
       "LISP-UNIT2 is a Common Lisp library that supports unit testing in the
style of JUnit for Java.  It is a new version of the lisp-unit library written
by Chris Riesbeck.")
      (home-page "https://github.com/AccelerationNet/lisp-unit2")
      (license license:expat))))

(define-public cl-lisp-unit2
  (sbcl-package->cl-source-package sbcl-lisp-unit2))

(define-public ecl-lisp-unit2
  (sbcl-package->ecl-package sbcl-lisp-unit2))

(define-public sbcl-nst
  (let ((commit "6c0990f594abcf5887e8d80f1035e3b60454b61b")
        (revision "1"))
    (package
     (name "sbcl-nst")
     (version (git-version "4.1.2" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jphmrst/cl-nst")
             (commit commit)))
       (file-name (git-file-name "nst" version))
       (sha256
        (base32 "1hf3r6pqbnd9vsd1i24qmz928kia72hdgmiafiwb6jw1hmj3r6ga"))))
     (build-system asdf-build-system/sbcl)
     (inputs
      (list sbcl-closer-mop sbcl-org-sampler))
     (home-page "https://github.com/jphmrst/cl-nst")
     (synopsis "Unit testing for Common Lisp")
     (description
      "NST is a unit/regression testing system for Common Lisp.")
     (license license:llgpl))))

(define-public ecl-nst
  (sbcl-package->ecl-package sbcl-nst))

(define-public cl-nst
  (sbcl-package->cl-source-package sbcl-nst))

(define-public sbcl-parachute
  (let ((commit "ca04dd8e43010a6dfffa26dbe1d62af86008d666")
        (revision "0"))
    (package
      (name "sbcl-parachute")
      (version (git-version "1.1.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/Shinmera/parachute")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1mvsm3r0r6a2bg75nw0q7n9vlby3ch45qjl7hnb5k1z2n5x5lh60"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-documentation-utils sbcl-form-fiddle))
      (synopsis "Extensible and cross-compatible testing framework for Common Lisp")
      (description
       "Parachute is a simple-to-use and extensible testing framework.
In Parachute, things are organised as a bunch of named tests within a package.
Each test can contain a bunch of test forms that make up its body.")
      (home-page "https://shinmera.github.io/parachute/")
      (license license:zlib))))

(define-public cl-parachute
  (sbcl-package->cl-source-package sbcl-parachute))

(define-public ecl-parachute
  (sbcl-package->ecl-package sbcl-parachute))

(define-public sbcl-prove
  (let ((commit "5d71f02795b89e36f34e8c7d50e69b67ec6ca2de")
        (revision "2"))
    (package
      (name "sbcl-prove")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/fukamachi/prove")
               (commit commit)))
         (sha256
          (base32 "0ca6ha3zhmckq3ad9lxm6sbg4i0hg3m81xhan4dkxd3x9898jzpc"))
         (file-name (git-file-name "prove" version))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-alexandria sbcl-cl-colors sbcl-cl-ppcre
             sbcl-cl-ansi-text))
      (synopsis "Yet another unit testing framework for Common Lisp")
      (description
       "This project was originally called @command{cl-test-more}.
@command{prove} is yet another unit testing framework for Common Lisp.  The
advantages of @command{prove} are:

@itemize
@item Various simple functions for testing and informative error messages
@item ASDF integration
@item Extensible test reporters
@item Colorizes the report if it's available (note for SLIME)
@item Reports test durations
@end itemize\n")
      (home-page "https://github.com/fukamachi/prove")
      (license license:expat))))

(define-public cl-prove
  (sbcl-package->cl-source-package sbcl-prove))

(define-public ecl-prove
  (sbcl-package->ecl-package sbcl-prove))

(define-public sbcl-ptester
  (let ((commit "fe69fde54f4bce00ce577feb918796c293fc7253")
        (revision "1"))
    (package
      (name "sbcl-ptester")
      (version (git-version "2.1.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "http://git.kpe.io/ptester.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1l0lfl7cdnr2qf4zh38hi4llxg22c49zkm639bdkmvlkzwj3ndwf"))))
      (build-system asdf-build-system/sbcl)
      (home-page "http://quickdocs.org/ptester/")
      (synopsis "Portable test harness package")
      (description
       "@command{ptester} is a portable testing framework based on Franz's
tester module.")
      (license license:llgpl))))

(define-public cl-ptester
  (sbcl-package->cl-source-package sbcl-ptester))

(define-public ecl-ptester
  (sbcl-package->ecl-package sbcl-ptester))

(define-public sbcl-rove
  (package
    (name "sbcl-rove")
    (version "0.9.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fukamachi/rove")
             (commit "f3695db08203bf26f3b861dc22ac0f4257d3ec21")))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "07ala4l2fncxf540fzxj3h5mhi9i4wqllhj0rqk8m2ljl5zbz89q"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     (list sbcl-bordeaux-threads sbcl-dissect sbcl-trivial-gray-streams))
    (home-page "https://github.com/fukamachi/rove")
    (synopsis
     "Yet another common lisp testing library")
    (description
     "Rove is a unit testing framework for Common Lisp applications.
This is intended to be a successor of Prove.")
    (license license:bsd-3)))

(define-public cl-rove
  (sbcl-package->cl-source-package sbcl-rove))

(define-public ecl-rove
  (sbcl-package->ecl-package sbcl-rove))

(define-public sbcl-rt
  (let ((commit "a6a7503a0b47953bc7579c90f02a6dba1f6e4c5a")
        (revision "1"))
    (package
      (name "sbcl-rt")
      (version (git-version "1990.12.19" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "http://git.kpe.io/rt.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "13si2rrxaagbr0bkvg6sqicxxpyshabx6ad6byc9n2ik5ysna69b"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "MIT Regression Tester")
      (description
       "RT provides a framework for writing regression test suites.")
      (home-page "https://www.cliki.net/rt")
      (license license:expat))))

(define-public cl-rt
  (sbcl-package->cl-source-package sbcl-rt))

(define-public ecl-rt
  (sbcl-package->ecl-package sbcl-rt))

(define-public sbcl-should-test
  (let ((commit "48facb9f9c07aeceb71fc0c48ce17fd7d54a09d4")
        (revision "0"))
    (package
      (name "sbcl-should-test")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/vseloved/should-test")
               (commit commit)))
         (file-name (git-file-name "should-test" version))
         (sha256
          (base32 "1fqqa7lhf28qg60ji9libkylkcy747x576qpjn1y7c945j2fxmnm"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       (list sbcl-cl-ppcre sbcl-local-time sbcl-osicat sbcl-rutils))
      (home-page "https://github.com/vseloved/should-test")
      (synopsis "Minimal yet feature-rich Common Lisp test framework")
      (description
       "SHOULD-TEST is a methodology-agnostic and non-opinionated Common Lisp
test framework, i.e. it doesn't care what kind of test approach you'd like to
take.")
      (license license:expat))))

(define-public cl-should-test
  (sbcl-package->cl-source-package sbcl-should-test))

(define-public ecl-should-test
  (sbcl-package->ecl-package sbcl-should-test))

(define-public sbcl-stefil
  (let ((commit "0398548ec95dceb50fc2c2c03e5fb0ce49b86c7a")
        (revision "0"))
    (package
      (name "sbcl-stefil")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.common-lisp.net/stefil/stefil.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0bqz64q2szzhf91zyqyssmvrz7da6442rs01808pf3wrdq28bclh"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("iterate" ,sbcl-iterate)
         ("metabang-bind" ,sbcl-metabang-bind)
         ("swank" ,sbcl-slime-swank)))
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'drop-unnecessary-dependency
             (lambda _
               (substitute* "package.lisp"
                 ((":stefil-system") ""))
               #t)))))
      (home-page "https://common-lisp.net/project/stefil/index-old.shtml")
      (synopsis "Simple test framework")
      (description
       "Stefil is a simple test framework for Common Lisp, with a focus on
interactive development.")
      (license license:public-domain))))

(define-public cl-stefil
  (sbcl-package->cl-source-package sbcl-stefil))

(define-public ecl-stefil
  (sbcl-package->ecl-package sbcl-stefil))

(define-public sbcl-unit-test
  (let ((commit "266afaf4ac091fe0e8803bac2ae72d238144e735")
        (revision "1"))
    (package
      (name "sbcl-unit-test")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/hanshuebner/unit-test")
               (commit commit)))
         (file-name (git-file-name "unit-test" version))
         (sha256
          (base32 "11hpksz56iqkv7jw25p2a8r3n9dj922fyarn16d98589g6hdskj9"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/hanshuebner/unit-test")
      (synopsis "Unit-testing framework for Common Lisp")
      (description "This is a unit-testing framework for Common Lisp.")
      (license license:unlicense))))

(define-public ecl-unit-test
  (sbcl-package->ecl-package sbcl-unit-test))

(define-public cl-unit-test
  (sbcl-package->cl-source-package sbcl-unit-test))

(define-public sbcl-xlunit
  (let ((commit "3805d34b1d8dc77f7e0ee527a2490194292dd0fc")
        (revision "1"))
    (package
      (name "sbcl-xlunit")
      (version (git-version "0.6.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "http://git.kpe.io/xlunit.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0argfmp9nghs4sihyj3f8ch9qfib2b7ll07v5m9ziajgzsfl5xw3"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-tests
             (lambda _
               (substitute* "xlunit.asd"
                 ((" :force t") ""))
               #t)))))
      (synopsis "Unit testing package for Common Lisp")
      (description
       "The XLUnit package is a toolkit for building test suites.  It is based
on the XPTest package by Craig Brozensky and the JUnit package by Kent Beck.")
      (home-page "http://quickdocs.org/xlunit/")
      (license license:bsd-3))))

(define-public cl-xlunit
  (sbcl-package->cl-source-package sbcl-xlunit))

(define-public ecl-xlunit
  (sbcl-package->ecl-package sbcl-xlunit))
