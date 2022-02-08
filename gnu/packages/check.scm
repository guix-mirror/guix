;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014 David Thompson <davet@gnu.org>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2014, 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2017 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015, 2016, 2018, 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Christine Lemmer-Webber <cwebber@dustycloud.org>
;;; Copyright © 2016, 2017 Danny Milosavljevic <dannym+a@scratchpost.org>
;;; Copyright © 2016 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016 Troy Sankey <sankeytms@gmail.com>
;;; Copyright © 2016 Lukas Gradl <lgradl@openmailbox.org>
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2016–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017, 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017 Frederick M. Muriithi <fredmanglis@gmail.com>
;;; Copyright © 2017, 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017, 2019 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2015, 2017, 2018, 2020, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016-2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2017, 2018, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Fis Trivial <ybbs.daans@hotmail.com>
;;; Copyright © 2019, 2021 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2019 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2020 Lars-Dominik Braun <ldb@leibniz-psychology.org>
;;; Copyright © 2020 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2020 Josh Marshall <joshua.r.marshall.1991@gmail.com>
;;; Copyright © 2020 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2020 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2020, 2021, 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Hugo Lecomte <hugo.lecomte@inria.fr>
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

(define-module (gnu packages check)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages time)
  #:use-module (gnu packages xml)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (srfi srfi-1))

(define-public pict
  (package
    (name "pict")
    (version "3.7.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Microsoft/pict")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1hpff8x49ixlh71sbyhj1rircf0mg95v5q9y0ys52rhiph99wy3n"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:test-target "test"
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (replace 'install
            (lambda _
              (install-file "pict" (string-append #$output "/bin"))
              (install-file "doc/pict.md"
                            (string-append #$output
                                           "/share/doc/pict-" #$version)))))))
    (native-inputs (list perl))
    (home-page "https://www.pairwise.org/")
    (synopsis "Pairwise Independent Combinatorial Tool")
    (description "PICT is a pairwise testing tool that generates test cases
and test configurations.  With PICT, you can generate tests that are more
effective than manually generated tests and in a fraction of the time required
by hands-on test case design.  PICT runs as a command line tool.  It takes a
model file detailing the parameters of the interface as an input and generates
a compact set of parameter value choices that represent the test cases you
should use to get comprehensive combinatorial coverage of your parameters.")
    (license license:expat)))

(define-public pedansee
  (package
    (name "pedansee")
    (version "0.0.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://www.flyn.org/projects/"
                       name "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "0lsg791x6n95pxg6vif8qfc46nqcamhjq3g0dl5xqf6imy7n3acd"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     (list clang pkg-config python-wrapper))
    (inputs
     (list glib))
    (synopsis "Code checker for C")
    (description "Pedansee checks C source files for compliance with a particular
programming style.  The style is currently defined by the pedansee source code
in the form of functions which walk each source file’s syntax tree.  You can
modify some aspects of this style through the use of regular expressions.")
    (home-page "https://www.flyn.org/projects/pedansee/")
    (license license:gpl3+)))

(define-public mutest
  (package
    (name "mutest")
    (version "0.0.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/ebassi/mutest")
         (commit "e6246c9")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gdqwq6fvk06wld4rhnw5752hahrvhd69zrci045x25rwx90x26q"))))
    (build-system meson-build-system)
    (synopsis "Small C testing library")
    (description "Mutest aims to be a small unit testing library for C projects,
with an API heavily modelled on high level Behavior-Driver Development frameworks
like Jasmine or Mocha.")
    (home-page "https://ebassi.github.io/mutest/mutest.md.html")
    (license license:expat)))

(define-public check
  (package
    (name "check")
    (version "0.15.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://github.com/libcheck/check/releases/download/"
                          version "/check-" version ".tar.gz"))
      (sha256
       (base32
        "02m25y9m46pb6n46s51av62kpd936lkfv3b13kfpckgvmh5lxpm8"))
      (patches
       (list
        ;; This patch fixes some tests that would otherwise fail on
        ;; powerpc64le-linux.  Without this patch, the tests make certain
        ;; assumptions about floating point number precision that are not true
        ;; on that platform.
        ;;
        ;; TODO: Remove this patch when updating to the next check release,
        ;; since it will be included there.  See:
        ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=47698
        (origin
          (method url-fetch)
          (uri
           (string-append "https://github.com/libcheck/check/commit/"
                          "4fbe702fa4f35bee8a90512f9f59d1441c4ae82e.patch"))
          (file-name (string-append name
                                    "-fix-test-precision-for-ppc.patch"))
          (sha256
           (base32
            "04qg1p9afdd6453k18qskazrvscysdcjz9j6w4i6p5x4xyma19v6")))))))
    (build-system gnu-build-system)
    (home-page "https://libcheck.github.io/check/")
    (synopsis "Unit test framework for C")
    (description
     "Check is a unit testing framework for C.  It features a simple
interface for defining unit tests, putting little in the way of the
developer.  Tests are run in a separate address space, so Check can
catch both assertion failures and code errors that cause segmentation
faults or other signals.  The output from unit tests can be used within
source code editors and IDEs.")
    (license license:lgpl2.1+)))

;; Some packages require older versions.  Removed once no longer needed.
(define-public check-0.14
  (package
    (inherit check)
    (version "0.14.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/libcheck/check/releases"
                                  "/download/" version "/check-" version ".tar.gz"))
              (sha256
               (base32
                "02zkfiyklckmivrfvdsrlzvzphkdsgjrz3igncw05dv5pshhq3xx"))))))

(define-public check-0.12
  (package
   (inherit check)
   (version "0.12.0")
   (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/libcheck/check/releases"
                                 "/download/" version "/check-" version ".tar.gz"))
             (sha256
              (base32
               "0d22h8xshmbpl9hba9ch3xj8vb9ybm5akpsbbh7yj07fic4h2hj6"))))))

(define-public clitest
  (package
    (name "clitest")
    (version "0.4.0")
    (home-page "https://github.com/aureliojargas/clitest")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1p745mxiq3hgi3ywfljs5sa1psi06awwjxzw0j9c2xx1b09yqv4a"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; This package is distributed as a single shell script and comes
          ;; without a proper build system.
          (delete 'configure)
          (delete 'build)
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (substitute* "test.md"
                  ;; One test looks for an error from grep in the form "grep: foo",
                  ;; but our grep returns the absolute file name on errors.  Adjust
                  ;; the test to cope with that.
                  (("sed 's/\\^e\\*grep: \\.\\*/")
                   "sed 's/.*e*grep: .*/"))

                (setenv "HOME" "/tmp")
                (invoke "./clitest" "test.md"))))
          (replace 'install
            (lambda _
              (install-file "clitest" (string-append #$output "/bin"))
              (install-file "README.md"
                            (string-append #$output "/share/doc/clitest-"
                                           #$(package-version this-package))))))))
    (native-inputs
     (list perl))                 ;for tests
    (inputs
     (list bash-minimal))
    (synopsis "Command line test tool")
    (description
     "@command{clitest} is a portable shell script that performs automatic
testing of Unix command lines.")
    (license license:expat)))

(define-public cunit
  (package
    (name "cunit")
    (version "2.1-3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/cunit/CUnit/"
                           version "/CUnit-" version ".tar.bz2"))
       (sha256
        (base32
         "057j82da9vv4li4z5ri3227ybd18nzyq81f6gsvhifs5z0vr3cpm"))))
    (build-system gnu-build-system)
    (arguments '(#:phases
                 (modify-phases %standard-phases
                   ;; XXX: The "bootstrap" phase detects the "bootstrap"
                   ;; script, but fails to execute it, so we bootstrap
                   ;; manually.
                   (replace 'bootstrap
                     (lambda _ (invoke "autoreconf" "-vfi"))))))
    (native-inputs
     (list automake autoconf libtool))
    (home-page "http://cunit.sourceforge.net/")
    (synopsis "Automated testing framework for C")
    (description
     "CUnit is a lightweight system for writing, administering, and running
unit tests in C.  It provides C programmers with basic testing functionality
with a flexible variety of user interfaces.")
    (license license:gpl2+)))

(define-public cppunit
  (package
    (name "cppunit")
    (version "1.15.1")
    (source (origin
             (method url-fetch)
              (uri (string-append "http://dev-www.libreoffice.org/src/"
                                  name "-" version ".tar.gz"))
             (sha256
              (base32
               "19qpqzy66bq76wcyadmi3zahk5v1ll2kig1nvg96zx9padkcdic9"))))
    ;; Explicitly link with libdl. This is expected to be done by packages
    ;; relying on cppunit for their tests. However, not all of them do.
    ;; If we added the linker flag to such packages, we would pollute all
    ;; binaries, not only those used for testing.
    (arguments
     `(#:make-flags '("LDFLAGS=-ldl")))
    (build-system gnu-build-system)
    (home-page "https://wiki.freedesktop.org/www/Software/cppunit/")
    (synopsis "Unit testing framework for C++")
    (description "CppUnit is the C++ port of the famous JUnit framework for
unit testing.  Test output is in XML for automatic testing and GUI based for
supervised tests.")
    (license license:lgpl2.1))) ; no copyright notices. LGPL2.1 is in the tarball

(define-public shunit2
  (package
    (name "shunit2")
    (version "2.1.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/kward/shunit2")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "08vs0jjl3pfh100sjlw31x4638xj7fghr0j2g1zfikba8n1f9491"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)    ; no configure script
         (delete 'build)
         (add-after 'patch-source-shebangs 'patch-more-shebangs
           (lambda _
             (substitute* "shunit2"
               (("#! /bin/sh") (string-append "#! " (which "sh")))
               (("/usr/bin/od") (which "od")))
             (substitute* "test_runner"
               (("/bin/sh") (which "sh"))
               (("/bin/bash") (which "bash")))
             #t))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; This test is buggy in the build container.
               (delete-file "shunit2_misc_test.sh")
               (invoke "sh" "test_runner"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "shunit2"
                           (string-append (assoc-ref outputs "out")
                                          "/bin"))
             #t)))))
    (home-page "https://github.com/kward/shunit2")
    (synopsis "@code{xUnit} based unit testing for Unix shell scripts")
    (description "@code{shUnit2} was originally developed to provide a
consistent testing solution for @code{log4sh}, a shell based logging framework
similar to @code{log4j}.  It is designed to work in a similar manner to JUnit,
PyUnit and others.")
    (license license:asl2.0)))

;; When dependent packages upgraded to use newer version of catch, this one should
;; be removed.
(define-public catch-framework
  (package
    (name "catch")
    (version "1.3.5")                  ;Sub-minor is the build number
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/philsquared/Catch")
                    ;; Semi-arbitrary.
                    (commit "ae5ee2cf63d6d67bd1369b512d2a7b60b571c907")))
              (file-name (string-append name "-" version))
              (sha256
               (base32
                "1yfb3lxv929szqy1nw9xw3d45wzkppziqshkjxvrb1fdmf46x564"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let* ((source (assoc-ref %build-inputs "source"))
                          (output (assoc-ref %outputs "out"))
                          (incdir (string-append output "/include"))
                          (docdir (string-append output "/share/doc/catch-"
                                                 ,version)))
                     (for-each mkdir-p (list incdir docdir))
                     (install-file (string-append source
                                                  "/single_include/catch.hpp")
                                   incdir)
                     (copy-recursively (string-append source "/docs")
                                       docdir)
                     #t))))
    (home-page "http://catch-lib.net/")
    (synopsis "Automated test framework for C++ and Objective-C")
    (description
     "Catch stands for C++ Automated Test Cases in Headers and is a
multi-paradigm automated test framework for C++ and Objective-C.")
    (license license:boost1.0)))

(define-public catch-framework2-1
  (package
    (name "catch2")
    (version "1.12.2")
    (home-page "https://github.com/catchorg/Catch2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/catchorg/Catch2")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1gdp5wm8khn02g2miz381llw3191k7309qj8s3jd6sasj01rhf23"))))
    (build-system cmake-build-system)
    (synopsis "Automated test framework for C++ and Objective-C")
    (description "Catch2 stands for C++ Automated Test Cases in Headers and is
a multi-paradigm automated test framework for C++ and Objective-C.")
    (license license:boost1.0)))

(define-public catch-framework2
  (package
    (name "catch2")
    (version "2.13.8")
    (home-page "https://github.com/catchorg/Catch2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/catchorg/Catch2")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "18a6d7rcb6ilhxd5dff32jkfdf2ik58pbywrv04ras70217kdq4c"))))
    (build-system cmake-build-system)
    (inputs
     (list python-wrapper))
    (synopsis "Automated test framework for C++ and Objective-C")
    (description "Catch2 stands for C++ Automated Test Cases in Headers and is
a multi-paradigm automated test framework for C++ and Objective-C.")
    (license license:boost1.0)))

(define-public cmdtest
  (package
    (name "cmdtest")
    ;; Use the latest commit (from 2019) in order to get Python 3 support.
    (version "0.32-14-gcdfe14e")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "git://git.liw.fi/cmdtest/")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1yhcwsqcpckkq5kw3h07k0xg6infyiyzq9ni3nqphrzxis7hxjf1"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f))          ;requires Python 2!
    (native-inputs
     (list python-coverage-test-runner python))
    (inputs
     (list python-cliapp python-markdown python-ttystatus))
    (home-page "https://liw.fi/cmdtest/")
    (synopsis "Black box Unix program tester")
    (description
     "@code{cmdtest} black box tests Unix command line tools.  Roughly, it is
given a command line and input files, and the expected output, and it verifies
that the command line produces the expected output.  If not, it reports a
problem, and shows the differences.")
    (license license:gpl3+)))

(define-public cmocka
  (package
    (name "cmocka")
    (version "1.1.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://cmocka.org/files/"
                                  (version-major+minor version) "/cmocka-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1dm8pdvkyfa8dsbz9bpq7wwgixjij4sii9bbn5sgvqjm5ljdik7h"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ; no test target
    (home-page "https://cmocka.org/")
    (synopsis "Unit testing framework for C")
    (description "Cmocka is a unit testing framework for C with support for
mock objects.  It only requires the standard C library, and works with
different compilers.  Cmocka supports several different message output formats
like Test Anything Protocol, Subunit, xUnit XML or the original cmockery output
format.")
    (license license:asl2.0)))

(define-public cppcheck
  (package
    (name "cppcheck")
    (version "2.3")
    (source (origin
      (method git-fetch)
      (uri (git-reference
             (url "https://github.com/danmar/cppcheck")
             (commit version)))
      (file-name (git-file-name name version))
      (sha256
       (base32 "03ic5mig3ryzkf85r95ryagf84s7y5nd6sqr915l3zj30apnifvz"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '("-DBUILD_TESTS=ON")))
    (home-page "http://cppcheck.sourceforge.net")
    (synopsis "Static C/C++ code analyzer")
    (description "Cppcheck is a static code analyzer for C and C++.  Unlike
C/C++ compilers and many other analysis tools it does not detect syntax errors
in the code.  Cppcheck primarily detects the types of bugs that the compilers
normally do not detect.  The goal is to detect only real errors in the code
(i.e. have zero false positives).")
    (license license:gpl3+)))

(define-public cxxtest
  (package
    (name "cxxtest")
    (version "4.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/cxxtest/cxxtest/"
                                  version "/cxxtest-" version ".tar.gz"))
              (sha256
               (base32
                "1n7pbj4z9ivx005hqvivj9ddhq8awynzg6jishfbypf6j7ply58w"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir-to-source
           (lambda _
             (chdir "python")
             #t))
         (add-after 'install 'install-headers
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (include-dir (string-append out "/include/cxxtest")))
               (for-each (lambda (header-file)
                           (install-file header-file include-dir))
                         (find-files "../cxxtest"))
               #t)))
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc-dir (string-append out "/share/doc/cxxtest")))
               (install-file "../README" doc-dir)
               (install-file "../doc/guide.txt" doc-dir)
               (copy-recursively "../sample" (string-append doc-dir "/sample"))
               #t))))))
    (propagated-inputs
     (list python-ply))
    (home-page "https://cxxtest.com/")
    (synopsis "Unit testing framework for C++")
    (description "CxxTest is a unit testing framework for C++ that is similar
in spirit to JUnit, CppUnit, and xUnit.  CxxTest does not require precompiling
a CxxTest testing library, it employs no advanced features of C++ (e.g. RTTI)
and it supports a very flexible form of test discovery.")
    (license license:lgpl3+)))

(define-public doctest
  (package
    (name "doctest")
    (version "2.4.8")
    (home-page "https://github.com/onqtam/doctest")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "057wdkv3gcz42mh1j284sgvm16i5fk1f9b1plgvavca70q4p52gz"))))
    (build-system cmake-build-system)
    (synopsis "C++ test framework")
    (description
     "doctest is a single-header testing framework for C++11 and later.  It
has been designed to be fast, light and unintrusive.")
    (license license:expat)))

(define-public go-github.com-smartystreets-gunit
  (package
    (name "go-github.com-smartystreets-gunit")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/smartystreets/gunit")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00m4zg0kdj49mnpmf9klb44ba71p966xsk6zknrzqgfc8119f35z"))))
    (build-system go-build-system)
    (arguments
     '(;; TODO: This package depends on go-github.com-smartystreets-assertions
       ;; for running the tests, but go-github.com-smartystreets-assertions
       ;; depends on this package, so break this loop by not running the tests
       ;; for this package.
       #:tests? #f
       #:import-path "github.com/smartystreets/gunit"))
    (synopsis "Testing tool for Go, in the style of xUnit")
    (description
     "@code{gunit} allows the test author to use a struct as the scope for a
group of related test cases, in the style of xUnit fixtures.  This makes
extraction of setup/teardown behavior (as well as invoking the system under
test) much simpler.")
    (home-page "https://github.com/smartystreets/gunit")
    (license license:expat)))

(define-public go-github.com-smartystreets-assertions
  (package
    (name "go-github.com-smartystreets-assertions")
    (version "1.8.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/smartystreets/assertions")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1j0adgbykl55rf2945g0n5bmqdsnjcqlx5dcmpfh4chki43hiwg9"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/smartystreets/assertions"))
    (native-inputs
     (list go-github.com-smartystreets-gunit))
    (synopsis "Assertions for testing with Go")
    (description
     "The @code{assertions} package provides convenient assertion functions
for writing tests in Go.")
    (home-page "https://github.com/smartystreets/assertions")
    (license license:expat)))

(define-public go-github.com-smartystreets-goconvey
  (package
    (name "go-github.com-smartystreets-goconvey")
    (version "1.6.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/smartystreets/goconvey")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ph18rkl3ns3fgin5i4j54w5a69grrmf3apcsmnpdn1wlrbs3dxh"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/smartystreets/goconvey"))
    (propagated-inputs
     (list go-github.com-jtolds-gls go-github.com-smartystreets-assertions))
    (synopsis "Go testing tool with both a web and terminal user interface")
    (description
     "GoConvey is a testing tool for Go. It integrates with go test, can show
test coverage and has a web user interface that will refresh automatically.")
    (home-page "https://github.com/smartystreets/goconvey")
    (license license:expat)))

(define-public googletest
  (package
    (name "googletest")
    (version "1.11.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/googletest")
             (commit (string-append "release-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pd4y1gpx1z8fiyarkvqlmk6hbv0lc8fr00ivnsvqzi1xg34jfaa"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DBUILD_SHARED_LIBS=ON")))
    (native-inputs
     `(("python" ,python-wrapper)))
    (home-page "https://github.com/google/googletest/")
    (synopsis "Test discovery and XUnit test framework")
    (description "Google Test features an XUnit test framework, automated test
discovery, death tests, assertions, parameterized tests and XML test report
generation.")
    (license license:bsd-3)))

(define-public googletest-1.8
  (package
    (inherit googletest)
   (version "1.8.1")
   (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/google/googletest")
                   (commit (string-append "release-" version))))
             (file-name (git-file-name "googletest" version))
             (sha256
              (base32
               "0270msj6n7mggh4xqqjp54kswbl7mkcc8px1p5dqdpmw5ngh9fzk"))))))

(define-public googlebenchmark
  (package
    (name "googlebenchmark")
    (version "1.5.3")
    (home-page "https://github.com/google/benchmark")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (file-name (git-file-name "google-benchmark" version))
              (sha256
               (base32
                "1hls0aqqj5cfldn9jfpvzjhpxkhrydrz9crp477rwllwjsybdxw7"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags (list "-DBUILD_SHARED_LIBS=ON"
                               (string-append
                                "-DGOOGLETEST_PATH="
                                (assoc-ref %build-inputs "googletest")))))
    (inputs
     `(("googletest" ,(package-source googletest))))
    (synopsis "C++ library to support the benchmarking of functions")
    (description
     "The googlebenchmark C++ library support the benchmarking of functions,
similar to unit tests.")
    (license license:asl2.0)))

(define-public cpputest
  (package
    (name "cpputest")
    (version "4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/cpputest/cpputest/releases/download/v"
                           version "/cpputest-" version ".tar.gz"))
       (sha256
        (base32
         "1xslavlb1974y5xvs8n1j9zkk05dlw8imy4saasrjlmibl895ii1"))))
    (build-system gnu-build-system)
    (native-inputs
     (list googletest))
    (home-page "https://cpputest.github.io/")
    (synopsis "Unit testing and mocking framework for C/C++")
    (description
     "CppUTest is a C/C++ based unit xUnit test framework.  It is written in
C++ but is used in C and C++ projects and frequently used in embedded systems
but it works for any C/C++ project.")
    (license license:bsd-3)))

(define-public python-parameterized
  (package
    (name "python-parameterized")
    (version "0.7.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "parameterized" version))
       (sha256
        (base32 "1444fdz5bj0k10nmhxv0bv2gfrfisi7hfzqdndb0pvhf4g3qq3qr"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (if tests?
                          (invoke "nosetests" "-v")
                          (format #t "test suite not run~%"))
                      #t)))))
    (native-inputs
     (list python-mock python-nose))
    (home-page "https://github.com/wolever/parameterized")
    (synopsis "Parameterized testing with any Python test framework")
    (description
     "Parameterized is a Python library that aims to fix parameterized testing
for every Python test framework.  It supports nose, py.test, and unittest.")
    (properties `((python2-variant . ,(delay python2-parameterized))))
    (license license:bsd-2)))

(define-public python2-parameterized
  (let ((base (package-with-python2 (strip-python2-variant
                                     python-parameterized))))
    (package/inherit
     base
     (source
      (origin
        (inherit (package-source base))
        (patches (search-patches "python2-parameterized-docstring-test.patch")))))))

(define-public python-minimock
  (package
    (name "python-minimock")
    (version "1.2.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "MiniMock" version))
       (sha256
        (base32
         "0k2sxb1ibnyg05iblz7zhbv825f1zk9906rab7883iqgvzmdzpsz"))))
    (build-system python-build-system)
    (home-page "https://pypi.org/project/MiniMock")
    (synopsis "Simple Python library for using mock objects")
    (description "MiniMock is a simple library for building mock objects with
doctest.")
    (license license:expat)))

(define-public python2-minimock
  (package-with-python2 python-minimock))

(define-public python-mock
  (package
    (name "python-mock")
    (version "3.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mock" version))
       (sha256
        (base32
         "1hrp6j0yrx2xzylfv02qa8kph661m6yq4p0mc8fnimch9j4psrc3"))))
    (propagated-inputs
     (list python-six))
    (build-system python-build-system)
    (arguments
     ;; FIXME: Tests require "pytest", which depends on this package.
     '(#:tests? #f))
    (home-page "https://github.com/testing-cabal/mock")
    (synopsis "Python mocking and patching library for testing")
    (description
     "Mock is a library for testing in Python.  It allows you to replace parts
of your system under test with mock objects and make assertions about how they
have been used.  This library is now part of Python (since Python 3.3),
available via the @code{unittest.mock} module.")
    (properties `((python2-variant . ,(delay python2-mock))))
    (license license:expat)))

(define-public python2-mock
  (let ((base (package-with-python2
               (strip-python2-variant python-mock))))
    (package/inherit base
      (propagated-inputs
       `(("python2-functools32" ,python2-functools32)
         ("python2-funcsigs" ,python2-funcsigs)
         ,@(package-propagated-inputs base))))))

;;; This package is unmaintained (see the note at the top of doc/index.rst).
(define-public python-nose
  (package
    (name "python-nose")
    (version "1.3.7")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "nose" version))
        (sha256
          (base32
            "164a43k7k2wsqqk1s6vavcdamvss4mz0vd6pwzv2h9n8rgwzxgzi"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'invoke-2to3
                    (lambda _
                      (invoke "2to3" "-w" "."))))))
    (home-page "http://readthedocs.org/docs/nose/")
    (synopsis "Python testing library")
    (description
     "Nose extends the unittest library to make testing easier.")
    (license license:lgpl2.0+)
    (properties `((python2-variant . ,(delay python2-nose))))))

(define-public python2-nose
  (let ((base (package-with-python2
               (strip-python2-variant python-nose))))
    (package/inherit base
      (arguments (substitute-keyword-arguments (package-arguments base)
                   ((#:phases phases)
                    `(modify-phases ,phases
                       (delete 'invoke-2to3))))))))

(define-public python-nose2
  (package
    (name "python-nose2")
    (version "0.9.2")
      (source
        (origin
          (method url-fetch)
          (uri (pypi-uri "nose2" version))
          (sha256
           (base32
            "0pmbb6nk31yhgh4zkcblzxsznml7f7pf5q1ihgrwvbxv4mwzfql7"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f)) ; 'module' object has no attribute 'collector'
    (propagated-inputs
     (list python-cov-core python-pytest-cov python-six))
    (home-page "https://github.com/nose-devs/nose2")
    (synopsis "Next generation of nicer testing for Python")
    (description
     "Nose2 is the next generation of nicer testing for Python, based on the
plugins branch of unittest2.  Nose2 aims to improve on nose by providing a
better plugin api, being easier for users to configure, and simplifying internal
interfaces and processes.")
    (license license:bsd-2)))

(define-public python2-nose2
  (package-with-python2 python-nose2))

(define-public python-unittest2
  (package
    (name "python-unittest2")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "unittest2" version))
       (patches
        (search-patches "python-unittest2-python3-compat.patch"
                        "python-unittest2-remove-argparse.patch"))
       (sha256
        (base32
         "0y855kmx7a8rnf81d3lh5lyxai1908xjp0laf4glwa4c8472m212"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (zero? (system* "python" "-m" "unittest2" "discover" "--verbose")))))))
    (propagated-inputs
     (list python-six python-traceback2))
    (home-page "https://pypi.org/project/unittest2/")
    (synopsis "Python unit testing library")
    (description
     "Unittest2 is a replacement for the unittest module in the Python
standard library.")
    (license license:psfl)))

(define-public python2-unittest2
  (package-with-python2 python-unittest2))

(define-public python-pytest
  (package
    (name "python-pytest")
    (version "6.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest" version))
       (sha256
        (base32
         "12cyi0lnyaq8sdqfnqlppd76gkw6zcg10gyih5knx9v611l3c6qk"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'pretend-version
           ;; The version string is usually derived via setuptools-scm, but
           ;; without the git metadata available, the version string is set to
           ;; '0.0.0'.
           (lambda _
             (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" ,version)))
         (replace 'check
           (lambda* (#:key (tests? #t) #:allow-other-keys)
             (setenv "TERM" "dumb")     ;attempt disabling markup tests
             (if tests?
                 (invoke "pytest" "-vv" "-k"
                         (string-append
                          ;; This test involves the /usr directory, and fails.
                          " not test_argcomplete"
                          ;; These test do not honor the isatty detection and
                          ;; fail.
                          " and not test_code_highlight"
                          " and not test_color_yes"))
                 (format #t "test suite not run~%")))))))
    (propagated-inputs
     `(("python-attrs" ,python-attrs-bootstrap)
       ("python-iniconfig" ,python-iniconfig)
       ("python-more-itertools" ,python-more-itertools)
       ("python-packaging" ,python-packaging-bootstrap)
       ("python-pluggy" ,python-pluggy)
       ("python-py" ,python-py)
       ("python-six" ,python-six-bootstrap)
       ("python-toml" ,python-toml)
       ("python-wcwidth" ,python-wcwidth)))
    (native-inputs
     `(;; Tests need the "regular" bash since 'bash-final' lacks `compgen`.
       ("bash" ,bash)
       ("python-hypothesis" ,python-hypothesis)
       ("python-nose" ,python-nose)
       ("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest-bootstrap)
       ("python-setuptools-scm" ,python-setuptools-scm)
       ("python-toml" ,python-toml)
       ("python-xmlschema" ,python-xmlschema)))
    (home-page "https://docs.pytest.org/en/latest/")
    (synopsis "Python testing library")
    (description
     "Pytest is a testing tool that provides auto-discovery of test modules
and functions, detailed info on failing assert statements, modular fixtures,
and many external plugins.")
    (license license:expat)
    (properties `((python2-variant . ,(delay python2-pytest))))))

(define-public python-pytest-6 python-pytest)

;; Pytest 4.x are the last versions that support Python 2.
(define-public python2-pytest
  (package
    (inherit (strip-python2-variant python-pytest))
    (name "python2-pytest")
    (version "4.6.11")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pytest" version))
              (sha256
               (base32
                "0ls3pqr86xgif6bphsb6wrww9r2vc7p7a2naq8zcq8115wwq5yjh"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       ,@(package-arguments python-pytest)))
    (propagated-inputs
     `(("python-atomicwrites" ,python2-atomicwrites)
       ("python-attrs" ,python2-attrs-bootstrap)
       ("python-funcsigs" ,python2-funcsigs)
       ("python-importlib-metadata" ,python2-importlib-metadata-bootstrap)
       ("python-more-itertools" ,python2-more-itertools)
       ("python-packaging" ,python2-packaging-bootstrap)
       ("python-pathlib2" ,python2-pathlib2)
       ("python-pluggy" ,python2-pluggy)
       ("python-py" ,python2-py)
       ("python-six" ,python2-six-bootstrap)
       ("python-wcwidth" ,python2-wcwidth)))
    (native-inputs
     `(("bash" ,bash)                   ;tests require 'compgen'
       ("python-hypothesis" ,python2-hypothesis)
       ("python-nose" ,python2-nose)
       ("python-mock" ,python2-mock)
       ("python-pytest" ,python2-pytest-bootstrap)
       ("python-setuptools-scm" ,python2-setuptools-scm)))))

(define-public python-pytest-bootstrap
  (package
    (inherit (strip-python2-variant python-pytest))
    (name "python-pytest-bootstrap")
    (native-inputs (list python-iniconfig python-setuptools-scm
                         python-toml))
    (arguments `(#:tests? #f))
    (properties `((python2-variant . ,(delay python2-pytest-bootstrap))))))

(define-public python2-pytest-bootstrap
  (hidden-package
   (package/inherit
    python2-pytest
    (name "python2-pytest-bootstrap")
    (arguments
     (substitute-keyword-arguments (package-arguments python2-pytest)
       ((#:tests? _ #f) #f)))
    (native-inputs
     `(("python-setuptools-scm" ,python2-setuptools-scm)))
     (propagated-inputs
      `(("python-atomicwrites" ,python2-atomicwrites)
        ("python-attrs" ,python2-attrs-bootstrap)
        ("python-funcsigs" ,python2-funcsigs-bootstrap)
        ("python-importlib-metadata" ,python2-importlib-metadata-bootstrap)
        ("python-more-itertools" ,python2-more-itertools)
        ("python-packaging" ,python2-packaging-bootstrap)
        ("python-pathlib2" ,python2-pathlib2-bootstrap)
        ("python-pluggy" ,python2-pluggy-bootstrap)
        ("python-py" ,python2-py)
        ("python-wcwidth" ,python2-wcwidth))))))

(define-public python-pytest-assume
  (package
    (name "python-pytest-assume")
    (version "2.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-assume" version))
       (sha256
        (base32 "0zilqsy9fcjr6l2f9qzfxpkp40h24csnjm5mifhpmzb0fr9r0glq"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests?
                        (invoke "pytest")))))))
    (propagated-inputs
     (list python-pytest python-six))
    (home-page "https://github.com/astraw38/pytest-assume")
    (synopsis "Pytest plugin that allows multiple failures per test")

    (description "This package provides a Pytest plugin that allows multiple
failures per test.  This is a fork from pytest-expect which includes the
following improvements:
@itemize
@item showlocals support (the Pytest option)
@item global usage support (a fixture is not required)
@item output refinements and tweaks.
@end itemize")
    (license license:expat)))

(define-public python-pytest-cov
  (package
    (name "python-pytest-cov")
    (version "2.8.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pytest-cov" version))
        (sha256
         (base32 "0avzlk9p4nc44k7lpx9109dybq71xqnggxb9f4hp0l64pbc44ryc"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
          (lambda _
            ;; Options taken from tox.ini.
            ;; TODO: make "--restructuredtext" tests pass. They currently fail
            ;; with "Duplicate implicit target name".
            (invoke "python" "./setup.py" "check"
                    "--strict" "--metadata"))))))
    (propagated-inputs
     (list python-coverage python-pytest))
    (home-page "https://github.com/pytest-dev/pytest-cov")
    (synopsis "Pytest plugin for measuring coverage")
    (description
     "Pytest-cov produces coverage reports.  It supports centralised testing and
distributed testing in both @code{load} and @code{each} modes.  It also
supports coverage of subprocesses.")
  (license license:expat)))

(define-public python2-pytest-cov
  (package-with-python2 python-pytest-cov))

(define-public python-pytest-httpserver
  (package
    (name "python-pytest-httpserver")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pytest_httpserver" version))
              (sha256
               (base32
                "0vbls0j570l5my83j4jnk5blmnir44i0w511azlh41nl6k8rac5f"))))
    (build-system python-build-system)
    (native-inputs
     (list python-pytest))
    (propagated-inputs
     (list python-werkzeug))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-library-loading
           (lambda _
             (setenv "GUIX_PYTHONPATH" (string-append (getenv "GUIX_PYTHONPATH") ":."))))
         (replace 'check
           (lambda _
             (invoke "pytest" "tests" "-vv")
             (invoke "pytest" "tests" "-vv" "--ssl"))))))
    (home-page "https://github.com/csernazs/pytest-httpserver")
    (synopsis "HTTP server for pytest")
    (description "Pytest plugin library to test http clients without
contacting the real http server.")
    (license license:expat)))

(define-public python-pytest-random-order
  (package
    (name "python-pytest-random-order")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-random-order" version))
       (sha256
        (base32 "0lpzl218l04vjy4gckrbrcacc3w9xrjnvz64bf2i132c58s5j8bb"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests?
                        (invoke "python" "-m" "pytest" "--random-order")))))))
    (propagated-inputs
     (list python-pytest))
    (home-page "https://github.com/jbasko/pytest-random-order")
    (synopsis "Pytest plugin to randomize the order of tests")
    (description "@code{pytest-random-order} is a Pytest plugin that
randomizes the order of tests.  This can be useful to detect a test that
passes just because it happens to run after an unrelated test that leaves the
system in a favourable state.  The plugin allows user to control the level of
randomness they want to introduce and to disable reordering on subsets of
tests.  Tests can be rerun in a specific order by passing a seed value
reported in a previous test run.")
    (license license:expat)))

(define-public python-pytest-runner
  (package
    (name "python-pytest-runner")
    (version "5.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-runner" version))
       (sha256
        (base32
         "0awll1bva5zy8cspsxcpv7pjcrdf5c6pf56nqn4f74vvmlzfgiwn"))))
    (build-system python-build-system)
    (arguments
     '(;; FIXME: The test suite requires 'python-flake8' and 'python-black',
       ;; but that introduces a circular dependency.
       #:tests? #f
       #:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (if tests?
                          (invoke "pytest" "-vv")
                          (format #t "test suite not run~%"))
                      #t)))))
    (native-inputs
     (list python-setuptools-scm))
    (home-page "https://github.com/pytest-dev/pytest-runner")
    (synopsis "Invoke py.test as a distutils command")
    (description
     "This package provides a @command{pytest-runner} command that
@file{setup.py} files can use to run tests.")
    (license license:expat)))

(define-public python2-pytest-runner
  (package-with-python2 python-pytest-runner))

;; python-bleach 3.1.0 requires this ancient version of pytest-runner.
;; Remove once no longer needed.
(define-public python-pytest-runner-2
  (package
    (inherit python-pytest-runner)
   (version "2.12.2")
   (source (origin
             (method url-fetch)
             (uri (pypi-uri "pytest-runner" version))
             (sha256
              (base32
               "11ivjj9hfphkv4yfb2g74av4yy86y8gcbf7gbif0p1hcdfnxg3w6"))))))

(define-public python2-pytest-runner-2
  (package-with-python2 python-pytest-runner-2))

(define-public python-pytest-lazy-fixture
  (package
    (name "python-pytest-lazy-fixture")
    (version "0.6.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pytest-lazy-fixture" version))
        (sha256
         (base32 "1b0hmnsxw4s2wf9pks8dg6dfy5cx3zcbzs8517lfccxsfizhqz8f"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Make the installed plugin discoverable by Pytest.
             (add-installed-pythonpath inputs outputs)
             (invoke "pytest" "-vv"))))))
    (propagated-inputs
     (list python-pytest))
    (home-page "https://github.com/tvorog/pytest-lazy-fixture")
    (synopsis "Use fixtures in @code{pytest.mark.parametrize}")
    (description "This plugin helps to use fixtures in
@code{pytest.mark.parametrize}.")
    (license license:expat)))

(define-public python-pytest-mock
  (package
    (name "python-pytest-mock")
    (version "3.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-mock" version))
       (sha256
        (base32 "0qhfmd05z3g88bnwq6644jl6p5wy01i4yy7h8883z9jjih2pl8a0"))
       (modules '((guix build utils)))
       (snippet
        ;; Some tests do a string match on Pytest output, and fails when
        ;; warnings are present.  Adjust to cope with warnings from
        ;; third-party libraries (looking at you, pytest-asyncio).
        '(substitute* "tests/test_pytest_mock.py"
           (("1 passed in \\*")
            "1 passed*")))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; Skip the assertion rewriting tests, which don't work in the
               ;; presence of read-only Python modules (a limitation of
               ;; Pytest).  Also skip the "test_standalone_mock" test, which
               ;; can only work when 'python-mock' is not available
               ;; (currently propagated by Pytest 5).
               (invoke "pytest" "--assert=plain" "-vv"
                       "-k" "not test_standalone_mock")))))))
    (native-inputs
     (list python-pytest-asyncio python-setuptools-scm))
    (propagated-inputs
     (list python-pytest))
    (home-page "https://github.com/pytest-dev/pytest-mock/")
    (synopsis "Thin-wrapper around the mock package for easier use with py.test")
    (description
     "This plugin installs a @code{mocker} fixture which is a thin-wrapper
around the patching API provided by the @code{mock} package, but with the
benefit of not having to worry about undoing patches at the end of a test.
The mocker fixture has the same API as @code{mock.patch}, supporting the
same arguments.")
    (properties `((python2-variant . ,(delay python2-pytest-mock))))
    (license license:expat)))

(define-public python2-pytest-mock
  (let ((base (package-with-python2
               (strip-python2-variant python-pytest-mock))))
    (package/inherit base
      (version "1.10.1")
      (source
       (origin
         (method url-fetch)
         (uri (pypi-uri "pytest-mock" version))
         (sha256
          (base32
           "1i5mg3ff1qk0wqfcxfz60hwy3q5dskdp36i10ckigkzffg8hc3ad"))))
      (arguments
       `(#:python ,python-2))
      (native-inputs
       `(("python2-setuptools-scm" ,python2-setuptools-scm)))
      (propagated-inputs
       `(("python2-mock" ,python2-mock)
         ("python2-pytest" ,python2-pytest))))))

(define-public python-pytest-xdist
  (package
    (name "python-pytest-xdist")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-xdist" version))
       (sha256
        (base32
         "0wh6pn66nncfs6ay0n863bgyriwsgppn8flx5l7551j1lbqkinc2"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove pre-compiled .pyc files from source.
           (for-each delete-file-recursively
                     (find-files "." "__pycache__" #:directories? #t))
           (for-each delete-file (find-files "." "\\.pyc$"))
           #t))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f ; Lots of tests fail.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-setup-py
           (lambda _
             ;; Relax pytest requirement.
             (substitute* "setup.py"
               (("pytest>=6\\.0\\.0") "pytest"))))
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "pytest" "-vv"
                       "-n" (number->string (parallel-job-count)))))))))
    (native-inputs
     (list python-setuptools-scm))
    (propagated-inputs
     (list python-execnet python-pytest python-py python-pytest-forked))
    (home-page
     "https://github.com/pytest-dev/pytest-xdist")
    (synopsis
     "Plugin for py.test with distributed testing and loop-on-failing modes")
    (description
     "The pytest-xdist plugin extends py.test with some unique test execution
modes: parallelization, running tests in boxed subprocesses, the ability
to run tests repeatedly when failed, and the ability to run tests on multiple
Python interpreters or platforms.  It uses rsync to copy the existing
program code to a remote location, executes there, and then syncs the
result back.")
    (license license:expat)))

(define-public python2-pytest-xdist
  (package-with-python2 python-pytest-xdist))

(define-public python-pytest-xdist-next
  (package/inherit python-pytest-xdist
    (name "python-pytest-xdist")
    (version "2.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-xdist" version))
       (sha256
        (base32
         "19cy57jrf3pwi7x6fnbxryjvqagsl0yv736jnynvr3yqhlpxxv78"))))
    (propagated-inputs
     `(("python-execnet" ,python-execnet)
       ("python-pytest" ,python-pytest-6)
       ("python-pytest-forked" ,python-pytest-forked)))))

(define-public python-pytest-timeout
  (package
    (name "python-pytest-timeout")
    (version "2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-timeout" version))
       (sha256
        (base32
         "04l1cd2qyp3fbccw95a8nqg682r647v7yil8807dgs7xv9a8pyg6"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      ;; Make the installed plugin discoverable by Pytest.
                      (add-installed-pythonpath inputs outputs)
                      (invoke "pytest" "-vv"))))))
    (propagated-inputs
     (list python-pytest python-pytest-cov))
    (native-inputs
     (list python-pexpect))
    (home-page "https://github.com/pytest-dev/pytest-timeout")
    (synopsis "Plugin for py.test to abort hanging tests")
    (description
     "This package provides a py.test plugin that aborts hanging tests after a
timeout has been exceeded.")
    (license license:expat)))

(define-public python-pytest-forked
  (package
    (name "python-pytest-forked")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)               ;for tests
       (uri (git-reference
             (url "https://github.com/pytest-dev/pytest-forked")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1aip4kx50ynvykl7kq2mlbsi82vx701dvb8mm64lhp69bbv105rc"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-setuptools-scm
           (lambda _
             (substitute* "setup.py"
               (("use_scm_version=True")
                (format #f "version=~s" ,version))
               (("setup_requires=\\['setuptools_scm'\\],.*")
                ""))))
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "pytest" "-vv")))))))
    (native-inputs
     ;; XXX: The bootstrap variant of Pytest is used to ensure the
     ;; 'hypothesis' plugin is not in the environment (due to
     ;; <http://issues.guix.gnu.org/25235>), which would cause the test suite
     ;; to fail (see: https://github.com/pytest-dev/pytest-forked/issues/54).
     `(("python-pytest" ,python-pytest-bootstrap)))
    (home-page "https://github.com/pytest-dev/pytest-forked")
    (synopsis "Pytest plugin to run tests in isolated forked subprocesses")
    (description "This package provides a Pytest plugin which enables running
each test in a subprocess and will report if a test crashed the process.  It
can be useful to isolate tests against undesirable global environment
side-effects (such as setting environment variables).")
    (license license:expat)))

(define-public python-scripttest
  (package
    (name "python-scripttest")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "scripttest" version))
       (sha256
        (base32
         "0f4w84k8ck82syys7yg9maz93mqzc8p5ymis941x034v44jzq74m"))))
    (build-system python-build-system)
    (native-inputs
     (list python-pytest))
    (home-page (string-append "https://web.archive.org/web/20161029233413/"
                              "http://pythonpaste.org/scripttest/"))
    (synopsis "Python library to test command-line scripts")
    (description "Scripttest is a Python helper library for testing
interactive command-line applications.  With it you can run a script in a
subprocess and see the output as well as any file modifications.")
    (license license:expat)))

(define-public python-testtools-bootstrap
  (package
    (name "python-testtools-bootstrap")
    (version "2.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "testtools" version))
       (sha256
        (base32
         "0gxjbjk93jjqi491k4s9rh3ia37v21yifd35pvizv7sgv4rk9hap"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))
    (propagated-inputs
     `(("python-extras" ,python-extras)
       ("python-fixtures" ,python-fixtures-bootstrap)
       ("python-pbr" ,python-pbr-minimal)))
    (home-page "https://github.com/testing-cabal/testtools")
    (synopsis
     "Extensions to the Python standard library unit testing framework")
    (description
     "This package is only for bootstrapping.  Do not use this.")
    (license license:psfl)))

(define-public python-testtools
  (package
    (inherit python-testtools-bootstrap)
    (name "python-testtools")
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "python" "-m" "testtools.run"
                       "testtools.tests.test_suite")))))))
    (propagated-inputs
     (list python-extras python-fixtures python-pbr))
    (native-inputs
     `(("python-testscenarios" ,python-testscenarios-bootstrap)))
    (description
     "Testtools extends the Python standard library unit testing framework to
provide matchers, more debugging information, and cross-Python
compatibility.")))

(define-public python-testscenarios-bootstrap
  (package
    (name "python-testscenarios-bootstrap")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "testscenarios" version))
       (sha256
        (base32
         "1dm2aydqpv76vnsk1pw7k8n42hq58cfi4n1ixy7nyzpaj1mwnmy2"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda _
                      (invoke "python" "-m" "testtools.run"
                              "testscenarios.test_suite"))))))
    (propagated-inputs
     `(("python-pbr" ,python-pbr-minimal)
       ("python-testtools" ,python-testtools-bootstrap)))
    (home-page "https://launchpad.net/testscenarios")
    (synopsis "Pyunit extension for dependency injection")
    (description
     "This package is only for bootstrapping.  Don't use this.")
    (license (list license:bsd-3 license:asl2.0)))) ; at the user's option

(define-public python-testscenarios
  (package
    (inherit python-testscenarios-bootstrap)
    (name "python-testscenarios")
    (propagated-inputs
     (list python-pbr python-testtools))
    (description
     "Testscenarios provides clean dependency injection for Python unittest
style tests.")))

;; Testresources requires python-pbr at runtime, but pbr needs it for its
;; own tests.  Hence this bootstrap variant.
(define-public python-testresources-bootstrap
  (package
    (name "python-testresources-bootstrap")
    (version "2.0.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "testresources" version))
              (sha256
               (base32
                "05s4dsli9g17m1r3b1gvwicbbgq011hnpb2b9qnj27ja2n11k7gf"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))
    (propagated-inputs
     `(("python-pbr" ,python-pbr-minimal)))
    (home-page "https://launchpad.net/testresources")
    (synopsis
     "Pyunit extension for managing test resources")
    (description
     "This package is only here for bootstrapping purposes.  Use the regular
testresources package instead.")
    (license (list license:bsd-3 license:asl2.0)))) ; at the user's option

(define-public python-testresources
  (package
    (inherit python-testresources-bootstrap)
    (name "python-testresources")
    (propagated-inputs
     (list python-pbr))
    (arguments '())
    (native-inputs
     (list python-fixtures python-testtools))
    (description
     "Testresources is an extension to Python's unittest to allow declarative
use of resources by test cases.")))

(define-public python-subunit-bootstrap
  (package
    (name "python-subunit-bootstrap")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-subunit" version))
       (sha256
        (base32
         "0j0ymmnc5nfxi1qzvy59j27viqca7l7xd0y9x29g7yr0h693j804"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-extras python-testtools-bootstrap))
    (native-inputs
     (list python-fixtures-bootstrap python-hypothesis
           python-testscenarios-bootstrap))
    (home-page "https://launchpad.net/subunit")
    (synopsis "Python implementation of the subunit protocol")
    (description
     "This package is here for bootstrapping purposes only.  Use the regular
python-subunit package instead.")
    (license (list license:bsd-3 license:asl2.0)))) ; at the user's option

(define-public python-subunit
  (package
    (inherit python-subunit-bootstrap)
    (name "python-subunit")
    (propagated-inputs
     (list python-extras python-testtools))
    (native-inputs
     (list python-fixtures python-hypothesis python-testscenarios))
    (description
     "Python-subunit is a Python implementation of the subunit test streaming
protocol.")))

;; Fixtures requires python-pbr at runtime, but pbr uses fixtures for its
;; own tests.  Hence this bootstrap variant.
(define-public python-fixtures-bootstrap
  (package
    (name "python-fixtures-bootstrap")
    (version "3.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "fixtures" version))
        (sha256
         (base32
          "1vxj29bzz3rd4pcy51d05wng9q9dh4jq6wx92yklsm7i6h1ddw7w"))
        (patches (search-patches "python-fixtures-remove-monkeypatch-test.patch"))))
    (build-system python-build-system)
    (arguments
      `(#:tests? #f
        #:phases
         (modify-phases %standard-phases
           ;; Package is not loadable on its own at this stage.
           (delete 'sanity-check))))
    (propagated-inputs
     (list python-pbr-minimal python-six))
    (home-page "https://launchpad.net/python-fixtures")
    (synopsis "Python test fixture library")
    (description
     "This package is only used for bootstrapping.  Use the regular
python-fixtures package instead.")
    (license (list license:bsd-3 license:asl2.0)))) ; at user's option

(define-public python-fixtures
  (package
    (inherit python-fixtures-bootstrap)
    (name "python-fixtures")
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "python" "-m" "testtools.run"
                       "fixtures.test_suite")))))))
    (propagated-inputs
     ;; Fixtures uses pbr at runtime to check versions, etc.
     (list python-pbr python-six))
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-testtools" ,python-testtools-bootstrap)))
    (description
     "Fixtures provides a way to create reusable state, useful when writing
Python tests.")))

(define-public python-testrepository-bootstrap
  (package
    (name "python-testrepository-bootstrap")
     (version "0.0.20")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "testrepository" version))
       (sha256
        (base32
         "1ssqb07c277010i6gzzkbdd46gd9mrj0bi0i8vn560n2k2y4j93m"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))
    (propagated-inputs
     `(("python-fixtures" ,python-fixtures-bootstrap)
       ("python-subunit" ,python-subunit-bootstrap)
       ("python-testtools" ,python-testtools-bootstrap)))
    (native-inputs
     (list python-mimeparse))
    (home-page "https://launchpad.net/testrepository")
    (synopsis "Database for Python test results")
    (description
     "Bootstrap package for python-testrepository.  Don't use this.")
    (license (list license:bsd-3 license:asl2.0)))) ; at user's option

(define-public python-testrepository
  (package
    (inherit python-testrepository-bootstrap)
    (name "python-testrepository")
    (arguments
     ;; FIXME: Many tests are failing.
     '(#:tests? #f))
    (propagated-inputs
     (list python-fixtures python-subunit python-testtools))
    (native-inputs
     (list python-mimeparse))
    (description "Testrepository provides a database of test results which can
be used as part of a developer's workflow to check things such as what tests
have failed since the last commit or what tests are currently failing.")))

(define-public python-coverage
  (package
    (name "python-coverage")
    (version "5.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "coverage" version))
       (sha256
        (base32
         "16z8i18msgs8k74n73dj9x49wzkl0vk4vq8k5pl1bsj70y7b4k53"))))
    (build-system python-build-system)
    (arguments
     ;; FIXME: 95 tests failed, 539 passed, 6 skipped, 2 errors.
     '(#:tests? #f))
    (home-page "https://coverage.readthedocs.io")
    (synopsis "Code coverage measurement for Python")
    (description
     "Coverage measures code coverage, typically during test execution.  It
uses the code analysis tools and tracing hooks provided in the Python standard
library to determine which lines are executable, and which have been
executed.")
    (license license:bsd-3)))

(define-public python2-coverage
  (package-with-python2 python-coverage))

(define-public python-pytest-asyncio
  (package
    (name "python-pytest-asyncio")
    (version "0.17.2")
    (source
     (origin
       (method git-fetch)               ;for tests
       (uri (git-reference
             (url "https://github.com/pytest-dev/pytest-asyncio")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sl0ckc23m40q6r2xcidsizrgqbbsfa7rwmr80fss359xsydf073"))))
    (build-system python-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'pretend-version
                 (lambda _
                   (setenv "SETUPTOOLS_SCM_PRETEND_VERSION"
                           #$(package-version this-package))))
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (invoke "pytest" "-vv" "tests"))))))
    (native-inputs
     (list python-async-generator
           python-flaky
           python-hypothesis
           python-pytest
           python-setuptools-scm))
    (home-page "https://github.com/pytest-dev/pytest-asyncio")
    (synopsis "Pytest support for asyncio")
    (description "Python asyncio code is usually written in the form of
coroutines, which makes it slightly more difficult to test using normal
testing tools.  @code{pytest-asyncio} provides useful fixtures and markers
to make testing async code easier.")
    (license license:asl2.0)))

(define-public python-cov-core
  (package
    (name "python-cov-core")
    (version "1.15.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "cov-core" version))
        (sha256
         (base32
          "0k3np9ymh06yv1ib96sb6wfsxjkqhmik8qfsn119vnhga9ywc52a"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-coverage))
    (home-page "https://github.com/schlamar/cov-core")
    (synopsis "Coverage plugin core for pytest-cov, nose-cov and nose2-cov")
    (description
     "This is a library package for use by @code{pytest-cov}, @code{nose-cov}
and @code{nose2-cov}.  It is useful for developing coverage plugins for these
testing frameworks.")
    (license license:expat)))

(define-public python2-cov-core
 (package-with-python2 python-cov-core))

(define-public python-codecov
  (package
    (name "python-codecov")
    (version "2.0.15")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "codecov" version))
        (sha256
         (base32
          "1217c0vqf7ii65635gvl27a5pfhv0r7zhrpdp9cx640hg73bgn4f"))))
    (build-system python-build-system)
    (native-inputs
     (list python-unittest2))
    (propagated-inputs
     (list python-coverage python-requests))
    (home-page "https://github.com/codecov/codecov-python")
    (synopsis "Upload code coverage reports to @code{codecov.io}")
    (description
     "Codecov collects code coverage reports from code written in Python, Java,
C/C++, R, and more, and uploads it to the @code{codecov.io} service.")
    (license license:asl2.0)))

(define-public python-testpath
  (package
    (name "python-testpath")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/jupyter/testpath")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "08r1c6bhvj8pcdvzkqv1950k36a6q3v81fd2p1yqdq3c07mcwgif"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "pyproject.toml"
                (("flit_core >=3.2.0,<3.3")
                 "flit_core >=3.2.0"))))
          ;; XXX: PEP 517 manual build copied from python-isort.
          (replace 'build
            (lambda _
              (invoke "python" "-m" "build" "--wheel" "--no-isolation" ".")))
          (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest"))))
          (replace 'install
            (lambda _
              (let ((whl (car (find-files "dist" "\\.whl$"))))
                (invoke "pip" "--no-cache-dir" "--no-input"
                        "install" "--no-deps" "--prefix" #$output whl)))))))
    (native-inputs
     (list python-pypa-build python-flit-core python-pytest))
    (home-page "https://github.com/jupyter/testpath")
    (synopsis "Test utilities for code working with files and commands")
    (description
     "Testpath is a collection of utilities for Python code working with files
and commands.  It contains functions to check things on the file system, and
tools for mocking system commands and recording calls to those.")
    (license license:expat)))

(define-public python-testlib
  (package
    (name "python-testlib")
    (version "0.6.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "testlib" version ".zip"))
       (sha256
        (base32 "1mz26cxn4x8bbgv0rn0mvj2z05y31rkc8009nvdlb3lam5b4mj3y"))))
    (build-system python-build-system)
    (native-inputs
     (list unzip))  ; for unpacking the source
    (synopsis "Python micro test suite harness")
    (description "A micro unittest suite harness for Python.")
    (home-page "https://github.com/trentm/testlib")
    (license license:expat)))

(define-public python2-testlib
  (package-with-python2 python-testlib))

;;; The software provided by this package was integrated into pytest 2.8.
(define-public python-pytest-cache
  (package
    (name "python-pytest-cache")
    (version "1.0")
    (source (origin
             (method url-fetch)
             (uri (pypi-uri "pytest-cache" version))
             (sha256
              (base32
               "1a873fihw4rhshc722j4h6j7g3nj7xpgsna9hhg3zn6ksknnhx5y"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-apipkg python-execnet python-py python-pytest))
    (synopsis "Py.test plugin with mechanisms for caching across test runs")
    (description "The pytest-cache plugin provides tools to rerun failures from
the last py.test invocation.")
    (home-page "https://bitbucket.org/hpk42/pytest-cache/")
    (license license:expat)))

(define-public python2-pytest-cache
  (package-with-python2 python-pytest-cache))

(define-public python-pytest-localserver
  (package
    (name "python-pytest-localserver")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pytest-localserver" version))
              (sha256
               (base32
                "1hpgpxrpfq5c731ndnsay2lc0y9nh2wy9fn1f83s3z8xkn82fm1s"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "py.test" "-v"))))))
    (native-inputs
     (list python-pytest python-requests python-six))
    (propagated-inputs
     (list python-werkzeug))
    (synopsis "Py.test plugin to test server connections locally")
    (description "Pytest-localserver is a plugin for the pytest testing
framework which enables you to test server connections locally.")
    (home-page "https://pypi.org/project/pytest-localserver/")
    (license license:expat)))

(define-public python-pytest-xprocess
  (package
    (name "python-pytest-xprocess")
    (version "0.18.1")
    (source (origin
             (method url-fetch)
             (uri (pypi-uri "pytest-xprocess" version))
             (sha256
              (base32
               "0rm2rchrr63imn44xk5slwydxf8gvy579524qcxq7dc42pnk17zx"))))
    (build-system python-build-system)
    (native-inputs
     (list python-setuptools-scm))
    (propagated-inputs
     (list python-pytest python-psutil))
    (synopsis "Pytest plugin to manage external processes across test runs")
    (description "Pytest-xprocess is an experimental py.test plugin for managing
processes across test runs.")
    (home-page "https://github.com/pytest-dev/pytest-xprocess/")
    (license license:expat)))

(define-public python-pytest-subtesthack
  (package
    (name "python-pytest-subtesthack")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pytest-subtesthack" version))
              (sha256
               (base32
                "15kzcr5pchf3id4ikdvlv752rc0j4d912n589l4rifp8qsj19l1x"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-pytest))
    (synopsis "Set-up and tear-down fixtures for unit tests")
    (description "This plugin allows you to set up and tear down fixtures within
unit test functions that use @code{py.test}. This is useful for using
@command{hypothesis} inside py.test, as @command{hypothesis} will call the test
function multiple times, without setting up or tearing down fixture state as is
normally the case.")
    (home-page "https://github.com/untitaker/pytest-subtesthack/")
    (license license:unlicense)))

(define-public python2-pytest-subtesthack
  (package-with-python2 python-pytest-subtesthack))

(define-public python-pytest-sugar
  (package
    (name "python-pytest-sugar")
    (version "0.9.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-sugar" version))
       (sha256
        (base32 "1i0hv3h49zvl62jbiyjag84carbrp3zprqzxffdr291nxavvac0n"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-packaging python-pytest python-termcolor))
    (home-page "https://pivotfinland.com/pytest-sugar/")
    (synopsis "Plugin for pytest that changes the default look and feel")
    (description
     "@code{pytest-sugar} is a plugin for py.test that changes the default
look and feel of py.test, using a progress bar and showing failures and errors
instantly.")
    (license license:bsd-3)))

(define-public python-hypothesis
  (package
    (name "python-hypothesis")
    (version "6.0.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "hypothesis" version))
              (sha256
               (base32
                "0wj7ip779naf2n076nylf2gi0sjz68z1ir9d9r2rgs7br18naqdf"))))
    (build-system python-build-system)
    (arguments
     ;; XXX: Tests are not distributed with the PyPI archive.
     '(#:tests? #f))
    (propagated-inputs
     `(("python-attrs" ,python-attrs-bootstrap)
       ("python-sortedcontainers" ,python-sortedcontainers)))
    (synopsis "Library for property based testing")
    (description "Hypothesis is a library for testing your Python code against a
much larger range of examples than you would ever want to write by hand.  It’s
based on the Haskell library, Quickcheck, and is designed to integrate
seamlessly into your existing Python unit testing work flow.")
    (home-page "https://github.com/HypothesisWorks/hypothesis-python")
    (license license:mpl2.0)
    (properties `((python2-variant . ,(delay python2-hypothesis))))))

(define-public python-hypothesis-6.23
  (package
    (inherit python-hypothesis)
    (version "6.23.4")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "hypothesis" version))
              (sha256
               (base32
                "0wp8i9qmd5wl1sq1l2b97fgliyk5fyphssl6j7q5qn5zjlfgi4qs"))))))

;; This is the last version of Hypothesis that supports Python 2.
(define-public python2-hypothesis
  (let ((hypothesis (package-with-python2
                     (strip-python2-variant python-hypothesis))))
    (package (inherit hypothesis)
      (version "4.57.1")
      (source (origin
                (method url-fetch)
                (uri (pypi-uri "hypothesis" version))
                (sha256
                 (base32
                  "183gpxbfcdhdqzlahkji5a71n6lmvgqsbkcb0ihqad51n2j6jhrw"))))
      (propagated-inputs
       (modify-inputs (package-propagated-inputs hypothesis)
         (prepend python2-enum34))))))

(define-public python-hypothesmith
  (package
    (name "python-hypothesmith")
    (version "0.1.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hypothesmith" version))
       (sha256
        (base32
         "02j101m5grjrbvrgjap17jsxd1hgawkylmyswcn33vf42mxh9zzr"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-hypothesis python-lark-parser python-libcst-minimal))
    (home-page "https://github.com/Zac-HD/hypothesmith")
    (synopsis "Strategies for generating Python programs")
    (description
     "This package contains hypothesis strategies for generating Python
programs, something like CSmith, a random generator of C programs.")
    (license license:mpl2.0)))

(define-public python-lit
  (package
    (name "python-lit")
    (version "12.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "lit" version))
        (sha256
         (base32
          "01yggsb73s2gbq36xwifxl6k5ll5lkss5rwz59k9h3jnbnn7m5fj"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "python" "lit.py" "tests"))))))
    (native-inputs
     (list llvm))
    (home-page "https://llvm.org/")
    (synopsis "LLVM Software Testing Tool")
    (description "@code{lit} is a portable tool for executing LLVM and Clang
style test suites, summarizing their results, and providing indication of
failures.")
    (license license:ncsa)))

(define-public python-pytest-pep8
  (package
    (name "python-pytest-pep8")
    (version "1.0.6")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pytest-pep8" version))
              (sha256
               (base32
                "06032agzhw1i9d9qlhfblnl3dw5hcyxhagn7b120zhrszbjzfbh3"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; Fails with recent pytest and pep8. See upstream issues #8 and #12.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-dependencies
           (lambda _
             (substitute* "setup.py"
               (("'pytest-cache', ") ""))))  ; Included in recent pytest
         (replace 'check
            (lambda* (#:key tests? inputs outputs #:allow-other-keys)
              (when tests?
                (add-installed-pythonpath inputs outputs)
                (invoke "pytest" "-v")))))))
    (native-inputs
     (list python-pytest))
    (propagated-inputs
     (list python-pep8))
    (home-page "https://bitbucket.org/pytest-dev/pytest-pep8")
    (synopsis "Py.test plugin to check PEP8 requirements")
    (description "Pytest plugin for checking PEP8 compliance.")
    (license license:expat)))

(define-public python2-pytest-pep8
  (package-with-python2 python-pytest-pep8))

(define-public python-pytest-flakes
  (package
    (name "python-pytest-flakes")
    (version "4.0.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pytest-flakes" version))
              (sha256
               (base32
                "0045h3hnrkn2jwr42jgy2j98npx4amwr6wxzi9j0nppaqz33l49p"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key outputs inputs #:allow-other-keys)
             ;; It's easier to run tests after install.
             ;; Make installed package available for running the tests
             (add-installed-pythonpath inputs outputs)
             (invoke "py.test" "-vv" "-k" "not test_syntax_error"))))))
    (native-inputs
     (list python-coverage python-pytest python-pytest-cache
           python-pytest-pep8))
    (propagated-inputs
     (list python-pyflakes))
    (home-page "https://github.com/fschulze/pytest-flakes")
    (synopsis "Py.test plugin to check source code with pyflakes")
    (description "Pytest plugin for checking Python source code with pyflakes.")
    (license license:expat)))

(define-public python2-pytest-flakes
  (package-with-python2 python-pytest-flakes))

(define-public python-coverage-test-runner
  (package
    (name "python-coverage-test-runner")
    (version "1.15")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://git.liw.fi/cgi-bin/cgit/cgit.cgi/"
             "coverage-test-runner/snapshot/coverage-test-runner-"
             version ".tar.gz"))
       (sha256
        (base32
         "1kjjb9llckycnfxag8zcvqsn4z1s3dwyw6b1n0avxydihgf30rny"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "./testrun"))))))
    (propagated-inputs
     (list python-coverage))
    (home-page "https://liw.fi/coverage-test-runner/")
    (synopsis "Python module for running unit tests")
    (description "@code{CoverageTestRunner} is a python module for running
unit tests and failing them if the unit test module does not exercise all
statements in the module it tests.")
    (license license:gpl3+)))

(define-public python2-coverage-test-runner
  (package-with-python2 python-coverage-test-runner))

(define-public python-pylint
  (package
    (name "python-pylint")
    (version "2.12.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/PyCQA/pylint")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0spmy7j1vvh55shzgma80q61y0d1cj45dcgslb4g5w3y602miq5i"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; The unused but collected 'primer'-related test files require
               ;; the extraneous 'git' Python module; remove them.
               (delete-file "tests/primer/test_primer_external.py")
               (delete-file "tests/testutils/test_package_to_lint.py")
               (setenv "HOME" "/tmp")
               (invoke "pytest" "-k" "test_functional")))))))
    (native-inputs
     (list python-pytest))
    (propagated-inputs
     (list python-astroid
           python-isort
           python-mccabe
           python-platformdirs
           python-toml
           python-typing-extensions))
    (home-page "https://github.com/PyCQA/pylint")
    (synopsis "Advanced Python code static checker")
    (description "Pylint is a Python source code analyzer which looks
for programming errors, helps enforcing a coding standard and sniffs
for some code smells (as defined in Martin Fowler's Refactoring book).

Pylint has many rules enabled by default, way too much to silence them
all on a minimally sized program.  It's highly configurable and handle
pragmas to control it from within your code.  Additionally, it is
possible to write plugins to add your own checks.")
    (license license:gpl2+)))

(define-public python-paramunittest
  (package
    (name "python-paramunittest")
    (version "0.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ParamUnittest" version))
       (sha256
        (base32
         "0kp793hws5xv1wvycxq7jw2pwy36f35k39jg8hx5qikij5a0jid1"))))
    (build-system python-build-system)
    (home-page
     "https://github.com/rik0/ParamUnittest")
    (synopsis
     "Simple extension to have parametrized unit tests")
    (description
     "This package creates parameterized unit-tests that work with the standard
unittest package.  A parameterized test case is automatically converted to multiple test
cases.  Since they are TestCase subclasses, they work with other test suites that
recognize TestCases.")
    (license license:bsd-2)))

(define-public python2-python-paramunittest
  (package-with-python2 python-paramunittest))

(define-public python-pytest-warnings
  (package
    (name "python-pytest-warnings")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-warnings" version))
       (sha256
        (base32
         "0gf2dpahpl5igb7jh1sr9acj3z3gp7zahqdqb69nk6wx01c8kc1g"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-pytest))
    (home-page "https://github.com/fschulze/pytest-warnings")
    (synopsis "Pytest plugin to list Python warnings in pytest report")
    (description
     "Python-pytest-warnings is a pytest plugin to list Python warnings in
pytest report.")
    (license license:expat)
    (properties `((python2-variant . ,(delay python2-pytest-warnings))
                  ;; This package is part of pytest as of version 3.1.0.
                  (superseded . ,python-pytest)))))

(define-public python2-pytest-warnings
  (package (inherit (package-with-python2
                     (strip-python2-variant python-pytest-warnings)))
           (properties `((superseded . ,python2-pytest)))))

(define-public python-pytest-capturelog
  (package
    (name "python-pytest-capturelog")
    (version "0.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-capturelog" version ".tar.gz"))
       (sha256
        (base32
         "038049nyjl7di59ycnxvc9nydivc5m8np3hqq84j2iirkccdbs5n"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-pytest))
    (home-page "https://bitbucket.org/memedough/pytest-capturelog/overview")
    (synopsis "Pytest plugin to catch log messages")
    (description
     "Python-pytest-catchlog is a pytest plugin to catch log messages.")
    (license license:expat)))

(define-public python2-pytest-capturelog
  (package-with-python2 python-pytest-capturelog))

(define-public python-pytest-catchlog
  (package
    (name "python-pytest-catchlog")
    (version "1.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-catchlog" version ".zip"))
       (sha256
        (base32
         "1w7wxh27sbqwm4jgwrjr9c2gy384aca5jzw9c0wzhl0pmk2mvqab"))))
    (build-system python-build-system)
    (native-inputs
     (list unzip))
    (propagated-inputs
     (list python-pytest))
    (home-page "https://github.com/eisensheng/pytest-catchlog")
    (synopsis "Pytest plugin to catch log messages")
    (description
     "Python-pytest-catchlog is a pytest plugin to catch log messages.  This is
a fork of pytest-capturelog.")
    (license license:expat)))

(define-public python2-pytest-catchlog
  (package-with-python2 python-pytest-catchlog))

(define-public python-nosexcover
  (package
    (name "python-nosexcover")
    (version "1.0.11")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "nosexcover" version))
              (sha256
               (base32
                "10xqr12qv62k2flxwqhh8cr00cjhn7sfjrm6p35gd1x5bmjkr319"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-coverage python-nose))
    (home-page "https://github.com/cmheisel/nose-xcover")
    (synopsis "Extends nose.plugins.cover to add Cobertura-style XML reports")
    (description "Nose-xcover is a companion to the built-in
@code{nose.plugins.cover}.  This plugin will write out an XML coverage report
to a file named coverage.xml.

It will honor all the options you pass to the Nose coverage plugin,
especially -cover-package.")
    (license license:expat)))

(define-public python2-nosexcover
  (package-with-python2 python-nosexcover))

(define-public python-discover
  (package
    (name "python-discover")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "discover" version))
       (sha256
        (base32
         "0y8d0zwiqar51kxj8lzmkvwc3b8kazb04gk5zcb4nzg5k68zmhq5"))))
    (build-system python-build-system)
    (home-page "https://pypi.org/project/discover/")
    (synopsis
     "Python test discovery for unittest")
    (description
     "Discover provides test discovery for unittest, a feature that has been
backported from Python 2.7 for Python 2.4+.")
    (license license:bsd-3)))

(define-public python2-discover
  (package-with-python2 python-discover))

(define-public behave
  (package
    (name "behave")
    ;; The 1.2.6 release from 2018 has several problems with newer Python
    ;; versions, so we package a recent snapshot.
    (version "1.2.7.dev2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/behave/behave")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0sv94wagi214h0l91zn8m04f78x5wn83vqxib81hnl1qahvx9hq7"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests?
                        (invoke "pytest" "-c" "/dev/null" "-vv")))))))
    (native-inputs
     (list python-mock python-nose python-pathpy python-pyhamcrest
           python-pytest))
    (propagated-inputs
     (list python-colorama
           python-cucumber-tag-expressions
           python-parse
           python-parse-type))
    (home-page "https://github.com/behave/behave")
    (synopsis "Python behavior-driven development")
    (description
     "Behave is a tool for behavior-driven development in python.
Behavior-driven development (or BDD) is an agile software development
technique that encourages collaboration between developers, QA and
non-technical or business participants in a software project.  Behave uses
tests written in a natural language style, backed up by Python code.")
    (license license:x11)))

(define-public python-behave-web-api
  (package
    (name "python-behave-web-api")
    (version "1.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "behave-web-api" version))
       (sha256
        (base32
         "03kpq2xsy1gab3jy0dccbxlsg7vwfy4lagss0qldwmx3xz6b3i19"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-dependencies
           (lambda _
             (substitute* "setup.py"
               (("'wheel'") "")                ; We don't use it.
               (("'ordereddict==1.1'") ""))    ; Python >= 2.7 has it built-in.
             #t)))))
    (propagated-inputs
     (list behave python-requests))
    (home-page "https://github.com/jefersondaniel/behave-web-api")
    (synopsis "Provides testing for JSON APIs with Behave for Python")
    (description "This package provides testing utility modules for testing
JSON APIs with Behave.")
    (license license:expat)))

(define-public python-rednose
  (package
    (name "python-rednose")
    (version "1.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "rednose" version))
        (sha256
          (base32
            "11x5nx5b4wdq04s7vj1gcdl07jvvkfb37p0r5lg773gr5rr8mj6h"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-setup.py
                    (lambda _
                      ;; Six is only required for tests and later versions
                      ;; work fine.
                      (substitute* "setup.py"
                        (("six==1.10.0") "six"))
                      #t)))))
    (propagated-inputs
     (list python-colorama python-termstyle))
    (native-inputs
     (list python-six python-nose))
    (home-page "https://github.com/JBKahn/rednose")
    (synopsis "Colored output for Python nosetests")
    (description "This package provides colored output for the
@command{nosetests} command of the Python Nose unit test framework.")
    (license license:bsd-3)))

(define-public python2-rednose
  (package-with-python2 python-rednose))

(define-public python-nose-random
  (package
    (name "python-nose-random")
    (version "1.0.0")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/fzumstein/nose-random")
            (commit version)))
      (file-name (git-file-name name version))
      (sha256
       (base32
        "1dvip61r2frjv35mv6mmfjc07402z73pjbndfp3mhxyjn2zhksw2"))))
    (build-system python-build-system)
    (native-inputs
     (list python-nose))
    (home-page "https://github.com/fzumstein/nose-random")
    (synopsis "Nose plugin to facilitate randomized unit testing with
Python")
    (description "Python nose-random is designed to facilitate
Monte-Carlo style unit testing.  The idea is to improve testing by
running your code against a large number of randomly generated input
scenarios.")
    (license license:expat)))

(define-public python-nose-randomly
  (package
    (name "python-nose-randomly")
    (version "1.2.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "nose-randomly" version))
       (sha256
        (base32 "0z662rqhfk4bjmg806mn4frb8nz4gbh7mrddsrhfffp1g4yklj3y"))))
    (build-system python-build-system)
    (native-inputs
     (list python-nose python-numpy))
    (home-page "https://github.com/adamchainz/nose-randomly")
    (synopsis
     "Nose plugin to randomly order tests and control random.seed")
    (description
     "This is a @code{Nose} plugin to randomly order tests which can be quite
powerful in discovering hidden flaws in the tests themselves, while helping to
reduce inter-test dependencies.  It also helps in controlling @code{random.seed},
by resetting it to a repeatable number for each test, enabling the tests to
create data based on random numbers and yet remain repeatable.")
    (license license:bsd-3)))

(define-public python2-nose-randomly
  (package-with-python2 python-nose-randomly))

(define-public python-nose-timer
  (package
    (name "python-nose-timer")
    (version "0.7.5")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "nose-timer" version))
        (sha256
          (base32 "05wzkc88vbzw62pqkvhl33211b90kns0lny70b7qw62rcg4flzk4"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-nose python-termcolor))
    (home-page "https://github.com/mahmoudimus/nose-timer")
    (synopsis "Timer plugin for nosetests")
    (description "Shows how much time was needed to run individual tests.")
    (license license:expat)))

(define-public python2-nose-timer
  (package-with-python2 python-nose-timer))

(define-public python-freezegun
  (package
    (name "python-freezegun")
    (version "0.3.14")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "freezegun" version))
       (sha256
        (base32 "0al75mk829j1izxi760b7yjnknjihyfhp2mvi5qiyrxb9cpxwqk2"))))
    (build-system python-build-system)
    (native-inputs
     (list python-mock python-pytest))
    (propagated-inputs
     (list python-six python-dateutil))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; The tests are normally executed via `make test`, but the PyPi
         ;; package does not include the Makefile.
         (replace 'check
           (lambda _
             (invoke "pytest" "-vv"))))))
    (home-page "https://github.com/spulec/freezegun")
    (synopsis "Test utility for mocking the datetime module")
    (description
     "FreezeGun is a library that allows your python tests to travel through
time by mocking the datetime module.")
    (license license:asl2.0)))

(define-public python2-freezegun
  (package-with-python2 python-freezegun))

(define-public python-flexmock
  (package
    (name "python-flexmock")
    (version "0.10.4")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "flexmock" version))
              (sha256
               (base32
                "0b6qw3grhgx58kxlkj7mdma7xdvlj02zabvcf7w2qifnfjwwwcsh"))))
    (build-system python-build-system)
    (home-page "https://flexmock.readthedocs.org")
    (synopsis "Testing library for Python")
    (description
     "flexmock is a testing library for Python that makes it easy to create
mocks, stubs and fakes.")
    (license license:bsd-3)))

(define-public python2-flexmock
  (package-with-python2 python-flexmock))

(define-public python-flaky
  (package
    (name "python-flaky")
    (version "3.5.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "flaky" version))
              (sha256
               (base32
                "1nm1kjf857z5aw7v642ffsy1vwf255c6wjvmil71kckjyd0mxg8j"))))
    (build-system python-build-system)
    (arguments
     ;; TODO: Tests require 'coveralls' and 'genty' which are not in Guix yet.
     '(#:tests? #f))
    (home-page "https://github.com/box/flaky")
    (synopsis "Automatically rerun flaky tests")
    (description
     "Flaky is a plugin for @code{nose} or @code{py.test} that automatically
reruns flaky tests.

Ideally, tests reliably pass or fail, but sometimes test fixtures must rely
on components that aren't 100% reliable.  With flaky, instead of removing
those tests or marking them to @code{@@skip}, they can be automatically
retried.")
    (license license:asl2.0)))

(define-public python2-flaky
  (package-with-python2 python-flaky))

(define-public python-pyhamcrest
  (package
    (name "python-pyhamcrest")
    (version "2.0.2")
    (source (origin
              (method git-fetch)        ;no tests in PyPI archive
              (uri (git-reference
                    (url "https://github.com/hamcrest/PyHamcrest")
                    (commit (string-append "V" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "05kdzlhs2kvj82pfca13qszszcj6dyrk4b9pbr46x06sq2s4qyls"))))
    (native-inputs                      ;all native inputs are for tests
     (list python-pytest-cov python-mock python-pytest python-hypothesis))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (add-installed-pythonpath inputs outputs)
                      (invoke "pytest" "-vv"))))))
    (home-page "http://hamcrest.org/")
    (synopsis "Hamcrest matchers for Python")
    (description "PyHamcrest is a framework for writing matcher objects,
allowing you to declaratively define \"match\" rules.")
    (license license:bsd-3)))

(define-public unittest-cpp
  (package
    (name "unittest-cpp")
    (version "2.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/unittest-cpp/unittest-cpp")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0sxb3835nly1jxn071f59fwbdzmqi74j040r81fanxyw3s1azw0i"))))
    (arguments
     `(#:tests? #f))                     ; It's run after build automatically.
    (build-system cmake-build-system)
    (home-page "https://github.com/unittest-cpp/unittest-cpp")
    (synopsis "Lightweight unit testing framework for C++")
    (description "UnitTest++ is a lightweight unit testing framework for C++.
It was designed to do test-driven development on a wide variety of platforms.
Simplicity, portability, speed, and small footprint are all very important
aspects of UnitTest++.  UnitTest++ is mostly standard C++ and makes minimal use
of advanced library and language features, which means it should be easily
portable to just about any platform.")
    (license license:expat)))

(define-public libfaketime
  (package
    (name "libfaketime")
    (version "0.9.9")
    (home-page "https://github.com/wolfcw/libfaketime")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "1gi1xciqga5hl2xlk7rc3j8wy47ag97pi7ngmdl6ny1d11b2wn1z"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'embed-date-reference
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((coreutils (assoc-ref inputs "coreutils")))
                        (substitute* "src/faketime.c"
                          (("\"date\"")
                           (string-append "\"" coreutils "/bin/date\""))))))
                  (replace 'configure
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (setenv "CC" ,(cc-for-target))
                        (setenv "PREFIX" out)

                        ;; XXX: Without this flag, the CLOCK_REALTIME test hangs
                        ;; indefinitely.  See README.packagers for more information.
                        ;; There are specific instructions to not enable more flags
                        ;; than absolutely needed.
                        ,(if (or (target-ppc64le?)
                                 (target-riscv64?))
                           `(setenv "FAKETIME_COMPILE_CFLAGS"
                                    "-DFORCE_MONOTONIC_FIX -DFORCE_PTHREAD_NONVER")
                           `(setenv "FAKETIME_COMPILE_CFLAGS"
                                    "-DFORCE_MONOTONIC_FIX")))))
                  (add-before 'check 'pre-check
                    (lambda _
                      (substitute* "test/functests/test_exclude_mono.sh"
                        (("/bin/bash") (which "bash"))))))
       #:test-target "test"))
    (native-inputs
     (list perl))                           ;for tests
    (inputs
     (list coreutils))
    (synopsis "Fake the system time for single applications")
    (description
     "The libfaketime library allows users to modify the system time that an
application \"sees\".  It is meant to be loaded using the dynamic linker's
@code{LD_PRELOAD} environment variable.  The @command{faketime} command
provides a simple way to achieve this.")
    (license license:gpl2)))

(define-public umockdev
  (package
    (name "umockdev")
    (version "0.14.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/martinpitt/umockdev/"
                                  "releases/download/" version  "/"
                                  "umockdev-" version ".tar.xz"))
              (sha256
               (base32
                "0xmi24ckpps32k7hc139psgbsnsf4g106sv4l9m445m46amkxggd"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-test
           (lambda _
             (substitute* "tests/test-umockdev.c"
               (("/run") "/tmp"))
             #t)))))
    (native-inputs
     (list vala
           gobject-introspection
           gtk-doc/stable
           pkg-config
           ;; For tests.
           python
           which))
    (inputs
     (list glib eudev libgudev))
    (home-page "https://github.com/martinpitt/umockdev/")
    (synopsis "Mock hardware devices for creating unit tests")
    (description "umockdev mocks hardware devices for creating integration
tests for hardware related libraries and programs.  It also provides tools to
record the properties and behaviour of particular devices, and to run a
program or test suite under a test bed with the previously recorded devices
loaded.")
    (license license:lgpl2.1+)))

(define-public virtest
  ;; No releases yet, so we take the commit that "vc" expects.
  (let ((commit "f7d03ef39fceba168745bd29e1b20af6e7971e04")
        (revision "0"))
    (package
      (name "virtest")
      (version (git-version "0.0" revision commit))
      (home-page "https://github.com/mattkretz/virtest")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "07pjyb0mk7y2w1dg1bhl26nb7416xa1mw16ifj6mmps5y6aq054l"))))
      (build-system cmake-build-system)
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (add-after 'unpack 'adjust-install-directory
                      (lambda _
                        ;; Vc is the only consumer of this library, and expects
                        ;; to find it in "virtest/vir/" instead of "vir/vir/".
                        (substitute* "CMakeLists.txt"
                          (("DESTINATION include/vir")
                           "DESTINATION include/virtest"))
                        #t)))))
      (synopsis "Header-only test framework")
      (description
       "@code{virtest} is a small header-only test framework for C++.  It
grew out of the @dfn{Vc} project.")
      (license license:bsd-3))))

(define-public python-pyfakefs
  (package
    (name "python-pyfakefs")
    (version "3.7.1")
    (source (origin
              (method url-fetch)
              ;; We use the PyPI URL because there is no proper release
              ;; available from GitHub.  The GitHub project only provides
              ;; autogenerated tarballs, which are known to change in place.
              (uri (pypi-uri "pyfakefs" version))
              (sha256
               (base32
                "1cp2yw96fa2qkgi39xa3nlr3inf8wb5rgh9kdq53256ca2r8pdhy"))
              (patches (search-patches
                        "python-pyfakefs-remove-bad-test.patch"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-testsuite
           (lambda _
             ;; Time difference is larger than expected.
             (substitute* "pyfakefs/tests/fake_filesystem_unittest_test.py"
               (("(\\s+)def test_copy_real_file" all indent)
                (string-append
                  indent
                  "@unittest.skip('disabled by guix')\n"
                  all)))))
         ;; The default test suite does not run these extra tests.
         (add-after 'check 'check-pytest-plugin
           (lambda _
             (invoke
              "python" "-m" "pytest"
              "pyfakefs/pytest_tests/pytest_plugin_test.py"))))))
    (native-inputs
     (list python-pytest))
    (build-system python-build-system)
    ;; Guix lint doesn't like that this is a permanent redirect to the GitHub
    ;; page, but the pyfakefs documentation asks us to use this specific URL
    ;; when linking to the project.  Honor their request.
    (home-page "http://pyfakefs.org/")
    ;; TRANSLATORS: In the synopsis, "Mock" is a verb.
    (synopsis "Mock file system interactions in tests")
    (description
     "This package provides a Python library intended for use in automated
tests.  One difficulty when testing software is that the code under test might
need to read or write to files in the local file system.  If the file system
is not set up in just the right way, it might cause a spurious error during
the test.  The pyfakefs library provides a solution to problems like this by
mocking file system interactions.  In other words, it arranges for the code
under test to interact with a fake file system instead of the real file
system.  The code under test requires no modification to work with pyfakefs.")
    (license license:asl2.0)))

;; This minimal variant is used to avoid a circular dependency between
;; python2-importlib-metadata, which requires pyfakefs for its tests, and
;; python2-pytest, which requires python2-importlib-metadata.
(define-public python2-pyfakefs-bootstrap
  (hidden-package
   (package
     (inherit (package-with-python2 python-pyfakefs))
     (name "python2-pyfakefs-bootstrap")
     (native-inputs '())
     (arguments
      `(#:python ,python-2
        #:tests? #f)))))

(define-public python-aiounittest
  (package
    (name "python-aiounittest")
    (version "1.4.0")
    ;; Pypi package lacks tests.
    (source
     (origin (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/kwarunek/aiounittest.git")
                   (commit version)))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "0hql5mw62lclrpblbh7xvinwjfcdcfvhhlvl7xlq2hi9isjq1c8r"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (if tests?
                          (invoke "nosetests" "-v")
                          (format #t "test suite not run~%"))
                      #t)))))
    (propagated-inputs (list python-wrapt))
    (native-inputs
     (list python-coverage python-nose))
    (home-page
     "https://github.com/kwarunek/aiounittest")
    (synopsis "Test asyncio code more easily")
    (description "Aiounittest is a library that helps write tests using
asynchronous code in Python (asyncio).")
    (license license:expat)))

(define-public python-pytest-dependency
  (package
    (name "python-pytest-dependency")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pytest-dependency" version))
        (sha256
          (base32
            "0swl3mxca7nnjbb5grfzrm3fa2750h9vjsha0f2kyrljc6895a62"))))
    (build-system python-build-system)
    (propagated-inputs
      (list python-pytest))
    (home-page
      "https://github.com/RKrahl/pytest-dependency")
    (synopsis "Manage dependencies of tests")
    (description "This pytest plugin manages dependencies of tests.  It allows
to mark some tests as dependent from other tests.  These tests will then be
skipped if any of the dependencies did fail or has been skipped.")
    (license license:asl2.0)))

(define-public python-pytest-datadir
  (package
    (name "python-pytest-datadir")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-datadir" version))
       (sha256
        (base32
         "066bg6wlzgq2pqnjp73dfrcmk8951xw3aqcxa3p1axgqimrixbyk"))))
    (build-system python-build-system)
    (native-inputs
     (list python-setuptools-scm))
    (propagated-inputs
     (list python-pytest python-wheel))
    (home-page "https://github.com/gabrielcnr/pytest-datadir")
    (synopsis "Pytest plugin for manipulating test data directories and files")
    (description
     "This package provides a Pytest plugin for manipulating test data
directories and files.")
    (license license:expat)))

(define-public python-pytest-regressions
  (package
    (name "python-pytest-regressions")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-regressions" version))
       (sha256
        (base32
         "05jpsvv8rj8i4x24fphpnar5dl4s6d6bw6ikjk5d8v96rdviz9qm"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-pytest-datadir python-pyyaml))
    (native-inputs
     (list python-matplotlib
           python-numpy
           python-pandas
           python-pillow
           python-pre-commit
           python-restructuredtext-lint
           python-tox
           python-setuptools-scm
           python-pytest))
    (home-page "https://github.com/ESSS/pytest-regressions")
    (synopsis "Easy to use fixtures to write regression tests")
    (description
     "This plugin makes it simple to test general data, images, files, and numeric
tables by saving expected data in a data directory (courtesy of pytest-datadir)
that can be used to verify that future runs produce the same data.")
    (license license:expat)))
