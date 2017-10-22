;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016 Lukas Gradl <lgradl@openmailbox.org>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2017 ng0 <ng0@infotropique.org>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
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
  #:use-module (gnu packages python)
  #:use-module (guix utils)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial))

(define-public check
  (package
    (name "check")
    (version "0.10.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://github.com/libcheck/check/files/71408/"
                          "/check-" version ".tar.gz"))
      (sha256
       (base32
        "0lhhywf5nxl3dd0hdakra3aasl590756c9kmvyifb3vgm9k0gxgm"))))
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
    (license lgpl2.1+)))

;; XXX: Some packages require this newer version.  Incorporate this
;; into the main 'check' package during the next rebuild cycle.
(define-public check-0.11.0
  (package
    (inherit check)
    (version "0.11.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/libcheck/check/releases"
                                  "/download/" version "/check-" version ".tar.gz"))
              (sha256
               (base32
                "05jn1pgb7hqb937xky2147nnq3r4qy5wwr79rddpax3bms5a9xr4"))))))

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
                   (add-before 'configure 'autoconf
                     (lambda _
                       (zero? (system* "autoreconf" "-vfi")))))))
    (native-inputs
     `(("automake" ,automake)
       ("autoconf" ,autoconf)
       ("libtool" ,libtool)))
    (home-page "http://cunit.sourceforge.net/")
    (synopsis "Automated testing framework for C")
    (description
     "CUnit is a lightweight system for writing, administering, and running
unit tests in C.  It provides C programmers with basic testing functionality
with a flexible variety of user interfaces.")
    (license gpl2+)))

(define-public cppunit
  (package
    (name "cppunit")
    (version "1.13.2")
    (source (origin
             (method url-fetch)
              (uri (string-append "http://dev-www.libreoffice.org/src/"
                                  name "-" version ".tar.gz"))
             (sha256
              (base32
               "17s2kzmkw3kfjhpp72rfppyd7syr7bdq5s69syj2nvrlwd3d4irz"))))
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
    (license lgpl2.1))) ; no copyright notices. LGPL2.1 is in the tarball

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
                     (begin
                       (for-each mkdir-p (list incdir docdir))
                       (install-file (string-append source
                                                 "/single_include/catch.hpp")
                                     incdir)
                       (copy-recursively (string-append source "/docs")
                                         docdir))))))
    (home-page "http://catch-lib.net/")
    (synopsis "Automated test framework for C++ and Objective-C")
    (description
     "Catch stands for C++ Automated Test Cases in Headers and is a
multi-paradigm automated test framework for C++ and Objective-C.")
    (license boost1.0)))

(define-public cmdtest
  (package
    (name "cmdtest")
    (version "0.29")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://git.liw.fi/cmdtest/snapshot/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "1i6gi4yp4qqx1liax098c7nwdb24pghh11xqlrcs7lnhh079rqhb"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         ;; check phase needs to be run before the build phase. If not, the
         ;; coverage test runner looks for tests for the built source files,
         ;; and fails.
         (delete 'check)
         (add-before 'build 'check
           (lambda _
             (substitute* "yarn"
               (("/bin/sh") (which "sh")))
             ;; yarn uses python2-ttystatus to print messages.
             ;; python2-ttystatus requires /dev/tty which is not present in
             ;; the build environment. Hence assuming-failure test fails.
             (delete-file "yarn.tests/assuming-failure.script")
             (delete-file "yarn.tests/assuming-failure.stdout")
             (zero? (system* "python" "setup.py" "check")))))))
    (native-inputs
     `(("python2-coverage-test-runner" ,python2-coverage-test-runner)))
    (propagated-inputs
     `(("python2-cliapp" ,python2-cliapp)
       ("python2-markdown" ,python2-markdown)
       ("python2-ttystatus" ,python2-ttystatus)))
    (home-page "https://liw.fi/cmdtest/")
    (synopsis "Black box Unix program tester")
    (description
     "@code{cmdtest} black box tests Unix command line tools.  Roughly, it is
given a command line and input files, and the expected output, and it verifies
that the command line produces the expected output.  If not, it reports a
problem, and shows the differences.")
    (license gpl3+)))

(define-public cmocka
  (package
    (name "cmocka")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://cmocka.org/files/"
                                  (version-major+minor version) "/cmocka-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1283zi9qf5613g8iadm1fxmjh4rzxqd5np2j3lcpgairf25g8bph"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f)) ; No test target
    (home-page "https://cmocka.org/")
    (synopsis "Unit testing framework for C")
    (description "Cmocka is a unit testing framework for C with support for
mock objects.  It only requires the standard C library, and works with
different compilers.  Cmocka supports several different message output formats
like Test Anything Protocol, Subunit, xUnit XML or the original cmockery output
format.")
    (license asl2.0)))

(define-public cppcheck
  (package
    (name "cppcheck")
    (version "1.81")
    (source (origin
      (method url-fetch)
      (uri (string-append "https://github.com/danmar/cppcheck/archive/"
                          version ".tar.gz"))
      (sha256
       (base32 "0miamqk7pa2dzmnmi5wb6hjp2a3zya1x8afnlcxby8jb6gp6wf8j"))
      (file-name (string-append name "-" version ".tar.gz"))))
    (build-system cmake-build-system)
    (home-page "http://cppcheck.sourceforge.net")
    (synopsis "Static C/C++ code analyzer")
    (description "Cppcheck is a static code analyzer for C and C++.  Unlike
C/C++ compilers and many other analysis tools it does not detect syntax errors
in the code.  Cppcheck primarily detects the types of bugs that the compilers
normally do not detect.  The goal is to detect only real errors in the code
(i.e. have zero false positives).")
    (license gpl3+)))

(define-public googletest
  (package
    (name "googletest")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/google/googletest/archive/"
                           "release-" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1n5p1m2m3fjrjdj752lf92f9wq3pl5cbsfrb49jqbg52ghkz99jq"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DBUILD_SHARED_LIBS=ON")))
    (native-inputs
     `(("python-2" ,python-2)))
    (home-page "https://github.com/google/googletest/")
    (synopsis "Test discovery and XUnit test framework")
    (description "Google Test features an XUnit test framework, automated test
discovery, death tests, assertions, parameterized tests and XML test report
generation.")
    (license bsd-3)))

(define-public cpputest
  (package
    (name "cpputest")
    (version "3.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/cpputest/cpputest/releases/download/v"
                           version "/cpputest-" version ".tar.gz"))
       (sha256
        (base32
         "0mk48xd3klyqi7wf3f4wn4zqxxzmvrhhl32r25jzrixzl72wq7f8"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("googletest" ,googletest)))
    (home-page "https://cpputest.github.io/")
    (synopsis "Unit testing and mocking framework for C/C++")
    (description
     "CppUTest is a C/C++ based unit xUnit test framework.  It is written in
C++ but is used in C and C++ projects and frequently used in embedded systems
but it works for any C/C++ project.")
    (license bsd-3)))

(define-public python-parameterized
  (package
    (name "python-parameterized")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "parameterized" version))
       (sha256
        (base32
         "1qj1939shm48d9ql6fm1nrdy4p7sdyj8clz1szh5swwpf1qqxxfa"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f)) ; there are no tests
    (home-page "https://github.com/wolever/parameterized")
    (synopsis "Parameterized testing with any Python test framework")
    (description
     "Parameterized is a Python library that aims to fix parameterized testing
for every Python test framework.  It supports nose, py.test, and unittest.")
    (license bsd-2)))

(define-public python2-parameterized
  (package-with-python2 python-parameterized))
