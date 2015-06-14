;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
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
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial))

(define-public check
  (package
    (name "check")
    (version "0.9.14")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/check/check/"
                          version "/check-" version ".tar.gz"))
      (sha256
       (base32
        "02l4g79d81s07hzywcv1knwj5dyrwjiq2pgxaz7kidxi8m364wn2"))))
    (build-system gnu-build-system)
    (home-page "http://check.sourceforge.net/")
    (synopsis "Unit test framework for C")
    (description
     "Check is a unit testing framework for C.  It features a simple
interface for defining unit tests, putting little in the way of the
developer.  Tests are run in a separate address space, so Check can
catch both assertion failures and code errors that cause segmentation
faults or other signals.  The output from unit tests can be used within
source code editors and IDEs.")
    (license lgpl2.1+)))

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
                 (alist-cons-before
                  'configure 'autoconf
                  (lambda _
                    (zero? (system* "autoreconf" "-vfi")))
                  %standard-phases)))
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
    (version "1.12.1")
    (source (origin
             (method url-fetch)
              (uri (string-append "mirror://sourceforge/cppunit/" name "/"
                                  name "-"
                                  version ".tar.gz"))
             (sha256
              (base32
               "0jm49v5rmc5qw34vqs56gy8xja1dhci73bmh23cig4kcir6a0a5c"))))
    ;; Explicitly link with libdl. This is expected to be done by packages
    ;; relying on cppunit for their tests. However, not all of them do.
    ;; If we added the linker flag to such packages, we would pollute all
    ;; binaries, not only those used for testing.
    (arguments
     `(#:make-flags '("LDFLAGS=-ldl")))
    (build-system gnu-build-system)
    (home-page "http://sourceforge.net/projects/cppunit/")
    (synopsis "Unit testing framework for C++")
    (description "CppUnit is the C++ port of the famous JUnit framework for
unit testing.  Test output is in XML for automatic testing and GUI based for
supervised tests.")
    (license lgpl2.1))) ; no copyright notices. LGPL2.1 is in the tarball

(define-public catch-framework
  (package
    (name "catch")
    (version "1.1.3")                  ;Sub-minor is the build number
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/philsquared/Catch")
                    ;; Semi-arbitrary.
                    (commit "c51e86819d")))
              (file-name (string-append name "-" version))
              (sha256
               (base32
                "0kgi7wxxysgjbpisqfj4dj0k19cyyai92f001zi8gzkybd4fkgv5"))))
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
                       (copy-file (string-append source
                                                 "/single_include/catch.hpp")
                                  (string-append incdir
                                                 "/catch.hpp"))
                       (copy-recursively (string-append source "/docs")
                                         docdir))))))
    (home-page "http://catch-lib.net/")
    (synopsis "Automated test framework for C++ and Objective-C")
    (description
     "Catch stands for C++ Automated Test Cases in Headers and is a
multi-paradigm automated test framework for C++ and Objective-C.")
    (license boost1.0)))
