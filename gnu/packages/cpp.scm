;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Ethan R. Jones <doubleplusgood23@gmail.com>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Fis Trivial <ybbs.daans@hotmail.com>
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2019, 2020 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 Jan Wielkiewicz <tona_kosmicznego_smiecia@interia.pl>
;;; Copyright © 2020 Nicolò Balzarotti <nicolo@nixo.xyz>
;;; Copyright © 2020 Roel Janssen <roel@gnu.org>
;;; Copyright © 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2020 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2020 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020 Alexandros Theodotou <alex@zrythm.org>
;;; Copyright © 2020, 2021 Greg Hogan <code@greghogan.com>
;;; Copyright © 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 Milkey Mouse <milkeymouse@meme.institute>
;;; Copyright © 2021 Raghav Gururajan <rg@raghavgururajan.name>

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

(define-module (gnu packages cpp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix modules)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages c)
  #:use-module (gnu packages check)
  #:use-module (gnu packages code)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml))

(define-public range-v3
  (package
    (name "range-v3")
    (version "0.11.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/ericniebler/range-v3.git")
         (commit version)))
       (file-name
        (git-file-name name version))
       (sha256
        (base32 "18230bg4rq9pmm5f8f65j444jpq56rld4fhmpham8q3vr1c1bdjh"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("doxygen" ,doxygen)
       ("perl" ,perl)))
    (inputs
     `(("boost" ,boost)))
    (synopsis "Range library for C++14/17/20")
    (description "Range-v3 is an extension of the Standard Template Library that
makes its iterators and algorithms more powerful by making them composable.
Unlike other range-like solutions which, seek to do away with iterators, in
range-v3 ranges are an abstraction layer on top of iterators.")
    (home-page "https://github.com/ericniebler/range-v3/")
    (license
     (list
      ;; Elements of Programming
      (license:x11-style "file:///LICENSE.txt")
      ;; SGI STL
      license:sgifreeb2.0
      ;;; LibC++ (dual-licensed)
      license:expat
      license:ncsa
      ;; Others
      license:boost1.0))))

(define-public c++-gsl
  (package
    (name "c++-gsl")
    (version "3.1.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/microsoft/GSL.git")
         (commit
          (string-append "v" version))))
       (file-name
        (git-file-name name version))
       (patches
        (search-patches
         "c++-gsl-find-system-gtest.patch"))
       (sha256
        (base32 "0gbvr48f03830g3154bjhw92b8ggmg6wwh5xyb8nppk9v6w752l0"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("googletest" ,googletest)
       ("pkg-config" ,pkg-config)))
    (synopsis "Guidelines Support Library")
    (description "c++-gsl contains functions and types that are suggested for
use by the C++ Core Guidelines maintained by the Standard C++ Foundation.")
    (home-page "https://github.com/microsoft/GSL/")
    (license license:expat)))

(define-public libzen
  (package
    (name "libzen")
    (version "0.4.38")
    (source (origin
              (method url-fetch)
              ;; Warning: This source has proved unreliable 1 time at least.
              ;; Consider an alternate source or report upstream if this
              ;; happens again.
              (uri (string-append "https://mediaarea.net/download/source/"
                                  "libzen/" version "/"
                                  "libzen_" version ".tar.bz2"))
              (sha256
               (base32
                "1nkygc17sndznpcf71fdrhwpm8z9a3hc9csqlafwswh49axhfkjr"))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       ;; The build scripts are not at the root of the archive.
       (modify-phases %standard-phases
         (add-after 'unpack 'pre-configure
           (lambda _
             (chdir "Project/GNU/Library")
             #t)))))
    (home-page "https://github.com/MediaArea/ZenLib")
    (synopsis "C++ utility library")
    (description "ZenLib is a C++ utility library.  It includes classes for handling
strings, configuration, bit streams, threading, translation, and cross-platform
operating system functions.")
    (license license:zlib)))

(define-public rttr
  (package
    (name "rttr")
    (version "0.9.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rttrorg/rttr/")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1yxad8sj40wi75hny8w6imrsx8wjasjmsipnlq559n4b6kl84ijp"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     '(;; No check target. Setting test-target to "unit_test" runs it twice.
       #:tests? #f
       #:configure-flags
       '("-DBUILD_DOCUMENTATION=OFF" "-DBUILD_EXAMPLES=OFF")
       #:phases
       (modify-phases %standard-phases
         ;; library_test fails in chroot.
         (add-after 'unpack 'skip-library-test
           (lambda _
             (substitute* "src/unit_tests/unit_tests.cmake"
               (("misc/library_test.cpp") ""))
             #t)))))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://github.com/rttrorg/rttr/")
    (synopsis "C++ Reflection Library")
    (description
     "RTTR stands for Run Time Type Reflection.  It describes the ability of a
computer program to introspect and modify an object at runtime.  It is also
the name of the library itself, which is written in C++.")
    (license license:expat)))

(define-public rct
  (let* ((commit "b3e6f41d9844ef64420e628e0c65ed98278a843a")
         (revision "2"))
    (package
      (name "rct")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Andersbakken/rct")
                      (commit commit)))
                (sha256
                 (base32
                  "1m2931jacka27ghnpgf1z1plkkr64z0pga4r4zdrfpp2d7xnrdvb"))
                (patches (search-patches "rct-add-missing-headers.patch"))
                (file-name (git-file-name name version))))
      (build-system cmake-build-system)
      (arguments
       '(#:configure-flags
         '("-DWITH_TESTS=ON"            ; To run the test suite
           "-DRCT_RTTI_ENABLED=ON")))
      (native-inputs
       `(("cppunit" ,cppunit)
         ("pkg-config" ,pkg-config)))
      (inputs
       `(("openssl" ,openssl)
         ("zlib" ,zlib)))
      (home-page "https://github.com/Andersbakken/rct")
      (synopsis "C++ library providing Qt-like APIs on top of the STL")
      (description "Rct is a set of C++ tools that provide nicer (more Qt-like)
 APIs on top of Standard Template Library (@dfn{STL}) classes.")
      (license (list license:expat        ; cJSON
                     license:bsd-4)))))   ; everything else (LICENSE.txt)

(define-public dashel
  (package
    (name "dashel")
    (version "1.3.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aseba-community/dashel")
             (commit version)))
       (sha256
        (base32 "0anks2l2i2qp0wlzqck1qgpq15a3l6dg8lw2h8s4nsj7f61lffwy"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f))          ; no tests
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://github.com/aseba-community/dashel")
    (synopsis "Data stream helper encapsulation library")
    (description
     "Dashel is a data stream helper encapsulation C++ library.  It provides a
unified access to TCP/UDP sockets, serial ports, console, and files streams.
It also allows a server application to wait for any activity on any
combination of these streams.")
    (license license:bsd-3)))

(define-public xsimd
  (package
    (name "xsimd")
    (version "7.4.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/QuantStack/xsimd")
             (commit version)))
       (sha256
        (base32 "11by8gbshm4vv6flqp0ihff8c6nmbaqq7ms93b38rrq68bigcply"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags (list "-DBUILD_TESTS=ON")
       #:test-target "xtest"))
    (native-inputs
     `(("googletest" ,googletest)))
    (home-page "https://github.com/QuantStack/xsimd")
    (synopsis "C++ wrappers for SIMD intrinsics and math implementations")
    (description
     "xsimd provides a unified means for using @acronym{SIMD, single instruction
multiple data} features for library authors.  Namely, it enables manipulation of
batches of numbers with the same arithmetic operators as for single values.
It also provides accelerated implementation of common mathematical functions
operating on batches.")
    (license license:bsd-3)))

(define-public chaiscript
  (package
    (name "chaiscript")
    (version "6.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ChaiScript/ChaiScript")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0i1c88rn1wwz8nf3dpapcdkk4w623m3nksfy5yjai10k9irkzy3c"))))
    (build-system cmake-build-system)
    (home-page "https://chaiscript.com/")
    (synopsis "Embedded scripting language designed for C++")
    (description
     "ChaiScript is one of the only embedded scripting language designed from
the ground up to directly target C++ and take advantage of modern C++
development techniques.  Being a native C++ application, it has some advantages
over existing embedded scripting languages:

@enumerate
@item Uses a header-only approach, which makes it easy to integrate with
existing projects.
@item Maintains type safety between your C++ application and the user scripts.
@item Supports a variety of C++ techniques including callbacks, overloaded
functions, class methods, and stl containers.
@end enumerate\n")
    (license license:bsd-3)))

(define-public fifo-map
  (let* ((commit "0dfbf5dacbb15a32c43f912a7e66a54aae39d0f9")
         (revision "0")
         (version (git-version "1.1.1" revision commit)))
    (package
      (name "fifo-map")
      (version version)
      (home-page "https://github.com/nlohmann/fifo_map")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (sha256
                 (base32
                  "0pi77b75kp0l7z454ihcd14nzpi3nc5m4nyjbsgy5f9bw3676196"))
                (patches (search-patches "fifo-map-remove-catch.hpp.patch"
                                         "fifo-map-fix-flags-for-gcc.patch"))
                (file-name (git-file-name name version))
                (modules '((guix build utils)))
                (snippet '(delete-file-recursively "./test/thirdparty"))))
      (native-inputs
       `(("catch2" ,catch-framework2-1)))
      (build-system cmake-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (replace 'check
             (lambda _
               (invoke "./unit")))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (inc (string-append out "/include/fifo_map")))
                 (with-directory-excursion
                     (string-append "../" ,name "-" ,version "-checkout")
                   (install-file "src/fifo_map.hpp" inc)
                   #t)))))))
      (synopsis "FIFO-ordered associative container for C++")
      (description "Fifo_map is a C++ header only library for associative
container which uses the order in which keys were inserted to the container
as ordering relation.")
      (license license:expat))))

(define-public json-modern-cxx
  (package
    (name "json-modern-cxx")
    (version "3.9.1")
    (home-page "https://github.com/nlohmann/json")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference (url home-page)
                           (commit (string-append "v" version))))
       (sha256
        (base32 "0ar4mzp53lskxw3vdzw07f47njcshl3lwid9jfq6l7yx6ds2nyjc"))
       (file-name (git-file-name name version))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Delete bundled software.  Preserve doctest_compatibility.h, which
           ;; is a wrapper library added by this package.
           (install-file "./test/thirdparty/doctest/doctest_compatibility.h" "/tmp")
           (for-each delete-file-recursively
                     '("./third_party" "./test/thirdparty" "./benchmarks/thirdparty"))
           (install-file "/tmp/doctest_compatibility.h" "./test/thirdparty/doctest")

           ;; Adjust for the unbundled fifo_map and doctest.
           (substitute* "./test/thirdparty/doctest/doctest_compatibility.h"
             (("#include \"doctest\\.h\"")
              "#include <doctest/doctest.h>"))
           (with-directory-excursion "test/src"
             (let ((files (find-files "." "\\.cpp$")))
               (substitute* files
                 (("#include ?\"(fifo_map.hpp)\"" all fifo-map-hpp)
                  (string-append
                   "#include <fifo_map/" fifo-map-hpp ">")))))
           #t))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       (list "-DJSON_MultipleHeaders=ON" ; For json_fwd.hpp.
             (string-append "-DJSON_TestDataDirectory="
                            (assoc-ref %build-inputs "json_test_data")))
       #:phases (modify-phases %standard-phases
                  ;; XXX: When tests are enabled, the install phase will cause
                  ;; a needless rebuild without the given configure flags,
                  ;; ultimately creating both $out/lib and $out/lib64.  Move
                  ;; the check phase after install to work around it.
                  (delete 'check)
                  (add-after 'install 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (if tests?
                          ;; Some tests need git and a full checkout, skip those.
                          (invoke "ctest" "-LE" "git_required")
                          (format #t "test suite not run~%"))
                      #t)))))
    (native-inputs
     `(("amalgamate" ,amalgamate)
       ("doctest" ,doctest)
       ("json_test_data"
        ,(let ((version "3.0.0"))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/nlohmann/json_test_data")
                   (commit (string-append "v" version))))
             (file-name (git-file-name "json_test_data" version))
             (sha256
              (base32
               "0nzsjzlvk14dazwh7k2jb1dinb0pv9jbx5jsyn264wvva0y7daiv")))))))
    (inputs
     `(("fifo-map" ,fifo-map)))
    (synopsis "JSON parser and printer library for C++")
    (description "JSON for Modern C++ is a C++ JSON library that provides
intuitive syntax and trivial integration.")
    (license license:expat)))

(define-public nlohmann-json-cpp
  (deprecated-package "nlohmann-json-cpp" json-modern-cxx))

(define-public xtl
  (package
    (name "xtl")
    (version "0.6.23")
    (source (origin
              (method git-fetch)
              (uri
               (git-reference
                (url "https://github.com/QuantStack/xtl")
                (commit version)))
              (sha256
               (base32
                "1kd9zl4h6nrsg29hq13vwp4zhfj8sa90vj40726lpw6vxz48k4di"))
              (file-name (git-file-name name version))))
    (native-inputs
     `(("googletest" ,googletest)
       ("json-modern-cxx" ,json-modern-cxx)))
    (arguments
     `(#:configure-flags
       '("-DBUILD_TESTS=ON")
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* _
             (with-directory-excursion "test"
               (invoke "./test_xtl")
               #t))))))
    (home-page "https://github.com/QuantStack/xtl")
    (build-system cmake-build-system)
    (synopsis "C++ template library providing some basic tools")
    (description "xtl is a C++ header-only template library providing basic
tools (containers, algorithms) used by other QuantStack packages.")
    (license license:bsd-3)))

(define-public ccls
  (package
    (name "ccls")
    (version "0.20201219")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/MaskRay/ccls")
             (commit version)))
       (sha256
        (base32 "0nkg92rgb1x6scpiwdamfrd1ag87j7ajxyn5qi861r916m5mh9m8"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f))                    ; no check target
    (inputs
     `(("rapidjson" ,rapidjson)))
    (native-inputs
     `(("clang" ,clang)
       ("llvm" ,llvm)))
    (home-page "https://github.com/MaskRay/ccls")
    (synopsis "C/C++/Objective-C language server")
    (description
     "@code{ccls} is a server implementing the Language Server Protocol (LSP)
for C, C++ and Objective-C languages.  It uses @code{clang} to perform static
code analysis and supports cross references, hierarchies, completion and
syntax highlighting.  @code{ccls} is derived from @code{cquery} which is not
maintained anymore.")
    (license license:asl2.0)))

(define-public gperftools
  (package
    (name "gperftools")
    (version "2.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gperftools/gperftools")
             (commit (string-append "gperftools-" version))))
       (sha256
        (base32 "19bj2vlsbfwq7m826v2ccqg47kd7cb5vcz1yw2x0v5qzhaxbakk1"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
      ;; The tests are flaky when run in parallel. For more info:
      ;; https://bugs.gnu.org/46562
     '(#:parallel-tests? #f))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ;; For tests.
       ("perl" ,perl)))
    (home-page "https://github.com/gperftools/gperftools")
    (synopsis "Multi-threaded malloc() and performance analysis tools for C++")
    (description
     "@code{gperftools} is a collection of a high-performance multi-threaded
malloc() implementation plus some thread-friendly performance analysis
tools:

@itemize
@item tcmalloc,
@item heap profiler,
@item heap checker,
@item CPU checker.
@end itemize\n")
    (license license:bsd-3)))

(define-public cpplint
  (package
    (name "cpplint")
    (version "1.4.5")
    (source
     (origin
       (method git-fetch)
       ;; Fetch from github instead of pypi, since the test cases are not in
       ;; the pypi archive.
       (uri (git-reference
             (url "https://github.com/cpplint/cpplint")
             (commit version)))
       (sha256
        (base32 "1yzcxqx0186sh80p0ydl9z0ld51fn2cdpz9hmhrp15j53g9ira7c"))
       (file-name (git-file-name name version))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'use-later-pytest
           (lambda _
             (substitute* "test-requirements"
               (("pytest.*") "pytest\n"))
             #t)))))
    (build-system python-build-system)
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-pytest-runner" ,python-pytest-runner)))
    (home-page "https://github.com/cpplint/cpplint")
    (synopsis "Static code checker for C++")
    (description "@code{cpplint} is a command-line tool to check C/C++ files
for style issues following Google’s C++ style guide.  While Google maintains
it's own version of the tool, this is a fork that aims to be more responsive
and make @code{cpplint} usable in wider contexts.")
    (license license:bsd-3)))

(define-public reproc
  (package
    (name "reproc")
    (version "14.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/DaanDeMeyer/reproc")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1n71wb50qv2dmhjgw7azx5gigbrp19l2n3d41g9p05l5l0y1qg0q"))))
   (build-system cmake-build-system)
   (arguments
      ;; No tests.
    `(#:tests? #f
      ;; Enable building of shared library.
      #:configure-flags `("-DBUILD_SHARED_LIBS=1")))
   (native-inputs
    `(("pkg-config" ,pkg-config)))
   (synopsis "Process IO library")
   (description "reproc (Redirected Process) is a C/C++ library that
simplifies starting, stopping and communicating with external programs.  The
main use case is executing command line applications directly from C or C++
code and retrieving their output.")
   (home-page "https://github.com/DaanDeMeyer/reproc")
   (license license:expat)))

(define-public sobjectizer
  (package
    (name "sobjectizer")
    (version "5.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Stiffstream/sobjectizer")
             (commit (string-append "v." version))))
       (sha256
        (base32 "0jfai7sqxnnjkms38krm7mssj5l79nb3pllkbyj4j581a7l5j6l5"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'change-directory
           (lambda _
             (chdir "dev")
             #t)))))
    (home-page "https://stiffstream.com/en/products/sobjectizer.html")
    (synopsis "Cross-platform actor framework for C++")
    (description
     "SObjectizer is a cross-platform \"actor frameworks\" for C++.
SObjectizer supports not only the Actor Model but also the Publish-Subscribe
Model and CSP-like channels.  The goal of SObjectizer is to simplify
development of concurrent and multithreaded applications in C++.")
    (license license:bsd-3)))

(define-public tweeny
  (package
    (name "tweeny")
    (version "3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mobius3/tweeny")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1adm4c17pi7xf3kf6sjyxibz5rdg1ka236p72xsm6js4j9gzlbp4"))))
    (arguments
     '(#:tests? #f))                    ;no check target
    (build-system cmake-build-system)
    (home-page "https://mobius3.github.io/tweeny/")
    (synopsis "Modern C++ tweening library")
    (description "@code{Tweeny} is an inbetweening library designed for the
creation of complex animations for games and other beautiful interactive
software.  It leverages features of modern @code{C++} to empower developers with
an intuitive API for declaring tweenings of any type of value, as long as they
support arithmetic operations.  The goal of @code{Tweeny} is to provide means to
create fluid interpolations when animating position, scale, rotation, frames or
other values of screen objects, by setting their values as the tween starting
point and then, after each tween step, plugging back the result.")
    (license license:expat)))

(define-public abseil-cpp
  (package
    (name "abseil-cpp")
    (version "20200923.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/abseil/abseil-cpp")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1p4djhm1f011ficbjjxx3n8428p8481p20j4glpaawnpsi362hkl"))
              ;; Remove after next googletest release and update.
              (patches
               (search-patches
                "abseil-cpp-fix-gtest.patch"
                "abseil-cpp-fix-strerror_test.patch"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags (list "-DBUILD_SHARED_LIBS=ON"
                               "-DABSL_RUN_TESTS=ON"
                               "-DABSL_USE_EXTERNAL_GOOGLETEST=ON"
                               ;; Needed, else we get errors like:
                               ;;
                               ;; ld: CMakeFiles/absl_periodic_sampler_test.dir/internal/periodic_sampler_test.cc.o:
                               ;;   undefined reference to symbol '_ZN7testing4Mock16UnregisterLockedEPNS_8internal25UntypedFunctionMockerBaseE'
                               ;; ld: /gnu/store/...-googletest-1.10.0/lib/libgmock.so:
                               ;;   error adding symbols: DSO missing from command line
                               ;; collect2: error: ld returned 1 exit status
                               "-DCMAKE_EXE_LINKER_FLAGS=-lgtest -lpthread -lgmock")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'remove-gtest-check
           ;; The CMakeLists fails to find our googletest for some reason, but
           ;; it works nonetheless.
           (lambda _
             (substitute* "CMakeLists.txt"
               (("check_target\\(gtest\\)") "")
               (("check_target\\(gtest_main\\)") "")
               (("check_target\\(gmock\\)") "")))))))
    (native-inputs
     `(("googletest" ,googletest)))
    (home-page "https://abseil.io")
    (synopsis "Augmented C++ standard library")
    (description "Abseil is a collection of C++ library code designed to
augment the C++ standard library.  The Abseil library code is collected from
Google's C++ code base.")
    (license license:asl2.0)))

(define-public pegtl
  (package
    (name "pegtl")
    (version "2.8.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/taocpp/PEGTL")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "17crgjfdx55imi2dqnz6xpvsxq07390yfgkz5nd2g77ydkvq9db3"))))
    (build-system cmake-build-system)
    (home-page "https://github.com/taocpp/PEGTL")
    (synopsis "Parsing Expression Grammar template library")
    (description "The Parsing Expression Grammar Template Library (PEGTL) is
a zero-dependency C++ header-only parser combinator library for creating
parsers according to a Parsing Expression Grammar (PEG).")
    (license license:expat)))

(define-public cxxopts
  (package
    (name "cxxopts")
    (version "2.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jarro2783/cxxopts")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0d3y747lsh1wkalc39nxd088rbypxigm991lk3j91zpn56whrpha"))))
    (build-system cmake-build-system)
    (synopsis "Lightweight C++ command line option parser")
    (description
     "A lightweight header-only C++ option parser library, supporting the
standard GNU style syntax for options.")
    (home-page "https://github.com/jarro2783/cxxopts/wiki")
    (license license:expat)))

(define-public folly
  (package
    (name "folly")
    (version "2021.01.25.00")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/facebook/folly")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "14dl1g6vf7mc90mcync5h2lp14fwcx8n9h91pmiq6rfgv1fjjrwz"))))
    (build-system cmake-build-system)
    (arguments
     '(;; Tests must be explicitly enabled
       ;;#:configure-flags '("-DBUILD_TESTS=ON")))
       ;; Leave tests disabled; see https://github.com/facebook/folly/issues/1456
       #:tests? #f))
    (propagated-inputs
     `(("boost" ,boost)
       ("gflags" ,gflags)
       ("glog" ,glog)
       ("liburing" ,liburing)))
    (inputs
     `(("bzip2" ,bzip2)
       ("double-conversion" ,double-conversion)
       ("fmt" ,fmt)
       ("libaio" ,libaio)
       ("libevent" ,libevent)
       ("libiberty" ,libiberty)
       ("libsodium" ,libsodium)
       ("libunwind" ,libunwind)
       ("lz4" ,lz4)
       ("openssl" ,openssl)
       ("snappy" ,snappy)
       ("zlib" ,zlib)
       ("zstd" ,zstd "lib")))
    (native-inputs
     `(("googletest" ,googletest)))
    (synopsis "Collection of C++ components complementing the standard library")
    (description
     "Folly (acronymed loosely after Facebook Open Source Library) is a library
of C++14 components that complements @code{std} and Boost.")
    (home-page "https://github.com/facebook/folly/wiki")
    ;; 32-bit is not supported: https://github.com/facebook/folly/issues/103
    (supported-systems '("aarch64-linux" "x86_64-linux"))
    (license license:asl2.0)))

(define-public aws-sdk-cpp
  (package
    (name "aws-sdk-cpp")
    (version "1.8.159")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/aws/aws-sdk-cpp")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0jpfv9x82nq7hcix9in7qgrc8009dwpg6gr96hlgmcvqrqckd2r9"))))
    (build-system cmake-build-system)
    (arguments
     '(;; Tests are run during the build phase.
       #:tests? #f
       #:configure-flags
       '("-DBUILD_SHARED_LIBS=ON"
         "-DBUILD_DEPS=OFF")))
    (propagated-inputs
     `(("aws-c-common" ,aws-c-common)
       ("aws-c-event-stream" ,aws-c-event-stream)))
    (inputs
     `(("aws-checksums" ,aws-checksums)
       ("curl" ,curl)
       ("openssl" ,openssl)
       ("zlib" ,zlib)))
    (synopsis "Amazon Web Services SDK for C++")
    (description
     "The AWS SDK for C++ provides a C++11 interface to the @acronym{AWS,Amazon
Web Services} API.  AWS provides on-demand computing infrastructure and software
services including database, analytic, and machine learning technologies.")
    (home-page "https://github.com/aws/aws-sdk-cpp")
    (license license:asl2.0)))

(define-public libexpected
  (package
    (name "libexpected")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/TartanLlama/expected")
             (commit (string-append "v" version))
             ;; NOTE: Requires TL_CMAKE from custom
             ;; repository. Should not affect reproducibility.
             (recursive? #t)))
       (file-name (git-file-name name version))
       ;; NOTE: This patch will be unnecessary on subsequent tags.
       (patches (search-patches "libexpected-nofetch.patch"))
       (sha256
        (base32 "1ckzfrljzzdw9wf8hvdfjz4wjx5na57iwxc48mbv9rf5067m21a5"))))
    (build-system cmake-build-system)
    ;; TODO: Clean up install phase.
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "./tests"))))))
    (native-inputs
     `(("catch2" ,catch-framework2)))
    (synopsis "C++11/14/17 std::expected with functional-style extensions")
    (description "@code{std::expected} is proposed as the preferred way to
represent objects which will either have an expected value, or an unexpected
value giving information about why something failed.  Unfortunately, chaining
together many computations which may fail can be verbose, as error-checking
code will be mixed in with the actual programming logic.  This implementation
provides a number of utilities to make coding with expected cleaner.")
    (home-page "https://tl.tartanllama.xyz/")
    (license license:cc0)))

(define-public magic-enum
  (package
    (name "magic-enum")
    (version "0.7.2")
    (home-page "https://github.com/Neargye/magic_enum")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "07j5zdf3vkliwrcv6k663k35akn7qp23794sz2mnvkj9hbv9s8cx"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("gcc" ,gcc-9)))
    (synopsis "C++17 header only library for compile time reflection of enums")
    (description "Magic Enum offers static reflection of enums, with
conversions to and from strings, iteration and related functionality.")
    (license license:expat)))

(define-public cli11
  (package
    (name "cli11")
    (version "1.9.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/CLIUtils/CLI11")
              (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0hbch0vk8irgmiaxnfqlqys65v1770rxxdfn3d23m2vqyjh0j9l6"))
        (modules '((guix build utils)))
        (snippet
         '(begin (delete-file-recursively "extern")
                 #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       '("-DCLI11_SINGLE_FILE=OFF"
         "-DCLI11_BUILD_EXAMPLES=OFF")
       #:imported-modules ,%cmake-build-system-modules
       #:modules ((guix build cmake-build-system)
                  (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'no-vendor-gtest
           (lambda _
             (substitute* "tests/CMakeLists.txt"
               ;; We provide our own googletest, so this is not really a
               ;; problem.
               (("message\\(FATAL_ERROR \"You have requested")
                "message(TRACE \"You have requested"))
             (substitute* "cmake/AddGoogletest.cmake"
               (("^add_subdirectory\\(.*googletest.*$") "find_package(GTest REQUIRED)")
               (("^set_target_properties\\(gtest gtest_main gmock gmock_main") "")
               (("^    PROPERTIES FOLDER \"Extern\"\\)") ""))
             #t)))))
    (native-inputs
     `(("doxygen" ,doxygen)
       ("googletest" ,googletest)))
    (synopsis "Command line parser for C++11")
    (description
     "CLI11 is a command line parser for C++11 and beyond that provides a rich
feature set with a simple and intuitive interface.")
    (home-page "https://cliutils.github.io/CLI11/book/")
    (license license:bsd-3)))

(define-public caf
  (package
    (name "caf")
    (version "0.18.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/actor-framework/actor-framework")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1c3spd6vm1h9qhlk5c4fdwi6nbqx5vwz2zvv6qp0rj1hx6xpq3cx"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       '("-DCAF_ENABLE_EXAMPLES=OFF")))
    (inputs
     `(("openssl" ,openssl)))
    (synopsis "C++ implementation of the actor model")
    (description "The C++ Actor Framework (CAF) offers a high-level C++17
programming environment using the actor model for concurrent, distributed
computation.")
    (home-page "https://www.actor-framework.org/")
    (license license:bsd-3)))

(define-public pcg-cpp
  (let ((commit "5b5cac8d61339e810c5dbb4692d868a1d7ca1b2d")
        (revision "1"))
    (package
      (name "pcg-cpp")
      (version (git-version "0.98.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/imneme/pcg-cpp")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1s9dcd4iydlc1xj9m6f7c52nlyx99klk043sk7arqy6kp7gdaa33"))))
      (build-system gnu-build-system)
      (arguments
       `(#:test-target "test"
         #:phases
          (modify-phases %standard-phases
            (delete 'configure))
         #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))))
      (synopsis "C++11 header only library for random number generation")
      (description "The Permuted Congruential Generator (PCG) extends the
Linear Congruential Generator (LCG) with a permutation function to increase
output randomness while retaining speed, simplicity, and conciseness.")
      (home-page "https://www.pcg-random.org")
      (license (list license:expat license:asl2.0))))) ; dual licensed

(define-public libcutl
  (package
    (name "libcutl")
    (version "1.10.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.codesynthesis.com/download/libcutl/"
                    (version-major+minor version)
                    "/libcutl-" version ".tar.bz2"))
              (sha256
               (base32
                "070j2x02m4gm1fn7gnymrkbdxflgzxwl7m96aryv8wp3f3366l8j"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Remove bundled sources.
                  (with-directory-excursion "cutl/details"
                    (for-each delete-file-recursively
                              ;; FIXME: Boost_RegEx isn't being detected.
                              (list
                               ;;"boost"
                               "expat")))))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list "--disable-static"
                               ;;"--with-external-boost"
                               "--with-external-expat")))
    (inputs
     `(;;("boost ,boost)
       ("expat" ,expat)))
    (home-page "https://www.codesynthesis.com/projects/libcutl/")
    (synopsis "C++ utility library with generic and independent components")
    (description "libcutl is a C++ utility library.  It contains a collection
of generic and independent components such as meta-programming tests, smart
pointers, containers, compiler building blocks, etc.")
    (license (list license:expat        ;everything except...
                   license:boost1.0)))) ;...the files under cutl/details/boost

(define-public libxsd-frontend
  (package
    (name "libxsd-frontend")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.codesynthesis.com/download/"
                           "libxsd-frontend/" (version-major+minor version)
                           "/libxsd-frontend-" version ".tar.bz2"))
       (sha256
        (base32 "1nmzchsvwvn66jpmcx18anzyl1a3l309x1ld4zllrg37ijc31fim"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:imported-modules ((guix build copy-build-system)
                           ,@%gnu-build-system-modules)
       #:modules (((guix build copy-build-system) #:prefix copy:)
                  (guix build gnu-build-system)
                  (guix build utils))
       #:make-flags (list (string-append "--include-dir="
                                         (assoc-ref %build-inputs "build")
                                         "/include/"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda args
             (apply (assoc-ref copy:%standard-phases 'install)
                    #:install-plan
                    '(("xsd-frontend" "include/xsd-frontend"
                       #:include-regexp ("\\.?xx$"))
                      ("xsd-frontend" "lib"
                       #:include-regexp ("\\.so$")))
                    args))))))
    (native-inputs
     `(("build" ,build)))
    (inputs
     `(("libcutl" ,libcutl)
       ("libxerces-c" ,xerces-c)))
    (synopsis "XSD Front-end")
    (description "@code{libxsd-frontend} is a compiler frontend for the W3C
XML Schema definition language.  It includes a parser, semantic graph types
and a traversal mechanism.")
    (home-page "https://www.codesynthesis.com/projects/libxsd-frontend/")
    (license license:gpl2+)))

(define-public cli
  (package
    (name "cli")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.codesynthesis.com/download/"
                           "cli/" (version-major+minor version)
                           "/cli-" version ".tar.bz2"))
       (sha256
        (base32 "0bg0nsai2q4h3mldpnj0jz4iy4svs0bcfvmq0v0c9cdyknny606g"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:make-flags (list (string-append "--include-dir="
                                         (assoc-ref %build-inputs "build")
                                         "/include")
                          (string-append "install_prefix="
                                         (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch
           (lambda _
             (substitute* (find-files "." "\\.make$")
               (("build-0\\.3")
                (string-append (assoc-ref %build-inputs "build")
                               "/include/build-0.3")))
             ;; Add the namespace prefix, to avoid errors such as "error:
             ;; ‘iterate_and_dispatch’ was not declared in this scope".
             (substitute* (find-files "." "\\.?xx$")
               (("add \\(typeid \\(type\\), \\*this\\);" all)
                (string-append "traverser_map<B>::" all))
               (("iterate_and_dispatch \\(s\\.names_begin.*;" all)
                (string-append "edge_dispatcher::" all)))))
         (delete 'configure))))
    (native-inputs
     `(("build" ,build)))
    (inputs
     `(("libcutl" ,libcutl)))
    (synopsis "C++ Command Line Interface (CLI) definition language")
    (description "@code{cli} is a domain-specific language (DSL) for defining
command line interfaces of C++ programs.  It allows you to describe the
options that your program supports, their types, default values, and
documentation.")
    (home-page "https://codesynthesis.com/projects/cli/")
    (license license:expat)))

(define-public xsd
  (package
    (name "xsd")
    (version "4.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.codesynthesis.com/download/"
                           "xsd/" (version-major+minor version)
                           "/xsd-" version ".tar.bz2"))
       (sha256
        (base32 "1hi9ppxd34np8ydv1h0vgc2qpdmgcd1cdzgk30aidv670xjg91fx"))))
    (build-system gnu-build-system)
    (outputs '("out" "doc"))            ;3.8 MiB of doc and examples
    (arguments
     `(#:test-target "test"
       #:make-flags (list (string-append "--include-dir="
                                         (assoc-ref %build-inputs "build")
                                         "/include/")
                          (string-append "install_prefix="
                                         (assoc-ref %outputs "out")))
       #:phases (modify-phases %standard-phases
                  (add-after 'install 'move-doc
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out"))
                            (doc (assoc-ref outputs "doc")))
                        (mkdir-p (string-append doc "/share/doc"))
                        (rename-file (string-append out "/share/doc/xsd")
                                     (string-append doc "/share/doc/xsd-"
                                                    ,version)))))
                  (delete 'configure))))
    (native-inputs
     `(("build" ,build)
       ("cli" ,cli)))
    (inputs
     `(("libcutl" ,libcutl)
       ("libnsl" ,libnsl)
       ("libxsd-frontend" ,libxsd-frontend)))
    (propagated-inputs
     ;; The code XSD generates requires the following library at run time;
     ;; propagate it for convenience.
     `(("xerces-c" ,xerces-c)))
    (synopsis "XML Data Binding for C++")
    (description "CodeSynthesis XSD (also known as libxsd or xsdcxx) is an XML
Schema to C++ data binding compiler.  Provided with an XML instance
specification (XML Schema), it generates C++ classes that represent the given
vocabulary as well as XML parsing and serialization code.  The data stored in
XML can then be accessed using types and functions that semantically
correspond to an application domain rather than dealing with the intricacies
of reading and writing XML.")
    (home-page "https://codesynthesis.com/projects/xsd/")
    ;; Exceptions are made to allow using the generated source files as well
    ;; as the libxsd library in free software projects whose license is
    ;; incompatible with the GPL v2.  Refer to the file named FLOSSE for the
    ;; details.
    (license license:gpl2+)))
