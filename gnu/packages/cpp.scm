;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Ethan R. Jones <doubleplusgood23@gmail.com>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Fis Trivial <ybbs.daans@hotmail.com>
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 Jan Wielkiewicz <tona_kosmicznego_smiecia@interia.pl>
;;; Copyright © 2020 Nicolò Balzarotti <nicolo@nixo.xyz>
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
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages code)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web))

(define-public libzen
  (package
    (name "libzen")
    (version "0.4.37")
    (source (origin
              (method url-fetch)
              ;; Warning: This source has proved unreliable 1 time at least.
              ;; Consider an alternate source or report upstream if this
              ;; happens again.
              (uri (string-append "https://mediaarea.net/download/source/"
                                  name "/" version "/"
                                  name "_" version ".tar.bz2"))
              (sha256
               (base32
                "1dkqbgabzpa6bd7dkqrvd35sdxrhr6qxalb88f3dw0afk65xqb0k"))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       ;; build scripts not in root of archive
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
             (url "https://github.com/aseba-community/dashel.git")
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
    (version "7.2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/QuantStack/xsimd.git")
             (commit version)))
       (sha256
        (base32 "1ny2qin1j4h35mljivh8z52kwdyjxf4yxlzb8j52ji91v2ccc88j"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags (list "-DBUILD_TESTS=ON")
       #:test-target "xtest"))
    (native-inputs
     `(("googletest" ,googletest)))
    (home-page "https://github.com/QuantStack/xsimd")
    (synopsis "C++ wrappers for SIMD intrinsics and math implementations")
    (description "xsimd provides a unified means for using SIMD features for
library authors.  Namely, it enables manipulation of batches of numbers with
the same arithmetic operators as for single values.  It also provides
accelerated implementation of common mathematical functions operating on
batches.")
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
    (version "3.7.3")
    (home-page "https://github.com/nlohmann/json")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference (url home-page)
                           (commit (string-append "v" version))))
       (sha256
        (base32
         "04rry1xzis71z5gj1ylcj8b4li5q18zxhcwaviwvi3hx0frzxl9w"))
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
    (native-inputs
     `(("amalgamate" ,amalgamate)
       ("doctest" ,doctest)))
    (inputs
     `(("fifo-map" ,fifo-map)))
    (build-system cmake-build-system)
    (synopsis "JSON parser and printer library for C++")
    (description "JSON for Modern C++ is a C++ JSON library that provides
intuitive syntax and trivial integration.")
    (license license:expat)))

(define-public nlohmann-json-cpp
  (deprecated-package "nlohmann-json-cpp" json-modern-cxx))

(define-public xtl
  (package
    (name "xtl")
    (version "0.6.8")
    (source (origin
              (method git-fetch)
              (uri
               (git-reference
                (url "https://github.com/QuantStack/xtl.git")
                (commit version)))
              (sha256
               (base32
                "13gm8vm1b9nzvlcc632f9khnjw1xdjqj6c7k51r173y1hlk0div7"))
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
    (version "0.20190823.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/MaskRay/ccls")
             (commit version)))
       (sha256
        (base32 "0b2pkpzn576b92zcxpwchpkyw2fww6s69818rx4g9z34kzm35zy5"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f)) ; no check target.
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
    (version "2.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gperftools/gperftools")
             (commit (string-append "gperftools-" version))))
       (sha256
        (base32 "0amvwrzn5qc0b0jpxpy5g6zkmj97zjh4hhjrd130hsg2lwwcwhy1"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ;; For tests:
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
    (version "1.4.4")
    (source
     (origin
       (method git-fetch)
       ;; Fetch from github instead of pypi, since the test cases are not in
       ;; the pypi archive.
       (uri (git-reference
             (url "https://github.com/cpplint/cpplint")
             (commit version)))
       (sha256
        (base32 "1ns9wbizr10w7rpyp106d7ip68s5nyskr54vw9bij11sci9z0v3j"))
       (file-name (git-file-name name version))))
    (build-system python-build-system)
    (home-page "https://github.com/cpplint/cpplint")
    (synopsis "Static code checker for C++")
    (description "@code{cpplint} is a command-line tool to check C/C++ files
for style issues following Google’s C++ style guide.  While Google maintains
it's own version of the tool, this is a fork that aims to be more responsive
and make @code{cpplint} usable in wider contexts.")
    (license license:bsd-3)))

(define-public sobjectizer
  (package
    (name "sobjectizer")
    (version "5.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Stiffstream/sobjectizer.git")
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
             (url "https://github.com/mobius3/tweeny.git")
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
