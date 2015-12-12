;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages llvm)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages bootstrap)           ;glibc-dynamic-linker
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xml))

(define-public llvm
  (package
    (name "llvm")
    (version "3.6.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://llvm.org/releases/"
                          version "/llvm-" version ".src.tar.xz"))
      (sha256
       (base32
        "153vcvj8gvgwakzr4j0kndc0b7wn91c2g1vy2vg24s6spxcc23gn"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("python" ,python-wrapper)
       ("perl"   ,perl)))
    (arguments
     `(#:configure-flags '("-DCMAKE_SKIP_BUILD_RPATH=FALSE"
                           "-DCMAKE_BUILD_WITH_INSTALL_RPATH=FALSE")))
    (home-page "http://www.llvm.org")
    (synopsis "Optimizing compiler infrastructure")
    (description
     "LLVM is a compiler infrastructure designed for compile-time, link-time,
runtime, and idle-time optimization of programs from arbitrary programming
languages.  It currently supports compilation of C and C++ programs, using
front-ends derived from GCC 4.0.1.  A new front-end for the C family of
languages is in development.  The compiler infrastructure includes mirror sets
of programming tools as well as libraries with equivalent functionality.")
    (license ncsa)))

(define (clang-runtime-from-llvm llvm hash)
  (package
    (name "clang-runtime")
    (version (package-version llvm))
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://llvm.org/releases/"
                           version "/compiler-rt-" version ".src.tar.xz"))
       (sha256 (base32 hash))))
    (build-system cmake-build-system)
    (native-inputs (package-native-inputs llvm))
    (inputs
     `(("llvm" ,llvm)))
    (arguments
     `(;; Don't use '-g' during the build to save space.
       #:build-type "Release"))

    (home-page "http://compiler-rt.llvm.org")
    (synopsis "Runtime library for Clang/LLVM")
    (description
     "The \"clang-runtime\" library provides the implementations of run-time
functions for C and C++ programs.  It also provides header files that allow C
and C++ source code to interface with the \"sanitization\" passes of the clang
compiler.  In LLVM this library is called \"compiler-rt\".")
    (license ncsa)

    ;; <http://compiler-rt.llvm.org/> doesn't list MIPS as supported.
    (supported-systems (delete "mips64el-linux" %supported-systems))))

(define (clang-from-llvm llvm clang-runtime hash)
  (package
    (name "clang")
    (version (package-version llvm))
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://llvm.org/releases/"
                           version "/cfe-" version ".src.tar.xz"))
       (sha256 (base32 hash))
       (patches (list (search-patch "clang-libc-search-path.patch")))))
    ;; Using cmake allows us to treat llvm as an external library.  There
    ;; doesn't seem to be any way to do this with clang's autotools-based
    ;; build system.
    (build-system cmake-build-system)
    (native-inputs (package-native-inputs llvm))
    (inputs
     `(("libxml2" ,libxml2)
       ("gcc-lib" ,gcc "lib")
       ,@(package-inputs llvm)))
    (propagated-inputs
     `(("llvm" ,llvm)
       ("clang-runtime" ,clang-runtime)))
    (arguments
     `(#:configure-flags
       (list "-DCLANG_INCLUDE_TESTS=True"

             ;; Find libgcc_s, crtbegin.o, and crtend.o.
             (string-append "-DGCC_INSTALL_PREFIX="
                            (assoc-ref %build-inputs "gcc-lib"))

             ;; Use a sane default include directory.
             (string-append "-DC_INCLUDE_DIRS="
                            (assoc-ref %build-inputs "libc")
                            "/include"))

       ;; Don't use '-g' during the build to save space.
       #:build-type "Release"

       #:phases (modify-phases %standard-phases
                  (add-after
                   'unpack 'set-glibc-file-names
                   (lambda* (#:key inputs #:allow-other-keys)
                     (let ((libc (assoc-ref inputs "libc"))
                           (compiler-rt (assoc-ref inputs "clang-runtime")))
                       (substitute* "lib/Driver/Tools.cpp"
                         ;; Patch the 'getLinuxDynamicLinker' function to that
                         ;; it uses the right dynamic linker file name.
                         (("/lib64/ld-linux-x86-64.so.2")
                          (string-append libc
                                         ,(glibc-dynamic-linker)))

                         ;; Link to libclang_rt files from clang-runtime.
                         (("TC\\.getDriver\\(\\)\\.ResourceDir")
                          (string-append "\"" compiler-rt "\"")))

                       ;; Same for libc's libdir, to allow crt1.o & co. to be
                       ;; found.
                       (substitute* "lib/Driver/ToolChains.cpp"
                         (("@GLIBC_LIBDIR@")
                          (string-append libc "/lib")))))))))

    ;; Clang supports the same environment variables as GCC.
    (native-search-paths
     (list (search-path-specification
            (variable "CPATH")
            (files '("include")))
           (search-path-specification
            (variable "LIBRARY_PATH")
            (files '("lib" "lib64")))))

    (home-page "http://clang.llvm.org")
    (synopsis "C language family frontend for LLVM")
    (description
     "Clang is a compiler front end for the C, C++, Objective-C and
Objective-C++ programming languages.  It uses LLVM as its back end.  The Clang
project includes the Clang front end, the Clang static analyzer, and several
code analysis tools.")
    (license ncsa)))

(define-public clang-runtime
  (clang-runtime-from-llvm
   llvm
   "11qx8d3pbfqjaj2x207pvlvzihbs1z2xbw4crpz7aid6h1yz6bqg"))

(define-public clang
  (clang-from-llvm llvm clang-runtime
                   "1wwr8s6lzr324hv4s1k6na4j5zv6n9kdhi14s4kb9b13d93814df"))

(define-public llvm-3.5
  (package (inherit llvm)
    (version "3.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://llvm.org/releases/"
                           version "/llvm-" version ".src.tar.xz"))
       (sha256
        (base32
         "00swb43mzlvda8306arlg2jw7g6k3acwfccgf1k4c2pgd3rrkq98"))))))

(define-public clang-runtime-3.5
  (clang-runtime-from-llvm
   llvm-3.5
   "0dl1kbrhz96djsxqr61iw5h788s7ncfpfb7aayixky1bhdaydcx4"))

(define-public clang-3.5
  (clang-from-llvm llvm-3.5 clang-runtime-3.5
                   "12yv3jwdjcbkrx7zjm8wh4jrvb59v8fdw4mnmz3zc1jb00p9k07w"))
