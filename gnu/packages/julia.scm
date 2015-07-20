;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages julia)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision) ; mpfr
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages version-control)
  #:use-module (ice-9 match))

(define-public julia
  (package
    (name "julia")
    (version "0.3.10")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/JuliaLang/julia/releases/download/v"
                    version "/julia-" version "_c8ceeefcc1.tar.gz"))
              (sha256
               (base32
                "0j6mw6wr35lxid10nh9gz7k6wck3a90ic92w99n1r052325gl9r7"))
              (patches (list (search-patch "julia-0.3.10-fix-empty-array.patch")))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:modules ((ice-9 match)
                  (guix build gnu-build-system)
                  (guix build utils))


       ;; The DSOs use $ORIGIN to refer to each other, but (guix build
       ;; gremlin) doesn't support it yet, so skip this phase.
       #:validate-runpath? #f

       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after
          'unpack 'hardcode-soname-map
          ;; ./src/ccall.cpp creates a map from library names to paths using the
          ;; output of "/sbin/ldconfig -p".  Since ldconfig is not used in Guix,
          ;; we patch ccall.cpp to contain a static map.
          (lambda* (#:key inputs #:allow-other-keys)
            (use-modules (ice-9 match))
            (substitute* "src/ccall.cpp"
              (("jl_read_sonames.*;")
               (string-join
                (map (match-lambda
                       ((input libname soname)
                        (string-append
                         "sonameMap[\"" libname "\"] = "
                         "\"" (assoc-ref inputs input) "/lib/" soname "\";")))
                     '(("libc"        "libc"           "libc.so.6")
                       ("pcre"        "libpcre"        "libpcre.so")
                       ("mpfr"        "libmpfr"        "libmpfr.so")
                       ("openblas"    "libblas"        "libopenblas.so")
                       ("arpack-ng"   "libarpack"      "libarpack.so")
                       ("lapack"      "liblapack"      "liblapack.so")
                       ("gmp"         "libgmp"         "libgmp.so")
                       ("openlibm"    "libopenlibm"    "libopenlibm.so")
                       ("openspecfun" "libopenspecfun" "libopenspecfun.so")
                       ("fftw"        "libfftw3"       "libfftw3.so")
                       ("fftwf"       "libfftw3f"      "libfftw3f.so"))))))
            #t))
         ;; This phase will no longer be necessary in 0.3.11; see
         ;; https://github.com/JuliaLang/julia/issues/12028
         (add-before
          'build 'fix-building-with-mcjit-llvm
          (lambda _
            (substitute* "src/cgutils.cpp"
              (("addComdat\\(gv\\);") ""))
            #t))
         (add-before
          'build 'patch-include-path
          (lambda _
            (substitute* "deps/Makefile"
              (("/usr/include/double-conversion")
               (string-append (assoc-ref %build-inputs "double-conversion")
                              "/include/double-conversion")))
            #t))
         (add-before
          'build 'replace-default-shell
          (lambda _
            (substitute* "base/client.jl"
              (("/bin/sh") (which "sh")))
            #t))
         (add-before
          'check 'disable-broken-test
          ;; One test fails because it produces slightly different output.
          (lambda _
            (substitute* "test/repl.jl"
              (("@test output") "# @test output"))
            #t)))
       #:make-flags
       (list
        (string-append "prefix=" (assoc-ref %outputs "out"))

        ;; Passing the MARCH flag is necessary to build binary substitutes for
        ;; the supported architectures.
        ,(match (or (%current-target-system)
                    (%current-system))
           ("x86_64-linux" "MARCH=x86-64")
           ("i686-linux" "MARCH=pentium4")
           ;; Prevent errors when querying this package on unsupported
           ;; platforms, e.g. when running "guix package --search="
           (_ "MARCH=UNSUPPORTED"))

        "CONFIG_SHELL=bash"     ;needed to build bundled libraries
        "USE_SYSTEM_LIBUV=0"    ;Julia expects a modified libuv
        "USE_SYSTEM_DSFMT=0"    ;not packaged for Guix and upstream has no
                                ;build system for a shared library.
        "USE_SYSTEM_RMATH=0"    ;Julia uses a bundled version of R's math
                                ;library, patched to use the DSFMT RNG.

        "USE_SYSTEM_LAPACK=1"
        "USE_SYSTEM_BLAS=1"
        "USE_BLAS64=0"          ;needed when USE_SYSTEM_BLAS=1

        "USE_SYSTEM_FFTW=1"
        "LIBFFTWNAME=libfftw3"
        "LIBFFTWFNAME=libfftw3f"

        ;; TODO: Suitesparse does not install shared libraries, so we cannot
        ;; use the suitesparse package.
        ;; "USE_SYSTEM_SUITESPARSE=1"
        ;; (string-append "SUITESPARSE_INC=-I "
        ;;                (assoc-ref %build-inputs "suitesparse")
        ;;                "/include")

        "USE_SYSTEM_GRISU=1"    ;for double-conversion
        "USE_SYSTEM_UTF8PROC=1"
        "USE_SYSTEM_LLVM=1"
        "USE_SYSTEM_LIBUNWIND=1"
        "USE_SYSTEM_PCRE=1"
        "USE_SYSTEM_OPENLIBM=1"
        "USE_SYSTEM_GMP=1"
        "USE_SYSTEM_MPFR=1"
        "USE_SYSTEM_ARPACK=1"
        "USE_SYSTEM_LIBGIT2=1"
        "USE_SYSTEM_OPENSPECFUN=1")))
    (inputs
     `(("llvm" ,llvm-3.5)
       ("arpack-ng" ,arpack-ng)
       ("lapack" ,lapack)
       ("openblas" ,openblas) ;Julia does not build with Atlas
       ("libunwind" ,libunwind)
       ("openlibm" ,openlibm)
       ("openspecfun" ,openspecfun)
       ("double-conversion" ,double-conversion)
       ("fftw" ,fftw)
       ("fftwf" ,fftwf)
       ("fortran" ,gfortran)
       ("pcre" ,pcre)
       ("utf8proc" ,utf8proc)
       ("git" ,git)
       ("mpfr" ,mpfr)
       ("gmp" ,gmp)))
    (native-inputs
     `(("perl" ,perl)
       ("patchelf" ,patchelf)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)
       ("which" ,which)))
    ;; Julia is not officially released for ARM and MIPS.
    ;; See https://github.com/JuliaLang/julia/issues/10639
    (supported-systems '("i686-linux" "x86_64-linux"))
    (home-page "http://julialang.org/")
    (synopsis "High-performance dynamic language for technical computing")
    (description
     "Julia is a high-level, high-performance dynamic programming language for
technical computing, with syntax that is familiar to users of other technical
computing environments.  It provides a sophisticated compiler, distributed
parallel execution, numerical accuracy, and an extensive mathematical function
library.")
    (license license:expat)))
