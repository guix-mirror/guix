;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020, 2021 Nicolò Balzarotti <nicolo@nixo.xyz>
;;; Copyright © 2020 Tim Howes <timhowes@lavabit.com>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Jean-Baptiste Volatier <jbv@pm.me>
;;; Copyright © 2021 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision) ; mpfr
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wget)
  #:use-module (ice-9 match))

(define libunwind-julia
  ;; The Julia projects requires their patched version.
  ;; Get from https://github.com/JuliaLang/julia/tree/master/deps/patches
  (package
    (inherit libunwind)
    (name "libunwind-julia")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://savannah/libunwind/libunwind-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1y0l08k6ak1mqbfj6accf9s5686kljwgsl4vcqpxzk5n74wpm6a3"))
       (patches
        (append
            ;; Fix linker issue for i686-linux because GCC10 changed default
            ;; (see '-fno-common' option).
            (search-patches "libunwind-julia-fix-GCC10-fno-common.patch")
            (list
             (julia-patch "libunwind-prefer-extbl"
                          "0lr4dafw8qyfh8sw8hhbwkql1dlhqv8px7k81y2l20hhxfgnh2m1")
             (julia-patch "libunwind-static-arm"
                          "1jk3bmiw61ypcchqkk1fyg5wh8wpggk574wxyfyaic870zh3lhgq")
             (julia-patch "libunwind-cfa-rsp"
                          "1aswjhvysahhldbzh1afbf0hsjxrvs6xidsz2i7s1cjkjbdiia1z"))))))
    (home-page "https://github.com/JuliaLang/tree/master/deps/")))

(define (julia-patch-url version name)
  (string-append "https://raw.githubusercontent.com/JuliaLang/julia/v" version
                 "/deps/patches/" name ".patch"))

(define-public (julia-patch name sha)
  (let ((version "1.6.1"))
    (origin (method url-fetch)
            (uri (julia-patch-url version name))
            (sha256 (base32 sha))
            (file-name name))))

(define-public libwhich
  (package
    (name "libwhich")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vtjnash/libwhich")
             ;; fixes linux-vdso.so related tests
             (commit "87cffe10080c98e7b5786c5166e420bf1ada1d41")))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32
         "1bpa0fcqpa3ai3hm8mz0p13bf76fsq53wsfcx5qw302zh22108xr"))))
    (arguments
     `(#:make-flags
       (list (string-append "CC=" ,(cc-for-target)))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'check 'set-ld-library-path
           (lambda* (#:key native-inputs inputs #:allow-other-keys)
             (setenv "LD_LIBRARY_PATH"
                     (string-append (assoc-ref (or native-inputs inputs) "zlib")
                                    "/lib"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "libwhich" (string-append out "/bin")))
             #t)))))
    (native-inputs
     ;; used for tests
     (list zlib))
    (build-system gnu-build-system)
    (home-page "https://github.com/vtjnash/libwhich")
    (synopsis "Like @code{which}, for dynamic libraries")
    (description "@code{libwhich} is like @code{which}, but for dynamic
libraries.  It is also a bit like @code{ldd} and @code{otool -L}.")
    (license license:expat)))

(define-public julia
  (package
    (name "julia")
    (version "1.6.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/JuliaLang/julia/releases/download/v"
                    version "/julia-" version ".tar.gz"))
              (sha256
               (base32
                "1515x8fs25l3f9csbmd1v4nm041zvjnvigy6s5iidy4yrkwdx4r5"))
              (patches
               (search-patches "julia-SOURCE_DATE_EPOCH-mtime.patch"
                               "julia-allow-parallel-build.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:modules ((ice-9 match)
                  (guix build gnu-build-system)
                  (guix build utils))

       ;; The test suite takes many times longer than building and
       ;; can easily fail on smaller machines when they run out of memory.
       #:tests? ,(not (target-aarch64?))

       ;; Do not strip binaries to keep support for full backtraces.
       ;; See https://github.com/JuliaLang/julia/issues/17831
       #:strip-binaries? #f

       ;; The DSOs use $ORIGIN to refer to each other, but (guix build
       ;; gremlin) doesn't support it yet, so skip this phase.
       #:validate-runpath? #f

       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'prepare-deps
           (lambda* (#:key inputs #:allow-other-keys)
             ;; needed by libwhich
             (setenv "LD_LIBRARY_PATH"
                     (string-join (map (lambda (pkg)
                                         (string-append (assoc-ref inputs pkg)
                                                        "/lib"))
                                       '("curl" "dsfmt"
                                         "gmp" "lapack"
                                         "libssh2" "libnghttp2" "libgit2"
                                         "mbedtls" "mpfr"
                                         "openblas" "openlibm" "pcre2"
                                         "suitesparse" "gfortran:lib"))
                                  ":"))))
         ;; FIXME: Building the documentation requires Julia packages that
         ;; would be downloaded from the Internet.  We should build them in a
         ;; separate build phase.
         (add-after 'unpack 'disable-documentation
           (lambda _
             (substitute* "Makefile"
               (("(install: .*) \\$\\(BUILDROOT\\)/doc/_build/html/en/index.html" _ line)
                (string-append line "\n"))
               (("src ui doc deps")
                "src ui deps"))))
         (add-after 'unpack 'use-system-libwhich
           (lambda* (#:key inputs #:allow-other-keys)
             ;; don't build it
             (substitute* "deps/Makefile"
               (("DEP_LIBS \\+= libwhich") ""))
             ;; call our version
             (substitute* "base/Makefile"
               (("\\$\\$\\(build_depsbindir\\)/libwhich")
                (search-input-file inputs "/bin/libwhich")))))
         (add-after 'unpack 'change-number-of-precompile-statements
           (lambda _
             ;; Remove nss-certs drops the number of statements below 1200,
             ;; causing the build to fail prematurely.
             (substitute* "contrib/generate_precompile.jl"
               (("1200") "1100"))))
         ;; For some reason libquadmath is unavailable on this architecture.
         ;; https://github.com/JuliaLang/julia/issues/41613
         ,@(if (target-aarch64?)
             '((add-after 'unpack 'drop-libquadmath-on-aarch64
                 (lambda _
                   (substitute* '("contrib/fixup-libgfortran.sh"
                                  "deps/csl.mk"
                                  "base/Makefile")
                     ((".*libquadmath.*") ""))
                   (substitute* "Makefile"
                     (("libquadmath ") ""))
                   #t)))
             '())
         (add-before 'check 'set-home
           ;; Some tests require a home directory to be set.
           (lambda _ (setenv "HOME" "/tmp") #t))
         (add-before 'build 'fix-include-and-link-paths
           (lambda* (#:key inputs #:allow-other-keys)
             ;; LIBUTF8PROC is a linker flag, not a build target.  It is
             ;; included in the LIBFILES_* variable which is used as a
             ;; collection of build targets and a list of libraries to link
             ;; against.
             (substitute* "src/flisp/Makefile"
               (("\\$\\(BUILDDIR\\)/\\$\\(EXENAME\\)\\$\\(EXE\\): \\$\\(OBJS\\) \\$\\(LIBFILES_release\\)")
                "$(BUILDDIR)/$(EXENAME)$(EXE): $(OBJS) $(LLT_release)")
               (("\\$\\(BUILDDIR\\)/\\$\\(EXENAME\\)-debug$(EXE): \\$\\(DOBJS\\) \\$\\(LIBFILES_debug\\)")
                "$(BUILDDIR)/$(EXENAME)-debug\\$\\(EXE\\): $(DOBJS) $(LLT_debug)"))

             ;; The REPL must be linked with libuv.
             (substitute* "cli/Makefile"
               (("JLDFLAGS \\+= ")
                (string-append "JLDFLAGS += "
                               (assoc-ref %build-inputs "libuv")
                               "/lib/libuv.so ")))

             (substitute* "base/Makefile"
               (("\\$\\(build_includedir\\)/uv/errno.h")
                (search-input-file inputs "/include/uv/errno.h")))))
         (add-before 'build 'replace-default-shell
           (lambda _
             (substitute* "base/client.jl"
               (("/bin/sh") (which "sh")))))
         (add-before 'build 'shared-objects-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((jlpath
                    (lambda (pkgname)
                      (string-append
                       "stdlib/" pkgname "_jll/src/" pkgname "_jll.jl")))
                   (from
                    (lambda (libname)
                      (string-append "const " libname " = .*\\.so")))
                   (to
                    (lambda* (pkg libname #:optional libname_jl)
                      (string-append
                       "const " (or libname_jl libname)  "= \""
                       (assoc-ref inputs pkg) "/lib/" libname ".so"))))
               (substitute* (jlpath "dSFMT")
                 (((from "libdSFMT")) (to "dsfmt" "libdSFMT")))
               (substitute* (jlpath "GMP")
                 (((from "libgmp")) (to "gmp" "libgmp"))
                 (((from "libgmpxx")) (to "gmp" "libgmpxx")))
               (substitute* (jlpath "libLLVM")
                 (((from "libLLVM")) (to "llvm" "libLLVM")))
               (substitute* (jlpath "LibCURL")
                 (((from "libcurl")) (to "curl" "libcurl")))
               (substitute* (jlpath "LibGit2")
                 (((from "libgit2")) (to "libgit2" "libgit2")))
               (substitute* (jlpath "LibSSH2")
                 (((from "libssh2")) (to "libssh2" "libssh2")))
               (substitute* (jlpath "LibUV")
                 (((from "libuv")) (to "libuv" "libuv")))
               (substitute* (jlpath "LibUnwind")
                 (((from "libunwind")) (to "libunwind" "libunwind")))
               (substitute* (jlpath "MPFR")
                 (((from "libmpfr")) (to "mpfr" "libmpfr")))
               (substitute* (jlpath "MbedTLS")
                 ;; For the newer version of mbedtls-apache:
                 (("libmbedcrypto.so.5") "libmbedcrypto.so.6")
                 (((from "libmbedcrypto")) (to "mbedtls" "libmbedcrypto"))
                 (((from "libmbedtls")) (to "mbedtls" "libmbedtls"))
                 (((from "libmbedx509")) (to "mbedtls" "libmbedx509")))
               (substitute* (jlpath "nghttp2")
                 (((from "libnghttp2")) (to "libnghttp2" "libnghttp2")))
               (substitute* (jlpath "OpenBLAS")
                 (((from "libopenblas")) (to "openblas" "libopenblas")))
               (substitute* (jlpath "OpenLibm")
                 (((from "libopenlibm")) (to "openlibm" "libopenlibm")))
               (substitute* (jlpath "PCRE2")
                 (((from "libpcre2")) (to "pcre2" "libpcre2" "libpcre2_8")))
               (substitute* (jlpath "SuiteSparse")
                 (((from "libamd")) (to "suitesparse" "libamd"))
                 (((from "libbtf")) (to "suitesparse" "libbtf"))
                 (((from "libcamd")) (to "suitesparse" "libcamd"))
                 (((from "libccolamd")) (to "suitesparse" "libccolamd"))
                 (((from "libcholmod")) (to "suitesparse" "libcholmod"))
                 (((from "libcolamd")) (to "suitesparse" "libcolamd"))
                 (((from "libklu")) (to "suitesparse" "libklu"))
                 (((from "libldl")) (to "suitesparse" "libldl"))
                 (((from "librbio")) (to "suitesparse" "librbio"))
                 (((from "libspqr")) (to "suitesparse" "libspqr"))
                 (((from "libsuitesparse")) (to "suitesparse" "libsuitesparse"))
                 (((from "libsuitesparseconfig"))
                  (to "suitesparse" "libsuitesparseconfig"))
                 (((from "libumfpack")) (to "suitesparse" "libumfpack")))
               (substitute* (jlpath "Zlib")
                 (((from "libz")) (to "zlib" "libz"))))))
         (add-after 'unpack 'enable-parallel-tests
           (lambda* (#:key parallel-tests? #:allow-other-keys)
             (setenv "JULIA_CPU_THREADS" (if parallel-tests?
                                             (number->string (parallel-job-count))
                                             "1"))
             (format #t "JULIA_CPU_THREADS environment variable set to ~a~%"
                     (getenv "JULIA_CPU_THREADS"))))
         (add-after 'unpack 'adjust-test-suite
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((pcre2 (assoc-ref inputs "pcre2"))
                   (mbedtls-apache (assoc-ref inputs "mbedtls"))
                   (mpfr (assoc-ref inputs "mpfr"))
                   (gmp (assoc-ref inputs "gmp"))
                   (nghttp2 (assoc-ref inputs "libnghttp2"))
                   (suitesparse (assoc-ref inputs "suitesparse")))
               ;; Some tests only check to see if the input is the correct version.
               (substitute* "stdlib/PCRE2_jll/test/runtests.jl"
                 (("10.36.0") ,(package-version pcre2)))
               (substitute* "stdlib/MbedTLS_jll/test/runtests.jl"
                 (("2.24.0") ,(package-version mbedtls-apache)))
               (substitute* "stdlib/MPFR_jll/test/runtests.jl"
                 (("4.1.0") ,(package-version mpfr)))
               (substitute* "stdlib/GMP_jll/test/runtests.jl"
                 (("6.2.0") ,(package-version gmp)))
               (substitute* "stdlib/nghttp2_jll/test/runtests.jl"
                 (("1.41.0") ,(package-version nghttp2)))
               (substitute* "stdlib/SuiteSparse_jll/test/runtests.jl"
                 (("5004") ,(string-replace-substring
                              (version-major+minor
                                (package-version suitesparse)) "." "0"))))))
         (add-before 'check 'disable-broken-tests
           (lambda _
             ;; disabling REPL tests because they require a stdin
             ;; There are some read-only precompile issues in the 1.6 series.
             ;; https://github.com/JuliaLang/julia/pull/41614
             ;; https://github.com/JuliaLang/julia/issues/41156
             (substitute* "test/choosetests.jl"
               (("skip_tests = \\[\\]")
                "skip_tests = [\"REPL\", \"precompile\"]"))
             ;; Dates/io tests fail on master when networking is unavailable
             ;; https://github.com/JuliaLang/julia/issues/34655
             (substitute* "stdlib/Dates/test/io.jl"
               (("using Dates") "import Dates
using Dates: @dateformat_str, Date, DateTime, DateFormat, Time"))
             ;; Upstream bug I found when packaging
             ;; https://github.com/JuliaLang/julia/issues/35785
             (substitute* "test/file.jl"
               (("@test dirname\\(t\\) == d") "@test_broken dirname(t) == d"))
             ;; julia embeds a certificate, we are not doing that
             (substitute* "stdlib/MozillaCACerts_jll/test/runtests.jl"
               (("@test isfile\\(MozillaCACerts_jll.cacert\\)")
                "@test_broken isfile(MozillaCACerts_jll.cacert)"))
             ;; since certificate is not present some tests are failing in network option
             (substitute* "usr/share/julia/stdlib/v1.6/NetworkOptions/test/runtests.jl"
               (("@test isfile\\(bundled_ca_roots\\(\\)\\)")
                "@test_broken isfile(bundled_ca_roots())")
               (("@test ispath\\(ca_roots_path\\(\\)\\)")
                "@test_broken ispath(ca_roots_path())")
               (("@test ca_roots_path\\(\\) \\!= bundled_ca_roots\\(\\)")
                "@test_broken ca_roots_path() != bundled_ca_roots()"))
             ;; WARNING: failed to select UTF-8 encoding, using ASCII
             ;; Using 'setlocale' doesn't affect the test failures.
             ;(setlocale LC_ALL "en_US.utf8")
             ;(setenv "LC_ALL" "en_US.utf8")
             (substitute* "test/cmdlineargs.jl"
               (("test v\\[3") "test_broken v[3")
               (("test isempty\\(v\\[3") "test_broken isempty(v[3"))
             ;; These tests randomly fails because they depend on CPU.
             (substitute* "stdlib/LinearAlgebra/test/matmul.jl"
               ;; Fixed in v1.6.4 (see:
               ;; https://github.com/JuliaLang/julia/blob/v1.6.4/
               ;; stdlib/LinearAlgebra/test/matmul.jl#L155).
               (("@test mul\\!\\(C, vf, transpose\\(vf\\), 2, 3\\)\
 == 2vf\\*vf' \\.\\+ 3C0")
                "@test mul!(C, vf, transpose(vf), 2, 3) ≈ 2vf*vf' .+ 3C0"))
             (substitute* "test/math.jl"
               ;; @test_broken cannot be used because if the test randomly
               ;; passes, then it also raises an error.
               (("@test isinf\\(log1p\\(-one\\(T\\)\\)\\)")
                " "))))
         (add-before 'install 'symlink-libraries
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((link
                    (lambda (pkgname dir pred)
                      (map (lambda (file)
                             (unless (file-exists?
                                       (string-append dir (basename file)))
                               (symlink file (string-append dir (basename file)))))
                           (find-files (string-append (assoc-ref inputs pkgname)
                                                      "/lib") pred)))))
               (link "curl" "usr/lib/" "\\.so") ; missing libpthreads libLLVM-11jl
               (link "suitesparse" "usr/lib/julia/" "libbtf\\.so")
               (link "suitesparse" "usr/lib/julia/" "libklu\\.so")
               (link "suitesparse" "usr/lib/julia/" "libldl\\.so")
               (link "suitesparse" "usr/lib/julia/" "librbio\\.so")
               (link "gmp" "usr/lib/julia/" "libgmpxx\\.so")
               (link "libuv" "usr/lib/julia/" "libuv\\.so")
               (link "zlib" "usr/lib/julia/" "libz\\.so")
               (link "libunwind" "usr/lib/julia/" "libunwind\\.so")
               (symlink (string-append (assoc-ref inputs "p7zip") "/bin/7z")
                        "usr/libexec/7z"))))
         (add-after 'install 'symlink-llvm-utf8proc
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (link
                      (lambda (pkgname pred)
                        (map (lambda (file)
                               (unless (file-exists?
                                         (string-append out "/lib/julia/"
                                                        (basename file)))
                                 (symlink file (string-append out "/lib/julia/"
                                                              (basename file)))))
                        (find-files (string-append (assoc-ref inputs pkgname)
                                                   "/lib") pred)))))
               (link "llvm" "libLLVM-11jl\\.so")
               (link "utf8proc" "libutf8proc\\.so"))))
         (add-after 'install 'make-wrapper
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (program "julia"))
               (with-directory-excursion bin
                 (wrap-program program
                   `("JULIA_LOAD_PATH" ":" prefix
                     ("" "$JULIA_LOAD_PATH"))
                   `("JULIA_DEPOT_PATH" ":" prefix
                     ("" "$JULIA_DEPOT_PATH"))))))))
       #:make-flags
       (list
        (string-append "prefix=" (assoc-ref %outputs "out"))

         ;; Passing the MARCH or JULIA_CPU_TARGET flag is necessary to build
         ;; binary substitutes for the supported architectures.  See also
         ;; https://docs.julialang.org/en/v1/devdocs/sysimg/#Specifying-multiple-system-image-targets
         ,(match (or (%current-target-system)
                     (%current-system))
                 ("x86_64-linux"
                  ;; These are the flags that upstream uses for their binaries.
                  "JULIA_CPU_TARGET=generic;generic,-cx16,clone_all;sandybridge,-xsaveopt,clone_all;haswell,-rdrnd,base(1)")
                 ("i686-linux" "MARCH=pentium4")
                 ("armhf-linux" "JULIA_CPU_TARGET=armv7-a,neon")
                 ("powerpc64le-linux" "JULIA_CPU_TARGET=pwr8")
                 ;; Prevent errors when querying this package on unsupported
                 ;; platforms, e.g. when running "guix package --search="
                 ;; and also of targeting the builder's architecture.
                 (_ "JULIA_CPU_TARGET=generic"))

         "CONFIG_SHELL=bash -x"     ; needed to build bundled libraries
         "USE_BINARYBUILDER=0"
         ;; list (and order!) of "USE_SYSTEM_*" is here:
         ;; https://github.com/JuliaLang/julia/blob/v1.6.0/Make.inc
         "USE_SYSTEM_CSL=1"
         "USE_SYSTEM_LLVM=1"
         "USE_SYSTEM_LIBUNWIND=1"
         "USE_SYSTEM_PCRE=1"
         "USE_SYSTEM_OPENLIBM=1"
         "USE_SYSTEM_DSFMT=1"
         "USE_SYSTEM_BLAS=1"
         "USE_SYSTEM_LAPACK=1"
         "USE_SYSTEM_GMP=1"
         "USE_SYSTEM_MPFR=1"
         "USE_SYSTEM_SUITESPARSE=1"
         "USE_SYSTEM_LIBUV=1"
         "USE_SYSTEM_UTF8PROC=1"
         "USE_SYSTEM_MBEDTLS=1"
         "USE_SYSTEM_LIBSSH2=1"
         "USE_SYSTEM_NGHTTP2=1"
         "USE_SYSTEM_CURL=1"
         "USE_SYSTEM_LIBGIT2=1"
         "USE_SYSTEM_PATCHELF=1"
         "USE_SYSTEM_ZLIB=1"
         "USE_SYSTEM_P7ZIP=1"

         "NO_GIT=1"             ; build from release tarball.
         "USE_BLAS64=0"         ; needed when USE_SYSTEM_BLAS=1
         "LIBBLAS=-lopenblas"
         "LIBBLASNAME=libopenblas"

         (string-append "SUITESPARSE_INC=-I "
                        (assoc-ref %build-inputs "suitesparse")
                        "/include")
         "USE_GPL_LIBS=1"       ; proudly
         (string-append "UTF8PROC_INC="
                        (assoc-ref %build-inputs "utf8proc")
                        "/include")
         "LLVM_VER=11.0.0"

         "USE_LLVM_SHLIB=1"
         (string-append "LIBUV="
                        (assoc-ref %build-inputs "libuv")
                        "/lib/libuv.so")
         (string-append "LIBUV_INC="
                        (assoc-ref %build-inputs "libuv")
                        "/include"))))
    (inputs
     `(("coreutils" ,coreutils) ; for bindings to "mkdir" and the like
       ("curl" ,curl-ssh)
       ("gfortran" ,gfortran)
       ;; required for libgcc_s.so
       ("gfortran:lib" ,gfortran "lib")
       ("gmp" ,gmp)
       ("lapack" ,lapack)
       ("libgit2" ,libgit2-1.1)
       ("libnghttp2" ,nghttp2 "lib")
       ("libssh2" ,libssh2)
       ("libunwind" ,libunwind-julia)
       ("libuv" ,libuv-julia)
       ("llvm" ,llvm-julia)
       ("mbedtls" ,mbedtls-apache)
       ("mpfr" ,mpfr)
       ("openblas" ,openblas)
       ("openlibm" ,openlibm)
       ("p7zip" ,p7zip)
       ("pcre2" ,pcre2)
       ("suitesparse" ,suitesparse)
       ("utf8proc" ,utf8proc-2.6.1)
       ("wget" ,wget)
       ("which" ,which)
       ("zlib" ,zlib)
       ;; Find dependencies versions here:
       ;; https://raw.githubusercontent.com/JuliaLang/julia/v1.6.0/deps/Versions.make
       ("dsfmt" ,dsfmt)
       ("libwhich" ,libwhich)))
    (native-inputs
     `(("openssl" ,openssl)
       ("perl" ,perl)
       ("patchelf" ,patchelf)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)))
    (native-search-paths
      (list (search-path-specification
              (variable "JULIA_LOAD_PATH")
              (files (list "share/julia/loadpath/")))
            (search-path-specification
              (variable "JULIA_DEPOT_PATH")
              (files (list "share/julia/")))))
    ;; Julia is not officially released for ARM and MIPS.
    ;; See https://github.com/JuliaLang/julia/issues/10639
    (supported-systems '("i686-linux" "x86_64-linux" "aarch64-linux"))
    (home-page "https://julialang.org/")
    (synopsis "High-performance dynamic language for technical computing")
    (description
     "Julia is a high-level, high-performance dynamic programming language for
technical computing, with syntax that is familiar to users of other technical
computing environments.  It provides a sophisticated compiler, distributed
parallel execution, numerical accuracy, and an extensive mathematical function
library.")
    (license license:expat)))
