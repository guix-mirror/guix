;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Nicolò Balzarotti <nicolo@nixo.xyz>
;;; Copyright © 2020 Tim Howes <timhowes@lavabit.com>
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
  #:use-module (gnu packages wget)
  #:use-module (ice-9 match))

(define libuv-julia
  (let ((commit "35b1504507a7a4168caae3d78db54d1121b121e1")
        (revision "1"))
    ;; When upgrading Julia, also upgrade this.  Get the commit from
    ;; https://github.com/JuliaLang/julia/blob/v1.5.2/deps/libuv.version
    (package
      (inherit libuv)
      (name "libuv-julia")
      (version (git-version "2.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/JuliaLang/libuv")
                       (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "0dn3v6fdp1z382pqg3nhjzk60l61ky9b65mfgaj29fv2da95rwjs"))))
      (build-system gnu-build-system)
      (arguments
       (substitute-keyword-arguments (package-arguments libuv)
         ((#:phases phases)
          `(modify-phases ,phases
             (delete 'autogen)))))
      (home-page "https://github.com/JuliaLang/libuv"))))

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
        (list
         (julia-patch "libunwind-prefer-extbl"
                      "0lr4dafw8qyfh8sw8hhbwkql1dlhqv8px7k81y2l20hhxfgnh2m1")
         (julia-patch "libunwind-static-arm"
                      "1jk3bmiw61ypcchqkk1fyg5wh8wpggk574wxyfyaic870zh3lhgq")))))
    (home-page "https://github.com/JuliaLang/tree/master/deps/")))

(define (julia-patch-url version name)
  (string-append "https://raw.githubusercontent.com/JuliaLang/julia/v" version
                 "/deps/patches/" name ".patch"))

(define (julia-patch name sha)
  (let ((version "1.5.2"))
    (origin (method url-fetch)
            (uri (julia-patch-url version name))
            (sha256 (base32 sha))
            (file-name name))))

(define llvm-julia
  (package
    (inherit llvm-9)
    (name "llvm-julia")
    (source (origin
              (inherit (package-source llvm-9))
              ;; Those patches are inside the Julia source repo.
              ;; They are _not_ Julia specific (https://github.com/julialang/julia#llvm)
              ;; but they are required to build Julia.
              ;; Discussion: https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=919628
              (patches
               (map (match-lambda
                      ((name hash)
                       (julia-patch name hash)))
                    (list
                     '("llvm-D27629-AArch64-large_model_6.0.1"
                       "1qrshmlqvnasdyc158vfn3hnbigqph3lsq7acb9w8lwkpnnm2j4z")
                     '("llvm8-D34078-vectorize-fdiv"
                       "19spqc3xsazn1xs9gpcgv9ldadfkv49rmc5khl7sf1dlmhgi4602")
                     '("llvm-7.0-D44650"
                       "1h55kkmkiisfj6sk956if2bcj9s0v6n5czn8dxb870vp5nccj3ir")
                     '("llvm9-D50010-VNCoercion-ni"
                       "1s1d3sjsiq4vxg7ncy5cz56zgy5vcq6ls3iqaiqkvr23wyryqmdx")
                     '("llvm-exegesis-mingw"
                       "0ph1cj1j7arvf1xq2xcr7qf9g0cpdl14fincgr67vpi520zvd3vp")
                     '("llvm-test-plugin-mingw"
                       "12z738cnahbf6n381im7i0hxp1m6k9hrnfjlmq9sac46nxly9gnj")
                     '("llvm7-revert-D44485"
                       "0f59kq3p3mpwsbmskypbi4zn01l6ig0x7v2rjp08k2r8z8m6fa8n")
                     '("llvm-8.0-D66657-codegen-degenerate"
                       "1n1ddx19h90bbpimdyd9dh8fsm6gb93xxyqm4ljkxa1k3cx2vm72")
                     '("llvm-8.0-D71495-vectorize-freduce"
                       "1zff08wvji9lnpskk4b3p5zyjsy5hhy23ynxjqlj9dw7jvvfrf0p")
                     '("llvm-D75072-SCEV-add-type"
                       "029a3fywsm233vf48mscina24idd50dc75wr70lmimrhwnw27p0z")
                     '("llvm-9.0-D65174-limit-merge-stores"
                       "04bff1mnblfj9mxfdwr1qdnw3i3szmp60gnhxwas5y68qg33z6j0")
                     '("llvm9-D71443-PPC-MC-redef-symbol"
                       "1c93nv7rgc9jg5mqrnvv08xib1789qvlql94fwggh18mp3b9hbgy")
                     '("llvm-9.0-D78196"
                       "08a43hyg7yyqjq2vmfsmppf34xcz60wq6y9zw5fdyhw2h1mcnmns")
                     '("llvm-julia-tsan-custom-as"
                       "0awh40kf6lm4wn1nsjd1bmhfwq7rqj811szanp2xkpspykw9hg9s")
                     '("llvm-9.0-D85499"
                       "0vxlr35srvbvihlgrxq15v6dylp90vgi0qahj22j01jgqmdasjkm"))))
              (patch-flags '("-p1"))))
    (arguments
     (substitute-keyword-arguments (package-arguments llvm-9)
       ((#:configure-flags flags)
        `(list ;; Taken from NixOS. Only way I could get libLLVM-6.0.so
           "-DCMAKE_BUILD_TYPE=Release"

           ;; Build a native compiler and the NVPTX backend (NVIDIA) since
           ;; Julia insists on it, nothing more.  This reduces build times and
           ;; disk usage.
           ,(string-append "-DLLVM_TARGETS_TO_BUILD=" (system->llvm-target))
           "-DLLVM_EXPERIMENTAL_TARGETS_TO_BUILD=NVPTX"

           "-DLLVM_INSTALL_UTILS=ON"
           "-DLLVM_BUILD_TESTS=ON"
           "-DLLVM_ENABLE_FFI=ON"
           "-DLLVM_ENABLE_RTTI=ON"
           ;; "-DLLVM_HOST_TRIPLE=${stdenv.hostPlatform.config}"
           ;; "-DLLVM_DEFAULT_TARGET_TRIPLE=${stdenv.hostPlatform.config}"
           ;; "-DLLVM_EXPERIMENTAL_TARGETS_TO_BUILD=WebAssembly"
           "-DLLVM_ENABLE_DUMP=ON"
           "-DLLVM_LINK_LLVM_DYLIB=ON"))))))

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
       (list "CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'check 'set-ld-library-path
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "LD_LIBRARY_PATH"
                     (string-append (assoc-ref inputs "zlib") "/lib"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "libwhich" (string-append out "/bin")))
             #t)))))
    (native-inputs
     ;; used for tests
     `(("zlib" ,zlib)))
    (build-system gnu-build-system)
    (home-page "https://github.com/vtjnash/libwhich")
    (synopsis "Like @code{which}, for dynamic libraries")
    (description "@code{libwhich} is like @code{which}, but for dynamic
libraries.  It is also a bit like @code{ldd} and @code{otool -L}.")
    (license license:expat)))

(define-public julia
  (package
    (name "julia")
    (version "1.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/JuliaLang/julia/releases/download/v"
                    version "/julia-" version ".tar.gz"))
              (sha256
               (base32
                "08wazf3f1lb2c2c5s700kyak8llfqwki8xlnqyrbwmwxjj801p2n"))
              (patches
               (search-patches "julia-SOURCE_DATE_EPOCH-mtime.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:modules ((ice-9 match)
                  (guix build gnu-build-system)
                  (guix build utils))

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
                                       '("arpack-ng" "curl" "dsfmt"
                                         "gmp" "lapack"
                                         "libssh2" "libgit2"
                                         "mbedtls" "mpfr"
                                         "openblas" "openlibm" "pcre2"
                                         "suitesparse"))
                                  ":"))
             #t))
         ;; FIXME: Building the documentation requires Julia packages that
         ;; would be downloaded from the Internet.  We should build them in a
         ;; separate build phase.
         (add-after 'unpack 'disable-documentation
           (lambda _
             (substitute* "Makefile"
               (("(install: .*) \\$\\(BUILDROOT\\)/doc/_build/html/en/index.html" _ line)
                (string-append line "\n"))
               (("src ui doc deps")
                "src ui deps"))
             #t))
         (add-after 'unpack 'use-system-libwhich
           (lambda* (#:key inputs #:allow-other-keys)
             ;; don't build it
             (substitute* "deps/Makefile"
               (("DEP_LIBS \\+= libwhich") ""))
             ;; call our version
             (substitute* "base/Makefile"
               (("\\$\\$\\(build_depsbindir\\)/libwhich")
                (string-append (assoc-ref inputs "libwhich") "/bin/libwhich")))
             #t))
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
             (substitute* "ui/Makefile"
               (("JLDFLAGS \\+= ")
                (string-append "JLDFLAGS += "
                               (assoc-ref %build-inputs "libuv")
                               "/lib/libuv.so ")))

             (substitute* "base/Makefile"
               (("\\$\\(build_includedir\\)/uv/errno.h")
                (string-append (assoc-ref inputs "libuv")
                               "/include/uv/errno.h")))
             #t))
         (add-before 'build 'replace-default-shell
           (lambda _
             (substitute* "base/client.jl"
               (("/bin/sh") (which "sh")))
             #t))
         (add-before 'build 'fix-precompile
           (lambda _
             (substitute* "base/loading.jl"
               (("something(Base.active_project(), \"\")") "\"\""))
             #t))
         (add-before 'check 'disable-broken-tests
           (lambda _
             (substitute* "test/choosetests.jl"
               (("tests = testnames")
                ;; Those failings are not deterministic.  They depends on the
                ;; running order.  I think it depends on the number of
                ;; runners, disabling it for now
                ;; https://github.com/JuliaLang/julia/issues/34330
                "tests = filter(e->!in(e,[\"backtrace\",\"exceptions\",\"precompile\",
                                           \"client\",\"stacktraces\"]),
                                       testnames)"))
             ;; Marking the test as broken as it's a known bug:
             ;; https://github.com/JuliaLang/julia/issues/32377
             (substitute* "stdlib/REPL/test/replcompletions.jl"
               (("@test count") "@test_broken count"))
             ;; Dates has a similar bug:
             ;; https://github.com/JuliaLang/julia/issues/34655
             (substitute* "stdlib/Dates/test/io.jl"
               (("\"Dates.Date") "\"Date")
               (("\"Dates.Time") "\"Time"))
             ;; Upstream bug I found when packaging
             ;; https://github.com/JuliaLang/julia/issues/35785
             (substitute* "test/file.jl"
               (("@test dirname\\(t\\) == d") "@test_broken dirname(t) == d"))
             ;; Deprecation test fails with --depwarn=no
             ;; https://github.com/JuliaLang/julia/issues/37673
             (substitute* "test/Makefile"
               (("./runtests.jl") "--depwarn=error ./runtests.jl"))
             #t))
         (add-after 'install 'make-wrapper
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (program "julia"))
               (with-directory-excursion bin
                 (wrap-program program
                   `("JULIA_LOAD_PATH" ":" prefix
                     ("" "$JULIA_LOAD_PATH")))
                 (wrap-program program
                   `("JULIA_DEPOT_PATH" ":" prefix
                     ("" "$JULIA_DEPOT_PATH"))))
               #t))))
       #:make-flags
       (list
        (string-append "prefix=" (assoc-ref %outputs "out"))

         ;; Passing the MARCH flag is necessary to build binary substitutes for
         ;; the supported architectures.
         ,(match (or (%current-target-system)
                     (%current-system))
                 ("x86_64-linux" "MARCH=x86-64")
                 ("i686-linux" "MARCH=pentium4")
                 ("aarch64-linux" "MARCH=armv8-a")
                 ;; Prevent errors when querying this package on unsupported
                 ;; platforms, e.g. when running "guix package --search="
                 (_ "MARCH=UNSUPPORTED"))

         "CONFIG_SHELL=bash"     ;needed to build bundled libraries
         ;; list of "USE_SYSTEM_*" is here:
         ;; https://github.com/JuliaLang/julia/blob/v1.3.1/Make.inc
         "USE_SYSTEM_DSFMT=1"
         "USE_SYSTEM_P7ZIP=1"
         "USE_SYSTEM_LAPACK=1"
         "USE_SYSTEM_BLAS=1"
         "USE_BLAS64=0"          ;needed when USE_SYSTEM_BLAS=1
         "LIBBLAS=-lopenblas"
         "LIBBLASNAME=libopenblas"

         "USE_SYSTEM_SUITESPARSE=1"
         (string-append "SUITESPARSE_INC=-I "
                        (assoc-ref %build-inputs "suitesparse")
                        "/include")
         "USE_GPL_LIBS=1"        ;proudly
         "USE_SYSTEM_UTF8PROC=1"
         (string-append "UTF8PROC_INC="
                        (assoc-ref %build-inputs "utf8proc")
                        "/include")
         "USE_SYSTEM_LLVM=1"
         "LLVM_VER=9.0.1"

         "USE_LLVM_SHLIB=1"
         "USE_SYSTEM_LIBUNWIND=1"
         "USE_SYSTEM_LIBUV=1"
         (string-append "LIBUV="
                        (assoc-ref %build-inputs "libuv")
                        "/lib/libuv.so")
         (string-append "LIBUV_INC="
                        (assoc-ref %build-inputs "libuv")
                        "/include")
         "USE_SYSTEM_PATCHELF=1"
         "USE_SYSTEM_PCRE=1"
         "USE_SYSTEM_OPENLIBM=1"
         "USE_SYSTEM_MBEDTLS=1"
         "USE_SYSTEM_LIBSSH2=1"
         "USE_SYSTEM_GMP=1"
         "USE_SYSTEM_MPFR=1"
         "USE_SYSTEM_ARPACK=1"
         "USE_SYSTEM_LIBGIT2=1"
         "USE_SYSTEM_ZLIB=1")))
    (inputs
     `(("llvm" ,llvm-julia)
       ("p7zip" ,p7zip)
       ;; The bundled version is 3.3.0 so stick to that version.  With other
       ;; versions, we get test failures in 'linalg/arnoldi' as described in
       ;; <https://bugs.gnu.org/30282>.
       ("arpack-ng" ,arpack-ng-3.3.0)

       ("coreutils" ,coreutils) ;for bindings to "mkdir" and the like
       ("lapack" ,lapack)
       ("openblas" ,openblas) ;Julia does not build with Atlas
       ("libunwind" ,libunwind-julia)
       ("openlibm" ,openlibm)
       ("mbedtls" ,mbedtls-apache)
       ("curl" ,curl)
       ("libgit2" ,libgit2-0.28)
       ("libssh2" ,libssh2)
       ("fortran" ,gfortran)
       ("libuv" ,libuv-julia)
       ("pcre2" ,pcre2)
       ("utf8proc" ,utf8proc)
       ("mpfr" ,mpfr)
       ("wget" ,wget)
       ("which" ,which)
       ("zlib" ,zlib)
       ("gmp" ,gmp)
       ("suitesparse" ,suitesparse)
       ;; Find dependencies versions here:
       ;; https://raw.githubusercontent.com/JuliaLang/julia/v1.3.0/deps/Versions.make
       ("libwhich" ,libwhich)
       ("dsfmt" ,dsfmt)))
    (native-inputs
     `(("openssl" ,openssl)
       ("perl" ,perl)
       ("patchelf" ,patchelf)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)))
    (native-search-paths
      (list (search-path-specification
              (variable "JULIA_LOAD_PATH")
              (files (list "share/julia/packages/")))
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
