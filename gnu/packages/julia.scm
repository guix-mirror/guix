;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2020 Efraim Flashner <efraim@flashner.co.il>
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
    ;; https://github.com/JuliaLang/julia/blob/v1.3.1/deps/libuv.version
    (package
      (inherit libuv)
      (name "libuv-julia")
      (version (git-version "2.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/JuliaLang/libuv.git")
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
                 "/deps/patches/" name))

(define (julia-patch name sha)
  (let ((version "1.3.1"))
    (origin (method url-fetch)
            (uri (julia-patch-url version name))
            (sha256 (base32 sha))
            (file-name name))))

(define llvm-julia
  (package
    (inherit llvm-6)
    (name "llvm-julia")
    (source (origin
              (inherit (package-source llvm-6))
              ;; Those patches are inside the Julia source repo.
              ;; They are _not_ Julia specific (https://github.com/julialang/julia#llvm)
              ;; but they are required to build Julia.
              ;; Discussion: https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=919628
              (patches
                (map (match-lambda
                       ((name hash)
                        (julia-patch name hash)))
                     (list
                       '("llvm-6.0-D44650"
                         "1336q4vqayr94wdcnlmcxh90mjdh34dzw9x2cbiqjnx9b1j8fxyb")
                       '("llvm-6.0-DISABLE_ABI_CHECKS"
                         "014fawd1ba7yckalypfld22zgic87x9nx3cim42zrwygywd36pyg")
                       '("llvm-6.0-NVPTX-addrspaces"
                         "1qdi2zmrjsrj0h84zv2vyly2hjcn4f67mfy0s1q353g4v4jkscqc")
                       '("llvm-6.0.0_D27296-libssp"
                         "0s5hi2r1j63i8m6ig1346crx2aiv9f7rgb3mg80kw1wx5y7pdpfh")
                       '("llvm-D27629-AArch64-large_model_6.0.1"
                         "1qrshmlqvnasdyc158vfn3hnbigqph3lsq7acb9w8lwkpnnm2j4z")
                       '("llvm-D34078-vectorize-fdiv"
                         "1696hg84a0jxcnggvqsc2cdp271hf9a44p4qsd078qm1mfawkaay")
                       '("llvm-D42262-jumpthreading-not-i1"
                         "1c8w210gwidbnkkw8anp17dk5pnxws2fl3mb2qxh7y9wzfpixgaq")
                       '("llvm-D44892-Perf-integration"
                         "0r37jd0ssh2k1pndkfd5blgpg9z90im4vlzprhb0n0wwz45g4b05")
                       '("llvm-D46460"
                         "1miqgswdc0qvbaf4571c2xkxyp9ais06b1bcpa83sq22vr4hbsfb")
                       '("llvm-D49832-SCEVPred"
                         "0v5c88hgqj6dymv3j86ca5mhpqab5fbnrvjiw1nvnrnya9l4dlbn")
                       '("llvm-D50010-VNCoercion-ni"
                         "0iblb3q1xixwrb12jpb89h3ywmqmzdp6aqp416j4ncwakyjhhfkp")
                       '("llvm-D50167-scev-umin"
                         "1f2rakcnnyhr7w10k7gqg0k0491pyvx5ijplivw557f714ys3q6v")
                       '("llvm-OProfile-line-num"
                         "1jvbbmwyags0xfwamb13qrf3rgcz9i1r03m9lava7swag8xb78c7")
                       '("llvm-PPC-addrspaces"
                         "1f23nhsxh2s3jskbgs7da9nwg3s1hrkbk5aahl08x41wi3mny01p")
                       '("llvm-rL323946-LSRTy"
                         "10cz3vy1yw0w643z7xx021wa4kymx9fcm3bjg61s6vzdqd6d9fns")
                       '("llvm-rL326967-aligned-load"
                         "04jxnv32yj5x17hqhi8g2p8rhgp38gmjzr871w7z8s44pq10v9v4")
                       '("llvm-rL327898"
                         "15ah49gbsll23z28kpyahi5vl0fh3fkxcgd1zmxxdcl96s3x8bnq"))))))
    (arguments
     (substitute-keyword-arguments (package-arguments llvm-6)
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

(define-public julia
  (package
    (name "julia")
    (version "1.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/JuliaLang/julia/releases/download/v"
                    version "/julia-" version ".tar.gz"))
              (sha256
               (base32
                "1nwkmr9j55g1zkxdchnid1h022s0is52vx23niksshgvh793g41x"))
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
             (mkdir "deps/srccache")
             ;; no USE_SYSTEM_{OBJCONV/LIBWHICH} option
             (copy-file (assoc-ref inputs "objconv")
                        "deps/srccache/objconv.zip")
             (copy-file (assoc-ref inputs "libwhich")
                        (string-append "deps/srccache/libwhich-"
                                       "81e9723c0273d78493dc8c8ed570f68d9ce7e89e"
                                       ".tar.gz"))

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
         (add-before 'check 'set-home
           ;; Some tests require a home directory to be set.
           (lambda _ (setenv "HOME" "/tmp") #t))
         (add-after 'unpack 'hardcode-soname-map
           ;; ./src/runtime_ccall.cpp creates a map from library names to paths
           ;; using the output of "/sbin/ldconfig -p".  Since ldconfig is not
           ;; used in Guix, we patch runtime_ccall.cpp to contain a static map.
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "base/math.jl"
               (("const libm = Base.libm_name")
                (string-append "const libm = \""
                               (assoc-ref inputs "openlibm")
                               "/lib/libopenlibm.so"
                               "\"")))
             #t))
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
                "tests = filter(e->!in(e,[\"backtrace\",\"exceptions\",
                                          \"stress\",\"precompile\",
                                          \"client\",\"stacktraces\"]),
                                       testnames)"))
             ;; When HOME is not set, julia calls uv_os_homedir, which in
             ;; turns call getpwuid_r. Add the HOME env variable to the
             ;; external julia call to fix this
             (substitute* "test/cmdlineargs.jl"
               (("\"JULIA_PROJECT\"") "\"HOME\"=>\"/tmp\", \"JULIA_PROJECT\""))
             ;; Marking the test as broken as it's a known bug:
             ;; https://github.com/JuliaLang/julia/issues/32377
             (substitute* "stdlib/REPL/test/replcompletions.jl"
               (("@test count") "@test_broken count"))
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
         (string-append "PREFIX=" (assoc-ref %outputs "out"))

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
         "LLVM_VER=6.0.1"

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

         "USE_SYSTEM_GMP=1"
         "USE_SYSTEM_MPFR=1"
         "USE_SYSTEM_ARPACK=1"
         "USE_SYSTEM_LIBGIT2=1"
         (string-append "LIBUV="
                        (assoc-ref %build-inputs "libuv")
                        "/lib/libuv.so")
         (string-append "LIBUV_INC="
                        (assoc-ref %build-inputs "libuv")
                        "/include")
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
       ("libgit2" ,libgit2)
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
       ("objconv"
        ,(origin
           (method url-fetch)
           ;; No versioned URL, see <https://www.agner.org/optimize/> for updates.
           (uri "https://www.agner.org/optimize/objconv.zip")
           (file-name "objconv-2018-10-07.zip")
           (sha256
            (base32
             "0wp6ld9vk11f4nnkn56627zmlv9k5vafi99qa3yyn1pgcd61zcfs"))))
       ("libwhich"
        ,(let ((commit "81e9723c0273d78493dc8c8ed570f68d9ce7e89e"))
           (origin
             ;; Note: We use a /tarball URL, but that's because Julia's build
             ;; system checks the hash of that tarball; thus we can't use
             ;; 'git-fetch'.
             (method url-fetch)
             (uri (string-append
                    "https://api.github.com/repos/vtjnash/libwhich/tarball/"
                    commit))
             (file-name (string-append "libwhich-" (string-take commit 7)
                                       ".tar.gz"))
             (sha256
              (base32
               "1p7zg31kpmpbmh1znrk1xrbd074agx13b9q4dcw8n2zrwwdlbz3b")))))
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
