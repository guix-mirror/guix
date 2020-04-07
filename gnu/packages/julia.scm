;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages wget)
  #:use-module (ice-9 match))

(define libuv-julia
  (let ((commit "26dbe5672c33fc885462c509fe2a9b36f35866fd")
        (revision "6"))
    ;; When upgrading Julia, also upgrade this.
    ;; Get the commit from https://github.com/JuliaLang/julia/blob/v1.1.1/deps/libuv.version
    (package
      (inherit libuv)
      (name "libuv-julia")
      (version (string-append "1.9.0-" revision "." (string-take commit 8)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/JuliaLang/libuv.git")
                       (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "17pn2xmqaramilx897s9grs966i5246gi6sric5alch4g9j4685n"))))
      (build-system gnu-build-system)
      (arguments
       (substitute-keyword-arguments (package-arguments libuv)
         ((#:phases phases)
          `(modify-phases ,phases
             (delete 'autogen)))))
      (home-page "https://github.com/JuliaLang/libuv"))))

(define (llvm-patch-url version name)
  (string-append "https://raw.githubusercontent.com/JuliaLang/julia/v" version
                 "/deps/patches/" name))

(define (llvm-patch name sha)
  (let ((version "1.1.1"))
    (origin (method url-fetch)
            (uri (llvm-patch-url version name))
            (sha256 (base32 sha))
            (file-name name))))

(define llvm-julia
  (package
    (inherit llvm-6)
    (name "llvm-julia")
    (source (origin
              (method url-fetch)
              (uri "http://releases.llvm.org/6.0.1/llvm-6.0.1.src.tar.xz")
              (sha256
               (base32
                "1qpls3vk85lydi5b4axl0809fv932qgsqgdgrk098567z4jc7mmn"))
              ;; Those patches are inside the Julia source repo.
              ;; They are _not_ Julia specific (https://github.com/julialang/julia#llvm)
              ;; but they are required to build Julia.
              ;; Discussion: https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=919628
              (patches
                (list
                  (llvm-patch "llvm-6.0-D44650.patch"
                              "1336q4vqayr94wdcnlmcxh90mjdh34dzw9x2cbiqjnx9b1j8fxyb")
                  (llvm-patch "llvm-6.0-DISABLE_ABI_CHECKS.patch"
                              "014fawd1ba7yckalypfld22zgic87x9nx3cim42zrwygywd36pyg")
                  (llvm-patch "llvm-6.0-NVPTX-addrspaces.patch"
                              "1qdi2zmrjsrj0h84zv2vyly2hjcn4f67mfy0s1q353g4v4jkscqc")
                  (llvm-patch "llvm-6.0.0_D27296-libssp.patch"
                              "0s5hi2r1j63i8m6ig1346crx2aiv9f7rgb3mg80kw1wx5y7pdpfh")
                  (llvm-patch "llvm-D27629-AArch64-large_model_6.0.1.patch"
                              "1qrshmlqvnasdyc158vfn3hnbigqph3lsq7acb9w8lwkpnnm2j4z")
                  (llvm-patch "llvm-D34078-vectorize-fdiv.patch"
                              "1696hg84a0jxcnggvqsc2cdp271hf9a44p4qsd078qm1mfawkaay")
                  (llvm-patch "llvm-D42262-jumpthreading-not-i1.patch"
                              "1c8w210gwidbnkkw8anp17dk5pnxws2fl3mb2qxh7y9wzfpixgaq")
                  (llvm-patch "llvm-D44892-Perf-integration.patch"
                              "0r37jd0ssh2k1pndkfd5blgpg9z90im4vlzprhb0n0wwz45g4b05")
                  (llvm-patch "llvm-D46460.patch"
                              "1miqgswdc0qvbaf4571c2xkxyp9ais06b1bcpa83sq22vr4hbsfb")
                  (llvm-patch "llvm-D49832-SCEVPred.patch"
                              "0v5c88hgqj6dymv3j86ca5mhpqab5fbnrvjiw1nvnrnya9l4dlbn")
                  (llvm-patch "llvm-D50010-VNCoercion-ni.patch"
                              "0iblb3q1xixwrb12jpb89h3ywmqmzdp6aqp416j4ncwakyjhhfkp")
                  (llvm-patch "llvm-D50167-scev-umin.patch"
                              "1f2rakcnnyhr7w10k7gqg0k0491pyvx5ijplivw557f714ys3q6v")
                  (llvm-patch "llvm-OProfile-line-num.patch"
                              "1jvbbmwyags0xfwamb13qrf3rgcz9i1r03m9lava7swag8xb78c7")
                  (llvm-patch "llvm-PPC-addrspaces.patch"
                              "1f23nhsxh2s3jskbgs7da9nwg3s1hrkbk5aahl08x41wi3mny01p")
                  (llvm-patch "llvm-rL323946-LSRTy.patch"
                              "10cz3vy1yw0w643z7xx021wa4kymx9fcm3bjg61s6vzdqd6d9fns")
                  (llvm-patch "llvm-rL326967-aligned-load.patch"
                              "04jxnv32yj5x17hqhi8g2p8rhgp38gmjzr871w7z8s44pq10v9v4")
                  (llvm-patch "llvm-rL327898.patch"
                              "15ah49gbsll23z28kpyahi5vl0fh3fkxcgd1zmxxdcl96s3x8bnq")))))
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
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/JuliaLang/julia/releases/download/v"
                    version "/julia-" version ".tar.gz"))
              (sha256
               (base32
                "0hk983mywimclgnjc41zmlppm5kfdz2aj85ky07p49ilcqxi998f"))))
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
             (copy-file (assoc-ref inputs "dsfmt")
                        "deps/srccache/dsfmt-2.2.3.tar.gz")
             (copy-file (assoc-ref inputs "objconv")
                        "deps/srccache/objconv.zip")
             (copy-file (assoc-ref inputs "suitesparse")
                        "deps/srccache/SuiteSparse-4.4.5.tar.gz")
             (copy-file (string-append (assoc-ref inputs "virtualenv")
                                       "/bin/virtualenv")
                        "julia-env")
             (copy-file (assoc-ref inputs "libwhich")
                        (string-append "deps/srccache/libwhich-"
                                       "81e9723c0273d78493dc8c8ed570f68d9ce7e89e"
                                       ".tar.gz"))
             (copy-file (assoc-ref inputs "rmath")
                        "deps/srccache/Rmath-julia-0.1.tar.gz")

             ;; needed by libwhich
             (setenv "LD_LIBRARY_PATH"
                     (string-join (map (lambda (pkg)
                                         (string-append (assoc-ref inputs pkg)
                                                        "/lib"))
                                       '("arpack-ng" "fftw" "gmp" "lapack"
                                         "libgit2" "mpfr" "openblas" "openlibm"
                                         "openspecfun" "pcre2"))
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
             (use-modules (ice-9 match))
             (substitute* "src/runtime_ccall.cpp"
               ;; Patch out invocations of '/sbin/ldconfig' to avoid getting
               ;; error messages about missing '/sbin/ldconfig' on Guix System.
               (("popen\\(.*ldconfig.*\\);")
                "NULL;\n")

               ;; Populate 'sonameMap'.
               (("jl_read_sonames.*;")
                (string-join
                  (map (match-lambda
                         ((input libname soname)
                          (string-append
                            "sonameMap[\"" libname "\"] = "
                            "\"" (assoc-ref inputs input) "/lib/" soname "\";")))
                       '(("libc"        "libc"           "libc.so.6")
                         ("pcre2"       "libpcre2-8"     "libpcre2-8.so")
                         ("mpfr"        "libmpfr"        "libmpfr.so")
                         ("openblas"    "libblas"        "libopenblas.so")
                         ("arpack-ng"   "libarpack"      "libarpack.so")
                         ("lapack"      "liblapack"      "liblapack.so")
                         ("libgit2"     "libgit2"        "libgit2.so")
                         ("gmp"         "libgmp"         "libgmp.so")
                         ("openspecfun" "libopenspecfun" "libopenspecfun.so")
                         ("fftw"        "libfftw3"       "libfftw3_threads.so")
                         ("fftwf"       "libfftw3f"      "libfftw3f_threads.so"))))))
             (substitute* "base/math.jl"
               (("const libm = Base.libm_name")
                (string-append "const libm = \""
                               (assoc-ref inputs "openlibm")
                               "/lib/libopenlibm.so"
                               "\""))
               (("const openspecfun = \"libopenspecfun\"")
                (string-append "const openspecfun = \""
                               (assoc-ref inputs "openspecfun")
                               "/lib/libopenspecfun.so"
                               "\"")))
             #t))
         (add-before 'build 'fix-include-and-link-paths
           (lambda* (#:key inputs #:allow-other-keys)
             ;; LIBUTF8PROC is a linker flag, not a build target.  It is
             ;; included in the LIBFILES_* variable which is used as a
             ;; collection of build targets and a list of libraries to link
             ;; against.
             (substitute* "src/flisp/Makefile"
               (("\\$\\(BUILDDIR\\)/\\$\\(EXENAME\\): \\$\\(OBJS\\) \\$\\(LIBFILES_release\\)")
                "$(BUILDDIR)/$(EXENAME): $(OBJS) $(LLT_release)")
               (("\\$\\(BUILDDIR\\)/\\$\\(EXENAME\\)-debug: \\$\\(DOBJS\\) \\$\\(LIBFILES_debug\\)")
                "$(BUILDDIR)/$(EXENAME)-debug: $(DOBJS) $(LLT_debug)"))

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
         (add-after 'unpack 'hardcode-paths
           (lambda _
             (substitute* "stdlib/InteractiveUtils/src/InteractiveUtils.jl"
               (("`which") (string-append "`" (which "which")))
               (("`wget")  (string-append "`" (which "wget"))))
             #t))
         (add-before 'check 'disable-broken-tests
           (lambda _
             (define (touch file-name)
               (call-with-output-file file-name (const #t)))
             ;; FIXME: All git tests works except this one. But *THIS* "fix"
             ;; is not working, so right now I'm disabling all libgit2.jl tests
             ;; (substitute* "stdlib/LibGit2/test/libgit2.jl"
             ;; (("!LibGit2.use_http_path(cfg, github_cred)") "true")
             ;; (("LibGit2.use_http_path(cfg, mygit_cred)") "true"))
             (map (lambda (test)
                    (delete-file test)
                    (touch test))
                  '("stdlib/Sockets/test/runtests.jl"
                    "stdlib/Distributed/test/runtests.jl"
                    ;; FIXME: see above
                    "stdlib/LibGit2/test/libgit2.jl"))
             (substitute* "test/choosetests.jl"
               ;; These tests fail, probably because some of the input
               ;; binaries have been stripped and thus backtraces don't look
               ;; as expected.
               (("\"backtrace\",") "")
               (("\"cmdlineargs\",") ""))
             #t)))
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
         "USE_SYSTEM_DSFMT=0"    ;not packaged for Guix and upstream has no
         ;build system for a shared library.
         "USE_SYSTEM_LAPACK=1"
         "USE_SYSTEM_BLAS=1"
         "USE_BLAS64=0"          ;needed when USE_SYSTEM_BLAS=1
         "LIBBLAS=-lopenblas"
         "LIBBLASNAME=libopenblas"

         "USE_SYSTEM_FFTW=1"
         "LIBFFTWNAME=libfftw3"
         "LIBFFTWFNAME=libfftw3f"

         ;; TODO: Suitesparse does not install shared libraries, so we cannot
         ;; use the suitesparse package.
         ;; "USE_SYSTEM_SUITESPARSE=1"
         ;; (string-append "SUITESPARSE_INC=-I "
         ;;                (assoc-ref %build-inputs "suitesparse")
         ;;                "/include")

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
         "USE_SYSTEM_ZLIB=1"
         "USE_SYSTEM_OPENSPECFUN=1")))
    (inputs
     `(("llvm" ,llvm-julia)

       ;; The bundled version is 3.3.0 so stick to that version.  With other
       ;; versions, we get test failures in 'linalg/arnoldi' as described in
       ;; <https://bugs.gnu.org/30282>.
       ("arpack-ng" ,arpack-ng-3.3.0)

       ("coreutils" ,coreutils) ;for bindings to "mkdir" and the like
       ("lapack" ,lapack)
       ("openblas" ,openblas) ;Julia does not build with Atlas
       ("libunwind" ,libunwind)
       ("openlibm" ,openlibm)
       ("openspecfun" ,openspecfun)
       ("libgit2" ,libgit2)
       ("fftw" ,fftw)
       ("fftwf" ,fftwf)
       ("fortran" ,gfortran)
       ("libuv" ,libuv-julia)
       ("pcre2" ,pcre2)
       ("utf8proc" ,utf8proc)
       ("mpfr" ,mpfr)
       ("wget" ,wget)
       ("which" ,which)
       ("zlib" ,zlib)
       ("gmp" ,gmp)
       ("virtualenv" ,python2-virtualenv)
       ;; FIXME: The following inputs are downloaded from upstream to allow us
       ;; to use the lightweight Julia release tarball.  Ideally, these inputs
       ;; would eventually be replaced with proper Guix packages.

       ;; TODO: run "make -f contrib/repackage_system_suitesparse4.make" to copy static lib
       ;; Find dependency versions here:
       ;; https://raw.githubusercontent.com/JuliaLang/julia/77a2c1e245c85812dc1c7687540beedecc52758f/deps/Versions.make
       ("rmath"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                  (url "https://github.com/JuliaLang/Rmath-julia")
                  (commit "v0.1")))
           (file-name "rmath-julia-0.1-checkout")
           (sha256
            (base32
             "1zkpy0cg5zivq40zbhbdgj9128fqzs2j94wkwih8nc6xaj3gp9p6"))))
       ("suitesparse"
        ,(origin
           (method url-fetch)
           (uri "http://faculty.cse.tamu.edu/davis/SuiteSparse/SuiteSparse-4.4.5.tar.gz")
           (sha256
            (base32
             "1jcbxb8jx5wlcixzf6n5dca2rcfx6mlcms1k2rl5gp67ay3bix43"))))
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
       ("dsfmt"
        ,(origin
           (method url-fetch)
           (uri (string-append
                  "http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/"
                  "SFMT/dSFMT-src-2.2.3.tar.gz"))
           (sha256
            (base32
             "03kaqbjbi6viz0n33dk5jlf6ayxqlsq4804n7kwkndiga9s4hd42"))))))
    (native-inputs
     `(("openssl" ,openssl)
       ("perl" ,perl)
       ("patchelf" ,patchelf)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)))
    (native-search-paths
      (list (search-path-specification
              (variable "JULIA_LOAD_PATH")
              (files (list "share/julia/packages/")))))
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
