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
  (let ((commit "52d72a52cc7ccd570929990f010ed16e2ec604c8")
        (revision "5"))
    (package (inherit libuv)
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
                  "1daxh6ci6q7znxxajr3bm16dd53ragm0d681wf4kzg542qnjq3lh"))))
      (build-system gnu-build-system)
      (arguments
       (substitute-keyword-arguments (package-arguments libuv)
         ((#:phases phases)
          `(modify-phases ,phases
             (delete 'autogen)))))
      (home-page "https://github.com/JuliaLang/libuv"))))

(define libunwind-for-julia
  (package
    (inherit libunwind)
    (version "1.1-julia2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://s3.amazonaws.com/julialang/src/"
                                  "libunwind-" version ".tar.gz"))
              (sha256
               (base32
                "0499x7sg2v18a6cry6l8y713cgmic0adnjph8i0xr1db9p7n8qyv"))))))

(define-public julia
  (package
    (name "julia")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/JuliaLang/julia/releases/download/v"
                    version "/julia-" version ".tar.gz"))
              (sha256
               (base32
                "0rd6lcc9sic10q1j3c6f9qr901i1c4554m93n2sz5b3mh37byqhw"))))
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
             (copy-file (assoc-ref inputs "unicode-data")
                        "doc/UnicodeData.txt")
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
                       ("openlibm"    "libopenlibm"    "libopenlibm.so")
                       ("openspecfun" "libopenspecfun" "libopenspecfun.so")
                       ("fftw"        "libfftw3"       "libfftw3_threads.so")
                       ("fftwf"       "libfftw3f"      "libfftw3f_threads.so"))))))
            (substitute* "base/fft/FFTW.jl"
              (("const libfftw = Base.libfftw_name")
               (string-append "const libfftw = \""
                              (assoc-ref inputs "fftw") "/lib/libfftw3_threads.so"
                              "\""))
              (("const libfftwf = Base.libfftwf_name")
               (string-append "const libfftwf = \""
                              (assoc-ref inputs "fftwf") "/lib/libfftw3f_threads.so"
                              "\"")))
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
            (substitute* "base/pcre.jl"
              (("const PCRE_LIB = \"libpcre2-8\"")
               (string-append "const PCRE_LIB = \""
                              (assoc-ref inputs "pcre2")
                              "/lib/libpcre2-8.so" "\"")))
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
              (("\\$\\(build_includedir\\)/uv-errno.h")
               (string-append (assoc-ref inputs "libuv")
                              "/include/uv-errno.h")))
            #t))
         (add-before 'build 'replace-default-shell
          (lambda _
            (substitute* "base/client.jl"
              (("/bin/sh") (which "sh")))
            #t))
         (add-after 'unpack 'hardcode-paths
           (lambda _
             (substitute* "base/interactiveutil.jl"
               (("`which") (string-append "`" (which "which")))
               (("`wget")  (string-append "`" (which "wget"))))
             #t))
         (add-before 'check 'disable-broken-tests
           (lambda _
             ;; Adjust expected error messages to match what current libgit2
             ;; provides.
             (substitute* "test/libgit2.jl"
               (("Invalid Content-Type") "invalid Content-Type")
               (("Failed to resolve path") "failed to resolve path"))

             (substitute* "test/choosetests.jl"
               ;; These tests fail, probably because some of the input
               ;; binaries have been stripped and thus backtraces don't look
               ;; as expected.
               (("\"backtrace\",") "")
               (("\"compile\",") "")
               (("\"replutil\",") "")
               (("\"cmdlineargs\",") "")
               ;; FIXME: This test fails with the following error:
               ;; Error in testset file:
               ;; Test Failed
               ;;   Expression: download("ba\0d", "good")
               ;;     Expected: ArgumentError
               ;;       Thrown: Base.UVError
               (("\"file\",") ""))
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
        "USE_LLVM_SHLIB=0" ; FIXME: fails when set to 1

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
        "USE_SYSTEM_OPENSPECFUN=1")))
    (inputs
     `(("llvm" ,llvm-3.9.1)

       ;; The bundled version is 3.3.0 so stick to that version.  With other
       ;; versions, we get test failures in 'linalg/arnoldi' as described in
       ;; <https://bugs.gnu.org/30282>.
       ("arpack-ng" ,arpack-ng-3.3.0)

       ("coreutils" ,coreutils) ;for bindings to "mkdir" and the like
       ("lapack" ,lapack)
       ("openblas" ,openblas) ;Julia does not build with Atlas
       ("libunwind" ,libunwind-for-julia)
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
       ("python" ,python-2)
       ("unicode-data"
        ,(origin
           (method url-fetch)
           (uri "http://www.unicode.org/Public/9.0.0/ucd/UnicodeData.txt")
           (sha256
            (base32
             "13zfannnr6sa6s27ggvcvzmh133ndi38pfyxsssvjmw2s8ac9pv8"))))))
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
