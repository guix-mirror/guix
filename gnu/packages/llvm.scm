;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2016, 2018 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Dennis Mungai <dmngaie@gmail.com>
;;; Copyright © 2016, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Roel Janssen <roel@gnu.org>
;;; Copyright © 2018, 2019, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Tim Gesthuizen <tim.gesthuizen@yahoo.de>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2019 Arm Ltd <David.Truby@arm.com>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019 Brett Gilio <brettg@gnu.org>
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
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages bootstrap)           ;glibc-dynamic-linker
  #:use-module (gnu packages compression)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xml)
  #:export (system->llvm-target))

(define* (system->llvm-target #:optional
                              (system (or (and=> (%current-target-system)
                                                 gnu-triplet->nix-system)
                                          (%current-system))))
  "Return the LLVM target name that corresponds to SYSTEM, a system type such
as \"x86_64-linux\"."
  ;; See the 'lib/Target' directory of LLVM for a list of supported targets.
  (letrec-syntax ((matches (syntax-rules (=>)
                             ((_ (system-prefix => target) rest ...)
                              (if (string-prefix? system-prefix system)
                                  target
                                  (matches rest ...)))
                             ((_)
                              (error "LLVM target for system is unknown" system)))))
    (matches ("aarch64"     => "AArch64")
             ("armhf"       => "ARM")
             ("mips64el"    => "Mips")
             ("powerpc"     => "PowerPC")
             ("riscv"       => "RISCV")
             ("x86_64"      => "X86")
             ("i686"        => "X86"))))

(define (llvm-download-uri component version)
  (if (version>=? version "9.0.1")
      (string-append "https://github.com/llvm/llvm-project/releases/download"
                     "/llvmorg-" version "/" component "-" version ".src.tar.xz")
      (string-append "https://releases.llvm.org/" version "/" component "-"
                     version ".src.tar.xz")))

(define-public llvm
  (package
    (name "llvm")
    (version "9.0.1")
    (source
     (origin
      (method url-fetch)
      (uri (llvm-download-uri "llvm" version))
      (sha256
       (base32
        "16hwp3qa54c3a3v7h8nlw0fh5criqh0hlr1skybyk0cz70gyx880"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("python" ,python-2) ;bytes->str conversion in clang>=3.7 needs python-2
       ("perl"   ,perl)))
    (inputs
     `(("libffi" ,libffi)))
    (propagated-inputs
     `(("zlib" ,zlib)))                 ;to use output from llvm-config
    (arguments
     `(#:configure-flags '("-DCMAKE_SKIP_BUILD_RPATH=FALSE"
                           "-DCMAKE_BUILD_WITH_INSTALL_RPATH=FALSE"
                           "-DBUILD_SHARED_LIBS:BOOL=TRUE"
                           "-DLLVM_ENABLE_FFI:BOOL=TRUE"
                           "-DLLVM_REQUIRES_RTTI=1" ; For some third-party utilities
                           "-DLLVM_INSTALL_UTILS=ON") ; Needed for rustc.

       ;; Don't use '-g' during the build, to save space.
       #:build-type "Release"
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'shared-lib-workaround
                    ;; Even with CMAKE_SKIP_BUILD_RPATH=FALSE, llvm-tblgen
                    ;; doesn't seem to get the correct rpath to be able to run
                    ;; from the build directory.  Set LD_LIBRARY_PATH as a
                    ;; workaround.
                    (lambda _
                      (setenv "LD_LIBRARY_PATH"
                              (string-append (getcwd) "/lib"))
                      #t)))))
    (home-page "https://www.llvm.org")
    (synopsis "Optimizing compiler infrastructure")
    (description
     "LLVM is a compiler infrastructure designed for compile-time, link-time,
runtime, and idle-time optimization of programs from arbitrary programming
languages.  It currently supports compilation of C and C++ programs, using
front-ends derived from GCC 4.0.1.  A new front-end for the C family of
languages is in development.  The compiler infrastructure includes mirror sets
of programming tools as well as libraries with equivalent functionality.")
    (license license:asl2.0)))  ;with LLVM exceptions, see LICENSE.txt

(define* (clang-runtime-from-llvm llvm hash
                                  #:optional (patches '()))
  (package
    (name "clang-runtime")
    (version (package-version llvm))
    (source
     (origin
       (method url-fetch)
       (uri (llvm-download-uri "compiler-rt" version))
       (sha256 (base32 hash))
       (patches (map search-patch patches))))
    (build-system cmake-build-system)
    (native-inputs (package-native-inputs llvm))
    (inputs
     `(("llvm" ,llvm)))
    (arguments
     `(;; Don't use '-g' during the build to save space.
       #:build-type "Release"
       #:tests? #f))                    ; Tests require gtest
    (home-page "https://compiler-rt.llvm.org")
    (synopsis "Runtime library for Clang/LLVM")
    (description
     "The \"clang-runtime\" library provides the implementations of run-time
functions for C and C++ programs.  It also provides header files that allow C
and C++ source code to interface with the \"sanitization\" passes of the clang
compiler.  In LLVM this library is called \"compiler-rt\".")
    (license (package-license llvm))

    ;; <https://compiler-rt.llvm.org/> doesn't list MIPS as supported.
    (supported-systems (delete "mips64el-linux" %supported-systems))))

(define* (clang-from-llvm llvm clang-runtime hash
                          #:key (patches '()))
  (package
    (name "clang")
    (version (package-version llvm))
    (source
     (origin
       (method url-fetch)
       (uri (llvm-download-uri (if (version>=? version "9.0.1")
                                   "clang"
                                   "cfe")
                               version))
       (sha256 (base32 hash))
       (patches (map search-patch patches))))
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
                           (compiler-rt (assoc-ref inputs "clang-runtime"))
                           (gcc (assoc-ref inputs "gcc"))
                           (version
                            (string->number
                             ,(version-major (package-version clang-runtime)))))
                       (cond
                         ((> version 3)
                          ;; Link to libclang_rt files from clang-runtime.
                          (substitute* "lib/Driver/ToolChain.cpp"
                            (("getDriver\\(\\)\\.ResourceDir")
                             (string-append "\"" compiler-rt "\"")))

                          ;; Make "LibDir" refer to <glibc>/lib so that it
                          ;; uses the right dynamic linker file name.
                          (substitute* "lib/Driver/ToolChains/Linux.cpp"
                            (("(^[[:blank:]]+LibDir = ).*" _ declaration)
                             (string-append declaration "\"" libc "/lib\";\n"))

                            ;; Make clang look for libstdc++ in the right
                            ;; location.
                            (("LibStdCXXIncludePathCandidates\\[\\] = \\{")
                             (string-append
                              "LibStdCXXIncludePathCandidates[] = { \"" gcc "/include/c++\","))

                            ;; Make sure libc's libdir is on the search path, to
                            ;; allow crt1.o & co. to be found.
                            (("@GLIBC_LIBDIR@")
                             (string-append libc "/lib"))))
                         (else
                          (substitute* "lib/Driver/Tools.cpp"
                            ;; Patch the 'getLinuxDynamicLinker' function so that
                            ;; it uses the right dynamic linker file name.
                            (("/lib64/ld-linux-x86-64.so.2")
                             (string-append libc
                                            ,(glibc-dynamic-linker))))

                          ;; Link to libclang_rt files from clang-runtime.
                          ;; This substitution needed slight adjustment in 3.8.
                          (if (< 3.8 (string->number ,(version-major+minor
                                                       (package-version
                                                        clang-runtime))))
                              (substitute* "lib/Driver/Tools.cpp"
                                (("TC\\.getDriver\\(\\)\\.ResourceDir")
                                 (string-append "\"" compiler-rt "\"")))
                              (substitute* "lib/Driver/ToolChain.cpp"
                                (("getDriver\\(\\)\\.ResourceDir")
                                 (string-append "\"" compiler-rt "\""))))

                          ;; Make sure libc's libdir is on the search path, to
                          ;; allow crt1.o & co. to be found.
                          (substitute* "lib/Driver/ToolChains.cpp"
                            (("@GLIBC_LIBDIR@")
                             (string-append libc "/lib")))))
                       #t)))
                  (add-after 'install 'install-clean-up-/share/clang
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (compl-dir (string-append
                                         out "/etc/bash_completion.d")))
                        (with-directory-excursion (string-append out
                                                                 "/share/clang")
                          (for-each
                            (lambda (file)
                              (when (file-exists? file)
                                (delete-file file)))
                            ;; Delete extensions for proprietary text editors.
                            '("clang-format-bbedit.applescript"
                              "clang-format-sublime.py"
                              ;; Delete Emacs extensions: see their respective Emacs
                              ;; Guix package instead.
                              "clang-rename.el" "clang-format.el"))
                          ;; Install bash completion.
                          (when (file-exists?  "bash-autocomplete.sh")
                            (mkdir-p compl-dir)
                            (rename-file "bash-autocomplete.sh"
                                         (string-append compl-dir "/clang")))))
                      #t)))))

    ;; Clang supports the same environment variables as GCC.
    (native-search-paths
     (list (search-path-specification
            (variable "CPATH")
            (files '("include")))
           (search-path-specification
            (variable "LIBRARY_PATH")
            (files '("lib" "lib64")))))

    (home-page "https://clang.llvm.org")
    (synopsis "C language family frontend for LLVM")
    (description
     "Clang is a compiler front end for the C, C++, Objective-C and
Objective-C++ programming languages.  It uses LLVM as its back end.  The Clang
project includes the Clang front end, the Clang static analyzer, and several
code analysis tools.")
    (license (if (version>=? version "9.0")
                 license:asl2.0         ;with LLVM exceptions
                 license:ncsa))))

(define (make-clang-toolchain clang)
  (package
    (name (string-append (package-name clang) "-toolchain"))
    (version (package-version clang))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build union))
       #:builder (begin
                   (use-modules (ice-9 match)
                                (srfi srfi-26)
                                (guix build union))

                   (let ((out (assoc-ref %outputs "out")))

                     (match %build-inputs
                       (((names . directories) ...)
                        (union-build out directories)))

                     (union-build (assoc-ref %outputs "debug")
                                  (list (assoc-ref %build-inputs
                                                   "libc-debug")))
                     (union-build (assoc-ref %outputs "static")
                                  (list (assoc-ref %build-inputs
                                                   "libc-static")))
                     #t))))

    (native-search-paths (package-native-search-paths clang))
    (search-paths (package-search-paths clang))

    (license (package-license clang))
    (home-page "https://clang.llvm.org")
    (synopsis "Complete Clang toolchain for C/C++ development")
    (description "This package provides a complete Clang toolchain for C/C++
development to be installed in user profiles.  This includes Clang, as well as
libc (headers and binaries, plus debugging symbols in the @code{debug}
output), and Binutils.")
    (outputs '("out" "debug" "static"))
    (inputs `(("clang" ,clang)
              ("ld-wrapper" ,(car (assoc-ref (%final-inputs) "ld-wrapper")))
              ("binutils" ,binutils)
              ("libc" ,glibc)
              ("libc-debug" ,glibc "debug")
              ("libc-static" ,glibc "static")))))

(define-public clang-runtime
  (clang-runtime-from-llvm
   llvm
   "0xwh79g3zggdabxgnd0bphry75asm1qz7mv3hcqihqwqr6aspgy2"))

(define-public clang
  (clang-from-llvm llvm clang-runtime
                   "0ls2h3iv4finqyflyhry21qhc9cm9ga7g1zq21020p065qmm2y2p"
                   #:patches '("clang-9.0-libc-search-path.patch")))

(define-public clang-toolchain
  (make-clang-toolchain clang))

(define-public llvm-9 llvm)
(define-public clang-runtime-9 clang-runtime)
(define-public clang-9 clang)
(define-public clang-toolchain-9 clang-toolchain)

(define-public llvm-8
  (package
    (inherit llvm)
    (version "8.0.0")
    (source (origin
              (method url-fetch)
              (uri (llvm-download-uri "llvm" version))
              (sha256
               (base32
                "0k124sxkfhfi1rca6kzkdraf4axhx99x3cw2rk55056628dvwwl8"))))
    (license license:ncsa)))

(define-public clang-runtime-8
  (clang-runtime-from-llvm
   llvm-8
   "1c919wsm17xnv7lr8bhpq2wkq8113lzlw6hzhfr737j59x3wfddl"))

(define-public clang-8
  (clang-from-llvm llvm-8 clang-runtime-8
                   "0svk1f70hvpwrjp6x5i9kqwrqwxnmcrw5s7f4cxyd100mdd12k08"
                   #:patches '("clang-7.0-libc-search-path.patch")))

(define-public clang-toolchain-8
  (make-clang-toolchain clang-8))

(define-public llvm-7
  (package
    (inherit llvm-8)
    (version "7.0.1")
    (source (origin
              (method url-fetch)
              (uri (llvm-download-uri "llvm" version))
              (sha256
               (base32
                "16s196wqzdw4pmri15hadzqgdi926zln3an2viwyq0kini6zr3d3"))))))

(define-public clang-runtime-7
  (clang-runtime-from-llvm
   llvm-7
   "065ybd8fsc4h2hikbdyricj6pyv4r7r7kpcikhb2y5zf370xybkq"))

(define-public clang-7
  (clang-from-llvm llvm-7 clang-runtime-7
                   "067lwggnbg0w1dfrps790r5l6k8n5zwhlsw7zb6zvmfpwpfn4nx4"
                   #:patches '("clang-7.0-libc-search-path.patch")))

(define-public clang-toolchain-7
  (make-clang-toolchain clang-7))

(define-public llvm-6
  (package
    (inherit llvm-7)
    (version "6.0.1")
    (source (origin
              (method url-fetch)
              (uri (llvm-download-uri "llvm" version))
              (sha256
               (base32
                "1qpls3vk85lydi5b4axl0809fv932qgsqgdgrk098567z4jc7mmn"))))))

(define-public clang-runtime-6
  (clang-runtime-from-llvm
   llvm-6
   "1fcr3jn24yr8lh36nc0c4ikli4744i2q9m1ik67p1jymwwaixkgl"))

(define-public clang-6
  (clang-from-llvm llvm-6 clang-runtime-6
                   "0rxn4rh7rrnsqbdgp4gzc8ishbkryhpl1kd3mpnxzpxxhla3y93w"
                   #:patches '("clang-6.0-libc-search-path.patch")))

(define-public clang-toolchain-6
  (make-clang-toolchain clang-6))

(define-public llvm-3.9.1
  (package (inherit llvm-6)
    (name "llvm")
    (version "3.9.1")
    (source
     (origin
      (method url-fetch)
      (uri (llvm-download-uri "llvm" version))
      (sha256
       (base32
        "1vi9sf7rx1q04wj479rsvxayb6z740iaz3qniwp266fgp5a07n8z"))))))

(define-public clang-runtime-3.9.1
  (clang-runtime-from-llvm
   llvm-3.9.1
   "16gc2gdmp5c800qvydrdhsp0bzb97s8wrakl6i8a4lgslnqnf2fk"
   '("clang-runtime-asan-build-fixes.patch"
     "clang-runtime-esan-build-fixes.patch"
     "clang-3.5-libsanitizer-ustat-fix.patch")))

(define-public clang-3.9.1
  (clang-from-llvm llvm-3.9.1 clang-runtime-3.9.1
                   "0qsyyb40iwifhhlx9a3drf8z6ni6zwyk3bvh0kx2gs6yjsxwxi76"
                   #:patches '("clang-3.8-libc-search-path.patch")))

(define-public llvm-3.8
  (package (inherit llvm-3.9.1)
    (name "llvm")
    (version "3.8.1")
    (source
     (origin
      (method url-fetch)
      (uri (llvm-download-uri "llvm" version))
      (sha256
       (base32
        "1ybmnid4pw2hxn12ax5qa5kl1ldfns0njg8533y3mzslvd5cx0kf"))))))

(define-public clang-runtime-3.8
  (clang-runtime-from-llvm
   llvm-3.8
   "0p0y85c7izndbpg2l816z7z7558axq11d5pwkm4h11sdw7d13w0d"
   '("clang-runtime-asan-build-fixes.patch"
     "clang-3.5-libsanitizer-ustat-fix.patch")))

(define-public clang-3.8
  (clang-from-llvm llvm-3.8 clang-runtime-3.8
                   "1prc72xmkgx8wrzmrr337776676nhsp1qd3mw2bvb22bzdnq7lsc"
                   #:patches '("clang-3.8-libc-search-path.patch")))

(define-public llvm-3.7
  (package (inherit llvm-3.8)
    (version "3.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (llvm-download-uri "llvm" version))
       (sha256
        (base32
         "1masakdp9g2dan1yrazg7md5am2vacbkb3nahb3dchpc1knr8xxy"))))))

(define-public clang-runtime-3.7
  (clang-runtime-from-llvm
   llvm-3.7
   "10c1mz2q4bdq9bqfgr3dirc6hz1h3sq8573srd5q5lr7m7j6jiwx"
   '("clang-runtime-asan-build-fixes.patch"
     "clang-3.5-libsanitizer-ustat-fix.patch")))

(define-public clang-3.7
  (clang-from-llvm llvm-3.7 clang-runtime-3.7
                   "0x065d0w9b51xvdjxwfzjxng0gzpbx45fgiaxpap45ragi61dqjn"
                   #:patches '("clang-3.5-libc-search-path.patch")))

(define-public llvm-3.6
  (package (inherit llvm-3.7)
    (version "3.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (llvm-download-uri "llvm" version))
       (sha256
        (base32
         "153vcvj8gvgwakzr4j0kndc0b7wn91c2g1vy2vg24s6spxcc23gn"))))))

(define-public clang-runtime-3.6
  (clang-runtime-from-llvm
   llvm-3.6
   "11qx8d3pbfqjaj2x207pvlvzihbs1z2xbw4crpz7aid6h1yz6bqg"
   '("clang-runtime-asan-build-fixes.patch")))

(define-public clang-3.6
  (clang-from-llvm llvm-3.6 clang-runtime-3.6
                   "1wwr8s6lzr324hv4s1k6na4j5zv6n9kdhi14s4kb9b13d93814df"
                   #:patches '("clang-3.5-libc-search-path.patch")))

(define-public llvm-3.5
  (package (inherit llvm-3.6)
    (version "3.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (llvm-download-uri "llvm" version))
       (patches
        (search-patches "llvm-3.5-fix-clang-build-with-gcc5.patch"))
       (sha256
        (base32
         "0xf5q17kkxsrm2gsi93h4pwlv663kji73r2g4asb97klsmb626a4"))))))

(define-public clang-runtime-3.5
  (clang-runtime-from-llvm
   llvm-3.5
   "1hsdnzzdr5kglz6fnv3lcsjs222zjsy14y8ax9dy6zqysanplbal"
   '("clang-runtime-asan-build-fixes.patch"
     "clang-3.5-libsanitizer-ustat-fix.patch")))

(define-public clang-3.5
  (clang-from-llvm llvm-3.5 clang-runtime-3.5
                   "0846h8vn3zlc00jkmvrmy88gc6ql6014c02l4jv78fpvfigmgssg"
                   #:patches '("clang-3.5-libc-search-path.patch")))

(define-public llvm-for-extempore
  (package (inherit llvm-3.7)
    (name "llvm-for-extempore")
    (source
     (origin
       (inherit (package-source llvm-3.7))
       (patches (list (search-patch "llvm-for-extempore.patch")))))
    ;; Extempore refuses to build on architectures other than x86_64
    (supported-systems '("x86_64-linux"))))

(define-public libcxx
  (package
    (name "libcxx")
    (version "9.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (llvm-download-uri "libcxx" version))
       (sha256
        (base32
         "0d2bj5i6mk4caq7skd5nsdmz8c2m5w5anximl5wz3x32p08zz089"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("clang" ,clang)
       ("llvm" ,llvm)))
    (home-page "https://libcxx.llvm.org")
    (synopsis "C++ standard library")
    (description
     "This package provides an implementation of the C++ standard library for
use with Clang, targeting C++11, C++14 and above.")
    (license license:expat)))

;; Libcxx files specifically used by PySide2.
(define-public libcxx-6
  (package
    (inherit libcxx)
    (version (package-version llvm-6))
    (source
     (origin
       (inherit (package-source libcxx))
       (uri (llvm-download-uri "libcxx" version))
       (sha256
        (base32
         "0rzw4qvxp6qx4l4h9amrq02gp7hbg8lw4m0sy3k60f50234gnm3n"))))
    (native-inputs
     `(("clang" ,clang-6)
       ("llvm" ,llvm-6)))))

(define-public libclc
  (package
    (name "libclc")
    (version "9.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/llvm/llvm-project.git")
             (commit (string-append "llvmorg-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1d1qayvrvvc1di7s7jfxnjvxq2az4lwq1sw1b2gq2ic0nksvajz0"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "-DLLVM_CLANG="
                            (assoc-ref %build-inputs "clang")
                            "/bin/clang")
             (string-append "-DPYTHON="
                            (assoc-ref %build-inputs "python")
                            "/bin/python3"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "libclc") #t)))))
    (native-inputs
     `(("clang" ,clang)
       ("llvm" ,llvm)
       ("python" ,python)))
    (home-page "https://libclc.llvm.org")
    (synopsis "Libraries for the OpenCL programming language")
    (description
     "This package provides an implementation of the OpenCL library
requirements according to version 1.1 of the OpenCL specification.")
    ;; Apache license 2.0 with LLVM exception
    (license license:asl2.0)))

(define-public libomp
  (package
    (name "libomp")
    (version "9.0.1")
    (source (origin
              (method url-fetch)
              (uri (llvm-download-uri "openmp" version))
              (sha256
               (base32
                "1knafnpp0f7hylx8q20lkd6g1sf0flly572dayc5d5kghh7hd52w"))
              (file-name (string-append "libomp-" version ".tar.xz"))))
    (build-system cmake-build-system)
    ;; XXX: Note this gets built with GCC because building with Clang itself
    ;; fails (missing <atomic>, even when libcxx is added as an input.)
    (arguments
     '(#:configure-flags '("-DLIBOMP_USE_HWLOC=ON"
                           "-DOPENMP_TEST_C_COMPILER=clang"
                           "-DOPENMP_TEST_CXX_COMPILER=clang++")
       #:test-target "check-libomp"))
    (native-inputs
     `(("clang" ,clang)
       ("llvm" ,llvm)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("hwloc" ,hwloc "lib")))
    (home-page "https://openmp.llvm.org")
    (synopsis "OpenMP run-time support library")
    (description
     "This package provides the run-time support library developed by the LLVM
project for the OpenMP multi-theaded programming extension.  This package
notably provides @file{libgomp.so}, which is has a binary interface compatible
with that of libgomp, the GNU Offloading and Multi Processing Library.")
    (license license:expat)))

(define-public python-llvmlite
  (package
    (name "python-llvmlite")
    (version "0.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "llvmlite" version))
       (sha256
        (base32
         "01wspdc0xhnydl66jyhyr4ii16h3fnw6mjihiwnnxdxg9j6kkajf"))))
    (build-system python-build-system)
    (arguments
     ;; FIXME: One test fails unable to find libm.so
     ;; https://github.com/numba/llvmlite/issues/537
     `(#:tests? #f))
    (inputs
     `(("llvm"
        ,(package
           (inherit llvm-7)
           (source (origin
                     (inherit (package-source llvm-7))
                     (patches
                      (list
                       (origin
                         (method url-fetch)
                         (uri (string-append "https://raw.githubusercontent.com/numba/"
                                             "llvmlite/v" version "/conda-recipes/"
                                             "D47188-svml-VF.patch"))
                         (sha256
                          (base32
                           "0wxhgb61k17f0zg2m0726sf3hppm41f8jar2kkg2n8sl5cnjj9mr")))
                       (origin
                         (method url-fetch)
                         (uri (string-append "https://raw.githubusercontent.com/numba/"
                                             "llvmlite/v" version "/conda-recipes/"
                                             "twine_cfg_undefined_behavior.patch"))
                         (sha256
                          (base32
                           "07h71n2m1mn9zcfgw04zglffknplb233zqbcd6pckq0wygkrxflp")))))))))))
    (home-page "http://llvmlite.pydata.org")
    (synopsis "Wrapper around basic LLVM functionality")
    (description
     "This package provides a Python binding to LLVM for use in Numba.")
    (license license:bsd-3)))

(define (package-elisp-from-package source-package package-name
                                    source-files)
  "Return a package definition named PACKAGE-NAME that packages the Emacs Lisp
SOURCE-FILES found in SOURCE-PACKAGE."
  (let ((orig (package-source source-package)))
    (package
      (inherit source-package)
      (name package-name)
      (build-system emacs-build-system)
      (source (origin
                (method (origin-method orig))
                (uri (origin-uri orig))
                (sha256 (origin-sha256 orig))
                (file-name (string-append package-name "-"
                                          (package-version source-package)))
                (modules '((guix build utils)
                           (srfi srfi-1)
                           (ice-9 ftw)))
                (snippet
                 `(let* ((source-files (quote ,source-files))
                         (basenames (map basename source-files)))
                    (map copy-file
                         source-files basenames)
                    (map delete-file-recursively
                         (fold delete
                               (scandir ".")
                               (append '("." "..") basenames)))
                    #t)))))))

(define-public emacs-clang-format
  (package
    (inherit clang)
    (name "emacs-clang-format")
    (build-system emacs-build-system)
    (inputs
     `(("clang" ,clang)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'configure
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((clang (assoc-ref inputs "clang")))
               (copy-file "tools/clang-format/clang-format.el" "clang-format.el")
               (emacs-substitute-variables "clang-format.el"
                 ("clang-format-executable"
                  (string-append clang "/bin/clang-format"))))
             #t)))))
    (synopsis "Format code using clang-format")
    (description "This package allows to filter code through @code{clang-format}
to fix its formatting.  @code{clang-format} is a tool that formats
C/C++/Obj-C code according to a set of style options, see
@url{https://clang.llvm.org/docs/ClangFormatStyleOptions.html}.")))

(define-public emacs-clang-rename
  (package
    (inherit clang)
    (name "emacs-clang-rename")
    (build-system emacs-build-system)
    (inputs
     `(("clang" ,clang)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'configure
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((clang (assoc-ref inputs "clang")))
               (copy-file "tools/clang-rename/clang-rename.el" "clang-rename.el")
               (emacs-substitute-variables "clang-rename.el"
                 ("clang-rename-binary"
                  (string-append clang "/bin/clang-rename"))))
             #t)))))
    (synopsis "Rename every occurrence of a symbol using clang-rename")
    (description "This package renames every occurrence of a symbol at point
using @code{clang-rename}.")))
