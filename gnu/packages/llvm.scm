;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2016, 2018 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2017, 2018, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Dennis Mungai <dmngaie@gmail.com>
;;; Copyright © 2016, 2018, 2019, 2020, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Roel Janssen <roel@gnu.org>
;;; Copyright © 2018–2022 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Tim Gesthuizen <tim.gesthuizen@yahoo.de>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2019 Arm Ltd <David.Truby@arm.com>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2020, 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2021 Lars-Dominik Braun <lars@6xq.net>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
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

(define-module (gnu packages llvm)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix memoization)
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
  #:use-module (gnu packages check)               ;python-lit
  #:use-module (gnu packages compression)
  #:use-module (gnu packages julia)               ;julia-patch
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages xml)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (make-lld-wrapper
            system->llvm-target))

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
             ("i686"        => "X86")
             ("i586"        => "X86"))))

(define (llvm-uri component version)
  (string-append "https://github.com/llvm/llvm-project/releases/download"
                 "/llvmorg-" version "/" component "-" version ".src.tar.xz"))

(define* (clang-runtime-from-llvm llvm hash
                                  #:optional (patches '()))
  (package
    (name "clang-runtime")
    (version (package-version llvm))
    (source
     (origin
       (method url-fetch)
       (uri (llvm-uri "compiler-rt" version))
       (sha256 (base32 hash))
       (patches (map search-patch patches))))
    (build-system cmake-build-system)
    (native-inputs (package-native-inputs llvm))
    (inputs
     (list llvm))
    (arguments
     `(;; Don't use '-g' during the build to save space.
       #:build-type "Release"
       #:tests? #f                      ; Tests require gtest
       #:modules ((srfi srfi-1)
                  (ice-9 match)
                  ,@%cmake-build-system-modules)
       #:phases (modify-phases (@ (guix build cmake-build-system) %standard-phases)
                  (add-after 'set-paths 'hide-glibc
                    ;; Work around https://issues.guix.info/issue/36882.  We need to
                    ;; remove glibc from CPLUS_INCLUDE_PATH so that the one hardcoded
                    ;; in GCC, at the bottom of GCC include search-path is used.
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let* ((filters '("libc"))
                             (input-directories
                              (filter-map (lambda (input)
                                            (match input
                                              ((name . dir)
                                               (and (not (member name filters))
                                                    dir))))
                                          inputs)))
                        (set-path-environment-variable "CPLUS_INCLUDE_PATH"
                                                       '("include")
                                                       input-directories)
                        #t))))))
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
                          #:key (patches '()) tools-extra
                          (properties
                           (clang-properties (package-version llvm))))
  "Produce Clang with dependencies on LLVM and CLANG-RUNTIME, and applying the
given PATCHES.  When TOOLS-EXTRA is given, it must point to the
'clang-tools-extra' tarball, which contains code for 'clang-tidy', 'pp-trace',
'modularize', and other tools."
  (package
    (name "clang")
    (version (package-version llvm))
    (source
     (origin
       (method url-fetch)
       (uri (llvm-uri (if (version>=? version "9.0.1")
                                   "clang"
                                   "cfe")
                               version))
       (sha256 (base32 hash))
       (patches (map search-patch patches))))
    ;; Using cmake allows us to treat llvm as an external library.  There
    ;; doesn't seem to be any way to do this with clang's autotools-based
    ;; build system.
    (build-system cmake-build-system)
    (outputs (if tools-extra '("out" "extra") '("out")))
    (native-inputs (package-native-inputs llvm))
    (inputs
     `(("libxml2" ,libxml2)
       ("gcc-lib" ,gcc "lib")
       ,@(package-inputs llvm)
       ,@(if tools-extra
             `(("clang-tools-extra" ,tools-extra))
             '())))
    (propagated-inputs
     (list llvm clang-runtime))
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
                  ,@(if tools-extra
                        `((add-after 'unpack 'add-tools-extra
                            (lambda* (#:key inputs #:allow-other-keys)
                              ;; Unpack the 'clang-tools-extra' tarball under
                              ;; tools/.
                              (let ((extra (assoc-ref inputs
                                                      "clang-tools-extra")))
                                (invoke "tar" "xf" extra)
                                (rename-file ,(string-append
                                               "clang-tools-extra-"
                                               (package-version llvm)
                                               ".src")
                                             "tools/extra")
                                #t)))
                          (add-after 'install 'move-extra-tools
                            (lambda* (#:key outputs #:allow-other-keys)
                              ;; Move the extra tools to the "extra" output.
                              ;; These programs alone weigh in at 296 MiB,
                              ;; because they statically-link a whole bunch of
                              ;; Clang libraries.
                              (let* ((out   (assoc-ref outputs "out"))
                                     (extra (assoc-ref outputs "extra"))
                                     (bin   (string-append out "/bin"))
                                     (bin*  (string-append extra "/bin"))
                                     (lib   (string-append out "/lib")))
                                (define (move program)
                                  (rename-file (string-append bin "/" program)
                                               (string-append bin* "/"
                                                              program)))

                                (mkdir-p bin*)
                                (for-each move
                                          '("clang-apply-replacements"
                                            "clang-change-namespace"
                                            "clangd"
                                            "clang-doc"
                                            "clang-include-fixer"
                                            "clang-move"
                                            "clang-query"
                                            "clang-reorder-fields"
                                            "clang-tidy"
                                            "find-all-symbols"
                                            "modularize"
                                            "pp-trace"))

                                ;; Remove MiBs of .a files coming from
                                ;; 'clang-tools-extra'.
                                (for-each (lambda (component)
                                            (delete-file
                                             (string-append lib "/libclang"
                                                            component ".a")))
                                          '("ApplyReplacements"
                                            "ChangeNamespace"
                                            "Daemon"
                                            "DaemonTweaks"
                                            "Doc"
                                            "IncludeFixer"
                                            "IncludeFixerPlugin"
                                            "Move"))
                                (for-each delete-file
                                          (find-files
                                           lib
                                           "^(libfindAllSymbols|libclangTidy)"))
                                #t))))
                        '())
                  (add-after 'unpack 'add-missing-triplets
                    (lambda _
                      ;; Clang iterates through known triplets to search for
                      ;; GCC's headers, but does not recognize some of the
                      ;; triplets that are used in Guix.
                      (substitute* ,@(if (version>=? version "6.0")
                                         '("lib/Driver/ToolChains/Gnu.cpp")
                                         '("lib/Driver/ToolChains.cpp"))
                        (("\"aarch64-linux-gnu\"," all)
                         (string-append "\"aarch64-unknown-linux-gnu\", "
                                        all))
                        (("\"arm-linux-gnueabihf\"," all)
                         (string-append all
                                        " \"arm-unknown-linux-gnueabihf\","))
                        (("\"i686-pc-linux-gnu\"," all)
                         (string-append "\"i686-unknown-linux-gnu\", "
                                        all)))
                      #t))
                  (add-after 'unpack 'set-glibc-file-names
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((libc (assoc-ref inputs "libc"))
                            (compiler-rt (assoc-ref inputs "clang-runtime"))
                            (gcc (assoc-ref inputs "gcc")))
                        ,@(cond
                           ((version>=? version "6.0")
                            `(;; Link to libclang_rt files from clang-runtime.
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
                                  "LibStdCXXIncludePathCandidates[] = { \"" gcc
                                  "/include/c++\","))

                                ;; Make sure libc's libdir is on the search path, to
                                ;; allow crt1.o & co. to be found.
                                (("@GLIBC_LIBDIR@")
                                 (string-append libc "/lib")))))
                           (else
                            `((substitute* "lib/Driver/Tools.cpp"
                                ;; Patch the 'getLinuxDynamicLinker' function so that
                                ;; it uses the right dynamic linker file name.
                                (("/lib64/ld-linux-x86-64.so.2")
                                 (string-append libc
                                                ,(glibc-dynamic-linker))))

                              ;; Link to libclang_rt files from clang-runtime.
                              ;; This substitution needed slight adjustment in 3.8.
                              ,@(if (version>=? version "3.8")
                                    '((substitute* "lib/Driver/Tools.cpp"
                                        (("TC\\.getDriver\\(\\)\\.ResourceDir")
                                         (string-append "\"" compiler-rt "\""))))
                                    '((substitute* "lib/Driver/ToolChain.cpp"
                                        (("getDriver\\(\\)\\.ResourceDir")
                                         (string-append "\"" compiler-rt "\"")))))

                              ;; Make sure libc's libdir is on the search path, to
                              ;; allow crt1.o & co. to be found.
                              (substitute* "lib/Driver/ToolChains.cpp"
                                (("@GLIBC_LIBDIR@")
                                 (string-append libc "/lib"))))))
                        #t)))
                  ,@(if (version>=? version "10")
                        `((add-after 'install 'adjust-cmake-file
                            (lambda* (#:key outputs #:allow-other-keys)
                              (let ((out (assoc-ref outputs "out")))
                                ;; Clang generates a CMake file with "targets"
                                ;; for each installed library file.  Downstream
                                ;; consumers of the CMake interface can use this
                                ;; to get absolute library locations.  Including
                                ;; this file will needlessly assert that _all_
                                ;; libraries are available, which causes problems
                                ;; in Guix because some are removed (see the
                                ;; move-extra-tools phase).  Thus, remove the
                                ;; asserts so that the main functionality works.
                                (substitute*
                                    (string-append
                                     out
                                     "/lib/cmake/clang/ClangTargets-release.cmake")
                                  (("list\\(APPEND _IMPORT_CHECK_TARGETS.*" all)
                                   (string-append "# Disabled by Guix.\n#" all)))
                                #t))))
                        '())
                  ,@(if (version>? version "3.8")
                        `((add-after 'install 'symlink-cfi_ignorelist
                            (lambda* (#:key inputs outputs #:allow-other-keys)
                              (let* ((out (assoc-ref outputs "out"))
                                     (lib-share (string-append out "/lib/clang/"
                                                               ,version "/share"))
                                     (compiler-rt (assoc-ref inputs "clang-runtime"))
                                     (file-name ,(if (version>=? version "13")
                                                     "cfi_ignorelist.txt"
                                                     "cfi_blacklist.txt"))
                                     ;; The location varies between Clang versions.
                                     (cfi-ignorelist
                                      (cond
                                       ((file-exists?
                                         (string-append compiler-rt "/" file-name))
                                        (string-append compiler-rt "/" file-name))
                                       (else (string-append compiler-rt
                                                            "/share/" file-name)))))
                                (mkdir-p lib-share)
                                ;; Symlink the ignorelist to where Clang expects
                                ;; to find it.
                                (symlink cfi-ignorelist
                                         (string-append lib-share "/" file-name))))))
                        '())
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
            (variable "C_INCLUDE_PATH")
            (files '("include")))
           (search-path-specification
            (variable "CPLUS_INCLUDE_PATH")
            (files '("include/c++" "include")))
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
    (properties properties)
    (license (if (version>=? version "9.0")
                 license:asl2.0         ;with LLVM exceptions
                 license:ncsa))))

(define (clang-properties version)
  "Return package properties for Clang VERSION."
  `((compiler-cpu-architectures
     ("x86_64"
      ;; This list was obtained by running:
      ;;
      ;;   guix shell clang -- llc -march=x86-64 -mattr=help
      ;;
      ;; filtered from uninteresting entries such as "i686" and "pentium".
      ,@(if (version>=? version "10.0")           ;TODO: refine
            '("atom"
              "barcelona"
              "bdver1"
              "bdver2"
              "bdver3"
              "bdver4"
              "bonnell"
              "broadwell"
              "btver1"
              "btver2"
              "c3"
              "c3-2"
              "cannonlake"
              "cascadelake"
              "cooperlake"
              "core-avx-i"
              "core-avx2"
              "core2"
              "corei7"
              "corei7-avx"
              "generic"
              "geode"
              "goldmont"
              "goldmont-plus"
              "haswell"
              "icelake-client"
              "icelake-server"
              "ivybridge"
              "k8"
              "k8-sse3"
              "knl"
              "knm"
              "lakemont"
              "nehalem"
              "nocona"
              "opteron"
              "opteron-sse3"
              "sandybridge"
              "silvermont"
              "skx"
              "skylake"
              "skylake-avx512"
              "slm"
              "tigerlake"
              "tremont"
              "westmere"
              "x86-64"
              "x86-64-v2"
              "x86-64-v3"
              "x86-64-v4"
              "znver1"
              "znver2"
              "znver3")
            '())))))

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

                     ;; Create 'cc' and 'c++' so that one can use it as a
                     ;; drop-in replacement for the default tool chain and
                     ;; have configure scripts find the compiler.
                     (symlink "clang" (string-append out "/bin/cc"))
                     (symlink "clang++" (string-append out "/bin/c++"))

                     (union-build (assoc-ref %outputs "debug")
                                  (list (assoc-ref %build-inputs
                                                   "libc-debug")))
                     (union-build (assoc-ref %outputs "static")
                                  (list (assoc-ref %build-inputs
                                                   "libc-static")))
                     #t))))

    (native-search-paths
     (append (package-native-search-paths clang)
             (list (search-path-specification     ;copied from glibc
                    (variable "GUIX_LOCPATH")
                    (files '("lib/locale"))))))
    (search-paths (package-search-paths clang))

    (license (package-license clang))
    (properties (package-properties clang))  ;for 'compiler-cpu-architectures'
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
              ("libomp" ,libomp)            ;used when linking with '-fopenmp'
              ("libc" ,glibc)
              ("libc-debug" ,glibc "debug")
              ("libc-static" ,glibc "static")))))

(define-public llvm-13
  (package
    (name "llvm")
    (version "13.0.0")
    (source
     (origin
      (method url-fetch)
      (uri (llvm-uri "llvm" version))
      (sha256
       (base32
        "081h2vw757j5xjg2441539j2vhfzzihrgxwza5pq5sj3hrq133a0"))))
    (build-system cmake-build-system)
    (outputs '("out" "opt-viewer"))
    (native-inputs
     `(("python" ,python-wrapper)
       ("perl"   ,perl)))
    (inputs
     (list libffi))
    (propagated-inputs
     (list zlib))                 ;to use output from llvm-config
    (arguments
     `(#:configure-flags
       ,#~(quasiquote
           ;; These options are required for cross-compiling LLVM according to
           ;; https://llvm.org/docs/HowToCrossCompileLLVM.html.
           (#$@(if (%current-target-system)
                   #~(,(string-append "-DLLVM_TABLEGEN="
                                      #+(file-append this-package
                                                     "/bin/llvm-tblgen"))
                      #$(string-append "-DLLVM_DEFAULT_TARGET_TRIPLE="
                                       (%current-target-system))
                      #$(string-append "-DLLVM_TARGET_ARCH="
                                       (system->llvm-target))
                      #$(string-append "-DLLVM_TARGETS_TO_BUILD="
                                       (system->llvm-target)))
                   #~())
            "-DCMAKE_SKIP_BUILD_RPATH=FALSE"
            "-DCMAKE_BUILD_WITH_INSTALL_RPATH=FALSE"
            "-DBUILD_SHARED_LIBS:BOOL=TRUE"
            "-DLLVM_ENABLE_FFI:BOOL=TRUE"
            "-DLLVM_REQUIRES_RTTI=1" ; For some third-party utilities
            "-DLLVM_INSTALL_UTILS=ON")) ; Needed for rustc.
       ;; Don't use '-g' during the build, to save space.
       #:build-type "Release"
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-opt-viewer
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (opt-viewer-out (assoc-ref outputs "opt-viewer"))
                    (opt-viewer-share-dir (string-append opt-viewer-out "/share"))
                    (opt-viewer-dir (string-append opt-viewer-share-dir "/opt-viewer")))
               (mkdir-p opt-viewer-share-dir)
               (rename-file (string-append out "/share/opt-viewer")
                            opt-viewer-dir)))))))
    (home-page "https://www.llvm.org")
    (synopsis "Optimizing compiler infrastructure")
    (description
     "LLVM is a compiler infrastructure designed for compile-time, link-time,
runtime, and idle-time optimization of programs from arbitrary programming
languages.  It currently supports compilation of C and C++ programs, using
front-ends derived from GCC 4.0.1.  A new front-end for the C family of
languages is in development.  The compiler infrastructure includes mirror sets
of programming tools as well as libraries with equivalent functionality.")
    (license license:asl2.0)))

(define-public clang-runtime-13
  (clang-runtime-from-llvm
   llvm-13
   "0gyvfhnypfmlf7hdgkiz2wh2lgk4nz26aqf361msjs3qdkbh4djc"))

(define-public clang-13
  (clang-from-llvm llvm-13 clang-runtime-13
                   "0zp1p6syii5iajm8v2c207s80arv00yz5ckfwimn5dng0sxiqqax"
                   #:patches '("clang-13.0-libc-search-path.patch")
                   #:tools-extra
                   (origin
                     (method url-fetch)
                     (uri (llvm-uri "clang-tools-extra"
                                    (package-version llvm-13)))
                     (sha256
                      (base32
                       "1mgalgdgxlxi08yxw7k6yh4iia1bpjmjgn7mrpqas8lbl9h612s2")))))

(define-public clang-toolchain-13
  (make-clang-toolchain clang-13))

(define-public llvm-12
  (package
    (inherit llvm-13)
    (version "12.0.1")
    (source
     (origin
      (method url-fetch)
      (uri (llvm-uri "llvm" version))
      (sha256
       (base32
        "1pzx9zrmd7r3481sbhwvkms68fwhffpp4mmz45dgrkjpyl2q96kx"))))
    (arguments
     ;; TODO(core-updates): Unconditionally use quasiquote
     `(#:configure-flags
       ,#~(#$(if (%current-target-system)
                 #~quasiquote
                 #~quote)
           ;; These options are required for cross-compiling LLVM according to
           ;; https://llvm.org/docs/HowToCrossCompileLLVM.html.
           (#$@(if (%current-target-system)
                   #~(,(string-append "-DLLVM_TABLEGEN="
                                      #+(file-append this-package
                                                     "/bin/llvm-tblgen"))
                      #$(string-append "-DLLVM_DEFAULT_TARGET_TRIPLE="
                                       (%current-target-system))
                      #$(string-append "-DLLVM_TARGET_ARCH="
                                       (system->llvm-target))
                      #$(string-append "-DLLVM_TARGETS_TO_BUILD="
                                       (system->llvm-target)))
                   #~())
            "-DCMAKE_SKIP_BUILD_RPATH=FALSE"
            "-DCMAKE_BUILD_WITH_INSTALL_RPATH=FALSE"
            "-DBUILD_SHARED_LIBS:BOOL=TRUE"
            "-DLLVM_ENABLE_FFI:BOOL=TRUE"
            "-DLLVM_REQUIRES_RTTI=1" ; For some third-party utilities
            "-DLLVM_INSTALL_UTILS=ON")) ; Needed for rustc.
       ;; Don't use '-g' during the build, to save space.
       #:build-type "Release"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'shared-lib-workaround
           ;; Even with CMAKE_SKIP_BUILD_RPATH=FALSE, llvm-tblgen
           ;; doesn't seem to get the correct rpath to be able to run
           ;; from the build directory.  Set LD_LIBRARY_PATH as a
           ;; workaround.
           (lambda _
             (setenv "LD_LIBRARY_PATH"
                     (string-append (getcwd) "/lib"))
             #t))
         (add-after 'install 'install-opt-viewer
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (opt-viewer-out (assoc-ref outputs "opt-viewer"))
                    (opt-viewer-share-dir (string-append opt-viewer-out "/share"))
                    (opt-viewer-dir (string-append opt-viewer-share-dir "/opt-viewer")))
               (mkdir-p opt-viewer-share-dir)
               (rename-file (string-append out "/share/opt-viewer")
                            opt-viewer-dir))
             #t)))))))

(define-public clang-runtime-12
  (clang-runtime-from-llvm
   llvm-12
   "1950rg294izdwkaasi7yjrmadc9mzdd5paf0q63jjcq2m3rdbj5l"))

(define-public clang-12
  (clang-from-llvm llvm-12 clang-runtime-12
                   "0px4gl27az6cdz6adds89qzdwb1cqpjsfvrldbz9qvpmphrj34bf"
                   #:patches '("clang-12.0-libc-search-path.patch")
                   #:tools-extra
                   (origin
                     (method url-fetch)
                     (uri (llvm-uri "clang-tools-extra"
                                    (package-version llvm-12)))
                     (sha256
                      (base32
                       "1r9a4fdz9ci58b5z2inwvm4z4cdp6scrivnaw05dggkxz7yrwrb5")))))

(define-public clang-toolchain-12
  (make-clang-toolchain clang-12))

(define-public llvm-11
  (package
    (inherit llvm-12)
    (version "11.0.0")
    (source
     (origin
      (method url-fetch)
      (uri (llvm-uri "llvm" version))
      (sha256
       (base32
        "0s94lwil98w7zb7cjrbnxli0z7gklb312pkw74xs1d6zk346hgwi"))))))

(define-public clang-runtime-11
  (clang-runtime-from-llvm
   llvm-11
   "0d5j5l8phwqjjscmk8rmqn0i2i0abl537gdbkagl8fjpzy1gyjip"))

(define-public clang-11
  (clang-from-llvm llvm-11 clang-runtime-11
                   "02ajkij85966vd150iy246mv16dsaph1kfi0y8wnncp8w6nar5hg"
                   #:patches '("clang-11.0-libc-search-path.patch")
                   #:tools-extra
                   (origin
                     (method url-fetch)
                     (uri (llvm-uri "clang-tools-extra"
                                    (package-version llvm-11)))
                     (sha256
                      (base32
                       "02bcwwn54661madhq4nxc069s7p7pj5gpqi8ww50w3anbpviilzy")))))

(define-public clang-toolchain-11
  (make-clang-toolchain clang-11))

(define-public llvm-10
  (package
    (inherit llvm-11)
    (version "10.0.1")
    (source
     (origin
      (method url-fetch)
      (uri (llvm-uri "llvm" version))
      (sha256
       (base32
        "1wydhbp9kyjp5y0rc627imxgkgqiv3dfirbqil9dgpnbaw5y7n65"))))))

(define-public clang-runtime-10
  (clang-runtime-from-llvm
   llvm-10
   "1yjqjri753w0fzmxcyz687nvd97sbc9rsqrxzpq720na47hwh3fr"))

(define-public clang-10
  (clang-from-llvm llvm-10 clang-runtime-10
                   "091bvcny2lh32zy8f3m9viayyhb2zannrndni7325rl85cwgr6pr"
                   #:patches '("clang-10.0-libc-search-path.patch")
                   #:tools-extra
                   (origin
                     (method url-fetch)
                     (uri (llvm-uri "clang-tools-extra"
                                             (package-version llvm-10)))
                     (sha256
                      (base32
                       "06n1yp638rh24xdxv9v2df0qajxbjz4w59b7dd4ky36drwmpi4yh")))))

(define-public clang-toolchain-10
  (make-clang-toolchain clang-10))

(define-public llvm-9
  (package
    (inherit llvm-10)
    (version "9.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (llvm-uri "llvm" version))
       (sha256
        (base32
         "16hwp3qa54c3a3v7h8nlw0fh5criqh0hlr1skybyk0cz70gyx880"))
       (patches (search-patches
                 "llvm-9-fix-bitcast-miscompilation.patch"
                 "llvm-9-fix-scev-miscompilation.patch"
                 "llvm-9-fix-lpad-miscompilation.patch"))))))

(define-public clang-runtime-9
  (clang-runtime-from-llvm
   llvm-9
   "0xwh79g3zggdabxgnd0bphry75asm1qz7mv3hcqihqwqr6aspgy2"
   '("clang-runtime-9-libsanitizer-mode-field.patch")))

(define-public clang-9
  (clang-from-llvm llvm-9 clang-runtime-9
                   "0ls2h3iv4finqyflyhry21qhc9cm9ga7g1zq21020p065qmm2y2p"
                   #:patches '("clang-9.0-libc-search-path.patch")))

(define-public clang-toolchain-9
  (make-clang-toolchain clang-9))

(define-public llvm-8
  (package
    (inherit llvm-9)
    (version "8.0.0")
    (source (origin
              (method url-fetch)
              (uri (llvm-uri "llvm" version))
              (sha256
               (base32
                "0k124sxkfhfi1rca6kzkdraf4axhx99x3cw2rk55056628dvwwl8"))))
    (license license:ncsa)))

(define-public clang-runtime-8
  (clang-runtime-from-llvm
   llvm-8
   "1c919wsm17xnv7lr8bhpq2wkq8113lzlw6hzhfr737j59x3wfddl"
   '("clang-runtime-9-libsanitizer-mode-field.patch")))

(define-public clang-8
  (clang-from-llvm llvm-8 clang-runtime-8
                   "0svk1f70hvpwrjp6x5i9kqwrqwxnmcrw5s7f4cxyd100mdd12k08"
                   #:patches '("clang-8.0-libc-search-path.patch")))

(define-public clang-toolchain-8
  (make-clang-toolchain clang-8))

(define-public llvm-7
  (package
    (inherit llvm-8)
    (version "7.1.0")
    (source (origin
              (method url-fetch)
              (uri (llvm-uri "llvm" version))
              (sha256
               (base32
                "0r1p5didv4rkgxyvbkyz671xddg6i3dxvbpsi1xxipkla0l9pk0v"))))))

(define-public clang-runtime-7
  (clang-runtime-from-llvm
   llvm-7
   "1n48p8gjarihkws0i2bay5w9bdwyxyxxbpwyng7ba58jb30dlyq5"
   '("clang-runtime-9-libsanitizer-mode-field.patch")))

(define-public clang-7
  (clang-from-llvm llvm-7 clang-runtime-7
                   "0vc4i87qwxnw9lci4ayws9spakg0z6w5w670snj9f8g5m9rc8zg9"
                   #:patches '("clang-7.0-libc-search-path.patch")))

(define-public clang-toolchain-7
  (make-clang-toolchain clang-7))

(define-public llvm-6
  (package
    (inherit llvm-7)
    (version "6.0.1")
    (source (origin
              (method url-fetch)
              (uri (llvm-uri "llvm" version))
              (sha256
               (base32
                "1qpls3vk85lydi5b4axl0809fv932qgsqgdgrk098567z4jc7mmn"))))))

(define-public clang-runtime-6
  (clang-runtime-from-llvm
   llvm-6
   "1fcr3jn24yr8lh36nc0c4ikli4744i2q9m1ik67p1jymwwaixkgl"
   '("clang-runtime-9-libsanitizer-mode-field.patch")))

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
      (uri (llvm-uri "llvm" version))
      (sha256
       (base32
        "1vi9sf7rx1q04wj479rsvxayb6z740iaz3qniwp266fgp5a07n8z"))))
    (outputs '("out"))
    (arguments
     (substitute-keyword-arguments (package-arguments llvm)
       ((#:phases phases)
        `(modify-phases ,phases
           (delete 'install-opt-viewer)))))))

(define-public clang-runtime-3.9.1
  (clang-runtime-from-llvm
   llvm-3.9.1
   "16gc2gdmp5c800qvydrdhsp0bzb97s8wrakl6i8a4lgslnqnf2fk"
   '("clang-runtime-3.9-libsanitizer-mode-field.patch"
     "clang-runtime-asan-build-fixes.patch"
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
      (uri (llvm-uri "llvm" version))
      (sha256
       (base32
        "1ybmnid4pw2hxn12ax5qa5kl1ldfns0njg8533y3mzslvd5cx0kf"))))))

(define-public clang-runtime-3.8
  (clang-runtime-from-llvm
   llvm-3.8
   "0p0y85c7izndbpg2l816z7z7558axq11d5pwkm4h11sdw7d13w0d"
   '("clang-runtime-asan-build-fixes.patch"
     "clang-runtime-3.8-libsanitizer-mode-field.patch"
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
       (uri (llvm-uri "llvm" version))
       (sha256
        (base32
         "1masakdp9g2dan1yrazg7md5am2vacbkb3nahb3dchpc1knr8xxy"))))))

(define-public clang-runtime-3.7
  (clang-runtime-from-llvm
   llvm-3.7
   "10c1mz2q4bdq9bqfgr3dirc6hz1h3sq8573srd5q5lr7m7j6jiwx"
   '("clang-runtime-asan-build-fixes.patch"
     "clang-runtime-3.8-libsanitizer-mode-field.patch"
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
       (uri (llvm-uri "llvm" version))
       (patches
        (search-patches "llvm-3.6-fix-build-with-gcc-10.patch"))
       (sha256
        (base32
         "153vcvj8gvgwakzr4j0kndc0b7wn91c2g1vy2vg24s6spxcc23gn"))))))

(define-public llvm-3.5
  (package (inherit llvm-3.6)
    (version "3.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (llvm-uri "llvm" version))
       (patches
        (search-patches "llvm-3.5-fix-clang-build-with-gcc5.patch"))
       (sha256
        (base32
         "0xf5q17kkxsrm2gsi93h4pwlv663kji73r2g4asb97klsmb626a4"))))))

(define-public clang-runtime-3.5
  (let ((runtime (clang-runtime-from-llvm
                  llvm-3.5
                  "1hsdnzzdr5kglz6fnv3lcsjs222zjsy14y8ax9dy6zqysanplbal"
                  '("clang-runtime-asan-build-fixes.patch"
                    "clang-runtime-3.5-libsanitizer-mode-field.patch"
                    "clang-3.5-libsanitizer-ustat-fix.patch"))))
    (package/inherit runtime
      (arguments
       (substitute-keyword-arguments (package-arguments runtime)
         ((#:phases phases '%standard-phases)
          `(modify-phases ,phases
             ;; glibc no longer includes rpc/xdr.h, so we use the headers from
             ;; libtirpc.
             (add-after 'unpack 'find-rpc-includes
               (lambda* (#:key inputs #:allow-other-keys)
                 (setenv "CPATH"
                         (string-append
                          (search-input-directory inputs "/include/tirpc")
                          ":" (or (getenv "CPATH") "")))
                 (setenv "CPLUS_INCLUDE_PATH"
                         (string-append
                          (search-input-directory inputs "/include/tirpc")
                          ":" (or (getenv "CPLUS_INCLUDE_PATH") "")))))))))
      (inputs
       `(("libtirpc" ,libtirpc)
         ("llvm" ,llvm-3.5))))))

(define-public clang-3.5
  (clang-from-llvm llvm-3.5 clang-runtime-3.5
                   "0846h8vn3zlc00jkmvrmy88gc6ql6014c02l4jv78fpvfigmgssg"
                   #:patches '("clang-3.5-libc-search-path.patch")))

;; Default LLVM and Clang version.
(define-public llvm llvm-9)
(define-public clang-runtime clang-runtime-9)
(define-public clang clang-9)
(define-public clang-toolchain clang-toolchain-9)

(define-public llvm-for-rocm
  (package
    ;; Actually based on LLVM 13 as of v4.3, but llvm-12 works just fine.
    (inherit llvm-12)
    (name "llvm-for-rocm")
    (version "4.3.0")                         ;this must match '%rocm-version'
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/RadeonOpenCompute/llvm-project.git")
                    (commit (string-append "rocm-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0p75nr1qpmy6crymdax5hm40wkimman4lnglz4x5cnbiqindya7s"))
              (patches
               (search-patches "llvm-roc-4.2.0-add_Object.patch"
                               "llvm-roc-3.0.0-add_libraries.patch"
                               "llvm-roc-4.0.0-remove-isystem-usr-include.patch"))))
    (arguments
     (substitute-keyword-arguments (package-arguments llvm-12)
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (add-after 'unpack 'chdir
             (lambda _
               (chdir "llvm")))))
       ((#:configure-flags flags)
        ''("-DLLVM_ENABLE_PROJECTS=llvm;clang;lld"
           "-DLLVM_TARGETS_TO_BUILD=AMDGPU;X86"
           "-DCMAKE_SKIP_BUILD_RPATH=FALSE"
           "-DCMAKE_BUILD_WITH_INSTALL_RPATH=FALSE"
           "-DBUILD_SHARED_LIBS:BOOL=TRUE"
           "-DLLVM_VERSION_SUFFIX="))))
    (properties `((hidden? . #t)
                  ,@(package-properties llvm-12)))))



(define-public libunwind-headers
  (package
    (name "libunwind-headers")
    (version "13.0.0")
    (source (origin
              (method url-fetch)
              (uri (llvm-uri "libunwind" version))
              (sha256
               (base32
                "1qb5ickp7qims5q7sxacj3fwq1kklvnl94k3v9hpl5qn284iky1n"))))
    (build-system cmake-build-system)
    (arguments
     '(#:phases (modify-phases (map (lambda (phase)
                                      (assq phase %standard-phases))
                                    '(set-paths unpack))
                  (add-after 'unpack 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (mkdir out)
                        (copy-recursively "include"
                                          (string-append out "/include"))))))))
    (home-page "https://clang.llvm.org/docs/Toolchain.html")
    (synopsis "LLVM libunwind header files")
    (description
     "This package contains header files for the LLVM C++ unwinding library.")
    (license license:asl2.0)))          ;with LLVM exceptions

(define-public lld
  (package
    (name "lld")
    (version "13.0.0")
    (source (origin
              (method url-fetch)
              (uri (llvm-uri "lld" version))
              (sha256
               (base32
                "11lkwv4jy35z2f3zcpv7hbbk9v9gpavfvxlif8265zv4rl5r1l90"))))
    (build-system cmake-build-system)
    (native-inputs
     ;; Note: check <https://bugs.llvm.org/show_bug.cgi?id=49228> to see
     ;; whether this is still necessary.
     (list libunwind-headers))
    (inputs
     (list llvm-13))
    (arguments
     `(#:build-type "Release"
       ;; TODO: Tests require the lit tool, which isn't installed by the LLVM
       ;; package.
       #:tests? #f))
    (home-page "https://lld.llvm.org/")
    (synopsis "Linker from the LLVM project")
    (description "LLD is a high-performance linker, built as a set of reusable
components which highly leverage existing libraries in the larger LLVM Project.")
    (license license:asl2.0))) ; With LLVM exception

(define-public lld-12
  (package
    (inherit lld)
    (version "12.0.1")
    (source (origin
              (method url-fetch)
              (uri (llvm-uri "lld" version))
              (sha256
               (base32
                "0qg3fgc7wj34hdkqn21y03zcmsdd01szhhm1hfki63iifrm3y2v9"))))
    (inputs (modify-inputs (package-inputs lld)
              (replace "llvm" llvm-12)))))

(define* (make-lld-wrapper lld #:key lld-as-ld?)
  "Return a LLD wrapper.  When LLD-AS-LD? is true, create a 'ld' symlink that
points to 'lld'."
  (package
    (name (if lld-as-ld? "lld-as-ld-wrapper" "lld-wrapper"))
    (version "0")
    (source #f)
    (build-system trivial-build-system)
    (inputs (list (make-ld-wrapper "ld.lld-wrapper" #:binutils lld
                                   #:linker "ld.lld")
                  (make-ld-wrapper "lld-wrapper" #:binutils lld #:linker
                                   "lld")))
    (arguments
     (list #:builder
           #~(let ((ld.lld (string-append #$(this-package-input
                                             "ld.lld-wrapper")
                                          "/bin/ld.lld"))
                   (lld (string-append #$(this-package-input "lld-wrapper")
                                       "/bin/lld")))
               (mkdir #$output)
               (mkdir (string-append #$output "/bin"))
               (symlink ld.lld (string-append #$output "/bin/ld.lld"))
               (symlink lld (string-append #$output "/bin/lld"))
               (when #$lld-as-ld?
                 (symlink ld.lld (string-append #$output "/bin/ld"))))))
    (synopsis "LLD linker wrapper")
    (description "This is a linker wrapper for LLD; like @code{ld-wrapper}, it
wraps the linker to add any missing @code{-rpath} flags, and to detect any
misuse of libraries outside of the store.")
    (home-page "https://www.gnu.org/software/guix/")
    (license license:gpl3+)))

;;; A LLD wrapper suitable to use with -fuse-ld and GCC or with Clang.
(define-public lld-wrapper
  (make-lld-wrapper lld))

;;; A LLD wrapper that can be used as a (near) drop-in replacement to GNU ld.
(define-public lld-as-ld-wrapper
  (make-lld-wrapper lld #:lld-as-ld? #t))

(define-public lldb
  (package
    (name "lldb")
    (version "12.0.1")
    (source (origin
              (method url-fetch)
              (uri (llvm-uri "lldb" version))
              (sha256
               (base32
                "0g3pj1m3chafavpr35r9fynm85y2hdyla6klj0h28khxs2613i78"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DCMAKE_CXX_COMPILER=clang++")))
    (native-inputs
     (list pkg-config swig))
    (inputs
     (list clang-12
           llvm-12
           ;; Optional (but recommended) inputs.
           ncurses
           libedit
           xz
           libxml2
           lua
           python))
    (home-page "https://lldb.llvm.org/")
    (synopsis "Low level debugger")
    (description
     "LLDB is a high performance debugger built as a set of reusable components
which highly leverage existing libraries in the larger LLVM project.")
    (license license:asl2.0))) ;with LLVM exceptions

(define-public libcxx
  (package
    (name "libcxx")
    (version "9.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (llvm-uri "libcxx" version))
       (sha256
        (base32
         "0d2bj5i6mk4caq7skd5nsdmz8c2m5w5anximl5wz3x32p08zz089"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases (@ (guix build cmake-build-system) %standard-phases)
         (add-after 'set-paths 'adjust-CPLUS_INCLUDE_PATH
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gcc (assoc-ref inputs  "gcc")))
               ;; Hide GCC's C++ headers so that they do not interfere with
               ;; the ones we are attempting to build.
               (setenv "CPLUS_INCLUDE_PATH"
                       (string-join (delete (string-append gcc "/include/c++")
                                            (string-split (getenv "CPLUS_INCLUDE_PATH")
                                                          #\:))
                                    ":"))
               (format #t
                       "environment variable `CPLUS_INCLUDE_PATH' changed to ~a~%"
                       (getenv "CPLUS_INCLUDE_PATH"))
               #t))))))
    (native-inputs
     (list clang llvm))
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
       (uri (llvm-uri "libcxx" version))
       (sha256
        (base32
         "0rzw4qvxp6qx4l4h9amrq02gp7hbg8lw4m0sy3k60f50234gnm3n"))))
    (native-inputs
     (list clang-6 llvm-6))))

(define-public libcxxabi-6
  (package
    (name "libcxxabi")
    (version "6.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/llvm/llvm-project")
             (commit (string-append "llvmorg-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0ki6796b5z08kh3a3rbysr5wwb2dkl6wal5dzd03i4li5xfkvx1g"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "-DLIBCXXABI_LIBCXX_INCLUDES="
                            (assoc-ref %build-inputs "libcxx")
                            "/include")
             "-DCMAKE_C_COMPILER=clang"
             "-DCMAKE_CXX_COMPILER=clang++")
       #:phases
       (modify-phases (@ (guix build cmake-build-system) %standard-phases)
         (add-after 'unpack 'chdir
           (lambda _ (chdir "libcxxabi")))
         (add-after 'set-paths 'adjust-CPLUS_INCLUDE_PATH
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gcc (assoc-ref inputs  "gcc")))
               ;; Hide GCC's C++ headers so that they do not interfere with
               ;; the ones we are attempting to build.
               (setenv "CPLUS_INCLUDE_PATH"
                       (string-join
                        (cons (string-append
                               (assoc-ref inputs "libcxx") "/include/c++/v1")
                              (delete (string-append gcc "/include/c++")
                                      (string-split (getenv "CPLUS_INCLUDE_PATH")
                                                    #\:)))
                        ":"))
               (format #true
                       "environment variable `CPLUS_INCLUDE_PATH' changed to ~a~%"
                       (getenv "CPLUS_INCLUDE_PATH")))))
         (add-after 'install 'install-headers
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((include-dir (string-append
                                 (assoc-ref outputs "out") "/include")))
               (install-file "../libcxxabi/include/__cxxabi_config.h" include-dir)
               (install-file "../libcxxabi/include/cxxabi.h" include-dir)))))))
    (native-inputs
     (list clang-6 llvm-6 libcxx-6))
    (home-page "https://libcxxabi.llvm.org")
    (synopsis "C++ standard library support")
    (description
     "This package provides an implementation of low level support for a
standard C++ library.")
    (license license:expat)))

(define-public libcxx+libcxxabi-6
  (package
    (inherit libcxx-6)
    (name "libcxx+libcxxabi")
    (version (package-version libcxx-6))
    (arguments
     `(#:configure-flags
       (list "-DLIBCXX_CXX_ABI=libcxxabi"
             (string-append "-DLIBCXX_CXX_ABI_INCLUDE_PATHS="
                            (assoc-ref %build-inputs "libcxxabi")
                            "/include"))
       #:phases
       (modify-phases (@ (guix build cmake-build-system) %standard-phases)
         (add-after 'set-paths 'adjust-CPLUS_INCLUDE_PATH
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gcc (assoc-ref inputs  "gcc")))
               ;; Hide GCC's C++ headers so that they do not interfere with
               ;; the ones we are attempting to build.
               (setenv "CPLUS_INCLUDE_PATH"
                       (string-join
                        (delete (string-append gcc "/include/c++")
                                (string-split (getenv "CPLUS_INCLUDE_PATH")
                                              #\:))
                        ":"))
               (format #true
                       "environment variable `CPLUS_INCLUDE_PATH' changed to ~a~%"
                       (getenv "CPLUS_INCLUDE_PATH"))))))))
    (native-inputs
     (list clang-6 llvm-6 libcxxabi-6))))

(define-public libclc
  (package
    (name "libclc")
    (version "9.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/llvm/llvm-project")
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
     (list clang llvm python))
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
              (uri (llvm-uri "openmp" version))
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
     (list clang llvm perl pkg-config))
    (inputs
     (list `(,hwloc "lib")))
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
    (version "0.37.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "llvmlite" version))
       (sha256
        (base32
         "05avhsvdcqh8wfpblx16qslfq3masqcbkfyn8p3c13h1rmqbi4k3"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-reference-to-llvmlite.so
           ;; ctypes.CDLL uses dlopen to load libllvmlite.so, which
           ;; fails, so locate it by its absolute path.  Change it in
           ;; ffi.py, not utils.py, because setup.py relies on the
           ;; output of get_library_name for proper installation.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (libllvmlite.so (string-append out "/lib/python"
                                                   ,(version-major+minor
                                                     (package-version python))
                                                   "/site-packages/llvmlite/"
                                                   "binding/libllvmlite.so")))
               (substitute* "llvmlite/binding/ffi.py"
                 (("_lib_name = get_library_name\\(\\)")
                  (format #f "_lib_name = ~s" libllvmlite.so))))))
         (add-after 'unpack 'skip-failing-tests
           (lambda _
             (substitute* "llvmlite/tests/test_binding.py"
               (("    def test_libm\\(self\\).*" all)
                (string-append "    @unittest.skip('Fails on Guix')\n" all)))))
         (add-before 'build 'set-compiler/linker-flags
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((llvm (assoc-ref inputs "llvm")))
               ;; Refer to ffi/Makefile.linux.
               (setenv "CPPFLAGS" "-fPIC")
               (setenv "LDFLAGS" (string-append "-Wl,-rpath="
                                                llvm "/lib"))))))))
    (inputs
     (list
      (let* ((patches-commit
              "a4a19e8af2c5ef9b9901f20193e4be070726da97")
             (patch-uri (lambda (name)
                          (string-append
                           "https://raw.githubusercontent.com/numba/"
                           "llvmlite/"
                           patches-commit
                           "/conda-recipes/"
                           name)))
             (patch-origin (lambda (name hash)
                             (origin (method url-fetch)
                                     (uri (patch-uri name))
                                     (sha256 (base32 hash)))))
             (arch-independent-patches
              (list (patch-origin
                     "partial-testing.patch"
                     "0g3nkci87knvmn7piqhmh4bcc65ff8r921cvfcibyiv65klv3syg")
                    (patch-origin
                     "0001-Revert-Limit-size-of-non-GlobalValue-name.patch"
                     "0n4k7za0smx6qwdipsh6x5lm7bfvzzb3p9r8q1zq1dqi4na21295"))))
        (package
          (inherit llvm-11)
          (source
           (origin
             (inherit (package-source llvm-11))
             (patches (if (string=? "aarch64-linux" (%current-system))
                          `(,(patch-origin
                              "intel-D47188-svml-VF_LLVM9.patch"
                              "0gnnlfxr8p1a7ls93hzcpfqpa8r0icypfwj8l9cmkslq5sz8p64r")
                            ,@arch-independent-patches
                            ,@(origin-patches (package-source llvm-11)))
                          `(,(patch-origin
                              "intel-D47188-svml-VF.patch"
                              "0gnnlfxr8p1a7ls93hzcpfqpa8r0icypfwj8l9cmkslq5sz8p64r")
                            ,(patch-origin
                              "expect-fastmath-entrypoints-in-add-TLI-mappings.ll.patch"
                              "0jxhjkkwwi1cy898l2n57l73ckpw0v73lqnrifp7r1mwpsh624nv")
                            ,@arch-independent-patches
                            ,@(origin-patches (package-source llvm-11)))))))))))
    (home-page "https://llvmlite.pydata.org")
    (synopsis "Wrapper around basic LLVM functionality")
    (description
     "This package provides a Python binding to LLVM for use in Numba.")
    (license license:bsd-3)))

(define-public (clang-python-bindings clang)
  "Return a package for the Python bindings of CLANG."
  (package
    (inherit clang)
    (name "python-clang")
    (build-system python-build-system)
    (outputs '("out"))
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'build 'change-directory
                    (lambda _
                      (chdir "bindings/python")))
                  (add-before 'build 'create-setup-py
                    (lambda _
                      ;; Generate a basic "setup.py", enough so it can be
                      ;; built and installed.
                      (with-output-to-file "setup.py"
                        (lambda ()
                          (display "from setuptools import setup
setup(name=\"clang\", packages=[\"clang\"])\n")))))
                  (add-before 'build 'set-libclang-file-name
                    (lambda* (#:key inputs #:allow-other-keys)
                      ;; Record the absolute file name of libclang.so.
                      (let ((libclang (search-input-file inputs
                                                         "/lib/libclang.so")))
                        (substitute* "clang/cindex.py"
                          (("libclang\\.so") libclang))))))))
    (inputs (list clang))
    (synopsis "Python bindings to libclang")))

(define-public python-clang-10
  (clang-python-bindings clang-10))

(define-public python-clang-11
  (clang-python-bindings clang-11))

(define-public python-clang-12
  (clang-python-bindings clang-12))

(define-public python-clang-13
  (clang-python-bindings clang-13))

(define-public emacs-clang-format
  (package
    (inherit clang)
    (name "emacs-clang-format")
    (build-system emacs-build-system)
    (inputs
     (list clang))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'configure
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((clang-format (search-input-file inputs "/bin/clang-format")))
               (copy-file "tools/clang-format/clang-format.el" "clang-format.el")
               (emacs-substitute-variables "clang-format.el"
                 ("clang-format-executable"
                  clang-format))))))))
    (synopsis "Format code using clang-format")
    (description "This package filters code through @code{clang-format}
to fix its formatting.  @code{clang-format} is a tool that formats
C/C++/Obj-C code according to a set of style options, see
@url{https://clang.llvm.org/docs/ClangFormatStyleOptions.html}.")))

(define-public emacs-clang-rename
  (package
    (inherit clang)
    (name "emacs-clang-rename")
    (build-system emacs-build-system)
    (inputs
     (list clang))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'configure
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((clang-rename (search-input-file inputs "/bin/clang-rename")))
               (copy-file "tools/clang-rename/clang-rename.el" "clang-rename.el")
               (emacs-substitute-variables "clang-rename.el"
                 ("clang-rename-binary"
                  clang-rename))))))))
    (synopsis "Rename every occurrence of a symbol using clang-rename")
    (description "This package renames every occurrence of a symbol at point
using @code{clang-rename}.")))


;;;
;;; LLVM variants.
;;;

(define make-ocaml-llvm
  ;; Make it a memoizing procedure so its callers below don't end up defining
  ;; two equal-but-not-eq "ocaml-llvm" packages for the default LLVM.
  (mlambdaq (llvm)
    (package
      (inherit llvm)
      (name "ocaml-llvm")
      (outputs '("out"))
      (arguments
       `(#:configure-flags
         (list
          (string-append "-DLLVM_OCAML_EXTERNAL_LLVM_LIBDIR="
                         (assoc-ref %build-inputs "llvm") "/lib")
          "-DBUILD_SHARED_LIBS=TRUE"
          "-DLLVM_OCAML_OUT_OF_TREE=TRUE"
          (string-append "-DLLVM_OCAML_INSTALL_PATH="
                         (assoc-ref %outputs "out") "/lib/ocaml/site-lib"))
         #:phases
         (modify-phases %standard-phases
           (replace 'build
             (lambda _
               (invoke "make" "ocaml_all")))
           (replace 'install
             (lambda _
               (invoke "cmake" "-P" "bindings/ocaml/cmake_install.cmake"))))))
      (inputs
       (list llvm))
      (native-inputs
       (list ocaml ocaml-findlib ocaml-ounit python))
      (propagated-inputs
       (list ocaml-integers ocaml-ctypes))
      (synopsis "OCaml bindings to LLVM")
      (description "This package contains the OCaml bindings distributed with
LLVM."))))

(define-public ocaml-llvm (make-ocaml-llvm llvm))
(define-public ocaml-llvm-9 (make-ocaml-llvm llvm-9))
(define-public ocaml-llvm-10 (make-ocaml-llvm llvm-10))
(define-public ocaml-llvm-11 (make-ocaml-llvm llvm-11))

(define-public llvm-julia
  (package
    (inherit llvm-11)
    (name "llvm-julia")
    (properties `((hidden? . #t)
                  ,@(package-properties llvm-11)))
    (source (origin
              (inherit (package-source llvm-11))
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
                     '("llvm7-symver-jlprefix"
                       "00ng32x6xhm9czczirn5r1q1mc1myad44fqhi061hwh1vb46dwgm")
                     '("llvm-6.0-DISABLE_ABI_CHECKS"
                       "014fawd1ba7yckalypfld22zgic87x9nx3cim42zrwygywd36pyg")
                     '("llvm9-D50010-VNCoercion-ni"
                       "1s1d3sjsiq4vxg7ncy5cz56zgy5vcq6ls3iqaiqkvr23wyryqmdx")
                     '("llvm7-revert-D44485"
                       "0f59kq3p3mpwsbmskypbi4zn01l6ig0x7v2rjp08k2r8z8m6fa8n")
                     '("llvm-11-D75072-SCEV-add-type"
                       "176xi1lnbnv2rcs05ywhyb7pd0xgmibayvwzksksg44wg2dh8mbx")
                     '("llvm-julia-tsan-custom-as"
                       "0awh40kf6lm4wn1nsjd1bmhfwq7rqj811szanp2xkpspykw9hg9s")
                     '("llvm-D80101"
                       "1gsdwmgmpbignvqyxcnlprj899259p3dvdznlncd84ss445qgq3j")
                     '("llvm-D84031"
                       "0nks9sbk7p0r5gyr0idrmm93a5fmdai8kihz9532dx4zhcvvqbjc")
                     '("llvm-10-D85553"
                       "1zjq7j9q2qp56hwkc8yc8f0z7kvam3j7hj8sb7qyd77r255ff78l")
                     '("llvm-10-unique_function_clang-sa"
                       "1jys9w2zqk3dasnxqh0qz5ij7rxi6mkgq9pqjsclmamr5169zyan")
                     ;'("llvm-D88630-clang-cmake"
                     ;  "0rs6s71nqnjkny7i69gqazhqj5jqfdr0bkxs2v5a55sfx8fa1k54")
                     '("llvm-11-D85313-debuginfo-empty-arange"
                       "1f672d5385xpgb8yrim8d3b7wg2z1l81agnshm1q61kdvjixqx32")
                     '("llvm-11-D90722-rtdyld-absolute-relocs"
                       "0kmnai229yyxkmpk9lxd180mcnhk2i8d87k2sg89gc8as18w10r6")
                     '("llvm-invalid-addrspacecast-sink"
                       "1n1b7j4s80vj7x5377aj9vyphmxx1q6bm0chhkxp6zsy3mx3g2ry")
                     '("llvm-11-D92906-ppc-setjmp"
                       "0cmd4dsblp7a8m03j16dqxws0ijh55zf4jzzxmj341qxa1gamdp9")
                     '("llvm-11-PR48458-X86ISelDAGToDAG"
                       "0vwzvlhsdazhxg4gj8g2f00a4f8qc5cgac23w575xk3pgba1jh6y")
                     '("llvm-11-D93092-ppc-knownbits"
                       "1748bypsc7c9lbs3fnkv0kwvch6bn85kj98j4jdaz254ig0wa6xj")
                     '("llvm-11-D93154-globalisel-as"
                       "1k5wd4z3pa7zj0gyjkif7viqj906dhqlbb7dc95gig40nbxv6zpj")
                     '("llvm-11-ppc-half-ctr"
                       "0piywisfz6cmw3133kz7vzhiqflq2y7igakqxlym0gi8pqylv7w9")
                     '("llvm-11-ppc-sp-from-bp"
                       "1wmg3485cx5f9pbykyl3jibk1wwv4w1x30hl4jyfndzr2yh8azf9")
                     '("llvm-rGb498303066a6-gcc11-header-fix"
                       "0hkd4rwhvh8g2yh13g29wiwnjpv2yd1hdyiv1ryw8izl25bz9c67")
                     '("llvm-11-D94813-mergeicmps"
                       "0cmy0ywkgyrdcvr9bd6pd912lyd4gcsrib4z0v05dwgcdxhk7y29")
                     '("llvm-11-D94980-CTR-half"
                       "1yf8cxib3z8hz7zi9n6v2g2c6vpfr4slq9hpx8m8yq8f1jbyw3fw")
                     '("llvm-11-D94058-sext-atomic-ops"
                       "1x6p6k6q651z5jcqxx8vj17cxnv196mka7mwn7dpp6c23lwgfdpb")
                     '("llvm-11-D96283-dagcombine-half"
                       "0lv4iq2f8qrcz1xyxfic3bcr5p0aqam3a7c6pp6fnw3riixm096k"))))
              (patch-flags '("-p1"))))
    (arguments
     (substitute-keyword-arguments (package-arguments llvm-11)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'patch-round-two
             ;; We have to do the patching in two rounds because we can't
             ;; pass '-p1' and '-p2' in the source field.
             (lambda* (#:key inputs #:allow-other-keys)
               (map (lambda (patchname)
                      (invoke "patch" patchname "-p2"))
                    (list "llvm-11-AArch64-FastIsel-bug"
                          "llvm-11-D97435-AArch64-movaddrreg"
                          "llvm-11-D97571-AArch64-loh"
                          "llvm-11-aarch64-addrspace"))))))
       ((#:build-type _) "Release")
       ((#:configure-flags flags)
        `(list
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
           "-DLLVM_LINK_LLVM_DYLIB=ON"
           "-DLLVM_VERSION_SUFFIX:STRING=jl"))))
    (inputs
     (append
       (package-inputs llvm-11)
       `(("llvm-11-AArch64-FastIsel-bug"
          ,(julia-patch "llvm-11-AArch64-FastIsel-bug"
                        "1m2vddj1mw4kbij8hbrx82piyy6bvr2x7wwdnlxfaqcm72ipzyh9"))
         ("llvm-11-D97435-AArch64-movaddrreg"
          ,(julia-patch "llvm-11-D97435-AArch64-movaddrreg"
                        "10jnavq9ljkj7j2gqj2zd1pwqpqb5zs3zp9h96pmz0djbmxwa86y"))
         ("llvm-11-D97571-AArch64-loh"
          ,(julia-patch "llvm-11-D97571-AArch64-loh"
                        "128zcbg1w1j7hngsf7z1a7alc6lig6l2rqgjp6i8nk3k3f842v6n"))
         ("llvm-11-aarch64-addrspace"
          ,(julia-patch "llvm-11-aarch64-addrspace"
                        "0ckbzgfirxrf2d5bpinpngp7gnilbjrk0cbdfyl3h6f5v6i6xj6m")))))))

(define %cling-version "0.9")

(define llvm-cling             ;LLVM 9 with approximately 10 patches for cling
  (let ((base llvm-9))
    (package/inherit base
      (name "llvm-cling")
      (source
       (origin
         (inherit (package-source base))
         (method git-fetch)
         (uri (git-reference
               (url "http://root.cern/git/llvm.git")
               (commit (string-append "cling-v" %cling-version))))
         (file-name (git-file-name "llvm-cling" %cling-version))
         (sha256
          (base32
           "0y3iwv3c9152kybmdrwvadggjs163r25h7rmlxzr3hfpr463pnwf"))
         (modules '((guix build utils)))
         (snippet
          ;; The source is missing an include directive (see:
          ;; https://github.com/vgvassilev/cling/issues/219).
          '(substitute* "utils/benchmark/src/benchmark_register.h"
             (("^#include <vector>.*" all)
              (string-append all "#include <limits>\n"))))))
      (outputs '("out"))
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:configure-flags _ ''())
          '(list "-DLLVM_PARALLEL_LINK_JOBS=1" ;cater to smaller build machines
                 ;; Only enable compiler support for the host architecture to
                 ;; save on build time.
                 "-DLLVM_TARGETS_TO_BUILD=host;NVPTX"
                 "-DLLVM_INSTALL_UTILS=ON"
                 "-DLLVM_ENABLE_RTTI=ON"
                 "-DLLVM_ENABLE_FFI=ON"
                 "-DLLVM_BUILD_LLVM_DYLIB=ON"
                 "-DLLVM_LINK_LLVM_DYLIB=ON"))
         ((#:phases phases '%standard-phases)
          `(modify-phases ,phases
             (delete 'shared-lib-workaround)
             (delete 'install-opt-viewer))))))))

(define clang-cling-runtime
  (let ((base clang-runtime-9))
    (package/inherit base
      (name "clang-cling-runtime")
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases '%standard-phases)
          `(modify-phases ,phases
             (add-after 'install 'delete-static-libraries
               ;; This reduces the size from 22 MiB to 4 MiB.
               (lambda* (#:key outputs #:allow-other-keys)
                 (let ((out (assoc-ref outputs "out")))
                   (for-each delete-file (find-files out "\\.a$")))))))))
      (inputs (modify-inputs (package-inputs base)
                (replace "llvm" llvm-cling))))))

(define clang-cling              ;modified clang 9 with ~ 60 patches for cling
  (let ((base clang-9))
    (package/inherit base
      (name "clang-cling")
      (source
       (origin
         (inherit (package-source base))
         (method git-fetch)
         (uri (git-reference
               (url "http://root.cern/git/clang.git")
               (commit (string-append "cling-v" %cling-version))))
         (file-name (git-file-name "clang-cling" %cling-version))
         (sha256
          (base32
           "128mxkwghss6589wvm6amzv183aq88rdrnfxjiyjcji5hx84vpby"))))
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases '%standard-phases)
          `(modify-phases ,phases
             (add-after 'install 'delete-static-libraries
               ;; This reduces the size by half, from 220 MiB to 112 MiB.
               (lambda* (#:key outputs #:allow-other-keys)
                 (let ((out (assoc-ref outputs "out")))
                   (for-each delete-file (find-files out "\\.a$")))))))))
      (propagated-inputs
       (modify-inputs (package-propagated-inputs base)
         (replace "llvm" llvm-cling)
         (replace "clang-runtime" clang-cling-runtime))))))

(define-public cling
  ;; The tagged v0.9 release doesn't build, so use the latest commit.
  (let ((commit "d78d1a03fedfd2bf6d2b6ff295aca576d98940df")
        (revision "1")
        (version* "0.9"))
    (package
      (name "cling")
      (version (git-version version* revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "http://root.cern/git/cling.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0lsbxv21b4qw11xkw9iipdpca64jjwwqxm0qf5v2cgdlibf8m8n9"))
                ;; Patch submitted upstream here:
                ;; https://github.com/root-project/cling/pull/433.
                (patches (search-patches "cling-use-shared-library.patch"))))
      (build-system cmake-build-system)
      (arguments
       `(#:build-type "Release"         ;keep the build as lean as possible
         #:tests? #f                    ;FIXME: 78 tests fail (out of ~200)
         #:test-target "check-cling"
         #:configure-flags
         (list (string-append "-DCLING_CXX_PATH="
                              (assoc-ref %build-inputs "gcc") "/bin/g++")
               ;; XXX: The AddLLVM.cmake module expects LLVM_EXTERNAL_LIT to
               ;; be a Python script, not a shell executable.
               (string-append "-DLLVM_EXTERNAL_LIT="
                              (assoc-ref %build-inputs "python-lit")
                              "/bin/.lit-real"))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'set-version
             (lambda _
               (make-file-writable "VERSION")
               (call-with-output-file "VERSION"
                 (lambda (port)
                   (format port "~a~%" ,version)))))
           (add-after 'unpack 'patch-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "lib/Interpreter/CIFactory.cpp"
                 (("\bsed\b")
                  (which "sed"))
                 ;; This ensures that the default C++ library used by Cling is
                 ;; that of the compiler that was used to build it, rather
                 ;; than that of whatever g++ happens to be on PATH.
                 (("ReadCompilerIncludePaths\\(CLING_CXX_RLTV")
                  (string-append "ReadCompilerIncludePaths(\""
                                 (assoc-ref inputs "gcc") "/bin/g++\""))
                 ;; Cling uses libclang's CompilerInvocation::GetResourcesPath
                 ;; to resolve Clang's library prefix, but this fails on Guix
                 ;; because it is relative to the output of cling rather than
                 ;; clang (see:
                 ;; https://github.com/root-project/cling/issues/434).  Fully
                 ;; shortcut the logic in this method to return the correct
                 ;; static location.
                 (("static std::string getResourceDir.*" all)
                  (string-append all
                                 "    return std::string(\""
                                 (assoc-ref inputs "clang-cling")
                                 "/lib/clang/" ,(package-version clang-cling)
                                 "\");")))
               ;; Check for the 'lit' command for the tests, not 'lit.py'
               ;; (see: https://github.com/root-project/cling/issues/432).
               (substitute* "CMakeLists.txt"
                 (("lit.py")
                  "lit"))))
           (add-after 'unpack 'adjust-lit.cfg
             ;; See: https://github.com/root-project/cling/issues/435.
             (lambda _
               (substitute* "test/lit.cfg"
                 (("config.llvm_tools_dir \\+ '")
                  "config.cling_obj_root + '/bin"))))
           (add-after 'install 'delete-static-libraries
             ;; This reduces the size from 17 MiB to 5.4 MiB.
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (for-each delete-file (find-files out "\\.a$"))))))))
      (native-inputs
       (list python python-lit))
      (inputs
       (list clang-cling llvm-cling))
      (home-page "https://root.cern/cling/")
      (synopsis "Interactive C++ interpreter")
      (description "Cling is an interactive C++17 standard compliant
interpreter, built on top of LLVM and Clang.  Cling can be used as a
read-eval-print loop (REPL) to assist with rapid application development.
Here's how to print @samp{\"Hello World!\"} using @command{cling}:

@example
cling '#include <stdio.h>' 'printf(\"Hello World!\\n\");'
@end example")
      (license license:lgpl2.1+))))     ;for the combined work
