;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016 Roel Janssen <roel@gnu.org>
;;; Copyright © 2015, 2018 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2017 Frederick Muriithi <fredmanglis@gmail.com>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Guy Fleury Iteriteka <gfleury@disroot.org>
;;; Copyright © 2021 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages dlang)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module ((guix build utils) #:hide (delete which))
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-1))

(define-public rdmd
  (package
    (name "rdmd")
    (version "2.077.1")
    (source (origin
      (method url-fetch)
      (uri (string-append "https://github.com/dlang/tools/archive/v" version ".tar.gz"))
      (file-name (string-append name "-" version ".tar.gz"))
      (sha256
       (base32
        "0c8w373rv6iz3xfid94w40ncv2lr2ncxi662qsr4lda4aghczmq7"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'check) ; There is no Makefile, so there's no 'make check'.
         (replace
          'build
          (lambda _
            (invoke "ldc2" "rdmd.d")))
         (replace
          'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
              (install-file "rdmd" bin)))))))
    (native-inputs
     (list ldc))
    (home-page "https://github.com/D-Programming-Language/tools/")
    (synopsis "Specialized equivalent to 'make' for the D language")
    (description
     "rdmd is a companion to the dmd compiler that simplifies the typical
edit-compile-link-run or edit-make-run cycle to a rapid edit-run cycle.  Like
make and other tools, rdmd uses the relative dates of the files involved to
minimize the amount of work necessary.  Unlike make, rdmd tracks dependencies
and freshness without requiring additional information from the user.")
    (license license:boost1.0)))

;;; The 0.17.6 version is the last release to support being bootstrapped
;;; without a D compiler (requiring only a C++ compiler).
;;; TODO: Bootstrap ldc from GDC (the D frontend for GCC).
(define ldc-bootstrap-0.17
  (package
    (name "ldc")
    (version "0.17.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ldc-developers/ldc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1q6hm4fkrcwys83x0p4kfg9xrc1b9g2qicqif2zy5z4nsfsb5vgs"))))
    (build-system cmake-build-system)
    (supported-systems '("x86_64-linux" "i686-linux" "armhf-linux"))
    (properties
     ;; Some of the tests take a very long time on ARMv7.  See
     ;; <https://lists.gnu.org/archive/html/guix-devel/2018-02/msg00312.html>.
     `((max-silent-time . ,(* 3600 3))))
    (arguments
     `(#:tests? #f               ;requires obsolete python-lit test dependency
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-submodule-sources
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((unpack (lambda (input target)
                             (let ((source (assoc-ref inputs input)))
                               ;; Git checkouts are directories as long as
                               ;; there are no patches; tarballs otherwise.
                               (if (file-is-directory? source)
                                   (copy-recursively source target)
                                   (with-directory-excursion target
                                     (invoke "tar" "xvf" source
                                             "--strip-components=1")))))))
               (unpack "phobos-src" "runtime/phobos")
               (unpack "druntime-src" "runtime/druntime")
               (unpack "dmd-testsuite-src" "tests/d2/dmd-testsuite"))))
         (add-after 'unpack-submodule-sources 'patch-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "runtime/phobos/std/process.d"
               (("/bin/sh") (which "sh"))
               (("echo") (which "echo")))
             (substitute* "runtime/phobos/std/datetime.d"
               (("/usr/share/zoneinfo/")
                (string-append (assoc-ref inputs "tzdata") "/share/zoneinfo"))
               (("tzName == \"[+]VERSION\"")
                "(tzName == \"+VERSION\" || \
std.algorithm.endsWith(tzName, \"/leapseconds\"))")))))))
    (inputs
     `(("libconfig" ,libconfig)
       ("libedit" ,libedit)
       ("tzdata" ,tzdata)
       ("zlib" ,zlib)))
    (native-inputs
     `(("llvm" ,llvm-6)
       ("python-wrapper" ,python-wrapper)
       ("unzip" ,unzip)
       ("phobos-src"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/ldc-developers/phobos")
                 (commit (string-append "ldc-v" version))))
           (file-name (git-file-name "phobos" version))
           (sha256
            (base32 "15jzs38wanks2jfp2izzl7zqrp4c8ai54ppsgm8ws86p3sbbkmj8"))))
       ("druntime-src"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/ldc-developers/druntime")
                 (commit (string-append "ldc-v" version))))
           (file-name (git-file-name "druntime" version))
           (sha256
            (base32 "00wr2kiggwnd8h7by51fhj1xc65hv1ysip5gbgdbkfar58p2d0bb"))))
       ("dmd-testsuite-src"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/ldc-developers/dmd-testsuite")
                 (commit (string-append "ldc-v" version))))
           (file-name (git-file-name "dmd-testsuite" version))
           (sha256
            (base32 "1d1c0979wbippldrkjf7szyj4n87hxz8dwqg1r5b3aai37g9kcky"))))))
    (home-page "http://wiki.dlang.org/LDC")
    (synopsis "LLVM-based compiler for the D programming language")
    (description
     "LDC is an LLVM compiler for the D programming language.  It is based on
the latest DMD compiler that was written in C and is used for
bootstrapping more recent compilers written in D.")
    ;; Most of the code is released under BSD-3, except for code originally
    ;; written for GDC, which is released under GPLv2+, and the DMD frontend,
    ;; which is released under the "Boost Software License version 1.0".
    (license (list license:bsd-3
                   license:gpl2+
                   license:boost1.0))))

;;; This is the last version that supports being built with 32 bit machines
;;; from 0.17.
(define ldc-bootstrap-1.12
  (package
    (inherit ldc-bootstrap-0.17)
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       ;; The official release include the matching source code releases of
       ;; phobos, druntime and dmd-testsuite.
       (uri (string-append "https://github.com/ldc-developers/ldc/releases"
                           "/download/v" version "/ldc-" version "-src.tar.gz"))
       (sha256
        (base32 "1fdma1w8j37wkr0pqdar11slkk36qymamxnk6d9k8ybhjmxaaawm"))))
    (arguments
     (substitute-keyword-arguments (package-arguments ldc-bootstrap-0.17)
       ((#:build-type _ #f) "Release")
       ((#:configure-flags _ #f)
        `(list "-GNinja"))
       ((#:make-flags _ #f)             ;used as build targets
        `(list "all"))
       ((#:tests? _) #f)
       ((#:phases phases)
        `(modify-phases ,phases
           (delete 'unpack-submodule-sources)
           (replace 'patch-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* '("runtime/phobos/std/process.d")
                 (("/bin/sh") (which "sh"))
                 (("echo") (which "echo")))))
           (replace 'build
             ;; Building with Make would result in "make: *** [Makefile:166:
             ;; all] Error 2".
             (lambda* (#:key make-flags parallel-tests? #:allow-other-keys)
               (let ((job-count (number->string (or (and parallel-tests?
                                                         (parallel-job-count))
                                                    1))))
                 (apply invoke "cmake" "--build" "." "-j" job-count
                        "--target" make-flags))))
           (replace 'install
             (lambda _
               (invoke "cmake" "--install" ".")))))))
    (native-inputs
     ;; Importing (gnu packages commencement) would introduce a cycle.
     `(("ld-gold-wrapper" ,(module-ref (resolve-interface
                                        '(gnu packages commencement))
                                       'ld-gold-wrapper))
       ("llvm" ,llvm-6)
       ("ldc" ,ldc-bootstrap-0.17)
       ("ninja" ,ninja)
       ("python-wrapper" ,python-wrapper)
       ("unzip" ,unzip)))))

;;; For 32 bits systems, 1.12 cannot build 1.27 directly, so we need another
;;; hop.
(define ldc-bootstrap-1.24
  (package
    (inherit ldc-bootstrap-1.12)
    (version "1.24.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/ldc-developers/ldc/releases"
                           "/download/v" version "/ldc-" version "-src.tar.gz"))
       (sha256
        (base32 "0g5svf55i0kq55q49awmwqj9qi1n907cyrn1vjdjgs8nx6nn35gx"))))
    (native-inputs
     (fold alist-replace
           (package-native-inputs ldc-bootstrap-1.12)
           '("ldc" "llvm")
           `((,ldc-bootstrap-1.12) (,llvm-11))))))

(define ldc-bootstrap-1.27
  (package
    (inherit ldc-bootstrap-1.24)
    (version "1.27.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/ldc-developers/ldc/releases"
                           "/download/v" version "/ldc-" version "-src.tar.gz"))
       (sha256
        (base32 "1775001ba6n8w46ln530kb5r66vs935ingnppgddq8wqnc0gbj4k"))))
    (native-inputs
     (fold alist-replace
           (package-native-inputs ldc-bootstrap-1.24)
           '("ldc" "llvm")
           `((,ldc-bootstrap-1.24) (,llvm-11))))))

(define-public ldc
  (package
    (inherit ldc-bootstrap-1.27)
    (arguments
     (substitute-keyword-arguments (package-arguments ldc-bootstrap-1.27)
       ((#:make-flags _ #f)
        '(list "all"
               ;; Also build the test runner binaries.
               "ldc2-unittest" "all-test-runners"))
       ((#:configure-flags flags)
        `(,@flags "-DBUILD_SHARED_LIBS=ON"
                  "-DLDC_LINK_MANUALLY=OFF"))
       ((#:tests? _) #t)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'fix-compiler-rt-library-discovery
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((clang-runtime (assoc-ref inputs "clang-runtime"))
                     (system ,(or (%current-target-system)
                                  (%current-system))))
                 (define (gnu-triplet->clang-arch system)
                   (letrec-syntax
                       ((matches (syntax-rules (=>)
                                   ((_ (system-prefix => target) rest ...)
                                    (if (string-prefix? system-prefix system)
                                        target
                                        (matches rest ...)))
                                   ((_)
                                    (error "Clang target for system is unknown"
                                           system)))))
                     (matches ("x86_64"      => "x86_64")
                              ("i686"        => "i386")
                              ("armhf"       => "armhf"))))
                 ;; Coax LLVM into agreeing with Clang about system target
                 ;; naming.
                 (substitute* "driver/linker-gcc.cpp"
                   (("triple.getArchName\\(\\)")
                    (format #f "~s" (gnu-triplet->clang-arch system))))
                 ;; Augment the configuration of the ldc2 binaries so they can
                 ;; find the compiler-rt libraries they need to be linked with
                 ;; for the tests.
                 (substitute* (find-files "." "^ldc2.*\\.conf\\.in$")
                   ((".*lib-dirs = \\[\n" all)
                    (string-append all
                                   "        \"" clang-runtime
                                   "/lib/linux\",\n"))))))
           (add-after 'unpack 'patch-paths-in-tests
             (lambda _
               (substitute* "tests/d2/dmd-testsuite/Makefile"
                 (("/bin/bash") (which "bash")))
               (substitute* "tests/linking/linker_switches.d"
                 (("echo") (which "echo")))
               (substitute* "tests/d2/dmd-testsuite/dshell/test6952.d"
                 (("/usr/bin/env bash")
                  (which "bash")))))
           (add-after 'unpack 'disable-problematic-tests
             (lambda* (#:key inputs #:allow-other-keys)
               ;; Disable unittests in the following files.
               (substitute* '("runtime/phobos/std/net/curl.d"
                              "runtime/phobos/std/datetime/systime.d"
                              "runtime/phobos/std/datetime/timezone.d")
                 (("version(unittest)") "version(skipunittest)")
                 ((" unittest") " version(skipunittest) unittest"))
               ;; The following tests plugins we don't have.
               (delete-file "tests/plugins/addFuncEntryCall/testPlugin.d")
               ;; The following tests requires AVX instruction set in the CPU.
               (substitute* "tests/d2/dmd-testsuite/runnable/cdvecfill.sh"
                 (("^// DISABLED: ") "^// DISABLED: linux64 "))
               ;; This unit test requires networking, fails with
               ;; "core.exception.RangeError@std/socket.d(778): Range
               ;; violation".
               (substitute* "runtime/phobos/std/socket.d"
                 (("assert\\(ih.addrList\\[0\\] == 0x7F_00_00_01\\);.*")
                  ""))
               ;; The GDB tests suite fails; there are a few bug reports about
               ;; it upstream.
               (for-each delete-file (find-files "tests" "gdb.*\\.(d|sh)$"))
               (delete-file "tests/d2/dmd-testsuite/runnable/debug_info.d")
               (delete-file "tests/d2/dmd-testsuite/runnable/b18504.d")
               (substitute* "runtime/druntime/test/exceptions/Makefile"
                 ((".*TESTS\\+=rt_trap_exceptions_drt_gdb.*")
                  ""))
               ;; The following tests fail on the supported 32 bit systems,
               ;; which are not tested upstream.
               (with-directory-excursion "tests"
                 (let ((system ,(or (%current-target-system)
                                    (%current-system))))
                   (when (or (string-prefix? "armhf" system )
                             (string-prefix? "i686" system ))
                     (for-each delete-file
                               '("PGO/profile_rt_calls.d"
                                 "codegen/mangling.d"
                                 "debuginfo/print_gdb.d"
                                 "dynamiccompile/bind.d"
                                 "dynamiccompile/bind_bool.d"
                                 "dynamiccompile/bind_func_opt.d"
                                 "dynamiccompile/bind_nested_opt.d"
                                 "dynamiccompile/bind_opt.d"
                                 "dynamiccompile/compiler_context.d"
                                 "dynamiccompile/compiler_context_parallel.d"
                                 "instrument/xray_check_pipeline.d"
                                 "instrument/xray_link.d"
                                 "instrument/xray_simple_execution.d"
                                 "sanitizers/msan_noerror.d"
                                 "sanitizers/msan_uninitialized.d"
                                 "d2/dmd-testsuite/runnable_cxx/cppa.d")))))))
           (add-before 'configure 'set-cc-and-cxx-to-use-clang
             ;; The tests require to be built with Clang; build everything
             ;; with it, for simplicity.
             (lambda _
               (setenv "CC" (which "clang"))
               (setenv "CXX" (which "clang++"))))
           (replace 'check
             (lambda* (#:key tests? parallel-tests? #:allow-other-keys)
               (when tests?
                 (let ((job-count (number->string
                                   (or (and parallel-tests?
                                            (parallel-job-count))
                                       1))))
                   ;; The test targets are tested separately to provide
                   ;; finer-grained diagnostics (see:
                   ;; https://raw.githubusercontent.com/ldc-developers/
                   ;; ldc/master/.azure-pipelines/3-posix-test.yml)
                   (display "running the ldc2 unit tests...\n")
                   (invoke "ctest" "--output-on-failure" "-j" job-count
                           "-R" "ldc2-unittest")
                   (display "running the lit test suite...\n")
                   (invoke "ctest" "--output-on-failure" "-j" job-count
                           "-R" "lit-tests")
                   (display "running the dmd test suite...\n")
                   (invoke "ctest" "--output-on-failure" "-j" job-count
                           "-R" "dmd-testsuite")
                   (display "running the defaultlib unit tests and druntime \
integration tests...\n")
                   (invoke "ctest" "--output-on-failure" "-j" job-count
                           "-E" "dmd-testsuite|lit-tests|ldc2-unittest")))))))))
    (native-inputs
     (append (delete "llvm"
                     (alist-replace "ldc" (list ldc-bootstrap-1.27)
                                    (package-native-inputs ldc-bootstrap-1.27)))
         `(("clang" ,clang-11)          ;propagates llvm and clang-runtime
           ("python-lit" ,python-lit))))))

(define-public dub
  (package
    (name "dub")
    (version "1.7.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dlang/dub")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "073ibvgm1gphcqs1yjrav9ryp677nh3b194nxmvicwgvdc0sb6w9"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; it would have tested itself by installing some packages (vibe etc)
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (replace 'build
           (lambda _
             (invoke "./build.sh")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "bin/dub" bin)
               #t))))))
    (inputs
     (list curl))
    (native-inputs
     (list ldc))
    (home-page "https://code.dlang.org/getting_started")
    (synopsis "Package and build manager for D projects")
    (description
     "DUB is a package and build manager for applications and
libraries written in the D programming language.  It can
automatically retrieve a project's dependencies and integrate
them in the build process.

The design emphasis is on maximum simplicity for simple projects,
while providing the opportunity to customize things when
needed.")
    (license license:expat)))

(define-public gtkd
  (package
    (name "gtkd")
    (version "3.9.0")
    (source
     (origin
      (method url-fetch/zipbomb)
      (uri (string-append "https://gtkd.org/Downloads/sources/GtkD-"
                          version ".zip"))
      (sha256
       (base32 "0qv8qlpwwb1d078pnrf0a59vpbkziyf53cf9p6m8ms542wbcxllp"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("unzip" ,unzip)
       ("ldc" ,ldc)
       ("pkg-config" ,pkg-config)
       ("xorg-server-for-tests" ,xorg-server-for-tests)))
    (arguments
     `(#:test-target "test"
       #:make-flags
       `("DC=ldc2"
         ,(string-append "prefix=" (assoc-ref %outputs "out"))
         ,(string-append "libdir=" (assoc-ref %outputs "out")
                         "/lib"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'patch-makefile
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "GNUmakefile"
               ;; We do the tests ourselves.
               (("default-goal: libs test") "default-goal: libs")
               (("all: libs shared-libs test") "all: libs shared-libs")
               ;; Work around upstream bug.
               (("\\$\\(prefix\\)\\/\\$\\(libdir\\)") "$(libdir)"))
             #t))
         (add-before 'check 'prepare-x
           (lambda _
             (system "Xvfb :1 &")
             (setenv "DISPLAY" ":1")
             #t)))))
    (home-page "https://gtkd.org/")
    (synopsis "D binding and OO wrapper of GTK+")
    (description "This package provides bindings to GTK+ for D.")
    (license license:lgpl2.1)))
