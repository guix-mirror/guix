;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016 Eric Le Bihan <eric.le.bihan.dev@free.fr>
;;; Copyright © 2016 ng0 <ng0@n0.is>
;;; Copyright © 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017, 2018 Nikolai Merinov <nikolai.merinov@member.fsf.org>
;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Danny Milosavljevic <dannym+a@scratchpost.org>
;;; Copyright © 2019 Ivan Petkov <ivanppetkov@gmail.com>
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

(define-module (gnu packages rust)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module ((guix build utils) #:select (alist-replace))
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26))

(define %cargo-reference-project-file "/dev/null")
(define %cargo-reference-hash
  "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")

(define* (nix-system->gnu-triplet-for-rust
          #:optional (system (%current-system)))
  (match system
    ("x86_64-linux"   "x86_64-unknown-linux-gnu")
    ("i686-linux"     "i686-unknown-linux-gnu")
    ("armhf-linux"    "armv7-unknown-linux-gnueabihf")
    ("aarch64-linux"  "aarch64-unknown-linux-gnu")
    ("mips64el-linux" "mips64el-unknown-linux-gnuabi64")
    (_                (nix-system->gnu-triplet system))))


(define* (rust-source version hash #:key (patches '()))
  (origin
    (method url-fetch)
    (uri (string-append "https://static.rust-lang.org/dist/"
                        "rustc-" version "-src.tar.gz"))
    (sha256 (base32 hash))
    (modules '((guix build utils)))
    (snippet '(begin (delete-file-recursively "src/llvm") #t))
    (patches (map search-patch patches))))

(define* (rust-bootstrapped-package base-rust version checksum
                                    #:key (patches '()))
  "Bootstrap rust VERSION with source checksum CHECKSUM patched with PATCHES using BASE-RUST."
  (package
    (inherit base-rust)
    (version version)
    (source
     (rust-source version checksum #:patches patches))
    (native-inputs
     (alist-replace "cargo-bootstrap" (list base-rust "cargo")
                    (alist-replace "rustc-bootstrap" (list base-rust)
                                   (package-native-inputs base-rust))))))

(define-public mrustc
  (let ((rustc-version "1.19.0"))
    (package
      (name "mrustc")
      (version "0.8.0")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/thepowersgang/mrustc.git")
                      (commit (string-append "v" version))))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0a7v8ccyzp1sdkwni8h1698hxpfz2sxhcpx42n6l2pbm0rbjp08i"))
                (patches
                 (search-patches "mrustc-0.8.0-fix-variable-length-integer-receiving.patch"))))
      (outputs '("out" "cargo"))
      (build-system gnu-build-system)
      (inputs
       `(("llvm" ,llvm-3.9.1)))
      (native-inputs
       `(("bison" ,bison)
         ("flex" ,flex)
         ;; Required for the libstd sources.
         ("rustc"
          ,(rust-source "1.19.0" "0l8c14qsf42rmkqy92ahij4vf356dbyspxcips1aswpvad81y8qm"))))
      (arguments
       `(#:test-target "local_tests"
         #:make-flags (list (string-append "LLVM_CONFIG="
                                           (assoc-ref %build-inputs "llvm")
                                           "/bin/llvm-config"))
         #:phases
         (modify-phases %standard-phases
          (add-after 'unpack 'patch-date
            (lambda _
              (substitute* "Makefile"
               (("shell date") "shell date -d @1"))
              #t))
           (add-after 'patch-date 'unpack-target-compiler
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (substitute* "minicargo.mk"
                 ;; Don't try to build LLVM.
                 (("^[$][(]LLVM_CONFIG[)]:") "xxx:")
                 ;; Build for the correct target architecture.
                 (("^RUSTC_TARGET := x86_64-unknown-linux-gnu")
                  (string-append "RUSTC_TARGET := "
                                 ,(or (%current-target-system)
                                      (nix-system->gnu-triplet-for-rust)))))
               (invoke "tar" "xf" (assoc-ref inputs "rustc"))
               (chdir "rustc-1.19.0-src")
               (invoke "patch" "-p0" "../rust_src.patch")
               (chdir "..")
               #t))
           (replace 'configure
             (lambda* (#:key inputs #:allow-other-keys)
               (setenv "CC" (string-append (assoc-ref inputs "gcc") "/bin/gcc"))
               #t))
           (add-after 'build 'build-minicargo
             (lambda _
               (for-each (lambda (target)
                           (invoke "make" "-f" "minicargo.mk" target))
                         '("output/libstd.hir" "output/libpanic_unwind.hir"
                           "output/libproc_macro.hir" "output/libtest.hir"))
               ;; Technically the above already does it - but we want to be clear.
               (invoke "make" "-C" "tools/minicargo")))
           (replace 'install
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin"))
                      (tools-bin (string-append out "/tools/bin"))
                      (cargo-out (assoc-ref outputs "cargo"))
                      (cargo-bin (string-append cargo-out "/bin"))
                      (lib (string-append out "/lib"))
                      (lib/rust (string-append lib "/mrust"))
                      (gcc (assoc-ref inputs "gcc")))
                 ;; These files are not reproducible.
                 (for-each delete-file (find-files "output" "\\.txt$"))
                 (delete-file-recursively "output/local_tests")
                 (mkdir-p lib)
                 (copy-recursively "output" lib/rust)
                 (mkdir-p bin)
                 (mkdir-p tools-bin)
                 (install-file "bin/mrustc" bin)
                 ;; minicargo uses relative paths to resolve mrustc.
                 (install-file "tools/bin/minicargo" tools-bin)
                 (install-file "tools/bin/minicargo" cargo-bin)
                 #t))))))
      (synopsis "Compiler for the Rust progamming language")
      (description "Rust is a systems programming language that provides memory
safety and thread safety guarantees.")
      (home-page "https://github.com/thepowersgang/mrustc")
      ;; Dual licensed.
      (license (list license:asl2.0 license:expat)))))

(define rust-1.19
  (package
    (name "rust")
    (version "1.19.0")
    (source (rust-source version "0l8c14qsf42rmkqy92ahij4vf356dbyspxcips1aswpvad81y8qm"
            #:patches '("rust-1.19-mrustc.patch")))
    (outputs '("out" "cargo"))
    (properties '((timeout . 72000)               ;20 hours
                  (max-silent-time . 18000)))     ;5 hours (for armel)
    (arguments
     `(#:imported-modules ,%cargo-utils-modules ;for `generate-checksums'
       #:modules ((guix build utils) (ice-9 match) (guix build gnu-build-system))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-env
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Disable test for cross compilation support.
             (setenv "CFG_DISABLE_CROSS_TESTS" "1")
             (setenv "SHELL" (which "sh"))
             (setenv "CONFIG_SHELL" (which "sh"))
             (setenv "CC" (string-append (assoc-ref inputs "gcc") "/bin/gcc"))
             ;; guix llvm-3.9.1 package installs only shared libraries
             (setenv "LLVM_LINK_SHARED" "1")
             #t))
         (add-after 'unpack 'patch-cargo-tomls
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "src/librustc_errors/Cargo.toml"
               (("[[]dependencies[]]") "
[dependencies]
term = \"0.4.4\"
"))
             (substitute* "src/librustc/Cargo.toml"
               (("[[]dependencies[]]") "
[dependencies]
getopts = { path = \"../libgetopts\" }
"))
             (substitute* "src/librustdoc/Cargo.toml"
               (("[[]dependencies[]]") "
[dependencies]
test = { path = \"../libtest\" }
"))
             #t))
         (add-after 'unpack 'patch-tests
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((bash (assoc-ref inputs "bash")))
               (substitute* "src/libstd/process.rs"
                 ;; The newline is intentional.
                 ;; There's a line length "tidy" check in Rust which would
                 ;; fail otherwise.
                 (("\"/bin/sh\"") (string-append "\n\"" bash "/bin/sh\"")))
               (substitute* "src/libstd/net/tcp.rs"
                 ;; There is no network in build environment
                 (("fn connect_timeout_unroutable")
                  "#[ignore]\nfn connect_timeout_unroutable"))
               ;; <https://lists.gnu.org/archive/html/guix-devel/2017-06/msg00222.html>
               (substitute* "src/libstd/sys/unix/process/process_common.rs"
                (("fn test_process_mask") "#[allow(unused_attributes)]
    #[ignore]
    fn test_process_mask"))
               #t)))
         (add-after 'patch-tests 'patch-aarch64-test
           (lambda* _
             (substitute* "src/librustc_back/dynamic_lib.rs"
               ;; This test is known to fail on aarch64 and powerpc64le:
               ;; https://github.com/rust-lang/rust/issues/45410
               (("fn test_loading_cosine") "#[ignore]\nfn test_loading_cosine"))
             #t))
         (add-after 'patch-tests 'use-readelf-for-tests
           (lambda* _
             ;; nm doesn't recognize the file format because of the
             ;; nonstandard sections used by the Rust compiler, but readelf
             ;; ignores them.
             (substitute* "src/test/run-make/atomic-lock-free/Makefile"
               (("\tnm ")
                "\treadelf -c "))
             #t))
         (add-after 'patch-tests 'remove-unsupported-tests
           (lambda* _
             ;; Our ld-wrapper cannot process non-UTF8 bytes in LIBRARY_PATH.
             ;; <https://lists.gnu.org/archive/html/guix-devel/2017-06/msg00193.html>
             (delete-file-recursively "src/test/run-make/linker-output-non-utf8")
             #t))
         (add-after 'patch-source-shebangs 'patch-cargo-checksums
           (lambda* _
             (substitute* "src/Cargo.lock"
               (("(\"checksum .* = )\".*\"" all name)
                (string-append name "\"" ,%cargo-reference-hash "\"")))
             (for-each
              (lambda (filename)
                (use-modules (guix build cargo-utils))
                (delete-file filename)
                (let* ((dir (dirname filename)))
                  (display (string-append
                            "patch-cargo-checksums: generate-checksums for "
                            dir "\n"))
                  (generate-checksums dir ,%cargo-reference-project-file)))
              (find-files "src/vendor" ".cargo-checksum.json"))
             #t))
         ;; This phase is overridden by newer versions.
         (replace 'configure
           (const #t))
         ;; This phase is overridden by newer versions.
         (replace 'build
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((rustc-bootstrap (assoc-ref inputs "rustc-bootstrap")))
               (setenv "CFG_COMPILER_HOST_TRIPLE"
                ,(nix-system->gnu-triplet (%current-system)))
               (setenv "CFG_RELEASE" "")
               (setenv "CFG_RELEASE_CHANNEL" "stable")
               (setenv "CFG_LIBDIR_RELATIVE" "lib")
               (setenv "CFG_VERSION" "1.19.0-stable-mrustc")
               ; bad: (setenv "CFG_PREFIX" "mrustc") ; FIXME output path.
               (mkdir-p "output")
               (invoke (string-append rustc-bootstrap "/tools/bin/minicargo")
                       "src/rustc" "--vendor-dir" "src/vendor"
                       "--output-dir" "output/rustc-build"
                       "-L" (string-append rustc-bootstrap "/lib/mrust")
                       "-j" "1")
               (setenv "CFG_COMPILER_HOST_TRIPLE" #f)
               (setenv "CFG_RELEASE" #f)
               (setenv "CFG_RELEASE_CHANNEL" #f)
               (setenv "CFG_VERSION" #f)
               (setenv "CFG_PREFIX" #f)
               (setenv "CFG_LIBDIR_RELATIVE" #f)
               (invoke (string-append rustc-bootstrap "/tools/bin/minicargo")
                       "src/tools/cargo" "--vendor-dir" "src/vendor"
                       "--output-dir" "output/cargo-build"
                       "-L" "output/"
                       "-L" (string-append rustc-bootstrap "/lib/mrust")
                       "-j" "1")
               ;; Now use the newly-built rustc to build the libraries.
               ;; One day that could be replaced by:
               ;; (invoke "output/cargo-build/cargo" "build"
               ;;         "--manifest-path" "src/bootstrap/Cargo.toml"
               ;;         "--verbose") ; "--locked" "--frozen"
               ;; but right now, Cargo has problems with libstd's circular
               ;; dependencies.
               (mkdir-p "output/target-libs")
               (for-each (match-lambda
                          ((name . flags)
                            (write name)
                            (newline)
                            (apply invoke
                                   "output/rustc-build/rustc"
                                   "-C" (string-append "linker="
                                                       (getenv "CC"))
                                   ;; Required for libterm.
                                   "-Z" "force-unstable-if-unmarked"
                                   "-L" "output/target-libs"
                                   (string-append "src/" name "/lib.rs")
                                   "-o"
                                   (string-append "output/target-libs/"
                                                  (car (string-split name #\/))
                                                  ".rlib")
                                   flags)))
                         '(("libcore")
                           ("libstd_unicode")
                           ("liballoc")
                           ("libcollections")
                           ("librand")
                           ("liblibc/src" "--cfg" "stdbuild")
                           ("libunwind" "-l" "gcc_s")
                           ("libcompiler_builtins")
                           ("liballoc_system")
                           ("libpanic_unwind")
                           ;; Uses "cc" to link.
                           ("libstd" "-l" "dl" "-l" "rt" "-l" "pthread")
                           ("libarena")

                           ;; Test dependencies:

                           ("libgetopts")
                           ("libterm")
                           ("libtest")))
               #t)))
         ;; This phase is overridden by newer versions.
         (replace 'check
           (const #t))
         ;; This phase is overridden by newer versions.
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (target-system ,(or (%current-target-system)
                                        (nix-system->gnu-triplet
                                         (%current-system))))
                    (out-libs (string-append out "/lib/rustlib/"
                                             target-system "/lib")))
                                        ;(setenv "CFG_PREFIX" out)
               (mkdir-p out-libs)
               (copy-recursively "output/target-libs" out-libs)
               (install-file "output/rustc-build/rustc"
                             (string-append out "/bin"))
               (install-file "output/rustc-build/rustdoc"
                             (string-append out "/bin"))
               (install-file "output/cargo-build/cargo"
                             (string-append (assoc-ref outputs "cargo")
                                            "/bin")))
             #t)))))
    (build-system gnu-build-system)
    (native-inputs
     `(("bison" ,bison) ; For the tests
       ("cmake" ,cmake)
       ("flex" ,flex) ; For the tests
       ("gdb" ,gdb)   ; For the tests
       ("procps" ,procps) ; For the tests
       ("python-2" ,python-2)
       ("rustc-bootstrap" ,mrustc)
       ("cargo-bootstrap" ,mrustc "cargo")
       ("pkg-config" ,pkg-config) ; For "cargo"
       ("which" ,which)))
    (inputs
     `(("jemalloc" ,jemalloc-4.5.0)
       ("llvm" ,llvm-3.9.1)
       ("openssl" ,openssl)
       ("libssh2" ,libssh2) ; For "cargo"
       ("libcurl" ,curl)))  ; For "cargo"

    ;; rustc invokes gcc, so we need to set its search paths accordingly.
    ;; Note: duplicate its value here to cope with circular dependencies among
    ;; modules (see <https://bugs.gnu.org/31392>).
    (native-search-paths
     (list (search-path-specification
            (variable "C_INCLUDE_PATH")
            (files '("include")))
           (search-path-specification
            (variable "CPLUS_INCLUDE_PATH")
            (files '("include")))
           (search-path-specification
            (variable "LIBRARY_PATH")
            (files '("lib" "lib64")))))

    (synopsis "Compiler for the Rust progamming language")
    (description "Rust is a systems programming language that provides memory
safety and thread safety guarantees.")
    (home-page "https://www.rust-lang.org")
    ;; Dual licensed.
    (license (list license:asl2.0 license:expat))))

(define-public rust-1.20
  (let ((base-rust
         (rust-bootstrapped-package rust-1.19 "1.20.0"
          "0542y4rnzlsrricai130mqyxl8r6rd991frb4qsnwb27yigqg91a")))
    (package
      (inherit base-rust)
      (outputs '("out" "doc" "cargo"))
      ;; Since rust-1.19 is local, it's quite probable that Hydra
      ;; will build rust-1.19 only as a dependency of rust-1.20.
      ;; But then Hydra will use the wrong properties, the ones here,
      ;; for rust-1.19.  Therefore, we copied the properties of
      ;; rust-1.19 here.
      (properties '((timeout . 72000)               ;20 hours
                    (max-silent-time . 18000)))     ;5 hours (for armel)
      (arguments
       (substitute-keyword-arguments (package-arguments rust-1.19)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'patch-tests 'patch-cargo-tests
               (lambda _
                 (substitute* "src/tools/cargo/tests/build.rs"
                  (("/usr/bin/env") (which "env"))
                  ;; Guix llvm is compiled without asmjs-unknown-emscripten.
                  (("fn wasm32_final_outputs") "#[ignore]\nfn wasm32_final_outputs"))
                 (substitute* "src/tools/cargo/tests/death.rs"
                  ;; This is stuck when built in container.
                  (("fn ctrl_c_kills_everyone") "#[ignore]\nfn ctrl_c_kills_everyone"))
                 ;; Prints test output in the wrong order when built on
                 ;; i686-linux.
                 (substitute* "src/tools/cargo/tests/test.rs"
                   (("fn cargo_test_env") "#[ignore]\nfn cargo_test_env"))

                 ;; These tests pull in a dependency on "git", which changes
                 ;; too frequently take part in the Rust toolchain.
                 (substitute* "src/tools/cargo/tests/new.rs"
                   (("fn author_prefers_cargo") "#[ignore]\nfn author_prefers_cargo")
                   (("fn finds_author_git") "#[ignore]\nfn finds_author_git")
                   (("fn finds_local_author_git") "#[ignore]\nfn finds_local_author_git"))
                 #t))
             (add-after 'patch-cargo-tests 'ignore-glibc-2.27-incompatible-test
               ;; https://github.com/rust-lang/rust/issues/47863
               (lambda _
                 (substitute* "src/test/run-pass/out-of-stack.rs"
                   (("// ignore-android") "// ignore-test\n// ignore-android"))
                 #t))
             (replace 'configure
               (lambda* (#:key inputs outputs #:allow-other-keys)
                 (let* ((out (assoc-ref outputs "out"))
                        (doc (assoc-ref outputs "doc"))
                        (gcc (assoc-ref inputs "gcc"))
                        (gdb (assoc-ref inputs "gdb"))
                        (binutils (assoc-ref inputs "binutils"))
                        (python (assoc-ref inputs "python-2"))
                        (rustc (assoc-ref inputs "rustc-bootstrap"))
                        (cargo (assoc-ref inputs "cargo-bootstrap"))
                        (llvm (assoc-ref inputs "llvm"))
                        (jemalloc (assoc-ref inputs "jemalloc")))
                   (call-with-output-file "config.toml"
                     (lambda (port)
                       (display (string-append "
[llvm]
[build]
cargo = \"" cargo "/bin/cargo" "\"
rustc = \"" rustc "/bin/rustc" "\"
docs = true
python = \"" python "/bin/python2" "\"
gdb = \"" gdb "/bin/gdb" "\"
vendor = true
submodules = false
[install]
prefix = \"" out "\"
docdir = \"" doc "/share/doc/rust" "\"
sysconfdir = \"etc\"
[rust]
default-linker = \"" gcc "/bin/gcc" "\"
channel = \"stable\"
rpath = true
" ;; There are 2 failed codegen tests:
;; codegen/mainsubprogram.rs and codegen/mainsubprogramstart.rs
;; These tests require a patched LLVM
"codegen-tests = false
[target." ,(nix-system->gnu-triplet-for-rust) "]
llvm-config = \"" llvm "/bin/llvm-config" "\"
cc = \"" gcc "/bin/gcc" "\"
cxx = \"" gcc "/bin/g++" "\"
ar = \"" binutils "/bin/ar" "\"
jemalloc = \"" jemalloc "/lib/libjemalloc_pic.a" "\"
[dist]
") port)))
                   #t)))
             (add-after 'configure 'provide-cc
               (lambda* (#:key inputs #:allow-other-keys)
                 (symlink (string-append (assoc-ref inputs "gcc") "/bin/gcc")
                          "/tmp/cc")
                 (setenv "PATH" (string-append "/tmp:" (getenv "PATH")))
                 #t))
             (add-after 'provide-cc 'configure-archiver
               (lambda* (#:key inputs #:allow-other-keys)
                 (substitute* "src/build_helper/lib.rs"
                  ;; Make sure "ar" is always used as the archiver.
                  (("\"musl\"") "\"\"")
                  ;; Then substitute "ar" by our name.
                  (("\"ar\"") (string-append "\""
                               (assoc-ref inputs "binutils")
                               "/bin/ar\"")))
                 #t))
             (delete 'patch-cargo-tomls)
             (add-before 'build 'reset-timestamps-after-changes
               (lambda* _
                 (for-each
                  (lambda (filename)
                    ;; Rust 1.20.0 treats timestamp 0 as "file doesn't exist".
                    ;; Therefore, use timestamp 1.
                    (utime filename 1 1 1 1))
                  (find-files "." #:directories? #t))
                 #t))
             (replace 'build
               (lambda* _
                 (invoke "./x.py" "build")
                 (invoke "./x.py" "build" "src/tools/cargo")))
             (replace 'check
               (lambda* _
                 ;; Disable parallel execution to prevent EAGAIN errors when
                 ;; running tests.
                 (invoke "./x.py" "-j1" "test" "-vv")
                 (invoke "./x.py" "-j1" "test" "src/tools/cargo")
                 #t))
             (replace 'install
               (lambda* (#:key outputs #:allow-other-keys)
                 (invoke "./x.py" "install")
                 (substitute* "config.toml"
                   ;; replace prefix to specific output
                   (("prefix = \"[^\"]*\"")
                    (string-append "prefix = \"" (assoc-ref outputs "cargo") "\"")))
                 (invoke "./x.py" "install" "cargo")))
             (add-after 'install 'wrap-rustc
               (lambda* (#:key inputs outputs #:allow-other-keys)
                 (let ((out (assoc-ref outputs "out"))
                       (libc (assoc-ref inputs "libc"))
                       (ld-wrapper (assoc-ref inputs "ld-wrapper")))
                   ;; Let gcc find ld and libc startup files.
                   (wrap-program (string-append out "/bin/rustc")
                     `("PATH" ":" prefix (,(string-append ld-wrapper "/bin")))
                     `("LIBRARY_PATH" ":" suffix (,(string-append libc "/lib"))))
                   #t))))))))))

(define-public rust-1.21
  (let ((base-rust (rust-bootstrapped-package rust-1.20 "1.21.0"
                    "1yj8lnxybjrybp00fqhxw8fpr641dh8wcn9mk44xjnsb4i1c21qp")))
    (package
      (inherit base-rust)
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'configure 'remove-ar
               (lambda* (#:key inputs #:allow-other-keys)
                 ;; Remove because toml complains about "unknown field".
                 (substitute* "config.toml"
                  (("^ar =.*") "\n"))
                 #t)))))))))

(define-public rust-1.22
  (let ((base-rust (rust-bootstrapped-package rust-1.21 "1.22.1"
                    "1lrzzp0nh7s61wgfs2h6ilaqi6iq89f1pd1yaf65l87bssyl4ylb")))
    (package
      (inherit base-rust)
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'unpack 'remove-flaky-test
               (lambda _
                 ;; See <https://github.com/rust-lang/rust/issues/43402>.
                 (when (file-exists? "src/test/run-make/issue-26092")
                   (delete-file-recursively "src/test/run-make/issue-26092"))
                 #t)))))))))

(define-public rust-1.23
  (let ((base-rust (rust-bootstrapped-package rust-1.22 "1.23.0"
                    "14fb8vhjzsxlbi6yrn1r6fl5dlbdd1m92dn5zj5gmzfwf4w9ar3l")))
    (package
      (inherit base-rust)
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             (delete 'configure-archiver)
             (delete 'remove-ar)
             (add-after 'unpack 'dont-build-native
               (lambda _
                 ;; XXX: Revisit this when we use gcc 6.
                 (substitute* "src/binaryen/CMakeLists.txt"
                  (("ADD_COMPILE_FLAG\\(\\\"-march=native\\\"\\)") ""))
                 #t)))))))))

(define-public rust-1.24
  (let ((base-rust
         (rust-bootstrapped-package rust-1.23 "1.24.1"
                                    "1vv10x2h9kq7fxh2v01damdq8pvlp5acyh1kzcda9sfjx12kv99y")))
    (package
      (inherit base-rust)
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             (delete 'use-readelf-for-tests)
             (replace 'patch-aarch64-test
               (lambda* _
                 (substitute* "src/librustc_metadata/dynamic_lib.rs"
                   ;; This test is known to fail on aarch64 and powerpc64le:
                   ;; https://github.com/rust-lang/rust/issues/45410
                   (("fn test_loading_cosine") "#[ignore]\nfn test_loading_cosine"))
                 #t)))))))))

;;; Rust 1.25 release support work with llvm 6--but build with llvm 6 is
;;; not determenistic due to <https://github.com/rust-lang/rust/issues/50556>.
;;; Keep using llvm 3.9.1 until builds become determenistic
(define-public rust-1.25
  (let ((base-rust
         (rust-bootstrapped-package rust-1.24 "1.25.0"
          "0baxjr99311lvwdq0s38bipbnj72pn6fgbk6lcq7j555xq53mxpf"
          #:patches '("rust-1.25-accept-more-detailed-gdb-lines.patch"))))
    (package
      (inherit base-rust)
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'patch-cargo-tests 'patch-cargo-index-update
               (lambda _
                 (substitute* "src/tools/cargo/tests/generate-lockfile.rs"
                   ;; This test wants to update the crate index.
                   (("fn no_index_update") "#[ignore]\nfn no_index_update"))
                 #t))
             (replace 'patch-aarch64-test
               (lambda _
                 (substitute* "src/librustc_metadata/dynamic_lib.rs"
                   ;; This test is known to fail on aarch64 and powerpc64le:
                   ;; https://github.com/rust-lang/rust/issues/45410
                   (("fn test_loading_cosine") "#[ignore]\nfn test_loading_cosine"))
                 ;; This test fails on aarch64 with llvm@6.0:
                 ;; https://github.com/rust-lang/rust/issues/49807
                 ;; other possible solution:
                 ;; https://github.com/rust-lang/rust/pull/47688
                 (delete-file "src/test/debuginfo/by-value-self-argument-in-trait-impl.rs")
                 #t))
             (delete 'ignore-glibc-2.27-incompatible-test))))))))

(define-public rust-1.26
  (let ((base-rust
         (rust-bootstrapped-package rust-1.25 "1.26.2"
          "0047ais0fvmqvngqkdsxgrzhb0kljg8wy85b01kbbjc88hqcz7pv"
          #:patches '("rust-coresimd-doctest.patch"
                      "rust-1.25-accept-more-detailed-gdb-lines.patch"))))
    (package
      (inherit base-rust)
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             ;; binaryen was replaced with LLD project from LLVM
             (delete 'dont-build-native)
             (replace 'remove-unsupported-tests
               (lambda* _
                 ;; Our ld-wrapper cannot process non-UTF8 bytes in LIBRARY_PATH.
                 ;; <https://lists.gnu.org/archive/html/guix-devel/2017-06/msg00193.html>
                 (delete-file-recursively "src/test/run-make-fulldeps/linker-output-non-utf8")
                 #t))
             (replace 'patch-cargo-tests
               (lambda* _
                 (substitute* "src/tools/cargo/tests/testsuite/build.rs"
                   (("/usr/bin/env") (which "env"))
                   ;; Guix llvm is compiled without asmjs-unknown-emscripten.
                   (("fn wasm32_final_outputs") "#[ignore]\nfn wasm32_final_outputs"))
                 (substitute* "src/tools/cargo/tests/testsuite/death.rs"
                   ;; This is stuck when built in container.
                   (("fn ctrl_c_kills_everyone") "#[ignore]\nfn ctrl_c_kills_everyone"))
                 ;; Prints test output in the wrong order when built on
                 ;; i686-linux.
                 (substitute* "src/tools/cargo/tests/testsuite/test.rs"
                   (("fn cargo_test_env") "#[ignore]\nfn cargo_test_env"))

                 ;; Avoid dependency on "git".
                 (substitute* "src/tools/cargo/tests/testsuite/new.rs"
                   (("fn author_prefers_cargo") "#[ignore]\nfn author_prefers_cargo")
                   (("fn finds_author_git") "#[ignore]\nfn finds_author_git")
                   (("fn finds_local_author_git") "#[ignore]\nfn finds_local_author_git"))
                 #t))
             (add-after 'patch-cargo-tests 'disable-cargo-test-for-nightly-channel
               (lambda* _
                 ;; This test failed to work on "nightly" channel builds
                 ;; https://github.com/rust-lang/cargo/issues/5648
                 (substitute* "src/tools/cargo/tests/testsuite/resolve.rs"
                   (("fn test_resolving_minimum_version_with_transitive_deps")
                    "#[ignore]\nfn test_resolving_minimum_version_with_transitive_deps"))
                 #t))
             (replace 'patch-cargo-index-update
               (lambda* _
                 (substitute* "src/tools/cargo/tests/testsuite/generate_lockfile.rs"
                   ;; This test wants to update the crate index.
                   (("fn no_index_update") "#[ignore]\nfn no_index_update"))
                 #t)))))))))

(define-public rust-1.27
  (let ((base-rust
         (rust-bootstrapped-package rust-1.26 "1.27.2"
                                    "0pg1s37bhx9zqbynxyydq5j6q7kij9vxkcv8maz0m25prm88r0cs"
                                    #:patches
                                    '("rust-coresimd-doctest.patch"
                                      "rust-bootstrap-stage0-test.patch"
                                      "rust-1.25-accept-more-detailed-gdb-lines.patch"
                                      "rust-reproducible-builds.patch"))))
    (package
      (inherit base-rust)
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-before 'install 'mkdir-prefix-paths
               (lambda* (#:key outputs #:allow-other-keys)
                 ;; As result of https://github.com/rust-lang/rust/issues/36989
                 ;; `prefix' directory should exist before `install' call
                 (mkdir-p (assoc-ref outputs "out"))
                 (mkdir-p (assoc-ref outputs "cargo"))
                 #t))
             (add-after 'patch-cargo-tests 'disable-thinlto-test
               (lambda* _
                 ;; thinlto required llvm 6.0 for work
                 (substitute* "src/tools/cargo/tests/testsuite/path.rs"
                   (("fn thin_lto_works") "#[ignore]\nfn thin_lto_works"))
                 #t)))))))))

(define-public rust-1.28
  (let ((base-rust
         (rust-bootstrapped-package rust-1.27 "1.28.0"
                                    "11k4rn77bca2rikykkk9fmprrgjswd4x4kaq7fia08vgkir82nhx"
                                    #:patches
                                    '("rust-coresimd-doctest.patch"
                                      "rust-bootstrap-stage0-test.patch"
                                      "rust-1.25-accept-more-detailed-gdb-lines.patch"
                                      "rust-reproducible-builds.patch"))))
    (package
      (inherit base-rust)
      (inputs
       ;; Use LLVM 6.0
       (alist-replace "llvm" (list llvm-6)
                      (package-inputs base-rust)))
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'configure 'enable-codegen-tests
               ;; Codegen tests should pass with llvm 6, so enable them.
               (lambda* _
                 (substitute* "config.toml"
                   (("codegen-tests = false") ""))
                 #t))
             (add-after 'patch-tests 'disable-amd64-avx-test
               ;; That test would fail on x86_64 machines without avx.
               (lambda* _
                 (substitute* "src/test/run-pass/issue-44056.rs"
                   (("only-x86_64") "ignore-test"))
                 #t))
             ;; The thinlto test should pass with llvm 6.
             (delete 'disable-thinlto-test))))))))

(define-public rust-1.29
  (let ((base-rust
         (rust-bootstrapped-package rust-1.28 "1.29.2"
                                    "1jb787080z754caa2w3w1amsygs4qlzj9rs1vy64firfmabfg22h"
                                    #:patches
                                    '("rust-1.25-accept-more-detailed-gdb-lines.patch"
                                      "rust-reproducible-builds.patch"))))
    (package
      (inherit base-rust))))

(define-public rust-1.30
  (let ((base-rust
         (rust-bootstrapped-package rust-1.29 "1.30.1"
                                    "0aavdc1lqv0cjzbqwl5n59yd0bqdlhn0zas61ljf38yrvc18k8rn"
                                    #:patches
                                    '("rust-1.25-accept-more-detailed-gdb-lines.patch"
                                      "rust-1.30-gdb-llvm.patch"
                                      "rust-reproducible-builds.patch"))))
    (package
      (inherit base-rust)
      (inputs
       ;; Use LLVM 7.0
       (alist-replace "llvm" (list llvm)
                      (package-inputs base-rust)))
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'patch-cargo-tests 'patch-cargo-env-shebang
               (lambda* (#:key inputs #:allow-other-keys)
                 (let ((coreutils (assoc-ref inputs "coreutils")))
                   (substitute* "src/tools/cargo/tests/testsuite/fix.rs"
                     ;; Cargo has a test which explicitly sets a
                     ;; RUSTC_WRAPPER environment variable which points
                     ;; to /usr/bin/env. Since it's not a shebang, it
                     ;; needs to be manually patched
                     (("\"/usr/bin/env\"")
                      (string-append "\"" coreutils "/bin/env\"")))
                   #t)))
             (add-after 'patch-cargo-env-shebang 'ignore-cargo-package-tests
               (lambda* _
                 (substitute* "src/tools/cargo/tests/testsuite/package.rs"
                   ;; These tests largely check that cargo outputs warning/error
                   ;; messages as expected. It seems that cargo outputs an
                   ;; absolute path to something in the store instead of the
                   ;; expected relative path (e.g. `[..]`) so we'll ignore
                   ;; these for now
                   (("fn include") "#[ignore]\nfn include")
                   (("fn exclude") "#[ignore]\nfn exclude"))
                   #t))
             ;; Appears that this test isn't currently running and has been
             ;; moved elsewhere, so the patch doesn't apply.
             (delete 'disable-amd64-avx-test))))))))

(define-public rust
  (let ((base-rust
         (rust-bootstrapped-package rust-1.30 "1.31.1"
                                    "0sk84ff0cklybcp0jbbxcw7lk7mrm6kb6km5nzd6m64dy0igrlli"
                                    #:patches
                                    '("rust-1.25-accept-more-detailed-gdb-lines.patch"
                                      "rust-1.30-gdb-llvm.patch"
                                      "rust-reproducible-builds.patch"))))
    (package
      (inherit base-rust)
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'patch-tests 'patch-command-exec-tests
               (lambda* (#:key inputs #:allow-other-keys)
                 (let ((coreutils (assoc-ref inputs "coreutils")))
                   (substitute* "src/test/run-pass/command-exec.rs"
                     ;; This test suite includes some tests that the stdlib's
                     ;; `Command` execution properly handles situations where
                     ;; the environment or PATH variable are empty, but this
                     ;; fails since we don't have `echo` available in the usual
                     ;; Linux directories.
                     ;; NB: the leading space is so we don't fail a tidy check
                     ;; for trailing whitespace, and the newlines are to ensure
                     ;; we don't exceed the 100 chars tidy check as well
                     ((" Command::new\\(\"echo\"\\)")
                      (string-append "\nCommand::new(\"" coreutils "/bin/echo\")\n")))
                   #t)))
             (add-after 'patch-tests 'patch-process-docs-rev-cmd
               (lambda* _
                 ;; Disable some doc tests which depend on the "rev" command
                 ;; https://github.com/rust-lang/rust/pull/58746
                 (substitute* "src/libstd/process.rs"
                   (("```rust") "```rust,no_run"))
                 #t)))))))))
