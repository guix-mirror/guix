;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016 Eric Le Bihan <eric.le.bihan.dev@free.fr>
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017, 2018 Nikolai Merinov <nikolai.merinov@member.fsf.org>
;;; Copyright © 2017, 2019, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Danny Milosavljevic <dannym+a@scratchpost.org>
;;; Copyright © 2019 Ivan Petkov <ivanppetkov@gmail.com>
;;; Copyright © 2020, 2021 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2020 Matthew Kraai <kraai@ftbfs.org>
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

;; This is the hash for the empty file, and the reason it's relevant is not
;; the most obvious.
;;
;; The root of the problem is that Cargo keeps track of a file called
;; Cargo.lock, that contains the hash of the tarball source of each dependency.
;;
;; However, tarball sources aren't handled well by Guix because of the need to
;; patch shebangs in any helper scripts. This is why we use Cargo's vendoring
;; capabilities, where instead of the tarball, a directory is provided in its
;; place. (In the case of rustc, the source code already ships with vendored
;; dependencies, but crates built with cargo-build-system undergo vendoring
;; during the build.)
;;
;; To preserve the advantages of checksumming, vendored dependencies contain
;; a file called .cargo-checksum.json, which contains the hash of the tarball,
;; as well as the list of files in it, with the hash of each file.
;;
;; The patch-cargo-checksums phase of cargo-build-system runs after
;; any Guix-specific patches to the vendored dependencies and regenerates the
;; .cargo-checksum.json files, but it's hard to know the tarball checksum that
;; should be written to the file - and taking care of any unhandled edge case
;; would require rebuilding everything that depends on rust. This is why we lie,
;; and say that the tarball has the hash of an empty file. It's not a problem
;; because cargo-build-system removes the Cargo.lock file. We can't do that
;; for rustc because of a quirk of its build system, so we modify the lock file
;; to substitute the hash.
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

(define* (rust-uri version #:key (dist "static"))
  (string-append "https://" dist ".rust-lang.org/dist/"
                 "rustc-" version "-src.tar.gz"))

(define* (rust-bootstrapped-package base-rust version checksum)
  "Bootstrap rust VERSION with source checksum CHECKSUM using BASE-RUST."
  (package
    (inherit base-rust)
    (version version)
    (source
     (origin
       (inherit (package-source base-rust))
       (uri (rust-uri version))
       (sha256 (base32 checksum))))
    (native-inputs
     (alist-replace "cargo-bootstrap" (list base-rust "cargo")
                    (alist-replace "rustc-bootstrap" (list base-rust)
                                   (package-native-inputs base-rust))))))

(define (patch-command-exec-tests-phase test-path)
"The command-exec.rs test moves around between releases.  We need to apply
a Guix-specific patch to it for each release.  This function generates the phase
that applies said patch, parametrized by the test-path.  This is done this way
because the phase is more complex than the equivalents for other tests that
move around."
`(lambda* (#:key inputs #:allow-other-keys)
   (let ((coreutils (assoc-ref inputs "coreutils")))
     (substitute* ,test-path
       ;; This test suite includes some tests that the stdlib's
       ;; `Command` execution properly handles situations where
       ;; the environment or PATH variable are empty, but this
       ;; fails since we don't have `echo` available in the usual
       ;; Linux directories.
       ;; NB: the leading space is so we don't fail a tidy check
       ;; for trailing whitespace, and the newlines are to ensure
       ;; we don't exceed the 100 chars tidy check as well
       ((" Command::new\\(\"echo\"\\)")
        (string-append "\nCommand::new(\"" coreutils "/bin/echo\")\n"))))))

(define-public rust-1.31
  (let ((base-rust (rust-bootstrapped-package
                    rust-1.30 "1.31.1"
                    "0sk84ff0cklybcp0jbbxcw7lk7mrm6kb6km5nzd6m64dy0igrlli")))
    (package
      (inherit base-rust)
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'patch-tests 'patch-command-exec-tests
               ,(patch-command-exec-tests-phase
                 "src/test/run-pass/command-exec.rs"))
             ;; The test has been moved elsewhere.
             (replace 'disable-amd64-avx-test
               (lambda _
                 (substitute* "src/test/ui/issues/issue-44056.rs"
                   (("only-x86_64")
                    "ignore-test"))))
             (add-after 'patch-tests 'patch-process-docs-rev-cmd
               (lambda* _
                 ;; Disable some doc tests which depend on the "rev" command
                 ;; https://github.com/rust-lang/rust/pull/58746
                 (substitute* "src/libstd/process.rs"
                   (("```rust")
                    "```rust,no_run")))))))))))

(define-public rust-1.32
  (let ((base-rust (rust-bootstrapped-package
                    rust-1.31 "1.32.0"
                    "0ji2l9xv53y27xy72qagggvq47gayr5lcv2jwvmfirx029vlqnac")))
    (package
      (inherit base-rust)
      (source
       (origin
         (inherit (package-source base-rust))
         (snippet '(for-each delete-file-recursively
                             '("src/llvm"
                               "src/llvm-emscripten"
                               "src/tools/clang"
                               "src/tools/lldb"
                               "vendor/jemalloc-sys/jemalloc")))
         (patches (search-patches "rust-reproducible-builds.patch"))
         ;; the vendor directory has moved to the root of
         ;; the tarball, so we have to strip an extra prefix
         (patch-flags '("-p2"))))
      (inputs
       ;; Downgrade to LLVM 6, all LTO tests appear to fail with LLVM 7.0.1
       (alist-replace "llvm" (list llvm-6) (package-inputs base-rust)))
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             ;; Cargo.lock and the vendor/ directory have been moved to the
             ;; root of the rust tarball
             (replace 'patch-cargo-checksums
               (lambda* _
                 (use-modules (guix build cargo-utils))
                 (substitute* "Cargo.lock"
                   (("(\"checksum .* = )\".*\"" all name)
                    (string-append name "\"" ,%cargo-reference-hash "\"")))
                 (generate-all-checksums "vendor")))
             (add-after 'enable-codegen-tests 'override-jemalloc
               (lambda* (#:key inputs #:allow-other-keys)
                 ;; The compiler is no longer directly built against jemalloc,
                 ;; but rather via the jemalloc-sys crate (which vendors the
                 ;; jemalloc source). To use jemalloc we must enable linking to
                 ;; it (otherwise it would use the system allocator), and set
                 ;; an environment variable pointing to the compiled jemalloc.
                 (substitute* "config.toml"
                   (("^jemalloc =.*$") "")
                   (("[[]rust[]]") "\n[rust]\njemalloc=true\n"))
                 (setenv "JEMALLOC_OVERRIDE" (string-append
                                              (assoc-ref inputs "jemalloc")
                                              "/lib/libjemalloc_pic.a"))))
             ;; Remove no longer relevant steps
             (delete 'remove-flaky-test)
             (delete 'patch-aarch64-test))))))))

(define-public rust-1.33
  (let ((base-rust (rust-bootstrapped-package
                    rust-1.32 "1.33.0"
                    "152x91mg7bz4ygligwjb05fgm1blwy2i70s2j03zc9jiwvbsh0as")))
    (package
      (inherit base-rust)
      (source
       (origin
         (inherit (package-source base-rust))
         (patches '())
         (patch-flags '("-p1"))))
      (inputs
       ;; Upgrade to jemalloc@5.1.0
       (alist-replace "jemalloc" (list jemalloc) (package-inputs base-rust)))
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             (delete 'ignore-cargo-package-tests)
             (add-after 'configure 'configure-test-threads
               ;; Several rustc and cargo tests will fail if run on one core
               ;; https://github.com/rust-lang/rust/issues/59122
               ;; https://github.com/rust-lang/cargo/issues/6746
               ;; https://github.com/rust-lang/rust/issues/58907
               (lambda* (#:key inputs #:allow-other-keys)
                 (setenv "RUST_TEST_THREADS" "2"))))))))))

(define-public rust-1.34
  (let ((base-rust (rust-bootstrapped-package
                    rust-1.33 "1.34.1"
                    "19s09k7y5j6g3y4d2rk6kg9pvq6ml94c49w6b72dmq8p9lk8bixh")))
    (package
      (inherit base-rust)
      (source
       (origin
         (inherit (package-source base-rust))
         (snippet '(for-each delete-file-recursively
                             '("src/llvm-emscripten"
                               "src/llvm-project"
                               "vendor/jemalloc-sys/jemalloc"))))))))

(define-public rust-1.35
  (let ((base-rust (rust-bootstrapped-package
                    rust-1.34 "1.35.0"
                    "0bbizy6b7002v1rdhrxrf5gijclbyizdhkglhp81ib3bf5x66kas")))
    (package
      (inherit base-rust)
      (inputs
       (alist-replace "llvm" (list llvm-8) (package-inputs base-rust)))
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             ;; The tidy test includes a pass which ensures large binaries
             ;; don't accidentally get checked into the rust git repo.
             ;; Unfortunately the test assumes that git is always available,
             ;; so we'll comment out the invocation of this pass.
             (add-after 'configure 'disable-tidy-bins-check
               (lambda* _
                 (substitute* "src/tools/tidy/src/main.rs"
                   (("bins::check")
                    "//bins::check")))))))))))

(define-public rust-1.36
  (let ((base-rust (rust-bootstrapped-package
                    rust-1.35 "1.36.0"
                    "06xv2p6zq03lidr0yaf029ii8wnjjqa894nkmrm6s0rx47by9i04")))
    (package
      (inherit base-rust)
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             (delete 'patch-process-docs-rev-cmd))))))))

(define-public rust-1.37
  (let ((base-rust (rust-bootstrapped-package
                    rust-1.36 "1.37.0"
                    "1hrqprybhkhs6d9b5pjskfnc5z9v2l2gync7nb39qjb5s0h703hj")))
    (package
      (inherit base-rust)
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-before 'configure 'configure-cargo-home
               (lambda _
                 (let ((cargo-home (string-append (getcwd) "/.cargo")))
                   (mkdir-p cargo-home)
                   (setenv "CARGO_HOME" cargo-home)))))))))))

(define-public rust-1.38
  (let ((base-rust (rust-bootstrapped-package
                    rust-1.37 "1.38.0"
                    "101dlpsfkq67p0hbwx4acqq6n90dj4bbprndizpgh1kigk566hk4")))
    (package
      (inherit base-rust)
      (inputs
       (alist-replace "llvm" (list llvm-9) (package-inputs base-rust)))
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             (replace 'patch-command-exec-tests
               ,(patch-command-exec-tests-phase
                 "src/test/ui/command-exec.rs"))
             (add-after 'patch-tests 'patch-command-uid-gid-test
               (lambda _
                 (substitute* "src/test/ui/command-uid-gid.rs"
                   (("/bin/sh")
                    (which "sh"))
                   (("ignore-sgx")
                    "ignore-sgx\n// ignore-tidy-linelength")))))))))))

(define-public rust-1.39
  (let ((base-rust (rust-bootstrapped-package
                    rust-1.38 "1.39.0"
                    "0mwkc1bnil2cfyf6nglpvbn2y0zfbv44zfhsd5qg4c9rm6vgd8dl")))
    (package
      (inherit base-rust)
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             (replace 'patch-cargo-checksums
               ;; The Cargo.lock format changed.
               (lambda* _
                 (use-modules (guix build cargo-utils))
                 (substitute* "Cargo.lock"
                   (("(checksum = )\".*\"" all name)
                    (string-append name "\"" ,%cargo-reference-hash "\"")))
                 (generate-all-checksums "vendor"))))))))))

(define-public rust-1.40
  (let ((base-rust (rust-bootstrapped-package
                    rust-1.39 "1.40.0"
                    "1ba9llwhqm49w7sz3z0gqscj039m53ky9wxzhaj11z6yg1ah15yx")))
    (package
      (inherit base-rust)
      (source
       (origin
         (inherit (package-source base-rust))
         ;; llvm-emscripten is no longer bundled, as that codegen backend
         ;; got removed.
         (snippet '(for-each delete-file-recursively
                             '("src/llvm-project"
                               "vendor/jemalloc-sys/jemalloc")))))
      (arguments
       ;; Rust 1.40 does not ship rustc-internal libraries by default
       ;; (see rustc-dev-split). This means that librustc_driver.so is no
       ;; longer available in lib/rustlib/$target/lib, which is the directory
       ;; included in the runpath of librustc_codegen_llvm-llvm.so.
       ;; This is detected by our validate-runpath phase as an error, but it
       ;; is harmless as the codegen backend is loaded by librustc_driver.so
       ;; itself, which must at that point have been already loaded.
       ;; As such, we skip validating the runpath for Rust 1.40.
       ;; Rust 1.41 stopped putting the codegen backend in a separate library,
       ;; which makes this workaround only necessary for this release.
       (cons* #:validate-runpath? #f
              (substitute-keyword-arguments (package-arguments base-rust)
                ((#:phases phases)
                 `(modify-phases ,phases
                    ;; We often need to patch tests with various Guix-specific paths.
                    ;; This often increases the line length and makes tidy, rustc's
                    ;; style checker, complain. We could insert additional newlines
                    ;; or add an "// ignore-tidy-linelength" comment, but as an
                    ;; ignore comment must be used, both approaches are fragile due
                    ;; to upstream formatting changes. As such, disable running the
                    ;; linter during tests, since it's intended for rustc developers
                    ;; anyway.
                    ;;
                    ;; TODO(rebuild-rust): This phase could be added earlier to
                    ;; simplify a significant amount of code, but it would require
                    ;; rebuilding the entire rusty universe.
                    (add-after 'patch-tests 'neuter-tidy
                      (lambda _
                        (substitute* "src/bootstrap/builder.rs"
                          (("^.*::Tidy,")
                           ""))))
                    ;; TODO(rebuild-rust): Adapt the find-files approach for
                    ;; earlier testsuite patches.
                    (replace 'patch-command-uid-gid-test
                      (lambda _
                        (match (find-files "src/test" "command-uid-gid\\.rs")
                          ((file)
                           (substitute* file
                             (("/bin/sh")
                              (which "sh")))))))
                    (replace 'patch-command-exec-tests
                      ,(patch-command-exec-tests-phase
                        '(match (find-files "src/test" "command-exec\\.rs")
                           ((file) file))))
                    ;; The test got removed in commit 000fe63b6fc57b09828930cacbab20c2ee6e6d15
                    ;; "Remove painful test that is not pulling its weight"
                    (delete 'remove-unsupported-tests)))))))))

(define-public rust-1.41
  (let ((base-rust (rust-bootstrapped-package
                    rust-1.40 "1.41.1"
                    "0ws5x0fxv57fyllsa6025h3q6j9v3m8nb3syl4x0hgkddq0kvj9q")))
    (package
      (inherit base-rust)
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:validate-runpath? _ #f) #t))))))

(define-public rust-1.42
  (rust-bootstrapped-package
   rust-1.41 "1.42.0" "0x9lxs82may6c0iln0b908cxyn1cv7h03n5cmbx3j1bas4qzks6j"))

(define-public rust-1.43
  (rust-bootstrapped-package
   rust-1.42 "1.43.0" "18akhk0wz1my6y9vhardriy2ysc482z0fnjdcgs9gy59kmnarxkm"))

(define-public rust-1.44
  (rust-bootstrapped-package
   rust-1.43 "1.44.1" "0ww4z2v3gxgn3zddqzwqya1gln04p91ykbrflnpdbmcd575n8bky"))

(define-public rust-1.45
  (let ((base-rust (rust-bootstrapped-package
                    rust-1.44 "1.45.2"
                    "0273a1g3f59plyi1n0azf21qjzwml1yqdnj5z472crz37qggr8xp")))
    (package
      (inherit base-rust)
      (source
       (origin
         (inherit (package-source base-rust))
         (patches (search-patches "rust-1.45-linker-locale.patch"))))
      (inputs
       (alist-replace "llvm" (list llvm-10) (package-inputs base-rust)))
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             ;; These tests make sure that the parser behaves properly when
             ;; a source file starts with a shebang. Unfortunately,
             ;; the patch-shebangs phase changes the meaning of these edge-cases.
             ;; We skip the test since it's drastically unlikely Guix's packaging
             ;; will introduce a bug here.
             (add-after 'patch-tests 'skip-shebang-tests
               (lambda _
                 (with-directory-excursion "src/test/ui/parser/shebang"
                   (delete-file "shebang-doc-comment.rs")
                   (delete-file "sneaky-attrib.rs"))))
             ;; This test case synchronizes itself by starting a localhost TCP
             ;; server. This doesn't work as networking is not available.
             (add-after 'patch-tests 'skip-networking-test
               (lambda _
                 (substitute* "src/tools/cargo/tests/testsuite/freshness.rs"
                   (("fn linking_interrupted" all)
                    (string-append "#[ignore] " all))))))))))))

(define-public rust-1.46
  (let ((base-rust (rust-bootstrapped-package
                    rust-1.45 "1.46.0"
                    "0a17jby2pd050s24cy4dfc0gzvgcl585v3vvyfilniyvjrqknsid")))
    (package
      (inherit base-rust)
      (outputs (cons "rustfmt" (package-outputs base-rust)))
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             (replace 'build
               ;; Phase overridden to also build rustfmt.
               (lambda* _
                 (invoke "./x.py" "build")
                 (invoke "./x.py" "build" "src/tools/cargo")
                 (invoke "./x.py" "build" "src/tools/rustfmt")))
             (replace 'check
               ;; Phase overridden to also test rustfmt.
               (lambda* _
                 (let ((job-spec (string-append
                                  "-j" (if parallel-build?
                                           (number->string (parallel-job-count))
                                           "1"))))
                   (invoke "./x.py" job-spec "test" "-vv")
                   (invoke "./x.py" job-spec "test" "src/tools/cargo")
                   (invoke "./x.py" job-spec "test" "src/tools/rustfmt"))))
             (replace 'install
               ;; Phase overridden to also install rustfmt.
               (lambda* (#:key outputs #:allow-other-keys)
                 (invoke "./x.py" "install")
                 (substitute* "config.toml"
                   ;; Adjust the prefix to the 'cargo' output.
                   (("prefix = \"[^\"]*\"")
                    (format #f "prefix = ~s" (assoc-ref outputs "cargo"))))
                 (invoke "./x.py" "install" "cargo")
                 (substitute* "config.toml"
                   ;; Adjust the prefix to the 'rustfmt' output.
                   (("prefix = \"[^\"]*\"")
                    (format #f "prefix = ~s" (assoc-ref outputs "rustfmt"))))
                 (invoke "./x.py" "install" "rustfmt")))
             (replace 'delete-install-logs
               (lambda* (#:key outputs #:allow-other-keys)
                 (define (delete-manifest-file out-path file)
                   (delete-file (string-append out-path "/lib/rustlib/" file)))

                 (let ((out (assoc-ref outputs "out"))
                       (cargo-out (assoc-ref outputs "cargo"))
                       (rustfmt-out (assoc-ref outputs "rustfmt")))
                   (for-each
                    (lambda (file) (delete-manifest-file out file))
                    '("install.log"
                      "manifest-rust-docs"
                      ,(string-append "manifest-rust-std-"
                                      (nix-system->gnu-triplet-for-rust))
                      "manifest-rustc"))
                   (for-each
                    (lambda (file) (delete-manifest-file cargo-out file))
                    '("install.log"
                      "manifest-cargo"))
                   (for-each
                    (lambda (file) (delete-manifest-file rustfmt-out file))
                    '("install.log"
                      "manifest-rustfmt-preview"))))))))))))

(define-public rust-1.47
  (let ((base-rust (rust-bootstrapped-package
                    rust-1.46 "1.47.0"
                    "07fqd2vp7cf1ka3hr207dnnz93ymxml4935vp74g4is79h3dz19i")))
    (package
      (inherit base-rust)
      (inputs
       (alist-replace "llvm" (list llvm-11) (package-inputs base-rust)))
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             ;; The source code got rearranged: libstd is now in the newly created library folder.
             (replace 'patch-tests
               (lambda* (#:key inputs #:allow-other-keys)
                 (let ((bash (assoc-ref inputs "bash")))
                   (substitute* "library/std/src/process.rs"
                     (("\"/bin/sh\"") (string-append "\"" bash "/bin/sh\"")))
                   ;; <https://lists.gnu.org/archive/html/guix-devel/2017-06/msg00222.html>
                   (substitute* "library/std/src/sys/unix/process/process_common.rs"
                     (("fn test_process_mask") "#[allow(unused_attributes)]
    #[ignore]
    fn test_process_mask")))))
             (delete 'patch-cargo-checksums)
             (add-after 'patch-generated-file-shebangs 'patch-cargo-checksums
               ;; Generate checksums after patching generated files (in
               ;; particular, vendor/jemalloc/rep/Makefile).
               (lambda* _
                 (use-modules (guix build cargo-utils))
                 (substitute* "Cargo.lock"
                   (("(checksum = )\".*\"" all name)
                    (string-append name "\"" ,%cargo-reference-hash "\"")))
                 (generate-all-checksums "vendor"))))))))))

(define-public rust-1.48
  (let ((base-rust (rust-bootstrapped-package
                    rust-1.47 "1.48.0"
                    "0fz4gbb5hp5qalrl9lcl8yw4kk7ai7wx511jb28nypbxninkwxhf")))
    (package
      (inherit base-rust)
      (source
       (origin
         (inherit (package-source base-rust))
         ;; New patch required due to the second part of the source code rearrangement:
         ;; the relevant source code is now in the compiler directory.
         (patches (search-patches "rust-1.48-linker-locale.patch"))))
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             ;; Some tests got split out into separate files.
             (replace 'patch-tests
               (lambda* (#:key inputs #:allow-other-keys)
                 (let ((bash (assoc-ref inputs "bash")))
                   (substitute* "library/std/src/process/tests.rs"
                     (("\"/bin/sh\"")
                      (string-append "\"" bash "/bin/sh\"")))
                   (substitute* "library/std/src/sys/unix/process/process_common/tests.rs"
                     (("fn test_process_mask")
                      "#[allow(unused_attributes)]
    #[ignore]
    fn test_process_mask"))))))))))))

(define-public rust-1.49
  (rust-bootstrapped-package
   rust-1.48 "1.49.0" "0yf7kll517398dgqsr7m3gldzj0iwsp3ggzxrayckpqzvylfy2mm"))

(define-public rust rust-1.49)
