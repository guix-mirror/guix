;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016 Eric Le Bihan <eric.le.bihan.dev@free.fr>
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017, 2018 Nikolai Merinov <nikolai.merinov@member.fsf.org>
;;; Copyright © 2017, 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Danny Milosavljevic <dannym+a@scratchpost.org>
;;; Copyright © 2019 Ivan Petkov <ivanppetkov@gmail.com>
;;; Copyright © 2020, 2021 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2020 Matthew James Kraai <kraai@ftbfs.org>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 (unmatched parenthesis <paren@disroot.org>
;;; Copyright © 2022 Zheng Junjie <873216071@qq.com>
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
  #:use-module (guix build-system copy)
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

;;; Note: mrustc's only purpose is to be able to bootstap Rust; it's designed
;;; to be used in source form.  The latest support for bootstrapping from
;;; 1.39.0 is not yet released so use the latest commit (see:
;;; https://github.com/thepowersgang/mrustc/issues/185).
(define %mrustc-commit "c7066542f8e93d320323749216bf3c82aecb67c2")
(define %mrustc-source
  (let* ((version "0.9")
         (commit %mrustc-commit)
         (revision "1")
         (name "mrustc"))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/thepowersgang/mrustc")
            (commit commit)))
      (file-name (git-file-name name (git-version version revision commit)))
      (sha256
       (base32
        "0zv1x6601s5fnnkcdlqkc4bknisqz569qb0iyb9rjsmaf1kh0na3")))))

;;; Rust 1.39 is special in that it is built with mrustc, which shortens the
;;; bootstrap path.
(define rust-1.39
  (package
    (name "rust")
    (version "1.39.0")
    (source
     (origin
       (method url-fetch)
       (uri (rust-uri version))
       (sha256 (base32 "0mwkc1bnil2cfyf6nglpvbn2y0zfbv44zfhsd5qg4c9rm6vgd8dl"))
       (modules '((guix build utils)))
       (snippet '(for-each delete-file-recursively
                           '("src/llvm-emscripten"
                             "src/llvm-project"
                             "vendor/jemalloc-sys/jemalloc")))
       (patches (search-patches "rustc-1.39.0-src.patch"))
       (patch-flags '("-p0"))))         ;default is -p1
    (outputs '("out" "cargo"))
    (properties '((timeout . 72000)           ;20 hours
                  (max-silent-time . 18000))) ;5 hours (for armel)
    (build-system gnu-build-system)
    (inputs
     `(("libcurl" ,curl)
       ("libssh2" ,libssh2)
       ("llvm" ,llvm)
       ("openssl" ,openssl)
       ("zlib" ,zlib)))
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)
       ("pkg-config" ,pkg-config)
       ;; Required for the libstd sources.
       ("mrustc-source" ,%mrustc-source)))
    (arguments
     `(#:imported-modules ,%cargo-utils-modules ;for `generate-all-checksums'
       #:modules ((guix build cargo-utils)
                  (guix build utils)
                  (guix build gnu-build-system))
       #:test-target "test"
       ;; Rust's own .so library files are not found in any RUNPATH, but
       ;; that doesn't seem to cause issues.
       #:validate-runpath? #f
       #:make-flags
       (list ,(string-append "RUSTC_TARGET="
                             (or (%current-target-system)
                                 (nix-system->gnu-triplet-for-rust)))
             ,(string-append "RUSTC_VERSION=" version)
             ,(string-append "MRUSTC_TARGET_VER="
                             (version-major+minor version))
             "OUTDIR_SUF=")           ;do not add version suffix to output dir
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-reference-to-cc
           ;; This prevents errors like 'error: linker `cc` not found' when
           ;; "cc" is not found on PATH.
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gcc (assoc-ref inputs "gcc")))
               (substitute* (find-files "." "^link.rs$")
                 (("\"cc\".as_ref")
                  (format #f "~s.as_ref" (string-append gcc "/bin/gcc")))))))
         (add-after 'unpack 'setup-mrustc-sources
           (lambda* (#:key inputs #:allow-other-keys)
             (copy-recursively (assoc-ref inputs "mrustc-source") "../mrustc")
             ;; The Makefile of mrustc expects the sources directory of rustc
             ;; to be at this location, and it simplifies things to make it
             ;; so.
             (symlink (getcwd)
                      (string-append "../mrustc/rustc-" ,version "-src"))))
         (add-after 'setup-mrustc-sources 'patch-makefiles
           ;; This disables building the (unbundled) LLVM.
           (lambda* (#:key inputs parallel-build? #:allow-other-keys)
             (let ((llvm (assoc-ref inputs "llvm"))
                   (job-spec (format #f "-j~a"
                                     (if parallel-build?
                                         (number->string (parallel-job-count))
                                         "1"))))
               (with-directory-excursion "../mrustc"
                 (substitute* '("minicargo.mk"
                                "run_rustc/Makefile")
                   ;; Use the system-provided LLVM.
                   (("LLVM_CONFIG := .*")
                    (string-append "LLVM_CONFIG := " llvm "/bin/llvm-config\n"))
                   (("\\$\\(LLVM_CONFIG\\): .*")
                    "$(LLVM_CONFIG):\n")
                   (("\\$Vcd \\$\\(RUSTCSRC\\)build && \\$\\(MAKE\\).*")
                    "true\n"))
                 (substitute* "Makefile"
                   ;; Patch date and git obtained version information.
                   ((" -D VERSION_GIT_FULLHASH=.*")
                    (string-append
                     " -D VERSION_GIT_FULLHASH=\\\"" ,%mrustc-commit "\\\""
                     " -D VERSION_GIT_BRANCH=\\\"master\\\""
                     " -D VERSION_GIT_SHORTHASH=\\\""
                     ,(string-take %mrustc-commit 7) "\\\""
                     " -D VERSION_BUILDTIME="
                     "\"\\\"Thu, 01 Jan 1970 00:00:01 +0000\\\"\""
                     " -D VERSION_GIT_ISDIRTY=0\n"))
                   ;; Do not try to fetch sources from the Internet.
                   ((": \\$\\(RUSTC_SRC_DL\\)")
                    ":"))
                 (substitute* "run_rustc/Makefile"
                   (("[$]Vtime ")
                    "$V ")
                   ;; Unlock the number of parallel jobs for cargo.
                   (("-j [[:digit:]]+ ")
                    "")
                   ;; Patch the shebang of a generated wrapper for rustc
                   (("#!/bin/sh")
                    (string-append "#!" (which "sh"))))))))
         (add-after 'patch-generated-file-shebangs 'patch-cargo-checksums
           (lambda* _
             (substitute* "Cargo.lock"
               (("(checksum = )\".*\"" all name)
                (string-append name "\"" ,%cargo-reference-hash "\"")))
             (generate-all-checksums "vendor")))
         (add-before 'configure 'configure-cargo-home
           (lambda _
             (let ((cargo-home (string-append (getcwd) "/.cargo")))
               (mkdir-p cargo-home)
               (setenv "CARGO_HOME" cargo-home))))
         (replace 'configure
           (lambda _
             (setenv "CC" "gcc")
             (setenv "CXX" "g++")
             ;; The Guix LLVM package installs only shared libraries.
             (setenv "LLVM_LINK_SHARED" "1")
             ;; This is a workaround for
             ;; https://github.com/thepowersgang/mrustc/issues/138.
             (setenv "LIBSSH2_SYS_USE_PKG_CONFIG" "yes")
             ;; rustc still insists on having 'cc' on PATH in some places
             ;; (e.g. when building the 'test' library crate).
             (mkdir-p "/tmp/bin")
             (symlink (which "gcc") "/tmp/bin/cc")
             (setenv "PATH" (string-append "/tmp/bin:" (getenv "PATH")))))
         (delete 'patch-generated-file-shebangs)
         (replace 'build
           (lambda* (#:key make-flags parallel-build? #:allow-other-keys)
             (let* ((src-root (getcwd))
                    (job-count (if parallel-build?
                                   (parallel-job-count)
                                   1))
                    (job-spec (string-append "-j" (number->string job-count))))
               ;; Adapted from:
               ;; https://github.com/dtolnay/bootstrap/blob/master/build.sh.
               (chdir "../mrustc")
               (setenv "MINICARGO_FLAGS" job-spec)
               (setenv "CARGO_BUILD_JOBS" (number->string job-count))
               (display "Building rustc...\n")
               (apply invoke "make" "-f" "minicargo.mk" "output/rustc"
                      job-spec make-flags)
               (display "Building cargo...\n")
               (apply invoke "make" "-f" "minicargo.mk" "output/cargo"
                      job-spec make-flags)
               (display "Rebuilding stdlib with rustc...\n")
               ;; Note: invoking make with -j would cause a compiler error
               ;; (unexpected panic).
               (apply invoke "make" "-C" "run_rustc" make-flags))))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (cargo (assoc-ref outputs "cargo"))
                    (bin (string-append out "/bin"))
                    (rustc (string-append bin "/rustc"))
                    (cargo-bin (string-append cargo "/bin"))
                    (lib (string-append out "/lib"))
                    (gnu-triplet ,(or (%current-target-system)
                                      (nix-system->gnu-triplet-for-rust)))
                    (system-lib-prefix (string-append lib "/rustlib/"
                                                      gnu-triplet "/lib")))
               (mkdir-p (dirname rustc))
               (copy-file "run_rustc/output/prefix/bin/rustc_binary" rustc)
               (wrap-program rustc
                 `("LD_LIBRARY_PATH" = (,system-lib-prefix)))
               (mkdir-p lib)
               (copy-recursively "run_rustc/output/prefix/lib" lib)
               (install-file "run_rustc/output/prefix/bin/cargo" cargo-bin)))))))
    (synopsis "Compiler for the Rust programming language")
    (description "Rust is a systems programming language that provides memory
safety and thread safety guarantees.")
    (home-page "https://github.com/thepowersgang/mrustc")

    ;; So far mrustc is x86_64-only.  It may support i686 soon:
    ;; <https://github.com/thepowersgang/mrustc/issues/78>.
    (supported-systems '("x86_64-linux"))

    ;; Dual licensed.
    (license (list license:asl2.0 license:expat))))

(define rust-1.40
  (package
    (name "rust")
    (version "1.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (rust-uri version))
       (sha256 (base32 "1ba9llwhqm49w7sz3z0gqscj039m53ky9wxzhaj11z6yg1ah15yx"))
       (modules '((guix build utils)))
       ;; llvm-emscripten is no longer bundled, as that codegen backend got
       ;; removed.
       (snippet '(for-each delete-file-recursively
                           '("src/llvm-project"
                             "vendor/jemalloc-sys/jemalloc")))))
    (outputs '("out" "cargo"))
    (properties '((timeout . 72000)           ;20 hours
                  (max-silent-time . 18000))) ;5 hours (for armel)
    (build-system gnu-build-system)
    ;; Rust 1.40 does not ship rustc-internal libraries by default (see
    ;; rustc-dev-split). This means that librustc_driver.so is no longer
    ;; available in lib/rustlib/$target/lib, which is the directory
    ;; included in the runpath of librustc_codegen_llvm-llvm.so.  This is
    ;; detected by our validate-runpath phase as an error, but it is
    ;; harmless as the codegen backend is loaded by librustc_driver.so
    ;; itself, which must at that point have been already loaded.  As such,
    ;; we skip validating the runpath for Rust 1.40.  Rust 1.41 stopped
    ;; putting the codegen backend in a separate library, which makes this
    ;; workaround only necessary for this release.
    (arguments
     `(#:validate-runpath? #f
       ;; Only the final Rust is tested, not the intermediate bootstrap ones,
       ;; for performance and simplicity.
       #:tests? #f
       #:imported-modules ,%cargo-utils-modules ;for `generate-all-checksums'
       #:modules ((guix build cargo-utils)
                  (guix build utils)
                  (guix build gnu-build-system)
                  (ice-9 match)
                  (srfi srfi-1))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-env
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "SHELL" (which "sh"))
             (setenv "CONFIG_SHELL" (which "sh"))
             (setenv "CC" (search-input-file inputs "/bin/gcc"))
             ;; The Guix LLVM package installs only shared libraries.
             (setenv "LLVM_LINK_SHARED" "1")))
         (add-after 'unpack 'add-cc-shim-to-path
           (lambda _
             (mkdir-p "/tmp/bin")
             (symlink (which "gcc") "/tmp/bin/cc")
             (setenv "PATH" (string-append "/tmp/bin:" (getenv "PATH")))))
         (add-after 'unpack 'neuter-tidy
           ;; We often need to patch tests with various Guix-specific paths.
           ;; This often increases the line length and makes tidy, rustc's
           ;; style checker, complain.  We could insert additional newlines or
           ;; add an "// ignore-tidy-linelength" comment, but as an ignore
           ;; comment must be used, both approaches are fragile due to
           ;; upstream formatting changes.  As such, disable running the
           ;; linter during tests, since it's intended for rustc developers
           ;; anyway.
           (lambda _
             (substitute* "src/bootstrap/builder.rs"
               ((".*::Tidy,.*")
                ""))))
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (gcc (assoc-ref inputs "gcc"))
                    (python (assoc-ref inputs "python"))
                    (binutils (assoc-ref inputs "binutils"))
                    (rustc (assoc-ref inputs "rustc-bootstrap"))
                    (cargo (assoc-ref inputs "cargo-bootstrap"))
                    (llvm (assoc-ref inputs "llvm"))
                    (jemalloc (assoc-ref inputs "jemalloc")))
               ;; The compiler is no longer directly built against jemalloc, but
               ;; rather via the jemalloc-sys crate (which vendors the jemalloc
               ;; source). To use jemalloc we must enable linking to it (otherwise
               ;; it would use the system allocator), and set an environment
               ;; variable pointing to the compiled jemalloc.
               (setenv "JEMALLOC_OVERRIDE"
                       (search-input-file inputs
                                          "/lib/libjemalloc_pic.a"))
               (call-with-output-file "config.toml"
                 (lambda (port)
                   (display (string-append "
[llvm]
[build]
cargo = \"" cargo "/bin/cargo" "\"
rustc = \"" rustc "/bin/rustc" "\"
docs = false
python = \"" python "/bin/python" "\"
vendor = true
submodules = false
[install]
prefix = \"" out "\"
sysconfdir = \"etc\"
[rust]
jemalloc=true
default-linker = \"" gcc "/bin/gcc" "\"
channel = \"stable\"
rpath = true
[target." ,(nix-system->gnu-triplet-for-rust) "]
llvm-config = \"" llvm "/bin/llvm-config" "\"
cc = \"" gcc "/bin/gcc" "\"
cxx = \"" gcc "/bin/g++" "\"
ar = \"" binutils "/bin/ar" "\"
[dist]
") port))))))
         (replace 'build
           (lambda* (#:key parallel-build? #:allow-other-keys)
             (let ((job-spec (string-append
                              "-j" (if parallel-build?
                                       (number->string (parallel-job-count))
                                       "1"))))
               (invoke "./x.py" job-spec "build" "--stage=1"
                       "src/libstd"
                       "src/tools/cargo"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (cargo-out (assoc-ref outputs "cargo"))
                    (gnu-triplet ,(or (%current-target-system)
                                      (nix-system->gnu-triplet-for-rust)))
                    (build (string-append "build/" gnu-triplet)))
               ;; Manually do the installation instead of calling './x.py
               ;; install', as that is slow and needlessly rebuilds some
               ;; things.
               (install-file (string-append build "/stage1/bin/rustc")
                             (string-append out "/bin"))
               (copy-recursively (string-append build "/stage1/lib")
                                 (string-append out "/lib"))
               (install-file (string-append build "/stage1-tools-bin/cargo")
                             (string-append cargo-out "/bin")))))
         (add-after 'install 'delete-install-logs
           (lambda* (#:key outputs #:allow-other-keys)
             (for-each (lambda (f)
                         (false-if-exception (delete-file f)))
                       (append-map (lambda (output)
                                     (find-files (string-append
                                                  output "/lib/rustlib")
                                                 "(^install.log$|^manifest-)"))
                                   (map cdr outputs)))))
         (add-after 'install 'wrap-rustc
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (libc (assoc-ref inputs "libc"))
                   (ld-wrapper (assoc-ref inputs "ld-wrapper")))
               ;; Let gcc find ld and libc startup files.
               (wrap-program (string-append out "/bin/rustc")
                 `("PATH" ":" prefix (,(string-append ld-wrapper "/bin")))
                 `("LIBRARY_PATH" ":"
                   suffix (,(string-append libc "/lib"))))))))))
    (native-inputs
     `(("cmake" ,cmake-minimal)
       ("pkg-config" ,pkg-config)       ; For "cargo"
       ("python" ,python-wrapper)
       ("rustc-bootstrap" ,rust-1.39)
       ("cargo-bootstrap" ,rust-1.39 "cargo")
       ("which" ,which)))
    (inputs
     `(("jemalloc" ,jemalloc)
       ("llvm" ,llvm)
       ("openssl" ,openssl)
       ("libssh2" ,libssh2)             ; For "cargo"
       ("libcurl" ,curl)))              ; For "cargo"
    ;; rustc invokes gcc, so we need to set its search paths accordingly.
    ;; Note: duplicate its value here to cope with circular dependencies among
    ;; modules (see <https://bugs.gnu.org/31392>).
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
    (synopsis "Compiler for the Rust programming language")
    (description "Rust is a systems programming language that provides memory
safety and thread safety guarantees.")
    (home-page "https://www.rust-lang.org")
    ;; Dual licensed.
    (license (list license:asl2.0 license:expat))))

(define rust-1.41
  (let ((base-rust (rust-bootstrapped-package
                    rust-1.40 "1.41.1"
                    "0ws5x0fxv57fyllsa6025h3q6j9v3m8nb3syl4x0hgkddq0kvj9q")))
    (package
      (inherit base-rust)
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:validate-runpath? _ #t)
          #t)
         ((#:phases phases)
          `(modify-phases ,phases
             (delete 'add-cc-shim-to-path)
             (add-after 'patch-generated-file-shebangs 'patch-cargo-checksums
               (lambda* _
                 (substitute* "Cargo.lock"
                   (("(checksum = )\".*\"" all name)
                    (string-append name "\"" ,%cargo-reference-hash "\"")))
                 (generate-all-checksums "vendor"))))))))))

(define rust-1.42
  (rust-bootstrapped-package
   rust-1.41 "1.42.0" "0x9lxs82may6c0iln0b908cxyn1cv7h03n5cmbx3j1bas4qzks6j"))

(define rust-1.43
  (rust-bootstrapped-package
   rust-1.42 "1.43.0" "18akhk0wz1my6y9vhardriy2ysc482z0fnjdcgs9gy59kmnarxkm"))

;; This version requires llvm <= 11.
(define rust-1.44
  (rust-bootstrapped-package
   rust-1.43 "1.44.1"
   "0ww4z2v3gxgn3zddqzwqya1gln04p91ykbrflnpdbmcd575n8bky"))

(define rust-1.45
  (let ((base-rust (rust-bootstrapped-package
                    rust-1.44 "1.45.2"
                    "0273a1g3f59plyi1n0azf21qjzwml1yqdnj5z472crz37qggr8xp")))
    (package
      (inherit base-rust)
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'unpack 'set-linker-locale-to-utf8
               (lambda _
                 (substitute* (find-files "." "^linker.rs$")
                   (("linker.env\\(\"LC_ALL\", \"C\"\\);")
                    "linker.env(\"LC_ALL\", \"en_US.UTF-8\");")))))))))))

(define rust-1.46
  (rust-bootstrapped-package
   rust-1.45 "1.46.0" "0a17jby2pd050s24cy4dfc0gzvgcl585v3vvyfilniyvjrqknsid"))

(define rust-1.47
  (let ((base-rust (rust-bootstrapped-package
                    rust-1.46 "1.47.0"
                    "07fqd2vp7cf1ka3hr207dnnz93ymxml4935vp74g4is79h3dz19i")))
    (package/inherit base-rust
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:phases phases)
          `(modify-phases ,phases
             (replace 'build
               ;; The standard library source location moved in this release.
               (lambda* (#:key parallel-build? #:allow-other-keys)
                 (let ((job-spec (string-append
                                  "-j" (if parallel-build?
                                           (number->string (parallel-job-count))
                                           "1"))))
                   (invoke "./x.py" job-spec "build" "--stage=1"
                           "library/std"
                           "src/tools/cargo")))))))))))

(define rust-1.48
  (rust-bootstrapped-package
   rust-1.47 "1.48.0" "0fz4gbb5hp5qalrl9lcl8yw4kk7ai7wx511jb28nypbxninkwxhf"))

(define rust-1.49
  (rust-bootstrapped-package
   rust-1.48 "1.49.0" "0yf7kll517398dgqsr7m3gldzj0iwsp3ggzxrayckpqzvylfy2mm"))

(define rust-1.50
  (rust-bootstrapped-package
   rust-1.49 "1.50.0" "0pjs7j62maiyvkmhp9zrxl528g2n0fphp4rq6ap7aqdv0a6qz5wm"))

(define rust-1.51
  (rust-bootstrapped-package
   rust-1.50 "1.51.0" "0ixqkqglv3isxbvl4ldr4byrkx692wghsz3fasy1pn5kr2prnsvs"))

;;; The LLVM requiriment has been bumped to version 10 in Rust 1.52.  Use the
;;; latest available.
(define rust-1.52
  (let ((base-rust (rust-bootstrapped-package
                    rust-1.51 "1.52.1"
                    "165zs3xzp9dravybwslqs1qhn35agp6wacmzpymqg3qfdni26vrs")))
    (package
      (inherit base-rust)
      (inputs (alist-replace "llvm" (list llvm-12)
                             (package-inputs base-rust))))))

(define rust-1.53
  (rust-bootstrapped-package
   rust-1.52 "1.53.0" "1f95p259dfp5ca118bg107rj3rqwlswy65dxn3hg8sqgl4wwmxsw"))

(define rust-1.54
  (let ((base-rust
         (rust-bootstrapped-package
          rust-1.53
          "1.54.0" "0xk9dhfff16caambmwij67zgshd8v9djw6ha0fnnanlv7rii31dc")))
    (package/inherit base-rust
      (source
       (origin
         (inherit (package-source base-rust))
         (snippet '(delete-file-recursively "src/llvm-project")))))))

(define rust-1.55
  (rust-bootstrapped-package
   rust-1.54 "1.55.0" "07l28f7grdmi65naq71pbmvdd61hwcpi40ry7kp7dy7m233rldxj"))

(define rust-1.56
  (rust-bootstrapped-package
   rust-1.55 "1.56.1" "04cmqx7nn63hzz7z27b2b0dj2qx18rck9ifvip43s6dampx8v2f3"))

(define rust-1.57
  (let ((base-rust
         (rust-bootstrapped-package
          rust-1.56 "1.57.0"
          "06jw8ka2p3kls8p0gd4p0chhhb1ia1mlvj96zn78n7qvp71zjiim")))
    (package
      (inherit base-rust)
      (outputs (cons "rustfmt" (package-outputs base-rust)))
      (arguments
       (substitute-keyword-arguments (package-arguments base-rust)
         ((#:tests? _ #f)
          #t)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'unpack 'relax-gdb-auto-load-safe-path
               ;; Allow GDB to load binaries from any location, otherwise the
               ;; gdbinfo tests fail.  This is only useful when testing with a
               ;; GDB version newer than 8.2.
               (lambda _
                 (setenv "HOME" (getcwd))
                 (with-output-to-file (string-append (getenv "HOME") "/.gdbinit")
                   (lambda _
                     (format #t "set auto-load safe-path /~%")))
                 ;; Do not launch gdb with '-nx' which causes it to not execute
                 ;; any init file.
                 (substitute* "src/tools/compiletest/src/runtest.rs"
                   (("\"-nx\".as_ref\\(\\), ")
                    ""))))
             (add-after 'unpack 'patch-cargo-env-shebang
               (lambda _
                 (substitute* '("src/tools/cargo/tests/testsuite/build.rs"
                                "src/tools/cargo/tests/testsuite/fix.rs")
                   ;; The cargo *_wrapper tests set RUSTC.*WRAPPER environment
                   ;; variable which points to /usr/bin/env.  Since it's not a
                   ;; shebang, it needs to be manually patched.
                   (("/usr/bin/env")
                    (which "env")))))
             (add-after 'unpack 'disable-tests-requiring-git
               (lambda _
                 (substitute* "src/tools/cargo/tests/testsuite/new.rs"
                   (("fn author_prefers_cargo")
                    "#[ignore]\nfn author_prefers_cargo")
                   (("fn finds_author_git")
                    "#[ignore]\nfn finds_author_git")
                   (("fn finds_local_author_git")
                    "#[ignore]\nfn finds_local_author_git"))))
             (add-after 'unpack 'patch-command-exec-tests
               ;; This test suite includes some tests that the stdlib's
               ;; `Command` execution properly handles in situations where
               ;; the environment or PATH variable are empty, but this fails
               ;; since we don't have `echo` available at its usual FHS
               ;; location.
               (lambda _
                 (substitute* (match (find-files "." "^command-exec.rs$")
                                ((file) file))
                   (("Command::new\\(\"echo\"\\)")
                    (format #f "Command::new(~s)" (which "echo"))))))
             (add-after 'unpack 'patch-command-uid-gid-test
               (lambda _
                 (substitute* (match (find-files "." "^command-uid-gid.rs$")
                                ((file) file))
                   (("/bin/sh")
                    (which "sh")))))
             (add-after 'unpack 'skip-shebang-tests
               ;; This test make sure that the parser behaves properly when a
               ;; source file starts with a shebang. Unfortunately, the
               ;; patch-shebangs phase changes the meaning of these edge-cases.
               ;; We skip the test since it's drastically unlikely Guix's
               ;; packaging will introduce a bug here.
               (lambda _
                 (delete-file "src/test/ui/parser/shebang/sneaky-attrib.rs")))
             (add-after 'unpack 'patch-process-tests
               (lambda* (#:key inputs #:allow-other-keys)
                 (let ((bash (assoc-ref inputs "bash")))
                   (substitute* "library/std/src/process/tests.rs"
                     (("\"/bin/sh\"")
                      (string-append "\"" bash "/bin/sh\"")))
                   (substitute* "library/std/src/sys/unix/process/process_common/tests.rs"
                     (("fn test_process_mask")
                      "#[allow(unused_attributes)]
    #[ignore]
    fn test_process_mask")))))
             (add-after 'unpack 'disable-interrupt-tests
               (lambda _
                 ;; This test hangs in the build container; disable it.
                 (substitute* (match (find-files "." "^freshness.rs$")
                                ((file) file))
                   (("fn linking_interrupted")
                    "#[ignore]\nfn linking_interrupted"))
                 ;; Likewise for the ctrl_c_kills_everyone test.
                 (substitute* (match (find-files "." "^death.rs$")
                                ((file) file))
                   (("fn ctrl_c_kills_everyone")
                    "#[ignore]\nfn ctrl_c_kills_everyone"))))
             (add-after 'configure 'add-gdb-to-config
               (lambda* (#:key inputs #:allow-other-keys)
                 (let ((gdb (assoc-ref inputs "gdb")))
                   (substitute* "config.toml"
                     (("^python =.*" all)
                      (string-append all
                                     "gdb = \"" gdb "/bin/gdb\"\n"))))))
             (replace 'build
               ;; Phase overridden to also build rustfmt.
               (lambda* (#:key parallel-build? #:allow-other-keys)
                 (let ((job-spec (string-append
                                  "-j" (if parallel-build?
                                           (number->string (parallel-job-count))
                                           "1"))))
                   (invoke "./x.py" job-spec "build"
                           "library/std" ;rustc
                           "src/tools/cargo"
                           "src/tools/rustfmt"))))
             (replace 'check
               ;; Phase overridden to also test rustfmt.
               (lambda* (#:key tests? parallel-build? #:allow-other-keys)
                 (when tests?
                   (let ((job-spec (string-append
                                    "-j" (if parallel-build?
                                             (number->string (parallel-job-count))
                                             "1"))))
                     (invoke "./x.py" job-spec "test" "-vv"
                             "library/std"
                             "src/tools/cargo"
                             "src/tools/rustfmt")))))
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
                 (invoke "./x.py" "install" "rustfmt")))))))
      ;; Add test inputs.
      (native-inputs (cons* `("gdb" ,gdb)
                            `("procps" ,procps)
                            (package-native-inputs base-rust))))))

;;; Note: Only the latest versions of Rust are supported and tested.  The
;;; intermediate rusts are built for bootstrapping purposes and should not
;;; be relied upon.  This is to ease maintenance and reduce the time
;;; required to build the full Rust bootstrap chain.
(define-public rust rust-1.57)

(define-public rust-src
  (hidden-package
   (package
     (inherit rust)
     (name "rust-src")
     (build-system copy-build-system)
     (native-inputs '())
     (inputs '())
     (native-search-paths '())
     (outputs '("out"))
     (arguments
      `(#:install-plan
        '(("library" "lib/rustlib/src/rust/library")
          ("src" "lib/rustlib/src/rust/src"))))
     (synopsis "Source code for the Rust standard library")
     (description "This package provide source code for the Rust standard
library, only use by rust-analyzer, make rust-analyzer out of the box."))))
