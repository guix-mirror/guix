;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016 Eric Le Bihan <eric.le.bihan.dev@free.fr>
;;; Copyright © 2016 ng0 <ng0@infotropique.org>
;;; Copyright © 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017 Nikolai Merinov <nikolai.merinov@member.fsf.org>
;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix base16)      ;for generated "cargo" native-inputs
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26))

;; Should be one less than the current released version.
(define %rust-bootstrap-binaries-version "1.21.0")

(define %rust-bootstrap-binaries
  (origin
    (method url-fetch)
    (uri (string-append
          "https://static.rust-lang.org/dist/"
          "rust-" %rust-bootstrap-binaries-version
          "-" %host-type ".tar.gz"))
    (sha256
     (base32
      (match %host-type
        ("i686-unknown-linux-gnu"
         "1vnvqwz30hvyjcfr1f602lg43v2vlqjr3yhb5vr8xnrcc07yvjmp")
        ("x86_64-unknown-linux-gnu"
         "1s0866qcy0645bqhsbs3pvk2hi52ps8jzs7x096w0as033h707ml")
        ("armv7-unknown-linux-gnueabihf"
         "1ml8fjq2b6j2vn1j314w93pf4wjl97n1mbz609h3i7md0zqscvs1")
        ("aarch64-unknown-linux-gnu"
         "1hv4m2m7xjcph39r6baryfg23hjcr4sbsrfnd1lh0wn67k2fc7j9")
        ("mips64el-unknown-linux-gnuabi64"
         "0p7fzkfcqg5yvj86v434z351dp7s7pgns8nzxj0fz3hmbfbvlvn9")
        (_ "")))))) ; Catch-all for other systems.

(define %cargo-reference-project-file "/dev/null")
(define %cargo-reference-hash
  "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")

(define (increment-rust-version rust-version major patch)
  (match (string-split rust-version #\.)
    (("1" minor _)
     (string-append (number->string major) "."
                    (number->string (+ (string->number minor) 1)) "."
                    (number->string patch)))))

(define* (cargo-version rustc-version #:optional (patch 0))
  ;; Computes the cargo version that matches the rustc version.
  ;; https://github.com/rust-lang/cargo#Releases
  (increment-rust-version rustc-version 0 patch))

(define* (rustc-version bootstrap-version #:optional (patch 0))
  ;; Computes the rustc version that can be compiled from a given
  ;; other rustc version. The patch argument is for selecting
  ;; a stability or security fix. 1.11.0 -> 1.12.1 -> 1.13.0
  (increment-rust-version bootstrap-version 1 patch))

(define rustc-bootstrap
  (package
    (name "rustc-bootstrap")
    (version %rust-bootstrap-binaries-version)
    (source %rust-bootstrap-binaries)
    (build-system gnu-build-system)
    (native-inputs
     `(("patchelf" ,patchelf)))
    (inputs
     `(("gcc:lib" ,(canonical-package gcc) "lib")
       ("zlib" ,zlib)))
    (arguments
     `(#:tests? #f
       #:strip-binaries? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (gcc:lib (assoc-ref inputs "gcc:lib"))
                    (libc (assoc-ref inputs "libc"))
                    (zlib (assoc-ref inputs "zlib"))
                    (ld-so (string-append libc ,(glibc-dynamic-linker)))
                    (rpath (string-append out "/lib:" zlib "/lib:"
                                          libc "/lib:" gcc:lib "/lib"))
                    (rustc (string-append out "/bin/rustc"))
                    (rustdoc (string-append out "/bin/rustdoc")))
               (system* "bash" "install.sh"
                        (string-append "--prefix=" out)
                        (string-append "--components=rustc,"
                                       "rust-std-" %host-type))
               (for-each (lambda (file)
                           (system* "patchelf" "--set-rpath" rpath file))
                         (cons* rustc rustdoc (find-files out "\\.so$")))
               (for-each (lambda (file)
                           (system* "patchelf" "--set-interpreter" ld-so file))
                         (list rustc rustdoc))))))))
    (home-page "https://www.rust-lang.org")
    (synopsis "Prebuilt rust compiler")
    (description "This package provides a pre-built @command{rustc} compiler,
which can in turn be used to build the final Rust compiler.")
    (license license:asl2.0)))

(define cargo-bootstrap
  (package
    (name "cargo-bootstrap")
    (version (cargo-version %rust-bootstrap-binaries-version 1))
    (source %rust-bootstrap-binaries)
    (build-system gnu-build-system)
    (native-inputs
     `(("patchelf" ,patchelf)))
    (inputs
     `(("gcc:lib" ,(canonical-package gcc) "lib")))
    (arguments
     `(#:tests? #f
       #:strip-binaries? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (gcc:lib (assoc-ref inputs "gcc:lib"))
                    (libc (assoc-ref inputs "libc"))
                    (ld-so (string-append libc ,(glibc-dynamic-linker)))
                    (rpath (string-append out "/lib:" libc "/lib:"
                                          gcc:lib "/lib"))
                    (cargo (string-append out "/bin/cargo")))
               (system* "bash" "install.sh"
                        (string-append "--prefix=" out)
                        "--components=cargo")
               (system* "patchelf"
                        "--set-interpreter" ld-so
                        "--set-rpath" rpath
                        cargo)))))))
    (home-page "https://www.rust-lang.org")
    (synopsis "Prebuilt cargo package manager")
    (description "This package provides a pre-built @command{cargo} package
manager, which is required to build itself.")
    (license license:asl2.0)))

(define rust-bootstrap
  (package
    (name "rust-bootstrap")
    (version %rust-bootstrap-binaries-version)
    (source #f)
    (build-system trivial-build-system)
    (propagated-inputs
     `(("rustc-bootstrap" ,rustc-bootstrap)
       ("cargo-bootstrap" ,cargo-bootstrap)
       ("gcc" ,(canonical-package gcc))))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let ((out (assoc-ref %outputs "out"))
               (gcc (assoc-ref %build-inputs "gcc")))
           (mkdir-p (string-append out "/bin"))
           ;; Rust requires a C toolchain for linking. The prebuilt
           ;; binaries expect a compiler called cc. Thus symlink gcc
           ;; to cc.
           (symlink (string-append gcc "/bin/gcc")
                    (string-append out "/bin/cc"))))))
    (home-page "https://www.rust-lang.org")
    (synopsis "Rust bootstrapping meta package")
    (description "Meta package for a rust environment. Provides pre-compiled
rustc-bootstrap and cargo-bootstrap packages.")
    (license license:asl2.0)))

(define-public rustc
  (package
    (name "rustc")
    (version (rustc-version %rust-bootstrap-binaries-version 1))
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://static.rust-lang.org/dist/"
                    "rustc-" version "-src.tar.gz"))
              (sha256
               (base32
                "1lrzzp0nh7s61wgfs2h6ilaqi6iq89f1pd1yaf65l87bssyl4ylb"))
              (modules '((guix build utils)))
              (snippet
               `(begin
                  (delete-file-recursively "src/llvm")
                  #t))))
    (build-system gnu-build-system)
    (native-inputs
     `(("bison" ,bison) ; For the tests
       ("cmake" ,cmake)
       ("flex" ,flex) ; For the tests
       ("gdb" ,gdb)   ; For the tests
       ("git" ,git)
       ("procps" ,procps) ; For the tests
       ("python-2" ,python-2)
       ("rust-bootstrap" ,rust-bootstrap)
       ("which" ,which)))
    (inputs
     `(("jemalloc" ,jemalloc-4.5.0)
       ("llvm" ,llvm-3.9.1)))
    (arguments
     `(#:imported-modules ,%cargo-build-system-modules ;for `generate-checksums'
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-env
           (lambda _
             (setenv "SHELL" (which "sh"))
             (setenv "CONFIG_SHELL" (which "sh"))
             ;; guix llvm-3.9.1 package installs only shared libraries
             (setenv "LLVM_LINK_SHARED" "1")
             #t))
         (add-after 'unpack 'patch-tests
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((bash (assoc-ref inputs "bash")))
               (substitute* "src/build_helper/lib.rs"
                 ;; In same folder as gcc there is only "gcc-ar" utility
                 (("file\\.push_str\\(\"ar\"\\);") "file.push_str(\"gcc-ar\");"))
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
                 (("fn test_process_mask") "#[ignore]\nfn test_process_mask"))
               ;; Our ld-wrapper cannot process non-UTF8 bytes in LIBRARY_PATH.
               ;; <https://lists.gnu.org/archive/html/guix-devel/2017-06/msg00193.html>
               (delete-file-recursively "src/test/run-make/linker-output-non-utf8")
               (substitute* "src/build_helper/lib.rs"
                 ;; Bug in Rust code.
                 ;; Current implementation assume that if dst not exist then it's mtime
                 ;; is 0, but in same time "src" have 0 mtime in guix build!
                 (("let threshold = mtime\\(dst\\);")
                  "if !dst.exists() {\nreturn false\n}\n let threshold = mtime(dst);"))
               #t)))
         (add-after 'patch-source-shebangs 'patch-cargo-checksums
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/Cargo.lock"
               (("(\"checksum .* = )\".*\"" all name)
                (string-append name "\"" ,%cargo-reference-hash "\"")))
             (for-each
              (lambda (filename)
                (use-modules (guix build cargo-build-system))
                (delete-file filename)
                (let* ((dir (dirname filename)))
                  (display (string-append
                            "patch-cargo-checksums: generate-checksums for "
                            dir "\n"))
                  (generate-checksums dir ,%cargo-reference-project-file)))
              (find-files "src/vendor" ".cargo-checksum.json"))
             #t))
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
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
python = \"" python "/bin/python2" "\"
gdb = \"" gdb "/bin/gdb" "\"
vendor = true
submodules = false
[install]
prefix = \"" out "\"
[rust]
default-linker = \"" gcc "/bin/gcc" "\"
default-ar = \"" binutils "/bin/ar" "\"
channel = \"stable\"
rpath = true
# There is 2 failed codegen tests:
# codegen/mainsubprogram.rs and codegen/mainsubprogramstart.rs
# This tests required patched LLVM
codegen-tests = false
[target." %host-type "]
llvm-config = \"" llvm "/bin/llvm-config" "\"
cc = \"" gcc "/bin/gcc" "\"
cxx = \"" gcc "/bin/g++" "\"
jemalloc = \"" jemalloc "/lib/libjemalloc_pic.a" "\"
[dist]
") port)))
               #t)))
         (add-before 'build 'reset-timestamps-after-changes
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (define ref (stat "README.md"))
             (for-each
              (lambda (filename)
                (set-file-time filename ref))
              (find-files "." #:directories? #t))
             #t))
         (replace 'build
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (zero? (system* "./x.py" "build"))))
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (zero? (system* "./x.py" "test"))))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (zero? (system* "./x.py" "install"))))
         (add-after 'install 'wrap-rustc
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (libc (assoc-ref inputs "libc"))
                   (ld-wrapper (assoc-ref inputs "ld-wrapper")))
               ;; Let gcc find ld and libc startup files.
               (wrap-program (string-append out "/bin/rustc")
                 `("PATH" ":" prefix (,(string-append ld-wrapper "/bin")))
                 `("LIBRARY_PATH" ":" suffix (,(string-append libc "/lib"))))
               #t))))))
    ;; rustc invokes gcc, so we need to set its search paths accordingly.
    (native-search-paths (package-native-search-paths gcc))
    (synopsis "Compiler for the Rust progamming language")
    (description "Rust is a systems programming language that provides memory
safety and thread safety guarantees.")
    (home-page "https://www.rust-lang.org")
    ;; Dual licensed.
    (license (list license:asl2.0 license:expat))))

;; This tries very hard not to get into a cyclic dependency like this:
;;   cargo <- cargo-build-system <- cargo.
(define-public cargo
  (package
    (name "cargo")
    (version (cargo-version (rustc-version %rust-bootstrap-binaries-version) 0))
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/rust-lang/cargo/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0kr7rml7v2bm7zl8jcb3056h63zpyy9m08s212i8vfwxf6lf5fzl"))))
    (build-system cargo-build-system)
    (propagated-inputs
     `(("cmake" ,cmake)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("curl" ,curl)
       ("libgit2" ,libgit2)
       ("libssh2" ,libssh2)
       ("openssl" ,openssl)
       ("python-2" ,python-2)
       ("zlib" ,zlib)))
    (native-inputs
     `(("git" ,git) ; required for tests
       ;; Next dependencies generated with next command:
       ;; cat Cargo.lock | awk '
       ;;   /^"checksum/
       ;;   { oname=name=$2; vers=$3; hash=$6;
       ;;     if (ns[name] != 1) { ns[name]=1; } else { name = name "-" vers; }
       ;;     print "       (\"rust-" name "\"";
       ;;     print "        ,(origin";
       ;;     print "           (method url-fetch)";
       ;;     print "           (uri (crate-uri \"" oname "\" \"" vers "\"))";
       ;;     print "           (file-name \"rust-" oname "\-\" vers "\")
       ;;     print "           (sha256";
       ;;     print "            (base16-string->bytevector";
       ;;     print "             " hash "))))"
       ;;   }'
       ("rust-advapi32-sys"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "advapi32-sys" "0.2.0"))
           (file-name "rust-advapi32-sys-0.2.0")
           (sha256
            (base16-string->bytevector
             "e06588080cb19d0acb6739808aafa5f26bfb2ca015b2b6370028b44cf7cb8a9a"))))
       ("rust-aho-corasick"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "aho-corasick" "0.5.3"))
           (file-name "rust-aho-corasick-0.5.3")
           (sha256
            (base16-string->bytevector
             "ca972c2ea5f742bfce5687b9aef75506a764f61d37f8f649047846a9686ddb66"))))
       ("rust-aho-corasick-0.6.3"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "aho-corasick" "0.6.3"))
           (file-name "rust-aho-corasick-0.6.3")
           (sha256
            (base16-string->bytevector
             "500909c4f87a9e52355b26626d890833e9e1d53ac566db76c36faa984b889699"))))
       ("rust-atty"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "atty" "0.2.3"))
           (file-name "rust-atty-0.2.3")
           (sha256
            (base16-string->bytevector
             "21e50800ec991574876040fff8ee46b136a53e985286fbe6a3bdfe6421b78860"))))
       ("rust-backtrace"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "backtrace" "0.3.3"))
           (file-name "rust-backtrace-0.3.3")
           (sha256
            (base16-string->bytevector
             "99f2ce94e22b8e664d95c57fff45b98a966c2252b60691d0b7aeeccd88d70983"))))
       ("rust-backtrace-sys"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "backtrace-sys" "0.1.14"))
           (file-name "rust-backtrace-sys-0.1.14")
           (sha256
            (base16-string->bytevector
             "c63ea141ef8fdb10409d0f5daf30ac51f84ef43bff66f16627773d2a292cd189"))))
       ("rust-bitflags"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "bitflags" "0.7.0"))
           (file-name "rust-bitflags-0.7.0")
           (sha256
            (base16-string->bytevector
             "aad18937a628ec6abcd26d1489012cc0e18c21798210f491af69ded9b881106d"))))
       ("rust-bitflags-0.9.1"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "bitflags" "0.9.1"))
           (file-name "rust-bitflags-0.9.1")
           (sha256
            (base16-string->bytevector
             "4efd02e230a02e18f92fc2735f44597385ed02ad8f831e7c1c1156ee5e1ab3a5"))))
       ("rust-bufstream"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "bufstream" "0.1.3"))
           (file-name "rust-bufstream-0.1.3")
           (sha256
            (base16-string->bytevector
             "f2f382711e76b9de6c744cc00d0497baba02fb00a787f088c879f01d09468e32"))))
       ("rust-cc"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "cc" "1.0.0"))
           (file-name "rust-cc-1.0.0")
           (sha256
            (base16-string->bytevector
             "7db2f146208d7e0fbee761b09cd65a7f51ccc38705d4e7262dad4d73b12a76b1"))))
       ("rust-cfg-if"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "cfg-if" "0.1.2"))
           (file-name "rust-cfg-if-0.1.2")
           (sha256
            (base16-string->bytevector
             "d4c819a1287eb618df47cc647173c5c4c66ba19d888a6e50d605672aed3140de"))))
       ("rust-cmake"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "cmake" "0.1.26"))
           (file-name "rust-cmake-0.1.26")
           (sha256
            (base16-string->bytevector
             "357c07e7a1fc95732793c1edb5901e1a1f305cfcf63a90eb12dbd22bdb6b789d"))))
       ("rust-commoncrypto"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "commoncrypto" "0.2.0"))
           (file-name "rust-commoncrypto-0.2.0")
           (sha256
            (base16-string->bytevector
             "d056a8586ba25a1e4d61cb090900e495952c7886786fc55f909ab2f819b69007"))))
       ("rust-commoncrypto-sys"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "commoncrypto-sys" "0.2.0"))
           (file-name "rust-commoncrypto-sys-0.2.0")
           (sha256
            (base16-string->bytevector
             "1fed34f46747aa73dfaa578069fd8279d2818ade2b55f38f22a9401c7f4083e2"))))
       ("rust-conv"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "conv" "0.3.3"))
           (file-name "rust-conv-0.3.3")
           (sha256
            (base16-string->bytevector
             "78ff10625fd0ac447827aa30ea8b861fead473bb60aeb73af6c1c58caf0d1299"))))
       ("rust-core-foundation"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "core-foundation" "0.4.4"))
           (file-name "rust-core-foundation-0.4.4")
           (sha256
            (base16-string->bytevector
             "5909502e547762013619f4c4e01cc7393c20fe2d52d7fa471c1210adb2320dc7"))))
       ("rust-core-foundation-sys"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "core-foundation-sys" "0.4.4"))
           (file-name "rust-core-foundation-sys-0.4.4")
           (sha256
            (base16-string->bytevector
             "bc9fb3d6cb663e6fd7cf1c63f9b144ee2b1e4a78595a0451dd34bff85b9a3387"))))
       ("rust-crossbeam"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "crossbeam" "0.2.10"))
           (file-name "rust-crossbeam-0.2.10")
           (sha256
            (base16-string->bytevector
             "0c5ea215664ca264da8a9d9c3be80d2eaf30923c259d03e870388eb927508f97"))))
       ("rust-crossbeam-0.3.0"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "crossbeam" "0.3.0"))
           (file-name "rust-crossbeam-0.3.0")
           (sha256
            (base16-string->bytevector
             "8837ab96533202c5b610ed44bc7f4183e7957c1c8f56e8cc78bb098593c8ba0a"))))
       ("rust-crypto-hash"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "crypto-hash" "0.3.0"))
           (file-name "rust-crypto-hash-0.3.0")
           (sha256
            (base16-string->bytevector
             "34903878eec1694faf53cae8473a088df333181de421d4d3d48061d6559fe602"))))
       ("rust-curl"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "curl" "0.4.8"))
           (file-name "rust-curl-0.4.8")
           (sha256
            (base16-string->bytevector
             "7034c534a1d7d22f7971d6088aa9d281d219ef724026c3428092500f41ae9c2c"))))
       ("rust-curl-sys"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "curl-sys" "0.3.15"))
           (file-name "rust-curl-sys-0.3.15")
           (sha256
            (base16-string->bytevector
             "4bee31aa3a079d5f3ff9579ea4dcfb1b1a17a40886f5f467436d383e78134b55"))))
       ("rust-custom_derive"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "custom_derive" "0.1.7"))
           (file-name "rust-custom_derive-0.1.7")
           (sha256
            (base16-string->bytevector
             "ef8ae57c4978a2acd8b869ce6b9ca1dfe817bff704c220209fdef2c0b75a01b9"))))
       ("rust-dbghelp-sys"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "dbghelp-sys" "0.2.0"))
           (file-name "rust-dbghelp-sys-0.2.0")
           (sha256
            (base16-string->bytevector
             "97590ba53bcb8ac28279161ca943a924d1fd4a8fb3fa63302591647c4fc5b850"))))
       ("rust-docopt"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "docopt" "0.8.1"))
           (file-name "rust-docopt-0.8.1")
           (sha256
            (base16-string->bytevector
             "3b5b93718f8b3e5544fcc914c43de828ca6c6ace23e0332c6080a2977b49787a"))))
       ("rust-dtoa"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "dtoa" "0.4.2"))
           (file-name "rust-dtoa-0.4.2")
           (sha256
            (base16-string->bytevector
             "09c3753c3db574d215cba4ea76018483895d7bff25a31b49ba45db21c48e50ab"))))
       ("rust-env_logger"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "env_logger" "0.4.3"))
           (file-name "rust-env_logger-0.4.3")
           (sha256
            (base16-string->bytevector
             "3ddf21e73e016298f5cb37d6ef8e8da8e39f91f9ec8b0df44b7deb16a9f8cd5b"))))
       ("rust-error-chain"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "error-chain" "0.11.0"))
           (file-name "rust-error-chain-0.11.0")
           (sha256
            (base16-string->bytevector
             "ff511d5dc435d703f4971bc399647c9bc38e20cb41452e3b9feb4765419ed3f3"))))
       ("rust-filetime"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "filetime" "0.1.12"))
           (file-name "rust-filetime-0.1.12")
           (sha256
            (base16-string->bytevector
             "6ab199bf38537c6f38792669e081e0bb278b9b7405bba2642e4e5d15bf732c0e"))))
       ("rust-flate2"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "flate2" "0.2.20"))
           (file-name "rust-flate2-0.2.20")
           (sha256
            (base16-string->bytevector
             "e6234dd4468ae5d1e2dbb06fe2b058696fdc50a339c68a393aefbf00bc81e423"))))
       ("rust-fnv"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "fnv" "1.0.5"))
           (file-name "rust-fnv-1.0.5")
           (sha256
            (base16-string->bytevector
             "6cc484842f1e2884faf56f529f960cc12ad8c71ce96cc7abba0a067c98fee344"))))
       ("rust-foreign-types"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "foreign-types" "0.2.0"))
           (file-name "rust-foreign-types-0.2.0")
           (sha256
            (base16-string->bytevector
             "3e4056b9bd47f8ac5ba12be771f77a0dae796d1bbaaf5fd0b9c2d38b69b8a29d"))))
       ("rust-fs2"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "fs2" "0.4.2"))
           (file-name "rust-fs2-0.4.2")
           (sha256
            (base16-string->bytevector
             "9ab76cfd2aaa59b7bf6688ad9ba15bbae64bff97f04ea02144cfd3443e5c2866"))))
       ("rust-git2"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "git2" "0.6.8"))
           (file-name "rust-git2-0.6.8")
           (sha256
            (base16-string->bytevector
             "0c1c0203d653f4140241da0c1375a404f0a397249ec818cd2076c6280c50f6fa"))))
       ("rust-git2-curl"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "git2-curl" "0.7.0"))
           (file-name "rust-git2-curl-0.7.0")
           (sha256
            (base16-string->bytevector
             "68676bc784bf0bef83278898929bf64a251e87c0340723d0b93fa096c9c5bf8e"))))
       ("rust-glob"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "glob" "0.2.11"))
           (file-name "rust-glob-0.2.11")
           (sha256
            (base16-string->bytevector
             "8be18de09a56b60ed0edf84bc9df007e30040691af7acd1c41874faac5895bfb"))))
       ("rust-globset"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "globset" "0.2.0"))
           (file-name "rust-globset-0.2.0")
           (sha256
            (base16-string->bytevector
             "feeb1b6840809ef5efcf7a4a990bc4e1b7ee3df8cf9e2379a75aeb2ba42ac9c3"))))
       ("rust-hamcrest"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "hamcrest" "0.1.1"))
           (file-name "rust-hamcrest-0.1.1")
           (sha256
            (base16-string->bytevector
             "bf088f042a467089e9baa4972f57f9247e42a0cc549ba264c7a04fbb8ecb89d4"))))
       ("rust-hex"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "hex" "0.2.0"))
           (file-name "rust-hex-0.2.0")
           (sha256
            (base16-string->bytevector
             "d6a22814455d41612f41161581c2883c0c6a1c41852729b17d5ed88f01e153aa"))))
       ("rust-home"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "home" "0.3.0"))
           (file-name "rust-home-0.3.0")
           (sha256
            (base16-string->bytevector
             "9f25ae61099d8f3fee8b483df0bd4ecccf4b2731897aad40d50eca1b641fe6db"))))
       ("rust-idna"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "idna" "0.1.4"))
           (file-name "rust-idna-0.1.4")
           (sha256
            (base16-string->bytevector
             "014b298351066f1512874135335d62a789ffe78a9974f94b43ed5621951eaf7d"))))
       ("rust-ignore"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "ignore" "0.2.2"))
           (file-name "rust-ignore-0.2.2")
           (sha256
            (base16-string->bytevector
             "b3fcaf2365eb14b28ec7603c98c06cc531f19de9eb283d89a3dff8417c8c99f5"))))
       ("rust-itoa"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "itoa" "0.3.4"))
           (file-name "rust-itoa-0.3.4")
           (sha256
            (base16-string->bytevector
             "8324a32baf01e2ae060e9de58ed0bc2320c9a2833491ee36cd3b4c414de4db8c"))))
       ("rust-jobserver"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "jobserver" "0.1.6"))
           (file-name "rust-jobserver-0.1.6")
           (sha256
            (base16-string->bytevector
             "443ae8bc0af6c106e6e8b77e04684faecc1a5ce94e058f4c2b0a037b0ea1b133"))))
       ("rust-kernel32-sys"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "kernel32-sys" "0.2.2"))
           (file-name "rust-kernel32-sys-0.2.2")
           (sha256
            (base16-string->bytevector
             "7507624b29483431c0ba2d82aece8ca6cdba9382bff4ddd0f7490560c056098d"))))
       ("rust-lazy_static"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "lazy_static" "0.2.9"))
           (file-name "rust-lazy_static-0.2.9")
           (sha256
            (base16-string->bytevector
             "c9e5e58fa1a4c3b915a561a78a22ee0cac6ab97dca2504428bc1cb074375f8d5"))))
       ("rust-libc"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "libc" "0.2.31"))
           (file-name "rust-libc-0.2.31")
           (sha256
            (base16-string->bytevector
             "d1419b2939a0bc44b77feb34661583c7546b532b192feab36249ab584b86856c"))))
       ("rust-libgit2-sys"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "libgit2-sys" "0.6.16"))
           (file-name "rust-libgit2-sys-0.6.16")
           (sha256
            (base16-string->bytevector
             "6f74b4959cef96898f5123148724fc7dee043b9a6b99f219d948851bfbe53cb2"))))
       ("rust-libssh2-sys"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "libssh2-sys" "0.2.6"))
           (file-name "rust-libssh2-sys-0.2.6")
           (sha256
            (base16-string->bytevector
             "0db4ec23611747ef772db1c4d650f8bd762f07b461727ec998f953c614024b75"))))
       ("rust-libz-sys"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "libz-sys" "1.0.17"))
           (file-name "rust-libz-sys-1.0.17")
           (sha256
            (base16-string->bytevector
             "44ebbc760fd2d2f4d93de09a0e13d97e057612052e871da9985cedcb451e6bd5"))))
       ("rust-log"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "log" "0.3.8"))
           (file-name "rust-log-0.3.8")
           (sha256
            (base16-string->bytevector
             "880f77541efa6e5cc74e76910c9884d9859683118839d6a1dc3b11e63512565b"))))
       ("rust-magenta"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "magenta" "0.1.1"))
           (file-name "rust-magenta-0.1.1")
           (sha256
            (base16-string->bytevector
             "4bf0336886480e671965f794bc9b6fce88503563013d1bfb7a502c81fe3ac527"))))
       ("rust-magenta-sys"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "magenta-sys" "0.1.1"))
           (file-name "rust-magenta-sys-0.1.1")
           (sha256
            (base16-string->bytevector
             "40d014c7011ac470ae28e2f76a02bfea4a8480f73e701353b49ad7a8d75f4699"))))
       ("rust-matches"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "matches" "0.1.6"))
           (file-name "rust-matches-0.1.6")
           (sha256
            (base16-string->bytevector
             "100aabe6b8ff4e4a7e32c1c13523379802df0772b82466207ac25b013f193376"))))
       ("rust-memchr"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "memchr" "0.1.11"))
           (file-name "rust-memchr-0.1.11")
           (sha256
            (base16-string->bytevector
             "d8b629fb514376c675b98c1421e80b151d3817ac42d7c667717d282761418d20"))))
       ("rust-memchr-1.0.1"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "memchr" "1.0.1"))
           (file-name "rust-memchr-1.0.1")
           (sha256
            (base16-string->bytevector
             "1dbccc0e46f1ea47b9f17e6d67c5a96bd27030519c519c9c91327e31275a47b4"))))
       ("rust-miniz-sys"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "miniz-sys" "0.1.10"))
           (file-name "rust-miniz-sys-0.1.10")
           (sha256
            (base16-string->bytevector
             "609ce024854aeb19a0ef7567d348aaa5a746b32fb72e336df7fcc16869d7e2b4"))))
       ("rust-miow"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "miow" "0.2.1"))
           (file-name "rust-miow-0.2.1")
           (sha256
            (base16-string->bytevector
             "8c1f2f3b1cf331de6896aabf6e9d55dca90356cc9960cca7eaaf408a355ae919"))))
       ("rust-net2"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "net2" "0.2.31"))
           (file-name "rust-net2-0.2.31")
           (sha256
            (base16-string->bytevector
             "3a80f842784ef6c9a958b68b7516bc7e35883c614004dd94959a4dca1b716c09"))))
       ("rust-num"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "num" "0.1.40"))
           (file-name "rust-num-0.1.40")
           (sha256
            (base16-string->bytevector
             "a311b77ebdc5dd4cf6449d81e4135d9f0e3b153839ac90e648a8ef538f923525"))))
       ("rust-num-bigint"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "num-bigint" "0.1.40"))
           (file-name "rust-num-bigint-0.1.40")
           (sha256
            (base16-string->bytevector
             "8fd0f8dbb4c0960998958a796281d88c16fbe68d87b1baa6f31e2979e81fd0bd"))))
       ("rust-num-complex"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "num-complex" "0.1.40"))
           (file-name "rust-num-complex-0.1.40")
           (sha256
            (base16-string->bytevector
             "503e668405c5492d67cf662a81e05be40efe2e6bcf10f7794a07bd9865e704e6"))))
       ("rust-num-integer"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "num-integer" "0.1.35"))
           (file-name "rust-num-integer-0.1.35")
           (sha256
            (base16-string->bytevector
             "d1452e8b06e448a07f0e6ebb0bb1d92b8890eea63288c0b627331d53514d0fba"))))
       ("rust-num-iter"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "num-iter" "0.1.34"))
           (file-name "rust-num-iter-0.1.34")
           (sha256
            (base16-string->bytevector
             "7485fcc84f85b4ecd0ea527b14189281cf27d60e583ae65ebc9c088b13dffe01"))))
       ("rust-num-rational"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "num-rational" "0.1.39"))
           (file-name "rust-num-rational-0.1.39")
           (sha256
            (base16-string->bytevector
             "288629c76fac4b33556f4b7ab57ba21ae202da65ba8b77466e6d598e31990790"))))
       ("rust-num-traits"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "num-traits" "0.1.40"))
           (file-name "rust-num-traits-0.1.40")
           (sha256
            (base16-string->bytevector
             "99843c856d68d8b4313b03a17e33c4bb42ae8f6610ea81b28abe076ac721b9b0"))))
       ("rust-num_cpus"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "num_cpus" "1.7.0"))
           (file-name "rust-num_cpus-1.7.0")
           (sha256
            (base16-string->bytevector
             "514f0d73e64be53ff320680ca671b64fe3fb91da01e1ae2ddc99eb51d453b20d"))))
       ("rust-openssl"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "openssl" "0.9.19"))
           (file-name "rust-openssl-0.9.19")
           (sha256
            (base16-string->bytevector
             "816914b22eb15671d62c73442a51978f311e911d6a6f6cbdafa6abce1b5038fc"))))
       ("rust-openssl-probe"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "openssl-probe" "0.1.1"))
           (file-name "rust-openssl-probe-0.1.1")
           (sha256
            (base16-string->bytevector
             "d98df0270d404ccd3c050a41d579c52d1db15375168bb3471e04ec0f5f378daf"))))
       ("rust-openssl-sys"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "openssl-sys" "0.9.19"))
           (file-name "rust-openssl-sys-0.9.19")
           (sha256
            (base16-string->bytevector
             "1e4c63a7d559c1e5afa6d6a9e6fa34bbc5f800ffc9ae08b72c605420b0c4f5e8"))))
       ("rust-percent-encoding"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "percent-encoding" "1.0.0"))
           (file-name "rust-precent-encoding-1.0.0")
           (sha256
            (base16-string->bytevector
             "de154f638187706bde41d9b4738748933d64e6b37bdbffc0b47a97d16a6ae356"))))
       ("rust-pkg-config"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "pkg-config" "0.3.9"))
           (file-name "rust-pkg-config-0.3.9")
           (sha256
            (base16-string->bytevector
             "3a8b4c6b8165cd1a1cd4b9b120978131389f64bdaf456435caa41e630edba903"))))
       ("rust-psapi-sys"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "psapi-sys" "0.1.0"))
           (file-name "rust-psapi-sys-0.1.0")
           (sha256
            (base16-string->bytevector
             "abcd5d1a07d360e29727f757a9decb3ce8bc6e0efa8969cfaad669a8317a2478"))))
       ("rust-quote"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "quote" "0.3.15"))
           (file-name "rust-quote-0.3.15")
           (sha256
            (base16-string->bytevector
             "7a6e920b65c65f10b2ae65c831a81a073a89edd28c7cce89475bff467ab4167a"))))
       ("rust-rand"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "rand" "0.3.16"))
           (file-name "rust-rand-0.3.16")
           (sha256
            (base16-string->bytevector
             "eb250fd207a4729c976794d03db689c9be1d634ab5a1c9da9492a13d8fecbcdf"))))
       ("rust-redox_syscall"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "redox_syscall" "0.1.31"))
           (file-name "rust-redox_syscall-0.1.31")
           (sha256
            (base16-string->bytevector
             "8dde11f18c108289bef24469638a04dce49da56084f2d50618b226e47eb04509"))))
       ("rust-redox_termios"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "redox_termios" "0.1.1"))
           (file-name "rust-redox_termios-0.1.1")
           (sha256
            (base16-string->bytevector
             "7e891cfe48e9100a70a3b6eb652fef28920c117d366339687bd5576160db0f76"))))
       ("rust-regex"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "regex" "0.1.80"))
           (file-name "rust-regex-0.1.80")
           (sha256
            (base16-string->bytevector
             "4fd4ace6a8cf7860714a2c2280d6c1f7e6a413486c13298bbc86fd3da019402f"))))
       ("rust-regex-0.2.2"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "regex" "0.2.2"))
           (file-name "rust-regex-0.2.2")
           (sha256
            (base16-string->bytevector
             "1731164734096285ec2a5ec7fea5248ae2f5485b3feeb0115af4fda2183b2d1b"))))
       ("rust-regex-syntax"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "regex-syntax" "0.3.9"))
           (file-name "rust-regex-syntax-0.3.9")
           (sha256
            (base16-string->bytevector
             "f9ec002c35e86791825ed294b50008eea9ddfc8def4420124fbc6b08db834957"))))
       ("rust-regex-syntax-0.4.1"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "regex-syntax" "0.4.1"))
           (file-name "rust-regex-syntax-0.4.1")
           (sha256
            (base16-string->bytevector
             "ad890a5eef7953f55427c50575c680c42841653abd2b028b68cd223d157f62db"))))
       ("rust-rustc-demangle"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "rustc-demangle" "0.1.5"))
           (file-name "rust-rustc-demangle-0.1.5")
           (sha256
            (base16-string->bytevector
             "aee45432acc62f7b9a108cc054142dac51f979e69e71ddce7d6fc7adf29e817e"))))
       ("rust-rustc-serialize"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "rustc-serialize" "0.3.24"))
           (file-name "rust-rustc-serialize-0.3.24")
           (sha256
            (base16-string->bytevector
             "dcf128d1287d2ea9d80910b5f1120d0b8eede3fbf1abe91c40d39ea7d51e6fda"))))
       ("rust-same-file"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "same-file" "0.1.3"))
           (file-name "rust-same-file-0.1.3")
           (sha256
            (base16-string->bytevector
             "d931a44fdaa43b8637009e7632a02adc4f2b2e0733c08caa4cf00e8da4a117a7"))))
       ("rust-scoped-tls"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "scoped-tls" "0.1.0"))
           (file-name "rust-scoped-tls-0.1.0")
           (sha256
            (base16-string->bytevector
             "f417c22df063e9450888a7561788e9bd46d3bb3c1466435b4eccb903807f147d"))))
       ("rust-scopeguard"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "scopeguard" "0.1.2"))
           (file-name "rust-scopeguard-0.1.2")
           (sha256
            (base16-string->bytevector
             "59a076157c1e2dc561d8de585151ee6965d910dd4dcb5dabb7ae3e83981a6c57"))))
       ("rust-semver"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "semver" "0.8.0"))
           (file-name "rust-semver-0.8.0")
           (sha256
            (base16-string->bytevector
             "bee2bc909ab2d8d60dab26e8cad85b25d795b14603a0dcb627b78b9d30b6454b"))))
       ("rust-semver-parser"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "semver-parser" "0.7.0"))
           (file-name "rust-semver-parser-0.7.0")
           (sha256
            (base16-string->bytevector
             "388a1df253eca08550bef6c72392cfe7c30914bf41df5269b68cbd6ff8f570a3"))))
       ("rust-serde"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "serde" "1.0.15"))
           (file-name "rust-serde-1.0.15")
           (sha256
            (base16-string->bytevector
             "6a7046c9d4c6c522d10b2d098f9bebe2bef227e0e74044d8c1bfcf6b476af799"))))
       ("rust-serde_derive"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "serde_derive" "1.0.15"))
           (file-name "rust-serde_derive-1.0.15")
           (sha256
            (base16-string->bytevector
             "1afcaae083fd1c46952a315062326bc9957f182358eb7da03b57ef1c688f7aa9"))))
       ("rust-serde_derive_internals"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "serde_derive_internals" "0.16.0"))
           (file-name "rust-serde_derive_internals-0.16.0")
           (sha256
            (base16-string->bytevector
             "bd381f6d01a6616cdba8530492d453b7761b456ba974e98768a18cad2cd76f58"))))
       ("rust-serde_ignored"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "serde_ignored" "0.0.4"))
           (file-name "rust-serde_ignored-0.0.4")
           (sha256
            (base16-string->bytevector
             "190e9765dcedb56be63b6e0993a006c7e3b071a016a304736e4a315dc01fb142"))))
       ("rust-serde_json"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "serde_json" "1.0.3"))
           (file-name "rust-serde_json-1.0.3")
           (sha256
            (base16-string->bytevector
             "d243424e06f9f9c39e3cd36147470fd340db785825e367625f79298a6ac6b7ac"))))
       ("rust-shell-escape"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "shell-escape" "0.1.3"))
           (file-name "rust-shell-escape-0.1.3")
           (sha256
            (base16-string->bytevector
             "dd5cc96481d54583947bfe88bf30c23d53f883c6cd0145368b69989d97b84ef8"))))
       ("rust-socket2"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "socket2" "0.2.3"))
           (file-name "rust-socket2-0.2.3")
           (sha256
            (base16-string->bytevector
             "9e76b159741052c7deaa9fd0b5ca6b5f79cecf525ed665abfe5002086c6b2791"))))
       ("rust-strsim"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "strsim" "0.6.0"))
           (file-name "rust-strsim-0.6.0")
           (sha256
            (base16-string->bytevector
             "b4d15c810519a91cf877e7e36e63fe068815c678181439f2f29e2562147c3694"))))
       ("rust-syn"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "syn" "0.11.11"))
           (file-name "rust-syn-0.11.11")
           (sha256
            (base16-string->bytevector
             "d3b891b9015c88c576343b9b3e41c2c11a51c219ef067b264bd9c8aa9b441dad"))))
       ("rust-synom"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "synom" "0.11.3"))
           (file-name "rust-synom-0.11.3")
           (sha256
            (base16-string->bytevector
             "a393066ed9010ebaed60b9eafa373d4b1baac186dd7e008555b0f702b51945b6"))))
       ("rust-tar"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "tar" "0.4.13"))
           (file-name "rust-tar-0.4.13")
           (sha256
            (base16-string->bytevector
             "281285b717926caa919ad905ef89c63d75805c7d89437fb873100925a53f2b1b"))))
       ("rust-tempdir"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "tempdir" "0.3.5"))
           (file-name "rust-tempdir-0.3.5")
           (sha256
            (base16-string->bytevector
             "87974a6f5c1dfb344d733055601650059a3363de2a6104819293baff662132d6"))))
       ("rust-termcolor"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "termcolor" "0.3.3"))
           (file-name "rust-termcolor-0.3.3")
           (sha256
            (base16-string->bytevector
             "9065bced9c3e43453aa3d56f1e98590b8455b341d2fa191a1090c0dd0b242c75"))))
       ("rust-termion"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "termion" "1.5.1"))
           (file-name "rust-termion-1.5.1")
           (sha256
            (base16-string->bytevector
             "689a3bdfaab439fd92bc87df5c4c78417d3cbe537487274e9b0b2dce76e92096"))))
       ("rust-thread-id"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "thread-id" "2.0.0"))
           (file-name "rust-thread-id-2.0.0")
           (sha256
            (base16-string->bytevector
             "a9539db560102d1cef46b8b78ce737ff0bb64e7e18d35b2a5688f7d097d0ff03"))))
       ("rust-thread_local"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "thread_local" "0.2.7"))
           (file-name "rust-thread_local-0.2.7")
           (sha256
            (base16-string->bytevector
             "8576dbbfcaef9641452d5cf0df9b0e7eeab7694956dd33bb61515fb8f18cfdd5"))))
       ("rust-thread_local-0.3.4"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "thread_local" "0.3.4"))
           (file-name "rust-thread_local-0.3.4")
           (sha256
            (base16-string->bytevector
             "1697c4b57aeeb7a536b647165a2825faddffb1d3bad386d507709bd51a90bb14"))))
       ("rust-toml"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "toml" "0.4.5"))
           (file-name "rust-toml-0.4.5")
           (sha256
            (base16-string->bytevector
             "a7540f4ffc193e0d3c94121edb19b055670d369f77d5804db11ae053a45b6e7e"))))
       ("rust-unicode-bidi"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "unicode-bidi" "0.3.4"))
           (file-name "rust-unicode-bidi-0.3.4")
           (sha256
            (base16-string->bytevector
             "49f2bd0c6468a8230e1db229cff8029217cf623c767ea5d60bfbd42729ea54d5"))))
       ("rust-unicode-normalization"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "unicode-normalization" "0.1.5"))
           (file-name "rust-unicode-normalization-0.1.5")
           (sha256
            (base16-string->bytevector
             "51ccda9ef9efa3f7ef5d91e8f9b83bbe6955f9bf86aec89d5cce2c874625920f"))))
       ("rust-unicode-xid"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "unicode-xid" "0.0.4"))
           (file-name "rust-unicode-xid-0.0.4")
           (sha256
            (base16-string->bytevector
             "8c1f860d7d29cf02cb2f3f359fd35991af3d30bac52c57d265a3c461074cb4dc"))))
       ("rust-unreachable"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "unreachable" "1.0.0"))
           (file-name "rust-unreachable-1.0.0")
           (sha256
            (base16-string->bytevector
             "382810877fe448991dfc7f0dd6e3ae5d58088fd0ea5e35189655f84e6814fa56"))))
       ("rust-url"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "url" "1.5.1"))
           (file-name "rust-url-1.5.1")
           (sha256
            (base16-string->bytevector
             "eeb819346883532a271eb626deb43c4a1bb4c4dd47c519bd78137c3e72a4fe27"))))
       ("rust-userenv-sys"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "userenv-sys" "0.2.0"))
           (file-name "rust-userenv-sys-0.2.0")
           (sha256
            (base16-string->bytevector
             "71d28ea36bbd9192d75bd9fa9b39f96ddb986eaee824adae5d53b6e51919b2f3"))))
       ("rust-utf8-ranges"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "utf8-ranges" "0.1.3"))
           (file-name "rust-utf8-ranges-0.1.3")
           (sha256
            (base16-string->bytevector
             "a1ca13c08c41c9c3e04224ed9ff80461d97e121589ff27c753a16cb10830ae0f"))))
       ("rust-utf8-ranges-1.0.0"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "utf8-ranges" "1.0.0"))
           (file-name "rust-utf8-ranges-1.0.0")
           (sha256
            (base16-string->bytevector
             "662fab6525a98beff2921d7f61a39e7d59e0b425ebc7d0d9e66d316e55124122"))))
       ("rust-vcpkg"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "vcpkg" "0.2.2"))
           (file-name "rust-vcpkg-0.2.2")
           (sha256
            (base16-string->bytevector
             "9e0a7d8bed3178a8fb112199d466eeca9ed09a14ba8ad67718179b4fd5487d0b"))))
       ("rust-void"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "void" "1.0.2"))
           (file-name "rust-void-1.0.2")
           (sha256
            (base16-string->bytevector
             "6a02e4885ed3bc0f2de90ea6dd45ebcbb66dacffe03547fadbb0eeae2770887d"))))
       ("rust-walkdir"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "walkdir" "1.0.7"))
           (file-name "rust-walkdir-1.0.7")
           (sha256
            (base16-string->bytevector
             "bb08f9e670fab86099470b97cd2b252d6527f0b3cc1401acdb595ffc9dd288ff"))))
       ("rust-winapi"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "winapi" "0.2.8"))
           (file-name "rust-winapi-0.2.8")
           (sha256
            (base16-string->bytevector
             "167dc9d6949a9b857f3451275e911c3f44255842c1f7a76f33c55103a909087a"))))
       ("rust-winapi-build"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "winapi-build" "0.1.1"))
           (file-name "rust-winapi-build-0.1.1")
           (sha256
            (base16-string->bytevector
             "2d315eee3b34aca4797b2da6b13ed88266e6d612562a0c46390af8299fc699bc"))))
       ("rust-wincolor"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "wincolor" "0.1.4"))
           (file-name "rust-wincolor-0.1.4")
           (sha256
            (base16-string->bytevector
             "a39ee4464208f6430992ff20154216ab2357772ac871d994c51628d60e58b8b0"))))
       ("rust-ws2_32-sys"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "ws2_32-sys" "0.2.1"))
           (file-name "rust-ws2_32-sys-0.2.1")
           (sha256
            (base16-string->bytevector
             "d59cefebd0c892fa2dd6de581e937301d8552cb44489cdff035c6187cb63fa5e"))))))
    (arguments
     `(#:cargo ,cargo-bootstrap
       #:rustc ,rustc ; Force to use rustc from current file
       #:modules
       ((ice-9 match)
        (srfi srfi-1) ; 'every
        (guix build utils)
        (guix build cargo-build-system))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-dependencies
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (define (unpack source target)
               (mkdir-p target)
               (with-directory-excursion target
                 (zero? (system* "tar" "xf"
                                 source
                                 "--strip-components=1"))))
             (define (touch file-name)
               (call-with-output-file file-name (const #t)))
             (define (install-rust-library entry)
               (match entry
                 ((name . src)
                  (if (string-prefix? "rust-" name)
                      (let* ((rust-length (string-length "rust-"))
                             (rust-name (string-drop name rust-length))
                             (rsrc (string-append "vendor/" rust-name))
                             (unpack-status (unpack src rsrc)))
                        (touch (string-append rsrc "/.cargo-ok"))
                        (generate-checksums rsrc src)
                        unpack-status)))
                 (_ #t)))
             (mkdir "vendor")
             (every install-rust-library inputs)))
         (add-after 'patch-generated-file-shebangs 'patch-cargo-checksums
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "Cargo.lock"
               (("(\"checksum .* = )\".*\"" all name)
                (string-append name "\"" ,%cargo-reference-hash "\"")))
             (for-each
              (lambda (filename)
                (use-modules (guix build cargo-build-system))
                (delete-file filename)
                (let* ((dir (dirname filename)))
                  (display (string-append
                            "patch-cargo-checksums: generate-checksums for "
                            dir "\n"))
                  (generate-checksums dir ,%cargo-reference-project-file)))
              (find-files "vendor" ".cargo-checksum.json"))
             #t))
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "tests/build.rs"
               (("/usr/bin/env") (which "env"))
               ;; Guix llvm compiled without asmjs-unknown-emscripten at all
               (("fn wasm32_final_outputs") "#[ignore]\nfn wasm32_final_outputs"))
             (substitute* "tests/death.rs"
               ;; Stuck when built in container
               (("fn ctrl_c_kills_everyone") "#[ignore]\nfn ctrl_c_kills_everyone"))
             (mkdir ".cargo")
             (call-with-output-file ".cargo/config"
               (lambda (port)
                 (display "
[source.crates-io]
registry = 'https://github.com/rust-lang/crates.io-index'
replace-with = 'vendored-sources'

[source.vendored-sources]
directory = 'vendor'
" port)))
             ;; Disable test for cross compilation support
             (setenv "CFG_DISABLE_CROSS_TESTS" "1")
             (setenv "SHELL" (which "sh"))
             (setenv "CONFIG_SHELL" (which "sh"))
             (setenv "CC" (string-append (assoc-ref inputs "gcc") "/bin/gcc"))
             #t)))))
    (home-page "https://github.com/rust-lang/cargo")
    (synopsis "Build tool and package manager for Rust")
    (description "Cargo is a tool that allows Rust projects to declare their
dependencies and ensures a reproducible build.")
    ;; Cargo is dual licensed Apache and MIT. Also contains
    ;; code from openssl which is GPL2 with linking exception.
    (license (list license:asl2.0 license:expat license:gpl2))))
