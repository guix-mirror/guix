;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016 Eric Le Bihan <eric.le.bihan.dev@free.fr>
;;; Copyright © 2016 ng0 <ng0@infotropique.org>
;;; Copyright © 2017 Ben Woodcroft <donttrustben@gmail.com>
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
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26))

;; Should be one less than the current released version.
(define %rust-bootstrap-binaries-version "1.15.0")

(define %rust-bootstrap-binaries
  (origin
    (method url-fetch)
    (uri (string-append
          "https://static.rust-lang.org/dist/"
          "rust-" %rust-bootstrap-binaries-version
          "-i686-unknown-linux-gnu.tar.gz"))
    (sha256
     (base32
      "0wmkfx8pxmkkw021mrq9s3xhra8f0daqdl6j56pxyn4w39i0rzrw"))))

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
       #:system "i686-linux"
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
                    (ld-so (string-append libc
                                          ,(glibc-dynamic-linker "i686-linux")))
                    (rpath (string-append out "/lib:" zlib "/lib:"
                                          libc "/lib:" gcc:lib "/lib"))
                    (rustc (string-append out "/bin/rustc"))
                    (rustdoc (string-append out "/bin/rustdoc")))
               (system* "bash" "install.sh"
                        (string-append "--prefix=" out)
                        (string-append "--components=rustc,"
                                       "rust-std-i686-unknown-linux-gnu"))
               (for-each (lambda (file)
                           (system* "patchelf" "--set-rpath" rpath file))
                         (cons* rustc rustdoc (find-files out "\\.so$")))
               (for-each (lambda (file)
                           (system* "patchelf" "--set-interpreter" ld-so file))
                         (list rustc rustdoc))))))))
    (supported-systems '("i686-linux" "x86_64-linux"))
    (home-page "https://www.rust-lang.org")
    (synopsis "Prebuilt rust compiler")
    (description "This package provides a pre-built @command{rustc} compiler,
which can in turn be used to build the final Rust compiler.")
    (license license:asl2.0)))

(define cargo-bootstrap
  (package
    (name "cargo-bootstrap")
    (version (cargo-version %rust-bootstrap-binaries-version))
    (source %rust-bootstrap-binaries)
    (build-system gnu-build-system)
    (native-inputs
     `(("patchelf" ,patchelf)))
    (inputs
     `(("gcc:lib" ,(canonical-package gcc) "lib")))
    (arguments
     `(#:tests? #f
       #:strip-binaries? #f
       #:system "i686-linux"
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (gcc:lib (assoc-ref inputs "gcc:lib"))
                    (libc (assoc-ref inputs "libc"))
                    (ld-so (string-append libc
                                          ,(glibc-dynamic-linker "i686-linux")))
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
    (supported-systems '("i686-linux" "x86_64-linux"))
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
    (version (rustc-version %rust-bootstrap-binaries-version))
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://static.rust-lang.org/dist/"
                    "rustc-" version "-src.tar.gz"))
              (sha256
               (base32
                "1d78jq7mc34n265by68amr9r4nzbiqrilfbwh7gx56ydn4gb6rpr"))
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
       ("git" ,git)
       ("procps" ,procps) ; For the tests
       ("python-2" ,python-2)
       ("rust-bootstrap" ,rust-bootstrap)
       ("which" ,which)))
    (inputs
     `(("jemalloc" ,jemalloc)
       ("llvm" ,llvm-3.9.1)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-configure
           (lambda _
             (substitute* "configure"
               (("/usr/bin/env") (which "env")) ; Detect target CPU correctly.
               (("probe_need CFG_CURL curl") "") ; Avoid curl build dependency.
               ;; Newer LLVM has a NVPTX (NVIDIA) backend which the Rust
               ;; Makefiles don't know about, causing a linker failure
               ;; if we don't intervene.
               ;; Therefore, we add NVPTX here.
               ;; See <https://github.com/rust-lang/rust/issues/40698>.
               ;; For the next release, we will have to use rustbuild.
               ;; Right now, rustbuild doesn't work yet.
               (("-DLLVM_TARGETS_TO_BUILD='")
                "-DLLVM_TARGETS_TO_BUILD='NVPTX;")) ; Make LLVM >= 3.8.1 work.
             (substitute* "src/tools/compiletest/src/util.rs"
               (("(\"amd64\", \"x86_64\"),") "(\"amd64\", \"x86_64\"),
(\"nvptx\", \"nvptx\"),")) ; Make LLVM >= 3.8.1 work.
             (substitute* "mk/main.mk"
               (("LLVM_OPTIONAL_COMPONENTS=")
                "LLVM_OPTIONAL_COMPONENTS=nvptx ")) ; Make LLVM >= 3.8.1 work.
             #t))
         (add-after 'unpack 'set-env
           (lambda _
             (setenv "SHELL" (which "sh"))
             (setenv "CONFIG_SHELL" (which "sh"))
             #t))
         (add-after 'unpack 'patch-tests
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((bash (assoc-ref inputs "bash")))
               (substitute* "src/tools/tidy/src/main.rs"
                 (("^.*cargo.*::check.*$") ""))
               (substitute* "src/libstd/process.rs"
                 ;; The newline is intentional.
                 ;; There's a line length "tidy" check in Rust which would
                 ;; fail otherwise.
                 (("\"/bin/sh\"") (string-append "
\"" bash "/bin/sh\"")))
               ;; See <https://lists.gnu.org/archive/html/guix-devel/2017-06/msg00222.html>.
               (substitute* "src/libstd/sys/unix/process/process_common.rs"
                 (("fn test_process_mask") "#[cfg_attr(target_os = \"linux\", ignore)]
fn test_process_mask"))
               ;; Our ld-wrapper cannot process non-UTF8 bytes in LIBRARY_PATH.
               ;; See <https://lists.gnu.org/archive/html/guix-devel/2017-06/msg00193.html>.
               (delete-file-recursively "src/test/run-make/linker-output-non-utf8")
               #t)))
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (gcc (assoc-ref inputs "gcc"))
                    (binutils (assoc-ref inputs "binutils"))
                    (python (assoc-ref inputs "python-2"))
                    (rustc (assoc-ref inputs "rustc-bootstrap"))
                    (llvm (assoc-ref inputs "llvm"))
                    (jemalloc (assoc-ref inputs "jemalloc"))
                    (flags (list
                            (string-append "--prefix=" out)
                            (string-append "--datadir=" out "/share")
                            (string-append "--infodir=" out "/share/info")
                            (string-append "--default-linker=" gcc "/bin/gcc")
                            (string-append "--default-ar=" binutils "/bin/ar")
                            (string-append "--python=" python "/bin/python2")
                            (string-append "--local-rust-root=" rustc)
                            (string-append "--llvm-root=" llvm)
                            (string-append "--jemalloc-root=" jemalloc "/lib")
                            "--release-channel=stable"
                            "--enable-rpath"
                            "--enable-local-rust"
                            "--disable-rustbuild" ; rustbuild doesn't work yet.
                            "--disable-manage-submodules")))
               ;; Rust uses a custom configure script (no autoconf).
               (zero? (apply system* "./configure" flags)))))
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
    (version (cargo-version (rustc-version %rust-bootstrap-binaries-version)))
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/rust-lang/cargo/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1y0zy8gk1ly0wh57y78fisk7cdd92qk0x7z664f6l7lzl2krqs7w"))))
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
     `(("rust-openssl"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "openssl" "0.9.6"))
           (sha256
            (base32
             "0g28g692gby6izp9qmnwnyxyhf9b0870yhd500p18j9l69lxl00c"))))
       ("rust-strsim"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "strsim" "0.5.1"))
           (sha256
            (base32
             "0bj4fsm1l2yqbfpspyvjf9m3m50pskapcddzm0ji9c74jbgnkh2h"))))
       ("rust-libc"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "libc" "0.2.18"))
           (sha256
            (base32
             "0w5cghr0wx3hi2sclk8r9iyzlbxsakil87ada40q2ykyhky24655"))))
       ("rust-bitflags"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "bitflags" "0.7.0"))
           (sha256
            (base32
             "0v8hh6wdkpk9my8z8442g4hqrqf05h0qj53dsay6mv18lqvqklda"))))
       ("rust-unicode-normalization"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "unicode-normalization" "0.1.2"))
           (sha256
            (base32
             "0whi4xxqcjfsz6ywyrfd5lhgk1a44c86qwgvfqcmzidshcpklr16"))))
       ("rust-rand"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "rand" "0.3.14"))
           (sha256
            (base32
             "1984zvj8572ig28fz6idc4r96fx39h4lzmr07yf7kb7gdn6di497"))))
       ("rust-gcc"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "gcc" "0.3.39"))
           (sha256
            (base32
             "1q0idjvmhp6shkb9hqabh51rgfr8dqpi1xfmyzq7q8vgzybll7kp"))))
       ("rust-tempdir"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "tempdir" "0.3.5"))
           (sha256
            (base32
             "1mij45kgzflkja0h8q9avrik76h5a0b60m9hfd6k9yqxbiplm5w7"))))
       ("rust-memchr"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "memchr" "0.1.11"))
           (sha256
            (base32
             "084d85hjfa3xf5kwdms2mhbkh78m1gl2254cp5swcxj3a7xjkdnq"))))
       ("rust-rustc-serialize"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "rustc-serialize" "0.3.21"))
           (sha256
            (base32
             "064qmyr2508qf78dwcpiv25rfjp9h9vd0wrj4mmwgppjg4fgrydz"))))
       ("rust-cmake"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "cmake" "0.1.19"))
           (sha256
            (base32
             "0am8c8ns1h6b1a5x9z2r1m3rszvya5nccl2pzszzjv5aiiaydgcf"))))
       ("rust-matches"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "matches" "0.1.4"))
           (sha256
            (base32
             "1c8190j84hbicy8jwscw5icfam12j6lcxi02lvmadq9260p65mzg"))))
       ("rust-winapi"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "winapi" "0.2.8"))
           (sha256
            (base32
             "0yh816lh6lf56dpsgxy189c2ai1z3j8mw9si6izqb6wsjkbcjz8n"))))
       ("rust-pkg-config"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "pkg-config" "0.3.8"))
           (sha256
            (base32
             "1ypj4nj2z9z27qg06v3g40jyhw685i3l2wi098d21bvyri781vlc"))))
       ("rust-libssh2-sys"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "libssh2-sys" "0.2.5"))
           (sha256
            (base32
             "0d2r36hrh9vc1821r0v4kywv30svpf37d31calwql69fbij3bqci"))))
       ("rust-libz-sys"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "libz-sys" "1.0.13"))
           (sha256
            (base32
             "034pgvxzgsv37iafgs0lmvd1ifm0bg0zm1xcsn9x71nn8lm93vp5"))))
       ("rust-curl-sys"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "curl-sys" "0.3.6"))
           (sha256
            (base32
             "0fi8kjz3f8m8vfazycs3ddm0h6j3x78hw78gwbvybx71129192i1"))))
       ("rust-error-chain"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "error-chain" "0.7.2"))
           (sha256
            (base32
             "03qjh6l2a9fkiyg0428p7q3dcpi47cbmrqf9zmlymkg43v3v731i"))))
       ("rust-metadeps"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "metadeps" "1.1.1"))
           (sha256
            (base32
             "0l818461bslb7nrs7r1amkqv45n53fcp5sabyqipwx0xxbkzz7w2"))))
       ("rust-openssl-sys"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "openssl-sys" "0.9.6"))
           (sha256
            (base32
             "1hzpyf9z8xg1yn5r9g17bl5j20nifd6s2zp10xh90v7m0sd2yj5i"))))
       ("rust-fs2"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "fs2" "0.3.0"))
           (sha256
            (base32
             "0lg57mgcm1r0m8jm4nqpcrl6lmxg8lj854k2h0r7qp46pphh2034"))))
       ("rust-log"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "log" "0.3.6"))
           (sha256
            (base32
             "0m40hgs3cg57dd5kk1mabfk6gk8z6l1cihar8akx4kmzz1xlk0xb"))))
       ("rust-filetime"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "filetime" "0.1.10"))
           (sha256
            (base32
             "08p9scgv30i1141cnp5xi4pqlnkfci455nrpca55df1r867anqsk"))))
       ("rust-tar"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "tar" "0.4.9"))
           (sha256
            (base32
             "1vi3nl8s3jjf5l20ni47gmh1p4bdjfh7q50fbg7izzqrf7i4i40c"))))
       ("rust-glob"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "glob" "0.2.11"))
           (sha256
            (base32
             "1ysvi72slkw784fcsymgj4308c3y03gwjjzqxp80xdjnkbh8vqcb"))))
       ("rust-cfg-if"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "cfg-if" "0.1.0"))
           (sha256
            (base32
             "137qikjcal4h75frzcn6mknygqk8vy5bva7w851aydb5gc6pc7ny"))))
       ("rust-winapi-build"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "winapi-build" "0.1.1"))
           (sha256
            (base32
             "1g4rqsgjky0a7530qajn2bbfcrl2v0zb39idgdws9b1l7gp5wc9d"))))
       ("rust-advapi32-sys"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "advapi32-sys" "0.2.0"))
           (sha256
            (base32
             "16largvlrd1800vvdchml0ngnszjlnpqm01rcz5hm7di1h48hrg0"))))
       ("rust-gdi32-sys"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "gdi32-sys" "0.2.0"))
           (sha256
            (base32
             "0605d4ngjsspghwjv4jicajich1gnl0aik9f880ajjzjixd524h9"))))
       ("rust-ws2_32-sys"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "ws2_32-sys" "0.2.1"))
           (sha256
            (base32
             "0ppscg5qfqaw0gzwv2a4nhn5bn01ff9iwn6ysqnzm4n8s3myz76m"))))
       ("rust-user32-sys"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "user32-sys" "0.2.0"))
           (sha256
            (base32
             "0ivxc7hmsxax9crdhxdd1nqwik4s9lhb2x59lc8b88bv20fp3x2f"))))
       ("rust-unicode-bidi"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "unicode-bidi" "0.2.3"))
           (sha256
            (base32
             "0gqbyf6slkgzr14nf6v8dw8a19l5snh6bpms8bpfvzpxdawwxxy1"))))
       ("rust-net2"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "net2" "0.2.26"))
           (sha256
            (base32
             "1qp3q6xynb481rsp3ig1nmqb6qlxfba3shfrmqij88cppsv9rpsy"))))
       ("rust-utf8-ranges"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "utf8-ranges" "0.1.3"))
           (sha256
            (base32
             "03xf604b2v51ag3jgzw92l97xnb10kw9zv948bhc7ja1ik017jm1"))))
       ("rust-crossbeam"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "crossbeam" "0.2.10"))
           (sha256
            (base32
             "15wga0kvk3iqf3l077957j931brf1pl3p74xibd698jccqas4phc"))))
       ("rust-toml"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "toml" "0.2.1"))
           (sha256
            (base32
             "1d1cz43bxrx4fd6j2p6myckf81f72bp47akg36y3flxjkhj60svk"))))
       ("rust-aho-corasick"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "aho-corasick" "0.5.3"))
           (sha256
            (base32
             "0rnvdmlajikq0i4zdy1p3pv699q6apvsxfc7av7byhppllp2r5ya"))))
       ("rust-psapi-sys"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "psapi-sys" "0.1.0"))
           (sha256
            (base32
             "0y14g8qshsfnmb7nk2gs1rpbrs1wrggajmzp4yby4q6k0wd5vkdb"))))
       ("rust-idna"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "idna" "0.1.0"))
           (sha256
            (base32
             "049c2rmlydrrrgrxdaq2v21adx9vkfh6k9x4xj56ckyf01p26lqh"))))
       ("rust-url"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "url" "1.2.3"))
           (sha256
            (base32
             "1myr1i8djbl2bhvvrm6n3h7bj7sl6kh5dmaaz2f7c6x8hyyzgk28"))))
       ("rust-regex-syntax"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "regex-syntax" "0.3.9"))
           (sha256
            (base32
             "0ms9hgdhhsxw9w920i7gipydvagf100bb56jbs192rz86ln01v7r"))))
       ("rust-kernel32-sys"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "kernel32-sys" "0.2.2"))
           (sha256
            (base32
             "1389av0601a9yz8dvx5zha9vmkd6ik7ax0idpb032d28555n41vm"))))
       ("rust-term"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "term" "0.4.4"))
           (sha256
            (base32
             "0jpr7jb1xidadh0arklwr99r8w1k1dfc4an3ginpsq5nnfigivrx"))))
       ("rust-thread-id"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "thread-id" "2.0.0"))
           (sha256
            (base32
             "00zzs2bx1xw8aqm5plqqgr7bc2zz6zkqrdxq8vpiqb8hc2srslx9"))))
       ("rust-thread_local"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "thread_local" "0.2.7"))
           (sha256
            (base32
             "1mgxikqvhpsic6xk7pan95lvgsky1sdxzw2w5m2l35pgrazxnxl5"))))
       ("rust-miow"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "miow" "0.1.3"))
           (sha256
            (base32
             "16jvfjsp6fr4mbd2sw5hcdmi4dsa0m0aa45gjz78mb1h4mwcdgym"))))
       ("rust-regex"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "regex" "0.1.80"))
           (sha256
            (base32
             "0bs036h3vzc6pj5jj4vc909s9rppq7b808ic99qn0y6gm3karm2g"))))
       ("rust-num_cpus"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "num_cpus" "1.1.0"))
           (sha256
            (base32
             "1bfwcn3yhwa31rinjw9yr7b6gvn6c06hnwnjz06pvm938w4fd448"))))
       ("rust-libgit2-sys"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "libgit2-sys" "0.6.6"))
           (sha256
            (base32
             "074h9q4p60xh6canb0sj4vrc801wqv6p53l9lp0q724bkwzf7967"))))
       ("rust-env_logger"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "env_logger" "0.3.5"))
           (sha256
            (base32
             "0bvcjgkw4s3k1rd7glpflgc8s9a393zjd6jfdgvs8gjvwj0dgaqm"))))
       ("rust-openssl-probe"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "openssl-probe" "0.1.0"))
           (sha256
            (base32
             "0689h6rhzy6dypqr90lsxnf108nsnh952wsx7ggs70s48b44jvbm"))))
       ("rust-lazy_static"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "lazy_static" "0.2.2"))
           (sha256
            (base32
             "16z1h7w702sxnscak38jykxlhxq0b5ip4mndlb46pkaqwzi0xgka"))))
       ("rust-semver-parser"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "semver-parser" "0.6.1"))
           (sha256
            (base32
             "1s8s7a7yg8xhgci17y0xhyyncg229byivhpr0wbs3ljdlyjl73p8"))))
       ("rust-semver"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "semver" "0.5.1"))
           (sha256
            (base32
             "1xbiv8l72rmngb3lgbmk3vd4lalcbzxcnrn085c2b75irl7gcbxf"))))
       ("rust-docopt"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "docopt" "0.6.86"))
           (sha256
            (base32
             "1nf4f4zf5yk0d0l4kl7hkii4na22fhn0l2hgfb46yzv08l2g6zja"))))
       ("rust-miniz-sys"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "miniz-sys" "0.1.7"))
           (sha256
            (base32
             "0m7dlggsxash0k5jkx576p556g9r8vnhyl9244gjxhq1g8rls7wx"))))
       ("rust-curl"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "curl" "0.4.1"))
           (sha256
            (base32
             "1b0y27b6vpqffgzm2kxc1s2i6bgdzxk3wn65g2asbcdxrvys3mcg"))))
       ("rust-flate2"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "flate2" "0.2.14"))
           (sha256
            (base32
             "1fx3zsls5bb1zfx87s5sxkgk853z4nhjsbvq5s6if13kjlg4isry"))))
       ("rust-git2"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "git2" "0.6.3"))
           (sha256
            (base32
             "06b1bw3pwszs8617xn8js6h0j983qjgfwsychw33lshccj3cld05"))))
       ("rust-crates-io"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "crates-io" "0.4.0"))
           (sha256
            (base32
             "0kk6abp1qbpv44hkq1yjp7xgpzjzafs83i1l26ycr0aph1gbwig9"))))
       ("rust-git2-curl"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "git2-curl" "0.7.0"))
           (sha256
            (base32
             "13mzqp4rd81zp78261rlq23iw9aaysdr56484y1yy2xzhk3nnrv8"))))
       ("rust-bufstream"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "bufstream" "0.1.2"))
           (sha256
            (base32
             "0x6h27md1fwabbhbycfldj0wklrpjr520z9p0cpzm60fzzidnj3v"))))
       ("rust-hamcrest"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "hamcrest" "0.1.1"))
           (sha256
            (base32
             "1m49rf7bnkx0qxja56slrjh44zi4z5bjz5x4pblqjw265828y25z"))))
       ("rust-num"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "num" "0.1.36"))
           (sha256
            (base32
             "081i1r3mdz6jasqd7qwraqqfqa3sdpvdvxl1xq0s7ip714xw1rxx"))))
       ("rust-num-traits"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "num-traits" "0.1.36"))
           (sha256
            (base32
             "07688sp4z40p14lh5ywvrpm4zq8kcxzhjks8sg33jsr5da2l4sm1"))))
       ("rust-num-integer"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "num-integer" "0.1.32"))
           (sha256
            (base32
             "14pvaaawl0pgdcgh4dfdd67lz58yxlfl95bry86h28pjnfzxj97v"))))
       ("rust-num-bigint"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "num-bigint" "0.1.35"))
           (sha256
            (base32
             "0jayfkdm33p4zvcahlv46zdfhlzg053mpw32abf2lz0z8xw47cc8"))))
       ("rust-num-rational"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "num-rational" "0.1.35"))
           (sha256
            (base32
             "1bwaygv64qg7i78yqg0v4d0amfhamj598rpy4yxjz9rlhcxn1zsl"))))
       ("rust-num-iter"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "num-iter" "0.1.32"))
           (sha256
            (base32
             "0p74nj5c1mc33h9lx4wpmlmggmn5lnkhxv1225g0aix8d6ciqyi8"))))
       ("rust-num-complex"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "num-complex" "0.1.35"))
           (sha256
            (base32
             "0bzrjfppnnzf9vmkpklhp2dw9sb1lqzydb8r6k83z76i9l2qxizh"))))
       ("rust-shell-escape"
        ,(origin
           (method url-fetch)
           (uri (crate-uri "shell-escape" "0.1.3"))
           (sha256
            (base32
             "1y2fp2brv639icv4a0fdqs1zhlrxq8qbz27ygfa86ifmh5jcjp6x"))))))
    (arguments
     `(#:cargo ,cargo-bootstrap
       #:tests? #f ; FIXME
       #:modules
       ((ice-9 match)
        (srfi srfi-1) ; 'every
        (guix build utils)
        (guix build cargo-build-system))
       #:phases
       (modify-phases %standard-phases
         ;; Avoid cargo complaining about missmatched checksums.
         (delete 'patch-source-shebangs)
         (delete 'patch-generated-file-shebangs)
         (delete 'patch-usr-bin-file)
         (add-after 'unpack 'unpack-submodule-sources
           (lambda* (#:key inputs #:allow-other-keys)
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
                           (rust-name (string-drop name
                                                   rust-length))
                           (rsrc (string-append "vendor/"
                                                rust-name))
                           (unpack-status (unpack src rsrc)))
                      (touch (string-append rsrc "/.cargo-ok"))
                      (generate-checksums rsrc src)
                      unpack-status)))
                 (_ #t)))
               (mkdir "vendor")
               (every install-rust-library inputs)))
         (add-after 'unpack 'set-environment-up
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((gcc (assoc-ref inputs "gcc"))
                    (cc (string-append gcc "/bin/gcc")))
               (mkdir ".cargo")
               (call-with-output-file ".cargo/config"
                 (lambda (p)
                   (format p "
[source.crates-io]
registry = 'https://github.com/rust-lang/crates.io-index'
replace-with = 'vendored-sources'

[source.vendored-sources]
directory = 'vendor'
")))
               (setenv "CMAKE_C_COMPILER" cc)
               (setenv "CC" cc))
             #t))
         (delete 'configure))))
    (home-page "https://github.com/rust-lang/cargo")
    (synopsis "Build tool and package manager for Rust")
    (description "Cargo is a tool that allows Rust projects to declare their
dependencies and ensures a reproducible build.")
    ;; Cargo is dual licensed Apache and MIT. Also contains
    ;; code from openssl which is GPL2 with linking exception.
    (license (list license:asl2.0 license:expat license:gpl2))))
