;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Ivan Petkov <ivanppetkov@gmail.com>
;;; Copyright © 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2019 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages crates-io)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web))

;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;

(define-public rust-adler32
  (package
    (name "rust-adler32")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "adler32" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0p7fxlnks9l7p7rwfqi7aqgnk2bps5zc0rjiw00mdw19nnbjjlky"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs (("rust-rand" ,rust-rand))))
    (home-page "https://github.com/remram44/adler32-rs")
    (synopsis "Implementation of the Adler32 rolling hash algorithm")
    (description
     "This library is an implementation of the Adler32 rolling hash algorithm in
the Rust programming language.")
    (license (list license:bsd-3
                   license:zlib))))

(define-public rust-ansi-term
  (package
    (name "rust-ansi-term")
    (version "0.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ansi_term" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "16wpvrghvd0353584i1idnsgm0r3vchg8fyrm0x8ayv1rgvbljgf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-winapi" ,rust-winapi))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-tests
           ;; https://github.com/ogham/rust-ansi-term/commit/5ff6af6f38790abcb3aafee1239286c10ef69576.patch
           (lambda _
             (substitute* "src/debug.rs"
               (("^ *Blue") "        Blue,")
               (("underline: false") "underline: false,"))
             #t)))))
    (home-page "https://github.com/ogham/rust-ansi-term")
    (synopsis "Library for ANSI terminal colours and styles")
    (description
     "This is a library for controlling colours and formatting, such as red bold
text or blue underlined text, on ANSI terminals.")
    (license license:expat)))

(define-public rust-antidote
  (package
    (name "rust-antidote")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "antidote" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "19g2sw2qa2ibnh4x7j1snk46593jgx6y7rnvva496ynq61af5z9l"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/sfackler/rust-antidote")
    (synopsis "Poison-free Mutex and RwLock types")
    (description
    "These types expose identical APIs to the standard library @code{Mutex} and
@code{RwLock} except that they do not return @code{PoisonError}s.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-atty
  (package
    (name "rust-atty")
    (version "0.2.13")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "atty" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "140sswp1bwqwc4zk80bxkbnfb3g936hgrb77g9g0k1zcld3wc0qq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc)
        ("rust-winapi" ,rust-winapi))
       #:tests? #f)) ; tests fail in our sandbox
    (home-page "https://github.com/softprops/atty")
    (synopsis "A simple interface for querying atty")
    (description
     "This package provides a simple interface for querying atty.")
    (license license:expat)))

(define-public rust-autocfg
  (package
    (name "rust-autocfg")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "autocfg" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0asl6fnc35yk5l2rxwhp25v128jgm45dp754h9z8x51b6n90w4r2"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/cuviper/autocfg")
    (synopsis "Automatic cfg for Rust compiler features")
    (description "Rust library for build scripts to automatically configure
code based on compiler support.  Code snippets are dynamically tested to see
if the @code{rustc} will accept them, rather than hard-coding specific version
support.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-backtrace-sys
  (package
    (name "rust-backtrace-sys")
    (version "0.1.31")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "backtrace-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0as2pk77br4br04daywhivpi1ixxb8y2c7f726kj849dxys31a42"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-compiler-builtins"
         ,rust-compiler-builtins)
        ("rust-libc" ,rust-libc)
        ("rust-rustc-std-workspace-core"
         ,rust-rustc-std-workspace-core))
       #:cargo-development-inputs
       (("rust-cc" ,rust-cc))))
    (home-page "https://github.com/rust-lang/backtrace-rs")
    (synopsis "Bindings to the libbacktrace gcc library")
    (description
     "This package provides bindings to the libbacktrace gcc library.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-base-x
  (package
    (name "rust-base-x")
    (version "0.2.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "base-x" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0hv4y5cdhv6bk0ghk2434clw8v4mmk5cc9lsh6qrpri92zlfmx3n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs
       (("rust-bencher" ,rust-bencher)
        ("rust-json" ,rust-json)
        ("rust-rand" ,rust-rand-0.3))))
    (home-page "https://github.com/OrKoN/base-x-rs")
    (synopsis "Encode/decode any base")
    (description "This library provides for encoding and decoding any base.")
    (license license:expat)))

(define-public rust-bencher
  (package
    (name "rust-bencher")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bencher" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1x8p2xblgqssay8cdykp5pkfc0np0jk5bs5cx4f5av097aav9zbx"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bluss/bencher/")
    (synopsis "Port of the libtest benchmark runner to Rust stable")
    (description "This package provides a port of the libtest (unstable Rust)
benchmark runner to Rust stable releases.  Supports running benchmarks and
filtering based on the name.  Benchmark execution works exactly the same way
and no more (caveat: black_box is still missing!).")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-bitflags
  (package
    (name "rust-bitflags")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bitflags" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1zc1qb1hwsnl2d8rhzicsv9kqd5b2hwbrscrcfw5as4sfr35659x"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bitflags/bitflags")
    (synopsis "Macro to generate structures which behave like bitflags")
    (description "This package provides a macro to generate structures which
behave like a set of bitflags.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-blas-sys
  (package
    (name "rust-blas-sys")
    (version "0.7.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "blas-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0h14zjycwc76v15h8qll9z1xiryvmpvsb5gam97pqpdjrrwv5c8k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-libc" ,rust-libc))))
    (home-page "https://github.com/blas-lapack-rs/blas-sys")
    (synopsis "Bindings to BLAS (Fortran)")
    (description
     "Ths package provides bindings to BLAS (Fortran).")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-cargon
  (package
    (name "rust-cargon")
    (version "0.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cargon" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1cszlab7jk736p0lb50ag4l9nv72m7j41bwrmygl0lr4iz0350w2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-gcc" ,rust-gcc))))
    (home-page "https://github.com/bryant/argon2rs")
    (synopsis "Thin wrapper around the Argon2 C library")
    (description
     "This package provides a thin wrapper around the Argon2 C library.  It is
used in argon2rs' bench suite.")
    (license license:wtfpl2)))

(define-public rust-cblas-sys
  (package
    (name "rust-cblas-sys")
    (version "0.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cblas-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0rgsn3klhhh09d8qf3b87zl4rwk93l2g0qzh9hhb0lff5kcfrzmn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-libc" ,rust-libc))))
    (home-page "https://github.com/blas-lapack-rs/cblas-sys")
    (synopsis "Bindings to CBLAS (C)")
    (description
     "The package provides bindings to CBLAS (C).")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-cc
  (package
    (name "rust-cc")
    (version "1.0.41")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cc" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1zxzd559dbbf1iwdzmkj7czapzccs17kqqmsj9ayijpdix5rrbld"))))
    (build-system cargo-build-system)
    (arguments
     `(;#:cargo-inputs
       ;(("rust-rayon" ,rust-rayon))
       #:cargo-development-inputs
       (("rust-tempdir" ,rust-tempdir))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-optional-deps
           (lambda _
             (substitute* "Cargo.toml.orig"
               ((".*optional.*") "\n")
               ((".*features.*") "")
               ((".*parallel.*") ""))
             (delete-file "Cargo.toml")
             (copy-file "Cargo.toml.orig" "Cargo.toml")
             #t)))
       #:tests? #f)) ; Tests require cc-test from git repo.
    (home-page "https://github.com/alexcrichton/cc-rs")
    (synopsis "Invoke the native C compiler")
    (description
     "This package provides a build-time dependency for Cargo build scripts to
assist in invoking the native C compiler to compile native C code into a static
archive to be linked into Rustcode.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-cfg-if
  (package
    (name "rust-cfg-if")
    (version "0.1.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cfg-if" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0csygklgz3ybpr0670rkip49zh76m43ar3k7xgypkzbzrwycx1ml"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/alexcrichton/cfg-if")
    (synopsis "Define an item depending on parameters")
    (description "This package provides a macro to ergonomically define an item
depending on a large number of #[cfg] parameters.  Structured like an
@code{if-else} chain, the first matching branch is the item that gets emitted.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-clang-sys
  (package
    (name "rust-clang-sys")
    (version "0.28.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clang-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0ls8zcqi5bmmrvrk3b6r1ym4wlivinbv590d2dvg2xn9f44mbpl1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-glob" ,rust-glob)
        ("rust-libc" ,rust-libc)
        ("rust-libloading" ,rust-libloading))
       #:cargo-development-inputs
       (("rust-glob" ,rust-glob))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-environmental-variable
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((clang (assoc-ref inputs "libclang")))
               (setenv "LIBCLANG_PATH"
                       (string-append clang "/lib")))
             #t)))))
    (inputs
     `(("libclang" ,clang)))
    (home-page "https://github.com/KyleMayes/clang-sys")
    (synopsis "Rust bindings for libclang")
    (description
     "This package provides Rust bindings for @code{libclang}.")
    (license license:asl2.0)))

(define-public rust-clang-sys-0.26
  (package
    (inherit rust-clang-sys)
    (name "rust-clang-sys")
    (version "0.26.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clang-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1r50dwy5hj5gq07dn0qf8222d07qv0970ymx0j8n9779yayc3w3f"))))
    (arguments
     `(#:cargo-inputs
       (("rust-glob" ,rust-glob-0.2)
        ("rust-libc" ,rust-libc)
        ("rust-libloading" ,rust-libloading))
       #:cargo-development-inputs
       (("rust-glob" ,rust-glob-0.2))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-environmental-variable
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((clang (assoc-ref inputs "libclang")))
               (setenv "LIBCLANG_PATH"
                       (string-append clang "/lib")))
            #t)))))))

(define-public rust-clicolors-control
  (package
    (name "rust-clicolors-control")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clicolors-control" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1y80cgarxhrd1bz5yjm81r444v6flvy36aaxrrsac0yhfd6gvavk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-atty" ,rust-atty)
        ("rust-lazy-static" ,rust-lazy-static)
        ("rust-libc" ,rust-libc)
        ("rust-winapi" ,rust-winapi))))
    (home-page "https://github.com/mitsuhiko/clicolors-control")
    (synopsis "Common utility library to control CLI colorization")
    (description
     "This package provides a common utility library to control CLI
colorization.")
    (license license:expat)))

(define-public rust-clippy
  (package
    (name "rust-clippy")
    (version "0.0.302")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clippy" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1562x3sq9mgmc8j39gd34wqm7ybrdvpmj7cc1n450gwsawayw4fr"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs
        (("rust-term" ,rust-term))))
    (home-page "https://github.com/rust-lang/rust-clippy")
    (synopsis
      "A bunch of helpful lints to avoid common pitfalls in Rust.")
    (description
      "This package provides a bunch of helpful lints to avoid common pitfalls in Rust.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-cloudabi
  (package
    (name "rust-cloudabi")
    (version "0.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cloudabi" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0kxcg83jlihy0phnd2g8c2c303px3l2p3pkjz357ll6llnd5pz6x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags))))
    (home-page "https://nuxi.nl/cloudabi/")
    (synopsis "Low level interface to CloudABI")
    (description
     "Low level interface to CloudABI.  Contains all syscalls and related types.")
    (license license:bsd-2)))

(define-public rust-cmake
  (package
    (name "rust-cmake")
    (version "0.1.42")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cmake" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0qkwibkvx5xjazvv9v8gvdlpky2jhjxvcz014nrixgzqfyv2byw1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-cc" ,rust-cc))))
    (home-page "https://github.com/alexcrichton/cmake-rs")
    (synopsis "Rust build dependency for running cmake")
    (description
     "This package provides a build dependency for running @code{cmake} to build
a native library.  The CMake executable is assumed to be @code{cmake} unless the
CMAKE environmental variable is set.")
    (license (list license:asl2.0
                   license:expat))))

;; This package requires features which are unavailable
;; on the stable releases of Rust.
(define-public rust-compiler-builtins
  (package
    (name "rust-compiler-builtins")
    (version "0.1.19")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "compiler_builtins" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1fpabpmg8paj4r5a37vmidh1jx1b7a6ilxm4s3xsxczx27ybjcjf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cc" ,rust-cc)
        ("rust-rustc-std-workspace-core"
         ,rust-rustc-std-workspace-core))))
    (home-page "https://github.com/rust-lang-nursery/compiler-builtins")
    (synopsis "Compiler intrinsics used by the Rust compiler")
    (description
     "This package provides compiler intrinsics used by the Rust compiler.  This
package is primarily useful when building the @code{core} crate yourself and you
need compiler-rt intrinsics.")
    (properties `((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-constant-time-eq
  (package
    (name "rust-constant-time-eq")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "constant_time_eq" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "083icpr9xb72rrdxw3p4068dcspn6ai22jy7rhl2a8grfz448nlr"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/cesarb/constant_time_eq")
    (synopsis
     "Compares two equal-sized byte strings in constant time")
    (description
     "This package compares two equal-sized byte strings in constant time.
It is inspired by the Linux kernel's @code{crypto_memneq}.")
    (license license:cc0)))

(define-public rust-core-foundation-sys
  (package
    (name "rust-core-foundation-sys")
    (version "0.6.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "core-foundation-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0fzsw1j9g1x598yhwklg59l15hwzc0pyvs01w9fg2kin4598mjp7"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/servo/core-foundation-rs")
    (synopsis "Bindings to Core Foundation for OS X")
    (description
     "Bindings to Core Foundation for OS X.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-curl-sys
  (package
    (name "rust-curl-sys")
    (version "0.4.20")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "curl-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "02542zmvl3fpdqf7ai4cqnamm4albx9j645dkjx5qr1myq8ax42y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc)
        ("rust-libnghttp2-sys" ,rust-libnghttp2-sys)
        ("rust-libz-sys" ,rust-libz-sys)
        ("rust-openssl-sys" ,rust-openssl-sys)
        ("rust-winapi" ,rust-winapi))
       #:cargo-development-inputs
       (("rust-cc" ,rust-cc)
        ("rust-pkg-config" ,rust-pkg-config)
        ("rust-openssl-src" ,rust-openssl-src)
        ("rust-vcpkg" ,rust-vcpkg))
        #:phases
       (modify-phases %standard-phases
        (add-after 'unpack 'find-openssl
          (lambda* (#:key inputs #:allow-other-keys)
            (let ((openssl (assoc-ref inputs "openssl")))
              (setenv "OPENSSL_DIR" openssl))
            #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("curl" ,curl)
       ("nghttp2" ,nghttp2)
       ("openssl" ,openssl)
       ("zlib" ,zlib)))
    (home-page "https://github.com/alexcrichton/curl-rust")
    (synopsis "Native bindings to the libcurl library")
    (description
     "This package provides native bindings to the @code{libcurl} library.")
    (license license:expat)))

(define-public rust-data-encoding
  (package
    (name "rust-data-encoding")
    (version "2.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "data-encoding" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "15xd6afhsjl08285piwczrafmckpp8i29padj8v12xhahshprx7l"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/ia0/data-encoding")
    (synopsis "Efficient and customizable data-encoding functions")
    (description
     "This library provides encodings for many different common cases, including
hexadecimal, bas32, and base64.")
    (license license:expat)))

(define-public rust-defmac
  (package
    (name "rust-defmac")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "defmac" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "01ff3jdmcc5waffkwllndnx5hsn414r7x1rq4ib73n7awsyzxkxv"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bluss/defmac")
    (synopsis "Macro to define lambda-like macros inline")
    (description "A macro to define lambda-like macros inline.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-defmac-0.1
  (package
    (inherit rust-defmac)
    (name "rust-defmac")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "defmac" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "17giv0n0n1r64z0dahfvkjy3ys517jxyhs8sd9lmgvcljpjyryxa"))))))

(define-public rust-dirs
  (package
    (name "rust-dirs")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "dirs" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "02vigc566z5i6n9wr2x8sch39qp4arn89xhhrh18fhpm3jfc0ygn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc)
        ("rust-winapi" ,rust-winapi))))
    (home-page "https://github.com/soc/dirs-rs")
    (synopsis "Abstractions for standard locations for various platforms")
    (description
     "This package provides a tiny low-level library that provides
platform-specific standard locations of directories for config, cache and other
data on Linux, Windows, macOS and Redox by leveraging the mechanisms defined by
the XDG base/user directory specifications on Linux, the Known Folder API on
Windows, and the Standard Directory guidelines on macOS.")
    (license (list license:expat license:asl2.0))))

(define-public rust-discard
  (package
    (name "rust-discard")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "discard" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1h67ni5bxvg95s91wgicily4ix7lcw7cq0a5gy9njrybaibhyb91"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/Pauan/rust-discard")
    (synopsis "Allow for intentionally leaking memory")
    (description "There are situations where you need to intentionally leak some
memory but not other memory.  This package provides a discard trait which allows
for intentionally leaking memory")
    (license license:expat)))

(define-public rust-doc-comment
  (package
    (name "rust-doc-comment")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "doc-comment" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "15rsqxgarfpb1yim9sbp9yfgj7p2dq6v51c6bq1a62paii9ylgcj"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/GuillaumeGomez/doc-comment")
    (synopsis "Macro to generate doc comments")
    (description "This package provides a way to generate doc comments
from macros.")
    (license license:expat)))

(define-public rust-dtoa
  (package
    (name "rust-dtoa")
    (version "0.4.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "dtoa" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0phbm7i0dpn44gzi07683zxaicjap5064w62pidci4fhhciv8mza"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/dtolnay/dtoa")
    (synopsis "Fast functions for printing floating-point primitives")
    (description "This crate provides fast functions for printing
floating-point primitives to an @code{io::Write}.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-dtoa-0.2
  (package
    (inherit rust-dtoa)
    (name "rust-dtoa")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "dtoa" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0g96cap6si1g6wi62hsdk2fnj3sf5vd4i97zj6163j8hhnsl3n0d"))))))

(define-public rust-fallible-iterator
  (package
    (name "rust-fallible-iterator")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fallible-iterator" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1xq759lsr8gqss7hva42azn3whgrbrs2sd9xpn92c5ickxm1fhs4"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/sfackler/rust-fallible-iterator")
    (synopsis "Fallible iterator traits")
    (description "If the @code{std} or @code{alloc} features are enabled, this
crate provides implementations for @code{Box}, @code{Vec}, @code{BTreeMap}, and
@code{BTreeSet}.  If the @code{std} feature is enabled, this crate additionally
provides implementations for @code{HashMap} and @code{HashSet}.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-filetime
  (package
    (name "rust-filetime")
    (version "0.2.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "filetime" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0sflihq2l77xjrza7yjalnxsc7dxzg25rhzcfbd9vmyfah5kimvb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if)
        ("rust-libc" ,rust-libc)
        ("rust-redox-syscall" ,rust-redox-syscall)
        ("rust-winapi" ,rust-winapi))
       #:cargo-development-inputs
       (("rust-tempdir" ,rust-tempdir))))
    (home-page "https://github.com/alexcrichton/filetime")
    (synopsis "Platform-agnostic accessors of timestamps in File metadata")
    (description
     "This library contains a helper library for inspecting and setting the
various timestamps of files in Rust.  This library takes into account
cross-platform differences in terms of where the timestamps are located, what
they are called, and how to convert them into a platform-independent
representation.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-findshlibs
  (package
    (name "rust-findshlibs")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "findshlibs" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1n2vagn0q5yim32hxkwi1cjgp3yn1dm45p7z8nw6lapywihhs9mi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-lazy-static" ,rust-lazy-static)
        ("rust-libc" ,rust-libc))))
    (home-page "https://github.com/gimli-rs/findshlibs")
    (synopsis "Find the set of shared libraries loaded in the current process")
    (description
     "Find the set of shared libraries loaded in the current process with a
cross platform API.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-fixedbitset
  (package
    (name "rust-fixedbitset")
    (version "0.1.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fixedbitset" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0czam11mi80dbyhf4rd4lz0ihcf7vkfchrdcrn45wbs0h40dxm46"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/petgraph/fixedbitset")
    (synopsis "FixedBitSet is a simple bitset collection")
    (description "FixedBitSet is a simple bitset collection.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-fnv
  (package
    (name "rust-fnv")
    (version "1.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fnv" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1ww56bi1r5b8id3ns9j3qxbi7w5h005rzhiryy0zi9h97raqbb9g"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/servo/rust-fnv")
    (synopsis "implementation of the Fowler-Noll-Vo hash function")
    (description "The @code{fnv} hash function is a custom @code{Hasher}
implementation that is more efficient for smaller hash keys.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-foreign-types-shared
  (package
    (name "rust-foreign-types-shared")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "foreign-types-shared" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0kanxlif1vp0ffh2r9l610jqbkmb3183yqykxq1z5w1vay2rn7y6"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/sfackler/foreign-types")
    (synopsis "An internal crate used by foreign-types")
    (description
     "An internal crate used by foreign-types.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-fs-extra
  (package
    (name "rust-fs-extra")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fs_extra" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0x6675wdhsx277k1k1235jwcv38naf20d8kwrk948ds26hh4lajz"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/webdesus/fs_extra")
    (synopsis "Extra filesystem methods")
    (description "Expanding opportunities standard library @code{std::fs} and
@code{std::io}.  Recursively copy folders with recept information about
process and much more.")
    (license license:expat)))

(define-public rust-fuchsia-cprng
  (package
    (name "rust-fuchsia-cprng")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fuchsia-cprng" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1fnkqrbz7ixxzsb04bsz9p0zzazanma8znfdqjvh39n14vapfvx0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f)) ; tests require zircon
    (home-page "https://fuchsia.googlesource.com/fuchsia/+/master/garnet/public/rust/fuchsia-cprng")
    (synopsis "Fuchsia cryptographically secure pseudorandom number generator")
    (description "Rust crate for the Fuchsia cryptographically secure
pseudorandom number generator")
    (license license:bsd-3)))

(define-public rust-fuchsia-zircon
  (package
    (name "rust-fuchsia-zircon")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fuchsia-zircon" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "10jxc5ks1x06gpd0xg51kcjrxr35nj6qhx2zlc5n7bmskv3675rf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags)
        ("rust-fuchsia-zircon-sys" ,rust-fuchsia-zircon-sys))
       #:tests? #f)) ; tests require zircon
    (home-page "https://fuchsia.googlesource.com/garnet/")
    (synopsis "Rust bindings for the Zircon kernel")
    (description "Rust bindings for the Zircon kernel.")
    (license license:bsd-3)))

(define-public rust-fuchsia-zircon-sys
  (package
    (name "rust-fuchsia-zircon-sys")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fuchsia-zircon-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "19zp2085qsyq2bh1gvcxq1lb8w6v6jj9kbdkhpdjrl95fypakjix"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f)) ; tests require zircon
    (home-page "https://fuchsia.googlesource.com/garnet/")
    (synopsis "Low-level Rust bindings for the Zircon kernel")
    (description "Low-level Rust bindings for the Zircon kernel.")
    (license license:bsd-3)))

(define-public rust-futures
  (package
    (name "rust-futures")
    (version "0.1.28")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0saq8ffjw1pwf1pzhw3kq1z7dfq6wpd8x93dnni6vbkc799kkp25"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-lang-nursery/futures-rs")
    (synopsis "Implementation of zero-cost futures in Rust")
    (description "An implementation of @code{futures} and @code{streams}
featuring zero allocations, composability, and iterator-like interfaces.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-futures-core-preview
  (package
    (name "rust-futures-core-preview")
    (version "0.3.0-alpha.17")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-core-preview" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1xaq8m609k6cz8xydwhwp8xxyxigabcw1w9ngycfy0bnkg7iq52b"))))
    (build-system cargo-build-system)
    (arguments
     '(#:tests? #f)) ; The only tests are doc tests, which fail.
    (home-page "https://rust-lang-nursery.github.io/futures-rs/")
    (synopsis "Core traits and types in for the @code{futures} library.")
    (description "This crate provides the core traits and types in for the
@code{futures} library.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-futures-cpupool
  (package
    (name "rust-futures-cpupool")
    (version "0.1.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-cpupool" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1r32456gpblzfvnkf60545v8acqk7gh5zhyhi1jn669k9gicv45b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-futures" ,rust-futures)
        ("rust-num-cpus" ,rust-num-cpus))))
    (home-page "https://github.com/rust-lang-nursery/futures-rs")
    (synopsis "Implementation of thread pools which hand out futures")
    (description
     "An implementation of thread pools which hand out futures to the results of
the computation on the threads themselves.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-futures-io-preview
  (package
    (name "rust-futures-io-preview")
    (version "0.3.0-alpha.17")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-io-preview" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0fhvwhdb8ywjjbfng0ra1r8yyc9yzpyxg9sv3spb3f7w0lk40bh8"))))
    (build-system cargo-build-system)
    (home-page "https://rust-lang-nursery.github.io/futures-rs/")
    (synopsis "Async read and write traits for the futures library")
    (description "This crate provides the @code{AsyncRead} and
@code{AsyncWrite} traits for the @code{futures-rs} library.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-futures-sink-preview
  (package
    (name "rust-futures-sink-preview")
    (version "0.3.0-alpha.17")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-sink-preview" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1r4d0gy73hdxkh5g1lrhl1kjnwp6mywjgcj70v0z78b921da42a3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-futures-core-preview" ,rust-futures-core-preview))))
    (home-page "https://rust-lang-nursery.github.io/futures-rs/")
    (synopsis  "Asynchronous `Sink` trait for the futures-rs library")
    (description
     "This package provides the asynchronous @code{Sink} trait for the
futures-rs library.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-gcc
  (package
    (inherit rust-cc)
    (name "rust-gcc")
    (version "0.3.55")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gcc" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1hng1sajn4r67hndvhjysswz8niayjwvcj42zphpxzhbz89kjpwg"))))
    (build-system cargo-build-system)
    (arguments
     `(;#:cargo-inputs
       ;(("rust-rayon" ,rust-rayon))
       #:cargo-development-inputs
       (("rust-tempdir" ,rust-tempdir))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-optional-deps
           (lambda _
             (substitute* "Cargo.toml.orig"
               ((".*optional.*") "\n")
               ((".*features.*") "")
               ((".*parallel.*") ""))
             (delete-file "Cargo.toml")
             (copy-file "Cargo.toml.orig" "Cargo.toml")
             #t)))
       #:tests? #f))
    (home-page "https://github.com/alexcrichton/cc-rs")
    (synopsis "Library to compile C/C++ code into a Rust library/application")
    (description
     "This package provides a build-time dependency for Cargo build scripts to
assist in invoking the native C compiler to compile native C code into a static
archive to be linked into Rustcode.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-getopts
  (package
    (name "rust-getopts")
    (version "0.2.17")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "getopts" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "018yhq97zgcrcxwhj3pxh31h83704sgaiijdnpl0r1ir366c005r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs
       (("rust-log" ,rust-log))))
    (home-page "https://github.com/rust-lang-nursery/getopts")
    (synopsis "Rust library for option parsing for CLI utilities")
    (description "This library provides getopts-like option parsing.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-glob
  (package
    (name "rust-glob")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "glob" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0x25wfr7vg3mzxc9x05dcphvd3nwlcmbnxrvwcvrrdwplcrrk4cv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs
       (("rust-tempdir" ,rust-tempdir))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-tests
           ;; This test assumes /root exists but is unreadable by the user.
           (lambda _
             (substitute* "src/lib.rs"
               (("cfg\\(all\\(unix,.*") "cfg(windows)]\n"))
             #t)))))
    (home-page "https://github.com/rust-lang-nursery/glob")
    (synopsis "Match file paths against Unix shell style patterns")
    (description
     "This package provides support for matching file paths against Unix
shell style patterns.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-glob-0.2
  (package
    (inherit rust-glob)
    (name "rust-glob")
    (version "0.2.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "glob" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1ysvi72slkw784fcsymgj4308c3y03gwjjzqxp80xdjnkbh8vqcb"))))
    (arguments
     `(#:cargo-development-inputs
       (("rust-tempdir" ,rust-tempdir))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-tests
           ;; This test assumes /root exists but is unreadable by the user.
           (lambda _
             (substitute* "src/lib.rs"
               (("cfg\\(unix") "cfg(windows"))
             #t)))))))

(define-public rust-heapsize
  (package
    (name "rust-heapsize")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "heapsize" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0q94q9ppqjgrw71swiyia4hgby2cz6dldp7ij57nkvhd6zmfcy8n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-winapi" ,rust-winapi))
       ;; Tests assume rust is built with jemalloc.
       ;; https://github.com/servo/heapsize/issues/74
       #:cargo-test-flags '("--features" "flexible-tests")))
    (home-page "https://github.com/servo/heapsize")
    (synopsis "Measure the total runtime size of an object on the heap")
    (description
     "Infrastructure for measuring the total runtime size of an object on the
heap.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-heapsize-0.3
  (package
    (inherit rust-heapsize)
    (name "rust-heapsize")
    (version "0.3.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "heapsize" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0dmwc37vgsdjzk10443dj4f23439i9gch28jcwzmry3chrwx8v2m"))))
    (arguments
     `(#:cargo-inputs (("rust-kernel32-sys" ,rust-kernel32-sys))
       #:tests? #f)))) ;; No flexible-tests feature flags on this release.

;; This package makes use of removed features
(define-public rust-heapsize-plugin
  (package
    (name "rust-heapsize-plugin")
    (version "0.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "heapsize_plugin" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1i72isf699q9jl167g2kg4xd6h3cd05rc79zaph58aqjy0g0m9y9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs (("rust-heapsize" ,rust-heapsize-0.3))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-Cargo-toml
           (lambda _
             (substitute* "Cargo.toml"
               (("path = \"..\", ") ""))
             #t)))))
    (home-page "https://github.com/servo/heapsize")
    (synopsis "Measure runtime size of an object on the heap")
    (description
     "This package automatically generates infrastructure for measuring the
total runtime size of an object on the heap")
    (properties `((hidden? . #t)))
    (license license:mpl2.0)))

(define-public rust-hex
  (package
    (name "rust-hex")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hex" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0xsdcjiik5j750j67zk42qdnmm4ahirk3gmkmcqgq7qls2jjcl40"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/KokaKiwi/rust-hex")
    (synopsis "Encode and decode data to/from hexadecimals")
    (description "This crate allows for encoding and decoding data into/from
hexadecimal representation.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-hostname
  (package
    (name "rust-hostname")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hostname" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0kprf862qaa7lwdms6aw7f3275h0j2rwhs9nz5784pm8hdmb9ki1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc)
        ("rust-winutil" ,rust-winutil))))
    (home-page "https://github.com/fengcen/hostname")
    (synopsis "Get hostname for Rust")
    (description
     "Get hostname for Rust.")
    (license license:expat)))

(define-public rust-iovec
  (package
    (name "rust-iovec")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "iovec" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "025vi072m22299z3fg73qid188z2iip7k41ba6v5v5yhwwby9rnv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc)
        ("rust-winapi" ,rust-winapi-0.2))))
    (home-page "https://github.com/carllerche/iovec")
    (synopsis "Portable buffer type for scatter/gather I/O operations")
    (description
     "Portable buffer type for scatter/gather I/O operations.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-itoa
  (package
    (name "rust-itoa")
    (version "0.4.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "itoa" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0zvg2d9qv3avhf3d8ggglh6fdyw8kkwqg3r4622ly5yhxnvnc4jh"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/dtolnay/itoa")
    (synopsis "Fast functions for printing integer primitives")
    (description "This crate provides fast functions for printing integer
primitives to an @code{io::Write}.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-itoa-0.1
 (package
   (inherit rust-itoa)
   (name "rust-itoa")
   (version "0.1.1")
   (source
     (origin
       (method url-fetch)
       (uri (crate-uri "itoa" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "18g7p2hrb3dk84z3frfgmszfc9hjb4ps9vp99qlb1kmf9gm8hc5f"))))))

(define-public rust-jemalloc-sys
  (package
    (name "rust-jemalloc-sys")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "jemalloc-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0ify9vlql01qhfxlj7d4p9jvcp90mj2h69nkbq7slccvbhzryfqd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc)
        ("rust-cc" ,rust-cc)
        ("rust-fs-extra" ,rust-fs-extra))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'override-jemalloc
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((jemalloc (assoc-ref inputs "jemalloc")))
               (delete-file-recursively "jemalloc")
               (setenv "JEMALLOC_OVERRIDE"
                       (string-append jemalloc "/lib/libjemalloc_pic.a")))
             #t)))))
    (inputs
     `(("jemalloc" ,jemalloc)))
    (home-page "https://github.com/gnzlbg/jemallocator")
    (synopsis "Rust FFI bindings to jemalloc")
    (description "This package provides Rust FFI bindings to jemalloc.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-json
  (package
    (name "rust-json")
    (version "0.11.14")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "json" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1hj8c6xj5c2aqqszi8naaflmcdbya1i9byyjrq4iybxjb4q91mq1"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/maciejhirsz/json-rust")
    (synopsis "JSON implementation in Rust")
    (description "This crate provides a JSON implementation in Rust, reducing
friction with idiomatic Rust structs to ease interopability.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-kernel32-sys
  (package
    (name "rust-kernel32-sys")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "kernel32-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1389av0601a9yz8dvx5zha9vmkd6ik7ax0idpb032d28555n41vm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-winapi" ,rust-winapi-0.2)
        ("rust-winapi-build" ,rust-winapi-build))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-Cargo-toml
           (lambda _
             (substitute* "Cargo.toml"
               ((", path =.* }") "}\n"))
             #t)))))
    (home-page "https://github.com/retep998/winapi-rs")
    (synopsis "Function definitions for the Windows API library kernel32")
    (description "Contains function definitions for the Windows API library
kernel32.")
    (license license:expat)))

(define-public rust-language-tags
  (package
    (name "rust-language-tags")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "language-tags" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "16hrjdpa827carq5x4b8zhas24d8kg4s16m6nmmn1kb7cr5qh7d9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-heapsize" ,rust-heapsize-0.3))
       #:cargo-development-inputs
       (("rust-heapsize-plugin" ,rust-heapsize-plugin))))
    (home-page "https://github.com/pyfisch/rust-language-tags")
    (synopsis "Language tags for Rust")
    (description
     "Language tags can be used identify human languages, scripts e.g. Latin
script, countries and other regions.  They are commonly used in HTML and HTTP
@code{Content-Language} and @code{Accept-Language} header fields.  This package
currently supports parsing (fully conformant parser), formatting and comparing
language tags.")
    (license license:expat)))

(define-public rust-lazy-static
  (package
    (name "rust-lazy-static")
    (version "1.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lazy_static" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "052ac27w189hrf1j3hz7sga46rp84zl2hqnzyihxv78mgzr2jmxw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-spin" ,rust-spin))))
    (home-page  "https://github.com/rust-lang-nursery/lazy-static.rs")
    (synopsis "Macro for declaring lazily evaluated statics in Rust")
    (description
     "This package provides a macro for declaring lazily evaluated statics in
Rust.  Using this macro, it is possible to have @code{static}s that require code
to be executed at runtime in order to be initialized.  This includes anything
requiring heap allocations, like vectors or hash maps, as well as anything that
requires non-const function calls to be computed.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-libc
  (package
    (name "rust-libc")
    (version "0.2.62")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libc" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1fh69kpjg8hqff36kdczx7sax98gk4qs4ws1dwvjz0rgip0d5z1l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core))))
    (home-page "https://github.com/rust-lang/libc")
    (synopsis "Raw FFI bindings to platform libraries like libc")
    (description
     "libc provides all of the definitions necessary to easily
interoperate with C code (or \"C-like\" code) on each of the platforms
that Rust supports. This includes type definitions (e.g., c_int),
constants (e.g., EINVAL) as well as function headers (e.g., malloc).

This crate exports all underlying platform types, functions, and
constants under the crate root, so all items are accessible as
@samp{libc::foo}.  The types and values of all the exported APIs match
the platform that libc is compiled for.")
    (license (list license:expat
                   license:asl2.0))))

(define-public rust-libgit2-sys
  (package
    (name "rust-libgit2-sys")
    (version "0.8.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libgit2-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0y2mibmx7wy91s2kmb2gfb29mrqlqaxpy5wcwr8s1lwws7b9w5sc")) ))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc)
        ("rust-libssh2-sys" ,rust-libssh2-sys)
        ("rust-libz-sys" ,rust-libz-sys)
        ("rust-openssl-sys" ,rust-openssl-sys))
       #:cargo-development-inputs
       (("rust-cc" ,rust-cc)
        ("rust-pkg-config" ,rust-pkg-config))
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'find-openssl
          (lambda* (#:key inputs #:allow-other-keys)
            (let ((openssl (assoc-ref inputs "openssl")))
              (setenv "OPENSSL_DIR" openssl))
            (delete-file-recursively "libgit2")
            (setenv "LIBGIT2_SYS_USE_PKG_CONFIG" "1")
            (setenv "LIBSSH2_SYS_USE_PKG_CONFIG" "1")
            #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libgit2" ,libgit2)
       ("openssl" ,openssl)
       ("zlib" ,zlib)))
    (home-page "https://github.com/rust-lang/git2-rs")
    (synopsis "Native bindings to the libgit2 library")
    (description
     "This package provides native rust bindings to the @code{libgit2} library.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-libgit2-sys-0.7
  (package
    (inherit rust-libgit2-sys)
    (name "rust-libgit2-sys")
    (version "0.7.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libgit2-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1wcvg2qqra2aviasvqcscl8gb2rnjnd6h998wy5dlmf2bnriqi28"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-curl-sys" ,rust-curl-sys)
        ("rust-libc" ,rust-libc)
        ("rust-libssh2-sys" ,rust-libssh2-sys)
        ("rust-libz-sys" ,rust-libz-sys)
        ("rust-openssl-sys" ,rust-openssl-sys))
       #:cargo-development-inputs
       (("rust-cc" ,rust-cc)
        ("rust-pkg-config" ,rust-pkg-config))))
   (inputs
    `(("curl" ,curl)
      ,@(package-inputs rust-libgit2-sys)))))

(define-public rust-libloading
  (package
    (name "rust-libloading")
    (version "0.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libloading" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0lyply8rcqc8agajzxs7bq6ivba9dnn1i68kgb9z2flnfjh13cgj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cc" ,rust-cc)
        ("rust-winapi" ,rust-winapi))))
    (home-page "https://github.com/nagisa/rust_libloading/")
    (synopsis "Rust library for loading dynamic libraries")
    (description
     "A memory-safer wrapper around system dynamic library loading primitives.
The most important safety guarantee by this library is prevention of
dangling-Symbols that may occur after a Library is unloaded.  Using this library
allows loading dynamic libraries (also known as shared libraries) as well as use
functions and static variables these libraries contain.")
    (license license:isc)))

(define-public rust-libssh2-sys
  (package
    (name "rust-libssh2-sys")
    (version "0.2.12")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libssh2-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1zb6gsw795nq848nk5x2smzpfnn1s15wjlzjnvr8ihlz2l5x2549"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc)
        ("rust-libz-sys" ,rust-libz-sys)
        ("rust-openssl-sys" ,rust-openssl-sys))
       #:cargo-development-inputs
       (("rust-cc" ,rust-cc)
        ("rust-openssl-src" ,rust-openssl-src)
        ("rust-pkg-config" ,rust-pkg-config)
        ("rust-vcpkg" ,rust-vcpkg))
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'find-openssl
          (lambda* (#:key inputs #:allow-other-keys)
            (let ((openssl (assoc-ref inputs "openssl")))
              (setenv "OPENSSL_DIR" openssl))
            (delete-file-recursively "libssh2")
            (setenv "LIBSSH2_SYS_USE_PKG_CONFIG" "1")
            #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libssh2" ,libssh2)
       ("openssl" ,openssl)
       ("zlib" ,zlib)))
    (home-page "https://github.com/alexcrichton/ssh2-rs")
    (synopsis "Native bindings to the libssh2 library")
    (description
     "This package provides native rust bindings to the @code{libssh2} library.")
    (license (list license:asl2.0
                   license:expat))))


(define-public rust-lock-api
  (package
    (name "rust-lock-api")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lock_api" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0b24q9mh258xa52ap636q1sxz0j5vrnp0hwbbh7ddjka3wwz3sv2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-owning-ref" ,rust-owning-ref)
        ("rust-scopeguard" ,rust-scopeguard-0.3))))
    (home-page "https://github.com/Amanieu/parking_lot")
    (synopsis "Wrappers to create fully-featured Mutex and RwLock types")
    (description
     "This package provides wrappers to create fully-featured @code{Mutex} and
@code{RwLock} types.  It is compatible with @code{no_std}.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-log
  (package
    (name "rust-log")
    (version "0.3.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "log" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0nsn28syc49vvjhxcfc8261rd1frhjc0r4bn9v3mqvps3ra7f3w8"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-lang-nursery/log")
    (synopsis "Lightweight logging")
    (description
     "This package provides a lightweight logging facade for Rust")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-lzma-sys
  (package
    (name "rust-lzma-sys")
    (version "0.1.15")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lzma-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "14gyj256yh0wm77jbvmlc39v7lfn0navpfrja4alczarzlc8ir2k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cc" ,rust-cc)
        ("rust-libc" ,rust-libc)
        ("rust-pkg-config" ,rust-pkg-config))
        #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unbundle-xz
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((xz (assoc-ref inputs "xz")))
               (delete-file-recursively "xz-5.2"))
             #t)))))
    (inputs
     `(("pkg-config" ,pkg-config)
       ("xz" ,xz)))
    (home-page "https://github.com/alexcrichton/xz2-rs")
    (synopsis "Bindings to liblzma for lzma and xz stream encoding/decoding")
    (description
     "This package contains the raw bindings to liblzma which contains an
implementation of LZMA and xz stream encoding/decoding.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-maplit
  (package
    (name "rust-maplit")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "maplit" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0hsczmvd6zkqgzqdjp5hfyg7f339n68w83n4pxvnsszrzssbdjq8"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bluss/maplit")
    (synopsis "Collection of Map macros")
    (description "This crate provides a collection of @code{literal} macros for
@code{HashMap}, @code{HashSet}, @code{BTreeMap}, and @code{BTreeSet.}")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-matches
  (package
    (name "rust-matches")
    (version "0.1.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "matches" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "020axl4q7rk9vz90phs7f8jas4imxal9y9kxl4z4v7a6719mrz3z"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/SimonSapin/rust-std-candidates")
    (synopsis "Macro to evaluate whether an expression matches a pattern.")
    (description "This package provides a macro to evaluate, as a boolean,
whether an expression matches a pattern.")
    (license license:expat)))

(define-public rust-md5
  (package
    (name "rust-md5")
    (version "0.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "md5" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "17b2xm4h4cvxsdjsf3kdrzqv2za60kak961xzi5kmw6g6djcssvy"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/stainless-steel/md5")
    (synopsis "MD5 hash function in Rust")
    (description "The package provides the MD5 hash function.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-memmap
  (package
    (name "rust-memmap")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "memmap" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0ns7kkd1h4pijdkwfvw4qlbbmqmlmzwlq3g2676dcl5vwyazv1b5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc)
        ("rust-winapi" ,rust-winapi))
       #:cargo-development-inputs
       (("rust-tempdir" ,rust-tempdir))))
    (home-page "https://github.com/danburkert/memmap-rs")
    (synopsis "Rust library for cross-platform memory mapped IO")
    (description
     "This package provides a cross-platform Rust API for memory-mapped
file IO.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-memmap-0.6
  (package
    (inherit rust-memmap)
    (name "rust-memmap")
    (version "0.6.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "memmap" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1zy6s0ni0lx9rjzq3gq2zz9r8zgjmbp02332g3gsj4fyhv4s5zz2"))))
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc)
        ("rust-winapi" ,rust-winapi))
       #:cargo-development-inputs
       (("rust-tempdir" ,rust-tempdir))))))

(define-public rust-mime
  (package
    (name "rust-mime")
    (version "0.3.13")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "mime" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "09clbyvdkwflp8anwjhqdib0sw8191gphcchdp80nc8ayhhwl9ry"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-unicase" ,rust-unicase))))
    (home-page "https://github.com/hyperium/mime")
    (synopsis "Strongly Typed Mimes")
    (description
     "Support MIME (HTTP Media Types) as strong types in Rust.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-miniz-oxide
  (package
    (name "rust-miniz-oxide")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "miniz_oxide" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "041s41l5w7z8pkp93pdzn8rngxr93q4wxp034pr0cvc7bgway23i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-adler32" ,rust-adler32))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-tests
           (lambda _
             (substitute* "tests/test.rs"
               (("../miniz/miniz.c") "Cargo.toml"))
             #t)))))
    (home-page  "https://github.com/Frommi/miniz_oxide/tree/master/miniz_oxide")
    (synopsis "Pure rust replacement for the miniz DEFLATE/zlib encoder/decoder")
    (description
     "A pure rust replacement for the miniz DEFLATE/zlib encoder/decoder.  Using
@code{flate2} with the @code{rust_backend} feature provides an easy to use
streaming API for miniz_oxide.")
    (license license:expat)))

(define-public rust-miniz-sys
  (package
    (name "rust-miniz-sys")
    (version "0.1.12")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "miniz-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "00l2r4anm8g35x0js2zfdnwfbrih9m43vphdpb77c5ga3kjkm7hy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cc" ,rust-cc)
        ("rust-libc" ,rust-libc))))
    (home-page "https://github.com/alexcrichton/flate2-rs")
    (synopsis "Bindings to the miniz.c library")
    (description
     "This package provides bindings to the @code{miniz.c} library.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-miow
  (package
    (name "rust-miow")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "miow" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "09ljvx6wg30f2xlv7b7hhpkw7k312n3hjgmrbhwzhz9x03ra0sir"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-socket2" ,rust-socket2)
        ("rust-winapi" ,rust-winapi))
       #:cargo-development-inputs
       (("rust-rand" ,rust-rand))))
    (home-page "https://github.com/alexcrichton/miow")
    (synopsis "Rust I/O library for Windows")
    (description
     "This package provides a zero overhead I/O library for Windows, focusing on
IOCP and Async I/O abstractions.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-miow-0.2
  (package
    (inherit rust-miow)
    (name "rust-miow")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "miow" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "06g9b8sqlh5gxakwqq4rrib07afwanfnxgxajrldwcgk3hxjy7wc"))))
    (arguments
     `(#:cargo-inputs
       (("rust-kernel32-sys" ,rust-kernel32-sys)
        ("rust-net2" ,rust-net2)
        ("rust-winapi" ,rust-winapi-0.2)
        ("rust-ws2-32-sys" ,rust-ws2-32-sys))
       #:cargo-development-inputs
       (("rust-rand" ,rust-rand-0.3))))))

(define-public rust-modifier
  (package
    (name "rust-modifier")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "modifier" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0n3fmgli1nsskl0whrfzm1gk0rmwwl6pw1q4nb9sqqmn5h8wkxa1"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/reem/rust-modifier")
    (synopsis
      "Chaining APIs for both self -> Self and &mut self methods.")
    (description
      "Chaining APIs for both self -> Self and &mut self methods.")
    (license license:expat)))

(define-public rust-net2
  (package
    (name "rust-net2")
    (version "0.2.33")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "net2" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "126g3fgfxp06zimc1l9iyxnn9cif1hjsg7sd81nlls5nnyghsma2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if)
        ("rust-libc" ,rust-libc)
        ("rust-winapi" ,rust-winapi))))
    (home-page "https://github.com/rust-lang-nursery/net2-rs")
    (synopsis "Extensions to the standard library's networking types")
    (description
     "This library contains extensions to the standard library's networking
types as proposed in RFC 1158.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-netlib-src
  (package
    (name "rust-netlib-src")
    (version "0.7.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "netlib-src" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "112hwfw1zzdj10h3j213xxqjrq38iygb3nb3ijay65ycmrg819s4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs
       (("rust-cmake" ,rust-cmake)
        ("rust-libc" ,rust-libc))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'use-system-lapack
           (lambda _
             (delete-file-recursively "source")
             (substitute* "Cargo.toml"
               (("default .*")
                "default = [\"system\"]\n"))
             #t)))))
    (inputs
     `(("gfortran:lib" ,gfortran "lib")
       ("lapack" ,lapack)))
    (home-page "https://github.com/blas-lapack-rs/netlib-src")
    (synopsis "Source of BLAS and LAPACK via Netlib")
    (description
     "The package provides a source of BLAS and LAPACK via Netlib.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-libnghttp2-sys
  (package
    (name "rust-libnghttp2-sys")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libnghttp2-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0qr4lyh7righx9n22c7amlcpk906rn1jnb2zd6gdfpa3yi24s982"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc))
       #:cargo-development-inputs
       (("rust-cc" ,rust-cc))))
    (inputs
     `(("nghttp2" ,nghttp2)))
    (home-page "https://github.com/alexcrichton/nghttp2-rs")
    (synopsis "FFI bindings for libnghttp2 (nghttp2)")
    (description
     "This package provides FFI bindings for libnghttp2 (nghttp2).")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-libz-sys
  (package
    (name "rust-libz-sys")
    (version "1.0.25")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libz-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1gjycyl2283525abks98bhxa4r259m617xfm5z52p3p3c8ry9d9f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc)
        ("rust-cc" ,rust-cc)
        ("rust-pkg-config" ,rust-pkg-config)
        ("rust-vcpkg" ,rust-vcpkg))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'delete-vendored-zlib
           (lambda _
             (delete-file-recursively "src/zlib")
             #t)))))
    (inputs
     `(("pkg-config" ,pkg-config)
       ("zlib" ,zlib)))
    (home-page "https://github.com/rust-lang/libz-sys")
    (synopsis "Bindings to the system libz library")
    (description
     "This package provides bindings to the system @code{libz} library (also
known as zlib).")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-nodrop
  (package
    (name "rust-nodrop")
    (version "0.1.13")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "nodrop" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0if9ifn6rvar5jirx4b3qh4sl5kjkmcifycvzhxa9j3crkfng5ig"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-nodrop-union" ,rust-nodrop-union))))
    (home-page "https://github.com/bluss/arrayvec")
    (synopsis "Wrapper type to inhibit drop (destructor)")
    (description "This package provides a wrapper type to inhibit drop
(destructor).  Use @code{std::mem::ManuallyDrop} instead!")
    (license (list license:asl2.0
                   license:expat))))

;; This package requires features which are unavailable
;; on the stable releases of Rust.
(define-public rust-nodrop-union
  (package
    (name "rust-nodrop-union")
    (version "0.1.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "nodrop-union" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0jsnkdn9l8jlmb9h4wssi76sxnyxwnyi00p6y1p2gdq7c1gdw2b7"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bluss/arrayvec")
    (synopsis "Wrapper type to inhibit drop (destructor)")
    (description "This package provides a wrapper type to inhibit drop
(destructor).  Implementation crate for nodrop, the untagged unions
implementation (which is unstable / requires nightly).")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-num-cpus
  (package
    (name "rust-num-cpus")
    (version "1.10.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num_cpus" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0wrj3zvj6h3q26sqj9zxpd59frjb54n7jhjwf307clq31ic47vxw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc))
       #:cargo-development-inputs
       (("rust-doc-comment" ,rust-doc-comment))))
    (home-page "https://github.com/seanmonstar/num_cpus")
    (synopsis "Get the number of CPUs on a machine")
    (description
     "Get the number of CPUs on a machine.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-num-integer
  (package
    (name "rust-num-integer")
    (version "0.1.41")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "num-integer" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "02dwjjpfbi16c71fq689s4sw3ih52cvfzr5z5gs6qpr5z0g58pmq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-autocfg" ,rust-autocfg)
        ("rust-num-traits" ,rust-num-traits))))
    (home-page "https://github.com/rust-num/num-integer")
    (synopsis "Integer traits and functions")
    (description "Integer traits and functions.")
    ;; Dual licensed.
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-num-iter
  (package
    (name "rust-num-iter")
    (version "0.1.39")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num-iter" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0bhk2qbr3261r6zvfc58lz4spfqjhvdripxgz5mks5rd85r55gbn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-num-integer" ,rust-num-integer)
        ("rust-num-traits" ,rust-num-traits))
       #:cargo-development-inputs
       (("rust-autocfg" ,rust-autocfg))))
    (home-page "https://github.com/rust-num/num-iter")
    (synopsis "External iterators for generic mathematics")
    (description
     "This crate provides external iterators for generic mathematics.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-num-traits
  (package
    (name "rust-num-traits")
    (version "0.2.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "num-traits" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0clvrm34rrqc8p6gq5ps5fcgws3kgq5knh7nlqxf2ayarwks9abb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-autocfg" ,rust-autocfg))))
    (home-page "https://github.com/rust-num/num-traits")
    (synopsis "Numeric traits for generic mathematics")
    (description "Numeric traits for generic mathematics.")
    ;; Dual licensed.
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-num-traits-0.1
  (package
    (inherit rust-num-traits)
    (name "rust-num-traits")
    (version "0.1.43")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num-traits" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0c9whknf2dm74a3cqirafy6gj83a76gl56g4v3g19k6lkwz13rcj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-num-traits" ,rust-num-traits))))))

(define-public rust-openssl-probe
  (package
    (name "rust-openssl-probe")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "openssl-probe" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1pijrdifgsdwd45b08c2g0dsmnhz7c3kmagb70839ngrd7d29bvp"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/alexcrichton/openssl-probe")
    (synopsis "Find SSL certificate locations")
    (description
     "This package provides a tool to find SSL certificate locations on the
system for OpenSSL.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-openssl-src
  (package
    (name "rust-openssl-src")
    (version "111.6.0+1.1.1d")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "openssl-src" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "172xh95hp7aygahah1940kg1dnx60c5m80cwj5hgi8x7x0fxmhmr"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs (("rust-cc" ,rust-cc))))
    (home-page "https://github.com/alexcrichton/openssl-src-rs")
    (synopsis "Source of OpenSSL for rust crates")
    (description
     "This package contains the source of OpenSSL and logic to build it.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-openssl-sys
  (package
    (name "rust-openssl-sys")
    (version "0.9.49")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "openssl-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1168vivyqbzaxl48bvv9r1x714c03f5c1za8pv5x8fyj9gjxkypl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-autocfg" ,rust-autocfg)
        ("rust-libc" ,rust-libc))
       #:cargo-development-inputs
       (("rust-autocfg" ,rust-autocfg)
        ("rust-cc" ,rust-cc)
        ("rust-openssl-src" ,rust-openssl-src)
        ("rust-pkg-config" ,rust-pkg-config)
        ("rust-vcpkg" ,rust-vcpkg))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'find-openssl
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((openssl (assoc-ref inputs "openssl")))
               (setenv "OPENSSL_DIR" openssl))
             #t)))))
    (inputs
     `(("openssl" ,openssl)
       ("pkg-config" ,pkg-config)))
    (home-page "https://github.com/sfackler/rust-openssl")
    (synopsis "FFI bindings to OpenSSL")
    (description
     "This package provides FFI bindings to OpenSSL for use in rust crates.")
    (license license:expat)))

(define-public rust-owning-ref
  (package
    (name "rust-owning-ref")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "owning_ref" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "04zgwy77lin8qz398s6g44467pd6kjhbrlqifkia5rkr47mbi929"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-stable-deref-trait" ,rust-stable-deref-trait))))
    (home-page "https://github.com/Kimundi/owning-ref-rs")
    (synopsis "Create references that carry their owner with them")
    (description
     "This package provides a library for creating references that carry their
owner with them.  This can sometimes be useful because Rust borrowing rules
normally prevent moving a type that has been borrowed from.")
    (license license:expat)))

(define-public rust-parity-wasm
  (package
    (name "rust-parity-wasm")
    (version "0.40.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "parity-wasm" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1p84f0k36q05j18jy66n122lyali794cj78hbxgy9wj6si84plqd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs
       (("rust-time" ,rust-time))
       #:tests? #f)) ; Test files are not included in release.
    (home-page "https://github.com/paritytech/parity-wasm")
    (synopsis "Low-level WebAssembly format library")
    (description
     "This package provides a WebAssembly binary format serialization,
deserialization, and interpreter in Rust.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-peeking-take-while
  (package
    (name "rust-peeking-take-while")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "peeking_take_while" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "16bhqr6rdyrp12zv381cxaaqqd0pwysvm1q8h2ygihvypvfprc8r"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/fitzgen/peeking_take_while")
    (synopsis "Provides the peeking_take_while iterator adaptor method")
    (description
      "Like @code{Iterator::take_while}, but calls the predicate on a peeked
value.  This allows you to use @code{Iterator::by_ref} and
@code{Iterator::take_while} together, and still get the first value for which
the @code{take_while} predicate returned false after dropping the @code{by_ref}.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-percent-encoding
  (package
    (name "rust-percent-encoding")
    (version "2.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "percent-encoding" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0bp3zrsk3kr47fbpipyczidbbx4g54lzxdm77ni1i3qws10mdzfl"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/servo/rust-url/")
    (synopsis "Percent encoding and decoding")
    (description "This crate provides percent encoding and decoding.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-percent-encoding-1
  (package
    (inherit rust-percent-encoding)
    (name "rust-percent-encoding")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "percent-encoding" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0cgq08v1fvr6bs5fvy390cz830lq4fak8havdasdacxcw790s09i"))))))

(define-public rust-permutohedron
  (package
    (name "rust-permutohedron")
    (version "0.2.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "permutohedron" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0b1pzh48j86v46wxngch6k1kx9cdw3jr3lwa86gd6jd4bmxzz1xn"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bluss/permutohedron")
    (synopsis "Generate permutations of sequences")
    (description
     "Generate permutations of sequences.  Either lexicographical order
permutations, or a minimal swaps permutation sequence implemented using Heap's
algorithm.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-pico-sys
  (package
    (name "rust-pico-sys")
    (version "0.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pico-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1q5pg0ms6szz6b5h26h4k40zb76zbwwjgyigac4wly9qngdj4yl5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-gcc" ,rust-gcc)
        ("rust-libc" ,rust-libc))))
    (home-page "https://github.com/reem/rust-pico-sys")
    (synopsis "Bindings to the PicoHTTPParser")
    (description
     "This package provides bindings to the PicoHTTPParser.")
    (license license:expat)))

(define-public rust-pin-utils
  (package
    (name "rust-pin-utils")
    (version "0.1.0-alpha.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pin-utils" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "11xmyx00n4m37d546by2rxb8ryxs12v55cc172i3yak1rqccd52q"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-lang-nursery/pin-utils")
    (synopsis "Utilities for pinning")
    (description "This crate provides utilities for pinning values on the stack.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-pkg-config
  (package
    (name "rust-pkg-config")
    (version "0.3.14")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pkg-config" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "135ia995lqzr0gxpk85h0bjxf82kj6hbxdx924sh9jdln6r8wvk7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs
       (("rust-lazy-static" ,rust-lazy-static))))
    (inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://github.com/rust-lang/pkg-config-rs")
    (synopsis "Library to run the pkg-config system tool")
    (description
     "A library to run the pkg-config system tool at build time in order to be
used in Cargo build scripts.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-plain
  (package
    (name "rust-plain")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "plain" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "19n1xbxb4wa7w891268bzf6cbwq4qvdb86bik1z129qb0xnnnndl"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/randomites/plain")
    (synopsis "Rust library that allows reinterpreting data safely")
    (description "This package provides a small Rust library that allows users
 to reinterpret data of certain types safely.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-plugin
  (package
    (name "rust-plugin")
    (version "0.2.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "plugin" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1q7nghkpvxxr168y2jnzh3w7qc9vfrby9n7ygy3xpj0bj71hsshs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-typemap" ,rust-typemap))
       #:cargo-development-inputs
       (("rust-void" ,rust-void))))
    (home-page "https://github.com/reem/rust-plugin")
    (synopsis "Lazily evaluated, order-independent plugins for extensible types")
    (description
     "Lazily evaluated, order-independent plugins for extensible types.")
    (license license:expat)))

(define-public rust-pocket-resources
  (package
    (name "rust-pocket-resources")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pocket-resources" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1n2i5vmi8fdbw89wm5nz1ws1z9f1qax911p6ksg4scmdg23z6df1"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/tomaka/pocket-resources")
    (synopsis "Include resources in your applications")
    (description "This crate allows you to include resources in your
applications.")
    (license license:expat)))

(define-public rust-ppv-lite86
  (package
    (name "rust-ppv-lite86")
    (version "0.2.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ppv-lite86" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "06snnv338w341nicfqba2jgln5dsla72ndkgrw7h1dfdb3vgkjz3"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/cryptocorrosion/cryptocorrosion")
    (synopsis "Implementation of the crypto-simd API for x86")
    (description "This crate provides an implementation of the crypto-simd API
for x86.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-proc-macro2
  (package
    (name "rust-proc-macro2")
    (version "0.4.30")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "proc-macro2" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "0nd71fl24sys066jrha6j7i34nfkjv44yzw8yww9742wmc8j0gfg"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs (("rust-unicode-xid" ,rust-unicode-xid-0.1))
        #:cargo-development-inputs (("rust-quote" ,rust-quote))))
    (home-page "https://github.com/alexcrichton/proc-macro2")
    (synopsis "Stable implementation of the upcoming new `proc_macro` API")
    (description "This package provides a stable implementation of the upcoming new
`proc_macro` API.  Comes with an option, off by default, to also reimplement itself
in terms of the upstream unstable API.")
    ;; Dual licensed.
    (license (list license:asl2.0 license:expat))))

(define-public rust-quick-error
  (package
    (name "rust-quick-error")
    (version "1.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quick-error" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1w6kgwwv7p7zr0yyg5rb315lkk24bimywklwx7fsvsbwi10bjx4j"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/tailhook/quick-error")
    (synopsis "Macro which makes error types pleasant to write")
    (description "This crate provides a macro which makes error types pleasant
to write.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-quote
  (package
    (name "rust-quote")
    (version "0.6.12")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quote" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
          (base32
            "1nw0klza45hf127kfyrpxsxd5jw2l6h21qxalil3hkr7bnf7kx7s"))))
    (build-system cargo-build-system)
    (arguments
      `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2))))
    (home-page "https://github.com/dtolnay/quote")
    (synopsis "Quasi-quoting macro quote!(...)")
    (description "Quasi-quoting macro quote!(...)")
    ;; Dual licensed.
    (license (list license:asl2.0 license:expat))))

(define-public rust-rand
  (package
    (name "rust-rand")
    (version "0.4.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1gfgnqfj2pyh27dcb720jpawskllwnbvxh816ddyykv269xz8ml3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-fuchsia-zircon" ,rust-fuchsia-zircon)
        ("rust-libc" ,rust-libc)
        ("rust-winapi" ,rust-winapi))))
    (home-page "https://crates.io/crates/rand")
    (synopsis "Random number generators and other randomness functionality")
    (description
     "Rand provides utilities to generate random numbers, to convert them to
useful types and distributions, and some randomness-related algorithms.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rand-0.3
  (package
    (inherit rust-rand)
    (name "rust-rand")
    (version "0.3.23")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0v679h38pjjqj5h4md7v2slsvj6686qgcn7p9fbw3h43iwnk1b34"))))
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc)
        ("rust-rand" ,rust-rand))))))

(define-public rust-rawpointer
  (package
    (name "rust-rawpointer")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rawpointer" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "06ghpm9y7gacks78s3maakha07kbnwrxif5q37r2l7z1sali3b7b"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bluss/rawpointer/")
    (synopsis "Extra methods for raw pointers")
    (description "Extra methods for raw pointers.  For example
@code{.post_inc()} and @code{.pre_dec()} (c.f. @code{ptr++} and @code{--ptr})
and @code{ptrdistance}.")
    (license (list license:asl2.0
                   license:expat))))

;; This package requires features which are unavailable
;; on the stable releases of Rust.
(define-public rust-redox-syscall ; guix upstreamable
  (package
    (name "rust-redox-syscall")
    (version "0.1.56")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "redox_syscall" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "110y7dyfm2vci4x5vk7gr0q551dvp31npl99fnsx2fb17wzwcf94"))))
    (build-system cargo-build-system)
    (home-page "https://gitlab.redox-os.org/redox-os/syscall")
    (synopsis "Rust library to access raw Redox system calls")
    (description "This package provides a Rust library to access raw Redox
system calls.")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-regex-syntax
  (package
    (name "rust-regex-syntax")
    (version "0.6.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "regex-syntax" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0p47lf38yj2g2fnmvnraccqlxwk35zr76hlnqi8yva932nzqam6d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ucd-util" ,rust-ucd-util))))
    (home-page "https://github.com/rust-lang/regex")
    (synopsis "Regular expression parser")
    (description
     "This package provides a regular expression parser.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-remove-dir-all
  (package
    (name "rust-remove-dir-all")
    (version "0.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "remove_dir_all" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0bkrlyg26mgizpiy1yb2hhpgscxcag8r5fnckqsvk25608vzm0sa"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-winapi" ,rust-winapi))
       #:cargo-development-inputs
       (("rust-doc-comment" ,rust-doc-comment))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-source
           ;; The test phase expects there to be a README.md in the root directory.
           (lambda _
             (invoke "touch" "README.md"))))))
    (home-page "https://github.com/XAMPPRocky/remove_dir_all")
    (synopsis "Implementation of remove_dir_all for Windows")
    (description
     "This package provides a safe, reliable implementation of
@code{remove_dir_all} for Windows")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-resolv-conf
  (package
    (name "rust-resolv-conf")
    (version "0.6.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "resolv-conf" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1jvdsmksdf6yiipm3aqahyv8n1cjd7wqc8sa0p0gzsax3fmb8qxj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-quick-error" ,rust-quick-error)
        ("rust-hostname" ,rust-hostname))
       #:cargo-test-flags '("--release" "--lib" "--examples"))) ; doc tests fail
    (home-page "https://github.com/tailhook/resolv-conf")
    (synopsis "/etc/resolv.conf parser")
    (description
     "An /etc/resolv.conf parser crate for Rust.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rustc-demangle
  (package
    (name "rust-rustc-demangle")
    (version "0.1.16")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustc-demangle" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "10qp42sl1wrdbgbbh8rnay2grm976z7hqgz32c4y09l1c071qsac"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-compiler-builtins"
         ,rust-compiler-builtins)
        ("rust-rustc-std-workspace-core"
         ,rust-rustc-std-workspace-core))))
    (home-page "https://github.com/alexcrichton/rustc-demangle")
    (synopsis "Rust compiler symbol demangling")
    (description
     "This package demanges the symbols from the Rust compiler.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rustc-hash
  (package
    (name "rust-rustc-hash")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustc-hash" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "114bf72466bl63i5hh8fgqfnhihs0w1m9c9jz505095agfixnvg0"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-lang-nursery/rustc-hash")
    (synopsis "Speedy, non-cryptographic hash used in rustc")
    (description
     "This package provides a speedy, non-cryptographic hash used in rustc.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rustc-serialize
  (package
    (name "rust-rustc-serialize")
    (version "0.3.24")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustc-serialize" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1nkg3vasg7nk80ffkazizgiyv3hb1l9g3d8h17cajbkx538jiwfw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs
       (("rust-rand" ,rust-rand-0.3))))
    (home-page "https://github.com/rust-lang-deprecated/rustc-serialize")
    (synopsis "Generic serialization/deserialization support")
    (description
     "This package provides generic serialization/deserialization support
corresponding to the @code{derive(RustcEncodable, RustcDecodable)} mode in the
compiler.  Also includes support for hex, base64, and json encoding and
decoding.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rustc-std-workspace-core
  (package
    (name "rust-rustc-std-workspace-core")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustc-std-workspace-core" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1309xhwyai9xpz128xrfjqkmnkvgjwddznmj7brbd8i8f58zamhr"))))
    (build-system cargo-build-system)
    (home-page "https://crates.io/crates/rustc-std-workspace-core")
    (synopsis "Explicitly empty crate for rust-lang/rust integration")
    (description "This crate provides an explicitly empty crate for
rust-lang/rust integration.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-safemem
  (package
    (name "rust-safemem")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "safemem" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1l1ljkm4lpyryrv2ndaxi1f7z1f3v9bwy1rzl9f9mbhx04iq9c6j"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/abonander/safemem")
    (synopsis "Safe wrappers for memory-accessing functions")
    (description
     "Safe wrappers for memory-accessing functions, like @code{std::ptr::copy()}.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-same-file
  (package
    (name "rust-same-file")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "same-file" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "08a4zy10pjindf2rah320s6shgswk13mqw7s61m8i1y1xpf8spjq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
      (("rust-winapi-util" ,rust-winapi-util))))
    (home-page "https://github.com/BurntSushi/same-file")
    (synopsis "Determine whether two file paths point to the same file")
    (description
     "This package provides a simple crate for determining whether two file
paths point to the same file.")
    (license (list license:unlicense
                   license:expat))))

(define-public rust-schannel
  (package
    (name "rust-schannel")
    (version "0.1.15")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "schannel" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0f9k4pm8yc3z0n1n8hazvnrvg52f0sfxjc91bhf3r76rb3rapxpj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-lazy-static" ,rust-lazy-static)
        ("rust-winapi" ,rust-winapi))))
    (home-page "https://github.com/steffengy/schannel-rs")
    (synopsis "Rust bindings to the Windows SChannel APIs")
    (description
     "Rust bindings to the Windows SChannel APIs providing TLS client and
server functionality.")
    (license license:expat)))

(define-public rust-scoped-threadpool
  (package
    (name "rust-scoped-threadpool")
    (version "0.1.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "scoped_threadpool" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1a26d3lk40s9mrf4imhbik7caahmw2jryhhb6vqv6fplbbgzal8x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs
       (("rust-lazy-static" ,rust-lazy-static))))
    (home-page "https://github.com/Kimundi/scoped-threadpool-rs")
    (synopsis "library for scoped and cached threadpools")
    (description
     "This crate provides a stable, safe and scoped threadpool.  It can be used
to execute a number of short-lived jobs in parallel without the need to respawn
the underlying threads.  Jobs are runnable by borrowing the pool for a given
scope, during which an arbitrary number of them can be executed. These jobs can
access data of any lifetime outside of the pools scope, which allows working on
non-'static references in parallel.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-scoped-tls
  (package
    (name "rust-scoped-tls")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "scoped-tls" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1hj8lifzvivdb1z02lfnzkshpvk85nkgzxsy2hc0zky9wf894spa"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/alexcrichton/scoped-tls")
    (synopsis "Rust library providing the old standard library's scoped_thread_local")
    (description "This crate provides a library implementation of the standard
library's old @code{scoped_thread_local!} macro for providing scoped access to
@dfn{thread local storage} (TLS) so any type can be stored into TLS.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-scoped-tls-0.1
  (package
    (inherit rust-scoped-tls)
    (name "rust-scoped-tls")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "scoped-tls" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0a2bn9d2mb07c6l16sadijy4p540g498zddfxyiq4rsqpwrglbrk"))))))

(define-public rust-scopeguard
  (package
    (name "rust-scopeguard")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "scopeguard" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "03aay84r1f6w87ckbpj6cc4rnsxkxcfs13n5ynxjia0qkgjiabml"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bluss/scopeguard")
    (synopsis "Scope guard which will run a closure even out of scope")
    (description "This package provides a RAII scope guard that will run a
given closure when it goes out of scope, even if the code between panics
(assuming unwinding panic).  Defines the macros @code{defer!},
@code{defer_on_unwind!}, @code{defer_on_success!} as shorthands for guards
with one of the implemented strategies.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-scopeguard-0.3
  (package
    (inherit rust-scopeguard)
    (name "rust-scopeguard")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "scopeguard" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "09sy9wbqp409pkwmqni40qmwa99ldqpl48pp95m1xw8sc19qy9cl"))))))

(define-public rust-security-framework-sys
  (package
    (name "rust-security-framework-sys")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "security-framework-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0mlsakq9kmqyc0fg2hcbgm6rjk55mb0rhjw2wid3hqdzkjcghdln"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-core-foundation-sys"
         ,rust-core-foundation-sys))))
    (home-page "https://lib.rs/crates/security-framework-sys")
    (synopsis "Apple `Security.framework` low-level FFI bindings")
    (description
     "Apple `Security.framework` low-level FFI bindings.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-semver-parser
  (package
    (name "rust-semver-parser")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "semver-parser" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1ahqhvgpzhcsd28id7xnrjv4419i9yyalhm7d7zi430qx0hi2vml"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/steveklabnik/semver-parser")
    (synopsis "Parsing of the semver spec")
    (description "This package provides for parsing of the semver spec.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-semver-parser-0.7
  (package
    (inherit rust-semver-parser)
    (name "rust-semver-parser")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "semver-parser" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "18vhypw6zgccnrlm5ps1pwa0khz7ry927iznpr88b87cagr1v2iq"))))))

(define-public rust-shlex
  (package
    (name "rust-shlex")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "shlex" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1lmv6san7g8dv6jdfp14m7bdczq9ss7j7bgsfqyqjc3jnjfippvz"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/comex/rust-shlex")
    (synopsis "Split a string into shell words, like Python's shlex")
    (description "This crate provides a method to split a string into shell
words, like Python's shlex.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-slab
  (package
    (name "rust-slab")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "slab" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1y59xsa27jk84sxzswjk60xcjf8b4fm5960jwpznrrcmasyva4f1"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/carllerche/slab")
    (synopsis "Pre-allocated storage for a uniform data type")
    (description "This create provides a pre-allocated storage for a uniform
data type.")
    (license license:expat)))

(define-public rust-socket2
  (package
    (name "rust-socket2")
    (version "0.3.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "socket2" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "11bdcz04i106g4q7swkll0qxrb4287srqd2k3aq2q6i22zjlvdz8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if)
        ("rust-libc" ,rust-libc)
        ("rust-redox-syscall" ,rust-redox-syscall)
        ("rust-winapi" ,rust-winapi))
       #:cargo-development-inputs
       (("rust-tempdir" ,rust-tempdir))
       #:tests? #f)) ; Tests expect network connectivity.
    (home-page "https://github.com/alexcrichton/socket2-rs")
    (synopsis "Networking sockets in Rust")
    (description
     "This package provides utilities for handling networking sockets with a
maximal amount of configuration possible intended.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-sourcefile
  (package
    (name "rust-sourcefile")
    (version "0.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sourcefile" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1lwa6973zs4bgj29my7agfjgk4nw9hp6j7dfnr13nid85fw7rxsb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs
       (("rust-tempfile" ,rust-tempfile))))
    (home-page "https://github.com/derekdreery/sourcefile-rs")
    (synopsis "Concatenate source from multiple files")
    (description
     "A library for concatenating source from multiple files, whilst keeping
track of where each new file and line starts.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-spin
  (package
    (name "rust-spin")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "spin" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0m9clchsj0rf13bggsgvbv9haiy0f6rhvnvkpvkk8720a5pkydj4"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/mvdnes/spin-rs")
    (synopsis "Synchronization primitives based on spinning")
    (description "This crate provides synchronization primitives based on
spinning.  They may contain data, are usable without @code{std},and static
initializers are available.")
    (license license:expat)))

(define-public rust-stable-deref-trait
  (package
    (name "rust-stable-deref-trait")
    (version "1.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "stable_deref_trait" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1j2lkgakksmz4vc5hfawcch2ipiskrhjs1sih0f3br7s7rys58fv"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/storyyeller/stable_deref_trait0")
    (synopsis "Defines an unsafe marker trait, StableDeref")
    (description
      "This crate defines an unsafe marker trait, StableDeref, for container
types which deref to a fixed address which is valid even when the containing
type is moved.  For example, Box, Vec, Rc, Arc and String implement this trait.
Additionally, it defines CloneStableDeref for types like Rc where clones deref
to the same address.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-stacker
  (package
    (name "rust-stacker")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "stacker" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0js0axz5nla1mkr2dm2vrv9rj964ng1lrv4l43sqlnfgawplhygv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if)
        ("rust-libc" ,rust-libc)
        ("rust-winapi" ,rust-winapi)
        ("rust-cc" ,rust-cc))))
    (home-page "https://github.com/rust-lang/stacker")
    (synopsis "Manual segmented stacks for Rust")
    (description
     "This package provides a stack growth library useful when implementing
deeply recursive algorithms that may accidentally blow the stack.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-static-assertions
  (package
    (name "rust-static-assertions")
    (version "0.3.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "static-assertions" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1lw33i89888yb3x29c6dv4mrkg3534n0rlg3r7qzh4p58xmv6gkz"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/nvzqz/static-assertions-rs")
    (synopsis "Compile-time assertions for rust")
    (description
     "This package provides compile-time assertions to ensure that invariants
are met.")
    (license (list license:expat license:asl2.0))))

(define-public rust-stdweb-internal-runtime
  (package
    (name "rust-stdweb-internal-runtime")
    (version "0.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "stdweb-internal-runtime" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1nhpyra7glbwcpakhpj5a3d7h7kx1ynif473nzshmk226m91f8ym"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/koute/stdweb")
    (synopsis "Internal runtime for the @code{stdweb} crate")
    (description "This crate provides internal runtime for the @code{stdweb}
crate.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-stdweb-internal-test-macro
  (package
    (name "rust-stdweb-internal-test-macro")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "stdweb-internal-test-macro" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "12rrm7p77xnm3xacgn3rgniiyyjb4gq7902wpbljsvbx045z69l2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2)
        ("rust-quote" ,rust-quote))))
    (home-page "https://github.com/koute/stdweb")
    (synopsis "Internal crate of the `stdweb` crate")
    (description
     "Internal crate of the @code{stdweb} crate.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-streaming-stats
  (package
    (name "rust-streaming-stats")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "streaming-stats" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0l7xz4g6709s80zqpvlhrg0qhgz64r94cwhmfsg8xhabgznbp2px"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-num-traits" ,rust-num-traits))))
    (home-page "https://github.com/BurntSushi/rust-stats")
    (synopsis "Compute basic statistics on streams")
    (description
     "Experimental crate for computing basic statistics on streams.")
    (license (list license:unlicense
                   license:expat))))

(define-public rust-strsim
  (package
    (name "rust-strsim")
    (version "0.9.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "strsim" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1xphwhf86yxxmcpvm4mikj8ls41f6nf7gqyjm98b74mfk81h6b03"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/dguo/strsim-rs")
    (synopsis "Rust implementations of string similarity metrics")
    (description "This crate includes implementations of string similarity
metrics.  It includes Hamming, Levenshtein, OSA, Damerau-Levenshtein, Jaro,
and Jaro-Winkler.")
    (license license:expat)))

(define-public rust-strsim-0.8
  (package
    (inherit rust-strsim)
    (name "rust-strsim")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "strsim" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0sjsm7hrvjdifz661pjxq5w4hf190hx53fra8dfvamacvff139cf"))))))

(define-public rust-synstructure-test-traits
  (package
    (name "rust-synstructure-test-traits")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "synstructure_test_traits" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1b3fs2b9kc1gy9dilaxqjbdl4z0mlrbbxjzkprdx953rif1c3q66"))))
    (build-system cargo-build-system)
    (home-page "https://crates.io/crates/synstructure_test_traits")
    (synopsis "Helper test traits for synstructure doctests")
    (description
     "This package provides helper test traits for synstructure doctests.")
    (license license:expat)))

(define-public rust-tar
  (package
    (name "rust-tar")
    (version "0.4.26")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tar" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1lr6v3cpkfxd2lk5ll2jd8wr1xdskwj35smnh5sfb8xvzzxnn6dk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-filetime" ,rust-filetime)
        ("rust-libc" ,rust-libc)
        ("rust-redox-syscall" ,rust-redox-syscall)
        ("rust-xattr" ,rust-xattr))
       #:cargo-development-inputs
       (("rust-tempdir" ,rust-tempdir))
       #:tests? #f)) ; Test archives are not distributed in the crate tarball.
    (home-page "https://github.com/alexcrichton/tar-rs")
    (synopsis "Tar file reading/writing for Rust")
    (description
     "This package provides a Rust implementation of a TAR file reader and
writer.  This library does not currently handle compression, but it is abstract
over all I/O readers and writers.  Additionally, great lengths are taken to
ensure that the entire contents are never required to be entirely resident in
memory all at once.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-tempdir
  (package
    (name "rust-tempdir")
    (version "0.3.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tempdir" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1n5n86zxpgd85y0mswrp5cfdisizq2rv3la906g6ipyc03xvbwhm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-rand" ,rust-rand)
        ("rust-remove-dir-all" ,rust-remove-dir-all))))
    (home-page "https://github.com/rust-lang-deprecated/tempdir")
    (synopsis "Temporary directory management for Rust")
    (description
     "This package provides a library for managing a temporary directory and
deleting all contents when it's dropped.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-tempfile
  (package
    (name "rust-tempfile")
    (version "3.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tempfile" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0ny1cdbcsrmwjpy4k9366xm6p0jqkrmrvr0cln2djxspp1inyxs7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc)
        ("rust-rand" ,rust-rand)
        ("rust-redox-syscall" ,rust-redox-syscall)
        ("rust-remove-dir-all" ,rust-remove-dir-all)
        ("rust-winapi" ,rust-winapi))))
    (home-page "http://stebalien.com/projects/tempfile-rs")
    (synopsis "Library for managing temporary files and directories")
    (description
     "This package provides a library for managing temporary files and
directories.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-term
  (package
    (name "rust-term")
    (version "0.4.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "term" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1wbh8ngqkqr3f6wz902yplf60bd5yapnckvrkgmzp5nffi7n8qzs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-kernel32-sys" ,rust-kernel32-sys)
        ("rust-winapi" ,rust-winapi))))
    (home-page "https://github.com/Stebalien/term")
    (synopsis "Terminal formatting library")
    (description
     "This package provides a terminal formatting library in rust.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-termcolor
  (package
    (name "rust-termcolor")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "termcolor" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0vjfsn1a8zvqhnrbygrz1id6yckwv1dncw3w4zj65qdx0f00kmln"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-wincolor" ,rust-wincolor))))
    (home-page "https://github.com/BurntSushi/termcolor")
    (synopsis "Library for writing colored text to a terminal")
    (description "This package provides a simple cross platform library for
writing colored text to a terminal.")
    (license (list license:unlicense
                   license:expat))))

(define-public rust-termios
  (package
    (name "rust-termios")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "termios" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "09any1p4jp4bphvb5ikagnvwjc3xn2djchy96nkpa782xb2j1dkj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-libc" ,rust-libc))))
    (home-page  "https://github.com/dcuddeback/termios-rs")
    (synopsis "Safe bindings for the termios library")
    (description
     "The termios crate provides safe bindings for the Rust programming language
to the terminal I/O interface implemented by Unix operating systems.  The safe
bindings are a small wrapper around the raw C functions, which converts integer
return values to @code{std::io::Result} to indicate success or failure.")
    (license license:expat)))

(define-public rust-thread-id
  (package
    (name "rust-thread-id")
    (version "3.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "thread-id" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1h90v19fjz3x9b25ywh68z5yf2zsmm6h5zb4rl302ckbsp4z9yy7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc)
        ("rust-redox-syscall" ,rust-redox-syscall)
        ("rust-winapi" ,rust-winapi))))
    (home-page "https://github.com/ruuda/thread-id")
    (synopsis "Get a unique ID for the current thread in Rust")
    (description
     "For diagnostics and debugging it can often be useful to get an ID that is
different for every thread.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-thread-local
  (package
    (name "rust-thread-local")
    (version "0.3.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "thread_local" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "06rzik99p8c5js8238yhc8rk6np543ylb1dy9nrw5v80j0r3xdf6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
      (("rust-lazy-static" ,rust-lazy-static))))
    (home-page "https://github.com/Amanieu/thread_local-rs")
    (synopsis "Per-object thread-local storage")
    (description "Per-object thread-local storage")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-threadpool
  (package
    (name "rust-threadpool")
    (version "1.7.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "threadpool" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0rd89n1q7vy47w4c32cnynibffv9kj3jy3dwr0536n9lbw5ckw72"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-num-cpus" ,rust-num-cpus))))
    (home-page "https://github.com/rust-threadpool/rust-threadpool")
    (synopsis "Thread pool for running jobs on a fixed set of worker threads")
    (description
     "This package provides a thread pool for running a number of jobs on a
fixed set of worker threads.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-time
  (package
    (name "rust-time")
    (version "0.1.39")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "time" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "161hqx0gw722ikydanpahky447vaxqncwmkj66rny282vzqpalx1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc)
        ("rust-redox-syscall" ,rust-redox-syscall)
        ("rust-rustc-serialize" ,rust-rustc-serialize)
        ("rust-winapi" ,rust-winapi))
       #:cargo-development-inputs
       (("rust-log" ,rust-log)
        ("rust-winapi" ,rust-winapi))
       #:tests? #f)) ; Tests have difficulty with the timezones.
    (home-page "https://github.com/rust-lang-deprecated/time")
    (synopsis "Simple time handling in Rust")
    (description
     "This package provides utilities for working with time-related functions
in Rust.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-tokio-mock-task
  (package
    (name "rust-tokio-mock-task")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-mock-task" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1y7q83qfk9ljjfvs82b453pmz9x1v3d6kr4x55j8mal01s6790dw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-futures" ,rust-futures))))
    (home-page  "https://github.com/carllerche/tokio-mock-task")
    (synopsis "Mock a Tokio task")
    (description "Mock a Tokio task")
    (license license:expat)))

(define-public rust-tracing-core
  (package
    (name "rust-tracing-core")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tracing-core" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "01fa73wzw2m5ybi3kkd52dgrw97mgc3i6inmhwys46ab28giwnxi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-lazy-static" ,rust-lazy-static))))
    (home-page "https://tokio.rs")
    (synopsis "Core primitives for application-level tracing")
    (description
     "Core primitives for application-level tracing.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-traitobject
  (package
    (name "rust-traitobject")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "traitobject" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0yb0n8822mr59j200fyr2fxgzzgqljyxflx9y8bdy3rlaqngilgg"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/reem/rust-traitobject")
    (synopsis "Unsafe helpers for dealing with raw trait objects")
    (description "Unsafe helpers for dealing with raw trait objects.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-try-from
  (package
    (name "rust-try-from")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "try_from" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "12wdd4ja7047sd3rx70hv2056hyc8gcdllcx3a41g1rnw64kng98"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
      (("rust-cfg-if" ,rust-cfg-if))))
    (home-page "https://github.com/derekjw/try_from")
    (synopsis "TryFrom and TryInto traits for failable conversions")
    (description
      "TryFrom and TryInto traits for failable conversions that return a Result.")
    (license license:expat)))

(define-public rust-try-lock
  (package
    (name "rust-try-lock")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "try-lock" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "10p36rx6pqi9d0zr876xa8vksx2m66ha45myakl50rn08dxyn176"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/seanmonstar/try-lock")
    (synopsis "Lightweight atomic lock")
    (description
     "This package provides a lightweight atomic lock.")
    (license license:expat)))

(define-public rust-typeable
  (package
    (name "rust-typeable")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "typeable" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "11w8dywgnm32hb291izjvh4zjd037ccnkk77ahk63l913zwzc40l"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/reem/rust-typeable")
    (synopsis "Exposes Typeable, for getting TypeIds at runtime")
    (description "Exposes Typeable, for getting TypeIds at runtime.")
    (license license:expat)))

(define-public rust-typemap
  (package
    (name "rust-typemap")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "typemap" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1xm1gbvz9qisj1l6d36hrl9pw8imr8ngs6qyanjnsad3h0yfcfv5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-unsafe-any" ,rust-unsafe-any))))
    (home-page "https://github.com/reem/rust-typemap")
    (synopsis "Typesafe store for many value types")
    (description
     "A typesafe store for many value types.")
    (license license:expat)))

(define-public rust-typenum
  (package
    (name "rust-typenum")
    (version "1.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "typenum" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0sc1jirllfhdi52z1xv9yqzxzpk6v7vadd13n7wvs1wnjipn6bb1"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/paholg/typenum")
    (synopsis "Rust library for type-level numbers evaluated at compile time")
    (description "Typenum is a Rust library for type-level numbers evaluated at
compile time.  It currently supports bits, unsigned integers, and signed
integers.  It also provides a type-level array of type-level numbers, but its
implementation is incomplete.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-ucd-trie
  (package
    (name "rust-ucd-trie")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ucd-trie" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1hh6kyzh5xygwy96wfmsf8v8czlzhps2lgbcyhj1xzy1w1xys04g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs
      (("rust-lazy-static" ,rust-lazy-static))))
    (home-page "https://github.com/BurntSushi/ucd-generate")
    (synopsis "Trie for storing Unicode codepoint sets and maps")
    (description
     "This package provides a trie for storing Unicode codepoint sets and maps.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-ucd-util
  (package
    (name "rust-ucd-util")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ucd-util" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0x088q5z0m09a2jqcfgsnq955y8syn1mgn35cl78qinkxm4kp6zs"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/BurntSushi/ucd-generate")
    (synopsis "library for working with the Unicode character database")
    (description "This package provides a small utility library for working
with the Unicode character database.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-unicase
  (package
    (name "rust-unicase")
    (version "2.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicase" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1xmpmkakhhblq7dzab1kwyv925kv7fqjkjsxjspg6ix9n88makm8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-version-check" ,rust-version-check-0.1))))
    (home-page "https://github.com/seanmonstar/unicase")
    (synopsis "Case-insensitive wrapper around strings")
    (description
     "A case-insensitive wrapper around strings.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-unicode-width
  (package
    (name "rust-unicode-width")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicode-width" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "09k5lipygardwy0660jhls08fsgknrazzivmn804gps53hiqc8w8"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/unicode-rs/unicode-width")
    (synopsis "Determine displayed width according to Unicode rules")
    (description "This crate allows you to determine displayed width of
@code{char} and @code{str} types according to Unicode Standard Annex #11 rules.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-unicode-xid
  (package
    (name "rust-unicode-xid")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicode-xid" version))
        (file-name
          (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0z09fn515xm7zyr0mmdyxa9mx2f7azcpv74pqmg611iralwpcvl2"))))
    (build-system cargo-build-system)
    (home-page
      "https://github.com/unicode-rs/unicode-xid")
    (synopsis "Determine Unicode XID related properties")
    (description "Determine whether characters have the XID_Start
or XID_Continue properties according to Unicode Standard Annex #31.")
    ;; Dual licensed.
    (license (list license:asl2.0 license:expat))))

(define-public rust-unicode-xid-0.1
  (package
    (inherit rust-unicode-xid)
    (name "rust-unicode-xid")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicode-xid" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1z57lqh4s18rr4x0j4fw4fmp9hf9346h0kmdgqsqx0fhjr3k0wpw"))))))

(define-public rust-unindent
  (package
    (name "rust-unindent")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unindent" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "14s97blyqgf9hzxk22iazrghj60midajkw2801dfspz3n2iqmwb3"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/dtolnay/indoc")
    (synopsis "Remove a column of leading whitespace from a string")
    (description "This crate allows you to remove a column of leading
whitespace from a string.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-unreachable
  (package
    (name "rust-unreachable")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unreachable" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0mps2il4xy2mjqc3appas27hhn2xmvixc3bzzhfrjj74gy3i0a1q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-void" ,rust-void))))
    (home-page "https://github.com/reem/rust-unreachable")
    (synopsis "Unreachable code optimization hint in rust")
    (description
     "This package provides an unreachable code optimization hint in rust.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-unsafe-any
  (package
    (name "rust-unsafe-any")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unsafe-any" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0zwwphsqkw5qaiqmjwngnfpv9ym85qcsyj7adip9qplzjzbn00zk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-traitobject" ,rust-traitobject))))
    (home-page "https://tokio.rs")
    (synopsis "Traits and implementations for unchecked downcasting")
    (description
     "Traits and implementations for unchecked downcasting.")
    (license license:expat)))

(define-public rust-untrusted
  (package
    (name "rust-untrusted")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "untrusted" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1kmfykcwif6ashkwg54gcnhxj03kpba2i9vc7z5rpr0xlgvrwdk0"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/briansmith/untrusted")
    (synopsis "Zero-allocation parsing of untrusted inputs in Rust")
    (description
     "Safe, fast, zero-panic, zero-crashing, zero-allocation parsing of
untrusted inputs in Rust.")
    (license license:isc)))

(define-public rust-vcpkg
  (package
    (name "rust-vcpkg")
    (version "0.2.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "vcpkg" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "15dzk1b96q946v9aisbd1bbhi33n93wvgziwh1shmscn1xflbp9k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs
       (("rust-lazy-static" ,rust-lazy-static)
        ("rust-tempdir" ,rust-tempdir))
       #:tests? #f)) ; tests try to link libmysql, graphite2, harfbuzz
    (home-page "https://github.com/mcgoo/vcpkg-rs")
    (synopsis "Find native dependencies in a vcpkg tree at build time")
    (description
     "This package provides a library to find native dependencies in a
@code{vcpkg} tree at build time in order to be used in Cargo build scripts.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-version-check
  (package
    (name "rust-version-check")
    (version "0.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "version_check" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1kikqlnggii1rvnxrbls55sc46lxvinz5k3giscgncjj4p87b1q7"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/SergioBenitez/version_check")
    (synopsis "Check that the installed rustc meets some version requirements")
    (description
     "This tiny crate checks that the running or installed rustc meets some
version requirements.  The version is queried by calling the Rust compiler with
@code{--version}.  The path to the compiler is determined first via the
@code{RUSTC} environment variable.  If it is not set, then @code{rustc} is used.
If that fails, no determination is made, and calls return None.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-version-check-0.1
  (package
    (inherit rust-version-check)
    (name "rust-version-check")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "version_check" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1pf91pvj8n6akh7w6j5ypka6aqz08b3qpzgs0ak2kjf4frkiljwi"))))))

(define-public rust-void
  (package
    (name "rust-void")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "void" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0zc8f0ksxvmhvgx4fdg0zyn6vdnbxd2xv9hfx4nhzg6kbs4f80ka"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/reem/rust-void")
    (synopsis "Void type for use in statically impossible cases")
    (description
     "The uninhabited void type for use in statically impossible cases.")
    (license license:expat)))

(define-public rust-walkdir
  (package
    (name "rust-walkdir")
    (version "2.2.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "walkdir" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "07ppalpvxkf8cnqr64np422792y4z5bs9m8b4nrflh5rm17wjn4n"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-same-file" ,rust-same-file)
        ("rust-winapi" ,rust-winapi)
        ("rust-winapi-util" ,rust-winapi-util))
       #:cargo-development-inputs
       (("rust-doc-comment" ,rust-doc-comment))))
    (home-page  "https://github.com/BurntSushi/walkdir")
    (synopsis "Recursively walk a directory")
    (description "Recursively walk a directory.")
    (license (list license:unlicense
                   license:expat))))

(define-public rust-wasi
  (package
    (name "rust-wasi")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasi" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1ir3pd4phdfml0cbziw9bqp7mnk0vfp9biy8bh25lln6raml4m7x"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/CraneStation/rust-wasi")
    (synopsis "Experimental WASI API bindings for Rust")
    (description "This package contains experimental WASI API bindings
in Rust.")
    (license license:asl2.0)))

(define-public rust-wasm-bindgen-shared
  (package
    (name "rust-wasm-bindgen-shared")
    (version "0.2.48")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-bindgen-shared" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "08rnfhjyk0f6liv8n4rdsvhx7r02glkhcbj2lp9lcbkbfpad9hnr"))))
    (build-system cargo-build-system)
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis "Shared support between wasm-bindgen and wasm-bindgen cli")
    (description "This package provides shared support between
@code{wasm-bindgen} and @code{wasm-bindgen} cli, an internal dependency.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-wasm-bindgen-test-macro
  (package
    (name "rust-wasm-bindgen-test-macro")
    (version "0.2.48")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-bindgen-test-macro" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0n28mr6vncf1k1qr2b5bvfxq4jvqkjdzq0z0ab6w2f5d6v8q3q3l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2)
        ("rust-quote" ,rust-quote))))
    (home-page "https://github.com/rustwasm/wasm-bindgen")
    (synopsis "Internal testing macro for wasm-bindgen")
    (description
     "This library contains the internal testing macro for wasm-bindgen.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-widestring
  (package
    (name "rust-widestring")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "widestring" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1dhx6dndjsz1y7c9w06922412kdxyrrkqblvggm76mh8z17hxz7g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs
       (("rust-winapi" ,rust-winapi))))
    (home-page "https://github.com/starkat99/widestring-rs")
    (synopsis "Wide string Rust FFI library")
    (description
     "A wide string Rust FFI library for converting to and from wide strings,
such as those often used in Windows API or other FFI libraries.  Both UTF-16 and
UTF-32 types are provided, including support for malformed encoding.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-winapi
  (package
    (name "rust-winapi")
    (version "0.3.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winapi" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1ii9j9lzrhwri0902652awifzx9fpayimbp6hfhhc296xcg0k4w0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-winapi-i686-pc-windows-gnu"
         ,rust-winapi-i686-pc-windows-gnu)
        ("rust-winapi-x86-64-pc-windows-gnu"
         ,rust-winapi-x86-64-pc-windows-gnu))))
    (home-page "https://github.com/retep998/winapi-rs")
    (synopsis "Raw FFI bindings for all of Windows API.")
    (description
     "Raw FFI bindings for all of Windows API.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-winapi-0.2
  (package
    (inherit rust-winapi)
    (name "rust-winapi")
    (version "0.2.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winapi" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0yh816lh6lf56dpsgxy189c2ai1z3j8mw9si6izqb6wsjkbcjz8n"))))
    ;; We do not want to package 48 -sys crates for a package we do not want.
    ;; They are all dev dependencies, so we skip building and testing.
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build))))))

(define-public rust-winapi-build
  (package
    (name "rust-winapi-build")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winapi-build" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1g4rqsgjky0a7530qajn2bbfcrl2v0zb39idgdws9b1l7gp5wc9d"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/retep998/winapi-rs")
    (synopsis "Common code for build.rs in WinAPI -sys crates")
    (description
     "Common code for build.rs in WinAPI -sys crates.")
    (license license:expat)))

(define-public rust-winapi-i686-pc-windows-gnu
  (package
    (name "rust-winapi-i686-pc-windows-gnu")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winapi-i686-pc-windows-gnu" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1dmpa6mvcvzz16zg6d5vrfy4bxgg541wxrcip7cnshi06v38ffxc"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/retep998/winapi-rs")
    (synopsis "Import libraries for the i686-pc-windows-gnu target")
    (description "This crate provides import libraries for the
i686-pc-windows-gnu target.  Please don't use this crate directly, depend on
@code{winapi} instead.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-winapi-util
  (package
    (name "rust-winapi-util")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winapi-util" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1j839dc6y8vszvrsb7yk0qvs0w6asnahxzbyans37vnsw6vbls3i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-winapi" ,rust-winapi))))
    (home-page "https://github.com/BurntSushi/winapi-util")
    (synopsis "Dumping ground for high level safe wrappers over winapi")
    (description
     "This package provides a dumping ground for high level safe wrappers over
winapi.")
    (license (list license:unlicense
                   license:expat))))

(define-public rust-winapi-x86-64-pc-windows-gnu
  (package
    (name "rust-winapi-x86-64-pc-windows-gnu")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winapi-x86_64-pc-windows-gnu" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0gqq64czqb64kskjryj8isp62m2sgvx25yyj3kpc2myh85w24bki"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/retep998/winapi-rs")
    (synopsis "Import libraries for the x86_64-pc-windows-gnu target")
    (description "This package provides import libraries for the
x86_64-pc-windows-gnu target.  Please don't use this crate directly, depend on
@code{winapi} instead.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-wincolor
  (package
    (name "rust-wincolor")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wincolor" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1agaf3hcav113i86912ajnw6jxcy4rvkrgyf8gdj8kc031mh3xcn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-winapi" ,rust-winapi)
        ("rust-winapi-util" ,rust-winapi-util))))
    (home-page "https://github.com/BurntSushi/termcolor/tree/master/wincolor")
    (synopsis "Windows API for controlling text color in a Windows console")
    (description
     "This package provides a simple Windows specific API for controlling text
color in a Windows console.")
    (license (list license:unlicense
                   license:expat))))

(define-public rust-winutil
  (package
    (name "rust-winutil")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winutil" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0vkyl3fbbf05n5ph5yz8sfaccrk9x3qsr25560w6w68ldf5i7bvx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-winapi" ,rust-winapi))
       ;; This unmaintained crate cannot find winapi when built directly.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build))))
    (home-page "https://bitbucket.org/DaveLancaster/winutil")
    (synopsis "Library wrapping a handful of useful winapi functions")
    (description
     "A simple library wrapping a handful of useful winapi functions.")
    (license license:expat)))

(define-public rust-ws2-32-sys
  (package
    (name "rust-ws2-32-sys")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ws2_32-sys" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0ppscg5qfqaw0gzwv2a4nhn5bn01ff9iwn6ysqnzm4n8s3myz76m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-winapi" ,rust-winapi-0.2)
        ("rust-winapi-build" ,rust-winapi-build))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-Cargo-toml
           (lambda _
             (substitute* "Cargo.toml"
               ((", path =.* }") "}\n"))
             #t)))))
    (home-page "https://github.com/retep998/winapi-rs")
    (synopsis "Function definitions for the Windows API library ws2_32")
    (description
     "Contains function definitions for the Windows API library ws2_32.")
    (license license:expat)))

(define-public rust-xattr
  (package
    (name "rust-xattr")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "xattr" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0k556fb6f5jc907975j9c8iynl2fqz3rf0w6fiig83i4yi0kfk14"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc))
       #:cargo-development-inputs
       (("rust-tempfile" ,rust-tempfile))
       #:tests? #f)) ; Tests assume read access to /var.
    (home-page "https://github.com/Stebalien/xattr")
    (synopsis "Unix extended filesystem attributes")
    (description
     "This package provide a small library for setting, getting, and listing
extended attributes.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-xdg
  (package
    (name "rust-xdg")
    (version "2.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "xdg" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0mws8a0fr3cqk5nh7aq9lmkmhzghvasqy4mhw6nnza06l4d6i2fh"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/whitequark/rust-xdg")
    (synopsis "Store and retrieve files according to XDG specification")
    (description
     "This package provides a library for storing and retrieving files according
to XDG Base Directory specification")
    (license (list license:asl2.0
                   license:expat))))
