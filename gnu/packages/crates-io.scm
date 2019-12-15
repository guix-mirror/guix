;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Ivan Petkov <ivanppetkov@gmail.com>
;;; Copyright © 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2019 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 John Soo <jsoo1@asu.edu>
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
  #:use-module (guix packages))

;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;

(define-public rust-adler32-1.0
  (package
    (name "rust-adler32")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "adler32" version))
        (file-name
          (string-append name "-" version ".crate"))
        (sha256
          (base32
            "0p7fxlnks9l7p7rwfqi7aqgnk2bps5zc0rjiw00mdw19nnbjjlky"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/remram44/adler32-rs")
    (synopsis "Implementation of the Adler32 rolling hash algorithm")
    (description
     "This library is an implementation of the Adler32 rolling hash algorithm in
the Rust programming language.")
    (properties '((hidden? . #t)))
    (license (list license:bsd-3
                   license:zlib))))

(define-public rust-afl-0.4
  (package
    (name "rust-afl")
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "afl" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0g2chc18ji7qxi0d03n2ai140qdcww958v5si6rcjnnhmri1vyfb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-cc" ,rust-cc-1.0)
        ("rust-clap" ,rust-clap-2)
        ("rust-rustc-version" ,rust-rustc-version-0.2)
        ("rust-xdg" ,rust-xdg-2.2))
       #:cargo-development-inputs
       (("rust-rustc-version" ,rust-rustc-version-0.2)
        ("rust-xdg" ,rust-xdg-2.2))))
    (home-page "https://github.com/rust-fuzz/afl.rs")
    (synopsis
     "Fuzzing Rust code with american-fuzzy-lop")
    (description
     "Fuzz Rust code with american-fuzzy-lop.")
    (license license:asl2.0)))

(define-public rust-ansi-term-0.11
  (package
    (name "rust-ansi-term")
    (version "0.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ansi_term" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "16wpvrghvd0353584i1idnsgm0r3vchg8fyrm0x8ayv1rgvbljgf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/ogham/rust-ansi-term")
    (synopsis "Library for ANSI terminal colours and styles")
    (description
     "This is a library for controlling colours and formatting, such as red bold
text or blue underlined text, on ANSI terminals.")
    (license license:expat)))

(define-public rust-antidote-1.0
  (package
    (name "rust-antidote")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "antidote" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "19g2sw2qa2ibnh4x7j1snk46593jgx6y7rnvva496ynq61af5z9l"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/sfackler/rust-antidote")
    (synopsis "Poison-free Mutex and RwLock types")
    (description
    "These types expose identical APIs to the standard library @code{Mutex} and
@code{RwLock} except that they do not return @code{PoisonError}s.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-approx-0.3
  (package
    (name "rust-approx")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "approx" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1hx580xjdxl3766js9b49rnbnmr8gw8c060809l43k9f0xshprph"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-num-complex" ,rust-num-complex-0.2)
        ("rust-num-traits" ,rust-num-traits-0.2))))
    (home-page "https://github.com/brendanzab/approx")
    (synopsis
     "Approximate floating point equality comparisons and assertions")
    (description
     "Approximate floating point equality comparisons and assertions.")
    (license license:asl2.0)))

(define-public rust-arrayvec-0.4
  (package
    (name "rust-arrayvec")
    (version "0.4.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "arrayvec" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0wcch3ca9qvkixgdbd2afrv1xa27l83vpraf7frsh9l8pivgpiwj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-nodrop" ,rust-nodrop-0.1)
        ("rust-serde" ,rust-serde-1.0))
       #:cargo-development-inputs
       (("rust-bencher" ,rust-bencher-0.1)
        ("rust-matches" ,rust-matches-0.1)
        ("rust-serde-test" ,rust-serde-test-1.0))))
    (home-page "https://github.com/bluss/arrayvec")
    (synopsis "Vector with fixed capacity")
    (description
     "This package provides a vector with fixed capacity, backed by an
array (it can be stored on the stack too).  Implements fixed capacity
ArrayVec and ArrayString.")
    (license (list license:expat license:asl2.0))))

(define-public rust-ascii-0.9
  (package
    (name "rust-ascii")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ascii" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0dck6rsjnxlczyjnncn8hf16bxj42m1vi6s2n32c1jg2ijd9dz55"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-quickcheck" ,rust-quickcheck-0.8)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-serde-test" ,rust-serde-test-1.0))))
    (home-page "https://github.com/tomprogrammer/rust-ascii")
    (synopsis
     "ASCII-only equivalents to char, str and String")
    (description
     "ASCII-only equivalents to @code{char}, @code{str} and @code{String}.")
    (license (list license:expat license:asl2.0))))

(define-public rust-atty-0.2
  (package
    (name "rust-atty")
    (version "0.2.13")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "atty" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "140sswp1bwqwc4zk80bxkbnfb3g936hgrb77g9g0k1zcld3wc0qq"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/softprops/atty")
    (synopsis "A simple interface for querying atty")
    (description
     "This package provides a simple interface for querying atty.")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-autocfg-0.1
  (package
    (name "rust-autocfg")
    (version "0.1.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "autocfg" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1chwgimpx5z7xbag7krr9d8asxfqbh683qhgl9kn3hxk2l0djj8x"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/cuviper/autocfg")
    (synopsis "Automatic cfg for Rust compiler features")
    (description "Rust library for build scripts to automatically configure
code based on compiler support.  Code snippets are dynamically tested to see
if the @code{rustc} will accept them, rather than hard-coding specific version
support.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-backtrace-sys-0.1
  (package
    (name "rust-backtrace-sys")
    (version "0.1.31")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "backtrace-sys" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0as2pk77br4br04daywhivpi1ixxb8y2c7f726kj849dxys31a42"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-lang/backtrace-rs")
    (synopsis "Bindings to the libbacktrace gcc library")
    (description
     "This package provides bindings to the libbacktrace gcc library.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-base64-0.10
  (package
    (name "rust-base64")
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "base64" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "13k6bvd3n6dm7jqn9x918w65dd9xhx454bqphbnv0bkd6n9dj98b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1.3))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.2)
        ("rust-rand" ,rust-rand-0.4))))
    (home-page "https://github.com/marshallpierce/rust-base64")
    (synopsis "Encodes and decodes base64 as bytes or utf8")
    (description
     "Encodes and decodes base64 as bytes or utf8.")
    (license (list license:expat license:asl2.0))))

(define-public rust-base-x-0.2
  (package
    (name "rust-base-x")
    (version "0.2.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "base-x" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0hv4y5cdhv6bk0ghk2434clw8v4mmk5cc9lsh6qrpri92zlfmx3n"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/OrKoN/base-x-rs")
    (synopsis "Encode/decode any base")
    (description "This library provides for encoding and decoding any base.")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-bencher-0.1
  (package
    (name "rust-bencher")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bencher" version))
        (file-name (string-append name "-" version ".crate"))
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
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-bincode-1.1
  (package
    (name "rust-bincode")
    (version "1.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bincode" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1xx6bp39irvsndk6prnmmq8m1l9p6q2qj21j6mfks2y81pjsa14z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-autocfg" ,rust-autocfg-0.1)
        ("rust-byteorder" ,rust-byteorder-1.3)
        ("rust-serde" ,rust-serde-1.0))
       #:cargo-development-inputs
       (("rust-serde-bytes" ,rust-serde-bytes-0.11)
        ("rust-serde-derive" ,rust-serde-derive-1.0))))
    (home-page "https://github.com/servo/bincode")
    (synopsis
     "Binary serialization/deserialization strategy")
    (description
     "This package provides a binary serialization/deserialization strategy
that uses Serde for transforming structs into bytes and vice versa!")
    (license license:expat)))

(define-public rust-bitflags-1
  (package
    (name "rust-bitflags")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bitflags" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1zc1qb1hwsnl2d8rhzicsv9kqd5b2hwbrscrcfw5as4sfr35659x"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bitflags/bitflags")
    (synopsis "Macro to generate structures which behave like bitflags")
    (description "This package provides a macro to generate structures which
behave like a set of bitflags.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-blas-sys-0.7
  (package
    (name "rust-blas-sys")
    (version "0.7.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "blas-sys" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0h14zjycwc76v15h8qll9z1xiryvmpvsb5gam97pqpdjrrwv5c8k"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/blas-lapack-rs/blas-sys")
    (synopsis "Bindings to BLAS (Fortran)")
    (description
     "Ths package provides bindings to BLAS (Fortran).")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-bumpalo-2.5
  (package
    (name "rust-bumpalo")
    (version "2.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bumpalo" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "018b5calz3895v04shk9bn7i73r4zf8yf7p1dqg92s3xya13vm1c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.2)
        ("rust-quickcheck" ,rust-quickcheck-0.8))))
    (home-page "https://github.com/fitzgen/bumpalo")
    (synopsis "Fast bump allocation arena for Rust")
    (description
     "This package provides a fast bump allocation arena for Rust.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-bstr-0.2
  (package
    (name "rust-bstr")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bstr" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0prq6yk3da0k5bg2czcgg1i4ynsq1l59xc89ycsv6v7p08p5gh3c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-lazy-static" ,rust-lazy-static-1.3)
        ("rust-memchr" ,rust-memchr-2.2)
        ("rust-regex-automata" ,rust-regex-automata-0.1)
        ("rust-serde" ,rust-serde-1.0))
       #:cargo-development-inputs
       (("rust-quickcheck" ,rust-quickcheck-0.8)
        ("rust-ucd-parse" ,rust-ucd-parse-0.1)
        ("rust-unicode-segmentation" ,rust-unicode-segmentation-1.3))))
    (home-page "https://github.com/BurntSushi/bstr")
    (synopsis
     "String type that is not required to be valid UTF-8")
    (description
     "This package provides a string type that is not required to be valid
UTF-8.")
    (license (list license:expat license:asl2.0))))

(define-public rust-bstr-0.1
  (package
    (inherit rust-bstr-0.2)
    (name "rust-bstr")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bstr" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0nzi9vqhl56ws8gq39f3aj4qjrr4l3g5lbkkcj8xq1x4cb74wq2r"))))))

(define-public rust-byteorder-1.3
  (package
    (name "rust-byteorder")
    (version "1.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "byteorder" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1xbwjlmq2ziqjmjvkqxdx1yh136xxhilxd40bky1w4d7hn4xvhx7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-development-inputs
       (("rust-doc-comment" ,rust-doc-comment-0.3)
        ("rust-quickcheck" ,rust-quickcheck-0.8)
        ("rust-rand" ,rust-rand-0.4))))
    (home-page
     "https://github.com/BurntSushi/byteorder")
    (synopsis
     "Reading/writing numbers in big-endian and little-endian")
    (description
     "Library for reading/writing numbers in big-endian and
little-endian.")
    (license (list license:expat license:unlicense))))

(define-public rust-bytes-0.4
  (package
    (name "rust-bytes")
    (version "0.4.12")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bytes" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0768a55q2fsqdjsvcv98ndg9dq7w2g44dvq1avhwpxrdzbydyvr0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1.3)
        ("rust-either" ,rust-either-1.5)
        ("rust-iovec" ,rust-iovec-0.1)
        ("rust-serde" ,rust-serde-1.0))
       #:cargo-development-inputs
       (("rust-serde-test" ,rust-serde-test-1.0))))
    (home-page "https://github.com/tokio-rs/bytes")
    (synopsis
     "Types and traits for working with bytes")
    (description
     "Types and traits for working with bytes.")
    (license license:expat)))

(define-public rust-cargon-0.0
  (package
    (name "rust-cargon")
    (version "0.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cargon" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1cszlab7jk736p0lb50ag4l9nv72m7j41bwrmygl0lr4iz0350w2"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bryant/argon2rs")
    (synopsis "Thin wrapper around the Argon2 C library")
    (description
     "This package provides a thin wrapper around the Argon2 C library.  It is
used in argon2rs' bench suite.")
    (properties '((hidden? . #t)))
    (license license:wtfpl2)))

(define-public rust-cast-0.2
  (package
    (name "rust-cast")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cast" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "09yl2700crxa4n860b080msij25klvs1kfzazhp2aihchvr16q4j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-development-inputs
       (("rust-quickcheck" ,rust-quickcheck-0.8))))
    (home-page "https://github.com/japaric/cast.rs")
    (synopsis
     "Ergonomic, checked cast functions for primitive types")
    (description
     "Ergonomic, checked cast functions for primitive types.")
    (license (list license:expat license:asl2.0))))

(define-public rust-cblas-sys-0.1
  (package
    (name "rust-cblas-sys")
    (version "0.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cblas-sys" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0rgsn3klhhh09d8qf3b87zl4rwk93l2g0qzh9hhb0lff5kcfrzmn"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/blas-lapack-rs/cblas-sys")
    (synopsis "Bindings to CBLAS (C)")
    (description
     "The package provides bindings to CBLAS (C).")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-cc-1.0
  (package
    (name "rust-cc")
    (version "1.0.41")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cc" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1zxzd559dbbf1iwdzmkj7czapzccs17kqqmsj9ayijpdix5rrbld"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/alexcrichton/cc-rs")
    (synopsis "Invoke the native C compiler")
    (description
     "This package provides a build-time dependency for Cargo build scripts to
assist in invoking the native C compiler to compile native C code into a static
archive to be linked into Rustcode.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-chrono-0.4
  (package
    (name "rust-chrono")
    (version "0.4.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "chrono" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1glam3iqhshbamzgf0npn7hgghski92r31lm7gg8841hnxc1zn3p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-num-integer" ,rust-num-integer-0.1)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-rustc-serialize" ,rust-rustc-serialize-0.3)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-time" ,rust-time-0.1))
       #:cargo-development-inputs
       (("rust-bincode" ,rust-bincode-1.1)
        ("rust-doc-comment" ,rust-doc-comment-0.3)
        ("rust-num-iter" ,rust-num-iter-0.1)
        ("rust-serde-derive" ,rust-serde-derive-1.0)
        ("rust-serde-json" ,rust-serde-json-1.0))))
    (home-page
     "https://github.com/chronotope/chrono")
    (synopsis "Date and time library for Rust")
    (description "Date and time library for Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-cfg-if-0.1
  (package
    (name "rust-cfg-if")
    (version "0.1.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cfg-if" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "08h80ihs74jcyp24cd75wwabygbbdgl05k6p5dmq8akbr78vv1a7"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/alexcrichton/cfg-if")
    (synopsis "Define an item depending on parameters")
    (description "This package provides a macro to ergonomically define an item
depending on a large number of #[cfg] parameters.  Structured like an
@code{if-else} chain, the first matching branch is the item that gets emitted.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-clang-sys-0.28
  (package
    (name "rust-clang-sys")
    (version "0.28.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clang-sys" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0ls8zcqi5bmmrvrk3b6r1ym4wlivinbv590d2dvg2xn9f44mbpl1"))))
    (build-system cargo-build-system)
    ;(arguments
    ; `(#:phases
    ;   (modify-phases %standard-phases
    ;     (add-after 'unpack 'set-environmental-variable
    ;       (lambda* (#:key inputs #:allow-other-keys)
    ;         (let ((clang (assoc-ref inputs "libclang")))
    ;           (setenv "LIBCLANG_PATH"
    ;                   (string-append clang "/lib")))
    ;         #t)))))
    ;(inputs
    ; `(("libclang" ,clang)))
    (home-page "https://github.com/KyleMayes/clang-sys")
    (synopsis "Rust bindings for libclang")
    (description
     "This package provides Rust bindings for @code{libclang}.")
    (properties '((hidden? . #t)))
    (license license:asl2.0)))

(define-public rust-clang-sys-0.26
  (package
    (inherit rust-clang-sys-0.28)
    (name "rust-clang-sys")
    (version "0.26.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clang-sys" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1r50dwy5hj5gq07dn0qf8222d07qv0970ymx0j8n9779yayc3w3f"))))))

(define-public rust-clap-2
  (package
    (name "rust-clap")
    (version "2.33.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clap" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1nf6ld3bims1n5vfzhkvcb55pdzh04bbhzf8nil5vvw05nxzarsh"))))
    (build-system cargo-build-system)
    (home-page "https://clap.rs/")
    (synopsis "Command Line Argument Parser")
    (description
     "This package provides a simple to use, efficient, and full-featured
Command Line Argument Parser.")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-clicolors-control-1.0
  (package
    (name "rust-clicolors-control")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "clicolors-control" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1y80cgarxhrd1bz5yjm81r444v6flvy36aaxrrsac0yhfd6gvavk"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/mitsuhiko/clicolors-control")
    (synopsis "Common utility library to control CLI colorization")
    (description
     "This package provides a common utility library to control CLI
colorization.")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-clippy-0.0
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
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-term" ,rust-term-0.5.1))))
    (home-page "https://github.com/rust-lang/rust-clippy")
    (synopsis
     "Lints to avoid common pitfalls in Rust")
    (description
     "This package provides a bunch of helpful lints to avoid common
pitfalls in Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-cloudabi-0.0
  (package
    (name "rust-cloudabi")
    (version "0.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cloudabi" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0kxcg83jlihy0phnd2g8c2c303px3l2p3pkjz357ll6llnd5pz6x"))))
    (build-system cargo-build-system)
    (home-page "https://nuxi.nl/cloudabi/")
    (synopsis "Low level interface to CloudABI")
    (description
     "Low level interface to CloudABI.  Contains all syscalls and related types.")
    (properties '((hidden? . #t)))
    (license license:bsd-2)))

(define-public rust-cmake-0.1
  (package
    (name "rust-cmake")
    (version "0.1.42")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cmake" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0qkwibkvx5xjazvv9v8gvdlpky2jhjxvcz014nrixgzqfyv2byw1"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/alexcrichton/cmake-rs")
    (synopsis "Rust build dependency for running cmake")
    (description
     "This package provides a build dependency for running @code{cmake} to build
a native library.  The CMake executable is assumed to be @code{cmake} unless the
CMAKE environmental variable is set.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

;; This package requires features which are unavailable
;; on the stable releases of Rust.
(define-public rust-compiler-builtins-0.1
  (package
    (name "rust-compiler-builtins")
    (version "0.1.19")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "compiler_builtins" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1fpabpmg8paj4r5a37vmidh1jx1b7a6ilxm4s3xsxczx27ybjcjf"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-lang-nursery/compiler-builtins")
    (synopsis "Compiler intrinsics used by the Rust compiler")
    (description
     "This package provides compiler intrinsics used by the Rust compiler.  This
package is primarily useful when building the @code{core} crate yourself and you
need compiler-rt intrinsics.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-constant-time-eq-0.1
  (package
    (name "rust-constant-time-eq")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "constant_time_eq" version))
       (file-name (string-append name "-" version ".crate"))
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
    (properties '((hidden? . #t)))
    (license license:cc0)))

(define-public rust-core-foundation-sys-0.6
  (package
    (name "rust-core-foundation-sys")
    (version "0.6.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "core-foundation-sys" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0fzsw1j9g1x598yhwklg59l15hwzc0pyvs01w9fg2kin4598mjp7"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/servo/core-foundation-rs")
    (synopsis "Bindings to Core Foundation for OS X")
    (description
     "Bindings to Core Foundation for OS X.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-crc32fast-1.2
  (package
    (name "rust-crc32fast")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crc32fast" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1c9dhkvf3brrzzplcijaywxi2w8wv5578i0ryhcm7x8dmzi5s4ms"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if-0.1))
       #:cargo-development-inputs
       (("rust-bencher" ,rust-bencher-0.1)
        ("rust-quickcheck" ,rust-quickcheck-0.8)
        ("rust-rand" ,rust-rand-0.4))))
    (home-page "https://github.com/srijs/rust-crc32fast")
    (synopsis
     "Fast, SIMD-accelerated CRC32 (IEEE) checksum computation")
    (description
     "Fast, SIMD-accelerated CRC32 (IEEE) checksum computation.")
    (license (list license:expat license:asl2.0))))

(define-public rust-criterion-0.2
  (package
    (name "rust-criterion")
    (version "0.2.11")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "criterion" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1543wlpc4p1kz7sqqa7ylr8bkdr8l4f34hy4bxj7krpkahwhaqq3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-atty" ,rust-atty-0.2)
        ("rust-cast" ,rust-cast-0.2)
        ("rust-clap" ,rust-clap-2)
        ("rust-criterion-plot" ,rust-criterion-plot-0.3)
        ("rust-csv" ,rust-csv-1.1)
        ("rust-itertools" ,rust-itertools-0.8)
        ("rust-lazy-static" ,rust-lazy-static-1.3)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-rand-core" ,rust-rand-core-0.5)
        ("rust-rand-os" ,rust-rand-os-0.2)
        ("rust-rand-xoshiro" ,rust-rand-xoshiro-0.3)
        ("rust-rayon" ,rust-rayon-1.1)
        ("rust-rayon-core" ,rust-rayon-core-1.5)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-serde-derive" ,rust-serde-derive-1.0)
        ("rust-serde-json" ,rust-serde-json-1.0)
        ("rust-tinytemplate" ,rust-tinytemplate-1.0)
        ("rust-walkdir" ,rust-walkdir-2.2))
       #:cargo-development-inputs
       (("rust-approx" ,rust-approx-0.3)
        ("rust-quickcheck" ,rust-quickcheck-0.8)
        ("rust-rand" ,rust-rand-0.4)
        ("rust-tempdir" ,rust-tempdir-0.3))))
    (home-page "https://bheisler.github.io/criterion.rs/book/index.html")
    (synopsis "Statistics-driven micro-benchmarking library")
    (description
     "Statistics-driven micro-benchmarking library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-criterion-plot-0.3
  (package
    (name "rust-criterion-plot")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "criterion-plot" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "13pv09z4ryp70qyzablkibwa2mh6c2852qq1sjr9wjigvwnj3ybn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1.3)
        ("rust-cast" ,rust-cast-0.2)
        ("rust-itertools" ,rust-itertools-0.8))
       #:cargo-development-inputs
       (("rust-itertools-num" ,rust-itertools-num-0.1)
        ("rust-num-complex" ,rust-num-complex-0.2)
        ("rust-rand" ,rust-rand-0.4))))
    (home-page "https://github.com/bheisler/criterion.rs")
    (synopsis "Criterion's plotting library")
    (description "Criterion's plotting library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-crossbeam-0.7
  (package
    (name "rust-crossbeam")
    (version "0.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crossbeam" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0g5jysq5x4gndc1v5sq9n3f1m97k7qihwdpigw6ar6knj14qm09d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if-0.1)
        ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.3)
        ("rust-crossbeam-deque" ,rust-crossbeam-deque-0.7)
        ("rust-crossbeam-epoch" ,rust-crossbeam-epoch-0.7)
        ("rust-crossbeam-queue" ,rust-crossbeam-queue-0.1)
        ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.6))
       #:cargo-development-inputs
       (("rust-rand" ,rust-rand-0.4))))
    (home-page "https://github.com/crossbeam-rs/crossbeam")
    (synopsis "Tools for concurrent programming")
    (description "Tools for concurrent programming.")
    (license (list license:expat license:asl2.0))))

(define-public rust-crossbeam-channel-0.3
  (package
    (name "rust-crossbeam-channel")
    (version "0.3.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crossbeam-channel" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0azpymyk0ld4akrjfy69ck5pzfgz1f2gb3smm2ywld92vsjd23hg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-crossbeam-utils" ,rust-crossbeam-utils-0.6)
        ("rust-smallvec" ,rust-smallvec-0.6))
       #:cargo-development-inputs
       (("rust-rand" ,rust-rand-0.4)
        ("rust-signal-hook" ,rust-signal-hook-0.1))))
    (home-page
     "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-channel")
    (synopsis
     "Multi-producer multi-consumer channels for message passing")
    (description
     "Multi-producer multi-consumer channels for message passing.")
    (license (list license:expat
                   license:asl2.0
                   license:bsd-2))))

(define-public rust-crossbeam-deque-0.7
  (package
    (name "rust-crossbeam-deque")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crossbeam-deque" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0was9x71cz5g1y3670cyy6jdmsdfg6k9mbf0ddz2k1mdd7hx535i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-crossbeam-epoch" ,rust-crossbeam-epoch-0.7)
        ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.6))
       #:cargo-development-inputs
       (("rust-rand" ,rust-rand-0.4))))
    (home-page
     "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-deque")
    (synopsis "Concurrent work-stealing deque")
    (description "Concurrent work-stealing deque.")
    (license (list license:expat license:asl2.0))))

(define-public rust-crossbeam-epoch-0.7
  (package
    (name "rust-crossbeam-epoch")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crossbeam-epoch" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1d408b9x82mdbnb405gw58v5mmdbj2rl28a1h7b9rmn25h8f7j84"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-arrayvec" ,rust-arrayvec-0.4)
        ("rust-cfg-if" ,rust-cfg-if-0.1)
        ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.6)
        ("rust-lazy-static" ,rust-lazy-static-1.3)
        ("rust-memoffset" ,rust-memoffset-0.2)
        ("rust-scopeguard" ,rust-scopeguard-0.3))
       #:cargo-development-inputs
       (("rust-rand" ,rust-rand-0.4))))
    (home-page
     "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-epoch")
    (synopsis "Epoch-based garbage collection")
    (description "Epoch-based garbage collection.")
    (license (list license:expat license:asl2.0))))

(define-public rust-crossbeam-queue-0.1
  (package
    (name "rust-crossbeam-queue")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crossbeam-queue" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0jsa9dbxnwqcxfws09vaschf92d4imlbbikmcn4ka8z7rzb9r5vw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-crossbeam-utils" ,rust-crossbeam-utils-0.6))
       #:cargo-development-inputs
       (("rust-rand" ,rust-rand-0.4))))
    (home-page
     "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-utils")
    (synopsis "Concurrent queues")
    (description "Concurrent queues.")
    (license (list license:expat
                   license:asl2.0
                   license:bsd-2))))

(define-public rust-crossbeam-utils-0.6
  (package
    (name "rust-crossbeam-utils")
    (version "0.6.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crossbeam-utils" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0p5aa8k3wpsn17md4rx038ac2azm9354knbxdfvn7dd7yk76yc7q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if-0.1)
        ("rust-lazy-static" ,rust-lazy-static-1.3))
       #:cargo-development-inputs
       (("rust-rand" ,rust-rand-0.4))))
    (home-page
     "https://github.com/crossbeam-rs/crossbeam/tree/master/crossbeam-utils")
    (synopsis "Utilities for concurrent programming")
    (description
     "Utilities for concurrent programming.")
    (license (list license:expat license:asl2.0))))

(define-public rust-csv-1.1
  (package
    (name "rust-csv")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "csv" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0qxvzq030hi915dszazv6a7f0apzzi7gn193ni0g2lzkawjxck55"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bstr" ,rust-bstr-0.2)
        ("rust-csv-core" ,rust-csv-core-0.1)
        ("rust-itoa" ,rust-itoa-0.4)
        ("rust-ryu" ,rust-ryu-1.0)
        ("rust-serde" ,rust-serde-1.0))
       #:cargo-development-inputs
       (("rust-serde" ,rust-serde-1.0))))
    (home-page "https://github.com/BurntSushi/rust-csv")
    (synopsis "Fast CSV parsing with support for serde")
    (description
     "Fast CSV parsing with support for serde.")
    (license (list license:unlicense license:expat))))

(define-public rust-csv-core-0.1
  (package
    (name "rust-csv-core")
    (version "0.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "csv-core" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0k5zs0x0qmmn27pa5kcg86lg84s29491fw5sh3zswxswnavasp4v"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-memchr" ,rust-memchr-2.2))
       #:cargo-development-inputs
       (("rust-arrayvec" ,rust-arrayvec-0.4))))
    (home-page "https://github.com/BurntSushi/rust-csv")
    (synopsis
     "Bare bones CSV parsing with no_std support")
    (description
     "Bare bones CSV parsing with no_std support.")
    (license (list license:unlicense license:expat))))

(define-public rust-curl-sys-0.4
  (package
    (name "rust-curl-sys")
    (version "0.4.20")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "curl-sys" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "02542zmvl3fpdqf7ai4cqnamm4albx9j645dkjx5qr1myq8ax42y"))))
    (build-system cargo-build-system)
    ;(arguments
    ; `(#:phases
    ;   (modify-phases %standard-phases
    ;    (add-after 'unpack 'find-openssl
    ;      (lambda* (#:key inputs #:allow-other-keys)
    ;        (let ((openssl (assoc-ref inputs "openssl")))
    ;          (setenv "OPENSSL_DIR" openssl))
    ;        #t)))))
    ;(native-inputs
    ; `(("pkg-config" ,pkg-config)))
    ;(inputs
    ; `(("curl" ,curl)
    ;   ("nghttp2" ,nghttp2)
    ;   ("openssl" ,openssl)
    ;   ("zlib" ,zlib)))
    (home-page "https://github.com/alexcrichton/curl-rust")
    (synopsis "Native bindings to the libcurl library")
    (description
     "This package provides native bindings to the @code{libcurl} library.")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-data-encoding-2.1
  (package
    (name "rust-data-encoding")
    (version "2.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "data-encoding" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "15xd6afhsjl08285piwczrafmckpp8i29padj8v12xhahshprx7l"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/ia0/data-encoding")
    (synopsis "Efficient and customizable data-encoding functions")
    (description
     "This library provides encodings for many different common cases, including
hexadecimal, base32, and base64.")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-defmac-0.2
  (package
    (name "rust-defmac")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "defmac" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "01ff3jdmcc5waffkwllndnx5hsn414r7x1rq4ib73n7awsyzxkxv"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bluss/defmac")
    (synopsis "Macro to define lambda-like macros inline")
    (description "A macro to define lambda-like macros inline.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-defmac-0.1
  (package
    (inherit rust-defmac-0.2)
    (name "rust-defmac")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "defmac" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "17giv0n0n1r64z0dahfvkjy3ys517jxyhs8sd9lmgvcljpjyryxa"))))))

(define-public rust-cpp-demangle-0.2
  (package
    (name "rust-cpp-demangle")
    (version "0.2.12")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cpp_demangle" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0a4hqsfc0sfdwy7pcr0rc1fjp2j47fxbkqfc2lfrbi4zlm5hq36k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-afl" ,rust-afl-0.4)
        ("rust-cfg-if" ,rust-cfg-if-0.1))
       #:cargo-development-inputs
       (("rust-clap" ,rust-clap-2)
        ("rust-diff" ,rust-diff-0.1)
        ("rust-glob" ,rust-glob-0.3))))
    (home-page "https://github.com/gimli-rs/cpp_demangle")
    (synopsis "Demangle C++ symbols")
    (description
     "This package provides a crate for demangling C++ symbols.")
    (license (list license:expat license:asl2.0))))

(define-public rust-demo-hack-0.0
  (package
    (name "rust-demo-hack")
    (version "0.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "demo-hack" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0m0114p1g0zzrdph5bg03i8m8p70vrwn3whs191jrbjcrmh5lmnp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-demo-hack-impl" ,rust-demo-hack-impl-0.0)
        ("rust-proc-macro-hack" ,rust-proc-macro-hack-0.5))))
    (home-page "https://github.com/dtolnay/proc-macro-hack")
    (synopsis "Demo of proc-macro-hack")
    (description "Demo of proc-macro-hack.")
    (license (list license:expat license:asl2.0))))

(define-public rust-demo-hack-impl-0.0
  (package
    (name "rust-demo-hack-impl")
    (version "0.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "demo-hack-impl" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1f1fdl60xjas9wlmcl9v6f56vgm3mzwr019kcifav5464rx3w3ld"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-proc-macro-hack" ,rust-proc-macro-hack-0.5)
        ("rust-quote" ,rust-quote-1.0)
        ("rust-syn" ,rust-syn-0.15))))
    (home-page "https://github.com/dtolnay/proc-macro-hack")
    (synopsis "Demo of proc-macro-hack")
    (description "Demo of proc-macro-hack.")
    (license (list license:expat license:asl2.0))))

(define-public rust-diff-0.1
  (package
    (name "rust-diff")
    (version "0.1.11")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "diff" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0fhavni46a2rib93ig5fgbqmm48ysms5sxzb3h9bp7vp2bwnjarw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-development-inputs
       (("rust-quickcheck" ,rust-quickcheck-0.8)
        ("rust-speculate" ,rust-speculate-0.1))))
    (home-page "https://github.com/utkarshkukreti/diff.rs")
    (synopsis
     "LCS based slice and string diffing implementation")
    (description
     "An LCS based slice and string diffing implementation.")
    (license (list license:expat license:asl2.0))))

(define-public rust-dirs-1.0
  (package
    (name "rust-dirs")
    (version "1.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "dirs" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "02vigc566z5i6n9wr2x8sch39qp4arn89xhhrh18fhpm3jfc0ygn"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/soc/dirs-rs")
    (synopsis "Abstractions for standard locations for various platforms")
    (description
     "This package provides a tiny low-level library that provides
platform-specific standard locations of directories for config, cache and other
data on Linux, Windows, macOS and Redox by leveraging the mechanisms defined by
the XDG base/user directory specifications on Linux, the Known Folder API on
Windows, and the Standard Directory guidelines on macOS.")
    (properties '((hidden? . #t)))
    (license (list license:expat license:asl2.0))))

(define-public rust-discard-1.0
  (package
    (name "rust-discard")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "discard" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1h67ni5bxvg95s91wgicily4ix7lcw7cq0a5gy9njrybaibhyb91"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/Pauan/rust-discard")
    (synopsis "Allow for intentionally leaking memory")
    (description "There are situations where you need to intentionally leak some
memory but not other memory.  This package provides a discard trait which allows
for intentionally leaking memory")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-doc-comment-0.3
  (package
    (name "rust-doc-comment")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "doc-comment" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "15rsqxgarfpb1yim9sbp9yfgj7p2dq6v51c6bq1a62paii9ylgcj"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/GuillaumeGomez/doc-comment")
    (synopsis "Macro to generate doc comments")
    (description "This package provides a way to generate doc comments
from macros.")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-docopt-1.1
  (package
    (name "rust-docopt")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "docopt" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0s9rcpmnnivs502q69lc1h1wrwapkq09ikgbfbgqf31idmc5llkz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-lazy-static" ,rust-lazy-static-1.3)
        ("rust-regex" ,rust-regex-1.1)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-strsim" ,rust-strsim-0.9))))
    (home-page "https://github.com/docopt/docopt.rs")
    (synopsis "Command line argument parsing")
    (description "Command line argument parsing.")
    (license (list license:expat license:unlicense))))

(define-public rust-dtoa-0.4
  (package
    (name "rust-dtoa")
    (version "0.4.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "dtoa" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0phbm7i0dpn44gzi07683zxaicjap5064w62pidci4fhhciv8mza"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/dtolnay/dtoa")
    (synopsis "Fast functions for printing floating-point primitives")
    (description "This crate provides fast functions for printing
floating-point primitives to an @code{io::Write}.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-dtoa-0.2
  (package
    (inherit rust-dtoa-0.4)
    (name "rust-dtoa")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "dtoa" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0g96cap6si1g6wi62hsdk2fnj3sf5vd4i97zj6163j8hhnsl3n0d"))))))

(define-public rust-either-1.5
  (package
    (name "rust-either")
    (version "1.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "either" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0yyggfd5yq9hyyp0bd5jj0fgz3rwws42d19ri0znxwwqs3hcy9sm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1.0))))
    (home-page "https://github.com/bluss/either")
    (synopsis
     "Enum @code{Either} with variants @code{Left} and @code{Right}")
    (description
     "The enum @code{Either} with variants @code{Left} and
@code{Right} is a general purpose sum type with two cases.")
    (license (list license:expat license:asl2.0))))

(define-public rust-encode-unicode-0.3
  (package
    (name "rust-encode-unicode")
    (version "0.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "encode_unicode" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1g8a8pixkxz6r927f4sc4r15qyc0szxdxb1732v8q7h0di4wkclh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-ascii" ,rust-ascii-0.9)
        ("rust-clippy" ,rust-clippy-0.0))
       #:cargo-development-inputs
       (("rust-lazy-static" ,rust-lazy-static-1.3))))
    (home-page "https://github.com/tormol/encode_unicode")
    (synopsis
     "UTF-8 and UTF-16 support for char, u8 and u16")
    (description
     "UTF-8 and UTF-16 character types, iterators and related methods for
char, u8 and u16.")
    (license (list license:expat license:asl2.0))))

(define-public rust-env-logger-0.6
  (package
    (name "rust-env-logger")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "env_logger" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1lx2s5nk96xx4i3m4zc4ghqgi8kb07dsnyiv8jk2clhax42dxz5a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-atty" ,rust-atty-0.2)
        ("rust-humantime" ,rust-humantime-1.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-regex" ,rust-regex-1.1)
        ("rust-termcolor" ,rust-termcolor-1.0))))
    (home-page
     "https://github.com/sebasmagri/env_logger/")
    (synopsis
     "Logging implementation for @code{log}")
    (description
     "This package provides a logging implementation for @code{log} which
is configured via an environment variable.")
    (license (list license:expat license:asl2.0))))

(define-public rust-fallible-iterator-0.2
  (package
    (name "rust-fallible-iterator")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fallible-iterator" version))
        (file-name (string-append name "-" version ".crate"))
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
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-filetime-0.2
  (package
    (name "rust-filetime")
    (version "0.2.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "filetime" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0sflihq2l77xjrza7yjalnxsc7dxzg25rhzcfbd9vmyfah5kimvb"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/alexcrichton/filetime")
    (synopsis "Platform-agnostic accessors of timestamps in File metadata")
    (description
     "This library contains a helper library for inspecting and setting the
various timestamps of files in Rust.  This library takes into account
cross-platform differences in terms of where the timestamps are located, what
they are called, and how to convert them into a platform-independent
representation.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-findshlibs-0.5
  (package
    (name "rust-findshlibs")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "findshlibs" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1n2vagn0q5yim32hxkwi1cjgp3yn1dm45p7z8nw6lapywihhs9mi"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/gimli-rs/findshlibs")
    (synopsis "Find the set of shared libraries loaded in the current process")
    (description
     "Find the set of shared libraries loaded in the current process with a
cross platform API.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-fixedbitset-0.1
  (package
    (name "rust-fixedbitset")
    (version "0.1.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fixedbitset" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0czam11mi80dbyhf4rd4lz0ihcf7vkfchrdcrn45wbs0h40dxm46"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/petgraph/fixedbitset")
    (synopsis "FixedBitSet is a simple bitset collection")
    (description "FixedBitSet is a simple bitset collection.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-flate2-1.0
  (package
    (name "rust-flate2")
    (version "1.0.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "flate2" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1n639gc7sbmrkir6pif608xqpwcv60kigmp5cn9x7m8892nk82am"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-crc32fast" ,rust-crc32fast-1.2)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-libz-sys" ,rust-libz-sys-1.0)
        ("rust-miniz-sys" ,rust-miniz-sys-0.1)
        ("rust-miniz-oxide-c-api" ,rust-miniz-oxide-c-api-0.2)
        ("rust-tokio-io" ,rust-tokio-io-0.1))
       #:cargo-development-inputs
       (("rust-futures" ,rust-futures-0.1)
        ("rust-quickcheck" ,rust-quickcheck-0.8)
        ("rust-rand" ,rust-rand-0.4)
        ("rust-tokio-io" ,rust-tokio-io-0.1)
        ("rust-tokio-tcp" ,rust-tokio-tcp-0.1)
        ("rust-tokio-threadpool" ,rust-tokio-threadpool-0.1))))
    (home-page "https://github.com/alexcrichton/flate2-rs")
    (synopsis
     "Bindings to miniz.c for DEFLATE compression and decompression")
    (description
     "Bindings to miniz.c for DEFLATE compression and decompression exposed as
Reader/Writer streams.  Contains bindings for zlib, deflate, and gzip-based
streams.")
    (license (list license:expat license:asl2.0))))

(define-public rust-fnv-1.0
  (package
    (name "rust-fnv")
    (version "1.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fnv" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1ww56bi1r5b8id3ns9j3qxbi7w5h005rzhiryy0zi9h97raqbb9g"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/servo/rust-fnv")
    (synopsis "implementation of the Fowler-Noll-Vo hash function")
    (description "The @code{fnv} hash function is a custom @code{Hasher}
implementation that is more efficient for smaller hash keys.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-foreign-types-0.3
  (package
    (name "rust-foreign-types")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "foreign-types" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1cgk0vyd7r45cj769jym4a6s7vwshvd0z4bqrb92q1fwibmkkwzn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-foreign-types-macros" ,rust-foreign-types-macros-0.1)
        ("rust-foreign-types-shared" ,rust-foreign-types-shared-0.1))))
    (home-page "https://github.com/sfackler/foreign-types")
    (synopsis "Framework for Rust wrappers over C APIs")
    (description
     "This package provides a framework for Rust wrappers over C
APIs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-foreign-types-macros-0.1
  (package
    (name "rust-foreign-types-macros")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "foreign-types-macros" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "16yjigjcsklcwy2ad32l24k1nwm9n3bsnyhxc3z9whjbsrj60qk6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-0.4)
        ("rust-quote" ,rust-quote-1.0)
        ("rust-syn" ,rust-syn-0.15))))
    (home-page "https://github.com/sfackler/foreign-types")
    (synopsis "An internal crate used by foreign-types")
    (description
     "An internal crate used by foreign-types.")
    (license (list license:expat license:asl2.0))))

(define-public rust-foreign-types-shared-0.2
  (package
    (name "rust-foreign-types-shared")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "foreign-types-shared" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0kanxlif1vp0ffh2r9l610jqbkmb3183yqykxq1z5w1vay2rn7y6"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/sfackler/foreign-types")
    (synopsis "An internal crate used by foreign-types")
    (description
     "An internal crate used by foreign-types.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-foreign-types-shared-0.1
  (package
    (inherit rust-foreign-types-shared-0.2)
    (name "rust-foreign-types-shared")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "foreign-types-shared" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0jxgzd04ra4imjv8jgkmdq59kj8fsz6w4zxsbmlai34h26225c00"))))))

(define-public rust-fs-extra-1.1
  (package
    (name "rust-fs-extra")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fs_extra" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0x6675wdhsx277k1k1235jwcv38naf20d8kwrk948ds26hh4lajz"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/webdesus/fs_extra")
    (synopsis "Extra filesystem methods")
    (description "Expanding opportunities standard library @code{std::fs} and
@code{std::io}.  Recursively copy folders with recept information about
process and much more.")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-fuchsia-cprng-0.1
  (package
    (name "rust-fuchsia-cprng")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fuchsia-cprng" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1fnkqrbz7ixxzsb04bsz9p0zzazanma8znfdqjvh39n14vapfvx0"))))
    (build-system cargo-build-system)
    (home-page "https://fuchsia.googlesource.com/fuchsia/+/master/garnet/public/rust/fuchsia-cprng")
    (synopsis "Fuchsia cryptographically secure pseudorandom number generator")
    (description "Rust crate for the Fuchsia cryptographically secure
pseudorandom number generator")
    (properties '((hidden? . #t)))
    (license license:bsd-3)))

(define-public rust-fuchsia-zircon-0.3
  (package
    (name "rust-fuchsia-zircon")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fuchsia-zircon" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "10jxc5ks1x06gpd0xg51kcjrxr35nj6qhx2zlc5n7bmskv3675rf"))))
    (build-system cargo-build-system)
    (home-page "https://fuchsia.googlesource.com/garnet/")
    (synopsis "Rust bindings for the Zircon kernel")
    (description "Rust bindings for the Zircon kernel.")
    (properties '((hidden? . #t)))
    (license license:bsd-3)))

(define-public rust-fuchsia-zircon-sys-0.3
  (package
    (name "rust-fuchsia-zircon-sys")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "fuchsia-zircon-sys" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "19zp2085qsyq2bh1gvcxq1lb8w6v6jj9kbdkhpdjrl95fypakjix"))))
    (build-system cargo-build-system)
    (home-page "https://fuchsia.googlesource.com/garnet/")
    (synopsis "Low-level Rust bindings for the Zircon kernel")
    (description "Low-level Rust bindings for the Zircon kernel.")
    (properties '((hidden? . #t)))
    (license license:bsd-3)))

(define-public rust-futures-0.1
  (package
    (name "rust-futures")
    (version "0.1.28")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0saq8ffjw1pwf1pzhw3kq1z7dfq6wpd8x93dnni6vbkc799kkp25"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-lang-nursery/futures-rs")
    (synopsis "Implementation of zero-cost futures in Rust")
    (description "An implementation of @code{futures} and @code{streams}
featuring zero allocations, composability, and iterator-like interfaces.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-futures-channel-preview-0.3
  (package
    (name "rust-futures-channel-preview")
    (version "0.3.0-alpha.17")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "futures-channel-preview" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1blgpikhw391lzrfqcgg4xsn5xc0dlybni77ka7f0vb08zaixir1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-futures-core-preview" ,rust-futures-core-preview-0.3))))
    (home-page "https://rust-lang.github.io/futures-rs/")
    (synopsis
     "Channels for asynchronous communication using futures-rs")
    (description
     "Channels for asynchronous communication using futures-rs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-core-preview-0.3
  (package
    (name "rust-futures-core-preview")
    (version "0.3.0-alpha.17")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-core-preview" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1xaq8m609k6cz8xydwhwp8xxyxigabcw1w9ngycfy0bnkg7iq52b"))))
    (build-system cargo-build-system)
    (home-page "https://rust-lang-nursery.github.io/futures-rs/")
    (synopsis "Core traits and types in for the @code{futures} library.")
    (description "This crate provides the core traits and types in for the
@code{futures} library.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-futures-cpupool-0.1
  (package
    (name "rust-futures-cpupool")
    (version "0.1.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-cpupool" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1r32456gpblzfvnkf60545v8acqk7gh5zhyhi1jn669k9gicv45b"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-lang-nursery/futures-rs")
    (synopsis "Implementation of thread pools which hand out futures")
    (description
     "An implementation of thread pools which hand out futures to the results of
the computation on the threads themselves.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-futures-executor-preview-0.3
  (package
    (name "rust-futures-executor-preview")
    (version "0.3.0-alpha.17")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "futures-executor-preview" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "053g5kf2qa1xhdkwp3d1grrizzy4683mpbb3y0vvm00hwl7jdfl7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-futures-channel-preview" ,rust-futures-channel-preview-0.3)
        ("rust-futures-core-preview" ,rust-futures-core-preview-0.3)
        ("rust-futures-util-preview" ,rust-futures-util-preview-0.3)
        ("rust-num-cpus" ,rust-num-cpus-1.10)
        ("rust-pin-utils" ,rust-pin-utils-0.1))))
    (home-page "https://github.com/rust-lang/futures-rs")
    (synopsis
     "Executors for asynchronous tasks based on futures-rs")
    (description
     "Executors for asynchronous tasks based on the futures-rs
library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-io-preview-0.3
  (package
    (name "rust-futures-io-preview")
    (version "0.3.0-alpha.17")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-io-preview" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0fhvwhdb8ywjjbfng0ra1r8yyc9yzpyxg9sv3spb3f7w0lk40bh8"))))
    (build-system cargo-build-system)
    (home-page "https://rust-lang-nursery.github.io/futures-rs/")
    (synopsis "Async read and write traits for the futures library")
    (description "This crate provides the @code{AsyncRead} and
@code{AsyncWrite} traits for the @code{futures-rs} library.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-futures-select-macro-preview-0.3
  (package
    (name "rust-futures-select-macro-preview")
    (version "0.3.0-alpha.17")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "futures-select-macro-preview" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1a90ivjzkgz7msiz5si05xzi8xwsk5gar1gkrbmrgqpgkliqd7a6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-proc-macro-hack" ,rust-proc-macro-hack-0.5)
        ("rust-proc-macro2" ,rust-proc-macro2-0.4)
        ("rust-quote" ,rust-quote-1.0)
        ("rust-syn" ,rust-syn-0.15))))
    (home-page "https://github.com/rust-lang/futures-rs")
    (synopsis
     "Handle the first Future to complete")
    (description
     "The @code{select!} macro for waiting on multiple different
@code{Future}s at once and handling the first one to complete.")
    (license (list license:expat license:asl2.0))))

(define-public rust-futures-sink-preview-0.3
  (package
    (name "rust-futures-sink-preview")
    (version "0.3.0-alpha.17")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "futures-sink-preview" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1r4d0gy73hdxkh5g1lrhl1kjnwp6mywjgcj70v0z78b921da42a3"))))
    (build-system cargo-build-system)
    (home-page "https://rust-lang-nursery.github.io/futures-rs/")
    (synopsis  "Asynchronous `Sink` trait for the futures-rs library")
    (description
     "This package provides the asynchronous @code{Sink} trait for the
futures-rs library.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-futures-util-preview-0.3
  (package
    (name "rust-futures-util-preview")
    (version "0.3.0-alpha.17")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "futures-util-preview" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0kizm86wgr5qldyavskfi0r1msg6m4x2pkj0d4r04br2ig29i0dg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-futures" ,rust-futures-0.1)
        ("rust-futures-channel-preview" ,rust-futures-channel-preview-0.3)
        ("rust-futures-core-preview" ,rust-futures-core-preview-0.3)
        ("rust-futures-io-preview" ,rust-futures-io-preview-0.3)
        ("rust-futures-select-macro-preview"
         ,rust-futures-select-macro-preview-0.3)
        ("rust-futures-sink-preview" ,rust-futures-sink-preview-0.3)
        ("rust-memchr" ,rust-memchr-2.2)
        ("rust-pin-utils" ,rust-pin-utils-0.1)
        ("rust-proc-macro-hack" ,rust-proc-macro-hack-0.5)
        ("rust-proc-macro-nested" ,rust-proc-macro-nested-0.1)
        ("rust-rand" ,rust-rand-0.4)
        ("rust-rand-core" ,rust-rand-core-0.5)
        ("rust-slab" ,rust-slab-0.4)
        ("rust-tokio-io" ,rust-tokio-io-0.1))))
    (home-page "https://github.com/rust-lang/futures-rs")
    (synopsis
     "Utilities and extension traits for futures-rs library")
    (description
     "Common utilities and extension traits for the futures-rs
library.")
    (license (list license:expat license:asl2.0))))

(define-public rust-gcc-0.3
  (package
    (inherit rust-cc-1.0)
    (name "rust-gcc")
    (version "0.3.55")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "gcc" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1hng1sajn4r67hndvhjysswz8niayjwvcj42zphpxzhbz89kjpwg"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/alexcrichton/cc-rs")
    (synopsis "Library to compile C/C++ code into a Rust library/application")
    (description
     "This package provides a build-time dependency for Cargo build scripts to
assist in invoking the native C compiler to compile native C code into a static
archive to be linked into Rustcode.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-getopts-0.2
  (package
    (name "rust-getopts")
    (version "0.2.17")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "getopts" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "018yhq97zgcrcxwhj3pxh31h83704sgaiijdnpl0r1ir366c005r"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-lang-nursery/getopts")
    (synopsis "Rust library for option parsing for CLI utilities")
    (description "This library provides getopts-like option parsing.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-glob-0.3
  (package
    (name "rust-glob")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "glob" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0x25wfr7vg3mzxc9x05dcphvd3nwlcmbnxrvwcvrrdwplcrrk4cv"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-lang-nursery/glob")
    (synopsis "Match file paths against Unix shell style patterns")
    (description
     "This package provides support for matching file paths against Unix
shell style patterns.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-glob-0.2
  (package
    (inherit rust-glob-0.3)
    (name "rust-glob")
    (version "0.2.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "glob" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1ysvi72slkw784fcsymgj4308c3y03gwjjzqxp80xdjnkbh8vqcb"))))))

(define-public rust-globset-0.4
  (package
    (name "rust-globset")
    (version "0.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "globset" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1wnqxq91liknmr2w93wjq2spyxbrd1pmnhd4nbi3921dr35a4nlj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-aho-corasick" ,rust-aho-corasick-0.7)
        ("rust-bstr" ,rust-bstr-0.2)
        ("rust-fnv" ,rust-fnv-1.0)
        ("rust-log" ,rust-log-0.4)
        ("rust-regex" ,rust-regex-1.1))
       #:cargo-development-inputs
       (("rust-glob" ,rust-glob-0.3))))
    (home-page
     "https://github.com/BurntSushi/ripgrep/tree/master/globset")
    (synopsis
     "Cross platform single glob and glob set matching")
    (description
     "Cross platform single glob and glob set matching.  Glob set matching is
the process of matching one or more glob patterns against a single candidate
path simultaneously, and returning all of the globs that matched.")
    (license (list license:expat license:unlicense))))

(define-public rust-grep-cli-0.1
  (package
    (name "rust-grep-cli")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "grep-cli" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "05a502x5m4fijwx7zj9icxna2dx86scm76ap80zr89pnvpbfk1hp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-atty" ,rust-atty-0.2)
        ("rust-bstr" ,rust-bstr-0.2)
        ("rust-globset" ,rust-globset-0.4)
        ("rust-lazy-static" ,rust-lazy-static-1.3)
        ("rust-log" ,rust-log-0.4)
        ("rust-regex" ,rust-regex-1.1)
        ("rust-same-file" ,rust-same-file-1.0)
        ("rust-termcolor" ,rust-termcolor-1.0)
        ("rust-winapi-util" ,rust-winapi-util-0.1))))
    (home-page
     "https://github.com/BurntSushi/ripgrep")
    (synopsis
     "Utilities for search oriented command line applications")
    (description
     "Utilities for search oriented command line applications.")
    (license license:expat)))

(define-public rust-grep-matcher-0.1
  (package
    (name "rust-grep-matcher")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "grep-matcher" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "03j26zygfgwyam66bl5g922gimrvp4yyzl8qvaykyklnf247bl3r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-memchr" ,rust-memchr-2.2))
       #:cargo-development-inputs
       (("rust-regex" ,rust-regex-1.1))))
    (home-page "https://github.com/BurntSushi/ripgrep")
    (synopsis "Trait for regular expressions")
    (description
     "This crate provides a low level interface for describing regular
expression matchers.  The @code{grep} crate uses this interface in order to make
the regex engine it uses pluggable.")
    (license (list license:expat license:unlicense))))

(define-public rust-grep-pcre2-0.1
  (package
    (name "rust-grep-pcre2")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "grep-pcre2" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1wjc3gsan20gapga8nji6jcrmwn9n85q5zf2yfq6g50c7abkc2ql"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-grep-matcher" ,rust-grep-matcher-0.1)
        ("rust-pcre2" ,rust-pcre2-0.2))))
    (home-page
     "https://github.com/BurntSushi/ripgrep")
    (synopsis "Use PCRE2 with the grep crate")
    (description "Use PCRE2 with the grep crate.")
    (license (list license:expat license:unlicense))))

(define-public rust-heapsize-0.4
  (package
    (name "rust-heapsize")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "heapsize" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0q94q9ppqjgrw71swiyia4hgby2cz6dldp7ij57nkvhd6zmfcy8n"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/servo/heapsize")
    (synopsis "Measure the total runtime size of an object on the heap")
    (description
     "Infrastructure for measuring the total runtime size of an object on the
heap.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-heapsize-0.3
  (package
    (inherit rust-heapsize-0.4)
    (name "rust-heapsize")
    (version "0.3.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "heapsize" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0dmwc37vgsdjzk10443dj4f23439i9gch28jcwzmry3chrwx8v2m"))))))

;; This package makes use of removed features
(define-public rust-heapsize-plugin-0.1
  (package
    (name "rust-heapsize-plugin")
    (version "0.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "heapsize_plugin" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1i72isf699q9jl167g2kg4xd6h3cd05rc79zaph58aqjy0g0m9y9"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/servo/heapsize")
    (synopsis "Measure runtime size of an object on the heap")
    (description
     "This package automatically generates infrastructure for measuring the
total runtime size of an object on the heap")
    (properties `((hidden? . #t)))
    (license license:mpl2.0)))

(define-public rust-hex-0.3
  (package
    (name "rust-hex")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hex" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0xsdcjiik5j750j67zk42qdnmm4ahirk3gmkmcqgq7qls2jjcl40"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/KokaKiwi/rust-hex")
    (synopsis "Encode and decode data to/from hexadecimals")
    (description "This crate allows for encoding and decoding data into/from
hexadecimal representation.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-humantime-1.2
  (package
    (name "rust-humantime")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "humantime" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "057ilhy6vc9iqhhby5ymh45m051pgxwq2z437gwkbnqhw7rfb9rw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-quick-error" ,rust-quick-error-1.2))
       #:cargo-development-inputs
       (("rust-chrono" ,rust-chrono-0.4)
        ("rust-rand" ,rust-rand-0.4)
        ("rust-time" ,rust-time-0.1))))
    (home-page
     "https://github.com/tailhook/humantime")
    (synopsis
     "Parser and formatter for Duration and SystemTime")
    (description
     "A parser and formatter for @code{std::time::{Duration,
SystemTime}}.")
    (license (list license:expat license:asl2.0))))

(define-public rust-hostname-0.1
  (package
    (name "rust-hostname")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hostname" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0kprf862qaa7lwdms6aw7f3275h0j2rwhs9nz5784pm8hdmb9ki1"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/fengcen/hostname")
    (synopsis "Get hostname for Rust")
    (description
     "Get hostname for Rust.")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-indexmap-1.0
  (package
    (name "rust-indexmap")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "indexmap" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "13f5k1kl2759y4xfy0vhays35fmrkmhqngbr2ny8smvrbz0ag0by"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-serde" ,rust-serde-1.0))
       #:cargo-development-inputs
       (("rust-fnv" ,rust-fnv-1.0)
        ("rust-itertools" ,rust-itertools-0.8)
        ("rust-lazy-static" ,rust-lazy-static-1.3)
        ("rust-quickcheck" ,rust-quickcheck-0.8)
        ("rust-rand" ,rust-rand-0.4)
        ("rust-serde-test" ,rust-serde-test-1.0))))
    (home-page "https://github.com/bluss/indexmap")
    (synopsis
     "Hash table with consistent order and fast iteration")
    (description
     "This package provides a hash table with consistent order and fast iteration.

The indexmap is a hash table where the iteration order of the
key-value pairs is independent of the hash values of the keys.  It has
the usual hash table functionality, it preserves insertion order
except after removals, and it allows lookup of its elements by either
hash table key or numerical index.  A corresponding hash set type is
also provided.

This crate was initially published under the name ordermap, but it was
renamed to indexmap.")
    (license (list license:expat license:asl2.0))))

(define-public rust-insta-0.8
  (package
    (name "rust-insta")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "insta" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "17rvqw9xm61prncbqi3cplphr3l2dl85sljdpyr3fz2mqjgbdfwb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-chrono" ,rust-chrono-0.4)
        ("rust-ci-info" ,rust-ci-info-0.3)
        ("rust-console" ,rust-console-0.7)
        ("rust-difference" ,rust-difference-2.0)
        ("rust-failure" ,rust-failure-0.1)
        ("rust-lazy-static" ,rust-lazy-static-1.3)
        ("rust-pest" ,rust-pest-2.1)
        ("rust-pest-derive" ,rust-pest-derive-2.1)
        ("rust-ron" ,rust-ron-0.4)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-serde-json" ,rust-serde-json-1.0)
        ("rust-serde-yaml" ,rust-serde-yaml-0.8)
        ("rust-uuid" ,rust-uuid-0.7))))
    (home-page "https://github.com/mitsuhiko/insta")
    (synopsis "Snapshot testing library for Rust")
    (description
     "This package provides a snapshot testing library for Rust.")
    (license license:asl2.0)))

(define-public rust-iovec-0.1
  (package
    (name "rust-iovec")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "iovec" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "025vi072m22299z3fg73qid188z2iip7k41ba6v5v5yhwwby9rnv"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/carllerche/iovec")
    (synopsis "Portable buffer type for scatter/gather I/O operations")
    (description
     "Portable buffer type for scatter/gather I/O operations.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-itertools-0.8
  (package
    (name "rust-itertools")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "itertools" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0n2k13b6w4x2x6np2lykh9bj3b3z4hwh2r4cn3z2dgnfq7cng12v"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-either" ,rust-either-1.5))
       #:cargo-development-inputs
       (("rust-permutohedron" ,rust-permutohedron-0.2)
        ("rust-quickcheck" ,rust-quickcheck-0.8)
        ("rust-rand" ,rust-rand-0.4))))
    (home-page
     "https://github.com/rust-itertools/itertools")
    (synopsis
     "Extra iterator adaptors, iterator methods, free functions, and macros")
    (description
     "Extra iterator adaptors, iterator methods, free functions, and macros.")
    (license (list license:expat license:asl2.0))))

(define-public rust-itertools-num-0.1
  (package
    (name "rust-itertools-num")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "itertools-num" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1rr7ig9nkpampcas23s91x7yac6qdnwssq3nap522xbgkqps4wm8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-num-traits" ,rust-num-traits-0.2))
       #:cargo-development-inputs
       (("rust-itertools" ,rust-itertools-0.8)
        ("rust-quickcheck" ,rust-quickcheck-0.8))))
    (home-page
     "https://github.com/bluss/itertools-num")
    (synopsis
     "Numerical iterator tools")
    (description
     "Numerical iterator tools.  Extra iterators and iterator methods
and functions.")
    (license (list license:expat license:asl2.0))))

(define-public rust-itoa-0.4
  (package
    (name "rust-itoa")
    (version "0.4.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "itoa" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0zvg2d9qv3avhf3d8ggglh6fdyw8kkwqg3r4622ly5yhxnvnc4jh"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/dtolnay/itoa")
    (synopsis "Fast functions for printing integer primitives")
    (description "This crate provides fast functions for printing integer
primitives to an @code{io::Write}.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-itoa-0.1
 (package
   (inherit rust-itoa-0.4)
   (name "rust-itoa")
   (version "0.1.1")
   (source
     (origin
       (method url-fetch)
       (uri (crate-uri "itoa" version))
       (file-name (string-append name "-" version ".crate"))
       (sha256
        (base32
         "18g7p2hrb3dk84z3frfgmszfc9hjb4ps9vp99qlb1kmf9gm8hc5f"))))))

(define-public rust-jemalloc-sys-0.3
  (package
    (name "rust-jemalloc-sys")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "jemalloc-sys" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0ify9vlql01qhfxlj7d4p9jvcp90mj2h69nkbq7slccvbhzryfqd"))))
    (build-system cargo-build-system)
    ;(arguments
    ; `(#:phases
    ;   (modify-phases %standard-phases
    ;     (add-after 'unpack 'override-jemalloc
    ;       (lambda* (#:key inputs #:allow-other-keys)
    ;         (let ((jemalloc (assoc-ref inputs "jemalloc")))
    ;           (delete-file-recursively "jemalloc")
    ;           (setenv "JEMALLOC_OVERRIDE"
    ;                   (string-append jemalloc "/lib/libjemalloc_pic.a")))
    ;         #t)))))
    ;(inputs
    ; `(("jemalloc" ,jemalloc)))
    (home-page "https://github.com/gnzlbg/jemallocator")
    (synopsis "Rust FFI bindings to jemalloc")
    (description "This package provides Rust FFI bindings to jemalloc.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-json-0.11
  (package
    (name "rust-json")
    (version "0.11.14")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "json" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1hj8c6xj5c2aqqszi8naaflmcdbya1i9byyjrq4iybxjb4q91mq1"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/maciejhirsz/json-rust")
    (synopsis "JSON implementation in Rust")
    (description "This crate provides a JSON implementation in Rust, reducing
friction with idiomatic Rust structs to ease interopability.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-kernel32-sys-0.2
  (package
    (name "rust-kernel32-sys")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "kernel32-sys" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1389av0601a9yz8dvx5zha9vmkd6ik7ax0idpb032d28555n41vm"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/retep998/winapi-rs")
    (synopsis "Function definitions for the Windows API library kernel32")
    (description "Contains function definitions for the Windows API library
kernel32.")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-language-tags-0.2
  (package
    (name "rust-language-tags")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "language-tags" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "16hrjdpa827carq5x4b8zhas24d8kg4s16m6nmmn1kb7cr5qh7d9"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/pyfisch/rust-language-tags")
    (synopsis "Language tags for Rust")
    (description
     "Language tags can be used identify human languages, scripts e.g. Latin
script, countries and other regions.  They are commonly used in HTML and HTTP
@code{Content-Language} and @code{Accept-Language} header fields.  This package
currently supports parsing (fully conformant parser), formatting and comparing
language tags.")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-lazy-static-1.3
  (package
    (name "rust-lazy-static")
    (version "1.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lazy_static" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "052ac27w189hrf1j3hz7sga46rp84zl2hqnzyihxv78mgzr2jmxw"))))
    (build-system cargo-build-system)
    (home-page  "https://github.com/rust-lang-nursery/lazy-static.rs")
    (synopsis "Macro for declaring lazily evaluated statics in Rust")
    (description
     "This package provides a macro for declaring lazily evaluated statics in
Rust.  Using this macro, it is possible to have @code{static}s that require code
to be executed at runtime in order to be initialized.  This includes anything
requiring heap allocations, like vectors or hash maps, as well as anything that
requires non-const function calls to be computed.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-libc-0.2
  (package
    (name "rust-libc")
    (version "0.2.65")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libc" version))
       (file-name (string-append name "-" version ".crate"))
       (sha256
        (base32
         "1s14bjxnz6haw0gr1h3j4sr7s2s407hpgm8dxhwnl7yzgxia0c8s"))))
    (build-system cargo-build-system)
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
    (properties '((hidden? . #t)))
    (license (list license:expat
                   license:asl2.0))))

(define-public rust-libgit2-sys-0.8
  (package
    (name "rust-libgit2-sys")
    (version "0.8.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libgit2-sys" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0y2mibmx7wy91s2kmb2gfb29mrqlqaxpy5wcwr8s1lwws7b9w5sc")) ))
    (build-system cargo-build-system)
    ;(arguments
    ; `(#:phases
    ;  (modify-phases %standard-phases
    ;    (add-after 'unpack 'find-openssl
    ;      (lambda* (#:key inputs #:allow-other-keys)
    ;        (let ((openssl (assoc-ref inputs "openssl")))
    ;          (setenv "OPENSSL_DIR" openssl))
    ;        (delete-file-recursively "libgit2")
    ;        (setenv "LIBGIT2_SYS_USE_PKG_CONFIG" "1")
    ;        (setenv "LIBSSH2_SYS_USE_PKG_CONFIG" "1")
    ;        #t)))))
    ;(native-inputs
    ; `(("pkg-config" ,pkg-config)))
    ;(inputs
    ; `(("libgit2" ,libgit2)
    ;   ("openssl" ,openssl)
    ;   ("zlib" ,zlib)))
    (home-page "https://github.com/rust-lang/git2-rs")
    (synopsis "Native bindings to the libgit2 library")
    (description
     "This package provides native rust bindings to the @code{libgit2} library.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-libgit2-sys-0.7
  (package
    (inherit rust-libgit2-sys-0.8)
    (name "rust-libgit2-sys")
    (version "0.7.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libgit2-sys" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1wcvg2qqra2aviasvqcscl8gb2rnjnd6h998wy5dlmf2bnriqi28"))))
    (build-system cargo-build-system)))

(define-public rust-libloading-0.5
  (package
    (name "rust-libloading")
    (version "0.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libloading" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0lyply8rcqc8agajzxs7bq6ivba9dnn1i68kgb9z2flnfjh13cgj"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/nagisa/rust_libloading/")
    (synopsis "Rust library for loading dynamic libraries")
    (description
     "A memory-safer wrapper around system dynamic library loading primitives.
The most important safety guarantee by this library is prevention of
dangling-Symbols that may occur after a Library is unloaded.  Using this library
allows loading dynamic libraries (also known as shared libraries) as well as use
functions and static variables these libraries contain.")
    (properties '((hidden? . #t)))
    (license license:isc)))

(define-public rust-libssh2-sys-0.2
  (package
    (name "rust-libssh2-sys")
    (version "0.2.12")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libssh2-sys" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1zb6gsw795nq848nk5x2smzpfnn1s15wjlzjnvr8ihlz2l5x2549"))))
    (build-system cargo-build-system)
    ;(arguments
    ; `(#:phases
    ;  (modify-phases %standard-phases
    ;    (add-after 'unpack 'find-openssl
    ;      (lambda* (#:key inputs #:allow-other-keys)
    ;        (let ((openssl (assoc-ref inputs "openssl")))
    ;          (setenv "OPENSSL_DIR" openssl))
    ;        (delete-file-recursively "libssh2")
    ;        (setenv "LIBSSH2_SYS_USE_PKG_CONFIG" "1")
    ;        #t)))))
    ;(native-inputs
    ; `(("pkg-config" ,pkg-config)))
    ;(inputs
    ; `(("libssh2" ,libssh2)
    ;   ("openssl" ,openssl)
    ;   ("zlib" ,zlib)))
    (home-page "https://github.com/alexcrichton/ssh2-rs")
    (synopsis "Native bindings to the libssh2 library")
    (description
     "This package provides native rust bindings to the @code{libssh2} library.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-lock-api-0.2
  (package
    (name "rust-lock-api")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lock_api" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1zx7pksmgyggpczgw4qrr4vj2nkdk5lipgiysvr20slm552nv57d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-owning-ref" ,rust-owning-ref-0.4)
        ("rust-scopeguard" ,rust-scopeguard-1.0)
        ("rust-serde" ,rust-serde-1.0))))
    (home-page "https://github.com/Amanieu/parking_lot")
    (synopsis
     "Wrappers to create fully-featured Mutex and RwLock types")
    (description
     "This package provides wrappers to create fully-featured @code{Mutex} and
@code{RwLock} types.  It is compatible with @code{no_std}.")
    (license (list license:expat license:asl2.0))))

(define-public rust-lock-api-0.1
  (package
    (inherit rust-lock-api-0.2)
    (name "rust-lock-api")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lock_api" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0b24q9mh258xa52ap636q1sxz0j5vrnp0hwbbh7ddjka3wwz3sv2"))))
    (properties '((hidden? . #t)))))

(define-public rust-log-0.4
  (package
    (name "rust-log")
    (version "0.4.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "log" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1xz18ixccl5c6np4linv3ypc7hpmmgpc5zzd2ymp2ssfx0mhbdhl"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-lang/log")
    (synopsis "Lightweight logging facade for Rust")
    (description
     "This package provides a lightweight logging facade for Rust.")
    (properties '((hidden? . #t)))
    (license (list license:expat license:asl2.0))))

(define-public rust-log-0.3
  (package
    (inherit rust-log-0.4)
    (name "rust-log")
    (version "0.3.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "log" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0nsn28syc49vvjhxcfc8261rd1frhjc0r4bn9v3mqvps3ra7f3w8"))))))

(define-public rust-lzma-sys-0.1
  (package
    (name "rust-lzma-sys")
    (version "0.1.15")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lzma-sys" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "14gyj256yh0wm77jbvmlc39v7lfn0navpfrja4alczarzlc8ir2k"))))
    (build-system cargo-build-system)
    ;(arguments
    ; `(#:phases
    ;   (modify-phases %standard-phases
    ;     (add-after 'unpack 'unbundle-xz
    ;       (lambda* (#:key inputs #:allow-other-keys)
    ;         (let ((xz (assoc-ref inputs "xz")))
    ;           (delete-file-recursively "xz-5.2"))
    ;         #t)))))
    ;(inputs
    ; `(("pkg-config" ,pkg-config)
    ;   ("xz" ,xz)))
    (home-page "https://github.com/alexcrichton/xz2-rs")
    (synopsis "Bindings to liblzma for lzma and xz stream encoding/decoding")
    (description
     "This package contains the raw bindings to liblzma which contains an
implementation of LZMA and xz stream encoding/decoding.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-maplit-1.0
  (package
    (name "rust-maplit")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "maplit" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0hsczmvd6zkqgzqdjp5hfyg7f339n68w83n4pxvnsszrzssbdjq8"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bluss/maplit")
    (synopsis "Collection of Map macros")
    (description "This crate provides a collection of @code{literal} macros for
@code{HashMap}, @code{HashSet}, @code{BTreeMap}, and @code{BTreeSet.}")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-matches-0.1
  (package
    (name "rust-matches")
    (version "0.1.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "matches" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "020axl4q7rk9vz90phs7f8jas4imxal9y9kxl4z4v7a6719mrz3z"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/SimonSapin/rust-std-candidates")
    (synopsis "Macro to evaluate whether an expression matches a pattern.")
    (description "This package provides a macro to evaluate, as a boolean,
whether an expression matches a pattern.")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-md5-0.6
  (package
    (name "rust-md5")
    (version "0.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "md5" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "17b2xm4h4cvxsdjsf3kdrzqv2za60kak961xzi5kmw6g6djcssvy"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/stainless-steel/md5")
    (synopsis "MD5 hash function in Rust")
    (description "The package provides the MD5 hash function.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-memchr-2.2
  (package
    (name "rust-memchr")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "memchr" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0f8wdra7yaggsr4jzlrvpd8yknnqhd990iijdr6llgc8gk2ppz1f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-libc" ,rust-libc-0.2))
       #:cargo-development-inputs
       (("rust-quickcheck" ,rust-quickcheck-0.8))))
    (home-page
     "https://github.com/BurntSushi/rust-memchr")
    (synopsis "Safe interface to memchr")
    (description "The @code{memchr} crate provides heavily optimized routines
for searching bytes.")
    (license (list license:expat license:unlicense))))

(define-public rust-memchr-1.0
  (package
    (inherit rust-memchr-2.2)
    (name "rust-memchr")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "memchr" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0yjyja34pzhipdl855q3m21w1lyih4lw79x2dp3czwdla4pap3ql"))))))

(define-public rust-memmap-0.7
  (package
    (name "rust-memmap")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "memmap" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0ns7kkd1h4pijdkwfvw4qlbbmqmlmzwlq3g2676dcl5vwyazv1b5"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/danburkert/memmap-rs")
    (synopsis "Rust library for cross-platform memory mapped IO")
    (description
     "This package provides a cross-platform Rust API for memory-mapped
file IO.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-memmap-0.6
  (package
    (inherit rust-memmap-0.7)
    (name "rust-memmap")
    (version "0.6.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "memmap" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1zy6s0ni0lx9rjzq3gq2zz9r8zgjmbp02332g3gsj4fyhv4s5zz2"))))))

(define-public rust-memoffset-0.2
  (package
    (name "rust-memoffset")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "memoffset" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1cvm2z7dy138s302ii7wlzcxbka5a8yfl5pl5di7lbdnw9hw578g"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/Gilnaa/memoffset")
    (synopsis
     "offset_of functionality for Rust structs")
    (description
     "@code{offset_of} functionality for Rust structs.")
    (license license:expat)))

(define-public rust-mime-0.3
  (package
    (name "rust-mime")
    (version "0.3.13")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "mime" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "09clbyvdkwflp8anwjhqdib0sw8191gphcchdp80nc8ayhhwl9ry"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/hyperium/mime")
    (synopsis "Strongly Typed Mimes")
    (description
     "Support MIME (HTTP Media Types) as strong types in Rust.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-miniz-oxide-0.3
  (package
    (name "rust-miniz-oxide")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "miniz_oxide" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32 "1bmanbbcdmssfbgik3fs323g7vljc5wkjz7s61jsbbz2kg0nckrh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-adler32" ,rust-adler32-1.0))))
    (home-page  "https://github.com/Frommi/miniz_oxide/tree/master/miniz_oxide")
    (synopsis "Pure rust replacement for the miniz DEFLATE/zlib encoder/decoder")
    (description
     "A pure rust replacement for the miniz DEFLATE/zlib encoder/decoder.  Using
@code{flate2} with the @code{rust_backend} feature provides an easy to use
streaming API for miniz_oxide.")
    (license license:expat)))

(define-public rust-miniz-oxide-0.2
  (package
    (inherit rust-miniz-oxide-0.3)
    (name "rust-miniz-oxide")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "miniz_oxide" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "17f92krv9hhsyc38prpfyn99m2hqhr4fgszpsla66a6gcrnpbhxn"))))))

(define-public rust-miniz-oxide-c-api-0.2
  (package
    (name "rust-miniz-oxide-c-api")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "miniz_oxide_c_api" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1514mvlj8vl723xqxnww5cfqr2mhnqqqf18fn3df17yx8racly2v"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-crc32fast" ,rust-crc32fast-1.2)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-miniz-oxide" ,rust-miniz-oxide-0.2))
       #:cargo-development-inputs
       (("rust-cc" ,rust-cc-1.0))))
    (home-page "https://github.com/Frommi/miniz_oxide/")
    (synopsis "DEFLATE compression and decompression API")
    (description
     "DEFLATE compression and decompression API designed to be Rust
drop-in replacement for miniz.")
    (license license:expat)))

(define-public rust-miniz-sys-0.1
  (package
    (name "rust-miniz-sys")
    (version "0.1.12")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "miniz-sys" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "00l2r4anm8g35x0js2zfdnwfbrih9m43vphdpb77c5ga3kjkm7hy"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/alexcrichton/flate2-rs")
    (synopsis "Bindings to the miniz.c library")
    (description
     "This package provides bindings to the @code{miniz.c} library.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-mio-0.6
  (package
    (name "rust-mio")
    (version "0.6.19")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mio" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "08zzs227vrnyz5kvws6awzlgzb8zqpnihs71hkqlw07dlfb1kxc3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-fuchsia-zircon" ,rust-fuchsia-zircon-0.3)
        ("rust-fuchsia-zircon-sys" ,rust-fuchsia-zircon-sys-0.3)
        ("rust-iovec" ,rust-iovec-0.1)
        ("rust-kernel32-sys" ,rust-kernel32-sys-0.2)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-miow" ,rust-miow-0.2)
        ("rust-net2" ,rust-net2-0.2)
        ("rust-slab" ,rust-slab-0.4)
        ("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs
       (("rust-bytes" ,rust-bytes-0.4)
        ("rust-env-logger" ,rust-env-logger-0.6)
        ("rust-tempdir" ,rust-tempdir-0.3))))
    (home-page "https://github.com/tokio-rs/mio")
    (synopsis "Lightweight non-blocking IO")
    (description "Lightweight non-blocking IO.")
    (license license:expat)))

(define-public rust-miow-0.3
  (package
    (name "rust-miow")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "miow" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "09ljvx6wg30f2xlv7b7hhpkw7k312n3hjgmrbhwzhz9x03ra0sir"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/alexcrichton/miow")
    (synopsis "Rust I/O library for Windows")
    (description
     "This package provides a zero overhead I/O library for Windows, focusing on
IOCP and Async I/O abstractions.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-miow-0.2
  (package
    (inherit rust-miow-0.3)
    (name "rust-miow")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "miow" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "06g9b8sqlh5gxakwqq4rrib07afwanfnxgxajrldwcgk3hxjy7wc"))))))

(define-public rust-modifier-0.1
  (package
    (name "rust-modifier")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "modifier" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0n3fmgli1nsskl0whrfzm1gk0rmwwl6pw1q4nb9sqqmn5h8wkxa1"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/reem/rust-modifier")
    (synopsis
      "Chaining APIs for both self -> Self and &mut self methods.")
    (description
      "Chaining APIs for both self -> Self and &mut self methods.")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-net2-0.2
  (package
    (name "rust-net2")
    (version "0.2.33")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "net2" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "126g3fgfxp06zimc1l9iyxnn9cif1hjsg7sd81nlls5nnyghsma2"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-lang-nursery/net2-rs")
    (synopsis "Extensions to the standard library's networking types")
    (description
     "This library contains extensions to the standard library's networking
types as proposed in RFC 1158.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-netlib-src-0.7
  (package
    (name "rust-netlib-src")
    (version "0.7.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "netlib-src" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "112hwfw1zzdj10h3j213xxqjrq38iygb3nb3ijay65ycmrg819s4"))))
    (build-system cargo-build-system)
    ;(inputs
    ; `(("gfortran:lib" ,gfortran "lib")
    ;   ("lapack" ,lapack)))
    (home-page "https://github.com/blas-lapack-rs/netlib-src")
    (synopsis "Source of BLAS and LAPACK via Netlib")
    (description
     "The package provides a source of BLAS and LAPACK via Netlib.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-libnghttp2-sys-0.1
  (package
    (name "rust-libnghttp2-sys")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libnghttp2-sys" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0qr4lyh7righx9n22c7amlcpk906rn1jnb2zd6gdfpa3yi24s982"))))
    (build-system cargo-build-system)
    ;(inputs
    ; `(("nghttp2" ,nghttp2)))
    (home-page "https://github.com/alexcrichton/nghttp2-rs")
    (synopsis "FFI bindings for libnghttp2 (nghttp2)")
    (description
     "This package provides FFI bindings for libnghttp2 (nghttp2).")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-libz-sys-1.0
  (package
    (name "rust-libz-sys")
    (version "1.0.25")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "libz-sys" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1gjycyl2283525abks98bhxa4r259m617xfm5z52p3p3c8ry9d9f"))))
    (build-system cargo-build-system)
    ;(arguments
    ; `(#:phases
    ;   (modify-phases %standard-phases
    ;     (add-after 'unpack 'delete-vendored-zlib
    ;       (lambda _
    ;         (delete-file-recursively "src/zlib")
    ;         #t)))))
    ;(inputs
    ; `(("pkg-config" ,pkg-config)
    ;   ("zlib" ,zlib)))
    (home-page "https://github.com/rust-lang/libz-sys")
    (synopsis "Bindings to the system libz library")
    (description
     "This package provides bindings to the system @code{libz} library (also
known as zlib).")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-nodrop-0.1
  (package
    (name "rust-nodrop")
    (version "0.1.13")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "nodrop" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0if9ifn6rvar5jirx4b3qh4sl5kjkmcifycvzhxa9j3crkfng5ig"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bluss/arrayvec")
    (synopsis "Wrapper type to inhibit drop (destructor)")
    (description "This package provides a wrapper type to inhibit drop
(destructor).  Use @code{std::mem::ManuallyDrop} instead!")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

;; This package requires features which are unavailable
;; on the stable releases of Rust.
(define-public rust-nodrop-union-0.1
  (package
    (name "rust-nodrop-union")
    (version "0.1.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "nodrop-union" version))
        (file-name (string-append name "-" version ".crate"))
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

(define-public rust-num-complex-0.2
  (package
    (name "rust-num-complex")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "num-complex" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1z6zjdzx1g1hj4y132ddy83d3p3zvw06igbf59npxxrzzcqwzc7w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-rand" ,rust-rand-0.4)
        ("rust-serde" ,rust-serde-1.0))
       #:cargo-development-inputs
       (("rust-autocfg" ,rust-autocfg-0.1))))
    (home-page
     "https://github.com/rust-num/num-complex")
    (synopsis
     "Complex numbers implementation for Rust")
    (description
     "Complex numbers implementation for Rust.")
    (license (list license:expat license:asl2.0))))

(define-public rust-num-cpus-1.10
  (package
    (name "rust-num-cpus")
    (version "1.10.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num_cpus" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0wrj3zvj6h3q26sqj9zxpd59frjb54n7jhjwf307clq31ic47vxw"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/seanmonstar/num_cpus")
    (synopsis "Get the number of CPUs on a machine")
    (description
     "Get the number of CPUs on a machine.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-num-integer-0.1
  (package
    (name "rust-num-integer")
    (version "0.1.41")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "num-integer" version))
       (file-name
        (string-append name "-" version ".crate"))
       (sha256
        (base32
         "02dwjjpfbi16c71fq689s4sw3ih52cvfzr5z5gs6qpr5z0g58pmq"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-num/num-integer")
    (synopsis "Integer traits and functions")
    (description "Integer traits and functions.")
    (properties '((hidden? . #t)))
    ;; Dual licensed.
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-num-iter-0.1
  (package
    (name "rust-num-iter")
    (version "0.1.39")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num-iter" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0bhk2qbr3261r6zvfc58lz4spfqjhvdripxgz5mks5rd85r55gbn"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-num/num-iter")
    (synopsis "External iterators for generic mathematics")
    (description
     "This crate provides external iterators for generic mathematics.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-num-traits-0.2
  (package
    (name "rust-num-traits")
    (version "0.2.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "num-traits" version))
       (file-name
        (string-append name "-" version ".crate"))
       (sha256
        (base32
         "0clvrm34rrqc8p6gq5ps5fcgws3kgq5knh7nlqxf2ayarwks9abb"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-num/num-traits")
    (synopsis "Numeric traits for generic mathematics")
    (description "Numeric traits for generic mathematics.")
    (properties '((hidden? . #t)))
    ;; Dual licensed.
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-num-traits-0.1
  (package
    (inherit rust-num-traits-0.2)
    (name "rust-num-traits")
    (version "0.1.43")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "num-traits" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0c9whknf2dm74a3cqirafy6gj83a76gl56g4v3g19k6lkwz13rcj"))))
    (build-system cargo-build-system)))

(define-public rust-numtoa-0.1
  (package
    (name "rust-numtoa")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "numtoa" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1vs9rhggqbql1p26x8nkha1j06wawwgb2jp5fs88b5gi7prvvy5q"))))
    (build-system cargo-build-system)
    (home-page "https://gitlab.com/mmstick/numtoa")
    (synopsis "Convert numbers into stack-allocated byte arrays")
    (description
     "This package can convert numbers into stack-allocated byte arrays.")
    (properties '((hidden? . #t)))
    (license (list license:expat license:asl2.0))))

(define-public rust-openssl-0.10
  (package
    (name "rust-openssl")
    (version "0.10.26")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "openssl" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "11d505lwlrh5a0jc2l6q36gvsaqic3vizq5q860hiqcqkmwwag1s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-1)
        ("rust-cfg-if" ,rust-cfg-if-0.1)
        ("rust-foreign-types" ,rust-foreign-types-0.3)
        ("rust-lazy-static" ,rust-lazy-static-1.3)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-openssl-sys" ,rust-openssl-sys-0.9))
       #:cargo-development-inputs
       (("rust-hex" ,rust-hex-0.3)
        ("rust-tempdir" ,rust-tempdir-0.3))))
    (home-page "https://github.com/sfackler/rust-openssl")
    (synopsis "OpenSSL bindings")
    (description "OpenSSL bindings.")
    (license license:asl2.0)))

(define-public rust-openssl-probe-0.1
  (package
    (name "rust-openssl-probe")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "openssl-probe" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1pijrdifgsdwd45b08c2g0dsmnhz7c3kmagb70839ngrd7d29bvp"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/alexcrichton/openssl-probe")
    (synopsis "Find SSL certificate locations")
    (description
     "This package provides a tool to find SSL certificate locations on the
system for OpenSSL.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-openssl-src-111
  (package
    (name "rust-openssl-src")
    (version "111.6.0+1.1.1d")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "openssl-src" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "172xh95hp7aygahah1940kg1dnx60c5m80cwj5hgi8x7x0fxmhmr"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/alexcrichton/openssl-src-rs")
    (synopsis "Source of OpenSSL for rust crates")
    (description
     "This package contains the source of OpenSSL and logic to build it.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-openssl-sys-0.9
  (package
    (name "rust-openssl-sys")
    (version "0.9.50")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "openssl-sys" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32 "1dn3capgiz77s6rpmc2sf8gadwkmhwgfd6mw4rcnnm9jp36dqhic"))))
    (build-system cargo-build-system)
    ;(arguments
    ; `(#:phases
    ;   (modify-phases %standard-phases
    ;     (add-after 'unpack 'find-openssl
    ;       (lambda* (#:key inputs #:allow-other-keys)
    ;         (let ((openssl (assoc-ref inputs "openssl")))
    ;           (setenv "OPENSSL_DIR" openssl))
    ;         #t)))))
    ;(inputs
    ; `(("openssl" ,openssl)
    ;   ("pkg-config" ,pkg-config)))
    (home-page "https://github.com/sfackler/rust-openssl")
    (synopsis "FFI bindings to OpenSSL")
    (description
     "This package provides FFI bindings to OpenSSL for use in rust crates.")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-owning-ref-0.4
  (package
    (name "rust-owning-ref")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "owning_ref" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "04zgwy77lin8qz398s6g44467pd6kjhbrlqifkia5rkr47mbi929"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/Kimundi/owning-ref-rs")
    (synopsis "Create references that carry their owner with them")
    (description
     "This package provides a library for creating references that carry their
owner with them.  This can sometimes be useful because Rust borrowing rules
normally prevent moving a type that has been borrowed from.")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-parity-wasm-0.40
  (package
    (name "rust-parity-wasm")
    (version "0.40.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "parity-wasm" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1p84f0k36q05j18jy66n122lyali794cj78hbxgy9wj6si84plqd"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/paritytech/parity-wasm")
    (synopsis "Low-level WebAssembly format library")
    (description
     "This package provides a WebAssembly binary format serialization,
deserialization, and interpreter in Rust.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-pcre2-0.2
  (package
    (name "rust-pcre2")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pcre2" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "103i66a998g1fjrqf9sdyvi8qi83hwglz3pjdcq9n2r207hsagb0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-pcre2-sys" ,rust-pcre2-sys-0.2)
        ("rust-thread-local" ,rust-thread-local-0.3))))
    (home-page "https://github.com/BurntSushi/rust-pcre2")
    (synopsis "High level wrapper library for PCRE2")
    (description
     "This package provides a high level wrapper library for PCRE2.")
    (license (list license:expat license:unlicense))))

(define-public rust-pcre2-sys-0.2
  (package
    (name "rust-pcre2-sys")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pcre2-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0nwdvc43dkb89qmm5q8gw1zyll0wsfqw7kczpn23mljra3874v47"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-pkg-config" ,rust-pkg-config-0.3)
        ("rust-cc" ,rust-cc-1.0))))
    (home-page
     "https://github.com/BurntSushi/rust-pcre2")
    (synopsis "Low level bindings to PCRE2")
    (description "Low level bindings to PCRE2.")
    (license (list license:expat license:unlicense))))

(define-public rust-peeking-take-while-0.1
  (package
    (name "rust-peeking-take-while")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "peeking_take_while" version))
        (file-name (string-append name "-" version ".crate"))
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
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-percent-encoding-2.1
  (package
    (name "rust-percent-encoding")
    (version "2.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "percent-encoding" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0bp3zrsk3kr47fbpipyczidbbx4g54lzxdm77ni1i3qws10mdzfl"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/servo/rust-url/")
    (synopsis "Percent encoding and decoding")
    (description "This crate provides percent encoding and decoding.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-percent-encoding-1.0
  (package
    (inherit rust-percent-encoding-2.1)
    (name "rust-percent-encoding")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "percent-encoding" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0cgq08v1fvr6bs5fvy390cz830lq4fak8havdasdacxcw790s09i"))))))

(define-public rust-permutohedron-0.2
  (package
    (name "rust-permutohedron")
    (version "0.2.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "permutohedron" version))
        (file-name (string-append name "-" version ".crate"))
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
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-pico-sys-0.0
  (package
    (name "rust-pico-sys")
    (version "0.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pico-sys" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1q5pg0ms6szz6b5h26h4k40zb76zbwwjgyigac4wly9qngdj4yl5"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/reem/rust-pico-sys")
    (synopsis "Bindings to the PicoHTTPParser")
    (description
     "This package provides bindings to the PicoHTTPParser.")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-pin-utils-0.1
  (package
    (name "rust-pin-utils")
    (version "0.1.0-alpha.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pin-utils" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "11xmyx00n4m37d546by2rxb8ryxs12v55cc172i3yak1rqccd52q"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-lang-nursery/pin-utils")
    (synopsis "Utilities for pinning")
    (description "This crate provides utilities for pinning values on the stack.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-pkg-config-0.3
  (package
    (name "rust-pkg-config")
    (version "0.3.14")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pkg-config" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "135ia995lqzr0gxpk85h0bjxf82kj6hbxdx924sh9jdln6r8wvk7"))))
    (build-system cargo-build-system)
    ;(inputs
    ; `(("pkg-config" ,pkg-config)))
    (home-page "https://github.com/rust-lang/pkg-config-rs")
    (synopsis "Library to run the pkg-config system tool")
    (description
     "A library to run the pkg-config system tool at build time in order to be
used in Cargo build scripts.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-plain-0.2
  (package
    (name "rust-plain")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "plain" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "19n1xbxb4wa7w891268bzf6cbwq4qvdb86bik1z129qb0xnnnndl"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/randomites/plain")
    (synopsis "Rust library that allows reinterpreting data safely")
    (description "This package provides a small Rust library that allows users
 to reinterpret data of certain types safely.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-plugin-0.2
  (package
    (name "rust-plugin")
    (version "0.2.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "plugin" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1q7nghkpvxxr168y2jnzh3w7qc9vfrby9n7ygy3xpj0bj71hsshs"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/reem/rust-plugin")
    (synopsis "Lazily evaluated, order-independent plugins for extensible types")
    (description
     "Lazily evaluated, order-independent plugins for extensible types.")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-pocket-resources-0.3
  (package
    (name "rust-pocket-resources")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pocket-resources" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1n2i5vmi8fdbw89wm5nz1ws1z9f1qax911p6ksg4scmdg23z6df1"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/tomaka/pocket-resources")
    (synopsis "Include resources in your applications")
    (description "This crate allows you to include resources in your
applications.")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-ppv-lite86-0.2
  (package
    (name "rust-ppv-lite86")
    (version "0.2.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ppv-lite86" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "06snnv338w341nicfqba2jgln5dsla72ndkgrw7h1dfdb3vgkjz3"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/cryptocorrosion/cryptocorrosion")
    (synopsis "Implementation of the crypto-simd API for x86")
    (description "This crate provides an implementation of the crypto-simd API
for x86.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

;; Cyclic dependencies with rust-demo-hack.
(define-public rust-proc-macro-hack-0.5
  (package
    (name "rust-proc-macro-hack")
    (version "0.5.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "proc-macro-hack" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1www5lrvsk7pq04clgfmjlnnrshikgs1h51l17vrc7qy58bx878c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-0.4)
        ("rust-quote" ,rust-quote-1.0)
        ("rust-syn" ,rust-syn-0.15))
       #:cargo-development-inputs
       (("rust-demo-hack" ,rust-demo-hack-0.0)
        ("rust-demo-hack-impl" ,rust-demo-hack-impl-0.0))))
    (home-page "https://github.com/dtolnay/proc-macro-hack")
    (synopsis
     "Procedural macros in expression position")
    (description
     "Procedural macros in expression position.")
    (license (list license:expat license:asl2.0))))

(define-public rust-proc-macro-nested-0.1
  (package
    (name "rust-proc-macro-nested")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "proc-macro-nested" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0bmlksm8vl44wkwihmwr7jsjznhbg0n7aibcw1cs2jgjcp86x6in"))))
    (build-system cargo-build-system)
    (arguments `(#:skip-build? #t))
    (home-page "https://github.com/dtolnay/proc-macro-hack")
    (synopsis
     "Support for nested proc-macro-hack invocations")
    (description
     "Support for nested proc-macro-hack invocations.")
    (license (list license:expat license:asl2.0))))

(define-public rust-proc-macro2-1.0
  (package
    (name "rust-proc-macro2")
    (version "1.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "proc-macro2" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "09rgb5ab0jgw39kyad0lgqs4nb9yaf7mwcrgxqnsxbn4il54g7lw"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/alexcrichton/proc-macro2")
    (synopsis "Stable implementation of the upcoming new `proc_macro` API")
    (description "This package provides a stable implementation of the upcoming new
`proc_macro` API.  Comes with an option, off by default, to also reimplement itself
in terms of the upstream unstable API.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0 license:expat))))

(define-public rust-proc-macro2-0.4
  (package
    (inherit rust-proc-macro2-1.0)
    (name "rust-proc-macro2")
    (version "0.4.30")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "proc-macro2" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0nd71fl24sys066jrha6j7i34nfkjv44yzw8yww9742wmc8j0gfg"))))))

(define-public rust-quick-error-1.2
  (package
    (name "rust-quick-error")
    (version "1.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quick-error" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1w6kgwwv7p7zr0yyg5rb315lkk24bimywklwx7fsvsbwi10bjx4j"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/tailhook/quick-error")
    (synopsis "Macro which makes error types pleasant to write")
    (description "This crate provides a macro which makes error types pleasant
to write.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

;; Many circular dependencies.
;; Dev dependencies are allowed to have them in crates.io.
(define-public rust-quickcheck-0.8
  (package
    (name "rust-quickcheck")
    (version "0.8.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "quickcheck" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0mkl4wnvvjk4m32aq3an4ayfyvnmbxnzcybfm7n3fbsndb1xjdcw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-env-logger" ,rust-env-logger-0.6)
        ("rust-log" ,rust-log-0.4)
        ("rust-rand" ,rust-rand-0.4)
        ("rust-rand-core" ,rust-rand-core-0.5))))
    (home-page
     "https://github.com/BurntSushi/quickcheck")
    (synopsis
     "Automatic property based testing with shrinking")
    (description
     "Automatic property based testing with shrinking.")
    (license (list license:expat license:unlicense))))

(define-public rust-quote-1.0
  (package
    (name "rust-quote")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quote" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1zkc46ryacf2jdkc6krsy2z615xbk1x8kp1830rcxz3irj5qqfh5"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/dtolnay/quote")
    (synopsis "Quasi-quoting macro quote!(...)")
    (description "Quasi-quoting macro quote!(...)")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0 license:expat))))

(define-public rust-quote-0.6
  (package
    (inherit rust-quote-1.0)
    (name "rust-quote")
    (version "0.6.12")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "quote" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1nw0klza45hf127kfyrpxsxd5jw2l6h21qxalil3hkr7bnf7kx7s"))))))

(define-public rust-rand-0.6
  (package
    (name "rust-rand")
    (version "0.6.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1jl4449jcl4wgmzld6ffwqj5gwxrp8zvx8w573g1z368qg6xlwbd"))))
    (build-system cargo-build-system)
    (home-page "https://crates.io/crates/rand")
    (synopsis "Random number generators and other randomness functionality")
    (description
     "Rand provides utilities to generate random numbers, to convert them to
useful types and distributions, and some randomness-related algorithms.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rand-0.4
  (package
    (inherit rust-rand-0.6)
    (name "rust-rand")
    (version "0.4.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1gfgnqfj2pyh27dcb720jpawskllwnbvxh816ddyykv269xz8ml3"))))))

(define-public rust-rand-0.3
  (package
    (inherit rust-rand-0.6)
    (name "rust-rand")
    (version "0.3.23")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0v679h38pjjqj5h4md7v2slsvj6686qgcn7p9fbw3h43iwnk1b34"))))))

(define-public rust-rand-chacha-0.1
  (package
    (name "rust-rand-chacha")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_chacha" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1vxwyzs4fy1ffjc8l00fsyygpiss135irjf7nyxgq2v0lqf3lvam"))))
    (build-system cargo-build-system)
    (home-page "https://crates.io/crates/rand_chacha")
    (synopsis "ChaCha random number generator")
    (description "ChaCha random number generator")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rand-core-0.5
  (package
    (name "rust-rand-core")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rand_core" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1jis94x9ri8xlxki2w2w5k29sjpfwgzkjylg7paganp74hrnhpk1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-getrandom" ,rust-getrandom-0.1)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-serde-derive" ,rust-serde-derive-1.0))))
    (home-page "https://crates.io/crates/rand-core")
    (synopsis
     "Core random number generator traits and tools for implementation")
    (description
     "Core random number generator traits and tools for implementation.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rand-core-0.4
  (package
    (inherit rust-rand-core-0.5)
    (name "rust-rand-core")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_core" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1p09ynysrq1vcdlmcqnapq4qakl2yd1ng3kxh3qscpx09k2a6cww"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-serde" ,rust-serde-1.0)
        ("rust-serde-derive" ,rust-serde-derive-1.0))))))

(define-public rust-rand-core-0.3
  (package
    (inherit rust-rand-core-0.4)
    (name "rust-rand-core")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_core" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0jzdgszfa4bliigiy4hi66k7fs3gfwi2qxn8vik84ph77fwdwvvs"))))
    ;; This version is a 0.3 API wrapper around the 0.4 version.
    (arguments
     `(#:cargo-inputs (("rand-core" ,rust-rand-core-0.4))))))

(define-public rust-rand-hc-0.1
  (package
    (name "rust-rand-hc")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_hc" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1i0vl8q5ddvvy0x8hf1zxny393miyzxkwqnw31ifg6p0gdy6fh3v"))))
    (build-system cargo-build-system)
    (home-page "https://crates.io/crates/rand_hc")
    (synopsis "HC128 random number generator")
    (description "HC128 random number generator")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rand-isaac-0.1
  (package
    (name "rust-rand-isaac")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_isaac" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "027flpjr4znx2csxk7gxb7vrf9c7y5mydmvg5az2afgisp4rgnfy"))))
    (build-system cargo-build-system)
    (home-page "https://crates.io/crates/rand_isaac")
    (synopsis "ISAAC random number generator")
    (description "ISAAC random number generator")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rand-jitter-0.1
  (package
    (name "rust-rand-jitter")
    (version "0.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_jitter" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "16z387y46bfz3csc42zxbjq89vcr1axqacncvv8qhyy93p4xarhi"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-random/rand")
    (synopsis
      "Random number generator based on timing jitter")
    (description
      "Random number generator based on timing jitter")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rand-os-0.1
  (package
    (name "rust-rand-os")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_os" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0wahppm0s64gkr2vmhcgwc0lij37in1lgfxg5rbgqlz0l5vgcxbv"))))
    (build-system cargo-build-system)
    (home-page "https://crates.io/crates/rand_os")
    (synopsis "OS backed Random Number Generator")
    (description "OS backed Random Number Generator")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rand-pcg-0.1
  (package
    (name "rust-rand-pcg")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_pcg" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0i0bdla18a8x4jn1w0fxsbs3jg7ajllz6azmch1zw33r06dv1ydb"))))
    (build-system cargo-build-system)
    (home-page "https://crates.io/crates/rand_pcg")
    (synopsis
      "Selected PCG random number generators")
    (description
      "Selected PCG random number generators")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rand-xorshift-0.1
  (package
    (name "rust-rand-xorshift")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rand_xorshift" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0p2x8nr00hricpi2m6ca5vysiha7ybnghz79yqhhx6sl4gkfkxyb"))))
    (build-system cargo-build-system)
    (home-page "https://crates.io/crates/rand-xorshift")
    (synopsis "Xorshift random number generator")
    (description
      "Xorshift random number generator")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rand-xorshift-0.2
  (package
    (name "rust-rand-xorshift")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rand_xorshift" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1a6wy76lc5fimm1n9n8fzhp4cfjwfwxh4hx63bg3vlh1d2w1dm3p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-rand-core" ,rust-rand-core-0.5)
        ("rust-serde" ,rust-serde-1.0))
       #:cargo-development-inputs
       (("rust-bincode" ,rust-bincode-1.1))))
    (home-page "https://crates.io/crates/rand-xorshift")
    (synopsis "Xorshift random number generator")
    (description
     "Xorshift random number generator.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rand-xoshiro-0.3
  (package
    (name "rust-rand-xoshiro")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rand_xoshiro" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "07w3qgrac8r356lz5vqff42rly6yd9vs3g5lx5pbn13rcmb05rqb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1.3)
        ("rust-rand-core" ,rust-rand-core-0.5)
        ("rust-serde" ,rust-serde-1.0))
       #:cargo-development-inputs
       (("rust-bincode" ,rust-bincode-1.1))))
    (home-page "https://github.com/rust-random/rand")
    (synopsis
     "Xoshiro, xoroshiro and splitmix64 random number generators")
    (description
     "Xoshiro, xoroshiro and splitmix64 random number generators.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rawpointer-0.1
  (package
    (name "rust-rawpointer")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rawpointer" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "06ghpm9y7gacks78s3maakha07kbnwrxif5q37r2l7z1sali3b7b"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bluss/rawpointer/")
    (synopsis "Extra methods for raw pointers")
    (description "Extra methods for raw pointers.  For example
@code{.post_inc()} and @code{.pre_dec()} (c.f. @code{ptr++} and @code{--ptr})
and @code{ptrdistance}.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rayon-1.1
  (package
    (name "rust-rayon")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rayon" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "190hkbcdfvcphyyzkdg52zdia2y9d9yanpm072bmnzbn49p1ic54"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-crossbeam-deque" ,rust-crossbeam-deque-0.6)
        ("rust-either" ,rust-either-1.5)
        ("rust-rayon-core" ,rust-rayon-core-1.5))
       #:cargo-development-inputs
       (("rust-doc-comment" ,rust-doc-comment-0.3)
        ("rust-docopt" ,rust-docopt-1.1)
        ("rust-lazy-static" ,rust-lazy-static-1.3)
        ("rust-rand" ,rust-rand-0.4)
        ("rust-rand-xorshift" ,rust-rand-xorshift-0.2)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-serde-derive" ,rust-serde-derive-1.0))))
    (home-page "https://github.com/rayon-rs/rayon")
    (synopsis "Simple work-stealing parallelism for Rust")
    (description
     "Simple work-stealing parallelism for Rust.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-rayon-core-1.5
  (package
    (name "rust-rayon-core")
    (version "1.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rayon-core" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1ljva6blaf1wmzvg77h1i9pd0hsmsbbcmdk7sjbw7h2s8gw0vgpb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-crossbeam-deque" ,rust-crossbeam-deque-0.7)
        ("rust-crossbeam-queue" ,rust-crossbeam-queue-0.1)
        ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.6)
        ("rust-lazy-static" ,rust-lazy-static-1.3)
        ("rust-num-cpus" ,rust-num-cpus-1.10))
       #:cargo-development-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-rand" ,rust-rand-0.4)
        ("rust-rand-xorshift" ,rust-rand-xorshift-0.2)
        ("rust-scoped-tls" ,rust-scoped-tls-1.0))))
    (home-page "https://github.com/rayon-rs/rayon")
    (synopsis "Core APIs for Rayon")
    (description "Core APIs for Rayon.")
    (license (list license:expat license:asl2.0))))

(define-public rust-rdrand-0.4
  (package
    (name "rust-rdrand")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rdrand" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1cjq0kwx1bk7jx3kzyciiish5gqsj7620dm43dc52sr8fzmm9037"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/nagisa/rust_rdrand/")
    (synopsis "Random number generator")
    (description
     "This package is an implementation of random number generator based on
@code{rdrand} and @cpde{rdseed} instructions")
    (properties '((hidden? . #t)))
    (license license:isc)))

;; This package requires features which are unavailable
;; on the stable releases of Rust.
(define-public rust-redox-syscall-0.1
  (package
    (name "rust-redox-syscall")
    (version "0.1.56")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "redox_syscall" version))
        (file-name (string-append name "-" version ".crate"))
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

(define-public rust-redox-termios-0.1
  (package
    (name "rust-redox-termios")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "redox-termios" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0xhgvdh62mymgdl3jqrngl8hr4i8xwpnbsxnldq0l47993z1r2by"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/redox-os/termios")
    (synopsis "Rust library to access Redox termios functions")
    (description
     "This package provides a Rust library to access Redox termios functions.")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-regex-1.1
  (package
    (name "rust-regex")
    (version "1.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "regex" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1pabajpp0wzb7dm2x32gy8w7k0mwykr6zsvzn0fgpr6pww40hbqb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-aho-corasick" ,rust-aho-corasick-0.7)
        ("rust-memchr" ,rust-memchr-2.2)
        ("rust-regex-syntax" ,rust-regex-syntax-0.6)
        ("rust-thread-local" ,rust-thread-local-0.3)
        ("rust-utf8-ranges" ,rust-utf8-ranges-1.0))
       #:cargo-development-inputs
       (("rust-doc-comment" ,rust-doc-comment-0.3)
        ("rust-lazy-static" ,rust-lazy-static-1.3)
        ("rust-quickcheck" ,rust-quickcheck-0.8)
        ("rust-rand" ,rust-rand-0.4))))
    (home-page "https://github.com/rust-lang/regex")
    (synopsis "Regular expressions for Rust")
    (description
     "An implementation of regular expressions for Rust.  This implementation
uses finite automata and guarantees linear time matching on all inputs.")
    (license (list license:expat license:asl2.0))))

(define-public rust-regex-automata-0.1
  (package
    (name "rust-regex-automata")
    (version "0.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "regex-automata" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "11hzn3rz02vdgvx3ykhrbzkvs5c5sm59fyi3xwljn9qc48br5l1y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1.3)
        ("rust-regex-syntax" ,rust-regex-syntax-0.6)
        ("rust-utf8-ranges" ,rust-utf8-ranges-1.0))
       #:cargo-development-inputs
       (("rust-lazy-static" ,rust-lazy-static-1.3)
        ("rust-regex" ,rust-regex-1.1)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-serde-bytes" ,rust-serde-bytes-0.11)
        ("rust-serde-derive" ,rust-serde-derive-1.0)
        ("rust-toml" ,rust-toml-0.5))))
    (home-page "https://github.com/BurntSushi/regex-automata")
    (synopsis
     "Automata construction and matching using regular expressions")
    (description
     "Automata construction and matching using regular expressions.")
    (license (list license:expat license:unlicense))))

(define-public rust-regex-syntax-0.6
  (package
    (name "rust-regex-syntax")
    (version "0.6.10")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "regex-syntax" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0p47lf38yj2g2fnmvnraccqlxwk35zr76hlnqi8yva932nzqam6d"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-lang/regex")
    (synopsis "Regular expression parser")
    (description
     "This package provides a regular expression parser.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-remove-dir-all-0.5
  (package
    (name "rust-remove-dir-all")
    (version "0.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "remove_dir_all" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0bkrlyg26mgizpiy1yb2hhpgscxcag8r5fnckqsvk25608vzm0sa"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/XAMPPRocky/remove_dir_all")
    (synopsis "Implementation of remove_dir_all for Windows")
    (description
     "This package provides a safe, reliable implementation of
@code{remove_dir_all} for Windows")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-resolv-conf-0.6
  (package
    (name "rust-resolv-conf")
    (version "0.6.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "resolv-conf" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1jvdsmksdf6yiipm3aqahyv8n1cjd7wqc8sa0p0gzsax3fmb8qxj"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/tailhook/resolv-conf")
    (synopsis "/etc/resolv.conf parser")
    (description
     "An /etc/resolv.conf parser crate for Rust.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rustc-demangle-0.1
  (package
    (name "rust-rustc-demangle")
    (version "0.1.16")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustc-demangle" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "10qp42sl1wrdbgbbh8rnay2grm976z7hqgz32c4y09l1c071qsac"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/alexcrichton/rustc-demangle")
    (synopsis "Rust compiler symbol demangling")
    (description
     "This package demanges the symbols from the Rust compiler.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rustc-hash-1.0
  (package
    (name "rust-rustc-hash")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustc-hash" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "114bf72466bl63i5hh8fgqfnhihs0w1m9c9jz505095agfixnvg0"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-lang-nursery/rustc-hash")
    (synopsis "Speedy, non-cryptographic hash used in rustc")
    (description
     "This package provides a speedy, non-cryptographic hash used in rustc.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rustc-serialize-0.3
  (package
    (name "rust-rustc-serialize")
    (version "0.3.24")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustc-serialize" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1nkg3vasg7nk80ffkazizgiyv3hb1l9g3d8h17cajbkx538jiwfw"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-lang-deprecated/rustc-serialize")
    (synopsis "Generic serialization/deserialization support")
    (description
     "This package provides generic serialization/deserialization support
corresponding to the @code{derive(RustcEncodable, RustcDecodable)} mode in the
compiler.  Also includes support for hex, base64, and json encoding and
decoding.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rustc-std-workspace-core-1.0
  (package
    (name "rust-rustc-std-workspace-core")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "rustc-std-workspace-core" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1309xhwyai9xpz128xrfjqkmnkvgjwddznmj7brbd8i8f58zamhr"))))
    (build-system cargo-build-system)
    (home-page "https://crates.io/crates/rustc-std-workspace-core")
    (synopsis "Explicitly empty crate for rust-lang/rust integration")
    (description "This crate provides an explicitly empty crate for
rust-lang/rust integration.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rustc-version-0.2
  (package
    (name "rust-rustc-version")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustc_version" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "02h3x57lcr8l2pm0a645s9whdh33pn5cnrwvn5cb57vcrc53x3hk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-semver" ,rust-semver-0.9))))
    (home-page "https://github.com/Kimundi/rustc-version-rs")
    (synopsis
     "Library for querying the version of a installed rustc compiler")
    (description
     "This package provides a library for querying the version of a installed
rustc compiler.")
    (license (list license:expat license:asl2.0))))

(define-public rust-ryu-1.0
  (package
    (name "rust-ryu")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ryu" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1j0h74f1xqf9hjkhanp8i20mqc1aw35kr1iq9i79q7713mn51a5z"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/dtolnay/ryu")
    (synopsis
      "Fast floating point to string conversion")
    (description
      "Fast floating point to string conversion")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0 license:boost1.0))))

(define-public rust-safemem-0.3
  (package
    (name "rust-safemem")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "safemem" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1l1ljkm4lpyryrv2ndaxi1f7z1f3v9bwy1rzl9f9mbhx04iq9c6j"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/abonander/safemem")
    (synopsis "Safe wrappers for memory-accessing functions")
    (description
     "Safe wrappers for memory-accessing functions, like @code{std::ptr::copy()}.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-same-file-1.0
  (package
    (name "rust-same-file")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "same-file" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "08a4zy10pjindf2rah320s6shgswk13mqw7s61m8i1y1xpf8spjq"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/BurntSushi/same-file")
    (synopsis "Determine whether two file paths point to the same file")
    (description
     "This package provides a simple crate for determining whether two file
paths point to the same file.")
    (properties '((hidden? . #t)))
    (license (list license:unlicense
                   license:expat))))

(define-public rust-schannel-0.1
  (package
    (name "rust-schannel")
    (version "0.1.15")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "schannel" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0f9k4pm8yc3z0n1n8hazvnrvg52f0sfxjc91bhf3r76rb3rapxpj"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/steffengy/schannel-rs")
    (synopsis "Rust bindings to the Windows SChannel APIs")
    (description
     "Rust bindings to the Windows SChannel APIs providing TLS client and
server functionality.")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-scoped-threadpool-0.1
  (package
    (name "rust-scoped-threadpool")
    (version "0.1.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "scoped_threadpool" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1a26d3lk40s9mrf4imhbik7caahmw2jryhhb6vqv6fplbbgzal8x"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/Kimundi/scoped-threadpool-rs")
    (synopsis "library for scoped and cached threadpools")
    (description
     "This crate provides a stable, safe and scoped threadpool.  It can be used
to execute a number of short-lived jobs in parallel without the need to respawn
the underlying threads.  Jobs are runnable by borrowing the pool for a given
scope, during which an arbitrary number of them can be executed. These jobs can
access data of any lifetime outside of the pools scope, which allows working on
non-'static references in parallel.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-scoped-tls-1.0
  (package
    (name "rust-scoped-tls")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "scoped-tls" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1hj8lifzvivdb1z02lfnzkshpvk85nkgzxsy2hc0zky9wf894spa"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/alexcrichton/scoped-tls")
    (synopsis "Rust library providing the old standard library's scoped_thread_local")
    (description "This crate provides a library implementation of the standard
library's old @code{scoped_thread_local!} macro for providing scoped access to
@dfn{thread local storage} (TLS) so any type can be stored into TLS.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-scoped-tls-0.1
  (package
    (inherit rust-scoped-tls-1.0)
    (name "rust-scoped-tls")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "scoped-tls" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0a2bn9d2mb07c6l16sadijy4p540g498zddfxyiq4rsqpwrglbrk"))))))

(define-public rust-scopeguard-1.0
  (package
    (name "rust-scopeguard")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "scopeguard" version))
        (file-name (string-append name "-" version ".crate"))
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
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-scopeguard-0.3
  (package
    (inherit rust-scopeguard-1.0)
    (name "rust-scopeguard")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "scopeguard" version))
        (file-name
          (string-append name "-" version ".crate"))
        (sha256
         (base32
          "09sy9wbqp409pkwmqni40qmwa99ldqpl48pp95m1xw8sc19qy9cl"))))))

(define-public rust-security-framework-sys-0.3
  (package
    (name "rust-security-framework-sys")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "security-framework-sys" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0mlsakq9kmqyc0fg2hcbgm6rjk55mb0rhjw2wid3hqdzkjcghdln"))))
    (build-system cargo-build-system)
    (home-page "https://lib.rs/crates/security-framework-sys")
    (synopsis "Apple `Security.framework` low-level FFI bindings")
    (description
     "Apple `Security.framework` low-level FFI bindings.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-semver-0.9
  (package
    (name "rust-semver")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "semver" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "00q4lkcj0rrgbhviv9sd4p6qmdsipkwkbra7rh11jrhq5kpvjzhx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-semver-parser" ,rust-semver-parser-0.7)
        ("rust-serde" ,rust-serde-1.0))
       #:cargo-development-inputs
       (("rust-crates-index" ,rust-crates-index-0.13)
        ("rust-serde-derive" ,rust-serde-derive-1.0)
        ("rust-serde-json" ,rust-serde-json-1.0)
        ("rust-tempdir" ,rust-tempdir-0.3))))
    (home-page "https://docs.rs/crate/semver")
    (synopsis
     "Semantic version parsing and comparison")
    (description
     "Semantic version parsing and comparison.")
    (license (list license:expat license:asl2.0))))

(define-public rust-semver-parser-0.9
  (package
    (name "rust-semver-parser")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "semver-parser" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1ahqhvgpzhcsd28id7xnrjv4419i9yyalhm7d7zi430qx0hi2vml"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/steveklabnik/semver-parser")
    (synopsis "Parsing of the semver spec")
    (description "This package provides for parsing of the semver spec.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-semver-parser-0.7
  (package
    (inherit rust-semver-parser-0.9)
    (name "rust-semver-parser")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "semver-parser" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "18vhypw6zgccnrlm5ps1pwa0khz7ry927iznpr88b87cagr1v2iq"))))))

(define-public rust-serde-1.0
  (package
    (name "rust-serde")
    (version "1.0.101")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1p8r24hagcsrl92w5z32nfrg9040qkgqf8iwwnf7mzigpavwk5lp"))))
    (build-system cargo-build-system)
    (home-page "https://serde.rs")
    (synopsis "Generic serialization/deserialization framework")
    (description
     "This package provides a generic serialization/deserialization framework.")
    (properties '((hidden? . #t)))
    (license (list license:expat license:asl2.0))))

;; Circular dev dependency on bincode.
;; Probably not going away: https://github.com/rust-lang/cargo/issues/4242
(define-public rust-serde-bytes-0.11
  (package
    (name "rust-serde-bytes")
    (version "0.11.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_bytes" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1bl45kf3c71xclv7wzk5525nswm4bgsnjd3s1s15f4k2a8whfnij"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-serde" ,rust-serde-1.0))
       #:cargo-development-inputs
       (("rust-bincode" ,rust-bincode-1.1)
        ("rust-serde-derive" ,rust-serde-derive-1.0)
        ("rust-serde-test" ,rust-serde-test-1.0))))
    (home-page "https://github.com/serde-rs/bytes")
    (synopsis
     "Hanlde of integer arrays and vectors for Serde")
    (description
     "Optimized handling of @code{&[u8]} and @code{Vec<u8>} for Serde.")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-derive-1.0
  (package
    (name "rust-serde-derive")
    (version "1.0.101")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde-derive" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0bn0wz3j48248187mfmypyqnh73mq734snxxhr05vmgcl51kl4sb"))))
    (build-system cargo-build-system)
    (home-page "https://serde.rs")
    (synopsis
      "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (description
      "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (properties '((hidden? . #t)))
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-json-1.0
  (package
    (name "rust-serde-json")
    (version "1.0.41")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "serde-json" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1hipk84x40454mf599752mi7l08wb8qakz8vd6d3zp57d0mfnwig"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/serde-rs/json")
    (synopsis "A JSON serialization file format")
    (description
     "This package provides a JSON serialization file format.")
    (properties '((hidden? . #t)))
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-test-1.0
  (package
    (name "rust-serde-test")
    (version "1.0.101")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_test" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0070ycbh47yhxb5vxwa15vi2wpdkw3v1m14v4mjryz1568fqkbsa"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-serde" ,rust-serde-1.0))
       #:cargo-development-inputs
       (("rust-serde" ,rust-serde-1.0)
        ("rust-serde-derive" ,rust-serde-derive-1.0))))
    (home-page "https://serde.rs")
    (synopsis
     "Token De/Serializer for testing De/Serialize implementations")
    (description
     "Token De/Serializer for testing De/Serialize implementations.")
    (license (list license:expat license:asl2.0))))

(define-public rust-sha1-0.6
  (package
    (name "rust-sha1")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sha1" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "03gs2q4m67rn2p8xcdfxhip6mpgahdwm12bnb3vh90ahv9grhy95"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-serde" ,rust-serde-1.0))
       #:cargo-development-inputs
       (("rust-openssl" ,rust-openssl-0.10)
        ("rust-rand" ,rust-rand-0.4)
        ("rust-serde-json" ,rust-serde-json-1.0))))
    (home-page "https://github.com/mitsuhiko/rust-sha1")
    (synopsis "Minimal implementation of SHA1 for Rust")
    (description
     "Minimal implementation of SHA1 for Rust.")
    (license license:bsd-3)))

(define-public rust-shlex-0.1
  (package
    (name "rust-shlex")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "shlex" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1lmv6san7g8dv6jdfp14m7bdczq9ss7j7bgsfqyqjc3jnjfippvz"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/comex/rust-shlex")
    (synopsis "Split a string into shell words, like Python's shlex")
    (description "This crate provides a method to split a string into shell
words, like Python's shlex.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-slab-0.4
  (package
    (name "rust-slab")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "slab" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1y59xsa27jk84sxzswjk60xcjf8b4fm5960jwpznrrcmasyva4f1"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/carllerche/slab")
    (synopsis "Pre-allocated storage for a uniform data type")
    (description "This create provides a pre-allocated storage for a uniform
data type.")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-smallvec-0.6
  (package
    (name "rust-smallvec")
    (version "0.6.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "smallvec" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1dyl43rgzny79jjpgzi07y0ly2ggx1xwsn64csxj0j91bsf6lq5b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-serde" ,rust-serde-1.0))
       #:cargo-development-inputs
       (("rust-bincode" ,rust-bincode-1.1))))
    (home-page "https://github.com/servo/rust-smallvec")
    (synopsis "Small vector optimization")
    (description
     "'Small vector' optimization: store up to a small number of items on the
stack.")
    (license (list license:expat license:asl2.0))))

(define-public rust-socket2-0.3
  (package
    (name "rust-socket2")
    (version "0.3.11")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "socket2" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "11bdcz04i106g4q7swkll0qxrb4287srqd2k3aq2q6i22zjlvdz8"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/alexcrichton/socket2-rs")
    (synopsis "Networking sockets in Rust")
    (description
     "This package provides utilities for handling networking sockets with a
maximal amount of configuration possible intended.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-sourcefile-0.1
  (package
    (name "rust-sourcefile")
    (version "0.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sourcefile" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1lwa6973zs4bgj29my7agfjgk4nw9hp6j7dfnr13nid85fw7rxsb"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/derekdreery/sourcefile-rs")
    (synopsis "Concatenate source from multiple files")
    (description
     "A library for concatenating source from multiple files, whilst keeping
track of where each new file and line starts.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-speculate-0.1
  (package
    (name "rust-speculate")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "speculate" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0ph01n3fqkmnfr1wd13dqsi4znv06xy6p4h3hqqdzk81r0r5vd1w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-0.4)
        ("rust-quote" ,rust-quote-1.0)
        ("rust-syn" ,rust-syn-0.15)
        ("rust-unicode-xid" ,rust-unicode-xid-0.1))))
    (home-page "https://github.com/utkarshkukreti/speculate.rs")
    (synopsis "RSpec inspired testing framework for Rust")
    (description
     "An RSpec inspired minimal testing framework for Rust.")
    (license license:expat)))

(define-public rust-spin-0.5
  (package
    (name "rust-spin")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "spin" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0m9clchsj0rf13bggsgvbv9haiy0f6rhvnvkpvkk8720a5pkydj4"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/mvdnes/spin-rs")
    (synopsis "Synchronization primitives based on spinning")
    (description "This crate provides synchronization primitives based on
spinning.  They may contain data, are usable without @code{std},and static
initializers are available.")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-stable-deref-trait-1.1
  (package
    (name "rust-stable-deref-trait")
    (version "1.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "stable_deref_trait" version))
        (file-name (string-append name "-" version ".crate"))
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
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-stacker-0.1
  (package
    (name "rust-stacker")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "stacker" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0js0axz5nla1mkr2dm2vrv9rj964ng1lrv4l43sqlnfgawplhygv"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-lang/stacker")
    (synopsis "Manual segmented stacks for Rust")
    (description
     "This package provides a stack growth library useful when implementing
deeply recursive algorithms that may accidentally blow the stack.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-static-assertions-0.3
  (package
    (name "rust-static-assertions")
    (version "0.3.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "static-assertions" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1lw33i89888yb3x29c6dv4mrkg3534n0rlg3r7qzh4p58xmv6gkz"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/nvzqz/static-assertions-rs")
    (synopsis "Compile-time assertions for rust")
    (description
     "This package provides compile-time assertions to ensure that invariants
are met.")
    (properties '((hidden? . #t)))
    (license (list license:expat license:asl2.0))))

(define-public rust-stdweb-derive-0.5
  (package
    (name "rust-stdweb-derive")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "stdweb-derive" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0c1rxx6rqcc4iic5hx320ki3vshpi8k58m5600iqzq4x2zcyn88f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-0.4)
        ("rust-quote" ,rust-quote-1.0)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-serde-derive" ,rust-serde-derive-1.0)
        ("rust-syn" ,rust-syn-0.15))))
    (home-page "https://github.com/koute/stdweb")
    (synopsis "Derive macros for the stdweb crate")
    (description
     "Derive macros for the @code{stdweb} crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-stdweb-internal-macros-0.2
  (package
    (name "rust-stdweb-internal-macros")
    (version "0.2.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "stdweb-internal-macros" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1yjrmkc6sb1035avic383pa3avk2s9k3n17yjcza8yb9nw47v3z6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-base-x" ,rust-base-x-0.2)
        ("rust-proc-macro2" ,rust-proc-macro2-0.4)
        ("rust-quote" ,rust-quote-1.0)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-serde-derive" ,rust-serde-derive-1.0)
        ("rust-serde-json" ,rust-serde-json-1.0)
        ("rust-sha1" ,rust-sha1-0.6)
        ("rust-syn" ,rust-syn-0.15))))
    (home-page "https://github.com/koute/stdweb")
    (synopsis "Internal procedural macros for the stdweb crate")
    (description
     "Internal procedural macros for the stdweb crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-stdweb-internal-runtime-0.1
  (package
    (name "rust-stdweb-internal-runtime")
    (version "0.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "stdweb-internal-runtime" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1nhpyra7glbwcpakhpj5a3d7h7kx1ynif473nzshmk226m91f8ym"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/koute/stdweb")
    (synopsis "Internal runtime for the @code{stdweb} crate")
    (description "This crate provides internal runtime for the @code{stdweb}
crate.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-stdweb-internal-test-macro-0.1
  (package
    (name "rust-stdweb-internal-test-macro")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "stdweb-internal-test-macro" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "12rrm7p77xnm3xacgn3rgniiyyjb4gq7902wpbljsvbx045z69l2"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/koute/stdweb")
    (synopsis "Internal crate of the `stdweb` crate")
    (description
     "Internal crate of the @code{stdweb} crate.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-streaming-stats-0.2
  (package
    (name "rust-streaming-stats")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "streaming-stats" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0l7xz4g6709s80zqpvlhrg0qhgz64r94cwhmfsg8xhabgznbp2px"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/BurntSushi/rust-stats")
    (synopsis "Compute basic statistics on streams")
    (description
     "Experimental crate for computing basic statistics on streams.")
    (properties '((hidden? . #t)))
    (license (list license:unlicense
                   license:expat))))

(define-public rust-strsim-0.9
  (package
    (name "rust-strsim")
    (version "0.9.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "strsim" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1xphwhf86yxxmcpvm4mikj8ls41f6nf7gqyjm98b74mfk81h6b03"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/dguo/strsim-rs")
    (synopsis "Rust implementations of string similarity metrics")
    (description "This crate includes implementations of string similarity
metrics.  It includes Hamming, Levenshtein, OSA, Damerau-Levenshtein, Jaro,
and Jaro-Winkler.")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-strsim-0.8
  (package
    (inherit rust-strsim-0.9)
    (name "rust-strsim")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "strsim" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0sjsm7hrvjdifz661pjxq5w4hf190hx53fra8dfvamacvff139cf"))))))

(define-public rust-syn-1.0
  (package
    (name "rust-syn")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "syn" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1gw03w7lzrlqmp2vislcybikgl5wkhrqi6sy70w93xss2abhx1b6"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/dtolnay/syn")
    (synopsis "Parser for Rust source code")
    (description "Parser for Rust source code")
    (properties '((hidden? . #t)))
    (license (list license:expat license:asl2.0))))

(define-public rust-syn-0.15
  (package
    (inherit rust-syn-1.0)
    (name "rust-syn")
    (version "0.15.44")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "syn" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1id5g6x6zihv3j7hwrw3m1jp636bg8dpi671r7zy3jvpkavb794w"))))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-0.4)
        ("rust-quote" ,rust-quote-1.0)
        ("rust-unicode-xid" ,rust-unicode-xid-0.2))
       #:cargo-development-inputs
       (("rust-insta" ,rust-insta-0.8)
        ("rust-rayon" ,rust-rayon-1.1)
        ("rust-ref-cast" ,rust-ref-cast-0.2)
        ("rust-regex" ,rust-regex-1.1)
        ("rust-termcolor" ,rust-termcolor-1.0)
        ("rust-walkdir" ,rust-walkdir-2.2))))
    (properties '())))

(define-public rust-synstructure-test-traits-0.1
  (package
    (name "rust-synstructure-test-traits")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "synstructure_test_traits" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1b3fs2b9kc1gy9dilaxqjbdl4z0mlrbbxjzkprdx953rif1c3q66"))))
    (build-system cargo-build-system)
    (home-page "https://crates.io/crates/synstructure_test_traits")
    (synopsis "Helper test traits for synstructure doctests")
    (description
     "This package provides helper test traits for synstructure doctests.")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-tar-0.4
  (package
    (name "rust-tar")
    (version "0.4.26")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tar" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1lr6v3cpkfxd2lk5ll2jd8wr1xdskwj35smnh5sfb8xvzzxnn6dk"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/alexcrichton/tar-rs")
    (synopsis "Tar file reading/writing for Rust")
    (description
     "This package provides a Rust implementation of a TAR file reader and
writer.  This library does not currently handle compression, but it is abstract
over all I/O readers and writers.  Additionally, great lengths are taken to
ensure that the entire contents are never required to be entirely resident in
memory all at once.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-tempdir-0.3
  (package
    (name "rust-tempdir")
    (version "0.3.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tempdir" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1n5n86zxpgd85y0mswrp5cfdisizq2rv3la906g6ipyc03xvbwhm"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-lang-deprecated/tempdir")
    (synopsis "Temporary directory management for Rust")
    (description
     "This package provides a library for managing a temporary directory and
deleting all contents when it's dropped.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-tempfile-3.0
  (package
    (name "rust-tempfile")
    (version "3.0.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tempfile" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1vqk7aq2l04my2r3jiyyxirnf8f90nzcvjasvrajivb85s7p7i3x"))))
    (build-system cargo-build-system)
    (home-page "http://stebalien.com/projects/tempfile-rs")
    (synopsis "Library for managing temporary files and directories")
    (description
     "This package provides a library for managing temporary files and
directories.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-term-0.4
  (package
    (name "rust-term")
    (version "0.4.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "term" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1wbh8ngqkqr3f6wz902yplf60bd5yapnckvrkgmzp5nffi7n8qzs"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/Stebalien/term")
    (synopsis "Terminal formatting library")
    (description
     "This package provides a terminal formatting library in rust.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-term-0.5.1
  (package
    (inherit rust-term-0.4)
    (name "rust-term")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "term" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0qbmqd8jbjlqr4608qdmvp6yin5ypifzi5s2xyhlw8g8s5ynfssy"))))
    (arguments `(#:skip-build? #t))
    (properties '())))

(define-public rust-termcolor-1.0
  (package
    (name "rust-termcolor")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "termcolor" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0vjfsn1a8zvqhnrbygrz1id6yckwv1dncw3w4zj65qdx0f00kmln"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/BurntSushi/termcolor")
    (synopsis "Library for writing colored text to a terminal")
    (description "This package provides a simple cross platform library for
writing colored text to a terminal.")
    (properties '((hidden? . #t)))
    (license (list license:unlicense
                   license:expat))))

(define-public rust-termion-1.5
  (package
    (name "rust-termion")
    (version "1.5.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "termion" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0c634rg520zjjfhwnxrc2jbfjz7db0rcpsjs1qici0nyghpv53va"))))
    (build-system cargo-build-system)
    (home-page "https://gitlab.redox-os.org/redox-os/termion")
    (synopsis "Library for manipulating terminals")
    (description
     "This package provides a bindless library for manipulating terminals.")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-termios-0.3
  (package
    (name "rust-termios")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "termios" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "09any1p4jp4bphvb5ikagnvwjc3xn2djchy96nkpa782xb2j1dkj"))))
    (build-system cargo-build-system)
    (home-page  "https://github.com/dcuddeback/termios-rs")
    (synopsis "Safe bindings for the termios library")
    (description
     "The termios crate provides safe bindings for the Rust programming language
to the terminal I/O interface implemented by Unix operating systems.  The safe
bindings are a small wrapper around the raw C functions, which converts integer
return values to @code{std::io::Result} to indicate success or failure.")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-textwrap-0.11
  (package
    (name "rust-textwrap")
    (version "0.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "textwrap" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0q5hky03ik3y50s9sz25r438bc4nwhqc6dqwynv4wylc807n29nk"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/mgeisler/textwrap")
    (synopsis "Library for word wrapping, indenting, and dedenting strings")
    (description
     "Textwrap is a small library for word wrapping, indenting, and dedenting
strings.  You can use it to format strings (such as help and error messages)
for display in commandline applications.  It is designed to be efficient and
handle Unicode characters correctly.")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-thread-id-3.3
  (package
    (name "rust-thread-id")
    (version "3.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "thread-id" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1h90v19fjz3x9b25ywh68z5yf2zsmm6h5zb4rl302ckbsp4z9yy7"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/ruuda/thread-id")
    (synopsis "Get a unique ID for the current thread in Rust")
    (description
     "For diagnostics and debugging it can often be useful to get an ID that is
different for every thread.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-thread-local-0.3
  (package
    (name "rust-thread-local")
    (version "0.3.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "thread_local" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "06rzik99p8c5js8238yhc8rk6np543ylb1dy9nrw5v80j0r3xdf6"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/Amanieu/thread_local-rs")
    (synopsis "Per-object thread-local storage")
    (description "Per-object thread-local storage")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-threadpool-1.7
  (package
    (name "rust-threadpool")
    (version "1.7.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "threadpool" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0rd89n1q7vy47w4c32cnynibffv9kj3jy3dwr0536n9lbw5ckw72"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-threadpool/rust-threadpool")
    (synopsis "Thread pool for running jobs on a fixed set of worker threads")
    (description
     "This package provides a thread pool for running a number of jobs on a
fixed set of worker threads.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-time-0.1
  (package
    (name "rust-time")
    (version "0.1.39")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "time" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "161hqx0gw722ikydanpahky447vaxqncwmkj66rny282vzqpalx1"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rust-lang-deprecated/time")
    (synopsis "Simple time handling in Rust")
    (description
     "This package provides utilities for working with time-related functions
in Rust.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-tinytemplate-1.0
  (package
    (name "rust-tinytemplate")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tinytemplate" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "084w41m75i95sdid1wwlnav80jsl1ggyryl4nawxvb6amigvfx25"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-serde" ,rust-serde-1.0)
        ("rust-serde-json" ,rust-serde-json-1.0))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.2)
        ("rust-serde-derive" ,rust-serde-derive-1.0))))
    (home-page "https://github.com/bheisler/TinyTemplate")
    (synopsis "Simple, lightweight template engine")
    (description
     "Simple, lightweight template engine.")
    (license (list license:asl2.0 license:expat))))

;; Cyclic dependency with tokio-io
(define-public rust-tokio-codec-0.1
  (package
    (name "rust-tokio-codec")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tokio-codec" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "17y3hi3dd0bdfkrzshx9qhwcf49xv9iynszj7iwy3w4nmz71wl2w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bytes" ,rust-bytes-0.4)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-tokio-io" ,rust-tokio-io-0.1))))
    (home-page "https://tokio.rs")
    (synopsis
     "Utilities for encoding and decoding frames")
    (description
     "Utilities for encoding and decoding frames.")
    (license license:expat)))

(define-public rust-tokio-current-thread-0.1
  (package
    (name "rust-tokio-current-thread")
    (version "0.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tokio-current-thread" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0hx4c8v88kk0ih8x5s564gsgwwf8n11kryvxm72l1f7isz51fqni"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-futures" ,rust-futures-0.1)
        ("rust-tokio-executor" ,rust-tokio-executor-0.1))))
    (home-page "https://github.com/tokio-rs/tokio")
    (synopsis
     "Manage many tasks concurrently on the current thread")
    (description
     "Single threaded executor which manage many tasks concurrently on
the current thread.")
    (license license:expat)))

;; Cyclic dependency with rust-tokio.
(define-public rust-tokio-executor-0.1
  (package
    (name "rust-tokio-executor")
    (version "0.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tokio-executor" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0pjmgpg58k3hf5q9w6xjljsv8xy66lf734qnfwsc0g3pq3349sl3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-crossbeam-utils" ,rust-crossbeam-utils-0.6)
        ("rust-futures" ,rust-futures-0.1))
       #:cargo-development-inputs
       (("rust-tokio" ,rust-tokio-0.1))))
    (home-page "https://github.com/tokio-rs/tokio")
    (synopsis "Future execution primitives")
    (description "Future execution primitives.")
    (license license:expat)))

;; Cyclic dependencies with tokio and tokio-current-thread
(define-public rust-tokio-io-0.1
  (package
    (name "rust-tokio-io")
    (version "0.1.12")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tokio-io" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "09jrz1hh4h1vj45qy09y7m7m8jsy1hl6g32clnky25mdim3dp42h"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bytes" ,rust-bytes-0.4)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-log" ,rust-log-0.4))
       #:cargo-development-inputs
       (("rust-tokio-current-thread" ,rust-tokio-current-thread-0.1))))
    (home-page "https://tokio.rs")
    (synopsis
     "Core I/O primitives for asynchronous I/O in Rust")
    (description
     "Core I/O primitives for asynchronous I/O in Rust.")
    (license license:expat)))

(define-public rust-tokio-mock-task-0.1
  (package
    (name "rust-tokio-mock-task")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokio-mock-task" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1y7q83qfk9ljjfvs82b453pmz9x1v3d6kr4x55j8mal01s6790dw"))))
    (build-system cargo-build-system)
    (home-page  "https://github.com/carllerche/tokio-mock-task")
    (synopsis "Mock a Tokio task")
    (description "Mock a Tokio task")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-tokio-reactor-0.1
  (package
    (name "rust-tokio-reactor")
    (version "0.1.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tokio-reactor" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1khip64cn63xvayq1db68kxcnhgw3cb449a4n2lbw4p1qzx6pwba"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-crossbeam-utils" ,rust-crossbeam-utils-0.6)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-lazy-static" ,rust-lazy-static-1.3)
        ("rust-log" ,rust-log-0.4)
        ("rust-mio" ,rust-mio-0.6)
        ("rust-num-cpus" ,rust-num-cpus-1.10)
        ("rust-parking-lot" ,rust-parking-lot-0.7)
        ("rust-slab" ,rust-slab-0.4)
        ("rust-tokio-executor" ,rust-tokio-executor-0.1)
        ("rust-tokio-io" ,rust-tokio-io-0.1)
        ("rust-tokio-sync" ,rust-tokio-sync-0.1))
       #:cargo-development-inputs
       (("rust-num-cpus" ,rust-num-cpus-1.10)
        ("rust-tokio" ,rust-tokio-0.1)
        ("rust-tokio-io-pool" ,rust-tokio-io-pool-0.1))))
    (home-page "https://tokio.rs")
    (synopsis
     "Event loop that drives Tokio I/O resources")
    (description
     "Event loop that drives Tokio I/O resources.")
    (license license:expat)))

(define-public rust-tokio-sync-0.1
  (package
    (name "rust-tokio-sync")
    (version "0.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tokio-sync" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1ryalh7dcmnz46xj1va8aaw3if6vd4mj87r67dqvrqhpyf7j8qi1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-fnv" ,rust-fnv-1.0)
        ("rust-futures" ,rust-futures-0.1))
       #:cargo-development-inputs
       (("rust-env-logger" ,rust-env-logger-0.6)
        ("rust-loom" ,rust-loom-0.1)
        ("rust-tokio" ,rust-tokio-0.1)
        ("rust-tokio-mock-task" ,rust-tokio-mock-task-0.1))))
    (home-page "https://tokio.rs")
    (synopsis "Synchronization utilities")
    (description "Synchronization utilities.")
    (license license:expat)))

(define-public rust-tokio-tcp-0.1
  (package
    (name "rust-tokio-tcp")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tokio-tcp" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "06a15vg8bcd33ng3h9ldzlq7wl4jsw0p9qpy7v22ls5yah3b250x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bytes" ,rust-bytes-0.4)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-iovec" ,rust-iovec-0.1)
        ("rust-mio" ,rust-mio-0.6)
        ("rust-tokio-io" ,rust-tokio-io-0.1)
        ("rust-tokio-reactor" ,rust-tokio-reactor-0.1))
       #:cargo-development-inputs
       (("rust-env-logger" ,rust-env-logger-0.6)
        ("rust-tokio" ,rust-tokio-0.1))))
    (home-page "https://tokio.rs")
    (synopsis "TCP bindings for tokio")
    (description "TCP bindings for tokio.")
    (license license:expat)))

(define-public rust-tokio-threadpool-0.1
  (package
    (name "rust-tokio-threadpool")
    (version "0.1.14")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tokio-threadpool" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1wkj3wixicsqvllm8w74b24knw6mdn00zslm8l9fm1p81gr8lmbj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-crossbeam-deque" ,rust-crossbeam-deque-0.7)
        ("rust-crossbeam-queue" ,rust-crossbeam-queue-0.1)
        ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.6)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-log" ,rust-log-0.4)
        ("rust-num-cpus" ,rust-num-cpus-1.10)
        ("rust-rand" ,rust-rand-0.4)
        ("rust-slab" ,rust-slab-0.4)
        ("rust-tokio-executor" ,rust-tokio-executor-0.1))
       #:cargo-development-inputs
       (("rust-env-logger" ,rust-env-logger-0.6)
        ("rust-futures-cpupool" ,rust-futures-cpupool-0.1)
        ("rust-threadpool" ,rust-threadpool-1.7))))
    (home-page "https://github.com/tokio-rs/tokio")
    (synopsis
     "Task scheduler backed by a work-stealing thread pool")
    (description
     "This package provides a task scheduler backed by a work-stealing thread
pool.")
    (license license:expat)))

(define-public rust-toml-0.5
  (package
    (name "rust-toml")
    (version "0.5.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "toml" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "093p48vpqm4bb8q3514xsij0dkljxlr3jp9ypxr4p48xjisvxan7"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/alexcrichton/toml-rs")
    (synopsis "Rust encoder and decoder of TOML-formatted files and streams")
    (description
     "This package provides a native Rust encoder and decoder of TOML-formatted
files and streams.  Provides implementations of the standard
Serialize/Deserialize traits for TOML data to facilitate deserializing and
serializing Rust structures.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-tracing-core-0.1
  (package
    (name "rust-tracing-core")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tracing-core" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "01fa73wzw2m5ybi3kkd52dgrw97mgc3i6inmhwys46ab28giwnxi"))))
    (build-system cargo-build-system)
    (home-page "https://tokio.rs")
    (synopsis "Core primitives for application-level tracing")
    (description
     "Core primitives for application-level tracing.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-traitobject-0.1
  (package
    (name "rust-traitobject")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "traitobject" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0yb0n8822mr59j200fyr2fxgzzgqljyxflx9y8bdy3rlaqngilgg"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/reem/rust-traitobject")
    (synopsis "Unsafe helpers for dealing with raw trait objects")
    (description "Unsafe helpers for dealing with raw trait objects.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-try-from-0.3
  (package
    (name "rust-try-from")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "try_from" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "12wdd4ja7047sd3rx70hv2056hyc8gcdllcx3a41g1rnw64kng98"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/derekjw/try_from")
    (synopsis "TryFrom and TryInto traits for failable conversions")
    (description
      "TryFrom and TryInto traits for failable conversions that return a Result.")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-try-lock-0.2
  (package
    (name "rust-try-lock")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "try-lock" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "10p36rx6pqi9d0zr876xa8vksx2m66ha45myakl50rn08dxyn176"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/seanmonstar/try-lock")
    (synopsis "Lightweight atomic lock")
    (description
     "This package provides a lightweight atomic lock.")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-typeable-0.1
  (package
    (name "rust-typeable")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "typeable" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "11w8dywgnm32hb291izjvh4zjd037ccnkk77ahk63l913zwzc40l"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/reem/rust-typeable")
    (synopsis "Exposes Typeable, for getting TypeIds at runtime")
    (description "Exposes Typeable, for getting TypeIds at runtime.")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-typemap-0.3
  (package
    (name "rust-typemap")
    (version "0.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "typemap" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1xm1gbvz9qisj1l6d36hrl9pw8imr8ngs6qyanjnsad3h0yfcfv5"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/reem/rust-typemap")
    (synopsis "Typesafe store for many value types")
    (description
     "A typesafe store for many value types.")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-typenum-1.10
  (package
    (name "rust-typenum")
    (version "1.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "typenum" version))
        (file-name (string-append name "-" version ".crate"))
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
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-ucd-trie-0.1
  (package
    (name "rust-ucd-trie")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ucd-trie" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1hh6kyzh5xygwy96wfmsf8v8czlzhps2lgbcyhj1xzy1w1xys04g"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/BurntSushi/ucd-generate")
    (synopsis "Trie for storing Unicode codepoint sets and maps")
    (description
     "This package provides a trie for storing Unicode codepoint sets and maps.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-ucd-util-0.1
  (package
    (name "rust-ucd-util")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ucd-util" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0x088q5z0m09a2jqcfgsnq955y8syn1mgn35cl78qinkxm4kp6zs"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/BurntSushi/ucd-generate")
    (synopsis "library for working with the Unicode character database")
    (description "This package provides a small utility library for working
with the Unicode character database.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-unicase-2.4
  (package
    (name "rust-unicase")
    (version "2.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicase" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1xmpmkakhhblq7dzab1kwyv925kv7fqjkjsxjspg6ix9n88makm8"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/seanmonstar/unicase")
    (synopsis "Case-insensitive wrapper around strings")
    (description
     "A case-insensitive wrapper around strings.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-unicode-width-0.1
  (package
    (name "rust-unicode-width")
    (version "0.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicode-width" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "082f9hv1r3gcd1xl33whjhrm18p0w9i77zhhhkiccb5r47adn1vh"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/unicode-rs/unicode-width")
    (synopsis "Determine displayed width according to Unicode rules")
    (description "This crate allows you to determine displayed width of
@code{char} and @code{str} types according to Unicode Standard Annex #11 rules.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-unicode-xid-0.2
  (package
    (name "rust-unicode-xid")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicode-xid" version))
        (file-name
          (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0z09fn515xm7zyr0mmdyxa9mx2f7azcpv74pqmg611iralwpcvl2"))))
    (build-system cargo-build-system)
    (home-page
      "https://github.com/unicode-rs/unicode-xid")
    (synopsis "Determine Unicode XID related properties")
    (description "Determine whether characters have the XID_Start
or XID_Continue properties according to Unicode Standard Annex #31.")
    (properties '((hidden? . #t)))
    ;; Dual licensed.
    (license (list license:asl2.0 license:expat))))

(define-public rust-unicode-xid-0.1
  (package
    (inherit rust-unicode-xid-0.2)
    (name "rust-unicode-xid")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unicode-xid" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1z57lqh4s18rr4x0j4fw4fmp9hf9346h0kmdgqsqx0fhjr3k0wpw"))))))

(define-public rust-unindent-0.1
  (package
    (name "rust-unindent")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unindent" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32 "14s97blyqgf9hzxk22iazrghj60midajkw2801dfspz3n2iqmwb3"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/dtolnay/indoc")
    (synopsis "Remove a column of leading whitespace from a string")
    (description "This crate allows you to remove a column of leading
whitespace from a string.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-unreachable-1.0
  (package
    (name "rust-unreachable")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unreachable" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0mps2il4xy2mjqc3appas27hhn2xmvixc3bzzhfrjj74gy3i0a1q"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/reem/rust-unreachable")
    (synopsis "Unreachable code optimization hint in rust")
    (description
     "This package provides an unreachable code optimization hint in rust.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-unsafe-any-0.4
  (package
    (name "rust-unsafe-any")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "unsafe-any" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0zwwphsqkw5qaiqmjwngnfpv9ym85qcsyj7adip9qplzjzbn00zk"))))
    (build-system cargo-build-system)
    (home-page "https://tokio.rs")
    (synopsis "Traits and implementations for unchecked downcasting")
    (description
     "Traits and implementations for unchecked downcasting.")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-untrusted-0.7
  (package
    (name "rust-untrusted")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "untrusted" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1kmfykcwif6ashkwg54gcnhxj03kpba2i9vc7z5rpr0xlgvrwdk0"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/briansmith/untrusted")
    (synopsis "Zero-allocation parsing of untrusted inputs in Rust")
    (description
     "Safe, fast, zero-panic, zero-crashing, zero-allocation parsing of
untrusted inputs in Rust.")
    (properties '((hidden? . #t)))
    (license license:isc)))

(define-public rust-utf8-ranges-1.0
  (package
    (name "rust-utf8-ranges")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "utf8-ranges" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1ppzjsxmv1p1xfid8wwn07ciikk84k30frl28bwsny6za1vall4x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-development-inputs
       (("rust-doc-comment" ,rust-doc-comment-0.3)
        ("rust-quickcheck" ,rust-quickcheck-0.8))))
    (home-page "https://github.com/BurntSushi/utf8-ranges")
    (synopsis
     "Convert ranges of Unicode codepoints to UTF-8 byte ranges")
    (description
     "Convert ranges of Unicode codepoints to UTF-8 byte ranges.")
    (license (list license:expat license:unlicense))))

(define-public rust-vcpkg-0.2
  (package
    (name "rust-vcpkg")
    (version "0.2.7")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "vcpkg" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "15dzk1b96q946v9aisbd1bbhi33n93wvgziwh1shmscn1xflbp9k"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/mcgoo/vcpkg-rs")
    (synopsis "Find native dependencies in a vcpkg tree at build time")
    (description
     "This package provides a library to find native dependencies in a
@code{vcpkg} tree at build time in order to be used in Cargo build scripts.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-vec-map-0.8
  (package
    (name "rust-vec-map")
    (version "0.8.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "vec_map" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "06n8hw4hlbcz328a3gbpvmy0ma46vg1lc0r5wf55900szf3qdiq5"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/contain-rs/vec-map")
    (synopsis "Simple map based on a vector for small integer keys")
    (description
     "This package provides a simple map based on a vector for small integer keys.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-version-check-0.9
  (package
    (name "rust-version-check")
    (version "0.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "version_check" version))
        (file-name (string-append name "-" version ".crate"))
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
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-version-check-0.1
  (package
    (inherit rust-version-check-0.9)
    (name "rust-version-check")
    (version "0.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "version_check" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1pf91pvj8n6akh7w6j5ypka6aqz08b3qpzgs0ak2kjf4frkiljwi"))))))

(define-public rust-void-1.0
  (package
    (name "rust-void")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "void" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0zc8f0ksxvmhvgx4fdg0zyn6vdnbxd2xv9hfx4nhzg6kbs4f80ka"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/reem/rust-void")
    (synopsis "Void type for use in statically impossible cases")
    (description
     "The uninhabited void type for use in statically impossible cases.")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-walkdir-2.2
  (package
    (name "rust-walkdir")
    (version "2.2.9")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "walkdir" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "07ppalpvxkf8cnqr64np422792y4z5bs9m8b4nrflh5rm17wjn4n"))))
    (build-system cargo-build-system)
    (home-page  "https://github.com/BurntSushi/walkdir")
    (synopsis "Recursively walk a directory")
    (description "Recursively walk a directory.")
    (properties '((hidden? . #t)))
    (license (list license:unlicense
                   license:expat))))

(define-public rust-wasi-0.5
  (package
    (name "rust-wasi")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasi" version))
       (file-name
        (string-append name "-" version ".crate"))
       (sha256
        (base32
         "1ir3pd4phdfml0cbziw9bqp7mnk0vfp9biy8bh25lln6raml4m7x"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/CraneStation/rust-wasi")
    (synopsis "Experimental WASI API bindings for Rust")
    (description "This package contains experimental WASI API bindings
in Rust.")
    (properties '((hidden? . #t)))
    (license license:asl2.0)))

(define-public rust-wasm-bindgen-backend-0.2
  (package
    (name "rust-wasm-bindgen-backend")
    (version "0.2.48")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasm-bindgen-backend" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1qxqkbjkjg4pphhcr91nk95c0gizx77dyq24mmijqnwzxxqc30jx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-bumpalo" ,rust-bumpalo-2.5)
        ("rust-lazy-static" ,rust-lazy-static-1.3)
        ("rust-log" ,rust-log-0.4)
        ("rust-proc-macro2" ,rust-proc-macro2-0.4)
        ("rust-quote" ,rust-quote-1.0)
        ("rust-syn" ,rust-syn-0.15)
        ("rust-wasm-bindgen-shared" ,rust-wasm-bindgen-shared-0.2))))
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis "Backend code generation of the wasm-bindgen tool")
    (description
     "Backend code generation of the wasm-bindgen tool.")
    (license (list license:expat license:asl2.0))))

(define-public rust-wasm-bindgen-shared-0.2
  (package
    (name "rust-wasm-bindgen-shared")
    (version "0.2.48")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-bindgen-shared" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "08rnfhjyk0f6liv8n4rdsvhx7r02glkhcbj2lp9lcbkbfpad9hnr"))))
    (build-system cargo-build-system)
    (home-page "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis "Shared support between wasm-bindgen and wasm-bindgen cli")
    (description "This package provides shared support between
@code{wasm-bindgen} and @code{wasm-bindgen} cli, an internal dependency.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-wasm-bindgen-test-macro-0.2
  (package
    (name "rust-wasm-bindgen-test-macro")
    (version "0.2.48")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wasm-bindgen-test-macro" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0n28mr6vncf1k1qr2b5bvfxq4jvqkjdzq0z0ab6w2f5d6v8q3q3l"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/rustwasm/wasm-bindgen")
    (synopsis "Internal testing macro for wasm-bindgen")
    (description
     "This library contains the internal testing macro for wasm-bindgen.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-widestring-0.4
  (package
    (name "rust-widestring")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "widestring" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1dhx6dndjsz1y7c9w06922412kdxyrrkqblvggm76mh8z17hxz7g"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/starkat99/widestring-rs")
    (synopsis "Wide string Rust FFI library")
    (description
     "A wide string Rust FFI library for converting to and from wide strings,
such as those often used in Windows API or other FFI libraries.  Both UTF-16 and
UTF-32 types are provided, including support for malformed encoding.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-winapi-0.3
  (package
    (name "rust-winapi")
    (version "0.3.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winapi" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1ii9j9lzrhwri0902652awifzx9fpayimbp6hfhhc296xcg0k4w0"))))
    (build-system cargo-build-system)
    ;; This package depends unconditionally on these two crates.
    (arguments
     `(#:cargo-inputs
       (("winapi-i686-pc-windows-gnu" ,rust-winapi-i686-pc-windows-gnu-0.4)
        ("winapi-x86-64-pc-windows-gnu" ,rust-winapi-x86-64-pc-windows-gnu-0.4))))
    (home-page "https://github.com/retep998/winapi-rs")
    (synopsis "Raw FFI bindings for all of Windows API.")
    (description
     "Raw FFI bindings for all of Windows API.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-winapi-0.2
  (package
    (inherit rust-winapi-0.3)
    (name "rust-winapi")
    (version "0.2.8")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winapi" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0yh816lh6lf56dpsgxy189c2ai1z3j8mw9si6izqb6wsjkbcjz8n"))))
    (arguments '())))

(define-public rust-winapi-build-0.1
  (package
    (name "rust-winapi-build")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winapi-build" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1g4rqsgjky0a7530qajn2bbfcrl2v0zb39idgdws9b1l7gp5wc9d"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/retep998/winapi-rs")
    (synopsis "Common code for build.rs in WinAPI -sys crates")
    (description
     "Common code for build.rs in WinAPI -sys crates.")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-winapi-i686-pc-windows-gnu-0.4
  (package
    (name "rust-winapi-i686-pc-windows-gnu")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winapi-i686-pc-windows-gnu" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1dmpa6mvcvzz16zg6d5vrfy4bxgg541wxrcip7cnshi06v38ffxc"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/retep998/winapi-rs")
    (synopsis "Import libraries for the i686-pc-windows-gnu target")
    (description "This crate provides import libraries for the
i686-pc-windows-gnu target.  Please don't use this crate directly, depend on
@code{winapi} instead.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-winapi-util-0.1
  (package
    (name "rust-winapi-util")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winapi-util" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1j839dc6y8vszvrsb7yk0qvs0w6asnahxzbyans37vnsw6vbls3i"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/BurntSushi/winapi-util")
    (synopsis "Dumping ground for high level safe wrappers over winapi")
    (description
     "This package provides a dumping ground for high level safe wrappers over
winapi.")
    (properties '((hidden? . #t)))
    (license (list license:unlicense
                   license:expat))))

(define-public rust-winapi-x86-64-pc-windows-gnu-0.4
  (package
    (name "rust-winapi-x86-64-pc-windows-gnu")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winapi-x86_64-pc-windows-gnu" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0gqq64czqb64kskjryj8isp62m2sgvx25yyj3kpc2myh85w24bki"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/retep998/winapi-rs")
    (synopsis "Import libraries for the x86_64-pc-windows-gnu target")
    (description "This package provides import libraries for the
x86_64-pc-windows-gnu target.  Please don't use this crate directly, depend on
@code{winapi} instead.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-wincolor-1.0
  (package
    (name "rust-wincolor")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "wincolor" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1agaf3hcav113i86912ajnw6jxcy4rvkrgyf8gdj8kc031mh3xcn"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/BurntSushi/termcolor/tree/master/wincolor")
    (synopsis "Windows API for controlling text color in a Windows console")
    (description
     "This package provides a simple Windows specific API for controlling text
color in a Windows console.")
    (properties '((hidden? . #t)))
    (license (list license:unlicense
                   license:expat))))

(define-public rust-winutil-0.1
  (package
    (name "rust-winutil")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "winutil" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0vkyl3fbbf05n5ph5yz8sfaccrk9x3qsr25560w6w68ldf5i7bvx"))))
    (build-system cargo-build-system)
    (home-page "https://bitbucket.org/DaveLancaster/winutil")
    (synopsis "Library wrapping a handful of useful winapi functions")
    (description
     "A simple library wrapping a handful of useful winapi functions.")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-ws2-32-sys-0.2
  (package
    (name "rust-ws2-32-sys")
    (version "0.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "ws2_32-sys" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0ppscg5qfqaw0gzwv2a4nhn5bn01ff9iwn6ysqnzm4n8s3myz76m"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/retep998/winapi-rs")
    (synopsis "Function definitions for the Windows API library ws2_32")
    (description
     "Contains function definitions for the Windows API library ws2_32.")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define-public rust-xattr-0.2
  (package
    (name "rust-xattr")
    (version "0.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "xattr" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0k556fb6f5jc907975j9c8iynl2fqz3rf0w6fiig83i4yi0kfk14"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/Stebalien/xattr")
    (synopsis "Unix extended filesystem attributes")
    (description
     "This package provide a small library for setting, getting, and listing
extended attributes.")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-xdg-2.2
  (package
    (name "rust-xdg")
    (version "2.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "xdg" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "0mws8a0fr3cqk5nh7aq9lmkmhzghvasqy4mhw6nnza06l4d6i2fh"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/whitequark/rust-xdg")
    (synopsis "Store and retrieve files according to XDG specification")
    (description
     "This package provides a library for storing and retrieving files according
to XDG Base Directory specification")
    (properties '((hidden? . #t)))
    (license (list license:asl2.0
                   license:expat))))
