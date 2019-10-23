;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages rust-cbindgen)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cargo))

(define rust-ansi-term-0.11
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
    (home-page "https://github.com/ogham/rust-ansi-term")
    (synopsis "Library for ANSI terminal colours and styles")
    (description
     "This is a library for controlling colours and formatting, such as red bold
text or blue underlined text, on ANSI terminals.")
    (properties '((hidden? . #t)))
    (license license:expat)))

(define rust-atty-0.2
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

(define rust-autocfg-0.1
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

(define rust-bitflags-1.1
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

(define rust-cfg-if-0.1
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

(define rust-clap-2
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

(define rust-cloudabi-0.0
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

(define rust-fuchsia-cprng-0.1
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
    (description "Thix package provides a rust crate for the Fuchsia
cryptographically secure pseudorandom number generator.")
    (properties '((hidden? . #t)))
    (license license:bsd-3)))

(define rust-itoa-0.4
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
