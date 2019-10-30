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
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages crates-io))

(define-public rust-cbindgen
  (package
    (name "rust-cbindgen")
    (version "0.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cbindgen" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "1zgamxahlxmg4warzivaa8m1f8d6b45mhznm7n6d7p5l18acdblx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("clap" ,rust-clap-2)
        ("log" ,rust-log-0.4)
        ("proc-macro2" ,rust-proc-macro2-1.0)
        ("quote" ,rust-quote-1.0)
        ("serde" ,rust-serde-1.0)
        ("serde-json" ,rust-serde-json-1.0)
        ("syn" ,rust-syn-1.0)
        ("tempfile" ,rust-tempfile-3.0)
        ("toml" ,rust-toml-0.5))
       #:cargo-development-inputs
       (("ansi-term" ,rust-ansi-term-0.11)
        ("atty" ,rust-atty-0.2)
        ("autocfg" ,rust-autocfg-0.1)
        ("bitflags" ,rust-bitflags-1)
        ("cfg-if" ,rust-cfg-if-0.1)
        ("cloudabi" ,rust-cloudabi-0.0)
        ("fuchsia-cprng" ,rust-fuchsia-cprng-0.1)
        ("itoa" ,rust-itoa-0.4)
        ("libc" ,rust-libc-0.2)
        ("numtoa" ,rust-numtoa-0.1)
        ("rand" ,rust-rand-0.6)
        ("rand-chacha" ,rust-rand-chacha-0.1)
        ("rand-core" ,rust-rand-core-0.3)
        ("rand-hc" ,rust-rand-hc-0.1)
        ("rand-isaac" ,rust-rand-isaac-0.1)
        ("rand-jitter" ,rust-rand-jitter-0.1)
        ("rand-os" ,rust-rand-os-0.1)
        ("rand-pcg" ,rust-rand-pcg-0.1)
        ("rand-xorshift" ,rust-rand-xorshift-0.1)
        ("rdrand" ,rust-rdrand-0.4)
        ("redox-syscall" ,rust-redox-syscall-0.1)
        ("redox-termios" ,rust-redox-termios-0.1)
        ("remove-dir-all" ,rust-remove-dir-all-0.5)
        ("ryu" ,rust-ryu-1.0)
        ("serde-derive" ,rust-serde-derive-1.0)
        ("strsim" ,rust-strsim-0.8)
        ("termion" ,rust-termion-1.5)
        ("textwrap" ,rust-textwrap-0.11)
        ("unicode-width" ,rust-unicode-width-0.1)
        ("unicode-xid" ,rust-unicode-xid-0.2)
        ("vec-map" ,rust-vec-map-0.8)
        ("winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/eqrion/cbindgen/")
    (synopsis "Tool for generating C bindings to Rust code")
    (description
     "This package provides a tool for generating C/C++ bindings to Rust code.")
    (license license:mpl2.0)))
