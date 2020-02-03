;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020 John Soo <jsoo1@asu.edu>
;;; Copyright © 2019, 2020 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages rust-apps)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control))

(define-public ripgrep
  (package
    (name "ripgrep")
    (version "11.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ripgrep" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0bn40lz9n08llq615p4qqqbi24zbkf0appfx3zgxg34a86ga9zds"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bstr" ,rust-bstr-0.1)
        ("rust-clap" ,rust-clap-2)
        ("rust-grep" ,rust-grep-0.2)
        ("rust-ignore" ,rust-ignore-0.4)
        ("rust-lazy-static" ,rust-lazy-static-1.3)
        ("rust-log" ,rust-log-0.4)
        ("rust-num-cpus" ,rust-num-cpus-1.10)
        ("rust-regex" ,rust-regex-1.1)
        ("rust-serde-json" ,rust-serde-json-1.0)
        ("rust-termcolor" ,rust-termcolor-1.0))
       #:cargo-development-inputs
       (("rust-clap" ,rust-clap-2)
        ("rust-lazy-static" ,rust-lazy-static-1.3)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-serde-derive" ,rust-serde-derive-1.0))))
    (home-page "https://github.com/BurntSushi/ripgrep")
    (synopsis "Line-oriented search tool")
    (description
     "ripgrep is a line-oriented search tool that recursively searches
your current directory for a regex pattern while respecting your
gitignore rules.")
    (license (list license:unlicense license:expat))))

(define-public rust-cbindgen
  (package
    (name "rust-cbindgen")
    (version "0.12.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cbindgen" version))
        (file-name (string-append name "-" version ".crate"))
        (sha256
         (base32
          "13jzbmjz1bmmfr0i80hw6ar484mgabx3hbpb2ynhk0ddqi0yr58m"))))
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
        ("toml" ,rust-toml-0.5))))
    (home-page "https://github.com/eqrion/cbindgen/")
    (synopsis "Tool for generating C bindings to Rust code")
    (description
     "This package provides a tool for generating C/C++ bindings to Rust code.")
    (license license:mpl2.0)))

(define-public tokei
  (package
    (name "tokei")
    (version "10.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tokei" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "07f5laqw2k9l3k8wrg9h8p2m5d9hkfxngyacwrn3vs7mlnw8l81m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-clap" ,rust-clap-2)
        ("rust-crossbeam-channel" ,rust-crossbeam-channel-0.4)
        ("rust-dirs" ,rust-dirs-2.0)
        ("rust-encoding-rs-io" ,rust-encoding-rs-io-0.1)
        ("rust-env-logger" ,rust-env-logger-0.7)
        ("rust-grep-searcher" ,rust-grep-searcher-0.1)
        ("rust-hex" ,rust-hex-0.4)
        ("rust-ignore" ,rust-ignore-0.4)
        ("rust-log" ,rust-log-0.4)
        ("rust-rayon" ,rust-rayon-1.3)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-serde-cbor" ,rust-serde-cbor-0.10)
        ("rust-serde-derive" ,rust-serde-derive-1.0)
        ("rust-serde-json" ,rust-serde-json-1.0)
        ("rust-serde-yaml" ,rust-serde-yaml-0.8)
        ("rust-term-size" ,rust-term-size-0.3)
        ("rust-toml" ,rust-toml-0.5))
       #:cargo-development-inputs
       (("rust-git2" ,rust-git2-0.11)
        ("rust-handlebars" ,rust-handlebars-2.0)
        ("rust-ignore" ,rust-ignore-0.4)
        ("rust-lazy-static" ,rust-lazy-static-1.4)
        ("rust-regex" ,rust-regex-1.3)
        ("rust-serde-json" ,rust-serde-json-1.0)
        ("rust-tempfile" ,rust-tempfile-3.0))
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'unvendor-libraries-from-crates
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((openssl (assoc-ref inputs "openssl")))
               (setenv "OPENSSL_DIR" openssl))
             #t)))))
    (native-inputs
     `(("libgit2" ,libgit2)
       ("openssl" ,openssl)
       ("pkg-config" ,pkg-config)
       ("zlib" ,zlib)))
    (home-page "https://tokei.rs")
    (synopsis "Count code, quickly")
    (description
     "Tokei is a program that displays statistics about your code.  Tokei will
show number of files, total lines within those files and code, comments, and
blanks grouped by language.")
    (license (list license:expat license:asl2.0))))
