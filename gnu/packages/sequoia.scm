;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020, 2021 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2021 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages sequoia)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)  ; glibc
  #:use-module (gnu packages check) ;; python-pytest
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages libffi) ;; python-cffi
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz) ;; python-setuptools
  #:use-module (gnu packages rust)
  #:use-module (gnu packages tls))

(define-public rust-sequoia-autocrypt-0.23
  (package
    (name "rust-sequoia-autocrypt")
    (version "0.23.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sequoia-autocrypt" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0skj0dv15341v470g6w5pggsl0iy27qb8h24rr8k6rq7vxdjxl7g"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build? #t
        #:cargo-inputs
        (("rust-base64" ,rust-base64-0.13)
         ("rust-sequoia-openpgp" ,rust-sequoia-openpgp-1))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-other-crypto-features
           (lambda _
             (substitute* "Cargo.toml"
               (("^crypto-cng =" line) (string-append "# " line))
               (("^crypto-rust =" line) (string-append "# " line))))))))
    (home-page "https://sequoia-pgp.org/")
    (synopsis "Deal with Autocrypt encoded data")
    (description "This crate implements low-level functionality like encoding
and decoding of Autocrypt headers and setup messages.  Note: Autocrypt is more
than just headers; it requires tight integration with the MUA.")
    (license license:lgpl2.0+)))

(define-public rust-sequoia-ipc-0.26
  (package
    (name "rust-sequoia-ipc")
    (version "0.26.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sequoia-ipc" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0xyhz55g1igzjw46f667kqmbbk7pgqy2zf5p13zspr6bwv39s1yk"))))
    (build-system cargo-build-system)
    (arguments
      `(#:skip-build? #t
        #:cargo-inputs
        (("rust-anyhow" ,rust-anyhow-1)
         ("rust-buffered-reader" ,rust-buffered-reader-1)
         ("rust-capnp-rpc" ,rust-capnp-rpc-0.13)
         ("rust-ctor" ,rust-ctor-0.1)
         ("rust-dirs" ,rust-dirs-2)
         ("rust-fs2" ,rust-fs2-0.4)
         ("rust-futures" ,rust-futures-0.3)
         ("rust-lalrpop" ,rust-lalrpop-0.19)
         ("rust-lalrpop-util" ,rust-lalrpop-util-0.19)
         ("rust-lazy-static" ,rust-lazy-static-1)
         ("rust-libc" ,rust-libc-0.2)
         ("rust-memsec" ,rust-memsec-0.6)
         ("rust-rand" ,rust-rand-0.7)
         ("rust-sequoia-openpgp" ,rust-sequoia-openpgp-1)
         ("rust-socket2" ,rust-socket2-0.3)
         ("rust-tempfile" ,rust-tempfile-3)
         ("rust-thiserror" ,rust-thiserror-1)
         ("rust-tokio" ,rust-tokio-0.2)
         ("rust-tokio-util" ,rust-tokio-util-0.3)
         ("rust-winapi" ,rust-winapi-0.3))
        #:cargo-development-inputs
        (("rust-clap" ,rust-clap-2)
         ("rust-quickcheck" ,rust-quickcheck-0.9))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-other-crypto-features
           (lambda _
             (substitute* "Cargo.toml"
               (("^crypto-cng =" line) (string-append "# " line))
               (("^crypto-rust =" line) (string-append "# " line))))))))
    (home-page "https://sequoia-pgp.org/")
    (synopsis "Interprocess communication infrastructure for Sequoia")
    (description "Interprocess communication infrastructure for Sequoia")
    (license license:lgpl2.0+)))

(define-public rust-sequoia-net-0.23
  (package
    (name "rust-sequoia-net")
    (version "0.23.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sequoia-net" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "05gv053zqnb00mrai5hva3i4909hn77bnh4z1g4b29cw5qb52cbl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-http" ,rust-http-0.2)
        ("rust-hyper" ,rust-hyper-0.13)
        ("rust-hyper-tls" ,rust-hyper-tls-0.4)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-native-tls" ,rust-native-tls-0.2)
        ("rust-percent-encoding" ,rust-percent-encoding-2)
        ("rust-sequoia-openpgp" ,rust-sequoia-openpgp-1)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-url" ,rust-url-2)
        ("rust-zbase32" ,rust-zbase32-0.1))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-other-crypto-features
           (lambda _
             (substitute* "Cargo.toml"
               (("^crypto-cng =" line) (string-append "# " line))
               (("^crypto-rust =" line) (string-append "# " line))))))))
    (home-page "https://sequoia-pgp.org/")
    (synopsis "Discover and publish OpenPGP certificates over the network")
    (description "This package provides a crate to access keyservers using the
HKP protocol, and searching and publishing Web Key Directories.")
    (license license:lgpl2.0+)))

(define-public rust-sequoia-openpgp-1
  (package
    (name "rust-sequoia-openpgp")
    (version "1.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sequoia-openpgp" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "1mdprsijszkg2j6jk1iq7q1z9yikq598y12m5zbv94fs37xlx3qm"))
        (modules '((guix build utils)))
        (snippet
         ;; Remove dependencies on rust-crypto and win32-cng
         '(let* ((other-crypto-pkgs
                  (list ;; rust-crypto
                   "aes" "block-modes" "block-padding" "blowfish" "cast5"
                   "cipher" "des" "digest" "eax" "ed25519-dalek"
                   "generic-array" "idea" "md-5" "num-bigint-dig" "rand"
                   "ripemd160" "rsa" "sha-1" "sha2" "twofish" "typenum"
                   "x25519-dalek" "p256" "rand_core" "rand_core" "ecdsa"
                   ;; win32-cng
                   "eax" "winapi" "win-crypto-ng" "ed25519-dalek"
                   "num-bigint-dig"))
                 (pkgs-pattern (pk (string-join
                                    (list "^\\[dependencies\\.("
                                          (string-join other-crypto-pkgs "|")
                                          ")\\]")
                                    ""))))
            (substitute* "Cargo.toml"
              ((pkgs-pattern line name) (string-append "[off." name "]"))
              (("^crypto-cng =" line) (string-append "# " line))
              (("^crypto-rust =" line) (string-append "# " line))
              (("^\\[(target\\.\"cfg\\(windows\\))" line name)
               (string-append "[off." name)))))))
    (build-system cargo-build-system)
    (native-inputs
     (list clang pkg-config))
    (inputs
     (list gmp nettle))
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-backtrace" ,rust-backtrace-0.3)
        ("rust-base64" ,rust-base64-0.13)
        ("rust-buffered-reader" ,rust-buffered-reader-1)
        ("rust-bzip2" ,rust-bzip2-0.4)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-dyn-clone" ,rust-dyn-clone-1)
        ("rust-flate2" ,rust-flate2-1)
        ("rust-idna" ,rust-idna-0.2)
        ("rust-itertools" ,rust-itertools-0.10)
        ("rust-lalrpop" ,rust-lalrpop-0.19)
        ("rust-lalrpop-util" ,rust-lalrpop-util-0.19)
        ("rust-lazy-static" ,rust-lazy-static-1)  ;; 1.4.0
        ("rust-libc" ,rust-libc-0.2)  ;; 0.2.66
        ("rust-memsec" ,rust-memsec-0.6)
        ("rust-nettle" ,rust-nettle-7)
        ("rust-plotters" ,rust-plotters-0.3)
        ("rust-regex" ,rust-regex-1)
        ("rust-regex-syntax" ,rust-regex-syntax-0.6)
        ("rust-sha1collisiondetection" ,rust-sha1collisiondetection-0.2)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-unicode-normalization" ,rust-unicode-normalization-0.1)
        ("rust-xxhash-rust" ,rust-xxhash-rust-0.8))
       #:cargo-development-inputs
       ;; keep the development-inputs to allow running tests easily
       (("rust-criterion" ,rust-criterion-0.3)
        ("rust-quickcheck" ,rust-quickcheck-0.9)
        ("rust-rand" ,rust-rand-0.7)
        ("rust-rpassword" ,rust-rpassword-5))))
    (home-page "https://sequoia-pgp.org/")
    (synopsis "OpenPGP data types and associated machinery")
    (description "This crate aims to provide a complete implementation of
OpenPGP as defined by RFC 4880 as well as some extensions (e.g., RFC 6637,
which describes ECC cryptography) for OpenPGP.  This includes support for
unbuffered message processing.

A few features that the OpenPGP community considers to be deprecated (e.g.,
version 3 compatibility) have been left out.  The developers have also updated
some OpenPGP defaults to avoid foot guns (e.g., they selected modern algorithm
defaults).

This Guix package is built to use the nettle cryptographic library.")
    (license license:lgpl2.0+)))

(define-public rust-sequoia-openpgp-0.9
  (package
    (inherit rust-sequoia-openpgp-1)
    (name "rust-sequoia-openpgp")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sequoia-openpgp" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "007h2pi7lcph5jf5bxjydm7hjwjai33yk6dic3cxknki22lxlkfw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-base64" ,rust-base64-0.9)
        ("rust-buffered-reader" ,rust-buffered-reader-0.9)
        ("rust-bzip2" ,rust-bzip2-0.3)
        ("rust-failure" ,rust-failure-0.1)
        ("rust-flate2" ,rust-flate2-1)
        ("rust-idna" ,rust-idna-0.1)
        ("rust-lalrpop" ,rust-lalrpop-0.17)
        ("rust-lalrpop-util" ,rust-lalrpop-util-0.17)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-memsec" ,rust-memsec-0.5)
        ("rust-nettle" ,rust-nettle-5)
        ("rust-quickcheck" ,rust-quickcheck-0.8)
        ("rust-rand" ,rust-rand-0.6)
        ("rust-sequoia-rfc2822" ,rust-sequoia-rfc2822-0.9)
        ("rust-time" ,rust-time-0.1))))))

(define-public rust-sequoia-rfc2822-0.9
  (package
    (name "rust-sequoia-rfc2822")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sequoia-rfc2822" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1aj34i6862718m162rqfv69fkmvdw063s6ws7hbp42n73gb08p5c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-failure" ,rust-failure-0.1)
        ("rust-lalrpop" ,rust-lalrpop-0.17)
        ("rust-lalrpop-util" ,rust-lalrpop-util-0.17))))
    (home-page "https://sequoia-pgp.org/")
    (synopsis "RFC 2822 name-addr parser")
    (description "Currently, this crate only recognizes the RFC 2822 name-addr
and addr-spec productions, i.e., things of the form: @code{Name (Comment)
<email@@example.org>} and @code{email@@example.org}

Although the above appear simple to parse, RFC 2822's whitespace and comment
rules are rather complex.  This crate implements the whole grammar." )
    (license license:gpl3)))

(define-public sequoia-sq
  (package
    (name "sequoia-sq")
    (version "0.25.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sequoia-sq" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0j26vpp98i7zwwhqsvwj0zknj4s0s0ilfqpynj1vgd5laanhyr0d"))))
    (build-system cargo-build-system)
    (inputs
     (list nettle openssl))
    (native-inputs
     (list clang pkg-config))
    (arguments
     `(#:tests? #f  ;; tests require data-files not provided in the package
       #:install-source? #f
       #:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-buffered-reader" ,rust-buffered-reader-1)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-clap" ,rust-clap-2)
        ("rust-clap" ,rust-clap-2)
        ("rust-itertools" ,rust-itertools-0.9)
        ("rust-rpassword" ,rust-rpassword-5)
        ("rust-sequoia-autocrypt" ,rust-sequoia-autocrypt-0.23)
        ("rust-sequoia-net" ,rust-sequoia-net-0.23)
        ("rust-sequoia-openpgp" ,rust-sequoia-openpgp-1)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-term-size" ,rust-term-size-0.3)
        ("rust-tokio" ,rust-tokio-0.2))
       #:cargo-development-inputs
       (("rust-assert-cli" ,rust-assert-cli-0.6))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-other-crypto-features
           (lambda _
             (substitute* "Cargo.toml"
               (("^crypto-cng =" line) (string-append "# " line))
               (("^crypto-rust =" line) (string-append "# " line))))))))
    (home-page "https://sequoia-pgp.org/")
    (synopsis "Command-line frontend for Sequoia OpenPGP")
    (description "This package provides the command-line frontend for Sequoia
OpenPGP.

This Guix package is built to use the nettle cryptographic library.")
    (license license:lgpl2.0+)))

(define-public sequoia-sqv
  (package
    (name "sequoia-sqv")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sequoia-sqv" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
          (base32 "0nrfjn64fm038x5dssipf7jxi27z741m5n5a7zsa9768zab1hr9d"))))
    (build-system cargo-build-system)
    (inputs
     (list nettle openssl))
    (native-inputs
     (list clang pkg-config))
    (arguments
     `(#:tests? #f  ;; tests require data-files not provided in the package
       #:install-source? #f
       #:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-clap" ,rust-clap-2)
        ("rust-clap" ,rust-clap-2)
        ("rust-sequoia-openpgp" ,rust-sequoia-openpgp-1))
       #:cargo-development-inputs
       (("rust-assert-cli" ,rust-assert-cli-0.6))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-other-crypto-features
           (lambda _
             (substitute* "Cargo.toml"
               (("^crypto-cng =" line) (string-append "# " line))
               (("^crypto-rust =" line) (string-append "# " line))))))))
    (home-page "https://sequoia-pgp.org/")
    (synopsis "Simple OpenPGP signature verification program")
    (description "@code{sqv} verifies detached OpenPGP signatures.  It is a
replacement for @code{gpgv}.  Unlike @code{gpgv}, it can take additional
constraints on the signature into account.

This Guix package is built to use the nettle cryptographic library.")
    (license license:lgpl2.0+)))

(define (sequoia-package-origin version)
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://gitlab.com/sequoia-pgp/sequoia.git")
          (commit (string-append "openpgp/v" version))))
    (sha256
     (base32 "1cq1xgvllbpii5hfl3wlia2ayznpvhv8lq8g8ygwxga86ijg98lq"))
    (file-name (git-file-name "sequoia" version))
    (patches (search-patches "libsequoia-remove-store.patch"
                             "libsequoia-fix-ffi-Makefile.patch"))))

(define-public libsequoia
  (package
    (name "libsequoia")
    (version "0.22.0")
    (source (sequoia-package-origin "1.6.0"))
    (build-system cargo-build-system)
    (outputs '("out" "python"))
    (native-inputs
     (list clang pkg-config python-pytest python-pytest-runner
           python-wrapper))
    (inputs
     (list gmp nettle openssl python python-cffi))
    (arguments
     (list
      #:tests? #f ;; TODO make python tests find the shared object file
      #:cargo-inputs
      `(("rust-anyhow" ,rust-anyhow-1)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-memsec" ,rust-memsec-0.6)
        ("rust-native-tls" ,rust-native-tls-0.2)
        ("rust-proc-macro2" ,rust-proc-macro2-1)  ;; for ffi-macros
        ("rust-quote" ,rust-quote-1)  ;; for ffi-macros
        ("rust-sequoia-ipc" ,rust-sequoia-ipc-0.26)
        ("rust-sequoia-net" ,rust-sequoia-net-0.23)
        ("rust-sequoia-openpgp" ,rust-sequoia-openpgp-1)
        ("rust-sha2" ,rust-sha2-0.8)  ;; for ffi-macros
        ("rust-tokio" ,rust-tokio-1.8))
      #:cargo-development-inputs
      `(("rust-filetime" ,rust-filetime-0.2))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'configure 'set-PREFIX
            (lambda _
              (setenv "PREFIX" #$output)))
          (replace 'build
            (lambda _
              (invoke "make" "-C" "openpgp-ffi" "build-release")
              (invoke "make" "-C" "ffi" "build-release")))
          (delete 'package)  ;; cargo can't package a multi-crate workspace
          (replace 'check
            (lambda* (#:key tests?  #:allow-other-keys)
              (when tests?
                (begin
                  (invoke "make" "-C" "openpgp-ffi" "check")
                  (invoke "make" "-C" "ffi" "check")))))
          (replace 'install
            (lambda _
              (invoke "make" "-C" "openpgp-ffi" "install")
              (invoke "make" "-C" "ffi" "install")))
          (add-after 'configure 'fix-build-environment
            (lambda _
              (delete-file "Cargo.toml")
              (symlink "../.cargo" "openpgp-ffi/.cargo")
              (symlink "../.cargo" "ffi/.cargo")
              (for-each delete-file-recursively
                       (find-files "guix-vendor" "^sequoia-[0-9]+\\.*"
                                   #:directories? #t))))
          (add-after 'unpack 'fix-for-python-output
            (lambda _
              (substitute* "ffi/lang/python/Makefile"
                ;; adjust prefix for python package
                (("PREFIX\\s*\\??=.*")
                (string-append "PREFIX = " (pk #$output:python) "\n"))
                ;; fix rpath to include the main package
                (("\\WLDFLAGS=" text)
                 (string-append text "'-Wl,-rpath=" #$output "/lib '"))
                ;; make setuptools install into the prefix, see
                ;; guix/build/python-build-system.scm for explanation
               (("\\ssetup.py\\s+install\\s")
                " setup.py install --root=/ --single-version-externally-managed "))))
          (add-after 'unpack 'fix-Makefiles
            (lambda _
              (substitute* '("openpgp-ffi/Makefile")
                (("^check-headers: force-build") "check-headers:"))))
          (add-after 'unpack 'remove-other-crypto-features
           (lambda _
             (substitute* '("openpgp-ffi/Cargo.toml" "ffi/Cargo.toml")
               (("^crypto-cng =" line) (string-append "# " line))
               (("^crypto-rust =" line) (string-append "# " line)))))
          (add-after 'unpack 'fix-missing-feature
            (lambda _
              (substitute* '("ffi/Cargo.toml")
               (("^(tokio = .* features = \\[)" line)
                (string-append line "\"net\", ")))))
          (add-after 'unpack 'unbundle-crates
            (lambda _
              (substitute* '("openpgp-ffi/Cargo.toml" "ffi/Cargo.toml")
                (("path = \"\\.\\./(openpgp|store|net|ipc)\",") "")))))))
    (home-page "https://sequoia-pgp.org")
    (synopsis "C/FFI interfaces for Sequoia-PGP")
    (description "This package provides a C and FFI interface to both the
low-level and a high-level API of Sequoia-PGP.

Use with caution: This is an \"unofficial\" package, which are not officially
released, but part of the Sequoia-PGP v1.6.0 archive.  So this package might
even go away.")
    (license license:lgpl2.0+)))

(define-public sequoia
  (package
    (name "sequoia")
    (version "1.6.0")
    (source #f)
    (build-system trivial-build-system)
    (outputs '("out" "python"))
    (inputs
     (list glibc ;; for ldconfig in make-dynamic-linker-cache
           libsequoia
           `(,libsequoia "python")
           sequoia-sq
           sequoia-sqv))
    (arguments
     (list
      #:modules '((guix build utils) (guix build gnu-build-system)
                  (guix build gremlin) (guix elf))
      #:builder
      #~(begin
          (use-modules (guix build utils) (guix build gnu-build-system))
          (let ((make-dynamic-linker-cache
                 (assoc-ref %standard-phases 'make-dynamic-linker-cache))
                (ld.so.cache
                 (string-append #$output "/etc/ld.so.cache")))
            (copy-recursively #$libsequoia #$output)
            (copy-recursively #$sequoia-sq #$output)
            (delete-file ld.so.cache)
            (copy-recursively #$sequoia-sqv #$output)
            (delete-file ld.so.cache)
            (copy-recursively #$libsequoia:python #$output:python)
           (setenv "PATH"
                   (string-append (getenv "PATH") ":" #$glibc "/sbin"))
           (make-dynamic-linker-cache #:outputs %outputs)))))
    (home-page "https://sequoia-pgp.org")
    (synopsis "New OpenPGP implementation (meta-package)")
    (description "Sequoia is a new OpenPGP implementation, written in Rust,
consisting of several Rust crates/packages.  This Guix meta-package combines
these packages into a single one for convenience.  Anyhow, you should not
depend other packages on this one avoid excessive compile-times for users.")
    (license license:lgpl2.0+)))
