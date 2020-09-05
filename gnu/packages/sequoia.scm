;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
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
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages check) ;; python-pytest
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages libffi) ;; python-cffi
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz) ;; python-setuptools
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls))

(define-public sequoia
  (package
    (name "sequoia")
    (version "0.17.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/sequoia-pgp/sequoia.git")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1rf9q67qmjfkgy6r3mz1h9ibfmc04r4j8nzacqv2l75x4mwvf6xb"))
       (file-name (git-file-name name version))))
    (build-system cargo-build-system)
    (outputs '("out" "python"))
    (native-inputs
     `(("clang" ,clang)
       ("pkg-config" ,pkg-config)
       ("python-pytest" ,python-pytest)
       ("python-pytest-runner" ,python-pytest-runner)))
    (inputs
     `(("capnproto" ,capnproto)
       ("gmp" ,gmp)
       ("nettle" ,nettle)
       ("openssl" ,openssl)
       ("python" ,python)
       ("python-cffi" ,python-cffi)
       ("sqlite" ,sqlite)))
    (arguments
     `(#:tests? #f ;; building the tests requires 9.7GB total
       #:cargo-inputs
       (("rust-assert-cli" ,rust-assert-cli-0.6)
        ("rust-anyhow" ,rust-anyhow-1.0)
        ("rust-base64" ,rust-base64-0.11)
        ;;("rust-buffered-reader" included
        ("rust-bzip2" ,rust-bzip2-0.3)
        ("rust-capnp" ,rust-capnp-0.10)
        ("rust-capnp-rpc" ,rust-capnp-rpc-0.10)
        ("rust-capnpc" ,rust-capnpc-0.10)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-clap" ,rust-clap-2)
        ("rust-colored" ,rust-colored-1.9.1)
        ("rust-crossterm" ,rust-crossterm-0.13)
        ("rust-ctor" ,rust-ctor-0.1)
        ("rust-dirs" ,rust-dirs-2.0)
        ;;("rust-failure" included
        ("rust-filetime" ,rust-filetime-0.2)
        ("rust-flate2" ,rust-flate2-1.0)
        ("rust-fs2" ,rust-fs2-0.4)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-http" ,rust-http-0.1)
        ("rust-hyper" ,rust-hyper-0.12)
        ("rust-hyper-tls" ,rust-hyper-tls-0.3)
        ("rust-idna" ,rust-idna-0.2)
        ("rust-itertools" ,rust-itertools-0.8)
        ("rust-lalrpop-util" ,rust-lalrpop-util-0.17)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-memsec" ,rust-memsec-0.5)
        ("rust-native-tls" ,rust-native-tls-0.2)
        ("rust-nettle" ,rust-nettle-7)
        ("rust-parity-tokio-ipc" ,rust-parity-tokio-ipc-0.4)
        ("rust-percent-encoding" ,rust-percent-encoding-2.1)
        ("rust-prettytable-rs" ,rust-prettytable-rs-0.8)
        ("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quickcheck" ,rust-quickcheck-0.9)
        ("rust-rand" ,rust-rand-0.7)
        ("rust-regex" ,rust-regex-1)
        ("rust-rusqlite" ,rust-rusqlite-0.19)
        ("rust-structopt" ,rust-structopt-0.3)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-thiserror" ,rust-thiserror-1.0)
        ("rust-tokio" ,rust-tokio-0.1)
        ("rust-tokio-core" ,rust-tokio-core-0.1)
        ("rust-unicode-normalization" ,rust-unicode-normalization-0.1)
        ("rust-url" ,rust-url-2.1)
        ("rust-zbase32" ,rust-zbase32-0.1))
       #:cargo-development-inputs
       (("rust-bindgen" ,rust-bindgen-0.51) ;; FIXME for nettle-sys and rusqlite
        ("rust-lalrpop" ,rust-lalrpop-0.17)
        ("rust-rpassword" ,rust-rpassword-4))
       #:phases
       (modify-phases %standard-phases
         ;; Run make instead of using the rust build system, as
         ;; suggested by the installation instructions
         (replace 'build (lambda _ (invoke "make" "build-release") #t))
         (replace 'check
           (lambda* (#:key tests?  #:allow-other-keys)
             (if tests?
                 (invoke "make" "check")
                 #t)))
         (replace 'install (lambda _ (invoke "make" "install") #t))
         (add-after 'unpack 'adjust-prefix
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "PREFIX" (assoc-ref outputs "out"))
             #t))
         (add-after 'unpack 'fix-fo-python-output
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (pyout (assoc-ref outputs "python")))
               (substitute* "ffi/lang/python/Makefile"
                 ;; adjust prefix for python package
                 (("PREFIX\\s*\\??=.*")
                  (string-append "PREFIX = " pyout "\n"))
                 ;; fix rpath to include the main package
                 (("\\WLDFLAGS=" text)
                  (string-append text "'-Wl,-rpath=" out "/lib '"))
                 ;; make setuptools install into the prefix, see
                 ;; guix/build/python-build-system.scm for explanation
                 (("\\ssetup.py\\s+install\\s")
                  " setup.py install --root=/ --single-version-externally-managed "))
               #t)))
         (add-after 'unpack 'fix-pkgconfig-file-substitutes
           ;; preempt Makefiles replacing PREFIX by pwd
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "ffi/sequoia.pc.in"
                 (("PREFIX") out))
               (substitute* "openpgp-ffi/sequoia-openpgp.pc.in"
                 (("PREFIX") out))
               #t)))
         (add-after 'unpack 'set-missing-env-vars
           (lambda* (#:key inputs #:allow-other-keys)
             ;; FIXME: why do we need to set this here?
             (setenv "LIBCLANG_PATH"
                     (string-append (assoc-ref inputs "clang") "/lib"))
             #t))
         (add-after 'unpack 'unpin-deps
           (lambda _
             ;; As the comment in that file explains, upstream encourages
             ;; unpinning, as the pinned version is only to make sure the crate
             ;; compiles on older versions of rustc
             (substitute* '("openpgp/Cargo.toml" "tool/Cargo.toml")
               (("= \"=") "= \""))
             #t)))))
    (home-page "https://sequoia-pgp.org")
    (synopsis "New OpenPGP implementation")
    (description "Sequoia is a new OpenPGP implementation.  It consists of
several crates, providing both a low-level and a high-level API for dealing
with OpenPGP data.")
    (license license:gpl2+)))
