;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
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
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
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
  #:use-module (gnu packages rust)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls))

(define-public sequoia
  (package
    (name "sequoia")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/sequoia-pgp/sequoia.git")
             (commit (string-append "v" version))))
       (sha256
        (base32 "0y80bl786m29ww3272qsl1ql0xc3pwd6iiqlkv3nmhnjsmygbn0d"))
       (file-name (git-file-name name version))))
    (build-system cargo-build-system)
    (outputs '("out" "python"))
    (native-inputs
     `(("clang" ,clang)
       ("pkg-config" ,pkg-config)
       ("python-pytest" ,python-pytest)
       ("python-pytest-runner" ,python-pytest-runner)
       ("python-wrapper" ,python-wrapper)))
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
       #:rust ,rust-1.46
       #:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-backtrace" ,rust-backtrace-0.3)
        ("rust-base64" ,rust-base64-0.13)
        ;;("rust-buffered-reader" included
        ("rust-bzip2" ,rust-bzip2-0.4)
        ("rust-capnp" ,rust-capnp-0.13)
        ("rust-capnp-rpc" ,rust-capnp-rpc-0.13)
        ("rust-capnpc" ,rust-capnpc-0.13)
        ("rust-chrono" ,rust-chrono-0.4)  ;; for sqv, sop
        ("rust-clap" ,rust-clap-2)        ;; for sqv
        ("rust-colored" ,rust-colored-1.9.1)
        ("rust-crossterm" ,rust-crossterm-0.13)
        ("rust-ctor" ,rust-ctor-0.1)
        ("rust-dirs" ,rust-dirs-2)
        ("rust-dyn-clone" ,rust-dyn-clone-1)
        ("rust-ed25519-dalek" ,rust-ed25519-dalek-1)
        ("rust-eax" ,rust-eax-0.3)
        ;;("rust-failure" included
        ("rust-filetime" ,rust-filetime-0.2)
        ("rust-flate2" ,rust-flate2-1)
        ("rust-fs2" ,rust-fs2-0.4)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-http" ,rust-http-0.2)
        ("rust-hyper" ,rust-hyper-0.13)
        ("rust-hyper-tls" ,rust-hyper-tls-0.4)
        ("rust-idna" ,rust-idna-0.2)
        ("rust-itertools" ,rust-itertools-0.9) ;; for sq
        ("rust-lalrpop" ,rust-lalrpop-0.19)
        ("rust-lalrpop-util" ,rust-lalrpop-util-0.19)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-memsec" ,rust-memsec-0.6)
        ("rust-native-tls" ,rust-native-tls-0.2)
        ("rust-nettle" ,rust-nettle-7)
        ("rust-num-bigint-dig" ,rust-num-bigint-dig-0.6)
        ("rust-parity-tokio-ipc" ,rust-parity-tokio-ipc-0.4)
        ("rust-percent-encoding" ,rust-percent-encoding-2)
        ("rust-prettytable-rs" ,rust-prettytable-rs-0.8)  ;; for sq
        ("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quickcheck" ,rust-quickcheck-0.9)
        ("rust-rand" ,rust-rand-0.7)
        ("rust-regex" ,rust-regex-1)
        ("rust-rusqlite" ,rust-rusqlite-0.24)
        ("rust-sha1collisiondetection" ,rust-sha1collisiondetection-0.2)
        ("rust-socket2", rust-socket2-0.3)
        ("rust-structopt" ,rust-structopt-0.3) ;; for sop
        ("rust-tempfile" ,rust-tempfile-3) ;; for sq
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-tokio" ,rust-tokio-0.2)
        ("rust-tokio-core" ,rust-tokio-core-0.1)
        ("rust-unicode-normalization" ,rust-unicode-normalization-0.1)
        ("rust-url" ,rust-url-2)
        ("rust-win-crypto-ng" ,rust-win-crypto-ng-0.4)
        ("rust-winapi" ,rust-winapi-0.3)
        ("rust-zbase32" ,rust-zbase32-0.1))
       #:cargo-development-inputs
       (("rust-assert-cli" ,rust-assert-cli-0.6) ;; dev-dep for for sq, sqv
        ("rust-bindgen" ,rust-bindgen-0.51) ;; FIXME for nettle-sys and rusqlite
        ;;("rust-lalrpop" ,rust-lalrpop-0.19)
        ("rust-quickcheck" ,rust-quickcheck-0.9)
        ("rust-rand" ,rust-rand-0.7)
        ("rust-rpassword" ,rust-rpassword-5))
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
         (add-after 'unpack 'fix-environment
           (lambda* (#:key outputs #:allow-other-keys)
             ;; adjust prefix
             (setenv "PREFIX" (assoc-ref outputs "out"))
             ;; fix install script detection
             (setenv "INSTALL" "install")
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
         (add-after 'unpack 'keep-SOURCE_DATE_EPOCH
           (lambda _
               ;; preempt Makefiles replacing SOURCE_DATE_EPOCH
               (substitute* "Makefile"
                 (("SOURCE_DATE_EPOCH\\s=" line)
                  (string-append "#" line)))
             #t))
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
             (substitute* '("openpgp/Cargo.toml" "sq/Cargo.toml")
               (("= \"=") "= \""))
             #t)))))
    (home-page "https://sequoia-pgp.org")
    (synopsis "New OpenPGP implementation")
    (description "Sequoia is a new OpenPGP implementation.  It consists of
several crates, providing both a low-level and a high-level API for dealing
with OpenPGP data.")
    (license license:gpl2+)))

(define-public sequoia4pEp
  ;; Currently pEp Engine requires sequoia in not-so-current version
  (package/inherit sequoia
    (name "sequoia")
    (version "0.15.0-pEp")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/sequoia-pgp/sequoia.git")
             (commit "0eb1b6cd846ea8c36b3dfdf01ec88383fc64f2fe")))
       (sha256
        (base32 "06dqs9whwp9lfibwp8dqm0aw4nm3s3v4jp2n4fz51zcvsld40nfh"))
       (file-name (git-file-name name version))))
    (arguments
     (substitute-keyword-arguments (package-arguments sequoia)
       ((#:cargo-inputs _)
        `(("rust-anyhow" ,rust-anyhow-1)
          ("rust-base64" ,rust-base64-0.11)
          ("rust-bzip2" ,rust-bzip2-0.3)
          ("rust-capnp" ,rust-capnp-0.10)
          ("rust-capnp-rpc" ,rust-capnp-rpc-0.10)
          ("rust-chrono" ,rust-chrono-0.4)
          ("rust-clap" ,rust-clap-2)
          ("rust-crossterm" ,rust-crossterm-0.13)
          ("rust-dirs" ,rust-dirs-2)
          ("rust-flate2" ,rust-flate2-1)
          ("rust-fs2" ,rust-fs2-0.4)
          ("rust-futures" ,rust-futures-0.1)
          ("rust-http" ,rust-http-0.1)
          ("rust-hyper" ,rust-hyper-0.12)
          ("rust-hyper-tls" ,rust-hyper-tls-0.3)
          ("rust-idna" ,rust-idna-0.2)
          ("rust-itertools" ,rust-itertools-0.8)
          ("rust-lalrpop" ,rust-lalrpop-0.17)
          ("rust-lalrpop-util" ,rust-lalrpop-util-0.17)
          ("rust-lazy-static" ,rust-lazy-static-1)
          ("rust-libc" ,rust-libc-0.2)
          ("rust-memsec" ,rust-memsec-0.5)
          ("rust-native-tls" ,rust-native-tls-0.2)
          ("rust-nettle" ,rust-nettle-7)
          ("rust-percent-encoding" ,rust-percent-encoding-2)
          ("rust-prettytable-rs" ,rust-prettytable-rs-0.8)
          ("rust-proc-macro2" ,rust-proc-macro2-1)
          ("rust-quickcheck" ,rust-quickcheck-0.9)
          ("rust-quote" ,rust-quote-1)
          ("rust-rand" ,rust-rand-0.7)
          ("rust-regex" ,rust-regex-1)
          ("rust-rpassword" ,rust-rpassword-4)
          ("rust-rusqlite" ,rust-rusqlite-0.19)
          ("rust-sha2" ,rust-sha2-0.8)
          ("rust-syn" ,rust-syn-1)
          ("rust-tempfile" ,rust-tempfile-3)
          ("rust-thiserror" ,rust-thiserror-1)
          ("rust-tokio" ,rust-tokio-0.1)
          ("rust-tokio-core" ,rust-tokio-core-0.1)
          ("rust-tokio-io" ,rust-tokio-io-0.1)
          ("rust-unicode-normalization" ,rust-unicode-normalization-0.1)
          ("rust-url" ,rust-url-2)
          ("rust-zbase32" ,rust-zbase32-0.1)))
       ((#:cargo-development-inputs _)
        `(("rust-assert-cli" ,rust-assert-cli-0.6)
          ("rust-colored" ,rust-colored-1)
          ("rust-filetime" ,rust-filetime-0.2)))
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'unpin-deps
             (lambda _
               (substitute* (find-files "." "Cargo.toml")
                 (("= \"<") "= \"")
                 (("= \"=") "= \""))
               #t))))))
    (properties `((hidden? . #t)))))
