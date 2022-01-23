;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Eric Dvorsak <eric@dvorsak.fr>
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2015, 2016, 2017, 2019 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2017, 2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2017 Ben Sturmfels <ben@sturm.com.au>
;;; Copyright © 2016 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014, 2017 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015, 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2016, 2017, 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2016, 2017, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Danny Milosavljevic <dannym+a@scratchpost.org>
;;; Copyright © 2016, 2017, 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017 Carlo Zancanaro <carlo@zancanaro.id.au>
;;; Copyright © 2018 Tomáš Čech <sleep_walker@gnu.org>
;;; Copyright © 2018, 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2018 Vagrant Cascadian <vagrant@debian.org>
;;; Copyright © 2018 Nam Nguyen <namn@berkeley.edu>
;;; Copyright © 2019 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2019 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2020 Alexandros Theodotou <alex@zrythm.org>
;;; Copyright © 2020 Justus Winter <justus@sequoia-pgp.org>
;;; Copyright © 2020, 2021 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
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

(define-module (gnu packages python-crypto)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (srfi srfi-1))

(define-public python-potr
  (package
    (name "python-potr")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/python-otr/pure-python-otr")
         (commit version)))
       (file-name
        (git-file-name name version))
       (sha256
        (base32 "1hzw6h01fm216nmipyylgz0zybd80w1xsk12m7djycnhqrnrvvv1"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-pycrypto))
    (synopsis "Python OTR Implementation")
    (description "Python OTR is an Off-The-Record Protocol Implementation in
Python.  It does not bind to libotr.")
    (home-page "https://github.com/python-otr/pure-python-otr")
    (license license:lgpl3+)))

(define-public python-base58
  (package
    (name "python-base58")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "base58" version))
       (sha256
        (base32
         "1317ly0db7nnjg5k58f6nqa0svfcvn446xd5bpiyi0bfbczwpl65"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests? (invoke "pytest" "-vv")))))))
    (native-inputs
     (list python-pyhamcrest python-pytest python-pytest-benchmark))
    (home-page "https://github.com/keis/base58")
    (synopsis "Base58 and Base58Check implementation")
    (description "Base58 and Base58Check implementation compatible
with what is used by the Bitcoin network.")
    (license license:expat)))

(define-public python-bcrypt
  (package
    (name "python-bcrypt")
    (version "3.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "bcrypt" version))
       (sha256
        (base32 "0agvzdn7r7jx5y4scl5gjmrmr6njvizwmr9n7h1kmaahdrrc34sv"))))
    (build-system python-build-system)
    (native-inputs
     (list python-pycparser python-pytest))
    (propagated-inputs
     (list python-cffi python-six))
    (home-page "https://github.com/pyca/bcrypt/")
    (synopsis
     "Modern password hashing library")
    (description
     "Bcrypt is a Python module which provides a password hashing method based
on the Blowfish password hashing algorithm, as described in
@url{http://static.usenix.org/events/usenix99/provos.html,\"A Future-Adaptable
Password Scheme\"} by Niels Provos and David Mazieres.")
    (license license:asl2.0)))

(define-public python-passlib
  (package
    (name "python-passlib")
    (version "1.7.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "passlib" version))
       (sha256
        (base32 "015y5qaw9qnxr29lg60dml1g5rbqd4586wy5n8m41ib55gvm1zfy"))))
    (build-system python-build-system)
    (native-inputs
     (list python-nose))
    (propagated-inputs
     (list python-bcrypt))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-PYTHON_EGG_CACHE
           ;; Some tests require access to "$HOME/.cython".
           (lambda _ (setenv "PYTHON_EGG_CACHE" "/tmp"))))))
    (home-page "https://bitbucket.org/ecollins/passlib")
    (synopsis "Comprehensive password hashing framework")
    (description
     "Passlib is a password hashing library for Python 2 & 3, which provides
cross-platform implementations of over 30 password hashing algorithms, as well
as a framework for managing existing password hashes.  It's designed to be
useful for a wide range of tasks, from verifying a hash found in /etc/shadow,
to providing full-strength password hashing for multi-user application.")
    (license license:bsd-3)))

(define-public python-pyblake2
  (package
    (name "python-pyblake2")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyblake2" version))
       (sha256
        (base32
         "0gz9hgznv5zw4qjq43xa56y0yikimx30gffvibxzm0nv5sq7xk2w"))))
    (build-system python-build-system)
    (home-page "https://github.com/dchest/pyblake2")
    (synopsis "BLAKE2 hash function for Python")
    (description "BLAKE2 is a cryptographic hash function, which offers
stronger security while being as fast as MD5 or SHA-1, and comes in two
flavors: @code{BLAKE2b}, optimized for 64-bit platforms and produces digests
of any size between 1 and 64 bytes, and @code{BLAKE2s}, optimized for 8- to
32-bit platforms and produces digests of any size between 1 and 32 bytes.

This package provides a Python interface for BLAKE2.")
    ;; The COPYING file declares it as public domain, with the option to
    ;; alternatively use and redistribute it under a variety of permissive
    ;; licenses. cc0 is explicitly mentioned in setup.py and pyblake2module.c.
    (license (list license:public-domain license:cc0))))

(define-public python-paramiko
  (package
    (name "python-paramiko")
    (version "2.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "paramiko" version))
       (sha256
        (base32 "0dahwq623jnna7gsr9j0mkwr9k2n1pvkapjryhcx508d5jxg8dkz"))))
    (build-system python-build-system)
    (arguments
     `(;; FIXME: Tests require many unpackaged libraries, see dev-requirements.txt.
       #:tests? #f))
    (propagated-inputs
     (list python-bcrypt python-pyasn1 python-pynacl python-cryptography))
    (home-page "https://www.paramiko.org/")
    (synopsis "SSHv2 protocol library")
    (description "Paramiko is a python implementation of the SSHv2 protocol,
providing both client and server functionality.  While it leverages a Python C
extension for low level cryptography (PyCrypto), Paramiko itself is a pure
Python interface around SSH networking concepts.")
    (license license:lgpl2.1+)))

(define-public python-ecdsa
  (package
    (name "python-ecdsa")
    (version "0.17.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ecdsa" version))
       (sha256
        (base32 "1ak8xa2r660d85abrlffp0bqvwdadg9ga4066g856hcy8fxh1xdr"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _ (invoke
                      "pytest"
                      "-vv"
                      "-k"
                      "not test_multithreading_with_interrupts"))))))
    (propagated-inputs
     (list python-six))
    (native-inputs
     (list openssl python-pytest))
    (home-page "https://github.com/warner/python-ecdsa")
    (synopsis "ECDSA cryptographic signature library (pure python)")
    (description
     "This is an easy-to-use implementation of ECDSA cryptography (Elliptic
Curve Digital Signature Algorithm), implemented purely in Python.  With this
library, you can quickly create key pairs (signing key and verifying key), sign
messages, and verify the signatures.  The keys and signatures are very short,
making them easy to handle and incorporate into other protocols.")
    (license license:expat)))

(define-public python2-ecdsa
  (package-with-python2 python-ecdsa))

;;; Pycrypto is abandoned upstream:
;;;
;;; https://github.com/dlitz/pycrypto/issues/173
;;;
;;; TODO Remove this package from GNU Guix.
(define-public python-pycrypto
  (package
    (name "python-pycrypto")
    (version "2.6.1")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "pycrypto" version))
      (patches (search-patches "python-pycrypto-CVE-2013-7459.patch"
                               "python-pycrypto-time-clock.patch"))
      (sha256
       (base32
        "0g0ayql5b9mkjam8hym6zyg6bv77lbh66rv1fyvgqb17kfc1xkpj"))))
    (build-system python-build-system)
    (inputs
     (list python gmp))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-build-env
           ;; pycrypto runs an autoconf configure script behind the scenes
           (lambda _ (setenv "CONFIG_SHELL" (which "bash")) #t)))))
    (home-page "https://www.dlitz.net/software/pycrypto/")
    (synopsis "Cryptographic modules for Python")
    (description
     "Pycrypto is a collection of both secure hash functions (such as SHA256
and RIPEMD160), and various encryption algorithms (AES, DES, RSA, ElGamal,
etc.).  The package is structured to make adding new modules easy.")
    (license license:public-domain)))

(define-public python2-pycrypto
  (let ((pycrypto (package-with-python2 python-pycrypto)))
    (package/inherit pycrypto
      (inputs
       `(("python" ,python-2)
         ,@(alist-delete
            "python"
            (package-inputs pycrypto)))))))

(define-public python-kerberos
  (package
    (name "python-kerberos")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "kerberos" version))
       (sha256
        (base32
         "19663qxmma0i8bfbjc2iwy5hgq0g4pfb75r023v5dps68zfvffgh"))))
    (build-system python-build-system)
    (inputs
     (list mit-krb5))
    (home-page "https://github.com/apple/ccs-pykerberos")
    (synopsis
     "Python Kerberos library used by CalendarServer")
    (description
     "This Python package is a high-level wrapper for Kerberos (GSSAPI)
operations.  The goal is to avoid having to build a module that wraps the
entire Kerberos.framework, and instead offer a limited set of functions that
do what is needed for client/server Kerberos authentication based on
<http://www.ietf.org/rfc/rfc4559.txt>.")
    (license license:asl2.0)))

(define-public python-keyring
  (package
    (name "python-keyring")
    (version "22.0.1")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "keyring" version))
      (sha256
       (base32
        "1pvqc6may03did0iz98gasg7cy4h8ljzs4ibh927bfzda8a3xjws"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest"))
             #t)))))
    (native-inputs
     (list python-toml
           python-pytest
           python-pytest-checkdocs
           python-pytest-cov
           python-pytest-flake8
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-secretstorage))
    (home-page "https://github.com/jaraco/keyring")
    (synopsis "Store and access your passwords safely")
    (description
     "The Python keyring lib provides a easy way to access the system keyring
service from python.  It can be used in any application that needs safe
password storage.")
    ;; "MIT" and PSF dual license
    (properties `((python2-variant . ,(delay python2-keyring))))
    (license license:x11)))

(define-public python2-keyring
  (let ((keyring (package-with-python2
                   (strip-python2-variant python-keyring))))
    (package
      (inherit keyring)
      (name "python2-keyring")
      (version "8.7")
      (source
        (origin
          (method url-fetch)
          (uri (pypi-uri "keyring" version))
          (sha256
           (base32
            "0482rmi2x6p78wl2kz8qzyq21xz1sbbfwnv5x7dggar4vkwxhzfx"))))
      (arguments
       `(#:python ,python-2))
      (native-inputs
       (list python2-pytest python2-pytest-runner python2-setuptools-scm))
      (propagated-inputs
       (list python2-pycrypto)))))

(define-public python-keyrings.alt
  (package
    (name "python-keyrings.alt")
    (version "3.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "keyrings.alt" version))
        (sha256
         (base32
          "0gdjdqpq2hf770p6iwi891mil0vbsdhvy88x0v8b2w4y4b28lcli"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (delete-file "keyrings/alt/_win_crypto.py")
            ;; Rely on python-keyring>20:
            ;; https://github.com/jaraco/keyrings.alt/issues/33
            (substitute* '("keyrings/alt/tests/test_Gnome.py"
                           "keyrings/alt/tests/test_Google.py"
                           "keyrings/alt/tests/test_Windows.py"
                           "keyrings/alt/tests/test_file.py"
                           "keyrings/alt/tests/test_pyfs.py")
              (("keyring.tests.test_backend") "keyring.testing.backend")
              (("keyring.tests.util") "keyring.testing.util"))
            #t))))
    (build-system python-build-system)
    (native-inputs
     (list python-keyring python-pytest python-setuptools-scm))
    (home-page "https://github.com/jaraco/keyrings.alt")
    (synopsis "Alternate keyring implementations")
    (description "Keyrings in this package may have security risks or other
implications.  These backends were extracted from the main keyring project to
make them available for those who wish to employ them, but are discouraged for
general production use.  Include this module and use its backends at your own
risk.")
    (license license:expat)))

(define-public python-certauth
  (package
    (name "python-certauth")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "certauth" version))
       (sha256
        (base32
         "1yxqfb5131wahjyw9pxz03bq476rcfx62s6k53xx4cqbzzgdaqkq"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-pyopenssl python-tldextract))
    (native-inputs
     (list python-pytest-cov))
    (home-page "https://github.com/ikreymer/certauth")
    (synopsis "Certificate authority creation tool")
    (description "This package provides a small library, built on top of
pyOpenSSL, which allows for creating a custom certificate authority (CA)
certificate, and generating on-demand dynamic host certs using that CA
certificate.  It is most useful for use with a man-in-the-middle HTTPS proxy,
for example, for recording or replaying web content.")
    (license license:expat)))

(define-public python-certifi
  (package
    (name "python-certifi")
    (version "2020.12.5")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "certifi" version))
              (sha256
               (base32
                "177mdbw0livdjvp17sz6wsfrc32838m9y59v871gpgv2888raj8s"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f))          ;no tests
    (home-page "https://certifi.io/")
    (synopsis "Python CA certificate bundle")
    (description
     "Certifi is a Python library that contains a CA certificate bundle, which
is used by the Requests library to verify HTTPS requests.")
    (license license:asl2.0)))

(define-public python2-certifi
  (package-with-python2 python-certifi))

(define-public python-cryptography-vectors
  (package
    (name "python-cryptography-vectors")
    (version "3.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cryptography_vectors" version))
       (sha256
        (base32
         "192wix3sr678x21brav5hgc6j93l7ab1kh69p2scr3fsblq9qy03"))))
    (build-system python-build-system)
    (home-page "https://github.com/pyca/cryptography")
    (synopsis "Test vectors for the cryptography package")
    (description
      "This package contains test vectors for the cryptography package.")
    ;; Distributed under either BSD-3 or ASL2.0
    (license (list license:bsd-3 license:asl2.0))))

(define-public python2-cryptography-vectors
  (package-with-python2 python-cryptography-vectors))

(define-public python-cryptography
  (package
    (name "python-cryptography")
    (version "3.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cryptography" version))
       (sha256
        (base32
         "1ribd1vxq9wwz564mg60dzcy699gng54admihjjkgs9dx95pw5vy"))))
    (build-system python-build-system)
    (inputs
     (list openssl))
    (propagated-inputs
     (list python-asn1crypto python-cffi python-six python-idna
           python-iso8601))
    (native-inputs
     (list python-cryptography-vectors python-hypothesis python-pretend
           python-pytz python-pytest))
    (home-page "https://github.com/pyca/cryptography")
    (synopsis "Cryptographic recipes and primitives for Python")
    (description
      "cryptography is a package which provides cryptographic recipes and
primitives to Python developers.  It aims to be the “cryptographic standard
library” for Python.  The package includes both high level recipes, and low
level interfaces to common cryptographic algorithms such as symmetric ciphers,
message digests and key derivation functions.")
    ;; Distributed under either BSD-3 or ASL2.0
    (license (list license:bsd-3 license:asl2.0))
    (properties `((python2-variant . ,(delay python2-cryptography))))))

(define-public python2-cryptography
  (let ((crypto (package-with-python2
                 (strip-python2-variant python-cryptography))))
    (package/inherit crypto
      (arguments
       `(#:python ,python-2
         #:phases
         (modify-phases %standard-phases
           ;; The sanity-check attempts attempts to import the non-existent
           ;; modules "_openssl" and "_padding".
           (delete 'sanity-check))))
      (propagated-inputs
       `(("python2-ipaddress" ,python2-ipaddress)
         ("python2-backport-ssl-match-hostname"
          ,python2-backport-ssl-match-hostname)
         ("python2-enum34" ,python2-enum34)
         ,@(package-propagated-inputs crypto))))))

;; TODO: Make this the default in the next staging cycle.
(define-public python-cryptography-vectors-next
  (package
    (inherit python-cryptography-vectors)
    (version "36.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cryptography_vectors" version))
       (sha256
        (base32
         "166mvhhmgglqai1sjkkb76mpdkad2yykam11d2w44hs2snpr117w"))))))

(define-public python-cryptography-next
  (package
    (inherit python-cryptography)
    (version "36.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cryptography" version))
       (sha256
        (base32
         "0f1n8bvngarhsssm60xc59xfzkh7yqpyyyypaph3v5bs7pfc3rak"))))
    (build-system python-build-system)
    (arguments
     (list
      #:imported-modules (append %cargo-build-system-modules
                                 %python-build-system-modules)
      #:modules `(((guix build cargo-build-system) #:prefix cargo:)
                  ,@%python-build-system-modules
                  (srfi srfi-1)
                  (ice-9 match))
      #:phases
      #~(modify-phases (@ (guix build python-build-system) %standard-phases)
          (add-after 'unpack 'loosen-ouroboros-version
            (lambda _
              (substitute* "src/rust/Cargo.toml"
                (("ouroboros = \"0\\.13\"")
                 "ouroboros = \"0.14\""))))
          (add-before 'build 'configure-cargo
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Hide irrelevant inputs from cargo-build-system so it does
              ;; not try to unpack sanity-check.py, etc.
              (let ((cargo-inputs (filter (match-lambda
                                            ((name . path)
                                             (or (string-prefix? "rust-" name)
                                                 (string=? "gcc" name))))
                                          inputs)))
                (with-directory-excursion "src/rust"
                  ((assoc-ref cargo:%standard-phases 'unpack-rust-crates)
                   #:inputs cargo-inputs
                   #:vendor-dir "guix-vendor")
                  ((assoc-ref cargo:%standard-phases 'configure)
                   #:inputs cargo-inputs)
                  ((assoc-ref cargo:%standard-phases 'patch-cargo-checksums)
                   #:vendor-dir "guix-vendor"))
                (rename-file "src/rust/.cargo" ".cargo"))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "pytest" "-vv" "tests")))))))
    (inputs
     (list openssl
           rust-aliasable-0.1
           rust-asn1-0.8
           rust-asn1-derive-0.8
           rust-autocfg-1
           rust-base64-0.13
           rust-bitflags-1
           rust-cfg-if-0.1
           rust-cfg-if-1
           rust-chrono-0.4
           rust-cloudabi-0.1
           rust-lazy-static-1
           rust-libc-0.2
           rust-indoc-0.3
           rust-indoc-impl-0.3
           rust-inflector-0.11
           rust-instant-0.1
           rust-lock-api-0.4
           rust-num-integer-0.1
           rust-num-traits-0.2
           rust-once-cell-1
           rust-ouroboros-0.14
           rust-ouroboros-macro-0.14
           rust-parking-lot-0.11
           rust-parking-lot-core-0.8
           rust-paste-0.1
           rust-paste-impl-0.1
           rust-pem-1
           rust-proc-macro-error-1
           rust-proc-macro-error-attr-1
           rust-proc-macro-hack-0.5
           rust-proc-macro2-1
           rust-pyo3-0.15
           rust-pyo3-build-config-0.15
           rust-pyo3-macros-0.15
           rust-pyo3-macros-backend-0.15
           rust-quote-1
           rust-redox-syscall-0.1
           rust-scopeguard-1
           rust-smallvec-1
           rust-stable-deref-trait-1
           rust-syn-1
           rust-unicode-xid-0.2
           rust-unindent-0.1
           rust-version-check-0.9
           rust-winapi-0.3))
    (propagated-inputs
     (list python-asn1crypto python-cffi python-six python-idna
           python-iso8601))
    (native-inputs
     (list python-cryptography-vectors-next
           python-hypothesis
           python-pretend
           python-pytz
           python-pytest
           python-pytest-subtests
           python-setuptools-rust
           rust `(,rust "cargo")))
    (properties '())))

(define-public python-pyopenssl
  (package
    (name "python-pyopenssl")
    (version "20.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyOpenSSL" version))
       (sha256
        (base32
         "0labcbh2g0jhgisd79wx9kixmi6fip28096d1xb05fj3jmsiq8sc"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             ;; PyOpenSSL runs tests against a certificate with a fixed
             ;; expiry time.  To ensure successful builds in the future,
             ;; set the time to roughly the release date.
             (invoke "faketime" "2021-05-01" "py.test" "-v" "-k"
                     (string-append
                      ;; This test tries to look up certificates from
                      ;; the compiled-in default path in OpenSSL, which
                      ;; does not exist in the build environment.
                      "not test_fallback_default_verify_paths "
                      ;; This test attempts to make a connection to
                      ;; an external web service.
                      "and not test_set_default_verify_paths "
                      ;; Fails on i686-linux and possibly other 32-bit platforms
                      ;; https://github.com/pyca/pyopenssl/issues/974
                      "and not test_verify_with_time")))))))
    (propagated-inputs
     (list python-cryptography python-six))
    (inputs
     (list openssl))
    (native-inputs
     (list libfaketime python-flaky python-pretend python-pytest))
    (home-page "https://github.com/pyca/pyopenssl")
    (synopsis "Python wrapper module around the OpenSSL library")
    (description
      "PyOpenSSL is a high-level wrapper around a subset of the OpenSSL
library.")
    (license license:asl2.0)))

(define-public python2-pyopenssl
  (package-with-python2 python-pyopenssl))

(define-public python-ed25519
  (package
    (name "python-ed25519")
    (version "1.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "ed25519" version))
        (sha256
          (base32
            "0ahx1nkxa0xis3cw0h5c4fpgv8mq4znkq7kajly33lc3317bk499"))))
    (build-system python-build-system)
    (home-page "https://github.com/warner/python-ed25519")
    (synopsis "Ed25519 public-key signatures")
    (description "Ed25519 public-key signatures")
    (license license:expat)))

(define-public python2-ed25519
  (package-with-python2 python-ed25519))

(define-public python-axolotl-curve25519
  (package
    (name "python-axolotl-curve25519")
    (version "0.4.1.post2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-axolotl-curve25519" version))
       (sha256
        (base32
         "18v3rfyv7xi26fb97nw1xc0l6x8wi0i4xj8dlq4gblpbjxiac187"))))
    (build-system python-build-system)
    (home-page "https://github.com/tgalal/python-axolotl-curve25519")
    (synopsis "Python wrapper for curve25519 library")
    (description "This is a python wrapper for the curve25519 library
with ed25519 signatures.  The C code was pulled from
libaxolotl-android.  At the moment this wrapper is meant for use by
python-axolotl.")
    (license (list license:gpl3    ; Most files
                   license:bsd-3)))) ; curve/curve25519-donna.c

(define-public python2-axolotl-curve25519
  (package-with-python2 python-axolotl-curve25519))

(define-public python-axolotl
  (package
    (name "python-axolotl")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-axolotl" version))
       (sha256
        (base32
         "1bwdp24fmriffwx91aigs9k162albb51iskp23nc939z893q23py"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Don't install tests
         (add-before 'install 'remove-tests
           (lambda _
             (for-each delete-file-recursively
                       '("axolotl/tests" "build/lib/axolotl/tests"))
             #t)))))
    (propagated-inputs
     (list python-axolotl-curve25519 python-cryptography python-protobuf))
    (home-page "https://github.com/tgalal/python-axolotl")
    (synopsis "Python port of libaxolotl-android")
    (description "This is a python port of libaxolotl-android.  This
is a ratcheting forward secrecy protocol that works in synchronous and
asynchronous messaging environments.")
    (license license:gpl3)))

(define-public python2-axolotl
  (package-with-python2 python-axolotl))

;; SlowAES isn't compatible with Python 3.
(define-public python2-slowaes
  (package
    (name "python2-slowaes")
    (version "0.1a1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "slowaes" version))
       (sha256
        (base32
         "02dzajm83a7lqgxf6r3hgj64wfmcxz8gs4nvgxpvj5n19kjqlrc3"))))
    (build-system python-build-system)
    (arguments `(#:python ,python-2))
    (home-page "http://code.google.com/p/slowaes/")
    (synopsis "Implementation of AES in Python")
    (description "This package contains an implementation of AES in Python.
This implementation is slow (hence the project name) but still useful when
faster ones are not available.")
    (license license:asl2.0)))

(define-public python-pyaes
  (package
    (name "python-pyaes")
    (version "1.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyaes" version))
       (sha256
        (base32
         "13vdaff15k0jyfcss4b4xvfgm8xyv0nrbyw5n1qc7lrqbi0b3h82"))))
    (build-system python-build-system)
    (home-page "https://github.com/ricmoo/pyaes")
    (synopsis "Implementation of AES in Python")
    (description "This package contains a pure-Python implementation of the
AES block cipher algorithm and the common modes of operation (CBC, CFB, CTR,
ECB and OFB).")
    (license license:expat)))

(define-public python2-pyaes
  (package-with-python2 python-pyaes))

(define-public python-asn1crypto
  (package
    (name "python-asn1crypto")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/wbond/asn1crypto")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "19abibn6jw20mzi1ln4n9jjvpdka8ygm4m439hplyrdfqbvgm01r"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda _
                      (invoke "python" "run.py" "tests"))))))
    (home-page "https://github.com/wbond/asn1crypto")
    (synopsis "ASN.1 parser and serializer in Python")
    (description "asn1crypto is an ASN.1 parser and serializer with definitions
for private keys, public keys, certificates, CRL, OCSP, CMS, PKCS#3, PKCS#7,
PKCS#8, PKCS#12, PKCS#5, X.509 and TSP.")
    (license license:expat)))

(define-public python2-asn1crypto
  (package-with-python2 python-asn1crypto))

(define-public python-pynacl
  (package
    (name "python-pynacl")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PyNaCl" version))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove spurious dependency on python-wheel, can be removed
           ;; for 1.5.
           (substitute* "setup.py"
             (("\"wheel\"") ""))
           ;; Remove bundled libsodium.
           (delete-file-recursively "src/libsodium")))
       (sha256
        (base32
         "01b56hxrbif3hx8l6rwz5kljrgvlbj7shmmd2rjh0hn7974a5sal"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'use-system-sodium
           (lambda _
             (setenv "SODIUM_INSTALL" "system")))
         (replace 'check
           (lambda _
             (invoke "pytest" "-vv"))))))
    (native-inputs
     (list python-hypothesis python-pytest))
    (propagated-inputs
     (list python-cffi python-six libsodium))
    (home-page "https://github.com/pyca/pynacl/")
    (synopsis "Python bindings to libsodium")
    (description
     "PyNaCl is a Python binding to libsodium, which is a fork of the
Networking and Cryptography library.  These libraries have a stated goal
of improving usability, security and speed.")
    (license license:asl2.0)))

(define-public python-blurhash
  (package
    (name "python-blurhash")
    (version "1.1.4")
    (source
      (origin
        ;; Tests not included in pypi release and releases not tagged in git repo.
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/halcy/blurhash-python")
               (commit "22e081ef1c24da1bb5c5eaa2c1d6649724deaef8")))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1qq6mhydlp7q3na4kmaq3871h43wh3pyfyxr4b79bia73wjdylxf"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (delete-file "setup.cfg")
             (invoke "pytest"))))))
    (native-inputs
     (list python-numpy python-pillow python-pytest))
    (home-page "https://github.com/halcy/blurhash-python")
    (synopsis
     "Pure-Python implementation of the blurhash algorithm")
    (description
     "Pure-Python implementation of the blurhash algorithm.")
    (license license:expat)))

(define-public python-ecpy
  (package
    (name "python-ecpy")
    (version "0.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "ECPy" version))
        (sha256
          (base32
            "1gc3i5s93zq6x1nkaxkq1dvmsc12vmrw0hns9f5s1hcb78ni52c8"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-future))
    (home-page "https://github.com/ubinity/ECPy")
    (synopsis "Pure Python Elliptic Curve Library")
    (description "This package provides a Elliptic Curve Library in pure
Python.")
    (license license:asl2.0)))

(define-public python2-ecpy
  (package-with-python2 python-ecpy))

(define-public python-josepy
  (package
    (name "python-josepy")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "josepy" version))
              (sha256
               (base32
                "11khz8malzrv375b27jjkv66z6z6khdx1v5mkkr4vq16gp3n4p7v"))))
    (build-system python-build-system)
    (arguments
     ;; The tests require flake8 >= 3.5, which is not yet packaged.
     '(#:tests? #f))
    (propagated-inputs
     (list python-cryptography python-pyopenssl python-six))
;; TODO Enable when we have flake8 >= 3.5.
;    (native-inputs
;     `(("python-coverage" ,python-coverage)
;       ("python-flake8" ,python-flake8)
;       ("python-isort" ,python-isort)
;       ("python-mock" ,python-mock)
;       ("python-pytest" ,python-pytest)
;       ("python-pytest-cov" ,python-pytest-cov)
;       ("python-pytest-cache" ,python-pytest-cache)
;       ("python-pytest-flake8" ,python-pytest-flake8)))
    (home-page "https://github.com/certbot/josepy")
    (synopsis "JOSE protocol implementation in Python")
    (description "This package provides a Python implementation of the JOSE
protocol (Javascript Object Signing and Encryption).")
    (license license:asl2.0)))

(define-public python2-josepy
  (package-with-python2 python-josepy))

(define pycryptodome-unbundle-tomcrypt-snippet
  #~(begin
      ;; Unbundle libtomcrypt.
      (delete-file-recursively "src/libtom")
      (substitute* "src/DES.c"
        (("#include \"libtom/tomcrypt_des.c\"")
         "#include <tomcrypt.h>"))
      (substitute* "setup.py"
        (("include_dirs=\\['src/', 'src/libtom/'\\]")
         ;; FIXME: why does '-ltomcrypt' need to be added
         ;; manually, even when 'tomcrypt' is added to 'libraries'?
         ;; This behaviour is not documented at
         ;; <https://docs.python.org/3/extending/building.html>.
         "include_dirs=['src/'], libraries=['tomcrypt', 'tommath'],
 extra_link_args=['-ltomcrypt', '-ltommath']"))))

(define-public python-pycryptodome
  (package
    (name "python-pycryptodome")
    (version "3.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pycryptodome" version))
       (sha256
        (base32
         "1l3a80z3lxcj1q0hzj1d3plavy2d51y4vzcd85zj0zm7yyxrd022"))
       (modules '((guix build utils)))
       (snippet pycryptodome-unbundle-tomcrypt-snippet)))
    (build-system python-build-system)
    (inputs
     (list libtomcrypt libtommath))
    (home-page "https://www.pycryptodome.org")
    (synopsis "Low-level cryptographic Python library")
    (description
     "PyCryptodome is a self-contained Python package of low-level
cryptographic primitives.  It's not a wrapper to a separate C library like
OpenSSL.  To the largest possible extent, algorithms are implemented in pure
Python.  Only the pieces that are extremely critical to performance (e.g.,
block ciphers) are implemented as C extensions.

You are expected to have a solid understanding of cryptography and security
engineering to successfully use these primitives.  You must also be able to
recognize that some are obsolete (e.g., TDES) or even insecure (RC4).

It provides many enhancements over the last release of PyCrypto (2.6.1):

@itemize
@item Authenticated encryption modes (GCM, CCM, EAX, SIV, OCB)
@item Accelerated AES on Intel platforms via AES-NI
@item First-class support for PyPy
@item Elliptic curves cryptography (NIST P-256 curve only)
@item Better and more compact API (nonce and iv attributes for ciphers,
automatic generation of random nonces and IVs, simplified CTR cipher mode, and
more)
@item SHA-3 (including SHAKE XOFs) and BLAKE2 hash algorithms
@item Salsa20 and ChaCha20 stream ciphers
@item scrypt and HKDF
@item Deterministic (EC)DSA
@item Password-protected PKCS#8 key containers
@item Shamir’s Secret Sharing scheme
@item Random numbers get sourced directly from the OS (and not from a CSPRNG
in userspace)
@item Cleaner RSA and DSA key generation (largely based on FIPS 186-4)
@item Major clean-ups and simplification of the code base
@end itemize

This package provides drop-in compatibility with PyCrypto.  It is one of two
PyCryptodome variants, the other being python-pycryptodomex.")
    (license (list license:bsd-2
                   license:public-domain)))) ; code inherited from PyCrypto

(define-public python2-pycryptodome
  (package-with-python2 python-pycryptodome))

(define-public python-pycryptodomex
  (package (inherit python-pycryptodome)
    (name "python-pycryptodomex")
    (version (package-version python-pycryptodome))
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pycryptodomex" version))
       (sha256
        (base32 "0vcd65ylri2a4pdqcc1897jasj7wfmqklj8x3pdynmdvark3d603"))
       (modules '((guix build utils)))
       (snippet pycryptodome-unbundle-tomcrypt-snippet)))
    (description
     "PyCryptodome is a self-contained Python package of low-level
cryptographic primitives.  It's not a wrapper to a separate C library like
OpenSSL.  To the largest possible extent, algorithms are implemented in pure
Python.  Only the pieces that are extremely critical to performance (e.g.,
block ciphers) are implemented as C extensions.

You are expected to have a solid understanding of cryptography and security
engineering to successfully use these primitives.  You must also be able to
recognize that some are obsolete (e.g., TDES) or even insecure (RC4).

It provides many enhancements over the last release of PyCrypto (2.6.1):

@itemize
@item Authenticated encryption modes (GCM, CCM, EAX, SIV, OCB)
@item Accelerated AES on Intel platforms via AES-NI
@item First-class support for PyPy
@item Elliptic curves cryptography (NIST P-256 curve only)
@item Better and more compact API (nonce and iv attributes for ciphers,
automatic generation of random nonces and IVs, simplified CTR cipher mode, and
more)
@item SHA-3 (including SHAKE XOFs) and BLAKE2 hash algorithms
@item Salsa20 and ChaCha20 stream ciphers
@item scrypt and HKDF
@item Deterministic (EC)DSA
@item Password-protected PKCS#8 key containers
@item Shamir’s Secret Sharing scheme
@item Random numbers get sourced directly from the OS (and not from a CSPRNG
in userspace)
@item Cleaner RSA and DSA key generation (largely based on FIPS 186-4)
@item Major clean-ups and simplification of the code base
@end itemize

PyCryptodomex is the stand-alone version of PyCryptodome that no longer
provides drop-in compatibility with PyCrypto.")))

(define-public python-m2crypto
  (package
    (name "python-m2crypto")
    (version "0.35.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "M2Crypto" version))
       (sha256
        (base32 "09yirf3w77w6f49q6nxhrjm9c3a4y9s30s1k09chqrw8zdgx8sjc"))))
    (build-system python-build-system)
    (arguments
     `(;; FIXME: Tests start failing with time due to date checks in TLS
       ;; certificates.
       #:tests? #f))
    (inputs (list openssl))
    (native-inputs (list swig))
    (home-page "https://gitlab.com/m2crypto/m2crypto")
    (synopsis "Python crypto and TLS toolkit")
    (description "@code{M2Crypto} is a complete Python wrapper for OpenSSL
featuring RSA, DSA, DH, EC, HMACs, message digests, symmetric ciphers
(including AES); TLS functionality to implement clients and servers; HTTPS
extensions to Python's httplib, urllib, and xmlrpclib; unforgeable HMAC'ing
AuthCookies for web session management; FTP/TLS client and server; S/MIME;
M2Crypto can also be used to provide TLS for Twisted.  Smartcards supported
through the Engine interface.")
    (properties `((python2-variant . ,(delay python2-m2crypto))))
    (license license:expat)))

(define-public python2-m2crypto
  (let ((m2crypto (package-with-python2
                   (strip-python2-variant python-m2crypto))))
    (package/inherit m2crypto
             (propagated-inputs
              `(("python2-typing" ,python2-typing))))))

(define-public python-pykeepass
  (package
    (name "python-pykeepass")
    (version "3.2.1")
    (source
     (origin
       (method git-fetch)
       ;; Source tarball on PyPI doesn't include tests.
       (uri (git-reference
             (url "https://github.com/libkeepass/pykeepass")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1symxf4ahylynihnp9z4z3lh2vy65ipvg8s4hjrnn936hcaaxghk"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-kdbx-writable
           ;; Tests have to write to the .kdbx files in the test directory.
           (lambda _
             (with-directory-excursion "tests"
               (for-each make-file-writable (find-files "."))
               #t)))
         (add-before 'build 'patch-requirements
           (lambda _
             ;; Update requirements from dependency==version
             ;; to dependency>=version.
             (substitute* "setup.py"
               (("==") ">="))
             #t)))))
    (propagated-inputs
     (list python-argon2-cffi
           python-construct
           python-dateutil
           python-future
           python-lxml
           python-pycryptodomex))
    (home-page "https://github.com/libkeepass/pykeepass")
    (synopsis "Python library to interact with keepass databases")
    (description
     "This library allows you to write entries to a KeePass database.  It
supports KDBX3 and KDBX4.")
    ;; There are no copyright headers in the source code.  The LICENSE file
    ;; indicates GPL3.
    (license license:gpl3+)))

(define-public python-pylibscrypt
  (package
    (name "python-pylibscrypt")
    (version "1.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pylibscrypt" version))
       (sha256
        (base32
         "1b3rgzl6dbzs08vhv41b6y4n5189wv7lr27acxn104hs45745abs"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'hard-code-path-to-libscrypt
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((libscrypt (assoc-ref inputs "libscrypt")))
               (substitute* "pylibscrypt/pylibscrypt.py"
                 (("find_library\\('scrypt'\\)")
                  (string-append "'" libscrypt "/lib/libscrypt.so'")))
               #t))))
       ;; The library can use various scrypt implementations and tests all of
       ;; them.  Since we only provide a single implementation, most tests
       ;; fail.  Simply skip them.
       #:tests? #f))
    ;; FIXME: Using "libscrypt" is the second best choice.  The best one
    ;; requires "hashlib.scrypt", provided by Python 3.6+ built with OpenSSL
    ;; 1.1+.  Use that as soon as Guix provides it.
    (inputs
     (list libscrypt))
    (home-page "https://github.com/jvarho/pylibscrypt")
    (synopsis "Scrypt for Python")
    (description "There are a lot of different scrypt modules for Python, but
none of them have everything that I'd like, so here's one more.  It uses
@code{libscrypt}.")
    (license license:isc)))

(define-public python-libnacl
  (package
    (name "python-libnacl")
    (version "1.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "libnacl" version))
       (sha256
        (base32
         "0srx7i264v4dq9and8y6gpzzhrg8jpxs5iy9ggw4plimfj0rjfdm"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'locate-libsodium
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "libnacl/__init__.py"
               (("/usr/local/lib/libsodium.so")
                (search-input-file inputs "/lib/libsodium.so"))))))))
    (native-inputs
     (list python-pyhamcrest))
    (inputs
     (list libsodium))
    (home-page "https://libnacl.readthedocs.org/")
    (synopsis "Python bindings for libsodium based on ctypes")
    (description "@code{libnacl} is used to gain direct access to the
functions exposed by @code{NaCl} library via @code{libsodium}.  It has
been constructed to maintain extensive documentation on how to use
@code{NaCl} as well as being completely portable.")
    (license license:asl2.0)))

(define-public python-pyotp
  (package
    (name "python-pyotp")
    (version "2.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyotp" version))
       (sha256
        (base32 "0jsqfmx9i7j8z81r4zazv76xzy1fcq8v9s2r4kvx7ajfndq3z2h3"))))
    (build-system python-build-system)
    (home-page "https://github.com/pyauth/pyotp")
    (synopsis "Python One Time Password Library")
    (description
     "PyOTP is a Python library for generating and verifying one-time
passwords.  It can be used to implement two-factor (2FA) or multi-factor
(MFA) authentication methods in web applications and in other systems that
require users to log in.")
    (license license:expat)))

(define-public python-scrypt
  (package
    (name "python-scrypt")
    (version "0.8.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "scrypt" version))
       (sha256
        (base32
         "0hjk71k3mgnl8siikm9lii9im8kv0rb7inkjzx78rnancra48xxr"))))
    (build-system python-build-system)
    (inputs
     (list openssl))
    (home-page "https://bitbucket.org/mhallin/py-scrypt")
    (synopsis "Bindings for the scrypt key derivation function library")
    (description "This is a set of Python bindings for the scrypt key
derivation function.")
    (license license:bsd-2)))

(define-public python-service-identity
  (package
    (name "python-service-identity")
    (version "18.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "service_identity" version))
       (sha256
        (base32
         "0b9f5qiqjy8ralzgwjgkhx82h6h8sa7532psmb8mkd65md5aan08"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-attrs python-pyasn1 python-pyasn1-modules
           python-pyopenssl))
    (home-page "https://service-identity.readthedocs.io/")
    (synopsis "Service identity verification for PyOpenSSL")
    (description
     "@code{service_identity} aspires to give you all the tools you need
for verifying whether a certificate is valid for the intended purposes.
In the simplest case, this means host name verification.  However,
service_identity implements RFC 6125 fully and plans to add other
relevant RFCs too.")
    (license license:expat)))

(define-public python2-service-identity
  (package-with-python2 python-service-identity))

(define-public python-hkdf
  (package
    (name "python-hkdf")
    (version "0.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "hkdf" version))
        (sha256
         (base32
          "1jhxk5vhxmxxjp3zj526ry521v9inzzl8jqaaf0ma65w6k332ak2"))))
    (build-system python-build-system)
    (native-inputs
     (list python-nose))
    (home-page "https://github.com/casebeer/python-hkdf")
    (synopsis "HMAC-based Extract-and-Expand Key Derivation Function (HKDF)")
    (description "This package provides a Python implementation of the HMAC Key
Derivation function (HKDF) defined in RFC 5869.")
    (license license:bsd-2)))

(define-public python-spake2
  (package
    (name "python-spake2")
    (version "0.8")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "spake2" version))
        (sha256
         (base32
          "1x16r7lrbklvfzbacb66qv9iiih6liq1y612dqh2chgf555n2yn1"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-hkdf))
    (home-page "https://github.com/warner/python-spake2")
    (synopsis "SPAKE2 password-authenticated key exchange in Python")
    (description "This package provides a Python implementation of the SPAKE2
Password-Authenticated Key Exchange algorithm.")
    (license license:expat)))

(define-public python-txtorcon
  (package
    (name "python-txtorcon")
    (version "19.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "txtorcon" version))
        (sha256
         (base32
          "0fxzhsc62bhmr730vj9pzallmw56gz6iykvl28a5agrycm0bfc9p"))))
    (build-system python-build-system)
    (arguments
      ;; The tests fail immediately due to a missing file. Reported upstream:
      ;; <https://github.com/meejah/txtorcon/issues/330>
     `(#:tests? #f))
    (propagated-inputs
     (list python-automat
           python-idna
           python-incremental
           python-service-identity
           python-twisted
           python-zope-interface))
    (home-page "https://github.com/meejah/txtorcon")
    (synopsis "Twisted-based Tor controller client")
    (description "This package provides a Twisted-based Tor controller client,
with state-tracking and configuration abstractions.")
    (license license:expat)))

(define-public python-keyutils
  (package
    (name "python-keyutils")
    (version "0.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "keyutils" version))
       (sha256
        (base32
         "0lipygpzhwzzsq2k5imb1jgkmj8y4khxdwhzadjs3bd56g6bmkx9"))))
    (build-system python-build-system)
    (native-inputs
     (list python-pytest python-pytest-runner))
    (inputs
     (list keyutils))
    (arguments
     '(#:tests? #f))
    (home-page "https://github.com/sassoftware/python-keyutils")
    (synopsis "Python bindings for keyutils")
    (description
     "This is a set of python bindings for keyutils, a key management suite
that leverages the infrastructure provided by the Linux kernel for safely
storing and retrieving sensitive information in your programs.")
    (license license:asl2.0)))

(define-public python-mcuboot-imgtool
  (package
    (name "python-mcuboot-imgtool")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuulLabs-OSS/mcuboot")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1m1csyvzq4jx81zg635ssy1n7sc0z539z0myh872ll3nwqx7wa0q"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-broken-test
           (lambda _
             (substitute* "scripts/imgtool/keys/ed25519_test.py"
               (("raw_sign") "sign_digest"))
             #t))
         (add-before 'build 'change-directory
           (lambda _
             (chdir "scripts")
             #t)))))
    (propagated-inputs
     (list python-click python-intelhex python-cryptography))
    (home-page "https://mcuboot.com")
    (synopsis "Tool to securely sign firmware images for booting by MCUboot")
    (description "MCUboot is a secure bootloader for 32-bit MCUs.  This
package provides a tool to securely sign firmware images for booting by
MCUboot.")
    (license license:expat)))

(define-public python-ntlm-auth
  (package
    (name "python-ntlm-auth")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ntlm-auth" version))
       (sha256
        (base32
         "16mavidki4ma5ip8srqalr19gz4f5yn3cnmmgps1fmgfr24j63rm"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-cryptography))
    (home-page "https://github.com/jborean93/ntlm-auth")
    (synopsis
     "Calculates NTLM Authentication codes")
    (description
     "This library handles the low-level details of NTLM authentication for
use in authenticating with a service that uses NTLM.  It will create and parse
the 3 different message types in the order required and produce a base64
encoded value that can be attached to the HTTP header.

The goal of this library is to offer full NTLM support including signing and
sealing of messages as well as supporting MIC for message integrity and the
ability to customise and set limits on the messages sent.  Please see Features
and Backlog for a list of what is and is not currently supported.")
    (license license:expat)))

(define-public python-secretstorage
  (package
    (name "python-secretstorage")
    (version "3.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "SecretStorage" version))
        (sha256
         (base32
          "15ginv4gzxrx77n7517xnvf2jcpqc6ran12s951hc85zlr8nqrpx"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; Tests require a running dbus service.
    (propagated-inputs
     (list python-cryptography python-jeepney))
    (home-page "https://github.com/mitya57/secretstorage")
    (synopsis "Python bindings to FreeDesktop.org Secret Service API")
    (description
     "@code{python-secretstorage} provides a way for securely storing passwords
and other secrets.  It uses D-Bus Secret Service API that is supported by GNOME
Keyring (since version 2.30) and KSecretsService.  SecretStorage supports most
of the functions provided by Secret Service, including creating and deleting
items and collections, editing items, locking and unlocking collections
(asynchronous unlocking is also supported).")
    (license license:bsd-3)))

(define-public python-trustme
  (package
    (name "python-trustme")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "trustme" version))
       (sha256
        (base32 "0v3vr5z6apnfmklf07m45kv5kaqvm6hxrkaqywch57bjd2siiywx"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (invoke "pytest" "-vv"))))))
    (native-inputs
     (list python-more-itertools
           python-pyopenssl
           python-pytest
           python-pytest-cov
           python-service-identity
           python-zipp))
    (propagated-inputs
     (list python-cryptography))
    (home-page "https://github.com/python-trio/trustme")
    (synopsis "Fake a certificate authority for tests")
    (description
     "@code{trustme} is a tiny Python package that does one thing: it gives you
a fake certificate authority (CA) that you can use to generate fake TLS certs to
use in your tests.")
    ;; Either license applies.
    (license (list license:expat license:asl2.0))))

(define-public python-certipy
  (package
    (name "python-certipy")
    (version "0.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "certipy" version))
        (sha256
         (base32
          "0n980gqpzh0fm58h3i4mi2i10wgj606lscm1r5sk60vbf6vh8mv9"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-pyopenssl))
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/LLNL/certipy")
    (synopsis "Utility to create and sign CAs and certificates")
    (description
     "Certipy was made to simplify the certificate creation process.  To that
end, certipy exposes methods for creating and managing certificate authorities,
certificates, signing and building trust bundles.")
    (license license:bsd-3)))

(define-public python-jeepney
  (package
    (name "python-jeepney")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "jeepney" version))
        (sha256
         (base32 "0mw6ch5s4czpmsiwqwhcidgk27858pl8vlvb7acrxjkm4ribcnbx"))))
    (build-system python-build-system)
    (native-inputs
     (list python-testpath python-tornado python-trio python-pytest
           python-pytest-trio))
    (home-page "https://gitlab.com/takluyver/jeepney")
    (synopsis "Low-level, pure Python DBus protocol wrapper")
    (description
     "This is a low-level, pure Python DBus protocol client.  It has an
I/O-free core, and integration modules for different event loops.")
    (license license:expat)))

(define-public python-argon2-cffi
  (package
    (name "python-argon2-cffi")
    (version "20.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "argon2-cffi" version))
        (sha256
         (base32
          "0zgr4mnnm0p4i99023safb0qb8cgvl202nly1rvylk2b7qnrn0nq"))
        (modules '((guix build utils)))
        (snippet '(begin (delete-file-recursively "extras") #t))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (setenv "ARGON2_CFFI_USE_SYSTEM" "1")
             (invoke "python" "setup.py" "build")))
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (invoke "pytest")
             (invoke "python" "-m" "argon2" "--help")
             ;; see tox.ini
             (invoke "python" "-m" "argon2" "-n" "1" "-t" "1" "-m" "8" "-p" "1"))))))
    (propagated-inputs
     (list python-cffi python-six))
    (inputs (list argon2))
    (native-inputs
     (list python-hypothesis python-pytest))
    (home-page "https://argon2-cffi.readthedocs.io/")
    (synopsis "Secure Password Hashes for Python")
    (description
     "Argon2 is a secure password hashing algorithm.  It is designed to have
both a configurable runtime as well as memory consumption.  This means that you
can decide how long it takes to hash a password and how much memory is required.")
    (license license:expat)))

(define-public python-privy
  (package
    (name "python-privy")
    (version "6.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               ;; Releases are untagged
               (url "https://github.com/ofek/privy")
               (commit "2838db3df239797c71bddacc48a4c49a83f35747")))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1m32dh5fqc8cy7jyf1z5fs6zvmdkbq5fi98hr609gbl7s0l0y0i9"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "python" "-m" "pytest"))))))
    (native-inputs
     (list python-pytest))
    (propagated-inputs
     (list python-argon2-cffi python-cryptography))
    (home-page "https://www.dropbox.com/developers")
    (synopsis "Library to password-protect your data")
    (description
     "Privy is a small and fast utility for password-protecting secret
data such as API keys, cryptocurrency wallets, or seeds for digital
signatures.")
    (license (list license:expat license:asl2.0)))) ; dual licensed

(define-public python-pgpy
  (package
    (name "python-pgpy")
    (version "0.5.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "PGPy" version))
        (sha256
         (base32 "11rrq15gmn6qbahli7czflfcngjl7zyybjlvk732my6axnf2d754"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest")))))))
    (native-inputs
     (list python-cryptography
           python-pyasn1
           python-pytest
           python-singledispatch
           python-six
           python-wheel))
    (home-page "https://github.com/SecurityInnovation/PGPy")
    (synopsis "Python implementation of OpenPGP")
    (description
     "Currently, PGPy can load keys and signatures of all kinds in both ASCII
armored and binary formats.

It can create and verify RSA, DSA, and ECDSA signatures, at the moment.  It
can also encrypt and decrypt messages using RSA and ECDH.")
    (license license:bsd-3)))

(define-public python-sop
  (package
    (name "python-sop")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "sop" version))
        (sha256
         (base32
          "0gljyjsdn6hdmwlwwb5g5s0c031p6izamvfxp0d39x60af8k5jyf"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; There are no tests, and unittest throws an error trying
                     ; to find some:
                     ;     TypeError: don't know how to make test from: 0.2.0
    (home-page "https://gitlab.com/dkg/python-sop")
    (synopsis "Stateless OpenPGP Command-Line Interface")
    (description
     "The Stateless OpenPGP Command-Line Interface (or sop) is a
specification that encourages OpenPGP implementors to provide a common,
relatively simple command-line API for purposes of object security.

This Python module helps implementers build such a CLI from any implementation
accessible to the Python interpreter.

It does not provide such an implementation itself -- this is just the
scaffolding for the command line, which should make it relatively easy to
supply a handful of python functions as methods to a class.")
    (license license:expat))) ; MIT license
