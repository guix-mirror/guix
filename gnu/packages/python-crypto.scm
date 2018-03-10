;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Eric Dvorsak <eric@dvorsak.fr>
;;; Copyright © 2015, 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2015, 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Ben Sturmfels <ben@sturm.com.au>
;;; Copyright © 2016 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014, 2017 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015, 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2016, 2017 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2016, 2017 ng0 <ng0@infotropique.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2016, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Danny Milosavljevic <dannym+a@scratchpost.org>
;;; Copyright © 2016, 2017 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017 Carlo Zancanaro <carlo@zancanaro.id.au>
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
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (srfi srfi-1))

(define-public python-passlib
  (package
    (name "python-passlib")
    (version "1.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "passlib" version))
       (sha256
        (base32
         "1q2khqpj9rlcgdmkypjdq1kswvhjf72bq0zk2cv669cc2dj8z51x"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-nose" ,python-nose)))
    (propagated-inputs
     `(("python-py-bcrypt" ,python-py-bcrypt)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-PYTHON_EGG_CACHE
           ;; some tests require access to "$HOME/.cython"
           (lambda* _ (setenv "PYTHON_EGG_CACHE" "/tmp") #t)))))
    (home-page "https://bitbucket.org/ecollins/passlib")
    (synopsis
     "Comprehensive password hashing framework")
    (description
     "Passlib is a password hashing library for Python 2 & 3, which provides
cross-platform implementations of over 30 password hashing algorithms, as well
as a framework for managing existing password hashes.  It's designed to be
useful for a wide range of tasks, from verifying a hash found in /etc/shadow,
to providing full-strength password hashing for multi-user application.")
    (license license:bsd-3)))

(define-public python2-passlib
  (package-with-python2 python-passlib))

(define-public python-py-bcrypt
  (package
    (name "python-py-bcrypt")
    (version "0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/p/py-bcrypt/py-bcrypt-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0y6smdggwi5s72v6p1nn53dg6w05hna3d264cq6kas0lap73p8az"))))
    (build-system python-build-system)
    (home-page "https://code.google.com/p/py-bcrypt")
    (synopsis
     "Bcrypt password hashing and key derivation")
    (description
     "A python wrapper of OpenBSD's Blowfish password hashing code.  This
system hashes passwords using a version of Bruce Schneier's Blowfish block
cipher with modifications designed to raise the cost of off-line password
cracking and frustrate fast hardware implementation.  The computation cost of
the algorithm is parametised, so it can be increased as computers get faster.
The intent is to make a compromise of a password database less likely to
result in an attacker gaining knowledge of the plaintext passwords (e.g. using
John the Ripper).")
    ;; "sha2.c" is under BSD-3;
    ;; "blowfish.c" and "bcrypt.c" are under BSD-4;
    ;; the rest is under ISC.
    (license (list license:isc license:bsd-3 license:bsd-4))))

(define-public python2-py-bcrypt
  (package-with-python2 python-py-bcrypt))

(define-public python-paramiko
  (package
    (name "python-paramiko")
    (version "2.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "paramiko" version))
       (sha256
        (base32
         "04734n0wy3hxk6rij4fr29in5jmr70nxpc7pqi2ksbjysfz4kbjz"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (zero? (system* "python" "test.py")))))))
    (propagated-inputs
     `(("python-pyasn1" ,python-pyasn1)
       ("python-cryptography" ,python-cryptography)))
    (home-page "http://www.paramiko.org/")
    (synopsis "SSHv2 protocol library")
    (description "Paramiko is a python implementation of the SSHv2 protocol,
providing both client and server functionality.  While it leverages a Python C
extension for low level cryptography (PyCrypto), Paramiko itself is a pure
Python interface around SSH networking concepts.")
    (license license:lgpl2.1+)))

(define-public python2-paramiko
  (package-with-python2 python-paramiko))

(define-public python-ecdsa
  (package
    (name "python-ecdsa")
    (version "0.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pypi.python.org/packages/source/e/ecdsa/ecdsa-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1yj31j0asmrx4an9xvsaj2icdmzy6pw0glfpqrrkrphwdpi1xkv4"))))
    (build-system python-build-system)
    (inputs
     `(("openssl" ,openssl)))
    (home-page
     "https://github.com/warner/python-ecdsa")
    (synopsis
     "ECDSA cryptographic signature library (pure python)")
    (description
     "This is an easy-to-use implementation of ECDSA cryptography (Elliptic
Curve Digital Signature Algorithm), implemented purely in Python.  With this
library, you can quickly create keypairs (signing key and verifying key), sign
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
      (patches (search-patches "python-pycrypto-CVE-2013-7459.patch"))
      (sha256
       (base32
        "0g0ayql5b9mkjam8hym6zyg6bv77lbh66rv1fyvgqb17kfc1xkpj"))))
    (build-system python-build-system)
    (inputs
     `(("python" ,python)
       ("gmp" ,gmp)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-build-env
           ;; pycrypto runs an autoconf configure script behind the scenes
           (lambda _ (setenv "CONFIG_SHELL" (which "bash")) #t)))))
    (home-page "http://www.pycrypto.org/")
    (synopsis "Cryptographic modules for Python")
    (description
     "Pycrypto is a collection of both secure hash functions (such as SHA256
and RIPEMD160), and various encryption algorithms (AES, DES, RSA, ElGamal,
etc.).  The package is structured to make adding new modules easy.")
    (license license:public-domain)))

(define-public python2-pycrypto
  (let ((pycrypto (package-with-python2 python-pycrypto)))
    (package (inherit pycrypto)
      (inputs
       `(("python" ,python-2)
         ,@(alist-delete
            "python"
            (package-inputs pycrypto)))))))

(define-public python-keyring
  (package
    (name "python-keyring")
    (version "8.7")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "keyring" version))
      (sha256
       (base32
        "0482rmi2x6p78wl2kz8qzyq21xz1sbbfwnv5x7dggar4vkwxhzfx"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-setuptools-scm" ,python-setuptools-scm)))
    (propagated-inputs
     `(("python-pycrypto" ,python-pycrypto)))
    (arguments
     `(#:tests? #f))                      ;TODO: tests require pytest
    (home-page "https://github.com/jaraco/keyring")
    (synopsis "Store and access your passwords safely")
    (description
     "The Python keyring lib provides a easy way to access the system keyring
service from python.  It can be used in any application that needs safe
password storage.")
    ;; "MIT" and PSF dual license
    (license license:x11)))

(define-public python2-keyring
  (package-with-python2 python-keyring))

(define-public python-certifi
  (package
    (name "python-certifi")
    (version "2017.1.23")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "certifi" version))
              (sha256
               (base32
                "1klrzl3hgvcf2mjk00g0k3kk1p2z27vzwnxivwar4vhjmjvpz1w1"))))
    (build-system python-build-system)
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
    (version "2.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cryptography_vectors" version))
       (sha256
        (base32
         "1jm5c33qaz297sf99kz9kw8xi792ap9m6bwf0dfylls8z3rv9i3q"))))
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
    (version "2.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cryptography" version))
       (sha256
        (base32
         "14aj5ipbj1w5kba2hv6323954pachhflfrjhhmkjwssv3hvngng4"))))
    (build-system python-build-system)
    (inputs
     `(("openssl" ,openssl)))
    (propagated-inputs
     `(("python-asn1crypto" ,python-asn1crypto)
       ("python-cffi" ,python-cffi)
       ("python-six" ,python-six)
       ("python-idna" ,python-idna)
       ("python-iso8601" ,python-iso8601)))
    (native-inputs
     `(("python-cryptography-vectors" ,python-cryptography-vectors)
       ("python-hypothesis" ,python-hypothesis)
       ("python-pretend" ,python-pretend)
       ("python-pytz" ,python-pytz)
       ("python-pytest" ,python-pytest)))
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
    (package (inherit crypto)
      (propagated-inputs
       `(("python2-ipaddress" ,python2-ipaddress)
         ("python2-backport-ssl-match-hostname"
          ,python2-backport-ssl-match-hostname)
         ("python2-enum34" ,python2-enum34)
         ,@(package-propagated-inputs crypto))))))

(define-public python-pyopenssl
  (package
    (name "python-pyopenssl")
    (version "17.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyOpenSSL" version))
       (sha256
        (base32
         "0wv78mwsdqbxqwdwllf4maqybhbj3vb8328ia04hnb558sxcy41c"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (zero? (system* "py.test" "-v" "-k"
                             (string-append
                              ;; This test tries to look up certificates from
                              ;; the compiled-in default path in OpenSSL, which
                              ;; does not exist in the build environment.
                              "not test_fallback_default_verify_paths "
                              ;; This test attempts to make a connection to
                              ;; an external web service.
                              "and not test_set_default_verify_paths"))))))))
    (propagated-inputs
     `(("python-cryptography" ,python-cryptography)
       ("python-six" ,python-six)))
    (inputs
     `(("openssl" ,openssl)))
    (native-inputs
     `(("python-flaky" ,python-flaky)
       ("python-pretend" ,python-pretend)
       ("python-pytest" ,python-pytest)))
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
    (version "0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tgalal/python-axolotl-curve25519")
             (commit "e4a9c4de0eae27223200579c58d1f8f6d20637e2")))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32
         "0agap5q0hmvf6cwzjqc05kw53pjgf6942pcivpazksmg1vk400ra"))))
    (build-system python-build-system)
    (arguments
     `(;; Prevent creation of the egg. This works around
       ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=20765
       #:configure-flags '("--root=/")))
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
    (version "0.1.39")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/tgalal/python-axolotl/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (patches (search-patches "python-axolotl-AES-fix.patch"))
       (sha256
        (base32 "0badsgkgz0ir3hqynxzsfjgacppi874syvvmgccc6j164053x6zm"))))
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
     `(("python-axolotl-curve25519" ,python-axolotl-curve25519)
       ("python-dateutil" ,python-dateutil)
       ("python-protobuf" ,python-protobuf)
       ("python-pycrypto" ,python-pycrypto)))
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
    (version "0.22.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/wbond/asn1crypto/archive/"
                            version ".tar.gz"))
        (sha256
         (base32
          "1kn910896l3knmilla1c9ly20q181s43w1ah08lzkbm1h3j6pcz0"))))
    (build-system python-build-system)
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
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PyNaCl" version))
       (modules '((guix build utils)))
       ;; Remove bundled libsodium
       (snippet '(delete-file-recursively "src/libsodium"))
       (sha256
        (base32
         "135gz0020fqx8fbr9izpwyq49aww202nkqacq0cw61xz99sjpx9j"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'use-system-sodium
           (lambda _
             (setenv "SODIUM_INSTALL" "system")
             #t)))))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (propagated-inputs
     `(("python-cffi" ,python-cffi)
       ("python-six" ,python-six)
       ("libsodium" ,libsodium)))
    (home-page "https://github.com/pyca/pynacl/")
    (synopsis "Python bindings to libsodium")
    (description
     "PyNaCl is a Python binding to libsodium, which is a fork of the
Networking and Cryptography library.  These libraries have a stated goal
of improving usability, security and speed.")
    (license license:asl2.0)))

(define-public python2-pgpdump
  (package
    (name "python2-pgpdump")
    (version "1.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pgpdump" version))
       (sha256
        (base32
         "0s4nh8h7qsdj2yf29bspjs1zvxd4lcd11r6g11dp7fppgf2h0iqw"))))
    (build-system python-build-system)

    ;; Currently fails to build with Python 3.
    (arguments `(#:python ,python-2))

    (home-page "https://github.com/toofishes/python-pgpdump")
    (synopsis "Python library for parsing PGP packets")
    (description
     "Python-pgpdump is an OpenPGP packet parser based on
@uref{http://www.mew.org/~kazu/proj/pgpdump/, pgpdump}.  It notably supports:

@itemize
@item signature packets;
@item public key packets;
@item secret key packets;
@item trust, user ID, and user attribute packets;
@item ASCII-armor decoding and CRC check.
@end itemize\n")
    (license license:bsd-3)))

(define-public python2-roca-detect
  (package
    (name "python2-roca-detect")
    (version "1.0.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "roca-detect" version))
       (sha256
        (base32
         "1di4akyw2lf5r8zfwvyhkilz8jv8g4b66rgaqwfabmjwma6gnw27"))))
    (build-system python-build-system)
    (native-inputs
     ;; TODO: apk_parse_ph4, pyjks
     `(("python2-dateutil" ,python2-dateutil)
       ("python2-six" ,python2-six)
       ("python2-cryptography" ,python2-cryptography)
       ("python2-future" ,python2-future)
       ("python2-coloredlogs" ,python2-coloredlogs)
       ("python2-pgpdump" ,python2-pgpdump)))
    (arguments
     `(;; Basic testing routine is quite simple and works with Py3
       ;; but the rest of the code that processes the different
       ;; key formats and extracts the modulus for inspection is
       ;; not yet fully py3 ready.
       #:python ,python-2))
    (home-page "https://github.com/crocs-muni/roca")
    (synopsis "ROCA detection tool")
    (description
     "This tool is related to the paper entitled @i{Return of the
Coppersmith’s Attack: Practical Factorization of Widely Used RSA Moduli}.  It
enables you to test public RSA keys for a presence of the described
vulnerability.  Currently the tool supports the following key formats: X.509
Certificate (DER encoded, PEM encoded), RSA PEM (encoded private key, public
key), SSH public key, ASC-encoded OpenPGP key, APK Android application, LDIFF
file, and more.")
    (license license:gpl3)))

(define-public python-ecpy
  (package
    (name "python-ecpy")
    (version "0.8.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "ECPy" version))
        (sha256
          (base32
            "090fqnj8z0xm44jrfpll7j45r68m6kp7mjr7yxzg93j42h3sj285"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-future" ,python-future)))
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
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "josepy" version))
              (sha256
               (base32
                "1k0ahzzaq2rrjiifwbhbp7vm8z4zk0ipgiqwicil80kzlf6bhj4z"))))
    (build-system python-build-system)
    (arguments
     ;; The tests require pytest >= 3.2, which is not yet packaged.
     '(#:tests? #f))
    (propagated-inputs
     `(("python-cryptography" ,python-cryptography)
       ("python-pyopenssl" ,python-pyopenssl)
       ("python-six" ,python-six)))
;; TODO Enable when we have pytest >= 3.2.
;    (native-inputs
;     `(("python-coverage" ,python-coverage)
;       ("python-flake8" ,python-flake8)
;       ("python-isort" ,python-isort)
;       ("python-mock" ,python-mock)
;       ("python-pytest" ,python-pytest-3.0)
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

(define-public python-pycryptodome
  (package
    (name "python-pycryptodome")
    (version "3.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pycryptodome" version))
       (sha256
        (base32
         "15mc1h5ij1b6hcqvb048kb4k0ip64n2ky0zz0rml2s78ylv7g5dp"))))
    (build-system python-build-system)
    (home-page "https://www.pycryptodome.org")
    (synopsis "Cryptographic library for Python")
    (description "This package provides a cryptographic library for Python.

It brings the following enhancements with respect to the last official version
of PyCrypto:

@itemize
@item Authenticated encryption modes (GCM, CCM, EAX, SIV, OCB)
@item Accelerated AES on Intel platforms via AES-NI
@item First class support for PyPy
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
@item Major clean ups and simplification of the code base
@end itemize\n")
    (license license:bsd-2)))

(define-public python2-pycryptodome
  (package-with-python2 python-pycryptodome))
