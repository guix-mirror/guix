;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2015, 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 ng0 <ng0@infotropique.org>
;;; Copyright © 2017 rsiddharth <s@ricketyspace.net>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages haskell-crypto)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-check)
  #:use-module (guix build-system haskell)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public ghc-asn1-types
  (package
    (name "ghc-asn1-types")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "asn1-types/asn1-types-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "05vjchyqiy9n275cygffhn0ma7fz7jx52j0dcdm9qm8h9bziymqc"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-memory" ,ghc-memory)
       ("ghc-hourglass" ,ghc-hourglass)))
    (home-page "https://github.com/vincenthz/hs-asn1-types")
    (synopsis "ASN.1 types for Haskell")
    (description
     "The package provides the standard types for dealing with the ASN.1
format.")
    (license license:bsd-3)))

(define-public ghc-asn1-encoding
  (package
    (name "ghc-asn1-encoding")
    (version "0.9.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "asn1-encoding/asn1-encoding-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0adgbamyq0mj1l1hdq4zyyllay714bac1wl0rih3fv1z6vykp1hy"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-hourglass" ,ghc-hourglass)
       ("ghc-asn1-types" ,ghc-asn1-types)
       ("ghc-text" ,ghc-text)
       ("ghc-mtl" ,ghc-mtl)))
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)))
    (home-page "https://github.com/vincenthz/hs-asn1")
    (synopsis "ASN1 data reader and writer in RAW, BER and DER forms")
    (description
     "This package provides a reader and writer for ASN1 data in raw form with
supports for high level forms of ASN1 (BER, and DER).")
    (license license:bsd-3)))

(define-public ghc-asn1-parse
  (package
    (name "ghc-asn1-parse")
    (version "0.9.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "asn1-parse/asn1-parse-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "025prsihk5g6rdv9xlfmj0zpa0wa3qjzj5i4ilzvg7f6f3sji8y6"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-asn1-types" ,ghc-asn1-types)
       ("ghc-asn1-encoding" ,ghc-asn1-encoding)))
    (home-page "https://github.com/vincenthz/hs-asn1")
    (synopsis "Simple monadic parser for ASN1 stream types")
    (description
     "This package provides a simple monadic parser for ASN1 stream types,
when ASN1 pattern matching is not convenient.")
    (license license:bsd-3)))

(define-public ghc-crypto-api
  (package
    (name "ghc-crypto-api")
    (version "0.13.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "crypto-api-" version "/"
                           "crypto-api-" version ".tar.gz"))
       (sha256
        (base32
         "1vc27qcgbg7hf50rkqhlrs58zn1888ilh4b6wrrm07bnm48xacak"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-cereal" ,ghc-cereal)
              ("ghc-tagged" ,ghc-tagged)
              ("ghc-entropy" ,ghc-entropy)))
    (home-page "https://github.com/TomMD/crypto-api")
    (synopsis "Provides generic interface for cryptographic operations
for Haskell")
    (description "This Haskell package provides a generic interface for
cryptographic operations (hashes, ciphers, randomness).

Maintainers of hash and cipher implementations are encouraged to add instances
for the classes defined in @code{Crypto.Classes}.  @code{Crypto} users are
similarly encouraged to use the interfaces defined in the @code{Classes} module.

Any concepts or functions of general use to more than one cryptographic
algorithm (ex: padding) is within scope of this package.")
    (license license:bsd-3)))

(define-public ghc-crypto-api-tests
  (package
    (name "ghc-crypto-api-tests")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "crypto-api-tests-" version "/"
                           "crypto-api-tests-" version ".tar.gz"))
       (sha256
        (base32
         "0w3j43jdrlj28jryp18hc6q84nkl2yf4vs1hhgrsk7gb9kfyqjpl"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)
              ("ghc-crypto-api" ,ghc-crypto-api)
              ("ghc-cereal" ,ghc-cereal)
              ("ghc-test-framework" ,ghc-test-framework)
              ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
              ("ghc-hunit" ,ghc-hunit)
              ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://github.com/TomMD/crypto-api-tests")
    (synopsis "Test framework and KATs for cryptographic operations for Haskell")
    (description "This Haskell package provides a test framework for hash and
cipher operations using the crypto-api interface.  Known answer tests (KATs)
for common cryptographic algorithms are included.")
    (license license:bsd-3)))

(define-public ghc-cryptohash
  (package
    (name "ghc-cryptohash")
    (version "0.11.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/cryptohash/cryptohash-"
             version ".tar.gz"))
       (sha256
        (base32
         "1yr2iyb779znj79j3fq4ky8l1y8a600a2x1fx9p5pmpwq5zq93y2"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-byteable" ,ghc-byteable)
       ("ghc-cryptonite" ,ghc-cryptonite)
       ("ghc-memory" ,ghc-memory)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)))
    (home-page "https://github.com/vincenthz/hs-cryptohash")
    (synopsis "Collection of cryptographic hashes in Haskell")
    (description
     "A collection of crypto hashes, with a practical incremental and one-pass,
pure APIs, with performance close to the fastest implementations available in
other languages.  The implementations are made in C with a haskell FFI wrapper
that hides the C implementation.")
    (license license:bsd-3)))

(define-public ghc-cryptohash-md5
  (package
    (name "ghc-cryptohash-md5")
    (version "0.11.100.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "cryptohash-md5-" version "/"
                           "cryptohash-md5-" version ".tar.gz"))
       (sha256
        (base32
         "1y8q7s2bn4gdknw1wjikdnar2b5pgz3nv3220lxrlgpsf23x82vi"))))
    (build-system haskell-build-system)
    (arguments
     `(#:tests? #f)) ; tests require old version of ghc-hunit (0.9)
    (native-inputs `(("ghc-base16-bytestring" ,ghc-base16-bytestring)
                     ("ghc-puremd5" ,ghc-puremd5)
                     ("ghc-tasty" ,ghc-tasty)
                     ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
                     ("ghc-hunit" ,ghc-hunit)))
    (home-page "https://github.com/hvr/cryptohash-md5")
    (synopsis "MD5 implementation for Haskell")
    (description "This Haskell package provides implementation of MD5.")
    (license license:bsd-3)))

(define-public ghc-cryptohash-sha1
  (package
    (name "ghc-cryptohash-sha1")
    (version "0.11.100.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "cryptohash-sha1-" version "/"
                           "cryptohash-sha1-" version ".tar.gz"))
       (sha256
        (base32
         "1aqdxdhxhl9jldh951djpwxx8z7gzaqspxl7iwpl84i5ahrsyy9w"))))
    (build-system haskell-build-system)
    (arguments
     `(#:tests? #f)) ; tests require old version of ghc-hunit (0.9)
    (native-inputs `(("ghc-base16-bytestring" ,ghc-base16-bytestring)
                     ("ghc-sha" ,ghc-sha)
                     ("ghc-tasty" ,ghc-tasty)
                     ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
                     ("ghc-hunit" ,ghc-hunit)))
    (home-page "https://github.com/hvr/cryptohash-sha1")
    (synopsis "SHA-1 implementation for Haskell")
    (description "This Haskell package provides an incremental and one-pass,
pure API to the @uref{https://en.wikipedia.org/wiki/SHA-1, SHA-1 hash algorithm},
including @uref{https://en.wikipedia.org/wiki/HMAC, HMAC support}, with
performance close to the fastest implementations available in other languages.

The implementation is made in C with a haskell FFI wrapper that hides
the C implementation.")
    (license license:bsd-3)))

(define-public ghc-cryptonite
  (package
    (name "ghc-cryptonite")
    (version "0.19")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "cryptonite/cryptonite-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1rw5k34vpn4134yrzdhf0rsshsrkixfbv9ap18di2n00z2cw1shw"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-memory" ,ghc-memory)
       ("ghc-byteable" ,ghc-byteable)))
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-tasty-kat" ,ghc-tasty-kat)))
    (home-page "https://github.com/haskell-crypto/cryptonite")
    (synopsis "Cryptography primitives")
    (description
     "This package is a repository of cryptographic primitives for Haskell.
It supports a wide range of symmetric ciphers, cryptographic hash functions,
public key algorithms, key derivation numbers, cryptographic random number
generators, and more.")
    (license license:bsd-3)))

(define-public ghc-digest
  (package
    (name "ghc-digest")
    (version "0.0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/digest/digest-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "04gy2zp8yzvv7j9bdfvmfzcz3sqyqa6rwslqcn4vyair2vmif5v4"))))
    (build-system haskell-build-system)
    (inputs
     `(("zlib" ,zlib)))
    (home-page
     "https://hackage.haskell.org/package/digest")
    (synopsis
     "Various cryptographic hashes for bytestrings")
    (description
     "This package provides efficient cryptographic hash implementations for
strict and lazy bytestrings.  For now, CRC32 and Adler32 are supported; they
are implemented as FFI bindings to efficient code from zlib.")
    (license license:bsd-3)))

(define-public ghc-entropy
  (package
    (name "ghc-entropy")
    (version "0.3.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "entropy-" version "/"
                           "entropy-" version ".tar.gz"))
       (sha256
        (base32
         "1l3lfigqvdlmxkz1wl7zdkmc0i2r5p6z4xzhiw8xdsbsw7aljfkl"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/TomMD/entropy")
    (synopsis "Provides platform independent entropy source for Haskell")
    (description "This Haskell package provides a platform independent method
to obtain cryptographically strong entropy.")
    (license license:bsd-3)))

(define-public ghc-pem
  (package
    (name "ghc-pem")
    (version "0.2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "pem/pem-" version ".tar.gz"))
              (sha256
               (base32
                "162sk5sg22w21wqz5qv8kx6ibxp99v5p20g3nknhm1kddk3hha1p"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-mtl" ,ghc-mtl)
       ("ghc-base64-bytestring" ,ghc-base64-bytestring)))
    (native-inputs
     `(("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://github.com/vincenthz/hs-pem")
    (synopsis "Privacy Enhanced Mail (PEM) format reader and writer")
    (description
     "This library provides readers and writers for the @dfn{Privacy Enhanced
Mail} (PEM) format.")
    (license license:bsd-3)))

(define-public ghc-puremd5
  (package
    (name "ghc-puremd5")
    (version "2.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "pureMD5-" version "/"
                           "pureMD5-" version ".tar.gz"))
       (sha256
        (base32
         "0zdilz41cla2ck7mcw1a9702gyg2abq94mqahr4vci9sbs53bwxy"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-cereal" ,ghc-cereal)
              ("ghc-crypto-api" ,ghc-crypto-api)
              ("ghc-tagged" ,ghc-tagged)))
    (native-inputs `(("ghc-crypto-api-tests" ,ghc-crypto-api-tests)
                     ("ghc-quickcheck" ,ghc-quickcheck)
                     ("ghc-test-framework" ,ghc-test-framework)
                     ("ghc-test-framework-quickcheck2"
                      ,ghc-test-framework-quickcheck2)
                     ("ghc-pretty-hex" ,ghc-pretty-hex)))
    (home-page "https://github.com/TomMD/pureMD5")
    (synopsis "Haskell implementation of the MD5 hash algorithm")
    (description "This package provides a Haskell-only implementation of
the MD5 digest (hash) algorithm.  This now supports the @code{crypto-api} class
interface.")
    (license license:bsd-3)))

(define-public ghc-sha
  (package
    (name "ghc-sha")
    (version "1.6.4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "SHA/SHA-" version ".tar.gz"))
              (sha256
               (base32
                "134ajm87fm4lpsw86m9q8apv20dw4bpk46raa389zr6bcdpifw64"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)))
    (home-page "https://hackage.haskell.org/package/SHA")
    (synopsis "SHA suite of message digest functions")
    (description
     "This library implements the SHA suite of message digest functions,
according to NIST FIPS 180-2 (with the SHA-224 addendum), as well as the
SHA-based HMAC routines.  The functions have been tested against most of the
NIST and RFC test vectors for the various functions.  While some attention has
been paid to performance, these do not presently reach the speed of well-tuned
libraries, like OpenSSL.")
    (license license:bsd-3)))

(define-public ghc-x509
  (package
    (name "ghc-x509")
    (version "1.6.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "x509/x509-" version ".tar.gz"))
              (sha256
               (base32
                "1pmsby29abn485fvnymsgipvb3p1ch9c591xj5ncszkf0ivjiiin"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-memory" ,ghc-memory)
       ("ghc-mtl" ,ghc-mtl)
       ("ghc-hourglass" ,ghc-hourglass)
       ("ghc-pem" ,ghc-pem)
       ("ghc-asn1-types" ,ghc-asn1-types)
       ("ghc-asn1-encoding" ,ghc-asn1-encoding)
       ("ghc-asn1-parse" ,ghc-asn1-parse)
       ("ghc-cryptonite" ,ghc-cryptonite)))
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)))
    (home-page "https://github.com/vincenthz/hs-certificate")
    (synopsis "X509 reader and writer")
    (description
     "This library provides functions to read and write X509 certificates.")
    (license license:bsd-3)))

(define-public ghc-x509-store
  (package
    (name "ghc-x509-store")
    (version "1.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "x509-store/x509-store-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "01abx528i9f7djq2772xyna8x2mykrnwqkcfrapcx7z3bhprvml3"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-mtl" ,ghc-mtl)
       ("ghc-pem" ,ghc-pem)
       ("ghc-asn1-types" ,ghc-asn1-types)
       ("ghc-asn1-encoding" ,ghc-asn1-encoding)
       ("ghc-cryptonite" ,ghc-cryptonite)
       ("ghc-x509" ,ghc-x509)))
    (home-page "https://github.com/vincenthz/hs-certificate")
    (synopsis "X.509 collection accessing and storing methods")
    (description
     "This package provides functions for accessing and storing X.509
collections, certificates, revocation lists, and exception lists.")
    (license license:bsd-3)))

(define-public ghc-x509-validation
  (package
    (name "ghc-x509-validation")
    (version "1.6.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "x509-validation/x509-validation-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1qr1v561hdlhjgqjv9pj9mbk0q1xf2mr1j67ghy93nlxxyzd7dw0"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-memory" ,ghc-memory)
       ("ghc-byteable" ,ghc-byteable)
       ("ghc-mtl" ,ghc-mtl)
       ("ghc-hourglass" ,ghc-hourglass)
       ("ghc-data-default-class" ,ghc-data-default-class)
       ("ghc-pem" ,ghc-pem)
       ("ghc-asn1-types" ,ghc-asn1-types)
       ("ghc-asn1-encoding" ,ghc-asn1-encoding)
       ("ghc-x509" ,ghc-x509)
       ("ghc-x509-store" ,ghc-x509-store)
       ("ghc-cryptonite" ,ghc-cryptonite)))
    (home-page "https://github.com/vincenthz/hs-certificate")
    (synopsis "X.509 certificate and revocation list validation")
    (description
     "This package provides functions for X.509 certificate and revocation
list validation.")
    (license license:bsd-3)))

(define-public ghc-x509-system
  (package
    (name "ghc-x509-system")
    (version "1.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "x509-system/x509-system-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "10qf60d2f9jqwnbrhsb2cwpi86xg66m6dxndlxw967v1cdb3h6gf"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-mtl" ,ghc-mtl)
       ("ghc-pem" ,ghc-pem)
       ("ghc-x509" ,ghc-x509)
       ("ghc-x509-store" ,ghc-x509-store)))
    (home-page "https://github.com/vincenthz/hs-certificate")
    (synopsis "Handle system X.509 accessors and storage")
    (description
     "This package provides a library to handle system accessors and storage
for X.509 certificates.")
    (license license:bsd-3)))
