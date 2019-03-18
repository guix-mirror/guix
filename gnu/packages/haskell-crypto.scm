;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2015, 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Nils Gillmann <ng0@n0.is>
;;; Copyright © 2017 rsiddharth <s@ricketyspace.net>
;;; Copyright © 2017, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
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
       ("ghc-text" ,ghc-text)))
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
    (version "0.13.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "crypto-api-" version "/"
                           "crypto-api-" version ".tar.gz"))
       (sha256
        (base32
         "19bsmkqkpnvh01b77pmyarx00fic15j4hvg4pzscrj4prskrx2i9"))))
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
     `(#:cabal-revision
       ("2" "0vyb9cfvpfxpslxvvhd48gw37i9g8ry5x63xwxd9q7xfiqhs7p3a")
       #:tests? #f)) ; tests require old version of ghc-hunit (0.9)
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
     `(#:cabal-revision
       ("2" "0xas0nbq9bfdzlj6k565ibizv1cqvzfzsdj6q9pdiiwyxqblqc3m")
       #:tests? #f)) ; tests require old version of ghc-hunit (0.9)
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

(define-public ghc-cryptohash-sha256
  (package
    (name "ghc-cryptohash-sha256")
    (version "0.11.101.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "cryptohash-sha256-" version "/"
                           "cryptohash-sha256-" version ".tar.gz"))
       (sha256
        (base32
         "1p85vajcgw9hmq8zsz9krzx0vxh7aggwbg5w9ws8w97avcsn8xaj"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("1" "19birnmwga1yh82l4jqc3fygqkqcf5y8dlldnxfswngkzc3rvwp3")
       #:tests? #f)) ; tests require old version of ghc-hunit (0.9)
    (inputs
     `(("ghc-base16-bytestring" ,ghc-base16-bytestring)))
    (native-inputs
     `(("ghc-sha" ,ghc-sha)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
       ("ghc-hunit" ,ghc-hunit)))
    (home-page "https://github.com/hvr/cryptohash-sha1")
    (synopsis "SHA-256 implementation for Haskell")
    (description "This Haskell package provides an incremental and
one-pass, pure API to the @uref{https://en.wikipedia.org/wiki/SHA-2,
SHA-256 cryptographic hash algorithm}, with performance close to the
fastest implementations available in other languages.

The implementation is made in C with a haskell FFI wrapper that hides
the C implementation.")
    (license license:bsd-3)))

(define-public ghc-cryptonite
  (package
    (name "ghc-cryptonite")
    (version "0.25")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "cryptonite/cryptonite-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "131wbbdr5yavs5k1ah9sz6fqx1ffyvaxf66pwjzsfc47mwc1mgl9"))))
    (build-system haskell-build-system)
    ;; FIXME: tests are broken.
    ;; See https://github.com/haskell-crypto/cryptonite/issues/260
    (arguments '(#:tests? #f))
    (inputs
     `(("ghc-basement" ,ghc-basement)
       ("ghc-memory" ,ghc-memory)
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
    (version "0.4.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "entropy-" version "/"
                           "entropy-" version ".tar.gz"))
       (sha256
        (base32 "1fgf47l9klwn1xssbcbq6by651vikd8hlfxhiwd5bqzxr1jnlgrf"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/TomMD/entropy")
    (synopsis "Provides platform independent entropy source for Haskell")
    (description "This Haskell package provides a platform independent method
to obtain cryptographically strong entropy.")
    (license license:bsd-3)))

(define-public ghc-pem
  (package
    (name "ghc-pem")
    (version "0.2.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "pem/pem-" version ".tar.gz"))
              (sha256
               (base32
                "1m7qjsxrd8m88cvkqmr8kscril500j2a9y0iynvksjyjkhdlq33p"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-basement" ,ghc-basement)
       ("ghc-memory" ,ghc-memory)))
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
    (version "1.6.4.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "SHA/SHA-" version ".tar.gz"))
              (sha256
               (base32
                "0i4b2wjisivdy72synal711ywhx05mfqfba5n65rk8qidggm1nbb"))))
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
    (version "1.7.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "x509/x509-" version ".tar.gz"))
              (sha256
               (base32
                "1j67c35g8334jx7x32hh6awhr43dplp0qwal5gnlkmx09axzrc5i"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-memory" ,ghc-memory)
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
    (version "1.6.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "x509-store/x509-store-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1y8yyr1i95jkllg8k0z54k5v4vachp848clc07m33xpxidn3b1lp"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-pem" ,ghc-pem)
       ("ghc-asn1-types" ,ghc-asn1-types)
       ("ghc-asn1-encoding" ,ghc-asn1-encoding)
       ("ghc-cryptonite" ,ghc-cryptonite)
       ("ghc-x509" ,ghc-x509)))
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)))
    (home-page "https://github.com/vincenthz/hs-certificate")
    (synopsis "X.509 collection accessing and storing methods")
    (description
     "This package provides functions for accessing and storing X.509
collections, certificates, revocation lists, and exception lists.")
    (license license:bsd-3)))

(define-public ghc-x509-validation
  (package
    (name "ghc-x509-validation")
    (version "1.6.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "x509-validation/x509-validation-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1ms51scawldgyfcim5a2qlgyn3rnrclyh205d6djaa1569vrs73n"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-memory" ,ghc-memory)
       ("ghc-byteable" ,ghc-byteable)
       ("ghc-hourglass" ,ghc-hourglass)
       ("ghc-data-default-class" ,ghc-data-default-class)
       ("ghc-pem" ,ghc-pem)
       ("ghc-asn1-types" ,ghc-asn1-types)
       ("ghc-asn1-encoding" ,ghc-asn1-encoding)
       ("ghc-x509" ,ghc-x509)
       ("ghc-x509-store" ,ghc-x509-store)
       ("ghc-cryptonite" ,ghc-cryptonite)))
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)))
    (home-page "https://github.com/vincenthz/hs-certificate")
    (synopsis "X.509 certificate and revocation list validation")
    (description
     "This package provides functions for X.509 certificate and revocation
list validation.")
    (license license:bsd-3)))

(define-public ghc-x509-system
  (package
    (name "ghc-x509-system")
    (version "1.6.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "x509-system/x509-system-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "06a4m9c7vlr9nhp9gmqbb46arf0yj1dkdm4nip03hzy67spdmp20"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-pem" ,ghc-pem)
       ("ghc-x509" ,ghc-x509)
       ("ghc-x509-store" ,ghc-x509-store)))
    (home-page "https://github.com/vincenthz/hs-certificate")
    (synopsis "Handle system X.509 accessors and storage")
    (description
     "This package provides a library to handle system accessors and storage
for X.509 certificates.")
    (license license:bsd-3)))

(define-public ghc-crypto-cipher-types
  (package
    (name "ghc-crypto-cipher-types")
    (version "0.0.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "crypto-cipher-types-" version "/"
                           "crypto-cipher-types-" version ".tar.gz"))
       (sha256
        (base32
         "03qa1i1kj07pfrxsi7fiaqnnd0vi94jd4jfswbmnm4gp1nvzcwr0"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-byteable" ,ghc-byteable)
              ("ghc-securemem" ,ghc-securemem)))
    (home-page "https://github.com/vincenthz/hs-crypto-cipher")
    (synopsis "Generic cryptography cipher types for Haskell")
    (description "This Haskell package provides basic typeclasses and types
for symmetric ciphers.")
    (license license:bsd-3)))

(define-public ghc-cipher-aes
  (package
    (name "ghc-cipher-aes")
    (version "0.2.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "cipher-aes-" version "/"
                           "cipher-aes-" version ".tar.gz"))
       (sha256
        (base32
         "05ahz6kjq0fl1w66gpiqy0vndli5yx1pbsbw9ni3viwqas4p3cfk"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-byteable" ,ghc-byteable)
              ("ghc-securemem" ,ghc-securemem)
              ("ghc-crypto-cipher-types" ,ghc-crypto-cipher-types)))
    (native-inputs `(("ghc-quickcheck" ,ghc-quickcheck)
                     ("ghc-test-framework" ,ghc-test-framework)
                     ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)
                     ("ghc-crypto-cipher-tests" ,ghc-crypto-cipher-tests)))
    (home-page "https://github.com/vincenthz/hs-cipher-aes")
    (synopsis "AES cipher implementation with advanced mode of operations for
Haskell")
    (description "This Haskell package provides AES cipher implementation.

The modes of operations available are ECB (Electronic code book), CBC (Cipher
block chaining), CTR (Counter), XTS (XEX with ciphertext stealing),
GCM (Galois Counter Mode).

The AES implementation uses AES-NI when available (on x86 and x86-64
architecture), but fallback gracefully to a software C implementation.

The software implementation uses S-Boxes, which might suffer for cache timing
issues.  However do notes that most other known software implementations,
including very popular one (openssl, gnutls) also uses similar
implementation.  If it matters for your case, you should make sure you have
AES-NI available, or you'll need to use a different implementation.")
    (license license:bsd-3)))

(define-public ghc-crypto-random
  (package
    (name "ghc-crypto-random")
    (version "0.0.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "crypto-random-" version "/"
                           "crypto-random-" version ".tar.gz"))
       (sha256
        (base32
         "0139kbbb2h7vshf68y3fvjda29lhj7jjwl4vq78w4y8k8hc7l2hp"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-securemem" ,ghc-securemem)
              ("ghc-vector" ,ghc-vector)))
    (home-page "https://github.com/vincenthz/hs-crypto-random")
    (synopsis "Simple cryptographic random related types for Haskell")
    (description "Simple cryptographic random related types: a safe
abstraction for CPRNGs.")
    (license license:bsd-3)))

(define-public ghc-cprng-aes
  (package
    (name "ghc-cprng-aes")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "cprng-aes-" version "/"
                           "cprng-aes-" version ".tar.gz"))
       (sha256
        (base32
         "1wr15kbmk1g3l8a75n0iwbzqg24ixv78slwzwb2q6rlcvq0jlnb4"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-byteable" ,ghc-byteable)
              ("ghc-crypto-random" ,ghc-crypto-random)
              ("ghc-cipher-aes" ,ghc-cipher-aes)))
    (home-page "https://github.com/vincenthz/hs-cprng-aes")
    (synopsis "Crypto Pseudo Random Number Generator using AES in counter mode
in Haskell")
    (description "Simple crypto pseudo-random-number-generator with really
good randomness property.

Using ent, a randomness property maker on one 1Mb sample:

@itemize
@item Entropy = 7.999837 bits per byte.
@item Optimum compression would reduce the size of this 1048576 byte file by 0
percent.
@item Chi square distribution for 1048576 samples is 237.02.
@item Arithmbetic mean value of data bytes is 127.3422 (127.5 = random).
@item Monte Carlo value for Pi is 3.143589568 (error 0.06 percent).
@end itemize

Compared to urandom with the same sampling:

@itemize
@item Entropy = 7.999831 bits per byte.
@item Optimum compression would reduce the size of this 1048576 byte file by 0
percent.
@item Chi square distribution for 1048576 samples is 246.63.
@item Arithmetic mean value of data bytes is 127.6347 (127.5 = random).
@item Monte Carlo value for Pi is 3.132465868 (error 0.29 percent).
@end itemize")
    (license license:bsd-3)))

(define-public ghc-ed25519
  (package
    (name "ghc-ed25519")
    (version "0.0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/ed25519/ed25519-"
             version ".tar.gz"))
       (sha256
        (base32
         "0v8msqvgzimhs7p5ri25hrb1ni2wvisl5rmdxy89fc59py79b9fq"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("2" "1cq6h3jqkb1kvd9fjfhsllg5gq78sdiyf2gy9862xhlbv6wil19f")
       ;; We omit these test suites because they require old versions of
       ;; packages and packages we do not have.
       #:configure-flags
       '("--flags=-test-hlint -test-doctests -test-properties")))
    (home-page "http://thoughtpolice.github.com/hs-ed25519")
    (synopsis "Ed25519 cryptographic signatures")
    (description "This package provides a simple, fast, self-contained
copy of the Ed25519 public-key signature system with a clean interface.
It also includes support for detached signatures, and thorough
documentation on the design and implementation, including usage
guidelines.")
    (license license:expat)))
