;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2015, 2016, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages haskell-web)
  #:use-module (gnu packages)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-crypto)
  #:use-module (gnu packages tls)
  #:use-module (guix build-system haskell)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public ghc-tagsoup
  (package
    (name "ghc-tagsoup")
    (version "0.14.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/tagsoup/"
                           "tagsoup-" version ".tar.gz"))
       (sha256
        (base32
         "1yv3dbyb0i1yqm796jgc4jj5kxkla1sxb3b2klw5ks182kdx8kjb"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-text" ,ghc-text)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "http://community.haskell.org/~ndm/tagsoup/")
    (synopsis
     "Parsing and extracting information from (possibly malformed) HTML/XML
documents")
    (description
     "TagSoup is a library for parsing HTML/XML.  It supports the HTML 5
specification, and can be used to parse either well-formed XML, or
unstructured and malformed HTML from the web.  The library also provides
useful functions to extract information from an HTML document, making it ideal
for screen-scraping.")
    (license license:bsd-3)))

(define-public ghc-cookie
  (package
    (name "ghc-cookie")
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/cookie/cookie-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0qpdydhb9gw590ffabqg70x7xvjpg8l74idqnrfbhv5yrr7hryzv"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-old-locale" ,ghc-old-locale)
       ("ghc-blaze-builder" ,ghc-blaze-builder)
       ("ghc-text" ,ghc-text)
       ("ghc-data-default-class" ,ghc-data-default-class)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)))
    (home-page "https://github.com/snoyberg/cookie")
    (synopsis "HTTP cookie parsing and rendering")
    (description "HTTP cookie parsing and rendering library for Haskell.")
    (license license:bsd-3)))

(define-public ghc-httpd-shed
  (package
    (name "ghc-httpd-shed")
    (version "0.4.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/httpd-shed/"
                           "httpd-shed-" version ".tar.gz"))
       (sha256
        (base32
         "064jy1mqhnf1hvq6s04wlhmp916rd522x58djb9qixv13vc8gzxh"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-network-uri" ,ghc-network-uri)
       ("ghc-network" ,ghc-network)))
    (home-page "https://hackage.haskell.org/package/httpd-shed")
    (synopsis "Simple web-server with an interact style API")
    (description
     "This web server promotes a function from @code{Request} to @code{IO
Response} into a local web server.  The user can decide how to interpret the
requests, and the library is intended for implementing Ajax APIs.")
    (license license:bsd-3)))

(define-public ghc-http-types
  (package
    (name "ghc-http-types")
    (version "0.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/http-types/"
                           "http-types-" version ".tar.gz"))
       (sha256
        (base32
         "1wv9k6nlvkdsxwlr7gaynphvzmvi5211gvwq96mbcxgk51a739rz"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-doctest" ,ghc-doctest)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-quickcheck-instances" ,ghc-quickcheck-instances)
       ("hspec-discover" ,hspec-discover)))
    (inputs
     `(("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-blaze-builder" ,ghc-blaze-builder)
       ("ghc-text" ,ghc-text)))
    (home-page "https://github.com/aristidb/http-types")
    (synopsis "Generic HTTP types for Haskell")
    (description "This package provides generic HTTP types for Haskell (for
both client and server code).")
    (license license:bsd-3)))

(define-public ghc-http
  (package
    (name "ghc-http")
    (version "4000.3.12")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/HTTP/"
                           "HTTP-" version ".tar.gz"))
       (sha256
        (base32
         "140r6qy1ay25piv0z3hih11zhigyi08nkwc32097j43pjff6mzx3"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-httpd-shed" ,ghc-httpd-shed)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)))
    (inputs
     `(("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-conduit" ,ghc-conduit)
       ("ghc-conduit-extra" ,ghc-conduit-extra)
       ("ghc-http-types" ,ghc-http-types)
       ("ghc-old-time" ,ghc-old-time)
       ("ghc-parsec" ,ghc-parsec)
       ("ghc-puremd5" ,ghc-puremd5)
       ("ghc-network" ,ghc-network)
       ("ghc-network-uri" ,ghc-network-uri)
       ("ghc-split" ,ghc-split)))
    (arguments
     `(#:tests? #f)) ; FIXME: currently missing libraries used for tests.
    (home-page "https://github.com/haskell/HTTP")
    (synopsis "Library for client-side HTTP")
    (description
     "The HTTP package supports client-side web programming in Haskell.  It
lets you set up HTTP connections, transmitting requests and processing the
responses coming back.")
    (license license:bsd-3)))

(define-public ghc-http-client
  (package
    (name "ghc-http-client")
    (version "0.5.13.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "http-client/http-client-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0szwbgvkkdz56lgi91armkagmb7nnfwbpp4j7cm9zhmffv3ba8g1"))))
    (build-system haskell-build-system)
    ;; Tests require access to the web.
    (arguments `(#:tests? #f))
    (inputs
     `(("ghc-async" ,ghc-async)
       ("ghc-base64-bytestring" ,ghc-base64-bytestring)
       ("ghc-blaze-builder" ,ghc-blaze-builder)
       ("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-cookie" ,ghc-cookie)
       ("ghc-data-default-class" ,ghc-data-default-class)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-http-types" ,ghc-http-types)
       ("ghc-memory" ,ghc-memory)
       ("ghc-mime-types" ,ghc-mime-types)
       ("ghc-monad-control" ,ghc-monad-control)
       ("ghc-network" ,ghc-network)
       ("ghc-network-uri" ,ghc-network-uri)
       ("ghc-random" ,ghc-random)
       ("ghc-streaming-commons" ,ghc-streaming-commons)
       ("ghc-text" ,ghc-text)
       ("ghc-zlib" ,ghc-zlib)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)))
    (home-page "https://github.com/snoyberg/http-client")
    (synopsis "HTTP client engine")
    (description
     "This package provides an HTTP client engine, intended as a base layer
for more user-friendly packages.")
    (license license:expat)))

(define-public ghc-http-client-tls
  (package
    (name "ghc-http-client-tls")
    (version "0.3.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "http-client-tls/http-client-tls-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0n4mi8z77qaggfyq17z79cl304nf1f4h6gag60v4wjwghvmj7yn1"))))
    (build-system haskell-build-system)
    ;; Tests require Internet access
    (arguments `(#:tests? #f))
    (inputs
     `(("ghc-data-default-class" ,ghc-data-default-class)
       ("ghc-http-client" ,ghc-http-client)
       ("ghc-connection" ,ghc-connection)
       ("ghc-network" ,ghc-network)
       ("ghc-tls" ,ghc-tls)
       ("ghc-http-types" ,ghc-http-types)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)))
    (home-page "https://github.com/snoyberg/http-client")
    (synopsis "Backend for http-client using the TLS library")
    (description
     "This package provides a backend for the http-client package using the
connection and TLS libraries.  It is intended for use by higher-level
libraries, such as http-conduit.")
    (license license:expat)))

(define-public ghc-http-date
  (package
    (name "ghc-http-date")
    (version "0.0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "http-date-" version "/"
                           "http-date-" version ".tar.gz"))
       (sha256
        (base32
         "0dknh28kyarnzqrsc80ssalxjrq0qbv7ir49247p2grb7rh0dqgj"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-attoparsec" ,ghc-attoparsec)))
    (native-inputs
     `(("ghc-doctest" ,ghc-doctest)
       ("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)
       ("ghc-old-locale" ,ghc-old-locale)))
    (home-page "https://github.com/kazu-yamamoto/http-date")
    (synopsis "HTTP Date parser/formatter")
    (description "Library for Parsing and formatting HTTP
Date in Haskell.")
    (license license:bsd-3)))

(define-public ghc-http2
  (package
    (name "ghc-http2")
    (version "1.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "http2-" version "/"
                           "http2-" version ".tar.gz"))
       (sha256
        (base32
         "0hww0rfsv6lqx62qzycbcqy5q6rh9k09qkyjkdm5m1sp1z50wqk1"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-bytestring-builder" ,ghc-bytestring-builder)
       ("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-aeson" ,ghc-aeson)
       ("ghc-aeson-pretty" ,ghc-aeson-pretty)
       ("ghc-hex" ,ghc-hex)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-vector" ,ghc-vector)
       ("ghc-word8" ,ghc-word8)
       ("ghc-psqueues" ,ghc-psqueues)
       ("ghc-stm" ,ghc-stm)))
       (native-inputs
        `(("ghc-glob" ,ghc-glob)
         ("ghc-hspec" ,ghc-hspec)
         ("ghc-doctest" ,ghc-doctest)
         ("hspec-discover" ,hspec-discover)))
    (home-page "https://github.com/kazu-yamamoto/http2")
    (synopsis "HTTP/2 library including frames, priority queues and HPACK")
    (description "This package provides a HTTP/2.0 library including frames
and HPACK.  Currently HTTP/2 16 framing and HPACK 10 is supported.")
    (license license:bsd-3)))

(define-public ghc-http-conduit
  (package
    (name  "ghc-http-conduit")
    (version "2.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "http-conduit-" version "/" "http-conduit-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1iay4hr0mj8brkxvgkv1liqa8irl9axfc3qhn8qsvcyq4n1l95km"))))
    (build-system haskell-build-system)
    ;; FIXME: `httpLbs TLS` in test-suite `test` fails with
    ;; ConnectionFailure getProtocolByName: does not exist (no such protocol
    ;; name: tcp)
    (arguments `(#:tests? #f))
    (inputs
     `(("ghc-aeson" ,ghc-aeson)
       ("ghc-resourcet" ,ghc-resourcet)
       ("ghc-conduit" ,ghc-conduit)
       ("ghc-conduit-extra" ,ghc-conduit-extra)
       ("ghc-http-types" ,ghc-http-types)
       ("ghc-lifted-base" ,ghc-lifted-base)
       ("ghc-http-client" ,ghc-http-client)
       ("ghc-http-client-tls" ,ghc-http-client-tls)
       ("ghc-monad-control" ,ghc-monad-control)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-unliftio" ,ghc-unliftio)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-data-default-class" ,ghc-data-default-class)
       ("ghc-connection" ,ghc-connection)
       ("ghc-warp-tls" ,ghc-warp-tls)
       ("ghc-blaze-builder" ,ghc-blaze-builder)
       ("ghc-text" ,ghc-text)
       ("ghc-conduit" ,ghc-conduit)
       ("ghc-utf8-string" ,ghc-utf8-string)
       ("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-lifted-base" ,ghc-lifted-base)
       ("ghc-network" ,ghc-network)
       ("ghc-wai" ,ghc-wai)
       ("ghc-warp" ,ghc-warp)
       ("ghc-wai-conduit" ,ghc-wai-conduit)
       ("ghc-http-types" ,ghc-http-types)
       ("ghc-http-client" ,ghc-http-client)
       ("ghc-cookie" ,ghc-cookie)
       ("ghc-conduit-extra" ,ghc-conduit-extra)
       ("ghc-streaming-commons" ,ghc-streaming-commons)
       ("ghc-aeson" ,ghc-aeson)
       ("ghc-temporary" ,ghc-temporary)
       ("ghc-resourcet" ,ghc-resourcet)))
    (home-page "https://hackage.haskell.org/package/http-conduit")
    (synopsis "HTTP/HTTPS client with conduit interface")
    (description "This library uses attoparsec for parsing the actual
contents of the HTTP connection.  It also provides higher-level functions
which allow you to avoid direct usage of conduits.")
    (license license:bsd-3)))

(define-public ghc-wai
  (package
    (name "ghc-wai")
    (version "3.2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/wai/wai-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "08afasnirja21vr0bmzcywz4w29x736dmdv7h8nnh1l8bn7sd02x"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-bytestring-builder" ,ghc-bytestring-builder)
       ("ghc-unix-compat" ,ghc-unix-compat)
       ("ghc-vault" ,ghc-vault)
       ("ghc-blaze-builder" ,ghc-blaze-builder)
       ("ghc-network" ,ghc-network)
       ("ghc-text" ,ghc-text)
       ("ghc-http-types" ,ghc-http-types)))
    (native-inputs
     `(("hspec-discover" ,hspec-discover)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-hspec" ,ghc-hspec)))
    (home-page "https://hackage.haskell.org/package/wai")
    (synopsis "Web application interface for Haskell")
    (description "This package provides a Web Application Interface (WAI)
library for the Haskell language.  It defines a common protocol for
communication between web applications and web servers.")
    (license license:bsd-3)))

(define-public ghc-wai-logger
  (package
    (name "ghc-wai-logger")
    (version "2.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/wai-logger/wai-logger-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1w0b0vinsyqr37wciljkz8g5dcmfi2r210lq194a0wkycly9kkch"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f)) ; FIXME: Tests cannot find libraries exported
                               ; by propagated-inputs.
    (inputs
     `(("ghc-auto-update" ,ghc-auto-update)
       ("ghc-byteorder" ,ghc-byteorder)
       ("ghc-easy-file" ,ghc-easy-file)
       ("ghc-unix-time" ,ghc-unix-time)
       ("ghc-blaze-builder" ,ghc-blaze-builder)
       ("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-fast-logger" ,ghc-fast-logger)
       ("ghc-http-types" ,ghc-http-types)
       ("ghc-network" ,ghc-network)
       ("ghc-wai" ,ghc-wai)))
    (home-page "https://hackage.haskell.org/package/wai-logger")
    (synopsis "Logging system for WAI")
    (description "This package provides the logging system for WAI.")
    (license license:bsd-3)))

(define-public ghc-wai-extra
  (package
    (name "ghc-wai-extra")
    (version "3.0.18")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/wai-extra/wai-extra-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0r079mqqdv14fp97w0rigdpwk6b88grpjlqsjc5y8bbc0skf5za2"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-ansi-terminal" ,ghc-ansi-terminal)
       ("ghc-base64-bytestring" ,ghc-base64-bytestring)
       ("ghc-cookie" ,ghc-cookie)
       ("ghc-blaze-builder" ,ghc-blaze-builder)
       ("ghc-network" ,ghc-network)
       ("ghc-lifted-base" ,ghc-lifted-base)
       ("ghc-streaming-commons" ,ghc-streaming-commons)
       ("ghc-stringsearch" ,ghc-stringsearch)
       ("ghc-resourcet" ,ghc-resourcet)
       ("ghc-fast-logger" ,ghc-fast-logger)
       ("ghc-wai-logger" ,ghc-wai-logger)
       ("ghc-zlib" ,ghc-zlib)
       ("ghc-word8" ,ghc-word8)
       ("ghc-iproute" ,ghc-iproute)
       ("ghc-void" ,ghc-void)
       ("ghc-wai" ,ghc-wai)
       ("ghc-http-types" ,ghc-http-types)
       ("ghc-text" ,ghc-text)
       ("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-data-default-class" ,ghc-data-default-class)
       ("ghc-unix-compat" ,ghc-unix-compat)
       ("ghc-vault" ,ghc-vault)
       ("ghc-aeson" ,ghc-aeson)))
    (native-inputs
     `(("hspec-discover" ,hspec-discover)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-hunit" ,ghc-hunit)))
    (home-page "https://github.com/yesodweb/wai")
    (synopsis "Some basic WAI handlers and middleware")
    (description "This library provides basic WAI handlers and middleware
functionality.")
    (license license:expat)))

(define-public ghc-wai-conduit
  (package
    (name "ghc-wai-conduit")
    (version "3.0.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "wai-conduit-" version "/"
                           "wai-conduit-" version ".tar.gz"))
       (sha256
        (base32
         "1zvsiwjq2mvkb9sjgp3ly9m968m7a2jjzr4id6jpi3mmqykj15z4"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-conduit" ,ghc-conduit)
       ("ghc-http-types" ,ghc-http-types)
       ("ghc-wai" ,ghc-wai)
       ("ghc-blaze-builder" ,ghc-blaze-builder)))
    (home-page "https://github.com/yesodweb/wai")
    (synopsis "Conduit wrappers for Haskell's WAI")
    (description "This package provides data streaming abstraction for
Haskell's Web Application Interface (WAI).")
    (license license:expat)))

(define-public ghc-warp
  (package
    (name "ghc-warp")
    (version "3.2.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "warp-" version "/" "warp-" version
                           ".tar.gz"))
       (sha256
        (base32
         "1zp5cy0bbj508vdvms1n5z80z37m253kwsqc5a83cfc990n6fgw5"))))
    (build-system haskell-build-system)
    (arguments
     `(#:tests? #f)) ; FIXME: Test-Suite `spec` fails.
    (inputs
     `(("ghc-async" ,ghc-async)
       ("ghc-auto-update" ,ghc-auto-update)
       ("ghc-blaze-builder" ,ghc-blaze-builder)
       ("ghc-bytestring-builder" ,ghc-bytestring-builder)
       ("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-http-types" ,ghc-http-types)
       ("ghc-iproute" ,ghc-iproute)
       ("ghc-network" ,ghc-network)
       ("ghc-stm" ,ghc-stm)
       ("ghc-streaming-commons" ,ghc-streaming-commons)
       ("ghc-text" ,ghc-text)
       ("ghc-unix-compat" ,ghc-unix-compat)
       ("ghc-vault" ,ghc-vault)
       ("ghc-wai" ,ghc-wai)
       ("ghc-word8" ,ghc-word8)
       ("ghc-lifted-base" ,ghc-lifted-base)
       ("ghc-http-date" ,ghc-http-date)
       ("ghc-simple-sendfile" ,ghc-simple-sendfile)
       ("ghc-http2" ,ghc-http2)))
    (native-inputs
     `(("ghc-silently" ,ghc-silently)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-auto-update" ,ghc-auto-update)
       ("ghc-doctest" ,ghc-doctest)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-http" ,ghc-http)
       ("hspec-discover" ,hspec-discover)))
    (home-page "http://github.com/yesodweb/wai")
    (synopsis "HTTP server library for Haskell's WAI")
    (description "Warp is a server library for HTTP/1.x and HTTP/2
based WAI (Web Application Interface in Haskell).")
    (license license:expat)))

(define-public ghc-warp-tls
  (package
    (name "ghc-warp-tls")
    (version "3.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "warp-tls-" version "/"
                           "warp-tls-" version ".tar.gz"))
       (sha256
        (base32
         "14m2bzk5ivz9gdpxlcj6qnh46f2lycm1ybdjnfkj2876zrqwii7m"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-cryptonite" ,ghc-cryptonite)
       ("ghc-data-default-class" ,ghc-data-default-class)
       ("ghc-network" ,ghc-network)
       ("ghc-streaming-commons" ,ghc-streaming-commons)
       ("ghc-tls" ,ghc-tls)
       ("ghc-wai" ,ghc-wai)
       ("ghc-warp" ,ghc-warp)))
    (home-page "http://github.com/yesodweb/wai")
    (synopsis "SSL/TLS support for Warp")
    (description "This package provides SSL/TLS support for Warp,
a WAI handler, via the native Haskell TLS implementation.")
    (license license:expat)))

(define-public ghc-xss-sanitize
  (package
    (name "ghc-xss-sanitize")
    (version "0.3.5.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/xss-sanitize/xss-sanitize-"
             version ".tar.gz"))
       (sha256
        (base32
         "005cmhaw9xbzkcn42jmhvcvk63bzmg4lml368xwmqdvh7r0mcn4m"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-tagsoup" ,ghc-tagsoup)
       ("ghc-utf8-string" ,ghc-utf8-string)
       ("ghc-css-text" ,ghc-css-text)
       ("ghc-network-uri" ,ghc-network-uri)))
    (native-inputs
     `(("ghc-text" ,ghc-text)
       ("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-hunit" ,ghc-hunit)))
    (home-page "https://github.com/yesodweb/haskell-xss-sanitize")
    (synopsis "Sanitize untrusted HTML to prevent XSS attacks")
    (description "This library provides @code{sanitizeXSS}.  Run untrusted
HTML through @code{Text.HTML.SanitizeXSS.sanitizeXSS} to prevent XSS
attacks.")
    (license license:bsd-3)))

(define-public ghc-css-text
  (package
    (name "ghc-css-text")
    (version "0.1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/css-text/css-text-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0ynd9f4hn2sfwqzbsa0y7phmxq8za7jiblpjwx0ry8b372zhgxaz"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-text" ,ghc-text)
       ("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "http://www.yesodweb.com/")
    (synopsis "CSS parser and renderer")
    (description "This package provides a CSS parser and renderer for
Haskell.")
    (license license:bsd-3)))

(define-public ghc-mime-types
  (package
    (name "ghc-mime-types")
    (version "0.1.0.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "mime-types/mime-types-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1fg9cqpp5lswk8ajlq4f41n12c2v2naz179l8dsz6zisjqj4l5l3"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-text" ,ghc-text)))
    (home-page "https://github.com/yesodweb/wai")
    (synopsis "Basic MIME type handling types and functions")
    (description
     "This library provides basic MIME type handling types and functions.")
    (license license:expat)))

(define-public ghc-html
  (package
    (name "ghc-html")
    (version "1.0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/html/html-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0q9hmfii62kc82ijlg238fxrzxhsivn42x5wd6ffcr9xldg4jd8c"))))
    (build-system haskell-build-system)
    (home-page
     "https://hackage.haskell.org/package/html")
    (synopsis "HTML combinator library")
    (description
     "This package contains a combinator library for constructing HTML
documents.")
    (license license:bsd-3)))

(define-public ghc-xhtml
  (package
    (name "ghc-xhtml")
    (version "3000.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/xhtml/xhtml-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1n6wgzxbj8xf0wf1il827qidphnffb5vzhwzqlxhh70c2y10f0ik"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/haskell/xhtml")
    (synopsis "XHTML combinator library")
    (description
     "This package provides combinators for producing XHTML 1.0, including the
Strict, Transitional and Frameset variants.")
    (license license:bsd-3)))

(define-public ghc-blaze-html
  (package
    (name "ghc-blaze-html")
    (version "0.9.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "blaze-html/blaze-html-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0r0acv47nh75bmf7kjyfvhcwz8f02rn9x0a1l80pzgyczfrsmkmf"))))
    (build-system haskell-build-system)
    (arguments
     `(#:configure-flags (list "--allow-newer=QuickCheck"
                               "--allow-newer=HUnit")))
    (inputs
     `(("ghc-blaze-builder" ,ghc-blaze-builder)
       ("ghc-text" ,ghc-text)
       ("ghc-blaze-markup" ,ghc-blaze-markup)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)))
    (home-page "http://jaspervdj.be/blaze")
    (synopsis "Fast HTML combinator library")
    (description "This library provides HTML combinators for Haskell.")
    (license license:bsd-3)))

(define-public ghc-aeson
  (package
    (name "ghc-aeson")
    (version "1.2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/aeson/aeson-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "16zwpd07cmhs58wwsqbhxy3b58gqw8w5nr7nf6lwi4nvznjdn09l"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f)) ; FIXME: testing libraries are missing.
    (inputs
     `(("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-base-compat" ,ghc-base-compat)
       ("ghc-dlist" ,ghc-dlist)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-scientific" ,ghc-scientific)
       ("ghc-tagged" ,ghc-tagged)
       ("ghc-text" ,ghc-text)
       ("ghc-th-abstraction" ,ghc-th-abstraction)
       ("ghc-time-locale-compat" ,ghc-time-locale-compat)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-uuid-types" ,ghc-uuid-types)
       ("ghc-vector" ,ghc-vector)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-integer-logarithms" ,ghc-integer-logarithms)
       ("ghc-base-orphans" ,ghc-base-orphans)
       ("ghc-base16-bytestring" ,ghc-base16-bytestring)
       ("ghc-generic-deriving" ,ghc-generic-deriving)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)
       ("ghc-quickcheck-instances" ,ghc-quickcheck-instances)
       ("ghc-hashable-time" ,ghc-hashable-time)))
    (home-page "https://github.com/bos/aeson")
    (synopsis "Fast JSON parsing and encoding")
    (description "This package provides a JSON parsing and encoding library
for Haskell, optimized for ease of use and high performance.  (A note on
naming: in Greek mythology, Aeson was the father of Jason.)")
    (license license:bsd-3)))

(define-public ghc-aeson-for-pandoc-1
  (package (inherit ghc-aeson)
    (version "1.1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/aeson/aeson-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1zy5z8pzvh53qkjm0nm3f4rwqfqg3867ck8ncd6mrxpcyvxqqj1p"))))))

(define-public ghc-aeson-pretty
  (package
    (name "ghc-aeson-pretty")
    (version "0.8.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://hackage.haskell.org/package/aeson-pretty/aeson-pretty-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1yd98972srlbkn0f2jhrb3f443j9wnq2fnw5gbxjxzmkcinfh5yx"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-aeson" ,ghc-aeson)
       ("ghc-base-compat" ,ghc-base-compat)
       ("ghc-scientific" ,ghc-scientific)
       ("ghc-vector" ,ghc-vector)
       ("ghc-text" ,ghc-text)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-cmdargs" ,ghc-cmdargs)))
    (home-page "https://github.com/informatikr/aeson-pretty")
    (synopsis "JSON pretty-printing library and command-line tool")
    (description
     "This package provides a JSON pretty-printing library compatible with aeson
as well as a command-line tool to improve readabilty of streams of JSON data.
The library provides the function @code{encodePretty}.  It is a drop-in
replacement for aeson's @code{encode} function, producing JSON-ByteStrings for
human readers.  The command-line tool reads JSON from stdin and writes
prettified JSON to stdout.  It also offers a complementary \"compact\"-mode,
essentially the opposite of pretty-printing.")
    (license license:bsd-3)))

(define-public ghc-aeson-qq
  (package
    (name "ghc-aeson-qq")
    (version "0.8.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "aeson-qq/aeson-qq-" version ".tar.gz"))
              (sha256
               (base32
                "0ln13jqyfh5726hdrk1rad9a6cgrrj201plmwcfcpvq18v4m5ckd"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-base-compat" ,ghc-base-compat)
       ("ghc-text" ,ghc-text)
       ("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-scientific" ,ghc-scientific)
       ("ghc-vector" ,ghc-vector)
       ("ghc-aeson" ,ghc-aeson)
       ("ghc-parsec" ,ghc-parsec)
       ("ghc-haskell-src-meta" ,ghc-haskell-src-meta)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://github.com/zalora/aeson-qq")
    (synopsis "JSON quasiquoter for Haskell")
    (description
     "aeson-qq provides a JSON quasiquoter for Haskell.  This package exposes
the function @code{aesonQQ} that compile-time converts a string representation
of a JSON value into a @code{Data.Aeson.Value}.")
    (license license:expat)))

(define-public ghc-multipart
  (package
    (name "ghc-multipart")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/multipart/multipart-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0g04jhyw1ib1s7c9bcldyyn4n90qd9x7dmvic4vgq57bgcqgnhz5"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-parsec" ,ghc-parsec)))
    (home-page
     "http://www.github.com/silkapp/multipart")
    (synopsis
     "HTTP multipart library")
    (description
     "HTTP multipart split out of the cgi package, for Haskell.")
    (license license:bsd-3)))

(define-public ghc-uri-encode
  (package
    (name "ghc-uri-encode")
    (version "1.5.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/uri-encode/uri-encode-"
             version ".tar.gz"))
       (sha256
        (base32
         "11miwb5vvnn17m92ykz1pzg9x6s8fbpz3mmsyqs2s4b3mn55haz8"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-text" ,ghc-text)
       ("ghc-utf8-string" ,ghc-utf8-string)
       ("ghc-network-uri" ,ghc-network-uri)))
    (home-page "https://hackage.haskell.org/package/uri-encode")
    (synopsis "Unicode aware uri-encoding")
    (description "Unicode aware uri-encoding for Haskell.")
    (license license:bsd-3)))

(define-public ghc-path-pieces
  (package
    (name "ghc-path-pieces")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "path-pieces-" version "/"
                           "path-pieces-" version ".tar.gz"))
       (sha256
        (base32
         "0vx3sivcsld76058925hym2j6hm3g71f0qjr7v59f1g2afgx82q8"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-text" ,ghc-text)))
    (native-inputs `(("ghc-hunit" ,ghc-hunit)
                     ("ghc-hspec" ,ghc-hspec)
                     ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://github.com/yesodweb/path-pieces")
    (synopsis "Used in Yesod to automatically marshall data in the request path")
    (description  "This Haskell package provides two typeclasses for converting
Haskell data types to and from route pieces.")
    (license license:bsd-3)))

(define-public ghc-skein
  (package
    (name "ghc-skein")
    (version "1.0.9.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "skein-" version "/"
                           "skein-" version ".tar.gz"))
       (sha256
        (base32
         "1jdqdk0rz2wnvw735clnj8jh0a9rkrbqjg7vk3w6wczdql6cm0pq"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-cereal" ,ghc-cereal)
              ("ghc-tagged" ,ghc-tagged)
              ("ghc-crpto-api" ,ghc-crypto-api)))
    (native-inputs `(("ghc-hspec" ,ghc-hspec)))
    (home-page "https://github.com/yesodweb/path-pieces")
    (synopsis "Skein family of cryptographic hash functions for Haskell")
    (description "@uref{(http://www.skein-hash.info, Skein} is a family of
fast secure cryptographic hash functions designed by Niels Ferguson, Stefan
Lucks, Bruce Schneier, Doug Whiting, Mihir Bellare, Tadayoshi Kohno, Jon
Callas and Jesse Walker.

This Haskell package uses bindings to the optimized C implementation of Skein.")
    (license license:bsd-3)))

(define-public ghc-clientsession
  (package
    (name "ghc-clientsession")
    (version "0.9.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "clientsession-" version "/"
                           "clientsession-" version ".tar.gz"))
       (sha256
        (base32
         "0s6h4ykj16mpf7nlw2iqn2ji0p8g1fn5ni0s7yqaili6vv2as5ar"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-cereal" ,ghc-cereal)
              ("ghc-tagged" ,ghc-tagged)
              ("ghc-crypto-api" ,ghc-crypto-api)
              ("ghc-skein" ,ghc-skein)
              ("ghc-base64-bytestring" ,ghc-base64-bytestring)
              ("ghc-entropy" ,ghc-entropy)
              ("ghc-cprng-aes" ,ghc-cprng-aes)
              ("ghc-cipher-aes" ,ghc-cipher-aes)
              ("ghc-crypto-random" ,ghc-crypto-random)
              ("ghc-setenv" ,ghc-setenv)))
    (native-inputs `(("ghc-hunit" ,ghc-hunit)
                     ("ghc-hspec" ,ghc-hspec)
                     ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://github.com/yesodweb/clientsession/tree/master")
    (synopsis "Haskell library for securely store session data in a
client-side cookie")
    (description "This Haskell package achieves security through AES-CTR
encryption and Skein-MAC-512-256 authentication.  Uses Base64 encoding to
avoid any issues with characters.")
    (license license:expat)))

(define-public ghc-yesod-core
  (package
    (name "ghc-yesod-core")
    (version "1.4.37")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "yesod-core-" version "/"
                           "yesod-core-" version ".tar.gz"))
       (sha256
        (base32
         "0ww8hl0cx2g58zrdx3j6d5m2xwhssbajdqws1xk6rzl7rpfm1b9j"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-wai" ,ghc-wai)
              ("ghc-extra" ,ghc-extra)
              ("ghc-text" ,ghc-text)
              ("ghc-shakespeare" ,ghc-shakespeare)
              ("ghc-blaze-builder" ,ghc-blaze-builder)
              ("ghc-clientsession" ,ghc-clientsession)
              ("ghc-random" ,ghc-random)
              ("ghc-cereal" ,ghc-cereal)
              ("ghc-old-locale" ,ghc-old-locale)
              ("ghc-unordered-containers" ,ghc-unordered-containers)
              ("ghc-monad-control" ,ghc-monad-control)
              ("ghc-transformers-base" ,ghc-transformers-base)
              ("ghc-cookie" ,ghc-cookie)
              ("ghc-http-types" ,ghc-http-types)
              ("ghc-case-insensitive" ,ghc-case-insensitive)
              ("ghc-parsec" ,ghc-parsec)
              ("ghc-vector" ,ghc-vector)
              ("ghc-aeson" ,ghc-aeson)
              ("ghc-fast-logger" ,ghc-fast-logger)
              ("ghc-wai-logger" ,ghc-wai-logger)
              ("ghc-monad-logger" ,ghc-monad-logger)
              ("ghc-conduit" ,ghc-conduit)
              ("ghc-resourcet" ,ghc-resourcet)
              ("ghc-lifted-base" ,ghc-lifted-base)
              ("ghc-blaze-html" ,ghc-blaze-html)
              ("ghc-blaze-markup" ,ghc-blaze-markup)
              ("ghc-data-default" ,ghc-data-default)
              ("ghc-safe" ,ghc-safe)
              ("ghc-warp" ,ghc-warp)
              ("ghc-unix-compat" ,ghc-unix-compat)
              ("ghc-conduit-extra" ,ghc-conduit-extra)
              ("ghc-exceptions" ,ghc-exceptions)
              ("ghc-deepseq-generics" ,ghc-deepseq-generics)
              ("ghc-mwc-random" ,ghc-mwc-random)
              ("ghc-primitive" ,ghc-primitive)
              ("ghc-word8" ,ghc-word8)
              ("ghc-auto-update" ,ghc-auto-update)
              ("ghc-semigroups" ,ghc-semigroups)
              ("ghc-byteable" ,ghc-byteable)))
    (native-inputs `(("ghc-hspec" ,ghc-hspec)
                     ("ghc-path-pieces" ,ghc-path-pieces)
                     ("ghc-hunit" ,ghc-hunit)
                     ("ghc-hspec-expectations" ,ghc-hspec-expectations)
                     ("ghc-quickcheck" ,ghc-quickcheck)
                     ("ghc-network" ,ghc-network)
                     ("ghc-async" ,ghc-async)
                     ("ghc-streaming-commons" ,ghc-streaming-commons)
                     ("ghc-wai-extra" ,ghc-wai-extra)))
    (home-page "https://www.yesodweb.com")
    (synopsis "Core package for the Yesod web framework")
    (description "This Haskell package provides all core functionality, for
Yesod, on which other packages can be built.  It provides dispatch, handler
functions, widgets, etc.")
    (license license:expat)))

(define-public ghc-yesod-persistent
  (package
    (name "ghc-yesod-persistent")
    (version "1.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "yesod-persistent-" version "/"
                           "yesod-persistent-" version ".tar.gz"))
       (sha256
        (base32
         "0kiksw46c8ww9yiwl28pkrppx8d6fhsasr0hvmsliqbrp16likj8"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f)) ; FIXME: hspec-discover not available in PATH.
    (inputs `(("ghc-yesod-core" ,ghc-yesod-core)
              ("ghc-persistent" ,ghc-persistent)
              ("ghc-persistent-template" ,ghc-persistent-template)
              ("ghc-blaze-builder" ,ghc-blaze-builder)
              ("ghc-conduit" ,ghc-conduit)
              ("ghc-resourcet" ,ghc-resourcet)
              ("ghc-resource-pool" ,ghc-resource-pool)))
    (native-inputs `(("ghc-hspec" ,ghc-hspec)
                     ("ghc-wai-extra" ,ghc-wai-extra)
                     ("ghc-yesod-core" ,ghc-yesod-core)
                     ("ghc-persistent-sqlite" ,ghc-persistent-sqlite)
                     ("ghc-text" ,ghc-text)))
    (home-page "http://www.yesodweb.com/")
    (synopsis "Helpers for using Persistent from Yesod")
    (description "This Haskell package provides helpers for using Persistent
from Yesod.")
    (license license:expat)))

(define-public ghc-yesod-form
    (package
    (name "ghc-yesod-form")
    (version "1.4.16")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/yesod-form/yesod-form-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "0lij3m5vn8nvh6y88r1dhk03xmmjwmjzazm307nc2wvc5fmx9p2j"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-yesod-core" ,ghc-yesod-core)
        ("ghc-yesod-persistent" ,ghc-yesod-persistent)
        ("ghc-shakespeare" ,ghc-shakespeare)
        ("ghc-persistent" ,ghc-persistent)
        ("ghc-data-default" ,ghc-data-default)
        ("ghc-xss-sanitize" ,ghc-xss-sanitize)
        ("ghc-blaze-builder" ,ghc-blaze-builder)
        ("ghc-email-validate" ,ghc-email-validate)
        ("ghc-text" ,ghc-text)
        ("ghc-wai" ,ghc-wai)
        ("ghc-blaze-html" ,ghc-blaze-html)
        ("ghc-blaze-markup" ,ghc-blaze-markup)
        ("ghc-attoparsec" ,ghc-attoparsec)
        ("ghc-byteable" ,ghc-byteable)
        ("ghc-aeson" ,ghc-aeson)
        ("ghc-resourcet" ,ghc-resourcet)
        ("ghc-semigroups" ,ghc-semigroups)
        ("ghc-network-uri" ,ghc-network-uri)
        ("ghc-hspec" ,ghc-hspec)))
    (home-page "https://www.yesodweb.com")
    (synopsis "Form handling support for Yesod Web Framework")
    (description "This Haskell package provies a set of basic form inputs such
as text, number, time, checkbox, select, textarea, etc through the
@code{Yesod.Form.Fields} module.  Also, there is @code{Yesod.Form.Nic} module
providing richtext field using Nic editor. ")
    (license license:expat)))

(define-public ghc-yesod
  (package
    (name "ghc-yesod")
    (version "1.4.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/yesod/yesod-"
             version ".tar.gz"))
       (sha256
        (base32
         "1sg66nq8yaas2m5nqsdrxricvcizd1ik02zqk60sxh3wna08fz16"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-yesod-core" ,ghc-yesod-core)
       ("ghc-yesod-persistent" ,ghc-yesod-persistent)
       ("ghc-yesod-form" ,ghc-yesod-form)
       ("ghc-monad-control" ,ghc-monad-control)
       ("ghc-wai" ,ghc-wai)
       ("ghc-wai-extra" ,ghc-wai-extra)
       ("ghc-warp" ,ghc-warp)
       ("ghc-blaze-html" ,ghc-blaze-html)
       ("ghc-blaze-markup" ,ghc-blaze-markup)
       ("ghc-aeson" ,ghc-aeson)
       ("ghc-data-default-class" ,ghc-data-default-class)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-yaml" ,ghc-yaml)
       ("ghc-text" ,ghc-text)
       ("ghc-monad-logger" ,ghc-monad-logger)
       ("ghc-fast-logger" ,ghc-fast-logger)
       ("ghc-conduit" ,ghc-conduit)
       ("ghc-conduit-extra" ,ghc-conduit-extra)
       ("ghc-resourcet" ,ghc-resourcet)
       ("ghc-shakespeare" ,ghc-shakespeare)
       ("ghc-streaming-commons" ,ghc-streaming-commons)
       ("ghc-wai-logger" ,ghc-wai-logger)
       ("ghc-semigroups" ,ghc-semigroups)))
    (home-page "https://www.yesodweb.com")
    (synopsis "Framework for creating type-safe, RESTful web applications")
    (description "The Haskell package package groups together the various
Yesod related packages into one cohesive whole.  This is the version of Yesod,
whereas most of the core code lives in @code{ghc-yesod-core}.")
    (license license:expat)))
