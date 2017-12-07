;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2015, 2016 Ricardo Wurmus <rekado@elephly.net>
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
    (version "0.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/tagsoup/tagsoup-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "07pax7i0bl79dmqqz58zy09yrgpnyyr2ya0z183hv96kp65jv0lh"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-text" ,ghc-text)))
    (home-page
     "http://community.haskell.org/~ndm/tagsoup/")
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

(define-public ghc-http-types
  (package
    (name "ghc-http-types")
    (version "0.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/http-types/http-types-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "08w30rf1i7kbh2j1iajqmj6yhhmglnb8kjggc8kdni3xahhrgcss"))))
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
    (version "4000.2.20")
    (outputs '("out" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/HTTP/HTTP-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0nyqdxr5ls2dxkf4a1f3x15xzwdm46ppn99nkcbhswlr6s3cq1s4"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)))
    (inputs
     `(("ghc-old-time" ,ghc-old-time)
       ("ghc-parsec" ,ghc-parsec)
       ("ghc-mtl" ,ghc-mtl)
       ("ghc-network" ,ghc-network)
       ("ghc-network-uri" ,ghc-network-uri)))
    (arguments
     `(#:tests? #f))  ; FIXME: currently missing libraries used for tests.
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
    (version "0.5.7.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "http-client/http-client-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "19cvnnfcjj2m3pgs6ivyjs21rw9wx5ynarh6hvb27a76cscai2fy"))))
    (build-system haskell-build-system)
    ;; Tests require access to the web.
    (arguments `(#:tests? #f))
    (inputs
     `(("ghc-text" ,ghc-text)
       ("ghc-http-types" ,ghc-http-types)
       ("ghc-blaze-builder" ,ghc-blaze-builder)
       ("ghc-data-default-class" ,ghc-data-default-class)
       ("ghc-network" ,ghc-network)
       ("ghc-streaming-commons" ,ghc-streaming-commons)
       ("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-base64-bytestring" ,ghc-base64-bytestring)
       ("ghc-cookie" ,ghc-cookie)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-random" ,ghc-random)
       ("ghc-mime-types" ,ghc-mime-types)
       ("ghc-network-uri" ,ghc-network-uri)
       ("ghc-monad-control" ,ghc-monad-control)
       ("ghc-zlib" ,ghc-zlib)
       ("ghc-async" ,ghc-async)))
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
    (version "2.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "http-conduit-" version "/" "http-conduit-"
                           version ".tar.gz"))
       (sha256 (base32
                "1wcl3lpg4v1ylq9j77j9fmf6l9qbmp8dmj3a9829q19q6bbgza7l"))))
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
       ("ghc-mtl" ,ghc-mtl)
       ("ghc-exceptions" ,ghc-exceptions)))
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
    (version "2.2.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/wai-logger/wai-logger-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1s6svvy3ci4j1dj1jaw8hg628miwj8f5gpy9n8d8hpsaxav6nzgk"))))
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
    (version "3.0.13.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/wai-extra/wai-extra-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0mh761a1bayr4ydwqmh3h8ndpi19zqw34mmy49lp2abr70r0nm1p"))))
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
    (version "0.3.5.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/xss-sanitize/xss-sanitize-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1j2qrn2dbfx01m7zyk9ilgnp9zjwq9mk62b0rdal4zkg4vh212h0"))))
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
    (version "0.1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/css-text/css-text-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1xi1n2f0g8y43p95lynhcg50wxbq7hqfzbfzm7fy8mn7gvd920nw"))))
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
    (version "0.1.0.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "mime-types/mime-types-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "090z3dp928243amnc6s8g10rk2h2bprk9y138q6wj3cpflzr72pw"))))
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
    (version "0.8.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/blaze-html/blaze-html-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1dnw50kh0s405cg9i2y4a8awanhj3bqzk21jwgfza65kcjby7lpq"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f)) ; FIXME: testing libraries are missing.
    (inputs
     `(("ghc-blaze-builder" ,ghc-blaze-builder)
       ("ghc-text" ,ghc-text)
       ("ghc-blaze-markup" ,ghc-blaze-markup)))
    (home-page "http://jaspervdj.be/blaze")
    (synopsis "Fast HTML combinator library")
    (description "This library provides HTML combinators for Haskell.")
    (license license:bsd-3)))

(define-public ghc-aeson
  (package
    (name "ghc-aeson")
    (version "0.10.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/aeson/aeson-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "19kp33rfivr4d3myyr8xn803wd7p8x5nc4wb3qvlgjwgyqjaxvrz"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f)) ; FIXME: testing libraries are missing.
    (inputs
     `(("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-dlist" ,ghc-dlist)
       ("ghc-mtl" ,ghc-mtl)
       ("ghc-scientific" ,ghc-scientific)
       ("ghc-syb" ,ghc-syb)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-vector" ,ghc-vector)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-text" ,ghc-text)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://github.com/bos/aeson")
    (synopsis "Fast JSON parsing and encoding")
    (description "This package provides a JSON parsing and encoding library
for Haskell, optimized for ease of use and high performance.  (A note on
naming: in Greek mythology, Aeson was the father of Jason.)")
    (license license:bsd-3)))

(define-public ghc-aeson-pretty
  (package
    (name "ghc-aeson-pretty")
    (version "0.7.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://hackage.haskell.org/package/aeson-pretty/aeson-pretty-"
                    version ".tar.gz"))
              (sha256
               (base32
                "03ap81853qi8yd9kdgczllrrni23a6glsfxrwj8zab6ipjrbh234"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-aeson" ,ghc-aeson)
       ("ghc-vector" ,ghc-vector)
       ("ghc-text" ,ghc-text)
       ("ghc-unordered-containers"
        ,ghc-unordered-containers)
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
