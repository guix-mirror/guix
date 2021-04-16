;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2015, 2016, 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 rsiddharth <s@ricketyspace.net>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Robert Vollmert <rob@vllmrt.net>
;;; Copyright © 2019 John Soo <jsoo1@asu.edu>
;;; Copyright © 2020 Alexandru-Sergiu Marton <brown121407@gmail.com>
;;; Copyright © 2020 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020 Kyle Meyer <kyle@kyleam.com>
;;; Copyright © 2020 Alexandru-Sergiu Marton <brown121407@member.fsf.org>
;;; Copyright © 2020 Giacomo Leidi <goodoldpaul@autistici.org>
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
  #:use-module (gnu packages curl)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-crypto)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (guix build-system haskell)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public ghc-tagsoup
  (package
    (name "ghc-tagsoup")
    (version "0.14.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/tagsoup/"
                           "tagsoup-" version ".tar.gz"))
       (sha256
        (base32
         "1m9sx6gr9y9yxvkmcap8xsks8cnhznvma1mrfl39zljkv005azms"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://github.com/ndmitchell/tagsoup")
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
    (version "0.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/cookie/cookie-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1qy09i0jh2z9i9avy2khf8a8afq4fqgnv0fyrszgfg4kmq2fsi9j"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-old-locale" ,ghc-old-locale)
       ("ghc-blaze-builder" ,ghc-blaze-builder)
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

(define-public ghc-curl
  (package
    (name "ghc-curl")
    (version "1.3.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://hackage/package/curl/curl-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0vj4hpaa30jz7c702xpsfvqaqdxz28zslsqnsfx6bf6dpwvck1wh"))))
    (build-system haskell-build-system)
    (inputs
     `(("curl" ,curl)))
    (home-page "https://hackage.haskell.org/package/curl")
    (synopsis "Haskell bindings for libcurl")
    (description
     "@code{libcurl} is a versatile client-side URL transfer library.
This package provides a Haskell binding to libcurl.")
    (license license:bsd-3)))

(define-public ghc-httpd-shed
  (package
    (name "ghc-httpd-shed")
    (version "0.4.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/httpd-shed/"
                           "httpd-shed-" version ".tar.gz"))
       (sha256
        (base32
         "19dgdimpzr7pxk7pqvyin6j87gmvnf0rm35gzhmna8qr835wy3sr"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-network-bsd" ,ghc-network-bsd)
       ("ghc-network-uri" ,ghc-network-uri)
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
    (version "0.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/http-types/"
                           "http-types-" version ".tar.gz"))
       (sha256
        (base32
         "05j00b9nqmwh9zaq9y9x50k81v2pd3j7a71kd91zlnbl8xk4m2jf"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-doctest" ,ghc-doctest)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-quickcheck-instances" ,ghc-quickcheck-instances)
       ("hspec-discover" ,hspec-discover)))
    (inputs
     `(("ghc-case-insensitive" ,ghc-case-insensitive)))
    (home-page "https://github.com/aristidb/http-types")
    (synopsis "Generic HTTP types for Haskell")
    (description "This package provides generic HTTP types for Haskell (for
both client and server code).")
    (license license:bsd-3)))

(define-public ghc-http
  (package
    (name "ghc-http")
    (version "4000.3.14")
    (outputs '("out" "static" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/HTTP/"
                           "HTTP-" version ".tar.gz"))
       (sha256
        (base32
         "0yv8mbjicpl7l2017c4dhm49117lblgwpy1llv368wci1vrxf0m6"))))
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
       ("ghc-puremd5" ,ghc-puremd5)
       ("ghc-network" ,ghc-network)
       ("ghc-network-uri" ,ghc-network-uri)
       ("ghc-split" ,ghc-split)))
    (arguments
     `(#:tests? #f  ; FIXME: currently missing libraries used for tests.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'create-simple-paths-module
           (lambda _
             (call-with-output-file "Paths_HTTP.hs"
               (lambda (port)
                 (format port "\
{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_HTTP (version) where
import Data.Version (Version(..))
version :: Version
version = Version [~a] []
" (string-map (lambda (chr) (if (eq? chr #\.) #\, chr)) ,version))))
             #t)))))
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
    (version "0.6.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "http-client/http-client-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1n9rnbp8lwkd4whi2anniywi4y1bn9kx6nzfigfvz28d7pn7i4in"))))
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
    (version "0.3.5.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "http-client-tls/http-client-tls-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0qj3pcpgbsfsc4m52dz35khhl4hf1i0nmcpa445z82d9567vy6j7"))))
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
    (version "0.0.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "http-date-" version "/"
                           "http-date-" version ".tar.gz"))
       (sha256
        (base32
         "09slbzqayjnqqz9zybk7slgzvizgplikqgg4b2flzgks91466k0g"))))
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
    (version "1.6.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "http2-" version "/"
                           "http2-" version ".tar.gz"))
       (sha256
        (base32
         "1vlmy8vnp6ml2n2pr11aa5fzigldsscgzmibrni64ykgfvpd3sqn"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-network-byte-order" ,ghc-network-byte-order)
       ("ghc-aeson" ,ghc-aeson)
       ("ghc-aeson-pretty" ,ghc-aeson-pretty)
       ("ghc-base16-bytestring" ,ghc-base16-bytestring)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-vector" ,ghc-vector)
       ("ghc-word8" ,ghc-word8)
       ("ghc-psqueues" ,ghc-psqueues)))
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
    (version "2.3.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "http-conduit-" version "/" "http-conduit-"
                           version ".tar.gz"))
       (sha256
        (base32
         "00rshi1y0h8y4rvsnnad0bppxgpvp40sk7lw1kxmdwy8pi8xrvbs"))))
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
       ("ghc-conduit" ,ghc-conduit)
       ("ghc-utf8-string" ,ghc-utf8-string)
       ("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-lifted-base" ,ghc-lifted-base)
       ("ghc-network" ,ghc-network)
       ("ghc-wai" ,ghc-wai)
       ("ghc-warp" ,ghc-warp)
       ("ghc-wai-conduit" ,ghc-wai-conduit)
       ("ghc-http-types" ,ghc-http-types)
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

(define-public ghc-http-reverse-proxy
  (package
    (name "ghc-http-reverse-proxy")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/http-reverse-proxy/"
             "http-reverse-proxy-" version ".tar.gz"))
       (sha256
        (base32
         "1a6i5njf85b2lhg8m83njagcf09wih5q2irnyb2890s724qr277v"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-http-types" ,ghc-http-types)
       ("ghc-word8" ,ghc-word8)
       ("ghc-blaze-builder" ,ghc-blaze-builder)
       ("ghc-http-client" ,ghc-http-client)
       ("ghc-wai" ,ghc-wai)
       ("ghc-network" ,ghc-network)
       ("ghc-conduit" ,ghc-conduit)
       ("ghc-conduit-extra" ,ghc-conduit-extra)
       ("ghc-wai-logger" ,ghc-wai-logger)
       ("ghc-resourcet" ,ghc-resourcet)
       ("ghc-unliftio" ,ghc-unliftio)
       ("ghc-streaming-commons" ,ghc-streaming-commons)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("ghc-warp" ,ghc-warp)
       ("ghc-http-conduit" ,ghc-http-conduit)))
    (home-page
     "https://github.com/fpco/http-reverse-proxy")
    (synopsis
     "Reverse proxy HTTP requests, either over raw sockets or with WAI")
    (description
     "Provides a simple means of reverse-proxying HTTP requests.  The raw
approach uses the same technique as leveraged by keter, whereas the WAI
approach performs full request/response parsing via WAI and http-conduit.")
    (license license:bsd-3)))

(define-public ghc-wai
  (package
    (name "ghc-wai")
    (version "3.2.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/wai/wai-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "058871axlq6r0gcqxbjw37w57df9xbv81dmz99b1zq59wf329xzy"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-bytestring-builder" ,ghc-bytestring-builder)
       ("ghc-unix-compat" ,ghc-unix-compat)
       ("ghc-vault" ,ghc-vault)
       ("ghc-blaze-builder" ,ghc-blaze-builder)
       ("ghc-network" ,ghc-network)
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
    (version "2.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/wai-logger/wai-logger-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "05gbipyw0672irynsc3wqvvgzqixhmq69ay2mxh2phb734r8bcmm"))))
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
    (version "3.0.28")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/wai-extra/wai-extra-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0iky7k4kirngvk1p2nz19zgzffb5hppfaxdjan80v06ikc8w1wm7"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-ansi-terminal" ,ghc-ansi-terminal)
       ("ghc-base64-bytestring" ,ghc-base64-bytestring)
       ("ghc-cookie" ,ghc-cookie)
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
       ("ghc-http2" ,ghc-http2)
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
    (version "3.0.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "wai-conduit-" version "/"
                           "wai-conduit-" version ".tar.gz"))
       (sha256
        (base32
         "07yn41rn2skd5p3wqqa09wa761vj7ibl8l19gh4bi4i8slxhk417"))))
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

(define-public ghc-bsb-http-chunked
  (package
    (name "ghc-bsb-http-chunked")
    (version "0.0.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/"
             "bsb-http-chunked/bsb-http-chunked-"
             version ".tar.gz"))
       (sha256
        (base32
         "0z0f18yc6zlwh29c6175ivfcin325lvi4irpvv0n3cmq7vi0k0ql"))))
    (build-system haskell-build-system)
    (arguments
     `(;; XXX: As of 0.0.4, one property test ("Identical output as Blaze")
       ;; fails on i686-linux.
       #:tests? ,(not (string-prefix? "i686" (or (%current-target-system)
                                                 (%current-system))))))
    (native-inputs
     `(("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-blaze-builder" ,ghc-blaze-builder)
       ("ghc-hedgehog" ,ghc-hedgehog)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hedgehog" ,ghc-tasty-hedgehog)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-doctest" ,ghc-doctest)))
    (home-page "https://github.com/sjakobi/bsb-http-chunked")
    (synopsis "Chunked HTTP transfer encoding for bytestring builders")
    (description "This Haskell library contains functions for encoding
bytestring builders for chunked Hypertext Transfer Protocol (HTTP) 1.1
transfers.")
    (license license:bsd-3)))

(define-public ghc-warp
  (package
    (name "ghc-warp")
    (version "3.2.28")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "warp-" version "/" "warp-" version
                           ".tar.gz"))
       (sha256
        (base32 "0w2w3aiccpb2f8zssqiszcxzqdysihqi5xply23lwif5arz4saw7"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-async" ,ghc-async)
       ("ghc-auto-update" ,ghc-auto-update)
       ("ghc-bsb-http-chunked" ,ghc-bsb-http-chunked)
       ("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-http-types" ,ghc-http-types)
       ("ghc-iproute" ,ghc-iproute)
       ("ghc-network" ,ghc-network)
       ("ghc-streaming-commons" ,ghc-streaming-commons)
       ("ghc-time-manager" ,ghc-time-manager)
       ("ghc-unix-compat" ,ghc-unix-compat)
       ("ghc-vault" ,ghc-vault)
       ("ghc-wai" ,ghc-wai)
       ("ghc-word8" ,ghc-word8)
       ("ghc-http-date" ,ghc-http-date)
       ("ghc-simple-sendfile" ,ghc-simple-sendfile)
       ("ghc-http2" ,ghc-http2)))
    (native-inputs
     `(("curl" ,curl)
       ("ghc-silently" ,ghc-silently)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-doctest" ,ghc-doctest)
       ("ghc-lifted-base" ,ghc-lifted-base)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-http-client" ,ghc-http-client)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://github.com/yesodweb/wai")
    (synopsis "HTTP server library for Haskell's WAI")
    (description "Warp is a server library for HTTP/1.x and HTTP/2
based WAI (Web Application Interface in Haskell).")
    (license license:expat)))

(define-public ghc-tls-session-manager
  (package
  (name "ghc-tls-session-manager")
  (version "0.0.3")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
            "https://hackage.haskell.org/package/"
            "tls-session-manager/tls-session-manager-"
             version ".tar.gz"))
      (sha256
        (base32
          "0k57flqp2b4bipafiyfipnqmdqv04ky39yr4s4s9sx577zz2j2yi"))))
  (build-system haskell-build-system)
  (inputs
    `(("ghc-auto-update" ,ghc-auto-update)
      ("ghc-clock" ,ghc-clock)
      ("ghc-psqueues" ,ghc-psqueues)
      ("ghc-tls" ,ghc-tls)))
  (home-page "https://hackage.haskell.org/package/tls-session-manager")
  (synopsis "In-memory TLS session manager")
  (description "This Haskell library provides a TLS session manager with
limitation, automatic pruning, energy saving and replay resistance.")
  (license license:bsd-3)))

(define-public ghc-warp-tls
  (package
    (name "ghc-warp-tls")
    (version "3.2.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "warp-tls-" version "/"
                           "warp-tls-" version ".tar.gz"))
       (sha256
        (base32
         "1z5jzl40x1gp249fk8h51gkw6m3hzxchm2bp3kbpqdgmw8r5im8y"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-cryptonite" ,ghc-cryptonite)
       ("ghc-data-default-class" ,ghc-data-default-class)
       ("ghc-network" ,ghc-network)
       ("ghc-streaming-commons" ,ghc-streaming-commons)
       ("ghc-tls" ,ghc-tls)
       ("ghc-tls-session-manager" ,ghc-tls-session-manager)
       ("ghc-wai" ,ghc-wai)
       ("ghc-warp" ,ghc-warp)))
    (home-page "https://github.com/yesodweb/wai")
    (synopsis "SSL/TLS support for Warp")
    (description "This package provides SSL/TLS support for Warp,
a WAI handler, via the native Haskell TLS implementation.")
    (license license:expat)))

(define-public ghc-websockets
  (package
    (name "ghc-websockets")
    (version "0.12.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://hackage/package/websockets/websockets-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1vp3790w3hmr6v96314vdx74f7sg2c7hvnc93gafq0xhbxnr7nvx"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-base64-bytestring" ,ghc-base64-bytestring)
       ("ghc-bytestring-builder" ,ghc-bytestring-builder)
       ("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-network" ,ghc-network)
       ("ghc-random" ,ghc-random)
       ("ghc-sha" ,ghc-sha)
       ("ghc-streaming-commons" ,ghc-streaming-commons)
       ("ghc-entropy" ,ghc-entropy)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)))
    (home-page "https://jaspervdj.be/websockets/")
    (synopsis
     "Write WebSocket-capable servers in Haskell")
    (description
     "This library allows you to write WebSocket-capable servers.

An example server:
@url{https://github.com/jaspervdj/websockets/blob/master/example/server.lhs}
An example client:
@url{https://github.com/jaspervdj/websockets/blob/master/example/client.hs}

See also:
@itemize
@item The specification of the WebSocket protocol:
@url{http://www.whatwg.org/specs/web-socket-protocol/}
@item The JavaScript API for dealing with WebSockets:
@url{http://www.w3.org/TR/websockets/}
@end itemize")
    (license license:bsd-3)))

(define-public ghc-wai-websockets
  (package
    (name "ghc-wai-websockets")
    (version "3.0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://hackage/package/wai-websockets/wai-websockets-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0b2xmdsrsqpssyib53wbr6r8hf75789ndyyanv37sv99iyqcwz4i"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-wai" ,ghc-wai)
       ("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-network" ,ghc-network)
       ("ghc-websockets" ,ghc-websockets)
       ("ghc-http-types" ,ghc-http-types)))
    (arguments
     `(#:configure-flags '("--flags=-example")))
    (home-page "https://github.com/yesodweb/wai")
    (synopsis
     "Provide a bridge between WAI and the websockets package")
    (description
     "Use websockets with WAI applications, primarily those hosted via Warp.")
    (license license:expat)))

(define-public ghc-xss-sanitize
  (package
    (name "ghc-xss-sanitize")
    (version "0.3.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/xss-sanitize/xss-sanitize-"
             version ".tar.gz"))
       (sha256
        (base32
         "1d72s3a6520iwwc1wbn9v2znqgbw6a5wwzb23iq8ny9ccnjyx1dk"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-tagsoup" ,ghc-tagsoup)
       ("ghc-utf8-string" ,ghc-utf8-string)
       ("ghc-css-text" ,ghc-css-text)
       ("ghc-network-uri" ,ghc-network-uri)))
    (native-inputs
     `(("ghc-attoparsec" ,ghc-attoparsec)
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
     `(("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://www.yesodweb.com/")
    (synopsis "CSS parser and renderer")
    (description "This package provides a CSS parser and renderer for
Haskell.")
    (license license:bsd-3)))

(define-public ghc-mime-types
  (package
    (name "ghc-mime-types")
    (version "0.1.0.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "mime-types/mime-types-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1lkipa4v73z3l5lqs6sdhl898iq41kyxv2jb9agsajzgd58l6cha"))))
    (build-system haskell-build-system)
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

(define-public ghc-html-conduit
  (package
    (name "ghc-html-conduit")
    (version "1.3.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/html-conduit/"
             "html-conduit-" version ".tar.gz"))
       (sha256
        (base32
         "196c8zcnjp1pc5qvqxd8arx3xkw0a90rvg9mmiw2l4zwnx65709n"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-resourcet" ,ghc-resourcet)
       ("ghc-conduit" ,ghc-conduit)
       ("ghc-xml-conduit" ,ghc-xml-conduit)
       ("ghc-xml-types" ,ghc-xml-types)
       ("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-conduit-extra" ,ghc-conduit-extra)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("ghc-hunit" ,ghc-hunit)))
    (home-page "https://github.com/snoyberg/xml")
    (synopsis "Parse HTML documents using xml-conduit datatypes")
    (description
     "This package provides a parser for HTML documents that uses
tagstream-conduit.  It automatically balances mismatched tags, so that
there shouldn't be any parse failures.  It does not handle a full HTML
document rendering, such as adding missing html and head tags.  Note that,
since version 1.3.1, it uses an inlined copy of tagstream-conduit with
entity decoding bugfixes applied.")
    (license license:expat)))

(define-public ghc-blaze-html
  (package
    (name "ghc-blaze-html")
    (version "0.9.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "blaze-html/blaze-html-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0k1r1hddjgqighazcazxrx6xfhvy2gm8il8l82ainv3cai13yl30"))))
    (build-system haskell-build-system)
    (outputs '("out" "static" "doc"))
    (inputs
     `(("ghc-blaze-builder" ,ghc-blaze-builder)
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
    (version "1.4.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/aeson/aeson-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1jhabz1lbbv6yqxqiybifi86cb5xlsadrn368n5dd0wzzc7ja4iz"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f)) ; FIXME: testing libraries are missing.
    (outputs '("out" "static" "doc"))
    (inputs
     `(("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-base-compat" ,ghc-base-compat)
       ("ghc-dlist" ,ghc-dlist)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-scientific" ,ghc-scientific)
       ("ghc-tagged" ,ghc-tagged)
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

(define-public ghc-aeson-pretty
  (package
    (name "ghc-aeson-pretty")
    (version "0.8.8")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://hackage.haskell.org/package/aeson-pretty/aeson-pretty-"
                    version ".tar.gz"))
              (sha256
               (base32
                "09n7gs91y1fbw6gjszrd2na3isnvk3y5rsi90lzjrwywnqfadkl1"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-aeson" ,ghc-aeson)
       ("ghc-base-compat" ,ghc-base-compat)
       ("ghc-scientific" ,ghc-scientific)
       ("ghc-vector" ,ghc-vector)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-cmdargs" ,ghc-cmdargs)))
    (home-page "https://github.com/informatikr/aeson-pretty")
    (synopsis "JSON pretty-printing library and command-line tool")
    (description
     "This package provides a JSON pretty-printing library compatible with aeson
as well as a command-line tool to improve readability of streams of JSON data.
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
       ("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-scientific" ,ghc-scientific)
       ("ghc-vector" ,ghc-vector)
       ("ghc-aeson" ,ghc-aeson)
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

(define-public ghc-aeson-better-errors
  (package
    (name "ghc-aeson-better-errors")
    (version "0.9.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://hackage/package/aeson-better-errors/aeson-better-errors-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "09vkyrhwak3bmpfsqcd2az8hfqqkxyhg468hv5avgisy0nzh3w38"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-aeson" ,ghc-aeson)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-dlist" ,ghc-dlist)
       ("ghc-scientific" ,ghc-scientific)
       ("ghc-vector" ,ghc-vector)
       ("ghc-transformers-compat" ,ghc-transformers-compat)
       ("ghc-void" ,ghc-void)))
    (home-page
     "https://github.com/hdgarrood/aeson-better-errors")
    (synopsis
     "Better error messages when decoding JSON values in Haskell")
    (description
     "Gives you the tools to build parsers to decode JSON values, and gives
good error messages when parsing fails.  See also
@url{http://harry.garrood.me/blog/aeson-better-errors/}.")
    (license license:expat)))

(define-public ghc-multipart
  (package
    (name "ghc-multipart")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/multipart/multipart-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1x4n4yyva22dhfr1pg5ki112qvvzb4hyd7bwpm189iq4gcp52q4z"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-stringsearch" ,ghc-stringsearch)))
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
     `(("ghc-utf8-string" ,ghc-utf8-string)
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
    (version "1.6.17.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "yesod-core-" version "/"
                           "yesod-core-" version ".tar.gz"))
       (sha256
        (base32
         "0rcfksbxnwcpg5qh9vjkddv39q95mx4nxzgix51bbwa128hhzcwf"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-wai" ,ghc-wai)
              ("ghc-extra" ,ghc-extra)
              ("ghc-shakespeare" ,ghc-shakespeare)
              ("ghc-blaze-builder" ,ghc-blaze-builder)
              ("ghc-clientsession" ,ghc-clientsession)
              ("ghc-random" ,ghc-random)
              ("ghc-cereal" ,ghc-cereal)
              ("ghc-old-locale" ,ghc-old-locale)
              ("ghc-unliftio" ,ghc-unliftio)
              ("ghc-unordered-containers" ,ghc-unordered-containers)
              ("ghc-monad-control" ,ghc-monad-control)
              ("ghc-transformers-base" ,ghc-transformers-base)
              ("ghc-cookie" ,ghc-cookie)
              ("ghc-http-types" ,ghc-http-types)
              ("ghc-case-insensitive" ,ghc-case-insensitive)
              ("ghc-vector" ,ghc-vector)
              ("ghc-aeson" ,ghc-aeson)
              ("ghc-fast-logger" ,ghc-fast-logger)
              ("ghc-wai-logger" ,ghc-wai-logger)
              ("ghc-monad-logger" ,ghc-monad-logger)
              ("ghc-conduit" ,ghc-conduit)
              ("ghc-resourcet" ,ghc-resourcet)
              ("ghc-rio" ,ghc-rio)
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
    (version "1.6.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "yesod-persistent-" version "/"
                           "yesod-persistent-" version ".tar.gz"))
       (sha256
        (base32
         "1gsiw2zx6z7za7a164h0fxfggkrdqz6fn0qyb2zn9qr7r2jbg1c0"))))
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
                     ("ghc-persistent-sqlite" ,ghc-persistent-sqlite)))
    (home-page "https://www.yesodweb.com/")
    (synopsis "Helpers for using Persistent from Yesod")
    (description "This Haskell package provides helpers for using Persistent
from Yesod.")
    (license license:expat)))

(define-public ghc-yesod-form
    (package
    (name "ghc-yesod-form")
    (version "1.6.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/yesod-form/yesod-form-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "0mny71dyp6cp5akyp5wvmrhmip5rkqi8ibdn3lipvmajx9h58r5d"))))
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
    (description "This Haskell package provides a set of basic form inputs such
as text, number, time, checkbox, select, textarea, etc through the
@code{Yesod.Form.Fields} module.  Also, there is @code{Yesod.Form.Nic} module
providing richtext field using Nic editor. ")
    (license license:expat)))

(define-public ghc-yesod
  (package
    (name "ghc-yesod")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/yesod/yesod-"
             version ".tar.gz"))
       (sha256
        (base32
         "0wx77nbpzdh40p1bm527kimfj48vs9d2avpvvz2w42zi3pz2y94a"))))
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

(define-public ghc-hxt-charproperties
  (package
    (name "ghc-hxt-charproperties")
    (version "9.4.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "hxt-charproperties/hxt-charproperties-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1bk88hj2pqlvcnyfncqyb9j7w9vvdxcq3cgr0w2l09c0abas23pm"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/UweSchmidt/hxt")
    (synopsis "Character properties and classes for XML and Unicode")
    (description
     "The modules provided by this package contain predicates for Unicode
blocks and char properties and character predicates defined by XML.  The
supported Unicode version is 7.0.0")
    (license license:expat)))

(define-public ghc-hxt-unicode
  (package
    (name "ghc-hxt-unicode")
    (version "9.0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/hxt-unicode/hxt-unicode-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0rj48cy8z4fl3zpg5bpa458kqr83adav6jnqv4i71dclpprj6n3v"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-hxt-charproperties" ,ghc-hxt-charproperties)))
    (home-page
     "http://www.fh-wedel.de/~si/HXmlToolbox/index.html https://github.com/UweSchmidt/hxt")
    (synopsis
     "Unicode en-/decoding functions for utf8, iso-latin-* and other encodings")
    (description
     "This package provides Unicode encoding and decoding functions for
encodings used in the Haskell XML Toolbox.  ISO Latin 1-16, utf8, utf16, ASCII
are supported. Decoding is done with lazy functions, errors may be detected or
ignored.")
    (license license:expat)))

(define-public ghc-hxt-regex-xmlschema
  (package
    (name "ghc-hxt-regex-xmlschema")
    (version "9.2.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "hxt-regex-xmlschema/hxt-regex-xmlschema-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1c4jr0439f5yc05h7iz53fa47g6l2wrvqp6gvwf01mlqajk3nx7l"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-hxt-charproperties" ,ghc-hxt-charproperties)
       ("ghc-hunit" ,ghc-hunit)))
    (home-page "https://wiki.haskell.org/Regular_expressions_for_XML_Schema")
    (synopsis "Regular expression library for W3C XML Schema regular expressions")
    (description
     "This library supports full W3C XML Schema regular expressions inclusive
all Unicode character sets and blocks.  It is implemented by the technique of
derivations of regular expressions.")
    (license license:expat)))

(define-public ghc-hxt
  (package
    (name "ghc-hxt")
    (version "9.3.1.18")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/hxt/hxt-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0836k65px3w9c5h1h2bmzq5a7mp6ajxwvfg3pfr2kbxwkgc0j63j"))))
    (build-system haskell-build-system)
    (outputs '("out" "static" "doc"))
    (inputs
     `(("ghc-hxt-charproperties" ,ghc-hxt-charproperties)
       ("ghc-hxt-unicode" ,ghc-hxt-unicode)
       ("ghc-hxt-regex-xmlschema" ,ghc-hxt-regex-xmlschema)
       ("ghc-network-uri" ,ghc-network-uri)))
    (home-page "https://github.com/UweSchmidt/hxt")
    (synopsis "Collection of tools for processing XML with Haskell")
    (description
     "The Haskell XML Toolbox bases on the ideas of HaXml and HXML, but
introduces a more general approach for processing XML with Haskell.")
    (license license:expat)))

(define-public ghc-http-common
  (package
    (name "ghc-http-common")
    (version "0.8.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "http-common/http-common-" version ".tar.gz"))
       (sha256
        (base32
         "14s5a178sb2vm5k00rs21760mds5dz2gs10k9iyn22h01mxyf599"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-base64-bytestring" ,ghc-base64-bytestring)
       ("ghc-blaze-builder" ,ghc-blaze-builder)
       ("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-network" ,ghc-network)
       ("ghc-unordered-containers" ,ghc-unordered-containers)))
    (home-page "https://github.com/afcowie/http-streams/")
    (synopsis "Common types for HTTP clients and servers")
    (description "Base types used by a variety of HTTP clients and
servers.  See http-streams @code{Network.Http.Client} or pipes-http
@code{Pipes.Http.Client} for full documentation.  You can import
@code{Network.Http.Types} if you like, but both http-streams and
pipes-http re-export this package's types and functions.")
    (license license:bsd-3)))

(define-public ghc-http-streams
  (package
    (name "ghc-http-streams")
    (version "0.8.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "http-streams/http-streams-" version ".tar.gz"))
       (sha256
        (base32
         "18vxd35n7s3z4gjvad94bknc8z1w9d7ccgphnhsxlz5cackizmxq"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-base64-bytestring" ,ghc-base64-bytestring)
       ("ghc-blaze-builder" ,ghc-blaze-builder)
       ("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-io-streams" ,ghc-io-streams)
       ("ghc-hsopenssl" ,ghc-hsopenssl)
       ("ghc-openssl-streams" ,ghc-openssl-streams)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-aeson" ,ghc-aeson)
       ("ghc-http-common" ,ghc-http-common)
       ("ghc-network-uri" ,ghc-network-uri)
       ("ghc-network" ,ghc-network)))
    (arguments
     `(#:tests? #f)) ; tests rely on an outdated version of snap-server
    (home-page "https://github.com/afcowie/http-streams/")
    (synopsis "HTTP client using io-streams")
    (description "An HTTP client using the Snap Framework's io-streams
library to handle the streaming IO.  The API is optimized for ease of
use for the rather common case of code needing to query web services and
deal with the result.")
    (license license:bsd-3)))

(define-public ghc-snap-core
  (package
    (name "ghc-snap-core")
    (version "1.0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "snap-core/snap-core-" version ".tar.gz"))
       (sha256
        (base32
         "0dklxgrbqhnb6bc4ic358g4fyj11ywmjrkxxhqcjmci2hhpn00mr"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-old-locale" ,ghc-old-locale)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-bytestring-builder" ,ghc-bytestring-builder)
       ("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-lifted-base" ,ghc-lifted-base)
       ("ghc-io-streams" ,ghc-io-streams)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-monad-control" ,ghc-monad-control)
       ("ghc-random" ,ghc-random)
       ("ghc-readable" ,ghc-readable)
       ("ghc-regex-posix" ,ghc-regex-posix)
       ("ghc-transformers-base" ,ghc-transformers-base)
       ("ghc-unix-compat" ,ghc-unix-compat)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-vector" ,ghc-vector)
       ("ghc-network-uri" ,ghc-network-uri)
       ("ghc-network" ,ghc-network)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-parallel" ,ghc-parallel)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)
       ("ghc-zlib" ,ghc-zlib)))
    (home-page "http://snapframework.com/")
    (synopsis "Haskell Web Framework (core interfaces and types)")
    (description "Snap is a simple and fast web development framework
and server written in Haskell.  For more information, you can visit the
Snap project website at @uref{http://snapframework.com/}.  This library
contains the core definitions and types for the Snap framework.")
    (license license:bsd-3)))

(define-public ghc-snap-server
  (package
    (name "ghc-snap-server")
    (version "1.1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "snap-server/snap-server-" version ".tar.gz"))
       (sha256
        (base32
         "0lw475wp0lnrbgc3jcfif3qjjc3pmrh2k74d8cgpnc1304g6a2s5"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-blaze-builder" ,ghc-blaze-builder)
       ("ghc-bytestring-builder" ,ghc-bytestring-builder)
       ("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-clock" ,ghc-clock)
       ("ghc-io-streams" ,ghc-io-streams)
       ("ghc-io-streams-haproxy" ,ghc-io-streams-haproxy)
       ("ghc-lifted-base" ,ghc-lifted-base)
       ("ghc-network" ,ghc-network)
       ("ghc-old-locale" ,ghc-old-locale)
       ("ghc-snap-core" ,ghc-snap-core)
       ("ghc-unix-compat" ,ghc-unix-compat)
       ("ghc-vector" ,ghc-vector)))
    (native-inputs
     `(("ghc-base16-bytestring" ,ghc-base16-bytestring)
       ("ghc-monad-control" ,ghc-monad-control)
       ("ghc-random" ,ghc-random)
       ("ghc-threads" ,ghc-threads)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-http-streams" ,ghc-http-streams)
       ("ghc-http-common" ,ghc-http-common)
       ("ghc-parallel" ,ghc-parallel)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)))
    (arguments
     `(#:cabal-revision
       ("1" "094b7ll47lxd4lvr6kd59jyw0vz686gw5cx16w758d6fli0cy3x3")))
    (home-page "http://snapframework.com/")
    (synopsis "Web server for the Snap Framework")
    (description "Snap is a simple and fast web development framework
and server written in Haskell.  For more information, you can visit the
Snap project website at @uref{http://snapframework.com/}.  The Snap HTTP
server is a high performance web server library written in Haskell.
Together with the snap-core library upon which it depends, it provides a
clean and efficient Haskell programming interface to the HTTP
protocol.")
    (license license:bsd-3)))

(define-public ghc-js-jquery
  (package
    (name "ghc-js-jquery")
    (version "3.3.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://hackage.haskell.org/package/js-jquery/js-jquery-"
         version ".tar.gz"))
       (sha256
        (base32
         "16q68jzbs7kp07dnq8cprdcc8fd41rim38039vg0w4x11lgniq70"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f)) ; tests do network IO
    (home-page "https://github.com/ndmitchell/js-jquery")
    (synopsis "Obtain minified jQuery code")
    (description "This package bundles the minified
@url{http://jquery.com/, jQuery} code into a Haskell package, so it can
be depended upon by Cabal packages.  The first three components of the
version number match the upstream jQuery version.  The package is
designed to meet the redistribution requirements of downstream
users (e.g. Debian).")
    (license license:expat)))

(define-public ghc-js-flot
  (package
    (name "ghc-js-flot")
    (version "0.8.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://hackage.haskell.org/package/js-flot/js-flot-"
         version ".tar.gz"))
       (sha256
        (base32
         "0yjyzqh3qzhy5h3nql1fckw0gcfb0f4wj9pm85nafpfqp2kg58hv"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-http" ,ghc-http)))
    (home-page "https://github.com/ndmitchell/js-flot")
    (synopsis "Obtain minified flot code")
    (description "This package bundles the minified
@url{http://www.flotcharts.org/, Flot} code (a jQuery plotting library)
into a Haskell package, so it can be depended upon by Cabal packages.
The first three components of the version number match the upstream flot
version.  The package is designed to meet the redistribution
requirements of downstream users (e.g. Debian).")
    (license license:expat)))

(define-public ghc-happstack-server
  (package
    (name "ghc-happstack-server")
    (version "7.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/happstack-server/happstack-server-"
             version ".tar.gz"))
       (sha256
        (base32
         "0i7csvmwv7n68gkwqzi985p2mjdgzipjnlj873sdiknhx9pfmq70"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-network" ,ghc-network)
       ("ghc-network-bsd" ,ghc-network-bsd)
       ("ghc-network-uri" ,ghc-network-uri)
       ("ghc-base64-bytestring" ,ghc-base64-bytestring)
       ("ghc-blaze-html" ,ghc-blaze-html)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-extensible-exceptions"
        ,ghc-extensible-exceptions)
       ("ghc-hslogger" ,ghc-hslogger)
       ("ghc-html" ,ghc-html)
       ("ghc-monad-control" ,ghc-monad-control)
       ("ghc-old-locale" ,ghc-old-locale)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-sendfile" ,ghc-sendfile)
       ("ghc-system-filepath" ,ghc-system-filepath)
       ("ghc-syb" ,ghc-syb)
       ("ghc-threads" ,ghc-threads)
       ("ghc-transformers-base" ,ghc-transformers-base)
       ("ghc-transformers-compat"
        ,ghc-transformers-compat)
       ("ghc-utf8-string" ,ghc-utf8-string)
       ("ghc-zlib" ,ghc-zlib)))
    (native-inputs `(("ghc-hunit" ,ghc-hunit)))
    (home-page "http://happstack.com")
    (synopsis "Web related tools and services for Haskell")
    (description
     "Happstack Server provides an HTTP server and a rich set of functions for
routing requests, handling query parameters, generating responses, working with
cookies, serving files, and more.")
    (license license:bsd-3)))

(define-public ghc-sendfile
  (package
    (name "ghc-sendfile")
    (version "0.7.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/sendfile/sendfile-"
             version ".tar.gz"))
       (sha256
        (base32
         "0988snmx3bylpw3kcq8hsgji8idc6xcrcfp275qjv3apfdgc9rp0"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-network" ,ghc-network)))
    (home-page
     "https://hub.darcs.net/stepcut/sendfile")
    (synopsis "Portable sendfile library for Haskell")
    (description
     "Haskell library which exposes zero-copy sendfile functionality in a portable way.")
    (license license:bsd-3)))

(define-public ghc-scalpel-core
  (package
    (name "ghc-scalpel-core")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/scalpel-core/"
               "scalpel-core-" version ".tar.gz"))
        (sha256
         (base32
          "1qf0gnidyh8zk0acj99vn6hsj37m410lrm50sqpiv1i36rpmmsqh"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-data-default" ,ghc-data-default)
       ("ghc-fail" ,ghc-fail)
       ("ghc-pointedlist" ,ghc-pointedlist)
       ("ghc-regex-base" ,ghc-regex-base)
       ("ghc-regex-tdfa" ,ghc-regex-tdfa)
       ("ghc-tagsoup" ,ghc-tagsoup)
       ("ghc-vector" ,ghc-vector)))
    (native-inputs `(("ghc-hunit" ,ghc-hunit)))
    (home-page "https://github.com/fimad/scalpel")
    (synopsis
     "High level web scraping library for Haskell")
    (description
     "Scalpel core provides a subset of the scalpel web scraping library
that is intended to have lightweight dependencies and to be free of all
non-Haskell dependencies.")
    (license license:asl2.0)))

(define-public ghc-scalpel
  (package
    (name "ghc-scalpel")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/scalpel/"
               "scalpel-" version ".tar.gz"))
        (sha256
         (base32
          "0jbrfcgljl8kbcwi2zqx1jp3c3dpxrkc94za44x56kcz68n89hlz"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-scalpel-core" ,ghc-scalpel-core)
       ("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-data-default" ,ghc-data-default)
       ("ghc-http-client" ,ghc-http-client)
       ("ghc-http-client-tls" ,ghc-http-client-tls)
       ("ghc-tagsoup" ,ghc-tagsoup)))
    (home-page "https://github.com/fimad/scalpel")
    (synopsis
     "High level web scraping library for Haskell")
    (description
     "Scalpel is a web scraping library inspired by libraries like Parsec
and Perl's @code{Web::Scraper}.  Scalpel builds on top of TagSoup to provide a
declarative and monadic interface.")
    (license license:asl2.0)))

(define-public ghc-sourcemap
  (package
    (name "ghc-sourcemap")
    (version "0.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://hackage/package/sourcemap/sourcemap-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0ynfm44ym8y592wnzdwa0d05dbkffyyg5sm26y5ylzpynk64r85r"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-aeson" ,ghc-aeson)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-utf8-string" ,ghc-utf8-string)))
    (arguments
     `(#:tests? #f ; FIXME: Fail to compile
       #:cabal-revision
       ("1" "1f7q44ar6qfip8fsllg43jyn7r15ifn2r0vz32cbmx0sb0d38dax")))
    (home-page
     "http://hackage.haskell.org/package/sourcemap")
    (synopsis
     "Implementation of source maps as proposed by Google and Mozilla")
    (description
     "Sourcemap provides an implementation of source maps, revision 3,
proposed by Google and Mozilla here
@url{https://wiki.mozilla.org/DevTools/Features/SourceMap}.")
    (license license:bsd-3)))

(define-public ghc-language-javascript
  (package
    (name "ghc-language-javascript")
    (version "0.7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://hackage/package/language-javascript/language-javascript-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "15bpqpkjf2y3fk8wff9zlnkpsjc63bnbvhlkxrs9alj0bikq17nk"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-blaze-builder" ,ghc-blaze-builder)
       ("ghc-utf8-string" ,ghc-utf8-string)))
    (native-inputs
     `(("ghc-alex" ,ghc-alex)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-happy" ,ghc-happy)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-utf8-light" ,ghc-utf8-light)))
    (home-page
     "https://github.com/erikd/language-javascript")
    (synopsis "Parser for JavaScript")
    (description
     "Parses Javascript into an Abstract Syntax Tree (AST).  Initially intended
as frontend to hjsmin.")
    (license license:bsd-3)))

(define-public ghc-bower-json
  (package
    (name "ghc-bower-json")
    (version "1.0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://hackage/package/bower-json/bower-json-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0wvygg3rdbxzrmr61a9w6ddv9pfric85ih8hnxyk0ydzn7i59abs"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-aeson" ,ghc-aeson)
       ("ghc-aeson-better-errors" ,ghc-aeson-better-errors)
       ("ghc-scientific" ,ghc-scientific)
       ("ghc-transformers" ,ghc-transformers)
       ("ghc-unordered-containers" ,ghc-unordered-containers)))
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)))
    (home-page "https://github.com/hdgarrood/bower-json")
    (synopsis "Read bower.json from Haskell")
    (description
     "This package provides a data type and ToJSON/FromJSON instances for
Bower's package manifest file, bower.json.")
    (license license:expat)))

(define-public ghc-dav
  (package
    (name "ghc-dav")
    (version "1.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/DAV/DAV-"
                           version ".tar.gz"))
       (sha256
        (base32 "1isvi4fahq70lzxfz23as7qzkc01g7kba568l6flrgd0j1984fsy"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-data-default" ,ghc-data-default)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-http-client" ,ghc-http-client)
       ("ghc-http-client-tls" ,ghc-http-client-tls)
       ("ghc-http-types" ,ghc-http-types)
       ("ghc-lens" ,ghc-lens)
       ("ghc-transformers-base" ,ghc-transformers-base)
       ("ghc-transformers-compat" ,ghc-transformers-compat)
       ("ghc-utf8-string" ,ghc-utf8-string)
       ("ghc-xml-conduit" ,ghc-xml-conduit)
       ("ghc-xml-hamlet" ,ghc-xml-hamlet)
       ("ghc-network" ,ghc-network)
       ("ghc-network-uri" ,ghc-network-uri)
       ("ghc-optparse-applicative" ,ghc-optparse-applicative)))
    (home-page "http://floss.scru.org/hDAV")
    (synopsis "RFC 4918 WebDAV support")
    (description "This package provides a library for the Web Distributed
Authoring and Versioning (WebDAV) extensions to HTTP as well an executable,
@command{hdav}, for command-line operation.")
    (license license:gpl3)))

(define-public ghc-yesod-test
  (package
    (name "ghc-yesod-test")
    (version "1.6.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "yesod-test/yesod-test-" version ".tar.gz"))
       (sha256
        (base32
         "0v25lqq7hgd5lggnyfd9kamkzd4126rd7vlhc131n253n4by4yak"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-aeson" ,ghc-aeson)
       ("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-blaze-builder" ,ghc-blaze-builder)
       ("ghc-blaze-html" ,ghc-blaze-html)
       ("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-conduit" ,ghc-conduit)
       ("ghc-cookie" ,ghc-cookie)
       ("ghc-hspec-core" ,ghc-hspec-core)
       ("ghc-html-conduit" ,ghc-html-conduit)
       ("ghc-http-types" ,ghc-http-types)
       ("ghc-network" ,ghc-network)
       ("ghc-memory" ,ghc-memory)
       ("ghc-pretty-show" ,ghc-pretty-show)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-wai" ,ghc-wai)
       ("ghc-wai-extra" ,ghc-wai-extra)
       ("ghc-xml-conduit" ,ghc-xml-conduit)
       ("ghc-xml-types" ,ghc-xml-types)
       ("ghc-yesod-core" ,ghc-yesod-core)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("ghc-yesod-form" ,ghc-yesod-form)
       ("ghc-unliftio" ,ghc-unliftio)
       ("ghc-unliftio-core" ,ghc-unliftio-core)))
    (home-page "https://www.yesodweb.com")
    (synopsis "Integration testing for WAI/Yesod Applications")
    (description "This package's main goal is to encourage integration
and system testing of web applications by making everything easy to
test.  Tests are like browser sessions that keep track of cookies and
the last visited page.  You can perform assertions on the content of
HTML responses using CSS selectors.")
    (license license:expat)))

(define-public ghc-wai-app-static
  (package
    (name "ghc-wai-app-static")
    (version "3.1.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "wai-app-static/wai-app-static-"
                           version ".tar.gz"))
       (sha256
        (base32
         "10k6jb450p89r6dgpnwh428gg0wfw2qbx9n126jkvbchcjr1f4v8"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-wai" ,ghc-wai)
       ("ghc-http-types" ,ghc-http-types)
       ("ghc-unix-compat" ,ghc-unix-compat)
       ("ghc-old-locale" ,ghc-old-locale)
       ("ghc-file-embed" ,ghc-file-embed)
       ("ghc-cryptonite" ,ghc-cryptonite)
       ("ghc-memory" ,ghc-memory)
       ("ghc-http-date" ,ghc-http-date)
       ("ghc-blaze-html" ,ghc-blaze-html)
       ("ghc-blaze-markup" ,ghc-blaze-markup)
       ("ghc-mime-types" ,ghc-mime-types)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-zlib" ,ghc-zlib)
       ("ghc-wai-extra" ,ghc-wai-extra)
       ("ghc-optparse-applicative" ,ghc-optparse-applicative)
       ("ghc-warp" ,ghc-warp)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("ghc-network" ,ghc-network)
       ("ghc-temporary" ,ghc-temporary)
       ("ghc-mockery" ,ghc-mockery)))
    (arguments
     `(#:cabal-revision
       ("1" "0bkmml30rzifvb7nxddj3pxczk0kniahra19mjn0qrkzy1n5752p")))
    (home-page "https://www.yesodweb.com/book/web-application-interface")
    (synopsis "WAI application for static serving")
    (description "This package provides a Web Application
Interface (WAI) application for static serving.  It also provides some
helper functions and datatypes for use outside of WAI.")
    (license license:expat)))

(define-public ghc-hjsmin
  (package
    (name "ghc-hjsmin")
    (version "0.2.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "hjsmin/hjsmin-" version ".tar.gz"))
       (sha256
        (base32
         "1r2p5rjdjr25j3w4s57q5hxw2c3ymw12x7ms18yvglnq2ivr9fc1"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-dist-directory-for-tests
           (lambda _
             (substitute* '("test/test-cli.hs" "test/cli/core/runner")
               (("dist-newstyle") "dist")))))))
    (inputs
     `(("ghc-language-javascript" ,ghc-language-javascript)
       ("ghc-optparse-applicative" ,ghc-optparse-applicative)))
    (native-inputs
     `(("ghc-extra" ,ghc-extra)))
    (home-page "https://github.com/erikd/hjsmin")
    (synopsis "Haskell implementation of a JavaScript minifier")
    (description "This library provides tools reduce the size of
JavaScript files by stripping out extraneous whitespace and other
syntactic elements, without changing the semantics.")
    (license license:bsd-3)))

(define-public ghc-yesod-static
  (package
    (name "ghc-yesod-static")
    (version "1.6.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "yesod-static/yesod-static-" version ".tar.gz"))
       (sha256
        (base32
         "1jd0ryfr2vyrwasyvbqmc6j4ngv1lgz78w427f169l7gyl1firxb"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-async" ,ghc-async)
       ("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-base64-bytestring" ,ghc-base64-bytestring)
       ("ghc-blaze-builder" ,ghc-blaze-builder)
       ("ghc-conduit" ,ghc-conduit)
       ("ghc-cryptonite" ,ghc-cryptonite)
       ("ghc-cryptonite-conduit" ,ghc-cryptonite-conduit)
       ("ghc-css-text" ,ghc-css-text)
       ("ghc-data-default" ,ghc-data-default)
       ("ghc-file-embed" ,ghc-file-embed)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-hjsmin" ,ghc-hjsmin)
       ("ghc-http-types" ,ghc-http-types)
       ("ghc-memory" ,ghc-memory)
       ("ghc-mime-types" ,ghc-mime-types)
       ("ghc-unix-compat" ,ghc-unix-compat)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-wai" ,ghc-wai)
       ("ghc-wai-app-static" ,ghc-wai-app-static)
       ("ghc-yesod-core" ,ghc-yesod-core)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("ghc-yesod-test" ,ghc-yesod-test)
       ("ghc-wai-extra" ,ghc-wai-extra)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-rio" ,ghc-rio)))
    (home-page "https://www.yesodweb.com/")
    (synopsis "Static file serving subsite for Yesod")
    (description "This package provides a static file serving subsite
for the Yesod Web Framework.")
    (license license:expat)))

(define-public ghc-wai-handler-launch
  (package
    (name "ghc-wai-handler-launch")
    (version "3.0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "wai-handler-launch/wai-handler-launch-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1ifqgyc1ccig5angh5l1iq7vyms4lvi8wzvysg5dw82nml49n02m"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-wai" ,ghc-wai)
       ("ghc-warp" ,ghc-warp)
       ("ghc-http-types" ,ghc-http-types)
       ("ghc-streaming-commons" ,ghc-streaming-commons)
       ("ghc-async" ,ghc-async)))
    (home-page "https://hackage.haskell.org/package/wai-handler-launch")
    (synopsis "Launch a Web application in the default browser")
    (description "This package handles cross-platform Web browser
launching and inserts JavaScript code to ping the server.  When the
server no longer receives pings, it shuts down.")
    (license license:expat)))

(define-public ghc-wai-cors
  (package
    (name "ghc-wai-cors")
    (version "0.2.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "wai-cors/wai-cors-" version ".tar.gz"))
       (sha256
        (base32
         "10gv3jjlkcb13031frr818p56v2s0qf6dqjsfpwlhwdxdssvx5r5"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; As of version 0.2.7, there are two test suites: "unit-tests"
         ;; and "phantomjs".  Since we do not have a PhantomJS package,
         ;; we only run the unit tests.
         (replace 'check
           (lambda _
             (invoke "runhaskell" "Setup.hs" "test" "unit-tests"))))))
    (inputs
     `(("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-base-unicode-symbols" ,ghc-base-unicode-symbols)
       ("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-http-types" ,ghc-http-types)
       ("ghc-wai" ,ghc-wai)))
    (native-inputs
     `(("ghc-network" ,ghc-network)
       ("ghc-wai-websockets" ,ghc-wai-websockets)
       ("ghc-warp" ,ghc-warp)
       ("ghc-websockets" ,ghc-websockets)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-wai-extra" ,ghc-wai-extra)
       ("ghc-wai-websockets" ,ghc-wai-websockets)
       ("ghc-warp" ,ghc-warp)
       ("ghc-websockets" ,ghc-websockets)))
    (home-page "https://github.com/larskuhtz/wai-cors")
    (synopsis "Cross-Origin Resource Sharing (CORS) for WAI")
    (description "This package provides an implementation of Cross-Origin
Resource Sharing (CORS) for the Web Application Framework (WAI) that
aims to be compliant with @url{https://www.w3.org/TR/cors}.")
    (license license:expat)))
