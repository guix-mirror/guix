;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Robert Vollmert <rob@vllmrt.net>
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

(define-module (gnu packages haskell-xyz)
  #:use-module (gnu packages)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-check)
  #:use-module (guix build-system haskell)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public ghc-concurrent-extra
  (package
    (name "ghc-concurrent-extra")
    (version "0.7.0.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "concurrent-extra/concurrent-extra-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1y8xk460fvnw0idzdiylmm874sjny4q9jxb1js9fjz8lw2wns3h4"))))
    (build-system haskell-build-system)
    (arguments
     ;; XXX: The ReadWriteLock 'stressTest' fails.
     `(#:tests? #f))
    (inputs
     `(("ghc-unbounded-delays" ,ghc-unbounded-delays)))
    (native-inputs
     `(("ghc-async" ,ghc-async)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-random" ,ghc-random)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)))
    (home-page "https://github.com/basvandijk/concurrent-extra")
    (synopsis "Extra concurrency primitives")
    (description "This Haskell library offers (among other things) the
following selection of synchronisation primitives:

@itemize
@item @code{Broadcast}: Wake multiple threads by broadcasting a value.
@item @code{Event}: Wake multiple threads by signalling an event.
@item @code{Lock}: Enforce exclusive access to a resource.  Also known
as a binary semaphore or mutex.  The package additionally provides an
alternative that works in the STM monad.
@item @code{RLock}: A lock which can be acquired multiple times by the
same thread.  Also known as a reentrant mutex.
@item @code{ReadWriteLock}: Multiple-reader, single-writer locks.  Used
to protect shared resources which may be concurrently read, but only
sequentially written.
@item @code{ReadWriteVar}: Concurrent read, sequential write variables.
@end itemize

Please consult the API documentation of the individual modules for more
detailed information.

This package was inspired by the concurrency libraries of Java and
Python.")
    (license license:bsd-3)))

(define-public ghc-io-streams
  (package
    (name "ghc-io-streams")
    (version "1.5.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "io-streams/io-streams-" version ".tar.gz"))
       (sha256
        (base32
         "12rcdg2d70644bvn838fxcjkssqj8pssnx5y657si5rijcbkgjsx"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-bytestring-builder" ,ghc-bytestring-builder)
       ("ghc-network" ,ghc-network)
       ("ghc-primitive" ,ghc-primitive)
       ("ghc-vector" ,ghc-vector)
       ("ghc-zlib-bindings" ,ghc-zlib-bindings)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)
       ("ghc-zlib" ,ghc-zlib)))
    (arguments
     `(#:cabal-revision
       ("2" "1mcab95d6hm098myh9gp7sh10srigjphgvm8s9pfs7jg5hzghy14")))
    (home-page "http://hackage.haskell.org/package/io-streams")
    (synopsis "Simple and composable stream I/O")
    (description "This library contains simple and easy-to-use
primitives for I/O using streams.")
    (license license:bsd-3)))

(define-public ghc-language-glsl
  (package
    (name "ghc-language-glsl")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "language-glsl/language-glsl-" version ".tar.gz"))
       (sha256
        (base32
         "0hdg67ainlqpjjghg3qin6fg4p783m0zmjqh4rd5gyizwiplxkp1"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-prettyclass" ,ghc-prettyclass)))
    (arguments
     `(#:tests? #f
       #:cabal-revision
       ("1" "10ac9pk4jy75k03j1ns4b5136l4kw8krr2d2nw2fdmpm5jzyghc5")))
    (home-page "http://hackage.haskell.org/package/language-glsl")
    (synopsis "GLSL abstract syntax tree, parser, and pretty-printer")
    (description "This package is a Haskell library for the
representation, parsing, and pretty-printing of GLSL 1.50 code.")
    (license license:bsd-3)))

(define-public ghc-prettyclass
  (package
    (name "ghc-prettyclass")
    (version "1.0.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "prettyclass/prettyclass-" version ".tar.gz"))
       (sha256
        (base32
         "11l9ajci7nh1r547hx8hgxrhq8mh5gdq30pdf845wvilg9p48dz5"))))
    (build-system haskell-build-system)
    (home-page "http://hackage.haskell.org/package/prettyclass")
    (synopsis "Pretty printing class similar to Show")
    (description "This package provides a pretty printing class similar
to @code{Show}, based on the HughesPJ pretty printing library.  It
provides the pretty printing class and instances for the Prelude
types.")
    (license license:bsd-3)))

(define-public ghc-readable
  (package
    (name "ghc-readable")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "readable/readable-" version ".tar.gz"))
       (sha256
        (base32
         "1ja39cg26wy2fs00gi12x7iq5k8i366pbqi3p916skfa5jnkfc3h"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/mightybyte/readable")
    (synopsis "Type class for reading from Text and ByteString")
    (description "This package provides a @code{Readable} type class for
reading data types from @code{ByteString} and @code{Text}.  It also
includes efficient implementations for common data types.")
    (license license:bsd-3)))

(define-public ghc-threads
  (package
    (name "ghc-threads")
    (version "0.5.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "threads/threads-" version ".tar.gz"))
       (sha256
        (base32
         "0bjnjhnq3km6xqk0fn1fgyz5xdw4h6lylbwwbcmkkfzwcz0c76hk"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-concurrent-extra" ,ghc-concurrent-extra)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)))
    (home-page "https://github.com/basvandijk/threads")
    (synopsis "Fork threads and wait for their result")
    (description "This package provides functions to fork threads and
wait for their result, whether it's an exception or a normal value.
Besides waiting for the termination of a single thread this package also
provides functions to wait for a group of threads to terminate.  This
package is similar to the @code{threadmanager}, @code{async} and
@code{spawn} packages.  The advantages of this package are:

@itemize
@item Simpler API.
@item More efficient in both space and time.
@item No space-leak when forking a large number of threads.
@item Correct handling of asynchronous exceptions.
@item GHC specific functionality like @code{forkOn} and
@code{forkIOWithUnmask}.
@end itemize")
    (license license:bsd-3)))

(define-public ghc-zlib-bindings
  (package
    (name "ghc-zlib-bindings")
    (version "0.1.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "zlib-bindings/zlib-bindings-" version ".tar.gz"))
       (sha256
        (base32
         "02ciywlz4wdlymgc3jsnicz9kzvymjw1www2163gxidnz4wb8fy8"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-zlib" ,ghc-zlib)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (arguments
     `(#:cabal-revision
       ("2" "0fq49694gqkab8m0vq4i879blswczwd66n7xh4r4gwiahf0ryvqc")))
    (home-page "https://github.com/snapframework/zlib-bindings")
    (synopsis "Low-level bindings to the @code{zlib} package")
    (description "This package provides low-level bindings to the
@code{zlib} package.")
    (license license:bsd-3)))
