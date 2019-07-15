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
