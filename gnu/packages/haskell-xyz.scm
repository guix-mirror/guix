;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015 Siniša Biđin <sinisa@bidin.eu>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2015, 2019 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2016, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2015, 2016, 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017 David Craven <david@craven.ch>
;;; Copyright © 2017 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2017 Peter Mikkelsen <petermikkelsen10@gmail.com>
;;; Copyright © 2017, 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2017 rsiddharth <s@ricketyspace.net>
;;; Copyright © 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Tonton <tonton@riseup.net>
;;; Copyright © 2018, 2019 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2018, 2019 Gabriel Hondet <gabrielhondet@gmail.com>
;;; Copyright © 2019 Robert Vollmert <rob@vllmrt.net>
;;; Copyright © 2019 Jacob MacDonald <jaccarmac@gmail.com>
;;; Copyright © 2019,2020 John Soo <jsoo1@asu.edu>
;;; Copyright © 2019, 2020 Kyle Meyer <kyle@kyleam.com>
;;; Copyright © 2019 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 JoJo <jo@jo.zone>
;;; Copyright © 2020, 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Alexandru-Sergiu Marton <brown121407@member.fsf.org>
;;; Copyright © 2020 Carlo Holl <carloholl@gmail.com>
;;; Copyright © 2020 Christopher Lemmer Webber <cwebber@dustycloud.org>
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
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-apps)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-crypto)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system haskell)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1))

(define-public ghc-abstract-deque
  (package
    (name "ghc-abstract-deque")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "abstract-deque-" version "/"
                           "abstract-deque-" version ".tar.gz"))
       (sha256
        (base32
         "18jwswjxwzc9bjiy4ds6hw2a74ki797jmfcifxd2ga4kh7ri1ah9"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-random" ,ghc-random)))
    (home-page "https://github.com/rrnewton/haskell-lockfree/wiki")
    (synopsis "Abstract, parameterized interface to mutable Deques for Haskell")
    (description "This Haskell package provides an abstract interface to
highly-parameterizable queues/deques.

Background: There exists a feature space for queues that extends between:

@itemize
@item Simple, single-ended, non-concurrent, bounded queues

@item Double-ended, thread-safe, growable queues with important points
in between (such as the queues used for work stealing).
@end itemize

This package includes an interface for Deques that allows the programmer
to use a single API for all of the above, while using the type system to
select an efficient implementation given the requirements (using type families).

This package also includes a simple reference implementation based on
@code{IORef} and @code{Data.Sequence}.")
    (license license:bsd-3)))

(define-public ghc-abstract-par
  (package
    (name "ghc-abstract-par")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "abstract-par-" version "/"
                           "abstract-par-" version ".tar.gz"))
       (sha256
        (base32
         "0q6qsniw4wks2pw6wzncb1p1j3k6al5njnvm2v5n494hplwqg2i4"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/simonmar/monad-par")
    (synopsis "Abstract parallelization interface for Haskell")
    (description "This Haskell package is an abstract interface
only.  It provides a number of type clasess, but not an
implementation.  The type classes separate different levels
of @code{Par} functionality.  See the @code{Control.Monad.Par.Class}
module for more details.")
    (license license:bsd-3)))

(define-public ghc-active
  (package
    (name "ghc-active")
    (version "0.2.0.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "active/active-" version ".tar.gz"))
       (sha256
        (base32
         "0x3b4ln6csa554qls28wbxvclkbdz3yi60i1m0q5ing0cs16fifz"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-vector" ,ghc-vector)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-semigroupoids" ,ghc-semigroupoids)
       ("ghc-lens" ,ghc-lens)
       ("ghc-linear" ,ghc-linear)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://hackage.haskell.org/package/active")
    (synopsis "Abstractions for animation")
    (description "This package defines an @code{Active} abstraction for
time-varying values with finite start and end times.  It is used for
describing animations within the
@url{https://archives.haskell.org/projects.haskell.org/diagrams/,
diagrams framework}.")
    (license license:bsd-3)))

(define-public ghc-adjunctions
  (package
    (name "ghc-adjunctions")
    (version "4.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/adjunctions/adjunctions-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1sbal7cbhm12crfnfhkk322jnzgx7lhw3jzq0p463bipagsjwz2h"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("2" "1yfsjx7dqikg3hvld7i91xfsg5lawmr5980lvfd794sybmgxsf17")))
    (inputs
     `(("ghc-profunctors" ,ghc-profunctors)
       ("ghc-comonad" ,ghc-comonad)
       ("ghc-contravariant" ,ghc-contravariant)
       ("ghc-distributive" ,ghc-distributive)
       ("ghc-free" ,ghc-free)
       ("ghc-tagged" ,ghc-tagged)
       ("ghc-semigroupoids" ,ghc-semigroupoids)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-transformers-compat" ,ghc-transformers-compat)
       ("ghc-void" ,ghc-void)))
    (native-inputs
     `(("ghc-generic-deriving" ,ghc-generic-deriving)
       ("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://github.com/ekmett/adjunctions/")
    (synopsis "Adjunctions and representable functors")
    (description "This library provides adjunctions and representable functors
for Haskell.")
    (license license:bsd-3)))

(define-public ghc-aeson-compat
  (package
    (name "ghc-aeson-compat")
    (version "0.3.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "aeson-compat-" version "/"
                           "aeson-compat-" version ".tar.gz"))
       (sha256
        (base32
         "1j13gykv4ryvmr14w5blz0nnpdb4p0hpa27wahw3mhb1lwdr8hz0"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f))  ;  FIXME: Tests require QuickCheck >= 2.10
    (inputs `(("ghc-base-compat" ,ghc-base-compat)
              ("ghc-aeson" ,ghc-aeson)
              ("ghc-attoparsec" ,ghc-attoparsec)
              ("ghc-attoparsec" ,ghc-attoparsec-iso8601)
              ("ghc-exceptions" ,ghc-exceptions)
              ("ghc-hashable" ,ghc-hashable)
              ("ghc-scientific" ,ghc-scientific)
              ("ghc-time-locale-compat" ,ghc-time-locale-compat)
              ("ghc-unordered-containers" ,ghc-unordered-containers)
              ("ghc-vector" ,ghc-vector)
              ("ghc-tagged" ,ghc-tagged)
              ("ghc-semigroups" ,ghc-semigroups)
              ("ghc-nats" ,ghc-nats)))
    (home-page "https://github.com/phadej/aeson-compat")
    (synopsis "Compatibility layer for ghc-aeson")
    (description "This Haskell package provides compatibility layer for
ghc-aeson.")
    (license license:bsd-3)))

(define-public ghc-aeson-diff
  (package
    (name "ghc-aeson-diff")
    (version "1.1.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "aeson-diff/aeson-diff-" version ".tar.gz"))
       (sha256
        (base32
         "01d48pd7d1mb9cd5yxfajln8rmjdjq8ch91s0lav4qw1azv6vp2r"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-aeson" ,ghc-aeson)
       ("ghc-edit-distance-vector" ,ghc-edit-distance-vector)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-scientific" ,ghc-scientific)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-vector" ,ghc-vector)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-optparse-applicative" ,ghc-optparse-applicative)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-quickcheck-instances" ,ghc-quickcheck-instances)
       ("ghc-glob" ,ghc-glob)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-quickcheck-instances" ,ghc-quickcheck-instances)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-doctest" ,ghc-doctest)
       ("hlint" ,hlint)))
    (home-page "https://github.com/thsutton/aeson-diff")
    (synopsis "Extract and apply patches to JSON documents")
    (description "This is a small library for working with changes to JSON
documents.  It includes a library and two command-line executables in the
style of the @command{diff} and @command{patch} commands available on many
systems.")
    (license license:bsd-3)))

(define-public ghc-alex
  (package
    (name "ghc-alex")
    (version "3.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/alex/alex-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0cpjixgsr0b2x4s6hz4aa6gnmjw9i7xd9nlfi8m37zqlidq4v3nm"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-check-variables
           (lambda _
             (setenv "PATH" (string-append (getcwd) "/dist/build/alex:"
                                           (getenv "PATH")))
             (setenv "alex_datadir" (string-append (getcwd) "/data"))
             #t)))))
    (inputs `(("ghc-quickcheck" ,ghc-quickcheck)))
    (native-inputs
     `(("which" ,which)))
    (home-page "https://www.haskell.org/alex/")
    (synopsis
     "Tool for generating lexical analysers in Haskell")
    (description
     "Alex is a tool for generating lexical analysers in Haskell.  It takes a
description of tokens based on regular expressions and generates a Haskell
module containing code for scanning text efficiently.  It is similar to the
tool lex or flex for C/C++.")
    (license license:bsd-3)))

(define-public ghc-alsa-core
  (package
    (name "ghc-alsa-core")
    (version "0.5.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://hackage/package/alsa-core/alsa-core-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1avh4a419h9d2zsslg6j8hm87ppgsgqafz8ll037rk2yy1g4jl7b"))))
    (build-system haskell-build-system)
    (arguments
     `(#:extra-directories ("alsa-lib")))
    (inputs
     `(("ghc-extensible-exceptions" ,ghc-extensible-exceptions)
       ("alsa-lib" ,alsa-lib)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://wiki.haskell.org/ALSA")
    (synopsis "Binding to the ALSA Library API (Exceptions)")
    (description "This package provides access to ALSA infrastructure, that is
needed by both alsa-seq and alsa-pcm.")
    (license license:bsd-3)))

(define-public ghc-alsa-mixer
  (package
   (name "ghc-alsa-mixer")
   (version "0.3.0")
   (source
    (origin
     (method url-fetch)
     (uri
      (string-append
       "mirror://hackage/package/alsa-mixer/alsa-mixer-"
       version ".tar.gz"))
     (sha256
      (base32
       "00ny2p3276jilidjs44npc8zmbhynz3f2lpmlwwl6swwx5yijsnb"))))
   (build-system haskell-build-system)
   (inputs `(("ghc-alsa-core" ,ghc-alsa-core)))
   (native-inputs `(("ghc-c2hs" ,ghc-c2hs)))
   (home-page "https://github.com/ttuegel/alsa-mixer")
   (synopsis "Bindings to the ALSA simple mixer API")
   (description
    "This package provides bindings to the ALSA simple mixer API.")
   (license license:bsd-3)))

(define-public ghc-annotated-wl-pprint
  (package
    (name "ghc-annotated-wl-pprint")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/annotated-wl-pprint"
             "/annotated-wl-pprint-" version
             ".tar.gz"))
       (sha256
        (base32
         "061xfz6qany3wf95csl8dcik2pz22cn8iv1qchhm16isw5zjs9hc"))))
    (build-system haskell-build-system)
    (home-page
     "https://github.com/david-christiansen/annotated-wl-pprint")
    (synopsis
     "The Wadler/Leijen Pretty Printer, with annotation support")
    (description "This is a modified version of wl-pprint, which was based on
Wadler's paper \"A Prettier Printer\".  This version allows the library user
to annotate the text with semantic information, which can later be rendered in
a variety of ways.")
    (license license:bsd-3)))

(define-public ghc-ansi-terminal
  (package
    (name "ghc-ansi-terminal")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/ansi-terminal/ansi-terminal-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1yr0ld0kqns3w3j9gl62bdwshvyazidx4dv1qkvq19ivnf08w23l"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-colour" ,ghc-colour)))
    (home-page "https://github.com/feuerbach/ansi-terminal")
    (synopsis "ANSI terminal support for Haskell")
    (description "This package provides ANSI terminal support for Haskell.  It
allows cursor movement, screen clearing, color output showing or hiding the
cursor, and changing the title.")
    (license license:bsd-3)))

(define-public ghc-ansi-wl-pprint
  (package
    (name "ghc-ansi-wl-pprint")
    (version "0.6.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "ansi-wl-pprint/ansi-wl-pprint-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1b2fg8px98dzbaqyns10kvs8kn6cl1hdq5wb9saz40izrpkyicm7"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-ansi-terminal" ,ghc-ansi-terminal)))
    (home-page "https://github.com/ekmett/ansi-wl-pprint")
    (synopsis "Wadler/Leijen Pretty Printer for colored ANSI terminal output")
    (description "This is a pretty printing library based on Wadler's paper
\"A Prettier Printer\".  It has been enhanced with support for ANSI terminal
colored output using the ansi-terminal package.")
    (license license:bsd-3)))

(define-public ghc-appar
  (package
    (name "ghc-appar")
    (version "0.1.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/appar/appar-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "07v3h766q9mnhphsm53718h1lds147ix7dj15kc5hnsj4vffvkn4"))))
    (build-system haskell-build-system)
    (home-page
     "https://hackage.haskell.org/package/appar")
    (synopsis "Simple applicative parser")
    (description "This package provides a simple applicative parser in Parsec
style.")
    (license license:bsd-3)))

(define-public ghc-assoc
  (package
    (name "ghc-assoc")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/assoc/assoc-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1m9n4vp190bvn2wcrd4ggfwa9pi93jp0zgx02mdgywn2zfidw020"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-bifunctors" ,ghc-bifunctors)
       ("ghc-tagged" ,ghc-tagged)))
    (home-page
     "http://hackage.haskell.org/package/assoc")
    (synopsis
     "Swap and assoc: Symmetric and Semigroupy Bifunctors")
    (description
     "Provides generalisations of @code{swap :: (a,b) -> (b,a)} and
@code{assoc :: ((a,b),c) -> (a,(b,c))} to @code{Bifunctor}s supporting
similar operations (e.g. @code{Either}, @code{These}).")
    (license license:bsd-3)))

(define-public ghc-async
  (package
    (name "ghc-async")
    (version "2.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/async/async-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1zxvfcyy4sg8lmzphi5dgnavksj5pav6rbvd5kc48lf4hanb2jjb"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-hashable" ,ghc-hashable)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)))
    (home-page "https://github.com/simonmar/async")
    (synopsis "Library to run IO operations asynchronously")
    (description "Async provides a library to run IO operations
asynchronously, and wait for their results.  It is a higher-level interface
over threads in Haskell, in which @code{Async a} is a concurrent thread that
will eventually deliver a value of type @code{a}.")
    (license license:bsd-3)))

(define-public ghc-atomic-primops
  (package
    (name "ghc-atomic-primops")
    (version "0.8.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/atomic-primops"
                           "/atomic-primops-" version ".tar.gz"))
       (sha256
        (base32
         "03n5dmyplrqgbyf8dr91izkxci7gkl3i3fnp82i5ld869zrgjfh0"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-primitive" ,ghc-primitive)))
    (home-page "https://github.com/rrnewton/haskell-lockfree/wiki")
    (synopsis "Safe approach to CAS and other atomic ops")
    (description
     "GHC 7.4 introduced a new @code{casMutVar} PrimOp which is difficult to
use safely, because pointer equality is a highly unstable property in Haskell.
This library provides a safer method based on the concept of @code{Ticket}s.")
    (license license:bsd-3)))

(define-public ghc-atomic-write
  (package
    (name "ghc-atomic-write")
    (version "0.2.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/atomic-write/atomic-write-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1xs3shwnlj8hmnm3q6jc8nv78z0481i5n4hrqqdmbpx8grvlnqyl"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-temporary" ,ghc-temporary)
       ("ghc-unix-compat" ,ghc-unix-compat)))
    (native-inputs
     `(("ghc-temporary" ,ghc-temporary)
       ("ghc-unix-compat" ,ghc-unix-compat)
       ("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://github.com/stackbuilders/atomic-write")
    (synopsis "Atomically write to a file")
    (description
     "Atomically write to a file on POSIX-compliant systems while preserving
permissions.  @code{mv} is an atomic operation.  This makes it simple to write
to a file atomically just by using the @code{mv} operation.  However, this
will destroy the permissions on the original file.  This library preserves
permissions while atomically writing to a file.")
    (license license:expat)))

(define-public ghc-atomic-write-0.2.0.7
  (package
    (inherit ghc-atomic-write)
    (version "0.2.0.7")
    (source
     (origin
       (inherit (package-source ghc-atomic-write))
       (uri (string-append
             "https://hackage.haskell.org/package/atomic-write/atomic-write-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "03cn3ii74h0w3g4h78xsx9v2sn58r3qsr2dbdwq340xwhiwcgxdm"))))))

(define-public ghc-attoparsec
  (package
    (name "ghc-attoparsec")
    (version "0.13.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/attoparsec/attoparsec-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1ngjn9h5n0vyki0m2jir4mg85875ysswy9hznpmj1r856mqwc6ix"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-for-newer-quickcheck
           (lambda _
             (substitute* "attoparsec.cabal"
               (("QuickCheck >= 2\\.7 && < 2\\.10")
                "QuickCheck >= 2.7 && < 2.12"))
             ;; This test fails because of the newer QuickCheck:
             ;; <https://github.com/bos/attoparsec/issues/134>.
             (substitute* "tests/QC/ByteString.hs"
               ((", testProperty \"satisfyWith\" satisfyWith")
                "")))))))
    (inputs
     `(("ghc-scientific" ,ghc-scientific)))
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-quickcheck-unicode" ,ghc-quickcheck-unicode)
       ("ghc-vector" ,ghc-vector)))
    (home-page "https://github.com/bos/attoparsec")
    (synopsis "Fast combinator parsing for bytestrings and text")
    (description "This library provides a fast parser combinator library,
aimed particularly at dealing efficiently with network protocols and
complicated text/binary file formats.")
    (license license:bsd-3)))

(define-public ghc-attoparsec-bootstrap
  (package
    (inherit ghc-attoparsec)
    (name "ghc-attoparsec-bootstrap")
    (arguments `(#:tests? #f))
    (inputs
     `(("ghc-scientific" ,ghc-scientific-bootstrap)))
    (native-inputs '())
    (properties '((hidden? #t)))))

(define-public ghc-attoparsec-iso8601
  (package
    (name "ghc-attoparsec-iso8601")
    (version "1.0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "attoparsec-iso8601-" version "/"
                           "attoparsec-iso8601-" version ".tar.gz"))
       (sha256
        (base32
         "0hj10w15qp2z5bz2v4xahhmbgzclpyfi5l2sv97wqycysg9gp7s9"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("1" "1rjhscmczgs1bwyqx7lvkm8py3ylxjd2797mrzgnq60fvm292750")))
    (inputs `(("ghc-attoparsec" ,ghc-attoparsec)
              ("ghc-base-compat" ,ghc-base-compat)))
    (home-page "https://github.com/bos/aeson")
    (synopsis "Parse ISO 8601 dates")
    (description "Haskell library for parsing of ISO 8601 dates, originally
from aeson.")
    (license license:bsd-3)))

(define-public ghc-auto-update
  (package
    (name "ghc-auto-update")
    (version "0.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/auto-update/auto-update-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1i36xc2i34aync8271x3pv515l3zb53i518dybn8ghqkhzf27q7l"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-retry" ,ghc-retry)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://github.com/yesodweb/wai")
    (synopsis "Efficiently run periodic, on-demand actions")
    (description "This library provides mechanisms to efficiently run
periodic, on-demand actions in Haskell.")
    (license license:expat)))

(define-public ghc-aws
  (package
    (name "ghc-aws")
    (version "0.20")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "aws-" version "/aws-" version ".tar.gz"))
       (sha256 (base32
                "0pwpabmypi1w8rni9qfwabgn95jks4h8dyw6889mn8xzsrhdhyf0"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f)) ; Tests require AWS credentials.
    (inputs
     `(("ghc-aeson" ,ghc-aeson)
       ("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-base16-bytestring" ,ghc-base16-bytestring)
       ("ghc-base64-bytestring" ,ghc-base64-bytestring)
       ("ghc-blaze-builder" ,ghc-blaze-builder)
       ("ghc-byteable" ,ghc-byteable)
       ("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-cereal" ,ghc-cereal)
       ("ghc-conduit" ,ghc-conduit)
       ("ghc-conduit-extra" ,ghc-conduit-extra)
       ("ghc-cryptonite" ,ghc-cryptonite)
       ("ghc-data-default" ,ghc-data-default)
       ("ghc-http-conduit" ,ghc-http-conduit)
       ("ghc-http-types" ,ghc-http-types)
       ("ghc-lifted-base" ,ghc-lifted-base)
       ("ghc-monad-control" ,ghc-monad-control)
       ("ghc-network" ,ghc-network)
       ("ghc-old-locale" ,ghc-old-locale)
       ("ghc-safe" ,ghc-safe)
       ("ghc-scientific" ,ghc-scientific)
       ("ghc-tagged" ,ghc-tagged)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-utf8-string" ,ghc-utf8-string)
       ("ghc-vector" ,ghc-vector)
       ("ghc-xml-conduit" ,ghc-xml-conduit)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-errors" ,ghc-errors)
       ("ghc-http-client" ,ghc-http-client)
       ("ghc-http-client-tls" ,ghc-http-client-tls)
       ("ghc-quickcheck-instances" ,ghc-quickcheck-instances)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-conduit-combinators" ,ghc-conduit-combinators)))
    (home-page "https://github.com/aristidb/aws")
    (synopsis "Amazon Web Services for Haskell")
    (description "This package attempts to provide support for using
Amazon Web Services like S3 (storage), SQS (queuing) and others to
Haskell programmers.  The ultimate goal is to support all Amazon
Web Services.")
    (license license:bsd-3)))

(define-public ghc-base16-bytestring
  (package
    (name "ghc-base16-bytestring")
    (version "0.1.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/base16-bytestring/"
             "base16-bytestring-" version ".tar.gz"))
       (sha256
        (base32
         "0jf40m3yijqw6wd1rwwvviww46fasphaay9m9rgqyhf5aahnbzjs"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/bos/base16-bytestring")
    (synopsis "Fast base16 (hex) encoding and decoding for ByteStrings")
    (description
     "This package provides a Haskell library for working with base16-encoded
data quickly and efficiently, using the ByteString type.")
    (license license:bsd-3)))

(define-public ghc-base64-bytestring
  (package
    (name "ghc-base64-bytestring")
    (version "1.0.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/base64-bytestring/base64-bytestring-"
             version
             ".tar.gz"))
       (sha256
        (base32 "13305brzlac24pifiqd5a2z10c6k6amhpdy9cc0z5ryrkgnm8dhr"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f))         ; FIXME: testing libraries are missing.
    (home-page "https://github.com/bos/base64-bytestring")
    (synopsis "Base64 encoding and decoding for ByteStrings")
    (description "This library provides fast base64 encoding and decoding for
Haskell @code{ByteString}s.")
    (license license:bsd-3)))

(define-public ghc-base-compat
  (package
    (name "ghc-base-compat")
    (version "0.10.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/base-compat/base-compat-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0hgvlqcr852hfp52jp99snhbj550mvxxpi8qn15d8ml9aqhyl2lr"))))
    (build-system haskell-build-system)
    (outputs '("out" "static" "doc"))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://hackage.haskell.org/package/base-compat")
    (synopsis "Haskell compiler compatibility library")
    (description "This library provides functions available in later versions
of base to a wider range of compilers, without requiring the use of CPP
pragmas in your code.")
    (license license:bsd-3)))

(define-public ghc-base-compat-batteries
  (package
    (name "ghc-base-compat-batteries")
    (version "0.10.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "base-compat-batteries/base-compat-batteries-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1vkhc639vqiv5p39jn1v312z32i7yk5q2lf0ap4jxl1v8p8wyp8p"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-base-compat" ,ghc-base-compat)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("hspec-discover" ,hspec-discover)))
    (arguments
     `(#:cabal-revision
       ("1" "15sn2qc8k0hxbb2nai341kkrci98hlhzcj2ci087m0zxcg5jcdbp")))
    (home-page "https://hackage.haskell.org/package/base-compat-batteries")
    (synopsis "base-compat with extra batteries")
    (description "This library provides functions available in later
versions of @code{base} to a wider range of compilers, without requiring
you to use CPP pragmas in your code.  This package provides the same API
as the @code{base-compat} library, but depends on compatibility
packages (such as @code{semigroups}) to offer a wider support window
than @code{base-compat}, which has no dependencies.")
    (license license:expat)))

(define-public ghc-basement
  (package
    (name "ghc-basement")
    (version "0.0.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "basement/basement-" version ".tar.gz"))
       (sha256
        (base32
         "0srlws74yiraqaapgcjd9p5d1fwb3zr9swcz74jpjm55fls2nn37"))))
    (build-system haskell-build-system)
    (outputs '("out" "static" "doc"))
    (home-page "https://github.com/haskell-foundation/foundation")
    (synopsis "Basic primitives for Foundation starter pack")
    (description
     "This package contains basic primitives for the Foundation set of
packages.")
    (license license:bsd-3)))

(define-public ghc-base-orphans
  (package
    (name "ghc-base-orphans")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/base-orphans/base-orphans-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1nwr9av27i9p72k0sn96mw3ywdczw65dy5gd5wxpabhhxlxdcas4"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://hackage.haskell.org/package/base-orphans")
    (synopsis "Orphan instances for backwards compatibility")
    (description "This package defines orphan instances that mimic instances
available in later versions of base to a wider (older) range of compilers.")
    (license license:bsd-3)))

(define-public ghc-base-prelude
  (package
    (name "ghc-base-prelude")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "base-prelude-" version "/"
                           "base-prelude-" version ".tar.gz"))
       (sha256
        (base32
         "1zk728sd09hh2r4xwz4lazsrrgg5cshydn64932sm0vckplndk73"))))
    (build-system haskell-build-system)
    (outputs '("out" "static" "doc"))
    (home-page "https://github.com/nikita-volkov/base-prelude")
    (synopsis "The most complete prelude formed solely from the Haskell's base
package")
    (description "This Haskell package aims to reexport all the non-conflicting
and most general definitions from the \"base\" package.

This includes APIs for applicatives, arrows, monoids, foldables, traversables,
exceptions, generics, ST, MVars and STM.

This package will never have any dependencies other than \"base\".

Versioning policy:

The versioning policy of this package deviates from PVP in the sense
that its exports in part are transitively determined by the version of \"base\".
Therefore it's recommended for the users of @code{ghc-base-prelude} to specify
the bounds of \"base\" as well.")
    (license license:expat)))

(define-public ghc-base-unicode-symbols
  (package
    (name "ghc-base-unicode-symbols")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://hackage/package/base-unicode-symbols/base-unicode-symbols-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1ia6li7qjg1zkak4gf6mnbshw45mq9bfjr0jch58ds0lscmvwyzf"))))
    (build-system haskell-build-system)
    (home-page "https://wiki.haskell.org/Unicode-symbols")
    (synopsis "Unicode alternatives for common functions and operators")
    (description "This package defines new symbols for a number of functions,
operators and types in the base package.  All symbols are documented with
their actual definition and information regarding their Unicode code point.
They should be completely interchangeable with their definitions.  For
further Unicode goodness you can enable the @code{UnicodeSyntax}
@url{https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exs.html#unicode-syntax,
language extension}.  This extension enables Unicode characters to be used to
stand for certain ASCII character sequences, i.e. → instead of @code{->},
∀ instead of @code{forall} and many others.")
    (license license:bsd-3)))

(define-public ghc-basic-prelude
  (package
    (name "ghc-basic-prelude")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/basic-prelude/"
             "basic-prelude-" version ".tar.gz"))
       (sha256
        (base32
         "0yckmnvm6i4vw0mykj4fzl4ldsf67v8d2h0vp1bakyj84n4myx8h"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-hashable" ,ghc-hashable)
       ("ghc-unordered-containers"
        ,ghc-unordered-containers)
       ("ghc-vector" ,ghc-vector)))
    (home-page "https://github.com/snoyberg/basic-prelude#readme")
    (synopsis "Enhanced core prelude; a common foundation for alternate preludes")
    (description
     "The premise of basic-prelude is that there are a lot of very commonly
desired features missing from the standard Prelude, such as commonly used
operators (<$> and >=>, for instance) and imports for common datatypes
(e.g., ByteString and Vector).  At the same time, there are lots of other
components which are more debatable, such as providing polymorphic versions
of common functions.

So basic-prelude is intended to give a common foundation for a number of
alternate preludes.  The package provides two modules: CorePrelude provides
the common ground for other preludes to build on top of, while BasicPrelude
exports CorePrelude together with commonly used list functions to provide a
drop-in replacement for the standard Prelude.

Users wishing to have an improved Prelude can use BasicPrelude.  Developers
wishing to create a new prelude should use CorePrelude.")
    (license license:expat)))

(define-public ghc-bencode
  (package
    (name "ghc-bencode")
    (version "0.6.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/bencode/bencode-"
             version ".tar.gz"))
       (sha256
        (base32 "0znv0y3b3zm5jvhlvj5f5s7y93db67j9yd59w1bnrw2pqv30gqaq"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-transformers-compat" ,ghc-transformers-compat)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://hackage.haskell.org/package/bencode")
    (synopsis "Parsers and printers for bencoded data")
    (description
     "This library provides parsers and printers for bencoded data.  Bencode
is the encoding used by the peer-to-peer file sharing system BitTorrent for
storing and transmitting loosely structured data.")
    (license license:bsd-3)))

(define-public ghc-bifunctors
  (package
    (name "ghc-bifunctors")
    (version "5.5.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/bifunctors/bifunctors-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0rn47q8dzv0g1fyams99p4py6q0asxdc50q9k0nj497brk738xcb"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-base-orphans" ,ghc-base-orphans)
       ("ghc-comonad" ,ghc-comonad)
       ("ghc-th-abstraction" ,ghc-th-abstraction)
       ("ghc-transformers-compat" ,ghc-transformers-compat)
       ("ghc-tagged" ,ghc-tagged)
       ("ghc-semigroups" ,ghc-semigroups)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://github.com/ekmett/bifunctors/")
    (synopsis "Bifunctors for Haskell")
    (description "This package provides bifunctors for Haskell.")
    (license license:bsd-3)))

(define-public ghc-bindings-dsl
  (package
    (name "ghc-bindings-dsl")
    (version "1.0.25")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/bindings-DSL/"
                           "bindings-DSL-" version ".tar.gz"))
       (sha256
        (base32
         "0kqrd78nspl3lk4a0fqn47d8dirjg3b24dkvkigcrlb81hw35pk3"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/jwiegley/bindings-dsl/wiki")
    (synopsis "FFI domain specific language, on top of hsc2hs")
    (description
     "This is a set of macros to be used when writing Haskell FFI.  They were
designed to be able to fully describe C interfaces, so that @code{hsc2hs} can
extract from them all Haskell code needed to mimic such interfaces.  All
Haskell names used are automatically derived from C names, structures are
mapped to Haskell instances of @code{Storable}, and there are also macros you
can use with C code to help write bindings to inline functions or macro
functions.")
    (license license:bsd-3)))

(define-public ghc-bitarray
  (package
    (name "ghc-bitarray")
    (version "0.0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "bitarray/bitarray-" version ".tar.gz"))
       (sha256
        (base32
         "00nqd62cbh42qqqvcl6iv1i9kbv0f0mkiygv4j70wfh5cl86yzxj"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("1" "10fk92v9afjqk43zi621jxl0n8kci0xjj32lz3vqa9xbh67zjz45")))
    (home-page "https://hackage.haskell.org/package/bitarray")
    (synopsis "Mutable and immutable bit arrays")
    (description "The package provides mutable and immutable bit arrays.")
    (license license:bsd-3)))

(define-public ghc-blaze-builder
  (package
    (name "ghc-blaze-builder")
    (version "0.4.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/blaze-builder/blaze-builder-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "05681dih2d8s96an945wkbwl05w8ddbcfx8n3r3ck79ydyb8pz4i"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f))          ; FIXME: Missing test libraries.
    (inputs
     `(("ghc-utf8-string" ,ghc-utf8-string)))
    (home-page "https://github.com/lpsmith/blaze-builder")
    (synopsis "Efficient buffered output")
    (description "This library provides an implementation of the older
@code{blaze-builder} interface in terms of the new builder that shipped with
@code{bytestring-0.10.4.0}.  This implementation is mostly intended as a
bridge to the new builder, so that code that uses the old interface can
interoperate with code that uses the new implementation.")
    (license license:bsd-3)))

(define-public ghc-blaze-markup
  (package
    (name "ghc-blaze-markup")
    (version "0.8.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "blaze-markup/blaze-markup-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1g9m7ansj7fdyzhz1wqkbzn5amjm50vjgjdwkbjc5qqhagnv1y3j"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'update-constraints
           (lambda _
             (substitute* "blaze-markup.cabal"
               (("tasty            >= 1\\.0  && < 1\\.1")
                "tasty            >= 1.0  && < 1.2")))))))
    (inputs
     `(("ghc-blaze-builder" ,ghc-blaze-builder)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)))
    (home-page "https://jaspervdj.be/blaze")
    (synopsis "Fast markup combinator library for Haskell")
    (description "This library provides core modules of a markup combinator
library for Haskell.")
    (license license:bsd-3)))

(define-public ghc-bloomfilter
  (package
    (name "ghc-bloomfilter")
    (version "2.0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "bloomfilter/bloomfilter-" version ".tar.gz"))
       (sha256
        (base32
         "03vrmncg1c10a2wcg5skq30m1yiknn7nwxz2gblyyfaxglshspkc"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-random" ,ghc-random)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)))
    (home-page "https://github.com/bos/bloomfilter")
    (synopsis "Pure and impure Bloom filter implementations")
    (description "This package provides both mutable and immutable Bloom
filter data types, along with a family of hash functions and an easy-to-use
interface.")
    (license license:bsd-3)))

(define-public ghc-boxes
  (package
    (name "ghc-boxes")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/boxes/boxes-"
                           version ".tar.gz"))
       (sha256
        (base32 "1hsnmw95i58d4bkpxby3ddsj1cawypw4mdyb18m393s5i8p7iq9q"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-split" ,ghc-split)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://hackage.haskell.org/package/boxes")
    (synopsis "2D text pretty-printing library")
    (description
     "Boxes is a pretty-printing library for laying out text in two dimensions,
using a simple box model.")
    (license license:bsd-3)))

(define-public ghc-byteable
  (package
    (name "ghc-byteable")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "byteable/byteable-" version ".tar.gz"))
              (sha256
               (base32
                "1qizg0kxxjqnd3cbrjhhidk5pbbciz0pb3z5kzikjjxnnnhk8fr4"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/vincenthz/hs-byteable")
    (synopsis "Type class for sequence of bytes")
    (description
     "This package provides an abstract class to manipulate sequence of bytes.
The use case of this class is abstracting manipulation of types that are just
wrapping a bytestring with stronger and more meaniful name.")
    (license license:bsd-3)))

(define-public ghc-byteorder
  (package
    (name "ghc-byteorder")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/byteorder/byteorder-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "06995paxbxk8lldvarqpb3ygcjbg4v8dk4scib1rjzwlhssvn85x"))))
    (build-system haskell-build-system)
    (home-page
     "http://community.haskell.org/~aslatter/code/byteorder")
    (synopsis
     "Exposes the native endianness of the system")
    (description
     "This package is for working with the native byte-ordering of the
system.")
    (license license:bsd-3)))

(define-public ghc-bytes
  (package
   (name "ghc-bytes")
   (version "0.15.5")
   (source
    (origin
     (method url-fetch)
     (uri
      (string-append "https://hackage.haskell.org/package/bytes-"
                     version "/bytes-"
                     version ".tar.gz"))
     (file-name (string-append name "-" version ".tar.gz"))
     (sha256
      (base32
       "063il2vrn0p88r9gzndh4ijs0mxj37khkc9ym9bqdsv7ngk3b683"))))
   (build-system haskell-build-system)
   (inputs `(("ghc-cereal" ,ghc-cereal)
             ("cabal-doctest" ,cabal-doctest)
             ("ghc-doctest" ,ghc-doctest)
             ("ghc-scientific" ,ghc-scientific)
             ("ghc-transformers-compat" ,ghc-transformers-compat)
             ("ghc-unordered-containers" ,ghc-unordered-containers)
             ("ghc-void" ,ghc-void)
             ("ghc-vector" ,ghc-vector)))
   (synopsis "Serialization between @code{binary} and @code{cereal}")
   (description "This package provides a simple compatibility shim that lets
you work with both @code{binary} and @code{cereal} with one chunk of
serialization code.")
   (home-page "https://hackage.haskell.org/package/bytes")
   (license license:bsd-3)))

(define-public ghc-bytestring-builder
  (package
    (name "ghc-bytestring-builder")
    (version "0.10.8.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/bytestring-builder"
             "/bytestring-builder-" version ".tar.gz"))
       (sha256
        (base32
         "0grcrgwwwcvwrs9az7l4d3kf0lsqfa9qpmjzf6iyanvwn9nyzyi7"))))
    (build-system haskell-build-system)
    (arguments `(#:haddock? #f)) ; Package contains no documentation.
    (home-page "https://hackage.haskell.org/package/bytestring-builder")
    (synopsis "The new bytestring builder, packaged outside of GHC")
    (description "This package provides the bytestring builder that is
debuting in bytestring-0.10.4.0, which should be shipping with GHC 7.8.
Compatibility package for older packages.")
    (license license:bsd-3)))

(define-public ghc-bytestring-handle
  (package
    (name "ghc-bytestring-handle")
    (version "0.1.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/bytestring-handle/bytestring-handle-"
             version ".tar.gz"))
       (sha256
        (base32
         "18f17aja1ivhr3zyg2cccn2m03hdn5jf5410dndkhf12gvgiqs7y"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("1" "0x11aj6w1lijh84jcdq1qgyvdnc7i9ivbyq4wf9rxicg57viisz9")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'update-constraints
           (lambda _
             (substitute* "bytestring-handle.cabal"
               (("QuickCheck >= 2\\.1\\.2 && < 2\\.11")
                "QuickCheck >= 2.1.2 && < 2.14")))))))
    (inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)))
    (home-page "https://hub.darcs.net/ganesh/bytestring-handle")
    (synopsis "ByteString-backed Handles")
    (description "ByteString-backed Handles") ; There is no description
    (license license:bsd-3)))

(define-public ghc-bytestring-lexing
  (package
    (name "ghc-bytestring-lexing")
    (version "0.5.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "bytestring-lexing/bytestring-lexing-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0wrzniawhgpphc6yx1v972gyqxdbv0pizaz9bafahrshyb9svy81"))))
    (build-system haskell-build-system)
    (home-page "http://code.haskell.org/~wren/")
    (synopsis "Parse and produce literals from strict or lazy bytestrings")
    (description
     "This package provides tools to parse and produce literals efficiently
from strict or lazy bytestrings.")
    (license license:bsd-2)))

(define-public ghc-bzlib-conduit
  (package
    (name "ghc-bzlib-conduit")
    (version "0.3.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/bzlib-conduit/"
                           "bzlib-conduit-" version ".tar.gz"))
       (sha256
        (base32
         "0a21zin5plsl37hkxh2jv8cxwyjrbs2fy7n5cyrzgdaa7lmp6b7b"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-bindings-dsl" ,ghc-bindings-dsl)
       ("ghc-conduit" ,ghc-conduit)
       ("ghc-data-default-class" ,ghc-data-default-class)
       ("ghc-resourcet" ,ghc-resourcet)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("ghc-random" ,ghc-random)))
    (home-page "https://github.com/snoyberg/bzlib-conduit")
    (synopsis "Streaming compression/decompression via conduits")
    (description
     "This package provides Haskell bindings to bzlib and Conduit support for
streaming compression and decompression.")
    (license license:bsd-3)))

(define-public ghc-c2hs
  (package
    (name "ghc-c2hs")
    (version "0.28.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/c2hs/c2hs-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1nplgxfin139x12sb656f5870rpdclrhzi8mq8pry035qld15pci"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-language-c" ,ghc-language-c)
       ("ghc-dlist" ,ghc-dlist)))
    (native-inputs
     `(("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-shelly" ,ghc-shelly)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-cc
           ;; add a cc executable in the path, needed for some tests to pass
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gcc (assoc-ref inputs "gcc"))
                   (tmpbin (tmpnam))
                   (curpath (getenv "PATH")))
               (mkdir-p tmpbin)
               (symlink (which "gcc") (string-append tmpbin "/cc"))
               (setenv "PATH" (string-append tmpbin ":" curpath)))
             #t))
         (add-after 'check 'remove-cc
           ;; clean the tmp dir made in 'set-cc
           (lambda _
             (let* ((cc-path (which "cc"))
                    (cc-dir (dirname cc-path)))
               (delete-file-recursively cc-dir)
               #t))))))
    (home-page "https://github.com/haskell/c2hs")
    (synopsis "Create Haskell bindings to C libraries")
    (description "C->Haskell assists in the development of Haskell bindings to
C libraries.  It extracts interface information from C header files and
generates Haskell code with foreign imports and marshaling.  Unlike writing
foreign imports by hand (or using hsc2hs), this ensures that C functions are
imported with the correct Haskell types.")
    (license license:gpl2)))

(define-public ghc-cairo
  (package
    (name "ghc-cairo")
    (version "0.13.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/cairo/"
                           "cairo-" version ".tar.gz"))
       (sha256
        (base32
         "1wxylv4d8120ri0vgar168ikqa9m6533ipdwi38qlmxmw20ws2j2"))))
    (build-system haskell-build-system)
    (arguments
     `(#:modules ((guix build haskell-build-system)
                  (guix build utils)
                  (ice-9 match)
                  (srfi srfi-26))
       #:phases
       (modify-phases %standard-phases
         ;; FIXME: This is a copy of the standard configure phase with a tiny
         ;; difference: this package needs the -package-db flag to be passed
         ;; to "runhaskell" in addition to the "configure" action, because it
         ;; depends on gtk2hs-buildtools, which provide setup hooks.  Without
         ;; this option the Setup.hs file cannot be evaluated.  The
         ;; haskell-build-system should be changed to pass "-package-db" to
         ;; "runhaskell" in any case.
         (replace 'configure
           (lambda* (#:key outputs inputs tests? (configure-flags '())
                     #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (name-version (strip-store-file-name out))
                    (input-dirs (match inputs
                                  (((_ . dir) ...)
                                   dir)
                                  (_ '())))
                    (ghc-path (getenv "GHC_PACKAGE_PATH"))
                    (params (append `(,(string-append "--prefix=" out))
                                    `(,(string-append "--libdir=" out "/lib"))
                                    `(,(string-append "--bindir=" out "/bin"))
                                    `(,(string-append
                                        "--docdir=" out
                                        "/share/doc/" name-version))
                                    '("--libsubdir=$compiler/$pkg-$version")
                                    '("--package-db=../package.conf.d")
                                    '("--global")
                                    `(,@(map
                                         (cut string-append "--extra-include-dirs=" <>)
                                         (search-path-as-list '("include") input-dirs)))
                                    `(,@(map
                                         (cut string-append "--extra-lib-dirs=" <>)
                                         (search-path-as-list '("lib") input-dirs)))
                                    (if tests?
                                        '("--enable-tests")
                                        '())
                                    configure-flags)))
               (unsetenv "GHC_PACKAGE_PATH")
               (apply invoke "runhaskell" "-package-db=../package.conf.d"
                      "Setup.hs" "configure" params)
               (setenv "GHC_PACKAGE_PATH" ghc-path)
               #t))))))
    (inputs
     `(("ghc-utf8-string" ,ghc-utf8-string)
       ("cairo" ,cairo)))
    (native-inputs
     `(("ghc-gtk2hs-buildtools" ,ghc-gtk2hs-buildtools)
       ("pkg-config" ,pkg-config)))
    (home-page "http://projects.haskell.org/gtk2hs/")
    (synopsis "Haskell bindings to the Cairo vector graphics library")
    (description
     "Cairo is a library to render high quality vector graphics.  There exist
various backends that allows rendering to Gtk windows, PDF, PS, PNG and SVG
documents, amongst others.")
    (license license:bsd-3)))

(define-public ghc-call-stack
  (package
    (name "ghc-call-stack")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "call-stack/call-stack-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1qmihf5jafmc79sk52l6gpx75f5bnla2lp62kh3p34x3j84mwpzj"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-nanospec" ,ghc-nanospec)))
    (home-page "https://github.com/sol/call-stack#readme")
    (synopsis "Use GHC call-stacks in a backward compatible way")
    (description "This package provides a compatibility layer for using GHC
call stacks with different versions of the compiler.")
    (license license:expat)))

;; This is used as an input to ghc-hunit.  We cannot use ghc-call-stack there,
;; because it depends on ghc-nanospec, which depends on ghc-hunit.
(define-public ghc-call-stack-boot
  (hidden-package
   (package
     (inherit ghc-call-stack)
     (arguments '(#:tests? #f))
     (inputs '()))))

(define-public ghc-case-insensitive
  (package
    (name "ghc-case-insensitive")
    (version "1.2.0.11")
    (outputs '("out" "static" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/case-insensitive/case-insensitive-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1qrpxfirsxckg7jv28f5ah2qc8lh95hp7rnqkbqs1ahcwlbnvkm7"))))
    (build-system haskell-build-system)
    ;; these inputs are necessary to use this library
    (inputs
     `(("ghc-hashable" ,ghc-hashable)))
    (arguments
     `(#:tests? #f)) ; FIXME: currently missing libraries used for tests.
    (home-page
     "https://github.com/basvandijk/case-insensitive")
    (synopsis "Case insensitive string comparison")
    (description
     "The module @code{Data.CaseInsensitive} provides the @code{CI} type
constructor which can be parameterised by a string-like type like:
@code{String}, @code{ByteString}, @code{Text}, etc.  Comparisons of values of
the resulting type will be insensitive to cases.")
    (license license:bsd-3)))

(define-public ghc-cassava
  (package
    (name "ghc-cassava")
    (version "0.5.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/cassava/cassava-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "01h1zrdqb313cjd4rqm1107azzx4czqi018c2djf66a5i7ajl3dk"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-scientific" ,ghc-scientific)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-vector" ,ghc-vector)
       ("ghc-only" ,ghc-only)
       ("ghc-text-short" ,ghc-text-short)
       ("ghc-bytestring-builder" ,ghc-bytestring-builder)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-quickcheck-instances" ,ghc-quickcheck-instances)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)))
    (arguments
     `(#:cabal-revision
       ("1"
        "1ph8rf91z4nf1ryrh9s4gd1kq98jlgk2manwddkpch8k0n9xvfk4")
       #:configure-flags '("--flags=-bytestring--lt-0_10_4")))
    (home-page "https://github.com/haskell-hvr/cassava")
    (synopsis "CSV parsing and encoding library")
    (description
     "@code{cassava} is a library for parsing and encoding
@url{https://tools.ietf.org/html/rfc4180, RFC 4180} compliant @url{https://
en.wikipedia.org/wiki/Comma-separated_values, comma-separated values (CSV)}
data, which is a textual line-oriented format commonly used for exchanging
tabular data.

@code{cassava}'s API includes support for:

@itemize @bullet

@item
Index-based record-conversion
@item
Name-based record-conversion
@item
Typeclass directed conversion of fields and records
@item
Built-in field-conversion instances for standard types
@item
Customizable record-conversion instance derivation via GHC generics
@item
Low-level @url{https://hackage.haskell.org/package/bytestring), bytestring}
builders (see @url{https://hackage.haskell.org/package/cassava-0.5.2.0/docs/
Data-Csv-Builder.html, Data.Csv.Builder})
@item
Incremental decoding and encoding API (see @url{https://hackage.haskell.org/
package/cassava-0.5.2.0/docs/Data-Csv-Incremental.html, Data.Csv.Incremental})
@item
Streaming API for constant-space decoding (see @url{https://hackage.haskell.org/
package/cassava-0.5.2.0/docs/Data-Csv-Streaming.html, Data.Csv.Streaming})
@end itemize

Moreover, this library is designed to be easy to use; for instance, here's a
very simple example of encoding CSV data:

@verbatim
>>> Data.Csv.encode [(\"John\",27),(\"Jane\",28)]
\"John,27\r\nJane,28\r\n\"
@end verbatim
")
    (license license:bsd-3)))

(define-public ghc-cassava-megaparsec
  (package
    (name "ghc-cassava-megaparsec")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/cassava-megaparsec/"
             "cassava-megaparsec-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0q4skw98nzy6icmgpwqvgw0c5pqcgi25rf7nmwh2pksvv94pi3p3"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-cassava" ,ghc-cassava)
       ("ghc-megaparsec" ,ghc-megaparsec)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-vector" ,ghc-vector)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("ghc-hspec-megaparsec" ,ghc-hspec-megaparsec)))
    (home-page "https://github.com/stackbuilders/cassava-megaparsec")
    (synopsis "Megaparsec parser for CSV files that plays nicely with Cassava")
    (description
     "Alternative parser for the Cassava package written with Megaparsec that
provides for better error messages at the expense of some speed.")
    (license license:expat)))

(define-public ghc-cborg
  (package
    (name "ghc-cborg")
    (version "0.2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://hackage/package/cborg/cborg-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1rdnvy0w17s70ikmbyrnwax5rvqh19l95sh8i7ipgxi23z1r0bp1"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-half" ,ghc-half)
       ("ghc-primitive" ,ghc-primitive)))
    (native-inputs
     `(("ghc-aeson" ,ghc-aeson)
       ("ghc-base64-bytestring" ,ghc-base64-bytestring)
       ("ghc-base16-bytestring" ,ghc-base16-bytestring)
       ("ghc-fail" ,ghc-fail)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-scientific" ,ghc-scientific)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
       ("ghc-vector" ,ghc-vector)))
    (home-page "http://hackage.haskell.org/package/cborg")
    (synopsis "Concise Binary Object Representation")
    (description
     "This package (formerly binary-serialise-cbor) provides an
efficient implementation of the Concise Binary Object
Representation (CBOR), as specified by RFC 7049 at
https://tools.ietf.org/html/rfc7049.

If you are looking for a library for serialisation of Haskell values, have a
look at the @url{https://hackage.haskell.org/package/serialise} package, which
is built upon this library.

An implementation of the standard bijection between CBOR and JSON is provided
by the @url{https://hackage.haskell.org/package/cborg-json} package.

Also see @code{https://hackage.haskell.org/package/cbor-tool} for a convenient
command-line utility for working with CBOR data.")
    (license license:bsd-3)))

(define-public ghc-cborg-json
  (package
    (name "ghc-cborg-json")
    (version "0.2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://hackage/package/cborg-json/cborg-json-"
             version
             ".tar.gz"))
       (sha256
        (base32 "0ysilz7rrjk94sqr3a61s98hr9qfi1xg13bskmlpc6mpgi2s4s5b"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-aeson" ,ghc-aeson)
       ("ghc-aeson-pretty" ,ghc-aeson-pretty)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-scientific" ,ghc-scientific)
       ("ghc-vector" ,ghc-vector)
       ("ghc-cborg" ,ghc-cborg)))
    (home-page "https://github.com/well-typed/cborg")
    (synopsis "A library for encoding JSON as CBOR")
    (description
     "This package implements the bijection between JSON and CBOR
defined in the CBOR specification, RFC 7049.")
    (license license:bsd-3)))

(define-public ghc-cereal
  (package
    (name "ghc-cereal")
    (version "0.5.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/cereal/cereal-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1mqvd1iwzr50az4y24332x3g3wsrzw8j1iwph02vr7jbjfn8i7id"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-fail" ,ghc-fail)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)))
    (home-page "https://hackage.haskell.org/package/cereal")
    (synopsis "Binary serialization library")
    (description "This package provides a binary serialization library,
similar to @code{binary}, that introduces an @code{isolate} primitive for
parser isolation, and labeled blocks for better error messages.")
    (license license:bsd-3)))

(define-public ghc-cereal-conduit
  (package
    (name "ghc-cereal-conduit")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "cereal-conduit/cereal-conduit-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1srr7agvgfw78q5s1npjq5sgynvhjgllpihiv37ylkwqm4c4ap6r"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-conduit" ,ghc-conduit)
       ("ghc-resourcet" ,ghc-resourcet)
       ("ghc-cereal" ,ghc-cereal)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)))
    (home-page "https://github.com/snoyberg/conduit")
    (synopsis "Turn Data.Serialize Gets and Puts into Sources, Sinks, and Conduits")
    (description
     "This package turn @code{Data.Serialize} @code{Gets} and @code{Puts} into
@code{Sources}, @code{Sinks}, and @code{Conduits}.")
    (license license:bsd-3)))

(define-public ghc-cgi
  (package
    (name "ghc-cgi")
    (version "3001.4.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/cgi/cgi-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1d0nh5ymkqskkp4yn0gfz4mff8i0cxyw1wws8xxp6k1mg1ywa25k"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-exceptions" ,ghc-exceptions)
       ("ghc-multipart" ,ghc-multipart)
       ("ghc-network-uri" ,ghc-network-uri)
       ("ghc-network" ,ghc-network)))
    (native-inputs
     `(("ghc-doctest" ,ghc-doctest)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page
     "https://github.com/cheecheeo/haskell-cgi")
    (synopsis "Library for writing CGI programs")
    (description
     "This is a Haskell library for writing CGI programs.")
    (license license:bsd-3)))

(define-public ghc-charset
  (package
    (name "ghc-charset")
    (version "0.3.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/charset/charset-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1gn0m96qpjww8hpp2g1as5yy0wcwy4iq73h3kz6g0yxxhcl5sh9x"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("1" "1z6nxw2g9vgsjq0g159sk8mwj68lwzxzi5iv5ynha0h85jcqxszy")))
    (inputs
     `(("ghc-semigroups" ,ghc-semigroups)
       ("ghc-unordered-containers" ,ghc-unordered-containers)))
    (home-page "https://github.com/ekmett/charset")
    (synopsis "Fast unicode character sets for Haskell")
    (description "This package provides fast unicode character sets for
Haskell, based on complemented PATRICIA tries.")
    (license license:bsd-3)))

(define-public ghc-chart
  (package
    (name "ghc-chart")
    (version "1.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/Chart/"
                           "Chart-" version ".tar.gz"))
       (sha256
        (base32
         "1pn735k9ifxlb9mdh8xy7wi22cxni8xyr28n8zx9w0j6vprcg89l"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-old-locale" ,ghc-old-locale)
       ("ghc-lens" ,ghc-lens)
       ("ghc-colour" ,ghc-colour)
       ("ghc-data-default-class" ,ghc-data-default-class)
       ("ghc-operational" ,ghc-operational)
       ("ghc-vector" ,ghc-vector)))
    (home-page "https://github.com/timbod7/haskell-chart/wiki")
    (synopsis "Library for generating 2D charts and plots")
    (description
     "This package provides a library for generating 2D charts and plots, with
backends provided by the @code{Cairo} and @code{Diagrams} libraries.")
    (license license:bsd-3)))

(define-public ghc-chart-cairo
  (package
    (name "ghc-chart-cairo")
    (version "1.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/Chart-cairo/"
                           "Chart-cairo-" version ".tar.gz"))
       (sha256
        (base32
         "0hknj4rsjf2m8p5pyq5zff8ai7v80yvmxb5c6n0bkgxs4317nbl9"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-old-locale" ,ghc-old-locale)
       ("ghc-cairo" ,ghc-cairo)
       ("ghc-colour" ,ghc-colour)
       ("ghc-data-default-class" ,ghc-data-default-class)
       ("ghc-operational" ,ghc-operational)
       ("ghc-lens" ,ghc-lens)
       ("ghc-chart" ,ghc-chart)))
    (home-page "https://github.com/timbod7/haskell-chart/wiki")
    (synopsis "Cairo backend for Charts")
    (description "This package provides a Cairo vector graphics rendering
backend for the Charts library.")
    (license license:bsd-3)))

(define-public ghc-chasingbottoms
  (package
    (name "ghc-chasingbottoms")
    (version "1.3.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/ChasingBottoms/"
                           "ChasingBottoms-" version ".tar.gz"))
       (sha256
        (base32
         "0ziiqfsvv1ypdra6kd0bhbsl852i0wqn43jkfii38yl879cdacan"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-random" ,ghc-random)
       ("ghc-syb" ,ghc-syb)))
    (home-page "https://hackage.haskell.org/package/ChasingBottoms")
    (synopsis "Testing of partial and infinite values in Haskell")
    (description
     ;; FIXME: There should be a @comma{} in the uref text, but it is not
     ;; rendered properly.
     "This is a library for testing code involving bottoms or infinite values.
For the underlying theory and a larger example involving use of QuickCheck,
see the article
@uref{http://www.cse.chalmers.se/~nad/publications/danielsson-jansson-mpc2004.html,
\"Chasing Bottoms A Case Study in Program Verification in the Presence of
Partial and Infinite Values\"}.")
    (license license:expat)))

(define-public ghc-cheapskate
  (package
    (name "ghc-cheapskate")
    (version "0.1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/cheapskate/cheapskate-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0qnyd8bni2rby6b02ff4bvfdhm1hwc8vzpmnms84jgrlg1lly3fm"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-blaze-html" ,ghc-blaze-html)
       ("ghc-xss-sanitize" ,ghc-xss-sanitize)
       ("ghc-data-default" ,ghc-data-default)
       ("ghc-syb" ,ghc-syb)
       ("ghc-uniplate" ,ghc-uniplate)))
    (home-page "https://github.com/jgm/cheapskate")
    (synopsis "Experimental markdown processor")
    (description "Cheapskate is an experimental Markdown processor in pure
Haskell.  It aims to process Markdown efficiently and in the most forgiving
possible way.  It is designed to deal with any input, including garbage, with
linear performance.  Output is sanitized by default for protection against
cross-site scripting (@dfn{XSS}) attacks.")
    (license license:bsd-3)))

(define-public ghc-chell
  (package
    (name "ghc-chell")
    (version "0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/chell/chell-"
             version ".tar.gz"))
       (sha256
        (base32
         "1i845isfbk0yq852am9bqmxfpfkpnlha8nfidffsv4gw2p8gg6fg"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("1" "1q93wrw03ix4cmnkz3lzkixcvvizw6i2ia2zifdfak1dvxnblxk0")))
    (inputs
     `(("ghc-options-bootstrap" ,ghc-options-bootstrap)
       ("ghc-patience" ,ghc-patience)
       ("ghc-random" ,ghc-random)
       ("ghc-ansi-terminal" ,ghc-ansi-terminal)))
    (home-page "https://john-millikin.com/software/chell/")
    (synopsis "Simple and intuitive library for automated testing")
    (description
     "Chell is a simple and intuitive library for automated testing.
It natively supports assertion-based testing, and can use companion
libraries such as @code{chell-quickcheck} to support more complex
testing strategies.")
    (license license:expat)))

(define-public ghc-chell-quickcheck
  (package
    (name "ghc-chell-quickcheck")
    (version "0.2.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/chell-quickcheck/"
             "chell-quickcheck-" version ".tar.gz"))
       (sha256
        (base32
         "0n8c57n88r2bx0bh8nabsz07m42rh23ahs3hgyzf8gr76l08zq03"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'update-constraints
           (lambda _
             (substitute* "chell-quickcheck.cabal"
               (("QuickCheck >= 2\\.3 && < 2\\.13")
                "QuickCheck >= 2.3 && < 2.14")))))))
    (inputs
     `(("ghc-chell" ,ghc-chell)
       ("ghc-chell-quickcheck-bootstrap" ,ghc-chell-quickcheck-bootstrap)
       ("ghc-random" ,ghc-random)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://john-millikin.com/software/chell/")
    (synopsis "QuickCheck support for the Chell testing library")
    (description "More complex tests for @code{chell}.")
    (license license:expat)))

(define ghc-chell-quickcheck-bootstrap
  (package
    (name "ghc-chell-quickcheck-bootstrap")
    (version "0.2.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/chell-quickcheck/"
             "chell-quickcheck-" version ".tar.gz"))
       (sha256
        (base32
         "0n8c57n88r2bx0bh8nabsz07m42rh23ahs3hgyzf8gr76l08zq03"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-chell" ,ghc-chell)
       ("ghc-random" ,ghc-random)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'update-constraints
           (lambda _
             (substitute* "chell-quickcheck.cabal"
               (("QuickCheck >= 2\\.3 && < 2\\.13")
                "QuickCheck >= 2.3 && < 2.14")))))))
    (home-page "https://john-millikin.com/software/chell/")
    (synopsis "QuickCheck support for the Chell testing library")
    (description "More complex tests for @code{chell}.")
    (license license:expat)))

(define-public ghc-chunked-data
  (package
    (name "ghc-chunked-data")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "chunked-data-" version "/"
                           "chunked-data-" version ".tar.gz"))
       (sha256
        (base32
         "16m7y7fwrirbjbqqcsfmr4yxa9qvfax6r7pw0zl9ky71ms0wa47p"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-vector" ,ghc-vector)
              ("ghc-semigroups" ,ghc-semigroups)))
    (home-page "https://github.com/snoyberg/mono-traversable")
    (synopsis "Typeclasses for dealing with various chunked data
representations for Haskell")
    (description "This Haskell package was originally present in
classy-prelude.")
    (license license:expat)))

(define-public ghc-clock
  (package
    (name "ghc-clock")
    (version "0.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/"
             "clock/"
             "clock-" version ".tar.gz"))
       (sha256
        (base32 "0539w9bjw6xbfv9v6aq9hijszxqdnqhilwpbwpql1400ji95r8q8"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)))
    (home-page "https://hackage.haskell.org/package/clock")
    (synopsis "High-resolution clock for Haskell")
    (description "A package for convenient access to high-resolution clock and
timer functions of different operating systems via a unified API.")
    (license license:bsd-3)))

;; This package builds `clock` without tests, since the tests rely on tasty
;; and tasty-quickcheck, which in turn require clock to build.
(define-public ghc-clock-bootstrap
  (package
    (inherit ghc-clock)
    (name "ghc-clock-bootstrap")
    (arguments '(#:tests? #f))
    (inputs '())
    (properties '((hidden? #t)))))

(define-public ghc-cmark
  (package
    (name "ghc-cmark")
    (version "0.6")
    (source (origin
              (method url-fetch)
              ;; XXX As of version 0.6, this package bundles libcmark 0.28.0.
              ;; See cbits/cmark_version.h.
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "cmark/cmark-" version ".tar.gz"))
              (sha256
               (base32
                "1p41z6z8dqxk62287lvhhg4ayy9laai9ljh4azsnzb029v6mbv0d"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)))
    (home-page "https://github.com/jgm/commonmark-hs")
    (synopsis "Fast, accurate CommonMark (Markdown) parser and renderer")
    (description
     "This package provides Haskell bindings for
@uref{https://github.com/jgm/cmark, libcmark}, the reference parser for
CommonMark, a fully specified variant of Markdown.  It includes bundled libcmark
sources, and does not require prior installation of the C library.")
    (license license:bsd-3)))

(define-public ghc-cmark-gfm
  (package
    (name "ghc-cmark-gfm")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "cmark-gfm/cmark-gfm-"
                           version ".tar.gz"))
       (sha256
        (base32
         "03xflrkyw84qv3yjly5iks9311bqv5cmrmsylr763v4ph0fn7rjq"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)))
    (home-page "https://github.com/kivikakk/cmark-gfm-hs")
    (synopsis
     "Fast, accurate GitHub Flavored Markdown parser and renderer")
    (description
     "This package provides Haskell bindings for libcmark-gfm, the reference
parser for GitHub Flavored Markdown, a fully specified variant of Markdown.
It includes sources for libcmark-gfm and does not require prior installation
of the C library.")
    (license license:bsd-3)))

(define-public ghc-cmdargs
  (package
    (name "ghc-cmdargs")
    (version "0.10.20")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/cmdargs/cmdargs-"
             version ".tar.gz"))
       (sha256
        (base32
         "0cbkmgrcnwgigg6z88y3c09gm7g6dwm7gzbgr53h8k1xik29s9hf"))))
    (build-system haskell-build-system)
    (outputs '("out" "static" "doc"))
    (home-page
     "http://community.haskell.org/~ndm/cmdargs/")
    (synopsis "Command line argument processing")
    (description
     "This library provides an easy way to define command line parsers.")
    (license license:bsd-3)))

(define-public ghc-code-page
  (package
  (name "ghc-code-page")
  (version "0.2")
  (source
   (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/code-page/code-page-"
           version ".tar.gz"))
      (sha256
       (base32
        "0i0qbrbhvrwkbikqb7hh7yxaipaavwzvyrw211d0vkz99f62mqxz"))))
  (build-system haskell-build-system)
  (home-page "https://github.com/RyanGlScott/code-page")
  (synopsis "Windows code page library for Haskell")
  (description "A cross-platform library with functions for adjusting
code pages on Windows.  On all other operating systems, the library does
nothing.")
  (license license:bsd-3)))

(define-public ghc-colour
(package
  (name "ghc-colour")
  (version "2.3.5")
  (source
   (origin
     (method url-fetch)
     (uri (string-append
           "https://hackage.haskell.org/package/colour/colour-"
           version ".tar.gz"))
     (sha256
      (base32
       "1rq4l46jx4lpdppy71wf7m1n7pw2jwy788rm35ycwzb1g4clg39v"))))
  (arguments
   ;; The tests for this package have the following dependency cycle:
   ;; ghc-test-framework -> ghc-ansi-terminal -> ghc-colour.
   `(#:tests? #f))
  (build-system haskell-build-system)
  (home-page "https://wiki.haskell.org/Colour")
  (synopsis "Model for human colour perception")
  (description
   "This package provides a data type for colours and transparency.
Colours can be blended and composed.  Various colour spaces are
supported.  A module of colour names (\"Data.Colour.Names\") is provided.")
  (license license:expat)))

(define-public ghc-comonad
  (package
    (name "ghc-comonad")
    (version "5.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/comonad/comonad-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1l7snp2mszgnjgd0nc9kzfyd13vla0rlazqi03rwx2akcxk14n3c"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("cabal-doctest" ,cabal-doctest)
       ("ghc-doctest" ,ghc-doctest)))
    (inputs
     `(("ghc-contravariant" ,ghc-contravariant)
       ("ghc-distributive" ,ghc-distributive)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-tagged" ,ghc-tagged)
       ("ghc-transformers-compat" ,ghc-transformers-compat)))
    (home-page "https://github.com/ekmett/comonad/")
    (synopsis "Comonads for Haskell")
    (description "This library provides @code{Comonad}s for Haskell.")
    (license license:bsd-3)))

(define-public ghc-concatenative
  (package
    (name "ghc-concatenative")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://hackage.haskell.org/package/concatenative/concatenative-"
                    version ".tar.gz"))
              (sha256
               (base32
                "05xwqvcdnk8bsyj698ab9jxpa1nk23pf3m7wi9mwmw0q8n99fngd"))))
    (build-system haskell-build-system)
    (home-page
     "https://patch-tag.com/r/salazar/concatenative/snapshot/current/content/pretty")
    (synopsis "Library for postfix control flow")
    (description
     "Concatenative gives Haskell Factor-style combinators and arrows for
postfix notation.  For more information on stack based languages, see
@uref{https://concatenative.org}.")
    (license license:bsd-3)))

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

(define-public ghc-concurrent-output
  (package
    (name "ghc-concurrent-output")
    (version "1.10.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://hackage/package/concurrent-output/concurrent-output-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1d1aaqg5814k59b0iws3fh06p3g2siaj922gkhs75qgncj0my2p3"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-async" ,ghc-async)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-ansi-terminal" ,ghc-ansi-terminal)
       ("ghc-terminal-size" ,ghc-terminal-size)))
    (home-page
     "https://hackage.haskell.org/package/concurrent-output")
    (synopsis
     "Ungarble output from several threads or commands")
    (description
     "Lets multiple threads and external processes concurrently output to the
console, without it getting all garbled up.

Built on top of that is a way of defining multiple output regions, which are
automatically laid out on the screen and can be individually updated by
concurrent threads.  Can be used for progress displays etc.")
    (license license:bsd-2)))

(define-public ghc-conduit
  (package
    (name "ghc-conduit")
    (version "1.3.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "conduit/conduit-" version ".tar.gz"))
              (sha256
               (base32
                "18izjgff4pmrknc8py06yvg3g6x27nx0rzmlwjxcflwm5v4szpw4"))))
    (build-system haskell-build-system)
    (outputs '("out" "static" "doc"))
    (inputs
     `(("ghc-exceptions" ,ghc-exceptions)
       ("ghc-lifted-base" ,ghc-lifted-base)
       ("ghc-mono-traversable" ,ghc-mono-traversable)
       ("ghc-mmorph" ,ghc-mmorph)
       ("ghc-resourcet" ,ghc-resourcet)
       ("ghc-silently" ,ghc-silently)
       ("ghc-transformers-base" ,ghc-transformers-base)
       ("ghc-unliftio" ,ghc-unliftio)
       ("ghc-unliftio-core" ,ghc-unliftio-core)
       ("ghc-vector" ,ghc-vector)
       ("ghc-void" ,ghc-void)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-safe" ,ghc-safe)
       ("ghc-split" ,ghc-split)))
    (home-page "https://github.com/snoyberg/conduit")
    (synopsis "Streaming data library ")
    (description
     "The conduit package is a solution to the streaming data problem,
allowing for production, transformation, and consumption of streams of data
in constant memory.  It is an alternative to lazy I/O which guarantees
deterministic resource handling, and fits in the same general solution
space as enumerator/iteratee and pipes.")
    (license license:expat)))

(define-public ghc-conduit-algorithms
  (package
    (name "ghc-conduit-algorithms")
    (version "0.0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "conduit-algorithms/conduit-algorithms-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0c1jwz30kkvimx7lb61782yk0kyfamrf5bqc3g1h7g51lk8bbv9i"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-async" ,ghc-async)
       ("ghc-bzlib-conduit" ,ghc-bzlib-conduit)
       ("ghc-conduit" ,ghc-conduit)
       ("ghc-conduit-combinators" ,ghc-conduit-combinators)
       ("ghc-conduit-extra" ,ghc-conduit-extra)
       ("ghc-conduit-zstd" ,ghc-conduit-zstd)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-lzma-conduit" ,ghc-lzma-conduit)
       ("ghc-monad-control" ,ghc-monad-control)
       ("ghc-pqueue" ,ghc-pqueue)
       ("ghc-resourcet" ,ghc-resourcet)
       ("ghc-stm-conduit" ,ghc-stm-conduit)
       ("ghc-streaming-commons" ,ghc-streaming-commons)
       ("ghc-unliftio-core" ,ghc-unliftio-core)
       ("ghc-vector" ,ghc-vector)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework-th" ,ghc-test-framework-th)))
    (home-page "https://github.com/luispedro/conduit-algorithms#readme")
    (synopsis "Conduit-based algorithms")
    (description
     "This package provides algorithms on @code{Conduits}, including higher
level asynchronous processing and some other utilities.")
    (license license:expat)))

(define-public ghc-conduit-combinators
  (package
    (name "ghc-conduit-combinators")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "conduit-combinators-" version "/"
                           "conduit-combinators-" version ".tar.gz"))
       (sha256
        (base32
         "1lz70vwp4y4lpsivxl0cshq7aq3968rh48r6rjvpyaj2l0bdj5wp"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-conduit" ,ghc-conduit)
              ("ghc-conduit-extra" ,ghc-conduit-extra)
              ("ghc-transformers-base" ,ghc-transformers-base)
              ("ghc-primitive" ,ghc-primitive)
              ("ghc-vector" ,ghc-vector)
              ("ghc-void" ,ghc-void)
              ("ghc-mwc-random" ,ghc-mwc-random)
              ("ghc-unix-compat" ,ghc-unix-compat)
              ("ghc-base16-bytestring" ,ghc-base16-bytestring)
              ("ghc-base64-bytestring" ,ghc-base64-bytestring)
              ("ghc-resourcet" ,ghc-resourcet)
              ("ghc-monad-control" ,ghc-monad-control)
              ("ghc-chunked-data" ,ghc-chunked-data)
              ("ghc-mono-traversable" ,ghc-mono-traversable)))
    (native-inputs `(("ghc-hspec" ,ghc-hspec)
                     ("ghc-silently" ,ghc-silently)
                     ("ghc-safe" ,ghc-safe)
                     ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://github.com/snoyberg/mono-traversable")
    (synopsis "Commonly used conduit functions, for both chunked and
unchunked data")
    (description "This Haskell package provides a replacement for Data.Conduit.List,
as well as a convenient Conduit module.")
    (license license:expat)))

(define-public ghc-conduit-extra
  (package
    (name "ghc-conduit-extra")
    (version "1.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "conduit-extra/conduit-extra-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1d853d39vj5pb8yxfcsnjwdzqzkm34ixzbnba8bslpihb7182wxi"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-conduit" ,ghc-conduit)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-monad-control" ,ghc-monad-control)
       ("ghc-transformers-base" ,ghc-transformers-base)
       ("ghc-typed-process" ,ghc-typed-process)
       ("ghc-async" ,ghc-async)
       ("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-blaze-builder" ,ghc-blaze-builder)
       ("ghc-network" ,ghc-network)
       ("ghc-primitive" ,ghc-primitive)
       ("ghc-resourcet" ,ghc-resourcet)
       ("ghc-streaming-commons" ,ghc-streaming-commons)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-bytestring-builder" ,ghc-bytestring-builder)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (native-inputs
     `(("hspec-discover" ,hspec-discover)))
    (home-page "https://github.com/snoyberg/conduit")
    (synopsis "Conduit adapters for common libraries")
    (description
     "The @code{conduit} package itself maintains relative small dependencies.
The purpose of this package is to collect commonly used utility functions
wrapping other library dependencies, without depending on heavier-weight
dependencies.  The basic idea is that this package should only depend on
@code{haskell-platform} packages and @code{conduit}.")
    (license license:expat)))

(define-public ghc-conduit-zstd
  (package
    (name "ghc-conduit-zstd")
    (version "0.0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "conduit-zstd/conduit-zstd-" version ".tar.gz"))
       (sha256
        (base32
         "04h7w2903hgw4gjcx2pg29yinnmfapawvc19hd3r57rr12fzb0c6"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-conduit" ,ghc-conduit)
       ("ghc-zstd" ,ghc-zstd)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-conduit-combinators" ,ghc-conduit-combinators)
       ("ghc-conduit-extra" ,ghc-conduit-extra)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework-th" ,ghc-test-framework-th)))
    (home-page "https://github.com/luispedro/conduit-zstd#readme")
    (synopsis "Conduit-based ZStd Compression")
    (description "Zstandard compression packaged as a conduit.  This is
a very thin wrapper around the
@url{https://github.com/facebookexperimental/hs-zstd/, official hs-zstd
interface}.")
    (license license:expat)))

(define-public ghc-configurator
  (package
    (name "ghc-configurator")
    (version "0.3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "configurator/configurator-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1d1iq1knwiq6ia5g64rw5hqm6dakz912qj13r89737rfcxmrkfbf"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-unix-compat" ,ghc-unix-compat)
       ("ghc-unordered-containers" ,ghc-unordered-containers)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)))
    (home-page "https://github.com/bos/configurator")
    (synopsis "Configuration management")
    (description
     "This package provides a configuration management library for programs
and daemons.  The features include:

@enumerate
@item Automatic, dynamic reloading in response to modifications to
  configuration files.
@item A simple, but flexible, configuration language, supporting several of
  the most commonly needed types of data, along with interpolation of strings
  from the configuration or the system environment (e.g. @code{$(HOME)}).
@item Subscription-based notification of changes to configuration properties.
@item An @code{import} directive allows the configuration of a complex
  application to be split across several smaller files, or common configuration
  data to be shared across several applications.
@end enumerate\n")
    (license license:bsd-3)))

(define-public ghc-connection
  (package
    (name "ghc-connection")
    (version "0.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "connection/connection-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1nbmafhlg0wy4aa3p7amjddbamdz6avzrxn4py3lvhrjqn4raxax"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-byteable" ,ghc-byteable)
       ("ghc-data-default-class" ,ghc-data-default-class)
       ("ghc-network" ,ghc-network)
       ("ghc-tls" ,ghc-tls)
       ("ghc-socks" ,ghc-socks)
       ("ghc-x509" ,ghc-x509)
       ("ghc-x509-store" ,ghc-x509-store)
       ("ghc-x509-system" ,ghc-x509-system)
       ("ghc-x509-validation" ,ghc-x509-validation)))
    (home-page "https://github.com/vincenthz/hs-connection")
    (synopsis "Simple and easy network connections API")
    (description
     "This package provides a simple network library for all your connection
needs.  It provides a very simple API to create sockets to a destination with
the choice of SSL/TLS, and SOCKS.")
    (license license:bsd-3)))

(define-public ghc-constraints
  (package
    (name "ghc-constraints")
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/constraints/constraints-"
             version ".tar.gz"))
       (sha256
        (base32
         "1xy3vv78jxc17hm0z7qqspxjwv7l2jbcbj670yrl2f053qkfr02q"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-hashable" ,ghc-hashable)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-transformers-compat" ,ghc-transformers-compat)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://github.com/ekmett/constraints/")
    (synopsis "Constraint manipulation")
    (description
     "GHC 7.4 gave us the ability to talk about @code{ConstraintKinds}.
They stopped crashing the compiler in GHC 7.6.  This package provides
a vocabulary for working with them.")
    (license license:bsd-3)))

(define-public ghc-contravariant
  (package
    (name "ghc-contravariant")
    (version "1.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/contravariant/contravariant-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0366gl62wwwdbl9i6kqy60asf60876k55v91la6bmhnwwcj2q9n4"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-void" ,ghc-void)
       ("ghc-transformers-compat" ,ghc-transformers-compat)
       ("ghc-statevar" ,ghc-statevar)
       ("ghc-semigroups" ,ghc-semigroups)))
    (home-page
     "https://github.com/ekmett/contravariant/")
    (synopsis "Contravariant functors")
    (description "Contravariant functors for Haskell.")
    (license license:bsd-3)))

(define-public ghc-contravariant-extras
  (package
    (name "ghc-contravariant-extras")
    (version "0.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "contravariant-extras-" version "/"
                           "contravariant-extras-" version ".tar.gz"))
       (sha256
        (base32
         "0gg62ccl94kvh7mnvdq09pifqxjx2kgs189si90nmg44bafj7a9n"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("1" "1h2955ahga6i4fn7k8v66l03v77p6fhsac6ck8gpabkc08ij60wp")))
    (inputs
     `(("ghc-tuple-th" ,ghc-tuple-th)
       ("ghc-contravariant" ,ghc-contravariant)
       ("ghc-base-prelude" ,ghc-base-prelude)
       ("ghc-semigroups" ,ghc-semigroups)))
    (home-page "https://github.com/nikita-volkov/contravariant-extras")
    (synopsis "Extras for the @code{ghc-contravariant} Haskell package")
    (description "This Haskell package provides extras for the
@code{ghc-contravariant} package.")
    (license license:expat)))

(define-public ghc-control-monad-free
  (package
    (name "ghc-control-monad-free")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/"
             "package/control-monad-free/control-monad-free-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1habgf7byffqf1rqjkzpihvdhclaafgqsqpfpwp3fgpj5ayk1j33"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/pepeiborra/control-monad-free")
    (synopsis "Free monads and monad transformers")
    (description
     "This package provides datatypes to construct Free monads, Free monad
transformers, and useful instances.  In addition it provides the constructs to
avoid quadratic complexity of left associative bind, as explained in:

@itemize @bullet
@item
Janis Voigtlander, @cite{Asymptotic Improvement of Computations over
Free Monads, MPC'08}
@end itemize")
    (license license:public-domain)))

(define-public ghc-convertible
  (package
    (name "ghc-convertible")
    (version "1.1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/convertible/"
                           "convertible-" version ".tar.gz"))
       (sha256
        (base32
         "0v18ap1mccnndgxmbfgyjdicg8jlss01bd5fq8a576dr0h4sgyg9"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-old-time" ,ghc-old-time)
       ("ghc-old-locale" ,ghc-old-locale)))
    (home-page "https://hackage.haskell.org/package/convertible")
    (synopsis "Typeclasses and instances for converting between types")
    (description
     "This package provides a typeclass with a single function that is
designed to help convert between different types: numeric values, dates and
times, and the like.  The conversions perform bounds checking and return a
pure @code{Either} value.  This means that you need not remember which specific
function performs the conversion you desire.")
    (license license:bsd-3)))

(define-public ghc-csv
  (package
    (name "ghc-csv")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/csv/csv-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "00767ai09wm7f0yzmpqck3cpgxncpr9djnmmz5l17ajz69139x4c"))))
    (build-system haskell-build-system)
    (home-page "http://hackage.haskell.org/package/csv")
    (synopsis "CSV loader and dumper")
    (description
     "This library parses and dumps documents that are formatted according to
RFC 4180, @cite{The common Format and MIME Type for Comma-Separated
Values (CSV) Files}.  This format is used, among many other things, as a
lingua franca for spreadsheets, and for certain web services.")
    (license license:expat)))

(define-public ghc-data-accessor
  (package
    (name "ghc-data-accessor")
    (version "0.2.2.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://hackage/package/data-accessor/data-accessor-"
             version ".tar.gz"))
       (sha256
        (base32 "1fq4gygxbz0bd0mzgvc1sl3m4gjnsv8nbgpnmdpa29zj5lb9agxc"))))
    (build-system haskell-build-system)
    (home-page "https://wiki.haskell.org/Record_access")
    (synopsis
     "Haskell utilities for accessing and manipulating fields of records")
    (description "This package provides Haskell modules for accessing and
manipulating fields of records.")
    (license license:bsd-3)))

(define-public ghc-data-accessor-transformers
  (package
    (name "ghc-data-accessor-transformers")
    (version "0.2.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://hackage/package/data-accessor-transformers/"
             "data-accessor-transformers-" version ".tar.gz"))
       (sha256
        (base32 "0yp030vafbpddl27m606aibbbr5ar5j5bsv4bksscz3cq4yq5j10"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-data-accessor" ,ghc-data-accessor)))
    (home-page "https://wiki.haskell.org/Record_access")
    (synopsis "Use Accessor to access state in transformers State monad")
    (description "This package provides Haskell modules to allow use of
Accessor to access state in transformers State monad.")
    (license license:bsd-3)))

(define-public ghc-data-default
  (package
    (name "ghc-data-default")
    (version "0.7.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/data-default/data-default-"
             version
             ".tar.gz"))
       (sha256
        (base32 "04d5n8ybmcxba9qb6h389w9zfq1lvj81b82jh6maqp6pkhkmvydh"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-data-default-class"
        ,ghc-data-default-class)
       ("ghc-data-default-instances-base"
        ,ghc-data-default-instances-base)
       ("ghc-data-default-instances-containers"
        ,ghc-data-default-instances-containers)
       ("ghc-data-default-instances-dlist"
        ,ghc-data-default-instances-dlist)
       ("ghc-data-default-instances-old-locale"
        ,ghc-data-default-instances-old-locale)))
    (home-page "https://hackage.haskell.org/package/data-default")
    (synopsis "Types with default values")
    (description
     "This package defines a class for types with a default value, and
provides instances for types from the base, containers, dlist and old-locale
packages.")
    (license license:bsd-3)))

(define-public ghc-data-default-class
  (package
    (name "ghc-data-default-class")
    (version "0.1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/data-default-class/"
             "data-default-class-" version ".tar.gz"))
       (sha256
        (base32 "0miyjz8d4jyvqf2vp60lyfbnflx6cj2k8apmm9ly1hq0y0iv80ag"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/data-default-class")
    (synopsis "Types with default values")
    (description
     "This package defines a class for types with default values.")
    (license license:bsd-3)))

(define-public ghc-data-default-instances-base
  (package
    (name "ghc-data-default-instances-base")
    (version "0.1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/"
             "data-default-instances-base/"
             "data-default-instances-base-" version ".tar.gz"))
       (sha256
        (base32 "0ym1sw3ssdzzifxxhh76qlv8kkmb2iclc158incv1dklyr9y8kw4"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-data-default-class" ,ghc-data-default-class)))
    (home-page "https://hackage.haskell.org/package/data-default-instances-base")
    (synopsis "Default instances for types in base")
    (description
     "This package provides default instances for types from the base
package.")
    (license license:bsd-3)))

(define-public ghc-data-default-instances-containers
  (package
    (name "ghc-data-default-instances-containers")
    (version "0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/"
             "data-default-instances-containers/"
             "data-default-instances-containers-" version ".tar.gz"))
       (sha256
        (base32 "06h8xka031w752a7cjlzghvr8adqbl95xj9z5zc1b62w02phfpm5"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-data-default-class" ,ghc-data-default-class)))
    (home-page "https://hackage.haskell.org/package/data-default-instances-containers")
    (synopsis "Default instances for types in containers")
    (description "Provides default instances for types from the containers
package.")
    (license license:bsd-3)))

(define-public ghc-data-default-instances-dlist
  (package
    (name "ghc-data-default-instances-dlist")
    (version "0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/"
             "data-default-instances-dlist/"
             "data-default-instances-dlist-" version ".tar.gz"))
       (sha256
        (base32 "0narkdqiprhgayjiawrr4390h4rq4pl2pb6mvixbv2phrc8kfs3x"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-data-default-class" ,ghc-data-default-class)
       ("ghc-dlist" ,ghc-dlist)))
    (home-page "https://hackage.haskell.org/package/data-default-instances-dlist")
    (synopsis "Default instances for types in dlist")
    (description "Provides default instances for types from the dlist
package.")
    (license license:bsd-3)))

(define-public ghc-data-default-instances-old-locale
  (package
    (name "ghc-data-default-instances-old-locale")
    (version "0.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
              "https://hackage.haskell.org/package/"
              "data-default-instances-old-locale/"
              "data-default-instances-old-locale-" version ".tar.gz"))
        (sha256
          (base32 "00h81i5phib741yj517p8mbnc48myvfj8axzsw44k34m48lv1lv0"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-data-default-class" ,ghc-data-default-class)
       ("ghc-old-locale" ,ghc-old-locale)))
    (home-page
      "https://hackage.haskell.org/package/data-default-instances-old-locale")
    (synopsis "Default instances for types in old-locale")
    (description "Provides Default instances for types from the old-locale
  package.")
    (license license:bsd-3)))

(define-public ghc-data-fix
  (package
    (name "ghc-data-fix")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://hackage/package/data-fix/"
             "data-fix-" version ".tar.gz"))
       (sha256
        (base32 "14hk6hq5hdb3l5bhmzhw086jpzlvp9qbw9dzw30wlz5jbh2ihmvy"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/spell-music/data-fix")
    (synopsis "Fixpoint data types")
    (description
     "Fixpoint types and recursion schemes.  If you define your AST as
fixpoint type, you get fold and unfold operations for free.

Thanks for contribution to: Matej Kollar, Herbert Valerio Riedel")
    (license license:bsd-3)))

(define-public ghc-data-hash
  (package
    (name "ghc-data-hash")
    (version "0.2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/data-hash"
                           "/data-hash-" version ".tar.gz"))
       (sha256
        (base32 "1ghbqvc48gf9p8wiy71hdpaj7by3b9cw6wgwi3qqz8iw054xs5wi"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)))
    (home-page "https://hackage.haskell.org/package/data-hash")
    (synopsis "Combinators for building fast hashing functions")
    (description
     "This package provides combinators for building fast hashing functions.
It includes hashing functions for all basic Haskell98 types.")
    (license license:bsd-3)))

(define-public ghc-data-ordlist
  (package
    (name "ghc-data-ordlist")
    (version "0.4.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/data-ordlist/data-ordlist-"
             version ".tar.gz"))
       (sha256
        (base32
         "03a9ix1fcx08viwv2jg5ndw1qbkydyyrmjvqr9wasmcik9x1wv3g"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/data-ordlist")
    (synopsis "Set and bag operations on ordered lists")
    (description
     "This module provides set and multiset operations on ordered lists.")
    (license license:bsd-3)))

(define-public ghc-dbus
  (package
    (name "ghc-dbus")
    (version "1.2.7")
    (source
      (origin
        (method url-fetch)
        (uri
         (string-append
          "mirror://hackage/package/dbus/dbus-"
          version ".tar.gz"))
        (sha256
          (base32
            "0ypkjlw9fn65g7p28kb3p82glk7qs7p7vyffccw7qxa3z57s12w5"))))
    (build-system haskell-build-system)
    (inputs
      `(("ghc-cereal" ,ghc-cereal)
        ("ghc-conduit" ,ghc-conduit)
        ("ghc-exceptions" ,ghc-exceptions)
        ("ghc-lens" ,ghc-lens)
        ("ghc-network" ,ghc-network)
        ("ghc-random" ,ghc-random)
        ("ghc-split" ,ghc-split)
        ("ghc-th-lift" ,ghc-th-lift)
        ("ghc-vector" ,ghc-vector)
        ("ghc-xml-conduit" ,ghc-xml-conduit)
        ("ghc-xml-types" ,ghc-xml-types)))
    (native-inputs
      `(("ghc-extra" ,ghc-extra)
        ("ghc-quickcheck" ,ghc-quickcheck)
        ("ghc-resourcet" ,ghc-resourcet)
        ("ghc-tasty" ,ghc-tasty)
        ("ghc-tasty-hunit" ,ghc-tasty-hunit)
        ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)))
    ;; FIXME - Some tests try to talk to network.
    (arguments `(#:tests? #f))
    (home-page "https://github.com/rblaze/haskell-dbus")
    (synopsis "Client library for the D-Bus IPC system")
    (description
      "D-Bus is a simple, message-based protocol for inter-process
communication, which allows applications to interact with other parts
of the machine and the user's session using remote procedure
calls.   D-Bus is a essential part of the modern Linux desktop, where
it replaces earlier protocols such as CORBA and DCOP.  This library
is an implementation of the D-Bus protocol in Haskell.  It can be used
to add D-Bus support to Haskell applications, without the awkward
interfaces common to foreign bindings.")
    (license license:asl2.0)))

(define-public ghc-decimal
  (package
    (name "ghc-decimal")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/Decimal/Decimal-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0k7kh05mr2f54w1lpgq1nln0h8k6s6h99dyp5jzsb9cfbb3aap2p"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)))
    (home-page "https://github.com/PaulJohnson/Haskell-Decimal")
    (synopsis "Decimal numbers with variable precision")
    (description
     "A decimal number has an integer mantissa and a negative exponent.
The exponent can be interpreted as the number of decimal places in the
value.")
    (license license:bsd-3)))

(define-public ghc-deepseq-generics
  (package
    (name "ghc-deepseq-generics")
    (version "0.2.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "deepseq-generics/deepseq-generics-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "17bwghc15mc9pchfd1w46jh2p3wzc86aj6a537wqwxn08rayzcxh"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("4" "0928s2qnbqsjzrm94x88rvmvbigfmhcyp4m73gw6asinp2qg1kii")))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)))
    (home-page "https://github.com/hvr/deepseq-generics")
    (synopsis "Generic RNF implementation")
    (description
     "This package provides a @code{GHC.Generics}-based
@code{Control.DeepSeq.Generics.genericRnf} function which can be used for
providing an @code{rnf} implementation.")
    (license license:bsd-3)))

(define-public ghc-dense-linear-algebra
  (package
    (name "ghc-dense-linear-algebra")
    (version "0.1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "dense-linear-algebra/dense-linear-algebra-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1m7jjxahqxj7ilic3r9806mwp5rnnsmn8vvipkmk40xl65wplxzp"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-math-functions" ,ghc-math-functions)
       ("ghc-primitive" ,ghc-primitive)
       ("ghc-vector" ,ghc-vector)
       ("ghc-vector-algorithms" ,ghc-vector-algorithms)
       ("ghc-vector-th-unbox" ,ghc-vector-th-unbox)
       ("ghc-vector-binary-instances" ,ghc-vector-binary-instances)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://hackage.haskell.org/package/dense-linear-algebra")
    (synopsis "Simple and incomplete implementation of linear algebra")
    (description "This library is simply a collection of linear-algebra
related modules split from the statistics library.")
    (license license:bsd-2)))

(define-public ghc-descriptive
  (package
    (name "ghc-descriptive")
    (version "0.9.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/descriptive/descriptive-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0y5693zm2kvqjilybbmrcv1g6n6x2p6zjgi0k0axjw1sdhh1g237"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-aeson" ,ghc-aeson)
       ("ghc-bifunctors" ,ghc-bifunctors)
       ("ghc-scientific" ,ghc-scientific)
       ("ghc-vector" ,ghc-vector)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-hspec" ,ghc-hspec)))
    (home-page
     "https://github.com/chrisdone/descriptive")
    (synopsis
     "Self-describing consumers/parsers: forms, cmd-line args, JSON, etc.")
    (description
     "This package provides datatypes and functions for creating consumers
and parsers with useful semantics.")
    (license license:bsd-3)))

(define-public ghc-diagrams-core
  (package
    (name "ghc-diagrams-core")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "diagrams-core/diagrams-core-" version ".tar.gz"))
       (sha256
        (base32
         "0qgb43vy23g4fxh3nmxfq6jyp34imqvkhgflaa6rz0iq6d60gl43"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-monoid-extras" ,ghc-monoid-extras)
       ("ghc-dual-tree" ,ghc-dual-tree)
       ("ghc-lens" ,ghc-lens)
       ("ghc-linear" ,ghc-linear)
       ("ghc-adjunctions" ,ghc-adjunctions)
       ("ghc-distributive" ,ghc-distributive)
       ("ghc-profunctors" ,ghc-profunctors)))
    (home-page "https://archives.haskell.org/projects.haskell.org/diagrams/")
    (synopsis "Core libraries for diagrams embedded domain-specific language")
    (description "This package provides the core modules underlying
diagrams, an embedded domain-specific language for compositional,
declarative drawing.")
    (license license:bsd-3)))

(define-public ghc-diagrams-lib
  (package
    (name "ghc-diagrams-lib")
    (version "1.4.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "diagrams-lib/diagrams-lib-" version ".tar.gz"))
       (sha256
        (base32
         "175yzi5kw4yd8ykdkpf64q85c7j3p89l90m3h6qcsx9ipv6av9r5"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-semigroups" ,ghc-semigroups)
       ("ghc-monoid-extras" ,ghc-monoid-extras)
       ("ghc-dual-tree" ,ghc-dual-tree)
       ("ghc-diagrams-core" ,ghc-diagrams-core)
       ("ghc-diagrams-solve" ,ghc-diagrams-solve)
       ("ghc-active" ,ghc-active)
       ("ghc-colour" ,ghc-colour)
       ("ghc-data-default-class" ,ghc-data-default-class)
       ("ghc-fingertree" ,ghc-fingertree)
       ("ghc-intervals" ,ghc-intervals)
       ("ghc-lens" ,ghc-lens)
       ("ghc-tagged" ,ghc-tagged)
       ("ghc-optparse-applicative" ,ghc-optparse-applicative)
       ("ghc-juicypixels" ,ghc-juicypixels)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-linear" ,ghc-linear)
       ("ghc-adjunctions" ,ghc-adjunctions)
       ("ghc-distributive" ,ghc-distributive)
       ("ghc-fsnotify" ,ghc-fsnotify)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-profunctors" ,ghc-profunctors)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-cereal" ,ghc-cereal)))
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
       ("ghc-numeric-extras" ,ghc-numeric-extras)))
    (arguments
     `(#:cabal-revision
       ("3" "157y2qdsh0aczs81vzlm377mks976mpv6y3aqnchwsnr7apzp8ai")))
    (home-page "https://archives.haskell.org/projects.haskell.org/diagrams/")
    (synopsis "Embedded domain-specific language for declarative graphics")
    (description "Diagrams is a flexible, extensible embedded
domain-specific language (EDSL) for creating graphics of many types.
Graphics can be created in arbitrary vector spaces and rendered with
multiple backends.  This package provides a standard library of
primitives and operations for creating diagrams.")
    (license license:bsd-3)))

(define-public ghc-diagrams-solve
  (package
    (name "ghc-diagrams-solve")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "diagrams-solve/diagrams-solve-"
                           version ".tar.gz"))
       (sha256
        (base32
         "17agchqkmj14b17sw50kzxq4hm056g5d8yy0wnqn5w8h1d0my7x4"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)))
    (arguments
     `(#:cabal-revision
       ("5" "1yl8cs05fzqcz49p601am1ij66m9pa70yamhfxgcvya2pf8nimlf")))
    (home-page "https://archives.haskell.org/projects.haskell.org/diagrams/")
    (synopsis "Pure Haskell solver routines used by diagrams")
    (description "This library provides Pure Haskell solver routines for
use by the
@url{https://archives.haskell.org/projects.haskell.org/diagrams/,
diagrams framework}.  It currently includes routines for finding real
roots of low-degree (@math{n < 5}) polynomials, and solving tridiagonal
and cyclic tridiagonal linear systems.")
    (license license:bsd-3)))

(define-public ghc-diagrams-svg
  (package
    (name "ghc-diagrams-svg")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "diagrams-svg/diagrams-svg-" version ".tar.gz"))
       (sha256
        (base32
         "1lnyxx45yawqas7hmvvannwaa3ycf1l9g40lsl2m8sl2ja6vcmal"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-base64-bytestring" ,ghc-base64-bytestring)
       ("ghc-colour" ,ghc-colour)
       ("ghc-diagrams-core" ,ghc-diagrams-core)
       ("ghc-diagrams-lib" ,ghc-diagrams-lib)
       ("ghc-monoid-extras" ,ghc-monoid-extras)
       ("ghc-svg-builder" ,ghc-svg-builder)
       ("ghc-juicypixels" ,ghc-juicypixels)
       ("ghc-split" ,ghc-split)
       ("ghc-lens" ,ghc-lens)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-optparse-applicative" ,ghc-optparse-applicative)
       ("ghc-semigroups" ,ghc-semigroups)))
    (arguments
     `(#:cabal-revision
       ("2" "15sn85xaachw4cj56w61bjcwrbf4qmnkfl8mbgdapxi5k0y4f2qv")))
    (home-page "https://archives.haskell.org/projects.haskell.org/diagrams/")
    (synopsis "Scalable Vector Grpahics backend for the diagrams framework")
    (description "This package provides a modular backend for rendering
diagrams created with the diagrams embedded domain-specific
language (EDSL) to Scalable Vector Graphics (SVG) files.")
    (license license:bsd-3)))

(define-public ghc-dictionary-sharing
  (package
    (name "ghc-dictionary-sharing")
    (version "0.1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://hackage.haskell.org/package/"
                            "dictionary-sharing/dictionary-sharing-"
                            version ".tar.gz"))
        (sha256
          (base32
            "00aspv943qdqhlk39mbk00kb1dsa5r0caj8sslrn81fnsn252fwc"))))
    (build-system haskell-build-system)
    (arguments
      `(#:cabal-revision
        ("3" "1mn7jcc7h3b8f1pn9zigqp6mc2n0qb66lms5qnrx4zswdv5w9439")))
    (home-page "https://hackage.haskell.org/package/dictionary-sharing")
    (synopsis "Sharing/memoization of class members")
    (description "This library provides tools for ensuring that class
members are shared.")
    (license license:bsd-3)))

(define-public ghc-diff
  (package
    (name "ghc-diff")
    (version "0.3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "Diff/Diff-" version ".tar.gz"))
              (patches (search-patches "ghc-diff-swap-cover-args.patch"))
              (sha256
               (base32
                "0bqcdvhxx8dmqc3793m6axg813wv9ldz2j37f1wygbbrbbndmdvp"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)))
    (home-page "https://hub.darcs.net/sterlingclover/Diff")
    (synopsis "O(ND) diff algorithm in Haskell")
    (description
     "This package provides an implementation of the standard diff algorithm,
and utilities for pretty printing.")
    (license license:bsd-3)))

(define-public ghc-disk-free-space
  (package
    (name "ghc-disk-free-space")
    (version "0.1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "disk-free-space/disk-free-space-"
                           version ".tar.gz"))
       (sha256
        (base32
         "07rqj8k1vh3cykq9yidpjxhgh1f7vgmjs6y1nv5kq2217ff4yypi"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/redneb/disk-free-space")
    (synopsis "Retrieve information about disk space usage")
    (description "A cross-platform library for retrieving information about
disk space usage.")
    (license license:bsd-3)))

(define-public ghc-distributive
  (package
    (name "ghc-distributive")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/distributive/distributive-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1j93zkfffm6s16kgr0j0z87y5ds28rw0r2gyc5ncwcylvrqy4kl2"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-tagged" ,ghc-tagged)
       ("ghc-base-orphans" ,ghc-base-orphans)
       ("ghc-transformers-compat" ,ghc-transformers-compat)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-generic-deriving" ,ghc-generic-deriving)))
    (native-inputs
     `(("cabal-doctest" ,cabal-doctest)
       ("ghc-doctest" ,ghc-doctest)
       ("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://github.com/ekmett/distributive/")
    (synopsis "Distributive functors for Haskell")
    (description "This package provides distributive functors for Haskell.
Dual to @code{Traversable}.")
    (license license:bsd-3)))

(define-public ghc-dlist
  (package
    (name "ghc-dlist")
    (version "0.8.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/dlist/dlist-"
             version
             ".tar.gz"))
       (sha256
        (base32 "0b5spkzvj2kx8pk86xz0djkxs13j7dryf5fl16dk4mlp1wh6mh53"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://github.com/spl/dlist")
    (synopsis "Difference lists")
    (description
     "Difference lists are a list-like type supporting O(1) append.  This is
particularly useful for efficient logging and pretty printing (e.g. with the
Writer monad), where list append quickly becomes too expensive.")
    (license license:bsd-3)))

(define-public ghc-doctemplates
  (package
    (name "ghc-doctemplates")
    (version "0.2.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "doctemplates/doctemplates-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1gyckfg3kgvzhxw14i7iwrw0crygvsp86sy53bbr1yn7bxbgn33b"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-aeson" ,ghc-aeson)
       ("ghc-blaze-markup" ,ghc-blaze-markup)
       ("ghc-blaze-html" ,ghc-blaze-html)
       ("ghc-vector" ,ghc-vector)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-scientific" ,ghc-scientific)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)))
    (home-page "https://github.com/jgm/doctemplates#readme")
    (synopsis "Pandoc-style document templates")
    (description
     "This package provides a simple text templating system used by pandoc.")
    (license license:bsd-3)))

(define-public ghc-doctest
  (package
    (name "ghc-doctest")
    (version "0.16.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/doctest/doctest-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0lk4cjfzi5bx2snfzw1zi06li0gvgz3ckfh2kwa98l7nxfdl39ag"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f))          ; FIXME: missing test framework
    (inputs
     `(("ghc-base-compat" ,ghc-base-compat)
       ("ghc-code-page" ,ghc-code-page)
       ("ghc-paths" ,ghc-paths)
       ("ghc-syb" ,ghc-syb)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-mockery" ,ghc-mockery)
       ("ghc-setenv" ,ghc-setenv)
       ("ghc-silently" ,ghc-silently)
       ("ghc-stringbuilder" ,ghc-stringbuilder)))
    (home-page
     "https://github.com/sol/doctest#readme")
    (synopsis "Test interactive Haskell examples")
    (description "The doctest program checks examples in source code comments.
It is modeled after doctest for Python, see
@uref{https://docs.python.org/library/doctest.html, the Doctest website}.")
    (license license:expat)))

(define-public ghc-dotgen
  (package
    (name "ghc-dotgen")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://hackage/package/dotgen/dotgen-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "148q93qsmqgr5pzdwvpjqfd6bdm1pwzcp2rblfwswx2x8c5f43fg"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/ku-fpg/dotgen")
    (synopsis
     "Simple interface for building .dot graph files")
    (description
     "This package provides a simple interface for building .dot graph
files, for input into the dot and graphviz tools.  It includes a
monadic interface for building graphs.")
    (license license:bsd-3)))

(define-public ghc-double-conversion
  (package
    (name "ghc-double-conversion")
    (version "2.0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "double-conversion/double-conversion-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0sx2kc1gw72mjvd8vph8bbjw5whfxfv92rsdhjg1c0al75rf3ka4"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)))
    (home-page "https://github.com/bos/double-conversion")
    (synopsis "Fast conversion between double precision floating point and text")
    (description
     "This package provides a library that performs fast, accurate conversion
between double precision floating point and text.")
    (license license:bsd-3)))

(define-public ghc-dual-tree
  (package
    (name "ghc-dual-tree")
    (version "0.2.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "dual-tree/dual-tree-" version ".tar.gz"))
       (sha256
        (base32
         "17kdfnf0df0z5pkiifxrlmyd1xd7hjjaazd2kzyajl0gd00vbszx"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-semigroups" ,ghc-semigroups)
       ("ghc-newtype-generics" ,ghc-newtype-generics)
       ("ghc-monoid-extras" ,ghc-monoid-extras)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-testing-feat" ,ghc-testing-feat)))
    (home-page "https://hackage.haskell.org/package/dual-tree")
    (synopsis "Rose trees with cached and accumulating monoidal annotations")
    (description "Rose (@math{n}-ary) trees with both upwards- (i.e.
cached) and downwards-traveling (i.e. accumulating) monoidal
annotations.  This is used as the core data structure underlying the
@url{https://archives.haskell.org/projects.haskell.org/diagrams/,
diagrams framework}, but potentially has other applications as well.")
    (license license:bsd-3)))

(define-public ghc-easy-file
  (package
    (name "ghc-easy-file")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/easy-file/easy-file-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0zmlcz723051qpn8l8vi51c5rx1blwrw4094jcshkmj8p9r2xxaj"))))
    (build-system haskell-build-system)
    (home-page
     "https://github.com/kazu-yamamoto/easy-file")
    (synopsis "File handling library for Haskell")
    (description "This library provides file handling utilities for Haskell.")
    (license license:bsd-3)))

(define-public ghc-easyplot
  (package
    (name "ghc-easyplot")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/easyplot/easyplot-"
             version ".tar.gz"))
       (sha256
        (base32 "18kndgvdj2apjpfga6fp7m16y1gx8zrwp3c5vfj03sx4v6jvciqk"))))
    (build-system haskell-build-system)
    (propagated-inputs `(("gnuplot" ,gnuplot)))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'fix-setup-suffix
                    (lambda _ (rename-file "Setup.lhs" "Setup.hs") #t)))))
    (home-page "https://hub.darcs.net/scravy/easyplot")
    (synopsis "Haskell plotting library based on gnuplot")
    (description "This package provides a plotting library for
Haskell, using gnuplot for rendering.")
    (license license:expat)))

(define-public ghc-echo
  (package
    (name "ghc-echo")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/echo/echo-"
             version ".tar.gz"))
       (sha256
        (base32
         "1vw5ykpwhr39wc0hhcgq3r8dh59zq6ib4zxbz1qd2wl21wqhfkvh"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("1" "0br8wfiybcw5hand4imiw0i5hacdmrax1dv8g95f35gazffbx42l")))
    (home-page "https://github.com/RyanGlScott/echo")
    (synopsis "Echo terminal input portably")
    (description "The @code{base} library exposes the @code{hGetEcho} and
@code{hSetEcho} functions for querying and setting echo status, but
unfortunately, neither function works with MinTTY consoles on Windows.
This library provides an alternative interface which works with both
MinTTY and other consoles.")
    (license license:bsd-3)))

(define-public ghc-edisonapi
  (package
    (name "ghc-edisonapi")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/EdisonAPI"
                           "/EdisonAPI-" version ".tar.gz"))
       (sha256
        (base32 "0vmmlsj8ggbpwx6fkf5fvb6jp0zpx6iba6b28m80lllr2p8bi8wm"))))
    (build-system haskell-build-system)
    (home-page "http://rwd.rdockins.name/edison/home/")
    (synopsis "Library of efficient, purely-functional data structures (API)")
    (description
     "Edison is a library of purely functional data structures written by
Chris Okasaki.  It is named after Thomas Alva Edison and for the mnemonic
value EDiSon (Efficient Data Structures).  Edison provides several families of
abstractions, each with multiple implementations.  The main abstractions
provided by Edison are: Sequences such as stacks, queues, and dequeues;
Collections such as sets, bags and heaps; and Associative Collections such as
finite maps and priority queues where the priority and element are distinct.")
    (license license:expat)))

(define-public ghc-edisoncore
  (package
    (name "ghc-edisoncore")
    (version "1.3.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/EdisonCore"
                           "/EdisonCore-" version ".tar.gz"))
       (sha256
        (base32 "0fgj5iwiv3v2gdgx7kjcr15dcs4x1kvmjspp3p99wyhh0x6h3ikk"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-edisonapi" ,ghc-edisonapi)))
    (home-page "http://rwd.rdockins.name/edison/home/")
    (synopsis "Library of efficient, purely-functional data structures")
    (description
     "This package provides the core Edison data structure implementations,
including multiple sequence, set, bag, and finite map concrete implementations
with various performance characteristics.")
    (license license:expat)))

(define-public ghc-edit-distance
  (package
    (name "ghc-edit-distance")
    (version "0.2.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/edit-distance"
                           "/edit-distance-" version ".tar.gz"))
       (sha256
        (base32 "0jkca97zyv23yyilp3jydcrzxqhyk27swhzh82llvban5zp8b21y"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'update-constraints
           (lambda _
             (substitute* "edit-distance.cabal"
               (("QuickCheck >= 2\\.4 && <2\\.9")
                "QuickCheck >= 2.4 && < 2.14")))))))
    (inputs
     `(("ghc-random" ,ghc-random)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)))
    (home-page "https://github.com/phadej/edit-distance")
    (synopsis "Levenshtein and restricted Damerau-Levenshtein edit distances")
    (description
     "This package provides optimized functions to determine the edit
distances for fuzzy matching, including Levenshtein and restricted
Damerau-Levenshtein algorithms.")
    (license license:bsd-3)))

(define-public ghc-edit-distance-vector
  (package
    (name "ghc-edit-distance-vector")
    (version "1.0.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "edit-distance-vector/edit-distance-vector-"
                           version ".tar.gz"))
       (sha256
        (base32
         "07qgc8dyi9kkzkd3xcd78wdlljy0xwhz65b4r2qg2piidpcdvpxp"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-vector" ,ghc-vector)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-quickcheck-instances" ,ghc-quickcheck-instances)))
    (home-page "https://github.com/thsutton/edit-distance-vector")
    (synopsis "Calculate edit distances and edit scripts between vectors")
    (description "This package provides implementation of the
Wagner-Fischer dynamic programming algorithm to find the optimal edit
script and cost between two sequences.  The implementation in this
package is specialised to sequences represented with @code{Data.Vector}
but is otherwise agnostic to:
@itemize
@item The type of values in the vectors;
@item The type representing edit operations; and
@item The type representing the cost of operations.
@end itemize")
    (license license:bsd-3)) )

(define-public ghc-either
  (package
    (name "ghc-either")
    (version "5.0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "either-" version "/"
                           "either-" version ".tar.gz"))
       (sha256
        (base32
         "09yzki8ss56xhy9vggdw1rls86b2kf55hjl5wi0vbv02d8fxahq2"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-bifunctors" ,ghc-bifunctors)
              ("ghc-exceptions" ,ghc-exceptions)
              ("ghc-free" ,ghc-free)
              ("ghc-monad-control" ,ghc-monad-control)
              ("ghc-manodrandom" ,ghc-monadrandom)
              ("ghc-mmorph" ,ghc-mmorph)
              ("ghc-profunctors" ,ghc-profunctors)
              ("ghc-semigroups" ,ghc-semigroups)
              ("ghc-semigroupoids" ,ghc-semigroupoids)
              ("ghc-transformers-base" ,ghc-transformers-base)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)))
    (home-page "https://github.com/ekmett/either")
    (synopsis "Provides an either monad transformer for Haskell")
    (description "This Haskell package provides an either monad transformer.")
    (license license:bsd-3)))

(define-public ghc-email-validate
  (package
    (name "ghc-email-validate")
    (version "2.3.2.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/"
             "email-validate/email-validate-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0ar3cfjia3x11chb7w60mi7hp5djanms883ddk875l6lifr2lyqf"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-doctest" ,ghc-doctest)))
    (home-page
     "https://github.com/Porges/email-validate-hs")
    (synopsis "Email address validator for Haskell")
    (description
     "This Haskell package provides a validator that can validate an email
address string against RFC 5322.")
    (license license:bsd-3)))

(define-public ghc-enclosed-exceptions
  (package
    (name "ghc-enclosed-exceptions")
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "enclosed-exceptions/enclosed-exceptions-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1fghjj7nkiddrf03ks8brjpr5x25yi9fs7xg6adbi4mc2gqr6vdg"))))
    (build-system haskell-build-system)
    ;; FIXME: one of the tests blocks forever:
    ;; "thread blocked indefinitely in an MVar operation"
    (arguments '(#:tests? #f))
    (inputs
     `(("ghc-lifted-base" ,ghc-lifted-base)
       ("ghc-monad-control" ,ghc-monad-control)
       ("ghc-async" ,ghc-async)
       ("ghc-transformers-base" ,ghc-transformers-base)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://github.com/jcristovao/enclosed-exceptions")
    (synopsis "Catch all exceptions from within an enclosed computation")
    (description
     "This library implements a technique to catch all exceptions raised
within an enclosed computation, while remaining responsive to (external)
asynchronous exceptions.")
    (license license:expat)))

(define-public ghc-equivalence
  (package
    (name "ghc-equivalence")
    (version "0.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/equivalence"
                           "/equivalence-" version ".tar.gz"))
       (sha256
        (base32 "167njzd1cf32aa7br90rjafrxy6hw3fxkk8awifqbxjrcwm5maqp"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-stmonadtrans" ,ghc-stmonadtrans)
       ("ghc-transformers-compat" ,ghc-transformers-compat)
       ("ghc-fail" ,ghc-fail)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://github.com/pa-ba/equivalence")
    (synopsis "Maintaining an equivalence relation implemented as union-find")
    (description
     "This is an implementation of Tarjan's Union-Find algorithm (Robert E.@:
Tarjan.  \"Efficiency of a Good But Not Linear Set Union Algorithm\",JACM
22(2), 1975) in order to maintain an equivalence relation.  This
implementation is a port of the @code{union-find} package using the @code{ST}
monad transformer (instead of the IO monad).")
    (license license:bsd-3)))

(define-public ghc-erf
  (package
    (name "ghc-erf")
    (version "2.0.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "erf-" version "/"
                           "erf-" version ".tar.gz"))
       (sha256
        (base32
         "0dxk2r32ajmmc05vaxcp0yw6vgv4lkbmh8jcshncn98xgsfbgw14"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/erf")
    (synopsis "The error function, erf, and related functions for Haskell")
    (description "This Haskell library provides a type class for the
error function, erf, and related functions.  Instances for Float and
Double.")
    (license license:bsd-3)))

(define-public ghc-errorcall-eq-instance
  (package
    (name "ghc-errorcall-eq-instance")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "errorcall-eq-instance/errorcall-eq-instance-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0hqw82m8bbrxy5vgdwb83bhzdx070ibqrm9rshyja7cb808ahijm"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-base-orphans" ,ghc-base-orphans)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://hackage.haskell.org/package/errorcall-eq-instance")
    (synopsis "Orphan Eq instance for ErrorCall")
    (description
     "Prior to @code{base-4.7.0.0} there was no @code{Eq} instance for @code{ErrorCall}.
This package provides an orphan instance.")
    (license license:expat)))

(define-public ghc-errors
  (package
    (name "ghc-errors")
    (version "2.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "errors-" version "/"
                           "errors-" version ".tar.gz"))
       (sha256
        (base32
         "0x8znwn31qcx6kqx99wp7bc86kckfb39ncz3zxvj1s07kxlfawk7"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-exceptions" ,ghc-exceptions)
       ("ghc-transformers-compat" ,ghc-transformers-compat)
       ("ghc-unexceptionalio" ,ghc-unexceptionalio)
       ("ghc-safe" ,ghc-safe)))
    (home-page "https://github.com/gabriel439/haskell-errors-library")
    (synopsis "Error handling library for Haskell")
    (description "This library encourages an error-handling style that
directly uses the type system, rather than out-of-band exceptions.")
    (license license:bsd-3)))

(define-public ghc-esqueleto
  (package
    (name "ghc-esqueleto")
    (version "3.3.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "esqueleto/esqueleto-" version ".tar.gz"))
       (sha256
        (base32
         "1qi28ma8j5kfygjxnixlazxsyrkdqv8ljz3icwqi5dlscsnj6v3v"))))
    (build-system haskell-build-system)
    (arguments
     `(#:haddock? #f  ; Haddock reports an internal error.
       #:phases
       (modify-phases %standard-phases
         ;; This package normally runs tests for the MySQL, PostgreSQL, and
         ;; SQLite backends.  Since we only have Haskell packages for
         ;; SQLite, we remove the other two test suites.  FIXME: Add the
         ;; other backends and run all three test suites.
         (add-before 'configure 'remove-non-sqlite-test-suites
           (lambda _
             (use-modules (ice-9 rdelim))
             (with-atomic-file-replacement "esqueleto.cabal"
               (lambda (in out)
                 (let loop ((line (read-line in 'concat)) (deleting? #f))
                   (cond
                    ((eof-object? line) #t)
                    ((string-every char-set:whitespace line)
                     (unless deleting? (display line out))
                     (loop (read-line in 'concat) #f))
                    ((member line '("test-suite mysql\n"
                                    "test-suite postgresql\n"))
                     (loop (read-line in 'concat) #t))
                    (else
                     (unless deleting? (display line out))
                     (loop (read-line in 'concat) deleting?)))))))))))
    (inputs
     `(("ghc-blaze-html" ,ghc-blaze-html)
       ("ghc-conduit" ,ghc-conduit)
       ("ghc-monad-logger" ,ghc-monad-logger)
       ("ghc-persistent" ,ghc-persistent)
       ("ghc-resourcet" ,ghc-resourcet)
       ("ghc-tagged" ,ghc-tagged)
       ("ghc-unliftio" ,ghc-unliftio)
       ("ghc-unordered-containers" ,ghc-unordered-containers)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("ghc-persistent-sqlite" ,ghc-persistent-sqlite)
       ("ghc-persistent-template" ,ghc-persistent-template)))
    (home-page "https://github.com/bitemyapp/esqueleto")
    (synopsis "Type-safe embedded domain specific language for SQL queries")
    (description "This library provides a type-safe embedded domain specific
language (EDSL) for SQL queries that works with SQL backends as provided by
@code{ghc-persistent}.  Its language closely resembles SQL, so you don't have
to learn new concepts, just new syntax, and it's fairly easy to predict the
generated SQL and optimize it for your backend.")
    (license license:bsd-3)))

(define-public ghc-exactprint
  (package
    (name "ghc-exactprint")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/"
             "ghc-exactprint/ghc-exactprint-" version ".tar.gz"))
       (sha256
        (base32
         "12nqpqmi9c57a3hgpfy8q073zryz66ylmcvf29hyffpj7vmmnvhl"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-paths" ,ghc-paths)
       ("ghc-syb" ,ghc-syb)
       ("ghc-free" ,ghc-free)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-diff" ,ghc-diff)
       ("ghc-silently" ,ghc-silently)
       ("ghc-filemanip" ,ghc-filemanip)))
    (home-page
     "https://hackage.haskell.org/package/ghc-exactprint")
    (synopsis "ExactPrint for GHC")
    (description
     "Using the API Annotations available from GHC 7.10.2, this library
provides a means to round-trip any code that can be compiled by GHC, currently
excluding @file{.lhs} files.")
    (license license:bsd-3)))

(define-public ghc-exceptions
  (package
    (name "ghc-exceptions")
    (version "0.10.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/exceptions/exceptions-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1w25j4ys5s6v239vbqlbipm9fdwxl1j2ap2lzms7f7rgnik5ir24"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)))
    (inputs
     `(("ghc-transformers-compat" ,ghc-transformers-compat)))
    (home-page "https://github.com/ekmett/exceptions/")
    (synopsis "Extensible optionally-pure exceptions")
    (description "This library provides extensible optionally-pure exceptions
for Haskell.")
    (license license:bsd-3)))

(define-public ghc-executable-path
  (package
    (name "ghc-executable-path")
    (version "0.0.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "executable-path/executable-path-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0vxwmnsvx13cawcyhbyljkds0l1vr996ijldycx7nj0asjv45iww"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/executable-path")
    (synopsis "Find out the full path of the executable")
    (description
     "The documentation of @code{System.Environment.getProgName} says that
\"However, this is hard-to-impossible to implement on some non-Unix OSes, so
instead, for maximum portability, we just return the leafname of the program
as invoked.\" This library tries to provide the missing path.")
    (license license:public-domain)))

(define-public ghc-extensible-exceptions
  (package
    (name "ghc-extensible-exceptions")
    (version "0.1.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "extensible-exceptions/extensible-exceptions-"
                           version ".tar.gz"))
       (sha256
        (base32 "1273nqws9ij1rp1bsq5jc7k2jxpqa0svawdbim05lf302y0firbc"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/extensible-exceptions")
    (synopsis "Extensible exceptions for Haskell")
    (description
     "This package provides extensible exceptions for both new and old
versions of GHC (i.e., < 6.10).")
    (license license:bsd-3)))

(define-public ghc-extra
  (package
    (name "ghc-extra")
    (version "1.6.21")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/extra/extra-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1gjx98w4w61g043k6rzc8i34cbxpcigi8lb6i7pp1vwp8w8jm5vl"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-clock" ,ghc-clock)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-quickcheck-instances" ,ghc-quickcheck-instances)))
    (home-page "https://github.com/ndmitchell/extra")
    (synopsis "Extra Haskell functions")
    (description "This library provides extra functions for the standard
Haskell libraries.  Most functions are simple additions, filling out missing
functionality.  A few functions are available in later versions of GHC, but
this package makes them available back to GHC 7.2.")
    (license license:bsd-3)))

(define-public ghc-fail
  (package
    (name "ghc-fail")
    (version "4.9.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/fail/fail-"
                           version ".tar.gz"))
       (sha256
        (base32 "18nlj6xvnggy61gwbyrpmvbdkq928wv0wx2zcsljb52kbhddnp3d"))))
    (build-system haskell-build-system)
    (arguments `(#:haddock? #f)) ; Package contains no documentation.
    (home-page "https://prime.haskell.org/wiki/Libraries/Proposals/MonadFail")
    (synopsis "Forward-compatible MonadFail class")
    (description
     "This package contains the @code{Control.Monad.Fail} module providing the
@uref{https://prime.haskell.org/wiki/Libraries/Proposals/MonadFail, MonadFail}
class that became available in
@uref{https://hackage.haskell.org/package/base-4.9.0.0, base-4.9.0.0} for
older @code{base} package versions.  This package turns into an empty package
when used with GHC versions which already provide the
@code{Control.Monad.Fail} module.")
    (license license:bsd-3)))

(define-public ghc-fast-logger
  (package
    (name "ghc-fast-logger")
    (version "2.4.17")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/fast-logger/fast-logger-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "02mxb1ckvx1s2r2m11l5i2l5rdl7232p0f61af6773haykjp0qxk"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-auto-update" ,ghc-auto-update)
       ("ghc-easy-file" ,ghc-easy-file)
       ("ghc-unix-time" ,ghc-unix-time)
       ("ghc-unix-compat" ,ghc-unix-compat)))
    (native-inputs
     `(("hspec-discover" ,hspec-discover)
       ("ghc-hspec" ,ghc-hspec)))
    (home-page "https://hackage.haskell.org/package/fast-logger")
    (synopsis "Fast logging system")
    (description "This library provides a fast logging system for Haskell.")
    (license license:bsd-3)))

(define-public ghc-feed
  (package
    (name "ghc-feed")
    (version "1.2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "feed/feed-" version ".tar.gz"))
       (sha256
        (base32
         "004lwdng4slj6yl8mgscr3cgj0zzc8hzkf4450dby2l6cardg4w0"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-base-compat" ,ghc-base-compat)
       ("ghc-old-locale" ,ghc-old-locale)
       ("ghc-old-time" ,ghc-old-time)
       ("ghc-safe" ,ghc-safe)
       ("ghc-time-locale-compat" ,ghc-time-locale-compat)
       ("ghc-utf8-string" ,ghc-utf8-string)
       ("ghc-xml-conduit" ,ghc-xml-conduit)
       ("ghc-xml-types" ,ghc-xml-types)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-markdown-unlit" ,ghc-markdown-unlit)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)))
    (home-page "https://github.com/bergmark/feed")
    (synopsis "Haskell package for handling various syndication formats")
    (description "This Haskell package includes tools for generating and
consuming feeds in both RSS (Really Simple Syndication) and Atom format.")
    (license license:bsd-3)))

(define-public ghc-fgl
  (package
    (name "ghc-fgl")
    (version "5.7.0.1")
    (outputs '("out" "static" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/fgl/fgl-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "04793yh778ck3kz1z2svnfdwwls2kisbnky4lzvf4zjfgpv7mkpz"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'update-constraints
           (lambda _
             (substitute* "fgl.cabal"
               (("QuickCheck >= 2\\.8 && < 2\\.13")
                "QuickCheck >= 2.8 && < 2.14")
               (("hspec >= 2\\.1 && < 2\\.7")
                "hspec >= 2.1 && < 2.8")))))))
    (inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://web.engr.oregonstate.edu/~erwig/fgl/haskell")
    (synopsis
     "Martin Erwig's Functional Graph Library")
    (description "The functional graph library, FGL, is a collection of type
and function definitions to address graph problems.  The basis of the library
is an inductive definition of graphs in the style of algebraic data types that
encourages inductive, recursive definitions of graph algorithms.")
    (license license:bsd-3)))

(define-public ghc-fgl-arbitrary
  (package
    (name "ghc-fgl-arbitrary")
    (version "0.2.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/fgl-arbitrary/fgl-arbitrary-"
             version ".tar.gz"))
       (sha256
        (base32
         "0ln1szgfy8fa78l3issq4fx3aqnnd54w3cb4wssrfi48vd5rkfjm"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'update-constraints
           (lambda _
             (substitute* "fgl-arbitrary.cabal"
               (("QuickCheck >= 2\\.3 && < 2\\.10")
                "QuickCheck >= 2.3 && < 2.14")
               (("hspec >= 2\\.1 && < 2\\.5")
                "hspec >= 2.1 && < 2.8")))))))
    (inputs
     `(("ghc-fgl" ,ghc-fgl)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-hspec" ,ghc-hspec)))
    (home-page "https://hackage.haskell.org/package/fgl-arbitrary")
    (synopsis "QuickCheck support for fgl")
    (description
     "Provides Arbitrary instances for fgl graphs to avoid adding a
QuickCheck dependency for fgl whilst still making the instances
available to others.  Also available are non-fgl-specific functions
for generating graph-like data structures.")
    (license license:bsd-3)))

(define-public ghc-file-embed
  (package
    (name "ghc-file-embed")
    (version "0.0.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/file-embed/"
                           "file-embed-" version ".tar.gz"))
       (sha256
        (base32
         "0l6dkwccbzzyx8rcav03lya2334dgi3vfwk96h7l93l0fc4x19gf"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/snoyberg/file-embed")
    (synopsis "Use Template Haskell to embed file contents directly")
    (description
     "This package allows you to use Template Haskell to read a file or all
the files in a directory, and turn them into @code{(path, bytestring)} pairs
embedded in your Haskell code.")
    (license license:bsd-3)))

(define-public ghc-filemanip
  (package
    (name "ghc-filemanip")
    (version "0.3.6.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "filemanip/filemanip-" version ".tar.gz"))
              (sha256
               (base32
                "0ilqr8jv41zxcj5qyicg29m8s30b9v70x6f9h2h2rw5ap8bxldl8"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-unix-compat" ,ghc-unix-compat)))
    (home-page "https://github.com/bos/filemanip")
    (synopsis "File and directory manipulation for Haskell")
    (description
     "This package provides a Haskell library for working with files and
directories.  It includes code for pattern matching, finding files, modifying
file contents, and more.")
    (license license:bsd-3)))

(define-public ghc-filepath-bytestring
  (package
    (name "ghc-filepath-bytestring")
    (version "1.4.2.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
              "https://hackage.haskell.org/package/filepath-bytestring/"
              "filepath-bytestring-" version ".tar.gz"))
       (sha256
        (base32
         "11xrrzdkm5i96dazbz0gi1qp8nnj2lwbnxzwy7f4cnahskz4f4g7"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://hackage.haskell.org/package/filepath-bytestring")
    (synopsis "Library for manipulating RawFilePaths in a cross-platform way")
    (description "This package provides a drop-in replacement for the standard
@code{filepath} library, operating on @code{RawFilePath} values rather than
@code{FilePath} values to get the speed benefits of using @code{ByteStrings}.")
    (license license:bsd-3)))

(define-public ghc-findbin
  (package
    (name "ghc-findbin")
    (version "0.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/FindBin/FindBin-"
             version ".tar.gz"))
       (sha256
        (base32
         "197xvn05yysmibm1p5wzxfa256lvpbknr5d1l2ws6g40w1kpk717"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/audreyt/findbin")
    (synopsis "Get the absolute path of the running program")
    (description
     "This module locates the full directory of the running program, to allow
the use of paths relative to it.  FindBin supports invocation of Haskell
programs via \"ghci\", via \"runhaskell/runghc\", as well as compiled as
an executable.")
    (license license:bsd-3)))

(define-public ghc-fingertree
  (package
    (name "ghc-fingertree")
    (version "0.1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/fingertree/fingertree-"
             version ".tar.gz"))
       (sha256
        (base32
         "0zvandj8fysck7ygpn0dw5bhrhmj1s63i326nalxbfkh2ls4iacm"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)))
    (home-page "https://hackage.haskell.org/package/fingertree")
    (synopsis "Generic finger-tree structure")
    (description "This library provides finger trees, a general sequence
representation with arbitrary annotations, for use as a base for
implementations of various collection types.  It includes examples, as
described in section 4 of Ralf Hinze and Ross Paterson, \"Finger trees: a
simple general-purpose data structure\".")
    (license license:bsd-3)))

(define-public ghc-fixed
  (package
    (name "ghc-fixed")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/fixed/fixed-"
                           version ".tar.gz"))
       (sha256
        (base32
         "10l2sh179xarb774q92cff2gkb20rsrlilfwp1fk61rzmz9yn64j"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/ekmett/fixed")
    (synopsis "Signed 15.16 precision fixed point arithmetic")
    (description
     "This package provides functions for signed 15.16 precision fixed point
arithmetic.")
    (license license:bsd-3)))

(define-public ghc-fmlist
  (package
    (name "ghc-fmlist")
    (version "0.9.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://hackage.haskell.org/package/fmlist/fmlist-"
         version ".tar.gz"))
       (sha256
        (base32
         "1w9nhm2zybdx4c1lalkajwqr8wcs731lfjld2r8gknd7y96x8pwf"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/sjoerdvisscher/fmlist")
    (synopsis "FoldMap lists")
    (description "FoldMap lists are lists represented by their
@code{foldMap} function.  FoldMap lists have @math{O(1)} cons, snoc and
append, just like DLists, but other operations might have favorable
performance characteristics as well.  These wild claims are still
completely unverified though.")
    (license license:bsd-3)))

(define-public ghc-foldl
  (package
    (name "ghc-foldl")
    (version "1.4.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "foldl-" version "/"
                           "foldl-" version ".tar.gz"))
       (sha256
        (base32
         "19qjmzc7gaxfwgqbgy0kq4vhbxvh3qjnwsxnc7pzwws2if5bv80b"))))
    (build-system haskell-build-system)
    (outputs '("out" "static" "doc"))
    (inputs `(("ghc-mwc-randam" ,ghc-mwc-random)
              ("ghc-primitive" ,ghc-primitive)
              ("ghc-vector" ,ghc-vector)
              ("ghc-unordered-containers" ,ghc-unordered-containers)
              ("ghc-hashable" ,ghc-hashable)
              ("ghc-contravariant" ,ghc-contravariant)
              ("ghc-semigroups" ,ghc-semigroups)
              ("ghc-profunctors" ,ghc-profunctors)
              ("ghc-semigroupoids" ,ghc-semigroupoids)
              ("ghc-comonad" ,ghc-comonad)
              ("ghc-vector-builder" ,ghc-vector-builder)))
    (home-page "https://github.com/Gabriel439/Haskell-Foldl-Library")
    (synopsis "Composable, streaming, and efficient left folds for Haskell")
    (description "This Haskell library provides strict left folds that stream
in constant memory, and you can combine folds using @code{Applicative} style
to derive new folds.  Derived folds still traverse the container just once
and are often as efficient as hand-written folds.")
    (license license:bsd-3)))

(define-public ghc-foundation
  (package
    (name "ghc-foundation")
    (version "0.0.25")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "foundation/foundation-" version ".tar.gz"))
       (sha256
        (base32
         "0q6kx57ygmznlpf8n499hid4x6mj3180paijx0a8dgi9hh7man61"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; This test is broken.  For details, see
         ;; https://github.com/haskell-foundation/foundation/issues/530
         (add-after 'unpack 'patch-tests
           (lambda _
             (substitute* "tests/Test/Foundation/Number.hs"
               ((", testDividible proxy") ""))
             #t)))))
    (outputs '("out" "static" "doc"))
    (inputs `(("ghc-basement" ,ghc-basement)))
    (home-page "https://github.com/haskell-foundation/foundation")
    (synopsis "Alternative prelude with batteries and no dependencies")
    (description
     "This package provides a custom prelude with no dependencies apart from
the base package.

Foundation has the following goals:

@enumerate
@item provide a base like sets of modules that provide a consistent set of
   features and bugfixes across multiple versions of GHC (unlike base).
@item provide a better and more efficient prelude than base's prelude.
@item be self-sufficient: no external dependencies apart from base;
@item provide better data-types: packed unicode string by default, arrays;
@item Numerical classes that better represent mathematical things (no more
   all-in-one @code{Num});
@item I/O system with less lazy IO.
@end enumerate\n")
    (license license:bsd-3)))

(define-public ghc-free
  (package
    (name "ghc-free")
    (version "5.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/free/free-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0vlf3f2ckl3cr7z2zl8c9c8qkdlfgvmh04gxkp2fg0z9dz80nlyb"))))
    (build-system haskell-build-system)
    (outputs '("out" "static" "doc"))
    (inputs
     `(("ghc-prelude-extras" ,ghc-prelude-extras)
       ("ghc-profunctors" ,ghc-profunctors)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-bifunctors" ,ghc-bifunctors)
       ("ghc-comonad" ,ghc-comonad)
       ("ghc-distributive" ,ghc-distributive)
       ("ghc-semigroupoids" ,ghc-semigroupoids)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-transformers-base" ,ghc-transformers-base)
       ("ghc-transformers-compat" ,ghc-transformers-compat)))
    (home-page "https://github.com/ekmett/free/")
    (synopsis "Unrestricted monads for Haskell")
    (description "This library provides free monads, which are useful for many
tree-like structures and domain specific languages.  If @code{f} is a
@code{Functor} then the free @code{Monad} on @code{f} is the type of trees
whose nodes are labeled with the constructors of @code{f}.  The word \"free\"
is used in the sense of \"unrestricted\" rather than \"zero-cost\": @code{Free
f} makes no constraining assumptions beyond those given by @code{f} and the
definition of @code{Monad}.")
    (license license:bsd-3)))

(define-public ghc-fsnotify
  (package
    (name "ghc-fsnotify")
    (version "0.3.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://hackage.haskell.org/package/fsnotify/"
                    "fsnotify-" version ".tar.gz"))
              (sha256
               (base32
                "19bdbz9wb9jvln6yg6qm0hz0w84bypvkxf0wjhgrgd52f9gidlny"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-async" ,ghc-async)
       ("ghc-unix-compat" ,ghc-unix-compat)
       ("ghc-hinotify" ,ghc-hinotify)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-random" ,ghc-random)
       ("ghc-shelly" ,ghc-shelly)
       ("ghc-temporary" ,ghc-temporary)))
    (home-page "https://github.com/haskell-fswatch/hfsnotify")
    (synopsis "Cross platform library for file change notification.")
    (description "Cross platform library for file creation, modification, and
deletion notification. This library builds upon existing libraries for platform
specific Windows, Mac, and Linux file system event notification.")
    (license license:bsd-3)))

(define-public ghc-generic-deriving
  (package
    (name "ghc-generic-deriving")
    (version "1.12.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/generic-deriving/generic-deriving-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0vdg9qdq35jl3m11a87wk8cq1y71qm4i1g1b2pxki0wk70yw20a4"))))
    (build-system haskell-build-system)
    (outputs '("out" "static" "doc"))
    (inputs
     `(("ghc-th-abstraction" ,ghc-th-abstraction)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://hackage.haskell.org/package/generic-deriving")
    (synopsis "Generalise the deriving mechanism to arbitrary classes")
    (description "This package provides functionality for generalising the
deriving mechanism in Haskell to arbitrary classes.")
    (license license:bsd-3)))

(define-public ghc-generic-random
  (package
    (name "ghc-generic-random")
    (version "1.2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/generic-random/"
             "generic-random-" version ".tar.gz"))
       (sha256
        (base32 "130lmblycxnpqbsl7vf6a90zccibnvcb5zaclfajcn3by39007lv"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-quickcheck" ,ghc-quickcheck)))
    (native-inputs
     `(("ghc-inspection-testing" ,ghc-inspection-testing)))
    (arguments
     `(#:cabal-revision
       ("1" "1d0hx41r7yq2a86ydnfh2fv540ah8cz05l071s2z4wxcjw0ymyn4")))
    (home-page
     "https://github.com/lysxia/generic-random")
    (synopsis
     "Generic random generators for QuickCheck")
    (description
     "Derive instances of @code{Arbitrary} for QuickCheck, with various options
to customize implementations.

Automating the arbitrary boilerplate also ensures that when a type changes to
have more or fewer constructors, then the generator either fixes itself to
generate that new case (when using the uniform distribution) or causes a
compilation error so you remember to fix it (when using an explicit
distribution).

This package also offers a simple (optional) strategy to ensure termination
for recursive types: make @code{Test.QuickCheck.Gen}'s size parameter decrease
at every recursive call; when it reaches zero, sample directly from a
trivially terminating generator given explicitly (@code{genericArbitraryRec}
and @code{withBaseCase}) or implicitly (@code{genericArbitrary'}).")
    (license license:expat)))

(define-public ghc-generic-random-1.3.0.1
  (package
    (inherit ghc-generic-random)
    (version "1.3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/generic-random/"
             "generic-random-" version ".tar.gz"))
       (sha256
        (base32 "0d9w7xcmsb31b95fr9d5jwbsajcl1yi4347dlbw4bybil2vjwd7k"))))
    (arguments '())))

(define-public ghc-generics-sop
  (package
    (name "ghc-generics-sop")
    (version "0.4.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "generics-sop-" version "/"
                           "generics-sop-" version ".tar.gz"))
       (sha256
        (base32
         "160knr2phnzh2gldfv954lz029jzc7y8kz5xpmbf4z3vb5ngm6fw"))))
    (build-system haskell-build-system)
    (outputs '("out" "static" "doc"))
    (inputs
     `(("ghc-sop-core" ,ghc-sop-core)
       ("ghc-transformers-compat" ,ghc-transformers-compat)))
    (home-page "https://github.com/well-typed/generics-sop")
    (synopsis "Generic Programming using True Sums of Products for Haskell")
    (description "This Haskell package supports the definition of generic
functions.  Datatypes are viewed in a uniform, structured way: the choice
between constructors is represented using an n-ary sum, and the arguments of
each constructor are represented using an n-ary product.")
    (license license:bsd-3)))

(define-public ghc-geniplate-mirror
  (package
    (name "ghc-geniplate-mirror")
    (version "0.7.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package"
                           "/geniplate-mirror"
                           "/geniplate-mirror-" version ".tar.gz"))
       (sha256
        (base32 "1y0m0bw5zpm1y1y6d9qmxj3swl8j8hlw1shxbr5awycf6k884ssb"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("2" "03fg4vfm1wgq4mylggawdx0bfvbbjmdn700sqx7v3hk1bx0kjfzh")))
    (home-page "https://github.com/danr/geniplate")
    (synopsis "Use Template Haskell to generate Uniplate-like functions")
    (description
     "Use Template Haskell to generate Uniplate-like functions.  This is a
maintained mirror of the @uref{https://hackage.haskell.org/package/geniplate,
geniplate} package, written by Lennart Augustsson.")
    (license license:bsd-3)))

(define-public ghc-genvalidity
  (package
    (name "ghc-genvalidity")
    (version "0.8.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/genvalidity/genvalidity-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0w38aq9hfyymidncgkrs6yvja7j573d9sap5qfg5rz910fhsij9a"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-validity" ,ghc-validity)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)
       ("ghc-hspec-core" ,ghc-hspec-core)))
    (home-page
     "https://github.com/NorfairKing/validity")
    (synopsis
     "Testing utilities for the @code{validity} library")
    (description
     "This package provides testing utilities that are useful in conjunction
with the @code{Validity} typeclass.")
    (license license:expat)))

(define-public ghc-genvalidity-property
  (package
    (name "ghc-genvalidity-property")
    (version "0.4.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/"
             "genvalidity-property/genvalidity-property-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0zayycx62226w54rvkxwhvqhznsr33dk3ds55yyqrfqbnhvph1s9"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-genvalidity" ,ghc-genvalidity)
       ("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)
       ("ghc-validity" ,ghc-validity)))
    (native-inputs `(("ghc-doctest" ,ghc-doctest)))
    (home-page
     "https://github.com/NorfairKing/validity")
    (synopsis
     "Standard properties for functions on @code{Validity} types")
    (description
     "This package supplements the @code{Validity} typeclass with standard
properties for functions operating on them.")
    (license license:expat)))

(define-public ghc-getopt-generics
  (package
    (name "ghc-getopt-generics")
    (version "0.13.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "getopt-generics/getopt-generics-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1rszkcn1rg38wf35538ljk5bbqjc57y9sb3a0al7qxm82gy8yigr"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-base-compat" ,ghc-base-compat)
       ("ghc-base-orphans" ,ghc-base-orphans)
       ("ghc-generics-sop" ,ghc-generics-sop)
       ("ghc-tagged" ,ghc-tagged)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-safe" ,ghc-safe)
       ("ghc-silently" ,ghc-silently)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://github.com/soenkehahn/getopt-generics")
    (synopsis "Create command line interfaces with ease")
    (description "This library provides tools to create command line
interfaces with ease.")
    (license license:bsd-3)))

(define-public ghc-gitrev
  (package
    (name "ghc-gitrev")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/gitrev/gitrev-"
                           version ".tar.gz"))
       (sha256
        (base32 "0cl3lfm6k1h8fxp2vxa6ihfp4v8igkz9h35iwyq2frzm4kdn96d8"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-base-compat" ,ghc-base-compat)))
    (home-page "https://github.com/acfoltzer/gitrev")
    (synopsis "Compile git revision info into Haskell projects")
    (description
     "This package provides some handy Template Haskell splices for including
the current git hash and branch in the code of your project.  This is useful
for including in panic messages, @command{--version} output, or diagnostic
info for more informative bug reports.")
    (license license:bsd-3)))

(define-public ghc-glob
  (package
    (name "ghc-glob")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "Glob-" version "/"
                           "Glob-" version ".tar.gz"))
       (sha256
        (base32
         "0953f91f62ncna402vsrfzdcyxhdpjna3bgdw017kad0dfymacs7"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-dlist" ,ghc-dlist)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-transformers-compat" ,ghc-transformers-compat)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)))
    (home-page "http://iki.fi/matti.niemenmaa/glob/")
    (synopsis "Haskell library matching glob patterns against file paths")
    (description "This package provides a Haskell library for @dfn{globbing}:
matching patterns against file paths.")
    (license license:bsd-3)))

(define-public ghc-gluraw
  (package
    (name "ghc-gluraw")
    (version "2.0.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/GLURaw/GLURaw-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1i2xi35n5z0d372px9mh6cyhgg1m0cfaiy3fnspkf6kbn9fgsqxq"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-openglraw" ,ghc-openglraw)))
    (home-page "https://wiki.haskell.org/Opengl")
    (synopsis "Raw Haskell bindings GLU")
    (description "GLURaw is a raw Haskell binding for the GLU 1.3 OpenGL
utility library.  It is basically a 1:1 mapping of GLU's C API, intended as a
basis for a nicer interface.")
    (license license:bsd-3)))

(define-public ghc-glut
  (package
    (name "ghc-glut")
    (version "2.7.0.15")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/GLUT/GLUT-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0271vnf6wllhxjwy0m348x90kv27aybxcbqkkglmd5w4cpwjg5g9"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-statevar" ,ghc-statevar)
       ("ghc-opengl" ,ghc-opengl)
       ("ghc-openglraw" ,ghc-openglraw)
       ("freeglut" ,freeglut)))
    (home-page "https://wiki.haskell.org/Opengl")
    (synopsis "Haskell bindings for the OpenGL Utility Toolkit")
    (description "This library provides Haskell bindings for the OpenGL
Utility Toolkit, a window system-independent toolkit for writing OpenGL
programs.")
    (license license:bsd-3)))

(define-public ghc-gnuplot
  (package
    (name "ghc-gnuplot")
    (version "0.5.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://hackage/package/gnuplot/gnuplot-"
             version ".tar.gz"))
       (sha256
        (base32 "1g6xgnlkh17avivn1rlq7l2nvs26dvrbx4rkfld0bf6kyqaqwrgp"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-temporary" ,ghc-temporary)
       ("ghc-utility-ht" ,ghc-utility-ht)
       ("ghc-data-accessor-transformers" ,ghc-data-accessor-transformers)
       ("ghc-data-accessor" ,ghc-data-accessor)
       ("ghc-semigroups" ,ghc-semigroups)
       ("gnuplot" ,gnuplot)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-path-to-gnuplot
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gnuplot (assoc-ref inputs "gnuplot")))
               (substitute* "os/generic/Graphics/Gnuplot/Private/OS.hs"
                 (("(gnuplotName = ).*$" all cmd)
                  (string-append cmd "\"" gnuplot "/bin/gnuplot\"")))))))))
    (home-page "https://wiki.haskell.org/Gnuplot")
    (synopsis "2D and 3D plots using gnuplot")
    (description "This package provides a Haskell module for creating 2D and
3D plots using gnuplot.")
    (license license:bsd-3)))

(define-public ghc-graphviz
  (package
    (name "ghc-graphviz")
    (version "2999.20.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "graphviz/graphviz-" version ".tar.gz"))
              (sha256
               (base32
                "04k26zw61nfv1pkd00iaq89pgsaiym0sf4cbzkmm2k2fj5xa587g"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'update-constraints
           (lambda _
             (substitute* "graphviz.cabal"
               (("QuickCheck >= 2\\.3 && < 2\\.13")
                "QuickCheck >= 2.3 && < 2.14")
               (("hspec >= 2\\.1 && < 2\\.7")
                "hspec >= 2.1 && < 2.8")))))))
    (inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-colour" ,ghc-colour)
       ("ghc-dlist" ,ghc-dlist)
       ("ghc-fgl" ,ghc-fgl)
       ("ghc-fgl-arbitrary" ,ghc-fgl-arbitrary)
       ("ghc-polyparse" ,ghc-polyparse)
       ("ghc-temporary" ,ghc-temporary)
       ("ghc-wl-pprint-text" ,ghc-wl-pprint-text)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("graphviz" ,graphviz)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://hackage.haskell.org/package/graphviz")
    (synopsis "Bindings to Graphviz for graph visualisation")
    (description
     "This library provides bindings for the Dot language used by
the @uref{https://graphviz.org/, Graphviz} suite of programs for
visualising graphs, as well as functions to call those programs.
Main features of the graphviz library include:

@enumerate
@item Almost complete coverage of all Graphviz attributes and syntax
@item Support for specifying clusters
@item The ability to use a custom node type
@item Functions for running a Graphviz layout tool with all specified output types
@item Generate and parse Dot code with two options: strict and liberal
@item Functions to convert FGL graphs and other graph-like data structures
@item Round-trip support for passing an FGL graph through Graphviz to augment node
and edge labels with positional information, etc.
@end enumerate\n")
    (license license:bsd-3)))

(define-public ghc-groups
  (package
    (name "ghc-groups")
    (version "0.4.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://hackage.haskell.org/package/"
                            "groups/groups-" version ".tar.gz"))
        (sha256
          (base32
            "0ggkygkyxw5ga4cza82bjvdraavl294k0h6b62d2px7z3nvqhifx"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/groups")
    (synopsis "Haskell 98 groups")
    (description "This package provides Haskell 98 groups.  A group is a
monoid with invertibility.")
    (license license:bsd-3)))

(define-public ghc-gtk2hs-buildtools
  (package
    (name "ghc-gtk2hs-buildtools")
    (version "0.13.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "gtk2hs-buildtools/gtk2hs-buildtools-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1flxsacxwmabzzalhn8558kmj95z01c0lmikrn56nxh7p62nxm25"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-random" ,ghc-random)
       ("ghc-hashtables" ,ghc-hashtables)))
    (native-inputs
     `(("ghc-alex" ,ghc-alex)
       ("ghc-happy" ,ghc-happy)))
    (home-page "http://projects.haskell.org/gtk2hs/")
    (synopsis "Tools to build the Gtk2Hs suite of user interface libraries")
    (description
     "This package provides a set of helper programs necessary to build the
Gtk2Hs suite of libraries. These tools include a modified c2hs binding tool
that is used to generate FFI declarations, a tool to build a type hierarchy
that mirrors the C type hierarchy of GObjects found in glib, and a generator
for signal declarations that are used to call back from C to Haskell. These
tools are not needed to actually run Gtk2Hs programs.")
    (license license:gpl2)))

(define-public ghc-hackage-security
  (package
    (name "ghc-hackage-security")
    (version "0.5.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "hackage-security/hackage-security-"
                           version ".tar.gz"))
       (sha256
        (base32
         "08bwawc7ramgdh54vcly2m9pvfchp0ahhs8117jajni6x4bnx66v"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("6" "1xs2nkzlvkdz8g27yzfxbjdbdadfmgiydnlpn5dm77cg18r495ay")
       #:tests? #f)) ; Tests fail because of framework updates.
    (inputs
     `(("ghc-base16-bytestring" ,ghc-base16-bytestring)
       ("ghc-base64-bytestring" ,ghc-base64-bytestring)
       ("ghc-cryptohash-sha256" ,ghc-cryptohash-sha256)
       ("ghc-ed25519" ,ghc-ed25519)
       ("ghc-network" ,ghc-network)
       ("ghc-network-uri" ,ghc-network-uri)
       ("ghc-tar" ,ghc-tar)
       ("ghc-zlib" ,ghc-zlib)))
    (native-inputs
     `(("ghc-network-uri" ,ghc-network-uri)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-tar" ,ghc-tar)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
       ("ghc-temporary" ,ghc-temporary)
       ("ghc-zlib" ,ghc-zlib)))
    (home-page "https://github.com/haskell/hackage-security")
    (synopsis "Hackage security library")
    (description "This Hackage security library provides both server and
client utilities for securing @uref{http://hackage.haskell.org/, the
Hackage package server}.  It is based on
@uref{http://theupdateframework.com/, The Update Framework}, a set of
recommendations developed by security researchers at various universities
in the US as well as developers on the @uref{https://www.torproject.org/,
Tor project}.")
    (license license:bsd-3)))

(define-public ghc-haddock
  (package
    (name "ghc-haddock")
    (version "2.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/haddock/haddock-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1k42z2zh550rl93c8pa9cg2xsanp6wvb031xvan6cmngnplmdib6"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; The release tarball for 2.22.0 is missing the test data for
         ;; the Hoogle test, causing it to fail.  This is fixed in the
         ;; next release, but for now we disable it.
         (add-before 'configure 'remove-hoogle-test
           (lambda _
             (use-modules (ice-9 rdelim))
             (with-atomic-file-replacement "haddock.cabal"
               (lambda (in out)
                 (let loop ((line (read-line in 'concat)) (deleting? #f))
                   (cond
                    ((eof-object? line) #t)
                    ((string-every char-set:whitespace line)
                     (unless deleting? (display line out))
                     (loop (read-line in 'concat) #f))
                    ((string=? line "test-suite hoogle-test\n")
                     (loop (read-line in 'concat) #t))
                    (else
                     (unless deleting? (display line out))
                     (loop (read-line in 'concat) deleting?))))))))
         (add-before 'check 'add-haddock-to-path
           (lambda _
             (setenv "PATH" (string-append (getcwd) "/dist/build/haddock"
                                           ":" (getenv "PATH")))
             #t)))))
    (inputs `(("ghc-haddock-api" ,ghc-haddock-api)))
    (native-inputs
     `(("ghc-haddock-test" ,ghc-haddock-test)
       ("ghc-hspec" ,ghc-hspec)))
    (home-page "https://www.haskell.org/haddock/")
    (synopsis
     "Documentation-generation tool for Haskell libraries")
    (description
     "Haddock is a documentation-generation tool for Haskell libraries.")
    (license license:bsd-3)))

(define-public ghc-haddock-api
  (package
    (name "ghc-haddock-api")
    (version "2.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/haddock-api/haddock-api-"
             version
             ".tar.gz"))
       (patches (search-patches "ghc-haddock-api-fix-haddock.patch"))
       (sha256
        (base32
         "149q4zlf4m7wcrr4af2n2flh0jxjsypshbc229vsj1m0kmz4z014"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'update-constraints
           (lambda _
             (substitute* "haddock-api.cabal"
               (("QuickCheck      \\^>= 2\\.11")
                "QuickCheck      ^>= 2.13")
               (("hspec           >= 2\\.4\\.4 && < 2\\.6")
                "hspec           >= 2.4.4 && < 2.8")))))))
    (inputs
     `(("ghc-paths" ,ghc-paths)
       ("ghc-haddock-library" ,ghc-haddock-library)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://www.haskell.org/haddock/")
    (synopsis "API for documentation-generation tool Haddock")
    (description "This package provides an API to Haddock, the
documentation-generation tool for Haskell libraries.")
    (license license:bsd-3)))

(define-public ghc-haddock-library
  (package
    (name "ghc-haddock-library")
    (version "1.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/haddock-library/haddock-library-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "04fhcjk0pvsaqvsgp2w06cv2qvshq1xs1bwc157q4lmkgr57khp7"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Since there is no revised Cabal file upstream, we have to
         ;; patch it manually.
         (add-before 'configure 'relax-test-suite-dependencies
           (lambda _
             (substitute* "haddock-library.cabal"
               (("hspec\\s*>= 2.4.4   && < 2.6") "hspec")
               (("QuickCheck\\s*\\^>= 2.11") "QuickCheck"))
             #t)))))
    (native-inputs
     `(("ghc-base-compat" ,ghc-base-compat)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-optparse-applicative" ,ghc-optparse-applicative)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-tree-diff" ,ghc-tree-diff)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://www.haskell.org/haddock/")
    (synopsis "Library exposing some functionality of Haddock")
    (description
     "Haddock is a documentation-generation tool for Haskell libraries.  These
modules expose some functionality of it without pulling in the GHC dependency.
Please note that the API is likely to change so specify upper bounds in your
project if you can't release often.  For interacting with Haddock itself, see
the ‘haddock’ package.")
    (license license:bsd-3)))

;; This package is needed for testing 'ghc-haddock'.  It is no longer
;; published to Hackage, but it is maintained in the Haddock Git
;; repository.
(define ghc-haddock-test
  (package
    (name "ghc-haddock-test")
    (version "2.22.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/haskell/haddock")
             (commit (string-append "haddock-" version "-release"))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1ywxmqqan10gs0ppybdmdgsmvkzkpw7yirj2rw4qylg3x49a9zca"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'change-directory
           (lambda _
             (chdir "haddock-test"))))))
    (inputs
     `(("ghc-syb" ,ghc-syb)
       ("ghc-xml" ,ghc-xml)))
    (home-page "https://www.haskell.org/haddock/")
    (synopsis "Test utilities for Haddock")
    (description "This package provides test utilities for Haddock.")
    (license license:bsd-3)
    (properties '((hidden? #t)))))

(define-public ghc-half
  (package
    (name "ghc-half")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/half/half-"
             version ".tar.gz"))
       (sha256
        (base32
         "14r0nx8hm5fic9gz0ybjjw4kyc758zfgvhhwvzsshpx5caq6zch6"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://github.com/ekmett/half")
    (synopsis "Half-precision floating-point computations")
    (description "This library provides a half-precision floating-point
computation library for Haskell.")
    (license license:bsd-3)))

(define-public ghc-happy
  (package
    (name "ghc-happy")
    (version "1.19.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/happy/happy-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "03xlmq6qmdx4zvzw8bp33kd9g7yvcq5cz4wg50xilw812kj276pv"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-test-issue93
           (lambda _
             ;; Tests run out of memory on a system with 2GB of available RAM,
             ;; in 'issue93.a.hs' and 'issue93.n.hs'.
             (substitute* "tests/Makefile"
               ((" issue93.y ") " "))
             #t)))))
    (home-page "https://hackage.haskell.org/package/happy")
    (synopsis "Parser generator for Haskell")
    (description "Happy is a parser generator for Haskell.  Given a grammar
specification in BNF, Happy generates Haskell code to parse the grammar.
Happy works in a similar way to the yacc tool for C.")
    (license license:bsd-3)))

(define-public ghc-hashable
  (package
    (name "ghc-hashable")
    (version "1.2.7.0")
    (outputs '("out" "static" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/hashable/hashable-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1gra8gq3kb7b2sd845h55yxlrfqx3ii004c6vjhga8v0b30fzdgc"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("1" "197063dpl0wn67dp7a06yc2hxp81n24ykk7klbjx0fndm5n87dh3")))
    (inputs
     `(("ghc-random" ,ghc-random)))
    (native-inputs
     `(("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://github.com/tibbe/hashable")
    (synopsis "Class for types that can be converted to a hash value")
    (description
     "This package defines a class, @code{Hashable}, for types that can be
converted to a hash value.  This class exists for the benefit of hashing-based
data structures.  The package provides instances for basic types and a way to
combine hash values.")
    (license license:bsd-3)))

(define-public ghc-hashable-bootstrap
  (package
    (inherit ghc-hashable)
    (name "ghc-hashable-bootstrap")
    (arguments
     `(#:tests? #f
       ,@(package-arguments ghc-hashable)))
    (native-inputs '())
    (properties '((hidden? #t)))))

(define-public ghc-hashable-time
  (package
    (name "ghc-hashable-time")
    (version "0.2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/hashable-time/hashable-time-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1q7y4plqqwy5286hhx2fygn12h8lqk0y047b597sbdckskxzfqgs"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("2" "006phc5y9rrvsshdcmjmhxzxh8dpgs685mpqbkjm9c40xb1ydjbz")))
    (inputs `(("ghc-hashable" ,ghc-hashable)))
    (home-page "https://hackage.haskell.org/package/hashable-time")
    (synopsis "Hashable instances for Data.Time")
    (description
     "This package provides @code{Hashable} instances for types in
@code{Data.Time}.")
    (license license:bsd-3)))

(define-public ghc-hashtables
  (package
    (name "ghc-hashtables")
    (version "1.2.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/hashtables/hashtables-"
             version ".tar.gz"))
       (sha256
        (base32 "1rjmxnr30g4hygiywkpz5p9sanh0abs7ap4zc1kgd8zv04kycp0j"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-hashable" ,ghc-hashable)
       ("ghc-primitive" ,ghc-primitive)
       ("ghc-vector" ,ghc-vector)))
    (home-page "https://github.com/gregorycollins/hashtables")
    (synopsis "Haskell Mutable hash tables in the ST monad")
    (description "This package provides a Haskell library including a
couple of different implementations of mutable hash tables in the ST
monad, as well as a typeclass abstracting their common operations, and
a set of wrappers to use the hash tables in the IO monad.")
    (license license:bsd-3)))

(define-public ghc-haskeline-0.8
  (package
    (name "ghc-haskeline")
    (version "0.8.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/haskeline/haskeline-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0gqsa5s0drim9m42hv4wrq61mnvcdylxysfxfw3acncwilfrn9pb"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-exceptions" ,ghc-exceptions)))
    (native-inputs `(("ghc-hunit" ,ghc-hunit)))
    ;; FIXME: Tests failing
    (arguments `(#:tests? #f))
    (home-page "https://github.com/judah/haskeline")
    (synopsis
     "Command-line interface for user input, written in Haskell")
    (description
     "Haskeline provides a user interface for line input in command-line
programs.  This library is similar in purpose to readline, but since it is
written in Haskell it is (hopefully) more easily used in other Haskell
programs.

Haskeline runs both on POSIX-compatible systems and on Windows.")
    (license license:bsd-3)))

(define-public ghc-haskell-lexer
  (package
    (name "ghc-haskell-lexer")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/haskell-lexer/haskell-lexer-"
             version ".tar.gz"))
       (sha256
        (base32 "1wyxd8x33x4v5vxyzkhm610pl86gbkc8y439092fr1735q9g7kfq"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/haskell-lexer")
    (synopsis "Fully compliant Haskell 98 lexer")
    (description
     "This package provides a fully compliant Haskell 98 lexer.")
    (license license:bsd-3)))

(define-public ghc-haskell-src
  (package
    (name "ghc-haskell-src")
    (version "1.0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/haskell-src/haskell-src-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1g4dj1f0j68bhn4ixfac63wjzy6gsp6kwgxryb1k5nl3i0g99d5l"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-happy" ,ghc-happy)
       ("ghc-syb" ,ghc-syb)))
    (home-page
     "https://hackage.haskell.org/package/haskell-src")
    (synopsis
     "Support for manipulating Haskell source code")
    (description
     "The @code{haskell-src} package provides support for manipulating Haskell
source code.  The package provides a lexer, parser and pretty-printer, and a
definition of a Haskell abstract syntax tree (AST).  Common uses of this
package are to parse or generate Haskell 98 code.")
    (license license:bsd-3)))

(define-public ghc-haskell-src-exts
  (package
    (name "ghc-haskell-src-exts")
    (version "1.21.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/haskell-src-exts/haskell-src-exts-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0q1y8n3d82gid9bcx8wxsqqmj9mq11fg3gp5yzpfbw958dhi3j9f"))))
    (build-system haskell-build-system)
    (outputs '("out" "static" "doc"))
    (inputs
     `(("cpphs" ,cpphs)
       ("ghc-happy" ,ghc-happy)
       ("ghc-pretty-show" ,ghc-pretty-show)))
    (native-inputs
     `(("ghc-smallcheck" ,ghc-smallcheck)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-smallcheck" ,ghc-tasty-smallcheck)
       ("ghc-tasty-golden" ,ghc-tasty-golden)))
    (home-page "https://github.com/haskell-suite/haskell-src-exts")
    (synopsis "Library for manipulating Haskell source")
    (description "Haskell-Source with Extensions (HSE, haskell-src-exts) is an
extension of the standard @code{haskell-src} package, and handles most
registered syntactic extensions to Haskell.  All extensions implemented in GHC
are supported.  Apart from these standard extensions, it also handles regular
patterns as per the HaRP extension as well as HSX-style embedded XML syntax.")
    (license license:bsd-3)))

(define-public ghc-haskell-src-exts-util
  (package
    (name "ghc-haskell-src-exts-util")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "haskell-src-exts-util/haskell-src-exts-util-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0fvqi72m74p7q5sbpy8m2chm8a1lgy10mfrcxcz8wrh59vngj0n8"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-data-default" ,ghc-data-default)
       ("ghc-haskell-src-exts" ,ghc-haskell-src-exts)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-uniplate" ,ghc-uniplate)))
    (home-page "https://github.com/pepeiborra/haskell-src-exts-util")
    (synopsis "Helper functions for working with haskell-src-exts trees")
    (description
     "This package provides helper functions for working with
@code{haskell-src-exts} trees.")
    (license license:bsd-3)))

(define-public ghc-haskell-src-meta
  (package
    (name "ghc-haskell-src-meta")
    (version "0.8.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "haskell-src-meta/haskell-src-meta-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "17znnaqj2hnnfyc9p9xjzbs97h2jh1h4f4qbw648y3xa14wx5ra9"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-haskell-src-exts" ,ghc-haskell-src-exts)
       ("ghc-syb" ,ghc-syb)
       ("ghc-th-orphans" ,ghc-th-orphans)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)))
    (home-page "https://hackage.haskell.org/package/haskell-src-meta")
    (synopsis "Parse source to template-haskell abstract syntax")
    (description
     "This package provides tools to parse Haskell sources to the
template-haskell abstract syntax.")
    (license license:bsd-3)))

(define-public ghc-hasktags
  (package
    (name "ghc-hasktags")
    (version "0.71.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/hasktags/hasktags-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1s2k9qrgy1jily96img2pmn7g35mwnnfiw6si3aw32jfhg5zsh1c"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-system-filepath" ,ghc-system-filepath)
       ("ghc-optparse-applicative" ,ghc-optparse-applicative)))
    (native-inputs
     `(("ghc-json" ,ghc-json)
       ("ghc-utf8-string" ,ghc-utf8-string)
       ("ghc-microlens-platform" ,ghc-microlens-platform)
       ("ghc-hunit" ,ghc-hunit)))
    (home-page "https://github.com/MarcWeber/hasktags")
    (synopsis "Make @code{Ctags} and @code{Etags} files for Haskell programs")
    (description
     "This package provides a means of generating tag files for Emacs and
Vim.")
    (license license:bsd-3)))

(define-public ghc-hex
  (package
    (name "ghc-hex")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "hex-" version "/"
                           "hex-" version ".tar.gz"))
       (sha256
        (base32
         "1v31xiaivrrn0q2jz8919wvkjplv1kxna5ajhsj701fqxm1i5vhj"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/hex")
    (synopsis "Convert strings into hexadecimal and back")
    (description "This package converts between bytestrings and their
hexadecimal string representation.")
    (license license:bsd-3)))

(define-public ghc-highlighting-kate
  (package
    (name "ghc-highlighting-kate")
    (version "0.6.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "highlighting-kate/highlighting-kate-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1bqv00gfmrsf0jjr4qf3lhshvfkyzmhbi3pjb6mafbnsyn2k7f6q"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-diff" ,ghc-diff)
       ("ghc-regex-pcre-builtin" ,ghc-regex-pcre-builtin)))
    (native-inputs
     `(("ghc-blaze-html" ,ghc-blaze-html)
       ("ghc-utf8-string" ,ghc-utf8-string)))
    (home-page "https://github.com/jgm/highlighting-kate")
    (synopsis "Syntax highlighting library")
    (description
     "Highlighting-kate is a syntax highlighting library with support for
nearly one hundred languages.  The syntax parsers are automatically generated
from @uref{https://kate-editor.org/, Kate syntax descriptions}, so any syntax
supported by Kate can be added.  An (optional) command-line program is
provided, along with a utility for generating new parsers from Kate XML syntax
descriptions.")
    (license license:gpl2+)))

(define-public ghc-hindent
  (package
    (name "ghc-hindent")
    (version "5.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/hindent/hindent-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "008s8zm9qs972b7v5kkmr8l3i9kc6zm7yj33mkw6dv69b7h3c01l"))))
    (build-system haskell-build-system)
    (arguments
     `(#:modules ((guix build haskell-build-system)
                  (guix build utils)
                  (guix build emacs-utils))
       #:imported-modules (,@%haskell-build-system-modules
                           (guix build emacs-utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'emacs-install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (elisp-file "elisp/hindent.el")
                    (dest  (string-append out "/share/emacs/site-lisp"))
                    (emacs (string-append (assoc-ref inputs "emacs")
                                          "/bin/emacs")))
               (make-file-writable elisp-file)
               (emacs-substitute-variables elisp-file
                 ("hindent-process-path"
                  (string-append out "/bin/hindent")))
               (install-file elisp-file dest)
               (emacs-generate-autoloads "hindent" dest)))))))
    (inputs
     `(("ghc-haskell-src-exts" ,ghc-haskell-src-exts)
       ("ghc-monad-loops" ,ghc-monad-loops)
       ("ghc-utf8-string" ,ghc-utf8-string)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-yaml" ,ghc-yaml)
       ("ghc-unix-compat" ,ghc-unix-compat)
       ("ghc-path" ,ghc-path)
       ("ghc-path-io" ,ghc-path-io)
       ("ghc-optparse-applicative" ,ghc-optparse-applicative)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("ghc-diff" ,ghc-diff)
       ("emacs" ,emacs-minimal)))
    (home-page
     "https://github.com/commercialhaskell/hindent")
    (synopsis "Extensible Haskell pretty printer")
    (description
     "This package provides automatic formatting for Haskell files.  Both a
library and an executable.")
    (license license:bsd-3)))

(define-public ghc-hinotify
  (package
    (name "ghc-hinotify")
    (version "0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://hackage.haskell.org/package/hinotify/"
                    "hinotify-" version ".tar.gz"))
              (sha256
               (base32
                "1x1lm685ws2q0z0ibwq6x3l72xh67mj06s36xiga3al48d92q63x"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-async" ,ghc-async)))
    (home-page "https://github.com/kolmodin/hinotify.git")
    (synopsis "Haskell binding to inotify")
    (description "This library provides a wrapper to the Linux kernel's inotify
feature, allowing applications to subscribe to notifications when a file is
accessed or modified.")
    (license license:bsd-3)))

(define-public ghc-hledger-lib
  (package
    (name "ghc-hledger-lib")
    (version "1.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/hledger-lib/hledger-lib-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1w6qp01cak6spnpldm01czlm6i5a2alw47w76875l2nagrc4rfp2"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-ansi-terminal" ,ghc-ansi-terminal)
       ("ghc-base-compat-batteries" ,ghc-base-compat-batteries)
       ("ghc-blaze-markup" ,ghc-blaze-markup)
       ("ghc-call-stack" ,ghc-call-stack)
       ("ghc-cassava" ,ghc-cassava)
       ("ghc-cassava-megaparsec" ,ghc-cassava-megaparsec)
       ("ghc-cmdargs" ,ghc-cmdargs)
       ("ghc-data-default" ,ghc-data-default)
       ("ghc-decimal" ,ghc-decimal)
       ("ghc-easytest" ,ghc-easytest)
       ("ghc-extra" ,ghc-extra)
       ("ghc-file-embed" ,ghc-file-embed)
       ("ghc-glob" ,ghc-glob)
       ("ghc-hashtables" ,ghc-hashtables)
       ("ghc-megaparsec" ,ghc-megaparsec)
       ("ghc-mtl-compat" ,ghc-mtl-compat)
       ("ghc-old-time" ,ghc-old-time)
       ("ghc-parser-combinators" ,ghc-parser-combinators)
       ("ghc-pretty-show" ,ghc-pretty-show)
       ("ghc-regex-tdfa" ,ghc-regex-tdfa)
       ("ghc-safe" ,ghc-safe)
       ("ghc-split" ,ghc-split)
       ("ghc-tabular" ,ghc-tabular)
       ("ghc-uglymemo" ,ghc-uglymemo)
       ("ghc-utf8-string" ,ghc-utf8-string)))
    (native-inputs `(("ghc-doctest" ,ghc-doctest)))
    (home-page "https://hledger.org")
    (synopsis "Reusable library providing the core functionality of hledger")
    (description
     "A reusable library containing hledger's core functionality.
This is used by most hledger* packages so that they support the same common
file formats, command line options, reports etc.

hledger is a robust, cross-platform set of tools for tracking money, time, or
any other commodity, using double-entry accounting and a simple, editable file
format, with command-line, terminal and web interfaces.  It is a Haskell
rewrite of Ledger, and one of the leading implementations of Plain Text
Accounting.")
    (license license:gpl3)))

(define-public ghc-hmatrix
  (package
    (name "ghc-hmatrix")
    (version "0.20.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/hmatrix/hmatrix-"
             version ".tar.gz"))
       (sha256
        (base32 "1sqy1aci5zfagkb34mz3xdil7cl96z4b4cx28cha54vc5sx1lhpg"))))
    (build-system haskell-build-system)
    (arguments
     `(#:extra-directories ("lapack")))
    (inputs
     `(("ghc-random" ,ghc-random)
       ("ghc-split" ,ghc-split)
       ("ghc-storable-complex" ,ghc-storable-complex)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-vector" ,ghc-vector)
       ;;("openblas" ,openblas)
       ("lapack" ,lapack)))
    ;; Guix's OpenBLAS is built with the flag "NO_LAPACK=1" which
    ;; disables inclusion of the LAPACK functions.
    ;; (arguments `(#:configure-flags '("--flags=openblas")))
    (home-page "https://github.com/albertoruiz/hmatrix")
    (synopsis "Haskell numeric linear algebra library")
    (description "The HMatrix package provices a Haskell library for
dealing with linear systems, matrix decompositions, and other
numerical computations based on BLAS and LAPACK.")
    (license license:bsd-3)))

(define-public ghc-hmatrix-gsl
  (package
    (name "ghc-hmatrix-gsl")
    (version "0.19.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/hmatrix-gsl/hmatrix-gsl-"
             version ".tar.gz"))
       (sha256
        (base32 "0v6dla426x4ywaq59jm89ql1i42n39iw6z0j378xwb676v9kfxhm"))))
    (build-system haskell-build-system)
    (arguments
     `(#:extra-directories ("gsl")))
    (inputs
     `(("ghc-hmatrix" ,ghc-hmatrix)
       ("ghc-vector" ,ghc-vector)
       ("ghc-random" ,ghc-random)
       ("gsl" ,gsl)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "https://github.com/albertoruiz/hmatrix")
    (synopsis "Haskell GSL binding")
    (description "This Haskell library provides a purely functional
interface to selected numerical computations, internally implemented
using GSL.")
    (license license:gpl3+)))

(define-public ghc-hmatrix-gsl-stats
  (package
    (name "ghc-hmatrix-gsl-stats")
    (version "0.4.1.8")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://hackage.haskell.org/package/hmatrix-gsl-stats/hmatrix-gsl-stats-"
         version ".tar.gz"))
       (sha256
        (base32 "1cq049sj3q5r06x7i35hqrkf2jc4p4kfi9zv0jmi2vp7w4644i5q"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-vector" ,ghc-vector)
       ("ghc-storable-complex" ,ghc-storable-complex)
       ("ghc-hmatrix" ,ghc-hmatrix)
       ("gsl" ,gsl)))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (home-page "http://code.haskell.org/hmatrix-gsl-stats")
    (synopsis "GSL Statistics interface for Haskell")
    (description "This Haskell library provides a purely functional
interface for statistics based on hmatrix and GSL.")
    (license license:bsd-3)))

(define-public ghc-hmatrix-special
  (package
    (name "ghc-hmatrix-special")
    (version "0.19.0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://hackage.haskell.org/package/hmatrix-special/hmatrix-special-"
         version ".tar.gz"))
       (sha256
        (base32 "1mywr61kr852sbff26n9x95kswx9l4ycbv6s68qsbkh02xzqq7qz"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-hmatrix" ,ghc-hmatrix)
       ("ghc-hmatrix-gsl" ,ghc-hmatrix-gsl)))
    (home-page "https://github.com/albertoruiz/hmatrix")
    (synopsis "Haskell interface to GSL special functions")
    (description "This library provides an interface to GSL special
functions for Haskell.")
    (license license:gpl3+)))

(define-public ghc-hostname
  (package
    (name "ghc-hostname")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/hostname/"
                           "hostname-" version ".tar.gz"))
       (sha256
        (base32
         "0p6gm4328946qxc295zb6vhwhf07l1fma82vd0siylnsnsqxlhwv"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/hostname")
    (synopsis "Hostname in Haskell")
    (description "Network.HostName is a simple package providing a means to
determine the hostname.")
    (license license:bsd-3)))

(define-public ghc-hourglass
  (package
    (name "ghc-hourglass")
    (version "0.2.12")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "hourglass/hourglass-" version ".tar.gz"))
              (sha256
               (base32
                "0jnay5j13vpz6i1rkaj3j0d9v8jfpri499xn3l7wd01f81f5ncs4"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-old-locale" ,ghc-old-locale)))
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)))
    (home-page "https://github.com/vincenthz/hs-hourglass")
    (synopsis "Simple time-related library for Haskell")
    (description
     "This is a simple time library providing a simple but powerful and
performant API.  The backbone of the library are the @code{Timeable} and
@code{Time} type classes.  Each @code{Timeable} instances can be converted to
a type that has a @code{Time} instances, and thus are different
representations of current time.")
    (license license:bsd-3)))

(define-public ghc-hpack
  (package
    (name "ghc-hpack")
    (version "0.31.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/hpack/"
                           "hpack-" version ".tar.gz"))
       (patches (search-patches "ghc-hpack-fix-tests.patch"))
       (sha256
        (base32
         "1l2d6185lawwhsj70swxkvcacm0hvcn9qsrlx4ph4gs6k578603g"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-aeson" ,ghc-aeson)
       ("ghc-bifunctors" ,ghc-bifunctors)
       ("ghc-cryptonite" ,ghc-cryptonite)
       ("ghc-glob" ,ghc-glob)
       ("ghc-http-client" ,ghc-http-client)
       ("ghc-http-client-tls" ,ghc-http-client-tls)
       ("ghc-http-types" ,ghc-http-types)
       ("ghc-infer-license" ,ghc-infer-license)
       ("ghc-scientific" ,ghc-scientific)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-vector" ,ghc-vector)
       ("ghc-yaml" ,ghc-yaml)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-interpolate" ,ghc-interpolate)
       ("ghc-mockery" ,ghc-mockery)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-temporary" ,ghc-temporary)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://github.com/sol/hpack")
    (synopsis "Tools for an alternative Haskell package format")
    (description
     "Hpack is a format for Haskell packages.  It is an alternative to the
Cabal package format and follows different design principles.  Hpack packages
are described in a file named @code{package.yaml}.  Both @code{cabal2nix} and
@code{stack} support @code{package.yaml} natively.  For other build tools the
@code{hpack} executable can be used to generate a @code{.cabal} file from
@code{package.yaml}.")
    (license license:expat)))

(define-public ghc-hspec-megaparsec
  (package
    (name "ghc-hspec-megaparsec")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/"
             "package/hspec-megaparsec/hspec-megaparsec-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0w8nn2rh01lkiwsiyqh3gviklhfmy0245rakj94dmliyljw8skfg"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-hspec-expectations" ,ghc-hspec-expectations)
       ("ghc-megaparsec" ,ghc-megaparsec)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)))
    (home-page "https://github.com/mrkkrp/hspec-megaparsec")
    (synopsis "Utility functions for testing Megaparsec parsers with Hspec")
    (description
     "Provides a small set of helper functions for testing Megaparsec parsers
with Hspec.")
    (license license:bsd-3)))

(define-public ghc-hs-bibutils
  (package
    (name "ghc-hs-bibutils")
    (version "6.7.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/hs-bibutils/hs-bibutils-"
             version ".tar.gz"))
       (sha256
        (base32
         "1qfyssl76lm4g09yxr3y10kmf8cnzls46g5h0ijk0wpk9wlhbln5"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-syb" ,ghc-syb)))
    (home-page "https://hackage.haskell.org/package/hs-bibutils")
    (synopsis "Haskell bindings to bibutils")
    (description
     "This package provides Haskell bindings to @code{bibutils}, a library
that interconverts between various bibliography formats using a common
MODS-format XML intermediate.")
    (license license:gpl2+)))

(define-public ghc-hslogger
  (package
    (name "ghc-hslogger")
    (version "1.2.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "hslogger-" version "/" "hslogger-"
                           version ".tar.gz"))
       (sha256 (base32
                "0ykcsk7wqygvrg60r5kpl6xfinr706al8pfyk5wj67wjs24lqypr"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-network" ,ghc-network)
       ("ghc-old-locale" ,ghc-old-locale)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)))
    (home-page "https://software.complete.org/hslogger")
    (synopsis "Logging framework for Haskell, similar to Python's logging module")
    (description "Hslogger lets each log message have a priority and source be
associated with it.  The programmer can then define global handlers that route
or filter messages based on the priority and source.  It also has a syslog
handler built in.")
    (license license:bsd-3)))

(define-public ghc-hslua
  (package
    (name "ghc-hslua")
    (version "1.0.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "hslua/hslua-" version ".tar.gz"))
              (sha256
               (base32
                "183bgl5jcx5y2r94lviqfw0a5w9089nxjd1z40k8vx9y2h60pm6j"))))
    (build-system haskell-build-system)
    (arguments
     `(#:configure-flags '("-fsystem-lua")
       #:extra-directories ("lua")))
    (inputs
     `(("lua" ,lua)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-fail" ,ghc-fail)))
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-expected-failure" ,ghc-tasty-expected-failure)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-quickcheck-instances" ,ghc-quickcheck-instances)))
    (home-page "https://hackage.haskell.org/package/hslua")
    (synopsis "Lua language interpreter embedding in Haskell")
    (description
     "The Scripting.Lua module is a wrapper of the Lua language interpreter as
described in @url{https://www.lua.org/}.")
    (license license:expat)))

(define-public ghc-hslua-module-system
  (package
    (name "ghc-hslua-module-system")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "hslua-module-system/hslua-module-system-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1m7wz3g5c34pyizqw5mllzhsy2vziddhlbhjfwdvd7nhd3p4v3hh"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-exceptions" ,ghc-exceptions)
       ("ghc-hslua" ,ghc-hslua)
       ("ghc-temporary" ,ghc-temporary)))
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)))
    (home-page "https://github.com/hslua/hslua-module-system")
    (synopsis "Lua module wrapper around Haskell's System module")
    (description "This library provides access to system information and
functionality to Lua scripts via Haskell's @code{System} module.  Intended
usage for this package is to preload it by adding the loader function to
@code{package.preload}.  Note that the Lua @code{package} library must have
already been loaded before the loader can be added.")
    (license license:expat)))

(define-public ghc-hslua-module-text
  (package
    (name "ghc-hslua-module-text")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "hslua-module-text/hslua-module-text-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1ikdwvvxhbd5wmfr85dzs2ccamh9rbbpgy899z7s1vlv5q1dj0hk"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-hslua" ,ghc-hslua)))
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)))
    (home-page "https://github.com/hslua/hslua-module-text")
    (synopsis "Lua module for text")
    (description
     "This package provides a UTF-8 aware subset of Lua's @code{string} module
for Haskell.  The functions provided by this module are @code{upper},
@code{lower}, @code{len}, @code{reverse}, and @code{sub}.")
    (license license:expat)))

(define-public ghc-hsyaml
  (package
    (name "ghc-hsyaml")
    (version "0.1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "HsYAML/HsYAML-" version ".tar.gz"))
       (sha256
        (base32
         "1pajfhj16559v64ixm8j7bvxdqmxg6c3c0z3wz7in8ckswgzfp54"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("1" "0j6qmmcz5yqh89hs2cq453maix50q61vl2h0ahj5lg02bygn42cf")))
    (home-page "https://github.com/haskell-hvr/HsYAML")
    (synopsis "Pure Haskell YAML 1.2 parser")
    (description "This library provides a
@url{http://yaml.org/spec/1.2/spec.html, YAML 1.2} parser implementation
for Haskell.  Its features include:

@itemize
@item Pure Haskell implementation with small dependency footprint and
emphasis on strict compliance with the YAML 1.2 specification.

@item Direct decoding to native Haskell types via (aeson-inspired)
typeclass-based API (see @code{Data.YAML}).

@item Support for constructing custom YAML node graph
representation (including support for cyclic YAML data structures).

@item Support for the standard (untyped) @emph{Failsafe}, (strict)
@emph{JSON}, and (flexible) @emph{Core} ``schemas'' providing implicit
typing rules as defined in the YAML 1.2 specification (including support
for user-defined custom schemas).

@item Event-based API resembling LibYAML's Event-based API (see
@code{Data.YAML.Event}).

@item Low-level API access to lexical token-based scanner (see
@code{Data.YAML.Token}).
@end itemize")
    (license license:gpl2+)))

(define-public ghc-http-api-data
  (package
    (name "ghc-http-api-data")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "http-api-data-" version "/"
                           "http-api-data-" version ".tar.gz"))
       (sha256
        (base32
         "1ps4bvln43gz72dr9mc3c9n1rn38c4rz6m49vxzz9nz6jz1978rv"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-attoparsec" ,ghc-attoparsec)
              ("ghc-attoparsec-iso8601" ,ghc-attoparsec-iso8601)
              ("ghc-cookie" ,ghc-cookie)
              ("ghc-hashable" ,ghc-hashable)
              ("ghc-http-types" ,ghc-http-types)
              ("ghc-time-compat" ,ghc-time-compat)
              ("ghc-unordered-containers" ,ghc-unordered-containers)
              ("ghc-uuid-types" ,ghc-uuid-types)))
    (native-inputs
     `(("cabal-doctest" ,cabal-doctest)
       ("ghc-nats" ,ghc-nats)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-quickcheck-instances" ,ghc-quickcheck-instances)
       ("ghc-doctest" ,ghc-doctest)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://github.com/fizruk/http-api-data")
    (synopsis "Convert to/from HTTP API data like URL pieces, headers and
query parameters")
    (description "This Haskell package defines typeclasses used for converting
Haskell data types to and from HTTP API data.")
    (license license:bsd-3)))

(define-public ghc-ieee754
  (package
    (name "ghc-ieee754")
    (version "0.8.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://hackage.haskell.org/package/ieee754/"
                    "ieee754-" version ".tar.gz"))
              (sha256
               (base32
                "1lcs521g9lzy9d7337vg4w7q7s8500rfqy7rcifcz6pm6yfgyb8f"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/patperry/hs-ieee754")
    (synopsis "Utilities for dealing with IEEE floating point numbers")
    (description "Utilities for dealing with IEEE floating point numbers,
ported from the Tango math library; approximate and exact equality comparisons
for general types.")
    (license license:bsd-3)))

(define-public ghc-ifelse
  (package
    (name "ghc-ifelse")
    (version "0.85")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "IfElse/IfElse-" version ".tar.gz"))
       (sha256
        (base32
         "1kfx1bwfjczj93a8yqz1n8snqiq5655qgzwv1lrycry8wb1vzlwa"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/IfElse")
    (synopsis "Monadic control flow with anaphoric variants")
    (description "This library provides functions for control flow inside of
monads with anaphoric variants on @code{if} and @code{when} and a C-like
@code{switch} function.")
    (license license:bsd-3)))

(define-public ghc-indents
  (package
    (name "ghc-indents")
    (version "0.5.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://hackage.haskell.org/package/indents/indents-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0dpcwiz0dwn5aqdsc50plfaawh86adhf7jx5dsmhn5q5nz32qn51"))))
    (build-system haskell-build-system)
    ;; This package needs an older version of tasty.
    (arguments '(#:tests? #f))
    (inputs
     `(("ghc-concatenative" ,ghc-concatenative)))
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)))
    (home-page "https://github.com/jaspervdj/indents")
    (synopsis "Indentation sensitive parser-combinators for parsec")
    (description
     "This library provides functions for use in parsing indentation sensitive
contexts.  It parses blocks of lines all indented to the same level as well as
lines continued at an indented level below.")
    (license license:bsd-3)))

(define-public ghc-infer-license
  (package
    (name "ghc-infer-license")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "infer-license/infer-license-" version ".tar.gz"))
       (sha256
        (base32
         "0wlfm6bf55kfvm74xar9lmjg5v1103rs9m3grw1rq5bmcmhzxrhj"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-text-metrics" ,ghc-text-metrics)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://hackage.haskell.org/package/infer-license")
    (synopsis "Infer software license from a given license file")
    (description "This library provides tools to infer a software
license from a given license file.")
    (license license:expat)))

(define-public ghc-inline-c
  (package
    (name "ghc-inline-c")
    (version "0.7.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/inline-c/"
                           "inline-c-" version ".tar.gz"))
       (sha256
        (base32
         "19scbviwiv1fbsdcjji3dscjg7w0xa8r97xwkqqrwm7zhvrg5wns"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-ansi-wl-pprint" ,ghc-ansi-wl-pprint)
       ("ghc-cryptohash" ,ghc-cryptohash)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-parsers" ,ghc-parsers)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-vector" ,ghc-vector)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-raw-strings-qq" ,ghc-raw-strings-qq)
       ("ghc-regex-posix" ,ghc-regex-posix)))
    (home-page "https://hackage.haskell.org/package/inline-c")
    (synopsis "Write Haskell source files including C code inline")
    (description
     "inline-c lets you seamlessly call C libraries and embed high-performance
inline C code in Haskell modules.  Haskell and C can be freely intermixed in
the same source file, and data passed to and from code in either language with
minimal overhead.  No FFI required.")
    (license license:expat)))

(define-public ghc-inline-c-cpp
  (package
    (name "ghc-inline-c-cpp")
    (version "0.3.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/inline-c-cpp/"
                           "inline-c-cpp-" version ".tar.gz"))
       (sha256
        (base32
         "1sxwx9dh60qfpa72dymj015zwd6prhb70x5mkabqzi7nhg3aakln"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-inline-c" ,ghc-inline-c)
       ("ghc-safe-exceptions" ,ghc-safe-exceptions)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)))
    (home-page "https://hackage.haskell.org/package/inline-c-cpp")
    (synopsis "Lets you embed C++ code into Haskell")
    (description
     "This package provides utilities to inline C++ code into Haskell using
@code{inline-c}.")
    (license license:expat)))

(define-public ghc-integer-logarithms
  (package
    (name "ghc-integer-logarithms")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "integer-logarithms/integer-logarithms-"
                           version ".tar.gz"))
       (sha256
        (base32
         "05pc5hws66csvcvfswlwcr2fplwn1lbssvwifjxkbbwqhq0n5qjs"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'update-constraints
           (lambda _
             (substitute* "integer-logarithms.cabal"
               (("tasty >= 0\\.10 && < 1\\.1")
                "tasty >= 0.10 && < 1.2")))))))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-smallcheck" ,ghc-smallcheck)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
       ("ghc-tasty-smallcheck" ,ghc-tasty-smallcheck)))
    (home-page "https://github.com/Bodigrim/integer-logarithms")
    (synopsis "Integer logarithms")
    (description
     "This package provides the following modules:
@code{Math.NumberTheory.Logarithms} and
@code{Math.NumberTheory.Powers.Integer} from the @code{arithmoi} package,
@code{GHC.Integer.Logarithms.Compat} and
@code{Math.NumberTheory.Power.Natural}, as well as some additional functions
in migrated modules.")
    (license license:expat)))

(define-public ghc-integer-logarithms-bootstrap
  (package
    (inherit ghc-integer-logarithms)
    (name "ghc-integer-logarithms-bootstrap")
    (arguments `(#:tests? #f))
    (native-inputs '())
    (properties '((hidden? #t)))))

(define-public ghc-interpolate
  (package
    (name "ghc-interpolate")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/interpolate/"
                           "interpolate-" version ".tar.gz"))
       (sha256
        (base32
         "1gkaj98yz363v38fv78sqby236mp8yqwqcilx7kr2b9z0w3204bf"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-haskell-src-meta" ,ghc-haskell-src-meta)))
    (native-inputs
     `(("ghc-base-compat" ,ghc-base-compat)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-quickcheck-instances" ,ghc-quickcheck-instances)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://github.com/sol/interpolate")
    (synopsis "String interpolation library")
    (description "This package provides a string interpolation library for
Haskell.")
    (license license:expat)))

(define-public ghc-intervalmap
  (package
    (name "ghc-intervalmap")
    (version "0.6.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/IntervalMap/"
                           "IntervalMap-" version ".tar.gz"))
       (sha256
        (base32
         "0vdlvxvhf7vjyv0mfn6jaj2i2gclqv8419ck32s2jxfcmki5m5g8"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "http://www.chr-breitkopf.de/comp/IntervalMap")
    (synopsis "Containers for intervals, with efficient search")
    (description
     "This package provides ordered containers of intervals, with efficient
search for all keys containing a point or overlapping an interval.  See the
example code on the home page for a quick introduction.")
    (license license:bsd-3)))

(define-public ghc-intervals
  (package
    (name "ghc-intervals")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "intervals/intervals-" version ".tar.gz"))
       (sha256
        (base32
         "00vyxf3ba9d7aas3npfapr53w71fslgh69fczjb25axr66fvzqww"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-distributive" ,ghc-distributive)))
    (native-inputs
     `(("cabal-doctest" ,cabal-doctest)
       ("ghc-doctest" ,ghc-doctest)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (arguments
     `(#:cabal-revision
       ("4" "1qx3q0v13l1zaln9zdk8chxpxhshbz5x0vqm0qda7d1kpv7h6a7r")))
    (home-page "https://github.com/ekmett/intervals")
    (synopsis "Interval arithmetic")
    (description "This library provides
@code{Numeric.Interval.Interval}, which represets a closed, convex set
of floating point values.")
    (license license:bsd-3)))

(define-public ghc-invariant
  (package
    (name "ghc-invariant")
    (version "0.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/invariant/invariant-"
             version ".tar.gz"))
       (sha256
        (base32
         "03245nhcqxx6b0yw81fzqaqd7cgllmx8awzhvs2xv7ys73pmsgnp"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-bifunctors" ,ghc-bifunctors)
       ("ghc-comonad" ,ghc-comonad)
       ("ghc-contravariant" ,ghc-contravariant)
       ("ghc-profunctors" ,ghc-profunctors)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-statevar" ,ghc-statevar)
       ("ghc-tagged" ,ghc-tagged)
       ("ghc-th-abstraction" ,ghc-th-abstraction)
       ("ghc-transformers-compat" ,ghc-transformers-compat)
       ("ghc-unordered-containers" ,ghc-unordered-containers)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://github.com/nfrisby/invariant-functors")
    (synopsis "Haskell98 invariant functors")
    (description "Haskell98 invariant functors (also known as exponential
functors).  For more information, see Edward Kmett's article
@uref{http://comonad.com/reader/2008/rotten-bananas/, Rotten Bananas}.")
    (license license:bsd-2)))

(define-public ghc-io-streams
  (package
    (name "ghc-io-streams")
    (version "1.5.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "io-streams/io-streams-" version ".tar.gz"))
       (sha256
        (base32
         "1c7byr943x41nxpc3bnz152fvfbmakafq2958wyf9qiyp2pz18la"))))
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
    (home-page "https://hackage.haskell.org/package/io-streams")
    (synopsis "Simple and composable stream I/O")
    (description "This library contains simple and easy-to-use
primitives for I/O using streams.")
    (license license:bsd-3)))

(define-public ghc-io-streams-haproxy
  (package
    (name "ghc-io-streams-haproxy")
    (version "1.0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "io-streams-haproxy/io-streams-haproxy-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1dcn5hd4fiwyq7m01r6fi93vfvygca5s6mz87c78m0zyj29clkmp"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-io-streams" ,ghc-io-streams)
       ("ghc-network" ,ghc-network)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)))
    (home-page "http://snapframework.com/")
    (synopsis "HAProxy protocol 1.5 support for io-streams")
    (description "HAProxy protocol version 1.5 support
(see @uref{http://haproxy.1wt.eu/download/1.5/doc/proxy-protocol.txt})
for applications using io-streams.  The proxy protocol allows information
about a networked peer (like remote address and port) to be propagated
through a forwarding proxy that is configured to speak this protocol.")
    (license license:bsd-3)))

(define-public ghc-iproute
  (package
    (name "ghc-iproute")
    (version "1.7.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/iproute/iproute-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0gab5930nvzrpvisx3x43ydnp2rd4fbmy9cq1zpgqy1adx5gx8z6"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f)) ; FIXME: Tests cannot find System.ByteOrder,
                               ; exported by ghc-byteorder.  Doctest issue.
    (inputs
     `(("ghc-appar" ,ghc-appar)
       ("ghc-byteorder" ,ghc-byteorder)
       ("ghc-network" ,ghc-network)
       ("ghc-safe" ,ghc-safe)))
    (home-page "https://www.mew.org/~kazu/proj/iproute/")
    (synopsis "IP routing table")
    (description "IP Routing Table is a tree of IP ranges to search one of
them on the longest match base.  It is a kind of TRIE with one way branching
removed.  Both IPv4 and IPv6 are supported.")
    (license license:bsd-3)))

(define-public ghc-ipynb
  (package
    (name "ghc-ipynb")
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "ipynb/ipynb-" version ".tar.gz"))
       (sha256
        (base32
         "0daadhzil4q573mqb0rpvjzm0vpkzgzqcimw480qpvlh6rhppwj5"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-base64-bytestring" ,ghc-base64-bytestring)
       ("ghc-aeson" ,ghc-aeson)
       ("ghc-semigroups" ,ghc-semigroups)))
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-aeson-diff" ,ghc-aeson-diff)
       ("ghc-microlens-aeson" ,ghc-microlens-aeson)
       ("ghc-microlens" ,ghc-microlens)
       ("ghc-vector" ,ghc-vector)))
    (home-page "https://hackage.haskell.org/package/ipynb")
    (synopsis "Data structure for working with Jupyter notebooks")
    (description "This library defines a data structure for representing
Jupyter notebooks, along with @code{ToJSON} and @code{FromJSON}
instances for conversion to and from JSON .ipynb files.")
    (license license:bsd-3)))

(define-public ghc-iwlib
  (package
    (name "ghc-iwlib")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/iwlib/iwlib-"
                           version ".tar.gz"))
       (sha256
        (base32 "0khmfwql4vwj55idsxmhjhrbqzfir3g9wm5lmpvnf77mm95cfpdz"))))
    (build-system haskell-build-system)
    (arguments
     `(#:extra-directories ("wireless-tools")))
    (inputs
     `(("wireless-tools" ,wireless-tools)))
    (home-page "https://github.com/jaor/iwlib")
    (synopsis "Haskell binding to the iw wireless networking library")
    (description
     "IWlib is a thin Haskell binding to the iw C library.  It provides
information about the current wireless network connections, and adapters on
supported systems.")
    (license license:bsd-3)))

(define-public ghc-json
  (package
    (name "ghc-json")
    (version "0.9.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/json/"
                           "json-" version ".tar.gz"))
       (sha256
        (base32
         "1z8s3mfg76p2flqqd2wqsi96l5bg8k8w8m58zlv81pw3k7h1vbwb"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-syb" ,ghc-syb)))
    (home-page "https://hackage.haskell.org/package/json")
    (synopsis "Serializes Haskell data to and from JSON")
    (description "This package provides a parser and pretty printer for
converting between Haskell values and JSON.
JSON (JavaScript Object Notation) is a lightweight data-interchange format.")
    (license license:bsd-3)))

(define-public ghc-juicypixels
  (package
    (name "ghc-juicypixels")
    (version "3.3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "JuicyPixels/JuicyPixels-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0qacrnz2qcykj3f6c4k2p8qd31pa2slpv3ykfblgizrfh3401q6x"))))
    (build-system haskell-build-system)
    (outputs '("out" "static" "doc"))
    (inputs
     `(("ghc-zlib" ,ghc-zlib)
       ("ghc-vector" ,ghc-vector)
       ("ghc-primitive" ,ghc-primitive)
       ("ghc-mmap" ,ghc-mmap)))
    (home-page "https://github.com/Twinside/Juicy.Pixels")
    (synopsis "Picture loading and serialization library")
    (description
     "This library can load and store images in PNG, Bitmap, JPEG, Radiance,
TIFF and GIF formats.")
    (license license:bsd-3)))

(define-public ghc-kan-extensions
  (package
    (name "ghc-kan-extensions")
    (version "5.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/kan-extensions/kan-extensions-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1lyvyiwwh962j2nnnsqzlvp5zq6z8p3spvhmji99cjvldxc7wwkb"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-adjunctions" ,ghc-adjunctions)
       ("ghc-comonad" ,ghc-comonad)
       ("ghc-contravariant" ,ghc-contravariant)
       ("ghc-distributive" ,ghc-distributive)
       ("ghc-free" ,ghc-free)
       ("ghc-invariant" ,ghc-invariant)
       ("ghc-semigroupoids" ,ghc-semigroupoids)
       ("ghc-tagged" ,ghc-tagged)
       ("ghc-transformers-compat" ,ghc-transformers-compat)))
    (home-page "https://github.com/ekmett/kan-extensions/")
    (synopsis "Kan extensions library")
    (description "This library provides Kan extensions, Kan lifts, various
forms of the Yoneda lemma, and (co)density (co)monads for Haskell.")
    (license license:bsd-3)))

(define-public ghc-language-c
  (package
    (name "ghc-language-c")
    (version "0.8.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "language-c/language-c-" version ".tar.gz"))
       (sha256
        (base32
         "0bi02jdirkys8v7flf39vrpla2a74z1z0sdhy9lb9v7cmcc6rmpk"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-syb" ,ghc-syb)))
    (native-inputs
     `(("ghc-happy" ,ghc-happy)
       ("ghc-alex" ,ghc-alex)))
    (home-page "https://visq.github.io/language-c/")
    (synopsis "Analysis and generation of C code")
    (description
     "Language C is a Haskell library for the analysis and generation of C code.
It features a complete, well-tested parser and pretty printer for all of C99
and a large set of GNU extensions.")
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
    (home-page "https://hackage.haskell.org/package/language-glsl")
    (synopsis "GLSL abstract syntax tree, parser, and pretty-printer")
    (description "This package is a Haskell library for the
representation, parsing, and pretty-printing of GLSL 1.50 code.")
    (license license:bsd-3)))

(define-public ghc-language-haskell-extract
  (package
    (name "ghc-language-haskell-extract")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "language-haskell-extract-" version "/"
                           "language-haskell-extract-" version ".tar.gz"))
       (sha256
        (base32
         "1nxcs7g8a1sp91bzpy4cj6s31k5pvc3gvig04cbrggv5cvjidnhl"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-regex-posix" ,ghc-regex-posix)))
    (home-page "https://github.com/finnsson/template-helper")
    (synopsis "Haskell module to automatically extract functions from
the local code")
    (description "This package contains helper functions on top of
Template Haskell.

For example, @code{functionExtractor} extracts all functions after a
regexp-pattern, which can be useful if you wish to extract all functions
beginning with @code{test} (for a test framework) or all functions beginning
with @code{wc} (for a web service).")
    (license license:bsd-3)))

(define-public ghc-lens
  (package
    (name "ghc-lens")
    (version "4.17.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/lens/lens-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1gpkc53l2cggnfrgg5k4ih82rycjbdvpj9pnbi5cq8ms0dbvs4a7"))))
    (build-system haskell-build-system)
    (outputs '("out" "static" "doc"))
    (inputs
     `(("ghc-base-orphans" ,ghc-base-orphans)
       ("ghc-bifunctors" ,ghc-bifunctors)
       ("ghc-distributive" ,ghc-distributive)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-free" ,ghc-free)
       ("ghc-kan-extensions" ,ghc-kan-extensions)
       ("ghc-parallel" ,ghc-parallel)
       ("ghc-reflection" ,ghc-reflection)
       ("ghc-semigroupoids" ,ghc-semigroupoids)
       ("ghc-vector" ,ghc-vector)
       ("ghc-call-stack" ,ghc-call-stack)
       ("ghc-comonad" ,ghc-comonad)
       ("ghc-contravariant" ,ghc-contravariant)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-profunctors" ,ghc-profunctors)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-tagged" ,ghc-tagged)
       ("ghc-transformers-compat" ,ghc-transformers-compat)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-void" ,ghc-void)
       ("ghc-generic-deriving" ,ghc-generic-deriving)
       ("ghc-nats" ,ghc-nats)
       ("ghc-simple-reflect" ,ghc-simple-reflect)
       ("hlint" ,hlint)))
    (native-inputs
     `(("cabal-doctest" ,cabal-doctest)
       ("ghc-doctest" ,ghc-doctest)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework-th" ,ghc-test-framework-th)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://github.com/ekmett/lens/")
    (synopsis "Lenses, Folds and Traversals")
    (description "This library provides @code{Control.Lens}.  The combinators
in @code{Control.Lens} provide a highly generic toolbox for composing families
of getters, folds, isomorphisms, traversals, setters and lenses and their
indexed variants.")
    (license license:bsd-3)))

(define-public ghc-lens-family-core
  (package
    (name "ghc-lens-family-core")
    (version "1.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://hackage/package/lens-family-core/lens-family-core-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "009rf10pj1cb50v44cc1pq7qvfrmkkk9dikahs9qmvbvgl3mykwi"))))
    (build-system haskell-build-system)
    (home-page
     "http://hackage.haskell.org/package/lens-family-core")
    (synopsis "Haskell 98 Lens Families")
    (description
     "This package provides first class functional references.  In addition to
the usual operations of getting, setting and composition, plus integration
with the state monad, lens families provide some unique features:

@itemize
@item Polymorphic updating
@item Traversals
@item Cast projection functions to read-only lenses
@item Cast @code{toList} functions to read-only traversals
@item Cast semantic editor combinators to modify-only traversals
@end itemize

For optimal first-class support use the lens-family package with rank 2/rank N
polymorphism.  @code{Lens.Family.Clone} allows for first-class support of
lenses and traversals for those who require Haskell 98.")
    (license license:bsd-3)))

(define-public ghc-libffi
  (package
    (name "ghc-libffi")
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "libffi/libffi-" version ".tar.gz"))
       (sha256
        (base32
         "0g7jnhng3j7z5517aaqga0144aamibsbpgm3yynwyfzkq1kp0f28"))))
    (build-system haskell-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("libffi" ,libffi)))
    (home-page "https://hackage.haskell.org/package/libffi")
    (synopsis "Haskell binding to libffi")
    (description
     "A binding to libffi, allowing C functions of types only known at runtime
to be called from Haskell.")
    (license license:bsd-3)))

(define-public ghc-libmpd
  (package
    (name "ghc-libmpd")
    (version "0.9.0.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://hackage/package/libmpd/libmpd-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0vy287mn1vk8kvij5i3hc0p02l886cpsq5dds7kl6g520si3abkb"))))
    (build-system haskell-build-system)
    ;; Tests fail on i686.
    ;; See https://github.com/vimus/libmpd-haskell/issues/112
    (arguments `(#:tests? #f))
    (inputs
     `(("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-old-locale" ,ghc-old-locale)
       ("ghc-data-default-class" ,ghc-data-default-class)
       ("ghc-network" ,ghc-network)
       ("ghc-safe-exceptions" ,ghc-safe-exceptions)
       ("ghc-utf8-string" ,ghc-utf8-string)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://github.com/vimus/libmpd-haskell")
    (synopsis "Haskell client library for the Music Player Daemon")
    (description "This package provides a pure Haskell client library for the
Music Player Daemon.")
    (license license:expat)))

(define-public ghc-lib-parser
  (package
    (name "ghc-lib-parser")
    (version "8.8.0.20190424")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "ghc-lib-parser/ghc-lib-parser-" version ".tar.gz"))
       (sha256
        (base32
         "12gsh994pr13bsybwlravmi21la66dyw74pk74yfw2pnz682wv10"))))
    (build-system haskell-build-system)
    (outputs '("out" "static" "doc")) ; documentation is 39M
    (native-inputs
     `(("ghc-alex" ,ghc-alex)
       ("ghc-happy" ,ghc-happy)))
    (home-page "https://github.com/digital-asset/ghc-lib")
    (synopsis "The GHC API, decoupled from GHC versions")
    (description "This library implements the GHC API.  It is like the
compiler-provided @code{ghc} package, but it can be loaded on many
compiler versions.")
    (license license:bsd-3)))

(define-public ghc-libxml
  (package
    (name "ghc-libxml")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/libxml/"
                           "libxml-" version ".tar.gz"))
       (sha256
        (base32
         "01zvk86kg726lf2vnlr7dxiz7g3xwi5a4ak9gcfbwyhynkzjmsfi"))))
    (build-system haskell-build-system)
    (inputs
     `(("libxml2" ,libxml2)))
    (arguments
     `(#:configure-flags
       `(,(string-append "--extra-include-dirs="
                         (assoc-ref %build-inputs "libxml2")
                         "/include/libxml2"))))
    (home-page "https://hackage.haskell.org/package/libxml")
    (synopsis "Haskell bindings to libxml2")
    (description
     "This library provides minimal Haskell binding to libxml2.")
    (license license:bsd-3)))

(define-public ghc-libyaml
  (package
    (name "ghc-libyaml")
    (version "0.1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "libyaml/libyaml-" version ".tar.gz"))
       (sha256
        (base32
         "0psznm9c3yjsyj9aj8m2svvv9m2v0x90hnwarcx5sbswyi3l00va"))
       (modules '((guix build utils)))
       (snippet
        ;; Delete bundled LibYAML.
        '(begin
           (delete-file-recursively "libyaml_src")
           #t))))
    (build-system haskell-build-system)
    (arguments
     `(#:configure-flags `("--flags=system-libyaml")
       #:extra-directories ("libyaml+static")))
    (inputs
     `(("ghc-conduit" ,ghc-conduit)
       ("ghc-resourcet" ,ghc-resourcet)
       ("libyaml+static" ,libyaml+static)))
    (home-page "https://github.com/snoyberg/yaml#readme")
    (synopsis "Low-level, streaming YAML interface.")
    (description "This package provides a Haskell wrapper over the
LibYAML C library.")
    (license license:bsd-3)))

(define-public ghc-lifted-async
  (package
    (name "ghc-lifted-async")
    (version "0.10.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/lifted-async/lifted-async-"
             version ".tar.gz"))
       (sha256
        (base32
         "0cwl1d0wjpdk0v1l1qxiqiksmak950c8gx169c1q77cg0z18ijf9"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-async" ,ghc-async)
       ("ghc-lifted-base" ,ghc-lifted-base)
       ("ghc-transformers-base" ,ghc-transformers-base)
       ("ghc-monad-control" ,ghc-monad-control)
       ("ghc-constraints" ,ghc-constraints)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-expected-failure" ,ghc-tasty-expected-failure)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-tasty-th" ,ghc-tasty-th)))
    (home-page "https://github.com/maoe/lifted-async")
    (synopsis "Run lifted IO operations asynchronously and wait for their results")
    (description
     "This package provides IO operations from @code{async} package lifted to any
instance of @code{MonadBase} or @code{MonadBaseControl}.")
    (license license:bsd-3)))

(define-public ghc-lifted-base
  (package
    (name "ghc-lifted-base")
    (version "0.2.3.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/lifted-base/lifted-base-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1i8p8d3rkdh21bhgjjh32vd7qqjr7jq7p59qds0aw2kmargsjd61"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f)) ; FIXME: Missing testing libraries.
    (inputs
     `(("ghc-transformers-base" ,ghc-transformers-base)
       ("ghc-monad-control" ,ghc-monad-control)
       ("ghc-transformers-compat" ,ghc-transformers-compat)
       ("ghc-hunit" ,ghc-hunit)))
    (home-page "https://github.com/basvandijk/lifted-base")
    (synopsis "Lifted IO operations from the base library")
    (description "Lifted-base exports IO operations from the @code{base}
library lifted to any instance of @code{MonadBase} or @code{MonadBaseControl}.
Note that not all modules from @code{base} are converted yet.  The package
includes a copy of the @code{monad-peel} test suite written by Anders
Kaseorg.")
    (license license:bsd-3)))

(define-public ghc-linear
  (package
    (name "ghc-linear")
    (version "1.20.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/linear/"
                           "linear-" version ".tar.gz"))
       (sha256
        (base32
         "0h7yqigq593n7wsl7nz6a5f137wznm7y679wsii0ph0zsc4v5af5"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-adjunctions" ,ghc-adjunctions)
       ("ghc-base-orphans" ,ghc-base-orphans)
       ("ghc-bytes" ,ghc-bytes)
       ("ghc-cereal" ,ghc-cereal)
       ("ghc-distributive" ,ghc-distributive)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-lens" ,ghc-lens)
       ("ghc-reflection" ,ghc-reflection)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-semigroupoids" ,ghc-semigroupoids)
       ("ghc-tagged" ,ghc-tagged)
       ("ghc-transformers-compat" ,ghc-transformers-compat)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-vector" ,ghc-vector)
       ("ghc-void" ,ghc-void)))
    (native-inputs
     `(("cabal-doctest" ,cabal-doctest)
       ("ghc-doctest" ,ghc-doctest)
       ("ghc-simple-reflect" ,ghc-simple-reflect)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-hunit" ,ghc-hunit)))
    (home-page "https://github.com/ekmett/linear/")
    (synopsis "Linear algebra library for Haskell")
    (description
     "This package provides types and combinators for linear algebra on free
vector spaces.")
    (license license:bsd-3)))

(define-public ghc-listlike
  (package
    (name "ghc-listlike")
    (version "4.6.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://hackage.haskell.org/package/ListLike/ListLike-"
         version ".tar.gz"))
       (sha256
        (base32
         "0m65x8yaq7q50gznln8mga2wrc8cvjx6gw9rim8s7xqcrx6y5zjh"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-vector" ,ghc-vector)
       ("ghc-dlist" ,ghc-dlist)
       ("ghc-fmlist" ,ghc-fmlist)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-random" ,ghc-random)
       ("ghc-utf8-string" ,ghc-utf8-string)))
    (home-page "https://github.com/JohnLato/listlike")
    (synopsis "Generic support for list-like structures")
    (description "The ListLike module provides a common interface to the
various Haskell types that are list-like.  Predefined interfaces include
standard Haskell lists, Arrays, ByteStrings, and lazy ByteStrings.
Custom types can easily be made ListLike instances as well.

ListLike also provides for String-like types, such as String and
ByteString, for types that support input and output, and for types that
can handle infinite lists.")
    (license license:bsd-3)))

(define-public ghc-llvm-hs-pure
  (package
    (name "ghc-llvm-hs-pure")
    (version "9.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/llvm-hs-pure/"
                           "llvm-hs-pure-" version ".tar.gz"))
       (sha256
        (base32
         "0pxb5ah8r5pzpz2ibqw3g9g1isigb4z7pbzfrwr8kmcjn74ab3kf"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-fail" ,ghc-fail)
       ("ghc-unordered-containers" ,ghc-unordered-containers)))
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)))
    (home-page "https://github.com/llvm-hs/llvm-hs/")
    (synopsis "Pure Haskell LLVM functionality (no FFI)")
    (description "llvm-hs-pure is a set of pure Haskell types and functions
for interacting with LLVM.  It includes an algebraic datatype (ADT) to represent
LLVM IR.  The llvm-hs package builds on this one with FFI bindings to LLVM, but
llvm-hs-pure does not require LLVM to be available.")
    (license license:bsd-3)))

(define-public ghc-llvm-hs
  (package
    (name "ghc-llvm-hs")
    (version "9.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/llvm-hs/llvm-hs-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0723xgh45h9cyxmmjsvxnsp8bpn1ljy4qgh7a7vqq3sj9d6wzq00"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-utf8-string" ,ghc-utf8-string)
       ("ghc-llvm-hs-pure" ,ghc-llvm-hs-pure)
       ("llvm" ,llvm-9)))
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-temporary" ,ghc-temporary)
       ("ghc-pretty-show" ,ghc-pretty-show)
       ("ghc-temporary" ,ghc-temporary)))
    (home-page "https://github.com/llvm-hs/llvm-hs/")
    (synopsis "General purpose LLVM bindings for Haskell")
    (description "llvm-hs is a set of Haskell bindings for LLVM.  Unlike other
current Haskell bindings, it uses an algebraic datatype (ADT) to represent LLVM
IR, and so offers two advantages: it handles almost all of the stateful
complexities of using the LLVM API to build IR; and it supports moving IR not
only from Haskell into LLVM C++ objects, but the other direction - from LLVM C++
into Haskell.")
    (license license:bsd-3)))

(define-public ghc-logging-facade
  (package
    (name "ghc-logging-facade")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "logging-facade/logging-facade-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0d0lwxxgd16is9aw6v3ps4r9prv3dj8xscmm45fvzq3nicjiawcf"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://hackage.haskell.org/package/logging-facade")
    (synopsis "Simple logging abstraction that allows multiple back-ends")
    (description
     "This package provides a simple logging abstraction that allows multiple
back-ends.")
    (license license:expat)))

(define-public ghc-logict
  (package
    (name "ghc-logict")
    (version "0.7.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/logict/logict-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1xfgdsxg0lp8m0a2cb83rcxrnnc37asfikay2kydi933anh9ihfc"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)))
    (home-page "http://code.haskell.org/~dolio/")
    (synopsis "Backtracking logic-programming monad")
    (description "This library provides a continuation-based, backtracking,
logic programming monad.  An adaptation of the two-continuation implementation
found in the paper \"Backtracking, Interleaving, and Terminating Monad
Transformers\" available @uref{http://okmij.org/ftp/papers/LogicT.pdf,
online}.")
    (license license:bsd-3)))

(define-public ghc-lucid
  (package
    (name "ghc-lucid")
    (version "2.9.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/lucid/lucid-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "156wniydd1hlb7rygbm95zln8ky8lai8rn2apkkv0rax9cdw6jrh"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-blaze-builder" ,ghc-blaze-builder)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-mmorph" ,ghc-mmorph)
       ("ghc-unordered-containers" ,ghc-unordered-containers)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-bifunctors" ,ghc-bifunctors)))
    (arguments
     `(#:cabal-revision
       ("1"
        "1f0whk5ncanxfjjanrf6rqyncig2xgc5mh2j0sqy3nrlyjr9aqq9")))
    (home-page "https://github.com/chrisdone/lucid")
    (synopsis "Haskell DSL for rendering HTML")
    (description "Clear to write, read and edit Haskell DSL for HTML.

@itemize @bullet
@item
Names are consistent, and do not conflict with base or are keywords
(all have suffix @code{-}).
@item
Same combinator can be used for attributes and elements
(e.g. @code{style_}).
@end itemize")
    (license license:bsd-3)))

(define-public ghc-lzma
  (package
    (name "ghc-lzma")
    (version "0.0.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/lzma/"
                           "lzma-" version ".tar.gz"))
       (sha256
        (base32
         "0i416gqi8j55nd1pqbkxvf3f6hn6fjys6gq98lkkxphva71j30xg"))))
    (build-system haskell-build-system)
    (arguments
     '(#:tests? #f ; requires older versions of QuickCheck and tasty.
       #:cabal-revision
       ("3" "1sify6gnsalyp6dakfzi0mdy5jcz2kcp9jsdsgkmxd40nfzgd44m")))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)))
    (home-page "https://github.com/hvr/lzma")
    (synopsis "LZMA/XZ compression and decompression")
    (description
     "This package provides a pure interface for compressing and
decompressing LZMA streams of data represented as lazy @code{ByteString}s.  A
monadic incremental interface is provided as well.")
    (license license:bsd-3)))

(define-public ghc-lzma-conduit
  (package
    (name "ghc-lzma-conduit")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/lzma-conduit/"
                           "lzma-conduit-" version ".tar.gz"))
       (sha256
        (base32
         "0hm72da7xk9l3zxjh274yg444vf405djxqbkf3q3p2qhicmxlmg9"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-conduit" ,ghc-conduit)
       ("ghc-lzma" ,ghc-lzma)
       ("ghc-resourcet" ,ghc-resourcet)))
    (native-inputs
     `(("ghc-base-compat" ,ghc-base-compat)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://github.com/alphaHeavy/lzma-conduit")
    (synopsis "Conduit interface for lzma/xz compression")
    (description
     "This package provides a @code{Conduit} interface for the LZMA
compression algorithm used in the @code{.xz} file format.")
    (license license:bsd-3)))

(define-public ghc-magic
  (package
    (name "ghc-magic")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/magic/magic-"
             version ".tar.gz"))
       (sha256
        (base32
         "10p0gjjjwr1dda7hahwrwn5njbfhl67arq3v3nf1jr3vymlkn75j"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/magic")
    (synopsis "Interface to C file/magic library")
    (description
     "This package provides a full-featured binding to the C libmagic library.
With it, you can determine the type of a file by examining its contents rather
than its name.")
    (license license:bsd-3)))

(define-public ghc-managed
  (package
    (name "ghc-managed")
    (version "1.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://hackage/package/managed/managed-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1kbrw99yh5x5blykmx2n88mplbbi4ss1ij5j17b7asw6q0ihm9zi"))))
    (build-system haskell-build-system)
    (home-page "http://hackage.haskell.org/package/managed")
    (synopsis "Monad for managed values")
    (description
     "In Haskell you very often acquire values using the with... idiom using
functions of type (a -> IO r) -> IO r.  This idiom forms a Monad, which is a
special case of the ContT monad (from transformers) or the Codensity
monad (from kan-extensions).  The main purpose behind this package is to
provide a restricted form of these monads specialized to this unusually common
case.

The reason this package defines a specialized version of these types
is to:

@itemize
@item be more beginner-friendly,
@item simplify inferred types and error messages, and:
@item provide some additional type class instances that would otherwise be
orphan instances
@end itemize")
    (license license:bsd-3)))

(define-public ghc-markdown-unlit
  (package
    (name "ghc-markdown-unlit")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://hackage/package/markdown-unlit/"
                    "markdown-unlit-" version ".tar.gz"))
              (sha256
               (base32
                "1gy79vr85vcp13rdjh0hz7zv6daqqffww4j0cqn2lpjjh9xhsbg7"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-base-compat" ,ghc-base-compat)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-silently" ,ghc-silently)
       ("ghc-stringbuilder" ,ghc-stringbuilder)
       ("ghc-temporary" ,ghc-temporary)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://github.com/sol/markdown-unlit#readme")
    (synopsis "Literate Haskell support for Markdown")
    (description "This package allows you to have a README.md that at the
same time is a literate Haskell program.")
    (license license:expat)))

(define-public ghc-math-functions
  (package
    (name "ghc-math-functions")
    (version "0.3.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "math-functions-" version "/"
                           "math-functions-" version ".tar.gz"))
       (sha256
        (base32
         "1s5nbs40sc3r4z08n0j8bw40cy0zkp03fjjn3p27zkd4fvm9kib3"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f))  ; FIXME: 1 test fails.
    (inputs
     `(("ghc-data-default-class" ,ghc-data-default-class)
       ("ghc-vector" ,ghc-vector)
       ("ghc-vector-th-unbox" ,ghc-vector-th-unbox)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-erf" ,ghc-erf)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)))
    (home-page "https://github.com/bos/math-functions")
    (synopsis "Special functions and Chebyshev polynomials for Haskell")
    (description "This Haskell library provides implementations of
special mathematical functions and Chebyshev polynomials.  These
functions are often useful in statistical and numerical computing.")
    (license license:bsd-3)))

(define-public ghc-megaparsec
  (package
    (name "ghc-megaparsec")
    (version "7.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "megaparsec/megaparsec-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0bqx1icbmk8s7wmbcdzsgnlh607c7kzg8l80cp02dxr5valjxp7j"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-parser-combinators" ,ghc-parser-combinators)
       ("ghc-scientific" ,ghc-scientific)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-hspec-expectations" ,ghc-hspec-expectations)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://github.com/mrkkrp/megaparsec")
    (synopsis "Monadic parser combinators")
    (description
     "This is an industrial-strength monadic parser combinator library.
Megaparsec is a feature-rich package that strikes a nice balance between
speed, flexibility, and quality of parse errors.")
    (license license:bsd-2)))

(define-public ghc-memory
  (package
    (name "ghc-memory")
    (version "0.14.18")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "memory/memory-" version ".tar.gz"))
              (sha256
               (base32
                "01rmq3vagxzjmm96qnfxk4f0516cn12bp5m8inn8h5r918bqsigm"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-basement" ,ghc-basement)
       ("ghc-foundation" ,ghc-foundation)))
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)))
    (home-page "https://github.com/vincenthz/hs-memory")
    (synopsis "Memory abstractions for Haskell")
    (description
     "This package provides memory abstractions, such as chunk of memory,
polymorphic byte array management and manipulation functions.  It contains a
polymorphic byte array abstraction and functions similar to strict ByteString,
different type of byte array abstraction, raw memory IO operations (memory
set, memory copy, ..) and more")
    (license license:bsd-3)))

(define-public ghc-memotrie
  (package
    (name "ghc-memotrie")
    (version "0.6.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/MemoTrie/MemoTrie-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "157p0pi6rrq74a35mq6zkkycv4ah7xhkbrcmnkb9xf7pznw4aq0x"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-newtype-generics" ,ghc-newtype-generics)))
    (home-page "https://github.com/conal/MemoTrie")
    (synopsis "Trie-based memo functions")
    (description "This package provides a functional library for creating
efficient memo functions using tries.")
    (license license:bsd-3)))

(define-public ghc-microlens
  (package
    (name "ghc-microlens")
    (version "0.4.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "microlens-" version "/"
                           "microlens-" version ".tar.gz"))
       (sha256
        (base32
         "1v277yyy4p9q57xr2lfp6qs24agglfczmcabrapxrzci3jfshmcw"))))
    (build-system haskell-build-system)
    (home-page
     "https://github.com/aelve/microlens")
    (synopsis "Provides a tiny lens Haskell library with no dependencies")
    (description "This Haskell package provides a lens library, just like
@code{ghc-lens}, but smaller.  It provides essential lenses and
traversals (like @code{_1} and @code{_Just}), as well as ones which are simply
nice to have (like @code{each}, @code{at}, and @code{ix}), and some
combinators (like @code{failing} and @code{singular}), but everything else is
stripped.  As the result, this package has no dependencies.")
    (license license:bsd-3)))

(define-public ghc-microlens-aeson
  (package
    (name "ghc-microlens-aeson")
    (version "2.3.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "microlens-aeson/microlens-aeson-"
                           version ".tar.gz"))
       (patches (search-patches "ghc-microlens-aeson-fix-tests.patch"))
       (sha256
        (base32
         "0w630kk5bnily1qh41081gqgbwmslrh5ad21899gwnb2r3jripyw"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-aeson" ,ghc-aeson)
       ("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-microlens" ,ghc-microlens)
       ("ghc-scientific" ,ghc-scientific)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-vector" ,ghc-vector)))
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)))
    (home-page "https://github.com/fosskers/microlens-aeson")
    (synopsis "Law-abiding lenses for Aeson, using microlens")
    (description "This library provides law-abiding lenses for Aeson, using
microlens.")
    (license license:expat)))

(define-public ghc-microlens-ghc
  (package
    (name "ghc-microlens-ghc")
    (version "0.4.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/microlens-ghc/microlens-ghc-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "102dbrdsdadxbbhvx8avv1wbk84767a7lkb8ckp3zxk9g7qlly33"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-microlens" ,ghc-microlens)))
    (home-page "https://github.com/monadfix/microlens")
    (synopsis "Use @code{microlens} with GHC libraries like @code{array}")
    (description "This library provides everything that @code{microlens}
provides plus instances to make @code{each}, @code{at}, and @code{ix}
usable with arrays, @code{ByteString}, and containers.  This package is
a part of the @uref{http://hackage.haskell.org/package/microlens,
microlens} family; see the readme
@uref{https://github.com/aelve/microlens#readme, on Github}.")
    (license license:bsd-3)))

(define-public ghc-microlens-mtl
  (package
    (name "ghc-microlens-mtl")
    (version "0.1.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/microlens-mtl/microlens-mtl-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0l6z1gkzwcpv89bxf5vgfrjb6gq2pj7sjjc53nvi5b9alx34zryk"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-microlens" ,ghc-microlens)
       ("ghc-transformers-compat" ,ghc-transformers-compat)))
    (home-page "https://github.com/monadfix/microlens")
    (synopsis
     "@code{microlens} support for Reader/Writer/State from mtl")
    (description
     "This package contains functions (like @code{view} or @code{+=}) which
work on @code{MonadReader}, @code{MonadWriter}, and @code{MonadState} from the
mtl package.  This package is a part of the
@uref{http://hackage.haskell.org/package/microlens, microlens} family; see the
readme @uref{https://github.com/aelve/microlens#readme, on Github}.")
    (license license:bsd-3)))

(define-public ghc-microlens-platform
  (package
    (name "ghc-microlens-platform")
    (version "0.3.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/"
             "microlens-platform/microlens-platform-" version ".tar.gz"))
       (sha256
        (base32
         "18950lxgmsg5ksvyyi3zs1smjmb1qf1q73a3p3g44bh21miz0xwb"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-hashable" ,ghc-hashable)
       ("ghc-microlens" ,ghc-microlens)
       ("ghc-microlens-ghc" ,ghc-microlens-ghc)
       ("ghc-microlens-mtl" ,ghc-microlens-mtl)
       ("ghc-microlens-th" ,ghc-microlens-th)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-vector" ,ghc-vector)))
    (home-page "https://github.com/monadfix/microlens")
    (synopsis "Feature-complete microlens")
    (description
     "This package exports a module which is the recommended starting point
for using @uref{http://hackage.haskell.org/package/microlens, microlens} if
you aren't trying to keep your dependencies minimal.  By importing
@code{Lens.Micro.Platform} you get all functions and instances from
@uref{http://hackage.haskell.org/package/microlens, microlens},
@uref{http://hackage.haskell.org/package/microlens-th, microlens-th},
@uref{http://hackage.haskell.org/package/microlens-mtl, microlens-mtl},
@uref{http://hackage.haskell.org/package/microlens-ghc, microlens-ghc}, as
well as instances for @code{Vector}, @code{Text}, and @code{HashMap}.  The
minor and major versions of @code{microlens-platform} are incremented whenever
the minor and major versions of any other @code{microlens} package are
incremented, so you can depend on the exact version of
@code{microlens-platform} without specifying the version of @code{microlens}
you need.  This package is a part of the
@uref{http://hackage.haskell.org/package/microlens, microlens} family; see the
readme @uref{https://github.com/aelve/microlens#readme, on Github}.")
    (license license:bsd-3)))

(define-public ghc-microlens-th
  (package
    (name "ghc-microlens-th")
    (version "0.4.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "microlens-th-" version "/"
                           "microlens-th-" version ".tar.gz"))
       (sha256
        (base32
         "13qw0pwcgd6f6i39rwgqwcwk1d4da5x7wv3gna7gdlxaq331h41j"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("1" "167in7b1qhgrspx81bdm2jyg9qji66sk7id282c0s99kmp0d01n6")))
    (inputs `(("ghc-microlens" ,ghc-microlens)
              ("ghc-th-abstraction" ,ghc-th-abstraction)))
    (home-page
     "https://github.com/aelve/microlens")
    (synopsis "Automatic generation of record lenses for
@code{ghc-microlens}")
    (description "This Haskell package lets you automatically generate lenses
for data types; code was extracted from the lens package, and therefore
generated lenses are fully compatible with ones generated by lens (and can be
used both from lens and microlens).")
    (license license:bsd-3)))

(define-public ghc-missingh
  (package
    (name "ghc-missingh")
    (version "1.4.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/MissingH/"
                           "MissingH-" version ".tar.gz"))
       (sha256
        (base32
         "1jp0vk6w9a7fzrbxfhx773105jp2s1n50klq9ak6spfl7bgx5v29"))))
    (build-system haskell-build-system)
    ;; Tests require the unmaintained testpack package, which depends on the
    ;; outdated QuickCheck version 2.7, which can no longer be built with
    ;; recent versions of GHC and Haskell libraries.
    (arguments '(#:tests? #f))
    (inputs
     `(("ghc-network" ,ghc-network)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-regex-compat" ,ghc-regex-compat)
       ("ghc-hslogger" ,ghc-hslogger)
       ("ghc-random" ,ghc-random)
       ("ghc-old-time" ,ghc-old-time)
       ("ghc-old-locale" ,ghc-old-locale)))
    (native-inputs
     `(("ghc-errorcall-eq-instance" ,ghc-errorcall-eq-instance)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-hunit" ,ghc-hunit)))
    ;; ‘Official’ <http://software.complete.org/missingh> redirects to a 404.
    (home-page "https://github.com/haskell-hvr/missingh")
    (synopsis "Large utility library")
    (description
     "MissingH is a library of all sorts of utility functions for Haskell
programmers.  It is written in pure Haskell and thus should be extremely
portable and easy to use.")
    (license license:bsd-3)))

(define-public ghc-mmap
  (package
    (name "ghc-mmap")
    (version "0.5.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "mmap/mmap-" version ".tar.gz"))
              (sha256
               (base32
                "1y5mk3yf4b8r6rzmlx1xqn4skaigrqnv08sqq0v7r3nbw42bpz2q"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/mmap")
    (synopsis "Memory mapped files for Haskell")
    (description
     "This library provides a wrapper to @code{mmap}, allowing files or
devices to be lazily loaded into memory as strict or lazy @code{ByteStrings},
@code{ForeignPtrs} or plain @code{Ptrs}, using the virtual memory subsystem to
do on-demand loading.")
    (license license:bsd-3)))

(define-public ghc-mmorph
  (package
    (name "ghc-mmorph")
    (version "1.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/mmorph/mmorph-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0rfsy9n9mlinpmqi2s17fhc67fzma2ig5fbmh6m5m830canzf8vr"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-transformers-compat" ,ghc-transformers-compat)))
    (home-page "https://hackage.haskell.org/package/mmorph")
    (synopsis "Monad morphisms")
    (description
     "This library provides monad morphism utilities, most commonly used for
manipulating monad transformer stacks.")
    (license license:bsd-3)))

(define-public ghc-mockery
  (package
    (name "ghc-mockery")
    (version "0.3.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "mockery/mockery-" version ".tar.gz"))
              (sha256
               (base32
                "09ypgm3z69gq8mj6y66ss58kbjnk15r8frwcwbqcfbfksfnfv8dp"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-temporary" ,ghc-temporary)
       ("ghc-logging-facade" ,ghc-logging-facade)
       ("ghc-base-compat" ,ghc-base-compat)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://hackage.haskell.org/package/mockery")
    (synopsis "Support functions for automated testing")
    (description
     "The mockery package provides support functions for automated testing.")
    (license license:expat)))

(define-public ghc-monad-control
  (package
    (name "ghc-monad-control")
    (version "1.0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/monad-control"
             "/monad-control-" version ".tar.gz"))
       (sha256
        (base32
         "1c92833gr6cadidjdp8mlznkpp8lyxl0w3y7d19y8yi3klc3843c"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-transformers-base" ,ghc-transformers-base)
       ("ghc-transformers-compat" ,ghc-transformers-compat)))
    (home-page "https://github.com/basvandijk/monad-control")
    (synopsis "Monad transformers to lift control operations like exception
catching")
    (description "This package defines the type class @code{MonadBaseControl},
a subset of @code{MonadBase} into which generic control operations such as
@code{catch} can be lifted from @code{IO} or any other base monad.")
    (license license:bsd-3)))

(define-public ghc-monad-logger
  (package
    (name "ghc-monad-logger")
    (version "0.3.30")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "monad-logger-" version "/"
                           "monad-logger-" version ".tar.gz"))
       (sha256
        (base32
         "102l0v75hbvkmrypiyg4ybb6rbc7nij5nxs1aihmqfdpg04rkkp7"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-transformers-compat" ,ghc-transformers-compat)
              ("ghc-stm-chans" ,ghc-stm-chans)
              ("ghc-lifted-base" ,ghc-lifted-base)
              ("ghc-resourcet" ,ghc-resourcet)
              ("ghc-conduit" ,ghc-conduit)
              ("ghc-conduit-extra" ,ghc-conduit-extra)
              ("ghc-fast-logger" ,ghc-fast-logger)
              ("ghc-transformers-base" ,ghc-transformers-base)
              ("ghc-monad-control" ,ghc-monad-control)
              ("ghc-monad-loops" ,ghc-monad-loops)
              ("ghc-blaze-builder" ,ghc-blaze-builder)
              ("ghc-exceptions" ,ghc-exceptions)))
    (home-page "https://github.com/kazu-yamamoto/logger")
    (synopsis "Provides a class of monads which can log messages for Haskell")
    (description "This Haskell package uses a monad transformer approach
for logging.

This package provides Template Haskell functions for determining source
code locations of messages.")
    (license license:expat)))

(define-public ghc-monad-loops
  (package
    (name "ghc-monad-loops")
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "monad-loops-" version "/"
                           "monad-loops-" version ".tar.gz"))
       (sha256
        (base32
         "062c2sn3hc8h50p1mhqkpyv6x8dydz2zh3ridvlfjq9nqimszaky"))))
    (build-system haskell-build-system)
    (native-inputs `(("ghc-tasty" ,ghc-tasty)
                     ("ghc-tasty-hunit" ,ghc-tasty-hunit)))
    (home-page "https://github.com/mokus0/monad-loops")
    (synopsis "Monadic loops for Haskell")
    (description "This Haskell package provides some useful control
operators for looping.")
    (license license:public-domain)))

(define-public ghc-monad-par
  (package
    (name "ghc-monad-par")
    (version "0.3.4.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "monad-par-" version "/"
                           "monad-par-" version ".tar.gz"))
       (patches (search-patches "ghc-monad-par-fix-tests.patch"))
       (sha256
        (base32
         "0ldrzqy24fsszvn2a2nr77m2ih7xm0h9bgkjyv1l274aj18xyk7q"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-abstract-par" ,ghc-abstract-par)
              ("ghc-abstract-deque" ,ghc-abstract-deque)
              ("ghc-monad-par-extras" ,ghc-monad-par-extras)
              ("ghc-mwc-random" ,ghc-mwc-random)
              ("ghc-parallel" ,ghc-parallel)))
    (native-inputs `(("ghc-quickcheck" ,ghc-quickcheck)
                     ("ghc-hunit" ,ghc-hunit)
                     ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
                     ("ghc-test-framework-quickcheck2"
                      ,ghc-test-framework-quickcheck2)
                     ("ghc-test-framework" ,ghc-test-framework)
                     ("ghc-test-framework-th" ,ghc-test-framework-th)))
    (home-page "https://github.com/simonmar/monad-par")
    (synopsis "Haskell library for parallel programming based on a monad")
    (description "The @code{Par} monad offers an API for parallel
programming.  The library works for parallelising both pure and @code{IO}
computations, although only the pure version is deterministic.  The default
implementation provides a work-stealing scheduler and supports forking tasks
that are much lighter weight than IO-threads.")
    (license license:bsd-3)))

(define-public ghc-monad-par-extras
  (package
    (name "ghc-monad-par-extras")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "monad-par-extras-" version "/"
                           "monad-par-extras-" version ".tar.gz"))
       (sha256
        (base32
         "0bl4bd6jzdc5zm20q1g67ppkfh6j6yn8fwj6msjayj621cck67p2"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-abstract-par" ,ghc-abstract-par)
              ("ghc-cereal" ,ghc-cereal)
              ("ghc-random" ,ghc-random)))
    (home-page "https://github.com/simonmar/monad-par")
    (synopsis "Combinators and extra features for Par monads for Haskell")
    (description "This Haskell package provides additional data structures,
and other added capabilities layered on top of the @code{Par} monad.")
    (license license:bsd-3)))

(define-public ghc-monadrandom
  (package
    (name "ghc-monadrandom")
    (version "0.5.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "MonadRandom-" version "/"
                           "MonadRandom-" version ".tar.gz"))
       (sha256
        (base32
         "0w44jl1n3kqvqaflh82l1wj3xxbhzfs3kf4m8rk7w6fgg8llmnmb"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-transformers-compat" ,ghc-transformers-compat)
              ("ghc-primitive" ,ghc-primitive)
              ("ghc-fail" ,ghc-fail)
              ("ghc-random" ,ghc-random)))
    (home-page "https://github.com/byorgey/MonadRandom")
    (synopsis "Random-number generation monad for Haskell")
    (description "This Haskell package provides support for computations
which consume random values.")
    (license license:bsd-3)))

(define-public ghc-monads-tf
  (package
    (name "ghc-monads-tf")
    (version "0.1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/monads-tf/monads-tf-"
             version ".tar.gz"))
       (sha256
        (base32
         "1wdhskwa6dw8qljbvwpyxj8ca6y95q2np7z4y4q6bpf4anmd5794"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/monads-tf")
    (synopsis "Monad classes, using type families")
    (description
     "Monad classes using type families, with instances for various monad transformers,
inspired by the paper 'Functional Programming with Overloading and Higher-Order
Polymorphism', by Mark P Jones.  This package is almost a compatible replacement for
the @code{mtl-tf} package.")
    (license license:bsd-3)))

(define-public ghc-mono-traversable
  (package
    (name "ghc-mono-traversable")
    (version "1.0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "mono-traversable-" version "/"
                           "mono-traversable-" version ".tar.gz"))
       (sha256
        (base32
         "1bqy982lpdb83lacfy76n8kqw5bvd31avxj25kg8gkgycdh0g0ma"))))
    (build-system haskell-build-system)
    (outputs '("out" "static" "doc"))
    (inputs `(("ghc-unordered-containers" ,ghc-unordered-containers)
              ("ghc-hashable" ,ghc-hashable)
              ("ghc-vector" ,ghc-vector)
              ("ghc-vector-algorithms" ,ghc-vector-algorithms)
              ("ghc-split" ,ghc-split)))
    (native-inputs `(("ghc-hspec" ,ghc-hspec)
                     ("ghc-hunit" ,ghc-hunit)
                     ("ghc-quickcheck" ,ghc-quickcheck)
                     ("ghc-semigroups" ,ghc-semigroups)
                     ("ghc-foldl" ,ghc-foldl)))
    (home-page "https://github.com/snoyberg/mono-traversable")
    (synopsis "Haskell classes for mapping, folding, and traversing monomorphic
containers")
    (description "This Haskell package provides Monomorphic variants of the
Functor, Foldable, and Traversable typeclasses.  If you understand Haskell's
basic typeclasses, you understand mono-traversable.  In addition to what
you are used to, it adds on an IsSequence typeclass and has code for marking
data structures as non-empty.")
    (license license:expat)))

(define-public ghc-monoid-extras
  (package
    (name "ghc-monoid-extras")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "monoid-extras/monoid-extras-" version ".tar.gz"))
       (sha256
        (base32
         "0xfrkgqn9d31z54l617m3w3kkd5m9vjb4yl247r3zzql3mpb1f37"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-groups" ,ghc-groups)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-semigroupoids" ,ghc-semigroupoids)))
    (arguments
     `(#:cabal-revision
       ("1" "0b8x5d6vh7mpigvjvcd8f38a1nyzn1vfdqypslw7z9fgsr742913")))
    (home-page "https://hackage.haskell.org/package/monoid-extras")
    (synopsis "Various extra monoid-related definitions and utilities")
    (description "This package provides various extra monoid-related
definitions and utilities, such as monoid actions, monoid coproducts,
semi-direct products, \"deletable\" monoids, \"split\" monoids, and
\"cut\" monoids.")
    (license license:bsd-3)))

(define-public ghc-mountpoints
  (package
    (name "ghc-mountpoints")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/mountpoints/mountpoints-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1hnm31pqcffphyc463wf0vbik9fzm5lb2r4wjdc1y4dqzmjdzz37"))))
    (build-system haskell-build-system)
    (home-page
     "http://hackage.haskell.org/package/mountpoints")
    (synopsis "Haskell library for listing mount points")
    (description "This library provides Haskell bindings for checking
currently mounted filesystems.")
    (license license:lgpl2.1+)))

(define-public ghc-mtl-compat
  (package
    (name "ghc-mtl-compat")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://hackage/package/mtl-compat/mtl-compat-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "17iszr5yb4f17g8mq6i74hsamii8z6m2qfsmgzs78mhiwa7kjm8r"))))
    (build-system haskell-build-system)
    (home-page
     "https://github.com/haskell-compat/mtl-compat")
    (synopsis
     "Backported Control.Monad.Except module from mtl")
    (description
     "This package backports the Control.Monad.Except module from mtl (if
using mtl-2.2.0.1 or earlier), which reexports the ExceptT monad transformer
and the MonadError class.

This package should only be used if there is a need to use the
Control.Monad.Except module specifically.  If you just want the mtl class
instances for ExceptT, use transformers-compat instead, since mtl-compat does
nothing but reexport the instances from that package.

Note that unlike how mtl-2.2 or later works, the Control.Monad.Except
module defined in this package exports all of ExceptT's monad class instances.
Therefore, you may have to declare @code{import Control.Monad.Except ()} at
the top of your file to get all of the ExceptT instances in scope.")
    (license license:bsd-3)))

(define-public ghc-murmur-hash
  (package
    (name "ghc-murmur-hash")
    (version "0.1.0.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/murmur-hash"
                           "/murmur-hash-" version ".tar.gz"))
       (sha256
        (base32 "1bb58kfnzvx3mpc0rc0dhqc1fk36nm8prd6gvf20gk6lxaadpfc9"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/nominolo/murmur-hash")
    (synopsis "MurmurHash2 implementation for Haskell")
    (description
     "This package provides an implementation of MurmurHash2, a good, fast,
general-purpose, non-cryptographic hashing function.  See
@url{https://sites.google.com/site/murmurhash/} for details.  This
implementation is pure Haskell, so it might be a bit slower than a C FFI
binding.")
    (license license:bsd-3)))

(define-public ghc-mwc-random
  (package
    (name "ghc-mwc-random")
    (version "0.14.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "mwc-random-" version "/"
                           "mwc-random-" version ".tar.gz"))
       (sha256
        (base32
         "18pg24sw3b79b32cwx8q01q4k0lm34mwr3l6cdkchl8alvd0wdq0"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-primitive" ,ghc-primitive)
       ("ghc-vector" ,ghc-vector)
       ("ghc-math-functions" ,ghc-math-functions)))
    (arguments
     `(#:tests? #f)) ; FIXME: Test-Suite `spec` fails.
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)))
    (home-page "https://github.com/bos/mwc-random")
    (synopsis "Random number generation library for Haskell")
    (description "This Haskell package contains code for generating
high quality random numbers that follow either a uniform or normal
distribution.  The generated numbers are suitable for use in
statistical applications.

The uniform PRNG uses Marsaglia's MWC256 (also known as MWC8222)
multiply-with-carry generator, which has a period of 2^{8222} and
fares well in tests of randomness.  It is also extremely fast,
between 2 and 3 times faster than the Mersenne Twister.")
    (license license:bsd-3)))

(define-public ghc-nats
  (package
    (name "ghc-nats")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/nats/nats-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1v40drmhixck3pz3mdfghamh73l4rp71mzcviipv1y8jhrfxilmr"))))
    (build-system haskell-build-system)
    (arguments `(#:haddock? #f))
    (inputs
     `(("ghc-hashable" ,ghc-hashable)))
    (home-page "https://hackage.haskell.org/package/nats")
    (synopsis "Natural numbers")
    (description "This library provides the natural numbers for Haskell.")
    (license license:bsd-3)))

(define-public ghc-nats-bootstrap
  (package
    (inherit ghc-nats)
    (name "ghc-nats-bootstrap")
    (inputs
     `(("ghc-hashable" ,ghc-hashable-bootstrap)))
    (properties '((hidden? #t)))))

(define-public ghc-ncurses
  (package
    (name "ghc-ncurses")
    (version "0.2.16")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://hackage.haskell.org/package/ncurses/ncurses-"
               version ".tar.gz"))
        (sha256
         (base32
          "0gsyyaqyh5r9zc0rhwpj5spyd6i4w2vj61h4nihgmmh0yyqvf3z5"))))
    (build-system haskell-build-system)
    (arguments
     '(#:extra-directories ("ncurses")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-includes
           (lambda _
             (substitute* '("cbits/hsncurses-shim.h"
                            "lib/UI/NCurses.chs"
                            "lib/UI/NCurses/Enums.chs"
                            "lib/UI/NCurses/Panel.chs")
               (("<ncursesw/") "<"))
             #t)))
       #:cabal-revision
       ("1"
        "1wfdy716s5p1sqp2gsg43x8wch2dxg0vmbbndlb2h3d8c9jzxnca")))
    (inputs `(("ncurses" ,ncurses)))
    (native-inputs `(("ghc-c2hs" ,ghc-c2hs)))
    (home-page "https://john-millikin.com/software/haskell-ncurses/")
    (synopsis "Modernised bindings to GNU ncurses")
    (description "GNU ncurses is a library for creating command-line application
with pseudo-graphical interfaces.  This package is a nice, modern binding to GNU
ncurses.")
    (license license:gpl3)))

(define-public ghc-network
  (package
    (name "ghc-network")
    (version "2.8.0.1")
    (outputs '("out" "static" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/network/network-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0im8k51rw3ahmr23ny10pshwbz09jfg0fdpam0hzf2hgxnzmvxb1"))))
    (build-system haskell-build-system)
    ;; The regression tests depend on an unpublished module.
    (arguments `(#:tests? #f))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-doctest" ,ghc-doctest)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)))
    (home-page "https://github.com/haskell/network")
    (synopsis "Low-level networking interface")
    (description
     "This package provides a low-level networking interface.")
    (license license:bsd-3)))

(define-public ghc-network-bsd
  (package
    (name "ghc-network-bsd")
    (version "2.8.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "network-bsd/network-bsd-" version ".tar.gz"))
       (sha256
        (base32
         "0dfbwgrr28y6ypw7p1ppqg7v746qf14569q4xazj4ahdjw2xkpi5"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-network" ,ghc-network)))
    (home-page "https://github.com/haskell/network-bsd")
    (synopsis "POSIX network database (<netdb.h>) API")
    (description "This package provides Haskell bindings to the the POSIX
network database (<netdb.h>) API.")
    (license license:bsd-3)))

(define-public ghc-network-byte-order
  (package
    (name "ghc-network-byte-order")
    (version "0.1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "network-byte-order/network-byte-order-"
                           version ".tar.gz"))
       (sha256
        (base32
         "19cs6157amcc925vwr92q1azwwzkbam5g0k70i6qi80fhpikh37c"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-doctest" ,ghc-doctest)))
    (home-page "https://hackage.haskell.org/package/network-byte-order")
    (synopsis "Network byte order utilities")
    (description "This library provides peek and poke functions for network
byte order.")
    (license license:bsd-3)))

(define-public ghc-network-info
  (package
    (name "ghc-network-info")
    (version "0.2.0.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "network-info-" version "/"
                           "network-info-" version ".tar.gz"))
       (sha256
        (base32
         "0anmgzcpnz7nw3n6vq0r25m1s9l2svpwi83wza0lzkrlbnbzd02n"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/jystic/network-info")
    (synopsis "Access the local computer's basic network configuration")
    (description "This Haskell library provides simple read-only access to the
local computer's networking configuration.  It is currently capable of
getting a list of all the network interfaces and their respective
IPv4, IPv6 and MAC addresses.")
    (license license:bsd-3)))

(define-public ghc-network-multicast
  (package
    (name "ghc-network-multicast")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/network-multicast/network-multicast-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0whvi0pbwjy6dbwfdf9rv1j3yr3lcmfp3q7a8pwq63g537l4l2l3"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-network" ,ghc-network)
       ("ghc-network-bsd" ,ghc-network-bsd)))
    (home-page
     "http://hackage.haskell.org/package/network-multicast")
    (synopsis "Simple multicast library for Haskell")
    (description
     "This package provides the Network.Multicast Haskell module for
sending UDP datagrams over multicast (class D) addresses.")
    ;; Note that this is technically under CC0 1.0 and Expat, though it's not
    ;; totally clear what the breakdown is.  Since CC0 1.0 is effectively
    ;; "public domain with a minimal fallback license", figuring marking it
    ;; as effectively Expat is probably correct.
    (license license:expat)))

(define-public ghc-network-uri
  (package
    (name "ghc-network-uri")
    (version "2.6.1.0")
    (outputs '("out" "static" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/network-uri/network-uri-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1w27zkvn39kjr9lmw9421y8w43h572ycsfafsb7kyvr3a4ihlgj2"))))
    (build-system haskell-build-system)
    (arguments
     `(#:tests? #f))  ; FIXME: currently missing libraries used for tests.
    (inputs
     `(("ghc-network" ,ghc-network)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)))
    (home-page
     "https://github.com/haskell/network-uri")
    (synopsis "Library for URI manipulation")
    (description "This package provides an URI manipulation interface.  In
@code{network-2.6} the @code{Network.URI} module was split off from the
@code{network} package into this package.")
    (license license:bsd-3)))

(define-public ghc-newtype-generics
  (package
    (name "ghc-newtype-generics")
    (version "0.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "newtype-generics/newtype-generics-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0cprfg4n0z62cnix1qrbc79bfdd4s50b05fj9m9hk6vm1pc3szq0"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://github.com/sjakobi/newtype-generics")
    (synopsis "Typeclass and set of functions for working with newtypes")
    (description "The @code{Newtype} typeclass represents the packing and
unpacking of a newtype, and allows you to operate under that newtype with
functions such as @code{ala}.  Generics support was added in version 0.4,
making this package a full replacement for the original newtype package,
and an alternative to newtype-th.")
    (license license:bsd-3)))

(define-public ghc-non-negative
  (package
    (name "ghc-non-negative")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://hackage.haskell.org/package/non-negative/non-negative-"
         version ".tar.gz"))
       (sha256
        (base32
         "0f01q916dzkl1i0v15qrw9cviycki5g3fgi6x8gs45iwbzssq52n"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-semigroups" ,ghc-semigroups)
       ("ghc-utility-ht" ,ghc-utility-ht)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://hackage.haskell.org/package/non-negative")
    (synopsis "Non-negative numbers class")
    (description "This library provides a class for non-negative numbers,
a wrapper which can turn any ordered numeric type into a member of that
class, and a lazy number type for non-negative numbers (a generalization
of Peano numbers).")
    (license license:gpl3+)))

(define-public ghc-nonce
  (package
    (name "ghc-nonce")
    (version "1.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/nonce/"
             "nonce-" version ".tar.gz"))
       (sha256
        (base32
         "1q9ph0aq51mvdvydnriqd12sfin36pfb8f588zgac1ybn8r64ksb"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-base64-bytestring" ,ghc-base64-bytestring)
       ("ghc-entropy" ,ghc-entropy)
       ("ghc-unliftio" ,ghc-unliftio)
       ("ghc-unliftio-core" ,ghc-unliftio-core)))
    (home-page "https://github.com/prowdsponsor/nonce")
    (synopsis "Generate cryptographic nonces in Haskell")
    (description
     "A nonce is an arbitrary number used only once in a cryptographic
communication.  This package contain helper functions for generating nonces.
There are many kinds of nonces used in different situations.  It's not
guaranteed that by using the nonces from this package you won't have any
security issues.  Please make sure that the nonces generated via this
package are usable on your design.")
    (license license:bsd-3)))

(define-public ghc-numeric-extras
  (package
    (name "ghc-numeric-extras")
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "numeric-extras/numeric-extras-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1mk11c0gz1yjy5b8dvq6czfny57pln0bs7x28fz38qyr44872067"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/ekmett/numeric-extras")
    (synopsis "Useful tools from the C standard library")
    (description "This library provides some useful tools from the C
standard library.")
    (license license:bsd-3)))

(define-public ghc-objectname
  (package
    (name "ghc-objectname")
    (version "1.1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/ObjectName/ObjectName-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "046jm94rmm46cicd31pl54vdvfjvhd9ffbfycy2lxzc0fliyznvj"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/ObjectName")
    (synopsis "Helper library for Haskell OpenGL")
    (description "This tiny package contains the class ObjectName, which
corresponds to the general notion of explicitly handled identifiers for API
objects, e.g. a texture object name in OpenGL or a buffer object name in
OpenAL.")
    (license license:bsd-3)))

(define-public ghc-old-locale
  (package
    (name "ghc-old-locale")
    (version "1.0.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/old-locale/old-locale-"
             version
             ".tar.gz"))
       (sha256
        (base32 "0l3viphiszvz5wqzg7a45zp40grwlab941q5ay29iyw8p3v8pbyv"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("2" "04b9vn007hlvsrx4ksd3r8r3kbyaj2kvwxchdrmd4370qzi8p6gs")))
    (home-page "https://hackage.haskell.org/package/old-locale")
    (synopsis "Adapt to locale conventions")
    (description
     "This package provides the ability to adapt to locale conventions such as
date and time formats.")
    (license license:bsd-3)))

(define-public ghc-old-time
  (package
    (name "ghc-old-time")
    (version "1.1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/old-time/old-time-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1h9b26s3kfh2k0ih4383w90ibji6n0iwamxp6rfp2lbq1y5ibjqw"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("2" "1j6ln1dkvhdvnwl33bp0xf9lhc4sybqk0aw42p8cq81xwwzbn7y9")))
    (inputs
     `(("ghc-old-locale" ,ghc-old-locale)))
    (home-page "https://hackage.haskell.org/package/old-time")
    (synopsis "Time compatibility library for Haskell")
    (description "Old-time is a package for backwards compatibility with the
old @code{time} library.  For new projects, the newer
@uref{https://hackage.haskell.org/package/time, time library} is recommended.")
    (license license:bsd-3)))

(define-public ghc-only
  (package
    (name "ghc-only")
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/Only/Only-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0rdj3a629fk2vp121jq8mf2smkblrz5w3cxhlsyx6my2x29s2ymb"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("1"
        "1ahk7p34kmh041mz7lyc10nhcxgv2i4z8nvzxvqm2x34gslmsbzr")))
    (home-page "https://hackage.haskell.org/package/Only")
    (synopsis "The 1-tuple type or single-value collection")
    (description
     "This package provides a canonical anonymous 1-tuple type missing from
Haskell for attaching typeclass instances.

There is also the @url{https://hackage.haskell.org/package/OneTuple, OneTuple
package} which by using a boxed @code{data}-type provides a 1-tuple type which
has laziness properties which are more faithful to the ones of Haskell's
native tuples; whereas the primary purpose of @code{Only} is to provide the
traditionally so named type-wrapper for attaching typeclass instances.")
    (license license:bsd-3)))

(define-public ghc-opengl
  (package
    (name "ghc-opengl")
    (version "3.0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/OpenGL/OpenGL-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "069fg8jcxqq2z9iikynd8vi3jxm2b5y3qywdh4bdviyzab3zy1as"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("1" "1748mrb6r9mpf5jbrx436lwbg8w6dadyy8dhxw2dwnrj5z7zf741")))
    (inputs
     `(("ghc-objectname" ,ghc-objectname)
       ("ghc-gluraw" ,ghc-gluraw)
       ("ghc-statevar" ,ghc-statevar)
       ("ghc-openglraw" ,ghc-openglraw)))
    (home-page "https://wiki.haskell.org/Opengl")
    (synopsis "Haskell bindings for the OpenGL graphics system")
    (description "This package provides Haskell bindings for the OpenGL
graphics system (GL, version 4.5) and its accompanying utility library (GLU,
version 1.3).")
    (license license:bsd-3)))

(define-public ghc-openglraw
  (package
    (name "ghc-openglraw")
    (version "3.3.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/OpenGLRaw/OpenGLRaw-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0zgllb4bcash2i2cispa3j565aw3dpxs41ghmhpvyvi4a6xmyldx"))))
    (build-system haskell-build-system)
    (arguments
     `(#:extra-directories ("glu")))
    (inputs
     `(("ghc-half" ,ghc-half)
       ("ghc-fixed" ,ghc-fixed)
       ("glu" ,glu)))
    (home-page "https://wiki.haskell.org/Opengl")
    (synopsis "Raw Haskell bindings for the OpenGL graphics system")
    (description "OpenGLRaw is a raw Haskell binding for the OpenGL 4.5
graphics system and lots of OpenGL extensions.  It is basically a 1:1 mapping
of OpenGL's C API, intended as a basis for a nicer interface.  OpenGLRaw
offers access to all necessary functions, tokens and types plus a general
facility for loading extension entries.  The module hierarchy closely mirrors
the naming structure of the OpenGL extensions, making it easy to find the
right module to import.  All API entries are loaded dynamically, so no special
C header files are needed for building this package.  If an API entry is not
found at runtime, a userError is thrown.")
    (license license:bsd-3)))

(define-public ghc-operational
  (package
    (name "ghc-operational")
    (version "0.2.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/operational/"
                           "operational-" version ".tar.gz"))
       (sha256
        (base32
         "1x2abg2q9d26h1vzj40r6k7k3gqgappbs4g9d853vvg77837km4i"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-random" ,ghc-random)))
    (home-page "http://wiki.haskell.org/Operational")
    (synopsis "Implementation of difficult monads made easy with operational semantics")
    (description
     "This library makes it easy to implement monads with tricky control
flow. This is useful for: writing web applications in a sequential style,
programming games with a uniform interface for human and AI players and easy
replay capababilities, implementing fast parser monads, designing monadic
DSLs, etc.")
    (license license:bsd-3)))

(define-public ghc-optional-args
  (package
    (name "ghc-optional-args")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://hackage/package/optional-args/optional-args-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1r5hhn6xvc01grggxdyy48daibwzi0aikgidq0ahpa6bfynm8d1f"))))
    (build-system haskell-build-system)
    (home-page
     "http://hackage.haskell.org/package/optional-args")
    (synopsis "Optional function arguments")
    (description
     "This library provides a type for specifying @code{Optional} function
arguments.")
    (license license:bsd-3)))

(define-public ghc-options
  (package
    (name "ghc-options")
    (version "1.2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/options/options-"
             version ".tar.gz"))
       (sha256
        (base32
         "0qjs0v1ny52w51n5582d4z8wy9h6n0zw1xb5dh686ff5wadflgi8"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'update-constraints
           (lambda _
             (substitute* "options.cabal"
               (("chell >= 0\\.4 && < 0\\.5") "chell >= 0.4 && < 0.6"))
             #t)))))
    (inputs
     `(("ghc-monads-tf" ,ghc-monads-tf)
       ("ghc-chell" ,ghc-chell)
       ("ghc-chell-quickcheck" ,ghc-chell-quickcheck)))
    (home-page "https://john-millikin.com/software/haskell-options/")
    (synopsis "Powerful and easy-to-use command-line option parser")
    (description
     "The @code{options} package lets library and application developers
easily work with command-line options.")
    (license license:expat)))

;; See ghc-system-filepath-bootstrap, chell and chell-quickcheck are required for tests.
(define ghc-options-bootstrap
  (package
    (name "ghc-options-bootstrap")
    (version "1.2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/options/options-"
             version ".tar.gz"))
       (sha256
        (base32
         "0qjs0v1ny52w51n5582d4z8wy9h6n0zw1xb5dh686ff5wadflgi8"))))
    (build-system haskell-build-system)
    (arguments
     `(#:tests? #f))
    (inputs
     `(("ghc-monads-tf" ,ghc-monads-tf)))
    (home-page "https://john-millikin.com/software/haskell-options/")
    (synopsis "Powerful and easy-to-use command-line option parser")
    (description
     "The @code{options} package lets library and application developers
easily work with command-line options.")
    (license license:expat)))


(define-public ghc-optparse-applicative
  (package
    (name "ghc-optparse-applicative")
    (version "0.14.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/optparse-applicative"
             "/optparse-applicative-" version ".tar.gz"))
       (sha256
        (base32
         "0qvn1s7jwrabbpmqmh6d6iafln3v3h9ddmxj2y4m0njmzq166ivj"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("2" "1a08dqjly1xy730f6jf45frr8g8gap0n1vg9b0mpzpydv0kgzmrp")))
    (inputs
     `(("ghc-transformers-compat" ,ghc-transformers-compat)
       ("ghc-ansi-wl-pprint" ,ghc-ansi-wl-pprint)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://github.com/pcapriotti/optparse-applicative")
    (synopsis "Utilities and combinators for parsing command line options")
    (description "This package provides utilities and combinators for parsing
command line options in Haskell.")
    (license license:bsd-3)))

(define-public ghc-jira-wiki-markup
  (package
    (name "ghc-jira-wiki-markup")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/jira-wiki-markup/"
             "jira-wiki-markup-" version ".tar.gz"))
       (sha256
        (base32 "1sl2jjcsqg61si33mxjwpf8zdn56kbbgcwqqqzbgifx2qbv4wmf8"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)))
    (home-page "https://github.com/tarleb/jira-wiki-markup")
    (synopsis "Handle Jira wiki markup")
    (description
     "Parse jira wiki text into an abstract syntax tree for easy transformation
to other formats.")
    (license license:expat)))

(define-public ghc-emojis
  (package
    (name "ghc-emojis")
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/emojis/"
             "emojis-" version ".tar.gz"))
       (sha256
        (base32 "1c6zkj9gmk1y90gbdrn50hyp7mw1mggzhnr2khqd728ryipw60ss"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)))
    (home-page "https://github.com/jgm/emojis#readme")
    (synopsis "Conversion between emoji characters and their names.")
    (description
     "This package provides functions for converting emoji names to emoji
characters and vice versa.

How does it differ from the @code{emoji} package?
@itemize
@item It supports a fuller range of emojis, including all those supported by
GitHub
@item It supports lookup of emoji aliases from emoji
@item It uses Text rather than String
@item It has a lighter dependency footprint: in particular, it does not
require aeson
@item It does not require TemplateHaskell
@end itemize")
    (license license:bsd-3)))

(define-public ghc-text-conversions
  (package
    (name "ghc-text-conversions")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/text-conversions/"
             "text-conversions-" version ".tar.gz"))
       (sha256
        (base32 "089c56vdj9xysqfr1hnvbnrghlg83q6w10xk02gflpsidcpvwmhp"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-base16-bytestring" ,ghc-base16-bytestring)
       ("ghc-base64-bytestring" ,ghc-base64-bytestring)
       ("ghc-errors" ,ghc-errors)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://github.com/cjdev/text-conversions#readme")
    (synopsis "Safe conversions between textual types")
    (description "Safe conversions between textual types")
    (license license:isc)))

(define-public ghc-text-short
  (package
    (name "ghc-text-short")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/text-short/text-short-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0xyrxlb602z8bc9sr2y1fag0x56a20yj5qrkvy7iwc6hnznrynxz"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-hashable" ,ghc-hashable)))
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-quickcheck-instances"
        ,ghc-quickcheck-instances)))
    (arguments
     `(#:cabal-revision
       ("2"
        "17cb7p0qywf2dsrq3g8qb3ssknd9wl5k0nc2pxz9gc3l8rxpkw51")))
    (home-page "https://hackage.haskell.org/package/text-short")
    (synopsis "Memory-efficient representation of Unicode text strings")
    (description "This package provides the @code{ShortText} type which
is suitable for keeping many short strings in memory.  This is similar
to how @code{ShortByteString} relates to @code{ByteString}.

The main difference between @code{Text} and @code{ShortText} is that
@code{ShortText} uses UTF-8 instead of UTF-16 internally and also doesn't
support zero-copy slicing (thereby saving 2 words).  Consequently, the memory
footprint of a (boxed) @{ShortText} value is 4 words (2 words when unboxed)
plus the length of the UTF-8 encoded payload.")
    (license license:bsd-3)))

(define-public ghc-doclayout
  (package
    (name "ghc-doclayout")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/doclayout/"
             "doclayout-" version ".tar.gz"))
       (sha256
        (base32 "1wmnwq28jcyd6c80srivsnd5znmyl9sgmwwnlk2crwiiwqadbal7"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-safe" ,ghc-safe)))
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-golden" ,ghc-tasty-golden)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)))
    (home-page "https://github.com/jgm/doclayout")
    (synopsis "Pretty-printing library for laying out text documents")
    (description
     "doclayout is a pretty-printing library for laying out text documents,
with several features not present in pretty-printing libraries designed for
code.  It was designed for use in @code{Pandoc}.")
    (license license:bsd-3)))

(define-public ghc-pandoc
  (package
    (name "ghc-pandoc")
    (version "2.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/pandoc/pandoc-"
                           version ".tar.gz"))
       (patches (search-patches "ghc-pandoc-fix-html-tests.patch"
                                "ghc-pandoc-fix-latex-test.patch"))
       (sha256
        (base32
         "0dpjrr40h54cljzhvixyym07z792a9izg6b9dmqpjlgcg4rj0xx8"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'find-library
           (lambda _
             (substitute* "test/Tests/Command.hs"
               (("= dynlibEnv")
                (format #f "= [(\"LD_LIBRARY_PATH\" , \"~a/dist/build\")]"
                        (getcwd))))
             #t)))))
    (outputs '("out" "static" "doc"))
    (inputs
     `(("ghc-aeson" ,ghc-aeson)
       ("ghc-aeson-pretty" ,ghc-aeson-pretty)
       ("ghc-base64-bytestring" ,ghc-base64-bytestring)
       ("ghc-blaze-html" ,ghc-blaze-html)
       ("ghc-blaze-markup" ,ghc-blaze-markup)
       ("ghc-cmark-gfm" ,ghc-cmark-gfm)
       ("ghc-data-default" ,ghc-data-default)
       ("ghc-diff" ,ghc-diff)
       ("ghc-doctemplates" ,ghc-doctemplates)
       ("ghc-executable-path" ,ghc-executable-path)
       ("ghc-glob" ,ghc-glob)
       ("ghc-haddock-library" ,ghc-haddock-library)
       ("ghc-hslua" ,ghc-hslua)
       ("ghc-hslua-module-system" ,ghc-hslua-module-system)
       ("ghc-hslua-module-text" ,ghc-hslua-module-text)
       ("ghc-hsyaml" ,ghc-hsyaml)
       ("ghc-http" ,ghc-http)
       ("ghc-http-client" ,ghc-http-client)
       ("ghc-http-client-tls" ,ghc-http-client-tls)
       ("ghc-http-types" ,ghc-http-types)
       ("ghc-ipynb" ,ghc-ipynb)
       ("ghc-juicypixels" ,ghc-juicypixels)
       ("ghc-network" ,ghc-network)
       ("ghc-network-uri" ,ghc-network-uri)
       ("ghc-pandoc-types" ,ghc-pandoc-types)
       ("ghc-random" ,ghc-random)
       ("ghc-scientific" ,ghc-scientific)
       ("ghc-sha" ,ghc-sha)
       ("ghc-skylighting" ,ghc-skylighting)
       ("ghc-split" ,ghc-split)
       ("ghc-syb" ,ghc-syb)
       ("ghc-tagsoup" ,ghc-tagsoup)
       ("ghc-temporary" ,ghc-temporary)
       ("ghc-texmath" ,ghc-texmath)
       ("ghc-unicode-transforms" ,ghc-unicode-transforms)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-vector" ,ghc-vector)
       ("ghc-xml" ,ghc-xml)
       ("ghc-zip-archive" ,ghc-zip-archive)
       ("ghc-zlib" ,ghc-zlib)))
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-golden" ,ghc-tasty-golden)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-tasty-lua" ,ghc-tasty-lua)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-hunit" ,ghc-hunit)))
    (home-page "https://pandoc.org")
    (synopsis "Conversion between markup formats")
    (description
     "Pandoc is a Haskell library for converting from one markup format to
another, and a command-line tool that uses this library.  It can read and
write Markdown and (subsets of) other formats, such as HTML, reStructuredText,
LaTeX, DocBook, and many more.

Pandoc extends standard Markdown syntax with footnotes, embedded LaTeX,
definition lists, tables, and other features.  A compatibility mode is
provided for those who need a drop-in replacement for Markdown.pl.")
    (license license:gpl2+)))

(define-public pandoc
  (package
    (inherit ghc-pandoc)
    (name "pandoc")
    (arguments
     `(#:configure-flags
       (list "-fstatic"
             ;; Do not build trypandoc; this is the default but it's better to
             ;; be explicit.
             "-f-trypandoc"
             ;; TODO: Without these we cannot link the Haskell libraries
             ;; statically.  It would be nice if we could also build the
             ;; shared libraries.
             "--disable-shared"
             "--disable-executable-dynamic"
             ;; That's where we place all static libraries
             "--extra-lib-dirs=static-libs/"
             "--ghc-option=-static")
       #:modules ((guix build haskell-build-system)
                  (guix build utils)
                  (ice-9 match)
                  (srfi srfi-1))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'create-simple-paths-module
           (lambda* (#:key outputs #:allow-other-keys)
             (call-with-output-file "Paths_pandoc.hs"
               (lambda (port)
                 (format port "\
{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_pandoc (version,getDataDir,getDataFileName) where
import Prelude
import Data.Version (Version(..))
import System.Info
version :: Version
version = Version [~a] []

datadir :: FilePath
datadir = \"~a/share/\" ++
  arch ++ \"-\" ++
  os ++ \"-\" ++
  compilerName ++ \"-~a/pandoc-~a\"

getDataDir :: IO FilePath
getDataDir = return datadir

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ \"/\" ++ name)
"
                         (string-map (lambda (chr) (if (eq? chr #\.) #\, chr))
                                     ,(package-version ghc-pandoc))
                         (assoc-ref outputs "out")
                         ,(package-version ghc)
                         ,(package-version ghc-pandoc))))
             #t))
         (add-after 'unpack 'prepare-static-libraries
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p (string-append (getcwd) "/static-libs"))
             (for-each
              (lambda (input)
                (when (or (string-prefix? "static-" (car input))
                          (string-prefix? "ghc" (car input)))
                  (match (find-files (cdr input) "\\.a$")
                    ((and (first . rest) libs)
                     (for-each (lambda (lib)
                                 (let ((target (string-append (getcwd) "/static-libs/"
                                                              (basename lib))))
                                   (unless (file-exists? target)
                                     (symlink first target))))
                               libs))
                    (_ #f))))
              inputs)
             #t))
         (delete 'check)
         (add-after 'install 'post-install-check
           (assoc-ref %standard-phases 'check)))))
    (outputs '("out" "lib" "static" "doc"))
    (inputs
     (let* ((direct-inputs (package-inputs ghc-pandoc))
            (all-static-inputs
             (map (lambda (pkg)
                    (list (string-append "static-" (package-name pkg))
                          pkg "static"))
                  (delete-duplicates
                   (append (map cadr direct-inputs)
                           (filter (lambda (pkg)
                                     (string-prefix? "ghc-" (package-name pkg)))
                                   (package-closure
                                    (map cadr direct-inputs))))))))
       `(("zlib:static" ,zlib "static")
         ,@all-static-inputs
         ,@direct-inputs)))
    (native-inputs
     (let* ((direct-inputs (package-native-inputs ghc-pandoc))
            (all-static-inputs
             (map (lambda (pkg)
                    (list (string-append "static-" (package-name pkg))
                          pkg "static"))
                  (delete-duplicates
                   (append (map cadr direct-inputs)
                           (filter (lambda (pkg)
                                     (string-prefix? "ghc-" (package-name pkg)))
                                   (package-closure
                                    (map cadr direct-inputs))))))))
       `(,@all-static-inputs
         ,@direct-inputs)))))

(define-public ghc-pandoc-citeproc
  (package
    (name "ghc-pandoc-citeproc")
    (version "0.16.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "pandoc-citeproc/pandoc-citeproc-"
                           version ".tar.gz"))
       (sha256
        (base32
         "15mm17awgi1b5yazwhr5nh8b59qml1qk6pz6gpyijks70fq2arsv"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Many YAML tests (44) are failing do to changes in ghc-yaml:
         ;; <https://github.com/jgm/pandoc-citeproc/issues/342>.
         (add-before 'configure 'patch-tests
           (lambda _
             (substitute* "tests/test-pandoc-citeproc.hs"
               (("let allTests = citeprocTests \\+\\+ biblio2yamlTests")
                "let allTests = citeprocTests"))))
         ;; Tests need to be run after installation.
         (delete 'check)
         (add-after 'install 'post-install-check
           (assoc-ref %standard-phases 'check)))))
    (inputs
     `(("ghc-pandoc-types" ,ghc-pandoc-types)
       ("ghc-pandoc" ,ghc-pandoc)
       ("ghc-tagsoup" ,ghc-tagsoup)
       ("ghc-aeson" ,ghc-aeson)
       ("ghc-vector" ,ghc-vector)
       ("ghc-xml-conduit" ,ghc-xml-conduit)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-data-default" ,ghc-data-default)
       ("ghc-setenv" ,ghc-setenv)
       ("ghc-split" ,ghc-split)
       ("ghc-yaml" ,ghc-yaml)
       ("ghc-hs-bibutils" ,ghc-hs-bibutils)
       ("ghc-rfc5051" ,ghc-rfc5051)
       ("ghc-syb" ,ghc-syb)
       ("ghc-old-locale" ,ghc-old-locale)
       ("ghc-aeson-pretty" ,ghc-aeson-pretty)
       ("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-temporary" ,ghc-temporary)))
    (home-page "https://github.com/jgm/pandoc-citeproc")
    (synopsis "Library for using pandoc with citeproc")
    (description
     "The @code{pandoc-citeproc} library exports functions for using the
citeproc system with pandoc.  It relies on @code{citeproc-hs}, a library for
rendering bibliographic reference citations into a variety of styles using a
macro language called @dfn{Citation Style Language} (CSL).  This package also
contains an executable @code{pandoc-citeproc}, which works as a pandoc filter,
and also has a mode for converting bibliographic databases a YAML format
suitable for inclusion in pandoc YAML metadata.")
    (license license:bsd-3)))

(define-public pandoc-citeproc
  (package (inherit ghc-pandoc-citeproc)
    (name "pandoc-citeproc")
    (arguments
     `(#:configure-flags
       (list "-fstatic"
             "--disable-shared"
             "--disable-executable-dynamic"
             ;; That's where we place all static libraries
             "--extra-lib-dirs=static-libs/"
             "--ghc-option=-static")
       #:modules ((guix build haskell-build-system)
                  (guix build utils)
                  (ice-9 match)
                  (srfi srfi-1))
       #:phases
       (modify-phases %standard-phases
         ;; Many YAML tests (44) are failing do to changes in ghc-yaml:
         ;; <https://github.com/jgm/pandoc-citeproc/issues/342>.
         (add-before 'configure 'patch-tests
           (lambda _
             (substitute* "tests/test-pandoc-citeproc.hs"
               (("let allTests = citeprocTests \\+\\+ biblio2yamlTests")
                "let allTests = citeprocTests"))))
         ;; Tests need to be run after installation.
         (delete 'check)
         (add-after 'install 'post-install-check
           (assoc-ref %standard-phases 'check))
         (add-after 'unpack 'create-simple-paths-module
           (lambda* (#:key outputs #:allow-other-keys)
             (call-with-output-file "Paths_pandoc_citeproc.hs"
               (lambda (port)
                 (format port "\
{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_pandoc_citeproc (version,getDataFileName) where
import Prelude
import Data.Version (Version(..))
import System.Info
version :: Version
version = Version [~a] []

datadir :: FilePath
datadir = \"~a/share/\" ++
  arch ++ \"-\" ++
  os ++ \"-\" ++
  compilerName ++ \"-~a/pandoc-citeproc-~a\"

getDataDir :: IO FilePath
getDataDir = return datadir

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ \"/\" ++ name)
"
                         (string-map (lambda (chr) (if (eq? chr #\.) #\, chr))
                                     ,(package-version ghc-pandoc-citeproc))
                         (assoc-ref outputs "out")
                         ,(package-version ghc)
                         ,(package-version ghc-pandoc-citeproc))))
             #t))
         (add-after 'unpack 'prepare-static-libraries
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p (string-append (getcwd) "/static-libs"))
             (for-each
              (lambda (input)
                (when (or (string-prefix? "static-" (car input))
                          (string-prefix? "ghc" (car input)))
                  (match (find-files (cdr input) "\\.a$")
                    ((and (first . rest) libs)
                     (for-each (lambda (lib)
                                 (let ((target (string-append (getcwd) "/static-libs/"
                                                              (basename lib))))
                                   (unless (file-exists? target)
                                     (symlink first target))))
                               libs))
                    (_ #f))))
              inputs)
             #t)))))
    (inputs
     (let* ((direct-inputs
             (cons `("ghc-pandoc" ,pandoc)
                   (alist-delete "ghc-pandoc"
                                 (package-inputs ghc-pandoc-citeproc))))
            (all-static-inputs
             (map (lambda (pkg)
                    (list (string-append "static-" (package-name pkg))
                          pkg "static"))
                  (delete-duplicates
                   (append (map cadr direct-inputs)
                           (filter (lambda (pkg)
                                     (string-prefix? "ghc-" (package-name pkg)))
                                   (package-closure
                                    (map cadr direct-inputs))))))))
       `(("zlib:static" ,zlib "static")
         ("pandoc" ,pandoc "lib")
         ,@all-static-inputs
         ,@direct-inputs)))
    (outputs '("out" "lib" "static" "doc"))
    (synopsis "Pandoc filter for bibliographic references")))

(define-public ghc-pandoc-types
  (package
    (name "ghc-pandoc-types")
    (version "1.17.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "pandoc-types/pandoc-types-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1d6ygq991ddria71l7hg9yd7lq94sjy4m71rdws1v8hq943c4d0q"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; None of the directory names are actually used.  By generating a
         ;; simpler module without references to store names we avoid
         ;; introducing references in the pandoc executable.
         (add-after 'unpack 'create-simple-paths-module
           (lambda _
             (call-with-output-file "Paths_pandoc_types.hs"
               (lambda (port)
                 (format port "\
{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_pandoc_types (version) where
import Data.Version (Version(..))
version :: Version
version = Version [~a] []
" (string-map (lambda (chr) (if (eq? chr #\.) #\, chr)) ,version))))
             #t)))))
    (inputs
     `(("ghc-syb" ,ghc-syb)
       ("ghc-aeson" ,ghc-aeson)
       ("ghc-string-qq" ,ghc-string-qq)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)
       ("ghc-hunit" ,ghc-hunit)))
    (home-page "https://johnmacfarlane.net/pandoc")
    (synopsis "Types for representing a structured document")
    (description
     "This module defines the @code{Pandoc} data structure, which is used by
pandoc to represent structured documents.  It also provides functions for
building up, manipulating and serialising @code{Pandoc} structures.")
    (license license:bsd-3)))

(define-public ghc-parallel
  (package
    (name "ghc-parallel")
    (version "3.2.2.0")
    (outputs '("out" "static" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/parallel/parallel-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1xkfi96w6yfpppd0nw1rnszdxmvifwzm699ilv6332ra3akm610p"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/parallel")
    (synopsis "Parallel programming library")
    (description
     "This package provides a library for parallel programming.")
    (license license:bsd-3)))

(define-public ghc-parsec-numbers
  (package
    (name "ghc-parsec-numbers")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "parsec-numbers/parsec-numbers-" version ".tar.gz"))
       (sha256
        (base32 "1gzy4v3r02kvdxvgg1nj83mmb6aph2v4ilf9c7y6nbvi2x49l0bp"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/parsec-numbers")
    (synopsis "Utilities for parsing numbers from strings")
    (description
     "This package provides the number parsers without the need to use a large
(and unportable) token parser.")
    (license license:bsd-3)))

(define-public ghc-parser-combinators
  (package
    (name "ghc-parser-combinators")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "parser-combinators/parser-combinators-"
                           version ".tar.gz"))
       (sha256
        (base32
         "149yhbnrrl108h1jinrsxni3rwrldhphpk9bbmbpr90q5fbl4xmc"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/mrkkrp/parser-combinators")
    (synopsis "Commonly useful parser combinators")
    (description
     "This is a lightweight package providing commonly useful parser
combinators.")
    (license license:bsd-3)))

(define-public ghc-parsers
  (package
    (name "ghc-parsers")
    (version "0.12.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/parsers/parsers-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0v0smxbzk1qpdfkfqqmrzd2dngv3vxba10mkjn9nfm6a309izf8p"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f)) ; FIXME: Test fails with "cannot satisfy
                               ; -package attoparsec-0.13.0.1"
    (inputs
     `(("ghc-base-orphans" ,ghc-base-orphans)
       ("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-scientific" ,ghc-scientific)
       ("ghc-charset" ,ghc-charset)
       ("ghc-unordered-containers" ,ghc-unordered-containers)))
    (home-page "https://github.com/ekmett/parsers/")
    (synopsis "Parsing combinators")
    (description "This library provides convenient combinators for working
with and building parsing combinator libraries.  Given a few simple instances,
you get access to a large number of canned definitions.  Instances exist for
the parsers provided by @code{parsec}, @code{attoparsec} and @code{base}'s
@code{Text.Read}.")
    (license license:bsd-3)))

(define-public ghc-path
  (package
    (name "ghc-path")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/path/path-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0nayla4k1gb821k8y5b9miflv1bi8f0czf9rqr044nrr2dddi2sb"))))
    (build-system haskell-build-system)
    (arguments
     ;; TODO: There are some Windows-related tests and modules that need to be
     ;; danced around.
     `(#:tests? #f
       #:cabal-revision
       ("1" "05b1zwx2a893h4h5wvgpc5g5pyx71hfmx409rqisd8s1bq1hn463")))
    (inputs
     `(("ghc-aeson" ,ghc-aeson)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-hashable" ,ghc-hashable)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-genvalidity" ,ghc-genvalidity)
       ("ghc-genvalidity-property" ,ghc-genvalidity-property)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-validity" ,ghc-validity)))
    (home-page
     "https://hackage.haskell.org/package/path")
    (synopsis "Support for well-typed paths")
    (description "This package introduces a type for paths upholding useful
invariants.")
    (license license:bsd-3)))

(define-public ghc-path-io
  (package
    (name "ghc-path-io")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/path-io/path-io-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0jqx3mi4an4kb3kg78n1p3xrz832yrfrnvj795b0xhkv6h1z5ir3"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-dlist" ,ghc-dlist)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-path" ,ghc-path)
       ("ghc-transformers-base" ,ghc-transformers-base)
       ("ghc-unix-compat" ,ghc-unix-compat)
       ("ghc-temporary" ,ghc-temporary)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)))
    (home-page
     "https://github.com/mrkkrp/path-io")
    (synopsis "Functions for manipulating well-typed paths")
    (description "This package provides an interface to the @code{directory}
package for users of @code{path}.  It also implements some missing stuff like
recursive scanning and copying of directories, working with temporary
files/directories, and more.")
    (license license:bsd-3)))

(define-public ghc-paths
  (package
    (name "ghc-paths")
    (version "0.1.0.12")
    (outputs '("out" "static" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/ghc-paths/ghc-paths-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1164w9pqnf7rjm05mmfjznz7rrn415blrkk1kjc0gjvks1vfdjvf"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/simonmar/ghc-paths")
    (synopsis
     "Knowledge of GHC's installation directories")
    (description
     "Knowledge of GHC's installation directories.")
    (license license:bsd-3)))

(define-public ghc-patience
  (package
    (name "ghc-patience")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/patience/patience-"
             version ".tar.gz"))
       (sha256
        (base32
         "1i1b37lgi31c17yrjyf8pdm4nf5lq8vw90z3rri78hf0k66d0p3i"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/patience")
    (synopsis "Patience diff and longest increasing subsequence")
    (description
     "This library implements the 'patience diff' algorithm, as well as the
patience algorithm for the longest increasing subsequence problem.
Patience diff computes the difference between two lists, for example the lines
of two versions of a source file.  It provides a good balance between
performance, nice output for humans, and simplicity of implementation.")
    (license license:bsd-3)))

(define-public ghc-pattern-arrows
  (package
    (name "ghc-pattern-arrows")
    (version "0.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://hackage/package/pattern-arrows/pattern-arrows-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "13q7bj19hd60rnjfc05wxlyck8llxy11z3mns8kxg197wxrdkhkg"))))
    (build-system haskell-build-system)
    (home-page
     "https://blog.functorial.com/posts/2013-10-27-Pretty-Printing-Arrows.html")
    (synopsis "Arrows for Pretty Printing")
    (description
     "A library for generating concise pretty printers based on precedence
rules.")
    (license license:expat)))

(define-public ghc-pcre-light
  (package
    (name "ghc-pcre-light")
    (version "0.4.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/pcre-light/pcre-light-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0xcyi1fivwg7a92mch5bcqzmrfxzqj42rmb3m8kgs61x4qwpxj82"))))
    (build-system haskell-build-system)
    (arguments
     `(#:extra-directories ("pcre")))
    (inputs
     `(("pcre" ,pcre)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://github.com/Daniel-Diaz/pcre-light")
    (synopsis "Haskell library for Perl 5 compatible regular expressions")
    (description "This package provides a small, efficient, and portable regex
library for Perl 5 compatible regular expressions.  The PCRE library is a set
of functions that implement regular expression pattern matching using the same
syntax and semantics as Perl 5.")
    (license license:bsd-3)))

(define-public ghc-persistent
  (package
    (name "ghc-persistent")
    (version "2.10.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/persistent/"
             "persistent-" version ".tar.gz"))
       (sha256
        (base32
         "1cxswz72sqdg2z1nbpgp1k5qr41djgk8qbf8nz7wfppsrhacyffi"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-aeson" ,ghc-aeson)
       ("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-base64-bytestring" ,ghc-base64-bytestring)
       ("ghc-blaze-html" ,ghc-blaze-html)
       ("ghc-conduit" ,ghc-conduit)
       ("ghc-fast-logger" ,ghc-fast-logger)
       ("ghc-http-api-data" ,ghc-http-api-data)
       ("ghc-monad-logger" ,ghc-monad-logger)
       ("ghc-path-pieces" ,ghc-path-pieces)
       ("ghc-resource-pool" ,ghc-resource-pool)
       ("ghc-resourcet" ,ghc-resourcet)
       ("ghc-scientific" ,ghc-scientific)
       ("ghc-silently" ,ghc-silently)
       ("ghc-unliftio-core" ,ghc-unliftio-core)
       ("ghc-unliftio" ,ghc-unliftio)
       ("ghc-unordered-containers"
        ,ghc-unordered-containers)
       ("ghc-vector" ,ghc-vector)))
    (native-inputs `(("ghc-hspec" ,ghc-hspec)))
    (home-page "https://www.yesodweb.com/book/persistent")
    (synopsis "Type-safe, multi-backend data serialization for Haskell")
    (description "This Haskell package allows Haskell programs to access data
storage systems like PostgreSQL, SQLite, MySQL and MongoDB in a type-safe
way.")
    (license license:expat)))

(define-public ghc-persistent-sqlite
  (package
    (name "ghc-persistent-sqlite")
    (version "2.10.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/persistent-sqlite/"
             "persistent-sqlite-" version ".tar.gz"))
       (sha256
        (base32
         "0agag3cgivl6mk38pqzr0qw5lxps9p2bgdwvi5658l46hs7bixxn"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-persistent" ,ghc-persistent)
       ("ghc-aeson" ,ghc-aeson)
       ("ghc-conduit" ,ghc-conduit)
       ("ghc-microlens-th" ,ghc-microlens-th)
       ("ghc-monad-logger" ,ghc-monad-logger)
       ("ghc-resource-pool" ,ghc-resource-pool)
       ("ghc-resourcet" ,ghc-resourcet)
       ("ghc-unliftio-core" ,ghc-unliftio-core)
       ("ghc-unordered-containers"
        ,ghc-unordered-containers)))
    (native-inputs
     `(("ghc-persistent-template"
        ,ghc-persistent-template)
       ("ghc-persistent-test" ,ghc-persistent-test)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-fast-logger" ,ghc-fast-logger)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-system-fileio" ,ghc-system-fileio)
       ("ghc-system-filepath" ,ghc-system-filepath)
       ("ghc-temporary" ,ghc-temporary)))
    (home-page
     "https://www.yesodweb.com/book/persistent")
    (synopsis "Backend for the persistent library using sqlite3")
    (description "This Haskell package includes a thin sqlite3 wrapper based
on the direct-sqlite package, as well as the entire C library, so there are no
system dependencies.")
    (license license:expat)))

(define-public ghc-persistent-template
  (package
    (name "ghc-persistent-template")
    (version "2.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/persistent-template/"
             "persistent-template-" version ".tar.gz"))
       (sha256
        (base32
         "16yjrl0gh4jbs4skr7iv6a55lny59bqhd6hjmvch1cl9j5d0c0g3"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-persistent" ,ghc-persistent)
       ("ghc-aeson" ,ghc-aeson)
       ("ghc-http-api-data" ,ghc-http-api-data)
       ("ghc-monad-control" ,ghc-monad-control)
       ("ghc-monad-logger" ,ghc-monad-logger)
       ("ghc-path-pieces" ,ghc-path-pieces)
       ("ghc-th-lift-instances" ,ghc-th-lift-instances)
       ("ghc-unordered-containers"
        ,ghc-unordered-containers)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://www.yesodweb.com/book/persistent")
    (synopsis "Type-safe, non-relational, multi-backend persistence")
    (description "This Haskell package provides interfaces and helper
functions for the ghc-persistent package.")
    (license license:expat)))

(define-public ghc-persistent-test
  (package
    (name "ghc-persistent-test")
    (version "2.0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/persistent-test/"
             "persistent-test-" version ".tar.gz"))
       (sha256
        (base32
         "11aq5cy0n43jamf6mg4sr4300bc2zdbjxsczzxwjkb4hzs0ijsdv"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-aeson" ,ghc-aeson)
       ("ghc-blaze-html" ,ghc-blaze-html)
       ("ghc-conduit" ,ghc-conduit)
       ("ghc-monad-control" ,ghc-monad-control)
       ("ghc-monad-logger" ,ghc-monad-logger)
       ("ghc-path-pieces" ,ghc-path-pieces)
       ("ghc-persistent" ,ghc-persistent)
       ("ghc-persistent-template" ,ghc-persistent-template)
       ("ghc-random" ,ghc-random)
       ("ghc-resourcet" ,ghc-resourcet)
       ("ghc-transformers-base" ,ghc-transformers-base)
       ("ghc-unliftio" ,ghc-unliftio)
       ("ghc-unliftio-core" ,ghc-unliftio-core)
       ("ghc-unordered-containers" ,ghc-unordered-containers)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-quickcheck-instances" ,ghc-quickcheck-instances)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-hspec-expectations" ,ghc-hspec-expectations)
       ("ghc-hunit" ,ghc-hunit)))
    (home-page "https://www.yesodweb.com/book/persistent")
    (synopsis "Tests for the Persistent database library")
    (description
     "This is only for use in developing libraries that should conform to
the persistent interface, not for users of the persistent suite of database
libraries.")
    (license license:expat)))

(define-public ghc-pgp-wordlist
  (package
    (name "ghc-pgp-wordlist")
    (version "0.1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://hackage/package/pgp-wordlist/pgp-wordlist-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "15g6qh0fb7kjj3l0w8cama7cxgnhnhybw760md9yy7cqfq15cfzg"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-vector" ,ghc-vector)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
       ("ghc-doctest" ,ghc-doctest)))
    (home-page
     "https://github.com/quchen/pgp-wordlist")
    (synopsis
     "Translate between binary data and a human-readable collection of words")
    (description
     "The PGP Word List consists of two phonetic alphabets, each with one word
per possible byte value.  A string of bytes is translated with these
alphabets, alternating between them at each byte.

The PGP words corresponding to the bytes 5B 1D CA 6E are \"erase breakaway
spellbind headwaters\", for example.

For further information, see
@url{http://en.wikipedia.org/wiki/PGP_word_list}.")
    (license license:bsd-3)))

(define-public ghc-pipes
  (package
    (name "ghc-pipes")
    (version "4.3.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/pipes/"
             "pipes-" version ".tar.gz"))
       (sha256
        (base32
         "1ch3xr5f5if0psd3lsyrpkwrgh36synnzqcpimghprys68l4zfkn"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-exceptions" ,ghc-exceptions)
       ("ghc-mmorph" ,ghc-mmorph)
       ("ghc-void" ,ghc-void)
       ("ghc-semigroups" ,ghc-semigroups)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-quickcheck2"
        ,ghc-test-framework-quickcheck2)))
    (home-page
     "https://hackage.haskell.org/package/pipes")
    (synopsis "Compositional pipelines")
    (description
     "A clean and powerful stream processing library that lets you build
and connect reusable streaming components.  Advantages over traditional streaming
libraries:
@itemize
@item Concise API: Use simple commands like @code{for}, (@code{>->}), @code{await},
and @code{yield}
@item Blazing fast: Implementation tuned for speed, including shortcut fusion
@item Lightweight Dependency: pipes is small and compiles very rapidly, including
dependencies
@item Elegant semantics: Use practical category theory
@item ListT: Correct implementation of @code{ListT} that interconverts with pipes
@item Bidirectionality: Implement duplex channels
@end itemize")
    (license license:bsd-3)))

(define-public ghc-pointedlist
  (package
    (name "ghc-pointedlist")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/pointedlist/"
             "pointedlist-" version ".tar.gz"))
       (sha256
        (base32
         "16xsrzqql7i4z6a3xy07sqnbyqdmcar1jiacla58y4mvkkwb0g3l"))))
    (build-system haskell-build-system)
    (home-page
     "https://hackage.haskell.org/package/pointedlist")
    (synopsis
     "Zipper-like comonad which works as a list, tracking a position")
    (description
     "A PointedList tracks the position in a non-empty list which works
similarly to a zipper.  A current item is always required, and therefore
the list may never be empty.  A circular PointedList wraps around to the
other end when progressing past the actual edge.")
    (license license:bsd-3)))

(define-public ghc-polyparse
  (package
    (name "ghc-polyparse")
    (version "1.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/polyparse/polyparse-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "19fs18g7fvfdkm9zy28cgighjcxfa6mcpqgyp6whmsjkb3h393fx"))))
    (build-system haskell-build-system)
    (home-page
     "http://code.haskell.org/~malcolm/polyparse/")
    (synopsis
     "Alternative parser combinator libraries")
    (description
     "This package provides a variety of alternative parser combinator
libraries, including the original HuttonMeijer set.  The Poly sets have
features like good error reporting, arbitrary token type, running state, lazy
parsing, and so on.  Finally, Text.Parse is a proposed replacement for the
standard Read class, for better deserialisation of Haskell values from
Strings.")
    (license license:lgpl2.1)))

(define-public ghc-pqueue
  (package
    (name "ghc-pqueue")
    (version "1.4.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "pqueue/pqueue-" version ".tar.gz"))
       (sha256
        (base32
         "1v4zhv2sc1zsw91hvnarkjhayx2dnf7ccxz6rrhsqpcs0szaranj"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://hackage.haskell.org/package/pqueue")
    (synopsis "Reliable, persistent, fast priority queues")
    (description
     "This package provides a fast, reliable priority queue implementation
based on a binomial heap.")
    (license license:bsd-3)))

(define-public ghc-prelude-extras
  (package
    (name "ghc-prelude-extras")
    (version "0.4.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/prelude-extras/prelude-extras-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0xzqdf3nl2h0ra4gnslm1m1nsxlsgc0hh6ky3vn578vh11zhifq9"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/ekmett/prelude-extras")
    (synopsis "Higher order versions of Prelude classes")
    (description "This library provides higher order versions of
@code{Prelude} classes to ease programming with polymorphic recursion and
reduce @code{UndecidableInstances}.")
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
    (home-page "https://hackage.haskell.org/package/prettyclass")
    (synopsis "Pretty printing class similar to Show")
    (description "This package provides a pretty printing class similar
to @code{Show}, based on the HughesPJ pretty printing library.  It
provides the pretty printing class and instances for the Prelude
types.")
    (license license:bsd-3)))

(define-public ghc-prettyprinter
  (package
    (name "ghc-prettyprinter")
    (version "1.2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://hackage/package/prettyprinter/prettyprinter-"
             version
             ".tar.gz"))
       (sha256
        (base32 "1p9c3q55hba4c0zyxc624g5df7wgsclpsmd8wqpdnmib882q9d1v"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-doctest" ,ghc-doctest)
       ("ghc-pgp-wordlist" ,ghc-pgp-wordlist)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)))
    (home-page "https://github.com/quchen/prettyprinter")
    (synopsis
     "Modern, easy to use, well-documented, extensible pretty-printer")
    (description
     "A prettyprinter/text rendering engine.  Easy to use, well-documented,
ANSI terminal backend exists, HTML backend is trivial to implement, no name
clashes, @code{Text}-based, extensible.")
    (license license:bsd-2)))

(define-public ghc-prettyprinter-1.6
  (package
    (inherit ghc-prettyprinter)
    (version "1.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://hackage/package/prettyprinter/prettyprinter-"
             version
             ".tar.gz"))
       (sha256
        (base32 "10fphxh8lvdaw7i8jyllwmj87w02db92mf99zfw5vddp9mv6b5rz"))))
    (inputs
     `(("ghc-quickckeck-instances" , ghc-quickcheck-instances)
       ,@(package-inputs ghc-prettyprinter)))))

(define-public ghc-prettyprinter-ansi-terminal
  (package
    (name "ghc-prettyprinter-ansi-terminal")
    (version "1.1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://hackage/package/prettyprinter-ansi-terminal/"
             "prettyprinter-ansi-terminal-" version ".tar.gz"))
       (sha256
        (base32 "0ha6vz707qzb5ky7kdsnw2zgphg2dnxrpbrxy8gaw119vwhb9q6k"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-ansi-terminal" ,ghc-ansi-terminal)
       ("ghc-prettyprinter" ,ghc-prettyprinter-1.6)))
    (native-inputs `(("ghc-doctest" ,ghc-doctest)))
    (home-page
     "https://github.com/quchen/prettyprinter")
    (synopsis
     "ANSI terminal backend for the prettyprinter package")
    (description "ANSI terminal backend for the prettyprinter package.")
    (license license:bsd-2)))

(define-public ghc-pretty-hex
  (package
    (name "ghc-pretty-hex")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "pretty-hex-" version "/"
                           "pretty-hex-" version ".tar.gz"))
       (sha256
        (base32
         "0ylwkvvjvmpprha9nx83xb8gkhyanhk5fffc0r7lb96n4ch5z6pz"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/GaloisInc/hexdump")
    (synopsis "Haskell library for hex dumps of ByteStrings")
    (description "This Haskell library generates pretty hex dumps of
ByteStrings in the style of other common *nix hex dump tools.")
    (license license:bsd-3)))

(define-public ghc-pretty-show
  (package
    (name "ghc-pretty-show")
    (version "1.9.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/pretty-show/"
                           "pretty-show-" version ".tar.gz"))
       (sha256
        (base32
         "0gs2pabi4qa4b0r5vffpf9b1cf5n9y2939a3lljjw7cmg6xvx5dh"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-haskell-lexer" ,ghc-haskell-lexer)
       ("ghc-happy" ,ghc-happy)))
    (home-page "https://wiki.github.com/yav/pretty-show")
    (synopsis "Tools for working with derived `Show` instances")
    (description
     "This package provides a library and an executable for working with
derived @code{Show} instances.  By using the library, derived @code{Show}
instances can be parsed into a generic data structure.  The @code{ppsh} tool
uses the library to produce human-readable versions of @code{Show} instances,
which can be quite handy for debugging Haskell programs.  We can also render
complex generic values into an interactive Html page, for easier
examination.")
    (license license:expat)))

(define-public ghc-pretty-simple
  (package
    (name "ghc-pretty-simple")
    (version "2.2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://hackage/package/pretty-simple/"
             "pretty-simple-" version ".tar.gz"))
       (sha256
        (base32 "0wsi9235ihm15s145lxi7325vv2k4bhighc5m88kn1lk0pl81aqq"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-ansi-terminal" ,ghc-ansi-terminal)
       ("ghc-glob" ,ghc-glob)
       ("ghc-optparse-applicative" ,ghc-optparse-applicative)
       ("ghc-aeson" ,ghc-aeson)))
    (native-inputs
     `(("ghc-doctest" ,ghc-doctest)))
    (home-page "https://github.com/cdepillabout/pretty-simple")
    (synopsis "Pretty printer for data types with a 'Show' instance")
    (description
     "Pretty-simple is a pretty printer for Haskell data types that have a
Show instance.")
    (license license:bsd-3)))

(define-public ghc-primitive
  (package
    (name "ghc-primitive")
    (version "0.6.4.0")
    (outputs '("out" "static" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/primitive/primitive-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0r0cda7acvplgwaxy69kviv4jp7kkfi038by68gj4yfx4iwszgjc"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("1" "18a14k1yiam1m4l29rin9a0y53yp3nxvkz358nysld8aqwy2qsjv")))
    (home-page
     "https://github.com/haskell/primitive")
    (synopsis "Primitive memory-related operations")
    (description
     "This package provides various primitive memory-related operations.")
    (license license:bsd-3)))

(define-public ghc-process-extras
  (package
    (name "ghc-process-extras")
    (version "0.7.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://hackage.haskell.org/package/process-extras/"
         "process-extras-" version ".tar.gz"))
       (sha256
        (base32
         "0klqgr37f1z2z6i0a9b0giapmq0p35l5k9kz1p7f0k1597w7agi9"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-data-default" ,ghc-data-default)
       ("ghc-generic-deriving" ,ghc-generic-deriving)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-listlike" ,ghc-listlike)))
    (home-page "https://github.com/seereason/process-extras")
    (synopsis "Extra tools for managing processes")
    (description "This package extends
@url{http://hackage.haskell.org/package/process}.  It allows you to read
process input and output as ByteStrings or Text, or write your own
ProcessOutput instance.  It also provides lazy process input and output,
and a ProcessMaker class for more flexibility in the process creation
API.")
    (license license:expat)))

(define-public ghc-profunctors
  (package
    (name "ghc-profunctors")
    (version "5.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/profunctors/profunctors-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1dx3nkc27yxsrbrhh3iwhq7dl1xn6bj7n62yx6nh8vmpbg62lqvl"))))
    (build-system haskell-build-system)
    (outputs '("out" "static" "doc"))
    (inputs
     `(("ghc-base-orphans" ,ghc-base-orphans)
       ("ghc-bifunctors" ,ghc-bifunctors)
       ("ghc-comonad" ,ghc-comonad)
       ("ghc-contravariant" ,ghc-contravariant)
       ("ghc-distributive" ,ghc-distributive)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-tagged" ,ghc-tagged)))
    (home-page "https://github.com/ekmett/profunctors/")
    (synopsis "Profunctors for Haskell")
    (description "This library provides profunctors for Haskell.")
    (license license:bsd-3)))

(define-public ghc-project-template
  (package
    (name "ghc-project-template")
    (version "0.2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/project-template/project-template-"
             version ".tar.gz"))
       (sha256
        (base32
         "1p69ww4rhah2qxragl615wl4a6mk4x9w09am8knmz3s4lxpljlpb"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-base64-bytestring" ,ghc-base64-bytestring)
       ("ghc-conduit" ,ghc-conduit)
       ("ghc-conduit-extra" ,ghc-conduit-extra)
       ("ghc-resourcet" ,ghc-resourcet)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (arguments
     `(#:cabal-revision
       ("1"
        "0lq3sqnq0nr0gbvgzp0lqdl3j3mqdmdlf8xsw0j3pjh581xj3k0a")))
    (home-page "https://github.com/fpco/haskell-ide")
    (synopsis "Specify Haskell project templates and generate files")
    (description
     "Haskell library for both generating and consuming project templates.

ost IDEs provide the concept of a project template: instead of writing all
of the code for a project from scratch, you select a template, answer a few
questions, and a bunch of files are automatically generated.

project-template tries to provide a canonical Haskell library for implementing
the ideal templating system.")
    (license license:bsd-3)))

(define-public ghc-protolude
  (package
    (name "ghc-protolude")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://hackage/package/protolude/protolude-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0zzkyxz0vmcbncpid7gp72fpjj0fla3gqhlfkij5c5lg12skjgfj"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-async" ,ghc-async)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-mtl-compat" ,ghc-mtl-compat)
       ("ghc-transformers-compat" ,ghc-transformers-compat)))
    (home-page "https://github.com/protolude/protolude")
    (synopsis "Sensible set of defaults for writing custom Preludes")
    (description
     "Protolude gives you sensible defaults for writing custom Preludes to
replace the standard one provided by GHC.")
    (license license:expat)))

(define-public ghc-protolude-0.3
  (package
    (inherit ghc-protolude)
    (name "ghc-protolude")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://hackage/package/protolude/protolude-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1b6wprbwfdjyvds2bm6na0fbqgzdkj5ikkk33whbkyh3krd3i0s0"))))))

(define-public ghc-psqueue
  (package
    (name "ghc-psqueue")
    (version "1.1.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://hackage/package/PSQueue-"
                                  version "/PSQueue-" version ".tar.gz"))
              (sha256
               (base32
                "1cik7sw10sacsijmfhghzy54gm1qcyxw14shlp86lx8z89kcnkza"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/PSQueue")
    (synopsis "Priority search queue")
    (description
     "A @dfn{priority search queue} efficiently supports the operations of
both a search tree and a priority queue.  A @code{Binding} is a product of
a key and a priority.  Bindings can be inserted, deleted, modified and queried
in logarithmic time, and the binding with the least priority can be retrieved
in constant time.  A queue can be built from a list of bindings, sorted by
keys, in linear time.")
    (license license:bsd-3)))

(define-public ghc-psqueues
  (package
    (name "ghc-psqueues")
    (version "0.2.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "psqueues-" version "/"
                           "psqueues-" version ".tar.gz"))
       (sha256
        (base32
         "1yckx2csqswghiy9nfj03cybmza8104nmnpbpcc9ngwlbmakn9i6"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-hashable" ,ghc-hashable)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-tagged" ,ghc-tagged)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)))
    (home-page "https://github.com/jaspervdj/psqueues")
    (synopsis "Pure priority search queues")
    (description "The psqueues package provides
@uref{https://en.wikipedia.org/wiki/Priority_queue, Priority Search Queues} in
three different flavors:

@itemize
@item @code{OrdPSQ k p v}, which uses the @code{Ord k} instance to provide
fast insertion, deletion and lookup.  This implementation is based on Ralf
Hinze's @uref{http://citeseer.ist.psu.edu/hinze01simple.html, A Simple
Implementation Technique for Priority Search Queues}.

Hence, it is similar to the @uref{https://hackage.haskell.org/package/PSQueue,
PSQueue} library, although it is considerably faster and provides a slightly
different API.

@item @code{IntPSQ p v} is a far more efficient implementation.  It fixes the
key type to @code{Int} and uses a
@code{https://en.wikipedia.org/wiki/Radix_tree, radix tree} (like @code{IntMap})
with an additional min-heap property.

@item @code{HashPSQ k p v} is a fairly straightforward extension
of @code{IntPSQ}: it simply uses the keys' hashes as indices in the
@code{IntPSQ}.  If there are any hash collisions, it uses an
@code{OrdPSQ} to resolve those.  The performance of this implementation
is comparable to that of @code{IntPSQ}, but it is more widely
applicable since the keys are not restricted to @code{Int},
but rather to any @code{Hashable} datatype.
@end itemize

Each of the three implementations provides the same API, so they can
be used interchangeably.

Typical applications of Priority Search Queues include:

@itemize
@item Caches, and more specifically LRU Caches;
@item Schedulers;
@item Pathfinding algorithms, such as Dijkstra's and A*.
@end itemize")
    (license license:bsd-3)))

(define-public ghc-pwstore-fast
  (package
    (name "ghc-pwstore-fast")
    (version "2.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/pwstore-fast/"
             "pwstore-fast-" version ".tar.gz"))
       (sha256
        (base32
         "1cpvlwzg3qznhygrr78f75p65mnljd9v5cvnagfxjqppnrkay6bj"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-base64-bytestring" ,ghc-base64-bytestring)
       ("ghc-cryptohash" ,ghc-cryptohash)
       ("ghc-random" ,ghc-random)
       ("ghc-byteable" ,ghc-byteable)))
    (home-page "https://github.com/PeterScott/pwstore")
    (synopsis "Secure password storage")
    (description
     "To store passwords securely, they should be salted, then hashed with
a slow hash function.  This library uses PBKDF1-SHA256, and handles all the
details.  It uses the cryptohash package for speed; if you need a pure
Haskell library, pwstore-purehaskell has the exact same API, but uses only
pure Haskell.  It is about 25 times slower than this package, but still quite
usable.")
    (license license:bsd-3)))

(define-public ghc-random
  (package
    (name "ghc-random")
    (version "1.1")
    (outputs '("out" "static" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/random/random-"
             version
             ".tar.gz"))
       (sha256
        (base32 "0nis3lbkp8vfx8pkr6v7b7kr5m334bzb0fk9vxqklnp2aw8a865p"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/random")
    (synopsis "Random number library")
    (description "This package provides a basic random number generation
library, including the ability to split random number generators.")
    (license license:bsd-3)))

(define-public ghc-raw-strings-qq
  (package
    (name "ghc-raw-strings-qq")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "raw-strings-qq/raw-strings-qq-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1lxy1wy3awf52968iy5y9r5z4qgnn2sxkdrh7js3m9gadb11w09f"))))
    (build-system haskell-build-system)
    (native-inputs `(("ghc-hunit" ,ghc-hunit)))
    (home-page "https://github.com/23Skidoo/raw-strings-qq")
    (synopsis "Raw string literals for Haskell")
    (description
     "This package provides a quasiquoter for raw string literals, i.e. string
literals that don't recognise the standard escape sequences.  Basically, they
make your code more readable by freeing you from the responsibility to escape
backslashes.  They are useful when working with regular expressions,
DOS/Windows paths and markup languages (such as XML).")
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

(define-public ghc-rebase
  (package
    (name "ghc-rebase")
    (version "1.3.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "rebase-" version "/"
                           "rebase-" version ".tar.gz"))
       (sha256
        (base32
         "0q4m2fa7wkgxs0grir8rlqwibasmi3s1x7c107ynndwfm62nzv0a"))))
    (build-system haskell-build-system)
    (outputs '("out" "static" "doc"))
    (inputs `(("ghc-hashable" ,ghc-hashable)
              ("ghc-vector" ,ghc-vector)
              ("ghc-unordered-containers" ,ghc-unordered-containers)
              ("ghc-scientific" ,ghc-scientific)
              ("ghc-uuid" ,ghc-uuid)
              ("ghc-dlist" ,ghc-dlist)
              ("ghc-void" ,ghc-void)
              ("ghc-bifunctors" ,ghc-bifunctors)
              ("ghc-profunctors" ,ghc-profunctors)
              ("ghc-contravariant" ,ghc-contravariant)
              ("ghc-contravariant-extras" ,ghc-contravariant-extras)
              ("ghc-semigroups" ,ghc-semigroups)
              ("ghc-either" ,ghc-either)
              ("ghc-fail" ,ghc-fail)
              ("ghc-base-prelude" ,ghc-base-prelude)))
    (home-page "https://github.com/nikita-volkov/rebase")
    (synopsis "Progressive alternative to the base package
for Haskell")
    (description "This Haskell package is intended for those who are
tired of keeping long lists of dependencies to the same essential libraries
in each package as well as the endless imports of the same APIs all over again.

It also supports the modern tendencies in the language.

To solve those problems this package does the following:

@itemize
@item Reexport the original APIs under the @code{Rebase} namespace.

@item Export all the possible non-conflicting symbols from the
@code{Rebase.Prelude} module.

@item Give priority to the modern practices in the conflicting cases.
@end itemize

The policy behind the package is only to reexport the non-ambiguous and
non-controversial APIs, which the community has obviously settled on.
The package is intended to rapidly evolve with the contribution from
the community, with the missing features being added with pull-requests.")
    (license license:expat)))

(define-public ghc-reducers
  (package
    (name "ghc-reducers")
    (version "3.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/reducers/reducers-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "09wf8pl9ycglcv6qj5ba26gkg2s5iy81hsx9xp0q8na0cwvp71ki"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-fingertree" ,ghc-fingertree)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-semigroupoids" ,ghc-semigroupoids)
       ("ghc-semigroups" ,ghc-semigroups)))
    (home-page "https://github.com/ekmett/reducers/")
    (synopsis "Semigroups, specialized containers and a general map/reduce framework")
    (description "This library provides various semigroups, specialized
containers and a general map/reduce framework for Haskell.")
    (license license:bsd-3)))

(define-public ghc-refact
  (package
    (name "ghc-refact")
    (version "0.3.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "refact/refact-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0v0zxcx29b8jxs2kgy9csykqcp8kzhdvyylw2xfwmj4pfxr2kl0a"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/refact")
    (synopsis "Specify refactorings to perform with apply-refact")
    (description
     "This library provides a datatype which can be interpreted by
@code{apply-refact}.  It exists as a separate library so that applications can
specify refactorings without depending on GHC.")
    (license license:bsd-3)))

(define-public ghc-reflection
  (package
    (name "ghc-reflection")
    (version "2.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/reflection/reflection-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0xr947nj1vww5b8fwqmypxm3y3j5sxl4z8wnf834f83jzfzyjbi7"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-tagged" ,ghc-tagged)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://github.com/ekmett/reflection")
    (synopsis "Reify arbitrary terms into types that can be reflected back
into terms")
    (description "This package addresses the 'configuration problem' which is
propagating configurations that are available at run-time, allowing multiple
configurations to coexist without resorting to mutable global variables or
@code{System.IO.Unsafe.unsafePerformIO}.")
    (license license:bsd-3)))

(define-public ghc-regex
  (package
    (name "ghc-regex")
    (version "1.0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/regex/"
                           "regex-" version ".tar.gz"))
       (sha256
        (base32
         "1f2z025hif1fr24b5khq3qxxyvpxrnhyx8xmbms332arw28rpkda"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'relax-dependencies
           (lambda _
             (substitute* "regex.cabal"
               (("base-compat.*>=.*0.6.*")
                "base-compat >= 0.6\n")
               (("template-haskell.*>=.*2.7.*")
                "template-haskell >= 2.7\n"))
             #t)))))
    (inputs
     `(("ghc-base-compat" ,ghc-base-compat)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-regex-base" ,ghc-regex-base)
       ("ghc-regex-pcre-builtin" ,ghc-regex-pcre-builtin)
       ("ghc-regex-tdfa" ,ghc-regex-tdfa)
       ("ghc-regex-tdfa-text" ,ghc-regex-tdfa-text)
       ("ghc-time-locale-compat" ,ghc-time-locale-compat)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-utf8-string" ,ghc-utf8-string)))
    (home-page "http://regex.uk")
    (synopsis "Toolkit for regex-base")
    (description
     "This package provides a regular expression toolkit for @code{regex-base}
with compile-time checking of regular expression syntax, data types for
matches and captures, a text replacement toolkit, portable options, high-level
AWK-like tools for building text processing apps, regular expression macros
with parsers and test bench, comprehensive documentation, tutorials and
copious examples.")
    (license license:bsd-3)))

(define-public ghc-regex-applicative
  (package
    (name "ghc-regex-applicative")
    (version "0.3.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/regex-applicative/"
             "regex-applicative-" version ".tar.gz"))
       (sha256
        (base32
         "0p0anx5vamrhrdvviwkh2zn6pa3pv2bjb7nfyc7dvz2q7g14y1lg"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-smallcheck" ,ghc-smallcheck)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-smallcheck" ,ghc-tasty-smallcheck)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)))
    (home-page "https://github.com/feuerbach/regex-applicative")
    (synopsis "Regex-based parsing with applicative interface")
    (description
     "@code{regex-applicative} is a Haskell library for parsing using
regular expressions.  Parsers can be built using Applicative interface.")
    (license license:expat)))

(define-public ghc-regex-base
  (package
    (name "ghc-regex-base")
    (version "0.93.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/regex-base/regex-base-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0y1j4h2pg12c853nzmczs263di7xkkmlnsq5dlp5wgbgl49mgp10"))))
    (build-system haskell-build-system)
    (home-page
     "https://sourceforge.net/projects/lazy-regex")
    (synopsis "Replaces/Enhances Text.Regex")
    (description "@code{Text.Regex.Base} provides the interface API for
regex-posix, regex-pcre, regex-parsec, regex-tdfa, regex-dfa.")
    (license license:bsd-3)))

(define-public ghc-regex-compat
  (package
    (name "ghc-regex-compat")
    (version "0.95.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/regex-compat/regex-compat-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0fwmima3f04p9y4h3c23493n1xj629ia2dxaisqm6rynljjv2z6m"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-regex-base" ,ghc-regex-base)
       ("ghc-regex-posix" ,ghc-regex-posix)))
    (home-page "https://sourceforge.net/projects/lazy-regex")
    (synopsis "Replaces/Enhances Text.Regex")
    (description "This library provides one module layer over
@code{regex-posix} to replace @code{Text.Regex}.")
    (license license:bsd-3)))

(define-public ghc-regex-compat-tdfa
  (package
    (name "ghc-regex-compat-tdfa")
    (version "0.95.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/regex-compat-tdfa/regex-compat-tdfa-"
             version ".tar.gz"))
       (sha256
        (base32
         "1p90fn90yhp7fvljjdqjp41cszidcfz4pw7fwvzyx4739b98x8sg"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-regex-base" ,ghc-regex-base)
       ("ghc-regex-tdfa" ,ghc-regex-tdfa)))
    (home-page "https://hub.darcs.net/shelarcy/regex-compat-tdfa")
    (synopsis "Unicode Support version of Text.Regex, using regex-tdfa")
    (description
     "One module layer over @code{regex-tdfa} to replace @code{Text.Regex}.
@code{regex-compat} can't use Unicode characters correctly because of using regex-posix.
This is not good for Unicode users.  This modified regex-compat uses regex-tdfa to solve
this problem.")
    (license license:bsd-3)))

(define-public ghc-regex-pcre
  (package
    (name "ghc-regex-pcre")
    (version "0.94.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "regex-pcre/regex-pcre-" version ".tar.gz"))
              (sha256
               (base32
                "1h16w994g9s62iwkdqa7bar2n9cfixmkzz2rm8svm960qr57valf"))))
    (build-system haskell-build-system)
    (arguments
     `(#:extra-directories ("pcre")))
    (inputs
     `(("ghc-regex-base" ,ghc-regex-base)
       ("pcre" ,pcre)))
    (home-page "https://hackage.haskell.org/package/regex-pcre")
    (synopsis "Enhancement of the builtin Text.Regex library")
    (description
     "This package is an enhancement of the @code{Text.Regex} library.
It wraps the @code{PCRE} C library providing Perl-compatible regular
expressions.")
    (license license:bsd-3)))

(define-public ghc-regex-pcre-builtin
  (package
    (name "ghc-regex-pcre-builtin")
    (version "0.94.5.8.8.35")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "regex-pcre-builtin/regex-pcre-builtin-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1s755qdg1mxrf125sh83bsc5kjkrj8fkq8wf6dg1jan86c7p7gl4"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-regex-base" ,ghc-regex-base)))
    (home-page "https://hackage.haskell.org/package/regex-pcre-builtin")
    (synopsis "Enhancement of the builtin Text.Regex library")
    (description
     "This package is an enhancement of the @code{Text.Regex} library,
providing the PCRE backend to accompany regex-base, with bundled code from
@url{https://www.pcre.org}.")
    (license license:bsd-3)))

(define-public ghc-regex-posix
  (package
    (name "ghc-regex-posix")
    (version "0.95.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/regex-posix/regex-posix-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0gkhzhj8nvfn1ija31c7xnl6p0gadwii9ihyp219ck2arlhrj0an"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-regex-base" ,ghc-regex-base)))
    (home-page "https://sourceforge.net/projects/lazy-regex")
    (synopsis "POSIX regular expressions for Haskell")
    (description "This library provides the POSIX regex backend used by the
Haskell library @code{regex-base}.")
    (license license:bsd-3)))

(define-public ghc-regex-tdfa
  (package
    (name "ghc-regex-tdfa")
    (version "1.2.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/regex-tdfa/regex-tdfa-"
             version ".tar.gz"))
       (sha256
        (base32
         "03yhpqrqz977nwlnhnyz9dacnbzw8xb6j18h365rkgmbc05sb3hf"))))
    (build-system haskell-build-system)
    (outputs '("out" "static" "doc"))
    (inputs
     `(("ghc-regex-base" ,ghc-regex-base)))
    (home-page "https://github.com/haskell-hvr/regex-tdfa")
    (synopsis "POSIX extended regular expressions in Haskell.")
    (description
     "Regex-tdfa is a pure Haskell regular expression library implementing POSIX
extended regular expressions.  It is a \"tagged\" DFA regex engine. It is
inspired by libtre.")
    (license license:bsd-3)))

(define-public ghc-regex-tdfa-text
  (package
    (name "ghc-regex-tdfa-text")
    (version "1.0.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/regex-tdfa-text/"
             "regex-tdfa-text-" version ".tar.gz"))
       (sha256
        (base32
         "0090g6lgbdm9lywpqm2d3724nnnh24nx3vnlqr96qc2w486pmmrq"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-regex-base" ,ghc-regex-base)
       ("ghc-regex-tdfa" ,ghc-regex-tdfa)))
    (home-page
     "https://hackage.haskell.org/package/regex-tdfa-text")
    (synopsis "Text interface for regex-tdfa")
    (description
     "This provides an extra text interface for regex-tdfa.")
    (license license:bsd-3)))

(define-public ghc-repline
  (package
    (name "ghc-repline")
    (version "0.2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://hackage/package/repline/repline-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1ph21kbbanlcs8n5lwk16g9vqkb98mkbz5mzwrp8j2rls2921izc"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/sdiehl/repline")
    (synopsis "Haskeline wrapper for GHCi-like REPL interfaces")
    (description
     "Haskeline wrapper for GHCi-like REPL interfaces.  Composable with
normal mtl transformers.")
    (license license:expat)))

(define-public ghc-repline-0.3
  (package
    (inherit ghc-repline)
    (version "0.3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://hackage/package/repline/repline-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0niihfyggg2qisadg7w49cr5k5qyyynia93iip0ng2bbmzwi88g8"))))
    (inputs
     `(("ghc-exceptions" ,ghc-exceptions)
       ("ghc-haskeline" ,ghc-haskeline-0.8)))))

(define-public ghc-rerebase
  (package
    (name "ghc-rerebase")
    (version "1.3.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/rerebase/rerebase-"
             version ".tar.gz"))
       (sha256
        (base32
         "1jbqif6k249rkknm2zwk8v8jil3kgi9ar53358v8l4ffx346rm82"))))
    (build-system haskell-build-system)
    (outputs '("out" "static" "doc"))
    (inputs
     `(("ghc-rebase" ,ghc-rebase)))
    (home-page "https://github.com/nikita-volkov/rerebase")
    (synopsis "Reexports from ``base'' with many other standard libraries")
    (description "A rich drop-in replacement for @code{base}.  For details and
documentation please visit @uref{https://github.com/nikita-volkov/rerebase,
the project's home page}.")
    (license license:expat)))

(define-public ghc-resolv
  (package
    (name "ghc-resolv")
    (version "0.1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/resolv/resolv-"
             version ".tar.gz"))
       (sha256
        (base32
         "0wczdy3vmpfcfwjn1m95bygc5d83m97xxmavhdvy5ayn8c402fp4"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'update-constraints
           (lambda _
             (substitute* "resolv.cabal"
               (("tasty         >= 1\\.1     && < 1\\.2")
                "tasty         >= 1.1     && < 1.3"))
             #t)))))
    (inputs
     `(("ghc-base16-bytestring" ,ghc-base16-bytestring)))
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)))
    (home-page "https://github.com/haskell/hackage-security")
    (synopsis "Domain Name Service (DNS) lookup via @code{libresolv}")
    (description "This package implements an API for accessing the
@uref{https://tools.ietf.org/html/rfc1035, Domain Name Service (DNS)}
resolver service via the standard @code{libresolv} system library (whose
API is often available directly via the standard @code{libc} C library) on
Unix systems.")
    (license license:gpl3)))

(define-public ghc-resource-pool
  (package
    (name "ghc-resource-pool")
    (version "0.2.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "resource-pool-" version "/"
                           "resource-pool-" version ".tar.gz"))
       (sha256
        (base32
         "04mw8b9djb14zp4rdi6h7mc3zizh597ffiinfbr4m0m8psifw9w6"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-hashable" ,ghc-hashable)
              ("ghc-monad-control" ,ghc-monad-control)
              ("ghc-transformers-base" ,ghc-transformers-base)
              ("ghc-vector" ,ghc-vector)))
    (home-page "https://github.com/bos/pool")
    (synopsis "Striped resource pooling implementation in Haskell")
    (description "This Haskell package provides striped pooling abstraction
for managing flexibly-sized collections of resources such as database
connections.")
    (license license:bsd-3)))

(define-public ghc-resourcet
  (package
    (name "ghc-resourcet")
    (version "1.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/resourcet/"
                           "resourcet-" version ".tar.gz"))
       (sha256
        (base32
         "1rfbfcv3r1h29y0yqr3x6a1s04lbc3vzm3jqnfg4f9rqp9d448qk"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-transformers-base" ,ghc-transformers-base)
       ("ghc-monad-control" ,ghc-monad-control)
       ("ghc-transformers-compat" ,ghc-transformers-compat)
       ("ghc-mmorph" ,ghc-mmorph)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-unliftio-core" ,ghc-unliftio-core)))
    (native-inputs
     `(("ghc-lifted-base" ,ghc-lifted-base)
       ("ghc-hspec" ,ghc-hspec)))
    (home-page "https://github.com/snoyberg/conduit")
    (synopsis "Deterministic allocation and freeing of scarce resources")
    (description "ResourceT is a monad transformer which creates a region of
code where you can safely allocate resources.")
    (license license:bsd-3)))

(define-public ghc-retry
  (package
    (name "ghc-retry")
    (version "0.8.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "retry/retry-" version ".tar.gz"))
       (sha256
        (base32
         "02k03r86amg1vbrsvb644342ym13d9jwkzki9sk93pdg5l8j35dj"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-exceptions" ,ghc-exceptions)
       ("ghc-random" ,ghc-random)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-tasty-hedgehog" ,ghc-tasty-hedgehog)
       ("ghc-hedgehog" ,ghc-hedgehog)))
    (home-page "https://github.com/Soostone/retry")
    (synopsis "Retry combinators for monadic actions that may fail")
    (description "This package exposes combinators that can wrap
arbitrary monadic actions.  They run the action and potentially retry
running it with some configurable delay for a configurable number of
times.  The purpose is to make it easier to work with IO and especially
network IO actions that often experience temporary failure and warrant
retrying of the original action.  For example, a database query may time
out for a while, in which case we should hang back for a bit and retry
the query instead of simply raising an exception.")
    (license license:bsd-3)))

(define-public ghc-rfc5051
  (package
    (name "ghc-rfc5051")
    (version "0.1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/rfc5051/"
                           "rfc5051-" version ".tar.gz"))
       (sha256
        (base32
         "1lxkq414ni986ciml4gbvf463fn55z299knky7pq3ixb1qislpb1"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/rfc5051")
    (synopsis "Simple unicode collation as per RFC5051")
    (description
     "This library implements @code{unicode-casemap}, the simple, non
locale-sensitive unicode collation algorithm described in RFC 5051.  Proper
unicode collation can be done using @code{text-icu}, but that is a big
dependency that depends on a large C library, and @code{rfc5051} might be
better for some purposes.")
    (license license:bsd-3)))

(define-public ghc-rio
  (package
    (name "ghc-rio")
    (version "0.1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/rio/rio-"
             version ".tar.gz"))
       (sha256
        (base32
         "0xzjkh6aavynpyskikhs8dmv0zhkiqiwz9zdn80zbd25b2182pif"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-exceptions" ,ghc-exceptions)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-microlens" ,ghc-microlens)
       ("ghc-primitive" ,ghc-primitive)
       ("ghc-typed-process" ,ghc-typed-process)
       ("ghc-unliftio" ,ghc-unliftio)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-vector" ,ghc-vector)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://github.com/commercialhaskell/rio#readme")
    (synopsis "A standard library for Haskell")
    (description "This package works as a prelude replacement for Haskell,
providing more functionality and types out of the box than the standard
prelude (such as common data types like @code{ByteString} and
@code{Text}), as well as removing common ``gotchas'', like partial
functions and lazy I/O.  The guiding principle here is:
@itemize
@item If something is safe to use in general and has no expected naming
conflicts, expose it.
@item If something should not always be used, or has naming conflicts,
expose it from another module in the hierarchy.
@end itemize")
    (license license:expat)))

(define-public ghc-safe
  (package
    (name "ghc-safe")
    (version "0.3.17")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/safe/safe-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0p3yaf5slvyz1cyy51jq64c5rlp8yzwim8iy2dlnk42if4gc9ibr"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://github.com/ndmitchell/safe#readme")
    (synopsis "Library of safe (exception free) functions")
    (description "This library provides wrappers around @code{Prelude} and
@code{Data.List} functions, such as @code{head} and @code{!!}, that can throw
exceptions.")
    (license license:bsd-3)))

(define-public ghc-safe-exceptions
  (package
    (name "ghc-safe-exceptions")
    (version "0.1.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "safe-exceptions/safe-exceptions-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0sd0zfsm9pcll5bzzj523rbn45adjrnavdkz52hgmdjjgdcdrk8q"))))
    (build-system haskell-build-system)
    (arguments
     '(#:cabal-revision
       ("4" "0fid41gishzsyb47wzxhd5falandfirqcp760hcja81qjpfmqd32")))
    (inputs `(("ghc-exceptions" ,ghc-exceptions)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("ghc-void" ,ghc-void)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://github.com/fpco/safe-exceptions")
    (synopsis "Safe, consistent, and easy exception handling")
    (description "Runtime exceptions - as exposed in @code{base} by the
@code{Control.Exception} module - have long been an intimidating part of the
Haskell ecosystem.  This package is intended to overcome this.  It provides a
safe and simple API on top of the existing exception handling machinery.  The
API is equivalent to the underlying implementation in terms of power but
encourages best practices to minimize the chances of getting the exception
handling wrong.")
    (license license:expat)))

(define-public ghc-safeio
  (package
    (name "ghc-safeio")
    (version "0.0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/safeio/"
                           "safeio-" version ".tar.gz"))
       (sha256
        (base32
         "04g3070cbjdqj0h9l9ii6470xcbn40xfv4fr89a8yvnkdim9nyfm"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-conduit" ,ghc-conduit)
       ("ghc-conduit-combinators" ,ghc-conduit-combinators)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-resourcet" ,ghc-resourcet)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework-th" ,ghc-test-framework-th)))
    (home-page "https://github.com/luispedro/safeio")
    (synopsis "Write output to disk atomically")
    (description
     "This package implements utilities to perform atomic output so as to
avoid the problem of partial intermediate files.")
    (license license:expat)))

(define-public ghc-safesemaphore
  (package
    (name "ghc-safesemaphore")
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "SafeSemaphore/SafeSemaphore-" version ".tar.gz"))
       (sha256
        (base32
         "0rpg9j6fy70i0b9dkrip9d6wim0nac0snp7qzbhykjkqlcvvgr91"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)))
    (home-page "https://github.com/ChrisKuklewicz/SafeSemaphore")
    (synopsis "Exception safe semaphores")
    (description "This library provides exception safe semaphores that can be
used in place of @code{QSem}, @code{QSemN}, and @code{SampleVar}, all of which
are not exception safe and can be broken by @code{killThread}.")
    (license license:bsd-3)))

(define-public ghc-sandi
  (package
    (name "ghc-sandi")
    (version "0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/sandi/sandi-"
             version ".tar.gz"))
       (sha256
        (base32
         "1ndgai8idlxyccvkz5zsgq06v58blc30i6hkky5b1sf5x6gs2h29"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-stringsearch" ,ghc-stringsearch)
       ("ghc-conduit" ,ghc-conduit)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
       ("ghc-tasty-th" ,ghc-tasty-th)))
    (home-page "https://hackage.haskell.org/package/sandi")
    (synopsis "Data encoding library")
    (description "Reasonably fast data encoding library.")
    (license license:bsd-3)))

(define-public ghc-say
  (package
    (name "ghc-say")
    (version "0.1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/say/say-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1r5kffjfwpas45g74sip8glrj1m9nygrnxjm7xgw898rq9pnafgn"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)
       ("ghc-unliftio" ,ghc-unliftio)))
    (home-page "https://github.com/fpco/say")
    (synopsis
     "Send textual messages to a Handle in a thread-friendly way")
    (description
     "A thread safe API to write a line of textual data to a Handle, such
as sending some messages to the terminal - that has the following properties:
@itemize
@item Properly handle character encoding settings on the Handle
@item For reasonably sized messages, ensure that the entire message is written
 in one chunk to avoid interleaving data with other threads
@item Avoid unnecessary memory allocations and copies
@item Minimize locking.
@end itemize")
    (license license:expat)))

(define-public ghc-scientific
  (package
    (name "ghc-scientific")
    (version "0.3.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/scientific/scientific-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "03ql2f0ac8bsl524idy9xxa3kxisb2sj3avflzw580j5hzy0m397"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-integer-logarithms" ,ghc-integer-logarithms)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-primitive" ,ghc-primitive)))
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-ant-xml" ,ghc-tasty-ant-xml)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-tasty-smallcheck" ,ghc-tasty-smallcheck)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
       ("ghc-smallcheck" ,ghc-smallcheck)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://github.com/basvandijk/scientific")
    (synopsis "Numbers represented using scientific notation")
    (description "This package provides @code{Data.Scientific}, which provides
the number type @code{Scientific}.  Scientific numbers are arbitrary precision
and space efficient.  They are represented using
@uref{https://en.wikipedia.org/wiki/Scientific_notation, scientific
notation}.")
    (license license:bsd-3)))

(define-public ghc-scientific-bootstrap
  (package
    (inherit ghc-scientific)
    (name "ghc-scientific-bootstrap")
    (arguments `(#:tests? #f))
    (inputs
     `(("ghc-integer-logarithms" ,ghc-integer-logarithms-bootstrap)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-primitive" ,ghc-primitive)))
    (native-inputs '())
    (properties '((hidden? #t)))))

(define-public ghc-sdl
  (package
    (name "ghc-sdl")
    (version "0.6.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/SDL/SDL-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "00y67v80a8l09i3k76z09lg25kw72ivl09nag8ckdlk4a0cfnzfq"))))
    (build-system haskell-build-system)
    (inputs
     `(("sdl" ,sdl)))
    (home-page "https://hackage.haskell.org/package/SDL")
    (synopsis "LibSDL for Haskell")
    (description "Simple DirectMedia Layer (libSDL) is a cross-platform
multimedia library designed to provide low level access to audio, keyboard,
mouse, joystick, 3D hardware via OpenGL, and 2D video framebuffer.  It is used
by MPEG playback software, emulators, and many popular games, including the
award winning Linux port of \"Civilization: Call To Power.\"")
    (license license:bsd-3)))

(define-public ghc-sdl2
  (package
    (name "ghc-sdl2")
    (version "2.5.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "sdl2/sdl2-" version ".tar.gz"))
       (sha256
        (base32
         "1x368yhdd55b3cvx8dvj1sj6nzygzigbhrwhssjs4k0rcxlwqfw8"))))
    (build-system haskell-build-system)
    (arguments '(#:tests? #f)) ; tests require graphical environment
    (inputs
     `(("ghc-exceptions" ,ghc-exceptions)
       ("ghc-linear" ,ghc-linear)
       ("ghc-statevar" ,ghc-statevar)
       ("ghc-vector" ,ghc-vector)
       ("sdl2" ,sdl2)))
    (native-inputs
     `(("ghc-weigh" ,ghc-weigh)
       ("pkg-config" ,pkg-config)))
    (home-page "https://hackage.haskell.org/package/sdl2")
    (synopsis "High- and low-level bindings to the SDL library")
    (description
     "This package contains bindings to the SDL 2 library, in both high- and
low-level forms.  The @code{SDL} namespace contains high-level bindings, where
enumerations are split into sum types, and we perform automatic
error-checking.  The @code{SDL.Raw} namespace contains an almost 1-1
translation of the C API into Haskell FFI calls.  As such, this does not
contain sum types nor error checking.  Thus this namespace is suitable for
building your own abstraction over SDL, but is not recommended for day-to-day
programming.")
    (license license:bsd-3)))

(define-public ghc-sdl2-image
  (package
    (name "ghc-sdl2-image")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/sdl2-image/"
                           "sdl2-image-" version ".tar.gz"))
       (sha256
        (base32
         "1pr6dkg73cy9z0w54lrkj9c5bhxj56nl92lxikjy8kz6nyr455rr"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-sdl2" ,ghc-sdl2)
       ("sdl2-image" ,sdl2-image)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://hackage.haskell.org/package/sdl2-image")
    (synopsis "Bindings to SDL2_image")
    (description "This package provides Haskell bindings to
@code{SDL2_image}.")
    (license license:expat)))

(define-public ghc-sdl2-mixer
  (package
    (name "ghc-sdl2-mixer")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/sdl2-mixer/"
                           "sdl2-mixer-" version ".tar.gz"))
       (sha256
        (base32
         "1k8avyccq5l9z7bwxigim312yaancxl1sr3q6a96bcm7pnhiak0g"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-data-default-class" ,ghc-data-default-class)
       ("ghc-lifted-base" ,ghc-lifted-base)
       ("ghc-monad-control" ,ghc-monad-control)
       ("ghc-sdl2" ,ghc-sdl2)
       ("ghc-vector" ,ghc-vector)
       ("sdl2-mixer" ,sdl2-mixer)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://hackage.haskell.org/package/sdl2-mixer")
    (synopsis "Bindings to SDL2 mixer")
    (description "This package provides Haskell bindings to
@code{SDL2_mixer}.")
    (license license:bsd-3)))

(define-public ghc-sdl-image
  (package
    (name "ghc-sdl-image")
    (version "0.6.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/SDL-image/SDL-image-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1gxwrvswgwjw6g7ym52gik22l9l3ljy592phv97jdmcf3gi6qcg1"))))
    (build-system haskell-build-system)
    (arguments
     `(#:configure-flags
       (let* ((sdl-image (assoc-ref %build-inputs "sdl-image"))
              (sdl-image-include (string-append sdl-image "/include/SDL")))
         (list (string-append "--extra-include-dirs=" sdl-image-include)))))
    (inputs
     `(("ghc-sdl" ,ghc-sdl)
       ("sdl-image" ,sdl-image)))
    (home-page "https://hackage.haskell.org/package/SDL-image")
    (synopsis "Haskell bindings to libSDL_image")
    (description "SDL_image is an image file loading library.  It loads images
as SDL surfaces, and supports the following formats: BMP, GIF, JPEG, LBM, PCX,
PNG, PNM, TGA, TIFF, XCF, XPM, XV.")
    (license license:bsd-3)))

(define-public ghc-sdl-mixer
  (package
    (name "ghc-sdl-mixer")
    (version "0.6.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/SDL-mixer/SDL-mixer-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0k26hqgdh789ka3mv4dsk6rin6x6vwcs6hjmnsqq7j3mnrh1342r"))))
    (build-system haskell-build-system)
    (arguments
     `(#:configure-flags
       (let* ((sdl-mixer (assoc-ref %build-inputs "sdl-mixer"))
              (sdl-mixer-include (string-append sdl-mixer "/include/SDL")))
         (list (string-append "--extra-include-dirs=" sdl-mixer-include)))))
    (inputs
     `(("ghc-sdl" ,ghc-sdl)
       ("sdl-mixer" ,sdl-mixer)))
    (home-page "https://hackage.haskell.org/package/SDL-mixer")
    (synopsis "Haskell bindings to libSDL_mixer")
    (description "SDL_mixer is a sample multi-channel audio mixer library.  It
supports any number of simultaneously playing channels of 16 bit stereo audio,
plus a single channel of music, mixed by the popular MikMod MOD, Timidity
MIDI, Ogg Vorbis, and SMPEG MP3 libraries.")
    (license license:bsd-3)))

(define-public ghc-securemem
  (package
    (name "ghc-securemem")
    (version "0.1.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "securemem-" version "/"
                           "securemem-" version ".tar.gz"))
       (sha256
        (base32
         "19hnw2cfbsfjynxq1bq9f6djbxhsc1k751ml0y1ab3ah913mm29j"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-byteable" ,ghc-byteable)
              ("ghc-memory" ,ghc-memory)))
    (home-page "https://github.com/vincenthz/hs-securemem")
    (synopsis "Auto-scrubbing and const-time-eq memory chunk abstraction for
Haskell")
    (description "SecureMem is similar to ByteString, except that it provides
a memory chunk that will be auto-scrubbed after it run out of scope.")
    (license license:bsd-3)))

(define-public ghc-semialign
  (package
    (name "ghc-semialign")
    (version "1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/semialign/semialign-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "004x0a80sqqdgvsyk4z0nasxpi6z3g1d8kgwj804bj9ka8dlc75m"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-these" ,ghc-these)
       ("ghc-base-compat" ,ghc-base-compat)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-tagged" ,ghc-tagged)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-vector" ,ghc-vector)
       ("ghc-semigroupoids" ,ghc-semigroupoids)))
    (arguments
     `(#:cabal-revision
       ("1"
        "0qnqnyfng4kwy2h2anrcy5id2ijnawava3zcc5h5b8ri1y6ks6zi")))
    (home-page
     "https://github.com/isomorphism/these")
    (synopsis
     "Align and Zip type-classes from the common Semialign ancestor ")
    (description
     "The major use of @code{These} of this is provided by the
@code{align} member of @code{Semialign} class, representing a
generalized notion of \"zipping with padding\" that combines
structures without truncating to the size of the smaller input.  It
turns out that @code{zip} operation fits well the @code{Semialign}
class, forming lattice-like structure.")
    (license license:bsd-3)))

(define-public ghc-semigroupoids
  (package
    (name "ghc-semigroupoids")
    (version "5.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/semigroupoids/semigroupoids-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "016hc4imr9l4szs3p7f1aahvxr5wv4clvr3qzrm3nibssg5vrs61"))))
    (build-system haskell-build-system)
    (outputs '("out" "static" "doc"))
    (inputs
     `(("ghc-base-orphans" ,ghc-base-orphans)
       ("ghc-transformers-compat" ,ghc-transformers-compat)
       ("ghc-bifunctors" ,ghc-bifunctors)
       ("ghc-comonad" ,ghc-comonad)
       ("ghc-contravariant" ,ghc-contravariant)
       ("ghc-distributive" ,ghc-distributive)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-tagged" ,ghc-tagged)
       ("ghc-unordered-containers" ,ghc-unordered-containers)))
    (native-inputs
     `(("cabal-doctest" ,cabal-doctest)
       ("ghc-doctest" ,ghc-doctest)))
    (home-page "https://github.com/ekmett/semigroupoids")
    (synopsis "Semigroupoids operations for Haskell")
    (description "This library provides a wide array of (semi)groupoids and
operations for working with them.  A @code{Semigroupoid} is a @code{Category}
without the requirement of identity arrows for every object in the category.
A @code{Category} is any @code{Semigroupoid} for which the Yoneda lemma holds.
Finally, to work with these weaker structures it is beneficial to have
containers that can provide stronger guarantees about their contents, so
versions of @code{Traversable} and @code{Foldable} that can be folded with
just a @code{Semigroup} are added.")
    (license license:bsd-3)))

(define-public ghc-semigroups
  (package
    (name "ghc-semigroups")
    (version "0.18.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/semigroups/semigroups-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "17g29h62g1k51ghhvzkw72zksjgi6vs6bfipkj81pqw1dsprcamb"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-nats" ,ghc-nats)
       ("ghc-tagged" ,ghc-tagged)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-hashable" ,ghc-hashable)))
    (home-page "https://github.com/ekmett/semigroups/")
    (synopsis "Semigroup operations for Haskell")
    (description "This package provides semigroups for Haskell.  In
mathematics, a semigroup is an algebraic structure consisting of a set
together with an associative binary operation.  A semigroup generalizes a
monoid in that there might not exist an identity element.  It
also (originally) generalized a group (a monoid with all inverses) to a type
where every element did not have to have an inverse, thus the name
semigroup.")
    (license license:bsd-3)))

(define-public ghc-semigroups-bootstrap
  (package
    (inherit ghc-semigroups)
    (name "ghc-semigroups-bootstrap")
    (inputs
     `(("ghc-nats" ,ghc-nats-bootstrap)
       ("ghc-tagged" ,ghc-tagged)
       ("ghc-unordered-containers" ,ghc-unordered-containers-bootstrap)
       ("ghc-hashable" ,ghc-hashable-bootstrap)))
    (properties '((hidden? #t)))))

(define-public ghc-serialise
  (package
    (name "ghc-serialise")
    (version "0.2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://hackage/package/serialise/serialise-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "19ary6ivzk8z7wcxhm860qmh7pwqj0qjqzav1h42y85l608zqgh4"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-cborg" ,ghc-cborg)
       ("ghc-half" ,ghc-half)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-primitive" ,ghc-primitive)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-vector" ,ghc-vector)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
       ("ghc-quickcheck-instances" ,ghc-quickcheck-instances)))
    (arguments
     `(#:cabal-revision
       ("1" "1rknhad1i8bpknsnphmcmb6dnb48c2p2c13ia2qqch3hkhsvfpr6")))
    (home-page "https://github.com/well-typed/cborg")
    (synopsis "Binary serialisation library for Haskell values")
    (description
     "This package (formerly binary-serialise-cbor) provides pure,
efficient serialization of Haskell values directly into ByteStrings for
storage or transmission purposes.  By providing a set of type class instances,
you can also serialise any custom data type you have as well.

The underlying binary format used is the 'Concise Binary Object
Representation', or CBOR, specified in RFC 7049.  As a result, serialised
Haskell values have implicit structure outside of the Haskell program itself,
meaning they can be inspected or analyzed without custom tools.

An implementation of the standard bijection between CBOR and JSON is
provided by the https://hackage.haskell.org/package/cborg-json
package.  Also see https://hackage.haskell.org/package/cbor-tool for a
convenient command-line utility for working with CBOR data.")
    (license license:bsd-3)))

(define-public ghc-setenv
  (package
    (name "ghc-setenv")
    (version "0.1.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/setenv/setenv-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0cnbgrvb9byyahb37zlqrj05rj25v190crgcw8wmlgf0mwwxyn73"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/setenv")
    (synopsis "Library for setting environment variables")
    (description "This package provides a Haskell library for setting
environment variables.")
    (license license:expat)))

(define-public ghc-setlocale
  (package
    (name "ghc-setlocale")
    (version "1.0.0.9")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://hackage.haskell.org/package/setlocale-"
                    version "/setlocale-" version ".tar.gz"))
              (sha256
               (base32
                "18b6xafspzxrmz5m9r9nzy3z053crqi59xc8n8aqd4gw0pvqdcrv"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/setlocale")
    (synopsis "Haskell bindings to setlocale")
    (description "This package provides Haskell bindings to the
@code{setlocale} C function.")
    (license license:bsd-3)))

(define-public ghc-shakespeare
  (package
    (name "ghc-shakespeare")
    (version "2.0.22")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "shakespeare-" version "/"
                           "shakespeare-" version ".tar.gz"))
       (sha256
        (base32
         "1mc1a0vv070gcawwcx6vzpj6gpfh1qnlqrndiyfic3p500y656vh"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-aeson" ,ghc-aeson)
              ("ghc-blaze-markup" ,ghc-blaze-markup)
              ("ghc-blaze-html" ,ghc-blaze-html)
              ("ghc-exceptions" ,ghc-exceptions)
              ("ghc-vector" ,ghc-vector)
              ("ghc-unordered-containers" ,ghc-unordered-containers)
              ("ghc-scientific" ,ghc-scientific)))
    (native-inputs `(("ghc-hspec" ,ghc-hspec)
                     ("ghc-hunit" ,ghc-hunit)
                     ("hspec-discover" ,hspec-discover)))
    (home-page "https://www.yesodweb.com/book/shakespearean-templates")
    (synopsis "Family of type-safe template languages for Haskell")
    (description "This Haskell package provides a family of type-safe
templates with simple variable interpolation.  Shakespeare templates can
be used inline with a quasi-quoter or in an external file and it
interpolates variables according to the type being inserted.")
    (license license:expat)))

(define-public ghc-shelly
  (package
    (name "ghc-shelly")
    (version "1.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/shelly/shelly-"
             version ".tar.gz"))
       (sha256
        (base32
         "023fbvbqs5gdwm30j5517gbdcc7fvz0md70dgwgpypkskj3i926y"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-unix-compat" ,ghc-unix-compat)
       ("ghc-system-filepath-bootstrap" ,ghc-system-filepath-bootstrap)
       ("ghc-system-fileio-bootstrap" ,ghc-system-fileio-bootstrap)
       ("ghc-monad-control" ,ghc-monad-control)
       ("ghc-lifted-base" ,ghc-lifted-base)
       ("ghc-lifted-async" ,ghc-lifted-async)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-enclosed-exceptions" ,ghc-enclosed-exceptions)
       ("ghc-async" ,ghc-async)
       ("ghc-transformers-base" ,ghc-transformers-base)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-hspec-contrib" ,ghc-hspec-contrib)))
    (home-page "https://github.com/yesodweb/Shelly.hs")
    (synopsis "Shell-like (systems) programming in Haskell")
    (description
     "Shelly provides convenient systems programming in Haskell, similar in
spirit to POSIX shells.  Shelly is originally forked  from the Shellish package.")
    (license license:bsd-3)))

(define-public ghc-silently
  (package
    (name "ghc-silently")
    (version "1.2.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/silently/silently-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1lgs1gsr5dp0x21diqn4l03fxgai2kgdmj85gqp0iz3zykvbmjbz"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f)) ;; circular dependency with nanospec
    ;; (inputs
    ;;  `(("ghc-temporary" ,ghc-temporary)))
    (home-page "https://github.com/hspec/silently")
    (synopsis "Prevent writing to stdout")
    (description "This package provides functions to prevent or capture
writing to stdout and other handles.")
    (license license:bsd-3)))

(define-public ghc-simple-reflect
  (package
    (name "ghc-simple-reflect")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/simple-reflect/simple-reflect-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0ayvrx5cm8n6db21jiyjmk5h93pw7cz1707hih09hlhk9jh5x0h7"))))
    (build-system haskell-build-system)
    (home-page
     "https://twanvl.nl/blog/haskell/simple-reflection-of-expressions")
    (synopsis
     "Simple reflection of expressions containing variables")
    (description
     "This package allows simple reflection of expressions containing
variables.  Reflection here means that a Haskell expression is turned into a
string.  The primary aim of this package is teaching and understanding; there
are no options for manipulating the reflected expressions beyond showing
them.")
    (license license:bsd-3)))

(define-public ghc-simple-sendfile
  (package
    (name "ghc-simple-sendfile")
    (version "0.2.30")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "simple-sendfile-" version "/"
                           "simple-sendfile-" version ".tar.gz"))
       (sha256
        (base32
         "112j0qfsjazf9wg1zywf7hjybgsiywk9wkm27yi8xzv27hmlv1mn"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-conduit" ,ghc-conduit)
       ("ghc-conduit-extra" ,ghc-conduit-extra)
       ("ghc-network" ,ghc-network)
       ("ghc-resourcet" ,ghc-resourcet)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://github.com/kazu-yamamoto/simple-sendfile")
    (synopsis "Cross platform library for the sendfile system call")
    (description "This library tries to call minimum system calls which
are the bottleneck of web servers.")
    (license license:bsd-3)))

(define-public ghc-size-based
  (package
    (name "ghc-size-based")
    (version "0.1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "size-based/size-based-" version ".tar.gz"))
       (sha256
        (base32
         "06hmlic0n73ncwlkpx49xlv09bzsrr27ncnp5byhzlknak2gd7vp"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-dictionary-sharing" ,ghc-dictionary-sharing)
       ("ghc-testing-type-modifiers" ,ghc-testing-type-modifiers)
       ("ghc-semigroups" ,ghc-semigroups)))
    (arguments
     `(#:cabal-revision
       ("1" "0kax1ypjyglkn6iff1x4yz12y7f2n249m95xvdhrc63hsa4xlcqv")))
    (home-page "https://hackage.haskell.org/package/size-based")
    (synopsis "Sized functors for size-based enumerations")
    (description "This library provides a framework for size-based
enumerations.")
    (license license:bsd-3)))

(define-public ghc-skylighting-core
  (package
    (name "ghc-skylighting-core")
    (version "0.8.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "skylighting-core/skylighting-core-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0hdchivb4af9w7v5v7lrwfwawd3kcwmpzk69m1vkkm3pis8lcr1s"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-aeson" ,ghc-aeson)
       ("ghc-ansi-terminal" ,ghc-ansi-terminal)
       ("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-base64-bytestring" ,ghc-base64-bytestring)
       ("ghc-blaze-html" ,ghc-blaze-html)
       ("ghc-case-insensitive" ,ghc-case-insensitive)
       ("ghc-colour" ,ghc-colour)
       ("ghc-hxt" ,ghc-hxt)
       ("ghc-regex-pcre-builtin" ,ghc-regex-pcre-builtin)
       ("ghc-safe" ,ghc-safe)
       ("ghc-utf8-string" ,ghc-utf8-string)))
    (native-inputs
     `(("ghc-diff" ,ghc-diff)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-pretty-show" ,ghc-pretty-show)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-golden" ,ghc-tasty-golden)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)))
    (home-page "https://github.com/jgm/skylighting")
    (synopsis "Syntax highlighting library")
    (description "Skylighting is a syntax highlighting library with support
for over one hundred languages.  It derives its tokenizers from XML syntax
definitions used by KDE's @code{KSyntaxHighlighting} framework, so any syntax
supported by that framework can be added.  An optional command-line program is
provided.  Skylighting is intended to be the successor to highlighting-kate.")
    (license license:gpl2)))

(define-public ghc-skylighting
  (package
    (inherit ghc-skylighting-core)
    (name "ghc-skylighting")
    (version "0.8.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/skylighting-"
                                  version "/skylighting-" version ".tar.gz"))
              (sha256
               (base32
                "1xls8ycad77m55ax4hp55k60h3pi5sm3m32hycbc8baixbgfx5xz"))))
    (inputs
     `(("ghc-skylighting-core" ,ghc-skylighting-core)
       ,@(package-inputs ghc-skylighting-core)))))

(define-public ghc-smallcheck
  (package
    (name "ghc-smallcheck")
    (version "1.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/smallcheck/smallcheck-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "195fj7w3v03d1y1nm2ylavzrwxjcdbq0lb6zsw1dwyx5jmwfc84h"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-logict" ,ghc-logict)))
    (home-page
     "https://github.com/feuerbach/smallcheck")
    (synopsis "Property-based testing library")
    (description "SmallCheck is a testing library that verifies
properties for all test cases up to some depth.  The test cases are generated
automatically by SmallCheck.")
    (license license:bsd-3)))

(define-public ghc-socks
  (package
    (name "ghc-socks")
    (version "0.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "socks/socks-" version ".tar.gz"))
              (sha256
               (base32
                "0wvaxy3dkv97wrncjv1rxrmjr4014hgxz82kixvcwqdhidalfi3k"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-cereal" ,ghc-cereal)
       ("ghc-basement" ,ghc-basement)
       ("ghc-network" ,ghc-network)))
    (home-page "https://github.com/vincenthz/hs-socks")
    (synopsis "SOCKS proxy (version 5) implementation")
    (description
     "This library provides a SOCKS proxy (version 5) implementation.")
    (license license:bsd-3)))

(define-public ghc-sop-core
  (package
    (name "ghc-sop-core")
    (version "0.4.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "sop-core/sop-core-" version ".tar.gz"))
       (sha256
        (base32
         "07ci2mh8cbjvipb576rxsj3iyhkj5c5dnsns4xkdppp2p3pv10d3"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/sop-core")
    (synopsis "True Sums of Products")
    (description "This package provides an implementation of
@math{n}-ary sums and @math{n}-ary products.  The module @code{Data.SOP}
is the main module of this library and contains more detailed
documentation.  The main use case of this package is to serve as the
core of @url{https://hackage.haskell.org/package/generics-sop,
generics-sop}.")
    (license license:bsd-3)))

(define-public ghc-special-values
  (package
    (name "ghc-special-values")
    (version "0.1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/special-values/"
             "special-values-" version ".tar.gz"))
       (sha256
        (base32
         "1kkdw2c4d2hha99v9f89ahmifjxp7fxmxyfwq9a8xk6s0h9xs51w"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-scientific" ,ghc-scientific)
       ("ghc-ieee754" ,ghc-ieee754)
       ("ghc-nats" ,ghc-nats)))
    (home-page
     "https://github.com/minad/special-values#readme")
    (synopsis "Typeclass providing special values")
    (description
     "Special values are provided by a SpecialValues typeclass.  Those can be
used for example by QuickCheck, see quickcheck-special."  )
    (license license:expat)))

(define-public ghc-split
  (package
    (name "ghc-split")
    (version "0.2.3.3")
    (outputs '("out" "static" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/split/split-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "04qlmkcyklznl03gsjg95b4nzb6i96gdapqg60rny9szgi7ngk8x"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("2" "1c8bcssxq5rkxkixgms6w6x6lzf4n7cxk6cx6av1dp3lixdy9j34")))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://hackage.haskell.org/package/split")
    (synopsis "Combinator library for splitting lists")
    (description "This package provides a collection of Haskell functions for
splitting lists into parts, akin to the @code{split} function found in several
mainstream languages.")
    (license license:bsd-3)))

(define-public ghc-splitmix
  (package
    (name "ghc-splitmix")
    (version "0.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "splitmix/splitmix-" version ".tar.gz"))
       (sha256
        (base32
         "1k0amgkz7rvyz3lnw7m786ilnr1cibwhx9sc4qynq329gxan5r7w"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-random" ,ghc-random)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-async" ,ghc-async)
       ("ghc-base-compat-batteries" ,ghc-base-compat-batteries)
       ("ghc-tf-random" ,ghc-tf-random)
       ("ghc-vector" ,ghc-vector)))
    (home-page "https://hackage.haskell.org/package/splitmix")
    (synopsis "Fast and splittable pseudorandom number generator")
    (description "This package provides a Pure Haskell implementation of the
SplitMix pseudorandom number generator.  SplitMix is a \"splittable\"
pseudorandom number generator that is quite fast: 9 64-bit
arithmetic/logical operations per 64 bits generated.  SplitMix is tested
with two standard statistical test suites (DieHarder and TestU01, this
implementation only using the former) and it appears to be adequate for
\"everyday\" use, such as Monte Carlo algorithms and randomized data
structures where speed is important.  In particular, it @strong{should not
be used for cryptographic or security applications}, because generated
sequences of pseudorandom values are too predictable (the mixing functions
are easily inverted, and two successive outputs suffice to reconstruct the
internal state).")
    (license license:bsd-3)))

(define-public ghc-splitmix-bootstrap
  (package
    (inherit ghc-splitmix)
    (name "ghc-splitmix-bootstrap")
    (arguments `(#:tests? #f))
    (native-inputs '())
    (properties '((hidden? #t)))))

(define-public ghc-spoon
  (package
    (name "ghc-spoon")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/spoon/spoon-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1m41k0mfy6fpfrv2ym4m5jsjaj9xdfl2iqpppd3c4d0fffv51cxr"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("1"
        "09s5jjcsg4g4qxchq9g2l4i9d5zh3rixpkbiysqcgl69kj8mwv74")))
    (home-page
     "http://hackage.haskell.org/package/spoon")
    (synopsis
     "Catch errors thrown from pure computations")
    (description
     "Takes an error-throwing expression and puts it back in the Maybe it
belongs in.

Note that this suffers from the
@url{https://ghc.haskell.org/trac/ghc/ticket/5902}.  Buyer beware.")
    (license license:bsd-3)))

(define-public ghc-statevar
  (package
    (name "ghc-statevar")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/StateVar/StateVar-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "12sz6zkc9j5z3lwrjvljrkfxa5vhwnir5wsarigz2f6d3w13dh5g"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/StateVar")
    (synopsis "State variables for Haskell")
    (description "This package provides state variables, which are references
in the @code{IO} monad, like @code{IORef}s or parts of the OpenGL state.")
    (license license:bsd-3)))

(define-public ghc-statistics
  (package
    (name "ghc-statistics")
    (version "0.15.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "statistics-" version "/"
                           "statistics-" version ".tar.gz"))
       (sha256
        (base32
         "015rn74f1glii26j4b2fh1fc63xvxzrh2xckiancz48kc8jdzabj"))))
    (build-system haskell-build-system)
    (arguments
     '(;; Two tests fail: "Discrete CDF is OK" and "Quantile is CDF inverse".
       #:tests? #f))
    (inputs
     `(("ghc-aeson" ,ghc-aeson)
       ("ghc-async" ,ghc-async)
       ("ghc-base-orphans" ,ghc-base-orphans)
       ("ghc-dense-linear-algebra" ,ghc-dense-linear-algebra)
       ("ghc-math-functions" ,ghc-math-functions)
       ("ghc-monad-par" ,ghc-monad-par)
       ("ghc-mwc-random" ,ghc-mwc-random)
       ("ghc-primitive" ,ghc-primitive)
       ("ghc-vector" ,ghc-vector)
       ("ghc-vector-algorithms" ,ghc-vector-algorithms)
       ("ghc-vector-th-unbox" ,ghc-vector-th-unbox)
       ("ghc-vector-binary-instances" ,ghc-vector-binary-instances)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-ieee754" ,ghc-ieee754)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)))
    (home-page "https://github.com/bos/mwc-random")
    (synopsis "Haskell library of statistical types, data, and functions")
    (description "This library provides a number of common functions
and types useful in statistics.  We focus on high performance, numerical
robustness, and use of good algorithms.  Where possible, we provide references
to the statistical literature.

The library's facilities can be divided into four broad categories:

@itemize
@item Working with widely used discrete and continuous probability
distributions.  (There are dozens of exotic distributions in use; we focus
on the most common.)

@item Computing with sample data: quantile estimation, kernel density
estimation, histograms, bootstrap methods, significance testing,
and regression and autocorrelation analysis.

@item Random variate generation under several different distributions.

@item Common statistical tests for significant differences between samples.
@end itemize")
    (license license:bsd-2)))

(define-public ghc-stm-chans
  (package
    (name "ghc-stm-chans")
    (version "3.0.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "stm-chans-" version "/"
                           "stm-chans-" version ".tar.gz"))
       (sha256
        (base32
         "0f27sp09yha43xk9q55sc185jyjs5h7gq2dhsyx6bm9kz9dzqi13"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/stm-chans")
    (synopsis "Additional types of channels for ghc-stm")
    (description "This Haskell package offers a collection of channel types,
similar to @code{Control.Concurrent.STM.@{TChan,TQueue@}} but with additional
features.")
    (license license:bsd-3)))

(define-public ghc-stm-conduit
  (package
    (name "ghc-stm-conduit")
    (version "4.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/stm-conduit/"
                           "stm-conduit-" version ".tar.gz"))
       (sha256
        (base32
         "0hhlxvpp7mah8dcvkknh6skx44jfk3092zz2w52zlr255bkmn3p8"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-stm-chans" ,ghc-stm-chans)
       ("ghc-cereal" ,ghc-cereal)
       ("ghc-cereal-conduit" ,ghc-cereal-conduit)
       ("ghc-conduit" ,ghc-conduit)
       ("ghc-conduit-extra" ,ghc-conduit-extra)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-resourcet" ,ghc-resourcet)
       ("ghc-async" ,ghc-async)
       ("ghc-monad-loops" ,ghc-monad-loops)
       ("ghc-unliftio" ,ghc-unliftio)))
    (native-inputs
     `(("ghc-doctest" ,ghc-doctest)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)))
    (home-page "https://github.com/cgaebel/stm-conduit")
    (synopsis "Introduces conduits to channels and promotes using conduits concurrently")
    (description
     "This package provides two simple conduit wrappers around STM channels: a
source and a sink.")
    (license license:bsd-3)))

(define-public ghc-stmonadtrans
  (package
    (name "ghc-stmonadtrans")
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/STMonadTrans"
                           "/STMonadTrans-" version ".tar.gz"))
       (sha256
        (base32 "1nr26fnmi5fdjc6d00w13kjhmfyvb5b837d0006w4dj0yxndaksp"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/STMonadTrans")
    (synopsis "Monad transformer version of the ST monad")
    (description
     "This package provides a monad transformer version of the @code{ST} monad
for strict state threads.")
    (license license:bsd-3)))

(define-public ghc-storable-complex
  (package
    (name "ghc-storable-complex")
    (version "0.2.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/storable-complex/storable-complex-"
             version ".tar.gz"))
       (sha256
        (base32 "0fnwbfmd5vsaaqvf9182qdcjrzcfjd1zhdyvjwzifbwvn6r9kx4s"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-base-orphans" ,ghc-base-orphans)))
    (home-page "https://github.com/cartazio/storable-complex")
    (synopsis "Haskell Storable instance for Complex")
    (description "This package provides a Haskell library including a
Storable instance for Complex which is binary compatible with C99, C++
and Fortran complex data types.")
    (license license:bsd-3)))

(define-public ghc-storable-record
  (package
    (name "ghc-storable-record")
    (version "0.0.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://hackage.haskell.org/package/storable-record/"
         "storable-record-" version ".tar.gz"))
       (sha256
        (base32
         "0hjs1km0fc9ch0i1rbycxia5w3939hk4p4md73ikgg4aipqb5zyf"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-semigroups" ,ghc-semigroups)
       ("ghc-utility-ht" ,ghc-utility-ht)
       ("ghc-storablevector" ,ghc-storablevector)
       ("ghc-timeit" ,ghc-timeit)))
    (home-page "https://hackage.haskell.org/package/storable-record")
    (synopsis "Elegant definition of Storable instances for records")
    (description "With this package you can build a Storable instance of
a record type from Storable instances of its elements in an elegant way.
It does not do any magic, just a bit arithmetic to compute the right
offsets, that would be otherwise done manually or by a preprocessor like
C2HS.  There is no guarantee that the generated memory layout is
compatible with that of a corresponding C struct.  However, the module
generates the smallest layout that is possible with respect to the
alignment of the record elements.")
    (license license:bsd-3)))

(define-public ghc-storable-tuple
  (package
    (name "ghc-storable-tuple")
    (version "0.0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://hackage.haskell.org/package/storable-tuple/"
         "storable-tuple-" version ".tar.gz"))
       (sha256
        (base32
         "0dfzhxgkn1l6ls7zh6iifhyvhm8l47n40z0ar23c6ibsa94w1ynw"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-storable-record" ,ghc-storable-record)
       ("ghc-utility-ht" ,ghc-utility-ht)
       ("ghc-base-orphans" ,ghc-base-orphans)))
    (home-page "https://hackage.haskell.org/package/storable-tuple")
    (synopsis "Storable instance for pairs and triples")
    (description "This package provides a Storable instance for pairs
and triples which should be binary compatible with C99 and C++.  The
only purpose of this package is to provide a standard location for this
instance so that other packages needing this instance can play nicely
together.")
    (license license:bsd-3)))

(define-public ghc-storablevector
  (package
    (name "ghc-storablevector")
    (version "0.2.13")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://hackage.haskell.org/package/storablevector/storablevector-"
         version ".tar.gz"))
       (sha256
        (base32
         "1zmr738vwnhnyxbikayqnaz31ilv2qlmscp6iqgl7adcfbal4dzq"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-non-negative" ,ghc-non-negative)
       ("ghc-utility-ht" ,ghc-utility-ht)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-unsafe" ,ghc-unsafe)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-syb" ,ghc-syb)))
    (home-page "https://wiki.haskell.org/Storable_Vector")
    (synopsis "Fast, packed, strict storable arrays with a list interface")
    (description "This library provides fast, packed, strict storable
arrays with a list interface, a chunky lazy list interface with variable
chunk size and an interface for write access via the ST monad.  This is
much like bytestring and binary but can be used for every
@code{Foreign.Storable.Storable} type.  See also
@url{http://hackage.haskell.org/package/vector}, a library with a
similar intention.

This library does not do advanced fusion optimization, since especially
for lazy vectors this would either be incorrect or not applicable.  See
@url{http://hackage.haskell.org/package/storablevector-streamfusion} for
a library that provides fusion with lazy lists.")
    (license license:bsd-3)))

(define-public ghc-streaming-commons
  (package
    (name "ghc-streaming-commons")
    (version "0.2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "streaming-commons/streaming-commons-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1lmyx3wkjsayhy5yilzvy0kf8qwmycwlk26r1d8f3cxbfhkr7s52"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-async" ,ghc-async)
       ("ghc-blaze-builder" ,ghc-blaze-builder)
       ("ghc-network" ,ghc-network)
       ("ghc-random" ,ghc-random)
       ("ghc-zlib" ,ghc-zlib)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://hackage.haskell.org/package/streaming-commons")
    (synopsis "Conduit and pipes needed by some streaming data libraries")
    (description "This package provides low-dependency functionality commonly
needed by various Haskell streaming data libraries, such as @code{conduit} and
@code{pipe}s.")
    (license license:expat)))

(define-public ghc-strict
  (package
    (name "ghc-strict")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/strict/strict-"
                           version ".tar.gz"))
       (sha256
        (base32 "08cjajqz9h47fkq98mlf3rc8n5ghbmnmgn8pfsl3bdldjdkmmlrc"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/strict")
    (synopsis "Strict data types and String IO")
    (description
     "This package provides strict versions of some standard Haskell data
types, such as pairs, @code{Maybe} and @code{Either}.  It also contains strict
IO operations.")
    (license license:bsd-3)))

(define-public ghc-stringbuilder
  (package
    (name "ghc-stringbuilder")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/stringbuilder/stringbuilder-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1fh3csx1wcssn8xyvl4ip4aprh9l4qyz2kk8mgjvqvc0vb2bsy6q"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f)) ; FIXME: circular dependencies with tests
                               ; enabled
    (home-page "https://hackage.haskell.org/package/stringbuilder")
    (synopsis "Writer monad for multi-line string literals")
    (description "This package provides a writer monad for multi-line string
literals.")
    (license license:expat)))

(define-public ghc-string-qq
  (package
    (name "ghc-string-qq")
    (version "0.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/string-qq/string-qq-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0wfxkw4x6j6jq9nd82k83g2k3hskpsvk1dp4cpkshvjr4wg9qny8"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)))
    (home-page "https://hackage.haskell.org/package/string-qq")
    (synopsis
     "QuasiQuoter for non-interpolated strings, texts and bytestrings.")
    (description
     "This package provides a quasiquoter for non-interpolated strings, texts
and bytestrings.")
    (license license:public-domain)))

(define-public ghc-stringsearch
  (package
    (name "ghc-stringsearch")
    (version "0.3.6.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/stringsearch/stringsearch-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0jpy9xjcjdbpi3wk6mg7xwd7wfi2mma70p97v1ij5i8bj9qijpr9"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("1" "0z5pz5dccapz9k39r2zmf056m0x2m2lj3jahhnw3mfxlmps07378")))
    (home-page "https://bitbucket.org/dafis/stringsearch")
    (synopsis "Fast searching, splitting and replacing of ByteStrings")
    (description "This package provides several functions to quickly search
for substrings in strict or lazy @code{ByteStrings}.  It also provides
functions for breaking or splitting on substrings and replacing all
occurrences of a substring (the first in case of overlaps) with another.")
    (license license:bsd-3)))

(define-public ghc-svg-builder
  (package
    (name "ghc-svg-builder")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "svg-builder/svg-builder-" version ".tar.gz"))
       (sha256
        (base32
         "1k420f497lzkymmxin88ql6ib8dziic43avykv31yq65rgrf7l2g"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-blaze-builder" ,ghc-blaze-builder)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-unordered-containers" ,ghc-unordered-containers)))
    (arguments
     `(#:cabal-revision
       ("1" "1bhp9gvid2iis411k1vvyj5krzc4ahxcqcd9cwx9h37jxg180xw1")))
    (home-page "https://github.com/diagrams/svg-builder.git")
    (synopsis "Domain-specific language for building Scalable Vector Graphics")
    (description "Easy-to-write domain-specific language (DSL) for
building Scalable Vector Graphics (SVG).")
    (license license:bsd-3)))

(define-public ghc-syb
  (package
    (name "ghc-syb")
    (version "0.7.1")
    (outputs '("out" "static" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/syb/syb-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0077vxzyi9ppbphi2ialac3p376k49qly1kskdgf57wdwix9qjp0"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-hunit" ,ghc-hunit)))
    (home-page
     "http://www.cs.uu.nl/wiki/GenericProgramming/SYB")
    (synopsis "Scrap Your Boilerplate")
    (description "This package contains the generics system described in the
/Scrap Your Boilerplate/ papers (see
@uref{http://www.cs.uu.nl/wiki/GenericProgramming/SYB, the website}).  It
defines the @code{Data} class of types permitting folding and unfolding of
constructor applications, instances of this class for primitive types, and a
variety of traversals.")
    (license license:bsd-3)))

(define-public ghc-system-fileio
  (package
    (name "ghc-system-fileio")
    (version "0.3.16.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/system-fileio/system-fileio-"
             version ".tar.gz"))
       (sha256
        (base32
         "1iy6g1f35gzyj12g9mdiw4zf75mmxpv1l8cyaldgyscsl648pr9l"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'update-constraints
           (lambda _
             (substitute* "system-fileio.cabal"
               (("chell >= 0\\.4 && < 0\\.5") "chell >= 0.4"))
             #t)))))
    (inputs
     `(("ghc-system-filepath" ,ghc-system-filepath)
       ("ghc-chell" ,ghc-chell)
       ("ghc-temporary" ,ghc-temporary)))
    (home-page "https://github.com/fpco/haskell-filesystem")
    (synopsis "Consistent file system interaction across GHC versions")
    (description
     "This is a small wrapper around the directory, unix, and Win32 packages,
for use with system-filepath.  It provides a consistent API to the various
versions of these packages distributed with different versions of GHC.
In particular, this library supports working with POSIX files that have paths
which can't be decoded in the current locale encoding.")
    (license license:expat)))

;; See ghc-system-filepath-bootstrap. In addition this package depends on
;; ghc-system-filepath.
(define ghc-system-fileio-bootstrap
  (package
    (name "ghc-system-fileio-bootstrap")
    (version "0.3.16.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/system-fileio/system-fileio-"
             version ".tar.gz"))
       (sha256
        (base32
         "1484hcl27s2qcby8ws5djj11q9bz68bspcifz9h5gii2ndy70x9i"))))
    (build-system haskell-build-system)
    (arguments
     `(#:tests? #f))
    (inputs
     `(("ghc-system-filepath-bootstrap" ,ghc-system-filepath-bootstrap)
       ("ghc-temporary" ,ghc-temporary)))
    (home-page "https://github.com/fpco/haskell-filesystem")
    (synopsis "Consistent file system interaction across GHC versions")
    (description
     "This is a small wrapper around the directory, unix, and Win32 packages,
for use with system-filepath.  It provides a consistent API to the various
versions of these packages distributed with different versions of GHC.
In particular, this library supports working with POSIX files that have paths
which can't be decoded in the current locale encoding.")
    (license license:expat)))


(define-public ghc-system-filepath
  (package
    (name "ghc-system-filepath")
    (version "0.4.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/system-filepath/system-filepath-"
             version ".tar.gz"))
       (sha256
        (base32
         "14yras4pz2dh55xpwmazcgxijvi8913pjgzb9iw50mjq1lycwmhn"))))
    (build-system haskell-build-system)
    ;; FIXME: One of the tests fails:
    ;; [ FAIL  ] tests.validity.posix
    ;; note: seed=7310214548328823169
    ;; *** Failed! Falsifiable (after 24 tests):
    ;; FilePath "/r2\ENQ52\t ;$/o\US=/okG\146\&6\n<u\DC3/5\182\223a\DELN\EOT#\NUL/`[m/\USEKV\ETX([)b6/\ACK\SOo\245\ETBO/f\128\STX`|\EM\"/*\EMA\USD3/\143\&4/\CAN?\SUBee\CANR/9/B0\187Kx4/Vqr\232'b:/\a\234\DLE.\"\179/\ENQ{J/|/G)@^\237/\219ml/\DC3pd\ESC"
    (arguments `(#:tests? #f))
    (inputs
     `(("ghc-chell" ,ghc-chell)
       ("ghc-chell-quickcheck" ,ghc-chell-quickcheck)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://github.com/fpco/haskell-filesystem")
    (synopsis "High-level, byte-based file and directory path manipulations")
    (description
     "Provides a FilePath datatype and utility functions for operating on it.
Unlike the filepath package, this package does not simply reuse String,
increasing type safety.")
    (license license:expat)))

;; Ghc-shelly depends on ghc-system-filepath and ghc-system-fileio, who in turn depend on
;; ghc-chell and ghc-chell-quickcheck for the test phase. Ghc-chell depends on ghc-options
;; which depends on ghc-chell and ghc-chell-quickcheck.
;; Therefore we bootstrap it with tests disabled.
(define ghc-system-filepath-bootstrap
  (package
    (name "ghc-system-filepath-bootstrap")
    (version "0.4.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/system-filepath/system-filepath-"
             version ".tar.gz"))
       (sha256
        (base32
         "14yras4pz2dh55xpwmazcgxijvi8913pjgzb9iw50mjq1lycwmhn"))))
    (build-system haskell-build-system)
    (arguments
     `(#:tests? #f))
    (inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://github.com/fpco/haskell-filesystem")
    (synopsis "High-level, byte-based file and directory path manipulations")
    (description
     "Provides a FilePath datatype and utility functions for operating on it.
Unlike the filepath package, this package does not simply reuse String,
increasing type safety.")
    (license license:expat)))

(define-public ghc-tabular
  (package
    (name "ghc-tabular")
    (version "0.2.2.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/tabular/tabular-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1ysgq7rrks7f98nnvxil8xz1q27hxdgz4szbjhqwzbwd209dmy0k"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-csv" ,ghc-csv)
       ("ghc-html" ,ghc-html)))
    (home-page "https://github.com/bgamari/tabular")
    (synopsis "Two-dimensional data tables with rendering functions")
    (description
     "Tabular provides a Haskell representation of two-dimensional data
tables, the kind that you might find in a spreadsheet or or a research report.
It also comes with some default rendering functions for turning those tables
into ASCII art, simple text with an arbitrary delimiter, CSV, HTML or LaTeX.

Below is an example of the kind of output this library produces.  The tabular
package can group rows and columns, each group having one of three
separators (no line, single line, double line) between its members.

@example

    || memtest 1 | memtest 2 ||  time test  | time test 2
====++===========+===========++=============+============
A 1 ||       hog |  terrible ||        slow |      slower
A 2 ||       pig |   not bad ||        fast |     slowest
----++-----------+-----------++-------------+------------
B 1 ||      good |     awful || intolerable |    bearable
B 2 ||    better | no chance ||    crawling |     amazing
B 3 ||       meh |   well... ||  worst ever |          ok

@end example")
    (license license:bsd-3)))

(define-public ghc-tagged
  (package
    (name "ghc-tagged")
    (version "0.8.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/tagged/tagged-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1pciqzxf9ncv954v4r527xkxkn7r5hcr13mfw5dg1xjci3qdw5md"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("1" "070xwfw7y81hd63il76dmwbdl9ca1rd07c54zynfx6vpr4wyx4vh")))
    (inputs
     `(("ghc-transformers-compat" ,ghc-transformers-compat)))
    (home-page "https://hackage.haskell.org/package/tagged")
    (synopsis "Haskell phantom types to avoid passing dummy arguments")
    (description "This library provides phantom types for Haskell 98, to avoid
having to unsafely pass dummy arguments.")
    (license license:bsd-3)))

(define-public ghc-tar
  (package
    (name "ghc-tar")
    (version "0.5.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/tar/tar-"
             version ".tar.gz"))
       (sha256
        (base32
         "1ppim7cgmn7ng8zbdrwkxhhizc30h15h1c9cdlzamc5jcagl915k"))))
    (build-system haskell-build-system)
    ;; FIXME: 2/24 tests fail.
    (arguments `(#:tests? #f))
    (inputs
     `(("ghc-bytestring-handle" ,ghc-bytestring-handle)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)))
    (home-page "https://hackage.haskell.org/package/tar")
    (synopsis "Reading, writing and manipulating \".tar\" archive files")
    (description
     "This library is for working with \\\"@.tar@\\\" archive files.
It can read and write a range of common variations of the tar archive format
including V7, POSIX USTAR and GNU formats.  It provides support for packing and
unpacking portable archives.  This makes it suitable for distribution but not
backup because details like file ownership and exact permissions are not
preserved.  It also provides features for random access to archive content using
an index.")
    (license license:bsd-3)))

(define-public ghc-tar-conduit
  (package
    (name "ghc-tar-conduit")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "tar-conduit/tar-conduit-" version ".tar.gz"))
       (sha256
        (base32
         "0bgn3hyf20g1gfnzy8f41s7nj54kfcyjk2izw99svrw8f3dphi80"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-conduit" ,ghc-conduit)
       ("ghc-conduit-combinators" ,ghc-conduit-combinators)
       ("ghc-safe-exceptions" ,ghc-safe-exceptions)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-conduit-extra" ,ghc-conduit-extra)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-weigh" ,ghc-weigh)))
    (home-page "https://github.com/snoyberg/tar-conduit#readme")
    (synopsis "Extract and create tar files using conduit for streaming")
    (description "This library provides a conduit-based, streaming
interface for extracting and creating tar files.")
    (license license:expat)))

(define-public ghc-temporary
  (package
    (name "ghc-temporary")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/temporary/temporary-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "144qhwfwg37l3k313raf4ssiz16jbgwlm1nf4flgqpsbd69jji4c"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-exceptions" ,ghc-exceptions)
       ("ghc-random" ,ghc-random)))
    (native-inputs
     `(("ghc-base-compat" ,ghc-base-compat)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)))
    (home-page "https://www.github.com/batterseapower/temporary")
    (synopsis "Temporary file and directory support")
    (description "The functions for creating temporary files and directories
in the Haskelll base library are quite limited.  This library just repackages
the Cabal implementations of its own temporary file and folder functions so
that you can use them without linking against Cabal or depending on it being
installed.")
    (license license:bsd-3)))

(define-public ghc-temporary-rc
  (package
    (name "ghc-temporary-rc")
    (version "1.2.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/temporary-rc/temporary-rc-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1nqih0qks439k3pr5kmbbc8rjdw730slrxlflqb27fbxbzb8skqs"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-exceptions" ,ghc-exceptions)))
    (home-page
     "https://www.github.com/feuerbach/temporary")
    (synopsis
     "Portable temporary file and directory support")
    (description
     "The functions for creating temporary files and directories in the base
library are quite limited.  The unixutils package contains some good ones, but
they aren't portable to Windows.  This library just repackages the Cabal
implementations of its own temporary file and folder functions so that you can
use them without linking against Cabal or depending on it being installed.
This is a better maintained fork of the \"temporary\" package.")
    (license license:bsd-3)))

(define-public ghc-terminal-size
  (package
    (name "ghc-terminal-size")
    (version "0.3.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://hackage.haskell.org/package/terminal-size/"
                    "terminal-size-" version ".tar.gz"))
              (sha256
               (base32
                "0n4nvj3dbj9gxfnprgish45asn9z4dipv9j98s8i7g2n8yb3xhmm"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/terminal-size")
    (synopsis "Get terminal window height and width")
    (description "Get terminal window height and width without ncurses
dependency.")
    (license license:bsd-3)))

(define-public ghc-texmath
  (package
    (name "ghc-texmath")
    (version "0.11.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "texmath/texmath-" version ".tar.gz"))
              (sha256
               (base32
                "03rpxbp43bjs62mmw4hv4785n6f6nbf8kj2y9mma5nzk6i2xs09f"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-syb" ,ghc-syb)
       ("ghc-network-uri" ,ghc-network-uri)
       ("ghc-split" ,ghc-split)
       ("ghc-temporary" ,ghc-temporary)
       ("ghc-utf8-string" ,ghc-utf8-string)
       ("ghc-xml" ,ghc-xml)
       ("ghc-pandoc-types" ,ghc-pandoc-types)))
    (home-page "https://github.com/jgm/texmath")
    (synopsis "Conversion between formats used to represent mathematics")
    (description
     "The texmath library provides functions to read and write TeX math,
presentation MathML, and OMML (Office Math Markup Language, used in Microsoft
Office).  Support is also included for converting math formats to pandoc's
native format (allowing conversion, via pandoc, to a variety of different
markup formats).  The TeX reader supports basic LaTeX and AMS extensions, and
it can parse and apply LaTeX macros.")
    (license license:gpl2+)))

(define-public ghc-text-binary
  (package
    (name "ghc-text-binary")
    (version "0.2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "text-binary/text-binary-"
                           version ".tar.gz"))
       (sha256
        (base32
         "18gl10pwg3qwsk0za3c70j4n6a9129wwf1b7d3a461h816yv55xn"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/kawu/text-binary")
    (synopsis "Binary instances for text types")
    (description
     "This package provides a compatibility layer providing @code{Binary}
instances for strict and lazy text types for versions older than 1.2.1 of the
text package.")
    (license license:bsd-2)))

(define-public ghc-text-manipulate
  (package
    (name "ghc-text-manipulate")
    (version "0.2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/text-manipulate"
             "/text-manipulate-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0bwxyjj3ll45srxhsp2ihikgqglvjc6m02ixr8xpvyqwkcfwgsg0"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)))
    (home-page
     "https://github.com/brendanhay/text-manipulate")
    (synopsis
     "Case conversion, word boundary manipulation, and textual subjugation")
    (description
     "Manipulate identifiers and structurally non-complex pieces of text by
delimiting word boundaries via a combination of whitespace,
control-characters, and case-sensitivity.

Has support for common idioms like casing of programmatic variable names,
taking, dropping, and splitting by word, and modifying the first character of
a piece of text.

Caution: this library makes heavy use of the text library's internal loop
optimisation framework.  Since internal modules are not guaranteed to have a
stable API there is potential for build breakage when the text dependency is
upgraded.  Consider yourself warned!")
    (license license:mpl2.0)))

(define-public ghc-text-metrics
  (package
    (name "ghc-text-metrics")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "text-metrics/text-metrics-" version ".tar.gz"))
       (sha256
        (base32
         "18mzxwkdvjp31r720ai9bnxr638qq8x3a2v408bz0d8f0rsayx1q"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-vector" ,ghc-vector)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-hspec" ,ghc-hspec)))
    (arguments
     `(#:cabal-revision
       ("4" "017drxq9x56b345d8w5m8xdsi1zzs0z16pbdx8j35cd1lsnh3kf1")))
    (home-page "https://github.com/mrkkrp/text-metrics")
    (synopsis "Calculate various string metrics efficiently")
    (description "This library provides tools to calculate various
string metrics efficiently.")
    (license license:bsd-3)))

(define-public ghc-tf-random
  (package
    (name "ghc-tf-random")
    (version "0.5")
    (outputs '("out" "static" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/tf-random/tf-random-"
             version
             ".tar.gz"))
       (sha256
        (base32 "0445r2nns6009fmq0xbfpyv7jpzwv0snccjdg7hwj4xk4z0cwc1f"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-primitive" ,ghc-primitive)
       ("ghc-random" ,ghc-random)))
    (home-page "https://hackage.haskell.org/package/tf-random")
    (synopsis "High-quality splittable pseudorandom number generator")
    (description "This package contains an implementation of a high-quality
splittable pseudorandom number generator.  The generator is based on a
cryptographic hash function built on top of the ThreeFish block cipher.  See
the paper \"Splittable Pseudorandom Number Generators Using Cryptographic
Hashing\" by Claessen, Pałka for details and the rationale of the design.")
    (license license:bsd-3)))

(define-public ghc-th-abstraction
  (package
    (name "ghc-th-abstraction")
    (version "0.3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "th-abstraction/th-abstraction-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1f81w0gshvc816cf9qz0f19jsrzh6wpzmmy317xrgn63dv61p7jb"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/glguy/th-abstraction")
    (synopsis "Nicer interface for reified information about data types")
    (description
     "This package normalizes variations in the interface for inspecting
datatype information via Template Haskell so that packages and support a
single, easier to use informational datatype while supporting many versions of
Template Haskell.")
    (license license:isc)))

(define-public ghc-th-expand-syns
  (package
    (name "ghc-th-expand-syns")
    (version "0.4.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "th-expand-syns/th-expand-syns-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1p4wfyycan8zsp9wi7npx36qwbfsbcgdyxi3ii51scf69dkrx42y"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-syb" ,ghc-syb)))
    (home-page "https://hackage.haskell.org/package/th-expand-syns")
    (synopsis "Expands type synonyms in Template Haskell ASTs")
    (description
     "This package enables users to expand type synonyms in Template Haskell
@dfn{abstract syntax trees} (ASTs).")
    (license license:bsd-3)))

(define-public ghc-th-lift
  (package
    (name "ghc-th-lift")
    (version "0.8.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "th-lift/th-lift-" version ".tar.gz"))
              (sha256
               (base32
                "1a6qlbdg15cfr5rvl9g3blgwx4v1p0xic0pzv13zx165xbc36ld0"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-th-abstraction" ,ghc-th-abstraction)))
    (home-page "https://github.com/mboes/th-lift")
    (synopsis "Derive Template Haskell's Lift class for datatypes")
    (description
     "This is a Haskell library to derive Template Haskell's Lift class for
datatypes.")
    (license license:bsd-3)))

(define-public ghc-th-lift-instances
  (package
    (name "ghc-th-lift-instances")
    (version "0.1.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "th-lift-instances/th-lift-instances-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0r1b4jnvcj64wp4hfccwkl4a70n1p1q7qzyx6ax7cmd8k961jz78"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-th-lift" ,ghc-th-lift)
       ("ghc-vector" ,ghc-vector)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://github.com/bennofs/th-lift-instances/")
    (synopsis "Lift instances for template-haskell for common data types.")
    (description "Most data types in the Haskell platform do not have Lift
instances.  This package provides orphan instances for @code{containers},
@code{text}, @code{bytestring} and @code{vector}.")
    (license license:bsd-3)))

(define-public ghc-th-orphans
  (package
    (name "ghc-th-orphans")
    (version "0.13.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "th-orphans/th-orphans-" version ".tar.gz"))
              (sha256
               (base32
                "1xj1gssv77hdx1r3ndg8k49v3ipj3a6r7crkyvx13jrps3m6ng1z"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-th-lift" ,ghc-th-lift)
       ("ghc-th-lift-instances" ,ghc-th-lift-instances)
       ("ghc-th-reify-many" ,ghc-th-reify-many)
       ("ghc-generic-deriving" ,ghc-generic-deriving)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)))
    (home-page "https://hackage.haskell.org/package/th-orphans")
    (synopsis "Orphan instances for TH datatypes")
    (description
     "This package provides orphan instances for Template Haskell datatypes.  In particular,
instances for @code{Ord} and @code{Lift}, as well as a few missing @code{Show}
and @code{Eq} instances.  These instances used to live in the haskell-src-meta
package, and that's where the version number started.")
    (license license:bsd-3)))

(define-public ghc-these
  (package
    (name "ghc-these")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/these/these-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1k0pi65g7cm9hzdw6my6bzz2zvddkmj1qs45ymqmi316bpiixk3r"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-base-compat" ,ghc-base-compat)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-aeson" ,ghc-aeson)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-assoc" ,ghc-assoc)
       ("ghc-semigroupoids" ,ghc-semigroupoids)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (arguments
     `(#:cabal-revision
       ("1"
        "0923r86fnmgpx0msm68aszirh2n19nn5bccgjxfh2146jw4z7w3z")))
    (home-page
     "https://github.com/isomorphism/these")
    (synopsis "Either-or-both data type")
    (description
     "This package provides a data type @code{These a b} which can
hold a value of either type or values of each type.  This is usually
thought of as an \"inclusive or\" type (contrasting @code{Either a b} as
\"exclusive or\") or as an \"outer join\" type (contrasting @code{(a, b)}
as \"inner join\").

@code{data These a b = This a | That b | These a b}

Since version 1, this package was split into parts:

@itemize
@item
https://hackage.haskell.org/package/semialign For @code{Align} and
@code{Zip} type-classes.
@item
https://hackage.haskell.org/package/semialign-indexed For
@code{SemialignWithIndex} class, providing @code{ialignWith} and
@code{izipWith}
@item
https://hackage.haskell.org/package/these-lens For lens combinators.
@item
http://hackage.haskell.org/package/monad-chronicle For transformers
variant of @code{These}.
@end itemize")
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

(define-public ghc-th-reify-many
  (package
    (name "ghc-th-reify-many")
    (version "0.1.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "th-reify-many/th-reify-many-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0hxf56filzqnyrc8q7766vai80y6cgrrbrczx6n93caskl1dv2gq"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-safe" ,ghc-safe)
       ("ghc-th-expand-syns" ,ghc-th-expand-syns)))
    (home-page "https://github.com/mgsloan/th-reify-many")
    (synopsis "Recurseively reify template haskell datatype info")
    (description
     "th-reify-many provides functions for recursively reifying top level
declarations.  The main intended use case is for enumerating the names of
datatypes reachable from an initial datatype, and passing these names to some
function which generates instances.")
    (license license:bsd-3)))

(define-public ghc-time-compat
  (package
    (name "ghc-time-compat")
    (version "1.9.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "time-compat/time-compat-" version ".tar.gz"))
       (sha256
        (base32
         "05va0rqs759vbridbcl6hksp967j9anjvys8vx72fnfkhlrn2s52"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-base-orphans" ,ghc-base-orphans)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("ghc-base-compat" ,ghc-base-compat)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-tagged" ,ghc-tagged)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)))
    (arguments
     `(#:cabal-revision
       ("1" "0k8ph4sydaiqp8dav4if6hpiaq8h1xsr93khmdr7a1mmfwdxr64r")))
    (home-page "https://github.com/phadej/time-compat")
    (synopsis "Compatibility package for time")
    (description "This package tries to compat as many @code{time}
features as possible.")
    (license license:bsd-3)))

(define-public ghc-time-locale-compat
  (package
    (name "ghc-time-locale-compat")
    (version "0.1.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "time-locale-compat/time-locale-compat-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0b2hmj8wwrfkndwzgm11qr496ca2ahwdxcj3m0ii91bxvrk1bzq7"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-old-locale" ,ghc-old-locale)))
    (home-page "https://github.com/khibino/haskell-time-locale-compat")
    (synopsis "Compatibility of TimeLocale between old-locale and time-1.5")
    (description "This package contains a wrapped name module for
@code{TimeLocale}.")
    (license license:bsd-3)))

(define-public ghc-time-manager
  (package
    (name "ghc-time-manager")
    (version "0.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "time-manager/time-manager-" version ".tar.gz"))
       (sha256
        (base32
         "1nzwj0fxz370ks6vr1sylcidx33rnqq45y3q9yv9n4dj43nid9lh"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-auto-update" ,ghc-auto-update)))
    (home-page "https://github.com/yesodweb/wai")
    (synopsis "Scalable timer")
    (description "This library contains scalable timer functions provided by a
timer manager.")
    (license license:expat)))

(define-public ghc-timeit
  (package
    (name "ghc-timeit")
    (version "2.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://hackage.haskell.org/package/timeit/timeit-"
         version ".tar.gz"))
       (sha256
        (base32
         "1sliqpvl501rlcj6s0lhmsf5ym24j4h881wzc1f1wdyvg3jz8kd1"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/merijn/timeit")
    (synopsis "Time monadic computations with an IO base")
    (description "This package provides a simple wrapper to show the
used CPU time of monadic computation with an IO base.")
    (license license:bsd-3)))

(define-public ghc-timezone-series
  (package
   (name "ghc-timezone-series")
   (version "0.1.9")
   (source
    (origin
     (method url-fetch)
     (uri
      (string-append
       "mirror://hackage/package/timezone-series/timezone-series-"
       version ".tar.gz"))
     (sha256
      (base32
       "1blwgnyzqn917rgqkl4dncv9whv3xmk0lav040qq0214vksmvlz5"))))
   (build-system haskell-build-system)
   (home-page "https://archives.haskell.org/projects.haskell.org/time-ng/")
   (synopsis "Enhanced timezone handling for Time")
   (description
    "This package endows @code{Data.Time}, from the time package, with several
data types and functions for enhanced processing of timezones.  For one way to
create timezone series, see the ghc-timezone-olson package.")
   (license license:bsd-3)))

(define-public ghc-timezone-olson
  (package
   (name "ghc-timezone-olson")
   (version "0.1.9")
   (source
    (origin
     (method url-fetch)
     (uri
      (string-append
       "mirror://hackage/package/timezone-olson/timezone-olson-"
       version ".tar.gz"))
     (sha256
      (base32
       "05abywx1nrcaz0nqzfy4zw62bc5qd7pdfnjvv4drxkwv084ha8rj"))))
   (build-system haskell-build-system)
   (inputs
    `(("ghc-timezone-series" ,ghc-timezone-series)
      ("ghc-extensible-exceptions" ,ghc-extensible-exceptions)))
   (home-page "https://archives.haskell.org/projects.haskell.org/time-ng/")
   (synopsis "Parser and renderer for binary Olson timezone files")
   (description
    "A parser and renderer for binary Olson timezone files whose format
is specified by the tzfile(5) man page on Unix-like systems.  For more
information about this format, see
@url{http://www.iana.org/time-zones/repository/tz-link.html}.  Functions
are provided for converting the parsed data into @code{TimeZoneSeries}
objects from the timezone-series package.")
   (license license:bsd-3)))

(define-public ghc-tldr
  (package
    (name "ghc-tldr")
    (version "0.4.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/tldr/tldr-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1zy9yyg7bxiyz1prkvrscggsb9p0f8y0nqxxxzlgzvnh2nmqf8f2"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-cmark" ,ghc-cmark)
       ("ghc-optparse-applicative" ,ghc-optparse-applicative)
       ("ghc-typed-process" ,ghc-typed-process)
       ("ghc-semigroups" ,ghc-semigroups)))
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-golden" ,ghc-tasty-golden)))
    (home-page "https://github.com/psibi/tldr-hs#readme")
    (synopsis "Haskell tldr client")
    (description "This package provides the @command{tldr} command and a
Haskell client library allowing users to update and view @code{tldr} pages
from a shell.  The @code{tldr} pages are a community effort to simplify the
man pages with practical examples.")
    (license license:bsd-3)))

(define-public ghc-torrent
  (package
    (name "ghc-torrent")
    (version "10000.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/torrent/torrent-"
             version ".tar.gz"))
       (sha256
        (base32 "0m7s0q7f8c7glxzqhf2j86ch5xhk6jnzwwsa4mkywag22119c290"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-bencode" ,ghc-bencode)
       ("ghc-syb" ,ghc-syb)))
    (home-page "https://hackage.haskell.org/package/torrent")
    (synopsis "BitTorrent file parser and generator")
    (description "This library provides support for parsing and generating
BitTorrent files.")
    (license license:bsd-3)))

(define-public ghc-transformers
  (package
    (name "ghc-transformers")
    (version "0.5.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://hackage/package/transformers/transformers-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0v66j5k0xqk51pmca55wq192qyw2p43s2mgxlz4f95q2c1fpjs5n"))))
    (build-system haskell-build-system)
    (home-page
     "http://hackage.haskell.org/package/transformers")
    (synopsis "Concrete functor and monad transformers")
    (description
     "Transformers provides functor and monad transformers, inspired by the
paper \"Functional Programming with Overloading and Higher-Order
Polymorphism\", by Mark P Jones, in Advanced School of Functional Programming,
1995 @url{http://web.cecs.pdx.edu/~mpj/pubs/springschool.html}.

This package contains:
@itemize
@item the monad transformer class (in @code{Control.Monad.Trans.Class})
@item concrete functor and monad transformers, each with associated operations
and functions to lift operations associated with other transformers.
@end itemize

This package can be used on its own in portable Haskell code, in which case
operations need to be manually lifted through transformer stacks (see
@code{Control.Monad.Trans.Class} for some examples).  Alternatively, it can be
used with the non-portable monad classes in the mtl or monads-tf packages,
which automatically lift operations introduced by monad transformers through
other transformers.")
    (license license:bsd-3)))

(define-public ghc-transformers-base
  (package
    (name "ghc-transformers-base")
    (version "0.4.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/transformers-base/transformers-base-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1s256bi0yh0x2hp2gwd30f3mg1cv53zz397dv1yhfsnfzmihrj6h"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-transformers-compat" ,ghc-transformers-compat)))
    (home-page
     "https://hackage.haskell.org/package/transformers-compat")
    (synopsis
     "Backported transformer library")
    (description
     "Backported versions of types that were added to transformers in
transformers 0.3 and 0.4 for users who need strict transformers 0.2 or 0.3
compatibility to run on old versions of the platform.")
    (license license:bsd-3)))

(define-public ghc-transformers-compat
  (package
    (name "ghc-transformers-compat")
    (version "0.6.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/transformers-compat"
             "/transformers-compat-" version ".tar.gz"))
       (sha256
        (base32
         "02v2fjbvcrlpvhcsssap8dy8y9pp95jykrlc5arm39sxa48wyrys"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/ekmett/transformers-compat/")
    (synopsis "Small compatibility shim between transformers 0.3 and 0.4")
    (description "This package includes backported versions of types that were
added to transformers in transformers 0.3 and 0.4 for users who need strict
transformers 0.2 or 0.3 compatibility to run on old versions of the platform,
but also need those types.")
    (license license:bsd-3)))

(define-public ghc-tree-diff
  (package
    (name "ghc-tree-diff")
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/tree-diff/tree-diff-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1156nbqn0pn9lp4zjsy4vv5g5wmy4zxwmbqdgvq349rydynh3ng3"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-aeson" ,ghc-aeson)
       ("ghc-ansi-wl-pprint" ,ghc-ansi-wl-pprint)
       ("ghc-ansi-terminal" ,ghc-ansi-terminal)
       ("ghc-base-compat" ,ghc-base-compat)
       ("ghc-bytestring-builder" ,ghc-bytestring-builder)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-parsers" ,ghc-parsers)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-scientific" ,ghc-scientific)
       ("ghc-tagged" ,ghc-tagged)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-uuid-types" ,ghc-uuid-types)
       ("ghc-vector" ,ghc-vector)))
    (native-inputs
     `(("ghc-base-compat" ,ghc-base-compat)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-ansi-terminal" ,ghc-ansi-terminal)
       ("ghc-ansi-wl-pprint" ,ghc-ansi-wl-pprint)
       ("ghc-trifecta" ,ghc-trifecta)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-golden" ,ghc-tasty-golden)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)))
    (home-page "https://github.com/phadej/tree-diff")
    (synopsis "Compute difference between (expression) trees")
    (description "This Haskell library provides a function for computing
the difference between (expression) trees.  It also provides a way to
compute the difference between arbitrary abstract datatypes (ADTs) using
@code{Generics}-derivable helpers.")
    (license license:bsd-3)))

(define-public ghc-trifecta
  (package
    (name "ghc-trifecta")
    (version "2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://hackage.haskell.org/package/trifecta/"
                    "trifecta-" version ".tar.gz"))
              (sha256
               (base32
                "0hznd8i65s81xy13i2qc7cvipw3lfb2yhkv53apbdsh6sbljz5sk"))))
    (build-system haskell-build-system)
    (arguments
     `(#:tests? #f ; doctest suite fails to build on i686
       #:cabal-revision
       ("4" "0xbwyvwl6f2zylk60f2akwgq03qv49113xil7b1z1s3vlwbn5aj1")))
    (inputs
     `(("ghc-reducers" ,ghc-reducers)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-ansi-wl-pprint" ,ghc-ansi-wl-pprint)
       ("ghc-ansi-terminal" ,ghc-ansi-terminal)
       ("ghc-blaze-builder" ,ghc-blaze-builder)
       ("ghc-blaze-html" ,ghc-blaze-html)
       ("ghc-blaze-markup" ,ghc-blaze-markup)
       ("ghc-charset" ,ghc-charset)
       ("ghc-comonad" ,ghc-comonad)
       ("ghc-fingertree" ,ghc-fingertree)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-lens" ,ghc-lens)
       ("ghc-parsers" ,ghc-parsers)
       ("ghc-profunctors" ,ghc-profunctors)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-utf8-string" ,ghc-utf8-string)))
    (native-inputs
     `(("cabal-doctest" ,cabal-doctest)
       ("ghc-doctest" ,ghc-doctest)
       ("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://github.com/ekmett/trifecta/")
    (synopsis "Parser combinator library with convenient diagnostics")
    (description "Trifecta is a modern parser combinator library for Haskell,
with slicing and Clang-style colored diagnostics.")
    (license license:bsd-3)))

(define-public ghc-tuple-th
  (package
    (name "ghc-tuple-th")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "tuple-th-" version "/"
                           "tuple-th-" version ".tar.gz"))
       (sha256
        (base32
         "1mrl4vvxmby7sf1paf7hklzidnr6wq55822i73smqyz0xpf3gsjn"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/DanielSchuessler/tuple-th")
    (synopsis "Generate utility functions for tuples of statically known size
for Haskell")
    (description "This Haskell package contains Template Haskell functions for
generating functions similar to those in @code{Data.List} for tuples of
statically known size.")
    (license license:bsd-3)))

(define-public ghc-turtle
  (package
    (name "ghc-turtle")
    (version "1.5.15")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://hackage/package/turtle/turtle-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0yckgsc2a4g5x867gni80ldp226bsnhncfbil4ql6v2zwm4r8p7f"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-ansi-wl-pprint" ,ghc-ansi-wl-pprint)
       ("ghc-async" ,ghc-async)
       ("ghc-clock" ,ghc-clock)
       ("ghc-exceptions" ,ghc-exceptions)
       ("ghc-foldl" ,ghc-foldl)
       ("ghc-hostname" ,ghc-hostname)
       ("ghc-managed" ,ghc-managed)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-system-filepath" ,ghc-system-filepath)
       ("ghc-system-fileio" ,ghc-system-fileio)
       ("ghc-streaming-commons" ,ghc-streaming-commons)
       ("ghc-temporary" ,ghc-temporary)
       ("ghc-optparse-applicative" ,ghc-optparse-applicative)
       ("ghc-optional-args" ,ghc-optional-args)
       ("ghc-unix-compat" ,ghc-unix-compat)))
    (native-inputs
     `(("ghc-doctest" ,ghc-doctest)
       ("ghc-fail" ,ghc-fail)))
    (arguments
     `(#:cabal-revision
       ("1" "02q1rv7zx31xz9wnmcqwd4w3iw7623p07iyi21zr0cqlignic5pg")))
    (home-page
     "http://hackage.haskell.org/package/turtle")
    (synopsis "Shell programming, Haskell-style")
    (description
     "Turtle is a reimplementation of the Unix command line environment in
Haskell so that you can use Haskell as both a shell and a scripting
language.  Features include:

@itemize
@item Batteries included: Command an extended suite of predefined utilities.
@item Interoperability: You can still run external shell commands.
@item Portability: Works on Windows, OS X, and Linux.
@item Exception safety: Safely acquire and release resources.
@item Streaming: Transform or fold command output in constant space.
@item Patterns: Use typed regular expressions that can parse structured values.
@item Formatting: Type-safe printf-style text formatting.
@item Modern: Supports text and system-filepath.
@end itemize

Read \"Turtle.Tutorial\" for a detailed tutorial or \"Turtle.Prelude\" for a
quick-start guide.  Turtle is designed to be beginner-friendly, but as a
result lacks certain features, like tracing commands.  If you feel comfortable
using turtle then you should also check out the Shelly library which provides
similar functionality.")
    (license license:bsd-3)))

(define-public ghc-typed-process
  (package
    (name "ghc-typed-process")
    (version "0.2.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "typed-process/typed-process-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1cf2pfym8zdxvvy7xv72ixj7wy3rjrdss6f57k1ysgs66cgsi8ii"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-async" ,ghc-async)
       ("ghc-unliftio-core" ,ghc-unliftio-core)))
    (native-inputs
     `(("ghc-base64-bytestring" ,ghc-base64-bytestring)
       ("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)
       ("ghc-temporary" ,ghc-temporary)))
    (home-page "https://haskell-lang.org/library/typed-process")
    (synopsis "Run external processes with strong typing of streams")
    (description
     "This library provides the ability to launch and interact with external
processes.  It wraps around the @code{process} library, and intends to improve
upon it.")
    (license license:expat)))

(define-public ghc-uglymemo
  (package
    (name "ghc-uglymemo")
    (version "0.1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/uglymemo/uglymemo-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0ixqg5d0ly1r18jbgaa89i6kjzgi6c5hanw1b1y8c5fbq14yz2gy"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/uglymemo")
    (synopsis "Simple memoization function for Haskell")
    (description
     "This package provides a simple (but internally ugly) memoization
function.")
    (license license:public-domain)))

(define-public ghc-unagi-chan
  (package
    (name "ghc-unagi-chan")
    (version "0.4.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/unagi-chan"
                           "/unagi-chan-" version ".tar.gz"))
       (sha256
        (base32
         "1lnl5n4jnjmm4chp461glcwkrrw63rjz3fvprwxcy3lkpbkrqvgn"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-atomic-primops" ,ghc-atomic-primops)
       ("ghc-primitive" ,ghc-primitive)))
    (arguments
     `(#:tests? #f ; FIXME: Tests expect primitive 0.7
       #:cabal-revision
       ("1"
        "09pqi867wskwgc5lpn197f895mbn1174ydgllvcppcsmrz2b6yr6")))
    (home-page "https://hackage.haskell.org/package/unagi-chan")
    (synopsis "Fast concurrent queues with a Chan-like API, and more")
    (description
     "This library provides implementations of concurrent FIFO queues (for
both general boxed and primitive unboxed values) that are fast, perform well
under contention, and offer a Chan-like interface.  The library may be of
limited usefulness outside of x86 architectures where the fetch-and-add
instruction is not available.")
    (license license:bsd-3)))

(define-public ghc-unbounded-delays
  (package
    (name "ghc-unbounded-delays")
    (version "0.1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/unbounded-delays/unbounded-delays-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1ir9fghbrc214c97bwafk5ck6cacxz1pdnq4i18p604d1b8zg9wa"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/basvandijk/unbounded-delays")
    (synopsis "Unbounded thread delays and timeouts")
    (description "The @code{threadDelay} and @code{timeout} functions from the
Haskell base library use the bounded @code{Int} type for specifying the delay
or timeout period.  This package provides alternative functions which use the
unbounded @code{Integer} type.")
    (license license:bsd-3)))

(define-public ghc-unexceptionalio
  (package
    (name "ghc-unexceptionalio")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "unexceptionalio-" version "/" "unexceptionalio-"
                           version ".tar.gz"))
       (sha256 (base32 "09gynk472l7nn5l2w320n4dwigwp0wh0shfp6dyw6r5h2jdxz18p"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/singpolyma/unexceptionalio")
    (synopsis "IO without any non-error, synchronous exceptions")
    (description "When you've caught all the exceptions that can be
handled safely, this is what you're left with.")
    (license license:isc)))

(define-public ghc-unicode-transforms
  (package
    (name "ghc-unicode-transforms")
    (version "0.3.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "unicode-transforms/unicode-transforms-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1akscvyssif4hki3g6hy0jmjyr8cqly1whzvzj0km2b3qh0x09l3"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-bitarray" ,ghc-bitarray)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-getopt-generics" ,ghc-getopt-generics)
       ("ghc-split" ,ghc-split)))
    (home-page "https://github.com/composewell/unicode-transforms")
    (synopsis "Unicode normalization")
    (description "This library provides tools for fast Unicode 12.1.0
normalization in Haskell (normalization forms C, KC, D, and KD).")
    (license license:bsd-3)))

(define-public ghc-union-find
  (package
    (name "ghc-union-find")
    (version "0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://hackage.haskell.org/package/union-find/union-find-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1v7hj42j9w6jlzi56jg8rh4p58gfs1c5dx30wd1qqvn0p0mnihp6"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/nominolo/union-find")
    (synopsis "Efficient union and equivalence testing of sets")
    (description
     "The Union/Find algorithm implements these operations in (effectively)
constant-time:
@enumerate
@item Check whether two elements are in the same equivalence class.
@item Create a union of two equivalence classes.
@item Look up the descriptor of the equivalence class.
@end enumerate\n")
    (license license:bsd-3)))

(define-public ghc-uniplate
  (package
    (name "ghc-uniplate")
    (version "1.6.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/uniplate/uniplate-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1dx8f9aw27fz8kw0ad1nm6355w5rdl7bjvb427v2bsgnng30pipw"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-syb" ,ghc-syb)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-unordered-containers" ,ghc-unordered-containers)))
    (home-page "https://github.com/ndmitchell/uniplate")
    (synopsis "Simple, concise and fast generic operations")
    (description "Uniplate is a library for writing simple and concise generic
operations.  Uniplate has similar goals to the original Scrap Your Boilerplate
work, but is substantially simpler and faster.")
    (license license:bsd-3)))

(define-public ghc-unix-compat
  (package
    (name "ghc-unix-compat")
    (version "0.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/unix-compat/unix-compat-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1a8brv9fax76b1fymslzyghwa6ma8yijiyyhn12msl3i5x24x6k5"))))
    (build-system haskell-build-system)
    (home-page
     "https://github.com/jystic/unix-compat")
    (synopsis "Portable POSIX-compatibility layer")
    (description
     "This package provides portable implementations of parts of the unix
package.  This package re-exports the unix package when available.  When it
isn't available, portable implementations are used.")
    (license license:bsd-3)))

(define-public ghc-unix-time
  (package
    (name "ghc-unix-time")
    (version "0.4.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/unix-time/unix-time-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "02fyh298lm8jsg52i3z0ikazwz477ljqjmhnqr2d88grmn5ky8qr"))))
    (build-system haskell-build-system)
    (arguments
     `(#:tests? #f)) ; FIXME: Test fails with "System.Time not found".  This
                     ; is weird, that should be provided by GHC 7.10.2.
    (inputs
     `(("ghc-old-time" ,ghc-old-time)
       ("ghc-old-locale" ,ghc-old-locale)))
    (home-page "https://hackage.haskell.org/package/unix-time")
    (synopsis "Unix time parser/formatter and utilities")
    (description "This library provides fast parsing and formatting utilities
for Unix time in Haskell.")
    (license license:bsd-3)))

(define-public ghc-unliftio
  (package
    (name "ghc-unliftio")
    (version "0.2.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/unliftio/unliftio-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "02gy1zrxgzg4xmzm8lafsf1nyr3as1q20r8ld73xg3q7rkag9acg"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f)) ; FIXME: hspec-discover not in PATH
    (outputs '("out" "static" "doc"))
    (inputs
     `(("ghc-async" ,ghc-async)
       ("ghc-unliftio-core" ,ghc-unliftio-core)))
    (native-inputs `(("ghc-hspec" ,ghc-hspec)))
    (home-page "https://github.com/fpco/unliftio")
    (synopsis "Provides MonadUnliftIO typecplass for unlifting monads to
IO (batteries included)")
    (description "This Haskell package provides the core @code{MonadUnliftIO}
typeclass, a number of common instances, and a collection of common functions
working with it.")
    (license license:expat)))

(define-public ghc-unliftio-core
  (package
    (name "ghc-unliftio-core")
    (version "0.1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "unliftio-core-" version "/"
                           "unliftio-core-" version ".tar.gz"))
       (sha256
        (base32
         "0y3siyx3drkw7igs380a87h8qfbbgcyxxlcnshp698hcc4yqphr4"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("2" "0jqrjjbgicx48wzcjxs1xmih48ay79rhmrz6081dldlfxynli6vz")))
    (home-page
     "https://github.com/fpco/unliftio/tree/master/unliftio-core#readme")
    (synopsis "The MonadUnliftIO typeclass for unlifting monads to IO")
    (description "This Haskell package provides the core @code{MonadUnliftIO}
typeclass, instances for base and transformers, and basic utility
functions.")
    (license license:expat)))

(define-public ghc-unordered-containers
  (package
    (name "ghc-unordered-containers")
    (version "0.2.10.0")
    (outputs '("out" "static" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/unordered-containers"
             "/unordered-containers-" version ".tar.gz"))
       (sha256
        (base32
         "0wy5hfrs880hh8hvp648bl07ws777n3kkmczzdszr7papnyigwb5"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-chasingbottoms" ,ghc-chasingbottoms)
       ("ghc-hunit" ,ghc-hunit)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)
       ("ghc-hashable" ,ghc-hashable)))
    (home-page
     "https://github.com/tibbe/unordered-containers")
    (synopsis
     "Efficient hashing-based container types")
    (description
     "Efficient hashing-based container types.  The containers have been
optimized for performance critical use, both in terms of large data quantities
and high speed.")
    (license license:bsd-3)))

(define-public ghc-unordered-containers-bootstrap
  (package
    (inherit ghc-unordered-containers)
    (name "ghc-unordered-containers-bootstrap")
    (arguments `(#:tests? #f))
    (inputs
     `(("ghc-hashable" ,ghc-hashable-bootstrap)))
    (properties '((hidden? #t)))))

(define-public ghc-unsafe
  (package
    (name "ghc-unsafe")
    (version "0.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://hackage.haskell.org/package/unsafe/unsafe-"
         version ".tar.gz"))
       (sha256
        (base32
         "0hc6xr1i3hkz25gdgfx1jqgpsc9mwa05bkfynp0mcfdlyz6782nz"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/unsafe")
    (synopsis "Unified interface to unsafe functions")
    (description "Safe Haskell introduced the notion of safe and unsafe
modules.  In order to make as many as possible modules ``safe'', the
well-known unsafe functions were moved to distinguished modules.  This
makes it hard to write packages that work with both old and new versions
of GHC.  This package provides a single module System.Unsafe that
exports the unsafe functions from the base package.  It provides them in
a style ready for qualification, that is, you should import them by
@code{import qualified System.Unsafe as Unsafe}.")
    (license license:bsd-3)))

(define-public ghc-uri-bytestring
  (package
    (name "ghc-uri-bytestring")
    (version "0.3.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "uri-bytestring-" version "/"
                           "uri-bytestring-" version ".tar.gz"))
       (sha256
        (base32
         "0spzv3mwlpxiamd7347sxwcy2xri16ak1y7p1v4fisnvq4jprm67"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-attoparsec" ,ghc-attoparsec)
              ("ghc-blaze-builder" ,ghc-blaze-builder)
              ("ghc-th-lift-instances" ,ghc-th-lift-instances)))
    (native-inputs `(("ghc-hunit" ,ghc-hunit)
                     ("ghc-tasty" ,ghc-tasty)
                     ("ghc-tasty-hunit" ,ghc-tasty-hunit)
                     ("ghc-hedgehog" ,ghc-hedgehog)
                     ("ghc-tasty-hedgehog" ,ghc-tasty-hedgehog)
                     ("ghc-base-compat" ,ghc-base-compat)
                     ("ghc-semigroups" ,ghc-semigroups)
                     ("ghc-safe" ,ghc-safe)))
    (home-page "https://github.com/Soostone/uri-bytestring")
    (synopsis "Haskell URI parsing as ByteStrings")
    (description "This Haskell package aims to be an RFC3986 compliant URI
parser that uses ByteStrings for parsing and representing the URI data.")
    (license license:bsd-3)))

(define-public ghc-utf8-light
  (package
    (name "ghc-utf8-light")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://hackage/package/utf8-light/utf8-light-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0rwyc5z331yfnm4hpx0sph6i1zvkd1z10vvglhnp0vc9wy644k0q"))))
    (build-system haskell-build-system)
    (home-page
     "http://hackage.haskell.org/package/utf8-light")
    (synopsis "Lightweight unicode support for Haskell")
    (description
     "This package profides a class for encoding and decoding UTF8 strings
with instances for several common types.  It also includes several functions
for working with UTF8.  It aims to be lightweight, depending only on Base and
including only one module.")
    (license license:bsd-3)))

(define-public ghc-utf8-string
  (package
    (name "ghc-utf8-string")
    (version "1.0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/utf8-string/utf8-string-"
             version
             ".tar.gz"))
       (sha256
        (base32 "0h7imvxkahiy8pzr8cpsimifdfvv18lizrb33k6mnq70rcx9w2zv"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("3" "02vhj5gykkqa2dyn7s6gn8is1b5fdn9xcqqvlls268g7cpv6rk38")))
    (home-page "https://github.com/glguy/utf8-string/")
    (synopsis "Support for reading and writing UTF8 Strings")
    (description
     "A UTF8 layer for Strings.  The utf8-string package provides operations
for encoding UTF8 strings to Word8 lists and back, and for reading and writing
UTF8 without truncation.")
    (license license:bsd-3)))

(define-public ghc-utility-ht
  (package
    (name "ghc-utility-ht")
    (version "0.0.14")
    (home-page "https://hackage.haskell.org/package/utility-ht")
    (source
     (origin
       (method url-fetch)
       (uri (string-append home-page "/utility-ht-" version ".tar.gz"))
       (sha256
        (base32 "1a7bgk7wv7sqbxbiv7kankiimr3wij7zdm7s83zwsf886ghyxhk9"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-quickcheck" ,ghc-quickcheck)))
    (synopsis "Haskell helper functions for Lists, Maybes, Tuples, Functions")
    (description "This package includes Hakell modules providing various
helper functions for Lists, Maybes, Tuples, Functions.")
    (license license:bsd-3)))

(define-public ghc-uuid
  (package
    (name "ghc-uuid")
    (version "1.3.13")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "uuid-" version "/"
                           "uuid-" version ".tar.gz"))
       (sha256
        (base32
         "09xhk42yhxvqmka0iqrv3338asncz8cap3j0ic0ps896f2581b6z"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("2" "0m185q62jkfb5jsv358nxbnrkv8y8hd0qqvgvh22wvc5g9ipz0r9")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'strip-test-framework-constraints
           (lambda _
             (substitute* "uuid.cabal"
               (("HUnit            >= 1\\.2   && < 1\\.4") "HUnit")
               (("QuickCheck       >= 2\\.4   && < 2\\.10") "QuickCheck")
               (("tasty            >= 0\\.10  && < 0\\.12") "tasty")
               (("tasty-hunit      == 0\\.9\\.\\*") "tasty-hunit")
               (("tasty-quickcheck == 0\\.8\\.\\*") "tasty-quickcheck")))))))
    (inputs `(("ghc-cryptohash-sha1" ,ghc-cryptohash-sha1)
              ("ghc-cryptohash-md5" ,ghc-cryptohash-md5)
              ("ghc-entropy" ,ghc-entropy)
              ("ghc-network-info" ,ghc-network-info)
              ("ghc-random" ,ghc-random)
              ("ghc-uuid-types" ,ghc-uuid-types)))
    (native-inputs `(("ghc-hunit" ,ghc-hunit)
                     ("ghc-quickcheck" ,ghc-quickcheck)
                     ("ghc-tasty" ,ghc-tasty)
                     ("ghc-tasty-hunit" ,ghc-tasty-hunit)
                     ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)))
    (home-page "https://github.com/hvr/uuid")
    (synopsis "Haskell library to create, compare, parse, and print UUIDs")
    (description "This Haskell library provides utilities creating, comparing,
parsing and printing @dfn{Universally Unique Identifiers} or UUIDs.")
    (license license:bsd-3)))

(define-public ghc-uuid-types
  (package
    (name "ghc-uuid-types")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "uuid-types-" version "/"
                           "uuid-types-" version ".tar.gz"))
       (sha256
        (base32
         "1zdka5jnm1h6k36w3nr647yf3b5lqb336g3fkprhd6san9x52xlj"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'strip-test-framework-constraints
           (lambda _
             (substitute* "uuid-types.cabal"
               (("HUnit >=1\\.2 && < 1\\.4") "HUnit")
               (("QuickCheck >=2\\.4 && < 2\\.9") "QuickCheck")
               (("tasty >= 0\\.10 && < 0\\.12") "tasty")
               (("tasty-hunit == 0\\.9\\.\\*") "tasty-hunit")
               (("tasty-quickcheck == 0\\.8\\.\\*") "tasty-quickcheck")))))))
    (inputs `(("ghc-hashable" ,ghc-hashable)
              ("ghc-random" ,ghc-random)))
    (native-inputs `(("ghc-hunit" ,ghc-hunit)
                     ("ghc-quickcheck" ,ghc-quickcheck)
                     ("ghc-tasty" ,ghc-tasty)
                     ("ghc-tasty-hunit" ,ghc-tasty-hunit)
                     ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)))
    (home-page "https://github.com/hvr/uuid")
    (synopsis "Haskell type definitions for UUIDs")
    (description "This Haskell library contains type definitions for
@dfn{Universally Unique Identifiers} or
@uref{https://en.wikipedia.org/wiki/UUID, UUIDs}, and basic conversion
functions.")
    (license license:bsd-3)))

(define-public ghc-validation
  (package
    (name "ghc-validation")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://hackage/package/validation/validation-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1acj7mh3581ks405xswxw6667z7y1y0slisg6jvp6chc191ji9l5"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("1" "1rrjg9z399k6pb55nv85mlr5bkmdqbjwkvl1cy7ydccdx6ks4syp")))
    (inputs
     `(("ghc-semigroups" ,ghc-semigroups)
       ("ghc-semigroupoids" ,ghc-semigroupoids)
       ("ghc-bifunctors" ,ghc-bifunctors)
       ("ghc-lens" ,ghc-lens)))
    (native-inputs
     `(("ghc-hedgehog" ,ghc-hedgehog)
       ("ghc-hunit" ,ghc-hunit)))
    (home-page "https://github.com/qfpl/validation")
    (synopsis
     "Data-type like Either but with an accumulating Applicative")
    (description
     "A data-type like Either but with differing properties and type-class
instances.

Library support is provided for this different representation, including
@code{lens}-related functions for converting between each and abstracting over
their similarities.

The @code{Validation} data type is isomorphic to @code{Either}, but has an
instance of @code{Applicative} that accumulates on the error side.  That is to
say, if two (or more) errors are encountered, they are appended using a
@{Semigroup} operation.

As a consequence of this @code{Applicative} instance, there is no
corresponding @code{Bind} or @code{Monad} instance.  @code{Validation} is an
example of, \"An applicative functor that is not a monad.\"")
    (license license:bsd-3)))

(define-public ghc-validity
  (package
    (name "ghc-validity")
    (version "0.9.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/validity/validity-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1aa93lp1pqwv7vhx19nazlig8qhbp3psblbz360s5lii3s5rli2v"))))
    (build-system haskell-build-system)
    (native-inputs `(("ghc-hspec" ,ghc-hspec)
                     ("hspec-discover" ,hspec-discover)))
    (home-page
     "https://github.com/NorfairKing/validity")
    (synopsis "Validity typeclass")
    (description
     "Values of custom types usually have invariants imposed upon them.  This
package provides the @code{Validity} type class, which makes these invariants
explicit by providing a function to check whether the invariants hold.")
    (license license:expat)))

(define-public ghc-vault
  (package
    (name "ghc-vault")
    (version "0.3.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/vault/vault-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0vdm472vn734xa27jjm2mjacl37mxiqaaahvm4hzqjgyh4cqq377"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-hashable" ,ghc-hashable)
       ("ghc-semigroups" ,ghc-semigroups)))
    (home-page
     "https://github.com/HeinrichApfelmus/vault")
    (synopsis "Persistent store for arbitrary values")
    (description "This package provides vaults for Haskell.  A vault is a
persistent store for values of arbitrary types.  It's like having first-class
access to the storage space behind @code{IORefs}.  The data structure is
analogous to a bank vault, where you can access different bank boxes with
different keys; hence the name.  Also provided is a @code{locker} type,
representing a store for a single element.")
    (license license:bsd-3)))

(define-public ghc-vector
  (package
    (name "ghc-vector")
    (version "0.12.0.3")
    (outputs '("out" "static" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/vector/vector-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1a756s4w759ji3als5alfxwlckh5zcmykfg9rll4mlr2knzvz8mq"))))
    (build-system haskell-build-system)
    ;; FIXME: To simplify upgrading all Haskell packages, we leave the tests
    ;; disabled for now.
    (arguments
     `(#:tests? #f))
    (inputs
     `(("ghc-primitive" ,ghc-primitive)
       ("ghc-random" ,ghc-random)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ;; ("ghc-hunit" ,ghc-hunit)
       ;; ("ghc-test-framework" ,ghc-test-framework)
       ;; ("ghc-test-framework-hunit" ,ghc-test-framework-hunit)
       ;; ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)
       ))
    (home-page "https://github.com/haskell/vector")
    (synopsis "Efficient Arrays")
    (description "This library provides an efficient implementation of
Int-indexed arrays (both mutable and immutable), with a powerful loop
optimisation framework.")
    (license license:bsd-3)))

(define-public ghc-vector-algorithms
  (package
    (name "ghc-vector-algorithms")
    (version "0.8.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "vector-algorithms-" version "/"
                           "vector-algorithms-" version ".tar.gz"))
       (sha256
        (base32
         "1zip8r7hh5g12xrjvhbg38z6hfxy7l6h6pl88qcqc0ygdmwdxg0m"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-vector" ,ghc-vector)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)))
    (home-page "https://github.com/bos/math-functions")
    (synopsis "Algorithms for vector arrays in Haskell")
    (description "This Haskell library algorithms for vector arrays.")
    (license license:bsd-3)))

(define-public ghc-vector-binary-instances
  (package
    (name "ghc-vector-binary-instances")
    (version "0.2.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/"
             "vector-binary-instances/vector-binary-instances-"
             version ".tar.gz"))
       (sha256
        (base32
         "04n5cqm1v95pw1bp68l9drjkxqiy2vswxdq0fy1rqcgxisgvji9r"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-vector" ,ghc-vector)))
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)))
    (home-page "https://github.com/bos/vector-binary-instances")
    (synopsis "Instances of Data.Binary and Data.Serialize for vector")
    (description "This library provides instances of @code{Binary} for the
types defined in the @code{vector} package, making it easy to serialize
vectors to and from disk.  We use the generic interface to vectors, so all
vector types are supported.  Specific instances are provided for unboxed,
boxed and storable vectors.")
    (license license:bsd-3)))

(define-public ghc-vector-builder
  (package
    (name "ghc-vector-builder")
    (version "0.3.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "vector-builder-" version "/"
                           "vector-builder-" version ".tar.gz"))
       (sha256
        (base32
         "0ww0l52p8s6gmh985adnjbvm1vrqpqbm08qdcrvxwhhcqmxgv6m3"))))
    (build-system haskell-build-system)
    (inputs `(("ghc-vector" ,ghc-vector)
              ("ghc-semigroups" ,ghc-semigroups)
              ("ghc-base-prelude" ,ghc-base-prelude)))
    (native-inputs `(("ghc-attoparsec" ,ghc-attoparsec)
                     ("ghc-tasty" ,ghc-tasty)
                     ("ghc-tasty-hunit" ,ghc-tasty-hunit)
                     ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)
                     ("ghc-hunit" ,ghc-hunit)
                     ("ghc-quickcheck-instances" ,ghc-quickcheck-instances)
                     ("ghc-rerebase" ,ghc-rerebase)))
    (home-page "https://github.com/nikita-volkov/vector-builder")
    (synopsis "Vector builder for Haskell")
    (description "This Haskell package provides an API for constructing vectors.
It provides the composable @code{Builder} abstraction, which has instances of the
@code{Monoid} and @code{Semigroup} classes.

You would first use the @code{Builder} abstraction to specify the structure of
the vector; then you can execute the builder to actually produce the
vector. ")
    (license license:expat)))

(define-public ghc-vector-th-unbox
  (package
    (name "ghc-vector-th-unbox")
    (version "0.2.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "vector-th-unbox-" version "/"
                           "vector-th-unbox-" version ".tar.gz"))
       (sha256
        (base32
         "0q8dqnbv1c2gi7jjdhqj14abj1vik23ki6lq4iz2sz18yc7q69fi"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-vector" ,ghc-vector)
       ("ghc-data-default" ,ghc-data-default)))
    (home-page "https://github.com/liyang/vector-th-unbox")
    (synopsis "Deriver for Data.Vector.Unboxed using Template Haskell")
    (description "This Haskell library provides a Template Haskell
deriver for unboxed vectors, given a pair of coercion functions to
and from some existing type with an Unbox instance.")
    (license license:bsd-3)))

(define-public ghc-void
  (package
    (name "ghc-void")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/void/void-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "05vk3x1r9a2pqnzfji475m5gdih2im1h7rbi2sc67p1pvj6pbbsk"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-semigroups" ,ghc-semigroups)
       ("ghc-hashable" ,ghc-hashable)))
    (home-page "https://github.com/ekmett/void")
    (synopsis
     "Logically uninhabited data type")
    (description
     "A Haskell 98 logically uninhabited data type, used to indicate that a
given term should not exist.")
    (license license:bsd-3)))

(define-public ghc-wave
  (package
    (name "ghc-wave")
    (version "0.2.0")
    (source (origin
      (method url-fetch)
      (uri (string-append
             "https://hackage.haskell.org/package/wave/wave-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "149kgwngq3qxc7gxpkqb16j669j0wpv2f3gnvfwp58yg6m4259ki"))))
    (build-system haskell-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'update-constraints
           (lambda _
             (substitute* "wave.cabal"
               (("temporary.* < 1\\.3")
                "temporary >= 1.1 && < 1.4")))))))
    (inputs
     `(("ghc-cereal" ,ghc-cereal)
       ("ghc-data-default-class"
        ,ghc-data-default-class)
       ("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-temporary" ,ghc-temporary)))
    (native-inputs
     `(("hspec-discover" ,hspec-discover)
       ("ghc-hspec" ,ghc-hspec)))
    (home-page "https://github.com/mrkkrp/wave")
    (synopsis "Work with WAVE and RF64 files in Haskell")
    (description "This package allows you to work with WAVE and RF64
files in Haskell.")
    (license license:bsd-3)))

(define-public ghc-wcwidth
  (package
    (name "ghc-wcwidth")
    (version "0.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/wcwidth/wcwidth-"
             version ".tar.gz"))
       (sha256
        (base32
         "1n1fq7v64b59ajf5g50iqj9sa34wm7s2j3viay0kxpmvlcv8gipz"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-setlocale" ,ghc-setlocale)
       ("ghc-utf8-string" ,ghc-utf8-string)
       ("ghc-attoparsec" ,ghc-attoparsec)))
    (home-page "https://github.com/solidsnack/wcwidth/")
    (synopsis "Haskell bindings to wcwidth")
    (description "This package provides Haskell bindings to your system's
native wcwidth and a command line tool to examine the widths assigned by it.
The command line tool can compile a width table to Haskell code that assigns
widths to the Char type.")
    (license license:bsd-3)))

(define-public ghc-wcwidth-bootstrap
  (package
    (inherit ghc-wcwidth)
    (name "ghc-wcwidth-bootstrap")
    (inputs
     `(("ghc-setlocale" ,ghc-setlocale)
       ("ghc-utf8-string" ,ghc-utf8-string)
       ("ghc-attoparsec" ,ghc-attoparsec-bootstrap)))
    (properties '((hidden? #t)))))

(define-public ghc-weigh
  (package
    (name "ghc-weigh")
    (version "0.0.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/weigh/"
                           "weigh-" version ".tar.gz"))
       (sha256
        (base32
         "0l85marb5rl9nr1c0id42dnr5i9fk1jciy5h6lywhb34w3hbj61g"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-split" ,ghc-split)
       ("ghc-temporary" ,ghc-temporary)))
    (home-page "https://github.com/fpco/weigh#readme")
    (synopsis "Measure allocations of a Haskell functions/values")
    (description "This package provides tools to measure the memory usage of a
Haskell value or function.")
    (license license:bsd-3)))

(define-public ghc-wizards
  (package
    (name "ghc-wizards")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/wizards/wizards-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1clvbd1ckhvy29qrbmpkn7bya7300fq6znnps23nn3nxyrxhsr85"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-control-monad-free" ,ghc-control-monad-free)))
    (arguments
     `(#:cabal-revision
       ("1"
        "095qd17zrdhqmcvmslbyzfa5sh9glvvsnsvnlz31gzsmi8nnsgim")))
    (home-page "http://hackage.haskell.org/package/wizards")
    (synopsis "High level, generic library for interrogative user interfaces")
    (description
     "@code{wizards} is a package designed for the quick and painless
development of @emph{interrogative} programs, which revolve around a dialogue
with the user, who is asked a series of questions in a sequence much like an
installation wizard.

Everything from interactive system scripts, to installation wizards, to
full-blown shells can be implemented with the support of @code{wizards}.

It is developed transparently on top of a free monad, which separates out the
semantics of the program from any particular interface.  A variety of backends
exist, including console-based @code{System.Console.Wizard.Haskeline} and
@code{System.Console.Wizard.BasicIO}, and the pure
@code{System.Console.Wizard.Pure}.  It is also possible to write your own
backends, or extend existing back-ends with new features.  While both built-in
IO backends operate on a console, there is no reason why @code{wizards} cannot
also be used for making GUI wizard interfaces.")
    (license license:bsd-3)))

(define-public ghc-wl-pprint
  (package
    (name "ghc-wl-pprint")
    (version "1.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://hackage.haskell.org/package/wl-pprint/wl-pprint-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0kn7y8pdrv8f87zhd5mifcl8fy3b2zvnzmzwhdqhxxlyzwiq6z0c"))))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/wl-pprint")
    (synopsis "Wadler/Leijen pretty printer")
    (description
     "This is a pretty printing library based on Wadler's paper @i{A Prettier
Printer}.  This version allows the library user to declare overlapping
instances of the @code{Pretty} class.")
    (license license:bsd-3)))

(define-public ghc-wl-pprint-annotated
  (package
    (name "ghc-wl-pprint-annotated")
    (version "0.1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://hackage/package/wl-pprint-annotated/wl-pprint-annotated-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1br7qyf27iza213inwhf9bm2k6in0zbmfw6w4clqlc9f9cj2nrkb"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)))
    (home-page
     "https://github.com/minad/wl-pprint-annotated#readme")
    (synopsis
     "Wadler/Leijen pretty printer with annotation support")
    (description
     "Annotations are useful for coloring.  This is a limited version of
@code{wl-pprint-extras} without support for point effects and without the free
monad.  Like in @code{annotated-wl-pprint}, only annotations are supported.
Compared to @code{annotated-wl-pprint} this library provides a slightly
modernized interface.")
    (license license:bsd-3)))

(define-public ghc-wl-pprint-text
  (package
    (name "ghc-wl-pprint-text")
    (version "1.2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/wl-pprint-text/wl-pprint-text-"
             version ".tar.gz"))
       (sha256
        (base32
         "0g3w92rad6x5appfb22rbzcas2ix2h0hy91sdxhq8a4a5cnlrpa0"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-base-compat" ,ghc-base-compat)))
    (home-page "https://hackage.haskell.org/package/wl-pprint-text")
    (synopsis "Wadler/Leijen Pretty Printer for Text values")
    (description
     "A clone of wl-pprint for use with the text library.")
    (license license:bsd-3)))

(define-public ghc-word8
  (package
    (name "ghc-word8")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/word8/word8-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "12jx7f13d2h1djq4fh4dyrab61sm49mj1w61j3rzp2vjfm696c16"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("hspec-discover" ,hspec-discover)))
    (home-page "https://hackage.haskell.org/package/word8")
    (synopsis "Word8 library for Haskell")
    (description "Word8 library to be used with @code{Data.ByteString}.")
    (license license:bsd-3)))

(define-public ghc-wordexp
  (package
    (name "ghc-wordexp")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/wordexp/wordexp-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1mbcrq89jz0dcibw66w0jdy4f4bfpx4zwjfs98rm3jjgdikwdzb4"))))
    (build-system haskell-build-system)
    (native-inputs `(("ghc-c2hs" ,ghc-c2hs)))
    (inputs
     `(("ghc-semigroups" ,ghc-semigroups)))
    (home-page "https://hackage.haskell.org/package/wordexp")
    (synopsis "Library wrapping @code{wordexp} for Haskell")
    (description "@code{wordexp(3)} wrapper library for Haskell to perform
word expansion like a posix-shell.")
    (license license:bsd-3)))

(define-public ghc-x11
  (package
    (name "ghc-x11")
    (version "1.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/X11/"
                           "X11-" version ".tar.gz"))
       (sha256
        (base32 "0gg6852mrlgl8zng1j84fismz7k81jr5fk92glgkscf8q6ryg0bm"))))
    (build-system haskell-build-system)
    (arguments
     `(#:extra-directories
       ("libx11" "libxrandr" "libxinerama" "libxscrnsaver")))
    (inputs
     `(("libx11" ,libx11)
       ("libxrandr" ,libxrandr)
       ("libxinerama" ,libxinerama)
       ("libxscrnsaver" ,libxscrnsaver)
       ("ghc-data-default" ,ghc-data-default)))
    (home-page "https://github.com/haskell-pkg-janitors/X11")
    (synopsis "Bindings to the X11 graphics library")
    (description
     "This package provides Haskell bindings to the X11 graphics library.  The
bindings are a direct translation of the C bindings.")
    (license license:bsd-3)))

(define-public ghc-x11-xft
  (package
    (name "ghc-x11-xft")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/X11-xft/"
                           "X11-xft-" version ".tar.gz"))
       (sha256
        (base32 "1lgqb0s2qfwwgbvwxhjbi23rbwamzdi0l0slfr20c3jpcbp3zfjf"))))
    (arguments
     `(#:extra-directories ("libx11" "libxft" "xorgproto")))
    (inputs
     `(("ghc-x11" ,ghc-x11)
       ("ghc-utf8-string" ,ghc-utf8-string)
       ("libx11" ,libx11)
       ("libxft" ,libxft)
       ("xorgproto" ,xorgproto)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (build-system haskell-build-system)
    (home-page "https://hackage.haskell.org/package/X11-xft")
    (synopsis "Bindings to Xft")
    (description
     "Bindings to the Xft, X Free Type interface library, and some Xrender
parts.")
    (license license:lgpl2.1)))

(define-public ghc-xdg-basedir
  (package
    (name "ghc-xdg-basedir")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/xdg-basedir/"
             "xdg-basedir-" version ".tar.gz"))
       (sha256
        (base32
         "0azlzaxp2dn4l1nr7shsxah2magk1szf6fx0mv75az00qsjw6qg4"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/willdonnelly/xdg-basedir")
    (synopsis "XDG Base Directory library for Haskell")
    (description "This package provides a library implementing the XDG Base Directory spec.")
    (license license:bsd-3)))

(define-public ghc-xml
  (package
    (name "ghc-xml")
    (version "1.3.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/xml/xml-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "0g814lj7vaxvib2g3r734221k80k7ap9czv9hinifn8syals3l9j"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/GaloisInc/xml")
    (synopsis "Simple XML library for Haskell")
    (description "This package provides a simple XML library for Haskell.")
    (license license:bsd-3)))

(define-public ghc-xml-conduit
  (package
    (name "ghc-xml-conduit")
    (version "1.8.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/xml-conduit/"
                           "xml-conduit-" version ".tar.gz"))
       (sha256
        (base32
         "177gmyigxql1pn3ncz0r8annwv5cbxnihbgrrg1dhm4gmc9jy2wq"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-conduit" ,ghc-conduit)
       ("ghc-conduit-extra" ,ghc-conduit-extra)
       ("ghc-doctest" ,ghc-doctest)
       ("ghc-resourcet" ,ghc-resourcet)
       ("ghc-xml-types" ,ghc-xml-types)
       ("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-data-default-class" ,ghc-data-default-class)
       ("ghc-blaze-markup" ,ghc-blaze-markup)
       ("ghc-blaze-html" ,ghc-blaze-html)
       ("ghc-monad-control" ,ghc-monad-control)
       ("ghc-hspec" ,ghc-hspec)
       ("ghc-hunit" ,ghc-hunit)))
    (home-page "https://github.com/snoyberg/xml")
    (synopsis "Utilities for dealing with XML with the conduit package")
    (description
     "This package provides pure-Haskell utilities for dealing with XML with
the @code{conduit} package.")
    (license license:expat)))

(define-public ghc-xml-types
  (package
    (name "ghc-xml-types")
    (version "0.3.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/xml-types/"
                           "xml-types-" version ".tar.gz"))
       (sha256
        (base32
         "1jgqxsa9p2q3h6nymbfmvhldqrqlwrhrzmwadlyc0li50x0d8dwr"))))
    (build-system haskell-build-system)
    (home-page "https://john-millikin.com/software/haskell-xml/")
    (synopsis "Basic types for representing XML")
    (description "This package provides basic types for representing XML
documents.")
    (license license:expat)))

(define-public ghc-xml-hamlet
  (package
    (name "ghc-xml-hamlet")
    (version "0.5.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/xml-hamlet/"
                           "xml-hamlet-" version ".tar.gz"))
       (sha256
        (base32 "0jrhcjy7ww59dafg857f2g2df1fw2jmbwcs1q379ph0pc5rxj3lj"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-shakespeare" ,ghc-shakespeare)
       ("ghc-xml-conduit" ,ghc-xml-conduit)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("ghc-hunit" ,ghc-hunit)))
    (home-page "https://www.yesodweb.com/")
    (synopsis "Hamlet-style quasiquoter for XML content")
    (description "This package provides a type-safe tool for generating XML
code via quasi-quoting built on top of @code{ghc-shakespeare}.")
    (license license:bsd-3)))

(define-public ghc-yaml
  (package
    (name "ghc-yaml")
    (version "0.11.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://hackage.haskell.org/package/"
                                  "yaml/yaml-" version ".tar.gz"))
              (sha256
               (base32
                "028pz77n92l6kjgjv263h4b6yhw1iibdbf3a3dkn5qnz537xpzhc"))))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-conduit" ,ghc-conduit)
       ("ghc-resourcet" ,ghc-resourcet)
       ("ghc-aeson" ,ghc-aeson)
       ("ghc-unordered-containers" ,ghc-unordered-containers)
       ("ghc-vector" ,ghc-vector)
       ("ghc-attoparsec" ,ghc-attoparsec)
       ("ghc-scientific" ,ghc-scientific)
       ("ghc-semigroups" ,ghc-semigroups)
       ("ghc-temporary" ,ghc-temporary)
       ("ghc-enclosed-exceptions" ,ghc-enclosed-exceptions)
       ("ghc-base-compat" ,ghc-base-compat)
       ("ghc-libyaml" ,ghc-libyaml)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)
       ("ghc-hunit" ,ghc-hunit)
       ("hspec-discover" ,hspec-discover)
       ("ghc-mockery" ,ghc-mockery)
       ("ghc-raw-strings-qq" ,ghc-raw-strings-qq)))
    (home-page "https://github.com/snoyberg/yaml/")
    (synopsis "Parsing and rendering YAML documents")
    (description
     "This package provides a library to parse and render YAML documents.")
    (license license:bsd-3)))

(define-public ghc-zip-archive
  (package
    (name "ghc-zip-archive")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/zip-archive/zip-archive-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1cdix5mnxrbs7b2kivhdydhfzgxidd9dqlw71mdw5p21cabwkmf5"))))
    (build-system haskell-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-PATH-for-tests
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((unzip (assoc-ref inputs "unzip"))
                   (which (assoc-ref inputs "which"))
                   (path (getenv "PATH")))
               (setenv "PATH" (string-append unzip "/bin:" which "/bin:" path))
               #t))))))
    (inputs
     `(("ghc-digest" ,ghc-digest)
       ("ghc-temporary" ,ghc-temporary)
       ("ghc-zlib" ,ghc-zlib)))
    (native-inputs
     `(("ghc-hunit" ,ghc-hunit)
       ("unzip" ,unzip)
       ("which" ,which)))
    (home-page "https://hackage.haskell.org/package/zip-archive")
    (synopsis "Zip archive library for Haskell")
    (description "The zip-archive library provides functions for creating,
modifying, and extracting files from zip archives in Haskell.")
    (license license:bsd-3)))

(define-public ghc-zlib
  (package
    (name "ghc-zlib")
    (version "0.6.2.1")
    (outputs '("out" "static" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/zlib/zlib-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1l11jraslcrp9d4wnhwfyhwk4fsiq1aq8i6vj81vcq1m2zzi1y7h"))))
    (build-system haskell-build-system)
    (arguments
     `(#:extra-directories ("zlib")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'strip-test-framework-constraints
           (lambda _
             (substitute* "zlib.cabal"
               (("tasty            >= 0\\.8 && < 0\\.12") "tasty")
               (("tasty-hunit      >= 0\\.8 && < 0\\.10") "tasty-hunit")
               (("tasty-quickcheck == 0\\.8\\.\\*") "tasty-quickcheck")))))))
    (inputs `(("zlib" ,zlib)))
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-tasty" ,ghc-tasty)
       ("ghc-tasty-hunit" ,ghc-tasty-hunit)
       ("ghc-tasty-quickcheck" ,ghc-tasty-quickcheck)))
    (home-page "https://hackage.haskell.org/package/zlib")
    (synopsis
     "Compression and decompression in the gzip and zlib formats")
    (description
     "This package provides a pure interface for compressing and decompressing
streams of data represented as lazy @code{ByteString}s.  It uses the zlib C
library so it has high performance.  It supports the @code{zlib}, @code{gzip}
and @code{raw} compression formats.  It provides a convenient high level API
suitable for most tasks and for the few cases where more control is needed it
provides access to the full zlib feature set.")
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

(define-public ghc-zstd
  (package
    (name "ghc-zstd")
    (version "0.1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://hackage.haskell.org/package/"
                           "zstd/zstd-" version ".tar.gz"))
       (sha256
        (base32
         "147s496zvw13akxqzg65mgfvk3bvhrcilxgf8n786prxg5cm4jz2"))))
    (build-system haskell-build-system)
    (native-inputs
     `(("ghc-quickcheck" ,ghc-quickcheck)
       ("ghc-test-framework" ,ghc-test-framework)
       ("ghc-test-framework-quickcheck2" ,ghc-test-framework-quickcheck2)))
    (home-page "https://github.com/luispedro/hs-zstd")
    (synopsis "Haskell bindings to the Zstandard compression algorithm")
    (description "This library provides Haskell bindings to the
Zstandard compression algorithm, a fast lossless compression algorithm
targeting real-time compression scenarios at zlib-level and better
compression ratios.")
    (license license:bsd-3)))
