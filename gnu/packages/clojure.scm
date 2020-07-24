;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages clojure)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system clojure)
  #:use-module (ice-9 match))

(define-public clojure
  (let* ((lib (lambda (prefix version hash)
                (origin (method url-fetch)
                        (uri (string-append "https://github.com/clojure/"
                                            prefix version ".tar.gz"))
                        (sha256 (base32 hash)))))
         ;; The libraries below are needed to run the tests.
         (libraries
          `(("core-specs-alpha-src"
             ,(lib "core.specs.alpha/archive/core.specs.alpha-"
                   "0.1.24"
                   "0v2a0svf1ar2y42ajxwsjr7zmm5j7pp2zwrd2jh3k7xzd1p9x1fv"))
            ("data-generators-src"
             ,(lib "data.generators/archive/data.generators-"
                   "0.1.2"
                   "0kki093jp4ckwxzfnw8ylflrfqs8b1i1wi9iapmwcsy328dmgzp1"))
            ("spec-alpha-src"
             ,(lib "spec.alpha/archive/spec.alpha-"
                   "0.1.143"
                   "00alf0347licdn773w2jarpllyrbl52qz4d8mw61anjksacxylzz"))
            ("test-check-src"
             ,(lib "test.check/archive/test.check-"
                   "0.9.0"
                   "0p0mnyhr442bzkz0s4k5ra3i6l5lc7kp6ajaqkkyh4c2k5yck1md"))
            ("test-generative-src"
             ,(lib "test.generative/archive/test.generative-"
                   "0.5.2"
                   "1pjafy1i7yblc7ixmcpfq1lfbyf3jaljvkgrajn70sws9xs7a9f8"))
            ("tools-namespace-src"
             ,(lib "tools.namespace/archive/tools.namespace-"
                   "0.2.11"
                   "10baak8v0hnwz2hr33bavshm7y49mmn9zsyyms1dwjz45p5ymhy0"))))
         (library-names (match libraries
                          (((library-name _) ...)
                           library-name))))

    (package
      (name "clojure")
      (version "1.10.0")
      (source (let ((name+version (string-append name "-" version)))
                (origin
                  (method git-fetch)
                  (uri (git-reference
                        (url "https://github.com/clojure/clojure")
                        (commit name+version)))
                  (file-name (string-append name+version "-checkout"))
                  (sha256
                   (base32 "1kcyv2836acs27vi75hvf3r773ahv2nlh9b3j9xa9m9sdanz1h83")))))
      (build-system ant-build-system)
      (arguments
       `(#:imported-modules ((guix build clojure-utils)
                             (guix build guile-build-system)
                             ,@%ant-build-system-modules)
         #:modules ((guix build ant-build-system)
                    (guix build clojure-utils)
                    (guix build java-utils)
                    (guix build utils)
                    (srfi srfi-26))
         #:test-target "test"
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'unpack-library-sources
             (lambda* (#:key inputs #:allow-other-keys)
               (define (extract-library name)
                 (mkdir-p name)
                 (with-directory-excursion name
                   (invoke "tar"
                           "--extract"
                           "--verbose"
                           "--file" (assoc-ref inputs name)
                           "--strip-components=1"))
                 (copy-recursively (string-append name "/src/main/clojure/")
                                   "src/clj/"))
               (for-each extract-library ',library-names)
               #t))
           (add-after 'unpack-library-sources 'fix-manifest-classpath
             (lambda _
               (substitute* "build.xml"
                 (("<attribute name=\"Class-Path\" value=\".\"/>") ""))
               #t))
           (add-after 'build 'build-javadoc ant-build-javadoc)
           (replace 'install (install-jars "./"))
           (add-after 'install-license-files 'install-doc
             (cut install-doc #:doc-dirs '("doc/clojure/") <...>))
           (add-after 'install-doc 'install-javadoc
             (install-javadoc "target/javadoc/")))))
      (native-inputs libraries)
      (home-page "https://clojure.org/")
      (synopsis "Lisp dialect running on the JVM")
      (description "Clojure is a dynamic, general-purpose programming language,
combining the approachability and interactive development of a scripting
language with an efficient and robust infrastructure for multithreaded
programming.  Clojure is a compiled language, yet remains completely dynamic
– every feature supported by Clojure is supported at runtime.  Clojure
provides easy access to the Java frameworks, with optional type hints and type
inference, to ensure that calls to Java can avoid reflection.

Clojure is a dialect of Lisp, and shares with Lisp the code-as-data philosophy
and a powerful macro system.  Clojure is predominantly a functional programming
language, and features a rich set of immutable, persistent data structures.
When mutable state is needed, Clojure offers a software transactional memory
system and reactive Agent system that ensure clean, correct, multithreaded
designs.")
      ;; Clojure is licensed under EPL1.0
      ;; ASM bytecode manipulation library is licensed under BSD-3
      ;; Guava Murmur3 hash implementation is licensed under APL2.0
      ;; src/clj/repl.clj is licensed under CPL1.0

      ;; See readme.html or readme.txt for details.
      (license (list license:epl1.0
                     license:bsd-3
                     license:asl2.0
                     license:cpl1.0)))))

(define-public clojure-algo-generic
  (package
    (name "clojure-algo-generic")
    (version "0.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/clojure/algo.generic")
             (commit (string-append "algo.generic-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s6q10qp276dcpzv06bq1q3bvkvlw03qhmncqcs9cc6p9lc0w4p4"))))
    (build-system clojure-build-system)
    (arguments
     '(#:source-dirs '("src/main/clojure/")
       #:test-dirs '("src/test/clojure/")
       #:doc-dirs '()))
    (synopsis "Generic versions of common functions")
    (description
     "Generic versions of commonly used functions, implemented as multimethods
that can be implemented for any data type.")
    (home-page "https://github.com/clojure/algo.generic")
    (license license:epl1.0)))

(define-public clojure-algo-monads
  (package
    (name "clojure-algo-monads")
    (version "0.1.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/clojure/algo.monads")
             (commit (string-append "algo.monads-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mv3ba72hyhgasg2k3zy83ij61gak6cs4d6qgh8123z3j02mbh8p"))))
    (build-system clojure-build-system)
    (arguments
     '(#:source-dirs '("src/main/clojure/")
       #:test-dirs '("src/test/clojure/")
       #:doc-dirs '()))
    (native-inputs
     `(("clojure-tools-macro" ,clojure-tools-macro)))
    (synopsis
     "Monad Macros and Definitions")
    (description
     "This library contains the most commonly used monads as well as macros for
defining and using monads and useful monadic functions.")
    (home-page "https://github.com/clojure/algo.monads")
    (license license:epl1.0)))

(define-public clojure-core-match
  (let ((commit "1837ffbd4a150e8f3953b2d9ed5cf4a4ad3720a7")
        (revision "1")) ; this is the 1st commit buildable with clojure 1.9
    (package
      (name "clojure-core-match")
      (version (git-version "0.3.0-alpha5" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/clojure/core.match")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "04bdlp5dgkrqzrz0lw3mfwmygj2218qnm1cz3dkb9wy4m0238s4d"))))
      (build-system clojure-build-system)
      (arguments
       '(#:source-dirs '("src/main/clojure")
         #:test-dirs '("src/test/clojure")
         #:doc-dirs '()))
      (synopsis "Optimized pattern matching for Clojure")
      (description
       "An optimized pattern matching library for Clojure.
It supports Clojure 1.5.1 and later as well as ClojureScript.")
      (home-page "https://github.com/clojure/core.match")
      (license license:epl1.0))))

(define-public clojure-instaparse
  (let ((commit "dcfffad5b065e750f0f5835f017cdd8188b8ca2e")
        (version "1.4.9")) ; upstream forget to tag this release
    (package
      (name "clojure-instaparse")
      (version version)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Engelberg/instaparse")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "002mrgin4z3dqy88r1lak7smd0m7x8d22vmliw0m6w6mh5pa17lk"))))
      (build-system clojure-build-system)
      (arguments
       '(#:doc-dirs '("docs/")))
      (synopsis "No grammar left behind")
      (description
       "Instaparse aims to be the simplest way to build parsers in Clojure.

@itemize
@item Turns @emph{standard EBNF or ABNF notation} for context-free grammars
into an executable parser that takes a string as an input and produces a parse
tree for that string.

@item @dfn{No Grammar Left Behind}: Works for @emph{any} context-free grammar,
including @emph{left-recursive}, @emph{right-recursive}, and @emph{ambiguous}
grammars.

@item Extends the power of context-free grammars with PEG-like syntax for
lookahead and negative lookahead.

@item Supports both of Clojure's most popular tree formats (hiccup and enlive)
as output targets

@item Detailed reporting of parse errors.

@item Optionally produces lazy sequence of all parses (especially useful for
diagnosing and debugging ambiguous grammars).

@item ``Total parsing'' mode where leftover string is embedded in the parse
tree.

@item Optional combinator library for building grammars programmatically.

@item Performant.
@end itemize")
      (home-page "https://github.com/Engelberg/instaparse")
      (license license:epl1.0))))

(define-public clojure-tools-macro
  (package
    (name "clojure-tools-macro")
    (version "0.1.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/clojure/tools.macro")
             (commit (string-append "tools.macro-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14mdxqkwja0cffmyfav5pbcli2qvw1mjdgz0n619a2z2036andx8"))))
    (build-system clojure-build-system)
    (arguments
     '(#:source-dirs '("src/main/clojure/")
       #:test-dirs '("src/test/clojure/")
       #:doc-dirs '()))
    (synopsis "Utilities for macro writers")
    (description "Tools for writing macros.")
    (home-page "https://github.com/clojure/tools.macro")
    (license license:epl1.0)))

(define-public clojure-tools-cli
  (package
    (name "clojure-tools-cli")
    (version "0.4.2")
    (home-page "https://github.com/clojure/tools.cli")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "tools.cli-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1yqlm8lwbcjm0dp032z7vzc4bdlmc4jixznvf4adsqhvqw85hvj2"))))
    (build-system clojure-build-system)
    (arguments
     '(#:source-dirs '("src/main/clojure/")
       #:test-dirs '("src/test/clojure/")
       #:doc-dirs '()))
    (synopsis "Clojure library for working with command-line arguments")
    (description
     "The @code{tools.cli} library provides Clojure programmers with tools to
work with command-line arguments.")
    (license license:epl1.0)))
