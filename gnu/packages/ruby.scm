;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 David Thompson <davet@gnu.org>
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages ruby)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages java)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages gdbm)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages xml)
  #:use-module (guix build-system ruby))

(define-public ruby
  (package
    (name "ruby")
    (version "2.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://cache.ruby-lang.org/pub/ruby/"
                           (version-major+minor version)
                           "/ruby-" version ".tar.xz"))
       (sha256
        (base32
         "19x8gs67klgc3ag815jpin83jn2nv1akgjcgayd6v3h1xplr1v66"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:parallel-tests? #f
       #:phases
       (alist-cons-before
        'configure 'replace-bin-sh
        (lambda _
          (substitute* '("Makefile.in"
                         "ext/pty/pty.c"
                         "io.c"
                         "lib/mkmf.rb"
                         "process.c"
                         "test/rubygems/test_gem_ext_configure_builder.rb"
                         "test/rdoc/test_rdoc_parser.rb"
                         "test/ruby/test_rubyoptions.rb"
                         "test/ruby/test_process.rb"
                         "test/ruby/test_system.rb"
                         "tool/rbinstall.rb")
            (("/bin/sh") (which "sh"))))
        %standard-phases)))
    (inputs
     `(("readline" ,readline)
       ("openssl" ,openssl)
       ("libffi" ,libffi)
       ("gdbm" ,gdbm)
       ("zlib" ,zlib)))
    (native-search-paths
     (list (search-path-specification
            (variable "GEM_PATH")
            (files (list (string-append "lib/ruby/gems/"
                                        (version-major+minor version)
                                        ".0"))))))
    (synopsis "Programming language interpreter")
    (description "Ruby is a dynamic object-oriented programming language with
a focus on simplicity and productivity.")
    (home-page "https://ruby-lang.org")
    (license license:ruby)))

(define-public ruby-2.1
  (package (inherit ruby)
    (version "2.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://cache.ruby-lang.org/pub/ruby/"
                           (version-major+minor version)
                           "/ruby-" version ".tar.bz2"))
       (sha256
        (base32
         "1sbcmbhadcxk0509svwxbm2vvgmpf3xjxr1397bgp9x46nz36lkv"))))
    (arguments
     `(#:test-target "test"
       #:parallel-tests? #f
       #:phases
        (alist-cons-before
         'configure 'replace-bin-sh
         (lambda _
           (substitute* '("Makefile.in"
                          "ext/pty/pty.c"
                          "io.c"
                          "lib/mkmf.rb"
                          "process.c")
             (("/bin/sh") (which "sh"))))
         %standard-phases)))
    (native-search-paths
     (list (search-path-specification
            (variable "GEM_PATH")
            (files (list (string-append "lib/ruby/gems/"
                                        (version-major+minor version)
                                        ".0"))))))))

(define-public ruby-1.8
  (package (inherit ruby)
    (version "1.8.7-p374")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://cache.ruby-lang.org/pub/ruby/"
                           (version-major+minor version)
                           "/ruby-" version ".tar.bz2"))
       (sha256
        (base32
         "1qq7khilwkayrhwmzlxk83scrmiqfi7lgsn4c63znyvz2c1lgqxl"))))
    (native-search-paths '())
    (arguments
     `(#:test-target "test"
       #:parallel-tests? #f
       #:phases
        (alist-cons-before
         'configure 'replace-bin-sh
         (lambda _
           (substitute* '("Makefile.in"
                          "ext/pty/pty.c"
                          "io.c"
                          "lib/mkmf.rb"
                          "process.c")
             (("/bin/sh") (which "sh"))))
         %standard-phases)))))

(define-public ruby-hoe
  (package
    (name "ruby-hoe")
    (version "3.13.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "hoe" version))
              (sha256
               (base32
                "1mac13krdrasn9819dd65xj27kklfy0xdbj3p6s2ij4vlcb46h8q"))) )
    (build-system ruby-build-system)
    (synopsis "Ruby project management helper")
    (description
     "Hoe is a rake/rubygems helper for project Rakefiles.  It helps manage,
maintain, and release projects and includes a dynamic plug-in system allowing
for easy extensibility.  Hoe ships with plug-ins for all the usual project
tasks including rdoc generation, testing, packaging, deployment, and
announcement.")
    (home-page "http://www.zenspider.com/projects/hoe.html")
    (license license:expat)))

(define-public ruby-rake-compiler
  (package
    (name "ruby-rake-compiler")
    (version "0.9.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rake-compiler" version))
              (sha256
               (base32
                "1k8im2vzj849xdgjk6wafspkiwwapqwm738majchb4dnhnsk64cx"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; needs cucumber
    (synopsis "Building and packaging helper for Ruby native extensions")
    (description "Rake-compiler provides a framework for building and
packaging native C and Java extensions in Ruby.")
    (home-page "https://github.com/rake-compiler/rake-compiler")
    (license license:expat)))

(define-public ruby-i18n
  (package
    (name "ruby-i18n")
    (version "0.7.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "i18n" version))
              (sha256
               (base32
                "1i5z1ykl8zhszsxcs8mzl8d0dxgs3ylz8qlzrw74jb0gplkx6758"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; no tests
    (synopsis "Internationalization library for Ruby")
    (description "Ruby i18n is an internationalization and localization
solution for Ruby programs.  It features translation and localization,
interpolation of values to translations, pluralization, customizable
transliteration to ASCII, flexible defaults, bulk lookup, lambdas as
translation data, custom key/scope separator, custom exception handlers, and
an extensible architecture with a swappable backend.")
    (home-page "http://github.com/svenfuchs/i18n")
    (license license:expat)))

;; RSpec is the dominant testing library for Ruby projects.  Even RSpec's
;; dependencies use RSpec for their test suites!  To avoid these circular
;; dependencies, we disable tests for all of the RSpec-related packages.
(define ruby-rspec-support
  (package
    (name "ruby-rspec-support")
    (version "3.2.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rspec-support" version))
              (sha256
               (base32
                "194zry5195ls2hni7r9824vqb5d3qfg4jb15fgj8glfy0rvw3zxl"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; avoid dependency cycles
    (synopsis "RSpec support library")
    (description "Support utilities for RSpec gems.")
    (home-page "https://github.com/rspec/rspec-support")
    (license license:expat)))

(define-public ruby-rspec-core
  (package
    (name "ruby-rspec-core")
    (version "3.2.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rspec-core" version))
              (sha256
               (base32
                "0k2471iw30gc2cvv67damrx666pmsvx8l0ahk3hm20dhfnmcmpvv"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; avoid dependency cycles
    (propagated-inputs
     `(("ruby-rspec-support" ,ruby-rspec-support)))
    (synopsis "RSpec core library")
    (description "Rspec-core provides the RSpec test runner and example
groups.")
    (home-page "https://github.com/rspec/rspec-core")
    (license license:expat)))

(define-public ruby-diff-lcs
  (package
    (name "ruby-diff-lcs")
    (version "1.2.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "diff-lcs" version))
              (sha256
               (base32
                "1vf9civd41bnqi6brr5d9jifdw73j9khc6fkhfl1f8r9cpkdvlx1"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; avoid dependency cycles
    (synopsis "Compute the difference between two Enumerable sequences")
    (description "Diff::LCS computes the difference between two Enumerable
sequences using the McIlroy-Hunt longest common subsequence (LCS) algorithm.
It includes utilities to create a simple HTML diff output format and a
standard diff-like tool.")
    (home-page "https://github.com/halostatue/diff-lcs")
    (license license:expat)))

(define-public ruby-rspec-expectations
  (package
    (name "ruby-rspec-expectations")
    (version "3.2.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rspec-expectations" version))
              (sha256
               (base32
                "01kmchabgpdcaqdsqg8r0g5gy385xhp1k1jsds3w264ypin17n14"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; avoid dependency cycles
    (propagated-inputs
     `(("ruby-rspec-support" ,ruby-rspec-support)
       ("ruby-diff-lcs" ,ruby-diff-lcs)))
    (synopsis "RSpec expectations library")
    (description "Rspec-expectations provides a simple API to express expected
outcomes of a code example.")
    (home-page "https://github.com/rspec/rspec-expectations")
    (license license:expat)))

(define-public ruby-rspec-mocks
  (package
    (name "ruby-rspec-mocks")
    (version "3.2.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rspec-mocks" version))
              (sha256
               (base32
                "09yig1lwgxl8fsns71z3xhv7wkg7zvagydh37pvaqpw92dz55jv2"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; avoid dependency cycles
    (propagated-inputs
     `(("ruby-rspec-support" ,ruby-rspec-support)
       ("ruby-diff-lcs" ,ruby-diff-lcs)))
    (synopsis "RSpec stubbing and mocking library")
    (description "Rspec-mocks provides RSpec's \"test double\" framework, with
support for stubbing and mocking.")
    (home-page "https://github.com/rspec/rspec-mocks")
    (license license:expat)))

(define-public ruby-rspec
  (package
    (name "ruby-rspec")
    (version "3.2.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rspec" version))
              (sha256
               (base32
                "0lkz01j4yxcwb3g5w6r1l9khnyw3sxib4rrh4npd2pxh390fcc4f"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; avoid dependency cycles
    (propagated-inputs
     `(("ruby-rspec-core" ,ruby-rspec-core)
       ("ruby-rspec-mocks" ,ruby-rspec-mocks)
       ("ruby-rspec-expectations" ,ruby-rspec-expectations)))
    (synopsis "Behavior-driven development framework for Ruby")
    (description "RSpec is a behavior-driven development (BDD) framework for
Ruby.  This meta-package includes the RSpec test runner, along with the
expectations and mocks frameworks.")
    (home-page "http://rspec.info/")
    (license license:expat)))

;; Bundler is yet another source of circular dependencies, so we must disable
;; its test suite as well.
(define-public bundler
  (package
    (name "bundler")
    (version "1.10.6")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "bundler" version))
              (sha256
               (base32
                "1vlzfq0bkkj4jyq6av0y55mh5nj5n0f3mfbmmifwgkh44g8k6agv"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; avoid dependency cycles
    (synopsis "Ruby gem bundler")
    (description "Bundler automatically downloads and installs a list of gems
specified in a \"Gemfile\", as well as their dependencies.")
    (home-page "http://bundler.io/")
    (license license:expat)))

(define-public ruby-builder
  (package
    (name "ruby-builder")
    (version "3.2.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "builder" version))
              (sha256
               (base32
                "14fii7ab8qszrvsvhz6z2z3i4dw0h41a62fjr2h1j8m41vbrmyv2"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'do-not-use-rvm
          (lambda _
            (substitute* "rakelib/tags.rake"
              (("RVM_GEMDIR = .*") "RVM_GEMDIR = 'no-rvm-please'\n"))
            #t)))))
    (synopsis "Ruby library to create structured data")
    (description "Builder provides a number of builder objects that make it
easy to create structured data.  Currently the following builder objects are
supported: XML Markup and XML Events.")
    (home-page "https://github.com/jimweirich/builder")
    (license license:expat)))

(define-public ruby-rjb
  (package
    (name "ruby-rjb")
    (version "1.5.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rjb" version))
              (sha256
               (base32
                "0gzs92dagk981s4vrymnqg0vll783b9k564j0cdgp167nc5a2zg4"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f ; no rakefile
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-java-home
          (lambda* (#:key inputs #:allow-other-keys)
            (setenv "JAVA_HOME" (assoc-ref inputs "jdk"))
            #t)))))
    (native-inputs
     `(("jdk" ,icedtea7 "jdk")))
    (synopsis "Ruby-to-Java bridge using the Java Native Interface")
    (description "RJB is a bridge program that connects Ruby and Java via the
Java Native Interface.")
    (home-page "http://www.artonx.org/collabo/backyard/?RubyJavaBridge")
    (license license:lgpl2.1+)))

(define-public ruby-atoulme-antwrap
  (package
    (name "ruby-atoulme-antwrap")
    (version "0.7.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "atoulme-Antwrap" version))
              (sha256
               (base32
                "05s3iw44lqa81f8nfy5f0xjj808600h82zb9bsh46b9kcq2w2kmz"))))
    (build-system ruby-build-system)
    ;; Test data required for most of the tests are not included.
    (arguments `(#:tests? #f))
    (native-inputs
     `(("ruby-hoe" ,ruby-hoe)))
    (inputs
     `(("ruby-rjb" ,ruby-rjb)))
    (synopsis "Ruby wrapper for the Ant build tool")
    (description "Antwrap is a Ruby module that wraps the Apache Ant build
tool.  Antwrap can be used to invoke Ant tasks from a Ruby or a JRuby
script.")
    (home-page "http://rubyforge.org/projects/antwrap/")
    (license license:expat)))

(define-public ruby-orderedhash
  (package
    (name "ruby-orderedhash")
    (version "0.0.6")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "orderedhash" version))
              (sha256
               (base32
                "0fryy7f9jbpx33jq5m402yqj01zcg563k9fsxlqbhmq638p4bzd7"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; no test suite
    (synopsis "Ruby library providing an order-preserving hash")
    (description "Orderedhash is a Ruby library providing a hash
implementation that preserves the order of items and features some array-like
extensions.")
    (home-page "http://codeforpeople.com/lib/ruby/orderedhash/")
    (license license:public-domain)))

(define-public ruby-xml-simple
  (package
    (name "ruby-xml-simple")
    (version "1.1.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "xml-simple" version))
              (sha256
               (base32
                "0xlqplda3fix5pcykzsyzwgnbamb3qrqkgbrhhfz2a2fxhrkvhw8"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; no test suite
    (synopsis "Simple Ruby library for XML processing")
    (description "This library provides a simple API for XML processing in
Ruby.")
    (home-page "https://github.com/maik/xml-simple")
    (license license:ruby)))

(define-public ruby-thor
  (package
    (name "ruby-thor")
    (version "0.19.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "thor" version))
              (sha256
               (base32
                "08p5gx18yrbnwc6xc0mxvsfaxzgy2y9i78xq7ds0qmdm67q39y4z"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; no test suite
    (native-inputs
     `(("bundler" ,bundler)))
    (synopsis "Ruby toolkit for building command-line interfaces")
    (description "Thor is a toolkit for building powerful command-line
interfaces.")
    (home-page "http://whatisthor.com/")
    (license license:expat)))

(define-public ruby-useragent
  (package
    (name "ruby-useragent")
    (version "0.13.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "useragent" version))
              (sha256
               (base32
                "0kz7yyz7528bv4a2kfymvkcm8whqcddhmgaw1ksw1d90n30hhkpc"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; no test suite
    (synopsis "HTTP user agent parser for Ruby")
    (description "UserAgent is a Ruby library that parses and compares HTTP
User Agents.")
    (home-page "https://github.com/gshutler/useragent")
    (license license:expat)))

(define-public ruby-bacon
  (package
    (name "ruby-bacon")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "bacon" version))
              (sha256
               (base32
                "1f06gdj77bmwzc1k5iragl1595hbn67yc7sqvs56ca8plrr2vmai"))))
    (build-system ruby-build-system)
    (synopsis "Small RSpec clone")
    (description "Bacon is a small RSpec clone providing all essential
features.")
    (home-page "https://github.com/chneukirchen/bacon")
    (license license:expat)))

(define-public ruby-arel
  (package
    (name "ruby-arel")
    (version "6.0.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "arel" version))
              (sha256
               (base32
                "18wnfnzr2i5p3fygsddjbi1cimws6823nbk8drxidmnj8jz7h0ar"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; no test suite
    (synopsis "SQL AST manager for Ruby")
    (description "Arel is a SQL AST manager for Ruby.  It simplifies the
generation of complex SQL queries and adapts to various relational database
implementations.")
    (home-page "https://github.com/rails/arel")
    (license license:expat)))

(define-public ruby-connection-pool
  (package
    (name "ruby-connection-pool")
    (version "2.2.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "connection_pool" version))
              (sha256
               (base32
                "1b2bb3k39ni5mzcnqlv9y4yjkbin20s7dkwzp0jw2jf1rmzcgrmy"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("bundler" ,bundler)))
    (synopsis "Generic connection pool for Ruby")
    (description "Connection_pool provides a generic connection pooling
interface for Ruby programs.")
    (home-page "https://github.com/mperham/connection_pool")
    (license license:expat)))

(define-public ruby-net-http-persistent
  (package
    (name "ruby-net-http-persistent")
    (version "2.9.4")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "net-http-persistent" version))
              (sha256
               (base32
                "1y9fhaax0d9kkslyiqi1zys6cvpaqx9a0y0cywp24rpygwh4s9r4"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("ruby-connection-pool" ,ruby-connection-pool)
       ("ruby-hoe" ,ruby-hoe)))
    (synopsis "Persistent HTTP connection manager")
    (description "Net::HTTP::Persistent manages persistent HTTP connections
using Net::HTTP, supporting reconnection and retry according to RFC 2616.")
    (home-page "https://github.com/drbrain/net-http-persistent")
    (license license:expat)))

(define-public ruby-minitest
  (package
    (name "ruby-minitest")
    (version "5.7.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "minitest" version))
              (sha256
               (base32
                "0rxqfakp629mp3vwda7zpgb57lcns5znkskikbfd0kriwv8i1vq8"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("ruby-hoe" ,ruby-hoe)))
    (synopsis "Small test suite library for Ruby")
    (description "Minitest provides a complete suite of Ruby testing
facilities supporting TDD, BDD, mocking, and benchmarking.")
    (home-page "https://github.com/seattlerb/minitest")
    (license license:expat)))

(define-public ruby-minitest-sprint
  (package
    (name "ruby-minitest-sprint")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "minitest-sprint" version))
              (sha256
               (base32
                "179d6pj56l9xzm46fqsqj10mzjkr1f9fv4cxa8wvchs97hqz33w1"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("ruby-hoe" ,ruby-hoe)
       ("ruby-minitest" ,ruby-minitest)))
    (synopsis "Fast test suite runner for minitest")
    (description "Minitest-sprint is a test runner for minitest that makes it
easier to re-run individual failing tests.")
    (home-page "https://github.com/seattlerb/minitest-sprint")
    (license license:expat)))

(define-public ruby-minitest-bacon
  (package
    (name "ruby-minitest-bacon")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "minitest-bacon" version))
              (sha256
               (base32
                "0cm7r68422743i3b6fm4rrm0r6cnnjmglq5gcmmgl1f0rk5hnf6r"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("ruby-hoe" ,ruby-hoe)))
    (inputs
     `(("ruby-minitest" ,ruby-minitest)))
    (synopsis "Bacon compatibility library for minitest")
    (description "Minitest-bacon extends minitest with bacon-like
functionality, making it easier to migrate test suites from bacon to minitest.")
    (home-page "https://github.com/seattlerb/minitest-bacon")
    (license license:expat)))

(define-public ruby-daemons
  (package
    (name "ruby-daemons")
    (version "1.2.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "daemons" version))
              (sha256
               (base32
                "121c7vkimg3baxga69xvdkwxiq8wkmxqvdbyqi5i82vhih5d3cn3"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f)) ; no test suite
    (synopsis "Daemonize Ruby programs")
    (description "Daemons provides a way to wrap existing Ruby scripts to be
run as a daemon and to be controlled by simple start/stop/restart commands.")
    (home-page "https://github.com/thuehlinger/daemons")
    (license license:expat)))

(define-public ruby-git
  (package
    (name "ruby-git")
    (version "1.2.9.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "git" version))
              (sha256
               (base32
                "1sqfj8lmhl7c5zamcckkpik4izfph2zkv6krw0i8mzj5pdws5acs"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:phases (modify-phases %standard-phases
                  (add-after 'install 'patch-git-binary
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      ;; Make the default git binary an absolute path to the
                      ;; store.
                      (let ((git    (string-append (assoc-ref inputs "git")
                                                   "/bin/git"))
                            (config (string-append (getenv "GEM_HOME")
                                                   "/gems/git-" ,version
                                                   "/lib/git/config.rb")))
                        (substitute* (list config)
                          (("'git'")
                           (string-append "'" git "'")))
                        #t))))))
    (inputs
     `(("git" ,git)))
    (synopsis "Ruby wrappers for Git")
    (description "Ruby/Git is a Ruby library that can be used to create, read
and manipulate Git repositories by wrapping system calls to the git binary.")
    (home-page "https://github.com/schacon/ruby-git")
    (license license:expat)))

(define-public ruby-slop
  (package
    (name "ruby-slop")
    (version "4.1.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "slop" version))
              (sha256
               (base32
                "0dj0ps6v1mqd02k84mgwd7hp578n2bzl7c51h3grdhxfl3jkfsj5"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("ruby-minitest" ,ruby-minitest)))
    (synopsis "Ruby command line option parser")
    (description "Slop provides a Ruby domain specific language for gathering
options and parsing command line flags.")
    (home-page "https://github.com/leejarvis/slop")
    (license license:expat)))

(define-public ruby-slop-3
  (package (inherit ruby-slop)
    (version "3.6.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "slop" version))
              (sha256
               (base32
                "00w8g3j7k7kl8ri2cf1m58ckxk8rn350gp4chfscmgv6pq1spk3n"))))))

(define-public ruby-multipart-post
  (package
    (name "ruby-multipart-post")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "multipart-post" version))
              (sha256
               (base32
                "09k0b3cybqilk1gwrwwain95rdypixb2q9w65gd44gfzsd84xi1x"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("bundler" ,bundler)))
    (synopsis "Multipart POST library for Ruby")
    (description "Multipart-Post Adds multipart POST capability to Ruby's
net/http library.")
    (home-page "https://github.com/nicksieger/multipart-post")
    (license license:expat)))

(define-public ruby-arel
  (package
    (name "ruby-arel")
    (version "6.0.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "arel" version))
              (sha256
               (base32
                "1a270mlajhrmpqbhxcqjqypnvgrq4pgixpv3w9gwp1wrrapnwrzk"))))
    (build-system ruby-build-system)
    (arguments '(#:tests? #f)) ; no tests
    (home-page "https://github.com/rails/arel")
    (synopsis "SQL AST manager for Ruby")
    (description "Arel is a SQL AST manager for Ruby.  It simplifies the
generation of complex SQL queries and is compatible with various RDBMSes.")
    (license license:expat)))

(define-public ruby-minitar
  (package
    (name "ruby-minitar")
    (version "0.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "minitar" version))
       (sha256
        (base32
         "1vpdjfmdq1yc4i620frfp9af02ia435dnpj8ybsd7dc3rypkvbka"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; missing a gemspec
    (synopsis "Ruby library and utility for handling tar archives")
    (description
     "Archive::Tar::Minitar is a pure-Ruby library and command-line utility
that provides the ability to deal with POSIX tar archive files.")
    (home-page "http://www.github.com/atoulme/minitar")
    (license (list license:gpl2+ license:ruby))))

(define-public ruby-mini-portile
  (package
    (name "ruby-mini-portile")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "mini_portile" version))
       (sha256
        (base32
         "0h3xinmacscrnkczq44s6pnhrp4nqma7k056x5wv5xixvf2wsq2w"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; tests require network access
    (synopsis "Ports system for Ruby developers")
    (description "Mini-portile is a port/recipe system for Ruby developers.
It provides a standard way to compile against specific versions of libraries
to reproduce user environments.")
    (home-page "http://github.com/flavorjones/mini_portile")
    (license license:expat)))

(define-public ruby-nokogiri
  (package
    (name "ruby-nokogiri")
    (version "1.6.6.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "nokogiri" version))
              (sha256
               (base32
                "1j4qv32qjh67dcrc1yy1h8sqjnny8siyy4s44awla8d6jk361h30"))))
    (build-system ruby-build-system)
    (arguments
     ;; Tests fail because Nokogiri can only test with an installed extension,
     ;; and also because many test framework dependencies are missing.
     '(#:tests? #f
       #:gem-flags (list "--" "--use-system-libraries"
                         (string-append "--with-xml2-include="
                                        (assoc-ref %build-inputs "libxml2")
                                        "/include/libxml2" ))))
    (native-inputs
     `(("ruby-hoe" ,ruby-hoe)
       ("ruby-rake-compiler", ruby-rake-compiler)))
    (inputs
     `(("zlib" ,zlib)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)))
    (propagated-inputs
     `(("ruby-mini-portile" ,ruby-mini-portile)))
    (synopsis "HTML, XML, SAX, and Reader parser for Ruby")
    (description "Nokogiri (鋸) parses and searches XML/HTML, and features
both CSS3 selector and XPath 1.0 support.")
    (home-page "http://www.nokogiri.org/")
    (license license:expat)))

(define-public ruby-method-source
  (package
    (name "ruby-method-source")
    (version "0.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "method_source" version))
       (sha256
        (base32
         "1g5i4w0dmlhzd18dijlqw5gk27bv6dj2kziqzrzb7mpgxgsd1sf2"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("ruby-bacon" ,ruby-bacon)
       ("git" ,git)))
    (synopsis "Retrieve the source code for Ruby methods")
    (description "Method_source retrieves the source code for Ruby methods.
Additionally, it can extract source code from Proc and Lambda objects or just
extract comments.")
    (home-page "https://github.com/banister/method_source")
    (license license:expat)))

(define-public ruby-coderay
  (package
    (name "ruby-coderay")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "coderay" version))
       (sha256
        (base32
         "059wkzlap2jlkhg460pkwc1ay4v4clsmg1bp4vfzjzkgwdckr52s"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; missing test files
    (synopsis "Ruby syntax highlighting library")
    (description "Coderay is a Ruby library that provides syntax highlighting
for select languages.")
    (home-page "http://coderay.rubychan.de")
    (license license:expat)))

(define-public ruby-pry
  (package
    (name "ruby-pry")
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "pry" version))
       (sha256
        (base32
         "1j0r5fm0wvdwzbh6d6apnp7c0n150hpm9zxpm5xvcgfqr36jaj8z"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; no tests
    (propagated-inputs
     `(("ruby-coderay" ,ruby-coderay)
       ("ruby-method-source" ,ruby-method-source)
       ("ruby-slop" ,ruby-slop-3)))
    (synopsis "Ruby REPL")
    (description "Pry is an IRB alternative and runtime developer console for
Ruby.  It features syntax highlighting, a plugin architecture, runtime
invocation, and source and documentation browsing.")
    (home-page "http://pryrepl.org")
    (license license:expat)))

(define-public ruby-thread-safe
  (package
    (name "ruby-thread-safe")
    (version "0.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "thread_safe" version))
       (sha256
        (base32
         "1hq46wqsyylx5afkp6jmcihdpv4ynzzq9ygb6z2pb1cbz5js0gcr"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; needs simplecov, among others
    (synopsis "Thread-safe utilities for Ruby")
    (description "The thread_safe library provides thread-safe collections and
utilities for Ruby.")
    (home-page "https://github.com/ruby-concurrency/thread_safe")
    (license license:asl2.0)))

(define-public ruby-tzinfo
  (package
    (name "ruby-tzinfo")
    (version "1.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "tzinfo" version))
       (sha256
        (base32
         "1c01p3kg6xvy1cgjnzdfq45fggbwish8krd0h864jvbpybyx7cgx"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-thread-safe" ,ruby-thread-safe)))
    (synopsis "Time zone library for Ruby")
    (description "TZInfo is a Ruby library that provides daylight savings
aware transformations between times in different time zones.")
    (home-page "http://tzinfo.github.io")
    (license license:expat)))

(define-public ruby-json
  (package
    (name "ruby-json")
    (version "1.8.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "json" version))
       (sha256
        (base32
         "1nsby6ry8l9xg3yw4adlhk2pnc7i0h0rznvcss4vk3v74qg0k8lc"))))
    (build-system ruby-build-system)
    (arguments '(#:tests? #f)) ; dependency cycle with sdoc
    (synopsis "JSON library for Ruby")
    (description "This Ruby library provides a JSON implementation written as
a native C extension.")
    (home-page "http://json-jruby.rubyforge.org/")
    (license (list license:ruby license:gpl2)))) ; GPL2 only

(define-public ruby-activesupport
  (package
    (name "ruby-activesupport")
    (version "4.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "activesupport" version))
       (sha256
        (base32
         "19n38rj6r1gyxgka18qvcxyla0fwan8a5p3ghq0pp8aj93sbmr6f"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; no tests
    (propagated-inputs
     `(("ruby-i18n" ,ruby-i18n)
       ("ruby-json" ,ruby-json)
       ("ruby-minitest" ,ruby-minitest)
       ("ruby-thread-safe" ,ruby-thread-safe)
       ("ruby-tzinfo" ,ruby-tzinfo)))
    (synopsis "Ruby on Rails utility library")
    (description "ActiveSupport is a toolkit of support libraries and Ruby
core extensions extracted from the Rails framework.  It includes support for
multibyte strings, internationalization, time zones, and testing.")
    (home-page "http://www.rubyonrails.org")
    (license license:expat)))
