;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2014, 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014, 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016, 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017 ng0 <ng0@n0.is>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
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

(define-module (gnu packages ruby)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages java)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ragel)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages web)
  #:use-module (guix build-system ruby))

(define-public ruby
  (package
    (name "ruby")
    (replacement ruby-2.4.3)
    (version "2.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://cache.ruby-lang.org/pub/ruby/"
                           (version-major+minor version)
                           "/ruby-" version ".tar.xz"))
       (sha256
        (base32
         "0dgp4ypk3smrsbh2c249n5pl6nqhpd2igq9484dbsh81sf08k2kl"))
       (modules '((guix build utils)))
       (snippet `(begin
                   ;; Remove bundled libffi
                   (delete-file-recursively "ext/fiddle/libffi-3.2.1")
                   #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'replace-bin-sh-and-remove-libffi
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
               (("/bin/sh") (which "sh")))
             #t)))))
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
    (home-page "https://www.ruby-lang.org")
    (license license:ruby)))

(define-public ruby-2.4.3
  (package
    (inherit ruby)
    (name "ruby")
    (version "2.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://cache.ruby-lang.org/pub/ruby/"
                           (version-major+minor version)
                           "/ruby-" version ".tar.xz"))
       (sha256
        (base32
         "0l9bv67dgsphk42lmiskhrnh47hbyj6rfg2rcjx22xivpx07srr3"))
       (modules '((guix build utils)))
       (snippet `(begin
                   ;; Remove bundled libffi
                   (delete-file-recursively "ext/fiddle/libffi-3.2.1")
                   #t))))))

(define-public ruby-2.3
  (package
    (inherit ruby)
    (version "2.3.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://cache.ruby-lang.org/pub/ruby/"
                           (version-major+minor version)
                           "/ruby-" version ".tar.xz"))
       (sha256
        (base32
         "0mlz0mk7yyxia37k8fdv8m8a72h61nfbns28430h796l4an6kng0"))
       (modules '((guix build utils)))
       (snippet `(begin
                   ;; Remove bundled libffi
                   (delete-file-recursively "ext/fiddle/libffi-3.2.1")
                   #t))))))

(define-public ruby-2.2
  (package (inherit ruby)
    (version "2.2.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://cache.ruby-lang.org/pub/ruby/"
                           (version-major+minor version)
                           "/ruby-" version ".tar.xz"))
       (sha256
        (base32
         "0p18xykx8dm5mmlx5n5243z67lj4vbvwr70bnc5x12am22ql8fri"))))))

(define-public ruby-2.1
  (package (inherit ruby)
    (version "2.1.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://cache.ruby-lang.org/pub/ruby/"
                           (version-major+minor version)
                           "/ruby-" version ".tar.bz2"))
       (sha256
        (base32
         "1wglbd599mlwxfcb2xgqcxi2shr363pjn5dpbv11m04si9bpaim7"))))
    (arguments
     `(#:test-target "test"
       #:parallel-tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'replace-bin-sh
           (lambda _
             (substitute* '("Makefile.in"
                            "ext/pty/pty.c"
                            "io.c"
                            "lib/mkmf.rb"
                            "process.c")
               (("/bin/sh") (which "sh")))
             #t)))))
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
       (modify-phases %standard-phases
         (add-before 'configure 'replace-bin-sh
           (lambda _
             (substitute* '("Makefile.in"
                            "ext/pty/pty.c"
                            "io.c"
                            "lib/mkmf.rb"
                            "process.c")
               (("/bin/sh") (which "sh")))
             #t)))))))

(define (gem-directory ruby-version)
  "Return the relative gem install directory for RUBY-VERSION."
  (string-append "/lib/ruby/gems/" (version-major+minor ruby-version)
                 ".0/gems"))

(define-public ruby-highline
  (package
    (name "ruby-highline")
    (version "1.7.10")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "highline" version))
       (sha256
        (base32
         "01ib7jp85xjc4gh4jg0wyzllm46hwv8p0w1m4c75pbgi41fps50y"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f)) ;; TODO: NameError: uninitialized constant SPEC
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-code-statistics" ,ruby-code-statistics)))
    (synopsis
     "HighLine helps you build command-line interfaces")
    (description
     "HighLine provides a high-level IO library that provides validation,
type conversion, and more for command-line interfaces.  HighLine also includes
a menu system for providing multiple options to the user.")
    (home-page "https://github.com/JEG2/highline")
    (license (list license:gpl2 license:ruby))))

(define-public ruby-hoe
  (package
    (name "ruby-hoe")
    (version "3.16.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "hoe" version))
              (sha256
               (base32
                "12q6dn2irsfamdbjpqvs0dwl4i1vl7wflxrcg972h9jw0ds38f3a"))))
    (build-system ruby-build-system)
    (synopsis "Ruby project management helper")
    (description
     "Hoe is a rake/rubygems helper for project Rakefiles.  It helps manage,
maintain, and release projects and includes a dynamic plug-in system allowing
for easy extensibility.  Hoe ships with plug-ins for all the usual project
tasks including rdoc generation, testing, packaging, deployment, and
announcement.")
    (home-page "https://www.zenspider.com/projects/hoe.html")
    (license license:expat)))

(define-public ruby-rake-compiler
  (package
    (name "ruby-rake-compiler")
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rake-compiler" version))
              (sha256
               (base32
                "1xpdi4w8zaklk1i9ps8g3k0icw3v5fcks092l84w28rgrpx82qip"))))
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
    (home-page "https://github.com/svenfuchs/i18n")
    (license license:expat)))

;; RSpec is the dominant testing library for Ruby projects.  Even RSpec's
;; dependencies use RSpec for their test suites!  To avoid these circular
;; dependencies, we disable tests for all of the RSpec-related packages.
(define ruby-rspec-support
  (package
    (name "ruby-rspec-support")
    (version "3.5.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rspec-support" version))
              (sha256
               (base32
                "10vf3k3d472y573mag2kzfsfrf6rv355s13kadnpryk8d36yq5r0"))))
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
    (version "3.5.4")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rspec-core" version))
              (sha256
               (base32
                "1nacs062qbr98fx6czf1vwppn1js956nv2c8vfwj6i65axdfs46i"))))
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

(define-public ruby-rspec-core-2
  (package (inherit ruby-rspec-core)
    (version "2.14.8")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rspec-core" version))
              (sha256
               (base32
                "0psjy5kdlz3ph39br0m01w65i1ikagnqlg39f8p65jh5q7dz8hwc"))))
    (propagated-inputs `())))

(define-public ruby-diff-lcs
  (package
    (name "ruby-diff-lcs")
    (version "1.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "diff-lcs" version))
              (sha256
               (base32
                "18w22bjz424gzafv6nzv98h0aqkwz3d9xhm7cbr1wfbyas8zayza"))))
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
    (version "3.5.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rspec-expectations" version))
              (sha256
               (base32
                "0bbqfrb1x8gmwf8x2xhhwvvlhwbbafq4isbvlibxi6jk602f09gs"))))
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

(define-public ruby-rspec-expectations-2
  (package (inherit ruby-rspec-expectations)
    (version "2.14.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rspec-expectations" version))
              (sha256
               (base32
                "1ni8kw8kjv76jvwjzi4jba00k3qzj9f8wd94vm6inz0jz3gwjqf9"))))
    (propagated-inputs
     `(("ruby-diff-lcs" ,ruby-diff-lcs)))))

(define-public ruby-rspec-mocks
  (package
    (name "ruby-rspec-mocks")
    (version "3.5.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rspec-mocks" version))
              (sha256
               (base32
                "0nl3ksivh9wwrjjd47z5dggrwx40v6gpb3a0gzbp1gs06a5dmk24"))))
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

(define-public ruby-rspec-mocks-2
  (package (inherit ruby-rspec-mocks)
    (version "2.14.6")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rspec-mocks" version))
              (sha256
               (base32
                "1fwsmijd6w6cmqyh4ky2nq89jrpzh56hzmndx9wgkmdgfhfakv30"))))
    (propagated-inputs
     `(("ruby-diff-lcs" ,ruby-diff-lcs)))))

(define-public ruby-rspec
  (package
    (name "ruby-rspec")
    (version "3.5.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rspec" version))
              (sha256
               (base32
                "16g3mmih999f0b6vcz2c3qsc7ks5zy4lj1rzjh8hf6wk531nvc6s"))))
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

(define-public ruby-rspec-2
  (package (inherit ruby-rspec)
    (version "2.14.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rspec" version))
              (sha256
               (base32
                "134y4wzk1prninb5a0bhxgm30kqfzl8dg06af4js5ylnhv2wd7sg"))))
    (propagated-inputs
     `(("ruby-rspec-core" ,ruby-rspec-core-2)
       ("ruby-rspec-mocks" ,ruby-rspec-mocks-2)
       ("ruby-rspec-expectations" ,ruby-rspec-expectations-2)))))

;; Bundler is yet another source of circular dependencies, so we must disable
;; its test suite as well.
(define-public bundler
  (package
    (name "bundler")
    (version "1.15.4")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "bundler" version))
              (sha256
               (base32
                "0wl4r7wbwdq68xidfv4hhzfb1spb6lmhbspwlzrg4pf1l6ipxlgs"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; avoid dependency cycles
    (synopsis "Ruby gem bundler")
    (description "Bundler automatically downloads and installs a list of gems
specified in a \"Gemfile\", as well as their dependencies.")
    (home-page "https://bundler.io/")
    (license license:expat)))

(define-public ruby-builder
  (package
    (name "ruby-builder")
    (version "3.2.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "builder" version))
              (sha256
               (base32
                "0qibi5s67lpdv1wgcj66wcymcr04q6j4mzws6a479n0mlrmh5wr1"))))
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
    (version "1.5.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rjb" version))
              (sha256
               (base32
                "1ppj8rbicj3w0nhh7f73mflq19yd7pzdzkh2a91hcvphriy5b0ca"))))
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
     `(("jdk" ,icedtea "jdk")))
    (synopsis "Ruby-to-Java bridge using the Java Native Interface")
    (description "RJB is a bridge program that connects Ruby and Java via the
Java Native Interface.")
    (home-page "https://www.artonx.org/collabo/backyard/?RubyJavaBridge")
    (license license:lgpl2.1+)))

(define-public ruby-log4r
  (package
    (name "ruby-log4r")
    (version "1.1.10")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "log4r" version))
        (sha256
          (base32
            "0ri90q0frfmigkirqv5ihyrj59xm8pq5zcmf156cbdv4r4l2jicv"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; no Rakefile in gem
    (synopsis "Flexible logging library for Ruby")
    (description "Comprehensive and flexible logging library written
in Ruby for use in Ruby programs.  It features a hierarchical logging
system of any number of levels, custom level names, logger
inheritance, multiple output destinations per log event, execution
tracing, custom formatting, thread safteyness, XML and YAML
configuration, and more.")
     (home-page "http://log4r.rubyforge.org/")
     (license license:bsd-3)))

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

(define-public ruby-atoulme-saikuro
  (package
    (name "ruby-atoulme-saikuro")
    (version "1.2.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "atoulme-Saikuro" version))
              (sha256
               (base32
                "0kvd2nsxffbza61d3q4j94wrbnbv50r1zy3a7q26f6k706fw1f19"))))
    (build-system ruby-build-system)
    ;; FIXME: There are no unit tests.  The tests are demonstrations of the
    ;; "saikuro" tool.
    (arguments `(#:tests? #f))
    (synopsis "Cyclomatic complexity analyzer")
    (description "Saikuro is a Ruby cyclomatic complexity analyzer.  When
given Ruby source code Saikuro will generate a report listing the cyclomatic
complexity of each method found.  In addition, Saikuro counts the number of
lines per method and can generate a listing of the number of tokens on each
line of code.")
    (home-page "http://www.github.com/atoulme/Saikuro")
    ;; File headers contain the BSD-3 license and the README.rdoc says that
    ;; "Saikuro uses the BSD license", but the LICENSE file contains the text
    ;; of the Expat license.
    (license license:bsd-3)))

(define-public ruby-ci-reporter
  (package
    (name "ruby-ci-reporter")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "ci_reporter" version))
              (sha256
               (base32
                "17fm20jmw3ajdryhkkxpjahcfx7bgswqzxrskivlkns2718ayyyg"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "rspec"))
    (propagated-inputs
     `(("ruby-builder" ,ruby-builder)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)))
    (synopsis "Generate XML reports of runs test")
    (description
     "@code{CI::Reporter} is an add-on to Ruby testing frameworks that allows
you to generate XML reports of your test runs.  The resulting files can be
read by a continuous integration system that understands Ant's JUnit report
format.")
    (home-page "https://github.com/nicksieger/ci_reporter")
    (license license:expat)))

(define-public ruby-saikuro-treemap
  (package
    (name "ruby-saikuro-treemap")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "saikuro_treemap" version))
              (sha256
               (base32
                "0w70nmh43mwfbpq20iindl61siqqr8acmf7p3m7n5ipd61c24950"))))
    (build-system ruby-build-system)
    ;; Some of the tests fail because the generated JSON has keys in a
    ;; different order.  This is a problem with the test suite rather than any
    ;; of the involved libraries.
    (arguments `(#:tests? #f))
    (propagated-inputs
     `(("ruby-json-pure" ,ruby-json-pure)
       ("ruby-atoulme-saikuro" ,ruby-atoulme-saikuro)))
    (synopsis "Generate complexity treemap based on saikuro analysis")
    (description
     "This gem generates a treemap showing the complexity of Ruby code on
which it is run.  It uses Saikuro under the covers to analyze Ruby code
complexity.")
    (home-page "https://github.com/ThoughtWorksStudios/saikuro_treemap")
    (license license:expat)))

(define-public ruby-options
  (package
    (name "ruby-options")
    (version "2.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "options" version))
       (sha256
        (base32
         "1s650nwnabx66w584m1cyw82icyym6hv5kzfsbp38cinkr5klh9j"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f ;; TODO: NameError: uninitialized constant Config
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-LIB
           (lambda _
             ;; This is used in the Rakefile, and setting it avoids an issue
             ;; with running the tests.
             (setenv "LIB" "options"))))))
    (synopsis "Ruby library to parse options from *args cleanly")
    (description
     "The @code{options} library helps with parsing keyword options in Ruby
functions.")
    (home-page "https://github.com/ahoward/options")
    (license license:ruby)))

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

(define-public ruby-libxml
  (package
    (name "ruby-libxml")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "libxml-ruby" version))
       (sha256
        (base32
         "0xy8wmjwjcnv36zi042678ncjzpxvy351ccbv7mzkns2n3kxfp54"))))
    (build-system ruby-build-system)
    (inputs
     `(("zlib" ,zlib)
       ("libxml2" ,libxml2)))
    (arguments
     '(#:tests? #f ; test suite hangs for unknown reason
       #:gem-flags
       (list "--"
             (string-append "--with-xml2-include="
                            (assoc-ref %build-inputs "libxml2")
                            "/include/libxml2" ))))
    (synopsis "Ruby bindings for GNOME Libxml2")
    (description "The Libxml-Ruby project provides Ruby language bindings for
the GNOME Libxml2 XML toolkit.")
    (home-page "https://xml4r.github.com/libxml-ruby")
    (license license:expat)))

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
    (version "0.19.4")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "thor" version))
              (sha256
               (base32
                "01n5dv9kql60m6a00zc0r66jvaxx98qhdny3klyj0p3w34pad2ns"))))
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

(define-public ruby-lumberjack
  (package
    (name "ruby-lumberjack")
    (version "1.0.12")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "lumberjack" version))
              (sha256
               (base32
                "0yz26k9mi0djx1qvlmvdw1xw2yf7a2rkfmnb2j0d28kms33xpibp"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("ruby-rspec" ,ruby-rspec)
       ("ruby-timecop" ,ruby-timecop)))
    (synopsis "Logging utility library for Ruby")
    (description "Lumberjack is a simple logging utility that can be a drop in
replacement for Logger or ActiveSupport::BufferedLogger.  It provides support
for automatically rolling log files even with multiple processes writing the
same log file.")
    (home-page "https://github.com/bdurand/lumberjack")
    (license license:expat)))

(define-public ruby-nenv
  (package
    (name "ruby-nenv")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "nenv" version))
              (sha256
               (base32
                "0r97jzknll9bhd8yyg2bngnnkj8rjhal667n7d32h8h7ny7nvpnr"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f)) ; no tests included
    (native-inputs
     `(("ruby-rspec" ,ruby-rspec)
       ("bundler" ,bundler)))
    (synopsis "Ruby interface for modifying the environment")
    (description "Nenv provides a convenient wrapper for Ruby's ENV to modify
and inspect the environment.")
    (home-page "https://github.com/e2/nenv")
    (license license:expat)))

(define-public ruby-permutation
  (package
    (name "ruby-permutation")
    (version "0.1.8")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "permutation" version))
              (sha256
               (base32
                "13crwk2vfbzv99czva7881027dbcnidihmvx2jc58z2vm3bp9sl8"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-rakefile
          (lambda _
            (substitute* "Rakefile"
              (("require 'rake/gempackagetask'")
               "require 'rubygems/package_task'")
              (("include Config") ""))
            #t))
         (replace 'check
          (lambda _
            (zero? (system* "ruby" "-Ilib" "test/test.rb")))))))
    (synopsis "Library to perform operations with sequence permutations")
    (description "This package provides a Ruby library to perform different
operations with permutations of sequences, such as strings and arrays.")
    (home-page "https://flori.github.io/permutation")
    (license license:gpl2))) ; GPL 2 only

(define-public ruby-shellany
  (package
    (name "ruby-shellany")
    (version "0.0.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "shellany" version))
              (sha256
               (base32
                "1ryyzrj1kxmnpdzhlv4ys3dnl2r5r3d2rs2jwzbnd1v96a8pl4hf"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "default"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-version-test
          (lambda _
            (substitute* "spec/shellany_spec.rb"
              (("^RSpec") "require \"shellany\"\nRSpec"))
            #t)))))
    (native-inputs
     `(("ruby-rspec" ,ruby-rspec)
       ("ruby-nenv" ,ruby-nenv)
       ("bundler" ,bundler)))
    (synopsis "Capture command output")
    (description "Shellany is a Ruby library providing functions to capture
the output produced by running shell commands.")
    (home-page "https://rubygems.org/gems/shellany")
    (license license:expat)))

(define-public ruby-notiffany
  (package
    (name "ruby-notiffany")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "notiffany" version))
              (sha256
               (base32
                "0x838fa5il0dd9zbm3lxkpbfxcf5fxv9556mayc2mxsdl5ghv8nx"))))
    (build-system ruby-build-system)
    ;; Tests are not included in the gem.
    (arguments `(#:tests? #f))
    (propagated-inputs
     `(("ruby-shellany" ,ruby-shellany)
       ("ruby-nenv" ,ruby-nenv)))
    (native-inputs
     `(("bundler" ,bundler)))
    (synopsis "Wrapper libray for notification libraries")
    (description "Notiffany is a Ruby wrapper libray for notification
libraries such as Libnotify.")
    (home-page "https://github.com/guard/notiffany")
    (license license:expat)))

(define-public ruby-formatador
  (package
    (name "ruby-formatador")
    (version "0.2.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "formatador" version))
              (sha256
               (base32
                "1gc26phrwlmlqrmz4bagq1wd5b7g64avpx0ghxr9xdxcvmlii0l0"))))
    (build-system ruby-build-system)
    ;; Circular dependency: Tests require ruby-shindo, which requires
    ;; ruby-formatador at runtime.
    (arguments `(#:tests? #f))
    (synopsis "Ruby library to format text on stdout")
    (description "Formatador is a Ruby library to format text printed to the
standard output stream.")
    (home-page "https://github.com/geemus/formatador")
    (license license:expat)))

(define-public ruby-shindo
  (package
    (name "ruby-shindo")
    (version "0.3.8")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "shindo" version))
              (sha256
               (base32
                "0s8v1jbz8i0jh92f2fgxb3p51l1azrpkc8nv4mhrqy4vndpvd7wq"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "shindo_tests"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-tests
          (lambda _
            (substitute* "Rakefile"
              (("system \"shindo") "system \"./bin/shindo")
              ;; This test doesn't work, so we disable it.
              (("fail \"The build_error test should fail") "#"))
            #t)))))
    (propagated-inputs
     `(("ruby-formatador" ,ruby-formatador)))
    (synopsis "Simple depth first Ruby testing")
    (description "Shindo is a simple depth first testing library for Ruby.")
    (home-page "https://github.com/geemus/shindo")
    (license license:expat)))

(define-public ruby-rubygems-tasks
  (package
    (name "ruby-rubygems-tasks")
    (version "0.2.4")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rubygems-tasks" version))
              (sha256
               (base32
                "16cp45qlbcglnqdm4f1vj3diywdz4v024saqpgrz6palf0wmgz2j"))))
    (build-system ruby-build-system)
    ;; Tests need Internet access.
    (arguments `(#:tests? #f))
    (native-inputs
     `(("ruby-rspec" ,ruby-rspec)
       ("ruby-yard" ,ruby-yard)))
    (synopsis "Rake tasks for managing and releasing Ruby Gems")
    (description "Rubygems-task provides Rake tasks for managing and releasing
Ruby Gems.")
    (home-page "https://github.com/postmodern/rubygems-tasks")
    (license license:expat)))

(define-public ruby-rubyzip
  (package
  (name "ruby-rubyzip")
  (version "1.2.1")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "rubyzip" version))
      (sha256
        (base32
          "06js4gznzgh8ac2ldvmjcmg9v1vg9llm357yckkpylaj6z456zqz"))))
  (build-system ruby-build-system)
  (arguments
   '(#:phases
     (modify-phases %standard-phases
       (add-before 'check 'patch-tests
         (lambda* (#:key inputs #:allow-other-keys)
           (substitute* "test/gentestfiles.rb"
             (("/usr/bin/zip")
              (string-append
               (assoc-ref inputs "zip") "/bin/zip")))
           (substitute* "test/input_stream_test.rb"
             (("/usr/bin/env ruby") (which "ruby")))
           #t)))))
  (native-inputs
   `(("bundler" ,bundler)
     ("ruby-simplecov" ,ruby-simplecov)
     ("zip" ,zip)
     ("unzip" ,unzip)))
  (synopsis "Ruby module is for reading and writing zip files")
  (description
    "The rubyzip module provides ways to read from and create zip files.")
  (home-page "http://github.com/rubyzip/rubyzip")
  (license license:bsd-2)))

(define-public ruby-simplecov-html
  (package
    (name "ruby-simplecov-html")
    (version "0.10.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "simplecov-html" version))
              (sha256
               (base32
                "1lihraa4rgxk8wbfl77fy9sf0ypk31iivly8vl3w04srd7i0clzn"))))
    (build-system ruby-build-system)
    (arguments `(#:tests? #f)) ; there are no tests
    (native-inputs
     `(("bundler" ,bundler)))
    (synopsis "Default HTML formatter for SimpleCov code coverage tool")
    (description "This package provides the default HTML formatter for
the SimpleCov code coverage tool for Ruby version 1.9 and above.")
    (home-page "https://github.com/colszowka/simplecov-html")
    (license license:expat)))

(define-public ruby-simplecov
  (package
    (name "ruby-simplecov")
    (version "0.12.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "simplecov" version))
              (sha256
               (base32
                "0ffhyrfnq2zm2mc1742a4hqy475g3qa1zf6yfldwg1ldh5sn3qbx"))))
    (build-system ruby-build-system)
    ;; Simplecov depends on rubocop for code style checking at build time.
    ;; Rubocop needs simplecov at build time.
    (arguments `(#:tests? #f))
    (propagated-inputs
     `(("ruby-json" ,ruby-json)
       ("ruby-docile" ,ruby-docile)
       ("ruby-simplecov-html" ,ruby-simplecov-html)))
    (native-inputs
     `(("bundler" ,bundler)))
    (synopsis "Code coverage framework for Ruby")
    (description "SimpleCov is a code coverage framework for Ruby with a
powerful configuration library and automatic merging of coverage across test
suites.")
    (home-page "https://github.com/colszowka/simplecov")
    (license license:expat)))

(define-public ruby-useragent
  (package
    (name "ruby-useragent")
    (version "0.16.8")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "useragent" version))
              (sha256
               (base32
                "1139cjqyv1hk1qcw89k81ajjkqyakqgbcyvmfrsmjqi8yn9kgqhq"))))
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

(define-public ruby-connection-pool
  (package
    (name "ruby-connection-pool")
    (version "2.2.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "connection_pool" version))
              (sha256
               (base32
                "17vpaj6kyf2i8bimaxz7rg1kyadf4d10642ja67qiqlhwgczl2w7"))))
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
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "net-http-persistent" version))
              (sha256
               (base32
                "156rv95bgxfz6qw5y1r7c7bswr77918hygl8dyl14qzbqc5vyp18"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("ruby-connection-pool" ,ruby-connection-pool)
       ("ruby-hoe" ,ruby-hoe)))
    (synopsis "Persistent HTTP connection manager")
    (description "Net::HTTP::Persistent manages persistent HTTP connections
using Net::HTTP, supporting reconnection and retry according to RFC 2616.")
    (home-page "https://github.com/drbrain/net-http-persistent")
    (license license:expat)))

(define-public ruby-power-assert
  (package
    (name "ruby-power-assert")
    (version "0.2.7")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "power_assert" version))
              (sha256
               (base32
                "0ka6w71lcan4wgf111xi3pcn9ma9lhakv31jg8w007nwzi0xfjbi"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("bundler" ,bundler)))
    (synopsis "Assert library with descriptive assertion messages")
    (description "Power-assert is an assertion library providing descriptive
assertion messages for tests.")
    (home-page "https://github.com/k-tsj/power_assert")
    (license (list license:bsd-2 license:ruby))))

(define-public ruby-locale
  (package
    (name "ruby-locale")
    (version "2.1.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "locale" version))
              (sha256
               (base32
                "1sls9bq4krx0fmnzmlbn64dw23c4d6pz46ynjzrn9k8zyassdd0x"))))
    (build-system ruby-build-system)
    ;; ruby-test-unit is required to run tests, but that needs ruby-packnga,
    ;; which needs ruby-gettext, which needs ruby-locale.  To break the
    ;; dependency cycle we disable tests.
    (arguments `(#:tests? #f))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-yard" ,ruby-yard)))
    (synopsis "Ruby library providing basic localization APIs")
    (description
     "Ruby-Locale is the pure ruby library which provides basic APIs for
localization.")
    (home-page "https://github.com/ruby-gettext/locale")
    (license (list license:lgpl3+ license:ruby))))

(define-public ruby-text
  (package
    (name "ruby-text")
    (version "1.3.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "text" version))
              (sha256
               (base32
                "1x6kkmsr49y3rnrin91rv8mpc3dhrf3ql08kbccw8yffq61brfrg"))))
    (build-system ruby-build-system)
    (synopsis "Collection of text algorithms for Ruby")
    (description
     "This package provides a collection of text algorithms: Levenshtein,
Soundex, Metaphone, Double Metaphone, Porter Stemming.")
    (home-page "https://github.com/threedaymonk/text")
    (license license:expat)))

(define-public ruby-gettext
  (package
    (name "ruby-gettext")
    (version "3.1.7")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "gettext" version))
              (sha256
               (base32
                "1hg9islkm324mb4sd4za1fgafj1hqnm3bdvzj3k4fqpnzqnbcfiq"))))
    (build-system ruby-build-system)
    ;; ruby-test-unit is required to run tests, but that needs ruby-packnga,
    ;; which needs ruby-gettext.  To break the dependency cycle we disable
    ;; tests.
    (arguments `(#:tests? #f))
    (propagated-inputs
     `(("ruby-locale" ,ruby-locale)
       ("ruby-text" ,ruby-text)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-yard" ,ruby-yard)))
    (synopsis "GNU gettext-like program for Ruby")
    (description
     "Gettext is a GNU gettext-like program for Ruby.  The catalog
file (po-file) used is the same as that used by GNU gettext, allowing you to
use GNU gettext tools for maintenance.")
    (home-page "https://ruby-gettext.github.com/")
    (license (list license:lgpl3+ license:ruby))))

(define-public ruby-packnga
  (package
    (name "ruby-packnga")
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "packnga" version))
              (sha256
               (base32
                "1vv2j0i43s4xid2km5hgrrxqlqpwgq8nlm8kaxfg2531c1vwfsd4"))))
    (build-system ruby-build-system)
    ;; ruby-test-unit is required to run tests, but that needs ruby-packnga.
    ;; To break the dependency cycle we disable tests.
    (arguments `(#:tests? #f))
    (propagated-inputs
     `(("ruby-gettext" ,ruby-gettext)
       ("ruby-yard" ,ruby-yard)))
    (native-inputs
     `(("bundler" ,bundler)))
    (synopsis "Utility library to package internationalized libraries")
    (description
     "Packnga is a library to translate to many languages using YARD.")
    (home-page "http://ranguba.org/packnga/")
    (license license:lgpl2.0+)))

(define-public ruby-test-unit
  (package
    (name "ruby-test-unit")
    (version "3.2.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "test-unit" version))
              (sha256
               (base32
                "05bx36fw01iqz0xqhvjfrwjgnj1zx3b2vn6w1fzp19rchd7zqc52"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-power-assert" ,ruby-power-assert)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-packnga" ,ruby-packnga)
       ("ruby-yard" ,ruby-yard)))
    (synopsis "Unit testing framework for Ruby")
    (description "@code{Test::Unit} is unit testing framework for Ruby, based
on xUnit principles.  These were originally designed by Kent Beck, creator of
extreme programming software development methodology, for Smalltalk's SUnit.
It allows writing tests, checking results and automated testing in Ruby.")
    (home-page "https://test-unit.github.io/")
    (license (list license:psfl license:ruby))))

(define-public ruby-metaclass
  (package
    (name "ruby-metaclass")
    (version "0.0.4")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "metaclass" version))
              (sha256
               (base32
                "0hp99y2b1nh0nr8pc398n3f8lakgci6pkrg4bf2b2211j1f6hsc5"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'add-test-unit-to-search-path
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((test-unit (assoc-ref inputs "ruby-test-unit"))
                    (test-unit-home (gem-home test-unit
                                              ,(package-version ruby))))
               (substitute* "Rakefile"
                 (("t\\.libs << \"test\"" line)
                  (string-append line "; t.libs << \""
                                 test-unit-home
                                 "/gems/test-unit-"
                                 ,(package-version ruby-test-unit)
                                 "/lib\""))))
             #t)))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-test-unit" ,ruby-test-unit)))
    (synopsis "Ruby library adding metaclass method to all objects")
    (description
     "Metaclass is a Ruby library adding a @code{metaclass} method to all Ruby
objects.")
    (home-page "https://github.com/floehopper/metaclass")
    (license license:expat)))

(define-public ruby-blankslate
  (package
    (name "ruby-blankslate")
    (version "3.1.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "blankslate" version))
              (sha256
               (base32
                "0fwkb4d1j9gc7vdwn2nxvwgy2g5wlag4c4bp7bl85jvq0kgp6cyx"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
          (lambda _ (zero? (system* "rspec" "spec/")))))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)))
    (synopsis "Abstract base class with no predefined methods")
    (description
     "BlankSlate provides an abstract base class with no predefined
methods (except for @code{__send__} and @code{__id__}).  BlankSlate is useful
as a base class when writing classes that depend upon
@code{method_missing} (e.g. dynamic proxies).")
    (home-page "https://github.com/masover/blankslate")
    (license license:expat)))

(define-public ruby-instantiator
  (package
    (name "ruby-instantiator")
    (version "0.0.6")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "instantiator" version))
              (sha256
               (base32
                "0mfmqhg9xrv9i8i1kmphf15ywddhivyh2z3ccl0xjw8qy54zr21i"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'add-test-unit-to-search-path
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((test-unit (assoc-ref inputs "ruby-test-unit"))
                    (test-unit-home (gem-home test-unit ,(package-version
                                                          ruby))))
               (substitute* "Rakefile"
                 (("t\\.libs << \"test\"" line)
                  (string-append line "; t.libs << \""
                                 test-unit-home
                                 "/gems/test-unit-"
                                 ,(package-version ruby-test-unit)
                                 "/lib\""))))
             #t)))))
    (propagated-inputs
     `(("ruby-blankslate" ,ruby-blankslate)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-test-unit" ,ruby-test-unit)))
    (synopsis "Instantiate an arbitrary Ruby class")
    (description
     "Instantiator lets you instantiate an arbitrary Ruby class without
knowing anything about the constructor.")
    (home-page "https://github.com/floehopper/instantiator")
    (license license:expat)))

(define-public ruby-introspection
  (package
    (name "ruby-introspection")
    (version "0.0.4")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "introspection" version))
              (sha256
               (base32
                "1y2nbijkc0zlfmn9ss6588ilarq2kbn2i7w7pwwsli66dj84zgca"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'add-test-unit-to-search-path
          (lambda* (#:key inputs #:allow-other-keys)
            (let* ((test-unit (assoc-ref inputs "ruby-test-unit"))
                   (test-unit-home (gem-home test-unit ,(package-version
                                                         ruby))))
              (substitute* "Rakefile"
                (("t\\.libs << \"test\"" line)
                 (string-append line "; t.libs << \""
                                test-unit-home
                                "/gems/test-unit-"
                                ,(package-version ruby-test-unit)
                                "/lib\""))))
            #t)))))
    (propagated-inputs
     `(("ruby-instantiator" ,ruby-instantiator)
       ("ruby-metaclass" ,ruby-metaclass)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-blankslate" ,ruby-blankslate)
       ("ruby-test-unit" ,ruby-test-unit)))
    (synopsis "Dynamic inspection of the method hierarchy on a Ruby object")
    (description
     "Introspection provides tools to inspect the hierarchy of method
definitions on a Ruby object.")
    (home-page "https://github.com/floehopper/introspection")
    (license license:expat)))

(define-public ruby-redcarpet
  (package
    (name "ruby-redcarpet")
    (version "3.4.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "redcarpet" version))
              (sha256
               (base32
                "0h9qz2hik4s9knpmbwrzb3jcp3vc5vygp9ya8lcpl7f1l9khmcd7"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; The gem archive does not include the conformance tests.
         (add-after 'unpack 'disable-conformance-tests
          (lambda _
            (substitute* "Rakefile"
              (("task :test => %w\\[test:unit test:conformance\\]")
               "task :test => %w[test:unit]"))
            #t)))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-test-unit" ,ruby-test-unit)
       ("ruby-rake-compiler" ,ruby-rake-compiler)))
    (synopsis "Extensible Markdown to (X)HTML converter")
    (description
     "Redcarpet is an extensible Ruby library for Markdown processing and
conversion to (X)HTML.")
    (home-page "https://github.com/vmg/redcarpet")
    (license license:expat)))

(define-public ruby-mocha
  (package
    (name "ruby-mocha")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "mocha" version))
              (sha256
               (base32
                "107nmnngbv8lq2g7hbjpn5kplb4v2c8gs9lxrg6vs8gdbddkilzi"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'add-test-unit-to-search-path
          (lambda* (#:key inputs #:allow-other-keys)
            (let* ((test-unit (assoc-ref inputs "ruby-test-unit"))
                   (test-unit-home (gem-home test-unit
                                             ,(package-version ruby))))
              (substitute* "Rakefile"
                (("t\\.libs << 'test'" line)
                 (string-append line "; t.libs << \""
                                test-unit-home
                                "/gems/test-unit-"
                                ,(package-version ruby-test-unit)
                                "/lib\""))))
            #t))
         (add-before 'check 'use-latest-redcarpet
          (lambda _
            (substitute* "mocha.gemspec"
              (("<redcarpet>.freeze, \\[\"~> 1\"\\]")
               "<redcarpet>.freeze, [\">= 3\"]"))
            #t))
         (add-before 'check 'hardcode-version
          (lambda _
            ;; Mocha is undefined at build time
            (substitute* "Rakefile"
              (("#\\{Mocha::VERSION\\}") ,version))
            #t))
         (add-before 'check 'remove-failing-test
          ;; FIXME: This test fails for reasons unrelated to Guix packaging.
          (lambda _
            (delete-file "test/acceptance/stubbing_nil_test.rb")
            #t)))))
    (propagated-inputs
     `(("ruby-metaclass" ,ruby-metaclass)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-yard" ,ruby-yard)
       ("ruby-introspection" ,ruby-introspection)
       ("ruby-test-unit" ,ruby-test-unit)
       ("ruby-redcarpet" ,ruby-redcarpet)))
    (synopsis "Mocking and stubbing library for Ruby")
    (description
     "Mocha is a mocking and stubbing library with JMock/SchMock syntax, which
allows mocking and stubbing of methods on real (non-mock) classes.")
    (home-page "http://gofreerange.com/mocha/docs")
    (license license:expat)))

(define-public ruby-net-ssh
  (package
    (name "ruby-net-ssh")
    (version "4.1.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "net-ssh" version))
              (sha256
               (base32
                "013p5jb4wy0cq7x7036piw2a3s1i9p752ki1srx2m289mpz4ml3q"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-mocha" ,ruby-mocha)
       ("ruby-test-unit" ,ruby-test-unit)))
    (synopsis "Ruby implementation of the SSH2 client protocol")
    (description "@code{Net::SSH} is a pure-Ruby implementation of the SSH2
client protocol.  It allows you to write programs that invoke and interact
with processes on remote servers, via SSH2.")
    (home-page "https://github.com/net-ssh/net-ssh")
    (license license:expat)))

(define-public ruby-minitest
  (package
    (name "ruby-minitest")
    (version "5.10.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "minitest" version))
              (sha256
               (base32
                "05521clw19lrksqgvg2kmm025pvdhdaniix52vmbychrn2jm7kz2"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("ruby-hoe" ,ruby-hoe)))
    (synopsis "Small test suite library for Ruby")
    (description "Minitest provides a complete suite of Ruby testing
facilities supporting TDD, BDD, mocking, and benchmarking.")
    (home-page "https://github.com/seattlerb/minitest")
    (license license:expat)))

;; This is the last release of Minitest 4, which is used by some packages.
(define-public ruby-minitest-4
  (package (inherit ruby-minitest)
    (version "4.7.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "minitest" version))
              (sha256
               (base32
                "03p6iban9gcpcflzp4z901s1hgj9369p6515h967ny6hlqhcf2iy"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-unsupported-method
          (lambda _
            (substitute* "Rakefile"
              (("self\\.rubyforge_name = .*") ""))
            #t))
         (add-after 'build 'exclude-failing-tests
           (lambda _
             ;; Some tests are failing on Ruby 2.4 due to the deprecation of
             ;; Fixnum.
             (delete-file "test/minitest/test_minitest_spec.rb")
             #t)))))))

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
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "minitest-bacon" version))
              (sha256
               (base32
                "0zhdwcl6bgha61qiyfvr7zs7ywaxc33wmj9xhxl8jdmpdvifvfaj"))))
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

(define-public ruby-minitest-focus
  (package
    (name "ruby-minitest-focus")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "minitest-focus" version))
       (sha256
        (base32
         "1zgjslp6d7dzcn8smj595idymgd5j603p9g2jqkfgi28sqbhz6m0"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-minitest" ,ruby-minitest)))
    (native-inputs
     `(("ruby-hoe" ,ruby-hoe)))
    (synopsis "Allows a few specific tests to be focused on")
    (description
     "@code{minitest-focus} gives the ability focus on a few tests with ease
without having to use command-line arguments.  It introduces a @code{focus}
class method for use in testing classes, specifying that the next defined test
is to be run.")
    (home-page "https://github.com/seattlerb/minitest-focus")
    (license license:expat)))

(define-public ruby-minitest-pretty-diff
  ;; Use git reference because gem is out of date and does not contain testing
  ;; script.  There are no releases on GitHub.
  (let ((commit "11f32e930f574225432f42e5e1ef6e7471efe572"))
    (package
      (name "ruby-minitest-pretty-diff")
      (version (string-append "0.1-1." (string-take commit 8)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/adammck/minitest-pretty_diff.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "13y5dhmcckhzd83gj1nfwh41iykbjcm2w7y4pr6j6rpqa5as122r"))))
      (build-system ruby-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (replace 'check
             (lambda _
               (zero? (system* "script/test")))))))
      (native-inputs
       `(("bundler" ,bundler)
         ("ruby-turn" ,ruby-turn)))
      (synopsis "Pretty-print hashes and arrays in MiniTest")
      (description
       "@code{minitest-pretty_diff} monkey-patches
@code{MiniTest::Assertions#mu_pp} to pretty-print hashes and arrays before
diffing them.  This makes it easier to spot differences between nested
structures when tests fail.")
      (home-page "https://github.com/adammck/minitest-pretty_diff")
      (license license:expat))))

(define-public ruby-minitest-moar
  (package
    (name "ruby-minitest-moar")
    (version "0.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "minitest-moar" version))
       (sha256
        (base32
         "0nb83blrsab92gcy6nfpw39njys7zisia8pw4igzzfzfl51cis0x"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'clean-dependencies
           (lambda _
             ;; Remove all gems defined in the Gemfile because these are not
             ;; truly needed.
             (substitute* "Gemfile"
               (("gem .*") ""))
             ;; Remove byebug as not needed to run tests.
             (substitute* "test/test_helper.rb"
               (("require 'byebug'") ""))
             #t)))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-minitest" ,ruby-minitest)))
    (synopsis "Extra features and changes to MiniTest")
    (description "@code{MiniTest Moar} add some additional features and
changes some default behaviours in MiniTest.  For instance, Moar replaces the
MiniTest @code{Object#stub} with a global @code{stub} method.")
    (home-page "https://github.com/dockyard/minitest-moar")
    (license license:expat)))

(define-public ruby-minitest-bonus-assertions
  (package
    (name "ruby-minitest-bonus-assertions")
    (version "3.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "minitest-bonus-assertions" version))
       (sha256
        (base32
         "1hbq9jk904xkz868yha1bqcm6azm7kmjsll2k4pn2nrcib508h2a"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'clean-dependencies
           (lambda _
             ;; Remove unneeded require statement that would entail another
             ;; dependency.
             (substitute* "test/minitest_config.rb"
               (("require 'minitest/bisect'") ""))
             #t)))))
    (native-inputs
     `(("ruby-hoe" ,ruby-hoe)
       ("ruby-minitest-pretty-diff" ,ruby-minitest-pretty-diff)
       ("ruby-minitest-focus" ,ruby-minitest-focus)
       ("ruby-minitest-moar" ,ruby-minitest-moar)))
    (synopsis "Bonus assertions for @code{Minitest}")
    (description
     "Minitest bonus assertions provides extra MiniTest assertions.  For
instance, it provides @code{assert_true}, @code{assert_false} and
@code{assert_set_equal}.")
    (home-page "https://github.com/halostatue/minitest-bonus-assertions")
    (license license:expat)))

(define-public ruby-minitest-rg
  (package
    (name "ruby-minitest-rg")
    (version "5.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "minitest-rg" version))
       (sha256
        (base32
         "0sq509ax1x62rd0w10b0hcydcxyk5bxxr3fwrgxv02r8drq2r354"))))
    (build-system ruby-build-system)
    (arguments
     ;; Some tests fail even outside Guix, so disable tests.
     ;; https://github.com/blowmage/minitest-rg/issues/12
     ;; https://github.com/blowmage/minitest-rg/pull/13
     `(#:tests? #f))
    (propagated-inputs
     `(("ruby-minitest" ,ruby-minitest)))
    (synopsis "Coloured output for Minitest")
    (description
     "@code{minitest-rg} changes the colour of the output from Minitest.")
    (home-page "http://blowmage.com/minitest-rg")
    (license license:expat)))

(define-public ruby-minitest-hooks
  (package
    (name "ruby-minitest-hooks")
    (version "1.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "minitest-hooks" version))
       (sha256
        (base32
         "05bngfyxwq1cflk568nhddgfrmws5ff6kiqax4skklsjnh71ykbi"))))
    (build-system ruby-build-system)
    (arguments
     '(#:test-target "spec"))
    (native-inputs
     `(("ruby-sequel" ,ruby-sequel)
       ("ruby-sqlite3" ,ruby-sqlite3)))
    (synopsis "Hooks for the minitest framework")
    (description
     "Minitest-hooks adds @code{around}, @code{before_all}, @code{after_all},
@code{around_all} hooks for Minitest.  This allows, for instance, running each
suite of specs inside a database transaction, running each spec inside its own
savepoint inside that transaction.  This can significantly speed up testing
for specs that share expensive database setup code.")
    (home-page "https://github.com/jeremyevans/minitest-hooks")
    (license license:expat)))

(define-public ruby-daemons
  (package
    (name "ruby-daemons")
    (version "1.2.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "daemons" version))
              (sha256
               (base32
                "15smbsg0gxb7nf0nrlnplc68y0cdy13dm6fviavpmw7c630sring"))))
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
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "git" version))
              (sha256
               (base32
                "1waikaggw7a1d24nw0sh8fd419gbf7awh000qhsf411valycj6q3"))))
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
    (version "4.5.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "slop" version))
              (sha256
               (base32
                "0bfm8535g0rkn9cbjndkckf0f7a3wj0rg4rqhrpsgxnbfdf2lm0p"))))
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

(define-public ruby-multi-json
  (package
    (name "ruby-multi-json")
    (version "1.12.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "multi_json" version))
       (sha256
        (base32
         "1raim9ddjh672m32psaa9niw67ywzjbxbdb8iijx3wv9k5b0pk2x"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ;; No testsuite included in the gem.
    (synopsis "Common interface to multiple JSON libraries for Ruby")
    (description
     "This package provides a common interface to multiple JSON libraries,
including Oj, Yajl, the JSON gem (with C-extensions), the pure-Ruby JSON gem,
NSJSONSerialization, gson.rb, JrJackson, and OkJson.")
    (home-page "http://github.com/intridea/multi_json")
    (license license:expat)))

(define-public ruby-arel
  (package
    (name "ruby-arel")
    (version "8.0.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "arel" version))
              (sha256
               (base32
                "0nw0qbc6ph625p6n3maqq9f527vz3nbl0hk72fbyka8jzsmplxzl"))))
    (build-system ruby-build-system)
    (arguments '(#:tests? #f)) ; no tests
    (home-page "https://github.com/rails/arel")
    (synopsis "SQL AST manager for Ruby")
    (description "Arel is an SQL @dfn{Abstract Syntax Tree} (AST) manager for
Ruby.  It simplifies the generation of complex SQL queries and adapts to
various relational database implementations.")
    (license license:expat)))

(define-public ruby-minitar
  ;; We package from the GitHub source to fix the security issue reported at
  ;; https://github.com/halostatue/minitar/issues/16.
  (let ((commit "e25205ecbb6277ae8a3df1e6a306d7ed4458b6e4"))
    (package
      (name "ruby-minitar")
      (version (string-append "0.5.4-1." (string-take commit 8)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/halostatue/minitar.git")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "1iywfx07jgjqcmixzkxk9zdwfmij1fyg1z2jlwzj15cj7s99qlfv"))))
      (build-system ruby-build-system)
      (arguments
       '(#:tests? #f)) ; missing a gemspec
      (synopsis "Ruby library and utility for handling tar archives")
      (description
       "Archive::Tar::Minitar is a pure-Ruby library and command-line utility
that provides the ability to deal with POSIX tar archive files.")
      (home-page "http://www.github.com/atoulme/minitar")
      (license (list license:gpl2+ license:ruby)))))

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
    (home-page "https://github.com/flavorjones/mini_portile")
    (license license:expat)))

(define-public ruby-mini-portile-2
  (package (inherit ruby-mini-portile)
    (version "2.2.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "mini_portile2" version))
              (sha256
               (base32
                "0g5bpgy08q0nc0anisg3yvwc1gc3inl854fcrg48wvg7glqd6dpm"))))))

(define-public ruby-nokogiri
  (package
    (name "ruby-nokogiri")
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "nokogiri" version))
              (sha256
               (base32
                "1nffsyx1xjg6v5n9rrbi8y1arrcx2i5f21cp6clgh9iwiqkr7rnn"))))
    (build-system ruby-build-system)
    (arguments
     ;; Tests fail because Nokogiri can only test with an installed extension,
     ;; and also because many test framework dependencies are missing.
     `(#:tests? #f
       #:gem-flags (list "--" "--use-system-libraries"
                         (string-append "--with-xml2-include="
                                        (assoc-ref %build-inputs "libxml2")
                                        "/include/libxml2" ))
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-extconf
           ;; 'pkg-config' is not included in the GEM_PATH during
           ;; installation, so we add it directly to the load path.
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((pkg-config (assoc-ref inputs "ruby-pkg-config"))
                    (pkg-config-home (gem-home pkg-config
                                               ,(package-version ruby))))
               (substitute* "ext/nokogiri/extconf.rb"
                 (("gem 'pkg-config'.*")
                  (string-append "$:.unshift '"
                                 pkg-config-home
                                 "/gems/pkg-config-"
                                 ,(package-version ruby-pkg-config)
                                 "/lib'\n"))))
             #t)))))
    (native-inputs
     `(("ruby-hoe" ,ruby-hoe)))
    (inputs
     `(("zlib" ,zlib)
       ("libxml2" ,libxml2)
       ("libxslt" ,libxslt)))
    (propagated-inputs
     `(("ruby-mini-portile" ,ruby-mini-portile-2)
       ("ruby-pkg-config" ,ruby-pkg-config)))
    (synopsis "HTML, XML, SAX, and Reader parser for Ruby")
    (description "Nokogiri (鋸) parses and searches XML/HTML, and features
both CSS3 selector and XPath 1.0 support.")
    (home-page "http://www.nokogiri.org/")
    (license license:expat)))

(define-public ruby-method-source
  (package
    (name "ruby-method-source")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "method_source" version))
       (sha256
        (base32
         "0xqj21j3vfq4ldia6i2akhn2qd84m0iqcnsl49kfpq3xk6x0dzgn"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "spec"))
    (native-inputs
     `(("ruby-rspec" ,ruby-rspec)
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
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "coderay" version))
       (sha256
        (base32
         "15vav4bhcc2x3jmi3izb11l4d9f3xv8hp2fszb7iqmpsccv1pz4y"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; missing test files
    (synopsis "Ruby syntax highlighting library")
    (description "Coderay is a Ruby library that provides syntax highlighting
for select languages.")
    (home-page "http://coderay.rubychan.de")
    (license license:expat)))

(define-public ruby-progress_bar
  (package
    (name "ruby-progress_bar")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "progress_bar" version))
       (sha256
        (base32
         "1qc40mr6p1z9a3vlpnsg1zfgk1qswviql2a31y63wpv3vr6b5f48"))))
    (build-system ruby-build-system)
    (arguments
     '(#:test-target "spec"))
    (propagated-inputs
     `(("ruby-highline" ,ruby-highline)
       ("ruby-options" ,ruby-options)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-timecop" ,ruby-timecop)))
    (synopsis
     "Ruby library for displaying progress bars")
    (description
     "ProgressBar is a simple library for displaying progress bars.  The
maximum value is configurable, and additional information can be displayed
like the percentage completion, estimated time remaining, elapsed time and
rate.")
    (home-page "https://github.com/paul/progress_bar")
    (license license:wtfpl2)))

(define-public ruby-pry
  (package
    (name "ruby-pry")
    (version "0.11.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "pry" version))
       (sha256
        (base32
         "1mh312k3y94sj0pi160wpia0ps8f4kmzvm505i6bvwynfdh7v30g"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; no tests
    (propagated-inputs
     `(("ruby-coderay" ,ruby-coderay)
       ("ruby-method-source" ,ruby-method-source)))
    (synopsis "Ruby REPL")
    (description "Pry is an IRB alternative and runtime developer console for
Ruby.  It features syntax highlighting, a plugin architecture, runtime
invocation, and source and documentation browsing.")
    (home-page "https://pryrepl.org")
    (license license:expat)))

(define-public ruby-guard
  (package
    (name "ruby-guard")
    (version "2.13.0")
    (source (origin
              (method url-fetch)
              ;; The gem does not include a Rakefile, nor does it contain a
              ;; gemspec file, nor does it come with the tests.  This is why
              ;; we fetch the tarball from Github.
              (uri (string-append "https://github.com/guard/guard/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1hwj0yi17k6f5axrm0k2bb7fq71dlp0zfywmd7pij9iimbppcca0"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f ; tests require cucumber
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-git-ls-files
          (lambda* (#:key outputs #:allow-other-keys)
            (substitute* "guard.gemspec"
              (("git ls-files -z") "find . -type f -print0"))
            #t))
         (replace 'build
          (lambda _
            (zero? (system* "gem" "build" "guard.gemspec")))))))
    (propagated-inputs
     `(("ruby-formatador" ,ruby-formatador)
       ("ruby-listen" ,ruby-listen)
       ("ruby-lumberjack" ,ruby-lumberjack)
       ("ruby-nenv" ,ruby-nenv)
       ("ruby-notiffany" ,ruby-notiffany)
       ("ruby-pry" ,ruby-pry)
       ("ruby-shellany" ,ruby-shellany)
       ("ruby-thor" ,ruby-thor)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)))
    (synopsis "Tool to handle events on file system modifications")
    (description
     "Guard is a command line tool to easily handle events on file system
modifications.  Guard automates various tasks by running custom rules whenever
file or directories are modified.")
    (home-page "http://guardgem.org/")
    (license license:expat)))

(define-public ruby-thread-safe
  (package
    (name "ruby-thread-safe")
    (version "0.3.6")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "thread_safe" version))
       (sha256
        (base32
         "0nmhcgq6cgz44srylra07bmaw99f5271l0dpsvl5f75m44l0gmwy"))))
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
    (version "1.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "tzinfo" version))
       (sha256
        (base32
         "09dpbrih054mn42flbbcdpzk2727mzfvjrgqb12zdafhx7p9rrzp"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-thread-safe" ,ruby-thread-safe)))
    (synopsis "Time zone library for Ruby")
    (description "TZInfo is a Ruby library that provides daylight savings
aware transformations between times in different time zones.")
    (home-page "https://tzinfo.github.io")
    (license license:expat)))

(define-public ruby-tzinfo-data
  (package
    (name "ruby-tzinfo-data")
    (version "1.2017.3")
    (source
     (origin
       (method url-fetch)
       ;; Download from GitHub because the rubygems version does not contain
       ;; Rakefile or tests.
       (uri (string-append
             "https://github.com/tzinfo/tzinfo-data/archive/v"
             version
             ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "01wff7syqzikbxalbg3isgxasmvzicr85bzadzkb6bf20bip4v54"))
       ;; Remove the known test failure.
       ;; https://github.com/tzinfo/tzinfo-data/issues/10
       ;; https://bugs.launchpad.net/ubuntu/+source/glibc/+bug/1587128
       (patches (search-patches
                 "ruby-tzinfo-data-ignore-broken-test.patch"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-tzinfo" ,ruby-tzinfo)))
    (synopsis "Data from the IANA Time Zone database")
    (description
     "This library provides @code{TZInfo::Data}, which contains data from the
IANA Time Zone database packaged as Ruby modules for use with @code{TZInfo}.")
    (home-page "https://tzinfo.github.io")
    (license license:expat)))

(define-public ruby-rb-inotify
  (package
    (name "ruby-rb-inotify")
    (version "0.9.10")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rb-inotify" version))
       (sha256
        (base32
         "0yfsgw5n7pkpyky6a9wkf1g9jafxb0ja7gz0qw0y14fd2jnzfh71"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f ; there are no tests
       #:phases
       (modify-phases %standard-phases
         ;; Building the gemspec with rake is not working here since it is
         ;; generated with Jeweler.  It is also unnecessary because the
         ;; existing gemspec does not use any development tools to generate a
         ;; list of files.
         (replace 'build
          (lambda _
            (zero? (system* "gem" "build" "rb-inotify.gemspec")))))))
    (propagated-inputs
     `(("ruby-ffi" ,ruby-ffi)))
    (native-inputs
     `(("ruby-yard" ,ruby-yard)))
    (synopsis "Ruby wrapper for Linux's inotify")
    (description "rb-inotify is a simple wrapper over the @code{inotify} Linux
kernel subsystem for monitoring changes to files and directories.")
    (home-page "https://github.com/nex3/rb-inotify")
    (license license:expat)))

(define-public ruby-pry-editline
  (package
    (name "ruby-pry-editline")
    (version "1.1.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "pry-editline" version))
              (sha256
               (base32
                "1pjxyvdxvw41xw3yyl18pwzix8hbvn6lgics7qcfhjfsf1zs8x1z"))))
    (build-system ruby-build-system)
    (arguments `(#:tests? #f)) ; no tests included
    (native-inputs
     `(("bundler" ,bundler)))
    (synopsis "Open the current REPL line in an editor")
    (description
     "This gem provides a plugin for the Ruby REPL to enable opening the
current line in an external editor.")
    (home-page "https://github.com/tpope/pry-editline")
    (license license:expat)))

(define-public ruby-sdoc
  (package
    (name "ruby-sdoc")
    (version "0.4.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "sdoc" version))
              (sha256
               (base32
                "0qhvy10vnmrqcgh8494m13kd5ag9c3sczzhfasv8j0294ylk679n"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-rubylib-and-patch-gemfile
          (lambda _
            (setenv "RUBYLIB" "lib")
            (substitute* "sdoc.gemspec"
              (("s.add_runtime_dependency.*") "\n")
              (("s.add_dependency.*") "\n"))
            (substitute* "Gemfile"
              (("gem \"rake\".*")
               "gem 'rake'\ngem 'rdoc'\ngem 'json'\n"))
            #t)))))
    (propagated-inputs
     `(("ruby-json" ,ruby-json)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-minitest" ,ruby-minitest)
       ("ruby-hoe" ,ruby-hoe)))
    (synopsis "Generate searchable RDoc documentation")
    (description
     "SDoc is an RDoc documentation generator to build searchable HTML
documentation for Ruby code.")
    (home-page "https://github.com/voloko/sdoc")
    (license license:expat)))

(define-public ruby-tins
  (package
    (name "ruby-tins")
    (version "1.15.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "tins" version))
              (sha256
               (base32
                "09whix5a7ics6787zrkwjmp16kqyh6560p9f317syks785805f7s"))))
    (build-system ruby-build-system)
    ;; This gem needs gem-hadar at development time, but gem-hadar needs tins
    ;; at runtime.  To avoid the dependency on gem-hadar we disable rebuilding
    ;; the gemspec.
    (arguments
     `(#:tests? #f ; there are no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'build
          (lambda _
            ;; "lib/spruz" is a symlink.  Leaving it in the gemspec file
            ;; causes an error.
            (substitute* "tins.gemspec"
              (("\"lib/spruz\", ") ""))
            (zero? (system* "gem" "build" "tins.gemspec")))))))
    (synopsis "Assorted tools for Ruby")
    (description "Tins is a Ruby library providing assorted tools.")
    (home-page "https://github.com/flori/tins")
    (license license:expat)))

(define-public ruby-gem-hadar
  (package
    (name "ruby-gem-hadar")
    (version "1.9.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "gem_hadar" version))
              (sha256
               (base32
                "1zxvd9l95rbks7x3cxn396w0sn7nha5542bf97v8akkn4vm7nby9"))))
    (build-system ruby-build-system)
    ;; This gem needs itself at development time. We disable rebuilding of the
    ;; gemspec to avoid this loop.
    (arguments
     `(#:tests? #f ; there are no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'build
          (lambda _
            (zero? (system* "gem" "build" "gem_hadar.gemspec")))))))
    (propagated-inputs
     `(("git" ,git)
       ("ruby-tins" ,ruby-tins)
       ("ruby-yard" ,ruby-yard)))
    (synopsis "Library for the development of Ruby gems")
    (description
     "This library contains some useful functionality to support the
development of Ruby gems.")
    (home-page "https://github.com/flori/gem_hadar")
    (license license:expat)))

(define-public ruby-minitest-tu-shim
  (package
    (name "ruby-minitest-tu-shim")
    (version "1.3.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "minitest_tu_shim" version))
              (sha256
               (base32
                "0xlyh94iirvssix157ng2akr9nqhdygdd0c6094hhv7dqcfrn9fn"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-test-include-path
          (lambda* (#:key inputs #:allow-other-keys)
             (let* ((minitest (assoc-ref inputs "ruby-minitest-4"))
                    (minitest-home (gem-home minitest
                                             ,(package-version ruby))))
               (substitute* "Rakefile"
                 (("Hoe\\.add_include_dirs .*")
                  (string-append "Hoe.add_include_dirs \""
                                 minitest-home
                                 "/gems/minitest-"
                                 ,(package-version ruby-minitest-4)
                                 "/lib" "\""))))
             #t))
         (add-before 'check 'fix-test-assumptions
          (lambda _
            ;; The test output includes the file name, so a couple of tests
            ;; fail.  Changing the regular expressions slightly fixes this
            ;; problem.
            (substitute* "test/test_mini_test.rb"
              (("output.sub!\\(.*, 'FILE:LINE'\\)")
               "output.sub!(/\\/.+-[\\w\\/\\.]+:\\d+/, 'FILE:LINE')")
              (("gsub\\(/.*, 'FILE:LINE'\\)")
               "gsub(/\\/.+-[\\w\\/\\.]+:\\d+/, 'FILE:LINE')"))
            #t)))))
    (propagated-inputs
     `(("ruby-minitest-4" ,ruby-minitest-4)))
    (native-inputs
     `(("ruby-hoe" ,ruby-hoe)))
    (synopsis "Adapter library between minitest and test/unit")
    (description
     "This library bridges the gap between the small and fast minitest and
Ruby's large and slower test/unit.")
    (home-page "https://rubygems.org/gems/minitest_tu_shim")
    (license license:expat)))

(define-public ruby-term-ansicolor
  (package
    (name "ruby-term-ansicolor")
    (version "1.6.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "term-ansicolor" version))
              (sha256
               (base32
                "1b1wq9ljh7v3qyxkk8vik2fqx2qzwh5lval5f92llmldkw7r7k7b"))))
    (build-system ruby-build-system)
    ;; Rebuilding the gemspec seems to require git, even though this is not a
    ;; git repository, so we just build the gem from the existing gemspec.
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'build
          (lambda _
            (zero? (system* "gem" "build" "term-ansicolor.gemspec")))))))
    (propagated-inputs
     `(("ruby-tins" ,ruby-tins)))
    (native-inputs
     `(("ruby-gem-hadar" ,ruby-gem-hadar)
       ("ruby-minitest-tu-shim" ,ruby-minitest-tu-shim)))
    (synopsis "Ruby library to control the attributes of terminal output")
    (description
     "This Ruby library uses ANSI escape sequences to control the attributes
of terminal output.")
    (home-page "https://flori.github.io/term-ansicolor/")
    ;; There is no mention of the "or later" clause.
    (license license:gpl2)))

(define-public ruby-pstree
  (package
    (name "ruby-pstree")
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "pstree" version))
              (sha256
               (base32
                "1mig1sv5qx1cdyhjaipy8jlh9j8pnja04vprrzihyfr54x0215p1"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("ruby-gem-hadar" ,ruby-gem-hadar)
       ("bundler" ,bundler)))
    (synopsis "Create a process tree data structure")
    (description
     "This library uses the output of the @code{ps} command to create a
process tree data structure for the current host.")
    (home-page "https://github.com/flori/pstree")
    ;; There is no mention of the "or later" clause.
    (license license:gpl2)))

(define-public ruby-utils
  (package
    (name "ruby-utils")
    (version "0.9.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "utils" version))
              (sha256
               (base32
                "196zhgcygrnx09bb9mh22qas03rl9avzx8qs0wnxznpin4pffwcl"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-tins" ,ruby-tins)
       ("ruby-term-ansicolor" ,ruby-term-ansicolor)
       ("ruby-pstree" ,ruby-pstree)
       ("ruby-pry-editline" ,ruby-pry-editline)))
    (native-inputs
     `(("ruby-gem-hadar" ,ruby-gem-hadar)
       ("bundler" ,bundler)))
    (synopsis "Command line tools for working with Ruby")
    (description
     "This package provides assorted command line tools that may be useful
when working with Ruby code.")
    (home-page "https://github.com/flori/utils")
    ;; There is no mention of the "or later" clause.
    (license license:gpl2)))

(define-public ruby-json
  (package
    (name "ruby-json")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "json" version))
       (sha256
        (base32
         "01v6jjpvh3gnq6sgllpfqahlgxzj50ailwhj9b3cd20hi2dx0vxp"))))
    (build-system ruby-build-system)
    (arguments '(#:tests? #f)) ; dependency cycle with sdoc
    (synopsis "JSON library for Ruby")
    (description "This Ruby library provides a JSON implementation written as
a native C extension.")
    (home-page "http://json-jruby.rubyforge.org/")
    (license (list license:ruby license:gpl2)))) ; GPL2 only

(define-public ruby-json-pure
  (package
    (name "ruby-json-pure")
    (version "2.1.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "json_pure" version))
              (sha256
               (base32
                "12yf9fmhr4c2jm3xl20vf1qyz5i63vc8a6ngz9j0f86nqwhmi2as"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-rakefile
           (lambda _
             (substitute* "Rakefile"
               ;; Since this is not a git repository, do not call 'git'.
               (("`git ls-files`") "`find . -type f |sort`")
               ;; Loosen dependency constraint.
               (("'test-unit', '~> 2.0'") "'test-unit', '>= 2.0'"))
             #t))
         (add-after 'replace-git-ls-files 'regenerate-gemspec
           (lambda _
             ;; Regenerate gemspec so loosened dependency constraints are
             ;; propagated.
             (zero? (system* "rake" "gemspec")))))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ragel" ,ragel)
       ("ruby-simplecov" ,ruby-simplecov)
       ("ruby-test-unit" ,ruby-test-unit)))
    (synopsis "JSON implementation in pure Ruby")
    (description
     "This package provides a JSON implementation written in pure Ruby.")
    (home-page "https://flori.github.com/json")
    (license license:ruby)))

;; Even though this package only provides bindings for a Mac OSX API it is
;; required by "ruby-listen" at runtime.
(define-public ruby-rb-fsevent
  (package
    (name "ruby-rb-fsevent")
    (version "0.10.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rb-fsevent" version))
              (sha256
               (base32
                "1fbpmjypwxkb8r7y1kmhmyp6gawa4byw0yb3jc3dn9ly4ld9lizf"))))
    (build-system ruby-build-system)
    ;; Tests need "guard-rspec", which needs "guard".  However, "guard" needs
    ;; "listen", which needs "rb-fsevent" at runtime.
    (arguments `(#:tests? #f))
    (synopsis "FSEvents API with signals catching")
    (description
     "This library provides Ruby bindings for the Mac OSX FSEvents API.")
    (home-page "https://rubygems.org/gems/rb-fsevent")
    (license license:expat)))

(define-public ruby-listen
  (package
    (name "ruby-listen")
    (version "3.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "listen" version))
       (sha256
        (base32
         "01v5mrnfqm6sgm8xn2v5swxsn1wlmq7rzh2i48d4jzjsc7qvb6mx"))))
    (build-system ruby-build-system)
    (arguments '(#:tests? #f)) ; no tests
    (propagated-inputs
     `(("ruby-rb-inotify" ,ruby-rb-inotify)
       ("ruby-rb-fsevent" ,ruby-rb-fsevent)))
    (synopsis "Listen to file modifications")
    (description "The Listen gem listens to file modifications and notifies
you about the changes.")
    (home-page "https://github.com/guard/listen")
    (license license:expat)))

(define-public ruby-activesupport
  (package
    (name "ruby-activesupport")
    (version "5.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "activesupport" version))
       (sha256
        (base32
         "0sgf4rsfr7jcaqsx0wwzx4l4k9xsjlwv0mzl08pxiyp1qzyx8scr"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             ;; There is no tests, instead attempt to load the library.
             (zero? (system* "ruby" "-Ilib" "-r" "active_support")))))))
    (propagated-inputs
     `(("ruby-concurrent" ,ruby-concurrent)
       ("ruby-i18n" ,ruby-i18n)
       ("ruby-minitest" ,ruby-minitest)
       ("ruby-tzinfo" ,ruby-tzinfo)
       ("ruby-tzinfo-data" ,ruby-tzinfo-data)))
    (synopsis "Ruby on Rails utility library")
    (description "ActiveSupport is a toolkit of support libraries and Ruby
core extensions extracted from the Rails framework.  It includes support for
multibyte strings, internationalization, time zones, and testing.")
    (home-page "http://www.rubyonrails.org")
    (license license:expat)))

(define-public ruby-crass
  (package
    (name "ruby-crass")
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "crass" version))
              (sha256
               (base32
                "1czijxlagzpzwchr2ldrgfi7kywg08idjpq37ndcmwh4fmz72c4l"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-minitest" ,ruby-minitest)))
    (synopsis "Pure Ruby CSS parser")
    (description
     "Crass is a pure Ruby CSS parser based on the CSS Syntax Level 3 spec.")
    (home-page "https://github.com/rgrove/crass/")
    (license license:expat)))

(define-public ruby-nokogumbo
  (let ((commit "fb51ff299a1c34346837580b6d1d9a60fadf5dbd"))
    (package
      (name "ruby-nokogumbo")
      (version (string-append "1.4.7-1." (string-take commit 8)))
      (source (origin
                ;; We use the git reference, because there's no Rakefile in the
                ;; published gem and the tarball on Github is outdated.
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/rubys/nokogumbo.git")
                      (commit "d56f954d20a")))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "0bnppjy96xiadrsrc9dp8y6wvdwnkfa930n7acrp0mqm4qywl2wl"))))
      (build-system ruby-build-system)
      (arguments
       `(#:modules ((guix build ruby-build-system)
                    (guix build utils)
                    (ice-9 rdelim))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'build-gemspec
            (lambda _
              (substitute* "Rakefile"
                ;; Build Makefile even without a copy of gumbo-parser sources
                (("'gumbo-parser/src',") "")
                ;; We don't bundle gumbo-parser sources
                (("'gumbo-parser/src/\\*',") "")
                (("'gumbo-parser/visualc/include/\\*',") "")
                ;; The definition of SOURCES will be cut in gemspec, and
                ;; "FileList" will be undefined.
                (("SOURCES \\+ FileList\\[")
                 "['ext/nokogumboc/extconf.rb', 'ext/nokogumboc/nokogumbo.c', "))

              ;; Copy the Rakefile and cut out the gemspec.
              (copy-file "Rakefile" ".gemspec")
              (with-atomic-file-replacement ".gemspec"
                (lambda (in out)
                  (let loop ((line (read-line in 'concat))
                             (skipping? #t))
                    (if (eof-object? line)
                        #t
                        (let ((skip-next? (if skipping?
                                              (not (string-prefix? "SPEC =" line))
                                              (string-prefix? "end" line))))
                          (when (or (not skipping?)
                                    (and skipping? (not skip-next?)))
                                (format #t "~a" line)
                                (display line out))
                          (loop (read-line in 'concat) skip-next?))))))
              #t)))))
      (inputs
       `(("gumbo-parser" ,gumbo-parser)))
      (propagated-inputs
       `(("ruby-nokogiri" ,ruby-nokogiri)))
      (synopsis "Ruby bindings to the Gumbo HTML5 parser")
      (description
       "Nokogumbo allows a Ruby program to invoke the Gumbo HTML5 parser and
access the result as a Nokogiri parsed document.")
      (home-page "https://github.com/rubys/nokogumbo/")
      (license license:asl2.0))))

(define-public ruby-sanitize
  (package
    (name "ruby-sanitize")
    (version "4.0.0")
    (source (origin
              (method url-fetch)
              ;; The gem does not include the Rakefile, so we download the
              ;; release tarball from Github.
              (uri (string-append "https://github.com/rgrove/"
                                  "sanitize/archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "055xnj38l60gxnnng76kpy2l2jbrp0byjdyq17jw79w7l4b40znr"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-crass" ,ruby-crass)
       ("ruby-nokogiri" ,ruby-nokogiri)
       ("ruby-nokogumbo" ,ruby-nokogumbo)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-minitest" ,ruby-minitest)
       ("ruby-redcarpet" ,ruby-redcarpet)
       ("ruby-yard" ,ruby-yard)))
    (synopsis "Whitelist-based HTML and CSS sanitizer")
    (description
     "Sanitize is a whitelist-based HTML and CSS sanitizer.  Given a list of
acceptable elements, attributes, and CSS properties, Sanitize will remove all
unacceptable HTML and/or CSS from a string.")
    (home-page "https://github.com/rgrove/sanitize/")
    (license license:expat)))

(define-public ruby-ox
  (package
    (name "ruby-ox")
    (version "2.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "ox" version))
       (sha256
        (base32
         "0fmk62b1h2i79dfzjj8wmf8qid1rv5nhwfc17l489ywnga91xl83"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; no tests
    (synopsis "Optimized XML library for Ruby")
    (description
     "Optimized XML (Ox) is a fast XML parser and object serializer for Ruby
written as a native C extension.  It was designed to be an alternative to
Nokogiri and other Ruby XML parsers for generic XML parsing and as an
alternative to Marshal for Object serialization. ")
    (home-page "http://www.ohler.com/ox")
    (license license:expat)))

(define-public ruby-redcloth
  (package
    (name "ruby-redcloth")
    (version "4.3.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "RedCloth" version))
              (sha256
               (base32
                "0m9dv7ya9q93r8x1pg2gi15rxlbck8m178j1fz7r5v6wr1avrrqy"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         ;; Redcloth has complicated rake tasks to build various versions for
         ;; multiple targets using RVM.  We don't want this so we just use the
         ;; existing gemspec.
         (replace 'build
          (lambda _
            (zero? (system* "gem" "build" "redcloth.gemspec"))))
         ;; Make sure that the "redcloth" executable finds required Ruby
         ;; libraries.
         (add-after 'install 'wrap-bin-redcloth
          (lambda* (#:key outputs #:allow-other-keys)
            (wrap-program (string-append (assoc-ref outputs "out")
                                         "/bin/redcloth")
              `("GEM_HOME" ":" prefix (,(getenv "GEM_HOME"))))
            #t)))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-diff-lcs" ,ruby-diff-lcs)
       ("ruby-rspec-2" ,ruby-rspec-2)))
    (synopsis "Textile markup language parser for Ruby")
    (description
     "RedCloth is a Ruby parser for the Textile markup language.")
    (home-page "http://redcloth.org")
    (license license:expat)))

(define-public ruby-pg
  (package
    (name "ruby-pg")
    (version "0.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "pg" version))
       (sha256
        (base32
         "00vhasqwc4f98qb4wxqn2h07fjwzhp5lwyi41j2gndi2g02wrdqh"))))
    (build-system ruby-build-system)
    (arguments
     '(#:test-target "spec"))
    (native-inputs
     `(("ruby-rake-compiler" ,ruby-rake-compiler)
       ("ruby-hoe" ,ruby-hoe)
       ("ruby-rspec" ,ruby-rspec)))
    (inputs
     `(("postgresql" ,postgresql)))
    (synopsis "Ruby interface to PostgreSQL")
    (description "Pg is the Ruby interface to the PostgreSQL RDBMS.  It works
with PostgreSQL 8.4 and later.")
    (home-page "https://bitbucket.org/ged/ruby-pg")
    (license license:ruby)))

(define-public ruby-byebug
  (package
    (name "ruby-byebug")
    (version "9.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "byebug" version))
       (sha256
        (base32
         "1kbfcn65rgdhi72n8x9l393b89rvi5z542459k7d1ggchpb0idb0"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; no tests
    (synopsis "Debugger for Ruby 2")
    (description "Byebug is a Ruby 2 debugger implemented using the Ruby 2
TracePoint C API for execution control and the Debug Inspector C API for call
stack navigation.  The core component provides support that front-ends can
build on.  It provides breakpoint handling and bindings for stack frames among
other things and it comes with a command line interface.")
    (home-page "https://github.com/deivid-rodriguez/byebug")
    (license license:bsd-2)))

(define-public ruby-netrc
  (package
    (name "ruby-netrc")
    (version "0.11.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "netrc" version))
              (sha256
               (base32
                "0gzfmcywp1da8nzfqsql2zqi648mfnx6qwkig3cv36n9m0yy676y"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           ;; There is no Rakefile and minitest can only run one file at once,
           ;; so we have to iterate over all test files.
           (lambda _
             (and (map (lambda (file)
                         (zero? (system* "ruby" "-Itest" file)))
                       (find-files "./test" "test_.*\\.rb"))))))))
    (native-inputs
     `(("ruby-minitest" ,ruby-minitest)))
    (synopsis "Library to read and update netrc files")
    (description
     "This library can read and update netrc files, preserving formatting
including comments and whitespace.")
    (home-page "https://github.com/geemus/netrc")
    (license license:expat)))

(define-public ruby-unf-ext
  (package
    (name "ruby-unf-ext")
    (version "0.0.7.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "unf_ext" version))
              (sha256
               (base32
                "0ly2ms6c3irmbr1575ldyh52bz2v0lzzr2gagf0p526k12ld2n5b"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-ext
           (lambda _ (zero? (system* "rake" "compile:unf_ext")))))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rake-compiler" ,ruby-rake-compiler)
       ("ruby-test-unit" ,ruby-test-unit)))
    (synopsis "Unicode normalization form support library")
    (description
     "This package provides unicode normalization form support for Ruby.")
    (home-page "https://github.com/knu/ruby-unf_ext")
    (license license:expat)))

(define-public ruby-tdiff
  ;; Use a newer than released snapshot so that rspec-2 is not required.
  (let ((commit "b662a6048f08abc45c1a834e5f34dd1c662935e2"))
    (package
      (name "ruby-tdiff")
      (version (string-append "0.3.3-1." (string-take commit 8)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/postmodern/tdiff.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "0n3gq8rx49f7ln6zqlshqfg2mgqyy30rsdjlnki5mv307ykc7ad4"))))
      (build-system ruby-build-system)
      (native-inputs
       `(("ruby-rspec" ,ruby-rspec)
         ("ruby-yard" ,ruby-yard)
         ("ruby-rubygems-tasks" ,ruby-rubygems-tasks)))
      (synopsis "Calculate the differences between two tree-like structures")
      (description
       "This library provides functions to calculate the differences between two
tree-like structures.  It is similar to Ruby's built-in @code{TSort} module.")
      (home-page "https://github.com/postmodern/tdiff")
      (license license:expat))))

(define-public ruby-nokogiri-diff
  ;; Use a newer than released snapshot so that rspec-2 is not required.
  (let ((commit "a38491e4d8709b7406f2cae11a50226d927d06f5"))
    (package
      (name "ruby-nokogiri-diff")
      (version (string-append "0.2.0-1." (string-take commit 8)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/postmodern/nokogiri-diff.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1ah2sfjh9n1p0ln2wkqzfl448ml7j4zfy6dhp1qgzq2m41php6rf"))))
      (build-system ruby-build-system)
      (propagated-inputs
       `(("ruby-tdiff" ,ruby-tdiff)
         ("ruby-nokogiri" ,ruby-nokogiri)))
      (native-inputs
       `(("ruby-rspec" ,ruby-rspec)
         ("ruby-yard" ,ruby-yard)
         ("ruby-rubygems-tasks" ,ruby-rubygems-tasks)))
      (synopsis "Calculate the differences between two XML/HTML documents")
      (description
       "@code{Nokogiri::Diff} adds the ability to calculate the
differences (added or removed nodes) between two XML/HTML documents.")
      (home-page "https://github.com/postmodern/nokogiri-diff")
      (license license:expat))))

(define-public ruby-rack
  (package
    (name "ruby-rack")
    (version "2.0.3")
    (source
     (origin
       (method url-fetch)
       ;; Download from GitHub so that the patch can be applied.
       (uri (string-append
             "https://github.com/rack/rack/archive/"
             version
             ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "12bnqrcg43x9hsswjqg31qqwk8cwj2fh0d2m179y20bjghhn54kx"))
       ;; Ignore test which fails inside the build environment but works
       ;; outside.
       (patches (search-patches "ruby-rack-ignore-failing-test.patch"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-tests
           (lambda _
             ;; A few of the tests use the length of a file on disk for
             ;; Content-Length and Content-Range headers.  However, this file
             ;; has a shebang in it which an earlier phase patches, growing
             ;; the file size from 193 to 239 bytes when the store prefix is
             ;; "/gnu/store".
             (let ((size-diff (- (string-length (which "ruby"))
                                 (string-length "/usr/bin/env ruby"))))
               (substitute* '("test/spec_file.rb")
                 (("193")
                  (number->string (+ 193 size-diff)))
                 (("bytes(.)22-33" all delimiter)
                  (string-append "bytes"
                                 delimiter
                                 (number->string (+ 22 size-diff))
                                 "-"
                                 (number->string (+ 33 size-diff))))))
             #t)))))
    (native-inputs
     `(("ruby-minitest" ,ruby-minitest)
       ("ruby-minitest-sprint" ,ruby-minitest-sprint)
       ("which" ,which)))
    (propagated-inputs
     `(("ruby-concurrent" ,ruby-concurrent)))
    (synopsis "Unified web application interface for Ruby")
    (description "Rack provides a minimal, modular and adaptable interface for
developing web applications in Ruby.  By wrapping HTTP requests and responses,
it unifies the API for web servers, web frameworks, and software in between
into a single method call.")
    (home-page "https://rack.github.io/")
    (license license:expat)))

(define-public ruby-docile
  (package
    (name "ruby-docile")
    (version "1.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "docile" version))
       (sha256
        (base32
         "0m8j31whq7bm5ljgmsrlfkiqvacrw6iz9wq10r3gwrv5785y8gjx"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; needs github-markup, among others
    (synopsis "Ruby EDSL helper library")
    (description "Docile is a Ruby library that provides an interface for
creating embedded domain specific languages (EDSLs) that manipulate existing
Ruby classes.")
    (home-page "https://ms-ati.github.io/docile/")
    (license license:expat)))

(define-public ruby-gherkin
  (package
    (name "ruby-gherkin")
    (version "4.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "gherkin" version))
        (sha256
          (base32
            "1d18r8mf2qyd9jbq9xxvca8adyysdzvwdy8v9c2s5hrd6p02kg79"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("bundler" ,bundler)))
    (arguments
     '(#:tests? #f)) ; needs simplecov, among others
    (synopsis "Gherkin parser for Ruby")
    (description "Gherkin is a parser and compiler for the Gherkin language.
It is intended be used by all Cucumber implementations to parse '.feature'
files.")
    (home-page "https://github.com/cucumber/gherkin3")
    (license license:expat)))

(define-public ruby-cucumber-core
  (package
    (name "ruby-cucumber-core")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "cucumber-core" version))
       (sha256
        (base32
         "136hnvqv444qyxzcgy1k60y4i6cn3sn9lbqr4wan9dzz1yzllqbm"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-gherkin" ,ruby-gherkin)))
    (native-inputs
     `(("bundler" ,bundler)))
    (arguments
     '(#:tests? #f)) ; needs simplecov, among others
    (synopsis "Core library for the Cucumber BDD app")
    (description "Cucumber is a tool for running automated tests
written in plain language.  Because they're written in plain language,
they can be read by anyone on your team.  Because they can be read by
anyone, you can use them to help improve communication, collaboration
and trust on your team.")
    (home-page "https://cucumber.io/")
    (license license:expat)))

(define-public ruby-bio-logger
  (package
    (name "ruby-bio-logger")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "bio-logger" version))
       (sha256
        (base32
         "02pylfy8nkdqzyzplvnhn1crzmfkj1zmi3qjhrj2f2imlxvycd28"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f)) ; rake errors, missing shoulda
    (propagated-inputs
     `(("ruby-log4r" ,ruby-log4r)))
    (synopsis "Log4r wrapper for Ruby")
    (description "Bio-logger is a wrapper around Log4r adding extra logging
features such as filtering and fine grained logging.")
    (home-page "https://github.com/pjotrp/bioruby-logger-plugin")
    (license license:expat)))

(define-public ruby-yard
  (package
    (name "ruby-yard")
    (version "0.9.6")
    (source
     (origin
       (method url-fetch)
       ;; Tests do not pass if we build from the distributed gem.
       (uri (string-append "https://github.com/lsegal/yard/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0rsz4bghgx7fryzyhlz8wlnd2m9xgyvf1xhrq58mnzfrrfm41bdg"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             ;; $HOME needs to be set to somewhere writeable for tests to run
             (setenv "HOME" "/tmp")
             ;; Run tests without using 'rake' to avoid dependencies.
             (zero? (system* "rspec")))))))
    (native-inputs
     `(("ruby-rspec" ,ruby-rspec)
       ("ruby-rack" ,ruby-rack)))
    (synopsis "Documentation generation tool for Ruby")
    (description
     "YARD is a documentation generation tool for the Ruby programming
language.  It enables the user to generate consistent, usable documentation
that can be exported to a number of formats very easily, and also supports
extending for custom Ruby constructs such as custom class level definitions.")
    (home-page "https://yardoc.org")
    (license license:expat)))

(define-public ruby-clap
  (package
    (name "ruby-clap")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "clap" version))
              (sha256
               (base32
                "190m05k3pca72c1h8k0fnvby15m303zi0lpb9c478ad19wqawa5q"))))
    (build-system ruby-build-system)
    ;; Clap needs cutest for running tests, but cutest needs clap.
    (arguments `(#:tests? #f))
    (synopsis "Command line argument parsing for simple applications")
    (description
     "Clap provides command line argument parsing features.  It covers the
simple case of executing code based on the flags or parameters passed.")
    (home-page "https://github.com/djanowski/cutest")
    (license license:expat)))

(define-public ruby-cutest
  (package
    (name "ruby-cutest")
    (version "1.2.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "cutest" version))
              (sha256
               (base32
                "1mldhjn62g53vx4gq2qdqg2lgjvyrqxa8d0khf8347bbfgi16d32"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-clap" ,ruby-clap)))
    (synopsis "Run tests in separate processes")
    (description
     "Cutest runs tests in separate processes to avoid shared state.")
    (home-page "https://github.com/djanowski/cutest")
    (license license:expat)))

(define-public ruby-pygmentize
  (package
    (name "ruby-pygmentize")
    (version "0.0.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "pygmentize" version))
              (sha256
               (base32
                "1pxryhkiwvsz6xzda3bvqwz5z8ggzl1cdglf8qbcf4bb7akirdpb"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-pygmentize-path
          (lambda _
            (substitute* "lib/pygmentize.rb"
              (("\"/usr/bin/env python.*")
               (string-append "\"" (which "pygmentize") "\"\n")))
            #t))
         (add-after 'build 'do-not-use-vendor-directory
          (lambda _
            ;; Remove bundled pygments sources
            ;; FIXME: ruby-build-system does not support snippets.
            (delete-file-recursively "vendor")
            (substitute* "pygmentize.gemspec"
              (("\"vendor/\\*\\*/\\*\",") ""))
            #t)))))
    (inputs
     `(("pygments" ,python-pygments)))
    (native-inputs
     `(("ruby-cutest" ,ruby-cutest)
       ("ruby-nokogiri" ,ruby-nokogiri)))
    (synopsis "Thin Ruby wrapper around pygmentize")
    (description
     "Pygmentize provides a simple way to call pygmentize from within a Ruby
application.")
    (home-page "https://github.com/djanowski/pygmentize")
    (license license:expat)))

(define-public ruby-eventmachine
  (package
    (name "ruby-eventmachine")
    (version "1.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "eventmachine" version))
       (sha256
        (base32
         "075hdw0fgzldgss3xaqm2dk545736khcvv1fmzbf1sgdlkyh1v8z"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; test suite tries to connect to google.com
    (native-inputs
     `(("ruby-rake-compiler" ,ruby-rake-compiler)))
    (synopsis "Single-threaded network event framework for Ruby")
    (description
     "EventMachine implements a single-threaded engine for arbitrary network
communications.  EventMachine wraps all interactions with sockets, allowing
programs to concentrate on the implementation of network protocols.  It can be
used to create both network servers and clients.")
    (home-page "http://rubyeventmachine.com")
    (license (list license:ruby license:gpl3)))) ; GPLv3 only AFAICT

(define-public ruby-turn
  (package
    (name "ruby-turn")
    (version "0.9.7")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "turn" version))
       (sha256
        (base32
         "1691rc2sq04cw8mxxh340k2j04ll90kwgcy8ddrp6rligmfrf8fw"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Tests fail because turn changes its environment so can no longer
         ;; find test/unit.  Instead simply test if the executable runs
         ;; without issue.
         (replace 'check
           (lambda _
             (zero? (system* "ruby" "-Ilib" "bin/turn" "-h")))))))
    (propagated-inputs
     `(("ruby-ansi" ,ruby-ansi)
       ("ruby-minitest" ,ruby-minitest-4)))
    (synopsis "Alternate set of alternative runners for MiniTest")
    (description
     "TURN provides a set of alternative runners for MiniTest which are both
colorful and informative.  TURN displays each test on a separate line with
failures being displayed immediately instead of at the end of the tests.  Note
that TURN is no longer being maintained.")
    (home-page "http://rubygems.org/gems/turn")
    (license license:expat)))

(define-public ruby-mime-types-data
  (package
    (name "ruby-mime-types-data")
    (version "3.2016.0521")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "mime-types-data" version))
       (sha256
        (base32
         "04my3746hwa4yvbx1ranhfaqkgf6vavi1kyijjnw8w3dy37vqhkm"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("ruby-hoe" ,ruby-hoe)))
    (synopsis "Registry for information about MIME media type definitions")
    (description
     "@code{mime-types-data} provides a registry for information about
Multipurpose Internet Mail Extensions (MIME) media type definitions.  It can
be used with the Ruby mime-types library or other software to determine
defined filename extensions for MIME types, or to use filename extensions to
look up the likely MIME type definitions.")
    (home-page "https://github.com/mime-types/mime-types-data/")
    (license license:expat)))

(define-public ruby-mime-types
  (package
    (name "ruby-mime-types")
    (version "3.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "mime-types" version))
       (sha256
        (base32
         "0087z9kbnlqhci7fxh9f6il63hj1k02icq2rs0c6cppmqchr753m"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-mime-types-data" ,ruby-mime-types-data)))
    (native-inputs
     `(("ruby-hoe" ,ruby-hoe)
       ("ruby-fivemat" ,ruby-fivemat)
       ("ruby-minitest-focus" ,ruby-minitest-focus)
       ("ruby-minitest-rg" ,ruby-minitest-rg)
       ("ruby-minitest-bonus-assertions" ,ruby-minitest-bonus-assertions)
       ("ruby-minitest-hooks" ,ruby-minitest-hooks)))
    (synopsis "Library and registry for MIME content type definitions")
    (description "The mime-types library provides a library and registry for
information about Multipurpose Internet Mail Extensions (MIME) content type
definitions.  It can be used to determine defined filename extensions for MIME
types, or to use filename extensions to look up the likely MIME type
definitions.")
    (home-page "https://github.com/mime-types/ruby-mime-types")
    (license license:expat)))

(define-public ruby-fivemat
  (package
    (name "ruby-fivemat")
    (version "1.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "fivemat" version))
       (sha256
        (base32
         "0ij7n250gk5c1g34rsbwjnpcv64gk4vsas8lkz8fac4wbygvk6z1"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f)) ; no tests
    (synopsis "Each test file given its own line of dots")
    (description
     "Fivemat is a MiniTest/RSpec/Cucumber formatter that gives each test file
its own line of dots during testing.  It aims to provide test output that is
neither too verbose nor too minimal.")
    (home-page "https://github.com/tpope/fivemat")
    (license license:expat)))

(define-public ruby-sqlite3
  (package
    (name "ruby-sqlite3")
    (version "1.3.13")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "sqlite3" version))
       (sha256
        (base32
         "01ifzp8nwzqppda419c9wcvr8n82ysmisrs0hph9pdmv1lpa4f5i"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'add-gemtest-file
           ;; This file exists in the repository but is not distributed.
           (lambda _ (zero? (system* "touch" ".gemtest")))))))
    (inputs
     `(("sqlite" ,sqlite)))
    (native-inputs
     `(("ruby-hoe" ,ruby-hoe)
       ("ruby-rake-compiler" ,ruby-rake-compiler)
       ("ruby-mini-portile" ,ruby-mini-portile)))
    (synopsis "Interface with SQLite3 databases")
    (description
     "This module allows Ruby programs to interface with the SQLite3 database
engine.")
    (home-page
     "https://github.com/sparklemotion/sqlite3-ruby")
    (license license:bsd-3)))

(define-public ruby-shoulda-context
  (package
    (name "ruby-shoulda-context")
    (version "1.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "shoulda-context" version))
       (sha256
        (base32
         "1l0ncsxycb4s8n47dml97kdnixw4mizljbkwqc3rh05r70csq9bc"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             ;; Do not run tests to avoid circular dependence with rails.
             ;; Instead just import the library to test.
             (zero? (system* "ruby" "-Ilib" "-r" "shoulda-context")))))))
    (synopsis "Test::Unit context framework extracted from Shoulda")
    (description
     "@code{shoulda-context} is the context framework extracted from Shoulda.
Instead of writing Ruby methods with lots_of_underscores, shoulda-context adds
context, setup, and should blocks combine to produce natural test method
names.")
    (home-page "https://github.com/thoughtbot/shoulda-context")
    (license license:expat)))

(define-public ruby-shoulda-matchers
  (package
    (name "ruby-shoulda-matchers")
    (version "3.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "shoulda-matchers" version))
       (sha256
        (base32
         "1zvv94pqk5b5my3w1shdz7h34xf2ldhg5k4qfdpbwi2iy0j9zw2a"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             ;; Do not run tests to avoid circular dependence with rails.  Instead
             ;; just import the library to test.
             (zero? (system* "ruby" "-Ilib" "-r" "shoulda-matchers")))))))
    (propagated-inputs
     `(("ruby-activesupport" ,ruby-activesupport)))
    (synopsis "Collection of testing matchers extracted from Shoulda")
    (description
     "Shoulda Matchers provides RSpec- and Minitest-compatible one-liners that
test common Rails functionality.  These tests would otherwise be much longer,
more complex, and error-prone.")
    (home-page "https://github.com/thoughtbot/shoulda-matchers")
    (license license:expat)))

(define-public ruby-shoulda-matchers-2
  (package
    (inherit ruby-shoulda-matchers)
    (version "2.8.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "shoulda-matchers" version))
              (sha256
               (base32
                "0d3ryqcsk1n9y35bx5wxnqbgw4m8b3c79isazdjnnbg8crdp72d0"))))))

(define-public ruby-shoulda
  (package
    (name "ruby-shoulda")
    (version "3.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "shoulda" version))
       (sha256
        (base32
         "0csmf15a7mcinfq54lfa4arp0f4b2jmwva55m0p94hdf3pxnjymy"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           ;; Don't run tests to avoid circular dependence with rails.  Instead
           ;; just import the library to test.
           (lambda _ (zero? (system* "ruby" "-Ilib" "-r" "shoulda")))))))
    (propagated-inputs
     `(("ruby-shoulda-context" ,ruby-shoulda-context)
       ("ruby-shoulda-matchers" ,ruby-shoulda-matchers-2)))
    (synopsis "Context framework and matchers for testing")
    (description
     "@code{shoulda} is a meta-package combining @code{shoulda-context} and
@code{shoulda-matchers} providing tools for writing tests.")
    (home-page "https://github.com/thoughtbot/shoulda")
    (license license:expat)))

(define-public ruby-unf
  (package
    (name "ruby-unf")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "unf" version))
       (sha256
        (base32
         "0bh2cf73i2ffh4fcpdn9ir4mhq8zi50ik0zqa1braahzadx536a9"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'add-dependency-to-bundler
           (lambda _
             ;; test-unit is required but not provided by the bundler
             ;; environment.  This is fixed in the upstream repository but fix
             ;; has not been released.
             (substitute* "Gemfile"
               (("^gemspec") "gem 'test-unit'\ngemspec"))
             #t)))))
    (propagated-inputs
     `(("ruby-unf-ext" ,ruby-unf-ext)))
    (native-inputs
     `(("ruby-shoulda" ,ruby-shoulda)
       ("bundler" ,bundler)
       ("ruby-test-unit" ,ruby-test-unit)))
    (synopsis "Unicode Normalization Form support to Ruby and JRuby")
    (description
     "@code{ruby-unf} is a wrapper library to bring Unicode Normalization Form
support to both Ruby and JRuby.  It uses @code{unf_ext} on CRuby and
@code{java.text.Normalizer} on JRuby.")
    (home-page "https://github.com/knu/ruby-unf")
    (license license:bsd-2)))

(define-public ruby-domain-name
  (package
    (name "ruby-domain-name")
    (version "0.5.20170404")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "domain_name" version))
       (sha256
        (base32
         "12hs8yijhak7p2hf1xkh98g0mnp5phq3mrrhywzaxpwz1gw5r3kf"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-versions
           (lambda _
             ;; Fix NameError that appears to already be fixed upstream.
             (substitute* "Rakefile"
               (("DomainName::VERSION")
                "Bundler::GemHelper.gemspec.version"))
             ;; Loosen unnecessarily strict test-unit version specification.
             (substitute* "domain_name.gemspec"
               (("<test-unit>.freeze, \\[\\\"~> 2.5.5") "<test-unit>, [\">0"))
             #t)))))
    (propagated-inputs
     `(("ruby-unf" ,ruby-unf)))
    (native-inputs
     `(("ruby-shoulda" ,ruby-shoulda)
       ("bundler" ,bundler)
       ("ruby-test-unit" ,ruby-test-unit)))
    (synopsis "Domain name manipulation library")
    (description
     "@code{domain_name} is a Domain name manipulation library.  It parses a
domain name ready for extracting the registered domain and TLD (Top Level
Domain).  It can also be used for cookie domain validation based on the Public
Suffix List.")
    (home-page "https://github.com/knu/ruby-domain_name")
    (license license:bsd-2)))

(define-public ruby-http-cookie
  (package
    (name "ruby-http-cookie")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "http-cookie" version))
       (sha256
        (base32
         "004cgs4xg5n6byjs7qld0xhsjq3n6ydfh897myr2mibvh6fjc49g"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'add-dependency-to-bundler
           (lambda _
             ;; Fix NameError
             (substitute* "Rakefile"
               (("HTTP::Cookie::VERSION")
                "Bundler::GemHelper.gemspec.version"))
             #t)))))
    (propagated-inputs
     `(("ruby-domain-name" ,ruby-domain-name)))
    (native-inputs
     `(("rubysimplecov" ,ruby-simplecov)
       ("bundler" ,bundler)
       ("ruby-sqlite3" ,ruby-sqlite3)
       ("ruby-test-unit" ,ruby-test-unit)))
    (synopsis "Handle HTTP Cookies based on RFC 6265")
    (description
     "@code{HTTP::Cookie} is a Ruby library to handle HTTP Cookies based on
RFC 6265.  It has been designed with security, standards compliance and
compatibility in mind, to behave just the same as today's major web browsers.
It has built-in support for the legacy @code{cookies.txt} and
@code{cookies.sqlite} formats of Mozilla Firefox.")
    (home-page "https://github.com/sparklemotion/http-cookie")
    (license license:expat)))

(define-public ruby-httpclient
  (package
    (name "ruby-httpclient")
    (version "2.8.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "httpclient" version))
       (sha256
        (base32
         "19mxmvghp7ki3klsxwrlwr431li7hm1lczhhj8z4qihl2acy8l99"))))
    (build-system ruby-build-system)
    (arguments
     '(;; TODO: Some tests currently fail
       ;; ------
       ;; 211 tests, 729 assertions, 13 failures, 4 errors, 0 pendings,
       ;; 2 omissions, 0 notifications
       ;; 91.866% passed
       ;; ------
       ;; 6.49 tests/s, 22.41 assertions/s
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (if tests?
                 (zero?
                  (system* "ruby"
                           "-Ilib"
                           "test/runner.rb"))
                 #t)))
         (add-after 'install 'wrap-bin-httpclient
           (lambda* (#:key outputs #:allow-other-keys)
             (wrap-program (string-append (assoc-ref outputs "out")
                                          "/bin/httpclient")
               `("GEM_HOME" ":" prefix (,(getenv "GEM_HOME"))))
             #t)))))
    (native-inputs
     `(("ruby-rack" ,ruby-rack)))
    (synopsis
     "Make HTTP requests with support for HTTPS, Cookies, authentication and more")
    (description
     "The @code{httpclient} ruby library provides functionality related to
HTTP.  Compared to the @code{net/http} library, @{httpclient} also provides
Cookie, multithreading and authentication (digest, NTLM) support.

Also provided is a @command{httpclient} command, which can perform HTTP
requests either using arguments or with an interactive prompt.")
    (home-page "https://github.com/nahi/httpclient")
    (license license:ruby)))

(define-public ruby-ansi
  (package
    (name "ruby-ansi")
    (version "1.5.0")
    (source
     (origin
       (method url-fetch)
       ;; Fetch from GitHub as the gem does not contain testing code.
       (uri (string-append "https://github.com/rubyworks/ansi/archive/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1zdip30hivyipi8hndhb457bhiz033awd00bgrsk5axjrwp6zhly"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Disable testing to break the cycle ansi, ae, ansi, as well as the
         ;; cycle ansi, qed, ansi.  Instead simply test that the library can
         ;; be require'd.
         (replace 'check
           (lambda _
             (zero? (system* "ruby" "-Ilib" "-r" "ansi"))))
         (add-before 'validate-runpath 'replace-broken-symlink
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (file (string-append out
                                         ,(gem-directory (package-version ruby))
                                         "/ansi-" ,version "/lib/ansi.yml")))
               ;; XXX: This symlink is broken since ruby 2.4.
               ;; https://lists.gnu.org/archive/html/guix-devel/2017-06/msg00034.html
               (delete-file file)
               (symlink "../.index" file)
               #t))))))
    (synopsis "ANSI escape code related libraries")
    (description
     "This package is a collection of ANSI escape code related libraries
enabling ANSI colorization and stylization of console output.  Included in the
library are the @code{Code} module, which defines ANSI codes as constants and
methods, a @code{Mixin} module for including color methods, a @code{Logger}, a
@code{ProgressBar}, and a @code{String} subclass.  The library also includes a
@code{Terminal} module which provides information about the current output
device.")
    (home-page "https://rubyworks.github.io/ansi")
    (license license:bsd-2)))

(define-public ruby-systemu
  (package
    (name "ruby-systemu")
    (version "2.6.5")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "systemu" version))
       (sha256
        (base32
         "0gmkbakhfci5wnmbfx5i54f25j9zsvbw858yg3jjhfs5n4ad1xq1"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-version
           (lambda _
             (setenv "VERSION" ,version)
             #t)))))
    (synopsis "Capture of stdout/stderr and handling of child processes")
    (description
     "Systemu can be used on any platform to return status, stdout, and stderr
of any command.  Unlike other methods like @code{open3} and @code{popen4}
there is no danger of full pipes or threading issues hanging your process or
subprocess.")
    (home-page "https://github.com/ahoward/systemu")
    (license license:ruby)))

(define-public ruby-bio-commandeer
  (package
    (name "ruby-bio-commandeer")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "bio-commandeer" version))
       (sha256
        (base32
         "0khpfw1yl5l3d2m8nxpkk32ybc4c3pa5hic3agd160jdfjjjnlni"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           ;; Run test without calling 'rake' so that jeweler is
           ;; not required as an input.
           (lambda _
             (zero? (system* "rspec" "spec/bio-commandeer_spec.rb")))))))
    (propagated-inputs
     `(("ruby-bio-logger" ,ruby-bio-logger)
       ("ruby-systemu" ,ruby-systemu)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)))
    (synopsis "Simplified running of shell commands from within Ruby")
    (description
     "Bio-commandeer provides an opinionated method of running shell commands
from within Ruby.  The advantage of bio-commandeer over other methods of
running external commands is that when something goes wrong, messages printed
to the @code{STDOUT} and @code{STDERR} streams are reported, giving extra
detail to ease debugging.")
    (home-page "https://github.com/wwood/bioruby-commandeer")
    (license license:expat)))

(define-public ruby-rubytest
  (package
    (name "ruby-rubytest")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rubytest" version))
       (sha256
        (base32
         "19jydsdnkl81i9dhdcr4dc34j0ilm68ff2ngnka1hi38xiw4p5qz"))))
    (build-system ruby-build-system)
    (arguments
     ;; Disable regular testing to break the cycle rubytest, qed, brass,
     ;; rubytest, as well as the cycle rubytest, qed, ansi, rubytest.  Instead
     ;; simply test that the library can be require'd.
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (zero? (system* "ruby" "-Ilib" "-r" "rubytest")))))))
    (propagated-inputs
     `(("ruby-ansi" ,ruby-ansi)))
    (synopsis "Universal test harness for Ruby")
    (description
     "Rubytest is a testing meta-framework for Ruby.  It can handle any
compliant test framework and can run tests from multiple frameworks in a
single pass.")
    (home-page "https://rubyworks.github.io/rubytest")
    (license license:bsd-2)))

(define-public ruby-brass
  (package
    (name "ruby-brass")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "brass" version))
       (sha256
        (base32
         "154lp8rp1vmg60ri1j4cb8hqlw37z7bn575h899v8hzxwi11sxka"))))
    (build-system ruby-build-system)
    (arguments
     ;; Disable tests to break the cycle brass, lemon, ae, qed, brass.
     ;; Instead simply test that the library can be require'd.
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (zero? (system* "ruby" "-Ilib" "-r" "brass")))))))
    (synopsis "Basic foundational assertions framework")
    (description
     "BRASS (Bare-Metal Ruby Assertion System Standard) is a basic
foundational assertions framework for other assertion and test frameworks to
make use of.")
    (home-page "https://rubyworks.github.io/brass")
    (license license:bsd-2)))

(define-public ruby-qed
  (package
    (name "ruby-qed")
    (version "2.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "qed" version))
       (sha256
        (base32
         "03h4lmlxpcya8j7s2cnyscqlx8v3xl1xgsw5y1wk1scxcgz2vbmr"))))
    (build-system ruby-build-system)
    (arguments
     ;; Disable testing to break the cycle qed, ansi, qed, among others.
     ;; Instead simply test that the executable runs using --copyright.
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (zero? (system* "ruby" "-Ilib" "bin/qed" "--copyright")))))))
    (propagated-inputs
     `(("ruby-ansi" ,ruby-ansi)
       ("ruby-brass" ,ruby-brass)))
    (synopsis "Test framework utilizing literate programming techniques")
    (description
     "@dfn{Quality Ensured Demonstrations} (QED) is a test framework for
@dfn{Test Driven Development} (TDD) and @dfn{Behaviour Driven
Development} (BDD) utilizing Literate Programming techniques.  QED sits
somewhere between lower-level testing tools like @code{Test::Unit} and
requirement specifications systems like Cucumber.")
    (home-page "https://rubyworks.github.io/qed")
    (license license:bsd-2)))

(define-public ruby-ae
  (package
    (name "ruby-ae")
    (version "1.8.2")
    (source
     (origin
       (method url-fetch)
       ;; Fetch from github so tests are included.
       (uri (string-append
             "https://github.com/rubyworks/ae/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "147jmkx54x7asy2d8m4dyrhhf4hdx4galpnhwzai030y3cdsfrrl"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _ (zero? (system* "qed"))))
         (add-before 'validate-runpath 'replace-broken-symlink
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (file (string-append out
                                         ,(gem-directory (package-version ruby))
                                         "/ae-" ,version "/lib/ae.yml")))
               ;; XXX: This symlink is broken since ruby 2.4.
               ;; https://lists.gnu.org/archive/html/guix-devel/2017-06/msg00034.html
               (delete-file file)
               (symlink "../.index" file)
               #t))))))
    (propagated-inputs
     `(("ruby-ansi" ,ruby-ansi)))
    (native-inputs
     `(("ruby-qed" ,ruby-qed)))
    (synopsis "Assertions library")
    (description
     "Assertive Expressive (AE) is an assertions library specifically designed
for reuse by other test frameworks.")
    (home-page "https://rubyworks.github.io/ae")
    (license license:bsd-2)))

(define-public ruby-lemon
  (package
    (name "ruby-lemon")
    (version "0.9.1")
    (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "lemon" version))
      (sha256
       (base32
        "0gqhpgjavgpvx23rqpfqcv3d5bs8gc7lr9yvj8kxgp7mfbdc2jcm"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check (lambda _ (zero? (system* "qed")))))))
    (propagated-inputs
     `(("ruby-ae" ,ruby-ae)
       ("ruby-ansi" ,ruby-ansi)
       ("ruby-rubytest" ,ruby-rubytest)))
    (native-inputs
     `(("ruby-qed" ,ruby-qed)))
    (synopsis "Test framework correlating code structure and test unit")
    (description
     "Lemon is a unit testing framework that enforces highly formal
case-to-class and unit-to-method test construction.  This enforcement can help
focus concern on individual units of behavior.")
    (home-page "https://rubyworks.github.io/lemon")
    (license license:bsd-2)))

(define-public ruby-rubytest-cli
  (package
    (name "ruby-rubytest-cli")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rubytest-cli" version))
       (sha256
        (base32
         "0n7hv4k1ba4fm3i98c6ydbsqhkxgbp52mhi70ba1x3mqzfvk438p"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f)) ; no tests
    (propagated-inputs
     `(("ruby-ansi" ,ruby-ansi)
       ("ruby-rubytest" ,ruby-rubytest)))
    (synopsis "Command-line interface for rubytest")
    (description
     "Rubytest CLI is a command-line interface for running tests for
Rubytest-based test frameworks.  It provides the @code{rubytest} executable.")
    (home-page "https://rubyworks.github.io/rubytest-cli")
    (license license:bsd-2)))

(define-public ruby-hashery
  (package
    (name "ruby-hashery")
    (version "2.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "hashery" version))
       (sha256
        (base32
         "0qj8815bf7q6q7llm5rzdz279gzmpqmqqicxnzv066a020iwqffj"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (and (zero? (system* "qed"))
                  (zero? (system* "rubytest" "-Ilib" "-Itest" "test/"))))))))
    (native-inputs
     `(("ruby-rubytest-cli" ,ruby-rubytest-cli)
       ("ruby-qed" ,ruby-qed)
       ("ruby-lemon" ,ruby-lemon)))
    (synopsis "Hash-like classes with extra features")
    (description
     "The Hashery is a tight collection of @code{Hash}-like classes.
Included are the auto-sorting @code{Dictionary} class, the efficient
@code{LRUHash}, the flexible @code{OpenHash} and the convenient
@code{KeyHash}.  Nearly every class is a subclass of the @code{CRUDHash} which
defines a CRUD (Create, Read, Update and Delete) model on top of Ruby's
standard @code{Hash} making it possible to subclass and augment to fit any
specific use case.")
    (home-page "https://rubyworks.github.io/hashery")
    (license license:bsd-2)))

(define-public ruby-rc4
  (package
    (name "ruby-rc4")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "ruby-rc4" version))
       (sha256
        (base32
         "00vci475258mmbvsdqkmqadlwn6gj9m01sp7b5a3zd90knil1k00"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (zero? (system* "rspec" "spec/rc4_spec.rb")))))))
    (native-inputs
     `(("ruby-rspec" ,ruby-rspec-2)))
    (synopsis "Implementation of the RC4 algorithm")
    (description
     "RubyRC4 is a pure Ruby implementation of the RC4 algorithm.")
    (home-page "https://github.com/caiges/Ruby-RC4")
    (license license:expat)))

(define-public ruby-afm
  (package
    (name "ruby-afm")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "afm" version))
       (sha256
        (base32
         "06kj9hgd0z8pj27bxp2diwqh6fv7qhwwm17z64rhdc4sfn76jgn8"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("bundler" ,bundler)))
    (synopsis "Read Adobe Font Metrics (afm) files")
    (description
     "This library provides methods to read @dfn{Adobe Font Metrics} (afm)
files and use the data therein.")
    (home-page "https://github.com/halfbyte/afm")
    (license license:expat)))

(define-public ruby-ascii85
  (package
    (name "ruby-ascii85")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "Ascii85" version))
       (sha256
        (base32
         "0j95sbxd18kc8rhcnvl1w37kflqpax1r12h1x47gh4xxn3mz4m7q"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("bundler" ,bundler)))
    (synopsis "Encode and decode Ascii85 binary-to-text encoding")
    (description
     "This library provides methods to encode and decode Ascii85
binary-to-text encoding.  The main modern use of Ascii85 is in PostScript and
@dfn{Portable Document Format} (PDF) file formats.")
    (home-page "https://github.com/datawraith/ascii85gem")
    (license license:expat)))

(define-public ruby-ttfunk
  (package
    (name "ruby-ttfunk")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       ;; fetch from github as the gem does not contain testing code
       (uri (string-append
             "https://github.com/prawnpdf/ttfunk/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1izq84pnm9niyvkzp8k0vl232q9zj41hwmp9na9fzycfh1pbnsl6"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'remove-rubocop
           (lambda _
             ;; remove rubocop as a dependency as not needed for testing
             (substitute* "ttfunk.gemspec"
               (("spec.add_development_dependency\\('rubocop'.*") ""))
             (substitute* "Rakefile"
               (("require 'rubocop/rake_task'") "")
               (("Rubocop::RakeTask.new") ""))
             #t)))))
    (native-inputs
     `(("ruby-rspec" ,ruby-rspec)
       ("bundler" ,bundler)))
    (synopsis "Font metrics parser for the Prawn PDF generator")
    (description
     "TTFunk is a TrueType font parser written in pure Ruby.  It is used as
part of the Prawn PDF generator.")
    (home-page "https://github.com/prawnpdf/ttfunk")
    ;; From the README: "Matz's terms for Ruby, GPLv2, or GPLv3. See LICENSE
    ;; for details."
    (license (list license:gpl2 license:gpl3 license:ruby))))

(define-public ruby-puma
  (package
    (name "ruby-puma")
    (version "3.9.1")
    (source
     (origin
       (method url-fetch)
       ;; Fetch from GitHub because distributed gem does not contain tests.
       (uri (string-append "https://github.com/puma/puma/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "03pifga841h17brh4vgia8i2ybh3cmsyg0dbybzdf6dq51wzcxdx"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f ; Tests require an out-dated version of minitest.
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-gemspec
           (lambda _
             (substitute* "puma.gemspec"
               (("git ls-files") "find * |sort"))
             #t)))))
    (synopsis "Simple, concurrent HTTP server for Ruby/Rack")
    (description
     "Puma is a simple, fast, threaded, and highly concurrent HTTP 1.1 server
for Ruby/Rack applications.  Puma is intended for use in both development and
production environments.  In order to get the best throughput, it is highly
recommended that you use a Ruby implementation with real threads like Rubinius
or JRuby.")
    (home-page "http://puma.io")
    (license license:expat)))

(define-public ruby-hoe-git
  (package
    (name "ruby-hoe-git")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "hoe-git" version))
       (sha256
        (base32
         "10jmmbjm0lkglwxbn4rpqghgg1ipjxrswm117n50adhmy8yij650"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-hoe" ,ruby-hoe)
       ("git" ,git)))
    (synopsis "Hoe plugins for tighter Git integration")
    (description
     "This package provides a set of Hoe plugins for tighter Git integration.
It provides tasks to automate release tagging and pushing and changelog
generation.")
    (home-page "https://github.com/jbarnette/hoe-git")
    (license license:expat)))

(define-public ruby-sequel
  (package
    (name "ruby-sequel")
    (version "4.49.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "sequel" version))
       (sha256
        (base32
         "010p4a60npppvgbyw7pq5xia8aydpgxdlhh3qjm2615kwjsw3fl8"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; Avoid dependency loop with ruby-minitest-hooks.
    (synopsis "Database toolkit for Ruby")
    (description "Sequel provides thread safety, connection pooling and a
concise DSL for constructing SQL queries and table schemas.  It includes a
comprehensive ORM layer for mapping records to Ruby objects and handling
associated records.")
    (home-page "http://sequel.jeremyevans.net")
    (license license:expat)))

(define-public ruby-timecop
  (package
    (name "ruby-timecop")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "timecop" version))
       (sha256
        (base32
         "0d7mm786180v4kzvn1f77rhfppsg5n0sq2bdx63x9nv114zm8jrp"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-check-rubylib
           (lambda _
             ;; Set RUBYLIB so timecop tests finds its own lib.
             (setenv "RUBYLIB" "lib")
             #t)))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-minitest-rg" ,ruby-minitest-rg)
       ("ruby-mocha" ,ruby-mocha)
       ("ruby-activesupport" ,ruby-activesupport)))
    (synopsis "Test mocks for time-dependent functions")
    (description
     "Timecop provides \"time travel\" and \"time freezing\" capabilities,
making it easier to test time-dependent code.  It provides a unified method to
mock @code{Time.now}, @code{Date.today}, and @code{DateTime.now} in a single
call.")
    (home-page "https://github.com/travisjeffery/timecop")
    (license license:expat)))

(define-public ruby-concurrent
  (package
    (name "ruby-concurrent")
    (version "1.0.5")
    (source
     (origin
       (method url-fetch)
       ;; Download from GitHub because the rubygems version does not contain
       ;; Rakefile.
       (uri (string-append
             "https://github.com/ruby-concurrency/concurrent-ruby/archive/v"
             version
             ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0qhv0qzsby4iijgwa4s9r88zj8123pmyz1dwaqzdk57xgqll9pny"))
       ;; Exclude failing test reported at
       ;; https://github.com/ruby-concurrency/concurrent-ruby/issues/534
       (patches (search-patches "ruby-concurrent-ignore-broken-test.patch"
                                "ruby-concurrent-test-arm.patch"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         (add-before 'replace-git-ls-files 'remove-extra-gemspecs
           (lambda _
             ;; Delete extra gemspec files so 'first-gemspec' chooses the
             ;; correct one.
             (delete-file "concurrent-ruby-edge.gemspec")
             (delete-file "concurrent-ruby-ext.gemspec")
             #t))
         (add-before 'build 'replace-git-ls-files2
           (lambda _
             (substitute* "support/file_map.rb"
               (("git ls-files") "find * |sort"))
             #t))
         (add-before 'check 'rake-compile
           ;; Fix the test error described at
           ;; https://github.com/ruby-concurrency/concurrent-ruby/pull/408
           (lambda _ (zero? (system* "rake" "compile"))))
         (add-before 'check 'remove-timecop-dependency
           ;; Remove timecop-dependent tests as having timecop as a depedency
           ;; causes circular depedencies.
           (lambda _
             (delete-file "spec/concurrent/executor/timer_set_spec.rb")
             (delete-file "spec/concurrent/scheduled_task_spec.rb")
             #t)))))
    (native-inputs
     `(("ruby-rake-compiler" ,ruby-rake-compiler)
       ("ruby-rspec" ,ruby-rspec)))
    (synopsis "Concurrency tools for Ruby")
    (description
     "This library provides modern concurrency tools including agents,
futures, promises, thread pools, actors, supervisors, and more.  It is
inspired by Erlang, Clojure, Go, JavaScript, actors and classic concurrency
patterns.")
    (home-page "http://www.concurrent-ruby.com")
    (license license:expat)))

(define-public ruby-pkg-config
  (package
    (name "ruby-pkg-config")
    (version "1.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "pkg-config" version))
       (sha256
        (base32
         "056mzqdh4yjznsg36fi0xiq76f24vxlhzh2n4az919l3x5k318ar"))))
    (build-system ruby-build-system)
    (arguments
     ;; Tests require extra files not included in the gem.
     `(#:tests? #f))
    (synopsis "Detect libraries for compiling Ruby native extensions")
    (description
     "@code{pkg-config} can be used in your extconf.rb to properly detect need
libraries for compiling Ruby native extensions.")
    (home-page "https://github.com/ruby-gnome2/pkg-config")
    (license license:lgpl2.0+)))

(define-public ruby-net-http-digest-auth
  (package
    (name "ruby-net-http-digest-auth")
    (version "1.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "net-http-digest_auth" version))
       (sha256
        (base32
         "1nq859b0gh2vjhvl1qh1zrk09pc7p54r9i6nnn6sb06iv07db2jb"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("ruby-hoe" ,ruby-hoe)))
    (synopsis "RFC 2617 HTTP digest authentication library")
    (description
     "This library implements HTTP's digest authentication scheme based on
RFC 2617.  This enables the use of the digest authentication scheme instead
of the more insecure basic authentication scheme.")
    (home-page "https://github.com/drbrain/net-http-digest_auth")
    (license license:expat)))

(define-public ruby-mail
  (package
    (name "ruby-mail")
    (version "2.6.6")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "mail" version))
       (sha256
        (base32
         "0d7lhj2dw52ycls6xigkfz6zvfhc6qggply9iycjmcyj9760yvz9"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-mime-types" ,ruby-mime-types)))
    (arguments
     ;; Tests require extra gems not included in the Gemfile.
     ;; XXX: Try enabling this for the next version with mini_mime.
     `(#:tests? #f))
    (synopsis "Mail library for Ruby")
    (description
     "Mail is an internet library for Ruby that is designed to handle email
generation, parsing and sending.  The purpose of this library is to provide
a single point of access to handle all email functions, including sending
and receiving emails.  All network type actions are done through proxy
methods to @code{Net::SMTP}, @code{Net::POP3} etc.

Mail has been designed with a very simple object oriented system that
really opens up the email messages you are parsing, if you know what you
are doing, you can fiddle with every last bit of your email directly.")
    (home-page "https://github.com/mikel/mail")
    (license license:expat)))

(define-public ruby-code-statistics
  (package
    (name "ruby-code-statistics")
    (version "0.2.13")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "code_statistics" version))
       (sha256
        (base32
         "07rdpsbwbmh4vp8nxyh308cj7am2pbrfhv9v5xr2d5gq8hnnsm93"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f)) ; Not all test code is included in gem.
    (synopsis "Port of the rails 'rake stats' method")
    (description
     "This gem is a port of the rails 'rake stats' method so it can be made
more robust and work for non rails projects.")
    (home-page "http://github.com/danmayer/code_statistics")
    (license license:expat)))
