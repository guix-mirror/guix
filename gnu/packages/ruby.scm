;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 David Thompson <davet@gnu.org>
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
  #:use-module (gnu packages openssl)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages gdbm)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system ruby))

(define-public ruby
  (package
    (name "ruby")
    (version "2.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://cache.ruby-lang.org/pub/ruby/"
                           (version-major+minor version)
                           "/ruby-" version ".tar.xz"))
       (sha256
        (base32
         "0qj48a8ji8qj1sllsrhb6y65frwr77bvr08xikj86w5mib8baczh"))))
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
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/seattlerb/hoe.git")
                    (commit "0c11836"))) ; no release tags :(
              (sha256
               (base32
                "0i8dimf8kxcjgqj9x65bbi3l6hc9p9gbfbb1vmrz42764a4jjbz9"))) )
    (build-system ruby-build-system)
    (arguments
     '(#:phases (alist-replace
                 'build
                 (lambda _ (zero? (system* "rake" "gem")))
                 %standard-phases)))
    (synopsis "Ruby project management helper")
    (description
     "Hoe is a rake/rubygems helper for project Rakefiles.  It helps manage,
maintain, and release projects and includes a dynamic plug-in system allowing
for easy extensibility.  Hoe ships with plug-ins for all the usual project
tasks including rdoc generation, testing, packaging, deployment, and
announcement.")
    (home-page "http://www.zenspider.com/projects/hoe.html")
    (license license:expat)))

(define-public ruby-i18n
  (package
    (name "ruby-i18n")
    (version "0.6.11")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/svenfuchs/i18n/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1fdhnhh1p5g8vibv44d770z8nq208zrms3m2nswdvr54072y1m6k"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; requires bundler
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
              (uri (string-append
                    "https://github.com/rspec/rspec-support/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1pvzfrqgy0z0gwmdgjp9f2vz1d9c0cajyzfqj9z8i2ssxnzmj4bv"))))
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
              (uri (string-append
                    "https://github.com/rspec/rspec-core/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1clsa4lkh5c9c7xc3xa336ym00ycr67pchpg1bv4y3fz5hvzw8ki"))))
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

(define ruby-diff-lcs-for-rspec
  (package
    (name "ruby-diff-lcs")
    (version "1.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/halostatue/diff-lcs/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0kmfz2qdwbfjf97rx27hh9fm39mv3z9avjmvsajqnb5wxj2l5l4s"))))
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
              (uri (string-append
                    "https://github.com/rspec/rspec-expectations/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0h0rpprbh6h59gmksiyi1b8w6cvcai4wdbkikajwx3w1asxi6f7x"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; avoid dependency cycles
    (propagated-inputs
     `(("ruby-rspec-support" ,ruby-rspec-support)
       ("ruby-diff-lcs" ,ruby-diff-lcs-for-rspec)))
    (synopsis "RSpec expecations library")
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
              (uri (string-append
                    "https://github.com/rspec/rspec-mocks/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1xzxsg0idxkg7czmjgqq10lcd821ibw1hjzn404sk9j6rw0fbx2g"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; avoid dependency cycles
    (propagated-inputs
     `(("ruby-rspec-support" ,ruby-rspec-support)
       ("ruby-diff-lcs" ,ruby-diff-lcs-for-rspec)))
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
              (uri (string-append
                    "https://github.com/rspec/rspec/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1jg38dbaknsdhiav5vnrwfccg524fwcg6sq1715441vx1xl6p54q"))))
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
    (version "1.9.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/bundler/bundler/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "08flx3n9hb3yz8mm5k16cdz0sb7g774f6vxn6gc3wfh5la83vfyx"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; avoid dependency cycles
    (synopsis "Ruby gem bundler")
    (description "Bundler automatically downloads and installs a list of gems
specified in a \"Gemfile\", as well as their dependencies.")
    (home-page "http://bundler.io/")
    (license license:expat)))

(define-public ruby-useragent
  (package
    (name "ruby-useragent")
    (version "0.13.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/gshutler/useragent/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1hj00fw06i0y3rwxxhxmnrqxhpnffv4zfqx2sqqpc5qc4fdvd2x9"))))
    (build-system ruby-build-system)
    (arguments
     '(#:test-target "spec"))
    (native-inputs
     `(("ruby-rspec" ,ruby-rspec)
       ("bundler" ,bundler)))
    (synopsis "HTTP user agent parser for Ruby")
    (description "UserAgent is a Ruby library that parses and compares HTTP
User Agents.")
    (home-page "https://github.com/gshutler/useragent")
    (license license:expat)))

(define-public ruby-bacon
  (package
    (name "ruby-bacon")
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/chneukirchen/bacon/archive/"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0g03fxilrrx17dijww68n1lq5d8s69hrxgpga8c1i2k35bzcw5jc"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'build 'generate-docs
                    (lambda _
                      ;; This rake task also tries to generate a ChangeLog
                      ;; file from the Git log, which we don't have.  It fails
                      ;; but creates an empty file, allowing the rest of the
                      ;; build to succeed.
                      (zero? (system* "rake" "predist")))))))
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
              (uri (string-append
                    "https://github.com/rails/arel/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0fldwp2hmrmddx22xf7hdmybngzv97qnv5rvz3qw61m94ddd6w4n"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("bundler" ,bundler)))
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
              (uri (string-append
                    "https://github.com/mperham/connection_pool/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "02s5rwhmgy8qqns7a5y1daa0yaw38x6lzpwyvmy46h1yrj9mc6zf"))))
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
              (uri (string-append
                    "https://github.com/drbrain/net-http-persistent/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1q18vji31w8gfr6ajziqkqs8n5lzkw0bl00dm2a8s05zhavzw9j9"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'build ; no gemspec
                    (lambda _ (zero? (system* "rake" "gem")))))))
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
              (method git-fetch)
              ;; No release tarballs nor git tags.  This is the commit
              ;; corresponding to the addition of the release notes to
              ;; History.rdoc.
              (uri (git-reference
                    (url "https://github.com/seattlerb/minitest.git")
                    (commit "e975248")))
              (sha256
               (base32
                "09xjiahk7q8hid1i39ahrmghaslpj9n36zna72i3ah7kf1bh2l01"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'build ; no gemspec
                    (lambda _ (zero? (system* "rake" "gem")))))))
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
              (method git-fetch)
              ;; Same story as ruby-minitest.
              (uri (git-reference
                    (url "https://github.com/seattlerb/minitest-sprint.git")
                    (commit "49c02bc")))
              (sha256
               (base32
                "0rbmxz94lqg5vjz60p8v2bzq8adwvmx501amvk0l124sfwmw94ms"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'build ; no gemspec
                    (lambda _ (zero? (system* "rake" "gem")))))))
    (native-inputs
     `(("ruby-hoe" ,ruby-hoe)
       ("ruby-minitest" ,ruby-minitest)))
    (synopsis "Fast test suite runner for minitest")
    (description "Minitest-sprint is a test runner for minitest that makes it
easier to re-run individual failing tests.")
    (home-page "https://github.com/seattlerb/minitest-sprint")
    (license license:expat)))

(define-public ruby-daemons
  (package
    (name "ruby-daemons")
    (version "1.2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/thuehlinger/daemons/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1v5bpdvpvhk240pc7fkn44vfclppl44pp6wd42ipi5sd5lkk7zfd"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f)) ; no test suite
    (synopsis "Daemonize Ruby programs")
    (description "Daemons provides a way to wrap existing Ruby scripts to be
run as a daemon and to be controlled by simple start/stop/restart commands.")
    (home-page "https://github.com/thuehlinger/daemons")
    (license license:expat)))
