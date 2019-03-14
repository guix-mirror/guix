;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2014, 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014, 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2015, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016, 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017 ng0 <ng0@n0.is>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017, 2018, 2019 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2018 Vasile Dumitrascu <va511e@yahoo.com>
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
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
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages rails)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages java)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages ragel)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages web)
  #:use-module (guix build-system ruby)
  #:use-module ((srfi srfi-1) #:select (alist-delete)))

(define-public ruby
  (package
    (name "ruby")
    (version "2.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://cache.ruby-lang.org/pub/ruby/"
                           (version-major+minor version)
                           "/ruby-" version ".tar.xz"))
       (sha256
        (base32
         "0vrhrw7kcz9mg0jkqnihkcxqy5k05v8k1j0y2735z8wfk8sx1j8w"))
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
            (files (list (string-append "lib/ruby/vendor_ruby"))))))
    (synopsis "Programming language interpreter")
    (description "Ruby is a dynamic object-oriented programming language with
a focus on simplicity and productivity.")
    (home-page "https://www.ruby-lang.org")
    (license license:ruby)))

(define-public ruby-2.4
  (package
    (inherit ruby)
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
       (patches (search-patches "ruby-rubygems-276-for-ruby24.patch"))
       (modules '((guix build utils)))
       (snippet `(begin
                   ;; Remove bundled libffi
                   (delete-file-recursively "ext/fiddle/libffi-3.2.1")
                   #t))))))

(define-public ruby-2.3
  (package
    (inherit ruby)
    (version "2.3.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://cache.ruby-lang.org/pub/ruby/"
                           (version-major+minor version)
                           "/ruby-" version ".tar.xz"))
       (sha256
        (base32
         "1zhxbjff08pvbnxvn58krns6q0p6g4977q6ykfn823gxhifn63wi"))
       (modules '((guix build utils)))
       (snippet `(begin
                   ;; Remove bundled libffi
                   (delete-file-recursively "ext/fiddle/libffi-3.2.1")
                   #t))))))

(define-public mruby
  (package
    (name "mruby")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mruby/mruby.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1r6w1asjshff43ymdwa6xmrkggza99mi2kw88k7ic6ag2j81hcj5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'enable-verbose-tests
           (lambda _
             (substitute* "Makefile"
               (("ruby ./minirake" m)
                (string-append m " --verbose")))
             #t))
         (add-after 'unpack 'disable-broken-tests
           (lambda _
             (substitute* "mrbgems/mruby-io/test/io.rb"
               (("assert\\('IO.popen.+$" m)
                (string-append m "skip \"Hangs in the Guix build environment\"\n"))
               (("assert\\('IO#isatty.+$" m)
                (string-append m "skip \"Disable for Guix; there is no /dev/tty\"\n"))
               ;; This one is really weird.  The *expected* output is all wrong.
               (("assert\\('`cmd`.*" m)
                (string-append m "skip \"Disable for Guix\"\n"))
               (("echo foo")
                (string-append (which "echo") " foo")))
             #t))
         ;; There is no install target
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lib (string-append out "/lib")))
               (mkdir-p bin)
               (copy-recursively "build/host/bin" bin)
               (mkdir-p lib)
               (copy-recursively "build/host/lib" lib))
             #t)))))
    (native-inputs
     `(("ruby" ,ruby)
       ("bison" ,bison)))
    (home-page "https://github.com/mruby/mruby")
    (synopsis "Lightweight Ruby")
    (description "mruby is the lightweight implementation of the Ruby
language.  Its syntax is Ruby 1.9 compatible.  mruby can be linked and
embedded within your application.")
    (license license:expat)))

(define-public ruby-commander
  (package
    (name "ruby-commander")
    (version "4.4.7")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "commander" version))
       (sha256
        (base32
         "1pxakz596fjqak3cdbha6iva1dlqis86i3kjrgg6lf3sp8i5vhwg"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         ;; Don't run or require rubocop, the code linting tool, as this is a
         ;; bit unnecessary.
         (add-after 'unpack 'dont-run-rubocop
           (lambda _
             (substitute* "Rakefile"
               ((".*rubocop.*") "")
               ((".*RuboCop.*") ""))
             #t)))))
    (propagated-inputs
     `(("ruby-highline" ,ruby-highline)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec-core" ,ruby-rspec-core)
       ("ruby-rspec-expectations" ,ruby-rspec-expectations)
       ("ruby-rspec-mocks" ,ruby-rspec-mocks)
       ("ruby-simplecov" ,ruby-simplecov)))
    (home-page "https://github.com/commander-rb/commander")
    (synopsis "Library for building Ruby command-line executables")
    (description
     "Commander aims to be a complete solution for Ruby command-line
executables.  Commander bridges the gap between other terminal related
libraries (OptionParser, HighLine), while providing many new features, and an
elegant API.")
    (license license:expat)))

(define-public ruby-highline
  (package
    (name "ruby-highline")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "highline" version))
       (sha256
        (base32
         "0gr6pckj2jayxw1gdgh9193j5jag5zrrqqlrnl4jvcwpyd3sn2zc"))))
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

(define-public ruby-rsync
  (package
    (name "ruby-rsync")
    (version "1.0.9")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rsync" version))
       (sha256
        (base32
         "0p8b27q1gvxilqfq2528xpwglzcm2myikkjxpqk7mwbwg9r6knxv"))))
    (build-system ruby-build-system)
    (arguments
     '(#:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-coveralls-requirement
           (lambda _
             (substitute* "spec/spec_helper.rb"
               (("require 'coveralls'") "")
               (("Coveralls.wear!") ""))
             #t)))))
    (native-inputs
     `(("bundler" ,bundler)
       ("rsync" ,rsync)
       ("ruby-rspec-core" ,ruby-rspec-core)
       ("ruby-rspec-expectations" ,ruby-rspec-expectations)
       ("ruby-rspec-mocks" ,ruby-rspec-mocks)))
    (home-page "https://github.com/jbussdieker/ruby-rsync")
    (synopsis "Ruby wrapper around rsync")
    (description
     "Ruby Rsync is a Ruby library that can synchronize files between remote
hosts by wrapping the @file{rsync} binary.")
    (license license:expat)))

(define-public ruby-i18n
  (package
    (name "ruby-i18n")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "i18n" version))
              (sha256
               (base32
                "0ppvmla21hssvrfm8g1n2fnb4lxn4yhy9qmmba0imanflgldrjmr"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; no tests
    (propagated-inputs `(("concurrent-ruby" ,ruby-concurrent)))
    (synopsis "Internationalization library for Ruby")
    (description "Ruby i18n is an internationalization and localization
solution for Ruby programs.  It features translation and localization,
interpolation of values to translations, pluralization, customizable
transliteration to ASCII, flexible defaults, bulk lookup, lambdas as
translation data, custom key/scope separator, custom exception handlers, and
an extensible architecture with a swappable backend.")
    (home-page "https://github.com/svenfuchs/i18n")
    (license license:expat)))

(define-public ruby-iruby
  (package
    (name "ruby-iruby")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "iruby" version))
       (sha256
        (base32
         "1wdf2c0x8y6cya0n3y0p3p7b1sxkb2fdavdn2k58rf4rs37s7rzn"))))
    (build-system ruby-build-system)
    (arguments
     ;; TODO: Tests currently fail.
     ;;
     ;; Finished in 1.764405s, 1.1335 runs/s, 5.1009 assertions/s.
     ;;
     ;;   1) Failure:
     ;; IntegrationTest#test_interaction [/tmp/guix-build-ruby-iruby-0.3.drv-0/gem/test/integration_test.rb:25]:
     ;; In [ expected
     ;;
     ;; 2 runs, 9 assertions, 1 failures, 0 errors, 0 skips
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-ipython
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "lib/iruby/command.rb"
               (("version = `")
                (string-append
                 "version = `"
                 (assoc-ref inputs "python-ipython")
                 "/bin/"))
               (("Kernel\\.exec\\('")
                (string-append
                 "Kernel.exec('"
                 (assoc-ref inputs "python-ipython")
                 "/bin/")))
             #t)))))
    (inputs
     `(("python-ipython" ,python-ipython)))
    (propagated-inputs
     `(("ruby-bond" ,ruby-bond)
       ("ruby-data_uri" ,ruby-data_uri)
       ("ruby-mimemagic" ,ruby-mimemagic)
       ("ruby-multi-json" ,ruby-multi-json)
       ("ruby-cztop" ,ruby-cztop)
       ;; Optional inputs
       ("ruby-pry" ,ruby-pry)))
    (synopsis "Ruby kernel for Jupyter/IPython")
    (description
     "This package provides a Ruby kernel for Jupyter/IPython frontends (e.g.
notebook).")
    (home-page "https://github.com/SciRuby/iruby")
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

(define-public ruby-rspec-its
  (package
    (name "ruby-rspec-its")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rspec/rspec-its.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "190rz7v4q4wk80fzhr5hknvxx4vb2pywmqr8wc41w2blj9ylzi0f"))
       (patches
        (list
         (origin (method url-fetch)
                 (uri (string-append
                       "https://github.com/rspec/rspec-its/commit/"
                       "bfaab439c7c879f5ef25552f41827891f6308373.patch"))
                 (file-name "ruby-rspec-its-fix-specs-for-ruby-2.4.patch")
                 (sha256
                  (base32
                   "0lnik0kvrpgkakvdb2fmzg22pdlraf6kiidr9sv6rnfyviiqwxgh")))))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'dont-install-gems-from-gemfile
           (lambda _
             (substitute* "Gemfile"
               (("rspec rspec-core rspec-expectations rspec-mocks rspec-support")
                ""))
             #t))
         (add-before 'build 'remove-unnecessary-dependency-versions-from-gemfile
           (lambda _
             (substitute* "rspec-its.gemspec"
               (("rake.*") "rake'\n")
               (("cucumber.*") "cucumber'\n")
               (("bundler.*") "bundler'\n")
               (("aruba.*") "aruba'\n"))
             #t)))))
    (propagated-inputs
     `(("ruby-rspec-core" ,ruby-rspec-core)
       ("ruby-rspec-expectations" ,ruby-rspec-expectations)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-cucumber" ,ruby-cucumber)
       ("ruby-aruba" ,ruby-aruba)))
    (synopsis "RSpec extension that provides the @code{its} method")
    (description
     "RSpec::Its provides the its method as a short-hand to specify the expected
value of an attribute.  For example, one can use @code{its(:size)\\{should
eq(1)\\}}.")
    (home-page "https://github.com/rspec/rspec-its")
    (license license:expat)))

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

(define-public ruby-rspec-rerun
  (package
    (name "ruby-rspec-rerun")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rspec-rerun" version))
       (sha256
        (base32
         "1gy7znkcaqhpccfnk2nvaqbsvgxy3q57cmjwkl9fi1zabaq5lbkj"))))
    (build-system ruby-build-system)
    (arguments
     '(;; No included tests
       #:tests? #f))
    (propagated-inputs `(("ruby-rspec" ,ruby-rspec)))
    (synopsis "Track failed RSpec tests to re-run them")
    (description
     "This package provides an automated way to track, and then re-run failed
RSpec tests.")
    (home-page "https://github.com/dblock/rspec-rerun")
    (license license:expat)))

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
    (version "1.17.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "bundler" version))
              (sha256
               (base32
                "0ln3gnk7cls81gwsbxvrmlidsfd78s6b2hzlm4d4a9wbaidzfjxw"))))
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

(define-public ruby-bump
  (package
    (name "ruby-bump")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "bump" version))
       (sha256
        (base32
         "1xinbr9rzh6cj75x24niwgqcnbhdxc68a8bc41lk8xv6fd906fym"))))
    (build-system ruby-build-system)
    (arguments
     '(;; No included tests
       #:tests? #f))
    (synopsis "Tool for working with Rubygems")
    (description
     "Bump provides commands to manage Rubygem versioning, updating to the
next patch version for example.")
    (home-page "https://github.com/gregorym/bump")
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

(define-public ruby-asciidoctor
  (package
  (name "ruby-asciidoctor")
  (version "1.5.7.1")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "asciidoctor" version))
      (sha256
        (base32
          "0v52bzc72cvg7zfgq27pa4mgyf29dx9m20fghrw1xmvwgd519n1w"))))
  (build-system ruby-build-system)
  (arguments
   `(#:test-target "test:all"
     #:phases
     (modify-phases %standard-phases
       (add-before 'check 'remove-circular-tests
         (lambda _
           ;; Remove tests that require circular dependencies to load or pass.
           (delete-file "test/invoker_test.rb")
           (delete-file "test/converter_test.rb")
           (delete-file "test/options_test.rb")
           #t)))))
  (native-inputs
   `(("ruby-minitest" ,ruby-minitest)
     ("ruby-nokogiri" ,ruby-nokogiri)
     ("ruby-asciimath" ,ruby-asciimath)
     ("ruby-coderay" ,ruby-coderay)))
  (synopsis "Converter from AsciiDoc content to other formats")
  (description
    "Asciidoctor is a text processor and publishing toolchain for converting
AsciiDoc content to HTML5, DocBook 5 (or 4.5), PDF, and other formats.")
  (home-page "https://asciidoctor.org")
  (license license:expat)))

(define-public ruby-ast
  (package
    (name "ruby-ast")
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "ast" version))
       (sha256
        (base32
         "184ssy3w93nkajlz2c70ifm79jp3j737294kbc5fjw69v1w0n9x7"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-coveralls-requirement
           (lambda _
             (substitute* "test/helper.rb"
               (("require 'coveralls'") "")
               (("Coveralls::SimpleCov::Formatter") ""))
             #t))
         (add-after 'extract-gemspec 'remove-unnecessary-requirements
           (lambda _
             (substitute* "ast.gemspec"
               ((".*coveralls.*") "\n")
               (("%q<rest-client>.*") "%q<rest-client>.freeze, [\">= 0\"])\n")
               (("%q<mime-types>.*") "%q<mime-types>.freeze, [\">= 0\"])\n")
               (("%q<rake>.*") "%q<rake>.freeze, [\">= 0\"])\n"))
             #t)))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-simplecov" ,ruby-simplecov)
       ("ruby-json-pure" ,ruby-json-pure)
       ("ruby-mime-times" ,ruby-mime-types)
       ("ruby-yard" ,ruby-yard)
       ("ruby-kramdown" ,ruby-kramdown)
       ("ruby-rest-client" ,ruby-rest-client)
       ("ruby-bacon" ,ruby-bacon)
       ("ruby-bacon-colored-output" ,ruby-bacon-colored-output)
       ("ruby-racc" ,ruby-racc)))
    (synopsis "Library for working with Abstract Syntax Trees")
    (description
     "@code{ast} is a Ruby library for working with Abstract Syntax Trees.
It does this through immutable data structures.")
    (home-page "https://whitequark.github.io/ast/")
    (license license:expat)))

(define-public ruby-sporkmonger-rack-mount
  ;; Testing the addressable gem requires a newer commit than that released, so
  ;; use an up to date version.
  (let ((revision "1")
        (commit "076aa2c47d9a4c081f1e9bcb56a826a9e72bd5c3"))
    (package
      (name "ruby-sporkmonger-rack-mount")
      (version (git-version "0.8.3" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/sporkmonger/rack-mount.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1scx273g3xd93424x9lxc4zyvcp2niknbw5mkz6wkivpf7xsyxdq"))))
      (build-system ruby-build-system)
      (arguments
       ;; Tests currently fail so disable them.
       ;; https://github.com/sporkmonger/rack-mount/pull/1
       `(#:tests? #f))
      (propagated-inputs `(("ruby-rack" ,ruby-rack)))
      (synopsis "Stackable dynamic tree based Rack router")
      (description
       "@code{Rack::Mount} supports Rack's @code{X-Cascade} convention to
continue trying routes if the response returns pass.  This allows multiple
routes to be nested or stacked on top of each other.")
      (home-page "https://github.com/sporkmonger/rack-mount")
      (license license:expat))))

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

(define-public ruby-contracts
  (package
    (name "ruby-contracts")
    (version "0.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "contracts" version))
       (sha256
        (base32
         "119f5p1n6r5svbx8h09za6a4vrsnj5i1pzr9cqdn9hj3wrxvyl3a"))))
    (build-system ruby-build-system)
    (arguments
     '(#:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         ;; Don't run or require rubocop, the code linting tool, as this is a
         ;; bit unnecessary.
         (add-after 'unpack 'dont-run-rubocop
          (lambda _
            (substitute* "Rakefile"
              ((".*rubocop.*") "")
              ((".*RuboCop.*") ""))
            #t)))))
    (native-inputs
     `(("ruby-rspec" ,ruby-rspec)))
    (synopsis "Method contracts for Ruby")
    (description
     "This library provides contracts for Ruby.  A contract describes the
correct inputs and output for a method, and will raise an error if a incorrect
value is found.")
    (home-page "https://github.com/egonSchiele/contracts.ruby")
    (license license:bsd-2)))

(define-public ruby-crack
  (package
    (name "ruby-crack")
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "crack" version))
       (sha256
        (base32
         "0abb0fvgw00akyik1zxnq7yv391va148151qxdghnzngv66bl62k"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (for-each (lambda (file)
                           (display file)(display "\n")
                           (invoke "ruby" "-Ilib" "-Itest" "-rrubygems" file))
                         (find-files "test" ".*rb$")))
             #t)))))
    (propagated-inputs
     `(("ruby-safe-yaml" ,ruby-safe-yaml)))
    (synopsis "Simple JSON and XML parsing for Ruby")
    (description
     "@code{crack} provides really simple JSON and XML parsing, extracted from
code in Merb and Rails.")
    (home-page "https://github.com/jnunemaker/crack")
    (license license:expat)))

(define-public ruby-crass
  (package
    (name "ruby-crass")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "crass" version))
       (sha256
        (base32
         "0bpxzy6gjw9ggjynlxschbfsgmx8lv3zw1azkjvnb8b9i895dqfi"))))
    (build-system ruby-build-system)
    (synopsis "Pure Ruby CSS parser based on CSS Syntax Level 3")
    (description
     "Crass is a pure Ruby CSS parser based on the CSS Syntax Level 3 spec.")
    (home-page "https://github.com/rgrove/crass/")
    (license license:expat)))

(define-public ruby-cliver
  (package
    (name "ruby-cliver")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "cliver" version))
       (sha256
        (base32
         "096f4rj7virwvqxhkavy0v55rax10r4jqf8cymbvn4n631948xc7"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; Avoid a incompatibility between rspec@2 and rake. Using rspec@3
         ;; would be nice, but the tests look to be incompatible:
         ;;
         ;; NoMethodError: undefined method `last_comment'
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "rspec"))
             #t)))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec-2)))
    (synopsis "Assertions for command-line dependencies in Ruby")
    (description
     "@code{cliver} provides a way to detect missing command-line
dependencies, including versions.")
    (home-page "https://github.com/yaauie/cliver")
    (license license:expat)))

(define-public ruby-czmq-ffi-gen
  (package
    (name "ruby-czmq-ffi-gen")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "czmq-ffi-gen" version))
       (sha256
        (base32
         "1yf719dmf4mwks1hqdsy6i5kzfvlsha69sfnhb2fr2cgk2snbys3"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f ;; Tests are not included in the release on rubygems.org
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-lib_dirs
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "lib/czmq-ffi-gen/czmq/ffi.rb"
               (("lib\\_dirs = \\[.*\\]")
                (string-append "lib_dirs = ['"
                               (assoc-ref inputs "czmq") "/lib"
                               "']")))
             (substitute* "lib/czmq-ffi-gen/libzmq.rb"
               (("lib\\_dirs = \\[.*\\]")
                (string-append "lib_dirs = ['"
                               (assoc-ref inputs "zeromq") "/lib"
                               "']"))))))))
    (inputs
     `(("zeromq" ,zeromq)
       ("czmq" ,czmq)))
    (propagated-inputs `(("ruby-ffi" ,ruby-ffi)))
    (synopsis "Low-level Ruby bindings for CZMQ (generated using zproject)")
    (description
     "These Ruby bindings are not intended to be directly used, but rather
used by higher level bindings like those provided by CZTop.")
    (home-page
     "https://github.com/paddor/czmq-ffi-gen")
    (license license:isc)))

(define-public ruby-cztop
  (package
    (name "ruby-cztop")
    (version "0.12.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "cztop" version))
       (sha256
        (base32
         "0yqbpaiw5d7f271d73lyrsh8xpx6n4zi6xqwfgi00dacxrq3s3fa"))))
    (build-system ruby-build-system)
    (arguments
     '(#:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-lib_paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "lib/cztop/poller/zmq.rb"
               (("lib\\_paths = \\[.*\\]")
                (string-append "lib_paths = ['"
                               (assoc-ref inputs "zeromq") "/lib"
                               "']"))))))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)))
    (inputs
     `(("zeromq" ,zeromq)))
    (propagated-inputs
     `(("ruby-czmq-ffi-gen" ,ruby-czmq-ffi-gen)))
    (synopsis "CZMQ Ruby bindings")
    (description
     "CZMQ Ruby bindings, based on the generated low-level FFI bindings of
CZMQ.  The focus of of CZTop is on being easy to use and providing first class
support for security mechanisms.")
    (home-page "https://github.com/paddor/cztop")
    (license license:isc)))

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

(define-public ruby-oauth2
  (package
    (name "ruby-oauth2")
    (version "1.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "oauth2" version))
       (sha256
        (base32
         "0av6nlb5y2sm6m8fx669ywrqa9858yqaqfqzny75nqp3anag89qh"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; No included tests
    (propagated-inputs
     `(("ruby-faraday" ,ruby-faraday)
       ("ruby-jwt" ,ruby-jwt)
       ("ruby-multi-json" ,ruby-multi-json)
       ("ruby-multi-xml" ,ruby-multi-xml)
       ("ruby-rack" ,ruby-rack)))
    (synopsis "Ruby wrapper for the OAuth 2.0")
    (description
     "This package provides a Ruby wrapper for the OAuth 2.0 protocol built
with a similar style to the original OAuth spec.")
    (home-page "https://github.com/oauth-xx/oauth2")
    (license license:expat)))

(define-public ruby-omniauth
  (package
    (name "ruby-omniauth")
    (version "1.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "omniauth" version))
       (sha256
        (base32
         "1p16h1rp8by05k8gfw17xjhgwp60dk8qmj1xalv1n23kmxfsxb1x"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; No included tests
    (propagated-inputs
     `(("ruby-hashie" ,ruby-hashie)
       ("ruby-rack" ,ruby-rack)))
    (synopsis "Generalized Rack framework for multiple-provider authentication")
    (description
     "This package provides a generalized Rack framework for multiple-provider
authentication.")
    (home-page "https://github.com/omniauth/omniauth")
    (license license:expat)))

(define-public ruby-omniauth-oauth2
  (package
    (name "ruby-omniauth-oauth2")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "omniauth-oauth2" version))
       (sha256
        (base32
         "11mi36l9d97r77q99jnafdc1yaa0a9wahhpp7dj7ank8q52g7g79"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-unnecessary-dependencies
           (lambda _
             ;; The coveralls gem submits coverage information to an online
             ;; service, and is unnecessary when running the tests
             (substitute* "Gemfile"
               ((".*coveralls\"") ""))
             (substitute* "spec/helper.rb"
               (("require \"coveralls\"") "")
               (("Coveralls::SimpleCov::Formatter") ""))
             #t)))))
    (propagated-inputs
     `(("ruby-oauth2" ,ruby-oauth2)
       ("ruby-omniauth" ,ruby-omniauth)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-simplecov" ,ruby-simplecov)
       ("ruby-rack-test" ,ruby-rack-test)
       ("ruby-webmock" ,ruby-webmock-2)))
    (synopsis "Abstract OAuth2 strategy for OmniAuth")
    (description
     "This library provides a generic OAuth2 strategy for OmniAuth.  It
doesn't provide a way to gather user information, so should be used as a
building block for authentication strategies.")
    (home-page "https://github.com/omniauth/omniauth-oauth2")
    (license license:expat)))

(define-public ruby-open4
  (package
  (name "ruby-open4")
  (version "1.3.4")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "open4" version))
      (sha256
        (base32
          "1cgls3f9dlrpil846q0w7h66vsc33jqn84nql4gcqkk221rh7px1"))))
  (build-system ruby-build-system)
  (arguments
   '(#:phases
     (modify-phases %standard-phases
       (add-after 'unpack 'patch
         (lambda _
           (substitute* "rakefile"
             ;; Update the Rakefile so it works
             (("-rubygems") "-rrubygems")
             (("Config") "RbConfig"))
           #t))
       (add-before 'check 'set-LIB
         (lambda _
           ;; This is used in the rakefile when running the tests
           (setenv "LIB" "open4")
           #t)))))
  (synopsis "Open child processes from Ruby and manage them easily")
  (description
    "@code{Open4} is a Ruby library to run child processes and manage their
input and output.")
  (home-page "https://github.com/ahoward/open4")
  (license license:ruby)))

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
             (setenv "LIB" "options")
             #t)))))
    (synopsis "Ruby library to parse options from *args cleanly")
    (description
     "The @code{options} library helps with parsing keyword options in Ruby
functions.")
    (home-page "https://github.com/ahoward/options")
    (license license:ruby)))

(define-public ruby-erubi
  (package
    (name "ruby-erubi")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "erubi" version))
       (sha256
        (base32
         "1kagnf6ziahj0d781s6ryy6fwqwa3ad4xbzzj84p9m4nv4c2jir1"))))
    (build-system ruby-build-system)
    (synopsis "ERB template engine for Ruby")
    (description
     "Erubi is a ERB template engine for Ruby.  It is a simplified fork of
Erubis")
    (home-page "https://github.com/jeremyevans/erubi")
    (license license:expat)))

(define-public ruby-erubis
  (package
    (name "ruby-erubis")
    (version "2.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "erubis" version))
       (sha256
        (base32
         "1fj827xqjs91yqsydf0zmfyw9p4l2jz5yikg3mppz6d7fi8kyrb3"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; tests do not run properly with Ruby 2.0
    (synopsis "Implementation of embedded Ruby (eRuby)")
    (description
     "Erubis is a fast implementation of embedded Ruby (eRuby) with several
features such as multi-language support, auto escaping, auto trimming spaces
around @code{<% %>}, a changeable embedded pattern, and Ruby on Rails
support.")
    (home-page "http://www.kuwata-lab.com/erubis/")
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

(define-public ruby-lino
  (package
    (name "ruby-lino")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "lino" version))
       (sha256
        (base32
         "11d29g0fk372b9fcpyr0k6hxm2b4j4igpysmi542hgbbgqgp9cd3"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; No included tests
    (propagated-inputs
     `(("ruby-hamster" ,ruby-hamster)
       ("ruby-open4" ,ruby-open4)))
    (synopsis "Build and execute commands in Ruby")
    (description
     "@code{Lino} provides an interface to run external commands.  It provides
an interface to add options as well as managing the standard input, output and
error streams.")
    (home-page "https://github.com/tobyclemson/lino")
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
    (version "1.0.13")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "lumberjack" version))
              (sha256
               (base32
                "06im7gcg42x77yhz2w5da2ly9xz0n0c36y5ks7xs53v0l9g0vf5n"))))
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

(define-public ruby-rbnacl
  (package
    (name "ruby-rbnacl")
    (version "6.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rbnacl" version))
       (sha256
        (base32
         "0ajxy5kj2jw09wdsla3jmha8w07vj5l14288xr9djpl327g3lzhn"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-unnecessary-dependencies
           (lambda _
             ;; Coveralls relates to a network service, and Rubocop to code
             ;; linting and both are unnecessary to run the tests
             (substitute* "Gemfile"
               ((".*rubocop.*") "\n")
               ((".*guard-rspec.*") "\n")
               ((".*coveralls.*") "\n"))
             (substitute* "spec/spec_helper.rb"
               (("require \"coveralls\"") "")
               (("Coveralls.wear!") ""))
             #t))
         (add-after 'unpack 'use-libsodium-from-store
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* '("lib/rbnacl/init.rb"
                            "lib/rbnacl/sodium.rb")
               (("ffi_lib \\[.+\\]")
                (string-append "ffi_lib [\""
                               (assoc-ref inputs "libsodium") "/lib/libsodium.so"
                               "\"]")))
             #t))
         ;; Run Rspec directly to avoid the Rubocop dependency in the Rakefile
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "rspec"))
             #t)))))
    (propagated-inputs
     `(("ruby-ffi" ,ruby-ffi)))
    (inputs
     `(("libsodium" ,libsodium)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)))
    (synopsis "Ruby FFI binding to libsodium")
    (description
     "This package provides Ruby FFI bindings to the Networking and
Cryptography (NaCl) library, also known as libsodium.  This provides a
high-level toolkit for building cryptographic systems and protocols.")
    (home-page "https://github.com/crypto-rb/rbnacl")
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
            (invoke "ruby" "-Ilib" "test/test.rb"))))))
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

(define-public ruby-fuubar
  (package
    (name "ruby-fuubar")
    (version "2.3.2")
    (source
     (origin
       ;; Fetch from the git repository, as the gem package doesn't include
       ;; the tests.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/thekompanee/fuubar.git")
             (commit (string-append "releases/v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0jm1x2xp13csbnadixaikj7mlkp5yk4byx51npm56zi13izp7259"))))
    (build-system ruby-build-system)
    (arguments
     '(;; TODO: Some tests fail, unsure why.
       ;; 21 examples, 7 failures
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'delete-certificate
           (lambda _
             ;; Remove 's.cert_chain' as we do not build with a private key
             (substitute* "fuubar.gemspec"
               ((".*cert_chain.*") "")
               ((".*signing_key.*") ""))
             #t))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "rspec"))
             #t)))))
    (native-inputs
     `(("bundler" ,bundler)))
    (propagated-inputs
     `(("ruby-rspec-core" ,ruby-rspec-core)
       ("ruby-progressbar" ,ruby-progressbar)))
    (synopsis "Fuubar is an RSpec formatter that uses a progress bar")
    (description
     "Fuubar is an RSpec formatter that uses a progress bar instead of a
string of letters and dots as feedback.  It also stops on the first test
failure.")
    (home-page "https://github.com/thekompanee/fuubar")
    (license license:expat)))

(define-public ruby-haml
  (package
    (name "ruby-haml")
    (version "5.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "haml" version))
       (sha256
        (base32
         "1q0a9fvqh8kn6wm97fcks6qzbjd400bv8bx748w8v87m7p4klhac"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; No included tests
    (propagated-inputs
     `(("ruby-tilt" ,ruby-tilt)
       ("ruby-temple" ,ruby-temple)))
    (synopsis "Haml is a Ruby library to generate HTML documents")
    (description
     "@acronym{Haml, HTML Abstraction Markup Language} is a layer on top of
HTML or XML that is designed to express the structure of documents using
indentation rather than closing tags.  It was originally envisioned as a
plugin for Ruby on Rails, but it can function as a stand-alone templating
engine.")
    (home-page "http://haml.info/")
    (license license:expat)))

(define-public ruby-hamster
  (package
  (name "ruby-hamster")
  (version "3.0.0")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "hamster" version))
      (sha256
        (base32
          "1n1lsh96vnyc1pnzyd30f9prcsclmvmkdb3nm5aahnyizyiy6lar"))))
  (build-system ruby-build-system)
  (arguments
   '(#:phases
     (modify-phases %standard-phases
       (add-after 'unpack 'remove-unnecessary-dependencies
         (lambda _
           ;; pry is a debugging tool, and is unnecessary when running the
           ;; tests
           (substitute* "spec/lib/hamster/vector/insert_spec.rb"
             (("require 'pry'") ""))
           (substitute* "spec/spec_helper.rb"
             (("require \"pry\"") "")
             ;; CodeClimate is an online service, and is unnecessary for
             ;; running the tests
             (("require \"codeclimate-test-reporter\"") "")
             (("CodeClimate.*\n") ""))
           #t))
       ;; No Rakefile is included, so run rspec directly.
       (replace 'check
         (lambda* (#:key tests? #:allow-other-keys)
           (when tests?
             (invoke "rspec"))
           #t)))))
  (propagated-inputs
   `(("ruby-concurrent" ,ruby-concurrent)))
  (native-inputs
   `(("ruby-rspec" ,ruby-rspec)))
  (synopsis "Efficient, immutable, thread-safe collection classes for Ruby")
  (description
    "Hamster provides 6 persistent data structures: @code{Hash}, @code{Vector},
@code{Set}, @code{SortedSet}, @code{List}, and @code{Deque} (which works as an
immutable queue or stack).")
  (home-page "https://github.com/hamstergem/hamster")
  (license license:expat)))

(define-public ruby-hashdiff
  (package
    (name "ruby-hashdiff")
    (version "0.3.8")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "hashdiff" version))
       (sha256
        (base32
         "19ykg5pax8798nh1yv71adkx0zzs7gn2rxjj86v7nsw0jba5lask"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; Run tests directly via rspec to avoid Rake issue:
         ;; NoMethodError: undefined method `last_comment'
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "rspec"))
             #t)))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec-2)))
    (synopsis "HashDiff computes the smallest difference between two hashes")
    (description
     "HashDiff is a Ruby library to compute the smallest difference between
two hashes.")
    (home-page "https://github.com/liufengyun/hashdiff")
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
             (substitute* "tests/tests_helper.rb"
               (("-rubygems") ""))
             (substitute* "Rakefile"
               (("system \"shindo") "system \"./bin/shindo")
               ;; This test doesn't work, so we disable it.
               (("fail \"The build_error test should fail") "#")
               ((" -rubygems") ""))
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

(define-public ruby-backports
  (package
  (name "ruby-backports")
  (version "3.11.4")
  (source
    (origin
      (method url-fetch)
      (uri (rubygems-uri "backports" version))
      (sha256
        (base32
          "1hshjxww2h7s0dk57njrygq4zpp0nlqrjfya7zwm27iq3rhc3y8g"))))
  (build-system ruby-build-system)
  (arguments
   '(;; TODO: This should be default, but there is one test failure
     #:test-target "all_spec"))
  (native-inputs
   `(("ruby-mspec" ,ruby-mspec)
     ("ruby-activesupport" ,ruby-activesupport)))
  (synopsis "Backports of the features in newer Ruby versions")
  (description
    "Backports enables more compatibility across Ruby versions by providing
backports of some features.")
  (home-page "https://github.com/marcandre/backports")
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

(define-public ruby-bacon-bits
  (package
    (name "ruby-bacon-bits")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "bacon-bits" version))
       (sha256
        (base32
         "1ghpj8ja94lhi8rgi872hqk4fd2amz2k7g9znd64z5dj7v6l0dmx"))))
    (build-system ruby-build-system)
    (arguments
     ;; No tests
     '(#:tests? #f))
    (propagated-inputs `(("ruby-bacon" ,ruby-bacon)))
    (synopsis "Extensions to Bacon, for disabling tests, before and after
blocks and more")
    (description
     "This extends the bacon testing framework with useful extensions to
disable tests, have before and after blocks that run once and more.")
    (home-page "https://github.com/cldwalker/bacon-bits")
    (license license:expat)))

(define-public ruby-bacon-colored-output
  (package
    (name "ruby-bacon-colored-output")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "bacon-colored_output" version))
       (sha256
        (base32
         "1znyh3vkfdlmf19p3k4zip88ibym41dn5g4p4n5hmks2iznb7qpx"))))
    (build-system ruby-build-system)
    (arguments
     '(;; No included tests
       #:tests? #f))
    (propagated-inputs
     `(("ruby-bacon" ,ruby-bacon)))
    (synopsis "Colored output for Bacon test framework")
    (description
     "This package adds color through ANSI escape codes to Bacon test
output.")
    (home-page "https://github.com/whitequark/bacon-colored_output")
    (license license:expat)))

(define-public ruby-connection-pool
  (package
    (name "ruby-connection-pool")
    (version "2.2.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "connection_pool" version))
              (sha256
               (base32
                "0lflx29mlznf1hn0nihkgllzbj8xp5qasn8j7h838465pi399k68"))))
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

(define-public ruby-powerpack
  (package
    (name "ruby-powerpack")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "powerpack" version))
       (sha256
        (base32
         "1r51d67wd467rpdfl6x43y84vwm8f5ql9l9m85ak1s2sp3nc5hyv"))))
    (build-system ruby-build-system)
    (arguments
     '(#:test-target "spec"))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-yard" ,ruby-yard)))
    (synopsis "Useful extensions to core Ruby classes")
    (description
     "This package provides a few useful extensions to core Ruby classes,
including @code{Array}, @code{Enumerable}, @code{Hash}, @code{Numeric}, and
@code{String}.")
    (home-page "https://github.com/bbatsov/powerpack")
    (license license:expat)))

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

(define-public ruby-temple
  (package
    (name "ruby-temple")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "temple" version))
       (sha256
        (base32
         "158d7ygbwcifqnvrph219p7m78yjdjazhykv5darbkms7bxm5y09"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("ruby-tilt" ,ruby-tilt)
       ("ruby-bacon" ,ruby-bacon)
       ("ruby-erubis" ,ruby-erubis)))
    (synopsis "Template compilation framework in Ruby")
    (description
     "Temple is an abstraction and framework for compiling templates to pure
Ruby.")
    (home-page "https://github.com/judofyr/temple")
    (license license:expat)))

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

(define-public ruby-test-construct
  (package
    (name "ruby-test-construct")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "test_construct" version))
       (sha256
        (base32
         "1a2ym3l068d0pxzzr95kvqx87zpdsarxslz9ygd4qfm9frrz0kgj"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-mocha" ,ruby-mocha)
       ("ruby-rspec" ,ruby-rspec)))
    (synopsis "Creates temporary files and directories for testing")
    (description
     "TestConstruct is a @acronym{DSL, Domain Specific Language} for creating
temporary files and directories during tests.")
    (home-page "https://github.com/bhb/test_construct")
    (license license:expat)))

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

(define-public ruby-markaby
  (package
    (name "ruby-markaby")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "markaby" version))
       (sha256
        (base32
         "1j4jc31ycydbkh5h3q6zwidzpavg3g5mbb5lqyaczd3jrq78rd7i"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; Run rspec manually without using the Rakefile, as the versions of
         ;; Rake and RSpec 2 are incompatible:
         ;;
         ;; NoMethodError: undefined method `last_comment'
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "rspec"))
             #t)))))
    (propagated-inputs
     `(("ruby-builder" ,ruby-builder)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec-2)))
    (synopsis "Write HTML pages in pure Ruby")
    (description
     "Markaby allows writing HTML packages in pure Ruby.  This is similar to
the functionality provided by @acronym{ERB, Embeded Ruby}, but without the
mixture of HTML and additional ERB syntax.")
    (home-page "http://markaby.github.io/")
    (license license:expat)))

(define-public ruby-maruku
  (package
    (name "ruby-maruku")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "maruku" version))
       (sha256
        (base32
         "1r7bxpgnx2hp3g12bjrmdrpv663dfqxsdp0af69kjhxmaxpia56x"))))
    (build-system ruby-build-system)
    (arguments
     '(;; TODO: 3 tests seem to fail due to HTML encoding issues
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "rspec"))
             #t)))))
    (native-inputs
     `(("ruby-rspec" ,ruby-rspec)
       ("ruby-simplecov" ,ruby-simplecov)
       ("ruby-nokogiri-diff" ,ruby-nokogiri-diff)))
    (synopsis "Markdown interpreter in Ruby")
    (description
     "Maruku is a Markdown interpreter in Ruby.  It can export Markdown to
HTML, and PDF through LaTeX.")
    (home-page "https://github.com/bhollis/maruku")
    (license license:expat)))

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
             (let* ((test-unit (assoc-ref inputs "ruby-test-unit")))
               (substitute* "Rakefile"
                 (("t\\.libs << \"test\"" line)
                  (string-append line "; t.libs << \""
                                 test-unit "/lib/ruby/vendor_ruby"
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

(define-public ruby-mspec
  (package
    (name "ruby-mspec")
    (version "1.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "mspec" version))
       (sha256
        (base32
         "0wmyh2n40m4srwdx9z6h6g6p46k02pzyhcsja3hqcw5h5b0hfmhd"))))
    (build-system ruby-build-system)
    (arguments
     '(;; TODO: 3 test failures
       ;; ./spec/mocks/mock_spec.rb:82
       ;; ./spec/utils/name_map_spec.rb:151
       ;; ./spec/utils/name_map_spec.rb:155
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'extract-gemspec 'change-dependency-constraints
           (lambda _
             (substitute* "mspec.gemspec"
               (("rake.*") "rake>)\n")
               (("rspec.*") "rspec>)\n"))
             #t))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "rspec" "spec"))
             #t)))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rake" ,ruby-rake)
       ("ruby-rspec" ,ruby-rspec)))
    (synopsis "MSpec is a specialized framework for RubySpec")
    (description
     "MSpec is a specialized framework that is syntax-compatible with RSpec 2
for basic features.  MSpec contains additional features that assist in writing
specs for Ruby implementations in ruby/spec.")
    (home-page "http://rubyspec.org")
    (license license:expat)))

(define-public ruby-mysql2
  (package
    (name "ruby-mysql2")
    (version "0.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/brianmario/mysql2.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "11lvfgc2rmvkm52jp0nbi6pvhk06klznghr7llldfw8basl9n5wv"))))
    (build-system ruby-build-system)
    (arguments
     '(;; TODO: Tests require a running MySQL/MariaDB service
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'replace-git-ls-files
           (lambda _
             (substitute* "mysql2.gemspec"
               (("git ls-files .*`") "find . -type f |sort`"))
             #t))
         (add-before 'install 'set-MAKEFLAGS
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "MAKEFLAGS"
                     (string-append
                      "V=1 "
                      "prefix=" (assoc-ref outputs "out")))
             #t))
         ;; Move the 'check phase to after 'install, as then you can test
         ;; using the installed mysql2 gem in the store.
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key outputs tests? #:allow-other-keys)
             (setenv "GEM_PATH"
                     (string-append
                      (getenv "GEM_PATH")
                      ":"
                      (assoc-ref outputs "out") "/lib/ruby/vendor_ruby"))
             (when tests?
               (invoke "rspec"))
             #t)))))
    (inputs
     `(("mariadb" ,mariadb)
       ("zlib" ,zlib)))
    (native-inputs
     `(("ruby-rspec" ,ruby-rspec)
       ("ruby-rake-compiler" ,ruby-rake-compiler)))
    (synopsis "MySQL library for Ruby, binding to libmysql")
    (description
     "This package provides a simple, fast MySQL library for Ruby, binding to
libmysql.")
    (home-page "https://github.com/brianmario/mysql2")
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
          (lambda _ (invoke "rspec" "spec/"))))))
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

(define-public ruby-bond
  (package
    (name "ruby-bond")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "bond" version))
       (sha256
        (base32
         "1r19ifc4skyl2gxnifrxa5jvbbay9fb2in79ppgv02b6n4bhsw90"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("ruby-bacon" ,ruby-bacon)
       ("ruby-bacon-bits" ,ruby-bacon-bits)
       ("ruby-mocha-on-bacon" ,ruby-mocha-on-bacon)))
    (synopsis "Bond can provide custom autocompletion for arguments, methods
and more")
    (description
     "Bond can autocomplete argument(s) to methods, uniquely completing per
module, per method and per argument.  Bond provides a configuration system and
a DSL for creating custom completions and completion rules.  Bond can also
load completions that ship with gems.  Bond is able to offer more than irb's
completion since it uses the full line of input when completing as opposed to
irb's last-word approach.")
    (home-page "http://tagaholic.me/bond/")
    (license license:expat)))

(define-public ruby-idn-ruby
  (package
    (name "ruby-idn-ruby")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "idn-ruby" version))
       (sha256
        (base32
         "07vblcyk3g72sbq12xz7xj28snpxnh3sbcnxy8bglqbfqqhvmawr"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key tests? outputs #:allow-other-keys)
             (when tests?
               (let* ((gem-file (cadr (find-files "." "\\.gem")))
                      (name-and-version (basename gem-file ".gem")))
                 (apply invoke
                        "ruby" "--verbose"
                        (string-append "-I"
                                       (assoc-ref outputs "out")
                                       "/lib/ruby/vendor_ruby/gems/"
                                       name-and-version
                                       "/lib")
                        (find-files "./test" ".*\\.rb"))))
             #t)))))
    (inputs
     `(("libidn" ,libidn)))
    (synopsis "Ruby Bindings for the GNU LibIDN library")
    (description
     "Ruby Bindings for the GNU LibIDN library, an implementation of the
Stringprep, Punycode and IDNA specifications.  These are used to encode and
decode internationalized domain + names according to the IDNA2003
specifications.

Included are the most important parts of the Stringprep, Punycode and IDNA
APIs like performing Stringprep processings, encoding to and decoding from
Punycode strings and converting entire domain names to and from the ACE
encoded form.")
    (home-page "https://github.com/deepfryed/idn-ruby")
    (license license:asl2.0)))

(define-public ruby-instantiator
  (package
    (name "ruby-instantiator")
    (version "0.0.7")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "instantiator" version))
              (sha256
               (base32
                "0w07w3gkyqr7m0vz5h13vm8b411660qywjm2xxxgdjv4wb3fazbr"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'add-test-unit-to-search-path
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((test-unit (assoc-ref inputs "ruby-test-unit")))
               (substitute* "Rakefile"
                 (("t\\.libs << \"test\"" line)
                  (string-append line "; t.libs << \""
                                 test-unit "/lib/ruby/vendor_ruby"
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
            (let* ((test-unit (assoc-ref inputs "ruby-test-unit")))
              (substitute* "Rakefile"
                (("t\\.libs << \"test\"" line)
                 (string-append line "; t.libs << \""
                                test-unit "/lib/ruby/vendor_ruby"
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
            (let* ((test-unit (assoc-ref inputs "ruby-test-unit")))
              (substitute* "Rakefile"
                (("t\\.libs << 'test'" line)
                 (string-append line "; t.libs << \""
                                test-unit "/lib/ruby/vendor_ruby"
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

(define-public ruby-mocha-on-bacon
  (package
    (name "ruby-mocha-on-bacon")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "mocha-on-bacon" version))
       (sha256
        (base32
         "1h49b33rq889hn8x3wp9byczl91va16jh1w4d2wyy4yj23icdrcp"))))
    (build-system ruby-build-system)
    (arguments
     ;; rubygems.org release missing tests
     '(#:tests? #f))
    (propagated-inputs `(("ruby-mocha" ,ruby-mocha)))
    (synopsis "Mocha adapter for Bacon")
    (description
     "This package provides a Mocha adapter for Bacon, allowing you to use the
Mocha stubbing and mocking library with Bacon, a small RSpec clone.")
    (home-page
     "https://github.com/alloy/mocha-on-bacon")
    (license license:expat)))

(define-public ruby-net-ssh
  (package
    (name "ruby-net-ssh")
    (version "4.2.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "net-ssh" version))
              (sha256
               (base32
                "07c4v97zl1daabmri9zlbzs6yvkl56z1q14bw74d53jdj0c17nhx"))))
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

(define-public ruby-net-scp
  (package
    (name "ruby-net-scp")
    ;; The 1.2.1 release would be incompatible with ruby-net-ssh >= 4.
    (version "1.2.2.rc2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/net-ssh/net-scp/archive/v"
                           version ".tar.gz"))
       (sha256
        (base32
         "0xyf17mhgvyz54xjj9ria4wnq3x62bhmkfgzqv8jwiip2bplv1nk"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-test-unit" ,ruby-test-unit)
       ("ruby-mocha" ,ruby-mocha)))
    (propagated-inputs
     `(("ruby-net-ssh" ,ruby-net-ssh)))
    (synopsis "Pure-Ruby SCP client library")
    (description "@code{Net::SCP} is a pure-Ruby implementation of the SCP
client protocol.")
    (home-page "https://github.com/net-ssh/net-scp")
    (license license:expat)))

(define-public ruby-minitest
  (package
    (name "ruby-minitest")
    (version "5.11.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "minitest" version))
              (sha256
               (base32
                "0icglrhghgwdlnzzp4jf76b0mbc71s80njn5afyfjn4wqji8mqbq"))))
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

(define-public ruby-minitest-around
  (package
    (name "ruby-minitest-around")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "minitest-around" version))
       (sha256
        (base32
         "15ywnqx0719jl9c25yqfshmwcir57i5f4hr1ra9v9vay9ylcwndr"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'extract-gemspec 'remove-unnecessary-dependency-versions
           (lambda _
             (substitute* "minitest-around.gemspec"
               (("%q<cucumber>.*") "%q<cucumber>, [\">= 0\"])\n"))
             #t)))))
    (propagated-inputs
     `(("ruby-minitest" ,ruby-minitest)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-cucumber" ,ruby-cucumber)
       ("ruby-bump" ,ruby-bump)
       ("ruby-test-construct" ,ruby-test-construct)))
    (synopsis "Run code around tests in Minitest")
    (description
     "This library provides a way to run code around tests in Minitest,
written using either the unit test or spec style.")
    (home-page "https://github.com/splattael/minitest-around")
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
               (invoke "script/test"))))))
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

(define-public ruby-minitest-reporters
  (package
    (name "ruby-minitest-reporters")
    (version "1.3.6")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "minitest-reporters" version))
       (sha256
        (base32
         "1a3das80rwgys5rj48i5ly144nvszyqyi748bk9bss74jblcf5ay"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; Remove the requirement on Rubocop, as it isn't useful to run, and
         ;; including it as an input can lead to circular dependencies.
         (add-after 'unpack 'remove-rubocop-from-Rakefile
           (lambda _
             (substitute* "Rakefile"
               (("require 'rubocop/rake\\_task'") "")
               (("RuboCop::RakeTask\\.new\\(:rubocop\\)") "[].each"))
             #t))
         (add-after 'extract-gemspec 'remove-rubocop-from-gemspec
           (lambda _
             (substitute* "minitest-reporters.gemspec"
               ((".*%q<rubocop>.*") "\n"))
             #t)))))
    (propagated-inputs
     `(("ruby-ansi" ,ruby-ansi)
       ("ruby-builder" ,ruby-builder)
       ("ruby-minitest" ,ruby-minitest)
       ("ruby-progressbar" ,ruby-progressbar)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-maruku" ,ruby-maruku)))
    (synopsis "Enhanced reporting for Minitest tests")
    (description
     "@code{minitest/reporters} provides a custom Minitest runner to improve
how the test state is reported.  A number of different reporters are
available, including a spec reporter, progress bar reporter, a HTML
reporter.")
    (home-page "https://github.com/kern/minitest-reporters")
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
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "minitest-hooks" version))
       (sha256
        (base32
         "0lnpvzijbjrvxjc43d155jnbk2mkfshrz22an711wh004scavlzc"))))
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

(define-public ruby-data_uri
  (package
    (name "ruby-data_uri")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "data_uri" version))
       (sha256
        (base32
         "0fzkxgdxrlbfl4537y3n9mjxbm28kir639gcw3x47ffchwsgdcky"))))
    (build-system ruby-build-system)
    (synopsis "URI class for parsing data URIs")
    (description
     "Data @acronym{URI, universal resource idenfitier}s allow resources to be
embedded inside a URI.  The URI::Data class provides support for parsing these
URIs using the normal URI.parse method.")
    (home-page "https://github.com/dball/data_uri")
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
                            (config (string-append
                                     (assoc-ref outputs "out")
                                     "/lib/ruby/vendor_ruby/gems/git-"
                                     ,version "/lib/git/config.rb")))
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

(define-public ruby-multi-xml
  (package
    (name "ruby-multi-xml")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "multi_xml" version))
       (sha256
        (base32
         "0lmd4f401mvravi1i1yq7b2qjjli0yq7dfc4p1nj5nwajp7r6hyj"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; No included tests
    (synopsis "Swappable XML backends for Ruby")
    (description
     "@code{MultiXml} provides swappable XML backends utilizing either LibXML,
Nokogiri, Ox, or REXML.")
    (home-page "https://github.com/sferik/multi_xml")
    (license license:expat)))

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
    (version "1.13.1")
    (source
     (origin
       (method url-fetch)
       ;; Tests are not distributed at rubygems.org so download from GitHub
       ;; instead.
       (uri (string-append "https://github.com/intridea/multi_json/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1s64xqvrnrxmb59v6b2kchnisawg5ai9ky1w60dy6z6ws9la1xv4"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-signing-key-reference
           (lambda _
             (substitute* "multi_json.gemspec"
               ((".*spec.signing_key.*") ""))
             #t)))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-yard" ,ruby-yard)
       ("ruby-json-pure" ,ruby-json-pure)
       ("ruby-oj" ,ruby-oj)
       ("ruby-yajl-ruby" ,ruby-yajl-ruby)))
    (synopsis "Common interface to multiple JSON libraries for Ruby")
    (description
     "This package provides a common interface to multiple JSON libraries,
including Oj, Yajl, the JSON gem (with C-extensions), the pure-Ruby JSON gem,
NSJSONSerialization, gson.rb, JrJackson, and OkJson.")
    (home-page "https://github.com/intridea/multi_json")
    (license license:expat)))

(define-public ruby-multi-test
  (package
    (name "ruby-multi-test")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "multi_test" version))
       (sha256
        (base32
         "1sx356q81plr67hg16jfwz9hcqvnk03bd9n75pmdw8pfxjfy1yxd"))))
    (build-system ruby-build-system)
    (arguments
     '(;; Tests require different sets of specific gem versions to be available,
       ;; and there is no gemfile that specifies the newest versions of
       ;; dependencies to be tested.
       #:tests? #f))
    (synopsis
     "Interface to testing libraries loaded into a running Ruby process")
    (description
     "@code{multi_test} provides a uniform interface onto whatever testing
libraries that have been loaded into a running Ruby process to help control
rogue test/unit/autorun requires.")
    (home-page "https://github.com/cucumber/multi_test")
    (license license:expat)))

(define-public ruby-arel
  (package
    (name "ruby-arel")
    (version "9.0.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "arel" version))
              (sha256
               (base32
                "1jk7wlmkr61f6g36w9s2sn46nmdg6wn2jfssrhbhirv5x9n95nk0"))))
    (build-system ruby-build-system)
    (arguments '(#:tests? #f)) ; no tests
    (home-page "https://github.com/rails/arel")
    (synopsis "SQL AST manager for Ruby")
    (description "Arel is an SQL @dfn{Abstract Syntax Tree} (AST) manager for
Ruby.  It simplifies the generation of complex SQL queries and adapts to
various relational database implementations.")
    (license license:expat)))

(define-public ruby-marcel
  (package
    (name "ruby-marcel")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "marcel" version))
       (sha256
        (base32
         "1nxbjmcyg8vlw6zwagf17l9y2mwkagmmkg95xybpn4bmf3rfnksx"))))
    (build-system ruby-build-system)
    (arguments
     '(;; No included tests
       #:tests? #f))
    (propagated-inputs
     `(("ruby-mimemagic" ,ruby-mimemagic)))
    (synopsis "MIME type detection using magic numbers, filenames and extensions")
    (description
     "@code{marcel} provides @acronym{MIME, Multipurpose Internet Mail
Extensions} type detection using magic numbers, filenames, and extensions")
    (home-page "https://github.com/basecamp/marcel")
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
             (let* ((pkg-config (assoc-ref inputs "ruby-pkg-config")))
               (substitute* "ext/nokogiri/extconf.rb"
                 (("gem 'pkg-config'.*")
                  (string-append "$:.unshift '"
                                 pkg-config "/lib/ruby/vendor_ruby"
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

(define-public ruby-parser
  (package
    (name "ruby-parser")
    (version "2.6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "parser" version))
       (sha256
        (base32
         "1hhz2k5417vr2k1llwqgjdnmyrhlpqicy0y2arr6r1gp04fg9wlm"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-cliver" ,ruby-cliver)
       ("ruby-simplecov" ,ruby-simplecov)
       ("ruby-racc" ,ruby-racc)))
    (inputs
     `(("ragel" ,ragel)))
    (propagated-inputs
     `(("ruby-ast" ,ruby-ast)))
    (synopsis "Ruby parser written in pure Ruby")
    (description
     "This package provides a Ruby parser written in pure Ruby.")
    (home-page "https://github.com/whitequark/parser")
    (license license:expat)))

(define-public ruby-prawn-manual-builder
  (package
    (name "ruby-prawn-manual-builder")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "prawn-manual_builder" version))
       (sha256
        (base32
         "0wbjnkqp55p5wmz85ldypcray223glckd209hmdxhnzk8s5pb3za"))))
    (build-system ruby-build-system)
    (arguments
     '(;; No included tests
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'extract-gemspec 'patch-gemspec
           (lambda _
             (substitute* ".gemspec"
               ;; Loosen the requirement for pdf-inspector
               (("~> 1\\.0\\.7") ">= 0")))))))
    (propagated-inputs
     `(("ruby-coderay" ,ruby-coderay)))
    (synopsis "Tool for writing manuals for Prawn and Prawn accessories")
    (description
     "This package provides a tool for writing manuals for Prawn and Prawn
accessories")
    (home-page "https://github.com/prawnpdf/prawn-manual_builder")
    (license (list
              ;; GPLv2 or GPLv3 or custom license described in LICENSE file
              license:gpl2
              license:gpl3))))

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

(define-public ruby-progressbar
  (package
    (name "ruby-progressbar")
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "ruby-progressbar" version))
       (sha256
        (base32
         "1cv2ym3rl09svw8940ny67bav7b2db4ms39i4raaqzkf59jmhglk"))))
    (build-system ruby-build-system)
    (arguments
     '(;; TODO: There looks to be a circular dependency with ruby-fuubar.
       #:tests? #f))
    (synopsis "Text progress bar library for Ruby")
    (description
     "Ruby/ProgressBar is an flexible text progress bar library for Ruby.
The output can be customized with a formatting system.")
    (home-page "https://github.com/jfelchner/ruby-progressbar")
    (license license:expat)))

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
            (invoke "gem" "build" "guard.gemspec"))))))
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

(define-public ruby-tilt
  (package
    (name "ruby-tilt")
    (version "2.0.9")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "tilt" version))
       (sha256
        (base32
         "0ca4k0clwf0rkvy7726x4nxpjxkpv67w043i39saxgldxd97zmwz"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-some-dependencies
           (lambda _
             (substitute* "Gemfile"
               ;; TODO ronn is used for generating the manual
               (("gem 'ronn'.*") "\n")
               ;; ruby-haml has a runtime dependency on ruby-tilt, so don't
               ;; pass it in as a native-input
               (("gem 'haml'.*") "\n")
               ;; TODO Not all of these gems are packaged for Guix yet:
               ;; less, coffee-script, livescript, babel-transpiler,
               ;; typescript-node
               (("if can_execjs") "if false")
               ;; Disable the secondary group to reduce the number of
               ;; dependencies. None of the normal approaches work, so patch
               ;; the Gemfile instead.
               (("group :secondary") "[].each"))
             #t)))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-yard" ,ruby-yard)
       ("ruby-builder" ,ruby-builder)
       ("ruby-erubis" ,ruby-erubis)
       ("ruby-markaby" ,ruby-markaby)
       ("ruby-sassc" ,ruby-sassc)))
    (synopsis "Generic interface to multiple Ruby template engines")
    (description
     "Tilt is a thin interface over a number of different Ruby template
engines in an attempt to make their usage as generic as possible.")
    (home-page "https://github.com/rtomayko/tilt/")
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
            (invoke "gem" "build" "rb-inotify.gemspec"))))))
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
            (invoke "gem" "build" "tins.gemspec"))))))
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
            (invoke "gem" "build" "gem_hadar.gemspec"))))))
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
             (let* ((minitest (assoc-ref inputs "ruby-minitest-4")))
               (substitute* "Rakefile"
                 (("Hoe\\.add_include_dirs .*")
                  (string-append "Hoe.add_include_dirs \""
                                 minitest "/lib/ruby/vendor_ruby"
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
         (add-after 'unpack 'fix-test
           (lambda -
             (substitute* "tests/hsl_triple_test.rb"
               (("0\\\\\\.0%")
                "0\\.?0?%"))))
         (replace 'build
          (lambda _
            (invoke "gem" "build" "term-ansicolor.gemspec"))))))
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

(define-public ruby-terraform
  (package
  (name "ruby-terraform")
  (version "0.22.0")
  (source
   (origin
     (method url-fetch)
     (uri (rubygems-uri "ruby-terraform" version))
     (sha256
      (base32
       "13zjkp71cd19j2ds2h9rqwcfr1zdg5nsh63p89l6qcsc9z39z324"))))
  (build-system ruby-build-system)
  (arguments
   '(#:tests? #f)) ; No included tests
  (propagated-inputs
   `(("ruby-lino" ,ruby-lino)))
  (synopsis "Ruby wrapper around the Terraform command line interface")
  (description
   "This package provides a Ruby wrapper around the Terraform command line
interface so that Terraform can be more easily invoked from Ruby code.")
  (home-page "https://github.com/infrablocks/ruby_terraform")
  (license license:expat)))

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

(define-public ruby-jaro-winkler
  (package
    (name "ruby-jaro-winkler")
    (version "1.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "jaro_winkler" version))
       (sha256
        (base32
         "1zz27z88qznix4r65gd9h56gl177snlfpgv10b0s69vi8qpl909l"))))
    (build-system ruby-build-system)
    (arguments
     '(;; No included tests
       #:tests? #f))
    (synopsis "Ruby implementation of Jaro-Winkler distance algorithm")
    (description
     "@code{jaro_winkler} is an implementation of Jaro-Winkler distance
algorithm.  It is written as a C extension and will fallback to a pure Ruby
implementation on platforms where this is unsupported.")
    (home-page "https://github.com/tonytonyjan/jaro_winkler")
    (license license:expat)))

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
             (invoke "rake" "gemspec")))
         (add-after 'regenerate-gemspec 'fix-json-java.gemspec
           (lambda _
             ;; This gemspec doesn't look to be generated by the above
             ;; command, so patch it separately.
             (substitute* "json-java.gemspec"
               (("%q<test-unit>\\.freeze, \\[\"~> 2\\.0\"\\]")
                "%q<test-unit>.freeze, [\">= 2.0\"]"))
             #t)))))
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

(define-public ruby-jwt
  (package
    (name "ruby-jwt")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "jwt" version))
       (sha256
        (base32
         "1w0kaqrbl71cq9sbnixc20x5lqah3hs2i93xmhlfdg2y3by7yzky"))))
    (build-system ruby-build-system)
    (arguments
     '(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-unnecessary-dependencies
           (lambda _
             (substitute* "spec/spec_helper.rb"
               (("require 'simplecov.*") "\n")
               ;; Use [].each to disable running the SimpleCov configuration
               ;; block
               (("SimpleCov\\.configure") "[].each")
               (("require 'codeclimate-test-reporter'") "")
               (("require 'codacy-coverage'") "")
               (("Codacy::Reporter\\.start") ""))
             #t)))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-rbnacl" ,ruby-rbnacl)))
    (synopsis "Ruby implementation of the JSON Web Token standard")
    (description
     "This package provides a pure Ruby implementation of the RFC 7519 OAuth
@acronym{JWT, JSON Web Token} standard.")
    (home-page "https://github.com/jwt/ruby-jwt")
    (license license:expat)))

;; Even though this package only provides bindings for a Mac OSX API it is
;; required by "ruby-listen" at runtime.
(define-public ruby-rb-fsevent
  (package
    (name "ruby-rb-fsevent")
    (version "0.10.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rb-fsevent" version))
              (sha256
               (base32
                "1lm1k7wpz69jx7jrc92w3ggczkjyjbfziq5mg62vjnxmzs383xx8"))))
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

(define-public ruby-listen-3.0
  (package
    (inherit ruby-listen)
    (version "3.0.8")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "listen" version))
              (sha256
               (base32
                "1l0y7hbyfiwpvk172r28hsdqsifq1ls39hsfmzi1vy4ll0smd14i"))))))

(define-public ruby-loofah
  (package
    (name "ruby-loofah")
    (version "2.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "loofah" version))
       (sha256
        (base32
         "1ccsid33xjajd0im2xv941aywi58z7ihwkvaf1w2bv89vn5bhsjg"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-unnecessary-dependencies
           (lambda _
             ;; concourse is a development tool which is unused, so remove it
             ;; so it's not required.
             (substitute* "Gemfile"
               ((".*\"concourse\".*") "\n"))
             (substitute* "Rakefile"
               (("require 'concourse'") "")
               (("Concourse\\.new.*") "\n"))
             #t)))))
    (native-inputs
     `(("ruby-hoe" ,ruby-hoe)
       ("ruby-rr" ,ruby-rr)))
    (propagated-inputs
     `(("ruby-nokogiri" ,ruby-nokogiri)
       ("ruby-crass" ,ruby-crass)))
    (synopsis "Ruby library for manipulating and transforming HTML/XML")
    (description
     "Loofah is a general library for manipulating and transforming HTML/XML
documents and fragments.  It's built on top of Nokogiri and libxml2.")
    (home-page "https://github.com/flavorjones/loofah")
    (license license:expat)))

(define-public ruby-activesupport
  (package
    (name "ruby-activesupport")
    (version "5.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "activesupport" version))
       (sha256
        (base32
         "1iya7vxqwxysr74s7b4z1x19gmnx5advimzip3cbmsd5bd43wfgz"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             ;; There are no tests, instead attempt to load the library.
             (invoke "ruby" "-Ilib" "-r" "active_support"))))))
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
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "crass" version))
              (sha256
               (base32
                "0bpxzy6gjw9ggjynlxschbfsgmx8lv3zw1azkjvnb8b9i895dqfi"))))
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
    (version "4.6.3")
    (source (origin
              (method url-fetch)
              ;; The gem does not include the Rakefile, so we download the
              ;; release tarball from Github.
              (uri (string-append "https://github.com/rgrove/"
                                  "sanitize/archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1fmqppwif3cm8h79006jfzkdnlxxzlry9kzk03psk0d5xpg55ycc"))))
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

(define-public ruby-oj
  (package
    (name "ruby-oj")
    (version "3.6.7")
    (source
     (origin
       (method url-fetch)
       ;; Version on rubygems.org does not contain Rakefile, so download from
       ;; GitHub instead.
       (uri (string-append "https://github.com/ohler55/oj/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1x28ga72jxlnmsd8g8c0fw81vlh54r0qgagw2lxsd3x3la091g2h"))))
    (build-system ruby-build-system)
    (arguments
     '(#:test-target "test_all"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'disable-bundler
           (lambda _
             (substitute* "Rakefile"
               (("Bundler\\.with_clean_env") "1.times")
               (("bundle exec ") "")))))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-rake-compiler" ,ruby-rake-compiler)))
    (synopsis "JSON parser for Ruby optimized for speed")
    (description
     "Oj is a JSON parser and generator for Ruby, where the encoding and
decoding of JSON is implemented as a C extension to Ruby.")
    (home-page "http://www.ohler.com/oj")
    (license (list license:expat     ; Ruby code
                   license:bsd-3)))) ; extension code

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
            (invoke "gem" "build" "redcloth.gemspec"))))))
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
    (version "1.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "pg" version))
       (sha256
        (base32
         "1pnjw3rspdfjssxyf42jnbsdlgri8ylysimp0s28wxb93k6ff2qb"))))
    (build-system ruby-build-system)
    (arguments
     '(#:test-target "spec"))
    (native-inputs
     `(("ruby-rake-compiler" ,ruby-rake-compiler)
       ("ruby-hoe" ,ruby-hoe)
       ("ruby-rspec" ,ruby-rspec)))
    (inputs
     `(("postgresql" ,postgresql-9.6)))
    (synopsis "Ruby interface to PostgreSQL")
    (description "Pg is the Ruby interface to the PostgreSQL RDBMS.  It works
with PostgreSQL 9.0 and later.")
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
             (map (lambda (file)
                    (invoke "ruby" "-Itest" file))
                  (find-files "./test" "test_.*\\.rb")))))))
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
           (lambda _ (invoke "rake" "compile:unf_ext"))))))
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

(define-public ruby-racc
  (package
    (name "ruby-racc")
    (version "1.4.14")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "racc" version))
       (sha256
        (base32
         "00yhs2ag7yy5v83mqvkbnhk9bvsh6mx3808k53n61ddzx446v1zl"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("ruby-hoe" ,ruby-hoe)
       ("ruby-rake-compiler" ,ruby-rake-compiler)))
    (synopsis "LALR(1) parser generator for Ruby")
    (description
     "Racc is a LALR(1) parser generator.  It is written in Ruby itself, and
generates Ruby program.")
    (home-page "http://i.loveruby.net/en/projects/racc/")
    (license (list
              ;; Generally licensed under the LGPL2.1, and some files also
              ;; available under the same license as Ruby.
              license:lgpl2.1
              license:ruby))))

(define-public ruby-rack
  (package
    (name "ruby-rack")
    (version "2.0.6")
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
         "0pb3g5ymvbf07xaxcn51dpqv3djlxavckp3qkxsjdxiqznb0d9p1"))
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

(define-public ruby-rack-test
  (package
    (name "ruby-rack-test")
    (version "0.8.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rack-test" version))
       (sha256
        (base32
         "14ij39zywvr1i9f6jsixfg4zxi2q1m1n1nydvf47f0b6sfc9mv1g"))))
    (build-system ruby-build-system)
    (arguments
     ;; Disable tests because of circular dependencies: requires sinatra,
     ;; which requires rack-protection, which requires rack-test.  Instead
     ;; simply require the library.
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "ruby" "-Ilib" "-r" "rack/test"))))))
    (propagated-inputs
     `(("ruby-rack" ,ruby-rack)))
    (synopsis "Testing API for Rack applications")
    (description
     "Rack::Test is a small, simple testing API for Rack applications.  It can
be used on its own or as a reusable starting point for Web frameworks and
testing libraries to build on.")
    (home-page "https://github.com/rack-test/rack-test")
    (license license:expat)))

(define-public ruby-rack-protection
  (package
    (name "ruby-rack-protection")
    (version "2.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rack-protection" version))
       (sha256
        (base32
         "15167q25rmxipqwi6hjqj3i1byi9iwl3xq9b7mdar7qiz39pmjsk"))))
    (build-system ruby-build-system)
    (arguments
     '(;; Tests missing from the gem.
       #:tests? #f))
    (propagated-inputs
     `(("ruby-rack" ,ruby-rack)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec-2)
       ("ruby-rack-test" ,ruby-rack-test)))
    (synopsis "Rack middleware that protects against typical web attacks")
    (description "Rack middleware that can be used to protect against typical
web attacks.  It can protect all Rack apps, including Rails.  For instance, it
protects against cross site request forgery, cross site scripting,
clickjacking, directory traversal, session hijacking and IP spoofing.")
    (home-page "https://github.com/sinatra/sinatra/tree/master/rack-protection")
    (license license:expat)))

(define-public ruby-rainbow
  (package
    (name "ruby-rainbow")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rainbow" version))
       (sha256
        (base32
         "0bb2fpjspydr6x0s8pn1pqkzmxszvkfapv0p4627mywl7ky4zkhk"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; Run rspec directly, to avoid requiring Rubocop which is used from
         ;; the Rakefile.
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "rspec"))
             #t)))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)))
    (synopsis "Colorize printed text on ANSI terminals")
    (description
     "@code{rainbow} provides a string presenter object to colorize strings by
wrapping them in ANSI escape codes.")
    (home-page "https://github.com/sickill/rainbow")
    (license license:expat)))

(define-public ruby-rr
  (package
    (name "ruby-rr")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rr" version))
       (sha256
        (base32
         "1n9g78ba4c2zzmz8cdb97c38h1xm0clircag00vbcxwqs4dq0ymp"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; test files not included
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)))
    (synopsis "Ruby test double framework")
    (description
     "RR is a test double framework that features a rich selection of double
techniques and a terse syntax.")
    (home-page "https://rr.github.io/rr/")
    (license license:expat)))

(define-public ruby-rest-client
  (package
    (name "ruby-rest-client")
    (version "2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rest-client" version))
       (sha256
        (base32
         "1hzcs2r7b5bjkf2x2z3n8z6082maz0j8vqjiciwgg3hzb63f958j"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'remove-unnecessary-development-dependencies
           (lambda _
             (substitute* "rest-client.gemspec"
               ;; Remove rubocop as it's unused. Rubocop also indirectly
               ;; depends on this package through ruby-parser and ruby-ast so
               ;; this avoids a dependency loop.
               ((".*rubocop.*") "\n")
               ;; Remove pry as it's unused, it's a debugging tool
               ((".*pry.*") "\n")
               ;; Remove an unnecessarily strict rdoc dependency
               ((".*rdoc.*") "\n"))
             #t))
         (add-before 'check 'delete-network-dependent-tests
           (lambda _
             (delete-file "spec/integration/request_spec.rb")
             (delete-file "spec/integration/httpbin_spec.rb")
             #t)))))
    (propagated-inputs
     `(("ruby-http-cookie" ,ruby-http-cookie)
       ("ruby-mime-types" ,ruby-mime-types)
       ("ruby-netrc" ,ruby-netrc)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-webmock", ruby-webmock-2)
       ("ruby-rspec", ruby-rspec)))
    (synopsis "Simple HTTP and REST client for Ruby")
    (description
     "@code{rest-client} provides a simple HTTP and REST client for Ruby,
inspired by the Sinatra microframework style of specifying actions:
@code{get}, @code{put}, @code{post}, @code{delete}.")
    (home-page "https://github.com/rest-client/rest-client")
    (license license:expat)))

(define-public ruby-rubocop
  (package
    (name "ruby-rubocop")
    (version "0.64.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rubocop" version))
       (sha256
        (base32
         "07shi6kncwhkvlh9ci9rgccrjsq4448hbic3yrakh2w65ppynvbj"))))
    (build-system ruby-build-system)
    (arguments
     '(;; No included tests
       #:tests? #f))
    (propagated-inputs
     `(("ruby-parser" ,ruby-parser)
       ("ruby-powerpack" ,ruby-powerpack)
       ("ruby-rainbow" ,ruby-rainbow)
       ("ruby-progressbar" ,ruby-progressbar)
       ("ruby-parallel" ,ruby-parallel)
       ("ruby-jaro-winkler" ,ruby-jaro-winkler)
       ("ruby-unicode-display-width" ,ruby-unicode-display-width)))
    (synopsis "Ruby code style checking tool")
    (description
     "@code{rubocop} is a Ruby code style checking tool.  It aims to enforce
the community-driven Ruby Style Guide.")
    (home-page "https://github.com/rubocop-hq/rubocop")
    (license license:expat)))

(define-public ruby-contest
  (package
    (name "ruby-contest")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "contest" version))
       (sha256
        (base32
         "1p9f2292b7b0fbrcjswvj9v01z7ig5ig52328wyqcabgb553qsdf"))))
    (build-system ruby-build-system)
    (synopsis "Write declarative tests using nested contexts")
    (description
     "Contest allows writing declarative @code{Test::Unit} tests using nested
contexts without performance penalties.")
    (home-page "https://github.com/citrusbyte/contest")
    (license license:expat)))

(define-public ruby-creole
  (package
    (name "ruby-creole")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "creole" version))
       (sha256
        (base32
         "00rcscz16idp6dx0dk5yi5i0fz593i3r6anbn5bg2q07v3i025wm"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("ruby-bacon" ,ruby-bacon)))
    (synopsis "Creole markup language converter")
    (description
     "Creole is a lightweight markup language and this library for converting
creole to @code{HTML}.")
    (home-page "https://github.com/minad/creole")
    (license license:ruby)))

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
    (version "5.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "gherkin" version))
        (sha256
          (base32
            "1cgcdchwwdm10rsk44frjwqd4ihprhxjbm799nscqy2q1raqfj5s"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("bundler" ,bundler)))
    (arguments
     '(#:tests? #f)) ; needs simplecov, among others
    (synopsis "Gherkin parser for Ruby")
    (description "Gherkin is a parser and compiler for the Gherkin language.
It is intended be used by all Cucumber implementations to parse
@file{.feature} files.")
    (home-page "https://github.com/cucumber-attic/gherkin")
    (license license:expat)))

(define-public ruby-aruba
  (package
    (name "ruby-aruba")
    (version "0.14.8")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "aruba" version))
       (sha256
        (base32
         "0zdd81l1lp0x78sxa6kkfqclpj5il3xl70nz05wqv2sfzzhqydxh"))))
    (build-system ruby-build-system)
    (arguments
     '(#:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch
           (lambda _
             (substitute* "spec/aruba/api_spec.rb"
               ;; This resolves some errors in the specs
               ;;
               ;; undefined method `parse' for Time:Class
               (("require 'spec_helper'")
                "require 'spec_helper'\nrequire 'time'"))
             ;; Avoid shebang issues in this spec file
             (substitute* "spec/aruba/matchers/command_spec.rb"
               (("/usr/bin/env bash")
                (which "bash")))
             #t))
         (add-before 'check 'remove-unnecessary-dependencies
           (lambda _
             (substitute* "Gemfile"
               ((".*byebug.*") "\n")
               ((".*pry.*") "\n")
               ((".*yaml.*") "\n")
               ((".*bcat.*") "\n")
               ((".*kramdown.*") "\n")
               ((".*rubocop.*") "\n")
               ((".*cucumber-pro.*") "\n")
               ((".*cucumber.*") "\n")
               ((".*license_finder.*") "\n")
               ((".*rake.*") "gem 'rake'\n")
               ((".*simplecov.*") "\n")
               ((".*relish.*") "\n"))
             (substitute* "spec/spec_helper.rb"
               ((".*simplecov.*") "")
               (("^SimpleCov.*") ""))
             (substitute* "aruba.gemspec"
               (("spec\\.add\\_runtime\\_dependency 'cucumber'.*")
                "spec.add_runtime_dependency 'cucumber'"))
             #t))
         (add-before 'check 'set-home
           (lambda _ (setenv "HOME" "/tmp") #t)))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-fuubar" ,ruby-fuubar)))
    (propagated-inputs
     `(("ruby-childprocess" ,ruby-childprocess)
       ("ruby-contracts" ,ruby-contracts)
       ("ruby-cucumber" ,ruby-cucumber)
       ("ruby-ffi" ,ruby-ffi)
       ("ruby-rspec-expectations" ,ruby-rspec-expectations)
       ("ruby-thor" ,ruby-thor)
       ("ruby-yard" ,ruby-yard)))
    (synopsis "Test command-line applications with Cucumber, RSpec or Minitest")
    (description
     "Aruba is an extension for Cucumber, RSpec and Minitest for testing
command-line applications.  It supports applications written in any
language.")
    (home-page "https://github.com/cucumber/aruba")
    (license license:expat)))

;; A version of ruby-aruba without tests run so that circular dependencies can
;; be avoided.
(define ruby-aruba-without-tests
  (package
    (inherit ruby-aruba)
    (arguments '(#:tests? #f))
    (propagated-inputs
     `(("ruby-cucumber" ,ruby-cucumber-without-tests)
       ,@(alist-delete "ruby-cucumber"
                       (package-propagated-inputs ruby-aruba))))
    (native-inputs '())))

(define-public ruby-cucumber
  (package
    (name "ruby-cucumber")
    (version "3.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cucumber/cucumber-ruby.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0764wp2cjg60qa3l69q1dxda5g06a01n5w92szqbf89d2hgl47n3"))))
    (build-system ruby-build-system)
    (arguments
     '(#:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         ;; Don't run or require rubocop, the code linting tool, as this is a
         ;; bit unnecessary.
         (add-after 'unpack 'dont-run-rubocop
           (lambda _
             (substitute* "Rakefile"
               ((".*rubocop/rake\\_task.*") "")
               ((".*RuboCop.*") ""))
             #t)))))
    (propagated-inputs
     `(("ruby-builder" ,ruby-builder)
       ("ruby-cucumber-core" ,ruby-cucumber-core)
       ("ruby-cucumber-wire" ,ruby-cucumber-wire)
       ("ruby-cucumber-expressions" ,ruby-cucumber-expressions)
       ("ruby-diff-lcs" ,ruby-diff-lcs)
       ("ruby-gherkin" ,ruby-gherkin)
       ("ruby-multi-json" ,ruby-multi-json)
       ("ruby-multi-test" ,ruby-multi-test)))
    (native-inputs
     `(("bundler" ,bundler)
       ;; Use a untested version of aruba, to avoid a circular dependency, as
       ;; ruby-aruba depends on ruby-cucumber.
       ("ruby-aruba", ruby-aruba-without-tests)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-pry" ,ruby-pry)
       ("ruby-nokogiri" ,ruby-nokogiri)))
    (synopsis "Describe automated tests in plain language")
    (description
     "Cucumber is a tool for running automated tests written in plain
language.  It's designed to support a Behaviour Driven Development (BDD)
software development workflow.")
    (home-page "https://cucumber.io/")
    (license license:expat)))

(define ruby-cucumber-without-tests
  (package (inherit ruby-cucumber)
    (arguments
     '(#:tests? #f))
    (native-inputs
     '())))

(define-public ruby-cucumber-core
  (package
    (name "ruby-cucumber-core")
    ;; Stick to major version 3, until version 4 of Cucumber is released.
    (version "3.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "cucumber-core" version))
       (sha256
        (base32
         "1iavlh8hqj9lwljbpkw06259gdicbr1bdb6pbj5yy3n8szgr8k3c"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-backports" ,ruby-backports)
       ("ruby-gherkin" ,ruby-gherkin)
       ("ruby-cucumber-tag-expressions" ,ruby-cucumber-tag-expressions)))
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

(define-public ruby-cucumber-expressions
  (package
    (name "ruby-cucumber-expressions")
    (version "6.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "cucumber-expressions" version))
       (sha256
        (base32
         "0zwmv6hznyz9vk81f5dhwcr9jhxx2vmbk8yyazayvllvhy0fkpdw"))))
    (build-system ruby-build-system)
    (arguments
     '(#:test-target "spec"))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-simplecov" ,ruby-simplecov)))
    (synopsis "Simpler alternative to Regular Expressions")
    (description "Cucumber Expressions offer similar functionality to Regular
Expressions, with a syntax that is easier to read and write.  Cucumber
Expressions are extensible with parameter types.")
    (home-page "https://github.com/cucumber/cucumber-expressions-ruby")
    (license license:expat)))

(define-public ruby-cucumber-wire
  (package
    (name "ruby-cucumber-wire")
    ;; Package version 0.0.1 initially, as this is what's needed by Cucumber
    ;; 3, and Cucumber 4 hasn't been released yet.
    (version "0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "cucumber-wire" version))
       (sha256
        (base32
         "09ymvqb0sbw2if1nxg8rcj33sf0va88ancq5nmp8g01dfwzwma2f"))))
    (build-system ruby-build-system)
    (arguments
     '(;; TODO: Currently, the tests can't be run as cucumber is required,
       ;; which would lead to a circular dependency.
       #:tests? #f
       #:test-target "default"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-CUCUMBER_USE_RELEASED_GEMS
           (lambda _
             (setenv "CUCUMBER_USE_RELEASED_GEMS" "true")
             #t)))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)))
    (synopsis "Cucumber wire protocol plugin")
    (description
     "Cucumber's wire protocol allows step definitions to be implemented and
invoked on any platform.")
    (home-page "https://github.com/cucumber/cucumber-ruby-wire")
    (license license:expat)))

(define-public ruby-cucumber-tag-expressions
  (package
    (name "ruby-cucumber-tag-expressions")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "cucumber-tag_expressions" version))
       (sha256
        (base32
         "0cvmbljybws0qzjs1l67fvr9gqr005l8jk1ni5gcsis9pfmqh3vc"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "rspec")
             #t)))))
    (native-inputs
     `(("ruby-rspec" ,ruby-rspec)))
    (synopsis "Cucumber tag expressions for Ruby")
    (description
     "Cucumber tag expression parser for Ruby.  A tag expression is an infix
boolean expression used by Cucumber.")
    (home-page "https://github.com/cucumber/tag-expressions-ruby")
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

(define-public ruby-yajl-ruby
  (package
    (name "ruby-yajl-ruby")
    (version "1.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "yajl-ruby" version))
       (sha256
        (base32
         "16v0w5749qjp13xhjgr2gcsvjv6mf35br7iqwycix1n2h7kfcckf"))))
    (build-system ruby-build-system)
    (arguments
     '(#:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'patch-test-to-update-load-path
           (lambda _
             (substitute* "spec/parsing/large_number_spec.rb"
               (("require \"yajl\"")
                "$LOAD_PATH << 'lib'; require 'yajl'"))
             #t)))))
     (native-inputs
      `(("ruby-rake-compiler" ,ruby-rake-compiler)
        ("ruby-rspec" ,ruby-rspec)))
     (synopsis "Streaming JSON parsing and encoding library for Ruby")
     (description
      "Ruby C bindings to the Yajl JSON stream-based parser library.  The API
is compatible with the JSON gem, so yajl-ruby can act as a drop in
replacement.

A modified copy of yajl is used, and included in the package.")
     (home-page "https://github.com/brianmario/yajl-ruby")
     (license (list license:expat     ; Ruby code, yajl_ext.c and yajl_ext.h
                    license:bsd-3)))) ; Included, modified copy of yajl

(define-public ruby-yard
  (package
    (name "ruby-yard")
    (version "0.9.16")
    (source
     (origin
       (method url-fetch)
       ;; Tests do not pass if we build from the distributed gem.
       (uri (string-append "https://github.com/lsegal/yard/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0sqpbayy9sb406jh0zqg6qha1xds863qz9531dh6vp58hc00clfq"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             ;; $HOME needs to be set to somewhere writeable for tests to run
             (setenv "HOME" "/tmp")
             ;; Run tests without using 'rake' to avoid dependencies.
             (invoke "rspec"))))))
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
    (version "1.2.7")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "eventmachine" version))
       (sha256
        (base32
         "0wh9aqb0skz80fhfn66lbpr4f86ya2z5rx6gm5xlfhd05bj1ch4r"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f))               ; test suite tries to connect to google.com
    (native-inputs
     `(("ruby-rake-compiler" ,ruby-rake-compiler)))
    (synopsis "Single-threaded network event framework for Ruby")
    (description
     "EventMachine implements a single-threaded engine for arbitrary network
communications.  EventMachine wraps all interactions with sockets, allowing
programs to concentrate on the implementation of network protocols.  It can be
used to create both network servers and clients.")
    ;; The ‘official’ rubyeventmachine.com domain is now registrar-squatted.
    (home-page "https://github.com/eventmachine/eventmachine")
    (license (list license:ruby license:gpl3)))) ; GPLv3 only AFAICT

(define-public ruby-ruby-engine
  (package
    (name "ruby-ruby-engine")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "ruby_engine" version))
       (sha256
        (base32
         "1d0sd4q50zkcqhr395wj1wpn2ql52r0fpwhzjfvi1bljml7k546v"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'clean-up
           (lambda _
             (delete-file "Gemfile.lock")
             (substitute* "ruby_engine.gemspec"
               ;; Remove unnecessary imports that would entail further
               ;; dependencies.
               ((".*<rdoc.*") "")
               ((".*<rubygems-tasks.*") "")
               ;; Remove extraneous .gem file
               (("\\\"pkg/ruby_engine-1.0.0.gem\\\",") "")
               ;; Soften rake dependency
               (("%q<rake>.freeze, \\[\\\"~> 10.0\\\"\\]")
                "%q<rake>.freeze, [\">= 10.0\"]")
               ;; Soften the rspec dependency
               (("%q<rspec>.freeze, \\[\\\"~> 2.4\\\"\\]")
                "%q<rspec>.freeze, [\">= 2.4\"]"))
             (substitute* "Rakefile"
               (("require 'rubygems/tasks'") "")
               (("Gem::Tasks.new") ""))
             ;; Remove extraneous .gem file that otherwise gets installed.
             (delete-file "pkg/ruby_engine-1.0.0.gem")
             #t)))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rake" ,ruby-rake)
       ("ruby-rspec" ,ruby-rspec)))
    (synopsis "Simplifies checking for Ruby implementation")
    (description
     "@code{ruby_engine} provides an RubyEngine class that can be used to
check which implementation of Ruby is in use.  It can provide the interpreter
name and provides query methods such as @{RubyEngine.mri?}.")
    (home-page "https://github.com/janlelis/ruby_engine")
    (license license:expat)))

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
             (invoke "ruby" "-Ilib" "bin/turn" "-h"))))))
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

(define-public ruby-mimemagic
  (package
    (name "ruby-mimemagic")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "mimemagic" version))
       (sha256
        (base32
         "00ibc1mhvdfyfyl103xwb45621nwyqxf124cni5hyfhag0fn1c3q"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; This phase breaks the tests, as it patches some of the test data.
         (delete 'patch-source-shebangs))))
    (native-inputs
     `(("ruby-bacon" ,ruby-bacon)))
    (synopsis "Ruby library for MIME detection by extension or content")
    (description
     "@acronym{MIME, Multipurpose Internet Mail Extensions} detection by
extension or content, using the freedesktop.org.xml shared-mime-info
database.")
    (home-page "https://github.com/minad/mimemagic")
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
    (version "1.3.6")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "fivemat" version))
       (sha256
        (base32
         "006n7b09vviv5bs5hv2ccmjxw9iw3brcsm3xh3dhzfncsknz4jp7"))))
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
         (add-before 'check 'adjust-failing-test
           (lambda _
             ;; XXX: This test fails with SQLite versions >= 3.21.
             ;; See <https://github.com/sparklemotion/sqlite3-ruby/issues/226>.
             (substitute* "test/test_integration_resultset.rb"
               (("\"integer\", \"text\"") "\"INTEGER\", \"text\""))
             #t))
         (add-before 'check 'add-gemtest-file
           ;; This file exists in the repository but is not distributed.
           (lambda _ (invoke "touch" ".gemtest"))))))
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
             (invoke "ruby" "-Ilib" "-r" "shoulda-context"))))))
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
             (invoke "ruby" "-Ilib" "-r" "shoulda-matchers"))))))
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
           (lambda _ (invoke "ruby" "-Ilib" "-r" "shoulda"))))))
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

(define-public ruby-warden
  (package
    (name "ruby-warden")
    (version "1.2.8")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "warden" version))
       (sha256
        (base32
         "1fr9n9i9r82xb6i61fdw4xgc7zjv7fsdrr4k0njchy87iw9fl454"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; No included tests
    (propagated-inputs
     `(("ruby-rack" ,ruby-rack)))
    (synopsis "Rack middleware providing authentication")
    (description
     "Warden is a Rack-based middleware that provides a mechanism for
authentication in Ruby web applications.")
    (home-page "https://github.com/wardencommunity/warden")
    (license license:expat)))

(define-public ruby-warden-oauth2
  (package
    (name "ruby-warden-oauth2")
    (version "0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "warden-oauth2" version))
       (sha256
        (base32
         "1z9154lvzrnnfjbjkmirh4n811nygp6pm2fa6ikr7y1ysa4zv3cz"))))
    (build-system ruby-build-system)
    (arguments
     '(#:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-unnecessary-dependencies
           (lambda _
             (substitute* "Gemfile"
               ;; All of these gems relate to development, and are unnecessary
               ;; when running the tests
               (("gem 'guard-bundler'") "")
               (("gem 'guard'") "")
               (("gem 'guard-rspec'") "")
               (("gem 'rb-fsevent'") "")
               (("gem 'pry'") "")
               (("gem 'growl'") ""))
             #t))
         ;; The test suite doesn't work with rspec@2, and this is incompatible
         ;; with the current version of Rake, so invoke Rspec directly
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "bundle" "exec" "rspec"))
             #t)))))
    (propagated-inputs
     `(("ruby-warden" ,ruby-warden)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec-2)
       ("ruby-rack-test" ,ruby-rack-test)))
    (synopsis "OAuth 2.0 strategies for Warden")
    (description
     "This library extends Warden to support OAuth 2.0 authorized API
requests.")
    (home-page "https://github.com/opperator/warden-oauth2")
    (license license:expat)))

(define-public ruby-webmock-2
  (package
    (name "ruby-webmock")
    (version "2.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "webmock" version))
       (sha256
        (base32
         "04hkcqsmbfnp8g237pisnc834vpgildklicbjbyikqg0bg1rwcy5"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)))
    (propagated-inputs
     `(("ruby-addressable" ,ruby-addressable)
       ("ruby-crack" ,ruby-crack)
       ("ruby-hashdiff" ,ruby-hashdiff)))
    (synopsis "Allows stubbing and setting expectations on HTTP requests")
    (description
     "WebMock allows stubbing HTTP requests and setting expectations on HTTP
requests.  This is useful when testing software.")
    (home-page "https://github.com/bblimke/webmock")
    (license license:expat)))

(define-public ruby-unicode-display-width
  (package
    (name "ruby-unicode-display-width")
    (version "1.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "unicode-display_width" version))
       (sha256
        (base32
         "0bq528fibi8s0jmxz0xzlgzggdq0x4fx46wfqz49478pv8gb2diq"))))
    (build-system ruby-build-system)
    (arguments
     '(;; Test data not included.
       #:tests? #f))
    (synopsis "Determine the monospace display width of Ruby strings")
    (description
     "@code{Unicode::DisplayWidth} is a Ruby library which can determine the
display width of strings in Ruby.")
    (home-page "https://github.com/janlelis/unicode-display_width")
    (license license:expat)))

;; There is another gem called 'ruby-version' so we use an underscore in this
;; name
(define-public ruby_version
  (package
    (name "ruby_version")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "ruby_version" version))
       (sha256
        (base32
         "0854i1bjy56176anr05l5m0vc81nl53c7fyfg7sljj62m1d64dgj"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-dependencies
           (lambda _
             ;; Remove the Gemfile.lock, as we want to use Guix packages at
             ;; whatever versions.
             (delete-file "Gemfile.lock")
             ;; Remove the incldued gem file as it's unnecessary.
             (delete-file "pkg/ruby_version-1.0.0.gem")
             (substitute* "ruby_version.gemspec"
               ;; Don't require rdoc and rubygems-tasks as they're unnecessary
               ((".*rdoc.*") "\n")
               ((".*rubygems-tasks.*") "\n")
               ;; Accept any version of rake and rspec
               (("%q<rake.*") "%q<rake>)\n")
               (("%q<rspec.*") "%q<rspec>)\n"))
             ;; Remove the use of rubygems-tasks from the Rakefile, as it's
             ;; unnecessary.
             (substitute* "Rakefile"
               (("^require 'rubygems/tasks'") "")
               (("Gem::Tasks.new") ""))
             #t)))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)))
    (synopsis "Ruby library to help check the Ruby version")
    (description
     "@code{ruby_version} provides a @code{RubyVersion} module to simplify
checking for the right Ruby version in software.")
    (home-page "https://github.com/janlelis/ruby_version")
    (license license:expat)))

(define-public ruby-websocket-driver
  (package
   (name "ruby-websocket-driver")
   (version "0.7.0")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "websocket-driver" version))
     (sha256
      (base32
       "1551k3fs3kkb3ghqfj3n5lps0ikb9pyrdnzmvgfdxy8574n4g1dn"))))
   (build-system ruby-build-system)
   (arguments
    '(;; No included tests
      #:tests? #f))
   (propagated-inputs
    `(("ruby-websocket-extensions" ,ruby-websocket-extensions)))
   (synopsis "WebSocket protocol handler with pluggable I/O")
   (description
    "@code{websocket-driver} provides a complete implementation of the
WebSocket protocols that can be hooked up to any TCP library")
   (home-page "https://github.com/faye/websocket-driver-ruby")
   (license license:expat)))

(define-public ruby-websocket-extensions
  (package
    (name "ruby-websocket-extensions")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "websocket-extensions" version))
       (sha256
        (base32
         "034sdr7fd34yag5l6y156rkbhiqgmy395m231dwhlpcswhs6d270"))))
    (build-system ruby-build-system)
    (arguments
     '(;; No included tests
       #:tests? #f))
    (synopsis "Generic extension manager for WebSocket connections")
    (description
     "@code{websocket-extensions} provides a container for registering
extension plugins.")
    (home-page "https://github.com/faye/websocket-extensions-ruby")
    (license license:expat)))

(define-public ruby-domain-name
  (package
    (name "ruby-domain-name")
    (version "0.5.20180417")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "domain_name" version))
       (sha256
        (base32
         "0abdlwb64ns7ssmiqhdwgl27ly40x2l27l8hs8hn0z4kb3zd2x3v"))))
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
                 (invoke "ruby"
                         "-Ilib"
                         "test/runner.rb")
                 #t))))))
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
             (invoke "ruby" "-Ilib" "-r" "ansi")))
         (add-before 'validate-runpath 'replace-broken-symlink
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (file (string-append
                           out "/lib/ruby/vendor_ruby/gems/ansi-"
                           ,version "/lib/ansi.yml")))
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
             (invoke "rspec" "spec/bio-commandeer_spec.rb"))))))
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
             (invoke "ruby" "-Ilib" "-r" "rubytest"))))))
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
             (invoke "ruby" "-Ilib" "-r" "brass"))))))
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
             (invoke "ruby" "-Ilib" "bin/qed" "--copyright"))))))
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

(define-public ruby-que
  (package
    (name "ruby-que")
    (version "1.0.0.beta3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "que" version))
       (sha256
        (base32
         "0gr9pb814d4qj3ds98g6cjrdk7wv0yg8aqbm7c1lmgl87jkg8q04"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; No included tests
    (synopsis "Job queue using PostgreSQL written in Ruby")
    (description
     "This package provides a job queue that uses PostgreSQL for storing jobs
and locking between worker processes.")
    (home-page "https://github.com/chanks/que")
    (license license:expat)))

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
           (lambda _ (invoke "qed")))
         (add-before 'validate-runpath 'replace-broken-symlink
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (file (string-append
                           out "/lib/ruby/vendor_ruby/gems/ae-"
                           ,version "/lib/ae.yml")))
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
         (replace 'check (lambda _ (invoke "qed"))))))
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
             (invoke "qed")
             (invoke "rubytest" "-Ilib" "-Itest" "test/"))))))
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
             (invoke "rspec" "spec/rc4_spec.rb"))))))
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
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "Ascii85" version))
       (sha256
        (base32
         "0658m37jjjn6drzqg1gk4p6c205mgp7g1jh2d00n4ngghgmz5qvs"))))
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
    (version "1.5.1")
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
         "1ymcn12n5iws401yz03zsj8rr653fdqq13czsrciq09phgh9jzc5"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'remove-ssh
           (lambda _
             ;; remove dependency on an ssh key pair that doesn't exist
             (substitute* "ttfunk.gemspec"
               (("spec.signing_key.*") ""))
             #t))
         (add-before 'check 'remove-rubocop
           (lambda _
             ;; remove rubocop as a dependency as not needed for testing
             (substitute* "ttfunk.gemspec"
               (("spec.add_development_dependency\\('rubocop'.*") ""))
             (substitute* "Rakefile"
               (("require 'rubocop/rake_task'") "")
               (("RuboCop::RakeTask.new") ""))
             #t)))))
    (native-inputs
     `(("ruby-rspec" ,ruby-rspec)
       ("ruby-yard" ,ruby-yard)
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
           (lambda _ (invoke "rake" "compile")))
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

(define-public ruby-mathn
  (package
    (name "ruby-mathn")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "mathn" version))
       (sha256
        (base32
         "1wn812llln9jzgybz2d7536q39z3gi99i6fi0j1dapcpzvhgrr0p"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rake-compiler" ,ruby-rake-compiler)))
    (synopsis "Extends math operations for increased precision")
    (description
     "This gem makes mathematical operations more precise in Ruby and
integrates other mathematical standard libraries.  Prior to Ruby 2.5,
@code{mathn} was part of the Ruby standard library.")
    (home-page "https://github.com/ruby/mathn")
    (license license:bsd-2)))

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

(define-public ruby-rubypants
  (package
    (name "ruby-rubypants")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rubypants" version))
              (sha256
               (base32
                "0xpqkslan2wkyal2h9qhplkr5d4sdn7q6csigrhnljjpp8j4qfsh"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; need Codecov
    (synopsis "Port of the smart-quotes library SmartyPants")
    (description
     "RubyPants is a Ruby port of the smart-quotes library SmartyPants.  The
original SmartyPants is a web publishing plug-in for Movable Type, Blosxom,
and BBEdit that easily translates plain ASCII punctuation characters into
smart typographic punctuation HTML entities.")
    (home-page "https://github.com/jmcnevin/rubypants")
    (license license:bsd-2)))

(define-public ruby-org-ruby
  (package
    (name "ruby-org-ruby")
    (version "0.9.12")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "org-ruby" version))
              (sha256
               (base32
                "0x69s7aysfiwlcpd9hkvksfyld34d8kxr62adb59vjvh8hxfrjwk"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; no rakefile
    (propagated-inputs
     `(("ruby-rubypants" ,ruby-rubypants)))
    (synopsis "Org-mode parser written in Ruby")
    (description
     "Org-ruby is an org-mode parser written in Ruby.  The most significant
thing this library does today is convert org-mode files to HTML or Textile or
Markdown.")
    (home-page "https://github.com/wallyqs/org-ruby")
    (license license:expat)))

(define-public ruby-rake
  (package
    (name "ruby-rake")
    (version "12.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rake" version))
       (sha256
        (base32
         "1idi53jay34ba9j68c3mfr9wwkg3cd9qh0fn9cg42hv72c6q8dyg"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("bundler" ,bundler)))
    (synopsis "Rake is a Make-like program implemented in Ruby")
    (description
     "Rake is a Make-like program where tasks and dependencies are specified
in standard Ruby syntax.")
    (home-page "https://github.com/ruby/rake")
    (license license:expat)))

(define-public ruby-childprocess-0.6
  (package
    (name "ruby-childprocess")
    (version "0.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "childprocess" version))
       (sha256
        (base32
         "1p3f43scdzx9zxmy2kw5zsc3az6v46nq4brwcxmnscjy4w4racbv"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)))
    (propagated-inputs
     `(("ruby-ffi" ,ruby-ffi)))
    (synopsis "Control external programs running in the background, in Ruby")
    (description "@code{childprocess} provides a gem to control external
programs running in the background, in Ruby.")
    (home-page "http://github.com/enkessler/childprocess")
    (license license:expat)))

(define-public ruby-childprocess
  (package
    (inherit ruby-childprocess-0.6)
    (name "ruby-childprocess")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "childprocess" version))
       (sha256
        (base32
         "0a61922kmvcxyj5l70fycapr87gz1dzzlkfpq85rfqk5vdh3d28p"))))))

(define-public ruby-public-suffix
  (package
    (name "ruby-public-suffix")
    (version "3.0.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "public_suffix" version))
              (sha256
               (base32
                "08q64b5br692dd3v0a9wq9q5dvycc6kmiqmjbdxkxbfizggsvx6l"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; Remove the requirement on Rubocop, as it isn't useful to run, and
         ;; including it as an input can lead to circular dependencies.
         (add-after 'unpack 'remove-rubocop-from-Rakefile
           (lambda _
             (substitute* "Rakefile"
               (("require \"rubocop/rake\\_task\"") "")
               (("RuboCop::RakeTask\\.new") ""))
             #t)))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-yard" ,ruby-yard)
       ("ruby-mocha" ,ruby-mocha)
       ("ruby-minitest-reporters" ,ruby-minitest-reporters)))
    (home-page "https://simonecarletti.com/code/publicsuffix-ruby/")
    (synopsis "Domain name parser")
    (description "The gem @code{public_suffix} is a domain name parser,
written in Ruby, and based on the @dfn{Public Suffix List}.  A public suffix
is one under which Internet users can (or historically could) directly
register names.  Some examples of public suffixes are @code{.com},
@code{.co.uk} and @code{pvt.k12.ma.us}.  The Public Suffix List is a list of
all known public suffixes.")
    (license license:expat)))

(define-public ruby-addressable
  (package
    (name "ruby-addressable")
    (version "2.6.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "addressable" version))
              (sha256
               (base32
                "0bcm2hchn897xjhqj9zzsxf3n9xhddymj4lsclz508f4vw3av46l"))))
    (build-system ruby-build-system)
    (arguments
     '(#:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-unnecessary-dependencies-from-Gemfile
          (lambda _
            (substitute* "Gemfile"
              (("git: 'https://github.com/sporkmonger/rack-mount.git',") "")
              ((".*launchy.*") "")
              ((".*rake.*") "gem 'rake'\n")
              ((".*redcarpet.*") ""))
            #t))
         (add-before 'check 'delete-network-dependent-test
           (lambda _
             (delete-file "spec/addressable/net_http_compat_spec.rb")
             #t)))))
    (native-inputs
     `(("ruby-rspec" ,ruby-rspec)
       ("bundler" ,bundler)
       ("ruby-idn-ruby" ,ruby-idn-ruby)
       ("ruby-sporkmonger-rack-mount" ,ruby-sporkmonger-rack-mount)
       ("ruby-rspec-its", ruby-rspec-its)
       ("ruby-yard" ,ruby-yard)
       ("ruby-simplecov" ,ruby-simplecov)))
    (propagated-inputs
     `(("ruby-public-suffix" ,ruby-public-suffix)))
    (home-page "https://github.com/sporkmonger/addressable")
    (synopsis "Alternative URI implementation")
    (description "Addressable is a replacement for the URI implementation that
is part of Ruby's standard library.  It more closely conforms to RFC 3986,
RFC 3987, and RFC 6570 (level 4), providing support for IRIs and URI templates.")
    (license license:asl2.0)))

(define-public ruby-colorator
  (package
    (name "ruby-colorator")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "colorator" version))
              (sha256
               (base32
                "0f7wvpam948cglrciyqd798gdc6z3cfijciavd0dfixgaypmvy72"))))
    (build-system ruby-build-system)
    (arguments
     ;; No test target
     `(#:tests? #f))
    (home-page "http://octopress.org/colorator/")
    (synopsis "Terminal color library")
    (description "Colorator is a Ruby gem that helps you colorize your text
for the terminal.")
    (license license:expat)))

(define-public ruby-command-line-reporter
  (package
    (name "ruby-command-line-reporter")
    (version "4.0.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "command_line_reporter" version))
              (sha256
               (base32
                "1qma35xrb772whxwy1rs9bicb9d6lvz0s2dd2dnn4fr6zcbcxc0a"))))
    (build-system ruby-build-system)
    (arguments
     ;; No Rakefile
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-dependencies
           (lambda _
             (substitute* ".gemspec"
               ;; colored is unmaintained
               (("colored") "colorator")
               ;; colorator version
               (("= 1.2") "= 1.1"))
             #t)))))
    (propagated-inputs `(("ruby-colorator" ,ruby-colorator)))
    (home-page "https://github.com/wbailey/command_line_reporter")
    (synopsis "Report production while executing Ruby scripts")
    (description "This gem provides a DSL that makes it easy to write reports
of various types in ruby.  It eliminates the need to litter your source with
puts statements, instead providing a more readable, expressive interface to
your application.")
    (license license:asl2.0)))

(define-public ruby-command-line-reporter-3
  (package
    (inherit ruby-command-line-reporter)
    (version "3.3.6")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "command_line_reporter" version))
              (sha256
               (base32
                "1h39zqqxp3k4qk49ajpx0jps1vmvxgkh43mqkb6znk583bl0fv71"))))))

(define-public ruby-rdoc
  (package
    (name "ruby-rdoc")
    (version "6.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "rdoc" version))
        (sha256
          (base32
            "0anv42cqcdc6g4n386mrva7mgav5i0c2ry3yzvzzc6z6hymkmcr7"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("bundler" ,bundler)))
    (home-page "https://ruby.github.io/rdoc/")
    (synopsis "HTML and command-line documentation utility")
    (description "RDoc produces HTML and command-line documentation for Ruby
projects.  RDoc includes the +rdoc+ and +ri+ tools for generating and displaying
documentation from the command-line.")
    (license license:gpl2+)))

(define-public ruby-sass-listen
  (package
    (name "ruby-sass-listen")
    (version "4.0.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "sass-listen" version))
              (sha256
               (base32
                "0xw3q46cmahkgyldid5hwyiwacp590zj2vmswlll68ryvmvcp7df"))))
    (build-system ruby-build-system)
    (arguments
     ;; No test target
     `(#:tests? #f))
    (propagated-inputs
     `(("ruby-rb-fsevent" ,ruby-rb-fsevent)
       ("ruby-rb-inotify" ,ruby-rb-inotify)))
    (home-page "https://github.com/sass/listen")
    (synopsis "File modification notification library")
    (description "The Listen gem listens to file modifications and notifies you
about the changes.")
    (license license:expat)))

(define-public ruby-terminfo
  (package
    (name "ruby-terminfo")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "ruby-terminfo" version))
        (sha256
          (base32
            "0rl4ic5pzvrpgd42z0c1s2n3j39c9znksblxxvmhkzrc0ckyg2cm"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "test"
       ;; Rakefile requires old packages and would need modification to
       ;; work with current software.
       #:tests? #f))
    (inputs
     `(("ncurses" ,ncurses)))
    (native-inputs
     `(("ruby-rubygems-tasks" ,ruby-rubygems-tasks)
       ("ruby-rdoc" ,ruby-rdoc)))
    (home-page "http://www.a-k-r.org/ruby-terminfo/")
    (synopsis "Terminfo binding for Ruby")
    (description "Ruby-terminfo provides terminfo binding for Ruby.")
    (license license:bsd-3)))

(define-public ruby-diffy
  (package
    (name "ruby-diffy")
    (version "3.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "diffy" version))
        (sha256
          (base32
            "119imrkn01agwhx5raxhknsi331y5i4yda7r0ws0an6905ximzjg"))))
    (build-system ruby-build-system)
    (arguments
     ;; No tests
     `(#:tests? #f))
    (native-inputs
     `(("ruby-rspec" ,ruby-rspec)))
    (home-page "https://github.com/samg/diffy")
    (synopsis "Convenient diffing in ruby")
    (description "Diffy provides a convenient way to generate a diff from two
strings or files.")
    (license license:expat)))

(define-public ruby-sass-spec
  (package
    (name "ruby-sass-spec")
    (version "3.5.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/sass/sass-spec/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0nx8lp7c9qa58w489crgqa3c489xsyarn1a8h4np9mwwfqm1h3rr"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-command-line-reporter-3" ,ruby-command-line-reporter-3)
       ("ruby-diffy" ,ruby-diffy)
       ("ruby-terminfo" ,ruby-terminfo)))
    (arguments
     `(;; This package contains tests for a sass implementation, and the to
       ;; avoid any circular dependencies, the tests are not run here
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-test
           (lambda _
             (delete-file "spec/values/colors/alpha_hex-3.5/error")
             (substitute* "spec/values/colors/alpha_hex-3.5/expected_output.css"
               (("string") "color")))))))
    (home-page "https://github.com/sass/sass-spec")
    (synopsis "Test suite for Sass")
    (description "Sass Spec is a test suite for Sass.  Test cases are all in
the @file{spec} directory.")
    (license license:expat)))

(define-public ruby-sass
  (package
    (name "ruby-sass")
    (version "3.6.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "sass" version))
              (sha256
               (base32
                "18c6prbw9wl8bqhb2435pd9s0lzarl3g7xf8pmyla28zblvwxmyh"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-sass-listen" ,ruby-sass-listen)))
    (native-inputs
     `(("ruby-sass-spec" ,ruby-sass-spec)
       ("ruby-mathn" ,ruby-mathn)))
    (home-page "http://sass-lang.com/")
    (synopsis "CSS extension language")
    (description "Sass is a CSS extension language.  It extends CSS with
features that don't exist yet like variables, nesting, mixins and inheritance.")
    (license license:expat)))

(define-public ruby-sassc
  (package
    (name "ruby-sassc")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "sassc" version))
       (sha256
        (base32
         "1sr4825rlwsrl7xrsm0sgalcpf5zgp4i56dbi3qxfa9lhs8r6zh4"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         ;; TODO: This would be better as a snippet, but the ruby-build-system
         ;; doesn't seem to support that
         (add-after 'unpack 'remove-libsass
           (lambda _
             (delete-file-recursively "ext")
             #t))
         (add-after 'unpack 'dont-check-the-libsass-version
           (lambda _
             (substitute* "test/native_test.rb"
               (("assert_equal.*Native\\.version") ""))
             #t))
         (add-after 'unpack 'remove-git-from-gemspec
           (lambda _
             (substitute* "sassc.gemspec"
               (("`git ls-files -z`") "`find . -type f -print0 |sort -z`")
               (("`git submodule --quiet foreach pwd`") "''"))
             #t))
         (add-after 'unpack 'remove-extensions-from-gemspec
           (lambda _
             (substitute* "sassc.gemspec"
               (("\\[\"ext/Rakefile\"\\]") "[]"))
             #t))
         (add-after 'unpack 'fix-Rakefile
           (lambda _
             (substitute* "Rakefile"
               (("test: 'libsass:compile'") ":test"))
             #t))
         (add-after 'unpack 'remove-unnecessary-dependencies
           (lambda _
             (substitute* "test/test_helper.rb"
               (("require \"pry\"") ""))
             #t))
         (add-before 'build 'patch-native.rb
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "lib/sassc/native.rb"
               ((".*gem_root = spec.gem_dir") "")
               (("ffi_lib .*\n")
                (string-append
                 "ffi_lib '" (assoc-ref inputs "libsass") "/lib/libsass.so'")))
             #t))
         ;; The gemspec still references the libsass files, so just keep the
         ;; one in the gem.
         (delete 'extract-gemspec))))
    (propagated-inputs
     `(("ruby-ffi" ,ruby-ffi)
       ("ruby-rake" ,ruby-rake)))
    (inputs
     `(("libsass" ,libsass)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-minitest-around" ,ruby-minitest-around)
       ("ruby-test-construct" ,ruby-test-construct)))
    (synopsis "Use libsss from Ruby")
    (description
     "This library provides Ruby q@acronym{FFI, Foreign Function Interface}
bindings to the libsass library.  This enables rendering
@acronym{SASS,Syntactically awesome style sheets} from Ruby code.")
    (home-page "https://github.com/sass/sassc-ruby")
    (license license:expat)))

(define-public ruby-jekyll-sass-converter
  (package
    (name "ruby-jekyll-sass-converter")
    (version "1.5.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "jekyll-sass-converter" version))
              (sha256
               (base32
                "008ikh5fk0n6ri54mylcl8jn0mq8p2nfyfqif2q3pp0lwilkcxsk"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-sass" ,ruby-sass)))
    (arguments
     ;; No rakefile
     `(#:tests? #f))
    (home-page "https://github.com/jekyll/jekyll-sass-converter")
    (synopsis "Sass converter for Jekyll")
    (description "This gem provide built-in support for the Sass converter
in Jekyll.")
    (license license:expat)))

(define-public ruby-jekyll-watch
  (package
    (name "ruby-jekyll-watch")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "jekyll-watch" version))
              (sha256
               (base32
                "0m7scvj3ki8bmyx5v8pzibpg6my10nycnc28lip98dskf8iakprp"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-listen-3.0" ,ruby-listen-3.0)))
    (arguments
     ;; No rakefile
     `(#:tests? #f))
    (home-page "https://github.com/jekyll/jekyll-watch")
    (synopsis "Jekyll auto-rebuild support")
    (description "This gems add the @code{--watch} switch to the jekyll CLI
interface.  It allows Jekyll to rebuild your site when a file changes.")
    (license license:expat)))

(define-public ruby-parallel
  (package
    (name "ruby-parallel")
    (version "1.13.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/grosser/parallel.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1isqzbqxz2ndad4i5z3lb9ldrhaijfncj8bmffv04sq44sv87ikv"))))
    (build-system ruby-build-system)
    (arguments
     `(;; TODO 3 test failures
       ;; rspec ./spec/parallel_spec.rb:190 # Parallel.in_processes does not
       ;; open unnecessary pipes
       ;; rspec './spec/parallel_spec.rb[1:9:7]' # Parallel.each works with
       ;; SQLite in processes
       ;; rspec './spec/parallel_spec.rb[1:9:16]' # Parallel.each works with
       ;; SQLite in threads
       #:tests? #f
       #:test-target "rspec-rerun:spec"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-Gemfile
           (lambda _
             (substitute* "Gemfile"
               (("gem 'rspec-legacy_formatters'") "")
               (("gem 'activerecord.*$") "gem 'activerecord'\n"))))
         (add-before 'check 'delete-Gemfile.lock
           (lambda _
             ;; Bundler isn't being used for fetching dependendencies, so
             ;; delete the Gemfile.lock
             (delete-file "Gemfile.lock")
             #t))
         (add-before 'build 'patch-gemspec
           (lambda _
             (substitute* "parallel.gemspec"
               (("git ls-files") "find"))
             #t)))))
    (native-inputs
     `(("ruby-rspec" ,ruby-rspec)
       ("ruby-rspec-rerun" ,ruby-rspec-rerun)
       ("bundler" ,bundler)
       ("ruby-activerecord" ,ruby-activerecord)
       ("ruby-progressbar" ,ruby-progressbar)
       ("ruby-bump" ,ruby-bump)
       ("procps" ,procps)
       ("lsof" ,lsof)
       ("ruby-mysql2" ,ruby-mysql2)
       ("ruby-sqlite3" ,ruby-sqlite3)
       ("ruby-i18n" ,ruby-i18n)))
    (home-page "https://github.com/grosser/parallel")
    (synopsis "Parallel processing in Ruby")
    (description "Parallel allows you to run any code in parallel Processes
(to use all CPUs) or Threads(to speedup blocking operations).  It is best
suited for map-reduce or e.g. parallel downloads/uploads.")
    (license license:expat)))

(define-public ruby-cane
  (package
    (name "ruby-cane")
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "cane" version))
              (sha256
               (base32
                "0yf5za3l7lhrqa3g56sah73wh33lbxy5y3cb7ij0a2bp1b4kwhih"))))
    (build-system ruby-build-system)
    (arguments `(#:tests? #f)); No rakefile
    (home-page "https://github.com/square/cane")
    (propagated-inputs
     `(("ruby-parallel" ,ruby-parallel)))
    (synopsis "Code quality threshold checking")
    (description "Cane fails your build if code quality thresholds are not met.")
    (license license:asl2.0)))

(define-public ruby-morecane
  (package
    (name "ruby-morecane")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "morecane" version))
              (sha256
               (base32
                "0w70vb8z5bdhvr21h660aa43m5948pv0bd27z7ngai2iwdvqd771"))))
    (build-system ruby-build-system)
    (home-page "https://github.com/yob/morecane")
    (arguments `(#:tests? #f)); No rakefile
    (propagated-inputs
     `(("ruby-parallel" ,ruby-parallel)))
    (synopsis "Extra checks for cane")
    (description "The cane gem provides a great framework for running quality
checks over your ruby project as part of continuous integration build.  It
comes with a few checks out of the box, but also provides an API for loading
custom checks.  This gem provides a set of additional checks.")
    (license license:expat)))

(define-public ruby-pdf-reader
  (package
    (name "ruby-pdf-reader")
    (version "2.1.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "pdf-reader" version))
              (sha256
               (base32
                "1b3ig4wpcgdbqa7yw0ahwbmikkkywn2a22bfmrknl5ls7g066x45"))))
    (build-system ruby-build-system)
    (arguments `(#:test-target "spec"))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-cane" ,ruby-cane)
       ("ruby-morecane" ,ruby-morecane)))
    (propagated-inputs
     `(("ruby-afm" ,ruby-afm)
       ("ruby-ascii85" ,ruby-ascii85)
       ("ruby-hashery" ,ruby-hashery)
       ("ruby-rc4" ,ruby-rc4)
       ("ruby-ttfunk" ,ruby-ttfunk)))
    (home-page "https://github.com/yob/pdf-reader")
    (synopsis "PDF parser in Ruby")
    (description "The PDF::Reader library implements a PDF parser conforming as
much as possible to the PDF specification from Adobe.  It provides programmatic
access to the contents of a PDF file with a high degree of flexibility.")
    (license license:gpl3+)))

(define-public ruby-pdf-inspector
  (package
    (name "ruby-pdf-inspector")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "pdf-inspector" version))
              (sha256
               (base32
                "1g853az4xzgqxr5xiwhb76g4sqmjg4s79mm35mp676zjsrwpa47w"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-pdf-reader" ,ruby-pdf-reader)))
    (arguments `(#:tests? #f)); No rakefile
    (home-page "https://github.com/prawnpdf/pdf-inspector")
    (synopsis "Analysis classes for inspecting PDF output")
    (description "This library provides a number of PDF::Reader based tools for
use in testing PDF output.  Presently, the primary purpose of this tool is to
support the tests found in Prawn, a pure Ruby PDF generation library.")
    (license license:gpl3+)))

(define-public ruby-pdf-core
  (package
    (name "ruby-pdf-core")
    (version "0.8.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "pdf-core" version))
              (sha256
               (base32
                "15d6m99bc8bbzlkcg13qfpjjzphfg5x905pjbfygvpcxsm8gnsvg"))))
    (build-system ruby-build-system)
    (arguments
     ; No test target
     `(#:tests? #f))
    (home-page "https://github.com/prawnpdf/pdf-core")
    (synopsis "Low level PDF features for Prawn")
    (description "This is an experimental gem that extracts low-level PDF
functionality from Prawn.")
    (license license:gpl3+)))

(define-public ruby-prawn
  (package
    (name "ruby-prawn")
    (version "2.2.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "prawn" version))
              (sha256
               (base32
                "1qdjf1v6sfl44g3rqxlg8k4jrzkwaxgvh2l4xws97a8f3xv4na4m"))))
    (build-system ruby-build-system)
    (arguments
     ; No tests
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-dependencies
           (lambda _
             (substitute* "prawn.gemspec"
               (("~> 0.7.0") "~> 0.7"))
             #t)))))
    (propagated-inputs
     `(("ruby-pdf-core" ,ruby-pdf-core)
       ("ruby-ttfunk" ,ruby-ttfunk)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-pdf-inspector" ,ruby-pdf-inspector)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-simplecov" ,ruby-simplecov)
       ("ruby-yard" ,ruby-yard)))
    (home-page "http://prawnpdf.org/api-docs/2.0/")
    (synopsis "PDF generation for Ruby")
    (description "Prawn is a pure Ruby PDF generation library.")
    (license license:gpl3+)))

(define-public ruby-prawn-table
  (package
    (name "ruby-prawn-table")
    (version "0.2.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "prawn-table" version))
              (sha256
               (base32
                "1nxd6qmxqwl850icp18wjh5k0s3amxcajdrkjyzpfgq0kvilcv9k"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-prawn" ,ruby-prawn)
       ("ruby-pdf-inspector" ,ruby-pdf-inspector)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-yard" ,ruby-yard)
       ("ruby-mocha" ,ruby-mocha)
       ("ruby-coderay" ,ruby-coderay)
       ("ruby-prawn-manual-builder" ,ruby-prawn-manual-builder)
       ("ruby-simplecov" ,ruby-simplecov)
       ("ruby-rspec-2" ,ruby-rspec-2)))
    (arguments
     '(;; TODO: 1 test fails
       ;; Failure/Error: pdf.page_count.should == 1
       ;;   expected: 1
       ;;        got: 2 (using ==)
       ;; # ./spec/table_spec.rb:1308
       ;;
       ;; 225 examples, 1 failure
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'patch-gemspec
           (lambda _
             (substitute* "prawn-table.gemspec"
               ;; Loosen the requirement for pdf-inspector
               (("~> 1\\.1\\.0") ">= 0")
               ;; Loosen the requirement for pdf-reader
               (("~> 1\\.2") ">= 0"))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "rspec"))
             #t)))))
    (home-page "https://github.com/prawnpdf/prawn-table")
    (synopsis "Tables support for Prawn")
    (description "This gem provides tables support for Prawn.")
    (license license:gpl3+)))

(define-public ruby-kramdown
  (package
    (name "ruby-kramdown")
    (version "1.17.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "kramdown" version))
              (sha256
               (base32
                "1n1c4jmrh5ig8iv1rw81s4mw4xsp4v97hvf8zkigv4hn5h542qjq"))))
    (build-system ruby-build-system)
    (arguments `(#:tests? #f)); FIXME: some test failures
    (native-inputs
     `(("ruby-prawn" ,ruby-prawn)
       ("ruby-prawn-table" ,ruby-prawn-table)))
    (home-page "https://kramdown.gettalong.org/")
    (synopsis "Markdown parsing and converting library")
    (description "Kramdown is a library for parsing and converting a superset
of Markdown.  It is completely written in Ruby, supports standard Markdown
(with some minor modifications) and various extensions that have been made
popular by the PHP @code{Markdown Extra} package and @code{Maruku}.")
    (license license:expat)))

(define-public ruby-http-parser.rb
  (package
    (name "ruby-http-parser.rb")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "http_parser.rb" version))
        (sha256
          (base32
            "15nidriy0v5yqfjsgsra51wmknxci2n2grliz78sf9pga3n0l7gi"))))
    (build-system ruby-build-system)
    (arguments
     ;; No tests
     `(#:tests? #f))
    (native-inputs
     `(("ruby-rake-compiler" ,ruby-rake-compiler)
       ("ruby-rspec" ,ruby-rspec)))
    (home-page "https://github.com/tmm1/http_parser.rb")
    (synopsis "HTTP parser un Ruby")
    (description "This gem is a simple callback-based HTTP request/response
parser for writing http servers, clients and proxies.")
    (license license:expat)))

(define-public ruby-em-websocket
  (package
    (name "ruby-em-websocket")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "em-websocket" version))
        (sha256
          (base32
            "1bsw8vjz0z267j40nhbmrvfz7dvacq4p0pagvyp17jif6mj6v7n3"))))
    (build-system ruby-build-system)
    (arguments
     ;; No tests
     `(#:tests? #f))
    (propagated-inputs
      `(("ruby-eventmachine" ,ruby-eventmachine)
        ("ruby-http-parser.rb" ,ruby-http-parser.rb)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)))
    (home-page "https://github.com/igrigorik/em-websocket")
    (synopsis "EventMachine based WebSocket server")
    (description "Em-websocket is an EventMachine based WebSocket server
implementation.")
    (license license:expat)))

(define-public ruby-rouge
  (package
    (name "ruby-rouge")
    (version "3.2.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rouge" version))
              (sha256
               (base32
                "0h79gn2wmn1wix2d27lgiaimccyj8gvizrllyym500pir408x62f"))))
    (build-system ruby-build-system)
    (arguments `(#:tests? #f)); No rakefile
    (home-page "http://rouge.jneen.net/")
    (synopsis "Code highlighter")
    (description "Rouge is a code highlighter written in Ruby.  It supports more
than 100 languages and outputs HTML or ANSI 256-color text.  Its HTML output
is compatible with stylesheets designed for pygments.")
    (license (list
               ;; rouge is licensed under expat
               license:expat
               ;; pygments is licensed under bsd-2
               license:bsd-2))))

(define-public ruby-rouge-2
  (package
    (inherit ruby-rouge)
    (version "2.2.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rouge" version))
              (sha256
               (base32
                "02kpahk5nkc33yxnn75649kzxaz073wvazr2zyg491nndykgnvcs"))))))

(define-public ruby-hashie
  (package
    (name "ruby-hashie")
    (version "3.6.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "hashie" version))
              (sha256
               (base32
                "13bdzfp25c8k51ayzxqkbzag3wj5gc1jd8h7d985nsq6pn57g5xh"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("bundler" ,bundler)))
    (arguments `(#:tests? #f)); FIXME: Could not locate Gemfile or .bundle/ directory
    (home-page "https://github.com/intridea/hashie")
    (synopsis "Extensions to Ruby Hashes")
    (description "Hashie is a collection of classes and mixins that make Ruby
hashes more powerful.")
    (license license:expat)))

(define-public ruby-heredoc-unindent
  (package
    (name "ruby-heredoc-unindent")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "heredoc_unindent" version))
              (sha256
               (base32
                "14ijr2fsjwhrkjkcaz81d5xnfa4vvgvcflrff83avqw9klm011yw"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("ruby-hoe" ,ruby-hoe)))
    (home-page "https://github.com/adrianomitre/heredoc_unindent")
    (synopsis "Heredoc indentation cleaner")
    (description "This gem removes common margin from indented strings, such
as the ones produced by indented heredocs.  In other words, it strips out
leading whitespace chars at the beginning of each line, but only as much as
the line with the smallest margin.

It is acknowledged that many strings defined by heredocs are just code and
fact is that most parsers are insensitive to indentation.  If, however, the
strings are to be used otherwise, be it for printing or testing, the extra
indentation will probably be an issue and hence this gem.")
    (license license:expat)))

(define-public ruby-safe-yaml
  (package
    (name "ruby-safe-yaml")
    (version "1.0.4")
    (source
     (origin
       ;; TODO Fetch from the git repository so a patch can be applied
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dtao/safe_yaml.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1wnln8xdy8g6kwdj4amm8773xwffqxpf2sxslk6jjh2wxsy1lrig"))
       (patches
        (search-patches "ruby-safe-yaml-add-require-time.patch"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("ruby-rspec" ,ruby-rspec)
       ("ruby-hashie" ,ruby-hashie)
       ("ruby-heredoc-unindent" ,ruby-heredoc-unindent)))
    (arguments
     '(#:test-target "spec"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-TZ
           (lambda _
             ;; This test is dependent on the timezone
             ;; spec/transform/to_date_spec.rb:35
             ;; # SafeYAML::Transform::ToDate converts times to the local
             ;; timezone
             (setenv "TZ" "UTC-11")
             #t)))))
    (home-page "https://github.com/dtao/safe_yaml")
    (synopsis "YAML parser")
    (description "The SafeYAML gem provides an alternative implementation of
YAML.load suitable for accepting user input in Ruby applications.")
    (license license:expat)))

(define-public ruby-mercenary
  (package
    (name "ruby-mercenary")
    (version "0.3.6")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "mercenary" version))
              (sha256
               (base32
                "10la0xw82dh5mqab8bl0dk21zld63cqxb1g16fk8cb39ylc4n21a"))))
    (build-system ruby-build-system)
    (arguments `(#:test-target "spec"))
    (native-inputs
     `(("bundler" ,bundler)))
    (home-page "https://github.com/jekyll/mercenary")
    (synopsis "Command-line apps library in Ruby")
    (description "Mercenary is a lightweight and flexible library for writing
command-line apps in Ruby.")
    (license license:expat)))

(define-public ruby-liquid
  (package
    (name "ruby-liquid")
    (version "4.0.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "liquid" version))
              (sha256
               (base32
                "17fa0jgwm9a935fyvzy8bysz7j5n1vf1x2wzqkdfd5k08dbw3x2y"))))
    (build-system ruby-build-system)
    (arguments `(#:tests? #f)); No rakefile
    (home-page "https://shopify.github.io/liquid/")
    (synopsis "Template language")
    (description "Liquid is a template language written in Ruby.  It is used
to load dynamic content on storefronts.")
    (license license:expat)))

(define-public ruby-forwardable-extended
  (package
    (name "ruby-forwardable-extended")
    (version "2.6.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "forwardable-extended" version))
              (sha256
               (base32
                "15zcqfxfvsnprwm8agia85x64vjzr2w0xn9vxfnxzgcv8s699v0v"))))
    (build-system ruby-build-system)
    (arguments `(#:tests? #f)); Cyclic dependency on luna-rspec-formatters
    (home-page "https://github.com/envygeeks/forwardable-extended")
    (synopsis "Delegation to hashes and instance variables in Forwardable")
    (description "Forwardable Extended provides more @code{Forwardable}
methods for your source as @code{Forwardable::Extended}.")
    (license license:expat)))

(define-public ruby-pathutil
  (package
    (name "ruby-pathutil")
    (version "0.16.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "pathutil" version))
              (sha256
               (base32
                "0wc18ms1rzi44lpjychyw2a96jcmgxqdvy2949r4vvb5f4p0lgvz"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-forwardable-extended" ,ruby-forwardable-extended)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)))
    ;; Fails with: cannot load such file --
    ;; /tmp/guix-build-ruby-pathutil-0.16.0.drv-0/gem/benchmark/support/task
    (arguments `(#:tests? #f))
    (home-page "https://github.com/envygeeks/pathutil")
    (synopsis "Extended implementation of Pathname")
    (description "Pathutil tries to be a faster pure Ruby implementation of
Pathname.")
    (license license:expat)))

(define-public jekyll
  (package
    (name "jekyll")
    (version "3.8.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "jekyll" version))
              (sha256
               (base32
                "1iw90wihk9dscgmppf5v6lysg3kjmnx50mjyl4gghkdb4spw97xk"))))
    (build-system ruby-build-system)
    (arguments
     ;; No rakefile, but a test subdirectory
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-i18n
           (lambda _
             (substitute* ".gemspec"
               (("~> 0.7") ">= 0.7"))
             #t)))))
    (propagated-inputs
     `(("ruby-addressable" ,ruby-addressable)
       ("ruby-colorator" ,ruby-colorator)
       ("ruby-em-websocket" ,ruby-em-websocket)
       ("ruby-i18n" ,ruby-i18n)
       ("ruby-jekyll-sass-converter" ,ruby-jekyll-sass-converter)
       ("ruby-jekyll-watch" ,ruby-jekyll-watch)
       ("ruby-kramdown" ,ruby-kramdown)
       ("ruby-liquid" ,ruby-liquid)
       ("ruby-mercenary" ,ruby-mercenary)
       ("ruby-pathutil" ,ruby-pathutil)
       ("ruby-rouge" ,ruby-rouge-2)
       ("ruby-safe-yaml" ,ruby-safe-yaml)))
    (home-page "https://jekyllrb.com/")
    (synopsis "Static site generator")
    (description "Jekyll is a simple, blog aware, static site generator.")
    (license license:expat)))

(define-public ruby-jekyll-paginate-v2
  (package
    (name "ruby-jekyll-paginate-v2")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "jekyll-paginate-v2" version))
              (sha256
               (base32
                "154bfpyml6abxww9868hhyfvxasl8qhsc5zy2q30c7dxaj0igdib"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("jekyll" ,jekyll)))
    (home-page "https://github.com/sverrirs/jekyll-paginate-v2")
    (synopsis "Pagination Generator for Jekyll 3")
    (description "The Pagination Generator forms the core of the pagination
logic in Jekyll.  It calculates and generates the pagination pages.")
    (license license:expat)))

(define-public ruby-faraday
  (package
    (name "ruby-faraday")
    (version "0.15.4")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "faraday" version))
       (sha256
        (base32
         "0s72m05jvzc1pd6cw1i289chas399q0a14xrwg4rvkdwy7bgzrh0"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f))
    (propagated-inputs
     `(("ruby-multipart-post" ,ruby-multipart-post)))
    (synopsis "Ruby HTTP/REST API client library")
    (description
     "Faraday is a HTTP/REST API client library which provides a common
interface over different adapters.")
    (home-page "https://github.com/lostisland/faraday")
    (license license:expat)))

(define-public ruby-nio4r
  (package
   (name "ruby-nio4r")
   (version "2.3.1")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "nio4r" version))
     (sha256
      (base32
       "1a41ca1kpdmrypjp9xbgvckpy8g26zxphkja9vk7j5wl4n8yvlyr"))))
   (build-system ruby-build-system)
   (arguments
    '(#:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'remove-unnecessary-dependencies
          (lambda _
            (substitute* "spec/spec_helper.rb"
              ;; Coveralls is for uploading test coverage information to an
              ;; online service, and thus unnecessary for building the Guix
              ;; package
              (("require \"coveralls\"") "")
              (("Coveralls\\.wear!") "")
              ;; Remove rspec/retry as we are not retrying the tests
              (("require \"rspec/retry\"") "")
              (("config\\.display_try_failure_messages = true") "")
              (("config\\.verbose_retry = true") ""))
            #t))
        (add-before 'check 'compile
          (lambda _
            (invoke "rake" "compile")
            #t))
        (replace 'check
          (lambda* (#:key tests? #:allow-other-keys)
            (when tests?
              (invoke "rspec"))
            #t)))))
   (native-inputs
    `(("bundler" ,bundler)
      ("ruby-rake-compiler" ,ruby-rake-compiler)
      ("ruby-rspec" ,ruby-rspec)
      ("ruby-rubocop" ,ruby-rubocop)))
   (synopsis "New I/O for Ruby")
   (description
    "@code{nio} provides cross-platform asynchronous I/O primitives in Ruby
for scalable network clients and servers.")
   (home-page "https://github.com/socketry/nio4r")
   (license license:expat)))

(define-public ruby-globalid
  (package
   (name "ruby-globalid")
   (version "0.4.2")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "globalid" version))
     (sha256
      (base32
       "1zkxndvck72bfw235bd9nl2ii0lvs5z88q14706cmn702ww2mxv1"))))
   (build-system ruby-build-system)
   (arguments
    '(;; No included tests
      #:tests? #f))
   (propagated-inputs
    `(("ruby-activesupport" ,ruby-activesupport)))
   (synopsis "Generate URIs idenfitying model instances in Ruby")
   (description
    "@code{GlobalID} provides a way to generate URIs from a model in Ruby that
uniquely identify it.")
   (home-page "https://rubyonrails.org/")
   (license license:expat)))

(define-public ruby-sprockets
  (package
    (name "ruby-sprockets")
    (version "3.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "sprockets" version))
       (sha256
        (base32
         "182jw5a0fbqah5w9jancvfmjbk88h8bxdbwnl4d3q809rpxdg8ay"))))
    (build-system ruby-build-system)
    (arguments
     '(;; No included tests
       #:tests? #f))
    (propagated-inputs
     `(("ruby-concurrent" ,ruby-concurrent)
       ("ruby-rack" ,ruby-rack)))
    (synopsis "Sprockets is a Rack-based asset packaging system")
    (description
     "Sprockets is a Rack-based asset packaging system that concatenates and
serves JavaScript, CoffeeScript, CSS, LESS, Sass, and SCSS.")
    (home-page "https://github.com/rails/sprockets")
    (license license:expat)))

(define-public ruby-mustermann
  (package
    (name "ruby-mustermann")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "mustermann" version))
       (sha256
        (base32
         "0lycgkmnyy0bf29nnd2zql5a6pcf8sp69g9v4xw0gcfcxgpwp7i1"))))
    (build-system ruby-build-system)
    (arguments
     ;; No tests.
     '(#:tests? #f))
    (synopsis "Library implementing patterns that behave like regular expressions")
    (description "Given a string pattern, Mustermann will turn it into an
object that behaves like a regular expression and has comparable performance
characteristics.")
    (home-page "https://github.com/sinatra/mustermann")
    (license license:expat)))

(define-public ruby-sinatra
  (package
    (name "ruby-sinatra")
    (version "2.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "sinatra" version))
       (sha256
        (base32
         "1gasgn5f15myv08k10i16p326pchxjsy37pgqfw0xm66kcc5d7ry"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-mustermann" ,ruby-mustermann)
       ("ruby-rack" ,ruby-rack)
       ("ruby-rack-protection" ,ruby-rack-protection)
       ("ruby-tilt" ,ruby-tilt)))
    (synopsis "DSL for quick web applications creation in Ruby")
    (description
     "Sinatra is a DSL for quickly creating web applications in Ruby with
minimal effort.")
    (home-page "http://sinatrarb.com/")
    (license license:expat)))

(define-public ruby-thin
  (package
    (name "ruby-thin")
    (version "1.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "thin" version))
       (sha256
        (base32
         "0nagbf9pwy1vg09k6j4xqhbjjzrg5dwzvkn4ffvlj76fsn6vv61f"))))
    (build-system ruby-build-system)
    (arguments
     ;; No tests.
     '(#:tests? #f))
    (propagated-inputs
     `(("ruby-daemons" ,ruby-daemons)
       ("ruby-eventmachine" ,ruby-eventmachine)
       ("ruby-rack" ,ruby-rack)))
    (synopsis "Thin and fast web server for Ruby")
    (description "Thin is a Ruby web server that glues together 3 Ruby libraries:
@itemize
@item the Mongrel parser,
@item Event Machine, a network I/O library with high scalability, performance
and stability,
@item Rack, a minimal interface between webservers and Ruby frameworks.
@end itemize\n")
    (home-page "http://code.macournoyer.com/thin/")
    (license license:ruby)))

(define-public ruby-skinny
  (package
    (name "ruby-skinny")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "skinny" version))
       (sha256
        (base32
         "1y3yvx88ylgz4d2s1wskjk5rkmrcr15q3ibzp1q88qwzr5y493a9"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f ; No included tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-gemspec
           (lambda _
             (substitute* ".gemspec"
               (("<eventmachine>.freeze, \\[\\\"~> 1.0.0\"")
                "<eventmachine>, [\">= 1.0.0\"")
               (("<thin>.freeze, \\[\\\"< 1.7\", ") "<thin>, ["))
             #t)))))
    (propagated-inputs
     `(("ruby-eventmachine" ,ruby-eventmachine)
       ("ruby-thin" ,ruby-thin)))
    (synopsis "Simple, upgradable WebSockets for Ruby Thin")
    (description "Skinny is a simple, upgradable WebSockets for Ruby, using
the Thin library.")
    (home-page "https://github.com/sj26/skinny")
    (license license:expat)))

(define-public mailcatcher
  (package
    (name "mailcatcher")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "mailcatcher" version))
       (sha256
        (base32
         "02w1ycyfv7x0sh9799lz7xa65p5qvl5z4pa8a7prb68h2zwkfq0n"))))
    (build-system ruby-build-system)
    (arguments
     ;; Tests require web/assets which is not included in the output.  We
     ;; might be able to fix this by adding the Git repository to the GEM_PATH
     ;; of the tests.  See ruby-mysql2.
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-gemspec
           (lambda _
             (substitute* ".gemspec"
               (("<eventmachine>.freeze, \\[\\\"= 1.0.9.1")
                "<eventmachine>, [\">= 1.0.9.1")
               (("<rack>.freeze, \\[\\\"~> 1.5") "<rack>, [\">= 1.5")
               (("<thin>.freeze, \\[\\\"~> 1.5.0") "<thin>, [\">= 1.5.0")
               (("<sinatra>.freeze, \\[\\\"~> 1.2") "<sinatra>, [\">= 1.2"))
             #t))
         (add-before 'build 'loosen-dependency-contraint
             (lambda _
               (substitute* "lib/mail_catcher.rb"
                 (("\"eventmachine\", \"1.0.9.1\"") "\"eventmachine\", \">= 1.0.9.1\"")
                 (("\"rack\", \"~> 1.5\"") "\"rack\", \">= 1.5\"")
                 (("\"thin\", \"~> 1.5.0\"") "\"thin\", \">= 1.5.0\"")
                 (("\"sinatra\", \"~> 1.2\"") "\"sinatra\", \">= 1.2\""))
               #t)))))
    (inputs
     `(("ruby-eventmachine" ,ruby-eventmachine)
       ("ruby-mail" ,ruby-mail)
       ("ruby-rack" ,ruby-rack)
       ("ruby-sinatra" ,ruby-sinatra)
       ("ruby-skinny" ,ruby-skinny)
       ("ruby-sqlite3" ,ruby-sqlite3)
       ("ruby-thin" ,ruby-thin)))
    (synopsis "SMTP server which catches messages to display them a browser")
    (description
     "MailCatcher runs a super simple SMTP server which catches any message
sent to it to display in a web interface.  Run mailcatcher, set your favourite
app to deliver to smtp://127.0.0.1:1025 instead of your default SMTP server,
then check out http://127.0.0.1:1080 to see the mail.")
    (home-page "https://mailcatcher.me")
    (license license:expat)))
