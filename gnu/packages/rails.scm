;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Matthew Jordan <matthewjordandevops@yandex.com>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages rails)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages ruby)
  #:use-module (guix build-system ruby))

(define-public ruby-spring
  (package
    (name "ruby-spring")
    (version "1.7.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/rails/spring/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1dd58y0cpsm2izj74yscn0ybfygmgcbbfdw1891g7cq41aai4b35"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "test:unit"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'remove-bump
           (lambda _
             (substitute* "spring.gemspec"
               (("gem.add_development_dependency 'bump'") "")
               (("gem.add_development_dependency 'activesupport'.*")
                "gem.add_development_dependency 'activesupport'\n"))
             (substitute* "Rakefile"
               (("require \\\"bump/tasks\\\"") ""))
             #t)))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-activesupport" ,ruby-activesupport)))
    (synopsis "Ruby on Rails application preloader")
    (description
     "Spring is a Ruby on Rails application preloader.  It speeds up
development by keeping your application running in the background so the
application does need to boot it every time you run a test, rake task or
migration.")
    (home-page "https://github.com/rails/spring")
    (license license:expat)))

(define-public ruby-sass-rails
  (package
    (name "ruby-sass-rails")
    (version "5.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "sass-rails" version))
       (sha256
        (base32
         "1wa63sbsimrsf7nfm8h0m1wbsllkfxvd7naph5d1j6pbc555ma7s"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; No included tests
    (propagated-inputs
     `(("ruby-railties" ,ruby-railties)
       ("ruby-sass" ,ruby-sass)
       ("ruby-sprockets" ,ruby-sprockets)
       ("ruby-sprockets-rails" ,ruby-sprockets-rails)
       ("ruby-tilt" ,ruby-tilt)))
    (synopsis "Sass adapter for the Rails asset pipeline")
    (description
     "This library integrates the SASS stylesheet language into Ruby on
Rails.")
    (home-page "https://github.com/rails/sass-rails")
    (license license:expat)))

(define-public ruby-debug-inspector
  (package
    (name "ruby-debug-inspector")
    (version "0.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "debug_inspector" version))
       (sha256
        (base32
         "0vxr0xa1mfbkfcrn71n7c4f2dj7la5hvphn904vh20j3x4j5lrx0"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "rake" "compile")
             (invoke "ruby" "-Ilib" "-e"
                     (string-append
                      "require 'debug_inspector'; RubyVM::DebugInspector."
                      "open{|dc| p dc.backtrace_locations}")))))))
    (synopsis "Ruby wrapper for the MRI 2.0 debug_inspector API")
    (description
     "This package provides a Ruby wrapper for the MRI 2.0 debug_inspector
API.")
    (home-page
     "https://github.com/banister/debug_inspector")
    (license license:expat)))

(define-public ruby-activemodel
  (package
   (name "ruby-activemodel")
   (version "5.2.2.1")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "activemodel" version))
     (sha256
      (base32
       "1idmvqvpgri34k31s44pjb88rc3jad3yxra7fd1kpidpnv5f3v65"))))
   (build-system ruby-build-system)
   (arguments
    '(;; No included tests
      #:tests? #f))
   (propagated-inputs
    `(("ruby-activesupport" ,ruby-activesupport)))
   (synopsis "Toolkit for building modeling frameworks like Active Record")
   (description
    "This package provides a toolkit for building modeling frameworks like
Active Record.  ActiveSupport handles attributes, callbacks, validations,
serialization, internationalization, and testing.")
   (home-page "https://rubyonrails.org/")
   (license license:expat)))

(define-public ruby-activerecord
  (package
   (name "ruby-activerecord")
   (version "5.2.2.1")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "activerecord" version))
     (sha256
      (base32
       "1c5cz9v7ggpqjxf0fqs1xhy1pb9m34cp31pxarhs9aqb71qjl98v"))))
   (build-system ruby-build-system)
   (arguments
    '(;; No included tests
      #:tests? #f))
   (propagated-inputs
    `(("ruby-activemodel" ,ruby-activemodel)
      ("ruby-activesupport" ,ruby-activesupport)
      ("ruby-arel" ,ruby-arel)))
   (synopsis "Ruby library to connect to relational databases")
   (description
    "Active Record connects classes to relational database table to establish
an almost zero-configuration persistence layer for applications.")
   (home-page "https://rubyonrails.org")
   (license license:expat)))

(define-public ruby-rails-html-sanitizer
  (package
    (name "ruby-rails-html-sanitizer")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "rails-html-sanitizer" version))
       (sha256
        (base32
         "1gv7vr5d9g2xmgpjfq4nxsqr70r9pr042r9ycqqnfvw5cz9c7jwr"))))
    (build-system ruby-build-system)
    (arguments
     '(;; No included tests
       #:tests? #f))
    (propagated-inputs
     `(("ruby-loofah" ,ruby-loofah)))
    (synopsis "HTML sanitization for Rails applications")
    (description
     "This gem is used to handle HTML sanitization in Rails applications.  If
you need similar functionality in non Rails apps consider using Loofah
directly.")
    (home-page "https://github.com/rails/rails-html-sanitizer")
    (license license:expat)))

(define-public ruby-rails-dom-testing
  (package
   (name "ruby-rails-dom-testing")
   (version "2.0.2")
   (source
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/rails/rails-dom-testing.git")
            (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
       (base32
        "0zrg6x1w3wjgklbhcphjmggl11jx5s8cl21qjqij7wknm412i5wl"))))
   (build-system ruby-build-system)
   (native-inputs
    `(("bundler" ,bundler)))
   (propagated-inputs
    `(("ruby-activesupport" ,ruby-activesupport)
      ("ruby-nokogiri" ,ruby-nokogiri)))
   (synopsis "Compare HTML DOMs and assert certain elements exists")
   (description
    "This gem can compare HTML and assert certain elements exists.  This is
useful when writing tests.")
   (home-page "https://github.com/rails/rails-dom-testing")
   (license license:expat)))

(define-public ruby-actionview
  (package
   (name "ruby-actionview")
   (version "5.2.2.1")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "actionview" version))
     (sha256
      (base32
       "0832vlx37rly8ryfgi01b20mld8b3bv9cg62n5wax4zpzgn6jdxb"))))
   (build-system ruby-build-system)
   (arguments
    '(;; No included tests
      #:tests? #f))
   (propagated-inputs
    `(("ruby-activesupport" ,ruby-activesupport)
      ("ruby-builder" ,ruby-builder)
      ("ruby-erubi" ,ruby-erubi)
      ("ruby-rails-dom-testing" ,ruby-rails-dom-testing)
      ("ruby-rails-html-sanitizer" ,ruby-rails-html-sanitizer)))
   (synopsis "Conventions and helpers for building web pages")
   (description
    "ActionView provides conventions and helpers for building web pages in
Ruby.")
   (home-page "https://rubyonrails.org/")
   (license license:expat)))

(define-public ruby-actionpack
  (package
   (name "ruby-actionpack")
   (version "5.2.2.1")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "actionpack" version))
     (sha256
      (base32
       "1lxqzxa728dqg42yw0q4hqkaawqagiw1k0392an2ghjfgb16pafx"))))
   (build-system ruby-build-system)
   (arguments
    '(;; No included tests
      #:tests? #f))
   (propagated-inputs
    `(("ruby-actionview" ,ruby-actionview)
      ("ruby-activesupport" ,ruby-activesupport)
      ("ruby-rack" ,ruby-rack)
      ("ruby-rack-test" ,ruby-rack-test)
      ("ruby-rails-dom-testing" ,ruby-rails-dom-testing)
      ("ruby-rails-html-sanitizer" ,ruby-rails-html-sanitizer)))
   (synopsis "Conventions for building and testing MVC web applications")
   (description
    "ActionPack provides conventions for building and testing MVC web
applications.  These work with any Rack-compatible server.")
   (home-page "https://rubyonrails.org/")
   (license license:expat)))

(define-public ruby-actioncable
  (package
   (name "ruby-actioncable")
   (version "5.2.2.1")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "actioncable" version))
     (sha256
      (base32
       "1x5fxhsr2mxq5r6258s48xsn7ld081d3qaavppvj7yp7w9vqn871"))))
   (build-system ruby-build-system)
   (arguments
    '(;; No included tests
      #:tests? #f))
   (propagated-inputs
    `(("ruby-actionpack" ,ruby-actionpack)
      ("ruby-nio4r" ,ruby-nio4r)
      ("ruby-websocket-driver" ,ruby-websocket-driver)))
   (synopsis "Integrate integrates WebSockets with Rails applications")
   (description
    "Action Cable integrates WebSockets with Rails applications.  Through
WebSockets it allows for real-time features in web applications.")
   (home-page "https://rubyonrails.org/")
   (license license:expat)))

(define-public ruby-activejob
  (package
   (name "ruby-activejob")
   (version "5.2.2.1")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "activejob" version))
     (sha256
      (base32
       "1zma452lc3qp4a7r10zbdmsci0kv9a3gnk4da2apbdrc8fib5mr3"))))
   (build-system ruby-build-system)
   (arguments
    '(;; No included tests
      #:tests? #f))
   (propagated-inputs
    `(("ruby-activesupport" ,ruby-activesupport)
      ("ruby-globalid" ,ruby-globalid)))
   (synopsis "Declare job classes for multiple backends")
   (description
    "ActiveJob allows declaring job classes in a common way across Rails
applications.")
   (home-page "https://rubyonrails.org/")
   (license license:expat)))

(define-public ruby-activestorage
  (package
    (name "ruby-activestorage")
    (version "5.2.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "activestorage" version))
       (sha256
        (base32
         "155xpbzrz0kr0argx0vsh5prvadd2h1g1m61kdiabvfy2iygc02n"))))
    (build-system ruby-build-system)
    (arguments
     '(;; No included tests
       #:tests? #f))
    (propagated-inputs
     `(("ruby-actionpack" ,ruby-actionpack)
       ("ruby-activerecord" ,ruby-activerecord)
       ("ruby-marcel" ,ruby-marcel)))
    (synopsis "Integrate file storage services in to Rails applications")
    (description
     "ActiveStorage integrates file storage services with Rails applications,
allowing files to be attached to ActiveRecord models..")
    (home-page "https://rubyonrails.org/")
    (license license:expat)))

(define-public ruby-actionmailer
  (package
   (name "ruby-actionmailer")
   (version "5.2.2.1")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "actionmailer" version))
     (sha256
      (base32
       "10n2v2al68rsq5ghrdp7cpycsc1q0m19fcd8cd5i528n30nl23iw"))))
   (build-system ruby-build-system)
   (arguments
    '(;; No included tests
      #:tests? #f))
   (propagated-inputs
    `(("ruby-actionpack" ,ruby-actionpack)
      ("ruby-actionview" ,ruby-actionview)
      ("ruby-activejob" ,ruby-activejob)
      ("ruby-mail" ,ruby-mail)
      ("ruby-rails-dom-testing" ,ruby-rails-dom-testing)))
   (synopsis "Work with emails using the controller/view pattern")
   (description
    "Compose, deliver, receive, and test emails using the controller/view
pattern.  Including support for multipart email and attachments.")
   (home-page "https://rubyonrails.org/")
   (license license:expat)))

(define-public ruby-railties
  (package
   (name "ruby-railties")
   (version "5.2.2.1")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "railties" version))
     (sha256
      (base32
       "0al6mvh2jvr3n7cxkx0yvhgiiarby6gxc93vl5xg1yxkvx27qzd6"))))
   (build-system ruby-build-system)
   (arguments
    '(;; No included tests
      #:tests? #f))
   (propagated-inputs
    `(("ruby-actionpack" ,ruby-actionpack)
      ("ruby-activesupport" ,ruby-activesupport)
      ("ruby-method-source" ,ruby-method-source)
      ("ruby-thor" ,ruby-thor)))
   (synopsis "Rails internals, including application bootup and generators")
   (description
    "@code{railties} provides the core Rails internals including handling
application bootup, plugins, generators, and Rake tasks.")
   (home-page "https://rubyonrails.org/")
   (license license:expat)))

(define-public ruby-sprockets-rails
  (package
   (name "ruby-sprockets-rails")
   (version "3.2.1")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "sprockets-rails" version))
     (sha256
      (base32
       "0ab42pm8p5zxpv3sfraq45b9lj39cz9mrpdirm30vywzrwwkm5p1"))))
   (build-system ruby-build-system)
   (arguments
    '(;; No included tests
      #:tests? #f))
   (propagated-inputs
    `(("ruby-actionpack" ,ruby-actionpack)
      ("ruby-activesupport" ,ruby-activesupport)
      ("ruby-sprockets" ,ruby-sprockets)))
   (synopsis "Sprockets Rails integration")
   (description
    "Provides Sprockets implementation for the Rails Asset Pipeline.")
   (home-page
    "https://github.com/rails/sprockets-rails")
   (license license:expat)))

(define-public ruby-web-console
  (package
    (name "ruby-web-console")
    (version "3.7.0")
    (source
     (origin
       ;; Download from GitHub as test files are not provided in the gem.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rails/web-console.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0ir999p8cnm3l3zwbgpwxxcq1vwkj8d0d3r24362cyaf4v1rglq2"))))
    (build-system ruby-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-Gemfile
           (lambda _
             (substitute* "Gemfile"
               ;; Remove the github bit from the Gemfile, so that the Guix
               ;; packages are used.
               ((", github: .*") "\n")
               ;; The usual methods of not loading this group don't work, so
               ;; patch the Gemfile.
               (("group :development") "[].each")
               ;; tzinfo-data is propagated by ruby-activesupport, but it
               ;; needs to be in the Gemfile to become available.
               (("group :test do") "group :test do\n  gem 'tzinfo-data'"))
             #t))
         (add-after 'unpack 'fix-mocha-minitest-require
           (lambda _
             (substitute* "test/test_helper.rb"
               ;; This chanegd in recent versions of Mocha
               (("mocha/minitest") "mocha/mini_test"))
             #t)))))
    (propagated-inputs
     `(("ruby-actionview" ,ruby-actionview)
       ("ruby-activemodel" ,ruby-activemodel)
       ("ruby-bindex" ,ruby-bindex)
       ("ruby-railties" ,ruby-railties)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rails" ,ruby-rails)
       ("ruby-mocha" ,ruby-mocha)
       ("ruby-simplecov" ,ruby-simplecov)))
    (synopsis "Debugging tool for your Ruby on Rails applications")
    (description
     "This package allows you to create an interactive Ruby session in your
browser.  Those sessions are launched automatically in case of an error and
can also be launched manually in any page.")
    (home-page "https://github.com/rails/web-console")
    (license license:expat)))

(define-public ruby-with-advisory-lock
  (package
    (name "ruby-with-advisory-lock")
    (version "4.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "with_advisory_lock" version))
       (sha256
        (base32
         "1k37hxgmaqgsd54gplm5xim9nw3ghvqsbzaw7q4q64ha1nbd9a41"))))
    (build-system ruby-build-system)
    (arguments
     '(#:tests? #f)) ; TODO Tests require a running MySQL service
    (propagated-inputs
     `(("ruby-activerecord" ,ruby-activerecord)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-yard" ,ruby-yard)
       ("ruby-mysql2" ,ruby-mysql2)))
    (synopsis "Advisory locking for ActiveRecord")
    (description
     "The With advisory lock gem adds advisory locking to ActiveRecord for
PostgreSQL and MySQL.  SQLite is also supported, but this uses the filesystem
for locks.")
    (home-page "https://closuretree.github.io/with_advisory_lock/")
    (license license:expat)))

(define-public ruby-rails
  (package
   (name "ruby-rails")
   (version "5.2.2.1")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "rails" version))
     (sha256
      (base32
       "1jxmwrykwgbn116hhmi7h75hcsdifhj89wk12m7ch2f3mn1lrmp9"))))
   (build-system ruby-build-system)
   (arguments
    '(#:phases
      (modify-phases %standard-phases
        ;; This gem acts as glue between the gems that actually make up
        ;; Rails. The important thing to check is that the gemspec matches up
        ;; with the Guix packages and Rubygems can successfully activate the
        ;; Rails gem.
        ;;
        ;; The following check phase tests this.
        (delete 'check)
        (add-after 'install 'check
          (lambda* (#:key tests? outputs #:allow-other-keys)
            (setenv "GEM_PATH"
                    (string-append
                     (getenv "GEM_PATH")
                     ":"
                     (assoc-ref outputs "out") "/lib/ruby/vendor_ruby"))
            (when tests?
              (invoke "ruby" "-e" "gem 'rails'"))
            #t)))))
   (propagated-inputs
    `(("ruby-activesupport" ,ruby-activesupport)
      ("ruby-actionpack" ,ruby-actionpack)
      ("ruby-actionview" ,ruby-actionview)
      ("ruby-activemodel" ,ruby-activemodel)
      ("ruby-activerecord" ,ruby-activerecord)
      ("ruby-actionmailer" ,ruby-actionmailer)
      ("ruby-activejob" ,ruby-activejob)
      ("ruby-actioncable" ,ruby-actioncable)
      ("ruby-activestorage" ,ruby-activestorage)
      ("ruby-railties" ,ruby-railties)
      ("bundler" ,bundler)
      ("ruby-sprockets-rails" ,ruby-sprockets-rails)))
   (synopsis "Full-stack web framework optimized for programmer happiness")
   (description
    "Ruby on Rails is a full-stack web framework optimized for programmer
happiness and sustainable productivity.  It encourages beautiful code by
favoring convention over configuration.")
   (home-page "https://rubyonrails.org/")
   (license license:expat)))
