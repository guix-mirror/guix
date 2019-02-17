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
   (version "5.2.2")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "activemodel" version))
     (sha256
      (base32
       "1xmwi3mw8g4shbjvkhk72ra3r5jccbdsd4piphqka2y1h8s7sxvi"))))
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
   (version "5.2.2")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "activerecord" version))
     (sha256
      (base32
       "19a0sns6a5wz2wym25lb1dv4lbrrl5sd1n15s5ky2636znmhz30y"))))
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
   (version "5.2.2")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "actionview" version))
     (sha256
      (base32
       "1lz04drbi1z0xhvb8jnr14pbf505lilr02arahxq7y3mxiz0rs8z"))))
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
   (version "5.2.2")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "actionpack" version))
     (sha256
      (base32
       "0iwhbqqn0cm39dq040iwq8cfyclqk3kyzwlp5k3j5cz8k2668wws"))))
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
   (version "5.2.2")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "actioncable" version))
     (sha256
      (base32
       "0826k5ch0l03f9yrkxy69aiv039z4qi00lnahw2rzywd2iz6r68x"))))
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
   (version "5.2.2")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "activejob" version))
     (sha256
      (base32
       "1jjkl62x2aprg55x9rpm0h2c82vr2qr989hg3l9r21l01q4822ir"))))
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
    (version "5.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "activestorage" version))
       (sha256
        (base32
         "0c72837098sw384vk6dmrb2p7q3wx4swnibk6sw9dp4hn1vc4p31"))))
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
   (version "5.2.2")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "actionmailer" version))
     (sha256
      (base32
       "0sfpb8s95cmkpp9ybyp2c88r55r5llscmmnkfwcwgasz9ncjiq5n"))))
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
   (version "5.2.2")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "railties" version))
     (sha256
      (base32
       "00pnylmbz4c46mxw5lhxi8h39lndfg6fs1hpd0qd6swnjhkqsr1l"))))
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
   (version "5.2.2")
   (source
    (origin
     (method url-fetch)
     (uri (rubygems-uri "rails" version))
     (sha256
      (base32
       "1m9cszds68dsiycciiayd3c9g90s2yzn1izkr3gpgqkfw6dmvzyr"))))
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
