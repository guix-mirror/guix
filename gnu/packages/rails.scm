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
