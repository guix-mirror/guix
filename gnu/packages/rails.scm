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
