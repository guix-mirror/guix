;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix utils)
  #:use-module (guix build-system gnu))

(define-public ruby
  (package
    (name "ruby")
    (version "2.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://cache.ruby-lang.org/pub/ruby/2.1/ruby-"
                           version ".tar.gz"))
       (sha256
        (base32
         "00bz6jcbxgnllplk4b9lnyc3w8yd3pz5rn11rmca1s8cn6vvw608"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:parallel-tests? #f
       #:phases
       (alist-cons-after
        ;; Minor patch:
        ;; https://bugs.ruby-lang.org/projects/ruby-trunk/repository/revisions/45225/diff/
        'unpack 'patch-readline
        (lambda _
          (substitute* '("ext/readline/readline.c")
            (("Function ") "rl_hook_func_t ")))
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
         %standard-phases))))
    (inputs
     `(("readline" ,readline)
       ("autoconf" ,autoconf)
       ("openssl" ,openssl)
       ("libffi" ,libffi)
       ("gdbm" ,gdbm)
       ("zlib" ,zlib)))
    (native-search-paths
     (list (search-path-specification
            (variable "GEM_PATH")
            (directories '("lib/ruby/gems/2.1.3")))))
    (synopsis "Ruby programming language interpreter")
    (description "Ruby is a dynamic object-oriented programming language with
a focus on simplicity and productivity.")
    (home-page "https://ruby-lang.org")
    (license license:ruby)))

