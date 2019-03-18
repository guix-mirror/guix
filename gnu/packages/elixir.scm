;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2017 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 nee <nee.git@cock.li>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 ng0 <ng0@n0.is>
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

(define-module (gnu packages elixir)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages erlang)
  #:use-module (gnu packages version-control))

(define-public elixir
  (package
    (name "elixir")
    (version "1.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/elixir-lang/elixir"
                                  "/archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "08lq5p82pnicd49gsj6r2zsbg35wrk6098nrrjb28fcrm5p6736y"))
	      (patches (search-patches "elixir-path-length.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:parallel-tests? #f ;see <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=32171#23>
       #:make-flags (list (string-append "PREFIX="
                                         (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'replace-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* '("lib/elixir/lib/system.ex"
                              "lib/mix/lib/mix/scm/git.ex")
                 (("(cmd\\(['\"])git" _ prefix)
                  (string-append prefix (which "git"))))
               (substitute* "bin/elixir"
                 (("ERL_EXEC=\"erl\"")
                  (string-append "ERL_EXEC=" (which "erl"))))
               (substitute* "bin/mix"
                 (("#!/usr/bin/env elixir")
                  (string-append "#!" out "/bin/elixir"))))
             #t))
         (add-before 'build 'make-current
           ;; The Elixir compiler checks whether or not to compile files by
           ;; inspecting their timestamps.  When the timestamp is equal to the
           ;; epoch no compilation will be performed.  Some tests fail when
           ;; files are older than Jan 1, 2000.
           (lambda _
             (for-each (lambda (file)
                         (let ((recent 1400000000))
                           (utime file recent recent 0 0)))
                       (find-files "." ".*"))
             #t))
         (add-before 'check 'set-home
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Some tests require access to a home directory.
             (setenv "HOME" "/tmp")
             #t))
         (delete 'configure))))
    (inputs
     `(("erlang" ,erlang)
       ("git" ,git)))
    (home-page "https://elixir-lang.org/")
    (synopsis "Elixir programming language")
    (description "Elixir is a dynamic, functional language used to build
scalable and maintainable applications.  Elixir leverages the Erlang VM, known
for running low-latency, distributed and fault-tolerant systems, while also
being successfully used in web development and the embedded software domain.")
    (license license:asl2.0)))
