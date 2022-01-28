;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2017 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 nee <nee.git@cock.li>
;;; Copyright © 2018, 2019, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Nikita <nikita@n0.is>
;;; Copyright © 2021 Oskar Köök <oskar@maatriks.ee>
;;; Copyright © 2021 Cees de Groot <cg@evrl.com>
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
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages erlang)
  #:use-module (gnu packages version-control))

(define-public elixir
  (package
    (name "elixir")
    (version "1.13.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/elixir-lang/elixir")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1d3mk7abn3rk536vmsfbm4smj52rljff6s40sndg0i7p6dl3kzxa"))
       (patches (search-patches "elixir-path-length.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:test-target "test"
      #:parallel-tests? #f ;see <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=32171#23>
      #:make-flags #~(list (string-append "PREFIX=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'make-git-checkout-writable
            (lambda _
              (for-each make-file-writable (find-files "."))))
          (add-after 'make-git-checkout-writable 'replace-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* '("lib/elixir/lib/system.ex"
                             "lib/mix/lib/mix/scm/git.ex")
                (("(cmd\\(['\"])git" _ prefix)
                 (string-append prefix
                                (search-input-file inputs "/bin/git"))))
              (substitute* '("lib/mix/lib/mix/release.ex"
                             "lib/mix/lib/mix/tasks/release.init.ex")
                (("#!/bin/sh")
                 (string-append "#!" (search-input-file inputs "sh"))))
              (substitute* "bin/elixir"
                (("^ERTS_BIN=$")
                 (string-append
                  "ERTS_BIN="
                  ;; Elixir Releases will prepend to ERTS_BIN the path of
                  ;; a copy of erl.  We detect if a release is being generated
                  ;; by checking the initial ERTS_BIN value: if it's empty, we
                  ;; are not in release mode and can point to the actual erl
                  ;; binary in Guix store.
                  "\nif [ -z \"$ERTS_BIN\" ]; then ERTS_BIN="
                  (string-drop-right (search-input-file inputs "/bin/erl") 3)
                  "; fi\n")))
              (substitute* "bin/mix"
                (("#!/usr/bin/env elixir")
                 (string-append "#!" #$output "/bin/elixir")))))
          (add-before 'build 'make-current
            ;; The Elixir compiler checks whether or not to compile files by
            ;; inspecting their timestamps.  When the timestamp is equal to the
            ;; epoch no compilation will be performed.  Some tests fail when
            ;; files are older than Jan 1, 2000.
            (lambda _
              (for-each (lambda (file)
                          (let ((recent 1400000000))
                            (utime file recent recent 0 0)))
                        (find-files "." ".*"))))
          (add-before 'check 'set-home
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Some tests require access to a home directory.
              (setenv "HOME" "/tmp")))
          (delete 'configure))))
    (inputs
     (list erlang git))
    (home-page "https://elixir-lang.org/")
    (synopsis "Elixir programming language")
    (description "Elixir is a dynamic, functional language used to build
scalable and maintainable applications.  Elixir leverages the Erlang VM, known
for running low-latency, distributed and fault-tolerant systems, while also
being successfully used in web development and the embedded software domain.")
    (license license:asl2.0)))
