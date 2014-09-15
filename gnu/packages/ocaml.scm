;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
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

(define-module (gnu packages ocaml)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages curl))

(define-public ocaml
  (package
    (name "ocaml")
    (version "4.00.1")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://caml.inria.fr/pub/distrib/ocaml-4.00/ocaml-"
                   version ".tar.gz"))
             (sha256
              (base32
               "0yp86napnvbi2jgxr6bk1235bmjdclgzrzgq4mhwv87l7dymr3dl"))))
    (build-system gnu-build-system)
    (arguments
       `(#:modules ((guix build gnu-build-system)
                    (guix build utils)
                    (srfi srfi-1))
         #:phases (alist-replace
                   'configure
                   (lambda* (#:key outputs #:allow-other-keys)
                     ;; OCaml uses "-prefix <prefix>" rather than the usual
                     ;; "--prefix=<prefix>".
                     (let ((out (assoc-ref outputs "out")))
                      (zero? (system* "./configure" "-prefix" out
                                      "-mandir"
                                      (string-append out "/share/man")))))
                   (alist-replace
                    'build
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; "make" does not do anything, we must use
                      ;; "make world.opt".
                      (zero? (system* "make" "world.opt")))
                    (alist-replace
                     'check-after-install
                     (lambda* (#:key outputs #:allow-other-keys)
                       ;; There does not seem to be a "check" or "test" target.
                       (zero? (system "cd testsuite && make all")))
                     (let ((check (assq-ref %standard-phases 'check)))
                      ;; OCaml assumes that "make install" is run before
                      ;; launching the tests.
                      (alist-cons-after
                       'install 'check-after-install
                       check
                       (alist-delete 'check %standard-phases))))))))
    (inputs `(("perl" ,perl)))
    (home-page "http://caml.inria.fr/")
    (synopsis "The OCaml programming language")
    (description
     "OCaml is a general purpose industrial-strength programming language with
an emphasis on expressiveness and safety. Developed for more than 20 years at
Inria it benefits from one of the most advanced type systems and supports
functional, imperative and object-oriented styles of programming.")
    (license (list qpl gpl2))))

(define-public opam
  (package
    (name "opam")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              ;; Use the '-full' version, which includes all the dependencies.
              (uri (string-append
                    "https://github.com/ocaml/opam/releases/download/"
                    version "/opam-full-" version ".tar.gz")
               ;; (string-append "https://github.com/ocaml/opam/archive/"
               ;;                    version ".tar.gz")
               )
              (sha256
               (base32
                "1frzqkx6yn1pnyd9qz3bv3rbwv74bmc1xji8kl41r1dkqzfl3xqv"))))
    (build-system gnu-build-system)
    (arguments
     '(;; Sometimes, 'make -jX' would fail right after ./configure with
       ;; "Fatal error: exception End_of_file".
       #:parallel-build? #f

       ;; For some reason, 'ocp-build' needs $TERM to be set.
       #:make-flags '("TERM=screen")
       #:test-target "tests"

       ;; FIXME: There's an obscure test failure:
       ;;   …/_obuild/opam/opam.asm install P1' failed.
       #:tests? #f

       #:phases (alist-cons-before
                 'build 'pre-build
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let ((bash (assoc-ref inputs "bash")))
                     (substitute* "src/core/opamSystem.ml"
                       (("\"/bin/sh\"")
                        (string-append "\"" bash "/bin/sh\"")))))
                 (alist-cons-before
                  'check 'pre-check
                  (lambda _
                    (setenv "HOME" (getcwd))
                    (and (system "git config --global user.email guix@gnu.org")
                         (system "git config --global user.name Guix")))
                  %standard-phases))))
    (native-inputs
     `(("git" ,git)                               ;for the tests
       ("python" ,python)))                       ;for the tests
    (inputs
     `(("ocaml" ,ocaml)
       ("ncurses" ,ncurses)
       ("curl" ,curl)))
    (home-page "http://opam.ocamlpro.com/")
    (synopsis "Package manager for OCaml")
    (description
     "OPAM is a tool to manage OCaml packages.  It supports multiple
simultaneous compiler installations, flexible package constraints, and a
Git-friendly development workflow.")

    ;; The 'LICENSE' file waives some requirements compared to LGPLv3.
    (license lgpl3)))
