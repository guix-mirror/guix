;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
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
  #:use-module (gnu packages perl))

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
                      (zero? (system* "./configure" "-prefix" out))))
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
