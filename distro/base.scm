;;; Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
;;; Copyright (C) 2012 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of Guix.
;;;
;;; Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (distro base)
  #:use-module (guix packages)
  #:use-module (guix http)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils))

;;; Commentary:
;;;
;;; A Guix-based distribution.
;;;
;;; Code:

(define-public libsigsegv
  (package
   (name "libsigsegv")
   (version "2.10")
   (source (source
            (method http-fetch)
            (uri "http://ftp.gnu.org/gnu/libsigsegv/libsigsegv-2.10.tar.gz")
            (sha256
             (nix-base32-string->bytevector  ; TODO: make conversion implicit
              "16hrs8k3nmc7a8jam5j1fpspd6sdpkamskvsdpcw6m29vnis8q44"))))
   (build-system gnu-build-system)
   (outputs '("out" "lib"))                   ; separate libdir from the rest
   (home-page "http://www.gnu.org/software/libsigsegv/")
   (description "GNU libsigsegv, a library to handle page faults in user mode")
   (long-description
"GNU libsigsegv is a library for handling page faults in user mode. A page
fault occurs when a program tries to access to a region of memory that is
currently not available. Catching and handling a page fault is a useful
technique for implementing pageable virtual memory, memory-mapped access to
persistent databases, generational garbage collectors, stack overflow
handlers, distributed shared memory, and more.")
   (license "GPLv2+")))

(define-public gawk
  (package
   (name "gawk")
   (version "4.0.0")
   (source (source
            (method http-fetch)
            (uri "http://ftp.gnu.org/gnu/gawk/gawk-4.0.0.tar.bz2")
            (sha256
             (nix-base32-string->bytevector
              "0sss7rhpvizi2a88h6giv0i7w5h07s2fxkw3s6n1hqvcnhrfgbb0"))))
   (build-system gnu-build-system)
   (arguments '()
              ;; TODO: disable tests on Cygwin
              )
   (inputs `(("libsigsegv" ,libsigsegv)             ; headers
             ("libsigsegv/lib" ,libsigsegv "lib"))) ; library
   (home-page "http://www.gnu.org/software/gawk/")
   (description "GNU implementation of the Awk programming language")
   (long-description
    "Many computer users need to manipulate text files: extract and then
operate on data from parts of certain lines while discarding the rest, make
changes in various text files wherever certain patterns appear, and so on.
To write a program to do these things in a language such as C or Pascal is a
time-consuming inconvenience that may take many lines of code.  The job is
easy with awk, especially the GNU implementation: Gawk.

The awk utility interprets a special-purpose programming language that makes
it possible to handle many data-reformatting jobs with just a few lines of
code.")
   (license "GPLv3+")))

(define-public hello
  (package
   (name "hello")
   (version "2.8")
   (source (source
            (method http-fetch)
            (uri "http://ftp.gnu.org/gnu/hello/hello-2.8.tar.gz")
            (sha256
             (nix-base32-string->bytevector  ; TODO: make conversion implicit
              "0wqd8sjmxfskrflaxywc7gqw7sfawrfvdxd9skxawzfgyy0pzdz6"))))
   (build-system gnu-build-system)
   (arguments '(#:configure-flags
                `("--disable-dependency-tracking"
                  ,(string-append "--with-gawk="  ; for illustration purposes
                                 (assoc-ref %build-inputs "gawk")))))
   (inputs `(("gawk" ,gawk)))
   (description "GNU Hello")
   (long-description "Yeah...")
   (home-page "http://www.gnu.org/software/hello/")
   (license "GPLv3+")))
