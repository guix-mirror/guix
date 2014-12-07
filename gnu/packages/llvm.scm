;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
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

(define-module (gnu packages llvm)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xml))

(define-public llvm
  (package
    (name "llvm")
    (version "3.5.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://llvm.org/releases/"
                          version "/llvm-" version ".src.tar.xz"))
      (sha256
       (base32
        "00swb43mzlvda8306arlg2jw7g6k3acwfccgf1k4c2pgd3rrkq98"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("python" ,python-wrapper)
       ("perl"   ,perl)))
    (arguments
     `(#:phases (alist-cons-before
                 'build 'link-lib-for-build-exec
                 (lambda* (#:key outputs #:allow-other-keys)
                   ;; This is a hacky fix that will allow binaries to run
                   ;; before being installed.  -DCMAKE_SKIP_BUILD_RPATH=FALSE
                   ;; seems to not help.  Nixpkgs does the same.
                   (let* ((out       (assoc-ref outputs "out"))
                          (out-lib   (string-append out "/lib"))
                          (build-lib (string-append (getcwd) "/lib")))
                     (mkdir-p out)
                     (symlink build-lib out-lib)))
                 (alist-cons-after
                  'build 'cleanup-out
                  (lambda* (#:key outputs #:allow-other-keys)
                    ;; Cleanup the symlink that was created previously.  Let
                    ;; the install phase repopulate out.
                    (delete-file-recursively (assoc-ref outputs "out")))
                  %standard-phases))))
    (home-page "http://www.llvm.org")
    (synopsis "Optimizing compiler infrastructure")
    (description
     "LLVM is a compiler infrastructure designed for compile-time, link-time, runtime,
and idle-time optimization of programs from arbitrary programming languages.
It currently supports compilation of C and C++ programs, using front-ends
derived from GCC 4.0.1.  A new front-end for the C family of languages is in
development.  The compiler infrastructure includes mirror sets of programming
tools as well as libraries with equivalent functionality.")
    (license ncsa)))

(define-public clang
  (package
    (name "clang")
    (version (package-version llvm))
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://llvm.org/releases/"
                           version "/cfe-" version ".src.tar.xz"))
       (sha256
        (base32
         "12yv3jwdjcbkrx7zjm8wh4jrvb59v8fdw4mnmz3zc1jb00p9k07w"))))
    ;; Using cmake allows us to treat llvm as an external library.  There
    ;; doesn't seem to be any way to do this with clang's autotools-based
    ;; build system.
    (build-system cmake-build-system)
    (native-inputs (package-native-inputs llvm))
    (inputs
     `(("libxml2" ,libxml2)
       ,@(package-inputs llvm)))
    (propagated-inputs
     `(("llvm" ,llvm)))
    (arguments `(#:configure-flags '("-DCLANG_INCLUDE_TESTS=True")))
    (home-page "http://clang.llvm.org")
    (synopsis "C language family frontend for LLVM")
    (description
     "Clang is a compiler front end for the C, C++, Objective-C and
Objective-C++ programming languages.  It uses LLVM as its back end.  The Clang
project includes the Clang front end, the Clang static analyzer, and several
code analysis tools.")
    (license ncsa)))
