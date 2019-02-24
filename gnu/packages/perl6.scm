;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages perl6)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system perl)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pkg-config))

(define-public moarvm
  (package
    (name "moarvm")
    (version "2019.03")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://moarvm.org/releases/MoarVM-"
                            version ".tar.gz"))
        (sha256
         (base32
          "017w1zvr6yl0cgjfc1b3ddlc6vjw9q8p7alw1vvsckw95190xc14"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            ;(delete-file-recursively "3rdparty/dynasm") ; JIT
            (delete-file-recursively "3rdparty/dyncall")
            (delete-file-recursively "3rdparty/freebsd")
            (delete-file-recursively "3rdparty/libatomicops")
            (delete-file-recursively "3rdparty/libuv")
            (delete-file-recursively "3rdparty/libtommath")
            (delete-file-recursively "3rdparty/msinttypes")
            #t))))
    (build-system perl-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out        (assoc-ref outputs "out"))
                   (pkg-config (assoc-ref inputs "pkg-config")))
               (setenv "LDFLAGS" (string-append "-Wl,-rpath=" out "/lib"))
               (invoke "perl" "Configure.pl"
                       "--prefix" out
                       "--pkgconfig" (string-append pkg-config "/bin/pkg-config")
                       "--has-libtommath"
                       "--has-libatomic_ops"
                       "--has-libffi"
                       "--has-libuv")))))))
    (home-page "https://moarvm.org/")
    ;; These should be inputs but moar.h can't find them when building rakudo
    (propagated-inputs
     `(("libatomic-ops" ,libatomic-ops)
       ("libtommath" ,libtommath-1.0)
       ("libuv" ,libuv)))
    (inputs
     `(("libffi" ,libffi)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (synopsis "VM for NQP And Rakudo Perl 6")
    (description
     "Short for \"Metamodel On A Runtime\", MoarVM is a modern virtual machine
built for the Rakudo Perl 6 compiler and the NQP Compiler Toolchain.  Highlights
include:

@itemize
@item Great Unicode support, with strings represented at grapheme level
@item Dynamic analysis of running code to identify hot functions and loops, and
perform a range of optimizations, including type specialization and inlining
@item Support for threads, a range of concurrency control constructs, and
asynchronous sockets, timers, processes, and more
@item Generational, parallel, garbage collection
@item Support for numerous language features, including first class functions,
exceptions, continuations, runtime loading of code, big integers and interfacing
with native libraries.
@end itemize")
    (license license:artistic2.0)))
