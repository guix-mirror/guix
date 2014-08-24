;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages bdw-gc)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages pkg-config))

(define-public libgc-7.2
  (package
   (name "libgc")
   (version "7.2f")
   (source (origin
            (method url-fetch)
            (uri (string-append "http://www.hboehm.info/gc/gc_source/gc-"
                                version ".tar.gz"))
            (sha256
             (base32
              "119x7p1cqw40mpwj80xfq879l9m1dkc7vbc1f3bz3kvkf8bf6p16"))))
   (build-system gnu-build-system)
   (arguments
    ;; Make it so that we don't rely on /proc.  This is especially useful in
    ;; an initrd run before /proc is mounted.
    '(#:configure-flags '("CPPFLAGS=-DUSE_LIBC_PRIVATES")))
   (outputs '("out" "debug"))
   (synopsis "The Boehm-Demers-Weiser conservative garbage collector
for C and C++")
   (description
    "The Boehm-Demers-Weiser conservative garbage collector can be used
as a garbage collecting replacement for C malloc or C++ new.  It allows
you to allocate memory basically as you normally would, without
explicitly deallocating memory that is no longer useful.  The collector
automatically recycles memory when it determines that it can no longer
be otherwise accessed.

The collector is also used by a number of programming language
implementations that either use C as intermediate code, want to
facilitate easier interoperation with C libraries, or just prefer the
simple collector interface.

Alternatively, the garbage collector may be used as a leak detector for
C or C++ programs, though that is not its primary goal.")
   (home-page "http://www.hboehm.info/gc/")

   (license (x11-style (string-append home-page "license.txt")))))

(define-public libatomic-ops
  (package
    (name "libatomic-ops")
    (version "7.4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.ivmaisoft.com/_bin/atomic_ops/libatomic_ops-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1pdm0h1y7bgkczr8byg20r6bq15m5072cqm5pny4f9crc9gn3yh4"))))
    (build-system gnu-build-system)
    (outputs '("out" "debug"))
    (synopsis "Accessing hardware atomic memory update operations")
    (description
     "This C library provides semi-portable access to hardware-provided atomic
memory update operations on a number architectures.  These might allow you to
write code that does more interesting things in signal handlers, write
lock-free code, experiment with thread programming paradigms, etc.")
    (home-page "https://github.com/ivmai/libatomic_ops/")

    ;; Some source files are X11-style, others are GPLv2+.
    (license gpl2+)))

(define-public libgc
  (package (inherit libgc-7.2)
    (version "7.4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.hboehm.info/gc/gc_source/gc-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "18mg28rr6kwr5clc65k4l4hkyy4kd16amx831sjf8q2lqkbhlck3"))))

    ;; New dependencies.
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("libatomic-ops" ,libatomic-ops)))

    ;; 'USE_LIBC_PRIVATES' is now the default.
    (arguments '())))
