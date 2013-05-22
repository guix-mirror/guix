;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix build-system gnu))

(define-public libgc
  (package
   (name "libgc")
   (version "7.2alpha6")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "http://www.hpl.hp.com/personal/Hans_Boehm/gc/gc_source/gc-"
                  version ".tar.gz"))
            (sha256
             (base32
              "05jwadjbrv8pr7z9cb4miskicxqpxm0pca4h2rg5cgbpajr2bx7b"))))
   (build-system gnu-build-system)
   ;; TODO: Build with -DUSE_LIBC_PRIVATES (see make-bootstrap.scm).
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
   (home-page "http://www.hpl.hp.com/personal/Hans_Boehm/gc/")

   ;; permissive X11-style license:
   ;; http://www.hpl.hp.com/personal/Hans_Boehm/gc/license.txt
   (license x11)))

(define-public libgc-7.2
  ;; This is the latest final release of the 7.2 series.
  ;; TODO: Use it as the default when doing a core-updates.
  (package (inherit libgc)
    (version "7.2d")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://www.hpl.hp.com/personal/Hans_Boehm/gc/gc_source/gc-"
                   version ".tar.gz"))
             (sha256
              (base32
               "0phwa5driahnpn79zqff14w9yc8sn3599cxz91m78hqdcpl0mznr"))))))
