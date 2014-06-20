;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages libunistring)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public libunistring
  (package
   (name "libunistring")
   (version "0.9.3")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "mirror://gnu/libunistring/libunistring-"
                  version ".tar.gz"))
            (sha256
             (base32
              "18q620269xzpw39dwvr9zpilnl2dkw5z5kz3mxaadnpv4k3kw3b1"))))
   (propagated-inputs '())                  ; FIXME: add libiconv when !glibc
   (build-system gnu-build-system)
   (arguments
    ;; Work around parallel build issue whereby C files may be compiled before
    ;; config.h is built: see <http://hydra.gnu.org/build/59381/nixlog/2/raw> and
    ;; <http://lists.openembedded.org/pipermail/openembedded-core/2012-April/059850.html>.
    '(#:parallel-build? #f))
   (synopsis "C library for manipulating Unicode strings")
   (description
    "GNU libunistring is a library providing functions to manipulate
Unicode strings and for manipulating C strings according to the Unicode
standard.")
   (home-page "http://www.gnu.org/software/libunistring/")
   (license lgpl3+)))
