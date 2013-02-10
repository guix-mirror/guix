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
   (synopsis "GNU Libunistring, a Unicode string library")
   (description
    "This library provides functions for manipulating Unicode strings and for
manipulating C strings according to the Unicode standard.

GNU libunistring is for you if your application involves non-trivial text
processing, such as upper/lower case conversions, line breaking, operations
on words, or more advanced analysis of text.  Text provided by the user can,
in general, contain characters of all kinds of scripts.  The text processing
functions provided by this library handle all scripts and all languages.

libunistring is for you if your application already uses the ISO C / POSIX
<ctype.h>, <wctype.h> functions and the text it operates on is provided by
the user and can be in any language.

libunistring is also for you if your application uses Unicode strings as
internal in-memory representation.")
   (home-page "http://www.gnu.org/software/libunistring/")
   (license lgpl3+)))
