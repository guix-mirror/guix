;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
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

(define-module (gnu packages pcre)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages readline)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public pcre
  (package
   (name "pcre")
   (version "8.32")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://sourceforge/pcre/pcre/"
                                version "/pcre-" version ".tar.bz2"))
            (sha256
             (base32
              "0m8gvrf1q0iwll4csirvvj98xygw4cy7r14i5l53ivsqs2dzn4x9"))))
   (build-system gnu-build-system)
   (inputs `(("bzip2" ,bzip2)
             ("readline" ,readline)
             ("zlib" ,zlib)))
   (arguments
    `(#:configure-flags '("--enable-utf"
                          "--enable-pcregrep-libz"
                          "--enable-pcregrep-libbz2"
                          "--enable-pcretest-libreadline")))
   (synopsis "Perl Compatible Regular Expressions")
   (description
    "The PCRE library is a set of functions that implement regular expression
pattern matching using the same syntax and semantics as Perl 5.  PCRE has its
own native API, as well as a set of wrapper functions that correspond to the
POSIX regular expression API.")
   (license license:bsd-3)
   (home-page "http://www.pcre.org/")))
