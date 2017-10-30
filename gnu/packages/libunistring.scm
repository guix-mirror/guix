;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017 Eric Bavier <bavier@member.fsf.org>
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
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base))

(define-public libunistring
  (package
   (name "libunistring")
   (version "0.9.7")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "mirror://gnu/libunistring/libunistring-"
                  version ".tar.xz"))
            (sha256
             (base32
              "15z76qrmrvkc3c6hfq2lzzqysgd21s682f2smycfab5g598n8drf"))
            (modules '((guix build utils)))
            (snippet
             '(begin
                ;; The gnulib test-lock test is prone to writer starvation
                ;; with our glibc@2.25, which prefers readers, so disable it.
                ;; The gnulib commit b20e8afb0b2 should fix this once
                ;; incorporated here.
                (substitute* "tests/Makefile.in"
                  (("test-lock\\$\\(EXEEXT\\) ") ""))
                #t))))
   (propagated-inputs (libiconv-if-needed))
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
   (home-page "https://www.gnu.org/software/libunistring/")
   (license (list lgpl3+ gpl2))))
