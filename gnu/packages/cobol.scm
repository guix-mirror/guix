;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages cobol)
  #:use-module (gnu packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses))

(define-public gnucobol
  (package
    (name "gnucobol")
    (version "2.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://gnu/gnucobol/gnucobol-"
               version ".tar.xz"))
        (sha256
         (base32
          "1814s1n95xax2dz938cf4fkcp0q94nkj1gjbdblbzpk9q92zq66w"))))
    (arguments
     '(#:configure-flags (list (string-append "LDFLAGS=-Wl,-rpath="
                                              (assoc-ref %outputs "out")
                                              "/lib"))))
    (inputs
     `(("bdb" ,bdb)
       ("gmp" ,gmp)
       ("ncurses" ,ncurses)))
    (build-system gnu-build-system)
    (home-page "https://savannah.gnu.org/projects/gnucobol/")
    (synopsis "A modern COBOL compiler")
    (description "GnuCOBOL is a free, modern COBOL compiler.  GnuCOBOL
implements a substantial part of the COBOL 85, COBOL 2002 and COBOL 2014
standards and X/Open COBOL, as well as many extensions included in other
COBOL compilers (IBM COBOL, MicroFocus COBOL, ACUCOBOL-GT and others).
GnuCOBOL translates COBOL into C and compiles the translated code using
a native C compiler.")
    (license gpl3+)))
