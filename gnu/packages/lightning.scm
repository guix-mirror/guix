;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages lightning)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses))

(define-public lightning
  (package
    (name "lightning")
    (version "2.0.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/lightning/lightning-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0m0hc9aqw72wsg2sq2hx8zwhfvz814rfr7rz9qcyn3n4qv2kc5z4"))))
    (build-system gnu-build-system)
    (synopsis "Library for generating assembly code at runtime")
    (description
     "GNU lightning is a library that generates assembly language code at
run-time; it is very fast, making it ideal for Just-In-Time compilers, and it
abstracts over the target CPU, as it exposes to the clients a standardized
RISC instruction set inspired by the MIPS and SPARC chips.")
    (home-page "http://www.gnu.org/software/lightning/")
    (license gpl3+)))
