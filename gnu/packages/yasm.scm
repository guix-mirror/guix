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

(define-module (gnu packages yasm)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xml))

(define-public yasm
  (package
    (name "yasm")
    (version "1.2.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://www.tortall.net/projects/yasm/releases/yasm-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0cfg7ji3ia2in628w42wrfvw2ixmmm4rghwmv2k202mraysgm3vn"))))
    (build-system gnu-build-system)
    (inputs
     `(("python" ,python-wrapper)
       ("xmlto" ,xmlto)))
    (home-page "http://yasm.tortall.net/")
    (synopsis "Rewrite of the NASM assembler")
    (description
     "Yasm is a complete rewrite of the NASM assembler.

Yasm currently supports the x86 and AMD64 instruction sets, accepts NASM
and GAS assembler syntaxes, outputs binary, ELF32, ELF64, 32 and 64-bit
Mach-O, RDOFF2, COFF, Win32, and Win64 object formats, and generates source
debugging information in STABS, DWARF 2, and CodeView 8 formats.")
    (license (license:bsd-style "file://COPYING"
                                "See COPYING in the distribution."))))
