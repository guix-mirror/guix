;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2013, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages assembly)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xml))

(define-public nasm
  (package
    (name "nasm")
    (version "2.13.01")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.nasm.us/pub/nasm/releasebuilds/"
                                  version "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "0plsvcwxc7q3llr3bz10prwq1gn4ll38aqmv0yzfqcq4iw0160ma"))))
    (build-system gnu-build-system)
    (native-inputs `(("perl" ,perl)  ;for doc and test target
                     ("texinfo" ,texinfo)))
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'dont-build-ps-pdf-outputs
           (lambda _
             (substitute* "doc/Makefile.in"
               (("html nasmdoc.txt nasmdoc.pdf")
                "html nasmdoc.txt")
               (("\\$\\(INSTALL_DATA\\) nasmdoc.pdf")
                "$(INSTALL_DATA)"))
             #t))
         (add-after 'install 'install-info
           (lambda _
             (zero? (system* "make" "install_doc")))))))
    (home-page "http://www.nasm.us/")
    (synopsis "80x86 and x86-64 assembler")
    (description
     "NASM, the Netwide Assembler, is an 80x86 and x86-64 assembler designed
for portability and modularity.  It supports a range of object file formats,
including Linux and *BSD a.out, ELF, COFF, Mach-O, Microsoft 16-bit OBJ,
Windows32 and Windows64.  It will also output plain binary files.  Its syntax
is designed to be simple and easy to understand, similar to Intel's but less
complex.  It supports all currently known x86 architectural extensions, and
has strong support for macros.")
    (license license:bsd-3)))

(define-public yasm
  (package
    (name "yasm")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.tortall.net/projects/yasm/releases/yasm-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0gv0slmm0qpq91za3v2v9glff3il594x5xsrbgab7xcmnh0ndkix"))))
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
    (license (license:non-copyleft "file://COPYING"
                                   "See COPYING in the distribution."))))

(define-public lightning
  (package
    (name "lightning")
    (version "2.1.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/lightning/lightning-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0sbs2lm8b9in2m8d52zf0x9gpp40x6r7sl6sha92yq3pr78rwa4v"))))
    (build-system gnu-build-system)
    (native-inputs `(("zlib" ,zlib)))
    (synopsis "Library for generating assembly code at runtime")
    (description
     "GNU Lightning is a library that generates assembly language code at
run-time.  Thus, it is useful in creating Just-In-Time compilers.  It
abstracts over the target CPU by exposing a standardized RISC instruction set
to the clients.")
    (home-page "https://www.gnu.org/software/lightning/")
    (license license:gpl3+)))
