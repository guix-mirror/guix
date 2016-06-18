;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
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
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages texinfo))

(define-public nasm
  (package
    (name "nasm")
    (version "2.12.01")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.nasm.us/pub/nasm/releasebuilds/"
                                  version "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "12bl6vc5sjp9nnhf0iwy6l27vq783y0rxrjpp8sy84h5cb7a3fwx"))))
    (build-system gnu-build-system)
    (native-inputs `(("ghostscript" ,ghostscript) ; ps2pdf
                     ("perl" ,perl)  ;for test target
                     ("texinfo" ,texinfo)))
    (arguments
     `(#:test-target "test"
       #:phases (modify-phases %standard-phases
                  (add-after 'install 'install-info
                    (lambda _
                      ;; FIXME: The PDF and PS files are not reproducible.
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
    (supported-systems '("x86_64-linux" "i686-linux"))
    (license license:bsd-3)))
