;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2013, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Guy Fleury Iteriteka <hoonandon@gmail.com>
;;; Copyright © 2019 Andy Tai <atai@atai.org>
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
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages man)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xml)
  #:use-module ((guix utils)
                #:select (%current-system)))

(define-public nasm
  (package
    (name "nasm")
    (version "2.14.02")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.nasm.us/pub/nasm/releasebuilds/"
                                  version "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1xg8dfr49py15vbwk1rzcjc3zpqydmr49ahlijm56wlgj8zdwjp2"))))
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
             (invoke "make" "install_doc"))))))
    (home-page "https://www.nasm.us/")
    (synopsis "80x86 and x86-64 assembler")
    (description
     "NASM, the Netwide Assembler, is an 80x86 and x86-64 assembler designed
for portability and modularity.  It supports a range of object file formats,
including Linux and *BSD a.out, ELF, COFF, Mach-O, Microsoft 16-bit OBJ,
Windows32 and Windows64.  It will also output plain binary files.  Its syntax
is designed to be simple and easy to understand, similar to Intel's but less
complex.  It supports all currently known x86 architectural extensions, and
has strong support for macros.")
    (license license:bsd-2)))

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

(define-public fasm
  (package
    (name "fasm")
    (version "1.73.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://flatassembler.net/fasm-"
                           version ".tgz"))
       (sha256
        (base32 "1zhbs72qc8bw5158zh6mvzznfamcx5a1bsmbmq9ci0d7wb58sxmg"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests exist
       #:strip-binaries? #f             ; fasm has no sections
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no "configure" script
         (replace 'build
           (lambda _
             (chdir "source/Linux/")
             (if (string=? ,(%current-system) "x86_64-linux")
                 ;; Use pre-compiled binaries in top-level directory to build
                 ;; fasm.
                 (invoke "../../fasm.x64" "fasm.asm")
                 (invoke "../../fasm" "fasm.asm"))))
         (replace 'install
           (lambda _
             (let ((out (assoc-ref %outputs "out")))
               (install-file "fasm" (string-append out "/bin")))
             #t)))))
    (supported-systems '("x86_64-linux" "i686-linux"))
    (synopsis "Assembler for x86 processors")
    (description
     "FASM is an assembler that supports x86 and IA-64 Intel architectures.
It does multiple passes to optimize machine code.  It has macro abilities and
focuses on operating system portability.")
    (home-page "https://flatassembler.net/")
    (license license:bsd-2)))

(define-public dev86
  (package
    (name "dev86")
    (version "0.16.21")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://v3.sk/~lkundrak/dev86/Dev86src-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "154dyr2ph4n0kwi8yx0n78j128kw29rk9r9f7s2gddzrdl712jr3"))))
    (build-system gnu-build-system)
    (arguments
     `(#:parallel-build? #f ; They use submakes wrong
       #:make-flags (list "CC=gcc"
                          (string-append "PREFIX="
                                         (assoc-ref %outputs "out")))
       #:system "i686-linux" ; Standalone ld86 had problems otherwise
       #:tests? #f ; No tests exist
       #:phases
       (modify-phases %standard-phases
        (delete 'configure)
        (add-before 'install 'mkdir
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (mkdir-p (string-append out "/bin"))
              (mkdir-p (string-append out "/man/man1"))
              #t))))))
    (synopsis "Intel 8086 (primarily 16-bit) assembler, C compiler and
linker")
    (description "This package provides a Intel 8086 (primarily 16-bit)
assembler, a C compiler and a linker.  The assembler uses Intel syntax
(also Intel order of operands).")
    (home-page "https://github.com/jbruchon/dev86")
    (supported-systems '("i686-linux" "x86_64-linux"))
    (license license:gpl2+)))

(define-public libjit
  (let ((commit "554c9f5c750daa6e13a6a5cd416873c81c7b8226"))
    (package
      (name "libjit")
      (version "0.1.4")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.gnu.org/r/libjit.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "0p6wklslkkp3s4aisj3w5a53bagqn5fy4m6088ppd4fcfxgqkrcd"))))
      (build-system gnu-build-system)
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("bison" ,bison)
         ("flex" ,flex)
         ("help2man" ,help2man)
         ("gettext" ,gettext-minimal)
         ("libtool" ,libtool)
         ("makeinfo" ,texinfo)
         ("pkg-config" ,pkg-config)))
      (home-page "https://www.gnu.org/software/libjit/")
      (synopsis "Just-In-Time compilation library")
      (description
       "GNU libjit is a library that provides generic Just-In-Time compiler
functionality independent of any particular bytecode, language, or
runtime")
      (license license:lgpl2.1+))))
