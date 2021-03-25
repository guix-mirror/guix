;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2013, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Guy Fleury Iteriteka <hoonandon@gmail.com>
;;; Copyright © 2019 Andy Tai <atai@atai.org>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Christopher Lemmer Webber <cwebber@dustycloud.org>
;;; Copyright © 2020 B. Wilson <elaexuotee@wilsonb.com>
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
  #:use-module (guix build-system meson)
  #:use-module (guix build-system cmake)
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
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages python)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages xml)
  #:use-module ((guix utils)
                #:select (%current-system cc-for-target)))

(define-public nasm
  (package
    (name "nasm")
    (version "2.14.02")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.nasm.us/pub/nasm/releasebuilds/"
                                  version "/nasm-" version ".tar.xz"))
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
    (arguments
     '(#:parallel-tests? #f))           ; Some tests fail
                                        ; non-deterministically when run in
                                        ; parallel
    (inputs
     `(("python" ,python-wrapper)
       ("xmlto" ,xmlto)))
    (home-page "https://yasm.tortall.net/")
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
    (version "2.1.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/lightning/lightning-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "1jgxbq2cm51dzi3zhz38mmgwdcgs328mfl8iviw8dxn6dn36p1gd"))))
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

(define-public simde
  (package
    (name "simde")
    (version "0.7.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/simd-everywhere/simde")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xkf21gbkgz6zlxabkmgwvy7py6cdnfqx9aplj90gz25gzrr1mkb"))))
    (build-system meson-build-system)
    ;; We really want this for the headers, and the tests require a bundled library.
    (arguments '(#:configure-flags '("-Dtests=false")))
    (synopsis "Implementations of SIMD instruction sets for foreign systems")
    (description "The SIMDe header-only library provides fast, portable
implementations of SIMD intrinsics on hardware which doesn't natively support
them, such as calling SSE functions on ARM.  There is no performance penalty if
the hardware supports the native implementation (e.g., SSE/AVX runs at full
speed on x86, NEON on ARM, etc.).")
    (home-page "https://simd-everywhere.github.io/blog/")
    (license license:expat)))

(define-public fasm
  (package
    (name "fasm")
    (version "1.73.27")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://flatassembler.net/fasm-"
                           version ".tgz"))
       (sha256
        (base32 "1cghiks49ql77b9l4mwrnlk76kai0fm0z22j71kbdlxngwvlh0b8"))))
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
     "@acronym{FASM, the Flat ASseMbler} is an assembler that supports x86 and
IA-64 Intel architectures.  It does multiple passes to optimize machine code.
It has macro abilities and focuses on operating system portability.")
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
                (file-name (git-file-name name version))
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

(define-public rgbds
  (package
    (name "rgbds")
    (version "0.4.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/gbdev/rgbds")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0lygj7jzjlq4w0mkiir7ycysrd1p1akyvzrppjcchja05mi8wy9p"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'patch-pkg-config
           (lambda _
             (substitute* "Makefile"
               (("pkg-config")
                (or (which "pkg-config")
                    (string-append ,(%current-target-system)
                                   "-pkg-config"))))
             #t))
         (replace 'check
           (lambda _
             (with-directory-excursion "test/asm"
               (invoke "./test.sh"))
             (with-directory-excursion "test/link"
               (invoke "./test.sh")))))
       #:make-flags `(,(string-append "CC=" ,(cc-for-target))
                      ,(string-append "PREFIX="
                                      (assoc-ref %outputs "out")))))
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)
       ("pkg-config" ,pkg-config)
       ("util-linux" ,util-linux)))
    (inputs
     `(("libpng" ,libpng)))
    (home-page "https://github.com/gbdev/rgbds")
    (synopsis "Rednex Game Boy Development System")
    (description
     "RGBDS (Rednex Game Boy Development System) is an assembler/linker
package for the Game Boy and Game Boy Color.  It consists of:
@itemize @bullet
@item rgbasm (assembler)
@item rgblink (linker)
@item rgbfix (checksum/header fixer)
@item rgbgfx (PNG-to-Game Boy graphics converter)
@end itemize")
    (license license:expat)))

(define-public wla-dx
  (package
    (name "wla-dx")
    (version "9.12")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/vhelin/wla-dx")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wlbqv2rgk9q6m9an1mi0i29250zl8lw7zipki2bbi9mczpyczli"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("sphinx" ,python-sphinx)))      ; to generate man pages
    (arguments
     `(#:tests? #f))                    ; no tests
    (home-page "https://github.com/vhelin/wla-dx")
    (synopsis "Assemblers for various processors")
    (description "WLA DX is a set of tools to assemble assembly files to
object or library files (@code{wla-ARCH}) and link them together (@code{wlalink}).
Supported architectures are:

@itemize @bullet
@item z80
@item gb (z80-gb)
@item 6502
@item 65c02
@item 6510
@item 65816
@item 6800
@item 6801
@item 6809
@item 8008
@item 8080
@item huc6280
@item spc700
@end itemize")
    (license license:gpl2)))

(define-public xa
  (package
    (name "xa")
    (version "2.3.11")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.floodgap.com/retrotech/xa"
                                  "/dists/xa-" version ".tar.gz"))
              (sha256
               (base32
                "0b81r7mvzqxgnbbmhixcnrf9nc72v1nqaw19k67221g3k561dwij"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f   ; TODO: custom test harness, not sure how it works
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))            ; no "configure" script
       #:make-flags (list (string-append "DESTDIR=" (assoc-ref %outputs "out")))))
    (native-inputs `(("perl" ,perl)))
    (home-page "https://www.floodgap.com/retrotech/xa/")
    (synopsis "Two-pass portable cross-assembler")
    (description
     "xa is a high-speed, two-pass portable cross-assembler.
It understands mnemonics and generates code for NMOS 6502s (such
as 6502A, 6504, 6507, 6510, 7501, 8500, 8501, 8502 ...),
 CMOS 6502s (65C02 and Rockwell R65C02) and the 65816.")
    (license license:gpl2)))

(define-public armips
  (package
    (name "armips")
    (version "0.11.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Kingcom/armips")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1c4dhjkvynqn9xm2vcvwzymk7yg8h25alnawkz4z1dnn1z1k3r9g"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs #:allow-other-keys)
             (invoke "./armipstests" "../source/Tests")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "armips" (string-append (assoc-ref outputs "out")
                                                   "/bin"))
             #t)))))
    (home-page "https://github.com/Kingcom/armips")
    (synopsis "Assembler for various ARM and MIPS platforms")
    (description
     "armips is an assembler with full support for the MIPS R3000, MIPS R4000,
Allegrex and RSP instruction sets, partial support for the EmotionEngine
instruction set, as well as complete support for the ARM7 and ARM9 instruction
sets, both THUMB and ARM mode.")
    (license license:expat)))

(define-public intel-xed
  (package
    (name "intel-xed")
    (version "11.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/intelxed/xed")
             (commit version)))
       (sha256 (base32 "1jffayski2gpd54vaska7fmiwnnia8v3cka4nfyzjgl8xsky9v2s"))
       (file-name (git-file-name name version))
       (patches (search-patches "intel-xed-fix-nondeterminism.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("python-wrapper" ,python-wrapper)
       ("tcsh" ,tcsh)
       ;; As of the time of writing this comment, mbuild does not exist in the
       ;; Python Package Index and seems to only be used by intel-xed, so we
       ;; opt to include it here instead of packaging separately.  Note also
       ;; that the git repository contains no version tags, so we directly
       ;; reference the "version" variable from setup.py instead.
       ("mbuild"
        ,(let ((name "mbuild")
               (version "0.2496"))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/intelxed/mbuild")
                   (commit "5304b94361fccd830c0e2417535a866b79c1c297")))
             (sha256
              (base32
               "0r3avc3035aklqxcnc14rlmmwpj3jp09vbcbwynhvvmcp8srl7dl"))
             (file-name (git-file-name name version)))))))
    (outputs '("out" "lib"))
    (arguments
     `(#:phases
       ;; Upstream uses the custom Python build tool `mbuild', so we munge
       ;; gnu-build-system to fit.  The build process for this package is
       ;; documented at https://intelxed.github.io/build-manual/.
       (let* ((build-dir "build")
              (kit-dir "kit"))
         (modify-phases %standard-phases
           (delete 'configure)
           (replace 'build
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((mbuild (assoc-ref inputs "mbuild")))
                 (setenv "PYTHONPATH" (string-append
                                       (getenv "PYTHONPATH") ":" mbuild))
                 (invoke "./mfile.py"
                         (string-append "--build-dir=" build-dir)
                         (string-append "--install-dir=" kit-dir)
                         "examples"
                         "doc"
                         "install"))))
           (replace 'check
             (lambda _
               ;; Skip broken test group `tests/tests-avx512pf'.
               (invoke "tests/run-cmd.py"
                       (string-append "--build-dir=" kit-dir "/bin")
                       "--tests" "tests/tests-base"
                       "--tests" "tests/tests-avx512"
                       "--tests" "tests/tests-cet"
                       "--tests" "tests/tests-via"
                       "--tests" "tests/tests-syntax"
                       "--tests" "tests/tests-xop")))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (lib (assoc-ref outputs "lib")))
                 (copy-recursively (string-append kit-dir "/bin")
                                   (string-append out "/bin"))
                 (copy-recursively (string-append kit-dir "/include")
                                   (string-append lib "/include"))
                 (copy-recursively (string-append kit-dir "/lib")
                                   (string-append lib "/lib"))
                 #t)))))))
    (home-page "https://intelxed.github.io/")
    (synopsis "Encoder and decoder for x86 (IA32 and Intel64) instructions")
    (description "The Intel X86 Encoder Decoder (XED) is a software library and
for encoding and decoding X86 (IA32 and Intel64) instructions.  The decoder
takes sequences of 1-15 bytes along with machine mode information and produces
a data structure describing the opcode, operands, and flags.  The encoder takes
a similar data structure and produces a sequence of 1 to 15 bytes.  Disassembly
is essentially a printing pass on the data structure.

The library and development files are under the @code{lib} output, with a
family of command line utility wrappers in the default output.  Each of the cli
tools is named like @code{xed*}.  Documentation for the cli tools is sparse, so
this is a case where ``the code is the documentation.''")
    (license license:asl2.0)))
