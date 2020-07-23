;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2019 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2020 Marius Bakke <marius@gnu.org>
;;; Copyright @ 2020 Katherine Cox-Buday <cox.katherine.e@gmail.com>
;;; Copyright © 2020 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu packages c)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xml))

(define-public tcc
  (package
    (name "tcc")                                  ;aka. "tinycc"
    (version "0.9.27")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/tinycc/tcc-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "177bdhwzrnqgyrdv1dwvpd04fcxj68s5pm1dzwny6359ziway8yy"))))
    (build-system gnu-build-system)
    (native-inputs `(("perl" ,perl)
                     ("texinfo" ,texinfo)))
    (arguments
     `(#:configure-flags (list (string-append "--elfinterp="
                                              (assoc-ref %build-inputs "libc")
                                              ,(glibc-dynamic-linker))
                               (string-append "--crtprefix="
                                              (assoc-ref %build-inputs "libc")
                                              "/lib")
                               (string-append "--sysincludepaths="
                                              (assoc-ref %build-inputs "libc")
                                              "/include:"
                                              (assoc-ref %build-inputs
                                                         "kernel-headers")
                                              "/include:{B}/include")
                               (string-append "--libpaths="
                                              (assoc-ref %build-inputs "libc")
                                              "/lib")
                               ,@(if (string-prefix? "armhf-linux"
                                                     (or (%current-target-system)
                                                         (%current-system)))
                                     `("--triplet=arm-linux-gnueabihf")
                                     '()))
       #:test-target "test"))
    (native-search-paths
     (list (search-path-specification
            (variable "CPATH")
            (files '("include")))
           (search-path-specification
            (variable "LIBRARY_PATH")
            (files '("lib" "lib64")))))
    ;; Fails to build on MIPS: "Unsupported CPU"
    (supported-systems (delete "mips64el-linux" %supported-systems))
    (synopsis "Tiny and fast C compiler")
    (description
     "TCC, also referred to as \"TinyCC\", is a small and fast C compiler
written in C.  It supports ANSI C with GNU and extensions and most of the C99
standard.")
    (home-page "http://www.tinycc.org/")
    ;; An attempt to re-licence tcc under the Expat licence is underway but not
    ;; (if ever) complete.  See the RELICENSING file for more information.
    (license license:lgpl2.1+)))

(define-public pcc
  (package
    (name "pcc")
    (version "20170109")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://pcc.ludd.ltu.se/ftp/pub/pcc/pcc-"
                                  version ".tgz"))
              (sha256
               (base32
                "1p34w496095mi0473f815w6wbi57zxil106mg7pj6sg6gzpjcgww"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _ (invoke "make" "-C" "cc/cpp" "test") #t)))))
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)))
    (synopsis "Portable C compiler")
    (description
     "PCC is a portable C compiler.  The project goal is to write a C99
compiler while still keeping it small, simple, fast and understandable.")
    (home-page "http://pcc.ludd.ltu.se")
    (supported-systems (delete "aarch64-linux" %supported-systems))
    ;; PCC incorporates code under various BSD licenses; for new code bsd-2 is
    ;; preferred.  See http://pcc.ludd.ltu.se/licenses/ for more details.
    (license (list license:bsd-2 license:bsd-3))))

(define-public libbytesize
  (package
    (name "libbytesize")
    (version "2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/storaged-project/libbytesize/releases/"
                    "download/" version "/libbytesize-" version ".tar.gz"))
              (sha256
               (base32
                "1aivwypmnqcaj2230pifvf3jcgl5chja8rspkxf0j3480asm8g5r"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)
       ("python" ,python)))
    (inputs
     `(("mpfr" ,mpfr)
       ("pcre2" ,pcre2)))
    (home-page "https://github.com/storaged-project/libbytesize")
    (synopsis "Tiny C library for working with arbitrary big sizes in bytes")
    (description
     "The goal of this project is to provide a tiny library that would
facilitate the common operations with sizes in bytes.  Many projects need to
work with sizes in bytes (be it sizes of storage space, memory...) and all of
them need to deal with the same issues like:

@itemize
@item How to get a human-readable string for the given size?
@item How to store the given size so that no significant information is lost?
@item If we store the size in bytes, what if the given size gets over the
MAXUINT64 value?
@item How to interpret sizes entered by users according to their locale and
typing conventions?
@item How to deal with the decimal/binary units (MB versus MiB) ambiguity?
@end itemize

@code{libbytesize} offers a generally usable solution that could be used by
every project that needs to deal with sizes in bytes.  It is written in the C
language with thin bindings for other languages.")
    (license license:lgpl2.1+)))

(define-public udunits
  (package
    (name "udunits")
    (version "2.2.26")
    (source (origin
              (method url-fetch)
              (uri (string-append "ftp://ftp.unidata.ucar.edu/pub/udunits/"
                                  "udunits-" version ".tar.gz"))
              (sha256
               (base32
                "0v9mqw4drnkzkm57331ail6yvs9485jmi37s40lhvmf7r5lli3rn"))))
    (build-system gnu-build-system)
    (inputs
     `(("expat" ,expat)))
    (home-page "https://www.unidata.ucar.edu/software/udunits/")
    (synopsis "C library for units of physical quantities and value-conversion utils")
    (description
     "The UDUNITS-2 package provides support for units of physical quantities.
Its three main components are:

@enumerate
@item @code{udunits2lib}, a C library for units of physical quantities;
@item @code{udunits2prog}, a utility for obtaining the definition of a unit
  and for converting numeric values between compatible units; and
@item an extensive database of units.
@end enumerate\n")
    ;; Like the BSD-3 license but with an extra anti patent clause.
    (license (license:non-copyleft "file://COPYRIGHT"))))

(define-public libfixposix
  (package
    (name "libfixposix")
    (version "0.4.3")
    (home-page "https://github.com/sionescu/libfixposix")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1x4q6yspi5g2s98vq4qszw4z3zjgk9l5zs8471w4d4cs6l97w08j"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("check" ,check)))
    (synopsis "Thin wrapper over POSIX syscalls")
    (description
     "The purpose of libfixposix is to offer replacements for parts of POSIX
whose behaviour is inconsistent across *NIX flavours.")
    (license license:boost1.0)))

(define-public libhx
  (package
    (name "libhx")
    (version "3.25")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/libhx/libHX/"
                           "libHX-" version ".tar.xz"))
       (sha256
        (base32 "12avn16f8aqb0cq6jplz0sv7rh6f07m85dwc8dasnnwsvijwbpbj"))))
    (build-system gnu-build-system)
    (home-page "http://libhx.sourceforge.net")
    (synopsis "C library with common data structures and functions")
    (description
     "This is a C library (with some C++ bindings available) that provides data
structures and functions commonly needed, such as maps, deques, linked lists,
string formatting and autoresizing, option and config file parsing, type
checking casts and more.")
    (license license:lgpl2.1+)))

(define-public packcc
  (package
    (name "packcc")
    ;; We need a few fixes on top of the latest release to prevent test
    ;; failures in Universal Ctags.
    (version "1.2.5-19-g58d1b9d")
    (home-page "https://github.com/enechaev/packcc")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0biyv835jlk43fvmmd3p8jafs7k2iw9qlaj37hvsl604ai6rd5aj"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ;no tests
       #:make-flags '("-DUSE_SYSTEM_STRNLEN=1")
       #:phases (modify-phases %standard-phases
                  ;; The project consists of a single source file and has
                  ;; no actual build system, so we need to do it manually.
                  (delete 'configure)
                  (replace 'build
                    (lambda* (#:key make-flags #:allow-other-keys)
                      (apply invoke "gcc" "-o" "packcc" "packcc.c"
                                      make-flags)))
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (install-file "packcc" (string-append out "/bin"))
                        (install-file "README.md"
                                      (string-append out "/share/doc/packcc"))
                        #t))))))
    (synopsis "Packrat parser generator for C")
    (description
     "PackCC is a packrat parser generator for the C programming language.
Its main features are:
@itemize
@item Generates a parser in C from a grammar described in a PEG.
@item Gives your parser great efficiency by packrat parsing.
@item Supports direct and indirect left-recursive grammar rules.
@end itemize
The grammar of your parser can be described in a @acronym{PEG, Parsing
Expression Grammar}.  The PEG is a top-down parsing language, and is similar
to the regular-expression grammar.  The PEG does not require tokenization to
be a separate step, and tokenization rules can be written in the same way as
any other grammar rules.")
    (license license:expat)))

(define-public sparse
  (package
    (name "sparse")
    (version "0.6.2")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "mirror://kernel.org/software/devel/sparse/dist/"
                              "sparse-"  version ".tar.xz"))
              (sha256
               (base32
                "1z11chawwcmf5xxx5v52cj7wrr3warz6q5wlcjvxpif1jbga172i"))))
    (build-system gnu-build-system)
    (inputs `(("perl" ,perl)))
    (arguments
     '(#:make-flags `(,(string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-after 'unpack 'patch-cgcc
                    (lambda _
                      (substitute* "cgcc"
                        (("'cc'") (string-append "'" (which "gcc") "'")))
                      #t)))))
    (synopsis "Semantic C parser for Linux development")
    (description
     "Sparse is a semantic parser for C and is required for Linux development.
It provides a compiler frontend capable of parsing most of ANSI C as well as
many GCC extensions, and a collection of sample compiler backends, including a
static analyzer also called @file{sparse}.  Sparse provides a set of
annotations designed to convey semantic information about types, such as what
address space pointers point to, or what locks a function acquires or
releases.")
    (home-page "https://sparse.wiki.kernel.org/index.php/Main_Page")
    (license license:expat)))

(define-public libestr
  (package
    (name "libestr")
    (version "0.1.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rsyslog/libestr")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1ca4rj90c0dn7kqpbcchkflxjw88a7rxcnwbr0gply4a28i01nd8"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; autogen.sh calls configure at the end of the script.
         (replace 'bootstrap
           (lambda _ (invoke "autoreconf" "-vfi"))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)
       ("libtool" ,libtool)))
    (home-page "https://github.com/rsyslog/libestr")
    (synopsis "Helper functions for handling strings")
    (description
     "This C library contains some essential string manipulation functions and
more, like escaping special characters.")
    (license license:lgpl2.1+)))

(define-public libfastjson
  (package
    (name "libfastjson")
    (version "0.99.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rsyslog/libfastjson")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0qhs0g9slj3p0v2z4s3cnsx44msrlb4k78ljg7714qiziqbrbwyl"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (home-page "https://github.com/rsyslog/libfastjson")
    (synopsis "Fast JSON library for C")
    (description
     "libfastjson is a fork from json-c aiming to provide: a small library
with essential JSON handling functions, sufficiently good JSON support (not
100% standards compliant), and very fast processing.")
    (license license:expat)))

(define-public liblogging
  (package
    (name "liblogging")
    (version "1.0.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rsyslog/liblogging")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1l32m0y65svf5vxsgw935jnqs6842rcqr56dmzwqvr00yfrjhjkp"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; autogen.sh calls configure at the end of the script.
         (replace 'bootstrap
           (lambda _ (invoke "autoreconf" "-vfi"))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)
       ("libtool" ,libtool)
       ;; For rst2man.py
       ("python-docutils" ,python-docutils)))
    (home-page "https://github.com/rsyslog/liblogging")
    (synopsis "Easy to use and lightweight signal-safe logging library")
    (description
     "Liblogging is an easy to use library for logging.  It offers an enhanced
replacement for the syslog() call, but retains its ease of use.")
    (license license:bsd-2)))

(define-public unifdef
  (package
    (name "unifdef")
    (version "2.12")
    (source (origin
              (method url-fetch)
              ;; https://dotat.at/prog/unifdef/unifdef-2.12.tar.xz
              (uri (string-append "https://dotat.at/prog/" name "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "00647bp3m9n01ck6ilw6r24fk4mivmimamvm4hxp5p6wxh10zkj3"))
              (modules '((guix build utils)))
              (snippet
               '(begin (delete-file-recursively "FreeBSD")
                       (delete-file-recursively "win32")
                       #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'configure))
       #:make-flags (list "CC=gcc" (string-append "prefix=" %output))
       #:tests? #f))                    ;no test suite
    (native-inputs
     `(("perl" ,perl)))
    (home-page "https://dotat.at/prog/unifdef/")
    (synopsis "Utility to selectively processes conditional C preprocessor")
    (description "The @command{unifdef} utility selectively processes
conditional C preprocessor @code{#if} and @code{#ifdef} directives.  It
removes from a file both the directives and the additional text that they
delimit, while otherwise leaving the file alone.  It can be useful for
avoiding distractions when studying code that uses @code{#ifdef} heavily for
portability.")
    (license (list license:bsd-2        ;all files except...
                   license:bsd-3))))    ;...the unidef.1 manual page
