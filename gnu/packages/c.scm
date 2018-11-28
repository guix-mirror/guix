;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017, 2018, 2019, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018, 2020, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019, 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2019 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2020, 2021 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020 Katherine Cox-Buday <cox.katherine.e@gmail.com>
;;; Copyright © 2020, 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020, 2021 Greg Hogan <code@greghogan.com>
;;; Copyright © 2021 David Dashyan <mail@davie.li>
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
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
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
    (native-inputs (list perl texinfo))
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
     (list bison flex))
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
     (list mpfr pcre2))
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
    ;; Four-part version numbers are development snapshots, not releases.  See
    ;; <https://github.com/Unidata/UDUNITS-2/issues/99#issuecomment-732323472>.
    (version "2.2.28")
    (source (origin
              (method url-fetch)
              (uri (string-append "ftp://ftp.unidata.ucar.edu/pub/udunits/"
                                  "udunits-" version ".tar.gz"))
              (sha256
               (base32
                "17jpbp6f0rr132jn2gqy8ry8mv1w27v6dyhfq1igv8v1674aw2sr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--disable-static")))
    (inputs
     (list expat))
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
     (list autoconf automake libtool pkg-config check))
    (synopsis "Thin wrapper over POSIX syscalls")
    (description
     "The purpose of libfixposix is to offer replacements for parts of POSIX
whose behaviour is inconsistent across *NIX flavours.")
    (license license:boost1.0)))

(define-public libhx
  (package
    (name "libhx")
    (version "4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://inai.de/files/libhx/"
                           "libHX-" version ".tar.xz"))
       (sha256
        (base32 "1ri3sxiw5a8br27j7f20s40kihfvq6mmxzcrx68zydiwyxjvf5jj"))))
    (build-system gnu-build-system)
    (home-page "https://inai.de/projects/libhx/")
    (synopsis "C library with common data structures and functions")
    (description
     "This is a C library (with some C++ bindings available) that provides data
structures and functions commonly needed, such as maps, deques, linked lists,
string formatting and autoresizing, option and config file parsing, type
checking casts and more.")
    (license license:lgpl2.1+)))

(define-public libwuya
  ;; This commit is the one before "wuy_pool.h" was removed from libwuya,
  ;; which libleak currently requires.
  (let ((revision "1")
        (commit "883502041044f4616cfbf75c8f2bb60059f704a9"))
    (package
      (name "libwuya")
      (version (git-version "0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/WuBingzheng/libwuya")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1xrsqbgr13g2v0ag165ryp7xrwzv41xfygzk2a3445ca98c1qpdc"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ;no test suite
         #:phases (modify-phases %standard-phases
                    (add-after 'unpack 'patch-lua-includes
                      (lambda _
                        (substitute* '("wuy_cflua.h" "wuy_cflua.c")
                          (("<lua5\\.1/") "<"))
                        #t))
                    (add-after 'unpack 'add--fPIC-to-CFLAGS
                      (lambda _
                        (substitute* "Makefile"
                          (("CFLAGS[^\n]*" all)
                           (string-append all " -fPIC")))
                        #t))
                    (add-before 'build 'set-CC
                      (lambda _
                        (setenv "CC" "gcc")
                        #t))
                    (delete 'configure) ;no configure script
                    (replace 'install
                      (lambda* (#:key outputs #:allow-other-keys)
                        (let* ((out (assoc-ref outputs "out"))
                               (include-dir (string-append out "/include"))
                               (headers (find-files "." "\\.h$")))
                          (for-each (lambda (h)
                                      (install-file h include-dir))
                                    headers)
                          (install-file "libwuya.a" (string-append out "/lib"))
                          #t))))))
      (inputs (list lua))
      (home-page "https://github.com/WuBingzheng/libwuya/")
      (synopsis "C library implementing various data structures")
      (description "The @code{libwuya} library implements data structures such
as dictionaries, skip lists, and memory pools.")
      ;; There is no clear information as to what license this is distributed
      ;; under, but it is included (bundled) with libleak from the same author
      ;; under the GNU GPL v2 or later license, so use this here until it is
      ;; clarified (see: https://github.com/WuBingzheng/libwuya/issues/2).
      (license license:gpl2+))))

(define-public packcc
  (package
    (name "packcc")
    (version "1.5.0")
    (home-page "https://github.com/arithy/packcc")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1n9ivsa6b9ps2jbh34bycjqjpbwbk85l4jjg46pfhqxzz96793wy"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-before 'build 'chdir
                    (lambda _
                      (chdir "build/gcc")))
                  (add-before 'check 'pre-check
                    (lambda _
                      (setenv "CC" "gcc")
                      ;; The style tests are supposed to be skipped when
                      ;; uncrustify is unavailable, but a stray version
                      ;; check prevents it from working.  This can be
                      ;; removed for future versions of PackCC.
                      (substitute* "../../tests/style.d/style.bats"
                        (("^[[:blank:]]+check_uncrustify_version")
                         ""))))
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (install-file "release/bin/packcc"
                                      (string-append out "/bin"))
                        (install-file "../../README.md"
                                      (string-append out "/share/doc/packcc"))
                        #t))))))
    (native-inputs
     (list bats))
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
    (version "0.6.4")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "mirror://kernel.org/software/devel/sparse/dist/"
                              "sparse-"  version ".tar.xz"))
              (sha256
               (base32
                "0z1qds52144nvsdnl82r3zs3vax618v920jmffyyssmwj54qpcka"))))
    (build-system gnu-build-system)
    (inputs (list perl))
    (arguments
     '(#:make-flags `(,(string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-after 'unpack 'patch-cgcc
                    (lambda _
                      (substitute* "cgcc"
                        (("'cc'") (string-append "'" (which "gcc") "'"))))))))
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
     (list autoconf automake pkg-config libtool))
    (home-page "https://github.com/rsyslog/libestr")
    (synopsis "Helper functions for handling strings")
    (description
     "This C library contains some essential string manipulation functions and
more, like escaping special characters.")
    (license license:lgpl2.1+)))

(define-public libfastjson
  (package
    (name "libfastjson")
    (version "0.99.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rsyslog/libfastjson")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12rqcdqxazw8czzxbivdapdgj19pcswpw1jp2915sxbljis83g6q"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool))
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
     (list autoconf
           automake
           pkg-config
           libtool
           ;; For rst2man.py
           python-docutils))
    (home-page "https://github.com/rsyslog/liblogging")
    (synopsis "Easy to use and lightweight signal-safe logging library")
    (description
     "Liblogging is an easy to use library for logging.  It offers an enhanced
replacement for the syslog() call, but retains its ease of use.")
    (license license:bsd-2)))

(define-public liblognorm
  (package
    (name "liblognorm")
    (version "2.0.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rsyslog/liblognorm.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1pyy1swvq6jj12aqma42jimv71z8m66zy6ydd5v19cp2azm4krml"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:parallel-tests? #false ;not supported
      #:phases
      '(modify-phases %standard-phases
         ;; These tests fail because tmp.rulebase is never created.  This
         ;; looks rather harmless.
         (add-after 'unpack 'delete-failing-tests
           (lambda _
             (substitute* "tests/Makefile.am"
               (("string_rb_simple.sh ") "")
               (("string_rb_simple_2_lines.sh ") "")))))))
    (inputs
     (list json-c libestr libfastjson))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (home-page "https://www.liblognorm.com")
    (synopsis "Fast samples-based log normalization library")
    (description
     "Liblognorm normalizes event data into well-defined name-value pairs and
a set of tags describing the message.")
    ;; liblognorm is very slowly transitioning to ASL2.0
    ;; See https://github.com/rsyslog/liblognorm/issues/329
    (license license:lgpl2.1+)))

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
       #:make-flags (list (string-append "CC=" ,(cc-for-target))
                          (string-append "prefix=" %output))
       #:tests? #f))                    ;no test suite
    (native-inputs
     (list perl))
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

(define-public aws-c-common
  (package
    (name "aws-c-common")
    ; Update only when updating aws-crt-cpp.
    (version "0.6.11")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (string-append "https://github.com/awslabs/" name))
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1v4dhygiynl75y3702lbp9j8kph88j4f2sq39s4lkhn6lmbz5f0f"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       '("-DBUILD_SHARED_LIBS=ON")))
    (synopsis "Amazon Web Services core C library")
    (description
     "This library provides common C99 primitives, configuration, data
 structures, and error handling for the @acronym{AWS,Amazon Web Services} SDK.")
    (home-page "https://github.com/awslabs/aws-c-common")
    (license license:asl2.0)))

(define-public aws-checksums
  (package
    (name "aws-checksums")
    ; Update only when updating aws-crt-cpp.
    (version "0.1.12")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (string-append "https://github.com/awslabs/" name))
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "054f2hkmkxhw83q7zsz349k82xk6bkrvlsab088pf7kn9wd4hy4k"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       (list "-DBUILD_SHARED_LIBS=ON"
             (string-append "-DCMAKE_PREFIX_PATH="
                            (assoc-ref %build-inputs "aws-c-common")))))
    (inputs
     (list aws-c-common))
    (synopsis "Amazon Web Services checksum library")
    (description
     "This library provides cross-Platform hardware accelerated CRC32c and CRC32
with fallback to efficient C99 software implementations.")
    (home-page "https://github.com/awslabs/aws-checksums")
    (license license:asl2.0)))

(define-public aws-c-event-stream
  (package
    (name "aws-c-event-stream")
    ; Update only when updating aws-crt-cpp.
    (version "0.2.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (string-append "https://github.com/awslabs/" name))
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0xwwr7gdgfrphk6j7vk12rgimfim6m4qnj6hg8hgg16cplhvsfzh"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       (list "-DBUILD_SHARED_LIBS=ON"
             (string-append "-DCMAKE_PREFIX_PATH="
                            (assoc-ref %build-inputs "aws-c-common")))))
    (propagated-inputs
     (list aws-c-common aws-c-io aws-checksums))
    (inputs
     (list aws-c-cal s2n))
    (synopsis "Amazon Web Services client-server message format library")
    (description
     "This library is a C99 implementation for @acronym{AWS,Amazon Web Services}
event stream encoding, a binary format for bidirectional client-server
communication.")
    (home-page "https://github.com/awslabs/aws-c-event-stream")
    (license license:asl2.0)))

(define-public aws-c-io
  (package
    (name "aws-c-io")
    ; Update only when updating aws-crt-cpp.
    (version "0.10.9")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (string-append "https://github.com/awslabs/" name))
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "14rxa3k842fgk43702nz7z9y3clfhvax8j0k93i0c5vg14wj38yp"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       (list "-DBUILD_SHARED_LIBS=ON"
             (string-append "-DCMAKE_PREFIX_PATH="
                            (assoc-ref %build-inputs "aws-c-common"))
             "-DENABLE_NET_TESTS=OFF")))
    (propagated-inputs
     (list aws-c-cal aws-c-common s2n))
    (synopsis "Event driven framework for implementing application protocols")
    (description "This library provides a C99 framework for constructing
event-driven, asynchronous network application protocols.")
    (home-page "https://github.com/awslabs/aws-c-io")
    (license license:asl2.0)))

(define-public aws-c-cal
  (package
    (name "aws-c-cal")
    ; Update only when updating aws-crt-cpp.
    (version "0.5.12")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (string-append "https://github.com/awslabs/" name))
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09zqf610x4g2mcjcaf9nh88k6dkw14pi721yr8hxb5rmsx7rlfrb"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       (list "-DBUILD_SHARED_LIBS=ON"
             (string-append "-DCMAKE_PREFIX_PATH="
                            (assoc-ref %build-inputs "aws-c-common")))))
    (propagated-inputs
     (list aws-c-common))
    (inputs
     `(("openssl" ,openssl)
       ("openssl:static" ,openssl "static")))
    (synopsis "Amazon Web Services Crypto Abstraction Layer")
    (description "This library provides a C99 wrapper for hash, HMAC, and ECC
cryptographic primitives for the @acronym{AWS,Amazon Web Services} SDK.")
    (home-page "https://github.com/awslabs/aws-c-cal")
    (license license:asl2.0)))

(define-public pcl
  (package
    (name "pcl")
    (version "1.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
              "http://www.xmailserver.org/pcl-" version ".tar.gz"))
       (sha256
        (base32
         "06ly65rq4iyj2p4704i215c8y4rgspwl8sxfaifmf4ahfr30bcz7"))))
    (build-system gnu-build-system)
    (home-page "http://www.xmailserver.org/libpcl.html")
    (synopsis "Portable Coroutine Library")
    (description "The @acronym{PCL, Portable Coroutine Library} implements the
low level functionality for coroutines.")
    (license license:gpl2+)))

(define-public aws-c-http
  (package
    (name "aws-c-http")
    ; Update only when updating aws-crt-cpp.
    (version "0.6.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (string-append "https://github.com/awslabs/" name))
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1s06bz6w7355ldyhwjidcpbff7591ch4lwwjcj47a6k2kczdmiz4"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       (list "-DBUILD_SHARED_LIBS=ON"
             (string-append "-DCMAKE_PREFIX_PATH="
                            (assoc-ref %build-inputs "aws-c-common"))
             "-DENABLE_NET_TESTS=OFF")))
    (propagated-inputs
     (list aws-c-compression aws-c-io))
    (synopsis "Amazon Web Services HTTP library")
    (description
     "This library provides a C99 implementation of the HTTP/1.1 and HTTP/2
specifications.")
    (home-page "https://github.com/awslabs/aws-c-http")
    (license license:asl2.0)))

(define-public aws-c-compression
  (package
    (name "aws-c-compression")
    ; Update only when updating aws-crt-cpp.
    (version "0.2.14")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (string-append "https://github.com/awslabs/" name))
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0fs3zhhzxsb9nfcjpvfbcq79hal7si2ia1c09scab9a8m264f4vd"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       (list "-DBUILD_SHARED_LIBS=ON"
             (string-append "-DCMAKE_PREFIX_PATH="
                            (assoc-ref %build-inputs "aws-c-common")))))
    (propagated-inputs
     (list aws-c-common))
    (synopsis "Amazon Web Services compression library")
    (description
     "This library provides a C99 implementation of compression algorithms,
currently limited to Huffman encoding and decoding.")
    (home-page "https://github.com/awslabs/aws-c-compression")
    (license license:asl2.0)))

(define-public aws-c-auth
  (package
    (name "aws-c-auth")
    ; Update only when updating aws-crt-cpp.
    (version "0.6.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (string-append "https://github.com/awslabs/" name))
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "120p69lj279yq3d2b81f45kgfrvf32j6m7s03m8hh27w8yd4vbfp"))
              (patches
               (search-patches
                "aws-c-auth-install-private-headers.patch"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       (list "-DBUILD_SHARED_LIBS=ON"
             (string-append "-DCMAKE_PREFIX_PATH="
                            (assoc-ref %build-inputs "aws-c-common"))
             "-DENABLE_NET_TESTS=OFF")))
    (propagated-inputs
     (list aws-c-cal aws-c-common aws-c-http aws-c-io))
    (synopsis "Amazon Web Services client-side authentication library")
    (description
     "This library provides a C99 implementation for AWS client-side
authentication.")
    (home-page "https://github.com/awslabs/aws-c-auth")
    (license license:asl2.0)))

(define-public aws-c-s3
  (package
    (name "aws-c-s3")
    ; Update only when updating aws-crt-cpp.
    (version "0.1.26")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (string-append "https://github.com/awslabs/" name))
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0gaxnwwk0jbvkgjnxcgchq13xmn7jk5vjvjsps6b0vaz6bf12wv8"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       (list "-DBUILD_SHARED_LIBS=ON"
             (string-append "-DCMAKE_PREFIX_PATH="
                            (assoc-ref %build-inputs "aws-c-common"))
             "-DENABLE_NET_TESTS=OFF")))
    (propagated-inputs
     (list aws-c-auth aws-c-http))
    (synopsis "Amazon Web Services client library for Amazon S3")
    (description
     "This library provides a C99 client implementation of the Simple Storage
Service (S3) protocol for object storage.")
    (home-page "https://github.com/awslabs/aws-c-s3")
    (license license:asl2.0)))

(define-public aws-c-mqtt
  (package
    (name "aws-c-mqtt")
    ; Update only when updating aws-crt-cpp.
    (version "0.7.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (string-append "https://github.com/awslabs/" name))
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "19j6nw2v36c4yff4p0fbf0748s06fd5r9cp2yakry9ybn1ada99c"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       (list "-DBUILD_SHARED_LIBS=ON"
             (string-append "-DCMAKE_PREFIX_PATH="
                            (assoc-ref %build-inputs "aws-c-common")))))
    (propagated-inputs
     (list aws-c-http aws-c-io))
    (synopsis "Amazon Web Services MQTT library")
    (description
     "This library provides a C99 implementation of the Message Queuing
Telemetry Transport (MQTT) publish-subscribe messaging protocol.")
    (home-page "https://github.com/awslabs/aws-c-mqtt")
    (license license:asl2.0)))

(define-public utf8-h
  ;; The latest tag is used as there is no release.
  (let ((commit "500d4ea9f4c3449e5243c088d8af8700f7189734")
        (revision "0"))
    (package
      (name "utf8-h")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/sheredom/utf8.h")
                      (commit commit)))
                (file-name (git-file-name "utf8.h" version))
                (sha256
                 (base32
                  "0x9f7ivww8c7cigf4ck0hfx2bm79qgx6q4ccwzqbzkrmcrl9shfb"))))
      (build-system cmake-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (delete 'build)
           (delete 'configure)
           (replace 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (when tests?
                 (with-directory-excursion "test"
                   (invoke "cmake" ".")
                   (invoke "make")))))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (install-file "utf8.h" (string-append out "/include"))))))))
      (home-page "https://github.com/sheredom/utf8.h")
      (synopsis "Single header UTF-8 string functions for C and C++")
      (description "A simple one header solution to supporting UTF-8 strings in
C and C++.  The functions it provides are like those from the C header
string.h, but with a utf8* prefix instead of the str* prefix.")
      (license license:unlicense))))
