;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2015 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2016, 2018, 2019, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2016, 2020 Marius Bakke <marius@gnu.org>
;;; Copyright © 2017 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2017 Rene Saavedra <rennes@openmailbox.org>
;;; Copyright © 2017,2019 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2017 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2017 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018 Meiyo Peng <meiyo.peng@gmail.com>
;;; Copyright © 2019 Yoshinori Arai <kumagusu08@gmail.com>
;;; Copyright © 2019 Mădălin Ionel Patrașcu <madalinionel.patrascu@mdc-berlin.de>
;;; Copyright © 2019 Wiktor Żelazny <wzelazny@vurv.cz>
;;; Copyright © 2020 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Jean-Baptiste Volatier <jbv@pm.me>
;;; Copyright © 2021 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2021 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2021 Bonface Munyoki Kilyungi <me@bonfacemunyoki.com>
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

(define-module (gnu packages textutils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages java)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages slang)
  #:use-module (gnu packages syncthing)
  #:use-module (gnu packages web))

(define-public dos2unix
  (package
    (name "dos2unix")
    (version "7.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://waterlan.home.xs4all.nl/dos2unix/"
                           "dos2unix-" version ".tar.gz"))
       (sha256
        (base32 "00dfsf4rfyjb5j12gan8xjiirm0asshdz6dmd3l34a7ays6wadb0"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)))) ; no configure script
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("perl" ,perl)))
    (home-page "https://waterlan.home.xs4all.nl/dos2unix.html")
    (synopsis "DOS/Mac to Unix and vice versa text file format converter")
    (description
     "dos2unix is a tool to convert line breaks in a text file from Unix format
to DOS format and vice versa.")
    (license license:bsd-2)))

(define-public recode
  (package
    (name "recode")
    (version "3.7.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/rrthomas/recode/releases/"
                           "download/v" version "/recode-" version ".tar.gz"))
       (sha256
        (base32 "0m59sd1ca0zw1aydpc3m8sw03nc885knmccqryg7byzmqs585ia6"))))
    (build-system gnu-build-system)
    (native-inputs
     (list python python-cython))
    (home-page "https://github.com/rrthomas/recode")
    (synopsis "Text encoding converter")
    (description "The Recode library converts files between character sets and
usages.  It recognises or produces over 200 different character sets (or about
300 if combined with an iconv library) and transliterates files between almost
any pair.  When exact transliteration are not possible, it gets rid of
offending characters or falls back on approximations.  The recode program is a
handy front-end to the library.")
    (license license:gpl3+)))

(define-public enca
  (package
    (name "enca")
    (version "1.19")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/nijel/enca")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19q7cwwxmmk5j9438bsqdpjvdjawsd3zmw1zyqgi7s4m0rasr3ah"))))
    (build-system gnu-build-system)
    ;; enca-1.19 tests fail with recent recode.
    ;(inputs `(("recode" ,recode)))
    (home-page "https://github.com/nijel/enca")
    (synopsis "Text encoding detection tool")
    (description "Enca (Extremely Naive Charset Analyser) consists of libenca,
an encoding detection library, and enca, a command line frontend, integrating
libenca and several charset conversion libraries and tools.")
    (license license:gpl2)))

(define-public utf8proc
  (package
    (name "utf8proc")
    (version "2.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaStrings/utf8proc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xlkazhdnja4lksn5c9nf4bln5gjqa35a8gwlam5r0728w0h83qq"))))
    (build-system gnu-build-system)
    (native-inputs
     (let ((UNICODE_VERSION "13.0.0"))  ; defined in data/Makefile
       ;; Test data that is otherwise downloaded with curl.
       `(("NormalizationTest.txt"
          ,(origin
             (method url-fetch)
             (uri (string-append "https://www.unicode.org/Public/"
                                 UNICODE_VERSION "/ucd/NormalizationTest.txt"))
             (sha256
              (base32 "07g0ya4f6zfzvpp24ccxkb2yq568kh83gls85rjl950nv5fya3nn"))))
         ("GraphemeBreakTest.txt"
          ,(origin
             (method url-fetch)
             (uri (string-append "https://www.unicode.org/Public/"
                                 UNICODE_VERSION
                                 "/ucd/auxiliary/GraphemeBreakTest.txt"))
             (sha256
              (base32 "07f8rrvcsq4pibdz6zxggxy8w7zjjqyw2ggclqlhalyv45yv7prj"))))

         ;; For tests.
         ("perl" ,perl))))
    (arguments
     '(#:make-flags (list "CC=gcc"
                          (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'check 'check-data
           (lambda* (#:key inputs #:allow-other-keys)
             (for-each (lambda (i)
                         (copy-file (assoc-ref inputs i)
                                    (string-append "data/" i)))
                       '("NormalizationTest.txt" "GraphemeBreakTest.txt"))
             (substitute* "data/GraphemeBreakTest.txt"
               (("÷") "/")
               (("×") "+"))
             #t)))))
    (home-page "https://juliastrings.github.io/utf8proc/")
    (synopsis "C library for processing UTF-8 Unicode data")
    (description "utf8proc is a small C library that provides Unicode
normalization, case-folding, and other operations for data in the UTF-8
encoding, supporting Unicode version 9.0.0.")
    (license license:expat)))

(define-public utf8proc-2.6.1
  (package
    (inherit utf8proc)
    (name "utf8proc")
    (version "2.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaStrings/utf8proc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zqc6airkzkssbjxanx5v8blfk90180gc9id0dx8ncs54f1ib8w7"))))
    (arguments
     (substitute-keyword-arguments (package-arguments utf8proc)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'check-data
             (lambda* (#:key inputs native-inputs #:allow-other-keys)
               (display native-inputs)
               (for-each (lambda (i)
                           (copy-file (assoc-ref (or native-inputs inputs) i)
                                      (string-append "data/" i)))
                         '("NormalizationTest.txt" "GraphemeBreakTest.txt"
                           "DerivedCoreProperties.txt"))))))))
    (native-inputs
     (append
      (package-native-inputs utf8proc)
      (let ((UNICODE_VERSION "13.0.0"))
        `(("DerivedCoreProperties.txt"
           ,(origin
              (method url-fetch)
              (uri (string-append "https://www.unicode.org/Public/"
                                  UNICODE_VERSION "/ucd/DerivedCoreProperties.txt"))
              (sha256
               (base32 "0j12x112cd8fpgazkc8izxnhhpia44p1m36ff8yapslxndcmzm55"))))
          ;; For tests
          ("ruby" ,ruby)))))))

(define-public libconfuse
  (package
    (name "libconfuse")
    (version "3.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/martinh/libconfuse/"
                           "releases/download/v" version
                           "/confuse-" version ".tar.xz"))
       (sha256
        (base32 "043hqqykpprgrkw9s2hbdlxr308a7yxwsgxj4m8aadg1401hmm8x"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")))
    (home-page "https://github.com/martinh/libconfuse")
    (synopsis "Configuration file parser library")
    (description "libconfuse is a configuration file parser library.  It
supports sections and (lists of) values (strings, integers, floats, booleans
or other sections), as well as some other features (such as
single/double-quoted strings, environment variable expansion, functions and
nested include statements).")
    (license license:isc)))

(define-public libgtextutils
  (package
    (name "libgtextutils")
    (version "0.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/agordon/libgtextutils/releases/download/"
             version "/libgtextutils-" version ".tar.gz"))
       (sha256
        (base32 "0jiybkb2z58wa2msvllnphr4js2hvjvh988pavb3mzkgr6ihwbkr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'bootstrap
           (lambda _ (invoke "sh" "reconf")))
         (add-after 'set-paths 'hide-default-gcc
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gcc (assoc-ref inputs "gcc")))
               ;; Remove the default GCC from CPLUS_INCLUDE_PATH to prevent
               ;; conflicts with the GCC 5 input.
               (setenv "CPLUS_INCLUDE_PATH"
                       (string-join
                        (delete (string-append gcc "/include/c++")
                                (string-split (getenv "CPLUS_INCLUDE_PATH") #\:))
                        ":"))
               #t))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gcc@5" ,gcc-5) ; doesn't build with later versions
       ("libtool" ,libtool)))
    (home-page "https://github.com/agordon/libgtextutils")
    (synopsis "Gordon's text utils library")
    (description
     "libgtextutils is a text utilities library used by the fastx toolkit from
the Hannon Lab.")
    (license license:agpl3+)))

(define-public cityhash
  (let ((commit "8af9b8c"))
    (package
      (name "cityhash")
      (version (string-append "1.1-2." commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/google/cityhash")
                      (commit commit)))
                (file-name (string-append name "-" version ".tar.gz"))
                (sha256
                 (base32
                  "0n6skf5dv8yfl1ckax8dqhvsbslkwc9158zf2ims0xqdvzsahbi6"))))
      (build-system gnu-build-system)
      (arguments
       '(#:make-flags (list "CXXFLAGS=-g -O3")
         #:phases
         (modify-phases %standard-phases
           ;; citycrc is not installed by default but is used by some
           ;; programs.
           (add-after 'install 'install-citycrc
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (include (string-append out "/include")))
                 (install-file "src/citycrc.h" include))
               #t)))))
      (home-page "https://github.com/google/cityhash")
      (synopsis "C++ hash functions for strings")
      (description
       "CityHash provides hash functions for strings.  The functions mix the
input bits thoroughly but are not suitable for cryptography.")
      (license license:expat))))

(define-public ustr
  (package
    (name "ustr")
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.and.org/ustr/" version
                                  "/ustr-" version ".tar.bz2"))
              (sha256
               (base32
                "1i623ygdj7rkizj7985q9d6vj5amwg686aqb5j3ixpkqkyp6xbrx"))
              (patches (search-patches "ustr-fix-build-with-gcc-5.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             "HIDE="
             ;; Override "/sbin/ldconfig" with "echo" because we don't need
             ;; "ldconfig".
             "LDCONFIG=echo"
             (string-append "prefix=" (assoc-ref %outputs "out"))
             "all-shared")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-cross-compilation
           ;; The Makefile contains more insults than cross-compilation support.
           ;; It poorly reinvents autotools by compiling C programmes with $CC,
           ;; then tries to run them during the build.  Hard-code the results.
           (lambda _
             (substitute* "Makefile"
               (("\\./autoconf_64b")
                ,(if (target-64bit? (or (%current-target-system)
                                        (%current-system)))
                     "echo 1"
                     "echo 0"))
               (("\\./autoconf_vsnprintf") "echo 0"))))
         (add-after 'unpack 'omit-static-libraries
           ;; These are needed to build & test ustr, but don't install them.
           (lambda _
             (substitute* "Makefile"
               ((".*install.*LIB_STATIC.*") ""))))
         (add-after 'unpack 'disable-check-for-stdint
           (lambda _
             ;; Of course we have stdint.h, just not in /usr/include
             (substitute* '("Makefile"
                            "ustr-import.in")
               (("-f \"/usr/include/stdint.h\"") "-z \"\""))
             #t))
         ;; No configure script
         (delete 'configure))))
    (home-page "http://www.and.org/ustr/")
    (synopsis "String library with very low memory overhead")
    (description
     "Ustr is a string library for C with very low memory overhead.")
    ;; Quoted from the home page: "The License for the code is MIT, new-BSD,
    ;; LGPL, etc. ... if you need another license to help compatibility, just
    ;; ask for it.  It's basically public domain, without all the legal
    ;; problems for everyone that trying to make something public domain
    ;; entails."
    (license license:public-domain)))

(define-public ascii2binary
  (package
    (name "ascii2binary")
    (version "2.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://billposer.org/Software/Downloads/"
                           "ascii2binary-" version ".tar.bz2"))
       (sha256
        (base32 "0dc9fxcdmppbs9s06jvq61zbk552laxps0xyk098gj41697ihd96"))))
    (build-system gnu-build-system)
    (native-inputs
     (list gettext-minimal))
    (home-page "https://billposer.org/Software/a2b.html")
    (synopsis "Convert between ASCII, hexadecimal and binary representations")
    (description "The two programs are useful for generating test data, for
inspecting binary files, and for interfacing programs that generate textual
output to programs that require binary input and conversely.  They can also be
useful when it is desired to reformat numbers.

@itemize

@item @command{ascii2binary} reads input consisting of ascii or hexadecimal
   representation numbers separated by whitespace and produces as output
   the binary equivalents.  The type and precision of the binary output
   is selected using command line flags.

@item @command{binary2ascii} reads input consisting of binary numbers
   and converts them to their ascii or hexadecimal representation.
   Command line flags specify the type and size of the binary numbers
   and provide control over the format of the output.
   Unsigned integers may be written out in binary, octal, decimal,
   or hexadecimal.

   Signed integers may be written out only in binary or decimal.  Floating
   point numbers may be written out only decimal, either in standard or
   scientific notation.  (If you want to examine the binary representation
   of floating point numbers, just treat the input as a sequence of unsigned
   characters.)

@end itemize")
    (license license:gpl3)))

(define-public uniutils
  (package
    (name "uniutils")
    (version "2.27")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://billposer.org/Software/Downloads/"
                           "uniutils-" version ".tar.bz2"))
       (sha256
        (base32 "19w1510w87gx7n4qy3zsb0m467a4rn5scvh4ajajg7jh6x5xri08"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-dependency-tracking")
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'fix-paths
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (a2b (assoc-ref inputs "ascii2binary"))
                   (iconv (assoc-ref inputs "libiconv")))
               (substitute* "utf8lookup"
                 (("^ascii2binary ") (string-append a2b "/bin/ascii2binary "))
                 (("^uniname ") (string-append out "/bin/uniname "))
                 (("^iconv ") (string-append iconv "/bin/iconv ")))
             #t))))))
    (inputs
     (list ascii2binary libiconv))
    (home-page "https://billposer.org/Software/unidesc.html")
    (synopsis "Find out what is in a Unicode file")
    (description "Useful tools when working with Unicode files when one
doesn't know the writing system, doesn't have the necessary font, needs to
inspect invisible characters, needs to find out whether characters have been
combined or in what order they occur, or needs statistics on which characters
occur.

@itemize

@item @command{uniname} defaults to printing the character offset of each
character, its byte offset, its hex code value, its encoding, the glyph
itself, and its name.  It may also be used to validate UTF-8 input.

@item @command{unidesc} reports the character ranges to which different
portions of the text belong.   It can also be used to identify Unicode encodings
(e.g. UTF-16be) flagged by magic numbers.

@item @command{unihist} generates a histogram of the characters in its input.

@item @command{ExplicateUTF8} is intended for debugging or for learning about
Unicode.  It determines and explains the validity of a sequence of bytes as a
UTF8 encoding.

@item @command{utf8lookup} provides a handy way to look up Unicode characters
from the command line.

@item @command{unireverse} reverse each line of UTF-8 input
character-by-character.

@end itemize")
    (license license:gpl3)))

(define-public libconfig
  (package
    (name "libconfig")
    (version "1.7.3")
    (home-page "https://hyperrealm.github.io/libconfig/")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page "/dist/libconfig-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1rqynfxl1zxwk4b42sniz9xlw285aidcrsfih51p8dy0rbb6clal"))))
    (build-system gnu-build-system)
    (synopsis "C/C++ configuration file library")
    (description
     "Libconfig is a simple library for manipulating structured configuration
files.  This file format is more compact and more readable than XML.  And
unlike XML, it is type-aware, so it is not necessary to do string parsing in
application code.")
    (license license:lgpl2.1+)))

(define-public pfff
  (package
    (name "pfff")
    (version "1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/pfff/pfff")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1nxkfm7zliq3rmr7yp871sppwfnz71iz364m2sgazny71pzykggc"))))
    (build-system cmake-build-system)
    (home-page "https://biit.cs.ut.ee/pfff/")
    (synopsis "Probabilistic fast file fingerprinting tool")
    (description
     "pfff is a tool for calculating a compact digital fingerprint of a file
by sampling randomly from the file instead of reading it in full.
Consequently, the computation has a flat performance characteristic,
correlated with data variation rather than file size.  pfff can be as reliable
as existing hashing techniques, with provably negligible risk of collisions.")
    (license license:bsd-3)))

(define-public oniguruma
  (package
    (name "oniguruma")
    (version "6.9.5-rev1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/kkos/"
                                  "oniguruma/releases/download/v"
                                  ;; If there is a "-" in the version, convert
                                  ;; to underscore for this part of the URI.
                                  (string-map (lambda (c) (if (char=? #\- c) #\_ c))
                                              version)
                                  "/onig-" version ".tar.gz"))
              (sha256
               (base32
                "17m92k1n6bvza6m35fpd5g36zwpwm3hfz3478iwj5bvj2sfq8g6k"))))
    (build-system gnu-build-system)
    (arguments '(#:configure-flags '("--disable-static")))
    (home-page "https://github.com/kkos/oniguruma")
    (synopsis "Regular expression library")
    (description "Oniguruma is a regular expressions library.  The special
characteristic of this library is that different character encoding for every
regular expression object can be specified.")
    (license license:bsd-2)))

(define-public antiword
  (package
    (name "antiword")
    (version "0.37")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.winfield.demon.nl/linux"
                                  "/antiword-" version ".tar.gz"))
              (sha256
               (base32
                "1b7mi1l20jhj09kyh0bq14qzz8vdhhyf35gzwsq43mn6rc7h0b4f"))
              (patches (search-patches "antiword-CVE-2014-8123.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; There are no tests
       #:make-flags
       (list "-f" "Makefile.Linux"
             (string-append "GLOBAL_INSTALL_DIR="
                            (assoc-ref %outputs "out") "/bin")
             (string-append "GLOBAL_RESOURCES_DIR="
                            (assoc-ref %outputs "out") "/share/antiword"))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Ensure that mapping files can be found in the actual package
             ;; data directory.
             (substitute* "antiword.h"
               (("/usr/share/antiword")
                (string-append (assoc-ref outputs "out") "/share/antiword")))
             #t))
         (replace 'install
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "make" `("global_install" ,@make-flags)))))))
    (home-page "http://www.winfield.demon.nl/")
    (synopsis "Microsoft Word document reader")
    (description "Antiword is an application for displaying Microsoft Word
documents.  It can also convert the document to PostScript or XML.  Only
documents made by MS Word version 2 and version 6 or later are supported.  The
name comes from: \"The antidote against people who send Microsoft Word files
to everybody, because they believe that everybody runs Windows and therefore
runs Word\".")
    (license license:gpl2+)))

(define-public catdoc
  (package
    (name "catdoc")
    (version "0.95")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://ftp.wagner.pp.ru/pub/catdoc/"
                                  "catdoc-" version ".tar.gz"))
              (patches (search-patches "catdoc-CVE-2017-11110.patch"))
              (sha256
               (base32
                "15h7v3bmwfk4z8r78xs5ih6vd0pskn0rj90xghvbzdjj0cc88jji"))))
    (build-system gnu-build-system)
    ;; TODO: Also build `wordview` which requires `tk` – make a separate
    ;; package for this.
    (arguments
     '(#:tests? #f ; There are no tests
       #:configure-flags '("--disable-wordview")
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'fix-install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/share/man/man1"))))))))
    (home-page "https://www.wagner.pp.ru/~vitus/software/catdoc/")
    (synopsis "MS-Word to TeX or plain text converter")
    (description "@command{catdoc} extracts text from MS-Word files, trying to
preserve as many special printable characters as possible.  It supports
everything up to Word-97. Also supported are MS Write documents and RTF files.

@command{catdoc} does not preserve complex word formatting, but it can
translate some non-ASCII characters into TeX escape codes.  It's goal is to
extract plain text and allow you to read it and, probably, reformat with TeX,
according to TeXnical rules.

This package also provides @command{xls2csv}, which extracts data from Excel
spreadsheets and outputs it in comma-separated-value format, and
@command{catppt}, which extracts data from PowerPoint presentations.")
    (license license:gpl2+)))

(define-public utfcpp
  (package
    (name "utfcpp")
    (version "2.3.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/nemtrif/utfcpp")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1gr98d826z6wa58r1s5i7rz7q2x3r31v7zj0pjjlrc7gfxwklr4s"))))
    (build-system cmake-build-system)
    (arguments
     `(#:out-of-source? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'install              ; no install target
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (include (string-append out "/include"))
                    (doc (string-append out "/share/doc/" ,name)))
               (copy-recursively "source" include)
               (install-file "README.md" doc)
               #t))))))
    (home-page "https://github.com/nemtrif/utfcpp")
    (synopsis "Portable C++ library for handling UTF-8")
    (description "UTF8-CPP is a C++ library for handling UTF-8 encoded text
in a portable way.")
    (license license:boost1.0)))

(define-public dbacl
  (package
    (name "dbacl")
    (version "1.14.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/dbacl/dbacl/" version "/"
                           "dbacl-" version ".tar.gz"))
       (sha256
        (base32 "1gas0112wqjvwn9qg3hxnawk7h3prr0w9b2h68f3p1ifd1kzn3gz"))
       (patches (search-patches "dbacl-include-locale.h.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list
        (string-append "-I" (assoc-ref %build-inputs "slang")
                       "/include/slang")
        (string-append "-I" (assoc-ref %build-inputs "ncurses")
                       "/include/ncurses"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'delete-sample6-and-japanese
           (lambda _
             (substitute* "doc/Makefile.am"
               (("sample6.txt") "")
               (("japanese.txt") ""))
             (delete-file "doc/sample6.txt")
             (delete-file "doc/japanese.txt")
             (substitute* (list "src/tests/Makefile.am"
                                "src/tests/Makefile.in")
               (("dbacl-jap.shin") "")
               (("dbacl-jap.sh") ""))
             #t))
         (add-after 'unpack 'delete-test
           ;; See comments about the license.
           (lambda _
             (delete-file "src/tests/dbacl-jap.shin")
             #t))
         (add-after 'unpack 'fix-test-files
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (substitute* (find-files "src/tests/" "\\.shin$")
                 (("PATH=/bin:/usr/bin")
                  "#PATH=/bin:/usr/bin")
                 (("diff") (string-append (which "diff")))
                 (("tr") (string-append (which "tr"))))
               #t)))
         (replace 'bootstrap
           (lambda _
             (invoke "autoreconf" "-vif")
             #t)))))
    (inputs
     (list ncurses perl readline slang))
    (native-inputs
     (list libtool autoconf automake pkg-config))
    (home-page "https://www.lbreyer.com/dbacl.html")
    (synopsis "Bayesian text and email classifier")
    (description
     "dbacl is a fast Bayesian text and email classifier.  It builds a variety
of language models using maximum entropy (minimum divergence) principles, and
these can then be used to categorize input data automatically among multiple
categories.")
    ;; The software is licensed as GPLv3 or later, but
    ;; includes various sample texts in the doc dir:
    ;; - sample1.txt, sample3 and sampe5.txt are in the public domain,
    ;;   by Mark Twain.
    ;; - sample2.txt, sample4.txt are in the public domain, by Aristotle.
    ;; - sample6.txt is a forwarded email, copyright unknown.
    ;;   Guix does exclude sample6.txt.
    ;; - japanese.txt is a Japanese unoffical translation of the
    ;;   GNU General Public License, (c) by the Free Software Foundation.
    ;;   Guix excludes this file.
    (license (list license:gpl3+ license:public-domain))))

(define-public dotconf
  (package
    (name "dotconf")
    (version "1.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/williamh/dotconf")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1sc95hw5k2xagpafny0v35filmcn05k1ds5ghkldfpf6xw4hakp7"))))
    (build-system gnu-build-system)
    (arguments `(#:tests? #f))  ; FIXME maketest.sh does not work.
    (native-inputs
     (list autoconf automake libtool))
    (home-page "https://github.com/williamh/dotconf")
    (synopsis "Configuration file parser library")
    (description
     "C library for creating and parsing configuration files.")
    (license (list license:lgpl2.1         ; Main distribution.
                   license:asl1.1))))      ; src/readdir.{c,h}

(define-public drm-tools
  (package
    (name "drm-tools")
    (version "1.1.33")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/drmtools/drm_tools-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "187zbxw21zcg8gpyc13gxlycfw0n05a6rmqq6im5wr9zk1v1wj80"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ;the test suite fails
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'set-install-prefixes
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out")))
                        (substitute* "CMakeLists.txt"
                          (("tmp/testinstall")
                           (string-drop out 1))
                          (("/man/man1")
                           "/share/man/man1"))
                        #t)))
                  (add-after 'unpack 'adjust-test-paths
                    (lambda _
                      (substitute* '("test_extract_increment.sh"
                                     "test_extract_features.sh"
                                     "test_extract_features2.sh"
                                     "test_dmath.sh")
                        (("\\./extract") "extract")
                        (("\\./dmath") "dmath")
                        (("/usr/local/bin/") "")
                        (("/bin/rm") "rm")
                        (("/bin/cp") "cp"))
                      #t))
                  (delete 'check)
                  ;; The produced binaries are written directly to %output/bin.
                  (delete 'install)
                  (add-after 'build 'check
                    (lambda* (#:key outputs tests? #:allow-other-keys)
                      (when tests?
                        (let* ((out (assoc-ref outputs "out"))
                               (bin (string-append out "/bin")))
                          (setenv "PATH" (string-append bin ":"
                                                        (getenv "PATH")))
                          (with-directory-excursion
                              (format #f "../drm_tools-~a" ,version)
                            (invoke "sh" "test_all.sh")))))))))
    (native-inputs (list which)) ;for tests
    (inputs (list pcre))
    (home-page "http://drmtools.sourceforge.net/")
    (synopsis "Utilities to manipulate text and binary files")
    (description "The drm_tools package contains the following commands:
@table @command
@item accudate
An extended version of the \"date\" program that has sub-second accuracy.
@item binformat
Format complex binary data into text.
@item binload
Load data into a binary file using simple commands from the input.
@item binorder
Sort, merge, search, retrieve or generate test data consisting of fixed size
binary records.
@item binreplace
Find or find/replace in binary files.
@item binsplit
Split test data consisting of fixed size binary records into one or more
output streams.
@item chardiff
Find changes between two files at the character level.  Unlike \"diff\", it
lists just the characters that differ, so if the 40,000th character is
different only that one character will be shown, not the entire line.
@item columnadd
Add columns of integers, decimals, and/or times.
@item datasniffer
A utility for formatting binary data dumps.
@item dmath
Double precision interactive command line math calculator.
@item extract
Extract and emit data from text files based on character or token position.
@item execinput
A utility that reads from STDIN and executes each line as a command in a
sub-process.
@item indexed_text
A utility for rapid retrieval of text by line numbers, in any order, from a
text file.
@item mdump
Format binary data.
@item msgqueue
Create message queues and send/receive messages.
@item mbin
@itemx mbout
Multiple buffer in and out.  Used for buffering a lot of data between a slow
device and a fast device.  Mostly for buffering streaming tape drives for use
with slower network connections, so that streaming is maintained as much as
possible to minimize wear on the tape device.
@item pockmark
Corrupt data streams - useful for testing error correction and data recovery.
@item tarsieve
Filter, list, or split a tar file.
@end table")
    (license license:gpl2+)))

(define-public java-rsyntaxtextarea
  (package
    (name "java-rsyntaxtextarea")
    (version "2.6.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/bobbylight/RSyntaxTextArea")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0dyflzvxq2wvs0rgqfyi5yzzrb6r4bzw2dm8cl304dakxk38ddys"))))
    (build-system ant-build-system)
    (arguments
     `(;; FIXME: some tests fail because locale resources cannot be found.
       ;; Even when I add them to the class path,
       ;; RSyntaxTextAreaEditorKitDumbCompleteWordActionTest fails.
       #:tests? #f
       #:jar-name "rsyntaxtextarea.jar"))
    (native-inputs
     (list java-junit java-hamcrest-core))
    (home-page "https://bobbylight.github.io/RSyntaxTextArea/")
    (synopsis "Syntax highlighting text component for Java Swing")
    (description "RSyntaxTextArea is a syntax highlighting, code folding text
component for Java Swing.  It extends @code{JTextComponent} so it integrates
completely with the standard @code{javax.swing.text} package.  It is fast and
efficient, and can be used in any application that needs to edit or view
source code.")
    (license license:bsd-3)))

;; We use the sources from git instead of the tarball from pypi, because the
;; latter does not include the Cython source file from which bycython.cpp is
;; generated.
(define-public python-editdistance
  (let ((commit "3ea84a7dd3258c76aa3be851ef3d50e59c886846")
        (revision "1"))
    (package
      (name "python-editdistance")
      (version (string-append "0.3.1-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/aflc/editdistance")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1l43svsv12crvzphrgi6x435z6xg8m086c64armp8wzb4l8ccm7g"))))
      (build-system python-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'build-cython-code
             (lambda _
               (with-directory-excursion "editdistance"
                 (delete-file "bycython.cpp")
                 (invoke "cython" "--cplus" "bycython.pyx")))))))
      (native-inputs
       (list python-cython))
      (home-page "https://www.github.com/aflc/editdistance")
      (synopsis "Fast implementation of the edit distance (Levenshtein distance)")
      (description
       "This library simply implements Levenshtein distance algorithm with C++
and Cython.")
      (license license:expat))))

(define-public go-github.com-mattn-go-runewidth
  (let ((commit "703b5e6b11ae25aeb2af9ebb5d5fdf8fa2575211")
        (version "0.0.4")
        (revision "1"))
    (package
      (name "go-github.com-mattn-go-runewidth")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mattn/runewidth")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0znpyz71gajx3g0j2zp63nhjj2c07g16885vxv4ykwnrfmzbgk4w"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/mattn/go-runewidth"))
      (synopsis "@code{runewidth} provides Go functions to work with string widths")
      (description
       "The @code{runewidth} library provides Go functions for padding,
measuring and checking the width of strings, with support for East Asian
text.")
      (home-page "https://github.com/mattn/runewidth")
      (license license:expat))))

(define-public docx2txt
  (package
    (name "docx2txt")
    (version "1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/docx2txt/docx2txt/v"
                    version "/docx2txt-" version ".tgz"))
              (sha256
               (base32
                "06vdikjvpj6qdb41d8wzfnyj44jpnknmlgbhbr1w215420lpb5xj"))))
    (build-system gnu-build-system)
    (inputs
     (list unzip perl))
    (arguments
     `(#:tests? #f                      ; No tests.
       #:make-flags (list (string-append "BINDIR="
                                         (assoc-ref %outputs "out") "/bin")
                          (string-append "CONFIGDIR="
                                         (assoc-ref %outputs "out") "/etc")
                          ;; Makefile seems to be a bit dumb at guessing.
                          (string-append "INSTALL=install")
                          (string-append "PERL=perl"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'install 'fix-install
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (config (string-append out "/etc/docx2txt.config"))
                    (unzip (assoc-ref inputs "unzip")))
               ;; According to INSTALL, the .sh wrapper can be skipped.
               (delete-file (string-append bin "/docx2txt.sh"))
               (rename-file (string-append bin "/docx2txt.pl")
                            (string-append bin "/docx2txt"))
               (substitute* config
                 (("config_unzip         => '/usr/bin/unzip',")
                  (string-append "config_unzip         => '"
                                 unzip
                                 "/bin/unzip',")))
               ;; Makefile is wrong.
               (chmod config #o644)
               #t))))))
    (synopsis "Recover text from @file{.docx} files, with good formatting")
    (description
     "@command{docx2txt} is a Perl based command line utility to convert
Microsoft Office @file{.docx} documents to equivalent text documents.  Latest
version supports following features during text extraction.

@itemize
@item Character conversions; currency characters are converted to respective
names like Euro.
@item Capitalisation of text blocks.
@item Center and right justification of text fitting in a line of
(configurable) 80 columns.
@item Horizontal ruler, line breaks, paragraphs separation, tabs.
@item Indicating hyperlinked text along with the hyperlink (configurable).
@item Handling (bullet, decimal, letter, roman) lists along with (attempt at)
indentation.
@end itemize\n")
    (home-page "http://docx2txt.sourceforge.net")
    (license license:gpl3+)))

(define-public html2text
  ;; Use commit directly to get the fixes to the installation phase
  ;; that are not in a release yet.
  (let ((commit "05364c1028026a87d6f45130a8e86e1ee67704d2")
        (revision "1"))
    (package
      (name "html2text")
      (version (git-version "2.0.1_pre" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/grobian/html2text")
               (commit (string-append commit))))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0n6pl0nijcn4z3p0dvf3gmvvpjq261pagnk84s9f78c4c55bw5cm"))))
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags
         (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
         #:phases
         (modify-phases %standard-phases
           (replace 'configure
             ;; The configure script is not from autotools and does not accept
             ;; ‘--style’ options.  There is no proper error handling.
             (lambda* (#:key outputs #:allow-other-keys)
               (invoke "./configure"
                       (string-append "CXX=" ,(cxx-for-target))))))))
      (home-page "https://github.com/grobian/html2text")
      (synopsis "HTML to plain text converter")
      (description
       "@code{html2text} is a command line utility that converts HTML
documents into plain text.")
      (license (list license:bsd-4      ; cmp_nocase.cpp & sgml.h
                     license:gpl2+))))) ; everything else

(define-public odt2txt
  (package
    (name "odt2txt")
    (version "0.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/dstosberg/odt2txt/")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0im3kzvhxkjlx57w6h13mc9584c74ma1dyymgvpq2y61av3gc35v"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no make check
       #:make-flags (list "CC=gcc"
                          (string-append "DESTDIR=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         ;; no configure script
         (delete 'configure))))
    (inputs
     (list zlib))
    (home-page "https://github.com/dstosberg/odt2txt/")
    (synopsis "Converter from OpenDocument Text to plain text")
    (description "odt2txt is a command-line tool which extracts the text out
of OpenDocument Texts, as produced by OpenOffice.org, KOffice, StarOffice and
others.

odt2txt can also extract text from some file formats similar to OpenDocument
Text, such as OpenOffice.org XML (*.sxw), which was used by OpenOffice.org
version 1.x and older StarOffice versions.  To a lesser extent, odt2txt may be
useful to extract content from OpenDocument spreadsheets (*.ods) and
OpenDocument presentations (*.odp).")
    (license license:gpl2)))

(define-public bibutils
  (package
    (name "bibutils")
    (version "7.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/bibutils/"
                                  "bibutils_" version "_src.tgz"))

              (sha256
               (base32
                "1hxmwjjzw48w6hdh2x7ybkrhi1xngd55i67hrrd3wswa3vpql0kf"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list "--install-dir" (string-append #$output "/bin")
                   "--install-lib" (string-append #$output "/lib")
                   "--dynamic")
           #:make-flags
           #~(list (string-append "CC=" #+(cc-for-target))
                   (string-append "LDFLAGSIN=-Wl,-rpath=" #$output "/lib"))
           #:test-target "test"
           #:phases
           '(modify-phases %standard-phases
              (replace 'configure
                (lambda* (#:key configure-flags #:allow-other-keys)
                  ;; configure script is ill-formed, invoke it manually
                  (apply invoke "sh" "./configure" configure-flags))))))
    (home-page "https://bibutils.sourceforge.io/")
    (synopsis "Convert between various bibliography formats")
    (description "This package provides converters for various bibliography
formats (e.g. Bibtex, RIS, ...) using a common XML intermediate.")
    (license license:gpl2)))

(define-public opencc
  (package
    (name "opencc")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/BYVoid/OpenCC")
             (commit (string-append "ver." version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "07y0pvxjlmpcnym229j87qcfwnfm7vi10dad0a20xy6as55a9j3d"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; TODO: Unbundle tclap, darts-clone, gtest
           (delete-file-recursively "deps/rapidjson-1.1.0") #t))))
    (build-system cmake-build-system)
    (arguments
     ;; Required to locate the install script properly.
     `(#:out-of-source? #f
       #:parallel-build? #f             ;occasionally failed.
       #:imported-modules
       (,@%cmake-build-system-modules
        (guix build python-build-system))
       #:modules ((guix build cmake-build-system)
                  ((guix build python-build-system) #:prefix python:)
                  (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'prevent-rebuild-during-installation
           (lambda _
             (substitute* "python/setup.py"
               (("'build_py': BuildPyCommand,") ""))
             #t))
         (add-after 'unpack 'patch-3rd-party-references
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((rapidjson (assoc-ref inputs "rapidjson")))
               (substitute* "src/CMakeLists.txt"
                 (("../deps/rapidjson-1.1.0")
                  (string-append rapidjson "/include/rapidjson")))
               #t)))
         (add-before 'configure 'patch-python-binding-installation
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "python/opencc/__init__.py"
                 (("(_libopenccfile =).*$" _ prefix)
                  (format #f "~a os.path.join('~a/lib', _libopenccfilename)~%"
                          prefix out))
                 (("(_opencc_share_dir =).*$" _ prefix)
                  (format #f "~a '~a/share/opencc'~%" prefix out))))
             #t))
         (add-after 'install 'install-python-binding
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (dist (string-append
                           out "/lib/python"
                           ,(version-major+minor (package-version python))
                           "/site-packages")))
               (chdir "python")
               (mkdir-p dist)
               (setenv "PYTHONPATH"
                       (string-append dist ":" (getenv "GUIX_PYTHONPATH")))
               (invoke "python" "setup.py" "install"
                       "--root=/" "--single-version-externally-managed"
                       (string-append "--prefix=" out))
               #t)))
         (add-before 'install-python-binding 'enable-bytecode-determinism
           (assoc-ref python:%standard-phases 'enable-bytecode-determinism)))))
    (native-inputs
     `(("python" ,python-wrapper)
       ("rapidjson" ,rapidjson)
       ("python-setuptools" ,python-setuptools)
       ("python-wheel" ,python-wheel)))
    (home-page "https://github.com/BYVoid/OpenCC")
    (synopsis "Convert between Traditional Chinese and Simplified Chinese")
    (description "Open Chinese Convert (OpenCC) converts between Traditional
Chinese and Simplified Chinese, supporting character-level conversion,
phrase-level conversion, variant conversion, and regional idioms among
Mainland China, Taiwan, and Hong-Kong.")
    (license license:asl2.0)))

(define-public nkf
  (let ((commit "08043eadf4abdddcf277842217e3c77a24740dc2")
        (revision "1"))
    (package
      (name "nkf")
      ;; The commits corresponding to specific versions are published
      ;; here:
      ;; https://ja.osdn.net/projects/nkf/scm/git/nkf/
      (version "2.1.5")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/nurse/nkf")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0anw0knr1iy4p9w3d3b3pbwzh1c43p1i2q4c28kw9zviw8kx2rly"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; test for perl module
         #:make-flags (list "CC=gcc" "CFLAGS=-O2 -Wall -pedantic"
                            (string-append "prefix=" %output)
                            "MKDIR=mkdir -p")
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)))) ; No ./configure script
      (home-page "https://ja.osdn.net/projects/nkf/")
      (synopsis "Network Kanji Filter")
      (description "Nkf is yet another kanji code converter among networks,
hosts and terminals.  It converts input kanji code to designated kanji code
such as ISO-2022-JP, Shift_JIS, EUC-JP, UTF-8, UTF-16 or UTF-32.")
      (license license:zlib))))

(define-public python-pandocfilters
  (package
    (name "python-pandocfilters")
    (version "1.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pandocfilters" version))
       (sha256
        (base32 "0f3sb8q85dbwliv46cc1yvpy4r00qp4by5x8751kn8vx6c1rarqb"))))
    (build-system python-build-system)
    (home-page "https://github.com/jgm/pandocfilters")
    (synopsis "Python module for writing Pandoc filters")
    (description "Pandoc is a powerful utility to transform various
input formats into a wide range of output formats.  To alter the
exported output document, Pandoc allows the usage of filters, which
are pipes that read a JSON serialization of the Pandoc AST from stdin,
transform it in some way, and write it to stdout.  It allows therefore
to alter the processing of Pandoc's supported input formats, for
instance one can add new syntax elements to markdown, etc.

This package provides Python bindings.")
    (license license:bsd-3)))

(define-public aha
  (package
    (name "aha")
    (version "0.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/theZiz/aha")
             (commit version)))
       (sha256
        (base32 "1gywad0rvvz3c5balz8cxsnx0562hj2ngzqyr8zsy2mb4pn0lpgv"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure))
       #:make-flags (list (string-append "CC=" ,(cc-for-target))
                          (string-append "PREFIX="
                                         (assoc-ref %outputs "out")))
       #:tests? #f))                    ; no test suite
    (home-page "https://github.com/theZiz/aha")
    (synopsis "Converts terminal escape sequences to HTML")
    (description "@command{aha} (Ansi Html Adapter) converts ANSI escape sequences
of a Unix terminal to HTML code.")
    (license (list license:lgpl2.0+ license:mpl1.1))))

(define-public go-github-com-errata-ai-vale
  (package
    (name "go-github-com-errata-ai-vale")
    (version "2.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/errata-ai/vale")
             (commit (string-append "v" version))))
       (sha256
        (base32 "0d07fwha2220m8j24h527xl0gnl3svvyaywflgk5292d6g49ach2"))
       (file-name (git-file-name name version))))
    (build-system go-build-system)
    (native-inputs
     `(("github.com/sp13/afero" ,go-github-com-spf13-afero)
       ("github.com/urfave/cli" ,go-github-com-urfave-cli)
       ("github.com/olekukonko/tablewriter" ,go-github-com-olekukonko-tablewriter)
       ("github.com/mitchellh/mapstructure" ,go-github-com-mitchellh-mapstructure)))
    (arguments
     `(#:import-path "github.com/errata-ai/vale"
       #:install-source? #f))
    (home-page "https://github.com/errata-ai/vale")
    (synopsis "Fully customizable syntax-aware linter that focuses on your style")
    (description
     "Vale is a fully extensible linter that focuses on your own writing style
by making use of rules in individual YAML files.  It is syntax-aware on markup
languages such as HTML, Markdown, Asciidoc, and reStructuredText.  The community
around it also has a list of style guides implemented with Vale in
@url{https://github.com/errata-ai/styles, their styles repo}.")
    (license license:expat)))

(define-public utf-8-lineseparator
  (package
    (name "utf-8-lineseparator")
    (version "cj3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pflanze/utf-8-lineseparator")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1xnbcanqn5jr965gw9195ij6hz04clfm77m5776dysn9nykn20w1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list
         (string-append "CC=" ,(cc-for-target)))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "utf-8-lineseparator" bin)))))))
    (home-page "https://github.com/pflanze/utf-8-lineseparator")
    (synopsis "Line ending detection library")
    (description
"@code{utf-8-lineseparator} provides a tool to efficiently check text
files for valid UTF-8 use and to report which line endings they use.")
    (license license:expat)))

(define-public csvdiff
  (package
    (name "csvdiff")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aswinkarthik/csvdiff")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0cd1ikxsypjqisfnmr7zix3g7x8p892w77086465chyd39gpk97b"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/aswinkarthik/csvdiff"))
    (propagated-inputs
     (list go-golang-org-x-sys
           go-github-com-stretchr-testify
           go-github-com-spf13-cobra
           go-github-com-spf13-afero
           go-github-com-spaolacci-murmur3
           go-github-com-mattn-go-colorable
           go-github-com-fatih-color
           go-github-com-cespare-xxhash
           go-github-com-oneofone-xxhash))
    (home-page "https://github.com/aswinkarthik/csvdiff")
    (synopsis "Fast diff tool for comparing CSV files")
    (description "@code{csvdiff} is a diff tool to compute changes between two
CSV files.  It can compare CSV files with a million records in under 2
seconds.  It is specifically suited for comparing CSV files dumped from
database tables.  GNU Diff is orders of magnitude faster for comparing line by
line.  @code{csvdiff} supports

@itemize
@item Selective comparison of fields in a row
@item Specifying group of columns as primary-key to uniquely identify a row
@item Ignoring columns
@item Several output formats including colored git style output or
JSON for post-processing
@end itemize")
    (license license:expat)))

(define-public go-github-com-aswinkarthik-csvdiff
  (deprecated-package "go-github-com-aswinkarthik-csvdiff" csvdiff))
