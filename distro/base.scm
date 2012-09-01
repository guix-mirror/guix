;;; Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
;;; Copyright (C) 2012 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of Guix.
;;;
;;; Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (distro base)
  #:use-module (distro)
  #:use-module (guix packages)
  #:use-module (guix http)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils))

;;; Commentary:
;;;
;;; A Guix-based distribution.
;;;
;;; Code:

(define-public libsigsegv
  (package
   (name "libsigsegv")
   (version "2.10")
   (source (origin
            (method http-fetch)
            (uri (string-append
                  "http://ftp.gnu.org/gnu/libsigsegv/libsigsegv-"
                  version ".tar.gz"))
            (sha256
             (base32 "16hrs8k3nmc7a8jam5j1fpspd6sdpkamskvsdpcw6m29vnis8q44"))))
   (build-system gnu-build-system)
   (outputs '("out" "lib"))                   ; separate libdir from the rest
   (home-page "http://www.gnu.org/software/libsigsegv/")
   (description "GNU libsigsegv, a library to handle page faults in user mode")
   (long-description
"GNU libsigsegv is a library for handling page faults in user mode. A page
fault occurs when a program tries to access to a region of memory that is
currently not available. Catching and handling a page fault is a useful
technique for implementing pageable virtual memory, memory-mapped access to
persistent databases, generational garbage collectors, stack overflow
handlers, distributed shared memory, and more.")
   (license "GPLv2+")))

(define-public gawk
  (package
   (name "gawk")
   (version "4.0.0")
   (source (origin
            (method http-fetch)
            (uri (string-append "http://ftp.gnu.org/gnu/gawk/gawk-" version
                                ".tar.bz2"))
            (sha256
             (base32 "0sss7rhpvizi2a88h6giv0i7w5h07s2fxkw3s6n1hqvcnhrfgbb0"))))
   (build-system gnu-build-system)
   (arguments (case-lambda
                ((system)
                 (if (string=? system "i686-cygwin")
                     '(#:tests? #f)      ; work around test failure on Cygwin
                     '(#:parallel-tests? #f))) ; test suite fails in parallel
                ((system cross-system)
                 '(#:parallel-tests? #f))))
   (inputs `(("libsigsegv" ,libsigsegv)             ; headers
             ("libsigsegv/lib" ,libsigsegv "lib"))) ; library
   (home-page "http://www.gnu.org/software/gawk/")
   (description "GNU implementation of the Awk programming language")
   (long-description
    "Many computer users need to manipulate text files: extract and then
operate on data from parts of certain lines while discarding the rest, make
changes in various text files wherever certain patterns appear, and so on.
To write a program to do these things in a language such as C or Pascal is a
time-consuming inconvenience that may take many lines of code.  The job is
easy with awk, especially the GNU implementation: Gawk.

The awk utility interprets a special-purpose programming language that makes
it possible to handle many data-reformatting jobs with just a few lines of
code.")
   (license "GPLv3+")))

(define-public hello
  (package
   (name "hello")
   (version "2.8")
   (source (origin
            (method http-fetch)
            (uri (string-append "http://ftp.gnu.org/gnu/hello/hello-" version
                                ".tar.gz"))
            (sha256
             (base32 "0wqd8sjmxfskrflaxywc7gqw7sfawrfvdxd9skxawzfgyy0pzdz6"))))
   (build-system gnu-build-system)
   (arguments '(#:configure-flags
                `("--disable-dependency-tracking"
                  ,(string-append "--with-gawk="  ; for illustration purposes
                                 (assoc-ref %build-inputs "gawk")))))
   (inputs `(("gawk" ,gawk)))
   (description "GNU Hello")
   (long-description "Yeah...")
   (home-page "http://www.gnu.org/software/hello/")
   (license "GPLv3+")))

(define-public grep
  (package
   (name "grep")
   (version "2.14")
   (source (origin
            (method http-fetch)
            (uri (string-append "http://ftp.gnu.org/gnu/grep/grep-"
                                version ".tar.xz"))
            (sha256
             (base32
              "1qbjb1l7f9blckc5pqy8jlf6482hpx4awn2acmhyf5mv9wfq03p7"))))
   (build-system gnu-build-system)
   (description "GNU implementation of the Unix grep command")
   (long-description
    "The grep command searches one or more input files for lines containing a
match to a specified pattern.  By default, grep prints the matching
lines.")
   (license "GPLv3+")
   (home-page "http://www.gnu.org/software/grep/")))

(define-public sed
  (package
   (name "sed")
   (version "4.2.1")
   (source (origin
            (method http-fetch)
            (uri (string-append "http://ftp.gnu.org/gnu/sed/sed-" version
                                ".tar.bz2"))
            (sha256
             (base32
              "13wlsb4sf5d5a82xjhxqmdvrrn36rmw5f0pl9qyb9zkvldnb7hra"))))
   (build-system gnu-build-system)
   (description "GNU sed, a batch stream editor")
   (long-description
    "Sed (stream editor) isn't really a true text editor or text processor.
Instead, it is used to filter text, i.e., it takes text input and performs
some operation (or set of operations) on it and outputs the modified text.
Sed is typically used for extracting part of a file using pattern matching or
substituting multiple occurrences of a string within a file.")
   (license "GPLv3+")
   (home-page "http://www.gnu.org/software/sed/")))

(define-public tar
  (package
   (name "tar")
   (version "1.26")
   (source (origin
            (method http-fetch)
            (uri (string-append "http://ftp.gnu.org/gnu/tar/tar-"
                                version ".tar.bz2"))
            (sha256
             (base32
              "0hbdkzmchq9ycr2x1pxqdcgdbaxksh8c6ac0jf75jajhcks6jlss"))))
   (build-system gnu-build-system)
   (description "GNU implementation of the `tar' archiver")
   (long-description
    "The Tar program provides the ability to create tar archives, as well as
various other kinds of manipulation.  For example, you can use Tar on
previously created archives to extract files, to store additional files, or
to update or list files which were already stored.

Initially, tar archives were used to store files conveniently on magnetic
tape.  The name \"Tar\" comes from this use; it stands for tape archiver.
Despite the utility's name, Tar can direct its output to available devices,
files, or other programs (using pipes), it can even access remote devices or
files (as archives).")
   (license "GPLv3+")
   (home-page "http://www.gnu.org/software/tar/")))

(define-public gzip
  (package
   (name "gzip")
   (version "1.5")
   (source (origin
            (method http-fetch)
            (uri (string-append "http://ftp.gnu.org/gnu/gzip/gzip-"
                                version ".tar.gz"))
            (sha256
             (base32
              "18rm80kar7n016g8bsyy1a3zk50i2826xdgs874yh64rzj7nxmdm"))))
   (build-system gnu-build-system)
   (description "Gzip, the GNU zip compression program")
   (arguments
    ;; FIXME: The test suite wants `less', and optionally Perl.
    '(#:tests? #f))
   (long-description
    "gzip (GNU zip) is a popular data compression program written by Jean-loup
Gailly for the GNU project.  Mark Adler wrote the decompression part.

We developed this program as a replacement for compress because of the Unisys
and IBM patents covering the LZW algorithm used by compress.  These patents
made it impossible for us to use compress, and we needed a replacement.  The
superior compression ratio of gzip is just a bonus.")
   (license "GPLv3+")
   (home-page "http://www.gnu.org/software/gzip/")))

(define-public xz
  (package
   (name "xz")
   (version "5.0.4")
   (source (origin
            (method http-fetch)
            (uri (string-append "http://tukaani.org/xz/xz-" version
                                ".tar.gz"))
            (sha256
             (base32
              "1dl35ca8fdss9z2d6y234gxh24ixq904xksizrjmjr5dimwhax6n"))))
   (build-system gnu-build-system)
   (description
    "XZ, general-purpose data compression software, successor of LZMA")
   (long-description
    "XZ Utils is free general-purpose data compression software with high
compression ratio.  XZ Utils were written for POSIX-like systems, but also
work on some not-so-POSIX systems.  XZ Utils are the successor to LZMA Utils.

The core of the XZ Utils compression code is based on LZMA SDK, but it has
been modified quite a lot to be suitable for XZ Utils.  The primary
compression algorithm is currently LZMA2, which is used inside the .xz
container format.  With typical files, XZ Utils create 30 % smaller output
than gzip and 15 % smaller output than bzip2.")
   (license '("GPLv2+" "LGPLv2.1+"))              ; bits of both
   (home-page "http://tukaani.org/xz/")))

(define-public patch
  (package
   (name "patch")
   (version "2.6.1")
   (source (origin
            (method http-fetch)
            (uri (string-append "http://ftp.gnu.org/gnu/patch/patch-"
                                version ".tar.xz"))
            (sha256
             (base32
              "18012gxs9wc96izskp1q7bclrwns6rdmkn4jj31c8jbyfz6l5npq"))))
   (build-system gnu-build-system)
   (native-inputs '())                      ; FIXME: needs `ed' for the tests
   (arguments
    (case-lambda
      ((system) '(#:tests? #f))
      ((system cross-system)
       '(#:configure-flags '("ac_cv_func_strnlen_working=yes")))))
   (description "GNU Patch, a program to apply differences to files")
   (long-description
    "GNU Patch takes a patch file containing a difference listing produced by
the diff program and applies those differences to one or more original files,
producing patched versions.")
   (license "GPLv3+")
   (home-page "http://savannah.gnu.org/projects/patch/")))

(define-public diffutils
  (package
   (name "diffutils")
   (version "3.2")
   (source (origin
            (method http-fetch)
            (uri (string-append "http://ftp.gnu.org/gnu/diffutils/diffutils-"
                                version ".tar.xz"))
            (sha256
             (base32
              "0jci0wv68025xd0s0rq4s5qxpx56dd9d730lka63qpzk1rfvfkxb"))))
   (build-system gnu-build-system)
   (description "Programs to find differences among text files")
   (long-description
    "GNU Diffutils is a package of several programs related to finding
differences between files.

Computer users often find occasion to ask how two files differ. Perhaps one
file is a newer version of the other file. Or maybe the two files started out
as identical copies but were changed by different people.

You can use the diff command to show differences between two files, or each
corresponding file in two directories. diff outputs differences between files
line by line in any of several formats, selectable by command line
options. This set of differences is often called a ‘diff’ or ‘patch’. For
files that are identical, diff normally produces no output; for
binary (non-text) files, diff normally reports only that they are different.

You can use the cmp command to show the offsets and line numbers where two
files differ. cmp can also show all the characters that differ between the
two files, side by side.

You can use the diff3 command to show differences among three files. When two
people have made independent changes to a common original, diff3 can report
the differences between the original and the two changed versions, and can
produce a merged file that contains both persons' changes together with
warnings about conflicts.

You can use the sdiff command to merge two files interactively.")
   (license "GPLv3+")
   (home-page "http://www.gnu.org/software/diffutils/")))

(define-public findutils
  (package
   (name "findutils")
   (version "4.4.2")
   (source (origin
            (method http-fetch)
            (uri (string-append "http://ftp.gnu.org/gnu/findutils/findutils-"
                                version ".tar.gz"))
            (sha256
             (base32
              "0amn0bbwqvsvvsh6drfwz20ydc2czk374lzw5kksbh6bf78k4ks3"))))
   (build-system gnu-build-system)
   (native-inputs
    `(("patch/absolute-paths"
       ,(search-patch "findutils-absolute-paths.patch"))))
   (arguments
    (case-lambda
      ((system)
       `(#:patches (list (assoc-ref %build-inputs "patch/absolute-paths"))))
      ((system cross-system)
       ;; Work around cross-compilation failure.
       ;; See <http://savannah.gnu.org/bugs/?27299#comment1>.
       `(#:configure-flags '("gl_cv_func_wcwidth_works=yes")
         ,@(arguments cross-system)))))
   (description "Basic directory searching utilities of the GNU operating
system")
   (long-description
    "The GNU Find Utilities are the basic directory searching utilities of
the GNU operating system.  These programs are typically used in conjunction
with other programs to provide modular and powerful directory search and file
locating capabilities to other commands.

The tools supplied with this package are:

  * find - search for files in a directory hierarchy;
  * locate - list files in databases that match a pattern;
  * updatedb - update a file name database;
  * xargs - build and execute command lines from standard input.
")
   (license "GPLv3+")
   (home-page "http://www.gnu.org/software/findutils/")))

(define-public coreutils
  (package
   (name "coreutils")
   (version "8.19")
   (source (origin
            (method http-fetch)
            (uri (string-append "http://ftp.gnu.org/gnu/coreutils/coreutils-"
                                version ".tar.xz"))
            (sha256
             (base32
              "1rx9x3fp848w4nny7irdkcpkan9fcx24d99v5dkwgkyq7wc76f5d"))))
   (build-system gnu-build-system)
   (inputs `())                      ; TODO: optional deps: SELinux, ACL, GMP
   (arguments
    '(;; Perl is missing, and some tests are failing.
      #:tests? #f))
   (description
    "The basic file, shell and text manipulation utilities of the GNU
operating system")
   (long-description
    "The GNU Core Utilities are the basic file, shell and text manipulation
utilities of the GNU operating system.  These are the core utilities which
are expected to exist on every operating system.")
   (license "GPLv3+")
   (home-page "http://www.gnu.org/software/coreutils/")))

(define-public m4
  (package
   (name "m4")
   (version "1.4.16")
   (source (origin
            (method http-fetch)
            (uri (string-append "http://ftp.gnu.org/gnu/m4/m4-"
                                version ".tar.bz2"))
            (sha256
             (base32
              "035r7ma272j2cwni2961jp22k6bn3n9xwn3b3qbcn2yrvlghql22"))))
   (build-system gnu-build-system)
   (arguments (case-lambda
                ((system)
                 ;; XXX: Disable tests on those platforms with know issues.
                 `(#:tests? ,(not (member system
                                          '("x86_64-darwin"
                                            "i686-cygwin"
                                            "i686-sunos")))
                   #:patches (list (assoc-ref %build-inputs "patch/s_isdir")
                                   (assoc-ref %build-inputs
                                              "patch/readlink-EINVAL")
                                   (assoc-ref %build-inputs "patch/gets"))))
                ((system cross-system)
                 `(#:patches (list (assoc-ref %build-inputs "patch/s_isdir")
                                   (assoc-ref %build-inputs
                                              "patch/readlink-EINVAL")
                                   (assoc-ref %build-inputs "patch/gets"))))))
   (inputs `(("patch/s_isdir" ,(search-patch "m4-s_isdir.patch"))
             ("patch/readlink-EINVAL"
              ,(search-patch "m4-readlink-EINVAL.patch"))
             ("patch/gets" ,(search-patch "m4-gets-undeclared.patch"))))
   (description "GNU M4, a macro processor")
   (long-description
    "GNU M4 is an implementation of the traditional Unix macro processor.  It
is mostly SVR4 compatible although it has some extensions (for example,
handling more than 9 positional parameters to macros).  GNU M4 also has
built-in functions for including files, running shell commands, doing
arithmetic, etc.

GNU M4 is a macro processor in the sense that it copies its input to the
output expanding macros as it goes.  Macros are either builtin or
user-defined and can take any number of arguments.  Besides just doing macro
expansion, m4 has builtin functions for including named files, running UNIX
commands, doing integer arithmetic, manipulating text in various ways,
recursion etc...  m4 can be used either as a front-end to a compiler or as a
macro processor in its own right.")
   (license "GPLv3+")
   (home-page "http://www.gnu.org/software/m4/")))

(define-public gnu-make
  (package
   (name "make")
   (version "3.82")
   (source (origin
            (method http-fetch)
            (uri (string-append "http://ftp.gnu.org/gnu/make/make-" version
                                ".tar.bz2"))
            (sha256
             (base32
              "0ri98385hsd7li6rh4l5afcq92v8l2lgiaz85wgcfh4w2wzsghg2"))))
   (build-system gnu-build-system)
   (native-inputs
    `(("patch/impure-dirs" ,(search-patch "make-impure-dirs.patch"))))
   (arguments `(#:patches (list (assoc-ref %build-inputs
                                           "patch/impure-dirs"))))
   (description "GNU Make, a program controlling the generation of non-source
files from sources")
   (long-description
    "Make is a tool which controls the generation of executables and other
non-source files of a program from the program's source files.

Make gets its knowledge of how to build your program from a file called the
makefile, which lists each of the non-source files and how to compute it from
other files. When you write a program, you should write a makefile for it, so
that it is possible to use Make to build and install the program.")
   (license "GPLv3+")
   (home-page "http://www.gnu.org/software/make/")))

(define-public gmp
  (package
   (name "gmp")
   (version "5.0.5")
   (source (origin
            (method http-fetch)
            (uri (string-append "http://ftp.gnu.org/gnu/gmp/gmp-" version
                                ".tar.bz2"))
            (sha256
             (base32
              "1jfymbr90mpn0zw5sg001llqnvf2462y77vgjknrmfs1rjn8ln0z"))))
   (build-system gnu-build-system)
   (native-inputs `(("m4" ,m4)))
   (arguments `(#:configure-flags
                '(;; Build a "fat binary", with routines for several
                  ;; sub-architectures.
                  "--enable-fat"
                  "--enable-cxx")))
   (description "GMP, the GNU multiple precision arithmetic library")
   (long-description
    "GMP is a free library for arbitrary precision arithmetic, operating on
signed integers, rational numbers, and floating point numbers.  There is no
practical limit to the precision except the ones implied by the available
memory in the machine GMP runs on.  GMP has a rich set of functions, and the
functions have a regular interface.

The main target applications for GMP are cryptography applications and
research, Internet security applications, algebra systems, computational
algebra research, etc.

GMP is carefully designed to be as fast as possible, both for small operands
and for huge operands.  The speed is achieved by using fullwords as the basic
arithmetic type, by using fast algorithms, with highly optimised assembly
code for the most common inner loops for a lot of CPUs, and by a general
emphasis on speed.

GMP is faster than any other bignum library.  The advantage for GMP increases
with the operand sizes for many operations, since GMP uses asymptotically
faster algorithms.")
   (license "LGPLv3+")
   (home-page "http://gmplib.org/")))

(define-public mpfr
  (package
   (name "mpfr")
   (version "3.1.1")
   (source (origin
            (method http-fetch)
            (uri (string-append "http://ftp.gnu.org/gnu/mpfr/mpfr-" version
                                ".tar.xz"))
            (sha256 (base32
                     "0ym1ylcq803n52qrggxqmkz66gbn8ncc3ybawal31v5y5p1srma9"))))
   (build-system gnu-build-system)
   (inputs `(("gmp" ,gmp)))
   (description "GNU MPFR, a library for multiple-precision floating-point
arithmetic")
   (long-description
    "The GNU MPFR library is a C library for multiple-precision
floating-point computations with correct rounding.  MPFR is based on the GMP
multiple-precision library.

The main goal of MPFR is to provide a library for multiple-precision
floating-point computation which is both efficient and has a well-defined
semantics.  It copies the good ideas from the ANSI/IEEE-754 standard for
double-precision floating-point arithmetic (53-bit mantissa).")
   (license "LGPLv3+")
   (home-page "http://www.mpfr.org/")))

(define-public mpc
  (package
   (name "mpc")
   (version "0.9")
   (source (origin
            (method http-fetch)
            (uri (string-append
                  "http://www.multiprecision.org/mpc/download/mpc-"
                  version ".tar.gz"))
            (sha256 (base32
                     "1b29n3gd9awla1645nmyy8dkhbhs1p0g504y0n94ai8d5x1gwgpx"))))
   (build-system gnu-build-system)
   (inputs `(("gmp" ,gmp)
             ("mpfr" ,mpfr)))
   (description "GNU MPC, a library for multiprecision complex arithmetic
with exact rounding")
   (long-description
    "GNU MPC is a C library for the arithmetic of complex numbers with
arbitrarily high precision and correct rounding of the result.  It is built
upon and follows the same principles as GNU MPFR.")
   (license "LGPLv3+")
   (home-page "http://mpc.multiprecision.org/")))

(define-public binutils
  (package
   (name "binutils")
   (version "2.22")
   (source (origin
            (method http-fetch)
            (uri (string-append "http://ftp.gnu.org/gnu/binutils/binutils-"
                                version ".tar.bz2"))
            (sha256
             (base32
              "1a9w66v5dwvbnawshjwqcgz7km6kw6ihkzp6sswv9ycc3knzhykc"))))
   (build-system gnu-build-system)

   ;; TODO: Add dependency on zlib + those for Gold.
   (native-inputs
    `(("patch/new-dtags" ,(search-patch "binutils-ld-new-dtags.patch"))))
   (arguments
    `(#:patches (list (assoc-ref %build-inputs "patch/new-dtags"))

      ;; Add `-static-libgcc' to not retain a dependency on GCC when
      ;; bootstrapping.
      #:configure-flags '("LDFLAGS=-static-libgcc")))

   (description "GNU Binutils, tools for manipulating binaries (linker,
assembler, etc.)")
   (long-description
    "The GNU Binutils are a collection of binary tools.  The main ones are
`ld' (the GNU linker) and `as' (the GNU assembler).  They also include the
BFD (Binary File Descriptor) library, `gprof', `nm', `strip', etc.")
   (license "GPLv3+")
   (home-page "http://www.gnu.org/software/binutils/")))

(define-public gcc-4.7
  (let ((stripped? #t))                         ; TODO: make this a parameter
    (package
     (name "gcc")
     (version "4.7.1")
     (source (origin
              (method http-fetch)
              (uri (string-append "http://ftp.gnu.org/gnu/gcc/gcc-"
                                  version "/gcc-" version ".tar.bz2"))
              (sha256
               (base32
                "0vs0v89zzgkngkw2p8kdynyk7j8ky4wf6zyrg3rsschpl1pky28n"))))
     (build-system gnu-build-system)
     (inputs `(("libc" ,(nixpkgs-derivation* "glibc"))
               ("gmp" ,gmp)
               ("mpfr" ,mpfr)
               ("mpc" ,mpc)))           ; TODO: libelf, ppl, cloog, zlib, etc.
     (arguments
      `(#:modules ((guix build utils)
                   (guix build gnu-build-system)
                   (ice-9 regex))                 ; we need this one
        #:out-of-source? #t
        #:strip-binaries? ,stripped?
        #:configure-flags
        `("--enable-plugin"
          "--enable-languages=c,c++"
          "--disable-multilib"
          ,(string-append "--with-native-system-header-dir="
                          (assoc-ref %build-inputs "libc")
                          "/include"))
        #:make-flags
        (let ((libc (assoc-ref %build-inputs "libc")))
          `(,(string-append "LDFLAGS_FOR_BUILD="
                            "-L" libc "/lib "
                            "-Wl,-dynamic-linker "
                            "-Wl," libc "/lib/ld-linux-x86-64.so.2")
            ,(string-append "BOOT_CFLAGS=-O2 "
                            ,(if stripped? "-g0" "-g"))))

        ;; Exclude libc from $LIBRARY_PATH since the compiler being used
        ;; should know whether its libc is, and to avoid linking build tools
        ;; like `genhooks' against the wrong libc (for instance, when
        ;; building a gcc-for-glibc-2.16 with a gcc-for-glibc-2.13,
        ;; `genhooks' could end up being linked with glibc-2.16 but using
        ;; crt*.o from glibc-2.13.)
        #:path-exclusions '(("LIBRARY_PATH" "libc"))

        #:tests? #f
        #:phases
        (alist-cons-before
         'configure 'pre-configure
         (lambda* (#:key inputs outputs #:allow-other-keys)
           (let ((out  (assoc-ref outputs "out"))
                 (libc (assoc-ref inputs "libc")))
             ;; Fix the dynamic linker's file name.
             (substitute* "gcc/config/i386/linux64.h"
               (("#define GLIBC_DYNAMIC_LINKER([^ ]*).*$" _ suffix)
                (format #f "#define GLIBC_DYNAMIC_LINKER~a \"~a\"~%"
                        suffix
                        (string-append libc "/lib/ld-linux-x86-64.so.2"))))

             ;; Tell where to find libstdc++, libc, and `?crt*.o', except
             ;; `crt{begin,end}.o', which come with GCC.
             (substitute* ("gcc/config/gnu-user.h"
                           "gcc/config/i386/gnu-user.h"
                           "gcc/config/i386/gnu-user64.h")
               (("#define LIB_SPEC (.*)$" _ suffix)
                (format #f "#define LIB_SPEC \"-L~a/lib -rpath=~a/lib64 -rpath=~a/lib \" ~a~%"
                        libc out out suffix))
               (("^.*crt([^\\.])\\.o.*$" line)
                (regexp-substitute/global #f
                                          "([a-zA-Z]?)crt([^\\.])\\.o"
                                          (string-append line "\n")
                                          'pre libc "/lib/" 1 "crt" 2 ".o"
                                          'post)))))
         (alist-cons-after
          'configure 'post-configure
          (lambda _
            ;; Don't store configure flags, to avoid retaining references to
            ;; build-time dependencies---e.g., `--with-ppl=/nix/store/xxx'.
            (substitute* "Makefile"
              (("^TOPLEVEL_CONFIGURE_ARGUMENTS=(.*)$" _ rest)
               "TOPLEVEL_CONFIGURE_ARGUMENTS=\n")))
          (alist-replace 'install
                         (lambda* (#:key outputs #:allow-other-keys)
                           (zero?
                            (system* "make"
                                     ,(if stripped?
                                          "install-strip"
                                          "install"))))
                         %standard-phases)))))

     (properties `((gcc-libc . ,(assoc-ref inputs "libc"))))
     (description "The GNU Compiler Collection")
     (long-description
      "The GNU Compiler Collection includes compiler front ends for C, C++,
Objective-C, Fortran, OpenMP for C/C++/Fortran, Java, and Ada, as well as
libraries for these languages (libstdc++, libgcj, libgomp,...).

GCC development is a part of the GNU Project, aiming to improve the compiler
used in the GNU system including the GNU/Linux variant.")
     (license "GPLv3+")
     (home-page "http://gcc.gnu.org/"))))

(define-public ncurses
  (let ((post-install-phase
         '(lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              ;; When building a wide-character (Unicode) build, create backward
              ;; compatibility links from the the "normal" libraries to the
              ;; wide-character libraries (e.g. libncurses.so to libncursesw.so).
              (with-directory-excursion (string-append out "/lib")
                (for-each (lambda (lib)
                            (define libw.a
                              (string-append "lib" lib "w.a"))
                            (define lib.a
                              (string-append "lib" lib ".a"))
                            (define libw.so.x
                              (string-append "lib" lib "w.so.5"))
                            (define lib.so.x
                              (string-append "lib" lib ".so.5"))
                            (define lib.so
                              (string-append "lib" lib ".so"))

                            (when (file-exists? libw.a)
                              (format #t "creating symlinks for `lib~a'~%" lib)
                              (symlink libw.a lib.a)
                              (symlink libw.so.x lib.so.x)
                              (false-if-exception (delete-file lib.so))
                              (call-with-output-file lib.so
                                (lambda (p)
                                  (format p "INPUT (-l~aw)~%" lib)))))
                          '("curses" "ncurses" "form" "panel" "menu")))))))
    (package
     (name "ncurses")
     (version "5.9")
     (source (origin
              (method http-fetch)
              (uri (string-append "http://ftp.gnu.org/gnu/ncurses/ncurses-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0fsn7xis81za62afan0vvm38bvgzg5wfmv1m86flqcj0nj7jjilh"))))
     (build-system gnu-build-system)
     (arguments
      (case-lambda
        ((system)
         `(#:configure-flags
           `("--with-shared" "--without-debug" "--enable-widec"

             ;; By default headers land in an `ncursesw' subdir, which is not
             ;; what users expect.
             ,(string-append "--includedir=" (assoc-ref %outputs "out")
                             "/include")

             ;; C++ bindings fail to build on
             ;; `i386-pc-solaris2.11' with GCC 3.4.3:
             ;; <http://bugs.opensolaris.org/bugdatabase/view_bug.do?bug_id=6395191>.
             ,,@(if (string=? system "i686-solaris")
                    '("--without-cxx-binding")
                    '()))
           #:tests? #f                            ; no "check" target
           #:phases (alist-cons-after 'install 'post-install
                                      ,post-install-phase
                                      %standard-phases)))
        ((system cross-system)
         (arguments cross-system))))
     (self-native-input? #t)
     (description
      "GNU Ncurses, a free software emulation of curses in SVR4 and more")
     (long-description
      "The Ncurses (new curses) library is a free software emulation of curses
in System V Release 4.0, and more.  It uses Terminfo format, supports pads
and color and multiple highlights and forms characters and function-key
mapping, and has all the other SYSV-curses enhancements over BSD Curses.

The ncurses code was developed under GNU/Linux.  It has been in use for some
time with OpenBSD as the system curses library, and on FreeBSD and NetBSD as
an external package.  It should port easily to any ANSI/POSIX-conforming
UNIX.  It has even been ported to OS/2 Warp!")
     (license "X11")
     (home-page "http://www.gnu.org/software/ncurses/"))))

(define-public readline
  (package
   (name "readline")
   (version "6.2")
   (source (origin
            (method http-fetch)
            (uri (string-append "http://ftp.gnu.org/gnu/readline/readline-"
                                version ".tar.gz"))
            (sha256
             (base32
              "10ckm2bd2rkxhvdmj7nmbsylmihw0abwcsnxf8y27305183rd9kr"))))
   (build-system gnu-build-system)
   (propagated-inputs `(("ncurses" ,ncurses)))
   (inputs `(("patch/link-ncurses"
              ,(search-patch "readline-link-ncurses.patch"))))
   (arguments `(#:patches (list (assoc-ref %build-inputs
                                           "patch/link-ncurses"))
                #:patch-flags '("-p0")))
   (description "GNU Readline, a library for interactive line editing")
   (long-description
    "The GNU Readline library provides a set of functions for use by
applications that allow users to edit command lines as they are typed in.
Both Emacs and vi editing modes are available.  The Readline library includes
additional functions to maintain a list of previously-entered command lines,
to recall and perhaps reedit those lines, and perform csh-like history
expansion on previous commands.

The history facilites are also placed into a separate library, the History
library, as part of the build process.  The History library may be used
without Readline in applications which desire its capabilities.")
   (license "GPLv3+")
   (home-page "http://savannah.gnu.org/projects/readline/")))

(define-public bash
  (let ((cppflags (string-join '("-DSYS_BASHRC='\"/etc/bashrc\"'"
                                 "-DSYS_BASH_LOGOUT='\"/etc/bash_logout\"'"
                                 "-DDEFAULT_PATH_VALUE='\"/no-such-path\"'"
                                 "-DSTANDARD_UTILS_PATH='\"/no-such-path\"'"
                                 "-DNON_INTERACTIVE_LOGIN_SHELLS"
                                 "-DSSH_SOURCE_BASHRC")
                               " ")))
    (package
     (name "bash")
     (version "4.2")
     (source (origin
              (method http-fetch)
              (uri (string-append
                    "http://ftp.gnu.org/gnu/bash/bash-" version ".tar.gz"))
              (sha256
               (base32
                "1n5kbblp5ykbz5q8aq88lsif2z0gnvddg9babk33024wxiwi2ym2"))))
     (build-system gnu-build-system)
     (inputs `(("readline" ,readline)))           ; TODO: add texinfo
     (arguments
      `(#:configure-flags '("--with-installed-readline"
                            ,(string-append "CPPFLAGS=" cppflags))

        ;; XXX: The tests have a lot of hard-coded paths, so disable them
        ;; for now.
        #:tests? #f

        #:phases
        (alist-cons-after 'install 'post-install
                          (lambda* (#:key outputs #:allow-other-keys)
                            ;; Add a `bash' -> `sh' link.
                            (let ((out (assoc-ref outputs "out")))
                              (with-directory-excursion
                                  (string-append out "/bin")
                                (symlink "bash" "sh"))))
                          %standard-phases)))
     (description "GNU Bourne-Again Shell")
     (long-description
      "Bash is the shell, or command language interpreter, that will appear in
the GNU operating system.  Bash is an sh-compatible shell that incorporates
useful features from the Korn shell (ksh) and C shell (csh).  It is intended
to conform to the IEEE POSIX P1003.2/ISO 9945.2 Shell and Tools standard.  It
offers functional improvements over sh for both programming and interactive
use.  In addition, most sh scripts can be run by Bash without
modification.")
     (license "GPLv3+")
     (home-page "http://www.gnu.org/software/bash/"))))

(define-public libtool
  (package
   (name "libtool")
   (version "2.4.2")
   (source (origin
            (method http-fetch)
            (uri (string-append "http://ftp.gnu.org/gnu/libtool/libtool-"
                                version ".tar.gz"))
            (sha256
             (base32
              "0649qfpzkswgcj9vqkkr9rn4nlcx80faxpyqscy2k1x9c94f93dk"))))
   (build-system gnu-build-system)
   (native-inputs `(("m4" ,m4)
                    ("perl" ,(nixpkgs-derivation* "perl"))))
   (description "GNU Libtool, a generic library support script")
   (long-description
    "GNU libtool is a generic library support script.  Libtool hides the
complexity of using shared libraries behind a consistent, portable interface.

To use libtool, add the new generic library building commands to your
Makefile, Makefile.in, or Makefile.am.  See the documentation for
details.")
   (license "GPLv3+")
   (home-page "http://www.gnu.org/software/libtool/")))

(define-public libunistring
  (package
   (name "libunistring")
   (version "0.9.3")
   (source (origin
            (method http-fetch)
            (uri (string-append
                  "http://ftp.gnu.org/gnu/libunistring/libunistring-"
                  version ".tar.gz"))
            (sha256
             (base32
              "18q620269xzpw39dwvr9zpilnl2dkw5z5kz3mxaadnpv4k3kw3b1"))))
   (propagated-inputs '())                  ; FIXME: add libiconv when !glibc
   (build-system gnu-build-system)
   (description "GNU Libunistring, a Unicode string library")
   (long-description
    "This library provides functions for manipulating Unicode strings and for
manipulating C strings according to the Unicode standard.

GNU libunistring is for you if your application involves non-trivial text
processing, such as upper/lower case conversions, line breaking, operations
on words, or more advanced analysis of text.  Text provided by the user can,
in general, contain characters of all kinds of scripts.  The text processing
functions provided by this library handle all scripts and all languages.

libunistring is for you if your application already uses the ISO C / POSIX
<ctype.h>, <wctype.h> functions and the text it operates on is provided by
the user and can be in any language.

libunistring is also for you if your application uses Unicode strings as
internal in-memory representation.")
   (home-page "http://www.gnu.org/software/libunistring/")
   (license "LGPLv3+")))

(define-public recutils
  (package
   (name "recutils")
   (version "1.5")
   (source (origin
            (method http-fetch)
            (uri (string-append "http://ftp.gnu.org/gnu/recutils/recutils-"
                                version ".tar.gz"))
            (sha256
             (base32
              "1v2xzwwwhc5j5kmvg4sv6baxjpsfqh8ln7ilv4mgb1408rs7xmky"))))
   (build-system gnu-build-system)
   (inputs `(("curl" ,(nixpkgs-derivation* "curl"))
             ("emacs" ,(nixpkgs-derivation* "emacs"))
             ("check" ,(nixpkgs-derivation* "check"))
             ("bc" ,(nixpkgs-derivation* "bc"))))
   (description "GNU recutils, tools and libraries to access human-editable,
text-based databases")
   (long-description
    "GNU recutils is a set of tools and libraries to access human-editable,
text-based databases called recfiles.  The data is stored as a sequence of
records, each record containing an arbitrary number of named fields.")
   (license "GPLv3+")
   (home-page "http://www.gnu.org/software/recutils/")))

(define-public guile-1.8
  (package
   (name "guile")
   (version "1.8.8")
   (source (origin
            (method http-fetch)
            (uri (string-append "http://ftp.gnu.org/gnu/guile/guile-" version
                                ".tar.gz"))
            (sha256
             (base32
              "0l200a0v7h8bh0cwz6v7hc13ds39cgqsmfrks55b1rbj5vniyiy3"))))
   (build-system gnu-build-system)
   (arguments '(#:configure-flags '("--disable-error-on-warning")
                #:patches (list (assoc-ref %build-inputs "patch/snarf"))

                ;; Insert a phase before `configure' to patch things up.
                #:phases (alist-cons-before
                           'configure
                           'patch-loader-search-path
                           (lambda* (#:key outputs #:allow-other-keys)
                             ;; Add a call to `lt_dladdsearchdir' so that
                             ;; `libguile-readline.so' & co. are in the
                             ;; loader's search path.
                             (substitute* "libguile/dynl.c"
                                          (("lt_dlinit.*$" match)
                                           (format #f
                                                   "  ~a~%  lt_dladdsearchdir(\"~a/lib\");~%"
                                                   match
                                                   (assoc-ref outputs "out")))))
                           %standard-phases)))
   (inputs `(("patch/snarf" ,(search-patch "guile-1.8-cpp-4.5.patch"))
             ("gawk" ,gawk)
             ("readline" ,readline)))

   ;; Since `guile-1.8.pc' has "Libs: ... -lgmp -lltdl", these must be
   ;; propagated.
   (propagated-inputs `(("gmp" ,gmp)
                        ("libtool" ,libtool)))

   ;; When cross-compiling, a native version of Guile itself is needed.
   (self-native-input? #t)

   (description "GNU Guile 1.8, an embeddable Scheme interpreter")
   (long-description
"GNU Guile 1.8 is an interpreter for the Scheme programming language,
packaged as a library that can be embedded into programs to make them
extensible.  It supports many SRFIs.")
   (home-page "http://www.gnu.org/software/guile/")
   (license "LGPLv2+")))

(define-public libffi
  (let ((post-install-phase
         ;; Install headers in the right place.
         '(lambda* (#:key outputs #:allow-other-keys)
            (define out (assoc-ref outputs "out"))
            (mkdir (string-append out "/include"))
            (with-directory-excursion
                (string-append out "/lib/libffi-3.0.9/include")
              (for-each (lambda (h)
                          (format #t "moving `~a' to includedir~%" h)
                          (rename-file h (string-append out "/include/" h)))
                        (scandir "."
                                 (lambda (x)
                                   (not (member x '("." ".."))))))))))
   (package
    (name "libffi")
    (version "3.0.9")
    (source (origin
             (method http-fetch)
             (uri ;; FIXME: should be ftp://
              (string-append "http://sourceware.org/pub/libffi/"
                             name "-" version ".tar.gz"))
             (sha256
              (base32
               "0ln4jbpb6clcsdpb9niqk0frgx4k0xki96wiv067ig0q4cajb7aq"))))
    (build-system gnu-build-system)
    (arguments `(#:modules ((guix build utils) (guix build gnu-build-system)
                            (ice-9 ftw) (srfi srfi-26))
                 #:phases (alist-cons-after 'install 'post-install
                                            ,post-install-phase
                                            %standard-phases)))
    (description "libffi, a foreign function call interface library")
    (long-description
     "The libffi library provides a portable, high level programming interface
to various calling conventions.  This allows a programmer to call any
function specified by a call interface description at run-time.

FFI stands for Foreign Function Interface.  A foreign function interface is
the popular name for the interface that allows code written in one language
to call code written in another language.  The libffi library really only
provides the lowest, machine dependent layer of a fully featured foreign
function interface.  A layer must exist above libffi that handles type
conversions for values passed between the two languages.")
    (home-page "http://sources.redhat.com/libffi/")

    ;; See <http://github.com/atgreen/libffi/blob/master/LICENSE>.
    (license "free, non-copyleft"))))

(define-public guile-2.0
  (package
   (name "guile")
   (version "2.0.6")
   (source (origin
            (method http-fetch)
            (uri (string-append "http://ftp.gnu.org/gnu/guile/guile-" version
                                ".tar.xz"))
            (sha256
             (base32
              "000ng5qsq3cl1k35jvzvhwxj92wx4q87745n2fppkd4irh58vv5l"))))
   (build-system gnu-build-system)
   (native-inputs `(("xz" ,(nixpkgs-derivation* "xz"))
                    ("pkgconfig" ,(nixpkgs-derivation* "pkgconfig"))))
   (inputs `(("libffi" ,libffi)
             ("readline" ,readline)))

   (propagated-inputs
    `( ;; These ones aren't normally needed here, but since `libguile-2.0.la'
       ;; reads `-lltdl -lunistring', adding them here will add the needed
       ;; `-L' flags.  As for why the `.la' file lacks the `-L' flags, see
       ;; <http://thread.gmane.org/gmane.comp.lib.gnulib.bugs/18903>.
      ("libunistring" ,libunistring)
      ("libtool" ,libtool)

      ;; The headers and/or `guile-2.0.pc' refer to these packages, so they
      ;; must be propagated.
      ("bdw-gc" ,(nixpkgs-derivation* "boehmgc"))
      ("gmp" ,gmp)))

   (self-native-input? #t)

   (description "GNU Guile 2.0, an embeddable Scheme implementation")
   (long-description
"GNU Guile is an implementation of the Scheme programming language, with
support for many SRFIs, packaged for use in a wide variety of environments.
In addition to implementing the R5RS Scheme standard and a large subset of
R6RS, Guile includes a module system, full access to POSIX system calls,
networking support, multiple threads, dynamic linking, a foreign function
call interface, and powerful string processing.")
   (home-page "http://www.gnu.org/software/guile/")
   (license "LGPLv3+")))

(define-public linux-headers
  (let* ((version* "3.3.5")
         (build-phase
          '(lambda* (#:key outputs #:allow-other-keys)
             (setenv "ARCH" "x86_64")       ; XXX
             (and (zero? (system* "make" "defconfig"))
                  (zero? (system* "make" "mrproper" "headers_check")))))
         (install-phase
          `(lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (and (zero? (system* "make"
                                    (string-append "INSTALL_HDR_PATH=" out)
                                    "headers_install"))
                    (mkdir (string-append out "/include/config"))
                    (call-with-output-file
                        (string-append out
                                       "/include/config/kernel.release")
                      (lambda (p)
                        (format p "~a-default~%" ,version*))))))))
   (package
    (name "linux-headers")
    (version version*)
    (source (origin                               ; TODO: use Linux-Libre
             (method http-fetch)
             (uri (string-append
                   "http://www.kernel.org/pub/linux/kernel/v3.x/linux-"
                   version ".tar.xz"))
             (sha256
              (base32
               "0i74jn47f6vs5kcvk8abvz3k08z32c9bbqw0sdjkdxwvr4jbczpv"))))
    (build-system gnu-build-system)
    (native-inputs `(("perl" ,(nixpkgs-derivation* "perl"))))
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       #:phases (alist-replace
                 'build ,build-phase
                 (alist-replace
                  'install ,install-phase
                  (alist-delete 'configure %standard-phases)))
       #:tests? #f))
    (description "Linux kernel headers")
    (long-description "Headers of the Linux kernel.")
    (license "GPLv2")
    (home-page "http://kernel.org/"))))

(define-public glibc
  (package
   (name "glibc")
   (version "2.16.0")
   (source (origin
            (method http-fetch)
            (uri (string-append "http://ftp.gnu.org/gnu/glibc/glibc-"
                                version ".tar.xz"))
            (sha256
             (base32
              "092rdm49zh6l1pqkxbcpcaawgsgzxhpf1s7wf5wi5dvc5am3dp0y"))))
   (build-system gnu-build-system)
   (native-inputs `(("linux-headers" ,linux-headers)))
   (arguments
    `(#:modules ((guix build utils)
                 (guix build gnu-build-system)
                 (ice-9 regex))
      #:out-of-source? #t
      #:configure-flags
      (list "--enable-add-ons"
            "--sysconfdir=/etc"
            "--localedir=/var/run/current-system/sw/lib/locale" ; XXX
            (string-append "--with-headers="
                           (assoc-ref %build-inputs "linux-headers")
                           "/include")
            ;; To avoid linking with -lgcc_s (dynamic link) so the libc does
            ;; not depend on its compiler store path.
            "libc_cv_as_needed=no"

            ;; XXX: Work around "undefined reference to `__stack_chk_guard'".
            "libc_cv_ssp=no")
      #:tests? #f                                 ; XXX
      #:phases (alist-cons-before
                'configure 'pre-configure
                (lambda* (#:key outputs #:allow-other-keys)
                  (let ((out (assoc-ref outputs "out")))
                    ;; Use `pwd', not `/bin/pwd'.
                    (substitute* "configure"
                      (("^.*/bin/pwd.*$" line)
                       (regexp-substitute/global #f
                                                 "/bin/pwd"
                                                 (string-append line "\n")
                                                 'pre "pwd" 'post)))

                    ;; Install the rpc data base file under `$out/etc/rpc'.
                    (substitute* "sunrpc/Makefile"
                      (("^\\$\\(inst_sysconfdir\\)/rpc(.*)$" _ suffix)
                       (string-append out "/etc/rpc" suffix "\n"))
                      (("^install-others =.*$")
                       (string-append "install-others = " out "/etc/rpc\n")))))
                %standard-phases)))
   (description "The GNU C Library")
   (long-description
    "Any Unix-like operating system needs a C library: the library which
defines the \"system calls\" and other basic facilities such as open, malloc,
printf, exit...

The GNU C library is used as the C library in the GNU system and most systems
with the Linux kernel.")
   (license "LGPLv2+")
   (home-page "http://www.gnu.org/software/libc/")))

(define (guile-reader guile)
  "Build Guile-Reader against GUILE, a package of some version of Guile 1.8
or 2.0."
  (package
   (name (string-append "guile-reader-for-guile-" (package-version guile)))
   (version "0.6")
   (source  (origin
             (method http-fetch)
             (uri (string-append
                   "http://download-mirror.savannah.gnu.org/releases/guile-reader/guile-reader-"
                   version ".tar.gz"))
             (sha256
              (base32
               "1svlyk5pm4fsdp2g7n6qffdl6fdggxnlicj0jn9s4lxd63gzxy1n"))))
   (build-system gnu-build-system)
   (native-inputs `(("pkgconfig" ,(nixpkgs-derivation* "pkgconfig"))
                    ("gperf" ,(nixpkgs-derivation* "gperf"))))
   (inputs `(("guile" ,guile)))
   (description "Guile-Reader, a simple framework for building readers for
GNU Guile")
   (long-description
"Guile-Reader is a simple framework for building readers for GNU Guile.

The idea is to make it easy to build procedures that extend Guile’s read
procedure. Readers supporting various syntax variants can easily be written,
possibly by re-using existing “token readers” of a standard Scheme
readers. For example, it is used to implement Skribilo’s R5RS-derived
document syntax.

Guile-Reader’s approach is similar to Common Lisp’s “read table”, but
hopefully more powerful and flexible (for instance, one may instantiate as
many readers as needed).")
   (home-page "http://www.nongnu.org/guile-reader/")
   (license "GPLv3+")))

(define-public guile-reader/guile-1.8
  ;; Guile-Reader built against Guile 1.8.
  (guile-reader guile-1.8))

(define-public guile-reader/guile-2.0
  ;; Guile-Reader built against Guile 2.0.
  (guile-reader guile-2.0))

(define-public lout
  ;; This one is a bit tricky, because it doesn't follow the GNU Build System
  ;; rules.  Instead, it has a makefile that has to be patched to set the
  ;; prefix, etc., and it has no makefile rules to build its doc.
  (let ((configure-phase
         '(lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out"))
                  (doc (assoc-ref outputs "doc")))
              (substitute* "makefile"
                (("^PREFIX[[:blank:]]*=.*$")
                 (string-append "PREFIX = " out "\n"))
                (("^LOUTLIBDIR[[:blank:]]*=.*$")
                 (string-append "LOUTLIBDIR = " out "/lib/lout\n"))
                (("^LOUTDOCDIR[[:blank:]]*=.*$")
                 (string-append "LOUTDOCDIR = " doc "/doc/lout\n"))
                (("^MANDIR[[:blank:]]*=.*$")
                 (string-append "MANDIR = " out "/man\n")))
              (mkdir out)
              (mkdir (string-append out "/bin"))  ; TODO: use `mkdir-p'
              (mkdir (string-append out "/lib"))
              (mkdir (string-append out "/man"))
              (mkdir doc)
              (mkdir (string-append doc "/doc"))
              (mkdir (string-append doc "/doc/lout")))))
        (install-man-phase
         '(lambda* (#:key outputs #:allow-other-keys)
            (zero? (system* "make" "installman"))))
        (doc-phase
         '(lambda* (#:key outputs #:allow-other-keys)
            (define out
              (assoc-ref outputs "doc"))

            (setenv "PATH"
                    (string-append (assoc-ref outputs "out")
                                   "/bin:" (getenv "PATH")))
            (chdir "doc")
            (every (lambda (doc)
                     (format #t "doc: building `~a'...~%" doc)
                     (with-directory-excursion doc
                       (let ((file (string-append out "/doc/lout/"
                                                  doc ".ps")))
                         (and (or (file-exists? "outfile.ps")
                                  (zero? (system* "lout" "-r4" "-o"
                                                  "outfile.ps" "all")))
                              (begin
                                (copy-file "outfile.ps" file)
                                #t)
                              (zero? (system* "ps2pdf"
                                              "-dPDFSETTINGS=/prepress"
                                              "-sPAPERSIZE=a4"
                                              file
                                              (string-append out "/doc/lout/"
                                                             doc ".pdf")))))))
                   '("design" "expert" "slides" "user")))))
   (package
    (name "lout")
    (version "3.39")
    (source (origin
             (method http-fetch)
             ;; FIXME: `http-get' doesn't follow redirects, hence the URL.
             (uri (string-append
                   "http://download-mirror.savannah.gnu.org/releases/lout/lout-"
                   version ".tar.gz"))
             (sha256
              (base32
               "12gkyqrn0kaa8xq7sc7v3wm407pz2fxg9ngc75aybhi5z825b9vq"))))
    (build-system gnu-build-system)               ; actually, just a makefile
    (outputs '("out" "doc"))
    (inputs `(("ghostscript" ,(nixpkgs-derivation* "ghostscript"))))
    (arguments `(#:modules ((guix build utils)
                            (guix build gnu-build-system)
                            (srfi srfi-1))        ; we need SRFI-1
                 #:tests? #f                      ; no "check" target

                 ;; Customize the build phases.
                 #:phases (alist-replace
                           'configure ,configure-phase

                           (alist-cons-after
                            'install 'install-man-pages
                            ,install-man-phase

                            (alist-cons-after
                             'install 'install-doc
                             ,doc-phase
                             %standard-phases)))))
    (description "Lout, a document layout system similar in style to LaTeX")
    (long-description
"The Lout document formatting system is now reads a high-level description of
a document similar in style to LaTeX and produces a PostScript or plain text
output file.

Lout offers an unprecedented range of advanced features, including optimal
paragraph and page breaking, automatic hyphenation, PostScript EPS file
inclusion and generation, equation formatting, tables, diagrams, rotation and
scaling, sorted indexes, bibliographic databases, running headers and
odd-even pages, automatic cross referencing, multilingual documents including
hyphenation (most European languages are supported), formatting of computer
programs, and much more, all ready to use.  Furthermore, Lout is easily
extended with definitions which are very much easier to write than troff of
TeX macros because Lout is a high-level, purely functional language, the
outcome of an eight-year research project that went back to the
beginning.")
    (license "GPLv3+")
    (home-page "http://savannah.nongnu.org/projects/lout/"))))

;;; Local Variables:
;;; eval: (put 'lambda* 'scheme-indent-function 1)
;;; eval: (put 'substitute* 'scheme-indent-function 1)
;;; eval: (put 'with-directory-excursion 'scheme-indent-function 1)
;;; End:
