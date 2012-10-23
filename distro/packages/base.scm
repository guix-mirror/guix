;;; Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
;;; Copyright (C) 2012 Ludovic Courtès <ludo@gnu.org>
;;; Copyright (C) 2012 Nikita Karetnikov <nikita@karetnikov.org>
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

(define-module (distro packages base)
  #:use-module (distro)
  #:use-module (guix packages)
  #:use-module (guix ftp)
  #:use-module (guix http)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module ((guix store) #:select (add-to-store add-text-to-store))
  #:use-module ((guix derivations) #:select (derivation))
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match))

;;; Commentary:
;;;
;;; Base packages of the Guix-based GNU user-land software distribution.
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
   (inputs `(("patch/gets" ,(search-patch "tar-gets-undeclared.patch"))))
   (arguments
    `(#:patches (list (assoc-ref %build-inputs "patch/gets"))))
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

(define-public perl
  ;; Yeah, Perl...  It is required early in the bootstrap process by Linux.
  (package
    (name "perl")
    (version "5.16.1")
    (source (origin
             (method http-fetch)
             (uri (string-append "http://www.cpan.org/src/5.0/perl-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "15qxzba3a50c9nik5ydgyfp62x7h9vxxn12yd1jgl93hb1wj96km"))))
    (build-system gnu-build-system)
    (arguments
     (lambda (system)
       `(#:tests? #f
         #:patches (list (assoc-ref %build-inputs "patch/no-sys-dirs"))
         #:phases
         (alist-replace
          'configure
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((out  (assoc-ref outputs "out"))
                  (libc (assoc-ref inputs "libc"))
                  (pwd  (search-path (search-path-as-string->list
                                      (getenv "PATH"))
                                     "pwd")))
              ;; Use the right path for `pwd'.
              (substitute* "dist/Cwd/Cwd.pm"
                (("/bin/pwd") pwd))

              (zero?
               (system* "/bin/sh" "./Configure"
                        (string-append "-Dprefix=" out)
                        (string-append "-Dman1dir=" out "/share/man/man1")
                        (string-append "-Dman3dir=" out "/share/man/man3")
                        "-de" "-Dcc=gcc"
                        "-Uinstallusrbinperl"
                        "-Dinstallstyle=lib/perl5"
                        "-Duseshrplib"
                        (string-append "-Dlocincpth=" libc "/include")
                        (string-append "-Dloclibpth=" libc "/lib")))))
          %standard-phases))))
    (inputs `(("patch/no-sys-dirs" ,(search-patch "perl-no-sys-dirs.patch"))))
    (description "Implementation of the Perl programming language")
    (long-description
     "Perl 5 is a highly capable, feature-rich programming language with over
24 years of development.")
    (home-page "http://www.perl.org/")
    (license "GPLv1+")))                          ; or "Artistic"

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

(define-public bzip2
  (let ((fix-man-dir
         ;; Move man pages to $out/share/.
         '(lambda* (#:key outputs #:allow-other-keys)
            (with-directory-excursion (assoc-ref outputs "out")
              (mkdir "share")
              (rename-file "man" "share"))))
        (build-shared-lib
         ;; Build a shared library.
         '(lambda* (#:key inputs #:allow-other-keys)
            (zero? (system* "make" "-f" "Makefile-libbz2_so"))))
        (install-shared-lib
         '(lambda* (#:key outputs #:allow-other-keys)
            (let* ((out    (assoc-ref outputs "out"))
                   (libdir (string-append out "/lib")))
              (for-each (lambda (file)
                          (let ((base (basename file)))
                            (format #t "installing `~a' to `~a'~%"
                                    base libdir)
                            (copy-file file
                                       (string-append libdir "/" base))))
                        (find-files "." "^libbz2\\.so"))))))
    (package
      (name "bzip2")
      (version "1.0.6")
      (source (origin
               (method http-fetch)
               (uri (string-append "http://www.bzip.org/" version "/bzip2-"
                                   version ".tar.gz"))
               (sha256
                (base32
                 "1kfrc7f0ja9fdn6j1y6yir6li818npy6217hvr3wzmnmzhs8z152"))))
      (build-system gnu-build-system)
      (arguments
       `(#:modules ((guix build gnu-build-system)
                    (guix build utils)
                    (srfi srfi-1))
         #:phases
         (alist-cons-before
          'build 'build-shared-lib ,build-shared-lib
          (alist-cons-after
           'install 'fix-man-dir ,fix-man-dir
           (alist-cons-after
            'install 'install-shared-lib ,install-shared-lib
            (alist-delete 'configure %standard-phases))))
         #:make-flags (list (string-append "PREFIX="
                                           (assoc-ref %outputs "out")))))
      (description "high-quality data compression program")
      (long-description
       "bzip2 is a freely available, patent free (see below), high-quality data
compressor.  It typically compresses files to within 10% to 15% of the best
available techniques (the PPM family of statistical compressors), whilst
being around twice as fast at compression and six times faster at
decompression.")
      (license "BSD-style")
      (home-page "http://www.bzip.org/"))))

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
   (inputs `(("patch/gets"
              ,(search-patch "diffutils-gets-undeclared.patch"))))
   (arguments `(#:patches (list (assoc-ref %build-inputs "patch/gets"))))
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
   (version "1.0")
   (source (origin
            (method http-fetch)
            (uri (string-append
                  "http://www.multiprecision.org/mpc/download/mpc-"
                  version ".tar.gz"))
            (sha256 (base32
                     "00rxjmkpqnv6zzcyw9aa5w6rzaav32ys87km25zgfcv9i32km5cw"))))
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

(define (glibc-dynamic-linker system)
  "Return the name of Glibc's dynamic linker for SYSTEM."
  (cond ((string=? system "x86_64-linux") "/lib/ld-linux-x86-64.so.2")
        ((string=? system "i686-linux") "/lib/ld-linux.so.2")
        (else (error "dynamic linker name not known for this system" system))))

(define-public gcc-4.7
  (let ((stripped? #t))                         ; TODO: make this a parameter
    (package
     (name "gcc")
     (version "4.7.2")
     (source (origin
              (method http-fetch)
              (uri (string-append "http://ftp.gnu.org/gnu/gcc/gcc-"
                                  version "/gcc-" version ".tar.bz2"))
              (sha256
               (base32
                "115h03hil99ljig8lkrq4qk426awmzh0g99wrrggxf8g07bq74la"))))
     (build-system gnu-build-system)
     (inputs `(("gmp" ,gmp)
               ("mpfr" ,mpfr)
               ("mpc" ,mpc)))           ; TODO: libelf, ppl, cloog, zlib, etc.
     (arguments
      (lambda (system)
        `(#:out-of-source? #t
          #:strip-binaries? ,stripped?
          #:configure-flags
          `("--enable-plugin"
            "--enable-languages=c,c++"
            "--disable-multilib"
            ,(let ((libc (assoc-ref %build-inputs "libc")))
               (if libc
                   (string-append "--with-native-system-header-dir=" libc
                                  "/include")
                   "--without-headers")))
          #:make-flags
          (let ((libc (assoc-ref %build-inputs "libc")))
            `(,@(if libc
                    (list (string-append "LDFLAGS_FOR_BUILD="
                                         "-L" libc "/lib "
                                         "-Wl,-dynamic-linker "
                                         "-Wl," libc
                                         ,(glibc-dynamic-linker system)))
                    '())
              ,(string-append "BOOT_CFLAGS=-O2 "
                              ,(if stripped? "-g0" "-g"))))

          #:tests? #f
          #:phases
          (alist-cons-before
           'configure 'pre-configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out  (assoc-ref outputs "out"))
                   (libc (assoc-ref inputs "libc")))
               (when libc
                 ;; The following is not performed for `--without-headers'
                 ;; cross-compiler builds.

                 ;; Fix the dynamic linker's file name.
                 (substitute* "gcc/config/i386/linux64.h"
                   (("#define GLIBC_DYNAMIC_LINKER([^ ]*).*$" _ suffix)
                    (format #f "#define GLIBC_DYNAMIC_LINKER~a \"~a\"~%"
                            suffix
                            (string-append libc ,(glibc-dynamic-linker system)))))

                 ;; Tell where to find libstdc++, libc, and `?crt*.o', except
                 ;; `crt{begin,end}.o', which come with GCC.
                 (substitute* ("gcc/config/gnu-user.h"
                               "gcc/config/i386/gnu-user.h"
                               "gcc/config/i386/gnu-user64.h")
                   (("#define LIB_SPEC (.*)$" _ suffix)
                    (format #f "#define LIB_SPEC \"-L~a/lib -rpath=~a/lib \
-rpath=~a/lib64 -rpath=~a/lib \" ~a~%"
                            libc libc out out suffix))
                   (("#define STARTFILE_SPEC.*$" line)
                    (format #f "#define STANDARD_STARTFILE_PREFIX_1 \"~a/lib\"
#define STANDARD_STARTFILE_PREFIX_2 \"\"
~a~%"
                            libc line))))

               ;; Don't retain a dependency on the build-time sed.
               (substitute* "fixincludes/fixincl.x"
                 (("static char const sed_cmd_z\\[\\] =.*;")
                  "static char const sed_cmd_z[] = \"sed\";"))))

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
                           %standard-phases))))))

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
  ;; FIXME: `ncurses-config' retains a ref on bash
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
                #:patch-flags '("-p0")
                #:configure-flags
                (list (string-append "LDFLAGS=-Wl,-rpath -Wl,"
                                     (assoc-ref %build-inputs "ncurses")
                                     "/lib"))))
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
     (inputs `(("readline" ,readline)
               ("ncurses" ,ncurses)))             ; TODO: add texinfo
     (arguments
      `(#:configure-flags `("--with-installed-readline"
                            ,,(string-append "CPPFLAGS=" cppflags)
                            ,(string-append
                              "LDFLAGS=-Wl,-rpath -Wl,"
                              (assoc-ref %build-inputs "readline")
                              "/lib"
                              " -Wl,-rpath -Wl,"
                              (assoc-ref %build-inputs "ncurses")
                              "/lib"))

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
                    ("perl" ,perl)))
   (arguments
    ;; TODO: Use `TESTSUITEFLAGS=-jN' for tests.
    `(#:patches (list (assoc-ref %build-inputs "patch/skip-tests"))))
   (inputs `(("patch/skip-tests"
              ,(search-patch "libtool-skip-tests.patch"))))
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
             (method ftp-fetch)
             (uri
              (string-append "ftp://sourceware.org/pub/libffi/"
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

(define-public pkg-config
  (package
   (name "pkg-config")
   (version "0.27.1")
   (source (origin
            (method http-fetch)
            (uri (string-append
                  "http://pkgconfig.freedesktop.org/releases/pkg-config-"
                  version ".tar.gz"))
            (sha256
             (base32
              "05wc5nwkqz7saj2v33ydmz1y6jdg659dll4jjh91n41m63gx0qsg"))))
   (build-system gnu-build-system)
   (arguments `(#:configure-flags '("--with-internal-glib")))
   (home-page "http://www.freedesktop.org/wiki/Software/pkg-config")
   (license "GPLv2+")
   (description "a helper tool used when compiling applications and
libraries")
   (long-description
    "pkg-config is a helper tool used when compiling applications and
libraries.  It helps you insert the correct compiler options on the
command line so an application can use gcc -o test test.c `pkg-config
--libs --cflags glib-2.0` for instance, rather than hard-coding values
on where to find glib (or other libraries). It is language-agnostic, so
it can be used for defining the location of documentation tools, for
instance.")))

(define-public libgc
  (package
   (name "libgc")
   (version "7.2alpha6")
   (source (origin
            (method http-fetch)
            (uri (string-append
                  "http://www.hpl.hp.com/personal/Hans_Boehm/gc/gc_source/gc-"
                  version ".tar.gz"))
            (sha256
             (base32
              "05jwadjbrv8pr7z9cb4miskicxqpxm0pca4h2rg5cgbpajr2bx7b"))))
   (build-system gnu-build-system)
   (description "The Boehm-Demers-Weiser conservative garbage collector
for C and C++")
   (long-description
    "The Boehm-Demers-Weiser conservative garbage collector can be used
as a garbage collecting replacement for C malloc or C++ new.  It allows
you to allocate memory basically as you normally would, without
explicitly deallocating memory that is no longer useful.  The collector
automatically recycles memory when it determines that it can no longer
be otherwise accessed.

The collector is also used by a number of programming language
implementations that either use C as intermediate code, want to
facilitate easier interoperation with C libraries, or just prefer the
simple collector interface.

Alternatively, the garbage collector may be used as a leak detector for
C or C++ programs, though that is not its primary goal.")
   (home-page "http://www.hpl.hp.com/personal/Hans_Boehm/gc/")

   ;; permissive X11-style license:
   ;; http://www.hpl.hp.com/personal/Hans_Boehm/gc/license.txt
   (license "X11")))

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
   (native-inputs `(("pkgconfig" ,pkg-config)))
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
      ("bdw-gc" ,libgc)
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

(define-public linux-libre-headers
  (let* ((version* "3.3.8")
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
    (name "linux-libre-headers")
    (version version*)
    (source (origin
             (method http-fetch)
             (uri (string-append
                   "http://linux-libre.fsfla.org/pub/linux-libre/releases/3.3.8-gnu/linux-libre-"
                   version "-gnu.tar.xz"))
             (sha256
              (base32
               "0jkfh0z1s6izvdnc3njm39dhzp1cg8i06jv06izwqz9w9qsprvnl"))))
    (build-system gnu-build-system)
    (native-inputs `(("perl" ,perl)))
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
    (description "GNU Linux-Libre kernel headers")
    (long-description "Headers of the Linux-Libre kernel.")
    (license "GPLv2")
    (home-page "http://www.gnu.org/software/linux-libre/"))))

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

   ;; Glibc's <limits.h> refers to <linux/limit.h>, for instance, so glibc
   ;; users should automatically pull Linux headers as well.
   (propagated-inputs `(("linux-headers" ,linux-libre-headers)))

   (arguments
    `(#:out-of-source? #t
      #:configure-flags
      (list "--enable-add-ons"
            "--sysconfdir=/etc"
            "--localedir=/var/run/current-system/sw/lib/locale" ; XXX
            (string-append "--with-headers="
                           (assoc-ref %build-inputs "linux-headers")
                           "/include")

            ;; The default is to assume a 2.4 Linux interface, but we'll
            ;; always use something newer.  See "kernel-features.h" in the
            ;; GNU libc for details.
            "--enable-kernel=2.6.30"

            ;; XXX: Work around "undefined reference to `__stack_chk_guard'".
            "libc_cv_ssp=no")
      #:tests? #f                                 ; XXX
      #:phases (alist-cons-before
                'configure 'pre-configure
                (lambda* (#:key outputs #:allow-other-keys)
                  (let ((out (assoc-ref outputs "out")))
                    ;; Use `pwd', not `/bin/pwd'.
                    (substitute* "configure"
                      (("/bin/pwd") "pwd"))

                    ;; Install the rpc data base file under `$out/etc/rpc'.
                    ;; FIXME: Use installFlags = [ "sysconfdir=$(out)/etc" ];
                    (substitute* "sunrpc/Makefile"
                      (("^\\$\\(inst_sysconfdir\\)/rpc(.*)$" _ suffix)
                       (string-append out "/etc/rpc" suffix "\n"))
                      (("^install-others =.*$")
                       (string-append "install-others = " out "/etc/rpc\n")))

                    (substitute* "Makeconfig"
                      ;; According to
                      ;; <http://www.linuxfromscratch.org/lfs/view/stable/chapter05/glibc.html>,
                      ;; linking against libgcc_s is not needed with GCC
                      ;; 4.7.1.
                      ((" -lgcc_s") ""))))
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


;;;
;;; Bootstrap packages.
;;;

(define %bootstrap-inputs
  (compile-time-value
   `(("libc" ,(nixpkgs-derivation "glibc"))
     ,@(map (lambda (name)
              (list name (nixpkgs-derivation name)))
            '("gnutar" "gzip" "bzip2" "xz" "patch"
              "coreutils" "gnused" "gnugrep" "bash"
              "gawk"                                ; used by `config.status'
              "gcc" "binutils")))))

(define %bootstrap-guile
  ;; The Guile used to run the build scripts of the initial derivations.
  ;; It is just unpacked from a tarball containing a pre-built binary.
  ;; This is typically built using %GUILE-BOOTSTRAP-TARBALL below.
  ;;
  ;; XXX: Would need libc's `libnss_files2.so' for proper `getaddrinfo'
  ;; support (for /etc/services).
  (let ((raw (build-system
              (name "raw")
              (description "Raw build system with direct store access")
              (build (lambda* (store name source inputs #:key outputs system)
                       (define (->store file)
                         (add-to-store store file #t #t "sha256"
                                       (or (search-bootstrap-binary file
                                                                    system)
                                           (error "bootstrap binary not found"
                                                  file system))))

                       (let* ((tar   (->store "tar"))
                              (xz    (->store "xz"))
                              (mkdir (->store "mkdir"))
                              (bash  (->store "bash"))
                              (guile (->store "guile-bootstrap-2.0.6.tar.xz"))
                              (builder
                               (add-text-to-store store
                                                  "build-bootstrap-guile.sh"
                                                  (format #f "
echo \"unpacking bootstrap Guile to '$out'...\"
~a $out
cd $out
~a -dc < ~a | ~a xv

# Sanity check.
$out/bin/guile --version~%"
                                                          mkdir xz guile tar)
                                                  (list mkdir xz guile tar))))
                         (derivation store name system
                                     bash `(,builder) '()
                                     `((,bash) (,builder)))))))))
   (package
     (name "guile-bootstrap")
     (version "2.0")
     (source #f)
     (build-system raw)
     (description "Bootstrap Guile")
     (long-description "Pre-built Guile for bootstrapping purposes.")
     (home-page #f)
     (license "LGPLv3+"))))

(define (bootstrap-origin source)
  "Return a variant of SOURCE, an <origin> instance, whose method uses
%BOOTSTRAP-GUILE to do its job."
  (define (boot fetch)
    (lambda* (store url hash-algo hash #:optional name)
      (fetch store url hash-algo hash
             #:guile %bootstrap-guile)))

  (let ((orig-method (origin-method source)))
    (origin (inherit source)
      (method (cond ((eq? orig-method http-fetch)
                     (boot http-fetch))
                    ((eq? orig-method ftp-fetch)
                     (boot ftp-fetch))
                    (else orig-method))))))

(define (package-from-tarball name* source* program-to-test description*)
  "Return a package that correspond to the extraction of SOURCE*.
PROGRAM-TO-TEST is a program to run after extraction of SOURCE*, to
check whether everything is alright."
  (package
    (name name*)
    (version "0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     `(#:guile ,%bootstrap-guile
       #:modules ((guix build utils))
       #:builder
       (let ((out     (assoc-ref %outputs "out"))
             (tar     (assoc-ref %build-inputs "tar"))
             (xz      (assoc-ref %build-inputs "xz"))
             (tarball (assoc-ref %build-inputs "tarball")))
         (use-modules (guix build utils))

         (mkdir out)
         (copy-file tarball "binaries.tar.xz")
         (system* xz "-d" "binaries.tar.xz")
         (let ((builddir (getcwd)))
           (with-directory-excursion out
             (and (zero? (system* tar "xvf"
                                  (string-append builddir "/binaries.tar")))
                  (zero? (system* (string-append "bin/" ,program-to-test)
                                  "--version"))))))))
    (inputs
     `(("tar" ,(lambda (system)
                 (search-bootstrap-binary "tar" system)))
       ("xz"  ,(lambda (system)
                 (search-bootstrap-binary "xz" system)))
       ("tarball" ,(lambda (system)
                     (bootstrap-origin (source* system))))))
    (description description*)
    (long-description #f)
    (home-page #f)))

(define %bootstrap-base-url
  ;; This is where the initial binaries come from.
  "http://www.fdn.fr/~lcourtes/software/guix/packages")

(define %bootstrap-coreutils&co
  (package-from-tarball "bootstrap-binaries"
                        (lambda (system)
                          (origin
                           (method http-fetch)
                           (uri (string-append
                                 %bootstrap-base-url "/"
                                 system "/static-binaries.tar.xz"))
                           (sha256
                            (base32
                             "0bvhkzahjgf6w5i3db5bjgq8kqm6xdr23lig0s1p8fgdqbfp0bzm"))))
                        "true"                    ; the program to test
                        "Bootstrap binaries of Coreutils, Awk, etc."))

(define %bootstrap-binutils
  (package-from-tarball "binutils-bootstrap"
                        (lambda (system)
                          (origin
                           (method http-fetch)
                           (uri (string-append
                                 %bootstrap-base-url "/"
                                 system "/binutils-static-2.22.tar.xz"))
                           (sha256
                            (base32
                             "1cz1rwqhswgrr14kzbkaj3k32kzgv2b6mmzvc6ssbbz8k2m8jmqa"))))
                        "ld"                      ; the program to test
                        "Bootstrap binaries of the GNU Binutils"))

(define %bootstrap-glibc
  ;; The initial libc.
  (package
    (name "glibc-bootstrap")
    (version "0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     `(#:guile ,%bootstrap-guile
       #:modules ((guix build utils))
       #:builder
       (let ((out     (assoc-ref %outputs "out"))
             (tar     (assoc-ref %build-inputs "tar"))
             (xz      (assoc-ref %build-inputs "xz"))
             (tarball (assoc-ref %build-inputs "tarball")))
         (use-modules (guix build utils))

         (mkdir out)
         (copy-file tarball "binaries.tar.xz")
         (system* xz "-d" "binaries.tar.xz")
         (let ((builddir (getcwd)))
           (with-directory-excursion out
             (system* tar "xvf"
                      (string-append builddir
                                     "/binaries.tar"))
             (chmod "lib" #o755)

             ;; Patch libc.so so it refers to the right path.
             (substitute* "lib/libc.so"
               (("/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-glibc([^/]+)/")
                (string-append "/" (basename out) "/"))))))))
    (inputs
     `(("tar" ,(lambda (system)
                 (search-bootstrap-binary "tar" system)))
       ("xz"  ,(lambda (system)
                 (search-bootstrap-binary "xz" system)))
       ("tarball" ,(lambda (system)
                     (bootstrap-origin
                      (origin
                       (method http-fetch)
                       (uri (string-append %bootstrap-base-url "/"
                                           system "/glibc-2.16.0.tar.xz"))
                       (sha256
                        (base32 "1qbqmzjxsda74qmzl2rb7049iajary7lvznbym8m2xvqvyid8r2l"))))))))
    (description "Bootstrap binaries and headers of the GNU C Library")
    (long-description #f)
    (home-page #f)))

(define package-with-bootstrap-guile
  (memoize
   (lambda (p)
    "Return a variant of P such that all its origins are fetched with
%BOOTSTRAP-GUILE."
    (define rewritten-input
      (match-lambda
       ((name (? origin? o))
        `(,name ,(bootstrap-origin o)))
       ((name (? package? p) sub-drvs ...)
        `(,name ,(package-with-bootstrap-guile p) ,@sub-drvs))
       (x x)))

    (package (inherit p)
      (source (match (package-source p)
                ((? origin? o) (bootstrap-origin o))
                (s s)))
      (inputs (map rewritten-input
                   (package-inputs p)))
      (native-inputs (map rewritten-input
                          (package-native-inputs p)))
      (propagated-inputs (map rewritten-input
                              (package-propagated-inputs p)))))))

(define (default-keyword-arguments args defaults)
  "Return ARGS augmented with any keyword/value from DEFAULTS for
keywords not already present in ARGS."
  (let loop ((defaults defaults)
             (args     args))
    (match defaults
      ((kw value rest ...)
       (loop rest
             (if (assoc-ref kw args)
                 args
                 (cons* kw value args))))
      (()
       args))))

(define-syntax substitute-keyword-arguments
  (syntax-rules ()
    "Return a new list of arguments where the value for keyword arg KW is
replaced by EXP.  EXP is evaluated in a context where VAR is boud to the
previous value of the keyword argument."
    ((_ original-args ((kw var) exp) ...)
     (let loop ((args    original-args)
                (before '()))
       (match args
         ((kw var rest (... ...))
          (loop rest (cons* exp kw before)))
         ...
         ((x rest (... ...))
          (loop rest (cons x before)))
         (()
          (reverse before)))))))

(define gnu-make-boot0
  (package-with-bootstrap-guile
   (package (inherit gnu-make)
     (name "make-boot0")
     (location (source-properties->location (current-source-location)))
     (arguments `(#:guile ,%bootstrap-guile
                  #:implicit-inputs? #f
                  #:tests? #f                  ; cannot run "make check"
                  #:phases
                  (alist-replace
                   'build (lambda _
                            (zero? (system* "./build.sh")))
                   (alist-replace
                    'install (lambda* (#:key outputs #:allow-other-keys)
                               (let* ((out (assoc-ref outputs "out"))
                                      (bin (string-append out "/bin")))
                                 (mkdir-p bin)
                                 (copy-file "make"
                                            (string-append bin "/make"))))
                    %standard-phases))))
     (inputs %bootstrap-inputs))))

(define diffutils-boot0
  (package-with-bootstrap-guile
   (let ((p (package-with-explicit-inputs diffutils
                                          `(("make" ,gnu-make-boot0)
                                            ,@%bootstrap-inputs)
                                          #:guile %bootstrap-guile)))
     (package (inherit p)
       (location (source-properties->location (current-source-location)))
       (arguments `(#:tests? #f         ; the test suite needs diffutils
                    ,@(package-arguments p)))))))

(define findutils-boot0
  (package-with-bootstrap-guile
   (package-with-explicit-inputs findutils
                                 `(("make" ,gnu-make-boot0)
                                   ("diffutils" ,diffutils-boot0) ; for tests
                                   ,@%bootstrap-inputs)
                                 (current-source-location)
                                 #:guile %bootstrap-guile)))


(define %boot0-inputs
  `(("make" ,gnu-make-boot0)
    ("diffutils" ,diffutils-boot0)
    ("findutils" ,findutils-boot0)
    ,@%bootstrap-inputs))

(define* (nix-system->gnu-triplet system #:optional (vendor "unknown"))
  "Return an a guess of the GNU triplet corresponding to Nix system
identifier SYSTEM."
  (let* ((dash (string-index system #\-))
         (arch (substring system 0 dash))
         (os   (substring system (+ 1 dash))))
    (string-append arch
                   "-" vendor "-"
                   (if (string=? os "linux")
                       "linux-gnu"
                       os))))

(define boot-triplet
  ;; Return the triplet used to create the cross toolchain needed in the
  ;; first bootstrapping stage.
  (cut nix-system->gnu-triplet <> "guix"))

;; Following Linux From Scratch, build a cross-toolchain in stage 0.  That
;; toolchain actually targets the same OS and arch, but it has the advantage
;; of being independent of the libc and tools in %BOOTSTRAP-INPUTS, since
;; GCC-BOOT0 (below) is built without any reference to the target libc.

(define binutils-boot0
  (package-with-bootstrap-guile
   (package (inherit binutils)
     (name "binutils-cross-boot0")
     (arguments
      (lambda (system)
        `(#:guile ,%bootstrap-guile
          #:implicit-inputs? #f
          ,@(substitute-keyword-arguments (package-arguments binutils)
              ((#:configure-flags cf)
               `(list ,(string-append "--target=" (boot-triplet system))))))))
     (inputs %boot0-inputs))))

(define gcc-boot0
  (package-with-bootstrap-guile
   (package (inherit gcc-4.7)
     (name "gcc-cross-boot0")
     (arguments
      (lambda (system)
        `(#:guile ,%bootstrap-guile
          #:implicit-inputs? #f
          #:modules ((guix build gnu-build-system)
                     (guix build utils)
                     (ice-9 regex)
                     (srfi srfi-1)
                     (srfi srfi-26))
          ,@(substitute-keyword-arguments ((package-arguments gcc-4.7) system)
              ((#:configure-flags flags)
               `(append (list ,(string-append "--target="
                                              (boot-triplet system))

                              ;; No libc yet.
                              "--without-headers"

                              ;; Disable features not needed at this stage.
                              "--disable-shared"
                              "--enable-languages=c"
                              "--disable-libmudflap"
                              "--disable-libgomp"
                              "--disable-libssp"
                              "--disable-libquadmath"
                              "--disable-decimal-float")
                        (remove (cut string-match "--enable-languages.*" <>)
                                ,flags)))
              ((#:phases phases)
               `(alist-cons-after
                 'unpack 'unpack-gmp&co
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let ((gmp  (assoc-ref %build-inputs "gmp-source"))
                         (mpfr (assoc-ref %build-inputs "mpfr-source"))
                         (mpc  (assoc-ref %build-inputs "mpc-source")))

                     ;; To reduce the set of pre-built bootstrap inputs, build
                     ;; GMP & co. from GCC.
                     (for-each (lambda (source)
                                 (or (zero? (system* "tar" "xvf" source))
                                     (error "failed to unpack tarball"
                                            source)))
                               (list gmp mpfr mpc))

                     ;; Create symlinks like `gmp' -> `gmp-5.0.5'.
                     ,@(map (lambda (lib)
                              `(symlink ,(package-full-name lib)
                                        ,(package-name lib)))
                            (list gmp mpfr mpc))

                     ;; MPFR headers/lib are found under $(MPFR)/src, but
                     ;; `configure' wrongfully tells MPC too look under
                     ;; $(MPFR), so fix that.
                     (substitute* "configure"
                       (("extra_mpc_mpfr_configure_flags(.+)--with-mpfr-include=([^/]+)/mpfr(.*)--with-mpfr-lib=([^ ]+)/mpfr"
                         _ equals include middle lib)
                        (string-append "extra_mpc_mpfr_configure_flags" equals
                                       "--with-mpfr-include=" include
                                       "/mpfr/src" middle
                                       "--with-mpfr-lib=" lib
                                       "/mpfr/src"))
                       (("gmpinc='-I([^ ]+)/mpfr -I([^ ]+)/mpfr" _ a b)
                        (string-append "gmpinc='-I" a "/mpfr/src "
                                       "-I" b "/mpfr/src"))
                       (("gmplibs='-L([^ ]+)/mpfr" _ a)
                        (string-append "gmplibs='-L" a "/mpfr/src")))))
                 (alist-cons-after
                  'install 'symlink-libgcc_eh
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let ((out (assoc-ref outputs "out")))
                      ;; Glibc wants to link against libgcc_eh, so provide
                      ;; it.
                      (with-directory-excursion
                          (string-append out "/lib/gcc/"
                                         ,(boot-triplet system)
                                         "/" ,(package-version gcc-4.7))
                        (symlink "libgcc.a" "libgcc_eh.a"))))
                  ,phases)))))))

     (inputs `(("gmp-source" ,(package-source gmp))
               ("mpfr-source" ,(package-source mpfr))
               ("mpc-source" ,(package-source mpc))
               ("binutils-cross" ,binutils-boot0)

               ;; Call it differently so that the builder can check whether
               ;; the "libc" input is #f.
               ("libc-native" ,@(assoc-ref %boot0-inputs "libc"))
               ,@(alist-delete "libc" %boot0-inputs))))))

(define linux-libre-headers-boot0
  (package-with-bootstrap-guile
   (package (inherit linux-libre-headers)
     (arguments `(#:guile ,%bootstrap-guile
                  #:implicit-inputs? #f
                  ,@(package-arguments linux-libre-headers)))
     (native-inputs
      (let ((perl (package-with-explicit-inputs perl
                                                %boot0-inputs
                                                (current-source-location)
                                                #:guile %bootstrap-guile)))
        `(("perl" ,perl)
          ,@%boot0-inputs))))))

(define %boot1-inputs
  ;; 2nd stage inputs.
  `(("gcc" ,gcc-boot0)
    ("binutils-cross" ,binutils-boot0)

    ;; Keep "binutils" here because the cross-gcc invokes `as', not the
    ;; cross-`as'.
    ,@%boot0-inputs))

(define-public glibc-final
  ;; The final libc, "cross-built".  If everything went well, the resulting
  ;; store path has no dependencies.
  (package-with-bootstrap-guile
   (package (inherit glibc)
     (arguments
      (lambda (system)
        `(#:guile ,%bootstrap-guile
          #:implicit-inputs? #f

          ;; Leave /bin/sh as the interpreter for `ldd', `sotruss', etc. to
          ;; avoid keeping a reference to the bootstrap Bash.
          #:patch-shebangs? #f
          ,@(substitute-keyword-arguments (package-arguments glibc)
              ((#:configure-flags flags)
               `(append (list ,(string-append "--host=" (boot-triplet system))
                              ,(string-append "--build="
                                              (nix-system->gnu-triplet system))
                              "BASH_SHELL=/bin/sh"

                              ;; cross-rpcgen fails to build, because it gets
                              ;; built with the cross-compiler instead of the
                              ;; native compiler.  See also
                              ;; <http://sourceware.org/ml/libc-alpha/2012-03/msg00325.html>.
                              "--disable-obsolete-rpc")
                        ,flags))))))
     (propagated-inputs `(("linux-headers" ,linux-libre-headers-boot0)))
     (inputs `( ;; A native GCC is needed to build `cross-rpcgen'.
               ("native-gcc" ,@(assoc-ref %boot0-inputs "gcc"))
               ,@%boot1-inputs)))))

(define gcc-boot0-wrapped
  ;; Make the cross-tools GCC-BOOT0 and BINUTILS-BOOT0 available under the
  ;; non-cross names.
  (package (inherit gcc-4.7)
    (name (string-append (package-name gcc-boot0) "-wrapped"))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (lambda (system)
      `(#:guile ,%bootstrap-guile
        #:modules ((guix build utils))
        #:builder (begin
                    (use-modules (guix build utils))

                    (let* ((binutils (assoc-ref %build-inputs "binutils"))
                           (gcc      (assoc-ref %build-inputs "gcc"))
                           (libc     (assoc-ref %build-inputs "libc"))
                           (out      (assoc-ref %outputs "out"))
                           (bindir   (string-append out "/bin"))
                           (triplet  ,(boot-triplet system)))
                      (mkdir-p bindir)
                      (with-directory-excursion bindir
                        (for-each (lambda (tool)
                                    (symlink (string-append binutils "/bin/"
                                                            triplet "-" tool)
                                             tool))
                                  '("ar" "ranlib"))

                        ;; GCC-BOOT0 is a libc-less cross-compiler, so it
                        ;; needs to be told where to find the crt files and
                        ;; the dynamic linker.
                        (call-with-output-file "gcc"
                          (lambda (p)
                            (format p "#!/bin/sh
exec ~a/bin/~a-gcc -B~a/lib -Wl,-dynamic-linker -Wl,~a/~a \"$@\"~%"
                                    gcc triplet
                                    libc libc
                                    ,(glibc-dynamic-linker system))))

                        (chmod "gcc" #o555)))))))
    (native-inputs
     `(("binutils" ,binutils-boot0)
       ("gcc" ,gcc-boot0)
       ("libc" ,glibc-final)))
    (inputs '())))

(define %boot2-inputs
  ;; 3rd stage inputs.
  `(("libc" ,glibc-final)
    ("gcc" ,gcc-boot0-wrapped)
    ,@(fold alist-delete %boot1-inputs '("libc" "gcc"))))

(define binutils-final
  (package (inherit binutils)
    (arguments
     (lambda (system)
       `(#:guile ,%bootstrap-guile
         #:implicit-inputs? #f
         ,@(package-arguments binutils))))
    (inputs %boot2-inputs)))

(define-public gcc-final
  ;; The final GCC.
  (package (inherit gcc-boot0)
    (name "gcc")
    (arguments
     (lambda (system)
       `(#:guile ,%bootstrap-guile
         #:implicit-inputs? #f

         ;; Build again GMP & co. within GCC's build process, because it's hard
         ;; to do outside (because GCC-BOOT0 is a cross-compiler, and thus
         ;; doesn't honor $LIBRARY_PATH, which breaks `gnu-build-system'.)
         ,@(substitute-keyword-arguments ((package-arguments gcc-boot0) system)
             ((#:configure-flags boot-flags)
              (let loop ((args ((package-arguments gcc-4.7) system)))
                (match args
                  ((#:configure-flags normal-flags _ ...)
                   normal-flags)
                  ((_ rest ...)
                   (loop rest)))))
             ((#:phases phases)
              `(alist-delete 'symlink-libgcc_eh ,phases))))))

    (inputs `(("gmp-source" ,(package-source gmp))
              ("mpfr-source" ,(package-source mpfr))
              ("mpc-source" ,(package-source mpc))
              ("binutils" ,binutils-final)
              ,@%boot2-inputs))))

(define ld-wrapper-boot3
  ;; A linker wrapper that uses the bootstrap Guile.
  (package
    (name "ld-wrapper-boot3")
    (version "0")
    (source #f)
    (build-system trivial-build-system)
    (inputs `(("binutils" ,binutils-final)
              ("guile"   ,%bootstrap-guile)
              ("wrapper" ,(search-path %load-path
                                       "distro/packages/ld-wrapper.scm"))))
    (arguments
     `(#:guile ,%bootstrap-guile
       #:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils)
                                (system base compile))

                   (let* ((out (assoc-ref %outputs "out"))
                          (bin (string-append out "/bin"))
                          (ld  (string-append bin "/ld"))
                          (go  (string-append bin "/ld.go")))

                     (setvbuf (current-output-port) _IOLBF)
                     (format #t "building ~s/bin/ld wrapper in ~s~%"
                             (assoc-ref %build-inputs "binutils")
                             out)

                     (mkdir-p bin)
                     (copy-file (assoc-ref %build-inputs "wrapper") ld)
                     (substitute* ld
                       (("@GUILE@")
                        (string-append (assoc-ref %build-inputs "guile")
                                       "/bin/guile"))
                       (("@LD@")
                        (string-append (assoc-ref %build-inputs "binutils")
                                       "/bin/ld")))
                     (chmod ld #o555)
                     (compile-file ld #:output-file go)))))
    (description "The linker wrapper")
    (long-description
     "The linker wrapper (or `ld-wrapper') wraps the linker to add any
missing `-rpath' flags, and to detect any misuse of libraries outside of the
store.")
    (home-page #f)
    (license "GPLv3+")))

(define %boot3-inputs
  ;; 4th stage inputs.
  `(("gcc" ,gcc-final)
    ("ld-wrapper" ,ld-wrapper-boot3)
    ,@(alist-delete "gcc" %boot2-inputs)))

(define-public bash-final
  (package-with-bootstrap-guile
   (package-with-explicit-inputs bash %boot3-inputs
                                 (current-source-location)
                                 #:guile %bootstrap-guile)))

(define-public guile-final
  (package-with-bootstrap-guile
   (package-with-explicit-inputs guile-2.0
                                 `(("bash" ,bash-final)
                                   ,@(alist-delete "bash" %boot3-inputs))
                                 (current-source-location)
                                 #:guile %bootstrap-guile)))

(define-public ld-wrapper
  ;; The final `ld' wrapper, which uses the final Guile.
  (package (inherit ld-wrapper-boot3)
    (name "ld-wrapper")
    (inputs `(("guile" ,guile-final)
              ,@(alist-delete "guile" (package-inputs ld-wrapper-boot3))))))

(define-public %final-inputs
  ;; Final derivations used as implicit inputs by `gnu-build-system'.
  (let ((finalize (cut package-with-explicit-inputs <> %boot3-inputs
                       (current-source-location))))
    `(,@(map (match-lambda
              ((name package)
               (list name (finalize package))))
             `(("tar" ,tar)
               ("gzip" ,gzip)
               ("bzip2" ,bzip2)
               ("xz" ,xz)
               ("diffutils" ,diffutils)
               ("patch" ,patch)
               ("coreutils" ,coreutils)
               ("sed" ,sed)
               ("grep" ,grep)
               ("findutils" ,findutils)
               ("gawk" ,gawk)
               ("make" ,gnu-make)))
      ("bash" ,bash-final)
      ("ld-wrapper" ,ld-wrapper)
      ("binutils" ,binutils-final)
      ("gcc" ,gcc-final)
      ("libc" ,glibc-final))))


;;;
;;; Bootstrap binaries.
;;;
;;; These are the binaries that are taken for granted and used as the
;;; root of the whole bootstrap procedure.
;;;

(define* (static-package p #:optional (loc (current-source-location)))
  "Return a statically-linked version of package P."
  ;; TODO: Move to (guix build-system gnu).
  (let ((args (package-arguments p)))
    (package (inherit p)
      (location (source-properties->location loc))
      (arguments
       (let ((augment (lambda (args)
                        (let ((a (default-keyword-arguments args
                                   '(#:configure-flags '()
                                     #:strip-flags #f))))
                          (substitute-keyword-arguments a
                            ((#:configure-flags flags)
                             `(cons* "--disable-shared"
                                     "LDFLAGS=-static"
                                     ,flags))
                            ((#:strip-flags _)
                             ''("--strip-all")))))))
         (if (procedure? args)
             (lambda x
               (augment (apply args x)))
             (augment args)))))))

(define %bash-static
  (let ((bash-light (package (inherit bash-final)
                      (inputs '())              ; no readline, no curses
                      (arguments
                       (let ((args `(#:modules ((guix build gnu-build-system)
                                                (guix build utils)
                                                (srfi srfi-1)
                                                (srfi srfi-26))
                                               ,@(package-arguments bash))))
                         (substitute-keyword-arguments args
                           ((#:configure-flags flags)
                            `(list "--without-bash-malloc"
                                   "--disable-readline"
                                   "--disable-history"
                                   "--disable-help-builtin"
                                   "--disable-progcomp"
                                   "--disable-net-redirections"
                                   "--disable-nls"))))))))
    (static-package bash-light)))

(define %static-inputs
  ;; Packages that are to be used as %BOOTSTRAP-INPUTS.
  (let ((coreutils (package (inherit coreutils)
                     (arguments
                      `(#:configure-flags
                        '("--disable-nls"
                          "--disable-silent-rules"
                          "--enable-no-install-program=stdbuf,libstdbuf.so"
                          "LDFLAGS=-static -pthread")
                        ,@(package-arguments coreutils)))))
        (bzip2 (package (inherit bzip2)
                 (arguments
                  (substitute-keyword-arguments (package-arguments bzip2)
                    ((#:phases phases)
                     `(alist-cons-before
                       'build 'dash-static
                       (lambda _
                         (substitute* "Makefile"
                           (("^LDFLAGS[[:blank:]]*=.*$")
                            "LDFLAGS = -static")))
                       ,phases))))))
        (xz (package (inherit xz)
              (arguments
               `(#:strip-flags '("--strip-all")
                 #:phases (alist-cons-before
                           'configure 'static-executable
                           (lambda _
                             ;; Ask Libtool for a static executable.
                             (substitute* "src/xz/Makefile.in"
                               (("^xz_LDADD =")
                                "xz_LDADD = -all-static")))
                           %standard-phases)))))
        (gawk (package (inherit gawk)
                (arguments
                 (lambda (system)
                   `(#:phases (alist-cons-before
                               'build 'no-export-dynamic
                               (lambda* (#:key outputs #:allow-other-keys)
                                 ;; Since we use `-static', remove
                                 ;; `-export-dynamic'.
                                 (substitute* "configure"
                                   (("-export-dynamic") "")))
                               %standard-phases)
                     ,@((package-arguments gawk) system)))))))
    `(,@(map (match-lambda
              ((name package)
               (list name (static-package package (current-source-location)))))
             `(("tar" ,tar)
               ("gzip" ,gzip)
               ("bzip2" ,bzip2)
               ("xz" ,xz)
               ("patch" ,patch)
               ("coreutils" ,coreutils)
               ("sed" ,sed)
               ("grep" ,grep)
               ("gawk" ,gawk)))
      ("bash" ,%bash-static)
      ;; ("ld-wrapper" ,ld-wrapper)
      ;; ("binutils" ,binutils-final)
      ;; ("gcc" ,gcc-final)
      ;; ("libc" ,glibc-final)
      )))

(define %static-binaries
  (package
    (name "static-binaries")
    (version "0")
    (build-system trivial-build-system)
    (source #f)
    (inputs %static-inputs)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (ice-9 ftw)
                      (ice-9 match)
                      (srfi srfi-1)
                      (srfi srfi-26)
                      (guix build utils))

         (let ()
          (define (directory-contents dir)
            (map (cut string-append dir "/" <>)
                 (scandir dir (negate (cut member <> '("." ".."))))))

          (define (copy-directory source destination)
            (for-each (lambda (file)
                        (format #t "copying ~s...~%" file)
                        (copy-file file
                                   (string-append destination "/"
                                                  (basename file))))
                      (directory-contents source)))

          (let* ((out (assoc-ref %outputs "out"))
                 (bin (string-append out "/bin")))
            (mkdir-p bin)

            ;; Copy Coreutils binaries.
            (let* ((coreutils (assoc-ref %build-inputs "coreutils"))
                   (source    (string-append coreutils "/bin")))
              (copy-directory source bin))

            ;; For the other inputs, copy just one binary, which has the
            ;; same name as the input.
            (for-each (match-lambda
                       ((name . dir)
                        (let ((source (string-append dir "/bin/" name)))
                          (format #t "copying ~s...~%" source)
                          (copy-file source
                                     (string-append bin "/" name)))))
                      (alist-delete "coreutils" %build-inputs))

            ;; Clear references to the store path.
            (for-each remove-store-references
                      (directory-contents bin))

            #t)))))
    (description "Statically-linked bootstrap binaries")
    (long-description
     "Binaries used to bootstrap the distribution.")
    (license #f)
    (home-page #f)))

(define %binutils-static
  ;; Statically-linked Binutils.
  (package (inherit binutils)
    (name "binutils-static")
    (arguments
     `(#:configure-flags '("--disable-gold")
       #:strip-flags '("--strip-all")
       #:phases (alist-cons-before
                 'configure 'all-static
                 (lambda _
                   ;; The `-all-static' libtool flag can only be passed
                   ;; after `configure', since configure tests don't use
                   ;; libtool, and only for executables built with libtool.
                   (substitute* ("binutils/Makefile.in"
                                 "gas/Makefile.in"
                                 "ld/Makefile.in")
                     (("^LDFLAGS =(.*)$" line)
                      (string-append line
                                     "\nAM_LDFLAGS = -static -all-static\n"))))
                 %standard-phases)))))

(define %binutils-static-stripped
  ;; The subset of Binutils that we need.
  (package (inherit %binutils-static)
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))

         (setvbuf (current-output-port) _IOLBF)
         (let* ((in  (assoc-ref %build-inputs "binutils"))
                (out (assoc-ref %outputs "out"))
                (bin (string-append out "/bin")))
           (mkdir-p bin)
           (for-each (lambda (file)
                       (let ((target (string-append bin "/" file)))
                         (format #t "copying `~a'...~%" file)
                         (copy-file (string-append in "/bin/" file)
                                    target)
                         (remove-store-references target)))
                     '("ar" "as" "ld" "nm"  "objcopy" "objdump"
                       "ranlib" "readelf" "size" "strings" "strip"))
           #t))))
    (inputs `(("binutils" ,%binutils-static)))))

(define %glibc-stripped
  ;; GNU libc's essential shared libraries, dynamic linker, and headers,
  ;; with all references to store directories stripped.  As a result,
  ;; libc.so is unusable and need to be patched for proper relocation.
  (package (inherit glibc-final)
    (name "glibc-stripped")
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))

         (setvbuf (current-output-port) _IOLBF)
         (let* ((out    (assoc-ref %outputs "out"))
                (libdir (string-append out "/lib"))
                (incdir (string-append out "/include"))
                (libc   (assoc-ref %build-inputs "libc"))
                (linux  (assoc-ref %build-inputs "linux-headers")))
           (mkdir-p libdir)
           (for-each (lambda (file)
                       (let ((target (string-append libdir "/"
                                                    (basename file))))
                         (copy-file file target)
                         (remove-store-references target)))
                     (find-files (string-append libc "/lib")
                                 "^(crt.*|ld.*|lib(c|m|dl|rt|pthread|nsl|util).*\\.so(\\..*)?|libc_nonshared\\.a)$"))

           (copy-recursively (string-append libc "/include") incdir)

           ;; Copy some of the Linux-Libre headers that glibc headers
           ;; refer to.
           (mkdir (string-append incdir "/linux"))
           (for-each (lambda (file)
                       (copy-file (string-append linux "/include/linux/" file)
                                  (string-append incdir "/linux/"
                                                 (basename file))))
                     '("limits.h" "errno.h" "socket.h" "kernel.h"
                       "sysctl.h" "param.h"))

           (mkdir (string-append incdir "/asm"))
           (for-each (lambda (file)
                       (copy-file (string-append linux "/include/asm/" file)
                                  (string-append incdir "/asm/"
                                                 (basename file))))
                     '("types.h" "unistd.h" "ioctls.h" "socket.h"
                       "param.h" "errno.h"))

           (copy-recursively (string-append linux "/include/asm-generic")
                             (string-append incdir "/asm-generic"))
           #t))))
    (inputs `(("libc" ,glibc-final)
              ("linux-headers" ,linux-libre-headers)))))

(define %gcc-static
  ;; A statically-linked GCC, with stripped-down functionality.
  (package (inherit gcc-final)
    (name "gcc-static")
    (arguments
     (lambda (system)
       `(#:modules ((guix build utils)
                    (guix build gnu-build-system)
                    (srfi srfi-1)
                    (srfi srfi-26)
                    (ice-9 regex))
         ,@(substitute-keyword-arguments ((package-arguments gcc-final) system)
             ((#:guile _) #f)
             ((#:implicit-inputs? _) #t)
             ((#:configure-flags flags)
              `(append (list
                        "--disable-shared"
                        "--disable-plugin"
                        "--enable-languages=c"
                        "--disable-libmudflap"
                        "--disable-libgomp"
                        "--disable-libssp"
                        "--disable-libquadmath"
                        "--disable-decimal-float")
                       (remove (cut string-match "--(.*plugin|enable-languages)" <>)
                               ,flags)))
             ((#:make-flags flags)
              `(cons "BOOT_LDFLAGS=-static" ,flags))))))
    (inputs `(("gmp-source" ,(package-source gmp))
              ("mpfr-source" ,(package-source mpfr))
              ("mpc-source" ,(package-source mpc))
              ("binutils" ,binutils-final)
              ,@(package-inputs gcc-4.7)))))

(define %gcc-stripped
  ;; The subset of GCC files needed for bootstrap.
  (package (inherit gcc-4.7)
    (name "gcc-stripped")
    (build-system trivial-build-system)
    (source #f)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (srfi srfi-1)
                      (srfi srfi-26)
                      (guix build utils))

         (setvbuf (current-output-port) _IOLBF)
         (let* ((out        (assoc-ref %outputs "out"))
                (bindir     (string-append out "/bin"))
                (libdir     (string-append out "/lib"))
                (libexecdir (string-append out "/libexec"))
                (gcc        (assoc-ref %build-inputs "gcc")))
           (copy-recursively (string-append gcc "/bin") bindir)
           (for-each remove-store-references
                     (find-files bindir ".*"))

           (copy-recursively (string-append gcc "/lib") libdir)
           (for-each remove-store-references
                     (remove (cut string-suffix? ".h" <>)
                             (find-files libdir ".*")))

           (copy-recursively (string-append gcc "/libexec")
                             libexecdir)
           (for-each remove-store-references
                     (find-files libexecdir ".*"))
           #t))))
    (inputs `(("gcc" ,%gcc-static)))))

(define %guile-static
  ;; A statically-linked Guile that is relocatable--i.e., it can search
  ;; .scm and .go files relative to its installation directory, rather
  ;; than in hard-coded configure-time paths.
  (let ((guile (package (inherit guile-2.0)
                 (inputs
                  `(("patch/relocatable"
                     ,(search-patch "guile-relocatable.patch"))
                    ("patch/utf8"
                     ,(search-patch "guile-default-utf8.patch"))
                    ,@(package-inputs guile-2.0)))
                 (arguments
                  `(;; When `configure' checks for ltdl availability, it
                    ;; doesn't try to link using libtool, and thus fails
                    ;; because of a missing -ldl.  Work around that.
                    #:configure-flags '("LDFLAGS=-ldl")

                    #:phases (alist-cons-before
                              'configure 'static-guile
                              (lambda _
                                (substitute* "libguile/Makefile.in"
                                  ;; Create a statically-linked `guile'
                                  ;; executable.
                                  (("^guile_LDFLAGS =")
                                   "guile_LDFLAGS = -all-static")

                                  ;; Add `-ldl' *after* libguile-2.0.la.
                                  (("^guile_LDADD =(.*)$" _ ldadd)
                                   (string-append "guile_LDADD = "
                                                  (string-trim-right ldadd)
                                                  " -ldl\n"))))
                              %standard-phases)

                    ;; Allow Guile to be relocated, as is needed during
                    ;; bootstrap.
                    #:patches
                    (list (assoc-ref %build-inputs "patch/relocatable")
                          (assoc-ref %build-inputs "patch/utf8"))

                    ;; There are uses of `dynamic-link' in
                    ;; {foreign,coverage}.test that don't fly here.
                    #:tests? #f)))))
    (static-package guile (current-source-location))))

(define %guile-static-stripped
  ;; A stripped static Guile binary, for use during bootstrap.
  (package (inherit %guile-static)
    (name "guile-static-stripped")
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (let ()
         (use-modules (guix build utils))

         (let ((in  (assoc-ref %build-inputs "guile"))
               (out (assoc-ref %outputs "out")))
           (mkdir-p (string-append out "/share/guile/2.0"))
           (copy-recursively (string-append in "/share/guile/2.0")
                             (string-append out "/share/guile/2.0"))

           (mkdir-p (string-append out "/lib/guile/2.0/ccache"))
           (copy-recursively (string-append in "/lib/guile/2.0/ccache")
                             (string-append out "/lib/guile/2.0/ccache"))

           (mkdir (string-append out "/bin"))
           (copy-file (string-append in "/bin/guile")
                      (string-append out "/bin/guile"))
           (remove-store-references (string-append out "/bin/guile"))
           #t))))
    (inputs `(("guile" ,%guile-static)))))

(define (tarball-package pkg)
  "Return a package containing a tarball of PKG."
  (package (inherit pkg)
    (location (source-properties->location (current-source-location)))
    (name (string-append (package-name pkg) "-tarball"))
    (build-system trivial-build-system)
    (inputs `(("tar" ,tar)
              ("xz" ,xz)
              ("input" ,pkg)))
    (arguments
     (lambda (system)
       (let ((name    (package-name pkg))
             (version (package-version pkg)))
         `(#:modules ((guix build utils))
           #:builder
           (begin
             (use-modules (guix build utils))
             (let ((out   (assoc-ref %outputs "out"))
                   (input (assoc-ref %build-inputs "input"))
                   (tar   (assoc-ref %build-inputs "tar"))
                   (xz    (assoc-ref %build-inputs "xz")))
               (mkdir out)
               (set-path-environment-variable "PATH" '("bin") (list tar xz))
               (with-directory-excursion input
                 (zero? (system* "tar" "cJvf"
                                 (string-append out "/"
                                                ,name "-" ,version
                                                "-" ,system ".tar.xz")
                                 ".")))))))))))

(define %bootstrap-binaries-tarball
  ;; A tarball with the statically-linked bootstrap binaries.
  (tarball-package %static-binaries))

(define %binutils-bootstrap-tarball
  ;; A tarball with the statically-linked Binutils programs.
  (tarball-package %binutils-static-stripped))

(define %glibc-bootstrap-tarball
  ;; A tarball with GNU libc's shared libraries, dynamic linker, and headers.
  (tarball-package %glibc-stripped))

(define %gcc-bootstrap-tarball
  ;; A tarball with a dynamic-linked GCC and its headers.
  (tarball-package %gcc-stripped))

(define %guile-bootstrap-tarball
  ;; A tarball with the statically-linked, relocatable Guile.
  (tarball-package %guile-static-stripped))

;;; base.scm ends here
