;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages gcc)
  #:use-module ((guix licenses)
                #:select (gpl3+ gpl2+ lgpl2.1+ lgpl2.0+))
  #:use-module (gnu packages)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages multiprecision)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define %gcc-infrastructure
  ;; Base URL for GCC's infrastructure.
  "ftp://gcc.gnu.org/pub/gcc/infrastructure/")

(define-public gcc-4.7
  (let ((stripped? #t))                         ; TODO: make this a parameter
    (package
     (name "gcc")
     (version "4.7.2")
     (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gcc/gcc-"
                                  version "/gcc-" version ".tar.bz2"))
              (sha256
               (base32
                "115h03hil99ljig8lkrq4qk426awmzh0g99wrrggxf8g07bq74la"))))
     (build-system gnu-build-system)
     (inputs `(("gmp" ,gmp)
               ("mpfr" ,mpfr)
               ("mpc" ,mpc)))           ; TODO: libelf, ppl, cloog, zlib, etc.
     (arguments
      `(#:out-of-source? #t
        #:strip-binaries? ,stripped?
        #:configure-flags
        `("--enable-plugin"
          "--enable-languages=c,c++"
          "--disable-multilib"

          "--with-local-prefix=/no-gcc-local-prefix"

          ,(let ((libc (assoc-ref %build-inputs "libc")))
             (if libc
                 (string-append "--with-native-system-header-dir=" libc
                                "/include")
                 "--without-headers")))
        #:make-flags
        (let ((libc (assoc-ref %build-inputs "libc")))
          `(,@(if libc
                  (list (string-append "LDFLAGS_FOR_TARGET="
                                       "-B" libc "/lib "
                                       "-Wl,-dynamic-linker "
                                       "-Wl," libc
                                       ,(glibc-dynamic-linker)))
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
               (substitute* (find-files "gcc/config"
                                        "^linux(64|-elf)?\\.h$")
                 (("#define GLIBC_DYNAMIC_LINKER([^ ]*).*$" _ suffix)
                  (format #f "#define GLIBC_DYNAMIC_LINKER~a \"~a\"~%"
                          suffix
                          (string-append libc ,(glibc-dynamic-linker)))))

               ;; Tell where to find libstdc++, libc, and `?crt*.o', except
               ;; `crt{begin,end}.o', which come with GCC.
               (substitute* (find-files "gcc/config"
                                        "^(gnu-user(64)?|linux-elf)\\.h$")
                 (("#define LIB_SPEC (.*)$" _ suffix)
                  ;; Note that with this "lib" spec, we may still add a
                  ;; RUNPATH to GCC even when `libgcc_s' is not NEEDED.
                  ;; There's not much that can be done to avoid it, though.
                  (format #f "#define LIB_SPEC \"-L~a/lib %{!static:-rpath=~a/lib \
%{!static-libgcc:-rpath=~a/lib64 -rpath=~a/lib}} \" ~a"
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
                         %standard-phases)))))

     (native-search-paths
      (list (search-path-specification
             (variable "CPATH")
             (directories '("include")))
            (search-path-specification
             (variable "LIBRARY_PATH")
             (directories '("lib" "lib64")))))

     (properties `((gcc-libc . ,(assoc-ref inputs "libc"))))
     (synopsis "GNU Compiler Collection")
     (description
      "The GNU Compiler Collection includes compiler front ends for C, C++,
Objective-C, Fortran, OpenMP for C/C++/Fortran, Java, and Ada, as well as
libraries for these languages (libstdc++, libgcj, libgomp,...).

GCC development is a part of the GNU Project, aiming to improve the compiler
used in the GNU system including the GNU/Linux variant.")
     (license gpl3+)
     (home-page "http://gcc.gnu.org/"))))

(define-public isl
  (package
    (name "isl")
    (version "0.11.1")
    (source (origin
             (method url-fetch)
             (uri (list (string-append
                         "ftp://ftp.linux.student.kuleuven.be/pub/people/skimo/isl/isl-"
                         version
                         ".tar.bz2")
                        (string-append %gcc-infrastructure
                                       name "-" version ".tar.gz")))
             (sha256
              (base32
               "13d9cqa5rzhbjq0xf0b2dyxag7pqa72xj9dhsa03m8ccr1a4npq9"))))
    (build-system gnu-build-system)
    (inputs `(("gmp" ,gmp)))
    (home-page "http://www.kotnet.org/~skimo/isl/")
    (synopsis
     "A library for manipulating sets and relations of integer points bounded
by linear constraints")
    (description
     "isl is a library for manipulating sets and relations of integer points
bounded by linear constraints. Supported operations on sets include
intersection, union, set difference, emptiness check, convex hull, (integer)
affine hull, integer projection, computing the lexicographic minimum using
parametric integer programming, coalescing and parametric vertex
enumeration. It also includes an ILP solver based on generalized basis
reduction, transitive closures on maps (which may encode infinite graphs),
dependence analysis and bounds on piecewise step-polynomials.")
    (license lgpl2.1+)))

(define-public cloog
  (package
    (name "cloog")
    (version "0.18.0")
    (source
     (origin
      (method url-fetch)
      (uri (list (string-append
                  "http://www.bastoul.net/cloog/pages/download/count.php3?url=cloog-"
                  version
                  ".tar.gz")
                 (string-append %gcc-infrastructure
                                name "-" version ".tar.gz")))
      (sha256
       (base32
        "0a12rwfwp22zd0nlld0xyql11cj390rrq1prw35yjsw8wzfshjhw"))
      (file-name (string-append name "-" version ".tar.gz"))))
    (build-system gnu-build-system)
    (inputs `(("gmp" ,gmp)
              ("isl" ,isl)))
    (arguments '(#:configure-flags '("--with-isl=system")))
    (home-page "http://www.cloog.org/")
    (synopsis "A library to generate code for scanning Z-polyhedra")
    (description
     "CLooG is a free software library to generate code for scanning
Z-polyhedra.  That is, it finds a code (e.g., in C, FORTRAN...) that
reaches each integral point of one or more parameterized polyhedra.
CLooG has been originally written to solve the code generation problem
for optimizing compilers based on the polytope model.  Nevertheless it
is used now in various area e.g., to build control automata for
high-level synthesis or to find the best polynomial approximation of a
function.  CLooG may help in any situation where scanning polyhedra
matters.  While the user has full control on generated code quality,
CLooG is designed to avoid control overhead and to produce a very
effective code.")
    (license gpl2+)))

(define-public libelf
  (package
    (name "libelf")
    (version "0.8.13")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://www.mr511.de/software/libelf-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0vf7s9dwk2xkmhb79aigqm0x0yfbw1j0b9ksm51207qwr179n6jr"))))
    (build-system gnu-build-system)
    (arguments '(#:phases (alist-replace
                           'configure
                           (lambda* (#:key outputs #:allow-other-keys)
                             ;; This old `configure' script doesn't support
                             ;; variables passed as arguments.
                             (let ((out (assoc-ref outputs "out")))
                               (setenv "CONFIG_SHELL" (which "bash"))
                               (zero?
                                (system* "./configure"
                                         (string-append "--prefix=" out)))))
                           %standard-phases)))
    (home-page "http://www.mr511.de/software/english.html")
    (synopsis "An ELF object file access library")
    (description "libelf is a C library to access ELF object files.")
    (license lgpl2.0+)))
