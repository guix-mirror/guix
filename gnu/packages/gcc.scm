;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages elf)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module (ice-9 regex))

(define %gcc-infrastructure
  ;; Base URL for GCC's infrastructure.
  "ftp://gcc.gnu.org/pub/gcc/infrastructure/")

(define-public (gcc-configure-flags-for-triplet target)
  "Return a list of additional GCC `configure' flags for TARGET, a GNU triplet.

The purpose of this procedure is to translate extended GNU triplets---e.g.,
where the OS part is overloaded to denote a specific ABI---into GCC
`configure' options.  We take extended GNU triplets that glibc recognizes."
  (cond ((string-match "^mips64el.*gnuabin?64$" target)
         ;; Triplets recognized by glibc as denoting the N64 ABI; see
         ;; ports/sysdeps/mips/preconfigure.
         '("--with-abi=64"))
        (else
         ;; TODO: Add `armel.*gnueabi', `hf', etc.
         '())))

(define-public gcc-4.7
  (let* ((stripped? #t)                           ; TODO: make this a parameter
         (install-target
          (lambda ()
            ;; The 'install-strip' rule uses the native 'strip' instead of
            ;; 'TARGET-strip' when cross-compiling.  Thus, use 'install' in that
            ;; case.
            (if (and stripped? (not (%current-target-system)))
                "install-strip"
                "install")))
         (maybe-target-tools
          (lambda ()
            ;; Return the `_FOR_TARGET' variables that are needed when
            ;; cross-compiling GCC.
            (let ((target (%current-target-system)))
              (if target
                  (map (lambda (var tool)
                         (string-append (string-append var "_FOR_TARGET")
                                        "=" target "-" tool))
                       '("CC"  "CXX" "LD" "AR" "NM" "RANLIB" "STRIP")
                       '("gcc" "g++" "ld" "ar" "nm" "ranlib" "strip"))
                  '()))))
         (configure-flags
          (lambda ()
            ;; This is terrible.  Since we have two levels of quasiquotation,
            ;; we have to do this convoluted thing just so we can insert the
            ;; contents of (maybe-target-tools).
            (list 'quasiquote
                  (append
                   '("--enable-plugin"
                     "--enable-languages=c,c++"
                     "--disable-multilib"

                     ;; No pre-compiled libstdc++ headers, to save space.
                     "--disable-libstdcxx-pch"

                     "--with-local-prefix=/no-gcc-local-prefix"

                     ;; With a separate "lib" output, the build system
                     ;; incorrectly guesses GPLUSPLUS_INCLUDE_DIR, so force
                     ;; it.  (Don't use a versioned sub-directory, that's
                     ;; unnecessary.)
                     ,(string-append "--with-gxx-include-dir="
                                     (assoc-ref %outputs "out")
                                     "/include/c++")

                     ,(let ((libc (assoc-ref %build-inputs "libc")))
                        (if libc
                            (string-append "--with-native-system-header-dir=" libc
                                           "/include")
                            "--without-headers")))

                   ;; When cross-compiling GCC, pass the right options for the
                   ;; target triplet.
                   (or (and=> (%current-target-system)
                              gcc-configure-flags-for-triplet)
                       '())

                   (maybe-target-tools))))))
    (package
      (name "gcc")
      (version "4.7.4")
      (source (origin
               (method url-fetch)
               (uri (string-append "mirror://gnu/gcc/gcc-"
                                   version "/gcc-" version ".tar.bz2"))
               (sha256
                (base32
                 "10k2k71kxgay283ylbbhhs51cl55zn2q38vj5pk4k950qdnirrlj"))))
      (build-system gnu-build-system)

      ;; Separate out the run-time support libraries because all the
      ;; dynamic-linked objects depend on it.
      (outputs '("out"                     ; commands, etc. (60+ MiB)
                 "lib"))                   ; libgcc_s, libgomp, etc. (15+ MiB)

      (inputs `(("gmp" ,gmp)
                ("mpfr" ,mpfr)
                ("mpc" ,mpc)
                ("isl" ,isl)
                ("cloog" ,cloog)
                ("libelf" ,libelf)
                ("zlib" ,zlib)))

      ;; GCC is one of the few packages that doesn't ship .info files.
      (native-inputs `(("texinfo" ,texinfo)))

      (arguments
       `(#:out-of-source? #t
         #:strip-binaries? ,stripped?
         #:configure-flags ,(configure-flags)
         #:make-flags
         ;; None of the flags below are needed when doing a Canadian cross.
         ;; TODO: Simplify this.
         ,(if (%current-target-system)
              (if stripped?
                  ''("CFLAGS=-g0 -O2")
                  ''())
              `(let* ((libc        (assoc-ref %build-inputs "libc"))
                      (libc-native (or (assoc-ref %build-inputs "libc-native")
                                       libc)))
                 `(,@(if libc
                         (list (string-append "LDFLAGS_FOR_TARGET="
                                              "-B" libc "/lib "
                                              "-Wl,-dynamic-linker "
                                              "-Wl," libc
                                              ,(glibc-dynamic-linker)))
                         '())

                   ;; Native programs like 'genhooks' also need that right.
                   ,(string-append "LDFLAGS="
                                   "-Wl,-rpath=" libc-native "/lib "
                                   "-Wl,-dynamic-linker "
                                   "-Wl," libc-native ,(glibc-dynamic-linker))
                   ,(string-append "BOOT_CFLAGS=-O2 "
                                   ,(if stripped? "-g0" "-g")))))

         #:tests? #f
         #:phases
         (alist-cons-before
          'configure 'pre-configure
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((libdir (or (assoc-ref outputs "lib")
                              (assoc-ref outputs "out")))
                  (libc   (assoc-ref inputs "libc")))
              (when libc
                ;; The following is not performed for `--without-headers'
                ;; cross-compiler builds.

                ;; Join multi-line definitions of GLIBC_DYNAMIC_LINKER* into a
                ;; single line, to allow the next step to work properly.
                (for-each
                 (lambda (x)
                   (substitute* (find-files "gcc/config"
                                            "^linux(64|-elf)?\\.h$")
                     (("(#define GLIBC_DYNAMIC_LINKER.*)\\\\\n$" _ line)
                      line)))
                 '(1 2 3))

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
                                         "^gnu-user.*\\.h$")
                  (("#define GNU_USER_TARGET_LIB_SPEC (.*)$" _ suffix)
                   ;; Help libgcc_s.so be found (see also below.)  Always use
                   ;; '-lgcc_s' so that libgcc_s.so is always found by those
                   ;; programs that use 'pthread_cancel' (glibc dlopens
                   ;; libgcc_s.so when pthread_cancel support is needed, but
                   ;; having it in the application's RUNPATH isn't enough; see
                   ;; <http://sourceware.org/ml/libc-help/2013-11/msg00023.html>.)
                   (format #f "#define GNU_USER_TARGET_LIB_SPEC \
\"-L~a/lib %{!static:-rpath=~a/lib %{!static-libgcc:-rpath=~a/lib64 -rpath=~a/lib -lgcc_s}} \" ~a"
                           libc libc libdir libdir suffix))
                  (("#define GNU_USER_TARGET_STARTFILE_SPEC.*$" line)
                   (format #f "#define STANDARD_STARTFILE_PREFIX_1 \"~a/lib\"
#define STANDARD_STARTFILE_PREFIX_2 \"\"
~a"
                           libc line))))

              ;; Don't retain a dependency on the build-time sed.
              (substitute* "fixincludes/fixincl.x"
                (("static char const sed_cmd_z\\[\\] =.*;")
                 "static char const sed_cmd_z[] = \"sed\";"))

              ;; Move libstdc++*-gdb.py to the "lib" output to avoid a
              ;; circularity between "out" and "lib".  (Note:
              ;; --with-python-dir is useless because it imposes $(prefix) as
              ;; the parent directory.)
              (substitute* "libstdc++-v3/python/Makefile.in"
                (("pythondir = .*$")
                 (string-append "pythondir = " libdir "/share"
                                "/gcc-$(gcc_version)/python\n")))

              ;; Avoid another circularity between the outputs: this #define
              ;; ends up in auto-host.h in the "lib" output, referring to
              ;; "out".  (This variable is used to augment cpp's search path,
              ;; but there's nothing useful to look for here.)
              (substitute* "gcc/config.in"
                (("PREFIX_INCLUDE_DIR")
                 "PREFIX_INCLUDE_DIR_isnt_necessary_here"))))

          (alist-cons-after
           'configure 'post-configure
           (lambda _
             ;; Don't store configure flags, to avoid retaining references to
             ;; build-time dependencies---e.g., `--with-ppl=/gnu/store/xxx'.
             (substitute* "Makefile"
               (("^TOPLEVEL_CONFIGURE_ARGUMENTS=(.*)$" _ rest)
                "TOPLEVEL_CONFIGURE_ARGUMENTS=\n")))
           (alist-replace 'install
                          (lambda* (#:key outputs #:allow-other-keys)
                            (zero?
                             (system* "make" ,(install-target))))
                          %standard-phases)))))

      (native-search-paths
       (list (search-path-specification
              (variable "CPATH")
              (files '("include")))
             (search-path-specification
              (variable "LIBRARY_PATH")
              (files '("lib" "lib64")))))

      (properties `((gcc-libc . ,(assoc-ref inputs "libc"))))
      (synopsis "GNU Compiler Collection")
      (description
       "GCC is the GNU Compiler Collection.  It provides compiler front-ends
for several languages, including C, C++, Objective-C, Fortran, Java, Ada, and
Go.  It also includes runtime support libraries for these languages.")
      (license gpl3+)
      (home-page "http://gcc.gnu.org/"))))

(define-public gcc-4.8
  (package (inherit gcc-4.7)
    (version "4.8.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/gcc/gcc-"
                                 version "/gcc-" version ".tar.bz2"))
             (sha256
              (base32
               "15c6gwm6dzsaagamxkak5smdkf1rdfbqqjs9jdbrp3lbg4ism02a"))))))

(define-public gcc-4.9
  (package (inherit gcc-4.7)
    (version "4.9.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/gcc/gcc-"
                                 version "/gcc-" version ".tar.bz2"))
             (sha256
              (base32
               "1pbjp4blk2ycaa6r3jmw4ky5f1s9ji3klbqgv8zs2sl5jn1cj810"))))))

(define* (custom-gcc gcc name languages #:key (separate-lib-output? #t))
  "Return a custom version of GCC that supports LANGUAGES."
  (package (inherit gcc)
    (name name)
    (outputs (if separate-lib-output?
                 (package-outputs gcc)
                 (delete "lib" (package-outputs gcc))))
    (arguments
     (substitute-keyword-arguments `(#:modules ((guix build gnu-build-system)
                                                (guix build utils)
                                                (ice-9 regex)
                                                (srfi srfi-1)
                                                (srfi srfi-26))
                                               ,@(package-arguments gcc))
       ((#:configure-flags flags)
        `(cons (string-append "--enable-languages="
                              ,(string-join languages ","))
               (remove (cut string-match "--enable-languages.*" <>)
                       ,flags)))))))

(define-public gfortran-4.8
  (custom-gcc gcc-4.8 "gfortran" '("fortran")))

(define-public gccgo-4.8
  (custom-gcc gcc-4.8 "gccgo" '("go")
              ;; Suppress the separate "lib" output, because otherwise the
              ;; "lib" and "out" outputs would refer to each other, creating
              ;; a cyclic dependency.  <http://debbugs.gnu.org/18101>
              #:separate-lib-output? #f))

(define-public gcc-objc-4.8
  (custom-gcc gcc-4.8 "gcc-objc" '("objc")))

(define-public gcc-objc++-4.8
  (custom-gcc gcc-4.8 "gcc-objc++" '("obj-c++")))

(define-public isl
  (package
    (name "isl")
    (version "0.11.1")
    (source (origin
             (method url-fetch)
             (uri (list (string-append
                         "http://isl.gforge.inria.fr/isl-"
                         version
                         ".tar.bz2")
                        (string-append %gcc-infrastructure
                                       name "-" version ".tar.gz")))
             (sha256
              (base32
               "13d9cqa5rzhbjq0xf0b2dyxag7pqa72xj9dhsa03m8ccr1a4npq9"))))
    (build-system gnu-build-system)
    (inputs `(("gmp" ,gmp)))
    (home-page "http://isl.gforge.inria.fr/")
    (synopsis
     "Manipulating sets and relations of integer points \
bounded by linear constraints")
    (description
     "isl is a library for manipulating sets and relations of integer points
bounded by linear constraints.  Supported operations on sets include
intersection, union, set difference, emptiness check, convex hull, (integer)
affine hull, integer projection, computing the lexicographic minimum using
parametric integer programming, coalescing and parametric vertex
enumeration.  It also includes an ILP solver based on generalized basis
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
    (synopsis "Library to generate code for scanning Z-polyhedra")
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

