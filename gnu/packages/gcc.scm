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
  #:use-module (gnu packages texinfo)
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
      (version "4.7.3")
      (source (origin
               (method url-fetch)
               (uri (string-append "mirror://gnu/gcc/gcc-"
                                   version "/gcc-" version ".tar.bz2"))
               (sha256
                (base32
                 "1hx9h64ivarlzi4hxvq42as5m9vlr5cyzaaq4gzj4i619zmkfz1g"))))
      (build-system gnu-build-system)
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
         (let* ((libc        (assoc-ref %build-inputs "libc"))
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
                                         "^gnu-user.*\\.h$")
                  (("#define GNU_USER_TARGET_LIB_SPEC (.*)$" _ suffix)
                   ;; Note that with this "lib" spec, we may still add a
                   ;; RUNPATH to GCC even when `libgcc_s' is not NEEDED.
                   ;; There's not much that can be done to avoid it, though.
                   (format #f "#define GNU_USER_TARGET_LIB_SPEC \
\"-L~a/lib %{!static:-rpath=~a/lib %{!static-libgcc:-rpath=~a/lib64 -rpath=~a/lib}} \" ~a"
                           libc libc out out suffix))
                  (("#define GNU_USER_TARGET_STARTFILE_SPEC.*$" line)
                   (format #f "#define STANDARD_STARTFILE_PREFIX_1 \"~a/lib\"
#define STANDARD_STARTFILE_PREFIX_2 \"\"
~a"
                           libc line))))

              ;; Use '-lgcc_s' rather than '--as-needed -lgcc_s', so that
              ;; libgcc_s.so is always found by those programs that use
              ;; 'pthread_cancel' (glibc dlopens libgcc_s.so when
              ;; pthread_cancel support is needed, but having it in the
              ;; application's RUNPATH isn't enough; see
              ;; <http://sourceware.org/ml/libc-help/2013-11/msg00023.html>.)
              ;; Also, "gcc_cv_ld_as_needed=no" as a configure flag doesn't
              ;; work.
              (substitute* "gcc/gcc.c"
                (("#ifndef USE_LD_AS_NEEDED.*$" line)
                 (string-append "#undef USE_LD_AS_NEEDED\n" line)))

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
       "GCC is the GNU Compiler Collection.  It provides compiler front-ends
for several languages, including C, C++, Objective-C, Fortran, Java, Ada, and
Go.  It also includes standard libraries for these languages.")
      (license gpl3+)
      (home-page "http://gcc.gnu.org/"))))

(define-public gcc-4.8
  (package (inherit gcc-4.7)
    (version "4.8.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/gcc/gcc-"
                                 version "/gcc-" version ".tar.bz2"))
             (sha256
              (base32
               "1j6dwgby4g3p3lz7zkss32ghr45zpdidrg8xvazvn91lqxv25p09"))))))

(define (custom-gcc gcc name languages)
  "Return a custom version of GCC that supports LANGUAGES."
  (package (inherit gcc)
    (name name)
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
  (custom-gcc gcc-4.8 "gccgo" '("go")))

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
