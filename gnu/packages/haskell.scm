;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015 Siniša Biđin <sinisa@bidin.eu>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2015, 2019 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016, 2018, 2019, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020, 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017 David Craven <david@craven.ch>
;;; Copyright © 2017 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2017 Peter Mikkelsen <petermikkelsen10@gmail.com>
;;; Copyright © 2017, 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2017 rsiddharth <s@ricketyspace.net>
;;; Copyright © 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Tonton <tonton@riseup.net>
;;; Copyright © 2018, 2019, 2020 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2018, 2019 Gabriel Hondet <gabrielhondet@gmail.com>
;;; Copyright © 2019 Robert Vollmert <rob@vllmrt.net>
;;; Copyright © 2019 Jacob MacDonald <jaccarmac@gmail.com>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2021 Matthew James Kraai <kraai@ftbfs.org>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2021 Simon Tournier <zimon.toutoune@gmail.com>
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

(define-module (gnu packages haskell)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages file)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages version-control)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex))

(define-public cl-yale-haskell
  (let ((commit "85f94c72a16c5f70301dd8db04cde9de2d7dd270")
        (revision "1"))
    (package
      (name "cl-yale-haskell")
      (version (string-append "2.0.5-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.elephly.net/software/yale-haskell.git")
                      (commit commit)))
                (file-name (string-append "yale-haskell-" commit "-checkout"))
                (sha256
                 (base32
                  "0bal3m6ryrjamz5p93bhs9rp5msk8k7lpcqr44wd7xs9b9k8w74g"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ; no tests
         ;; Stripping binaries leads to a broken executable lisp system image.
         #:strip-binaries? #f
         #:make-flags
         (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
         #:phases
         (modify-phases %standard-phases
           (replace 'configure
             (lambda _
               (setenv "PRELUDE" "./progs/prelude")
               (setenv "HASKELL_LIBRARY" "./progs/lib")
               (setenv "PRELUDEBIN" "./progs/prelude/clisp")
               (setenv "HASKELLPROG" "./bin/clisp-haskell")
               #t)))))
      (inputs
       (list clisp))
      (home-page "https://git.elephly.net/software/yale-haskell.git")
      (synopsis "Port of the Yale Haskell system to CLISP")
      (description "This package provides the Yale Haskell system running on
top of CLISP.")
      (license license:bsd-4))))

;; This package contains lots of generated .hc files containing C code to
;; bootstrap the compiler without a Haskell compiler.  The included .hc files
;; cover not just the compiler sources but also all Haskell libraries.
(define-public nhc98
  (package
    (name "nhc98")
    (version "1.22")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.haskell.org/nhc98/nhc98src-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0fkgxgsd2iqxvwcgnad1702kradwlbcal6rxdrgb22vd6dnc3i8l"))))
    (build-system gnu-build-system)
    (supported-systems '("i686-linux" "x86_64-linux"))
    (arguments
     (list
      #:tests? #false                   ;there is no test target
      #:system "i686-linux"
      #:implicit-inputs? #false
      #:parallel-build? #false          ;not supported
      #:strip-binaries? #false          ;doesn't work
      #:make-flags '(list "all-gcc")
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            (lambda _
              (setenv "SHELL" (which "sh"))
              (setenv "CPATH" (string-append
                               (getcwd) "/src/runtime/Kernel:"
                               (or (getenv "C_INCLUDE_PATH") "")))
              (substitute* "configure"
                (("echo '#!/bin/sh'")
                 (string-append "echo '#!" (which "sh") "'")))
              (with-fluids ((%default-port-encoding #f))
                (substitute* '("script/greencard.inst"
                               "script/harch.inst"
                               "script/hi.inst"
                               "script/hmake-config.inst"
                               ;; TODO: can't fix this with substitute*
                                        ;"script/hmake.inst"
                               "script/hood.inst"
                               "script/hsc2hs.inst"
                               "script/nhc98-pkg.inst"
                               "script/nhc98.inst")
                  (("^MACHINE=.*") "MACHINE=ix86-Linux\n")))
              (invoke "sh" "configure"
                      (string-append "--prefix=" #$output)
                      ;; Remove -m32 from compiler/linker invocation
                      "--ccoption="
                      "--ldoption=")))
          (replace 'install
            (lambda _
              (invoke "sh" "configure"
                      (string-append "--prefix=" #$output)
                      ;; Remove -m32 from compiler/linker invocation
                      "--ccoption="
                      "--ldoption="
                      "--install"))))))
    (native-inputs
     `(("findutils" ,findutils)
       ("tar" ,tar)
       ("bzip2" ,bzip2)
       ("gzip" ,gzip)
       ("xz" ,xz)
       ("diffutils" ,diffutils)
       ("file" ,file)
       ("gawk" ,gawk)

       ("make" ,gnu-make)
       ("sed" ,sed)
       ("grep" ,grep)
       ("coreutils" ,coreutils)
       ("bash" ,bash-minimal)

       ("libc" ,glibc-2.2.5)
       ("gcc-wrapper"
        ,(module-ref (resolve-interface
                      '(gnu packages commencement))
                     'gcc-2.95-wrapper))
       ("gcc"
        ,(module-ref (resolve-interface
                      '(gnu packages commencement))
                     'gcc-mesboot0))
       ("binutils"
        ,(module-ref (resolve-interface
                      '(gnu packages commencement))
                     'binutils-mesboot))
       ("kernel-headers" ,linux-libre-headers)))
    (home-page "https://www.haskell.org/nhc98")
    (synopsis "Nearly a Haskell Compiler")
    (description
     "nhc98 is a small, standards-compliant compiler for Haskell 98, the lazy
functional programming language.  It aims to produce small executables that
run in small amounts of memory.  It produces medium-fast code, and compilation
is itself quite fast.")
    (license
     (license:non-copyleft
      "https://www.haskell.org/nhc98/copyright.html"))))

(define-public ghc-4
  (package
    (name "ghc")
    (version "4.08.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.haskell.org/ghc/dist/"
                           version "/" name "-" version "-src.tar.bz2"))
       (sha256
        (base32
         "0ar4nxy4cr5vwvfj71gmc174vx0n3lg9ka05sa1k60c8z0g3xp1q"))
       (patches (list (search-patch "ghc-4.patch")))))
    (build-system gnu-build-system)
    (supported-systems '("i686-linux" "x86_64-linux"))
    (arguments
     `(#:system "i686-linux"
       #:implicit-inputs? #f
       #:strip-binaries? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'bootstrap
           (lambda* (#:key inputs #:allow-other-keys)
             (delete-file "configure")
             (delete-file "config.sub")
             (install-file (string-append (assoc-ref inputs "automake")
                                          "/share/automake-1.16/config.sub")
                           ".")

             ;; Avoid dependency on "happy"
             (substitute* "configure.in"
               (("FPTOOLS_HAPPY") "echo sure\n"))

             ;; Set options suggested in ghc/interpreter/README.BUILDING.HUGS.
             (with-output-to-file "mk/build.mk"
               (lambda ()
                 (display "
WithGhcHc=ghc-4.06
GhcLibWays=u
#HsLibsFor=hugs
# Setting this leads to building the interpreter.
GhcHcOpts=-DDEBUG
GhcRtsHcOpts=-optc-DDEBUG -optc-D__HUGS__ -unreg -optc-g
GhcRtsCcOpts=-optc-DDEBUG -optc-g -optc-D__HUGS__
SplitObjs=NO
")))

             (substitute* "ghc/interpreter/interface.c"
               ;; interface.c:2702: `stackOverflow' redeclared as different kind of symbol
               ;; ../includes/Stg.h:188: previous declaration of `stackOverflow'
               ((".*Sym\\(stackOverflow\\).*") "")
               ;; interface.c:2713: `stg_error_entry' undeclared here (not in a function)
               ;; interface.c:2713: initializer element is not constant
               ;; interface.c:2713: (near initialization for `rtsTab[11].ad')
               ((".*SymX\\(stg_error_entry\\).*") "")
               ;; interface.c:2713: `Upd_frame_info' undeclared here (not in a function)
               ;; interface.c:2713: initializer element is not constant
               ;; interface.c:2713: (near initialization for `rtsTab[32].ad')
               ((".*SymX\\(Upd_frame_info\\).*") ""))

             ;; We need to use the absolute file names here or else the linker
             ;; will complain about missing symbols.  Perhaps this could be
             ;; avoided by modifying the library search path in a way that
             ;; this old linker understands.
             (substitute* "ghc/interpreter/Makefile"
               (("-lbfd -liberty")
                (string-append (assoc-ref inputs "binutils") "/lib/libbfd.a "
                               (assoc-ref inputs "binutils") "/lib/libiberty.a")))

             (let ((bash (which "bash")))
               (substitute* '("configure.in"
                              "ghc/configure.in"
                              "ghc/rts/gmp/mpn/configure.in"
                              "ghc/rts/gmp/mpz/configure.in"
                              "ghc/rts/gmp/configure.in"
                              "distrib/configure-bin.in")
                 (("`/bin/sh") (string-append "`" bash))
                 (("SHELL=/bin/sh") (string-append "SHELL=" bash))
                 (("^#! /bin/sh") (string-append "#! " bash)))

               (substitute* '("mk/config.mk.in"
                              "ghc/rts/gmp/mpz/Makefile.in"
                              "ghc/rts/gmp/Makefile.in")
                 (("^SHELL.*=.*/bin/sh") (string-append "SHELL = " bash)))
               (substitute* "aclocal.m4"
                 (("SHELL=/bin/sh") (string-append "SHELL=" bash)))

               (setenv "CONFIG_SHELL" bash)
               (setenv "SHELL" bash))

             (setenv "CPP" (string-append (assoc-ref inputs "gcc") "/bin/cpp"))
             (invoke "autoreconf" "--verbose" "--force")))
         (add-before 'configure 'configure-gmp
           (lambda* (#:key build inputs outputs #:allow-other-keys)
             (with-directory-excursion "ghc/rts/gmp"
               (let ((bash (which "bash"))
                     (out  (assoc-ref outputs "out")))
                 (invoke bash "./configure")))))
         (replace 'configure
           (lambda* (#:key build inputs outputs #:allow-other-keys)
             (let ((bash (which "bash"))
                   (out  (assoc-ref outputs "out")))
               (invoke bash "./configure"
                       "--enable-hc-boot"
                       (string-append "--prefix=" out)
                       (string-append "--build=" build)
                       (string-append "--host=" build)))))
         (add-before 'build 'make-boot
           (lambda _
             ;; Only when building with more recent GCC
             (when #false
               ;; GCC 2.95 is fine with these comments, but GCC 4.6 is not.
               (substitute* "ghc/rts/universal_call_c.S"
                 (("^# .*") ""))
               ;; CLK_TCK has been removed
               (substitute* "ghc/interpreter/nHandle.c"
                 (("CLK_TCK") "sysconf(_SC_CLK_TCK)")))

             ;; Only when using more recent Perl
             (when #false
               (substitute* "ghc/driver/ghc-asm.prl"
                 (("local\\(\\$\\*\\) = 1;") "")
                 (("endef\\$/") "endef$/s")))

             (setenv "CPATH"
                     (string-append (getcwd) "/ghc/includes:"
                                    (getcwd) "/mk:"
                                    (or (getenv "CPATH") "")))
             (invoke "make" "boot")))
         (replace 'build
           (lambda _
             ;; TODO: since we don't have a Haskell compiler we cannot build
             ;; the standard library.  And without the standard library we
             ;; cannot build a Haskell compiler.
             ;; make[3]: *** No rule to make target 'Array.o', needed by 'libHSstd.a'.  Stop.
             ;; make[2]: *** No rule to make target 'utils/Argv.o', needed by 'hsc'.  Stop.
             (invoke "make" "all")))
         (add-after 'build 'build-hugs
           (lambda _
             (invoke "make" "-C" "ghc/interpreter")
             (invoke "make" "-C" "ghc/interpreter" "install")))
         (add-after 'install 'install-sources
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((lib (string-append (assoc-ref outputs "out") "/lib")))
               (copy-recursively "hslibs"
                                 (string-append lib "/hslibs"))
               (copy-recursively "ghc/lib"
                                 (string-append lib "/ghc/lib"))
               (copy-recursively "ghc/compiler"
                                 (string-append lib "/ghc/compiler"))
               (copy-recursively "ghc/interpreter/lib" lib)
               (install-file "ghc/interpreter/nHandle.so" lib)))))))
    (native-inputs
     `(("findutils" ,findutils)
       ("tar" ,tar)
       ("bzip2" ,bzip2)
       ("xz" ,xz)
       ("diffutils" ,diffutils)
       ("file" ,file)
       ("gawk" ,gawk)
       ("autoconf" ,autoconf-2.13)
       ("automake" ,automake)
       ("bison" ,bison) ;for parser.y

       ("make" ,gnu-make)
       ("sed" ,sed)
       ("grep" ,grep)
       ("coreutils" ,coreutils)
       ("bash" ,bash-minimal)

       ("libc" ,glibc-2.2.5)
       ;; Lazily resolve binutils-mesboot in (gnu packages commencement) to
       ;; avoid a cycle.
       ("gcc-wrapper"
        ,(module-ref (resolve-interface
                      '(gnu packages commencement))
                     'gcc-2.95-wrapper))
       ("gcc"
        ,(module-ref (resolve-interface
                      '(gnu packages commencement))
                     'gcc-mesboot0))
       ("binutils"
        ,(module-ref (resolve-interface
                      '(gnu packages commencement))
                     'binutils-mesboot))
       ("kernel-headers" ,linux-libre-headers)

       ;; TODO: Perl used to allow setting $* to enable multi-line
       ;; matching.  If we want to use a more recent Perl we need to
       ;; patch all expressions that require multi-line matching.  Hard
       ;; to tell.
       ("perl" ,perl-5.14)))
    (home-page "https://www.haskell.org/ghc")
    (synopsis "The Glasgow Haskell Compiler")
    (description
     "The Glasgow Haskell Compiler (GHC) is a state-of-the-art compiler and
interactive environment for the functional language Haskell.  The value of
this package lies in the modified build of Hugs that is linked with GHC's STG
runtime system, the RTS.  \"STG\" stands for \"spineless, tagless,
G-machine\"; it is the abstract machine designed to support nonstrict
higher-order functional languages.  Neither the compiler nor the Haskell
libraries are included in this package.")
    (license license:bsd-3)))

(define ghc-bootstrap-x86_64-7.8.4
  (origin
    (method url-fetch)
    (uri
     "https://www.haskell.org/ghc/dist/7.8.4/ghc-7.8.4-x86_64-unknown-linux-deb7.tar.xz")
    (sha256
     (base32
      "13azsl53xgj20mi1hj9x0xb32vvcvs6cpmvwx6znxhas7blh0bpn"))))

(define ghc-bootstrap-i686-7.8.4
  (origin
    (method url-fetch)
    (uri
     "https://www.haskell.org/ghc/dist/7.8.4/ghc-7.8.4-i386-unknown-linux-deb7.tar.xz")
    (sha256
     (base32
      "0wj5s435j0zgww70bj1d3f6wvnnpzlxwvwcyh2qv4qjq5z8j64kg"))))

;; 43 tests out of 3965 fail.
;;
;; Most of them do not appear to be serious:
;;
;; - some tests generate files referring to "/bin/sh" and "/bin/ls". I've not
;;   figured out how these references are generated.
;;
;; - Some tests allocate more memory than expected (ca. 3% above upper limit)
;;
;; - Some tests try to load unavailable libriries: Control.Concurrent.STM,
;;   Data.Vector, Control.Monad.State.
;;
;; - Test posix010 tries to check the existence of a user on the system:
;;   getUserEntryForName: does not exist (no such user)
(define-public ghc-7
  (package
    (name "ghc")
    (version "7.10.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://www.haskell.org/ghc/dist/"
                          version "/" name "-" version "-src.tar.xz"))
      (sha256
       (base32
        "1x8m4rp2v7ydnrz6z9g8x7z3x3d3pxhv2pixy7i7hkbqbdsp7kal"))))
    (build-system gnu-build-system)
    (supported-systems '("i686-linux" "x86_64-linux"))
    (outputs '("out" "doc"))
    (inputs
     `(("gmp" ,gmp)
       ("ncurses" ,ncurses)
       ("libffi" ,libffi)
       ("ghc-testsuite"
        ,(origin
           (method url-fetch)
           (uri (string-append
                 "https://www.haskell.org/ghc/dist/"
                 version "/" name "-" version "-testsuite.tar.xz"))
           (sha256
            (base32
             "0qp9da9ar87zbyn6wjgacd2ic1vgzbi3cklxnhsmjqyafv9qaj4b"))))))
    (native-inputs
     `(("perl" ,perl)
       ("python" ,python-2)                ; for tests (fails with python-3)
       ("ghostscript" ,ghostscript)        ; for tests
       ("patchelf" ,patchelf)
       ;; GHC is built with GHC. Therefore we need bootstrap binaries.
       ("ghc-binary"
        ,(if (string-match "x86_64" (or (%current-target-system) (%current-system)))
             ghc-bootstrap-x86_64-7.8.4
             ghc-bootstrap-i686-7.8.4))))
    (arguments
     `(#:test-target "test"
       ;; We get a smaller number of test failures by disabling parallel test
       ;; execution.
       #:parallel-tests? #f

       ;; Don't pass --build=<triplet>, because the configure script
       ;; auto-detects slightly different triplets for --host and --target and
       ;; then complains that they don't match.
       #:build #f

       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-26)
                  (srfi srfi-1))
       #:configure-flags
       (list
        (string-append "--with-gmp-libraries="
                       (assoc-ref %build-inputs "gmp") "/lib")
        (string-append "--with-gmp-includes="
                       (assoc-ref %build-inputs "gmp") "/include")
        "--with-system-libffi"
        (string-append "--with-ffi-libraries="
                       (assoc-ref %build-inputs "libffi") "/lib")
        (string-append "--with-ffi-includes="
                       (assoc-ref %build-inputs "libffi") "/include"))
       ;; FIXME: The user-guide needs dblatex, docbook-xsl and docbook-utils.
       ;; Currently we do not have the last one.
       ;; #:make-flags
       ;; (list "BUILD_DOCBOOK_HTML = YES")
       #:phases
       (let* ((ghc-bootstrap-path
               (string-append (getcwd) "/" ,name "-" ,version "/ghc-bin"))
              (ghc-bootstrap-prefix
               (string-append ghc-bootstrap-path "/usr" )))
         (alist-cons-after
          'unpack-bin 'unpack-testsuite-and-fix-bins
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (with-directory-excursion ".."
              (copy-file (assoc-ref inputs "ghc-testsuite")
                         "ghc-testsuite.tar.xz")
              (invoke "tar" "xvf" "ghc-testsuite.tar.xz"))
            (substitute*
                (list "testsuite/timeout/Makefile"
                      "testsuite/timeout/timeout.py"
                      "testsuite/timeout/timeout.hs"
                      "testsuite/tests/rename/prog006/Setup.lhs"
                      "testsuite/tests/programs/life_space_leak/life.test"
                      "libraries/process/System/Process/Internals.hs"
                      "libraries/unix/cbits/execvpe.c")
              (("/bin/sh") (which "sh"))
              (("/bin/rm") "rm"))
            #t)
          (alist-cons-after
           'unpack 'unpack-bin
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (mkdir-p ghc-bootstrap-prefix)
             (with-directory-excursion ghc-bootstrap-path
               (copy-file (assoc-ref inputs "ghc-binary")
                          "ghc-bin.tar.xz")
               (invoke "tar" "xvf" "ghc-bin.tar.xz")))
           (alist-cons-before
            'install-bin 'configure-bin
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((binaries
                      (list
                       "./utils/ghc-pwd/dist-install/build/tmp/ghc-pwd"
                       "./utils/hpc/dist-install/build/tmp/hpc"
                       "./utils/haddock/dist/build/tmp/haddock"
                       "./utils/hsc2hs/dist-install/build/tmp/hsc2hs"
                       "./utils/runghc/dist-install/build/tmp/runghc"
                       "./utils/ghc-cabal/dist-install/build/tmp/ghc-cabal"
                       "./utils/hp2ps/dist/build/tmp/hp2ps"
                       "./utils/ghc-pkg/dist-install/build/tmp/ghc-pkg"
                       "./utils/unlit/dist/build/tmp/unlit"
                       "./ghc/stage2/build/tmp/ghc-stage2"))
                     (gmp (assoc-ref inputs "gmp"))
                     (gmp-lib (string-append gmp "/lib"))
                     (gmp-include (string-append gmp "/include"))
                     (ncurses-lib
                      (dirname (search-input-file inputs "/lib/libncurses.so")))
                     (ld-so (search-input-file inputs ,(glibc-dynamic-linker)))
                     (libtinfo-dir
                      (string-append ghc-bootstrap-prefix
                                     "/lib/ghc-7.8.4/terminfo-0.4.0.0")))
                (with-directory-excursion
                    (string-append ghc-bootstrap-path "/ghc-7.8.4")
                  (setenv "CONFIG_SHELL" (which "bash"))
                  (setenv "LD_LIBRARY_PATH" gmp-lib)
                  ;; The binaries have "/lib64/ld-linux-x86-64.so.2" hardcoded.
                  (for-each
                   (cut invoke "patchelf" "--set-interpreter" ld-so <>)
                   binaries)
                  ;; The binaries include a reference to libtinfo.so.5 which
                  ;; is a subset of libncurses.so.5.  We create a symlink in a
                  ;; directory included in the bootstrap binaries rpath.
                  (mkdir-p libtinfo-dir)
                  (symlink
                   (string-append ncurses-lib "/libncursesw.so."
                                  ;; Extract "6.0" from "6.0-20170930" if a
                                  ;; dash-separated version tag exists.
                                  ,(let* ((v (package-version ncurses))
                                          (d (or (string-index v #\-)
                                                 (string-length v))))
                                     (version-major+minor (string-take v d))))
                   (string-append libtinfo-dir "/libtinfo.so.5"))

                  (setenv "PATH"
                          (string-append (getenv "PATH") ":"
                                         ghc-bootstrap-prefix "/bin"))
                  (invoke
                   (string-append (getcwd) "/configure")
                   (string-append "--prefix=" ghc-bootstrap-prefix)
                   (string-append "--with-gmp-libraries=" gmp-lib)
                   (string-append "--with-gmp-includes=" gmp-include)))))
            (alist-cons-before
             'configure 'install-bin
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (with-directory-excursion
                   (string-append ghc-bootstrap-path "/ghc-7.8.4")
                 (invoke "make" "install")))
             %standard-phases)))))))
    (native-search-paths (list (search-path-specification
                                (variable "GHC_PACKAGE_PATH")
                                (files (list
                                        (string-append "lib/ghc-" version)))
                                (file-pattern ".*\\.conf\\.d$")
                                (file-type 'directory))))
    (home-page "https://www.haskell.org/ghc")
    (synopsis "The Glasgow Haskell Compiler")
    (description
     "The Glasgow Haskell Compiler (GHC) is a state-of-the-art compiler and
interactive environment for the functional language Haskell.")
    (license license:bsd-3)))

(define-public ghc-8.0
  (package
    (name "ghc")
    (version "8.0.2")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://www.haskell.org/ghc/dist/"
                          version "/" name "-" version "-src.tar.xz"))
      (sha256
       (base32 "1c8qc4fhkycynk4g1f9hvk53dj6a1vvqi6bklqznns6hw59m8qhi"))
      (patches
       (search-patches "ghc-8.0-fall-back-to-madv_dontneed.patch"))))
    (build-system gnu-build-system)
    (supported-systems '("i686-linux" "x86_64-linux"))
    (outputs '("out" "doc"))
    (inputs
     `(("gmp" ,gmp)
       ("ncurses" ,ncurses)
       ("libffi" ,libffi)
       ("ghc-testsuite"
        ,(origin
           (method url-fetch)
           (uri (string-append
                 "https://www.haskell.org/ghc/dist/"
                 version "/" name "-" version "-testsuite.tar.xz"))
           (sha256
            (base32 "1wjc3x68l305bl1h1ijd3yhqp2vqj83lkp3kqbr94qmmkqlms8sj"))))))
    (native-inputs
     `(("perl" ,perl)
       ("python" ,python-2)                ; for tests
       ("ghostscript" ,ghostscript)        ; for tests
       ;; GHC is built with GHC.
       ("ghc-bootstrap" ,ghc-7)))
    (arguments
     `(#:test-target "test"
       ;; We get a smaller number of test failures by disabling parallel test
       ;; execution.
       #:parallel-tests? #f

       ;; Don't pass --build=<triplet>, because the configure script
       ;; auto-detects slightly different triplets for --host and --target and
       ;; then complains that they don't match.
       #:build #f

       #:configure-flags
       (list
        (string-append "--with-gmp-libraries="
                       (assoc-ref %build-inputs "gmp") "/lib")
        (string-append "--with-gmp-includes="
                       (assoc-ref %build-inputs "gmp") "/include")
        "--with-system-libffi"
        (string-append "--with-ffi-libraries="
                       (assoc-ref %build-inputs "libffi") "/lib")
        (string-append "--with-ffi-includes="
                       (assoc-ref %build-inputs "libffi") "/include")
        (string-append "--with-curses-libraries="
                       (assoc-ref %build-inputs "ncurses") "/lib")
        (string-append "--with-curses-includes="
                       (assoc-ref %build-inputs "ncurses") "/include"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-testsuite
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion ".."
               (copy-file (assoc-ref inputs "ghc-testsuite")
                          "ghc-testsuite.tar.xz")
               (zero? (system* "tar" "xvf" "ghc-testsuite.tar.xz")))))
         (add-before 'build 'fix-lib-paths
           (lambda _
             (substitute*
                 (list "libraries/process/System/Process/Posix.hs"
                       "libraries/process/tests/process001.hs"
                       "libraries/process/tests/process002.hs"
                       "libraries/unix/cbits/execvpe.c")
               (("/bin/sh") (which "sh"))
               (("/bin/ls") (which "ls")))
             #t))
         (add-before 'build 'fix-environment
           (lambda _
             (unsetenv "GHC_PACKAGE_PATH")
             (setenv "CONFIG_SHELL" (which "bash"))
             #t))
         (add-before 'check 'fix-testsuite
           (lambda _
             (substitute*
                 (list "testsuite/timeout/Makefile"
                       "testsuite/timeout/timeout.py"
                       "testsuite/timeout/timeout.hs"
                       "testsuite/tests/programs/life_space_leak/life.test")
               (("/bin/sh") (which "sh"))
               (("/bin/rm") "rm"))
             #t)))))
    (native-search-paths (list (search-path-specification
                                (variable "GHC_PACKAGE_PATH")
                                (files (list
                                        (string-append "lib/ghc-" version)))
                                (file-pattern ".*\\.conf\\.d$")
                                (file-type 'directory))))
    (home-page "https://www.haskell.org/ghc")
    (synopsis "The Glasgow Haskell Compiler")
    (description
     "The Glasgow Haskell Compiler (GHC) is a state-of-the-art compiler and
interactive environment for the functional language Haskell.")
    (license license:bsd-3)))

(define-public ghc-8.4
  (package (inherit ghc-8.0)
    (name "ghc")
    (version "8.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.haskell.org/ghc/dist/"
                           version "/" name "-" version "-src.tar.xz"))
       (sha256
        (base32 "1ch4j2asg7pr52ai1hwzykxyj553wndg7wq93i47ql4fllspf48i"))))
    (inputs
     (list gmp ncurses libffi))
    (native-inputs
     `(("perl" ,perl)
       ("python" ,python)               ; for tests
       ("ghostscript" ,ghostscript)     ; for tests
       ;; GHC 8.4.3 is built with GHC 8.
       ("ghc-bootstrap" ,ghc-8.0)
       ("ghc-testsuite"
        ,(origin
           (method url-fetch)
           (uri (string-append
                 "https://www.haskell.org/ghc/dist/"
                 version "/" name "-" version "-testsuite.tar.xz"))
           (sha256
            (base32
             "0s8lf9sxj7n89pjagi58b3fahnp34qvmwhnn0j1fbg6955vbrfj6"))))))
    (arguments
     `(#:test-target "test"
       ;; We get a smaller number of test failures by disabling parallel test
       ;; execution.
       #:parallel-tests? #f

       ;; Don't pass --build=<triplet>, because the configure script
       ;; auto-detects slightly different triplets for --host and --target and
       ;; then complains that they don't match.
       #:build #f

       #:configure-flags
       (list
        (string-append "--with-gmp-libraries="
                       (assoc-ref %build-inputs "gmp") "/lib")
        (string-append "--with-gmp-includes="
                       (assoc-ref %build-inputs "gmp") "/include")
        "--with-system-libffi"
        (string-append "--with-ffi-libraries="
                       (assoc-ref %build-inputs "libffi") "/lib")
        (string-append "--with-ffi-includes="
                       (assoc-ref %build-inputs "libffi") "/include")
        (string-append "--with-curses-libraries="
                       (assoc-ref %build-inputs "ncurses") "/lib")
        (string-append "--with-curses-includes="
                       (assoc-ref %build-inputs "ncurses") "/include"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-testsuite
           (lambda* (#:key inputs #:allow-other-keys)
             (invoke "tar" "xvf"
                     (assoc-ref inputs "ghc-testsuite")
                     "--strip-components=1")
             #t))
         ;; This phase patches the 'ghc-pkg' command so that it sorts the list
         ;; of packages in the binary cache it generates.
         (add-before 'build 'fix-ghc-pkg-nondeterminism
           (lambda _
             (substitute* "utils/ghc-pkg/Main.hs"
               (("confs = map \\(path </>\\) \\$ filter \\(\".conf\" `isSuffixOf`\\) fs")
                "confs = map (path </>) $ filter (\".conf\" `isSuffixOf`) (sort fs)"))
             #t))
         (add-after 'unpack-testsuite 'fix-shell-wrappers
           (lambda _
             (substitute* '("driver/ghci/ghc.mk"
                            "utils/mkdirhier/ghc.mk"
                            "rules/shell-wrapper.mk")
               (("echo '#!/bin/sh'")
                (format #f "echo '#!~a'" (which "sh"))))
             #t))
         ;; This is necessary because the configure system no longer uses
         ;; “AC_PATH_” but “AC_CHECK_”, setting the variables to just the
         ;; plain command names.
         (add-before 'configure 'set-target-programs
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((binutils (assoc-ref inputs "binutils"))
                   (gcc (assoc-ref inputs "gcc"))
                   (ld-wrapper (assoc-ref inputs "ld-wrapper")))
               (setenv "CC" (string-append gcc "/bin/gcc"))
               (setenv "CXX" (string-append gcc "/bin/g++"))
               (setenv "LD" (string-append ld-wrapper "/bin/ld"))
               (setenv "NM" (string-append binutils "/bin/nm"))
               (setenv "RANLIB" (string-append binutils "/bin/ranlib"))
               (setenv "STRIP" (string-append binutils "/bin/strip"))
               ;; The 'ar' command does not follow the same pattern.
               (setenv "fp_prog_ar" (string-append binutils "/bin/ar"))
               #t)))
         (add-before 'build 'fix-references
           (lambda _
             (substitute* '("testsuite/timeout/Makefile"
                            "testsuite/timeout/timeout.py"
                            "testsuite/timeout/timeout.hs"
                            "testsuite/tests/programs/life_space_leak/life.test"
                            ;; libraries
                            "libraries/process/System/Process/Posix.hs"
                            "libraries/process/tests/process001.hs"
                            "libraries/process/tests/process002.hs"
                            "libraries/unix/cbits/execvpe.c")
               (("/bin/sh") (which "sh"))
               (("/bin/ls") (which "ls"))
               (("/bin/rm") "rm"))
             #t))
         (add-before 'build 'fix-environment
           (lambda _
             (unsetenv "GHC_PACKAGE_PATH")
             (setenv "CONFIG_SHELL" (which "bash"))
             #t)))))
    (native-search-paths (list (search-path-specification
                                (variable "GHC_PACKAGE_PATH")
                                (files (list
                                        (string-append "lib/ghc-" version)))
                                (file-pattern ".*\\.conf\\.d$")
                                (file-type 'directory))))))

(define-public ghc-8.6
  (package (inherit ghc-8.4)
    (name "ghc")
    (version "8.6.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.haskell.org/ghc/dist/"
                           version "/" name "-" version "-src.tar.xz"))
       (sha256
        (base32 "0qg3zsmbk4rkwkc3jpas3zs74qaxmw4sp4v1mhsbj0a0dzls2jjd"))))
    (native-inputs
     `(;; GHC 8.6.5 must be built with GHC >= 8.2.
       ("ghc-bootstrap" ,ghc-8.4)
       ("ghc-testsuite"
        ,(origin
           (method url-fetch)
           (uri (string-append
                 "https://www.haskell.org/ghc/dist/"
                 version "/" name "-" version "-testsuite.tar.xz"))
           (patches (search-patches "ghc-testsuite-dlopen-pie.patch"))
           (sha256
            (base32
             "0pw9r91g2np3i806g2f4f8z4jfdd7mx226cmdizk4swa7av1qf91"))))
       ,@(filter (match-lambda
                   (("ghc-bootstrap" . _) #f)
                   (("ghc-testsuite" . _) #f)
                   (_ #t))
                 (package-native-inputs ghc-8.4))))
    (arguments
     (substitute-keyword-arguments (package-arguments ghc-8.4)
       ((#:make-flags make-flags ''())
        `(cons "EXTRA_RUNTEST_OPTS=--skip-perf-tests"
               ,make-flags))
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (add-after 'install 'remove-unnecessary-references
             (lambda* (#:key outputs #:allow-other-keys)
               (substitute* (find-files (string-append (assoc-ref outputs "out") "/lib/")
                                        "settings")
                 (("/gnu/store/.*/bin/(.*)" m program) program))

               ;; Remove references to "doc" output from "out" by rewriting
               ;; the "haddock-interfaces" fields and removing the optional
               ;; "haddock-html" field in the generated .conf files.
               (let ((doc (assoc-ref outputs "doc"))
                     (out (assoc-ref outputs "out")))
                 (with-fluids ((%default-port-encoding #f))
                   (for-each (lambda (config-file)
                               (substitute* config-file
                                 (("^haddock-html: .*") "\n")
                                 (((format #f "^haddock-interfaces: ~a" doc))
                                  (string-append "haddock-interfaces: " out))))
                             (find-files (string-append out "/lib") ".conf")))
                 ;; Move the referenced files to the "out" output.
                 (for-each (lambda (haddock-file)
                             (let* ((subdir (string-drop haddock-file (string-length doc)))
                                    (new    (string-append out subdir)))
                               (mkdir-p (dirname new))
                               (rename-file haddock-file new)))
                           (find-files doc "\\.haddock$")))
               #t))
           (add-after 'unpack-testsuite 'skip-tests
             (lambda _
               ;; These two tests refer to the root user, which doesn't exist
               ;; (see <https://bugs.gnu.org/36692>).
               (substitute* "libraries/unix/tests/all.T"
                 (("^test\\('T8108'") "# guix skipped: test('T8108'"))
               (substitute* "libraries/unix/tests/libposix/all.T"
                 (("^test\\('posix010'") "# guix skipped: test('posix010'"))
               #t))))))
    (native-search-paths (list (search-path-specification
                                (variable "GHC_PACKAGE_PATH")
                                (files (list
                                        (string-append "lib/ghc-" version)))
                                (file-pattern ".*\\.conf\\.d$")
                                (file-type 'directory))))))

(define-public ghc-8.8
  (package (inherit ghc-8.6)
    (name "ghc")
    (version "8.8.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.haskell.org/ghc/dist/"
                           version "/ghc-" version "-src.tar.xz"))
       (sha256
        (base32 "0bgwbxxvdn56l91bp9p5d083gzcfdi6z8l8b17qzjpr3n8w5wl7h"))))
    (native-inputs
     `(("ghc-bootstrap" ,ghc-8.6)
       ("ghc-testsuite"
        ,(origin
           (method url-fetch)
           (uri (string-append
                 "https://www.haskell.org/ghc/dist/"
                 version "/ghc-" version "-testsuite.tar.xz"))
           (patches (search-patches "ghc-testsuite-dlopen-pie.patch"))
           (sha256
            (base32
             "0c55pj2820q26rikhpf636sn4mjgqsxjrl94vsywrh79dxp3k14z"))))
       ("git" ,git-minimal/fixed)                 ; invoked during tests
       ,@(filter (match-lambda
                   (("ghc-bootstrap" . _) #f)
                   (("ghc-testsuite" . _) #f)
                   (_ #t))
                 (package-native-inputs ghc-8.6))))
    (arguments
     (substitute-keyword-arguments (package-arguments ghc-8.6)
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (add-after 'fix-references 'fix-cc-reference
             (lambda _
               (substitute* "utils/hsc2hs/Common.hs"
                 (("\"cc\"") "\"gcc\""))
               #t))
           (add-after 'unpack-testsuite 'skip-more-tests
             (lambda _
               ;; XXX: This test fails because our ld-wrapper script
               ;; mangles the response file passed to the linker.
               (substitute* "testsuite/tests/hp2ps/all.T"
                 (("^test\\('T15904'") "# guix skipped: test('T15904'"))
               #t))))))
    (native-search-paths (list (search-path-specification
                                (variable "GHC_PACKAGE_PATH")
                                (files (list
                                        (string-append "lib/ghc-" version)))
                                (file-pattern ".*\\.conf\\.d$")
                                (file-type 'directory))))))

(define-public ghc-8.10
  (package
    (inherit ghc-8.8)
    (name "ghc")
    (version "8.10.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.haskell.org/ghc/dist/"
                           version "/ghc-" version "-src.tar.xz"))
       (sha256
        (base32 "179ws2q0dinl1a39wm9j37xzwm84zfz3c5543vz8v479khigdvp3"))))
    (native-inputs
     `(("ghc-bootstrap" ,ghc-8.8)
       ("ghc-testsuite"
        ,(origin
           (method url-fetch)
           (uri (string-append
                 "https://www.haskell.org/ghc/dist/"
                 version "/ghc-" version "-testsuite.tar.xz"))
           (patches (search-patches "ghc-testsuite-dlopen-pie.patch"))
           (sha256
            (base32
             "1zl25gg6bpx5601k8h3cqnns1xfc0nqgwnh8jvn2s65ra3f2g1nz"))))
       ("git" ,git-minimal/fixed)                 ; invoked during tests
       ,@(filter (match-lambda
                   (("ghc-bootstrap" . _) #f)
                   (("ghc-testsuite" . _) #f)
                   (_ #t))
                 (package-native-inputs ghc-8.8))))
    (arguments
     (substitute-keyword-arguments (package-arguments ghc-8.8)
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (add-after 'unpack-testsuite 'patch-more-shebangs
             (lambda* (#:key inputs #:allow-other-keys)
               (let ((bash (assoc-ref inputs "bash")))
                 (substitute* '("testsuite/tests/driver/T8602/T8602.script")
                   (("/bin/sh")
                    (string-append bash "/bin/sh"))))))
           ;; Mark failing tests as broken. Reason for failure is unknown.
           (add-after 'skip-more-tests 'skip-even-more-tests
             (lambda _
               (substitute* '("testsuite/tests/driver/T16521/all.T")
                 (("extra_files" all) (string-append "[" all))
                 (("\\]\\), " all)
                  (string-append all "expect_broken(0)], ")))))
           ;; TODO: Turn this into an undconditional patch on the next rebuild.
           ,@(if (string-prefix? "i686" (or (%current-target-system)
                                                  (%current-system)))
              '((add-after 'skip-more-tests 'skip-failing-tests-i686
                 (lambda _
                   (substitute* '("testsuite/tests/codeGen/should_compile/all.T")
                     (("(test\\('T15155l', )when\\(unregisterised\\(\\), skip\\)" all before)
                      (string-append before "when(arch('i386'), skip)")))
                   ;; Unexpected failures:
                   ;;    quasiquotation/T14028.run  T14028 [bad stderr] (dyn)
                   (substitute* '("testsuite/tests/quasiquotation/all.T")
                     (("unless\\(config.have_ext_interp, skip\\),")
                      "unless(config.have_ext_interp, skip), when(arch('i386'), skip),")))))
              '())))))
    (native-search-paths (list (search-path-specification
                                (variable "GHC_PACKAGE_PATH")
                                (files (list
                                        (string-append "lib/ghc-" version)))
                                (file-pattern ".*\\.conf\\.d$")
                                (file-type 'directory))))))

;; Versions newer than ghc defined below (i.e. the compiler
;; haskell-build-system uses) should use ghc-next as their name to
;; ensure ghc (without version specification) and ghc-* packages are
;; always compatible. See https://issues.guix.gnu.org/issue/47335.

(define-public ghc-8 ghc-8.10)

(define-public ghc ghc-8)

;;; haskell.scm ends here
