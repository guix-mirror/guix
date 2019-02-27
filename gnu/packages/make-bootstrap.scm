;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2019 Marius Bakke <mbakke@fastmail.com>
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

(define-module (gnu packages make-bootstrap)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:select (gpl3+))
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system gnu)
  #:use-module ((gnu packages) #:select (search-patch))
  #:use-module (gnu packages base)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages libunistring)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages hurd)
  #:use-module (gnu packages mes)
  #:use-module (gnu packages multiprecision)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (%bootstrap-binaries-tarball
            %linux-libre-headers-bootstrap-tarball
            %binutils-bootstrap-tarball
            %glibc-bootstrap-tarball
            %gcc-bootstrap-tarball
            %guile-bootstrap-tarball
            %mescc-tools-bootstrap-tarball
            %mes-bootstrap-tarball
            %bootstrap-tarballs

            %guile-static-stripped))

;;; Commentary:
;;;
;;; This module provides tools to build tarballs of the "bootstrap binaries"
;;; used in (gnu packages bootstrap).  These statically-linked binaries are
;;; taken for granted and used as the root of the whole bootstrap procedure.
;;;
;;; Code:

(define* (glibc-for-bootstrap #:optional (base glibc))
  "Return a libc deriving from BASE whose `system' and `popen' functions looks
for `sh' in $PATH, and without nscd, and with static NSS modules."
  (package (inherit base)
    (source (origin (inherit (package-source base))
              (patches (cons (search-patch "glibc-bootstrap-system.patch")
                             (origin-patches (package-source base))))))
    (arguments
     (substitute-keyword-arguments (package-arguments base)
       ((#:configure-flags flags)
        ;; Arrange so that getaddrinfo & co. do not contact the nscd,
        ;; and can use statically-linked NSS modules.
        `(cons* "--disable-nscd" "--disable-build-nscd"
                "--enable-static-nss"
                ,flags))))

    ;; Remove the 'debug' output to allow bit-reproducible builds (when the
    ;; 'debug' output is used, ELF files end up with a .gnu_debuglink, which
    ;; includes a CRC of the corresponding debugging symbols; those symbols
    ;; contain store file names, so the CRC changes at every rebuild.)
    (outputs (delete "debug" (package-outputs base)))))

(define (package-with-relocatable-glibc p)
  "Return a variant of P that uses the libc as defined by
`glibc-for-bootstrap'."

  (define (cross-bootstrap-libc)
    (let ((target (%current-target-system)))
      (glibc-for-bootstrap
       ;; `cross-libc' already returns a cross libc, so clear
       ;; %CURRENT-TARGET-SYSTEM.
       (parameterize ((%current-target-system #f))
         (cross-libc target)))))

  ;; Standard inputs with the above libc and corresponding GCC.

  (define (inputs)
    (if (%current-target-system)                ; is this package cross built?
        `(("cross-libc" ,(cross-bootstrap-libc)))
        '()))

  (define (native-inputs)
    (if (%current-target-system)
        (let ((target (%current-target-system)))
          `(("cross-gcc"      ,(cross-gcc target
                                          #:xbinutils (cross-binutils target)
                                          #:libc (cross-bootstrap-libc)))
            ("cross-binutils" ,(cross-binutils target))
            ,@(%final-inputs)))
        `(("libc" ,(glibc-for-bootstrap))
          ("libc:static" ,(glibc-for-bootstrap) "static")
          ("gcc" ,(package (inherit gcc)
                    (outputs '("out"))  ;all in one so libgcc_s is easily found
                    (inputs
                     `(;; Distinguish the name so we can refer to it below.
                       ("bootstrap-libc" ,(glibc-for-bootstrap))
                       ("libc:static" ,(glibc-for-bootstrap) "static")
                       ,@(package-inputs gcc)))
                    (arguments
                     (substitute-keyword-arguments (package-arguments gcc)
                       ((#:phases phases)
                        `(modify-phases ,phases
                           (add-before 'configure 'treat-glibc-as-system-header
                             (lambda* (#:key inputs #:allow-other-keys)
                               (let ((libc (assoc-ref inputs "bootstrap-libc")))
                                 ;; GCCs build processes requires that the libc
                                 ;; we're building against is on the system header
                                 ;; search path.
                                 (for-each (lambda (var)
                                             (setenv var (string-append libc "/include")))
                                           '("C_INCLUDE_PATH" "CPLUS_INCLUDE_PATH"))
                                 #t)))))))))
          ,@(fold alist-delete (%final-inputs) '("libc" "gcc")))))

  (package-with-explicit-inputs p inputs
                                (current-source-location)
                                #:native-inputs native-inputs))

(define %static-inputs
  ;; Packages that are to be used as %BOOTSTRAP-INPUTS.
  (let ((coreutils (package (inherit coreutils)
                      (arguments
                       `(#:configure-flags
                         '("--disable-nls"
                           "--disable-silent-rules"
                           "--enable-no-install-program=stdbuf,libstdbuf.so"
                           "CFLAGS=-Os -g0"        ; smaller, please
                           "LDFLAGS=-static -pthread")
                         #:tests? #f   ; signal-related Gnulib tests fail
                         ,@(package-arguments coreutils)))

                      ;; Remove optional dependencies such as GMP.  Keep Perl
                      ;; except if it's missing (which is the case when
                      ;; cross-compiling).
                      (inputs (match (assoc "perl" (package-inputs coreutils))
                                (#f '())
                                (x  (list x))))

                      ;; Remove the 'debug' output (see above for the reason.)
                      (outputs '("out"))))
        (bzip2 (package (inherit bzip2)
                 (arguments
                  (substitute-keyword-arguments (package-arguments bzip2)
                    ((#:phases phases)
                     `(modify-phases ,phases
                        (add-before 'build 'dash-static
                          (lambda _
                            (substitute* "Makefile"
                              (("^LDFLAGS[[:blank:]]*=.*$")
                               "LDFLAGS = -static"))
                            #t))))))))
        (xz (package (inherit xz)
              (outputs '("out"))
              (arguments
               `(#:strip-flags '("--strip-all")
                 #:phases (modify-phases %standard-phases
                            (add-before 'configure 'static-executable
                              (lambda _
                                ;; Ask Libtool for a static executable.
                                (substitute* "src/xz/Makefile.in"
                                  (("^xz_LDADD =")
                                   "xz_LDADD = -all-static"))
                                #t)))))))
        (gawk (package (inherit gawk)
                (source (origin (inherit (package-source gawk))
                          (patches (cons (search-patch "gawk-shell.patch")
                                         (origin-patches
                                          (package-source gawk))))))
                (arguments
                 `(;; Starting from gawk 4.1.0, some of the tests for the
                   ;; plug-in mechanism just fail on static builds:
                   ;;
                   ;; ./fts.awk:1: error: can't open shared library `filefuncs' for reading (No such file or directory)
                   #:tests? #f

                   ,@(substitute-keyword-arguments (package-arguments gawk)
                       ((#:phases phases)
                        `(modify-phases ,phases
                           (add-before 'configure 'no-export-dynamic
                             (lambda _
                               ;; Since we use `-static', remove
                               ;; `-export-dynamic'.
                               (substitute* "configure"
                                 (("-Wl,-export-dynamic") ""))
                               #t)))))))
                (inputs (if (%current-target-system)
                            `(("bash" ,static-bash))
                            '()))))
	(tar (package (inherit tar)
	       (arguments
                (substitute-keyword-arguments (package-arguments tar)
                  ((#:phases phases)
                   `(modify-phases ,phases
                      (replace 'set-shell-file-name
                        (lambda _
                          ;; Do not use "/bin/sh" to run programs; see
                          ;; <http://lists.gnu.org/archive/html/guix-devel/2016-09/msg02272.html>.
                          (substitute* "src/system.c"
                            (("/bin/sh") "sh")
                            (("execv ") "execvp "))
                          #t))))))))
        ;; We don't want to retain a reference to /gnu/store in the bootstrap
        ;; versions of egrep/fgrep, so we remove the custom phase added since
        ;; grep@2.25. The effect is 'egrep' and 'fgrep' look for 'grep' in
        ;; $PATH.
        (grep (package
                (inherit grep)
                (inputs '())                   ;remove PCRE, which is optional
                (arguments
                 (substitute-keyword-arguments (package-arguments grep)
                   ((#:phases phases)
                    `(modify-phases ,phases
                       (delete 'fix-egrep-and-fgrep)))))))
        (finalize (compose static-package
                           package-with-relocatable-glibc)))
    `(,@(map (match-lambda
              ((name package)
               (list name (finalize package))))
             `(("tar" ,tar)
               ("gzip" ,gzip)
               ("bzip2" ,bzip2)
               ("xz" ,xz)
               ("patch" ,patch)
               ("coreutils" ,coreutils)
               ("sed" ,sed)
               ("grep" ,grep)
               ("gawk" ,gawk)))
      ("bash" ,static-bash))))

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

            ;; But of course, there are exceptions to this rule.
            (let ((grep (assoc-ref %build-inputs "grep")))
              (install-file (string-append grep "/bin/fgrep") bin)
              (install-file (string-append grep "/bin/egrep") bin))

            ;; Clear references to the store path.
            (for-each remove-store-references
                      (directory-contents bin))

            (with-directory-excursion bin
              ;; Programs such as Perl's build system want these aliases.
              (symlink "bash" "sh")
              (symlink "gawk" "awk"))

            #t)))))
    (synopsis "Statically-linked bootstrap binaries")
    (description
     "Binaries used to bootstrap the distribution.")
    (license gpl3+)
    (home-page #f)))

(define %linux-libre-headers-stripped
  ;; The subset of Linux-Libre-Headers that we need.
  (package (inherit linux-libre-headers)
    (name (string-append (package-name linux-libre-headers) "-stripped"))
    (build-system trivial-build-system)
    (outputs '("out"))
    (arguments
     `(#:modules ((guix build utils)
                  (guix build make-bootstrap))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (guix build make-bootstrap))

         (let* ((in  (assoc-ref %build-inputs "linux-libre-headers"))
                (out (assoc-ref %outputs "out")))
           (copy-linux-headers out in)
           #t))))
    (inputs `(("linux-libre-headers" ,linux-libre-headers)))))

(define %binutils-static
  ;; Statically-linked Binutils.
  (package (inherit binutils)
    (name "binutils-static")
    (arguments
     `(#:configure-flags (cons "--disable-gold"
                               ,(match (memq #:configure-flags
                                             (package-arguments binutils))
                                  ((#:configure-flags flags _ ...)
                                   flags)))
       #:strip-flags '("--strip-all")
       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'all-static
                    (lambda _
                      ;; The `-all-static' libtool flag can only be passed
                      ;; after `configure', since configure tests don't use
                      ;; libtool, and only for executables built with libtool.
                      (substitute* '("binutils/Makefile.in"
                                     "gas/Makefile.in"
                                     "ld/Makefile.in")
                        (("^LDFLAGS =(.*)$" line)
                         (string-append line
                                        "\nAM_LDFLAGS = -static -all-static\n")))
                      #t)))))))

(define %binutils-static-stripped
  ;; The subset of Binutils that we need.
  (package (inherit %binutils-static)
    (name (string-append (package-name %binutils-static) "-stripped"))
    (build-system trivial-build-system)
    (outputs '("out"))
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

(define (%glibc-stripped)
  ;; GNU libc's essential shared libraries, dynamic linker, and headers,
  ;; with all references to store directories stripped.  As a result,
  ;; libc.so is unusable and need to be patched for proper relocation.
  (let ((glibc (glibc-for-bootstrap)))
    (package (inherit glibc)
      (name "glibc-stripped")
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils)
                    (guix build make-bootstrap))
         #:builder
         (begin
           (use-modules (guix build make-bootstrap))
           (make-stripped-libc (assoc-ref %outputs "out")
                               (assoc-ref %build-inputs "libc")
                               (assoc-ref %build-inputs "kernel-headers")))))
      (inputs `(("kernel-headers"
                 ,(if (or (and (%current-target-system)
                               (hurd-triplet? (%current-target-system)))
                          (string-suffix? "-hurd" (%current-system)))
                      gnumach-headers
                      linux-libre-headers))
                ("libc" ,(let ((target (%current-target-system)))
                           (if target
                               (glibc-for-bootstrap
                                (parameterize ((%current-target-system #f))
                                  (cross-libc target)))
                               glibc)))))
      (native-inputs '())
      (propagated-inputs '())

      ;; Only one output.
      (outputs '("out")))))

(define %gcc-static
  ;; A statically-linked GCC, with stripped-down functionality.
  (package-with-relocatable-glibc
   (package (inherit gcc)
     (name "gcc-static")
     (outputs '("out"))                           ; all in one
     (arguments
      `(#:modules ((guix build utils)
                   (guix build gnu-build-system)
                   (srfi srfi-1)
                   (srfi srfi-26)
                   (ice-9 regex))
        ,@(substitute-keyword-arguments (package-arguments gcc)
            ((#:guile _) #f)
            ((#:implicit-inputs? _) #t)
            ((#:configure-flags flags)
             `(append (list
                       ;; We don't need a full bootstrap here.
                       "--disable-bootstrap"

                       ;; Make sure '-static' is passed where it matters.
                       "--with-stage1-ldflags=-static"

                       ;; GCC 4.8+ requires a C++ compiler and library.
                       "--enable-languages=c,c++"

                       ;; Make sure gcc-nm doesn't require liblto_plugin.so.
                       "--disable-lto"

                       "--disable-shared"
                       "--disable-plugin"
                       "--disable-libmudflap"
                       "--disable-libatomic"
                       "--disable-libsanitizer"
                       "--disable-libitm"
                       "--disable-libgomp"
                       "--disable-libcilkrts"
                       "--disable-libvtv"
                       "--disable-libssp"
                       "--disable-libquadmath")
                      (remove (cut string-match "--(.*plugin|enable-languages)" <>)
                              ,flags)))
            ((#:phases phases)
             `(modify-phases ,phases
                (add-after 'pre-configure 'remove-lgcc_s
                  (lambda _
                    ;; Remove the '-lgcc_s' added to GNU_USER_TARGET_LIB_SPEC in
                    ;; the 'pre-configure phase of our main gcc package, because
                    ;; that shared library is not present in this static gcc.  See
                    ;; <https://lists.gnu.org/archive/html/guix-devel/2015-01/msg00008.html>.
                    (substitute* (cons "gcc/config/rs6000/sysv4.h"
                                       (find-files "gcc/config"
                                                   "^gnu-user.*\\.h$"))
                      ((" -lgcc_s}}") "}}"))
                    #t)))))))
     (inputs
      `(("zlib:static" ,zlib "static")
        ,@(package-inputs gcc)))
     (native-inputs
      (if (%current-target-system)
          `(;; When doing a Canadian cross, we need GMP/MPFR/MPC both
            ;; as target inputs and as native inputs; the latter is
            ;; needed when building build-time tools ('genconstants',
            ;; etc.)  Failing to do that leads to misdetections of
            ;; declarations by 'gcc/configure', and eventually to
            ;; duplicate declarations as reported in
            ;; <https://gcc.gnu.org/bugzilla/show_bug.cgi?id=59217>.
            ("gmp-native" ,gmp)
            ("mpfr-native" ,mpfr)
            ("mpc-native" ,mpc)
            ,@(package-native-inputs gcc))
          (package-native-inputs gcc))))))

(define %gcc-stripped
  ;; The subset of GCC files needed for bootstrap.
  (package (inherit gcc)
    (name "gcc-stripped")
    (build-system trivial-build-system)
    (source #f)
    (outputs '("out"))                            ;only one output
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
                (includedir (string-append out "/include"))
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

           ;; Starting from GCC 4.8, helper programs built natively
           ;; (‘genchecksum’, ‘gcc-nm’, etc.) rely on C++ headers.
           (copy-recursively (string-append gcc "/include/c++")
                             (string-append includedir "/c++"))

           ;; For native builds, check whether the binaries actually work.
           ,@(if (%current-target-system)
                 '()
                 '((for-each (lambda (prog)
                               (invoke (string-append gcc "/bin/" prog)
                                       "--version"))
                             '("gcc" "g++" "cpp"))))

           #t))))
    (inputs `(("gcc" ,%gcc-static)))))

(define %mescc-tools-static
  ;; A statically linked MesCC Tools for bootstrap.
  (package
    (inherit mescc-tools)
    (name "mescc-tools-static")
    (arguments
     `(#:system "i686-linux"
       ,@(substitute-keyword-arguments (package-arguments mescc-tools)
           ((#:make-flags flags)
            `(cons "CC=gcc -static" ,flags)))))))

(define-public %mes-minimal-stripped
  ;; A minimal Mes without documentation dependencies, for bootstrap.
  (let ((triplet "i686-unknown-linux-gnu"))
    (package
      (inherit mes)
      (name "mes-minimal-stripped")
      (native-inputs
       `(("guile" ,guile-2.2)))
      (arguments
       `(#:system "i686-linux"
         #:strip-binaries? #f
         #:configure-flags '("--mes")
         #:phases
         (modify-phases %standard-phases
           (add-after 'install 'strip-install
             (lambda _
               (let* ((out (assoc-ref %outputs "out"))
                      (share (string-append out "/share")))
                 (delete-file-recursively (string-append out "/lib/guile"))
                 (delete-file-recursively (string-append share "/guile"))
                 (delete-file-recursively (string-append share "/mes/scaffold"))
                 (for-each
                  delete-file
                  (find-files (string-append share  "/mes/lib")
                              "\\.(h|c)")))))))))))

(define %guile-static
  ;; A statically-linked Guile that is relocatable--i.e., it can search
  ;; .scm and .go files relative to its installation directory, rather
  ;; than in hard-coded configure-time paths.
  (let* ((patches (cons* (search-patch "guile-relocatable.patch")
                         (search-patch "guile-2.2-default-utf8.patch")
                         (search-patch "guile-linux-syscalls.patch")
                         (origin-patches (package-source guile-2.2))))
         (source  (origin (inherit (package-source guile-2.2))
                    (patches patches)))
         (guile (package (inherit guile-2.2)
                  (name (string-append (package-name guile-2.2) "-static"))
                  (source source)
                  (synopsis "Statically-linked and relocatable Guile")

                  ;; Remove the 'debug' output (see above for the reason.)
                  (outputs (delete "debug" (package-outputs guile-2.2)))

                  (inputs
                   `(("libunistring:static" ,libunistring "static")
                     ,@(package-inputs guile-2.2)))

                  (propagated-inputs
                   `(("bdw-gc" ,libgc)
                     ,@(alist-delete "bdw-gc"
                                     (package-propagated-inputs guile-2.2))))
                  (arguments
                   (substitute-keyword-arguments (package-arguments guile-2.2)
                     ((#:configure-flags flags '())
                      ;; When `configure' checks for ltdl availability, it
                      ;; doesn't try to link using libtool, and thus fails
                      ;; because of a missing -ldl.  Work around that.
                      ''("LDFLAGS=-ldl"))
                     ((#:phases phases '%standard-phases)
                      `(modify-phases ,phases

                         ;; Do not record the absolute file name of 'sh' in
                         ;; (ice-9 popen).  This makes 'open-pipe' unusable in
                         ;; a build chroot ('open-pipe*' is fine) but avoids
                         ;; keeping a reference to Bash.
                         (delete 'pre-configure)

                         (add-before 'configure 'static-guile
                           (lambda _
                             (substitute* "libguile/Makefile.in"
                               ;; Create a statically-linked `guile'
                               ;; executable.
                               (("^guile_LDFLAGS =")
                                "guile_LDFLAGS = -all-static")

                               ;; Add `-ldl' *after* libguile-2.2.la.
                               (("^guile_LDADD =(.*)$" _ ldadd)
                                (string-append "guile_LDADD = "
                                               (string-trim-right ldadd)
                                               " -ldl\n")))))))
                     ((#:tests? _ #f)
                      ;; There are uses of `dynamic-link' in
                      ;; {foreign,coverage}.test that don't fly here.
                      #f))))))
    (package-with-relocatable-glibc (static-package guile))))

(define %guile-static-stripped
  ;; A stripped static Guile binary, for use during bootstrap.
  (package (inherit %guile-static)
    (name "guile-static-stripped")
    (build-system trivial-build-system)
    (arguments
     ;; The end result should depend on nothing but itself.
     `(#:allowed-references ("out")
       #:modules ((guix build utils))
       #:builder
       (let ()
         (use-modules (guix build utils))

         (let* ((in     (assoc-ref %build-inputs "guile"))
                (out    (assoc-ref %outputs "out"))
                (guile1 (string-append in "/bin/guile"))
                (guile2 (string-append out "/bin/guile")))
           (mkdir-p (string-append out "/share/guile/2.2"))
           (copy-recursively (string-append in "/share/guile/2.2")
                             (string-append out "/share/guile/2.2"))

           (mkdir-p (string-append out "/lib/guile/2.2/ccache"))
           (copy-recursively (string-append in "/lib/guile/2.2/ccache")
                             (string-append out "/lib/guile/2.2/ccache"))

           (mkdir (string-append out "/bin"))
           (copy-file guile1 guile2)

           ;; Verify that the relocated Guile works.
           ,@(if (%current-target-system)
                 '()
                 '((invoke guile2 "--version")))

           ;; Strip store references.
           (remove-store-references guile2)

           ;; Verify that the stripped Guile works.  If it aborts, it could be
           ;; that it tries to open iconv descriptors and fails because libc's
           ;; iconv data isn't available (see `guile-default-utf8.patch'.)
           ,@(if (%current-target-system)
                 '()
                 '((invoke guile2 "--version")))

           #t))))
    (inputs `(("guile" ,%guile-static)))
    (outputs '("out"))
    (synopsis "Minimal statically-linked and relocatable Guile")))

(define (tarball-package pkg)
  "Return a package containing a tarball of PKG."
  (package (inherit pkg)
    (name (string-append (package-name pkg) "-tarball"))
    (build-system trivial-build-system)
    (native-inputs `(("tar" ,tar)
                     ("xz" ,xz)))
    (inputs `(("input" ,pkg)))
    (arguments
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
               (invoke "tar" "cJvf"
                       (string-append out "/"
                                      ,name "-" ,version
                                      "-"
                                      ,(or (%current-target-system)
                                           (%current-system))
                                      ".tar.xz")
                       "."
                       ;; avoid non-determinism in the archive
                       "--sort=name" "--mtime=@0"
                       "--owner=root:0" "--group=root:0")))))))))

(define %bootstrap-binaries-tarball
  ;; A tarball with the statically-linked bootstrap binaries.
  (tarball-package %static-binaries))

(define %linux-libre-headers-bootstrap-tarball
  ;; A tarball with the statically-linked Linux-Libre-Headers programs.
  (tarball-package %linux-libre-headers-stripped))

(define %binutils-bootstrap-tarball
  ;; A tarball with the statically-linked Binutils programs.
  (tarball-package %binutils-static-stripped))

(define (%glibc-bootstrap-tarball)
  ;; A tarball with GNU libc's shared libraries, dynamic linker, and headers.
  (tarball-package (%glibc-stripped)))

(define %gcc-bootstrap-tarball
  ;; A tarball with a dynamic-linked GCC and its headers.
  (tarball-package %gcc-stripped))

(define %guile-bootstrap-tarball
  ;; A tarball with the statically-linked, relocatable Guile.
  (tarball-package %guile-static-stripped))

(define %mescc-tools-bootstrap-tarball
  ;; A tarball with MesCC binary seed.
  (tarball-package %mescc-tools-static))

(define %mes-bootstrap-tarball
  ;; A tarball with Mes ASCII Seed and binary Mes C Library.
  (tarball-package %mes-minimal-stripped))

(define %bootstrap-tarballs
  ;; A single derivation containing all the bootstrap tarballs, for
  ;; convenience.
  (package
    (name "bootstrap-tarballs")
    (version "0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (let ((out (assoc-ref %outputs "out")))
         (use-modules (guix build utils)
                      (ice-9 match)
                      (srfi srfi-26))

         (setvbuf (current-output-port) _IOLBF)
         (mkdir out)
         (chdir out)
         (for-each (match-lambda
                    ((name . directory)
                     (for-each (lambda (file)
                                 (format #t "~a -> ~a~%" file out)
                                 (symlink file (basename file)))
                               (find-files directory "\\.tar\\."))))
                   %build-inputs)
         #t)))
    (inputs `(("guile-tarball" ,%guile-bootstrap-tarball)
              ,@(match (%current-system)
                  ((or "i686-linux" "x86_64-linux")
                   `(("bootstrap-mescc-tools" ,%mescc-tools-bootstrap-tarball)
                     ("bootstrap-mes" ,%mes-bootstrap-tarball)
                     ("bootstrap-linux-libre-headers"
                      ,%linux-libre-headers-bootstrap-tarball)))
                  (_ `(("gcc-tarball" ,%gcc-bootstrap-tarball)
                       ("binutils-tarball" ,%binutils-bootstrap-tarball)
                       ("glibc-tarball" ,(%glibc-bootstrap-tarball)))))
              ("coreutils&co-tarball" ,%bootstrap-binaries-tarball)))
    (synopsis "Tarballs containing all the bootstrap binaries")
    (description synopsis)
    (home-page #f)
    (license gpl3+)))

;;; make-bootstrap.scm ends here
