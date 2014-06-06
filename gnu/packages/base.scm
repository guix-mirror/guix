;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
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

(define-module (gnu packages base)
  #:use-module ((guix licenses)
                #:select (gpl3+ lgpl2.0+ public-domain))
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match))

;;; Commentary:
;;;
;;; Base packages of the Guix-based GNU user-land software distribution.
;;;
;;; Code:

(define-public hello
  (package
   (name "hello")
   (version "2.9")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/hello/hello-" version
                                ".tar.gz"))
            (sha256
             (base32 "19qy37gkasc4csb1d3bdiz9snn8mir2p3aj0jgzmfv0r2hi7mfzc"))))
   (build-system gnu-build-system)
   (synopsis "Hello, GNU world: An example GNU package")
   (description
    "GNU Hello prints the message \"Hello, world!\" and then exits.  It
serves as an example of standard GNU coding practices.  As such, it supports
command-line arguments, multiple languages, and so on.")
   (home-page "http://www.gnu.org/software/hello/")
   (license gpl3+)))

(define-public grep
  (package
   (name "grep")
   (version "2.19")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/grep/grep-"
                                version ".tar.xz"))
            (sha256
             (base32
              "0kf3xvcg2p6lxyzk4ha1x40a0pk2wqa3kkfrcmvazz4cwidjk233"))))
   (build-system gnu-build-system)
   (synopsis "Print lines matching a pattern")
   (description
    "grep is a tool for finding text inside files.  Text is found by
matching a pattern provided by the user in one or many files.  The pattern
may be provided as a basic or extended regular expression, or as fixed
strings.  By default, the matching text is simply printed to the screen,
however the output can be greatly customized to include, for example, line
numbers.  GNU grep offers many extensions over the standard utility,
including, for example, recursive directory searching.")
   (license gpl3+)
   (home-page "http://www.gnu.org/software/grep/")))

(define-public sed
  (package
   (name "sed")
   (version "4.2.2")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/sed/sed-" version
                                ".tar.bz2"))
            (sha256
             (base32
              "1myvrmh99jsvk7v3d7crm0gcrq51hmmm1r2kjyyci152in1x2j7h"))))
   (build-system gnu-build-system)
   (synopsis "Stream editor")
   (arguments
    (if (%current-target-system)
        '()
        `(#:phases (alist-cons-before
                    'patch-source-shebangs 'patch-test-suite
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((bash (assoc-ref inputs "bash")))
                        (patch-makefile-SHELL "testsuite/Makefile.tests")
                        (substitute* '("testsuite/bsd.sh"
                                       "testsuite/bug-regex9.c")
                          (("/bin/sh")
                           (string-append bash "/bin/bash")))))
                    %standard-phases))))
   (description
    "Sed is a non-interactive, text stream editor.  It receives a text
input from a file or from standard input and it then applies a series of text
editing commands to the stream and prints its output to standard output.  It
is often used for substituting text patterns in a stream.  The GNU
implementation offers several extensions over the standard utility.")
   (license gpl3+)
   (home-page "http://www.gnu.org/software/sed/")))

(define-public tar
  (package
   (name "tar")
   (version "1.27.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/tar/tar-"
                                version ".tar.bz2"))
            (sha256
             (base32
              "1iip0fk0wqhxb0jcwphz43r4fxkx1y7mznnhmlvr618jhp7b63wv"))))
   (build-system gnu-build-system)
   (synopsis "Managing tar archives")
   (description
    "Tar provides the ability to create tar archives, as well as the
ability to extract, update or list files in an existing archive.  It is
useful for combining many files into one larger file, while maintaining
directory structure and file information such as permissions and
creation/modification dates.  GNU tar offers many extensions over the
standard utility.")
   (license gpl3+)
   (home-page "http://www.gnu.org/software/tar/")))

(define-public patch
  (package
   (name "patch")
   (version "2.7.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/patch/patch-"
                                version ".tar.xz"))
            (sha256
             (base32
              "1sqckf560pzwgniy00vcpdv2c9c11s4cmhlm14yqgg8avd3bl94i"))))
   (build-system gnu-build-system)
   (native-inputs '())                      ; FIXME: needs `ed' for the tests
   (arguments
    '(#:tests? #f)
    ;; TODO: When cross-compiling, add this:
    ;;  '(#:configure-flags '("ac_cv_func_strnlen_working=yes"))
    )
   (synopsis "Apply differences to originals, with optional backups")
   (description
    "Patch is a program that applies changes to files based on differences
laid out as by the program \"diff\".  The changes may be applied to one or more
files depending on the contents of the diff file.  It accepts several
different diff formats.  It may also be used to revert previously applied
differences.")
   (license gpl3+)
   (home-page "http://savannah.gnu.org/projects/patch/")))

(define-public diffutils
  (package
   (name "diffutils")
   (version "3.3")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/diffutils/diffutils-"
                                version ".tar.xz"))
            (sha256
             (base32
              "1761vymxbp4wb5rzjvabhdkskk95pghnn67464byvzb5mfl8jpm2"))))
   (build-system gnu-build-system)
   (synopsis "Comparing and merging files")
   (description
    "GNU Diffutils is a package containing tools for finding the
differences between files.  The \"diff\" command is used to show how two files
differ, while \"cmp\" shows the offsets and line numbers where they differ. 
\"diff3\" allows you to compare three files.  Finally, \"sdiff\" offers an
interactive means to merge two files.")
   (license gpl3+)
   (home-page "http://www.gnu.org/software/diffutils/")))

(define-public findutils
  (package
   (name "findutils")
   (version "4.4.2")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/findutils/findutils-"
                                version ".tar.gz"))
            (sha256
             (base32
              "0amn0bbwqvsvvsh6drfwz20ydc2czk374lzw5kksbh6bf78k4ks3"))
            (patches (list (search-patch "findutils-absolute-paths.patch")))))
   (build-system gnu-build-system)
   (arguments
    ;; Work around cross-compilation failure.
    ;; See <http://savannah.gnu.org/bugs/?27299#comment1>.
    (if (%current-target-system)
        '(#:configure-flags '("gl_cv_func_wcwidth_works=yes"))
        '()))
   (synopsis "Operating on files matching given criteria")
   (description
    "Findutils supplies the basic file directory searching utilities of the
GNU system.  It consists of two primary searching utilities: \"find\"
recursively searches for files in a directory according to given criteria and
\"locate\" lists files in a database that match a query.  Two auxiliary tools
are included: \"updatedb\" updates the file name database and \"xargs\" may be
used to apply commands with arbitrarily long arguments.")
   (license gpl3+)
   (home-page "http://www.gnu.org/software/findutils/")))

(define-public coreutils
  (package
   (name "coreutils")
   (version "8.22")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/coreutils/coreutils-"
                                version ".tar.xz"))
            (sha256
             (base32
              "04hjzzv434fb8ak3hh3dyhdvg3hqjjwvjmjxqzk1gh2jh6cr8gjv"))
            (patches (list (search-patch "coreutils-dummy-man.patch")
                           ;; TODO: remove this patch for >= 8.23
                           (search-patch "coreutils-skip-nohup.patch")))))
   (build-system gnu-build-system)
   (inputs `(("acl"  ,acl)                        ; TODO: add SELinux
             ("gmp"  ,gmp)))
   (native-inputs
    ;; Perl is needed to run tests in native builds, and to run the bundled
    ;; copy of help2man.  However, don't pass it when cross-compiling since
    ;; that would lead it to try to run programs to get their '--help' output
    ;; for help2man.
    (if (%current-target-system)
        '()
        `(("perl" ,perl))))
   (outputs '("out" "debug"))
   (arguments
    `(#:parallel-build? #f            ; help2man may be called too early
      #:phases (alist-cons-before
                'build 'patch-shell-references
                (lambda* (#:key inputs #:allow-other-keys)
                  (let ((bash (assoc-ref inputs "bash")))
                    (substitute* (cons "src/split.c"
                                       (find-files "gnulib-tests"
                                                   "\\.c$"))
                      (("/bin/sh")
                       (format #f "~a/bin/sh" bash)))
                    (substitute* (find-files "tests" "\\.sh$")
                      (("#!/bin/sh")
                       (format #f "#!~a/bin/bash" bash)))))
                %standard-phases)))
   (synopsis "Core GNU utilities (file, text, shell)")
   (description
    "GNU Coreutils includes all of the basic command-line tools that are
expected in a POSIX system.  These provide the basic file, shell and text
manipulation functions of the GNU system.  Most of these tools offer extended
functionality beyond that which is outlined in the POSIX standard.")
   (license gpl3+)
   (home-page "http://www.gnu.org/software/coreutils/")))

(define-public gnu-make
  (package
   (name "make")
   (version "4.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/make/make-" version
                                ".tar.bz2"))
            (sha256
             (base32
              "1nyvn8mknw0mf7727lprva3lisl1y0n03lvar342rrpdmz3qc1p6"))
            (patches (list (search-patch "make-impure-dirs.patch")))))
   (build-system gnu-build-system)
   (native-inputs `(("pkg-config", pkg-config)))  ; to detect Guile
   (inputs `(("guile" ,guile-2.0)))
   (outputs '("out" "debug"))
   (arguments
    '(#:phases (alist-cons-before
                'build 'set-default-shell
                (lambda* (#:key inputs #:allow-other-keys)
                  ;; Change the default shell from /bin/sh.
                  (let ((bash (assoc-ref inputs "bash")))
                    (substitute* "job.c"
                      (("default_shell\\[\\] =.*$")
                       (format #f "default_shell[] = \"~a/bin/bash\";\n"
                               bash)))))
                %standard-phases)))
   (synopsis "Remake files automatically")
   (description
    "Make is a program that is used to control the production of
executables or other files from their source files.  The process is
controlled from a Makefile, in which the developer specifies how each file is
generated from its source.  It has powerful dependency resolution and the
ability to determine when files have to be regenerated after their sources
change.  GNU make offers many powerful extensions over the standard utility.")
   (license gpl3+)
   (home-page "http://www.gnu.org/software/make/")))

(define-public binutils
  (package
   (name "binutils")
   (version "2.24")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/binutils/binutils-"
                                version ".tar.bz2"))
            (sha256
             (base32
              "0ds1y7qa0xqihw4ihnsgg6bxanmb228r228ddvwzgrv4jszcbs75"))
            (patches (list (search-patch "binutils-ld-new-dtags.patch")
                           (search-patch "binutils-loongson-workaround.patch")))))
   (build-system gnu-build-system)

   ;; TODO: Add dependency on zlib + those for Gold.
   (arguments
    `(#:configure-flags '(;; Add `-static-libgcc' to not retain a dependency
                          ;; on GCC when bootstrapping.
                          "LDFLAGS=-static-libgcc"

                          ;; Don't search under /usr/lib & co.
                          "--with-lib-path=/no-ld-lib-path"

                          ;; Glibc 2.17 has a "comparison of unsigned
                          ;; expression >= 0 is always true" in wchar.h.
                          "--disable-werror"

                          ;; Install BFD.  It ends up in a hidden directory,
                          ;; but it's here.
                          "--enable-install-libbfd"

                          ;; Make sure 'ar' and 'ranlib' produce archives in a
                          ;; deterministic fashion.
                          "--enable-deterministic-archives")))

   (synopsis "Binary utilities: bfd gas gprof ld")
   (description
    "GNU Binutils is a collection of tools for working with binary files.
Perhaps the most notable are \"ld\", a linker, and \"as\", an assembler. Other
tools include programs to display binary profiling information, list the
strings in a binary file, and utilities for working with archives.  The \"bfd\"
library for working with executable and object formats is also included.")
   (license gpl3+)
   (home-page "http://www.gnu.org/software/binutils/")))

(define-public glibc
  (package
   (name "glibc")
   (version "2.19")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/glibc/glibc-"
                                version ".tar.xz"))
            (sha256
             (base32
              "18m2dssd6ja5arxmdxinc90xvpqcsnqjfwmjl2as07j0i3srff9d"))
            (snippet
             ;; Disable 'ldconfig' and /etc/ld.so.cache.  The latter is
             ;; required on LFS distros to avoid loading the distro's libc.so
             ;; instead of ours.
             '(substitute* "sysdeps/unix/sysv/linux/configure"
                (("use_ldconfig=yes")
                 "use_ldconfig=no")))
            (modules '((guix build utils)))
            (patches (list (search-patch "glibc-ldd-x86_64.patch")))))
   (build-system gnu-build-system)

   ;; Glibc's <limits.h> refers to <linux/limit.h>, for instance, so glibc
   ;; users should automatically pull Linux headers as well.
   (propagated-inputs `(("linux-headers" ,linux-libre-headers)))

   ;; Store the locales separately (~100 MiB).  Note that "out" retains a
   ;; reference to them anyway, so there's no space savings here.
   ;; TODO: Eventually we may want to add a $LOCALE_ARCHIVE search path like
   ;; Nixpkgs does.
   (outputs '("out" "locales" "debug"))

   (arguments
    `(#:out-of-source? #t
      #:configure-flags
      (list "--enable-add-ons"
            "--sysconfdir=/etc"
            (string-append "--localedir=" (assoc-ref %outputs "locales")
                           "/share/locale")

            ;; `--localedir' is not honored, so work around it.
            ;; See <http://sourceware.org/ml/libc-alpha/2013-03/msg00093.html>.
            (string-append "libc_cv_localedir="
                           (assoc-ref %outputs "locales")
                           "/share/locale")

            (string-append "--with-headers="
                           (assoc-ref %build-inputs "linux-headers")
                           "/include")

            ;; The default is to assume a 2.4 Linux interface, but we'll
            ;; always use something newer.  See "kernel-features.h" in the
            ;; GNU libc for details.
            "--enable-kernel=2.6.30"

            ;; Use our Bash instead of /bin/sh.
            (string-append "BASH_SHELL="
                           (assoc-ref %build-inputs "bash")
                           "/bin/bash")

            ;; XXX: Work around "undefined reference to `__stack_chk_guard'".
            "libc_cv_ssp=no")

      #:tests? #f                                 ; XXX
      #:phases (alist-cons-before
                'configure 'pre-configure
                (lambda* (#:key inputs outputs #:allow-other-keys)
                  (let* ((out  (assoc-ref outputs "out"))
                         (bin  (string-append out "/bin")))
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
                      ((" -lgcc_s") ""))

                    ;; Copy a statically-linked Bash in the output, with
                    ;; no references to other store paths.
                    (mkdir-p bin)
                    (copy-file (string-append (assoc-ref inputs "static-bash")
                                              "/bin/bash")
                               (string-append bin "/bash"))
                    (remove-store-references (string-append bin "/bash"))
                    (chmod (string-append bin "/bash") #o555)

                    ;; Keep a symlink, for `patch-shebang' resolution.
                    (with-directory-excursion bin
                      (symlink "bash" "sh"))

                    ;; Have `system' use that Bash.
                    (substitute* "sysdeps/posix/system.c"
                      (("#define[[:blank:]]+SHELL_PATH.*$")
                       (format #f "#define SHELL_PATH \"~a/bin/bash\"\n"
                               out)))

                    ;; Same for `popen'.
                    (substitute* "libio/iopopen.c"
                      (("/bin/sh")
                       (string-append out "/bin/bash")))

                    ;; Make sure we don't retain a reference to the
                    ;; bootstrap Perl.
                    (substitute* "malloc/mtrace.pl"
                      (("^#!.*")
                       ;; The shebang can be omitted, because there's the
                       ;; "bilingual" eval/exec magic at the top of the file.
                       "")
                      (("exec @PERL@")
                       "exec perl"))))
                (alist-cons-after
                 'install 'install-locales
                 (lambda _
                   (zero? (system* "make" "localedata/install-locales")))
                 %standard-phases))))

   (inputs `(("static-bash" ,(static-package bash-light))))

   ;; To build the manual, we need Texinfo and Perl.
   (native-inputs `(("texinfo" ,texinfo)
                    ("perl" ,perl)))

   (synopsis "The GNU C Library")
   (description
    "Any Unix-like operating system needs a C library: the library which
defines the \"system calls\" and other basic facilities such as open, malloc,
printf, exit...

The GNU C library is used as the C library in the GNU system and most systems
with the Linux kernel.")
   (license lgpl2.0+)
   (home-page "http://www.gnu.org/software/libc/")))

(define-public tzdata
  (package
    (name "tzdata")
    (version "2014a")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "http://www.iana.org/time-zones/repository/releases/tzdata"
                   version ".tar.gz"))
             (sha256
              (base32
               "1cg843ajz4g16axpz56zvalwsbp1s764na2bk4fb44ayx162bzvw"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f
       #:make-flags (let ((out (assoc-ref %outputs "out"))
                          (tmp (getenv "TMPDIR")))
                      (list (string-append "TOPDIR=" out)
                            (string-append "TZDIR=" out "/share/zoneinfo")

                            ;; Discard zic, dump, and tzselect, already
                            ;; provided by glibc.
                            (string-append "ETCDIR=" tmp "/etc")

                            ;; Likewise for the C library routines.
                            (string-append "LIBDIR=" tmp "/lib")
                            (string-append "MANDIR=" tmp "/man")

                            "AWK=awk"
                            "CC=gcc"))
       #:modules ((guix build utils)
                  (guix build gnu-build-system)
                  (srfi srfi-1))
       #:phases
       (alist-replace
        'unpack
        (lambda* (#:key source inputs #:allow-other-keys)
          (and (zero? (system* "tar" "xvf" source))
               (zero? (system* "tar" "xvf" (assoc-ref inputs "tzcode")))))
        (alist-cons-after
         'install 'post-install
         (lambda* (#:key outputs #:allow-other-keys)
           ;; Move data in the right place.
           (let ((out (assoc-ref outputs "out")))
             (copy-recursively (string-append out "/share/zoneinfo-posix")
                               (string-append out "/share/zoneinfo/posix"))
             (copy-recursively (string-append out "/share/zoneinfo-leaps")
                               (string-append out "/share/zoneinfo/right"))
             (delete-file-recursively (string-append out "/share/zoneinfo-posix"))
             (delete-file-recursively (string-append out "/share/zoneinfo-leaps"))))
         (alist-delete 'configure %standard-phases)))))
    (inputs `(("tzcode" ,(origin
                          (method url-fetch)
                          (uri (string-append
                                "http://www.iana.org/time-zones/repository/releases/tzcode"
                                version ".tar.gz"))
                          (sha256
                           (base32
                            "1xfkqi1q8cnxqbv8azdj5pqlzhkjz6xag09f1z0s8rxi86jkpf85"))))))
    (home-page "http://www.iana.org/time-zones")
    (synopsis "Database of current and historical time zones")
    (description "The Time Zone Database (often called tz or zoneinfo)
contains code and data that represent the history of local time for many
representative locations around the globe. It is updated periodically to
reflect changes made by political bodies to time zone boundaries, UTC offsets,
and daylight-saving rules.")
    (license public-domain)))


;;;
;;; Bootstrap packages.
;;;

(define gnu-make-boot0
  (package-with-bootstrap-guile
   (package (inherit gnu-make)
     (name "make-boot0")
     (location (source-properties->location (current-source-location)))
     (arguments
      `(#:guile ,%bootstrap-guile
        #:implicit-inputs? #f
        #:tests? #f                  ; cannot run "make check"
        ,@(substitute-keyword-arguments (package-arguments gnu-make)
            ((#:phases phases)
             `(alist-replace
               'build (lambda _
                        ;; Don't attempt to build 'guile.c' since we don't
                        ;; have Guile here.
                        (substitute* "build.sh"
                          (("guile\\.\\$\\{OBJEXT\\}") ""))
                        (zero? (system* "./build.sh")))
               (alist-replace
                'install (lambda* (#:key outputs #:allow-other-keys)
                           (let* ((out (assoc-ref outputs "out"))
                                  (bin (string-append out "/bin")))
                             (mkdir-p bin)
                             (copy-file "make"
                                        (string-append bin "/make"))))
                ,phases))))))
     (native-inputs '())                          ; no need for 'pkg-config'
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

(define* (nix-system->gnu-triplet
          #:optional (system (%current-system)) (vendor "unknown"))
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

(define* (boot-triplet #:optional (system (%current-system)))
  ;; Return the triplet used to create the cross toolchain needed in the
  ;; first bootstrapping stage.
  (nix-system->gnu-triplet system "guix"))

;; Following Linux From Scratch, build a cross-toolchain in stage 0.  That
;; toolchain actually targets the same OS and arch, but it has the advantage
;; of being independent of the libc and tools in %BOOTSTRAP-INPUTS, since
;; GCC-BOOT0 (below) is built without any reference to the target libc.

(define binutils-boot0
  (package-with-bootstrap-guile
   (package (inherit binutils)
     (name "binutils-cross-boot0")
     (arguments
      `(#:guile ,%bootstrap-guile
        #:implicit-inputs? #f
        ,@(substitute-keyword-arguments (package-arguments binutils)
            ((#:configure-flags cf)
             `(cons ,(string-append "--target=" (boot-triplet))
                    ,cf)))))
     (inputs %boot0-inputs))))

(define gcc-boot0
  (package-with-bootstrap-guile
   (package (inherit gcc-4.8)
     (name "gcc-cross-boot0")
     (arguments
      `(#:guile ,%bootstrap-guile
        #:implicit-inputs? #f
        #:modules ((guix build gnu-build-system)
                   (guix build utils)
                   (ice-9 regex)
                   (srfi srfi-1)
                   (srfi srfi-26))
        ,@(substitute-keyword-arguments (package-arguments gcc-4.8)
            ((#:configure-flags flags)
             `(append (list ,(string-append "--target=" (boot-triplet))

                            ;; No libc yet.
                            "--without-headers"

                            ;; Disable features not needed at this stage.
                            "--disable-shared"
                            "--enable-languages=c,c++"

                            ;; libstdc++ cannot be built at this stage
                            ;; ("Link tests are not allowed after
                            ;; GCC_NO_EXECUTABLES.").
                            "--disable-libstdc++-v3"

                            "--disable-threads"
                            "--disable-libmudflap"
                            "--disable-libatomic"
                            "--disable-libsanitizer"
                            "--disable-libitm"
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

                   ;; Create symlinks like `gmp' -> `gmp-x.y.z'.
                   ,@(map (lambda (lib)
                            ;; Drop trailing letters, as gmp-6.0.0a unpacks
                            ;; into gmp-6.0.0.
                            `(symlink ,(string-trim-right
                                        (package-full-name lib)
                                        char-set:letter)
                                      ,(package-name lib)))
                          (list gmp mpfr mpc))))
               (alist-cons-after
                'install 'symlink-libgcc_eh
                (lambda* (#:key outputs #:allow-other-keys)
                  (let ((out (assoc-ref outputs "out")))
                    ;; Glibc wants to link against libgcc_eh, so provide
                    ;; it.
                    (with-directory-excursion
                        (string-append out "/lib/gcc/"
                                       ,(boot-triplet)
                                       "/" ,(package-version gcc-4.8))
                      (symlink "libgcc.a" "libgcc_eh.a"))))
                ,phases))))))

     (inputs `(("gmp-source" ,(package-source gmp))
               ("mpfr-source" ,(package-source mpfr))
               ("mpc-source" ,(package-source mpc))
               ("binutils-cross" ,binutils-boot0)

               ;; Call it differently so that the builder can check whether
               ;; the "libc" input is #f.
               ("libc-native" ,@(assoc-ref %boot0-inputs "libc"))
               ,@(alist-delete "libc" %boot0-inputs)))

     ;; No need for Texinfo at this stage.
     (native-inputs (alist-delete "texinfo"
                                  (package-native-inputs gcc-4.8))))))

(define perl-boot0
  (package-with-bootstrap-guile
   (package-with-explicit-inputs perl
                                 %boot0-inputs
                                 (current-source-location)
                                 #:guile %bootstrap-guile)))

(define (linux-libre-headers-boot0)
  "Return Linux-Libre header files for the bootstrap environment."
  ;; Note: this is wrapped in a thunk to nicely handle circular dependencies
  ;; between (gnu packages linux) and this module.
  (package-with-bootstrap-guile
   (package (inherit linux-libre-headers)
     (arguments `(#:guile ,%bootstrap-guile
                  #:implicit-inputs? #f
                  ,@(package-arguments linux-libre-headers)))
     (native-inputs
      `(("perl" ,perl-boot0)
        ,@%boot0-inputs)))))

(define texinfo-boot0
  ;; Texinfo used to build libc's manual.
  ;; We build without ncurses because it fails to build at this stage, and
  ;; because we don't need the stand-alone Info reader.
  ;; Also, use %BOOT0-INPUTS to avoid building Perl once more.
  (let ((texinfo (package (inherit texinfo)
                   (inputs (alist-delete "ncurses" (package-inputs texinfo))))))
    (package-with-bootstrap-guile
     (package-with-explicit-inputs texinfo %boot0-inputs
                                   (current-source-location)
                                   #:guile %bootstrap-guile))))

(define %boot1-inputs
  ;; 2nd stage inputs.
  `(("gcc" ,gcc-boot0)
    ("binutils-cross" ,binutils-boot0)

    ;; Keep "binutils" here because the cross-gcc invokes `as', not the
    ;; cross-`as'.
    ,@%boot0-inputs))

(define glibc-final-with-bootstrap-bash
  ;; The final libc, "cross-built".  If everything went well, the resulting
  ;; store path has no dependencies.  Actually, the really-final libc is
  ;; built just below; the only difference is that this one uses the
  ;; bootstrap Bash.
  (package-with-bootstrap-guile
   (package (inherit glibc)
     (name "glibc-intermediate")
     (arguments
      `(#:guile ,%bootstrap-guile
        #:implicit-inputs? #f

        ,@(substitute-keyword-arguments (package-arguments glibc)
            ((#:configure-flags flags)
             `(append (list ,(string-append "--host=" (boot-triplet))
                            ,(string-append "--build="
                                            (nix-system->gnu-triplet))

                            ;; Build Sun/ONC RPC support.  In particular,
                            ;; install rpc/*.h.
                            "--enable-obsolete-rpc")
                      ,flags)))))
     (propagated-inputs `(("linux-headers" ,(linux-libre-headers-boot0))))
     (native-inputs
      `(("texinfo" ,texinfo-boot0)
        ("perl" ,perl-boot0)))
     (inputs
      `( ;; A native GCC is needed to build `cross-rpcgen'.
        ("native-gcc" ,@(assoc-ref %boot0-inputs "gcc"))

        ;; Here, we use the bootstrap Bash, which is not satisfactory
        ;; because we don't want to depend on bootstrap tools.
        ("static-bash" ,@(assoc-ref %boot0-inputs "bash"))

        ,@%boot1-inputs
        ,@(alist-delete "static-bash"
                        (package-inputs glibc))))))) ; patches

(define (cross-gcc-wrapper gcc binutils glibc bash)
  "Return a wrapper for the pseudo-cross toolchain GCC/BINUTILS/GLIBC
that makes it available under the native tool names."
  (package (inherit gcc-4.8)
    (name (string-append (package-name gcc) "-wrapped"))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     `(#:guile ,%bootstrap-guile
       #:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))

                   (let* ((binutils (assoc-ref %build-inputs "binutils"))
                          (gcc      (assoc-ref %build-inputs "gcc"))
                          (libc     (assoc-ref %build-inputs "libc"))
                          (bash     (assoc-ref %build-inputs "bash"))
                          (out      (assoc-ref %outputs "out"))
                          (bindir   (string-append out "/bin"))
                          (triplet  ,(boot-triplet)))
                     (define (wrap-program program)
                       ;; GCC-BOOT0 is a libc-less cross-compiler, so it
                       ;; needs to be told where to find the crt files and
                       ;; the dynamic linker.
                       (call-with-output-file program
                         (lambda (p)
                           (format p "#!~a/bin/bash
exec ~a/bin/~a-~a -B~a/lib -Wl,-dynamic-linker -Wl,~a/~a \"$@\"~%"
                                   bash
                                   gcc triplet program
                                   libc libc
                                   ,(glibc-dynamic-linker))))

                       (chmod program #o555))

                     (mkdir-p bindir)
                     (with-directory-excursion bindir
                       (for-each (lambda (tool)
                                   (symlink (string-append binutils "/bin/"
                                                           triplet "-" tool)
                                            tool))
                                 '("ar" "ranlib"))
                       (for-each wrap-program '("gcc" "g++")))))))
    (native-inputs
     `(("binutils" ,binutils)
       ("gcc" ,gcc)
       ("libc" ,glibc)
       ("bash" ,bash)))
    (inputs '())))

(define static-bash-for-glibc
  ;; A statically-linked Bash to be embedded in GLIBC-FINAL, for use by
  ;; system(3) & co.
  (let* ((gcc  (cross-gcc-wrapper gcc-boot0 binutils-boot0
                                  glibc-final-with-bootstrap-bash
                                  (car (assoc-ref %boot1-inputs "bash"))))
         (bash (package (inherit bash-light)
                 (arguments
                  `(#:guile ,%bootstrap-guile
                    ,@(package-arguments bash-light))))))
    (package-with-bootstrap-guile
     (package-with-explicit-inputs (static-package bash)
                                   `(("gcc" ,gcc)
                                     ("libc" ,glibc-final-with-bootstrap-bash)
                                     ,@(fold alist-delete %boot1-inputs
                                             '("gcc" "libc")))
                                   (current-source-location)))))

(define-public glibc-final
  ;; The final glibc, which embeds the statically-linked Bash built above.
  (package (inherit glibc-final-with-bootstrap-bash)
    (name "glibc")
    (inputs `(("static-bash" ,static-bash-for-glibc)
              ,@(alist-delete
                 "static-bash"
                 (package-inputs glibc-final-with-bootstrap-bash))))))

(define gcc-boot0-wrapped
  ;; Make the cross-tools GCC-BOOT0 and BINUTILS-BOOT0 available under the
  ;; non-cross names.
  (cross-gcc-wrapper gcc-boot0 binutils-boot0 glibc-final
                     (car (assoc-ref %boot1-inputs "bash"))))

(define %boot2-inputs
  ;; 3rd stage inputs.
  `(("libc" ,glibc-final)
    ("gcc" ,gcc-boot0-wrapped)
    ,@(fold alist-delete %boot1-inputs '("libc" "gcc"))))

(define binutils-final
  (package-with-bootstrap-guile
   (package (inherit binutils)
     (arguments
      `(#:guile ,%bootstrap-guile
        #:implicit-inputs? #f
        ,@(package-arguments binutils)))
     (inputs %boot2-inputs))))

(define libstdc++
  ;; Intermediate libstdc++ that will allow us to build the final GCC
  ;; (remember that GCC-BOOT0 cannot build libstdc++.)
  (package-with-bootstrap-guile
   (package (inherit gcc-4.8)
     (name "libstdc++")
     (arguments
      `(#:guile ,%bootstrap-guile
        #:implicit-inputs? #f

        #:out-of-source? #t
        #:phases (alist-cons-before
                  'configure 'chdir
                  (lambda _
                    (chdir "libstdc++-v3"))
                  %standard-phases)
        #:configure-flags `("--disable-shared"
                            "--disable-libstdcxx-threads"
                            "--disable-libstdcxx-pch"
                            ,(string-append "--with-gxx-include-dir="
                                            (assoc-ref %outputs "out")
                                            "/include"
                                            ;; "/include/c++/"
                                            ;; ,(package-version gcc-4.8)
                                            ))))
     (inputs %boot2-inputs)
     (native-inputs '())
     (propagated-inputs '())
     (synopsis "GNU C++ standard library (intermediate)"))))

(define-public gcc-final
  ;; The final GCC.
  (package (inherit gcc-boot0)
    (name "gcc")
    (location (source-properties->location (current-source-location)))
    (arguments
     `(#:guile ,%bootstrap-guile
       #:implicit-inputs? #f

       ;; Build again GMP & co. within GCC's build process, because it's hard
       ;; to do outside (because GCC-BOOT0 is a cross-compiler, and thus
       ;; doesn't honor $LIBRARY_PATH, which breaks `gnu-build-system'.)
       ,@(substitute-keyword-arguments (package-arguments gcc-boot0)
           ((#:configure-flags boot-flags)
            (let loop ((args (package-arguments gcc-4.8)))
              (match args
                ((#:configure-flags normal-flags _ ...)
                 normal-flags)
                ((_ rest ...)
                 (loop rest)))))
           ((#:make-flags flags)
            ;; Since $LIBRARY_PATH and $CPATH are not honored, add the
            ;; relevant flags.
            `(cons (string-append "CPPFLAGS=-I"
                                  (assoc-ref %build-inputs "libstdc++")
                                  "/include")
                   (map (lambda (flag)
                          (if (string-prefix? "LDFLAGS=" flag)
                              (string-append flag " -L"
                                             (assoc-ref %build-inputs "libstdc++")
                                             "/lib")
                              flag))
                        ,flags)))
           ((#:phases phases)
            `(alist-delete 'symlink-libgcc_eh ,phases)))))

    ;; This time we want Texinfo, so we get the manual.
    (native-inputs `(("texinfo" ,texinfo-boot0)
                     ,@(package-native-inputs gcc-boot0)))

    (inputs `(("gmp-source" ,(package-source gmp))
              ("mpfr-source" ,(package-source mpfr))
              ("mpc-source" ,(package-source mpc))
              ("binutils" ,binutils-final)
              ("libstdc++" ,libstdc++)
              ,@%boot2-inputs))))

(define ld-wrapper-boot3
  ;; A linker wrapper that uses the bootstrap Guile.
  (package
    (name "ld-wrapper-boot3")
    (version "0")
    (source #f)
    (build-system trivial-build-system)
    (inputs `(("binutils" ,binutils-final)
              ("guile"    ,%bootstrap-guile)
              ("bash"     ,@(assoc-ref %boot2-inputs "bash"))
              ("wrapper"  ,(search-path %load-path
                                        "gnu/packages/ld-wrapper.scm"))))
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
                       (("@BASH@")
                        (string-append (assoc-ref %build-inputs "bash")
                                       "/bin/bash"))
                       (("@LD@")
                        (string-append (assoc-ref %build-inputs "binutils")
                                       "/bin/ld")))
                     (chmod ld #o555)
                     (compile-file ld #:output-file go)))))
    (synopsis "The linker wrapper")
    (description
     "The linker wrapper (or `ld-wrapper') wraps the linker to add any
missing `-rpath' flags, and to detect any misuse of libraries outside of the
store.")
    (home-page #f)
    (license gpl3+)))

(define %boot3-inputs
  ;; 4th stage inputs.
  `(("gcc" ,gcc-final)
    ("ld-wrapper" ,ld-wrapper-boot3)
    ,@(alist-delete "gcc" %boot2-inputs)))

(define bash-final
  ;; Link with `-static-libgcc' to make sure we don't retain a reference
  ;; to the bootstrap GCC.
  (package-with-bootstrap-guile
   (package-with-explicit-inputs (static-libgcc-package bash)
                                 %boot3-inputs
                                 (current-source-location)
                                 #:guile %bootstrap-guile)))

(define %boot4-inputs
  ;; Now use the final Bash.
  `(("bash" ,bash-final)
    ,@(alist-delete "bash" %boot3-inputs)))

(define-public guile-final
  (package-with-bootstrap-guile
   (package-with-explicit-inputs guile-2.0/fixed
                                 %boot4-inputs
                                 (current-source-location)
                                 #:guile %bootstrap-guile)))

(define gnu-make-final
  ;; The final GNU Make, which uses the final Guile.
  (package-with-bootstrap-guile
   (package-with-explicit-inputs gnu-make
                                 `(("guile" ,guile-final)
                                   ,@%boot4-inputs)
                                 (current-source-location))))

(define-public ld-wrapper
  ;; The final `ld' wrapper, which uses the final Guile.
  (package (inherit ld-wrapper-boot3)
    (name "ld-wrapper")
    (inputs `(("guile" ,guile-final)
              ("bash"  ,bash-final)
              ,@(fold alist-delete (package-inputs ld-wrapper-boot3)
                      '("guile" "bash"))))))

(define coreutils-final
  ;; The final Coreutils.  Treat them specially because some packages, such as
  ;; Findutils, keep a reference to the Coreutils they were built with.
  (package-with-bootstrap-guile
   (package-with-explicit-inputs coreutils
                                 %boot4-inputs
                                 (current-source-location)

                                 ;; Use the final Guile, linked against the
                                 ;; final libc with working iconv, so that
                                 ;; 'substitute*' works well when touching
                                 ;; test files in Gettext.
                                 #:guile guile-final)))

(define %boot5-inputs
  ;; Now use the final Coreutils.
  `(("coreutils" ,coreutils-final)
    ,@%boot4-inputs))

(define-public %final-inputs
  ;; Final derivations used as implicit inputs by 'gnu-build-system'.  We
  ;; still use 'package-with-bootstrap-guile' so that the bootstrap tools are
  ;; used for origins that have patches, thereby avoiding circular
  ;; dependencies.
  (let ((finalize (compose package-with-bootstrap-guile
                           (cut package-with-explicit-inputs <> %boot5-inputs
                                (current-source-location)))))
    `(,@(map (match-lambda
              ((name package)
               (list name (finalize package))))
             `(("tar" ,tar)
               ("gzip" ,gzip)
               ("bzip2" ,bzip2)
               ("xz" ,xz)
               ("diffutils" ,diffutils)
               ("patch" ,patch)
               ("sed" ,sed)
               ("grep" ,grep)
               ("findutils" ,findutils)
               ("gawk" ,gawk)))
      ("coreutils" ,coreutils-final)
      ("make" ,gnu-make-final)
      ("bash" ,bash-final)
      ("ld-wrapper" ,ld-wrapper)
      ("binutils" ,binutils-final)
      ("gcc" ,gcc-final)
      ("libc" ,glibc-final))))

(define (gcc-toolchain gcc)
  "Return a complete toolchain for GCC."
  (package
    (name "gcc-toolchain")
    (version (package-version gcc))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build union))
       #:builder (begin
                   (use-modules (ice-9 match)
                                (guix build union))

                   (match %build-inputs
                     (((names . directories) ...)
                      (union-build (assoc-ref %outputs "out")
                                   directories)))

                   (union-build (assoc-ref %outputs "debug")
                                (list (assoc-ref %build-inputs
                                                 "libc-debug"))))))
    (license (package-license gcc))
    (synopsis "Complete GCC tool chain for C/C++ development")
    (description
     "This package provides a complete GCC tool chain for C/C++ development to
be installed in user profiles.  This includes GCC, as well as libc (headers
and binaries, plus debugging symbols in the 'debug' output), and Binutils.")
    (home-page "http://gcc.gnu.org/")
    (outputs '("out" "debug"))

    ;; The main raison d'être of this "meta-package" is (1) to conveniently
    ;; install everything that we need, and (2) to make sure ld-wrapper comes
    ;; before Binutils' ld in the user's profile.
    (inputs `(("gcc" ,gcc)
              ("ld-wrapper" ,(car (assoc-ref %final-inputs "ld-wrapper")))
              ("binutils" ,binutils-final)
              ("libc" ,glibc-final)
              ("libc-debug" ,glibc-final "debug")))))

(define-public gcc-toolchain-4.8
  (gcc-toolchain gcc-final))

(define-public gcc-toolchain-4.9
  (gcc-toolchain gcc-4.9))

;;; base.scm ends here
