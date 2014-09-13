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
  #:use-module (gnu packages guile)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

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
   (version "2.20")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/grep/grep-"
                                version ".tar.xz"))
            (sha256
             (base32
              "0rcs0spsxdmh6yz8y4frkqp6f5iw19mdbdl9s2v6956hq0mlbbzh"))))
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
                    ;; 'split' uses either $SHELL or /bin/sh.  Set $SHELL so
                    ;; that tests pass, since /bin/sh isn't in the chroot.
                    (setenv "SHELL" (which "sh"))

                    (substitute* (find-files "gnulib-tests" "\\.c$")
                      (("/bin/sh")
                       (format #f "~a/bin/sh" bash)))
                    (substitute* (find-files "tests" "\\.sh$")
                      (("#!/bin/sh")
                       (format #f "#!~a/bin/sh" bash)))))
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
Perhaps the most notable are \"ld\", a linker, and \"as\", an assembler.
Other tools include programs to display binary profiling information, list
the strings in a binary file, and utilities for working with archives.  The
\"bfd\" library for working with executable and object formats is also
included.")
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

(define-public (canonical-package package)
  ;; Avoid circular dependency by lazily resolving 'commencement'.
  (let* ((iface (resolve-interface '(gnu packages commencement)))
         (proc  (module-ref iface 'canonical-package)))
    (proc package)))

;;; base.scm ends here
