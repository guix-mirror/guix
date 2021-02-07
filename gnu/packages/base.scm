;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2019 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014, 2015, 2016, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Alex Kost <alezost@gmail.com>
;;; Copyright © 2014, 2015 Manolis Fragkiskos Ragkousis <manolis837@gmail.com>
;;; Copyright © 2016, 2017, 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016, 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2017 Rene Saavedra <rennes@openmailbox.org>
;;; Copyright © 2017, 2020 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017, 2018, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 Vitaliy Shatrov <D0dyBo0D0dyBo0@protonmail.com>
;;; Copyright © 2020 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2021 Leo Le Bouter <lle-bout@zaclys.net>
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
                #:select (gpl3+ lgpl2.0+ lgpl3+ public-domain))
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages ed)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages hurd)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages gettext)
  #:use-module (guix i18n)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (glibc
            libiconv-if-needed))

;;; Commentary:
;;;
;;; Base packages of the Guix-based GNU user-land software distribution.
;;;
;;; Code:

(define-public hello
  (package
    (name "hello")
    (version "2.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/hello/hello-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i"))))
    (build-system gnu-build-system)
    (synopsis "Hello, GNU world: An example GNU package")
    (description
     "GNU Hello prints the message \"Hello, world!\" and then exits.  It
serves as an example of standard GNU coding practices.  As such, it supports
command-line arguments, multiple languages, and so on.")
    (home-page "https://www.gnu.org/software/hello/")
    (license gpl3+)))

(define-public grep
  (package
   (name "grep")
   (version "3.4")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/grep/grep-"
                                version ".tar.xz"))
            (sha256
             (base32
              "1yy33kiwrxrwj2nxa4fg15bvmwyghqbs8qwkdvy5phm784f7brjq"))
            (patches (search-patches "grep-timing-sensitive-test.patch"))))
   (build-system gnu-build-system)
   (native-inputs `(("perl" ,perl)))             ;some of the tests require it
   (inputs `(("pcre" ,pcre)))
   (arguments
    `(#:phases
      (modify-phases %standard-phases
        (add-after 'install 'fix-egrep-and-fgrep
          ;; Patch 'egrep' and 'fgrep' to execute 'grep' via its
          ;; absolute file name instead of searching for it in $PATH.
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (bin (string-append out "/bin")))
              (substitute* (list (string-append bin "/egrep")
                                 (string-append bin "/fgrep"))
                (("^exec grep")
                 (string-append "exec " bin "/grep")))
              #t))))))
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
   (home-page "https://www.gnu.org/software/grep/")))

(define-public sed
  (package
   (name "sed")
   (version "4.8")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/sed/sed-" version
                                ".tar.gz"))
            (sha256
             (base32
              "0alqagh0nliymz23kfjg6g9w3cr086k0sfni56gi8fhzqwa3xksk"))))
   (build-system gnu-build-system)
   (arguments
    ;; TODO: When merging this into core-updates, keep the version of
    ;; this code (with comment!) applied as a snippet.
    `(,@(if (string-prefix? "powerpc64le" (or (%current-target-system)
                                              (%current-system)))
          `(#:phases
            (modify-phases %standard-phases
              (add-after 'unpack 'allow-building-on-selinux-systems
                (lambda _
                  (substitute* "Makefile.in"
                    (("^  abs_srcdir='\\$\\(abs_srcdir\\)'.*" previous-line)
                     (string-append
                      previous-line
                      "  CONFIG_HEADER='$(CONFIG_HEADER)'\t\t\\\n")))
                  #t))))
          '())))
   (synopsis "Stream editor")
   (native-inputs
    `(("perl" ,perl)))                            ;for tests
   (description
    "Sed is a non-interactive, text stream editor.  It receives a text
input from a file or from standard input and it then applies a series of text
editing commands to the stream and prints its output to standard output.  It
is often used for substituting text patterns in a stream.  The GNU
implementation offers several extensions over the standard utility.")
   (license gpl3+)
   (home-page "https://www.gnu.org/software/sed/")))

(define-public tar
  (package
   (name "tar")
   (version "1.32")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/tar/tar-"
                                version ".tar.xz"))
            (sha256
             (base32
              "1n7xy657ii0sa42zx6944v2m4v9qrh6sqgmw17l3nch3y43sxlyh"))
            (patches (search-patches "tar-skip-unreliable-tests.patch"
                                     "tar-remove-wholesparse-check.patch"))))
   (build-system gnu-build-system)
   ;; Note: test suite requires ~1GiB of disk space.
   (arguments
    `(,@(if (hurd-target?)
            '(#:make-flags
              (list (string-append
                     "TESTSUITEFLAGS= -k '"
                     "!sparse"
                     ",!renamed dirs in incrementals"
                     ",!--exclude-tag option in incremental pass"
                     ",!incremental dumps with -C"
                     ",!incremental dumps of nested directories"
                     ",!incremental restores with -C"
                     ",!concatenated incremental archives (renames)"
                     ",!renamed directory containing subdirectories"
                     ",!renamed subdirectories"
                     "'")))
            '())
      #:phases (modify-phases %standard-phases
                 (add-before 'build 'set-shell-file-name
                   (lambda* (#:key inputs #:allow-other-keys)
                     ;; Do not use "/bin/sh" to run programs.
                     (let ((bash (assoc-ref inputs "bash")))
                       (substitute* "src/system.c"
                         (("/bin/sh")
                          (string-append bash "/bin/sh")))
                       #t))))))

   ;; When cross-compiling, the 'set-shell-file-name' phase needs to be able
   ;; to refer to the target Bash.
   (inputs (if (%current-target-system)
               `(("bash" ,bash))
               '()))

   (synopsis "Managing tar archives")
   (description
    "Tar provides the ability to create tar archives, as well as the
ability to extract, update or list files in an existing archive.  It is
useful for combining many files into one larger file, while maintaining
directory structure and file information such as permissions and
creation/modification dates.  GNU tar offers many extensions over the
standard utility.")
   (license gpl3+)
   (home-page "https://www.gnu.org/software/tar/")))

(define-public patch
  (package
   (name "patch")
    (version "2.7.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/patch/patch-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1zfqy4rdcy279vwn2z1kbv19dcfw25d2aqy9nzvdkq5bjzd0nqdc"))
              (patches (search-patches "patch-hurd-path-max.patch"))))
   (build-system gnu-build-system)
   (arguments
    ;; Work around a cross-compilation bug whereby libpatch.a would provide
    ;; '__mktime_internal', which conflicts with the one in libc.a.
    (if (%current-target-system)
        `(#:configure-flags '("gl_cv_func_working_mktime=yes"))
        '()))
   (native-inputs `(("ed" ,ed)))
   (synopsis "Apply differences to originals, with optional backups")
   (description
    "Patch is a program that applies changes to files based on differences
laid out as by the program \"diff\".  The changes may be applied to one or more
files depending on the contents of the diff file.  It accepts several
different diff formats.  It may also be used to revert previously applied
differences.")
   (license gpl3+)
   (home-page "https://savannah.gnu.org/projects/patch/")))

(define-public diffutils
  (package
   (name "diffutils")
   (version "3.7")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/diffutils/diffutils-"
                                version ".tar.xz"))
            (sha256
             (base32
              "09isrg0isjinv8c535nxsi1s86wfdfzml80dbw41dj9x3hiad9xk"))))
   (build-system gnu-build-system)
   (native-inputs `(("perl" ,perl)))
   (synopsis "Comparing and merging files")
   (description
    "GNU Diffutils is a package containing tools for finding the
differences between files.  The \"diff\" command is used to show how two files
differ, while \"cmp\" shows the offsets and line numbers where they differ.
\"diff3\" allows you to compare three files.  Finally, \"sdiff\" offers an
interactive means to merge two files.")
   (license gpl3+)
   (home-page "https://www.gnu.org/software/diffutils/")))

(define-public findutils
  (package
   (name "findutils")
   (version "4.7.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/findutils/findutils-"
                                version ".tar.xz"))
            (sha256
             (base32
              "16kqz9yz98dasmj70jwf5py7jk558w96w0vgp3zf9xsqk3gzpzn5"))
            (patches (search-patches "findutils-localstatedir.patch"
                                     "findutils-test-rwlock-threads.patch"))))
   (build-system gnu-build-system)
   (arguments
    `(#:configure-flags (list
                         ;; Tell 'updatedb' to write to /var.
                         "--localstatedir=/var")
      #:phases (modify-phases %standard-phases
                 (add-before 'check 'adjust-test-shebangs
                   (lambda _
                     (substitute* '("tests/xargs/verbose-quote.sh"
                                    "tests/find/exec-plus-last-file.sh")
                       (("#!/bin/sh")
                        (string-append "#!" (which "sh"))))
                     #t)))))
   (synopsis "Operating on files matching given criteria")
   (description
    "Findutils supplies the basic file directory searching utilities of the
GNU system.  It consists of two primary searching utilities: \"find\"
recursively searches for files in a directory according to given criteria and
\"locate\" lists files in a database that match a query.  Two auxiliary tools
are included: \"updatedb\" updates the file name database and \"xargs\" may be
used to apply commands with arbitrarily long arguments.")
   (license gpl3+)
   (home-page "https://www.gnu.org/software/findutils/")))

(define-public coreutils
  (package
   (name "coreutils")
   (version "8.32")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/coreutils/coreutils-"
                                version ".tar.xz"))
            (sha256
             (base32
              "1yjcrh5hw70c0yn8zw55pd6j51dj90anpq8mmg649ps9g3gdhn24"))
            (patches (search-patches "coreutils-ls.patch"))))
   (build-system gnu-build-system)
   (inputs `(("acl"  ,acl)                        ; TODO: add SELinux
             ("gmp"  ,gmp)                        ;bignums in 'expr', yay!

             ;; Do not use libcap when cross-compiling since it's not quite
             ;; cross-compilable; and use it only for supported systems.
             ,@(if (and (not (%current-target-system))
                        (member (%current-system)
                                (package-supported-systems libcap)))
             `(("libcap" ,libcap-2.31))        ;capability support in 'ls', etc.
             '())))
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
      ,@(if (hurd-target?)
            '(#:make-flags            ; these tests fail deterministically
              (list (string-append "XFAIL_TESTS=tests/misc/env-S.pl"
                                   " tests/misc/kill.sh"
                                   " tests/misc/nice.sh"
                                   " tests/misc/pwd-long.sh"
                                   " tests/split/fail.sh"
                                   " test-fdutimensat"
                                   " test-futimens"
                                   " test-linkat"
                                   " test-renameat"
                                   " test-renameatu"
                                   " test-tls"
                                   " test-utimensat")))
            '())
      #:phases (modify-phases %standard-phases
                 (add-before 'build 'patch-shell-references
                   (lambda _
                     ;; 'split' uses either $SHELL or /bin/sh.  Set $SHELL so
                     ;; that tests pass, since /bin/sh isn't in the chroot.
                     (setenv "SHELL" (which "sh"))

                     (substitute* (find-files "gnulib-tests" "\\.c$")
                       (("/bin/sh") (which "sh")))
                     (substitute* (find-files "tests" "\\.sh$")
                       (("#!/bin/sh") (string-append "#!" (which "sh"))))
                     #t))
                 ,@(if (hurd-target?)
                       `((add-after 'unpack 'remove-tests
                           (lambda _
                             (substitute* "Makefile.in"
                               ;; this test hangs
                               (("^ *tests/misc/timeout-group.sh.*") ""))
                             #t)))
                       '()))))
   (synopsis "Core GNU utilities (file, text, shell)")
   (description
    "GNU Coreutils package includes all of the basic command-line tools that
are expected in a POSIX system, excluding shell.  This package is the union of
the GNU fileutils, sh-utils, and textutils packages.  Most of these tools
offer extended functionality beyond that which is outlined in the POSIX
standard.")
   (license gpl3+)
   (home-page "https://www.gnu.org/software/coreutils/")))

(define-public coreutils-minimal
  ;; Coreutils without its optional dependencies.
  (package
    (inherit coreutils)
    (name "coreutils-minimal")
    (outputs '("out"))
    (inputs '())))

(define-public coreutils-8.30
  ;; XXX: This version is kept just so we can run PRoot tests.
  (hidden-package
   (package
     (inherit coreutils-minimal)
     (version "8.30")
     (source (origin
               (method url-fetch)
               (uri (string-append "mirror://gnu/coreutils/coreutils-"
                                   version ".tar.xz"))
               (sha256
                (base32
                 "0mxhw43d4wpqmvg0l4znk1vm10fy92biyh90lzdnqjcic2lb6cg8"))))
     (arguments
      (substitute-keyword-arguments (package-arguments coreutils-minimal)
        ((#:phases phases '%standard-phases)
         `(modify-phases ,phases
            (add-before 'check 'disable-broken-test
              (lambda _
                ;; This test hits the 127 character shebang limit in the build
                ;; environment due to the way "env -S" splits arguments into
                ;; shebangs.  Note that "env-S-script.sh" works around this
                ;; specific issue, but "env-S.pl" is not adjusted for build
                ;; environments with long prefixes (/tmp/guix-build-...).
                (substitute* "Makefile"
                  (("^.*tests/misc/env-S.pl.*$") ""))
                #t)))))))))

(define-public gnu-make
  (package
   (name "make")
   (version "4.3")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/make/make-" version
                                ".tar.gz"))
            (sha256
             (base32
              "06cfqzpqsvdnsxbysl5p2fgdgxgl9y4p7scpnrfa8z2zgkjdspz0"))
            (patches (search-patches "make-impure-dirs.patch"))))
   (build-system gnu-build-system)
   (native-inputs `(("pkg-config" ,pkg-config)))  ; to detect Guile
   (inputs `(("guile" ,guile-3.0)))
   (outputs '("out" "debug"))
   (arguments
    `(,@(if (hurd-target?)
            '(#:configure-flags '("CFLAGS=-D__alloca=alloca"
                                  "ac_cv_func_posix_spawn=no"))
            '())
      #:phases
      (modify-phases %standard-phases
        (add-before 'build 'set-default-shell
          (lambda* (#:key inputs #:allow-other-keys)
            ;; Change the default shell from /bin/sh.
            (let ((bash (assoc-ref inputs "bash")))
              (substitute* "src/job.c"
                (("default_shell =.*$")
                 (format #f "default_shell = \"~a/bin/sh\";\n"
                         bash)))
              #t))))))
   (synopsis "Remake files automatically")
   (description
    "Make is a program that is used to control the production of
executables or other files from their source files.  The process is
controlled from a Makefile, in which the developer specifies how each file is
generated from its source.  It has powerful dependency resolution and the
ability to determine when files have to be regenerated after their sources
change.  GNU make offers many powerful extensions over the standard utility.")
   (license gpl3+)
   (home-page "https://www.gnu.org/software/make/")))

(define-public gnu-make-4.2
  (package
    (inherit gnu-make)
    (version "4.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/make/make-" version
                                  ".tar.bz2"))
              (sha256
               (base32
                "12f5zzyq2w56g95nni65hc0g5p7154033y2f3qmjvd016szn5qnn"))))
    (arguments
     `(#:configure-flags '("CFLAGS=-D__alloca=alloca")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-default-shell
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Change the default shell from /bin/sh.
             (let ((bash (assoc-ref inputs "bash")))
               (substitute* "job.c"
                 (("default_shell =.*$")
                  (format #f "default_shell = \"~a/bin/sh\";\n"
                          bash)))
               #t))))))))

(define-public binutils
  (package
   (name "binutils")
   (version "2.34")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/binutils/binutils-"
                                version ".tar.bz2"))
            (sha256
             (base32
              "1rin1f5c7wm4n3piky6xilcrpf2s0n3dd5vqq8irrxkcic3i1w49"))
            (patches (search-patches "binutils-loongson-workaround.patch"))))
   (build-system gnu-build-system)

   ;; TODO: Add dependency on zlib + those for Gold.
   (arguments
    `(#:configure-flags '(;; Add `-static-libgcc' to not retain a dependency
                          ;; on GCC when bootstrapping.
                          "LDFLAGS=-static-libgcc"

                          ;; Turn on --enable-new-dtags by default to make the
                          ;; linker set RUNPATH instead of RPATH on binaries.
                          ;; This is important because RUNPATH can be overriden
                          ;; using LD_LIBRARY_PATH at runtime.
                          "--enable-new-dtags"

                          ;; Don't search under /usr/lib & co.
                          "--with-lib-path=/no-ld-lib-path"

                          ;; Install BFD.  It ends up in a hidden directory,
                          ;; but it's here.
                          "--enable-install-libbfd"

                          ;; Make sure 'ar' and 'ranlib' produce archives in a
                          ;; deterministic fashion.
                          "--enable-deterministic-archives")

      ;; XXX: binutils 2.34 was mistakenly released without generated manuals:
      ;; <https://sourceware.org/bugzilla/show_bug.cgi?id=25491>.  To avoid a
      ;; circular dependency on texinfo, prevent the build system from creating
      ;; the manuals by calling "true" instead of "makeinfo"...
      #:make-flags '("MAKEINFO=true")))

   ;; ...and "hide" this package so that users who install binutils get the
   ;; version with documentation defined below.
   (properties '((hidden? . #t)))

   (synopsis "Binary utilities: bfd gas gprof ld")
   (description
    "GNU Binutils is a collection of tools for working with binary files.
Perhaps the most notable are \"ld\", a linker, and \"as\", an assembler.
Other tools include programs to display binary profiling information, list
the strings in a binary file, and utilities for working with archives.  The
\"bfd\" library for working with executable and object formats is also
included.")
   (license gpl3+)
   (home-page "https://www.gnu.org/software/binutils/")))

;; Work around a problem with binutils 2.34 whereby manuals are missing from
;; the release tarball.  Remove this and the related code above when updating.
(define-public binutils+documentation
  (package/inherit
   binutils
   (native-inputs
    `(("texinfo" ,texinfo)))
   (arguments
    (substitute-keyword-arguments (package-arguments binutils)
      ((#:make-flags _ ''()) ''())))
   (properties '())))

;; FIXME: ath9k-firmware-htc-binutils.patch do not apply on 2.34 because of a
;; big refactoring of xtensa-modules.c (commit 567607c11fbf7105 upstream).
;; Keep this version around until the patch is updated.
(define-public binutils-2.33
  (package
   (inherit binutils)
   (version "2.33.1")
   (source (origin
             (inherit (package-source binutils))
             (uri (string-append "mirror://gnu/binutils/binutils-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "1cmd0riv37bqy9mwbg6n3523qgr8b3bbm5kwj19sjrasl4yq9d0c"))))
   (arguments
    (substitute-keyword-arguments (package-arguments binutils)
      ((#:make-flags _ ''()) ''())))
   (properties '())))

(define-public binutils-gold
  (package/inherit binutils+documentation
    (name "binutils-gold")
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'patch-source-shebangs 'patch-more-shebangs
           (lambda _
             (substitute* "gold/Makefile.in"
               (("/bin/sh") (which "sh")))
             #t)))
       ,@(substitute-keyword-arguments (package-arguments binutils)
         ; Upstream is aware of unrelocatable test failures on arm*.
         ((#:tests? _ #f)
          (if (any (cute string-prefix? <> (or (%current-target-system)
                                               (%current-system)))
                   '("i686" "x86_64"))
              '#t '#f))
         ((#:configure-flags flags)
          `(cons* "--enable-gold=default"
                 (delete "LDFLAGS=-static-libgcc" ,flags))))))
     (native-inputs
     `(("bc" ,bc)))
     (inputs
     `(("gcc:lib" ,(canonical-package gcc) "lib")))))

(define* (make-ld-wrapper name #:key
                          (target (const #f))
                          binutils
                          (guile (canonical-package guile-3.0))
                          (bash (canonical-package bash))
                          (guile-for-build guile))
  "Return a package called NAME that contains a wrapper for the 'ld' program
of BINUTILS, which adds '-rpath' flags to the actual 'ld' command line.  The
wrapper uses GUILE and BASH.

TARGET must be a one-argument procedure that, given a system type, returns a
cross-compilation target triplet or #f.  When the result is not #f, make a
wrapper for the cross-linker for that target, called 'TARGET-ld'."
  ;; Note: #:system->target-triplet is a procedure so that the evaluation of
  ;; its result can be delayed until the 'arguments' field is evaluated, thus
  ;; in a context where '%current-system' is accurate.
  (package
    (name name)
    (version "0")
    (source #f)
    (build-system trivial-build-system)
    (inputs `(("binutils" ,binutils)
              ("guile"    ,guile)
              ("bash"     ,bash)
              ("wrapper"  ,(search-path %load-path
                                        "gnu/packages/ld-wrapper.in"))))
    (arguments
     (let ((target (target (%current-system))))
       `(#:guile ,guile-for-build
         #:modules ((guix build utils))
         #:builder (begin
                     (use-modules (guix build utils)
                                  (system base compile))

                     (let* ((out (assoc-ref %outputs "out"))
                            (bin (string-append out "/bin"))
                            (ld  ,(if target
                                      `(string-append bin "/" ,target "-ld")
                                      '(string-append bin "/ld")))
                            (go  (string-append ld ".go")))

                       (setvbuf (current-output-port)
                                (cond-expand (guile-2.0 _IOLBF)
                                             (else 'line)))
                       (format #t "building ~s/bin/ld wrapper in ~s~%"
                               (assoc-ref %build-inputs "binutils")
                               out)

                       (mkdir-p bin)
                       (copy-file (assoc-ref %build-inputs "wrapper") ld)
                       (substitute* ld
                         (("@SELF@")
                          ld)
                         (("@GUILE@")
                          (string-append (assoc-ref %build-inputs "guile")
                                         "/bin/guile"))
                         (("@BASH@")
                          (string-append (assoc-ref %build-inputs "bash")
                                         "/bin/bash"))
                         (("@LD@")
                          (string-append (assoc-ref %build-inputs "binutils")
                                         ,(if target
                                              (string-append "/bin/"
                                                             target "-ld")
                                              "/bin/ld"))))
                       (chmod ld #o555)
                       (compile-file ld #:output-file go)
                       #t)))))
    (synopsis "The linker wrapper")
    (description
     "The linker wrapper (or @code{ld-wrapper}) wraps the linker to add any
missing @code{-rpath} flags, and to detect any misuse of libraries outside of
the store.")
    (home-page "https://www.gnu.org/software/guix//")
    (license gpl3+)))

(export make-ld-wrapper)

(define-public glibc
  ;; This is the GNU C Library, used on GNU/Linux and GNU/Hurd.  Prior to
  ;; version 2.28, GNU/Hurd used a different glibc branch.
  (package
   (name "glibc")
   (version "2.31")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/glibc/glibc-" version ".tar.xz"))
            (sha256
             (base32
              "05zxkyz9bv3j9h0xyid1rhvh3klhsmrpkf3bcs6frvlgyr2gwilj"))
            (snippet
             ;; Disable 'ldconfig' and /etc/ld.so.cache.  The latter is
             ;; required on LFS distros to avoid loading the distro's libc.so
             ;; instead of ours.
             '(begin
                (substitute* "sysdeps/unix/sysv/linux/configure"
                  (("use_ldconfig=yes")
                   "use_ldconfig=no"))
                #t))
            (modules '((guix build utils)))
            (patches (search-patches "glibc-ldd-x86_64.patch"
                                     "glibc-hidden-visibility-ldconfig.patch"
                                     "glibc-versioned-locpath.patch"
                                     "glibc-allow-kernel-2.6.32.patch"
                                     "glibc-reinstate-prlimit64-fallback.patch"
                                     "glibc-supported-locales.patch"
                                     "glibc-hurd-clock_t_centiseconds.patch"
                                     "glibc-hurd-clock_gettime_monotonic.patch"
                                     "glibc-hurd-signal-sa-siginfo.patch"))))
   (build-system gnu-build-system)

   ;; Glibc's <limits.h> refers to <linux/limit.h>, for instance, so glibc
   ;; users should automatically pull Linux headers as well.  On GNU/Hurd,
   ;; libc provides <hurd.h>, which includes a bunch of Hurd and Mach headers,
   ;; so both should be propagated.
   (propagated-inputs
    (if (hurd-target?)
        `(("hurd-core-headers" ,hurd-core-headers))
        `(("kernel-headers" ,linux-libre-headers))))

   (outputs '("out" "debug"
              "static"))                          ;9 MiB of .a files

   (arguments
    `(#:out-of-source? #t

      ;; The libraries have an empty RUNPATH, but some, such as the versioned
      ;; libraries (libdl-2.24.so, etc.) have ld.so marked as NEEDED.  Since
      ;; these libraries are always going to be found anyway, just skip
      ;; RUNPATH checks.
      #:validate-runpath? #f

      #:modules ((ice-9 ftw)
                 (srfi srfi-26)
                 (guix build utils)
                 (guix build gnu-build-system))

      #:configure-flags
      (list "--sysconfdir=/etc"

            ;; Installing a locale archive with all the locales is to
            ;; expensive (~100 MiB), so we rely on users to install the
            ;; locales they really want.
            ;;
            ;; Set the default locale path.  In practice, $LOCPATH may be
            ;; defined to point whatever locales users want.  However, setuid
            ;; binaries don't honor $LOCPATH, so they'll instead look into
            ;; $libc_cv_complocaledir; we choose /run/current-system/locale/X.Y,
            ;; with the idea that it is going to be populated by the sysadmin.
            ;; The "X.Y" sub-directory is because locale data formats are
            ;; incompatible across libc versions; see
            ;; <https://lists.gnu.org/archive/html/guix-devel/2015-08/msg00737.html>.
            ;;
            ;; `--localedir' is not honored, so work around it.
            ;; See <http://sourceware.org/ml/libc-alpha/2013-03/msg00093.html>.
            (string-append "libc_cv_complocaledir=/run/current-system/locale/"
                           ,(version-major+minor version))

            (string-append "--with-headers="
                           (assoc-ref ,(if (%current-target-system)
                                           '%build-target-inputs
                                           '%build-inputs)
                                      "kernel-headers")
                           "/include")

            ;; This is the default for most architectures as of GNU libc 2.26,
            ;; but we specify it explicitly for clarity and consistency.  See
            ;; "kernel-features.h" in the GNU libc for details.
            "--enable-kernel=3.2.0"

            ;; Use our Bash instead of /bin/sh.
            (string-append "BASH_SHELL="
                           (assoc-ref %build-inputs "bash")
                           "/bin/bash")

            ;; On GNU/Hurd we get discarded-qualifiers warnings for
            ;; 'device_write_inband' among other things.  Ignore them.
            ,@(if (hurd-target?)
                  '("--disable-werror")
                  '()))

      #:tests? #f                                 ; XXX
      #:phases (modify-phases %standard-phases
                 (add-before
                  'configure 'pre-configure
                  (lambda* (#:key inputs native-inputs outputs
                                  #:allow-other-keys)
                    (let* ((out  (assoc-ref outputs "out"))
                           (bin  (string-append out "/bin"))
                           ;; FIXME: Normally we would look it up only in INPUTS
                           ;; but cross-base uses it as a native input.
                           (bash (or (assoc-ref inputs "static-bash")
                                     (assoc-ref native-inputs "static-bash"))))
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

                      ;; Have `system' use that Bash.
                      (substitute* "sysdeps/posix/system.c"
                        (("#define[[:blank:]]+SHELL_PATH.*$")
                         (format #f "#define SHELL_PATH \"~a/bin/bash\"\n"
                                 bash)))

                      ;; Same for `popen'.
                      (substitute* "libio/iopopen.c"
                        (("/bin/sh")
                         (string-append bash "/bin/sh")))

                      ;; Same for the shell used by the 'exec' functions for
                      ;; scripts that lack a shebang.
                      (substitute* (find-files "." "^paths\\.h$")
                        (("#define[[:blank:]]+_PATH_BSHELL[[:blank:]].*$")
                         (string-append "#define _PATH_BSHELL \""
                                        bash "/bin/sh\"\n")))

                      ;; Nscd uses __DATE__ and __TIME__ to create a string to
                      ;; make sure the client and server come from the same
                      ;; libc.  Use something deterministic instead.
                      (substitute* "nscd/nscd_stat.c"
                        (("static const char compilation\\[21\\] =.*$")
                         (string-append
                          "static const char compilation[21] = \""
                          (string-take (basename out) 20) "\";\n")))

                      ;; Make sure we don't retain a reference to the
                      ;; bootstrap Perl.
                      (substitute* "malloc/mtrace.pl"
                        (("^#!.*")
                         ;; The shebang can be omitted, because there's the
                         ;; "bilingual" eval/exec magic at the top of the file.
                         "")
                        (("exec @PERL@")
                         "exec perl"))

                      #t)))

                 (add-after 'install 'move-static-libs
                   (lambda* (#:key outputs #:allow-other-keys)
                     ;; Move static libraries to the "static" output.
                     (define (static-library? file)
                       ;; Return true if FILE is a static library.  The
                       ;; "_nonshared.a" files are referred to by libc.so,
                       ;; libpthread.so, etc., which are in fact linker
                       ;; scripts.
                       (and (string-suffix? ".a" file)
                            (not (string-contains file "_nonshared"))))

                     (define (linker-script? file)
                       ;; Guess whether FILE, a ".a" file, is actually a
                       ;; linker script.
                       (and (not (ar-file? file))
                            (not (elf-file? file))))

                     (let* ((out    (assoc-ref outputs "out"))
                            (lib    (string-append out "/lib"))
                            (files  (scandir lib static-library?))
                            (static (assoc-ref outputs "static"))
                            (slib   (string-append static "/lib")))
                       (mkdir-p slib)
                       (for-each (lambda (base)
                                   (rename-file (string-append lib "/" base)
                                                (string-append slib "/" base)))
                                 files)

                       ;; Usually libm.a is a linker script so we need to
                       ;; change the file names in there to refer to STATIC
                       ;; instead of OUT.
                       (for-each (lambda (ld-script)
                                   (substitute* ld-script
                                     ((out) static)))
                                 (filter linker-script?
                                         (map (cut string-append slib "/" <>)
                                              files)))
                       #t)))

                 ,@(if (target-powerpc?)
                     '((add-after 'unpack 'apply-patch
                         (lambda* (#:key inputs #:allow-other-keys)
                           (let ((patch (assoc-ref inputs
                                                   "powerpc64le-patch")))
                             (invoke "patch" "--force" "-p1"
                                     "-i" patch)))))
                     '())
                 ,@(if (hurd-target?)
                       '((add-after 'install 'augment-libc.so
                           (lambda* (#:key outputs #:allow-other-keys)
                             (let* ((out (assoc-ref outputs "out")))
                               (substitute* (string-append out "/lib/libc.so")
                                 (("/[^ ]+/lib/libc.so.0.3")
                                  (string-append out "/lib/libc.so.0.3"
                                                 " libmachuser.so libhurduser.so"))))
                             #t)))
                       '()))))

   (inputs `(("static-bash" ,static-bash)))

   ;; To build the manual, we need Texinfo and Perl.  Gettext is needed to
   ;; install the message catalogs, with 'msgfmt'.
   (native-inputs `(("texinfo" ,texinfo)
                    ("perl" ,perl)
                    ("bison" ,bison)
                    ("gettext" ,gettext-minimal)
                    ("python" ,python-minimal)

                    ,@(if (target-powerpc?)
                        `(("powerpc64le-patch" ,@(search-patches
                                                   "glibc-ldd-powerpc.patch")))
                        '())
                    ,@(if (hurd-target?)
                          `(("mig" ,mig)
                            ("perl" ,perl))
                          '())))

   (native-search-paths
    ;; Search path for packages that provide locale data.  This is useful
    ;; primarily in build environments.  Use 'GUIX_LOCPATH' rather than
    ;; 'LOCPATH' to avoid interference with the host system's libc on foreign
    ;; distros.
    (list (search-path-specification
           (variable "GUIX_LOCPATH")
           (files '("lib/locale")))))

   (synopsis "The GNU C Library")
   (description
    "Any Unix-like operating system needs a C library: the library which
defines the \"system calls\" and other basic facilities such as open, malloc,
printf, exit...

The GNU C library is used as the C library in the GNU system and most systems
with the Linux kernel.")
   (license lgpl2.0+)
   (home-page "https://www.gnu.org/software/libc/")))

;; Below are old libc versions, which we use mostly to build locale data in
;; the old format (which the new libc cannot cope with.)

(define-public glibc-2.30
  (package
    (inherit glibc)
    (version "2.30")
    (source (origin
              (inherit (package-source glibc))
              (uri (string-append "mirror://gnu/glibc/glibc-" version ".tar.xz"))
              (sha256
               (base32
                "1bxqpg91d02qnaz837a5kamm0f43pr1il4r9pknygywsar713i72"))
              (patches (search-patches "glibc-ldd-x86_64.patch"
                                       "glibc-CVE-2019-19126.patch"
                                       "glibc-hidden-visibility-ldconfig.patch"
                                       "glibc-versioned-locpath.patch"
                                       "glibc-allow-kernel-2.6.32.patch"
                                       "glibc-reinstate-prlimit64-fallback.patch"
                                       "glibc-2.29-supported-locales.patch"))))))

(define-public glibc-2.29
  (package
    (inherit glibc)
    (version "2.29")
    (source (origin
              (inherit (package-source glibc))
              (uri (string-append "mirror://gnu/glibc/glibc-" version ".tar.xz"))
              (sha256
               (base32
                "0jzh58728flfh939a8k9pi1zdyalfzlxmwra7k0rzji5gvavivpk"))
              (patches (search-patches "glibc-ldd-x86_64.patch"
                                       "glibc-CVE-2019-7309.patch"
                                       "glibc-CVE-2019-9169.patch"
                                       "glibc-2.29-git-updates.patch"
                                       "glibc-hidden-visibility-ldconfig.patch"
                                       "glibc-versioned-locpath.patch"
                                       "glibc-allow-kernel-2.6.32.patch"
                                       "glibc-reinstate-prlimit64-fallback.patch"
                                       "glibc-2.29-supported-locales.patch"))))))

(define-public glibc-2.28
  (package
    (inherit glibc)
    (version "2.28")
    (source (origin
              (inherit (package-source glibc))
              (uri (string-append "mirror://gnu/glibc/glibc-" version ".tar.xz"))
              (sha256
               (base32
                "10iha5ynvdj5m62vgpgqbq4cwvc2yhyl2w9yyyjgfxmdmx8h145i"))
              (patches (search-patches "glibc-ldd-x86_64.patch"
                                       "glibc-2.28-git-fixes.patch"
                                       "glibc-hidden-visibility-ldconfig.patch"
                                       "glibc-versioned-locpath.patch"
                                       "glibc-allow-kernel-2.6.32.patch"
                                       "glibc-reinstate-prlimit64-fallback.patch"
                                       "glibc-hurd-magic-pid.patch"
                                       "glibc-2.28-supported-locales.patch"))))))

(define-public glibc-2.27
  (package
    (inherit glibc)
    (version "2.27")
    (source (origin
              (inherit (package-source glibc))
              (uri (string-append "mirror://gnu/glibc/glibc-" version ".tar.xz"))
              (sha256
               (base32
                "0wpwq7gsm7sd6ysidv0z575ckqdg13cr2njyfgrbgh4f65adwwji"))
              (patches (search-patches "glibc-ldd-x86_64.patch"
                                       "glibc-2.27-git-fixes.patch"
                                       "glibc-hidden-visibility-ldconfig.patch"
                                       "glibc-versioned-locpath.patch"
                                       "glibc-allow-kernel-2.6.32.patch"
                                       "glibc-reinstate-prlimit64-fallback.patch"
                                       "glibc-2.27-supported-locales.patch"
                                       "glibc-CVE-2018-11236.patch"
                                       "glibc-CVE-2018-11237.patch"))))
    (properties `((lint-hidden-cve . ("CVE-2017-18269")))))) ; glibc-2.27-git-fixes

(define-public (make-gcc-libc base-gcc libc)
  "Return a GCC that targets LIBC."
  (package (inherit base-gcc)
           (name (string-append (package-name base-gcc) "-"
                                (package-name libc) "-"
                                (package-version libc)))
           (arguments
            (ensure-keyword-arguments (package-arguments base-gcc)
                                      '(#:implicit-inputs? #f)))
           (native-inputs
            `(,@(package-native-inputs base-gcc)
              ,@(append (fold alist-delete (%final-inputs) '("libc" "libc:static")))
              ("libc" ,libc)
              ("libc:static" ,libc "static")))))

(define-public (make-glibc-locales glibc)
  (package
    (inherit glibc)
    (name "glibc-locales")
    (source (origin (inherit (package-source glibc))
                    ;; The patch for glibc 2.28 and earlier replaces the same
                    ;; content, but the context in the patch is different
                    ;; enough to fail to merge.
                    (patches (cons (search-patch
                                    (if (version>=? (package-version glibc)
                                                    "2.29")
                                        "glibc-locales.patch"
                                        "glibc-locales-2.28.patch"))
                                   (origin-patches (package-source glibc))))))
    (synopsis "All the locales supported by the GNU C Library")
    (description
     "This package provides all the locales supported by the GNU C Library,
more than 400 in total.  To use them set the @code{LOCPATH} environment variable
to the @code{share/locale} sub-directory of this package.")
    (outputs '("out"))                            ;110+ MiB
    (native-search-paths '())
    (arguments
     (let ((args `(#:tests? #f #:strip-binaries? #f
                   ,@(package-arguments glibc))))
       (substitute-keyword-arguments args
         ((#:modules modules '((guix build utils)
                               (guix build gnu-build-system)))
          `((srfi srfi-11)
            (gnu build locale)
            ,@modules))
         ((#:imported-modules modules '())
          `((gnu build locale)
            ,@%gnu-build-system-modules))
         ((#:phases phases)
          `(modify-phases ,phases
             (replace 'build
               (lambda _
                 (invoke "make" "localedata/install-locales"
                         "-j" (number->string (parallel-job-count)))))
             (add-after 'build 'symlink-normalized-codesets
               (lambda* (#:key outputs #:allow-other-keys)
                 ;; The above phase does not install locales with names using
                 ;; the "normalized codeset."  Thus, create symlinks like:
                 ;;   en_US.utf8 -> en_US.UTF-8
                 (define (locale-directory? file stat)
                   (and (file-is-directory? file)
                        (string-index (basename file) #\_)
                        (string-rindex (basename file) #\.)))

                 (let* ((out (assoc-ref outputs "out"))
                        (locales (find-files out locale-directory?
                                             #:directories? #t)))
                   (for-each (lambda (directory)
                               (let*-values (((base)
                                              (basename directory))
                                             ((name codeset)
                                              (locale->name+codeset base))
                                             ((normalized)
                                              (normalize-codeset codeset)))
                                 (unless (string=? codeset normalized)
                                   (symlink base
                                            (string-append (dirname directory)
                                                           "/" name "."
                                                           normalized)))))
                             locales)
                   #t)))
             (delete 'install)
             (delete 'move-static-libs)))
         ((#:configure-flags flags)
          `(append ,flags
                   ;; Use $(libdir)/locale/X.Y as is the case by default.
                   (list (string-append "libc_cv_complocaledir="
                                        (assoc-ref %outputs "out")
                                        "/lib/locale/"
                                        ,(version-major+minor
                                          (package-version glibc)))))))))))

(define %default-utf8-locales
  ;; These are the locales commonly used for tests---e.g., in Guile's i18n
  ;; tests.
  '("de_DE" "el_GR" "en_US" "fr_FR" "tr_TR"))
(define*-public (make-glibc-utf8-locales glibc #:key
                                         (locales %default-utf8-locales)
                                         (name "glibc-utf8-locales"))
  (define default-locales? (equal? locales %default-utf8-locales))
  (package
    (name name)
    (version (package-version glibc))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))

                   (let* ((libc      (assoc-ref %build-inputs "glibc"))
                          (gzip      (assoc-ref %build-inputs "gzip"))
                          (out       (assoc-ref %outputs "out"))
                          (localedir (string-append out "/lib/locale/"
                                                    ,(version-major+minor version))))
                     ;; 'localedef' needs 'gzip'.
                     (setenv "PATH" (string-append libc "/bin:" gzip "/bin"))

                     (mkdir-p localedir)
                     (for-each (lambda (locale)
                                 (define file
                                   ;; Use the "normalized codeset" by
                                   ;; default--e.g., "en_US.utf8".
                                   (string-append localedir "/" locale ".utf8"))

                                 (invoke "localedef" "--no-archive"
                                         "--prefix" localedir
                                         "-i" locale
                                         "-f" "UTF-8" file)

                                 ;; For backward compatibility with Guix
                                 ;; <= 0.8.3, add "xx_YY.UTF-8".
                                 (symlink (string-append locale ".utf8")
                                          (string-append localedir "/"
                                                         locale ".UTF-8")))
                               ',locales)
                     #t))))
    (native-inputs `(("glibc" ,glibc)
                     ("gzip" ,gzip)))
    (synopsis (if default-locales?
                  (P_ "Small sample of UTF-8 locales")
                  (P_ "Customized sample of UTF-8 locales")))
    (description
     (if default-locales?
         (P_ "This package provides a small sample of UTF-8 locales mostly useful in
test environments.")
         (format #f (P_ "This package provides the following UTF-8 locales:
@itemize
~{@item ~a~%~}
@end itemize~%")
                 locales)))
    (home-page (package-home-page glibc))
    (license (package-license glibc))))

(define-public glibc-locales
  (make-glibc-locales glibc))
(define-public glibc-utf8-locales
  (make-glibc-utf8-locales glibc))

;; Packages provided to ease use of binaries linked against the previous libc.
(define-public glibc-locales-2.29
  (package (inherit (make-glibc-locales glibc-2.29))
           (name "glibc-locales-2.29")))
(define-public glibc-utf8-locales-2.29
  (package (inherit (make-glibc-utf8-locales glibc-2.29))
           (name "glibc-utf8-locales-2.29")))

(define-public which
  (package
    (name "which")
    (version "2.21")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/which/which-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1bgafvy3ypbhhfznwjv1lxmd6mci3x1byilnnkc7gcr486wlb8pl"))))
    (build-system gnu-build-system)
    (home-page "https://gnu.org/software/which/")
    (synopsis "Find full path of shell commands")
    (description
     "The which program finds the location of executables in PATH, with a
variety of options.  It is an alternative to the shell \"type\" built-in
command.")
    (license gpl3+))) ; some files are under GPLv2+

(define-public glibc/hurd-headers
  (package (inherit glibc)
    (name "glibc-hurd-headers")
    (outputs '("out"))
    (propagated-inputs `(("gnumach-headers" ,gnumach-headers)
                         ("hurd-headers" ,hurd-headers)))
    (native-inputs
     `(("mig" ,(if (%current-target-system)
                   ;; XXX: When targeting i586-pc-gnu, we need a 32-bit MiG,
                   ;; hence this hack.
                   (package
                     (inherit mig)
                     (arguments `(#:system "i686-linux")))
                   mig))
       ,@(package-native-inputs glibc)))
    (arguments
     (substitute-keyword-arguments (package-arguments glibc)
       ;; We just pass the flags really needed to build the headers.
       ((#:configure-flags _)
        `(list "--enable-add-ons"
               "--host=i586-pc-gnu"))
       ((#:phases _)
        '(modify-phases %standard-phases
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (invoke "make" "install-headers")

               ;; Make an empty stubs.h to work around not being able to
               ;; produce a valid stubs.h and causing the build to fail. See
               ;; <http://lists.gnu.org/archive/html/guix-devel/2014-04/msg00233.html>.
               (let ((out (assoc-ref outputs "out")))
                 (close-port
                  (open-output-file
                   (string-append out "/include/gnu/stubs.h"))))
               #t))
           (delete 'build)))))))                  ; nothing to build

(define-public tzdata
  (package
    (name "tzdata")
    ;; This package should be kept in sync with python-pytz in (gnu packages
    ;; time).
    (version "2021a")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://data.iana.org/time-zones/releases/tzdata"
                   version ".tar.gz"))
             (sha256
              (base32
               "022fn6gkmp7pamlgab04x0dm5hnyn2m2fcnyr3pvm36612xd5rrr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags (let ((out (assoc-ref %outputs "out"))
                          (tmp (getenv "TMPDIR")))
                      (list (string-append "TOPDIR=" out)
                            (string-append "TZDIR=" out "/share/zoneinfo")
                            (string-append "TZDEFAULT=" out
                                           "/share/zoneinfo/localtime")

                            ;; Likewise for the C library routines.
                            (string-append "LIBDIR=" tmp "/lib")
                            (string-append "MANDIR=" tmp "/man")

                            ;; XXX: tzdata 2020b changed the on-disk format
                            ;; of the time zone files from 'fat' to 'slim'.
                            ;; Many packages (particularly evolution-data-server)
                            ;; can not yet handle the latter, so we stick with
                            ;; 'fat' for now.
                            ,@(if (version>=? (package-version this-package)
                                              "2020b")
                                  '("CPPFLAGS=-DZIC_BLOAT_DEFAULT='\"fat\"'")
                                  '())

                            "AWK=awk"
                            "CC=gcc"))
       #:modules ((guix build utils)
                  (guix build gnu-build-system)
                  (srfi srfi-1))
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key source inputs #:allow-other-keys)
             (invoke "tar" "xvf" source)
             (invoke "tar" "xvf" (assoc-ref inputs "tzcode"))))
         (add-after 'install 'post-install
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Move data in the right place.
             (let ((out (assoc-ref outputs "out")))
               ;; Discard zic, dump, and tzselect, already
               ;; provided by glibc.
               (delete-file-recursively (string-append out "/usr"))
               (symlink (string-append out "/share/zoneinfo")
                        (string-append out "/share/zoneinfo/posix"))
               (delete-file-recursively
                (string-append out "/share/zoneinfo-posix"))
               (copy-recursively (string-append out "/share/zoneinfo-leaps")
                                 (string-append out "/share/zoneinfo/right"))
               (delete-file-recursively
                (string-append out "/share/zoneinfo-leaps"))
               #t)))
         (delete 'configure))))
    (inputs `(("tzcode" ,(origin
                          (method url-fetch)
                          (uri (string-append
                                "https://data.iana.org/time-zones/releases/tzcode"
                                version ".tar.gz"))
                          (sha256
                           (base32
                            "1l02b0jiwp3fl0xd6227i69d26rmx3yrnq0ssq9vvdmm4jhvyipb"))))))
    (home-page "https://www.iana.org/time-zones")
    (synopsis "Database of current and historical time zones")
    (description "The Time Zone Database (often called tz or zoneinfo)
contains code and data that represent the history of local time for many
representative locations around the globe.  It is updated periodically to
reflect changes made by political bodies to time zone boundaries, UTC offsets,
and daylight-saving rules.")
    (license public-domain)))

;;; A "fixed" version of tzdata, which is used in the test suites of glib and R
;;; and a few other places. We can update this whenever we are able to rebuild
;;; thousands of packages (for example, in a core-updates rebuild). This package
;;; will typically be obsolete and should never be referred to by a built
;;; package.
(define-public tzdata-for-tests
  (hidden-package
   (package
     (inherit tzdata)
     (version "2019c")
     (source (origin
               (method url-fetch)
               (uri (string-append
                     "https://data.iana.org/time-zones/releases/tzdata"
                     version ".tar.gz"))
               (sha256
                (base32
                 "0z7w1yv37cfk8yhix2cillam091vgp1j4g8fv84261q9mdnq1ivr"))))
     (inputs
      `(("tzcode" ,(origin
                     (method url-fetch)
                     (uri (string-append
                           "https://data.iana.org/time-zones/releases/tzcode"
                           version ".tar.gz"))
                     (sha256
                      (base32
                       "1m3y2rnf1nggxxhxplab5zdd5whvar3ijyrv7lifvm82irkd7szn")))))))))

(define-public libiconv
  (package
    (name "libiconv")
    (version "1.15")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/libiconv/libiconv-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0y1ij745r4p48mxq84rax40p10ln7fc7m243p8k8sia519i3dxfc"))
              (modules '((guix build utils)))
              (snippet
               ;; Work around "declared gets" error on glibc systems (fixed by
               ;; Gnulib commit 66712c23388e93e5c518ebc8515140fa0c807348.)
               '(begin
                  (substitute* "srclib/stdio.in.h"
                    (("^#undef gets") "")
                    (("^_GL_WARN_ON_USE \\(gets.*") ""))
                  #t))))
    (build-system gnu-build-system)
    (synopsis "Character set conversion library")
    (description
     "libiconv provides an implementation of the iconv function for systems
that lack it.  iconv is used to convert between character encodings in a
program.  It supports a wide variety of different encodings.")
    (home-page "https://www.gnu.org/software/libiconv/")
    (license lgpl3+)))

(define* (libiconv-if-needed #:optional (target (%current-target-system)))
  "Return either a libiconv package specification to include in a dependency
list for platforms that have an incomplete libc, or the empty list.  If a
package needs iconv ,@(libiconv-if-needed) should be added."
  ;; POSIX C libraries provide iconv.  Platforms with an incomplete libc
  ;; without iconv, such as MinGW, must return the then clause.
  (if (target-mingw? target)
      `(("libiconv" ,libiconv))
      '()))

(define-public (canonical-package package)
  ;; Avoid circular dependency by lazily resolving 'commencement'.
  (let* ((iface (resolve-interface '(gnu packages commencement)))
         (proc  (module-ref iface 'canonical-package)))
    (proc package)))

(define-public (%final-inputs)
  "Return the list of \"final inputs\"."
  ;; Avoid circular dependency by lazily resolving 'commencement'.
  (let ((iface (resolve-interface '(gnu packages commencement))))
    (module-ref iface '%final-inputs)))

;;; base.scm ends here
