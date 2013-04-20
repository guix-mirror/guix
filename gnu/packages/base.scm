;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
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
                #:select (gpl3+ lgpl2.0+))
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
   (version "2.8")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/hello/hello-" version
                                ".tar.gz"))
            (sha256
             (base32 "0wqd8sjmxfskrflaxywc7gqw7sfawrfvdxd9skxawzfgyy0pzdz6"))))
   (build-system gnu-build-system)
   (arguments '(#:configure-flags
                `("--disable-dependency-tracking"
                  ,(string-append "--with-gawk="  ; for illustration purposes
                                 (assoc-ref %build-inputs "gawk")))))
   (inputs `(("gawk" ,gawk)))
   (synopsis "Hello, GNU world: An example GNU package")
   (description "Yeah...")
   (home-page "http://www.gnu.org/software/hello/")
   (license gpl3+)))

(define-public grep
  (package
   (name "grep")
   (version "2.14")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/grep/grep-"
                                version ".tar.xz"))
            (sha256
             (base32
              "1qbjb1l7f9blckc5pqy8jlf6482hpx4awn2acmhyf5mv9wfq03p7"))))
   (build-system gnu-build-system)
   (synopsis "Print lines matching a pattern")
   (description
    "The grep command searches one or more input files for lines containing a
match to a specified pattern.  By default, grep prints the matching
lines.")
   (license gpl3+)
   (home-page "http://www.gnu.org/software/grep/")))

(define-public sed
  (package
   (name "sed")
   (version "4.2.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/sed/sed-" version
                                ".tar.bz2"))
            (sha256
             (base32
              "13wlsb4sf5d5a82xjhxqmdvrrn36rmw5f0pl9qyb9zkvldnb7hra"))))
   (build-system gnu-build-system)
   (synopsis "Stream editor")
   (arguments
    `(#:phases (alist-cons-before
                'patch-source-shebangs 'patch-test-suite
                (lambda* (#:key inputs #:allow-other-keys)
                  (let ((bash (assoc-ref inputs "bash")))
                    (patch-makefile-SHELL "testsuite/Makefile.tests")
                    (substitute* '("testsuite/bsd.sh"
                                   "testsuite/bug-regex9.c")
                      (("/bin/sh")
                       (string-append bash "/bin/bash")))))
                %standard-phases)))
   (description
    "Sed (stream editor) isn't really a true text editor or text processor.
Instead, it is used to filter text, i.e., it takes text input and performs
some operation (or set of operations) on it and outputs the modified text.
Sed is typically used for extracting part of a file using pattern matching or
substituting multiple occurrences of a string within a file.")
   (license gpl3+)
   (home-page "http://www.gnu.org/software/sed/")))

(define-public tar
  (package
   (name "tar")
   (version "1.26")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/tar/tar-"
                                version ".tar.bz2"))
            (sha256
             (base32
              "0hbdkzmchq9ycr2x1pxqdcgdbaxksh8c6ac0jf75jajhcks6jlss"))))
   (build-system gnu-build-system)
   (inputs `(("patch/gets" ,(search-patch "tar-gets-undeclared.patch"))))
   (arguments
    `(#:patches (list (assoc-ref %build-inputs "patch/gets"))))
   (synopsis "Managing tar archives")
   (description
    "The Tar program provides the ability to create tar archives, as well as
various other kinds of manipulation.  For example, you can use Tar on
previously created archives to extract files, to store additional files, or
to update or list files which were already stored.

Initially, tar archives were used to store files conveniently on magnetic
tape.  The name \"Tar\" comes from this use; it stands for tape archiver.
Despite the utility's name, Tar can direct its output to available devices,
files, or other programs (using pipes), it can even access remote devices or
files (as archives).")
   (license gpl3+)
   (home-page "http://www.gnu.org/software/tar/")))

(define-public patch
  (package
   (name "patch")
   (version "2.6.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/patch/patch-"
                                version ".tar.xz"))
            (sha256
             (base32
              "18012gxs9wc96izskp1q7bclrwns6rdmkn4jj31c8jbyfz6l5npq"))))
   (build-system gnu-build-system)
   (native-inputs '())                      ; FIXME: needs `ed' for the tests
   (arguments
    '(#:tests? #f)
    ;; TODO: When cross-compiling, add this:
    ;;  '(#:configure-flags '("ac_cv_func_strnlen_working=yes"))
    )
   (synopsis "Apply differences to originals, with optional backups")
   (description
    "GNU Patch takes a patch file containing a difference listing produced by
the diff program and applies those differences to one or more original files,
producing patched versions.")
   (license gpl3+)
   (home-page "http://savannah.gnu.org/projects/patch/")))

(define-public diffutils
  (package
   (name "diffutils")
   (version "3.2")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/diffutils/diffutils-"
                                version ".tar.xz"))
            (sha256
             (base32
              "0jci0wv68025xd0s0rq4s5qxpx56dd9d730lka63qpzk1rfvfkxb"))))
   (build-system gnu-build-system)
   (inputs `(("patch/gets"
              ,(search-patch "diffutils-gets-undeclared.patch"))))
   (arguments `(#:patches (list (assoc-ref %build-inputs "patch/gets"))))
   (synopsis "Comparing and merging files")
   (description
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
              "0amn0bbwqvsvvsh6drfwz20ydc2czk374lzw5kksbh6bf78k4ks3"))))
   (build-system gnu-build-system)
   (native-inputs
    `(("patch/absolute-paths"
       ,(search-patch "findutils-absolute-paths.patch"))))
   (arguments
    `(#:patches (list (assoc-ref %build-inputs "patch/absolute-paths")))

    ;; TODO: Work around cross-compilation failure.
    ;; See <http://savannah.gnu.org/bugs/?27299#comment1>.
    ;; `(#:configure-flags '("gl_cv_func_wcwidth_works=yes")
    ;;   ,@(arguments cross-system))
    )
   (synopsis "Operating on files matching given criteria")
   (description
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
   (license gpl3+)
   (home-page "http://www.gnu.org/software/findutils/")))

(define-public coreutils
  (package
   (name "coreutils")
   (version "8.20")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/coreutils/coreutils-"
                                version ".tar.xz"))
            (sha256
             (base32
              "1cly97xdy3v4nbbx631k43smqw0nnpn651kkprs0yyl2cj3pkjyv"))))
   (build-system gnu-build-system)
   (inputs `(("acl"  ,acl)
             ("gmp"  ,gmp)
             ("perl" ,perl)))                     ; TODO: add SELinux
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
    "The GNU Core Utilities are the basic file, shell and text manipulation
utilities of the GNU operating system.  These are the core utilities which
are expected to exist on every operating system.")
   (license gpl3+)
   (home-page "http://www.gnu.org/software/coreutils/")))

(define-public gnu-make
  (package
   (name "make")
   (version "3.82")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/make/make-" version
                                ".tar.bz2"))
            (sha256
             (base32
              "0ri98385hsd7li6rh4l5afcq92v8l2lgiaz85wgcfh4w2wzsghg2"))))
   (build-system gnu-build-system)
   (native-inputs
    `(("patch/impure-dirs" ,(search-patch "make-impure-dirs.patch"))))
   (arguments
    '(#:patches (list (assoc-ref %build-inputs "patch/impure-dirs"))
      #:phases (alist-cons-before
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
    "Make is a tool which controls the generation of executables and other
non-source files of a program from the program's source files.

Make gets its knowledge of how to build your program from a file called the
makefile, which lists each of the non-source files and how to compute it from
other files. When you write a program, you should write a makefile for it, so
that it is possible to use Make to build and install the program.")
   (license gpl3+)
   (home-page "http://www.gnu.org/software/make/")))

(define-public binutils
  (package
   (name "binutils")
   (version "2.22")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/binutils/binutils-"
                                version ".tar.bz2"))
            (sha256
             (base32
              "1a9w66v5dwvbnawshjwqcgz7km6kw6ihkzp6sswv9ycc3knzhykc"))))
   (build-system gnu-build-system)

   ;; Split Binutils in several outputs, mostly to avoid collisions in
   ;; user profiles with GCC---e.g., libiberty.a.
   (outputs '("out"                        ; ar, ld, binutils.info, etc.
              "lib"))                      ; libbfd.a, bfd.h, etc.

   ;; TODO: Add dependency on zlib + those for Gold.
   (native-inputs
    `(("patch/new-dtags" ,(search-patch "binutils-ld-new-dtags.patch"))))
   (arguments
    `(#:patches (list (assoc-ref %build-inputs "patch/new-dtags"))
      #:configure-flags '(;; Add `-static-libgcc' to not retain a dependency
                          ;; on GCC when bootstrapping.
                          "LDFLAGS=-static-libgcc"

                          ;; Don't search under /usr/lib & co.
                          "--with-lib-path=/no-ld-lib-path")))

   (synopsis "Binary utilities: bfd gas gprof ld")
   (description
    "The GNU Binutils are a collection of binary tools.  The main ones are
`ld' (the GNU linker) and `as' (the GNU assembler).  They also include the
BFD (Binary File Descriptor) library, `gprof', `nm', `strip', etc.")
   (license gpl3+)
   (home-page "http://www.gnu.org/software/binutils/")))

(define-public binutils-2.23
  (package (inherit binutils)
    (version "2.23.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/binutils/binutils-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "15qhbkz3r266xaa52slh857qn3abw7rb2x2jnhpfrafpzrb4x4gy"))))))

(define-public glibc
  (package
   (name "glibc")
   (version "2.17")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/glibc/glibc-"
                                version ".tar.xz"))
            (sha256
             (base32
              "0gmjnn4kma9vgizccw1jv979xw55a8n1nkk94gg0l3hy80vy6539"))))
   (build-system gnu-build-system)

   ;; Glibc's <limits.h> refers to <linux/limit.h>, for instance, so glibc
   ;; users should automatically pull Linux headers as well.
   (propagated-inputs `(("linux-headers" ,linux-libre-headers)))

   (arguments
    `(#:out-of-source? #t
      #:patches (list (assoc-ref %build-inputs "patch/ld.so.cache"))
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
                       (string-append out "/bin/bash")))))
                %standard-phases)))
   (inputs `(("patch/ld.so.cache"
              ,(search-patch "glibc-no-ld-so-cache.patch"))
             ("static-bash" ,(static-package bash-light))))
   (synopsis "The GNU C Library")
   (description
    "Any Unix-like operating system needs a C library: the library which
defines the \"system calls\" and other basic facilities such as open, malloc,
printf, exit...

The GNU C library is used as the C library in the GNU system and most systems
with the Linux kernel.")
   (license lgpl2.0+)
   (home-page "http://www.gnu.org/software/libc/")))


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
                        (zero? (system* "./build.sh")))
               (alist-replace
                'install (lambda* (#:key outputs #:allow-other-keys)
                           (let* ((out (assoc-ref outputs "out"))
                                  (bin (string-append out "/bin")))
                             (mkdir-p bin)
                             (copy-file "make"
                                        (string-append bin "/make"))))
                ,phases))))))
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
             `(list ,(string-append "--target=" (boot-triplet)))))))
     (inputs %boot0-inputs))))

(define gcc-boot0
  (package-with-bootstrap-guile
   (package (inherit gcc-4.7)
     (name "gcc-cross-boot0")
     (arguments
      `(#:guile ,%bootstrap-guile
        #:implicit-inputs? #f
        #:modules ((guix build gnu-build-system)
                   (guix build utils)
                   (ice-9 regex)
                   (srfi srfi-1)
                   (srfi srfi-26))
        ,@(substitute-keyword-arguments (package-arguments gcc-4.7)
            ((#:configure-flags flags)
             `(append (list ,(string-append "--target=" (boot-triplet))

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
                                       ,(boot-triplet)
                                       "/" ,(package-version gcc-4.7))
                      (symlink "libgcc.a" "libgcc_eh.a"))))
                ,phases))))))

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
     (propagated-inputs `(("linux-headers" ,linux-libre-headers-boot0)))
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
  (package (inherit gcc-4.7)
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
                           (format p "#!~a/bin/bash
exec ~a/bin/~a-gcc -B~a/lib -Wl,-dynamic-linker -Wl,~a/~a \"$@\"~%"
                                   bash
                                   gcc triplet
                                   libc libc
                                   ,(glibc-dynamic-linker))))

                       (chmod "gcc" #o555))))))
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

(define-public binutils-final
  (package-with-bootstrap-guile
   (package (inherit binutils)
     (arguments
      `(#:guile ,%bootstrap-guile
        #:implicit-inputs? #f
        ,@(package-arguments binutils)))
     (inputs %boot2-inputs))))

(define-public gcc-final
  ;; The final GCC.
  (package (inherit gcc-boot0)
    (name "gcc")
    (arguments
     `(#:guile ,%bootstrap-guile
       #:implicit-inputs? #f

       ;; Build again GMP & co. within GCC's build process, because it's hard
       ;; to do outside (because GCC-BOOT0 is a cross-compiler, and thus
       ;; doesn't honor $LIBRARY_PATH, which breaks `gnu-build-system'.)
       ,@(substitute-keyword-arguments (package-arguments gcc-boot0)
           ((#:configure-flags boot-flags)
            (let loop ((args (package-arguments gcc-4.7)))
              (match args
                ((#:configure-flags normal-flags _ ...)
                 normal-flags)
                ((_ rest ...)
                 (loop rest)))))
           ((#:phases phases)
            `(alist-delete 'symlink-libgcc_eh ,phases)))))

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

(define-public bash-final
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

(define-public ld-wrapper
  ;; The final `ld' wrapper, which uses the final Guile.
  (package (inherit ld-wrapper-boot3)
    (name "ld-wrapper")
    (inputs `(("guile" ,guile-final)
              ("bash"  ,bash-final)
              ,@(fold alist-delete (package-inputs ld-wrapper-boot3)
                      '("guile" "bash"))))))

(define-public ld-wrapper-2.23         ; TODO: remove when Binutils is updated
  (package (inherit ld-wrapper)
    (inputs `(("binutils" ,binutils-2.23)
              ,@(alist-delete "binutils" (package-inputs ld-wrapper))))))

(define-public gcc-4.8
  ;; FIXME: Move to gcc.scm when Binutils is updated.
  (package (inherit gcc-4.7)
    (version "4.8.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/gcc/gcc-"
                                 version "/gcc-" version ".tar.bz2"))
             (sha256
              (base32
               "0b6cp9d1sas3vq6dj3zrgd134p9b569fqhbixb9cl7mp698zwdxh"))))
    (inputs `(("gmp" ,gmp)
              ("mpfr" ,mpfr)
              ("mpc" ,mpc)
              ("isl" ,isl)
              ("cloog" ,cloog)
              ("zlib" ,(@ (gnu packages compression) zlib))

              ;; With ld from Binutils 2.22, we get the following error while
              ;; linking gcov:
              ;; ld: gcov: hidden symbol `__deregister_frame_info' in /nix/store/47myfniw4x7kfc601d7q1yvz5mixlr00-gcc-4.7.2/lib/gcc/x86_64-unknown-linux-gnu/4.7.2/libgcc_eh.a(unwind-dw2-fde-dip.o) is referenced by DSO
              ;; See <http://gcc.gnu.org/bugzilla/show_bug.cgi?id=57015>.
              ("ld-wrapper" ,ld-wrapper-2.23)))))

(define-public %final-inputs
  ;; Final derivations used as implicit inputs by `gnu-build-system'.
  (let ((finalize (cut package-with-explicit-inputs <> %boot4-inputs
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

;;; base.scm ends here
