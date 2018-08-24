;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016, 2017 Nils Gillmann <ng0@n0.is>
;;; Copyright © 2016, 2017 Andy Patterson <ajpatter@uwaterloo.ca>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Benjamin Slade <slade@jnanam.net>
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

(define-module (gnu packages lisp)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system asdf)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages ed)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages libffcall)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libsigsegv)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xorg)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

(define (asdf-substitutions lisp)
  ;; Prepend XDG_DATA_DIRS/LISP-bundle-systems to ASDF's
  ;; 'default-system-source-registry'.
  `((("\\(,dir \"systems/\"\\)\\)")
     (format #f
             "(,dir \"~a-bundle-systems\")))

      ,@(loop :for dir :in (xdg-data-dirs \"common-lisp/\")
              :collect `(:directory (,dir \"systems\"))"
             ,lisp))))

(define-public gcl
  (let ((commit "5956140b1083e2302a59d7ce2054b0b7c2cbb417")
        (revision "1")) ;Guix package revision
    (package
      (name "gcl")
      (version (string-append "2.6.12-" revision "."
                              (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.savannah.gnu.org/r/gcl.git")
               (commit commit)))
         (file-name (string-append "gcl-" version "-checkout"))
         (sha256
          (base32 "0mwclf2879mh3d9xqkqhghf58lwy7srsnsq9x0f1cc6j302sy4hb"))))
      (build-system gnu-build-system)
      (arguments
       `(#:parallel-build? #f  ; The build system seems not to be thread safe.
         #:tests? #f  ; There does not seem to be make check or anything similar.
         #:configure-flags '("--enable-ansi") ; required for use by the maxima package
         #:make-flags (list
                       (string-append "GCL_CC=" (assoc-ref %build-inputs "gcc")
                                      "/bin/gcc")
                       (string-append "CC=" (assoc-ref %build-inputs "gcc")
                                      "/bin/gcc"))
         #:phases
         (modify-phases %standard-phases
           (add-before 'configure 'pre-conf
             (lambda* (#:key inputs #:allow-other-keys)
               (chdir "gcl")
               (substitute*
                   (append
                    '("pcl/impl/kcl/makefile.akcl"
                      "add-defs"
                      "unixport/makefile.dos"
                      "add-defs.bat"
                      "gcl-tk/makefile.prev"
                      "add-defs1")
                    (find-files "h" "\\.defs"))
                 (("SHELL=/bin/bash")
                  (string-append "SHELL=" (which "bash")))
                 (("SHELL=/bin/sh")
                  (string-append "SHELL=" (which "sh"))))
               (substitute* "h/linux.defs"
                 (("#CC") "CC")
                 (("-fwritable-strings") "")
                 (("-Werror") ""))
               (substitute* "lsp/gcl_top.lsp"
                 (("\"cc\"")
                  (string-append "\"" (assoc-ref %build-inputs "gcc")
                                 "/bin/gcc\""))
                 (("\\(or \\(get-path \\*cc\\*\\) \\*cc\\*\\)") "*cc*")
                 (("\"ld\"")
                  (string-append "\"" (assoc-ref %build-inputs "binutils")
                                 "/bin/ld\""))
                 (("\\(or \\(get-path \\*ld\\*\\) \\*ld\\*\\)") "*ld*")
                 (("\\(get-path \"objdump --source \"\\)")
                  (string-append "\"" (assoc-ref %build-inputs "binutils")
                                 "/bin/objdump --source \"")))
               #t))
           (add-after 'install 'wrap
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((gcl (assoc-ref outputs "out"))
                      (input-path (lambda (lib path)
                                    (string-append
                                     (assoc-ref inputs lib) path)))
                      (binaries '("binutils")))
                 ;; GCC and the GNU binutils are necessary for GCL to be
                 ;; able to compile Lisp functions and programs (this is
                 ;; a standard feature in Common Lisp). While the
                 ;; the location of GCC is specified in the make-flags,
                 ;; the GNU binutils must be available in GCL's $PATH.
                 (wrap-program (string-append gcl "/bin/gcl")
                   `("PATH" prefix ,(map (lambda (binary)
                                           (input-path binary "/bin"))
                                         binaries))))
               #t))
           ;; drop strip phase to make maxima build, see
           ;; https://www.ma.utexas.edu/pipermail/maxima/2008/009769.html
           (delete 'strip))))
      (inputs
       `(("gmp" ,gmp)
         ("readline" ,readline)))
      (native-inputs
       `(("gcc" ,gcc-4.9)
         ("m4" ,m4)
         ("texinfo" ,texinfo)
         ("texlive" ,texlive)))
      (home-page "https://www.gnu.org/software/gcl/")
      (synopsis "A Common Lisp implementation")
      (description "GCL is an implementation of the Common Lisp language.  It
features the ability to compile to native object code and to load native
object code modules directly into its lisp core.  It also features a
stratified garbage collection strategy, a source-level debugger and a built-in
interface to the Tk widget system.")
      (license license:lgpl2.0+))))

(define-public ecl
  (package
    (name "ecl")
    (version "16.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://common-lisp.net/project/ecl/static/files/release/"
             name "-" version ".tgz"))
       (sha256
        (base32 "0m0j24w5d5a9dwwqyrg0d35c0nys16ijb4r0nyk87yp82v38b9bn"))
       (modules '((guix build utils)))
       (snippet
        ;; Add ecl-bundle-systems to 'default-system-source-registry'.
        `(begin
           (substitute* "contrib/asdf/asdf.lisp"
             ,@(asdf-substitutions name))
           #t))))
    (build-system gnu-build-system)
    ;; src/configure uses 'which' to confirm the existence of 'gzip'.
    (native-inputs `(("which" ,which)))
    (inputs `(("gmp" ,gmp)
              ("libatomic-ops" ,libatomic-ops)
              ("libgc" ,libgc)
              ("libffi" ,libffi)))
    (arguments
     '(#:tests? #t
       #:parallel-tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'check)
         (add-after 'install 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((ecl (assoc-ref outputs "out"))
                    (input-path (lambda (lib path)
                                  (string-append
                                   (assoc-ref inputs lib) path)))
                    (libraries '("gmp" "libatomic-ops" "libgc" "libffi" "libc"))
                    (binaries  '("gcc" "ld-wrapper" "binutils"))
                    (library-directories
                     (map (lambda (lib) (input-path lib "/lib"))
                          libraries)))

               (wrap-program (string-append ecl "/bin/ecl")
                 `("PATH" prefix
                   ,(map (lambda (binary)
                           (input-path binary "/bin"))
                         binaries))
                 `("CPATH" suffix
                   ,(map (lambda (lib)
                           (input-path lib "/include"))
                         `("kernel-headers" ,@libraries)))
                 `("LIBRARY_PATH" suffix ,library-directories)
                 `("LD_LIBRARY_PATH" suffix ,library-directories)))))
         (add-after 'wrap 'check (assoc-ref %standard-phases 'check))
         (add-before 'check 'fix-path-to-ecl
           (lambda _
             (substitute* "build/tests/Makefile"
               (("\\$\\{exec_prefix\\}/") ""))
             #t)))))
    (native-search-paths
     (list (search-path-specification
            (variable "XDG_DATA_DIRS")
            (files '("share")))))
    (home-page "http://ecls.sourceforge.net/")
    (synopsis "Embeddable Common Lisp")
    (description "ECL is an implementation of the Common Lisp language as
defined by the ANSI X3J13 specification.  Its most relevant features are: a
bytecode compiler and interpreter, being able to compile Common Lisp with any
C/C++ compiler, being able to build standalone executables and libraries, and
supporting ASDF, Sockets, Gray streams, MOP, and other useful components.")
    ;; Note that the file "Copyright" points to some files and directories
    ;; which aren't under the lgpl2.0+ and instead contain many different,
    ;; non-copyleft licenses.
    (license license:lgpl2.0+)))

(define-public clisp
  (package
    (name "clisp")
    (version "2.49-92")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/gnu-clisp/clisp")
             (commit "clisp-2.49.92-2018-02-18")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0k2dmgl0miz3767iks4p0mvp6xw0ysyxhjpklyh11j010rmh6hqb"))
       (patches (search-patches "clisp-remove-failing-test.patch"))))
    (build-system gnu-build-system)
    (inputs `(("libffcall" ,libffcall)
              ("ncurses" ,ncurses)
              ("readline" ,readline)
              ("libsigsegv" ,libsigsegv)))
    (arguments
     `(#:configure-flags '(,@(if (string-prefix? "armhf-linux"
                                                 (or (%current-system)
                                                     (%current-target-system)))
                                 '("CFLAGS=-falign-functions=4")
                                 '())
                            "--with-dynamic-ffi"
                            "--with-dynamic-modules"
                            "--with-module=rawsock")
       #:build #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-sh-and-pwd
           (lambda _
             ;; The package is very messy with its references to "/bin/sh" and
             ;; some other absolute paths to traditional tools.  These appear in
             ;; many places where our automatic patching misses them.  Therefore
             ;; we do the following, in this early (post-unpack) phase, to solve
             ;; the problem from its root.
             (substitute* '("src/clisp-link.in"
                            "src/unix.d"
                            "src/makemake.in")
               (("/bin/sh") (which "sh")))
             (substitute* (find-files "." "configure|Makefile")
               (("/bin/sh") "sh"))
             (substitute* '("src/clisp-link.in")
               (("/bin/pwd") "pwd"))
             #t)))
       ;; Makefiles seem to have race conditions.
       #:parallel-build? #f))
    (home-page "https://clisp.sourceforge.io/")
    (synopsis "A Common Lisp implementation")
    (description
     "GNU CLISP is an implementation of ANSI Common Lisp.  Common Lisp is a
high-level, object-oriented functional programming language.  CLISP includes
an interpreter, a compiler, a debugger, and much more.")
    (license license:gpl2+)))

(define-public sbcl
  (package
    (name "sbcl")
    (version "1.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/sbcl/sbcl/" version "/sbcl-"
                           version "-source.tar.bz2"))
       (sha256
        (base32 "1k6v5b8qv7vyxvh8asx6phf2hbapx5pp5p5j47hgnq123fwnh4fa"))
       (modules '((guix build utils)))
       (snippet
        ;; Add sbcl-bundle-systems to 'default-system-source-registry'.
        `(begin
           (substitute* "contrib/asdf/asdf.lisp"
             ,@(asdf-substitutions name))
           #t))))
    (build-system gnu-build-system)
    (outputs '("out" "doc"))
    ;; Bootstrap with CLISP.
    (native-inputs
     `(("clisp" ,clisp)
       ("which" ,which)
       ("inetutils" ,inetutils)         ;for hostname(1)
       ("ed" ,ed)
       ("texlive" ,texlive)
       ("texinfo" ,texinfo)))
    (arguments
     '(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'patch-unix-tool-paths
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (bash (assoc-ref inputs "bash"))
                   (coreutils (assoc-ref inputs "coreutils"))
                   (ed (assoc-ref inputs "ed")))
               (define (quoted-path input path)
                 (string-append "\"" input path "\""))
               ;; Patch absolute paths in string literals.  Note that this
               ;; occurs in some .sh files too (which contain Lisp code).  Use
               ;; ISO-8859-1 because some of the files are ISO-8859-1 encoded.
               (with-fluids ((%default-port-encoding #f))
                 ;; The removed file is utf-16-be encoded, which gives substitute*
                 ;; trouble. It does not contain references to the listed programs.
                 (substitute* (delete
                               "./tests/data/compile-file-pos-utf16be.lisp"
                               (find-files "." "\\.(lisp|sh)$"))
                   (("\"/bin/sh\"") (quoted-path bash "/bin/sh"))
                   (("\"/usr/bin/env\"") (quoted-path coreutils "/usr/bin/env"))
                   (("\"/bin/cat\"") (quoted-path coreutils "/bin/cat"))
                   (("\"/bin/ed\"") (quoted-path ed "/bin/ed"))
                   (("\"/bin/echo\"") (quoted-path coreutils "/bin/echo"))
                   (("\"/bin/uname\"") (quoted-path coreutils "/bin/uname"))))
               ;; This one script has a non-string occurrence of /bin/sh.
               (substitute* '("tests/foreign.test.sh")
                 ;; Leave whitespace so we don't match the shebang.
                 ((" /bin/sh ") " sh "))
               ;; This file contains a module that can create executable files
               ;; which depend on the presence of SBCL.  It generates shell
               ;; scripts doing "exec sbcl ..." to achieve this.  We patch both
               ;; the shebang and the reference to "sbcl", tying the generated
               ;; executables to the exact SBCL package that generated them.
               (substitute* '("contrib/sb-executable/sb-executable.lisp")
                 (("/bin/sh") (string-append bash "/bin/sh"))
                 (("exec sbcl") (string-append "exec " out "/bin/sbcl")))
               ;; Disable some tests that fail in our build environment.
               (substitute* '("contrib/sb-bsd-sockets/tests.lisp")
                 ;; This requires /etc/protocols.
                 (("\\(deftest get-protocol-by-name/error" all)
                  (string-append "#+nil ;disabled by Guix\n" all)))
               (substitute* '("contrib/sb-posix/posix-tests.lisp")
                 ;; These assume some users/groups which we don't have.
                 (("\\(deftest pwent\\.[12]" all)
                  (string-append "#+nil ;disabled by Guix\n" all))
                 (("\\(deftest grent\\.[12]" all)
                  (string-append "#+nil ;disabled by Guix\n" all))))))
         (replace 'build
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "CC" "gcc")
             (zero? (system* "sh" "make.sh" "clisp"
                             (string-append "--prefix="
                                            (assoc-ref outputs "out"))))))
         (replace 'install
           (lambda _
             (zero? (system* "sh" "install.sh"))))
         (add-after 'build 'build-doc
           (lambda _
             (with-directory-excursion "doc/manual"
               (and  (zero? (system* "make" "info"))
                     (zero? (system* "make" "dist"))))))
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc"))
                    (old-doc-dir (string-append out "/share/doc"))
                    (new-doc/sbcl-dir (string-append doc "/share/doc/sbcl")))
               (rmdir (string-append old-doc-dir "/sbcl/html"))
               (mkdir-p new-doc/sbcl-dir)
               (copy-recursively (string-append old-doc-dir "/sbcl")
                                 new-doc/sbcl-dir)
               (delete-file-recursively old-doc-dir)
               #t))))
         ;; No 'check' target, though "make.sh" (build phase) runs tests.
         #:tests? #f))
    (native-search-paths
     (list (search-path-specification
            (variable "XDG_DATA_DIRS")
            (files '("share")))))
    (home-page "http://www.sbcl.org/")
    (synopsis "Common Lisp implementation")
    (description "Steel Bank Common Lisp (SBCL) is a high performance Common
Lisp compiler.  In addition to the compiler and runtime system for ANSI Common
Lisp, it provides an interactive environment including a debugger, a
statistical profiler, a code coverage tool, and many other extensions.")
    ;; Public domain in jurisdictions that allow it, bsd-2 otherwise.  MIT
    ;; loop macro has its own license.  See COPYING file for further notes.
    (license (list license:public-domain license:bsd-2
                   (license:x11-style "file://src/code/loop.lisp")))))

(define-public ccl
  (package
    (name "ccl")
    (version "1.11.5")
    (source #f)
    (build-system gnu-build-system)
    ;; CCL consists of a "lisp kernel" and "heap image", both of which are
    ;; shipped in precompiled form in source tarballs.  The former is a C
    ;; program which we can rebuild from scratch, but the latter cannot be
    ;; generated without an already working copy of CCL, and is platform
    ;; dependent, so we need to fetch the correct tarball for the platform.
    (inputs
     `(("ccl"
        ,(origin
           (method url-fetch)
           (uri (string-append
                 "https://github.com/Clozure/ccl/releases/download/v" version
                 "/ccl-" version "-"
                 (match (%current-system)
                   ((or "i686-linux" "x86_64-linux") "linuxx86")
                   ("armhf-linux" "linuxarm")
                   ;; Prevent errors when querying this package on unsupported
                   ;; platforms, e.g. when running "guix package --search="
                   (_ "UNSUPPORTED"))
                 ".tar.gz"))
           (sha256
            (base32
             (match (%current-system)
               ((or "i686-linux" "x86_64-linux")
                "0hs1f3z7crgzvinpj990kv9gvbsipxvcvwbmk54n51nasvc5025q")
               ("armhf-linux"
                "0p0l1dzsygb6i1xxgbipjpxkn46xhq3jm41a34ga1qqp4x8lkr62")
               (_ ""))))))))
    (native-inputs
     `(("m4" ,m4)
       ("subversion" ,subversion)))
    (arguments
     `(#:tests? #f                      ;no 'check' target
       #:modules ((srfi srfi-26)
                  (guix build utils)
                  (guix build gnu-build-system))
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key inputs #:allow-other-keys)
             (and (zero? (system* "tar" "xzvf" (assoc-ref inputs "ccl")))
                  (begin (chdir "ccl") #t))))
         (delete 'configure)
         (add-before 'build 'pre-build
           ;; Enter the source directory for the current platform's lisp
           ;; kernel, and run 'make clean' to remove the precompiled one.
           (lambda _
             (substitute* "lisp-kernel/m4macros.m4"
               (("/bin/pwd") (which "pwd")))
             (chdir (string-append
                     "lisp-kernel/"
                     ,(match (or (%current-target-system) (%current-system))
                        ("i686-linux"   "linuxx8632")
                        ("x86_64-linux" "linuxx8664")
                        ("armhf-linux"  "linuxarm")
                        ;; Prevent errors when querying this package
                        ;; on unsupported platforms, e.g. when running
                        ;; "guix package --search="
                        (_              "UNSUPPORTED"))))
             (substitute* '("Makefile")
               (("/bin/rm") "rm"))
             (setenv "CC" "gcc")
             (zero? (system* "make" "clean"))))
         ;; XXX Do we need to recompile the heap image as well for Guix?
         ;; For now just use the one we already got in the tarball.
         (replace 'install
           (lambda* (#:key outputs inputs #:allow-other-keys)
             ;; The lisp kernel built by running 'make' in lisp-kernel/$system
             ;; is put back into the original directory, so go back.  The heap
             ;; image is there as well.
             (chdir "../..")
             (let* ((out (assoc-ref outputs "out"))
                    (libdir (string-append out "/lib/"))
                    (bindir (string-append out "/bin/"))
                    (wrapper (string-append bindir "ccl"))
                    (bash (assoc-ref inputs "bash"))
                    (kernel
                     ,(match (or (%current-target-system) (%current-system))
                        ("i686-linux"   "lx86cl")
                        ("x86_64-linux" "lx86cl64")
                        ("armhf-linux"  "armcl")
                        ;; Prevent errors when querying this package
                        ;; on unsupported platforms, e.g. when running
                        ;; "guix package --search="
                        (_              "UNSUPPORTED")))
                    (heap (string-append kernel ".image")))
               (install-file kernel libdir)
               (install-file heap libdir)

               (let ((dirs '("lib" "library" "examples" "contrib"
                             "tools" "objc-bridge")))
                 (for-each copy-recursively
                           dirs
                           (map (cut string-append libdir <>) dirs)))

               (mkdir-p bindir)
               (with-output-to-file wrapper
                 (lambda ()
                   (display
                    (string-append
                     "#!" bash "/bin/sh\n"
                     "CCL_DEFAULT_DIRECTORY=" libdir "\n"
                     "export CCL_DEFAULT_DIRECTORY\n"
                     "exec " libdir kernel "\n"))))
               (chmod wrapper #o755))
             #t)))))
    (supported-systems '("i686-linux" "x86_64-linux" "armhf-linux"))
    (home-page "http://ccl.clozure.com/")
    (synopsis "Common Lisp implementation")
    (description "Clozure CL (often called CCL for short) is a Common Lisp
implementation featuring fast compilation speed, native threads, a precise,
generational, compacting garbage collector, and a convenient foreign-function
interface.")
    ;; See file doc/LICENSE for clarifications it makes regarding how the LGPL
    ;; applies to Lisp code according to them.
    (license (list license:lgpl2.1
                   license:clarified-artistic)))) ;TRIVIAL-LDAP package

(define-public femtolisp
  (let ((commit "68c5b1225572ecf2c52baf62f928063e5a30511b")
        (revision "1"))
    (package
      (name "femtolisp")
      (version (string-append "0.0.0-" revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/JeffBezanson/femtolisp.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "04rnwllxnl86zw8c6pwxznn49bvkvh0f1lfliy085vjzvlq3rgja"))))
      ;; See "utils.h" for supported systems. Upstream bug:
      ;; https://github.com/JeffBezanson/femtolisp/issues/25
      (supported-systems
       (fold delete %supported-systems
             '("armhf-linux" "mips64el-linux" "aarch64-linux")))
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags '("CC=gcc" "release")
         #:test-target "test"
         #:phases
         (modify-phases %standard-phases
           (delete 'configure) ; No configure script
           (replace 'install ; Makefile has no 'install phase
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin")))
                (install-file "flisp" bin)
                #t)))
           ;; The flisp binary is now available, run bootstrap to
           ;; generate flisp.boot and afterwards runs make test.
           (add-after 'install 'bootstrap-gen-and-test
             (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin")))
                (and
                 (zero? (system* "./bootstrap.sh"))
                 (install-file "flisp.boot" bin))))))))
      (synopsis "Scheme-like lisp implementation")
      (description
       "@code{femtolisp} is a scheme-like lisp implementation with a
simple, elegant Scheme dialect.  It is a lisp-1 with lexical scope.
The core is 12 builtin special forms and 33 builtin functions.")
      (home-page "https://github.com/JeffBezanson/femtolisp")
      (license license:bsd-3))))

(define-public lush2
  (package
    (name "lush2")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/lush/lush2/lush-"
                           version ".tar.gz"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (substitute* "src/unix.c"
             (("\\{ \"LUSH_DATE\", __DATE__ \\},") "")
             (("\\{ \"LUSH_TIME\", __TIME__ \\},") ""))
           (substitute* "src/main.c"
             (("\" \\(built \" __DATE__ \"\\)\"") ""))
           #t))
       (sha256
        (base32
         "02pkfn3nqdkm9fm44911dbcz0v3r0l53vygj8xigl6id5g3iwi4k"))))
    (build-system gnu-build-system)
    (arguments
     `(;; We have to add these LIBS so that they are found.
       #:configure-flags (list "LIBS=-lz"
                               "X_EXTRA_LIBS=-lfontconfig"
                               "--with-x")
       #:tests? #f)) ; No make check.
    (native-inputs `(("intltool" ,intltool)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("sdl" ,sdl)
       ("sdl-image" ,sdl-image)
       ("sdl-mixer" ,sdl-mixer)
       ("sdl-net" ,sdl-net)
       ("sdl-ttf" ,sdl-ttf)
       ("lapack" ,lapack)
       ("libxft" ,libxft)
       ("fontconfig" ,fontconfig)
       ("gsl" ,gsl)
       ("openblas" ,openblas)
       ("glu" ,glu)
       ("mesa" ,mesa)
       ("mesa-utils" ,mesa-utils)
       ("binutils" ,binutils)
       ("libiberty" ,libiberty)
       ("readline" ,readline)
       ("zlib" ,zlib)
       ("gettext-minimal" ,gettext-minimal)))
    (synopsis "Lisp Universal Shell")
    (description
     "Lush is an object-oriented Lisp interpreter/compiler with features
designed to please people who want to prototype large numerical
applications.  Lush includes an extensive library of
vector/matrix/tensor manipulation, numerous numerical libraries
(including GSL, LAPACK, and BLAS), a set of graphic functions, a
simple GUI toolkit, and interfaces to various graphic and multimedia
libraries such as OpenGL, SDL, Video4Linux, and ALSA (video/audio
grabbing), and others.  Lush is an ideal frontend script language for
programming projects written in C or other languages.  Lush also has
libraries for Machine Learning, Neural Nets and statistical estimation.")
    (home-page "http://lush.sourceforge.net/")
    (license license:lgpl2.1+)))

(define-public sbcl-alexandria
  (let ((revision "1")
        (commit "926a066611b7b11cb71e26c827a271e500888c30"))
    (package
      (name "sbcl-alexandria")
      (version (string-append "0.0.0-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.common-lisp.net/alexandria/alexandria.git")
               (commit commit)))
         (sha256
          (base32
           "18yncicdkh294j05rhgm23gzi36y9qy6vrfba8vg69jrxjp1hx8l"))
         (file-name (string-append "alexandria-" version "-checkout"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Collection of portable utilities for Common Lisp")
      (description
       "Alexandria is a collection of portable utilities.  It does not contain
conceptual extensions to Common Lisp.  It is conservative in scope, and
portable between implementations.")
      (home-page "https://common-lisp.net/project/alexandria/")
      (license license:public-domain))))

(define-public cl-alexandria
  (sbcl-package->cl-source-package sbcl-alexandria))

(define-public ecl-alexandria
  (sbcl-package->ecl-package sbcl-alexandria))

(define-public sbcl-fiveam
  (package
    (name "sbcl-fiveam")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/sionescu/fiveam/archive/v"
             version ".tar.gz"))
       (sha256
        (base32 "0f48pcbhqs3wwwzjl5nk57d4hcbib4l9xblxc66b8c2fhvhmhxnv"))
       (file-name (string-append "fiveam-" version ".tar.gz"))))
    (inputs `(("alexandria" ,sbcl-alexandria)))
    (build-system asdf-build-system/sbcl)
    (synopsis "Common Lisp testing framework")
    (description "FiveAM is a simple (as far as writing and running tests
goes) regression testing framework.  It has been designed with Common Lisp's
interactive development model in mind.")
    (home-page "https://common-lisp.net/project/fiveam/")
    (license license:bsd-3)))

(define-public cl-fiveam
  (sbcl-package->cl-source-package sbcl-fiveam))

(define-public ecl-fiveam
  (sbcl-package->ecl-package sbcl-fiveam))

(define-public sbcl-bordeaux-threads
  (let ((commit "354abb0ae9f1d9324001e1a8abab3128d7420e0e")
        (revision "1"))
    (package
      (name "sbcl-bordeaux-threads")
      (version (git-version "0.8.5" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/sionescu/bordeaux-threads.git")
                      (commit commit)))
                (sha256
                 (base32 "1hcfp21l6av1xj6z7r77sp6h4mwf9vvx4s745803sysq2qy2mwnq"))
                (file-name
                 (git-file-name "bordeaux-threads" version))))
      (inputs `(("alexandria" ,sbcl-alexandria)))
      (native-inputs `(("fiveam" ,sbcl-fiveam)))
      (build-system asdf-build-system/sbcl)
      (synopsis "Portable shared-state concurrency library for Common Lisp")
      (description "BORDEAUX-THREADS is a proposed standard for a minimal
MP/Threading interface.  It is similar to the CLIM-SYS threading and lock
support.")
      (home-page "https://common-lisp.net/project/bordeaux-threads/")
      (license license:x11))))

(define-public cl-bordeaux-threads
  (sbcl-package->cl-source-package sbcl-bordeaux-threads))

(define-public ecl-bordeaux-threads
  (sbcl-package->ecl-package sbcl-bordeaux-threads))

(define-public sbcl-trivial-gray-streams
  (let ((revision "1")
        (commit "0483ade330508b4b2edeabdb47d16ec9437ee1cb"))
    (package
      (name "sbcl-trivial-gray-streams")
      (version (string-append "0.0.0-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/trivial-gray-streams/trivial-gray-streams.git")
           (commit commit)))
         (sha256
          (base32 "0m3rpf2x0zmdk3nf1qfa01j6a55vj7gkwhyw78qslcgbjlgh8p4d"))
         (file-name
          (string-append "trivial-gray-streams-" version "-checkout"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Compatibility layer for Gray streams implementations")
      (description "Gray streams is an interface proposed for inclusion with
ANSI CL by David N. Gray.  The proposal did not make it into ANSI CL, but most
popular CL implementations implement it.  This package provides an extremely
thin compatibility layer for gray streams.")
      (home-page "http://www.cliki.net/trivial-gray-streams")
      (license license:x11))))

(define-public cl-trivial-gray-streams
  (sbcl-package->cl-source-package sbcl-trivial-gray-streams))

(define-public ecl-trivial-gray-streams
  (sbcl-package->ecl-package sbcl-trivial-gray-streams))

(define-public sbcl-flexi-streams
  (package
    (name "sbcl-flexi-streams")
    (version "1.0.16")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/edicl/flexi-streams/archive/v"
             version ".tar.gz"))
       (sha256
        (base32 "1fb0jrwxr5c3i2lhy7kn30m1n0vggfzwjm1dacx6y5wf9wfsbamw"))
       (file-name (string-append "flexi-streams-" version ".tar.gz"))))
    (build-system asdf-build-system/sbcl)
    (inputs `(("trivial-gray-streams" ,sbcl-trivial-gray-streams)))
    (synopsis "Implementation of virtual bivalent streams for Common Lisp")
    (description "Flexi-streams is an implementation of \"virtual\" bivalent
streams that can be layered atop real binary or bivalent streams and that can
be used to read and write character data in various single- or multi-octet
encodings which can be changed on the fly.  It also supplies in-memory binary
streams which are similar to string streams.")
    (home-page "http://weitz.de/flexi-streams/")
    (license license:bsd-3)))

(define-public cl-flexi-streams
  (sbcl-package->cl-source-package sbcl-flexi-streams))

(define-public ecl-flexi-streams
  (sbcl-package->ecl-package sbcl-flexi-streams))

(define-public sbcl-cl-ppcre
  (package
    (name "sbcl-cl-ppcre")
    (version "2.0.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/edicl/cl-ppcre/archive/v"
             version ".tar.gz"))
       (sha256
        (base32 "1i7daxf0wnydb0pgwiym7qh2wy70n14lxd6dyv28sy0naa8p31gd"))
       (file-name (string-append "cl-ppcre-" version ".tar.gz"))))
    (build-system asdf-build-system/sbcl)
    (native-inputs `(("flexi-streams" ,sbcl-flexi-streams)))
    (synopsis "Portable regular expression library for Common Lisp")
    (description "CL-PPCRE is a portable regular expression library for Common
Lisp, which is compatible with perl.  It is pretty fast, thread-safe, and
compatible with ANSI-compliant Common Lisp implementations.")
    (home-page "http://weitz.de/cl-ppcre/")
    (license license:bsd-2)))

(define-public cl-ppcre
  (sbcl-package->cl-source-package sbcl-cl-ppcre))

(define-public ecl-cl-ppcre
  (sbcl-package->ecl-package sbcl-cl-ppcre))

(define sbcl-cl-unicode-base
  (let ((revision "1")
        (commit "9fcd06fba1ddc9e66aed2f2d6c32dc9b764f03ea"))
    (package
      (name "sbcl-cl-unicode-base")
      (version (string-append "0.1.5-" revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/edicl/cl-unicode.git")
                      (commit commit)))
                (file-name (string-append "cl-unicode-" version "-checkout"))
                (sha256
                 (base32
                  "1jicprb5b3bv57dy1kg03572gxkcaqdjhak00426s76g0plmx5ki"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       '(#:asd-file "cl-unicode.asd"
         #:asd-system-name "cl-unicode/base"))
      (inputs
       `(("cl-ppcre" ,sbcl-cl-ppcre)))
      (home-page "http://weitz.de/cl-unicode/")
      (synopsis "Portable Unicode library for Common Lisp")
      (description "CL-UNICODE is a portable Unicode library Common Lisp, which
is compatible with perl.  It is pretty fast, thread-safe, and compatible with
ANSI-compliant Common Lisp implementations.")
      (license license:bsd-2))))

(define-public sbcl-cl-unicode
  (package
    (inherit sbcl-cl-unicode-base)
    (name "sbcl-cl-unicode")
    (inputs
     `(("cl-unicode/base" ,sbcl-cl-unicode-base)
       ,@(package-inputs sbcl-cl-unicode-base)))
    (native-inputs
     `(("flexi-streams" ,sbcl-flexi-streams)))
    (arguments '())))

(define-public ecl-cl-unicode
  (sbcl-package->ecl-package sbcl-cl-unicode))

(define-public cl-unicode
  (sbcl-package->cl-source-package sbcl-cl-unicode))

(define-public sbcl-clx
  (let ((revision "1")
        (commit "1c62774b03c1cf3fe6e5cb532df8b14b44c96b95"))
    (package
      (name "sbcl-clx")
      (version (string-append "0.0.0-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/sharplispers/clx.git")
           (commit commit)))
         (sha256
          (base32 "0qffag03ns52kwq9xjns2qg1yr0bf3ba507iwq5cmx5xz0b0rmjm"))
         (file-name (string-append "clx-" version "-checkout"))
         (patches
          (list
           (search-patch "clx-remove-demo.patch")))
         (modules '((guix build utils)))
         (snippet
          '(begin
             ;; These removed files cause the compiled system to crash when
             ;; loading.
             (delete-file-recursively "demo")
             (delete-file "test/trapezoid.lisp")
             (substitute* "clx.asd"
               (("\\(:file \"trapezoid\"\\)") ""))
             #t))))
      (build-system asdf-build-system/sbcl)
      (home-page "http://www.cliki.net/portable-clx")
      (synopsis "X11 client library for Common Lisp")
      (description "CLX is an X11 client library for Common Lisp.  The code was
originally taken from a CMUCL distribution, was modified somewhat in order to
make it compile and run under SBCL, then a selection of patches were added
from other CLXes around the net.")
      (license license:x11))))

(define-public cl-clx
  (sbcl-package->cl-source-package sbcl-clx))

(define-public ecl-clx
  (sbcl-package->ecl-package sbcl-clx))

(define-public sbcl-cl-ppcre-unicode
  (package (inherit sbcl-cl-ppcre)
    (name "sbcl-cl-ppcre-unicode")
    (arguments
     `(#:tests? #f ; tests fail with "Component :CL-PPCRE-TEST not found"
       #:asd-file "cl-ppcre-unicode.asd"))
    (inputs
     `(("sbcl-cl-ppcre" ,sbcl-cl-ppcre)
       ("sbcl-cl-unicode" ,sbcl-cl-unicode)))))

(define-public sbcl-stumpwm
  (package
    (name "sbcl-stumpwm")
    (version "18.05")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/stumpwm/stumpwm/archive/"
                    version ".tar.gz"))
              (sha256
               (base32 "1n2gaab3lwgf5r1hmwdcw13dkv9xdd7drn2shx28kfxvhdc9kbb9"))
              (file-name (string-append "stumpwm-" version ".tar.gz"))))
    (build-system asdf-build-system/sbcl)
    (inputs `(("cl-ppcre" ,sbcl-cl-ppcre)
              ("clx" ,sbcl-clx)
              ("alexandria" ,sbcl-alexandria)))
    (outputs '("out" "lib"))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'create-symlinks 'build-program
           (lambda* (#:key outputs #:allow-other-keys)
             (build-program
              (string-append (assoc-ref outputs "out") "/bin/stumpwm")
              outputs
              #:entry-program '((stumpwm:stumpwm) 0))))
         (add-after 'build-program 'create-desktop-file
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (xsessions (string-append out "/share/xsessions")))
               (mkdir-p xsessions)
               (call-with-output-file
                   (string-append xsessions "/stumpwm.desktop")
                 (lambda (file)
                   (format file
                    "[Desktop Entry]~@
                     Name=stumpwm~@
                     Comment=The Stump Window Manager~@
                     Exec=~a/bin/stumpwm~@
                     TryExec=~@*~a/bin/stumpwm~@
                     Icon=~@
                     Type=Application~%"
                    out)))
               #t))))))
    (synopsis "Window manager written in Common Lisp")
    (description "Stumpwm is a window manager written entirely in Common Lisp.
It attempts to be highly customizable while relying entirely on the keyboard
for input.  These design decisions reflect the growing popularity of
productive, customizable lisp based systems.")
    (home-page "https://github.com/stumpwm/stumpwm")
    (license license:gpl2+)
    (properties `((ecl-variant . ,(delay ecl-stumpwm))))))

(define-public cl-stumpwm
  (sbcl-package->cl-source-package sbcl-stumpwm))

(define-public ecl-stumpwm
  (let ((base (sbcl-package->ecl-package sbcl-stumpwm)))
    (package
      (inherit base)
      (outputs '("out"))
      (arguments '()))))

;; The slynk that users expect to install includes all of slynk's contrib
;; modules.  Therefore, we build the base module and all contribs first; then
;; we expose the union of these as `sbcl-slynk'.  The following variable
;; describes the base module.
(define sbcl-slynk-boot0
  (let ((revision "2")
        (commit "cbf84c36c4eca8b032e3fd16177a7bc02df3ec4c"))
    (package
      (name "sbcl-slynk-boot0")
      (version (string-append "1.0.0-beta-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/joaotavora/sly.git")
           (commit commit)))
         (sha256
          (base32 "13dyhsravn591p7g6is01mp2ynzjnnj7pwgi57r6xqmd4611y9vh"))
         (file-name (string-append "slynk-" version "-checkout"))
         (modules '((guix build utils)
                    (ice-9 ftw)))
         (snippet
          '(begin
             ;; Move the contribs into the main source directory for easier
             ;; access
             (substitute* "slynk/slynk.asd"
               (("\\.\\./contrib")
                "contrib")
               (("\\(defsystem :slynk/util")
                "(defsystem :slynk/util :depends-on (:slynk)")
               ((":depends-on \\(:slynk :slynk/util\\)")
                ":depends-on (:slynk :slynk-util)"))
             (substitute* "contrib/slynk-trace-dialog.lisp"
               (("\\(slynk::reset-inspector\\)") ; Causes problems on load
                "nil"))
             (substitute* "contrib/slynk-profiler.lisp"
               (("slynk:to-line")
                "slynk-pprint-to-line"))
             (substitute* "contrib/slynk-fancy-inspector.lisp"
               (("slynk/util") "slynk-util")
               ((":compile-toplevel :load-toplevel") ""))
             (rename-file "contrib" "slynk/contrib")
             ;; Move slynk's contents into the base directory for easier
             ;; access
             (for-each (lambda (file)
                         (unless (string-prefix? "." file)
                           (rename-file (string-append "slynk/" file)
                                        (string-append "./" (basename file)))))
                       (scandir "slynk"))
             #t))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:tests? #f ; No test suite
         #:asd-system-name "slynk"))
      (synopsis "Common Lisp IDE for Emacs")
      (description "SLY is a fork of SLIME, an IDE backend for Common Lisp.
It also features a completely redesigned REPL based on Emacs's own
full-featured comint.el, live code annotations, and a consistent interactive
button interface.  Everything can be copied to the REPL.  One can create
multiple inspectors with independent history.")
      (home-page "https://github.com/joaotavora/sly")
      (license license:public-domain)
      (properties `((cl-source-variant . ,(delay cl-slynk)))))))

(define-public cl-slynk
  (package
    (inherit (sbcl-package->cl-source-package sbcl-slynk-boot0))
    (name "cl-slynk")))

(define ecl-slynk-boot0
  (sbcl-package->ecl-package sbcl-slynk-boot0))

(define sbcl-slynk-arglists
  (package
    (inherit sbcl-slynk-boot0)
    (name "sbcl-slynk-arglists")
    (inputs `(("slynk" ,sbcl-slynk-boot0)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-slynk-boot0)
       ((#:asd-file _ "") "slynk.asd")
       ((#:asd-system-name _ #f) "slynk/arglists")))))

(define ecl-slynk-arglists
  (sbcl-package->ecl-package sbcl-slynk-arglists))

(define sbcl-slynk-util
  (package
    (inherit sbcl-slynk-boot0)
    (name "sbcl-slynk-util")
    (inputs `(("slynk" ,sbcl-slynk-boot0)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-slynk-boot0)
       ((#:asd-file _ "") "slynk.asd")
       ((#:asd-system-name _ #f) "slynk/util")))))

(define ecl-slynk-util
  (sbcl-package->ecl-package sbcl-slynk-util))

(define sbcl-slynk-fancy-inspector
  (package
    (inherit sbcl-slynk-arglists)
    (name "sbcl-slynk-fancy-inspector")
    (inputs `(("slynk-util" ,sbcl-slynk-util)
              ,@(package-inputs sbcl-slynk-arglists)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-slynk-arglists)
       ((#:asd-system-name _ #f) "slynk/fancy-inspector")))))

(define ecl-slynk-fancy-inspector
  (sbcl-package->ecl-package sbcl-slynk-fancy-inspector))

(define sbcl-slynk-package-fu
  (package
    (inherit sbcl-slynk-arglists)
    (name "sbcl-slynk-package-fu")
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-slynk-arglists)
       ((#:asd-system-name _ #f) "slynk/package-fu")))))

(define ecl-slynk-package-fu
  (sbcl-package->ecl-package sbcl-slynk-package-fu))

(define sbcl-slynk-mrepl
  (package
    (inherit sbcl-slynk-fancy-inspector)
    (name "sbcl-slynk-mrepl")
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-slynk-arglists)
       ((#:asd-system-name _ #f) "slynk/mrepl")))))

(define ecl-slynk-mrepl
  (sbcl-package->ecl-package sbcl-slynk-mrepl))

(define sbcl-slynk-trace-dialog
  (package
    (inherit sbcl-slynk-arglists)
    (name "sbcl-slynk-trace-dialog")
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-slynk-arglists)
       ((#:asd-system-name _ #f) "slynk/trace-dialog")))))

(define ecl-slynk-trace-dialog
  (sbcl-package->ecl-package sbcl-slynk-trace-dialog))

(define sbcl-slynk-profiler
  (package
    (inherit sbcl-slynk-arglists)
    (name "sbcl-slynk-profiler")
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-slynk-arglists)
       ((#:asd-system-name _ #f) "slynk/profiler")))))

(define ecl-slynk-profiler
  (sbcl-package->ecl-package sbcl-slynk-profiler))

(define sbcl-slynk-stickers
  (package
    (inherit sbcl-slynk-arglists)
    (name "sbcl-slynk-stickers")
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-slynk-arglists)
       ((#:asd-system-name _ #f) "slynk/stickers")))))

(define ecl-slynk-stickers
  (sbcl-package->ecl-package sbcl-slynk-stickers))

(define sbcl-slynk-indentation
  (package
    (inherit sbcl-slynk-arglists)
    (name "sbcl-slynk-indentation")
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-slynk-arglists)
       ((#:asd-system-name _ #f) "slynk/indentation")))))

(define ecl-slynk-indentation
  (sbcl-package->ecl-package sbcl-slynk-indentation))

(define sbcl-slynk-retro
  (package
    (inherit sbcl-slynk-arglists)
    (name "sbcl-slynk-retro")
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-slynk-arglists)
       ((#:asd-system-name _ #f) "slynk/retro")))))

(define ecl-slynk-retro
  (sbcl-package->ecl-package sbcl-slynk-retro))

(define slynk-systems
  '("slynk"
    "slynk-util"
    "slynk-arglists"
    "slynk-fancy-inspector"
    "slynk-package-fu"
    "slynk-mrepl"
    "slynk-profiler"
    "slynk-trace-dialog"
    "slynk-stickers"
    "slynk-indentation"
    "slynk-retro"))

(define-public sbcl-slynk
  (package
    (inherit sbcl-slynk-boot0)
    (name "sbcl-slynk")
    (inputs
     `(("slynk" ,sbcl-slynk-boot0)
       ("slynk-util" ,sbcl-slynk-util)
       ("slynk-arglists" ,sbcl-slynk-arglists)
       ("slynk-fancy-inspector" ,sbcl-slynk-fancy-inspector)
       ("slynk-package-fu" ,sbcl-slynk-package-fu)
       ("slynk-mrepl" ,sbcl-slynk-mrepl)
       ("slynk-profiler" ,sbcl-slynk-profiler)
       ("slynk-trace-dialog" ,sbcl-slynk-trace-dialog)
       ("slynk-stickers" ,sbcl-slynk-stickers)
       ("slynk-indentation" ,sbcl-slynk-indentation)
       ("slynk-retro" ,sbcl-slynk-retro)))
    (native-inputs `(("sbcl" ,sbcl)))
    (build-system trivial-build-system)
    (source #f)
    (outputs '("out" "image"))
    (arguments
     `(#:modules ((guix build union)
                  (guix build utils)
                  (guix build lisp-utils))
       #:builder
       (begin
         (use-modules (ice-9 match)
                      (srfi srfi-1)
                      (guix build union)
                      (guix build lisp-utils))

         (union-build
          (assoc-ref %outputs "out")
          (filter-map
           (match-lambda
             ((name . path)
              (if (string-prefix? "slynk" name) path #f)))
           %build-inputs))

         (prepend-to-source-registry
          (string-append (assoc-ref %outputs "out") "//"))

         (parameterize ((%lisp-type "sbcl")
                        (%lisp (string-append (assoc-ref %build-inputs "sbcl")
                                              "/bin/sbcl")))
           (build-image (string-append
                         (assoc-ref %outputs "image")
                         "/bin/slynk")
                        %outputs
                        #:dependencies ',slynk-systems))
         #t)))))

(define-public ecl-slynk
  (package
    (inherit sbcl-slynk)
    (name "ecl-slynk")
    (inputs
     (map (match-lambda
            ((name pkg . _)
             (list name (sbcl-package->ecl-package pkg))))
          (package-inputs sbcl-slynk)))
    (native-inputs '())
    (outputs '("out"))
    (arguments
     '(#:modules ((guix build union))
       #:builder
       (begin
         (use-modules (ice-9 match)
                      (guix build union))
         (match %build-inputs
           (((names . paths) ...)
            (union-build (assoc-ref %outputs "out")
                         paths)
            #t)))))))

(define-public sbcl-stumpwm+slynk
  (package
    (inherit sbcl-stumpwm)
    (name "sbcl-stumpwm-with-slynk")
    (outputs '("out"))
    (inputs
     `(("stumpwm" ,sbcl-stumpwm "lib")
       ("slynk" ,sbcl-slynk)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-stumpwm)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'build-program
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (program (string-append out "/bin/stumpwm")))
                 (build-program program outputs
                                #:entry-program '((stumpwm:stumpwm) 0)
                                #:dependencies '("stumpwm"
                                                 ,@slynk-systems)
                                #:dependency-prefixes
                                (map (lambda (input) (assoc-ref inputs input))
                                     '("stumpwm" "slynk")))
                 ;; Remove unneeded file.
                 (delete-file (string-append out "/bin/stumpwm-exec.fasl"))
                 #t)))
           (delete 'copy-source)
           (delete 'build)
           (delete 'check)
           (delete 'create-asd-file)
           (delete 'cleanup)
           (delete 'create-symlinks)))))))

(define-public sbcl-parse-js
  (let ((commit "fbadc6029bec7039602abfc06c73bb52970998f6")
        (revision "1"))
    (package
      (name "sbcl-parse-js")
      (version (string-append "0.0.0-" revision "." (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "http://marijn.haverbeke.nl/git/parse-js")
               (commit commit)))
         (file-name (string-append name "-" commit "-checkout"))
         (sha256
          (base32
           "1wddrnr5kiya5s3gp4cdq6crbfy9fqcz7fr44p81502sj3bvdv39"))))
      (build-system asdf-build-system/sbcl)
      (home-page "http://marijnhaverbeke.nl/parse-js/")
      (synopsis "Parse JavaScript")
      (description "Parse-js is a Common Lisp package for parsing
JavaScript (ECMAScript 3).  It has basic support for ECMAScript 5.")
      (license license:zlib))))

(define-public sbcl-parse-number
  (package
    (name "sbcl-parse-number")
    (version "1.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/sharplispers/parse-number/"
                           "archive/v" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1k6s4v65ksc1j5i0dprvzfvj213v6nah7i0rgd0726ngfjisj9ir"))))
    (build-system asdf-build-system/sbcl)
    (home-page "http://www.cliki.net/PARSE-NUMBER")
    (synopsis "Parse numbers")
    (description "@code{parse-number} is a library of functions for parsing
strings into one of the standard Common Lisp number types without using the
reader.  @code{parse-number} accepts an arbitrary string and attempts to parse
the string into one of the standard Common Lisp number types, if possible, or
else @code{parse-number} signals an error of type @code{invalid-number}.")
    (license license:bsd-3)))

(define-public sbcl-iterate
  (package
    (name "sbcl-iterate")
    ;; The latest official release (1.4.3) fails to build so we have to take
    ;; the current darcs tarball from quicklisp.
    (version "20160825")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://beta.quicklisp.org/archive/iterate/"
                           "2016-08-25/iterate-"
                           version "-darcs.tgz"))
       (sha256
        (base32
         "0kvz16gnxnkdz0fy1x8y5yr28nfm7i2qpvix7mgwccdpjmsb4pgm"))))
    (build-system asdf-build-system/sbcl)
    (home-page "https://common-lisp.net/project/iterate/")
    (synopsis "Iteration construct for Common Lisp")
    (description "@code{iterate} is an iteration construct for Common Lisp.
It is similar to the @code{CL:LOOP} macro, with these distinguishing marks:

@itemize
@item it is extensible,
@item it helps editors like Emacs indent iterate forms by having a more
  lisp-like syntax, and
@item it isn't part of the ANSI standard for Common Lisp.
@end itemize\n")
    (license license:expat)))

(define-public sbcl-cl-uglify-js
  ;; There have been many bug fixes since the 2010 release.
  (let ((commit "429c5e1d844e2f96b44db8fccc92d6e8e28afdd5")
        (revision "1"))
    (package
      (name "sbcl-cl-uglify-js")
      (version (string-append "0.1-" revision "." (string-take commit 9)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mishoo/cl-uglify-js.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0k39y3c93jgxpr7gwz7w0d8yknn1fdnxrjhd03057lvk5w8js27a"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("sbcl-parse-js" ,sbcl-parse-js)
         ("sbcl-cl-ppcre" ,sbcl-cl-ppcre)
         ("sbcl-cl-ppcre-unicode" ,sbcl-cl-ppcre-unicode)
         ("sbcl-parse-number" ,sbcl-parse-number)
         ("sbcl-iterate" ,sbcl-iterate)))
      (home-page "https://github.com/mishoo/cl-uglify-js")
      (synopsis "JavaScript compressor library for Common Lisp")
      (description "This is a Common Lisp version of UglifyJS, a JavaScript
compressor.  It works on data produced by @code{parse-js} to generate a
@dfn{minified} version of the code.  Currently it can:

@itemize
@item reduce variable names (usually to single letters)
@item join consecutive @code{var} statements
@item resolve simple binary expressions
@item group most consecutive statements using the ``sequence'' operator (comma)
@item remove unnecessary blocks
@item convert @code{IF} expressions in various ways that result in smaller code
@item remove some unreachable code
@end itemize\n")
      (license license:zlib))))

(define-public uglify-js
  (package
    (inherit sbcl-cl-uglify-js)
    (name "uglify-js")
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (let* ((bin    (string-append (assoc-ref %outputs "out") "/bin/"))
              (script (string-append bin "uglify-js")))
         (use-modules (guix build utils))
         (mkdir-p bin)
         (with-output-to-file script
           (lambda _
             (format #t "#!~a/bin/sbcl --script
 (require :asdf)
 (push (truename \"~a/lib/sbcl\") asdf:*central-registry*)"
                     (assoc-ref %build-inputs "sbcl")
                     (assoc-ref %build-inputs "sbcl-cl-uglify-js"))
             ;; FIXME: cannot use progn here because otherwise it fails to
             ;; find cl-uglify-js.
             (for-each
              write
              '(;; Quiet, please!
                (let ((*standard-output* (make-broadcast-stream))
                      (*error-output* (make-broadcast-stream)))
                  (asdf:load-system :cl-uglify-js))
                (let ((file (cadr *posix-argv*)))
                  (if file
                      (format t "~a"
                              (cl-uglify-js:ast-gen-code
                               (cl-uglify-js:ast-mangle
                                (cl-uglify-js:ast-squeeze
                                 (with-open-file (in file)
                                                 (parse-js:parse-js in))))
                               :beautify nil))
                      (progn
                       (format *error-output*
                               "Please provide a JavaScript file.~%")
                       (sb-ext:exit :code 1))))))))
         (chmod script #o755)
         #t)))
    (inputs
     `(("sbcl" ,sbcl)
       ("sbcl-cl-uglify-js" ,sbcl-cl-uglify-js)))
    (synopsis "JavaScript compressor")))

(define-public confusion-mdl
  (let* ((commit "12a055581fc262225272df43287dae48281900f5"))
    (package
      (name "confusion-mdl")
      (version "0.2")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url (string-append "https://gitlab.com/emacsomancer/" name))
                      (commit commit)))
                (sha256
                 (base32
                  "1zi8kflzvwqg97ha1sa5xjisbjs5z1mvbpa772vfxiv5ksnpxp0d"))
                (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ; there are no tests
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (replace 'build
             (lambda* (#:key (make-flags '()) #:allow-other-keys)
               (apply invoke "make" "CC=gcc" make-flags)))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin")))
                 (install-file "mdli" bin)
                 #t))))))
      (native-inputs
       `(("perl" ,perl)))
      (inputs
       `(("libgc" ,libgc)))
      (synopsis "Interpreter for the MIT Design Language (MDL)")
      (description "MDL (the MIT Design Language) is a descendant of Lisp.  It
was originally developed in 1971 on the PDP-10 computer under the Incompatible
Timesharing System (ITS) to provide high level language support for the
Dynamic Modeling Group at MIT's Project MAC.  Infocom built the original
PDP-10 Zork in MDL and their later ZIL (Zork Implementation Language) was
based on a subset of MDL.  Confusion is a MDL interpreter that works just well
enough to play the original mainframe Zork all the way through.")
      (home-page "http://www.russotto.net/git/mrussotto/confusion/src/master/src/README")
      (license license:gpl3+))))

(define-public sbcl-cl-strings
  (let ((revision "1")
        (commit "c5c5cbafbf3e6181d03c354d66e41a4f063f00ae"))
    (package
      (name "sbcl-cl-strings")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/diogoalexandrefranco/cl-strings")
               (commit commit)))
         (sha256
          (base32
           "00754mfaqallj480lwd346nkfb6ra8pa8xcxcylf4baqn604zlmv"))
         (file-name (string-append "cl-strings-" version "-checkout"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Portable, dependency-free set of utilities to manipulate strings in Common Lisp")
      (description
       "@command{cl-strings} is a small, portable, dependency-free set of
utilities that make it even easier to manipulate text in Common Lisp.  It has
100% test coverage and works at least on sbcl, ecl, ccl, abcl and clisp.")
      (home-page "https://github.com/diogoalexandrefranco/cl-strings")
      (license license:expat))))

(define-public cl-strings
  (sbcl-package->cl-source-package sbcl-cl-strings))

(define-public ecl-cl-strings
  (sbcl-package->ecl-package sbcl-cl-strings))

(define-public sbcl-trivial-features
  (package
    (name "sbcl-trivial-features")
    (version "0.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/trivial-features/trivial-features/archive/v"
             version ".tar.gz"))
       (sha256
        (base32 "0db1awn6jyhcfhyfvpjvfziprmq85cigf19mwbvaprhblydsag3c"))
       (file-name (string-append "trivial-features-" version ".tar.gz"))))
    (build-system asdf-build-system/sbcl)
    (arguments '(#:tests? #f))
    (home-page "http://cliki.net/trivial-features")
    (synopsis "Ensures consistency of @code{*FEATURES*} in Common Lisp")
    (description "Trivial-features ensures that @code{*FEATURES*} is
consistent across multiple Common Lisp implementations.")
    (license license:expat)))

(define-public cl-trivial-features
  (sbcl-package->cl-source-package sbcl-trivial-features))

(define-public ecl-trivial-features
  (sbcl-package->ecl-package sbcl-trivial-features))

(define-public sbcl-hu.dwim.asdf
  (let ((commit "170b0e4fdde3df0bc537327e7600575daac9e141"))
    (package
      (name "sbcl-hu.dwim.asdf")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/nixeagle/hu.dwim.asdf")
           (commit commit)))
         (sha256
          (base32 "10ax7p8y6vjqxzcq125p62kf68zi455a65ysgk0kl1f2v839c33v"))
         (file-name (git-file-name "hu.dwim.asdf" version))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://hub.darcs.net/hu.dwim/hu.dwim.asdf")
      (synopsis "Extensions to ASDF")
      (description "Various ASDF extensions such as attached test and
documentation system, explicit development support, etc.")
      (license license:public-domain))))

(define-public cl-hu.dwim.asdf
  (sbcl-package->cl-source-package sbcl-hu.dwim.asdf))

(define-public ecl-hu.dwim.asdf
  (sbcl-package->ecl-package sbcl-hu.dwim.asdf))

(define-public sbcl-hu.dwim.stefil
  (let ((commit "ab6d1aa8995878a1b66d745dfd0ba021090bbcf9"))
    (package
      (name "sbcl-hu.dwim.stefil")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://gitlab.common-lisp.net/xcvb/hu.dwim.stefil.git")
           (commit commit)))
         (sha256
          (base32 "1d8yccw65zj3zh46cbi3x6nmn1dwdb76s9d0av035077mvyirqqp"))
         (file-name (git-file-name "hu.dwim.stefil" version))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       `(("asdf:cl-hu.dwim.asdf" ,sbcl-hu.dwim.asdf)))
      (inputs
       `(("sbcl-alexandria" ,sbcl-alexandria)))
      (home-page "https://hub.darcs.net/hu.dwim/hu.dwim.stefil")
      (synopsis "Simple test framework")
      (description "Stefil is a simple test framework for Common Lisp,
with a focus on interactive development.")
      (license license:public-domain))))

(define-public cl-hu.dwim.stefil
  (sbcl-package->cl-source-package sbcl-hu.dwim.stefil))

(define-public ecl-hu.dwim.stefil
  (sbcl-package->ecl-package sbcl-hu.dwim.stefil))

(define-public sbcl-babel
  (package
    (name "sbcl-babel")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/cl-babel/babel/archive/v"
             version ".tar.gz"))
       (sha256
        (base32 "189kgbmslh36xx0d2i1g6a7mcvjryvjzkdlnhilqy5xs7hkyqirq"))
       (file-name (string-append name "-" version ".tar.gz"))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     `(("tests:cl-hu.dwim.stefil" ,sbcl-hu.dwim.stefil)))
    (inputs
     `(("sbcl-alexandria" ,sbcl-alexandria)
       ("sbcl-trivial-features" ,sbcl-trivial-features)))
    (home-page "https://common-lisp.net/project/babel/")
    (synopsis "Charset encoding and decoding library")
    (description "Babel is a charset encoding and decoding library, not unlike
GNU libiconv, but completely written in Common Lisp.")
    (license license:expat)))

(define-public cl-babel
  (sbcl-package->cl-source-package sbcl-babel))

(define-public ecl-babel
  (sbcl-package->ecl-package sbcl-babel))

(define-public sbcl-cl-yacc
  (package
    (name "sbcl-cl-yacc")
    (version "0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jech/cl-yacc")
             (commit (string-append "cl-yacc-" version))))
       (sha256
        (base32
         "16946pzf8vvadnyfayvj8rbh4zjzw90h0azz2qk1mxrvhh5wklib"))
       (file-name (string-append "cl-yacc-" version "-checkout"))))
    (build-system asdf-build-system/sbcl)
    (arguments
     `(#:asd-file "yacc.asd"
       #:asd-system-name "yacc"))
    (synopsis "LALR(1) parser generator for Common Lisp, similar in spirit to Yacc")
    (description
     "CL-Yacc is a LALR(1) parser generator for Common Lisp, similar in spirit
to AT&T Yacc, Berkeley Yacc, GNU Bison, Zebu, lalr.cl or lalr.scm.

CL-Yacc uses the algorithm due to Aho and Ullman, which is the one also used
by AT&T Yacc, Berkeley Yacc and Zebu.  It does not use the faster algorithm due
to DeRemer and Pennello, which is used by Bison and lalr.scm (not lalr.cl).")
    (home-page "https://www.irif.fr/~jch//software/cl-yacc/")
    (license license:expat)))

(define-public cl-yacc
  (sbcl-package->cl-source-package sbcl-cl-yacc))

(define-public ecl-cl-yacc
  (sbcl-package->ecl-package sbcl-cl-yacc))

(define-public sbcl-jpl-util
  (let ((commit "0311ed374e19a49d43318064d729fe3abd9a3b62"))
    (package
      (name "sbcl-jpl-util")
      (version "20151005")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               ;; Quicklisp uses this fork.
               (url "https://github.com/hawkir/cl-jpl-util")
               (commit commit)))
         (file-name
          (git-file-name "jpl-util" version))
         (sha256
          (base32
           "0nc0rk9n8grkg3045xsw34whmcmddn2sfrxki4268g7kpgz0d2yz"))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Collection of Common Lisp utility functions and macros")
      (description
       "@command{cl-jpl-util} is a collection of Common Lisp utility functions
and macros, primarily for software projects written in CL by the author.")
      (home-page "https://www.thoughtcrime.us/software/cl-jpl-util/")
      (license license:isc))))

(define-public cl-jpl-util
  (sbcl-package->cl-source-package sbcl-jpl-util))

(define-public ecl-jpl-util
  (sbcl-package->ecl-package sbcl-jpl-util))

(define-public sbcl-jpl-queues
  (package
    (name "sbcl-jpl-queues")
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://www.thoughtcrime.us/software/jpl-queues/jpl-queues-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1wvvv7j117h9a42qaj1g4fh4mji28xqs7s60rn6d11gk9jl76h96"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     `(("jpl-util" ,sbcl-jpl-util)
       ("bordeaux-threads" ,sbcl-bordeaux-threads)))
    (arguments
     ;; Tests seem to be broken.
     `(#:tests? #f))
    (synopsis "Common Lisp library implementing a few different kinds of queues")
    (description
     "A Common Lisp library implementing a few different kinds of queues:

@itemize
@item Bounded and unbounded FIFO queues.
@item Lossy bounded FIFO queues that drop elements when full.
@item Unbounded random-order queues that use less memory than unbounded FIFO queues.
@end itemize

Additionally, a synchronization wrapper is provided to make any queue
conforming to the @command{jpl-queues} API thread-safe for lightweight
multithreading applications.  (See Calispel for a more sophisticated CL
multithreaded message-passing library with timeouts and alternation among
several blockable channels.)")
    (home-page "https://www.thoughtcrime.us/software/jpl-queues/")
    (license license:isc)))

(define-public cl-jpl-queues
  (sbcl-package->cl-source-package sbcl-jpl-queues))

(define-public ecl-jpl-queues
  (sbcl-package->ecl-package sbcl-jpl-queues))

(define-public sbcl-eos
  (let ((commit "b0faca83781ead9a588661e37bd47f90362ccd94"))
    (package
      (name "sbcl-eos")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/adlai/Eos")
               (commit commit)))
         (sha256
          (base32
           "1bq8cfg087iyxmxi1mwgx5cfgy3b8ydrf81xljcis8qbgb2vszph"))
         (file-name (git-file-name "eos" version))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Unit Testing for Common Lisp")
      (description
       "Eos was a unit testing library for Common Lisp.
It began as a fork of FiveAM; however, FiveAM development has continued, while
that of Eos has not.  Thus, Eos is now deprecated in favor of FiveAM.")
      (home-page "https://github.com/adlai/Eos")
      (license license:expat))))

(define-public cl-eos
  (sbcl-package->cl-source-package sbcl-eos))

(define-public ecl-eos
  (sbcl-package->ecl-package sbcl-eos))

(define-public sbcl-esrap
  (let ((commit "133be8b05c2aae48696fe5b739eea2fa573fa48d"))
    (package
      (name "sbcl-esrap")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/nikodemus/esrap")
               (commit commit)))
         (sha256
          (base32
           "02d5clihsdryhf7pix8c5di2571fdsffh75d40fkzhws90r5mksl"))
         (file-name (git-file-name "esrap" version))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       `(("eos" ,sbcl-eos)))            ;For testing only.
      (inputs
       `(("alexandria" ,sbcl-alexandria)))
      (synopsis "Common Lisp packrat parser")
      (description
       "A packrat parser for Common Lisp.
In addition to regular Packrat / Parsing Grammar / TDPL features ESRAP supports:

@itemize
@item dynamic redefinition of nonterminals
@item inline grammars
@item semantic predicates
@item introspective facilities (describing grammars, tracing, setting breaks)
@end itemize\n")
      (home-page "https://nikodemus.github.io/esrap/")
      (license license:expat))))

(define-public cl-esrap
  (sbcl-package->cl-source-package sbcl-esrap))

(define-public ecl-esrap
  (sbcl-package->ecl-package sbcl-esrap))
