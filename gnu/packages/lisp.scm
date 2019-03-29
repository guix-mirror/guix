;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016, 2017 ng0 <ng0@n0.is>
;;; Copyright © 2016, 2017 Andy Patterson <ajpatter@uwaterloo.ca>
;;; Copyright © 2017, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Benjamin Slade <slade@jnanam.net>
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2019 Katherine Cox-Buday <cox.katherine.e@gmail.com>
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
  #:use-module (guix hg-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system ant)
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
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xdisorg)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-19))

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
  (let ((commit "d3335e2b3deb63f930eb0328e9b05377744c9512")
        (revision "2")) ;Guix package revision
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
          (base32 "05v86lhvsby05nzvcd3c4k0wljvgdgd0i6arzd2fx1yd67dl6fgj"))))
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
       `(("m4" ,m4)
         ("texinfo" ,texinfo)))
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
    (version "1.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/sbcl/sbcl/" version "/sbcl-"
                           version "-source.tar.bz2"))
       (sha256
        (base32 "08z62qba0kmm15k93s2rq7ipi769895g8iwigcp20qjh6amwnwph"))
       (modules '((guix build utils)))
       (snippet
        ;; Add sbcl-bundle-systems to 'default-system-source-registry'.
        `(begin
           (substitute* "contrib/asdf/asdf.lisp"
             ,@(asdf-substitutions name))
           #t))))
    (build-system gnu-build-system)
    (outputs '("out" "doc"))
    (native-inputs
     ;; From INSTALL:
     ;;     Supported build hosts are:
     ;;       SBCL
     ;;       CMUCL
     ;;       CCL (formerly known as OpenMCL)
     ;;       ABCL (recent versions only)
     ;;       CLISP (only some versions: 2.44.1 is OK, 2.47 is not)
     ;;       XCL
     ;; CCL seems ideal then, but it unfortunately only builds reliably
     ;; on some architectures.
     `(,@(match (%current-system)
           ((or "x86_64-linux" "i686-linux")
            `(("ccl" ,ccl)))
           (_
            `(("clisp" ,clisp))))
       ("which" ,which)
       ("inetutils" ,inetutils)         ;for hostname(1)
       ("ed" ,ed)
       ("texlive" ,(texlive-union (list texlive-tex-texinfo)))
       ("texinfo" ,texinfo)))
    (arguments
     `(#:modules ((guix build gnu-build-system)
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
         ;; FIXME: the texlive-union insists on regenerating fonts.  It stores
         ;; them in HOME, so it needs to be writeable.
         (add-before 'build 'set-HOME
           (lambda _ (setenv "HOME" "/tmp") #t))
         (replace 'build
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "CC" "gcc")
             (invoke "sh" "make.sh" ,@(match (%current-system)
                                        ((or "x86_64-linux" "i686-linux")
                                         `("ccl"))
                                        (_
                                         `("clisp")))
                     (string-append "--prefix="
                                    (assoc-ref outputs "out")))))
         (replace 'install
           (lambda _
             (invoke "sh" "install.sh")))
         (add-after 'build 'build-doc
           (lambda _
             (with-directory-excursion "doc/manual"
               (and  (invoke "make" "info")
                     (invoke "make" "dist")))))
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
             (invoke "tar" "xzvf" (assoc-ref inputs "ccl"))
             (chdir "ccl")
             #t))
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
             (invoke "make" "clean")))
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

               (let ((dirs '("lib" "library" "examples" "tools" "objc-bridge"
                             ,@(match (%current-system)
                                ("x86_64-linux"
                                 '("x86-headers64"))
                                ("i686-linux"
                                 '("x86-headers"))
                                (_ '())))))
                 (for-each copy-recursively
                           dirs
                           (map (cut string-append libdir <>) dirs)))

               (mkdir-p bindir)
               (with-output-to-file wrapper
                 (lambda ()
                   (display
                    (string-append
                     "#!" bash "/bin/sh\n"
                     "export CCL_DEFAULT_DIRECTORY=" libdir "\n"
                     "exec -a \"$0\" " libdir kernel " \"$@\"\n"))))
               (chmod wrapper #o755))
             #t)))))
    (supported-systems '("i686-linux" "x86_64-linux" "armhf-linux"))
    (home-page "https://ccl.clozure.com/")
    (synopsis "Common Lisp implementation")
    (description "Clozure CL (often called CCL for short) is a Common Lisp
implementation featuring fast compilation speed, native threads, a precise,
generational, compacting garbage collector, and a convenient foreign-function
interface.")
    ;; See file doc/LICENSE for clarifications it makes regarding how the LGPL
    ;; applies to Lisp code according to them.
    (license (list license:lgpl2.1
                   license:clarified-artistic)))) ;TRIVIAL-LDAP package

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

(define-public sbcl-fiasco
  (let ((commit "d62f7558b21addc89f87e306f65d7f760632655f")
        (revision "1"))
    (package
      (name "sbcl-fiasco")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/joaotavora/fiasco.git")
               (commit commit)))
         (file-name (git-file-name "fiasco" version))
         (sha256
          (base32
           "1zwxs3d6iswayavcmb49z2892xhym7n556d8dnmvalc32pm9bkjh"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("trivial-gray-streams" ,sbcl-trivial-gray-streams)))
      (synopsis "Simple and powerful test framework for Common Lisp")
      (description "A Common Lisp test framework that treasures your failures,
logical continuation of Stefil.  It focuses on interactive debugging.")
      (home-page "https://github.com/joaotavora/fiasco")
      ;; LICENCE specifies this is public-domain unless the legislation
      ;; doesn't allow or recognize it.  In that case it falls back to a
      ;; permissive licence.
      (license (list license:public-domain
                     (license:x11-style "file://LICENCE"))))))

(define-public cl-fiasco
  (sbcl-package->cl-source-package sbcl-fiasco))

(define-public ecl-fiasco
  (sbcl-package->ecl-package sbcl-fiasco))

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

(define-public stumpwm
  (package
    (name "stumpwm")
    (version "18.11")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/stumpwm/stumpwm/archive/"
                    version ".tar.gz"))
              (sha256
               (base32 "177gxfk4c127i9crghx6fmkipznhgylvzgnjb2pna38g21gg6s39"))
              (file-name (string-append "stumpwm-" version ".tar.gz"))
              (patches
               ;; This patch is included in the post-18.11 git master tree
               ;; and can be removed when we move to the next release.
               (search-patches "stumpwm-fix-broken-read-one-line.patch"))))
    (build-system asdf-build-system/sbcl)
    (native-inputs `(("fiasco" ,sbcl-fiasco)
                     ("texinfo" ,texinfo)))
    (inputs `(("cl-ppcre" ,sbcl-cl-ppcre)
              ("clx" ,sbcl-clx)
              ("alexandria" ,sbcl-alexandria)))
    (outputs '("out" "lib"))
    (arguments
     '(#:asd-system-name "stumpwm"
       #:phases
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
               #t)))
         (add-after 'install 'install-manual
           (lambda* (#:key outputs #:allow-other-keys)
             ;; The proper way to the manual is bootstrapping a full autotools
             ;; build system and running ‘./configure && make stumpwm.info’ to
             ;; do some macro substitution.  We can get away with much less.
             (let* ((out  (assoc-ref outputs "out"))
                    (info (string-append out "/share/info")))
               (invoke "makeinfo" "stumpwm.texi.in")
               (install-file "stumpwm.info" info)
               #t))))))
    (synopsis "Window manager written in Common Lisp")
    (description "Stumpwm is a window manager written entirely in Common Lisp.
It attempts to be highly customizable while relying entirely on the keyboard
for input.  These design decisions reflect the growing popularity of
productive, customizable lisp based systems.")
    (home-page "https://github.com/stumpwm/stumpwm")
    (license license:gpl2+)
    (properties `((cl-source-variant . ,(delay cl-stumpwm))))))

(define-public sbcl-stumpwm
  (deprecated-package "sbcl-stumpwm" stumpwm))

(define-public cl-stumpwm
  (package
    (inherit (sbcl-package->cl-source-package stumpwm))
    (name "cl-stumpwm")))

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

(define-public stumpwm+slynk
  (package
    (inherit stumpwm)
    (name "stumpwm-with-slynk")
    (outputs '("out"))
    (inputs
     `(("stumpwm" ,stumpwm "lib")
       ("slynk" ,sbcl-slynk)))
    (arguments
     (substitute-keyword-arguments (package-arguments stumpwm)
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

(define-public sbcl-stumpwm+slynk
  (deprecated-package "sbcl-stumpwm-with-slynk" stumpwm+slynk))

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
@item group most consecutive statements using the @code{sequence} operator (comma)
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

(define-public sbcl-split-sequence
  (package
    (name "sbcl-split-sequence")
    (version "1.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sharplispers/split-sequence")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "0c3zp6b7fmmp93sfhq112ind4zkld49ycw68z409xpnz3gc0wpf0"))
       (file-name (git-file-name "split-sequence" version))))
    (build-system asdf-build-system/sbcl)
    (arguments
     ;; TODO: Tests seem to be broken.
     ;; https://github.com/sharplispers/split-sequence/issues/8
     `(#:tests? #f))
    (synopsis "Member of the Common Lisp Utilities family of programs")
    (description
     "Splits sequence into a list of subsequences delimited by objects
satisfying the test.")
    (home-page "https://cliki.net/split-sequence")
    (license license:expat)))

(define-public cl-split-sequence
  (sbcl-package->cl-source-package sbcl-split-sequence))

(define-public ecl-split-sequence
  (sbcl-package->ecl-package sbcl-split-sequence))

(define-public sbcl-html-encode
  (package
    (name "sbcl-html-encode")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://beta.quicklisp.org/archive/html-encode/2010-10-06/html-encode-"
             version ".tgz"))
       (sha256
        (base32
         "06mf8wn95yf5swhmzk4vp0xr4ylfl33dgfknkabbkd8n6jns8gcf"))
       (file-name (string-append "colorize" version "-checkout"))))
    (build-system asdf-build-system/sbcl)
    (synopsis "Common Lisp library for encoding text in various web-savvy encodings")
    (description
     "A library for encoding text in various web-savvy encodings.")
    (home-page "http://quickdocs.org/html-encode/")
    (license license:expat)))

(define-public cl-html-encode
  (sbcl-package->cl-source-package sbcl-html-encode))

(define-public ecl-html-encode
  (sbcl-package->ecl-package sbcl-html-encode))

(define-public sbcl-colorize
  (let ((commit "ea676b584e0899cec82f21a9e6871172fe3c0eb5"))
    (package
      (name "sbcl-colorize")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/kingcons/colorize")
               (commit commit)))
         (sha256
          (base32
           "1pdg4kiaczmr3ivffhirp7m3lbr1q27rn7dhaay0vwghmi31zcw9"))
         (file-name (git-file-name "colorize" version))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("split-sequence" ,sbcl-split-sequence)
         ("html-encode" ,sbcl-html-encode)))
      (synopsis "Common Lisp for syntax highlighting")
      (description
       "@command{colorize} is a Lisp library for syntax highlighting
supporting the following languages: Common Lisp, Emacs Lisp, Scheme, Clojure,
C, C++, Java, Python, Erlang, Haskell, Objective-C, Diff, Webkit.")
      (home-page "https://github.com/kingcons/colorize")
      ;; TODO: Missing license?
      (license license:expat))))

(define-public cl-colorize
  (sbcl-package->cl-source-package sbcl-colorize))

(define-public ecl-colorize
  (sbcl-package->ecl-package sbcl-colorize))

(define-public sbcl-3bmd
  (let ((commit "192ea13435b605a96ef607df51317056914cabbd"))
    (package
      (name "sbcl-3bmd")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/3b/3bmd")
               (commit commit)))
         (sha256
          (base32
           "1rgv3gi7wf963ikmmpk132wgn0icddf226gq3bmcnk1fr3v9gf2f"))
         (file-name (git-file-name "3bmd" version))))
      (build-system asdf-build-system/sbcl)
      (arguments
       ;; FIXME: We need to specify the name because the build-system thinks
       ;; "3" is a version marker.
       `(#:asd-system-name "3bmd"))
      (inputs
       `(("esrap" ,sbcl-esrap)
         ("split-sequence" ,sbcl-split-sequence)))
      (synopsis "Markdown processor in Command Lisp using esrap parser")
      (description
       "Common Lisp Markdown -> HTML converter, using @command{esrap} for
parsing, and grammar based on @command{peg-markdown}.")
      (home-page "https://github.com/3b/3bmd")
      (license license:expat))))

(define-public cl-3bmd
  (sbcl-package->cl-source-package sbcl-3bmd))

(define-public ecl-3bmd
  (sbcl-package->ecl-package sbcl-3bmd))

(define-public sbcl-3bmd-ext-code-blocks
  (let ((commit "192ea13435b605a96ef607df51317056914cabbd"))
    (package
      (inherit sbcl-3bmd)
      (name "sbcl-3bmd-ext-code-blocks")
      (arguments
       `(#:asd-system-name "3bmd-ext-code-blocks"
         #:asd-file "3bmd-ext-code-blocks.asd"))
      (inputs
       `(("3bmd" ,sbcl-3bmd)
         ("colorize" ,sbcl-colorize)))
      (synopsis "3bmd extension which adds support for GitHub-style fenced
code blocks")
      (description
       "3bmd extension which adds support for GitHub-style fenced code blocks,
with @command{colorize} support."))))

(define-public cl-3bmd-ext-code-blocks
  (sbcl-package->cl-source-package sbcl-3bmd-ext-code-blocks))

(define-public ecl-3bmd-ext-code-blocks
  (sbcl-package->ecl-package sbcl-3bmd-ext-code-blocks))

(define-public sbcl-cl-fad
  (package
    (name "sbcl-cl-fad")
    (version "0.7.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/edicl/cl-fad/")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "1l1qmk9z57q84bz5r04sxsksggsnd7dgkxlybzh9imz6ma7sm52m"))
       (file-name (string-append "cl-fad" version "-checkout"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     `(("bordeaux-threads" ,sbcl-bordeaux-threads)))
    (synopsis "Portable pathname library for Common Lisp")
    (description
     "CL-FAD (for \"Files and Directories\") is a thin layer atop Common
Lisp's standard pathname functions.  It is intended to provide some
unification between current CL implementations on Windows, OS X, Linux, and
Unix.  Most of the code was written by Peter Seibel for his book Practical
Common Lisp.")
    (home-page "https://edicl.github.io/cl-fad/")
    (license license:bsd-2)))

(define-public cl-fad
  (sbcl-package->cl-source-package sbcl-cl-fad))

(define-public ecl-cl-fad
  (sbcl-package->ecl-package sbcl-cl-fad))

(define-public sbcl-rt
  (package
    (name "sbcl-rt")
    (version "1990.12.19")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://beta.quicklisp.org/archive/rt/2010-10-06/rt-"
                           "20101006-git" ".tgz"))
       (sha256
        (base32
         "1jncar0xwkqk8yrc2dln389ivvgzs7ijdhhs3zpfyi5d21f0qa1v"))))
    (build-system asdf-build-system/sbcl)
    (synopsis "MIT Regression Tester")
    (description
     "RT provides a framework for writing regression test suites.")
    (home-page "https://github.com/sharplispers/nibbles")
    (license license:unlicense)))

(define-public cl-rt
  (sbcl-package->cl-source-package sbcl-rt))

(define-public ecl-rt
  (sbcl-package->ecl-package sbcl-rt))

(define-public sbcl-nibbles
  (package
    (name "sbcl-nibbles")
    (version "0.14")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sharplispers/nibbles/")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "1v7qfgpvdr6nz7v63dj69d26dis0kff3rd8xamr1llfdvza2pm8f"))
       (file-name (git-file-name "nibbles" version))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     ;; Tests only.
     `(("rt" ,sbcl-rt)))
    (synopsis "Common Lisp library for accessing octet-addressed blocks of data")
    (description
     "When dealing with network protocols and file formats, it's common to
have to read or write 16-, 32-, or 64-bit datatypes in signed or unsigned
flavors.  Common Lisp sort of supports this by specifying :element-type for
streams, but that facility is underspecified and there's nothing similar for
read/write from octet vectors.  What most people wind up doing is rolling their
own small facility for their particular needs and calling it a day.

This library attempts to be comprehensive and centralize such
facilities.  Functions to read 16-, 32-, and 64-bit quantities from octet
vectors in signed or unsigned flavors are provided; these functions are also
SETFable.  Since it's sometimes desirable to read/write directly from streams,
functions for doing so are also provided.  On some implementations,
reading/writing IEEE singles/doubles (i.e. single-float and double-float) will
also be supported.")
    (home-page "https://github.com/sharplispers/nibbles")
    (license license:bsd-3)))

(define-public cl-nibbles
  (sbcl-package->cl-source-package sbcl-nibbles))

(define-public ecl-nibbles
  (sbcl-package->ecl-package sbcl-nibbles))

(define-public sbcl-ironclad
  (package
    (name "sbcl-ironclad")
    (version "0.42")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sharplispers/ironclad/")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "1wjcb9vpybxjrmch7f7s78a5abxmnknbd4fl49dl5lz8a3fc8vf0"))
       (file-name (string-append "ironblad" version "-checkout"))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     ;; Tests only.
     `(("rt" ,sbcl-rt)))
    (inputs
     `(("flexi-streams" ,sbcl-flexi-streams)
       ("nibbles" ,sbcl-nibbles)))
    (synopsis "Cryptographic toolkit written in Common Lisp")
    (description
     "Ironclad is a cryptography library written entirely in Common Lisp.
It includes support for several popular ciphers, digests, MACs and public key
cryptography algorithms.  For several implementations that support Gray
streams, support is included for convenient stream wrappers.")
    (home-page "https://github.com/sharplispers/ironclad")
    (license license:bsd-3)))

(define-public cl-ironclad
  (sbcl-package->cl-source-package sbcl-ironclad))

(define-public ecl-ironclad
  (sbcl-package->ecl-package sbcl-ironclad))

(define-public sbcl-named-readtables
  (let ((commit "4dfb89fa1af6b305b6492b8af042f5190c11e9fc")
        (revision "1"))
    (package
      (name "sbcl-named-readtables")
      (version (string-append "0.9-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/melisgl/named-readtables.git")
               (commit commit)))
         (sha256
          (base32 "083kgh5462iqbb4px6kq8s7sggvpvkm36hx4qi9rnaw53b6ilqkk"))
         (file-name (git-file-name "named-readtables" version))))
      (build-system asdf-build-system/sbcl)
      (arguments
       ;; Tests seem to be broken.
       `(#:tests? #f))
      (home-page "https://github.com/melisgl/named-readtables/")
      (synopsis "Library that creates a namespace for named readtables")
      (description "Named readtables is a library that creates a namespace for
named readtables, which is akin to package namespacing in Common Lisp.")
      (license license:bsd-3))))

(define-public cl-named-readtables
  (sbcl-package->cl-source-package sbcl-named-readtables))

(define-public ecl-named-readtables
  (sbcl-package->ecl-package sbcl-named-readtables))

(define-public sbcl-pythonic-string-reader
  (let ((commit "47a70ba1e32362e03dad6ef8e6f36180b560f86a"))
    (package
      (name "sbcl-pythonic-string-reader")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/smithzvk/pythonic-string-reader/")
               (commit commit)))
         (sha256
          (base32 "1b5iryqw8xsh36swckmz8rrngmc39k92si33fgy5pml3n9l5rq3j"))
         (file-name (git-file-name "pythonic-string-reader" version))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("named-readtables" ,sbcl-named-readtables)))
      (home-page "https://github.com/smithzvk/pythonic-string-reader")
      (synopsis "Read table modification inspired by Python's three quote strings")
      (description "This piece of code sets up some reader macros that make it
simpler to input string literals which contain backslashes and double quotes
This is very useful for writing complicated docstrings and, as it turns out,
writing code that contains string literals that contain code themselves.")
      (license license:bsd-3))))

(define-public cl-pythonic-string-reader
  (sbcl-package->cl-source-package sbcl-pythonic-string-reader))

(define-public ecl-pythonic-string-reader
  (sbcl-package->ecl-package sbcl-pythonic-string-reader))

(define-public sbcl-slime-swank
  (package
    (name "sbcl-slime-swank")
    (version "2.22")
    (source
     (origin
       (file-name (string-append name "-" version ".tar.gz"))
       (method git-fetch)
       (uri (git-reference
             ;; (url "https://github.com/slime/slime/")
             ;; (commit "841f61467c03dea9f38ff9d5af0e21a8aa29e8f7")
             ;; REVIEW: Do we need sionescu's patch to package SWANK?
             (url "https://github.com/sionescu/slime/")
             ;; (commit "swank-asdf")
             (commit "2f7c3fcb3ac7d50d844d5c6ca0e89b52a45e1d3a")))
       (sha256
        (base32
         ;; "065bc4y6iskazdfwlhgcjlzg9bi2hyjbhmyjw3461506pgkj08vi"
         "0pkmg94wn4ii1zhlrncn44mdc5i6c5v0i9gbldx4dwl2yy7ibz5c"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (substitute* "contrib/swank-listener-hooks.lisp"
             ((":compile-toplevel :load-toplevel ") ""))
           (substitute* "contrib/swank-presentations.lisp"
             ((":compile-toplevel :load-toplevel ") ""))
           (substitute* "swank.asd"
             ((":file \"packages\".*" all)
              (string-append all "(:file \"swank-loader-asdf\")\n")))
           (substitute* "swank-loader-asdf.lisp"
             ((":common-lisp" all) (string-append all " #:asdf")))
           #t))))
    (build-system asdf-build-system/sbcl)
    (arguments
     `(#:asd-file "swank.asd"
       #:asd-system-name "swank"))
    (home-page "https://github.com/slime/slime")
    (synopsis "Common Lisp Swank server")
    (description
     "This is only useful if you want to start a Swank server in a Lisp
processes that doesn't run under Emacs.  Lisp processes created by
@command{M-x slime} automatically start the server.")
    (license (list license:gpl2+ license:public-domain))))

(define-public sbcl-mgl-pax
  (let ((commit "818448418d6b9de74620f606f5b23033c6082769"))
    (package
      (name "sbcl-mgl-pax")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/melisgl/mgl-pax")
               (commit commit)))
         (sha256
          (base32
           "1p97zfkh130bdxqqxwaw2j9psv58751wakx7czbfpq410lg7dd7i"))
         (file-name (git-file-name "mgl-pax" version))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("3bmd" ,sbcl-3bmd)
         ("3bmd-ext-code-blocks" ,sbcl-3bmd-ext-code-blocks)
         ("babel" ,sbcl-babel)
         ("cl-fad" ,sbcl-cl-fad)
         ("ironclad" ,sbcl-ironclad)
         ("named-readtables" ,sbcl-named-readtables)
         ("pythonic-string-reader" ,sbcl-pythonic-string-reader)
         ("swank" ,sbcl-slime-swank)))
      (synopsis "Exploratory programming environment and documentation generator")
      (description
       "PAX provides an extremely poor man's Explorable Programming
environment.  Narrative primarily lives in so called sections that mix markdown
docstrings with references to functions, variables, etc, all of which should
probably have their own docstrings.

The primary focus is on making code easily explorable by using SLIME's
@command{M-.} (@command{slime-edit-definition}).  See how to enable some
fanciness in Emacs Integration.  Generating documentation from sections and all
the referenced items in Markdown or HTML format is also implemented.

With the simplistic tools provided, one may accomplish similar effects as with
Literate Programming, but documentation is generated from code, not vice versa
and there is no support for chunking yet.  Code is first, code must look
pretty, documentation is code.")
      (home-page "http://quotenil.com/")
      (license license:expat))))

(define-public cl-mgl-pax
  (sbcl-package->cl-source-package sbcl-mgl-pax))

(define-public ecl-mgl-pax
  (sbcl-package->ecl-package sbcl-mgl-pax))

(define-public sbcl-lisp-unit
  (let ((commit "89653a232626b67400bf9a941f9b367da38d3815"))
    (package
      (name "sbcl-lisp-unit")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/OdonataResearchLLC/lisp-unit")
               (commit commit)))
         (sha256
          (base32
           "0p6gdmgr7p383nvd66c9y9fp2bjk4jx1lpa5p09g43hr9y9pp9ry"))
         (file-name (git-file-name "lisp-unit" version))))
      (build-system asdf-build-system/sbcl)
      (synopsis "Common Lisp Test framework inspired by JUnit to be simple of use")
      (description
       "@command{lisp-unit} is a Common Lisp library that supports unit
testing.  It is an extension of the library written by Chris Riesbeck.")
      (home-page "https://github.com/OdonataResearchLLC/lisp-unit")
      (license license:expat))))

(define-public cl-lisp-unit
  (sbcl-package->cl-source-package sbcl-lisp-unit))

(define-public ecl-lisp-unit
  (sbcl-package->ecl-package sbcl-lisp-unit))

(define-public sbcl-anaphora
  (package
    (name "sbcl-anaphora")
    (version "0.9.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tokenrove/anaphora")
             (commit version)))
       (sha256
        (base32
         "19wfrk3asimznkli0x2rfy637hwpdgqyvwj3vhq9x7vjvyf5vv6x"))
       (file-name (git-file-name "anaphora" version))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     `(("rt" ,sbcl-rt)))
    (synopsis "The anaphoric macro collection from Hell")
    (description
     "Anaphora is the anaphoric macro collection from Hell: it includes many
new fiends in addition to old friends like @command{aif} and
@command{awhen}.")
    (home-page "https://github.com/tokenrove/anaphora")
    (license license:public-domain)))

(define-public cl-anaphora
  (sbcl-package->cl-source-package sbcl-anaphora))

(define-public ecl-anaphora
  (sbcl-package->ecl-package sbcl-anaphora))

(define-public sbcl-lift
  (let ((commit "7d49a66c62759535624037826891152223d4206c"))
    (package
      (name "sbcl-lift")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gwkkwg/lift")
               (commit commit)))
         (sha256
          (base32
           "127v5avpz1i4m0lkaxqrq8hrl69rdazqaxf6s8awf0nd7wj2g4dp"))
         (file-name (git-file-name "lift" version))
         (modules '((guix build utils)))
         (snippet
          ;; Don't keep the bundled website
          `(begin
             (delete-file-recursively "website")
             #t))))
      (build-system asdf-build-system/sbcl)
      (arguments
       ;; The tests require a debugger, but we run with the debugger disabled.
       '(#:tests? #f))
      (synopsis "LIsp Framework for Testing")
      (description
       "The LIsp Framework for Testing (LIFT) is a unit and system test tool for LISP.
Though inspired by SUnit and JUnit, it's built with Lisp in mind.  In LIFT,
testcases are organized into hierarchical testsuites each of which can have
its own fixture.  When run, a testcase can succeed, fail, or error.  LIFT
supports randomized testing, benchmarking, profiling, and reporting.")
      (home-page "https://github.com/gwkkwg/lift")
      (license license:expat))))

(define-public cl-lift
  (sbcl-package->cl-source-package sbcl-lift))

(define-public ecl-lift
  (sbcl-package->ecl-package sbcl-lift))

(define-public sbcl-let-plus
  (let ((commit "5f14af61d501ecead02ec6b5a5c810efc0c9fdbb"))
    (package
      (name "sbcl-let-plus")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sharplispers/let-plus")
               (commit commit)))
         (sha256
          (base32
           "0i050ca2iys9f5mb7dgqgqdxfnc3b0rnjdwv95sqd490vkiwrsaj"))
         (file-name (git-file-name "let-plus" version))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("anaphora" ,sbcl-anaphora)))
      (native-inputs
       `(("lift" ,sbcl-lift)))
      (synopsis "Destructuring extension of let*")
      (description
       "This library implements the let+ macro, which is a dectructuring
extension of let*.  It features:

@itemize
@item Clean, consistent syntax and small implementation (less than 300 LOC,
not counting tests)
@item Placeholder macros allow editor hints and syntax highlighting
@item @command{&ign} for ignored values (in forms where that makes sense)
@item Very easy to extend
@end itemize\n")
      (home-page "https://github.com/sharplispers/let-plus")
      (license license:boost1.0))))

(define-public cl-let-plus
  (sbcl-package->cl-source-package sbcl-let-plus))

(define-public ecl-let-plus
  (sbcl-package->ecl-package sbcl-let-plus))

(define-public sbcl-cl-colors
  (let ((commit "827410584553f5c717eec6182343b7605f707f75"))
    (package
      (name "sbcl-cl-colors")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tpapp/cl-colors")
               (commit commit)))
         (sha256
          (base32
           "0l446lday4hybsm9bq3jli97fvv8jb1d33abg79vbylpwjmf3y9a"))
         (file-name (git-file-name "cl-colors" version))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("let-plus" ,sbcl-let-plus)))
      (synopsis "Simple color library for Common Lisp")
      (description
       "This is a very simple color library for Common Lisp, providing

@itemize
@item Types for representing colors in HSV and RGB spaces.
@item Simple conversion functions between the above types (and also
hexadecimal representation for RGB).
@item Some predefined colors (currently X11 color names – of course the
library does not depend on X11).Because color in your terminal is nice.
@end itemize

This library is no longer supported by its author.")
      (home-page "https://github.com/tpapp/cl-colors")
      (license license:boost1.0))))

(define-public cl-colors
  (sbcl-package->cl-source-package sbcl-cl-colors))

(define-public ecl-cl-colors
  (sbcl-package->ecl-package sbcl-cl-colors))

(define-public sbcl-cl-ansi-text
  (let ((commit "53badf7878f27f22f2d4a2a43e6df458e43acbe9"))
    (package
      (name "sbcl-cl-ansi-text")
      (version (git-version "1.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/pnathan/cl-ansi-text")
               (commit commit)))
         (sha256
          (base32
           "11i27n0dbz5lmygiw65zzr8lx0rac6b6yysqranphn31wls6ja3v"))
         (file-name (git-file-name "cl-ansi-text" version))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("cl-colors" ,sbcl-cl-colors)))
      (native-inputs
       `(("fiveam" ,sbcl-fiveam)))
      (synopsis "ANSI terminal color implementation for Common Lisp")
      (description
       "@command{cl-ansi-text} provides utilities which enable printing to an
ANSI terminal with colored text.  It provides the macro @command{with-color}
which causes everything printed in the body to be displayed with the provided
color.  It further provides functions which will print the argument with the
named color.")
      (home-page "https://github.com/pnathan/cl-ansi-text")
      (license license:llgpl))))

(define-public cl-ansi-text
  (sbcl-package->cl-source-package sbcl-cl-ansi-text))

(define-public ecl-cl-ansi-text
  (sbcl-package->ecl-package sbcl-cl-ansi-text))

(define-public sbcl-prove-asdf
  (let ((commit "4f9122bd393e63c5c70c1fba23070622317cfaa0"))
    (package
      (name "sbcl-prove-asdf")
      (version (git-version "1.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/fukamachi/prove")
               (commit commit)))
         (sha256
          (base32
           "07sbfw459z8bbjvx1qlmfa8qk2mvbjnnzi2mi0x72blaj8bkl4vc"))
         (file-name (git-file-name "prove" version))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(#:asd-file "prove-asdf.asd"))
      (synopsis "Test requirement for the Common Lisp 'prove' library")
      (description
       "Test requirement for the Common Lisp @command{prove} library.")
      (home-page "https://github.com/fukamachi/prove")
      (license license:expat))))

(define-public cl-prove-asdf
  (sbcl-package->cl-source-package sbcl-prove-asdf))

(define-public ecl-prove-asdf
  (sbcl-package->ecl-package sbcl-prove-asdf))

(define-public sbcl-prove
  (package
    (inherit sbcl-prove-asdf)
    (name "sbcl-prove")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("cl-ppcre" ,sbcl-cl-ppcre)
       ("cl-ansi-text" ,sbcl-cl-ansi-text)))
    (native-inputs
     `(("prove-asdf" ,sbcl-prove-asdf)))
    (arguments
     `(#:asd-file "prove.asd"))
    (synopsis "Yet another unit testing framework for Common Lisp")
    (description
     "This project was originally called @command{cl-test-more}.
@command{prove} is yet another unit testing framework for Common Lisp.  The
advantages of @command{prove} are:

@itemize
@item Various simple functions for testing and informative error messages
@item ASDF integration
@item Extensible test reporters
@item Colorizes the report if it's available (note for SLIME)
@item Reports test durations
@end itemize\n")))

(define-public cl-prove
  (sbcl-package->cl-source-package sbcl-prove))

(define-public ecl-prove
  (sbcl-package->ecl-package sbcl-prove))

(define-public sbcl-proc-parse
  (let ((commit "ac3636834d561bdc2686c956dbd82494537285fd"))
    (package
      (name "sbcl-proc-parse")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/fukamachi/proc-parse")
               (commit commit)))
         (sha256
          (base32
           "06rnl0h4cx6xv2wj3jczmmcxqn2703inmmvg1s4npbghmijsybfh"))
         (file-name (git-file-name "proc-parse" version))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("babel" ,sbcl-babel)))
      (native-inputs
       `(("prove" ,sbcl-prove)
         ("prove-asdf" ,sbcl-prove-asdf)))
      (arguments
       ;; TODO: Tests don't find "proc-parse-test", why?
       `(#:tests? #f))
      (synopsis "Procedural vector parser")
      (description
       "This is a string/octets parser library for Common Lisp with speed and
readability in mind.  Unlike other libraries, the code is not a
pattern-matching-like, but a char-by-char procedural parser.")
      (home-page "https://github.com/fukamachi/proc-parse")
      (license license:bsd-2))))

(define-public cl-proc-parse
  (sbcl-package->cl-source-package sbcl-proc-parse))

(define-public ecl-proc-parse
  (sbcl-package->ecl-package sbcl-proc-parse))

(define-public sbcl-parse-float
  (let ((commit "2aae569f2a4b2eb3bfb5401a959425dcf151b09c"))
    (package
      (name "sbcl-parse-float")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/soemraws/parse-float")
               (commit commit)))
         (sha256
          (base32
           "08xw8cchhmqcc0byng69m3f5a2izc9y2290jzz2k0qrbibp1fdk7"))
         (file-name (git-file-name "proc-parse" version))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("babel" ,sbcl-babel)))
      (native-inputs
       `(("prove" ,sbcl-prove)
         ("prove-asdf" ,sbcl-prove-asdf)))
      (arguments
       ;; TODO: Tests don't find "proc-parse-test", why?
       `(#:tests? #f))
      (synopsis "Parse a floating point value from a string in Common Lisp")
      (description
       "This package exports the following function to parse floating-point
values from a string in Common Lisp.")
      (home-page "https://github.com/soemraws/parse-float")
      (license license:public-domain))))

(define-public cl-parse-float
  (sbcl-package->cl-source-package sbcl-parse-float))

(define-public ecl-parse-float
  (sbcl-package->ecl-package sbcl-parse-float))

(define-public sbcl-ascii-strings
  (let ((revision "1")
        (changeset "5048480a61243e6f1b02884012c8f25cdbee6d97"))
    (package
      (name "sbcl-ascii-strings")
      (version (string-append "0-" revision "." (string-take changeset 7)))
      (source
       (origin
         (method hg-fetch)
         (uri (hg-reference
               (url "https://bitbucket.org/vityok/cl-string-match/")
               (changeset changeset)))
         (sha256
          (base32
           "01wn5qx562w43ssy92xlfgv79w7p0nv0wbl76mpmba131n9ziq2y"))
         (file-name (git-file-name "cl-string-match" version))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("babel" ,sbcl-babel)))
      (arguments
       `(#:asd-file "ascii-strings.asd"))
      (synopsis "Operations on ASCII strings")
      (description
       "Operations on ASCII strings.  Essentially this can be any kind of
single-byte encoded strings.")
      (home-page "https://bitbucket.org/vityok/cl-string-match/")
      (license license:bsd-3))))

(define-public cl-ascii-strings
  (sbcl-package->cl-source-package sbcl-ascii-strings))

(define-public ecl-ascii-strings
  (sbcl-package->ecl-package sbcl-ascii-strings))

(define-public sbcl-simple-scanf
  (package
    (inherit sbcl-ascii-strings)
    (name "sbcl-simple-scanf")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("iterate" ,sbcl-iterate)
       ("proc-parse" ,sbcl-proc-parse)
       ("parse-float" ,sbcl-parse-float)))
    (arguments
     `(#:asd-file "simple-scanf.asd"))
    (synopsis "Simple scanf-like functionality implementation")
    (description
     "A simple scanf-like functionality implementation.")))

(define-public cl-simple-scanf
  (sbcl-package->cl-source-package sbcl-simple-scanf))

(define-public ecl-simple-scanf
  (sbcl-package->ecl-package sbcl-simple-scanf))

(define-public sbcl-cl-string-match
  (package
    (inherit sbcl-ascii-strings)
    (name "sbcl-cl-string-match")
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("ascii-strings" ,sbcl-ascii-strings)
       ("yacc" ,sbcl-cl-yacc)
       ("jpl-util" ,sbcl-jpl-util)
       ("jpl-queues" ,sbcl-jpl-queues)
       ("mgl-pax" ,sbcl-mgl-pax)
       ("iterate" ,sbcl-iterate)))
    ;; TODO: Tests are not evaluated properly.
    (native-inputs
     ;; For testing:
     `(("lisp-unit" ,sbcl-lisp-unit)
       ("simple-scanf" ,sbcl-simple-scanf)))
    (arguments
     `(#:tests? #f
       #:asd-file "cl-string-match.asd"))
    (synopsis "Portable, dependency-free set of utilities to manipulate strings in Common Lisp")
    (description
     "@command{cl-strings} is a small, portable, dependency-free set of
utilities that make it even easier to manipulate text in Common Lisp.  It has
100% test coverage and works at least on sbcl, ecl, ccl, abcl and clisp.")))

(define-public cl-string-match
  (sbcl-package->cl-source-package sbcl-cl-string-match))

(define-public ecl-cl-string-match
  (sbcl-package->ecl-package sbcl-cl-string-match))

(define-public sbcl-ptester
  (package
    (name "sbcl-ptester")
    (version "20160929")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://beta.quicklisp.org/archive/ptester/"
                           (date->string (string->date version "~Y~m~d") "~Y-~m-~d")
                           "/ptester-"
                           version
                           "-git.tgz"))
       (sha256
        (base32
         "04rlq1zljhxc65pm31bah3sq3as24l0sdivz440s79qlnnyh13hz"))))
    (build-system asdf-build-system/sbcl)
    (home-page "http://quickdocs.org/ptester/")
    (synopsis "Portable test harness package")
    (description
     "@command{ptester} is a portable testing framework based on Franz's
tester module.")
    (license license:lgpl3+)))

(define-public cl-ptester
  (sbcl-package->cl-source-package sbcl-ptester))

(define-public ecl-ptester
  (sbcl-package->ecl-package sbcl-ptester))

(define-public sbcl-puri
  (package
    (name "sbcl-puri")
    (version "20180228")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://beta.quicklisp.org/archive/puri/"
                           (date->string (string->date version "~Y~m~d") "~Y-~m-~d")
                           "/puri-"
                           version
                           "-git.tgz"))
       (sha256
        (base32
         "1s4r5adrjy5asry45xbcbklxhdjydvf6n55z897nvyw33bigrnbz"))))
    (build-system asdf-build-system/sbcl)
    ;; REVIEW: Webiste down?
    (native-inputs
     `(("ptester" ,sbcl-ptester)))
    (home-page "http://files.kpe.io/puri/")
    (synopsis "Portable URI Library")
    (description
     "This is portable Universal Resource Identifier library for Common Lisp
programs.  It parses URI according to the RFC 2396 specification")
    (license license:lgpl3+)))

(define-public cl-puri
  (sbcl-package->cl-source-package sbcl-puri))

(define-public ecl-puri
  (sbcl-package->ecl-package sbcl-puri))

(define-public sbcl-queues
  (let ((commit "47d4da65e9ea20953b74aeeab7e89a831b66bc94"))
    (package
      (name "sbcl-queues")
      (version (git-version "0.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/oconnore/queues")
               (commit commit)))
         (file-name (git-file-name "queues" version))
         (sha256
          (base32
           "0wdhfnzi4v6d97pggzj2aw55si94w4327br94jrmyvwf351wqjvv"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/oconnore/queues")
      (synopsis "Common Lisp queue library")
      (description
       "This is a simple queue library for Common Lisp with features such as
non-consing thread safe queues and fibonacci priority queues.")
      (license license:expat))))

(define-public cl-queues
  (sbcl-package->cl-source-package sbcl-queues))

(define-public ecl-queues
  (sbcl-package->ecl-package sbcl-queues))

(define-public sbcl-queues.simple-queue
  (package
    (inherit sbcl-queues)
    (name "sbcl-queues.simple-queue")
    (inputs
     `(("sbcl-queues" ,sbcl-queues)))
    (arguments
     `(#:asd-file "queues.simple-queue.asd"))
    (synopsis "Simple queue implementation")
    (description
     "This is a simple queue library for Common Lisp with features such as
non-consing thread safe queues and fibonacci priority queues.")
    (license license:expat)))

(define-public cl-queues.simple-queue
  (sbcl-package->cl-source-package sbcl-queues.simple-queue))

(define-public ecl-queues.simple-queue
  (sbcl-package->ecl-package sbcl-queues.simple-queue))

(define-public sbcl-queues.simple-cqueue
  (package
    (inherit sbcl-queues)
    (name "sbcl-queues.simple-cqueue")
    (inputs
     `(("sbcl-queues" ,sbcl-queues)
       ("sbcl-queues.simple-queue" ,sbcl-queues.simple-queue)
       ("bordeaux-threads" ,sbcl-bordeaux-threads)))
    (arguments
     `(#:asd-file "queues.simple-cqueue.asd"))
    (synopsis "Thread safe queue implementation")
    (description
     "This is a simple queue library for Common Lisp with features such as
non-consing thread safe queues and fibonacci priority queues.")
    (license license:expat)))

(define-public cl-queues.simple-cqueue
  (sbcl-package->cl-source-package sbcl-queues.simple-cqueue))

(define-public ecl-queues.simple-cqueue
  (sbcl-package->ecl-package sbcl-queues.simple-cqueue))

(define-public sbcl-queues.priority-queue
  (package
    (inherit sbcl-queues)
    (name "sbcl-queues.priority-queue")
    (inputs
     `(("sbcl-queues" ,sbcl-queues)))
    (arguments
     `(#:asd-file "queues.priority-queue.asd"))
    (synopsis "Priority queue (Fibonacci) implementation")
    (description
     "This is a simple queue library for Common Lisp with features such as
non-consing thread safe queues and fibonacci priority queues.")
    (license license:expat)))

(define-public cl-queues.priority-queue
  (sbcl-package->cl-source-package sbcl-queues.priority-queue))

(define-public ecl-queues.priority-queue
  (sbcl-package->ecl-package sbcl-queues.priority-queue))

(define-public sbcl-queues.priority-cqueue
  (package
    (inherit sbcl-queues)
    (name "sbcl-queues.priority-cqueue")
    (inputs
     `(("sbcl-queues" ,sbcl-queues)
       ("sbcl-queues.priority-queue" ,sbcl-queues.priority-queue)
       ("bordeaux-threads" ,sbcl-bordeaux-threads)))
    (arguments
     `(#:asd-file "queues.priority-cqueue.asd"))
    (synopsis "Thread safe fibonacci priority queue implementation")
    (description
     "This is a simple queue library for Common Lisp with features such as
non-consing thread safe queues and fibonacci priority queues.")
    (license license:expat)))

(define-public cl-queues.priority-cqueue
  (sbcl-package->cl-source-package sbcl-queues.priority-cqueue))

(define-public ecl-queues.priority-cqueue
  (sbcl-package->ecl-package sbcl-queues.priority-cqueue))

(define sbcl-cffi-bootstrap
  (package
    (name "sbcl-cffi-bootstrap")
    (version "0.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/cffi/cffi/archive/v"
                           version ".tar.gz"))
       (sha256
        (base32 "07bc3c1fbfx17wgrvx6bh9byilfzfwv5n597cfdllm0vzwvbmiyk"))
       (file-name (string-append name "-" version ".tar.gz"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     `(("libffi" ,libffi)
       ("alexandria" ,sbcl-alexandria)
       ("babel" ,sbcl-babel)
       ("trivial-features" ,sbcl-trivial-features)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "libffi/libffi.lisp"
               (("libffi.so.6" all) (string-append
                                     (assoc-ref inputs "libffi")
                                     "/lib/" all)))
             (substitute* "toolchain/c-toolchain.lisp"
               (("\"cc\"") (format #f "~S" (which "gcc")))))))
       #:asd-system-name "cffi"
       #:tests? #f))
    (home-page "https://common-lisp.net/project/cffi/")
    (synopsis "Common Foreign Function Interface for Common Lisp")
    (description "The Common Foreign Function Interface (CFFI)
purports to be a portable foreign function interface for Common Lisp.
The CFFI library is composed of a Lisp-implementation-specific backend
in the CFFI-SYS package, and a portable frontend in the CFFI
package.")
    (license license:expat)))

(define-public sbcl-cffi-toolchain
  (package
    (inherit sbcl-cffi-bootstrap)
    (name "sbcl-cffi-toolchain")
    (inputs
     `(("libffi" ,libffi)
       ("sbcl-cffi" ,sbcl-cffi-bootstrap)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cffi-bootstrap)
       ((#:asd-system-name _) #f)
       ((#:tests? _) #t)))))

(define-public sbcl-cffi-libffi
  (package
    (inherit sbcl-cffi-toolchain)
    (name "sbcl-cffi-libffi")
    (inputs
     `(("cffi" ,sbcl-cffi-bootstrap)
       ("cffi-grovel" ,sbcl-cffi-grovel)
       ("trivial-features" ,sbcl-trivial-features)
       ("libffi" ,libffi)))))

(define-public sbcl-cffi-grovel
  (package
    (inherit sbcl-cffi-toolchain)
    (name "sbcl-cffi-grovel")
    (inputs
     `(("libffi" ,libffi)
       ("cffi" ,sbcl-cffi-bootstrap)
       ("cffi-toolchain" ,sbcl-cffi-toolchain)
       ("alexandria" ,sbcl-alexandria)))
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cffi-toolchain)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'build 'install-headers
             (lambda* (#:key outputs #:allow-other-keys)
               (install-file "grovel/common.h"
                             (string-append
                              (assoc-ref outputs "out")
                              "/include/grovel"))))))))))

(define-public sbcl-cffi
  (package
    (inherit sbcl-cffi-toolchain)
    (name "sbcl-cffi")
    (inputs (package-inputs sbcl-cffi-bootstrap))
    (native-inputs
     `(("cffi-grovel" ,sbcl-cffi-grovel)
       ("cffi-libffi" ,sbcl-cffi-libffi)
       ("rt" ,sbcl-rt)
       ("bordeaux-threads" ,sbcl-bordeaux-threads)
       ,@(package-native-inputs sbcl-cffi-bootstrap)))))

(define-public sbcl-cl-sqlite
  (let ((commit "c738e66d4266ef63a1debc4ef4a1b871a068c112"))
    (package
      (name "sbcl-cl-sqlite")
      (version (git-version "0.2" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/dmitryvk/cl-sqlite")
               (commit commit)))
         (file-name (git-file-name "cl-sqlite" version))
         (sha256
          (base32
           "1ng45k1hdb84sqjryrfx93g66bsbybmpy301wd0fdybnc5jzr36q"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("iterate" ,sbcl-iterate)
         ("cffi" ,sbcl-cffi)
         ("sqlite" ,sqlite)))
      (native-inputs
       `(("fiveam" ,sbcl-fiveam)
         ("bordeaux-threads" ,sbcl-bordeaux-threads)))
      (arguments
       `(#:tests? #f                    ; Upstream seems to have issues with tests: https://github.com/dmitryvk/cl-sqlite/issues/7
         #:asd-file "sqlite.asd"
         #:asd-system-name "sqlite"
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "sqlite-ffi.lisp"
                 (("libsqlite3" all) (string-append
                                      (assoc-ref inputs "sqlite")"/lib/" all))))))))
      (home-page "https://common-lisp.net/project/cl-sqlite/")
      (synopsis "Common Lisp binding for SQLite")
      (description
       "The @command{cl-sqlite} package is an interface to the SQLite embedded
relational database engine.")
      (license license:public-domain))))

(define-public sbcl-parenscript
  (let ((commit "061d8e286c81c3f45c84fb2b11ee7d83f590a8f8"))
    (package
      (name "sbcl-parenscript")
      (version (git-version "2.6" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.common-lisp.net/parenscript/parenscript")
               (commit commit)))
         (file-name (git-file-name "parenscript" version))
         (sha256
          (base32
           "1kbhgsjbikc73m5cwdp4d4fdafyqcr1b7b630qjrziql0nh6mi3k"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("cl-ppcre" ,sbcl-cl-ppcre)
         ("anaphora" ,sbcl-anaphora)
         ("named-readtables" ,sbcl-named-readtables)))
      (home-page "https://common-lisp.net/project/parenscript/")
      (synopsis "Translator from a subset of Common Lisp to JavaScript")
      (description
       "Parenscript is a translator from an extended subset of Common Lisp to
JavaScript.  Parenscript code can run almost identically on both the
browser (as JavaScript) and server (as Common Lisp).

Parenscript code is treated the same way as Common Lisp code, making the full
power of Lisp macros available for JavaScript.  This provides a web
development environment that is unmatched in its ability to reduce code
duplication and provide advanced meta-programming facilities to web
developers.

At the same time, Parenscript is different from almost all other \"language
X\" to JavaScript translators in that it imposes almost no overhead:

@itemize
@item No run-time dependencies: Any piece of Parenscript code is runnable
as-is.  There are no JavaScript files to include.
@item Native types: Parenscript works entirely with native JavaScript data
types.  There are no new types introduced, and object prototypes are not
touched.
@item Native calling convention: Any JavaScript code can be called without the
need for bindings.  Likewise, Parenscript can be used to make efficient,
self-contained JavaScript libraries.
@item Readable code: Parenscript generates concise, formatted, idiomatic
JavaScript code.  Identifier names are preserved.  This enables seamless
debugging in tools like Firebug.
@item Efficiency: Parenscript introduces minimal overhead for advanced Common
Lisp features.  The generated code is almost as fast as hand-written
JavaScript.
@end itemize\n")
      (license license:bsd-3))))

(define-public cl-parenscript
  (sbcl-package->cl-source-package sbcl-parenscript))

(define-public ecl-parenscript
  (sbcl-package->ecl-package sbcl-parenscript))

(define-public sbcl-cl-json
  (let ((commit "6dfebb9540bfc3cc33582d0c03c9ec27cb913e79"))
    (package
      (name "sbcl-cl-json")
      (version (git-version "0.5" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/hankhero/cl-json")
               (commit commit)))
         (file-name (git-file-name "cl-json" version))
         (sha256
          (base32
           "0fx3m3x3s5ji950yzpazz4s0img3l6b3d6l3jrfjv0lr702496lh"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       `(("fiveam" ,sbcl-fiveam)))
      (home-page "https://github.com/hankhero/cl-json")
      (synopsis "JSON encoder and decoder for Common-Lisp")
      (description
       "@command{cl-json} provides an encoder of Lisp objects to JSON format
and a corresponding decoder of JSON data to Lisp objects.  Both the encoder
and the decoder are highly customizable; at the same time, the default
settings ensure a very simple mode of operation, similar to that provided by
@command{yason} or @command{st-json}.")
      (license license:expat))))

(define-public cl-json
  (sbcl-package->cl-source-package sbcl-cl-json))

(define-public ecl-cl-json
  (sbcl-package->ecl-package sbcl-cl-json))

(define-public sbcl-unix-opts
  (package
    (name "sbcl-unix-opts")
    (version "0.1.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libre-man/unix-opts")
             (commit version)))
       (file-name (git-file-name "unix-opts" version))
       (sha256
        (base32
         "08djdi1ard09fijb7w9bdmhmwd98b1hzmcnjw9fqjiqa0g3b44rr"))))
    (build-system asdf-build-system/sbcl)
    (home-page "https://github.com/hankhero/cl-json")
    (synopsis "Unix-style command line options parser")
    (description
     "This is a minimalistic parser of command line options.  The main
advantage of the library is the ability to concisely define command line
options once and then use this definition for parsing and extraction of
command line arguments, as well as printing description of command line
options (you get --help for free).  This way you don't need to repeat
yourself.  Also, @command{unix-opts} doesn't depend on anything and allows to
precisely control behavior of the parser via Common Lisp restarts.")
    (license license:expat)))

(define-public cl-unix-opts
  (sbcl-package->cl-source-package sbcl-unix-opts))

(define-public ecl-unix-opts
  (sbcl-package->ecl-package sbcl-unix-opts))

(define-public sbcl-trivial-garbage
  (package
    (name "sbcl-trivial-garbage")
    (version "0.21")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/trivial-garbage/trivial-garbage/archive/v"
             version ".tar.gz"))
       (sha256
        (base32 "0b244nlszkrqawsnp568clnx32xmvjmbbagbz7625w9n0yq7396y"))
       (file-name (string-append "trivial-garbage-" version ".tar.gz"))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     `(("rt" ,sbcl-rt)))
    (home-page "https://common-lisp.net/project/trivial-garbage/")
    (synopsis "Portable GC-related APIs for Common Lisp")
    (description "@command{trivial-garbage} provides a portable API to
finalizers, weak hash-tables and weak pointers on all major implementations of
the Common Lisp programming language.")
    (license license:public-domain)))

(define-public cl-trivial-garbage
  (sbcl-package->cl-source-package sbcl-trivial-garbage))

(define-public ecl-trivial-garbage
  (sbcl-package->ecl-package sbcl-trivial-garbage))

(define-public sbcl-closer-mop
  (let ((commit "fac29ce90e3a46e1fc6cf182190e193526fa9dbc"))
    (package
      (name "sbcl-closer-mop")
      (version (git-version  "1.0.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/pcostanza/closer-mop")
               (commit commit)))
         (sha256
          (base32 "0hvh77y869h8fg9di5snyg85fxq6fdh9gj1igmx1g6j6j5x915dl"))
         (file-name (git-file-name "closer-mop" version ))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/pcostanza/closer-mop")
      (synopsis "Rectifies absent or incorrect CLOS MOP features")
      (description "Closer to MOP is a compatibility layer that rectifies many
of the absent or incorrect CLOS MOP features across a broad range of Common
Lisp implementations.")
      (license license:expat))))

(define-public cl-closer-mop
  (sbcl-package->cl-source-package sbcl-closer-mop))

(define-public ecl-closer-mop
  (sbcl-package->ecl-package sbcl-closer-mop))

(define sbcl-cl-cffi-gtk-boot0
  (let ((commit "29443c5aaca975709df8025c4649366d882033cb"))
    (package
      (name "sbcl-cl-cffi-gtk-boot0")
      (version (git-version "0.11.2" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Ferada/cl-cffi-gtk/")
               (commit commit)))
         (file-name (git-file-name "cl-cffi-gtk" version))
         (sha256
          (base32
           "0f6s92sf8xyzh1yksqx8bsy1sv0zmy0c13j3b8bavaba5hlxpxah"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("iterate" ,sbcl-iterate)
         ("cffi" ,sbcl-cffi)
         ("trivial-features" ,sbcl-trivial-features)))
      (home-page "https://github.com/Ferada/cl-cffi-gtk/")
      (synopsis "Common Lisp binding for GTK+3")
      (description
       "@command{cl-cffi-gtk} is a Lisp binding to GTK+ 3 (GIMP Toolkit) which
is a library for creating graphical user interfaces.")
      (license license:lgpl3))))

(define-public sbcl-cl-cffi-gtk-glib
  (package
    (inherit sbcl-cl-cffi-gtk-boot0)
    (name "sbcl-cl-cffi-gtk-glib")
    (inputs
     `(("glib" ,glib)
       ,@(package-inputs sbcl-cl-cffi-gtk-boot0)))
    (arguments
     `(#:asd-file "glib/cl-cffi-gtk-glib.asd"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "glib/glib.init.lisp"
               (("libglib|libgthread" all) (string-append
                                            (assoc-ref inputs "glib") "/lib/" all))))))))))

(define-public sbcl-cl-cffi-gtk-gobject
  (package
    (inherit sbcl-cl-cffi-gtk-boot0)
    (name "sbcl-cl-cffi-gtk-gobject")
    (inputs
     `(("glib" ,glib)
       ("cl-cffi-gtk-glib" ,sbcl-cl-cffi-gtk-glib)
       ("trivial-garbage" ,sbcl-trivial-garbage)
       ("bordeaux-threads" ,sbcl-bordeaux-threads)
       ("closer-mop" ,sbcl-closer-mop)
       ,@(package-inputs sbcl-cl-cffi-gtk-boot0)))
    (arguments
     `(#:asd-file "gobject/cl-cffi-gtk-gobject.asd"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "gobject/gobject.init.lisp"
               (("libgobject" all) (string-append
                                    (assoc-ref inputs "glib") "/lib/" all))))))))))

(define-public sbcl-cl-cffi-gtk-gio
  (package
    (inherit sbcl-cl-cffi-gtk-boot0)
    (name "sbcl-cl-cffi-gtk-gio")
    (inputs
     `(("glib" ,glib)
       ("cl-cffi-gtk-glib" ,sbcl-cl-cffi-gtk-glib)
       ("cl-cffi-gtk-gobject" ,sbcl-cl-cffi-gtk-gobject)
       ,@(package-inputs sbcl-cl-cffi-gtk-boot0)))
    (arguments
     `(#:asd-file "gio/cl-cffi-gtk-gio.asd"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "gio/gio.init.lisp"
               (("libgio" all)
                (string-append
                 (assoc-ref inputs "glib") "/lib/" all))))))))))

(define-public sbcl-cl-cffi-gtk-cairo
  (package
    (inherit sbcl-cl-cffi-gtk-boot0)
    (name "sbcl-cl-cffi-gtk-cairo")
    (inputs
     `(("cairo" ,cairo)
       ("cl-cffi-gtk-glib" ,sbcl-cl-cffi-gtk-glib)
       ,@(package-inputs sbcl-cl-cffi-gtk-boot0)))
    (arguments
     `(#:asd-file "cairo/cl-cffi-gtk-cairo.asd"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "cairo/cairo.init.lisp"
               (("libcairo" all)
                (string-append
                 (assoc-ref inputs "cairo") "/lib/" all))))))))))

(define-public sbcl-cl-cffi-gtk-pango
  (package
    (inherit sbcl-cl-cffi-gtk-boot0)
    (name "sbcl-cl-cffi-gtk-pango")
    (inputs
     `(("pango" ,pango)
       ("cl-cffi-gtk-glib" ,sbcl-cl-cffi-gtk-glib)
       ("cl-cffi-gtk-gobject" ,sbcl-cl-cffi-gtk-gobject)
       ("cl-cffi-gtk-cairo" ,sbcl-cl-cffi-gtk-cairo)
       ,@(package-inputs sbcl-cl-cffi-gtk-boot0)))
    (arguments
     `(#:asd-file "pango/cl-cffi-gtk-pango.asd"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "pango/pango.init.lisp"
               (("libpango" all)
                (string-append
                 (assoc-ref inputs "pango") "/lib/" all))))))))))

(define-public sbcl-cl-cffi-gtk-gdk-pixbuf
  (package
    (inherit sbcl-cl-cffi-gtk-boot0)
    (name "sbcl-cl-cffi-gtk-gdk-pixbuf")
    (inputs
     `(("gdk-pixbuf" ,gdk-pixbuf)
       ("cl-cffi-gtk-gobject" ,sbcl-cl-cffi-gtk-gobject)
       ,@(package-inputs sbcl-cl-cffi-gtk-boot0)))
    (arguments
     `(#:asd-file "gdk-pixbuf/cl-cffi-gtk-gdk-pixbuf.asd"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "gdk-pixbuf/gdk-pixbuf.init.lisp"
               (("libgdk_pixbuf" all)
                (string-append
                 (assoc-ref inputs "gdk-pixbuf") "/lib/" all))))))))))

(define-public sbcl-cl-cffi-gtk-gdk
  (package
    (inherit sbcl-cl-cffi-gtk-boot0)
    (name "sbcl-cl-cffi-gtk-gdk")
    (inputs
     `(("gtk" ,gtk+)
       ("cl-cffi-gtk-gobject" ,sbcl-cl-cffi-gtk-gobject)
       ("cl-cffi-gtk-gio" ,sbcl-cl-cffi-gtk-gio)
       ("cl-cffi-gtk-gdk-pixbuf" ,sbcl-cl-cffi-gtk-gdk-pixbuf)
       ("cl-cffi-gtk-cairo" ,sbcl-cl-cffi-gtk-cairo)
       ("cl-cffi-gtk-pango" ,sbcl-cl-cffi-gtk-pango)
       ,@(package-inputs sbcl-cl-cffi-gtk-boot0)))
    (arguments
     `(#:asd-file "gdk/cl-cffi-gtk-gdk.asd"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "gdk/gdk.init.lisp"
               (("libgdk" all)
                (string-append
                 (assoc-ref inputs "gtk") "/lib/" all)))
             (substitute* "gdk/gdk.package.lisp"
               (("libgtk" all)
                (string-append
                 (assoc-ref inputs "gtk") "/lib/" all))))))))))

(define-public sbcl-cl-cffi-gtk
  (package
    (inherit sbcl-cl-cffi-gtk-boot0)
    (name "sbcl-cl-cffi-gtk")
    (inputs
     `(("cl-cffi-gtk-glib" ,sbcl-cl-cffi-gtk-glib)
       ("cl-cffi-gtk-gobject" ,sbcl-cl-cffi-gtk-gobject)
       ("cl-cffi-gtk-gio" ,sbcl-cl-cffi-gtk-gio)
       ("cl-cffi-gtk-gdk" ,sbcl-cl-cffi-gtk-gdk)
       ,@(package-inputs sbcl-cl-cffi-gtk-boot0)))
    (native-inputs
     `(("fiveam" ,sbcl-fiveam)))
    (arguments
     `(#:asd-file "gtk/cl-cffi-gtk.asd"
       #:test-asd-file "test/cl-cffi-gtk-test.asd"
       ;; TODO: Tests fail with memory fault.
       ;; See https://github.com/Ferada/cl-cffi-gtk/issues/24.
       #:tests? #f))))

(define-public sbcl-cl-webkit
  (let ((commit "cd2a9008e0c152e54755e8a7f07b050fe36bab31"))
    (package
      (name "sbcl-cl-webkit")
      (version (git-version "2.4" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jmercouris/cl-webkit")
               (commit commit)))
         (file-name (git-file-name "cl-webkit" version))
         (sha256
          (base32
           "0f5lyn9i7xrn3g1bddga377mcbawkbxydijpg389q4n04gqj0vwf"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("cffi" ,sbcl-cffi)
         ("cl-cffi-gtk" ,sbcl-cl-cffi-gtk)
         ("webkitgtk" ,webkitgtk)))
      (arguments
       `(#:asd-file "webkit2/cl-webkit2.asd"
         #:asd-system-name "cl-webkit2"
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "webkit2/webkit2.init.lisp"
                 (("libwebkit2gtk" all)
                  (string-append
                   (assoc-ref inputs "webkitgtk") "/lib/" all))))))))
      (home-page "https://github.com/jmercouris/cl-webkit")
      (synopsis "Binding to WebKitGTK+ for Common Lisp")
      (description
       "@command{cl-webkit} is a binding to WebKitGTK+ for Common Lisp,
currently targeting WebKit version 2.  The WebKitGTK+ library adds web
browsing capabilities to an application, leveraging the full power of the
WebKit browsing engine.")
      (license license:expat))))

(define-public sbcl-lparallel
  (package
    (name "sbcl-lparallel")
    (version "2.8.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lmj/lparallel/")
             (commit (string-append "lparallel-" version))))
       (file-name (git-file-name "lparallel" version))
       (sha256
        (base32
         "0g0aylrbbrqsz0ahmwhvnk4cmc2931fllbpcfgzsprwnqqd7vwq9"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("bordeaux-threads" ,sbcl-bordeaux-threads)
       ("trivial-garbage" ,sbcl-trivial-garbage)))
    (home-page "https://lparallel.org/")
    (synopsis "Parallelism for Common Lisp")
    (description
     "@command{lparallel} is a library for parallel programming in Common
Lisp, featuring:

@itemize
@item a simple model of task submission with receiving queue,
@item constructs for expressing fine-grained parallelism,
@item asynchronous condition handling across thread boundaries,
@item parallel versions of map, reduce, sort, remove, and many others,
@item promises, futures, and delayed evaluation constructs,
@item computation trees for parallelizing interconnected tasks,
@item bounded and unbounded FIFO queues,
@item high and low priority tasks,
@item task killing by category,
@item integrated timeouts.
@end itemize\n")
    (license license:expat)))

(define-public cl-lparallel
  (sbcl-package->cl-source-package sbcl-lparallel))

(define-public ecl-lparallel
  (sbcl-package->ecl-package sbcl-lparallel))

(define-public sbcl-cl-markup
  (let ((commit "e0eb7debf4bdff98d1f49d0f811321a6a637b390"))
    (package
      (name "sbcl-cl-markup")
      (version (git-version "0.1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/arielnetworks/cl-markup/")
               (commit commit)))
         (file-name (git-file-name "cl-markup" version))
         (sha256
          (base32
           "10l6k45971dl13fkdmva7zc6i453lmq9j4xax2ci6pjzlc6xjhp7"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/arielnetworks/cl-markup/")
      (synopsis "Markup generation library for Common Lisp")
      (description
       "A modern markup generation library for Common Lisp that features:

@itemize
@item Fast (even faster through compiling the code)
@item Safety
@item Support for multiple document types (markup, xml, html, html5, xhtml)
@item Output with doctype
@item Direct output to stream
@end itemize\n")
      (license license:lgpl3+))))

(define-public cl-markup
  (sbcl-package->cl-source-package sbcl-cl-markup))

(define-public ecl-cl-markup
  (sbcl-package->ecl-package sbcl-cl-markup))

(define-public sbcl-cl-css
  (let ((commit "8fe654c8f0cf95b300718101cce4feb517f78e2f"))
    (package
      (name "sbcl-cl-css")
      (version (git-version "0.1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/inaimathi/cl-css/")
               (commit commit)))
         (file-name (git-file-name "cl-css" version))
         (sha256
          (base32
           "1lc42zi2sw11fl2589sc19nr5sd2p0wy7wgvgwaggxa5f3ajhsmd"))))
      (build-system asdf-build-system/sbcl)
      (home-page "https://github.com/inaimathi/cl-css/")
      (synopsis "Non-validating, inline CSS generator for Common Lisp")
      (description
       "This is a dead-simple, non validating, inline CSS generator for Common
Lisp.  Its goals are axiomatic syntax, simple implementation to support
portability, and boilerplate reduction in CSS.")
      (license license:expat))))

(define-public cl-css
  (sbcl-package->cl-source-package sbcl-cl-css))

(define-public ecl-cl-css
  (sbcl-package->ecl-package sbcl-cl-css))

(define-public sbcl-portable-threads
  (let ((commit "c0e61a1faeb0583c80fd3f20b16cc4c555226920"))
    (package
      (name "sbcl-portable-threads")
      (version (git-version "2.3" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/binghe/portable-threads/")
               (commit commit)))
         (file-name (git-file-name "portable-threads" version))
         (sha256
          (base32
           "03fmxyarc0xf4kavwkfa0a2spkyfrz6hbgbi9y4q7ny5aykdyfaq"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       `(;; Tests seem broken.
         #:tests? #f))
      (home-page "https://github.com/binghe/portable-threads")
      (synopsis "Portable threads (and scheduled and periodic functions) API for Common Lisp")
      (description
       "Portable Threads (and Scheduled and Periodic Functions) API for Common
Lisp (from GBBopen project).")
      (license license:asl2.0))))

(define-public cl-portable-threads
  (sbcl-package->cl-source-package sbcl-portable-threads))

(define-public ecl-portable-threada
  (sbcl-package->ecl-package sbcl-portable-threads))

(define-public sbcl-usocket-boot0
  ;; usocket's test rely on usocket-server which depends on usocket itself.
  ;; We break this cyclic dependency with -boot0 that packages usocket.
  (let ((commit "86e7efbfe50101931edf4b67cdcfa7e221ecfde9"))
    (package
      (name "sbcl-usocket-boot0")
      (version (git-version "0.7.1" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/usocket/usocket/")
               (commit commit)))
         (file-name (git-file-name "usocket" version))
         (sha256
          (base32
           "1lk6ipakrib7kdgzw44hrgmls9akp5pz4h35yynw0k5zwmmq6374"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("split-sequence" ,sbcl-split-sequence)))
      (arguments
       `(#:tests? #f
         #:asd-system-name "usocket"))
      (home-page "https://common-lisp.net/project/usocket/")
      (synopsis "Universal socket library for Common Lisp (server side)")
      (description
       "This library strives to provide a portable TCP/IP and UDP/IP socket
interface for as many Common Lisp implementations as possible, while keeping
the abstraction and portability layer as thin as possible.")
      (license license:expat))))

(define-public sbcl-usocket-server
  (package
    (inherit sbcl-usocket-boot0)
    (name "sbcl-usocket-server")
    (inputs
     `(("usocket" ,sbcl-usocket-boot0)
       ("portable-threads" ,sbcl-portable-threads)))
    (arguments
     '(#:asd-system-name "usocket-server"))
    (synopsis "Universal socket library for Common Lisp (server side)")))

(define-public cl-usocket-server
  (sbcl-package->cl-source-package sbcl-usocket-server))

(define-public ecl-socket-server
  (sbcl-package->ecl-package sbcl-usocket-server))

(define-public sbcl-usocket
  (package
    (inherit sbcl-usocket-boot0)
    (name "sbcl-usocket")
    (arguments
     ;; FIXME: Tests need network access?
     `(#:tests? #f))
    (native-inputs
     ;; Testing only.
     `(("usocket-server" ,sbcl-usocket-server)
       ("rt" ,sbcl-rt)))))

(define-public cl-usocket
  (sbcl-package->cl-source-package sbcl-usocket))

(define-public ecl-socket
  (sbcl-package->ecl-package sbcl-usocket))

(define-public sbcl-s-xml
  (package
    (name "sbcl-s-xml")
    (version "3")
    (source
     (origin
       (method url-fetch)
       (uri "https://common-lisp.net/project/s-xml/s-xml.tgz")
       (sha256
        (base32
         "061qcr0dzshsa38s5ma4ay924cwak2nq9gy59dw6v9p0qb58nzjf"))))
    (build-system asdf-build-system/sbcl)
    (home-page "https://common-lisp.net/project/s-xml/")
    (synopsis "Simple XML parser implemented in Common Lisp")
    (description
     "S-XML is a simple XML parser implemented in Common Lisp.  This XML
parser implementation has the following features:

@itemize
@item It works (handling many common XML usages).
@item It is very small (the core is about 700 lines of code, including
comments and whitespace).
@item It has a core API that is simple, efficient and pure functional, much
like that from SSAX (see also http://ssax.sourceforge.net).
@item It supports different DOM models: an XSML-based one, an LXML-based one
and a classic xml-element struct based one.
@item It is reasonably time and space efficient (internally avoiding garbage
generatation as much as possible).
@item It does support CDATA.
@item It should support the same character sets as your Common Lisp
implementation.
@item It does support XML name spaces.
@end itemize

This XML parser implementation has the following limitations:

@itemize
@item It does not support any special tags (like processing instructions).
@item It is not validating, even skips DTD's all together.
@end itemize\n")
    (license license:lgpl3+)))

(define-public cl-s-xml
  (sbcl-package->cl-source-package sbcl-s-xml))

(define-public ecl-s-xml
  (sbcl-package->ecl-package sbcl-s-xml))

(define-public sbcl-s-xml-rpc
  (package
    (name "sbcl-s-xml-rpc")
    (version "7")
    (source
     (origin
       (method url-fetch)
       (uri "https://common-lisp.net/project/s-xml-rpc/s-xml-rpc.tgz")
       (sha256
        (base32
         "02z7k163d51v0pzk8mn1xb6h5s6x64gjqkslhwm3a5x26k2gfs11"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     `(("s-xml" ,sbcl-s-xml)))
    (home-page "https://common-lisp.net/project/s-xml-rpc/")
    (synopsis "Implementation of XML-RPC in Common Lisp for both client and server")
    (description
     "S-XML-RPC is an implementation of XML-RPC in Common Lisp for both
client and server.")
    (license license:lgpl3+)))

(define-public cl-s-xml-rpc
  (sbcl-package->cl-source-package sbcl-s-xml-rpc))

(define-public ecl-s-xml-rpc
  (sbcl-package->ecl-package sbcl-s-xml-rpc))

(define-public sbcl-trivial-clipboard
  (let ((commit "5af3415d1484e6d69a1b5c178f24680d9fd01796"))
    (package
      (name "sbcl-trivial-clipboard")
      (version (git-version "0.0.0.0" "2" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/snmsts/trivial-clipboard")
               (commit commit)))
         (file-name (git-file-name "trivial-clipboard" version))
         (sha256
          (base32
           "1gb515z5yq6h5548pb1fwhmb0hhq1ssyb78pvxh4alq799xipxs9"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("xclip" ,xclip)))
      (native-inputs
       `(("fiveam" ,sbcl-fiveam)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "src/text.lisp"
                 (("\\(executable-find \"xclip\"\\)")
                  (string-append "(executable-find \""
                                 (assoc-ref inputs "xclip")
                                 "/bin/xclip\")"))))))))
      (home-page "https://github.com/snmsts/trivial-clipboard")
      (synopsis "Access system clipboard in Common Lisp")
      (description
       "@command{trivial-clipboard} gives access to the system clipboard.")
      (license license:expat))))

(define-public cl-trivial-clipboard
  (sbcl-package->cl-source-package sbcl-trivial-clipboard))

(define-public ecl-trivial-clipboard
  (sbcl-package->ecl-package sbcl-trivial-clipboard))

(define-public sbcl-trivial-backtrace
  (let ((commit "ca81c011b86424a381a7563cea3b924f24e6fbeb")
        (revision "1"))
    (package
     (name "sbcl-trivial-backtrace")
     (version (git-version "0.0.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gwkkwg/trivial-backtrace.git")
             (commit commit)))
       (sha256
        (base32 "10p41p43skj6cimdg8skjy7372s8v2xpkg8djjy0l8rm45i654k1"))))
     (build-system asdf-build-system/sbcl)
     (inputs
      `(("sbcl-lift" ,sbcl-lift)))
     (home-page "https://common-lisp.net/project/trivial-backtrace/")
     (synopsis "Portable simple API to work with backtraces in Common Lisp")
     (description
      "On of the many things that didn't quite get into the Common Lisp
standard was how to get a Lisp to output its call stack when something has
gone wrong.  As such, each Lisp has developed its own notion of what to
display, how to display it, and what sort of arguments can be used to
customize it.  @code{trivial-backtrace} is a simple solution to generating a
backtrace portably.")
     (license license:bsd-style))))

(define-public cl-trivial-backtrace
  (sbcl-package->cl-source-package sbcl-trivial-backtrace))

(define-public sbcl-rfc2388
  (let ((commit "591bcf7e77f2c222c43953a80f8c297751dc0c4e")
        (revision "1"))
    (package
     (name "sbcl-rfc2388")
     (version (git-version "0.0.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jdz/rfc2388.git")
             (commit commit)))
       (sha256
        (base32 "0phh5n3clhl9ji8jaxrajidn22d3f0aq87mlbfkkxlnx2pnw694k"))))
     (build-system asdf-build-system/sbcl)
     (home-page "https://github.com/jdz/rfc2388/")
     (synopsis "An implementation of RFC 2388 in Common Lisp")
     (description
      "This package contains an implementation of RFC 2388, which is used to
process form data posted with HTTP POST method using enctype
\"multipart/form-data\".")
     (license license:bsd-2))))

(define-public cl-rfc2388
  (sbcl-package->cl-source-package sbcl-rfc2388))

(define-public sbcl-md5
  (package
    (name "sbcl-md5")
    (version "2.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/pmai/md5/archive/release-" version ".tar.gz"))
       (sha256
        (base32 "19yl9n0pjdz5gw4qi711lka97xcd9f81ylg434hk7jwn9f2s6w11"))))
    (build-system asdf-build-system/sbcl)
    (home-page "https://github.com/pmai/md5")
    (synopsis
     "Common Lisp implementation of the MD5 Message-Digest Algorithm (RFC 1321)")
    (description
     "This package implements The MD5 Message-Digest Algorithm, as defined in
RFC 1321 by R. Rivest, published April 1992.")
    (license license:public-domain)))

(define-public cl-md5
  (sbcl-package->cl-source-package sbcl-md5))

(define-public sbcl-cl+ssl
  (let ((commit "b81c1135cf5700e870ce2573d5035d249e491788")
        (revision "1"))
    (package
      (name "sbcl-cl+ssl")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/cl-plus-ssl/cl-plus-ssl.git")
               (commit commit)))
         (sha256
          (base32 "1845i1pafmqb6cdlr53yaqy67kjrhkvbx6c37ca15cw70vhdr3z9"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-paths
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "src/reload.lisp"
                 (("libssl.so" all)
                  (string-append
                   (assoc-ref inputs "openssl") "/lib/" all))))))))
      (inputs
       `(("openssl" ,openssl)
         ("sbcl-cffi" ,sbcl-cffi)
         ("sbcl-trivial-gray-streams" ,sbcl-trivial-gray-streams)
         ("sbcl-flexi-streams" ,sbcl-flexi-streams)
         ("sbcl-bordeaux-threads" ,sbcl-bordeaux-threads)
         ("sbcl-trivial-garbage" ,sbcl-trivial-garbage)))
      (home-page "http://common-lisp.net/project/cl-plus-ssl/")
      (synopsis "Common Lisp bindings to OpenSSL")
      (description
       "This library is a fork of SSL-CMUCL.  The original SSL-CMUCL source
code was written by Eric Marsden and includes contributions by Jochen Schmidt.
Development into CL+SSL was done by David Lichteblau.")
      (license license:expat))))

(define-public cl-cl+ssl
  (sbcl-package->cl-source-package sbcl-cl+ssl))

(define-public sbcl-kmrcl
  (let ((version "1.109.0")
        (commit "5260068b2eb735af6796740c2db4955afac21636")
        (revision "1"))
    (package
      (name "sbcl-kmrcl")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "http://git.kpe.io/kmrcl.git/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1va7xjgzfv674bpsli674i7zj3f7wg5kxic41kz18r6hh4n52dfv"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       ;; Tests fail with: :FORCE and :FORCE-NOT arguments not allowed in a
       ;; nested call to ASDF/OPERATE:OPERATE unless identically to toplevel
       '(#:tests? #f))
      (inputs
       `(("sbcl-rt" ,sbcl-rt)))
      (home-page "http://files.kpe.io/kmrcl/")
      (synopsis "General utilities for Common Lisp programs")
      (description
       "KMRCL is a collection of utilities used by a number of Kevin
Rosenberg's CL packages.")
      (license license:llgpl))))

(define-public cl-kmrcl
  (sbcl-package->cl-source-package sbcl-kmrcl))

(define-public sbcl-cl-base64
  (let ((version "3.3.3"))
    (package
      (name "sbcl-cl-base64")
      (version version)
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "http://git.kpe.io/cl-base64.git")
               (commit (string-append "v" version))))
         (sha256
          (base32 "1dw6j7n6gsd2qa0p0rbsjxj00acxx3i9ca1qkgl0liy8lpnwkypl"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       ;; Tests fail with: :FORCE and :FORCE-NOT arguments not allowed
       ;; in a nested call to ASDF/OPERATE:OPERATE unless identically
       ;; to toplevel
       '(#:tests? #f))
      (inputs
       `(("sbcl-ptester" ,sbcl-ptester)
         ("sbcl-kmrcl" ,sbcl-kmrcl)))
      (home-page "http://files.kpe.io/cl-base64/")
      (synopsis
       "Common Lisp package to encode and decode base64 with URI support")
      (description
       "This package provides highly optimized base64 encoding and decoding.
Besides conversion to and from strings, integer conversions are supported.
Encoding with Uniform Resource Identifiers is supported by using a modified
encoding table that uses only URI-compatible characters.")
      (license license:bsd-3))))

(define-public cl-base64
  (sbcl-package->cl-source-package sbcl-cl-base64))

(define-public sbcl-chunga
  (package
    (name "sbcl-chunga")
    (version "1.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/edicl/chunga/archive/v" version ".tar.gz"))
       (sha256
        (base32 "0ra17kyc9l7qbaw003ly111r1cbn4zixbfq1ydr9cxw10v30q1n7"))))
    (build-system asdf-build-system/sbcl)
    (inputs
     `(("sbcl-trivial-gray-streams" ,sbcl-trivial-gray-streams)))
    (home-page "https://edicl.github.io/chunga/")
    (synopsis "Portable chunked streams for Common Lisp")
    (description
     "Chunga implements streams capable of chunked encoding on demand as
defined in RFC 2616.")
    (license license:bsd-2)))

(define-public cl-chunga
  (sbcl-package->cl-source-package sbcl-chunga))

(define-public sbcl-cl-who
  (let ((version "1.1.4")
        (commit "2c08caa4bafba720409af9171feeba3f32e86d32")
        (revision "1"))
    (package
      (name "sbcl-cl-who")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/edicl/cl-who.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0yjb6sr3yazm288m318kqvj9xk8rm9n1lpimgf65ymqv0i5agxsb"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       `(("sbcl-flexi-streams" ,sbcl-flexi-streams)))
      (home-page "https://edicl.github.io/cl-who/")
      (synopsis "Yet another Lisp markup language")
      (description
       "There are plenty of Lisp Markup Languages out there - every Lisp
programmer seems to write at least one during his career - and CL-WHO (where
WHO means \"with-html-output\" for want of a better acronym) is probably just
as good or bad as the next one.")
      (license license:bsd-2))))

(define-public cl-cl-who
  (sbcl-package->cl-source-package sbcl-cl-who))

(define-public sbcl-chipz
  (let ((version "0.8")
        (commit "75dfbc660a5a28161c57f115adf74c8a926bfc4d")
        (revision "1"))
    (package
      (name "sbcl-chipz")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/froydnj/chipz.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0plx4rs39zbs4gjk77h4a2q11zpy75fh9v8hnxrvsf8fnakajhwg"))))
      (build-system asdf-build-system/sbcl)
      (native-inputs
       `(("sbcl-flexi-streams" ,sbcl-flexi-streams)))
      (home-page "http://method-combination.net/lisp/chipz/")
      (synopsis
       "Common Lisp library for decompressing deflate, zlib, gzip, and bzip2
data")
      (description
       "DEFLATE data, defined in RFC1951, forms the core of popular
compression formats such as zlib (RFC 1950) and gzip (RFC 1952).  As such,
Chipz also provides for decompressing data in those formats as well.  BZIP2 is
the format used by the popular compression tool bzip2.")
      ;; The author describes it as "MIT-like"
      (license license:expat))))

(define-public cl-chipz
  (sbcl-package->cl-source-package sbcl-chipz))

(define-public sbcl-drakma
  (let ((version "2.0.4")
        (commit "7647c0ae842ff2058624e53979c7f297760c97a7")
        (revision "1"))
    (package
      (name "sbcl-drakma")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/edicl/drakma.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1c4i9wakhj5pxfyyykxshdmv3180sbkrx6fcyynikmc0jd0rh84r"))))
      (build-system asdf-build-system/sbcl)
      (inputs
       `(("sbcl-puri" ,sbcl-puri)
         ("sbcl-cl-base64" ,sbcl-cl-base64)
         ("sbcl-chunga" ,sbcl-chunga)
         ("sbcl-flexi-streams" ,sbcl-flexi-streams)
         ("sbcl-cl-ppcre" ,sbcl-cl-ppcre)
         ("sbcl-chipz" ,sbcl-chipz)
         ("sbcl-usocket" ,sbcl-usocket)
         ("sbcl-cl+ssl" ,sbcl-cl+ssl)))
      (native-inputs
       `(("sbcl-fiveam" ,sbcl-fiveam)))
      (home-page "https://edicl.github.io/drakma/")
      (synopsis "HTTP client written in Common Lisp")
      (description
       "Drakma is a full-featured HTTP client implemented in Common Lisp.  It
knows how to handle HTTP/1.1 chunking, persistent connections, re-usable
sockets, SSL, continuable uploads, file uploads, cookies, and more.")
      (license license:bsd-2))))

(define-public cl-drakma
  (sbcl-package->cl-source-package sbcl-drakma))

(define-public sbcl-hunchentoot
  (package
    (name "sbcl-hunchentoot")
    (version "1.2.38")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/edicl/hunchentoot/archive/v"
             version ".tar.gz"))
       (sha256
        (base32 "17z8rpd6b5w93jwrdwfwxjha617xnjqw8aq1hw2z76zp1fn8yrmh"))))
    (build-system asdf-build-system/sbcl)
    (native-inputs
     `(("sbcl-cl-who" ,sbcl-cl-who)
       ("sbcl-drakma" ,sbcl-drakma)))
    (inputs
     `(("sbcl-chunga" ,sbcl-chunga)
       ("sbcl-cl-base64" ,sbcl-cl-base64)
       ("sbcl-cl-fad" ,sbcl-cl-fad)
       ("sbcl-cl-ppcre" ,sbcl-cl-ppcre)
       ("sbcl-flexi-streams" ,sbcl-flexi-streams)
       ("sbcl-cl+ssl" ,sbcl-cl+ssl)
       ("sbcl-md5" ,sbcl-md5)
       ("sbcl-rfc2388" ,sbcl-rfc2388)
       ("sbcl-trivial-backtrace" ,sbcl-trivial-backtrace)
       ("sbcl-usocket" ,sbcl-usocket)))
    (home-page "https://edicl.github.io/hunchentoot/")
    (synopsis "Web server written in Common Lisp")
    (description
     "Hunchentoot is a web server written in Common Lisp and at the same
time a toolkit for building dynamic websites.  As a stand-alone web server,
Hunchentoot is capable of HTTP/1.1 chunking (both directions), persistent
connections (keep-alive), and SSL.")
    (license license:bsd-2)))

(define-public cl-hunchentoot
  (sbcl-package->cl-source-package sbcl-hunchentoot))

(define-public sbcl-trivial-types
  (package
    (name "sbcl-trivial-types")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/m2ym/trivial-types.git")
             (commit "ee869f2b7504d8aa9a74403641a5b42b16f47d88")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s4cp9bdlbn8447q7w7f1wkgwrbvfzp20mgs307l5pxvdslin341"))))
    (build-system asdf-build-system/sbcl)
    (home-page "https://github.com/m2ym/trivial-types")
    (synopsis "Trivial type definitions for Common Lisp")
    (description
     "TRIVIAL-TYPES provides missing but important type definitions such as
PROPER-LIST, ASSOCIATION-LIST, PROPERTY-LIST and TUPLE.")
    (license license:llgpl)))

(define-public cl-trivial-types
  (sbcl-package->cl-source-package sbcl-trivial-types))

(define-public sbcl-syntax
  (package
    (name "sbcl-syntax")
    (version "0.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/m2ym/cl-syntax.git")
             (commit "03f0c329bbd55b8622c37161e6278366525e2ccc")))
       (sha256
        (base32 "17ran8xp77asagl31xv8w819wafh6whwfc9p6dgx22ca537gyl4y"))))
    (build-system asdf-build-system/sbcl)
    (arguments
     '(#:asd-file "cl-syntax.asd"
       #:asd-system-name "cl-syntax"))
    (inputs `(("sbcl-trivial-types" ,sbcl-trivial-types)
              ("sbcl-named-readtables" ,sbcl-named-readtables)))
    (home-page "https://github.com/m2ym/cl-syntax")
    (synopsis "Reader Syntax Coventions for Common Lisp and SLIME")
    (description
     "CL-SYNTAX provides Reader Syntax Coventions for Common Lisp and SLIME.")
    (license license:llgpl)))

(define-public cl-syntax
  (sbcl-package->cl-source-package sbcl-syntax))

(define-public sbcl-annot
  (let ((commit "c99e69c15d935eabc671b483349a406e0da9518d")
        (revision "1"))
    (package
      (name "sbcl-annot")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/m2ym/cl-annot.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1wq1gs9jjd5m6iwrv06c2d7i5dvqsfjcljgbspfbc93cg5xahk4n"))))
      (build-system asdf-build-system/sbcl)
    (arguments
     '(#:asd-file "cl-annot.asd"
       #:asd-system-name "cl-annot"))
      (inputs
       `(("sbcl-alexandria" ,sbcl-alexandria)))
      (home-page "https://github.com/m2ym/cl-annot")
      (synopsis "Python-like Annotation Syntax for Common Lisp.")
      (description
       "@code{cl-annot} is an general annotation library for Common Lisp.")
      (license license:llgpl))))

(define-public cl-annot
  (sbcl-package->cl-source-package sbcl-annot))
