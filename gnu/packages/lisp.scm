;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016 ng0 <ngillmann@runbox.com>
;;; Copyright © 2016 Andy Patterson <ajpatter@uwaterloo.ca>
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
  #:use-module (gnu packages readline)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages m4)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system asdf)
  #:use-module (gnu packages base)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libffcall)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages libsigsegv)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages ed)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages version-control)
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
  (package
    (name "gcl")
    (version "2.6.12")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/" name "/" name "-" version ".tar.gz"))
      (sha256
       (base32 "1s4hs2qbjqmn9h88l4xvsifq5c3dlc5s74lyb61rdi5grhdlkf4f"))))
    (build-system gnu-build-system)
    (arguments
     `(#:parallel-build? #f  ; The build system seems not to be thread safe.
       #:tests? #f  ; There does not seem to be make check or anything similar.
       #:configure-flags '("--enable-ansi") ; required for use by the maxima package
       #:phases (alist-cons-before
                'configure 'pre-conf
                (lambda _
                  ;; Patch bug when building readline support.  This bug was
                  ;; also observed by Debian
                  ;; https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=741819
                  (substitute* "o/gcl_readline.d"
                    (("rl_attempted_completion_function = \
\\(CPPFunction \\*\\)rl_completion;")
                      "rl_attempted_completion_function = rl_completion;"))
                  (substitute*
                      (append
                       '("pcl/impl/kcl/makefile.akcl"
                         "add-defs"
                         "unixport/makefile.dos"
                         "add-defs.bat"
                         "gcl-tk/makefile.prev"
                         "add-defs1")
                       (find-files "h" "\\.defs"))
                    (("SHELL=/bin/(ba)?sh")
                     (string-append "SHELL=" (which "bash")))))
                ;; drop strip phase to make maxima build, see
                ;; https://www.ma.utexas.edu/pipermail/maxima/2008/009769.html
                (alist-delete 'strip
                 %standard-phases))))
    (native-inputs
     `(("m4" ,m4)
       ("readline" ,readline)
       ("texinfo" ,texinfo)
       ("texlive" ,texlive)))
    (home-page "http://www.gnu.org/software/gcl")
    (synopsis "A Common Lisp implementation")
    (description "GCL is an implementation of the Common Lisp language.  It
features the ability to compile to native object code and to load native
object code modules directly into its lisp core.  It also features a
stratified garbage collection strategy, a source-level debugger and a built-in
interface to the Tk widget system.")
    (license license:lgpl2.0+)))

(define-public ecl
  (package
    (name "ecl")
    (version "16.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://common-lisp.net/project/ecl/static/files/release/"
             name "-" version ".tgz"))
       (sha256
        (base32 "16ab8qs3awvdxy8xs8jy82v8r04x4wr70l9l2j45vgag18d2nj1d"))
       (modules '((guix build utils)))
       (snippet
        ;; Add ecl-bundle-systems to 'default-system-source-registry'.
        `(substitute* "contrib/asdf/asdf.lisp"
           ,@(asdf-substitutions name)))))
    (build-system gnu-build-system)
    ;; src/configure uses 'which' to confirm the existence of 'gzip'.
    (native-inputs `(("which" ,which)))
    (inputs `(("gmp" ,gmp)
              ("libatomic-ops" ,libatomic-ops)
              ("libgc" ,libgc)
              ("libffi" ,libffi)))
    (arguments
     '(#:tests? #t
       #:make-flags `(,(string-append "ECL="
                                      (assoc-ref %outputs "out")
                                      "/bin/ecl"))
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
         (add-after 'wrap 'check (assoc-ref %standard-phases 'check)))))
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
    (version "2.49")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/clisp/release/" version
                           "/clisp-" version ".tar.gz"))
       (sha256
        (base32 "0rp82nqp5362isl9i34rwgg04cidz7izljd9d85pqcw1qr964bxx"))))
    (build-system gnu-build-system)
    (inputs `(("libffcall" ,libffcall)
              ("readline" ,readline)
              ("libsigsegv" ,libsigsegv)))
    (arguments
     '(;; XXX The custom configure script does not cope well when passed
       ;; --build=<triplet>.
       #:build #f
       #:phases
       (alist-cons-after
        'unpack 'patch-sh-and-pwd
        (lambda _
          ;; The package is very messy with its references to "/bin/sh" and
          ;; some other absolute paths to traditional tools.  These appear in
          ;; many places where our automatic patching misses them.  Therefore
          ;; we do the following, in this early (post-unpack) phase, to solve
          ;; the problem from its root.
          (substitute* (find-files "." "configure|Makefile")
            (("/bin/sh") "sh"))
          (substitute* '("src/clisp-link.in")
            (("/bin/pwd") "pwd")))
        (alist-cons-before
         'build 'chdir-to-source
         (lambda _
           ;; We are supposed to call make under the src sub-directory.
           (chdir "src"))
         %standard-phases))
       ;; Makefiles seem to have race conditions.
       #:parallel-build? #f))
    (home-page "http://www.clisp.org/")
    (synopsis "A Common Lisp implementation")
    (description
     "GNU CLISP is an implementation of ANSI Common Lisp.  Common Lisp is a
high-level, object-oriented functional programming language.  CLISP includes
an interpreter, a compiler, a debugger, and much more.")
    ;; Website says gpl2+, COPYRIGHT file says gpl2; actual source files have
    ;; a lot of gpl3+.  (Also some parts are under non-copyleft licenses, such
    ;; as CLX by Texas Instruments.)  In that case gpl3+ wins out.
    (license license:gpl3+)))

(define-public sbcl
  (package
    (name "sbcl")
    (version "1.3.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/sbcl/sbcl/" version "/sbcl-"
                           version "-source.tar.bz2"))
       (sha256
        (base32 "0fjdqnb2rsm2vi9794ywp27jr239ddvzc4xfr0dk49jd4v7p2kc5"))
       (modules '((guix build utils)))
       (snippet
        ;; Add sbcl-bundle-systems to 'default-system-source-registry'.
        `(substitute* "contrib/asdf/asdf.lisp"
           ,@(asdf-substitutions name)))))
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
     '(#:phases
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
    (version "1.11")
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
                 "ftp://ftp.clozure.com/pub/release/" version
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
                "0w3dmj7q9kqyra3yrf1lxclnjz151yvf5s5q8ayllvmvqbl8bs08")
               ("armhf-linux"
                "1x487aaz2rqcb6k301sy2p39a1m4qdhg6z9p9fb76ssipqgr38b4")
               (_ ""))))))))
    (native-inputs
     `(("m4" ,m4)
       ("subversion" ,subversion)))
    (arguments
     `(#:tests? #f                      ;no 'check' target
       #:phases
       (alist-replace
        'unpack
        (lambda* (#:key inputs #:allow-other-keys)
          (and (zero? (system* "tar" "xzvf" (assoc-ref inputs "ccl")))
               (begin (chdir "ccl") #t)))
        (alist-delete
         'configure
         (alist-cons-before
          'build 'pre-build
          ;; Enter the source directory for the current platform's lisp
          ;; kernel, and run 'make clean' to remove the precompiled one.
          (lambda _
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
            (zero? (system* "make" "clean")))
          ;; XXX Do we need to recompile the heap image as well for Guix?
          ;; For now just use the one we already got in the tarball.
          (alist-replace
           'install
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
               (mkdir-p libdir)
               (mkdir-p bindir)
               (copy-file kernel (string-append libdir kernel))
               (copy-file heap (string-append libdir heap))
               (with-output-to-file wrapper
                 (lambda ()
                   (display
                    (string-append
                     "#!" bash "/bin/sh\n"
                     "if [ -z \"$CCL_DEFAULT_DIRECTORY\" ]; then\n"
                     "    CCL_DEFAULT_DIRECTORY=" libdir "\n"
                     "fi\n"
                     "export CCL_DEFAULT_DIRECTORY\n"
                     "exec " libdir kernel "\n"))))
               (chmod wrapper #o755)))
           %standard-phases))))))
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
             '("armhf-linux" "mips64el-linux")))
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
    (inputs `(("sbcl-alexandria" ,sbcl-alexandria)))
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
  (package
    (name "sbcl-bordeaux-threads")
    (version "0.8.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/sionescu/bordeaux-threads/archive/v"
                    version ".tar.gz"))
              (sha256
               (base32 "10ryrcx832fwqdawb6jmknymi7wpdzhi30qzx7cbrk0cpnka71w2"))
              (file-name
               (string-append "bordeaux-threads-" version ".tar.gz"))))
    (inputs `(("sbcl-alexandria" ,sbcl-alexandria)))
    (native-inputs `(("tests:cl-fiveam" ,sbcl-fiveam)))
    (build-system asdf-build-system/sbcl)
    (synopsis "Portable shared-state concurrency library for Common Lisp")
    (description "BORDEAUX-THREADS is a proposed standard for a minimal
MP/Threading interface.  It is similar to the CLIM-SYS threading and lock
support.")
    (home-page "https://common-lisp.net/project/bordeaux-threads/")
    (license license:x11)))

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
    (version "1.0.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/edicl/flexi-streams/archive/v"
             version ".tar.gz"))
       (sha256
        (base32 "16grnxvs7vqm5s6myf8a5s7vwblzq1kgwj8i7ahz8vwvihm9gzfi"))
       (file-name (string-append "flexi-streams-" version ".tar.gz"))))
    (build-system asdf-build-system/sbcl)
    (inputs `(("sbcl-trivial-gray-streams" ,sbcl-trivial-gray-streams)))
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
    (native-inputs `(("tests:cl-flexi-streams" ,sbcl-flexi-streams)))
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
               (("\\(:file \"trapezoid\"\\)") ""))))))
      (build-system asdf-build-system/sbcl)
      (arguments
       '(#:special-dependencies '("sb-bsd-sockets")))
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
