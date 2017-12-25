;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015, 2016 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016, 2017 ng0 <contact.ng0@cryptolab.net>
;;; Copyright © 2017 John Darrington <jmd@gnu.org>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages scheme)
  #:use-module (gnu packages)
  #:use-module ((guix licenses)
                #:select (gpl2+ lgpl2.0+ lgpl2.1+ lgpl3+ asl2.0 bsd-3
                          cc-by-sa4.0 non-copyleft))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libunistring)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages netpbm)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages libphidget)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages image)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages gl)
  #:use-module (ice-9 match))

(define (mit-scheme-source-directory system version)
  (string-append "mit-scheme-"
                 (if (or (string-prefix? "x86_64" system)
                         (string-prefix? "i686" system))
                     ""
                     "c-")
                 version))

(define-public mit-scheme
  (package
    (name "mit-scheme")
    (version "9.2")
    (source #f)                                   ; see below
    (outputs '("out" "doc"))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                                ; no "check" target
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key inputs #:allow-other-keys)
             (and (zero? (system* "tar" "xzvf"
                                  (assoc-ref inputs "source")))
                  (chdir ,(mit-scheme-source-directory (%current-system)
                                                       version))
                  (begin
                    ;; Delete these dangling symlinks since they break
                    ;; `patch-shebangs'.
                    (for-each delete-file
                              (append '("src/lib/shim-config.scm")
                                      (find-files "src/lib/lib" "\\.so$")
                                      (find-files "src/lib" "^liarc-")
                                      (find-files "src/compiler" "^make\\.")))
                    (chdir "src")
                    #t))))
         (replace 'build
           (lambda* (#:key system outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (if (or (string-prefix? "x86_64" system)
                       (string-prefix? "i686" system))
                   (zero? (system* "make" "compile-microcode"))
                   (zero? (system* "./etc/make-liarc.sh"
                                   (string-append "--prefix=" out)))))))
         (add-after 'configure 'configure-doc
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (with-directory-excursion "../doc"
               (let* ((out (assoc-ref outputs "out"))
                      (bash (assoc-ref inputs "bash"))
                      (bin/sh (string-append bash "/bin/sh")))
                 (system* bin/sh "./configure"
                          (string-append "--prefix=" out)
                          (string-append "SHELL=" bin/sh))
                 (substitute* '("Makefile" "make-common")
                   (("/lib/mit-scheme/doc")
                    (string-append "/share/doc/" ,name "-" ,version)))
                 #t))))
         (add-after 'build 'build-doc
           (lambda* _
             (with-directory-excursion "../doc"
               (zero? (system* "make")))))
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc"))
                    (old-doc-dir (string-append out "/share/doc"))
                    (new-doc/mit-scheme-dir
                     (string-append doc "/share/doc/" ,name "-" ,version)))
               (with-directory-excursion "../doc"
                 (for-each (lambda (target)
                             (system* "make" target))
                           '("install-config" "install-info-gz" "install-man"
                             "install-html" "install-pdf")))
               (mkdir-p new-doc/mit-scheme-dir)
               (copy-recursively
                (string-append old-doc-dir "/" ,name "-" ,version)
                new-doc/mit-scheme-dir)
               (delete-file-recursively old-doc-dir)
               #t))))))
    (native-inputs
     `(("texlive" ,texlive)
       ("texinfo" ,texinfo)
       ("m4" ,m4)))
    (inputs
     `(("libx11" ,libx11)

       ("source"

        ;; MIT/GNU Scheme is not bootstrappable, so it's recommended to
        ;; compile from the architecture-specific tarballs, which contain
        ;; pre-built binaries.  It leads to more efficient code than when
        ;; building the tarball that contains generated C code instead of
        ;; those binaries.
        ,(origin
          (method url-fetch)
          (uri (string-append "mirror://gnu/mit-scheme/stable.pkg/"
                              version "/mit-scheme-"
                              (match (%current-system)
                                ("x86_64-linux"
                                 (string-append version "-x86-64"))
                                ("i686-linux"
                                 (string-append version "-i386"))
                                (_
                                 (string-append "c-" version)))
                              ".tar.gz"))
          (sha256
           (match (%current-system)
             ("x86_64-linux"
              (base32
               "1skzxxhr0iq96bf0j5m7mvf3i4sppfyfa6gpqn34mwgkw1fx8274"))
             ("i686-linux"
              (base32
               "1fmlpnhf5a75db93phajh4ysbdgrgl72v45lk3kznriprl0a7jc6"))
             (_
              (base32
               "0w5ib5vsidihb4hb6fma3sp596ykr8izagm57axvgd6lqzwicsjg"))))))))

    ;; Fails to build on MIPS, see <http://bugs.gnu.org/18221>.
    (supported-systems '("x86_64-linux" "i686-linux" "armhf-linux"))

    (home-page "https://www.gnu.org/software/mit-scheme/")
    (synopsis "A Scheme implementation with integrated editor and debugger")
    (description
     "GNU/MIT Scheme is an implementation of the Scheme programming
language.  It provides an interpreter, a compiler and a debugger.  It also
features an integrated Emacs-like editor and a large runtime library.")
    (license gpl2+)
    (properties '((ftp-directory . "/gnu/mit-scheme/stable.pkg")))))

(define-public bigloo
  (package
    (name "bigloo")
    (version "4.3a")
    (source (origin
             (method url-fetch)
             (uri (string-append "ftp://ftp-sop.inria.fr/indes/fp/Bigloo/bigloo"
                                 version ".tar.gz"))
             (sha256
              (base32
               "03rcqs6kvy2j5lqk4fidqay5qfyp474qqspbh6wk4qdbds6w599w"))
             ;; Remove bundled libraries.
             (modules '((guix build utils)))
             (snippet
              '(for-each delete-file-recursively
                         '("gc" "gmp" "libuv")))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)

             (substitute* "configure"
               (("^shell=.*$")
                (string-append "shell=" (which "bash") "\n"))
               (("`date`") "0"))
             (substitute* "autoconf/runtest.in"
               ((", @DATE@") ""))
             (substitute* "autoconf/osversion"
               (("^version.*$") "version=\"\"\n"))

             ;; The `configure' script doesn't understand options
             ;; of those of Autoconf.
             (let ((out (assoc-ref outputs "out")))
               (zero?
                (system* "./configure"
                         (string-append "--prefix=" out)
                         ; use system libraries
                         "--customgc=no"
                         "--customunistring=no"
                         "--customlibuv=no"
                         (string-append"--mv=" (which "mv"))
                         (string-append "--rm=" (which "rm"))
                         "--cflags=-fPIC"
                         (string-append "--ldflags=-Wl,-rpath="
                                        (assoc-ref outputs "out")
                                        "/lib/bigloo/" ,version)
                         (string-append "--lispdir=" out
                                        "/share/emacs/site-lisp")
                         "--sharedbde=yes"
                         "--sharedcompiler=yes")))))
         (add-after 'install 'install-emacs-modes
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (dir (string-append out "/share/emacs/site-lisp")))
               (zero? (system* "make" "-C" "bmacs" "all" "install"
                               (string-append "EMACSBRAND=emacs25")
                               (string-append "EMACSDIR=" dir)))))))))
    (inputs
     `(("emacs" ,emacs)                      ;UDE needs the X version of Emacs
       ("libgc" ,libgc)
       ("libunistring" ,libunistring)
       ("libuv" ,libuv)
       ("openssl" ,openssl)
       ("sqlite" ,sqlite)

       ;; Optional APIs for which Bigloo has bindings.
       ("avahi" ,avahi)
       ("libphidget" ,libphidget)
       ("pcre" ,pcre)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("gmp" ,gmp)))                             ; bigloo.h refers to gmp.h
    (home-page "http://www-sop.inria.fr/indes/fp/Bigloo/")
    (synopsis "Efficient Scheme compiler")
    (description
     "Bigloo is a Scheme implementation devoted to one goal: enabling
Scheme based programming style where C(++) is usually
required.  Bigloo attempts to make Scheme practical by offering
features usually presented by traditional programming languages
but not offered by Scheme and functional programming.  Bigloo
compiles Scheme modules.  It delivers small and fast stand alone
binary executables.  Bigloo enables full connections between
Scheme and C programs and between Scheme and Java programs.")
    (license gpl2+)))

(define-public hop
  (package
    (name "hop")
    (version "3.1.0-pre2")
    (source (origin
             (method url-fetch)
             (uri (string-append "ftp://ftp-sop.inria.fr/indes/fp/Hop/hop-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "09m7pahjsp7wxzd20cdph9j3mgf2nq5dyckcjljcd40m25v85kks"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:make-flags '("BIGLOO=bigloo")
       #:parallel-build? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (zero?
                (system* "./configure"
                         (string-append "--prefix=" out)
                         (string-append "--blflags="
                                        ;; user flags completely override useful
                                        ;; default flags, so repeat them here.
                                        "-copt \\$(CPICFLAGS) "
                                        "-L \\$(BUILDLIBDIR) "
                                        "-ldopt -Wl,-rpath," out "/lib")))))))))
    (inputs `(("avahi" ,avahi)
              ("bigloo" ,bigloo)
              ("libgc" ,libgc)
              ("libunistring" ,libunistring)
              ("libuv" ,libuv)
              ("pcre" ,pcre)
              ("sqlite" ,sqlite)
              ("which" ,which)))
    (home-page "http://hop.inria.fr/")
    (synopsis "Multi-tier programming language for the Web 2.0")
    (description
     "HOP is a multi-tier programming language for the Web 2.0 and the
so-called diffuse Web.  It is designed for programming interactive web
applications in many fields such as multimedia (web galleries, music players,
...), ubiquitous and house automation (SmartPhones, personal appliance),
mashups, office (web agendas, mail clients, ...), etc.")
    (license gpl2+)))

(define-public chicken
  (package
    (name "chicken")
    (version "4.13.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://code.call-cc.org/releases/"
                                  version "/chicken-" version ".tar.gz"))
              (sha256
               (base32
                "0hvckhi5gfny3mlva6d7y9pmx7cbwvq0r7mk11k3sdiik9hlkmdd"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       
       ;; No `configure' script; run "make check" after "make install" as
       ;; prescribed by README.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'check)
         (add-after 'install 'check
           (assoc-ref %standard-phases 'check)))

       #:make-flags (let ((out (assoc-ref %outputs "out")))
                      (list "PLATFORM=linux"
                            (string-append "PREFIX=" out)
                            (string-append "VARDIR=" out "/var/lib")))

       ;; Parallel builds are not supported, as noted in README.
       #:parallel-build? #f))
    (home-page "http://www.call-cc.org/")
    (synopsis "R5RS Scheme implementation that compiles native code via C")
    (description
     "CHICKEN is a compiler for the Scheme programming language.  CHICKEN
produces portable and efficient C, supports almost all of the R5RS Scheme
language standard, and includes many enhancements and extensions.")
    (license bsd-3)))

(define-public scheme48
  (package
    (name "scheme48")
    (version "1.9.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://s48.org/" version
                                 "/scheme48-" version ".tgz"))
             (sha256
              (base32
               "1x4xfm3lyz2piqcw1h01vbs1iq89zq7wrsfjgh3fxnlm1slj2jcw"))
             (patches (search-patches "scheme48-tests.patch"))))
    (build-system gnu-build-system)
    (home-page "http://s48.org/")
    (synopsis "Scheme implementation using a bytecode interpreter")
    (description
     "Scheme 48 is an implementation of Scheme based on a byte-code
interpreter and is designed to be used as a testbed for experiments in
implementation techniques and as an expository tool.")

    ;; Most files are BSD-3; see COPYING for the few exceptions.
    (license bsd-3)))

(define-public racket
  (package
    (name "racket")
    (version "6.11")
    (source (origin
             (method url-fetch)
             (uri (list (string-append "http://mirror.racket-lang.org/installers/"
                                       version "/racket-" version "-src.tgz")
                        (string-append
                         "http://mirror.informatik.uni-tuebingen.de/mirror/racket/"
                         version "/racket-" version "-src.tgz")))
             (sha256
              (base32
               "1nk7705x24jjlbqqhj8yvbgqkfscxx3m81bry1g56kjxysjmf3sw"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'pre-configure
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Patch dynamically loaded libraries with their absolute paths.
             (let* ((library-path   (search-path-as-string->list
                                     (getenv "LIBRARY_PATH")))
                    (find-so        (lambda (soname)
                                      (search-path
                                       library-path
                                       (format #f "~a.so" soname))))
                    (patch-ffi-libs (lambda (file libs)
                                      (for-each
                                       (lambda (lib)
                                         (substitute* file
                                           (((format #f "\"~a\"" lib))
                                            (format #f "\"~a\"" (find-so lib)))))
                                       libs))))
               (substitute* "collects/db/private/sqlite3/ffi.rkt"
                 (("ffi-lib sqlite-so")
                  (format #f "ffi-lib \"~a\"" (find-so "libsqlite3"))))
               (substitute* "collects/openssl/libssl.rkt"
                 (("ffi-lib libssl-so")
                  (format #f "ffi-lib \"~a\"" (find-so "libssl"))))
               (substitute* "collects/openssl/libcrypto.rkt"
                 (("ffi-lib libcrypto-so")
                  (format #f "ffi-lib \"~a\"" (find-so "libcrypto"))))
               (substitute* "share/pkgs/math-lib/math/private/bigfloat/gmp.rkt"
                 (("ffi-lib libgmp-so")
                  (format #f "ffi-lib \"~a\"" (find-so "libgmp"))))
               (substitute* "share/pkgs/math-lib/math/private/bigfloat/mpfr.rkt"
                 (("ffi-lib libmpfr-so")
                  (format #f "ffi-lib \"~a\"" (find-so "libmpfr"))))
               (for-each
                (lambda (x) (apply patch-ffi-libs x))
                '(("share/pkgs/draw-lib/racket/draw/unsafe/cairo-lib.rkt"
                   ("libfontconfig" "libcairo"))
                  ("share/pkgs/draw-lib/racket/draw/unsafe/glib.rkt"
                   ("libglib-2.0" "libgmodule-2.0" "libgobject-2.0"))
                  ("share/pkgs/draw-lib/racket/draw/unsafe/jpeg.rkt"
                   ("libjpeg"))
                  ("share/pkgs/draw-lib/racket/draw/unsafe/pango.rkt"
                   ("libpango-1.0" "libpangocairo-1.0"))
                  ("share/pkgs/draw-lib/racket/draw/unsafe/png.rkt"
                   ("libpng"))
                  ("share/pkgs/db-lib/db/private/odbc/ffi.rkt"
                   ("libodbc"))
                  ("share/pkgs/gui-lib/mred/private/wx/gtk/x11.rkt"
                   ("libX11"))
                  ("share/pkgs/gui-lib/mred/private/wx/gtk/gsettings.rkt"
                   ("libgio-2.0"))
                  ("share/pkgs/gui-lib/mred/private/wx/gtk/gtk3.rkt"
                   ("libgdk-3" "libgtk-3"))
                  ("share/pkgs/gui-lib/mred/private/wx/gtk/unique.rkt"
                   ("libunique-1.0"))
                  ("share/pkgs/gui-lib/mred/private/wx/gtk/utils.rkt"
                   ("libgdk-x11-2.0" "libgdk_pixbuf-2.0" "libgtk-x11-2.0"))
                  ("share/pkgs/gui-lib/mred/private/wx/gtk/gl-context.rkt"
                   ("libGL"))
                  ("share/pkgs/sgl/gl.rkt"
                   ("libGL" "libGLU")))))
             (chdir "src")
             #t))
         (add-after 'unpack 'patch-/bin/sh
           (lambda _
             (substitute* "collects/racket/system.rkt"
               (("/bin/sh") (which "sh")))
             #t)))
       #:tests? #f                                ; XXX: how to run them?
       ))
    (inputs
     `(("libffi" ,libffi)
       ;; Hardcode dynamically loaded libraries for better functionality.
       ;; sqlite and libraries for `racket/draw' are needed to build the doc.
       ("cairo" ,cairo)
       ("fontconfig" ,fontconfig)
       ("glib" ,glib)
       ("glu" ,glu)
       ("gmp" ,gmp)
       ("gtk+" ,gtk+)  ; propagates gdk-pixbuf+svg
       ("libjpeg" ,libjpeg)
       ("libpng" ,libpng)
       ("libx11" ,libx11)
       ("mesa" ,mesa)
       ("mpfr" ,mpfr)
       ("openssl" ,openssl)
       ("pango" ,pango)
       ("sqlite" ,sqlite)
       ("unixodbc" ,unixodbc)))
    (home-page "http://racket-lang.org")
    (synopsis "Implementation of Scheme and related languages")
    (description
     "Racket is an implementation of the Scheme programming language (R5RS and
R6RS) and related languages, such as Typed Racket.  It features a compiler and
a virtual machine with just-in-time native compilation, as well as a large set
of libraries.")
    (license lgpl2.0+)))

(define-public gambit-c
  (package
    (name "gambit-c")
    (version "4.8.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://www.iro.umontreal.ca/~gambit/download/gambit/v"
             (version-major+minor version) "/source/gambit-v"
             (string-map (lambda (c) (if (char=? c #\.) #\_ c)) version)
             ".tgz"))
       (sha256
        (base32 "1plw1id94mpg2c4y6q9z39ndcz1hbxfnp3i08szsg6794rasmgkk"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       ;; According to the ./configure script, this makes the build slower and
       ;; use >= 1 GB memory, but makes Gambit much faster.
       '("--enable-single-host")
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-tests
           (lambda _
             (substitute* '("tests/makefile")
               ;; '-:' is how run-time options are set.  'tl' sets some terminal
               ;; option, which makes it fail in our build environment.  It
               ;; recommends using 'd-' as a solution, which sets the REPL
               ;; interaction channel to stdin/stdout.
               (("gsi -:tl") "gsi -:d-,tl"))
             #t)))))
    (home-page "http://www.iro.umontreal.ca/~gambit/")
    (synopsis "Efficient Scheme interpreter and compiler")
    (description
     "Gambit consists of two main programs: gsi, the Gambit Scheme
interpreter, and gsc, the Gambit Scheme compiler.  The interpreter contains
the complete execution and debugging environment.  The compiler is the
interpreter extended with the capability of generating executable files.  The
compiler can produce standalone executables or compiled modules which can be
loaded at run time.  Interpreted code and compiled code can be freely
mixed.")
    ;; Dual license.
    (license (list lgpl2.1+ asl2.0))))

(define-public chibi-scheme
  (package
    (name "chibi-scheme")
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/ashinn/chibi-scheme/archive/"
                           version ".tar.gz"))
       (sha256
        (base32 "16wppf4qzr0748iyp0m89gidsfgq9s6x3gw4xggym91waw4fh742"))
       (file-name (string-append "chibi-scheme-" version ".tar.gz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-before 'build 'set-cc
                    (lambda _
                      (setenv "CC" "gcc"))))
       #:make-flags (let ((out (assoc-ref %outputs "out")))
                      (list (string-append "PREFIX=" out)
                            (string-append "LDFLAGS=-Wl,-rpath=" out "/lib")))
       #:test-target "test"))
    (home-page "https://github.com/ashinn/chibi-scheme")
    (synopsis "Small embeddable Scheme implementation")
    (description
     "Chibi-Scheme is a very small library with no external dependencies
intended for use as an extension and scripting language in C programs.  In
addition to support for lightweight VM-based threads, each VM itself runs in
an isolated heap allowing multiple VMs to run simultaneously in different OS
threads.")
    (license bsd-3)))

(define-public scmutils
  (let ()
    (define (system-suffix)
      (cond
       ((string-prefix? "x86_64" (or (%current-target-system)
                                     (%current-system)))
        "x86-64")
       (else "i386")))

    (package
      (name "scmutils")
      (version "20160827")
      (source
       (origin
         (method url-fetch/tarbomb)
         (modules '((guix build utils)))
         (snippet
          ;; Remove binary code
          '(delete-file-recursively "scmutils/mit-scheme"))
         (uri (string-append "http://groups.csail.mit.edu/mac/users/gjs/6946"
                             "/scmutils-tarballs/" name "-" version
                             "-x86-64-gnu-linux.tar.gz"))
         (sha256
          (base32 "00ly5m0s4dy5kxravjaqlpii5zcnr6b9nqm0607lr7xcs52i4j8b"))))
      (build-system gnu-build-system)
      (inputs
       `(("mit-scheme" ,mit-scheme)
         ("emacs" ,emacs-minimal)))
      (arguments
       `(#:tests? #f ;; no tests-suite
         #:modules ((guix build gnu-build-system)
                    (guix build utils)
                    (guix build emacs-utils))
         #:imported-modules (,@%gnu-build-system-modules
                             (guix build emacs-utils))
         #:phases
         (modify-phases %standard-phases
           (replace 'configure
             ;; No standard build procedure is used. We set the correct
             ;; runtime path in the custom build system.
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 ;; Required to find .bci files at runtime.
                 (with-directory-excursion "scmutils"
                   (rename-file "src" "scmutils"))
                 (substitute* "scmutils/scmutils/load.scm"
                   (("/usr/local/scmutils/")
                    (string-append out "/lib/mit-scheme-"
                                   ,(system-suffix) "/")))
                 #t)))
           (replace 'build
             ;; Compile the code and build a band.
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (make-img (string-append
                                 "echo '(load \"load\") "
                                 "(disk-save \"edwin-mechanics.com\")'"
                                 "| mit-scheme")))
                 (with-directory-excursion "scmutils/scmutils"
                   (and (zero? (system "mit-scheme < compile.scm"))
                        (zero? (system make-img)))))))
           (add-before 'install 'fix-directory-names
             ;; Correct directory names in the startup script.
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (scm-root (assoc-ref inputs "mit-scheme")))
                 (substitute* "bin/mechanics"
                   (("ROOT=\"\\$\\{SCMUTILS_ROOT:-/.*\\}\"")
                    (string-append
                     "ROOT=\"${SCMUTILS_ROOT:-" scm-root "}\"\n"
                     "LIB=\"${ROOT}/lib/mit-scheme-"
                     ,(system-suffix) ":"
                     out "/lib/mit-scheme-" ,(system-suffix) "\""))
                   (("EDWIN_INFO_DIRECTORY=.*\n") "")
                   (("SCHEME=.*\n")
                    (string-append "SCHEME=\"${ROOT}/bin/scheme "
                                   "--library ${LIB}\"\n"))
                   (("export EDWIN_INFO_DIRECTORY") ""))
                 #t)))
           (add-before 'install 'emacs-tags
             ;; Generate Emacs's tags for easy reference to source
             ;; code.
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (with-directory-excursion "scmutils/scmutils"
                 (zero? (apply system* "etags"
                               (find-files "." "\\.scm"))))))
           (replace 'install
             ;; Copy files to the store.
             (lambda* (#:key outputs #:allow-other-keys)
               (define* (copy-files-to-directory files dir
                                                 #:optional (delete? #f))
                 (for-each (lambda (f)
                             (copy-file f (string-append dir "/" f))
                             (when delete? (delete-file f)))
                           files))

               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin"))
                      (doc (string-append out "/share/doc/"
                                          ,name "-" ,version))
                      (lib (string-append out "/lib/mit-scheme-"
                                          ,(system-suffix)
                                          "/scmutils")))
                 (for-each mkdir-p (list lib doc bin))
                 (with-directory-excursion "scmutils/scmutils"
                   (copy-files-to-directory '("COPYING" "LICENSE")
                                            doc #t)
                   (for-each delete-file (find-files "." "\\.bin"))
                   (copy-files-to-directory '("edwin-mechanics.com")
                                            (string-append lib "/..") #t)
                   (copy-recursively "." lib))
                 (with-directory-excursion "bin"
                   (copy-files-to-directory (find-files ".") bin))
                 (with-directory-excursion "scmutils/manual"
                   (copy-files-to-directory (find-files ".") doc))
                 #t)))
           (add-after 'install 'emacs-helpers
             ;; Add convenience Emacs commands to easily load the
             ;; Scmutils band in an MIT-Scheme buffer inside of Emacs
             ;; and to easily load code tags.
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (mit-root (assoc-ref inputs "mit-scheme"))
                      (emacs-lisp-dir
                       (string-append out "/share/emacs/site-lisp"
                                      "/guix.d/" ,name "-" ,version))
                      (el-file (string-append emacs-lisp-dir
                                              "/scmutils.el"))
                      (lib-relative-path
                       (string-append "/lib/mit-scheme-"
                                      ,(system-suffix))))
                 (mkdir-p emacs-lisp-dir)
                 (call-with-output-file el-file
                   (lambda (p)
                     (format p
                             ";;;###autoload
(defun scmutils-load ()
  (interactive)
  (require 'xscheme)
  (let ((mit-root \"~a\")
    (scmutils \"~a\"))
    (run-scheme
     (concat mit-root \"/bin/scheme --library \"
          mit-root \"~a:\" scmutils \"~a\"
          \" --band edwin-mechanics.com\"
          \" --emacs\"))))

;;;###autoload
(defun scmutils-load-tags ()
  (interactive)
  (let ((scmutils \"~a\"))
    (visit-tags-table (concat scmutils \"/TAGS\"))))
"
                                      mit-root out
                                      lib-relative-path
                                      lib-relative-path
                                      (string-append out lib-relative-path
                                                     "/scmutils"))))
                          (emacs-generate-autoloads ,name emacs-lisp-dir)
                          (emacs-byte-compile-directory emacs-lisp-dir)
                          #t))))))
      (home-page
       "http://groups.csail.mit.edu/mac/users/gjs/6946/linux-install.htm")
      (synopsis "Scmutils library for MIT Scheme")
      (description "The Scmutils system is an integrated library of
procedures, embedded in the programming language Scheme, and intended to
support teaching and research in mathematical physics and electrical
engineering.")
      (license gpl2+))))

(define-public sicp
  (let ((commit "225c172f9b859902a64a3c5dd5e1f9ac1a7382de"))
    (package
      (name "sicp")
      (version (string-append "20170703-1." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/sarabander/sicp")
                      (commit commit)))
                (sha256
                 (base32
                  "0bhdrdc1mgdjdsg4jksq9z6x129f3346jbf3zir2a0dfmsj6m10n"))
                (file-name (string-append name "-" version "-checkout"))))
      (build-system trivial-build-system)
      (native-inputs `(("gzip" ,gzip)
                       ("source" ,source)
                       ("texinfo" ,texinfo)))
      (arguments
       `(#:modules ((guix build utils)
                    (srfi srfi-1)
                    (srfi srfi-26))
         #:builder
         (begin
           (use-modules (guix build utils)
                        (srfi srfi-1)
                        (srfi srfi-26))
           (let ((gzip (assoc-ref %build-inputs "gzip"))
                 (source (assoc-ref %build-inputs "source"))
                 (texinfo (assoc-ref %build-inputs "texinfo"))
                 (html-dir (string-append %output "/share/doc/" ,name "/html"))
                 (info-dir (string-append %output "/share/info")))
             (copy-recursively (string-append source "/html") html-dir)
             (setenv "PATH" (string-append gzip "/bin"
                                           ":" texinfo "/bin"))
             (mkdir-p info-dir)
             (and (zero?
                   (system* "makeinfo" "--output"
                            (string-append info-dir "/sicp.info")
                            (string-append source "/sicp-pocket.texi")))
                  (every zero?
                         (map (cut system* "gzip" "-9n" <>)
                              (find-files info-dir))))))))
      (home-page "https://sarabander.github.io/sicp")
      (synopsis "Structure and Interpretation of Computer Programs")
      (description "Structure and Interpretation of Computer Programs (SICP) is
a textbook aiming to teach the principles of computer programming.

Using Scheme, a dialect of the Lisp programming language, the book explains
core computer science concepts such as abstraction in programming,
metalinguistic abstraction, recursion, interpreters, and modular programming.")
      (license cc-by-sa4.0))))

(define-public scheme48-rx
  (let* ((commit "dd9037f6f9ea01019390614f6b126b7dd293798d")
         (revision "2"))
    (package
      (name "scheme48-rx")
      (version (string-append "0.0.0-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/scheme/rx")
               (commit commit)))
         (sha256
          (base32
           "1bvriavxw5kf2izjbil3999vr983vkk2xplfpinafr86m40b2cci"))
         (file-name (string-append name "-" version "-checkout"))))
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (let ((share (string-append %output
                                       "/share/scheme48-"
                                       ,(package-version scheme48)
                                       "/rx")))
             (chdir (assoc-ref %build-inputs "source"))
             (mkdir-p share)
             (copy-recursively "." share)))))
      (native-inputs
       `(("source" ,source)
         ("scheme48" ,scheme48)))
      (home-page "https://github.com/scheme/rx/")
      (synopsis "SRE String pattern-matching library for scheme48")
      (description
       "String pattern-matching library for scheme48 based on the SRE
regular-expression notation.")
      (license bsd-3))))

(define-public slib
  (package
    (name "slib")
    (version "3b5")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://groups.csail.mit.edu/mac/ftpdir/scm/slib-"
                                 version ".zip"))
             (sha256
              (base32
               "0q0p2d53p8qw2592yknzgy2y1p5a9k7ppjx0cfrbvk6242c4mdpq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; There is no check target.
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'remove-bin-share
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (delete-file-recursively
                       (string-append (assoc-ref outputs "out") "/bin"))))
         (replace 'configure
                  (lambda* (#:key inputs outputs #:allow-other-keys)
                    (zero? (system* "./configure"
                                    (string-append "--prefix="
                                                   (assoc-ref outputs "out")))))))))
    (native-inputs `(("unzip" ,unzip)
                     ("texinfo" ,texinfo)))
    (home-page "http://people.csail.mit.edu/jaffer/SLIB.html")
    (synopsis "Compatibility and utility library for Scheme")
    (description "SLIB is a portable Scheme library providing compatibility and
utility functions for all standard Scheme implementations.")
    (license (non-copyleft
              "http://people.csail.mit.edu/jaffer/SLIB_COPYING.txt"
              "Or see COPYING in the distribution."))))

(define-public scm
  (package
    (name "scm")
    (version "5f2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://groups.csail.mit.edu/mac/ftpdir/scm/scm-"
                    version ".zip"))
              (sha256
               (base32
                "050ijb51jm1cij9g3r89zl9rawsrikhbb5y8zb7lspb7bsxq5w99"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
                  (lambda* (#:key inputs outputs #:allow-other-keys)
                    (zero? (system* "./configure"
                                    (string-append "--prefix="
                                                   (assoc-ref outputs "out"))))))
         (add-before 'build 'pre-build
                     (lambda* (#:key inputs #:allow-other-keys)
                       (substitute* "Makefile"
                         (("ginstall-info") "install-info"))))
         (replace 'build
                  (lambda* (#:key inputs outputs #:allow-other-keys)
                    (setenv "SCHEME_LIBRARY_PATH"
                            (string-append (assoc-ref inputs "slib")
                                           "/lib/slib/"))
                    (and
                     (zero? (system* "make" "scmlit" "CC=gcc"))
                     (zero? (system* "make" "all")))))
         (add-after 'install 'post-install
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((req
                             (string-append (assoc-ref outputs "out")
                                            "/lib/scm/require.scm")))
                        (and
                         (delete-file req)
                         (format (open req (logior O_WRONLY O_CREAT))
                                 "(define (library-vicinity) ~s)\n"
                                 (string-append (assoc-ref inputs "slib")
                                                "/lib/slib/"))

                         ;; We must generate the slibcat file
                         (zero? (system*
                                 (string-append
                                  (assoc-ref outputs "out")
                                  "/bin/scm")
                                 "-br" "new-catalog")))))))))
    (inputs `(("slib" ,slib)))
    (native-inputs `(("unzip" ,unzip)
                     ("texinfo" ,texinfo)))
    (home-page "http://people.csail.mit.edu/jaffer/SCM")
    (synopsis "Scheme implementation conforming to R5RS and IEEE P1178")
    (description "GNU SCM is an implementation of Scheme.  This
implementation includes Hobbit, a Scheme-to-C compiler, which can
generate C files whose binaries can be dynamically or statically
linked with a SCM executable.")
    (license lgpl3+)))
