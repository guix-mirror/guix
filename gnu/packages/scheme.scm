;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2018, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015, 2016 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2017 John Darrington <jmd@gnu.org>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017, 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Adam Massmann <massmannak@gmail.com>
;;; Copyright © 2018 Gabriel Hondet <gabrielhondet@gmail.com>
;;; Copyright © 2020 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 Edouard Klein <edk@beaver-labs.com>
;;; Copyright © 2021 Philip McGrath <philip@philipmcgrath.com>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021 Foo Chuan Wei <chuanwei.foo@hotmail.com>
;;; Copyright © 2022 Morgan Smith <Morgan.J.Smith@outlook.com>
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
                #:select (gpl2+ lgpl2.0+ lgpl2.1 lgpl2.1+ lgpl3+ asl2.0 bsd-3
                          cc-by-sa4.0 non-copyleft expat public-domain))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libunistring)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
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
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages linux)
  #:use-module (srfi srfi-1)
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
    (version "11.2")
    (source #f)                                   ; see below
    (outputs '("out" "doc"))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key inputs #:allow-other-keys)
             (invoke "tar" "xzvf"
                     (assoc-ref inputs "source"))
             (chdir ,(mit-scheme-source-directory (%current-system)
                                                  version))
             ;; Delete these dangling symlinks since they break
             ;; `patch-shebangs'.
             (for-each delete-file
                       (find-files "src/compiler" "^make\\."))
             (chdir "src")
             #t))
         (add-after 'unpack 'patch-/bin/sh
           (lambda _
             (setenv "CONFIG_SHELL" (which "sh"))
             (substitute* '("../tests/ffi/autogen.sh"
                            "../tests/ffi/autobuild.sh"
                            "../tests/ffi/test-ffi.sh"
                            "../tests/runtime/test-process.scm"
                            "runtime/unxprm.scm")
               (("/bin/sh") (which "sh"))
               (("\\./autogen\\.sh")
                (string-append (which "sh") " autogen.sh"))
               (("\\./configure")
                (string-append (which "sh") " configure")))
             #t))
         (replace 'build
           (lambda* (#:key system outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (if (or (string-prefix? "x86_64" system)
                       (string-prefix? "i686" system))
                   (invoke "make" "compile-microcode")
                   (invoke "./etc/make-liarc.sh"
                           (string-append "--prefix=" out)))
               #t)))
         (add-after 'configure 'configure-doc
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (with-directory-excursion "../doc"
               (let* ((out (assoc-ref outputs "out"))
                      (bash (assoc-ref inputs "bash"))
                      (bin/sh (string-append bash "/bin/sh")))
                 (invoke bin/sh "./configure"
                         (string-append "--prefix=" out)
                         (string-append "SHELL=" bin/sh))
                 #t))))
         (add-after 'build 'build-doc
           (lambda* _
             (with-directory-excursion "../doc"
               (invoke "make"))
             #t))
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc"))
                    (old-doc-dir (string-append out "/share/doc"))
                    (new-doc/mit-scheme-dir
                     (string-append doc "/share/doc/" ,name "-" ,version)))
               (with-directory-excursion "../doc"
                 (for-each (lambda (target)
                             (invoke "make" target))
                           '("install-info-gz" "install-man"
                             "install-html" "install-pdf")))
               (mkdir-p new-doc/mit-scheme-dir)
               (copy-recursively
                (string-append old-doc-dir "/" ,name)
                new-doc/mit-scheme-dir)
               (delete-file-recursively old-doc-dir)
               #t))))))
    (native-inputs
     `(;; Autoconf, Automake, and Libtool are necessary for the FFI tests.
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("texlive" ,(texlive-updmap.cfg (list texlive-tex-texinfo
                                             texlive-epsf)))
       ("texinfo" ,texinfo)
       ("ghostscript" ,ghostscript)
       ("m4" ,m4)))
    (inputs
     `(("libx11" ,libx11)
       ("ncurses" ,ncurses)

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
                                ("aarch64-linux"
                                 (string-append version "-aarch64le"))
                                (_
                                 (string-append "c-" version)))
                              ".tar.gz"))
          (sha256
           (match (%current-system)
             ("x86_64-linux"
              (base32
               "17822hs9y07vcviv2af17p3va7qh79dird49nj50bwi9rz64ia3w"))
             ("aarch64-linux"
              (base32
               "11maixldk20wqb5js5p4imq221zz9nf27649v9pqkdf8fv7rnrs9"))
             (_
              (base32
               "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))))))))

    ;; Fails to build on MIPS, see <http://bugs.gnu.org/18221>.
    ;; Also, the portable C version of MIT/GNU Scheme did not work in time for
    ;; release in version 10.1.
    (supported-systems '("x86_64-linux" "i686-linux"))

    (home-page "https://www.gnu.org/software/mit-scheme/")
    (synopsis "Scheme implementation with integrated editor and debugger")
    (description
     "GNU/MIT Scheme is an implementation of the Scheme programming
language.  It provides an interpreter, a compiler and a debugger.  It also
features an integrated Emacs-like editor and a large runtime library.")
    (license gpl2+)
    (properties '((ftp-directory . "/gnu/mit-scheme/stable.pkg")))))

(define-public bigloo
  ;; Upstream modifies source tarballs in place, making significant changes
  ;; long after the initial publication: <https://bugs.gnu.org/33525>.
  (let ((upstream-version "4.3g"))
    (package
      (name "bigloo")
      (version "4.3g")
      (source (origin
                (method url-fetch)
                (uri (string-append "ftp://ftp-sop.inria.fr/indes/fp/Bigloo/bigloo"
                                    upstream-version ".tar.gz"))
                (sha256
                 (base32
                  "07305c134v7s1nz44igwsyqpb9qqia5zyng1q2qj60sskw3nbd67"))
                ;; Remove bundled libraries.
                (modules '((guix build utils)))
                (snippet
                 '(begin
                    (for-each delete-file-recursively
                              '("gc" "gmp" "libuv" "libunistring" "pcre"))
                    #t))))
      (build-system gnu-build-system)
      (arguments
       `(#:test-target "test"
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-gmp-detection
             (lambda _
               (substitute* "configure"
                 (("gmpversion=`\\$autoconf gmp --lib=\\$gmplib`")
                  "gmpversion=`\\$autoconf gmp --lib=\"\\$gmplib\"`"))))
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
               (substitute* "comptime/Makefile"
                 (("\\$\\(LDCOMPLIBS\\)")
                  "$(LDCOMPLIBS) $(LDFLAGS)"))

               ;; The `configure' script doesn't understand options
               ;; of those of Autoconf.
               (let ((out (assoc-ref outputs "out")))
                 (invoke "./configure"
                         (string-append "--prefix=" out)
                                                  ; use system libraries
                         "--customgc=no"
                         "--enable-gmp"
                         "--customgmp=no"
                         "--customunistring=no"
                         "--customlibuv=no"
                         (string-append"--mv=" (which "mv"))
                         (string-append "--rm=" (which "rm"))
                         "--cflags=-fPIC"
                         (string-append "--ldflags=-Wl,-rpath="
                                        (assoc-ref outputs "out")
                                        "/lib/bigloo/" ,upstream-version)
                         (string-append "--lispdir=" out
                                        "/share/emacs/site-lisp")
                         "--sharedbde=yes"
                         "--sharedcompiler=yes"
                         "--disable-patch"))))
           (add-after 'install 'install-emacs-modes
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (dir (string-append out "/share/emacs/site-lisp")))
                 (invoke "make" "-C" "bmacs" "all" "install"
                         (string-append "EMACSBRAND=emacs25")
                         (string-append "EMACSDIR=" dir))))))))
      (inputs
       (list emacs ;UDE needs the X version of Emacs
             libgc
             libunistring
             libuv
             openssl
             sqlite
             ;; Optional APIs for which Bigloo has bindings.
             avahi
             libphidget
             pcre))
      (native-inputs
       (list pkg-config))
      (propagated-inputs
       (list gmp))                            ; bigloo.h refers to gmp.h
      (home-page "https://www-sop.inria.fr/indes/fp/Bigloo/")
      (synopsis "Efficient Scheme compiler")
      (description
       "Bigloo is a Scheme implementation devoted to one goal: enabling Scheme
based programming style where C(++) is usually required.  Bigloo attempts to
make Scheme practical by offering features usually presented by traditional
programming languages but not offered by Scheme and functional programming.
Bigloo compiles Scheme modules.  It delivers small and fast stand alone binary
executables.  Bigloo enables full connections between Scheme and C programs
and between Scheme and Java programs.")
      (license gpl2+))))

(define-public hop
  (package
    (name "hop")
    (version "3.2.0-pre1")
    (source (origin
             (method url-fetch)
             (uri (string-append "ftp://ftp-sop.inria.fr/indes/fp/Hop/hop-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0jf418d0s9imv98s6qrpjxr1mdaxr37knh5qyfl5y4a9cc41mlg5"))))
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
               (substitute* '("tools/Makefile"
                              "test/hopjs/TEST.in")
                 (("/bin/rm") (which "rm")))
               (invoke "./configure"
                       (string-append "--prefix=" out)
                       "--hostcc=gcc"
                       (string-append "--blflags="
                                      ;; user flags completely override useful
                                      ;; default flags, so repeat them here.
                                      "-copt \\$(CPICFLAGS) "
                                      "-L \\$(BUILDLIBDIR) "
                                      "-ldopt -Wl,-rpath," out "/lib"))))))))
    (inputs (list avahi
                  bigloo
                  libgc
                  libunistring
                  libuv
                  pcre
                  sqlite
                  which))
    (home-page "http://hop.inria.fr/")
    (synopsis "Multi-tier programming language for the Web 2.0")
    (description
     "HOP is a multi-tier programming language for the Web 2.0 and the
so-called diffuse Web.  It is designed for programming interactive web
applications in many fields such as multimedia (web galleries, music players,
...), ubiquitous and house automation (SmartPhones, personal appliance),
mashups, office (web agendas, mail clients, ...), etc.")
    (license gpl2+)))

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

(define-public gambit-c
  (package
    (name "gambit-c")
    (version "4.9.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://www.iro.umontreal.ca/~gambit/download/gambit/v"
             (version-major+minor version) "/source/gambit-v"
             (string-map (lambda (c) (if (char=? c #\.) #\_ c)) version)
             ".tgz"))
       (sha256
        (base32 "1p6172vhcrlpjgia6hsks1w4fl8rdyjf9xjh14wxfkv7dnx8a5hk"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       ;; According to the ./configure script, this makes the build slower and
       ;; use >= 1 GB memory, but makes Gambit much faster.
       '("--enable-single-host")))
    (home-page "http://dynamo.iro.umontreal.ca/wiki/index.php/Main_Page")
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
    (version "0.10")
    (home-page "https://github.com/ashinn/chibi-scheme")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference (url home-page) (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yhmj0lws3r3bl4ivs31dhlzxqc7f0dinixi904mraz1fmrg3w7f"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure))           ; no configure script
       #:make-flags (let ((out (assoc-ref %outputs "out")))
                      (list (string-append "PREFIX=" out)
                            (string-append "CC=" ,(cc-for-target))
                            (string-append "LDFLAGS=-Wl,-rpath=" out "/lib")))
       #:test-target "test"))
    (synopsis "Small embeddable Scheme implementation")
    (description
     "Chibi-Scheme is a very small library with no external dependencies
intended for use as an extension and scripting language in C programs.  In
addition to support for lightweight VM-based threads, each VM itself runs in
an isolated heap allowing multiple VMs to run simultaneously in different OS
threads.")
    (license bsd-3)))

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
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils)
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
             (invoke "makeinfo" "--output"
                     (string-append info-dir "/sicp.info")
                     (string-append source "/sicp-pocket.texi"))
             (for-each (cut invoke "gzip" "-9n" <>)
                       (find-files info-dir))
             #t))))
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
             (copy-recursively "." share)
             #t))))
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
    (version "3b6")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://groups.csail.mit.edu/mac/ftpdir/scm/slib-"
                                 version ".zip"))
             (sha256
              (base32
               "137dn2wwwwg0qbifgxfckjhzj4m4820crpg9kziv402l7f2b931f"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; There is no check target.
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'remove-bin-share
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (delete-file-recursively
                       (string-append (assoc-ref outputs "out") "/bin"))
                      #t))
         (replace 'configure
                  (lambda* (#:key inputs outputs #:allow-other-keys)
                    (invoke "./configure"
                            (string-append "--prefix="
                                           (assoc-ref outputs "out"))))))))
    (native-inputs (list unzip texinfo))
    (home-page "https://people.csail.mit.edu/jaffer/SLIB.html")
    (synopsis "Compatibility and utility library for Scheme")
    (description "SLIB is a portable Scheme library providing compatibility and
utility functions for all standard Scheme implementations.")
    (license (non-copyleft
              "http://people.csail.mit.edu/jaffer/SLIB_COPYING.txt"
              "Or see COPYING in the distribution."))))

(define-public scm
  (package
    (name "scm")
    (version "5f3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://groups.csail.mit.edu/mac/ftpdir/scm/scm-"
                    version ".zip"))
              (sha256
               (base32
                "1jxxlhmgal26mpcl97kz37djkn97rfy9h5pvw0hah6f3f6w49j97"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
                  (lambda* (#:key inputs outputs #:allow-other-keys)
                    (invoke "./configure"
                            (string-append "--prefix="
                                           (assoc-ref outputs "out")))))
         (add-before 'build 'pre-build
                     (lambda* (#:key inputs #:allow-other-keys)
                       (substitute* "Makefile"
                         (("ginstall-info") "install-info"))
                       #t))
         (replace 'build
                  (lambda* (#:key inputs outputs #:allow-other-keys)
                    (setenv "SCHEME_LIBRARY_PATH"
                            (search-input-directory inputs "lib/slib/"))
                    (invoke "make" "scmlit" "CC=gcc")
                    (invoke "make" "all")))
         (add-after 'install 'post-install
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out         (assoc-ref outputs "out"))
                             (req (string-append out "/lib/scm/require.scm")))
                        (delete-file req)
                        (format (open req (logior O_WRONLY O_CREAT))
                                "(define (library-vicinity) ~s)\n"
                                (search-input-directory inputs "lib/slib/"))

                        ;; We must generate the slibcat file.
                        (invoke (string-append out "/bin/scm")
                                "-br" "new-catalog")))))))
    (inputs (list slib))
    (native-inputs (list unzip texinfo))
    (home-page "https://people.csail.mit.edu/jaffer/SCM")
    (synopsis "Scheme implementation conforming to R5RS and IEEE P1178")
    (description "GNU SCM is an implementation of Scheme.  This
implementation includes Hobbit, a Scheme-to-C compiler, which can
generate C files whose binaries can be dynamically or statically
linked with a SCM executable.")
    (license lgpl3+)))

(define-public tinyscheme
  (package
    (name "tinyscheme")
    (version "1.42")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/" name "/" name "/"
                                  name "-" version "/" name "-" version ".zip"))
              (sha256
               (base32
                "0rik3qnxqd8wjlazx8rw996pfzkjjg60v6hcbpcqzi7rgml8q4n8"))))
    (build-system gnu-build-system)
    (native-inputs
     (list unzip))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key source #:allow-other-keys)
             (invoke "unzip" source)
             (chdir (string-append ,name "-" ,version))
             #t))
         (add-after 'unpack 'set-scm-directory
           ;; Hard-code ‘our’ init.scm instead of looking in the current
           ;; working directory, so invoking ‘scheme’ just works.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (scm (string-append out "/share/" ,name)))
               (substitute* "scheme.c"
                 (("init.scm" all)
                  (string-append scm "/" all)))
               #t)))
         (delete 'configure)            ; no configure script
         (replace 'install
           ;; There's no ‘install’ target.  Install files manually.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (bin     (string-append out "/bin"))
                    (doc     (string-append out "/share/doc/"
                                            ,name "-" ,version))
                    (include (string-append out "/include"))
                    (lib     (string-append out "/lib"))
                    (scm     (string-append out "/share/" ,name)))
               (install-file "scheme" bin)
               (install-file "Manual.txt" doc)
               (install-file "scheme.h" include)
               (install-file "libtinyscheme.so" lib)
               (install-file "init.scm" scm)
               #t))))
       #:tests? #f))                    ; no tests
    (home-page "http://tinyscheme.sourceforge.net/")
    (synopsis "Light-weight interpreter for the Scheme programming language")
    (description
     "TinyScheme is a light-weight Scheme interpreter that implements as large a
subset of R5RS as was possible without getting very large and complicated.

It's meant to be used as an embedded scripting interpreter for other programs.
As such, it does not offer an Integrated Development Environment (@dfn{IDE}) or
extensive toolkits, although it does sport a small (and optional) top-level
loop.

As an embedded interpreter, it allows multiple interpreter states to coexist in
the same program, without any interference between them.  Foreign functions in C
can be added and values can be defined in the Scheme environment.  Being quite a
small program, it is easy to comprehend, get to grips with, and use.")
    (license bsd-3)))                   ; there are no licence headers

(define-public stalin
  (let ((commit "ed1c9e339c352b7a6fee40bb2a47607c3466f0be"))
    ;; FIXME: The Stalin "source" contains C code generated by itself:
    ;; 'stalin-AMD64.c', etc.
    (package
      (name "stalin")
      (version "0.11")
      (source (origin
                ;; Use Pearlmutter's upstream branch with AMD64 patches
                ;; applied. Saves us from including those 20M! patches
                ;; in Guix. For more info, see:
                ;; <ftp.ecn.purdue.edu/qobi/stalin-0.11-amd64-patches.tgz>
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/barak/stalin")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "15a5gxj9v7jqlgkg0543gdflw0rbrir7fj5zgifnb33m074wiyhn"))
                (modules '((guix build utils)))
                (snippet
                 ;; remove gc libs from build, we have them as input
                 '(begin
                    (delete-file "gc6.8.tar.gz")
                    (delete-file-recursively "benchmarks")
                    (substitute* "build"
                      ((".*gc6.8.*") "")
                      (("  cd \\.\\.") "")
                      ((".*B include/libgc.a") "")
                      ((".*make.*") ""))
                    #t))))
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags (list "ARCH_OPTS=-freg-struct-return")
         #:phases
         (modify-phases %standard-phases
           (replace 'configure
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (include-out (string-append out "/include")))
                 (invoke "./build")
                 (for-each (lambda (fname)
                             (install-file fname include-out))
                           (find-files "include"))
                 (substitute* "makefile"
                   (("\\./include") include-out))
                 (substitute* "post-make"
                   (("`pwd`") out))
                 #t)))
           (delete 'check)
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (install-file "stalin.1"
                               (string-append out "/share/man/man1"))
                 (install-file "stalin"
                               (string-append out "/bin"))
                 #t))))))
      (inputs
       (list libx11))
      (propagated-inputs
       (list libgc))
      (supported-systems '("x86_64-linux"))
      (home-page "https://engineering.purdue.edu/~qobi/papers/fdlcc.pdf")
      (synopsis "Brutally efficient Scheme compiler")
      (description
       "Stalin is an aggressively optimizing whole-program compiler
for Scheme that does polyvariant interprocedural flow analysis,
flow-directed interprocedural escape analysis, flow-directed
lightweight CPS conversion, flow-directed lightweight closure
conversion, flow-directed interprocedural lifetime analysis, automatic
in-lining, unboxing, and flow-directed program-specific and
program-point-specific low-level representation selection and code
generation.")
      (license gpl2+))))

(define-public s9fes
  (package
    (name "s9fes")
    (version "20181205")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.t3x.org/s9fes/s9fes-" version ".tgz"))
       (sha256
        (base32 "0ynpl707bc9drwkdpdgvw14bz9zmwd3wffl1k02sxppjl28xm7rf"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "make" "install-all" make-flags))))
       #:tests? #f))  ; No check target.
    (inputs
     (list ncurses))
    (home-page "https://www.t3x.org/s9fes/")
    (synopsis "Interpreter for R4RS Scheme")
    (description
     "Scheme 9 from Empty Space (S9fES) is a mature, portable, and
comprehensible public-domain interpreter for R4RS Scheme offering:
@itemize
@item bignum arithmetics
@item decimal-based real number arithmetics
@item support for low-level Unix programming
@item cursor addressing with Curses
@item basic networking procedures
@item an integrated online help system
@item loads of useful library functions
@end itemize")
    (license public-domain)))

(define-public femtolisp
  (let ((commit "ec7601076a976f845bc05ad6bd3ed5b8cde58a97")
        (revision "2"))
    (package
      (name "femtolisp")
      (version (string-append "0.0.0-" revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/JeffBezanson/femtolisp")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1fcyiqlqn27nd4wxi27km8mhmlzpzzsxzpwsl1bxbmhraq468njw"))))
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
           (delete 'bootstrap)
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
                (invoke "./bootstrap.sh")
                (install-file "flisp.boot" bin)
                #t))))))
      (synopsis "Scheme-like lisp implementation")
      (description
       "@code{femtolisp} is a scheme-like lisp implementation with a
simple, elegant Scheme dialect.  It is a lisp-1 with lexical scope.
The core is 12 builtin special forms and 33 builtin functions.")
      (home-page "https://github.com/JeffBezanson/femtolisp")
      (license bsd-3))))

(define-public gauche
  (package
    (name "gauche")
    (version "0.9.10")
    (home-page "https://practical-scheme.net/gauche/index.html")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://sourceforge/gauche/Gauche/Gauche-"
             version ".tgz"))
       (sha256
        (base32 "0ci57ak5cp3lkmfy3nh50hifh8nbg58hh6r18asq0rn5mqfxyf8g"))
       (modules '((guix build utils)))
       (snippet '(begin
                   ;; Remove libatomic-ops.
                   (delete-file-recursively "gc/libatomic_ops")
                   #t))))
    (build-system gnu-build-system)
    (inputs
     (list libatomic-ops slib zlib))
    (native-inputs
     (list texinfo openssl ; needed for tests
           pkg-config))    ; needed to find external libatomic-ops
    (arguments
     `(#:configure-flags
       (list (string-append "--with-slib="
                            (assoc-ref %build-inputs "slib")
                            "/lib/slib"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-/bin/sh
           ;; Needed only for tests.
           (lambda _
             (substitute* '("test/www.scm"
                            "ext/tls/test.scm"
                            "lib/gauche/package/util.scm"
                            "libsrc/gauche/process.scm")
               (("/bin/sh") (which "sh")))
             #t))
         (add-after 'build 'build-doc
           (lambda _
             (with-directory-excursion "doc"
               (invoke "make" "info"))
             #t))
         (add-before 'check 'patch-network-tests
           ;; Remove net checks.
           (lambda _
             (delete-file "ext/net/test.scm")
             (invoke "touch" "ext/net/test.scm")
             #t))
         (add-after 'install 'install-docs
           (lambda _
             (with-directory-excursion "doc"
               (invoke "make" "install"))
             #t)))))
    (synopsis "Scheme scripting engine")
    (description "Gauche is a R7RS Scheme scripting engine aiming at being a
handy tool that helps programmers and system administrators to write small to
large scripts quickly.  Quick startup, built-in system interface, native
multilingual support are some of the goals.  Gauche comes with a package
manager/installer @code{gauche-package} which can download, compile, install
and list gauche extension packages.")
    (license bsd-3)))

(define-public gerbil
  (package
    (name "gerbil")
    (version "0.16")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vyzo/gerbil")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vng0kxpnwsg8jbjdpyn4sdww36jz7zfpfbzayg9sdpz6bjxjy0f"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'bootstrap)
         (add-before 'configure 'chdir
           (lambda _
             (chdir "src")
             #t))
         (replace 'configure
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (invoke "chmod" "755" "-R" ".")
             ;; Otherwise fails when editing an r--r--r-- file.
             (invoke "gsi-script" "configure"
                     "--prefix" (assoc-ref outputs "out")
                     "--with-gambit" (assoc-ref inputs "gambit-c"))))
         (add-before 'patch-generated-file-shebangs 'fix-gxi-shebangs
           (lambda _
             ;; Some .ss files refer to gxi using /usr/bin/env gxi
             ;; and 'patch-generated-file-shebangs can't fix that
             ;; because gxi has not been compiled yet.
             ;; We know where gxi is going to end up so we
             ;; Doctor Who our fix here before the problem
             ;; happens towards the end of the build.sh script.
             (let ((abs-srcdir (getcwd)))
               (for-each
                (lambda (f)
                   (substitute* f
                     (("#!/usr/bin/env gxi")
                      (string-append "#!" abs-srcdir "/../bin/gxi"))))
                 '("./gerbil/gxc"
                   "./lang/build.ss"
                   "./misc/http-perf/build.ss"
                   "./misc/rpc-perf/build.ss"
                   "./misc/scripts/docsnarf.ss"
                   "./misc/scripts/docstub.ss"
                   "./misc/scripts/docsyms.ss"
                   "./r7rs-large/build.ss"
                   "./release.ss"
                   "./std/build.ss"
                   "./std/run-tests.ss"
                   "./std/web/fastcgi-test.ss"
                   "./std/web/rack-test.ss"
                   "./tools/build.ss"
                   "./tutorial/httpd/build.ss"
                   "./tutorial/kvstore/build.ss"
                   "./tutorial/lang/build.ss"
                   "./tutorial/proxy/build-static.ss"
                   "./tutorial/proxy/build.ss")))
             #t))
         (replace
          'build
          (lambda*
           (#:key inputs #:allow-other-keys)
           (setenv "HOME" (getcwd))
             (invoke
              ;; The build script needs a tty or it'll crash on an ioctl
              ;; trying to find the width of the terminal it's running on.
              ;; Calling in script prevents that.
              "script"
              "-qefc"
              "./build.sh")))
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lib (string-append out "/lib")))
               (mkdir-p bin)
               (mkdir-p lib)
               (copy-recursively "../bin" bin)
               (copy-recursively "../lib" lib)))))))
    (native-inputs
     (list coreutils util-linux))
    (propagated-inputs
     (list gambit-c zlib openssl sqlite))
    (build-system gnu-build-system)
    (synopsis "Meta-dialect of Scheme with post-modern features")
    (description "Gerbil is an opinionated dialect of Scheme designed for Systems
Programming, with a state of the art macro and module system on top of the Gambit
runtime.  The macro system is based on quote-syntax, and provides the full meta-syntactic
tower with a native implementation of syntax-case.  It also provides a full-blown module
system, similar to PLT Scheme's (sorry, Racket) modules.  The main difference from Racket
is that Gerbil modules are single instantiation, supporting high performance ahead of
time compilation and compiled macros.")
    (home-page "https://cons.io")
    (license `(,lgpl2.1 ,asl2.0))))
