;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages which)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages libphidget)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages image)
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
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                                ; no "check" target
       #:phases
       (alist-replace
        'unpack
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
                 #t)))
        (alist-replace
         'build
         (lambda* (#:key system outputs #:allow-other-keys)
           (let ((out (assoc-ref outputs "out")))
             (if (or (string-prefix? "x86_64" system)
                     (string-prefix? "i686" system))
                 (zero? (system* "make" "compile-microcode"))
                 (zero? (system* "./etc/make-liarc.sh"
                                 (string-append "--prefix=" out))))))
         %standard-phases))))
    (inputs
     `(;; TODO: Build doc when TeX Live is available.
       ;; ("automake" ,automake)
       ;; ("texlive-core" ,texlive-core)
       ("texinfo" ,texinfo)
       ("m4" ,m4)

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
    (supported-systems (delete "mips64el-linux" %supported-systems))

    (home-page "http://www.gnu.org/software/mit-scheme/")
    (synopsis "A Scheme implementation with integrated editor and debugger")
    (description
     "GNU/MIT Scheme is an implementation of the Scheme programming
language.  It provides an interpreter, a compiler and a debugger.  It also
features an integrated Emacs-like editor and a large runtime library.")
    (license gpl2+)))

(define-public bigloo
  (package
    (name "bigloo")
    (version "4.1a")
    (source (origin
             (method url-fetch)
             (uri (string-append "ftp://ftp-sop.inria.fr/indes/fp/Bigloo/bigloo"
                                 version ".tar.gz"))
             (sha256
              (base32
               "170q7nh08n4v20xl81fxb0xcdxphqqacfa643hsa8i2ar6pki04c"))
             (patches (list (search-patch "bigloo-gc-shebangs.patch")))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:phases (alist-replace
                 'configure
                 (lambda* (#:key outputs #:allow-other-keys)

                   (substitute* "configure"
                     (("^shell=.*$")
                      (string-append "shell=" (which "bash") "\n")))

                   ;; Since libgc's pthread redirects are used, we end up
                   ;; using libgc symbols, so we must link against it.
                   ;; Reported on 2013-06-25.
                   (substitute* "api/pthread/src/Makefile"
                     (("^EXTRALIBS[[:blank:]]*=(.*)$" _ value)
                      (string-append "EXTRALIBS = "
                                     (string-trim-right value)
                                     " -l$(GCLIB)_fth-$(RELEASE)"
                                     " -Wl,-rpath=" (assoc-ref outputs "out")
                                     "/lib/bigloo/" ,version)))

                   ;; Those variables are used by libgc's `configure'.
                   (setenv "SHELL" (which "sh"))
                   (setenv "CONFIG_SHELL" (which "sh"))

                   ;; ... but they turned out to be overridden later, so work
                   ;; around that.
                   (substitute* (find-files "gc" "^configure-gc")
                     (("sh=/bin/sh")
                      (string-append "sh=" (which "sh"))))

                   ;; The `configure' script doesn't understand options
                   ;; of those of Autoconf.
                   (let ((out (assoc-ref outputs "out")))
                     (zero?
                      (system* "./configure"
                               (string-append "--prefix=" out)
                               ;; FIXME: Currently fails, see
                               ;; <http://article.gmane.org/gmane.lisp.scheme.bigloo/6126>.
                               ;; "--customgc=no" ; use our libgc
                               (string-append"--mv=" (which "mv"))
                               (string-append "--rm=" (which "rm"))))))
                 (alist-cons-after
                  'install 'install-emacs-modes
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let* ((out (assoc-ref outputs "out"))
                           (dir (string-append out "/share/emacs/site-lisp")))
                      (zero? (system* "make" "-C" "bmacs" "all" "install"
                                      (string-append "EMACSBRAND=emacs24")
                                      (string-append "EMACSDIR=" dir)))))
                  %standard-phases))))
    (inputs
     `(("emacs" ,emacs)

       ;; Optional APIs for which Bigloo has bindings.
       ("avahi" ,avahi)
       ("libphidget" ,libphidget)))
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
    (version "2.4.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "ftp://ftp-sop.inria.fr/indes/fp/Hop/hop-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "1v2r4ga58kk1sx0frn8qa8ccmjpic9csqzpk499wc95y9c4b1wy3"))
             (patches (list (search-patch "hop-bigloo-4.0b.patch")))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (alist-replace
        'configure
        (lambda* (#:key inputs outputs #:allow-other-keys)
          (let ((out (assoc-ref outputs "out")))
            (zero?
             (system* "./configure"
                      (string-append "--prefix=" out)))))
        (alist-cons-after
         'strip 'patch-rpath
         (lambda* (#:key outputs #:allow-other-keys)
           ;; Patch the RPATH of every installed library to point to $out/lib
           ;; instead of $TMPDIR.  Note that "patchelf --set-rpath" produces
           ;; invalid binaries when used before stripping.
           (let ((out    (assoc-ref outputs "out"))
                 (tmpdir (getcwd)))
             (every (lambda (lib)
                      (let* ((in    (open-pipe* OPEN_READ "patchelf"
                                                "--print-rpath" lib))
                             (rpath (read-line in)))
                        (and (zero? (close-pipe in))
                             (let ((rpath* (regexp-substitute/global
                                            #f (regexp-quote tmpdir) rpath
                                            'pre out 'post)))
                               (or (equal? rpath rpath*)
                                   (begin
                                     (format #t "~a: changing RPATH from `~a' to `~a'~%"
                                             lib rpath rpath*)
                                     (zero?
                                      (system* "patchelf" "--set-rpath"
                                               rpath* lib))))))))
                    (append (find-files (string-append out "/bin")
                                        ".*")
                            (find-files (string-append out "/lib")
                                        "\\.so$")))))
         %standard-phases))
       #:tests? #f                                ; no test suite
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 popen)
                  (ice-9 regex)
                  (ice-9 rdelim)
                  (srfi srfi-1))))
    (inputs `(("bigloo" ,bigloo)
              ("which" ,which)
              ("patchelf" ,patchelf)))
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
    (version "4.9.0.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://code.call-cc.org/releases/4.9.0/chicken-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0598mar1qswfd8hva9nqs88zjn02lzkqd8fzdd21dz1nki1prpq4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1))

       ;; No `configure' script; run "make check" after "make install" as
       ;; prescribed by README.
       #:phases (alist-cons-after
                 'install 'check
                 (assoc-ref %standard-phases 'check)
                 (fold alist-delete %standard-phases
                       '(configure check)))

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
    (version "1.9")
    (source (origin
             (method url-fetch)
             (uri (string-append "http://s48.org/" version
                                 "/scheme48-" version ".tgz"))
             (sha256
              (base32
               "0rw2lz5xgld0klvld292ds6hvfk5l12vskzgf1hhwjdpa38r3fnw"))
             (patches (list (search-patch "scheme48-tests.patch")))))
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
    (version "5.3.4")
    (source (origin
             (method url-fetch)
             (uri (list (string-append "http://download.racket-lang.org/installers/"
                                       version "/racket/racket-" version
                                       "-src-unix.tgz")
                        (string-append
                         "http://mirror.informatik.uni-tuebingen.de/mirror/racket/"
                         version "/racket/racket-" version "-src-unix.tgz")))
             (sha256
              ;; XXX: Used to be 1xhnx3yd74zrvn6sfcqmk57kxj51cwvm660dwiaxr1qxnm5lq0v7.
              (base32 "0yrdmpdvzf092869y6zjjjxl6j2kypgiv7qrfkv7lj8w01pbh7sd"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (let* ((gui-libs
               (lambda (inputs)
                 (define (lib input)
                   (string-append (assoc-ref inputs input) "/lib"))

                 (list (lib "glib")
                       (lib "cairo")
                       (lib "pango")
                       (lib "libjpeg")
                       (lib "gtk")
                       (lib "gdk-pixbuf")))))
         (alist-cons-before
          'configure 'pre-configure
          (lambda* (#:key inputs #:allow-other-keys)
            (chdir "src")

            ;; The GUI libs are dynamically opened through the FFI, so they
            ;; must be in the loader's search path.
            (setenv "LD_LIBRARY_PATH" (string-join (gui-libs inputs) ":")))
          (alist-cons-after
           'unpack 'patch-/bin/sh
           (lambda _
             (substitute* "collects/racket/system.rkt"
               (("/bin/sh") (which "sh"))))
           (alist-cons-after
            'install 'wrap-programs
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (define (wrap prog)
                  (wrap-program prog
                                `("LD_LIBRARY_PATH" ":" prefix
                                  ,(gui-libs inputs))))

                (with-directory-excursion (string-append out "/bin")
                  (for-each wrap
                            (list "gracket" "drracket" "slideshow" "mred"))
                  #t)))
            %standard-phases))))
       #:tests? #f                                ; XXX: how to run them?
       ))
    (inputs `(("libffi" ,libffi)
              ("glib" ,glib)                      ; for DrRacket
              ("cairo" ,cairo)
              ("pango" ,pango)
              ("libjpeg" ,libjpeg-8)
              ("gdk-pixbuf" ,gdk-pixbuf)
              ("gtk" ,gtk+-2)))
    (home-page "http://racket-lang.org")
    (synopsis "Implementation of Scheme and related languages")
    (description
     "Racket is an implementation of the Scheme programming language (R5RS and
R6RS) and related languages, such as Typed Racket.  It features a compiler and
a virtual machine with just-in-time native compilation, as well as a large set
of libraries.")
    (license lgpl2.0+)

    ;; Fails to build on MIPS with "address or size is not OS PAGE ALIGNED".
    ;; See <http://hydra.gnu.org/build/121775>.
    (supported-systems (delete "mips64el-linux" %supported-systems))))
