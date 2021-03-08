;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014, 2016, 2018 David Thompson <davet@gnu.org>
;;; Copyright © 2014, 2017, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2017 Christopher Allan Webber <cwebber@dustycloud.org>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Andy Wingo <wingo@igalia.com>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017, 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017, 2018 Amirouche <amirouche@hypermove.net>
;;; Copyright © 2018 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2018 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2019 Taylan Kammer <taylan.kammer@gmail.com>
;;; Copyright © 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages guile)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages hurd)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libunistring)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages version-control)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system guile)
  #:use-module (guix deprecation)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:prefix srfi-1:))

;;; Commentary:
;;;
;;; GNU Guile, and modules and extensions.
;;;
;;; Code:

(define-public guile-1.8
  (package
   (name "guile")
   (version "1.8.8")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/guile/guile-" version
                                ".tar.gz"))
            (sha256
             (base32
              "0l200a0v7h8bh0cwz6v7hc13ds39cgqsmfrks55b1rbj5vniyiy3"))
            (patches (search-patches "guile-1.8-cpp-4.5.patch"))))
   (build-system gnu-build-system)
   (arguments '(#:configure-flags '("--disable-error-on-warning")

                ;; Insert a phase before `configure' to patch things up.
                #:phases
                (modify-phases %standard-phases
                  (add-before 'configure 'patch-stuff
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; Add a call to `lt_dladdsearchdir' so that
                      ;; `libguile-readline.so' & co. are in the
                      ;; loader's search path.
                      (substitute* "libguile/dynl.c"
                        (("lt_dlinit.*$" match)
                         (format #f
                                 "  ~a~%  lt_dladdsearchdir(\"~a/lib\");~%"
                                 match
                                 (assoc-ref outputs "out"))))

                      ;; The usual /bin/sh...
                      (substitute* "ice-9/popen.scm"
                        (("/bin/sh") (which "sh")))
                      #t)))))

   ;; When cross-compiling, a native version of Guile itself is needed.
   (native-inputs (if (%current-target-system)
                      `(("self" ,this-package))
                      '()))

   (inputs `(("gawk" ,gawk)
             ("readline" ,readline)))

   ;; Since `guile-1.8.pc' has "Libs: ... -lgmp -lltdl", these must be
   ;; propagated.
   (propagated-inputs `(("gmp" ,gmp)
                        ("libltdl" ,libltdl)))

   (native-search-paths
    (list (search-path-specification
           (variable "GUILE_LOAD_PATH")
           (files '("share/guile/site")))))

   (synopsis "Scheme implementation intended especially for extensions")
   (description
    "Guile is the GNU Ubiquitous Intelligent Language for Extensions, the
official extension language of the GNU system.  It is an implementation of
the Scheme language which can be easily embedded in other applications to
provide a convenient means of extending the functionality of the application
without requiring the source code to be rewritten.")
   (home-page "https://www.gnu.org/software/guile/")
   (license license:lgpl2.0+)))

(define-public guile-2.0
  (package
   (name "guile")
   (version "2.0.14")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/guile/guile-" version
                                ".tar.xz"))
            (sha256
             (base32
              "10lxc6l5alf3lzbs3ihnbfy6dfcrsyf8667wa57f26vf4mk2ai78"))))
   (build-system gnu-build-system)

   ;; When cross-compiling, a native version of Guile itself is needed.
   (native-inputs `(,@(if (%current-target-system)
                          `(("self" ,this-package))
                          '())
                    ("pkgconfig" ,pkg-config)))
   (inputs `(("libffi" ,libffi)
             ,@(libiconv-if-needed)

             ;; We need Bash when cross-compiling because some of the scripts
             ;; in bin/ refer to it.  Use 'bash-minimal' because we don't need
             ;; an interactive Bash with Readline and all.
             ,@(if (target-mingw?) '() `(("bash" ,bash-minimal)))))
   (propagated-inputs
    `( ;; These ones aren't normally needed here, but since `libguile-2.0.la'
       ;; reads `-lltdl -lunistring', adding them here will add the needed
       ;; `-L' flags.  As for why the `.la' file lacks the `-L' flags, see
       ;; <http://thread.gmane.org/gmane.comp.lib.gnulib.bugs/18903>.
      ("libunistring" ,libunistring)

      ;; Depend on LIBLTDL, not LIBTOOL.  That way, we avoid some the extra
      ;; dependencies that LIBTOOL has, which is helpful during bootstrap.
      ("libltdl" ,libltdl)

      ;; The headers and/or `guile-2.0.pc' refer to these packages, so they
      ;; must be propagated.
      ("bdw-gc" ,libgc)
      ("gmp" ,gmp)))

   (outputs '("out" "debug"))

   (arguments
    `(#:configure-flags '("--disable-static") ; saves 3 MiB

      ;; Work around non-reproducible .go files as described in
      ;; <https://bugs.gnu.org/20272>, which affects 2.0, 2.2, and 3.0 so far.
      #:parallel-build? #f

      #:phases
      (modify-phases %standard-phases
        ,@(if (hurd-system?)
              '((add-after 'unpack 'disable-tests
                  (lambda _
                    ;; Hangs at: "Running 00-repl-server.test"
                    (rename-file "test-suite/tests/00-repl-server.test" "00-repl-server.test")
                    ;; Sometimes Hangs at: "Running 00-socket.test"
                    (rename-file "test-suite/tests/00-socket.test" "00-socket.test")
                    ;; FAIL: srfi-18.test: thread-sleep!: thread sleeps fractions of a second
                    (rename-file "test-suite/tests/srfi-18.test" "srfi-18.test")
                    ;; failed to remove 't-guild-compile-7215.go.tdL7yC
                    (substitute* "test-suite/standalone/Makefile.in"
                      (("test-guild-compile ") ""))
                    #t)))
              '())
        (add-before 'configure 'pre-configure
          (lambda* (#:key inputs #:allow-other-keys)
            ;; Tell (ice-9 popen) the file name of Bash.
            (let ((bash (assoc-ref inputs "bash")))
              (substitute* "module/ice-9/popen.scm"
                ;; If bash is #f allow fallback for user to provide
                ;; "bash" in PATH.  This happens when cross-building to
                ;; MinGW for which we do not have Bash yet.
                (("/bin/sh")
                 ,@(if (target-mingw?)
                       '((if bash
                             (string-append bash "/bin/bash")
                             "bash"))
                       '((string-append bash "/bin/bash")))))
              #t))))))

   (native-search-paths
    (list (search-path-specification
           (variable "GUILE_LOAD_PATH")
           (files '("share/guile/site/2.0")))
          (search-path-specification
           (variable "GUILE_LOAD_COMPILED_PATH")
           (files '("lib/guile/2.0/site-ccache")))))

   (synopsis "Scheme implementation intended especially for extensions")
   (description
    "Guile is the GNU Ubiquitous Intelligent Language for Extensions, the
official extension language of the GNU system.  It is an implementation of
the Scheme language which can be easily embedded in other applications to
provide a convenient means of extending the functionality of the application
without requiring the source code to be rewritten.")
   (home-page "https://www.gnu.org/software/guile/")
   (license license:lgpl3+)))

(define-public guile-2.2
  (package (inherit guile-2.0)
    (name "guile")
    (version "2.2.7")
    (source (origin
              (method url-fetch)

              ;; Note: we are limited to one of the compression formats
              ;; supported by the bootstrap binaries, so no lzip here.
              (uri (string-append "mirror://gnu/guile/guile-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "013mydzhfswqci6xmyc1ajzd59pfbdak15i0b090nhr9bzm7dxyd"))
              (modules '((guix build utils)))
              (patches (search-patches
                        "guile-2.2-skip-oom-test.patch"
                        "guile-2.2-skip-so-test.patch"))

              ;; Remove the pre-built object files.  Instead, build everything
              ;; from source, at the expense of significantly longer build
              ;; times (almost 3 hours on a 4-core Intel i5).
              (snippet '(begin
                          (for-each delete-file
                                    (find-files "prebuilt" "\\.go$"))
                          #t))))
    (properties '((timeout . 72000)               ;20 hours
                  (max-silent-time . 36000)))     ;10 hours (needed on ARM
                                                  ;  when heavily loaded)
    (native-search-paths
     (list (search-path-specification
            (variable "GUILE_LOAD_PATH")
            (files '("share/guile/site/2.2")))
           (search-path-specification
            (variable "GUILE_LOAD_COMPILED_PATH")
            (files '("lib/guile/2.2/site-ccache")))))))

(define-deprecated guile-2.2/bug-fix guile-2.2)

(define-public guile-2.2.4
  (package/inherit
   guile-2.2
   (version "2.2.4")
   (source (origin
             (inherit (package-source guile-2.2))
             (uri (string-append "mirror://gnu/guile/guile-" version
                                 ".tar.xz"))
             (sha256
              (base32
               "07p3g0v2ba2vlfbfidqzlgbhnzdx46wh2rgc5gszq1mjyx5bks6r"))))))

(define-public guile-3.0
  ;; This is the latest Guile stable version.
  (package
    (inherit guile-2.2)
    (name "guile")
    (version "3.0.7")
    (source (origin
              (inherit (package-source guile-2.2))
              (patches '())     ; We no longer need the patches.
              (uri (string-append "mirror://gnu/guile/guile-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1dwiwsrpm4f96alfnz6wibq378242z4f16vsxgy1n9r00v3qczgm"))
              ;; Replace the snippet because the oom-test still
              ;; fails on some 32-bit architectures.
              (snippet '(begin
                          (substitute* "test-suite/standalone/Makefile.in"
                            (("test-out-of-memory") ""))
                          (for-each delete-file
                                    (find-files "prebuilt" "\\.go$"))
                          #t))))

    ;; Build with the bundled mini-GMP to avoid interference with GnuTLS' own
    ;; use of GMP via Nettle: <https://issues.guix.gnu.org/46330>.
    (propagated-inputs
     (srfi-1:fold srfi-1:alist-delete
                  (package-propagated-inputs guile-2.2)
                  '("gmp" "libltdl")))
    (arguments
     (substitute-keyword-arguments (package-arguments guile-2.2)
       ((#:configure-flags flags ''())
        (let ((flags `(cons "--enable-mini-gmp" ,flags)))
          ;; XXX: JIT-enabled Guile crashes in obscure ways on GNU/Hurd.
          (if (hurd-target?)
              `(cons "--disable-jit" ,flags)
              flags)))
       ((#:phases phases)
         (if (string-prefix? "powerpc-" (%current-system))
           `(modify-phases ,phases
              (add-after 'unpack 'adjust-bootstrap-flags
                (lambda _
                  ;; Upstream knows about suggested solution.
                  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=45214
                  (substitute* "bootstrap/Makefile.in"
                    (("^GUILE_OPTIMIZATIONS.*")
                     "GUILE_OPTIMIZATIONS = -O1 -Oresolve-primitives -Ocps\n"))))
              (add-after 'unpack 'skip-failing-fdes-test
                (lambda _
                  ;; ERROR: ((system-error "seek" "~A" ("Bad file descriptor") (9)))
                  (substitute* "test-suite/tests/ports.test"
                    (("fdes not closed\"" all) (string-append all "(exit 77)")))
                  #t)))
           phases))))

    (native-search-paths
     (list (search-path-specification
            (variable "GUILE_LOAD_PATH")
            (files '("share/guile/site/3.0")))
           (search-path-specification
            (variable "GUILE_LOAD_COMPILED_PATH")
            (files '("lib/guile/3.0/site-ccache"
                     "share/guile/site/3.0")))))))

(define-public guile-3.0-latest
  guile-3.0)

(define-public guile-3.0/libgc-7
  ;; Using libgc-7 avoid crashes that can occur, particularly when loading
  ;; data in to the Guix Data Service:
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=40525
  (hidden-package
   (package
     (inherit guile-3.0-latest)
     (propagated-inputs
      `(("bdw-gc" ,libgc-7)
        ,@(srfi-1:alist-delete "bdw-gc" (package-propagated-inputs guile-3.0)))))))

(define-public guile-3.0/fixed
  ;; A package of Guile that's rarely changed.  It is the one used in the
  ;; `base' module, and thus changing it entails a full rebuild.
  (package
    (inherit guile-3.0)
    (properties '((hidden? . #t)            ;people should install 'guile-2.2'
                  (timeout . 72000)             ;20 hours
                  (max-silent-time . 36000))))) ;10 hours (needed on ARM
                                                ;  when heavily loaded)

(define-public guile-next
  (let ((version "3.0.5")
        (revision "0")
        (commit "91547abf54d5e0795afda2781259ab8923eb527b"))
    (package
      (inherit guile-3.0)
      (name "guile-next")
      (version (git-version version revision commit))
      (source (origin
                ;; The main goal here is to allow for '--with-branch'.
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.gnu.org/git/guile.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "09i1c77h2shygylfk0av31jsc1my6zjl230b2cx6vyl58q8c0cqy"))))
      (arguments
       (substitute-keyword-arguments (package-arguments guile-3.0)
         ((#:phases phases '%standard-phases)
          `(modify-phases ,phases
             (add-before 'check 'skip-failing-tests
               (lambda _
                 (substitute* "test-suite/standalone/test-out-of-memory"
                   (("!#") "!#\n\n(exit 77)\n"))
                 (delete-file "test-suite/tests/version.test")
                 #t))))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("libtool" ,libtool)
         ("flex" ,flex)
         ("gettext" ,gnu-gettext)
         ("texinfo" ,texinfo)
         ("gperf" ,gperf)
         ,@(package-native-inputs guile-3.0)))
      (synopsis "Development version of GNU Guile"))))

(define* (make-guile-readline guile #:optional (name "guile-readline"))
  (package
    (name name)
    (version (package-version guile))
    (source (package-source guile))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-silent-rules"
                           "--enable-mini-gmp")   ;for Guile >= 3.0.6
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'chdir
                    (lambda* (#:key outputs #:allow-other-keys)
                      (invoke "make" "-C" "libguile" "scmconfig.h")
                      (invoke "make" "-C" "lib")
                      (chdir "guile-readline")

                      (substitute* "Makefile"
                        (("../libguile/libguile-[[:graph:]]+\\.la")
                         ;; Remove dependency on libguile-X.Y.la.
                         "")
                        (("^READLINE_LIBS = (.*)$" _ libs)
                         ;; Link against the provided libguile.
                         (string-append "READLINE_LIBS = "
                                        "-lguile-$(GUILE_EFFECTIVE_VERSION) "
                                        libs "\n"))
                        (("\\$\\(top_builddir\\)/meta/build-env")
                         ;; Use the provided Guile, not the one from
                         ;; $(builddir).
                         "")

                        ;; Install modules to the 'site' directories.
                        (("^moddir = .*$")
                         "moddir = $(pkgdatadir)/site/$(GUILE_EFFECTIVE_VERSION)\n")
                        (("^ccachedir = .*$")
                         "ccachedir = $(pkglibdir)/$(GUILE_EFFECTIVE_VERSION)/site-ccache\n"))

                      ;; Load 'guile-readline.so' from the right place.
                      (substitute* "ice-9/readline.scm"
                        (("load-extension \"guile-readline\"")
                         (format #f "load-extension \
 (string-append ~s \"/lib/guile/\" (effective-version) \"/extensions/guile-readline\")"
                                 (assoc-ref outputs "out"))))
                      #t)))))
    (home-page (package-home-page guile))
    (native-inputs (package-native-inputs guile))
    (inputs
     `(,@(package-inputs guile)                   ;to placate 'configure'
       ,@(package-propagated-inputs guile)
       ("guile" ,guile)
       ("readline" ,readline)))
    (synopsis "Line editing support for GNU Guile")
    (description
     "This module provides line editing support via the Readline library for
GNU@tie{}Guile.  Use the @code{(ice-9 readline)} module and call its
@code{activate-readline} procedure to enable it.")
    (license license:gpl3+)))

(define-public guile-readline
  (make-guile-readline guile-3.0))

(define-public guile2.2-readline
  (make-guile-readline guile-2.2 "guile2.2-readline"))

(define (guile-variant-package-name prefix)
  (lambda (name)
    "Return NAME with PREFIX instead of \"guile-\", when applicable."
    (if (string-prefix? "guile-" name)
        (string-append prefix "-"
                       (string-drop name
                                    (string-length "guile-")))
        name)))

(define package-for-guile-2.0
  ;; A procedure that rewrites the dependency tree of the given package to use
  ;; GUILE-2.0 instead of GUILE-3.0.
  (package-input-rewriting `((,guile-3.0 . ,guile-2.0))
                           (guile-variant-package-name "guile2.0")
                           #:deep? #f))

(define package-for-guile-2.2
  (package-input-rewriting `((,guile-3.0 . ,guile-2.2))
                           (guile-variant-package-name "guile2.2")
                           #:deep? #f))

(define-syntax define-deprecated-guile3.0-package
  (lambda (s)
    "Define a deprecated package alias for \"guile3.0-something\"."
    (syntax-case s ()
      ((_ name)
       (and (identifier? #'name)
            (string-prefix? "guile3.0-" (symbol->string (syntax->datum
                                                         #'name))))
       (let ((->guile (lambda (str)
                        (let ((base (string-drop str
                                                 (string-length "guile3.0-"))))
                          (string-append "guile-" base)))))
         (with-syntax ((package-name (symbol->string (syntax->datum #'name)))
                       (package
                         (datum->syntax
                          #'name
                          (string->symbol
                           (->guile (symbol->string (syntax->datum
                                                     #'name))))))
                       (old-name
                        ;; XXX: This is the name generated by
                        ;; 'define-deprecated'.
                        (datum->syntax
                         #'name
                         (symbol-append '% (syntax->datum #'name)
                                        '/deprecated))))
           #'(begin
               (define-deprecated name package
                 (deprecated-package package-name package))
               (export old-name))))))))

(define-deprecated-guile3.0-package guile3.0-readline)

(define-public guile-for-guile-emacs
  (let ((commit "15ca78482ac0dd2e3eb36dcb31765d8652d7106d")
        (revision "1"))
    (package (inherit guile-2.2)
      (name "guile-for-guile-emacs")
      (version (git-version "2.1.2" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "git://git.savannah.gnu.org/guile.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1l7ik4q4zk7vq4m3gnwizc0b64b1mdr31hxqlzxs94xaf2lvi7s2"))))
      (arguments
       (substitute-keyword-arguments (package-arguments guile-2.2)
         ((#:phases phases '%standard-phases)
          `(modify-phases ,phases
             (replace 'bootstrap
               (lambda _
                 ;; Disable broken tests.
                 ;; TODO: Fix them!
                 (substitute* "test-suite/tests/gc.test"
                   (("\\(pass-if \"after-gc-hook gets called\"" m)
                    (string-append "#;" m)))
                 (substitute* "test-suite/tests/version.test"
                   (("\\(pass-if \"version reporting works\"" m)
                    (string-append "#;" m)))
                 ;; Warning: Unwind-only `out-of-memory' exception; skipping pre-unwind handler.
                 ;; FAIL: test-out-of-memory
                 (substitute* "test-suite/standalone/Makefile.am"
                   (("(check_SCRIPTS|TESTS) \\+= test-out-of-memory") ""))
                 
                 (patch-shebang "build-aux/git-version-gen")
                 (invoke "sh" "autogen.sh")
                 #t))))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("libtool" ,libtool)
         ("flex" ,flex)
         ("texinfo" ,texinfo)
         ("gettext" ,gettext-minimal)
         ,@(package-native-inputs guile-2.2))))))


;;;
;;; Extensions.
;;;

(define-public guile-json-1
  (package
    (name "guile-json")
    (version "1.3.2")
    (home-page "https://github.com/aconchillo/guile-json")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/guile-json/guile-json-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0m6yzb169r6iz56k3nkncjaiijwi4p0x9ijn1p5ax3s77jklxy9k"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags '("GUILE_AUTO_COMPILE=0")))   ;to prevent guild warnings
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("guile" ,guile-2.2)))
    (inputs `(("guile" ,guile-2.2)))
    (synopsis "JSON module for Guile")
    (description
     "Guile-JSON supports parsing and building JSON documents according to the
specification.  These are the main features:

@itemize
@item Strictly complies to @uref{http://json.org, specification}.
@item Build JSON documents programmatically via macros.
@item Unicode support for strings.
@item Allows JSON pretty printing.
@end itemize\n")

    ;; Version 1.2.0 switched to GPLv3+ (from LGPLv3+).
    (license license:gpl3+)))

;; Deprecate the 'guile-json' alias to force the use 'guile-json-1' or
;; 'guile-json-3'.  In the future, we may reuse 'guile-json' as an alias for
;; 'guile-json-3'.
(define-deprecated guile-json guile-json-1)
(export guile-json)

(define-public guile2.0-json
  (package-for-guile-2.0 guile-json-1))

(define-public guile-json-3
  ;; This version is incompatible with 1.x; see the 'NEWS' file.
  (package
    (inherit guile-json-1)
    (name "guile-json")
    (version "3.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/guile-json/guile-json-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0nj0684qgh6ppkbdyxqfyjwsv2qbyairxpi8fzrhsi3xnc7jn4im"))))
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("guile" ,guile-3.0)))
    (inputs `(("guile" ,guile-3.0)))))

(define-public guile3.0-json
  (deprecated-package "guile3.0-json" guile-json-3))

(define-public guile-json-4
  (package
    (inherit guile-json-3)
    (name "guile-json")
    (version "4.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/guile-json/guile-json-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0cqr0ljqmzlc2bwrapcsmcgxg147h66mcxf23824ri5i6vn4dc0s"))))))

(define-public guile2.2-json
  (package-for-guile-2.2 guile-json-4))

;; There are two guile-gdbm packages, one using the FFI and one with
;; direct C bindings, hence the verbose name.

(define-public guile-gdbm-ffi
  (package
    (name "guile-gdbm-ffi")
    (version "20120209.fa1d5b6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ijp/guile-gdbm")
                    (commit "fa1d5b6231d0e4d096687b378c025f2148c5f246")))
              (file-name (string-append name "-" version "-checkout"))
              (patches (search-patches
                        "guile-gdbm-ffi-support-gdbm-1.14.patch"))
              (sha256
               (base32
                "1j8wrsw7v9w6qkl47xz0rdikg50v16nn6kbs3lgzcymjzpa7babj"))))
    (build-system guile-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'move-examples
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; Move examples where they belong.
                      (let* ((out (assoc-ref outputs "out"))
                             (doc (string-append out "/share/doc/"
                                                 (strip-store-file-name out)
                                                 "/examples")))
                        (copy-recursively "examples" doc)
                        (delete-file-recursively "examples")
                        #t)))
                  (add-after 'unpack 'set-libgdbm-file-name
                    (lambda* (#:key inputs #:allow-other-keys)
                      (substitute* "gdbm.scm"
                        (("\\(dynamic-link \"libgdbm\"\\)")
                         (format #f "(dynamic-link \"~a/lib/libgdbm.so\")"
                                 (assoc-ref inputs "gdbm"))))
                      #t)))))
    (native-inputs
     `(("guile" ,guile-3.0)))
    (inputs
     `(("gdbm" ,gdbm)))
    (home-page "https://github.com/ijp/guile-gdbm")
    (synopsis "Guile bindings to the GDBM library via Guile's FFI")
    (description
     "Guile bindings to the GDBM key-value storage system, using
Guile's foreign function interface.")
    (license license:gpl3+)))

(define-public guile2.0-gdbm-ffi
  (package-for-guile-2.0 guile-gdbm-ffi))

(define-public guile2.2-gdbm-ffi
  (package-for-guile-2.2 guile-gdbm-ffi))

(define-deprecated-guile3.0-package guile3.0-gdbm-ffi)

(define-public guile-sqlite3
  (package
    (name "guile-sqlite3")
    (version "0.1.2")
    (home-page "https://notabug.org/guile-sqlite3/guile-sqlite3.git")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "1nryy9j3bk34i0alkmc9bmqsm0ayz92k1cdf752mvhyjjn8nr928"))
              (file-name (string-append name "-" version "-checkout"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("guile" ,guile-3.0)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("guile" ,guile-3.0)
       ("sqlite" ,sqlite)))
    (synopsis "Access SQLite databases from Guile")
    (description
     "This package provides Guile bindings to the SQLite database system.")
    (license license:gpl3+)))

(define-public guile2.0-sqlite3
  (package-for-guile-2.0 guile-sqlite3))

(define-public guile2.2-sqlite3
  (package-for-guile-2.2 guile-sqlite3))

(define-deprecated-guile3.0-package guile3.0-sqlite3)

(define-public guile-bytestructures
  (package
    (name "guile-bytestructures")
    (version "1.0.10")
    (home-page "https://github.com/TaylanUB/scheme-bytestructures")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "14k50jln32kkxv41hvsdgjkkfj6xlv06vc1caz01qkgk1fzh72nk"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags '("GUILE_AUTO_COMPILE=0")     ;to prevent guild warnings

       #:phases (modify-phases %standard-phases
                  (add-after 'install 'install-doc
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (package ,(package-full-name this-package "-"))
                             (doc (string-append out "/share/doc/" package)))
                        (install-file "README.md" doc)
                        #t))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)
       ("guile" ,guile-3.0)))
    (inputs
     `(("guile" ,guile-3.0)))
    (synopsis "Structured access to bytevector contents for Guile")
    (description
     "Guile bytestructures offers a system imitating the type system
of the C programming language, to be used on bytevectors.  C's type
system works on raw memory, and Guile works on bytevectors which are
an abstraction over raw memory.  It's also more powerful than the C
type system, elevating types to first-class status.")
    (license license:gpl3+)
    (properties '((upstream-name . "bytestructures")))))

(define-public guile2.0-bytestructures
  (package-for-guile-2.0 guile-bytestructures))

(define-public guile2.2-bytestructures
  (package-for-guile-2.2 guile-bytestructures))

(define-deprecated-guile3.0-package guile3.0-bytestructures)

(define-public guile-git
  (package
    (name "guile-git")
    (version "0.5.1")
    (home-page "https://gitlab.com/guile-git/guile-git.git")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1x3wa6la4j1wcfxyhhjlmd7yp85wwpny0y6lrzpz803i9z5fwagc"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags '("GUILE_AUTO_COMPILE=0")))     ; to prevent guild warnings
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("texinfo" ,texinfo)
       ("guile" ,guile-3.0)
       ("guile-bytestructures" ,guile-bytestructures)))
    (inputs
     `(("guile" ,guile-3.0)
       ("libgit2" ,libgit2)))
    (propagated-inputs
     `(("guile-bytestructures" ,guile-bytestructures)))
    (synopsis "Guile bindings for libgit2")
    (description
     "This package provides Guile bindings to libgit2, a library to
manipulate repositories of the Git version control system.")
    (license license:gpl3+)))

(define-public guile2.2-git
  (package-for-guile-2.2 guile-git))

(define-public guile2.0-git
  (package-for-guile-2.0 guile-git))

(define-deprecated-guile3.0-package guile3.0-git)

(define-public guile-zlib
  (package
    (name "guile-zlib")
    (version "0.1.0")
    (source
     (origin
       ;; XXX: Do not use "git-fetch" method here that would create and
       ;; endless inclusion loop, because this package is used as an extension
       ;; in the same method.
       (method url-fetch)
       (uri
        (string-append "https://notabug.org/guile-zlib/guile-zlib/archive/v"
                       version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        ;; content hash: 1ip18nzwnczqyhn9cpzxkm9vzpi5fz5sy96cgjhmp7cwhnkmv6zv
        (base32
         "1safz7rrbdf1d98x3lgx5v74kivpyf9n1v6pdyy22vd0f2sjdir5"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags
       '("GUILE_AUTO_COMPILE=0"))) ;to prevent guild warnings
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)
       ,@(if (%current-target-system)
             `(("guile" ,guile-3.0))   ;for 'guild compile' and 'guile-3.0.pc'
             '())))
    (inputs
     `(("guile" ,guile-3.0)
       ("zlib" ,zlib)))
    (synopsis "Guile bindings to zlib")
    (description
     "This package provides Guile bindings for zlib, a lossless
data-compression library.  The bindings are written in pure Scheme by using
Guile's foreign function interface.")
    (home-page "https://notabug.org/guile-zlib/guile-zlib")
    (license license:gpl3+)))

(define-public guile2.2-zlib
  (package-for-guile-2.2 guile-zlib))

(define-public guile-lzlib
  (package
    (name "guile-lzlib")
    (version "0.0.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://notabug.org/guile-lzlib/guile-lzlib/archive/"
                       version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "11sggvncyx08ssp1s5xii4d6nskh1qwqihnbpzzvkrs7sivxn8w6"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags
       '("GUILE_AUTO_COMPILE=0"))) ;to prevent guild warnings
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)
       ,@(if (%current-target-system)
             `(("guile" ,guile-3.0))   ;for 'guild compile' and 'guile-3.0.pc'
             '())))
    (inputs
     `(("guile" ,guile-3.0)
       ("lzlib" ,lzlib)))
    (synopsis "Guile bindings to lzlib")
    (description
     "This package provides Guile bindings for lzlib, a C library for
in-memory LZMA compression and decompression.  The bindings are written in
pure Scheme by using Guile's foreign function interface.")
    (home-page "https://notabug.org/guile-lzlib/guile-lzlib")
    (license license:gpl3+)))

(define-public guile2.2-lzlib
  (package-for-guile-2.2 guile-lzlib))

(define-public guile-zstd
  (package
    (name "guile-zstd")
    (version "0.1.1")
    (home-page "https://notabug.org/guile-zstd/guile-zstd")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1c8l7829b5yx8wdc0mrhzjfwb6h9hb7cd8dfxcr71a7vlsi86310"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)
       ("guile" ,guile-3.0)))
    (inputs
     `(("zstd" ,zstd "lib")
       ("guile" ,guile-3.0)))
    (synopsis "GNU Guile bindings to the zstd compression library")
    (description
     "This package provides a GNU Guile interface to the zstd (``zstandard'')
compression library.")
    (license license:gpl3+)))

;;; guile.scm ends here
