;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014, 2016, 2018 David Thompson <davet@gnu.org>
;;; Copyright © 2014, 2017, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2017 Christopher Allan Webber <cwebber@dustycloud.org>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Andy Wingo <wingo@igalia.com>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017, 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017, 2018 Amirouche <amirouche@hypermove.net>
;;; Copyright © 2018 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2018 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2019 Taylan Kammer <taylan.kammer@gmail.com>
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
      #:phases
      (modify-phases %standard-phases
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
    (version "2.2.6")
    (source (origin
              (method url-fetch)

              ;; Note: we are limited to one of the compression formats
              ;; supported by the bootstrap binaries, so no lzip here.
              (uri (string-append "mirror://gnu/guile/guile-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "1269ymxm56j1z1lvq1y42rm961f2n7rinm3k6l00p9k52hrpcddk"))
              (modules '((guix build utils)))
              (patches (search-patches
                        "guile-2.2-skip-oom-test.patch"))

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

(define-public guile-2.2.7
  ;; This version contains a bug fix for a relatively rare crash that could
  ;; affect shepherd as PID 1: <https://bugs.gnu.org/37757>.
  (package
    (inherit guile-2.2)
    (version "2.2.7")
    (source (origin
              (inherit (package-source guile-2.2))
              (uri (string-append "mirror://gnu/guile/guile-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "013mydzhfswqci6xmyc1ajzd59pfbdak15i0b090nhr9bzm7dxyd"))))))

(define-deprecated guile-2.2/bug-fix guile-2.2.7)

(define-public guile-2.2/fixed
  ;; A package of Guile 2.2 that's rarely changed.  It is the one used
  ;; in the `base' module, and thus changing it entails a full rebuild.
  (package
    (inherit guile-2.2)
    (properties '((hidden? . #t)            ;people should install 'guile-2.2'
                  (timeout . 72000)             ;20 hours
                  (max-silent-time . 36000))))) ;10 hours (needed on ARM
                                                ;  when heavily loaded)
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
    (name "guile-next")                           ;to be renamed to "guile"
    (version "3.0.0")
    (source (origin
              (inherit (package-source guile-2.2))
              (uri (string-append "mirror://gnu/guile/guile-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0x8ca6q1qdmk29lh12gj6ngvgn7kp79w42rxfgwrpxm9jmjqs4y9"))))
    (arguments
     (substitute-keyword-arguments (package-arguments guile-2.2)
       ;; XXX: On ARMv7, work around <https://bugs.gnu.org/39208> by disabling
       ;; JIT.
       ((#:configure-flags flags '())
        (if (target-arm32?)
            `(cons "--disable-jit" ,flags)
            flags))))
    (native-search-paths
     (list (search-path-specification
            (variable "GUILE_LOAD_PATH")
            (files '("share/guile/site/3.0")))
           (search-path-specification
            (variable "GUILE_LOAD_COMPILED_PATH")
            (files '("lib/guile/3.0/site-ccache"
                     "share/guile/site/3.0")))))))

(define-public guile-next guile-3.0)

(define* (make-guile-readline guile #:optional (name "guile-readline"))
  (package
    (name name)
    (version (package-version guile))
    (source (package-source guile))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-silent-rules")
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
  (make-guile-readline guile-2.2))

(define-public guile3.0-readline
  (make-guile-readline guile-next "guile3.0-readline"))

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
  ;; GUILE-2.0 instead of GUILE-2.2.
  (package-input-rewriting `((,guile-2.2 . ,guile-2.0))
                           (guile-variant-package-name "guile2.0")))

(define package-for-guile-3.0
  (package-input-rewriting `((,guile-2.2 . ,guile-next))
                           (guile-variant-package-name "guile3.0")))

(define-public guile-for-guile-emacs
  (package (inherit guile-2.2)
    (name "guile-for-guile-emacs")
    (version "20150510.d8d9a8d")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "git://git.hcoop.net/git/bpt/guile.git")
                    (commit "d8d9a8da05ec876acba81a559798eb5eeceb5a17")))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "00sprsshy16y8pxjy126hr2adqcvvzzz96hjyjwgg8swva1qh6b0"))))
    (arguments
     `(;; Tests aren't passing for now.
       ;; Obviously we should re-enable this!
       #:tests? #f
       ,@(package-arguments guile-2.2)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("flex" ,flex)
       ("texinfo" ,texinfo)
       ("gettext" ,gettext-minimal)
       ,@(package-native-inputs guile-2.2)))
    ;; Same as in guile-2.0
    (native-search-paths
     (list (search-path-specification
            (variable "GUILE_LOAD_PATH")
            (files '("share/guile/site/2.0")))
           (search-path-specification
            (variable "GUILE_LOAD_COMPILED_PATH")
            (files '("lib/guile/2.0/site-ccache"
                     "share/guile/site/2.0")))))))


;;;
;;; Extensions.
;;;

(define-public guile-json-1
  (package
    (name "guile-json")
    (version "1.2.0")
    (home-page "https://github.com/aconchillo/guile-json")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/guile-json/guile-json-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "15gnb84d7hpazqhskkf3g9z4r6knw54wfj4ch5270kakz1lp70c9"))))
    (build-system gnu-build-system)
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
    (version "3.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/guile-json/guile-json-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "14m6b6g2maw0mkvfm4x63rqb54vgbpn1gcqs715ijw4bikfzlqfz"))))))

(define-public guile3.0-json
  (package-for-guile-3.0 guile-json-3))

;; There are two guile-gdbm packages, one using the FFI and one with
;; direct C bindings, hence the verbose name.

(define-public guile-gdbm-ffi
  (package
    (name "guile-gdbm-ffi")
    (version "20120209.fa1d5b6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ijp/guile-gdbm.git")
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
     `(("guile" ,guile-2.2)))
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

(define-public guile3.0-gdbm-ffi
  (package-for-guile-3.0 guile-gdbm-ffi))

(define-public guile-sqlite3
  (package
    (name "guile-sqlite3")
    (version "0.1.0")
    (home-page "https://notabug.org/guile-sqlite3/guile-sqlite3.git")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "1nv8j7wk6b5n4p22szyi8lv8fs31rrzxhzz16gyj8r38c1fyp9qp"))
              (file-name (string-append name "-" version "-checkout"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Allow builds with Guile 3.0.
                  (substitute* "configure.ac"
                    (("^GUILE_PKG.*")
                     "GUILE_PKG([3.0 2.2 2.0])\n"))
                  #t))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("guile" ,guile-2.2)
       ("sqlite" ,sqlite)))
    (synopsis "Access SQLite databases from Guile")
    (description
     "This package provides Guile bindings to the SQLite database system.")
    (license license:gpl3+)))

(define-public guile2.0-sqlite3
  (package-for-guile-2.0 guile-sqlite3))

(define-public guile3.0-sqlite3
  (package-for-guile-3.0 guile-sqlite3))

(define-public guile-bytestructures
  (package
    (name "guile-bytestructures")
    (version "1.0.7")
    (home-page "https://github.com/TaylanUB/scheme-bytestructures")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0q0habjiy3h9cigb7q1br9kz6z212dn2ab31f6dgd3rrmsfn5rvb"))))
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
       ("guile" ,guile-2.2)))
    (inputs
     `(("guile" ,guile-2.2)))
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

(define-public guile3.0-bytestructures
  (package-for-guile-3.0 guile-bytestructures))

(define-public guile-git
  (package
    (name "guile-git")
    (version "0.3.0")
    (home-page "https://gitlab.com/guile-git/guile-git.git")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gitlab.com/guile-git/guile-git/uploads/"
                                  "4c563d8e7e1ff84396abe8ca7011bcaf/guile-git-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0c5i3d16hp7gp9rd78vk9zc45js8bphf92m4lbb5gyi4l1yl7kkm"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("guile" ,guile-2.2)
       ("guile-bytestructures" ,guile-bytestructures)))
    (inputs
     `(("guile" ,guile-2.2)
       ("libgit2" ,libgit2)))
    (propagated-inputs
     `(("guile-bytestructures" ,guile-bytestructures)))
    (synopsis "Guile bindings for libgit2")
    (description
     "This package provides Guile bindings to libgit2, a library to
manipulate repositories of the Git version control system.")
    (license license:gpl3+)))

(define-public guile3.0-git
  (package-for-guile-3.0 guile-git))

(define-public guile2.0-git
  (let ((base (package-for-guile-2.0 guile-git)))
    (package
      (inherit base)
      ;; Libgit2's Guile test driver requires (ice-9 textual-ports), which is
      ;; not in Guile 2.0.  Thus, keep LIBGIT2 as-is here (i.e., built against
      ;; Guile 2.2).
      (inputs `(("libgit2" ,libgit2)
                ,@(srfi-1:alist-delete "libgit2"
                                       (package-inputs base)))))))

;;; guile.scm ends here

