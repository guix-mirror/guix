;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Christopher Allan Webber <cwebber@dustycloud.org>
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
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages libunistring)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages ed)
  #:use-module (gnu packages base)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gdbm)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix utils)
  #:use-module (ice-9 match))

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
            (patches (list (search-patch "guile-1.8-cpp-4.5.patch")))))
   (build-system gnu-build-system)
   (arguments '(#:configure-flags '("--disable-error-on-warning")

                ;; Insert a phase before `configure' to patch things up.
                #:phases (alist-cons-before
                           'configure
                           'patch-stuff
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
                               (("/bin/sh") (which "sh"))))
                           %standard-phases)))
   (inputs `(("gawk" ,gawk)
             ("readline" ,readline)))

   ;; Since `guile-1.8.pc' has "Libs: ... -lgmp -lltdl", these must be
   ;; propagated.
   (propagated-inputs `(("gmp" ,gmp)
                        ("libltdl" ,libltdl)))

   ;; When cross-compiling, a native version of Guile itself is needed.
   (self-native-input? #t)

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
   (home-page "http://www.gnu.org/software/guile/")
   (license lgpl2.0+)))

(define-public guile-2.0
  (package
   (name "guile")
   (version "2.0.11")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/guile/guile-" version
                                ".tar.xz"))
            (sha256
             (base32
              "1qh3j7308qvsjgwf7h94yqgckpbgz2k3yqdkzsyhqcafvfka9l5f"))
            (patches (list (search-patch "guile-arm-fixes.patch")))))
   (build-system gnu-build-system)
   (native-inputs `(("pkgconfig" ,pkg-config)))
   (inputs `(("libffi" ,libffi)
             ("readline" ,readline)
             ("bash" ,bash)))

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

   (self-native-input? #t)

   (outputs '("out" "debug"))

   (arguments
    `(#:phases (alist-cons-before
                'configure 'pre-configure
                (lambda* (#:key inputs #:allow-other-keys)
                  ;; Tell (ice-9 popen) the file name of Bash.
                  (let ((bash (assoc-ref inputs "bash")))
                    (substitute* "module/ice-9/popen.scm"
                      (("/bin/sh")
                       (string-append bash "/bin/bash")))))
                %standard-phases)))

   (native-search-paths
    (list (search-path-specification
           (variable "GUILE_LOAD_PATH")
           (files '("share/guile/site/2.0")))
          (search-path-specification
           (variable "GUILE_LOAD_COMPILED_PATH")
           (files '("share/guile/site/2.0")))))

   (synopsis "Scheme implementation intended especially for extensions")
   (description
    "Guile is the GNU Ubiquitous Intelligent Language for Extensions, the
official extension language of the GNU system.  It is an implementation of
the Scheme language which can be easily embedded in other applications to
provide a convenient means of extending the functionality of the application
without requiring the source code to be rewritten.")
   (home-page "http://www.gnu.org/software/guile/")
   (license lgpl3+)))

(define-public guile-2.0/fixed
  ;; A package of Guile 2.0 that's rarely changed.  It is the one used
  ;; in the `base' module, and thus changing it entails a full rebuild.
  guile-2.0)

(define-public guile-for-guile-emacs
  (package (inherit guile-2.0)
    (name "guile-for-guile-emacs")
    (version "20150510.d8d9a8d")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "git://git.hcoop.net/git/bpt/guile.git")
                    (commit "d8d9a8da05ec876acba81a559798eb5eeceb5a17")))
              (sha256
               (base32
                "00sprsshy16y8pxjy126hr2adqcvvzzz96hjyjwgg8swva1qh6b0"))))
    (arguments
     (substitute-keyword-arguments `(;; Tests aren't passing for now.
                                     ;; Obviously we should re-enable this!
                                     #:tests? #f
                                     ,@(package-arguments guile-2.0))
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'autogen
                      (lambda _
                        (zero? (system* "sh" "autogen.sh"))))
           (add-before 'autogen 'patch-/bin/sh
                       (lambda _
                         (substitute* "build-aux/git-version-gen"
                           (("#!/bin/sh") (string-append "#!" (which "sh"))))
                         #t))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("flex" ,flex)
       ("texinfo" ,texinfo)
       ("gettext" ,gnu-gettext)
       ,@(package-native-inputs guile-2.0)))))


;;;
;;; Extensions.
;;;

(define-public guile-reader
  (package
    (name "guile-reader")
    (version "0.6.1")
    (source  (origin
               (method url-fetch)
               (uri (string-append "mirror://savannah/guile-reader/guile-reader-"
                                   version ".tar.gz"))
               (sha256
                (base32
                 "020wz5w8z6g79nbqifg2n496wxwkcjzh8xizpv6mz0hczpl155ma"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkgconfig" ,pkg-config)
                     ("gperf" ,gperf)))
    (inputs `(("guile" ,guile-2.0)))
    (arguments `(#:configure-flags
                 (let ((out (assoc-ref %outputs "out")))
                   (list (string-append "--with-guilemoduledir="
                                        out "/share/guile/site/2.0")))))
    (synopsis "Framework for building readers for GNU Guile")
    (description
     "Guile-Reader is a simple framework for building readers for GNU Guile.

The idea is to make it easy to build procedures that extend Guile’s read
procedure.  Readers supporting various syntax variants can easily be written,
possibly by re-using existing “token readers” of a standard Scheme
readers.  For example, it is used to implement Skribilo’s R5RS-derived
document syntax.

Guile-Reader’s approach is similar to Common Lisp’s “read table”, but
hopefully more powerful and flexible (for instance, one may instantiate as
many readers as needed).")
    (home-page "http://www.nongnu.org/guile-reader/")
    (license gpl3+)))

(define-public guile-ncurses
  (package
    (name "guile-ncurses")
    (version "1.6")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/guile-ncurses/guile-ncurses-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0wmk681zzi1wxw543r2s2r84ndnzxp69kr7pc01aw4l55hg7jn73"))))
    (build-system gnu-build-system)
    (inputs `(("ncurses" ,ncurses)
              ("guile" ,guile-2.0)))
    (arguments
     '(#:configure-flags (list "--with-ncursesw"  ; Unicode support
                               (string-append "--with-guilesitedir="
                                              (assoc-ref %outputs "out")
                                              "/share/guile/site/2.0"))
       #:phases (alist-cons-before
                 'check 'change-locale
                 (lambda _
                   ;; Use the locale that's actually available in the build
                   ;; environment.
                   (substitute* "test/f009_form_wide.test"
                     (("en_US\\.utf8")
                      "en_US.UTF-8")))
                 (alist-cons-after
                  'install 'post-install
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let* ((out   (assoc-ref outputs "out"))
                           (dir   (string-append out "/share/guile/site/"))
                           (files (find-files dir ".scm")))
                      (substitute* files
                        (("\"libguile-ncurses\"")
                         (format #f "\"~a/lib/libguile-ncurses\""
                                 out)))))
                  %standard-phases))))
    (home-page "http://www.gnu.org/software/guile-ncurses/")
    (synopsis "Guile bindings to ncurses")
    (description
     "guile-ncurses provides Guile language bindings for the ncurses
library.")
    (license lgpl3+)))

(define-public mcron
  (package
    (name "mcron")
    (version "1.0.8")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/mcron/mcron-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0zparwgf01jgl1x53ik71ghabldq6zz18ha4dscps1i0qrzgap1b"))
             (patches (list (search-patch "mcron-install.patch")))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("ed" ,ed) ("which" ,which) ("guile" ,guile-2.0)))
    (home-page "http://www.gnu.org/software/mcron/")
    (synopsis "Run jobs at scheduled times")
    (description
     "GNU Mcron is a complete replacement for Vixie cron.  It is used to run
tasks on a schedule, such as every hour or every Monday.  Mcron is written in
Guile, so its configuration can be written in Scheme; the original cron
format is also supported.")
    (license gpl3+)))

(define-public guile-lib
  (package
    (name "guile-lib")
    (version "0.2.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://savannah/guile-lib/guile-lib-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "1f9n2b5b5r75lzjinyk6zp6g20g60msa0jpfrk5hhg4j8cy0ih4b"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (alist-cons-before
                 'configure 'patch-module-dir
                 (lambda _
                   (substitute* "src/Makefile.in"
                     (("^moddir[[:blank:]]*=[[:blank:]]*([[:graph:]]+)" _ rhs)
                      (string-append "moddir = " rhs "/2.0\n"))))
                 %standard-phases)))
    (inputs `(("guile" ,guile-2.0)))
    (home-page "http://www.nongnu.org/guile-lib/")
    (synopsis "Collection of useful Guile Scheme modules")
    (description
     "Guile-Lib is intended as an accumulation place for pure-scheme Guile
modules, allowing for people to cooperate integrating their generic Guile
modules into a coherent library.  Think \"a down-scaled, limited-scope CPAN
for Guile\".")

    ;; The whole is under GPLv3+, but some modules are under laxer
    ;; distribution terms such as LGPL and public domain.  See `COPYING' for
    ;; details.
    (license gpl3+)))

(define-public guile-json
  (package
    (name "guile-json")
    (version "0.4.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://savannah/guile-json/guile-json-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0v06272rw4ycwzssjf3fzpk2vhpslvl55hz94q80vc6f74j0d5h6"))
             (modules '((guix build utils)))
             (snippet
              ;; Make sure everything goes under .../site/2.0, like Guile's
              ;; search paths expects.
              '(substitute* '("Makefile.in" "json/Makefile.in")
                 (("moddir =.*/share/guile/site" all)
                  (string-append all "/2.0"))))))
    (build-system gnu-build-system)
    (native-inputs `(("guile" ,guile-2.0)))
    (home-page "http://savannah.nongnu.org/projects/guile-json/")
    (synopsis "JSON module for Guile")
    (description
     "Guile-json supports parsing and building JSON documents according to the
http:://json.org specification.  These are the main features:
- Strictly complies to http://json.org specification.
- Build JSON documents programmatically via macros.
- Unicode support for strings.
- Allows JSON pretty printing.")
    (license lgpl3+)))

(define-public guile-minikanren
  (package
    (name "guile-minikanren")
    (version "20150424.e844d85")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ijp/minikanren.git")
                    (commit "e844d85512f8c055d3f96143ee506007389a25e3")))
              (sha256
               (base32
                "0r50jlpzi940jlmxyy3ddqqwmj5r12gb4bcv0ssini9v8km13xz6"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules
       ((guix build utils)
        (ice-9 match))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (ice-9 match))
         (let* ((out (assoc-ref %outputs "out"))
                (module-dir (string-append out "/share/guile/site/2.0"))
                (source (assoc-ref %build-inputs "source"))
                (doc (string-append out "/share/doc"))
                (scm-files '("minikanren.scm"
                             "minikanren/mkextraforms.scm"
                             "minikanren/mkprelude.scm"
                             "minikanren/mk.scm"))
                (guild (string-append (assoc-ref %build-inputs "guile")
                                      "/bin/guild")))
           ;; Make installation directories.
           (mkdir-p (string-append module-dir "/minikanren"))
           (mkdir-p doc)

           ;; Compile .scm files and install.
           (chdir source)
           (setenv "GUILE_AUTO_COMPILE" "0")
           (for-each (lambda (file)
                       (let* ((dest-file (string-append module-dir "/"
                                                        file ".scm"))
                              (go-file (match (string-split file #\.)
                                         ((base _)
                                          (string-append module-dir "/"
                                                         base ".go")))))
                         ;; Install source module.
                         (copy-file file dest-file)
                         ;; Install compiled module.
                         (unless (zero? (system* guild "compile"
                                                 "-L" source
                                                 "-o" go-file
                                                 file))
                           (error (format #f "Failed to compile ~s to ~s!"
                                          file go-file)))))
                     scm-files)

           ;; Also copy over the README.
           (copy-file "README.org" (string-append doc "/README.org"))
           #t))))
    (inputs
     `(("guile" ,guile-2.0)))
    (home-page "https://github.com/ijp/minikanren")
    (synopsis "MiniKanren declarative logic system, packaged for Guile")
    (description
     "MiniKanren is a relational programming extension to the Scheme
programming Language, written as a smaller version of Kanren suitable for
pedagogical purposes.  It is featured in the book, The Reasoned Schemer,
written by Dan Friedman, William Byrd, and Oleg Kiselyov.

This is Ian Price's r6rs packaged version of miniKanren, which deviates
slightly from miniKanren mainline.

See http://minikanren.org/ for more on miniKanren generally.")
    (license expat)))


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
              (sha256
               (base32
                "1j8wrsw7v9w6qkl47xz0rdikg50v16nn6kbs3lgzcymjzpa7babj"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules
       ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (system base compile))

         (let* ((out (assoc-ref %outputs "out"))
                (module-dir (string-append out "/share/guile/site/2.0"))
                (source (assoc-ref %build-inputs "source"))
                (doc (string-append out "/share/doc"))
                (guild (string-append (assoc-ref %build-inputs "guile")
                                      "/bin/guild"))
                (gdbm.scm-dest
                 (string-append module-dir "/gdbm.scm"))
                (gdbm.go-dest
                 (string-append module-dir "/gdbm.go")))
           ;; Make installation directories.
           (mkdir-p module-dir)
           (mkdir-p doc)

           ;; Switch directory for compiling and installing
           (chdir source)

           ;; copy the source
           (copy-file "gdbm.scm" gdbm.scm-dest)

           ;; Patch the FFI
           (substitute* gdbm.scm-dest
             (("\\(dynamic-link \"libgdbm\"\\)")
              (format #f "(dynamic-link \"~a/lib/libgdbm.so\")"
                      (assoc-ref %build-inputs "gdbm"))))

           ;; compile to the destination
           (compile-file gdbm.scm-dest
                         #:output-file gdbm.go-dest)))))
    (inputs
     `(("guile" ,guile-2.0)))
    (propagated-inputs
     `(("gdbm" ,gdbm)))
    (home-page "https://github.com/ijp/guile-gdbm")
    (synopsis "Guile bindings to the GDBM library via Guile's FFI")
    (description
     "Guile bindings to the GDBM key-value storage system, using
Guile's foreign function interface.")
    (license gpl3+)))

;;; guile.scm ends here
