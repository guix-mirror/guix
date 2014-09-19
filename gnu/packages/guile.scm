;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
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
  #:use-module (gnu packages libunistring)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages ed)
  #:use-module (gnu packages which)
  #:use-module (gnu packages gtk)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
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
                        ("libtool" ,libtool)))

   ;; When cross-compiling, a native version of Guile itself is needed.
   (self-native-input? #t)

   (native-search-paths
    (list (search-path-specification
           (variable "GUILE_LOAD_PATH")
           (directories '("share/guile/site")))))

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
              "1qh3j7308qvsjgwf7h94yqgckpbgz2k3yqdkzsyhqcafvfka9l5f"))))
   (build-system gnu-build-system)
   (native-inputs `(("pkgconfig" ,pkg-config)))
   (inputs `(("libffi" ,libffi)
             ("readline" ,readline)

             ;; TODO: On next core-updates, make Bash input unconditional.
             ,@(if (%current-target-system)
                   `(("bash" ,bash))
                   '())))

   (propagated-inputs
    `( ;; These ones aren't normally needed here, but since `libguile-2.0.la'
       ;; reads `-lltdl -lunistring', adding them here will add the needed
       ;; `-L' flags.  As for why the `.la' file lacks the `-L' flags, see
       ;; <http://thread.gmane.org/gmane.comp.lib.gnulib.bugs/18903>.
      ("libunistring" ,libunistring)
      ("libtool" ,libtool)

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
           (directories '("share/guile/site/2.0")))
          (search-path-specification
           (variable "GUILE_LOAD_COMPILED_PATH")
           (directories '("share/guile/site/2.0")))))

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


;;;
;;; Extensions.
;;;

(define (guile-reader guile)
  "Build Guile-Reader against GUILE, a package of some version of Guile 1.8
or 2.0."
  (package
   (name (string-append "guile-reader-for-guile_" (package-version guile)))
   (version "0.6")
   (source  (origin
             (method url-fetch)
             (uri (string-append
                   "http://download-mirror.savannah.gnu.org/releases/guile-reader/guile-reader-"
                   version ".tar.gz"))
             (sha256
              (base32
               "1svlyk5pm4fsdp2g7n6qffdl6fdggxnlicj0jn9s4lxd63gzxy1n"))))
   (build-system gnu-build-system)
   (native-inputs `(("pkgconfig" ,pkg-config)
                    ("gperf" ,gperf)))
   (inputs `(("guile" ,guile)))
   (arguments `(#:configure-flags
                (let ((out (assoc-ref %outputs "out")))
                  ,(if (string-prefix? "2." (package-version guile))
                       '(list (string-append "--with-guilemoduledir="
                                             out "/share/guile/site/2.0"))
                       '(list (string-append "--with-guilemoduledir="
                                             out "/share/guile/site"))))))
   (synopsis "Framework for building readers for GNU Guile")
   (description
"Guile-Reader is a simple framework for building readers for GNU Guile.

The idea is to make it easy to build procedures that extend Guile’s read
procedure. Readers supporting various syntax variants can easily be written,
possibly by re-using existing “token readers” of a standard Scheme
readers. For example, it is used to implement Skribilo’s R5RS-derived
document syntax.

Guile-Reader’s approach is similar to Common Lisp’s “read table”, but
hopefully more powerful and flexible (for instance, one may instantiate as
many readers as needed).")
   (home-page "http://www.nongnu.org/guile-reader/")
   (license gpl3+)))

(define-public guile-reader/guile-1.8
  ;; Guile-Reader built against Guile 1.8.
  (guile-reader guile-1.8))

(define-public guile-reader/guile-2.0
  ;; Guile-Reader built against Guile 2.0.
  (guile-reader guile-2.0))

(define-public guile-ncurses
  (package
    (name "guile-ncurses")
    (version "1.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/guile-ncurses/guile-ncurses-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "070wl664lsm14hb6y9ch97x9q6cns4k6nxgdzbdzi5byixn74899"))
             (patches (list (search-patch "guile-ncurses-tests.patch")))))
    (build-system gnu-build-system)
    (inputs `(("ncurses" ,ncurses)
              ("guile" ,guile-2.0)))
    (arguments
     '(#:configure-flags (list "--with-ncursesw"  ; Unicode support
                               (string-append "--with-guilesitedir="
                                              (assoc-ref %outputs "out")
                                              "/share/guile/site/2.0"))
       #:phases (alist-cons-after
                 'install 'post-install
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let* ((out   (assoc-ref outputs "out"))
                          (dir   (string-append out "/share/guile/site/"))
                          (files (find-files dir ".scm")))
                    (substitute* files
                      (("\"libguile-ncurses\"")
                       (format #f "\"~a/lib/libguile-ncurses\""
                               out)))))
                 %standard-phases)))
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
     "guile-lib is intended as an accumulation place for pure-scheme Guile
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
    (inputs `(("guile" ,guile-2.0)))
    (home-page "http://savannah.nongnu.org/projects/guile-json/")
    (synopsis "JSON module for Guile")
    (description
     "Guile-json supports parsing and building JSON documents according to the
http:://json.org specification. These are the main features:
- Strictly complies to http://json.org specification.
- Build JSON documents programmatically via macros.
- Unicode support for strings.
- Allows JSON pretty printing.")
    (license lgpl3+)))

(define-public guile-charting
  (package
    (name "guile-charting")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://wingolog.org/pub/guile-charting/"
                                  "guile-charting-" version ".tar.gz"))
              (sha256
               (base32
                "0w5qiyv9v0ip5li22x762bm48g8xnw281w66iyw094zdw611pb2m"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Use the standard location for modules.
                  (substitute* "Makefile.in"
                    (("godir = .*$")
                     "godir = $(moddir)\n"))))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("guile" ,guile-2.0)))
    (propagated-inputs `(("guile-cairo" ,guile-cairo)))
    (home-page "http://wingolog.org/software/guile-charting/")
    (synopsis "Create charts and graphs in Guile")
    (description
     "Guile-Charting is a Guile Scheme library to create bar charts and graphs
using the Cairo drawing library.")
    (license lgpl2.1+)))

;;; guile.scm ends here
