;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013 Ludovic Courtès <ludo@gnu.org>
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
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

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
              "0l200a0v7h8bh0cwz6v7hc13ds39cgqsmfrks55b1rbj5vniyiy3"))))
   (build-system gnu-build-system)
   (arguments '(#:configure-flags '("--disable-error-on-warning")
                #:patches (list (assoc-ref %build-inputs "patch/snarf"))

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
   (inputs `(("patch/snarf" ,(search-patch "guile-1.8-cpp-4.5.patch"))
             ("gawk" ,gawk)
             ("readline" ,readline)))

   ;; Since `guile-1.8.pc' has "Libs: ... -lgmp -lltdl", these must be
   ;; propagated.
   (propagated-inputs `(("gmp" ,gmp)
                        ("libtool" ,libtool)))

   ;; When cross-compiling, a native version of Guile itself is needed.
   (self-native-input? #t)

   (synopsis "Scheme implementation intended especially for extensions")
   (description
"GNU Guile 1.8 is an interpreter for the Scheme programming language,
packaged as a library that can be embedded into programs to make them
extensible.  It supports many SRFIs.")
   (home-page "http://www.gnu.org/software/guile/")
   (license lgpl2.0+)))

(define-public guile-2.0
  (package
   (name "guile")
   (version "2.0.7")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/guile/guile-" version
                                ".tar.xz"))
            (sha256
             (base32
              "0f53pxkia4v17n0avwqlcjpy0n89hkazm2xsa6p84lv8k6k8y9vg"))))
   (build-system gnu-build-system)
   (native-inputs `(("pkgconfig" ,pkg-config)))
   (inputs `(("libffi" ,libffi)
             ("readline" ,readline)))

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

   (arguments
    '(#:phases (alist-cons-before
                'configure 'pre-configure
                (lambda* (#:key inputs #:allow-other-keys)
                  (let ((bash (assoc-ref inputs "bash")))
                    (substitute* "module/ice-9/popen.scm"
                      (("/bin/sh")
                       (string-append bash "/bin/bash")))))
                %standard-phases)))

   (synopsis "Scheme implementation intended especially for extensions")
   (description
"GNU Guile is an implementation of the Scheme programming language, with
support for many SRFIs, packaged for use in a wide variety of environments.
In addition to implementing the R5RS Scheme standard and a large subset of
R6RS, Guile includes a module system, full access to POSIX system calls,
networking support, multiple threads, dynamic linking, a foreign function
call interface, and powerful string processing.")
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
   (synopsis "Guile-Reader, a simple framework for building readers for
GNU Guile")
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
    (version "1.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/guile-ncurses/guile-ncurses-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0chvfjrlmg99db98ra9vzwjmbypqx7d4ssm8q0kvzi0n0p9irszi"))))
    (build-system gnu-build-system)
    (inputs `(("ncurses" ,ncurses)
              ("guile" ,guile-2.0)))
    (arguments
     '(#:configure-flags (list (string-append "--with-guilesitedir="
                                              (assoc-ref %outputs "out")
                                              "/share/guile/site"))
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
     "GNU Guile-Ncurses is a library for the Guile Scheme interpreter that
provides functions for creating text user interfaces.  The text user interface
functionality is built on the ncurses libraries: curses, form, panel, and
menu.")
    (license lgpl3+)))

(define-public mcron
  (package
    (name "mcron")
    (version "1.0.6")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/mcron/mcron-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0yvrfzzdy2m7fbqkr61fw01wd9r2jpnbyabxhcsfivgxywknl0fy"))))
    (build-system gnu-build-system)
    (arguments
     '(#:patches (list (assoc-ref %build-inputs "patch/install"))))
    (inputs
     `(("ed" ,ed) ("which" ,which) ("guile" ,guile-1.8)
       ("patch/install" ,(search-patch "mcron-install.patch"))))
    (home-page "http://www.gnu.org/software/mcron/")
    (synopsis "Run jobs at scheduled times")
    (description
     "The GNU package mcron (Mellor's cron) is a 100% compatible replacement
for Vixie cron.  It is written in pure Guile, and allows configuration files
to be written in scheme (as well as Vixie's original format) for infinite
flexibility in specifying when jobs should be run.  Mcron was written by Dale
Mellor.")
    (license gpl3+)))

;;; guile.scm ends here
