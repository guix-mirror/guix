;;; Guix --- Nix package management from Guile.         -*- coding: utf-8 -*-
;;; Copyright (C) 2012 Ludovic Courtès <ludo@gnu.org>
;;;
;;; This file is part of Guix.
;;;
;;; Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (distro base)
  #:use-module (guix packages)
  #:use-module (guix http)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils))

;;; Commentary:
;;;
;;; A Guix-based distribution.
;;;
;;; Code:

(define-public libsigsegv
  (package
   (name "libsigsegv")
   (version "2.10")
   (source (origin
            (method http-fetch)
            (uri (string-append
                  "http://ftp.gnu.org/gnu/libsigsegv/libsigsegv-"
                  version ".tar.gz"))
            (sha256
             (base32 "16hrs8k3nmc7a8jam5j1fpspd6sdpkamskvsdpcw6m29vnis8q44"))))
   (build-system gnu-build-system)
   (outputs '("out" "lib"))                   ; separate libdir from the rest
   (home-page "http://www.gnu.org/software/libsigsegv/")
   (description "GNU libsigsegv, a library to handle page faults in user mode")
   (long-description
"GNU libsigsegv is a library for handling page faults in user mode. A page
fault occurs when a program tries to access to a region of memory that is
currently not available. Catching and handling a page fault is a useful
technique for implementing pageable virtual memory, memory-mapped access to
persistent databases, generational garbage collectors, stack overflow
handlers, distributed shared memory, and more.")
   (license "GPLv2+")))

(define-public gawk
  (package
   (name "gawk")
   (version "4.0.0")
   (source (origin
            (method http-fetch)
            (uri (string-append "http://ftp.gnu.org/gnu/gawk/gawk-" version
                                ".tar.bz2"))
            (sha256
             (base32 "0sss7rhpvizi2a88h6giv0i7w5h07s2fxkw3s6n1hqvcnhrfgbb0"))))
   (build-system gnu-build-system)
   (arguments (case-lambda
                ((system)
                 (if (string=? system "i686-cygwin")
                     '(#:tests? #f)      ; work around test failure on Cygwin
                     '(#:parallel-tests? #f))) ; test suite fails in parallel
                ((system cross-system)
                 '(#:parallel-tests? #f))))
   (inputs `(("libsigsegv" ,libsigsegv)             ; headers
             ("libsigsegv/lib" ,libsigsegv "lib"))) ; library
   (home-page "http://www.gnu.org/software/gawk/")
   (description "GNU implementation of the Awk programming language")
   (long-description
    "Many computer users need to manipulate text files: extract and then
operate on data from parts of certain lines while discarding the rest, make
changes in various text files wherever certain patterns appear, and so on.
To write a program to do these things in a language such as C or Pascal is a
time-consuming inconvenience that may take many lines of code.  The job is
easy with awk, especially the GNU implementation: Gawk.

The awk utility interprets a special-purpose programming language that makes
it possible to handle many data-reformatting jobs with just a few lines of
code.")
   (license "GPLv3+")))

(define-public hello
  (package
   (name "hello")
   (version "2.8")
   (source (origin
            (method http-fetch)
            (uri (string-append "http://ftp.gnu.org/gnu/hello/hello-" version
                                ".tar.gz"))
            (sha256
             (base32 "0wqd8sjmxfskrflaxywc7gqw7sfawrfvdxd9skxawzfgyy0pzdz6"))))
   (build-system gnu-build-system)
   (arguments '(#:configure-flags
                `("--disable-dependency-tracking"
                  ,(string-append "--with-gawk="  ; for illustration purposes
                                 (assoc-ref %build-inputs "gawk")))))
   (inputs `(("gawk" ,gawk)))
   (description "GNU Hello")
   (long-description "Yeah...")
   (home-page "http://www.gnu.org/software/hello/")
   (license "GPLv3+")))

(define-public guile-1.8
  (package
   (name "guile")
   (version "1.8.8")
   (source (origin
            (method http-fetch)
            (uri (string-append "http://ftp.gnu.org/gnu/guile/guile-" version
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
                           'patch-loader-search-path
                           (lambda* (#:key outputs #:allow-other-keys)
                             ;; Add a call to `lt_dladdsearchdir' so that
                             ;; `libguile-readline.so' & co. are in the
                             ;; loader's search path.
                             (substitute* "libguile/dynl.c"
                                          (("lt_dlinit.*$" match)
                                           (format #f
                                                   "  ~a~%  lt_dladdsearchdir(\"~a/lib\");~%"
                                                   match
                                                   (assoc-ref outputs "out")))))
                           %standard-phases)))
   (inputs `(("patch/snarf"
              ,(search-path %load-path "distro/guile-1.8-cpp-4.5.patch"))
             ("gawk" ,gawk)
             ("readline" ,(nixpkgs-derivation "readline"))
             ("gmp" ,(nixpkgs-derivation "gmp"))
             ("libtool" ,(nixpkgs-derivation "libtool"))))

   ;; When cross-compiling, a native version of Guile itself is needed.
   (self-native-input? #t)

   (description "GNU Guile 1.8, an embeddable Scheme interpreter")
   (long-description
"GNU Guile 1.8 is an interpreter for the Scheme programming language,
packaged as a library that can be embedded into programs to make them
extensible.  It supports many SRFIs.")
   (home-page "http://www.gnu.org/software/guile/")
   (license "LGPLv2+")))

(define-public guile-2.0
  (package
   (name "guile")
   (version "2.0.6")
   (source (origin
            (method http-fetch)
            (uri (string-append "http://ftp.gnu.org/gnu/guile/guile-" version
                                ".tar.xz"))
            (sha256
             (base32
              "000ng5qsq3cl1k35jvzvhwxj92wx4q87745n2fppkd4irh58vv5l"))))
   (build-system gnu-build-system)
   (native-inputs `(("xz" ,(nixpkgs-derivation "xz"))
                    ("pkgconfig" ,(nixpkgs-derivation "pkgconfig"))))
   (inputs `(("libunistring" ,(nixpkgs-derivation "libunistring"))
             ("libffi" ,(nixpkgs-derivation "libffi"))
             ("libtool" ,(nixpkgs-derivation "libtool"))
             ("readline" ,(nixpkgs-derivation "readline"))))

   ;; The headers and/or `guile-2.0.pc' refer to these packages, so they must
   ;; be propagated.
   (propagated-inputs `(("bdw-gc" ,(nixpkgs-derivation "boehmgc"))
                        ("gmp" ,(nixpkgs-derivation "gmp"))))

   (self-native-input? #t)

   (description "GNU Guile 2.0, an embeddable Scheme implementation")
   (long-description
"GNU Guile is an implementation of the Scheme programming language, with
support for many SRFIs, packaged for use in a wide variety of environments.
In addition to implementing the R5RS Scheme standard and a large subset of
R6RS, Guile includes a module system, full access to POSIX system calls,
networking support, multiple threads, dynamic linking, a foreign function
call interface, and powerful string processing.")
   (home-page "http://www.gnu.org/software/guile/")
   (license "LGPLv3+")))

(define-public lout
  ;; This one is a bit tricky, because it doesn't follow the GNU Build System
  ;; rules.  Instead, it has a makefile that has to be patched to set the
  ;; prefix, etc., and it has no makefile rules to build its doc.
  (let ((configure-phase
         '(lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out"))
                  (doc (assoc-ref outputs "doc")))
              (substitute* "makefile"
                (("^PREFIX[[:blank:]]*=.*$")
                 (string-append "PREFIX = " out "\n"))
                (("^LOUTLIBDIR[[:blank:]]*=.*$")
                 (string-append "LOUTLIBDIR = " out "/lib/lout\n"))
                (("^LOUTDOCDIR[[:blank:]]*=.*$")
                 (string-append "LOUTDOCDIR = " doc "/doc/lout\n"))
                (("^MANDIR[[:blank:]]*=.*$")
                 (string-append "MANDIR = " out "/man\n")))
              (mkdir out)
              (mkdir (string-append out "/bin"))  ; TODO: use `mkdir-p'
              (mkdir (string-append out "/lib"))
              (mkdir (string-append out "/man"))
              (mkdir doc)
              (mkdir (string-append doc "/doc"))
              (mkdir (string-append doc "/doc/lout")))))
        (install-man-phase
         '(lambda* (#:key outputs #:allow-other-keys)
            (zero? (system* "make" "installman"))))
        (doc-phase
         '(lambda* (#:key outputs #:allow-other-keys)
            (define out
              (assoc-ref outputs "doc"))

            (setenv "PATH"
                    (string-append (assoc-ref outputs "out")
                                   "/bin:" (getenv "PATH")))
            (chdir "doc")
            (every (lambda (doc)
                     (format #t "doc: building `~a'...~%" doc)
                     (with-directory-excursion doc
                       (let ((file (string-append out "/doc/lout/"
                                                  doc ".ps")))
                         (and (or (file-exists? "outfile.ps")
                                  (zero? (system* "lout" "-r4" "-o"
                                                  "outfile.ps" "all")))
                              (begin
                                (copy-file "outfile.ps" file)
                                #t)
                              (zero? (system* "ps2pdf"
                                              "-dPDFSETTINGS=/prepress"
                                              "-sPAPERSIZE=a4"
                                              file
                                              (string-append out "/doc/lout/"
                                                             doc ".pdf")))))))
                   '("design" "expert" "slides" "user")))))
   (package
    (name "lout")
    (version "3.39")
    (source (origin
             (method http-fetch)
             ;; FIXME: `http-get' doesn't follow redirects, hence the URL.
             (uri (string-append
                   "http://download-mirror.savannah.gnu.org/releases/lout/lout-"
                   version ".tar.gz"))
             (sha256
              (base32
               "12gkyqrn0kaa8xq7sc7v3wm407pz2fxg9ngc75aybhi5z825b9vq"))))
    (build-system gnu-build-system)               ; actually, just a makefile
    (outputs '("out" "doc"))
    (inputs `(("ghostscript" ,(nixpkgs-derivation "ghostscript"))))
    (arguments `(#:modules ((guix build utils)
                            (guix build gnu-build-system)
                            (srfi srfi-1))        ; we need SRFI-1
                 #:tests? #f                      ; no "check" target

                 ;; Customize the build phases.
                 #:phases (alist-replace
                           'configure ,configure-phase

                           (alist-cons-after
                            'install 'install-man-pages
                            ,install-man-phase

                            (alist-cons-after
                             'install 'install-doc
                             ,doc-phase
                             %standard-phases)))))
    (description "Lout, a document layout system similar in style to LaTeX")
    (long-description
"The Lout document formatting system is now reads a high-level description of
a document similar in style to LaTeX and produces a PostScript or plain text
output file.

Lout offers an unprecedented range of advanced features, including optimal
paragraph and page breaking, automatic hyphenation, PostScript EPS file
inclusion and generation, equation formatting, tables, diagrams, rotation and
scaling, sorted indexes, bibliographic databases, running headers and
odd-even pages, automatic cross referencing, multilingual documents including
hyphenation (most European languages are supported), formatting of computer
programs, and much more, all ready to use.  Furthermore, Lout is easily
extended with definitions which are very much easier to write than troff of
TeX macros because Lout is a high-level, purely functional language, the
outcome of an eight-year research project that went back to the
beginning.")
    (license "GPLv3+")
    (home-page "http://savannah.nongnu.org/projects/lout/"))))

;;; Local Variables:
;;; eval: (put 'lambda* 'scheme-indent-function 1)
;;; eval: (put 'substitute* 'scheme-indent-function 1)
;;; eval: (put 'with-directory-excursion 'scheme-indent-function 1)
;;; End:
