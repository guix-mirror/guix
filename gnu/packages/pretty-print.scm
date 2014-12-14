;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
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

(define-module (gnu packages pretty-print)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages gv)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages perl))

(define-public a2ps
  (package
    (name "a2ps")
    (version "4.14")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/a2ps/a2ps-"
                          version ".tar.gz"))
      (sha256
       (base32
        "195k78m1h03m961qn7jr120z815iyb93gwi159p1p9348lyqvbpk"))))
    (build-system gnu-build-system)
    (inputs
     `(("psutils" ,psutils)
       ("groff" ,groff)
       ("gv" ,gv)
       ("imagemagick" ,imagemagick)))
    (native-inputs
     `(("gperf" ,gperf)
       ("perl" ,perl)))
    (arguments
     '(#:phases (alist-cons-before
                 'build 'patch-scripts
                 (lambda _
                   (substitute*
                       '("afm/make_fonts_map.sh"
                         "tests/defs"
                         "tests/backup.tst"
                         "tests/styles.tst")
                     (("/bin/rm") (which "rm"))))
                 (alist-cons-before
                  'check 'patch-test-files
                  ;; Alternatively, we could unpatch the shebangs in tstfiles
                  (lambda* (#:key inputs #:allow-other-keys)
                    (let ((perl (assoc-ref inputs "perl")))
                      (substitute* '("tests/ps-ref/includeres.ps"
                                     "tests/gps-ref/includeres.ps")
                        (("/usr/local/bin/perl")
                         (string-append perl "/bin/perl"))))
                    ;; Some of the reference postscript contain a 'version 3'
                    ;; string that in inconsistent with the source text in the
                    ;; tstfiles directory.  Erroneous search-and-replace?
                    (substitute* '("tests/ps-ref/InsertBlock.ps"
                                   "tests/gps-ref/InsertBlock.ps"
                                   "tests/ps-ref/bookie.ps"
                                   "tests/gps-ref/bookie.ps")
                      (("version 3") "version 2"))
                    (substitute* '("tests/ps-ref/psmandup.ps"
                                   "tests/gps-ref/psmandup.ps")
                      (("#! */bin/sh") (string-append
                                        "#!" (which "sh")))))
                  %standard-phases))))
    (home-page "http://www.gnu.org/software/a2ps")
    (synopsis "Any file to PostScript, including pretty-printing")
    (description
     "GNU a2ps converts almost anything to a PostScript file, ready for
printing.  It accomplishes this by being able to delegate files to external
handlers, such as Groff and Gzip.  It handles as many steps as is necessary to
produce a pretty-printed file.  It also includes some extra abilities for
special cases, such as pretty-printing \"--help\" output.")
    (license gpl3+)))

(define-public trueprint
  (package
    (name "trueprint")
    (version "5.4")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/trueprint/trueprint-"
                          version ".tar.gz"))
      (sha256
       (base32
        "13rkc0fga10xyf56yy9dnq95zndnfadkhxflnp24skszj21y8jqh"))))
    (build-system gnu-build-system)
    (arguments
     ;; Must define DIFF_CMD for tests to pass
     '(#:configure-flags '("CPPFLAGS=-DDIFF_CMD=\\\"diff\\\"")))
    (home-page "http://www.gnu.org/software/trueprint")
    (synopsis "Pretty-print C sources and other plain text to PostScript")
    (description
     "GNU Trueprint translates C source code files as PostScript files.
In addition to the basic source code output, it can also perform diff-marking,
indentation counting, function and file indices and more.")
    (license gpl2)))

(define-public enscript
  (package
    (name "enscript")
    (version "1.6.6")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/enscript/enscript-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1fy0ymvzrrvs889zanxcaxjfcxarm2d3k43c9frmbl1ld7dblmkd"))))
    (build-system gnu-build-system)
    (home-page "http://www.gnu.org/software/enscript")
    (synopsis "Generating PostScript, including pretty-printing")
    (description
     "GNU Enscript is a program to convert ASCII text files to PostScript,
HTML or RTF formats, to be stored in files or sent immediately to a printer. 
It also includes the capability to perform syntax highlighting for several
different programming languages.")
    (license gpl3+)))

(define-public source-highlight
  (package
    (name "source-highlight")
    (version "3.1.7")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/src-highlite/source-highlight-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1s49ld8cnpzhhwq0r7s0sfm3cg3nhhm0wla27lwraifrrl3y1cp1"))
      (patches
       (list (search-patch
              ;; Patch submitted as Savannah item #41786
              "source-highlight-regexrange-test.patch")))))
    (build-system gnu-build-system)
    ;; The ctags that comes with emacs does not support the --excmd options,
    ;; so can't be used
    (inputs
     `(("boost" ,boost)))
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-boost="
                            (assoc-ref %build-inputs "boost")))
       #:parallel-tests? #f             ;There appear to be race conditions
       #:phases (alist-cons-before
                 'check 'patch-test-files
                 (lambda _
                   ;; Unpatch shebangs in test input so that source-highlight
                   ;; is still able to infer input language
                   (substitute* '("tests/test.sh"
                                  "tests/test2.sh"
                                  "tests/test.tcl")
                     (((string-append "#! *" (which "sh"))) "#!/bin/sh"))
                   ;; Initial patching unrecoverably removes whitespace, so
                   ;; remove it also in the comparison output.
                   (substitute* '("tests/test.sh.html"
                                  "tests/test2.sh.html"
                                  "tests/test.tcl.html")
                     (("#! */bin/sh") "#!/bin/sh")))
                 %standard-phases)))
    (home-page "http://www.gnu.org/software/src-highlite")
    (synopsis "Produce a document with syntax highlighting from a source file")
    (description
     "GNU source-highlight reads in a source code file and produces an output
file in which the keywords are highlighted in different colors to designate
their syntactic role.  It supports over 150 different languages and it can
output to 8 different formats, including HTML, LaTeX and ODF.  It can also
output to ANSI color escape sequences, so that highlighted source code can be
seen in a terminal.")
    (license gpl3+)))

(define-public astyle
  (package
    (name "astyle")
    (version "2.05")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/astyle/astyle/astyle%20"
                           version "/astyle_"  version "_linux.tar.gz"))
       (sha256
        (base32
         "0f9sh9kq5ajp1yz133h00fr9235p1m698x7n3h7zbrhjiwgynd6s"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;no tests
       #:make-flags (list (string-append "prefix=" %output)
                          "INSTALL=install"
                          "all")
       #:phases (alist-replace
                 'configure
                 (lambda _ (chdir "build/gcc"))
                 (alist-cons-after
                  'install 'install-libs
                  (lambda* (#:key outputs #:allow-other-keys)
                    ;; Libraries are not installed by default
                    (let* ((output (assoc-ref outputs "out"))
                           (libdir (string-append output "/lib")))
                      (begin
                        (mkdir-p libdir)
                        (for-each (lambda (l)
                                    (copy-file
                                     l (string-append libdir "/" (basename l))))
                                  (find-files "bin" "lib*")))))
                  %standard-phases))))
    (home-page "http://astyle.sourceforge.net/")
    (synopsis "Source code indenter, formatter, and beautifier")
    (description
     "Artistic Style is a source code indenter, formatter, and beautifier for
the C, C++, C++/CLI, Objective‑C, C#, and Java programming languages.")
    (license lgpl3+)))
