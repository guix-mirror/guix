;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages code)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages autogen)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages autotools))

;;; Tools to deal with source code: metrics, cross-references, etc.

(define-public cflow
  (package
    (name "cflow")
    (version "1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/cflow/cflow-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "1jkbq97ajcf834z68hbn3xfhiz921zhn39gklml1racf0kb3jzh3"))))
    (build-system gnu-build-system)

    ;; Needed to have cflow-mode.el installed.
    (native-inputs `(("emacs" ,emacs-no-x)))

    (home-page "http://www.gnu.org/software/cflow/")
    (synopsis "Create a graph of control flow within a program")
    (description
     "GNU cflow analyzes C source files and produces a graph charting the
control flow of the program.  It can output the graph in several styles and
in either the POSIX format or in an extended GNU format.  cflow also includes
a major mode for Emacs for examining the flowcharts that it produces.")
    (license license:gpl3+)))

(define-public complexity
  (package
    (name "complexity")
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/complexity/complexity-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0pmlhlj1chl4caaqffvn1cy9z8gwmjbx97syi7pdfa0vqygkql6d"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("texinfo" ,texinfo)
       ("autogen" ,autogen)))
    (home-page "http://www.gnu.org/software/complexity/")
    (synopsis "Analyze complexity of C functions")
    (description
     "GNU complexity provides tools for finding procedures that are
convoluted, overly long or otherwise difficult to understand.  This
may help in learning or reviewing unfamiliar code or perhaps
highlighting your own code that seemed comprehensible when you wrote it.")
    (license license:gpl3+)))

(define-public global                             ; a global variable
  (package
    (name "global")
    (version "6.5")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/global/global-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "1af6lhwhrpdnigd3707fnk3dd6y53pbc4g0i75xjf0563bdi5zaa"))))
    (build-system gnu-build-system)
    (inputs `(("ncurses" ,ncurses)
              ("libltdl" ,libltdl)
              ("sqlite" ,sqlite)))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-ncurses="
                            (assoc-ref %build-inputs "ncurses"))
             (string-append "--with-sqlite3="
                            (assoc-ref %build-inputs "sqlite")))

       #:phases (alist-cons-after
                 'install 'post-install
                 (lambda* (#:key outputs #:allow-other-keys)
                   ;; Install the Emacs Lisp file in the right place.
                   (let* ((out  (assoc-ref outputs "out"))
                          (data (string-append out "/share/gtags"))
                          (lisp (string-append out "/share/emacs/site-lisp")))
                     (mkdir-p lisp)
                     (copy-file (string-append data "/gtags.el")
                                (string-append lisp "/gtags.el"))
                     (delete-file (string-append data "/gtags.el"))
                     #t))
                 %standard-phases)))
    (home-page "http://www.gnu.org/software/global/")
    (synopsis "Cross-environment source code tag system")
    (description
     "GLOBAL is a source code tagging system that functions in the same way
across a wide array of environments, such as different text editors, shells
and web browsers.  The resulting tags are useful for quickly moving around in
a large, deeply nested project.")
    (license license:gpl3+)))

(define-public sloccount
  (package
    (name "sloccount")
    (version "2.26")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.dwheeler.com/sloccount/sloccount-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0ayiwfjdh1946asah861ah9269s5xkc8p5fv1wnxs9znyaxs4zzs"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-before 'build 'make-dotl-files-older
                              (lambda _
                                ;; Make the '.l' files as old as the '.c'
                                ;; files to avoid triggering the rule that
                                ;; requires Flex.
                                (define ref
                                  (stat "README"))

                                (for-each (lambda (file)
                                            (set-file-time file ref))
                                          (find-files "." "\\.[chl]$"))
                                #t))
                  (add-before 'install 'make-target-directories
                              (lambda* (#:key outputs #:allow-other-keys)
                                (let ((out (assoc-ref outputs "out")))
                                  (mkdir-p (string-append out "/bin"))
                                  (mkdir-p (string-append out
                                                          "/share/man/man1"))
                                  (mkdir-p (string-append out
                                                          "/share/doc")))))
                  (replace 'check
                           (lambda _
                             (setenv "HOME" (getcwd))
                             (setenv "PATH"
                                     (string-append (getcwd) ":"
                                                    (getenv "PATH")))
                             (zero? (system* "make" "test")))))

        #:make-flags (list (string-append "PREFIX="
                                          (assoc-ref %outputs "out")))))
    (inputs `(("perl" ,perl)))
    (home-page "http://www.dwheeler.com/sloccount/")
    (synopsis "Count physical source lines of code (SLOC)")
    (description
     "SLOCCount is a set of the programs for counting source lines of
code (SLOC) in large software systems.  It can automatically identify and
measure a wide range of programming languages.  It automatically estimates the
effort, time, and money it would take to develop the software, using the
COCOMO model or user-provided parameters.")
    (license license:gpl2+)))

(define-public the-silver-searcher
  (package
    (name "the-silver-searcher")
    (version "0.29.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/ggreer/the_silver_searcher/archive/"
                    version ".tar.gz"))
              (sha256
               (base32
                "0ah7vcqprl9hhafi68bvzaiywy7dfm28zf7kpw3xrlqzfn0vg7kp"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("pcre" ,pcre)
       ("xz" ,xz)
       ("zlib" ,zlib)))
    (arguments
     `(#:phases
       ;; There is no configure yet, so let's create it, but let configure and
       ;; make do the work in later phases.
       (alist-cons-before 'configure 'autoconf
                          (lambda _
                            (substitute* "build.sh"
                              (("./configure") "true")
                              (("make -j4") "true"))
                            (zero? (system* "sh" "build.sh")))
                          %standard-phases)))
    (home-page "http://geoff.greer.fm/ag/")
    (synopsis "Fast code searching tool")
    (description
     "The silver searcher, or 'ag', is tool for quickly searching through
files, but compared to grep is much faster and respects files like .gitignore,
.hgignore, etc.")
    (license license:asl2.0)))

(define-public withershins
  (package
    (name "withershins")
    (version "0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/cameronwhite/withershins/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "08z3lyvswx7sad10637vfpwglbcbgzzcpfihw0x8lzr74f3b70bh"))))
    (build-system cmake-build-system)
    (arguments
     `(#:out-of-source? #f
       #:phases
       (modify-phases %standard-phases
         (add-after
          'unpack 'find-libiberty
          (lambda _
            (let ((libiberty (assoc-ref %build-inputs "libiberty")))
              (substitute* "cmake/FindIberty.cmake"
                (("/usr/include") (string-append libiberty "/include"))
                (("libiberty.a iberty")
                 (string-append "NAMES libiberty.a iberty\nPATHS \""
                                libiberty "/lib" "\"")))
              #t)))
         (replace
          'install
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (mkdir-p (string-append out "/lib"))
              (mkdir (string-append out "/include"))
              (copy-file "src/withershins.hpp"
                         (string-append out "/include/withershins.hpp"))
              (copy-file "src/libwithershins.a"
                         (string-append out "/lib/libwithershins.a")))
            #t)))))
    (home-page "https://github.com/cameronwhite/withershins")
    (inputs
     `(("libiberty" ,libiberty)
       ("binutils" ,binutils) ;for libbfd
       ("zlib" ,zlib)))
    (synopsis "C++11 library for generating stack traces")
    (description
     "Withershins is a simple cross-platform C++11 library for generating
stack traces.")
    ;; Sources are released under Expat license, but since BFD is licensed
    ;; under the GPLv3+ the combined work is GPLv3+ as well.
    (license license:gpl3+)))
