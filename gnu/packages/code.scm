;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
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
  #:use-module (guix licenses)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages emacs)
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
    (native-inputs `(("emacs" ,emacs)))

    (home-page "http://www.gnu.org/software/cflow/")
    (synopsis "Create a graph of control flow within a program")
    (description
     "GNU cflow analyzes C source files and produces a graph charting the
control flow of the program.  It can output the graph in several styles and
in either the POSIX format or in an extended GNU format. cflow also includes
a major mode for Emacs for examining the flowcharts that it produces.")
    (license gpl3+)))

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
    (license gpl3+)))

(define-public global                             ; a global variable
  (package
    (name "global")
    (version "6.3.4")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/global/global-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0hcplcayyjf42d8ygzla6142b5dq4ybq4wg3n3cgx3b5yfhvic85"))))
    (build-system gnu-build-system)
    (inputs `(("ncurses" ,ncurses)
              ("libltdl" ,libltdl)))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-ncurses="
                            (assoc-ref %build-inputs "ncurses")))

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
    (license gpl3+)))

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
                  (delete configure)
                  (add-before build make-dotl-files-older
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
                  (add-before install make-target-directories
                              (lambda* (#:key outputs #:allow-other-keys)
                                (let ((out (assoc-ref outputs "out")))
                                  (mkdir-p (string-append out "/bin"))
                                  (mkdir-p (string-append out
                                                          "/share/man/man1"))
                                  (mkdir-p (string-append out
                                                          "/share/doc")))))
                  (replace check
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
    (license gpl2+)))
