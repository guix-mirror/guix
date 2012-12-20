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

(define-module (distro packages ncurses)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu))

(define-public ncurses
  (let ((patch-makefile-phase
         '(lambda _
            (substitute* (find-files "." "Makefile.in")
              (("^SHELL[[:blank:]]*=.*$") ""))))
        (pre-install-phase
         '(lambda _
            (for-each patch-shebang (find-files "." "\\.sh$"))))
        (post-install-phase
         '(lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              ;; When building a wide-character (Unicode) build, create backward
              ;; compatibility links from the the "normal" libraries to the
              ;; wide-character libraries (e.g. libncurses.so to libncursesw.so).
              (with-directory-excursion (string-append out "/lib")
                (for-each (lambda (lib)
                            (define libw.a
                              (string-append "lib" lib "w.a"))
                            (define lib.a
                              (string-append "lib" lib ".a"))
                            (define libw.so.x
                              (string-append "lib" lib "w.so.5"))
                            (define lib.so.x
                              (string-append "lib" lib ".so.5"))
                            (define lib.so
                              (string-append "lib" lib ".so"))

                            (when (file-exists? libw.a)
                              (format #t "creating symlinks for `lib~a'~%" lib)
                              (symlink libw.a lib.a)
                              (symlink libw.so.x lib.so.x)
                              (false-if-exception (delete-file lib.so))
                              (call-with-output-file lib.so
                                (lambda (p)
                                  (format p "INPUT (-l~aw)~%" lib)))))
                          '("curses" "ncurses" "form" "panel" "menu")))))))
    (package
     (name "ncurses")
     (version "5.9")
     (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/ncurses/ncurses-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0fsn7xis81za62afan0vvm38bvgzg5wfmv1m86flqcj0nj7jjilh"))))
     (build-system gnu-build-system)
     (arguments
      (case-lambda
        ((system)
         `(#:configure-flags
           `("--with-shared" "--without-debug" "--enable-widec"

             ;; By default headers land in an `ncursesw' subdir, which is not
             ;; what users expect.
             ,(string-append "--includedir=" (assoc-ref %outputs "out")
                             "/include")

             ;; C++ bindings fail to build on
             ;; `i386-pc-solaris2.11' with GCC 3.4.3:
             ;; <http://bugs.opensolaris.org/bugdatabase/view_bug.do?bug_id=6395191>.
             ,,@(if (string=? system "i686-solaris")
                    '("--without-cxx-binding")
                    '()))
           #:tests? #f                            ; no "check" target
           #:phases (alist-cons-after
                     'install 'post-install ,post-install-phase
                     (alist-cons-before
                      'configure 'patch-makefile-SHELL
                      ,patch-makefile-phase
                      (alist-cons-before
                       'install 'pre-install-phase
                       ,pre-install-phase
                       %standard-phases)))

           ;; The `ncursesw5-config' has a #!/bin/sh that we don't want to
           ;; patch, to avoid retaining a reference to the build-time Bash.
           #:patch-shebangs? #f))
        ((system cross-system)
         (arguments cross-system))))
     (self-native-input? #t)
     (synopsis
      "GNU Ncurses, a free software emulation of curses in SVR4 and more")
     (description
      "The Ncurses (new curses) library is a free software emulation of curses
in System V Release 4.0, and more.  It uses Terminfo format, supports pads
and color and multiple highlights and forms characters and function-key
mapping, and has all the other SYSV-curses enhancements over BSD Curses.

The ncurses code was developed under GNU/Linux.  It has been in use for some
time with OpenBSD as the system curses library, and on FreeBSD and NetBSD as
an external package.  It should port easily to any ANSI/POSIX-conforming
UNIX.  It has even been ported to OS/2 Warp!")
     (license x11)
     (home-page "http://www.gnu.org/software/ncurses/"))))
