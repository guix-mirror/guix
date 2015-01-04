;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu packages ncurses)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public ncurses
  (let ((patch-makefile-phase
         '(lambda _
            (for-each patch-makefile-SHELL
                      (find-files "." "Makefile.in"))))
        (configure-phase
         '(lambda* (#:key inputs outputs configure-flags
                    #:allow-other-keys)
            ;; The `ncursesw5-config' has a #!/bin/sh.  We want to patch
            ;; it to point to libc's embedded Bash, to avoid retaining a
            ;; reference to the bootstrap Bash.
            (let* ((libc (assoc-ref inputs "libc"))
                   (bash (string-append libc "/bin/bash"))
                   (out  (assoc-ref outputs "out")))
              (format #t "configure flags: ~s~%" configure-flags)
              (zero? (apply system* bash "./configure"
                            (string-append "SHELL=" bash)
                            (string-append "CONFIG_SHELL=" bash)
                            (string-append "--prefix=" out)
                            configure-flags)))))
        (cross-pre-install-phase
         '(lambda _
            ;; Run the native `tic' program, not the cross-built one.
            (substitute* "misc/run_tic.sh"
              (("\\{TIC_PATH:=.*\\}")
               "{TIC_PATH:=true}")
              (("cross_compiling:=no")
               "cross_compiling:=yes"))))
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
      `(#:configure-flags
        `("--with-shared" "--without-debug" "--enable-widec"

          ;; By default headers land in an `ncursesw' subdir, which is not
          ;; what users expect.
          ,(string-append "--includedir=" (assoc-ref %outputs "out")
                          "/include")

          ;; Make sure programs like 'tic', 'reset', and 'clear' have a
          ;; correct RUNPATH.
          ,(string-append "LDFLAGS=-Wl,-rpath=" (assoc-ref %outputs "out")
                          "/lib")

          ;; C++ bindings fail to build on
          ;; `i386-pc-solaris2.11' with GCC 3.4.3:
          ;; <http://bugs.opensolaris.org/bugdatabase/view_bug.do?bug_id=6395191>.
          ,,@(if (string=? (%current-system) "i686-solaris")
                 '("--without-cxx-binding")
                 '()))
        #:tests? #f                               ; no "check" target
        #:phases ,(if (%current-target-system)

                      `(alist-cons-before         ; cross build
                        'configure 'patch-makefile-SHELL
                        ,patch-makefile-phase
                        (alist-cons-before
                         'install 'pre-install
                         ,cross-pre-install-phase
                         (alist-cons-after
                          'install 'post-install ,post-install-phase
                          %standard-phases)))

                      `(alist-cons-after          ; native build
                        'install 'post-install ,post-install-phase
                        (alist-cons-before
                         'configure 'patch-makefile-SHELL
                         ,patch-makefile-phase
                         (alist-replace
                          'configure
                          ,configure-phase
                          %standard-phases))))))
     (self-native-input? #t)                      ; for `tic'
     (synopsis "Terminal emulation (termcap, terminfo) library")
     (description
      "GNU Ncurses is a library which provides capabilities to write text to
a terminal in a terminal-independent manner.  It supports pads and color as
well as multiple highlights and forms characters.  It is typically used to
implement user interfaces for command-line applications.  The accompanying
ncursesw library provides wide character support.")
     (license x11)
     (home-page "http://www.gnu.org/software/ncurses/"))))
