;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Leo Famulari <leo@famulari.name>
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
        (remove-shebang-phase
         '(lambda _
            ;; To avoid retaining a reference to the bootstrap Bash via the
            ;; shebang of the 'ncursesw5-config' script, simply remove that
            ;; shebang: it'll work just as well without it.
            (substitute* "misc/ncurses-config.in"
              (("#!@SHELL@")
               "# No shebang here, use /bin/sh!\n")
              (("@SHELL@ \\$0")
               "$0"))
            #t))
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
                              (string-append "lib" lib "w.so.6"))
                            (define lib.so.x
                              (string-append "lib" lib ".so.6"))
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
     (version "6.0")
     (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/ncurses/ncurses-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0q3jck7lna77z5r42f13c4xglc7azd19pxfrjrpgp2yf615w4lgm"))))
     (build-system gnu-build-system)
     (arguments
      `(#:configure-flags
        `("--with-shared" "--without-debug" "--enable-widec"

          ;; By default headers land in an `ncursesw' subdir, which is not
          ;; what users expect.
          ,(string-append "--includedir=" (assoc-ref %outputs "out")
                          "/include")
          "--enable-overwrite"                    ;really honor --includedir

          ;; Make sure programs like 'tic', 'reset', and 'clear' have a
          ;; correct RUNPATH.
          ,(string-append "LDFLAGS=-Wl,-rpath=" (assoc-ref %outputs "out")
                          "/lib"))
        #:tests? #f                               ; no "check" target
        #:phases (modify-phases %standard-phases
                   (add-after 'install 'post-install
                              ,post-install-phase)
                   (add-before 'configure 'patch-makefile-SHELL
                               ,patch-makefile-phase)
                   (add-after 'unpack 'remove-unneeded-shebang
                              ,remove-shebang-phase))))
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

(define-public dialog
  (package
    (name "dialog")
    (version "1.2-20150920")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://invisible-mirror.net/archives/dialog/dialog-"
                    version ".tgz"))
              (sha256
               (base32
                "01ccd585c241nkj02n0zdbx8jqhylgcfpcmmshynh0c7fv2ixrn4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f)) ; no test suite
    (inputs
     `(("ncurses" ,ncurses)))
    (synopsis "Curses widgets")
    (description "Dialog is a script-interpreter which provides a set of
curses widgets, such as dialog boxes.")
    (home-page "http://invisible-island.net/dialog/dialog.html")
    ;; Includes the gpl3 file "config.sub" from Automake.
    (license (list lgpl2.1 gpl3))))
