;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 ng0 <ng0@we.make.ritual.n0.is>
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
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (gnu packages)
  #:use-module (gnu packages perl))

(define-public ncurses
  (let ((patch-makefile-phase
         '(lambda _
            (for-each patch-makefile-SHELL
                      (find-files "." "Makefile.in"))))
        (configure-phase
         ;; The 'configure' script does not understand '--docdir', so we must
         ;; override that and use '--mandir' instead.
         '(lambda* (#:key build target outputs configure-flags
                    #:allow-other-keys)
            (let ((out (assoc-ref outputs "out"))
                  (doc (assoc-ref outputs "doc")))
              (zero? (apply system* "./configure"
                            (string-append "SHELL=" (which "sh"))
                            (string-append "--build=" build)
                            (string-append "--prefix=" out)
                            (string-append "--mandir=" doc "/share/man")
                            (if target
                                (cons (string-append "--host=" target)
                                      configure-flags)
                                configure-flags))))))
        (remove-shebang-phase
         '(lambda _
            ;; To avoid retaining a reference to the bootstrap Bash via the
            ;; shebang of the 'ncursesw6-config' script, simply remove that
            ;; shebang: it'll work just as well without it.  Likewise, do not
            ;; retain a reference to the "doc" output.
            (substitute* "misc/ncurses-config.in"
              (("#!@SHELL@")
               "# No shebang here, use /bin/sh!\n")
              (("@SHELL@ \\$0")
               "$0")
              (("mandir=.*$")
               "mandir=share/man"))
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
     (outputs '("out"
                "doc"))                          ;1 MiB of man pages
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
                   (replace 'configure ,configure-phase)
                   (add-after 'install 'post-install
                     ,post-install-phase)
                   (add-before 'configure 'patch-makefile-SHELL
                     ,patch-makefile-phase)
                   (add-after 'unpack 'remove-unneeded-shebang
                     ,remove-shebang-phase))))
     (self-native-input? #t)                      ; for `tic'
     (native-search-paths
      (list (search-path-specification
             (variable "TERMINFO_DIRS")
             (files '("share/terminfo")))))
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

(define-public perl-curses
  (package
    (name "perl-curses")
    (version "1.36")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/G/GI/GIRAFFED/"
                           "Curses-" version ".tar.gz"))
       (sha256
        (base32
         "0r6xd9wr0c25rr28zixhqipak575zqsfb7r7f2693i9il1dpj554"))))
    (build-system perl-build-system)
    (inputs
     `(("ncurses" ,ncurses)))
    (arguments
     `(#:make-maker-flags (list "PANELS" "MENUS")
       #:phases
       (modify-phases %standard-phases
         (add-before
           'configure 'set-curses-ldflags
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((ncurses (assoc-ref inputs "ncurses"))
                    (include (string-append ncurses "/include"))
                    (lib (string-append ncurses "/lib")))
               (setenv "CURSES_LIBTYPE" "ncurses")
               (setenv "CURSES_CFLAGS" (string-append "-I" include))
               (setenv "CURSES_PANEL_CFLAGS" (string-append "-I" include))
               (setenv "CURSES_MENU_CFLAGS" (string-append "-I" include))
               (setenv "CURSES_FORM_CFLAGS" (string-append "-I" include))
               (setenv "CURSES_LDFLAGS" (string-append "-L" lib " -lncurses"))
               (setenv "CURSES_PANEL_LDFLAGS" (string-append "-L" lib " -lpanel"))
               (setenv "CURSES_MENU_LDFLAGS" (string-append "-L" lib " -lmenu"))
               (setenv "CURSES_FORM_LDFLAGS" (string-append "-L" lib " -lform"))
               #t))))))
    (home-page "http://search.cpan.org/dist/Curses")
    (synopsis "Terminal screen handling and optimization")
    (description
     "@code{Curses} is the interface between Perl and the curses library
of your system.")
    (license (package-license perl))))
