;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2013, 2014, 2015, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Nils Gillmann <ng0@n0.is>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
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
  #:use-module (gnu packages)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (gnu packages)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages linux)
  #:use-module (guix utils))

(define-public ncurses
  (package
    (name "ncurses")
    (version "6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/ncurses/ncurses-"
                                  (car (string-split version #\-))
                                  ".tar.gz"))
              (sha256
               (base32
                "05qdmbmrrn88ii9f66rkcmcyzp1kb1ymkx7g040lfkd1nkp7w1da"))))
    (build-system gnu-build-system)
    (outputs '("out"
               "doc"))                ;1 MiB of man pages
    (arguments
     (let ((patch-makefile-phase
            '(lambda _
               (for-each patch-makefile-SHELL
                         (find-files "." "Makefile.in"))
               #t))
           (configure-phase
            ;; The 'configure' script does not understand '--docdir', so we must
            ;; override that and use '--mandir' instead.
            '(lambda* (#:key build target outputs configure-flags
                       #:allow-other-keys)
               (let ((out (assoc-ref outputs "out"))
                     (doc (assoc-ref outputs "doc")))
                 (apply invoke "./configure"
                        (string-append "SHELL=" (which "sh"))
                        (string-append "--build=" build)
                        (string-append "--prefix=" out)
                        (string-append "--mandir=" doc "/share/man")
                        (if target
                            (cons (string-append "--host=" target)
                                  configure-flags)
                            configure-flags))
                 #t)))
           (apply-rollup-patch-phase
            ;; Ncurses distributes "stable" patchsets to be applied on top
            ;; of the release tarball.  These are only available as shell
            ;; scripts(!) so we decompress and apply them in a phase.
            ;; See <https://invisible-mirror.net/archives/ncurses/6.1/README>.
            '(lambda* (#:key inputs native-inputs #:allow-other-keys)
               (copy-file (assoc-ref (or native-inputs inputs) "rollup-patch")
                          (string-append (getcwd) "/rollup-patch.sh.bz2"))
               (invoke "bzip2" "-d" "rollup-patch.sh.bz2")
               (invoke "sh" "rollup-patch.sh")
               #t))
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
            `(lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 ;; When building a wide-character (Unicode) build, create backward
                 ;; compatibility links from the the "normal" libraries to the
                 ;; wide-character ones (e.g. libncurses.so to libncursesw.so).
                 ,@(if (target-mingw?)
                       '( ;; TODO: create .la files to link to the .dll?
                         (with-directory-excursion (string-append out "/bin")
                           (for-each
                            (lambda (lib)
                              (define lib.dll
                                (string-append "lib" lib ".dll"))
                              (define libw6.dll
                                (string-append "lib" lib "w6.dll"))

                              (when (file-exists? libw6.dll)
                                (format #t "creating symlinks for `lib~a'~%" lib)
                                (symlink libw6.dll lib.dll)))
                            '("curses" "ncurses" "form" "panel" "menu"))))
                       '())
                 (with-directory-excursion (string-append out "/lib")
                   (for-each (lambda (lib)
                               (define libw.a
                                 (string-append "lib" lib "w.a"))
                               (define lib.a
                                 (string-append "lib" lib ".a"))

                               ,@(if (not (target-mingw?))
                                     '((define libw.so.x
                                         (string-append "lib" lib "w.so.6"))
                                       (define lib.so.x
                                         (string-append "lib" lib ".so.6"))
                                       (define lib.so
                                         (string-append "lib" lib ".so"))
                                       (define packagew.pc
                                         (string-append lib "w.pc"))
                                       (define package.pc
                                         (string-append lib ".pc")))
                                     '())

                               (when (file-exists? libw.a)
                                 (format #t "creating symlinks for `lib~a'~%" lib)
                                 (symlink libw.a lib.a)
                                 ,@(if (not (target-mingw?))
                                       '((symlink libw.so.x lib.so.x)
                                         (false-if-exception (delete-file lib.so))
                                         (call-with-output-file lib.so
                                           (lambda (p)
                                             (format p "INPUT (-l~aw)~%" lib)))
                                         (with-directory-excursion "pkgconfig"
                                           (format #t "creating symlink for `~a'~%"
                                                   package.pc)
                                           (when (file-exists? packagew.pc)
                                             (symlink packagew.pc package.pc))))
                                       '())))
                             '("curses" "ncurses" "form" "panel" "menu")))
                 #t))))
       `(#:configure-flags
         ,(cons*
           'quasiquote
           `(("--with-shared" "--without-debug" "--enable-widec"
              
              "--enable-pc-files"
              ,(list 'unquote '(string-append "--with-pkg-config-libdir="
                                              (assoc-ref %outputs "out")
                                              "/lib/pkgconfig"))

              ;; By default headers land in an `ncursesw' subdir, which is not
              ;; what users expect.
              ,(list 'unquote '(string-append "--includedir=" (assoc-ref %outputs "out")
                                              "/include"))
              "--enable-overwrite"      ;really honor --includedir

              ;; Make sure programs like 'tic', 'reset', and 'clear' have a
              ;; correct RUNPATH.
              ,(list 'unquote '(string-append "LDFLAGS=-Wl,-rpath=" (assoc-ref %outputs "out")
                                              "/lib"))

              ;; Starting from ncurses 6.1, "make install" runs "install -s"
              ;; by default, which doesn't work for cross-compiled binaries
              ;; because it invokes 'strip' instead of 'TRIPLET-strip'.  Work
              ;; around this.
              ,@(if (%current-target-system) '("--disable-stripping") '())

              ;; MinGW: Use term-driver created for the MinGW port.
              ,@(if (target-mingw?) '("--enable-term-driver") '()))))
         #:tests? #f                  ; no "check" target
         #:phases (modify-phases %standard-phases
                    (replace 'configure ,configure-phase)
                    (add-after 'install 'post-install
                      ,post-install-phase)
                    (add-before 'configure 'patch-makefile-SHELL
                      ,patch-makefile-phase)
                    (add-after 'unpack 'remove-unneeded-shebang
                      ,remove-shebang-phase)))))
    (self-native-input? #t)           ; for `tic'
    (native-inputs
     `(("pkg-config" ,pkg-config)))
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
    (home-page "https://www.gnu.org/software/ncurses/")))

(define-public ncurses/gpm
  (package/inherit ncurses
    (name "ncurses-with-gpm")
    (arguments
     (substitute-keyword-arguments (package-arguments ncurses)
       ((#:configure-flags cf)
        `(cons (string-append "--with-gpm="
                              (assoc-ref %build-inputs "gpm")
                              "/lib/libgpm.so.2")
               ,cf))))
    (inputs
     `(("gpm" ,gpm)))))

(define-public dialog
  (package
    (name "dialog")
    (version "1.3-20190211")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://invisible-mirror.net/archives/dialog/dialog-"
                    version ".tgz"))
              (sha256
               (base32
                "1lx0bvradzx1zl7znlrsnyljcs596r7wamkhyq37ikbxsy4y5h29"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f))                    ; no test suite
    (inputs
     `(("ncurses" ,ncurses)))
    (synopsis "Curses widgets")
    (description "Dialog is a script-interpreter which provides a set of
curses widgets, such as dialog boxes.")
    (home-page "https://invisible-island.net/dialog/dialog.html")
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
    (home-page "https://metacpan.org/release/Curses")
    (synopsis "Terminal screen handling and optimization")
    (description
     "@code{Curses} is the interface between Perl and the curses library
of your system.")
    (license perl-license)))

(define-public stfl
  (package
    (name "stfl")
    (version "0.24")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://www.clifford.at/stfl/stfl-"
                            version ".tar.gz"))
        (sha256
         (base32
          "1460d5lc780p3q38l3wc9jfr2a7zlyrcra0li65aynj738cam9yl"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; no test target
       #:make-flags (list (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; there is no configure script
         ;; in our ncurses, the headers are in /include
         (add-before 'build 'patch-ncursesw
           (lambda _
             (substitute* "stfl_internals.h"
               (("ncursesw/") ""))
             #t))
         (add-after 'install 'install-missing-symlink
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib")))
               ;; Some programs look for libstfl.so.0.
               (symlink "libstfl.so" (string-append lib "/libstfl.so.0"))))))))
    (inputs `(("ncurses" ,ncurses)))
    (native-inputs `(("swig" ,swig)))
    (home-page "http://www.clifford.at/stfl/")
    (synopsis "Structured terminal forms library")
    (description "Stfl is a library which implements a curses-based widget
set for text terminals.")
    (license lgpl3+)))
