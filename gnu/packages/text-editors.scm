;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 José Miguel Sánchez García <jmi2k@openmailbox.org>
;;; Copyright © 2016 Carlo Zancanaro <carlo@zancanaro.id.au>
;;; Copyright © 2017, 2018, 2020 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2017 Feng Shu <tumashu@163.com>
;;; Copyright © 2017 ng0 <ng0@n0.is>
;;; Copyright © 2014 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.org>
;;; Copyright © 2017, 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2019, 2020 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
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

(define-module (gnu packages text-editors)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages regex)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public vis
  (package
    (name "vis")
    (version "0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/martanne/vis/releases"
                                  "/download/v" version
                                  "/vis-v" version ".tar.gz"))
              (sha256
               (base32
                "0aw35n8xk7ir84ckvczc6yshj9ynishrlz0qlv4yc1afbra1gxmn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:tests? #f                  ; no releases; snapshots are missing tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-binary
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lpeg (assoc-ref inputs "lua-lpeg"))
                    (lua-version ,(version-major+minor (package-version lua)))
                    (LUA_PATH (string-append lpeg "/share/lua/"
                                             lua-version "/?.lua"))
                    (LUA_CPATH (string-append lpeg "/lib/lua/"
                                              lua-version "/?.so")))
               (wrap-program (string-append out "/bin/vis")
                 `("LUA_PATH" ":" prefix (,LUA_PATH))
                 `("LUA_CPATH" ":" prefix (,LUA_CPATH)))
               #t))))))
    (native-search-paths
     (list (search-path-specification
            (variable "VIS_PATH")
            (files '("share/vis")))))
    (inputs `(("lua" ,lua)
              ("ncurses" ,ncurses)
              ("libtermkey" ,libtermkey)
              ("lua-lpeg" ,lua-lpeg)
              ("tre" ,tre)))
    (synopsis "Vim-like text editor")
    (description
     "Vis aims to be a modern, legacy free, simple yet efficient vim-like text
editor.  It extends vim's modal editing with built-in support for multiple
cursors/selections and combines it with sam's structural regular expression
based command language.")
    (home-page "https://github.com/martanne/vis")
    (license (list license:isc               ; Main distribution.
                   license:public-domain     ; map.[ch]
                   license:expat))))         ; lexers and libutf.[ch]

(define-public kakoune
  (package
    (name "kakoune")
    (version "2019.12.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/mawww/kakoune/"
                           "releases/download/v" version "/"
                           "kakoune-" version ".tar.bz2"))
       (sha256
        (base32 "1y1gzax2dl7flh676k0rl1vacv10j7p5krkmb67b0afbrql8vbb6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             ;; kakoune uses confstr with _CS_PATH to find out where to find
             ;; a posix shell, but this doesn't work in the build
             ;; environment. This substitution just replaces that result
             ;; with the "sh" path.
             (substitute* "src/shell_manager.cc"
               (("if \\(m_shell.empty\\(\\)\\)" line)
                (string-append "m_shell = \"" (which "sh")
                               "\";\n        " line)))
             #t))
         (delete 'configure)            ; no configure script
         ;; kakoune requires us to be in the src/ directory to build
         (add-before 'build 'chdir
           (lambda _ (chdir "src") #t)))))
    (native-inputs
     `(("asciidoc" ,asciidoc)
       ("pkg-config" ,pkg-config)
       ("ruby" ,ruby)))
    (inputs
     `(("ncurses" ,ncurses)))
    (synopsis "Vim-inspired code editor")
    (description
     "Kakoune is a code editor heavily inspired by Vim, as such most of its
commands are similar to Vi's ones, and it shares Vi's \"keystrokes as a text
editing language\" model.  Kakoune has a strong focus on interactivity, most
commands provide immediate and incremental results, while still being
competitive (as in keystroke count) with Vim.")
    (home-page "https://kakoune.org/")
    (license license:unlicense)))

(define-public joe
  (package
    (name "joe")
    (version "4.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://sourceforge.net/projects/joe-editor/"
                           "files/JOE sources/joe-" version "/"
                           "joe-" version ".tar.gz"))
       (sha256
        (base32
         "1pmr598xxxm9j9dl93kq4dv36zyw0q2dh6d7x07hf134y9hhlnj9"))))
    (build-system gnu-build-system)
    (inputs `(("ncurses" ,ncurses)))
    (home-page "http://joe-editor.sourceforge.net/")
    (synopsis "Console screen editor")
    (description
     "JOE is a powerful console screen editor with a \"mode-less\" user
interface similar to many user-friendly editors.  JOE has some of the key
bindings and many of the powerful features of GNU Emacs.")
    (license license:gpl3+)))

(define-public jucipp
  (package
    (name "jucipp")
    (version "1.5.1")
    (home-page "https://gitlab.com/cppit/jucipp")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))
                                  ;; Two submodules are required which are
                                  ;; developed alongside JuCi++ and difficult
                                  ;; to package separately.
                                  (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0v7fmsya2zn1xx59bkv4cbyinmcnv52hm4j40nbfwalcks631xrr"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DBUILD_TESTING=ON"

                           ;; These arguments are here to facilitate an "in-source"
                           ;; build using "./build" instead of the default "../build".
                           ;; The test suite expects that to be the case.
                           "..")
       #:out-of-source? #f
       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'enter-build-directory
                    (lambda _
                      (mkdir "build")
                      (chdir "build")
                      #t))

                  ;; This phase is necessary to fix a test failure, see
                  ;; <https://gitlab.com/cppit/jucipp/-/issues/423>.
                  (add-after 'unpack 'add-reference-to-clang-internal-header
                    (lambda* (#:key inputs #:allow-other-keys)
                      (substitute* "src/compile_commands.cc"
                        ((".*-I/usr/lib/clang.*" all)
                         (string-append "arguments.emplace_back(\"-I"
                                        (assoc-ref inputs "libclang")
                                        "/lib/clang/"
                                        ,@(list (package-version clang))
                                        "/include\");\n"
                                        all)))
                      #t))
                  (add-after 'unpack 'patch-tiny-process-library
                    (lambda _
                      (with-directory-excursion "lib/tiny-process-library"
                        (substitute* '("process_unix.cpp"
                                       "tests/io_test.cpp")
                          (("/bin/sh") (which "sh"))))
                      #t))
                  (add-after 'unpack 'disable-git-test
                    (lambda _
                      (substitute* "tests/CMakeLists.txt"
                        ;; Disable the git test, as it requires the full checkout.
                        (("add_test\\(git_test.*\\)") ""))
                      #t))
                  (add-before 'check 'pre-check
                    (lambda* (#:key inputs #:allow-other-keys)
                      ;; Tests do not expect HOME to be empty.
                      (setenv "HOME" "/etc")

                      ;; Most tests require an X server.
                      (let ((xorg-server (assoc-ref inputs "xorg-server"))
                            (display ":1"))
                        (setenv "DISPLAY" display)
                        (system (string-append xorg-server "/bin/Xvfb "
                                               display " &")))
                      #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("xorg-server" ,xorg-server-for-tests)))
    (inputs
     `(("aspell" ,aspell)
       ("boost" ,boost)
       ("gtkmm" ,gtkmm)
       ("gtksourceviewmm" ,gtksourceviewmm)
       ("libclang" ,clang)
       ("libgit2" ,libgit2)))
    (synopsis "Lightweight C++ IDE")
    (description
     "juCi++ is a small @dfn{IDE} (Integrated Development Environment)
designed especially towards libclang with speed, stability, and ease of use
in mind.

It supports autocompletion, on-the-fly warnings and errors, syntax
highlighting, and integrates with Git as well as the CMake and Meson build
systems.")
    (license license:expat)))

(define-public leafpad
  (package
    (name "leafpad")
    (version "0.8.18.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/"
                                  "leafpad/leafpad-" version ".tar.gz"))
              (sha256
               (base32
                "0b0az2wvqgvam7w0ns1j8xp2llslm1rx6h7zcsy06a7j0yp257cm"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gtk+" ,gtk+-2)))
    (home-page "http://tarot.freeshell.org/leafpad/")
    (synopsis "GTK+ based text editor")
    (description "Leafpad is a GTK+ text editor that emphasizes simplicity.  As
development focuses on keeping weight down to a minimum, only the most essential
features are implemented in the editor.  Leafpad is simple to use, is easily
compiled, requires few libraries, and starts up quickly. ")
    (license license:gpl2+)))

(define-public e3
  (package
    (name "e3")
    (version "2.82")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://sites.google.com/site/e3editor/Home/"
                                  "e3-" version ".tgz"))
              (sha256
               (base32
                "0919kadkas020maqq37852isnzp053q2fnws2zh3mz81d1jiviak"))
              (modules '((guix build utils)))

              ;; Remove pre-built binaries.
              (snippet '(begin
                          (delete-file-recursively "bin")
                          #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags (list (string-append "PREFIX="
                                         (assoc-ref %outputs "out")))
       #:phases (modify-phases %standard-phases
                  (delete 'configure))))
    (native-inputs
     `(("nasm" ,nasm)))
    (home-page "https://sites.google.com/site/e3editor/")
    (synopsis "Tiny text editor written in assembly")
    (description
     "e3 is a micro text editor with an executable code size between 3800 and
35000 bytes.  Except for ``syntax highlighting'', the e3 binary supports all
of the basic functions one expects plus built in arithmetic calculations.
UTF-8 coding of unicode characters is supported as well.  e3 can use
Wordstar-, EMACS-, Pico, Nedit or vi-like key bindings.  e3 can be used on
16, 32, and 64-bit CPUs.")
    (supported-systems '("x86_64-linux" "i686-linux"))
    (license license:gpl2+)))

(define-public mg
  (package
    (name "mg")
    (version "20180927")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hboetes/mg")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "14vrm8lvwksf697sqks7xfd1xaqjlqjc9afjk33sksq5p27wr203"))
              (modules '((guix build utils)))
              (snippet '(begin
                          (substitute* "GNUmakefile"
                            (("/usr/bin/") ""))
                          #t))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libbsd" ,libbsd)
       ("ncurses" ,ncurses)))
    (arguments
     ;; No test suite available.
     '(#:tests? #f
       #:make-flags (list (string-append "prefix=" %output)
                          "CC=gcc")
       #:phases (modify-phases %standard-phases
                  (delete 'configure)   ; no configure script
                  (add-before 'build 'correct-location-of-difftool
                    (lambda _
                      (substitute* "buffer.c"
                        (("/usr/bin/diff")
                         (which "diff")))
                      #t))
                  (add-before 'install 'patch-tutorial-location
                    (lambda* (#:key outputs #:allow-other-keys)
                      (substitute* "mg.1"
                        (("/usr") (assoc-ref outputs "out")))
                      #t))
                  (add-after 'install 'install-tutorial
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (doc (string-append out "/share/doc/mg")))
                        (install-file "tutorial" doc)
                        #t))))))
    (home-page "https://homepage.boetes.org/software/mg/")
    (synopsis "Microscopic GNU Emacs clone")
    (description
     "Mg (@command{mg}) is a GNU Emacs style editor, with which it is
\"broadly\" compatible.  This is a portable version of the mg maintained by the
OpenBSD team.")
    (license license:public-domain)))

(define-public qemacs
  (package
    (name "qemacs")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://bellard.org/qemacs/"
                           "qemacs-" version ".tar.gz"))
       (sha256
        (base32 "156z4wpj49i6j388yjird5qvrph7hz0grb4r44l4jf3q8imadyrg"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;no test
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'build-qhtml
           ;; Build fails without first creating qHTML library.
           (lambda _ (invoke "make" "-C" "libqhtml")))
         (add-before 'install 'fix-man-pages-directory
           ;; Install in $out/share/man instead of $out/man.
           (lambda _
             (substitute* "Makefile"
               (("/man/man1" all) (string-append "/share" all)))
             #t))
         (add-before 'install 'create-directories
           ;; Ensure directories exist before installing files.
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each (lambda (d) (mkdir-p (string-append out d)))
                         '("/bin" "/share/man/man1" "/share/qe"))
               #t)))
         (add-after 'install 'install-extra-documentation
           ;; Install sample configuration file, Info, and HTML manual.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((share (string-append (assoc-ref outputs "out") "/share"))
                    (doc (string-append share "/doc/" ,name "-" ,version))
                    (html (string-append share "/html"))
                    (info (string-append share "/info"))
                    (makeinfo (string-append (assoc-ref %build-inputs "texinfo")
                                             "/bin/makeinfo")))
               ;; First fix Texinfo documentation, create appropriate
               ;; directories, then generate Info and HTML files there.
               (substitute* "qe-doc.texi"
                 (("^M-([{}])" _ bracket) (string-append "M-@" bracket)))
               (for-each (lambda (d) (mkdir-p d)) (list html info))
               (invoke makeinfo "qe-doc.texi" "-o" info)
               (invoke makeinfo "qe-doc.texi" "--html" "--no-split" "-o" html)
               ;; Install sample configuration file.
               (install-file "config.eg" doc)
               #t))))))
    (native-inputs
     `(("texinfo" ,texinfo)))
    (inputs
     `(("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxv" ,libxv)))
    (home-page "https://bellard.org/qemacs/")
    (synopsis "Small but powerful text editor")
    (description "QEmacs (for Quick Emacs) is a very small but
powerful editor.  It has features that even big editors lack:

@itemize

@item Full screen editor with an Emacs look and feel with all Emacs
common features: multi-buffer, multi-window, command mode, universal
argument, keyboard macros, config file with C-like syntax, minibuffer
with completion and history.

@item Can edit files of hundreds of Megabytes without being slow by
using a highly optimized internal representation and by mmaping the
file.

@item Full Unicode support, including multi charset handling (8859-x,
UTF8, SJIS, EUC-JP, ...) and bidirectional editing respecting the
Unicode bidi algorithm.  Arabic and Indic scripts handling (in
progress).

@item WYSIWYG HTML/XML/CSS2 mode graphical editing.  Also supports
Lynx like rendering on VT100 terminals.

@item WYSIWYG DocBook mode based on XML/CSS2 renderer.

@item C mode: coloring with immediate update.  Emacs like auto-indent.

@item Shell mode: colorized VT100 emulation so that your shell work
exactly as you expect.  Compile mode with next/prev error.

@item Input methods for most languages, including Chinese (input
methods come from the Yudit editor).

@item Hexadecimal editing mode with insertion and block commands.
Unicode hexa editing is also supported.

@item Works on any VT100 terminals without termcap.  UTF8 VT100
support included with double width glyphs.

@item X11 support.  Support multiple proportional fonts at the same
time (as XEmacs).  X Input methods supported.  Xft extension supported
for anti aliased font display.

@item Small! Full version (including HTML/XML/CSS2/DocBook rendering
and all charsets): 200KB big.  Basic version (without bidir/unicode
scripts/input/X11/C/Shell/HTML/Dired): 49KB.
@end itemize")
    (license license:lgpl2.1+)))

(define-public ghostwriter
  (package
    (name "ghostwriter")
    (version "1.8.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/wereturtle/ghostwriter.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "13yn82m1l2pq93wbl569a2lzpc3sn8a8g30hsgdch1l9xlmhwran"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("qttools" ,qttools)))           ; for lrelease
    (inputs
     `(("hunspell" ,hunspell)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qtmultimedia" ,qtmultimedia)
       ("qtquickcontrols" ,qtquickcontrols)
       ("qtsvg" ,qtsvg)
       ("qtwebchannel" ,qtwebchannel)))
    (propagated-inputs                  ; To get native-search-path
     `(("qtwebengine" ,qtwebengine)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke "qmake" (string-append "PREFIX=" out)))))
         (add-after 'configure 'create-translations
           (lambda _
             ;; `lrelease` will not overwrite, so delete existing .qm files
             (for-each delete-file (find-files "translations" ".*\\.qm"))
             (apply invoke "lrelease" (find-files "translations" ".*\\.ts"))))
         ;; Ensure that icons are found at runtime.
         (add-after 'install 'wrap-executable
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/ghostwriter")
                 `("QT_PLUGIN_PATH" ":" prefix
                   ,(map (lambda (label)
                           (string-append (assoc-ref inputs label)
                                          "/lib/qt5/plugins/"))
                         '("qtsvg" "qtmultimedia"))))))))))
    (home-page "https://wereturtle.github.io/ghostwriter/")
    (synopsis "Write without distractions")
    (description
     "@code{ghostwriter} provides a relaxing, distraction-free writing
environment with Markdown markup.")
    (license license:gpl3+)))           ; icons/* under CC-BY-SA3

(define-public manuskript
  (package
    (name "manuskript")
    (version "0.11.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/olivierkes/manuskript.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1l6l9k6k69yv8xqpll0zv9cwdqqg4zvxy90l6sx5nv2yywh5crla"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f                      ;no test
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share/manuskript")))
               ;; Install data.
               (mkdir-p share)
               (for-each
                (lambda (d)
                  (let ((destination  (string-append share "/" d)))
                    (mkdir-p destination)
                    (copy-recursively d destination)))
                '("bin" "i18n" "icons" "libs" "manuskript" "resources"))
               ;; Install documentation.
               (let ((doc (string-append out
                                         "/doc/manuskript-" ,version
                                         "/sample-projects")))
                 (mkdir-p doc)
                 (copy-recursively "sample-projects" doc))
               ;; Wrap executable in "$out/share/manuskript/bin" and
               ;; link to it from "$out/bin".
               (let ((bin (string-append out "/bin"))
                     (executable (string-append share "/bin/manuskript")))
                 (wrap-program executable
                   (list "PYTHONPATH" 'prefix (list (getenv "PYTHONPATH"))))
                 (mkdir-p bin)
                 (with-directory-excursion bin
                   (symlink (string-append share "/bin/manuskript")
                            "manuskript")))
               ;; Install icons and create .desktop file.
               (let ((apps (string-append out "/share/applications"))
                     (icons-dir (string-append out "/share/pixmaps")))
                 (install-file "icons/Manuskript/manuskript.svg" icons-dir)
                 (mkdir-p apps)
                 (with-output-to-file (string-append apps "/manuskript.desktop")
                   (lambda _
                     (format #t
                             "[Desktop Entry]~@
                         Name=Manuskript~@
                         MimeType=application/x-manuskript-book;~@
                         Exec=~a/bin/manuskript %f~@
                         Comment=Tool for writers~@
                         Comment[es]=Herramienta para escritores/as~@
                         Keywords=manuskript;office;write;edit;novel;text;msk~@
                         Terminal=false~@
                         Type=Application~@
                         Icon=manuskript~@
                         Categories=Office;WordProcessor;~%"
                             out))))
               #t))))))
    (inputs
     `(("ghc-pandoc" ,ghc-pandoc)
       ("python-lxml" ,python-lxml)
       ("python-markdown" ,python-markdown)
       ("python-pyqt" ,python-pyqt)
       ("qtsvg" ,qtsvg)))
    (home-page "http://www.theologeek.ch/manuskript/")
    (synopsis "Tool for writers")
    (description "Manuskript provides a rich environment to help
writers create their first draft and then further refine and edit
their masterpiece.  With Manuskript you can:

@itemize
@item Grow your premise from one sentence, to a paragraph, to a full
summary,
@item Create characters,
@item Conceive plots,
@item Construct outlines (Outline mode and/or Index cards),
@item Write with focus (Distraction free mode),
@item Build worlds,
@item Track items,
@item Edit and re-organize chapters and scenes,
@item View Story line,
@item Compose with fiction or non-fiction templates and writing modes,
@item Import and export document formats such as HTML, ePub,
OpenDocument, DocX, and more.
@end itemize

Additionally Manuskript can help in many more ways with a spell
checker, markdown highlighter, frequency analyzer, and automatic save
in plain text file format.")
    (license license:gpl3+)))

(define-public editorconfig-core-c
  (package
    (name "editorconfig-core-c")
    (version "0.12.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/editorconfig/editorconfig-core-c.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0jkc69r4jwn4rih6h6cqvgljjc3ff49cxj8286mi515aczr48cm1"))))
    (build-system cmake-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'insert-tests
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((tests (assoc-ref inputs "tests")))
               (copy-recursively tests "tests"))
             #t)))))
    (native-inputs
     `(("tests" ,(origin
                   (method git-fetch)
                   (uri (git-reference
                          (url "https://github.com/editorconfig/editorconfig-core-test")
                          (commit "6ea1d8ece62cac9cf72c79dce4879b046abe1fe7"))) ; matches version
                   (file-name (git-file-name "editorconfig-core-test" version))
                   (sha256
                    (base32
                     "1sf6910idnd4bgzbj8w8f9ldsbkaqa0lh6syymwy3hfqda63acj7"))))))
    (inputs
     `(("pcre2" ,pcre2)))
    (home-page "https://editorconfig.org/")
    (synopsis "EditorConfig core library written in C")
    (description "EditorConfig makes it easy to maintain the correct coding
style when switching between different text editors and between different
projects.  The EditorConfig project maintains a file format and plugins for
various text editors which allow this file format to be read and used by those
editors.")
    (license license:bsd-2)))

(define-public texmacs
  (package
    (name "texmacs")
    (version "1.99.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.texmacs.org/Download/ftp/tmftp/"
                           "source/TeXmacs-" version "-src.tar.gz"))
       (sha256
        (base32 "12bp0f34izzqimz49lfpgf4lyz3h45s9xbmk8v6zsawdjki76alg"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (delete-file-recursively "3rdparty")
           #t))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("freetype" ,freetype)
       ("guile" ,guile-1.8)
       ("perl" ,perl)
       ("python" ,python-wrapper)
       ("qt" ,qt-4)))
    (arguments
     `(#:tests? #f                      ; no check target
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'gzip-flags
           (lambda _
             (substitute* "Makefile.in"
               (("^GZIP = gzip -f") "GZIP = gzip -f -n")))))))
    (synopsis "Editing platform with special features for scientists")
    (description
     "GNU TeXmacs is a text editing platform which is specialized for
scientists.  It is ideal for editing structured documents with different
types of content.  It has robust support for mathematical formulas and plots.
 It can also act as an interface to external mathematical programs such as R
and Octave.  TeXmacs is completely extensible via Guile.")
    (license license:gpl3+)
    (home-page "https://www.texmacs.org/tmweb/home/welcome.en.html")))

(define-public scintilla
  (package
    (name "scintilla")
    (version "4.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (let ((v (apply string-append (string-split version #\.))))
              (string-append "https://www.scintilla.org/scintilla" v ".tgz")))
       (sha256
        (base32 "0d8ssl0d8r6bslbzd507l9c5g8mwn1zriak3fnf85936pdmkhq9h"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list "GTK3=1" "CC=gcc" "-Cgtk")
       #:tests? #f                      ;require un-packaged Pyside
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ;no configure script
         (add-after 'unpack 'build-shared-library
           (lambda _
             (substitute* "gtk/makefile"
               (("scintilla\\.a") "libscintilla.so")
               (("\\$\\(AR\\) \\$\\(ARFLAGS\\) \\$@ \\$\\^")
                "$(CC) -shared $^ -o $@")
               (("\\$\\(RANLIB\\) \\$@") ""))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (include (string-append out "/include")))
               (install-file "bin/libscintilla.so" lib)
               (for-each (lambda (f) (install-file f include))
                         (find-files "include/" "."))
               #t))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)))
    (inputs
     `(("gtk+" ,gtk+)))
    (home-page "https://www.scintilla.org/")
    (synopsis "Code editor for GTK+")
    (description "Scintilla is a source code editing component for
GTK+.  It has the usual features found in text editing components, as
well as some that are especially useful for editing and debugging
source code; these include support for syntax styling, error
indicators, code completion and call tips.  Styling choices are more
open than with many editors: Scintilla lets you use proportional
fonts, bold and italics, multiple foreground and background colours,
and multiple fonts.")
    (license license:hpnd)))

(define-public geany
  (package
    (name "geany")
    (version "1.36")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.geany.org/"
                                  "geany-" version ".tar.bz2"))
              (sha256
               (base32
                "0gnm17cr4rf3pmkf0axz4a0fxwnvp55ji0q0lzy88yqbshyxv14i"))
              (modules '((guix build utils)))
              (snippet '(begin
                          (delete-file-recursively "scintilla")
                          #t))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("doxygen" ,doxygen)
       ("glib" ,glib "bin")
       ("intltool" ,intltool)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("python-docutils" ,python-docutils))) ;for rst2html
    (inputs
     `(("gtk+" ,gtk+)
       ("scintilla" ,scintilla)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'use-scintilla-shared-library
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "configure.ac"
               (("scintilla/Makefile") "")
               (("scintilla/include/Makefile") ""))
             (substitute* "Makefile.am"
               (("scintilla ") ""))
             (substitute* "src/Makefile.am"
               (("\\$\\(top_builddir\\)/scintilla/libscintilla.la") "")
               (("geany_LDFLAGS =" all) (string-append all " -lscintilla")))
             (substitute* "doc/Makefile.am"
               (("\\$\\(INSTALL_DATA\\) \\$\\(top_srcdir\\)/scintilla/License.txt \\$\\(DOCDIR\\)/ScintillaLicense.txt") ""))
             (substitute* "tests/Makefile.am"
               (("AM_LDFLAGS =" all) (string-append all " -lscintilla")))
             (for-each delete-file (list "autogen.sh" "configure" "Makefile.in"))
             #t)))))
    (home-page "https://www.geany.org")
    (synopsis "Fast and lightweight IDE")
    (description "Geany is a small and fast Integrated Development
Environment (IDE) that only has a few dependencies on other packages and is as
independent as possible from special desktop environments like KDE or GNOME.

The basic features of Geany are:
@itemize
@item syntax highlighting
@item code completion
@item auto completion of often constructed constructs like if, for and while
@item auto completion of XML and HTML tags
@item call tips
@item folding
@item many supported filetypes like C, Java, PHP, HTML, Python, Perl, Pascal
@item symbol lists
@item embedded terminal emulation
@item extensibility through plugins
@end itemize")
    (license license:gpl2+)))

(define-public fe
  (package
    (name "fe")
    ;; Stable release is 1.8.  However, this development version
    ;; introduces support for UTF-8.
    (version "2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.moria.de/~michael/fe/"
                                  "fe-" version ".tar.gz"))
              (sha256
               (base32
                "1hwws7si1752z6hp61zxznvgsb6846lp8zl1hn5ddhsbafwalwb9"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;no test
       ;; Sendmail is only used to send a crash log.  Disable the
       ;; feature since it is (1) undocumented (2) not very useful.
       #:configure-flags (list "--disable-sendmail")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/" ,name "-" ,version)))
               (for-each (lambda (f) (install-file f doc))
                         '("fe.doc" "fe.html" "fe.ps" "feref.ps" "README"))
               #t))))))
    (native-inputs
     `(("gettext" ,gettext-minimal)))
    (inputs
     `(("ncurses" ,ncurses)))
    (home-page "http://www.moria.de/~michael/fe/")
    (synopsis "Small folding editor")
    (description "Fe is a small folding editor.  It allows to fold
arbitrary text regions; it is not bound to syntactic units.

Fe has no configuration or extension language and requires no setup.
Its user interface is emacs-like and it has menus for the very most
important functions to help beginners.  Further there is a reference
card.  It offers:

@itemize
@item Regions and Emacs-like kill ring
@item Incremental search
@item Keyboard macros
@item Editing binary files
@item Multiple windows and views
@item Compose function for Latin 1 characters
@end itemize")
    (license license:gpl2+)))

(define-public ne
  (package
    (name "ne")
    (version "3.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/vigna/ne.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0h6d08cnwrk96ss83i9bragwwanph6x54sm3ak1z81146dsqsiif"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("perl" ,perl)
       ("texinfo" ,texinfo)))
    (inputs
     `(("ncurses" ,ncurses)))
    (arguments
     `(#:tests? #f
       #:make-flags
       (list "CC=gcc"
             (string-append "PREFIX=" (assoc-ref %outputs "out"))
             (string-append "LDFLAGS=-L" (assoc-ref %build-inputs "ncurses")
                            "/lib"))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             (substitute* "src/makefile"
              (("-lcurses") "-lncurses"))
             #t)))))
    (home-page "http://ne.di.unimi.it/")
    (synopsis "Text editor with menu bar")
    (description "This package provides a modeless text editor with menu bar.
It supports syntax highlighting, regular expressions, configurable menus,
keybindings, autocomplete and unlimited undo.  It can pipe a marked block
of text through any command line filter.  It can also open very large binary
files.  It was originally developed on the Amiga 3000T.")
    (license license:gpl3+)))
