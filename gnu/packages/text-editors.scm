;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 José Miguel Sánchez García <jmi2k@openmailbox.org>
;;; Copyright © 2016 Carlo Zancanaro <carlo@zancanaro.id.au>
;;; Copyright © 2017, 2018, 2020 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2017 Feng Shu <tumashu@163.com>
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2014 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.org>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2019, 2020, 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020 Tom Zander <tomz@freedommail.ch>
;;; Copyright © 2020 Mark Meyer <mark@ofosos.org>
;;; Copyright © 2020 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2021 aecepoglu <aecepoglu@fastmail.fm>
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
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages code)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
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
  #:use-module (gnu packages python-web)
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
    (version "0.7")                     ; also update the vis-test input
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.sr.ht/~martanne/vis")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1g05ncsnk57kcqm9wsv6sz8b24kyzj8r5rfpa1wfwj8qkjzx3vji"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-test-suite
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((vis-test (assoc-ref inputs "vis-test")))
               (copy-recursively vis-test "test")
               #t)))
         (delete 'check)                ; the tests need a wrapped vis
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
               #t)))
         (add-after 'wrap-binary 'check
           (assoc-ref %standard-phases 'check))
         (add-before 'check 'set-up-tests
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; DEFAULT_COMPILER is hard-coded here.
               (substitute* "test/core/ccan-config.c"
                 (("\"cc\"")
                  (format #f "\"~a\"" ,(cc-for-target))))

               ;; Use the ‘vis’ executable that we wrapped above.
               (install-file (string-append out "/bin/vis") ".")

               ;; XXX Delete 2 failing tests.  TODO: make them not fail. :-)
               (for-each delete-file
                         (find-files "test/vis/selections" "^complement"))
               #t))))))
    (native-inputs
     `(("vis-test"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://git.sr.ht/~martanne/vis-test")
                 (commit "bbd2f34ff788e87a51a74069069273ad83c44f1f")))
           (sha256
            (base32 "1jsvg2lg3xqfgi79x08kx94mc34mh62ivca10vsci6fqsk68jbd0"))
           (file-name (git-file-name "vis-test" version))))))
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
    (version "2020.09.01")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/mawww/kakoune/"
                           "releases/download/v" version "/"
                           "kakoune-" version ".tar.bz2"))
       (sha256
        (base32 "0x81rxy7bqnhd9374g5ypy4w4nxmm0vnqw6b52bf62jxdg2qj6l6"))))
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
         ;; kakoune requires us to be in the src/ directory to build.
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

(define-public kak-lsp
  (package
    (name "kak-lsp")
    (version "9.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kak-lsp/kak-lsp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256 "1wfv2fy5ga6kc51zka3pak0hq97csm2l11bz74w3n1hrf5q9nnf8")))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-crossbeam-channel" ,rust-crossbeam-channel-0.4)
        ("rust-clap" ,rust-clap-2)
        ("rust-daemonize" ,rust-daemonize-0.4)
        ("rust-dirs" ,rust-dirs-2)
        ("rust-enum_primitive" ,rust-enum-primitive-0.1)
        ("rust-glob" ,rust-glob-0.3)
        ("rust-itertools" ,rust-itertools-0.9)
        ("rust-lsp-types" ,rust-lsp-types-0.80)
        ("rust-jsonrpc-core" ,rust-jsonrpc-core-14)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-rand" ,rust-rand-0.7)
        ("rust-regex" ,rust-regex-1)
        ("rust-ropey" ,rust-ropey-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde_derive" ,rust-serde-derive-1)
        ("rust-serde_json" ,rust-serde-json-1)
        ("rust-slog" ,rust-slog-2)
        ("rust-slog-scope" ,rust-slog-scope-4)
        ("rust-sloggers" ,rust-sloggers-1)
        ("rust-toml" ,rust-toml-0.5)
        ("rust-url" ,rust-url-2)
        ("rust-whoami" ,rust-whoami-0.8))))
    (home-page "https://github.com/kak-lsp/kak-lsp")
    (synopsis "Language Server Protocol (LSP) client for Kakoune")
    (description
     "kak-lsp is a Language Server Protocol client for Kakoune implemented in
Rust.")
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
    (version "1.6.2")
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
               (base32 "10idv2kyw2dg45wfcnh7nybs8qys7kfvif90sjrff3541k97pm5y"))))
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
                      #t))
                  (add-after 'install 'wrap
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      ;; The package needs GTK+ and GtkSourceView on XDG_DATA_DIRS
                      ;; for syntax highlighting to work.  shared-mime-info is
                      ;; necessary for MIME handling.
                      ;; XXX: Ideally we'd reuse glib-or-gtk-wrap here, but it
                      ;; does not pick up $gtksourceview/share/gtksourceview-3.0.
                      (let ((out (assoc-ref outputs "out"))
                            (gtk+ (assoc-ref inputs "gtk+"))
                            (gtksourceview (assoc-ref inputs "gtksourceview"))
                            (shared-mime-info (assoc-ref inputs "shared-mime-info")))
                        (wrap-program (string-append out "/bin/juci")
                          `("XDG_DATA_DIRS" ":" prefix
                            (,(string-join
                               (map (lambda (pkg)
                                      (string-append pkg "/share"))
                                    (list out gtk+ gtksourceview shared-mime-info))
                               ":"))))
                        #t))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("xorg-server" ,xorg-server-for-tests)))
    (inputs
     `(("aspell" ,aspell)
       ("boost" ,boost)
       ("ctags" ,universal-ctags)
       ("gtkmm" ,gtkmm)
       ("gtksourceviewmm" ,gtksourceviewmm)
       ("libclang" ,clang-10)     ;XXX: must be the same version as Mesas LLVM
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
    ;; XXX We use a release candidate to fix incompatibility with Qt 5.15.
    (version "2.0.0-rc4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/wereturtle/ghostwriter")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "07547503a209hc0fcg902w3x0s1m899c10nj3gqz3hak0cmrasi3"))))
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
             (url "https://github.com/olivierkes/manuskript")
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
                 (make-desktop-entry-file (string-append apps "/manuskript.desktop")
                   #:name "Manuskript"
                   #:mime-type "application/x-manuskript-book;"
                   #:exec (string-append out "/bin/manuskript %f")
                   #:comment '((#f "Tool for writers")
                               ("es" "Herramienta para escritores/as"))
                   #:keywords "manuskript;office;write;edit;novel;text;msk"
                   #:terminal #f
                   #:type "Application"
                   #:icon "manuskript"
                   #:categories "Office;WordProcessor;"))
               #t))))))
    (inputs
     `(("pandoc" ,pandoc)
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
    (version "0.12.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/editorconfig/editorconfig-core-c")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1311fhh2jfsja2hhk3nwb6nijlq03jw8dk35cwbrac0p9jvy03jx"))))
    (build-system cmake-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'insert-tests
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((tests (assoc-ref inputs "tests")))
               (copy-recursively tests "tests"))
             #t))
         (add-after 'install 'delete-static-library
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib")))
               (with-directory-excursion lib
                 (delete-file "libeditorconfig_static.a"))
               #t))))))
    (native-inputs
     `(("tests"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/editorconfig/editorconfig-core-test")
                 ;; The tests submodule commit matching this package's version.
                 (commit "48610d43b7455af12195473377f93c4ceea654f5")))
           (file-name (git-file-name "editorconfig-core-test" version))
           (sha256
            (base32 "1s29p4brmcsc3xsww3gk85dg45f1kk3iykh1air3ij0hymf5dyqy"))))))
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
    (version "1.99.19")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.texmacs.org/Download/ftp/tmftp/"
                           "source/TeXmacs-" version "-src.tar.gz"))
       (sha256
        (base32 "1izwqb0z4gqiglv57mjswk6sjivny73kd2sxrf3nmj7wr12pn5m8"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("xdg-utils" ,xdg-utils)))       ;for xdg-icon-resource
    (inputs
     `(("freetype" ,freetype)
       ("guile" ,guile-1.8)
       ("perl" ,perl)
       ("python" ,python-wrapper)
       ("qt" ,qtbase)
       ("qtsvg" ,qtsvg)))
    (arguments
     `(#:tests? #f                      ; no check target
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-icon-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "packages/linux/icons.sh"
                 (("/usr/share")
                  (string-append out "/share")))
               #t)))
         (add-after 'install 'install-desktop-file
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Install desktop file.
             (let* ((out (assoc-ref outputs "out"))
                    (apps (string-append out "/share/applications"))
                    (source "TeXmacs/misc/mime/texmacs.desktop"))
               (install-file source apps)
               #t)))
         (add-before 'configure 'gzip-flags
           (lambda _
             (substitute* "Makefile.in"
               (("^GZIP = gzip -f") "GZIP = gzip -f -n")))))))
    (synopsis "Editing platform with special features for scientists")
    (description
     "GNU TeXmacs is a text editing platform which is specialized for
scientists.  It is ideal for editing structured documents with different types
of content.  It has robust support for mathematical formulas and plots.  It
can also act as an interface to external mathematical programs such as R and
Octave.  TeXmacs is completely extensible via Guile.")
    (license license:gpl3+)
    (home-page "https://www.texmacs.org/tmweb/home/welcome.en.html")))

(define-public scintilla
  (package
    (name "scintilla")
    (version "5.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (let ((v (apply string-append (string-split version #\.))))
              (string-append "https://www.scintilla.org/scintilla" v ".tgz")))
       (sha256
        (base32 "0w5550fijkhmzvdydd8770qq9dgnbq1sd0a8rn4g6mwyfpcyhbfy"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list "GTK3=1" "CC=gcc" "-Cgtk")
       #:tests? #f                      ;require un-packaged Pyside
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ;no configure script
         (replace 'install
           ;; Upstream provides no install script.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (include (string-append out "/include")))
               (for-each (lambda (f) (install-file f lib))
                         (find-files "bin/" "\\.so$"))
               (for-each (lambda (f) (install-file f include))
                         (find-files "include/" "."))
               #t))))))
    (native-inputs
     `(("gcc" ,gcc-9)                   ;Code has C++17 requirements
       ("pkg-config" ,pkg-config)
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
    (version "1.37.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.geany.org/"
                           "geany-" version ".tar.bz2"))
       (sha256
        (base32 "060sachn33xpx3a609f09y97qq5ky17gvv686zbvrn618ij7bi8q"))))
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
       ;; FIXME: Geany bundles a 3.X release of Scintilla.  It is not
       ;; currently possible to replace it with our Scintilla package.
       ;; ("scintilla" ,scintilla)
       ))
    (arguments
     `(#:imported-modules ((guix build glib-or-gtk-build-system)
                           ,@%gnu-build-system-modules)
       #:modules (((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
                  (guix build gnu-build-system)
                  (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'glib-or-gtk-wrap
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)))))
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
    ;; Stable release is 1.9.  However, this development version
    ;; introduces support for UTF-8.
    (version "2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.moria.de/~michael/fe/"
                                  "fe-" version ".tar.gz"))
              (sha256
               (base32
                "10mk5wc3dsdp46b3hkjyd740gcdv6m1gvlr3p8xjxf55b3vfs0la"))))
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
    (description "Fe is a small folding editor.  It folds
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
    (version "3.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/vigna/ne")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "01aglnsfljlvx0wvyvpjfn4y88jf450a06qnj9a8lgdqv1hdkq1a"))))
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
    (home-page "https://ne.di.unimi.it/")
    (synopsis "Text editor with menu bar")
    (description "This package provides a modeless text editor with menu bar.
It supports syntax highlighting, regular expressions, configurable menus,
keybindings, autocomplete and unlimited undo.  It can pipe a marked block
of text through any command line filter.  It can also open very large binary
files.  It was originally developed on the Amiga 3000T.")
    (license license:gpl3+)))

(define-public hexer
  (package
    (name "hexer")
    (version "1.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://devel.ringlet.net/files/editors/hexer/"
                            "hexer-" version ".tar.xz"))
        (sha256
          (base32 "157z17z8qivdin2km2wp86x1bv1nx15frrwcz11mk0l3ab74mf76"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;no upstream tests
       #:make-flags
       (list "CC=gcc"
             (string-append "PREFIX=" (assoc-ref %outputs "out"))
             (string-append "LTERMCAP=-lncurses")
             (string-append "LDFLAGS=-L" (assoc-ref %build-inputs "ncurses")
                            "/lib"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))         ;no configure script
    (inputs
     `(("ncurses" ,ncurses)))
    (home-page "https://devel.ringlet.net/editors/hexer/")
    (synopsis "Multi buffer editor for binary files with vi-like interface")
    (description "Hexer is a multi-buffer editor for binary files for Unix-like
systems that displays its buffer(s) as a hex dump.  The user interface is kept
similar to vi/ex.")
    (license license:bsd-3)))

(define-public virtaal
  (package
    (name "virtaal")
    (version "0.7.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/translate/Virtaal/"
                                  version "/virtaal-" version ".tar.bz2"))
              (sha256
               (base32
                "0cyimjp3191qlmw6n0ipqdr9xr0cq4f6dqvz4rl9q31h6l3kywf9"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:use-setuptools? #f
       #:tests? #f ;; Failing tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Set data file path to absolute store path.
             (substitute* "virtaal/common/pan_app.py"
               (("file_discovery\\.get_abs_data_filename.*")
                (string-append "os.path.join('"
                               (assoc-ref outputs "out")
                               "/share', *path_parts)"))))))))
    (inputs
     `(("python2-lxml" ,python2-lxml)
       ("python2-pygtk" ,python2-pygtk)
       ("python2-simplejson" ,python2-simplejson)
       ("python2-translate-toolkit" ,python2-translate-toolkit)
       ("python2-pycurl" ,python2-pycurl)))
    (synopsis "Graphical translation tool")
    (description "Virtaal is a powerful yet simple translation tool with an
uncluttered user interface.  It supports a multitude of translation formats
provided by the Translate Toolkit, including XLIFF and PO.")
    (home-page "https://virtaal.translatehouse.org/")
    (license license:gpl2+)))
