;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2016, 2017, 2018, 2019, 2020, 2021, 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 HiPhish <hiphish@posteo.de>
;;; Copyright © 2019 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2019, 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020, 2021 Jack Hill <jackhill@jackhill.us>
;;; Copyright © 2021 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2021 Tissevert <tissevert+guix@marvid.fr>
;;; Copyright © 2021 Foo Chuan Wei <chuanwei.foo@hotmail.com>
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

(define-module (gnu packages vim)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages admin) ; For GNU hostname
  #:use-module (gnu packages attr)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages code)
  #:use-module (gnu packages enlightenment)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages text-editors)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

(define-public vim
  (package
    (name "vim")
    (version "8.2.3995")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                    (url "https://github.com/vim/vim")
                    (commit (string-append "v" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "1aqrywyry4vxf1x7mk5g1k5k6md38bnjb6f778hmk8ahx26mpqpb"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:parallel-tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'patch-absolute-paths
           (lambda _
             (substitute* "runtime/tools/mve.awk"
               (("/usr/bin/nawk") (which "gawk")))
             (substitute* '("src/testdir/Makefile"
                            "src/testdir/test_normal.vim"
                            "src/testdir/test_popupwin.vim"
                            "src/testdir/test_shell.vim"
                            "src/testdir/test_system.vim"
                            "src/testdir/test_terminal.vim"
                            "src/testdir/test_terminal2.vim")
               (("/bin/sh") (which "sh")))
             (substitute* "src/testdir/test_autocmd.vim"
               (("/bin/kill") (which "kill")))))
         (add-before 'check 'set-environment-variables
           (lambda* (#:key inputs #:allow-other-keys)
             ;; One of the tests tests timezone-dependent functions.
             (setenv "TZDIR"
                     (search-input-directory inputs "share/zoneinfo"))

             ;; Make sure the TERM environment variable is set for the tests
             (setenv "TERM" "xterm")))
         (add-before 'check 'skip-or-fix-failing-tests
           (lambda _
             ;; This test assumes that PID 1 is run as root and that the user
             ;; running the test suite does not have permission to kill(1, 0)
             ;; it.  This is not true in the build container, where both PID 1
             ;; and the test suite are run as the same user.  Skip the test.
             ;; An alternative fix would be to patch the PID used to a random
             ;; 32-bit value and hope it never shows up in the test environment.
             (substitute* "src/testdir/test_swap.vim"
               (("if !IsRoot\\(\\)") "if 0"))

             ;; These tests check how the terminal looks after executing some
             ;; actions.  The path of the bash binary is shown, which results in
             ;; a difference being detected.  Patching the expected result is
             ;; non-trivial due to the special format used, so skip the test.
             (substitute* "src/testdir/test_terminal.vim"
               ((".*Test_open_term_from_cmd.*" line)
                (string-append line "return\n"))
               ((".*Test_terminal_postponed_scrollback.*" line)
                (string-append line "return\n"))
               ((".*Test_combining_double_width.*" line)
                (string-append line "return\n")))
             (substitute* "src/testdir/test_popupwin.vim"
               ((".*Test_popup_drag_termwin.*" line)
                (string-append line "return\n")))))
         (add-before 'install 'fix-installman.sh
           (lambda _
             (substitute* "src/installman.sh"
               (("/bin/sh")
                (which "sh")))))
         (add-after 'install 'install-guix.vim
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((vimdir (string-append (assoc-ref outputs "out") "/share/vim")))
               (mkdir-p vimdir)
               (copy-file (assoc-ref inputs "guix.vim")
                          (string-append vimdir "/vimrc"))))))))
    (inputs
     (list gawk ncurses perl tcsh))                 ; For runtime/tools/vim32
    (native-inputs
     `(("libtool" ,libtool)
       ("guix.vim" ,(search-auxiliary-file "guix.vim"))

       ;; For tests.
       ("tzdata" ,tzdata-for-tests)))
    (home-page "https://www.vim.org/")
    (synopsis "Text editor based on vi")
    ;; The description shares language with the vim-full package. When making
    ;; changes, check if the other description also needs to be updated.
    (description
     "Vim is a highly configurable text editor built to enable efficient text
editing.  It is an improved version of the vi editor distributed with most UNIX
systems.

Vim is often called a \"programmer's editor,\" and so useful for programming
that many consider it an entire IDE.  It's not just for programmers, though.
Vim is perfect for all kinds of text editing, from composing email to editing
configuration files.")
    (license license:vim)))

(define-public xxd
  (package (inherit vim)
    (name "xxd")
    (arguments
     (list
       #:make-flags #~(list (string-append "CC=" #$(cc-for-target)))
       #:tests? #f ; there are none
       #:phases
       #~(modify-phases %standard-phases
           (delete 'configure)
           (add-after 'unpack 'chdir
             (lambda _
               (chdir "src/xxd")))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
                 (install-file "xxd" bin)))))))
    (inputs `())
    (native-inputs `())
    (synopsis "Hexdump utility from vim")
    (description "This package provides the Hexdump utility xxd that comes
with the editor vim.")))

(define-public vim-full
  (package
    (inherit vim)
    (name "vim-full")
    (arguments
     `(#:configure-flags
       (list (string-append "--with-lua-prefix="
                            (assoc-ref %build-inputs "lua"))
             "--with-features=huge"
             "--enable-python3interp=yes"
             "--enable-perlinterp=yes"
             "--enable-rubyinterp=yes"
             "--enable-tclinterp=yes"
             "--enable-luainterp=yes"
             "--enable-cscope"
             "--enable-sniff"
             "--enable-multibyte"
             "--enable-xim"
             "--disable-selinux"
             "--enable-gui")
       ,@(substitute-keyword-arguments (package-arguments vim)
           ;; This flag fixes the following error:
           ;; .../libpython3.7m.a(pyexpat.o): undefined reference to symbol 'XML_FreeContentModel'
           ;; .../libexpat.so.1: error adding symbols: DSO missing from command line
           ((#:make-flags flags)
            `(append
              (list "LDFLAGS=-lexpat")
              ,flags))
           ((#:phases phases)
            `(modify-phases ,phases
               (add-before 'check 'start-xserver
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; Some tests require an X server, but does not start one.
                   (let ((xorg-server (assoc-ref inputs "xorg-server"))
                         (display ":1"))
                     (setenv "DISPLAY" display)
                     (zero? (system (string-append xorg-server "/bin/Xvfb "
                                                    display " &")))))))))))
    (native-inputs
     (modify-inputs (package-native-inputs vim)
       (prepend pkg-config xorg-server-for-tests)))
    (inputs
     `(("acl" ,acl)
       ("atk" ,atk)
       ("attr" ,attr)
       ("cairo" ,cairo)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("gettext" ,gettext-minimal)
       ("glib" ,glib)
       ("gpm" ,gpm)
       ("gtk" ,gtk+-2)
       ("harfbuzz" ,harfbuzz)
       ("libice" ,libice)
       ("libpng" ,libpng)
       ("libsm" ,libsm)
       ("libx11" ,libx11)
       ("libxdmcp" ,libxdmcp)
       ("libxt" ,libxt)
       ("libxpm" ,libxpm)
       ("lua" ,lua)
       ("pango" ,pango)
       ("pixman" ,pixman)
       ("python-3" ,python)
       ("ruby" ,ruby)
       ("tcl" ,tcl)
       ,@(package-inputs vim)))
    ;; The description shares language with the vim package. When making
    ;; changes, check if the other description also needs to be updated.
    (description "Vim is a highly configurable text editor built to enable efficient text
editing.  It is an improved version of the vi editor distributed with most UNIX
systems.

Vim is often called a \"programmer's editor,\" and so useful for programming
that many consider it an entire IDE.  It's not just for programmers, though.
Vim is perfect for all kinds of text editing, from composing email to editing
configuration files.

This package provides a version of Vim with many optional features enabled.
It includes a graphical interface, @command{gvim}, and support for plugins
written in the Python 3, Perl, Ruby, Tcl, and Lua programming languages.")))

(define-public vim-neocomplete
  (package
    (name "vim-neocomplete")
    (version "2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/Shougo/neocomplete.vim")
              (commit (string-append "ver." version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1h6sci5mhdfg6sjsjpi8l5li02hg858zcayiwl60y9j2gqnd18lv"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("autoload" "share/vim/vimfiles/")
         ("doc" "share/vim/vimfiles/")
         ("plugin" "share/vim/vimfiles/"))))
    (synopsis "Next generation completion framework for Vim")
    (description
     "@code{neocomplete}, an abbreviation of 'neo-completion with cache',
is a plugin for Vim.
It provides keyword completion system by maintaining a cache of keywords in
the current buffer.  Neocomplete can be customized easily and has many more
features than Vim's built-in completion.")
    (home-page "https://github.com/Shougo/neocomplete.vim/")
    (license license:expat)))

;; There are no release tarballs.
(define-public vim-neosnippet-snippets
  (let ((commit "8e2b1c0cab9ed9a832b3743dbb65e9966a64331a")
        (revision "1"))
    (package
      (name "vim-neosnippet-snippets")
      (version (string-append "0.0.0-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shougo/neosnippet-snippets")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "151wpvbj6jb9jdkbhj3b77f5sq7y328spvwfbqyj1y32rg4ifmc6"))))
      (build-system copy-build-system)
      (arguments
       '(#:install-plan
         '(("neosnippets" "share/vim/vimfiles/"))))
    (synopsis "Snippets for neosnippet")
    (description
     "@code{neosnippet-snippets} provides standard snippets for the Vim plugin
@code{neosnippet}.  Snippets are small templates for commonly used code that
you can fill in on the fly.")
    (home-page "https://github.com/Shougo/neosnippet-snippets")
    (license license:expat))))

;; The released tarball is too old for our Vim.
(define-public vim-neosnippet
  (let ((commit "1bd7e23c79b73da16eb0c9469b25c376d3594583")
        (revision "1"))
  (package
    (name "vim-neosnippet")
    (version (string-append "4.2-" revision "." (string-take commit 7)))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Shougo/neosnippet.vim/")
             (commit commit)))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32
         "0k80syscmpnj38ks1fq02ds59g0r4jlg9ll7z4qc048mgi35alw5"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("autoload" "share/vim/vimfiles/")
         ("doc" "share/vim/vimfiles/")
         ("ftdetect" "share/vim/vimfiles/")
         ("ftplugin" "share/vim/vimfiles/")
         ("indent" "share/vim/vimfiles/")
         ("plugin" "share/vim/vimfiles/")
         ("rplugin" "share/vim/vimfiles/")
         ("syntax" "share/vim/vimfiles/"))))
    (synopsis "Snippet support for Vim")
    (description
     "@code{neosnippet}, is a plugin for Vim which adds snippet support to Vim.
Snippets are small templates for commonly used code that you can fill in on
the fly.  To use snippets can increase your productivity in Vim a lot.
The functionality of this plug-in is quite similar to plug-ins like
@code{snipMate.vim} or @code{snippetsEmu.vim}.  But since you can choose
snippets with the neocomplcache / neocomplete interface, you might have less
trouble using them, because you do not have to remember each snippet name.")
    (home-page "https://github.com/Shougo/neosnippet.vim/")
    (license license:expat))))

(define-public vim-scheme
  (let ((commit "e22fc8e199ef52f2efacd08e71c3add90d83b375")
        (revision "3"))
    (package
      (name "vim-scheme")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.foldling.org/vim-scheme.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "04h946vr4f8wxap3wzqs69y2v8n50g2zbk22jsg2kxr4c01z5cbw"))))
      (build-system copy-build-system)
      (arguments
       '(#:install-plan
         '(("ftplugin" "share/vim/vimfiles/")
           ("indent" "share/vim/vimfiles/")
           ("syntax" "share/vim/vimfiles/"))))
      (synopsis "Scheme syntax for Vim")
      (description
       "@code{vim-scheme} provides Scheme support for Vim (R7RS and CHICKEN).")
      (home-page "https://foldling.org/git/vim-scheme.git/")
      (license license:unlicense))))

(define-public vim-luna
  (let ((commit "633619953dcf8577168e255230f96b05f28d6371")
        (revision "1"))
    (package
      (name "vim-luna")
      (version (string-append "0.0.0-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/notpratheek/vim-luna")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "0ka3qbhsh8lix1vyj4678j7dnchkd8khhirrnn3aylxxf8fpqyg8"))))
      (build-system copy-build-system)
      (arguments
       '(#:install-plan
         '(("colors" "share/vim/vimfiles/"))))
      (synopsis "Dark color theme for Vim")
      (description
       "@code{vim-luna} is a dark color theme for Vim.")
      (home-page "https://github.com/notpratheek/vim-luna")
      (license license:expat))))

;; There are no tarball releases.
(define-public vim-context-filetype
  (let ((commit "5e85f8cae26806f391aefe2661791a6de53bcea2")
        (revision "1"))
    (package
      (name "vim-context-filetype")
      (version (string-append "0.0.0-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Shougo/context_filetype.vim")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "0alvrfhmd91zkd9h83s8wvgyq4iakcf6rybsyjd369qbgpcqky89"))))
      (build-system copy-build-system)
      (arguments
       '(#:install-plan
         '(("doc" "share/vim/vimfiles/")
           ("autoload" "share/vim/vimfiles/"))))
      (synopsis "Context filetype library for Vim")
      (description
       "@code{vim-context-filetype} is context filetype library for Vim script.")
      (home-page "https://github.com/Shougo/context_filetype.vim")
      (license license:expat)))) ; ??? check again

(define-public vim-fugitive
  (package
    (name "vim-fugitive")
    (version "3.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/tpope/vim-fugitive")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "17c3wzqkbzbf0nmlxpgk90yyv3d09209fqxqysand8bzb1cbfwzn"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("autoload" "share/vim/vimfiles/")
         ("doc" "share/vim/vimfiles/")
         ("ftdetect" "share/vim/vimfiles/")
         ("ftplugin" "share/vim/vimfiles/")
         ("plugin" "share/vim/vimfiles/")
         ("syntax" "share/vim/vimfiles/"))))
    (home-page "https://github.com/tpope/vim-fugitive")
    (synopsis "Vim plugin to work with Git")
    (description "Vim-fugitive is a wrapper for Vim that complements the
command window, where you can stage and review your changes before the next
commit or run any Git arbitrary command.")
    (license license:vim))) ; distributed under the same license as vim

(define-public vim-airline
  (package
    (name "vim-airline")
    (version "0.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/vim-airline/vim-airline")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1aksmr73648pvyc75pfdz28k2d4ky52rn7xiwcv7lz87q3vqld7k"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("autoload" "share/vim/vimfiles/")
         ("doc" "share/vim/vimfiles/")
         ("plugin" "share/vim/vimfiles/"))))
    (synopsis "Statusline for Vim")
    (description
     "@code{vim-airline} is an extensible statusline for Vim.
It can be extended and costumized with themes, works with unicode fonts
and powerline symbols, etc.")
    (home-page "https://github.com/vim-airline/vim-airline")
    (license license:expat)))

;; There are no tarball releases.
(define-public vim-airline-themes
  (let ((commit "e6f233231b232b6027cde6aebeeb18d9138e5324")
        (revision "2"))
    (package
      (name "vim-airline-themes")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/vim-airline/vim-airline-themes")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1sb7nb7j7bz0pv1c9bgdy0smhr0jk2b1vbdv9yzghg5lrknpsbr6"))))
      (build-system copy-build-system)
      (arguments
       '(#:install-plan
         '(("autoload" "share/vim/vimfiles/")
           ("doc" "share/vim/vimfiles/")
           ("plugin" "share/vim/vimfiles/"))))
      (synopsis "Collection of themes for Vim-airline")
      (description
       "@code{vim-airline-themes} is a collection of themes for @code{vim-airline}.")
      (home-page "https://github.com/vim-airline/vim-airline-themes")
      (license license:expat))))

(define-public vim-syntastic
  (package
    (name "vim-syntastic")
    (version "3.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vim-syntastic/syntastic")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j91f72jaz1s6aw1hpjiz30vk2ds2aqd9gisk91grsldy6nz6hhz"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("autoload" "share/vim/vimfiles/")
         ("doc" "share/vim/vimfiles/")
         ("plugin" "share/vim/vimfiles/")
         ("syntax_checkers" "share/vim/vimfiles/"))))
    (synopsis "Syntax checking plugin for Vim")
    (description
     "Vim-syntastic is a syntax checking plugin for Vim.  It runs files through
external syntax checkers and displays any resulting errors to the user.  This
can be done on demand, or automatically as files are saved.  If syntax errors
are detected, the user is notified.")
    (home-page "https://github.com/vim-syntastic/syntastic")
    (license license:wtfpl2)))

(define-public vim-solarized
  (let ((commit "62f656a02f93c5190a8753159e34b385588d5ff3")
        (revision "1"))
    (package
      (name "vim-solarized")
      (version (git-version "1.0.0beta1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/altercation/solarized")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0001mz5v3a8zvi3gzmxhi3yrsb6hs7qf6i497arsngnvj2cwn61d"))))
      (build-system copy-build-system)
      (arguments
       '(#:install-plan
         '(("vim-colors-solarized/colors" "share/vim/vimfiles/")
           ("vim-colors-solarized/doc" "share/vim/vimfiles/"))))
      (home-page "https://github.com/altercation/vim-colors-solarized")
      (synopsis "Solarized color scheme for Vim")
      (description
       "This package provides the Solarized theme as a Vim color scheme.

Solarized is a 16-color palette comprising 8 monotones and 8 accent
colors.  It was designed for use with both terminal and GUI applications, and
has a dark and a light mode.

Based on CIELAB lightness relationships between colors, this theme reduces
brightness contrast but retains contrasting hues based on colorwheel relations
for syntax highlighting readability.

It keeps the same selective contrast relationships and overall feel when
switching between the light and dark background modes.")
      (license license:expat))))

(define-public editorconfig-vim
  (package
    (name "editorconfig-vim")
    (version "1.1.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/editorconfig/editorconfig-vim")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0mp80bi2m56bb93szw87vy6q5s85yk9g91sl4pr51316rgdv5kkv"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("autoload" "share/vim/vimfiles/")
         ("doc" "share/vim/vimfiles/")
         ("plugin" "share/vim/vimfiles/"))))
    (home-page "https://editorconfig.org/")
    (synopsis "EditorConfig plugin for Vim")
    (description "EditorConfig makes it easy to maintain the correct coding
style when switching between different text editors and between different
projects.  The EditorConfig project maintains a file format and plugins for
various text editors which allow this file format to be read and used by those
editors.")
    (license license:bsd-2)))

(define-public neovim-syntastic
  (package
    (inherit vim-syntastic)
    (name "neovim-syntastic")
    (arguments
     '(#:install-plan
       '(("autoload" "share/nvim/site/")
         ("doc" "share/nvim/site/")
         ("plugin" "share/nvim/site/")
         ("syntax_checkers" "share/nvim/site/"))))
    (synopsis "Syntax checking plugin for Neovim")
    (description
     "Vim-syntastic is a syntax checking plugin for Neovim.  It runs files through
external syntax checkers and displays any resulting errors to the user.  This
can be done on demand, or automatically as files are saved.  If syntax errors
are detected, the user is notified.")))

(define-public neovim
  (package
    (name "neovim")
    (version "0.4.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/neovim/neovim")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11zyj6jvkwas3n6w1ckj3pk6jf81z1g7ngg4smmwm7c27y2a6f2m"))))
    (build-system cmake-build-system)
    (arguments
     `(#:modules ((srfi srfi-26)
                  (guix build cmake-build-system)
                  (guix build utils))
       #:configure-flags '("-DPREFER_LUA:BOOL=YES")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-lua-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((lua-version "5.1")
                    (lua-cpath-spec
                     (lambda (prefix)
                       (let ((path (string-append prefix "/lib/lua/" lua-version)))
                         (string-append path "/?.so;" path "/?/?.so"))))
                    (lua-path-spec
                     (lambda (prefix)
                       (let ((path (string-append prefix "/share/lua/" lua-version)))
                         (string-append path "/?.lua;" path "/?/?.lua"))))
                    (lua-inputs (map (cute assoc-ref inputs <>)
                                     '("lua"
                                       "lua-luv"
                                       "lua-lpeg"
                                       "lua-bitop"
                                       "lua-libmpack"))))
               (setenv "LUA_PATH"
                       (string-join (map lua-path-spec lua-inputs) ";"))
               (setenv "LUA_CPATH"
                       (string-join (map lua-cpath-spec lua-inputs) ";"))
               #t)))
         (add-after 'unpack 'prevent-embedding-gcc-store-path
           (lambda _
             ;; nvim remembers its build options, including the compiler with
             ;; its complete path.  This adds gcc to the closure of nvim, which
             ;; doubles its size.  We remove the refirence here.
             (substitute* "cmake/GetCompileFlags.cmake"
               (("\\$\\{CMAKE_C_COMPILER\\}") "/gnu/store/.../bin/gcc"))
             #t)))))
    (inputs
     `(("libuv" ,libuv)
       ("msgpack" ,msgpack)
       ("libtermkey" ,libtermkey)
       ("libvterm" ,libvterm)
       ("unibilium" ,unibilium)
       ("jemalloc" ,jemalloc)
       ("libiconv" ,libiconv)
       ("lua" ,lua-5.1)
       ("lua-luv" ,lua5.1-luv)
       ("lua-lpeg" ,lua5.1-lpeg)
       ("lua-bitop" ,lua5.1-bitop)
       ("lua-libmpack" ,lua5.1-libmpack)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gettext" ,gettext-minimal)
       ("gperf" ,gperf)))
    (home-page "https://neovim.io")
    (synopsis "Fork of vim focused on extensibility and agility")
    (description "Neovim is a project that seeks to aggressively
refactor Vim in order to:

@itemize
@item Simplify maintenance and encourage contributions
@item Split the work between multiple developers
@item Enable advanced external UIs without modifications to the core
@item Improve extensibility with a new plugin architecture
@end itemize\n")
    ;; Neovim is licensed under the terms of the Apache 2.0 license,
    ;; except for parts that were contributed under the Vim license.
    (license (list license:asl2.0 license:vim))))

(define-public eovim
  (package
    (name "eovim")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jeanguyomarch/eovim/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06b7crmz3wvvq15ncl0jk20s8j1pmna2jin0k5y5n5qxpafbgp3k"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #false ;no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'reference-nvim
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((nvim (search-input-file inputs "/bin/nvim")))
               ;; This substitution should change one line, and replaces the default
               ;; value in the struct of options with an absolute store reference.
               (substitute* "../source/src/main.c"
                 (("(^[[:blank:]]+\\.nvim = \")nvim" _ start)
                  (string-append start nvim))))))
         (add-before 'build 'set-home
           (lambda _ (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list efl msgpack neovim))
    (home-page "https://github.com/jeanguyomarch/eovim/")
    (synopsis "EFL GUI for Neovim")
    (description "Graphical Neovim interface based on the @acronym{EFL, Enlightenment
Foundation Libraries} toolkit.  Its features include customizable appearance
and support for fonts with ligatures.")
    (license license:expat)))

(define-public vifm
  (package
    (name "vifm")
    (version "0.12")
    (source
      (origin
        (method url-fetch)
        (uri (list
               (string-append "https://github.com/vifm/vifm/releases/download/v"
                              version "/vifm-" version ".tar.bz2")
               (string-append "https://sourceforge.net/projects/vifm/files/vifm/"
                              "vifm-" version ".tar.bz2")))
        (sha256
         (base32
          "1h5j4y704nciyzg3aaav8sl3r5h9mpwq8f28cj65nnxk6a7n3a9k"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-build-timestamp")
       #:phases
       (modify-phases %standard-phases
         (add-after 'patch-source-shebangs 'patch-test-shebangs
           (lambda _
             (substitute* (cons* "src/background.c"
                                 "src/cfg/config.c"
                                 (find-files "tests" "\\.c$"))
               (("/bin/sh") (which "sh"))
               (("/bin/bash") (which "bash")))
             ;; This test segfaults
             (substitute* "tests/Makefile"
               (("misc") ""))
             #t))
          (add-after 'install 'install-vim-plugin-files
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (vifm (string-append out "/share/vifm"))
                     (vimfiles (string-append out "/share/vim/vimfiles")))
                (copy-recursively (string-append vifm "/colors")
                                  (string-append vimfiles "/colors"))
                (copy-recursively (string-append vifm "/vim")
                                  vimfiles)
                (delete-file-recursively (string-append vifm "/colors"))
                (delete-file-recursively (string-append vifm "/vim")))
              #t)))))
    (native-inputs
     (list groff)) ; for the documentation
    (inputs
     (list libx11 ncurses perl))
    (home-page "https://vifm.info/")
    (synopsis "Flexible vi-like file manager using ncurses")
    (description "Vifm is a file manager providing a @command{vi}-like usage
experience.  It has similar keybindings and modes (e.g. normal, command line,
visual).  The interface uses ncurses, thus vifm can be used in text-only
environments.  It supports a wide range of features, some of which are known
from the @command{vi}-editor:
@enumerate
@item utf8 support
@item user mappings (almost like in @code{vi})
@item ranges in command
@item line commands
@item user defined commands (with support for ranges)
@item registers
@item operation undoing/redoing
@item fuse file systems support
@item trash
@item multiple files renaming
@item support of filename modifiers
@item colorschemes support
@item file name color according to file type
@item path specific colorscheme customization
@item bookmarks
@item operation backgrounding
@item customizable file viewers
@item handy @code{less}-like preview mode
@item filtering out and searching for files using regular expressions
@item one or two panes view
@end enumerate
With the package comes a plugin to use vifm as a vim file selector.")
    (license license:gpl2+)))

(define-public python-pynvim
  (package
    (name "python-pynvim")
    (version "0.4.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pynvim" version))
              (sha256
               (base32
                "13qgwkqbx012j5spis1aw8rb120rw0zphgjy1j58irax8r6j1ikb"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-greenlet python-msgpack))
    (arguments
     `(#:tests? #f))
    (home-page "https://github.com/neovim/pynvim")
    (synopsis "Python client and plugin host for neovim")
    (description "Pynvim implements support for python plugins in neovim.  It
also works as a library for connecting to and scripting neovim processes
through its msgpack-rpc API.")
    (license license:asl2.0)))

(define-public vim-guix-vim
  (package
    (name "vim-guix-vim")
    (version "0.3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://gitlab.com/Efraim/guix.vim")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "080ni4z23qdr8rkrswjqfqfrrcnpn7qdgrg14glwji46wzvwxqyx"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("autoload" "share/vim/vimfiles/")
         ("compiler" "share/vim/vimfiles/")
         ("doc" "share/vim/vimfiles/")
         ("indent" "share/vim/vimfiles/")
         ("ftdetect" "share/vim/vimfiles/")
         ("ftplugin" "share/vim/vimfiles/")
         ("plugin" "share/vim/vimfiles/")
         ("syntax" "share/vim/vimfiles/"))))
    (home-page "https://gitlab.com/Efraim/guix.vim")
    (synopsis "Guix integration in Vim")
    (description "This package provides support for GNU Guix in Vim.")
    (license license:vim)))

(define-public vim-asyncrun
  (package
    (name "vim-asyncrun")
    (version "2.8.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/skywind3000/asyncrun.vim")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "11zcw0sll6qg6ha0rr6n1cw5v73azvf7ycwn9lgiwa5cj7rrqjf4"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("plugin" "share/vim/vimfiles/")
         ("doc/" "share/vim/vimfiles/doc" #:include ("asyncrun.txt")))))
    (home-page "https://github.com/skywind3000/asyncrun.vim")
    (synopsis "Run Async Shell Commands in Vim")
    (description "This plugin takes the advantage of new APIs in Vim 8 (and
NeoVim) to enable you to run shell commands in background and read output in the
quickfix window in realtime.")
    (license license:expat)))

(define-public vim-dispatch
  (package
    (name "vim-dispatch")
    (version "1.8")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/tpope/vim-dispatch")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1m8b5mn2zqlphzs6xfwykwmghf6p0wabrhpjmh7vav35jgcxc4wl"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("autoload" "share/vim/vimfiles/")
         ("doc" "share/vim/vimfiles/")
         ("plugin" "share/vim/vimfiles/"))))
    (home-page "https://github.com/tpope/vim-dispatch")
    (synopsis "Asynchronous build and test dispatcher")
    (description "Leverage the power of Vim's compiler plugins without being
bound by synchronicity.  Kick off builds and test suites using one of several
asynchronous adapters (including tmux, screen, and a headless mode), and when
the job completes, errors will be loaded and parsed automatically.")
    (license license:vim)))

(define-public vim-gemini-vim
  ;; No releases have been tagged.
  (let ((commit "f300c54174fc0db8fb68f1bc04307b58612e9630")
        (revision "1"))
    (package
      (name "vim-gemini-vim")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://git.sr.ht/~torresjrjr/gemini.vim")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32 "05ffhhfahjqwxyrqmsinsahrs15wknzl2qbj8mznyv319mn2civ2"))))
      (build-system copy-build-system)
      (arguments
       `(#:install-plan
         '(("ftdetect" "share/vim/vimfiles/")
           ("syntax" "share/vim/vimfiles/"))))
      (home-page "https://git.sr.ht/~torresjrjr/gemini.vim")
      (synopsis "Vim syntax highlighting plugin for Gemini")
      (description "This Vim plugin provides a Vim syntax highlighting plugin
for Gemini Text, the text/gemini media type, as defined in the Gemini protocol
specification.")
      (license license:gpl3))))

(define-public vim-eunuch
  (let ((commit "33e875b31c8b811a0a47908884a5e2339106bbe8")
        (revision "1"))
    (package
      (name "vim-eunuch")
      (version (git-version "1.2" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/tpope/vim-eunuch")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "1xadb22kd40swmww0qxmmkcpcq6viy8l167pjck5q32hfngll5d3"))))
      (build-system copy-build-system)
      (arguments
       '(#:install-plan
         '(("doc" "share/vim/vimfiles/")
           ("plugin" "share/vim/vimfiles/"))))
      (home-page "https://github.com/tpope/vim-eunuch")
      (synopsis "Vim sugar for the UNIX shell commands")
      (description "Vim sugar for the UNIX shell commands that need it the most.
This package includes commands such as @code{SudoWrite} and @code{SudoEdit} and
help working on Vim buffers and the files they reference with one command.")
      (license license:vim))))

(define-public vim-slime
  ;; No tagged releases.
  (let ((commit "a522fed677e50175f52efc5848cc35209af33216")
        (revision "1"))
    (package
      (name "vim-slime")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/jpalardy/vim-slime")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32 "0k4b629jn6xlxyjxdl3cgm06v9dmx967rqnslv5m82c9kscwpyh4"))))
      (build-system copy-build-system)
      (arguments
       `(#:install-plan
         '(("autoload" "share/vim/vimfiles/")
           ("doc" "share/vim/vimfiles/")
           ("ftplugin" "share/vim/vimfiles/")
           ("plugin" "share/vim/vimfiles/"))))
      (home-page "https://technotales.wordpress.com/2007/10/03/like-slime-for-vim/")
      (synopsis "Vim plugin to give you some slime")
      (description "SLIME is an Emacs plugin to turn Emacs into a Lisp IDE.  You
can type text in a file, send it to a live REPL, and avoid having to reload all
your code every time you make a change.  @code{Vim-slime} is an attempt at
getting some of these features into Vim.  It works with any REPL and isn't tied
to Lisp.")
      (license license:expat))))

(define-public vim-paredit
  ;; The last tagged version is from August 2013.
  (let ((commit "97d51d099523b37bb35cbcf3564cbfb46e66e4ec")
        (revision "1"))
    (package
      (name "vim-paredit")
      (version (git-version "0.9.11" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/kovisoft/paredit")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32 "07d5s20r0ssd7rir45vy0fqlci44gha1a81rcilgar227f3nw328"))))
      (build-system copy-build-system)
      (arguments
       '(#:install-plan
         '(("doc" "share/vim/vimfiles/")
           ("plugin" "share/vim/vimfiles/"))))
      (home-page "https://github.com/kovisoft/paredit")
      (synopsis "Vim plugin for structured editing of Lisp S-expressions")
      (description
       "Paredit performs structured editing of Lisp S-expressions in Vim.
@code{Paredit.vim} is similar to @code{paredit.el} for Emacs.")
      ;; License listed in plugin/paredit.vim.
      (license license:public-domain))))

(define-public vim-surround
  (package
    (name "vim-surround")
    (version "2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/tpope/vim-surround")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1b0bd5m5lv1p4d299mrwjfs2gk0zqwyaqdaid9hs9yqlxnr8s5nf"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("doc" "share/vim/vimfiles/")
         ("plugin" "share/vim/vimfiles/"))))
    (home-page "https://github.com/tpope/vim-surround")
    (synopsis "Vim plugin for easy quoting and parenthesizing")
    (description
     "Surround.vim is all about \"surroundings\": parentheses, brackets,
quotes, XML tags, and more.  The plugin provides mappings to easily delete,
change and add such surroundings in pairs.")
    (license license:vim)))

(define-public vim-gnupg
  (package
    (name "vim-gnupg")
    (version "2.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/jamessan/vim-gnupg/releases/"
                           "download/v" version
                           "/vim-gnupg-v" version ".tar.gz"))
       (sha256
        (base32 "02w8lgyyh7wgxysvmmcf9ja5c06vrbyh3alzvv97x8cfhrp0skn7"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("autoload" "share/vim/vimfiles/")
         ("doc" "share/vim/vimfiles/")
         ("plugin" "share/vim/vimfiles/"))))
    (home-page "https://www.vim.org/scripts/script.php?script_id=3645")
    (synopsis "Vim plugin for transparent editing of gpg encrypted files")
    (description
     "This script implements transparent editing of gpg encrypted files.  The
filename must have a @code{.gpg}, @code{.pgp} or @code{.asc} suffix.  When
opening such a file the content is decrypted, and the content will be encrypted
to all recipients before it is written.  This script turns off viminfo,
swapfile, and undofile when editing encrypted files to increase security.")
    (properties
     '((release-monitoring-url . "https://github.com/jamessan/vim-gnupg/releases")))
    (license license:gpl2+)))

(define-public vim-ctrlp
  (package
    (name "vim-ctrlp")
    (version "1.81")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ctrlpvim/ctrlp.vim")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0n68hg59h4rjn0ziqbsh5pr03l3kr98zk54659ny6vq107af1w96"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("autoload" "share/vim/vimfiles/")
         ("doc" "share/vim/vimfiles/")
         ("plugin" "share/vim/vimfiles/"))))
    (home-page "https://ctrlpvim.github.io/ctrlp.vim/")
    (synopsis "Fuzzy file, buffer, mru, tag, etc. finder for Vim")
    (description
     "CtrlP features:
@itemize
@item Written in pure Vimscript for MacVim, gVim and Vim 7.0+.
@item Full support for Vim's regexp as search patterns.
@item Built-in @acronym{Most Recently Used, MRU} files monitoring and search.
@item Built-in project's root finder.
@item Open multiple files at once.
@item Create new files and directories.
@item Execute Ex commands on an opening file (jump to a line, to a string or do
anything).
@item Optional cross-session caching and history allow for fast initialization.
@item Mappings and usage conform to Vim's conventions.
@end itemize")
    (license license:vim)))

(define-public vim-mucomplete
  (package
    (name "vim-mucomplete")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/lifepillar/vim-mucomplete")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "054g80n09mmxxlh8xaic29bn8bgn3clvv732rymljdyvbj1mlhwd"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("autoload" "share/vim/vimfiles/")
         ("doc" "share/vim/vimfiles/")
         ("plugin" "share/vim/vimfiles/"))))
    (home-page "https://github.com/lifepillar/vim-mucomplete")
    (synopsis "MUcomplete is a minimalist autocompletion plugin for Vim")
    (description
     "MUcomplete is an implementation of chained (fallback) completion,
whereby several completion methods are attempted one after another until a
result is returned.")
    (license license:expat)))

(define-public vim-gitgutter
  (let ((commit "256702dd1432894b3607d3de6cd660863b331818")
        (revision "1"))
    (package
      (name "vim-gitgutter")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/airblade/vim-gitgutter")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0zpa7cs59a8sq0k3frlf9flpf30jcn239yrpmv40r7nqvxzglbpl"))))
      (build-system copy-build-system)
      (arguments
       '(#:install-plan
         '(("autoload" "share/vim/vimfiles/")
           ("doc" "share/vim/vimfiles/")
           ("plugin" "share/vim/vimfiles/"))))
      (synopsis "Vim plugin which shows a git diff in the sign column")
      (description
       "A Vim plugin which shows a git diff in the sign column.  It shows which
lines have been added, modified, or removed.  You can also preview, stage, and
undo individual hunks; and stage partial hunks.  The plugin also provides a hunk
text object.  The signs are always up to date and the plugin never saves your
buffer.")
      (home-page "https://github.com/airblade/vim-gitgutter")
      (license license:expat))))

(define-public vim-characterize
  (package
    (name "vim-characterize")
    (version "1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/tpope/vim-characterize")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ppsbsd696ih40d9f76mdl9sd9y7p2pvm65qmvq4b2zhkv4xbpxz"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("autoload" "share/vim/vimfiles/")
         ("doc" "share/vim/vimfiles/")
         ("plugin" "share/vim/vimfiles/"))))
    (home-page "https://github.com/tpope/vim-characterize")
    (synopsis "Vim plugin for showing Unicode character metadata")
    (description
     "In Vim, pressing @code{ga} on a character reveals its representation in
decimal, octal, and hex.  Characterize.vim modernizes this with the following
additions:
@itemize
@item Unicode character names: @code{U+00A9 COPYRIGHT SYMBOL}
@item Vim digraphs (type after @code{<C-K>} to insert the character):
@code{Co}, @code{cO}
@item Emoji codes: @code{:copyright:}
@item HTML entities: @code{&copy;}
@end itemize")
    (license license:vim)))

(define-public vim-tagbar
  (package
    (name "vim-tagbar")
    (version "3.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/preservim/tagbar")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fqfs8msmr6d4kpvxqp14sdjvp5fj52q5w5kz71myzcd4kqzmirp"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("autoload" "share/vim/vimfiles/")
         ("doc" "share/vim/vimfiles/")
         ("plugin" "share/vim/vimfiles/")
         ("syntax" "share/vim/vimfiles/"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'link-univerisal-ctags
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((ctags (assoc-ref inputs "universal-ctags")))
               (substitute* "autoload/tagbar.vim"
                 (("(.*)universal-ctags']" all leader)
                  (string-append all "\n"
                                 leader ctags "/bin/ctags']")))))))))
    (inputs
     (list universal-ctags))
    (home-page "https://github.com/preservim/tagbar")
    (synopsis "Vim plugin that displays tags in a window, ordered by scope")
    (description
     "Tagbar is a Vim plugin that provides an easy way to browse the tags of
the current file and get an overview of its structure.  It does this by creating
a sidebar that displays the ctags-generated tags of the current file, ordered
by their scope.  This means that for example methods in C++ are displayed under
the class they are defined in.")
    (license license:vim)))

(define-public vim-nerdtree
  (package
    (name "vim-nerdtree")
    (version "6.10.16")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/preservim/nerdtree")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1si8qla86ng8cffbmfrk9gss0i3912yw0f1ph4bsiq0kk837lccp"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("autoload" "share/vim/vimfiles/")
         ("doc" "share/vim/vimfiles/")
         ("lib" "share/vim/vimfiles/")
         ("nerdtree_plugin" "share/vim/vimfiles/")
         ("plugin" "share/vim/vimfiles/")
         ("syntax" "share/vim/vimfiles/"))))
    (home-page "https://github.com/preservim/nerdtree")
    (synopsis "Tree explorer plugin for Vim")
    (description
     "The NERDTree is a file system explorer for the Vim editor.  Using this
plugin, users can visually browse complex directory hierarchies, quickly open
files for reading or editing, and perform basic file system operations.")
    (license license:wtfpl2)))

(define-public vim-nerdcommenter
  (let ((commit "a65465d321f2f8a74b2ffa540b9b87563f7e12e8")
        (revision "1"))
    (package
      (name "vim-nerdcommenter")
      (version (git-version "2.5.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/preservim/nerdcommenter")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "00ir65iv8jfbgzjmj7332fmydh0qhabbhx8zbvd3j6pgfxqpaafw"))))
      (build-system copy-build-system)
      (arguments
       '(#:install-plan
         '(("autoload" "share/vim/vimfiles/")
           ("doc" "share/vim/vimfiles/")
           ("plugin" "share/vim/vimfiles/"))))
      (home-page "https://github.com/preservim/nerdcommenter")
      (synopsis "Vim plugin for easy commenting of code")
      (description
       "NERD commenter is a Vim plugin that provides many different commenting
operations and styles which are invoked via key mappings and a menu.  These
operations are available for most filetypes.")
      (license license:cc0))))
