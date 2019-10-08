;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 José Miguel Sánchez García <jmi2k@openmailbox.org>
;;; Copyright © 2016 Carlo Zancanaro <carlo@zancanaro.id.au>
;;; Copyright © 2017, 2018 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2017 Feng Shu <tumashu@163.com>
;;; Copyright © 2017 ng0 <ng0@n0.is>
;;; Copyright © 2014 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.org>
;;; Copyright © 2017, 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2019 Nicolas Goaziou <mail@nicolasgoaziou.fr>
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
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
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
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages libreoffice)
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
  #:use-module (gnu packages xml))

(define-public vis
  (package
    (name "vis")
    (version "0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/martanne/"
                                  name "/archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1xbxb3q963s6sav63yw0x30lm0wvxsrzb7hr6a7dh4f8r7mp1skp"))))
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
    (version "2019.01.20")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/mawww/kakoune/"
                           "releases/download/v" version "/"
                           "kakoune-" version ".tar.bz2"))
       (sha256
        (base32 "15drk7i17qdiqxqkjxhrxfclryj9qzb5ymxd20dwl05y4yi064cr"))))
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
    (version "20180408")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hboetes/mg")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "06w86xk7sjl2x2h3z6msn8kpmwj05qdimcym77wzhz5s94dzh1bl"))
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
     "Mg (mg) is a GNU Emacs style editor, with which it is \"broadly\"
compatible.  This is a portable version of the mg maintained by the OpenBSD
team.")
    (license license:public-domain)))

(define-public ghostwriter
  (package
    (name "ghostwriter")
    (version "1.7.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/wereturtle/ghostwriter.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1pqlr08z5syqcq5p282asxwzrrm7c1w94baxyb467swh8yp3fj5m"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("qttools" ,qttools)))           ; for lrelease
    (inputs
     `(("hunspell" ,hunspell)
       ("qtbase" ,qtbase)
       ("qtmultimedia" ,qtmultimedia)
       ("qtsvg" ,qtsvg)
       ("qtwebkit" ,qtwebkit)))
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
    (version "4.2.0")
    (source (origin
              (method url-fetch)
              (uri (let ((v (apply string-append (string-split version #\.))))
                     (string-append
                      "https://www.scintilla.org/scintilla" v ".tgz")))
              (sha256
               (base32
                "02ymi86fpcypg6423vfr54lbkxbks046q02v3m3dypawcf3bqy42"))))
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
         (add-before 'build 'expand-C++-include-path
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Make <gcc>/include/c++/ext/string_conversions.h find
             ;; <stdlib.h>.
             (let* ((path "CPLUS_INCLUDE_PATH")
                    (gcc  (assoc-ref inputs "gcc"))
                    (c++  (string-append gcc "/include/c++")))
               (setenv path (string-append c++ ":" (getenv path))))
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
     `(("gcc" ,gcc-7)                   ;require GCC 7.1+
       ("pkg-config" ,pkg-config)))
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
    (version "1.35")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.geany.org/"
                                  "geany-" version ".tar.bz2"))
              (sha256
               (base32
                "179xfnvhcxsv54v2mlrhykqv2j7klniln5sffvqqpjmdvwyivvim"))
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
