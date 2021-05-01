;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2014, 2019 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2016, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2017 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018 Timo Eisenmann <eisenmann@fn.de>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2019 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 Raghav Gururajan <raghavgururajan@disroot.org>
;;; Copyright © 2020 B. Wilson <elaexuotee@wilsonb.com>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020 Nicolò Balzarotti <nicolo@nixo.xyz>
;;; Copyright © 2020 Alexandru-Sergiu Marton <brown121407@posteo.ro>
;;; Copyright © 2021 Cage <cage-dev@twistfold.it>
;;; Copyright © 2021 Benoit Joly <benoit@benoitj.ca>
;;; Copyright © 2021 Alexander Krotov <krotov@iitp.ru>
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

(define-module (gnu packages web-browsers)
  #:use-module (guix build-system asdf)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fltk)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnome-xyz)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages libunistring)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages man)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages nano)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xorg))

(define-public midori
  (package
    (name "midori")
    (version "9.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/midori-browser/core/releases/"
                       "download/v" version "/" name "-v" version ".tar.gz"))
       (sha256
        (base32
         "05i04qa83dnarmgkx4xsk6fga5lw1lmslh4rb3vhyyy4ala562jy"))))
    (build-system cmake-build-system)
    (arguments
     `(#:imported-modules
       (,@%cmake-build-system-modules
        (guix build glib-or-gtk-build-system))
       #:modules
       ((guix build cmake-build-system)
        ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
        (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'glib-or-gtk-compile-schemas
           (assoc-ref glib-or-gtk:%standard-phases
                      'glib-or-gtk-compile-schemas))
         (add-after 'install 'glib-or-gtk-wrap
           (assoc-ref glib-or-gtk:%standard-phases
                      'glib-or-gtk-wrap)))))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("gtk+:bin" ,gtk+ "bin")
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("adwaita-icon-theme" ,adwaita-icon-theme)
       ("gcr" ,gcr)
       ("glib" ,glib)
       ("glib-networking" ,glib-networking)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gtk+" ,gtk+)
       ("json-glib" ,json-glib)
       ("libarchive" ,libarchive)
       ("libpeas" ,libpeas)
       ("libsoup" ,libsoup)
       ("sqlite" ,sqlite)
       ("vala" ,vala)
       ("webkitgtk" ,webkitgtk)))
    (synopsis "Lightweight graphical web browser")
    (description "@code{Midori} is a lightweight, Webkit-based web browser.
It features integration with GTK+3, configurable web search engine, bookmark
management, extensions such as advertisement blocker and colorful tabs.")
    (home-page "https://www.midori-browser.org")
    (license license:lgpl2.1+)))

(define-public dillo
  (package
    (name "dillo")
    (version "3.0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.dillo.org/download/"
                                  "dillo-" version ".tar.bz2"))
              (sha256
               (base32
                "12ql8n1lypv3k5zqgwjxlw1md90ixz3ag6j1gghfnhjq3inf26yv"))))
    (build-system gnu-build-system)
    (arguments `(#:configure-flags '("--enable-ssl" "--enable-ipv6")))
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("fltk" ,fltk)
              ("fontconfig" ,fontconfig)
              ("libjpeg" ,libjpeg-turbo)
              ("libpng" ,libpng)
              ("libxcursor" ,libxcursor)
              ("libxft" ,libxft)
              ("libxi" ,libxi)
              ("libxinerama" ,libxinerama)
              ("openssl" ,openssl-1.0) ;XXX try latest openssl for dillo > 3.0.5
              ("perl" ,perl)
              ("zlib" ,zlib)))
    (synopsis "Very small and fast graphical web browser")
    (description "Dillo is a minimalistic web browser particularly intended for
older or slower computers and embedded systems.")
    (home-page "https://www.dillo.org")
    (license license:gpl3+)))

(define-public links
  (package
    (name "links")
    (version "2.22")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://links.twibright.com/download/"
                                  "links-" version ".tar.bz2"))
              (sha256
               (base32
                "0k88qbmq0mf6zmk2v158c0rxvqbi7ysn58xyf4qqw7kz79mrhr03"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             ;; The tarball uses a very old version of autconf. It doesn't
             ;; understand extra flags like `--enable-fast-install', so
             ;; we need to invoke it with just what it understands.
             (let ((out (assoc-ref outputs "out")))
               ;; 'configure' doesn't understand '--host'.
               ,@(if (%current-target-system)
                     `((setenv "CHOST" ,(%current-target-system)))
                     '())
               (setenv "CONFIG_SHELL" (which "bash"))
               (invoke "./configure"
                       (string-append "--prefix=" out)
                       "--enable-graphics")
               #t))))))
    (native-inputs `(("linux-libre-headers" ,linux-libre-headers)
                     ("pkg-config" ,pkg-config)))
    (inputs `(("gpm" ,gpm)
              ("libevent" ,libevent)
              ("libjpeg" ,libjpeg-turbo)
              ("libpng" ,libpng)
              ("libtiff" ,libtiff)
              ("libxt" ,libxt)
              ("openssl" ,openssl)
              ("zlib" ,zlib)))
    (synopsis "Text and graphics mode web browser")
    (description "Links is a graphics and text mode web browser, with many
features including, tables, builtin image display, bookmarks, SSL and more.")
    (home-page "http://links.twibright.com")
    ;; The distribution contains a copy of GPLv2
    ;; However, the copyright notices simply say:
    ;; "This file is a part of the Links program, released under GPL."
    ;; Therefore, under the provisions of Section 9, we can choose
    ;; any version ever published by the FSF.
    ;; One file (https.c) contains an exception permitting
    ;; linking of the program with openssl.
    (license license:gpl1+)))

(define-public luakit
  (package
    (name "luakit")
    (version "2.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/luakit/luakit")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0km5nxn6innzn8pfsvlkxvfj2z5g46fp6dy5bnmaklbn13mqlcrn"))))
    (inputs
     `(("lua-5.1" ,lua-5.1)
       ("gtk+" ,gtk+)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("glib-networking" ,glib-networking)
       ("lua5.1-filesystem" ,lua5.1-filesystem)
       ("luajit" ,luajit)
       ("webkitgtk" ,webkitgtk)
       ("sqlite" ,sqlite)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(#:make-flags
       (let ((out (assoc-ref %outputs "out")))
         (list
          "CC=gcc"
          "LUA_BIN_NAME=lua"
          "DEVELOPMENT_PATHS=0"
          (string-append "PREFIX=" out)
          (string-append "XDGPREFIX=" out "/etc/xdg")))
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'lfs-workaround
           (lambda _
             (setenv "LUA_CPATH"
                     (string-append
                      (assoc-ref %build-inputs "lua5.1-filesystem")
                      "/lib/lua/5.1/?.so;;"))
             #t))
         (delete 'configure)
         (delete 'check)
         (add-after 'install 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((luakit (assoc-ref outputs "out"))
                    (lua5.1-filesystem (assoc-ref inputs "lua5.1-filesystem") )
                    (gtk (assoc-ref inputs "gtk+"))
                    (gtk-share (string-append gtk "/share")))
               (wrap-program (string-append luakit "/bin/luakit")
                 `("LUA_CPATH" prefix
                   (,(string-append lua5.1-filesystem
                                    "/lib/lua/5.1/?.so;;")))
                 `("XDG_CONFIG_DIRS" prefix
                   (,(string-append luakit "/etc/xdg/"))))
               #t))))))
    (synopsis "Fast, lightweight, and simple browser based on WebKit")
    (description "Luakit is a fast, lightweight, and simple to use
micro-browser framework extensible by Lua using the WebKit web content engine
and the GTK+ toolkit.")
    (home-page "https://luakit.github.io/")
    (license license:gpl3+)))

(define-public lynx
  (package
    (name "lynx")
    (version "2.9.0dev.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://invisible-mirror.net/archives/lynx/tarballs"
                    "/lynx" version ".tar.bz2"))
              (sha256
               (base32
                "1cjkpwxc1r8x8q73bgh9a4skaph1bwa0anml6f6lvf7lh5zvxw3q"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)
                     ("perl" ,perl)))
    (inputs `(("ncurses" ,ncurses)
              ("libidn" ,libidn)
              ("openssl" ,openssl)
              ("libgcrypt" ,libgcrypt)
              ("unzip" ,unzip)
              ("zlib" ,zlib)
              ("gzip" ,gzip)
              ("bzip2" ,bzip2)))
    (arguments
     `(#:configure-flags
       (let ((openssl (assoc-ref %build-inputs "openssl")))
         `("--with-pkg-config"
           "--with-screen=ncurses"
           "--with-zlib"
           "--with-bzlib"
           ,(string-append "--with-ssl=" openssl)
           ;; "--with-socks5"    ; XXX TODO
           "--enable-widec"
           "--enable-ascii-ctypes"
           "--enable-local-docs"
           "--enable-htmlized-cfg"
           "--enable-gzip-help"
           "--enable-nls"
           "--enable-ipv6"))
       #:tests? #f  ; no check target
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-makefile-shell
           (lambda _ (substitute* "po/makefile.inn"
                       (("/bin/sh") (which "sh")))
                     #t))
         (replace 'install
           (lambda* (#:key (make-flags '()) #:allow-other-keys)
             (apply invoke "make" "install-full" make-flags)
             #t)))))
    (synopsis "Text Web Browser")
    (description
     "Lynx is a fully-featured World Wide Web (WWW) client for users running
cursor-addressable, character-cell display devices.  It will display Hypertext
Markup Language (HTML) documents containing links to files on the local
system, as well as files on remote systems running http, gopher, ftp, wais,
nntp, finger, or cso/ph/qi servers.  Lynx can be used to access information on
the WWW, or to build information systems intended primarily for local
access.")
    (home-page "https://lynx.invisible-island.net/")
    ;; This was fixed in 2.8.9dev.10.
    (properties `((lint-hidden-cve . ("CVE-2016-9179"))))
    (license license:gpl2)))

(define-public kristall
  ;; Fixes to the build system applied after the latest tag
  ;; Use tagged release when updating
  (let ((commit "204b08a9303e75cd8d4c252b0554935062766f86")
        (revision "1"))
    (package
      (name "kristall")
      (version (string-append "0.3-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/MasterQ32/kristall")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1mymq0dh6r0829x74j0jkw8hw46amqwbznlf1b4ra6w77h9yz3lj"))
         (modules '((srfi srfi-1)
                    (ice-9 ftw)
                    (guix build utils)))
         (snippet
          '(let ((preserved-lib-files '("luis-l-gist")))
             (with-directory-excursion "lib"
               (for-each
                (lambda (directory)
                  (simple-format #t "deleting: ~A\n" directory)
                  (delete-file-recursively directory))
                (lset-difference string=?
                                 (scandir ".")
                                 (cons* "." ".." preserved-lib-files))))
             ;; Contains executable of 7z and pscp
             (delete-file-recursively "ci/tools")
             ;; Remove bundled fonts
             (delete-file-recursively "src/fonts")
             #t))))
      (build-system gnu-build-system)
      (arguments
       `(#:modules ((guix build gnu-build-system)
                    (guix build qt-utils)
                    (guix build utils))
         #:imported-modules (,@%gnu-build-system-modules
                             (guix build qt-utils))
         #:make-flags
         (list (string-append "PREFIX=" %output))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)          ; no ./configure script
           (delete 'check)              ; no check target
           (add-before 'build 'set-program-version
             (lambda _
               ;; configure.ac relies on ‘git --describe’ to get the version.
               ;; Patch it to just return the real version number directly.
               (substitute* "src/kristall.pro"
                 (("(KRISTALL_VERSION=).*" _ match)
                  (string-append match ,version "\n")))
               #t))
           (add-before 'build 'dont-use-bundled-cmark
             (lambda _
               (substitute* "src/kristall.pro"
                 (("(^include\\(.*cmark.*)" _ match)
                  (string-append
                   "LIBS += -I" (assoc-ref %build-inputs "cmark") " -lcmark")))
               #t))
           (add-before 'build 'dont-use-bundled-breeze-stylesheet
             (lambda _
               (substitute* "src/kristall.pro"
                 (("../lib/BreezeStyleSheets/breeze.qrc")
                  (string-append
                   (assoc-ref %build-inputs "breeze-stylesheet") "/breeze.qrc")))
               #t))
           (add-before 'build 'dont-use-bundled-fonts
             (lambda _
               (substitute* "src/kristall.pro"
                 ((".*fonts.qrc.*") ""))
               (substitute* "src/main.cpp"
                 (("/fonts/OpenMoji-Color")
                  (string-append
                   (assoc-ref %build-inputs "font-openmoji")
                   "/share/fonts/truetype/OpenMoji-Color"))
                 (("/fonts/NotoColorEmoji")
                  (string-append
                   (assoc-ref %build-inputs "font-google-noto")
                   "/share/fonts/truetype/NotoColorEmoji")))
               #t))
           (add-after 'install 'wrap-program
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (wrap-qt-program out "kristall"))
               #t)))))
      (native-inputs
       `(("breeze-stylesheet"
          ,(let ((commit "2d595a956f8a5f493aa51139a470b768a6d82cce")
                 (revision "0"))
             (origin
               (method git-fetch)
               (uri
                (git-reference
                 (url "https://github.com/Alexhuszagh/BreezeStyleSheets")
                 (commit "2d595a956f8a5f493aa51139a470b768a6d82cce")))
               (file-name (git-file-name "breeze-stylesheet"
                                         (git-version "0" revision commit)))
               (sha256
                (base32
                 "1kvkxkisi3czldnb43ig60l55pi4a3m2a4ixp7krhpf9fc5wp294")))))))
      (inputs
       `(("cmark" ,cmark)
         ("font-google-noto" ,font-google-noto)
         ("font-openmoji" ,font-openmoji)
         ("openssl" ,openssl)
         ("qtbase" ,qtbase)
         ("qtmultimedia" ,qtmultimedia)
         ("qtsvg" ,qtsvg)))
      (home-page "https://kristall.random-projects.net")
      (synopsis "Small-internet graphical client")
      (description "Graphical small-internet client with with many features
including multi-protocol support (gemini, HTTP, HTTPS, gopher, finger),
bookmarks, TSL certificates management, outline generation and a tabbed
interface.")
      (license (list license:gpl3+
                     ;; for breeze-stylesheet
                     license:expat)))))

(define-public qutebrowser
  (package
    (name "qutebrowser")
    (version "2.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/qutebrowser/"
                           "qutebrowser/releases/download/v" version "/"
                           "qutebrowser-" version ".tar.gz"))
       (sha256
        (base32 "0vr6xpjy93w4i0x408vvs9xl497sjah51lxdk4awnx2gfg3acz14"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-attrs" ,python-attrs))) ; for tests
    (inputs
     `(("python-colorama" ,python-colorama)
       ("python-cssutils" ,python-cssutils)
       ("python-importlib-resources" ,python-importlib-resources)
       ("python-jinja2" ,python-jinja2)
       ("python-markupsafe" ,python-markupsafe)
       ("python-pygments" ,python-pygments)
       ("python-pypeg2" ,python-pypeg2)
       ("python-pyyaml" ,python-pyyaml)
       ;; FIXME: python-pyqtwebengine needs to come before python-pyqt so
       ;; that it's __init__.py is used first.
       ("python-pyqtwebengine" ,python-pyqtwebengine)
       ("python-pyqt" ,python-pyqt)
       ;; While qtwebengine is provided by python-pyqtwebengine, it's
       ;; included here so we can wrap QTWEBENGINEPROCESS_PATH.
       ("qtwebengine" ,qtwebengine)))
    (arguments
     `(;; FIXME: With the existance of qtwebengine, tests can now run.  But
       ;; they are still disabled because test phase hangs.  It's not readily
       ;; apparent as to why.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-env-offscreen
           (lambda _
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t))
         (add-after 'install 'install-more
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (app (string-append out "/share/applications"))
                    (hicolor (string-append out "/share/icons/hicolor")))
               (install-file "doc/qutebrowser.1"
                             (string-append out "/share/man/man1"))
               (for-each
                (lambda (i)
                  (let ((src  (format #f "icons/qutebrowser-~dx~d.png" i i))
                        (dest (format #f "~a/~dx~d/apps/qutebrowser.png"
                                      hicolor i i)))
                    (mkdir-p (dirname dest))
                    (copy-file src dest)))
                '(16 24 32 48 64 128 256 512))
               (install-file "icons/qutebrowser.svg"
                             (string-append hicolor "/scalable/apps"))
               (substitute* "misc/org.qutebrowser.qutebrowser.desktop"
                 (("Exec=qutebrowser")
                  (string-append "Exec=" out "/bin/qutebrowser")))
               (install-file "misc/org.qutebrowser.qutebrowser.desktop" app)
               #t)))
         (add-after 'wrap 'wrap-qt-process-path
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/qutebrowser"))
                    (qt-process-path (string-append
                                      (assoc-ref inputs "qtwebengine")
                                      "/lib/qt5/libexec/QtWebEngineProcess")))
               (wrap-program bin
                 `("QTWEBENGINEPROCESS_PATH" = (,qt-process-path)))
               #t))))))
    (home-page "https://qutebrowser.org/")
    (synopsis "Minimal, keyboard-focused, vim-like web browser")
    (description "qutebrowser is a keyboard-focused browser with a minimal
GUI.  It is based on PyQt5 and QtWebEngine.")
    (license license:gpl3+)))

(define-public vimb
  (package
    (name "vimb")
    (version "3.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fanglingsu/vimb/")
             (commit version)))
       (sha256
        (base32 "0228khh3lqbal046k6akqah7s5igq9s0wjfjbdjam75kjj42pbhj"))
       (file-name (git-file-name name version))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(#:tests? #f                      ; no tests
       #:make-flags (list "CC=gcc"
                          "DESTDIR="
                          (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (inputs
     `(("glib-networking" ,glib-networking)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("webkitgtk" ,webkitgtk)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://fanglingsu.github.io/vimb/")
    (synopsis "Fast and lightweight Vim-like web browser")
    (description "Vimb is a fast and lightweight vim like web browser based on
the webkit web browser engine and the GTK toolkit.  Vimb is modal like the great
vim editor and also easily configurable during runtime.  Vimb is mostly keyboard
driven and does not detract you from your daily work.")
    (license license:gpl3+)))

(define-public nyxt
  (package
    (name "nyxt")
    ;; Package the pre-release because latest stable 1.5.0 does not build
    ;; anymore.
    (version "2-pre-release-6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             ;; TODO: Mirror seems to hang, let's fallback to GitHub for now.
             ;; (url "https://source.atlas.engineer/public/nyxt")
             (url "https://github.com/atlas-engineer/nyxt")
             (commit version)))
       (sha256
        (base32
         "0kcqp3p070i6x2jj27h8pxzvmhrzsl4kl3vkc8m76abkxc9lvn03"))
       (file-name (git-file-name "nyxt" version))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list "nyxt" "NYXT_INTERNAL_QUICKLISP=false"
                          (string-append "DESTDIR=" (assoc-ref %outputs "out"))
                          "PREFIX=")
       #:strip-binaries? #f             ; Stripping breaks SBCL binaries.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'fix-common-lisp-cache-folder
           (lambda _
             (setenv "HOME" "/tmp")
             #t))
         (add-before 'build 'set-version
           (lambda _
             (setenv "NYXT_VERSION" ,version)
             #t))
         (add-before 'check 'configure-tests
           (lambda _
             (setenv "NYXT_TESTS_NO_NETWORK" "1")
             (setenv "NYXT_TESTS_ERROR_ON_FAIL" "1")
             #t))
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((bin (string-append (assoc-ref outputs "out") "/bin/nyxt"))
                    (glib-networking (assoc-ref inputs "glib-networking"))
                    (libs '("gsettings-desktop-schemas"))
                    (path (string-join
                           (map (lambda (lib)
                                  (string-append (assoc-ref inputs lib) "/lib"))
                                libs)
                           ":"))
                    (gi-path (string-join
                              (map (lambda (lib)
                                     (string-append (assoc-ref inputs lib)
                                                    "/lib/girepository-1.0"))
                                   libs)
                              ":"))
                    (xdg-path (string-join
                               (map (lambda (lib)
                                      (string-append (assoc-ref inputs lib) "/share"))
                                    libs)
                               ":")))
               (wrap-program bin
                 `("GIO_EXTRA_MODULES" prefix
                   (,(string-append glib-networking "/lib/gio/modules")))
                 `("GI_TYPELIB_PATH" prefix (,gi-path))
                 `("LD_LIBRARY_PATH" ":" prefix (,path))
                 `("XDG_DATA_DIRS" ":" prefix (,xdg-path)))
               #t))))))
    (native-inputs
     `(("prove" ,sbcl-prove)
       ("sbcl" ,sbcl)))
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("bordeaux-threads" ,sbcl-bordeaux-threads)
       ("cl-calispel" ,sbcl-calispel)
       ("cl-containers" ,sbcl-cl-containers)
       ("cl-css" ,sbcl-cl-css)
       ("cl-custom-hash-table" ,sbcl-custom-hash-table)
       ("cl-html-diff" ,sbcl-cl-html-diff)
       ("cl-json" ,sbcl-cl-json)
       ("cl-markup" ,sbcl-cl-markup)
       ("cl-ppcre" ,sbcl-cl-ppcre)
       ("cl-prevalence" ,sbcl-cl-prevalence)
       ("closer-mop" ,sbcl-closer-mop)
       ("cluffer" ,sbcl-cluffer)
       ("dexador" ,sbcl-dexador)
       ("enchant" ,sbcl-enchant)
       ("file-attributes" ,sbcl-file-attributes)
       ("fset" ,sbcl-fset)
       ("hu.dwim.defclass-star" ,sbcl-hu.dwim.defclass-star)
       ("iolib" ,sbcl-iolib)
       ("local-time" ,sbcl-local-time)
       ("log4cl" ,sbcl-log4cl)
       ("mk-string-metrics" ,sbcl-mk-string-metrics)
       ("moptilities" ,sbcl-moptilities)
       ("named-readtables" ,sbcl-named-readtables)
       ("parenscript" ,sbcl-parenscript)
       ("plump" ,sbcl-plump)
       ("quri" ,sbcl-quri)
       ("serapeum" ,sbcl-serapeum)
       ("str" ,sbcl-cl-str)
       ("swank" ,sbcl-slime-swank)
       ("trivia" ,sbcl-trivia)
       ("trivial-clipboard" ,sbcl-trivial-clipboard)
       ("trivial-features" ,sbcl-trivial-features)
       ("trivial-package-local-nicknames" ,sbcl-trivial-package-local-nicknames)
       ("trivial-types" ,sbcl-trivial-types)
       ("unix-opts" ,sbcl-unix-opts)
       ;; WebKitGTK deps
       ("cl-cffi-gtk" ,sbcl-cl-cffi-gtk)
       ("cl-webkit" ,sbcl-cl-webkit)
       ("glib-networking" ,glib-networking)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)))
    (synopsis "Extensible web-browser in Common Lisp")
    (home-page "https://nyxt.atlas.engineer")
    (description "Nyxt is a keyboard-oriented, extensible web-browser
designed for power users.  The application has familiar Emacs and VI
key-bindings and is fully configurable and extensible in Common Lisp.")
    (license license:bsd-3)))

(define-public next
  (deprecated-package "next" nyxt))

(define-public sbcl-next
  (deprecated-package "sbcl-next" nyxt))

(define-public lagrange
  (package
    (name "lagrange")
    (version "1.3.4")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://git.skyjake.fi/skyjake/lagrange/releases/"
                       "download/v" version "/lagrange-" version ".tar.gz"))
       (sha256
        (base32 "108b8a9vdmf846v7p30fs9z0b68w3naifixnia0ra54ssxjvy0h6"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #false))                ;no tests
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("libunistring" ,libunistring)
       ("mpg123" ,mpg123)
       ("openssl" ,openssl)
       ("pcre" ,pcre)
       ("sdl2" ,sdl2)
       ("zlib" ,zlib)))
    (home-page "https://gmi.skyjake.fi/lagrange/")
    (synopsis "Graphical Gemini client")
    (description
     "Lagrange is a desktop GUI client for browsing Geminispace.  It offers
modern conveniences familiar from web browsers, such as smooth scrolling,
inline image viewing, multiple tabs, visual themes, Unicode fonts, bookmarks,
history, and page outlines.")
    (license license:bsd-2)))

(define-public gmni
  (let ((commit "d8f0870446c471a42612d6a8e853ad9b723a6d39")
        (revision "0"))
    (package
      (name "gmni")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.sr.ht/~sircmpwn/gmni")
                      (commit commit)))
                (sha256
                 (base32
                  "1h0iqm7l0i06glf5b2872w656s1mjdiqva14zh6sl4f5yp7zmvwr"))
                (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; no check target
         #:make-flags (list (string-append "CC=" ,(cc-for-target)))))
      (inputs
       `(("openssl" ,openssl)))
      (native-inputs
       `(("pkg-config" ,pkg-config)
         ("scdoc" ,scdoc)))
      (home-page "https://sr.ht/~sircmpwn/gmni")
      (synopsis "Minimalist command line Gemini client")
      (description "The gmni package includes:

@itemize
@item A CLI utility (like curl): gmni
@item A line-mode browser: gmnlm
@end itemize")
      (license (list license:gpl3+
                     (license:non-copyleft
                      "https://curl.se/docs/copyright.html"
                      "Used only for files taken from curl."))))))

(define-public bombadillo
  (package
    (name "bombadillo")
    (version "2.3.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://tildegit.org/sloum/bombadillo")
                    (commit version)))
              (sha256
               (base32
                "02w6h44sxzmk3bkdidl8xla0i9rwwpdqljnvcbydx5kyixycmg0q"))
              (file-name (git-file-name name version))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "tildegit.org/sloum/bombadillo"
       #:install-source? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'install 'install-data
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((builddir "src/tildegit.org/sloum/bombadillo")
                             (out (assoc-ref outputs "out"))
                             (pkg (strip-store-file-name out))
                             (sharedir (string-append out "/share"))
                             (appdir (string-append sharedir "/applications"))
                             (docdir (string-append sharedir "/doc/" pkg))
                             (mandir (string-append sharedir "/man/man1"))
                             (pixdir (string-append sharedir "/pixmaps")))
                        (with-directory-excursion builddir
                          (install-file "bombadillo.desktop" appdir)
                          (install-file "bombadillo.1" mandir)
                          (install-file "bombadillo-icon.png" pixdir)
                          #t)))))))
    (home-page "https://bombadillo.colorfield.space")
    (synopsis "Terminal browser for the gopher, gemini, and finger protocols")
    (description "Bombadillo is a non-web browser for the terminal with
vim-like key bindings, a document pager, configurable settings, and robust
command selection.  The following protocols are supported as first-class
citizens: gopher, gemini, finger, and local.  There is also support for telnet,
http, and https via third-party applications.")
    (license license:gpl3+)))

(define-public tinmop
  (package
    (name "tinmop")
    (version "0.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://notabug.org/cage/tinmop")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fz52agvxnavz375apb1pjalf3myjllr4pc096b6qvc6vzhadg4c"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("curl" ,curl)
       ("gettext" ,gnu-gettext)
       ("gnupg" ,gnupg)
       ("sbcl" ,sbcl)))
    (inputs
     `(("access" ,sbcl-access)
       ("alexandria" ,sbcl-alexandria)
       ("babel" ,sbcl-babel)
       ("bordeaux-threads" ,sbcl-bordeaux-threads)
       ("cl-base64" ,sbcl-cl-base64)
       ("cl-colors2" ,sbcl-cl-colors2)
       ("cl-html5-parser" ,sbcl-cl-html5-parser)
       ("cl-i18n" ,sbcl-cl-i18n)
       ("cl-ppcre" ,sbcl-cl-ppcre)
       ("cl-spark" ,sbcl-cl-spark)
       ("cl-sqlite" ,sbcl-cl-sqlite)
       ("cl+ssl" ,sbcl-cl+ssl)
       ("clunit2" ,sbcl-clunit2)
       ("croatoan" ,sbcl-croatoan)
       ("crypto-shortcuts" ,sbcl-crypto-shortcuts)
       ("drakma" ,sbcl-drakma)
       ("esrap" ,sbcl-esrap)
       ("ieee-floats" ,sbcl-ieee-floats)
       ("local-time" ,sbcl-local-time)
       ("log4cl" ,sbcl-log4cl)
       ("marshal" ,sbcl-marshal)
       ("nano" ,nano)
       ("openssl" ,openssl)
       ("osicat" ,sbcl-osicat)
       ("parse-number" ,sbcl-parse-number)
       ("percent-encoding" ,sbcl-percent-encoding)
       ("sxql" ,sbcl-sxql)
       ("sxql-composer" ,sbcl-sxql-composer)
       ("tooter" ,sbcl-tooter)
       ("unix-opts" ,sbcl-unix-opts)
       ("usocket" ,sbcl-usocket)
       ("xdg-utils" ,xdg-utils)))
    (arguments
     `(#:tests? #f
       #:strip-binaries? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-home
           (lambda _
             (setenv "HOME" "/tmp")
             #t))
         (add-after 'configure 'fix-asdf
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "Makefile.in"
               (("LISP_COMPILER) ")
                "LISP_COMPILER) --eval \"(require 'asdf)\" --eval \"(push \\\"$$(pwd)/\\\" asdf:*central-registry*)\"  "))
             #t)))))
    (synopsis "Gemini and pleroma client with a terminal interface")
    (description
     "This package provides a Gemini and pleroma client with a terminal
interface.")
    (home-page "https://www.autistici.org/interzona/tinmop.html")
    (license license:gpl3+)))
