;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2014, 2019 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2016, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2017 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018 Timo Eisenmann <eisenmann@fn.de>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2019 Brett Gilio <brettg@gnu.org>
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
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fltk)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages image)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gcc)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system python)
  #:use-module (guix build-system asdf))

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
              ("libjpeg" ,libjpeg)
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
    (version "2.20.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://links.twibright.com/download/"
                                  "links-" version ".tar.bz2"))
              (sha256
               (base32
                "097ll98ympzfx7qfdyhc52yzvsp167x5nnjs6v8ih496wv80fksb"))))
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
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("zlib" ,zlib)
              ("openssl" ,openssl)
              ("libjpeg" ,libjpeg)
              ("libtiff" ,libtiff)
              ("libevent" ,libevent)
              ("libpng" ,libpng)
              ("libxt" ,libxt)))
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
    (version "2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/luakit/luakit.git")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1qa90caxv1k6ak88fn0a7n7h0c4iv8szw6zn2axch8ig83i86az2"))))
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
    (version "2.8.9rel.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://invisible-mirror.net/archives/lynx/tarballs"
                    "/lynx" version ".tar.bz2"))
              (sha256
               (base32
                "15cmyyma2kz1hfaa6mwjgli8zwdzq3jv0q2cl6nwzycjfwyijzrq"))))
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

(define-public qutebrowser
  (package
    (name "qutebrowser")
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/qutebrowser/"
                           "qutebrowser/releases/download/v" version "/"
                           "qutebrowser-" version ".tar.gz"))
       (sha256
        (base32
         "1prvd3cysmcjfybn0dmr3ih0bl6lm5ml9i7wd09fn8hb7047mkby"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-attrs" ,python-attrs))) ; for tests
    (inputs
     `(("python-colorama" ,python-colorama)
       ("python-cssutils" ,python-cssutils)
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
                 `("QTWEBENGINEPROCESS_PATH" ":" prefix (,qt-process-path)))
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

(define next-gtk-webkit
  (package
    (name "next-gtk-webkit")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             ;; TODO: Mirror seems to hang, let's fallback to GitHub for now.
             ;; (url "https://source.atlas.engineer/public/next")
             (url "https://github.com/atlas-engineer/next")
             (commit version)))
       (sha256
        (base32
         "1gkmr746rqqg94698a051gv79fblc8n9dq0zg04llba44adhpmjl"))
       (file-name (git-file-name "next" version))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:make-flags (list "gtk-webkit"
                          "CC=gcc"
                          (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key (make-flags '()) #:allow-other-keys)
             (apply invoke "make" "install-gtk-webkit" make-flags))))))
    (inputs
     `(("glib-networking" ,glib-networking)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("webkitgtk" ,webkitgtk)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://next.atlas.engineer")
    (synopsis "Infinitely extensible web-browser (user interface only)")
    (description "Next is a keyboard-oriented, extensible web-browser
inspired by Emacs and designed for power users.  The application has familiar
key-bindings, is fully configurable and extensible in Lisp, and has powerful
features for productive professionals.")
    (license license:bsd-3)))

(define sbcl-next-download-manager
  (package
    (inherit next-gtk-webkit)
    (name "sbcl-next-download-manager")
    (build-system asdf-build-system/sbcl)
    (arguments
     `(#:tests? #f                      ; Need online access.
       #:asd-file "next.asd"
       #:asd-system-name "next/download-manager"))
    (inputs
     `(("cl-ppcre" ,sbcl-cl-ppcre)
       ("dexador" ,sbcl-dexador)
       ("log4cl" ,sbcl-log4cl)
       ("lparallel" ,sbcl-lparallel)
       ("quri" ,sbcl-quri)
       ("str" ,sbcl-cl-str)))
    (native-inputs
     `(("trivial-features" ,sbcl-trivial-features)
       ("prove-asdf" ,sbcl-prove-asdf)))
    (synopsis "Infinitely extensible web-browser (download manager)")))

(define sbcl-next-ring
  (package
    (inherit next-gtk-webkit)
    (name "sbcl-next-ring")
    (build-system asdf-build-system/sbcl)
    (arguments
     `(#:tests? #t
       #:asd-file "next.asd"
       #:asd-system-name "next/ring"))
    (native-inputs
     `(("trivial-features" ,sbcl-trivial-features)
       ("prove-asdf" ,sbcl-prove-asdf)))
    (synopsis "Infinitely extensible web-browser (ring)")))

(define sbcl-next-history-tree
  (package
    (inherit next-gtk-webkit)
    (name "sbcl-next-history-tree")
    (build-system asdf-build-system/sbcl)
    (arguments
     `(#:tests? #t
       #:asd-file "next.asd"
       #:asd-system-name "next/history-tree"))
    (native-inputs
     `(("trivial-features" ,sbcl-trivial-features)
       ("prove-asdf" ,sbcl-prove-asdf)))
    (synopsis "Infinitely extensible web-browser (history-tree)")))

(define sbcl-next-password-manager
  (package
    (inherit next-gtk-webkit)
    (name "sbcl-next-password-manager")
    (build-system asdf-build-system/sbcl)
    (arguments
     `(#:tests? #t
       #:asd-file "next.asd"
       #:asd-system-name "next/password-manager"))
    (inputs
     `(("bordeaux-threads" ,sbcl-bordeaux-threads)
       ("cl-annot" ,sbcl-cl-annot)
       ("cl-ppcre" ,sbcl-cl-ppcre)
       ("str" ,sbcl-cl-str)
       ("trivial-clipboard" ,sbcl-trivial-clipboard)))
    (native-inputs
     `(("trivial-features" ,sbcl-trivial-features)
       ("prove-asdf" ,sbcl-prove-asdf)))
    (synopsis "Infinitely extensible web-browser (password manager)")))

(define sbcl-next-hooks
  (package
    (inherit next-gtk-webkit)
    (name "sbcl-next-hooks")
    (build-system asdf-build-system/sbcl)
    (arguments
     `(#:tests? #t
       #:asd-file "next.asd"
       #:asd-system-name "next/hooks"))
    (inputs
     `(("alexandria" ,sbcl-alexandria)
       ("serapeum" ,sbcl-serapeum)
       ("fare-quasiquote-extras" ,cl-fare-quasiquote-extras)))
    (native-inputs
     `(("trivial-features" ,sbcl-trivial-features)
       ("prove-asdf" ,sbcl-prove-asdf)))
    (synopsis "Infinitely extensible web-browser (hooks)")))

(define-public next
  (let ((version (package-version next-gtk-webkit)))
    (package
      (inherit next-gtk-webkit)
      (name "next")
      (build-system asdf-build-system/sbcl)
      (outputs '("out" "lib"))
      (arguments
       `(#:tests? #f                    ; no tests
         #:asd-system-name "next"
         #:phases (modify-phases %standard-phases
                    (add-after 'unpack 'patch-platform-port-path
                      (lambda* (#:key inputs #:allow-other-keys)
                        (substitute* "source/ports/gtk-webkit.lisp"
                          (("\"next-gtk-webkit\"")
                           (string-append "\"" (assoc-ref inputs "next-gtk-webkit")
                                          "/bin/next-gtk-webkit\"")))
                        #t))
                    (add-after 'patch-platform-port-path 'patch-version
                      ;; When the version is not just dot-separated numerals
                      ;; (e.g. a git-commit version), Guix modifies the .asd with
                      ;; an illegal version number, and then Next fails to query
                      ;; it.  So we hard-code it here.
                      (lambda* (#:key inputs #:allow-other-keys)
                        (let ((version (format #f "~a" ,version)))
                          (substitute* "source/global.lisp"
                            (("version\\)\\)\\)")
                             (string-append "version)))
(setf +version+ \"" version "\")"))))
                        #t))
                    (add-before 'cleanup 'move-bundle
                      (lambda* (#:key outputs #:allow-other-keys)
                        (define lib (assoc-ref outputs "lib"))
                        (define actual-fasl (string-append
                                             lib
                                             "/lib/sbcl/next.fasl"))
                        (define expected-fasl (string-append
                                               lib
                                               "/lib/sbcl/next--system.fasl"))
                        (copy-file actual-fasl expected-fasl)
                        #t))
                    (add-after 'create-symlinks 'build-program
                      (lambda* (#:key outputs #:allow-other-keys)
                        (build-program
                         (string-append (assoc-ref outputs "out") "/bin/next")
                         outputs
                         #:entry-program '((next:entry-point) 0))))
                    (add-before 'build 'install-assets
                      ;; Since the ASDF build system generates a new .asd with a
                      ;; possibly suffixed and thus illegal version number, assets
                      ;; should not be installed after the 'build phase or else
                      ;; the illegal version will result in NIL in the .desktop
                      ;; file.
                      (lambda* (#:key outputs #:allow-other-keys)
                        (with-output-to-file "version"
                          (lambda _
                            (format #t "~a" ,(package-version next-gtk-webkit))))
                        (invoke "make" "install-assets"
                                (string-append "PREFIX="
                                               (assoc-ref outputs "out"))))))))
      (inputs
       `(("alexandria" ,sbcl-alexandria)
         ("bordeaux-threads" ,sbcl-bordeaux-threads)
         ("cl-annot" ,sbcl-cl-annot)
         ("cl-ansi-text" ,sbcl-cl-ansi-text)
         ("cl-css" ,sbcl-cl-css)
         ("cl-json" ,sbcl-cl-json)
         ("cl-markup" ,sbcl-cl-markup)
         ("cl-ppcre" ,sbcl-cl-ppcre)
         ("cl-ppcre-unicode" ,sbcl-cl-ppcre-unicode)
         ("cl-prevalence" ,sbcl-cl-prevalence)
         ("closer-mop" ,sbcl-closer-mop)
         ("dbus" ,cl-dbus)
         ("dexador" ,sbcl-dexador)
         ("fare-quasiquote-extras" ,cl-fare-quasiquote-extras) ; For serapeum.  Guix bug?
         ("ironclad" ,sbcl-ironclad)
         ("local-time" ,sbcl-local-time)
         ("log4cl" ,sbcl-log4cl)
         ("lparallel" ,sbcl-lparallel)
         ("mk-string-metrics" ,sbcl-mk-string-metrics)
         ("parenscript" ,sbcl-parenscript)
         ("plump" ,sbcl-plump)
         ("quri" ,sbcl-quri)
         ("serapeum" ,sbcl-serapeum)
         ("sqlite" ,sbcl-cl-sqlite)
         ("str" ,sbcl-cl-str)
         ("swank" ,sbcl-slime-swank)
         ("trivia" ,sbcl-trivia)
         ("trivial-clipboard" ,sbcl-trivial-clipboard)
         ("unix-opts" ,sbcl-unix-opts)
         ;; Local deps
         ("next-gtk-webkit" ,next-gtk-webkit)
         ("next-download-manager" ,sbcl-next-download-manager)
         ("next-ring" ,sbcl-next-ring)
         ("next-history-tree" ,sbcl-next-history-tree)
         ("next-password-manager" ,sbcl-next-password-manager)
         ("next-hooks" ,sbcl-next-hooks)))
      (native-inputs
       `(("trivial-features" ,sbcl-trivial-features)
         ("trivial-types" ,sbcl-trivial-types)
         ("prove-asdf" ,sbcl-prove-asdf)))
      (synopsis "Infinitely extensible web-browser (with Lisp development files)"))))

(define-public sbcl-next
  (deprecated-package "sbcl-next" next))
