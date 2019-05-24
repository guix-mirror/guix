;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2017 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018 Timo Eisenmann <eisenmann@fn.de>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
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
              (uri (string-append "http://www.dillo.org/download/"
                                  name "-" version ".tar.bz2"))
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
              ("openssl" ,openssl)
              ("perl" ,perl)
              ("zlib" ,zlib)))
    (synopsis "Very small and fast graphical web browser")
    (description "Dillo is a minimalistic web browser particularly intended for
older or slower computers and embedded systems.")
    (home-page "http://www.dillo.org")
    (license license:gpl3+)))

(define-public links
  (package
    (name "links")
    (version "2.19")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://links.twibright.com/download/"
                                  "links-" version ".tar.bz2"))
                (sha256
               (base32
                "02ls11c02p7xvsdjyb43rrzr850i1yly003r812z0w5vv5yqqxbh"))))
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
              (method url-fetch)
              (uri (string-append "https://github.com/luakit/luakit/archive/" version
                                  ".tar.gz"))
              (sha256
               (base32
                "09kgsm2w2xa4xwylsi0bwjvdl9k6hkyjyyrq2i3l8bvi2qrs5gzc"))
              (file-name (string-append name "-" version ".tar.gz"))))
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
              ("gnutls" ,gnutls)
              ("libgcrypt" ,libgcrypt)
              ("unzip" ,unzip)
              ("zlib" ,zlib)
              ("gzip" ,gzip)
              ("bzip2" ,bzip2)))
    (arguments
     `(#:configure-flags
       (let ((gnutls (assoc-ref %build-inputs "gnutls")))
         `("--with-pkg-config"
           "--with-screen=ncurses"
           "--with-zlib"
           "--with-bzlib"
           ,(string-append "--with-gnutls=" gnutls)
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
    (license license:gpl2)))

(define-public qutebrowser
  (package
    (name "qutebrowser")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/The-Compiler/"
                           "qutebrowser/releases/download/v" version "/"
                           "qutebrowser-" version ".tar.gz"))
       (sha256
        (base32
         "13ihx66jm1dd6vx8px7pm0kbzf2sf9x43hhivc1rp17kahnxxdyv"))))
    (build-system python-build-system)
    (native-inputs
     `(("asciidoc" ,asciidoc)))
    (inputs
     `(("python-colorama" ,python-colorama)
       ("python-cssutils" ,python-cssutils)
       ("python-jinja2" ,python-jinja2)
       ("python-markupsafe" ,python-markupsafe)
       ("python-pygments" ,python-pygments)
       ("python-pypeg2" ,python-pypeg2)
       ("python-pyyaml" ,python-pyyaml)
       ("python-pyqt" ,python-pyqt)
       ("qtwebkit" ,qtwebkit)))
    (arguments
     `(#:tests? #f                      ;no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-more
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (app (string-append out "/share/applications"))
                    (hicolor (string-append out "/share/icons/hicolor")))
               (invoke "a2x" "-f" "manpage" "doc/qutebrowser.1.asciidoc")
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
               
               (substitute* "qutebrowser.desktop"
                 (("Exec=qutebrowser")
                  (string-append "Exec=" out "/bin/qutebrowser")))
               (install-file "qutebrowser.desktop" app)
               #t))))))
    (home-page "https://qutebrowser.org/")
    (synopsis "Minimal, keyboard-focused, vim-like web browser")
    (description "qutebrowser is a keyboard-focused browser with a minimal
GUI.  It is based on PyQt5 and QtWebKit.")
    (license license:gpl3+)))

(define-public vimb
  (package
    (name "vimb")
    (version "3.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fanglingsu/vimb/")
             (commit version)))
       (sha256
        (base32
         "1qg18z2gnsli9qgrqfhqfrsi6g9mcgr90w8yab28nxrq4aha6brf"))
       (file-name (git-file-name name version))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(#:tests? #f ; no tests
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

(define-public next-gtk-webkit
  (package
    (name "next-gtk-webkit")
    (version "1.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://source.atlas.engineer/public/next")
             (commit version)))
       (sha256
        (base32
         "1bif1k738knhifxhkn0d2x1m521zkx40pri44vyjqncp9r95hkbk"))
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

(define-public sbcl-next
  (package
    (inherit next-gtk-webkit)
    (name "sbcl-next")
    (build-system asdf-build-system/sbcl)
    (outputs '("out" "lib"))
    (arguments
     `(#:tests? #f                      ; no tests
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-platform-port-path
                    (lambda* (#:key inputs #:allow-other-keys)
                      (substitute* "source/ports/gtk-webkit.lisp"
                        (("\"next-gtk-webkit\"")
                         (string-append "\"" (assoc-ref inputs "next-gtk-webkit")
                                        "/bin/next-gtk-webkit\"")))))
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
                       #:entry-program '((next:start-with-port) 0))))
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
     `(("next-gtk-webkit" ,next-gtk-webkit)
       ;; Lisp libraries:
       ("trivial-features" ,sbcl-trivial-features)
       ("alexandria" ,sbcl-alexandria)
       ("anaphora" ,sbcl-anaphora)
       ("closer-mop" ,sbcl-closer-mop)
       ("log4cl" ,sbcl-log4cl)
       ("find-port" ,sbcl-find-port)
       ("cl-strings" ,sbcl-cl-strings)
       ("cl-string-match" ,sbcl-cl-string-match)
       ("puri" ,sbcl-puri)
       ("sqlite" ,sbcl-cl-sqlite)
       ("parenscript" ,sbcl-parenscript)
       ("cl-json" ,sbcl-cl-json)
       ("swank" ,sbcl-slime-swank)
       ("cl-markup" ,sbcl-cl-markup)
       ("cl-css" ,sbcl-cl-css)
       ("bordeaux-threads" ,sbcl-bordeaux-threads)
       ("s-xml-rpc" ,sbcl-s-xml-rpc)
       ("unix-opts" ,sbcl-unix-opts)
       ("trivial-clipboard" ,sbcl-trivial-clipboard)))
    (synopsis "Infinitely extensible web-browser (with Lisp development files)")))
