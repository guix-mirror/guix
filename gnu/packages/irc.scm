;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014 Kevin Lemonnier <lemonnierk@ulrar.net>
;;; Copyright © 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2016 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages irc)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages autogen)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages file)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls))

(define-public quassel
  (package
    (name "quassel")
    (version "0.12.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://quassel-irc.org/pub/quassel-"
                            version ".tar.bz2"))
        (sha256
         (base32
          "0d6lwf6qblj1ia5j9mjy112zrmpbbg9mmxgscbgxiqychldyjgjd"))))
    (build-system cmake-build-system)
    (arguments
      ;; The three binaries are not mutually exlusive, and are all built
      ;; by default.
     `(#:configure-flags '(;;"-DWANT_QTCLIENT=OFF" ; 5.0 MiB
                           ;;"-DWANT_CORE=OFF" ; 2.3 MiB
                           ;;"-DWANT_MONO=OFF" ; 6.3 MiB
                           "-DUSE_QT5=ON" ; default is qt4
                           "-DWITH_KDE=OFF" ; no to integration
                           "-DWITH_OXYGEN=ON" ; on=embed icons
                           "-DWITH_WEBKIT=ON") ; wants qtwebkit, in qt5
       #:tests? #f)) ; no test target
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("oxygen-icons" ,oxygen-icons)
       ("qca" ,qca)
       ("qt", qt)
       ("snorenotify" ,snorenotify)
       ("zlib" ,zlib)))
    (home-page "http://quassel-irc.org/")
    (synopsis "Distributed IRC client")
    (description "Quassel is a distributed IRC client, meaning that one or more
clients can attach to and detach from the central core.  It resembles the
popular combination of screen and a text-based IRC client such as WeeChat or
irssi, but graphical.")
    (license (list license:gpl2 license:gpl3)))) ;; dual licensed

(define-public irssi
  (package
    (name "irssi")
    (version "0.8.18")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/irssi/irssi/"
                                 "releases/download/" version "/irssi-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0h3q0p5rnm6h0hcv06m8vrs97kyr1l7snjdy3qbjsz3m97vpg8f0"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (ncurses (assoc-ref inputs "ncurses")))
               (setenv "CONFIG_SHELL" (which "bash"))
               (zero?
                (system* "./configure"
                         (string-append "--prefix=" out)
                         (string-append "--with-ncurses=" ncurses)
                         (string-append "--with-proxy")
                         (string-append "--with-socks")
                         (string-append "--with-bot")))))))))
    (inputs
     `(("glib" ,glib)
       ("ncurses" ,ncurses)
       ("openssl" ,openssl)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("perl" ,perl)))
    (home-page "http://www.irssi.org/")
    (synopsis "Terminal-based IRC client")
    (description
     "Irssi is a terminal based IRC client for UNIX systems.  It also supports
SILC and ICB protocols via plugins.")
    (license license:gpl2+)))

(define-public weechat
  (package
    (name "weechat")
    (version "1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://weechat.org/files/src/weechat-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "19apd3hav77v74j7flicai0843k7wrkr2fd3q2ayvzkgnbrrp1ai"))
              (patches (list (search-patch "weechat-python.patch")))))
    (build-system gnu-build-system)
    (native-inputs `(("autoconf" ,autoconf)
                     ("pkg-config" ,pkg-config)
                     ("file" ,file)
                     ("autogen" ,autogen)
                     ("automake" ,automake)
                     ("libtool" ,libtool)))
    (inputs `(("ncurses" ,ncurses)
              ("diffutils" ,diffutils)
              ("gettext" ,gnu-gettext)
              ("libltdl" ,libltdl)
              ("libgcrypt" ,libgcrypt "out")
              ("zlib" ,zlib)
              ("aspell" ,aspell)
              ("curl" ,curl)
              ("gnutls" ,gnutls)
              ("guile" ,guile-2.0)
              ("openssl" ,openssl)
              ("cyrus-sasl" ,cyrus-sasl)
              ("lua" ,lua-5.1)
              ("python" ,python-2)
              ("perl" ,perl)
              ("tcl" ,tcl)))
    (arguments
     `(#:configure-flags (list (string-append
                                "--with-tclconfig="
                                (assoc-ref %build-inputs "tcl") "/lib"))
       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'autogen
                    (lambda _
                      (zero? (system* "./autogen.sh"))))
                  (add-before 'build 'set-python-file-name
                    (lambda* (#:key inputs #:allow-other-keys)
                      (substitute* "src/plugins/python/weechat-python.c"
                        (("python2_bin = weechat_python_get_python2_bin.*;")
                         (string-append "python2_bin = strdup (\""
                                        (assoc-ref inputs "python")
                                        "/bin/python\");\n")))
                      #t)))))
    (synopsis "Extensible chat client")
    (description "WeeChat (Wee Enhanced Environment for Chat) is an
Internet Relay Chat client, which is designed to be light and fast.
The client uses a curses frontend, and there are remote interfaces
for Web, Qt, Android and Emacs.  In WeeChat everything can be done
with a keyboard, though it also supports mouse.  It is customizable
and extensible with plugins and scripts.")
    (home-page "http://www.weechat.org/")
    (license license:gpl3)))
