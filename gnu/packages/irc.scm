;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014 Kevin Lemonnier <lemonnierk@ulrar.net>
;;; Copyright © 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2016 Efraim Flashner <efraim@flashner.co.il>
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
    (version "0.12.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://quassel-irc.org/pub/quassel-"
                            version ".tar.bz2"))
        (sha256
         (base32
          "0ka456fb8ha3w7g74xlzfg6w4azxjjxgrhl4aqpbwg3lnd6fbr4k"))))
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
                           "-DWITH_WEBKIT=OFF") ; qtwebkit isn't packaged
       #:tests? #f)) ; no test target
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("oxygen-icons" ,oxygen-icons)
       ("qca" ,qca)
       ("qtbase", qtbase)
       ("qttools" ,qttools)
       ("qtscript" ,qtscript)
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
    (version "0.8.19")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/irssi/irssi/"
                                 "releases/download/" version "/irssi-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "1lz57v3nkki30lb883pipp5syyfkssvjlq3xxf9yl578902h982c"))))
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
    (version "1.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://weechat.org/files/src/weechat-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0w87w4wy61x705ama8h36z9mgdj2gmmzdfrsxvwyh2m2as2max1i"))
              (patches (search-patches "weechat-python.patch"))))
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

(define-public ircii
  (package
    (name "ircii")
    (version "20151120")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://ircii.warped.com/"
                                 name "-" version ".tar.gz"))
             (sha256
              (base32
               "178dc279f5j894qvp96dzz7c0jpryqlcqw5g0dc9yaxg9kgw1lqm"))))
    (build-system gnu-build-system)
    ;; TODO: We should package a small socks4/5 library/server to configure
    ;; ircii with socks client. `ghc-socks' pulls in lots of haskell, which
    ;; is too big.
    (arguments
     `(#:tests? #f
       #:configure-flags (list
                          "--enable-ipv6"
                          "--with-emacs-meta-keys"
                          (string-append "--with-openssl="
                                         (assoc-ref %build-inputs "openssl")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-bsdinstall-absolute-path-bins
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "bsdinstall"
               (("/bin/strip") "strip")
               (("/bin/cp") "cp")
               (("/bin/chmod") "chmod")
               (("/etc/chown") "chown")
               (("/bin/chgrp") "chgrp")
               (("/bin/mkdir") "mkdir")
               (("/bin/rm") "rm")
               (("/bin/mv") "mv")))))))
    (inputs
     `(("ncurses" ,ncurses)
       ("openssl" ,openssl)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("perl" ,perl)))
    (home-page "http://www.eterna.com.au/ircii/")
    (synopsis "Terminal-based IRC and ICB client")
    (description
     "ircII is a terminal based IRC and ICB client for UNIX systems.")
    (license license:bsd-3)))

(define-public ii
  (package
    (name "ii")
    (version "1.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://dl.suckless.org/tools/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "176cqwnn6h7w4kbfd66hzqa243l26pqp2b06bii0nmnm0rkaqwis"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:make-flags (list (string-append "PREFIX=" %output)
                          "CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)))) ; no configure
    (home-page "http://tools.suckless.org/ii/")
    (synopsis "FIFO and file system based IRC client")
    (description
     "ii (Irc it) is a minimalist FIFO and file system based IRC client.")
    (license license:expat)))

(define-public sic
  (package
    (name "sic")
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://dl.suckless.org/tools/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "11aiavxp44yayibc58bvimi8mrxvbw1plbci8cnbl4syk42zj1xc"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:make-flags (list "CC=gcc"
                          (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)))) ; no configure
    (home-page "http://tools.suckless.org/sic/")
    (synopsis "Simple IRC client")
    (description
     "sic is a simple IRC client, even more minimalistic than ii.")
    (license license:expat)))
